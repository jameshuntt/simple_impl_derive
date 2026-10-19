use proc_macro2::TokenStream;
use simple_impl_core::{ValidationRule, ValidationSpec};
use simple_impl_quote_kit::{
    emit_simple_subcommand_contract_impls_shaped,
    ExitStatusQuote,
    SimpleSubcommandQuote,
    SubcommandArgKind,
    SubcommandFieldQuote,
    SubcommandShapeQuote,
    ValidationRuleQuote,
};
use simple_impl_attr_kit::AttrBag;
use quote::{format_ident, quote};
use syn::ext::IdentExt;
use syn::{Data, DeriveInput, Field, Fields, Ident, Type};
use crate::diag::{err_at, err_at_ctx, err_spanned, E};
use crate::xccute_policy::{emit_xccute_policy_metadata_impl, parse_xccute_policy};
use liaise_proc_macro::SynBuffer;

#[derive(Debug, Clone)]
enum ArgKind {
    Flag(String),
    KeyValue(String),
    Positional,
}

impl ArgKind {
    fn into_quote_kind(self) -> SubcommandArgKind {
        match self {
            ArgKind::Flag(flag) => SubcommandArgKind::Flag(flag),
            ArgKind::KeyValue(key) => SubcommandArgKind::KeyValue(key),
            ArgKind::Positional => SubcommandArgKind::Positional,
        }
    }
}

#[derive(Debug, Clone)]
struct SubcommandField {
    ident: Ident,
    ty: Type,
    method: Ident,
    kind: ArgKind,
    init_required: bool,
    /// `#[arg(each = ..)]`: what is called with the field's name and each value.
    each: Option<TokenStream>,
    /// `#[arg(min = N)]`: the fewest values a Vec field takes.
    min: Option<usize>,
}

/// What `#[subcommand(..)]` says about the line: its word, and the program
/// when the leaf is its own root.
struct SubcommandLine {
    segment: Option<String>,
    program: Option<String>,
}

impl SubcommandField {
    fn into_quote_spec(self) -> SubcommandFieldQuote {
        SubcommandFieldQuote::new(
            self.ident,
            self.ty,
            self.method,
            self.kind.into_quote_kind(),
        )
        .with_init_required(self.init_required)
    }
}

/// Segment, every field, every validation rule and the policy are all read;
/// each problem goes into one buffer. The impls are written only when nothing
/// was reported, since the quote kit trusts what was parsed.
pub fn expand_simple_subcommand(input: &DeriveInput) -> syn::Result<TokenStream> {
    let mut errs = SynBuffer::new();

    let line = errs.absorb(parse_subcommand_line(input));
    let segment = line.as_ref().map(|line| line.segment.clone().unwrap_or_default());
    let (fields, failed, field_rules) = parse_subcommand_fields(input, &mut errs);
    let struct_rules = simple_impl_attr_kit::parse_validation_attrs_into(&input.attrs, &mut errs);
    // every rule name and validate(with) path is checked; each rule keeps how
    // it reads as written, with the field it sits on, for its later errors
    let mut validation_rules: Vec<ValidationRuleQuote> = Vec::new();
    let mut rule_contexts: Vec<String> = Vec::new();
    let struct_rules = ValidationSpec::from(struct_rules).rules().to_vec();
    let owned = struct_rules.into_iter().map(|rule| (rule, None)).chain(field_rules);
    for (rule, owner) in owned {
        if let Some((rule, context)) = validation_rule_into_quote(rule, owner.as_deref(), &mut errs) {
            validation_rules.push(rule);
            rule_contexts.push(context);
        }
    }
    // the fields' own checks (#[arg(each, min)]) run after the rules, as one
    // generated function the contract calls like a #[validate(with)]
    let arg_checks = emit_arg_checks(&input.ident, &fields);
    if arg_checks.is_some() {
        validation_rules.push(ValidationRuleQuote::CustomFunction { function_path: syn::parse_quote!(Self::__simple_impl_arg_checks) });
        rule_contexts.push("#[arg(each, min)]".to_owned());
    }
    let exit_statuses = parse_exit_statuses(input, &mut errs);

    // The contract checks every field it is given and that every rule names
    // one of them. It runs on the fields that parsed (on a struct of the
    // wrong shape, under stand-in names); the ones that failed are passed as
    // skipped, so a rule naming one is still checked for its other names and
    // the failed field is not reported twice. The policy is independent of it.
    // a missing segment is already reported; the contract still checks the rest
    let field_specs = fields.into_iter().map(SubcommandField::into_quote_spec).collect();
    let quote_spec = SimpleSubcommandQuote::new(input.ident.clone(), input.vis.clone(), segment.clone().unwrap_or_default(), field_specs)
        .with_validation_rules(validation_rules);
    let shape = line.as_ref().map_or_else(SubcommandShapeQuote::new, |line| subcommand_shape(line, exit_statuses));
    let contract_impls = errs.absorb(emit_simple_subcommand_contract_impls_shaped(&quote_spec, &failed, &rule_contexts, &shape));
    let policy = errs.absorb(parse_xccute_policy(input));

    let (Some(_), Some(contract_impls), Some(policy)) = (segment, contract_impls, policy) else {
        return errs.finish_with(TokenStream::new());
    };
    let policy_impl = emit_xccute_policy_metadata_impl(&input.ident, &policy);

    errs.finish_with(::quote::quote! {
        #contract_impls
        #policy_impl
        #arg_checks
    })
}

/// The line's shape for the contract: no segment when none was given, the
/// leaf's own program, and the statuses it names with the codes it accepts.
/// 0 is accepted unless a status names it and says otherwise, as a command
/// that names nothing accepts it; any other named code is accepted only when
/// its status says so.
fn subcommand_shape(line: &SubcommandLine, statuses: Vec<ExitStatusQuote>) -> SubcommandShapeQuote {
    let mut shape = SubcommandShapeQuote::new();
    if line.segment.is_none() {
        shape = shape.without_segment();
    }
    if let Some(program) = &line.program {
        shape = shape.with_program(program.clone());
    }
    if !statuses.is_empty() {
        let zero_refused = statuses.iter().any(|status| status.code == 0 && !status.accepted);
        let ok_codes = (!zero_refused).then_some(0).into_iter().chain(statuses.iter().filter(|s| s.accepted && s.code != 0).map(|s| s.code)).collect();
        shape = shape.with_exit_statuses(statuses, ok_codes);
    }
    shape
}

/// Every `#[exit_status(code = N, word = "..", accepted)]`, in order; each
/// problem is reported and that status left out.
fn parse_exit_statuses(input: &DeriveInput, errs: &mut SynBuffer) -> Vec<ExitStatusQuote> {
    let mut statuses: Vec<ExitStatusQuote> = Vec::new();
    for attr in input.attrs.iter().filter(|attr| attr.path().is_ident("exit_status")) {
        let Some(bag) = errs.absorb(AttrBag::from_attr(attr, "exit_status").and_then(|bag| crate::vocabulary::exit_status().validate(&bag))) else {
            continue;
        };
        let code_at = bag.span_of("code").unwrap_or_else(|| bag.attr_span());
        let code = errs.absorb(bag.require_int("code")).and_then(|code| match i32::try_from(code) {
            Ok(code) => Some(code),
            Err(_) => {
                errs.push(err_at_ctx(code_at, E::ExitCodeNotI32, format!("`{code}`")));
                None
            }
        });
        let word = errs.absorb(bag.require_str("word")).map(ToOwned::to_owned);
        let accepted = errs.absorb(bag.optional_bool("accepted"));
        let (Some(code), Some(word), Some(accepted)) = (code, word, accepted) else {
            continue;
        };
        if statuses.iter().any(|status| status.code == code) {
            errs.push(err_at_ctx(code_at, E::ExitCodeTwice, format!("`{code}`")));
            continue;
        }
        statuses.push(ExitStatusQuote::new(code, word, accepted.unwrap_or(code == 0)));
    }
    statuses
}

/// The fields' own checks, `#[arg(min = N)]` then `#[arg(each = ..)]` for
/// each field in order, as one hidden method; `None` when no field has one.
/// A check is called with the field's name and the value, so its error can
/// name the field.
fn emit_arg_checks(ident: &Ident, fields: &[SubcommandField]) -> Option<TokenStream> {
    let checks: Vec<TokenStream> = fields
        .iter()
        .filter(|field| field.each.is_some() || field.min.is_some())
        .map(|field| {
            let field_ident = &field.ident;
            let name = field_ident.unraw().to_string();
            let min = field.min.map(|min| {
                quote! {
                    if self.#field_ident.len() < #min {
                        return ::std::result::Result::Err(
                            ::xccute_contract::CommandValidationError::structural(::std::format!(
                                "{} takes at least {} values, given {}",
                                #name,
                                #min,
                                self.#field_ident.len()
                            ))
                            .with_field(#name)
                            .with_rule("min"),
                        );
                    }
                }
            });
            let each = field.each.as_ref().map(|each| {
                if simple_impl_attr_kit::is_vec_ty(&field.ty) {
                    quote! { for value in &self.#field_ident { #each(#name, value)?; } }
                } else if simple_impl_attr_kit::is_option_ty(&field.ty) {
                    quote! { if let ::std::option::Option::Some(value) = &self.#field_ident { #each(#name, value)?; } }
                } else {
                    quote! { #each(#name, &self.#field_ident)?; }
                }
            });
            quote! { #min #each }
        })
        .collect();
    if checks.is_empty() {
        return None;
    }
    Some(quote! {
        impl #ident {
            #[doc(hidden)]
            fn __simple_impl_arg_checks(&self) -> ::xccute_contract::CommandValidationResult {
                #(#checks)*
                ::std::result::Result::Ok(())
            }
        }
    })
}

/// `#[subcommand(segment = "..")]`, `#[subcommand(no_segment)]`, or
/// `#[subcommand(program = "..")]` with or without a segment. Without any of
/// them the segment is missing, reported as it always was.
fn parse_subcommand_line(input: &DeriveInput) -> syn::Result<SubcommandLine> {
    if !input.attrs.iter().any(|attr| attr.path().is_ident("subcommand")) {
        return Err(err_spanned(input, E::SubcmdMissingSegment));
    }
    let written = AttrBag::from_attrs(&input.attrs, "subcommand")?;
    let segment_required = !(written.contains_key("program") || written.flag("no_segment").unwrap_or(false));
    let bag = crate::vocabulary::simple_subcommand(segment_required).validate(&written)?;
    Ok(SubcommandLine {
        segment: bag.optional_str("segment")?.map(ToOwned::to_owned),
        program: bag.optional_str("program")?.map(ToOwned::to_owned),
    })
}

/// Every field is read; a broken one reports and is left out, and its name
/// is returned among the failed. Every field's validation rules are returned
/// with the field they sit on, a failed field's too, so their other names are
/// still checked. A tuple struct is reported, and its fields are still read
/// and checked under stand-in names (`_0`, `_1`, ...); on an enum or union only
/// the fields' attributes are read.
fn parse_subcommand_fields(input: &DeriveInput, errs: &mut SynBuffer) -> (Vec<SubcommandField>, Vec<Ident>, Vec<(ValidationRule, Option<String>)>) {
    let fields: Vec<(&Field, Ident)> = match &input.data {
        Data::Struct(syn::DataStruct { fields: Fields::Named(named), .. }) => {
            named.named.iter().filter_map(|f| f.ident.clone().map(|ident| (f, ident))).collect()
        }
        // a line with no arguments (`hostname`, `systemctl daemon-reload`)
        Data::Struct(syn::DataStruct { fields: Fields::Unit, .. }) => Vec::new(),
        other => {
            if matches!(other, Data::Struct(_)) {
                errs.push_spanned(input, E::SubcmdNamedFields);
            } else {
                errs.push_spanned(input, E::SubcmdOnlyStructs);
            }
            let Data::Struct(data) = other else {
                // an enum's or union's fields are not the command's fields:
                // their attributes are read and reported, nothing more
                let fields: Vec<&Field> = match other {
                    Data::Enum(data) => data.variants.iter().flat_map(|v| v.fields.iter()).collect(),
                    Data::Union(data) => data.fields.named.iter().collect(),
                    Data::Struct(_) => Vec::new(),
                };
                let mut rules = Vec::new();
                let mut names = Vec::new();
                for (i, f) in fields.into_iter().enumerate() {
                    let ident = f.ident.clone().unwrap_or_else(|| format_ident!("_{}", i));
                    let name = ident.to_string();
                    let _ = read_builder(f, errs);
                    errs.absorb(read_arg_kind(f));
                    let spec = simple_impl_attr_kit::parse_field_validation_attrs_into(name.clone(), &f.attrs, errs);
                    rules.extend(ValidationSpec::from(spec).rules().iter().cloned().map(|rule| (rule, Some(name.clone()))));
                    names.push(ident);
                }
                // the fields exist; they are only not checked: rules naming them
                // are skipped for those names, not told the names are unknown
                return (Vec::new(), names, rules);
            };
            // a tuple or unit struct: its fields are checked under stand-in names
            data.fields
                .iter()
                .enumerate()
                .map(|(i, f)| (f, f.ident.clone().unwrap_or_else(|| format_ident!("_{}", i))))
                .collect()
        }
    };

    let mut parsed = Vec::new();
    let mut failed = Vec::new();
    let mut rules = Vec::new();
    for (f, ident) in fields {
        let (field, field_rules) = parse_subcommand_field(f, ident.clone(), errs);
        rules.extend(field_rules.into_iter().map(|rule| (rule, Some(ident.to_string()))));
        match field {
            Some(field) => parsed.push(field),
            None => failed.push(ident),
        }
    }
    (parsed, failed, rules)
}

/// One field: its `#[builder]`, its `#[arg]` and its validation attributes are
/// each read, and each reports on its own.
fn parse_subcommand_field(field: &Field, ident: Ident, errs: &mut SynBuffer) -> (Option<SubcommandField>, Vec<ValidationRule>) {
    let builder = read_builder(field, errs);
    let arg = errs.absorb(read_arg(field));
    let kind = arg.as_ref().and_then(|bag| errs.absorb(arg_kind(bag)));
    // a rule attribute that cannot be read is reported; the rules that were
    // read are kept, and the field is still checked
    let rules = ValidationSpec::from(simple_impl_attr_kit::parse_field_validation_attrs_into(ident.to_string(), &field.attrs, errs))
        .rules()
        .to_vec();

    let kind = match kind {
        Some(Some(kind)) => kind,
        Some(None) => {
            errs.push_spanned(field, E::SubcmdFieldNeedsArg);
            return (None, rules);
        }
        None => return (None, rules),
    };
    // an unreadable #[builder] is reported; the field keeps its own name and
    // is still checked for what does not need #[builder]
    let (method, init_required) = builder.unwrap_or((None, false));
    let method = method.unwrap_or_else(|| ident.clone());
    let repeated = simple_impl_attr_kit::is_vec_ty(&field.ty) && !matches!(kind, ArgKind::Flag(_));
    if init_required && repeated {
        errs.push_spanned(field, E::InitRequiredRepeated);
    }
    let (each, min) = arg.as_ref().map_or((None, None), |bag| read_arg_checks(bag, repeated, errs));

    let field = SubcommandField {
        ident,
        ty: field.ty.clone(),
        method,
        kind,
        init_required,
        each,
        min,
    };
    (Some(field), rules)
}

/// `#[arg(each = ..)]` and `#[arg(min = N)]`: what each value is checked with,
/// called as written when it is a path (`each = crate::plain`) and in
/// parentheses when it is any other expression; and the fewest values, on a
/// Vec field only.
fn read_arg_checks(bag: &AttrBag, repeated: bool, errs: &mut SynBuffer) -> (Option<TokenStream>, Option<usize>) {
    let each = crate::simple::read_expr(bag, "each", errs).flatten().map(|tokens| match syn::parse2::<syn::Path>(tokens.clone()) {
        Ok(_) => tokens,
        Err(_) => quote! { (#tokens) },
    });
    let min_at = bag.span_of("min").unwrap_or_else(|| bag.attr_span());
    let min = errs.absorb(bag.optional_int("min")).flatten().and_then(|min| {
        if !repeated {
            errs.push(err_at(min_at, E::MinNeedsVec));
            return None;
        }
        match usize::try_from(min) {
            Ok(min) => Some(min),
            Err(_) => {
                errs.push(err_at_ctx(min_at, E::MinNotCount, format!("`{min}`")));
                None
            }
        }
    });
    (each, min)
}

/// `#[builder(method = "...", init_required)]`.
/// `None` when the attribute cannot be read. A method name that is not an
/// identifier is reported and the field keeps its own name, so the field's
/// other checks still run.
fn read_builder(field: &Field, errs: &mut SynBuffer) -> Option<(Option<Ident>, bool)> {
    let builder = errs.absorb(
        AttrBag::from_attrs(&field.attrs, "builder").and_then(|bag| crate::vocabulary::subcommand_builder().validate(&bag)),
    )?;
    let method = match errs.absorb(builder.optional_str("method")).flatten() {
        Some(m) if crate::simple::is_ident(m) => crate::simple::method_ident(m),
        Some(_) => {
            let span = builder.span_of("method").unwrap_or_else(proc_macro2::Span::call_site);
            errs.push_at(span, E::MethodNotIdent);
            None
        }
        None => None,
    };
    let init_required = errs.absorb(builder.flag("init_required")).unwrap_or(false);
    Some((method, init_required))
}

/// `#[arg(flag = "...")]`, `#[arg(kv = "...")]` or `#[arg(positional)]`; `None` if none.
fn read_arg_kind(field: &Field) -> syn::Result<Option<ArgKind>> {
    arg_kind(&read_arg(field)?)
}

/// A field's `#[arg(...)]`, read against the vocabulary.
fn read_arg(field: &Field) -> syn::Result<AttrBag> {
    crate::vocabulary::subcommand_arg().validate(&AttrBag::from_attrs(&field.attrs, "arg")?)
}

/// The kind an `#[arg(...)]` says; `None` if none.
fn arg_kind(arg: &AttrBag) -> syn::Result<Option<ArgKind>> {
    Ok(if let Some(flag) = arg.optional_str("flag")? {
        Some(ArgKind::Flag(flag.to_owned()))
    } else if let Some(kv) = arg.optional_str("kv")? {
        Some(ArgKind::KeyValue(kv.to_owned()))
    } else if arg.flag("positional")? {
        Some(ArgKind::Positional)
    } else {
        None
    })
}

/// One rule, every name checked; each bad name is reported. A rule that lost
/// a name to that keeps its good names, so the contract still reports the
/// ones that are unknown: only the problem check survives, since nothing is
/// generated once a problem has been reported.
fn validation_rule_into_quote(rule: ValidationRule, owner: Option<&str>, errs: &mut SynBuffer) -> Option<(ValidationRuleQuote, String)> {
    let context = rule_context(&rule, owner);
    let mut name = |n: &str| errs.absorb(ident_from_rule_field(n, &context));
    // a pair that lost one name keeps the good one in both places: its kind
    // still names the rule in any later error
    let pair = |a: Option<Ident>, b: Option<Ident>, whole: fn(Ident, Ident) -> ValidationRuleQuote| match (a, b) {
        (Some(a), Some(b)) => Some(whole(a, b)),
        (Some(one), None) | (None, Some(one)) => Some(whole(one.clone(), one)),
        (None, None) => None,
    };
    let quote = match rule {
        ValidationRule::Requires { field, required } => {
            let (a, b) = (name(&field), name(&required));
            pair(a, b, |field, required| ValidationRuleQuote::Requires { field, required })
        }
        ValidationRule::InvalidWithout { field, required } => {
            let (a, b) = (name(&field), name(&required));
            pair(a, b, |field, required| ValidationRuleQuote::InvalidWithout { field, required })
        }
        ValidationRule::OnlyPairWith { field, paired_with } => {
            let (a, b) = (name(&field), name(&paired_with));
            pair(a, b, |field, paired_with| ValidationRuleQuote::OnlyPairWith { field, paired_with })
        }
        ValidationRule::ConflictsWith { field, conflicts_with } => {
            let (a, b) = (name(&field), name(&conflicts_with));
            pair(a, b, |field, conflicts_with| ValidationRuleQuote::ConflictsWith { field, conflicts_with })
        }
        // a list that lost every name has nothing left to check; an empty
        // list as written is kept, as it always was
        ValidationRule::OneOf { fields: names } => {
            let fields: Vec<Ident> = names.iter().filter_map(|n| name(n)).collect();
            (names.is_empty() || !fields.is_empty()).then_some(ValidationRuleQuote::OneOf { fields })
        }
        ValidationRule::AtLeastOneOf { fields: names } => {
            let fields: Vec<Ident> = names.iter().filter_map(|n| name(n)).collect();
            (names.is_empty() || !fields.is_empty()).then_some(ValidationRuleQuote::AtLeastOneOf { fields })
        }
        ValidationRule::CustomFunction { function_path } => errs
            .absorb(syn::parse_str(&function_path).map_err(|e| {
                err_at_ctx(proc_macro2::Span::call_site(), E::RuleFnNotPath, format!("`{function_path}` in {context}: {e}"))
            }))
            .map(|function_path| ValidationRuleQuote::CustomFunction { function_path }),
    };
    quote.map(|quote| (quote, context))
}

/// The rule as written and the field it sits on, for errors that have no
/// place of their own: two different rules then never print the same message.
fn rule_context(rule: &ValidationRule, owner: Option<&str>) -> String {
    let list = |names: &[String]| names.iter().map(|n| format!("{n:?}")).collect::<Vec<_>>().join(", ");
    let on = owner.map(|o| format!(" on `{o}`")).unwrap_or_default();
    match rule {
        ValidationRule::Requires { field, .. } => format!("#[requires] on `{field}`"),
        ValidationRule::InvalidWithout { field, .. } => format!("#[invalid_without] on `{field}`"),
        ValidationRule::OnlyPairWith { field, .. } => format!("#[only_pair_with] on `{field}`"),
        ValidationRule::ConflictsWith { field, .. } => format!("#[conflicts_with] on `{field}`"),
        ValidationRule::OneOf { fields } => format!("#[one_of({})]{on}", list(fields)),
        ValidationRule::AtLeastOneOf { fields } => format!("#[at_least_one_of({})]{on}", list(fields)),
        ValidationRule::CustomFunction { .. } => format!("#[validate(with)]{on}"),
    }
}

fn ident_from_rule_field(field: &str, context: &str) -> syn::Result<Ident> {
    syn::parse_str::<Ident>(field).map_err(|error| {
        err_at_ctx(proc_macro2::Span::call_site(), E::RuleFieldNotIdent, format!("`{field}` in {context}: {error}"))
    })
}
