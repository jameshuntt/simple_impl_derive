use proc_macro2::TokenStream;
use simple_impl_core::{ValidationRule, ValidationSpec};
use simple_impl_quote_kit::{
    emit_simple_subcommand_contract_impls_skipping,
    SimpleSubcommandQuote,
    SubcommandArgKind,
    SubcommandFieldQuote,
    ValidationRuleQuote,
};
use simple_impl_attr_kit::AttrBag;
use quote::format_ident;
use syn::{Data, DeriveInput, Field, Fields, Ident, Type};
use crate::diag::{err_at_ctx, err_spanned, E};
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

    let segment = errs.absorb(parse_subcommand_segment(input));
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

    // The contract checks every field it is given and that every rule names
    // one of them. It runs on the fields that parsed (on a struct of the
    // wrong shape, under stand-in names); the ones that failed are passed as
    // skipped, so a rule naming one is still checked for its other names and
    // the failed field is not reported twice. The policy is independent of it.
    // a missing segment is already reported; the contract still checks the rest
    let field_specs = fields.into_iter().map(SubcommandField::into_quote_spec).collect();
    let quote_spec = SimpleSubcommandQuote::new(input.ident.clone(), input.vis.clone(), segment.clone().unwrap_or_default(), field_specs)
        .with_validation_rules(validation_rules);
    let contract_impls = errs.absorb(emit_simple_subcommand_contract_impls_skipping(&quote_spec, &failed, &rule_contexts));
    let policy = errs.absorb(parse_xccute_policy(input));

    let (Some(_), Some(contract_impls), Some(policy)) = (segment, contract_impls, policy) else {
        return errs.finish_with(TokenStream::new());
    };
    let policy_impl = emit_xccute_policy_metadata_impl(&input.ident, &policy);

    errs.finish_with(::quote::quote! {
        #contract_impls
        #policy_impl
    })
}

fn parse_subcommand_segment(input: &DeriveInput) -> syn::Result<String> {
    if !input.attrs.iter().any(|attr| attr.path().is_ident("subcommand")) {
        return Err(err_spanned(input, E::SubcmdMissingSegment));
    }
    let bag = crate::vocabulary::subcommand().validate(&AttrBag::from_attrs(&input.attrs, "subcommand")?)?;
    Ok(bag.require_str("segment")?.to_owned())
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
    let kind = errs.absorb(read_arg_kind(field));
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

    let field = SubcommandField {
        ident,
        ty: field.ty.clone(),
        method,
        kind,
        init_required,
    };
    (Some(field), rules)
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
    let arg = crate::vocabulary::subcommand_arg().validate(&AttrBag::from_attrs(&field.attrs, "arg")?)?;
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
