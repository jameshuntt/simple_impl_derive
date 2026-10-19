use super::*;
use crate::vocabulary;
use simple_impl_attr_kit::{AttrBag, AttrValue};

/// What step 1 hands the emitters: every field, plus the names of the fields
/// each emitter must skip because their own problems were already reported.
pub(super) struct Parsed {
    pub shell_struct: ShellCfg,
    pub fields: Vec<FieldInfo>,
    /// `#[builder]` unreadable or contradictory: the builder skips these.
    pub builder_failed: HashSet<String>,
    /// `#[shell]` unreadable: the shell skips these.
    pub shell_failed: HashSet<String>,
    /// `cmd` is known. When it is not, the struct's `#[shell]` (or its
    /// cmd_expr) already reported an error, and "missing cmd" would only echo.
    pub cmd_known: bool,
    /// `#[builder]` unreadable: the shell skips the decisions that need it
    /// (an implied positional) but still checks the field's #[shell].
    pub builder_unread: HashSet<String>,
    /// `order = ...` unreadable (the rest of `#[shell]` was read): the field is
    /// still checked, but not reported again for a missing order.
    pub order_failed: HashSet<String>,
    /// Where an `order(before = x)` / `order(after = x)` target was written,
    /// by field, so a target that names nothing is reported on the name.
    pub order_target_spans: HashMap<String, Span>,
}

/// step 1
///
/// we must deconstruct the struct into workable units. Every field is read and
/// checked; each problem goes into `errs` and the next field is still read, so
/// one compile reports them all. `None` only when there is no struct to read.
pub(super) fn parse_struct_and_fields(input: &DeriveInput, mode: ExpandMode, errs: &mut SynBuffer) -> Option<Parsed> {
    // the setter checks matter only where a builder is generated
    let makes_setters = matches!(mode, ExpandMode::BuilderOnly | ExpandMode::Both);
    let data = match &input.data {
        Data::Struct(s) => s,
        _ => {
            errs.push_at(input.span(), E::OnlyStructsSupported);
            return None;
        }
    };

    // create the shell config
    let mut shell_struct = ShellCfg::default();
    // mutate the
    let cmd_known = parse_shell_struct_attrs(&input.attrs, &mut shell_struct, errs);

    let fields_named = match &data.fields {
        Fields::Named(n) => n,
        _ => {
            errs.push_at(data.fields.span(), E::NamedFieldsRequired);
            return None;
        }
    };

    let mut fields = Vec::new();
    let mut builder_failed = HashSet::new();
    let mut shell_failed = HashSet::new();
    let mut order_failed = HashSet::new();
    let mut order_target_spans = HashMap::new();
    let mut builder_unread = HashSet::new();

    for (i, f) in fields_named.named.iter().enumerate() {
        let Some(ident) = f.ident.clone() else {
            errs.push_at(f.span(), E::NamedFieldsRequiredGeneral);
            continue;
        };

        let ty = f.ty.clone();

        let mut bcfg = BuilderCfg::default();
        let mut scfg = ShellFieldCfg::default();

        // An unreadable attribute leaves its config half-filled: the emitters
        // that read it skip this field rather than guess.
        let builder_read = {
            let read = parse_builder_field_attrs(&f.attrs, &mut bcfg, errs);
            errs.absorb(read).is_some()
        };
        let shell_bag = errs.absorb(read_shell_field_bag(&f.attrs));
        let shell_read = match &shell_bag {
            Some(bag) => {
                let read = parse_shell_field_keys(bag, &mut scfg, errs);
                errs.absorb(read).is_some()
            }
            None => false,
        };
        // a bad `order` leaves the rest of #[shell] read: the field is still
        // checked, and only the order checks pass over it
        if let Some(bag) = &shell_bag {
            match errs.absorb(parse_shell_field_order(bag, &mut scfg)) {
                Some(Some(span)) => {
                    order_target_spans.insert(ident.to_string(), span);
                }
                Some(None) => {}
                None => {
                    order_failed.insert(ident.to_string());
                }
            }
        }

        // If user says `not_in_default`, treat it as `required`
        // (handled in parser via bcfg.required = true)

        // init_required (or init_only+required) means ctor supplies the value;
        // default_expr would be confusing / contradictory.
        let builder_ok = [
            ensure!(errs, !(bcfg.init_only && bcfg.required && bcfg.default_expr.is_some()), f => E::InitRequiredConflictDefault),
            ensure!(errs, !(bcfg.init_only && bcfg.kind.is_some()), f => E::InitOnlyConflictKind),
            ensure!(errs, !(matches!(bcfg.kind, Some(BuilderKind::Push)) && type_vec_inner(&ty).is_none()), &ident => E::PushRequiresVec),
            // kind and method shape a setter; a field without one, or a derive
            // that makes none, never uses them
            ensure!(errs, !makes_setters || bcfg.skip_setter || !(matches!(bcfg.kind, Some(BuilderKind::Opt | BuilderKind::OptInto)) && type_option_inner(&ty).is_none()), &ident => E::OptKindRequiresOption),
            ensure!(errs, !makes_setters || bcfg.skip_setter || !(matches!(bcfg.kind, Some(BuilderKind::VecInto | BuilderKind::VecIter)) && type_vec_inner(&ty).is_none()), &ident => E::VecKindRequiresVec),
            ensure!(errs, !makes_setters || bcfg.skip_setter || bcfg.method.as_deref().is_none_or(is_ident), f => E::MethodNotIdent),
        ]
        .iter()
        .all(|held| *held);

        if !(builder_read && builder_ok) {
            builder_failed.insert(ident.to_string());
        }
        if !shell_read {
            shell_failed.insert(ident.to_string());
        }
        if !builder_read {
            builder_unread.insert(ident.to_string());
        }

        // If no explicit builder kind, infer something sane.
        if bcfg.kind.is_none() && !bcfg.skip_setter {
            bcfg.kind = Some(infer_builder_kind(&ty, &bcfg));
        }

        // If positional_mode not set, infer:
        if scfg.positional && scfg.positional_mode.is_none() {
            scfg.positional_mode = Some(infer_pos_mode(&ty));
        }

        fields.push(FieldInfo {
            ident,
            ty,
            builder: bcfg,
            shell: scfg,
            order: i,
        });
    }

    Some(Parsed { shell_struct, fields, builder_failed, shell_failed, cmd_known, order_failed, order_target_spans, builder_unread })
}

/// The method a `method = "..."` name makes: the identifier the compiler
/// itself makes from the text (`r#name` raw), as `format_ident!` always did,
/// so only the compiler's rules decide. Where it refuses (a space, a comment,
/// a character it does not take, `r#self`), this is `None` instead of a panic.
/// A word that is a keyword in every edition can never name a method, so it
/// is refused too; edition keywords (`async`, `dyn`, `try`, `gen`) pass.
pub(crate) fn method_ident(name: &str) -> Option<Ident> {
    const ALWAYS_KEYWORDS: &[&str] = &[
        "as", "break", "const", "continue", "crate", "else", "enum", "extern", "false", "fn", "for", "if", "impl", "in", "let", "loop",
        "match", "mod", "move", "mut", "pub", "ref", "return", "self", "Self", "static", "struct", "super", "trait", "true", "type",
        "unsafe", "use", "where", "while", "abstract", "become", "box", "do", "final", "macro", "override", "priv", "typeof", "unsized",
        "virtual", "yield", "_",
    ];
    if ALWAYS_KEYWORDS.contains(&name) {
        return None;
    }
    let make = || match name.strip_prefix("r#") {
        Some(body) => Ident::new_raw(body, Span::call_site()),
        None => Ident::new(name, Span::call_site()),
    };
    // inside a proc macro the compiler's panic hook keeps a caught panic silent
    std::panic::catch_unwind(make).ok()
}

/// `method = "..."` makes a method.
pub(crate) fn is_ident(name: &str) -> bool {
    method_ident(name).is_some()
}

/// The struct's `#[shell(...)]`: every key is read and each problem is
/// reported. Returns whether `cmd` is known: false when the attribute could
/// not be read or its cmd_expr did not parse, so "missing cmd" would only echo.
pub(super) fn parse_shell_struct_attrs(attrs: &[Attribute], cfg: &mut ShellCfg, errs: &mut SynBuffer) -> bool {
    let Some(bag) = errs.absorb(AttrBag::from_attrs(attrs, "shell").and_then(|bag| vocabulary::shell_struct().validate(&bag))) else {
        return false;
    };
    if let Some(cmd) = errs.absorb(bag.optional_str("cmd")).flatten() {
        cfg.cmd = Some(cmd.to_owned());
    }
    cfg.require_order = errs.absorb(bag.flag("require_order")).unwrap_or(false);
    if let Some(trait_path) = errs.absorb(bag.optional_str("trait_path")).flatten() {
        let span = bag.span_of("trait_path").unwrap_or_else(Span::call_site);
        cfg.trait_path = errs.absorb(syn::parse_str::<Path>(trait_path).map_err(|e| Error::new(span, format!("trait_path is not a path: {e}"))));
    }
    let mut cmd_known = true;
    if let Some(expr) = read_expr(&bag, "cmd_expr", errs) {
        cfg.cmd_expr = expr;
        cmd_known = cfg.cmd_expr.is_some() || cfg.cmd.is_some();
    }
    cmd_known
}

/// A field's `#[shell(...)]`, every key checked against the vocabulary.
fn read_shell_field_bag(attrs: &[Attribute]) -> Result<AttrBag, Error> {
    vocabulary::shell_field().validate(&AttrBag::from_attrs(attrs, "shell")?)
}

/// Every key but `order`. An arg_expr or opt_expr that does not parse is
/// reported into `errs` and the rest of the field is still read.
pub(super) fn parse_shell_field_keys(bag: &AttrBag, cfg: &mut ShellFieldCfg, errs: &mut SynBuffer) -> Result<(), Error> {

    let strings: [(&str, &mut Option<String>); 15] = [
        ("flag", &mut cfg.flag),
        ("flag_off", &mut cfg.flag_off),
        ("count_flag", &mut cfg.count_flag),
        ("opt_kv", &mut cfg.opt_kv),
        ("opt_prefix", &mut cfg.opt_prefix),
        ("opt_eq", &mut cfg.opt_eq),
        ("kv", &mut cfg.kv),
        ("prefix", &mut cfg.prefix),
        ("eq", &mut cfg.eq),
        ("multi_opt_kv", &mut cfg.multi_opt_kv),
        ("multi_opt_prefix", &mut cfg.multi_opt_prefix),
        ("multi_arg_flag", &mut cfg.multi_arg_flag),
        ("sep", &mut cfg.arg_join_sep),
        ("fmt", &mut cfg.fmt),
        ("join", &mut cfg.join),
    ];
    for (key, slot) in strings {
        if let Some(value) = bag.optional_str(key)? {
            *slot = Some(value.to_owned());
        }
    }

    if bag.flag("positional")? || bag.flag("arg")? {
        cfg.positional = true;
    }
    if bag.flag("arg_clone")? {
        cfg.positional = true;
        cfg.positional_mode = Some(PosMode::Clone);
    }
    if bag.flag("arg_display")? {
        cfg.positional = true;
        cfg.positional_mode = Some(PosMode::Display);
    }
    cfg.is_mode = bag.flag("mode")?;
    cfg.subcommand = bag.flag("subcommand")?;

    if let Some(expr) = read_expr(bag, "arg_expr", errs) {
        cfg.positional = true;
        // written but unparsable: a stand-in keeps it present for the other
        // checks; nothing is generated once anything was reported
        cfg.arg_expr = Some(expr.unwrap_or_default());
    }
    if let Some(expr) = read_expr(bag, "opt_expr", errs) {
        cfg.positional = true;
        cfg.opt_expr = Some(expr.unwrap_or_default());
    }
    if let Some(other) = bag.optional_str("arg_join_opt")? {
        cfg.positional = true;
        cfg.arg_join_opt_with = Some(other.to_owned());
    }

    Ok(())
}

/// `order = N`, or "first" / "last" / "before:FIELD" / "after:FIELD", or
/// `order(first)`, `order(last)`, `order(before = FIELD)`, `order(after = FIELD)`.
/// For the `order(...)` form with a target, returns where the target was
/// written, so a target that names nothing is reported on the name.
fn parse_shell_field_order(bag: &AttrBag, cfg: &mut ShellFieldCfg) -> Result<Option<Span>, Error> {
    let Some(value) = bag.get("order") else { return Ok(None) };
    let span = bag.span_of("order").unwrap_or_else(Span::call_site);
    match value {
        AttrValue::Int(n) => {
            let v = i32::try_from(*n).map_err(|_| Error::new(span, format!("order {n} does not fit an i32")))?;
            cfg.order = Some(ShellOrder::Key(v));
        }
        AttrValue::Str(raw) => {
            let raw_trim = raw.trim();
            let norm = raw_trim.to_ascii_lowercase();
            cfg.order = Some(match norm.as_str() {
                "first" => ShellOrder::First,
                "last" => ShellOrder::Last,
                _ if norm.starts_with("before:") => {
                    let target = raw_trim.split_once(':').map(|(_, t)| t).unwrap_or("").trim();
                    if target.is_empty() {
                        return Err(diag::err_at(span, E::OrderBeforeMissingTarget));
                    }
                    ShellOrder::Before(target.to_string())
                }
                _ if norm.starts_with("after:") => {
                    let target = raw_trim.split_once(':').map(|(_, t)| t).unwrap_or("").trim();
                    if target.is_empty() {
                        return Err(diag::err_at(span, E::OrderAfterMissingTarget));
                    }
                    ShellOrder::After(target.to_string())
                }
                _ => return Err(diag::err_at(span, E::InvalidOrderValue)),
            });
        }
        // order(first), order(last), order(before = FIELD), order(after = FIELD)
        AttrValue::Nested(inner) => {
            let keys: Vec<&String> = inner.keys().collect();
            let [key] = keys.as_slice() else { return Err(diag::err_at(span, E::InvalidOrderValue)) };
            let key = key.as_str();
            let flag = matches!(inner.get(key), Some(AttrValue::Bool(true)));
            match key {
                "first" if flag => cfg.order = Some(ShellOrder::First),
                "last" if flag => cfg.order = Some(ShellOrder::Last),
                "before" | "after" => {
                    let at = inner.span_of(key).unwrap_or(span);
                    // one target: order(before = b, before = c) is refused, not cut to b
                    if matches!(inner.get(key), Some(AttrValue::List(_))) {
                        return Err(diag::err_at(at, E::InvalidOrderValue));
                    }
                    let (target, target_span) = order_target(inner, key).ok_or_else(|| diag::err_at(at, E::InvalidOrderValue))?;
                    cfg.order = Some(if key == "before" { ShellOrder::Before(target) } else { ShellOrder::After(target) });
                    return Ok(Some(target_span));
                }
                _ => return Err(diag::err_at(span, E::InvalidOrderValue)),
            }
        }
        _ => return Err(diag::err_at(span, E::InvalidOrderType)),
    }
    Ok(None)
}

/// The field `order(before = x)` names: one identifier as written (or a
/// string), with where it was written.
fn order_target(inner: &AttrBag, key: &str) -> Option<(String, Span)> {
    let written = inner.written_tokens(key)?;
    let mut tokens = written.clone().into_iter();
    match (tokens.next(), tokens.next()) {
        (Some(proc_macro2::TokenTree::Ident(ident)), None) => Some((ident.to_string(), ident.span())),
        _ => {
            let target = inner.get(key)?.as_str()?.trim();
            (!target.is_empty()).then(|| (target.to_owned(), written.clone().into_iter().next().map_or_else(Span::call_site, |t| t.span())))
        }
    }
}

/// `key = <expression>`, written as code or as a string. `None` when the key
/// is not written. `Some(None)` when it is written but is not an expression
/// (reported into `errs`). Code is emitted with the tokens as written, so an
/// error the compiler finds inside it points at the attribute; a string is
/// parsed as before.
pub(crate) fn read_expr(bag: &AttrBag, key: &str, errs: &mut SynBuffer) -> Option<Option<TokenStream2>> {
    let value = bag.get(key)?;
    let span = bag.span_of(key).unwrap_or_else(Span::call_site);
    let expr = match value {
        AttrValue::Str(text) => syn::parse_str::<TokenStream2>(text).map_err(|e| Error::new(span, format!("{key} parse error: {e}"))),
        // code: checked here, so a syntax error is reported on the token
        // where it is, not at the derive; emitted as written
        _ => match bag.written_tokens(key) {
            Some(tokens) => syn::parse2::<syn::Expr>(tokens.clone()).map(|_| tokens.clone()).map_err(|e| {
                // an expression cut short ends past its last token, which syn
                // reports at the derive; put it on the last token written
                match tokens.clone().into_iter().last() {
                    Some(last) if e.to_string().starts_with("unexpected end of input") => Error::new(last.span(), e.to_string()),
                    _ => e,
                }
            }),
            None => Err(diag::err_at_ctx(span, E::ExprNotWritten, format!("`{key}`"))),
        },
    };
    Some(errs.absorb(expr))
}

/// A field's `#[builder(...)]`. A default_expr that does not parse is reported
/// into `errs` and still counts as written, so the checks on it still run.
pub(super) fn parse_builder_field_attrs(attrs: &[Attribute], cfg: &mut BuilderCfg, errs: &mut SynBuffer) -> Result<(), Error> {
    let bag = vocabulary::builder_field().validate(&AttrBag::from_attrs(attrs, "builder")?)?;

    if bag.flag("required")? || bag.flag("not_in_default")? {
        cfg.required = true;
    }
    cfg.into = bag.flag("into")?;
    cfg.method = bag.optional_str("method")?.map(ToOwned::to_owned);
    if bag.flag("skip")? {
        cfg.skip_setter = true;
    }
    if bag.flag("init_only")? {
        cfg.init_only = true;
        cfg.skip_setter = true;
    }
    if bag.flag("init_required")? {
        cfg.init_only = true;
        cfg.skip_setter = true;
        cfg.required = true;
    }
    for kind in vocabulary::BUILDER_KINDS {
        if bag.flag(kind)? {
            cfg.kind = Some(match kind {
                "flag" => BuilderKind::Flag,
                "opt" => BuilderKind::Opt,
                "opt_into" => BuilderKind::OptInto,
                "set" => BuilderKind::Set,
                "set_into" => BuilderKind::SetInto,
                "vec_into" => BuilderKind::VecInto,
                "vec_iter" => BuilderKind::VecIter,
                _ => BuilderKind::Push,
            });
        }
    }
    if let Some(expr) = read_expr(&bag, "default_expr", errs) {
        // written but unparsable: a stand-in keeps it present; nothing is generated
        cfg.default_expr = Some(expr.unwrap_or_default());
    }
    Ok(())
}

pub(super) fn infer_builder_kind(ty: &Type, cfg: &BuilderCfg) -> BuilderKind {
    if type_is_bool(ty) {
        return BuilderKind::Flag;
    }
    if let Some(inner) = type_option_inner(ty) {
        if type_is_string(inner) || cfg.into {
            return BuilderKind::OptInto;
        }
        return BuilderKind::Opt;
    }
    if let Some(inner) = type_vec_inner(ty) {
        // default to “most flexible”
        let _ = inner;
        return BuilderKind::VecIter;
    }
    if type_is_string(ty) || cfg.into {
        return BuilderKind::SetInto;
    }
    BuilderKind::Set
}

pub(super) fn infer_pos_mode(ty: &Type) -> PosMode {
    // String => clone; everything else => display
    if type_is_string(ty) {
        PosMode::Clone
    } else if let Some(inner) = type_option_inner(ty) {
        if type_is_string(inner) {
            PosMode::Clone
        } else {
            PosMode::Display
        }
    } else {
        PosMode::Display
    }
}


#[allow(unused)]
pub(super) fn infer_builder_kind_my_way(ty:&Type,cfg:&BuilderCfg)->BuilderKind{
    if type_is_bool(ty){return BuilderKind::Flag;}
    if let Some(inner) = type_option_inner(ty){
        if type_is_string(inner) || cfg.into {return BuilderKind::OptInto;}
        return BuilderKind::Opt;
    }
    if let Some(inner) = type_vec_inner(ty){
        // default to “most flexible”
        let _ = inner;
        return BuilderKind::VecIter;
    }
    if type_is_string(ty) || cfg.into {return BuilderKind::SetInto;}
    BuilderKind::Set
}
#[allow(unused)]
pub(super) fn infer_pos_mode_my_way(ty:&Type)->PosMode{
    // String => clone; everything else => display
    if type_is_string(ty){PosMode::Clone}
    else if let Some(inner)=type_option_inner(ty){
        if type_is_string(inner){PosMode::Clone}
        else{PosMode::Display}
    }else{PosMode::Display}
}









