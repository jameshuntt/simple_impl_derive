use super::*;
use crate::vocabulary;
use simple_impl_attr_kit::{AttrBag, AttrValue};

/// step 1
/// 
/// we must deconstruct the struct into workable units
pub(super) fn parse_struct_and_fields(input: &DeriveInput) -> Result<(ShellCfg, Vec<FieldInfo>), Error> {
    let data = match &input.data {
        Data::Struct(s) => s,
        _ => return Err(diag::err_at(input.span(), E::OnlyStructsSupported)),
    };

    // create the shell config
    let mut shell_struct = ShellCfg::default();
    // mutate the
    parse_shell_struct_attrs(&input.attrs, &mut shell_struct)?;

    let fields_named = match &data.fields {
        Fields::Named(n) => n,
        _ => return Err(diag::err_at(data.fields.span(), E::NamedFieldsRequired)),
    };

    let mut fields = Vec::new();

    for (i, f) in fields_named.named.iter().enumerate() {
        let ident = f
            .ident
            .clone()
            .ok_or_else(|| diag::err_at(f.span(), E::NamedFieldsRequiredGeneral))?;

        let ty = f.ty.clone();

        let mut bcfg = BuilderCfg::default();
        let mut scfg = ShellFieldCfg::default();

        parse_builder_field_attrs(&f.attrs, &mut bcfg)?;
        parse_shell_field_attrs(&f.attrs, &mut scfg)?;

        // If user says `not_in_default`, treat it as `required`
        // (handled in parser via bcfg.required = true)

        // init_required (or init_only+required) means ctor supplies the value;
        // default_expr would be confusing / contradictory.
        if bcfg.init_only && bcfg.required && bcfg.default_expr.is_some() {
            // return Err(diag::err_at(f.span(), E::InitRequiredConflictDefault));
            return Err(diag::err_spanned(f, E::InitRequiredConflictDefault));
        }

        if bcfg.init_only && bcfg.kind.is_some() {
            // return Err(diag::err_spanned(f.span(), diag::Code::InitOnlyConflictKind));
            return Err(diag::err_spanned(f, E::InitOnlyConflictKind));
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

    Ok((shell_struct, fields))
}

pub(super) fn parse_shell_struct_attrs(attrs: &[Attribute], cfg: &mut ShellCfg) -> Result<(), Error> {
    let bag = vocabulary::shell_struct().validate(&AttrBag::from_attrs(attrs, "shell")?)?;
    if let Some(cmd) = bag.optional_str("cmd")? {
        cfg.cmd = Some(cmd.to_owned());
    }
    if let Some(trait_path) = bag.optional_str("trait_path")? {
        let span = bag.span_of("trait_path").unwrap_or_else(Span::call_site);
        let path: Path = syn::parse_str(trait_path).map_err(|e| Error::new(span, format!("trait_path is not a path: {e}")))?;
        cfg.trait_path = Some(path);
    }
    cfg.require_order = bag.flag("require_order")?;
    Ok(())
}

pub(super) fn parse_shell_field_attrs(attrs: &[Attribute], cfg: &mut ShellFieldCfg) -> Result<(), Error> {
    let bag = vocabulary::shell_field().validate(&AttrBag::from_attrs(attrs, "shell")?)?;

    let strings: [(&str, &mut Option<String>); 13] = [
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

    if let Some(expr) = bag.optional_str("arg_expr")? {
        let span = bag.span_of("arg_expr").unwrap_or_else(Span::call_site);
        let ts: TokenStream2 = syn::parse_str(expr).map_err(|e| Error::new(span, format!("arg_expr parse error: {e}")))?;
        cfg.positional = true;
        cfg.arg_expr = Some(ts);
    }
    if let Some(other) = bag.optional_str("arg_join_opt")? {
        cfg.positional = true;
        cfg.arg_join_opt_with = Some(other.to_owned());
    }

    // order = N, or "first" / "last" / "before:FIELD" / "after:FIELD"
    if let Some(value) = bag.get("order") {
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
            _ => return Err(diag::err_at(span, E::InvalidOrderType)),
        }
    }
    Ok(())
}

pub(super) fn parse_builder_field_attrs(attrs: &[Attribute], cfg: &mut BuilderCfg) -> Result<(), Error> {
    let bag = vocabulary::builder_field().validate(&AttrBag::from_attrs(attrs, "builder")?)?;

    if bag.flag("required")? || bag.flag("not_in_default")? {
        cfg.required = true;
    }
    cfg.into = bag.flag("into")?;
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
    if let Some(expr) = bag.optional_str("default_expr")? {
        let span = bag.span_of("default_expr").unwrap_or_else(Span::call_site);
        let ts: TokenStream2 = syn::parse_str(expr).map_err(|e| Error::new(span, format!("default_expr parse error: {e}")))?;
        cfg.default_expr = Some(ts);
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









