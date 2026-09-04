use super::*;

pub(super) fn expand_shell_impl(
    input: &DeriveInput,
    shell_struct: &ShellCfg,
    fields: &[FieldInfo],
) -> Result<TokenStream2, Error> {
    let name = &input.ident;
    let (impl_generics, ty_generics, where_clause) = input.generics.split_for_impl();

    let cmd_init: TokenStream2 = match (&shell_struct.cmd, &shell_struct.cmd_expr) {
        (Some(cmd), _) => {
            let cmd_lit = LitStr::new(cmd, input.span());
            quote! { #cmd_lit.to_string() }
        }
        (None, Some(expr)) => quote! { (#expr).to_string() },
        (None, None) => {
            return Err(Error::new(input.span(), "missing #[shell(cmd = \"...\")] or #[shell(cmd_expr = \"...\")] on struct"))
        }
    };

    let trait_path: Path = match &shell_struct.trait_path {
        Some(p) => p.clone(),
        None => syn::parse_str("::simple_impl::ShellCommand")
            .map_err(|e| Error::new(input.span(), format!("default trait_path parse error: {e}")))?,
    };

    // by_name: field-name -> (&Ident, &Type)
    let by_name: HashMap<String, (&Ident, &Type)> = fields
        .iter()
        .map(|f| (f.ident.to_string(), (&f.ident, &f.ty)))
        .collect();

    // --------------------------------------------------------------------
    // ordered emission (topo sort for before/after + priority key)
    // --------------------------------------------------------------------
    #[derive(Clone)]
    struct EmitItem {
        name: String,
        base: i32, // priority / numeric key
        tie: usize, // stable tie-breaker: struct field order
        tokens: TokenStream2,
        span: Span,
        rel: Option<ShellOrder>, // only Before/After will appear here
    }
    const ORD_SUBCMD: i32 = 500; // right after cmd, before flags
    // Default bands (used when no explicit order key exists)
    // These preserve your previous “flags then opts then args” behavior.
    const ORD_FLAG: i32 = 1000;
    const ORD_OPT_KV: i32 = 2000;
    const ORD_OPT_PREFIX: i32 = 2100;
    const ORD_OPT_EQ: i32 = 2200;
    const ORD_POS: i32 = 3000;
    const ORD_POS_TAIL: i32 = 4000; // init_required implied args go after normal positionals

    // Extremes for "first"/"last"
    const ORD_FIRST: i32 = i32::MIN / 2;
    const ORD_LAST: i32 = i32::MAX / 2;

    // fn shell_kind_count(s: &ShellFieldCfg) -> usize {
    //     let mut n = 0;
    //     if s.flag.is_some()       { n += 1; }
    //     if s.opt_kv.is_some()     { n += 1; }
    //     if s.opt_prefix.is_some() { n += 1; }
    //     if s.opt_eq.is_some()     { n += 1; }
    //     if s.positional           { n += 1; }
    //     n
    // }
    fn shell_kind_count(s: &ShellFieldCfg) -> usize {
        let mut n = 0;

        // treat flag + flag_off as ONE “kind”
        if s.flag.is_some() || s.flag_off.is_some() { n += 1; }

        if s.opt_kv.is_some() { n += 1; }
        if s.opt_prefix.is_some() { n += 1; }
        if s.opt_eq.is_some() { n += 1; }

        if s.multi_opt_kv.is_some() { n += 1; }
        if s.multi_opt_prefix.is_some() { n += 1; }
        if s.multi_arg_flag.is_some() { n += 1; }
        if s.count_flag.is_some() { n += 1; }

        if s.is_mode { n += 1; }
        if s.subcommand { n += 1; }

        if s.positional { n += 1; }

        if s.kv.is_some() { n += 1; }
        if s.prefix.is_some() { n += 1; }
        if s.eq.is_some() { n += 1; }
        n
    }


    fn base_order(f: &FieldInfo, band: i32) -> (i32, Option<ShellOrder>) {
        match &f.shell.order {
            Some(ShellOrder::Key(n)) => (*n, None),
            Some(ShellOrder::First) => (ORD_FIRST, None),
            Some(ShellOrder::Last) => (ORD_LAST, None),
            Some(ShellOrder::Before(t)) => (band + f.order as i32, Some(ShellOrder::Before(t.clone()))),
            Some(ShellOrder::After(t)) => (band + f.order as i32, Some(ShellOrder::After(t.clone()))),
            None => (band + f.order as i32, None),
        }
    }

    /// `(v).to_string()`, or `format!(fmt, v)` when the field carries `fmt`.
    fn value_tokens(f: &FieldInfo, v: TokenStream2) -> TokenStream2 {
        match &f.shell.fmt {
            Some(fmt) => {
                let fmt_lit = LitStr::new(fmt, f.ident.span());
                quote! { ::std::format!(#fmt_lit, #v) }
            }
            None => quote! { (#v).to_string() },
        }
    }

    /// `(v).clone()`, or `format!(fmt, v)` when the field carries `fmt`.
    fn clone_tokens(f: &FieldInfo, v: TokenStream2) -> TokenStream2 {
        match &f.shell.fmt {
            Some(fmt) => {
                let fmt_lit = LitStr::new(fmt, f.ident.span());
                quote! { ::std::format!(#fmt_lit, #v) }
            }
            None => quote! { (#v).clone() },
        }
    }

    /// The four value shapes every emitter pushes, for the field at hand.
    fn shapes(f: &FieldInfo) -> (TokenStream2, TokenStream2, TokenStream2, TokenStream2) {
        let fid = &f.ident;
        (
            value_tokens(f, quote! { v }),
            clone_tokens(f, quote! { v }),
            value_tokens(f, quote! { self.#fid }),
            clone_tokens(f, quote! { self.#fid }),
        )
    }

    /// `parts.push(<elements joined by `join`>)`, with `fmt` applied per element.
    fn joined_tokens(f: &FieldInfo, list: TokenStream2, join: &str) -> TokenStream2 {
        let join_lit = LitStr::new(join, f.ident.span());
        let per = value_tokens(f, quote! { v });
        quote! {
            parts.push((#list).iter().map(|v| #per).collect::<::std::vec::Vec<::std::string::String>>().join(#join_lit));
        }
    }

    fn emit_positional_tokens(
        f: &FieldInfo,
        by_name: &HashMap<String, (&Ident, &Type)>,
    ) -> Result<TokenStream2, Error> {
        let field = &f.ident;
        let ty = &f.ty;
        let (_vt, _vc, _vs, _vsc) = shapes(f);

        // 0) a Vec joined into one argument
        if let (Some(join), Some(_)) = (&f.shell.join, type_vec_inner(ty)) {
            let push = joined_tokens(f, quote! { self.#field }, join);
            return Ok(quote! { if !self.#field.is_empty() { #push } });
        }

        // 0.1) a Vec without join: every element is its own argument
        if type_vec_inner(ty).is_some() && f.shell.arg_expr.is_none() && f.shell.opt_expr.is_none() && f.shell.arg_join_opt_with.is_none() {
            return Ok(quote! { for v in &self.#field { parts.push(#_vt); } });
        }

        // 0.5) opt_expr: pushed only when the expression is Some
        if let Some(expr) = &f.shell.opt_expr {
            return Ok(quote! {
                if let ::core::option::Option::Some(v) = (#expr) {
                    parts.push(#_vt);
                }
            });
        }

        // 1) arg_expr escape hatch
        if let Some(expr) = &f.shell.arg_expr {
            return Ok(quote! {
                parts.push((#expr).to_string());
            });
        }

        // 2) arg_join_opt = "other", sep=":"
        if let Some(with_name) = &f.shell.arg_join_opt_with {
            let sep = f
                .shell
                .arg_join_sep
                .clone()
                .unwrap_or_else(|| ":".to_string());
            let sep_lit = LitStr::new(&sep, f.ident.span());

            let (other_ident, other_ty) = by_name.get(with_name).copied().ok_or_else(|| {
                // diag::err_spanned(f.ident, diag::Code::ArgJoinUnknownField(with_name))
                diag::err_spanned(&f.ident, E::ArgJoinUnknownField)
            })?;

            // base string from self.<field>
            let base_expr = if type_is_string(ty) {
                quote! { self.#field.clone() }
            } else {
                quote! { self.#field.to_string() }
            };

            // if other is Option<T>, only append if Some
            if type_option_inner(other_ty).is_some() {
                return Ok(quote! {
                    let mut __s = #base_expr;
                    if let ::core::option::Option::Some(v) = &self.#other_ident {
                        __s.push_str(#sep_lit);
                        __s.push_str(&v.to_string());
                    }
                    parts.push(__s);
                });
            }

            // if other is non-Option, always append
            return Ok(quote! {
                let mut __s = #base_expr;
                __s.push_str(#sep_lit);
                __s.push_str(&self.#other_ident.to_string());
                parts.push(__s);
            });
        }

        // 3) existing positional behavior:
        let mode = f.shell.positional_mode.unwrap_or_else(|| infer_pos_mode(ty));

        if type_option_inner(ty).is_some() {
            Ok(match mode {
                PosMode::Clone => quote! {
                    if let ::core::option::Option::Some(v) = &self.#field {
                        parts.push(#_vc);
                    }
                },
                PosMode::Display => quote! {
                    if let ::core::option::Option::Some(v) = &self.#field {
                        parts.push(#_vt);
                    }
                },
            })
        } else {
            Ok(match mode {
                PosMode::Clone => quote! { parts.push(#_vsc); },
                PosMode::Display => quote! { parts.push(#_vs); },
            })
        }
    }

    let mut items: Vec<EmitItem> = Vec::new();

    let mut errs = diag::Errors::default();

    // (optional) enforce only one subcommand field per struct
    let mut saw_subcommand = false;
    
    for f in fields {
        let k = shell_kind_count(&f.shell);

        // NEW: implied positional emission for init_required fields
        let implied_positional =
            // init_required = init_only + required
            (f.builder.init_only && f.builder.required)
            // avoid accidental "true/false" pushes
            && !type_is_bool(&f.ty)
            // required positional should not be Option<T>
            && type_option_inner(&f.ty).is_none();

        
        // Field not used by shell emission:
        if k == 0 {
            // sep without join is still invalid
            if f.shell.arg_join_sep.is_some() {
                errs.push_spanned(f.ident.clone(), E::SepWithoutJoin);
            }

//             guard!(f.shell.arg_join_sep.is_none(), {
//                 errs.push_spanned(f.ident.clone(), E::SepWithoutJoin);
//             });
//             
//             // Then you can use your original logic:
//             check!(f.shell.arg_join_sep.is_some(), {
//                 errs.push_spanned(f.ident.clone(), E::SepWithoutJoin);
//             });

            // If the user set order but provided no emission kind, allow it ONLY if
            // we will infer positional emission (init_required).
            if !implied_positional {
                if f.shell.order.is_some() {
                    errs.push_spanned(f.ident.clone(), E::OrderOnNonEmitted);
                }
                continue;
            }

            // Strict mode: require explicit order for inferred emitted fields too
            if shell_struct.require_order && f.shell.order.is_none() {
                errs.push_spanned(f.ident.clone(), E::RequireOrderMissing);
            }

            let (_vt, _vc, _vs, _vsc) = shapes(f);
            // Emit unconditional positional push:
            // String => clone push, others => to_string push
            let tokens = if type_is_string(&f.ty) {
                quote! { parts.push(#_vsc); }
            } else {
                quote! { parts.push(#_vs); }
            };

            let (base, rel) = base_order(f, ORD_POS_TAIL);

            let (_vt, _vc, _vs, _vsc) = shapes(f);

            items.push(EmitItem {
                name: f.ident.to_string(),
                base,
                tie: f.order,
                tokens,
                span: f.ident.span(),
                rel,
            });

            continue;
        }

        // fmt shapes a pushed value; kinds that build their own token refuse it
        if f.shell.fmt.is_some()
            && (f.shell.flag.is_some() || f.shell.flag_off.is_some() || f.shell.count_flag.is_some()
                || f.shell.prefix.is_some() || f.shell.opt_prefix.is_some() || f.shell.multi_opt_prefix.is_some()
                || f.shell.eq.is_some() || f.shell.opt_eq.is_some()
                || f.shell.arg_join_opt_with.is_some() || f.shell.arg_expr.is_some() || f.shell.opt_expr.is_some())
        {
            errs.push_spanned(f.ident.clone(), E::FmtOnWrongKind);
        }
        if let Some(fmt) = &f.shell.fmt {
            if fmt.matches("{}").count() != 1 {
                errs.push_spanned(f.ident.clone(), E::FmtNeedsOnePlaceholder);
            }
        }
        if f.shell.join.is_some()
            && f.shell.multi_arg_flag.is_none()
            && f.shell.multi_opt_kv.is_none()
            && !(f.shell.positional && f.shell.arg_expr.is_none() && f.shell.opt_expr.is_none() && f.shell.arg_join_opt_with.is_none() && type_vec_inner(&f.ty).is_some())
        {
            errs.push_spanned(f.ident.clone(), E::JoinOnWrongKind);
        }

        // Hygiene: exactly one emission kind per field
        if k > 1 {
            errs.push_spanned(f.ident.clone(), E::MultipleEmissionKinds);
        }

        // Validation: sep only with arg_join_opt
        if f.shell.arg_join_sep.is_some() && f.shell.arg_join_opt_with.is_none() {
            errs.push_spanned(f.ident.clone(), E::SepWithoutJoin);
        }

        // Strict mode: require explicit order for all emitted fields
        if shell_struct.require_order && f.shell.order.is_none() {
            errs.push_spanned(f.ident.clone(), E::RequireOrderMissing);
        }

//         // Hygiene: exactly one emission kind per field
//         ensure!(k <= 1, errs, f.ident.clone(), E::MultipleEmissionKinds);
// 
//         // Validation: sep only with arg_join_opt
//         let has_sep = f.shell.arg_join_sep.is_some();
//         let has_join = f.shell.arg_join_opt_with.is_some();
//         ensure!(!(has_sep && !has_join), errs, f.ident.clone(), E::SepWithoutJoin);

        // Build the ONE token block per field, with base order + possible before/after constraint.
        // if let Some(flag) = &f.shell.flag {
        //     let field = &f.ident;
        //     let flag_lit = LitStr::new(flag, f.ident.span());
        //     let (base, rel) = base_order(f, ORD_FLAG);

        //     items.push(EmitItem {
        //         name: f.ident.to_string(),
        //         base,
        //         tie: f.order,
        //         span: f.ident.span(),
        //         rel,
        //         tokens: quote! {
        //             if self.#field {
        //                 parts.push(#flag_lit.to_string());
        //             }
        //         },
        //     });
        //     continue;
        // }
// opt_kv / opt_prefix / opt_eq must be Option<T>
if f.shell.opt_kv.is_some() && type_option_inner(&f.ty).is_none() {
    errs.push_spanned(f.ident.clone(), E::OptKvRequiresOption);
}
if f.shell.opt_prefix.is_some() && type_option_inner(&f.ty).is_none() {
    errs.push_spanned(f.ident.clone(), E::OptPrefixRequiresOption);
}
if f.shell.opt_eq.is_some() && type_option_inner(&f.ty).is_none() {
    errs.push_spanned(f.ident.clone(), E::OptEqRequiresOption);
}
        if f.shell.subcommand {
            if saw_subcommand {
                return Err(Error::new(
                    f.ident.span(),
                    "multiple #[shell(subcommand)] fields; only one subcommand is supported",
                ));
            }
            saw_subcommand = true;

            let tokens = emit_positional_tokens(f, &by_name)?;
            let (base, rel) = base_order(f, ORD_SUBCMD);
            let (_vt, _vc, _vs, _vsc) = shapes(f);
            items.push(EmitItem {
                name: f.ident.to_string(),
                base,
                tie: f.order,
                span: f.ident.span(),
                rel,
                tokens,
            });
            continue;
        }

        if f.shell.is_mode {
            let field = &f.ident;
            let (base, rel) = base_order(f, ORD_FLAG);
            let (_vt, _vc, _vs, _vsc) = shapes(f);
            let tokens = if type_option_inner(&f.ty).is_some() {
                quote! {
                    if let ::core::option::Option::Some(v) = &self.#field {
                        v.__simple_impl_shell_mode_emit(&mut parts);
                    }
                }
            } else {
                quote! {
                    self.#field.__simple_impl_shell_mode_emit(&mut parts);
                }
            };

            items.push(EmitItem {
                name: f.ident.to_string(),
                base,
                tie: f.order,
                span: f.ident.span(),
                rel,
                tokens,
            });
            continue;
        }

        if let Some(ch) = &f.shell.count_flag {
            if !type_is_uint(&f.ty) {
                return Err(Error::new(
                    f.ident.span(),
                    "#[shell(count_flag = \"...\")] requires an unsigned integer field (u8/u16/u32/u64/usize)",
                ));
            }

            let field = &f.ident;
            let ch_lit = LitStr::new(ch, f.ident.span());
            let (base, rel) = base_order(f, ORD_FLAG);
            let (_vt, _vc, _vs, _vsc) = shapes(f);

            items.push(EmitItem {
                name: f.ident.to_string(),
                base,
                tie: f.order,
                span: f.ident.span(),
                rel,
                tokens: quote! {
                    let __n: usize = self.#field as usize;
                    if __n > 0 {
                        let mut __s = ::std::string::String::from("-");
                        for _ in 0..__n {
                            __s.push_str(#ch_lit);
                        }
                        parts.push(__s);
                    }
                },
            });
            continue;
        }

        if let Some(flag) = &f.shell.multi_opt_kv {
            let field = &f.ident;
            let flag_lit = LitStr::new(flag, f.ident.span());
            let (base, rel) = base_order(f, ORD_OPT_KV);
            let (_vt, _vc, _vs, _vsc) = shapes(f);

            if let Some(join) = &f.shell.join {
                let tokens = if type_vec_inner(&f.ty).is_some() {
                    let push = joined_tokens(f, quote! { self.#field }, join);
                    quote! { if !self.#field.is_empty() { parts.push(#flag_lit.to_string()); #push } }
                } else if type_option_vec_inner(&f.ty).is_some() {
                    let push = joined_tokens(f, quote! { list }, join);
                    quote! {
                        if let ::core::option::Option::Some(list) = &self.#field {
                            if !list.is_empty() { parts.push(#flag_lit.to_string()); #push }
                        }
                    }
                } else {
                    return Err(diag::err_spanned(&f.ident, E::MultiArgFlagRequiresVec));
                };
                items.push(EmitItem { name: f.ident.to_string(), base, tie: f.order, span: f.ident.span(), rel, tokens });
                continue;
            }

            let tokens = if type_vec_inner(&f.ty).is_some() {
                quote! {
                    for v in &self.#field {
                        parts.push(#flag_lit.to_string());
                        parts.push(#_vt);
                    }
                }
            } else if type_option_vec_inner(&f.ty).is_some() {
                quote! {
                    if let ::core::option::Option::Some(list) = &self.#field {
                        for v in list {
                            parts.push(#flag_lit.to_string());
                            parts.push(#_vt);
                        }
                    }
                }
            } else {
                return Err(Error::new(
                    f.ident.span(),
                    "#[shell(multi_opt_kv = \"...\")] requires Vec<T> or Option<Vec<T>>",
                ));
            };

            items.push(EmitItem { name: f.ident.to_string(), base, tie: f.order, span: f.ident.span(), rel, tokens });
            continue;
        }
        
        if let Some(prefix) = &f.shell.multi_opt_prefix {
            let field = &f.ident;
            let prefix_lit = LitStr::new(prefix, f.ident.span());
            let (base, rel) = base_order(f, ORD_OPT_PREFIX);
            let (_vt, _vc, _vs, _vsc) = shapes(f);

            let tokens = if type_vec_inner(&f.ty).is_some() {
                quote! {
                    for v in &self.#field {
                        parts.push(::std::format!("{}{}", #prefix_lit, v));
                    }
                }
            } else if type_option_vec_inner(&f.ty).is_some() {
                quote! {
                    if let ::core::option::Option::Some(list) = &self.#field {
                        for v in list {
                            parts.push(::std::format!("{}{}", #prefix_lit, v));
                        }
                    }
                }
            } else {
                return Err(Error::new(
                    f.ident.span(),
                    "#[shell(multi_opt_prefix = \"...\")] requires Vec<T> or Option<Vec<T>>",
                ));
            };

            items.push(EmitItem { name: f.ident.to_string(), base, tie: f.order, span: f.ident.span(), rel, tokens });
            continue;
        }

        if let Some(flag) = &f.shell.multi_arg_flag {
            let field = &f.ident;
            let flag_lit = LitStr::new(flag, f.ident.span());
            let (base, rel) = base_order(f, ORD_OPT_KV);
            let (_vt, _vc, _vs, _vsc) = shapes(f);

            if let Some(join) = &f.shell.join {
                let tokens = if type_vec_inner(&f.ty).is_some() {
                    let push = joined_tokens(f, quote! { self.#field }, join);
                    quote! { if !self.#field.is_empty() { parts.push(#flag_lit.to_string()); #push } }
                } else if type_option_vec_inner(&f.ty).is_some() {
                    let push = joined_tokens(f, quote! { list }, join);
                    quote! {
                        if let ::core::option::Option::Some(list) = &self.#field {
                            if !list.is_empty() { parts.push(#flag_lit.to_string()); #push }
                        }
                    }
                } else {
                    return Err(diag::err_spanned(&f.ident, E::MultiArgFlagRequiresVec));
                };
                items.push(EmitItem { name: f.ident.to_string(), base, tie: f.order, span: f.ident.span(), rel, tokens });
                continue;
            }

            let tokens = if type_vec_inner(&f.ty).is_some() {
                quote! {
                    if !self.#field.is_empty() {
                        parts.push(#flag_lit.to_string());
                        for v in &self.#field {
                            parts.push(#_vt);
                        }
                    }
                }
            } else if type_option_vec_inner(&f.ty).is_some() {
                quote! {
                    if let ::core::option::Option::Some(list) = &self.#field {
                        if !list.is_empty() {
                            parts.push(#flag_lit.to_string());
                            for v in list {
                                parts.push(#_vt);
                            }
                        }
                    }
                }
            } else {
                return Err(Error::new(
                    f.ident.span(),
                    "#[shell(multi_arg_flag = \"...\")] requires Vec<T> or Option<Vec<T>>",
                ));
            };

            items.push(EmitItem { name: f.ident.to_string(), base, tie: f.order, span: f.ident.span(), rel, tokens });
            continue;
        }
if (f.shell.flag.is_some() || f.shell.flag_off.is_some()) && !type_is_bool(&f.ty) {
    errs.push_spanned(f.ident.clone(), E::FlagRequiresBool);
}
        // Boolean on/off (toggle) + plain flag-only
        if f.shell.flag.is_some() || f.shell.flag_off.is_some() {
            if !type_is_bool(&f.ty) {
                return Err(Error::new(
                    f.ident.span(),
                    "shell flag/flag_off require a bool field",
                ));
            }

            let field = &f.ident;
            let (base, rel) = base_order(f, ORD_FLAG);
            let (_vt, _vc, _vs, _vsc) = shapes(f);

            let tokens = match (&f.shell.flag, &f.shell.flag_off) {
                (Some(on), Some(off)) => {
                    let on_lit = LitStr::new(on, f.ident.span());
                    let off_lit = LitStr::new(off, f.ident.span());
                    quote! {
                        if self.#field {
                            parts.push(#on_lit.to_string());
                        } else {
                            parts.push(#off_lit.to_string());
                        }
                    }
                }
                (Some(on), None) => {
                    let on_lit = LitStr::new(on, f.ident.span());
                    quote! { if self.#field { parts.push(#on_lit.to_string()); } }
                }
                (None, Some(off)) => {
                    let off_lit = LitStr::new(off, f.ident.span());
                    quote! { if !self.#field { parts.push(#off_lit.to_string()); } }
                }
                (None, None) => unreachable!(),
            };

            items.push(EmitItem { name: f.ident.to_string(), base, tie: f.order, span: f.ident.span(), rel, tokens });
            continue;
        }

        if let Some(flag) = &f.shell.opt_kv {
            let field = &f.ident;
            let flag_lit = LitStr::new(flag, f.ident.span());
            let (base, rel) = base_order(f, ORD_OPT_KV);
            let (_vt, _vc, _vs, _vsc) = shapes(f);

            items.push(EmitItem {
                name: f.ident.to_string(),
                base,
                tie: f.order,
                span: f.ident.span(),
                rel,
                tokens: quote! {
                    if let ::core::option::Option::Some(v) = &self.#field {
                        parts.push(#flag_lit.to_string());
                        parts.push(#_vt);
                    }
                },
            });
            continue;
        }
        
        if let Some(flag) = &f.shell.kv {
            let field = &f.ident;
            let flag_lit = LitStr::new(flag, f.ident.span());
            let (base, rel) = base_order(f, ORD_OPT_KV);
            let (_vt, _vc, _vs, _vsc) = shapes(f);

            let tokens = if type_option_inner(&f.ty).is_some() {
                quote! {
                    if let ::core::option::Option::Some(v) = &self.#field {
                        parts.push(#flag_lit.to_string());
                        parts.push(#_vt);
                    }
                }
            } else {
                quote! {
                    parts.push(#flag_lit.to_string());
                    parts.push(#_vs);
                }
            };

            items.push(EmitItem {
                name: f.ident.to_string(),
                base,
                tie: f.order,
                span: f.ident.span(),
                rel,
                tokens,
            });
            continue;
        }

        if let Some(prefix) = &f.shell.opt_prefix {
            let field = &f.ident;
            let prefix_lit = LitStr::new(prefix, f.ident.span());
            let (base, rel) = base_order(f, ORD_OPT_PREFIX);
            let (_vt, _vc, _vs, _vsc) = shapes(f);

            items.push(EmitItem {
                name: f.ident.to_string(),
                base,
                tie: f.order,
                span: f.ident.span(),
                rel,
                tokens: quote! {
                    if let ::core::option::Option::Some(v) = &self.#field {
                        parts.push(::std::format!("{}{}", #prefix_lit, v));
                    }
                },
            });
            continue;
        }
        
        if let Some(prefix) = &f.shell.prefix {
            let field = &f.ident;
            let prefix_lit = LitStr::new(prefix, f.ident.span());
            let (base, rel) = base_order(f, ORD_OPT_PREFIX);
            let (_vt, _vc, _vs, _vsc) = shapes(f);

            let tokens = if type_option_inner(&f.ty).is_some() {
                quote! {
                    if let ::core::option::Option::Some(v) = &self.#field {
                        parts.push(::std::format!("{}{}", #prefix_lit, v));
                    }
                }
            } else {
                quote! {
                    parts.push(::std::format!("{}{}", #prefix_lit, self.#field));
                }
            };

            items.push(EmitItem {
                name: f.ident.to_string(),
                base,
                tie: f.order,
                span: f.ident.span(),
                rel,
                tokens,
            });
            continue;
        }

        if let Some(flag) = &f.shell.opt_eq {
            let field = &f.ident;
            let flag_lit = LitStr::new(flag, f.ident.span());
            let (base, rel) = base_order(f, ORD_OPT_EQ);
            let (_vt, _vc, _vs, _vsc) = shapes(f);

            items.push(EmitItem {
                name: f.ident.to_string(),
                base,
                tie: f.order,
                span: f.ident.span(),
                rel,
                tokens: quote! {
                    if let ::core::option::Option::Some(v) = &self.#field {
                        parts.push(::std::format!("{}={}", #flag_lit, v));
                    }
                },
            });
            continue;
        }

        if let Some(flag) = &f.shell.eq {
            let field = &f.ident;
            let flag_lit = LitStr::new(flag, f.ident.span());
            let (base, rel) = base_order(f, ORD_OPT_EQ);
            let (_vt, _vc, _vs, _vsc) = shapes(f);

            let tokens = if type_option_inner(&f.ty).is_some() {
                quote! {
                    if let ::core::option::Option::Some(v) = &self.#field {
                        parts.push(::std::format!("{}={}", #flag_lit, v));
                    }
                }
            } else {
                quote! {
                    parts.push(::std::format!("{}={}", #flag_lit, self.#field));
                }
            };

            items.push(EmitItem {
                name: f.ident.to_string(),
                base,
                tie: f.order,
                span: f.ident.span(),
                rel,
                tokens,
            });
            continue;
        }

        if f.shell.positional {
            let pos_tokens = emit_positional_tokens(f, &by_name)?;
            let (base, rel) = base_order(f, ORD_POS);
            let (_vt, _vc, _vs, _vsc) = shapes(f);

            items.push(EmitItem {
                name: f.ident.to_string(),
                base,
                tie: f.order,
                span: f.ident.span(),
                rel,
                tokens: pos_tokens,
            });
            continue;
        }

        // Should be unreachable (k>0 means one of the above matched)
        errs.push_spanned(f.ident.clone(), E::Unmatched);    
    }

    // 3. The "Explosion" point
    // If any fields had errors, this returns Err and stops the macro.
    errs.finish()?;

    // -------------------------
    // Build constraint graph (before/after) over emitted items
    // -------------------------
    let n = items.len();

    let mut idx_exact: HashMap<String, usize> = HashMap::with_capacity(n);
    let mut idx_lower: HashMap<String, usize> = HashMap::with_capacity(n);

    for (i, it) in items.iter().enumerate() {
        idx_exact.insert(it.name.clone(), i);

        let lower = it.name.to_ascii_lowercase();
        if let Some(prev) = idx_lower.insert(lower, i) {
            // If two different fields collide case-insensitively, error out.
            if prev != i {
                return Err(Error::new(
                    it.span,
                    format!(
                        "case-insensitive order target collision between `{}` and `{}`; \
                        use exact field names in order=\"before:...\"/\"after:...\"",
                        items[prev].name, it.name
                    ),
                ));
            }
        }
    }

    // Helper: resolve a target name case-insensitively
    let resolve_target =
        |target: &str, span: Span|
    -> Result<usize, Error> {
        if let Some(&j) = idx_exact.get(target) {
            return Ok(j);
        }
        let key = target.to_ascii_lowercase();
        idx_lower.get(&key).copied().ok_or_else(|| {
            Error::new(
                span,
                format!(
                    "order target `{}` references unknown (or non-emitted) field",
                    target
                ),
            )
        })
    };

    let mut adj: Vec<Vec<usize>> = vec![Vec::new(); n];
    let mut indeg: Vec<usize> = vec![0; n];

    for i in 0..n {
        let Some(rel) = items[i].rel.clone() else { continue };
        match rel {
            ShellOrder::Before(target) => {
                let j = resolve_target(&target, items[i].span)?;
                adj[i].push(j);
                indeg[j] += 1;
            }
            ShellOrder::After(target) => {
                let j = resolve_target(&target, items[i].span)?;
                adj[j].push(i);
                indeg[i] += 1;
            }
            _ => {}
        }
    }

    // -------------------------
    // Stable topo sort with priority:
    // choose the smallest (base, tie) among available nodes
    // -------------------------
    let mut heap: BinaryHeap<Reverse<(i32, usize, usize)>> = BinaryHeap::new();
    for i in 0..n {
        if indeg[i] == 0 {
            heap.push(Reverse((items[i].base, items[i].tie, i)));
        }
    }

    let mut order: Vec<usize> = Vec::with_capacity(n);
    while let Some(Reverse((_base, _tie, i))) = heap.pop() {
        order.push(i);
        for &j in &adj[i] {
            indeg[j] -= 1;
            if indeg[j] == 0 {
                heap.push(Reverse((items[j].base, items[j].tie, j)));
            }
        }
    }

    if order.len() != n {
        let stuck = (0..n).find(|&i| indeg[i] > 0).unwrap_or(0);
        return Err(diag::err_at(items[stuck].span, E::OrderCycle));
    }

    let emitted = order.into_iter().map(|i| items[i].tokens.clone());

    Ok(quote! {
        impl #impl_generics #trait_path for #name #ty_generics #where_clause {
            fn build(&self) -> ::std::string::String {
                let mut parts: ::std::vec::Vec<::std::string::String> =
                    ::std::vec![#cmd_init];

                #(#emitted)*

                parts.join(" ")
            }
        }
    })
}








































