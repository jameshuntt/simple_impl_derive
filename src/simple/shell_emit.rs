use super::*;

/// How many emission kinds a field's `#[shell]` names.
pub(super) fn shell_kind_count(s: &ShellFieldCfg) -> usize {
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

/// The shell impl. Three stages, each reporting everything it finds into
/// `errs`: the struct, every field, then the ordering across fields. Every
/// stage runs; a field that failed in the parse is left out and a reference
/// to it is not reported again. Returns empty tokens when anything was
/// reported.
pub(super) fn expand_shell_impl(
    input: &DeriveInput,
    parsed: &Parsed,
    fields: &[FieldInfo],
    errs: &mut SynBuffer,
) -> TokenStream2 {
    let shell_struct = &parsed.shell_struct;
    let failed = &parsed.shell_failed;
    // require_order: a field whose `order` was unreadable is already reported
    let order_missing = |f: &FieldInfo| {
        shell_struct.require_order && f.shell.order.is_none() && !parsed.order_failed.contains(&f.ident.to_string())
    };
    let name = &input.ident;
    let (impl_generics, ty_generics, where_clause) = input.generics.split_for_impl();

    let cmd_init: TokenStream2 = match (&shell_struct.cmd, &shell_struct.cmd_expr) {
        (Some(cmd), _) => {
            let cmd_lit = LitStr::new(cmd, input.span());
            quote! { #cmd_lit.to_string() }
        }
        (None, Some(expr)) => quote! { (#expr).to_string() },
        (None, None) => {
            // an unreadable #[shell(...)] is already reported; cmd may be unset only because of it
            if parsed.cmd_known {
                errs.push_at(input.span(), E::MissingStructCmd);
            }
            TokenStream2::new()
        }
    };

    let trait_path: Path = match &shell_struct.trait_path {
        Some(p) => p.clone(),
        None => syn::parse_quote!(::simple_impl::ShellCommand),
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


    /// init_required (init_only + required) on a non-bool, non-Option field
    /// emits as a positional with no `#[shell]` at all.
    fn implied_positional(f: &FieldInfo) -> bool {
        f.builder.init_only && f.builder.required && type_allows_implied(f)
    }

    /// Only a field that is neither bool nor Option<T> can be an implied
    /// positional, whatever its #[builder] says.
    fn type_allows_implied(f: &FieldInfo) -> bool {
        !type_is_bool(&f.ty) && type_option_inner(&f.ty).is_none()
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

    // A field joined onto one that already failed is skipped, not reported:
    // its partner's error is the one to fix.
    let joins_failed = |f: &FieldInfo| f.shell.arg_join_opt_with.as_ref().is_some_and(|w| failed.contains(w));

    // (optional) enforce only one subcommand field per struct
    let mut saw_subcommand = false;
    
    for f in fields {
        let k = shell_kind_count(&f.shell);

        // NEW: implied positional emission for init_required fields
        let implied_positional = implied_positional(f);

        
        // Field not used by shell emission:
        if k == 0 {
            // sep without join is still invalid
            ensure!(errs, f.shell.arg_join_sep.is_none(), &f.ident => E::SepWithoutJoin);

            // an unreadable #[builder] leaves "implied positional?" open: the
            // checks below would be guesses, so this field stops here
            if parsed.builder_unread.contains(&f.ident.to_string()) && type_allows_implied(f) {
                continue;
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
                // a present but unreadable order still sits on a field that emits nothing
                let has_order = f.shell.order.is_some() || parsed.order_failed.contains(&f.ident.to_string());
                ensure!(errs, !has_order, &f.ident => E::OrderOnNonEmitted);
                continue;
            }

            // Strict mode: require explicit order for inferred emitted fields too
            ensure!(errs, !order_missing(f), &f.ident => E::RequireOrderMissing);

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
            errs.push_spanned(&f.ident, E::FmtOnWrongKind);
        }
        if let Some(fmt) = &f.shell.fmt {
            ensure!(errs, fmt.matches("{}").count() == 1, &f.ident => E::FmtNeedsOnePlaceholder);
        }
        if f.shell.join.is_some()
            && f.shell.multi_arg_flag.is_none()
            && f.shell.multi_opt_kv.is_none()
            && !(f.shell.positional && f.shell.arg_expr.is_none() && f.shell.opt_expr.is_none() && f.shell.arg_join_opt_with.is_none() && type_vec_inner(&f.ty).is_some())
        {
            errs.push_spanned(&f.ident, E::JoinOnWrongKind);
        }

        // Hygiene: exactly one emission kind per field
        ensure!(errs, k <= 1, &f.ident => E::MultipleEmissionKinds);

        // Validation: sep only with arg_join_opt
        ensure!(errs, !(f.shell.arg_join_sep.is_some() && f.shell.arg_join_opt_with.is_none()), &f.ident => E::SepWithoutJoin);

        // Strict mode: require explicit order for all emitted fields
        ensure!(errs, !order_missing(f), &f.ident => E::RequireOrderMissing);

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
        let is_option = type_option_inner(&f.ty).is_some();
        ensure!(errs, f.shell.opt_kv.is_none() || is_option, &f.ident => E::OptKvRequiresOption);
        ensure!(errs, f.shell.opt_prefix.is_none() || is_option, &f.ident => E::OptPrefixRequiresOption);
        ensure!(errs, f.shell.opt_eq.is_none() || is_option, &f.ident => E::OptEqRequiresOption);
        // more than one kind (already reported): only one branch below would
        // run, so every kind written gets its type check here; the field is not
        // built, and its placeholder stands in for it in the ordering
        if k > 1 {
            let is_vec = type_vec_inner(&f.ty).is_some() || type_option_vec_inner(&f.ty).is_some();
            let is_flag = f.shell.flag.is_some() || f.shell.flag_off.is_some();
            ensure!(errs, !is_flag || type_is_bool(&f.ty), &f.ident => E::FlagRequiresBool);
            ensure!(errs, f.shell.count_flag.is_none() || type_is_uint(&f.ty), &f.ident => E::CountFlagRequiresUint);
            ensure!(errs, f.shell.multi_opt_kv.is_none() || is_vec, &f.ident => E::MultiOptKvRequiresVec);
            ensure!(errs, f.shell.multi_opt_prefix.is_none() || is_vec, &f.ident => E::MultiOptPrefixRequiresVec);
            ensure!(errs, f.shell.multi_arg_flag.is_none() || is_vec, &f.ident => E::MultiArgFlagRequiresVec);
            // the partner is looked up only where emitting would read it: not
            // when an expression or a joined Vec comes first
            let joins = f.shell.arg_expr.is_none() && f.shell.opt_expr.is_none() && !(f.shell.join.is_some() && type_vec_inner(&f.ty).is_some());
            if let (true, Some(with)) = (joins, &f.shell.arg_join_opt_with) {
                ensure!(errs, failed.contains(with) || by_name.contains_key(with), &f.ident => E::ArgJoinUnknownField);
            }
            if f.shell.subcommand {
                ensure!(errs, !saw_subcommand, &f.ident => E::MultipleSubcommands);
                saw_subcommand = true;
            }
            continue;
        }
        if f.shell.subcommand {
            if !ensure!(errs, !saw_subcommand, &f.ident => E::MultipleSubcommands) {
                continue;
            }
            saw_subcommand = true;

            if joins_failed(f) { continue; }
            let Some(tokens) = errs.absorb(emit_positional_tokens(f, &by_name)) else { continue };
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
            if !ensure!(errs, type_is_uint(&f.ty), &f.ident => E::CountFlagRequiresUint) {
                continue;
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
                    errs.push_spanned(&f.ident, E::MultiOptKvRequiresVec);
                    continue;
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
                errs.push_spanned(&f.ident, E::MultiOptKvRequiresVec);
                continue;
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
                errs.push_spanned(&f.ident, E::MultiOptPrefixRequiresVec);
                continue;
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
                    errs.push_spanned(&f.ident, E::MultiArgFlagRequiresVec);
                    continue;
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
                errs.push_spanned(&f.ident, E::MultiArgFlagRequiresVec);
                continue;
            };

            items.push(EmitItem { name: f.ident.to_string(), base, tie: f.order, span: f.ident.span(), rel, tokens });
            continue;
        }
        // Boolean on/off (toggle) + plain flag-only
        if f.shell.flag.is_some() || f.shell.flag_off.is_some() {
            if !ensure!(errs, type_is_bool(&f.ty), &f.ident => E::FlagRequiresBool) {
                continue;
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
            if joins_failed(f) { continue; }
            let Some(pos_tokens) = errs.absorb(emit_positional_tokens(f, &by_name)) else { continue };
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
        errs.push_spanned(&f.ident, E::Unmatched);
    }

    // 3. The "Explosion" point used to be here. Ordering now runs on every
    // field. One that should have emitted but failed above gets an empty
    // placeholder item, so its before/after edges still count (a cycle
    // through it is reported now); nothing is generated once anything was
    // reported. A field that failed in the parse is not here at all, so an
    // order target naming it is skipped rather than reported unknown.
    let built: HashSet<String> = items.iter().map(|it| it.name.clone()).collect();
    let mut failed_targets: HashSet<String> = failed.iter().map(|n| n.to_ascii_lowercase()).collect();
    for f in fields {
        let name = f.ident.to_string();
        if shell_kind_count(&f.shell) == 0 && parsed.builder_unread.contains(&name) && type_allows_implied(f) {
            // whether it emits is unknown until its #[builder] reads
            failed_targets.insert(name.to_ascii_lowercase());
        } else if (shell_kind_count(&f.shell) > 0 || implied_positional(f)) && !built.contains(&name) {
            let (base, rel) = base_order(f, ORD_POS);
            items.push(EmitItem { name, base, tie: f.order, tokens: TokenStream2::new(), span: f.ident.span(), rel });
        }
    }

    // -------------------------
    // Build constraint graph (before/after) over emitted items
    // -------------------------
    let n = items.len();

    let mut idx_exact: HashMap<String, usize> = HashMap::with_capacity(n);
    let mut idx_lower: HashMap<String, usize> = HashMap::with_capacity(n);

    // a lowercase name two fields share (reported below) names neither of them
    let mut ambiguous: HashSet<String> = HashSet::new();
    for (i, it) in items.iter().enumerate() {
        idx_exact.insert(it.name.clone(), i);

        let lower = it.name.to_ascii_lowercase();
        if let Some(prev) = idx_lower.insert(lower.clone(), i) {
            ambiguous.insert(lower);
            // If two different fields collide case-insensitively, error out.
            if prev != i {
                errs.push_at_ctx(it.span, E::OrderCaseCollision, format!("`{}` and `{}`", items[prev].name, it.name));
            }
        }
    }

    // Helper: resolve a target name case-insensitively; a name that matches a
    // field which already failed is that field, not a case-alike of it
    let resolve_target = |target: &str| -> Option<usize> {
        let lower = target.to_ascii_lowercase();
        let unsure = failed_targets.contains(&lower) || ambiguous.contains(&lower);
        idx_exact.get(target).copied().or_else(|| if unsure { None } else { idx_lower.get(&lower).copied() })
    };

    let mut adj: Vec<Vec<usize>> = vec![Vec::new(); n];
    let mut indeg: Vec<usize> = vec![0; n];

    for i in 0..n {
        let Some(rel) = items[i].rel.clone() else { continue };
        match &rel {
            ShellOrder::Before(target) | ShellOrder::After(target) => {
                let Some(j) = resolve_target(target) else {
                    let lower = target.to_ascii_lowercase();
                    if !failed_targets.contains(&lower) && !ambiguous.contains(&lower) {
                        // on the name when it was written as order(before = name)
                        let at = parsed.order_target_spans.get(&items[i].name).copied().unwrap_or(items[i].span);
                        errs.push_at_ctx(at, E::OrderTargetUnknown, format!("`{target}`"));
                    }
                    continue;
                };
                if matches!(rel, ShellOrder::Before(_)) {
                    adj[i].push(j);
                    indeg[j] += 1;
                } else {
                    adj[j].push(i);
                    indeg[i] += 1;
                }
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
        // every circle of before/after is its own problem: one error each, at
        // its first field
        let stuck: Vec<bool> = indeg.iter().map(|&d| d > 0).collect();
        for cycle in order_cycles(&adj, &stuck) {
            // placeholders sit after the built items: pick by field order
            if let Some(&first) = cycle.iter().min_by_key(|&&i| items[i].tie) {
                errs.push_at(items[first].span, E::OrderCycle);
            }
        }
    }
    if !errs.is_empty() {
        return TokenStream2::new();
    }

    let emitted = order.into_iter().map(|i| items[i].tokens.clone());

    quote! {
        impl #impl_generics #trait_path for #name #ty_generics #where_clause {
            fn build(&self) -> ::std::string::String {
                let mut parts: ::std::vec::Vec<::std::string::String> =
                    ::std::vec![#cmd_init];

                #(#emitted)*

                parts.join(" ")
            }
        }
    }
}

/// The circles among the nodes the sort could not place: each strongly
/// connected set of two or more, or one node ordered against itself (Tarjan).
fn order_cycles(adj: &[Vec<usize>], stuck: &[bool]) -> Vec<Vec<usize>> {
    struct Walk<'a> {
        adj: &'a [Vec<usize>],
        stuck: &'a [bool],
        index: Vec<Option<usize>>,
        low: Vec<usize>,
        on_stack: Vec<bool>,
        stack: Vec<usize>,
        next: usize,
        cycles: Vec<Vec<usize>>,
    }
    fn visit(w: &mut Walk, v: usize) {
        w.index[v] = Some(w.next);
        w.low[v] = w.next;
        w.next += 1;
        w.stack.push(v);
        w.on_stack[v] = true;
        for &u in &w.adj[v] {
            if !w.stuck[u] {
                continue;
            }
            match w.index[u] {
                None => {
                    visit(w, u);
                    w.low[v] = w.low[v].min(w.low[u]);
                }
                Some(iu) if w.on_stack[u] => w.low[v] = w.low[v].min(iu),
                Some(_) => {}
            }
        }
        if Some(w.low[v]) == w.index[v] {
            let mut set = Vec::new();
            while let Some(u) = w.stack.pop() {
                w.on_stack[u] = false;
                set.push(u);
                if u == v {
                    break;
                }
            }
            if set.len() > 1 || w.adj[v].contains(&v) {
                set.sort_unstable();
                w.cycles.push(set);
            }
        }
    }
    let n = adj.len();
    let mut w = Walk { adj, stuck, index: vec![None; n], low: vec![0; n], on_stack: vec![false; n], stack: Vec::new(), next: 0, cycles: Vec::new() };
    for v in 0..n {
        if stuck[v] && w.index[v].is_none() {
            visit(&mut w, v);
        }
    }
    w.cycles.sort();
    w.cycles
}
