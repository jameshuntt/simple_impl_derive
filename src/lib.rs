// --------------------
// imports
// --------------------
use std::{cmp::Reverse, collections::{BinaryHeap, HashMap}};
use proc_macro::TokenStream;
use proc_macro2::{Span, TokenStream as TokenStream2};
use quote::{quote};
use syn::{
    parse_macro_input,
    spanned::Spanned,
    Attribute,
    Data,
    DeriveInput,
    Error,
    Fields,
    Ident,
    LitStr,
    Path,
    Type,
};






// --------------------
// high-level wrapper to export
// --------------------

#[proc_macro_derive(SimpleBuilder, attributes(builder))]
pub fn derive_simple_builder(input: TokenStream) -> TokenStream {
    let input = parse_macro_input!(input as DeriveInput);
    match expand(&input, ExpandMode::BuilderOnly) {
        Ok(ts) => ts.into(),
        Err(e) => e.to_compile_error().into(),
    }
}

#[proc_macro_derive(SimpleShell, attributes(shell))]
pub fn derive_simple_shell(input: TokenStream) -> TokenStream {
    let input = parse_macro_input!(input as DeriveInput);
    match expand(&input, ExpandMode::ShellOnly) {
        Ok(ts) => ts.into(),
        Err(e) => e.to_compile_error().into(),
    }
}

#[proc_macro_derive(SimpleImpl, attributes(builder, shell))]
pub fn derive_simple_impl(input: TokenStream) -> TokenStream {
    let input = parse_macro_input!(input as DeriveInput);
    match expand(&input, ExpandMode::Both) {
        Ok(ts) => ts.into(),
        Err(e) => e.to_compile_error().into(),
    }
}



// --------------------
// macros
// --------------------

macro_rules! match_shell_attr {
    ($meta:ident, $cfg:ident, {
        $( 
            $path:literal => {
                $( str => $field_str:ident )?
                $( bool => $field_bool:ident $( [ $($extra:stmt);* ] )? )?
            } 
        ),* $(,)?
    }) => {
        $(
            if $meta.path.is_ident($path) {
                $(
                    let lit: ::syn::LitStr = $meta.value()?.parse()?;
                    $cfg.$field_str = Some(lit.value());
                )?
                $(
                    $cfg.$field_bool = true;
                    $($($extra)*)?
                )?
                return Ok(());
            }
        )*
    };
}

macro_rules! match_builder_idents {
    ($meta:ident, { 
        $($ident:literal => $action:expr),* $(,)? 
    }) => {
        $(
            if $meta.path.is_ident($ident) {
                $action;
                return Ok(());
            }
        )*
    };
}

#[allow(unused)]
macro_rules! gen_builder_setter {
    ($arg:ident: $arg_ty:ty, $body:expr) => {
        quote! {
            #[inline]
            pub fn #method(mut self, $arg: $arg_ty) -> Self {
                $body;
                self
            }
        }
    };
    // Special case for zero-argument setters (like Flags)
    ($body:expr) => {
        quote! {
            #[inline]
            pub fn #method(mut self) -> Self {
                $body;
                self
            }
        }
    };
}








// --------------------
// expansion functions
// --------------------

fn expand(input: &DeriveInput, mode: ExpandMode) -> Result<TokenStream2, Error> {
    let (shell_struct, fields) = parse_struct_and_fields(input)?;

    let mut out = TokenStream2::new();

    match mode {
        ExpandMode::BuilderOnly => {
            out.extend(expand_builder_impl(input, &fields)?);
        }
        ExpandMode::ShellOnly => {
            out.extend(expand_shell_impl(input, &shell_struct, &fields)?);
        }
        ExpandMode::Both => {
            out.extend(expand_builder_impl(input, &fields)?);
            out.extend(expand_shell_impl(input, &shell_struct, &fields)?);
        }
    }

    let out_string = out.to_string();
    if let Ok(file) = syn::parse_file(&out_string) {
        eprintln!("GENERATED CODE for {}:\n{}", input.ident, prettyplease::unparse(&file));
    } else {
        eprintln!("GENERATED CODE (unformatted) for {}:\n{}", input.ident, out_string);
    }

    Ok(out)
}





fn expand_builder_impl(input: &DeriveInput, fields: &[FieldInfo]) -> Result<TokenStream2, Error> {
    let name = &input.ident;
    let (impl_generics, ty_generics, where_clause) = input.generics.split_for_impl();

    // Required = builder.required (includes not_in_default)
    let required: Vec<&FieldInfo> = fields.iter().filter(|f| f.builder.required).collect();

    // new(...) generation
    let new_fn = if required.is_empty() {
        // default-style new() if you want it: you can always mark one field required to force args.
        quote! {
            #[inline]
            pub fn new() -> Self {
                ::core::default::Default::default()
            }
        }
    } else {
        let args = required.iter().map(|f| {
            let id = &f.ident;
            let ty = &f.ty;
            if f.builder.into || type_is_string(ty) {
                quote! { #id: impl ::core::convert::Into<#ty> }
            } else {
                quote! { #id: #ty }
            }
        });

        let inits = fields.iter().map(|f| {
            let id = &f.ident;
            if f.builder.required {
                let ty = &f.ty;
                if f.builder.into || type_is_string(ty) {
                    quote! { #id: #id.into() }
                } else {
                    quote! { #id: #id }
                }
            } else if let Some(expr) = &f.builder.default_expr {
                quote! { #id: { #expr } }
            } else {
                quote! { #id: ::core::default::Default::default() }
            }
        });

        quote! {
            #[inline]
            pub fn new(#(#args),*) -> Self {
                Self { #(#inits),* }
            }
        }
    };

    // setters
    let setters = fields.iter().filter_map(|f| {
        if f.builder.skip_setter {
            return None;
        }
        let method = &f.ident;
        let field = &f.ident;
        let ty = &f.ty;

        let kind = f.builder.kind?;
        // let gen_setter = |arg_tokens: proc_macro2::TokenStream, body: proc_macro2::TokenStream| {
        //     quote! {
        //         #[inline]
        //         pub fn #method(mut self, #arg_tokens) -> Self {
        //             #body;
        //             self
        //         }
        //     }
        // };
        Some(match kind {
            BuilderKind::Flag => {
                quote! {
                    #[inline]
                    pub fn #method(mut self) -> Self {
                        self.#field = true;
                        self
                    }
                }
            }
            BuilderKind::Opt => {
                let inner = type_option_inner(ty).expect("Opt kind requires Option<T>");
                quote! {
                    #[inline]
                    pub fn #method(mut self, value: #inner) -> Self {
                        self.#field = ::core::option::Option::Some(value);
                        self
                    }
                }
            }
            BuilderKind::OptInto => {
                let inner = type_option_inner(ty).expect("OptInto kind requires Option<T>");
                quote! {
                    #[inline]
                    pub fn #method(mut self, value: impl ::core::convert::Into<#inner>) -> Self {
                        self.#field = ::core::option::Option::Some(value.into());
                        self
                    }
                }
            }
            BuilderKind::Set => {
                quote! {
                    #[inline]
                    pub fn #method(mut self, value: #ty) -> Self {
                        self.#field = value;
                        self
                    }
                }
            }
            BuilderKind::SetInto => {
                quote! {
                    #[inline]
                    pub fn #method(mut self, value: impl ::core::convert::Into<#ty>) -> Self {
                        self.#field = value.into();
                        self
                    }
                }
            }
            BuilderKind::VecInto => {
                let inner = type_vec_inner(ty).expect("VecInto kind requires Vec<T>");
                quote! {
                    #[inline]
                    pub fn #method(mut self, list: impl ::core::convert::Into<::std::vec::Vec<#inner>>) -> Self {
                        self.#field = list.into();
                        self
                    }
                }
            }
            BuilderKind::VecIter => {
                let inner = type_vec_inner(ty).expect("VecIter kind requires Vec<T>");
                quote! {
                    #[inline]
                    pub fn #method<I>(mut self, iter: I) -> Self
                    where
                        I: ::core::iter::IntoIterator<Item = #inner>,
                    {
                        self.#field = iter.into_iter().collect();
                        self
                    }
                }
            }
            BuilderKind::Push => {
                // keep it explicit later; for now: emit a compile error so you don’t silently get the wrong API.
                let msg = format!(
                    "builder(push) for field `{}` requires a dedicated naming scheme; implement this next (e.g. #[builder(push_name=\"push_{}\")] )",
                    field, field
                );
                quote! { ::core::compile_error!(#msg); }
            }
        })
    });

    Ok(quote! {
        impl #impl_generics #name #ty_generics #where_clause {
            #new_fn
            #(#setters)*
        }
    })
}



fn expand_shell_impl(
    input: &DeriveInput,
    shell_struct: &ShellCfg,
    fields: &[FieldInfo],
) -> Result<TokenStream2, Error> {
    let name = &input.ident;
    let (impl_generics, ty_generics, where_clause) = input.generics.split_for_impl();

    let cmd = shell_struct
        .cmd
        .as_ref()
        .ok_or_else(|| Error::new(input.span(), "missing #[shell(cmd = \"...\")] on struct"))?;
    let cmd_lit = LitStr::new(cmd, input.span());

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

    fn shell_kind_count(s: &ShellFieldCfg) -> usize {
        let mut n = 0;
        if s.flag.is_some() {
            n += 1;
        }
        if s.opt_kv.is_some() {
            n += 1;
        }
        if s.opt_prefix.is_some() {
            n += 1;
        }
        if s.opt_eq.is_some() {
            n += 1;
        }
        if s.positional {
            n += 1;
        }
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

    fn emit_positional_tokens(
        f: &FieldInfo,
        by_name: &HashMap<String, (&Ident, &Type)>,
    ) -> Result<TokenStream2, Error> {
        let field = &f.ident;
        let ty = &f.ty;

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
                Error::new(
                    f.ident.span(),
                    format!("arg_join_opt references unknown field `{}`", with_name),
                )
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
                        parts.push(v.clone());
                    }
                },
                PosMode::Display => quote! {
                    if let ::core::option::Option::Some(v) = &self.#field {
                        parts.push(v.to_string());
                    }
                },
            })
        } else {
            Ok(match mode {
                PosMode::Clone => quote! { parts.push(self.#field.clone()); },
                PosMode::Display => quote! { parts.push(self.#field.to_string()); },
            })
        }
    }

    let mut items: Vec<EmitItem> = Vec::new();

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
                return Err(Error::new(
                    f.ident.span(),
                    "#[shell(sep = \"...\")] is only valid with #[shell(arg_join_opt = \"other_field\")]",
                ));
            }

            // If the user set order but provided no emission kind, allow it ONLY if
            // we will infer positional emission (init_required).
            if !implied_positional {
                if f.shell.order.is_some() {
                    return Err(Error::new(
                        f.ident.span(),
                        "#[shell(order=...)] provided but this field is not emitted by shell (no flag/opt/arg)",
                    ));
                }
                continue;
            }

            // Strict mode: require explicit order for inferred emitted fields too
            if shell_struct.require_order && f.shell.order.is_none() {
                return Err(Error::new(
                    f.ident.span(),
                    "missing #[shell(order = N)] (struct has #[shell(require_order)])",
                ));
            }

            // Emit unconditional positional push:
            // String => clone push, others => to_string push
            let field = &f.ident;
            let tokens = if type_is_string(&f.ty) {
                quote! { parts.push(self.#field.clone()); }
            } else {
                quote! { parts.push(self.#field.to_string()); }
            };

            let (base, rel) = base_order(f, ORD_POS_TAIL);

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

        // Hygiene: exactly one emission kind per field
        if k > 1 {
            return Err(Error::new(
                f.ident.span(),
                "field has multiple #[shell(...)] emission kinds; choose only one of: flag/opt_kv/opt_prefix/opt_eq/arg",
            ));
        }

        // Validation: sep only with arg_join_opt
        if f.shell.arg_join_sep.is_some() && f.shell.arg_join_opt_with.is_none() {
            return Err(Error::new(
                f.ident.span(),
                "#[shell(sep = \"...\")] is only valid with #[shell(arg_join_opt = \"other_field\")]",
            ));
        }

        // Strict mode: require explicit order for all emitted fields
        if shell_struct.require_order && f.shell.order.is_none() {
            return Err(Error::new(
                f.ident.span(),
                "missing #[shell(order = ...)] (struct has #[shell(require_order)])",
            ));
        }

        // Build the ONE token block per field, with base order + possible before/after constraint.
        if let Some(flag) = &f.shell.flag {
            let field = &f.ident;
            let flag_lit = LitStr::new(flag, f.ident.span());
            let (base, rel) = base_order(f, ORD_FLAG);

            items.push(EmitItem {
                name: f.ident.to_string(),
                base,
                tie: f.order,
                span: f.ident.span(),
                rel,
                tokens: quote! {
                    if self.#field {
                        parts.push(#flag_lit.to_string());
                    }
                },
            });
            continue;
        }

        if let Some(flag) = &f.shell.opt_kv {
            let field = &f.ident;
            let flag_lit = LitStr::new(flag, f.ident.span());
            let (base, rel) = base_order(f, ORD_OPT_KV);

            items.push(EmitItem {
                name: f.ident.to_string(),
                base,
                tie: f.order,
                span: f.ident.span(),
                rel,
                tokens: quote! {
                    if let ::core::option::Option::Some(v) = &self.#field {
                        parts.push(#flag_lit.to_string());
                        parts.push(v.to_string());
                    }
                },
            });
            continue;
        }

        if let Some(prefix) = &f.shell.opt_prefix {
            let field = &f.ident;
            let prefix_lit = LitStr::new(prefix, f.ident.span());
            let (base, rel) = base_order(f, ORD_OPT_PREFIX);

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

        if let Some(flag) = &f.shell.opt_eq {
            let field = &f.ident;
            let flag_lit = LitStr::new(flag, f.ident.span());
            let (base, rel) = base_order(f, ORD_OPT_EQ);

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

        if f.shell.positional {
            let pos_tokens = emit_positional_tokens(f, &by_name)?;
            let (base, rel) = base_order(f, ORD_POS);

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
        return Err(Error::new(
            f.ident.span(),
            "internal error: field marked emitted but no emission kind matched",
        ));
    }

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
            // ShellOrder::Before(target) => {
            //     let j = *idx.get(&target).ok_or_else(|| {
            //         Error::new(
            //             items[i].span,
            //             format!("order=\"before:{}\" references unknown (or non-emitted) field", target),
            //         )
            //     })?;
            //     adj[i].push(j);
            //     indeg[j] += 1;
            // }
            // ShellOrder::After(target) => {
            //     let j = *idx.get(&target).ok_or_else(|| {
            //         Error::new(
            //             items[i].span,
            //             format!("order=\"after:{}\" references unknown (or non-emitted) field", target),
            //         )
            //     })?;
            //     adj[j].push(i);
            //     indeg[i] += 1;
            // }
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
        return Err(Error::new(
            items[stuck].span,
            "ordering cycle detected in #[shell(order=...)] constraints (before/after)",
        ));
    }

    let emitted = order.into_iter().map(|i| items[i].tokens.clone());

    Ok(quote! {
        impl #impl_generics #trait_path for #name #ty_generics #where_clause {
            fn build(&self) -> ::std::string::String {
                let mut parts: ::std::vec::Vec<::std::string::String> =
                    ::std::vec![#cmd_lit.to_string()];

                #(#emitted)*

                parts.join(" ")
            }
        }
    })
}








/// step 1
/// 
/// we must deconstruct the struct into workable units
fn parse_struct_and_fields(input: &DeriveInput) -> Result<(ShellCfg, Vec<FieldInfo>), Error> {
    let data = match &input.data {
        Data::Struct(s) => s,
        _ => return Err(Error::new(input.span(), "SimpleImpl only supports structs")),
    };

    // create the shell config
    let mut shell_struct = ShellCfg::default();
    // mutate the
    parse_shell_struct_attrs(&input.attrs, &mut shell_struct)?;

    let fields_named = match &data.fields {
        Fields::Named(n) => n,
        _ => return Err(Error::new(input.span(), "SimpleImpl requires a named-field struct")),
    };

    let mut fields = Vec::new();

    for (i, f) in fields_named.named.iter().enumerate() {
        let ident = f
            .ident
            .clone()
            .ok_or_else(|| Error::new(f.span(), "expected named field"))?;

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
            return Err(Error::new(
                f.span(),
                "#[builder(init_required)] (or #[builder(init_only, required)]) cannot be combined with #[builder(default_expr = \"...\")]",
            ));
        }

        if bcfg.init_only && bcfg.kind.is_some() {
            return Err(Error::new(
                f.span(),
                "#[builder(init_only)] cannot be combined with builder kind overrides (flag/opt/set/etc.)",
            ));
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

fn parse_shell_struct_attrs(attrs: &[Attribute], cfg: &mut ShellCfg) -> Result<(), Error> {
    for attr in attrs {
        if !attr.path().is_ident("shell") {
            continue;
        }
        attr.parse_nested_meta(|meta| {
            if meta.path.is_ident("cmd") {
                let lit: LitStr = meta.value()?.parse()?;
                cfg.cmd = Some(lit.value());
                return Ok(());
            }
            if meta.path.is_ident("trait_path") {
                let lit: LitStr = meta.value()?.parse()?;
                let p: Path = syn::parse_str(&lit.value())?;
                cfg.trait_path = Some(p);
                return Ok(());
            }

            // require_order
            if meta.path.is_ident("require_order") {
                cfg.require_order = true;
                return Ok(());
            }

            Err(meta.error("unknown #[shell(...)] item on struct"))
        })?;
    }
    Ok(())
}

fn parse_shell_field_attrs(attrs: &[Attribute], cfg: &mut ShellFieldCfg) -> Result<(), Error> {
    for attr in attrs {
        if !attr.path().is_ident("shell") {
            continue;
        }
        attr.parse_nested_meta(|meta| {
            match_shell_attr!(meta, cfg, {
                // String assignments
                "flag"         => { str => flag },
                "opt_kv"       => { str => opt_kv },
                "opt_prefix"   => { str => opt_prefix },
                "opt_eq"       => { str => opt_eq },
                // "arg_join_opt" => { str => arg_join_opt_with },
                "sep"          => { str => arg_join_sep },

                // Boolean / Flags
                "positional"   => { bool => positional },
                "arg"          => { bool => positional },
                "arg_clone"    => { bool => positional [ cfg.positional_mode = Some(PosMode::Clone) ] },
                "arg_display"  => { bool => positional [ cfg.positional_mode = Some(PosMode::Display) ] },
            });

            // order = N OR order = "first"/"last"/"before:FIELD"/"after:FIELD"
            if meta.path.is_ident("order") {
                let lit: syn::Lit = meta.value()?.parse()?;

                match lit {
                    syn::Lit::Int(n) => {
                        let v = n
                            .base10_parse::<i32>()
                            .map_err(|e| Error::new(n.span(), format!("order must be an integer: {e}")))?;
                        cfg.order = Some(ShellOrder::Key(v));
                        return Ok(());
                    }
                    syn::Lit::Str(s) => {
                        let raw = s.value();
                        let raw_trim = raw.trim();
                        let norm = raw_trim.to_ascii_lowercase();

                        match norm.as_str() {
                            "first" => {
                                cfg.order = Some(ShellOrder::First);
                                return Ok(());
                            }
                            "last" => {
                                cfg.order = Some(ShellOrder::Last);
                                return Ok(());
                            }
                            _ => {}
                        }

                        // IMPORTANT: detect before/after using lowercase,
                        // but extract the target from *raw_trim* to preserve case.
                        if norm.starts_with("before:") {
                        let target = raw_trim
                            .splitn(2, ':')
                            .nth(1)
                            .unwrap_or("")
                            .trim();

                        if target.is_empty() {
                            return Err(Error::new(s.span(), "order=\"before:FIELD\" requires a field name"));
                        }

                        cfg.order = Some(ShellOrder::Before(target.to_string()));
                            return Ok(());
                        }

                        if norm.starts_with("after:") {
                            let target = raw_trim
                                .splitn(2, ':')
                                .nth(1)
                                .unwrap_or("")
                                .trim();

                            if target.is_empty() {
                                return Err(Error::new(s.span(), "order=\"after:FIELD\" requires a field name"));
                            }

                            cfg.order = Some(ShellOrder::After(target.to_string()));
                            return Ok(());
                        }

                        return Err(Error::new(
                            s.span(),
                            "order must be an integer, or one of: \"first\", \"last\", \"before:FIELD\", \"after:FIELD\"",
                        ));
                    }
                    other => {
                        return Err(Error::new(
                            other.span(),
                            "order must be an integer or a string (\"first\"|\"last\"|\"before:FIELD\"|\"after:FIELD\")",
                        ));
                    }
                }
            }


            // Special parsing (arg_expr remains manual as it involves TokenStream conversion)
            if meta.path.is_ident("arg_expr") {
                let lit: LitStr = meta.value()?.parse()?;
                let ts: TokenStream2 = syn::parse_str(&lit.value())
                    .map_err(|e| Error::new(lit.span(), format!("arg_expr parse error: {e}")))?;
                cfg.positional = true;
                cfg.arg_expr = Some(ts);
                return Ok(());
            }

            // NEW: arg_join_opt (with) + sep
            if meta.path.is_ident("arg_join_opt") {
                let lit: LitStr = meta.value()?.parse()?;
                cfg.positional = true;
                cfg.arg_join_opt_with = Some(lit.value());
                return Ok(());
            }

            Err(meta.error("unknown #[shell(...)] item on field"))
        })?;
    }
    Ok(())
}

fn parse_builder_field_attrs(attrs: &[Attribute], cfg: &mut BuilderCfg) -> Result<(), Error> {
    for attr in attrs {
        if !attr.path().is_ident("builder") {
            continue;
        }
        attr.parse_nested_meta(|meta| {
            match_builder_idents!(meta, {
                // Boolean flags
                "required"       => cfg.required = true,
                "not_in_default" => cfg.required = true,
                "into"           => cfg.into = true,
                "skip"           => cfg.skip_setter = true,

                // init_only => no builder setter; still initialized by default_expr or Default::default()
                "init_only"      => { cfg.init_only = true; cfg.skip_setter = true; },

                // init_required => ctor-only field: must be in new(...), and no setter
                "init_required"  => { cfg.init_only = true; cfg.skip_setter = true; cfg.required = true; },

                // Kind overrides
                "flag"           => cfg.kind = Some(BuilderKind::Flag),
                "opt"            => cfg.kind = Some(BuilderKind::Opt),
                "opt_into"       => cfg.kind = Some(BuilderKind::OptInto),
                "set"            => cfg.kind = Some(BuilderKind::Set),
                "set_into"       => cfg.kind = Some(BuilderKind::SetInto),
                "vec_into"       => cfg.kind = Some(BuilderKind::VecInto),
                "vec_iter"       => cfg.kind = Some(BuilderKind::VecIter),
                "push"           => cfg.kind = Some(BuilderKind::Push),
            });

            if meta.path.is_ident("default_expr") {
                let lit: LitStr = meta.value()?.parse()?;
                let ts: TokenStream2 = syn::parse_str(&lit.value())
                    .map_err(|e| Error::new(lit.span(), format!("default_expr parse error: {e}")))?;
                cfg.default_expr = Some(ts);
                return Ok(());
            }

            Err(meta.error("unknown #[builder(...)] item on field"))
        })?;
    }
    Ok(())
}

fn infer_builder_kind(ty: &Type, cfg: &BuilderCfg) -> BuilderKind {
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

fn infer_pos_mode(ty: &Type) -> PosMode {
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
















// --------------------
// types
// --------------------

#[derive(Copy, Clone)]
enum ExpandMode {
    BuilderOnly,
    ShellOnly,
    Both,
}

#[derive(Default, Debug, Clone)]
struct BuilderCfg {
    required: bool, // `required` OR `not_in_default`
    into: bool,     // for required args / setters: take `impl Into<T>`
    skip_setter: bool,

    init_only: bool,

    // Explicit “kind” overrides (otherwise inferred)
    kind: Option<BuilderKind>,

    // Override init expression in new()
    default_expr: Option<TokenStream2>,
}

#[derive(Debug, Clone, Copy)]
enum BuilderKind {
    Flag,     // bool -> fn field(mut self)->Self { self.field=true; self }
    Opt,      // Option<T> -> fn field(mut self, v:T)->Self { self.field=Some(v); self }
    OptInto,  // Option<T> -> fn field(mut self, v: impl Into<T>)->Self { self.field=Some(v.into()); self }
    Set,      // T -> fn field(mut self, v:T)->Self { self.field=v; self }
    SetInto,  // T -> fn field(mut self, v: impl Into<T>)->Self { self.field=v.into(); self }
    VecInto,  // Vec<T> from impl Into<Vec<T>>
    VecIter,  // Vec<T> from IntoIterator<Item=T>
    Push,     // Vec<T> push single elem (needs explicit method name later; kept for future)
}

#[derive(Default, Debug, Clone)]
struct ShellCfg {
    cmd: Option<String>,
    trait_path: Option<Path>,

    require_order: bool,
}

#[derive(Debug, Clone, Copy)]
enum PosMode {
    Clone,
    Display,
}

#[derive(Debug, Clone)]
struct FieldInfo {
    ident: Ident,
    ty: Type,
    builder: BuilderCfg,
    shell: ShellFieldCfg,
    #[allow(unused)]
    order: usize,
}

#[derive(Debug, Clone)]
enum ShellOrder {
    Key(i32),
    First,
    Last,
    Before(String),
    After(String),
}

#[derive(Default, Debug, Clone)]
struct ShellFieldCfg {
    // existing
    flag: Option<String>,
    opt_kv: Option<String>,
    positional: bool,
    positional_mode: Option<PosMode>,

    // NEW:
    // Option<T> => single arg: "{prefix}{value}"  e.g. "-s:/tmp/x"
    opt_prefix: Option<String>,

    // Option<T> => single arg: "{flag}={value}"  e.g. "--user=bob"
    opt_eq: Option<String>,

    // Positional => push (expr).to_string()
    arg_expr: Option<TokenStream2>,

    // Positional composite: base field joined with another field, optionally present
    // e.g. host + ":" + port if Some(port)
    arg_join_opt_with: Option<String>,
    arg_join_sep: Option<String>,

    // ordering
    order: Option<ShellOrder>,
}



















// --------------------
// type helpers
// --------------------


fn type_is_bool(ty: &Type) -> bool {
    matches!(ty, Type::Path(p) if p.path.segments.last().is_some_and(|s| s.ident == "bool"))
}

fn type_is_string(ty: &Type) -> bool {
    matches!(ty, Type::Path(p) if p.path.segments.last().is_some_and(|s| s.ident == "String"))
}

fn type_option_inner(ty: &Type) -> Option<&Type> {
    let Type::Path(p) = ty else { return None; };
    let seg = p.path.segments.last()?;
    if seg.ident != "Option" {
        return None;
    }
    let syn::PathArguments::AngleBracketed(ab) = &seg.arguments else { return None; };
    let first = ab.args.first()?;
    match first {
        syn::GenericArgument::Type(t) => Some(t),
        _ => None,
    }
}

fn type_vec_inner(ty: &Type) -> Option<&Type> {
    let Type::Path(p) = ty else { return None; };
    let seg = p.path.segments.last()?;
    if seg.ident != "Vec" {
        return None;
    }
    let syn::PathArguments::AngleBracketed(ab) = &seg.arguments else { return None; };
    let first = ab.args.first()?;
    match first {
        syn::GenericArgument::Type(t) => Some(t),
        _ => None,
    }
}
