use proc_macro::TokenStream;
use proc_macro2::TokenStream as TokenStream2;
use quote::{quote};
use syn::{
    parse_macro_input, spanned::Spanned, Attribute, Data, DeriveInput, Error, Fields, Ident, LitStr,
    Path, Type,
};


mod type_helpers;
use type_helpers::{type_is_bool,type_is_string, type_option_inner, type_vec_inner};


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

#[derive(Default, Debug, Clone)]
struct ShellFieldCfg {
    flag: Option<String>,
    opt_kv: Option<String>,
    positional: bool,
    positional_mode: Option<PosMode>,
}

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

/// step 1
/// 
/// we must deconstruct the struct into workable units
fn parse_struct_and_fields(input: &DeriveInput) -> Result<(ShellCfg, Vec<FieldInfo>), Error> {
    let data = match &input.data {
        Data::Struct(s) => s,
        _ => return Err(Error::new(input.span(), "SimpleImpl only supports structs")),
    };

    let mut shell_struct = ShellCfg::default();
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
            if meta.path.is_ident("flag") {
                let lit: LitStr = meta.value()?.parse()?;
                cfg.flag = Some(lit.value());
                return Ok(());
            }
            if meta.path.is_ident("opt_kv") {
                let lit: LitStr = meta.value()?.parse()?;
                cfg.opt_kv = Some(lit.value());
                return Ok(());
            }
            if meta.path.is_ident("positional") || meta.path.is_ident("arg") {
                cfg.positional = true;
                return Ok(());
            }
            if meta.path.is_ident("arg_clone") {
                cfg.positional = true;
                cfg.positional_mode = Some(PosMode::Clone);
                return Ok(());
            }
            if meta.path.is_ident("arg_display") {
                cfg.positional = true;
                cfg.positional_mode = Some(PosMode::Display);
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
            if meta.path.is_ident("required") || meta.path.is_ident("not_in_default") {
                cfg.required = true;
                return Ok(());
            }
            if meta.path.is_ident("into") {
                cfg.into = true;
                return Ok(());
            }
            if meta.path.is_ident("skip") {
                cfg.skip_setter = true;
                return Ok(());
            }

            // Kind overrides:
            if meta.path.is_ident("flag") {
                cfg.kind = Some(BuilderKind::Flag);
                return Ok(());
            }
            if meta.path.is_ident("opt") {
                cfg.kind = Some(BuilderKind::Opt);
                return Ok(());
            }
            if meta.path.is_ident("opt_into") {
                cfg.kind = Some(BuilderKind::OptInto);
                return Ok(());
            }
            if meta.path.is_ident("set") {
                cfg.kind = Some(BuilderKind::Set);
                return Ok(());
            }
            if meta.path.is_ident("set_into") {
                cfg.kind = Some(BuilderKind::SetInto);
                return Ok(());
            }
            if meta.path.is_ident("vec_into") {
                cfg.kind = Some(BuilderKind::VecInto);
                return Ok(());
            }
            if meta.path.is_ident("vec_iter") {
                cfg.kind = Some(BuilderKind::VecIter);
                return Ok(());
            }
            if meta.path.is_ident("push") {
                cfg.kind = Some(BuilderKind::Push);
                return Ok(());
            }

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

    // emit arms in stable order: flags, opt_kv, positionals (field order preserved)
    let flag_arms = fields.iter().filter_map(|f| {
        let flag = f.shell.flag.as_ref()?;
        let field = &f.ident;
        let flag_lit = LitStr::new(flag, f.ident.span());
        Some(quote! {
            if self.#field {
                parts.push(#flag_lit.to_string());
            }
        })
    });

    let opt_kv_arms = fields.iter().filter_map(|f| {
        let flag = f.shell.opt_kv.as_ref()?;
        let field = &f.ident;
        let flag_lit = LitStr::new(flag, f.ident.span());
        Some(quote! {
            if let ::core::option::Option::Some(v) = &self.#field {
                parts.push(#flag_lit.to_string());
                parts.push(v.to_string());
            }
        })
    });

    let pos_arms = fields.iter().filter(|f| f.shell.positional).map(|f| {
        let field = &f.ident;
        let ty = &f.ty;
        let mode = f.shell.positional_mode.unwrap_or_else(|| infer_pos_mode(ty));

        // Option positional handling:
        if type_option_inner(ty).is_some() {
            match mode {
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
            }
        } else {
            match mode {
                PosMode::Clone => quote! { parts.push(self.#field.clone()); },
                PosMode::Display => quote! { parts.push(self.#field.to_string()); },
            }
        }
    });

    Ok(quote! {
        impl #impl_generics #trait_path for #name #ty_generics #where_clause {
            fn build(&self) -> ::std::string::String {
                let mut parts: ::std::vec::Vec<::std::string::String> = ::std::vec![#cmd_lit.to_string()];

                #(#flag_arms)*
                #(#opt_kv_arms)*
                #(#pos_arms)*

                parts.join(" ")
            }
        }
    })
}
