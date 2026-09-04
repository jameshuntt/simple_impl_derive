use super::*;

pub(super) fn expand_builder_impl(input: &DeriveInput, fields: &[FieldInfo]) -> Result<TokenStream2, Error> {
    let name = &input.ident;
    let (impl_generics, ty_generics, where_clause) = input.generics.split_for_impl();

    // Required = builder.required (includes not_in_default)
    let required: Vec<&FieldInfo> = fields.iter().filter(|f| f.builder.required).collect();

    // new(...) generation: `Default` alone when no field is required and none
    // carries a `default_expr`; otherwise every field is initialised explicitly.
    let any_default_expr = fields.iter().any(|f| f.builder.default_expr.is_some());
    let new_fn = if required.is_empty() && !any_default_expr {
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
        let method_ident = match &f.builder.method {
            Some(name) => ::quote::format_ident!("{}", name),
            None => f.ident.clone(),
        };
        let method = &method_ident;
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
                let inner = type_vec_inner(ty).expect("Push kind requires Vec<T>; checked at parse");
                quote! {
                    #[inline]
                    pub fn #method(mut self, value: impl ::core::convert::Into<#inner>) -> Self {
                        self.#field.push(value.into());
                        self
                    }
                }
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
