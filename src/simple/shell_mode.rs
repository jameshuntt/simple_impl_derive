use super::*;

pub(crate) fn expand_shell_mode(input: &DeriveInput) -> syn::Result<TokenStream2> {
    let name = &input.ident;
    let (impl_generics, ty_generics, where_clause) = input.generics.split_for_impl();

    let data = match &input.data {
        Data::Enum(e) => e,
        _ => return Err(Error::new(input.span(), "SimpleShellMode only supports enums")),
    };

    let mut arms: Vec<TokenStream2> = Vec::new();

    for v in &data.variants {
        // keep it “mode flags only” (unit variants)
        if !matches!(v.fields, Fields::Unit) {
            return Err(Error::new(
                v.span(),
                "SimpleShellMode only supports unit variants (e.g. Json, Xml, Raw)",
            ));
        }

        let bag = crate::vocabulary::shell_variant().validate(&simple_impl_attr_kit::AttrBag::from_attrs(&v.attrs, "shell")?)?;
        let flag: Option<String> = bag.optional_str("flag")?.map(ToOwned::to_owned);

        let var_ident = &v.ident;
        if let Some(f) = flag {
            let lit = LitStr::new(&f, v.span());
            arms.push(quote! { Self::#var_ident => parts.push(#lit.to_string()), });
        } else {
            // Allow “silent” variants if you ever want “Default / Inherit”.
            arms.push(quote! { Self::#var_ident => {}, });
        }
    }

    Ok(quote! {
        impl #impl_generics #name #ty_generics #where_clause {
            #[doc(hidden)]
            #[inline]
            pub fn __simple_impl_shell_mode_emit(
                &self,
                parts: &mut ::std::vec::Vec<::std::string::String>
            ) {
                match self {
                    #(#arms)*
                }
            }
        }
    })
}
