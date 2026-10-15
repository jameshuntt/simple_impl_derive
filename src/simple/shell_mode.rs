use super::*;

/// Every variant is checked; each problem goes into the buffer and the next
/// variant is still read. The impl is written only when nothing was reported.
pub(crate) fn expand_shell_mode(input: &DeriveInput) -> syn::Result<TokenStream2> {
    let name = &input.ident;
    let (impl_generics, ty_generics, where_clause) = input.generics.split_for_impl();
    let mut errs = SynBuffer::new();

    let data = match &input.data {
        Data::Enum(e) => e,
        _ => {
            errs.push_at(input.span(), E::ModeOnlyEnums);
            return errs.finish_with(TokenStream2::new());
        }
    };

    let mut arms: Vec<TokenStream2> = Vec::new();

    for v in &data.variants {
        // the attribute and the variant's shape are checked independently
        let flag = errs.absorb(variant_flag(v));
        // keep it “mode flags only” (unit variants)
        if !matches!(v.fields, Fields::Unit) {
            errs.push_at(v.span(), E::ModeUnitVariantsOnly);
            continue;
        }
        let Some(flag) = flag else { continue };

        let var_ident = &v.ident;
        if let Some(f) = flag {
            let lit = LitStr::new(&f, v.span());
            arms.push(quote! { Self::#var_ident => parts.push(#lit.to_string()), });
        } else {
            // Allow “silent” variants if you ever want “Default / Inherit”.
            arms.push(quote! { Self::#var_ident => {}, });
        }
    }

    errs.finish_with(quote! {
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

/// `#[shell(flag = "...")]` on one variant.
fn variant_flag(v: &syn::Variant) -> syn::Result<Option<String>> {
    let bag = crate::vocabulary::shell_variant().validate(&simple_impl_attr_kit::AttrBag::from_attrs(&v.attrs, "shell")?)?;
    Ok(bag.optional_str("flag")?.map(ToOwned::to_owned))
}
