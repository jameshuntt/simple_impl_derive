use proc_macro2::TokenStream;
use simple_impl_core::{CompositeEntry, CompositeEntryKind};
use crate::xccute_policy::{emit_xccute_policy_metadata_impl, parse_xccute_policy};
use simple_impl_quote_kit::{
    emit_composite_surface_impls,
    nested_command_rooted_ident_for,
    CompositeSurfaceCommandQuote,
    CompositeSurfaceQuote,
};
use syn::{Data, DeriveInput, Fields, Ident};

pub fn expand_composite_subcommand(input: &DeriveInput) -> syn::Result<TokenStream> {
    ensure_struct(input)?;

    let segment = parse_subcommand_segment(input)?;
    let entries = parse_surface_entries(input)?;
    let command_specs = entries
        .into_iter()
        .map(|entry| entry_to_command_quote(entry, &input.ident))
        .collect::<syn::Result<Vec<_>>>()?;

    let quote_spec = CompositeSurfaceQuote::new(
        input.ident.clone(),
        input.vis.clone(),
        segment,
        command_specs,
    );

    let contract_impls = emit_composite_surface_impls(&quote_spec);
    let policy = parse_xccute_policy(input)?;
    let policy_impl = emit_xccute_policy_metadata_impl(&input.ident, &policy);

    Ok(::quote::quote! {
        #contract_impls
        #policy_impl
    })
}

fn ensure_struct(input: &DeriveInput) -> syn::Result<()> {
    if matches!(input.data, Data::Struct(_)) {
        Ok(())
    } else {
        Err(syn::Error::new_spanned(
            input,
            "CompositeSubCommand can only be derived for structs",
        ))
    }
}

fn parse_subcommand_segment(input: &DeriveInput) -> syn::Result<String> {
    if !input.attrs.iter().any(|attr| attr.path().is_ident("subcommand")) {
        return Err(syn::Error::new_spanned(input, "CompositeSubCommand requires #[subcommand(segment = \"...\")]"));
    }
    let bag = crate::vocabulary::subcommand().validate(&simple_impl_attr_kit::AttrBag::from_attrs(&input.attrs, "subcommand")?)?;
    Ok(bag.require_str("segment")?.to_owned())
}

fn parse_surface_entries(input: &DeriveInput) -> syn::Result<Vec<CompositeEntry>> {
    let mut entries = Vec::new();

    for attr in input.attrs.iter().filter(|attr| attr.path().is_ident("composite")) {
        let entry = CompositeEntry::from_registry_attr(attr)?.ok_or_else(|| {
            syn::Error::new_spanned(attr, "expected #[composite(command = \"...\", ty = SomeType)]")
        })?;
        entries.push(entry);
    }

    if let Data::Struct(syn::DataStruct { fields: Fields::Named(named), .. }) = &input.data {
        for field in &named.named {
            if let Some(entry) = CompositeEntry::from_field(field)? {
                entries.push(entry);
            }
        }
    }

    if entries.is_empty() {
        return Err(syn::Error::new_spanned(
            input,
            "CompositeSubCommand requires at least one #[composite(command = \"...\", ty = SomeType)] entry",
        ));
    }

    Ok(entries)
}

fn entry_to_command_quote(
    entry: CompositeEntry,
    surface_ident: &Ident,
) -> syn::Result<CompositeSurfaceCommandQuote> {
    if entry.kind != CompositeEntryKind::Command {
        return Err(syn::Error::new_spanned(
            entry.method,
            "CompositeSubCommand nested surfaces are not generated yet; this pass supports command entries",
        ));
    }

    let rooted_ident = nested_command_rooted_ident_for(surface_ident, &entry.method);
    Ok(CompositeSurfaceCommandQuote::new(
        entry.method,
        entry.ty,
        rooted_ident,
        entry.init_args,
    ))
}
