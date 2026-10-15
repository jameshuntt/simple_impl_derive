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
use crate::diag::{err_at_ctx, err_spanned, E};
use proc_macro2::Span;
use syn::spanned::Spanned;
use liaise_proc_macro::{ensure, SynBuffer};

/// Segment, every entry and the policy are read; each problem goes into one
/// buffer. The impls are written only when nothing was reported.
pub fn expand_composite_subcommand(input: &DeriveInput) -> syn::Result<TokenStream> {
    let mut errs = SynBuffer::new();

    ensure!(errs, matches!(input.data, Data::Struct(_)), input => E::CompositeSubOnlyStructs);
    let segment = errs.absorb(parse_subcommand_segment(input));
    let entries = parse_surface_entries(input, &mut errs);
    let declared_fields: Vec<Ident> = entries.iter().filter_map(|(e, _)| e.field.clone()).collect();
    let command_specs: Vec<CompositeSurfaceCommandQuote> = entries
        .into_iter()
        .filter_map(|(entry, at)| errs.absorb(entry_to_command_quote(entry, at, &input.ident)))
        .collect();
    let policy = errs.absorb(parse_xccute_policy(input));

    let (Some(segment), Some(policy)) = (segment, policy) else {
        return errs.finish_with(TokenStream::new());
    };

    let quote_spec = CompositeSurfaceQuote::new(
        input.ident.clone(),
        input.vis.clone(),
        segment,
        command_specs,
    )
    .with_declared_fields(declared_fields);

    let contract_impls = emit_composite_surface_impls(&quote_spec);
    let policy_impl = emit_xccute_policy_metadata_impl(&input.ident, &policy);

    errs.finish_with(::quote::quote! {
        #contract_impls
        #policy_impl
    })
}

fn parse_subcommand_segment(input: &DeriveInput) -> syn::Result<String> {
    if !input.attrs.iter().any(|attr| attr.path().is_ident("subcommand")) {
        return Err(err_spanned(input, E::CompositeSubMissingSegment));
    }
    let bag = crate::vocabulary::subcommand().validate(&simple_impl_attr_kit::AttrBag::from_attrs(&input.attrs, "subcommand")?)?;
    Ok(bag.require_str("segment")?.to_owned())
}

/// Every `#[composite(..)]` on the struct and on each field, each with the
/// place it was written; a broken entry reports every problem it has and is
/// still checked for the rest.
fn parse_surface_entries(input: &DeriveInput, errs: &mut SynBuffer) -> Vec<(CompositeEntry, Span)> {
    let mut entries = Vec::new();
    // "no entries" means none was written, not that the written ones failed
    let mut written = false;

    for attr in input.attrs.iter().filter(|attr| attr.path().is_ident("composite")) {
        written = true;
        if let Some(entry) = CompositeEntry::from_registry_attr_into(attr, errs) {
            entries.push((entry, attr.span()));
        }
    }

    if let Data::Struct(syn::DataStruct { fields: Fields::Named(named), .. }) = &input.data {
        for field in &named.named {
            written |= field.attrs.iter().any(|attr| attr.path().is_ident("composite"));
            if let Some(entry) = CompositeEntry::from_field_into(field, errs) {
                entries.push((entry, crate::composite_shell::composite_attr_span(field)));
            }
        }
    }

    // on a non-struct, "only structs" already says it
    if !written && matches!(input.data, Data::Struct(_)) {
        errs.push_spanned(input, E::CompositeSubNoEntries);
    }

    entries
}

fn entry_to_command_quote(
    entry: CompositeEntry,
    at: Span,
    surface_ident: &Ident,
) -> syn::Result<CompositeSurfaceCommandQuote> {
    if entry.kind != CompositeEntryKind::Command {
        return Err(err_at_ctx(at, E::CompositeSubNestedSurface, format!("`{}`", entry.segment)));
    }

    let rooted_ident = nested_command_rooted_ident_for(surface_ident, &entry.method);
    Ok(CompositeSurfaceCommandQuote::new(
        entry.method,
        entry.ty,
        rooted_ident,
        entry.init_args,
    ))
}
