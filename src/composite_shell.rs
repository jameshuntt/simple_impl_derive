use proc_macro2::TokenStream;
use simple_impl_attr_kit::{AttrBag, AttrExpected, AttrSchema};
use simple_impl_core::{CompositeEntry, CompositeEntryKind};
use simple_impl_quote_kit::{
    emit_composite_shell_help_impl,
    emit_composite_shell_root_impls,
    CompositeShellEntryQuote,
    CompositeShellQuote,
};
use syn::{Data, DeriveInput, Fields};
use crate::diag::{err_at_ctx, E};
use proc_macro2::Span;
use syn::spanned::Spanned;
use liaise_proc_macro::{ensure, SynBuffer};

/// The program and every entry are read; each problem goes into one buffer.
/// The impls are written only when nothing was reported.
pub fn expand_composite_shell(input: &DeriveInput) -> syn::Result<TokenStream> {
    let mut errs = SynBuffer::new();

    ensure!(errs, matches!(input.data, Data::Struct(_)), input => E::CompositeShellOnlyStructs);
    let program = errs.absorb(parse_shell_program(input));
    let entries = parse_composite_entries(input, &mut errs);

    let Some(program) = program.filter(|_| errs.is_empty()) else {
        return errs.finish_with(TokenStream::new());
    };

    let declared_fields: Vec<syn::Ident> = entries.iter().filter_map(|e| e.field.clone()).collect();
    let quote_spec = CompositeShellQuote::new(input.ident.clone(), program, entries.into_iter().map(|e| e.quote).collect())
        .with_declared_fields(declared_fields);

    // the root's help lines, after its own items
    let root_impls = emit_composite_shell_root_impls(&quote_spec);
    let help_impl = emit_composite_shell_help_impl(&quote_spec);
    Ok(::quote::quote! {
        #root_impls
        #help_impl
    })
}

fn parse_shell_program(input: &DeriveInput) -> syn::Result<String> {
    let parsed = AttrBag::from_attrs(&input.attrs, "shell")?;
    let bag = AttrSchema::new()
        .required("program", AttrExpected::String)
        .validate(&parsed)?;

    bag.require_str("program").map(ToOwned::to_owned)
}

struct ParsedEntry {
    field: Option<syn::Ident>,
    quote: CompositeShellEntryQuote,
}

/// Every `#[composite(..)]` on the struct and on each field; a broken entry
/// reports every problem it has and is still checked for the rest.
fn parse_composite_entries(input: &DeriveInput, errs: &mut SynBuffer) -> Vec<ParsedEntry> {
    let mut entries = Vec::new();
    // "no entries" means none was written, not that the written ones failed
    let mut written = false;

    for attr in input.attrs.iter().filter(|attr| attr.path().is_ident("composite")) {
        written = true;
        if let Some(entry) = CompositeEntry::from_registry_attr_into(attr, errs) {
            if let Some(quote) = errs.absorb(entry_to_quote(entry, attr.span())) {
                entries.push(ParsedEntry { field: None, quote });
            }
        }
    }

    if let Data::Struct(syn::DataStruct { fields: Fields::Named(named), .. }) = &input.data {
        for field in &named.named {
            written |= field.attrs.iter().any(|attr| attr.path().is_ident("composite"));
            if let Some(entry) = CompositeEntry::from_field_into(field, errs) {
                let declared = entry.field.clone();
                if let Some(quote) = errs.absorb(entry_to_quote(entry, composite_attr_span(field))) {
                    entries.push(ParsedEntry { field: declared, quote });
                }
            }
        }
    }

    // on a non-struct, "only structs" already says it
    if !written && matches!(input.data, Data::Struct(_)) {
        errs.push_spanned(input, E::CompositeShellNoEntries);
    }

    entries
}

/// Where an entry was written, for its errors: names parsed from strings
/// carry no place of their own.
pub(crate) fn composite_attr_span(field: &syn::Field) -> Span {
    field.attrs.iter().find(|a| a.path().is_ident("composite")).map_or_else(|| field.span(), |a| a.span())
}

fn entry_to_quote(entry: CompositeEntry, at: Span) -> syn::Result<CompositeShellEntryQuote> {
    match entry.kind {
        CompositeEntryKind::Command => Ok(
            CompositeShellEntryQuote::command(entry.method, entry.ty)
                .with_init_args(entry.init_args),
        ),
        CompositeEntryKind::Surface => {
            if !entry.init_args.is_empty() {
                return Err(err_at_ctx(at, E::CompositeShellSurfaceInitArgs, format!("`{}`", entry.segment)));
            }

            Ok(CompositeShellEntryQuote::surface(entry.method, entry.ty))
        }
    }
}
