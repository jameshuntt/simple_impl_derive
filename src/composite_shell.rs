use proc_macro2::TokenStream;
use simple_impl_attr_kit::{AttrBag, AttrExpected, AttrSchema};
use simple_impl_core::{CompositeEntry, CompositeEntryKind};
use simple_impl_quote_kit::{
    emit_composite_shell_root_impls,
    CompositeShellEntryQuote,
    CompositeShellQuote,
};
use syn::{Data, DeriveInput, Fields};

pub fn expand_composite_shell(input: &DeriveInput) -> syn::Result<TokenStream> {
    ensure_struct(input)?;

    let program = parse_shell_program(input)?;
    let entries = parse_composite_entries(input)?;
    let declared_fields: Vec<syn::Ident> = entries.iter().filter_map(|e| e.field.clone()).collect();
    let quote_spec = CompositeShellQuote::new(input.ident.clone(), program, entries.into_iter().map(|e| e.quote).collect())
        .with_declared_fields(declared_fields);

    Ok(emit_composite_shell_root_impls(&quote_spec))
}

fn ensure_struct(input: &DeriveInput) -> syn::Result<()> {
    if matches!(input.data, Data::Struct(_)) {
        Ok(())
    } else {
        Err(syn::Error::new_spanned(
            input,
            "CompositeShell can only be derived for structs",
        ))
    }
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

fn parse_composite_entries(input: &DeriveInput) -> syn::Result<Vec<ParsedEntry>> {
    let mut entries = Vec::new();

    for attr in input.attrs.iter().filter(|attr| attr.path().is_ident("composite")) {
        let entry = CompositeEntry::from_registry_attr(attr)?.ok_or_else(|| {
            syn::Error::new_spanned(attr, "expected #[composite(command = \"...\", ty = SomeType)]")
        })?;
        entries.push(ParsedEntry { field: None, quote: entry_to_quote(entry)? });
    }

    if let Data::Struct(syn::DataStruct { fields: Fields::Named(named), .. }) = &input.data {
        for field in &named.named {
            if let Some(entry) = CompositeEntry::from_field(field)? {
                let declared = entry.field.clone();
                entries.push(ParsedEntry { field: declared, quote: entry_to_quote(entry)? });
            }
        }
    }

    if entries.is_empty() {
        return Err(syn::Error::new_spanned(
            input,
            "CompositeShell requires at least one #[composite(command = \"...\", ty = SomeType)] entry",
        ));
    }

    Ok(entries)
}

fn entry_to_quote(entry: CompositeEntry) -> syn::Result<CompositeShellEntryQuote> {
    match entry.kind {
        CompositeEntryKind::Command => Ok(
            CompositeShellEntryQuote::command(entry.method, entry.ty)
                .with_init_args(entry.init_args),
        ),
        CompositeEntryKind::Surface => {
            if !entry.init_args.is_empty() {
                return Err(syn::Error::new_spanned(
                    entry.method,
                    "CompositeShell surface entries cannot use init args yet",
                ));
            }

            Ok(CompositeShellEntryQuote::surface(entry.method, entry.ty))
        }
    }
}
