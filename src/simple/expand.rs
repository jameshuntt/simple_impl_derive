use super::*;

/// Every part writes its problems into one buffer and keeps going; the single
/// `finish_with` at the end reports all of them in one compile. Every stage
/// runs: each emitter skips only the fields whose own problems were already
/// reported, so nothing waits on an earlier fix and nothing is reported twice.
pub(crate) fn expand(input: &DeriveInput, mode: ExpandMode) -> Result<TokenStream2, Error> {
    let mut errs = SynBuffer::new();
    let mut out = TokenStream2::new();

    let Some(parsed) = parse_struct_and_fields(input, mode, &mut errs) else {
        return errs.finish_with(out);
    };
    let without = |failed: &HashSet<String>| -> Vec<FieldInfo> {
        parsed.fields.iter().filter(|f| !failed.contains(&f.ident.to_string())).cloned().collect()
    };

    if matches!(mode, ExpandMode::BuilderOnly | ExpandMode::Both) {
        out.extend(errs.absorb(expand_builder_impl(input, &without(&parsed.builder_failed))));
    }
    if matches!(mode, ExpandMode::ShellOnly | ExpandMode::Both) {
        let fields = without(&parsed.shell_failed);
        out.extend(expand_shell_impl(input, &parsed, &fields, &mut errs));
    }

//     let out_string = out.to_string();
//     if let Ok(file) = syn::parse_file(&out_string) {
//         eprintln!("GENERATED CODE for {}:\n{}", input.ident, prettyplease::unparse(&file));
//     } else {
//         eprintln!("GENERATED CODE (unformatted) for {}:\n{}", input.ident, out_string);
//     }

    errs.finish_with(out)
}
