use super::*;

pub(crate) fn expand(input: &DeriveInput, mode: ExpandMode) -> Result<TokenStream2, Error> {
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

//     let out_string = out.to_string();
//     if let Ok(file) = syn::parse_file(&out_string) {
//         eprintln!("GENERATED CODE for {}:\n{}", input.ident, prettyplease::unparse(&file));
//     } else {
//         eprintln!("GENERATED CODE (unformatted) for {}:\n{}", input.ident, out_string);
//     }

    Ok(out)
}
