
// --------------------
// type helpers
// --------------------

use syn::Type;

pub(crate)fn type_is_bool(ty: &Type) -> bool {
    matches!(ty, Type::Path(p) if p.path.segments.last().is_some_and(|s| s.ident == "bool"))
}

pub(crate)fn type_is_string(ty: &Type) -> bool {
    matches!(ty, Type::Path(p) if p.path.segments.last().is_some_and(|s| s.ident == "String"))
}

pub(crate)fn type_option_inner(ty: &Type) -> Option<&Type> {
    let Type::Path(p) = ty else { return None; };
    let seg = p.path.segments.last()?;
    if seg.ident != "Option" {
        return None;
    }
    let syn::PathArguments::AngleBracketed(ab) = &seg.arguments else { return None; };
    let first = ab.args.first()?;
    match first {
        syn::GenericArgument::Type(t) => Some(t),
        _ => None,
    }
}

pub(crate)fn type_vec_inner(ty: &Type) -> Option<&Type> {
    let Type::Path(p) = ty else { return None; };
    let seg = p.path.segments.last()?;
    if seg.ident != "Vec" {
        return None;
    }
    let syn::PathArguments::AngleBracketed(ab) = &seg.arguments else { return None; };
    let first = ab.args.first()?;
    match first {
        syn::GenericArgument::Type(t) => Some(t),
        _ => None,
    }
}
