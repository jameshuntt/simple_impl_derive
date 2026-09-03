use proc_macro2::TokenStream;
use quote::quote;
use syn::{DeriveInput, Ident};

#[derive(Debug, Clone, Default)]
pub struct ParsedXccutePolicy {
    pub sensitive: bool,
    pub requires_sudo: bool,
    pub iam_scope: Option<String>,
    pub path_role: Option<String>,
    pub dry_run_default: Option<bool>,
}

pub fn parse_xccute_policy(input: &DeriveInput) -> syn::Result<ParsedXccutePolicy> {
    let bag = crate::vocabulary::xccute_policy()
        .validate(&simple_impl_attr_kit::AttrBag::from_attrs(&input.attrs, "xccute_policy")?)?;

    Ok(ParsedXccutePolicy {
        sensitive: bag.flag("sensitive")?,
        requires_sudo: bag.flag("requires_sudo")?,
        iam_scope: bag.optional_str("iam_scope")?.map(ToOwned::to_owned),
        path_role: bag.optional_str("path_role")?.map(ToOwned::to_owned),
        dry_run_default: bag.optional_bool("dry_run_default")?,
    })
}

pub fn emit_xccute_policy_metadata_impl(
    ident: &Ident,
    policy: &ParsedXccutePolicy,
) -> TokenStream {
    let sensitive = policy.sensitive;
    let requires_sudo = policy.requires_sudo;
    let dry_run_default = policy.dry_run_default.unwrap_or(true);

    let iam_scope = policy.iam_scope.as_ref().map(|iam_scope| {
        quote! {
            policy = policy.with_iam_scope(#iam_scope);
        }
    });
    let path_role = policy.path_role.as_ref().map(|path_role| {
        quote! {
            policy = policy.with_path_role(#path_role);
        }
    });

    quote! {
        impl ::xccute_contract::XccutePolicyMetadata for #ident {
            fn xccute_policy(&self) -> ::xccute_contract::XccuteCommandPolicyMetadata {
                let mut policy = ::xccute_contract::XccuteCommandPolicyMetadata::new()
                    .with_sensitive(#sensitive)
                    .with_requires_sudo(#requires_sudo)
                    .with_dry_run_default(#dry_run_default);
                #iam_scope
                #path_role
                policy
            }
        }
    }
}
