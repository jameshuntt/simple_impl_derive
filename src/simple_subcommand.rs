use proc_macro2::TokenStream;
use quote::format_ident;
use simple_impl_core::{ValidationRule, ValidationSpec};
use simple_impl_quote_kit::{
    emit_simple_subcommand_contract_impls,
    SimpleSubcommandQuote,
    SubcommandArgKind,
    SubcommandFieldQuote,
    ValidationRuleQuote,
};
use simple_impl_attr_kit::AttrBag;
use syn::{Data, DeriveInput, Field, Fields, Ident, Type};
use crate::xccute_policy::{emit_xccute_policy_metadata_impl, parse_xccute_policy};

#[derive(Debug, Clone)]
enum ArgKind {
    Flag(String),
    KeyValue(String),
    Positional,
}

impl ArgKind {
    fn into_quote_kind(self) -> SubcommandArgKind {
        match self {
            ArgKind::Flag(flag) => SubcommandArgKind::Flag(flag),
            ArgKind::KeyValue(key) => SubcommandArgKind::KeyValue(key),
            ArgKind::Positional => SubcommandArgKind::Positional,
        }
    }
}

#[derive(Debug, Clone)]
struct SubcommandField {
    ident: Ident,
    ty: Type,
    method: Ident,
    kind: ArgKind,
    init_required: bool,
    validation_rules: Vec<ValidationRule>,
}

impl SubcommandField {
    fn into_quote_spec(self) -> SubcommandFieldQuote {
        SubcommandFieldQuote::new(
            self.ident,
            self.ty,
            self.method,
            self.kind.into_quote_kind(),
        )
        .with_init_required(self.init_required)
    }
}

pub fn expand_simple_subcommand(input: &DeriveInput) -> syn::Result<TokenStream> {
    let segment = parse_subcommand_segment(input)?;
    let fields = parse_subcommand_fields(input)?;
    let mut validation_rules = ValidationSpec::from(
        simple_impl_attr_kit::parse_validation_attrs(&input.attrs)?,
    )
    .rules()
    .to_vec();

    for field in &fields {
        validation_rules.extend(field.validation_rules.iter().cloned());
    }

    let validation_rules = validation_rules
        .into_iter()
        .map(validation_rule_into_quote)
        .collect::<syn::Result<Vec<_>>>()?;

    let field_specs = fields
        .into_iter()
        .map(SubcommandField::into_quote_spec)
        .collect();

    let quote_spec = SimpleSubcommandQuote::new(
        input.ident.clone(),
        input.vis.clone(),
        segment,
        field_specs,
    )
    .with_validation_rules(validation_rules);

    let contract_impls = emit_simple_subcommand_contract_impls(&quote_spec)?;
    let policy = parse_xccute_policy(input)?;
    let policy_impl = emit_xccute_policy_metadata_impl(&input.ident, &policy);

    Ok(::quote::quote! {
        #contract_impls
        #policy_impl
    })
}

fn parse_subcommand_segment(input: &DeriveInput) -> syn::Result<String> {
    if !input.attrs.iter().any(|attr| attr.path().is_ident("subcommand")) {
        return Err(syn::Error::new_spanned(input, "SimpleSubCommand requires #[subcommand(segment = \"...\")]"));
    }
    let bag = crate::vocabulary::subcommand().validate(&AttrBag::from_attrs(&input.attrs, "subcommand")?)?;
    Ok(bag.require_str("segment")?.to_owned())
}


fn parse_subcommand_fields(input: &DeriveInput) -> syn::Result<Vec<SubcommandField>> {
    let Data::Struct(data) = &input.data else {
        return Err(syn::Error::new_spanned(input, "SimpleSubCommand can only be derived for structs"));
    };

    let Fields::Named(named) = &data.fields else {
        return Err(syn::Error::new_spanned(input, "SimpleSubCommand requires named fields"));
    };

    named.named.iter().map(parse_subcommand_field).collect()
}

fn parse_subcommand_field(field: &Field) -> syn::Result<SubcommandField> {
    let ident = field
        .ident
        .clone()
        .ok_or_else(|| syn::Error::new_spanned(field, "SimpleSubCommand requires named fields"))?;

    let builder = crate::vocabulary::subcommand_builder().validate(&AttrBag::from_attrs(&field.attrs, "builder")?)?;
    let method = builder.optional_str("method")?.map(|m| format_ident!("{}", m));
    let init_required = builder.flag("init_required")?;

    let arg = crate::vocabulary::subcommand_arg().validate(&AttrBag::from_attrs(&field.attrs, "arg")?)?;
    let kind = if let Some(flag) = arg.optional_str("flag")? {
        Some(ArgKind::Flag(flag.to_owned()))
    } else if let Some(kv) = arg.optional_str("kv")? {
        Some(ArgKind::KeyValue(kv.to_owned()))
    } else if arg.flag("positional")? {
        Some(ArgKind::Positional)
    } else {
        None
    };

    let validation_rules = ValidationSpec::from(
        simple_impl_attr_kit::parse_field_validation_attrs(
            ident.to_string(),
            &field.attrs,
        )?,
    )
    .rules()
    .to_vec();

    let kind = kind.ok_or_else(|| syn::Error::new_spanned(field, "SimpleSubCommand fields require #[arg(flag = \"...\")], #[arg(kv = \"...\")], or #[arg(positional)]"))?;
    let method = method.unwrap_or_else(|| ident.clone());

    Ok(SubcommandField {
        ident,
        ty: field.ty.clone(),
        method,
        kind,
        init_required,
        validation_rules,
    })
}

fn validation_rule_into_quote(rule: ValidationRule) -> syn::Result<ValidationRuleQuote> {
    match rule {
        ValidationRule::Requires { field, required } => Ok(ValidationRuleQuote::Requires {
            field: ident_from_rule_field(&field)?,
            required: ident_from_rule_field(&required)?,
        }),
        ValidationRule::InvalidWithout { field, required } => Ok(ValidationRuleQuote::InvalidWithout {
            field: ident_from_rule_field(&field)?,
            required: ident_from_rule_field(&required)?,
        }),
        ValidationRule::OnlyPairWith { field, paired_with } => Ok(ValidationRuleQuote::OnlyPairWith {
            field: ident_from_rule_field(&field)?,
            paired_with: ident_from_rule_field(&paired_with)?,
        }),
        ValidationRule::ConflictsWith { field, conflicts_with } => Ok(ValidationRuleQuote::ConflictsWith {
            field: ident_from_rule_field(&field)?,
            conflicts_with: ident_from_rule_field(&conflicts_with)?,
        }),
        ValidationRule::OneOf { fields } => Ok(ValidationRuleQuote::OneOf {
            fields: fields
                .iter()
                .map(|field| ident_from_rule_field(field))
                .collect::<syn::Result<Vec<_>>>()?,
        }),
        ValidationRule::AtLeastOneOf { fields } => Ok(ValidationRuleQuote::AtLeastOneOf {
            fields: fields
                .iter()
                .map(|field| ident_from_rule_field(field))
                .collect::<syn::Result<Vec<_>>>()?,
        }),
        ValidationRule::CustomFunction { function_path } => Ok(ValidationRuleQuote::CustomFunction {
            function_path: syn::parse_str(&function_path)?,
        }),
    }
}

fn ident_from_rule_field(field: &str) -> syn::Result<Ident> {
    syn::parse_str::<Ident>(field).map_err(|error| {
        syn::Error::new(
            proc_macro2::Span::call_site(),
            format!("validation rule field `{field}` must be a Rust field identifier: {error}"),
        )
    })
}
