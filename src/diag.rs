use quote::ToTokens;
use syn::Error;
use proc_macro2::Span;

#[allow(unused)]
#[allow(non_camel_case_types)]
#[derive(Copy, Clone)]
pub(crate) enum E {
    // 0xxx: Struct level
    MissingStructCmd,
    OnlyStructsSupported,
    NamedFieldsRequired,
    NamedFieldsRequired2,
    NamedFieldsRequiredGeneral,

    // 1xxx: Field level
    MultipleEmissionKinds,
    SepWithoutJoin,
    RequireOrderMissing,
    OrderOnNonEmitted,
    ArgJoinUnknownField,
    InvalidOrderValue,
    InvalidOrderType,

    // 2xxx: Ordering
    OrderTargetUnknown,
    OrderCaseCollision,
    OrderCycle,
    OrderBeforeMissingTarget,
    OrderAfterMissingTarget,

    // 3xxx: Builder
    BuilderPushNeedsName,
    InitRequiredConflictDefault,
    InitOnlyConflictKind,

    OptKvRequiresOption,
    OptPrefixRequiresOption,
    OptEqRequiresOption,

    FlagRequiresBool,
    CountFlagRequiresUint,

    MultiOptKvRequiresVec,
    MultiOptPrefixRequiresVec,
    MultiArgFlagRequiresVec,

    MultipleSubcommands,

    ArgCloneRequiresString,

    FmtOnWrongKind,
    FmtNeedsOnePlaceholder,
    JoinOnWrongKind,
    PushRequiresVec,

    Unmatched
}

impl E {
    pub fn render(self) -> &'static str {
        match self {
            E::MissingStructCmd => "SHELL(1): missing #[shell(cmd = \"...\")] on struct",
            E::OnlyStructsSupported => "SHELL(2): SimpleImpl only supports structs",
            E::NamedFieldsRequired => "SHELL(3): SimpleImpl requires a named-field struct (e.g., struct Foo { bar: i32 })",
            E::NamedFieldsRequired2 => "SHELL(4): SimpleImpl requires named fields (e.g., `field_name: Type`)",
            E::NamedFieldsRequiredGeneral => "SHELL(5): expected named field",

            E::MultipleEmissionKinds => "SHELL(1001): field has multiple #[shell(...)] emission kinds; choose only one",
            E::SepWithoutJoin => "SHELL(1002): #[shell(sep = \"...\")] is only valid with #[shell(arg_join_opt = \"other_field\")]",
            E::RequireOrderMissing => "SHELL(1003): missing #[shell(order = ...)] (struct has #[shell(require_order)])",
            E::OrderOnNonEmitted => "SHELL(1004): #[shell(order=...)] provided but this field is not emitted",
            E::ArgJoinUnknownField => "SHELL(1005): arg_join_opt references unknown field",
            E::InvalidOrderValue => "SHELL(1006): order must be an integer, or \"first\", \"last\", \"before:FIELD\", \"after:FIELD\"",
            E::InvalidOrderType => "SHELL(1007): order must be an integer or a string",
            
            E::OptKvRequiresOption => "SHELL(1010): #[shell(opt_kv = \"...\")] requires Option<T>",
            E::OptPrefixRequiresOption => "SHELL(1011): #[shell(opt_prefix = \"...\")] requires Option<T>",
            E::OptEqRequiresOption => "SHELL(1012): #[shell(opt_eq = \"...\")] requires Option<T>",

            E::FlagRequiresBool => "SHELL(1013): #[shell(flag/flag_off)] requires bool",
            E::CountFlagRequiresUint => "SHELL(1014): #[shell(count_flag = \"...\")] requires u8/u16/u32/u64/usize",

            E::MultiOptKvRequiresVec => "SHELL(1015): #[shell(multi_opt_kv = \"...\")] requires Vec<T> or Option<Vec<T>>",
            E::MultiOptPrefixRequiresVec => "SHELL(1016): #[shell(multi_opt_prefix = \"...\")] requires Vec<T> or Option<Vec<T>>",
            E::MultiArgFlagRequiresVec => "SHELL(1017): #[shell(multi_arg_flag = \"...\")] requires Vec<T> or Option<Vec<T>>",

            E::MultipleSubcommands => "SHELL(1018): multiple #[shell(subcommand)] fields; only one is supported",

            E::ArgCloneRequiresString => "SHELL(1019): #[shell(arg_clone)] is only valid for String or Option<String>",
            E::FmtOnWrongKind => "SHELL(1020): #[shell(fmt = \"...\")] applies to kv, opt_kv, multi_opt_kv, multi_arg_flag and positional fields; prefix, eq, flag and count kinds shape their own value",
            E::FmtNeedsOnePlaceholder => "SHELL(1021): #[shell(fmt = \"...\")] needs exactly one `{}`",
            E::JoinOnWrongKind => "SHELL(1022): #[shell(join = \"...\")] applies to multi_arg_flag, multi_opt_kv and a positional Vec",
            E::PushRequiresVec => "SHELL(3004): #[builder(push)] requires a Vec<T> field",
            
            E::OrderTargetUnknown => "SHELL(2001): order target references unknown (or non-emitted) field",
            E::OrderCaseCollision => "SHELL(2002): case-insensitive order target collision; use exact field names",
            E::OrderCycle => "SHELL(2003): ordering cycle detected in #[shell(order=...)] constraints",
            E::OrderBeforeMissingTarget => "SHELL(2004): order=\"before:FIELD\" requires a field name",
            E::OrderAfterMissingTarget => "SHELL(2005): order=\"after:FIELD\" requires a field name",

            E::BuilderPushNeedsName => "SHELL(3001): builder(push) requires a dedicated naming scheme",
            E::InitRequiredConflictDefault => "SHELL(3002): #[builder(init_required)] cannot be combined with default_expr",
            E::InitOnlyConflictKind => "SHELL(3003): #[builder(init_only)] cannot be combined with builder kind overrides",

            E::Unmatched => "internal error: field marked emitted but no emission kind matched",
        }
    }
}

pub(crate) fn err_spanned<T: ToTokens>(t: T, e: E) -> Error {
    Error::new_spanned(t, e.render())
}

pub(crate) fn err_at(span: Span, e: E) -> Error {
    Error::new(span, e.render())
}

#[derive(Default)]
pub(crate) struct Errors {
    err: Option<syn::Error>,
}

impl Errors {
    pub fn push(&mut self, e: syn::Error) {
        match &mut self.err {
            Some(existing) => existing.combine(e),
            None => self.err = Some(e),
        }
    }

    pub fn push_spanned<T: ToTokens>(&mut self, t: T, e: E) {
        self.push(err_spanned(t, e));
    }

    pub fn finish(self) -> Result<(), syn::Error> {
        match self.err {
            Some(e) => Err(e),
            None => Ok(()),
        }
    }
}