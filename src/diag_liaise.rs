use proc_macro2::Span;
use quote::ToTokens;
use syn::Error;
use liaise::{Liaise, LiaiseCodes};

pub type Result<T> = std::result::Result<T, syn::Error>;

#[derive(LiaiseCodes, Debug, Copy, Clone)]
#[liaise(prefix = "SHELL")]
pub enum Code {
    // 0xxx: struct-level
    #[liaise(code = 1, msg = "missing #[shell(cmd = \"...\")] on struct")]
    MissingStructCmd,

    #[liaise(code = 2, msg = "SimpleImpl only supports structs")]
    OnlyStructsSupported,

    // #[liaise(code = 3, msg = "SimpleImpl requires a named-field struct (e.g., struct Foo { bar: i32 })")]
    #[liaise(code = 3, msg = "SimpleImpl requires a named-field struct (e.g., struct Foo ( bar: i32 ))")]
    NamedFieldsRequired,
    // ...
    #[liaise(code = 4, msg = "SimpleImpl requires named fields (e.g., `field_name: Type`)")]
    NamedFieldsRequired2,
    #[liaise(code = 5, msg = "expected named field")]
    NamedFieldsRequiredGeneral,
    // ...
    // ... 1xxx, 2xxx, 3xxx ...
    // 1xxx: field-level shell annotation / emission validation
    #[liaise(code = 1001, msg = "field has multiple #[shell(...)] emission kinds; choose only one of: flag/opt_kv/opt_prefix/opt_eq/arg")]
    MultipleEmissionKinds,

    #[liaise(code = 1002, msg = "#[shell(sep = \"...\")] is only valid with #[shell(arg_join_opt = \"other_field\")]")]
    SepWithoutJoin,

    #[liaise(code = 1003, msg = "missing #[shell(order = ...)] (struct has #[shell(require_order)])")]
    RequireOrderMissing,

    #[liaise(code = 1004, msg = "#[shell(order=...)] provided but this field is not emitted by shell (no flag/opt/arg)")]
    OrderOnNonEmitted,

    #[liaise(code = 1005, msg = "arg_join_opt references unknown field")]
    ArgJoinUnknownField,
    #[liaise(
        code = 1006,
        msg = "arg_join_opt references unknown field `{name}`"
    )]
    ArgJoinUnknownFieldWithName { name: &'static str }, // Use &'static str or String depending on your Liaise impl

    // 2xxx: cross-field ordering
    #[liaise(code = 2001, msg = "order target references unknown (or non-emitted) field")]
    OrderTargetUnknown,

    #[liaise(code = 2002, msg = "case-insensitive order target collision; use exact field names in order=\"before:...\"/\"after:...\"")]
    OrderCaseCollision,

    #[liaise(code = 2003, msg = "ordering cycle detected in #[shell(order=...)] constraints (before/after)")]
    OrderCycle,



    // 3xxx: builder-side
    #[liaise(code = 3001, msg = "builder(push) requires a dedicated naming scheme; e.g. #[builder(push_name=\"push_field\")]")]
    BuilderPushNeedsName,

    #[liaise(code = 3002, msg = "#[builder(init_required)] (or #[builder(init_only, required)]) cannot be combined with #[builder(default_expr = \"...\")]")]
    InitRequiredConflictDefault,

    #[liaise(code = 3003, msg = "#[builder(init_only)] cannot be combined with builder kind overrides (flag/opt/set/etc.)")]
    InitOnlyConflictKind,
    // ... previous variants ...
    
    // 1xxx: Attribute syntax
    #[liaise(code = 1006, msg = "order must be an integer, or one of: \"first\", \"last\", \"before:FIELD\", \"after:FIELD\"")]
    InvalidOrderValue,

    #[liaise(code = 1007, msg = "order must be an integer or a string (\"first\"|\"last\"|\"before:FIELD\"|\"after:FIELD\")")]
    InvalidOrderType,

    // 2xxx: Cross-field ordering
    #[liaise(code = 2004, msg = "order=\"before:FIELD\" requires a field name (e.g. \"before:my_field\")")]
    OrderBeforeMissingTarget,

    #[liaise(code = 2005, msg = "order=\"after:FIELD\" requires a field name (e.g. \"after:my_field\")")]
    OrderAfterMissingTarget,
}
// Helpers to bridge your Liaise-powered Code to syn::Error
#[inline]
pub fn err_at(span: Span, code: Code) -> Error {
    Error::new(span, code.render())
}

#[inline]
pub fn err_at_ctx(span: Span, code: Code, ctx: impl std::fmt::Display) -> Error {
    Error::new(span, format!("{}: {}", code.render(), ctx))
}

#[inline]
pub fn err_spanned(tokens: impl ToTokens, code: Code) -> Error {
    Error::new_spanned(tokens, code.render())
}

#[inline]
pub fn err_spanned_ctx(tokens: impl ToTokens, code: Code, ctx: impl std::fmt::Display) -> Error {
    Error::new_spanned(tokens, format!("{}: {}", code.render(), ctx))
}

#[derive(Default)]
pub struct Errors {
    err: Option<syn::Error>,
}

impl Errors {
    #[inline]
    pub fn is_empty(&self) -> bool {
        self.err.is_none()
    }

    #[inline]
    pub fn push(&mut self, e: syn::Error) {
        match &mut self.err {
            Some(existing) => existing.combine(e),
            None => self.err = Some(e),
        }
    }

    #[inline]
    pub fn push_at(&mut self, span: Span, code: Code) {
        self.push(err_at(span, code));
    }

    #[inline]
    pub fn push_at_ctx(&mut self, span: Span, code: Code, ctx: impl std::fmt::Display) {
        self.push(err_at_ctx(span, code, ctx));
    }

    #[inline]
    pub fn push_spanned(&mut self, tokens: impl ToTokens, code: Code) {
        self.push(err_spanned(tokens, code));
    }

    #[inline]
    pub fn push_spanned_ctx(&mut self, tokens: impl ToTokens, code: Code, ctx: impl std::fmt::Display) {
        self.push(err_spanned_ctx(tokens, code, ctx));
    }

    #[inline]
    pub fn finish(self) -> Result<()> {
        match self.err {
            Some(e) => Err(e),
            None => Ok(()),
        }
    }
}
