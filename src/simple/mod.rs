//! SimpleBuilder, SimpleShell, SimpleImpl and SimpleShellMode: parse the
//! attributes into the core models, then emit the builder and the shell impl.

use std::{
    cmp::Reverse,
    collections::{BinaryHeap, HashMap},
};

use proc_macro2::{Span, TokenStream as TokenStream2};
use quote::quote;
use syn::{
    spanned::Spanned,
    Attribute,
    Data,
    DeriveInput,
    Error,
    Fields,
    Ident,
    LitStr,
    Path,
    Type,
};

use crate::{diag, diag::E};
use simple_impl_core::{
    BuilderCfg,
    BuilderKind,
    ExpandMode,
    FieldInfo,
    PosMode,
    ShellCfg,
    ShellFieldCfg,
    Order as ShellOrder,
    type_is_bool,
    type_is_string,
    type_option_inner,
    type_vec_inner,
    type_option_vec_inner,
    type_is_uint,
};


mod builder_emit;
mod expand;
mod parse;
mod shell_emit;
mod shell_mode;

pub(crate) use expand::expand;
pub(crate) use shell_mode::expand_shell_mode;

use builder_emit::expand_builder_impl;
use parse::{infer_pos_mode, parse_struct_and_fields};
use shell_emit::expand_shell_impl;
