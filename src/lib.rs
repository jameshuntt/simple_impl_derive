#![doc = include_str!("../README.md")]
#![forbid(unsafe_code)]

use proc_macro::TokenStream;
use syn::{parse_macro_input, DeriveInput};

mod composite_shell;
mod composite_subcommand;
mod diag;
mod vocabulary;
mod simple;
mod simple_subcommand;
mod xccute_policy;

/// `new()` and a setter per field, shaped by `#[builder(...)]`.
#[proc_macro_derive(SimpleBuilder, attributes(builder))]
pub fn derive_simple_builder(input: TokenStream) -> TokenStream {
    let input = parse_macro_input!(input as DeriveInput);
    match simple::expand(&input, simple_impl_core::ExpandMode::BuilderOnly) {
        Ok(ts) => ts.into(),
        Err(e) => e.to_compile_error().into(),
    }
}

/// A `ShellCommand` impl whose `build()` joins the argument vector from `#[shell(...)]` on each field.
#[proc_macro_derive(SimpleShell, attributes(shell))]
pub fn derive_simple_shell(input: TokenStream) -> TokenStream {
    let input = parse_macro_input!(input as DeriveInput);
    match simple::expand(&input, simple_impl_core::ExpandMode::ShellOnly) {
        Ok(ts) => ts.into(),
        Err(e) => e.to_compile_error().into(),
    }
}

/// `SimpleBuilder` and `SimpleShell` together.
#[proc_macro_derive(SimpleImpl, attributes(builder, shell))]
pub fn derive_simple_impl(input: TokenStream) -> TokenStream {
    let input = parse_macro_input!(input as DeriveInput);
    match simple::expand(&input, simple_impl_core::ExpandMode::Both) {
        Ok(ts) => ts.into(),
        Err(e) => e.to_compile_error().into(),
    }
}

/// An enum whose selected variant contributes one flag through a `#[shell(mode)]` field.
#[proc_macro_derive(SimpleShellMode, attributes(shell))]
pub fn derive_simple_shell_mode(input: TokenStream) -> TokenStream {
    let input = parse_macro_input!(input as DeriveInput);
    match simple::expand_shell_mode(&input) {
        Ok(ts) => ts.into(),
        Err(e) => e.to_compile_error().into(),
    }
}


/// A leaf argv command under a root: `#[subcommand(segment = "..")]`, `#[arg(..)]` fields, validation rules, `#[xccute_policy(..)]`.
#[proc_macro_derive(SimpleSubCommand, attributes(subcommand, builder, arg, validate, requires, invalid_without, only_pair_with, conflicts_with, one_of, at_least_one_of, xccute_policy))]
pub fn derive_simple_subcommand(input: TokenStream) -> TokenStream {
    let input = parse_macro_input!(input as DeriveInput);
    match simple_subcommand::expand_simple_subcommand(&input) {
        Ok(ts) => ts.into(),
        Err(e) => e.to_compile_error().into(),
    }
}

/// A root program whose `#[composite(..)]` entries become methods returning rooted commands and surfaces.
#[proc_macro_derive(CompositeShell, attributes(shell, composite))]
pub fn derive_composite_shell(input: TokenStream) -> TokenStream {
    let input = parse_macro_input!(input as DeriveInput);
    match composite_shell::expand_composite_shell(&input) {
        Ok(ts) => ts.into(),
        Err(e) => e.to_compile_error().into(),
    }
}

/// A nested surface: a segment whose `#[composite(command = ..)]` entries become methods on the rooted surface.
#[proc_macro_derive(CompositeSubCommand, attributes(subcommand, composite, xccute_policy))]
pub fn derive_composite_subcommand(input: TokenStream) -> TokenStream {
    let input = parse_macro_input!(input as DeriveInput);
    match composite_subcommand::expand_composite_subcommand(&input) {
        Ok(ts) => ts.into(),
        Err(e) => e.to_compile_error().into(),
    }
}
