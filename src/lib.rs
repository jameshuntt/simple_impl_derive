//! Derives that turn a struct into a command-line builder and the argument
//! vector it produces.
//!
//! * [`SimpleBuilder`](derive@SimpleBuilder): `new()` and a setter per field,
//!   shaped by `#[builder(...)]` (`required`, `into`, `opt`, `vec_iter`,
//!   `default_expr`, ...).
//! * [`SimpleShell`](derive@SimpleShell): a `ShellCommand` impl whose
//!   `build()` assembles the argument vector from `#[shell(...)]` on each
//!   field (`flag`, `opt_kv`, `arg_clone`, `multi_opt_kv`, `count_flag`,
//!   `order`, ...).
//! * [`SimpleImpl`](derive@SimpleImpl): both at once.
//! * [`SimpleShellMode`](derive@SimpleShellMode): an enum whose selected
//!   variant contributes one flag.
//! * [`SimpleSubCommand`](derive@SimpleSubCommand), [`CompositeShell`](derive@CompositeShell),
//!   [`CompositeSubCommand`](derive@CompositeSubCommand): commands made of
//!   subcommands, with validation rules and an `xccute_policy`.
//!
//! Every attribute is parsed through
//! [`simple_impl_attr_kit`](https://crates.io/crates/simple_impl_attr_kit)
//! against a schema in one module, so an unknown key names the nearest known
//! one, a key given twice or of the wrong kind is refused at the key, and the
//! whole vocabulary is documented in one place. The README lists every key.
//!
//! The generated code expands to the macros of
//! [`simple_impl`](https://crates.io/crates/simple_impl), and runs against
//! the `ShellCommand` trait named by `trait_path`.

use proc_macro::TokenStream;
use syn::{parse_macro_input, DeriveInput};

mod composite_shell;
mod composite_subcommand;
mod diag;
mod vocabulary;
mod simple;
mod simple_subcommand;
mod xccute_policy;

#[proc_macro_derive(SimpleBuilder, attributes(builder))]
pub fn derive_simple_builder(input: TokenStream) -> TokenStream {
    let input = parse_macro_input!(input as DeriveInput);
    match simple::expand(&input, simple_impl_core::ExpandMode::BuilderOnly) {
        Ok(ts) => ts.into(),
        Err(e) => e.to_compile_error().into(),
    }
}

#[proc_macro_derive(SimpleShell, attributes(shell))]
pub fn derive_simple_shell(input: TokenStream) -> TokenStream {
    let input = parse_macro_input!(input as DeriveInput);
    match simple::expand(&input, simple_impl_core::ExpandMode::ShellOnly) {
        Ok(ts) => ts.into(),
        Err(e) => e.to_compile_error().into(),
    }
}

#[proc_macro_derive(SimpleImpl, attributes(builder, shell))]
pub fn derive_simple_impl(input: TokenStream) -> TokenStream {
    let input = parse_macro_input!(input as DeriveInput);
    match simple::expand(&input, simple_impl_core::ExpandMode::Both) {
        Ok(ts) => ts.into(),
        Err(e) => e.to_compile_error().into(),
    }
}

#[proc_macro_derive(SimpleShellMode, attributes(shell))]
pub fn derive_simple_shell_mode(input: TokenStream) -> TokenStream {
    let input = parse_macro_input!(input as DeriveInput);
    match simple::expand_shell_mode(&input) {
        Ok(ts) => ts.into(),
        Err(e) => e.to_compile_error().into(),
    }
}


#[proc_macro_derive(SimpleSubCommand, attributes(subcommand, builder, arg, validate, requires, invalid_without, only_pair_with, conflicts_with, one_of, at_least_one_of, xccute_policy))]
pub fn derive_simple_subcommand(input: TokenStream) -> TokenStream {
    let input = parse_macro_input!(input as DeriveInput);
    match simple_subcommand::expand_simple_subcommand(&input) {
        Ok(ts) => ts.into(),
        Err(e) => e.to_compile_error().into(),
    }
}

#[proc_macro_derive(CompositeShell, attributes(shell, composite))]
pub fn derive_composite_shell(input: TokenStream) -> TokenStream {
    let input = parse_macro_input!(input as DeriveInput);
    match composite_shell::expand_composite_shell(&input) {
        Ok(ts) => ts.into(),
        Err(e) => e.to_compile_error().into(),
    }
}

#[proc_macro_derive(CompositeSubCommand, attributes(subcommand, composite, xccute_policy))]
pub fn derive_composite_subcommand(input: TokenStream) -> TokenStream {
    let input = parse_macro_input!(input as DeriveInput);
    match composite_subcommand::expand_composite_subcommand(&input) {
        Ok(ts) => ts.into(),
        Err(e) => e.to_compile_error().into(),
    }
}
