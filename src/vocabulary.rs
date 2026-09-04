//! Every attribute the derives accept, in one place.
//!
//! Each function is the schema one parser validates against, so a key's
//! kind, whether it is required, what it conflicts with, and its one-line
//! description live together. `describe()` on any of them prints the
//! vocabulary; the tests below keep every key documented.

use simple_impl_attr_kit::{AttrExpected as K, AttrSchema};

/// `#[shell(...)]` on the struct.
pub(crate) fn shell_struct() -> AttrSchema {
    AttrSchema::new()
        .optional("cmd", K::String)
        .doc("cmd", "the program name, the first entry of the argument vector")
        .optional("trait_path", K::String)
        .doc("trait_path", "path of the ShellCommand trait to implement, as a string")
        .optional("require_order", K::Bool)
        .doc("require_order", "every emitted field must carry an `order`")
        .optional("cmd_expr", K::String)
        .doc("cmd_expr", "the program name as an expression on `self`, instead of `cmd`: cmd_expr = \"if self.sudo { \\\"sudo nano\\\" } else { \\\"nano\\\" }\"")
        .conflicts("cmd", "cmd_expr")
}

/// `#[shell(...)]` on a field.
pub(crate) fn shell_field() -> AttrSchema {
    AttrSchema::new()
        // one flag for a bool
        .optional("flag", K::String)
        .doc("flag", "pushed when the bool field is true")
        .optional("flag_off", K::String)
        .doc("flag_off", "pushed when the bool field is false")
        .optional("count_flag", K::String)
        .doc("count_flag", "a short flag repeated per unit of an unsigned field: 3 gives -vvv")
        // one value
        .optional("opt_kv", K::String)
        .doc("opt_kv", "`flag value` for an Option field that is Some")
        .optional("opt_prefix", K::String)
        .doc("opt_prefix", "`prefixvalue` as one argument for an Option field that is Some")
        .optional("opt_eq", K::String)
        .doc("opt_eq", "`flag=value` as one argument for an Option field that is Some")
        .optional("kv", K::String)
        .doc("kv", "`flag value` for a plain field")
        .optional("prefix", K::String)
        .doc("prefix", "`prefixvalue` as one argument for a plain field")
        .optional("eq", K::String)
        .doc("eq", "`flag=value` as one argument for a plain field")
        // many values
        .optional("multi_opt_kv", K::String)
        .doc("multi_opt_kv", "`flag value` repeated for every element of a Vec field")
        .optional("multi_opt_prefix", K::String)
        .doc("multi_opt_prefix", "`prefixvalue` repeated for every element of a Vec field")
        .optional("multi_arg_flag", K::String)
        .doc("multi_arg_flag", "the flag once, then every element of a Vec field")
        // positionals
        .optional("positional", K::Bool)
        .doc("positional", "the field's value as a bare argument")
        .optional("arg", K::Bool)
        .doc("arg", "same as `positional`")
        .optional("arg_clone", K::Bool)
        .doc("arg_clone", "positional, pushed by clone (String fields)")
        .optional("arg_display", K::Bool)
        .doc("arg_display", "positional, pushed by to_string")
        .optional("arg_expr", K::String)
        .doc("arg_expr", "positional, pushed as the given expression's to_string")
        .optional("opt_expr", K::String)
        .doc("opt_expr", "positional, pushed as v.to_string() when the given expression is Some(v)")
        .optional("arg_join_opt", K::String)
        .doc("arg_join_opt", "positional joined with the named Option field when it is Some")
        .optional("sep", K::String)
        .doc("sep", "the separator for `arg_join_opt`; the derive checks the pairing (SHELL(1002))")
        // value shaping
        .optional("fmt", K::String)
        .doc("fmt", "a format string with one `{}`, applied to each value before it is pushed: fmt = \"'{}'\" gives -H 'value'")
        .optional("join", K::String)
        .doc("join", "join a Vec's elements with this separator into one argument: multi_arg_flag = \"--features\", join = \",\" gives --features a,b")
        // shape
        .optional("mode", K::Bool)
        .doc("mode", "the field is an enum deriving SimpleShellMode; its variant's flag is pushed")
        .optional("subcommand", K::Bool)
        .doc("subcommand", "the field is the subcommand word")
        .optional("order", K::Any)
        .doc("order", "an integer, or \"first\", \"last\", \"before:FIELD\", \"after:FIELD\"")
}

/// `#[builder(...)]` on a field of a SimpleBuilder / SimpleImpl struct.
pub(crate) fn builder_field() -> AttrSchema {
    AttrSchema::new()
        .optional("required", K::Bool)
        .doc("required", "a constructor argument rather than a defaulted field")
        .optional("not_in_default", K::Bool)
        .doc("not_in_default", "same as `required`")
        .optional("into", K::Bool)
        .doc("into", "the setter takes `impl Into<T>`")
        .optional("skip", K::Bool)
        .doc("skip", "no setter")
        .optional("init_only", K::Bool)
        .doc("init_only", "set by the constructor or `default_expr`, no setter")
        .optional("init_required", K::Bool)
        .doc("init_required", "a constructor argument with no setter")
        .optional("default_expr", K::String)
        .doc("default_expr", "the expression `new()` initialises the field with")
        .optional("method", K::String)
        .doc("method", "the setter's name, when not the field's")
        .optional("flag", K::Bool)
        .doc("flag", "setter kind: `fn field(self) -> Self` setting a bool")
        .optional("opt", K::Bool)
        .doc("opt", "setter kind: `fn field(self, T)` for Option<T>")
        .optional("opt_into", K::Bool)
        .doc("opt_into", "setter kind: `fn field(self, impl Into<T>)` for Option<T>")
        .optional("set", K::Bool)
        .doc("set", "setter kind: `fn field(self, T)`")
        .optional("set_into", K::Bool)
        .doc("set_into", "setter kind: `fn field(self, impl Into<T>)`")
        .optional("vec_into", K::Bool)
        .doc("vec_into", "setter kind: `fn field(self, impl Into<Vec<T>>)`")
        .optional("vec_iter", K::Bool)
        .doc("vec_iter", "setter kind: `fn field(self, impl IntoIterator<Item = T>)`")
        .optional("push", K::Bool)
        .doc("push", "setter kind: `fn field(self, impl Into<T>)` pushing one element onto a Vec<T>; the derive checks kinds against `init_required` and `default_expr` (BUILDER(3002), BUILDER(3003))")
}

/// The setter kinds of [`builder_field`], in the order the last one written wins.
pub(crate) const BUILDER_KINDS: [&str; 8] = ["flag", "opt", "opt_into", "set", "set_into", "vec_into", "vec_iter", "push"];

/// `#[shell(...)]` on a variant of a SimpleShellMode enum.
pub(crate) fn shell_variant() -> AttrSchema {
    AttrSchema::new().optional("flag", K::String).doc("flag", "pushed when this variant is selected")
}

/// `#[subcommand(...)]` on a SimpleSubCommand or CompositeSubCommand struct.
pub(crate) fn subcommand() -> AttrSchema {
    AttrSchema::new().required("segment", K::String).doc("segment", "the subcommand word")
}

/// `#[builder(...)]` on a field of a SimpleSubCommand struct.
pub(crate) fn subcommand_builder() -> AttrSchema {
    AttrSchema::new()
        .optional("method", K::String)
        .doc("method", "the setter's name, when not the field's")
        .optional("into", K::Bool)
        .doc("into", "the setter takes `impl Into<T>`")
        .optional("init_required", K::Bool)
        .doc("init_required", "a constructor argument with no setter")
}

/// `#[arg(...)]` on a field of a SimpleSubCommand struct.
pub(crate) fn subcommand_arg() -> AttrSchema {
    AttrSchema::new()
        .optional("flag", K::String)
        .doc("flag", "pushed when the bool field is true")
        .optional("kv", K::String)
        .doc("kv", "`flag value` for the field")
        .optional("positional", K::Bool)
        .doc("positional", "the field's value as a bare argument")
        .conflicts("flag", "kv")
        .conflicts("flag", "positional")
        .conflicts("kv", "positional")
}

/// `#[xccute_policy(...)]` on a SimpleSubCommand or CompositeSubCommand struct.
pub(crate) fn xccute_policy() -> AttrSchema {
    AttrSchema::new()
        .optional("sensitive", K::Bool)
        .doc("sensitive", "the command touches something an operator should look at twice")
        .optional("requires_sudo", K::Bool)
        .doc("requires_sudo", "the command needs elevated rights")
        .optional("iam_scope", K::String)
        .doc("iam_scope", "the IAM scope the command runs under")
        .optional("path_role", K::String)
        .doc("path_role", "the path role the command claims")
        .optional("dry_run_default", K::Bool)
        .doc("dry_run_default", "whether a dry run is the default (true when absent)")
}

#[cfg(test)]
mod tests {
    use super::*;

    fn every_key_documented(schema: &AttrSchema) {
        let text = schema.describe();
        for key in schema.keys() {
            let line = text.lines().find(|l| l.starts_with(&format!("{key}: "))).unwrap_or_else(|| panic!("{key} missing from describe()"));
            assert!(line.contains(": "), "{key} has no doc");
            assert!(line.rsplit(": ").next().map(|d| !d.is_empty()).unwrap_or(false), "{key} has an empty doc");
        }
    }

    #[test]
    fn every_schema_documents_every_key() {
        for schema in [shell_struct(), shell_field(), builder_field(), shell_variant(), subcommand(), subcommand_builder(), subcommand_arg(), xccute_policy()] {
            every_key_documented(&schema);
        }
    }

    #[test]
    fn the_vocabulary_has_the_keys_the_derives_always_had() {
        let shell_schema = shell_field();
        let shell: Vec<&str> = shell_schema.keys().map(String::as_str).collect();
        for key in ["flag", "flag_off", "opt_kv", "opt_prefix", "opt_eq", "kv", "prefix", "eq", "sep", "multi_opt_kv", "multi_opt_prefix", "multi_arg_flag", "count_flag", "arg_expr", "arg_join_opt", "positional", "arg", "arg_clone", "arg_display", "mode", "subcommand", "order"] {
            assert!(shell.contains(&key), "shell field key {key} missing");
        }
        let builder_schema = builder_field();
        let builder: Vec<&str> = builder_schema.keys().map(String::as_str).collect();
        for key in BUILDER_KINDS.iter().copied().chain(["required", "not_in_default", "into", "skip", "init_only", "init_required", "default_expr"]) {
            assert!(builder.contains(&key), "builder key {key} missing");
        }
    }
}
