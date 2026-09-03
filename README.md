# simple_impl_derive

Derives that turn a struct into a command-line builder and the argument
vector it produces.

```rust
use simple_impl_derive::SimpleImpl;
use xccute::ShellCommand;

#[derive(Debug, Clone, Default, SimpleImpl)]
#[shell(cmd = "kill", trait_path = "::xccute::ShellCommand")]
struct Kill {
    #[builder(opt_into)]
    #[shell(opt_prefix = "-")]
    signal: Option<String>,

    #[builder(vec_iter)]
    #[shell(arg_display)]
    pids: Vec<u32>,

    #[shell(flag = "--verbose")]
    verbose: bool,
}

let cmd = Kill::new().signal("TERM").pids([12, 34]).verbose();
assert_eq!(cmd.build(), "kill --verbose -TERM 12 34");
```

| derive | generates |
|---|---|
| `SimpleBuilder` | `new()` and a setter per field, shaped by `#[builder(...)]` |
| `SimpleShell` | a `ShellCommand` impl whose `build()` assembles the argument vector from `#[shell(...)]` |
| `SimpleImpl` | both |
| `SimpleShellMode` | an enum whose selected variant contributes one flag |
| `SimpleSubCommand` | a subcommand with `#[arg(...)]` fields and validation rules |
| `CompositeShell`, `CompositeSubCommand` | a command made of subcommands, field or registry style |

## `#[shell(...)]` on the struct

| key | meaning |
|---|---|
| `cmd = "name"` | the program name, the first entry of the argument vector |
| `trait_path = "::xccute::ShellCommand"` | the trait to implement |
| `require_order` | every emitted field must carry an `order` |

## `#[shell(...)]` on a field

| key | meaning |
|---|---|
| `flag = "--x"` | pushed when the bool is true |
| `flag_off = "--no-x"` | pushed when the bool is false |
| `count_flag = "v"` | a short flag repeated per unit of an unsigned field: 3 gives `-vvv` |
| `opt_kv = "-n"` | `-n value` for an `Option` that is `Some` |
| `opt_prefix = "-s:"` | `-s:value` as one argument for an `Option` that is `Some` |
| `opt_eq = "--user"` | `--user=value` as one argument for an `Option` that is `Some` |
| `kv`, `prefix`, `eq` | the same three for a plain field |
| `multi_opt_kv = "-e"` | `-e value` repeated for every element of a `Vec` |
| `multi_opt_prefix = "-I"` | `-Ivalue` repeated for every element of a `Vec` |
| `multi_arg_flag = "--tags"` | the flag once, then every element of a `Vec` |
| `positional`, `arg` | the field's value as a bare argument |
| `arg_clone`, `arg_display` | positional, pushed by clone or by `to_string` |
| `arg_expr = "format!(..)"` | positional, pushed as the expression's `to_string` |
| `arg_join_opt = "other"`, `sep = ":"` | positional joined with the named `Option` field when it is `Some` |
| `mode` | the field is an enum deriving `SimpleShellMode` |
| `subcommand` | the field is the subcommand word |
| `order = 3` / `"first"` / `"last"` / `"before:field"` / `"after:field"` | where the field lands in the vector |

## `#[builder(...)]` on a field

| key | meaning |
|---|---|
| `required`, `not_in_default` | a constructor argument rather than a defaulted field |
| `into` | the setter (or constructor argument) takes `impl Into<T>` |
| `skip` | no setter |
| `init_only` | set by the constructor or `default_expr`, no setter |
| `init_required` | a constructor argument with no setter |
| `default_expr = "expr"` | the expression `new()` initialises the field with |
| `flag`, `opt`, `opt_into`, `set`, `set_into`, `vec_into`, `vec_iter`, `push` | the setter kind, when not inferred from the type |

## Subcommands

`#[subcommand(segment = "add")]` names the subcommand; on its fields
`#[arg(flag = "..")]`, `#[arg(kv = "..")]` or `#[arg(positional)]` (one of
them), `#[builder(method = "..", into, init_required)]`, and the validation
attributes `#[requires("..")]`, `#[invalid_without("..")]`,
`#[only_pair_with("..")]`, `#[conflicts_with("..")]`, `#[one_of(..)]`,
`#[at_least_one_of(..)]`, `#[validate(with = "..")]`.

## Errors

Every attribute goes through one schema. A misspelt key is refused at the
key and names the nearest known one; a key given twice, or given the wrong
kind of value, is refused at the key; the rules between keys (`sep` needs
`arg_join_opt`, one emission kind per field, `order` targets must exist,
no cycles) carry stable `SHELL(nnnn)` and `BUILDER(nnnn)` codes.

## License

MIT OR Apache-2.0.
