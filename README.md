# simple_impl_derive

Derives that turn a struct into a command-line builder and the command it
produces. Two families:

| family | derives | the command is | runtime crate |
|---|---|---|---|
| shell strings | `SimpleBuilder`, `SimpleShell`, `SimpleImpl`, `SimpleShellMode` | one `String`, joined by spaces | [`simple_impl`](https://crates.io/crates/simple_impl) (`ShellCommand`) |
| argv commands | `SimpleSubCommand`, `CompositeShell`, `CompositeSubCommand` | a `Vec<OsString>` under a root program, validated and previewed before it runs | [`xccute_contract`](https://crates.io/crates/xccute_contract) |

Add the runtime crate of the family you use to your dependencies; the derive
generates impls of its traits. Every example below compiles and runs as a
doctest.

## Shell strings: every key at once

```rust
use simple_impl::ShellCommand;
use simple_impl_derive::{SimpleImpl, SimpleShellMode};

#[derive(Debug, Clone, Default, PartialEq, SimpleShellMode)]
enum Color {
    #[default]
    #[shell(flag = "--color=auto")]
    Auto,
    #[shell(flag = "--color=never")]
    Never,
    Silent, // a variant without a flag contributes nothing
}

#[derive(Debug, Clone, Default, SimpleImpl)]
#[shell(cmd = "tool", require_order)]
struct Tool {
    #[builder(set_into)]
    #[shell(subcommand, order = 1)]
    action: String,

    // bool flags
    #[builder(flag)]
    #[shell(flag = "--on", order = 2)]
    on: bool,
    #[builder(flag)]
    #[shell(flag = "--check", flag_off = "--no-check", order = 3)]
    check: bool,
    #[builder(set)]
    #[shell(count_flag = "v", order = 4)]
    verbosity: u8,

    // one value, present when Some
    #[builder(opt)]
    #[shell(opt_kv = "-n", order = 5)]
    n: Option<u32>,
    #[builder(opt_into)]
    #[shell(opt_prefix = "-s:", order = 6)]
    socket: Option<String>,
    #[builder(opt_into)]
    #[shell(opt_eq = "--user", order = 7)]
    user: Option<String>,

    // one value, always present
    #[builder(set)]
    #[shell(kv = "--level", order = 8)]
    level: u8,
    #[builder(set_into)]
    #[shell(prefix = "-L", order = 9)]
    label: String,
    #[builder(set_into)]
    #[shell(eq = "--mode", order = 10)]
    mode_name: String,

    // many values
    #[builder(vec_iter)]
    #[shell(multi_opt_kv = "-e", order = 11)]
    env: Vec<String>,
    #[builder(vec_into)]
    #[shell(multi_opt_prefix = "-I", order = 12)]
    includes: Vec<String>,
    #[builder(vec_iter)]
    #[shell(multi_arg_flag = "--tags", order = 13)]
    tags: Vec<String>,

    // an enum deriving SimpleShellMode
    #[builder(set)]
    #[shell(mode, order = 14)]
    color: Color,

    // positionals
    #[builder(set_into)]
    #[shell(arg_clone, order = 15)]
    path: String,
    #[builder(set)]
    #[shell(arg_display, order = 16)]
    pid: u32,
    #[builder(opt_into)]
    #[shell(positional, order = 17)]
    maybe: Option<String>,
    #[builder(set_into)]
    #[shell(arg_join_opt = "port", sep = ":", order = 18)]
    host: String,
    #[builder(opt)]
    port: Option<u16>, // not emitted on its own; joined onto `host`
    #[builder(skip)]
    #[shell(arg_expr = "format!(\"x{}\", self.pid)", order = 19)]
    marker: (),

    // value shaping: fmt wraps each value, join collapses a Vec into one argument
    #[builder(push, method = "header")]
    #[shell(multi_opt_kv = "-H", fmt = "'{}'", order = 20)]
    headers: Vec<String>,
    #[builder(vec_iter)]
    #[shell(multi_arg_flag = "--features", join = ",", order = 21)]
    features: Vec<String>,
}

let cmd = Tool::new()
    .action("run")
    .on()
    .verbosity(3)
    .n(7)
    .socket("sock")
    .user("bob")
    .level(2)
    .label("L1")
    .mode_name("fast")
    .env(["A=1".to_string(), "B=2".to_string()])
    .includes(vec!["/x".to_string(), "/y".to_string()])
    .tags(["web".to_string(), "prod".to_string()])
    .color(Color::Never)
    .path("P")
    .pid(4)
    .maybe("M")
    .host("h")
    .port(80)
    .header("A: 1")
    .header("B: 2")
    .features(["x".to_string(), "y".to_string()]);

assert_eq!(
    cmd.build(),
    "tool run --on --no-check -vvv -n 7 -s:sock --user=bob --level 2 -LL1 --mode=fast \
     -e A=1 -e B=2 -I/x -I/y --tags web prod --color=never P 4 M h:80 x4 \
     -H 'A: 1' -H 'B: 2' --features x,y"
);

// nothing set: only what is always present
let quiet = Tool::new().action("run").path("P").pid(4).host("h").label("L1").mode_name("fast");
assert_eq!(quiet.build(), "tool run --no-check --level 0 -LL1 --mode=fast --color=auto P 4 h x4");
```

## Shell strings: a program chosen at build time

```rust
use simple_impl::ShellCommand;
use simple_impl_derive::SimpleImpl;

#[derive(Debug, Clone, Default, SimpleImpl)]
#[shell(cmd_expr = "if self.sudo { \"sudo nano\" } else { \"nano\" }")]
struct Nano {
    #[builder(flag)]
    sudo: bool,                 // no #[shell]: read by cmd_expr, never pushed
    #[builder(required, into)]
    #[shell(arg_clone)]
    file: String,
}

assert_eq!(Nano::new("notes.md").build(), "nano notes.md");
assert_eq!(Nano::new("notes.md").sudo().build(), "sudo nano notes.md");
```

## Shell strings: the builder keys

```rust
use simple_impl_derive::SimpleBuilder;

#[derive(Debug, Clone, Default, SimpleBuilder)]
struct Connect {
    #[builder(required, into)]                 // a constructor argument, `impl Into<String>`
    host: String,
    #[builder(init_required)]                  // a constructor argument, no setter
    port: Option<u16>,
    #[builder(init_only, default_expr = "3")]  // seeded by new(), no setter
    retries: u8,
    #[builder(set_into, default_expr = "String::from(\"tcp\")")]
    proto: String,
    #[builder(skip)]                           // no setter, Default value
    session: u64,
}

let c = Connect::new("db", Some(5432)).proto("udp");
assert_eq!((c.host.as_str(), c.port, c.retries, c.proto.as_str(), c.session), ("db", Some(5432), 3, "udp", 0));
```

## Argv commands: a root, leaf commands, a nested surface, preview and approval

```rust
use std::ffi::{OsStr, OsString};
use simple_impl_derive::{CompositeShell, CompositeSubCommand, SimpleSubCommand};
use xccute_contract::{
    CommandApproval, CommandPolicyError, CommandPolicyResult, CommandPreview, CommandPreviewPolicy,
    CommandValidationError, CommandValidationErrorKind, CommandValidationResult,
    CompositeShellCommand, ValidatedCommand, ValidatedCompositeShellCommand,
};

// The root program. Registry-style entries name the type of each command.
#[derive(CompositeShell, Debug, Clone, Copy, Default)]
#[shell(program = "orb")]
#[composite(command = "run", method = "run", ty = OrbRun, init = "target")]
#[composite(command = "status", ty = OrbStatus)]           // method defaults to the segment
#[composite(surface = "project", method = "project", ty = OrbProject)]
struct Orb;

#[derive(SimpleSubCommand, Debug, Clone)]
#[subcommand(segment = "run")]
#[validate(with = "validate_run_target")]                  // a preflight hook on the whole command
struct OrbRun {
    #[builder(init_required, into)]                        // forwarded by `Orb::run(target)`
    #[arg(positional)]
    target: OsString,
    #[builder(method = "profile", into)]                   // `.profile("prod")` => `--profile prod`
    #[arg(kv = "--profile")]
    profile: Option<OsString>,
    #[arg(flag = "--dry-run")]                             // `.dry_run()` => `--dry-run`
    dry_run: bool,
}

#[derive(SimpleSubCommand, Debug, Clone, Default)]
#[subcommand(segment = "status")]
struct OrbStatus {
    #[arg(flag = "--short")]
    short: bool,
}

// A nested surface: `orb project <command>`.
#[derive(CompositeSubCommand, Debug, Clone, Default)]
#[subcommand(segment = "project")]
#[composite(command = "create", ty = OrbProjectCreate, init = "name")]
#[composite(command = "set-owner", ty = OrbProjectSetOwner, init = "name,owner")]
struct OrbProject;

#[derive(SimpleSubCommand, Debug, Clone, Default)]
#[subcommand(segment = "create")]
struct OrbProjectCreate {
    #[builder(init_required, into)]
    #[arg(positional)]
    name: OsString,
}

#[derive(SimpleSubCommand, Debug, Clone, Default)]
#[subcommand(segment = "set-owner")]
struct OrbProjectSetOwner {
    #[builder(init_required, into)]
    #[arg(positional)]
    name: OsString,
    #[builder(init_required, into)]
    #[arg(positional)]
    owner: OsString,
}

fn validate_run_target(cmd: &OrbRun) -> CommandValidationResult {
    if cmd.target.is_empty() {
        return Err(CommandValidationError::runtime_preflight("run target cannot be empty")
            .with_field("target")
            .with_rule("validate_with"));
    }
    Ok(())
}

// Field-style entries: the field name is the segment and the method.
#[derive(CompositeShell, Debug, Clone, Default)]
#[shell(program = "orbctl")]
struct OrbCtl {
    #[composite(command, init = "name")]
    inspect: OrbInspect,
    #[composite(surface)]
    project: OrbProject,
}

#[derive(SimpleSubCommand, Debug, Clone, Default)]
#[subcommand(segment = "inspect")]
struct OrbInspect {
    #[builder(init_required, into)]
    #[arg(positional)]
    name: OsString,
    #[arg(flag = "--verbose")]
    verbose: bool,
}

// A leaf command under its root: setters, argv, display.
let run = Orb::run("worker-a").profile("prod").dry_run();
assert_eq!(CompositeShellCommand::program(&run), OsStr::new("orb"));
assert_eq!(run.argv(), ["run", "worker-a", "--profile", "prod", "--dry-run"].map(OsString::from));
assert_eq!(run.build_display(), "orb run worker-a --profile prod --dry-run");
assert_eq!(Orb::status().short().build_display(), "orb status --short");

// The nested surface.
assert_eq!(Orb::project().create("cp").build_display(), "orb project create cp");
assert_eq!(Orb::project().set_owner("cp", "james").build_display(), "orb project set-owner cp james");
assert_eq!(OrbCtl::inspect("node-17").verbose().build_display(), "orbctl inspect node-17 --verbose");
assert_eq!(OrbCtl::project().create("cp").build_display(), "orbctl project create cp");

// Validation runs before any preview.
let err = Orb::run("").validated_preview().unwrap_err();
assert_eq!(err.kind(), &CommandValidationErrorKind::RuntimePreflight);
assert_eq!((err.field(), err.rule()), (Some("target"), Some("validate_with")));

// A preview is data with no side effect; a policy turns it into an approval receipt.
struct DryRunsOnly;
impl CommandPreviewPolicy for DryRunsOnly {
    fn approve(&self, preview: &CommandPreview) -> CommandPolicyResult {
        if preview.argv().iter().any(|a| a == "--dry-run") {
            Ok(CommandApproval::new("dry_runs_only", "operator").with_reason("dry run"))
        } else {
            Err(CommandPolicyError::new("dry_runs_only", "only dry runs are approved"))
        }
    }
}
let receipt = run.approved_preview(&DryRunsOnly).unwrap();
assert_eq!(receipt.preview().display(), "orb run worker-a --profile prod --dry-run");
assert_eq!((receipt.approval().policy(), receipt.approval().actor()), ("dry_runs_only", "operator"));
assert!(Orb::run("worker-a").approved_preview(&DryRunsOnly).is_err());
// `receipt.to_std_command()` / `run.xccute_validated()` are the only steps that execute.
```

## Argv commands: every validation rule

```rust
use simple_impl_derive::{CompositeShell, SimpleSubCommand};
use xccute_contract::{CommandValidationErrorKind, CompositeShellCommand, ValidatedCommand};

#[derive(CompositeShell, Debug, Clone, Copy, Default)]
#[shell(program = "git")]
#[composite(command = "branch", ty = GitBranch)]
#[composite(command = "status", ty = GitStatus)]
#[composite(command = "push", ty = GitPush, init = "target")]
struct Git;

#[derive(SimpleSubCommand, Debug, Clone, Default)]
#[subcommand(segment = "branch")]
#[at_least_one_of("delete", "list")]                // the struct-level set rules
struct GitBranch {
    #[arg(flag = "-d")]
    #[invalid_without("force")]                     // `delete` is invalid unless `force` is set
    delete: bool,
    #[arg(flag = "-f")]
    #[only_pair_with("delete")]                     // `force` cannot appear alone
    force: bool,
    #[arg(flag = "--list")]
    list: bool,
    #[arg(flag = "--remote")]
    #[requires("list")]                             // `remote` needs `list`
    remote: bool,
}

#[derive(SimpleSubCommand, Debug, Clone, Default)]
#[subcommand(segment = "status")]
#[one_of("json", "yaml")]                           // exactly one
struct GitStatus {
    #[arg(flag = "--json")]
    #[conflicts_with("porcelain")]
    json: bool,
    #[arg(flag = "--yaml")]
    yaml: bool,
    #[arg(flag = "--porcelain")]
    porcelain: bool,
}

#[derive(SimpleSubCommand, Debug, Clone)]
#[subcommand(segment = "push")]
struct GitPush {
    #[builder(init_required, into)]
    #[arg(positional)]
    #[validate(with = "not_production")]            // a preflight hook on one field
    target: std::ffi::OsString,
}

fn not_production(cmd: &GitPush) -> xccute_contract::CommandValidationResult {
    if cmd.target == "production" {
        return Err(xccute_contract::CommandValidationError::custom("production needs an acknowledgment")
            .with_field("target")
            .with_rule("validate_with"));
    }
    Ok(())
}

// Struct-level rules are checked first, then field rules in field order;
// `validate()` reports the first one that fails.
fn rule_of<C: ValidatedCommand>(cmd: &C) -> (Option<String>, Option<String>) {
    let err = cmd.validate().unwrap_err();
    assert_eq!(err.kind(), &CommandValidationErrorKind::Structural);
    (err.field().map(str::to_owned), err.rule().map(str::to_owned))
}

assert_eq!(rule_of(&Git::branch().delete()), (Some("delete".into()), Some("invalid_without".into())));
assert_eq!(rule_of(&Git::branch().list().force()), (Some("force".into()), Some("only_pair_with".into())));
assert_eq!(rule_of(&Git::branch().delete().force().remote()), (Some("remote".into()), Some("requires".into())));
assert_eq!(rule_of(&Git::branch()).1, Some("at_least_one_of".into()));
assert_eq!(rule_of(&Git::status().json().porcelain()), (Some("json".into()), Some("conflicts_with".into())));
assert_eq!(rule_of(&Git::status()).1, Some("one_of".into()));
assert_eq!(rule_of(&Git::status().json().yaml()).1, Some("one_of".into()));

Git::branch().delete().force().validate().unwrap();
Git::branch().list().remote().validate().unwrap();
Git::status().yaml().validate().unwrap();
assert_eq!(Git::branch().delete().force().build_display(), "git branch -d -f");

let err = Git::push("production").validate().unwrap_err();
assert_eq!(err.kind(), &CommandValidationErrorKind::Custom);
Git::push("staging").validate().unwrap();
```

## Argv commands: policy metadata

```rust
use simple_impl_derive::{CompositeShell, SimpleSubCommand};
use xccute_contract::XccutePolicyMetadata;

#[derive(CompositeShell, Debug, Clone, Copy, Default)]
#[shell(program = "deployctl")]
#[composite(command = "rollout", ty = Rollout)]
struct DeployCtl;

#[derive(SimpleSubCommand, Debug, Clone, Default)]
#[subcommand(segment = "rollout")]
#[xccute_policy(sensitive, requires_sudo, iam_scope = "ops:deploy", path_role = "deployer", dry_run_default = false)]
struct Rollout {
    #[arg(flag = "--now")]
    now: bool,
}

let policy = DeployCtl::rollout().xccute_policy();
assert!(policy.is_sensitive());
assert!(policy.requires_sudo());
assert_eq!(policy.iam_scope(), Some("ops:deploy"));
assert_eq!(policy.path_roles(), ["deployer"]);
assert!(!policy.dry_run_default());
```

## The vocabulary

Every attribute is parsed against one schema. A misspelt key is refused at
the key and names the nearest known one; a key given twice, or given the
wrong kind of value, is refused at the key.

### `#[shell(...)]` on a struct (`SimpleShell`, `SimpleImpl`)

| key | meaning |
|---|---|
| `cmd = "name"` | the program name, the first entry of the command |
| `cmd_expr = "expr"` | the program name as an expression on `self`, instead of `cmd` |
| `trait_path = "::my::Trait"` | the trait to implement; default `::simple_impl::ShellCommand` |
| `require_order` | every emitted field must carry an `order` |

### `#[shell(...)]` on a field

| key | meaning |
|---|---|
| `flag = "--x"` | pushed when the bool is true |
| `flag_off = "--no-x"` | pushed when the bool is false |
| `count_flag = "v"` | a short flag repeated per unit of an unsigned field: 3 gives `-vvv` |
| `opt_kv = "-n"` | `-n value` for an `Option` that is `Some` |
| `opt_prefix = "-s:"` | `-s:value` as one argument for an `Option` that is `Some` |
| `opt_eq = "--user"` | `--user=value` as one argument for an `Option` that is `Some` |
| `kv`, `prefix`, `eq` | the same three shapes for a field that is always present |
| `multi_opt_kv = "-e"` | `-e value` repeated for every element of a `Vec` |
| `multi_opt_prefix = "-I"` | `-Ivalue` repeated for every element of a `Vec` |
| `multi_arg_flag = "--tags"` | the flag once, then every element of a `Vec` |
| `positional`, `arg` | the field's value as a bare argument; an `Option` only when `Some`, a `Vec` as one argument per element |
| `arg_clone`, `arg_display` | positional, pushed by clone or by `to_string` |
| `arg_expr = "expr"` | positional, pushed as the expression's `to_string` |
| `opt_expr = "expr"` | positional, pushed as `v.to_string()` when the expression is `Some(v)` |
| `arg_join_opt = "other"`, `sep = ":"` | positional joined with the named `Option` field when it is `Some` |
| `fmt = "'{}'"` | a format string with one `{}` applied to each value before it is pushed; for the kv, multi and positional kinds |
| `join = ","` | collapse a `Vec` into one argument: `multi_arg_flag = "--features", join = ","` gives `--features a,b` |
| `mode` | the field is an enum deriving `SimpleShellMode` |
| `subcommand` | the field is the subcommand word |
| `order = 3` / `"first"` / `"last"` / `"before:field"` / `"after:field"` | where the field lands in the command |

### `#[shell(flag = "...")]` on a variant (`SimpleShellMode`)

The flag pushed when that variant is selected. A variant without one pushes nothing.

### `#[builder(...)]` on a field (`SimpleBuilder`, `SimpleImpl`)

| key | meaning |
|---|---|
| `required`, `not_in_default` | a constructor argument rather than a defaulted field |
| `into` | the setter or constructor argument takes `impl Into<T>` |
| `skip` | no setter |
| `init_only` | set by the constructor or `default_expr`, no setter |
| `init_required` | a constructor argument with no setter |
| `default_expr = "expr"` | the expression `new()` initialises the field with |
| `method = "name"` | the setter's name, when not the field's |
| `flag`, `opt`, `opt_into`, `set`, `set_into`, `vec_into`, `vec_iter` | the setter kind |
| `push` | the setter kind that appends one element to a `Vec<T>`, taking `impl Into<T>` |

### `SimpleSubCommand`

| attribute | meaning |
|---|---|
| `#[subcommand(segment = "add")]` | the subcommand word; required |
| `#[arg(flag = "--x")]` | a bool pushed as the flag |
| `#[arg(kv = "--x")]` | `--x value`, an `Option` only when `Some` |
| `#[arg(positional)]` | the value as a bare argument |
| `#[builder(method = "name", into, init_required)]` | the setter's name; `impl Into<_>`; a `new()` argument with no setter |
| `#[requires("f")]`, `#[invalid_without("f")]`, `#[only_pair_with("f")]`, `#[conflicts_with("f")]` | field rules, checked by `validate()` |
| `#[one_of("a", "b")]`, `#[at_least_one_of("a", "b")]` | struct rules over a set of fields |
| `#[validate(with = "path::to::fn")]` | a preflight hook `fn(&Self) -> CommandValidationResult`, on the struct or a field |
| `#[xccute_policy(sensitive, requires_sudo, iam_scope = "..", path_role = "..", dry_run_default = false)]` | metadata read back through `XccutePolicyMetadata` |

One of `flag`, `kv`, `positional` per field.

### `CompositeShell` and `CompositeSubCommand`

| attribute | meaning |
|---|---|
| `#[shell(program = "orb")]` | the root program (`CompositeShell`) |
| `#[subcommand(segment = "project")]` | the surface's segment (`CompositeSubCommand`) |
| `#[composite(command = "run", ty = Run, method = "run", init = "a,b")]` | on the struct: a command entry; `method` defaults to the segment with `-` as `_`; `init` names the `new()` arguments forwarded |
| `#[composite(surface = "project", ty = Project)]` | on the struct: a nested surface entry |
| `#[composite(command)]`, `#[composite(surface)]` on a field | the field's name is the segment and the method; `command = "seg"` and `method`, `init` still apply |

A nested surface's commands take their `init` arguments; they do not yet
re-expose the leaf's optional setters, and a surface inside a surface is
refused.

## Errors

Unknown, repeated and wrong-kind keys are refused at the key. The rules
between keys carry stable codes: `SHELL(1002)` for `sep` without
`arg_join_opt`, `SHELL(1006)` for a bad `order` value, order targets that do
not exist or form a cycle, one emission kind per field, and `BUILDER(3002)`
/ `BUILDER(3003)` for setter kinds that contradict `init_required` or
`default_expr`.

## License

MIT OR Apache-2.0.
