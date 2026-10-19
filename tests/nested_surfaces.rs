//! A surface inside a surface, built and run: `orb project member add NAME`,
//! the leaf's setters, exit statuses, validation and policy at every depth,
//! and every level's help lines.

#![allow(clippy::duplicated_attributes)] // one #[exit_status] per status

use std::ffi::{OsStr, OsString};

use simple_impl_derive::{CompositeShell, CompositeSubCommand, SimpleSubCommand};
use xccute_contract::{
    CommandHelp, CommandValidationErrorKind, CompositeArgvPart, CompositeShellCommand, CompositeShellRoot, HelpLine,
    RootableSubCommand, ValidatedCommand, XccutePolicyMetadata,
};

#[derive(CompositeShell, Debug, Clone, Copy, Default)]
#[shell(program = "orb")]
#[composite(command = "status", ty = Status)]
#[composite(surface = "project", ty = Project)]
struct Orb;

/// `orb status [--short]`.
#[derive(SimpleSubCommand, Debug, Clone, Default)]
#[subcommand(segment = "status")]
struct Status {
    #[arg(flag = "--short")]
    short: bool,
}

#[derive(CompositeSubCommand, Debug, Clone, Default)]
#[subcommand(segment = "project")]
#[composite(command = "create", ty = Create, init = "name")]
#[composite(surface = "member", ty = Member)]
#[xccute_policy(iam_scope = "project")]
struct Project;

/// Makes a project.
#[derive(SimpleSubCommand, Debug, Clone, Default)]
#[subcommand(segment = "create")]
#[exit_status(code = 0, word = "created")]
#[exit_status(code = 3, word = "exists")]
struct Create {
    #[builder(init_required, into)]
    #[arg(positional)]
    name: OsString,
    #[arg(flag = "--private")]
    private: bool,
}

/// The surface under `project`, with a surface of its own, written field-style.
#[derive(CompositeSubCommand, Debug, Clone, Default)]
#[subcommand(segment = "member")]
#[xccute_policy(sensitive)]
struct Member {
    #[composite(command, init = "name")]
    add: Add,
    #[composite(surface)]
    role: Role,
}

/// Adds a member to the project.
/// The role is the member's own.
#[derive(SimpleSubCommand, Debug, Clone, Default)]
#[subcommand(segment = "add")]
#[exit_status(code = 0, word = "added")]
#[exit_status(code = 4, word = "already", accepted)]
#[exit_status(code = 5, word = "refused")]
struct Add {
    #[builder(init_required, into)]
    #[arg(positional)]
    name: OsString,
    #[builder(into)]
    #[arg(kv = "--role")]
    role: Option<OsString>,
    #[arg(kv = "--tag")]
    tags: Vec<OsString>,
    #[arg(flag = "--notify")]
    #[conflicts_with("quiet")]
    notify: bool,
    #[arg(flag = "--quiet")]
    quiet: bool,
}

/// Three surfaces down: `orb project member role set NAME ROLE`.
#[derive(CompositeSubCommand, Debug, Clone, Default)]
#[subcommand(segment = "role")]
#[composite(command = "set", ty = RoleSet, init = "name,role")]
struct Role;

#[derive(SimpleSubCommand, Debug, Clone, Default)]
#[subcommand(segment = "set")]
#[exit_status(code = 0, word = "set")]
struct RoleSet {
    #[builder(init_required, into)]
    #[arg(positional)]
    name: OsString,
    #[builder(init_required, into)]
    #[arg(positional)]
    role: OsString,
    #[arg(positional, min = 1)]
    projects: Vec<OsString>,
    #[arg(flag = "--dry-run")]
    dry_run: bool,
}

#[test]
fn a_command_directly_under_a_surface_has_its_setters_and_statuses() {
    let create = Orb::project().create("cp").private();
    assert_eq!(create.build_display(), "orb project create cp --private");
    assert_eq!(create.exit_status_of(Some(3)).map(|s| s.word), Some("exists"));
    assert_eq!(create.ok_codes(), &[0]);
    // the name 0.2.6 gave it still names it
    let named: ProjectCreateCommand<Orb> = Orb::project().create("cp");
    assert_eq!(named.argv(), ["project", "create", "cp"].map(OsString::from));
    // under the root as it always was
    assert_eq!(Orb::status().short().build_display(), "orb status --short");
}

#[test]
fn a_command_two_surfaces_down_has_its_setters_statuses_rules_and_policy() {
    let add = Orb::project().member().add("ann").role("owner").tags(["a", "b"]).notify();
    assert_eq!(CompositeShellCommand::program(&add), OsStr::new("orb"));
    assert_eq!(add.build_display(), "orb project member add ann --role owner --tag a --tag b --notify");
    assert_eq!(add.ok_codes(), &[0, 4]);
    assert_eq!(add.exit_status_of(Some(4)).map(|s| (s.word, s.accepted)), Some(("already", true)));
    assert_eq!(add.exit_status_of(Some(5)).map(|s| (s.word, s.accepted)), Some(("refused", false)));
    add.validate().unwrap();

    let err = Orb::project().member().add("ann").notify().quiet().validate().unwrap_err();
    assert_eq!(err.kind(), &CommandValidationErrorKind::Structural);
    assert_eq!(err.rule(), Some("conflicts_with"));

    // the policy of each surface on the way down and the leaf's, merged
    let policy = add.xccute_policy();
    assert!(policy.is_sensitive());
    assert_eq!(policy.iam_scope(), Some("project"));
}

#[test]
fn a_command_three_surfaces_down_runs_the_same_way() {
    let set = Orb::project().member().role().set("ann", "admin").projects(["cp", "dp"]).dry_run();
    assert_eq!(set.build_display(), "orb project member role set ann admin cp dp --dry-run");
    assert_eq!(set.exit_status_of(Some(0)).map(|s| s.word), Some("set"));

    let err = Orb::project().member().role().set("ann", "admin").validate().unwrap_err();
    assert_eq!((err.field(), err.rule()), (Some("projects"), Some("min")));

    // a surface under a surface pushes every word above it
    let mut words = Vec::new();
    Orb::project().member().role().push_argv_part(&mut words);
    assert_eq!(words, ["project", "member", "role"].map(OsString::from));
}

#[test]
fn every_level_has_its_help_lines() {
    assert_eq!(
        Add::help_lines(),
        [HelpLine::new("add NAME [--role ROLE] [--tag TAGS]... [--notify] [--quiet]", "Adds a member to the project. The role is the member's own.")]
    );
    assert_eq!(RoleSet::help_lines()[0].words, "set NAME ROLE PROJECTS... [--dry-run]");
    assert_eq!(RoleSet::help_lines()[0].about, "");
    let lines: Vec<String> = Orb::help_lines().into_iter().map(|line| line.words).collect();
    assert_eq!(
        lines,
        [
            "orb status [--short]",
            "orb project create NAME [--private]",
            "orb project member add NAME [--role ROLE] [--tag TAGS]... [--notify] [--quiet]",
            "orb project member role set NAME ROLE PROJECTS... [--dry-run]",
        ]
    );
    assert_eq!(Project::help_lines()[0].words, "project create NAME [--private]");
}

// A root over an entry written by hand, with no help lines: it still builds,
// and only its help lines are missing.
#[derive(CompositeShell, Debug, Clone, Copy, Default)]
#[shell(program = "plain")]
#[composite(command = "go", ty = Go)]
struct Plain;

#[derive(Debug, Clone, Default)]
struct Go;

impl CompositeArgvPart for Go {
    fn push_argv_part(&self, out: &mut Vec<OsString>) {
        out.push("go".into());
    }
}

#[derive(Debug, Clone)]
struct GoRooted<R>(std::marker::PhantomData<R>);

impl<R: CompositeShellRoot> CompositeShellCommand for GoRooted<R> {
    fn program(&self) -> &OsStr {
        R::program()
    }

    fn push_args(&self, out: &mut Vec<OsString>) {
        Go.push_argv_part(out);
    }
}

impl<R: CompositeShellRoot> RootableSubCommand<R> for Go {
    type Rooted = GoRooted<R>;

    fn rooted(self) -> GoRooted<R> {
        GoRooted(std::marker::PhantomData)
    }
}

#[test]
fn a_root_over_a_hand_written_entry_still_builds() {
    assert_eq!(Plain::go().build_display(), "plain go");
}
