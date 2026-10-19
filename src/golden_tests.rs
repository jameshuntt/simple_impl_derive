//! Golden expansions: what the derives write for a declaration, kept in
//! `src/golden/` and compared token for token. `SIMPLE_IMPL_BLESS=1 cargo test`
//! writes them again after a deliberate change.

use syn::{parse_quote, DeriveInput};

fn pretty(tokens: proc_macro2::TokenStream) -> String {
    prettyplease::unparse(&syn::parse2(tokens).expect("the expansion is a file"))
}

fn golden(name: &str, got: &str) {
    let path = format!("{}/src/golden/{name}", env!("CARGO_MANIFEST_DIR"));
    if std::env::var_os("SIMPLE_IMPL_BLESS").is_some() {
        std::fs::write(&path, got).unwrap();
        return;
    }
    let want = std::fs::read_to_string(&path).unwrap_or_else(|e| panic!("{path}: {e}"));
    assert!(want == got, "{name} differs from its golden; SIMPLE_IMPL_BLESS=1 writes it again\n{got}");
}

/// `orb project member role set NAME ROLE [--until UNTIL]`: a root, three surfaces
/// one inside the other, and a leaf under the first and under the last.
#[test]
fn three_level_surface() {
    let root: DeriveInput = parse_quote! {
        #[shell(program = "orb")]
        #[composite(surface = "project", ty = Project)]
        pub struct Orb;
    };
    let project: DeriveInput = parse_quote! {
        #[subcommand(segment = "project")]
        #[composite(command = "create", ty = Create, init = "name")]
        #[composite(surface = "member", ty = Member)]
        pub struct Project;
    };
    let member: DeriveInput = parse_quote! {
        #[subcommand(segment = "member")]
        #[xccute_policy(sensitive)]
        pub struct Member {
            #[composite(surface)]
            role: Role,
        }
    };
    let role: DeriveInput = parse_quote! {
        #[subcommand(segment = "role")]
        #[composite(command = "set", ty = RoleSet, init = "name,role")]
        pub struct Role;
    };
    let create: DeriveInput = parse_quote! {
        /// Makes a project.
        #[subcommand(segment = "create")]
        #[exit_status(code = 3, word = "exists")]
        pub struct Create {
            #[builder(init_required, into)]
            #[arg(positional)]
            name: OsString,
            #[arg(flag = "--private")]
            private: bool,
        }
    };
    let role_set: DeriveInput = parse_quote! {
        #[subcommand(segment = "set")]
        pub struct RoleSet {
            #[builder(init_required, into)]
            #[arg(positional)]
            name: OsString,
            #[builder(init_required, into)]
            #[arg(positional)]
            role: OsString,
            #[builder(into)]
            #[arg(kv = "--until")]
            until: Option<OsString>,
        }
    };
    let mut got = String::new();
    for (title, tokens) in [
        ("CompositeShell Orb", crate::composite_shell::expand_composite_shell(&root)),
        ("CompositeSubCommand Project", crate::composite_subcommand::expand_composite_subcommand(&project)),
        ("CompositeSubCommand Member", crate::composite_subcommand::expand_composite_subcommand(&member)),
        ("CompositeSubCommand Role", crate::composite_subcommand::expand_composite_subcommand(&role)),
        ("SimpleSubCommand Create", crate::simple_subcommand::expand_simple_subcommand(&create)),
        ("SimpleSubCommand RoleSet", crate::simple_subcommand::expand_simple_subcommand(&role_set)),
    ] {
        got.push_str(&format!("// ---- {title}\n{}\n", pretty(tokens.expect(title))));
    }
    golden("three_level_surface.rs.expanded", &got);
}
