//! One compile reports every problem: each test feeds a broken struct to the
//! derive and reads back every error the buffer collected, in order.

use super::*;
use syn::parse_quote;

fn errors(input: DeriveInput) -> Vec<String> {
    match expand(&input, ExpandMode::Both) {
        Ok(_) => Vec::new(),
        Err(e) => e.into_iter().map(|e| e.to_string()).collect(),
    }
}

fn codes(input: DeriveInput) -> Vec<String> {
    errors(input)
        .iter()
        .map(|m| m[1..m.find(']').unwrap()].to_string())
        .collect()
}

#[test]
fn every_broken_field_is_reported_in_one_compile() {
    let input: DeriveInput = parse_quote! {
        #[shell(cmd = "ls")]
        struct Ls {
            #[shell(flag = "-l")]
            long: String,
            #[shell(count_flag = "v")]
            verbose: String,
            #[shell(multi_arg_flag = "-I")]
            ignore: String,
            #[shell(opt_kv = "--color")]
            color: String,
        }
    };
    assert_eq!(codes(input), ["SHELL1013", "SHELL1014", "SHELL1017", "SHELL1010"]);
}

#[test]
fn every_parse_problem_is_reported_in_one_compile() {
    let input: DeriveInput = parse_quote! {
        #[shell(cmd = "x")]
        struct X {
            #[builder(init_required, default_expr = "1")]
            a: u8,
            #[builder(push)]
            b: String,
            #[builder(init_only, flag)]
            c: bool,
            #[shell(flag = "-d", order = "before:")]
            d: bool,
        }
    };
    assert_eq!(codes(input), ["SHELL3002", "SHELL3004", "SHELL3003", "SHELL2004"]);
}

#[test]
fn missing_cmd_and_broken_fields_come_back_together() {
    let input: DeriveInput = parse_quote! {
        struct NoCmd {
            #[shell(flag = "-x")]
            c: u8,
            #[shell(opt_eq = "--y")]
            y: String,
        }
    };
    assert_eq!(codes(input), ["SHELL0001", "SHELL1013", "SHELL1012"]);
}

#[test]
fn parse_and_shell_problems_come_back_in_one_compile() {
    // `a` fails at parse, `b` in the shell stage, `c` in ordering: all three show.
    let input: DeriveInput = parse_quote! {
        #[shell(cmd = "x")]
        struct X {
            #[builder(push)]
            a: String,
            #[shell(flag = "-b")]
            b: u8,
            #[shell(flag = "-c", order = "before:nobody")]
            c: bool,
        }
    };
    assert_eq!(codes(input), ["SHELL3004", "SHELL1013", "SHELL2001"]);
}

#[test]
fn a_field_that_failed_at_parse_is_not_echoed_later() {
    // `a`'s #[shell] is unreadable; `b` orders after it and `c` joins it.
    // Only `a`'s own error shows: no "unknown target `a`", no "unknown field".
    let input: DeriveInput = parse_quote! {
        #[shell(cmd = "x")]
        struct X {
            #[shell(flag = "-a", bogus)]
            a: bool,
            #[shell(flag = "-b", order = "after:a")]
            b: bool,
            #[shell(arg_join_opt = "a")]
            c: String,
        }
    };
    let errs = errors(input);
    assert_eq!(errs.len(), 1, "{errs:#?}");
    assert!(errs[0].starts_with("unknown attribute key `bogus`"));
}

#[test]
fn a_bad_builder_still_lets_the_shell_check_that_field() {
    // `b` is both a wrong builder kind and a wrong shell type: both show.
    let input: DeriveInput = parse_quote! {
        #[shell(cmd = "x")]
        struct X {
            #[builder(push)]
            #[shell(flag = "-b")]
            b: String,
        }
    };
    assert_eq!(codes(input), ["SHELL3004", "SHELL1013"]);
}

#[test]
fn one_field_can_carry_several_problems() {
    let input: DeriveInput = parse_quote! {
        #[shell(cmd = "x", require_order)]
        struct X {
            #[shell(kv = "-a", fmt = "{}{}", sep = ",")]
            a: String,
        }
    };
    assert_eq!(codes(input), ["SHELL1021", "SHELL1002", "SHELL1003"]);
}

#[test]
fn ordering_reports_every_bad_target() {
    let input: DeriveInput = parse_quote! {
        #[shell(cmd = "x")]
        struct X {
            #[shell(flag = "-a", order = "before:nope")]
            a: bool,
            #[shell(flag = "-b", order = "after:gone")]
            b: bool,
        }
    };
    assert_eq!(
        errors(input),
        [
            "[SHELL2001] order target references unknown (or non-emitted) field: `nope`",
            "[SHELL2001] order target references unknown (or non-emitted) field: `gone`",
        ]
    );
}

#[test]
fn a_failed_field_does_not_echo_into_ordering() {
    // `a` is broken; `b` orders itself after `a`. Only `a`'s own error shows,
    // not a second "target `a` does not exist" caused by the first.
    let input: DeriveInput = parse_quote! {
        #[shell(cmd = "x")]
        struct X {
            #[shell(flag = "-a")]
            a: String,
            #[shell(flag = "-b", order = "after:a")]
            b: bool,
        }
    };
    assert_eq!(codes(input), ["SHELL1013"]);
}

#[test]
fn second_subcommand_is_coded_and_the_rest_still_checked() {
    let input: DeriveInput = parse_quote! {
        #[shell(cmd = "git")]
        struct Git {
            #[shell(subcommand)]
            one: String,
            #[shell(subcommand)]
            two: String,
            #[shell(flag = "-q")]
            quiet: u8,
        }
    };
    assert_eq!(codes(input), ["SHELL1018", "SHELL1013"]);
}

#[test]
fn not_a_struct_stops_at_once() {
    let input: DeriveInput = parse_quote! { enum E { A } };
    assert_eq!(codes(input), ["SHELL0002"]);
}

#[test]
fn a_clean_struct_still_builds() {
    let input: DeriveInput = parse_quote! {
        #[shell(cmd = "ls")]
        struct Ls {
            #[shell(flag = "-l")]
            long: bool,
            #[shell(opt_kv = "--color")]
            color: Option<String>,
            #[shell(positional)]
            path: String,
        }
    };
    let out = expand(&input, ExpandMode::Both).unwrap().to_string();
    assert!(out.contains("fn build"));
    assert!(out.contains("\"-l\""));
}

#[test]
fn a_broken_struct_attribute_does_not_add_missing_cmd() {
    // the struct does give cmd; its #[shell] fails on another key, and the
    // field's own problem still shows
    let input: DeriveInput = parse_quote! {
        #[shell(cmd = "ls", requre_order)]
        struct Ls {
            #[shell(flag = "-l")]
            long: String,
        }
    };
    let errs = errors(input);
    assert_eq!(errs.len(), 2, "{errs:#?}");
    assert!(errs[0].starts_with("unknown attribute key `requre_order`"));
    assert!(errs[1].starts_with("[SHELL1013]"));
}

#[test]
fn builder_kinds_and_method_names_are_checked_not_panicked_on() {
    let input: DeriveInput = parse_quote! {
        #[shell(cmd = "x")]
        struct X {
            #[builder(opt)]
            a: u32,
            #[builder(vec_into)]
            b: Option<u8>,
            #[builder(method = "set name")]
            c: u8,
            #[builder(push)]
            d: String,
        }
    };
    assert_eq!(codes(input), ["SHELL3005", "SHELL3006", "SHELL3007", "SHELL3004"]);
}

#[test]
fn a_rule_naming_a_failed_field_still_checks_its_other_names() {
    // `b` fails to parse; the rule also names `nobody` and `9x`: both are
    // reported, and `b` is not reported a second time
    let input: DeriveInput = parse_quote! {
        #[subcommand(segment = "s")]
        #[one_of("b", "nobody", "9x")]
        pub struct S {
            #[arg(flag = "-a")]
            a: bool,
            #[arg(flag = 1)]
            b: bool,
        }
    };
    let errs: Vec<String> = match crate::simple_subcommand::expand_simple_subcommand(&input) {
        Ok(_) => Vec::new(),
        Err(e) => e.into_iter().map(|e| e.to_string()).collect(),
    };
    assert_eq!(errs.len(), 3, "{errs:#?}");
    assert!(errs[0].starts_with("expected a string for `flag`"), "{errs:#?}");
    assert!(errs[1].starts_with("[SHELL5005] validation rule field must be a Rust field identifier: `9x`"), "{errs:#?}");
    assert_eq!(errs[2], "validation rule references unknown field `nobody`: in #[one_of(\"b\", \"nobody\", \"9x\")]");
}

fn sub_errors(input: DeriveInput) -> Vec<String> {
    match crate::simple_subcommand::expand_simple_subcommand(&input) {
        Ok(_) => Vec::new(),
        Err(e) => e.into_iter().map(|e| e.to_string()).collect(),
    }
}

#[test]
fn a_field_without_a_setter_is_not_held_to_setter_rules() {
    // compiled before this release; must still compile
    let input: DeriveInput = parse_quote! {
        #[shell(cmd = "x")]
        struct X {
            #[builder(skip, opt)]
            a: u32,
            #[builder(skip, vec_into)]
            b: u32,
            #[builder(skip, method = "not an ident")]
            #[shell(flag = "-c")]
            c: bool,
            #[builder(init_only, method = "two words")]
            e: u32,
        }
    };
    assert_eq!(errors(input), Vec::<String>::new());
}

#[test]
fn a_method_name_with_anything_around_it_is_refused() {
    for name in [" name", "name\n", "\tname", "name /* why */", "foo // trailing", "set name", ""] {
        let input: DeriveInput = syn::parse_str(&format!(
            "#[shell(cmd = \"x\")] struct X {{ #[builder(method = {name:?})] a: u32, #[shell(flag = \"-b\")] b: u8 }}"
        ))
        .unwrap();
        assert_eq!(codes(input), ["SHELL3007", "SHELL1013"], "method = {name:?}");
    }
    for name in ["name", "r#type", "union", "_x"] {
        let input: DeriveInput = syn::parse_str(&format!("#[shell(cmd = \"x\")] struct X {{ #[builder(method = {name:?})] a: u32 }}")).unwrap();
        assert_eq!(codes(input), Vec::<String>::new(), "method = {name:?}");
    }
}

#[test]
fn a_bad_order_value_still_lets_the_field_be_checked() {
    let input: DeriveInput = parse_quote! {
        #[shell(cmd = "x", require_order)]
        struct X {
            #[shell(flag = "-x", order = "sideways")]
            x: String,
        }
    };
    assert_eq!(codes(input), ["SHELL1006", "SHELL1013"]);
}

#[test]
fn an_unreadable_builder_does_not_hide_the_fields_shell_problems() {
    let input: DeriveInput = parse_quote! {
        #[shell(cmd = "x")]
        struct X {
            #[builder(bogus)]
            #[shell(flag = "-x")]
            x: String,
        }
    };
    let errs = errors(input);
    assert_eq!(errs.len(), 2, "{errs:#?}");
    assert!(errs[0].starts_with("unknown attribute key `bogus`"));
    assert!(errs[1].starts_with("[SHELL1013]"));
}

#[test]
fn require_order_holds_when_trait_path_fails() {
    let input: DeriveInput = parse_quote! {
        #[shell(cmd = "x", require_order, trait_path = "not a path")]
        struct X {
            #[shell(flag = "-a")]
            a: bool,
        }
    };
    let errs = errors(input);
    assert_eq!(errs.len(), 2, "{errs:#?}");
    assert!(errs[0].starts_with("trait_path is not a path"));
    assert!(errs[1].starts_with("[SHELL1003]"));
}

#[test]
fn a_cycle_through_a_broken_field_is_reported_now() {
    let input: DeriveInput = parse_quote! {
        #[shell(cmd = "x")]
        struct X {
            #[shell(flag = "-a", order = "before:b")]
            a: bool,
            #[shell(flag = "-b", order = "before:a")]
            b: String,
        }
    };
    assert_eq!(codes(input), ["SHELL1013", "SHELL2003"]);
}

#[test]
fn an_empty_rule_list_is_kept_as_written() {
    let input: DeriveInput = parse_quote! {
        #[subcommand(segment = "s")]
        #[one_of()]
        pub struct S {
            #[arg(flag = "-a")]
            a: bool,
        }
    };
    let out = crate::simple_subcommand::expand_simple_subcommand(&input).unwrap().to_string();
    assert!(out.contains("exactly one of"), "{out}");
}

#[test]
fn subcommand_method_names_rules_on_failed_fields_and_function_paths() {
    let input: DeriveInput = parse_quote! {
        #[subcommand(segment = "s")]
        #[validate(with = "not a path")]
        pub struct S {
            #[arg(flag = "-a")]
            #[builder(method = "set a")]
            a: bool,
            #[arg(flag = 1)]
            #[requires("nobody")]
            b: bool,
        }
    };
    let errs = sub_errors(input);
    assert_eq!(errs.len(), 4, "{errs:#?}");
    assert!(errs[0].starts_with("[SHELL3007]"), "{errs:#?}");
    assert!(errs[1].starts_with("expected a string for `flag`"), "{errs:#?}");
    assert!(errs[2].starts_with("[SHELL5006] #[validate(with = \"...\")] must be a function path: `not a path` in #[validate(with)]"), "{errs:#?}");
    assert_eq!(errs[3], "validation rule references unknown field `nobody`: in #[requires] on `b`");
}

#[test]
fn a_raw_identifier_command_entry_does_not_panic() {
    let input: DeriveInput = parse_quote! {
        #[subcommand(segment = "s")]
        pub struct S {
            #[composite(command)]
            r#type: T,
        }
    };
    assert!(crate::composite_subcommand::expand_composite_subcommand(&input).is_ok());
}

fn composite_errors(input: DeriveInput, sub: bool) -> Vec<String> {
    let r = if sub {
        crate::composite_subcommand::expand_composite_subcommand(&input)
    } else {
        crate::composite_shell::expand_composite_shell(&input)
    };
    match r {
        Ok(_) => Vec::new(),
        Err(e) => e.into_iter().map(|e| e.to_string()).collect(),
    }
}

#[test]
fn a_decomposed_unicode_method_name_is_accepted() {
    let input: DeriveInput = syn::parse_str("#[shell(cmd = \"x\")] struct X { #[builder(method = \"e\u{301}\")] a: u32 }").unwrap();
    assert_eq!(errors(input), Vec::<String>::new());
}

#[test]
fn two_separate_cycles_are_two_errors() {
    let input: DeriveInput = parse_quote! {
        #[shell(cmd = "x")]
        struct X {
            #[shell(flag = "-a", order = "before:b")] a: bool,
            #[shell(flag = "-b", order = "before:a")] b: bool,
            #[shell(flag = "-c", order = "before:d")] c: bool,
            #[shell(flag = "-d", order = "before:c")] d: bool,
        }
    };
    assert_eq!(codes(input), ["SHELL2003", "SHELL2003"]);
}

#[test]
fn a_target_naming_a_failed_field_is_not_matched_to_a_case_alike() {
    // `a` failed to read; `A` orders before `a`, which must not resolve to `A` itself
    let input: DeriveInput = parse_quote! {
        #[shell(cmd = "x")]
        #[allow(non_snake_case)]
        struct X {
            #[shell(flag = "-x", bogus)] a: bool,
            #[shell(flag = "-y", order = "before:a")] A: bool,
        }
    };
    let errs = errors(input);
    assert_eq!(errs.len(), 1, "{errs:#?}");
    assert!(errs[0].starts_with("unknown attribute key `bogus`"));
}

#[test]
fn an_unreadable_builder_still_lets_sep_be_checked_and_order_counts_when_bad() {
    let input: DeriveInput = parse_quote! {
        #[shell(cmd = "x")]
        struct X {
            #[builder(bogus)] #[shell(sep = ",")] f: String,
            #[shell(order = "sideways")] g: bool,
        }
    };
    let errs = errors(input);
    assert_eq!(errs.len(), 4, "{errs:#?}");
    assert!(errs[0].starts_with("unknown attribute key `bogus`"));
    assert!(errs[1].starts_with("[SHELL1006]"));
    assert!(errs[2].starts_with("[SHELL1002]"));
    assert!(errs[3].starts_with("[SHELL1004]"));
}

#[test]
fn every_bad_expression_key_is_reported_and_a_missing_cmd_still_is() {
    let input: DeriveInput = parse_quote! {
        #[shell(trait_path = "not a path", cmd_expr = "(")]
        struct X {
            #[shell(arg_expr = "(", opt_expr = "(")] a: u8,
        }
    };
    let errs = errors(input);
    assert_eq!(errs.len(), 4, "{errs:#?}");
    assert!(errs[0].starts_with("trait_path is not a path"));
    assert!(errs[1].starts_with("cmd_expr parse error"));
    assert!(errs[2].starts_with("arg_expr parse error"));
    assert!(errs[3].starts_with("opt_expr parse error"));
    let input: DeriveInput = parse_quote! {
        #[shell(trait_path = "not a path")]
        struct Y { #[shell(flag = "-a")] a: bool }
    };
    let errs = errors(input);
    assert_eq!(errs.len(), 2, "{errs:#?}");
    assert!(errs[1].starts_with("[SHELL0001]"));
}

#[test]
fn subcommand_fields_keep_their_checks_past_a_bad_method_or_rule() {
    let input: DeriveInput = parse_quote! {
        #[subcommand(segment = "s")]
        #[one_of(1)]
        #[at_least_one_of(2)]
        pub struct S {
            #[arg(flag = "-a")] #[builder(method = "x y")] a: String,
            #[arg(flag = "-b")] #[requires(5)] #[conflicts_with(6)] b: u8,
        }
    };
    let errs = sub_errors(input);
    assert_eq!(errs.len(), 7, "{errs:#?}");
    // field a's method, field b's two rules, the struct's two rules, then the
    // contract's check of both fields
    assert!(errs[0].starts_with("[SHELL3007]"), "{errs:#?}");
    for e in &errs[1..5] {
        assert!(e.starts_with("expected string literal"), "{errs:#?}");
    }
    assert!(errs[5].starts_with("#[arg(flag = ...)] currently requires bool"), "{errs:#?}");
    assert!(errs[6].starts_with("#[arg(flag = ...)] currently requires bool"), "{errs:#?}");
}

#[test]
fn every_problem_in_one_composite_entry_is_reported() {
    let input: DeriveInput = parse_quote! {
        #[shell(program = "git")]
        #[composite(command = "if", init = "a b")]
        pub struct Root;
    };
    let errs = composite_errors(input, false);
    assert_eq!(errs.len(), 3, "{errs:#?}");
    assert!(errs[0].starts_with("`if` does not make a method name"), "{errs:#?}");
    assert!(errs[1].starts_with("registry-style #[composite] requires ty"), "{errs:#?}");
    let input: DeriveInput = parse_quote! {
        #[subcommand(segment = "remote")]
        pub struct Surf {
            #[composite(surface, method = "x y")] s: S,
        }
    };
    let errs = composite_errors(input, true);
    assert_eq!(errs.len(), 2, "{errs:#?}");
    assert!(errs[1].starts_with("[SHELL7004]"), "{errs:#?}");
}

#[test]
fn a_non_unit_mode_variant_still_has_its_attribute_checked() {
    let input: DeriveInput = parse_quote! { enum M { #[shell(bogus)] C(u8) } };
    let errs: Vec<String> = match crate::simple::expand_shell_mode(&input) {
        Ok(_) => Vec::new(),
        Err(e) => e.into_iter().map(|e| e.to_string()).collect(),
    };
    assert_eq!(errs.len(), 2, "{errs:#?}");
    assert!(errs[0].starts_with("unknown attribute key `bogus`"));
    assert!(errs[1].starts_with("[SHELL4002]"));
}

#[test]
fn method_names_are_read_by_the_lexer_with_spaces_and_comments_refused() {
    for name in ["foo\u{a0}", "foo\u{3000}", "foo\u{1680}", "foo\u{2000}", "\u{feff}foo", "foo\u{200b}", "r#self", "r#", "1a", "type", "fn", "self", "_", "yield"] {
        assert!(!is_ident(name), "{name:?} must be refused");
    }
    for name in ["foo", "_foo", "r#type", "async", "dyn", "union", "default", "e\u{301}", "\u{e9}", "größe"] {
        assert!(is_ident(name), "{name:?} must be accepted");
    }
}

#[test]
fn an_unparsable_expression_still_counts_as_written() {
    let input: DeriveInput = parse_quote! {
        #[shell(cmd = "x")]
        struct X {
            #[shell(arg_expr = "f(1", fmt = "<{}>")] a: u8,
            #[shell(arg_expr = "g(1", arg_join_opt = "nope")] b: String,
            #[builder(init_required, default_expr = "h(1")] c: u8,
        }
    };
    let errs = errors(input);
    assert_eq!(errs.len(), 5, "{errs:#?}");
    assert!(errs[0].starts_with("arg_expr parse error"));
    assert!(errs[1].starts_with("arg_expr parse error"));
    assert!(errs[2].starts_with("default_expr parse error"));
    assert!(errs[3].starts_with("[SHELL3002]"));
    assert!(errs[4].starts_with("[SHELL1020]"));
}

#[test]
fn a_surface_with_unreadable_init_names_still_gets_its_own_error() {
    let input: DeriveInput = parse_quote! {
        #[shell(program = "git")]
        pub struct Root {
            #[composite(surface, init = "1x, 2y")] s: S,
        }
    };
    let errs = composite_errors(input, false);
    assert_eq!(errs.len(), 3, "{errs:#?}");
    assert!(errs[0].starts_with("init name `1x` is not an identifier"));
    assert!(errs[1].starts_with("init name `2y` is not an identifier"));
    assert!(errs[2].starts_with("[SHELL6004]"));
}

#[test]
fn a_subcommand_of_the_wrong_shape_still_has_its_rules_checked() {
    let input: DeriveInput = parse_quote! {
        #[subcommand(segment = "s")]
        #[validate(with = "not a path")]
        pub enum S { A }
    };
    let errs = sub_errors(input);
    assert_eq!(errs.len(), 2, "{errs:#?}");
    assert!(errs[0].starts_with("[SHELL5002]"), "{errs:#?}");
    assert!(errs[1].starts_with("[SHELL5006]"), "{errs:#?}");
}

#[test]
fn every_kind_on_a_multi_kind_field_has_its_type_checked() {
    let input: DeriveInput = parse_quote! {
        #[shell(cmd = "x")]
        struct X {
            #[shell(count_flag = "v", flag = "-F")] a: String,
            #[shell(flag = "-H", arg_join_opt = "nope")] c: bool,
        }
    };
    assert_eq!(codes(input), ["SHELL1001", "SHELL1013", "SHELL1014", "SHELL1001", "SHELL1005"]);
}

#[test]
fn a_tuple_subcommand_still_has_its_field_rules_read() {
    let input: DeriveInput = parse_quote! {
        #[subcommand(segment = "s")]
        pub struct S(#[requires(1)] u8, #[conflicts_with("c d")] u8);
    };
    let errs = sub_errors(input);
    // the tuple's fields are checked under stand-in names: neither has an #[arg]
    assert_eq!(errs.len(), 5, "{errs:#?}");
    assert!(errs[0].starts_with("[SHELL5003]"), "{errs:#?}");
    assert!(errs[1].starts_with("expected string literal"), "{errs:#?}");
    assert!(errs[2].starts_with("[SHELL5004]"), "{errs:#?}");
    assert!(errs[3].starts_with("[SHELL5004]"), "{errs:#?}");
    assert!(errs[4].starts_with("[SHELL5005]") && errs[4].contains("on `_1`"), "{errs:#?}");
}

#[test]
fn a_subcommand_field_with_an_unreadable_builder_is_still_checked() {
    let input: DeriveInput = parse_quote! {
        #[subcommand(segment = "s")]
        pub struct S {
            #[arg(flag = "-a")] #[builder(nope)] a: String,
        }
    };
    let errs = sub_errors(input);
    assert_eq!(errs.len(), 2, "{errs:#?}");
    assert!(errs[0].starts_with("unknown attribute key `nope`"), "{errs:#?}");
    assert!(errs[1].starts_with("#[arg(flag = ...)] currently requires bool"), "{errs:#?}");
}

#[test]
fn a_composite_enum_is_only_told_it_is_not_a_struct() {
    let input: DeriveInput = parse_quote! {
        #[shell(program = "git")]
        pub enum Root { A(#[composite(command)] Add) }
    };
    assert_eq!(composite_errors(input, false), ["[SHELL6001] CompositeShell can only be derived for structs"]);
}

#[test]
fn an_unreadable_builder_on_a_field_that_cannot_be_positional_hides_nothing() {
    let input: DeriveInput = parse_quote! {
        #[shell(cmd = "x")]
        pub struct S {
            #[shell(order = 1)] #[builder(bogus)] pub b: bool,
            #[shell(flag = "-x", order = "before:c")] pub a: bool,
            #[builder(bogus)] pub c: Option<String>,
        }
    };
    let errs = errors(input);
    assert_eq!(errs.len(), 4, "{errs:#?}");
    assert!(errs[0].starts_with("unknown attribute key `bogus`"));
    assert!(errs[1].starts_with("unknown attribute key `bogus`"));
    assert!(errs[2].starts_with("[SHELL1004]"));
    assert!(errs[3].starts_with("[SHELL2001]"));
}

#[test]
fn a_multi_kind_field_is_not_told_about_a_partner_it_never_reads() {
    let input: DeriveInput = parse_quote! {
        #[shell(cmd = "x")]
        pub struct S {
            #[shell(arg_expr = "self.n + 1", arg_join_opt = "nope", flag = "-f")] pub a: bool,
            pub n: u8,
        }
    };
    assert_eq!(codes(input), ["SHELL1001"]);
}

#[test]
fn two_rules_naming_the_same_unknown_field_are_two_messages() {
    let input: DeriveInput = parse_quote! {
        #[subcommand(segment = "s")]
        pub struct S {
            #[arg(flag = "-a")] #[requires("nope")] a: bool,
            #[arg(flag = "-b")] #[conflicts_with("nope")] b: bool,
        }
    };
    assert_eq!(
        sub_errors(input),
        [
            "validation rule references unknown field `nope`: in #[requires] on `a`",
            "validation rule references unknown field `nope`: in #[conflicts_with] on `b`",
        ]
    );
}

#[test]
fn a_tuple_subcommand_still_has_its_field_attributes_read() {
    let input: DeriveInput = parse_quote! {
        #[subcommand(segment = "s")]
        pub struct S(#[arg(bogus)] u8, #[builder(method = "x y")] u8);
    };
    let errs = sub_errors(input);
    assert_eq!(errs.len(), 4, "{errs:#?}");
    assert!(errs[0].starts_with("[SHELL5003]"), "{errs:#?}");
    assert!(errs[1].starts_with("unknown attribute key `bogus`"), "{errs:#?}");
    assert!(errs[2].starts_with("[SHELL3007]"), "{errs:#?}");
    assert!(errs[3].starts_with("[SHELL5004]"), "{errs:#?}");
}

#[test]
fn a_target_two_case_alike_fields_share_adds_no_edge() {
    let input: DeriveInput = parse_quote! {
        #[shell(cmd = "x")]
        #[allow(non_snake_case)]
        pub struct S {
            #[shell(flag = "-a")] pub zz: bool,
            #[shell(flag = "-b", order = "after:a")] pub Zz: bool,
            #[shell(flag = "-c", order = "after:ZZ")] pub a: bool,
        }
    };
    assert_eq!(codes(input), ["SHELL2002"]);
}

#[test]
fn a_rule_reads_as_written_with_its_field() {
    let input: DeriveInput = parse_quote! {
        #[subcommand(segment = "s")]
        pub struct S {
            #[arg(flag = "-j")] #[one_of("k", "1x")] j: bool,
            #[arg(flag = "-m")] #[one_of("k", "2y")] m: bool,
        }
    };
    let errs = sub_errors(input);
    assert_eq!(errs.len(), 4, "{errs:#?}");
    assert!(errs[0].contains("`1x` in #[one_of(\"k\", \"1x\")] on `j`"), "{errs:#?}");
    assert!(errs[1].contains("`2y` in #[one_of(\"k\", \"2y\")] on `m`"), "{errs:#?}");
    assert_eq!(errs[2], "validation rule references unknown field `k`: in #[one_of(\"k\", \"1x\")] on `j`");
    assert_eq!(errs[3], "validation rule references unknown field `k`: in #[one_of(\"k\", \"2y\")] on `m`");
}

#[test]
fn a_tuple_subcommand_has_its_fields_checked_under_stand_in_names() {
    let input: DeriveInput = parse_quote! {
        #[subcommand(segment = "s")]
        pub struct S(#[arg(flag = "-a")] String, #[arg(flag = "-b")] #[builder(init_required)] bool);
    };
    let errs = sub_errors(input);
    assert_eq!(errs.len(), 3, "{errs:#?}");
    assert!(errs[0].starts_with("[SHELL5003]"), "{errs:#?}");
    assert!(errs[1].starts_with("#[arg(flag = ...)] currently requires bool"), "{errs:#?}");
    assert!(errs[2].starts_with("#[builder(init_required)] cannot be used with #[arg(flag = ...)]"), "{errs:#?}");
}

#[test]
fn an_enum_subcommand_is_not_told_its_own_fields_are_unknown() {
    let input: DeriveInput = parse_quote! {
        #[subcommand(segment = "add")]
        #[one_of("a", "b")]
        pub enum Add { V { #[arg(flag = "-a")] a: bool, #[arg(kv = "-b")] #[requires("a")] b: Option<String> } }
    };
    assert_eq!(sub_errors(input), ["[SHELL5002] SimpleSubCommand can only be derived for structs"]);
}

#[test]
fn the_same_bad_function_path_on_two_fields_is_two_messages() {
    let input: DeriveInput = parse_quote! {
        #[subcommand(segment = "add")]
        pub struct Add {
            #[arg(flag = "-a")] #[validate(with = "check a")] a: bool,
            #[arg(flag = "-b")] #[validate(with = "check a")] b: bool,
        }
    };
    let errs = sub_errors(input);
    assert_eq!(errs.len(), 2, "{errs:#?}");
    assert!(errs[0].contains("in #[validate(with)] on `a`"), "{errs:#?}");
    assert!(errs[1].contains("in #[validate(with)] on `b`"), "{errs:#?}");
}
