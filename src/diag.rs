//! The derive's error codes, in one liaise table. Rendered `[SHELL1002] message`.

pub(crate) use liaise_proc_macro::{err_at, err_at_ctx, err_spanned};

liaise_proc_macro::liaise_codes! {
    prefix: "SHELL",
    pub(crate) E {
        // 0xxx: struct level
        MissingStructCmd = 1 => "missing #[shell(cmd = \"...\")] or #[shell(cmd_expr = \"...\")] on struct",
        OnlyStructsSupported = 2 => "SimpleImpl only supports structs",
        NamedFieldsRequired = 3 => "SimpleImpl requires a named-field struct (e.g., struct Foo { bar: i32 })",
        NamedFieldsRequired2 = 4 => "SimpleImpl requires named fields (e.g., `field_name: Type`)",
        NamedFieldsRequiredGeneral = 5 => "expected named field",

        // 1xxx: field level
        MultipleEmissionKinds = 1001 => "field has multiple #[shell(...)] emission kinds; choose only one",
        SepWithoutJoin = 1002 => "#[shell(sep = \"...\")] is only valid with #[shell(arg_join_opt = \"other_field\")]",
        RequireOrderMissing = 1003 => "missing #[shell(order = ...)] (struct has #[shell(require_order)])",
        OrderOnNonEmitted = 1004 => "#[shell(order=...)] provided but this field is not emitted",
        ArgJoinUnknownField = 1005 => "arg_join_opt references unknown field",
        InvalidOrderValue = 1006 => "order must be an integer, or \"first\", \"last\", \"before:FIELD\", \"after:FIELD\", or order(first), order(last), order(before = FIELD), order(after = FIELD)",
        InvalidOrderType = 1007 => "order must be an integer, a string or order(...)",
        OptKvRequiresOption = 1010 => "#[shell(opt_kv = \"...\")] requires Option<T>",
        OptPrefixRequiresOption = 1011 => "#[shell(opt_prefix = \"...\")] requires Option<T>",
        OptEqRequiresOption = 1012 => "#[shell(opt_eq = \"...\")] requires Option<T>",
        FlagRequiresBool = 1013 => "#[shell(flag/flag_off)] requires bool",
        CountFlagRequiresUint = 1014 => "#[shell(count_flag = \"...\")] requires u8/u16/u32/u64/usize",
        MultiOptKvRequiresVec = 1015 => "#[shell(multi_opt_kv = \"...\")] requires Vec<T> or Option<Vec<T>>",
        MultiOptPrefixRequiresVec = 1016 => "#[shell(multi_opt_prefix = \"...\")] requires Vec<T> or Option<Vec<T>>",
        MultiArgFlagRequiresVec = 1017 => "#[shell(multi_arg_flag = \"...\")] requires Vec<T> or Option<Vec<T>>",
        MultipleSubcommands = 1018 => "multiple #[shell(subcommand)] fields; only one is supported",
        ArgCloneRequiresString = 1019 => "#[shell(arg_clone)] is only valid for String or Option<String>",
        FmtOnWrongKind = 1020 => "#[shell(fmt = \"...\")] applies to kv, opt_kv, multi_opt_kv, multi_arg_flag and positional fields; prefix, eq, flag and count kinds shape their own value",
        FmtNeedsOnePlaceholder = 1021 => "#[shell(fmt = \"...\")] needs exactly one `{}`",
        JoinOnWrongKind = 1022 => "#[shell(join = \"...\")] applies to multi_arg_flag, multi_opt_kv and a positional Vec",
        ExprNotWritten = 1023 => "this key takes an expression, written as code (key = self.n + 1) or as a string (key = \"self.n + 1\")",

        // 2xxx: ordering
        OrderTargetUnknown = 2001 => "order target references unknown (or non-emitted) field",
        OrderCaseCollision = 2002 => "case-insensitive order target collision; use exact field names",
        OrderCycle = 2003 => "ordering cycle detected in #[shell(order=...)] constraints",
        OrderBeforeMissingTarget = 2004 => "order=\"before:FIELD\" requires a field name",
        OrderAfterMissingTarget = 2005 => "order=\"after:FIELD\" requires a field name",

        // 3xxx: builder
        BuilderPushNeedsName = 3001 => "builder(push) requires a dedicated naming scheme",
        InitRequiredConflictDefault = 3002 => "#[builder(init_required)] cannot be combined with default_expr",
        InitOnlyConflictKind = 3003 => "#[builder(init_only)] cannot be combined with builder kind overrides",
        PushRequiresVec = 3004 => "#[builder(push)] requires a Vec<T> field",
        OptKindRequiresOption = 3005 => "#[builder(opt)] and #[builder(opt_into)] require an Option<T> field",
        VecKindRequiresVec = 3006 => "#[builder(vec_into)] and #[builder(vec_iter)] require a Vec<T> field",
        MethodNotIdent = 3007 => "#[builder(method = \"...\")] must be a Rust identifier",

        // 4xxx: SimpleShellMode
        ModeOnlyEnums = 4001 => "SimpleShellMode only supports enums",
        ModeUnitVariantsOnly = 4002 => "SimpleShellMode only supports unit variants (e.g. Json, Xml, Raw)",

        // 5xxx: SimpleSubCommand
        SubcmdMissingSegment = 5001 => "SimpleSubCommand requires #[subcommand(segment = \"...\")]",
        SubcmdOnlyStructs = 5002 => "SimpleSubCommand can only be derived for structs",
        SubcmdNamedFields = 5003 => "SimpleSubCommand requires named fields",
        SubcmdFieldNeedsArg = 5004 => "SimpleSubCommand fields require #[arg(flag = \"...\")], #[arg(kv = \"...\")], or #[arg(positional)]",
        RuleFieldNotIdent = 5005 => "validation rule field must be a Rust field identifier",
        RuleFnNotPath = 5006 => "#[validate(with = \"...\")] must be a function path",
        InitRequiredRepeated = 5007 => "#[builder(init_required)] takes one value; a Vec field is filled by its setter",
        MinNeedsVec = 5008 => "#[arg(min = N)] counts a Vec field's values; this field is not a Vec",
        MinNotCount = 5009 => "#[arg(min = N)] is a count: 0 or more",
        ExitCodeNotI32 = 5010 => "#[exit_status(code = N)] is an exit status, an i32",
        ExitCodeTwice = 5011 => "an exit status is named once; this code is already named",

        // 6xxx: CompositeShell
        CompositeShellOnlyStructs = 6001 => "CompositeShell can only be derived for structs",
        CompositeEntryExpected = 6002 => "expected #[composite(command = \"...\", ty = SomeType)]",
        CompositeShellNoEntries = 6003 => "CompositeShell requires at least one #[composite(command = \"...\", ty = SomeType)] entry",
        CompositeShellSurfaceInitArgs = 6004 => "CompositeShell surface entries cannot use init args yet",

        // 7xxx: CompositeSubCommand
        CompositeSubOnlyStructs = 7001 => "CompositeSubCommand can only be derived for structs",
        CompositeSubMissingSegment = 7002 => "CompositeSubCommand requires #[subcommand(segment = \"...\")]",
        CompositeSubNoEntries = 7003 => "CompositeSubCommand requires at least one #[composite(command = \"...\", ty = SomeType)] entry",
        CompositeSubNestedSurface = 7004 => "CompositeSubCommand nested surfaces are not generated yet; this pass supports command entries",

        // 9xxx: internal
        Unmatched = 9001 => "internal error: field marked emitted but no emission kind matched",
    }
}
