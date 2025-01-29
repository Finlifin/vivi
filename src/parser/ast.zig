pub const Tag = enum(u64) {
    id,
    int,
    real,
    str,
    bool,
    char,
    symbol,
    null,
    any,

    unit,

    object,
    tuple,
    quote,
    list,
    map,
    bool_not,
    do_block,
    @"< pattern >",

    // types
    Any,
    void,
    noreturn,
    @"#expr expr",
    @"!expr expr",
    @"?expr",

    when_expr,
    return_expr,
    range_to,
    range_to_inclusive,

    add,
    sub,
    mul,
    div,
    mod,
    pow,
    add_add,
    bool_eq,
    bool_not_eq,
    bool_and,
    bool_or,
    bool_gt,
    bool_gt_eq,
    bool_lt,
    bool_lt_eq,

    range_from,
    range_from_to,
    range_from_to_inclusive,

    bool_typed_with,
    bool_implements,

    select,
    image,
    pipe,
    pipe_first,

    // some()
    call,
    // some{}
    object_call,
    // some<>
    diamond_call,
    // some[]
    index_call,
    // expr # handler
    effect_elimination,
    // expr # use expr
    effect_elimination_use,
    // expr #
    effect_elimination_unwrap,
    // expr ! handler
    error_elimination,
    // expr ! use expr
    error_elimination_use,
    // expr !
    error_elimination_unwrap,
    // expr ? block
    option_elimination,
    // expr ?
    option_elimination_unwrap,
    // expr match branches
    match,
    // when cond_branches
    when,
    // |parameter*| expr
    lambda,
    // |parameter*| block
    lambda_block,

    list_rest_bind,
    as_bind,

    object_pattern,
    tuple_pattern,
    list_pattern,

    // postfix patterns
    @"pattern . *",
    @"id?",
    pattern_call,
    @"< expr >",
    object_pattern_call,
    if_guard,
    and_is,

    let_decl,
    const_decl,
    return_stmt,
    break_stmt,
    continue_stmt,
    use_stmt,
    defer_stmt,
    errdefer_stmt,
    asserts_stmt,
    expr_stmt,
    assign_stmt,
    assign_add_stmt,
    assign_sub_stmt,
    assign_mul_stmt,
    assign_div_stmt,
    assign_mod_stmt,
    newtype,
    typealias,
    when_stmt,
    if_stmt,
    if_is_stmt,
    if_is_match_stmt,
    for_loop,
    while_loop,
    while_is_loop,
    while_is_match_loop,

    // use terms
    package_path,
    super_path,
    select_multi,
    select_all,

    clauses,
    clause,
    trait_bound_clause,
    optional_trait_bound_clause,
    enum_def,
    enum_def_body,
    enum_variant,
    enum_variant_with_tuple,
    enum_variant_with_value,
    enum_variant_with_struct_def_body,
    enum_variant_with_sub_enum,
    struct_def,
    struct_def_body,
    struct_field,
    union_def,
    union_def_body,
    mod_def,
    fn_def,
    trait_def,
    derivation,
    // impl expr (for expr)? clauses? block
    impl_def,

    // .id expr
    property,
    // (catch_branch | pattern_branch)*
    branches,
    // pattern => expr | pattern => block
    pattern_branch,
    // catch id => expr | catch id => block
    catch_branch,
    // expr => expr | expr => block
    condition_branch,
    // { (property | stmt)* expr? }
    block,
    self,
    @"pub stmt",
    @"id : pattern",
    @"..id: expr",
    @"id:- expr",
    @".id:- expr",
    @"pattern : expr",
    @".id: expr = expr'",
    invalid,
};
