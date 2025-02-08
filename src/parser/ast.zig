const std = @import("std");
const vfs = @import("../vfs.zig");
const lexer = @import("../lexer/lexer.zig");
const error_code = @import("../error_code.zig");

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
    effect_def,
    diamond_fn_def,
    trait_def,
    derivation,
    // impl expr (for expr)? clauses? block
    impl_def,

    // requires expr
    requires_predicate,
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
    file_scope,
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

pub const Span = struct {
    node: u64,
    from: u64,
    to: u64,
};

pub const Ast = struct {
    spans: std.ArrayList(Span),
    src: []const u8,
    src_id: u32,
    vfs: *vfs.Vfs,
    tokens: std.ArrayList(lexer.Token),
    nodes: std.ArrayList(u64),

    // dump node to s-expression format
    pub fn dump(self: Ast, node_index: u64, writer: anytype) !void {
        const node = self.nodes.items[node_index];
        const tag: Tag = @enumFromInt(node);
        try writer.print("({s} ", .{@tagName(tag)});
        switch (tag) {
            // single tag
            .self,
            .invalid,
            .null,
            => {},
            // with single token
            .int,
            .str,
            .id,
            .bool,
            .char,
            => {
                const token = self.getToken(self.getNode(node_index + 1));
                try writer.writeAll(self.srcContent(token));
            },
            // with multiple tokens
            .real => {
                const left = self.getToken(self.getNode(node_index + 1));
                const right = self.getToken(self.getNode(node_index + 2));
                try writer.print("{s}.{s}", .{ self.srcContent(left), self.srcContent(right) });
            },
            // with single child
            .symbol,
            .do_block,
            .quote,
            .bool_not,
            .range_from,
            .range_to,
            .range_to_inclusive,
            .list_rest_bind,
            .@"id?",
            .@"pattern . *",
            .@"< expr >",
            .@"< pattern >",
            .@"pub stmt",
            .@"?expr",
            .select_all,
            .super_path,
            .package_path,
            .asserts_stmt,
            .expr_stmt,
            .effect_elimination_unwrap,
            .error_elimination_unwrap,
            .option_elimination_unwrap,
            .defer_stmt,
            .errdefer_stmt,
            .requires_predicate,
            => {
                try self.dump(self.getNode(node_index + 1), writer);
            },
            // with two children
            .property,
            .add,
            .sub,
            .div,
            .mul,
            .add_add,
            .select,
            .image,
            .bool_eq,
            .bool_not_eq,
            .bool_and,
            .bool_or,
            .bool_gt,
            .bool_gt_eq,
            .bool_lt,
            .bool_lt_eq,
            .index_call,
            .range_from_to,
            .range_from_to_inclusive,
            .as_bind,
            .if_guard,
            .use_stmt,
            .break_stmt,
            .continue_stmt,
            .return_stmt,
            .assign_stmt,
            .assign_add_stmt,
            .assign_sub_stmt,
            .assign_div_stmt,
            .assign_mul_stmt,
            .assign_mod_stmt,
            .effect_elimination,
            .effect_elimination_use,
            .error_elimination,
            .error_elimination_use,
            .option_elimination,
            .match,
            .catch_branch,
            .pattern_branch,
            .condition_branch,
            .if_is_match_stmt,
            .while_loop,
            .enum_variant,
            .enum_variant_with_struct_def_body,
            .enum_variant_with_tuple,
            .enum_variant_with_value,
            .while_is_match_loop,
            .mod_def,
            .trait_bound_clause,
            .optional_trait_bound_clause,
            .@"id : pattern",
            .@"..id: expr",
            .@"pattern : expr",
            .@"#expr expr",
            .@"!expr expr",
            .@"id:- expr",
            .@".id:- expr",
            => {
                try self.dump(self.getNode(node_index + 1), writer);
                try writer.writeAll(" ");
                try self.dump(self.getNode(node_index + 2), writer);
            },
            // with multiple children
            .object,
            .list,
            .tuple,
            .object_pattern,
            .tuple_pattern,
            .list_pattern,
            .block,
            .file_scope,
            .clauses,
            .branches,
            .when,
            .struct_def_body,
            .enum_def_body,
            .union_def_body,
            => {
                const len = self.getNode(node_index + 1);
                try writer.print("(len {d}) ", .{len});
                for (0..len) |i| {
                    try self.dump(self.getNode(node_index + 2 + i), writer);
                }
            },
            // one left item and multiple children
            .call,
            .object_call,
            .diamond_call,
            .pattern_call,
            .object_pattern_call,
            .select_multi,
            => {
                try self.dump(self.getNode(node_index + 1), writer);
                const len = self.getNode(node_index + 2);
                try writer.print("(len {d}) ", .{len});
                for (0..len) |i| {
                    try self.dump(self.getNode(node_index + 3 + i), writer);
                }
            },
            // two left items and multiple children
            .derivation,
            => {
                try self.dump(self.getNode(node_index + 1), writer);
                try self.dump(self.getNode(node_index + 2), writer);
                const len = self.getNode(node_index + 3);
                try writer.print("(len {d}) ", .{len});
                for (0..len) |i| {
                    try self.dump(self.getNode(node_index + 4 + i), writer);
                }
            },
            // three children!
            .and_is,
            .let_decl,
            .const_decl,
            .clause,
            .if_stmt,
            .struct_def,
            .enum_def,
            .union_def,
            .struct_field,
            .@".id: expr = expr'",
            .for_loop,
            .while_is_loop,
            .trait_def,
            => {
                try self.dump(self.getNode(node_index + 1), writer);
                try writer.writeAll(" ");
                try self.dump(self.getNode(node_index + 2), writer);
                try writer.writeAll(" ");
                try self.dump(self.getNode(node_index + 3), writer);
            },
            // four children!
            .impl_def,
            .if_is_stmt,
            => {
                try self.dump(self.getNode(node_index + 1), writer);
                for (2..5) |i| {
                    try writer.writeAll(" ");
                    try self.dump(self.getNode(node_index + i), writer);
                }
            },
            // |args| (-> expr)? clause* (block | expr)
            .lambda => {
                try self.dump(self.getNode(node_index + 1), writer);
                try writer.writeAll(" ");
                try self.dump(self.getNode(node_index + 2), writer);
                try writer.writeAll(" ");
                try self.dump(self.getNode(node_index + 3), writer);
                try writer.writeAll(" ");

                const len = self.getNode(node_index + 4);
                try writer.print("(arg len {d}) ", .{len});
                for (0..len) |i| {
                    try self.dump(self.getNode(node_index + 5 + i), writer);
                }
            },
            // fn name? (args) (->expr)? clauses? (block | ;)
            .fn_def,
            .effect_def,
            .diamond_fn_def,
            => {
                try self.dump(self.getNode(node_index + 1), writer);
                try writer.writeAll(" ");
                try self.dump(self.getNode(node_index + 2), writer);
                try writer.writeAll(" ");
                try self.dump(self.getNode(node_index + 3), writer);
                try writer.writeAll(" ");
                try self.dump(self.getNode(node_index + 4), writer);
                try writer.writeAll(" ");
                const len = self.getNode(node_index + 5);
                try writer.print("(arg len {d}) ", .{len});
                for (0..len) |i| {
                    try self.dump(self.getNode(node_index + 6 + i), writer);
                }
            },
            else => {
                try writer.writeAll("todo");
            },
        }
        try writer.writeAll(")");
    }

    pub inline fn getNode(self: Ast, node_index: u64) u64 {
        return self.nodes.items[node_index];
    }

    pub fn getNodeTag(self: Ast, node_index: u64) Tag {
        return @enumFromInt(self.getNode(node_index));
    }

    pub fn getToken(self: Ast, cursor: u64) lexer.Token {
        return self.tokens.items[cursor];
    }

    pub fn srcContent(self: Ast, token: lexer.Token) []const u8 {
        return self.src[token.from..token.to];
    }

    pub fn srcContentD(self: Ast, node: u64) []const u8 {
        const token = self.getToken(self.getNode(self.getNode(node) + 1));
        return self.src[token.from..token.to];
    }

    pub fn srcContentT(self: Ast, node: u64) []const u8 {
        return self.srcContent(self.getToken(self.getNode(node + 1)));
    }

    pub fn toSlice(self: Ast, node_index: u64) []u64 {
        const len = self.getNode(node_index);
        return self.nodes.items[node_index + 1 .. node_index + 1 + len];
    }

    pub fn report(
        self: *Ast,
        node: u64,
        level: error_code.Kind,
        code: error_code.GlobalErr,
        label_info: []const u8,
        extra_lines: u64,
    ) !void {
        const span = self.getSpan(node).?;
        try self.vfs.reportB(
            vfs.BigSpan{ .from = vfs.Span{
                .src = self.src_id,
                .token = self.getToken(span.from),
            }, .to = vfs.Span{
                .src = self.src_id,
                .token = self.getToken(span.to),
            } },
            level,
            code,
            label_info,
            extra_lines,
        );
    }

    // binary search
    pub fn getSpan(self: *Ast, node: u64) ?Span {
        if (node == 0 or node >= self.nodes.items.len) return null;

        var left: u64 = 0;
        var right: u64 = self.spans.items.len;
        while (left < right) {
            const mid = left + (right - left) / 2;
            const span = self.spans.items[mid];
            if (span.node == node) return span;
            if (span.node < node) {
                left = mid + 1;
            } else {
                right = mid;
            }
        }
        return null;
    }

    pub fn deinit(self: *Ast) void {
        self.nodes.deinit();
        self.tokens.deinit();
        self.spans.deinit();
    }
};
