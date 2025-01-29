const std = @import("std");
const lexer = @import("../lexer/lexer.zig");
const ast = @import("ast.zig");
const parser = @import("parser.zig");
const Parser = parser.Parser;
const Err = parser.Err;
const Tag = ast.Tag;
const basic = @import("basic.zig");
const expr_module = @import("expr.zig");
const stmt = @import("stmt.zig");

const Options = struct {
    no_object_call: bool = false,
};

const OpInfo = struct {
    prec: i8,
    tag: ast.Tag,
    assoc_left: bool = true,
};

pub fn tryPattern(self: *Parser, opt: Options) Err!u64 {
    return tryPratt(self, 0, opt);
    // return tryPrefixPattern(self, opt);
}

const op_table = std.enums.directEnumArrayDefault(
    lexer.Token.Tag,
    OpInfo,
    .{ .prec = -1, .tag = .invalid },
    0,
    .{
        .k_if = .{ .prec = 10, .tag = .if_guard },
        .k_and = .{ .prec = 10, .tag = .and_is },

        .k_as = .{ .prec = 20, .tag = .as_bind },

        .k_or = .{ .prec = 30, .tag = .bool_or },

        .@"?" = .{ .prec = 40, .tag = .@"id?" },

        .@"(" = .{ .prec = 80, .tag = .pattern_call },
        .@"{" = .{ .prec = 80, .tag = .object_pattern_call },

        .@"." = .{ .prec = 90, .tag = .select },
    },
);

fn tryPratt(self: *Parser, min_prec: i8, opt: Options) Err!u64 {
    try self.enter();
    defer self.exit();

    var left = try tryPrefixPattern(self, opt);
    if (left == 0) {
        return 0;
    }

    while (true) {
        const token = self.peekToken();
        const op_info = op_table[@intFromEnum(token.tag)];

        if (op_info.tag == .invalid or op_info.prec < min_prec)
            break;

        switch (token.tag) {
            .@"?" => {
                self.eatTokens(1);
                left = try self.pushNode(.{ Tag.@"id?", left });
                continue;
            },
            .@"(" => {
                const rules = .{
                    basic.rule("pattern", Parser.tryPattern),
                };
                const nodes = try basic.pMulti(self, rules, .@"(");
                defer nodes.deinit();
                left = try self.pushNode(.{ Tag.pattern_call, left, nodes.items });
                continue;
            },
            .@"{" => {
                if (opt.no_object_call) break;
                const rules = .{
                    basic.rule("object field pattern", tryObjectFieldPattern),
                    basic.rule("id binding", basic.tryId),
                };

                const nodes = try basic.pMulti(self, rules, .@"{");
                defer nodes.deinit();
                left = try self.pushNode(.{ Tag.object_pattern_call, left, nodes.items });
                continue;
            },
            // it might be a select, @"pattern . *", range_from, range_from_to, range_from_to_inclusive
            .@"." => {
                _ = self.nextToken();
                if (self.eatToken(.@"*")) {
                    left = try self.pushNode(.{ Tag.select_all, left });
                    continue;
                }

                if (self.peek(&.{.id})) {
                    const id = try self.pushAtom(.id);
                    left = try self.pushNode(.{ Tag.select, left, id });
                    continue;
                }

                if (self.eatToken(.@".")) {
                    if (basic.isTerminator(self.peekToken())) {
                        left = try self.pushNode(.{ Tag.range_from, left });
                        continue;
                    }

                    const tag = if (self.eatToken(.@"=")) Tag.range_from_to_inclusive else Tag.range_from_to;
                    const pattern = try tryPrefixPattern(self, .{});

                    left = try self.pushNode(.{ tag, left, pattern });
                    continue;
                }

                return self.invalidPattern(self.rcursor, self.rcursor, "unable to recognize this pattern😢");
            },
            // pattern and expr is pattern
            .k_and => {
                _ = self.nextToken();
                const expr = try self.tryExpr();
                try self.expectNextToken(.k_is, "expect a `is` to specify the pattern");
                const pattern = try tryPratt(self, op_info.prec + 1, .{});

                left = try self.pushNode(.{ Tag.and_is, left, expr, pattern });
                continue;
            },
            .k_if => {
                _ = self.nextToken();
                const expr = try self.tryExpr();

                left = try self.pushNode(.{ Tag.if_guard, left, expr });
                continue;
            },
            else => {},
        }

        self.eatTokens(1);
        const right = try tryPratt(self, op_info.prec + 1, .{});
        if (right == 0) {
            return self.unexpectedToken("expect a pattern");
        }

        const result = try self.push(@intFromEnum(op_info.tag), .tag);
        _ = try self.push(left, .child);
        _ = try self.push(right, .child);
        left = result;
    }

    return left;
}

fn tryPrefixPattern(self: *Parser, _: Options) Err!u64 {
    try self.enter();
    defer self.exit();
    const token = self.peekToken();
    return switch (token.tag) {
        .id => try self.pushAtom(.id),
        .int => try expr_module.tryDigit(self),
        .str => try self.pushAtom(.str),
        .char => try self.pushAtom(.char),
        .k_true => try self.pushAtom(.bool),
        .k_false => try self.pushAtom(.bool),
        .k_null => try self.pushAtom(.null),
        // .k_void => try self.pushAtom(.void),
        // .k_noreturn => try self.pushAtom(.noreturn),
        // .k_Any => try self.pushAtom(.Any),
        // .k_any => try self.pushAtom(.any),
        // .k_unit => try self.pushAtom(.unit),

        .@"." => try tryRangeOrSymbol(self),

        .@"(" => try tryTuplePattern(self),
        .@"{" => try tryObjectPattern(self),
        .@"[" => try tryListPattern(self),
        .@"<" => try tryExprPattern(self),

        else => 0,
    };
}

fn tryObjectFieldPattern(self: *Parser) Err!u64 {
    try self.enter();
    defer self.exit();

    if (!self.peek(&.{ .id, .@":" })) return 0;
    const id = try basic.tryId(self);
    self.eatTokens(1);

    return try self.pushNode(.{ Tag.@"id : pattern", id, try self.tryPattern() });
}

fn tryTuplePattern(self: *Parser) Err!u64 {
    try self.enter();
    defer self.exit();
    if (!self.peek(&.{.@"("}))
        return 0;

    const rules = .{
        basic.rule("pattern", Parser.tryPattern),
    };
    const nodes = try basic.pMulti(self, rules, .@"(");
    defer nodes.deinit();
    return try self.pushNode(.{ Tag.tuple_pattern, nodes.items });
}

fn tryObjectPattern(self: *Parser) Err!u64 {
    try self.enter();
    defer self.exit();
    if (!self.peek(&.{.@"{"}))
        return 0;

    const rules = .{
        basic.rule("object field pattern", tryObjectFieldPattern),
        basic.rule("id binding", basic.tryId),
    };

    const nodes = try basic.pMulti(self, rules, .@"{");
    defer nodes.deinit();
    return try self.pushNode(.{ Tag.object_pattern, nodes.items });
}

fn tryListPattern(self: *Parser) Err!u64 {
    try self.enter();
    defer self.exit();
    if (!self.peek(&.{.@"["}))
        return 0;

    const rules = .{
        basic.rule("pattern", Parser.tryPattern),
    };
    const nodes = try basic.pMulti(self, rules, .@"[");
    defer nodes.deinit();
    return try self.pushNode(.{ Tag.list_pattern, nodes.items });
}

fn tryExprPattern(self: *Parser) Err!u64 {
    try self.enter();
    defer self.exit();
    if (!self.peek(&.{.@"<"}))
        return 0;

    const expr = try self.tryExpr();
    try self.expectNextToken(.@">", "expect a `>` to close the pattern");
    return try self.pushNode(.{ Tag.@"< expr >", expr });
}

fn tryRangeOrSymbol(self: *Parser) Err!u64 {
    try self.enter();
    defer self.exit();

    if (self.peek(&.{ .@".", .@".", .@".", .id })) {
        self.eatTokens(3);
        const id = try basic.tryId(self);

        return try self.pushNode(.{ Tag.list_rest_bind, id });
    }

    // ..prefixPattern or ..= prefixPattern
    if (self.peek(&.{ .@".", .@"." })) {
        self.eatTokens(2);
        const tag = if (self.eatToken(.@"=")) Tag.range_to_inclusive else Tag.range_to;
        const pattern = try tryPrefixPattern(self, .{});
        if (pattern == 0)
            return self.unexpectedToken("expect a pattern after `..` or `..=`");

        return try self.pushNode(.{ tag, pattern });
    }

    return try basic.trySymbol(self);
}

pub fn tryBranches(self: *Parser) Err!u64 {
    try self.enter();
    defer self.exit();
    if (!self.peek(&.{.@"{"})) return 0;

    const rules = .{
        basic.rule("catch branch", tryCatchBranch),
        basic.rule("pattern branch", tryPatternBranch),
    };

    const nodes = try basic.pMulti(self, rules, .@"{");
    defer nodes.deinit();
    return try self.pushNode(.{ Tag.branches, nodes.items });
}

pub fn tryPatternBranch(self: *Parser) Err!u64 {
    try self.enter();
    defer self.exit();

    var pattern: u64 = 0;
    var stmt_or_block: u64 = 0;

    pattern = try tryPattern(self, .{});
    if (pattern == 0) return 0;

    try self.expectNextToken(.@"=>", "expect a `=>` to separate pattern and expr");
    stmt_or_block = try stmt.tryBlock(self);
    if (stmt_or_block == 0) {
        stmt_or_block = try self.tryStmt();
    }
    return try self.pushNode(.{ Tag.pattern_branch, pattern, stmt_or_block });
}

pub fn tryCatchBranch(self: *Parser) Err!u64 {
    try self.enter();
    defer self.exit();
    if (!self.eatToken(.k_catch)) return 0;

    var id: u64 = 0;
    var stmt_or_block: u64 = 0;

    if (self.peek(&.{.id}))
        id = try self.pushAtom(.id)
    else
        return self.unexpectedToken("expect an error name to bind the error");

    try self.expectNextToken(.@"=>", "expect a `=>` to separate error name and expr");

    stmt_or_block = try stmt.tryBlock(self);
    if (stmt_or_block == 0)
        stmt_or_block = try self.tryStmt();

    return try self.pushNode(.{ Tag.catch_branch, id, stmt_or_block });
}
