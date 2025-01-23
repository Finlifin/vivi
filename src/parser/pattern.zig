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

pub fn pPattern(self: *Parser, opt: Options) Err!u64 {
    return pPratt(self, 0, opt);
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

fn pPratt(self: *Parser, min_prec: i8, opt: Options) Err!u64 {
    var left = try pPrefixPattern(self, opt);
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
                _ = self.nextToken();
                left = try self.pushNode(.{ Tag.@"id?", left });
                continue;
            },
            .@"(" => {
                const rules = .{
                    basic.rule("pattern", Parser.pPattern),
                };
                const nodes = try basic.pMulti(
                    self,
                    rules,
                    .@"(",
                );
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

                const nodes = try basic.pMulti(
                    self,
                    rules,
                    .@"{",
                );
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
                    const id = try self.pushRaw(.id);
                    left = try self.pushNode(.{ Tag.select, left, id });
                    continue;
                }

                if (self.eatToken(.@".")) {
                    if (basic.isTerminator(self.peekToken())) {
                        left = try self.pushNode(.{ Tag.range_from, left });
                        continue;
                    }

                    const tag = if (self.eatToken(.@"=")) Tag.range_from_to_inclusive else Tag.range_from_to;
                    const pattern = try pPrefixPattern(self, .{});

                    left = try self.pushNode(.{ tag, left, pattern });
                    continue;
                }

                return self.invalidPattern(self.lcursor, self.rcursor, "unable to recognize this pattern😢");
            },
            // pattern and expr is pattern
            .k_and => {
                _ = self.nextToken();
                const expr = try self.pExpr();
                try self.expectNextToken(.k_is, "expect a `is` to specify the pattern");
                const pattern = try pPratt(self, op_info.prec + 1, .{});

                left = try self.pushNode(.{ Tag.and_is, left, expr, pattern });
                continue;
            },
            .k_if => {
                _ = self.nextToken();
                const expr = try self.pExpr();

                left = try self.pushNode(.{ Tag.if_guard, left, expr });
                continue;
            },
            else => {},
        }
        _ = self.nextToken();

        const right = try pPratt(self, op_info.prec + 1, .{});
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

fn pPrefixPattern(self: *Parser, _: Options) Err!u64 {
    errdefer self.fallback();
    const token = self.peekToken();
    return switch (token.tag) {
        .id => try self.pushRaw(.id),
        .int => try expr_module.pDigit(self),
        .str => try self.pushRaw(.str),
        .char => try self.pushRaw(.char),
        .k_true => try self.pushRaw(.bool),
        .k_false => try self.pushRaw(.bool),
        .k_null => try self.pushRaw(.null),
        .k_void => try self.pushRaw(.void),
        .k_noreturn => try self.pushRaw(.noreturn),
        .k_Any => try self.pushRaw(.Any),
        .k_any => try self.pushRaw(.any),
        .k_unit => try self.pushRaw(.unit),

        .@"." => try pRangeOrSymbol(self),

        .@"(" => try pTuplePattern(self),
        .@"{" => try pObjectPattern(self),
        .@"[" => try pListPattern(self),
        .@"<" => try pExprPattern(self),

        else => return self.invalidPattern(self.lcursor, self.rcursor + 1, "I don't know how to parse this pattern😢"),
    };
}

fn tryObjectFieldPattern(self: *Parser) Err!u64 {
    if (!self.peek(&.{ .id, .@":" }))
        return 0;
    const id = try self.pushRaw(.id);
    _ = self.nextToken();

    return try self.pushNode(.{ Tag.@"id : pattern", id, try self.pPattern() });
}

fn pTuplePattern(self: *Parser) Err!u64 {
    const rules = .{
        basic.rule("pattern", Parser.pPattern),
    };
    const nodes = try basic.pMulti(
        self,
        rules,
        .@"(",
    );
    defer nodes.deinit();
    return try self.pushNode(.{ Tag.tuple_pattern, nodes.items });
}

fn pObjectPattern(self: *Parser) Err!u64 {
    const rules = .{
        basic.rule("object field pattern", tryObjectFieldPattern),
        basic.rule("id binding", basic.tryId),
    };

    const nodes = try basic.pMulti(
        self,
        rules,
        .@"{",
    );
    defer nodes.deinit();
    return try self.pushNode(.{ Tag.object_pattern, nodes.items });
}

fn pListPattern(self: *Parser) Err!u64 {
    const rules = .{
        basic.rule("pattern", Parser.pPattern),
    };
    const nodes = try basic.pMulti(
        self,
        rules,
        .@"[",
    );
    defer nodes.deinit();
    return try self.pushNode(.{ Tag.list_pattern, nodes.items });
}

fn pExprPattern(self: *Parser) Err!u64 {
    _ = self.nextToken();
    const expr = try self.pExpr();
    try self.expectNextToken(.@">", "expect a `>` to close the pattern");
    return try self.pushNode(.{ Tag.@"< expr >", expr });
}

fn pRangeOrSymbol(self: *Parser) Err!u64 {
    _ = self.nextToken();
    if (self.eatToken(.@".")) {
        // ...id
        if (self.peek(&.{ .@".", .id })) {
            _ = self.nextToken();
            _ = self.nextToken();
            const id = try self.pushRaw(.id);

            return try self.pushNode(.{ Tag.list_rest_bind, id });
        }

        const tag = if (self.eatToken(.@"=")) Tag.range_to_inclusive else Tag.range_to;
        const pattern = try pPrefixPattern(self, .{});
        return try self.pushNode(.{ tag, pattern });
    }

    if (self.peek(&.{.id})) {
        return try self.pushRaw(.symbol);
    }
    return self.invalidPattern(self.lcursor, self.rcursor + 1, "I don't know how to parse this pattern😢");
}

pub fn pBranches(self: *Parser) Err!u64 {
    const rules = .{
        basic.rule("catch branch", tryCatchBranch),
        basic.rule("pattern branch", pPatternBranch),
    };

    const nodes = try basic.pMulti(
        self,
        rules,
        .@"{",
    );
    defer nodes.deinit();
    return try self.pushNode(.{ Tag.branches, nodes.items });
}

pub fn pPatternBranch(self: *Parser) Err!u64 {
    var pattern: u64 = 0;
    var stmt_or_block: u64 = 0;

    pattern = try pPattern(self, .{});
    try self.expectNextToken(.@"=>", "expect a `=>` to separate pattern and expr");
    stmt_or_block = try stmt.tryBlock(self);
    if (stmt_or_block == 0) {
        stmt_or_block = try self.pStmt();
    }
    return try self.pushNode(.{ Tag.pattern_branch, pattern, stmt_or_block });
}

pub fn tryCatchBranch(self: *Parser) Err!u64 {
    if (!self.eatToken(.k_catch)) return 0;
    var id: u64 = 0;
    var expr_or_block: u64 = 0;

    if (self.peek(&.{.id}))
        id = try self.pushRaw(.id)
    else
        return self.unexpectedToken("expect an error name to bind the error");

    try self.expectNextToken(.@"=>", "expect a `=>` to separate error name and expr");

    expr_or_block = try stmt.tryBlock(self);
    if (expr_or_block == 0)
        expr_or_block = try self.pExpr();

    return try self.pushNode(.{ Tag.catch_branch, id, expr_or_block });
}
