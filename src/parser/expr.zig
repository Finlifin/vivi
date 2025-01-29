const std = @import("std");
const lexer = @import("../lexer/lexer.zig");
const ast = @import("ast.zig");
const parser = @import("parser.zig");
const Parser = parser.Parser;
const Err = parser.Err;
const Tag = ast.Tag;
const basic = @import("basic.zig");
const stmt = @import("stmt.zig");
const pattern = @import("pattern.zig");
const definition = @import("definition.zig");

pub const Options = struct {
    no_object_call: bool = false,
    inside_pratt: bool = false,
};

pub fn tryExpr(self: *Parser, opt: Options) Err!u64 {
    return try tryPratt(self, 0, opt);
}

const OpInfo = struct {
    prec: i8,
    tag: ast.Tag,
    assoc_left: bool = true,
};

const op_table = std.enums.directEnumArrayDefault(
    lexer.Token.Tag,
    OpInfo,
    .{ .prec = -1, .tag = .invalid },
    0,
    .{
        // .@"==>" = .{ .prec = 10, .tag = .bool_implies },

        .k_or = .{ .prec = 20, .tag = .bool_or },

        .k_and = .{ .prec = 30, .tag = .bool_and },

        .@"!=" = .{ .prec = 40, .tag = .bool_not_eq },
        .@"==" = .{ .prec = 40, .tag = .bool_eq },
        .@">=" = .{ .prec = 40, .tag = .bool_gt_eq },
        .@" > " = .{ .prec = 40, .tag = .bool_gt },
        .@"<=" = .{ .prec = 40, .tag = .bool_lt_eq },
        .@" < " = .{ .prec = 40, .tag = .bool_lt },
        .@":" = .{ .prec = 40, .tag = .bool_typed_with },
        .@":-" = .{ .prec = 40, .tag = .bool_implements },

        .@" + " = .{ .prec = 60, .tag = .add },
        .@" - " = .{ .prec = 60, .tag = .sub },

        .@" / " = .{ .prec = 70, .tag = .div },
        .@" * " = .{ .prec = 70, .tag = .mul },
        .@" % " = .{ .prec = 70, .tag = .mod },
        .@"++" = .{ .prec = 70, .tag = .add_add },

        .@"|" = .{ .prec = 80, .tag = .pipe },
        .@"|>" = .{ .prec = 80, .tag = .pipe_first },

        .@"(" = .{ .prec = 90, .tag = .call },
        .@"[" = .{ .prec = 90, .tag = .index_call },
        .@"{" = .{ .prec = 90, .tag = .object_call },
        .@"<" = .{ .prec = 90, .tag = .diamond_call },
        .@"#" = .{ .prec = 90, .tag = .effect_elimination },
        .@"!" = .{ .prec = 90, .tag = .error_elimination },
        .@"?" = .{ .prec = 90, .tag = .option_elimination },
        .k_match = .{ .prec = 90, .tag = .match },

        .@"." = .{ .prec = 100, .tag = .select },
        .@"'" = .{ .prec = 100, .tag = .image },
    },
);

fn tryPratt(self: *Parser, min_prec: i8, opt: Options) Err!u64 {
    var left = try tryPrefixExpr(self);
    if (left == 0) {
        return 0;
    }

    while (true) {
        const token = self.peekToken();
        const op_info = op_table[@intFromEnum(token.tag)];

        if (op_info.tag == .invalid or op_info.prec < min_prec)
            break;

        switch (token.tag) {
            .@"(" => {
                const rules = .{
                    basic.rule("property", basic.tryProperty),
                    basic.rule("expr", Parser.tryExpr),
                };

                const nodes = try basic.pMulti(self, rules, .@"(");
                defer nodes.deinit();
                left = try self.pushNode(.{ Tag.call, left, nodes.items });

                continue;
            },
            .@"<" => {
                const rules = .{
                    basic.rule("property", basic.tryProperty),
                    basic.rule("expr", Parser.tryExpr),
                };

                const nodes = try basic.pMulti(self, rules, .@"<");
                defer nodes.deinit();
                left = try self.pushNode(.{ Tag.diamond_call, left, nodes.items });

                continue;
            },
            .@"{" => {
                if (opt.no_object_call) {
                    break;
                }
                const rules = .{
                    basic.rule("property", basic.tryProperty),
                    basic.rule("expr", Parser.tryExpr),
                };

                const nodes = try basic.pMulti(self, rules, .@"{");
                defer nodes.deinit();
                left = try self.pushNode(.{ Tag.object_call, left, nodes.items });

                continue;
            },
            .@"[" => {
                self.eatTokens(1);
                const index_expr = try self.tryExpr();
                try self.expectNextToken(.@"]", "expect a `]` to close index call");
                left = try self.pushNode(.{ Tag.index_call, left, index_expr });
                continue;
            },
            .@"#" => {
                self.eatTokens(1);
                switch (self.peekToken().tag) {
                    .@"{" => {
                        const branches = try pattern.tryBranches(self);
                        left = try self.pushNode(.{ Tag.effect_elimination, left, branches });
                        continue;
                    },
                    .k_use => {
                        _ = self.nextToken();
                        const expr = try tryPrefixExpr(self);
                        left = try self.pushNode(.{ Tag.effect_elimination_use, left, expr });
                        continue;
                    },
                    else => {
                        left = try self.pushNode(.{ Tag.effect_elimination_unwrap, left });
                        continue;
                    },
                }
            },
            .@"!" => {
                self.eatTokens(1);
                std.debug.print("DEBUG: {any}\n", .{self.peekToken()});
                switch (self.peekToken().tag) {
                    .@"{" => {
                        const branches = try pattern.tryBranches(self);
                        left = try self.pushNode(.{ Tag.error_elimination, left, branches });
                        continue;
                    },
                    .k_use => {
                        self.eatTokens(1);
                        const expr = try tryPrefixExpr(self);
                        left = try self.pushNode(.{ Tag.error_elimination_use, left, expr });
                        continue;
                    },
                    else => {
                        left = try self.pushNode(.{ Tag.error_elimination_unwrap, left });
                        continue;
                    },
                }
            },
            .@"?" => {
                self.eatTokens(1);
                var block: u64 = 0;
                block = try stmt.tryBlock(self);

                if (block != 0) {
                    left = try self.pushNode(.{ Tag.option_elimination, left, block });
                    continue;
                }
                left = try self.pushNode(.{ Tag.option_elimination_unwrap, left });
                continue;
            },
            .k_match => {
                self.eatTokens(1);
                const branches = try pattern.tryBranches(self);
                left = try self.pushNode(.{ Tag.match, left, branches });
                continue;
            },
            // it might be a select, @"pattern . *", range_from, range_from_to, range_from_to_inclusive
            .@"." => {
                try self.enter();
                defer self.exit();
                self.eatTokens(1);
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
                    const expr = try tryPrefixExpr(self);

                    left = try self.pushNode(.{ tag, left, expr });
                    continue;
                }

                return self.invalidExpr(self.rcursor, self.rcursor, "expect an identifier or a range after a `.`");
            },

            .@"'" => {
                _ = self.nextToken();
                const id = try self.pushAtom(.id);
                left = try self.pushNode(.{ Tag.image, left, id });
                continue;
            },
            else => {},
        }
        _ = self.nextToken();
        var opt_ = opt;
        opt_.inside_pratt = true;
        const right = try tryPratt(self, op_info.prec + 1, opt);
        if (right == 0)
            return self.invalidExpr(self.rcursor, self.rcursor, "expect an expression after a binary operator");

        left = try self.pushNode(.{ op_info.tag, left, right });
    }

    return left;
}

fn tryPrefixExpr(self: *Parser) Err!u64 {
    try self.enter();
    defer self.exit();

    const token = self.peekToken();
    return switch (token.tag) {
        .id => try basic.tryId(self),
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

        .k_not => try tryBoolNot(self),
        .int => try tryDigit(self),
        .@"." => try basic.trySymbol(self),
        .k_derive => try tryDerive(self),
        .k_if => try stmt.tryIf(self),
        .k_when => try stmt.tryWhen(self),

        .@"{" => try tryObject(self),
        .@"[" => try tryList(self),
        .@"(" => try tryQuoteOrTuple(self),

        .@"<" => try tryPatternExpr(self),

        .@"|" => try tryLambda(self),
        .k_do => try tryDoBlock(self),
        .@"?" => try tryOptionType(self),
        .@"#" => try tryEffectType(self),
        .@"!" => try tryErrorType(self),

        .k_struct,
        .k_impl,
        .k_fn,
        .k_enum,
        .k_union,
        .k_mod,
        .k_trait,
        => try definition.tryDefinition(self),

        else => 0,
    };
}

fn tryBoolNot(self: *Parser) Err!u64 {
    try self.enter();
    defer self.exit();
    if (!self.eatToken(.k_not)) return 0;

    self.eatTokens(1);
    const expr = try tryPrefixExpr(self);
    return self.pushNode(.{ Tag.bool_not, expr });
}

fn tryDerive(self: *Parser) Err!u64 {
    try self.enter();
    defer self.exit();
    if (!self.eatToken(.k_derive)) return 0;

    var traits = std.ArrayList(u64).init(self.tmp_alc.allocator());
    defer traits.deinit();
    while (true) {
        const trait = try self.tryExpr();
        try traits.append(trait);
        if (!self.eatToken(.@",")) break;
    }
    try self.expectNextToken(.k_for, "expect a `for` to specify the derive type");
    const derive_type = try self.tryExpr();
    const clauses = try definition.tryClauses(self);
    return self.pushNode(.{ Tag.derivation, derive_type, clauses, traits.items });
}

pub fn tryDigit(self: *Parser) Err!u64 {
    try self.enter();
    defer self.exit();
    if (self.peek(&.{ .int, .@".", .int })) {
        const result = try self.pushReal();
        return result;
    }
    return try self.pushAtom(.int);
}

fn tryObject(self: *Parser) Err!u64 {
    try self.enter();
    defer self.exit();
    if (!self.peek(&.{.@"{"})) return 0;

    const rules = .{
        basic.rule("property", basic.tryProperty),
        basic.rule("expr", Parser.tryExpr),
    };

    const nodes = try basic.pMulti(self, rules, .@"{");
    defer nodes.deinit();
    return try self.pushNode(.{ Tag.object, nodes.items });
}

fn tryList(self: *Parser) Err!u64 {
    try self.enter();
    defer self.exit();
    if (!self.peek(&.{.@"["})) return 0;

    const rules = .{
        basic.rule("expr", Parser.tryExpr),
    };

    const nodes = try basic.pMulti(self, rules, .@"[");
    defer nodes.deinit();
    return try self.pushNode(.{ Tag.list, nodes.items });
}

pub fn tryQuoteOrTuple(self: *Parser) Err!u64 {
    const rules = .{
        basic.rule("expr", Parser.tryExpr),
    };

    const nodes = try basic.pMulti(self, rules, .@"(");
    defer nodes.deinit();
    const tag = if (nodes.items.len == 1) Tag.quote else Tag.tuple;

    return try self.pushNode(.{ tag, nodes.items });
}

fn tryPatternExpr(self: *Parser) Err!u64 {
    try self.enter();
    defer self.exit();
    if (!self.eatToken(.@"<")) return 0;

    const expr = try self.tryPattern();
    try self.expectNextToken(.@">", "expected a `>` to close the pattern expr");
    return try self.pushNode(.{ Tag.@"< pattern >", expr });
}

fn tryDoBlock(self: *Parser) Err!u64 {
    _ = self.nextToken();
    return self.pushNode(.{ Tag.do_block, try stmt.tryBlock(self) });
}

// |args| (->expr)? clauses? (block | expr)
fn tryLambda(self: *Parser) Err!u64 {
    try self.enter();
    defer self.exit();
    if (!self.peek(&.{.@"|"})) return 0;

    var return_type: u64 = 0;
    var clauses: u64 = 0;
    var block_or_expr: u64 = 0;

    const rules = .{
        basic.rule("self", definition.trySelf),
        basic.rule("rest parameters", definition.tryRestParams),
        basic.rule("optional trait refined parameters", definition.tryOptionalTraitParam),
        basic.rule("optional parameter", definition.tryOptionalParam),
        basic.rule("trait refined parameters", definition.tryTraitParam),
        basic.rule("parameter", definition.pParam),
    };
    const args = try basic.pMulti(self, rules, .@"|");
    defer args.deinit();

    if (self.eatToken(.@"->"))
        return_type = try self.tryExpr();

    clauses = try definition.tryClauses(self);

    block_or_expr = try stmt.tryBlock(self);
    if (block_or_expr == 0)
        block_or_expr = try self.tryExpr();

    return try self.pushNode(.{ Tag.lambda, return_type, clauses, block_or_expr, args.items });
}

fn tryOptionType(self: *Parser) Err!u64 {
    try self.enter();
    defer self.exit();
    if (!self.eatToken(.@"?")) return 0;

    const expr = try self.tryExpr();
    return try self.pushNode(.{ Tag.@"?expr", expr });
}

// #expr expr
fn tryEffectType(self: *Parser) Err!u64 {
    try self.enter();
    defer self.exit();
    if (!self.eatToken(.@"#")) return 0;

    const left = try tryExpr(self, .{});
    const right = try tryExpr(self, .{ .no_object_call = true });
    return try self.pushNode(.{ Tag.@"#expr expr", left, right });
}

// !expr expr
fn tryErrorType(self: *Parser) Err!u64 {
    try self.enter();
    defer self.exit();
    if (!self.eatToken(.@"!")) return 0;

    const left = try tryExpr(self, .{});
    const right = try tryExpr(self, .{ .no_object_call = true });
    return try self.pushNode(.{ Tag.@"!expr expr", left, right });
}
