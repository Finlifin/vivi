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
};

pub fn pExpr(self: *Parser, opt: Options) Err!u64 {
    return try pPratt(self, 0, opt);
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

fn pPratt(self: *Parser, min_prec: i8, opt: Options) Err!u64 {
    var left = try pPrefixExpr(self);
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
                    basic.rule("expr", Parser.pExpr),
                };

                const nodes = try basic.pMulti(
                    self,
                    rules,
                    .@"(",
                );
                defer nodes.deinit();
                left = try self.pushNode(.{ Tag.call, left, nodes.items });

                continue;
            },
            .@"<" => {
                const rules = .{
                    basic.rule("property", basic.tryProperty),
                    basic.rule("expr", Parser.pExpr),
                };

                const nodes = try basic.pMulti(
                    self,
                    rules,
                    .@"<",
                );
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
                    basic.rule("expr", Parser.pExpr),
                };

                const nodes = try basic.pMulti(
                    self,
                    rules,
                    .@"{",
                );
                defer nodes.deinit();
                left = try self.pushNode(.{ Tag.object_call, left, nodes.items });

                continue;
            },
            .@"[" => {
                _ = self.nextToken();
                const index_expr = try self.pExpr();
                try self.expectNextToken(.@"]", "expect a `]` to close index call");
                left = try self.pushNode(.{ Tag.index_call, left, index_expr });
                continue;
            },
            .@"#" => {
                _ = self.nextToken();
                switch (self.peekToken().tag) {
                    .@"{" => {
                        const branches = try pattern.pBranches(self);
                        left = try self.pushNode(.{ Tag.effect_elimination, left, branches });
                        continue;
                    },
                    .k_use => {
                        _ = self.nextToken();
                        const expr = try pPrefixExpr(self);
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
                _ = self.nextToken();
                switch (self.peekToken().tag) {
                    .@"{" => {
                        const branches = try pattern.pBranches(self);
                        left = try self.pushNode(.{ Tag.error_elimination, left, branches });
                        continue;
                    },
                    .k_use => {
                        _ = self.nextToken();
                        const expr = try pPrefixExpr(self);
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
                _ = self.nextToken();
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
                _ = self.nextToken();

                const branches = try pattern.pBranches(self);
                left = try self.pushNode(.{ Tag.match, left, branches });
                continue;
            },
            // it might be a select, @"pattern . *", range_from, range_from_to, range_from_to_inclusive
            .@"." => {
                _ = self.nextToken();
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
                    const expr = try pPrefixExpr(self);

                    left = try self.pushNode(.{ tag, left, expr });
                    continue;
                }

                return self.invalidPattern(self.lcursor, self.rcursor, "unable to recognize this pattern😢");
            },

            .@"'" => {
                _ = self.nextToken();
                const id = try self.pushRaw(.id);
                left = try self.pushNode(.{ Tag.image, left, id });
                continue;
            },
            else => {},
        }
        _ = self.nextToken();

        const right = try pPratt(self, op_info.prec + 1, opt);
        if (right == 0) {
            return self.unexpectedToken("expect an expression while parsing pratt");
        }

        left = try self.pushNode(.{ op_info.tag, left, right });
    }

    return left;
}

fn pPrefixExpr(self: *Parser) Err!u64 {
    const token = self.peekToken();
    return switch (token.tag) {
        .id => try self.pushRaw(.id),
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

        .k_not => try pBoolNot(self),
        .int => try pDigit(self),
        .@"." => try pSymbol(self),
        .k_derive => try pDerive(self),
        .k_if => try stmt.tryIf(self),
        .k_when => try stmt.tryWhen(self),

        .@"{" => try pObject(self),
        .@"[" => try pList(self),
        .@"(" => try pQuoteOrTuple(self),

        .@"<" => try pPatternExpr(self),

        .@"|" => try pLambda(self),
        .k_do => try pDoBlock(self),
        .@"?" => try pOptionType(self),
        .@"#" => try pEffectType(self),
        .@"!" => try pErrorType(self),

        .k_struct,
        .k_impl,
        .k_fn,
        .k_enum,
        .k_union,
        .k_mod,
        .k_trait,
        => try definition.tryDefinition(self),

        else => return self.invalidExpr(self.lcursor, self.rcursor, "unable to recognize it as an expr😢"),
    };
}

fn pBoolNot(self: *Parser) Err!u64 {
    const expr = try pPrefixExpr(self);
    return self.pushNode(.{ Tag.bool_not, expr });
}

fn pDerive(self: *Parser) Err!u64 {
    _ = self.nextToken();
    var traits = std.ArrayList(u64).init(self.tmp_alc.allocator());
    defer traits.deinit();
    while (true) {
        const trait = try self.pExpr();
        try traits.append(trait);
        if (!self.eatToken(.@",")) break;
    }
    try self.expectNextToken(.k_for, "expect a `for` to specify the derive type");
    const derive_type = try self.pExpr();
    return self.pushNode(.{ Tag.derivation, derive_type, traits.items });
}

pub fn pDigit(self: *Parser) Err!u64 {
    if (self.peek(&.{ .int, .@".", .int })) {
        const result = try self.pushRaw(.real);
        _ = self.nextToken();
        _ = self.nextToken();
        return result;
    }
    return try self.pushRaw(.int);
}

fn pSymbol(self: *Parser) Err!u64 {
    _ = self.nextToken();
    if (!self.eatToken(.id))
        return self.invalidExpr(self.lcursor, self.rcursor + 1, "expected a identifier for a symbol expression");

    return self.pushNode(.{ Tag.symbol, self.rcursor });
}

fn pObject(self: *Parser) Err!u64 {
    const rules = .{
        basic.rule("property", basic.tryProperty),
        basic.rule("expr", Parser.pExpr),
    };

    const nodes = try basic.pMulti(
        self,
        rules,
        .@"{",
    );
    defer nodes.deinit();
    return try self.pushNode(.{ Tag.object, nodes.items });
}

fn pList(self: *Parser) Err!u64 {
    const rules = .{
        basic.rule("expr", Parser.pExpr),
    };

    const nodes = try basic.pMulti(
        self,
        rules,
        .@"[",
    );
    defer nodes.deinit();
    return try self.pushNode(.{ Tag.list, nodes.items });
}

pub fn pQuoteOrTuple(self: *Parser) Err!u64 {
    const rules = .{
        basic.rule("expr", Parser.pExpr),
    };

    const nodes = try basic.pMulti(
        self,
        rules,
        .@"(",
    );
    defer nodes.deinit();
    const tag = if (nodes.items.len == 1) Tag.quote else Tag.tuple;

    return try self.pushNode(.{ tag, nodes.items });
}

fn pPatternExpr(self: *Parser) Err!u64 {
    _ = self.nextToken();
    self.sync();
    const expr = try self.pPattern();
    try self.expectNextToken(.@">", "expected a `>` to close the pattern expr");
    return try self.pushNode(.{ Tag.@"< pattern >", expr });
}

fn pDoBlock(self: *Parser) Err!u64 {
    _ = self.nextToken();
    return self.pushNode(.{ Tag.do_block, try stmt.tryBlock(self) });
}

// |args| (->expr)? clauses? (block | expr)
fn pLambda(self: *Parser) Err!u64 {
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
        return_type = try self.pExpr();

    clauses = try definition.tryClauses(self);

    block_or_expr = try stmt.tryBlock(self);
    if (block_or_expr == 0)
        block_or_expr = try self.pExpr();

    return try self.pushNode(.{ Tag.lambda, return_type, clauses, block_or_expr, args.items });
}

fn pOptionType(self: *Parser) Err!u64 {
    _ = self.nextToken();
    const expr = try self.pExpr();
    return try self.pushNode(.{ Tag.@"?expr", expr });
}

// #expr expr
fn pEffectType(self: *Parser) Err!u64 {
    _ = self.nextToken();
    const left = try pExpr(self, .{});
    const right = try pExpr(self, .{ .no_object_call = true });
    return try self.pushNode(.{ Tag.@"#expr expr", left, right });
}

// !expr expr
fn pErrorType(self: *Parser) Err!u64 {
    _ = self.nextToken();
    const left = try pExpr(self, .{});
    const right = try pExpr(self, .{ .no_object_call = true });
    return try self.pushNode(.{ Tag.@"!expr expr", left, right });
}
