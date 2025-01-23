const std = @import("std");
const lexer = @import("../lexer/lexer.zig");
const ast = @import("ast.zig");
const parser = @import("parser.zig");
const Parser = parser.Parser;
const Err = parser.Err;
const Tag = ast.Tag;
const basic = @import("basic.zig");
const expr_module = @import("expr.zig");
const pattern_module = @import("pattern.zig");
const definition = @import("definition.zig");

// ordinary statements, these statements need to be terminated by a semicolon
pub fn pStmt(self: *Parser) Err!u64 {
    const next = self.peekToken();
    return switch (next.tag) {
        .k_use => try pUse(self),
        .k_asserts => try pAsserts(self),
        .k_break => try pBreak(self),
        .@"{" => try tryBlock(self),
        .k_continue => try pContinue(self),
        .k_return => try pReturn(self),
        .k_let, .k_const => try pDecl(self),
        .k_defer => try pDefer(self),
        .k_errdefer => try pErrdefer(self),
        .k_newtype => try pNewtype(self),
        .k_typealias => try pTypealias(self),
        else => try pExprStmt(self),
    };
}

fn pDecl(self: *Parser) Err!u64 {
    var pattern: u64 = 0;
    var type_: u64 = 0;
    var expr: u64 = 0;

    const next = self.peekToken();
    const tag = switch (next.tag) {
        .k_let => Tag.let_decl,
        .k_const => Tag.const_decl,
        else => std.debug.panic("unknown decl starter: {s}\n", .{@tagName(next.tag)}),
    };

    _ = self.nextToken();
    pattern = try self.pPattern();
    if (self.eatToken(.@":")) {
        type_ = try self.pExpr();
    }
    if (self.eatToken(.@"=")) {
        expr = try self.pExpr();
    }

    return try self.pushNode(.{ tag, pattern, type_, expr });
}

// use_term ->
//     id
//     | use_term . *
//     | use_term . id
//     | use_term as id
//     | (.)+ use_term
//     | use_term . { use_term* }
//     | @ use_term

// stmt_use ->
//     use use_term
fn pUse(self: *Parser) Err!u64 {
    _ = self.nextToken();
    return try self.pushNode(.{ Tag.use_stmt, try pUseTerm(self) });
}

const OpInfo = struct {
    prec: i8,
    tag: ast.Tag,
};

fn pUseTerm(self: *Parser) Err!u64 {
    var left = try pPrefixUseTerm(self);
    while (self.eatToken(.@".")) {
        switch (self.peekToken().tag) {
            .id => {
                const id = try self.pushRaw(.id);
                left = try self.pushNode(.{ Tag.select, left, id });
                continue;
            },
            .@"*" => {
                _ = self.nextToken();
                left = try self.pushNode(.{ Tag.select_all, left });
            },
            .@"{" => {
                const rules = .{basic.rule("use term", pUseTerm)};
                const nodes = try basic.pMulti(self, rules, .@"{");
                defer nodes.deinit();
                left = try self.pushNode(.{ Tag.select_multi, left, nodes.items });
            },
            else => {},
        }
        break;
    }

    return left;
}

fn pPrefixUseTerm(self: *Parser) Err!u64 {
    const next = self.peekToken();
    switch (next.tag) {
        .id => {
            return try self.pushRaw(.id);
        },
        .@"@" => {
            _ = self.nextToken();
            const use_term = try pPrefixUseTerm(self);
            return try self.pushNode(.{ Tag.package_path, use_term });
        },
        .@"." => {
            _ = self.nextToken();
            const use_term = try pPrefixUseTerm(self);
            return try self.pushNode(.{ Tag.super_path, use_term });
        },
        else => return self.unexpectedToken("expected a use path after `use`"),
    }
}

pub fn tryBlock(self: *Parser) Err!u64 {
    if (self.peekToken().tag != .@"{") return 0;

    const rules = .{
        basic.rule("property", basic.tryProperty),
        basic.ruleWithDelimiter("for loop", tryForLoop, null),
        basic.ruleWithDelimiter("while loop", tryWhileLoop, null),
        basic.ruleWithDelimiter("if", tryIf, null),
        basic.ruleWithDelimiter("definition", definition.tryDefinition, null),
        basic.ruleWithDelimiter("statement", Parser.pStmt, .@";"),
    };

    const nodes = try basic.pMulti(
        self,
        rules,
        .@"{",
    );
    defer nodes.deinit();
    return try self.pushNode(.{ Tag.block, nodes.items });
}

fn pAsserts(self: *Parser) Err!u64 {
    _ = self.nextToken();
    const cond = try self.pExpr();
    return try self.pushNode(.{ Tag.asserts_stmt, cond });
}

fn pBreak(self: *Parser) Err!u64 {
    _ = self.nextToken();
    var label: u64 = 0;
    var guard: u64 = 0;
    if (self.peek(&.{ .@".", .id })) {
        _ = self.nextToken();
        label = try self.pushRaw(.id);
    }
    if (self.eatToken(.k_if)) {
        guard = try self.pExpr();
    }
    return try self.pushNode(.{ Tag.break_stmt, label, guard });
}

fn pContinue(self: *Parser) Err!u64 {
    _ = self.nextToken();
    var label: u64 = 0;
    var guard: u64 = 0;
    if (self.peek(&.{ .@".", .id })) {
        _ = self.nextToken();
        label = try self.pushRaw(.id);
    }
    if (self.eatToken(.k_if)) {
        guard = try self.pExpr();
    }
    return try self.pushNode(.{ Tag.continue_stmt, label, guard });
}

fn pReturn(self: *Parser) Err!u64 {
    _ = self.nextToken();
    var value: u64 = 0;
    var guard: u64 = 0;
    self.sync();
    if (!basic.isTerminator(self.peekToken()))
        value = try self.pExpr();
    if (self.eatToken(.k_if)) {
        guard = try self.pExpr();
    }
    return try self.pushNode(.{ Tag.return_stmt, value, guard });
}

fn pDefer(self: *Parser) Err!u64 {
    _ = self.nextToken();
    const expr = try self.pExpr();
    return try self.pushNode(.{ Tag.defer_stmt, expr });
}

fn pErrdefer(self: *Parser) Err!u64 {
    _ = self.nextToken();
    const expr = try self.pExpr();
    return try self.pushNode(.{ Tag.errdefer_stmt, expr });
}

// newtype id = expr
// TODO: newtype id<T> = expr
fn pNewtype(self: *Parser) Err!u64 {
    _ = self.nextToken();
    const id = try self.pushRaw(.id);
    try self.expectNextToken(.@"=", "expect a `=` after `newtype`");
    const expr = try self.pExpr();
    return try self.pushNode(.{ Tag.newtype, id, expr });
}

// typealias id = expr
// TODO: typealias id<T> = expr
fn pTypealias(self: *Parser) Err!u64 {
    _ = self.nextToken();
    const id = try self.pushRaw(.id);
    try self.expectNextToken(.@"=", "expect a `=` after `typealias`");
    const expr = try self.pExpr();
    return try self.pushNode(.{ Tag.typealias, id, expr });
}

fn pExprStmt(self: *Parser) Err!u64 {
    const left = try self.pExpr();
    var right: u64 = 0;
    var tag: ast.Tag = Tag.expr_stmt;
    switch (self.peekToken().tag) {
        .@"=" => {
            _ = self.nextToken();
            tag = Tag.assign_stmt;
        },
        .@"+=" => {
            _ = self.nextToken();
            tag = Tag.assign_add_stmt;
        },
        .@"-=" => {
            _ = self.nextToken();
            tag = Tag.assign_sub_stmt;
        },
        .@"*=" => {
            _ = self.nextToken();
            tag = Tag.assign_mul_stmt;
        },
        .@"/=" => {
            _ = self.nextToken();
            tag = Tag.assign_div_stmt;
        },
        .@"%=" => {
            _ = self.nextToken();
            tag = Tag.assign_mod_stmt;
        },

        else => {},
    }
    if (tag == Tag.expr_stmt) {
        return try self.pushNode(.{ Tag.expr_stmt, left });
    }

    right = try self.pExpr();
    return try self.pushNode(.{ tag, left, right });
}

pub fn pModScope(self: *Parser) Err!u64 {
    const rules = .{
        basic.rule("property", basic.tryProperty),
        basic.ruleWithDelimiter("when", tryWhen, null),
        basic.ruleWithDelimiter("for loop", tryForLoop, null),
        basic.ruleWithDelimiter("while loop", tryWhileLoop, null),
        basic.ruleWithDelimiter("if", tryIf, null),
        basic.ruleWithDelimiter("definition", definition.tryDefinition, null),
        basic.ruleWithDelimiter("stmt", pStmt, .@";"),
    };

    const nodes = try basic.pMulti(self, rules, .eof);
    defer nodes.deinit();
    return try self.pushNode(.{ Tag.block, nodes.items });
}

pub fn pConditionBranch(self: *Parser) Err!u64 {
    var condition: u64 = 0;
    var expr_or_block: u64 = 0;

    condition = try self.pExpr();
    try self.expectNextToken(.@"=>", "expect a `=>` to separate condition and expr(or a block)");
    expr_or_block = try tryBlock(self);
    if (expr_or_block == 0)
        expr_or_block = try self.pExpr();

    return try self.pushNode(.{ Tag.condition_branch, condition, expr_or_block });
}

pub fn tryWhen(self: *Parser) Err!u64 {
    if (!self.eatToken(.k_when)) return 0;

    const rule = &.{
        basic.rule("condition branch", pConditionBranch),
    };
    const nodes = try basic.pMulti(self, rule, .@"{");
    defer nodes.deinit();

    return try self.pushNode(.{ Tag.when, nodes.items });
}

// if expr block (else (if | block))?
// if expr is pattern block (else block)?
// if expr is branches
pub fn tryIf(self: *Parser) Err!u64 {
    if (!self.eatToken(.k_if)) return 0;

    var cond: u64 = 0;
    var condition_branch: u64 = 0;
    var else_branch: u64 = 0;

    var is: bool = false;
    var pattern: u64 = 0;
    var branches_or_block: u64 = 0;

    cond = try expr_module.pExpr(self, .{ .no_object_call = true });

    if (self.eatToken(.k_is)) {
        is = true;
        if (self.peekToken().tag == .@"{") {
            branches_or_block = try pattern_module.pBranches(self);
        } else {
            pattern = try pattern_module.pPattern(self, .{ .no_object_call = true });
            branches_or_block = try tryBlock(self);
            if (branches_or_block == 0)
                return self.unexpectedToken("expected a block after pattern");
        }
    } else {
        condition_branch = try tryBlock(self);
        if (condition_branch == 0) {
            return self.unexpectedToken("expected a block after condition");
        }
    }

    if (self.eatToken(.k_else)) {
        else_branch = try tryIf(self);
        if (else_branch == 0) {
            else_branch = try tryBlock(self);
            if (else_branch == 0) {
                return self.unexpectedToken("expected a block or another if statement after `else`");
            }
        }
    }

    if (is and pattern != 0) {
        return try self.pushNode(.{ Tag.if_is_stmt, cond, pattern, branches_or_block, else_branch });
    }

    if (is and pattern == 0) {
        return try self.pushNode(.{ Tag.if_is_match_stmt, cond, branches_or_block });
    }

    return try self.pushNode(.{ Tag.if_stmt, cond, condition_branch, else_branch });
}

// for pattern in expr block
pub fn tryForLoop(self: *Parser) Err!u64 {
    if (!self.eatToken(.k_for)) return 0;

    var pattern: u64 = 0;
    var expr: u64 = 0;
    var block: u64 = 0;

    pattern = try self.pPattern();
    if (self.eatToken(.k_in))
        expr = try expr_module.pExpr(self, .{ .no_object_call = true })
    else
        return self.unexpectedToken("expected `in` after pattern in for loop");

    block = try tryBlock(self);
    if (block == 0)
        return self.unexpectedToken("expected a block after `for`");

    return try self.pushNode(.{ Tag.for_loop, pattern, expr, block });
}

// just like if
// while expr block
// while expr is pattern block
// while expr is branches
pub fn tryWhileLoop(self: *Parser) Err!u64 {
    if (!self.eatToken(.k_while)) return 0;

    var cond: u64 = 0;
    var condition_branch: u64 = 0;
    var is: bool = false;
    var pattern: u64 = 0;
    var branches_or_block: u64 = 0;

    cond = try expr_module.pExpr(self, .{ .no_object_call = true });

    if (self.eatToken(.k_is)) {
        is = true;
        if (self.peekToken().tag == .@"{") {
            branches_or_block = try pattern_module.pBranches(self);
        } else {
            pattern = try pattern_module.pPattern(self, .{ .no_object_call = true });
            branches_or_block = try tryBlock(self);
            if (branches_or_block == 0)
                return self.unexpectedToken("expected a block after pattern");
        }
    } else {
        condition_branch = try tryBlock(self);
        if (condition_branch == 0) {
            return self.unexpectedToken("expected a block after condition");
        }
    }

    if (is and pattern != 0) {
        return try self.pushNode(.{ Tag.while_is_loop, cond, pattern, branches_or_block });
    }

    if (is and pattern == 0) {
        return try self.pushNode(.{ Tag.while_is_match_loop, cond, branches_or_block });
    }

    return try self.pushNode(.{ Tag.while_loop, cond, condition_branch });
}
