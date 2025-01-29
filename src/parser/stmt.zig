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
pub fn tryStmt(self: *Parser) Err!u64 {
    const next = self.peekToken();
    return switch (next.tag) {
        .k_use => try tryUse(self),
        .k_asserts => try tryAsserts(self),
        .k_break => try tryBreak(self),
        .@"{" => try tryBlock(self),
        .k_continue => try tryContinue(self),
        .k_return => try tryReturn(self),
        .k_let, .k_const => try tryDecl(self),
        .k_defer => try tryDefer(self),
        .k_errdefer => try tryErrdefer(self),
        .k_newtype => try tryNewtype(self),
        .k_typealias => try tryTypealias(self),
        else => try tryExprStmt(self),
    };
}

fn tryDecl(self: *Parser) Err!u64 {
    try self.enter();
    defer self.exit();

    var pattern: u64 = 0;
    var type_: u64 = 0;
    var expr: u64 = 0;

    const next = self.peekToken();
    const tag = switch (next.tag) {
        .k_let => Tag.let_decl,
        .k_const => Tag.const_decl,
        else => return 0,
    };

    self.eatTokens(1);
    pattern = try self.tryPattern();
    if (self.eatToken(.@":"))
        type_ = try self.tryExpr();

    if (self.eatToken(.@"="))
        expr = try self.tryExpr();

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
fn tryUse(self: *Parser) Err!u64 {
    self.eatTokens(1);
    const term = try pUseTerm(self);
    if (term == 0)
        return self.unexpectedToken("expected a use term after `use`");
    return try self.pushNode(.{ Tag.use_stmt, term });
}

const OpInfo = struct {
    prec: i8,
    tag: ast.Tag,
};

fn pUseTerm(self: *Parser) Err!u64 {
    try self.enter();
    defer self.exit();
    var left = try tryPrefixUseTerm(self);
    if (left == 0) return 0;

    while (self.eatToken(.@".")) {
        switch (self.peekToken().tag) {
            .id => {
                const id = try self.pushAtom(.id);
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

fn tryPrefixUseTerm(self: *Parser) Err!u64 {
    try self.enter();
    defer self.exit();
    const next = self.peekToken();
    switch (next.tag) {
        .id => {
            return try self.pushAtom(.id);
        },
        .@"@" => {
            _ = self.nextToken();
            const use_term = try tryPrefixUseTerm(self);
            return try self.pushNode(.{ Tag.package_path, use_term });
        },
        .@"." => {
            _ = self.nextToken();
            const use_term = try tryPrefixUseTerm(self);
            return try self.pushNode(.{ Tag.super_path, use_term });
        },
        else => return 0,
    }
}

pub fn tryBlock(self: *Parser) Err!u64 {
    try self.enter();
    defer self.exit();
    if (self.peekToken().tag != .@"{") return 0;

    const rules = .{
        basic.rule("property", basic.tryProperty),
        basic.ruleWithDelimiter("for loop", tryForLoop, null),
        basic.ruleWithDelimiter("while loop", tryWhileLoop, null),
        basic.ruleWithDelimiter("if", tryIf, null),
        basic.ruleWithDelimiter("when", tryWhen, null),
        basic.ruleWithDelimiter("definition", definition.tryDefinition, null),
        basic.ruleWithDelimiter("statement", Parser.tryStmt, .@";"),
    };

    const nodes = try basic.pMulti(self, rules, .@"{");
    defer nodes.deinit();
    return try self.pushNode(.{ Tag.block, nodes.items });
}

fn tryAsserts(self: *Parser) Err!u64 {
    try self.enter();
    defer self.exit();
    if (!self.eatToken(.k_asserts)) return 0;

    const cond = try self.tryExpr();
    if (cond == 0)
        return self.unexpectedToken("expected an expression after `asserts`");
    return try self.pushNode(.{ Tag.asserts_stmt, cond });
}

fn tryBreak(self: *Parser) Err!u64 {
    try self.enter();
    defer self.exit();
    if (!self.eatToken(.k_break)) return 0;

    var label: u64 = 0;
    var guard: u64 = 0;
    label = try basic.trySymbol(self);

    if (self.eatToken(.k_if)) {
        guard = try self.tryExpr();
        if (guard == 0)
            return self.unexpectedToken("expected a guard expression after `if`");
    }
    return try self.pushNode(.{ Tag.break_stmt, label, guard });
}

fn tryContinue(self: *Parser) Err!u64 {
    try self.enter();
    defer self.exit();
    if (!self.eatToken(.k_continue)) return 0;

    var label: u64 = 0;
    var guard: u64 = 0;
    label = try basic.trySymbol(self);

    if (self.eatToken(.k_if)) {
        guard = try self.tryExpr();
        if (guard == 0)
            return self.unexpectedToken("expected a guard expression after `if`");
    }
    return try self.pushNode(.{ Tag.continue_stmt, label, guard });
}

fn tryReturn(self: *Parser) Err!u64 {
    try self.enter();
    defer self.exit();
    if (!self.eatToken(.k_return)) return 0;

    var value: u64 = 0;
    var guard: u64 = 0;
    if (!basic.isTerminator(self.peekToken()))
        value = try self.tryExpr();
    if (self.eatToken(.k_if)) {
        guard = try self.tryExpr();
    }
    return try self.pushNode(.{ Tag.return_stmt, value, guard });
}

fn tryDefer(self: *Parser) Err!u64 {
    try self.enter();
    defer self.exit();
    if (!self.eatToken(.k_defer)) return 0;

    const expr = try self.tryExpr();
    if (expr == 0)
        return self.unexpectedToken("expected an expression after `defer`");
    return try self.pushNode(.{ Tag.defer_stmt, expr });
}

fn tryErrdefer(self: *Parser) Err!u64 {
    try self.enter();
    defer self.exit();
    if (!self.eatToken(.k_errdefer)) return 0;

    const expr = try self.tryExpr();
    if (expr == 0)
        return self.unexpectedToken("expected an expression after `errdefer`");
    return try self.pushNode(.{ Tag.errdefer_stmt, expr });
}

// newtype id = expr
// TODO: newtype id<T> = expr
fn tryNewtype(self: *Parser) Err!u64 {
    try self.enter();
    defer self.exit();
    if (!self.eatToken(.k_newtype)) return 0;

    const id = try basic.tryId(self);
    try self.expectNextToken(.@"=", "expect a `=` after `newtype`");
    const expr = try self.tryExpr();
    return try self.pushNode(.{ Tag.newtype, id, expr });
}

// typealias id = expr
// TODO: typealias id<T> = expr
fn tryTypealias(self: *Parser) Err!u64 {
    try self.enter();
    defer self.exit();
    if (!self.eatToken(.k_typealias)) return 0;

    const id = try self.pushAtom(.id);
    try self.expectNextToken(.@"=", "expect a `=` after `typealias`");
    const expr = try self.tryExpr();
    return try self.pushNode(.{ Tag.typealias, id, expr });
}

fn tryExprStmt(self: *Parser) Err!u64 {
    const left = try self.tryExpr();
    var right: u64 = 0;
    var tag: ast.Tag = Tag.expr_stmt;
    switch (self.peekToken().tag) {
        .@"=" => {
            self.eatTokens(1);
            tag = Tag.assign_stmt;
        },
        .@"+=" => {
            self.eatTokens(1);
            tag = Tag.assign_add_stmt;
        },
        .@"-=" => {
            self.eatTokens(1);
            tag = Tag.assign_sub_stmt;
        },
        .@"*=" => {
            self.eatTokens(1);
            tag = Tag.assign_mul_stmt;
        },
        .@"/=" => {
            self.eatTokens(1);
            tag = Tag.assign_div_stmt;
        },
        .@"%=" => {
            self.eatTokens(1);
            tag = Tag.assign_mod_stmt;
        },

        else => {},
    }
    if (tag == Tag.expr_stmt) {
        return try self.pushNode(.{ Tag.expr_stmt, left });
    }

    right = try self.tryExpr();
    return try self.pushNode(.{ tag, left, right });
}

pub fn pModScope(self: *Parser) Err!u64 {
    try self.enter();
    defer self.exit();
    const rules = .{
        basic.rule("property", basic.tryProperty),
        basic.ruleWithDelimiter("when", tryWhen, null),
        basic.ruleWithDelimiter("for loop", tryForLoop, null),
        basic.ruleWithDelimiter("while loop", tryWhileLoop, null),
        basic.ruleWithDelimiter("if", tryIf, null),
        basic.ruleWithDelimiter("definition", definition.tryDefinition, null),
        basic.ruleWithDelimiter("stmt", tryStmt, .@";"),
    };

    const nodes = try basic.pMulti(self, rules, .eof);
    defer nodes.deinit();
    return try self.pushNode(.{ Tag.block, nodes.items });
}

pub fn tryConditionBranch(self: *Parser) Err!u64 {
    try self.enter();
    defer self.exit();

    var condition: u64 = 0;
    var stmt_or_block: u64 = 0;

    condition = try self.tryExpr();
    try self.expectNextToken(.@"=>", "expect a `=>` to separate condition and expr(or a block)");
    stmt_or_block = try tryBlock(self);
    if (stmt_or_block == 0)
        stmt_or_block = try self.tryStmt();

    return try self.pushNode(.{ Tag.condition_branch, condition, stmt_or_block });
}

pub fn tryWhen(self: *Parser) Err!u64 {
    try self.enter();
    defer self.exit();
    if (!self.eatToken(.k_when)) return 0;

    const rule = &.{
        basic.rule("condition branch", tryConditionBranch),
    };
    const nodes = try basic.pMulti(self, rule, .@"{");
    defer nodes.deinit();

    return try self.pushNode(.{ Tag.when, nodes.items });
}

// if expr block (else (if | block))?
// if expr is pattern block (else block)?
// if expr is branches
pub fn tryIf(self: *Parser) Err!u64 {
    try self.enter();
    defer self.exit();
    if (!self.eatToken(.k_if)) return 0;

    var cond: u64 = 0;
    var condition_branch: u64 = 0;
    var else_branch: u64 = 0;

    var is: bool = false;
    var pattern: u64 = 0;
    var branches_or_block: u64 = 0;

    cond = try expr_module.tryExpr(self, .{ .no_object_call = true });

    if (self.eatToken(.k_is)) {
        is = true;
        if (self.peekToken().tag == .@"{") {
            branches_or_block = try pattern_module.tryBranches(self);
        } else {
            pattern = try pattern_module.tryPattern(self, .{ .no_object_call = true });
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

    pattern = try self.tryPattern();
    if (self.eatToken(.k_in))
        expr = try expr_module.tryExpr(self, .{ .no_object_call = true })
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
    try self.enter();
    defer self.exit();
    if (!self.eatToken(.k_while)) return 0;

    var cond: u64 = 0;
    var condition_branch: u64 = 0;
    var is: bool = false;
    var pattern: u64 = 0;
    var branches_or_block: u64 = 0;

    cond = try expr_module.tryExpr(self, .{ .no_object_call = true });

    if (self.eatToken(.k_is)) {
        is = true;
        if (self.peekToken().tag == .@"{") {
            branches_or_block = try pattern_module.tryBranches(self);
        } else {
            pattern = try pattern_module.tryPattern(self, .{ .no_object_call = true });
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
