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

pub fn tryDefinition(self: *Parser) Err!u64 {
    try self.enter();
    defer self.exit();

    const next = self.peekToken();
    return switch (next.tag) {
        .k_fn => tryFn(self),
        .k_effect => tryEffect(self),
        .k_impl => tryImpl(self),
        .k_pub => tryPub(self),
        .k_struct => tryStruct(self),
        .k_trait => tryTrait(self),
        .k_enum => tryEnum(self),
        .k_union => tryUnion(self),
        .k_mod => tryMod(self),
        else => 0,
    };
}

// impl expr (for expr)? clauses? block
pub fn tryImpl(self: *Parser) Err!u64 {
    try self.enter();
    defer self.exit();
    if (!self.eatToken(.k_impl)) return 0;

    var left: u64 = 0;
    var right: u64 = 0;
    var clauses: u64 = 0;
    var block: u64 = 0;
    left = try self.tryExpr();
    if (self.eatToken(.k_for)) {
        right = try expr_module.tryExpr(self, .{ .no_object_call = true });
    }
    clauses = try tryClauses(self);
    block = try stmt.tryBlock(self);
    if (block == 0)
        return self.unexpectedToken("expect a block after `impl`");

    return try self.pushNode(.{ Tag.impl_def, left, right, clauses, block });
}

pub fn tryPub(self: *Parser) Err!u64 {
    try self.enter();
    defer self.exit();
    if (!self.eatToken(.k_pub)) return 0;
    return try self.pushNode(.{ Tag.@"pub stmt", try tryDefinition(self) });
}

pub fn tryStruct(self: *Parser) Err!u64 {
    try self.enter();
    defer self.exit();
    if (!self.eatToken(.k_struct)) return 0;
    var id: u64 = 0;
    var clauses: u64 = 0;
    var definition: u64 = 0;

    if (self.peek(&.{.id}))
        id = try self.pushAtom(.id);

    clauses = try tryClauses(self);
    definition = try tryStructDefBody(self);

    return try self.pushNode(.{ Tag.struct_def, id, clauses, definition });
}

pub fn tryStructDefBody(self: *Parser) Err!u64 {
    try self.enter();
    defer self.exit();
    if (!self.peek(&.{.@"{"})) return 0;

    const rules = .{
        basic.rule("property", basic.tryProperty),
        basic.ruleWithDelimiter("definition", tryDefinition, null),
        basic.rule("struct field", tryStructField),
        basic.ruleWithDelimiter("statement", Parser.tryStmt, .@";"),
    };

    const nodes = try basic.pMulti(self, rules, .@"{");
    defer nodes.deinit();

    return try self.pushNode(.{ Tag.struct_def_body, nodes.items });
}

pub fn tryStructField(self: *Parser) Err!u64 {
    try self.enter();
    defer self.exit();
    if (!self.peek(&.{ .id, .@":" })) return 0;

    var id: u64 = 0;
    var type_expr: u64 = 0;
    var default_value_expr: u64 = 0;

    id = try self.pushAtom(.id);
    if (!self.eatToken(.@":")) return self.unexpectedToken("expect a type for struct field");
    type_expr = try self.tryExpr();
    if (self.eatToken(.@"="))
        default_value_expr = try self.tryExpr();

    return try self.pushNode(.{ Tag.struct_field, id, type_expr, default_value_expr });
}

// 写得一坨，有机会再重构
// clauses -> where clause*
pub fn tryClauses(self: *Parser) Err!u64 {
    try self.enter();
    defer self.exit();
    if (!self.eatToken(.k_where)) return 0;

    var nodes = std.ArrayList(u64).init(self.tmp_alc.allocator());
    defer nodes.deinit();

    // id (: expr)? (= expr')?
    // .id (: expr)? (= expr')?
    // id :- expr
    // .id :- expr
    // requires expr
    while (true) {
        if (self.peek(&.{ .id, .@":-" })) {
            const id = try basic.tryId(self);
            self.eatTokens(1);
            const trait_bound = try self.tryExpr();
            if (trait_bound == 0) return self.unexpectedToken("expected a trait bound expression after `:-`");
            try nodes.append(try self.pushNode(.{ Tag.trait_bound_clause, id, trait_bound }));
        } else if (self.peek(&.{ .@".", .id, .@":-" })) {
            self.eatTokens(1);
            const id = try basic.tryId(self);
            self.eatTokens(1);
            const trait_bound = try self.tryExpr();
            if (trait_bound == 0) return self.unexpectedToken("expected a trait bound expression after `:-`");
            try nodes.append(try self.pushNode(.{ Tag.optional_trait_bound_clause, id, trait_bound }));
        } else if (self.peek(&.{.id})) {
            const id = try self.pushAtom(.id);
            var type_expr: u64 = 0;
            var value_expr: u64 = 0;
            if (self.eatToken(.@":")) {
                type_expr = try self.tryExpr();
            }
            if (self.eatToken(.@"=")) {
                value_expr = try self.tryExpr();
            }
            try nodes.append(try self.pushNode(.{ Tag.clause, id, type_expr, value_expr }));
        } else if (self.peek(&.{ .@".", .id })) {
            _ = self.nextToken();
            const symbol = try self.pushAtom(.symbol);
            var type_expr: u64 = 0;
            var value_expr: u64 = 0;
            if (self.eatToken(.@":")) {
                type_expr = try self.tryExpr();
            }
            if (self.eatToken(.@"=")) {
                value_expr = try self.tryExpr();
            }
            try nodes.append(try self.pushNode(.{ Tag.clause, symbol, type_expr, value_expr }));
            if (self.peek(&.{.@"{"})) break;
        } else {
            if (!self.eatToken(.k_requires)) break;
            const expr = try expr_module.tryExpr(self, .{ .no_object_call = true });
            if (expr == 0) return self.unexpectedToken("expected a predicate expression");
            try nodes.append(try self.pushNode(.{ Tag.requires_predicate, expr }));
        }

        if (!self.eatToken(.@",")) break;
    }

    return try self.pushNode(.{ Tag.clauses, nodes.items });
}

// self
pub fn trySelf(self: *Parser) Err!u64 {
    try self.enter();
    defer self.exit();
    if (!self.eatToken(.k_self)) return 0;
    return try self.pushNode(.{Tag.self});
}

// ..id (: expr)?
pub fn tryRestParams(self: *Parser) Err!u64 {
    try self.enter();
    defer self.exit();
    if (self.peek(&.{ .@".", .@"." })) {
        _ = self.nextToken();
        _ = self.nextToken();
        const id = try self.pushAtom(.id);
        var type_expr: u64 = 0;
        if (self.eatToken(.@":")) {
            type_expr = try self.tryExpr();
        }
        return try self.pushNode(.{ Tag.@"..id: expr", id, type_expr });
    }
    return 0;
}

// .id (: expr = expr')?
pub fn tryOptionalParam(self: *Parser) Err!u64 {
    try self.enter();
    defer self.exit();
    if (self.peek(&.{.@"."})) {
        _ = self.nextToken();
        const id = try self.pushAtom(.id);
        var type_expr: u64 = 0;
        var default_value: u64 = 0;
        if (self.eatToken(.@":")) {
            type_expr = try self.tryExpr();
            if (!self.eatToken(.@"=")) return self.unexpectedToken("expect `=` and a default value for option argument");
            default_value = try expr_module.tryExpr(self, .{ .no_object_call = true });
        }

        return try self.pushNode(.{ Tag.@".id: expr = expr'", id, type_expr, default_value });
    }
    return 0;
}

// id:- expr
pub fn tryTraitParam(self: *Parser) Err!u64 {
    try self.enter();
    defer self.exit();
    if (self.peek(&.{ .id, .@":-" })) {
        const id = try self.pushAtom(.id);
        _ = self.nextToken();
        const expr = try self.tryExpr();
        return try self.pushNode(.{ Tag.@"id:- expr", id, expr });
    }
    return 0;
}

// .id:- expr
pub fn tryOptionalTraitParam(self: *Parser) Err!u64 {
    try self.enter();
    defer self.exit();
    if (self.peek(&.{ .@".", .id, .@":-" })) {
        _ = self.nextToken();
        const id = try self.pushAtom(.id);
        _ = self.nextToken();
        const expr = try self.tryExpr();
        return try self.pushNode(.{ Tag.@".id:- expr", id, expr });
    }
    return 0;
}

// pattern : expr
pub fn tryParam(self: *Parser) Err!u64 {
    try self.enter();
    defer self.exit();

    self.sync();
    const pattern = try self.tryPattern();
    var type_expr: u64 = 0;
    if (self.eatToken(.@":"))
        type_expr = try self.tryExpr()
    else {
        self.fallback();
        return 0;
    }

    return try self.pushNode(.{ Tag.@"pattern : expr", pattern, type_expr });
}

// trait id? clauses? block
pub fn tryTrait(self: *Parser) Err!u64 {
    try self.enter();
    defer self.exit();
    if (!self.eatToken(.k_trait)) return 0;

    var id: u64 = 0;
    var clauses: u64 = 0;
    var block: u64 = 0;

    if (self.peek(&.{.id}))
        id = try self.pushAtom(.id);

    clauses = try tryClauses(self);
    block = try stmt.tryBlock(self);

    return try self.pushNode(.{ Tag.trait_def, id, clauses, block });
}

// fn name? (args) (->expr)? clauses? block?
// fn name? <args> (->expr)? clauses? block?
fn tryFn(self: *Parser) Err!u64 {
    try self.enter();
    defer self.exit();
    if (!self.eatToken(.k_fn)) return 0;
    var name: u64 = 0;
    var return_type: u64 = 0;
    var clauses: u64 = 0;
    var block_or_expr: u64 = 0;

    name = try basic.tryId(self);

    const rules = .{
        basic.rule("self", trySelf),
        basic.rule("rest parameters", tryRestParams),
        basic.rule("optional trait refined parameters", tryOptionalTraitParam),
        basic.rule("optional parameter", tryOptionalParam),
        basic.rule("trait refined parameters", tryTraitParam),
        basic.rule("parameter", tryParam),
        basic.rule("expression", Parser.tryExpr),
    };
    var args: std.ArrayList(u64) = undefined;
    var tag: Tag = .fn_def;
    if (self.peek(&.{.@"("}))
        args = try basic.pMulti(self, rules, .@"(")
    else {
        tag = .diamond_fn_def;
        args = try basic.pMulti(self, rules, .@"<");
    }
    defer args.deinit();

    if (self.eatToken(.@"->"))
        return_type = try self.tryExpr();

    clauses = try tryClauses(self);

    block_or_expr = try stmt.tryBlock(self);

    return try self.pushNode(.{ tag, name, return_type, clauses, block_or_expr, args.items });
}

// effect name? (args) (->expr)? clauses? block?
fn tryEffect(self: *Parser) Err!u64 {
    try self.enter();
    defer self.exit();
    if (!self.eatToken(.k_effect)) return 0;
    var name: u64 = 0;
    var return_type: u64 = 0;
    var clauses: u64 = 0;
    var block_or_expr: u64 = 0;

    name = try basic.tryId(self);

    const rules = .{
        basic.rule("self", trySelf),
        basic.rule("rest parameters", tryRestParams),
        basic.rule("optional trait refined parameters", tryOptionalTraitParam),
        basic.rule("optional parameter", tryOptionalParam),
        basic.rule("trait refined parameters", tryTraitParam),
        basic.rule("parameter", tryParam),
        basic.rule("expression", Parser.tryExpr),
    };
    var args: std.ArrayList(u64) = try basic.pMulti(self, rules, .@"(");

    defer args.deinit();

    if (self.eatToken(.@"->"))
        return_type = try self.tryExpr();

    clauses = try tryClauses(self);
    block_or_expr = try stmt.tryBlock(self);
    return try self.pushNode(.{ Tag.effect_def, name, return_type, clauses, block_or_expr, args.items });
}

// enum id? clauses? enum_def_body
fn tryEnum(self: *Parser) Err!u64 {
    try self.enter();
    defer self.exit();
    if (!self.eatToken(.k_enum)) return 0;
    var id: u64 = 0;
    var clauses: u64 = 0;
    var definition: u64 = 0;

    if (self.peek(&.{.id}))
        id = try self.pushAtom(.id);

    clauses = try tryClauses(self);
    definition = try pEnumDefBody(self);

    return try self.pushNode(.{ Tag.enum_def, id, clauses, definition });
}

fn pEnumDefBody(self: *Parser) Err!u64 {
    try self.enter();
    defer self.exit();
    const rules = .{
        basic.rule("property", basic.tryProperty),
        basic.ruleWithDelimiter("definition", tryDefinition, null),
        basic.rule("enum variant", tryEnumVariant),
        basic.ruleWithDelimiter("statement", Parser.tryStmt, .@";"),
    };

    const nodes = try basic.pMulti(self, rules, .@"{");
    defer nodes.deinit();

    return try self.pushNode(.{ Tag.enum_def_body, nodes.items });
}

// enum_variant ->
//     id parameters
//     | id { struct_field* }
//     | id . enum_def_body
//     | id = expr
fn tryEnumVariant(self: *Parser) Err!u64 {
    try self.enter();
    defer self.exit();
    if (!self.peek(&.{.id})) return 0;

    var tag: Tag = Tag.enum_variant;
    const id: u64 = try self.pushAtom(.id);
    var payload: u64 = 0;

    if (self.eatToken(.@"=")) {
        payload = try self.tryExpr();
        tag = Tag.enum_variant_with_value;
    } else if (self.peek(&.{.@"{"})) {
        payload = try tryStructDefBody(self);
        tag = Tag.enum_variant_with_struct_def_body;
    } else if (self.eatToken(.@".")) {
        payload = try pEnumDefBody(self);
        tag = Tag.enum_variant_with_sub_enum;
    } else if (self.peek(&.{.@"("})) {
        payload = try expr_module.tryQuoteOrTuple(self);
        tag = Tag.enum_variant_with_tuple;
    }

    return try self.pushNode(.{ tag, id, payload });
}

// union syntax is basically the same as enum
// reuse the enum definition
fn tryUnion(self: *Parser) Err!u64 {
    try self.enter();
    defer self.exit();
    if (!self.eatToken(.k_union)) return 0;
    var id: u64 = 0;
    var clauses: u64 = 0;
    var definition: u64 = 0;

    if (self.peek(&.{.id}))
        id = try self.pushAtom(.id);

    clauses = try tryClauses(self);
    definition = try pEnumDefBody(self);

    return try self.pushNode(.{ Tag.union_def, id, clauses, definition });
}

// mod id? block
fn tryMod(self: *Parser) Err!u64 {
    try self.enter();
    defer self.exit();
    if (!self.eatToken(.k_mod)) return 0;
    var block: u64 = 0;
    var id: u64 = 0;

    if (self.peek(&.{.id}))
        id = try self.pushAtom(.id);

    block = try stmt.tryBlock(self);
    if (block == 0)
        return self.unexpectedToken("expect a block after `mod`");

    return try self.pushNode(.{ Tag.mod_def, id, block });
}
