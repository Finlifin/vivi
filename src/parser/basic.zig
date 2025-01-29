const std = @import("std");
const lexer = @import("../lexer/lexer.zig");
const ast = @import("ast.zig");
const parser = @import("parser.zig");
const Parser = parser.Parser;
const Err = parser.Err;
const Tag = ast.Tag;

const expr_module = @import("expr.zig");

// it may return a property or a symbol or nothing
pub fn tryProperty(self: *Parser) Err!u64 {
    try self.enter();
    defer self.exit();
    if (!self.peek(&.{ .@".", .id })) return 0;

    if (isTerminator(self.getToken(self.rcursor + 3))) return 0;

    self.eatTokens(1);
    const id = try tryId(self);
    const expr = try self.tryExpr();
    return try self.pushNode(.{ Tag.property, id, expr });
}

// some tokens shall never be a starter of an expression, pattern or statement
pub fn isTerminator(token: lexer.Token) bool {
    return switch (token.tag) {
        .eof, .@"}", .@"]", .@")", .@",", .@">", .@";" => true,
        else => false,
    };
}

const Rule = struct {
    name: []const u8,
    p: fn (self: *Parser) Err!u64,
    delimiter: ?lexer.Token.Tag = .@",",
    terminators: []const lexer.Token.Tag = &.{ .eof, .@"}", .@"]", .@")", .@",", .@">" },
};

pub fn rule(name: []const u8, p: fn (self: *Parser) Err!u64) Rule {
    return Rule{ .name = name, .p = p };
}

pub fn ruleWithDelimiter(name: []const u8, p: fn (self: *Parser) Err!u64, delimiter: ?lexer.Token.Tag) Rule {
    return Rule{ .name = name, .p = p, .delimiter = delimiter };
}

pub fn pMulti(
    self: *Parser,
    comptime rules: anytype,
    comptime br: lexer.Token.Tag,
) Err!std.ArrayList(u64) {
    if (rules.len == 0) @compileError("provide at least one rule");
    const close_br: lexer.Token.Tag = switch (br) {
        .@"{" => .@"}",
        .@"[" => .@"]",
        .@"(" => .@")",
        .@"<" => .@">",
        .@"|" => .@"|",
        .eof => .eof,
        else => @compileError("unsupported bracket"),
    };
    if (br != .eof)
        try self.expectNextToken(br, "expected an opening bracket");

    var nodes = std.ArrayList(u64).init(self.tmp_alc.allocator());
    if (self.eatToken(close_br)) return nodes;
    while (true) {
        var node: u64 = 0;
        var delimiter: ?lexer.Token.Tag = .@",";

        inline for (rules) |r| {
            if (@TypeOf(r) != Rule) @compileError("rules should be a tuple whose elements are of type `Rule`");

            node = try r.p(self);
            if (node != 0) {
                delimiter = r.delimiter;
                break;
            }
        }
        try nodes.append(node);
        if (self.eatToken(close_br)) break;

        if (delimiter) |s|
            try self.expectNextToken(s, "expected a delimiter or a closing bracket");
        if (self.eatToken(close_br)) break;
    }

    return nodes;
}

pub fn tryId(self: *Parser) Err!u64 {
    try self.enter();
    defer self.exit();

    if (!self.peek(&.{.id})) return 0;
    return try self.pushAtom(.id);
}

pub fn trySymbol(self: *Parser) Err!u64 {
    try self.enter();
    defer self.exit();
    if (!self.peek(&.{ .@".", .id })) return 0;

    self.eatTokens(1);
    const id = try tryId(self);

    return self.pushNode(.{ Tag.symbol, id });
}
