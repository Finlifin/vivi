const std = @import("std");
const Vec = std.ArrayList;
const lexer = @import("../lexer/lexer.zig");
const ast = @import("ast.zig");
const expr = @import("expr.zig");
const pattern = @import("pattern.zig");
const stmt = @import("stmt.zig");
const vfs = @import("../vfs.zig");
const error_code = @import("../error_code.zig");
const handle_error = @import("error.zig").handle_error;
const basic = @import("basic.zig");

// an ast is flatten buffer of nodes
// if buf[i] == .int, then buf[i+1] is the main token

pub const Parser = struct {
    alc: std.mem.Allocator,
    tmp_alc: std.heap.FixedBufferAllocator,
    tmp: []u8,

    src: []const u8,
    src_id: u32,
    vfs: *vfs.Vfs,
    tokens: std.ArrayList(lexer.Token),
    nodes: Vec(u64),
    valid_nodes_len: u64,
    // for debug
    tags: Vec(ast.Tag),
    tags_location: Vec(u64),
    lcursor: u64,
    rcursor: u64,

    option: ParserSetting,
    err: ErrPayload,

    pub fn init(
        alc: std.mem.Allocator,
        vfs_handle: *vfs.Vfs,
        src_id: u32,
        options: ParserSetting,
    ) !Parser {
        const src = vfs_handle.fileContent(src_id) orelse return Err.SrcIdNotFound;

        var result: Parser = undefined;
        result.vfs = vfs_handle;
        result.src = src;
        result.src_id = src_id;
        result.alc = alc;
        result.lcursor = 0;
        result.rcursor = 0;
        result.tokens = try lexer.lex(alc, src);
        result.tmp = try alc.alloc(u8, 1024 * 1024);
        result.tmp_alc = std.heap.FixedBufferAllocator.init(result.tmp);
        result.nodes = Vec(u64).init(alc);
        result.option = options;
        if (options.mode == .debug) {
            result.tags = Vec(ast.Tag).init(alc);
            result.tags_location = Vec(u64).init(alc);
        }
        // index 0 is always nothing
        try result.nodes.append(@intFromEnum(ast.Tag.invalid));
        result.valid_nodes_len = 1;

        return result;
    }

    const NodeKind = enum {
        tag,
        len,
        token_index,
        child,
        property,
    };
    pub fn push(self: *Parser, item: u64, kind: NodeKind) Err!u64 {
        try self.nodes.append(item);
        if (kind == .tag and self.option.mode == .debug) {
            self.tags.append(@enumFromInt(item)) catch return Err.OutOfMemory;
            self.tags_location.append(self.nodes.items.len) catch return Err.OutOfMemory;
        }
        return self.nodes.items.len - 1;
    }

    // the first element shall be the tag
    pub fn pushNode(self: *Parser, node: anytype) Err!u64 {
        var result: u64 = 0;
        switch (node.len) {
            // single tag
            1 => {
                result = try self.push(@intFromEnum(node[0]), .tag);
            },
            // with single child or multiple children
            2 => {
                result = try self.push(@intFromEnum(node[0]), .tag);
                if (@TypeOf(node[1]) == []u64) {
                    const len = node[1].len;
                    _ = try self.push(len, .len);
                    try self.nodes.appendSlice(node[1]);
                } else {
                    _ = try self.push(node[1], .child);
                }
            },
            // with two children or one left and multiple children
            3 => {
                result = try self.push(@intFromEnum(node[0]), .tag);
                if (@TypeOf(node[2]) == []u64) {
                    _ = try self.push(node[1], .child);
                    const len = node[2].len;
                    _ = try self.push(len, .len);
                    try self.nodes.appendSlice(node[2]);
                } else {
                    _ = try self.push(node[1], .child);
                    _ = try self.push(node[2], .child);
                }
            },
            // three children
            4 => {
                result = try self.push(@intFromEnum(node[0]), .tag);
                _ = try self.push(node[1], .child);
                _ = try self.push(node[2], .child);
                _ = try self.push(node[3], .child);
            },
            // for lambda
            5 => {
                result = try self.push(@intFromEnum(node[0]), .tag);
                _ = try self.push(node[1], .child);
                _ = try self.push(node[2], .child);
                _ = try self.push(node[3], .child);
                if (@TypeOf(node[4]) == []u64) {
                    const len = node[4].len;
                    _ = try self.push(len, .len);
                    try self.nodes.appendSlice(node[4]);
                } else {
                    _ = try self.push(node[4], .child);
                }
            },
            // for function definition
            6 => {
                result = try self.push(@intFromEnum(node[0]), .tag);
                _ = try self.push(node[1], .child);
                _ = try self.push(node[2], .child);
                _ = try self.push(node[3], .child);
                _ = try self.push(node[4], .child);
                if (@TypeOf(node[5]) == []u64) {
                    const len = node[5].len;
                    _ = try self.push(len, .len);
                    try self.nodes.appendSlice(node[5]);
                } else {
                    _ = try self.push(node[5], .child);
                }
            },
            else => {
                @compileLog(node);
                @compileError("unsupported node length");
            },
        }

        return result;
    }

    pub fn sync(self: *Parser) void {
        self.valid_nodes_len = self.nodes.items.len;
        self.lcursor = self.rcursor;
    }

    pub fn fallback(self: *Parser) void {
        self.nodes.items = self.nodes.items[0..self.valid_nodes_len];
        self.rcursor = self.lcursor;
    }

    pub fn pushRaw(self: *Parser, tag: ast.Tag) Err!u64 {
        _ = self.nextToken();
        const result = try self.push(@intFromEnum(tag), .tag);
        _ = try self.push(self.rcursor, .token_index);
        // dbg
        // std.debug.print("pushRaw: {s}\n", .{@tagName(tag)});
        return result;
    }

    pub fn pExpr(self: *Parser) Err!u64 {
        const result = try expr.pExpr(self, .{});

        return result;
    }

    pub fn pPattern(self: *Parser) Err!u64 {
        return pattern.pPattern(self, .{});
    }

    pub fn pStmt(self: *Parser) Err!u64 {
        return stmt.pStmt(self);
    }

    pub fn parse(self: *Parser) u64 {
        // the first token is always sof
        return stmt.pModScope(self) catch |err| {
            handle_error(self, err);
            return 0;
        };
    }

    pub fn peekToken(self: *Parser) lexer.Token {
        return self.tokens.items[self.rcursor + 1];
    }

    pub fn peek(self: *Parser, tags: []const lexer.Token.Tag) bool {
        for (tags, 0..) |tag, i| {
            if (self.tokens.items[self.rcursor + 1 + i].tag != tag) return false;
        }
        return true;
    }

    pub fn nextToken(self: *Parser) lexer.Token {
        self.rcursor += 1;
        return self.currentToken();
    }

    pub fn currentToken(self: *Parser) lexer.Token {
        return self.tokens.items[self.rcursor];
    }

    pub fn eatToken(self: *Parser, tag: lexer.Token.Tag) bool {
        const token = self.peekToken();
        if (token.tag != tag) {
            return false;
        }
        self.rcursor += 1;
        return true;
    }

    pub fn srcContent(self: Parser, token: lexer.Token) []const u8 {
        return self.src[token.from..token.to];
    }

    pub fn expectNextToken(self: *Parser, tag: lexer.Token.Tag, info: []const u8) Err!void {
        if (!self.eatToken(tag)) {
            self.err = ErrPayload{ .UnexpectedToken = .{
                .got = self.peekToken(),
                .info = info,
            } };
            return Err.UnexpectedToken;
        }
    }

    pub fn unexpectedToken(self: *Parser, info: []const u8) Err {
        self.err = ErrPayload{ .UnexpectedToken = .{
            .got = self.currentToken(),
            .info = info,
        } };
        return Err.UnexpectedToken;
    }

    pub fn invalidExpr(self: *Parser, start: u64, end: u64, info: []const u8) Err {
        self.err = ErrPayload{ .InvalidExpr = .{
            .start = start,
            .end = end,
            .info = info,
        } };
        return Err.InvalidExpr;
    }

    pub fn invalidPattern(self: *Parser, start: u64, end: u64, info: []const u8) Err {
        self.err = ErrPayload{ .InvalidPattern = .{
            .start = start,
            .end = end,
            .info = info,
        } };
        return Err.InvalidPattern;
    }

    pub fn getToken(self: Parser, cursor: u64) lexer.Token {
        return self.tokens.items[cursor];
    }

    pub fn getNode(self: Parser, node_index: u64) u64 {
        return self.nodes.items[node_index];
    }

    pub fn deinit(self: *Parser) void {
        self.tokens.deinit();
        self.nodes.deinit();
        self.alc.free(self.tmp);
        if (self.option.mode == .debug) {
            self.tags.deinit();
            self.tags_location.deinit();
        }
    }

    // dump node to s-expression format
    pub fn dump(self: Parser, node_index: u64, writer: anytype) !void {
        const node = self.nodes.items[node_index];
        const tag: ast.Tag = @enumFromInt(node);
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
            .symbol,
            => {
                const token = self.getToken(self.getNode(node_index + 1));
                try writer.writeAll(self.srcContent(token));
            },
            // with multiple tokens
            .real => {
                const left = self.getToken(self.getNode(node_index + 1));
                const right = self.getToken(self.getNode(node_index + 1) + 2);
                try writer.print("{s}.{s}", .{ self.srcContent(left), self.srcContent(right) });
            },
            // with single child
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
            .derivation,
            => {
                try self.dump(self.getNode(node_index + 1), writer);
                const len = self.getNode(node_index + 2);
                try writer.print("(len {d}) ", .{len});
                for (0..len) |i| {
                    try self.dump(self.getNode(node_index + 3 + i), writer);
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
            .fn_def => {
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

    pub fn dumpTokens(self: Parser) void {
        for (self.tokens.items) |token| {
            std.debug.print("{s} ", .{@tagName(token.tag)});
        }
        std.debug.print("\n", .{});
    }
};

pub const Err = error{
    NoMatter,
    OutOfMemory,
    OutOfTmpMemory,
    UnexpectedToken,
    SrcIdNotFound,
    InvalidExpr,
    InvalidPattern,
};

pub const ErrPayload = union {
    OutOfMemory: void,
    OutOfTmpMemory: void,
    NoMatter: void,
    SrcIdNotFound: void,
    UnexpectedToken: struct {
        got: lexer.Token,
        info: []const u8,
    },
    InvalidExpr: struct {
        // token indexes
        start: u64,
        end: u64,
        info: []const u8,
    },
    InvalidPattern: struct {
        start: u64,
        end: u64,
        info: []const u8,
    },
};

const ParserSetting = struct {
    mode: enum { normal, debug } = .debug,
};
