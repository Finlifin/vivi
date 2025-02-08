const std = @import("std");
const hir = @import("hir.zig");
const ast = @import("../parser/ast.zig");
const gc_mod = @import("gc.zig");
const context = @import("context.zig");

const type_pool = @import("../vm/type_pool.zig");
const string_pool = @import("../common/string_pool.zig");

pub const Analyzer = struct {
    gpa: std.mem.Allocator,

    ctx: context.Context,

    ast: *ast.Ast,
    ast_root: u64,
    sp: *string_pool.StringPool,
    tp: *type_pool.TypePool,

    options: Options,
    errors: std.ArrayList(void),

    pub fn init(
        gpa: std.mem.Allocator,
        sp: *string_pool.StringPool,
        tp: *type_pool.TypePool,
        ast_: *ast.Ast,
        ast_root: u64,
        opt: Options,
    ) Err!Analyzer {
        return Analyzer{
            .gpa = gpa,
            .ctx = try context.Context.init(gpa, sp),
            .ast = ast_,
            .ast_root = ast_root,
            .sp = sp,
            .tp = tp,
            .options = opt,
            .errors = std.ArrayList(void).init(gpa),
        };
    }

    pub fn recognizeEntries(self: *Analyzer, len_index: u64) Err!std.ArrayList(hir.Children.Entry) {
        var entries = std.ArrayList(hir.Children.Entry).init(self.gpa);
        for (self.ast.toSlice(len_index)) |node| {
            const tag = self.ast.getNodeTag(node);
            switch (tag) {
                .mod_def,
                .struct_def,
                .enum_def,
                .union_def,
                .trait_def,
                .fn_def,
                .effect_def,
                => {
                    const id = self.ast.srcContentD(node + 1);
                    const name = try self.sp.put(id);
                    try entries.append(.{
                        .name = name,
                        .ast_node = node,
                        .obj = null,
                    });
                },
                .const_decl => {
                    std.debug.assert(self.ast.getNodeTag(self.ast.getNode(node + 1)) == .id);
                    const id = self.ast.srcContentD(node + 1);
                    const name = try self.sp.put(id);
                    try entries.append(.{
                        .name = name,
                        .ast_node = node,
                        .obj = null,
                    });
                },
                else => {},
            }
        }

        return entries;
    }

    // .file_scope, len, ...nodes
    pub fn analyzeFileScope(self: *Analyzer, filename: string_pool.Index) Err!void {
        const children_entries = try self.recognizeEntries(self.ast_root + 1);
        defer children_entries.deinit();

        const file_mod_ = try self.new(hir.ModDef.proto);
        const file_mod = hir.ModDef.at(file_mod_);
        file_mod.* = .{
            .name = filename,
            .type_index = 0,
            .ast_node = self.ast_root,
            .parent = self.ctx.current(),
            .children = try hir.Children.new(&self.ctx.gc, children_entries.items),
            .impls = undefined,
        };
        try self.ctx.push(file_mod_);
        for (hir.Children.Entry.entries(file_mod.children)) |*entry| {
            if (entry.obj == null)
                entry.obj = try self.analyze(entry.ast_node);
        }
    }

    pub fn analyze(self: *Analyzer, ast_node: u64) Err!?[*]u8 {
        const ast_node_tag = self.ast.getNodeTag(ast_node);
        return switch (ast_node_tag) {
            .mod_def => try analyzeModDef(self, ast_node),
            .const_decl => try analyzeConstDef(self, ast_node),
            else => null,
        };
    }

    // .const_decl, pattern, type, value
    pub fn analyzeConstDef(self: *Analyzer, ast_node: u64) Err![*]u8 {
        std.debug.assert(self.ast.getNodeTag(ast_node) == .const_decl);
        // `pattern` may not be an identifier
        std.debug.assert(self.ast.getNodeTag(self.ast.getNode(ast_node + 1)) == .id);

        const id = self.ast.srcContentD(ast_node + 1);
        const type_ = try self.analyzeExpr(self.ast.getNode(ast_node + 2));
        const value = try self.analyzeExpr(self.ast.getNode(ast_node + 3));

        std.debug.print("DEBUG: analyzing const def: {s}\n", .{id});

        const const_decl_ = try self.new(hir.ConstDef.proto);
        const const_decl = hir.ConstDef.at(const_decl_);
        const_decl.* = .{
            .name = try self.sp.put(id),
            .type_index = 0,
            .type = type_,
            .value = value,
            .ast_node = ast_node,
            .parent = self.ctx.current(),
        };
        return const_decl_;
    }

    pub fn analyzeExpr(self: *Analyzer, ast_node: u64) Err!?[*]u8 {
        if (ast_node == 0) return null;

        const ast_node_tag = self.ast.getNodeTag(ast_node);
        switch (ast_node_tag) {
            .id => {
                const id = self.ast.srcContentT(ast_node);
                std.debug.print("DEBUG: start to resolve id: {s}\n", .{id});
                const name = try self.sp.put(id);
                const entry = try self.resolve(name);
                if (entry) |e|
                    return e.obj
                else {
                    try self.ast.report(ast_node, .err, .UnknownIdentifier, "unknown identifier", 3);
                    return null;
                }
            },
            .int => {
                const int_ = try hir.Atom.new(&self.ctx.gc, ast_node, .int);
                const int = hir.Atom.Int.at(int_);
                int.* = .{
                    .type_index = 0,
                    .ast_node = ast_node,
                    .value = std.fmt.parseInt(
                        i64,
                        self.ast.srcContentT(ast_node),
                        10,
                    ) catch {
                        try self.ast.report(ast_node, .err, .InvalidIntegerLiteral, "FUCK YOU", 3);
                        std.debug.panic("failed to parse int: {s}", .{self.ast.srcContentT(ast_node)});
                    },
                };

                return int_;
            },

            else => return null,
        }
    }

    // .mod_def, id, block
    // .block, len, ...nodes
    pub fn analyzeModDef(self: *Analyzer, ast_node: u64) Err![*]u8 {
        std.debug.assert(self.ast.getNodeTag(ast_node) == .mod_def);
        const block = self.ast.getNode(ast_node + 2);
        const children_entries = try self.recognizeEntries(block + 1);
        defer children_entries.deinit();

        std.debug.print("DEBUG: analyzing mod def: {s}\n", .{self.ast.srcContentD(ast_node + 1)});

        const mod_def_ = try self.new(hir.ModDef.proto);
        const mod_def = hir.ModDef.at(mod_def_);
        mod_def.* = .{
            .name = try self.sp.put(self.ast.srcContentD(ast_node + 1)),
            .type_index = 0,
            .ast_node = ast_node,
            .parent = self.ctx.current(),
            .children = try hir.Children.new(&self.ctx.gc, children_entries.items),
            .impls = undefined,
        };
        if (mod_def.name == try self.sp.put("H")) {
            std.debug.print("DEBUG: resolving `Well`: {any}\n", .{self.resolve(try self.sp.put("Well"))});
        }
        try self.ctx.push(mod_def_);
        for (hir.Children.Entry.entries(mod_def.children)) |*entry| {
            if (entry.obj == null)
                entry.obj = try self.analyze(entry.ast_node);
        }
        return mod_def_;
    }

    //

    pub fn resolve(self: *Analyzer, name: string_pool.Index) !?*hir.Children.Entry {
        const result = self.ctx.resolve(name);
        if (result) |entry| {
            if (entry.obj == null) {
                entry.obj = try self.analyze(entry.ast_node);
                return entry;
            } else return entry;
        }

        return result;
    }

    pub fn deinit(self: *Analyzer) void {
        self.ctx.deinit();
    }

    pub inline fn new(self: *Analyzer, meta: gc_mod.MetaData) ![*]u8 {
        return self.ctx.gc.new(meta);
    }
};

pub const Options = struct {
    mode: enum { debug, release } = .debug,
};

pub const Err = error{
    OutOfMemory,
    Unknown,
};
