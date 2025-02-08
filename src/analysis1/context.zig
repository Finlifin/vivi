const std = @import("std");
const string_pool = @import("../common/string_pool.zig");
const type_pool = @import("../vm/type_pool.zig");
const hir = @import("hir.zig");
usingnamespace hir;
const gc_mod = @import("gc.zig");
usingnamespace gc_mod;

pub const Context = struct {
    gc: gc_mod.Gc,
    root: [*]u8,
    stack: std.ArrayList([*]u8),

    sp: *string_pool.StringPool,

    pub fn push(self: *Context, obj: [*]u8) !void {
        try self.stack.append(obj);
    }

    pub fn pop(self: *Context) [*]u8 {
        return self.stack.pop();
    }

    pub fn current(self: *Context) [*]u8 {
        return self.stack.items[self.stack.items.len - 1];
    }

    pub inline fn resolve(self: *Context, name: string_pool.Index) ?*hir.Children.Entry {
        return hir.Item.resolve(self.current(), name);
    }

    pub fn init(gpa: std.mem.Allocator, sp: *string_pool.StringPool) !Context {
        var result = Context{
            .gc = gc_mod.Gc.init(gpa, gpa, .{ .mode = .debug }),
            .root = undefined,
            .sp = sp,
            .stack = std.ArrayList([*]u8).init(gpa),
        };

        const meta = gc_mod.MetaData.init(0, 1, .root);
        result.root = try result.gc.new(meta);
        const builtins = try result.gc.new(gc_mod.MetaData.init(0, 2, .children));
        hir.Root.at(result.root).children = builtins;
        const builtins_entries = hir.Children.Entry.entries(builtins);
        builtins_entries[0].name = try result.sp.put("Integer");
        builtins_entries[0].obj = try result.gc.new(gc_mod.MetaData.init(0, 0, .unit));
        builtins_entries[1].name = try result.sp.put("Real");
        builtins_entries[1].obj = try result.gc.new(gc_mod.MetaData.init(0, 0, .unit));

        // create a file mod
        const file_mod = try result.gc.new(hir.ModDef.proto);
        const file_mod_children = try result.gc.new(gc_mod.MetaData.init(0, 0, .children));
        hir.ModDef.at(file_mod).children = file_mod_children;
        try result.push(result.root);

        return result;
    }

    pub fn deinit(self: *Context) void {
        self.gc.deinit();
        self.stack.deinit();
    }
};
