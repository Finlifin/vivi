const std = @import("std");
const gc = @import("gc.zig");
const context = @import("../analysis1/context.zig");
const string_pool = @import("../common/string_pool.zig");

pub const TypePool = struct {
    types: std.ArrayList(Type),
    sp: *string_pool.StringPool,

    pub fn init(gpa: std.mem.Allocator, sp: *string_pool.StringPool) !TypePool {
        var result = TypePool{
            .types = std.ArrayList(Type).init(gpa),
            .sp = sp,
        };

        // Add builtin types
        try result.types.append(.Any);
        try result.types.append(.str);
        try result.types.append(.{ .int = .{ .size = 1, .signed = true } });
        try result.types.append(.{ .int = .{ .size = 1, .signed = false } });
        try result.types.append(.{ .int = .{ .size = 2, .signed = true } });

        try result.types.append(.{ .int = .{ .size = 2, .signed = false } });
        try result.types.append(.{ .int = .{ .size = 4, .signed = true } });
        try result.types.append(.{ .int = .{ .size = 4, .signed = false } });
        try result.types.append(.{ .int = .{ .size = 8, .signed = true } });
        try result.types.append(.{ .int = .{ .size = 8, .signed = false } });

        try result.types.append(.{ .int = .{ .size = 16, .signed = true } });
        try result.types.append(.{ .int = .{ .size = 16, .signed = false } });
        try result.types.append(.{ .float = 4 });
        try result.types.append(.{ .float = 8 });
        try result.types.append(.void);

        try result.types.append(.noreturn);
        // Integer
        try result.types.append(.{ .int = .{ .size = 0, .signed = true } });
        // Real
        try result.types.append(.{ .float = 0 });
        try result.types.append(.Type);

        return result;
    }

    pub fn dumpType(self: TypePool, type_index: u32, writer: anytype) !void {
        const type_ = self.types.items[type_index];
        try writer.print("{any}", .{type_});
    }

    pub fn addType(self: *TypePool, type_: Type) !Index {
        const index = self.types.items.len;
        try self.types.append(type_);
        return @intCast(index);
    }

    pub fn deinit(self: *TypePool) void {
        self.types.deinit();
    }

    // TODO
    pub fn typeEql(_: TypePool, a: Index, b: Index) bool {
        if (a == b) return true;

        return false;
    }
};

pub const Type = union(enum) {
    Any: struct {},
    Trait,
    Impl,
    Type,
    Object,
    int: struct { size: u8, signed: bool },
    float: u8,
    char,
    str,
    void,
    noreturn,
    struct_,
    enum_,
    union_,
    mod,
    function,
    effect,

    optional: u32,
    effectful: u64,
    error_union: u64,

    // Slice<u8> -> [to_slice, 1, to_u8]
    // Slice: pure fn<T> -> Type
    mapping: struct { func: [*]u8, args: [][*]u8, optional_args: [][*]u8 },
};

pub const Index = u32;

const StructField = struct {
    name: string_pool.Index,
    type: Type,
    default_value: ?[*]u8,
};
