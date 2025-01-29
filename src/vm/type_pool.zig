const std = @import("std");
const gc = @import("gc.zig");

pub const TypePool = struct {

    // TODO: none permitive types
    pub fn sizeOf(_: TypePool, type_index: u32) usize {
        return switch (type_index) {
            permitive_types.get("null").? => return 0,

            permitive_types.get("bool").?,
            permitive_types.get("i8").?,
            permitive_types.get("u8").?,
            permitive_types.get("char").?,
            => return 1,

            permitive_types.get("i16").?,
            permitive_types.get("u16").?,
            => return 2,

            permitive_types.get("i32").?,
            permitive_types.get("u32").?,
            permitive_types.get("f32").?,
            permitive_types.get("Type").?,
            => return 4,
            permitive_types.get("i64").?,
            permitive_types.get("u64").?,
            => return 8,
            permitive_types.get("i128").? => return 16,

            permitive_types.get("Integer").?,
            permitive_types.get("Real").?,
            permitive_types.get("Ref").?,
            => return 8,

            else => return 8,
        };
    }

    // UNCOMPLETED
    pub fn fieldLenOf(_: TypePool, type_index: u32) u16 {
        return switch (type_index) {
            permitive_types.get("null").?,
            permitive_types.get("bool").?,
            permitive_types.get("i8").?,
            permitive_types.get("u8").?,
            permitive_types.get("char").?,
            permitive_types.get("i16").?,
            permitive_types.get("u16").?,
            permitive_types.get("i32").?,
            permitive_types.get("u32").?,
            permitive_types.get("f32").?,
            permitive_types.get("Type").?,
            permitive_types.get("i64").?,
            permitive_types.get("u64").?,
            permitive_types.get("i128").?,
            permitive_types.get("Integer").?,
            permitive_types.get("Real").?,
            => 0,
            permitive_types.get("Ref").? => 1,

            else => return 8,
        };
    }

    pub fn typeEql(_: TypePool, x: [*]u8, y: [*]u8) bool {
        return gc.MetaData.metaOf(x).type_index == gc.MetaData.metaOf(y).type_index;
    }
};

pub const Type = union(enum) {
    Any,
    Trait,
    Impl,
    Type,
    Object,
    int: struct { size: u8, signed: bool },
    float: u8,
    void,
    noreturn,
    // to the struct description
    struct_: u64,
    // to the enum description
    enum_: u64,
    // to the union description
    union_: u64,
    // to the module description
    mod: u64,
    // to the function description
    function: u64,
    // to the effect description
    effect: u64,

    optional: u32,
    effectful: u64,
    error_union: u64,

    // Slice<u8> -> [to_slice, 1, to_u8]
    // Slice: pure fn<T> -> Type
    compose: u64,
};

pub const permitive_types = std.StaticStringMap(u32).initComptime(.{
    .{ "null", 0 },
    .{ "bool", 1 },
    .{ "i8", 2 },
    .{ "i16", 3 },
    .{ "i32", 4 },
    .{ "i64", 5 },
    .{ "i128", 6 },
    .{ "u8", 7 },
    .{ "u16", 8 },
    .{ "u32", 9 },
    .{ "u64", 10 },
    .{ "u128", 11 },
    .{ "Integer", 12 },
    .{ "f32", 13 },
    .{ "f64", 14 },
    .{ "Real", 15 },
    .{ "char", 16 },
    .{ "str", 17 },
    .{ "Object", 18 },
    .{ "List", 19 },
    .{ "Type", 20 },
    .{ "Ref", 21 },
});
