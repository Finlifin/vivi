const std = @import("std");
const mem = @import("std").mem;
const type_pool = @import("type_pool.zig");
const permitive_types = type_pool.permitive_types;

pub const GcOption = struct {
    mode: enum { debug, release } = .debug,
    object_limit: usize = 1024 * 1024 * 32,
};

pub const Err = error{
    OutOfArenaMemory,
    OutOfGpaMemory,
    OutOfObjectLimit,
    TypeError,
    UnknownErr,
};

pub const Gc = struct {
    arena: mem.Allocator,
    gpa: mem.Allocator,

    objects: std.AutoHashMap([*]u8, void),
    // 所有object的数量
    object_count: usize = 0,

    option: GcOption,

    pub fn init(gpa: mem.Allocator, arena: mem.Allocator, opt: GcOption) Gc {
        return Gc{
            .arena = arena,
            .gpa = gpa,
            .objects = std.AutoHashMap([*]u8, void).init(gpa),
            .option = opt,
        };
    }

    //      -----------
    //     | metadata |
    //  -> -----------
    //     |   data   |
    //     |   ...    |
    pub fn alloc(self: *Gc, type_index: u32) Err![*]u8 {
        const field_len = type_pool.TypePool.fieldLenOf(undefined, type_index);
        const len = type_pool.TypePool.sizeOf(undefined, type_index);

        if (self.object_count >= self.option.object_limit)
            return Err.OutOfObjectLimit;

        const obj = self.arena.alloc(u8, len + 8) catch return Err.OutOfArenaMemory;
        self.object_count += 1;

        var meta: MetaData = undefined;
        meta.color = .used;
        meta.field_len = field_len;
        meta.type_index = type_index;
        const meta_bytes = @as([*]u8, (@ptrCast(&meta)));
        std.mem.copyForwards(u8, obj[0..8], meta_bytes[0..8]);
        self.objects.put(obj.ptr + 8, undefined) catch return Err.OutOfGpaMemory;
        return obj.ptr + 8;
    }

    pub fn free(self: *Gc, obj: [*]u8) void {
        self.arena.free(MetaData.entireObject(obj));
        self.object_count -= 1;
    }

    pub fn mark(self: *Gc, obj: [*]u8) void {
        const meta = MetaData.metaOf(obj);
        meta.color = .used;
        switch (meta.type_index) {
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
            => {},
            else => {
                const len = meta.field_len;
                const base = @as([*][*]u8, @alignCast(@ptrCast(obj)));
                for (0..len) |field| {
                    self.mark(base[field]);
                }
            },
        }
    }

    pub fn gc(self: *Gc, root: []const [*]u8) void {
        var iter = self.objects.keyIterator();
        while (iter.next()) |obj| {
            const meta = MetaData.metaOf(obj.*);
            meta.color = .unused;
        }

        for (root) |obj| {
            self.mark(obj);
        }

        iter = self.objects.keyIterator();
        while (iter.next()) |obj| {
            const meta = MetaData.metaOf(obj.*);
            if (meta.color == .unused) {
                self.free(obj.*);

                if (!self.objects.remove(obj.*))
                    std.debug.print("ERROR: Gc.objects.remove(obj) obj not exist!\n", .{});
            }
        }

        std.debug.print("DEBUG: object count after gc {any}\n", .{self.object_count});
    }

    pub fn newDigit(self: *Gc, comptime digit_type: type, value: digit_type) Err![*]u8 {
        const type_index = permitive_types.get(@typeName(digit_type)).?;
        const obj = try self.alloc(type_index);
        const len = type_pool.TypePool.sizeOf(undefined, type_index);
        std.mem.copyForwards(u8, obj[0..len], Object.rawValue(digit_type, &value)[0..len]);
        return obj;
    }

    pub fn deinit(self: *Gc) void {
        var iter = self.objects.keyIterator();
        while (iter.next()) |obj| {
            self.free(obj.*);
        }
        self.objects.deinit();
    }
};

// 8 bytes
pub const MetaData = struct {
    color: Color = .used,
    padding: u8 = 0,
    // if the object is a list, field_len is the length of the list
    // if the object is a struct, field_len is the number of fields
    field_len: u16 = 0,
    type_index: u32 = 0,

    comptime {
        std.debug.assert(@sizeOf(MetaData) == 8);
    }

    pub inline fn metaOf(obj: [*]u8) *MetaData {
        return @as(*MetaData, @alignCast(@ptrCast(obj - 8)));
    }

    pub fn entireObject(obj: [*]u8) []u8 {
        const meta = metaOf(obj);
        const type_index = meta.type_index;
        const base = obj - 8;

        var size: usize = 8;
        switch (type_index) {
            permitive_types.get("null").? => size += 0,

            permitive_types.get("bool").?,
            permitive_types.get("i8").?,
            permitive_types.get("u8").?,
            permitive_types.get("char").?,
            => size += 1,

            permitive_types.get("i16").?,
            permitive_types.get("u16").?,
            => size += 2,

            permitive_types.get("i32").?,
            permitive_types.get("u32").?,
            permitive_types.get("f32").?,
            permitive_types.get("Type").?,
            => size += 4,
            permitive_types.get("i64").?,
            permitive_types.get("u64").?,
            => size += 8,
            permitive_types.get("i128").? => size += 16,

            permitive_types.get("Integer").?,
            permitive_types.get("Real").?,
            permitive_types.get("Ref").?,
            => size += 8,

            else => size += 8 * meta.field_len,
        }
        return base[0..size];
    }
};

pub const Object = struct {
    pub fn assign(left: [*]u8, right: [*]u8) Err!void {
        // if (!type_pool.TypePool.typeEql(undefined, left, right)) return Err.TypeError;
        const p = @as(*[*]u8, @alignCast(@ptrCast(left)));
        p.* = right;
    }

    // dst and src must have the same type
    pub fn memcopy(dst: [*]u8, src: [*]u8) void {
        const dst_meta = MetaData.metaOf(dst);
        const len = type_pool.TypePool.sizeOf(undefined, dst_meta.type_index);
        std.mem.copyForwards(u8, dst[0..len], src[0..len]);
    }

    pub fn rawValue(comptime ValueType: type, value: *const ValueType) [*]u8 {
        return @as([*]u8, @alignCast(@ptrCast(@constCast(value))));
    }
};

pub const Color = enum(u8) {
    used,
    unused,
    undefined,
};

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer if (gpa.deinit() == .leak) {
        std.debug.print("Leaked....", .{});
    };

    var gc = Gc.init(gpa.allocator(), gpa.allocator(), .{});
    defer gc.deinit();

    const int0 = try gc.newDigit(i32, 0);
    _ = try gc.newDigit(i32, 20);
    const ref = try gc.alloc(permitive_types.get("Ref").?);
    try Object.assign(ref, int0);
    gc.gc(&.{ref});
}
