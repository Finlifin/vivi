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

    objects: std.ArrayList([*]u8),
    freed_objects: std.ArrayList(usize),
    // 所有object的数量

    option: GcOption,

    pub fn init(gpa: mem.Allocator, arena: mem.Allocator, opt: GcOption) Gc {
        return Gc{
            .arena = arena,
            .gpa = gpa,
            .objects = std.ArrayList([*]u8).init(gpa),
            .freed_objects = std.ArrayList(usize).init(gpa),
            .option = opt,
        };
    }

    pub fn allocAtom(self: *Gc, size: u8) Err![*]u8 {
        const obj = try self.allocRaw(size);
        const meta = MetaData.metaOf(obj);
        meta.size_description = size;

        return obj;
    }

    pub fn alloc(self: *Gc, len: u16) Err![*]u8 {
        const obj = try self.allocRaw(len * 8);
        const meta = MetaData.metaOf(obj);
        meta.size_description = 0;
        meta.field_len = len;

        return obj;
    }

    //      -----------
    //     | metadata |
    //  -> -----------
    //     |   data   |
    //     |   ...    |
    pub inline fn allocRaw(self: *Gc, size: usize) Err![*]u8 {
        if (self.objects.items.len >= self.option.object_limit)
            return Err.OutOfObjectLimit;

        const obj = self.arena.alloc(u8, size + 8) catch return Err.OutOfArenaMemory;

        var meta: MetaData = .{};
        const meta_bytes = @as([*]u8, (@ptrCast(&meta)));
        std.mem.copyForwards(u8, obj[0..8], meta_bytes[0..8]);
        self.objects.append(obj.ptr + 8) catch return Err.OutOfGpaMemory;
        return obj.ptr + 8;
    }

    pub fn free(self: *Gc, obj: [*]u8) void {
        self.arena.free(MetaData.entireObject(obj));
    }

    pub fn mark(self: *Gc, obj: [*]u8) void {
        const meta = MetaData.metaOf(obj);
        if (meta.color == .used or meta.color == .used_in_type) return;
        meta.color = .used;
        if (meta.size_description == 0) {
            const len = meta.field_len;
            const base = @as([*][*]u8, @alignCast(@ptrCast(obj)));
            for (0..len) |field| {
                self.mark(base[field]);
            }
        }
    }

    pub fn gc(self: *Gc, root: []const [*]u8) !void {
        for (self.objects.items) |obj| {
            const meta = MetaData.metaOf(obj);
            if (meta.color != .used_in_type)
                meta.color = .unused;
        }

        for (root) |obj| {
            self.mark(obj);
        }

        for (self.objects.items, 0..) |obj, i| {
            const meta = MetaData.metaOf(obj);
            if (meta.color == .unused) {
                self.free(obj);
                try self.freed_objects.append(i);
            }
        }

        var new_objects = std.ArrayList([*]u8).init(self.gpa);
        for (self.objects.items, 0..) |obj, i| {
            if (!self.bsContains(i))
                try new_objects.append(obj);
        }
        self.objects.deinit();
        self.objects = new_objects;
        self.freed_objects.clearRetainingCapacity();
        std.debug.print("DEBUG: self.objects.items.len after gc: {any}\n", .{self.objects.items.len});
    }

    // check freed_objects contains obj, binary search
    fn bsContains(self: Gc, obj: usize) bool {
        var l: usize = 0;
        var r: usize = self.freed_objects.items.len;
        while (l < r) {
            const m = l + (r - l) / 2;
            if (self.freed_objects.items[m] == obj)
                return true;
            if (self.freed_objects.items[m] < obj)
                l = m + 1
            else
                r = m;
        }
        return false;
    }

    pub fn newDigit(self: *Gc, comptime digit_type: type, value: digit_type) Err![*]u8 {
        const type_index = permitive_types.get(@typeName(digit_type)).?;
        const obj = try self.alloc(type_index);
        const len = type_pool.TypePool.sizeOf(undefined, type_index);
        std.mem.copyForwards(u8, obj[0..len], Object.rawValue(digit_type, &value)[0..len]);
        return obj;
    }

    pub fn deinit(self: *Gc) void {
        for (self.objects.items) |obj| {
            self.arena.free(MetaData.entireObject(obj));
        }
        self.objects.deinit();
        self.freed_objects.deinit();
    }
};

// 8 bytes
pub const MetaData = struct {
    color: Color = .used,
    // if the size_description is 0, the object's size is len * 8
    size_description: u8 = 0,
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
        const base = obj - 8;

        var size: usize = 8;
        if (meta.size_description == 0) {
            size += meta.field_len * 8;
        } else {
            size += meta.size_description;
        }
        return base[0..size];
    }
};

pub const Object = struct {
    pub fn rawValue(comptime ValueType: type, value: *const ValueType) [*]u8 {
        return @as([*]u8, @alignCast(@ptrCast(@constCast(value))));
    }

    pub fn nthField(obj: [*]u8, n: u16) [*]u8 {
        const meta = MetaData.metaOf(obj);
        std.debug.assert(meta.size_description == 0);
        std.debug.assert(n < meta.field_len);
        const base = @as([*][*]u8, @alignCast(@ptrCast(obj)));
        return base[n];
    }

    pub fn setNthField(obj: [*]u8, n: u16, value: [*]u8) void {
        const meta = MetaData.metaOf(obj);
        std.debug.assert(meta.size_description == 0);
        std.debug.assert(n < meta.field_len);
        const base = @as([*][*]u8, @alignCast(@ptrCast(obj)));
        base[n] = value;
    }
};

pub const Color = enum(u8) {
    used,
    unused,
    used_in_type,
    undefined,
};

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer if (gpa.deinit() == .leak) {
        std.debug.print("Leaked....", .{});
    };

    var gc = Gc.init(gpa.allocator(), gpa.allocator(), .{});
    defer gc.deinit();

    const some = try gc.allocAtom(8);
    // a -> b, b -> c, c -> a, a -> some
    const a = try gc.alloc(2);
    const b = try gc.alloc(1);
    const c = try gc.alloc(1);
    Object.setNthField(a, 0, b);
    Object.setNthField(b, 0, c);
    Object.setNthField(c, 0, a);
    Object.setNthField(a, 1, some);

    try gc.gc(&.{});
}
