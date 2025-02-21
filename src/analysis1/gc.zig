const std = @import("std");
const mem = std.mem;
const hir = @import("hir.zig");

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

    // // Despearted
    // pub fn allocAtom(self: *Gc, size: u8) Err![*]u8 {
    //     const obj = try self.allocRaw(size);
    //     const meta = MetaData.at(obj);
    //     meta.size_description = size;

    //     return obj;
    // }

    // // Despearted
    // pub fn alloc(self: *Gc, len: u16) Err![*]u8 {
    //     const obj = try self.allocRaw(len * 8);
    //     const meta = MetaData.at(obj);
    //     meta.size_description = 0;
    //     meta.field_len = len;

    //     return obj;
    // }

    // pub fn new(self: *Gc, meta: MetaData) ![*]u8 {
    //     var bytes: []u8 = undefined;
    //     if (meta.tag == .children)
    //         bytes = try self.arena.alloc(u8, meta.field_len * 24 + 8)
    //     else if (meta.size_description == 0)
    //         bytes = try self.arena.alloc(u8, meta.field_len * 8 + 8)
    //     else
    //         bytes = try self.arena.alloc(u8, meta.size_description + 8);

    //     const meta_addr: *MetaData = @alignCast(@ptrCast(bytes));
    //     meta_addr.* = meta;

    //     try self.objects.append(bytes.ptr + 8);

    //     return bytes[8..].ptr;
    // }

    pub fn newHir(self: *Gc, comptime T: type) Err![*]u8 {
        hirNodeTypeConstraints(T);
        const tag = T.tag;
        var field_len: u16 = 0;
        var ref_start: u8 = 0;
        inline for (@typeInfo(T).Struct.fields) |field| {
            if ((ref_start == 0 and field.type == Ref))
                ref_start = @as(u8, @intCast(field_len));
            field_len += 1;
        }
        // no ref field
        if (ref_start == 0)
            ref_start = @as(u8, @intCast(field_len));

        const meta = MetaData.init(
            .used,
            .hir,
            ref_start,
            field_len,
            @intFromEnum(tag),
        );
        const result = try self.allocRaw(field_len * 8);
        MetaData.of(result).* = meta;
        return result;
    }

    pub fn newHirChildren(self: *Gc, field_len: u16) Err![*]u8 {
        const meta = MetaData.init(
            .used,
            .hir,
            0,
            field_len,
            @intFromEnum(hir.Tag.children),
        );
        const result = try self.allocRaw(field_len * @sizeOf(hir.Children.Symbol));
        MetaData.of(result).* = meta;

        return result;
    }

    fn hirNodeTypeConstraints(comptime T: type) void {
        inline for (@typeInfo(T).Struct.fields) |field| {
            if (@sizeOf(field.type) > 8)
                @compileError("hir node (excluding `Children`) type field size must be less than 8 bytes");
        }
    }

    //      -----------
    //     | metadata |
    //  -> -----------
    //     |   data   |
    //     |   ...    |
    pub inline fn allocRaw(self: *Gc, byte_size: usize) Err![*]u8 {
        if (self.objects.items.len >= self.option.object_limit)
            return Err.OutOfObjectLimit;

        const obj = self.arena.alloc(u8, byte_size + 8) catch return Err.OutOfArenaMemory;

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
        const meta_ = MetaData.of(obj);
        const meta: *MetaData = @alignCast(meta_);
        if (meta.color == .used) return;
        meta.color = .used;

        switch (meta.domain) {
            .hir => {
                if (meta.hirTag() == .children) {
                    for (hir.Children.Symbol.entries(obj)) |child| {
                        if (child.obj) |child_obj| self.mark(child_obj);
                    }
                } else {
                    const ref_start = meta.size_description;
                    for (ref_start..meta.field_len) |f| {
                        if (Object.nthField(obj, @as(u8, @intCast(f)))) |field| {
                            self.mark(field);
                        }
                    }
                }
            },
            .type_ctx => {
                unreachable;
            },
            .gc => {
                if (meta.size_description == 0) {
                    for (0..meta.field_len) |f| {
                        self.mark(obj + f * 8);
                    }
                }
            },
        }
    }

    pub fn gc(self: *Gc, root: []const [*]u8) !void {
        for (self.objects.items) |obj|
            asRef(MetaData, obj - 8).color = .unused;

        for (root) |obj| {
            self.mark(obj);
        }

        for (self.objects.items, 0..) |obj, i| {
            const meta = asRef(MetaData, obj - 8);
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

    pub fn deinit(self: *Gc) void {
        for (self.objects.items) |obj| {
            self.arena.free(MetaData.entireObject(obj));
        }
        self.objects.deinit();
        self.freed_objects.deinit();
    }
};

// 8 bytes
pub const MetaData = packed struct {
    color: Color = .used,
    domain: Domain = .hir,
    // if the size_description is 0, the object's size is len * 8
    size_description: u8 = 0,
    field_len: u16 = 0,
    // tag or type_index
    tag: u32 = 0,

    comptime {
        std.debug.assert(@sizeOf(MetaData) == 8);
    }

    pub inline fn of(obj: [*]u8) *align(1) MetaData {
        return @as(*align(1) MetaData, @ptrCast(obj - 8));
    }

    pub fn init(
        color: Color,
        domain: Domain,
        size_description: u8,
        field_len: u16,
        tag: u32,
    ) MetaData {
        return MetaData{
            .color = color,
            .domain = domain,
            .size_description = size_description,
            .field_len = field_len,
            .tag = tag,
        };
    }

    pub inline fn hirTag(self: *MetaData) hir.Tag {
        return @enumFromInt(self.tag);
    }

    pub fn entireObject(obj: [*]u8) []u8 {
        const meta = asRef(MetaData, obj - 8);
        const base = obj - 8;

        var size: usize = 8;

        switch (meta.domain) {
            .hir => {
                if (meta.hirTag() == .children)
                    size += meta.field_len * @sizeOf(hir.Children.Symbol)
                else
                    size += meta.field_len * 8;
            },
            .type_ctx => {
                unreachable;
            },
            .gc => {
                if (meta.size_description == 0)
                    size += meta.field_len * 8
                else
                    size += meta.size_description;
            },
        }
        return base[0..size];
    }
};

pub const Object = struct {
    pub fn toBytes(comptime ValueType: type, value: *const ValueType) [*]u8 {
        return @as([*]u8, @alignCast(@ptrCast(@constCast(value))));
    }

    // not { domain: .hir, tag: .children }
    pub fn nthField(obj: [*]u8, n: u16) ?[*]u8 {
        const meta = MetaData.of(obj);
        std.debug.assert(n < meta.field_len);
        const base = @as([*]?[*]u8, @alignCast(@ptrCast(obj)));
        return base[n];
    }

    // not { domain: .hir, tag: .children }
    pub fn setNthField(obj: [*]u8, n: u16, value: [*]u8) void {
        const meta = MetaData.of(obj);
        std.debug.assert(n < meta.field_len);
        const base = @as([*][*]u8, @alignCast(@ptrCast(obj)));
        base[n] = value;
    }
};

pub const Color = enum(u4) {
    used,
    unused,
    undefined,
};

pub const Domain = enum(u4) {
    hir,
    type_ctx,
    gc,
};

pub const Ref = ?[*]u8;

pub inline fn asRef(comptime T: type, obj: [*]u8) *T {
    return @as(*T, @alignCast(@ptrCast(obj)));
}

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer if (gpa.deinit() == .leak) {
        std.debug.print("Leaked....", .{});
    };

    var gc = Gc.init(gpa.allocator(), gpa.allocator(), .{});
    defer gc.deinit();

    // A { B, C }
    const mod_A = try gc.newHir(hir.ModDef);
    const mod_B = try gc.newHir(hir.ModDef);
    hir.ModDef.at(mod_B).parent = mod_A;
    const mod_C = try gc.newHir(hir.ModDef);
    hir.ModDef.at(mod_C).parent = mod_A;
    const children_of_A = try gc.newHirChildren(2);
    hir.Children.Symbol.entries(children_of_A)[0].obj = mod_B;
    hir.Children.Symbol.entries(children_of_A)[1].obj = mod_C;

    try gc.gc(&.{mod_A});
}
