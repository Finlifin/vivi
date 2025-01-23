const std = @import("std");
const Allocator = std.mem.Allocator;

pub const StringPool = struct {
    allocator: Allocator,
    str_bytes: std.ArrayList(u8),
    map: std.AutoArrayHashMap(u32, Range),
    id: u32 = 0,

    pub fn init(alc: Allocator) StringPool {
        return StringPool{
            .allocator = alc,
            .str_bytes = std.ArrayList(u8).init(alc),
            .map = std.AutoArrayHashMap(u32, Range).init(alc),
        };
    }

    pub fn put(self: *StringPool, str: []const u8) !u32 {
        const start = self.str_bytes.items.len;
        try self.str_bytes.appendSlice(str);
        const end = self.str_bytes.items.len;
        const id = self.id;
        try self.map.put(id, Range{ .start = start, .end = end });
        self.id += 1;
        return id;
    }

    pub fn get(self: StringPool, id: u32) ?[]const u8 {
        if (self.map.get(id)) |range| {
            return self.str_bytes.items[range.start..range.end];
        } else return null;
    }

    pub fn deinit(self: *StringPool) void {
        self.map.deinit();
        self.str_bytes.deinit();
    }
};

const Range = struct {
    start: u64,
    end: u64,
};
