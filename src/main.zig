const std = @import("std");
const parser = @import("parser/parser.zig");
const lexer = @import("lexer/lexer.zig");
const vfs = @import("vfs.zig");

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer if (gpa.deinit() == .leak) std.debug.print("leaked\n", .{});
    const alc = gpa.allocator();

    var virtual_fs = vfs.Vfs.init(alc);
    defer virtual_fs.deinit();

    const path = try vfs.Src.resolve(alc, "test.vivi");
    defer alc.free(path);

    const src_id = try virtual_fs.addFile(path);
    var parser_ = try parser.Parser.init(alc, &virtual_fs, src_id, .{});
    defer parser_.deinit();
    var s_expression_buffer = std.ArrayList(u8).init(alc);
    defer s_expression_buffer.deinit();

    const node = parser_.parse();
    // parser_.dumpTokens();
    for (parser_.nodes.items, 0..) |n, i| {
        std.debug.print("%{d}:{d} ", .{ i, n });
    }
    for (parser_.tags.items, parser_.tags_location.items) |tag, i| {
        std.debug.print("%{}:{s}  ", .{ i - 1, @tagName(tag) });
    }
    std.debug.print("\n", .{});
    std.debug.print("DEBUG: node {any}\n", .{node});
    try parser_.dump(node, s_expression_buffer.writer());
    std.debug.print("{s}\n", .{s_expression_buffer.items});

    const dump_file = try std.fs.cwd().createFile("dump.lisp", .{ .read = true, .truncate = true });
    defer dump_file.close();
    try parser_.dump(node, dump_file.writer());
}
