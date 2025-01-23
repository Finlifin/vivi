const std = @import("std");
const parser = @import("parser/parser.zig");
const lexer = @import("lexer/lexer.zig");
const vfs = @import("vfs.zig");

// pub fn main() !void {
//     var gpa = std.heap.GeneralPurposeAllocator(.{}){};
//     defer if (gpa.deinit() == .leak) std.debug.print("fuck leak\n", .{});
//     const alc = gpa.allocator();

//     const src_file = try std.fs.cwd().openFile("test.vivi", .{ .mode = .read_only });
//     const src = try src_file.readToEndAlloc(alc, 1024 * 1024 * 1024);
//     defer alc.free(src);

//     var p = try parser.Parser.init(alc, src);
//     defer p.deinit();

//     p.dumpTokens();
//     try p.eatToken(.sof);
//     const node = try p.pExpr();
//     p.dump(node, 0);
// }

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer if (gpa.deinit() == .leak) std.debug.print("leaked\n", .{});
    const alc = gpa.allocator();

    var virtual_fs = vfs.Vfs.init(alc);
    defer virtual_fs.deinit();

    const path = try vfs.Src.resolve(alc, "test.vivi");
    defer alc.free(path);
    std.debug.print("{s}\n", .{path});

    const src_id = try virtual_fs.addFile(path);

    const tokens = try lexer.lex(alc, virtual_fs.fileContent(src_id) orelse return error.FUCKOFF);
    defer tokens.deinit();

    for (tokens.items) |token| {
        const span = vfs.Span{
            .src = src_id,
            .token = token,
        };
        std.debug.print("> {s}\n", .{virtual_fs.srcContent(span) orelse "||||||"});
    }

    const big_span = vfs.BigSpan{
        .from = vfs.Span{
            .src = src_id,
            .token = tokens.items[5],
        },
        .to = vfs.Span{
            .src = src_id,
            .token = tokens.items[10],
        },
    };

    try virtual_fs.reportB(big_span, .warn, .TestingErr, "big span指向多个token");
    try virtual_fs.report(big_span.from, .err, .TestingErr, "span指向一个id");
}
