const std = @import("std");
const parser = @import("parser/parser.zig");
const lexer = @import("lexer/lexer.zig");
const vfs = @import("vfs.zig");
const string_pool = @import("common/string_pool.zig");
const TypePool = @import("vm/type_pool.zig").TypePool;
const Analyzer = @import("analysis1/analyzer.zig").Analyzer;

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer if (gpa.deinit() == .leak) std.debug.print("leaked\n", .{});
    const alc = gpa.allocator();

    var virtual_fs = vfs.Vfs.init(alc);
    defer virtual_fs.deinit();

    var string_pool_ = string_pool.StringPool.init(alc);
    defer string_pool_.deinit();
    _ = try string_pool_.put("invalid");

    const path = try vfs.Src.resolve(alc, "test.vivi");
    defer alc.free(path);

    const src_id = try virtual_fs.addFile(path);
    var parser_ = try parser.Parser.init(alc, &virtual_fs, src_id, .{});
    var s_expression_buffer = std.ArrayList(u8).init(alc);
    defer s_expression_buffer.deinit();

    const node = parser_.parse();
    // parser_.dumpTokens();
    for (parser_.nodes.items, 0..) |n, i| {
        std.debug.print("%{d}:{d} ", .{ i, n });
    }
    for (parser_.tags.items, parser_.tags_location.items, 0..) |tag, w, i| {
        std.debug.print("%{}:{s}:{}  ", .{ i, @tagName(tag), w - 1 });
    }

    std.debug.print("\n", .{});
    try parser_.dump(node, s_expression_buffer.writer());
    std.debug.print("{s}\n", .{s_expression_buffer.items});

    const dump_file = try std.fs.cwd().createFile("dump.lisp", .{ .read = true, .truncate = true });
    defer dump_file.close();

    var ast = parser_.intoAst();
    defer ast.deinit();
    try ast.dump(node, dump_file.writer());

    var type_pool = try TypePool.init(alc, &string_pool_);
    defer type_pool.deinit();

    var analyzer = try Analyzer.init(alc, &string_pool_, &type_pool, &ast, node, .{});
    defer analyzer.deinit();

    try analyzer.analyzeFileScope(try string_pool_.put("test.vivi"));

    // const B_sp = try string_pool_.put("N");
    // const mod_B = try analyzer.resolve(B_sp);
    // if (mod_B) |item|
    //     try analyzer.ast.report(
    //         item.ast_node,
    //         .info,
    //         .TestingErr,
    //         "你自己看看这啥玩意",
    //         3,
    //     );

    const mod_E = try analyzer.resolve(try string_pool_.put("E"));
    const hir = @import("analysis1/hir.zig");
    for (hir.Children.Entry.entries(hir.ModDef.at(mod_E.?.obj.?).children)) |entry| {
        try analyzer.ast.report(
            entry.ast_node,
            .info,
            .TestingErr,
            "mod E中的成员之一",
            3,
        );
    }
}

// const Context = @import("analysis1/context.zig").Context;

// pub fn main() !void {
//     var gpa = std.heap.GeneralPurposeAllocator(.{}){};
//     defer if (gpa.deinit() == .leak) std.debug.print("leaked..", .{});

//     var sp = string_pool.StringPool.init(gpa.allocator());
//     defer sp.deinit();

//     var ctx = try Context.init(gpa.allocator(), &sp);
//     defer ctx.deinit();

//     const item = ctx.resolve(try sp.put("i64"));
//     std.debug.print("DEBUG: {any}\n", .{item});

//     const some = ctx.resolve(try sp.put("some"));
//     std.debug.print("DEBUG: {any}\n", .{some});
// }

// const hir = @import("analysis1/hir.zig");
// const context = @import("analysis1/context.zig");

// pub fn main() !void {
//     var gpa = std.heap.GeneralPurposeAllocator(.{}){};
//     defer if (gpa.deinit() == .leak) std.debug.print("leaked..", .{});

//     var sp = string_pool.StringPool.init(gpa.allocator());
//     defer sp.deinit();

//     var ctx = try context.Context.init(gpa.allocator(), &sp);
//     defer ctx.deinit();

//     try ctx.gc.gc(&.{ctx.root});

//     std.debug.print("DEBUG: {any}\n", .{ctx.resolve(try sp.put("Real"))});
// }
