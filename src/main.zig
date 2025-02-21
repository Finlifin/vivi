const std = @import("std");
const parser = @import("parser/parser.zig");
const lexer = @import("lexer/lexer.zig");
const vfs = @import("vfs.zig");
const string_pool = @import("common/string_pool.zig");
const TypePool = @import("vm/type_pool.zig").TypePool;
const Analyzer = @import("analysis1/analyzer.zig").Analyzer;

// pub fn main() !void {
//     var gpa = std.heap.GeneralPurposeAllocator(.{}){};
//     defer if (gpa.deinit() == .leak) std.debug.print("leaked\n", .{});
//     const alc = gpa.allocator();

//     var virtual_fs = vfs.Vfs.init(alc);
//     defer virtual_fs.deinit();

//     var string_pool_ = string_pool.StringPool.init(alc);
//     defer string_pool_.deinit();
//     _ = try string_pool_.put("invalid");

//     const path = try vfs.Src.resolve(alc, "test.vivi");
//     defer alc.free(path);

//     const src_id = try virtual_fs.addFile(path);
//     var parser_ = try parser.Parser.init(alc, &virtual_fs, src_id, .{});
//     var s_expression_buffer = std.ArrayList(u8).init(alc);
//     defer s_expression_buffer.deinit();

//     const node = parser_.parse();
//     // parser_.dumpTokens();
//     for (parser_.nodes.items, 0..) |n, i| {
//         std.debug.print("%{d}:{d} ", .{ i, n });
//     }
//     for (parser_.tags.items, parser_.tags_location.items, 0..) |tag, w, i| {
//         std.debug.print("%{}:{s}:{}  ", .{ i, @tagName(tag), w - 1 });
//     }

//     std.debug.print("\n", .{});
//     try parser_.dump(node, s_expression_buffer.writer());
//     std.debug.print("{s}\n", .{s_expression_buffer.items});

//     const dump_file = try std.fs.cwd().createFile("dump.lisp", .{ .read = true, .truncate = true });
//     defer dump_file.close();

//     var ast = parser_.intoAst();
//     defer ast.deinit();
//     try ast.dump(node, dump_file.writer());

//     var type_pool = try TypePool.init(alc, &string_pool_);
//     defer type_pool.deinit();

//     var analyzer = try Analyzer.init(alc, &string_pool_, &type_pool, &ast, node, .{});
//     defer analyzer.deinit();

//     try analyzer.analyzeFileScope(try string_pool_.put("test.vivi"));

//     // const B_sp = try string_pool_.put("N");
//     // const mod_B = try analyzer.resolve(B_sp);
//     // if (mod_B) |item|
//     //     try analyzer.ast.report(
//     //         item.ast_node,
//     //         .info,
//     //         .TestingErr,
//     //         "你自己看看这啥玩意",
//     //         3,
//     //     );

//     const mod_E = try analyzer.resolve(try string_pool_.put("E"));
//     const hir = @import("analysis1/hir.zig");
//     for (hir.Children.Entry.entries(hir.ModDef.at(mod_E.?.obj.?).children)) |entry| {
//         try analyzer.ast.report(
//             entry.ast_node,
//             .info,
//             .TestingErr,
//             "mod E中的成员之一",
//             3,
//         );
//     }
// }

const Gc = @import("analysis1/gc.zig").Gc;
const hir = @import("analysis1/hir.zig");
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
