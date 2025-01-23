const std = @import("std");
const Allocator = std.mem.Allocator;
const Token = @import("lexer/lexer.zig").Token;
const error_code = @import("error_code.zig");
const lexer = @import("lexer/lexer.zig");

// pub fn main() !void {
//     var gpa = std.heap.GeneralPurposeAllocator(.{}){};
//     defer if (gpa.deinit() == .leak) std.debug.print("leaked\n", .{});
//     const alc = gpa.allocator();

//     var virtual_fs = Vfs.init(alc);
//     defer virtual_fs.deinit();

//     const path = try Src.resolve(alc, "test.vivi");
//     defer alc.free(path);
//     std.debug.print("{s}\n", .{path});

//     const src_id = try virtual_fs.addFile(path);

//     const tokens = try lexer.lex(alc, virtual_fs.fileContent(src_id) orelse return error.FUCKOFF);
//     defer tokens.deinit();

//     for (tokens.items) |token| {
//         const span = Span{
//             .src = src_id,
//             .token = token,
//         };
//         std.debug.print("> {s}\n", .{virtual_fs.srcContent(span) orelse "||||||"});
//     }

//     try virtual_fs.reportB(
//         BigSpan{ .from = Span{
//             .src = src_id,
//             .token = tokens.items[2],
//         }, .to = Span{
//             .src = src_id,
//             .token = tokens.items[4],
//         } },
//         .err,
//         .TestingErr,
//         "span指向一个id",
//         2,
//     );
// }

pub const Vfs = struct {
    // allocator to use for all allocations
    allocator: Allocator,
    srcs: std.ArrayList(Src),

    pub fn init(alc: Allocator) Vfs {
        return Vfs{
            .allocator = alc,
            .srcs = std.ArrayList(Src).init(alc),
        };
    }

    pub fn addFile(self: *Vfs, file: []const u8) !u32 {
        const src = try Src.init(self.allocator, file);
        try self.srcs.append(src);
        const id = self.srcs.items.len - 1;
        return @as(u32, @intCast(id));
    }

    pub fn fileContent(self: Vfs, id: u32) ?[]const u8 {
        if (id >= self.srcs.items.len) return null;
        return self.srcs.items[id].content;
    }

    pub fn pathOf(self: Vfs, span: Span) ?[]const u8 {
        if (span.src >= self.srcs.items.len) return null;
        return self.srcs.items[span.src].path;
    }

    pub fn report(
        self: *Vfs,
        span: Span,
        kind: error_code.Kind,
        code: error_code.GlobalErr,
        label_info: []const u8,
        extra_lines: u64,
    ) !void {
        const src = self.srcs.items[span.src];
        try src.report(self.allocator, span, kind, code, label_info, extra_lines);
    }

    pub fn reportB(
        self: *Vfs,
        span: BigSpan,
        kind: error_code.Kind,
        code: error_code.GlobalErr,
        label_info: []const u8,
        extra_lines: u64,
    ) !void {
        const src = self.srcs.items[span.from.src];
        try src.reportB(self.allocator, span, kind, code, label_info, extra_lines);
    }

    pub fn srcContent(self: Vfs, span: Span) ?[]const u8 {
        if (span.src >= self.srcs.items.len) return null;
        const src = self.srcs.items[span.src];
        return src.content[span.token.from..span.token.to];
    }

    pub fn srcContentB(self: Vfs, span: BigSpan) ?[]const u8 {
        if (span.from.src != span.to.src) return null;
        if (span.from.src >= self.srcs.items.len) return null;
        const src = self.srcs.items[span.from.src];
        return src.content[span.from.token.from..span.to.token.to];
    }

    const reset = "\x1b[0m\x1b[39m";

    pub fn deinit(self: *Vfs) void {
        for (self.srcs.items) |*src| {
            src.deinit(self.allocator);
        }
        self.srcs.deinit();
    }
};

pub const Src = struct {
    // absolute path to the file
    path: []const u8,
    content: []const u8,
    lines: std.ArrayList(Line),

    pub fn init(alc: Allocator, file: []const u8) !Src {
        const path = try alc.alloc(u8, file.len);
        std.mem.copyForwards(u8, path, file);
        const src_file = try std.fs.openFileAbsolute(file, .{ .mode = .read_only });
        defer src_file.close();

        const content = try src_file.readToEndAlloc(alc, 1024 * 1024 * 1024);

        // record lines
        var lines = std.ArrayList(Line).init(alc);
        var line_start: u64 = 0;
        var i: u64 = 0;
        while (i < content.len) {
            const c = content[i];
            if (c == '\n') {
                try lines.append(Line{ .from = line_start, .to = i });
                line_start = i + 1;
            }
            i += 1;
        }
        try lines.append(Line{ .from = line_start, .to = content.len });
        return Src{ .path = path, .content = content, .lines = lines };
    }
    const reset_color = "\x1b[0m\x1b[39m";

    pub fn report(
        self: Src,
        alc: Allocator,
        span: Span,
        kind: error_code.Kind,
        code: error_code.GlobalErr,
        label_info: []const u8,
        extra_lines: u64,
    ) !void {
        const path = self.path;
        const lines = self.locateToken(span.token, extra_lines);
        const err_msg = error_code.errorMsg(code) orelse "unknown error";
        var buffer = std.ArrayList(u8).init(alc);
        defer buffer.deinit();
        const writer = buffer.writer();

        const kind_str = switch (kind) {
            error_code.Kind.err => "error",
            error_code.Kind.warn => "warning",
            error_code.Kind.info => "info",
            error_code.Kind.help => "help",
            error_code.Kind.note => "note",
        };
        const kind_color = switch (kind) {
            error_code.Kind.err => "\x1b[31m",
            error_code.Kind.warn => "\x1b[33m",
            error_code.Kind.info => "\x1b[36m",
            error_code.Kind.help => "\x1b[32m",
            error_code.Kind.note => "\x1b[34m",
        };
        try writer.print(
            "{s}{s}[{d}]: {s}{s}\n",
            .{
                kind_color,
                kind_str,
                @intFromEnum(code),
                err_msg,
                reset_color,
            },
        );
        try writer.print("{s}:\n", .{path});
        for (lines.lines, lines.start_line..) |line, line_idx| {
            const line_content = self.content[line.from..line.to];
            try writer.print("{d:>6} | {s}\n", .{ line_idx + 1, line_content });
            if (span.token.from >= line.from and span.token.to <= line.to) {
                const column = span.token.from - line.from;
                for (0..column + 9) |_| {
                    try writer.writeByte(' ');
                }
                for (span.token.from..span.token.to) |_| {
                    try writer.writeByte('^');
                }
                try writer.print(" {s}\n", .{label_info});
            }
        }

        std.debug.print("{s}", .{buffer.items});
    }

    pub fn reportB(
        self: Src,
        alc: Allocator,
        span: BigSpan,
        kind: error_code.Kind,
        code: error_code.GlobalErr,
        label_info: []const u8,
        extra_lines: u64,
    ) !void {
        const path = self.path;
        const lines = self.locateTokens(span.from.token, span.to.token, extra_lines);
        const err_msg = error_code.errorMsg(code) orelse "unknown error";
        var buffer = std.ArrayList(u8).init(alc);
        defer buffer.deinit();
        const writer = buffer.writer();
        const kind_str = switch (kind) {
            error_code.Kind.err => "error",
            error_code.Kind.warn => "warning",
            error_code.Kind.info => "info",
            error_code.Kind.help => "help",
            error_code.Kind.note => "note",
        };
        const kind_color = switch (kind) {
            error_code.Kind.err => "\x1b[31m",
            error_code.Kind.warn => "\x1b[33m",
            error_code.Kind.info => "\x1b[36m",
            error_code.Kind.help => "\x1b[32m",
            error_code.Kind.note => "\x1b[34m",
        };
        try writer.print(
            "{s}{s}[{d}]: {s}{s}\n",
            .{
                kind_color,
                kind_str,
                @intFromEnum(code),
                err_msg,
                reset_color,
            },
        );
        try writer.print("{s}:\n", .{path});
        for (lines.lines, lines.start_line..) |line, line_idx| {
            const line_content = self.content[line.from..line.to];
            try writer.print("{d:>6} | {s}\n", .{ line_idx + 1, line_content });
            // the first line
            if (span.from.token.from >= line.from and span.from.token.to <= line.to) {
                const column = span.from.token.from - line.from;
                for (0..column + 9) |_| {
                    try writer.writeByte(' ');
                }
                const min = if (span.to.token.to < line.to) span.to.token.to else line.to;
                for (span.from.token.from..min) |_| {
                    try writer.writeByte('^');
                }
                if (span.to.token.to <= line.to)
                    try writer.print(" {s}\n", .{label_info})
                else
                    try writer.writeByte('\n');
            } else if (span.from.token.from < line.from and span.to.token.to > line.to) {
                // the middle lines
                for (0..9) |_| {
                    try writer.writeByte(' ');
                }
                for (line.from..line.to) |_| {
                    try writer.writeByte('^');
                }
                try writer.writeByte('\n');
            } else if (span.to.token.from >= line.from and span.to.token.to <= line.to) {
                // the last line
                const column = span.to.token.to - line.from;
                try writer.writeAll("         ");
                for (0..column) |_| {
                    try writer.writeByte('^');
                }
                try writer.print(" {s}\n", .{label_info});
            }
        }

        std.debug.print("{s}", .{buffer.items});
    }
    // don't forget to free the result
    pub fn resolve(alc: Allocator, unresolved: []const u8) ![]const u8 {
        const cwd = try std.fs.cwd().realpathAlloc(alc, ".");
        defer alc.free(cwd);
        return try std.fs.path.resolve(alc, &.{ cwd, unresolved });
    }

    // binary search for the line number
    pub fn locate(self: Src, pos: u64) u64 {
        var lo: u64 = 0;
        var hi: u64 = self.lines.items.len;
        while (lo < hi) {
            const mid = lo + (hi - lo) / 2;
            const line = self.lines.items[mid];
            if (pos < line.from) {
                hi = mid;
            } else if (pos > line.to) {
                lo = mid + 1;
            } else {
                return mid;
            }
        }
        return lo;
    }

    // one token may cross multiple lines
    pub fn locateToken(self: Src, token: Token, extra_lines: u64) LinesSlice {
        var from = self.locate(token.from);
        from = if (from < extra_lines) 0 else from - extra_lines;
        var to = self.locate(token.to);
        to = if (to + extra_lines >= self.lines.items.len) self.lines.items.len - 1 else to + extra_lines;
        return LinesSlice{ .lines = self.lines.items[from .. to + 1], .start_line = from };
    }

    pub fn locateTokens(self: Src, token_begin: Token, token_end: Token, extra_lines: u64) LinesSlice {
        var from = self.locate(token_begin.from);
        from = if (from < extra_lines) 0 else from - extra_lines;
        var to = self.locate(token_end.to);
        to = if (to + extra_lines >= self.lines.items.len) self.lines.items.len - 1 else to + extra_lines;
        return LinesSlice{ .lines = self.lines.items[from .. to + 1], .start_line = from };
    }

    pub fn deinit(self: *Src, alc: Allocator) void {
        self.lines.deinit();
        alc.free(self.path);
        alc.free(self.content);
    }
};

pub const Span = struct {
    token: Token,
    src: u32,

    pub fn fromToken(token: Token, src: u32) Span {
        return Span{ .token = token, .src = src };
    }
};

pub const BigSpan = struct {
    from: Span,
    to: Span,

    pub fn fromSpans(from: Span, to: Span) BigSpan {
        return BigSpan{ .from = from, .to = to };
    }
};

const Line = struct {
    from: u64,
    to: u64,
};

const LinesSlice = struct {
    lines: []const Line,
    start_line: u64,
};
