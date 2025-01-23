const std = @import("std");
const parser = @import("parser.zig");
const Err = parser.Err;
const error_code = @import("../error_code.zig");
const vfs = @import("../vfs.zig");

pub fn handle_error(self: *parser.Parser, err: Err) void {
    switch (err) {
        Err.UnexpectedToken => {
            const got = self.err.UnexpectedToken.got;
            const info = self.err.UnexpectedToken.info;
            const span = vfs.Span{
                .src = self.src_id,
                .token = got,
            };
            self.vfs.report(span, .err, .UnexpectedToken, info, 1) catch unreachable;
        },
        Err.InvalidExpr => {
            const start = self.err.InvalidExpr.start;
            const end = self.err.InvalidExpr.end + 1;
            const info = self.err.InvalidExpr.info;

            const begin_span = vfs.Span{
                .src = self.src_id,
                .token = self.tokens.items[start],
            };
            const end_span = vfs.Span{
                .src = self.src_id,
                .token = self.tokens.items[end],
            };
            const span = vfs.BigSpan{
                .from = begin_span,
                .to = end_span,
            };
            self.vfs.reportB(span, .err, .InvalidExpr, info, 3) catch unreachable;
        },
        Err.InvalidPattern => {
            const start = self.err.InvalidPattern.start;
            const end = self.err.InvalidPattern.end + 1;
            const info = self.err.InvalidPattern.info;
            const begin_span = vfs.Span{
                .src = self.src_id,
                .token = self.tokens.items[start],
            };
            const end_span = vfs.Span{
                .src = self.src_id,
                .token = self.tokens.items[end],
            };
            const span = vfs.BigSpan{
                .from = begin_span,
                .to = end_span,
            };
            self.vfs.reportB(span, .err, .InvalidPattern, info, 3) catch unreachable;
        },
        else => {},
    }
}
