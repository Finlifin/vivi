pub const Lexer = struct {
    src: []const u8,
    cursor: Index,

    // todo
    err: void,

    const Self = @This();

    pub fn next(self: *Self) Token {
        const b = self.src;
        var old_cursor = self.cursor;

        while (true) : (self.cursor += 1) {
            if (self.cursor >= self.src.len) return token(.eof, 0, 0);
            const c = b[self.cursor];

            switch (c) {
                '^' => {
                    self.cursor += 1;
                    return token(.@"^", old_cursor, self.cursor);
                },
                '.' => {
                    self.cursor += 1;
                    return token(.@".", old_cursor, self.cursor);
                },
                '\'' => {
                    // recognize char
                    // TODO: escape
                    if (self.cursor + 2 < self.src.len and b[self.cursor + 2] == '\'') {
                        self.cursor += 3;
                        return token(.char, old_cursor, self.cursor);
                    }
                    self.cursor += 1;
                    return token(.@"'", old_cursor, self.cursor);
                },
                '@' => {
                    self.cursor += 1;
                    return token(.@"@", old_cursor, self.cursor);
                },

                '\\' => {
                    self.cursor += 1;
                    return token(.@"\\", old_cursor, self.cursor);
                },
                '&' => {
                    self.cursor += 1;
                    return token(.@"&", old_cursor, self.cursor);
                },
                '(' => {
                    self.cursor += 1;
                    return token(.@"(", old_cursor, self.cursor);
                },
                ')' => {
                    self.cursor += 1;
                    return token(.@")", old_cursor, self.cursor);
                },
                '[' => {
                    self.cursor += 1;
                    return token(.@"[", old_cursor, self.cursor);
                },
                ']' => {
                    self.cursor += 1;
                    return token(.@"]", old_cursor, self.cursor);
                },
                ';' => {
                    self.cursor += 1;
                    return token(.@";", old_cursor, self.cursor);
                },
                ',' => {
                    self.cursor += 1;
                    return token(.@",", old_cursor, self.cursor);
                },
                '?' => {
                    self.cursor += 1;
                    return token(.@"?", old_cursor, self.cursor);
                },
                '~' => {
                    if (self.eatChar('>')) {
                        self.cursor += 1;
                        return token(.@"~>", old_cursor, self.cursor);
                    } else {
                        self.cursor += 1;
                        return token(.@"~", old_cursor, self.cursor);
                    }
                },
                '#' => {
                    self.cursor += 1;
                    return token(.@"#", old_cursor, self.cursor);
                },
                '|' => {
                    if (self.eatChar('>')) {
                        self.cursor += 1;
                        return token(.@"|>", old_cursor, self.cursor);
                    } else {
                        self.cursor += 1;
                        return token(.@"|", old_cursor, self.cursor);
                    }
                },
                '>' => {
                    if (self.seperated(self.cursor)) {
                        self.cursor += 2;
                        return token(.@" > ", old_cursor, self.cursor);
                    } else {
                        if (self.eatChar('=')) {
                            self.cursor += 1;
                            return token(.@">=", old_cursor, self.cursor);
                        } else {
                            self.cursor += 1;
                            return token(.@">", old_cursor, self.cursor);
                        }
                    }
                },

                '<' => {
                    if (self.seperated(self.cursor)) {
                        self.cursor += 2;
                        return token(.@" < ", old_cursor, self.cursor);
                    } else {
                        if (self.eatChar('=')) {
                            self.cursor += 1;
                            return token(.@"<=", old_cursor, self.cursor);
                        } else {
                            self.cursor += 1;
                            return token(.@"<", old_cursor, self.cursor);
                        }
                    }
                },

                '+' => {
                    if (self.seperated(self.cursor)) {
                        self.cursor += 2;
                        return token(.@" + ", old_cursor, self.cursor);
                    } else {
                        if (self.eatChar('=')) {
                            self.cursor += 1;
                            return token(.@"+=", old_cursor, self.cursor);
                        } else if (self.eatChar('+')) {
                            self.cursor += 1;
                            return token(.@"++", old_cursor, self.cursor);
                        } else {
                            self.cursor += 1;
                            return token(.@"+", old_cursor, self.cursor);
                        }
                    }
                },
                '-' => {
                    if (self.seperated(self.cursor)) {
                        self.cursor += 2;
                        return token(.@" - ", old_cursor, self.cursor);
                    } else {
                        if (self.eatChar('=')) {
                            self.cursor += 1;
                            return token(.@"-=", old_cursor, self.cursor);
                        } else if (self.eatChar('>')) {
                            self.cursor += 1;
                            return token(.@"->", old_cursor, self.cursor);
                        } else if (self.eatChar('-')) {
                            // comment
                            while (true) : (self.cursor += 1) {
                                if (self.cursor >= self.src.len) return token(.comment, old_cursor, self.cursor - 1);
                                const c_ = b[self.cursor];
                                if (c_ == '\n') {
                                    self.cursor += 1;
                                    return token(.comment, old_cursor, self.cursor);
                                }
                            }
                        } else {
                            self.cursor += 1;
                            return token(.@"-", old_cursor, self.cursor);
                        }
                    }
                },

                '!' => {
                    if (self.eatChar('=')) {
                        self.cursor += 1;
                        return token(.@"!=", old_cursor, self.cursor);
                    } else {
                        self.cursor += 1;
                        return token(.@"!", old_cursor, self.cursor);
                    }
                },

                '=' => {
                    if (self.eatChar('=')) {
                        self.cursor += 1;
                        return token(.@"==", old_cursor, self.cursor);
                    } else if (self.eatChar('>')) {
                        self.cursor += 1;
                        return token(.@"=>", old_cursor, self.cursor);
                    } else {
                        self.cursor += 1;
                        return token(.@"=", old_cursor, self.cursor);
                    }
                },

                '%' => {
                    if (self.seperated(self.cursor)) {
                        self.cursor += 2;
                        return token(.@" % ", old_cursor, self.cursor);
                    } else {
                        if (self.eatChar('=')) {
                            self.cursor += 1;
                            return token(.@"%=", old_cursor, self.cursor);
                        } else {
                            self.cursor += 1;
                            return token(.@"%", old_cursor, self.cursor);
                        }
                    }
                },
                '{' => {
                    if (self.eatChar('-')) {
                        // comment
                        while (true) : (self.cursor += 1) {
                            if (self.cursor >= self.src.len) return token(.comment, old_cursor, self.cursor - 1);
                            const c_ = b[self.cursor];
                            if (c_ == '-') {
                                if (self.eatChar('}')) {
                                    self.cursor += 1;
                                    return token(.comment, old_cursor, self.cursor);
                                }
                            }
                        }
                    } else {
                        self.cursor += 1;
                        return token(.@"{", old_cursor, self.cursor);
                    }
                },
                '}' => {
                    self.cursor += 1;
                    return token(.@"}", old_cursor, self.cursor);
                },
                ':' => {
                    if (self.eatChar('-')) {
                        self.cursor += 1;
                        return token(.@":-", old_cursor, self.cursor);
                    } else if (self.eatChar(':')) {
                        self.cursor += 1;
                        return token(.@"::", old_cursor, self.cursor);
                    } else {
                        self.cursor += 1;
                        return token(.@":", old_cursor, self.cursor);
                    }
                },
                '*' => {
                    if (self.seperated(self.cursor)) {
                        self.cursor += 2;
                        return token(.@" * ", old_cursor, self.cursor);
                    } else if (self.eatChar('=')) {
                        self.cursor += 1;
                        return token(.@"*=", old_cursor, self.cursor);
                    } else {
                        self.cursor += 1;
                        return token(.@"*", old_cursor, self.cursor);
                    }
                },
                '/' => {
                    if (self.seperated(self.cursor)) {
                        self.cursor += 2;
                        return token(.@" / ", old_cursor, self.cursor);
                    } else if (self.eatChar('=')) {
                        self.cursor += 1;
                        return token(.@"/=", old_cursor, self.cursor);
                    } else {
                        self.cursor += 1;
                        return token(.@"/", old_cursor, self.cursor);
                    }
                },
                '_', 'a'...'z', 'A'...'Z' => return self.recognizeId(),

                '`' => return self.recognizeArbitaryId(),

                '0'...'9' => return self.recognizeDigit(),

                ' ', '\t', '\n' => {
                    old_cursor += 1;
                },

                '"' => return self.recognizeStr(),

                else => {
                    std.debug.print("{c}\n", .{c});
                    unreachable;
                },
            }
        }

        return token(.eof, b.len, b.len);
    }
    pub fn init(src: []const u8) Self {
        return .{
            .src = src,
            .cursor = 0,
            .err = undefined,
        };
    }
    pub fn deinit(_: *Self) void {}

    fn recognizeStr(self: *Self) Token {
        const b = self.src;
        const old_cursor = self.cursor;
        var started: bool = false;

        while (true) : (self.cursor += 1) {
            if (self.cursor >= self.src.len) return token(.eof, 0, 0);
            const c = b[self.cursor];

            switch (c) {
                '"' => {
                    if (!started)
                        started = true
                    else if (self.cursor - 1 >= 0 and (b[self.cursor - 1] != '\\')) {
                        self.cursor += 1;
                        return token(.str, old_cursor, self.cursor);
                    }
                },
                else => {},
            }
        }
        return token(.invalid, old_cursor, self.cursor);
    }
    fn recognizeMultiLineStr() void {}
    fn recognizeComment() void {}

    // BUG: `some`，当`为最后一个字符时，len不包括其在内
    fn recognizeId(self: *Self) Token {
        const b = self.src;
        const old_cursor = self.cursor;
        while (true) : (self.cursor += 1) {
            if (self.cursor >= self.src.len)
                if (Token.keywords.get(b[old_cursor..self.cursor])) |kw|
                    return token(kw, old_cursor, self.cursor)
                else {
                    return token(.id, old_cursor, self.cursor);
                };
            const c = b[self.cursor];

            switch (c) {
                'a'...'z', 'A'...'Z', '0'...'9', '_' => {},
                else => {
                    if (Token.keywords.get(b[old_cursor..self.cursor])) |kw|
                        return token(kw, old_cursor, self.cursor)
                    else {
                        return token(.id, old_cursor, self.cursor);
                    }
                },
            }
        }
    }

    fn recognizeDigit(self: *Self) Token {
        const b = self.src;
        const old_cursor = self.cursor;

        while (true) : (self.cursor += 1) {
            if (self.cursor >= self.src.len) return token(.int, old_cursor, self.cursor);
            const c = b[self.cursor];
            switch (c) {
                '0'...'9' => {},
                else => return token(.int, old_cursor, self.cursor),
            }
        }
    }
    fn recognizeArbitaryId(self: *Self) Token {
        const b = self.src;
        const old_cursor = self.cursor;
        var started: bool = false;

        while (true) : (self.cursor += 1) {
            if (self.cursor >= self.src.len) return token(.eof, 0, 0);
            const c = b[self.cursor];

            switch (c) {
                '`' => {
                    if (!started)
                        started = true
                    else {
                        self.cursor += 1;
                        return token(.id, old_cursor, self.cursor);
                    }
                },
                else => {},
            }
        }
        return token(.invalid, old_cursor, self.cursor);
    }

    fn peek(self: Self, char: u8) bool {
        const at = self.cursor;
        return if (self.src.len <= at + 1) false else if (self.src[at + 1] == char) true else false;
    }

    fn seperated(self: Self, at: Index) bool {
        return if (self.src.len <= at + 1 or at < 1) false else if (self.src[at + 1] == ' ' and self.src[at - 1] == ' ') true else false;
    }

    fn eatChar(self: *Self, char: u8) bool {
        var result: bool = false;

        if (self.src.len <= self.cursor + 1) return false;

        if (self.src[self.cursor + 1] == char) {
            self.cursor += 1;
            result = true;
        }

        return result;
    }
};

pub const Token = struct {
    tag: Tag,
    from: Index,
    to: Index,

    pub const Tag = enum(Index) {
        // operators
        @"+",
        @"+=",
        @"++",
        @" + ",
        @"<",
        @"<=",
        @" < ",
        @">",
        @">=",
        @" > ",
        @"!",
        @"!=",
        @"-",
        @"->",
        @"-=",
        @" - ",
        @".",
        @"..=",
        @":",
        @"::",
        @":-",
        @"*",
        @"*=",
        @" * ",
        @"/",
        @"/=",
        @" / ",
        @"%",
        @"%=",
        @" % ",
        @"=",
        @"=>",
        @"==",
        @"==>",
        @"~",
        @"~>",
        @"|",
        @"|>",
        @"#",
        @"?",
        @"\\",
        @"&",
        @"[",
        @"]",
        @"(",
        @")",
        @"{",
        @"}",
        @",",
        @"'",
        @";",
        @"^",
        @"$",
        @"@",
        @"_",

        // permitive literals
        str,
        int,
        real,
        char,

        // keywords
        k_and,
        k_any,
        k_Any,
        k_as,
        k_asserts,
        k_asume,
        k_async,
        k_atomic,
        k_await,
        k_bool,
        k_break,
        k_case,
        k_catch,
        k_comptime,
        k_const,
        k_continue,
        k_decreases,
        k_defer,
        k_define,
        k_derive,
        k_do,
        k_dyn,
        k_effect,
        k_else,
        k_ensures,
        k_enum,
        k_errdefer,
        k_error,
        k_exists,
        k_extern,
        k_false,
        k_fn,
        k_Fn,
        k_FnMut,
        k_FnOnce,
        k_for,
        k_forall,
        k_ghost,
        k_handle,
        k_if,
        k_impl,
        k_in,
        k_inline,
        k_invariant,
        k_is,
        k_lemma,
        k_let,
        k_lifting,
        k_match,
        k_move,
        k_mod,
        k_mut,
        k_mutdyn,
        k_mutptr,
        k_mutref,
        k_newtype,
        k_noreturn,
        k_not,
        k_null,
        k_opaque,
        k_opens,
        k_or,
        k_pcfn,
        k_perform,
        k_predicate,
        k_ptr,
        k_pub,
        k_pure,
        k_ref,
        k_refines,
        k_requires,
        k_resume,
        k_return,
        k_returns,
        k_self,
        k_Self,
        k_static,
        k_struct,
        k_test,
        k_trait,
        k_true,
        k_tuple,
        k_typealias,
        k_undefined,
        k_union,
        k_unit,
        k_unreachable,
        k_unsafe,
        k_use,
        k_void,
        k_when,
        k_while,
        k_where,

        // others
        id,
        macro_content,
        comment,
        invalid,
        sof,
        eof,
    };

    const keywords = std.StaticStringMap(Tag).initComptime(.{
        .{ "and", .k_and },
        .{ "any", .k_any },
        .{ "Any", .k_Any },
        .{ "as", .k_as },
        .{ "asserts", .k_asserts },
        .{ "asume", .k_asume },
        .{ "async", .k_async },
        .{ "atomic", .k_atomic },
        .{ "await", .k_await },
        .{ "bool", .k_bool },
        .{ "break", .k_break },
        .{ "case", .k_case },
        .{ "catch", .k_catch },
        .{ "comptime", .k_comptime },
        .{ "const", .k_const },
        .{ "continue", .k_continue },
        .{ "decreases", .k_decreases },
        .{ "define", .k_define },
        .{ "defer", .k_defer },
        .{ "derive", .k_derive },
        .{ "do", .k_do },
        .{ "dyn", .k_dyn },
        .{ "effect", .k_effect },
        .{ "else", .k_else },
        .{ "ensures", .k_ensures },
        .{ "enum", .k_enum },
        .{ "errdefer", .k_errdefer },
        .{ "error", .k_error },
        .{ "exists", .k_exists },
        .{ "extern", .k_extern },
        .{ "false", .k_false },
        .{ "fn", .k_fn },
        .{ "Fn", .k_Fn },
        .{ "FnMut", .k_FnMut },
        .{ "FnOnce", .k_FnOnce },
        .{ "for", .k_for },
        .{ "forall", .k_forall },
        .{ "ghost", .k_ghost },
        .{ "handle", .k_handle },
        .{ "if", .k_if },
        .{ "impl", .k_impl },
        .{ "in", .k_in },
        .{ "inline", .k_inline },
        .{ "invariant", .k_invariant },
        .{ "is", .k_is },
        .{ "lemma", .k_lemma },
        .{ "let", .k_let },
        .{ "lifting", .k_lifting },
        .{ "match", .k_match },
        .{ "move", .k_move },
        .{ "mod", .k_mod },
        .{ "mut", .k_mut },
        .{ "mutdyn", .k_mutdyn },
        .{ "mutptr", .k_mutptr },
        .{ "mutref", .k_mutref },
        .{ "newtype", .k_newtype },
        .{ "noreturn", .k_noreturn },
        .{ "not", .k_not },
        .{ "null", .k_null },
        .{ "opaque", .k_opaque },
        .{ "opens", .k_opens },
        .{ "or", .k_or },
        .{ "pcfn", .k_pcfn },
        .{ "perform", .k_perform },
        .{ "predicate", .k_predicate },
        .{ "ptr", .k_ptr },
        .{ "pub", .k_pub },
        .{ "pure", .k_pure },
        .{ "ref", .k_ref },
        .{ "refines", .k_refines },
        .{ "requires", .k_requires },
        .{ "resume", .k_resume },
        .{ "return", .k_return },
        .{ "returns", .k_returns },
        .{ "self", .k_self },
        .{ "Self", .k_Self },
        .{ "static", .k_static },
        .{ "struct", .k_struct },
        .{ "test", .k_test },
        .{ "trait", .k_trait },
        .{ "true", .k_true },
        .{ "tuple", .k_tuple },
        .{ "typealias", .k_typealias },
        .{ "undefined", .k_undefined },
        .{ "union", .k_union },
        .{ "unit", .k_unit },
        .{ "unreachable", .k_unreachable },
        .{ "unsafe", .k_unsafe },
        .{ "use", .k_use },
        .{ "void", .k_void },
        .{ "when", .k_when },
        .{ "while", .k_while },
        .{ "where", .k_where },
    });
};

inline fn token(tag: Token.Tag, from: Index, to: Index) Token {
    return .{ .tag = tag, .from = from, .to = to };
}

const Index = u32;
const std = @import("std");

pub fn lex(gpa: std.mem.Allocator, src: []const u8) !std.ArrayList(Token) {
    var lexer = Lexer.init(src);
    var result = std.ArrayList(Token).init(gpa);
    try result.append(token(.sof, 0, 0));
    while (true) {
        const t = lexer.next();
        if (t.tag != .comment)
            try result.append(t);
        if (t.tag == .eof) break;
    }
    return result;
}
