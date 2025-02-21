const type_pool = @import("../vm/type_pool.zig");
const string_pool = @import("../common/string_pool.zig");
const std = @import("std");
const ast = @import("../parser/ast.zig");

const gc = @import("gc.zig");
const Ref = gc.Ref;

pub const Tag = enum(u32) {
    root,
    mod_def,
    struct_def,
    enum_def,
    union_def,
    fn_def,
    trait_def,
    effect_def,
    const_def,

    children,
    impls,

    // exprs
    binary,
    int,
    float,
    str,
    char,

    unit,

    invalid,
};

pub const ModDef = struct {
    name: string_pool.Index,
    type_index: type_pool.Index,
    ast_node: u64,
    impls: void,
    parent: Ref,
    children: Ref,

    pub const tag = Tag.mod_def;

    comptime {
        std.debug.assert(@sizeOf(ModDef) == 40);
    }

    pub fn at(obj: [*]u8) *ModDef {
        return @as(*ModDef, @alignCast(@ptrCast(obj)));
    }

    pub inline fn resolve(obj: [*]u8, name: string_pool.Index) ?[*]u8 {
        const mod = ModDef.at(obj);
        return Children.resolve(mod.children, name);
    }

    pub const proto: gc.MetaData = .{
        .size_description = 0,
        .field_len = 6,
        .tag = Tag.mod_def,
    };
};

pub const ConstDef = struct {
    name: string_pool.Index,
    type_index: type_pool.Index,
    ast_node: u64,
    parent: Ref,
    type: Ref,
    value: Ref,

    pub fn at(obj: [*]u8) *ConstDef {
        return @as(*ConstDef, @alignCast(@ptrCast(obj)));
    }

    pub const proto: gc.MetaData = .{
        .size_description = 0,
        .field_len = 6,
        .tag = Tag.const_def,
    };
};

pub const Root = struct {
    children: [*]u8,

    comptime {
        std.debug.assert(@sizeOf(Root) == 8);
    }

    pub fn at(obj: [*]u8) *Root {
        return @as(*Root, @alignCast(@ptrCast(obj)));
    }
};

pub const Children = struct {
    pub fn resolve(children: [*]u8, name: string_pool.Index) ?*Symbol {
        for (Symbol.entries(children)) |*entry| {
            if (entry.name == name) return entry;
        }

        return null;
    }

    pub fn new(
        gc_: *gc.Gc,
        entries: []Symbol,
    ) ![*]u8 {
        const meta = gc.MetaData.init(0, @intCast(entries.len), Tag.children);
        const children = try gc_.new(meta);

        @memcpy(Symbol.entries(children), entries);

        return children;
    }

    pub const Symbol = struct {
        name: string_pool.Index,
        ast_node: u64,
        obj: ?[*]u8,

        pub fn at(obj: [*]u8) *Symbol {
            return @as(*Symbol, @alignCast(@ptrCast(obj)));
        }

        pub fn entries(obj: [*]u8) []Symbol {
            const meta = gc.MetaData.of(obj);
            const base = @as([*]Symbol, @alignCast(@ptrCast(obj)));
            return base[0..meta.field_len];
        }

        comptime {
            std.debug.assert(@sizeOf(Symbol) == 24);
        }
    };
};

pub const Item = struct {
    name: string_pool.Index,
    type_index: type_pool.Index,
    ast_node: u64,
    parent: ?[*]u8,

    pub fn at(obj: [*]u8) *Item {
        return @as(*Item, @alignCast(@ptrCast(obj)));
    }

    pub fn resolve(obj: [*]u8, name: string_pool.Index) ?*Children.Symbol {
        const meta = gc.MetaData.of(obj);
        var result: ?*Children.Symbol = null;
        switch (meta.tag) {
            .root => {
                const root = Root.at(obj);
                return Children.resolve(root.children, name);
            },
            .mod_def => {
                const mod = ModDef.at(obj);
                result = Children.resolve(mod.children, name);
            },
            // .struct_def => {
            //     const struct_def = StructDef.at(obj);
            //     result = Children.resolve(struct_def.scope.children, name);
            // },
            else => return null,
        }

        if (result) |entry| return entry;

        return resolve(Item.at(obj).parent.?, name);
    }
};

pub const Binary = struct {
    op: Op,
    type_index: type_pool.Index,
    ast_node: u64,
    left: [*]u8,
    right: [*]u8,

    pub const Op = enum(u64) {
        add,
        sub,
        mul,
        div,
        mod,
        bool_eq,
        bool_ne,
        bool_lt,
        bool_le,
        bool_gt,
        bool_ge,
        bool_and,
        bool_or,
    };

    pub const proto: gc.MetaData = .{
        .size_description = 0,
        .field_len = 5,
        .tag = Tag.binary,
    };

    pub fn at(obj: [*]u8) *Binary {
        return @as(*Binary, @alignCast(@ptrCast(obj)));
    }
};

pub const Atom = struct {
    pub fn new(gc_: *gc.Gc, ast_node: u64, tag: Tag) ![*]u8 {
        const meta = gc.MetaData.init(0, 2, tag);
        const result = try gc_.new(meta);
        Int.at(result).type_index = 0;
        Int.at(result).ast_node = ast_node;
        return result;
    }

    pub const Int = struct {
        type_index: type_pool.Index,
        ast_node: u64,
        value: i64,

        pub fn at(obj: [*]u8) *Int {
            return @as(*Int, @alignCast(@ptrCast(obj)));
        }
    };

    pub const Float = struct {
        type_index: type_pool.Index,
        ast_node: u64,
        value: f64,

        pub fn at(obj: [*]u8) *Float {
            return @as(*Float, @alignCast(@ptrCast(obj)));
        }
    };

    pub const Str = struct {
        type_index: type_pool.Index,
        ast_node: u64,
        value: string_pool.Index,

        pub fn at(obj: [*]u8) *Str {
            return @as(*Str, @alignCast(@ptrCast(obj)));
        }
    };

    pub const Char = struct {
        type_index: type_pool.Index,
        ast_node: u64,
        value: u8,

        pub fn at(obj: [*]u8) *Char {
            return @as(*Char, @alignCast(@ptrCast(obj)));
        }
    };
};

// pub const Fn = struct {
//     pure: bool,
//     diamond: bool,
//     is_method: bool,
//     type_index: type_pool.Index,
//     name: string_pool.Index,
//     params: std.ArrayList(Param),

//     // block: *Block,

//     pub fn new(
//         gpa: std.mem.Allocator,
//         pure: bool,
//         diamond: bool,
//         is_method: bool,
//         type_index: type_pool.Index,
//         name: string_pool.Index,
//     ) !*Fn {
//         const result = try gpa.create(Fn);
//         result.* = .{
//             .pure = pure,
//             .diamond = diamond,
//             .is_method = is_method,
//             .type_index = type_index,
//             .name = name,
//             .params = std.ArrayList(Param).init(gpa),
//         };
//         return result;
//     }

//     pub fn drop(self: *Fn, gpa: std.mem.Allocator) void {
//         for (self.params.items) |param| {
//             switch (param) {
//                 .optional => |optional| optional.default.drop(gpa),
//                 .destructed => |destructed| destructed.pattern.drop(gpa),
//                 else => {},
//             }
//         }

//         self.params.deinit();
//         // block.drop(gpa);
//         gpa.destroy(self);
//     }
// };

// pub const Param = union(enum) {
//     common: struct {
//         name: string_pool.Index,
//         type_index: type_pool.Index = 0,
//     },
//     rest: struct {
//         name: string_pool.Index,
//         type_index: type_pool.Index = 0,
//     },
//     destructed: struct {
//         type_index: type_pool.Index = 0,
//         pattern: *Pattern,
//     },
//     optional: struct {
//         name: string_pool.Index,
//         type_index: type_pool.Index = 0,
//         default: *Expr,
//     },
// };

// pub const StructDef = struct {
//     scope: Scope,
//     type_index: type_pool.Index,
//     name: string_pool.Index,
//     fields: std.ArrayList(StructField),

//     pub fn new(
//         gpa: std.mem.Allocator,
//         type_index: type_pool.Index,
//         name: string_pool.Index,
//     ) !*StructDef {
//         const result = try gpa.create(StructDef);
//         result.* = .{
//             .scope = try Scope.new(gpa, true),
//             .type_index = type_index,
//             .name = name,
//             .fields = std.ArrayList(StructField).init(gpa),
//         };
//         return result;
//     }

//     pub fn drop(self: *StructDef, gpa: std.mem.Allocator) void {
//         self.scope.drop(gpa);
//         for (self.fields.items) |f|
//             if (f.default) |default_expr|
//                 default_expr.drop(gpa);
//         self.fields.deinit();
//         gpa.destroy(self);
//     }
// };

// pub const StructField = struct {
//     name: string_pool.Index,
//     type_index: type_pool.Index,
//     default: ?*Expr,
// };

// pub const EnumDef = struct {
//     type_index: type_pool.Index,
//     name: string_pool.Index,
//     scope: Scope,
//     variants: std.ArrayList(EnumVariant),

//     pub fn new(
//         gpa: std.mem.Allocator,
//         type_index: type_pool.Index,
//         name: string_pool.Index,
//     ) !*EnumDef {
//         const result = try gpa.create(EnumDef);
//         result.* = .{
//             .type_index = type_index,
//             .name = name,
//             .scope = try Scope.new(gpa, false),
//             .variants = std.ArrayList(EnumVariant).init(gpa),
//         };
//         return result;
//     }

//     pub fn drop(self: *EnumDef, gpa: std.mem.Allocator) void {
//         self.scope.drop(gpa);
//         for (self.variants.items) |variant|
//             variant.drop(gpa);
//         self.variants.deinit();
//         gpa.destroy(self);
//     }
// };

// pub const EnumVariant = union(enum) {
//     common: string_pool.Index,
//     with_struct: struct {
//         name: string_pool.Index,
//         fields: std.ArrayList(StructField),
//     },
//     with_tuple: struct {
//         name: string_pool.Index,
//         fields: std.ArrayList(type_pool.Index),
//     },
//     with_sub_enum: struct {
//         name: string_pool.Index,
//         variants: std.ArrayList(EnumVariant),
//     },

//     pub fn drop(self: *EnumVariant, gpa: std.mem.Allocator) void {
//         switch (self.*) {
//             .with_struct => |with_struct| {
//                 for (with_struct.fields.items) |field|
//                     if (field.default) |default_expr|
//                         default_expr.drop(gpa);
//                 with_struct.fields.deinit();
//             },
//             .with_tuple => |with_tuple| {
//                 with_tuple.fields.deinit();
//             },
//             .with_sub_enum => |with_sub_enum| {
//                 with_sub_enum.variants.deinit();
//             },
//             else => {},
//         }
//     }
// };

// pub const UnionDef = struct {
//     type_index: type_pool.Index,
//     name: string_pool.Index,
//     scope: Scope,
//     variants: std.ArrayList(UnionVariant),

//     pub fn new(
//         gpa: std.mem.Allocator,
//         type_index: type_pool.Index,
//         name: string_pool.Index,
//     ) !*UnionDef {
//         const result = try gpa.create(UnionDef);
//         result.* = .{
//             .type_index = type_index,
//             .name = name,
//             .scope = try Scope.new(gpa, false),
//             .variants = std.ArrayList(UnionVariant).init(gpa),
//         };
//         return result;
//     }

//     pub fn drop(self: *UnionDef, gpa: std.mem.Allocator) void {
//         self.scope.drop(gpa);
//         for (self.variants.items) |variant|
//             variant.drop(gpa);
//         self.variants.deinit();
//         gpa.destroy(self);
//     }
// };

// pub const UnionVariant = union(enum) {
//     common: string_pool.Index,
//     with_struct: struct {
//         name: string_pool.Index,
//         fields: std.ArrayList(StructField),
//     },
//     with_tuple: struct {
//         name: string_pool.Index,
//         fields: std.ArrayList(type_pool.Index),
//     },

//     pub fn drop(self: *UnionVariant, gpa: std.mem.Allocator) void {
//         switch (self.*) {
//             .with_struct => |with_struct| {
//                 for (with_struct.fields.items) |field|
//                     if (field.default) |default_expr|
//                         default_expr.drop(gpa);
//                 with_struct.fields.deinit();
//             },
//             .with_tuple => |with_tuple| {
//                 with_tuple.fields.deinit();
//             },
//             else => {},
//         }
//     }
// };

// pub const Pattern = struct {
//     type_index: type_pool.Index,
//     payload: Payload,

//     pub const Payload = union(enum) {
//         optional_null,
//         optional_binding: string_pool.Index,
//         // underscore
//         any,
//         binding: string_pool.Index,
//         select: struct {
//             left: *Pattern,
//             name: string_pool.Index,
//         },
//         as_binding: struct {
//             binding: string_pool.Index,
//             left_pattern: *Pattern,
//         },

//         // atoms
//         int: i64,
//         real: f64,
//         string: string_pool.Index,
//         char: u8,
//         bool: bool,
//         symbol: string_pool.Index,

//         object_pattern: []Pattern,
//         object_pattern_field: struct {
//             name: string_pool.Index,
//             pattern: *Pattern,
//         },

//         tuple_pattern: []Pattern,
//         list_pattern: []Pattern,
//         list_rest_binding: string_pool.Index,

//         enum_select_all: *Pattern,

//         pattern_call: struct {
//             left: *Pattern,
//             args: []Pattern,
//         },
//         object_pattern_call: struct {
//             left: *Pattern,
//             args: []Pattern,
//         },

//         // expr flatten
//         from_expr: *Expr,

//         and_is: struct {
//             left: *Pattern,
//             expr: *Expr,
//             right: *Pattern,
//         },
//         if_guard: struct {
//             left: *Pattern,
//             guard: *Expr,
//         },
//     };
// };

// pub const Expr = struct {
//     type_index: type_pool.Index,
//     payload: Payload,

//     pub const Payload = union(enum) {
//         // atoms
//         id: string_pool.Index,
//         int: i64,
//         real: f64,
//         string: string_pool.Index,
//         char: u8,
//         bool: bool,
//         unit,
//         null,
//         symbol: string_pool.Index,

//         // expr flatten
//         from_pattern: *Pattern,

//         // expr
//         select: struct {
//             left: *Expr,
//             name: string_pool.Index,
//         },
//         as_binding: struct {
//             binding: string_pool.Index,
//             left_expr: *Expr,
//         },
//         tuple: []Expr,
//         list: []Expr,
//         object: struct {
//             elements: []Expr,
//             fields: []Property,
//         },
//         call: struct {
//             left: *Expr,
//             args: []Expr,
//             optional_args: []Expr,
//         },
//         diamond_call: struct {
//             left: *Expr,
//             args: []Expr,
//             properties: []Property,
//         },
//         object_call: struct {
//             left: *Expr,
//             args: []Expr,
//             properties: []Property,
//         },

//         binary: struct {
//             op: ast.Tag,
//             left: *Expr,
//             right: *Expr,
//         },
//     };
// };

// // pub const Block = struct {
// //     items: []Item,

// //     pub const Item = union(enum) {};
// // };

// pub const Property = struct {
//     name: string_pool.Index,
//     expr: *Expr,
// };

// pub const Stmt = union(enum) { expr: *Expr, let_decl: struct {
//     name: string_pool.Index,
//     type_index: type_pool.Index,
//     expr: *Expr,
// }, const_decl: struct {
//     name: string_pool.Index,
//     type_index: type_pool.Index,
//     expr: *Expr,
// }, if_stmt: struct {
//     cond: *Expr,
//     then: *Block,
//     else_: ?*Stmt,
// } };

// pub const Block = struct {};
