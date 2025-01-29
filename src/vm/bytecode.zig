pub const BC = struct {
    tag: Tag,
};

pub const Tag = enum {
    // constants
    null,
    u8,
    i8,
    u16,
    i16,
    u32,
    i32,
    u64,
    usize,
    i64,
    isize,
    u128,
    i128,
    f32,
    f64,
    char,
    bool,
    str,

    // unary operators
    pos,
    neg,
    bool_not,
    type_of,

    // binary operators
    add,
    sub,
    mul,
    div,
    mod,
    add_add,
    gt,
    lt,
    ge,
    le,
    eq,
    type_eq,
    ne,
    bool_and,

    push,
    new_obj,
    assign,
    fn_call,
    type_cast,
};
