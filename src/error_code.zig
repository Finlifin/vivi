// 以8位十进制错误码的第一个数字划分错误码类型
// 1: 语法错误
// 2: 语义错误
// 3: 外部系统错误
// 4: 内部系统错误
// 5: 未知错误
// 6: 其他错误

pub fn errorMsg(code: GlobalErr) ?[]const u8 {
    return switch (code) {
        .InvalidStringLiteral => "unable to recognize string literal",
        .InvalidArbitraryIdentifier => "unable to recognize arbitrary identifier",
        .InvalidIntegerLiteral => "unable to recognize integer literal",
        .InvalidFloatLiteral => "unable to recognize float literal",
        .InvalidCharLiteral => "unable to recognize char literal",

        .UnexpectedToken => "unexpected token",
        .InvalidExpr => "invalid expression",
        .InvalidPattern => "invalid pattern",

        .UnknownIdentifier => "unknown identifier",

        .FileNotFound => "file not found",

        .AllocationFailed => "allocation failed",
        .OutOfArenaMemory => "out of arena memory",
        .OutOfGpaMemory => "out of gpa memory",
        .OutOfObjectLimit => "out of object limit",
        .SrcIdNotFound => "source id not found",

        .UnknownErr => "unknown error",

        .TestingErr => "testing error, 孩子们别怕",
        .ErrJustForControlFlow => "error just for control flow",

        else => "unknown error",
    };
}

pub const Kind = enum {
    err,
    warn,
    info,
    help,
    note,
};

pub const GlobalErr = enum(u32) {
    // while lexing or extracting tokens
    InvalidStringLiteral = 10_000_001,
    InvalidArbitraryIdentifier = 10_000_002,
    InvalidIntegerLiteral = 10_000_003,
    InvalidFloatLiteral = 10_000_004,
    InvalidCharLiteral = 10_000_005,

    // while parsing
    UnexpectedToken = 10_001_001,
    InvalidExpr = 10_001_002,
    InvalidPattern = 10_001_003,

    // while analyzing
    UnknownIdentifier = 20_000_001,

    FileNotFound = 30_000_001,

    AllocationFailed = 40_000_001,
    OutOfArenaMemory = 40_000_002,
    OutOfGpaMemory = 40_000_003,
    OutOfObjectLimit = 40_000_004,
    SrcIdNotFound = 40_000_005,
    Unimplemented = 40_000_006,

    UnknownErr = 50_000_001,

    TestingErr = 60_000_001,
    ErrJustForControlFlow = 60_000_002,
};
