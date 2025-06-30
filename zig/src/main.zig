const std = @import("std");

/// Experimental compile-time type enumeration.
/// Mirrors the "Type" variants from other implementations.
pub const Type = enum {
    Int,
    Str,
    Bool,
    Pair,
    List,
};

/// A value tagged with its Type.
pub const Value = union(Type) {
    Int: i32,
    Str: []const u8,
    Bool: bool,
    Pair: struct { a: Value, b: Value },
    List: []Value,
};

/// Compute a compile-time hash of a Zig type.
/// Currently uses a simple FNV-1a hash of the type name.
pub fn typeHash(comptime T: type) u64 {
    var hash: u64 = 0xcbf29ce484222325;
    const name = @typeName(T);
    for (name) |c| {
        hash = (hash ^ @as(u64, c)) * 0x100000001b3;
    }
    return hash;
}

pub fn main() void {
    const int_hash = typeHash(i32);
    const str_hash = typeHash([]const u8);
    std.debug.print("int hash: {x}\n", .{int_hash});
    std.debug.print("str hash: {x}\n", .{str_hash});
}
