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

/// Determine if a `Value` matches a particular `Type`.
pub fn matches(v: Value, t: Type) bool {
    return std.meta.activeTag(v) == t;
}

pub fn main() void {
    const int_hash = typeHash(i32);
    const str_hash = typeHash([]const u8);
    std.debug.print("int hash: {x}\n", .{int_hash});
    std.debug.print("str hash: {x}\n", .{str_hash});
}

// Basic unit tests for typeHash

test "typeHash stable" {
    const val = typeHash(i32);
    try std.testing.expectEqual(val, typeHash(i32));
}

test "typeHash distinct" {
    try std.testing.expect(typeHash(i32) != typeHash([]const u8));
}

test "matches basic" {
    try std.testing.expect(matches(Value{ .Int = 1 }, .Int));
    try std.testing.expect(!matches(Value{ .Str = "hi" }, .Int));
}

test "matches pair and list" {
    const pair = Value{ .Pair = .{ .a = Value{ .Int = 1 }, .b = Value{ .Bool = true } } };
    try std.testing.expect(matches(pair, .Pair));

    const arr = [_]Value{ Value{ .Int = 1 }, Value{ .Int = 2 } };
    const lst = Value{ .List = arr[0..] };
    try std.testing.expect(matches(lst, .List));
}
