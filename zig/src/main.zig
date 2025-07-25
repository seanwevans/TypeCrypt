const std = @import("std");

/// Runtime representation of a Type.
/// This mirrors the Rust `Type` enum and allows
/// nested structures to describe pairs and lists.
pub const Tag = enum { Int, Str, Bool, Pair, List };

pub const Type = union(Tag) {
    Int: void,
    Str: void,
    Bool: void,
    Pair: struct { a: *const Type, b: *const Type },
    List: *const Type,
};

/// A runtime value tagged with a `Tag`.
pub const Value = union(Tag) {
    Int: i32,
    Str: []const u8,
    Bool: bool,
    Pair: struct { a: Value, b: Value },
    List: []Value,
};

/// Produce the canonical byte encoding of a `Type`.
pub fn canonicalBytes(allocator: std.mem.Allocator, ty: Type) ![]u8 {
    var list = std.ArrayList(u8).init(allocator);
    try canonicalBytesImpl(ty, &list);
    return list.toOwnedSlice();
}

fn canonicalBytesImpl(ty: Type, list: *std.ArrayList(u8)) !void {
    switch (ty) {
        .Int => try list.append(0),
        .Str => try list.append(1),
        .Bool => try list.append(2),
        .Pair => |p| {
            try list.append(3);
            try canonicalBytesImpl(p.a.*, list);
            try canonicalBytesImpl(p.b.*, list);
        },
        .List => |elem| {
            try list.append(4);
            try canonicalBytesImpl(elem.*, list);
        },
    }
}

/// Derive a 32-byte key by hashing the canonical bytes with SHA-256.
pub fn deriveKey(allocator: std.mem.Allocator, ty: Type) ![32]u8 {
    const bytes = try canonicalBytes(allocator, ty);
    defer allocator.free(bytes);
    var hasher = std.crypto.sha2.Sha256.init(.{});
    hasher.update(bytes);
    var out: [32]u8 = undefined;
    hasher.final(out[0..]);
    return out;
}

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
pub fn matches(v: Value, expected: Type) bool {
    switch (expected) {
        .Int => return std.meta.activeTag(v) == .Int,
        .Str => return std.meta.activeTag(v) == .Str,
        .Bool => return std.meta.activeTag(v) == .Bool,
        .Pair => |pt| {
            if (std.meta.activeTag(v) != .Pair) return false;
            const val = v.Pair;
            return matches(val.a, pt.a.*) and matches(val.b, pt.b.*);
        },
        .List => |elem_ty| {
            if (std.meta.activeTag(v) != .List) return false;
            const slice = v.List;
            for (slice) |item| {
                if (!matches(item, elem_ty.*)) return false;
            }
            return true;
        },
    }
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
    try std.testing.expect(matches(Value{ .Int = 1 }, Type{ .Int = {} }));
    try std.testing.expect(!matches(Value{ .Str = "hi" }, Type{ .Int = {} }));
}

test "matches_pair" {
    const int_ty = Type{ .Int = {} };
    const str_ty = Type{ .Str = {} };
    const pair_ty = Type{ .Pair = .{ .a = &int_ty, .b = &str_ty } };
    const v = Value{ .Pair = .{ .a = Value{ .Int = 1 }, .b = Value{ .Str = "a" } } };
    try std.testing.expect(matches(v, pair_ty));
}

test "matches_list" {
    const int_ty = Type{ .Int = {} };
    const list_ty = Type{ .List = &int_ty };
    const arr = [_]Value{ Value{ .Int = 1 }, Value{ .Int = 2 } };
    const lst = Value{ .List = arr[0..] };
    try std.testing.expect(matches(lst, list_ty));
}

test "canonicalBytes_int" {
    var gpa = std.testing.allocator;
    const bytes = try canonicalBytes(gpa, Type{ .Int = {} });
    defer gpa.free(bytes);
    try std.testing.expectEqualSlices(u8, &[_]u8{0}, bytes);
}

test "canonicalBytes_pair" {
    var gpa = std.testing.allocator;
    const int_ty = Type{ .Int = {} };
    const bool_ty = Type{ .Bool = {} };
    const pair_ty = Type{ .Pair = .{ .a = &int_ty, .b = &bool_ty } };
    const bytes = try canonicalBytes(gpa, pair_ty);
    defer gpa.free(bytes);
    try std.testing.expectEqualSlices(u8, &[_]u8{3, 0, 2}, bytes);
}

test "deriveKey deterministic" {
    var gpa = std.testing.allocator;
    const k1 = try deriveKey(gpa, Type{ .Str = {} });
    const k2 = try deriveKey(gpa, Type{ .Str = {} });
    try std.testing.expectEqualSlices(u8, &k1, &k2);
}

test "deriveKey_distinct" {
    var gpa = std.testing.allocator;
    const k1 = try deriveKey(gpa, Type{ .Int = {} });
    const k2 = try deriveKey(gpa, Type{ .Bool = {} });
    try std.testing.expect(!std.mem.eql(u8, &k1, &k2));
}
