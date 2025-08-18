const std = @import("std");

pub fn canonicalBytes(comptime Type: type, allocator: std.mem.Allocator, ty: Type) ![]u8 {
    var list = std.ArrayList(u8).init(allocator);
    try canonicalBytesImpl(Type, ty, &list);
    return list.toOwnedSlice();
}

fn canonicalBytesImpl(comptime Type: type, ty: Type, list: *std.ArrayList(u8)) !void {
    switch (ty) {
        .Int => try list.append(0),
        .Str => try list.append(1),
        .Bool => try list.append(2),
        .Pair => |p| {
            try list.append(3);
            try canonicalBytesImpl(Type, p.a.*, list);
            try canonicalBytesImpl(Type, p.b.*, list);
        },
        .List => |elem| {
            try list.append(4);
            try canonicalBytesImpl(Type, elem.*, list);
        },
    }
}

pub fn deriveKey(comptime Type: type, allocator: std.mem.Allocator, ty: Type) ![32]u8 {
    const bytes = try canonicalBytes(Type, allocator, ty);
    defer allocator.free(bytes);
    const salt = "TypeCryptHKDFSalt";
    const info = "TypeCryptHKDFInfo";
    const prk = std.crypto.kdf.hkdf.HkdfSha256.extract(salt, bytes);
    var out: [32]u8 = undefined;
    std.crypto.kdf.hkdf.HkdfSha256.expand(out[0..], info, prk);
    return out;
}
