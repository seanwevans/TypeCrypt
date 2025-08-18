const std = @import("std");

pub const Tag = enum { Int, Str, Bool, Pair, List };

pub const Type = union(Tag) {
    Int: void,
    Str: void,
    Bool: void,
    Pair: struct { a: *const Type, b: *const Type },
    List: *const Type,
};

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

fn canonicalBytes(allocator: std.mem.Allocator, ty: Type) ![]u8 {
    var list = std.ArrayList(u8).init(allocator);
    try canonicalBytesImpl(ty, &list);
    return list.toOwnedSlice();
}

fn deriveKey(allocator: std.mem.Allocator, ty: Type) ![32]u8 {
    const bytes = try canonicalBytes(allocator, ty);
    defer allocator.free(bytes);
    const salt = "TypeCryptHKDFSalt";
    const info = "TypeCryptHKDFInfo";
    const prk = std.crypto.kdf.hkdf.HkdfSha256.extract(salt, bytes);
    var out: [32]u8 = undefined;
    std.crypto.kdf.hkdf.HkdfSha256.expand(out[0..], info, prk);
    return out;
}

fn hex(buf: []const u8) ![]u8 {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();
    return std.fmt.allocPrint(arena.allocator(), "{s}", .{std.fmt.fmtSliceHexLower(buf)});
}

pub fn main() !void {
    var gpa = std.heap.page_allocator;
    const int = Type{ .Int = {} };
    const str = Type{ .Str = {} };
    const bool = Type{ .Bool = {} };
    const pair = Type{ .Pair = .{ .a = &int, .b = &bool } };
    const list = Type{ .List = &int };
    const entries = [_]struct { name: []const u8, ty: Type }{
        .{ .name = "int", .ty = int },
        .{ .name = "str", .ty = str },
        .{ .name = "pair", .ty = pair },
        .{ .name = "list", .ty = list },
    };
    for (entries) |e| {
        const bytes = try canonicalBytes(gpa, e.ty);
        defer gpa.free(bytes);
        const key = try deriveKey(gpa, e.ty);
        var hex_buf: [64]u8 = undefined;
        const key_hex = std.fmt.bufPrint(&hex_buf, "{s}", .{std.fmt.fmtSliceHexLower(&key)}) catch unreachable;
        const bytes_repr = try std.fmt.allocPrint(gpa, "{any}", .{bytes});
        defer gpa.free(bytes_repr);
        std.debug.print("{{\"type\":\"{s}\",\"bytes\":{s},\"key\":\"{s}\"}}\n", .{ e.name, bytes_repr.*, key_hex });
    }
}
