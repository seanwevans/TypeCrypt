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
    const prk = std.crypto.hkdf.HkdfSha256.extract(salt, bytes);
    var out: [32]u8 = undefined;
    std.crypto.hkdf.HkdfSha256.expand(out[0..], info, prk);
    return out;
}

fn encrypt(
    allocator: std.mem.Allocator,
    ty: Type,
    plaintext: []const u8,
) ![]u8 {
    const key = try deriveKey(allocator, ty);
    var nonce: [12]u8 = undefined;
    std.crypto.random.bytes(&nonce);
    const tag_len = std.crypto.aead.chacha_poly.ChaCha20Poly1305.tag_length;
    var ct = try allocator.alloc(u8, plaintext.len);
    defer allocator.free(ct);
    var tag: [tag_len]u8 = undefined;
    std.crypto.aead.chacha_poly.ChaCha20Poly1305.encrypt(ct, &tag, plaintext, &[_]u8{}, nonce, key);
    var out = try allocator.alloc(u8, nonce.len + ct.len + tag_len);
    std.mem.copy(u8, out[0..nonce.len], &nonce);
    std.mem.copy(u8, out[nonce.len .. nonce.len + ct.len], ct);
    std.mem.copy(u8, out[nonce.len + ct.len ..], &tag);
    return out;
}

fn decrypt(
    allocator: std.mem.Allocator,
    ty: Type,
    ciphertext: []const u8,
) !?[]u8 {
    const tag_len = std.crypto.aead.chacha_poly.ChaCha20Poly1305.tag_length;
    if (ciphertext.len < 12 + tag_len) return null;
    const key = try deriveKey(allocator, ty);
    var nonce: [12]u8 = undefined;
    std.mem.copy(u8, &nonce, ciphertext[0..12]);
    const ct_len = ciphertext.len - 12 - tag_len;
    const ct = ciphertext[12 .. 12 + ct_len];
    var tag: [tag_len]u8 = undefined;
    std.mem.copy(u8, &tag, ciphertext[12 + ct_len ..]);
    var pt = try allocator.alloc(u8, ct_len);
    std.crypto.aead.chacha_poly.ChaCha20Poly1305.decrypt(pt, ct, tag, &[_]u8{}, nonce, key) catch {
        allocator.free(pt);
        return null;
    };
    return pt;
}

fn hex(allocator: std.mem.Allocator, buf: []const u8) ![]u8 {
    return std.fmt.allocPrint(allocator, "{s}", .{std.fmt.fmtSliceHexLower(buf)});
}

fn unhex(allocator: std.mem.Allocator, s: []const u8) ![]u8 {
    const n = s.len / 2;
    var out = try allocator.alloc(u8, n);
    var i: usize = 0;
    while (i < n) : (i += 1) {
        out[i] = try std.fmt.parseInt(u8, s[i * 2 .. i * 2 + 2], 16);
    }
    return out;
}

const ty_int = Type{ .Int = {} };
const ty_str = Type{ .Str = {} };
const ty_bool = Type{ .Bool = {} };
const ty_pair = Type{ .Pair = .{ .a = &ty_int, .b = &ty_bool } };

fn parseType(name: []const u8) !Type {
    if (std.mem.eql(u8, name, "int")) return ty_int;
    if (std.mem.eql(u8, name, "str")) return ty_str;
    if (std.mem.eql(u8, name, "pair")) return ty_pair;
    return error.UnknownType;
}

fn usage() !void {
    try std.io.getStdErr().writer().print(
        "usage: encrypt|decrypt <type> [hex]\n",
        .{},
    );
}

pub fn main() !void {
    var gpa = std.heap.page_allocator;
    const args = try std.process.argsAlloc(gpa);
    defer std.process.argsFree(gpa, args);
    if (args.len < 3) {
        try usage();
        std.process.exit(1);
    }
    const cmd = args[1];
    const ty = parseType(args[2]) catch {
        try std.io.getStdErr().writer().print("unknown type\n", .{});
        std.process.exit(1);
    };
    const plaintext = "cross-test";
    if (std.mem.eql(u8, cmd, "encrypt")) {
        if (args.len != 3) {
            try usage();
            std.process.exit(1);
        }
        const ct = try encrypt(gpa, ty, plaintext);
        defer gpa.free(ct);
        const hex_ct = try hex(gpa, ct);
        defer gpa.free(hex_ct);
        try std.io.getStdOut().writer().print("{s}\n", .{hex_ct});
    } else if (std.mem.eql(u8, cmd, "decrypt")) {
        if (args.len != 4) {
            try usage();
            std.process.exit(1);
        }
        const ct_bytes = try unhex(gpa, args[3]);
        defer gpa.free(ct_bytes);
        const pt_opt = try decrypt(gpa, ty, ct_bytes);
        if (pt_opt) |pt| {
            defer gpa.free(pt);
            try std.io.getStdOut().writer().print("{s}\n", .{pt});
        } else {
            try std.io.getStdOut().writer().print("FAIL\n", .{});
            std.process.exit(1);
        }
    } else {
        try usage();
        std.process.exit(1);
    }
}

