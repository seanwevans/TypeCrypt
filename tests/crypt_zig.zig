const std = @import("std");
const key = @import("../zig/src/key.zig");
const zig_main = @import("../zig/src/main.zig");

const Type = zig_main.Type;
const Value = zig_main.Value;

const ty_int = Type{ .Int = {} };
const ty_str = Type{ .Str = {} };
const ty_bool = Type{ .Bool = {} };
const ty_pair = Type{ .Pair = .{ .a = &ty_int, .b = &ty_bool } };

const default_int_value = Value{ .Int = 0 };
const default_str_value = Value{ .Str = "" };
const default_bool_value = Value{ .Bool = false };
const default_pair_value = Value{ .Pair = .{ .a = &default_int_value, .b = &default_bool_value } };
const empty_value_list = [_]Value{};

fn defaultValue(ty: Type) Value {
    return switch (ty) {
        .Int => default_int_value,
        .Str => default_str_value,
        .Bool => default_bool_value,
        .Pair => default_pair_value,
        .List => Value{ .List = empty_value_list[0..] },
    };
}

fn encrypt(
    allocator: std.mem.Allocator,
    ty: Type,
    plaintext: []const u8,
) ![]u8 {
    const Aead = std.crypto.aead.chacha_poly.ChaCha20Poly1305;
    var key_bytes = try key.deriveKey(Type, allocator, ty);
    defer std.crypto.utils.secureZero(u8, key_bytes[0..]);

    var nonce: [Aead.nonce_length]u8 = undefined;
    std.crypto.random.bytes(nonce[0..]);

    const total_len = Aead.nonce_length + plaintext.len + Aead.tag_length;
    var out = try allocator.alloc(u8, total_len);
    std.mem.copyForwards(u8, out[0..Aead.nonce_length], nonce[0..]);

    var tag: [Aead.tag_length]u8 = undefined;
    Aead.encrypt(
        out[Aead.nonce_length .. Aead.nonce_length + plaintext.len],
        &tag,
        plaintext,
        &[_]u8{},
        nonce,
        key_bytes,
    );
    std.mem.copyForwards(u8, out[Aead.nonce_length + plaintext.len ..], tag[0..]);
    return out;
}

fn decrypt(
    allocator: std.mem.Allocator,
    ty: Type,
    value: Value,
    ciphertext: []const u8,
) !?[]u8 {
    const Aead = std.crypto.aead.chacha_poly.ChaCha20Poly1305;
    if (!zig_main.matches(value, ty)) return null;
    if (ciphertext.len < Aead.nonce_length + Aead.tag_length) return null;

    var key_bytes = try key.deriveKey(Type, allocator, ty);
    defer std.crypto.utils.secureZero(u8, key_bytes[0..]);

    var nonce: [Aead.nonce_length]u8 = undefined;
    std.mem.copyForwards(u8, nonce[0..], ciphertext[0..Aead.nonce_length]);
    const ct_len = ciphertext.len - Aead.nonce_length - Aead.tag_length;
    const ct = ciphertext[Aead.nonce_length .. Aead.nonce_length + ct_len];
    var tag: [Aead.tag_length]u8 = undefined;
    std.mem.copyForwards(u8, tag[0..], ciphertext[Aead.nonce_length + ct_len ..]);

    var pt = try allocator.alloc(u8, ct_len);
    Aead.decrypt(pt, ct, tag, &[_]u8{}, nonce, key_bytes) catch {
        allocator.free(pt);
        return null;
    };
    return pt;
}

fn hex(allocator: std.mem.Allocator, buf: []const u8) ![]u8 {
    return std.fmt.allocPrint(allocator, "{s}", .{std.fmt.fmtSliceHexLower(buf)});
}

fn unhex(allocator: std.mem.Allocator, s: []const u8) ![]u8 {
    if (s.len % 2 != 0) return error.InvalidHexLength;
    const n = s.len / 2;
    var out = try allocator.alloc(u8, n);
    errdefer allocator.free(out);

    var i: usize = 0;
    while (i < n) : (i += 1) {
        out[i] = try std.fmt.parseInt(u8, s[i * 2 .. i * 2 + 2], 16);
    }
    return out;
}

fn parseType(name: []const u8) !Type {
    if (std.mem.eql(u8, name, "int")) return ty_int;
    if (std.mem.eql(u8, name, "str")) return ty_str;
    if (std.mem.eql(u8, name, "bool")) return ty_bool;
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
    const value = defaultValue(ty);
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
        const pt_opt = try decrypt(gpa, ty, value, ct_bytes);
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
