const std = @import("std");
const key = @import("../zig/src/key.zig");

pub const Tag = enum { Int, Str, Bool, Pair, List };

pub const Type = union(Tag) {
    Int: void,
    Str: void,
    Bool: void,
    Pair: struct { a: *const Type, b: *const Type },
    List: *const Type,
};

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
        const bytes = try key.canonicalBytes(Type, gpa, e.ty);
        defer gpa.free(bytes);
        const key = try key.deriveKey(Type, gpa, e.ty);
        var hex_buf: [64]u8 = undefined;
        const key_hex = std.fmt.bufPrint(&hex_buf, "{s}", .{std.fmt.fmtSliceHexLower(&key)}) catch unreachable;
        const bytes_repr = try std.fmt.allocPrint(gpa, "{any}", .{bytes});
        defer gpa.free(bytes_repr);
        std.debug.print("{{\"type\":\"{s}\",\"bytes\":{s},\"key\":\"{s}\"}}\n", .{ e.name, bytes_repr.*, key_hex });
    }
}
