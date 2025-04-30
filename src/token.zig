const std = @import("std");

const Location = @import("Location.zig");

pub fn Token(comptime Type: type) type {
    if (@typeInfo(Type) != .@"enum")
        @compileError("Type must be a enum type!");
    return struct {
        /// The location of the token in the source stream
        location: Location,

        /// The raw, unescaped string this token represents
        text: []const u8,

        /// The type of the token that was matched by a matching function
        type: Type,

        pub fn format(tok: @This(), fmt: []const u8, opt: std.fmt.FormatOptions, writer: anytype) !void {
            _ = fmt;
            _ = opt;

            try writer.print("Token(type={}, text=\"{}\", location={})", .{
                std.zig.fmtId(@tagName(tok.type)),
                std.zig.fmtEscapes(tok.text),
                tok.location,
            });
        }
    };
}
