const std = @import("std");

const Location = @import("Location.zig");

pub fn Token(comptime Type: type) type {
    if (@typeInfo(Type) != .Enum)
        @compileError("Type must be a enum type!");
    return struct {
        /// The location of the token in the source stream
        location: Location,

        /// The raw, unescaped string this token represents
        text: []const u8,

        /// The type of the token that was matched by a matching function
        type: Type,

        pub fn format(token: @This(), fmt: []const u8, options: std.fmt.FormatOptions, writer: anytype) !void {
            _ = fmt;
            _ = options;
            try writer.print("Token {{ .type = {}, .text = \"{}\", .location = {} }}", .{
                token.type,
                std.zig.fmtEscapes(token.text),
                token.location,
            });
        }
    };
}
