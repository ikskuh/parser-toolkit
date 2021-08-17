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
    };
}
