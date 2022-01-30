const std = @import("std");
const Location = @import("Location.zig");

location: Location,
message: [:0]const u8,
level: Level,

pub const Level = enum {
    /// An error was diagnosed. This is a fault in the program.
    @"error",

    /// A potential error was diagnosed. This indicates a potential fault in the program.
    warning,

    /// A purely informational message, it does not indicate faults in the program.
    info,
};
