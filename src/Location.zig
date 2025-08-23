const std = @import("std");

const Location = @This();

pub const empty = Location{ .source = null, .line = 0, .column = 0 };

source: ?[]const u8 = null,
line: u32,
column: u32,

pub fn min(a: Location, b: Location) Location {
    if (a.source != null and b.source != null) {
        if (!std.mem.eql(u8, a.source.?, b.source.?))
            @panic("a and b must be from the same source file!");
    }
    var loc = Location{
        .line = undefined,
        .column = undefined,
        .source = a.source orelse b.source,
    };
    if (a.line < b.line) {
        loc.line = a.line;
        loc.column = a.column;
    } else if (a.line > b.line) {
        loc.line = b.line;
        loc.column = b.column;
    } else {
        loc.line = a.line;
        loc.column = @min(a.column, b.column);
    }
    return loc;
}

pub fn max(a: Location, b: Location) Location {
    if (a.source != null and b.source != null) {
        if (!std.mem.eql(u8, a.source.?, b.source.?))
            @panic("a and b must be from the same source file!");
    }
    var loc = Location{
        .line = undefined,
        .column = undefined,
        .source = a.source orelse b.source,
    };
    if (a.line > b.line) {
        loc.line = a.line;
        loc.column = a.column;
    } else if (a.line < b.line) {
        loc.line = b.line;
        loc.column = b.column;
    } else {
        loc.line = a.line;
        loc.column = @max(a.column, b.column);
    }
    return loc;
}

pub fn advance(self: *Location, str: []const u8) void {
    for (str) |c| {
        if (c == '\n') {
            self.line += 1;
            self.column = 1;
        } else {
            self.column += 1;
        }
    }
}

pub fn format(location: Location, writer: *std.Io.Writer) !void {
    if (location.source) |source_file| {
        try writer.print("{s}:{d}:{d}", .{
            source_file,
            location.line,
            location.column,
        });
    } else {
        try writer.print("{d}:{d}", .{
            location.line,
            location.column,
        });
    }
}

test "min" {
    const a = Location{ .source = "source", .line = 10, .column = 30 };
    const b = Location{ .source = "source", .line = 10, .column = 40 };
    const c = Location{ .source = "source", .line = 12, .column = 20 };

    const ab = min(a, b);
    const bc = min(b, c);
    const ac = min(a, c);

    try std.testing.expectEqual(a.line, ab.line);
    try std.testing.expectEqual(a.column, ab.column);

    try std.testing.expectEqual(b.line, bc.line);
    try std.testing.expectEqual(b.column, bc.column);

    try std.testing.expectEqual(a.line, ac.line);
    try std.testing.expectEqual(a.column, ac.column);
}

test "max" {
    const a = Location{ .source = "source", .line = 10, .column = 30 };
    const b = Location{ .source = "source", .line = 10, .column = 40 };
    const c = Location{ .source = "source", .line = 12, .column = 20 };

    const ab = max(a, b);
    const bc = max(b, c);
    const ac = max(a, c);

    try std.testing.expectEqual(b.line, ab.line);
    try std.testing.expectEqual(b.column, ab.column);

    try std.testing.expectEqual(c.line, bc.line);
    try std.testing.expectEqual(c.column, bc.column);

    try std.testing.expectEqual(c.line, ac.line);
    try std.testing.expectEqual(c.column, ac.column);
}
