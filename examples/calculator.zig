const std = @import("std");
const ptk = @import("parser-toolkit");

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();

    const allocator = &gpa.allocator;

    var line_buffer = std.ArrayList(u8).init(allocator);
    defer line_buffer.deinit();

    var stdin = std.io.getStdIn().reader();
    var stdout = std.io.getStdOut().writer();

    var calc = Calculator.init(allocator);
    defer calc.deinit();

    try calc.set("pi", std.math.pi);

    main_loop: while (true) {
        try stdout.writeAll("? ");
        stdin.readUntilDelimiterArrayList(&line_buffer, '\n', 4096) catch |err| switch (err) {
            error.EndOfStream => break :main_loop,
            else => |e| return e,
        };
        const line = std.mem.trim(u8, line_buffer.items, "\t ");

        const value = calc.evaluate(line) catch |err| {
            try stdout.print("error: {s}\n", .{@errorName(err)});
            continue;
        };
        try stdout.print("= {d}\n", .{value});
    }
    try stdout.writeAll("\n");
}

const Calculator = struct {
    const Self = @This();

    arena: std.heap.ArenaAllocator,
    variables: std.StringHashMapUnmanaged(f64),

    pub fn init(allocator: *std.mem.Allocator) Self {
        return Self{
            .arena = std.heap.ArenaAllocator.init(allocator),
            .variables = .{},
        };
    }

    pub fn deinit(self: *Self) void {
        self.variables.deinit(&self.arena.allocator);
        self.arena.deinit();
        self.* = undefined;
    }

    pub fn set(self: *Self, name: []const u8, value: f64) !void {
        const gop = try self.variables.getOrPut(&self.arena.allocator, name);
        if (!gop.found_existing) {
            errdefer _ = self.variables.remove(name);
            gop.key_ptr.* = try self.arena.allocator.dupe(u8, name);
        }
        gop.value_ptr.* = value;
    }

    pub fn get(self: Self, name: []const u8) ?f64 {
        return self.variables.get(name);
    }

    pub fn evaluate(self: *Self, expression: []const u8) !f64 {
        _ = self;
        _ = expression;
        return error.Not;
    }
};
