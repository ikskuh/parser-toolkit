const std = @import("std");

pub fn build(b: *std.build.Builder) void {
    const optimize = b.standardOptimizeOption(.{});

    _ = b.addModule("parser-toolkit", .{
        .source_file = .{ .path = "src/main.zig" },
        .dependencies = &.{},
    });

    var main_tests = b.addTest(.{
        .root_source_file = .{ .path = "src/main.zig" },
        .optimize = optimize,
    });

    const test_step = b.step("test", "Run library tests");
    test_step.dependOn(&main_tests.step);

    const calculator_example = b.addExecutable(.{
        .root_source_file = .{ .path = "examples/calculator.zig" },
        .name = "calculator",
        .optimize = optimize,
    });

    calculator_example.install();
    calculator_example.addAnonymousModule("parser-toolkit", .{
        .source_file = .{ .path = "src/main.zig" },
    });

    const runner = calculator_example.run();
    b.step("run", "Runs the calculator example").dependOn(&runner.step);
}
