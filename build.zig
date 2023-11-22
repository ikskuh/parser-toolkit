const std = @import("std");

pub fn build(b: *std.build.Builder) void {
    const optimize = b.standardOptimizeOption(.{});

    _ = b.addModule("parser-toolkit", .{
        .source_file = .{ .path = "src/main.zig" },
        .dependencies = &.{},
    });

    const main_tests = b.addTest(.{
        .root_source_file = .{ .path = "src/main.zig" },
        .optimize = optimize,
    });

    const test_step = b.step("test", "Run library tests");
    test_step.dependOn(&b.addRunArtifact(main_tests).step);

    const calculator_example = b.addExecutable(.{
        .root_source_file = .{ .path = "examples/calculator.zig" },
        .name = "calculator",
        .optimize = optimize,
    });

    b.installArtifact(calculator_example);
    calculator_example.addAnonymousModule("parser-toolkit", .{
        .source_file = .{ .path = "src/main.zig" },
    });

    b.step("run", "Runs the calculator example").dependOn(&b.addRunArtifact(calculator_example).step);
}
