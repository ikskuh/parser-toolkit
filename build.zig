const std = @import("std");

pub fn build(b: *std.Build) void {
    const target = b.standardTargetOptions(.{});
    const optimize = b.standardOptimizeOption(.{});

    const module = b.addModule("parser-toolkit", .{
        .root_source_file = b.path("src/main.zig"),
    });

    const main_tests = b.addTest(.{
        .root_source_file = b.path("src/main.zig"),
        .optimize = optimize,
    });

    const test_step = b.step("test", "Run library tests");
    test_step.dependOn(&b.addRunArtifact(main_tests).step);

    const calculator_example = b.addExecutable(.{
        .root_source_file = b.path("examples/calculator.zig"),
        .name = "calculator",
        .optimize = optimize,
        .target = target,
    });
    calculator_example.root_module.addImport("parser-toolkit", module);
    b.installArtifact(calculator_example);

    b.step("run", "Runs the calculator example").dependOn(&b.addRunArtifact(calculator_example).step);
}
