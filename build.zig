const std = @import("std");

pub fn build(b: *std.Build) void {
    const target = b.standardTargetOptions(.{});
    const optimize = b.standardOptimizeOption(.{});

    const module = b.addModule("parser-toolkit", .{
        .root_source_file = b.path("src/main.zig"),
        .target = target,
        .optimize = optimize,
    });

    const main_tests = b.addTest(.{
        .root_module = module,
    });

    const test_step = b.step("test", "Run library tests");
    test_step.dependOn(&b.addRunArtifact(main_tests).step);

    const calculator_example = b.addExecutable(.{
        .name = "calculator",
        .root_module = b.createModule(.{
            .root_source_file = b.path("examples/calculator.zig"),
            .optimize = optimize,
            .target = target,
        }),
    });
    calculator_example.root_module.addImport("parser-toolkit", module);
    b.installArtifact(calculator_example);

    b.step("run", "Runs the calculator example").dependOn(&b.addRunArtifact(calculator_example).step);
}
