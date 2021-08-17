const std = @import("std");

pub fn build(b: *std.build.Builder) void {
    const mode = b.standardReleaseOptions();

    var main_tests = b.addTest("src/main.zig");
    main_tests.setBuildMode(mode);

    const test_step = b.step("test", "Run library tests");
    test_step.dependOn(&main_tests.step);

    const calculator_example = b.addExecutable("calculator", "examples/calculator.zig");
    calculator_example.setBuildMode(mode);
    calculator_example.install();

    calculator_example.addPackage(.{
        .name = "parser-toolkit",
        .path = .{ .path = "src/main.zig" },
    });

    const calculator_runner = calculator_example.run();
    b.step("run", "Runs the calculator example").dependOn(&calculator_runner.step);
}
