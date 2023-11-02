const std = @import("std");

pub fn build(b: *std.build.Builder) void {
    // build options:

    const target = b.standardTargetOptions(.{});
    const optimize = b.standardOptimizeOption(.{});

    const test_step = b.step("test", "Run library tests");
    const examples_step = b.step("examples", "Builds and installs examples");
    const run_calc_step = b.step("run-calculator", "Runs calculator example");

    const all_step = b.step("all", "Builds everything, tests everything");
    all_step.dependOn(b.getInstallStep());
    all_step.dependOn(test_step);
    all_step.dependOn(examples_step);

    // dependencies

    const args_dep = b.dependency("args", .{});

    // external modules

    const args_mod = args_dep.module("args");

    // internal modules

    const ptk_mod = b.addModule("parser-toolkit", .{
        .source_file = .{ .path = "src/toolkit/main.zig" },
        .dependencies = &.{},
    });

    // Applications
    const ptkdef_exe = blk: {
        const ptkdef = b.addExecutable(.{
            .name = "ptkgen",
            .root_source_file = .{ .path = "src/ptkgen/main.zig" },
            .optimize = optimize,
            .target = target,
        });

        ptkdef.addModule("parser-toolkit", ptk_mod);
        ptkdef.addModule("args", args_mod);

        b.installArtifact(ptkdef);

        break :blk ptkdef;
    };

    // test suite
    {
        // unit tests for ptk:
        var ptk_tests = b.addTest(.{
            .root_source_file = ptk_mod.source_file,
            .optimize = optimize,
        });
        for (ptk_mod.dependencies.keys()) |dep_name| {
            ptk_tests.addModule(dep_name, ptk_mod.dependencies.get(dep_name).?);
        }
        test_step.dependOn(&b.addRunArtifact(ptk_tests).step);

        // unit tests for ptkgen:
        var ptkgen_tests = b.addTest(.{
            .root_source_file = .{ .path = "src/ptkgen/main.zig" },
            .optimize = optimize,
        });
        ptkgen_tests.addModule("parser-toolkit", ptk_mod);
        test_step.dependOn(&b.addRunArtifact(ptkgen_tests).step);

        // Integration tests for ptkgen:
        for (parser_ok_files) |file| {
            const run = b.addRunArtifact(ptkdef_exe);
            run.addArg("--test_mode=parse_only");
            run.addFileArg(.{ .path = file });
            test_step.dependOn(&run.step);
        }

        for (parser_reject_files) |file| {
            const run = b.addRunArtifact(ptkdef_exe);
            run.addArg("--test_mode=parse_only");
            run.addFileArg(.{ .path = file });
            test_step.dependOn(&run.step);
        }
    }

    // examples
    {
        const calculator_example = b.addExecutable(.{
            .root_source_file = .{ .path = "examples/calculator.zig" },
            .name = "calculator",
            .optimize = optimize,
        });
        calculator_example.addModule("parser-toolkit", ptk_mod);
        examples_step.dependOn(&b.addInstallArtifact(calculator_example, .{}).step);

        run_calc_step.dependOn(&b.addRunArtifact(calculator_example).step);
    }
}

const parser_ok_files = [_][]const u8{
    "test/parser/accept/empty.ptk",
    "test/parser/accept/empty-with-comment-linefeed.ptk",
    "test/parser/accept/empty-with-comment.ptk",
    "test/parser/accept/identifiers.ptk",

    "test/parser/accept/optional-nospace.ptk",
    "test/parser/accept/optional-space.ptk",
    "test/parser/accept/rep_one-nospace.ptk",
    "test/parser/accept/rep_one-space.ptk",
    "test/parser/accept/rep_zero-nospace.ptk",
    "test/parser/accept/rep_zero-space.ptk",

    // "examples/ptkgen/ast-with-unions.ptk", // TODO: Move to examples
} ++ analyis_ok_files;

const analyis_ok_files = [_][]const u8{
    "test/analysis/accept/match-literal-rule.ptk",
    "test/analysis/accept/match-literal-sequence.ptk",
    "test/analysis/accept/match-literal-variants.ptk",
    "test/analysis/accept/match-literal-sequence-variant.ptk",
    "test/analysis/accept/match-group-one-item.ptk",
    "test/analysis/accept/match-group-one-sequence.ptk",
    "test/analysis/accept/match-group-many-item.ptk",
    "test/analysis/accept/match-group-many-sequence.ptk",
    "test/analysis/accept/match-group-nested.ptk",
    "test/analysis/accept/match-optional-one-item.ptk",
    "test/analysis/accept/match-optional-one-sequence.ptk",
    "test/analysis/accept/match-optional-many-item.ptk",
    "test/analysis/accept/match-optional-many-sequence.ptk",
    "test/analysis/accept/match-optional-nested.ptk",
    "test/analysis/accept/match-rep_zero-one-item.ptk",
    "test/analysis/accept/match-rep_zero-one-sequence.ptk",
    "test/analysis/accept/match-rep_zero-many-item.ptk",
    "test/analysis/accept/match-rep_zero-many-sequence.ptk",
    "test/analysis/accept/match-rep_zero-nested.ptk",
    "test/analysis/accept/match-rep_one-one-item.ptk",
    "test/analysis/accept/match-rep_one-one-sequence.ptk",
    "test/analysis/accept/match-rep_one-many-item.ptk",
    "test/analysis/accept/match-rep_one-many-sequence.ptk",
    "test/analysis/accept/match-rep_one-nested.ptk",
};

const parser_reject_files = [_][]const u8{
    "test/parser/reject/empty-rule.rule",
    "test/parser/reject/empty-group.rule",
    "test/parser/reject/empty-optional.rule",
    "test/parser/reject/empty-rep_one.rule",
    "test/parser/reject/empty-rep_zero.rule",
};
