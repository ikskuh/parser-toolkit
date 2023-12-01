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
        for (parser_accept_files ++ parser_reject_files) |file| {
            const run = b.addRunArtifact(ptkdef_exe);
            run.addArg("--test_mode=parse_only");
            run.addFileArg(.{ .path = file });
            test_step.dependOn(&run.step);
        }

        // Integration tests for ptkgen:
        for (analyis_accept_files ++ analyis_reject_files) |file| {
            const run = b.addRunArtifact(ptkdef_exe);
            run.addArg("--test_mode=no_codegen");
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

const example_files = [_][]const u8{
    "/home/felix/projects/parser-toolkit/examples/ptkgen/grammar.ptk",
    "examples/ptkgen/ast-with-unions.ptk",
};

const analyis_accept_files = [_][]const u8{
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

    "test/analysis/accept/start-decl.ptk",

    "test/analysis/accept/pattern-custom.ptk",
    "test/analysis/accept/pattern-literal.ptk",
    "test/analysis/accept/pattern-regex.ptk",
    "test/analysis/accept/pattern-word.ptk",

    "test/analysis/accept/pattern-word-skip.ptk",
    "test/analysis/accept/pattern-regex-skip.ptk",
    "test/analysis/accept/pattern-literal-skip.ptk",
    "test/analysis/accept/pattern-custom-skip.ptk",
} ++ example_files;

const analyis_reject_files = [_][]const u8{
    "test/analysis/reject/duplicate-node.ptk",
    "test/analysis/reject/duplicate-pattern.ptk",
    "test/analysis/reject/duplicate-rule.ptk",

    "test/analysis/accept/expect-warn-missing-start.ptk",

    "test/analysis/reject/undeclared-start.ptk",
    "test/analysis/reject/duplicate-undeclared-start.ptk",
    "test/analysis/reject/duplicate-start.ptk",

    "test/analysis/reject/duplicate-field-record.ptk",
    "test/analysis/reject/duplicate-field-variant.ptk",

    "test/analysis/reject/production-undeclared-pattern-ref.ptk",
    "test/analysis/reject/production-undeclared-rule-ref.ptk",
};

const parser_accept_files = [_][]const u8{
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

    "test/parser/accept/basic-rule-ref.ptk",
    "test/parser/accept/basic-token-ref.ptk",
    "test/parser/accept/rule-primitive-sequence.ptk",

    "test/parser/accept/document-start.ptk",

    "test/parser/accept/mapping-value-ref.ptk",
    "test/parser/accept/mapping-code-literal.ptk",
    "test/parser/accept/mapping-user-value.ptk",

    "test/parser/accept/mapping-builtin-function-a0.ptk",
    "test/parser/accept/mapping-builtin-function-a1.ptk",
    "test/parser/accept/mapping-builtin-function-a5.ptk",
    "test/parser/accept/mapping-builtin-function-nest.ptk",

    "test/parser/accept/mapping-user-function-a0.ptk",
    "test/parser/accept/mapping-user-function-a1.ptk",
    "test/parser/accept/mapping-user-function-a5.ptk",
    "test/parser/accept/mapping-user-function-nest.ptk",

    "test/parser/accept/mapping-array-a0.ptk",
    "test/parser/accept/mapping-array-a1.ptk",
    "test/parser/accept/mapping-array-a5.ptk",
    "test/parser/accept/mapping-array-nested.ptk",

    "test/parser/accept/mapping-variant-init.ptk",

    "test/parser/accept/mapping-record-init-f1.ptk",
    "test/parser/accept/mapping-record-init-f3.ptk",

    "test/parser/accept/rule-typespec-custom.ptk",
    "test/parser/accept/rule-typespec-ref.ptk",
    "test/parser/accept/rule-typespec-literal.ptk",

    "test/parser/accept/node-alias.ptk",
    "test/parser/accept/node-custom.ptk",
    "test/parser/accept/node-literal.ptk",

    "test/parser/accept/node-record-f1.ptk",
    "test/parser/accept/node-record-f4.ptk",

    "test/parser/accept/node-variant-f4.ptk",
    "test/parser/accept/node-variant-f1.ptk",
} ++ analyis_accept_files;

const parser_reject_files = [_][]const u8{
    "test/parser/reject/empty-rule.ptk",
    "test/parser/reject/empty-group.ptk",
    "test/parser/reject/empty-optional.ptk",
    "test/parser/reject/empty-rep_one.ptk",
    "test/parser/reject/empty-rep_zero.ptk",

    "test/parser/reject/unexpected-token-string.ptk",

    "test/parser/reject/empty-mapping.ptk",
    "test/parser/reject/bad-mapping-invalid-token.ptk",
    "test/parser/reject/bad-mapping-too-long.ptk",

    "test/parser/reject/node-no-type.ptk",
    "test/parser/reject/rule-no-type.ptk",
    "test/parser/reject/rule-no-type-no-prod.ptk",
    "test/parser/reject/rule-bad-prod.ptk",

    "test/parser/reject/pattern-unexpected-token.ptk",
};
