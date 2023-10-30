//!
//! Parser Toolkit Grammar Compiler
//!

const std = @import("std");
const args_parser = @import("args");
const ptk = @import("parser-toolkit");

const parser = @import("parser.zig");

comptime {
    // reference for unit tests:
    _ = parser;
}

pub const CliOptions = struct {
    help: bool = false,
    output: ?[]const u8 = null,
    test_mode: TestMode = .none,

    pub const shorthands = .{
        .h = "help",
        .o = "output",
    };

    pub const meta = .{
        .full_text = "Compiles a .ptk grammar file into Zig code.",

        .usage_summary = "[-h] [-o <file>] [<input>]",

        .option_docs = .{
            .help = "Prints this help.",
            .output = "If given, will print the generated code into <file>",

            .test_mode = "(internal use only, required for testing)",
        },
    };
};

const TestMode = enum {
    none,
    parse_only,
};

pub fn main() !u8 {
    var stdout = std.io.getStdOut();
    var stdin = std.io.getStdIn();
    var stderr = std.io.getStdErr();

    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();

    var arena = std.heap.ArenaAllocator.init(gpa.allocator());
    defer arena.deinit();

    const dynamic_allocator = gpa.allocator();
    const static_allocator = arena.allocator();

    var cli = args_parser.parseForCurrentProcess(CliOptions, static_allocator, .print) catch return 1;
    defer cli.deinit();

    if (cli.options.help) {
        try args_parser.printHelp(CliOptions, cli.executable_name orelse "ptkgen", stdout.writer());
        return 0;
    }

    var diagnostics = ptk.Diagnostics.init(dynamic_allocator);
    defer diagnostics.deinit();

    // From here on, always print the diagnostics on exit!
    defer diagnostics.print(stderr.writer()) catch {};

    var input_file = switch (cli.positionals.len) {
        0 => stdin,
        1 => std.fs.cwd().openFile(cli.positionals[0], .{}) catch |err| {
            try stderr.writer().print("failed to open file {s}: {s}\n", .{
                cli.positionals[0],
                @errorName(err),
            });
            return 1;
        },
        else => {
            try stderr.writeAll("Expects either a single positional file or none.\nSee --help for usage!\n");
            return 1;
        },
    };
    defer input_file.close();

    var ast = try parser.parse(
        dynamic_allocator,
        &diagnostics,
        if (cli.positionals.len > 0)
            cli.positionals[0]
        else
            "stdint",
        input_file.reader(),
    );
    defer ast.deinit();

    if (cli.options.test_mode == .parse_only) {
        // we're done if we're here
        return 0;
    }

    return 0;
}
