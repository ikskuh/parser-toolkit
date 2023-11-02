//!
//! Parser Toolkit Grammar Compiler
//!

const std = @import("std");
const args_parser = @import("args");
const ptk = @import("parser-toolkit");

const ast = @import("ast.zig");
const parser = @import("parser.zig");
const ast_dump = @import("ast_dump.zig");

const Diagnostics = @import("Diagnostics.zig");

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
    // errdefer |e| @compileLog(@TypeOf(e));

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

    var string_pool = try ptk.strings.Pool.init(dynamic_allocator);
    defer string_pool.deinit();

    var diagnostics = Diagnostics.init(dynamic_allocator);
    defer diagnostics.deinit();

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

    const file_name = if (cli.positionals.len > 0)
        cli.positionals[0]
    else
        "stdint";

    compileFile(
        dynamic_allocator,
        &diagnostics,
        &string_pool,
        input_file,
        file_name,
        cli.options.test_mode,
    ) catch |err| switch (err) {
        // syntax errors must produce diagnostics:
        error.SyntaxError, error.InvalidSourceEncoding => std.debug.assert(diagnostics.hasErrors()),

        error.OutOfMemory => {
            try diagnostics.emit(.{
                .source = file_name,
                .line = 1,
                .column = 1,
            }, .out_of_memory, .{});
        },

        error.FileTooBig => {
            try diagnostics.emit(.{
                .source = file_name,
                .line = 1,
                .column = 1,
            }, .file_limit_exceeded, .{});
        },

        error.InputOutput,
        error.AccessDenied,
        error.BrokenPipe,
        error.SystemResources,
        error.OperationAborted,
        error.WouldBlock,
        error.ConnectionResetByPeer,
        error.Unexpected,
        error.IsDir,
        error.ConnectionTimedOut,
        error.NotOpenForReading,
        error.NetNameDeleted,
        => |e| {
            try diagnostics.emit(.{
                .source = file_name,
                .line = 1,
                .column = 1,
            }, .io_error, .{ .error_code = e });
        },

        error.TestExpectationMismatched => return 1, // this is a shortcut we can take to not render the diagnostics on test failure
    };

    if (cli.options.test_mode == .none) {
        try diagnostics.render(stderr.writer());

        return if (diagnostics.hasErrors())
            1
        else
            0;
    } else {
        // test fails through `error.TestExpectationMismatched`, not through diagnostics
        return 0;
    }
}

const TestExpectation = struct {
    code: Diagnostics.Code,
};

fn validateDiagnostics(allocator: std.mem.Allocator, diagnostics: Diagnostics, expectations: []const TestExpectation) !void {
    var available = std.ArrayList(Diagnostics.Code).init(allocator);
    defer available.deinit();

    var expected = std.ArrayList(Diagnostics.Code).init(allocator);
    defer expected.deinit();

    try available.appendSlice(diagnostics.codes.items);
    try expected.resize(expectations.len);

    for (expected.items, expectations) |*dst, src| {
        dst.* = src.code;
    }

    // Remove everything from expected and available that is present in both:
    {
        var i: usize = 0;
        while (i < expected.items.len) {
            const e = expected.items[i];

            if (std.mem.indexOfScalar(Diagnostics.Code, available.items, e)) |index| {
                _ = available.swapRemove(index);
                _ = expected.swapRemove(i);
            } else {
                i += 1;
            }
        }
    }

    const ok = (available.items.len == 0) and (expected.items.len == 0);

    for (available.items) |code| {
        std.log.err("unexpected diagnostic: {0}", .{code});
    }
    for (expected.items) |code| {
        std.log.err("unmatched diagnostic:  {0}", .{code});
    }

    if (!ok)
        return error.TestExpectationMismatched;
}

fn compileFile(
    allocator: std.mem.Allocator,
    diagnostics: *Diagnostics,
    string_pool: *ptk.strings.Pool,
    input_file: std.fs.File,
    file_name: []const u8,
    mode: TestMode,
) !void {
    var source_code = try input_file.readToEndAlloc(allocator, 4 << 20); // 4 MB should be enough for now...
    defer allocator.free(source_code);

    var expectations = std.ArrayList(TestExpectation).init(allocator);
    defer expectations.deinit();

    if (mode != .none) {
        // parse expectations from source code:
        var lines = std.mem.tokenize(u8, source_code, "\n");
        while (lines.next()) |line| {
            const prefix = "# expected:";
            if (std.mem.startsWith(u8, line, prefix)) {
                var items = std.mem.tokenize(u8, line[prefix.len..], " \t,");
                while (items.next()) |error_code| {
                    if (error_code.len == 0 or (error_code[0] != 'E' and error_code[0] != 'W' and error_code[0] != 'D'))
                        @panic("invalid error code!");
                    const id = std.fmt.parseInt(u16, error_code[1..], 10) catch @panic("bad integer");
                    const code = std.meta.intToEnum(Diagnostics.Code, id) catch @panic("bad diagnostic code");
                    try expectations.append(.{ .code = code });
                }
            }
        }
    }

    var tree = try parser.parse(
        allocator,
        diagnostics,
        string_pool,
        file_name,
        source_code,
    );
    defer tree.deinit();

    if (mode == .parse_only) {
        try validateDiagnostics(allocator, diagnostics.*, expectations.items);
        return;
    }

    // TODO: Implement sema

    // TODO: Implement parsergen / tablegen / highlightergen

    if (mode == .none) {
        ast_dump.dump(string_pool, tree);
    } else {
        // we need to validate against test expectations when doing *any* test mode
        try validateDiagnostics(allocator, diagnostics.*, expectations.items);
    }
}
