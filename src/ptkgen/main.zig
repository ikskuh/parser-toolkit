//!
//! Parser Toolkit Grammar Compiler
//!

const std = @import("std");
const args_parser = @import("args");
const ptk = @import("parser-toolkit");

const ast = @import("ast.zig");
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

    var string_pool = try ptk.strings.Pool.init(dynamic_allocator);
    defer string_pool.deinit();

    var diagnostics = ptk.Diagnostics.init(dynamic_allocator);
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
        error.SyntaxError => std.debug.assert(diagnostics.hasErrors()),

        error.OutOfMemory => {
            try diagnostics.emit(.{
                .source = file_name,
                .line = 1,
                .column = 1,
            }, .@"error", "out of memory", .{});
        },

        error.StreamTooLong => {
            try diagnostics.emit(.{
                .source = file_name,
                .line = 1,
                .column = 1,
            }, .@"error", "input file too large", .{});
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
        => {
            try diagnostics.emit(.{
                .source = file_name,
                .line = 1,
                .column = 1,
            }, .@"error", "i/o error: {s}", .{@errorName(err)});
        },
    };

    try diagnostics.print(stderr.writer());

    return if (diagnostics.hasErrors())
        1
    else
        0;
}

fn compileFile(
    allocator: std.mem.Allocator,
    diagnostics: *ptk.Diagnostics,
    string_pool: *ptk.strings.Pool,
    input_file: std.fs.File,
    file_name: []const u8,
    mode: TestMode,
) !void {
    var tree = try parser.parse(
        allocator,
        diagnostics,
        string_pool,
        file_name,
        input_file.reader(),
    );
    defer tree.deinit();

    dumpAst(string_pool, tree.top_level_declarations);

    if (mode == .parse_only) {
        // we're done if we're here
        return;
    }
}

fn dumpAst(strings: *const ptk.strings.Pool, decls: ast.List(ast.TopLevelDeclaration)) void {
    std.debug.print("ast dump:\n", .{});

    var iter = ast.iterate(decls);
    while (iter.next()) |decl| {
        switch (decl) {
            .start => |item| std.debug.print("start {s}\n", .{strings.get(item.identifier)}),

            .rule => |rule| {
                std.debug.print("rule {s}", .{strings.get(rule.name.value)});

                if (rule.ast_type) |ast_type| {
                    std.debug.print(" : ", .{});
                    dumpAstType(strings, ast_type);
                }

                std.debug.print(" = \n", .{});

                var prods = ast.iterate(rule.productions);
                var first = true;
                while (prods.next()) |prod| {
                    defer first = false;
                    if (!first) {
                        std.debug.print("  | ", .{});
                    } else {
                        std.debug.print("    ", .{});
                    }
                    dumpMappedProd(strings, prod);
                }

                std.debug.print("\n;\n", .{});
            },

            .node => |node| {
                std.debug.print("node {s}", .{strings.get(node.name.value)});

                std.debug.print(";\n", .{});
            },
        }
    }
}

fn dumpAstType(strings: *const ptk.strings.Pool, typespec: ast.TypeSpec) void {
    _ = strings;
    _ = typespec;
    std.debug.print("<TYPE HERE>", .{});
}

fn dumpMappedProd(strings: *const ptk.strings.Pool, mapped_prod: ast.MappedProduction) void {
    dumpProd(strings, mapped_prod.production);

    if (mapped_prod.mapping) |mapping| {
        dumpMapping(strings, mapping);
    }
}

fn dumpProd(strings: *const ptk.strings.Pool, production: ast.Production) void {
    switch (production) {
        .literal => |lit| std.debug.print("\"{}\"", .{std.zig.fmtEscapes(strings.get(lit.value))}),
        .terminal => |term| std.debug.print("<{}>", .{std.zig.fmtId(strings.get(term.identifier))}),
        .recursion => std.debug.print("<recursion>", .{}),
        .sequence => |seq| {
            std.debug.print("(", .{});

            var iter = ast.iterate(seq);
            while (iter.next()) |item| {
                std.debug.print(" ", .{});
                dumpProd(strings, item);
            }

            std.debug.print(" )", .{});
        },
        .optional => std.debug.print("<optional>", .{}),
        .repetition_zero => std.debug.print("<repetition_zero>", .{}),
        .repetition_one => std.debug.print("<repetition_one>", .{}),
    }
}

fn dumpMapping(strings: *const ptk.strings.Pool, mapping: ast.AstMapping) void {
    _ = strings;
    _ = mapping;
    std.debug.print("<MAPPING HERE>", .{});
}
