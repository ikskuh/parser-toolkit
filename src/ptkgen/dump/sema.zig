const std = @import("std");
const ptk = @import("parser-toolkit");

const sema = @import("../sema.zig");
const parser = @import("../parser.zig");

pub fn dump(strings: *const ptk.strings.Pool, grammar: sema.Grammar) void {
    var printer = SemaPrinter{
        .strings = strings,
    };

    SemaPrinter.print("literal patterns:\n", .{});
    printer.dumpPatterns(grammar.literal_patterns);

    SemaPrinter.print("\nuser patterns:\n", .{});
    printer.dumpPatterns(grammar.patterns);

    SemaPrinter.print("\nstart rule: ", .{});
    if (grammar.start) |start| {
        SemaPrinter.print("<{}>\n", .{printer.fmtId(start.rule.name)});
    } else {
        SemaPrinter.print("-none-\n", .{});
    }

    SemaPrinter.print("\nast nodes:\n", .{});
    printer.dumpNodes(grammar.nodes);

    SemaPrinter.print("\nrules:\n", .{});
    printer.dumpRules(grammar.rules);
}

const SemaPrinter = struct {
    const print = std.debug.print;

    strings: *const ptk.strings.Pool,

    fn dumpPatterns(printer: SemaPrinter, patterns: sema.StringHashMap(*sema.Pattern)) void {
        for (patterns.values()) |pattern| {
            print("pattern {} = ", .{printer.fmtId(pattern.name)});

            switch (pattern.data) {
                inline else => |value, tag| print("{s} \"{}\"", .{ @tagName(tag), printer.fmtString(value) }),
            }

            print(";\n", .{});
        }
    }

    fn dumpNodes(printer: SemaPrinter, nodes: sema.StringHashMap(*sema.Node)) void {
        for (nodes.values()) |node| {
            print("node {} = ", .{printer.fmtId(node.name)});

            printer.dumpType(node.type);

            print(";\n", .{});
        }
    }

    fn dumpRules(printer: SemaPrinter, rules: sema.StringHashMap(*sema.Rule)) void {
        for (rules.values()) |rule| {
            print("rule {}", .{printer.fmtId(rule.name)});

            if (rule.type) |rule_type| {
                print(": ", .{});
                printer.dumpType(rule_type);
            }

            print(" = ", .{});

            for (rule.productions, 0..) |production, i| {
                if (i > 0) print("\n    | ", .{});
                printer.dumpMappedProduction(production);
            }

            print(";\n", .{});
        }
    }

    fn dumpMappedProduction(printer: SemaPrinter, mapped_prod: sema.MappedProduction) void {
        printer.dumpProduction(mapped_prod.production);

        if (mapped_prod.mapping) |mapping| {
            print(" -> ", .{});
            printer.dumpMapping(mapping);
        }
    }

    fn dumpProduction(printer: SemaPrinter, production: sema.Production) void {
        switch (production) {
            .terminal => |terminal| {
                if (terminal.is_literal) {
                    print("\"{}\"", .{printer.fmtString(terminal.data.literal_match)});
                } else {
                    print("${}", .{printer.fmtId(terminal.name)});
                }
            },
            .recursion => |recursion| print("<{}>", .{printer.fmtId(recursion.name)}),
            .sequence => |sequence| {
                for (sequence, 0..) |item, i| {
                    if (i > 0)
                        print(" ", .{});
                    printer.dumpProduction(item);
                }
            },
            .optional => |optional| {
                print("(", .{});
                printer.dumpProduction(optional.*);
                print(")?", .{});
            },
            .repetition_zero => |repetition_zero| {
                print("(", .{});
                printer.dumpProduction(repetition_zero.*);
                print(")*", .{});
            },
            .repetition_one => |repetition_one| {
                print("(", .{});
                printer.dumpProduction(repetition_one.*);
                print(")+", .{});
            },
        }
    }

    fn dumpMapping(printer: SemaPrinter, mapping: sema.Mapping) void {
        _ = mapping;
        _ = printer;
    }

    fn dumpType(printer: SemaPrinter, stype: *sema.Type) void {
        switch (stype.*) {
            .code_literal => |literal| print("`{}`", .{printer.fmtString(literal)}),
            .user_type => |literal| print("@{}", .{printer.fmtId(literal)}),
            .optional => |inner| {
                print("optional ", .{});
                printer.dumpType(inner);
            },
            inline .record, .variant => |compound, tag| {
                print("{s} ", .{@tagName(tag)});
                for (compound.fields.keys(), compound.fields.values(), 0..) |name, field, i| {
                    if (i > 0)
                        print(", ", .{});
                    print("{}: ", .{printer.fmtId(name)});
                    printer.dumpType(field.type);
                }
            },
            .named => |other| print("!{}", .{printer.fmtId(other.name)}),
        }
    }

    fn fmtString(printer: SemaPrinter, str: ptk.strings.String) StringPrinter {
        return StringPrinter{ .printer = printer, .str = str, .mode = .text };
    }

    fn fmtId(printer: SemaPrinter, str: ptk.strings.String) StringPrinter {
        return StringPrinter{ .printer = printer, .str = str, .mode = .id };
    }

    const StringPrinter = struct {
        printer: SemaPrinter,
        str: ptk.strings.String,
        mode: enum { id, text },

        pub fn format(strpr: StringPrinter, fmt: []const u8, opt: std.fmt.FormatOptions, writer: anytype) !void {
            _ = opt;
            _ = fmt;

            const text = strpr.printer.strings.get(strpr.str);
            switch (strpr.mode) {
                .id => try writer.print("{}", .{std.zig.fmtId(text)}),
                .text => try writer.print("{}", .{std.zig.fmtEscapes(text)}),
            }
        }
    };
};
