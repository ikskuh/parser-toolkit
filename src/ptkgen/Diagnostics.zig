const std = @import("std");
const ptk = @import("parser-toolkit");

const intl = @import("intl.zig");
const parser = @import("parser.zig");

const Diagnostics = @This();

pub const Code = enum(u16) {
    pub const first_error = 1000;
    pub const first_warning = 4000;
    pub const first_note = 8000;
    pub const last_item = 9999;

    // generic failures (1000-1099):
    out_of_memory = 1000,
    file_limit_exceeded = 1001,
    io_error = 1002,

    // non-recoverable syntax errors (1100-1199):
    invalid_source_encoding = 1100,
    unexpected_token_eof = 1101,
    unexpected_token = 1102,
    unexpected_character = 1103,
    unexpected_eof = 1104,
    bad_string_escape = 1105,
    invalid_string_escape = 1106,
    excess_tokens = 1107,
    unexpected_toplevel_token = 1108,
    unexpected_token_no_context = 1109,
    unexpected_token_type_spec = 1110,
    unexpected_token_mapping = 1111,
    unexpected_token_production_list = 1112,
    unexpected_token_production = 1113,
    unexpected_token_pattern = 1114,

    // recoverable syntax errors (1200-1299):
    illegal_empty_group = 1200,
    empty_mapping = 1201,
    integer_overflow = 1202,
    empty_typespec = 1203,

    // semantic errors (1300-1399):

    duplicate_identifier_rule = 1300,
    duplicate_identifier_node = 1301,
    duplicate_identifier_pattern = 1302,

    reference_to_undeclared_rule = 1303,
    reference_to_undeclared_node = 1304,
    reference_to_undeclared_pattern = 1305,

    multiple_start_symbols = 1306,

    duplicate_compound_field = 1307,

    context_reference_out_of_bounds = 1308,

    variant_does_not_exist = 1309,

    record_field_does_not_exist = 1310,
    record_field_already_initialized = 1311,
    record_field_not_initialized = 1312,

    mapping_requires_typed_rule = 1313,

    invalid_builtin_function = 1314,

    // semantic warnings (4000-4099):

    missing_start_symbol = 4000,

    comptime {
        std.debug.assert(first_error < first_warning);
        std.debug.assert(first_warning < first_note);
        std.debug.assert(first_note < last_item);
    }

    const max_item_len = blk: {
        var len = 0;
        for (@typeInfo(Code).Enum.fields) |fld| {
            len = @max(len, fld.name);
        }
        break :blk len;
    };

    const code_strings = blk: {
        @setEvalBranchQuota(10_000);
        var map = std.EnumArray(Code, []const u8).initUndefined();

        for (std.enums.values(Code)) |code| {
            const tag = @tagName(code);

            // perform kebab conversion:
            var buf: [tag.len]u8 = tag[0..tag.len].*;
            for (&buf) |*c| {
                if (c.* == '_')
                    c.* = '-';
            }

            map.set(code, &buf);
        }

        break :blk map;
    };

    pub fn isError(code: Code) bool {
        const int = @intFromEnum(code);
        return @intFromEnum(code) >= first_error and int < first_warning;
    }

    pub fn isWarning(code: Code) bool {
        const int = @intFromEnum(code);
        return int >= first_warning and int < first_note;
    }

    pub fn isNote(code: Code) bool {
        const int = @intFromEnum(code);
        return int >= first_note and int < last_item;
    }

    pub fn parse(string: []const u8) error{
        /// Format is not recognized
        InvalidFormat,
        /// Numeric error code is out of range.
        OutOfRange,
        /// Numeric error code does not exist.
        InvalidId,
    }!Code {
        if (string.len == 0 or (string[0] != 'E' and string[0] != 'W' and string[0] != 'D'))
            return error.InvalidFormat;
        const id = std.fmt.parseInt(u16, string[1..], 10) catch |err| switch (err) {
            error.InvalidCharacter => return error.InvalidFormat,
            error.Overflow => return error.OutOfRange,
        };
        if (id > last_item)
            return error.OutOfRange;
        return std.meta.intToEnum(Diagnostics.Code, id) catch return error.InvalidId;
    }

    pub fn format(code: Code, comptime fmt: []const u8, opt: std.fmt.FormatOptions, writer: anytype) !void {
        _ = opt;

        if (comptime std.mem.eql(u8, fmt, "d")) {
            const code_prefix = if (code.isError())
                "E"
            else if (code.isWarning())
                "W"
            else
                "D";

            try writer.print("{s}{d:0>4}", .{ code_prefix, @intFromEnum(code) });
        } else if (comptime std.mem.eql(u8, fmt, "s")) {
            try writer.writeAll(code_strings.get(code));
        } else {
            @compileError("Code fmt must be {s} (string variant) or {d} (numeric variant)!");
        }
        //
    }
};

const NoDiagnosticData = struct {};

const UnexpectedTokenMessage = struct {
    actual: parser.Token,
};

const DuplicateIdentifier = struct {
    identifier: []const u8,
    previous_location: ptk.Location,
};
const UndeclaredIdentifier = struct { identifier: []const u8 };

pub fn Data(comptime code: Code) type {
    return switch (code) {
        .out_of_memory => NoDiagnosticData,
        .file_limit_exceeded => NoDiagnosticData,
        .io_error => struct { error_code: intl.FormattableError },

        .unexpected_token_eof => struct {
            expected_type: parser.TokenType,
        },
        .unexpected_token => struct {
            expected_type: parser.TokenType,
            actual: parser.Token,
        },

        .unexpected_toplevel_token => UnexpectedTokenMessage,
        .unexpected_token_no_context => UnexpectedTokenMessage,
        .unexpected_token_type_spec => UnexpectedTokenMessage,
        .unexpected_token_mapping => UnexpectedTokenMessage,
        .unexpected_token_production_list => UnexpectedTokenMessage,
        .unexpected_token_production => UnexpectedTokenMessage,
        .unexpected_token_pattern => UnexpectedTokenMessage,

        .unexpected_eof => NoDiagnosticData,

        .invalid_source_encoding => NoDiagnosticData,
        .unexpected_character => struct { character: u21 },

        .bad_string_escape => NoDiagnosticData,
        .invalid_string_escape => struct { escape: u21 },
        .excess_tokens => struct { token_type: parser.TokenType },

        .illegal_empty_group => NoDiagnosticData,
        .empty_mapping => NoDiagnosticData,

        .integer_overflow => struct {
            min: []const u8,
            max: []const u8,
            actual: []const u8,
        },

        .empty_typespec => NoDiagnosticData,

        .duplicate_identifier_rule => DuplicateIdentifier,
        .duplicate_identifier_node => DuplicateIdentifier,
        .duplicate_identifier_pattern => DuplicateIdentifier,

        .reference_to_undeclared_rule => UndeclaredIdentifier,
        .reference_to_undeclared_node => UndeclaredIdentifier,
        .reference_to_undeclared_pattern => UndeclaredIdentifier,

        .multiple_start_symbols => struct {
            identifier: []const u8,
            previous_location: ptk.Location,
        },

        .missing_start_symbol => NoDiagnosticData,

        .duplicate_compound_field => struct {
            identifier: []const u8,
            previous_location: ptk.Location,
        },

        .context_reference_out_of_bounds => struct {
            index: u32,
            limit: u32,
        },

        .variant_does_not_exist => struct {
            field: []const u8,
            type_location: ptk.Location,
        },

        .record_field_does_not_exist => struct {
            field: []const u8,
            type_location: ptk.Location,
        },
        .record_field_already_initialized => struct {
            field: []const u8,
            prev_init: ptk.Location,
        },
        .record_field_not_initialized => struct {
            field: []const u8,
            field_location: ptk.Location,
        },

        .mapping_requires_typed_rule => NoDiagnosticData,

        .invalid_builtin_function => struct {
            name: []const u8,
        },

        // else => @compileError(std.fmt.comptimePrint("Code {} has no diagnostic type associated!", .{code})),
    };
}

pub const Message = struct {
    level: ptk.Error.Level,
    location: ptk.Location,
    text: []const u8,
};

inner: ptk.Diagnostics,
codes: std.ArrayList(Code),

pub fn init(allocator: std.mem.Allocator) Diagnostics {
    return Diagnostics{
        .inner = ptk.Diagnostics.init(allocator),
        .codes = std.ArrayList(Code).init(allocator),
    };
}

pub fn deinit(diag: *Diagnostics) void {
    diag.codes.deinit();
    diag.inner.deinit();
    diag.* = undefined;
}

pub fn hasErrors(diag: Diagnostics) bool {
    return diag.inner.hasErrors();
}

pub fn hasWarnings(diag: Diagnostics) bool {
    return diag.inner.hasWarnings();
}

fn Formatter(comptime T: type) type {
    return switch (T) {
        // text and unicode:
        []const u8 => struct {
            // TODO: Distinct between "string body" and "string literal"

            value: T,

            pub fn format(item: @This(), fmt: []const u8, options: std.fmt.FormatOptions, writer: anytype) !void {
                _ = options;
                _ = fmt;
                try writer.print("{}", .{std.zig.fmtEscapes(item.value)});
            }
        },

        u21 => struct {
            value: T,
            pub fn format(item: @This(), fmt: []const u8, options: std.fmt.FormatOptions, writer: anytype) !void {
                _ = options;
                _ = fmt;

                if (item.value < 0x80) {
                    const ascii: u8 = @intCast(item.value);

                    if (std.ascii.isPrint(ascii)) {
                        try writer.print("{c}", .{ascii});
                    } else {
                        try writer.print("[nonprint: 0x{X:0>2}]", .{ascii});
                    }
                } else {
                    var buf: [4]u8 = undefined;
                    if (std.unicode.utf8Encode(item.value, &buf)) |len| {
                        try writer.print("{s}", .{buf[0..len]});
                    } else |_| {
                        try writer.print("<bad unicode: U+{X:0>4}>", .{item.value});
                    }
                }
            }
        },

        // enums:
        parser.TokenType => struct {
            value: parser.TokenType,
            pub fn format(item: @This(), fmt: []const u8, options: std.fmt.FormatOptions, writer: anytype) !void {
                _ = options;
                _ = fmt;
                try writer.print("{s}", .{@tagName(item.value)});
            }
        },

        parser.Token => struct {
            value: parser.Token,
            pub fn format(item: @This(), fmt: []const u8, options: std.fmt.FormatOptions, writer: anytype) !void {
                _ = options;
                _ = fmt;
                try writer.print("{s} ('{}')", .{
                    @tagName(item.value.type),
                    std.zig.fmtEscapes(item.value.text),
                });
            }
        },

        ptk.Location => struct {
            value: ptk.Location,
            pub fn format(item: @This(), fmt: []const u8, options: std.fmt.FormatOptions, writer: anytype) !void {
                _ = options;
                _ = fmt;
                try writer.print("{}", .{item.value});
            }
        },

        intl.FormattableError => struct {
            value: T,

            pub fn format(item: @This(), fmt: []const u8, options: std.fmt.FormatOptions, writer: anytype) !void {
                _ = options;
                _ = fmt;

                inline for (@typeInfo(intl.FormattableError).ErrorSet.?) |err| {
                    if (item.value == @field(intl.FormattableError, err.name)) {
                        try writer.writeAll(@field(intl.localization.errors, err.name));
                        return;
                    }
                } else unreachable;
            }
        },

        // integers:

        u32 => struct {
            value: T,
            pub fn format(item: @This(), fmt: []const u8, options: std.fmt.FormatOptions, writer: anytype) !void {
                _ = options;
                _ = fmt;

                try writer.print("{}", .{item.value});
            }
        },

        else => @compileError(std.fmt.comptimePrint("{s} is not a supported diagnostic type!", .{@typeName(T)})),
    };
}

fn createFormatter(comptime T: type, value: T) Formatter(T) {
    return Formatter(T){ .value = value };
}

fn FormattedData(comptime code: Code) type {
    const Field = std.builtin.Type.StructField;
    const D = Data(code);

    const src_fields = @typeInfo(D).Struct.fields;

    var dst_fields: [src_fields.len]Field = undefined;

    for (&dst_fields, src_fields) |*dst, src| {
        dst.* = .{
            .name = src.name,
            .type = Formatter(src.type),
            .default_value = null,
            .is_comptime = false,
            .alignment = @alignOf(Formatter(src.type)),
        };
    }

    return @Type(.{
        .Struct = .{
            .layout = .Auto,
            .fields = &dst_fields,
            .decls = &.{},
            .is_tuple = false,
        },
    });
}

fn formatData(comptime code: Code, params: Data(code)) FormattedData(code) {
    var formatted: FormattedData(code) = undefined;
    inline for (std.meta.fields(Data(code))) |fld| {
        @field(formatted, fld.name) = createFormatter(fld.type, @field(params, fld.name));
    }
    return formatted;
}

pub fn emit(diag: *Diagnostics, location: ptk.Location, comptime code: Code, params: Data(code)) error{OutOfMemory}!void {
    const level = if (code.isError())
        ptk.Error.Level.@"error"
    else if (code.isWarning())
        ptk.Error.Level.warning
    else if (code.isNote())
        ptk.Error.Level.info
    else
        unreachable;

    const fmt_string = @field(intl.localization.diagnostics, @tagName(code));

    var stack_fallback = std.heap.stackFallback(1024, diag.inner.memory.allocator());
    const stack_fallback_allocator = stack_fallback.get();

    const formatted_params = formatData(code, params);

    const message_text = try std.fmt.allocPrint(stack_fallback_allocator, fmt_string, formatted_params);
    defer stack_fallback_allocator.free(message_text);

    try diag.inner.emit(location, level, "{d}: {s}", .{ code, message_text });
    try diag.codes.append(code);
}

pub fn render(diag: Diagnostics, stream: anytype) !void {
    try diag.inner.print(stream);
}
