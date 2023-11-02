const std = @import("std");
const ptk = @import("parser-toolkit");

const intl = @import("intl.zig");
const parser = @import("parser.zig");

const Diagnostics = @This();

pub const Code = enum(u16) {
    pub const first_error = 1000;
    pub const first_warning = 4000;
    pub const first_note = 8000;
    pub const last_item = 10000;

    // generic failures:
    out_of_memory = 1000,
    file_limit_exceeded = 1001,
    io_error = 1002,

    // non-recoverable syntax errors:

    invalid_source_encoding = 1100,
    unexpected_token_eof = 1101,
    unexpected_token = 1102,
    unexpected_character = 1103,
    unexpected_eof = 1104,
    bad_string_escape = 1105,
    invalid_string_escape = 1106,
    excess_tokens = 1107,

    // recoverable syntax errors:
    illegal_empty_group = 1200,

    comptime {
        std.debug.assert(first_error < first_warning);
        std.debug.assert(first_warning < first_note);
        std.debug.assert(first_note < last_item);
    }

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
};

const NoDiagnosticData = struct {};
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
            actual_type: parser.TokenType,
            actual_text: []const u8,
        },
        .unexpected_eof => NoDiagnosticData,

        .invalid_source_encoding => NoDiagnosticData,
        .unexpected_character => struct { character: u21 },

        .bad_string_escape => NoDiagnosticData,
        .invalid_string_escape => struct { escape: u21 },
        .excess_tokens => struct { token_type: parser.TokenType },

        .illegal_empty_group => NoDiagnosticData,

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
            value: T,
            pub fn format(item: @This(), fmt: []const u8, options: std.fmt.FormatOptions, writer: anytype) !void {
                _ = options;
                _ = fmt;
                try writer.print("{s}", .{@tagName(item.value)});
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

    const code_prefix = switch (level) {
        .@"error" => "E",
        .warning => "W",
        .info => "D",
    };

    try diag.inner.emit(location, level, "{s}{d:0>4}: {s}", .{ code_prefix, @intFromEnum(code), message_text });
    try diag.codes.append(code);
}

pub fn render(diag: Diagnostics, stream: anytype) !void {
    try diag.inner.print(stream);
}
