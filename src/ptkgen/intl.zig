const std = @import("std");

const Diagnostics = @import("Diagnostics.zig");

pub const Language = enum {
    en,
};

pub const language: Language = .en;

pub const localization = @field(localizations, @tagName(language));

pub const localizations = struct {
    pub const en = Localization.generate(@embedFile("intl/en.json"));
};

pub const FormattableError: type = blk: {
    const list = @typeInfo(std.meta.fieldInfo(Localization, .errors).type).Struct.fields;

    var errors: [list.len]std.builtin.Type.Error = undefined;
    for (&errors, list) |*dst, src| {
        dst.* = .{ .name = src.name };
    }

    break :blk @Type(.{
        .ErrorSet = &errors,
    });
};

pub const DiagnosticStrings: type = blk: {
    const list = @typeInfo(Diagnostics.Code).Enum.fields;

    var dst_fields: [list.len]std.builtin.Type.StructField = undefined;
    for (&dst_fields, list) |*dst, src| {
        dst.* = .{
            .name = src.name,
            .type = []const u8,
            .default_value = null,
            .is_comptime = false,
            .alignment = @alignOf([]const u8),
        };
    }

    break :blk @Type(.{
        .Struct = .{
            .layout = .Auto,
            .fields = &dst_fields,
            .decls = &.{},
            .is_tuple = false,
        },
    });
};

pub const Localization = struct {
    diagnostics: DiagnosticStrings,

    errors: struct {
        Unexpected: []const u8,

        OutOfMemory: []const u8,

        InputOutput: []const u8,
        AccessDenied: []const u8,
        BrokenPipe: []const u8,
        SystemResources: []const u8,
        OperationAborted: []const u8,
        WouldBlock: []const u8,
        ConnectionResetByPeer: []const u8,
        IsDir: []const u8,
        ConnectionTimedOut: []const u8,
        NotOpenForReading: []const u8,
        NetNameDeleted: []const u8,

        FileTooBig: []const u8,
        SyntaxError: []const u8,
        InvalidSourceEncoding: []const u8,
    },

    pub fn generate(comptime buffer: []const u8) Localization {
        @setEvalBranchQuota(1_000_000);

        var alloc_buf: [buffer.len]u8 = undefined;
        var fba = std.heap.FixedBufferAllocator.init(&alloc_buf);

        return std.json.parseFromSliceLeaky(Localization, fba.allocator(), buffer, .{}) catch @compileError("failed to parse json");
    }
};
