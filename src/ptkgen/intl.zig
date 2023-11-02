const std = @import("std");

pub const Language = enum {
    en,
};

pub const language: Language = .en;

pub const localization = @field(localizations, @tagName(language));

pub const localizations = struct {
    pub const en = Localization.generate(@embedFile("intl/en.json"));
};

pub const FormattableError = blk: {
    const list = @typeInfo(std.meta.fieldInfo(Localization, .errors).type).Struct.fields;

    var errors: [list.len]std.builtin.Type.Error = undefined;
    for (&errors, list) |*dst, src| {
        dst.* = .{ .name = src.name };
    }

    break :blk @Type(.{
        .ErrorSet = &errors,
    });
};

pub const Localization = struct {
    diagnostics: struct {
        out_of_memory: []const u8,
        file_limit_exceeded: []const u8,
        io_error: []const u8,
        invalid_source_encoding: []const u8,
        unexpected_token_eof: []const u8,
        unexpected_token: []const u8,
        unexpected_character: []const u8,
        unexpected_eof: []const u8,
        bad_string_escape: []const u8,
        invalid_string_escape: []const u8,
        excess_tokens: []const u8,
        illegal_empty_group: []const u8,
    },

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
