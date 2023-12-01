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

        SyntaxError: []const u8,
        SemanticError: []const u8,

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
        InvalidSourceEncoding: []const u8,

        DiskQuota: []const u8,
        NoSpaceLeft: []const u8,
        DeviceBusy: []const u8,
        InvalidArgument: []const u8,
        NotOpenForWriting: []const u8,
        LockViolation: []const u8,
        ProcessFdQuotaExceeded: []const u8,
        SystemFdQuotaExceeded: []const u8,
        SharingViolation: []const u8,
        PathAlreadyExists: []const u8,
        FileNotFound: []const u8,
        PipeBusy: []const u8,
        NameTooLong: []const u8,
        InvalidUtf8: []const u8,
        BadPathName: []const u8,
        NetworkNotFound: []const u8,
        InvalidHandle: []const u8,
        SymLinkLoop: []const u8,
        NoDevice: []const u8,
        NotDir: []const u8,
        FileLocksNotSupported: []const u8,
        FileBusy: []const u8,
        LinkQuotaExceeded: []const u8,
        ReadOnlyFileSystem: []const u8,
        RenameAcrossMountPoints: []const u8,
    },

    pub fn generate(comptime buffer: []const u8) Localization {
        @setEvalBranchQuota(1_000_000);

        var alloc_buf: [4 * buffer.len]u8 = undefined;
        var fba = std.heap.FixedBufferAllocator.init(&alloc_buf);

        return std.json.parseFromSliceLeaky(Localization, fba.allocator(), buffer, .{}) catch |err| @compileError(std.fmt.comptimePrint("failed to parse json: {}", .{err}));
    }
};
