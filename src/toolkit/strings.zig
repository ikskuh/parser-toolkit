pub const std = @import("std");

pub const String = enum(u32) {
    empty,

    _,

    pub fn format(string: String, fmt: []const u8, options: std.fmt.FormatOptions, writer: anytype) !void {
        _ = fmt;
        _ = options;
        if (string == .empty) {
            try writer.writeAll("String(empty)");
        } else {
            try writer.print("String({})", .{
                @intFromEnum(string),
            });
        }
    }
};

/// A string pool that can store up to 4 GB of text and deduplicate instances.
///
/// Use this to reduce the memory footprint of your AST and allow quick comparison of strings
/// by using the `String` type instead of doing a `std.mem.eql`.
pub const Pool = struct {
    data: std.ArrayList(u8),
    count: usize = 0,

    pub fn init(allocator: std.mem.Allocator) !Pool {
        var pool = Pool{
            .data = std.ArrayList(u8).init(allocator),
        };
        errdefer pool.deinit();

        std.debug.assert(try pool.insert("") == .empty);

        return pool;
    }

    pub fn deinit(pool: *Pool) void {
        pool.data.deinit();
        pool.* = undefined;
    }

    pub fn insert(pool: *Pool, string: []const u8) error{OutOfMemory}!String {
        std.debug.assert(std.mem.indexOfScalar(u8, string, 0) == null); // Interned strings must not contain NUL!

        const storage = pool.data.items;

        var search_index: usize = 0;
        while (search_index < storage.len) {
            const index = std.mem.indexOfPos(u8, storage, search_index, string) orelse break;

            if (index + string.len + 1 > storage.len)
                break;

            if (storage[index + string.len] == 0)
                return @enumFromInt(index);

            // starts with `string`, but doesn't end with NUL.
            search_index = index + string.len;
        }

        const index = storage.len;

        if (index > std.math.maxInt(u32)) {
            return error.OutOfMemory;
        }

        try pool.data.ensureUnusedCapacity(string.len + 1); // invalidates storage
        pool.data.appendSliceAssumeCapacity(string);
        pool.data.appendAssumeCapacity(0);
        pool.count += 1;

        return @enumFromInt(index);
    }

    /// Returns the string in the pool.
    pub fn get(pool: *const Pool, string: String) [:0]const u8 {
        const storage = pool.data.items;
        const index: usize = @intFromEnum(string);
        std.debug.assert(index < storage.len);
        const slice = std.mem.sliceTo(storage[index..], 0);
        return slice.ptr[0..slice.len :0];
    }

    pub fn format(pool: Pool, fmt: []const u8, options: std.fmt.FormatOptions, writer: anytype) !void {
        _ = fmt;
        _ = options;
        try writer.print("StringPool(count={}, size={:.2f})", .{
            pool.count,
            std.fmt.fmtIntSizeBin(pool.data.items.len),
        });
    }
};

/// Very simplistic string deduplicator, returns the same slice for each string.
/// Does only perform deduplication, no fancy storage strategy.
pub const Dedupe = struct {
    arena: std.heap.ArenaAllocator,
    items: std.StringHashMapUnmanaged(void),

    pub fn init(allocator: std.mem.Allocator) Dedupe {
        return Dedupe{
            .arena = std.heap.ArenaAllocator.init(allocator),
            .items = .{},
        };
    }

    pub fn deinit(cache: *Dedupe) void {
        cache.items.deinit(cache.arena.child_allocator);
        cache.arena.deinit();
        cache.* = undefined;
    }

    /// Gets or inserts a string into the cache. `string` might be a short-lived value,
    /// the returned value is guaranteed to have the livetime of the string cache.
    pub fn fetch(cache: *Dedupe, string: []const u8) ![]const u8 {
        const allocator = cache.arena.child_allocator;
        const gop = try cache.items.getOrPut(allocator, string);
        if (!gop.found_existing) {
            errdefer _ = cache.items.remove(string);
            gop.key_ptr.* = try cache.arena.allocator().dupe(u8, string);
        }
        return gop.key_ptr.*;
    }
};

test Pool {
    var pool = try Pool.init(std.testing.allocator);
    defer pool.deinit();

    try std.testing.expectEqualStrings("", pool.get(.empty));

    try std.testing.expectEqual(String.empty, try pool.insert(""));

    const a = try pool.insert("hello, world!");
    const b = try pool.insert("world!"); // suffix of a
    const c = try pool.insert("world"); // non-suffix

    // All strings must be unique:
    try std.testing.expect(a != b);
    try std.testing.expect(a != c);
    try std.testing.expect(b != c);

    // But must retain their qualities:
    try std.testing.expectEqualStrings("hello, world!", pool.get(a));
    try std.testing.expectEqualStrings("world!", pool.get(b));
    try std.testing.expectEqualStrings("world", pool.get(c));

    // sequential inserts may never return different values:
    try std.testing.expectEqual(a, try pool.insert("hello, world!"));
    try std.testing.expectEqual(a, try pool.insert("hello, world!"));
    try std.testing.expectEqual(a, try pool.insert("hello, world!"));
    try std.testing.expectEqual(a, try pool.insert("hello, world!"));
}
