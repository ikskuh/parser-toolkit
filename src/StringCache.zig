//! The string cache is a interning system for strings that allows deduplication
//! and efficient storage of strings.
//! As a lot of parsers and compilers use a lot strings, this is an efficient storage strategy.

const std = @import("std");

const StringCache = @This();

arena: std.heap.ArenaAllocator,
items: std.StringHashMapUnmanaged(void),

pub fn init(allocator: std.mem.Allocator) StringCache {
    return StringCache{
        .arena = .init(allocator),
        .items = .{},
    };
}

pub fn deinit(cache: *StringCache) void {
    cache.items.deinit(cache.arena.child_allocator);
    cache.arena.deinit();
    cache.* = undefined;
}

/// Gets or inserts a string into the cache. `string` might be a short-lived value,
/// the returned value is guaranteed to have the livetime of the string cache.
pub fn fetch(cache: *StringCache, string: []const u8) ![]const u8 {
    const allocator = cache.arena.child_allocator;
    const gop = try cache.items.getOrPut(allocator, string);
    if (!gop.found_existing) {
        errdefer _ = cache.items.remove(string);
        gop.key_ptr.* = try cache.arena.allocator().dupe(u8, string);
    }
    return gop.key_ptr.*;
}
