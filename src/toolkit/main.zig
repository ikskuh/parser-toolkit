const std = @import("std");

pub const Location = @import("Location.zig");

const tok = @import("tokenizer.zig");

const pcore = @import("parser_core.zig");

pub const Matcher = tok.Matcher;
pub const Pattern = tok.Pattern;
pub const Tokenizer = tok.Tokenizer;
pub const matchers = tok.matchers;

pub const ParserCore = pcore.ParserCore;
pub const RuleSet = pcore.RuleSet;

pub const Error = @import("Error.zig");
pub const Diagnostics = @import("Diagnostics.zig");
pub const StringCache = @import("StringCache.zig");

pub const testing = struct {
    pub const validateMatcher = tok.testMatcher;
};

test {
    _ = Location;
    _ = tok;
    _ = pcore;
}
