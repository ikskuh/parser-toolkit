const std = @import("std");

const Location = @import("Location.zig");
const GenericToken = @import("token.zig").Token;

pub const Matcher = *const fn (str: []const u8) ?usize;

pub fn Pattern(comptime TokenType: type) type {
    return struct {
        const Self = @This();

        type: TokenType,
        match: Matcher,

        pub fn create(token_type: TokenType, match: Matcher) Self {
            return Self{
                .type = token_type,
                .match = match,
            };
        }
    };
}

pub fn Tokenizer(comptime TokenTypeT: type, comptime patterns: []const Pattern(TokenTypeT)) type {
    return struct {
        const Self = @This();
        pub const Token = GenericToken(TokenTypeT);
        pub const TokenType = TokenTypeT;

        pub const State = struct {
            offset: usize,
            location: Location,
        };

        source: []const u8,
        offset: usize,
        current_location: Location,

        pub fn init(source: []const u8, file_name: ?[]const u8) Self {
            return Self{
                .source = source,
                .offset = 0,
                .current_location = Location{
                    .source = file_name,
                    .line = 1,
                    .column = 1,
                },
            };
        }

        pub fn saveState(self: Self) State {
            return State{
                .offset = self.offset,
                .location = self.current_location,
            };
        }

        pub fn restoreState(self: *Self, state: State) void {
            self.offset = state.offset;
            self.current_location = state.location;
        }

        pub const NextError = error{UnexpectedCharacter};
        pub fn next(self: *Self) NextError!?Token {
            const rest = self.source[self.offset..];
            if (rest.len == 0)
                return null;
            const maybe_token = for (patterns) |pat| {
                if (pat.match(rest)) |len| {
                    if (len > 0) {
                        break Token{
                            .location = self.current_location,
                            .text = rest[0..len],
                            .type = pat.type,
                        };
                    }
                }
            } else null;
            if (maybe_token) |token| {
                self.offset += token.text.len;
                self.current_location.advance(token.text);
                return token;
            } else {
                return error.UnexpectedCharacter;
            }
        }
    };
}

pub const matchers = struct {
    /// Matches the literal `text`.
    pub fn literal(comptime text: []const u8) Matcher {
        return struct {
            fn match(str: []const u8) ?usize {
                return if (std.mem.startsWith(u8, str, text))
                    text.len
                else
                    null;
            }
        }.match;
    }

    /// Matches any "word" that is "text\b"
    pub fn word(comptime text: []const u8) Matcher {
        return struct {
            fn match(input: []const u8) ?usize {
                if (std.mem.startsWith(u8, input, text)) {
                    if (text.len == input.len)
                        return text.len;
                    const c = input[text.len];
                    if (std.ascii.isAlphanumeric(c) or (c == '_')) // matches regex \w\W
                        return null;
                    return text.len;
                }

                return null;
            }
        }.match;
    }

    /// Takes characters while they are any of the given `chars`.
    pub fn takeAnyOf(comptime chars: []const u8) Matcher {
        return struct {
            fn match(str: []const u8) ?usize {
                for (str, 0..) |c, i| {
                    if (std.mem.indexOfScalar(u8, chars, c) == null) {
                        return i;
                    }
                }
                return str.len;
            }
        }.match;
    }

    /// Takes characters while they are any of the given `chars`.
    pub fn takeAnyOfIgnoreCase(comptime chars: []const u8) Matcher {
        const lower_chars = comptime blk: {
            comptime var buffer: [chars.len]u8 = undefined;
            break :blk std.ascii.lowerString(&buffer, chars);
        };

        return struct {
            fn match(str: []const u8) ?usize {
                for (str, 0..) |c, i| {
                    const lc = std.ascii.toLower(c);
                    if (std.mem.indexOfScalar(u8, lower_chars, lc) == null) {
                        return i;
                    }
                }
                return str.len;
            }
        }.match;
    }

    /// Takes characters while they are not any of the given `chars`.
    pub fn takeNoneOf(comptime chars: []const u8) Matcher {
        return struct {
            fn match(str: []const u8) ?usize {
                for (str, 0..) |c, i| {
                    if (std.mem.indexOfScalar(u8, chars, c) != null) {
                        return i;
                    }
                }
                return str.len;
            }
        }.match;
    }

    pub fn withPrefix(comptime prefix: []const u8, comptime matcher: Matcher) Matcher {
        return struct {
            fn match(str: []const u8) ?usize {
                if (!std.mem.startsWith(u8, str, prefix))
                    return null;
                const pattern_len = matcher(str[prefix.len..]) orelse return null;
                return prefix.len + pattern_len;
            }
        }.match;
    }

    /// Concats several matchers into a single one that matches a sequence of all patterns
    pub fn sequenceOf(comptime list: anytype) Matcher {
        const sequence: [list.len]Matcher = list;
        if (sequence.len == 0)
            @compileError("Empty sequence not allowed!");
        return struct {
            fn match(input: []const u8) ?usize {
                var total_len: usize = 0;
                for (sequence) |seq_match| {
                    const len = seq_match(input[total_len..]) orelse return null;
                    if (len == 0)
                        return null;
                    total_len += len;
                }
                return total_len;
            }
        }.match;
    }

    // pre-shipped typical patterns

    pub fn identifier(str: []const u8) ?usize {
        const first_char = "_abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ";
        const all_chars = first_char ++ "0123456789";
        for (str, 0..) |c, i| {
            if (std.mem.indexOfScalar(u8, if (i > 0) all_chars else first_char, c) == null) {
                return i;
            }
        }
        return str.len;
    }

    pub fn whitespace(str: []const u8) ?usize {
        for (str, 0..) |c, i| {
            if (!std.ascii.isWhitespace(c))
                return i;
        }
        return str.len;
    }

    pub fn linefeed(str: []const u8) ?usize {
        if (std.mem.startsWith(u8, str, "\r\n"))
            return 2;
        if (std.mem.startsWith(u8, str, "\n"))
            return 1;
        return null;
    }

    pub fn numberOfBase(comptime base: comptime_int) Matcher {
        return takeAnyOfIgnoreCase("0123456789ABCDEF"[0..base]);
    }

    pub const hexadecimalNumber = numberOfBase(16);
    pub const decimalNumber = numberOfBase(10);
    pub const octalNumber = numberOfBase(8);
    pub const binaryNumber = numberOfBase(2);
};

const TestTokenType = enum {
    number,
    identifier,
    keyword,
    whitespace,
};

const TestPattern = Pattern(TestTokenType);

const TestTokenizer = Tokenizer(TestTokenType, &[_]TestPattern{
    TestPattern.create(.number, matchers.takeAnyOf("0123456789")),
    TestPattern.create(.keyword, matchers.literal("while")),
    TestPattern.create(.identifier, matchers.takeAnyOf("abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ")),
    TestPattern.create(.whitespace, matchers.takeAnyOf(" \r\n\t")),
});

test "simple tokenization" {
    var tokenizer = TestTokenizer.init(
        \\10hellowhile10while
    , null);
    const number0 = (try tokenizer.next()) orelse return error.MissingToken;
    const identifier = (try tokenizer.next()) orelse return error.MissingToken;
    const number1 = (try tokenizer.next()) orelse return error.MissingToken;
    const keyword = (try tokenizer.next()) orelse return error.MissingToken;
    if ((try tokenizer.next()) != null)
        return error.TooManyTokens;

    try std.testing.expectEqual(TestTokenType.number, number0.type);
    try std.testing.expectEqual(TestTokenType.identifier, identifier.type);
    try std.testing.expectEqual(TestTokenType.number, number1.type);
    try std.testing.expectEqual(TestTokenType.keyword, keyword.type);

    try std.testing.expectEqualStrings("10", number0.text);
    try std.testing.expectEqualStrings("hellowhile", identifier.text);
    try std.testing.expectEqualStrings("10", number1.text);
    try std.testing.expectEqualStrings("while", keyword.text);

    try std.testing.expectEqual(Location{ .source = null, .line = 1, .column = 1 }, number0.location);
    try std.testing.expectEqual(Location{ .source = null, .line = 1, .column = 3 }, identifier.location);
    try std.testing.expectEqual(Location{ .source = null, .line = 1, .column = 13 }, number1.location);
    try std.testing.expectEqual(Location{ .source = null, .line = 1, .column = 15 }, keyword.location);
}

test "invalid character" {
    var tokenizer = TestTokenizer.init(
        \\hello!
    , null);
    const identifier = (try tokenizer.next()) orelse return error.MissingToken;
    try std.testing.expectEqual(TestTokenType.identifier, identifier.type);
    try std.testing.expectEqualStrings("hello", identifier.text);
    try std.testing.expectEqual(Location{ .source = null, .line = 1, .column = 1 }, identifier.location);

    try std.testing.expectError(error.UnexpectedCharacter, tokenizer.next());
}

test "save/restore tokenization" {
    var tokenizer = TestTokenizer.init(
        \\hello
        \\world
    , null);

    const id0 = (try tokenizer.next()) orelse return error.MissingToken;
    var state = tokenizer.saveState();
    const ws0 = (try tokenizer.next()) orelse return error.MissingToken;
    tokenizer.restoreState(state);
    const ws1 = (try tokenizer.next()) orelse return error.MissingToken;
    const id1 = (try tokenizer.next()) orelse return error.MissingToken;
    if ((try tokenizer.next()) != null)
        return error.TooManyTokens;

    try std.testing.expectEqual(TestTokenType.identifier, id0.type);
    try std.testing.expectEqual(TestTokenType.whitespace, ws0.type);
    try std.testing.expectEqual(TestTokenType.whitespace, ws1.type);
    try std.testing.expectEqual(TestTokenType.identifier, id1.type);

    try std.testing.expectEqualStrings("hello", id0.text);
    try std.testing.expectEqualStrings("\n", ws0.text);
    try std.testing.expectEqualStrings("\n", ws1.text);
    try std.testing.expectEqualStrings("world", id1.text);

    try std.testing.expectEqual(Location{ .source = null, .line = 1, .column = 1 }, id0.location);
    try std.testing.expectEqual(Location{ .source = null, .line = 1, .column = 6 }, ws0.location);
    try std.testing.expectEqual(Location{ .source = null, .line = 1, .column = 6 }, ws1.location);
    try std.testing.expectEqual(Location{ .source = null, .line = 2, .column = 1 }, id1.location);
}

fn testMatcher(match: Matcher, good: []const []const u8, bad: []const []const u8) !void {
    for (good) |str| {
        const v = match(str) orelse {
            std.log.err("Didn't match pattern '{s}'", .{str});
            return error.MissedGoodPattern;
        };
        if (v == 0) {
            std.log.err("Didn't match pattern '{s}'", .{str});
            return error.MissedGoodPattern;
        }
    }
    for (bad) |str| {
        const v = match(str);
        if (v != null and v.? > 0) {
            std.log.err("Matched pattern '{s}'", .{str});
            return error.MissedBadPattern;
        }
    }
}

test "premade patterns" {
    try testMatcher(
        matchers.literal("hello"),
        &[_][]const u8{ "hello", "hellobar", "hello_bar" },
        &[_][]const u8{ "gello", "foobar", " hello " },
    );
    try testMatcher(
        matchers.word("hello"),
        &[_][]const u8{ "hello", "hello bar" },
        &[_][]const u8{ "hellobar", "hello_bar", "gello", "foobar", " hello " },
    );
    try testMatcher(
        comptime matchers.withPrefix("foo_", matchers.word("hello")),
        &[_][]const u8{ "foo_hello", "foo_hello bar" },
        &[_][]const u8{ "foo_hellobar", "foo_hello_bar", "hello", "gello", "foobar", " hello " },
    );
    try testMatcher(
        comptime matchers.takeAnyOf("abc"),
        &[_][]const u8{ "a", "b", "c", "abc", "a x", "b x", "c x", "abcabcabcabc" },
        &[_][]const u8{ "xabc", "xa", "xb", " aaa", "x asd", " x " },
    );
    try testMatcher(
        comptime matchers.takeAnyOfIgnoreCase("abc"),
        &[_][]const u8{ "a", "b", "c", "abc", "a x", "b x", "c x", "abcabcabcabc", "A", "B", "C", "ABC", "A X", "B X", "C X", "ABCABCABCABC" },
        &[_][]const u8{ "xabc", "xa", "xb", " aaa", "x asd", " x ", "XABC", "XA", "XB", " AAA", "X ASD", " X " },
    );
    try testMatcher(
        matchers.identifier,
        &[_][]const u8{ "foo", "Foo", "hello_bar", "_bar", "he11o" },
        &[_][]const u8{ "10foo", "!", " foo " },
    );
    try testMatcher(
        matchers.whitespace,
        &[_][]const u8{ " ", " a", "\r", "\n", "\t" },
        &[_][]const u8{ "10foo", "!", "foo " },
    );
    try testMatcher(
        matchers.linefeed,
        &[_][]const u8{ "\n", "\r\n", "\nfoo", "\r\nbar" },
        &[_][]const u8{ "\r\r\n", "!", "  " },
    );
    try testMatcher(
        matchers.decimalNumber,
        &[_][]const u8{ "10", "99", "1234567890" },
        &[_][]const u8{ "a", "b", " 123 " },
    );
    try testMatcher(
        matchers.binaryNumber,
        &[_][]const u8{ "10", "01", "1100101" },
        &[_][]const u8{ "2", "3", " 01 " },
    );
    try testMatcher(
        comptime matchers.sequenceOf(.{ matchers.decimalNumber, matchers.literal("."), matchers.decimalNumber }),
        &[_][]const u8{ "10.0", "1.0", "0.01234" },
        &[_][]const u8{ ".1", "1", "10", ".1234", "10." },
    );
}
