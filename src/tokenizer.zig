const std = @import("std");

const Location = @import("Location.zig");
const GenericToken = @import("token.zig").Token;

pub const Matcher = fn (str: []const u8) ?usize;

pub fn Pattern(comptime TokenType: type) type {
    return struct {
        const Self = @This();

        type: TokenType,
        match: Matcher,

        /// Matches the literal `text`.
        pub fn literal(comptime kind: TokenType, comptime text: []const u8) Self {
            return Self{
                .type = kind,
                .match = struct {
                    fn match(str: []const u8) ?usize {
                        return if (std.mem.startsWith(u8, str, text))
                            text.len
                        else
                            null;
                    }
                }.match,
            };
        }

        /// Takes characters while they are any of the given `chars`.
        pub fn takeAnyOf(comptime kind: TokenType, comptime chars: []const u8) Self {
            return Self{
                .type = kind,
                .match = struct {
                    fn match(str: []const u8) ?usize {
                        for (str) |c, i| {
                            if (std.mem.indexOfScalar(u8, chars, c) == null) {
                                return if (i > 0)
                                    i
                                else
                                    null;
                            }
                        }
                        return str.len;
                    }
                }.match,
            };
        }
    };
}

pub fn Tokenizer(comptime TokenType: type, comptime patterns: []const Pattern(TokenType)) type {
    return struct {
        const Self = @This();
        pub const Token = GenericToken(TokenType);

        pub const State = struct {
            offset: usize,
            location: Location,
        };

        source: []const u8,
        offset: usize,
        current_location: Location,

        pub fn init(source: []const u8) Self {
            return Self{
                .source = source,
                .offset = 0,
                .current_location = Location{
                    .source = null,
                    .line = 1,
                    .column = 1,
                },
            };
        }

        pub fn deinit(self: *Self) void {
            self.* = undefined;
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

        const NextError = error{UnexpectedCharacter};
        pub fn next(self: *Self) NextError!?Token {
            const rest = self.source[self.offset..];
            if (rest.len == 0)
                return null;
            const maybe_token = for (patterns) |pat| {
                if (pat.match(rest)) |len| {
                    break Token{
                        .location = self.current_location,
                        .text = rest[0..len],
                        .type = pat.type,
                    };
                }
            } else null;
            if (maybe_token) |token| {
                self.offset += token.text.len;
                self.current_location.adavance(token.text);
                return token;
            } else {
                return error.UnexpectedCharacter;
            }
        }
    };
}

const TestTokenType = enum {
    number,
    identifier,
    keyword,
    whitespace,
};

const TestPattern = Pattern(TestTokenType);

const TestTokenizer = Tokenizer(TestTokenType, &[_]TestPattern{
    TestPattern.takeAnyOf(.number, "0123456789"),
    TestPattern.literal(.keyword, "while"),
    TestPattern.takeAnyOf(.identifier, "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ"),
    TestPattern.takeAnyOf(.whitespace, " \r\n\t"),
});

test "simple tokenization" {
    var tokenizer = TestTokenizer.init(
        \\10hellowhile10while
    );
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
    );
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
    );

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
