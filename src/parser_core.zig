const std = @import("std");

/// Creates a new parser core that provides the core functions for a recursive descent
/// parser.
/// `TokenizerT` is the type of the tokenizer to use.
/// `ignore_list` is a array of token types that will be ignored by this parser core.
/// This is useful to filter out comments and non-significant whitespace.
pub fn ParserCore(comptime TokenizerT: type, comptime ignore_list: anytype) type {
    const ignore_array: [ignore_list.len]TokenizerT.TokenType = ignore_list;
    return struct {
        const Self = @This();

        const Tokenizer = TokenizerT;
        const TokenType = Tokenizer.TokenType;
        const Token = Tokenizer.Token;

        pub const Rule = fn (TokenType) bool;

        pub const State = Tokenizer.State;

        pub const Error = AcceptError || Tokenizer.Error;

        tokenizer: *Tokenizer,

        /// Create a new parser core based on the tokenizer given.
        /// The core will only reference the Tokenizer and will modify
        /// it's state.
        pub fn init(tokenizer: *Tokenizer) Self {
            return Self{
                .tokenizer = tokenizer,
            };
        }

        /// Yields the next filtered token
        pub fn nextToken(self: *Self) !?Token {
            while (try self.tokenizer.next()) |tok| {
                if (std.mem.indexOfScalar(TokenType, &ignore_array, tok.type) == null)
                    return tok;
            }
            return null;
        }

        /// Saves the state of the parser. Can be later restored with `restoreState`.
        pub fn saveState(self: Self) State {
            return self.tokenizer.saveState();
        }

        /// Restores a previously saved state. This will rewind any parsing done
        /// since the
        pub fn restoreState(self: *Self, state: State) void {
            self.tokenizer.restoreState(state);
        }

        pub const AcceptError = error{ EndOfStream, UnexpectedToken } || Tokenizer.NextError;
        /// Accepts a token that matches `rule`. Otherwise returns
        /// - `error.EndOfStream` when no tokens are available
        /// - `error.UnexpectedToken` when an invalid token was encountered
        pub fn accept(self: *Self, comptime rule: Rule) AcceptError!Token {
            const state = self.saveState();
            errdefer self.restoreState(state);

            const token = (try self.nextToken()) orelse return error.EndOfStream;

            if (rule(token.type))
                return token;

            return error.UnexpectedToken;
        }

        /// Returns the next token if any, but doesn't modify the state of the parser.
        pub fn peek(self: *Self) Tokenizer.NextError!?Token {
            const state = self.saveState();
            defer self.restoreState(state);

            return try self.nextToken();
        }
    };
}

/// Provides generic rules to match tokens.
pub fn RuleSet(comptime TokenType: type) type {
    return struct {
        const Rule = fn (TokenType) bool;

        /// Returns a rule that matches one of the given token types.
        /// Usage: `expect(oneOf(.{ .foo, .bar }))`
        pub fn oneOf(comptime types: anytype) Rule {
            const types_array: [types.len]TokenType = types;
            return struct {
                fn f(t: TokenType) bool {
                    return (std.mem.indexOfScalar(TokenType, &types_array, t) != null);
                }
            }.f;
        }

        /// Returns a rule that matches the `expected` token type and nothing else.
        pub fn is(comptime expected: TokenType) Rule {
            return struct {
                fn f(t: TokenType) bool {
                    return (t == expected);
                }
            }.f;
        }

        /// A rule that matches *any* token.
        pub fn any(_: TokenType) bool {
            return true;
        }
    };
}
