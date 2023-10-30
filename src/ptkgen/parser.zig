const std = @import("std");
const ptk = @import("parser-toolkit");

pub const Document = struct {
    arena: std.heap.ArenaAllocator,

    file_name: []const u8,
    source_text: []const u8,

    pub fn deinit(ts: *Document) void {
        ts.arena.deinit();
        ts.* = undefined;
    }
};

pub fn parse(allocator: std.mem.Allocator, diagnostics: *ptk.Diagnostics, file_name: []const u8, stream: anytype) !Document {
    var arena = std.heap.ArenaAllocator.init(allocator);
    errdefer arena.deinit();

    const file_name_copy = try arena.allocator().dupe(u8, file_name);

    const text = try stream.readAllAlloc(arena.allocator(), 4 << 20); // 4 MB should be enough for now...

    var tokenizer = Tokenizer.init(text, file_name_copy);

    while (true) {
        const token_or_none = tokenizer.next() catch |err| switch (err) {
            error.UnexpectedCharacter => {
                try diagnostics.emit(tokenizer.current_location, .@"error", "Unexpected character: '{}'", .{
                    std.zig.fmtEscapes(tokenizer.source[tokenizer.offset..][0..1]),
                });
                return error.SyntaxError;
            },

            else => |e| return e,
        };
        const token = token_or_none orelse break;

        std.log.info("token: {}", .{token});
    }

    return Document{
        .arena = arena,
        .file_name = file_name_copy,
        .source_text = text,
    };
}

pub const TokenType = enum {

    // keywords

    node,
    @"struct",
    optional,
    start,
    rule,
    token,

    literal,
    custom,
    regex,
    skip,

    // user values

    raw_identifier, // foo-bar_bam
    node_ref, // !node
    rule_ref, // <rule>
    token_ref, // $token
    value_ref, // $0
    builtin_ref, // @builtin

    // values

    string_literal, // "string"
    code_literal, // `code`

    // operators

    @"=",
    @",",
    @".",
    @"*",
    @"+",
    @":",
    @";",
    @"|",
    @"!",
    @"?",
    @"[",
    @"]",
    @"(",
    @")",
    @"{",
    @"}",
    @"=>",

    // auxiliary

    line_comment,
    whitespace,
};

pub const Token = Tokenizer.Token;

const match = ptk.matchers;

const Pattern = ptk.Pattern(TokenType);

const ParserCore = ptk.ParserCore(TokenType, .{ .whitespace, .line_comment });

const Tokenizer = ptk.Tokenizer(TokenType, &.{
    Pattern.create(.line_comment, match.sequenceOf(.{ match.literal("#"), match.takeNoneOf("\r\n") })),

    Pattern.create(.node, match.word("node")),
    Pattern.create(.@"struct", match.word("struct")),
    Pattern.create(.optional, match.word("optional")),
    Pattern.create(.start, match.word("start")),
    Pattern.create(.rule, match.word("rule")),
    Pattern.create(.token, match.word("token")),
    Pattern.create(.literal, match.word("literal")),
    Pattern.create(.custom, match.word("custom")),
    Pattern.create(.regex, match.word("regex")),
    Pattern.create(.skip, match.word("skip")),

    Pattern.create(.@"=>", match.literal("=>")),

    Pattern.create(.@"=", match.literal("=")),
    Pattern.create(.@",", match.literal(",")),
    Pattern.create(.@".", match.literal(".")),
    Pattern.create(.@"*", match.literal("*")),
    Pattern.create(.@"+", match.literal("+")),
    Pattern.create(.@":", match.literal(":")),
    Pattern.create(.@";", match.literal(";")),
    Pattern.create(.@"|", match.literal("|")),
    Pattern.create(.@"!", match.literal("!")),
    Pattern.create(.@"?", match.literal("?")),
    Pattern.create(.@"[", match.literal("[")),
    Pattern.create(.@"]", match.literal("]")),
    Pattern.create(.@"(", match.literal("(")),
    Pattern.create(.@")", match.literal(")")),
    Pattern.create(.@"{", match.literal("{")),
    Pattern.create(.@"}", match.literal("}")),

    Pattern.create(.string_literal, matchStringLiteral),
    Pattern.create(.code_literal, matchCodeLiteral),

    // identifiers must come after keywords:
    Pattern.create(.raw_identifier, matchRawIdentifier),
    Pattern.create(.node_ref, matchNodeRef),
    Pattern.create(.rule_ref, matchRuleRef),
    Pattern.create(.token_ref, matchTokenRef),
    Pattern.create(.value_ref, matchValueRef),
    Pattern.create(.builtin_ref, matchBuiltinRef),

    // Whitespace is the "kitchen sink" at the end:
    Pattern.create(.whitespace, match.takeAnyOf(" \r\n\t")),
});

/// Accepts a basic identifier without any prefix or suffix.
/// The regex that matches this pattern is roughly this:
///
///     (@\"[^"]+\")|([A-Za-z_][A-Za-z0-9_\-]*)
///
fn matchRawIdentifier(text: []const u8) usize {
    if (text.len < 1)
        return 0;

    if (std.mem.startsWith(u8, text, "@\"")) {
        if (text.len < 3)
            return 0;

        var i: usize = 2; // skip `@"`
        while (i < text.len) : (i += 1) {
            if (text[i] == '\"')
                return i + 1;
            if (text[i] == '\\')
                i += 1;
        }

        return 0;
    } else {
        const prefix_chars = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz_";
        const suffix_chars = prefix_chars ++ "0123456789";
        const inner_chars = suffix_chars ++ "-";

        if (std.mem.indexOfScalar(u8, prefix_chars, text[0]) == null)
            return 0; // invalid start char

        // Suffix check is done in "postprocessing" by checking if any identifier ends with "-"

        var len: usize = 1;
        while (len < text.len and std.mem.indexOfScalar(u8, inner_chars, text[len]) != null) {
            len += 1;
        }

        return len;
    }

    return 0;
}

test matchRawIdentifier {
    try ptk.testing.validateMatcher(matchRawIdentifier, &.{
        // good:
        "a",
        "a-z",
        "items10",
        "_foo",
        "_",
        "_cheese-cake",
    }, &.{
        // bad:
        "-",
        "-10",
        "10",
        "1-2",
        "10items",
    });
}

const matchNodeRef = match.sequenceOf(.{ match.literal("!"), matchRawIdentifier });

test matchNodeRef {
    try ptk.testing.validateMatcher(matchNodeRef, &.{
        // good:
        "!a",
        "!foo_bar",
    }, &.{
        // bad:
        "a",
        "!",
    });
}

const matchRuleRef = match.sequenceOf(.{ match.literal("<"), matchRawIdentifier, match.literal(">") });

test matchRuleRef {
    try ptk.testing.validateMatcher(matchRuleRef, &.{
        // good:
        "<foo>",
        "<bad-boy>",
        "<good_boy>",
        "<@\"very exiting boy\">",
    }, &.{
        // bad:
        "<foo",
        "foo",
        "foo>",
    });
}

const matchTokenRef = match.sequenceOf(.{ match.literal("$"), matchRawIdentifier });

test matchTokenRef {
    try ptk.testing.validateMatcher(matchTokenRef, &.{
        // good:
        "$token",
        "$user-token",
        "$user_token",
        "$@\"wtf\"",
    }, &.{
        // bad:
        "$\"wtf\"",
        "bad boy",
        "bad-boy",
        "$0",
        "$100",
    });
}

const matchValueRef = match.sequenceOf(.{ match.literal("$"), match.decimalNumber });

test matchValueRef {
    try ptk.testing.validateMatcher(matchValueRef, &.{
        // good:
        "$0",
        "$10",
        "$99999999",
    }, &.{
        // bad:
        "9",
        "$",
        "$foo",
    });
}

const matchBuiltinRef = match.sequenceOf(.{ match.literal("@"), matchRawIdentifier });

test matchBuiltinRef {
    try ptk.testing.validateMatcher(matchBuiltinRef, &.{
        // good:
        "@token",
        "@user-token",
        "@user_token",
        "@@\"wtf\"",
    }, &.{
        // bad:
        "@\"wtf\"",
        "bad boy",
        "bad-boy",
        "@0",
        "@100",
    });
}

fn matchStringLiteral(text: []const u8) usize {
    if (text.len < 2)
        return 0;

    if (text[0] != '"')
        return 0;

    var i: usize = 1; // skip `"`
    while (i < text.len) : (i += 1) {
        if (text[i] == '\"')
            return i + 1;
        if (text[i] == '\\')
            i += 1;
    }

    return 0;
}

test matchStringLiteral {
    try ptk.testing.validateMatcher(matchStringLiteral, &.{
        // good:
        "\"\"",
        "\"x\"",
        "\" \"",
        "\" hello \\\"world\\\"\"",
    }, &.{
        // bad:
        "\"",
        "\"\\\"",
        "\"",
        "foo\"",
    });
}

fn matchCodeLiteral(text: []const u8) usize {
    var prefix_len: usize = 0;
    while (prefix_len < text.len and text[prefix_len] == '`') {
        prefix_len += 1;
    }

    if (prefix_len == 0 or 2 * prefix_len >= text.len)
        return 0;

    const body_len = std.mem.indexOf(u8, text[prefix_len..], text[0..prefix_len]) orelse return 0;

    return 2 * prefix_len + body_len;
}

test matchCodeLiteral {
    try ptk.testing.validateMatcher(matchCodeLiteral, &.{
        // good:
        "`x`",
        "`\"hello, World!\"`",
        "`\n\n`",
        "`\x00`",
        "``you can write a `code` snippet like this!``",
    }, &.{
        // bad:
        "`",
        "``",
        "```hello, world!``",
    });
}
