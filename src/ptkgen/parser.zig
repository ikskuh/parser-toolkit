const std = @import("std");
const ptk = @import("parser-toolkit");
const ast = @import("ast.zig");

const Diagnostics = @import("Diagnostics.zig");

const fmtEscapes = std.zig.fmtEscapes;

pub const Document = struct {
    arena: std.heap.ArenaAllocator,

    file_name: []const u8,
    source_text: []const u8,

    top_level_declarations: ast.Document,

    pub fn deinit(ts: *Document) void {
        ts.arena.deinit();
        ts.* = undefined;
    }
};

pub fn parse(allocator: std.mem.Allocator, diagnostics: *Diagnostics, string_pool: *ptk.strings.Pool, file_name: []const u8, stream: anytype) !Document {
    var arena = std.heap.ArenaAllocator.init(allocator);
    errdefer arena.deinit();

    const file_name_copy = try arena.allocator().dupe(u8, file_name);

    const text = try stream.readAllAlloc(arena.allocator(), 4 << 20); // 4 MB should be enough for now...

    var tokenizer = Tokenizer.init(text, file_name_copy);

    var parser = Parser{
        .core = ParserCore.init(&tokenizer),
        .arena = arena.allocator(),
        .pool = string_pool,
        .diagnostics = diagnostics,
    };

    const document_node = parser.acceptDocument() catch |err| switch (err) {

        // Unrecoverable syntax error, must have created diagnostics already
        error.SyntaxError, error.InvalidSourceEncoding => |e| {
            std.debug.assert(diagnostics.hasErrors());
            return e;
        },

        error.OutOfMemory => |e| return e,
    };

    if (tokenizer.next()) |token_or_null| {
        if (token_or_null) |token| {
            try diagnostics.emit(token.location, .excess_tokens, .{ .token_type = token.type });
            return error.SyntaxError;
        }
    } else |_| {
        try parser.emitUnexpectedCharacter(tokenizer.current_location, tokenizer.offset);
        return error.SyntaxError;
    }

    return Document{
        .arena = arena,
        .file_name = file_name_copy,
        .source_text = text,

        .top_level_declarations = document_node,
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

    identifier, // foo-bar_bam
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

const ParserCore = ptk.ParserCore(Tokenizer, .{ .whitespace, .line_comment });

const Parser = struct {
    const RS = ptk.RuleSet(TokenType);
    const String = ptk.strings.String;

    core: ParserCore,
    arena: std.mem.Allocator,
    pool: *ptk.strings.Pool,
    diagnostics: *Diagnostics,

    pub fn acceptDocument(parser: *Parser) !ast.Document {
        var doc = ast.Document{};

        while (true) {
            const decl_or_eof = try parser.acceptTopLevelDecl();

            const decl = decl_or_eof orelse break;

            try parser.append(ast.TopLevelDeclaration, &doc, decl);
        }

        return doc;
    }

    fn emitDiagnostic(parser: Parser, loc: ?ptk.Location, comptime code: Diagnostics.Code, data: Diagnostics.Data(code)) !void {
        // Anything detected here is always an error
        std.debug.assert(code.isError());
        try parser.diagnostics.emit(loc orelse parser.core.tokenizer.current_location, code, data);
    }

    fn emitUnexpectedCharacter(parser: Parser, location: ptk.Location, source_offset: usize) !void {
        var utf8_view = std.unicode.Utf8View.init(parser.core.tokenizer.source[source_offset..]) catch {
            try parser.emitDiagnostic(location, .invalid_source_encoding, .{});
            return error.InvalidSourceEncoding;
        };

        var iter = utf8_view.iterator();

        try parser.emitDiagnostic(location, .unexpected_character, .{
            .character = iter.nextCodepoint() orelse @panic("very unexpected end of file"),
        });
    }

    fn acceptTopLevelDecl(parser: *Parser) !?ast.TopLevelDeclaration {
        if (parser.acceptRule()) |rule| {
            return .{ .rule = rule };
        } else |err| try filterAcceptError(err);

        // Detect any excess tokens on the top level:
        const excess_tokens = if (parser.core.nextToken()) |token|
            (token != null)
        else |err| switch (err) {
            error.UnexpectedCharacter => true,
        };
        if (excess_tokens) {
            try parser.emitDiagnostic(null, .unexpected_eof, .{});
            return error.SyntaxError;
        }

        return null;
    }

    fn acceptRule(parser: *Parser) !ast.Rule {
        var state = parser.save();
        errdefer parser.restore(state);

        try parser.acceptLiteral(.rule, .recover);

        const identifier = try parser.acceptIdentifier(.fail);

        const rule_type = if (try parser.tryAcceptLiteral(.@":"))
            try parser.acceptTypeSpec()
        else
            null;

        try parser.acceptLiteral(.@"=", .fail);

        var list: ast.List(ast.MappedProduction) = .{};

        while (true) {
            var production = try parser.acceptMappedProduction();

            try parser.append(ast.MappedProduction, &list, production);

            // TODO: Improve error reporting here
            if (try parser.tryAcceptLiteral(.@";")) {
                break;
            }

            try parser.acceptLiteral(.@"|", .fail);
        }

        return ast.Rule{
            .ast_type = rule_type,
            .productions = list,
            .name = identifier,
        };
    }

    fn acceptMappedProduction(parser: *Parser) !ast.MappedProduction {
        var sequence = try parser.acceptProductionSequence();

        const mapping = if (try parser.tryAcceptLiteral(.@"=>"))
            try parser.acceptAstMapping()
        else
            null;

        return ast.MappedProduction{
            .production = if (sequence.only()) |item|
                item
            else
                .{ .sequence = sequence },
            .mapping = mapping,
        };
    }

    fn acceptProductionSequence(parser: *Parser) !ast.List(ast.Production) {
        var list: ast.List(ast.Production) = .{};

        while (true) {
            if (parser.acceptProduction()) |prod| {
                try parser.append(ast.Production, &list, prod);
            } else |err| switch (err) {
                error.UnexpectedTokenRecoverable => break,
                error.OutOfMemory, error.InvalidSourceEncoding, error.SyntaxError => |e| return e,
            }
        }

        return list;
    }

    fn acceptProduction(parser: *Parser) !ast.Production {
        const str = try parser.acceptStringLiteral(.recover);

        return ast.Production{
            .literal = str,
        };
    }

    fn acceptAstMapping(parser: *Parser) !ast.AstMapping {
        _ = parser;
        @panic("not implemented yet");
    }

    fn acceptTypeSpec(parser: *Parser) !ast.TypeSpec {
        _ = parser;
        @panic("not implemented yet");
    }

    fn acceptStringLiteral(parser: *Parser, accept_mode: AcceptMode) !ast.StringLiteral {
        const token = try parser.acceptToken(.string_literal, accept_mode);

        std.debug.assert(token.text.len >= 2);

        return ast.StringLiteral{
            .location = token.location,
            .value = try parser.unwrapString(token.location, token.text[1 .. token.text.len - 1]),
        };
    }

    fn acceptIdentifier(parser: *Parser, accept_mode: AcceptMode) !ast.Identifier {
        const token = try parser.acceptToken(.identifier, accept_mode);

        return ast.Identifier{
            .location = token.location,
            .value = try parser.unwrapIdentifierString(token.location, token.text),
        };
    }

    fn acceptLiteral(parser: *Parser, comptime token_type: TokenType, accept_mode: AcceptMode) !void {
        _ = try parser.acceptToken(token_type, accept_mode);
    }

    fn tryAcceptLiteral(parser: *Parser, comptime token_type: TokenType) !bool {
        _ = parser.acceptToken(token_type, .recover) catch |err| switch (err) {
            error.UnexpectedTokenRecoverable => return false,
            error.OutOfMemory, error.InvalidSourceEncoding, error.SyntaxError => |e| return e,
        };
        return true;
    }

    /// Tries to accept a given token and will emit a diagnostic if it fails.
    fn acceptToken(parser: *Parser, comptime token_type: TokenType, accept_mode: AcceptMode) AcceptError!Token {
        const saved_state = parser.save();
        errdefer parser.restore(saved_state);

        const source_offset = parser.core.tokenizer.offset;
        const location = parser.core.tokenizer.current_location;

        if (parser.core.accept(RS.any)) |token| {
            // std.log.debug("token trace: {}", .{token});

            if (token.type != token_type) {
                switch (accept_mode) {
                    .fail => {
                        try parser.emitDiagnostic(location, .unexpected_token, .{
                            .expected_type = token_type,
                            .actual_type = token.type,
                            .actual_text = token.text,
                        });
                        return error.SyntaxError;
                    },
                    .recover => return error.UnexpectedTokenRecoverable,
                }
            }
            return token;
        } else |err| switch (err) {
            error.UnexpectedToken => unreachable, // RS.any will always accept the token
            error.EndOfStream => switch (accept_mode) {
                .fail => {
                    try parser.emitDiagnostic(location, .unexpected_token_eof, .{ .expected_type = token_type });
                    return error.SyntaxError;
                },
                .recover => return error.UnexpectedTokenRecoverable,
            },
            error.UnexpectedCharacter => {
                try parser.emitUnexpectedCharacter(location, source_offset);
                return error.SyntaxError;
            },
        }
    }

    const AcceptMode = enum {
        /// Will emit a syntax error with diagnostic
        fail,

        /// Is recoverable
        recover,
    };

    // management:

    fn unwrapIdentifierString(parser: *Parser, loc: ptk.Location, raw: []const u8) !ptk.strings.String {
        std.debug.assert(raw.len > 0);
        if (raw[0] == '@') {
            std.debug.assert(raw[1] == '"');
            std.debug.assert(raw[raw.len - 1] == '"');
            // string-escaped identifier
            return try parser.unwrapString(loc, raw[2 .. raw.len - 1]);
        } else {
            return try parser.pool.insert(raw);
        }
    }

    fn unwrapString(parser: *Parser, loc: ptk.Location, raw: []const u8) !ptk.strings.String {
        var fallback = std.heap.stackFallback(512, parser.arena);

        var working_space = std.ArrayList(u8).init(fallback.get());
        defer working_space.deinit();

        var i: usize = 0;
        while (i < raw.len) {
            const c = raw[i];
            if (c == '\\') {
                i += 1;
                if (i >= raw.len) {
                    try parser.emitDiagnostic(loc, .bad_string_escape, .{});
                    return error.SyntaxError;
                }
                const escape = raw[i];
                const slice = switch (escape) {
                    'n' => "\n",
                    'r' => "\r",
                    '\"' => "\"",
                    '\'' => "\'",
                    '\\' => "\\",

                    'x' => @panic("Implement hex escape \\x??"),
                    'u' => @panic("Implement utf-16 \\u????"),
                    'U' => @panic("Implement utf-32 \\U????????"),

                    '0'...'3' => @panic("Implement octal escape \\???"),

                    else => {
                        try parser.emitDiagnostic(loc, .invalid_string_escape, .{ .escape = escape });
                        return error.SyntaxError;
                    },
                };
                try working_space.appendSlice(slice);
            } else {
                try working_space.append(c);
            }
            i += 1;
        }

        return try parser.pool.insert(working_space.items);
    }

    fn save(parser: Parser) ParserCore.State {
        return parser.core.saveState();
    }

    fn restore(parser: *Parser, state: ParserCore.State) void {
        parser.core.restoreState(state);
    }

    fn internString(parser: *Parser, string: []const u8) !String {
        return try parser.pool.insert(string);
    }

    fn append(parser: *Parser, comptime T: type, list: *ast.List(T), item: T) !void {
        const node = try parser.arena.create(ast.List(T).Node);
        errdefer parser.arena.destroy(node);

        node.data = item;

        list.append(node);
    }

    pub const FatalAcceptError = error{
        // We're out of memory accepting some rule. We cannot recover from this.
        OutOfMemory,

        // Something could not be accepted.
        SyntaxError,

        // The source code contained invalid bytes
        InvalidSourceEncoding,
    };

    pub const AcceptError = FatalAcceptError || error{
        // The token stream contains an unexpected token, this is a syntax error
        UnexpectedTokenRecoverable,
    };

    fn filterAcceptError(err: AcceptError) FatalAcceptError!void {
        return switch (err) {
            error.UnexpectedTokenRecoverable => {},

            error.OutOfMemory,
            error.SyntaxError,
            error.InvalidSourceEncoding,
            => |e| return e,
        };
    }
};

const match = ptk.matchers;
const Pattern = ptk.Pattern(TokenType);
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
    Pattern.create(.identifier, matchRawIdentifier),
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
