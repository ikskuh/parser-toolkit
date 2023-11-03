const std = @import("std");
const ptk = @import("parser-toolkit");
const ast = @import("ast.zig");

const Diagnostics = @import("Diagnostics.zig");

const fmtEscapes = std.zig.fmtEscapes;

pub const Document = struct {
    arena: std.heap.ArenaAllocator,
    file_name: []const u8,
    top_level_declarations: ast.Document,

    pub fn deinit(ts: *Document) void {
        ts.arena.deinit();
        ts.* = undefined;
    }
};

pub fn parse(opt: struct {
    allocator: std.mem.Allocator,
    diagnostics: *Diagnostics,
    string_pool: *ptk.strings.Pool,
    file_name: []const u8,
    source_code: []const u8,
    trace_enabled: bool,
}) !Document {
    var arena = std.heap.ArenaAllocator.init(opt.allocator);
    errdefer arena.deinit();

    const file_name_copy = try arena.allocator().dupe(u8, opt.file_name);

    var tokenizer = Tokenizer.init(opt.source_code, file_name_copy);

    var parser = Parser{
        .core = ParserCore.init(&tokenizer),
        .arena = arena.allocator(),
        .pool = opt.string_pool,
        .diagnostics = opt.diagnostics,
        .trace_enabled = opt.trace_enabled,
    };

    const document_node = parser.acceptDocument() catch |err| switch (err) {

        // Unrecoverable syntax error, must have created diagnostics already
        error.SyntaxError => |e| {
            std.debug.assert(opt.diagnostics.hasErrors());

            if (opt.trace_enabled) {
                if (@errorReturnTrace()) |trace| {
                    std.debug.dumpStackTrace(trace.*);
                }
            }

            return e;
        },
        error.InvalidSourceEncoding => |e| {
            std.debug.assert(opt.diagnostics.hasErrors());

            return e;
        },

        error.OutOfMemory => |e| return e,
    };

    if (tokenizer.next()) |token_or_null| {
        if (token_or_null) |token| {
            try opt.diagnostics.emit(token.location, .excess_tokens, .{ .token_type = token.type });
            return error.SyntaxError;
        }
    } else |_| {
        try parser.emitUnexpectedCharacter(tokenizer.current_location, tokenizer.offset);
        return error.SyntaxError;
    }

    return Document{
        .arena = arena,
        .file_name = file_name_copy,
        .top_level_declarations = document_node,
    };
}

pub const TokenType = enum {
    // keywords

    node,
    record,
    variant,
    optional,
    start,
    rule,
    token,

    custom,
    regex,
    skip,

    // user values

    identifier, // foo-bar_bam
    node_ref, // !node
    rule_ref, // <rule>
    token_ref, // $token
    value_ref, // $0
    userval_ref, // @userval

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

    trace_enabled: bool,
    trace_depth: u32 = 0,

    pub fn acceptDocument(parser: *Parser) FatalAcceptError!ast.Document {
        parser.traceEnterRule(@src());
        defer parser.popTrace();

        var doc = ast.Document{};

        while (true) {
            const decl_or_eof = try parser.acceptTopLevelDecl();

            const decl = decl_or_eof orelse break;

            try parser.append(ast.TopLevelDeclaration, &doc, decl);
        }

        return doc;
    }

    fn acceptTopLevelDecl(parser: *Parser) FatalAcceptError!?ast.TopLevelDeclaration {
        parser.traceEnterRule(@src());
        defer parser.popTrace();

        if (parser.acceptStartDecl()) |root_rule| {
            return .{ .start = root_rule };
        } else |err| try filterAcceptError(err);

        if (parser.acceptRule()) |rule| {
            return .{ .rule = rule };
        } else |err| try filterAcceptError(err);

        // Detect any excess tokens on the top level:
        if (parser.core.nextToken()) |maybe_token| {
            if (maybe_token) |token| {
                try parser.emitDiagnostic(token.location, .unexpected_toplevel_token, .{
                    .actual_type = token.type,
                    .actual_text = token.text,
                });
                return error.SyntaxError;
            } else {
                // This is actually the good path here, as only if we don't find any token or tokenization error,
                // we reached the end of the file.
            }
        } else |err| switch (err) {
            error.UnexpectedCharacter => {
                try parser.emitUnexpectedCharacter(parser.core.tokenizer.current_location, parser.core.tokenizer.offset);
                return error.SyntaxError;
            },
        }

        return null;
    }

    fn acceptStartDecl(parser: *Parser) AcceptError!ast.RuleRef {
        parser.traceEnterRule(@src());
        defer parser.popTrace();

        try parser.acceptLiteral(.start, .recover);
        const init_rule = try parser.acceptRuleReference(.fail);

        try parser.acceptLiteral(.@";", .fail);

        return init_rule;
    }

    fn acceptRule(parser: *Parser) AcceptError!ast.Rule {
        parser.traceEnterRule(@src());
        defer parser.popTrace();

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

    fn acceptMappedProduction(parser: *Parser) AcceptError!ast.MappedProduction {
        parser.traceEnterRule(@src());
        defer parser.popTrace();

        var sequence = try parser.acceptProductionSequence();

        const mapping = if (try parser.tryAcceptLiteral(.@"=>"))
            try parser.acceptAstMapping(.fail)
        else
            null;

        return ast.MappedProduction{
            // Auto-flatten the "tree" here if the top level production is a "sequence" of one
            .production = if (sequence.only()) |item|
                item
            else
                .{ .sequence = sequence },
            .mapping = mapping,
        };
    }

    fn acceptProductionSequence(parser: *Parser) AcceptError!ast.List(ast.Production) {
        parser.traceEnterRule(@src());
        defer parser.popTrace();

        var list: ast.List(ast.Production) = .{};

        while (true) {
            if (parser.acceptProduction()) |prod| {
                try parser.append(ast.Production, &list, prod);
            } else |err| switch (err) {
                error.UnexpectedTokenRecoverable => break,
                error.OutOfMemory, error.InvalidSourceEncoding, error.SyntaxError => |e| return e,
            }
        }

        if (list.len() == 0) {
            // Empty list is a recoverable syntax error:
            try parser.emitDiagnostic(null, .illegal_empty_group, .{});
        }

        return list;
    }

    fn acceptProduction(parser: *Parser) AcceptError!ast.Production {
        parser.traceEnterRule(@src());
        defer parser.popTrace();

        if (try parser.tryAcceptLiteral(.@"(")) {
            var sequence = try parser.acceptProductionSequence();
            try parser.acceptLiteral(.@")", .fail);

            if (try parser.tryAcceptLiteral(.@"?")) {
                return .{ .optional = sequence };
            } else if (try parser.tryAcceptLiteral(.@"+")) {
                return .{ .repetition_one = sequence };
            } else if (try parser.tryAcceptLiteral(.@"*")) {
                return .{ .repetition_zero = sequence };
            } else {
                return .{ .sequence = sequence };
            }
        }

        if (parser.acceptStringLiteral(.recover)) |str| {
            return ast.Production{ .literal = str };
        } else |err| try filterAcceptError(err);

        if (parser.acceptTokenReference(.recover)) |ref| {
            return ast.Production{ .terminal = ref };
        } else |err| try filterAcceptError(err);

        if (parser.acceptRuleReference(.recover)) |ref| {
            return ast.Production{ .recursion = ref };
        } else |err| try filterAcceptError(err);

        // We're done with out list
        return error.UnexpectedTokenRecoverable;
    }

    fn acceptAstMapping(parser: *Parser, accept_mode: AcceptMode) AcceptError!ast.AstMapping {
        parser.traceEnterRule(@src());
        defer parser.popTrace();

        const state = parser.save();
        errdefer parser.restore(state);

        const position = parser.core.tokenizer.current_location;

        if (parser.acceptVariantInit()) |init| {
            return .{ .variant = init };
        } else |err| try filterAcceptError(err);

        if (parser.acceptRecordInit()) |init| {
            return .{ .record = init };
        } else |err| try filterAcceptError(err);

        if (parser.acceptListInit()) |init| {
            return .{ .list = init };
        } else |err| try filterAcceptError(err);

        if (parser.acceptCodeLiteral()) |literal| {
            return .{ .literal = literal };
        } else |err| try filterAcceptError(err);

        if (parser.acceptValueReference()) |literal| {
            return .{ .context_reference = literal };
        } else |err| try filterAcceptError(err);

        if (parser.acceptBuiltinCall()) |call| {
            return .{ .function_call = call };
        } else |err| try filterAcceptError(err);

        if (parser.acceptUserCall()) |call| {
            return .{ .user_function_call = call };
        } else |err| try filterAcceptError(err);

        if (parser.acceptUserReference()) |ref| {
            return .{ .user_reference = ref };
        } else |err| try filterAcceptError(err);

        if (try parser.tryAcceptLiteral(.@";") or try parser.tryAcceptLiteral(.@"|")) {
            try parser.emitDiagnostic(position, .empty_mapping, .{});
            return error.SyntaxError;
        }

        switch (accept_mode) {
            .recover => return error.UnexpectedTokenRecoverable,
            .fail => return parser.emitUnexpectedToken(),
        }
    }

    fn acceptVariantInit(parser: *Parser) AcceptError!ast.VariantInitializer {
        parser.traceEnterRule(@src());
        defer parser.popTrace();

        const state = parser.save();
        errdefer parser.restore(state);

        const field = try parser.acceptIdentifier(.recover);

        try parser.acceptLiteral(.@":", .recover);

        const value = try parser.acceptAstMapping(.fail);

        const clone = try parser.arena.create(ast.AstMapping);
        clone.* = value;

        return .{
            .field = field,
            .value = clone,
        };
    }

    fn acceptRecordInit(parser: *Parser) AcceptError!ast.List(ast.FieldAssignment) {
        parser.traceEnterRule(@src());
        defer parser.popTrace();

        const state = parser.save();
        errdefer parser.restore(state);

        try parser.acceptLiteral(.@"{", .recover);

        var mode: AcceptMode = .recover;

        var list = ast.List(ast.FieldAssignment){};
        while (true) {
            // First item might fail, then it's not a record initializer, but
            // afterwards, all fields must comply
            defer mode = .fail;

            const node = try parser.acceptFieldInit(mode);

            try parser.append(ast.FieldAssignment, &list, node);

            if (!try parser.tryAcceptLiteral(.@",")) {
                break;
            }
        }

        try parser.acceptLiteral(.@"}", .fail);

        return list;
    }

    fn acceptFieldInit(parser: *Parser, mode: AcceptMode) AcceptError!ast.FieldAssignment {
        parser.traceEnterRule(@src());
        defer parser.popTrace();

        const state = parser.save();
        errdefer parser.restore(state);

        const location = parser.core.tokenizer.current_location;

        const field = try parser.acceptIdentifier(mode);

        try parser.acceptLiteral(.@"=", .fail);

        const value = try parser.acceptAstMapping(.fail);

        const clone = try parser.arena.create(ast.AstMapping);
        clone.* = value;

        return .{
            .location = location,
            .field = field,
            .value = clone,
        };
    }

    fn acceptListInit(parser: *Parser) AcceptError!ast.List(ast.AstMapping) {
        parser.traceEnterRule(@src());
        defer parser.popTrace();

        const state = parser.save();
        errdefer parser.restore(state);

        try parser.acceptLiteral(.@"{", .recover);

        var items = try parser.acceptMappingList();

        try parser.acceptLiteral(.@"}", .fail);

        return items;
    }

    fn acceptCodeLiteral(parser: *Parser) AcceptError!ast.CodeLiteral {
        parser.traceEnterRule(@src());
        defer parser.popTrace();

        const token = try parser.acceptToken(.code_literal, .recover);

        std.debug.assert(std.mem.startsWith(u8, token.text, "`"));
        std.debug.assert(std.mem.endsWith(u8, token.text, "`"));

        var prefix_len: usize = 0;
        while (token.text[prefix_len] == '`') {
            prefix_len += 1;
        }

        return ast.CodeLiteral{
            .location = token.location,
            .value = try parser.pool.insert(token.text[prefix_len .. token.text.len - prefix_len]),
        };
    }

    fn acceptValueReference(parser: *Parser) AcceptError!ast.ValueRef {
        parser.traceEnterRule(@src());
        defer parser.popTrace();

        const token = try parser.acceptToken(.value_ref, .recover);
        std.debug.assert(std.mem.startsWith(u8, token.text, "$"));
        return ast.ValueRef{
            .location = token.location,
            .index = std.fmt.parseInt(u32, token.text[1..], 10) catch |err| switch (err) {
                error.InvalidCharacter => unreachable, // ensured by tokenizer,
                error.Overflow => blk: {
                    try parser.emitDiagnostic(token.location, .integer_overflow, .{
                        .min = comptime std.fmt.comptimePrint("{}", .{std.math.minInt(u32)}),
                        .max = comptime std.fmt.comptimePrint("{}", .{std.math.maxInt(u32)}),
                        .actual = token.text[1..],
                    });
                    break :blk 0;
                },
            },
        };
    }

    fn acceptBuiltinCall(parser: *Parser) AcceptError!ast.FunctionCall(ast.Identifier) {
        parser.traceEnterRule(@src());
        defer parser.popTrace();

        const state = parser.save();
        errdefer parser.restore(state);

        const id = try parser.acceptIdentifier(.recover);

        try parser.acceptLiteral(.@"(", .fail); // a builtin function is the only legal way to use an identifier here, so we fail unrecoverably

        const list = try parser.acceptMappingList();

        try parser.acceptLiteral(.@")", .fail);

        return .{
            .function = id,
            .arguments = list,
        };
    }

    fn acceptUserCall(parser: *Parser) AcceptError!ast.FunctionCall(ast.UserDefinedIdentifier) {
        parser.traceEnterRule(@src());
        defer parser.popTrace();

        const state = parser.save();
        errdefer parser.restore(state);

        const id = try parser.acceptUserReference();

        // If we only accept a user value, fail and fall back to regular user value acceptance later
        try parser.acceptLiteral(.@"(", .recover);

        const list = try parser.acceptMappingList();

        try parser.acceptLiteral(.@")", .fail);

        return .{
            .function = id,
            .arguments = list,
        };
    }

    fn acceptUserReference(parser: *Parser) AcceptError!ast.UserDefinedIdentifier {
        parser.traceEnterRule(@src());
        defer parser.popTrace();

        const token = try parser.acceptToken(.userval_ref, .recover);
        std.debug.assert(std.mem.startsWith(u8, token.text, "@"));
        return ast.UserDefinedIdentifier{
            .location = token.location,
            .value = try parser.pool.insert(token.text[1..]),
        };
    }

    fn acceptMappingList(parser: *Parser) AcceptError!ast.List(ast.AstMapping) {
        parser.traceEnterRule(@src());
        defer parser.popTrace();

        const list_state = parser.save();
        errdefer parser.restore(list_state);

        var list = ast.List(ast.AstMapping){};

        var accept_mode: AcceptMode = .recover;
        while (true) {
            // first item is allowed to be failing, otherwise comma separation must be done!
            defer accept_mode = .fail;

            const item_state = parser.save();

            if (parser.acceptAstMapping(accept_mode)) |mapping| {
                try parser.append(ast.AstMapping, &list, mapping);
            } else |err| {
                try filterAcceptError(err);
                parser.restore(item_state); // rollback to the previous item
                break;
            }

            if (!try parser.tryAcceptLiteral(.@",")) {
                break;
            }
        }

        return list;
    }

    fn acceptTypeSpec(parser: *Parser) AcceptError!ast.TypeSpec {
        parser.traceEnterRule(@src());
        defer parser.popTrace();

        if (parser.acceptCodeLiteral()) |code| {
            return .{ .literal = code };
        } else |err| try filterAcceptError(err);

        if (parser.acceptUserReference()) |ref| {
            return .{ .custom = ref };
        } else |err| try filterAcceptError(err);

        if (parser.acceptNodeReference(.fail)) |ref| {
            return .{ .reference = ref };
        } else |err| try filterAcceptError(err);

        @panic("not implemented yet");
    }

    fn acceptStringLiteral(parser: *Parser, accept_mode: AcceptMode) AcceptError!ast.StringLiteral {
        parser.traceEnterRule(@src());
        defer parser.popTrace();

        const token = try parser.acceptToken(.string_literal, accept_mode);

        std.debug.assert(token.text.len >= 2);

        return ast.StringLiteral{
            .location = token.location,
            .value = try parser.unwrapString(token.location, token.text[1 .. token.text.len - 1]),
        };
    }

    fn acceptIdentifier(parser: *Parser, accept_mode: AcceptMode) AcceptError!ast.Identifier {
        parser.traceEnterRule(@src());
        defer parser.popTrace();

        const token = try parser.acceptToken(.identifier, accept_mode);
        return ast.Identifier{
            .location = token.location,
            .value = try parser.unwrapIdentifierString(token.location, token.text),
        };
    }

    fn acceptRuleReference(parser: *Parser, accept_mode: AcceptMode) AcceptError!ast.RuleRef {
        parser.traceEnterRule(@src());
        defer parser.popTrace();

        const token = try parser.acceptToken(.rule_ref, accept_mode);
        std.debug.assert(std.mem.startsWith(u8, token.text, "<"));
        std.debug.assert(std.mem.endsWith(u8, token.text, ">"));
        return ast.RuleRef{
            .location = token.location,
            .identifier = try parser.unwrapIdentifierString(token.location, token.text[1 .. token.text.len - 1]),
        };
    }

    fn acceptTokenReference(parser: *Parser, accept_mode: AcceptMode) AcceptError!ast.TokenRef {
        parser.traceEnterRule(@src());
        defer parser.popTrace();

        const token = try parser.acceptToken(.token_ref, accept_mode);
        std.debug.assert(std.mem.startsWith(u8, token.text, "$"));
        return ast.TokenRef{
            .location = token.location,
            .identifier = try parser.unwrapIdentifierString(token.location, token.text[1..]),
        };
    }

    fn acceptNodeReference(parser: *Parser, accept_mode: AcceptMode) AcceptError!ast.NodeRef {
        parser.traceEnterRule(@src());
        defer parser.popTrace();

        const token = try parser.acceptToken(.node_ref, accept_mode);
        std.debug.assert(std.mem.startsWith(u8, token.text, "!"));
        return ast.NodeRef{
            .location = token.location,
            .identifier = try parser.unwrapIdentifierString(token.location, token.text[1..]),
        };
    }

    fn acceptLiteral(parser: *Parser, comptime token_type: TokenType, accept_mode: AcceptMode) AcceptError!void {
        _ = try parser.acceptToken(token_type, accept_mode);
    }

    fn tryAcceptLiteral(parser: *Parser, comptime token_type: TokenType) FatalAcceptError!bool {
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
            errdefer parser.emitTrace(.{ .token_reject = .{ .actual = token, .expected = token_type } });
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
            parser.emitTrace(.{ .token_accept = token });
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
    const TraceKind = union(enum) {
        token_accept: Token,
        token_reject: struct { actual: Token, expected: TokenType },
        rule: []const u8,
    };

    const Trace = struct {
        depth: u32,
        kind: TraceKind,

        pub fn format(trace: Trace, fmt: []const u8, opt: std.fmt.FormatOptions, writer: anytype) !void {
            _ = fmt;
            _ = opt;
            try writer.writeByteNTimes(' ', 4 * trace.depth);
            try writer.print("{s}:", .{@tagName(trace.kind)});
            switch (trace.kind) {
                .token_accept => |item| try writer.print("accept {}", .{item}),
                .token_reject => |item| try writer.print("reject {}, expected '{s}'", .{ item.actual, @tagName(item.expected) }),
                .rule => |item| try writer.print("{s}", .{item}),
            }
        }
    };

    fn traceEnterRule(parser: *Parser, loc: std.builtin.SourceLocation) void {
        parser.emitTrace(.{ .rule = loc.fn_name });
        parser.trace_depth += 1;
    }

    fn popTrace(parser: *Parser) void {
        parser.trace_depth -= 1;
    }

    fn emitTrace(parser: Parser, trace: TraceKind) void {
        if (!parser.trace_enabled) {
            return;
        }
        std.log.debug("rule trace: {}", .{Trace{
            .depth = parser.trace_depth,
            .kind = trace,
        }});
    }

    fn emitDiagnostic(parser: Parser, loc: ?ptk.Location, comptime code: Diagnostics.Code, data: Diagnostics.Data(code)) !void {
        // Anything detected here is always an error
        std.debug.assert(code.isError());
        try parser.diagnostics.emit(loc orelse parser.core.tokenizer.current_location, code, data);
    }

    fn emitUnexpectedToken(parser: *Parser) AcceptError {
        const state = parser.save();
        defer parser.restore(state);

        const location = parser.core.tokenizer.current_location;
        const offset = parser.core.tokenizer.offset;

        const token_or_null = parser.core.nextToken() catch |err| switch (err) {
            error.UnexpectedCharacter => {
                try parser.emitUnexpectedCharacter(location, offset);
                return error.SyntaxError;
            },
        };

        const token = token_or_null orelse {
            try parser.emitDiagnostic(location, .unexpected_eof, .{});
            return error.SyntaxError;
        };

        try parser.emitDiagnostic(location, .unexpected_token_no_context, .{
            .actual_type = token.type,
        });
        return error.SyntaxError;
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
    Pattern.create(.record, match.word("record")),
    Pattern.create(.variant, match.word("variant")),
    Pattern.create(.optional, match.word("optional")),
    Pattern.create(.start, match.word("start")),
    Pattern.create(.rule, match.word("rule")),
    Pattern.create(.token, match.word("token")),
    Pattern.create(.custom, match.word("custom")),
    Pattern.create(.regex, match.word("regex")),
    Pattern.create(.skip, match.word("skip")),

    Pattern.create(.string_literal, matchStringLiteral),
    Pattern.create(.code_literal, matchCodeLiteral),

    // identifiers must come after keywords:
    Pattern.create(.identifier, matchRawIdentifier),
    Pattern.create(.node_ref, matchNodeRef),
    Pattern.create(.rule_ref, matchRuleRef),
    Pattern.create(.token_ref, matchTokenRef),
    Pattern.create(.value_ref, matchValueRef),
    Pattern.create(.userval_ref, matchBuiltinRef),

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
