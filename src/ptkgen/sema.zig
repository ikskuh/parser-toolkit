const std = @import("std");
const ptk = @import("parser-toolkit");

const ast = @import("ast.zig");
const Diagnostics = @import("Diagnostics.zig");

pub const AnalyzeError = error{ OutOfMemory, SemanticError };

const String = ptk.strings.String;

pub fn StringHashMap(comptime T: type) type {
    return std.AutoArrayHashMap(String, T);
}

pub const Grammar = struct {
    arena: std.heap.ArenaAllocator,

    start: ?StartDeclaration,

    rules: StringHashMap(*Rule),
    nodes: StringHashMap(*Node),
    patterns: StringHashMap(*Pattern),

    pub fn deinit(grammar: *Grammar) void {
        grammar.rules.deinit();
        grammar.nodes.deinit();
        grammar.patterns.deinit();
        grammar.arena.deinit();
        grammar.* = undefined;
    }
};

pub const StartDeclaration = struct {
    rule: *Rule,
    location: ptk.Location,
};

pub const Rule = struct {
    location: ptk.Location,
    name: String,

    type: ?*Type,
    production: *Production,
};

pub const Production = union(enum) {
    terminal: *Pattern, // literal and terminal ast nodes are wrapped to this
    recursion: *Rule, // <rule>
    sequence: []Production, // ...
    optional: *Production, // ( ... )?
    repetition_zero: *Production, // [ ... ]*
    repetition_one: *Production, // [ ... ]+
};

pub const Node = struct {
    location: ptk.Location,
    name: String,

    type: *Type,
};

pub const Pattern = struct {
    location: ptk.Location,
    name: String,

    data: Data,

    pub const Data = union(enum) {
        literal_match: String,
        word: String,
        regex: String,
        external: String,
    };
};

pub const Type = union(enum) {
    code_literal: String,
    user_value: String,

    optional: *Type,
    record: *CompoundType,
    variant: *CompoundType,
};

pub const CompoundType = struct {
    fields: StringHashMap(Field),
};

pub const Field = struct {
    name: String,
    type: *Type,
};

pub fn analyze(allocator: std.mem.Allocator, diagnostics: *Diagnostics, strings: *const ptk.strings.Pool, document: ast.Document) AnalyzeError!Grammar {
    std.debug.assert(diagnostics.hasErrors() == false);
    errdefer |err| if (err == error.SemanticError)
        std.debug.assert(diagnostics.hasErrors());

    var grammar = Grammar{
        .arena = std.heap.ArenaAllocator.init(allocator),

        .rules = StringHashMap(*Rule).init(allocator),
        .nodes = StringHashMap(*Node).init(allocator),
        .patterns = StringHashMap(*Pattern).init(allocator),

        .start = null,
    };
    errdefer grammar.deinit();

    var analyzer = Analyzer{
        .arena = grammar.arena.allocator(),
        .diagnostics = diagnostics,
        .strings = strings,

        .rule_to_ast = std.AutoHashMap(*Rule, *ast.Rule).init(allocator),
        .node_to_ast = std.AutoHashMap(*Node, *ast.Node).init(allocator),
        .pattern_to_ast = std.AutoHashMap(*Pattern, *ast.Pattern).init(allocator),

        .document = document,

        .target = &grammar,
    };
    defer analyzer.deinit();

    try innerAnalysis(&analyzer);

    if (grammar.start == null) {
        try analyzer.emitDiagnostic(ptk.Location{
            .line = 0,
            .column = 0,
            .source = null,
        }, .missing_start_symbol, .{});
    }

    return grammar;
}

fn innerAnalysis(analyzer: *Analyzer) AnalyzeError!void {
    // Phase 0: Validate productions on legality (coarse error checking)
    // - Generates errors for badly constructed elements
    try analyzer.validateAstRulesCoarse();

    // Phase 1: Create all global declarations
    // - Populates the declaration lookups
    // - Generates errors for duplicate identifiers
    try analyzer.createDeclarations();

    // Phase 2: Instantiate all node types and patterns, determine start symbol

    try analyzer.iterateOn(.start, Analyzer.instantiateStartSymbol);
    try analyzer.iterateOn(.node, Analyzer.instantiatePatterns);
    try analyzer.iterateOn(.node, Analyzer.instantiateNodeTypes);

    // Phase 3: Validate generated types

    // Phase 4: Instantiate AST productions

    // Phase 5: Instantiate and validate AST mappings

}

const Analyzer = struct {
    arena: std.mem.Allocator,
    diagnostics: *Diagnostics,
    strings: *const ptk.strings.Pool,
    target: *Grammar,

    document: ast.Document,

    rule_to_ast: std.AutoHashMap(*Rule, *ast.Rule),
    node_to_ast: std.AutoHashMap(*Node, *ast.Node),
    pattern_to_ast: std.AutoHashMap(*Pattern, *ast.Pattern),

    fn deinit(analyzer: *Analyzer) void {
        analyzer.rule_to_ast.deinit();
        analyzer.node_to_ast.deinit();
        analyzer.pattern_to_ast.deinit();
        analyzer.* = undefined;
    }

    const IterativeAnalysisError = error{RecoverableSemanticError} || AnalyzeError;

    fn iterateOn(
        analyzer: *Analyzer,
        comptime node_type: std.meta.FieldEnum(ast.TopLevelDeclaration),
        comptime functor: fn (*Analyzer, *std.meta.FieldType(ast.TopLevelDeclaration, node_type)) IterativeAnalysisError!void,
    ) AnalyzeError!void {
        var iter = ast.iterate(analyzer.document);
        while (iter.next()) |item| {
            switch (item.*) {
                @field(std.meta.Tag(ast.TopLevelDeclaration), @tagName(node_type)) => |*node| {
                    functor(analyzer, node) catch |err| switch (err) {
                        error.RecoverableSemanticError => {},
                        else => |e| return e,
                    };
                },
                else => {},
            }
        }
    }

    fn validateAstRulesCoarse(analyzer: *Analyzer) !void {
        var iter = ast.iterate(analyzer.document);
        while (iter.next()) |item| {
            switch (item.*) {
                .start => |start| {
                    _ = start;
                },

                .rule => |rule| {
                    _ = rule;
                },

                .node => |node| {
                    _ = node;
                },

                .pattern => |pattern| {
                    _ = pattern;
                },
            }
        }
    }

    fn createDeclarations(analyzer: *Analyzer) !void {
        var iter = ast.iterate(analyzer.document);
        while (iter.next()) |item| {
            switch (item.*) {
                .start => {},

                .rule => |*rule| {
                    const instance = try analyzer.declareElement(
                        Rule,
                        ast.Rule,
                        &analyzer.target.rules,
                        &analyzer.rule_to_ast,
                        rule,
                        rule.name,
                        .duplicate_identifier_rule,
                    );
                    instance.* = .{
                        .location = rule.name.location,
                        .name = rule.name.value,

                        .type = undefined, // created in phase 4
                        .production = undefined, // created in phase 5
                    };
                },

                .node => |*node| {
                    const instance = try analyzer.declareElement(
                        Node,
                        ast.Node,
                        &analyzer.target.nodes,
                        &analyzer.node_to_ast,
                        node,
                        node.name,
                        .duplicate_identifier_node,
                    );
                    instance.* = .{
                        .location = node.name.location,
                        .name = node.name.value,

                        .type = undefined, // created in phase 2
                    };
                },

                .pattern => |*pattern| {
                    const instance = try analyzer.declareElement(
                        Pattern,
                        ast.Pattern,
                        &analyzer.target.patterns,
                        &analyzer.pattern_to_ast,
                        pattern,
                        pattern.name,
                        .duplicate_identifier_pattern,
                    );
                    instance.* = .{
                        .location = pattern.name.location,
                        .name = pattern.name.value,

                        .data = undefined, // created in phase 2
                    };
                },
            }
        }
    }

    fn instantiateStartSymbol(analyzer: *Analyzer, start: *ast.RuleRef) !void {
        if (analyzer.target.start) |old_start| {
            try analyzer.emitDiagnostic(start.location, .multiple_start_symbols, .{
                .identifier = analyzer.strings.get(old_start.rule.name),
                .previous_location = old_start.location,
            });
            // error return is further down below so we can also catch the undefined reference error
        }

        const rule = analyzer.target.rules.get(start.identifier) orelse {
            try analyzer.emitDiagnostic(start.location, .reference_to_undeclared_rule, .{
                .identifier = analyzer.strings.get(start.identifier),
            });
            return error.RecoverableSemanticError;
        };

        if (analyzer.target.start != null) {
            // return for the first if block
            return error.RecoverableSemanticError;
        }

        analyzer.target.start = .{
            .rule = rule,
            .location = start.location,
        };
    }

    fn instantiatePatterns(analyzer: *Analyzer, node: *ast.Node) !void {
        _ = analyzer;
        _ = node;
        //
    }

    fn instantiateNodeTypes(analyzer: *Analyzer, node: *ast.Node) !void {
        _ = analyzer;
        _ = node;
        //
    }

    const DeclarationError = error{
        OutOfMemory,
        SemanticError,
    };
    fn declareElement(
        analyzer: *Analyzer,
        comptime Element: type,
        comptime AstNode: type,
        set: *StringHashMap(*Element),
        ast_map: *std.AutoHashMap(*Element, *AstNode),
        ast_node: *AstNode,
        name: ast.Identifier,
        comptime diagnostic: Diagnostics.Code,
    ) DeclarationError!*Element {
        const gop = try set.getOrPut(name.value);
        if (gop.found_existing) {
            // emit diagnostic here
            try analyzer.emitDiagnostic(name.location, diagnostic, .{
                .identifier = analyzer.strings.get(name.value),
                .previous_location = gop.value_ptr.*.*.location,
            });
            return error.SemanticError;
        }
        errdefer _ = set.swapRemove(name.value);

        const item = try analyzer.arena.create(Element);
        errdefer analyzer.arena.destroy(item);

        item.* = undefined;

        gop.value_ptr.* = item;

        try ast_map.putNoClobber(item, ast_node);

        return item;
    }

    fn emitDiagnostic(analyzer: *Analyzer, location: ptk.Location, comptime code: Diagnostics.Code, params: Diagnostics.Data(code)) !void {
        try analyzer.diagnostics.emit(location, code, params);
    }
};
