const std = @import("std");
const ptk = @import("parser-toolkit");

const logger = std.log.scoped(.ptk_sema);

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
    literal_patterns: StringHashMap(*Pattern),

    pub fn deinit(grammar: *Grammar) void {
        grammar.rules.deinit();
        grammar.nodes.deinit();
        grammar.patterns.deinit();
        grammar.literal_patterns.deinit();
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
    productions: []MappedProduction,
};

/// A production of a rule that is able to map the parsed structure
/// into an AST node.
pub const MappedProduction = struct {
    production: Production,
    mapping: ?Mapping,
};

pub const Production = union(enum) {
    terminal: *Pattern, // literal and terminal ast nodes are wrapped to this
    recursion: *Rule, // <rule>
    sequence: []Production, // ...
    optional: *Production, // ( ... )?
    repetition_zero: *Production, // [ ... ]*
    repetition_one: *Production, // [ ... ]+
};

pub const Mapping = struct {
    //
};

pub const Node = struct {
    location: ptk.Location,
    name: String,

    type: *Type,
};

pub const Pattern = struct {
    location: ptk.Location,
    name: String,
    is_literal: bool,
    data: Data,

    pub const Data = union(enum) {
        literal_match: String,
        word: String,
        regex: String,
        external: String,
    };
};

pub const Type = union(enum) {
    // trivial types:
    code_literal: String,
    user_type: String,

    // anonymous compound types:
    optional: *Type,
    record: *CompoundType,
    variant: *CompoundType,

    // ast nodes are basically "named types" and must be handled as such
    named: *Node,

    pub fn id(t: *const Type) TypeId {
        return @as(TypeId, t.*);
    }
};

pub const TypeId: type = std.meta.Tag(Type);

pub const CompoundType = struct {
    fields: StringHashMap(Field),
};

pub const Field = struct {
    location: ptk.Location,
    // name: String,
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
        .literal_patterns = StringHashMap(*Pattern).init(allocator),

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

        .type_stash = Analyzer.TypeStash.init(allocator),

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

var BAD_TYPE_SENTINEL: Type = undefined;
var BAD_NODE_SENTINEL: Node = undefined;
var BAD_RULE_SENTINEL: Rule = undefined;
var BAD_PATTERN_SENTINEL: Pattern = undefined;

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
    try analyzer.iterateOn(.pattern, Analyzer.instantiatePatterns);
    try analyzer.iterateOn(.node, Analyzer.instantiateNodeTypes);

    // Phase 3: Validate generated types
    try analyzer.iterateOn(.node, Analyzer.validateNodes);

    // Phase 4: Instantiate AST productions
    try analyzer.iterateOn(.rule, Analyzer.instantiateRules);

    // Phase 5: Instantiate and validate AST mappings

}

const Analyzer = struct {
    const TypeStash = std.HashMap(*Type, void, TypeContext, std.hash_map.default_max_load_percentage);

    arena: std.mem.Allocator,
    diagnostics: *Diagnostics,
    strings: *const ptk.strings.Pool,
    target: *Grammar,

    document: ast.Document,

    rule_to_ast: std.AutoHashMap(*Rule, *ast.Rule),
    node_to_ast: std.AutoHashMap(*Node, *ast.Node),
    pattern_to_ast: std.AutoHashMap(*Pattern, *ast.Pattern),

    type_stash: TypeStash,

    deduplicated_type_count: usize = 0,

    fn deinit(analyzer: *Analyzer) void {
        analyzer.rule_to_ast.deinit();
        analyzer.node_to_ast.deinit();
        analyzer.pattern_to_ast.deinit();
        analyzer.type_stash.deinit();
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

    /// Creates declarations in the target Grammar and makes sure all declared objects are reachable.
    /// Emits diagnostics for duplicate declarations.
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
                        .productions = &.{}, // created in phase 5
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
                        .is_literal = false,
                        .data = undefined, // created in phase 2
                    };
                },
            }
        }
    }

    /// Searches all start symbol declarations and stores a reference to the initial rule.
    /// Will emit diagnostics for duplicate start symbol decls and invalid references.
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

    /// Fully populate all content of the pattern declarations. Emits diagnostics for invalid patterns.
    fn instantiatePatterns(analyzer: *Analyzer, ast_pattern: *ast.Pattern) !void {
        const sema_pattern = analyzer.target.patterns.get(ast_pattern.name.value).?;

        sema_pattern.data = switch (ast_pattern.data) {
            .literal => |value| .{ .literal_match = value.value },
            .word => |value| .{ .word = value.value },
            .regex => |value| .{ .regex = value.value },
            .external => |value| .{ .external = value.value },
        };

        // TODO: Implement regex validation here!
    }

    /// Instantiates and validates all node declarations.
    /// Emits diagnostics for bad type declarations.
    fn instantiateNodeTypes(analyzer: *Analyzer, ast_node: *ast.Node) !void {
        const sema_node = analyzer.target.nodes.get(ast_node.name.value).?;

        sema_node.type = try analyzer.resolveType(&ast_node.value);
    }

    fn validateNodes(analyzer: *Analyzer, ast_node: *ast.Node) !void {
        const sema_node = analyzer.target.nodes.get(ast_node.name.value).?;

        try analyzer.validateType(sema_node.type);
    }

    fn instantiateRules(analyzer: *Analyzer, ast_rule: *ast.Rule) !void {
        const sema_rule = analyzer.target.rules.get(ast_rule.name.value).?;

        sema_rule.type = if (ast_rule.ast_type) |ast_type|
            try analyzer.resolveType(&ast_type)
        else
            null;

        sema_rule.productions = try analyzer.target.arena.allocator().alloc(MappedProduction, ast_rule.productions.len());
        errdefer {
            analyzer.target.arena.allocator().free(sema_rule.productions);
            sema_rule.productions = &.{};
        }

        if (sema_rule.productions.len == 0) {
            @panic("empty sema rule!");
        }

        var iter = ast.iterate(ast_rule.productions);
        var index: usize = 0;
        while (iter.next()) |ast_production| : (index += 1) {
            const sema_production = &sema_rule.productions[index];

            sema_production.* = MappedProduction{
                .production = try analyzer.translateProduction(ast_production.production),
                .mapping = null, // Will be instantiated later
            };
        }
    }

    fn translateProduction(analyzer: *Analyzer, ast_prod: ast.Production) error{OutOfMemory}!Production {
        switch (ast_prod) {
            .literal => |literal| {
                const gop = try analyzer.target.literal_patterns.getOrPut(literal.value);
                if (!gop.found_existing) {
                    gop.value_ptr.* = try analyzer.target.arena.allocator().create(Pattern);
                    gop.value_ptr.*.* = .{
                        .location = literal.location, // place of first use
                        .name = literal.value,
                        .data = .{ .literal_match = literal.value },
                        .is_literal = true,
                    };
                }
                return Production{ .terminal = gop.value_ptr.* };
            },
            .terminal => |terminal| {
                if (analyzer.target.patterns.get(terminal.identifier)) |pattern| {
                    return Production{ .terminal = pattern };
                } else {
                    try analyzer.emitDiagnostic(terminal.location, .reference_to_undeclared_pattern, .{
                        .identifier = analyzer.strings.get(terminal.identifier),
                    });
                    return Production{ .terminal = &BAD_PATTERN_SENTINEL };
                }
            },
            .recursion => |recursion| {
                if (analyzer.target.rules.get(recursion.identifier)) |rule| {
                    return Production{ .recursion = rule };
                } else {
                    try analyzer.emitDiagnostic(recursion.location, .reference_to_undeclared_rule, .{
                        .identifier = analyzer.strings.get(recursion.identifier),
                    });
                    return Production{ .recursion = &BAD_RULE_SENTINEL };
                }
            },
            .sequence => |sequence| {
                if (sequence.len() == 0)
                    @panic("bad sequence: empty");

                var seq = std.ArrayList(Production).init(analyzer.target.arena.allocator());
                defer seq.deinit();

                try seq.ensureTotalCapacityPrecise(sequence.len());

                var iter = ast.iterate(sequence);
                while (iter.next()) |inner_prod| {
                    const inner_sema = try analyzer.translateProduction(inner_prod.*);
                    seq.appendAssumeCapacity(inner_sema);
                }

                return Production{
                    .sequence = seq.toOwnedSlice() catch @panic("bad capacity"),
                };
            },
            .optional => |optional| {
                const nested = try analyzer.target.arena.allocator().create(Production);
                errdefer analyzer.target.arena.allocator().destroy(nested);
                nested.* = try analyzer.translateProduction(.{ .sequence = optional });
                return .{ .optional = nested };
            },
            .repetition_zero => |repetition| {
                const nested = try analyzer.target.arena.allocator().create(Production);
                errdefer analyzer.target.arena.allocator().destroy(nested);
                nested.* = try analyzer.translateProduction(.{ .sequence = repetition });
                return .{ .repetition_zero = nested };
            },
            .repetition_one => |repetition| {
                const nested = try analyzer.target.arena.allocator().create(Production);
                errdefer analyzer.target.arena.allocator().destroy(nested);
                nested.* = try analyzer.translateProduction(.{ .sequence = repetition });
                return .{ .repetition_one = nested };
            },
        }
    }

    /// Checks if the given type is semantically ok or emits compiler errors if not.
    fn validateType(analyzer: *Analyzer, type_node: *Type) error{OutOfMemory}!void {
        if (type_node == &BAD_TYPE_SENTINEL) {
            @panic("bad sentinel");
        }

        switch (type_node.*) {
            .code_literal, .user_type => {}, // always fine
            .optional => |child_type| try analyzer.validateType(child_type),
            .record, .variant => |compound_type| {
                var fields = compound_type.fields.iterator();
                while (fields.next()) |kv| {
                    const field_type = kv.value_ptr.type;
                    try analyzer.validateType(field_type);
                }
            },
            .named => |node| {
                if (node == &BAD_NODE_SENTINEL) {
                    @panic("bad node!");
                }
            },
        }
    }

    /// Constructs a new compound type from the given AST declaration. Will emit diagnostics
    /// on error and returns an incomplete type if errors happened.
    fn createCompoundType(analyzer: *Analyzer, def: ast.CompoundType) !*CompoundType {
        const ct = try analyzer.target.arena.allocator().create(CompoundType);
        errdefer analyzer.target.arena.allocator().destroy(ct);

        ct.* = CompoundType{
            .fields = StringHashMap(Field).init(analyzer.target.arena.allocator()),
        };
        errdefer ct.fields.deinit();

        try ct.fields.ensureTotalCapacity(def.fields.len());

        var iter = ast.iterate(def.fields);
        while (iter.next()) |field_def| {
            const field_type = try analyzer.resolveType(&field_def.type);
            const gop_result = ct.fields.getOrPutAssumeCapacity(field_def.name.value);

            if (gop_result.found_existing) {
                try analyzer.emitDiagnostic(field_def.location, .duplicate_compound_field, .{
                    .previous_location = gop_result.value_ptr.location,
                    .identifier = analyzer.strings.get(field_def.name.value),
                });
                continue;
            }

            gop_result.value_ptr.* = .{
                .type = field_type,
                .location = field_def.location,
            };
        }

        return ct;
    }

    fn destroyCompoundType(analyzer: *Analyzer, ct: *CompoundType) void {
        ct.fields.deinit();
        analyzer.target.arena.allocator().destroy(ct);
        ct.* = undefined;
    }

    fn resolveType(analyzer: *Analyzer, type_node: *const ast.TypeSpec) error{OutOfMemory}!*Type {
        var compound_type: ?*CompoundType = null;
        var proto_type: Type = switch (type_node.*) {
            .reference => |def| .{
                .named = analyzer.target.nodes.get(def.identifier) orelse blk: {
                    try analyzer.emitDiagnostic(def.location, .reference_to_undeclared_node, .{
                        .identifier = analyzer.strings.get(def.identifier),
                    });
                    break :blk &BAD_NODE_SENTINEL;
                },
            },
            .literal => |def| Type{ .code_literal = def.value },
            .custom => |def| Type{ .user_type = def.value },
            .record => |def| blk: {
                compound_type = try analyzer.createCompoundType(def);
                break :blk .{ .record = compound_type.? };
            },
            .variant => |def| blk: {
                compound_type = try analyzer.createCompoundType(def);
                break :blk .{ .record = compound_type.? };
            },
        };
        errdefer if (compound_type) |ct|
            analyzer.destroyCompoundType(ct);

        if (analyzer.getUniqueTypeHandle(&proto_type)) |resolved_type| {
            analyzer.deduplicated_type_count += 1;
            // logger.debug("deduplicated a {s}", .{@tagName(resolved_type.*)});
            return resolved_type;
        }

        const new_type = try analyzer.target.arena.allocator().create(Type);
        errdefer analyzer.target.arena.allocator().destroy(new_type);

        new_type.* = proto_type;

        try analyzer.type_stash.putNoClobber(new_type, {});

        return new_type;
    }

    fn getUniqueTypeHandle(analyzer: Analyzer, proto_type: *Type) ?*Type {
        if (analyzer.type_stash.getKey(proto_type)) |key| {
            return key;
        }
        return null;
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

const TypeContext = struct {
    const HashFn = std.hash.Fnv1a_64;

    pub fn eql(ctx: TypeContext, lhs: *Type, rhs: *Type) bool {
        _ = ctx;
        if (lhs == rhs)
            return true;
        if (lhs.id() != rhs.id())
            return false;
        switch (lhs.*) {
            inline .code_literal, .user_type, .optional, .named => |val, tag| return val == @field(rhs, @tagName(tag)),
            .record, .variant => return false, // they are same-by-identitiy
        }
    }

    pub fn hash(ctx: TypeContext, t: *Type) u64 {
        _ = ctx;
        var hasher = HashFn.init();
        hasher.update(@tagName(t.*));
        switch (t.*) {
            .code_literal => |lit| hasher.update(&std.mem.toBytes(@intFromEnum(lit))),
            .user_type => |lit| hasher.update(&std.mem.toBytes(@intFromEnum(lit))),
            .optional => |child| hasher.update(&std.mem.toBytes(child)),
            .named => |node| hasher.update(&std.mem.toBytes(node)),
            .record, .variant => hasher.update(&std.mem.toBytes(t)),
        }
        return hasher.final();
    }
};
