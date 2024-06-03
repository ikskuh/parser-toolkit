const std = @import("std");
const ptk = @import("parser-toolkit");

const sema = @import("../sema.zig");
const parser = @import("../parser.zig");

pub fn createJsonValue(
    arena: *std.heap.ArenaAllocator,
    strings: *const ptk.strings.Pool,
    grammar: sema.Grammar,
) !std.json.Value {
    const allocator = arena.allocator();

    var mapper = JsonMapper{
        .allocator = allocator,
        .strings = strings,
    };

    var root = std.json.ObjectMap.init(allocator);
    errdefer root.deinit();

    if (grammar.start) |start| {
        try root.put("start", mapper.jsonString(start.rule.name));
    } else {
        try root.put("start", .null);
    }

    {
        var list = mapper.newArray();
        errdefer list.deinit();

        var iter = grammar.literal_patterns.iterator();
        while (iter.next()) |kvp| {
            try list.append(mapper.jsonString(kvp.value_ptr.*.data.literal_match));
        }

        try root.put("literal_patterns", .{ .array = list });
    }

    {
        var patterns = std.json.ObjectMap.init(allocator);
        errdefer patterns.deinit();

        var iter = grammar.patterns.iterator();
        while (iter.next()) |kvp| {
            const spattern: *sema.Pattern = kvp.value_ptr.*;

            var jpattern = std.json.ObjectMap.init(allocator);
            errdefer jpattern.deinit();

            // try jpattern.put("name", .{ .string = strings.get(spattern.name) });
            try jpattern.put("kind", .{ .string = @tagName(spattern.data) });
            switch (spattern.data) {
                inline else => |val| try jpattern.put("data", mapper.jsonString(val)),
            }

            try patterns.putNoClobber(
                strings.get(kvp.key_ptr.*),
                .{ .object = jpattern },
            );
        }

        try root.put("patterns", .{ .object = patterns });
    }

    {
        var nodes = std.json.ObjectMap.init(allocator);
        errdefer nodes.deinit();

        var iter = grammar.nodes.iterator();
        while (iter.next()) |kvp| {
            const snode: *sema.Node = kvp.value_ptr.*;

            var jtype = try mapper.convertType(snode.type);

            try nodes.putNoClobber(
                strings.get(kvp.key_ptr.*),
                jtype,
            );
        }

        try root.put("ast_nodes", .{ .object = nodes });
    }

    {
        var rules = std.json.ObjectMap.init(allocator);
        errdefer rules.deinit();

        var iter = grammar.rules.iterator();
        while (iter.next()) |kvp| {
            const srule: *sema.Rule = kvp.value_ptr.*;

            var jrule = mapper.newObject();
            errdefer jrule.deinit();

            if (srule.type) |rule_type| {
                var jtype = try mapper.convertType(rule_type);
                try jrule.putNoClobber("type", jtype);
            } else {
                try jrule.putNoClobber("type", .null);
            }

            {
                var jprods = mapper.newArray();
                errdefer jprods.deinit();

                try jprods.resize(srule.productions.len);

                for (jprods.items, srule.productions) |*jmprod_val, mapped_production| {
                    var jmprod = mapper.newObject();
                    errdefer jmprod.deinit();

                    var jprod = try mapper.convertProduction(mapped_production.production);

                    try jmprod.putNoClobber("production", jprod);

                    if (mapped_production.mapping) |mapping| {
                        var jmap = try mapper.convertMapping(mapping);
                        try jmprod.putNoClobber("mapping", jmap);
                    } else {
                        try jmprod.putNoClobber("mapping", .null);
                    }

                    jmprod_val.* = .{ .object = jmprod };
                }

                try jrule.putNoClobber("mapped_productions", .{ .array = jprods });
            }

            try rules.putNoClobber(
                strings.get(kvp.key_ptr.*),
                .{ .object = jrule },
            );
        }

        try root.put("rules", .{ .object = rules });
    }

    return std.json.Value{ .object = root };
}

const JsonMapper = struct {
    allocator: std.mem.Allocator,
    strings: *const ptk.strings.Pool,

    fn convertProduction(mapper: JsonMapper, production: sema.Production) error{OutOfMemory}!std.json.Value {
        var jtype = mapper.newObject();
        errdefer jtype.deinit();

        try jtype.putNoClobber("kind", .{ .string = @tagName(production) });

        const data: std.json.Value = switch (production) {
            .terminal => |terminal| blk: {
                if (terminal.is_literal) {
                    try jtype.put("kind", .{ .string = "literal-terminal" });
                }
                break :blk mapper.jsonString(terminal.name);
            },
            .recursion => |recursion| mapper.jsonString(recursion.name),

            .sequence => |sequence| blk: {
                var list = mapper.newArray();
                errdefer list.deinit();

                try list.resize(sequence.len);

                for (list.items, sequence) |*dst, src| {
                    dst.* = try mapper.convertProduction(src);
                }

                break :blk .{ .array = list };
            },

            .optional, .repetition_zero, .repetition_one => |optional| try mapper.convertProduction(optional.*),
        };
        try jtype.putNoClobber("data", data);

        return .{ .object = jtype };
    }

    fn convertMapping(mapper: JsonMapper, mapping: sema.Mapping) error{OutOfMemory}!std.json.Value {
        var jtype = mapper.newObject();
        errdefer jtype.deinit();

        try jtype.putNoClobber("kind", .{ .string = @tagName(mapping) });

        switch (mapping) {
            .record_initializer => |record_initializer| {
                var list = mapper.newArray();
                errdefer list.deinit();

                try list.resize(record_initializer.fields.len);

                for (list.items, record_initializer.fields) |*dst, src| {
                    var jfield = mapper.newObject();
                    errdefer jfield.deinit();

                    try jfield.putNoClobber("field", mapper.jsonString(src.field.name));
                    try jfield.putNoClobber("value", try mapper.convertMapping(src.value));

                    dst.* = .{ .object = jfield };
                }

                try jtype.putNoClobber("fields", .{ .array = list });
            },
            .list_initializer => |list_initializer| {
                var list = mapper.newArray();
                errdefer list.deinit();

                try list.resize(list_initializer.items.len);

                for (list.items, list_initializer.items) |*dst, src| {
                    dst.* = try mapper.convertMapping(src);
                }

                try jtype.putNoClobber("items", .{ .array = list });
            },
            .variant_initializer => |variant_initializer| {
                try jtype.putNoClobber("field", mapper.jsonString(variant_initializer.field.name));
                try jtype.putNoClobber("value", try mapper.convertMapping(variant_initializer.value.*));
            },
            .user_function_call, .builtin_function_call => |function_call| {
                var list = mapper.newArray();
                errdefer list.deinit();

                try list.resize(function_call.arguments.len);

                for (list.items, function_call.arguments) |*dst, src| {
                    dst.* = try mapper.convertMapping(src);
                }

                try jtype.putNoClobber("arguments", .{ .array = list });

                try jtype.putNoClobber("function", mapper.jsonString(function_call.function));
            },

            .code_literal, .user_literal => |literal| {
                try jtype.putNoClobber("literal", mapper.jsonString(literal));
            },

            .context_reference => |context_reference| {
                try jtype.putNoClobber("index", .{ .integer = context_reference.index });
            },
        }

        return .{ .object = jtype };
    }

    fn convertType(mapper: JsonMapper, stype: *sema.Type) error{OutOfMemory}!std.json.Value {
        const data: std.json.Value = switch (stype.*) {
            .code_literal, .user_type => |literal| mapper.jsonString(literal),
            .named => |named| mapper.jsonString(named.name),

            .optional => |inner| try mapper.convertType(inner),

            .record, .variant => |compound| blk: {
                var fields = mapper.newObject();
                errdefer fields.deinit();

                for (compound.fields.keys(), compound.fields.values()) |name, field| {
                    var field_type = try mapper.convertType(field.type);
                    try fields.putNoClobber(
                        mapper.strings.get(name),
                        field_type,
                    );
                }

                break :blk .{ .object = fields };
            },

            .token => .null,
        };

        var jtype = mapper.newObject();
        errdefer jtype.deinit();

        try jtype.putNoClobber("kind", .{ .string = @tagName(stype.*) });
        try jtype.putNoClobber("data", data);

        return .{ .object = jtype };
    }

    fn jsonString(mapper: JsonMapper, string: ptk.strings.String) std.json.Value {
        return .{ .string = mapper.strings.get(string) };
    }

    fn newObject(mapper: JsonMapper) std.json.ObjectMap {
        return std.json.ObjectMap.init(mapper.allocator);
    }

    fn newArray(mapper: JsonMapper) std.json.Array {
        return std.json.Array.init(mapper.allocator);
    }
};
