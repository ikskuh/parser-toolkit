const std = @import("std");
const ptk = @import("parser-toolkit");

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();

    const allocator = &gpa.allocator;

    var line_buffer = std.ArrayList(u8).init(allocator);
    defer line_buffer.deinit();

    var stdin = std.io.getStdIn().reader();
    var stdout = std.io.getStdOut().writer();

    var calc = Calculator.init(allocator);
    defer calc.deinit();

    try calc.set("pi", std.math.pi);

    main_loop: while (true) {
        try stdout.writeAll("? ");
        stdin.readUntilDelimiterArrayList(&line_buffer, '\n', 4096) catch |err| switch (err) {
            error.EndOfStream => break :main_loop,
            else => |e| return e,
        };
        const line = std.mem.trim(u8, line_buffer.items, "\t ");

        const value = calc.evaluate(line) catch |err| {
            try stdout.print("error: {s}\n", .{@errorName(err)});
            continue;
        };
        try stdout.print("= {d}\n", .{value});
    }
    try stdout.writeAll("\n");
}

const Calculator = struct {
    const Self = @This();

    arena: std.heap.ArenaAllocator,
    variables: std.StringHashMapUnmanaged(f64),

    pub fn init(allocator: *std.mem.Allocator) Self {
        return Self{
            .arena = std.heap.ArenaAllocator.init(allocator),
            .variables = .{},
        };
    }

    pub fn deinit(self: *Self) void {
        self.variables.deinit(&self.arena.allocator);
        self.arena.deinit();
        self.* = undefined;
    }

    pub fn set(self: *Self, name: []const u8, value: f64) !void {
        const gop = try self.variables.getOrPut(&self.arena.allocator, name);
        if (!gop.found_existing) {
            errdefer _ = self.variables.remove(name);
            gop.key_ptr.* = try self.arena.allocator.dupe(u8, name);
        }
        gop.value_ptr.* = value;
    }

    pub fn get(self: Self, name: []const u8) ?f64 {
        return self.variables.get(name);
    }

    pub fn evaluate(self: *Self, expression: []const u8) !f64 {
        var ast = try Parser.parse(&self.arena.allocator, expression);
        defer ast.deinit();

        const value = self.evaluateAstNode(ast.root);

        return value;
    }

    const EvalError = error{ VariableNotFound, OutOfMemory, ArgumentMismatch, UnknownFunction };
    fn evaluateAstNode(self: *Self, node: Ast.Node) EvalError!f64 {
        return switch (node) {
            .number => |n| n,
            .variable => |v| self.get(v) orelse return error.VariableNotFound,
            .binary_operator => |op| blk: {
                const lhs = try self.evaluateAstNode(op.lhs.*);
                const rhs = try self.evaluateAstNode(op.rhs.*);
                break :blk switch (op.operator) {
                    .add => lhs + rhs,
                    .subtract => lhs - rhs,
                    .multiply => lhs * rhs,
                    .divide => lhs / rhs,
                    .modulus => @mod(lhs, rhs),
                };
            },
            .unary_operator => |op| blk: {
                const value = try self.evaluateAstNode(op.value.*);
                break :blk switch (op.operator) {
                    .negate => -value,
                };
            },
            .assignment => |ass| blk: {
                const value = try self.evaluateAstNode(ass.value.*);
                try self.set(ass.variable, value);
                break :blk value;
            },
            .function_invocation => |fun| try self.invokeFunction(fun),
        };
    }

    fn invokeFunction(self: *Self, fun: Ast.Node.FunctionInvocation) EvalError!f64 {
        inline for (std.meta.declarations(builtin_functions)) |decl| {
            if (std.mem.eql(u8, decl.name, fun.function)) {
                const FnType = decl.data.Fn.fn_type;
                const function = @field(builtin_functions, decl.name);
                return try self.invokeExplicitFunction(FnType, function, fun);
            }
        }
        return error.UnknownFunction;
    }

    fn invokeExplicitFunction(self: *Self, comptime FnType: type, comptime function: FnType, fun: Ast.Node.FunctionInvocation) EvalError!f64 {
        const ArgsTuple = std.meta.ArgsTuple(FnType);

        var args: ArgsTuple = undefined;
        if (fun.arguments.len != args.len)
            return error.ArgumentMismatch;

        comptime var i = 0;
        inline while (i < args.len) : (i += 1) {
            args[i] = try self.evaluateAstNode(fun.arguments[i]);
        }

        const value = @call(.{}, function, args);

        return value;
    }

    const builtin_functions = struct {
        pub fn sin(v: f64) f64 {
            return std.math.sin(v);
        }
        pub fn cos(v: f64) f64 {
            return std.math.cos(v);
        }
        pub fn tan(v: f64) f64 {
            return std.math.tan(v);
        }
        pub fn sqrt(v: f64) f64 {
            return std.math.sqrt(v);
        }
        pub fn pow(a: f64, b: f64) f64 {
            return std.math.pow(f64, a, b);
        }
    };
};

const UnOp = enum { negate };
const BinOp = enum { add, subtract, multiply, divide, modulus };

const Ast = struct {
    arena: std.heap.ArenaAllocator,
    root: Node,

    pub fn deinit(self: *Ast) void {
        self.arena.deinit();
        self.* = undefined;
    }

    const Node = union(enum) {
        const Self = @This();

        number: f64,
        variable: []const u8,
        unary_operator: UnaryOperator,
        binary_operator: BinaryOperator,
        function_invocation: FunctionInvocation,
        assignment: Assignment,

        const UnaryOperator = struct {
            operator: UnOp,
            value: *Self,
        };
        const BinaryOperator = struct {
            operator: BinOp,
            lhs: *Self,
            rhs: *Self,
        };
        const FunctionInvocation = struct {
            function: []const u8,
            arguments: []Self,
        };
        const Assignment = struct {
            variable: []const u8,
            value: *Self,
        };
    };
};

const Parser = struct {
    const Self = @This();

    const TokenType = enum {
        number,
        identifier,
        whitespace,
        @"+",
        @"-",
        @"*",
        @"/",
        @"%",
        @"(",
        @")",
        @"=",
        @",",
    };

    const Pattern = ptk.Pattern(TokenType);

    const Tokenizer = ptk.Tokenizer(TokenType, &[_]Pattern{
        Pattern.create(.number, ptk.matchers.sequenceOf(.{ ptk.matchers.decimalNumber, ptk.matchers.literal("."), ptk.matchers.decimalNumber })),
        Pattern.create(.number, ptk.matchers.decimalNumber),
        Pattern.create(.identifier, ptk.matchers.identifier),
        Pattern.create(.whitespace, ptk.matchers.whitespace),
        Pattern.create(.@"+", ptk.matchers.literal("+")),
        Pattern.create(.@"-", ptk.matchers.literal("-")),
        Pattern.create(.@"*", ptk.matchers.literal("*")),
        Pattern.create(.@"/", ptk.matchers.literal("/")),
        Pattern.create(.@"%", ptk.matchers.literal("%")),
        Pattern.create(.@"(", ptk.matchers.literal("(")),
        Pattern.create(.@")", ptk.matchers.literal(")")),
        Pattern.create(.@"=", ptk.matchers.literal("=")),
        Pattern.create(.@",", ptk.matchers.literal(",")),
    });

    const ParserCore = ptk.ParserCore(Tokenizer, .{.whitespace});

    pub fn parse(allocator: *std.mem.Allocator, expression: []const u8) !Ast {
        var ast = Ast{
            .arena = std.heap.ArenaAllocator.init(allocator),
            .root = undefined,
        };

        var tokenizer = Tokenizer.init(expression);

        var parser = Parser{
            .arena = &ast.arena.allocator,
            .core = ParserCore.init(&tokenizer),
        };

        ast.root = try parser.acceptTopLevelExpression();

        if ((try parser.core.peek()) != null)
            return error.SyntaxError;

        return ast;
    }

    arena: *std.mem.Allocator,
    core: ParserCore,

    const Error = ParserCore.Error || std.mem.Allocator.Error;
    const ruleset = ptk.RuleSet(TokenType);

    fn moveToHeap(self: *Self, value: anytype) !*@TypeOf(value) {
        const T = @TypeOf(value);
        std.debug.assert(@typeInfo(T) != .Pointer);
        const ptr = try self.arena.create(T);
        ptr.* = value;

        std.debug.assert(std.meta.eql(ptr.*, value));

        return ptr;
    }

    fn acceptTopLevelExpression(self: *Self) Error!Ast.Node {
        const state = self.core.saveState();
        errdefer self.core.restoreState(state);

        if (self.acceptAssignment()) |ass| {
            return ass;
        } else |_| {
            return try self.acceptExpression();
        }
    }

    fn acceptAssignment(self: *Self) Error!Ast.Node {
        const state = self.core.saveState();
        errdefer self.core.restoreState(state);

        const variable = try self.core.accept(comptime ruleset.is(.identifier));
        _ = try self.core.accept(comptime ruleset.is(.@"="));
        const value = try self.acceptExpression();

        return Ast.Node{
            .assignment = .{
                .variable = try self.arena.dupe(u8, variable.text),
                .value = try self.moveToHeap(value),
            },
        };
    }

    fn acceptExpression(self: *Self) Error!Ast.Node {
        const state = self.core.saveState();
        errdefer self.core.restoreState(state);
        return try self.acceptSumExpression();
    }

    fn acceptSumExpression(self: *Self) Error!Ast.Node {
        const state = self.core.saveState();
        errdefer self.core.restoreState(state);

        var expr = try self.acceptMulExpression();
        while (true) {
            const operator = self.core.accept(comptime ruleset.oneOf(.{ .@"+", .@"-" })) catch break;
            const rhs = try self.acceptMulExpression();

            const new_expr = Ast.Node{
                .binary_operator = .{
                    .operator = switch (operator.type) {
                        .@"+" => .add,
                        .@"-" => .subtract,
                        else => unreachable,
                    },
                    .lhs = try self.moveToHeap(expr),
                    .rhs = try self.moveToHeap(rhs),
                },
            };
            expr = new_expr;
        }
        return expr;
    }

    fn acceptMulExpression(self: *Self) Error!Ast.Node {
        const state = self.core.saveState();
        errdefer self.core.restoreState(state);

        var expr = try self.acceptUnaryPrefixOperatorExpression();
        while (true) {
            const operator = self.core.accept(comptime ruleset.oneOf(.{ .@"*", .@"/", .@"%" })) catch break;
            const rhs = try self.acceptUnaryPrefixOperatorExpression();

            const new_expr = Ast.Node{
                .binary_operator = .{
                    .operator = switch (operator.type) {
                        .@"*" => .multiply,
                        .@"/" => .divide,
                        .@"%" => .modulus,
                        else => unreachable,
                    },
                    .lhs = try self.moveToHeap(expr),
                    .rhs = try self.moveToHeap(rhs),
                },
            };
            expr = new_expr;
        }
        return expr;
    }

    fn acceptUnaryPrefixOperatorExpression(self: *Self) Error!Ast.Node {
        const state = self.core.saveState();
        errdefer self.core.restoreState(state);

        if (self.core.accept(comptime ruleset.is(.@"-"))) |_| {
            // this must directly recurse as we can write `- - x`
            const value = try self.acceptUnaryPrefixOperatorExpression();
            return Ast.Node{
                .unary_operator = .{
                    .operator = .negate,
                    .value = try self.moveToHeap(value),
                },
            };
        } else |_| {
            return try self.acceptFunctionCallExpression();
        }
    }

    fn acceptFunctionCallExpression(self: *Self) Error!Ast.Node {
        const state = self.core.saveState();
        errdefer self.core.restoreState(state);

        if (self.acceptFunctionCall()) |fncall| {
            return fncall;
        } else |_| {
            return try self.acceptValueExpression();
        }
    }

    fn acceptFunctionCall(self: *Self) Error!Ast.Node {
        const state = self.core.saveState();
        errdefer self.core.restoreState(state);

        const name = try self.core.accept(comptime ruleset.is(.identifier));
        _ = try self.core.accept(comptime ruleset.is(.@"("));

        var arglist = std.ArrayList(Ast.Node).init(self.arena);
        errdefer arglist.deinit();

        const no_arg_terminator = try self.core.peek();
        if (no_arg_terminator != null and no_arg_terminator.?.type == .@")") {
            _ = try self.core.accept(comptime ruleset.is(.@")"));
            return Ast.Node{
                .function_invocation = .{
                    .function = try self.arena.dupe(u8, name.text),
                    .arguments = arglist.toOwnedSlice(),
                },
            };
        }

        while (true) {
            const arg = try self.acceptExpression();
            try arglist.append(arg);

            const next = try self.core.accept(comptime ruleset.oneOf(.{ .@")", .@"," }));
            if (next.type == .@")")
                break;
        }

        return Ast.Node{
            .function_invocation = .{
                .function = try self.arena.dupe(u8, name.text),
                .arguments = arglist.toOwnedSlice(),
            },
        };
    }

    fn acceptValueExpression(self: *Self) Error!Ast.Node {
        const state = self.core.saveState();
        errdefer self.core.restoreState(state);

        const token = try self.core.accept(comptime ruleset.oneOf(.{
            .@"(",
            .number,
            .identifier,
        }));
        switch (token.type) {
            .@"(" => {
                const value = try self.acceptExpression();
                _ = try self.core.accept(comptime ruleset.is(.@")"));
                return value;
            },
            .number => {
                const val = std.fmt.parseFloat(f64, token.text) catch unreachable;
                return Ast.Node{
                    .number = val,
                };
            },
            .identifier => return Ast.Node{
                .variable = try self.arena.dupe(u8, token.text),
            },
            else => unreachable,
        }
    }
};
