const std = @import("std");
const ptk = @import("parser-toolkit");

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();

    const allocator = gpa.allocator();

    var line_buffer = std.ArrayList(u8).init(allocator);
    defer line_buffer.deinit();

    var stdin = std.io.getStdIn().reader();
    var stdout = std.io.getStdOut().writer();

    var calc = Calculator.init(allocator);
    defer calc.deinit();

    try calc.set("pi", std.math.pi);
    try calc.set("e", std.math.e);

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

    pub fn init(allocator: std.mem.Allocator) Self {
        return Self{
            .arena = std.heap.ArenaAllocator.init(allocator),
            .variables = .{},
        };
    }

    pub fn deinit(self: *Self) void {
        self.variables.deinit(self.arena.allocator());
        self.arena.deinit();
        self.* = undefined;
    }

    pub fn set(self: *Self, name: []const u8, value: f64) !void {
        const gop = try self.variables.getOrPut(self.arena.allocator(), name);
        if (!gop.found_existing) {
            errdefer _ = self.variables.remove(name);
            gop.key_ptr.* = try self.arena.allocator().dupe(u8, name);
        }
        gop.value_ptr.* = value;
    }

    pub fn get(self: Self, name: []const u8) ?f64 {
        return self.variables.get(name);
    }

    pub fn evaluate(self: *Self, expression: []const u8) !f64 {
        return try Parser.parse(.{ .calc = self }, expression);
    }

    const EvalError = error{ UnknownFunction, ArgumentMismatch };

    fn invokeFunction(self: *Self, func_name: []const u8, args: []const f64) EvalError!f64 {
        inline for (comptime std.meta.declarations(builtin_functions)) |decl| {
            if (std.mem.eql(u8, decl.name, func_name)) {
                const function = @field(builtin_functions, decl.name);
                const FnType = @TypeOf(function);
                return try self.invokeExplicitFunction(FnType, function, args);
            }
        }
        return error.UnknownFunction;
    }

    fn invokeExplicitFunction(self: *Self, comptime FnType: type, comptime function: FnType, arg_vals: []const f64) EvalError!f64 {
        _ = self;

        const ArgsTuple = std.meta.ArgsTuple(FnType);

        var args: ArgsTuple = undefined;
        if (arg_vals.len != args.len)
            return error.ArgumentMismatch;

        comptime var i = 0;
        inline while (i < args.len) : (i += 1) {
            args[i] = arg_vals[i];
        }

        return @call(.{}, function, args);
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
        pub fn ln(v: f64) f64 {
            return std.math.ln(v);
        }
        pub fn ln10(v: f64) f64 {
            return std.math.log10(v);
        }
        pub fn ln2(v: f64) f64 {
            return std.math.log2(v);
        }
        pub fn log(b: f64, v: f64) f64 {
            return std.math.log(f64, b, v);
        }
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

    const ExpressionContext = struct {
        calc: *Calculator,

        fn set(self: *ExpressionContext, var_name: []const u8, value: f64) !void {
            try self.calc.set(var_name, value);
        }

        fn get(self: *ExpressionContext, var_name: []const u8) ?f64 {
            return self.calc.get(var_name);
        }

        fn call(self: *ExpressionContext, func: []const u8, args: []const f64) !f64 {
            return try self.calc.invokeFunction(func, args);
        }
    };

    pub fn parse(context: ExpressionContext, expression: []const u8) !f64 {
        var tokenizer = Tokenizer.init(expression, null);

        var parser = Parser{
            .core = ParserCore.init(&tokenizer),
            .ctx = context,
        };

        const value = try parser.acceptTopLevelExpression();

        if ((try parser.core.peek()) != null)
            return error.SyntaxError;

        return value;
    }

    core: ParserCore,
    ctx: ExpressionContext,

    const Error = ParserCore.Error || Calculator.EvalError || std.mem.Allocator.Error || error{VariableNotFound};
    const ruleset = ptk.RuleSet(TokenType);

    fn acceptTopLevelExpression(self: *Self) Error!f64 {
        const state = self.core.saveState();
        errdefer self.core.restoreState(state);

        if (self.acceptAssignment()) |ass| {
            return ass;
        } else |_| {
            return try self.acceptExpression();
        }
    }

    fn acceptAssignment(self: *Self) Error!f64 {
        const state = self.core.saveState();
        errdefer self.core.restoreState(state);

        const variable = try self.core.accept(comptime ruleset.is(.identifier));
        _ = try self.core.accept(comptime ruleset.is(.@"="));
        const value = try self.acceptExpression();

        try self.ctx.set(variable.text, value);

        return value;
    }

    fn acceptExpression(self: *Self) Error!f64 {
        const state = self.core.saveState();
        errdefer self.core.restoreState(state);
        return try self.acceptSumExpression();
    }

    fn acceptSumExpression(self: *Self) Error!f64 {
        const state = self.core.saveState();
        errdefer self.core.restoreState(state);

        var value = try self.acceptMulExpression();
        while (true) {
            const operator = self.core.accept(comptime ruleset.oneOf(.{ .@"+", .@"-" })) catch break;
            const rhs = try self.acceptMulExpression();

            const new_value = switch (operator.type) {
                .@"+" => value + rhs,
                .@"-" => value - rhs,
                else => unreachable,
            };
            value = new_value;
        }
        return value;
    }

    fn acceptMulExpression(self: *Self) Error!f64 {
        const state = self.core.saveState();
        errdefer self.core.restoreState(state);

        var value = try self.acceptUnaryPrefixOperatorExpression();
        while (true) {
            const operator = self.core.accept(comptime ruleset.oneOf(.{ .@"*", .@"/", .@"%" })) catch break;
            const rhs = try self.acceptUnaryPrefixOperatorExpression();
            const new_value = switch (operator.type) {
                .@"*" => value * rhs,
                .@"/" => value / rhs,
                .@"%" => @mod(value, rhs),
                else => unreachable,
            };
            value = new_value;
        }
        return value;
    }

    fn acceptUnaryPrefixOperatorExpression(self: *Self) Error!f64 {
        const state = self.core.saveState();
        errdefer self.core.restoreState(state);

        if (self.core.accept(comptime ruleset.is(.@"-"))) |_| {
            // this must directly recurse as we can write `- - x`
            const value = try self.acceptUnaryPrefixOperatorExpression();
            return -value;
        } else |_| {
            return try self.acceptFunctionCallExpression();
        }
    }

    fn acceptFunctionCallExpression(self: *Self) Error!f64 {
        const state = self.core.saveState();
        errdefer self.core.restoreState(state);

        if (self.acceptFunctionCall()) |fncall| {
            return fncall;
        } else |_| {
            return try self.acceptValueExpression();
        }
    }

    fn acceptFunctionCall(self: *Self) Error!f64 {
        const state = self.core.saveState();
        errdefer self.core.restoreState(state);

        const name = try self.core.accept(comptime ruleset.is(.identifier));
        _ = try self.core.accept(comptime ruleset.is(.@"("));

        var args: [64]f64 = undefined;
        var argc: usize = 0;

        const no_arg_terminator = try self.core.peek();
        if (no_arg_terminator != null and no_arg_terminator.?.type == .@")") {
            _ = try self.core.accept(comptime ruleset.is(.@")"));
            return try self.ctx.call(name.text, args[0..argc]);
        }

        while (true) {
            const arg = try self.acceptExpression();
            args[argc] = arg;
            argc += 1;

            const next = try self.core.accept(comptime ruleset.oneOf(.{ .@")", .@"," }));
            if (next.type == .@")")
                break;
        }

        return try self.ctx.call(name.text, args[0..argc]);
    }

    fn acceptValueExpression(self: *Self) Error!f64 {
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
            .number => return std.fmt.parseFloat(f64, token.text) catch unreachable,
            .identifier => return self.ctx.get(token.text) orelse error.VariableNotFound,
            else => unreachable,
        }
    }
};
