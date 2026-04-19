const std = @import("std");
const inst = @import("instructions.zig");
const regTable = @import("registerTable.zig");
const analyser = @import("../analyser.zig");

pub const Compiler = struct {
    allocator: std.mem.Allocator,
    registerTable: regTable.RegisterTable,
    program: std.ArrayList(inst.Instruction),

    // the function table, registering all the function with the same name
    f_table: std.hash_map.StringHashMap(usize),
    func_uid_list: std.ArrayList(struct { uid: []const u8, version: analyser.functionVersion }),
    label_table: std.hash_map.StringHashMap(usize),

    stack_size: i64 = 0,

    pub fn incrementStackOffset(self: *Compiler) void {
        self.stack_size += 1;
    }

    pub fn decrementStackOffset(self: *Compiler) void {
        self.stack_size -= 1;
    }

    // here are defined global variables for the code generation
    pub inline fn getErrorUnionFunction() []const u8 {
        return "global.error.union_callback";
    }
    pub inline fn getErrorUnionLabel() []const u8 {
        return "global.error.union";
    }
    pub inline fn getMainWrapper() []const u8 {
        return "global.main.wrapper";
    }
    pub inline fn getGlobalFreeObject() []const u8 {
        return "global.object.free";
    }

    pub fn init(alloc: std.mem.Allocator) !*Compiler {
        const c = try alloc.create(Compiler);
        c.* = Compiler{
            .allocator = alloc,
            .registerTable = try regTable.RegisterTable.init(alloc),
            .program = .init(alloc),
            .f_table = .init(alloc),
            .label_table = .init(alloc),
            .func_uid_list = .init(alloc),
        };
        return c;
    }

    pub fn addInstruction(self: *Compiler, instruction: inst.Instruction) !void {
        try self.program.append(instruction);
    }

    pub fn generateLabel(self: *Compiler, name: []const u8) !inst.LABEL {
        const r = try self.label_table.getOrPutValue(name, 0);
        try self.label_table.put(name, r.value_ptr.* + 1);
        return inst.LABEL{ .name = try std.fmt.allocPrint(
            self.allocator,
            "{s}#{d}",
            .{ name, r.value_ptr.* },
        ) };
    }

    pub fn getUid(self: *Compiler, version: analyser.functionVersion) []const u8 {
        base_for: for (self.func_uid_list.items) |func| {
            if (std.mem.eql(u8, func.version.name, version.name)) {
                if (func.version.version.count() != version.version.count())
                    continue;
                var it = func.version.version.iterator();
                while (it.next()) |vers| {
                    if (!version.version.contains(vers.key_ptr.*))
                        continue :base_for;
                    const arg_type = version.version.get(vers.key_ptr.*).?;
                    if (!arg_type.matchType(vers.value_ptr.*))
                        continue :base_for;
                }
                return func.uid;
            }
        }
        std.debug.print("[INTERN ERROR]: Couldnt find version for {s}\n", .{version.name});
        return version.name;
    }
};
