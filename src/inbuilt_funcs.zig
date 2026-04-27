const std = @import("std");

pub const Param = struct {
    type_name: []const u8,
    any: bool,
    variadic: bool,
};

pub const Func = struct {
    name: []const u8,
    params: []Param,
    return_type: []const u8,
    return_type_has_error: bool,
    propagate_errors: bool,
};

const config_text = @embedFile("inbuilt_funcs.config");

pub fn load(allocator: std.mem.Allocator) ![]Func {
    var result = std.ArrayList(Func).init(allocator);
    var lines = std.mem.splitScalar(u8, config_text, '\n');
    while (lines.next()) |raw_line| {
        const line = std.mem.trim(u8, raw_line, " \t\r");
        if (line.len == 0 or line[0] == '#') continue;

        const paren_open = std.mem.indexOf(u8, line, "(") orelse continue;
        const paren_close = std.mem.indexOf(u8, line, ")") orelse continue;
        const arrow = std.mem.indexOf(u8, line, "->") orelse continue;
        if (paren_close < paren_open or arrow < paren_close) continue;

        const name = std.mem.trim(u8, line[0..paren_open], " \t");
        const params_str = std.mem.trim(u8, line[paren_open + 1 .. paren_close], " \t");
        var ret_str = std.mem.trim(u8, line[arrow + 2 ..], " \t");

        const propagate_errors = ret_str.len > 0 and ret_str[ret_str.len - 1] == '~';
        if (propagate_errors) ret_str = std.mem.trim(u8, ret_str[0 .. ret_str.len - 1], " \t");

        const ret_type_has_error = ret_str.len > 0 and ret_str[0] == '!';
        if (ret_type_has_error) ret_str = std.mem.trim(u8, ret_str[1..ret_str.len], " \t");

        var params = std.ArrayList(Param).init(allocator);
        if (params_str.len > 0) {
            var param_iter = std.mem.splitScalar(u8, params_str, ',');
            while (param_iter.next()) |raw_param| {
                const ps = std.mem.trim(u8, raw_param, " \t");
                if (ps.len == 0) continue;
                const variadic = ps[ps.len - 1] == '*';
                const type_str = if (variadic) std.mem.trim(u8, ps[0 .. ps.len - 1], " \t") else ps;
                const is_any = std.mem.eql(u8, type_str, "Any");
                try params.append(.{
                    .type_name = type_str,
                    .any = is_any,
                    .variadic = variadic,
                });
            }
        }

        try result.append(.{
            .name = name,
            .params = try params.toOwnedSlice(),
            .return_type = ret_str,
            .propagate_errors = propagate_errors,
            .return_type_has_error = ret_type_has_error,
        });
    }
    return result.toOwnedSlice();
}
