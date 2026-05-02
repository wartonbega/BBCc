
 ... 
TODO: écrire de la doc

## Adding inbuilt librairies in zig

name : foo

1. add `src/interpretor/inbuilt/foo.config` — signatures
2. `src/interpretor/inbuilt/foo.zig` — `pub fn dispatch(name: []const u8, args: []Values.Value, ctx: *Itpr.Context, ref: Parser.Location) !Values.Value {}`
3. One line in `src/inbuilt_libs.zig`:
    `.{ "foo", @embedFile("interpretor/inbuilt/foo.config") },`
And one routing line in `src/interpretor/inbuilt.zig`:
    `if (std.mem.eql(u8, lib, "foo")) return foo_lib.dispatch(func, args.items, ctx, reference);`

