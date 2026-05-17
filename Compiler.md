# BBC Compiler — Status & Implementation Plan

## Quick Reference — Build & Run

> **Platform:** macOS M2 (arm64). X86-64 code runs via Rosetta 2. Always use `arch -x86_64` to run the generated binary.

### 1 — Build the BBC compiler

```sh
zig build
# Produces: ./zig-out/bin/bbc
# NOTE: build.zig hardcodes .ReleaseFast — `unreachable` is silent UB, not a panic.
#       To get a stack trace, temporarily change to .Debug in build.zig.
```

### 2 — Codegen path (BBC → x86-64 NASM → binary)

```sh
# Full pipeline for any .bbc file:
./zig-out/bin/bbc <file.bbc> \
  && nasm -f macho64 output.asm -o output.o \
  && gcc -target x86_64-apple-macos12 output.o -o output_binary \
  && arch -x86_64 ./output_binary 2>&1; echo "exit: $?"

# Example with test4.bbc:
./zig-out/bin/bbc test4.bbc \
  && nasm -f macho64 output.asm -o output.o \
  && gcc -target x86_64-apple-macos12 output.o -o output_binary \
  && arch -x86_64 ./output_binary 2>&1; echo "exit: $?"
```

The BBC compiler always writes its assembly to `./output.asm` in the **current working directory**.  
All commands must be run from the repo root (`/Users/antonappel/Desktop/Code/bbc`).

### 3 — Interpreter path (reference implementation, fully working)

```sh
./zig-out/bin/bbc -X <file.bbc>
```

Use the interpreter to check expected output before debugging the codegen.  
**The codegen must produce identical observable behaviour to the interpreter.**

### 4 — Crash / debug workflow

```sh
# 1. Reproduce the crash
./zig-out/bin/bbc <file.bbc>; echo "bbc exit: $?"
# exit 139 = SIGSEGV in the BBC compiler itself (not the compiled program)

# 2. Get a backtrace (temporarily set .Debug in build.zig first, then rebuild)
lldb -o "run" -o "bt" -o "quit" -- ./zig-out/bin/bbc <file.bbc>

# 3. Inspect the generated assembly (only valid if bbc didn't crash)
cat output.asm

# 4. Assemble + link + run with signal info
./zig-out/bin/bbc <file.bbc> \
  && nasm -f macho64 output.asm -o output.o \
  && gcc -target x86_64-apple-macos12 output.o -o output_binary \
  && arch -x86_64 ./output_binary 2>&1; echo "exit: $?"
# exit 136 = SIGFPE (e.g. idiv by zero)
# exit 139 = SIGSEGV in the compiled binary
```

### 5 — Test suite

Run all tests via `./run_tests.sh`.  Use `--validate` to check interpreter only, `--codegen` for codegen only, `--show` to print interpreter output without comparing, `--leaks` to compile each test and run under `leaks --atExit` (macOS) to detect heap leaks.

| Test file | Covers | Expected output (newline-separated) | exit |
|-----------|--------|--------------------------------------|------|
| `test4.bbc` | structs, while, buffer, field access, if/else | `7 120 150 7 100` | 0 |
| `test5.bbc` | strings, for-loop, char print | `5`, `hello` | 0 |
| `test6.bbc` | infinite GC stress loop (constant memory) | infinite `1` | 0 |
| `tests/buf_test.bbc` | buffer alloc/lit/index/assign | no output | 1 |
| `tests/test_err_div.bbc` | division-by-zero guard + `? err` | `5 0 25` | 0 |
| `tests/test_err_buf.bbc` | buffer OOB + `? err` | `10 30 0` | 0 |
| `tests/test_err_str.bbc` | string OOB + `? err` | `h o ?` | 0 |
| `tests/test_err_subscription.bbc` | custom Subscription trait + `? err` | `0 20 0` | 0 |
| `tests/test_err_subscription_propagate.bbc` | error propagation via `!T` | `3 1` | 0 |
| `tests/test_err_propagate.bbc` | error propagation through call chain | `10 0` | 0 |
| `tests/test_err_generic.bbc` | generic struct + `? err` | `42 42 0` | 0 |
| `tests/test_generics.bbc` | generic Pair, `makePair`/`swapA` | `3 99` | 0 |
| `tests/test_bool_ops.bbc` | `not`, `&&`, `\|\|`, combined conditions | 15 lines of true/false | 0 |
| `tests/test_arithmetic.bbc` | `+/-/*/÷`, precedence, `elif`, comparisons | 19 lines | 0 |
| `tests/test_recursion.bbc` | recursive fib, gcd (subtraction), power | `0 1 5 21 4 25 1 1 256 81` | 0 |
| `tests/test_nested_while.bbc` | nested while loops, inner-scope vars | `9 25 6 0 3` | 0 |
| `tests/test_struct_methods.bbc` | struct + `implement` + method calls | `0 5 5 20 30` | 0 |
| `tests/test_buf_types.bbc` | `[Bool]`/`[Char]` buffers, lit + alloc | `5 3 2 1 5 hello Hello 3` | 0 |
| `tests/test_string_concat.bbc` | String + Char concat, GC | `5 3` | 0 |
| `tests/test_multi_tp.bbc` | multi-param generics (Pair<A,B>, swap, Triple) | `42 true x 99 99 x z` | 0 |
| `tests/test_multi_tp2.bbc` | multi-param generics variant 2 | `99 true 10 z 25 42 100` | 0 |
| `tests/test_multi_tp3.bbc` | multi-param generics variant 3 | `1 true false q a 7 10 20` | 0 |
| `tests/test_subscription.bbc` | string subscript `s[0]` | `h` | 0 |
| `tests/test_forloop.bbc` | for-loop + custom `countChars`, Strings import | `5`, `hello` | 0 |
| `tests/test_error1.bbc` | generic `Pair<A,B>` + `getFirst` | `1` | 0 |
| `tests/test_scope_err.bbc` | error in mid-scope caught by `? err` | `20` | 0 |
| `tests/test_inbuilt.bbc` | built-in functions (`min`, `max`, etc.) | varies | 0 |
| `tests/test_strings_truncate.bbc` | `Strings.truncate` namespace call | varies | 0 |
| `tests/namespace/test_depth.bbc` | nested namespace resolution | varies | 0 |
| `tests/namespace/test_flat_import.bbc` | flat import path | varies | 0 |
| `tests/namespace/test_namespace_basic.bbc` | basic namespace function call | varies | 0 |
| `tests/namespace/test_namespace_struct.bbc` | namespace + struct interaction | varies | 0 |
| `tests/namespace/test_paren_ns.bbc` | parenthesised namespace expression | varies | 0 |
| `tests/test_bound_method.bbc` | first-class bound method value (capture + call) | `42` | 0 |
| `tests/test_func_val.bbc` | first-class free function values (`let f = double; f(5)`) | `10\n42\n7` | 0 |
| `tests/test_float.bbc` | float literals, arithmetic (+/-/*/÷), comparisons, println | `5 2.5 4.6875 3 false true false true true false` | 0 |
| `tests/test_multi_tp4.bbc` | generic List<T> + Pair<A,B>, multi-specialization, buffer alloc in generic fn | `21 false \xc3 99 10 11` | 0 |
| `tests/test_gc_deep_calls.bbc` | deep call chain passing `[Int]` buffers; intermediate allocs freed at each frame | `33 56 85` | 0 |
| `tests/test_gc_struct_heap.bbc` | struct with `[Int]` field; repeated makeNode/reassign tests gc_dec balance | `33 86 61 182` | 0 |
| `tests/test_gc_buf_of_structs.bbc` | `Buffer<Pair<Int,Int>>`; fill + overwrite verifies element gc_dec on slot overwrite | `30 60` | 0 |

---

## Important notes

The code should be very segmented and reusable, a lot of refactoring to do.
Plan all actions.
Really care about where the GC should be called.
The pseudo code/IR can also be changed for something more usable and flexible. Get inspired by what llvm do.

It is important for the codegen to have the same behaviour as the interpreter.

## Overview

The BBC compiler has three execution paths sharing a common front-end:

```
Source → Parser → AST → Analyser → { Interpreter (works) | Codegen (incomplete) }
```

The **interpreter** is the reference implementation and is largely complete, including a full reference-counting GC. The **codegen** (AST → x86-64 assembly) is an early skeleton with numerous critical gaps.

---

## What Works (updated 2026-05-16, all 38 codegen tests passing — EU auto-upgrade + GC stress tests done)

| Component | Status |
|-----------|--------|
| Lexer / Parser | Complete |
| AST definitions | Complete |
| Analyser (type checker, inference, traits, generics) | Complete |
| Interpreter + GC | Complete |
| Codegen: intLit, boolLit, charLit | Done |
| Codegen: floatLit | Done — stores f64 bit-pattern in GP reg via XMM0; constants in .data |
| Codegen: nullLit, NULL | Placeholder (emits 0) |
| Codegen: stringLit | Done (malloc + header + char array) |
| Codegen: scope stack allocation (no GC) | Done |
| Codegen: binop `+` / `-` / `*` / `/` / `%` | Done |
| Codegen: binop comparisons (`<` `>` `<=` `>=` `==` `!=`) | Done |
| Codegen: binop logical `And` / `Or` (short-circuit) | Done |
| Codegen: assignment (stack + buffer index + struct field) | Done, no GC |
| Codegen: identifier load/store-address | Done (stack vars + function labels via `lea reg, [func_label]`) |
| Codegen: if/elif/else | Done |
| Codegen: whileLoop | Done |
| Codegen: forLoop | Done (iterator protocol via synthetic method calls) |
| Codegen: notOp | Done (`.xor` instruction in Instruction union) |
| Codegen: Print / println (Int, Bool, Char) | Done |
| Codegen: Print for String types | Done (r12/r13/rbx loop over chars) |
| Codegen: Print for Buffer types | Done (loop over elements, Int/Char/Bool dispatch) |
| Codegen: function call (ABI-correct, register + stack args) | Done, no GC |
| Codegen: struct init (malloc, field store, refcount=1) | Done, no GC |
| Codegen: field access / address (unaryOperatorRight) | Done |
| Codegen: first-class bound methods (heap object: _count+receiver+free_fn) | Done — `t.method` as a value allocates a 24-byte `BoundMethod{_count,recv,recv_free_fn}` on the heap; call site loads receiver from `[bm+8]`, pushes as self; **GC Section F done**: temporary bm objects are gc_inc'd before call and gc_dec'd after (with rax saved around the gc_dec); variable bm objects handled by scope exit as usual |
| Codegen: frame size pre-computation (fixed-frame prologue) | Done — `countSlotsInScope` walks entire AST; single `sub rsp, aligned_total*8` in prologue; `frame_is_fixed`/`frame_slot_cursor` used by scope.zig and forloop.zig; eliminates body-scope re-reservation hack |
| Codegen: bufferAlloc | Done — calloc + header; size-reg clobber fix applied (see Critical Bug 19bis) |
| Codegen: bufferLit | Done, no GC |
| Codegen: bufferIndex (read + address) | Done — bounds-checked (sets `bbc_error_flag` on OOB) |
| Codegen: bufferIndex as LHS of assignment | Fixed — `codegenValueAdr` now handles `.bufferIndex` |
| Codegen: bufferIndex type dispatch | Done — `.name` user struct → calls `.index` method; Buffer/String → raw bounds-checked load |
| Codegen: field offset for generic struct types | Fixed — `getFieldOffset` now resolves `.generic` type vars via `ensureGenericSpecialization` |
| Codegen: freeKeyword | Done |
| Codegen: errorCheck | Done — `bbc_make_error_fn` allocates ErrorObj{_count,error_code} on heap; `? err` extracts code and clears flag |
| Codegen: division by zero guard | Done — checks divisor before idiv, sets error flag, skips division |
| Codegen: error propagation (early return) | Done — error-propagating functions (`!T`) return early on flag set |
| Codegen: uncaught error at main | Done — `_main` checks flag, prints "Uncaught error", exits 1 |
| Codegen: `? err` suppresses inner early-returns | Fixed — `in_error_check_expr` flag prevents premature early exit inside `value ? err fallback` |
| `XOR` instruction in Instruction union | Added |
| `SHL` instruction in Instruction union | Added |
| IR Phase 3a: `IMUL`, `CQO`, `IDIV_MEM` instructions | Added — replaces all `inlineAsm` for multiply/divide in binop.zig |
| IR Phase 3a: `ALLOC` / `DEALLOC` instructions | Added — replaces `MALLOC`/`FREE` (also fixes `FREE` bug: `rdi` + `_free`) |
| IR Phase 3a: `MALLOC` / `FREE` instructions | Deleted — replaced by `ALLOC`/`DEALLOC` |
| IR Phase 3a: `inlineAsm` for `sub/add rsp, 8` alignment | Eliminated — replaced by `.minus`/`.plus` instructions throughout (including inbuilt.zig) |
| `inlineAsm` field in `Instruction` union | **Deleted** — was emitted by zero production code; union now has no dead variants |
| IR Phase 3b: `GC_INC` / `GC_DEC_NO_CHECK` / `GC_DEC` instructions | Added — GC ops are now first-class IR nodes; `gc.zig` helpers are thin wrappers |
| IR Phase 3c: `CALLWithArgs` instruction | Deleted — replaced by inline `LOAD`/`PLUS`/`PUSH`/`CALL` in funcall.zig and bufferindex.zig |
| IR Phase 3c: `GenerateFunctionFrame` instruction | Deleted — replaced by inline `PUSH`/`LOAD`/`MINUS`/`STORE` in funcdef.zig |
| Codegen: scope-exit GC (Section B) | Done for control-flow scopes (while/for/if bodies) — heap locals gc_dec'd before RSP restore; return-var identity protected via save/restore of last_used; function-body GC (args + Section C) deferred |
| GC: discarded heap temporaries | Fixed — `codegenScope` now gc_decs any heap-typed result register left live after a non-final statement (e.g. `foo()` as a discarded statement) |
| GC: return-variable heuristic unified | `returnVarName()` helper computes the skip-name once; `emitScopeGcExit` takes `skip_name: ?[]const u8` param; funcdef reuses it for Case B check — duplicate AST inspection removed |
| RegisterTable: `last_used` stale-pointer invariant | `free()` now clears `last_used` when it matches the freed index — prevents the discard path from double-freeing a register the callee already freed |
| Codegen: varDec | no-op in dispatch (stack slot pre-allocated by scope.zig) |
| Register allocator with spilling | Done |
| MALLOC instruction (rdi fix) | Fixed |
| FREE instruction (_free + rdi fix) | Fixed |
| CALLWithArgs instruction | Legacy; still in enum but new funcall.zig bypasses it |
| macOS entry point (`_main` wrapper) | Fixed |
| Struct destructor (`global.TName.free`) | Fixed — correct self offset (rsp+32), push order (free_fn first), `_free(self)` at end |
| `global.String.free` destructor | Added — flat alloc, just calls `_free(self)` |
| `global.Buffer.free` destructor | Done — flat free (no element GC) |
| All AST nodes wired into `value.zig` dispatch | Fixed |
| `CreateTypeError` in `types.zig` | Added — alias for Int type, used by errorcheck.zig to bind err_name variable |
| End-to-end test (test4.bbc) | **PASSING** — all 5 expected values correct |
| End-to-end test (test5.bbc) | **PASSING** — string iterator, for-loop, char print working |
| End-to-end test (test6.bbc) | **PASSING** — infinite GC loop (constant memory) |
| End-to-end test (tests/buf_test.bbc) | **PASSING** — buffer alloc/lit/index/assign working |
| End-to-end test (tests/test_err_div.bbc) | **PASSING** — division guard + error check working |
| End-to-end test (tests/test_generics.bbc) | **PASSING** — generic type inference from `.generic`-typed args fixed |
| End-to-end test (tests/test_recursion.bbc) | **PASSING** — caller-save fix + _main always exits 0 on success |
| End-to-end test (tests/test_nested_while.bbc) | **PASSING** |
| End-to-end test (tests/test_struct_methods.bbc) | **PASSING** |
| End-to-end test (tests/test_bool_ops.bbc) | **PASSING** |
| End-to-end test (tests/test_arithmetic.bbc) | **PASSING** |
| End-to-end test (tests/test_buf_types.bbc) | **PASSING** |
| End-to-end test (tests/test_string_concat.bbc) | **PASSING** — String + Char concat working |
| Codegen: String + Char (`+` operator) | Done — `bbc_string_append_char` always-allocate helper; GC-correct (new string at `_count=0`) |
| Analyser: `containFunction` duplicate version bug | Fixed — was `return false` on first type mismatch in list, now uses `continue :outer`; prevents same generic specialization from being compiled multiple times |
| Analyser: struct init type param binding strips error flag | Fixed — `!T` field values now bind `T` (not `!T`) to type params; error propagates to struct return type |
| Interpreter: String + Char COW bug | Fixed — COW path now returns `new_str` instead of `lhs` when `references != 1` |
| Codegen: Char print via putchar (null-safe) | Done — replaces printf("%c") which asserted on macOS when char==0 |
| Codegen: caller-save live regs around calls | Done — funcall.zig pushes all live reg-table values before call, pops after |
| Method `self` implicit argument handling | Fixed (funcdef.zig detects `parent != null`) |
| Namespace-qualified calls vs method calls | Fixed (funcall.zig checks import_ns receiver type) |
| `idiv` clobbers rdx (division by zero false positive) | Fixed (push divisor before cqo) |

---

### 19. ~~`[String]` / `[MyStruct]` buffer destructors missing~~ — FIXED

`getFreeFnLabel` now returns `global.Buffer_T_.free` (specialized per element type) for any buffer whose element type is heap-managed (String or user struct). `generateProgram` collects all needed element types from struct fields and function signatures, then emits a loop destructor for each that calls `global.object.free` on every non-null element before freeing the outer block. The struct destructor's buffer-field case is updated to use the same specialized labels.

### 20. ~~Error path leaks heap locals and args~~ — FIXED

`emitEarlyReturnOnError` now jumps to a per-function `func_error_exit` label instead of doing a bare `mov rsp, rbp; pop rbp; ret`. `codegenFuncdef` generates this label before `codegenScope` and sets `compiler.error_exit_label`. The error exit block restores RSP to the full frame (arg spills + body locals + 1 rax-save slot), saves rax (ErrorObj\*), runs `emitScopeGcExit` for body locals, GC-decs all heap-typed args, restores rax, then does the frame epilogue.

---

## Critical Bugs

### 1. ~~`MALLOC` uses wrong argument register~~ — FIXED

`MALLOC.toAsm` now correctly uses `rdi`.

### 2. ~~`FREE` calls `_malloc` instead of `_free`~~ — FIXED

`FREE.toAsm` now calls `_free` and passes pointer in `rdi`.

### 3. ~~`codegenIdentifier` for functions loads `0`~~ — FIXED

`identifier.zig` now searches `compiler.func_uid_list` for the first matching function name and emits `lea reg, [fname@N]` (the correct label). Non-generic functions always resolve to version 0. First-class generic function values remain unresolved but no longer crash.

### 4. ~~`CALLWithArgs` — non-standard method dispatch~~ — FUNCTIONALLY FIXED

`funcall.zig` now uses the correct System V AMD64 ABI (args in rdi/rsi/rdx/rcx/r8/r9). The `callwithargs` instruction type still exists in the enum but is no longer emitted by new code. It can be deleted once confirmed unused.

### 5. ~~Stack-size counter not reset between functions~~ — FIXED

`compiler.stack_size` is now reset to `0` at the start of each `codegenFuncdef`. Without this, later functions inherited the counter from earlier ones, producing wrong `[rbp - N]` offsets for their arguments.

### 6. ~~Assignment clobbers LHS address across calls~~ — FIXED

`codegenAssignement` used to compute the LHS address (into a caller-saved register such as `rcx`) *before* evaluating the RHS. Any function call in the RHS was free to overwrite `rcx`, corrupting the destination address. Fixed by swapping the order: evaluate RHS first, then compute LHS address (which only emits `lea` — no calls).

### 7. ~~macOS entry point not found by linker~~ — FIXED

BBC's `main` function is now labelled `bbc.main`. The C runtime wrapper is labelled `_main` (exported via `global _main`), calls `bbc.main` with a proper stack frame (`push rbp; call bbc.main`), and returns via `ret`. Previously the wrapper used a raw syscall exit; now it returns normally to the C runtime, which handles exit.

### 8. ~~Method `self` not saved to stack frame~~ — FIXED

`funcdef.zig` now detects methods via `function_def.parent != null`. When true, it adds 1 to `total_args` passed to `GenerateFunctionFrame` (so `rdi` gets saved to `[rbp-8]`), then assigns stack index 1 to `"self"` before processing explicit arguments. All explicit arg indices shift up by 1.

### 9. ~~`idiv` clobbers rdx, causing false division-by-zero SIGFPE~~ — FIXED

`cqo` sign-extends rax into rdx:rax, zeroing rdx. If the divisor lived in rdx, `idiv rdx` then divided by 0. Fixed by pushing the divisor onto the stack before `cqo` and using `idiv QWORD [rsp]`, then cleaning up with `add rsp, 8`.

### 10. ~~Namespace-qualified calls treated as method calls~~ — FIXED

When a function is imported from a namespace (e.g. `Strings.stringIterator`), the AST represents the call as `unaryOperatorRight{expr: Strings, op: stringIterator}`. `funcall.zig` was treating this as a method call (`is_method=true`), adding an extra implicit `self` arg (the namespace object). Fixed by checking if the receiver's type is `.import_ns`; if so, `is_method = false`.

### 11. ~~Frame size mismatch~~ — FIXED

`scope.zig` now saves `compiler.stack_size` before pre-allocating scope locals and restores it after the matching `add rsp, size*8`. Previously, `stack_size` grew by `size` each time a control-flow scope (while/for/if body) executed, causing GC alignment checks to see a wrong offset on subsequent loop iterations. The `funcdef.zig` save/restore (`pre_body_stack_size`) was already in place for the top-level function body; the fix extends this invariant to all nested scopes.

### 12. ~~`AccessingFreedValue` during codegen of `sumBuffer`~~ — FIXED

Was caused by missing dispatch cases hitting `else => unreachable` (UB in ReleaseFast corrupting `last_used`). All nodes now correctly wired. test4.bbc passes.

### 14. ~~Method functions used wrong stack slot for `self`~~ — FIXED

For methods defined in `implement` blocks, `self` is implicit (not in `func.arguments`). Previously `func.signature.argtypes.items.len` didn't include self, so `GenerateFunctionFrame` didn't store rdi, and no stack index was assigned. Fixed in `funcdef.zig`: detect `function_def.parent != null`, add 1 to frame argnum, assign self to slot 1 in both function ctx and body scope ctx.

### 15. ~~`funcall.zig` crashed on namespace function calls~~ — FIXED

`codegenValue(f.func)` for namespace-qualified calls like `Strings.stringIterator` invoked `codegenUopRight` which called `cctx.getVariable("Strings")` on a non-variable → UB/panic. Fixed in `funcall.zig`: detect method calls via base type `!= .import_ns`; for namespace/free calls push dummy 0 as self; for method calls push the receiver object.

### 16. ~~Generic type names contain NASM-invalid characters~~ — FIXED

Function labels for generic specializations like `Strings.StringIterator<Char>.next@0` contain `<>` which are invalid in NASM. Fixed by sanitizing label names in `funcdef.zig` (UID generation), `codegen.zig` (struct destructor labels), and `gc.zig` (free function label lookup).

### 19 (bis). ~~`bufferAlloc` writes `_size = 1` instead of actual size when size register is rdi~~ — FIXED

`bufferalloc.zig` sets up calloc arguments as `rsi = (size*8+16)` (byte count) and `rdi = 1` (nelem). When the register allocator happened to assign `size_reg = R5` (rdi), the `mov rdi, 1` instruction overwrote the original size before the caller-saved save loop ran. The loop then saved/restored the value `1`, so after calloc the register held `1` not the real size. Buffer `_size` was consequently set to `1` for all buffers allocated in generic specializations where register pressure forced size into rdi.

Fix: explicitly push `size_reg` to the stack *before* any calloc argument setup. After calloc + caller-save restore, pop into a fresh register and use that for the `buffer._size` store. This is independent of register table state.

Symptom: `tests/test_multi_tp4.bbc` SIGSEGV — buffer `_size = 1` caused bounds checks to fail at index 1, triggering `bbc_make_error_fn` which clobbered `rdx` (a live Pair struct pointer), leading to a null pointer write.

### 13. ~~ErrorChecking does not return/stops the current frame when an error is found~~ — FIXED

Early return on error flag is emitted by `scope.zig` after each non-final statement in functions with `!T` return types. `_main` wrapper prints "Uncaught error" and exits 1. Verified working.

### 17. ~~`getFieldOffset` returned 0 for fields of generic struct variables~~ — FIXED

When a variable `lst: List<Int>` is stored in the analyser context, its type is `{.decided = {.base = {.generic = {.name = "List", .params = [Int]}}}}` — a `.generic` variant, not `.name`. `getFieldOffset` only handled `.name`, so all fields of generic struct variables resolved to offset 0. Fixed by adding a `.generic` case in `unaryOperatorRight.zig` that calls `ensureGenericSpecialization` to resolve the spec name and then does a normal `getStructHabitantIndex` lookup. Also changed `getFieldOffset` signature to `!i64` and added `allocator` parameter.

### 18. ~~Early return inside `? err` expression exited function before handler could catch error~~ — FIXED

In error-propagating functions (`!T`), `codegenSubscriptionIndex` and `codegenRawBufIndex` emit an early return when `bbc_error_flag=1`. When the subscript expression was inside `value ? err fallback`, the early return fired before the `? err` handler could clear the flag and substitute the fallback. Same issue applied to division/modulus in `binop.zig`. Fixed by adding `compiler.in_error_check_expr: bool` to `Compiler`. `codegenErrorCheck` sets this flag while evaluating `errcheck.value`; all error-producing nodes check `!in_error_check_expr` before emitting early returns.


---

## Missing Codegen

| AST node | Status | Notes |
|----------|--------|-------|
| `floatLit` | Empty stub | needs SSE register allocation |
| `stringLit` | Missing | needs `malloc` + `StringObj` header init + ref count |
| `nullLit` | Missing | trivial immediate 0 |
| `NULL` | Missing | trivial immediate 0 |
| `forLoop` | Missing | requires iterator protocol (`isLast`, `next` method calls on iterable) |
| `errorCheck` | Done | flag check + fallback scope; `? err` correctly catches errors from subscript/div in `!T` functions |
| `function` (as value) | Done | `codegenFunctionValue` emits `lea reg, [func_label]`; `codegenIdentifier` does the same for function-name identifiers; `analyseValue` no longer calls `addFunctionToCompile` (funcall analysis at line 688 handles queuing with the resolved version) |
| Print for Buffer/String | Missing | needs loop over elements |

---

## GC — Not Integrated Into Codegen

The GC is fully specified in `garbagecollector.md` and fully implemented in the interpreter (`src/interpretor/values.zig`). **Zero GC calls are emitted by the codegen.** The required sites are:

### A. Variable assignment (`codegenAssignement`)

```
; pseudocode for: let x = expr  /  x = expr
emit: codegenValue(rhs)         → rhs_reg
emit: CALL gc_increment(rhs_reg)
if slot already holds a value:
    emit: LOAD old_val = [rbp - slot*8]
    emit: CALL gc_decrement(old_val)
emit: STORE [rbp - slot*8], rhs_reg
```

### B. Scope exit (`codegenScope` epilogue)

After all statements in a scope, before restoring RSP, emit a `gc_decrement` for every variable slot that holds a heap pointer. The type information from the analyser's `Context` determines which slots need it.

```
for each variable in scope.ctx.variables:
    if type is heap-allocated (String, Buffer, Object, Namespace, Error):
        emit: LOAD val = [rbp - slot*8]
        emit: CALL gc_decrement(val)
```

### C. Return value protection (`codegenFuncdef` epilogue)

Before destroying the function scope, protect the return value:

```
emit: CALL gc_increment(return_val)   ; protect from scope teardown
emit: [scope teardown / gc decrements for all locals — see B]
emit: CALL gc_dec_no_check(return_val)   ; restore to caller-owned count
emit: mov rax, return_val
emit: epilogue (pop rbp, ret)
```

### D. Struct init (`codegenStructinit`)

The `_count` field is initialized to `1` — **already done**.

### E. Container element write

When writing to a struct field or buffer slot, swap semantics (increment new, decrement old) must be emitted inline.

### F. Bound method values

When a method is looked up via `.` on an object, the bound function wrapper increments the parent object. The call site must emit a decrement after the call returns, unless the return value is `self`.

### G. `freeKeyword` AST node

**Already lowered to a `global.object.free` call** in `freekeyword.zig`. No GC stub needed here.

---

## IR / Instruction Set

### Current state (post Phase 3a/3b/3c + inlineAsm elimination)

The `Instruction` union (`src/codegen/instructions.zig`) is a two-layer IR: typed, named BBC IR nodes whose `toAsm` methods are the x86-64 lowering pass. **`inlineAsm` is fully eliminated from all call sites** — the field still exists in the union but is emitted by zero production code.

| Problem | Resolution |
|---------|-----------|
| `inlineAsm` for arithmetic | `IMUL`, `CQO`, `IDIV_MEM` first-class instructions |
| `MALLOC`/`FREE` x86 stubs | Replaced by `ALLOC`/`DEALLOC` (correct ABI, `rdi`, `_free`) |
| GC as raw call sequences | `GC_INC`, `GC_DEC_NO_CHECK`, `GC_DEC` first-class IR nodes |
| `CALLWithArgs` legacy dispatch | Deleted — inlined as `LOAD`/`PLUS`/`PUSH`/`CALL` in funcall.zig |
| `GenerateFunctionFrame` legacy | Deleted — inlined as `PUSH`/`LOAD`/`MINUS`/`STORE` in funcdef.zig |
| `sub/add rsp, 8` as inlineAsm | Replaced by `.minus`/`.plus` instructions throughout |
| Error-flag reads/writes/compares | `Dval.rip_memory` + `Dadr.rip_memory` → `QWORD [rel bbc_error_flag]` |
| Buffer/string element indexed loads | `Dval.sib` (`SIB_Address`) → `QWORD [base + index*scale + disp]` |
| Print loop raw asm (r12/r13/rbx) | Decomposed into typed `PUSH`/`POP`/`LOAD`/`INC`/`CMP`/`XOR` + `emitAlignedCall` |
| `_main` wrapper raw asm | Replaced by `.cmp`/`.lea`/`.load`/`.xor` |

### Current `Instruction` union (clean set)

```
Data movement:  LOAD, STORE, LEA, PUSH, POP
Control flow:   LABEL, JMP, JCond, CALL, RET, EXIT
Arithmetic:     PLUS, MINUS, INC, DEC, IMUL, CQO, IDIV_MEM, XOR, SHL
Comparison:     CMP
GC:             GC_INC, GC_DEC_NO_CHECK, GC_DEC
Heap:           ALLOC, DEALLOC
(inlineAsm field exists but is emitted by zero production code)
```

### New `Dval` / `Dadr` variants

| Variant | Syntax | Emits |
|---------|--------|-------|
| `Dval.rip_memory` | `.{ .rip_memory = "sym" }` | `QWORD [rel sym]` |
| `Dadr.rip_memory` | `.{ .rip_memory = "sym" }` | `QWORD [rel sym]` |
| `Dval.sib` | `.{ .sib = .{ .base, .index, .scale, .disp } }` | `QWORD [base + index*scale + disp]` |

### Remaining future IR work

- **Typed register states**: add `bbc_type` to `RegisterTable.RegisterState` so GC decisions don't need to re-query the analyser context at every scope exit
- **Float/SSE**: ✅ done — `FLOAT_LIT`/`FLOAT_BINOP`/`FLOAT_CMP_FLAGS`/`FLOAT_PRINT`; floats live in GP regs as bit-patterns; XMM used transiently only
- **SSA / phi nodes**: not planned — current branch-to-label control flow is correct and sufficient

---

## Calling Convention

### Correct System V AMD64 ABI (now implemented in funcall.zig)

Argument registers in order: `rdi`, `rsi`, `rdx`, `rcx`, `r8`, `r9`. Extra args on stack right-to-left. Caller cleans the stack. Return value in `rax`.

`funcall.zig` now implements this correctly: evaluates all args, pushes to stack in reverse order, pops into argument registers, calls, cleans up. Method calls pass `self` as the first positional argument.

### Method calls

A method call `obj.method(arg1, arg2)` lowers identically to a free function call with `self` as the first argument:

```
; obj.method(arg1, arg2)  →  method(obj, arg1, arg2)
mov rdi, obj_ptr
mov rsi, arg1
mov rdx, arg2
call method_label
```

---

## Refactoring Plan

### Phase 0 — Bug fixes (unblock existing tests)

1. ✅ Fix `MALLOC`: `rsi` → `rdi`
2. ✅ Fix `FREE`: `_malloc` → `_free`, `rsi` → `rdi`
3. ✅ Fix function identifier codegen: emit `lea` not `mov 0` (identifier.zig)
4. ✅ Fix `codegenStructinit` ref count: initialize `_count` word to `1`
5. ✅ Funcall now uses ABI-correct standard calling convention

### Phase 1 — Complete missing nodes

6. ✅ `charLit` (trivial immediate)
7. ✅ `floatLit` — `FLOAT_LIT` IR node; loads constant from .data via `movsd xmm0, [rel label]; movq gp, xmm0`
8. ✅ `nullLit` / `NULL` (emit immediate 0)
9. ✅ `stringLit` (alloc + header init)
10. ✅ `notOp` (XOR with 1)
11. ✅ `Print` / `println` (Int, Bool, Char)
12. ✅ Print for Buffer types (loop, Int/Char/Bool element dispatch)
13. ✅ `unaryOperatorRight` field load + address
14. ✅ Full binop set: `+` `-` `*` `/` `%` `<` `>` `<=` `>=` `==` `!=` `&&` `||`
15. ✅ `bufferAlloc` — malloc + header init
16. ✅ `bufferLit` — element-by-element init
17. ✅ `bufferIndex` — bounds-unchecked load + address
18. ✅ `whileLoop` — condition label + back-edge jump
19. ✅ `forLoop` — iterator protocol (`isLast` / `next` method calls)
20. ✅ `freeKeyword` — calls `global.object.free`
21. ✅ `errorCheck` — global flag checked after each error-producing operation; fallback scope executes with `err_name` bound to 0; `in_error_check_expr` prevents premature early return inside the checked expression.

### Phase 2 — GC integration

22. ✅ Add `gc_inc` / `gc_dec` / `gc_dec_no_check` — now first-class IR nodes (`GC_INC`, `GC_DEC`, `GC_DEC_NO_CHECK`)
23. ✅ ~~Add type-tag lookup to `Compiler`~~ — `RegState.bbc_type: ?*Ast.Type` field added to `RegisterTable`; `setType`/`getType` methods exposed; `codegenScope` populates the tag for each discarded register so downstream code can query the BBC type from the register index alone without re-querying `cctx`
24. ✅ Integrate GC at assignment sites (section A)
25. ✅ Integrate GC at scope exit (section B)
26. ✅ Integrate return-value protection at function boundaries (section C)
27. ✅ Container element swap semantics (section E)
28. ✅ Bound method decrement at call site (section F) — done: temporary bm gc_inc'd before call, gc_dec'd after with rax-save/restore; variable bm handled by scope exit

### Phase 3 — IR redesign ✅ (complete)

29. ✅ Promoted `IMUL`, `CQO`, `IDIV_MEM` — first-class integer arithmetic instructions
30. ✅ Replaced `MALLOC`/`FREE` with `ALLOC`/`DEALLOC` — correct ABI (`rdi`, `_free`)
31. ✅ Promoted `GC_INC`, `GC_DEC_NO_CHECK`, `GC_DEC` — GC as first-class IR nodes; `toAsm` is the lowering pass
32. ✅ Retired `CALLWithArgs`, `GenerateFunctionFrame`, `MALLOC`, `FREE`
33. ✅ Float/SSE — `FLOAT_LIT`, `FLOAT_BINOP`, `FLOAT_CMP_FLAGS`, `FLOAT_PRINT` IR nodes; floats stored as bit-patterns in GP registers; XMM used transiently for arithmetic and printf

### Phase 4 — Correctness hardening

34. ✅ ~~**Frame size pre-computation**~~: `countSlotsInScope` recursively sums all local slots across all nested scopes; prologue emits a single `sub rsp, aligned_total*8`; `frame_is_fixed` + `frame_slot_cursor` on `Compiler` allow scope.zig and forloop.zig to claim slots without adjusting RSP
35. Cycle detection: mark-and-sweep pass or prohibit cycles at the type level
36. ✅ ~~**SSE/float**~~ — done: floats stored as 64-bit bit-patterns in GP registers; XMM used transiently at arithmetic/print boundaries; `FLOAT_LIT`/`FLOAT_BINOP`/`FLOAT_CMP_FLAGS`/`FLOAT_PRINT` IR nodes; `%g` format string; float constants emitted as `dq` in .data. 34 tests pass.
37. Error-union layout: canonical memory representation for `!T` values (tag word + payload word)
38. ✅ ~~Generic specialization codegen: confirm each `functionVersion` is compiled exactly once~~ — `analyser.containFunction` correctly deduplicates generic function versions before codegen; `generateProgram` iterates the already-deduplicated `cctx.functions_to_compile` list; each UID is unique. Confirmed by test_multi_tp4.bbc which exercises 4 distinct generic specializations without duplication.
39. ✅ ~~Remove dead instructions~~ — done: `CALLWithArgs`, `GenerateFunctionFrame`, `MALLOC`, `FREE` all deleted; `inlineAsm` field now also deleted (was emitted by zero production code)
40. ✅ ~~**`bufferAlloc` size clobbering**~~ — done: push `size_reg` before calloc arg setup; pop into fresh register after restore; use saved value for `buffer._size`. Fixes SIGSEGV in test_multi_tp4.bbc. 35 tests pass.
41. ✅ ~~**GC leak testing automated**~~ — `./run_tests.sh --leaks` mode added: compiles each test to a native binary, runs under `leaks --atExit -- arch -x86_64 ./bin`; exit code 0 = clean, non-zero = leak detected with summary. All 35 tests confirmed CLEAN.
42. ✅ ~~**`RegState.bbc_type` type-tag**~~ — `bbc_type: ?*Ast.Type` added to `RegState`; `setType`/`getType` on `RegisterTable`; populated by `codegenScope`'s discard path after each `getTypeOfValue` call. Downstream code can now read the BBC type from a register index without the original AST node or analyser context.
43. ✅ ~~**`inlineAsm` field removed from `Instruction` union**~~ — field and its `toAsm` arm deleted; all emission was already eliminated in Phase 3a; union is now clean.
44. ✅ ~~**EU auto-upgrade in `analyseFunction`**~~ — when body's `actual_ret.err=true` but declared return type has `err=false`, `func.return_type.err` is promoted to `true` and `inferTypeScope` receives the error-union version. This makes `Int + !Int → !Int`, `!Int + Int → !Int`, `!Int + !Int → !Int` valid without any `? err` workaround. Same fix applied to `bbc-lsp/src/bbc/analyser.zig`.
45. ✅ ~~**GC stress tests (3 new)**~~ — `test_gc_deep_calls.bbc` (deep call chain with intermediate buffer allocs, expected 33/56/85), `test_gc_struct_heap.bbc` (struct with heap buffer field, reassignment, expected 33/86/61/182), `test_gc_buf_of_structs.bbc` (Buffer<Pair<Int,Int>> with overwrite, expected 30/60). All 3 added to `run_tests.sh`. 38 tests passing total.

---

## File Inventory and Ownership

```
src/
  main.zig                          — entry point, arg parsing
  lexer.zig / parser.zig            — front-end (complete)
  ast.zig                           — AST nodes (complete)
  analyser.zig                      — type checker (complete)
  types.zig                         — type inference helpers (complete)
  traits.zig                        — trait resolution (complete)
  errors.zig / position.zig         — diagnostics
  imports.zig                       — import resolution
  inbuilt_funcs.zig / .config       — built-in function table
  inbuilt_libs.zig                  — library loading
  interpretor/
    interpretor.zig                 — tree-walk interpreter (complete)
    valueInterpretor.zig            — value evaluation (complete)
    values.zig                      — runtime value types + GC (complete)
    objects.zig                     — Object struct
    inbuilt/                        — built-in implementations
  codegen/
    compiler.zig          — Compiler struct, label/uid management; `frame_is_fixed`/`frame_slot_cursor` for fixed-frame mode
    instructions.zig      — Instruction union → x86 text (clean; no dead instructions)
    registerTable.zig     — register allocator with spilling; `free()` clears `last_used` on match (stale-pointer invariant)
    x86.zig               — dump assembly to file
    program/
      codegenprog.zig     — module re-exports
      gc.zig              — GC helpers: emitGcInc/Dec/DecNoCheck → GC_INC/GC_DEC/GC_DEC_NO_CHECK IR nodes
      scope.zig           — scope codegen; GC exit for control-flow scopes done; discarded heap temporaries gc_dec'd; `returnVarName()` helper + explicit `skip_name` param on `emitScopeGcExit`; `countSlotsInScope`/`countSlotsInValue` for frame pre-computation
      funcdef.zig         — function codegen; GC Sections A+C done; fixed-frame prologue (single sub rsp, aligned_total*8); body-scope re-reservation hack removed
      value.zig           — AST value dispatch [else=>unreachable: floatLit only; function-as-value now dispatches to codegenFunctionValue]
      values/
        intlit.zig        — done
        boollit.zig       — done
        charlit.zig       — done
        binop.zig         — full set; uses IMUL/CQO/IDIV_MEM; no inlineAsm for arithmetic
        assignement.zig   — stack + bufferIndex + field; GC swap semantics done
        identifier.zig    — stack vars + function label load done; `codegenFunctionValue` for `Value.function` nodes; `getFuncLabel` uses `createFunctionSignature` + empty version map
        vardec.zig        — address emit only (stack slot pre-allocated by scope.zig)
        ifstatement.zig   — done
        whileloop.zig     — done
        funcall.zig       — ABI-correct; CALLWithArgs removed; inlines LOAD/CALL directly; handles bound_method receiver load from [bm+8]
        structinit.zig    — ALLOC + field store; GC Section E done
        unaryOperatorRight.zig — field load + address; handles .name and .generic variable types; bound method heap object creation for method-value accesses
        notop.zig         — done (XOR with 1)
        print.zig         — Int/Bool/Char/Buffer/String done
        freekeyword.zig   — done (calls global.object.free)
        bufferalloc.zig   — done (calloc + header)
        bufferlit.zig     — done (element-by-element)
        bufferindex.zig   — done (bounds-checked; CALLWithArgs removed; inlines LOAD/CALL directly)
        errorcheck.zig    — done; sets in_error_check_expr to suppress inner early returns
```

---

## GC Implementation Checklist (Codegen)

Mirrors the spec in `garbagecollector.md`, translated to codegen sites:

- [x] `codegenAssignement` emits `gc_inc(new)` on ALL assigns (including varDec first-assign) and `gc_dec(old)` on every non-first assign — covers plain stack vars, struct fields, and buffer indices uniformly. Creation sites (structinit/stringlit/bufferalloc/bufferlit) now initialize refcount=0 so that the first gc_inc (from assignment) brings the count to 1, matching the interpreter's semantics exactly.
- [x] `codegenScope` epilogue emits `gc_dec` for heap-type locals before restoring RSP — implemented for control-flow scopes (while/for/if bodies, `skip_gc_exit=false`). Function-body scopes pass `skip_gc_exit=true` — function-level GC (Section A for args + Section C return-value protection) is handled by funcdef.zig.
- [x] `codegenScope` discards heap-typed results from non-final statements — after each non-final statement, if a result register is left live, its type is checked and `gc_dec` is emitted before the register is freed. Prevents leaks from discarded heap temporaries (e.g. `foo()` used as a statement, not assigned).
- [x] `emitScopeGcExit` takes explicit `skip_name: ?[]const u8` parameter — return-variable heuristic moved to `returnVarName()` helper; called once in both `codegenScope` and `codegenFuncdef` to eliminate duplicate AST inspection and make the skip decision explicit.
- [x] `codegenFuncdef` GC Section A (gc_inc heap args at entry) + Section C (return-value protection: gc_inc ret, scope teardown, gc_dec args, gc_dec_no_check ret) — done. Fixed-frame prologue (frame size pre-computation) eliminates the need for body-scope re-reservation; GC alignment stable because `stack_size = aligned_total_slots` (even) throughout.
- [x] `codegenStructinit` initializes ref-count field to `1` (caller owns); GC Section E: gc_inc on each heap-typed field at struct creation so the struct owns those references and the destructor's gc_dec is balanced.
- [x] `codegenFuncall` emits `gc_dec` for bound method heap object after call — temporary bm (`t.method(args)`) gets gc_inc'd before the call and gc_dec'd (via `global.BoundMethod.free`) after all caller-save registers are restored; rax saved/restored around the gc_dec call to preserve the function return value; variable bm (`bm(args)`) relies on scope-exit gc_dec as usual
- [x] Container field write: swap semantics (`inc new`, `dec old`) in struct field assignment
- [x] `bufferIndex` assignment: swap semantics
- [x] `freeKeyword` lowers to `global.object.free` call
- [x] String concatenation: `bbc_string_append_char` always allocates new (COW optimization deferred; correctness confirmed via GC)
- [x] Error-scope setup: `? err` handler calls `_free(errptr)` after extracting the error code — ErrorObj is singly owned, so direct `_free` suffices
- [x] Short-circuit `And`/`Or`: `&&`/`||` operands are always `Bool` (not heap-allocated); both sides are evaluated before the branch but no heap pointer is involved — no double-free risk. Verified.
- [x] `gc_dec_no_check` is only emitted where the caller provably holds another reference — two call sites in funcdef.zig: (1) always: undoes the protective `gc_inc` (refcount N+1→N, N≥1 from assignment); (2) when `return_var_name != null`: undoes the assignment `gc_inc` that `emitScopeGcExit` skipped via `skip_name` (refcount N→0, object handed to caller at count=0; their assignment gc_incs to 1). `gc_dec_no_check` never calls the destructor so count=0 is safe. Verified.

---
### Test GC
Find a way to test GC, maybe use `leaks --atExit ...`

## Next Actions

In priority order:

1. ✅ ~~**GC Section E**~~ — done: swap semantics on struct field and buffer index assignment (gc_inc new, gc_dec old). Also fixed `stringlit.zig` save/restore bug exposed by this work.
2. ✅ ~~**Print for Buffer types**~~ — done: `[elem0, elem1, ...]` loop with Int/Char/Bool dispatch; format strings added to `.data`.
3. ✅ ~~**Buffer index bounds checking**~~ — done: `codegenRawBufIndex` emits unsigned `cmp idx, [buf+8]; JAE oob` before the load; sets `bbc_error_flag=1` on OOB. Tests `test_err_buf.bbc` and `test_err_str.bbc` pass.
4. ✅ ~~**Subscription trait dispatch (`obj[i]` for user structs)**~~ — done: `codegenBufferIndex` dispatches on type — user structs with `.index` method → `codegenSubscriptionIndex` (method call), Buffer/String → raw bounds-checked load.
5. ✅ ~~**Generic struct field offset bug**~~ — done: `getFieldOffset` now handles `.generic` type variables correctly (Bug #17).
6. ✅ ~~**`? err` swallowed by inner early return in `!T` functions**~~ — done: `in_error_check_expr` flag suppresses early returns inside `value ? err fallback` (Bug #18).
7. ✅ ~~**Improve error representation**~~ — done: `bbc_make_error_fn` allocates a 16-byte `ErrorObj{_count, error_code}` on the heap; error sites call it and store the ptr in rax; `bbc_error_flag` signals the error; `? err` handler pops the ptr, extracts the error code, clears the flag, and binds `err_name` to the code.
8. ✅ ~~**Generic type inference for `.generic`-typed arguments**~~ — done: `getFuncallVersion` and `inferTypeFuncall` now handle the case where an argument is stored as `.generic` (e.g. `Pair<Int>` returned by a generic function) — type params are bound directly from matching generic params. Fixes `test_generics.bbc`.
9. ✅ ~~**GC Section B for control-flow scopes**~~ — done: `codegenScope` now emits `gc_dec` for heap-typed locals before RSP restore when `skip_gc_exit=false`; return variable identity preserved via save/restore of `last_used`; gc_dec calls after last result register reload. Confirmed constant memory on test6.bbc (3124 KB stable over 3s). `codegenFuncdef` passes `skip_gc_exit=true` — function-body GC (Section A args + Section C return-value) is the next open item.
10. ✅ ~~**GC Section C (return value protection in functions)**~~ — done: `codegenFuncdef` emits gc_inc/gc_dec for heap args (Section A), protects return value with gc_inc + gc_dec_no_check (Section C), and emits scope GC exit for body locals. Critical fix: body scope's stack space is re-reserved before GC dec calls so pushes cannot overwrite old variable slots at [rbp-slot*8]. All 16 tests still pass.
11. ✅ ~~**GC Section E in struct init**~~ — done: `structinit.zig` now emits `gc_inc` for each heap-typed field value so the struct owns a reference. Fixes SIGABRT when a StringIterator (which holds a `String content` field) was freed — without the gc_inc, the destructor's gc_dec of `content` caused an unbalanced reference count leading to a double-free.
12. ✅ ~~**IR redesign (Phase 3)**~~ — done: `IMUL`/`CQO`/`IDIV_MEM` replace arithmetic `inlineAsm`; `ALLOC`/`DEALLOC` replace `MALLOC`/`FREE`; `GC_INC`/`GC_DEC`/`GC_DEC_NO_CHECK` are first-class IR nodes; `CALLWithArgs` and `GenerateFunctionFrame` deleted and inlined. All 31 tests pass.
13. ✅ ~~**Plain stack-variable reassignment GC**~~ — done: unified GC model — creation refcount=0, gc_inc on ALL assignments (including varDec), gc_dec(old) on reassignment. funcdef.zig Case B (named local return variable) gets an extra gc_dec_no_check so caller receives refcount=0. forloop.zig gc_inc's heap-typed iterators when storing. All 16 tests still pass; GC stress test (test6.bbc) still shows constant memory.
14. ✅ ~~**First-class bound methods**~~ — heap object `{_count @ 0, receiver @ 8, recv_free_fn @ 16}` created on `t.method` access; call site loads receiver from `[bm+8]` and passes as self; `bound_method` TypeBase variant wired through analyser, types.zig, imports.zig, traits.zig. Pending: `codegenFuncall` gc_dec of bound-method object after call (section F).
15. ✅ ~~**Error-scope setup gc_inc**~~ — done: `? err` error path now saves caller-saved registers, calls `_free(errptr)` directly, and restores. `bbc_make_error_fn` sets `_count=1`; `? err` is always the sole consumer so a direct `_free` suffices (no nested heap fields in ErrorObj). All 16 tests still pass.
16. ✅ ~~**String concatenation COW / GC**~~ — done: `bbc_string_append_char` runtime helper always allocates a new `StringObj` at `_count=0`; existing assignment GC (gc_inc new, gc_dec old) handles ownership correctly. Also fixed interpreter COW bug (was returning `lhs` instead of `new_str` in the `references != 1` path). 17 tests pass.
18. ✅ ~~**Error in mid-scope `? err` block**~~ — done: Two bugs fixed: (A) `scope.zig`'s inter-statement early-return now also checks `in_error_check_expr` and instead emits `jnz scope_err_exit` (jumps to scope cleanup, skipping remaining statements and GC exit); (B) `codegenErrorCheck` restructured to save `inner_last_used` before the flag-check allocation, check the flag first, on the error path read `ErrorObj*` directly from `rax` (which is preserved on the error-exit path), on the ok path restore `inner_last_used` before calling `lastReg()`. 24 tests pass.
17. ✅ ~~**Bug #11: stack_size drift in nested scopes**~~ — done: `codegenScope` now saves `compiler.stack_size` before pre-allocating locals and restores it after the matching `add rsp, size*8`. Prevents GC alignment checks from seeing a wrong offset on subsequent loop iterations. Fixed import path in `tests/test_forloop.bbc` (`../bbc-lib/Strings.bbc`). Added `tests/test_forloop.bbc` and `tests/test_error1.bbc` to the suite. 23 tests pass.
19. ✅ ~~**Frame size pre-computation (Phase 4)**~~ — `countSlotsInScope` walks the full AST recursively (scopes, if/while/for bodies, errorCheck, assignement RHS); prologue emits one `sub rsp, aligned_total*8`; `frame_is_fixed`/`frame_slot_cursor` added to `Compiler`; scope.zig and forloop.zig use cursor in fixed mode; body-scope re-reservation hack removed from funcdef.zig. 32 tests pass.
20. ✅ ~~**GC leak validation**~~ — done: `leaks` on live test6.bbc process shows 0 leaks; `leaks --atExit` on test_bound_method.bbc shows 0 leaks. GC is clean.
21. ✅ ~~**Float / SSE support**~~ — done: `FLOAT_LIT` loads f64 bit-pattern from .data into GP reg via xmm0; `FLOAT_BINOP` trampolines lhs/rhs through xmm0/xmm1 for addsd/subsd/mulsd/divsd; `FLOAT_CMP_FLAGS` emits `ucomisd` + unsigned jcond (B/A/BE/AE/E/NE); `FLOAT_PRINT` passes value in xmm0 with `al=1` for variadic printf (`%g`); float constants accumulated in `compiler.float_constants` and emitted as `dq` in .data. 34 tests pass.
26. ✅ ~~**First-class free function values**~~ — done: `codegenIdentifier` emits `lea reg, [func_label]` when the name resolves to a function; `codegenFunctionValue` does the same for `Value.function` AST nodes; `.function` case wired into `value.zig` dispatch; `analyseValue` no longer calls `addFunctionToCompile` for function identifiers (the funcall analysis path already handles queuing with the resolved version — adding it here caused generic functions like `safeGet<Type>` to be queued with an empty version map, failing type resolution). All 32 tests pass; `test_func_val.bbc` confirms `10 42 7` output.
25. ✅ ~~**GC Section F: bound method gc_dec at call site**~~ — done: `funcall.zig` pre-evaluates `f.func` (and loads receiver) before the live-reg snapshot for all bound_method calls; for temporary bm (`t.method(args)`), gc_inc is emitted before the call, gc_dec (with rax save/restore) after all caller-save restores; variable bm (`bm(args)`) relies on scope-exit gc_dec as usual. Verified 0 leaks via `leaks --atExit`. All 32 tests pass.
22. ✅ ~~**GC: discarded heap temporaries**~~ — done: `codegenScope` now calls `lastReg()` after each non-final statement; if the register is live and heap-typed, emits `gc_dec` before freeing. Prevents leaks from patterns like `foo()` used as a non-assigned statement. Required fixing `registerTable.free()` to clear `last_used` when it matches the freed index (invariant: `last_used` always points to a live register or null), which prevented a DoubleFree on assignment GC paths that free their own scratch registers. All 31 tests still pass.
23. ✅ ~~**GC: return-variable heuristic unified**~~ — done: `emitScopeGcExit` now takes `skip_name: ?[]const u8` instead of computing the return-variable heuristic internally. New `returnVarName()` helper computes it once. `codegenFuncdef` passes it to `emitScopeGcExit` and reuses it for the Case B `gc_dec_no_check` check — eliminates duplicate AST inspection.
24. ✅ ~~**IR: last `inlineAsm` alignment stubs in inbuilt.zig**~~ — done: `codegenErrorCall` and `codegenNamespaceInbuilt` now use `.minus`/`.plus` IR nodes for stack alignment instead of `inlineAsm "\tsub/add rsp, 8"`. Only remaining `inlineAsm` uses are error-flag writes and `bbc_make_error_fn` invocations in bufferindex.zig.
27. ✅ ~~**GC checklist audit**~~ — done: (A) `And`/`Or` short-circuit: both operands are `Bool` (not heap-allocated); no double-free risk regardless of evaluation order. (B) `gc_dec_no_check` audit: only two call sites in funcdef.zig — first always undoes the protective gc_inc (refcount N+1→N, N≥1); second (named return var only) undoes the assignment gc_inc skipped by `emitScopeGcExit` (refcount N→0, safe because gc_dec_no_check never calls the destructor; caller receives count=0 and gc_incs to 1 on assignment). Both provably correct. GC checklist fully complete.
28. ✅ ~~**`bufferAlloc` size clobbering when `size_reg` = rdi**~~ — done: explicitly push `size_reg` before calloc arg setup (`mov rdi, 1` would overwrite it); pop into fresh register after calloc + caller-save restore; use saved value for `buffer._size` init. Previously, when the register allocator assigned rdi to hold the size expression in a generic specialization, the nelem argument clobbered the saved register, causing `_size = 1` for all buffer allocations in that function. Root cause surfaced by `tests/test_multi_tp4.bbc` (SIGSEGV → rdx corrupted via failed bounds check → null write). 35 tests pass.

