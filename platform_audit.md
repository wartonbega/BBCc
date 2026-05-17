# BBC Compiler — Multi-Platform Portability Audit

**Date**: 2026-05-16  
**Goal**: Identify what must change to support architectures beyond x86-64 (ARM64/Apple Silicon first)

---

## What's Already Good

- **`Register` enum** (`src/codegen/instructions.zig`) — abstract names (R0–R13, RBP, RSP), easily extended
- **`Instruction` union** — the IR itself is mostly platform-agnostic
- **Type system, analyser, parser** — completely independent of codegen

---

## What's Hardcoded to x86-64

### Critical — must change to support any other arch

#### 1. Calling convention (`ARGUMENT_REGS`, `funcdef.zig`, `funcall.zig`)

`src/codegen/instructions.zig` defines:
```zig
pub const ARGUMENT_REGS = [_]Register{ .R5, .R4, .R3, .R2, .R6, .R7 };
// = rdi, rsi, rdx, rcx, r8, r9  ← System V AMD64 ABI, hardcoded
```
ARM64 uses `x0–x7` (8 regs) for integer args and separate `d0–d7` for floats.
`funcdef.zig` and `funcall.zig` both hardwire the "6 regs then spill to stack" rule and specific stack offsets.

#### 2. Register name mapping (`x86Reg()`)

Every `toAsm()` method on every instruction calls `.x86Reg()` directly. There is no backend dispatch — it is one concrete method that emits `"rax"`, `"rbx"`, etc. This call is embedded in ~10 value files.

#### 3. Float operations — SSE2 specific

`FLOAT_LIT`, `FLOAT_BINOP`, `FLOAT_CMP_FLAGS` all emit `movsd`, `addsd`, `ucomisd` via `xmm0/xmm1`. ARM64 floats live in `d0–d7` (NEON). The entire float register file is different. Also, the System V AMD64 ABI requires `al = number of XMM args` before variadic calls — `print.zig` hardcodes `mov al, 1` for every float print.

#### 4. Integer division — `CQO` + `IDIV_MEM`

`CQO` (sign-extend `rax→rdx:rax`) and `idiv QWORD [rsp]` are x86-64-only. ARM64 uses `sdiv` + `msub` (multiply-subtract for remainder). These are isolated instructions but need replacement.

#### 5. GC scratch register — hardcoded `rbx`

`GC_INC`/`GC_DEC` in `src/codegen/instructions.zig` directly emit `mov rbx, ...` as the scratch register. On ARM64 the callee-saved scratch registers are `x19+`.

---

### Significant — affects portability

#### 6. Assembly emitter (`src/codegen/x86.zig`)

The whole file is one concrete emitter: entry point `_main` (macOS), macOS syscall `0x2000001` for exit, NASM-specific directives (`section .data`, `QWORD`, `[rel ...]`), and all runtime helpers (`bbc_make_error_fn`, `bbc_string_append_char`, etc.) written directly in x86-64 NASM. An ARM64 backend needs a different assembler format (GNU as) and different register names throughout.

#### 7. Stack alignment logic

16-byte alignment checks (`% 16`, `sub rsp, 8` padding) are scattered across `funcdef.zig`, `funcall.zig`, and `instructions.zig`. ARM64 also requires 16-byte alignment but the frame pointer protocol differs (Apple uses an `fp`/`lr` pair on entry).

---

### Minor — needs attention

- **`build.zig`** — no `-Dtarget` flag; always assembles with NASM and links for macOS
- **Entry point** — hardcoded `_main`; Linux ELF needs `_start` or `main`

---

## Recommended Refactor Plan

### Phase 1 — Introduce `ArchTarget` + `CallingConvention`

Create a struct that packages:
- Argument register list (length varies by arch)
- Caller/callee-saved register sets
- Stack alignment constant
- Pointer size

Pass it through the compilation pipeline. This alone unblocks `funcdef.zig` and `funcall.zig` without touching the emitter.

### Phase 2 — Replace `x86Reg()` with backend dispatch

Introduce a `Backend` comptime interface (or a tagged union for runtime selection) with a `regName(Register) []const u8` method. Replace all `.x86Reg()` calls with a backend call. This is mechanical but widespread (~10 files).

### Phase 3 — Abstract the float register file

Add a float register type separate from `Register`. Parameterize `FLOAT_LIT`, `FLOAT_BINOP`, `FLOAT_CMP_FLAGS` to use backend-supplied float register names and mnemonics.

### Phase 4 — Replace arch-specific instructions

Swap `CQO` + `IDIV_MEM` for a generic `idiv` IR node that each backend lowers differently. Same treatment for `GC_INC`/`GC_DEC` scratch register selection.

### Phase 5 — New emitter for ARM64

Write `arm64.zig` implementing the same backend interface as `x86.zig`, emitting GNU as syntax for AArch64. Replace the macOS syscall with the ARM64 equivalent (`mov x16, 1; svc 0x80`).

---

## File Prioritization Table

| File | Change needed | Effort |
|---|---|---|
| `src/codegen/instructions.zig` | `x86Reg()` dispatch, CQO/IDIV, float/GC scratch | High |
| `src/codegen/program/funcdef.zig` | Calling convention parameterization | High |
| `src/codegen/program/values/funcall.zig` | Calling convention, stack alignment | High |
| `src/codegen/x86.zig` | Extract to backend interface, new `arm64.zig` | High |
| `src/codegen/program/values/print.zig` | `mov al, 1` and `rdi` format arg | Medium |
| `src/codegen/registerTable.zig` | Parameterize scratch reg count, RBP base | Medium |
| `src/codegen/program/values/binop.zig` | Float mnemonic dispatch | Medium |
| `src/codegen/program/gc.zig` | Scratch register for refcount ops | Low |
| `build.zig` | Add `-Dtarget` option | Low |

---

## Risk Assessment

| Component | Risk | Effort |
|---|---|---|
| Calling convention | High | High — affects all function calls |
| Register allocation | Medium | Medium — mostly parameterization |
| Float operations | High | High — SSE to NEON register file mismatch |
| Integer division | Medium | Low — few uses, isolated instructions |
| Stack frame layout | Medium | Medium — alignment logic is widespread |
| Assembly emission | Medium | High — pervasive across codebase |
| GC operations | Low | Low — isolated in gc.zig and instructions.zig |
| Runtime helpers | Low | Low — few functions in x86.zig |

---

## Key Insight

The biggest leverage point is **Phase 1 + 2**: once `ARGUMENT_REGS` and `x86Reg()` are parameterized through a backend context, most remaining changes become local and straightforward. The IR is sound — the problem is that the emission layer is not separated from the IR layer.
