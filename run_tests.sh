#!/usr/bin/env bash
# BBC test runner.
# Usage: ./run_tests.sh [--validate] [--codegen] [--show] [--leaks]
#
#   (no flags)    Run interpreter + codegen for each test; compare both to expected.
#   --validate    Run interpreter only; compare against expected output.
#   --codegen     Run codegen only; compare against expected output.
#   --show        Print interpreter output for every test (no pass/fail).
#   --leaks       Compile each test and run under `leaks --atExit` to detect heap leaks.
#                 Requires macOS leaks(1). Reports CLEAN / LEAK per test.
#
# Requirements: zig build already run; nasm + gcc on PATH.

set -euo pipefail

REPO="$(cd "$(dirname "$0")" && pwd)"
BBC="$REPO/zig-out/bin/bbc"
MODE="both"

for arg in "$@"; do
    case "$arg" in
        --validate) MODE="validate" ;;
        --codegen)  MODE="codegen" ;;
        --show)     MODE="show" ;;
        --leaks)    MODE="leaks" ;;
    esac
done

# ---------- test registry -----------------------------------------------------
# "file|expected_exit|line1|line2|..."  — one entry per test
TESTS=(
    "test4.bbc|0|7|120|150|7|100"
    "test5.bbc|0|5|hello"
    "tests/test_err_div.bbc|0|5|0|25"
    "tests/test_err_buf.bbc|0|10|30|0"
    "tests/test_err_str.bbc|0|h|o|?"
    "tests/test_err_subscription.bbc|0|0|20|0"
    "tests/test_err_subscription_propagate.bbc|0|3|1"
    "tests/test_err_propagate.bbc|0|10|0"
    "tests/test_err_generic.bbc|0|42|42|0"
    "tests/test_generics.bbc|0|3|99"
    "tests/test_bool_ops.bbc|0|false|true|false|true|true|false|false|false|true|true|true|false|true|true|false"
    "tests/test_arithmetic.bbc|0|14|4|13|30|0|-2|7|7|0|1|-1|0|25|false|true|true|false|true|false"
    "tests/test_recursion.bbc|0|0|1|5|21|4|25|1|1|256|81"
    "tests/test_nested_while.bbc|0|9|25|6|0|3"
    "tests/test_struct_methods.bbc|0|0|5|5|20|30"
    "tests/test_buf_types.bbc|0|5|3|2|1|5|hello|Hello|3"
    "tests/test_string_concat.bbc|0|5|3"
    "tests/test_multi_tp.bbc|0|42|true|x|99|99|x|z"
    "tests/test_multi_tp2.bbc|0|99|true|10|z|25|42|100"
    "tests/test_multi_tp3.bbc|0|1|true|false|q|a|7|10|20"
    "tests/test_subscription.bbc|0|h"
    "tests/test_forloop.bbc|0|5|hello"
    "tests/test_error1.bbc|0|1"
    "tests/test_scope_err.bbc|0|20"
    "tests/namespace/test_depth.bbc|0|36"
    "tests/namespace/test_flat_import.bbc|0|4|6|25|11"
    "tests/namespace/test_namespace_basic.bbc|0|36|7"
    "tests/namespace/test_namespace_struct.bbc|0|3|4"
    "tests/namespace/test_paren_ns.bbc|0|25|9"
    "tests/test_inbuilt.bbc|0|1|7|42|0|1"
    "tests/test_strings_truncate.bbc|0|ell"
    "tests/test_bound_method.bbc|0|42"
    "tests/test_float.bbc|0|5|2.5|4.6875|3|false|true|false|true|true|false"
    "tests/test_func_val.bbc|0|10|42|7"
    "tests/test_multi_tp4.bbc|0|21|false|"$'\xc3'"|99|10|11"
    "tests/test_gc_deep_calls.bbc|0|33|56|85"
    "tests/test_gc_struct_heap.bbc|0|33|86|61|182"
    "tests/test_gc_buf_of_structs.bbc|0|30|60"
)

# ---------- helpers -----------------------------------------------------------

extract_output() {
    local raw="$1"
    echo "$raw" \
        | grep --text -A9999 "Output of programme" \
        | grep --text -B9999 "Return value of programme" \
        | grep --text -v "Output of programme\|Return value\|^----" \
        | LC_ALL=C sed 's/[[:space:]]*$//'   # strip trailing whitespace (incl. null chars)
}

run_interp() {
    local file="$1"
    local raw
    raw="$("$BBC" -X "$REPO/$file" 2>&1)" || true
    extract_output "$raw"
}

# Compiles file to a temp binary, sets TMPBIN path, returns non-zero on failure.
# Caller must rm -f "$TMPASM" "$TMPOBJ" "$TMPBIN" after use.
TMPASM="" TMPOBJ="" TMPBIN=""
compile_to_bin() {
    local file="$1"
    TMPASM="$(mktemp /tmp/bbc_XXXXXX.asm)"
    TMPOBJ="$(mktemp /tmp/bbc_XXXXXX.o)"
    TMPBIN="$(mktemp /tmp/bbc_XXXXXX)"
    (cd "$REPO" && "$BBC" "$file" 2>/dev/null) \
        && cp "$REPO/output.asm" "$TMPASM" \
        && nasm -f macho64 "$TMPASM" -o "$TMPOBJ" 2>/dev/null \
        && gcc -target x86_64-apple-macos12 "$TMPOBJ" -o "$TMPBIN" 2>/dev/null
}

run_codegen() {
    local file="$1"
    local out=""
    if compile_to_bin "$file"; then
        out="$(arch -x86_64 "$TMPBIN" 2>&1 | LC_ALL=C sed 's/[[:space:]]*$//')" || true
    else
        out="[codegen/link failed]"
    fi
    rm -f "$TMPASM" "$TMPOBJ" "$TMPBIN"
    echo "$out"
}

# run_leaks: compile and run under `leaks --atExit`.
# Prints a one-line summary to stdout; returns 0 if clean, 1 if leaks/error.
run_leaks() {
    local file="$1"
    local leaklog
    leaklog="$(mktemp /tmp/bbc_leaks_XXXXXX.txt)"

    if ! compile_to_bin "$file"; then
        rm -f "$TMPASM" "$TMPOBJ" "$TMPBIN" "$leaklog"
        echo "[codegen/link failed]"
        return 1
    fi

    # leaks --atExit injects MallocStackLogging into the target via DYLD.
    # For Rosetta binaries launched with `arch -x86_64`, the exec preserves the
    # PID so the injection applies to the x86-64 process.
    # Exit code: 0 = no leaks, non-zero = leaks detected.
    leaks --atExit -- arch -x86_64 "$TMPBIN" > "$leaklog" 2>&1 || true
    local leaks_exit=$?

    local summary
    # Extract the "N leaks for M total leaked bytes" line if present.
    summary="$(LC_ALL=C grep -i 'leaks for' "$leaklog" 2>/dev/null | tail -1)" || summary=""

    rm -f "$TMPASM" "$TMPOBJ" "$TMPBIN" "$leaklog"

    if [ $leaks_exit -eq 0 ]; then
        echo "CLEAN"
        return 0
    else
        echo "LEAK  ${summary}"
        return 1
    fi
}

parse_expected() {
    local entry="$1"
    # Strip first two pipe-fields (file, exit), join the rest with newlines
    local rest="${entry#*|}"  # remove file|
    rest="${rest#*|}"          # remove exit|
    echo "$rest" | LC_ALL=C tr '|' '\n'
}

# ---------- main loop ---------------------------------------------------------

pass=0; fail=0

run_test() {
    local entry="$1"
    local file="${entry%%|*}"
    local expected; expected="$(parse_expected "$entry")"

    printf "%-60s" "$file"

    if [ "$MODE" = "show" ]; then
        echo ""
        run_interp "$file" | LC_ALL=C sed 's/^/  /'
        return
    fi

    if [ "$MODE" = "leaks" ]; then
        local result
        result="$(run_leaks "$file")"
        if [[ "$result" == CLEAN* ]]; then
            echo " CLEAN"
            ((pass++)) || true
        else
            echo " [${result}]"
            ((fail++)) || true
        fi
        return
    fi

    local interp_out codegen_out
    local ok=true

    if [ "$MODE" = "both" ] || [ "$MODE" = "validate" ]; then
        interp_out="$(run_interp "$file")"
        if [ "$interp_out" != "$expected" ]; then
            ok=false
            echo " [INTERP FAIL]"
            diff <(echo "$expected") <(echo "$interp_out") | head -10 | LC_ALL=C sed 's/^/  /' || true
        fi
    fi

    if [ "$MODE" = "both" ] || [ "$MODE" = "codegen" ]; then
        codegen_out="$(run_codegen "$file")"
        if [ "$codegen_out" != "$expected" ]; then
            ok=false
            echo " [CODEGEN FAIL]"
            diff <(echo "$expected") <(echo "$codegen_out") | head -10 | LC_ALL=C sed 's/^/  /' || true
        fi
    fi

    if $ok; then
        echo " OK"
        ((pass++)) || true
    else
        ((fail++)) || true
    fi
}

echo "BBC test suite  (mode: $MODE)"
echo "=============================="
for entry in "${TESTS[@]}"; do
    run_test "$entry"
done

if [ "$MODE" != "show" ]; then
    echo ""
    echo "Results: $pass passed, $fail failed"
    [ $fail -eq 0 ]
fi
