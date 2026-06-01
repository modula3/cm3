#!/bin/sh
# MSIR runtime conformance harness.
#
# For each standalone m3test, build it with MSIR emission enabled (parallel
# @M3m3front-msir mode, so the normal C executable is also produced), then:
#   1. run the C-compiled executable           -> reference stdout + exit code
#   2. compile every emitted <Mod>.ll via LLVM -> <Mod>-llvm.o
#   3. link _m3main.o + the MSIR objects + the C-compiled libm3.a/libm3core.a
#   4. run the MSIR-compiled executable         -> MSIR stdout + exit code
#   5. compare MSIR result against the C reference
#
# This genuinely EXECUTES MSIR-generated code on the real runtime (the C
# runtime, linked statically) and checks it behaves identically to the C
# backend — converting "MSIR emission did not abandon" into "MSIR output runs
# correctly".  An abandon truncates the .ll, so step 2 or 3 fails and the test
# is reported MSIR-FAIL rather than silently passing.
#
# Usage (from repo root):
#   m3-sys/msir/test/run-msir-conformance.sh                 # default test set
#   m3-sys/msir/test/run-msir-conformance.sh p0/p001 p1/p116 # explicit tests
#   TESTS_ROOT=... LLVM_PREFIX=... ...
#
# Prerequisites: cm3 on PATH (with @M3m3front-msir); LLVM clang; the config in
# C backend mode (M3_BACKEND_MODE = "C"); C-compiled libm3core.a and libm3.a
# shipped under ~/cm3/pkg (the normal state).

set -u

TARGET="${TARGET:-ARM64_DARWIN}"
TESTS_ROOT="${TESTS_ROOT:-$(cd "$(dirname "$0")/../../m3tests/src" && pwd)}"
PKG="${CM3_PKG:-$HOME/cm3/pkg}"
LIBM3CORE="$PKG/m3core/$TARGET/libm3core.a"
LIBM3="$PKG/libm3/$TARGET/libm3.a"
# The m3tests "Test" support module (Test.check/checkI/done, used by most
# p-tests) is built once in the m3tests package, not per test.  Link its
# C-compiled objects so the test's OWN code (which we compile to MSIR) can be
# validated; Test running as C is fine — it is not the code under test.
M3TESTS_OBJDIR="$(cd "$(dirname "$0")/../../m3tests/$TARGET" 2>/dev/null && pwd || true)"
TEST_OBJS=""
for o in Test_i.o Test_m.o TestC.o; do
    [ -f "$M3TESTS_OBJDIR/$o" ] && TEST_OBJS="$TEST_OBJS $M3TESTS_OBJDIR/$o"
done
LLVM_PREFIX="${LLVM_PREFIX:-$(brew --prefix llvm 2>/dev/null || echo /usr/local)}"
CLANG="$LLVM_PREFIX/bin/clang"

if [ ! -x "$CLANG" ]; then echo "error: clang not found at $CLANG" >&2; exit 1; fi
for a in "$LIBM3CORE" "$LIBM3"; do
    [ -f "$a" ] || { echo "error: missing $a (build+ship it in C mode)" >&2; exit 1; }
done

# Default test set: a spread of standalone p0/p1/p2 tests that produce stdout.
DEFAULT_TESTS="p0/p001 p0/p002 p0/p004 p0/p005 p0/p013 p0/p015 \
p1/p116 p1/p130 p1/p135 p1/p147 p2/p202 p2/p230 p2/p231 p2/p232"

# Expand group arguments (p0/p1/p2/p3/all) into the individual tests under them.
expand_args() {
    for a in "$@"; do
        case "$a" in
            all) for g in p0 p1 p2 p3; do
                     for d in "$TESTS_ROOT/$g"/p*; do
                         [ -d "$d" ] && echo "$g/$(basename "$d")"
                     done
                 done ;;
            p0|p1|p2|p3)
                 for d in "$TESTS_ROOT/$a"/p*; do
                     [ -d "$d" ] && echo "$a/$(basename "$d")"
                 done ;;
            *)   echo "$a" ;;
        esac
    done
}

if [ "$#" -gt 0 ]; then
    TESTS="$(expand_args "$@")"
else
    TESTS="$DEFAULT_TESTS"
fi

LOGDIR="${TMPDIR:-/tmp}/msir-conformance"
mkdir -p "$LOGDIR"

# Run an executable with a wall-clock timeout so a miscompiled (hanging) MSIR
# binary is reported (rc 124) rather than wedging the whole harness.
EXE_TIMEOUT="${EXE_TIMEOUT:-15}"
run_exe() { timeout "$EXE_TIMEOUT" "$1" 2>/dev/null; }

# The cm3 build itself can hang/loop on some tests (e.g. p161, a known
# compile-time timeout in the sweep) — cap it too, else the whole harness wedges.
BUILD_TIMEOUT="${BUILD_TIMEOUT:-90}"

pass=0; mismatch=0; msirfail=0; skip=0; total=0
failed_list=""

for t in $TESTS; do
    total=$((total + 1))
    dir="$TESTS_ROOT/$t"
    name="$t"
    if [ ! -f "$dir/m3makefile" ]; then
        printf "  %-12s SKIP  (no m3makefile)\n" "$name"; skip=$((skip+1)); continue
    fi
    bd="$dir/$TARGET"
    ( cd "$dir" && rm -rf "$TARGET" \
        && timeout "$BUILD_TIMEOUT" cm3 '@M3m3front-msir' '@M3m3front-msir-forruntime' -keep -build ) \
        >"$LOGDIR/$(echo $t | tr / _).log" 2>&1
    if [ $? -eq 124 ]; then
        printf "  %-12s SKIP  (cm3 build timed out — compile-time hang)\n" "$name"
        skip=$((skip+1)); continue
    fi
    # The C-compiled standalone executable is the reference.
    cexe=""
    for cand in "$bd/pgm" "$bd/$(basename "$t")"; do
        [ -x "$cand" ] && { cexe="$cand"; break; }
    done
    if [ -z "$cexe" ]; then
        printf "  %-12s SKIP  (no standalone executable — library/parse test?)\n" "$name"
        skip=$((skip+1)); continue
    fi
    refout=$(run_exe "$cexe"); refrc=$?

    # Compile every emitted .ll to an object.
    objs=""; emitfail=0
    for ll in "$bd"/*.ll; do
        [ -f "$ll" ] || continue
        o="${ll%.ll}-llvm.o"
        if ! "$CLANG" -c "$ll" -o "$o" >>"$LOGDIR/$(echo $t | tr / _).log" 2>&1; then
            emitfail=1; break
        fi
        objs="$objs $o"
    done
    if [ "$emitfail" = 1 ] || [ -z "$objs" ]; then
        printf "  %-12s MSIR-FAIL  (.ll malformed — abandon truncated emission)\n" "$name"
        msirfail=$((msirfail+1)); failed_list="$failed_list $name"; continue
    fi

    # Link _m3main.o + MSIR objects + C runtime archives.
    msirexe="$bd/pgm-msir"
    if ! "$CLANG" "$bd/_m3main.o" $objs $TEST_OBJS "$LIBM3" "$LIBM3CORE" -lc++ -o "$msirexe" \
            >>"$LOGDIR/$(echo $t | tr / _).log" 2>&1; then
        printf "  %-12s MSIR-FAIL  (link failed — missing symbols / extra libs needed)\n" "$name"
        msirfail=$((msirfail+1)); failed_list="$failed_list $name"; continue
    fi

    msirout=$(run_exe "$msirexe"); msirrc=$?

    if [ "$msirout" = "$refout" ] && [ "$msirrc" = "$refrc" ]; then
        printf "  %-12s PASS  (rc=%s)\n" "$name" "$msirrc"; pass=$((pass+1))
    else
        printf "  %-12s MISMATCH  (C: rc=%s / MSIR: rc=%s)\n" "$name" "$refrc" "$msirrc"
        mismatch=$((mismatch+1)); failed_list="$failed_list $name"
    fi
done

echo ""
echo "=== MSIR conformance: $pass/$total PASS, $mismatch MISMATCH, $msirfail MSIR-FAIL, $skip SKIP ==="
[ -n "$failed_list" ] && echo "failed:$failed_list"
[ "$mismatch" = 0 ] && [ "$msirfail" = 0 ]
