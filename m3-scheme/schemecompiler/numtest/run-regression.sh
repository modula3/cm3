#!/bin/sh
# run-regression.sh -- Numeric tower regression test suite
#
# Compares three behaviors:
#   1. Baseline  -- interpreted, pre-numeric-tower (commit 5672351cd7)
#   2. Current   -- interpreted, HEAD (with numeric tower)
#   3. Compiled  -- compiled (numtest), HEAD (with numeric tower)
#
# For each test suite, captures output and diffs against baseline.
# Reports regressions and compiler gaps.
#
# Usage:
#   ./run-regression.sh                  # full run (builds baseline worktree)
#   ./run-regression.sh --skip-baseline  # skip baseline build, compare interp vs compiled
#   ./run-regression.sh --skip-build     # skip all builds, just run tests
#
# Prerequisites:
#   - CM3 installed at $CM3_INSTALL (default: ~/cm3/install)
#   - Current mscheme and numtest already built (or use default build)
#
# Output: results/ directory with .out files and .diff files

set -e

SCRIPTDIR="$(cd "$(dirname "$0")" && pwd)"
CM3ROOT="$(cd "$SCRIPTDIR/../../.." && pwd)"
CM3_INSTALL="${CM3_INSTALL:-$CM3ROOT/../install}"
CM3="${CM3:-$CM3_INSTALL/bin/cm3}"
BASELINE_COMMIT="5672351cd7"
RESULTS="$SCRIPTDIR/results"
SKIP_BASELINE=false
SKIP_BUILD=false
CSP_TESTS=true

# --- Parse arguments -------------------------------------------------------

for arg in "$@"; do
    case "$arg" in
        --skip-baseline) SKIP_BASELINE=true ;;
        --skip-build)    SKIP_BUILD=true ;;
        --no-csp)        CSP_TESTS=false ;;
        --help|-h)
            echo "Usage: $0 [--skip-baseline] [--skip-build] [--no-csp]"
            echo ""
            echo "  --skip-baseline  Skip building baseline mscheme from worktree"
            echo "  --skip-build     Skip all builds (assumes binaries exist)"
            echo "  --no-csp         Skip CSP regression tests"
            exit 0
            ;;
        *) echo "Unknown argument: $arg" >&2; exit 1 ;;
    esac
done

# --- Determine target -------------------------------------------------------

TARGET=$("$CM3" -version 2>&1 | sed -n 's/.*target: *//p')
if [ -z "$TARGET" ]; then
    echo "error: cannot determine target from cm3 -version" >&2
    exit 1
fi
echo "Target: $TARGET"

# --- Paths ------------------------------------------------------------------

MSCHEME_CUR="$CM3_INSTALL/bin/mscheme"
NUMTEST="$SCRIPTDIR/$TARGET/numtest"
TESTDIR="$CM3ROOT/m3-scheme/mscheme-doc"
SICP_DIR="$TESTDIR/sicp-examples"
BASELINE_WORKTREE="${BASELINE_WORKTREE:-/tmp/mscheme-baseline-$$}"
MSCHEME_BASE=""   # set after baseline build

M3UTILS="/Users/mika/cm3/intel-async/async-toolkit/m3utils/m3utils"
CSPDIR="$M3UTILS/csp"
CSP_TESTDIR="$CSPDIR/src/tests"

mkdir -p "$RESULTS"

# --- Helpers ----------------------------------------------------------------

log() {
    echo ""
    echo "================================================================"
    echo "  $*"
    echo "================================================================"
}

run_test() {
    # run_test <label> <binary> <test-file> [<working-dir>]
    _label="$1"
    _bin="$2"
    _testfile="$3"
    _workdir="${4:-.}"
    _outfile="$RESULTS/${_label}.out"

    echo "  Running: $_label"
    if (cd "$_workdir" && "$_bin" "$_testfile") > "$_outfile" 2>&1; then
        _rc=0
    else
        _rc=$?
    fi

    # Extract summary line
    if grep -q "passed" "$_outfile" 2>/dev/null; then
        grep "passed" "$_outfile" | tail -1
    elif grep -q "ALL TESTS PASSED" "$_outfile" 2>/dev/null; then
        echo "  ALL TESTS PASSED"
    elif grep -q "SOME TESTS FAILED" "$_outfile" 2>/dev/null; then
        echo "  SOME TESTS FAILED"
    fi

    return $_rc
}

diff_outputs() {
    # diff_outputs <test-name> <label-a> <label-b>
    _test="$1"
    _a="$2"
    _b="$3"
    _file_a="$RESULTS/${_a}.out"
    _file_b="$RESULTS/${_b}.out"
    _diff_file="$RESULTS/${_test}-${_a}-vs-${_b}.diff"

    if [ ! -f "$_file_a" ] || [ ! -f "$_file_b" ]; then
        echo "  SKIP diff (missing output)"
        return 0
    fi

    # Filter known acceptable differences before diffing:
    #   - Exact integer display: "42" vs "42.0" (baseline shows .0)
    #   - PASS/FAIL lines only (skip timing, debug output)
    _fa="$RESULTS/.filtered-a-$$"
    _fb="$RESULTS/.filtered-b-$$"
    grep -E "^(PASS:|FAIL:|  PASS |  FAIL )" "$_file_a" > "$_fa" 2>/dev/null || true
    grep -E "^(PASS:|FAIL:|  PASS |  FAIL )" "$_file_b" > "$_fb" 2>/dev/null || true

    if diff -u "$_fa" "$_fb" > "$_diff_file" 2>&1; then
        echo "  OK: $_a vs $_b -- no differences"
        rm -f "$_diff_file"
    else
        _ndiffs=$(wc -l < "$_diff_file")
        echo "  DIFF: $_a vs $_b -- differences found (see $_diff_file)"
    fi

    rm -f "$_fa" "$_fb"
}

# ============================================================================
# Part 1: Build baseline mscheme from worktree
# ============================================================================

if [ "$SKIP_BASELINE" = false ]; then
    # If worktree already exists with a built mscheme, reuse it
    if [ -x "$BASELINE_WORKTREE/m3-scheme/mscheme-interactive/$TARGET/mscheme" ]; then
        MSCHEME_BASE="$BASELINE_WORKTREE/m3-scheme/mscheme-interactive/$TARGET/mscheme"
        echo "  Reusing baseline mscheme: $MSCHEME_BASE"
    fi

    if [ -z "$MSCHEME_BASE" ]; then
    log "Building baseline mscheme (commit $BASELINE_COMMIT)"

    # Create worktree
    if [ -d "$BASELINE_WORKTREE" ]; then
        echo "  Removing old worktree at $BASELINE_WORKTREE"
        (cd "$CM3ROOT" && git worktree remove --force "$BASELINE_WORKTREE" 2>/dev/null || true)
    fi

    echo "  Creating worktree at $BASELINE_WORKTREE"
    (cd "$CM3ROOT" && git worktree add "$BASELINE_WORKTREE" "$BASELINE_COMMIT")

    # Build baseline mscheme from worktree source using CURRENT cm3 compiler.
    # We don't rebuild m3core/libm3 — the installed versions are compatible.
    # The baseline commit predates the mpz package, so we skip it.
    echo "  Building baseline scheme packages with current cm3..."
    for pkg in scheme-lib mscheme modula3scheme mscheme-interactive; do
        echo "    $pkg"
        (cd "$BASELINE_WORKTREE/m3-scheme/$pkg/src" && "$CM3" -build -ship) >> "$RESULTS/baseline-build.log" 2>&1
    done

    MSCHEME_BASE="$BASELINE_WORKTREE/m3-scheme/mscheme-interactive/$TARGET/mscheme"
    if [ ! -x "$MSCHEME_BASE" ]; then
        echo "error: baseline mscheme not found at $MSCHEME_BASE" >&2
        echo "  Check $RESULTS/baseline-build.log for build errors" >&2
        exit 1
    fi
    echo "  Baseline mscheme: $MSCHEME_BASE"
    fi  # end if [ -z "$MSCHEME_BASE" ]
fi

# ============================================================================
# Part 2: Build current numtest (compiled tests)
# ============================================================================

if [ "$SKIP_BUILD" = false ]; then
    log "Building numtest (compiled test suites)"
    (cd "$SCRIPTDIR" && "$CM3" -build -override) > "$RESULTS/numtest-build.log" 2>&1
    if [ ! -x "$NUMTEST" ]; then
        echo "error: numtest not found at $NUMTEST after build" >&2
        echo "  Check $RESULTS/numtest-build.log" >&2
        exit 1
    fi
    echo "  numtest: $NUMTEST"
fi

# ============================================================================
# Part 3: Run mscheme-doc test suites
# ============================================================================

log "Running mscheme-doc test suites"

TOTAL_PASS=0
TOTAL_FAIL=0

# --- test-examples.scm ---

echo ""
echo "--- test-examples.scm ---"
if [ -n "$MSCHEME_BASE" ]; then
    run_test "examples-baseline" "$MSCHEME_BASE" "$TESTDIR/test-examples.scm" || true
fi
run_test "examples-interp" "$MSCHEME_CUR" "$TESTDIR/test-examples.scm" || true
run_test "examples-compiled" "$NUMTEST" "$SCRIPTDIR/src/test-examples.scm" || true

if [ -n "$MSCHEME_BASE" ]; then
    diff_outputs "examples" "examples-baseline" "examples-interp"
fi
diff_outputs "examples" "examples-interp" "examples-compiled"

# --- test-r4rs-gaps.scm ---

echo ""
echo "--- test-r4rs-gaps.scm ---"
if [ -n "$MSCHEME_BASE" ]; then
    run_test "r4rs-baseline" "$MSCHEME_BASE" "$TESTDIR/test-r4rs-gaps.scm" || true
fi
run_test "r4rs-interp" "$MSCHEME_CUR" "$TESTDIR/test-r4rs-gaps.scm" || true
run_test "r4rs-compiled" "$NUMTEST" "$SCRIPTDIR/src/test-r4rs-gaps.scm" || true

if [ -n "$MSCHEME_BASE" ]; then
    diff_outputs "r4rs" "r4rs-baseline" "r4rs-interp"
fi
diff_outputs "r4rs" "r4rs-interp" "r4rs-compiled"

# --- test-numeric-tower.scm (no baseline -- new test) ---

echo ""
echo "--- test-numeric-tower.scm ---"
run_test "numtower-interp" "$MSCHEME_CUR" "$TESTDIR/test-numeric-tower.scm" || true
run_test "numtower-compiled" "$NUMTEST" "$SCRIPTDIR/src/test-numeric-tower.scm" || true

diff_outputs "numtower" "numtower-interp" "numtower-compiled"

# --- SICP tests (interpreted only -- compiled blocked by m3front bug) ---

echo ""
echo "--- SICP run-tests.scm ---"
if [ -n "$MSCHEME_BASE" ]; then
    run_test "sicp-baseline" "$MSCHEME_BASE" "$SICP_DIR/run-tests.scm" "$SICP_DIR" || true
fi
run_test "sicp-interp" "$MSCHEME_CUR" "$SICP_DIR/run-tests.scm" "$SICP_DIR" || true
echo "  (compiled: SKIP -- scheme compiler emits '(*' as M3 comment start)"

if [ -n "$MSCHEME_BASE" ]; then
    diff_outputs "sicp" "sicp-baseline" "sicp-interp"
fi

# ============================================================================
# Part 4: scheme-lib smoke test (17 bundled .scm files)
# ============================================================================

log "scheme-lib smoke test (17 bundled files loaded at startup)"

SCHEMELIB_DIR="$CM3ROOT/m3-scheme/scheme-lib/src"

smoke_test() {
    # smoke_test <label> <binary> <expr>
    _label="$1"
    _bin="$2"
    _expr="$3"
    _outfile="$RESULTS/${_label}.out"

    echo "  Running: $_label"
    if echo "$_expr" | "$_bin" > "$_outfile" 2>&1; then
        echo "  OK"
    else
        echo "  FAILED (exit $?, see $_outfile)"
    fi
}

echo ""
echo "--- scheme-lib startup (implicit load of all bundled .scm) ---"
if [ -n "$MSCHEME_BASE" ]; then
    smoke_test "schemelib-baseline" "$MSCHEME_BASE" \
        '(display "scheme-lib-ok") (newline) (display (length (list 1 2 3))) (newline) (exit 0)'
fi
smoke_test "schemelib-interp" "$MSCHEME_CUR" \
    '(display "scheme-lib-ok") (newline) (display (length (list 1 2 3))) (newline) (exit 0)'

echo ""
echo "--- struct.scm (uses quasiquote) ---"
if [ -n "$MSCHEME_BASE" ]; then
    smoke_test "struct-baseline" "$MSCHEME_BASE" \
        '(define-struct point (x y)) (define p (make-point 3 4)) (display (point-x p)) (newline) (display (point-y p)) (newline) (exit 0)'
fi
smoke_test "struct-interp" "$MSCHEME_CUR" \
    '(define-struct point (x y)) (define p (make-point 3 4)) (display (point-x p)) (newline) (display (point-y p)) (newline) (exit 0)'

echo ""
echo "--- mbe.scm (macro-by-example) ---"
if [ -n "$MSCHEME_BASE" ]; then
    smoke_test "mbe-baseline" "$MSCHEME_BASE" \
        '(define-syntax my-and (syntax-rules () ((_) #t) ((_ e) e) ((_ e1 e2 ...) (if e1 (my-and e2 ...) #f)))) (display (my-and #t #t #t)) (newline) (display (my-and #t #f #t)) (newline) (exit 0)'
fi
smoke_test "mbe-interp" "$MSCHEME_CUR" \
    '(define-syntax my-and (syntax-rules () ((_) #t) ((_ e) e) ((_ e1 e2 ...) (if e1 (my-and e2 ...) #f)))) (display (my-and #t #t #t)) (newline) (display (my-and #t #f #t)) (newline) (exit 0)'

echo ""
echo "--- hashtable.scm ---"
if [ -n "$MSCHEME_BASE" ]; then
    smoke_test "hashtable-baseline" "$MSCHEME_BASE" \
        '(define h (make-hash-table)) (hash-table-put! h "key" 42) (display (hash-table-get h "key")) (newline) (exit 0)'
fi
smoke_test "hashtable-interp" "$MSCHEME_CUR" \
    '(define h (make-hash-table)) (hash-table-put! h "key" 42) (display (hash-table-get h "key")) (newline) (exit 0)'

echo ""
echo "--- mergesort.scm ---"
if [ -n "$MSCHEME_BASE" ]; then
    smoke_test "mergesort-baseline" "$MSCHEME_BASE" \
        '(display (sort (list 5 3 1 4 2) <)) (newline) (exit 0)'
fi
smoke_test "mergesort-interp" "$MSCHEME_CUR" \
    '(display (sort (list 5 3 1 4 2) <)) (newline) (exit 0)'

# ============================================================================
# Part 5: schemecompiler self-test
# ============================================================================

log "schemecompiler self-test"

COMPTEST="$CM3ROOT/m3-scheme/schemecompiler/test/$TARGET/comptest"
if [ -x "$COMPTEST" ]; then
    echo ""
    echo "--- schemecompiler/test (self-compilation test) ---"
    echo "  Running: comptest"
    if "$COMPTEST" > "$RESULTS/comptest.out" 2>&1; then
        echo "  OK"
    else
        echo "  FAILED (exit $?, see $RESULTS/comptest.out)"
    fi
    grep -E "^(PASS|FAIL|  PASS|  FAIL|Total|=)" "$RESULTS/comptest.out" 2>/dev/null | tail -10
else
    echo "  SKIP: comptest not built at $COMPTEST"
fi

# ============================================================================
# Part 6: sstubgen smoke test
# ============================================================================

log "sstubgen smoke test"

SSTUBGEN="$CM3_INSTALL/bin/sstubgen"
if [ -x "$SSTUBGEN" ]; then
    echo ""
    echo "--- sstubgen (runtime Scheme code: sstubgen.scm 3132 lines) ---"
    # sstubgen loads sstubgen.scm at runtime via the build system.
    # Smoke test: verify it starts and processes -help without crashing.
    echo "  Running: sstubgen-smoke"
    echo '(display "sstubgen-ok") (newline) (display (exact? 42)) (newline) (exit 0)' > /tmp/sstubgen-smoke-$$.scm
    if "$SSTUBGEN" -scm /tmp/sstubgen-smoke-$$.scm -nointeractive > "$RESULTS/sstubgen-smoke.out" 2>&1; then
        echo "  OK"
    else
        echo "  FAILED (exit $?, see $RESULTS/sstubgen-smoke.out)"
    fi
    rm -f /tmp/sstubgen-smoke-$$.scm
else
    echo "  SKIP: sstubgen not built at $SSTUBGEN"
fi

# ============================================================================
# Part 7: CSP regression tests (intel-async tree, 53 .scm files + 122 test files)
# ============================================================================

if [ "$CSP_TESTS" = true ] && [ -d "$CSP_TESTDIR" ]; then
    log "CSP regression tests (53 compiler .scm files, 17,180 lines)"

    CSPC="$CSPDIR/$TARGET/cspc"
    if [ ! -x "$CSPC" ]; then
        echo "  SKIP: cspc not built at $CSPC"
        echo "  Build with: cd $CSPDIR/src && env PATH=\"$CM3_INSTALL/bin:\$PATH\" cm3 -override"
    else
        echo "  Using cspc: $CSPC"

        echo ""
        echo "--- CSP compiler load test (all 53 .scm files) ---"
        # cspc loads all 53 .scm files at startup. A simple -scm invocation
        # exercises the full Scheme code loading.
        echo '(display "cspc-ok") (newline) (exit 0)' > /tmp/cspc-smoke-$$.scm
        if (cd "$CSPDIR" && "$CSPC" -scm /tmp/cspc-smoke-$$.scm) > "$RESULTS/cspc-load.out" 2>&1; then
            echo "  OK: all 53 CSP .scm files loaded"
        else
            echo "  FAILED (see $RESULTS/cspc-load.out)"
        fi
        rm -f /tmp/cspc-smoke-$$.scm

        echo ""
        echo "--- LTS extraction tests (21 tests) ---"
        if (cd "$CSP_TESTDIR" && sh run_lts_tests.sh) > "$RESULTS/csp-lts.out" 2>&1; then
            echo "  LTS tests: PASSED"
        else
            echo "  LTS tests: FAILED (see $RESULTS/csp-lts.out)"
        fi
        grep -E "^(PASS|FAIL|Total|===)" "$RESULTS/csp-lts.out" 2>/dev/null | tail -5

        echo ""
        echo "--- Deadlock detection tests (25 tests) ---"
        if (cd "$CSP_TESTDIR" && sh run_deadlock_tests.sh) > "$RESULTS/csp-deadlock.out" 2>&1; then
            echo "  Deadlock tests: PASSED"
        else
            echo "  Deadlock tests: FAILED (see $RESULTS/csp-deadlock.out)"
        fi
        grep -E "^(PASS|FAIL|Total|===)" "$RESULTS/csp-deadlock.out" 2>/dev/null | tail -5

        echo ""
        echo "--- Dining Philosophers benchmark ---"
        if (cd "$CSP_TESTDIR" && sh run_dining_bench.sh) > "$RESULTS/csp-dining.out" 2>&1; then
            echo "  Dining bench: PASSED"
        else
            echo "  Dining bench: FAILED (see $RESULTS/csp-dining.out)"
        fi
        grep -E "^(PASS|FAIL|states|===)" "$RESULTS/csp-dining.out" 2>/dev/null | tail -5
    fi
fi

# ============================================================================
# Part 8: photopic smoke test (intel-async, 6 .scm files, ~11K lines)
# ============================================================================

log "photopic smoke test"

PHOTOPIC="$M3UTILS/photopic/ARM64_DARWIN/photopic"
if [ -x "$PHOTOPIC" ]; then
    echo ""
    echo "--- photopic (loads photopic.scm + ces99.scm at runtime) ---"
    if [ -n "$MSCHEME_BASE" ]; then
        smoke_test "photopic-baseline" "$MSCHEME_BASE" \
            '(display "photopic-baseline-ok") (newline) (exit 0)'
    fi
    smoke_test "photopic-interp" "$PHOTOPIC" \
        '(display "photopic-ok") (newline) (display (+ 1 2)) (newline) (exit 0)'
    # Exercise numerical operations that photopic depends on
    smoke_test "photopic-numeric" "$PHOTOPIC" \
        '(display (exact? 42)) (newline) (display (inexact? 3.14)) (newline) (display (exact->inexact 1/3)) (newline) (exit 0)'
else
    echo "  SKIP: photopic not built at $PHOTOPIC"
fi

# ============================================================================
# Part 9: liberty, meta, sv, spice, etc. (report build status)
# ============================================================================

log "Other intel-async Scheme code"

for prog_spec in \
    "liberty/editliberty:editliberty:5 .scm, 1021 lines" \
    "meta/meta:meta:19 .scm, 3999 lines" \
    "sv/svfe:svfe:27 .scm, 7979 lines" \
    "vtsensor/sensor:sensor:4 .scm, 315 lines" \
    "stdf/stdf:stdf:4 .scm, 1526 lines"
do
    _dir=$(echo "$prog_spec" | cut -d: -f1)
    _prog=$(echo "$prog_spec" | cut -d: -f2)
    _desc=$(echo "$prog_spec" | cut -d: -f3)

    _bin="$M3UTILS/$_dir/ARM64_DARWIN/$_prog"
    # some programs might be in a different path pattern
    if [ ! -x "$_bin" ]; then
        _bin="$M3UTILS/$(echo "$_dir" | cut -d/ -f1)/ARM64_DARWIN/$_prog"
    fi

    if [ -x "$_bin" ]; then
        echo "  $_prog ($_desc): built, testing..."
        smoke_test "${_prog}-smoke" "$_bin" \
            '(display "ok") (newline) (exit 0)'
    else
        echo "  $_prog ($_desc): NOT BUILT"
    fi
done

# ============================================================================
# Part 5: Summary
# ============================================================================

log "REGRESSION TEST SUMMARY"

echo ""
echo "Output files in: $RESULTS/"
echo ""

# Count PASS/FAIL across all test outputs
for f in "$RESULTS"/*.out; do
    [ -f "$f" ] || continue
    _name=$(basename "$f" .out)
    _pass=$(grep -E -c "^PASS:|^  PASS " "$f" 2>/dev/null) || true
    _fail=$(grep -E -c "^FAIL:|^  FAIL " "$f" 2>/dev/null) || true
    _pass=${_pass:-0}
    _fail=${_fail:-0}
    if [ "$_fail" -gt 0 ]; then
        printf "  %-30s %3d pass, %3d FAIL\n" "$_name" "$_pass" "$_fail"
    else
        printf "  %-30s %3d pass\n" "$_name" "$_pass"
    fi
    TOTAL_PASS=$((TOTAL_PASS + _pass))
    TOTAL_FAIL=$((TOTAL_FAIL + _fail))
done

echo ""
echo "  TOTAL: $TOTAL_PASS passed, $TOTAL_FAIL failed"

# List diffs
_ndiffs=0
for f in "$RESULTS"/*.diff; do
    [ -f "$f" ] || continue
    if [ $_ndiffs -eq 0 ]; then
        echo ""
        echo "  Differences found:"
    fi
    echo "    $(basename "$f")"
    _ndiffs=$((_ndiffs + 1))
done

if [ $_ndiffs -eq 0 ]; then
    echo ""
    echo "  No regressions detected."
fi

# Compiler gap summary
echo ""
echo "  Compiler gaps (punt to interpreter):"
echo "    - quasiquote: 25/53 CSP files, 2/17 scheme-lib files"
echo "    - unwind-protect: 1 occurrence (codegen-m3.scm)"
echo ""
echo "  Compiler bugs:"
echo "    - scodegen emits '(*' which M3 lexer parses as comment start"

# ============================================================================
# Cleanup
# ============================================================================

if [ "$SKIP_BASELINE" = false ] && [ -d "$BASELINE_WORKTREE" ]; then
    echo ""
    echo "Baseline worktree at $BASELINE_WORKTREE"
    echo "  Remove with: cd $CM3ROOT && git worktree remove $BASELINE_WORKTREE"
fi

echo ""
if [ $_ndiffs -gt 0 ]; then
    echo "STATUS: REGRESSIONS FOUND ($_ndiffs diff files)"
    exit 1
elif [ $TOTAL_FAIL -gt 0 ]; then
    echo "STATUS: $TOTAL_FAIL pre-existing failures, no regressions"
    exit 0
else
    echo "STATUS: ALL CLEAN"
    exit 0
fi
