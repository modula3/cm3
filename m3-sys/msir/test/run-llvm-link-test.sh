#!/bin/sh
# Run the MSIR end-to-end LLVM link test.
#
# Steps:
#   1. Build the M3 smoke test with MSIR emission enabled
#   2. Compile the generated .ll to an object file via LLVM clang
#   3. Link with the C test harness and run
#
# Prerequisites:
#   - cm3 on PATH (with @M3m3front-msir support built in)
#   - LLVM clang on PATH, or LLVM_PREFIX set to the LLVM install prefix
#     (e.g. export LLVM_PREFIX=$(brew --prefix llvm))

set -e

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
SMOKE_SRC="$SCRIPT_DIR/smoke"
TARGET="${TARGET:-ARM64_DARWIN}"
BUILD_DIR="$SMOKE_SRC/$TARGET"

LLVM_PREFIX="${LLVM_PREFIX:-$(brew --prefix llvm 2>/dev/null || echo /usr/local)}"
LLVM_CLANG="$LLVM_PREFIX/bin/clang"

if [ ! -x "$LLVM_CLANG" ]; then
    echo "error: clang not found at $LLVM_CLANG" >&2
    echo "       Set LLVM_PREFIX to your LLVM install (e.g. brew --prefix llvm)" >&2
    exit 1
fi

echo "=== Step 1: build M3 smoke test with MSIR emission ==="
(cd "$SMOKE_SRC" && cm3 -clean && cm3 '@M3m3front-msir' -build)

LL="$BUILD_DIR/Main.ll"
if [ ! -f "$LL" ]; then
    echo "error: $LL not generated — check that cm3 has MSIR support" >&2
    exit 1
fi

echo ""
echo "=== Step 2: compile $LL via LLVM ==="
OBJ="$BUILD_DIR/Main-llvm.o"
"$LLVM_CLANG" -c "$LL" -o "$OBJ"
echo "compiled: $OBJ"

echo ""
echo "=== Step 3: link and run C test harness ==="
TEST_BIN="$BUILD_DIR/llvm-link-test"
"$LLVM_CLANG" "$SCRIPT_DIR/smoke/llvm_link_test.c" "$SCRIPT_DIR/smoke/raise_stub.cpp" "$OBJ" -lc++ -o "$TEST_BIN"
echo "linked:   $TEST_BIN"
echo ""
"$TEST_BIN"
