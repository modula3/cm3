#!/bin/sh
# Run all MScheme test suites: interpreter tests and compiled tests.
# Usage: ./run-tests.sh [TARGET]
#   TARGET defaults to ARM64_DARWIN.

set -e

TARGET="${1:-ARM64_DARWIN}"
DIR="$(cd "$(dirname "$0")" && pwd)"
NUMTEST="$DIR/$TARGET/numtest"
MSCHEME="${MSCHEME:-$(dirname "$DIR")/../../install/bin/mscheme}"
DOC_DIR="$DIR/../../mscheme-doc"

total=0
failed=0

run_suite() {
  runner="$1"
  file="$2"
  name="$(basename "$file" .scm)"
  echo "--- $name ---"
  if "$runner" "$file" exit 2>&1; then
    :
  else
    echo "CRASHED: $name (exit $?)"
    failed=$((failed + 1))
  fi
  total=$((total + 1))
  echo
}

# --- Interpreter tests (mscheme-doc/test-*.scm) ---
echo "========================================"
echo "  INTERPRETER TESTS"
echo "========================================"
echo

if [ -x "$MSCHEME" ]; then
  for f in "$DOC_DIR"/test-*.scm; do
    [ -f "$f" ] || continue
    run_suite "$MSCHEME" "$f"
  done
else
  echo "WARNING: mscheme not found at $MSCHEME, skipping interpreter tests" >&2
  echo "  Set MSCHEME=/path/to/mscheme to override." >&2
  echo
fi

# --- Compiled tests (numtest/src/test-*.scm) ---
echo "========================================"
echo "  COMPILED TESTS"
echo "========================================"
echo

if [ -x "$NUMTEST" ]; then
  for f in "$DIR"/src/test-cn-*.scm "$DIR"/src/test-examples.scm "$DIR"/src/test-r4rs-gaps.scm; do
    [ -f "$f" ] || continue
    run_suite "$NUMTEST" "$f"
  done
else
  echo "WARNING: numtest not found at $NUMTEST — build first with cm3" >&2
  echo
fi

# --- Summary ---
echo "================================"
echo "Suites run: $total"
if [ "$failed" -gt 0 ]; then
  echo "Suites with crashes: $failed"
  exit 1
else
  echo "All suites completed."
fi
