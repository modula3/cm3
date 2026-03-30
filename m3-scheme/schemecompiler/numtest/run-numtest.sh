#!/bin/sh
# run-numtest.sh -- build and run the numtest compiled Scheme test suites
#
# Usage: ./run-numtest.sh [cm3-path]
#
# The test .scm files in src/ are compiled to Modula-3 by _scheme_compile.
# The resulting binary is an mscheme interpreter with the compiled
# modules linked in.  When you load a .scm file by name, the
# SchemeCompiledRegistry transparently uses the compiled code.

set -e

SCRIPTDIR="$(cd "$(dirname "$0")" && pwd)"
cd "$SCRIPTDIR"

CM3="${1:-cm3}"

# Determine target directory (e.g., ARM64_DARWIN, AMD64_LINUX)
TARGET=$("$CM3" -version 2>&1 | sed -n 's/.*target: *//p')
if [ -z "$TARGET" ]; then
    echo "error: cannot determine target from cm3 -version" >&2
    exit 1
fi

# Build
echo "=== Building numtest ==="
"$CM3" -build -override

# Run each test suite
NUMTEST="$TARGET/numtest"
if [ ! -x "$NUMTEST" ]; then
    echo "error: $NUMTEST not found after build" >&2
    exit 1
fi

PASS=0
FAIL=0

for f in test-numeric-tower.scm test-examples.scm test-r4rs-gaps.scm; do
    echo ""
    echo "=== Running $f ==="
    if "./$NUMTEST" "src/$f"; then
        PASS=$((PASS + 1))
    else
        FAIL=$((FAIL + 1))
    fi
done

echo ""
echo "=== Summary: $PASS suites passed, $FAIL suites failed ==="
exit $FAIL
