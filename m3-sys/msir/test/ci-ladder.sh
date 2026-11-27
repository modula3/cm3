#!/usr/bin/env bash
# MSIR validation ladder: the three-stage bootstrap plus m3tests.
#
# Starts from an installed cm3 (e.g. a bootstrap release) at $CM3_INSTALL
# with $CM3_INSTALL/bin on PATH.  Run from the repository root.
#
#   Stage 1  C backend: concierge full-upgrade (base set), with ex_stack
#            on every target (AMD64_LINUX since #1418).
#   Stage 2  Switch the installed config to MSIRObj.  The stage-1 cm3
#            rebuilds the compiler packages through MSIR -> LLVM and the
#            result is installed: a cm3 whose own code is all MSIR.
#            IR emitted by stage 1 is saved as ir-1.
#   Tests    Full m3tests under the stage-2 cm3, in MSIRObj mode.
#   Stage 3  The stage-2 cm3 rebuilds the compiler packages (ir-2) and the
#            result is installed and smoke-tested.  The stage-3 cm3
#            rebuilds them once more (ir-3).
#   Fixpoint ir-1, ir-2 and ir-3 must be identical.
#
# Environment: CM3_TARGET (ARM64_DARWIN | AMD64_DARWIN | AMD64_LINUX),
#              CM3_INSTALL, optional WORK (scratch dir, default ../msir-ladder),
#              optional M3TESTS_TIMEOUT (seconds, default 5400).
# Exit status is non-zero if any m3test errors, the smoke test fails, or
# the IR differs between stages.

set -euo pipefail

: "${CM3_TARGET:?}" "${CM3_INSTALL:?}"
ROOT=$(pwd)
WORK=${WORK:-$ROOT/../msir-ladder}
M3TESTS_TIMEOUT=${M3TESTS_TIMEOUT:-5400}
SRC_CFG=$ROOT/m3-sys/cminstall/src/config/$CM3_TARGET
INST_CFG=$CM3_INSTALL/bin/config/$CM3_TARGET
mkdir -p "$WORK"

# Packages rebuilt through MSIR, in dependency order (scripts/pkginfo.txt).
PKGS=(m3-libs/m3core m3-libs/libm3 m3-libs/set m3-libs/sysutils m3-comm/tcp
      m3-sys/m3middle m3-sys/m3objfile m3-sys/m3linker m3-sys/m3back
      m3-sys/msir m3-sys/m3quake m3-sys/m3front m3-sys/cm3)

status=0
log() { printf '\n=== %s ===\n' "$*"; }
fail() { echo "::error::$*"; status=1; }

# set_cfg FILE MODE WALKER: set M3_BACKEND_MODE in a target config and,
# if WALKER is 1, uncomment M3_USE_STACK_WALKER = TRUE.  Only uncommented
# assignments are rewritten, so "% M3_BACKEND_MODE = ..." lines stay put.
set_cfg() {
  MODE=$2 WALKER=$3 perl -i -pe '
    s/^(\s*)M3_BACKEND_MODE\s*=\s*"[^"]*"/$1M3_BACKEND_MODE = "$ENV{MODE}"/;
    s/^%\s*(M3_USE_STACK_WALKER\s*=\s*TRUE)/$1/ if $ENV{WALKER};
  ' "$1"
  grep -nE '^\s*(M3_BACKEND_MODE|M3_USE_STACK_WALKER)\s*=' "$1" || true
}

install_cm3() {
  cp "$ROOT/m3-sys/cm3/$CM3_TARGET/cm3" "$CM3_INSTALL/bin/cm3"
  if [ "$(uname)" = Darwin ]; then codesign -f -s - "$CM3_INSTALL/bin/cm3"; fi
}

# build_through_msir OUTDIR: clean-build and ship PKGS with the installed
# cm3, then copy every emitted LLVM IR file to OUTDIR/<package>/.
# MSIRObj writes its textual LLVM IR as <unit>.mb / <unit>.ib and deletes
# it after llc unless -keep is given.  tcp is included because p204
# imports it, and m3core's ABI link guard (RTAbiCG.c) refuses to link a
# C-backend object against an MSIR-built runtime.
build_through_msir() {
  local out=$1 p
  rm -rf "$out"; mkdir -p "$out"
  for p in "${PKGS[@]}"; do
    echo "--- $p"
    (cd "$ROOT/$p" && cm3 -clean >/dev/null && cm3 -build -keep && cm3 -ship)
    mkdir -p "$out/$p"
    find "$ROOT/$p/$CM3_TARGET" -maxdepth 1 \( -name '*.mb' -o -name '*.ib' \) -exec cp {} "$out/$p/" \;
  done
  local n; n=$(find "$out" \( -name '*.mb' -o -name '*.ib' \) | wc -l | tr -d ' ')
  echo "saved $n LLVM IR files (.mb/.ib) to $out"
  [ "$n" -gt 0 ] || fail "no LLVM IR (.mb/.ib) kept; is MSIRObj mode active?"
}

smoke() {
  local d="$WORK/smoke"
  rm -rf "$d"; mkdir -p "$d/src"
  cat >"$d/src/Main.m3" <<'EOF'
MODULE Main;
IMPORT IO, Fmt;
VAR t := NEW(REF ARRAY OF INTEGER, 10);  s := 0;
BEGIN
  FOR i := FIRST(t^) TO LAST(t^) DO t[i] := i * i; INC(s, t[i]) END;
  TRY RAISE IO.Error EXCEPT IO.Error => INC(s) END;
  IO.Put("smoke " & Fmt.Int(s) & "\n");
END Main.
EOF
  printf 'import("libm3")\nimplementation("Main")\nprogram("smoke")\n' >"$d/src/m3makefile"
  # -keep leaves the LLVM IR (Main.mb) in $WORK/smoke for the CI artifact.
  (cd "$d/src" && cm3 -build -keep) || { fail "smoke: build failed"; return; }
  local got; got=$("$d/$CM3_TARGET/smoke") || { fail "smoke: run failed"; return; }
  [ "$got" = "smoke 286" ] || fail "smoke: expected 'smoke 286', got '$got'"
  echo "$got"
}

log "Stage 1: C backend"
# AMD64_LINUX now turns ex_stack on for the C backend itself (#1418), and
# the d5.12.0 bootstrap already has ex_stack, so one full-upgrade suffices
# on every target.
set_cfg "$SRC_CFG" C 1
scripts/concierge.py full-upgrade --backend c base
cm3 -version 2>&1 | sed -n 1,3p   # not head: an early close makes cm3 fail with EPIPE

log "Stage 2: stage-1 cm3 builds the compiler through MSIR"
set_cfg "$INST_CFG" MSIRObj 1
# cm3.cfg, written by concierge, sets the mode before the target config is
# read; AMD64_LINUX turns ex_stack on from it (#1418) and has no mode line of
# its own for set_cfg to rewrite.
set_cfg "$CM3_INSTALL/bin/cm3.cfg" MSIRObj 1
build_through_msir "$WORK/ir-1"
install_cm3

log "m3tests under the stage-2 cm3 (MSIRObj)"
(
  cd "$ROOT/m3-sys/m3tests"
  if command -v timeout >/dev/null; then T=timeout
  elif command -v gtimeout >/dev/null; then T=gtimeout
  else T=""; fi
  ${T:+$T $M3TESTS_TIMEOUT} cm3 -DHTML
) || echo "cm3 -DHTML exited non-zero (checking results file)"
RES=$ROOT/m3-sys/m3tests/m3tests-results.xml
if [ -f "$RES" ]; then
  head -c 2000 "$RES" | grep -m1 -oE '<testsuite[^>]*>' || true
  # As master's CI does: fail on a test that fails and is not listed in
  # known-failures/<TARGET>-msir, or on a listed test that now passes.
  python3 "$ROOT/m3-sys/m3tests/check-results.py" "$RES" \
    "$ROOT/m3-sys/m3tests/known-failures/$CM3_TARGET-msir" \
    || fail "m3tests: failures that are not known (see above)"
else
  fail "m3tests: no results file"
fi

log "Stage 3: stage-2 cm3 rebuilds the compiler"
build_through_msir "$WORK/ir-2"
install_cm3
log "Smoke test of the stage-3 cm3"
smoke

log "Stage-3 cm3 rebuilds the compiler (IR only compared)"
build_through_msir "$WORK/ir-3"

log "Fixpoint"
for pair in "ir-1 ir-2" "ir-2 ir-3"; do
  set -- $pair
  if diff -rq "$WORK/$1" "$WORK/$2" >"$WORK/diff-$1-$2.txt"; then
    echo "$1 == $2 ($(find "$WORK/$1" -type f | wc -l | tr -d ' ') files)"
  else
    fail "fixpoint: $1 and $2 differ ($(wc -l <"$WORK/diff-$1-$2.txt" | tr -d ' ') files)"
    head -20 "$WORK/diff-$1-$2.txt"
    # Keep both versions of each differing file (at most 20) for the CI
    # artifact, so the difference can be read without a rerun.
    sed -n 's/^Files \(.*\) and \(.*\) differ$/\1/p' "$WORK/diff-$1-$2.txt" | head -20 |
    while read -r f; do
      rel=${f#"$WORK/$1/"}
      for st in "$1" "$2"; do
        mkdir -p "$WORK/fixpoint-diff/$st/$(dirname "$rel")"
        cp "$WORK/$st/$rel" "$WORK/fixpoint-diff/$st/$rel"
      done
    done
  fi
done

exit $status
