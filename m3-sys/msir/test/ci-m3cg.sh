#!/bin/bash
# MSIR through M3CG (MSIRToM3CG) on a target the LLVM ladder does not cover:
# a 32-bit target, with ex_frame exceptions.  Run from the repository root
# with the bootstrap release's cm3 on PATH and CM3_TARGET set.
#
#   Build    concierge upgrade with the C backend, and tcp (for p204).
#   Tests    m3tests with every cm3 run through the MSIR front end and
#            MSIRToM3CG (@M3m3front-msir @M3msircg); a unit MSIRToM3CG cannot
#            lower falls back to the C walk, and is counted.
#
# The run is judged against m3tests/known-failures/$CM3_TARGET-msircg.
set -u
ROOT=$(pwd)
WORK=${WORK:-$ROOT/../msir-m3cg}
status=0
log() { printf '\n=== %s ===\n' "$*"; }
fail() { echo "::error::$*"; status=1; }
mkdir -p "$WORK/bin"

log "Build: concierge upgrade (C backend)"
scripts/concierge.py upgrade --backend c || { fail "upgrade failed"; exit 1; }
# m3tests' p204 imports tcp, which upgrade does not build
scripts/concierge.py buildship tcp || { fail "tcp failed"; exit 1; }
REAL=$(command -v cm3)

# A cm3 that compiles through MSIRToM3CG and collects the per-unit
# "msir-cg:" lines (lowered or fallback) in $WORK/msircg.log.
LOGF=$WORK/msircg.log
: > "$LOGF"
cat > "$WORK/bin/cm3" <<W
#!/bin/bash
exec 3>&1
"$REAL" @M3m3front-msir @M3msircg @M3msircgverbose "\$@" 2>&1 1>&3 3>&- | tee -a "$LOGF" | grep -v '^msir-cg:' >&2
exit \${PIPESTATUS[0]}
W
chmod +x "$WORK/bin/cm3"

log "m3tests through MSIRToM3CG"
(
  export PATH=$WORK/bin:$PATH
  cd "$ROOT/m3-sys/m3tests"
  cm3 -DHTML
) || true
RES=$ROOT/m3-sys/m3tests/m3tests-results.xml
if [ -f "$RES" ]; then
  python3 "$ROOT/m3-sys/m3tests/check-results.py" "$RES" \
    "$ROOT/m3-sys/m3tests/known-failures/$CM3_TARGET-msircg" \
    || fail "m3tests: failures that are not known (see above)"
else
  fail "m3tests: no results file"
fi

log "MSIRToM3CG coverage"
echo "units lowered: $(grep -c ' lowered$' "$LOGF")"
echo "units fallen back: $(grep -c ' fallback:' "$LOGF")"
grep ' fallback:' "$LOGF" | sed 's/^msir-cg: [^ ]* //' | sort | uniq -c | sort -rn | head -30
exit $status
