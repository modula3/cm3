#!/usr/bin/env python3
"""
sweep.py — MSIR abandon sweep tool.

Captures per-test abandon messages (written to /tmp/msir-debug.txt by
MSIRBuilder.Abandon) and msir-verify errors (written to stderr).

Usage (run from repo root):
  python3 m3-sys/msir/test/sweep.py baseline [--p0] [--p1] [--p2]
      Run all tests, save results to sweep-baseline.json.

  python3 m3-sys/msir/test/sweep.py summary
      Print current baseline categorised by abandon message.

  python3 m3-sys/msir/test/sweep.py grep <pattern>
      List tests whose abandons match <pattern>.

  python3 m3-sys/msir/test/sweep.py rerun <pattern>
      Rebuild only tests that match <pattern> in the baseline, print results.

  python3 m3-sys/msir/test/sweep.py check
      Rebuild every test that had any abandons in the baseline, diff vs baseline.
"""

import argparse, json, os, re, signal, subprocess, sys
from pathlib import Path

REPO_ROOT   = Path(__file__).resolve().parent.parent.parent.parent
TESTS_SRC   = REPO_ROOT / "m3-sys" / "m3tests" / "src"
BASELINE    = Path(__file__).parent / "sweep-baseline.json"
DEBUG_FILE  = Path("/tmp/msir-debug.txt")
TEST_TIMEOUT = 60   # seconds; kills hanging test binaries (e.g. p267)

# ---------------------------------------------------------------------------
# Test discovery
# ---------------------------------------------------------------------------

def find_tests(prefixes):
    tests = []
    for prefix in prefixes:
        d = TESTS_SRC / prefix
        if not d.is_dir():
            continue
        for sub in sorted(d.iterdir()):
            if sub.is_dir():
                tests.append(sub)
    return tests

# ---------------------------------------------------------------------------
# Single-test runner
# ---------------------------------------------------------------------------

def run_one(path: Path) -> list[str]:
    """Build one test with MSIR; return list of abandon/verify message strings."""
    # Delete compiled objects so cm3 always recompiles and emits fresh MSIR.
    # Without this, a cached .o skips compilation entirely and no
    # abandon/verify messages are emitted, giving false-negative results.
    for build_dir in path.iterdir():
        if build_dir.is_dir() and not build_dir.name.startswith("."):
            for f in build_dir.glob("*.o"):
                f.unlink(missing_ok=True)

    before = DEBUG_FILE.stat().st_size if DEBUG_FILE.exists() else 0

    proc = subprocess.Popen(
        ["cm3", "@M3m3front-msir", "-build"],
        stdout=subprocess.PIPE, stderr=subprocess.PIPE, text=True,
        cwd=path, start_new_session=True,   # own process group → kills grandchildren
    )
    try:
        stdout, stderr = proc.communicate(timeout=TEST_TIMEOUT)
    except subprocess.TimeoutExpired:
        os.killpg(os.getpgid(proc.pid), signal.SIGKILL)
        proc.communicate()
        stdout, stderr = "", ""
        return ["TIMEOUT"]

    messages = []

    # Abandons written to the shared debug file by MSIRBuilder.Abandon
    if DEBUG_FILE.exists():
        with open(DEBUG_FILE, "rb") as f:
            f.seek(before)
            new = f.read().decode("utf-8", errors="replace")
        for line in new.splitlines():
            line = line.strip()
            if "msir-abandon:" in line:
                messages.append(line)

    # Verify errors on stderr
    for line in stderr.splitlines():
        line = line.strip()
        if "msir-verify:" in line:
            messages.append(line)

    return messages

# ---------------------------------------------------------------------------
# Subcommands
# ---------------------------------------------------------------------------

def cmd_baseline(args):
    prefixes = []
    if args.p0: prefixes.append("p0")
    if args.p1: prefixes.append("p1")
    if args.p2: prefixes.append("p2")
    if not prefixes:
        prefixes = ["p0", "p1", "p2"]

    tests = find_tests(prefixes)
    print(f"Running {len(tests)} tests…", flush=True)

    baseline = {}
    total_abandons = 0
    for i, path in enumerate(tests, 1):
        msgs = run_one(path)
        if msgs:
            baseline[path.name] = msgs
            total_abandons += len(msgs)
        print(f"  [{i:3}/{len(tests)}] {path.name}: {len(msgs)} abandons",
              flush=True)

    BASELINE.write_text(json.dumps(baseline, indent=2))
    print(f"\nBaseline saved to {BASELINE}")
    print(f"Tests with abandons: {len(baseline)}  |  Total abandon msgs: {total_abandons}")


def cmd_summary(args):
    if not BASELINE.exists():
        sys.exit(f"No baseline at {BASELINE} — run 'baseline' first.")
    baseline = json.loads(BASELINE.read_text())

    counts: dict[str, int] = {}
    for msgs in baseline.values():
        for m in msgs:
            # Extract the reason after "msir-abandon: " or "msir-verify: "
            key = re.sub(r'^.*msir-abandon:\s*', '', m)
            key = re.sub(r'^.*msir-verify:\s*', 'VERIFY: ', key)
            counts[key] = counts.get(key, 0) + 1

    total = sum(counts.values())
    print(f"Baseline: {len(baseline)} tests with abandons, {total} total messages\n")
    for msg, n in sorted(counts.items(), key=lambda x: -x[1]):
        print(f"  {n:4}  {msg}")


def cmd_grep(args):
    if not BASELINE.exists():
        sys.exit(f"No baseline at {BASELINE} — run 'baseline' first.")
    baseline = json.loads(BASELINE.read_text())
    pat = re.compile(args.pattern, re.IGNORECASE)

    for test, msgs in sorted(baseline.items()):
        matched = [m for m in msgs if pat.search(m)]
        if matched:
            print(f"{test}: {len(matched)}")
            for m in matched:
                print(f"    {m}")


def cmd_rerun(args):
    if not BASELINE.exists():
        sys.exit(f"No baseline at {BASELINE} — run 'baseline' first.")
    baseline = json.loads(BASELINE.read_text())
    pat = re.compile(args.pattern, re.IGNORECASE)

    targets = {name: msgs for name, msgs in baseline.items()
               if any(pat.search(m) for m in msgs)}
    if not targets:
        print("No tests match that pattern in the baseline.")
        return

    print(f"Re-running {len(targets)} tests…\n", flush=True)
    improved = unchanged = regressed = 0
    for name in sorted(targets):
        # Find the test directory (could be under p0/p1/p2)
        path = _find_test_path(name)
        if path is None:
            print(f"  {name}: directory not found")
            continue

        before_msgs = targets[name]
        before_n    = len(before_msgs)
        after_msgs  = run_one(path)
        after_n     = len(after_msgs)

        if after_n < before_n:
            symbol = "✓"
            improved += 1
        elif after_n == before_n:
            symbol = "="
            unchanged += 1
        else:
            symbol = "✗"
            regressed += 1

        print(f"  {symbol} {name}: {before_n} → {after_n}")
        new_only = set(after_msgs) - set(before_msgs)
        for m in sorted(new_only):
            print(f"      NEW: {m}")

    print(f"\nImproved: {improved}  Unchanged: {unchanged}  Regressed: {regressed}")


def cmd_check(args):
    if not BASELINE.exists():
        sys.exit(f"No baseline at {BASELINE} — run 'baseline' first.")
    baseline = json.loads(BASELINE.read_text())

    if not baseline:
        print("Baseline has no abandons — nothing to check.")
        return

    print(f"Re-running {len(baseline)} tests with prior abandons…\n", flush=True)
    improved = unchanged = regressed = 0
    for name in sorted(baseline):
        path = _find_test_path(name)
        if path is None:
            print(f"  {name}: directory not found")
            continue

        before_msgs = baseline[name]
        after_msgs  = run_one(path)

        delta = len(after_msgs) - len(before_msgs)
        if delta < 0:
            symbol = "✓"
            improved += 1
        elif delta == 0:
            symbol = "="
            unchanged += 1
        else:
            symbol = "✗"
            regressed += 1

        if delta != 0 or args.verbose:
            print(f"  {symbol} {name}: {len(before_msgs)} → {len(after_msgs)}")
            for m in sorted(set(after_msgs) - set(before_msgs)):
                print(f"      NEW: {m}")
            for m in sorted(set(before_msgs) - set(after_msgs)):
                print(f"      GONE: {m}")

    print(f"\nImproved: {improved}  Unchanged: {unchanged}  Regressed: {regressed}")


# ---------------------------------------------------------------------------
# Helpers
# ---------------------------------------------------------------------------

def _find_test_path(name: str) -> Path | None:
    for prefix in ("p0", "p1", "p2"):
        p = TESTS_SRC / prefix / name
        if p.is_dir():
            return p
    return None

# ---------------------------------------------------------------------------
# Entry point
# ---------------------------------------------------------------------------

def main():
    ap = argparse.ArgumentParser(description=__doc__,
                                 formatter_class=argparse.RawDescriptionHelpFormatter)
    sub = ap.add_subparsers(dest="cmd", required=True)

    bl = sub.add_parser("baseline", help="Run all tests and save baseline")
    bl.add_argument("--p0", action="store_true", help="Include p0 tests")
    bl.add_argument("--p1", action="store_true", help="Include p1 tests")
    bl.add_argument("--p2", action="store_true", help="Include p2 tests")

    sub.add_parser("summary", help="Show baseline categorised by message")

    gr = sub.add_parser("grep", help="List tests matching a pattern")
    gr.add_argument("pattern")

    rr = sub.add_parser("rerun", help="Rebuild tests matching a pattern")
    rr.add_argument("pattern")

    ck = sub.add_parser("check", help="Rebuild all abandoning tests, diff vs baseline")
    ck.add_argument("-v", "--verbose", action="store_true",
                    help="Show unchanged tests too")

    args = ap.parse_args()
    {
        "baseline": cmd_baseline,
        "summary":  cmd_summary,
        "grep":     cmd_grep,
        "rerun":    cmd_rerun,
        "check":    cmd_check,
    }[args.cmd](args)


if __name__ == "__main__":
    main()
