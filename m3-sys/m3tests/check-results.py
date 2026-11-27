#!/usr/bin/env python3
"""Compare an m3tests run with the tests known to fail on this job.

usage: check-results.py RESULTS.xml KNOWN-FAILURES-FILE

RESULTS.xml is the m3tests-results.xml written by "cm3 -DHTML".  The
known-failures file lists test ids (p109, c142, ...), one or more per
line; "#" starts a comment.  A missing file means no known failures.

Exits non-zero, so that CI fails, when
  - a test fails that is not listed (a regression), or
  - a listed test passes or no longer exists (remove it from the list,
    so that the fix stays fixed).
"""

import re
import sys
import xml.etree.ElementTree as ET
from pathlib import Path


def results(path):
    """Return ({test id: failed?}) from an m3tests results file."""
    text = Path(path).read_text(errors="replace")
    tests = {}
    try:
        for case in ET.fromstring(text).iter("testcase"):
            tid = case.get("name", "").split(":")[0].strip()
            failed = case.find("failure") is not None or case.find("error") is not None
            tests[tid] = tests.get(tid, False) or failed
    except ET.ParseError:
        # Fall back to a textual scan if the file is not well-formed.
        for m in re.finditer(r'<testcase[^>]*name="([^":]+)[^>]*>(.*?)</testcase>', text, re.S):
            tid = m.group(1).strip()
            tests[tid] = tests.get(tid, False) or bool(re.search(r"<(failure|error)\b", m.group(2)))
    return tests


def known(path):
    p = Path(path)
    if not p.exists():
        return set()
    ids = set()
    for line in p.read_text().splitlines():
        ids.update(line.split("#", 1)[0].split())
    return ids


def main(argv):
    if len(argv) != 3:
        print(__doc__, file=sys.stderr)
        return 2
    res_path, known_path = argv[1], argv[2]
    if not Path(res_path).exists():
        print(f"::error::{res_path} not found: the m3tests run did not complete")
        return 1
    tests = results(res_path)
    if not tests:
        print(f"::error::{res_path} lists no tests")
        return 1
    failing = {t for t, f in tests.items() if f}
    expected = known(known_path)

    new = sorted(failing - expected)
    fixed = sorted(t for t in expected if t in tests and not tests[t])
    gone = sorted(t for t in expected if t not in tests)

    print(f"{len(tests)} tests, {len(failing)} failing; "
          f"{len(expected)} known failures in {known_path}")
    if failing & expected:
        print("known failures: " + " ".join(sorted(failing & expected)))
    for t in new:
        print(f"::error::{t} failed and is not a known failure (regression?)")
    for t in fixed:
        print(f"::error::{t} passed: remove it from {known_path}")
    for t in gone:
        print(f"::error::{t} is listed in {known_path} but was not run")
    return 1 if (new or fixed or gone) else 0


if __name__ == "__main__":
    sys.exit(main(sys.argv))
