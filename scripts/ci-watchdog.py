#!/usr/bin/env python3
"""Run a command, and if it goes silent for too long, dump it and fail.

usage: ci-watchdog.py [--idle SECONDS] [--dumps DIR] -- COMMAND [ARGS...]

For CI on Windows, where cm3 has occasionally hung without a word until
the job's time limit killed it.  The command's output is passed through.
When it prints nothing for --idle seconds (default 900), the watchdog
  - writes a full memory dump of every running cm3.exe with procdump, if
    procdump is on PATH, into --dumps (default ./dumps), and prints the
    threads' stacks with cdb, if the Windows debuggers are installed;
  - kills the command's process tree and exits 124.
Otherwise it exits with the command's own status.
"""

import argparse
import glob
import os
import shutil
import subprocess
import sys
import threading
import time


def log(msg):
    print(f"ci-watchdog: {msg}", flush=True)


def pids_of(image):
    """PIDs of running processes with the given image name (Windows)."""
    try:
        out = subprocess.run(["tasklist", "/FI", f"IMAGENAME eq {image}", "/FO", "CSV", "/NH"],
                             capture_output=True, text=True).stdout
    except OSError:
        return []
    pids = []
    for line in out.splitlines():
        fields = [f.strip('"') for f in line.split('","')]
        if len(fields) > 1 and fields[1].isdigit():
            pids.append(int(fields[1]))
    return pids


def find_cdb():
    for pat in [r"C:\Program Files (x86)\Windows Kits\10\Debuggers\x64\cdb.exe",
                r"C:\Program Files\Windows Kits\10\Debuggers\x64\cdb.exe"]:
        for path in glob.glob(pat):
            return path
    return shutil.which("cdb")


def dump_hung(dumps):
    pids = pids_of("cm3.exe")
    log(f"cm3.exe processes: {pids or 'none'}")
    procdump = shutil.which("procdump64") or shutil.which("procdump")
    if not procdump:
        log("procdump not found; no dumps written")
        return
    os.makedirs(dumps, exist_ok=True)
    cdb = find_cdb()
    for pid in pids:
        path = os.path.join(dumps, f"cm3-{pid}.dmp")
        log(f"dumping {pid} to {path}")
        subprocess.run([procdump, "-accepteula", "-ma", str(pid), path])
        if cdb and os.path.exists(path):
            log(f"thread stacks of {pid}:")
            subprocess.run([cdb, "-z", path, "-c", "~*k 40; q"])


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("--idle", type=int, default=900)
    ap.add_argument("--dumps", default="dumps")
    ap.add_argument("command", nargs=argparse.REMAINDER)
    args = ap.parse_args()
    cmd = args.command[1:] if args.command[:1] == ["--"] else args.command
    if not cmd:
        ap.error("no command")

    # Unbuffered, so a Python child (concierge.py) does not hold its output
    # back while it waits on a long compile.
    env = dict(os.environ, PYTHONUNBUFFERED="1")
    proc = subprocess.Popen(cmd, stdout=subprocess.PIPE, stderr=subprocess.STDOUT, env=env)
    last = [time.monotonic()]

    def pump():
        for chunk in iter(lambda: proc.stdout.read1(4096), b""):
            sys.stdout.buffer.write(chunk)
            sys.stdout.buffer.flush()
            last[0] = time.monotonic()

    reader = threading.Thread(target=pump, daemon=True)
    reader.start()
    while True:
        try:
            proc.wait(timeout=2)
            break
        except subprocess.TimeoutExpired:
            pass
        idle = time.monotonic() - last[0]
        if idle > args.idle:
            log(f"no output for {int(idle)} seconds: the command looks hung")
            dump_hung(args.dumps)
            if os.name == "nt":
                subprocess.run(["taskkill", "/T", "/F", "/PID", str(proc.pid)])
            else:
                proc.kill()
            proc.wait()
            reader.join(timeout=10)
            log("killed; failing the step")
            return 124
    reader.join(timeout=10)
    return proc.returncode


if __name__ == "__main__":
    sys.exit(main())
