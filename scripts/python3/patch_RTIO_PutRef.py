# Patch out RTIO.PutRef for compatibility with older runtime.
# This was written by ChatGPT.

import sys
from pathlib import Path

def comment_out_putref(directory):
    directory = Path(directory)

    for file_path in directory.rglob("*.m3"):
        if not file_path.is_file():
            continue

        content = file_path.read_text(encoding="utf-8")

        lines = content.splitlines(keepends=True)
        modified = False

        for i, line in enumerate(lines):
            if "RTIO.PutRef" in line:
                stripped = line.rstrip("\r\n")

                # Avoid commenting out an already-commented line
                if not stripped.lstrip().startswith("(*"):
                    newline = line[len(stripped):]
                    lines[i] = f"(* {stripped} *){newline}"
                    modified = True

        if modified:
            file_path.write_text(
                "".join(lines),
                encoding="utf-8"
            )
            print(f"Modified: {file_path}")

comment_out_putref(sys.argv[1])
