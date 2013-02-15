#! /usr/bin/env python

# This stub works around broken CVS compression at opencsw.org.
# Put in $HOME/bin/cvs.

import os, sys

for cvs in ["/opt/csw/cvs-feature/bin/cvs",
            "/opt/csw/bin/cvs",
            "/usr/bin/cvs"]:
  if os.access(cvs, os.X_OK):
    break;
if not os.access(cvs, os.X_OK):
  sys.stderr.write(cvs + " not executable\n")
  sys.exit(1)

if "-z3" in sys.argv:
  sys.argv.remove("-z3")

sys.argv[0] = cvs
print(" ".join(sys.argv)) # won't show quotes, ok
os.execvp(cvs, sys.argv)
