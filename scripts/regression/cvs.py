#! /usr/bin/env python

# This stub works around broken CVS compression on current10x.opencsw.org.
# Put in $HOME/bin/cvs.

import os, sys

CVSEXE = "/opt/csw/cvs-feature/bin/cvs"

if not os.access(CVSEXE, os.X_OK):
  CVSEXE = "/opt/csw/bin/cvs"

if not os.access(CVSEXE, os.X_OK):
  CVSEXE = "/usr/bin/cvs"

if not os.access(CVSEXE, os.X_OK):
  sys.stderr.write(CVSEXE + " not executable\n")
  sys.exit(1)

if os.uname()[1] == "current10x" and "-z3" in sys.argv:
  sys.argv.remove("-z3")

sys.argv[0] = CVSEXE
print(" ".join(sys.argv)) # won't show quotes, ok
os.execvp(CVSEXE, sys.argv)
