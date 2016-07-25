#! /usr/bin/env python

#!/bin/sh

#set -e
#set -x

#./make-dist-cfg.py $*
#./do-pkg.py m3cc buildship $*
#./do-cm3-all.py realclean skipgcc $*
#./do-pkg.py m3cc m3core libm3 buildship $*
#./upgrade.py skipgcc $*
#./do-cm3-all.py realclean skipgcc $*
#./do-cm3-all.py buildship $*

import os, sys
argv = sys.argv

def RemoveTrailingSpaces(a):
    while len(a) > 0 and a[-1] == ' ':
        a = a[:-1]
    return a

_CBackend = "c" in argv or "C" in argv

def Run(command):
    command = RemoveTrailingSpaces(command + " " + " ".join(argv[1:]))
    print("'" + command + "'")
    os.system(command) and sys.exit("ERROR: " + command)

# ./do-pkg.py doesn't like skipgcc plus just m3cc -- no packages to build
# Which is why this was rewritten in Python from Bourne shell.

Run("./make-dist-cfg.py")
if not _CBackend:
    Run("./do-pkg.py m3cc buildship")
Run("./do-cm3-all.py realclean skipgcc")
Run("./do-pkg.py m3cc m3core libm3 buildship")
Run("./upgrade.py skipgcc")
Run("./do-cm3-all.py realclean skipgcc")
Run("./do-cm3-all.py buildship")
