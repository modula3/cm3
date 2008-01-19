#! /usr/bin/env python
# $Id$

#
# User may want to edit the clean and/or m3cc steps.
#

import sys
import pylib
from pylib import *

#
# These should be arrays of function pointers instead of strings.
#
argv_RealClean = [sys.argv[0], "realclean"] + sys.argv[1:]
argv_BuildShip = [sys.argv[0], "buildship"] + sys.argv[1:]

# DoPackage(argv_RealClean, PackageSets["all"]) or sys.exit(1)

P = [
    "import-libs",
    "m3cc",
    "m3bundle",
    "m3middle",
    "m3quake",
    "m3middle",
    "m3objfile",
    "m3linker",
    "m3back",
    "m3staloneback",
    "m3front",
    "m3quake",
    "cm3",
    "mklib",
    ]

pylib.GCC_BACKEND = True
pylib.OMIT_GCC = False

DoPackage(argv_BuildShip, P) or sys.exit(1)
ShipCompiler() or sys.exit(1)
CopyConfigForDevelopment() or sys.exit(1)

pylib.Target = "NT386GNU"
pylib.GCC_BACKEND = False
pylib.OMIT_GCC = True

DoPackage(argv_RealClean, PackageSets["all"]) or sys.exit(1)

P = ["m3core", "libm3"] + P
DoPackage(argv_BuildShip, P) or sys.exit(1)

#
# problem needs fixing here
# ShipCompiler() or sys.exit(1)
#

CopyConfigForDevelopment() or sys.exit(1)

# DoPackage(argv_BuildShip, PackageSets["std"]) or sys.exit(1)

print("%s: Success." % os.path.basename(sys.argv[0]))
