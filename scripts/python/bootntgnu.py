#! /usr/bin/env python
# $Id$

import sys
import pylib
from pylib import *

# appropriate but doesn't work currently
pylib.Target = "NT386GNU"

# hack
os.environ["CM3_TARGET"] = "NT386GNU"
reload(pylib)

#
# These should be arrays of function pointers instead of strings.
#
argv_RealClean = [sys.argv[0], "realclean"] + sys.argv[1:]
argv_BuildShip = [sys.argv[0], "buildship"] + sys.argv[1:]

DoPackage(argv_RealClean, PackageSets["all"]) or sys.exit(1)

#
# There may not be a backend at all, so build it first.
#
DoPackage(argv_BuildShip, ["m3cc"]) or sys.exit(1)

DoPackage(
    "m3core",
    "libm3",
    "import-libs",
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
    #"mklib", # not yet working
    "cm3",
    argv_BuildShip,
    ) or sys.exit(1)

# not yet, not working
# ShipCompiler() or sys.exit(1)


# DoPackage(argv_BuildShip, PackageSets["std"]) or sys.exit(1)


print("%s: Success." % os.path.basename(sys.argv[0]))
