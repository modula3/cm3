#! /usr/bin/env python
# $Id: bootntgnu.py,v 1.20 2008-02-24 12:22:01 jkrell Exp $

import sys
import pylib
from pylib import *

# hack
os.environ["CM3_TARGET"] = "NT386"
os.environ["CM3_OSTYPE"] = "POSIX"
os.environ["CM3_GCC_BACKEND"] = "yes"
# os.environ["OMIT_GCC"] = "yes"
reload(pylib)

#
# These should be arrays of function pointers instead of strings.
#
argv_RealClean = [sys.argv[0], "realclean"] + sys.argv[1:]
argv_BuildShip = [sys.argv[0], "buildship"] + sys.argv[1:]

# DoPackage(argv_RealClean, PackageSets["all"]) or sys.exit(1)

#
# There may not be a backend at all, so build it first.
#
DoPackage(argv_BuildShip, ["m3cc"]) or sys.exit(1)

DoPackage(
    argv_BuildShip, [ "m3core", "libm3", "sysutils", "import-libs",
    "m3bundle", "m3middle", "m3quake", "m3middle", "m3objfile",
    "m3linker", "m3back", "m3staloneback", "m3front",
    "mklib", "cm3",
    ]) or sys.exit(1)

ShipCompiler() or sys.exit(1)

DoPackage(argv_BuildShip, PackageSets["std"]) or sys.exit(1)

print("%s: Success." % os.path.basename(sys.argv[0]))
