#! /usr/bin/env python
# $Id$

#
# User may want to edit the clean and/or m3cc steps.
#

import sys
import pylib
from pylib import *

def Hack1():

    # appropriate
    pylib.GCC_BACKEND = True
    pylib.OMIT_GCC = False

    # hack
    os.environ["GCC_BACKEND"] = "yes"
    os.environ.pop("OMIT_GCC", None)
    reload(pylib)

def Hack2():

    # appropriate
    pylib.GCC_BACKEND = False
    pylib.OMIT_GCC = True
    pylib.Target = "NT386GNU"

    # hack
    os.environ["OMIT_GCC"] = "yes"
    os.environ.pop("GCC_BACKEND", None)
    os.environ["CM3_TARGET"] = "NT386GNU"
    reload(pylib)

#
# These should be arrays of function pointers instead of strings.
#
argv_RealClean = [sys.argv[0], "realclean"] + sys.argv[1:]
argv_BuildShip = [sys.argv[0], "buildship"] + sys.argv[1:]
argv_Build = [sys.argv[0], "buildglobal"] + sys.argv[1:]

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
    #"mklib",
    "cm3",
    ]

P_lib = [
    "m3core",
    "libm3",
    ]

Hack1();

DoPackage(argv_BuildShip, P) or sys.exit(1)
ShipCompiler() or sys.exit(1)
CopyConfigForDevelopment() or sys.exit(1)

Hack2()

DoPackage(argv_RealClean, PackageSets["all"]) or sys.exit(1)
DoPackage(argv_BuildShip, P_lib + P) or sys.exit(1)
# not yet, not working
# ShipCompiler() or sys.exit(1)

# DoPackage(argv_BuildShip, PackageSets["std"]) or sys.exit(1)

print("%s: Success." % os.path.basename(sys.argv[0]))
