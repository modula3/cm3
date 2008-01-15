#! /usr/bin/env python
# $Id$

#
# It will be profitable for the user to make local edits
# to this file skip completed steps. This script is more
# guidance than exact automation.
#
# It would be greatly improved with some command line switches
# or automatic incrementality, or something like Olaf's idea
# where errors trigger more pessimistic behavior.
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

OriginalPath = os.environ["PATH"]

#
# Need to figure out how to do this properly, if at all.
#
SetupEnvironment()
CheckEnvironment()

P = [
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
    "cm3",
    "mklib",
    "m3cc",
    ]

DoPackage(argv_BuildShip, P) or sys.exit(1)
ShipCompiler() or sys.exit(1)
CopyConfigForDevelopment() or sys.exit(1)

Path = OriginalPath

PathPrefix = ""
EnvironmentModified = False

SetupEnvironment()

if NT:
    if not SearchPath("gcc") or not SearchPath("as"):
        PathPrefix = SystemDrive + "\\mingw\\bin;" + PathPrefix
    if not SearchPath("sh"):
        PathPrefix = SystemDrive + "\\msys\\1.0\\bin;" + PathPrefix

os.environ["CM3_TARGET"] = "NT386GNU"
print("set CM3_TARGET=NT386GNU")

os.environ["OMIT_GCC"] = "yes"
print("set OMIT_GCC=yes")
    
reload(pylib)

#
# Need to figure out how to do this properly, if at all.
#
SetupEnvironment()

# DoPackage(argv_RealClean, PackageSets["all"]) or sys.exit(1)

P = ["m3core", "libm3"] + P
DoPackage(argv_BuildShip, P) or sys.exit(1)
ShipCompiler() or sys.exit(1)
CopyConfigForDevelopment() or sys.exit(1)

# DoPackage(argv_BuildShip, PackageSets["std"]) or sys.exit(1)

print("%s: Success." % os.path.basename(sys.argv[0]))
