#! /usr/bin/env python
# $Id$

import sys
import pylib
from pylib import *

#
# These should be arrays of function pointers instead of strings.
#
argv_RealClean = [sys.argv[0], "realclean"] + sys.argv[1:]
argv_BuildShip = [sys.argv[0], "buildship"] + sys.argv[1:]

# DoPackage(argv_RealClean, PackageSets["all"]) or sys.exit(1)

NT = (os.environ.get("OS") == "Windows_NT")
SystemDrive = os.environ.get("SystemDrive", "")
Msdev = SystemDrive + "\\msdev\\80"
VC = SystemDrive + "\\msdev\\80\\VC"

OriginalPath = os.environ["PATH"]
Path = OriginalPath

if NT:
    #if os.environ.get("INCLUDE", "") == "":
    os.environ["INCLUDE"] = Msdev + "\\include"
    #if os.environ.get("LIB", "") == "":
    os.environ["LIB"] = Msdev + "\\lib;" + InstallRoot + "\\lib"
    #if not SearchPath("cl") or not SearchPath("link"):
    Path = Msdev + "\\Common7\IDE;" + Path
    Path = VC + "\\bin;" + Path

os.environ["PATH"] = Path

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

# DoPackage(argv_BuildShip, P) or sys.exit(1)
# ShipCompiler() or sys.exit(1)
# CopyConfigForDevelopment() or sys.exit(1)

if False:
    os.system("set CM")
    os.system("set IN")
    os.system("set LI")
    os.system("set PA")
    os.system("cl")
    os.system("link")

Path = OriginalPath

if NT:
    Path = SystemDrive + "\\mingw\\bin;" + Path
    Path = SystemDrive + "\\msys\\1.0\\bin;" + Path

os.environ["CM3_TARGET"] = "NT386GNU"
os.environ["OMIT_GCC"] = "yes"
if "LIB" in os.environ:
    del(os.environ["LIB"])
if "INCLUDE" in os.environ:
    del(os.environ["INCLUDE"])
os.environ["PATH"] = Path

reload(pylib)

if False:
    os.system("set CM")
    os.system("set IN")
    os.system("set LI")
    os.system("set PA")
    #os.system("set")
    os.system("cl")
    os.system("link")
    os.system("gcc")
    os.system("ld")
    os.system("make")
    os.system("sed")
    os.system("gawk")
    os.system("sh -c \"echo sh\"")

# DoPackage(argv_RealClean, PackageSets["all"]) or sys.exit(1)
DoPackage(argv_BuildShip, P) or sys.exit(1)
# ShipCompiler() or sys.exit(1)
# CopyConfigForDevelopment() or sys.exit(1)

DoPackage(argv_BuildShip, PackageSets["std"]) or sys.exit(1)

print("%s: Success." % os.path.basename(sys.argv[0]))
