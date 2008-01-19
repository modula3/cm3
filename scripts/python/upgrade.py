#! /usr/bin/env python
# $Id: upgrade.py,v 1.8 2008-01-19 04:05:55 jkrell Exp $

import sys
import pylib
from pylib import *

argv_RealClean = [sys.argv[0], "realclean"] + sys.argv[1:]
argv_BuildShip = [sys.argv[0], "buildship"] + sys.argv[1:]

#
# Need to figure out how to do this properly, if at all.
#
SetupEnvironment()

CopyConfigForDevelopment() or sys.exit(1)

#
# clean everything
#

DoPackage(
    argv_RealClean,
    [
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
    ]) or sys.exit(1)

#
# Now build the compiler with the installed version of the runtime;
# do _not_ compile m3core and libm3 here.
# We start with the front end...
#
DoPackage(
    argv_BuildShip,
    [
    "import-libs",
    "m3middle",
    "m3objfile",
    "m3linker",
    "m3back",
    "m3staloneback",
    "m3front",
    "m3quake",
    "cm3",
    "mklib",
    ]) or sys.exit(1)

#
# ... and continue with the backend, if needed
#
FilterPackages([ "m3cc" ]) and DoPackage(argv_BuildShip, [ "m3cc" ])

#
# Up to now, the compiler binaries have not been installed.
# We do this now but keep backups of the old ones. (not yet)
#
ShipCompiler() or sys.exit(1)
CopyConfigForDevelopment() or sys.exit(1)

#
# Now try the new compiler but building the core system (without
# m3cc, as this is written in C) from scratch with it.
#
os.environ["OMIT_GCC"] = "yes"
reload(pylib)

DoPackage(argv_RealClean, pylib.PackageSets["core"]) or sys.exit(1)
DoPackage(argv_BuildShip, pylib.PackageSets["core"]) or sys.exit(1)

#
# If everything has been successfull, we do another compiler upgrade.
#
ShipCompiler() or sys.exit(1)

print("%s: Success." % os.path.basename(sys.argv[0]))
