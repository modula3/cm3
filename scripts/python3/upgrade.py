#! /usr/bin/env python

import sys
import pylib
from pylib import *

cleangcc = not ("nocleangcc" in sys.argv)
clean = not ("noclean" in sys.argv)
argv_RealClean = [sys.argv[0], "realclean"] + sys.argv[1:]
argv_BuildShip = [sys.argv[0], "buildship"] + sys.argv[1:]

SetupEnvironment()

# delete lingering cm3cg in case old compiler/config uses it
for a in ["cm3cg", "gcc/m3cgc1"]:
    for b in ["", ".exe"]:
        DeleteFile(Root + "/m3-sys/m3cc/" + BuildDir + "/" + a + b)

#
# clean everything
#

if clean:
    DoPackage(
        argv_RealClean, [ "m3bundle", "m3middle", "m3quake", "m3objfile",
                          "set", "m3linker", "m3back", "m3front", "sysutils", "cm3",
                          "m3cgcat", "m3cggen", "mklib", "m3core", "libm3",
        ]) or sys.exit(1)
if cleangcc:
    DoPackage(argv_RealClean, [ "m3cc" ]) or sys.exit(1)

#
# Now build the compiler with the installed version of the runtime;
# do _not_ compile m3core and libm3 here.
# We start with the front end...
#
DoPackage(argv_BuildShip, [ "m3bundle", "m3middle", "m3quake", "m3objfile",
                            "set", "m3linker", "m3back", "m3front", "sysutils",
                            "cm3", "m3cggen", "mklib", "m3cgcat"
    ]) or sys.exit(1)

#
# ... and continue with the backend, if needed
#

a = Root + "/m3-sys/m3cggen/" + BuildDir + "/m3cggen > " + Root + "/m3-sys/m3cc/gcc/gcc/m3cg/m3cg.h"
print(a)
# os.system(a)
FilterPackages([ "m3cc" ]) and DoPackage(argv_BuildShip, [ "m3cc" ])

#
# Up to now, the compiler binaries have not been installed.
# We do this now but keep backups of the old ones. (not yet)
#
# Workarounds:
#  cm3 does not ship itself because it might be in use,
#    depending on operating system capabilities and workarounds.
#    This is a long standing implementation limit.
#  cm3cg does not ship itself in some versions. This is a bug.
#
#ShipFront() or sys.exit(1)
ShipCompiler() or sys.exit(1)

#
# Now try the new compiler but building the core system (without
# m3cc, as this is written in C) from scratch with it.
#
OMIT_GCC = True
os.environ["OMIT_GCC"] = "yes"
CopyConfigForDistribution(InstallRoot) or sys.exit(1)

# once more

DoPackage(argv_RealClean, [ "m3bundle", "m3middle", "m3quake", "m3objfile",
                            "set", "m3linker", "m3back", "m3front", "sysutils",
                            "cm3", "mklib", "m3core", "libm3"
    ]) or sys.exit(1)

DoPackage(argv_BuildShip, [ "m3bundle", "m3middle", "m3quake", "m3objfile",
                            "set", "m3linker", "m3back", "m3front", "sysutils",
                            "cm3", "mklib", "m3core", "libm3"
    ]) or sys.exit(1)

ShipFront() or sys.exit(1)

print("%s: Success." % os.path.basename(sys.argv[0]))
