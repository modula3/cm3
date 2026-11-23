#! /usr/bin/env python3

# TODO: boot2 and upgrade2 are about the same same. Cleanup.
# upgrade/upgrade2 only build the compiler, boot2 builds everything.
#
# Differences:
#  boot2 reuses upgrade.
#  boot2 starts with building a current libm3/m3core
#   This is a mistake and motivated upgrade2.
#  upgrade starts with an ambient libm3/core
#  upgrade2 is like upgrade but in the style of boot2 -- runs Python commands instead of in-proc.

# Boot2 is used historically mostly for bootstrapping the *current* system
# to a new system, a new port, from current source, where current
# libm3/m3core are correct. Boot1 provides only the compiler.
#
# This should probably be changed to have boot1 provide also libm3/m3core,
# erasing the difference between boot2 and upgrade.
#
# The out-of-proc nature of upgrade2 is OK though and upgrade can switch to it.

# Starting with a working compiler and its runtime (m3core/libm3), and some config,
# 1. build a current compiler
# 2. with current compiler, build itself and m3core/libm3#
#
# Currently callers use HEAD config which is debatable.
# In the distant past (3.x,4.x), bootstrap configs aged poorly,
# and therefore HEAD config development has been generally aimed
# at replacing them and being very compatible. Perhaps bootstrap
# configs are in better state now and will work.
#
# This is like upgrade2.py but:
#  upgrade.py does everything in-process, using commands formed early.
#  That is a nice optimization, perhaps.
#  We desire to add -DM3BOOT2 sometimes, not others -- dynamic commands.
#  This is presently difficult in-process, DoPackage() is inflexible.
#  Therefore, restart the process repeatedly, i.e. run do-pkg.py command.
#
# As well, the filtering is a little broken and we end up
# running commands with no packages, which fails. To workaround this, remove m3cc support
#
# Longer term, the command formation in pylib.py should be run
# by passing more parameters around instead of in global initialization, and then upgrade.py can work.
# And accomodate NoPackages sometimes.
#
import sys
import pylib
from pylib import *
argv = sys.argv

clean = not ("noclean" in sys.argv)

packages_compiler = " m3bundle m3middle m3quake m3objfile set m3linker m3back m3front sysutils cm3 mklib "
# m3cgen generates .h file for gcc backend (m3cc)
# m3cgcat reads/writes binary/ascii/C m3cg (IR) and is only for very active compiler development
# packages_compiler += " m3cgcat  "
# packages_compiler += " m3cggen "
packages_runtime  = " m3core libm3 "

SetupEnvironment()

def RemoveTrailingSpaces(a):
    while len(a) > 0 and a[-1] == ' ':
        a = a[:-1]
    return a

def Posix():
    return os.name == "posix"

pyexe = ""
if not Posix() and env_OS == "Windows_NT":
    pyexe = (pylib.SearchPath("python.exe") or pylib.SearchPath("python3.exe") or pylib.SearchPath("py.exe") or pylib.SearchPath("python2.exe")) + " "

def Run(command):
    command = RemoveTrailingSpaces(command + " " + " ".join(argv[1:]))
    print("'" + command + "'")
    os.system(command) and sys.exit("ERROR: " + command)

# clean everything

if clean: Run(pyexe + "./do-pkg.py realclean " + packages_compiler + packages_runtime)

# Build the compiler with the installed version of the runtime;
# do _not_ compile m3core and libm3 here.
# We start with the front end.
#
# -DM3BOOT2 so HEAD config tries to accomodate old compiler.
#
Run(pyexe + "./do-pkg.py -DM3BOOT2 buildship " + packages_compiler)

# Up to now, the compiler binaries have not been installed.
#
# Workarounds:
#  cm3 does not ship itself because it might be in use,
#    depending on operating system capabilities and workarounds.
#    This is a long standing implementation limit.
#
ShipFront() or sys.exit(1)

# Now try the new compiler but building the core system from scratch with it, without M3BOOT2.
#
CopyConfigForDistribution(InstallRoot) or sys.exit(1)
Run(pyexe + "./do-pkg.py realclean " + packages_compiler + packages_runtime)
Run(pyexe + "./do-pkg.py buildship " + packages_compiler + packages_runtime)
ShipFront() or sys.exit(1)

print("%s: Success." % os.path.basename(sys.argv[0]))
