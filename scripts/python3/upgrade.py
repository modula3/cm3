#! /usr/bin/env python3
#
# Upgrade a working compiler and runtime and config.
#   Build current compiler with old.
#   Build current compiler with current (twice).
#
# Next step to build everything is e.g. do-cm3-all buildship
#
# This is similar to and reused by boot2,
# but boot2 starts with a clean system except
# for current source tree and boot1[autools] output.
#
# Upgrade is common and has a public interface.
# boot2 is esoteric and interfaces privately
# with boot1[autotools].
#
import sys
from pylib import *

cleangcc = not ("nocleangcc" in sys.argv)
clean = not ("noclean" in sys.argv)

SetupEnvironment()

packages_compiler = " m3bundle m3middle m3quake m3objfile set m3linker m3back m3front sysutils cm3 mklib "
packages_runtime  = " m3core libm3 "

# m3cgen generates .h file for gcc backend (m3cc)
# m3cgcat reads/writes binary/ascii/C m3cg (IR) and is only for very active compiler development
# They could be usually skipped.
packages_compiler += " m3cggen "
packages_compiler += " m3cgcat "

packages = packages_compiler + packages_runtime

# Do a pass, if no pass or the other pass is specified.
pass1 = not "pass2" in sys.argv
pass2 = not "pass1" in sys.argv

if pass1:

    # Delete lingering unshipped cm3cg in case old compiler/config uses it.
    # Production config tends to use and keep using shipped matching cm3cg.
    # Some development config reaches into unshipped.
    for a in ["cm3cg", "gcc/m3cgc1"]:
        for b in ["", ".exe"]:
            DeleteFile(Root + "/m3-sys/m3cc/" + BuildDir + "/" + a + b)

    # Clean everything.
    if clean:
        PyRun("do-pkg realclean " + packages)
    if cleangcc and list(FilterPackages(["m3cc"])):
        PyRun("do-pkg realclean m3cc")

    # Build the compiler with the installed version of compiler and runtime and config.
    # Do not build m3core and libm3.
    # -DM3BOOT2 so HEAD config tries to accomodate old compiler.
    #
    # Build frontend, integrated backend, C++ backend.
    #
    PyRun("do-pkg -DM3BOOT2 buildship " + packages_compiler)

    # Build gcc backend.
    a = Root + "/m3-sys/m3cggen/" + BuildDir + "/m3cggen > " + Root + "/m3-sys/m3cc/gcc/gcc/m3cg/m3cg.h"
    print(a)
    # This makes sense to do, on the rare occasion
    # of IR updates, but has been historically commented out.
    # TODO: Setup workflows that require it.
    # os.system(a)
    list(FilterPackages(["m3cc"])) and PyRun("do-pkg buildship m3cc")

    # cm3 and cm3cg do not always cm3 -ship. Workaround bug.
    ShipCompiler() or sys.exit(1)

# Optionally split pass1 and pass2 for caller to unpatch.

if pass2:
    # Build compiler and runtime with new compiler.
    # No need to rebuild m3cc, since it is written in C++
    # and not affected by which old or new cm3 is in use
    # (assuming m3makefiles interpreted the same).
    #
    # Do it twice.  The first pass is compiled by the compiler shipped above,
    # i.e. the new sources compiled by the *old* compiler.  The compiler folds
    # floating point constants using its own EXTENDED (m3middle/src/TFloat.m3),
    # so if the representation of EXTENDED differs between the old and the new
    # compiler (it became 128 bits on AMD64_LINUX), that compiler folds EXTENDED
    # constants in m3core, libm3 and in the new compiler itself (Target's
    # Extended min/max) with the old representation.  Packages built later by
    # the shipped compiler then fail with "bad version stamps" on Extended.*
    # constants or INTERNAL CG ERRORs (e.g. m3-libs/arithmetic).  The second
    # pass is compiled by the compiler shipped by the first, and is consistent.
    CopyConfigForDistribution(InstallRoot) or sys.exit(1)
    for i in range(2):
        PyRun("do-pkg realclean " + packages)
        PyRun("do-pkg buildship " + packages)
        ShipCompiler() or sys.exit(1)

print("%s: Success." % os.path.basename(sys.argv[0]))
