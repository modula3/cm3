#! /usr/bin/env python3
#
# From a working compiler and runtime, build current compiler
# with old compiler, then compiler and runtime with current compiler.
# This uses HEAD config with old compiler but that is dubious.
# This stops at the compiler.
# Next step to build everything is e.g. do-cm3-all buildship
#
# This is same as boot2, but boot2 builds runtime first, then calls this.
#
import sys
from pylib import *

#print("cd " + orig_cwd)
#os.chdir(orig_cwd)
packages_compiler = " m3bundle m3middle m3quake m3objfile set m3linker m3back m3front sysutils cm3 mklib "
packages_runtime  = " m3core libm3 "

# m3cgen generates .h file for gcc backend (m3cc)
# m3cgcat reads/writes binary/ascii/C m3cg (IR) and is only for very active compiler development
# They could be usually skipped.
packages_compiler += " m3cggen "
packages_compiler += " m3cgcat "

if not "pass2" in sys.argv:
    PyRun("do-pkg realclean " + packages_compiler + packages_runtime)

    # TODO: Use old config.
    # install current config for old compiler
    PyRun("make-dist-cfg")

    # Build the compiler with the installed version of compiler and runtime;
    #  do not build m3core and libm3.
    #
    # -DM3BOOT2 so HEAD config tries to accomodate old compiler.
    #
    PyRun("do-pkg -DM3BOOT2 buildship " + packages_compiler)
    ShipCompiler() or sys.exit(1)

if not "pass1" in sys.argv:
    # Build compiler and runtime with new compiler.
    PyRun("do-pkg realclean " + packages_compiler + packages_runtime)
    PyRun("do-pkg buildship " + packages_compiler + packages_runtime)
    ShipCompiler() or sys.exit(1)

print("%s: Success." % os.path.basename(sys.argv[0]))
