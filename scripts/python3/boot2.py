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

# Roughly:
#!/bin/sh
#
#set -e
#set -x
#
#./make-dist-cfg.py $*
#./do-pkg.py m3cc buildship $*
#./do-cm3-all.py realclean skipgcc $*
#./do-pkg.py m3cc m3core libm3 buildship $*
#./upgrade.py skipgcc $*
#./do-cm3-all.py realclean skipgcc $*
#./do-cm3-all.py buildship $*

import os, sys, pylib
from os import getenv
argv = sys.argv

env_OS = getenv("OS")

def Posix():
    return os.name == "posix"

if Posix():
    from os import uname
elif env_OS == "Windows_NT":
    DevNull = "nul:"
    def uname():
        PROCESSOR_ARCHITECTURE = getenv("PROCESSOR_ARCHITECTURE")
        return (env_OS, "", PROCESSOR_ARCHITECTURE, "", PROCESSOR_ARCHITECTURE)
else:
    print("fatal error: unknown host")
    sys.exit(1)

def RemoveTrailingSpaces(a):
    while len(a) > 0 and a[-1] == ' ':
        a = a[:-1]
    return a

_CBackend = "c" in argv or "C" in argv
c = ""
if _CBackend:
    c = "c"

def Run(command):
    command = RemoveTrailingSpaces(command + " " + " ".join(argv[1:]))
    print("'" + command + "'")
    os.system(command) and sys.exit("ERROR: " + command)

# ./do-pkg.py doesn't like skipgcc plus just m3cc -- no packages to build
# Which is why this was rewritten in Python from Bourne shell.

pyexe = ""

if not Posix() and env_OS == "Windows_NT":
    pyexe = (pylib.SearchPath("python.exe") or pylib.SearchPath("python3.exe") or pylib.SearchPath("py.exe") or pylib.SearchPath("python2.exe")) + " "

Run(pyexe + "./make-dist-cfg.py")

if not _CBackend and env_OS != "Windows_NT":
    Run(pyexe + "./do-pkg.py m3cc buildship " + c)

# M3BOOT2 Inform HEAD config to try to be compatible with older compiler.
defines = pylib.PassThroughDefines() + " -DM3BOOT2"

# clean all
Run(pyexe + "./do-cm3-all.py realclean skipgcc " + c + defines)

# build current m3core and libm3 (mistake?)
Run(pyexe + "./do-pkg.py m3cc m3core libm3 buildship " + c + defines)

# build current compiler with old compiler and new compiler
Run(pyexe + "./upgrade.py skipgcc " + c + defines)

# Remove M3BOOT2 parameter.
defines = pylib.PassThroughDefines()

# Clean and build eveything.
Run(pyexe + "./do-cm3-all.py realclean skipgcc " + c + defines)
Run(pyexe + "./do-cm3-all.py buildship " + c + defines)
