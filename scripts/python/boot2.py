#! /usr/bin/env python2

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

def IsInterix():
    return os.name == "posix" and os.uname()[0].lower().startswith("interix")

if env_OS == "Windows_NT" and not IsInterix():
    def uname():
        PROCESSOR_ARCHITECTURE = getenv("PROCESSOR_ARCHITECTURE")
        return (env_OS, "", PROCESSOR_ARCHITECTURE, "", PROCESSOR_ARCHITECTURE)
else:
    from os import uname

def RemoveTrailingSpaces(a):
    while len(a) > 0 and a[-1] == ' ':
        a = a[:-1]
    return a

_CBackend = "c" in argv or "C" in argv

def Run(command):
    command = RemoveTrailingSpaces(command + " " + " ".join(argv[1:]))
    print("'" + command + "'")
    os.system(command) and sys.exit("ERROR: " + command)

# ./do-pkg.py doesn't like skipgcc plus just m3cc -- no packages to build
# Which is why this was rewritten in Python from Bourne shell.

c = ""
if _CBackend:
    c = "c"

pyexe = ""

if env_OS == "Windows_NT" and not IsInterix():
    pyexe = "python2.exe "
    if not pylib.SearchPath("python2.exe"):
        pyexe = "py.exe "

Run(pyexe + "./make-dist-cfg.py")

if not _CBackend and env_OS != "Windows_NT":
    Run(pyexe + "./do-pkg.py m3cc buildship " + c)

defines = pylib.PassThroughDefines()
Run(pyexe + "./do-cm3-all.py realclean skipgcc " + c + defines)
Run(pyexe + "./do-pkg.py m3cc m3core libm3 buildship " + c + defines)
Run(pyexe + "./upgrade.py skipgcc " + c + defines)
Run(pyexe + "./do-cm3-all.py realclean skipgcc " + c + defines)
Run(pyexe + "./do-cm3-all.py buildship " + c + defines)
