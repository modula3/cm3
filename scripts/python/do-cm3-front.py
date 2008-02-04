#! /usr/bin/env python
# $Id: do-cm3-front.py,v 1.7 2008-02-04 01:10:40 jkrell Exp $

import sys
from pylib import *
import os.path

SetupEnvironment()

DoPackage(sys.argv, [
    "import-libs",
    "m3core",
    "libm3",
    "sysutils",
    "m3middle",
    "m3objfile",
    "m3linker",
    "m3back",
    "m3staloneback",
    "m3front",
    "m3quake",
    "m3cc",
    "cm3",
    "mklib",
    ])

print("%s: Success." % os.path.basename(sys.argv[0]))
