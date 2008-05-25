#! /usr/bin/env python
# $Id: do-cm3-front.py,v 1.8 2008-05-25 01:16:59 jkrell Exp $

import sys
import os.path
from pylib import *

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
