#! /usr/bin/env python
# $Id$

import sys
from pylib import *

DoPackage(sys.argv, [
    "m3core",
    "libm3",
    "m3middle",
    "m3objfile",
    "m3linker",
    "m3back",
    "m3staloneback",
    "m3front",
    "m3quake",
    "m3cc",
    "cm3",
# "mklib"
    ])

print("%s: Success." % os.path.basename(sys.argv[0]))
