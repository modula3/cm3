#! /usr/bin/env python
# $Id: do-cm3-base.py,v 1.8 2008-01-07 09:44:50 jkrell Exp $

import sys
from pylib import *

# This needs to be data driven via separate files?
# As well, it might be reasonable for the m3makefiles to do the
# tests to build or not. That greatly simplifies here.

DoPackage(sys.argv, [
    "m3core",
    "libm3",
    "m3middle",
    "m3quake",
    "m3scanner",
    "m3tools",
    "m3cgcat",
    "m3cggen",
	"m3gdb",
    "m3bundle",
    "mklib",
    "dll2lib",
    "fix_nl",
    "libdump",
    "bitvector",
    "digraph",
    "parseparams",
    "realgeometry",
    "set",
    "slisp",
    "sortedtableextras",
    "table-list",
    "tempfiles",
    "tcl",
    "tcp",
    "tapi",
    "serial",
    ])

print("%s: Success." % os.path.basename(sys.argv[0]))
