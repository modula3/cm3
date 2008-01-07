#! /usr/bin/env python
# $Id: do-cm3-core.py,v 1.7 2008-01-07 08:58:06 jkrell Exp $

from pylib import *
import sys

# This needs to be data driven via separate files?
# As well, it might be reasonable for the m3makefiles to do the
# tests to build or not. That greatly simplifies here.

DoPackage(sys.argv, [
    "m3core",
    "libm3",
    "patternmatching",
    "m3middle",
    "m3objfile",
    "m3linker",
    "m3back",
    "m3staloneback",
    "m3front",
    "m3quake",
    "m3cc",
    "cm3",
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
    ])

print("%s: Success." % os.path.basename(sys.argv[0]))
