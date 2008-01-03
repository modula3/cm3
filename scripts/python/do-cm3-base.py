#! /usr/bin/env python
# $Id: do-cm3-base.py,v 1.6 2008-01-03 22:08:52 jkrell Exp $

import sys
from pylib import *

# This needs to be data driven via separate files?
# As well, it might be reasonable for the m3makefiles to do the
# tests to build or not. That greatly simplifies here.

do_pkg(sys.argv, [
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
