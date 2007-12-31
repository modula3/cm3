# $Id$

import sys
from libpy import *

# This needs to be data driven via separate files?
# As well, it might be reasonable for the m3makefiles to do the
# tests to build or not. That greatly simplifies here.

P = [
    "m3core",
    "libm3",
    "m3middle",
    "m3quake",
    "m3scanner",
    "m3tools",
    "m3cgcat",
    "m3cggen",
    ]
if (M3GDB):
	P += ["m3gdb"]

P += ["m3bundle"]

if (M3OSTYPE == "WIN32"):
	P += [
        "mklib",
        "dll2lib",
        "fix_nl",
        "libdump"
	]

P += [
    "bitvector",
    "digraph",
    "parseparams",
    "realgeometry",
    "set",
    "slisp",
    "sortedtableextras",
    "table-list",
    "tempfiles",
    ]

if (HAVE_TCL):
    P += ["tcl"]

P += ["tcp"]

if (M3OSTYPE == "WIN32"):
    P += ["tapi"]

if (HAVE_SERIAL):	
    P += ["serial"]

do_pkg(sys.argv, P)
