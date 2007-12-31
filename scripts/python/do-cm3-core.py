# $Id$

from lib import *
import sys

# This needs to be data driven via separate files?
# As well, it might be reasonable for the m3makefiles to do the
# tests to build or not. That greatly simplifies here.

P = [
    "m3core",
    "libm3",
    "patternmatching",
    "m3middle",
    ]
    
if (M3OSTYPE == "WIN32"):
    P += ["m3objfile"]

P += ["m3linker"]

if (not GCC_BACKEND):
    P += [
        "m3back",
        "m3staloneback",
        ]

P += [
    "m3front",
    "m3quake",
    ]

if (GCC_BACKEND and not OMIT_GCC):
    P += ["m3cc"]

P += [
    "cm3",
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
        "libdump",
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

do_pkg(sys.argv, P)
