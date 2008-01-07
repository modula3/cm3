#! /usr/bin/env python
# $Id: do-cm3-std.py,v 1.2 2008-01-07 09:44:50 jkrell Exp $

from pylib import *
import sys

if not SearchPath("m3bundle"):
    DoPackage(["", "buildship", "m3bundle"])

DoPackage(sys.argv, [

# base libraries

    "m3core",
    "libm3",
    "patternmatching",

# system / compiler libraries and tools

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

# more useful quasi-standard libraries

    "arithmetic",
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
    "udp",
    "libsio",
    "libbuf",
    "debug",
    "listfuncs",
    "embutils",
    "m3tk-misc",
    "http",
    "binIO",
    # "deepcopy",
    # "sgml",
    "commandrw",

    # some CM3 communication extensions

    "tapi",
    "serial",

    # tools

    "m3tk",
    "mtex",
    "m3totex",
    "m3tohtml",
    "m3scan",
    "m3markup",
    "m3browser",
    "cmpdir",
    "cmpfp",
    "dirfp",
    "uniq",
    # "pp" # needs lex and yacc or flex and bison
    # "kate"   # can be shipped only on systems with KDE
    # "nedit",

    # network objects -- distributed programming

    "netobj",
    "netobjd",
    "stubgen",
    "events",
    "rdwr",
    "sharedobj",
    "sharedobjgen",

    # database packages

    "odbc",
    "postgres95",
    "db",
    "smalldb",
    "stable",
    "stablegen",

    # the standard graphical user interface: trestle and formsvbt

    "X11R4",
    "ui",
    "PEX",
    "vbtkit",
    "cmvbt",
    "jvideo",
    "videovbt",
    "web",
    "formsvbtpixmaps",
    "formsvbt",
    "formsview",
    "formsedit",
    "codeview",
    "mg",
    "mgkit",
    "opengl",
    "anim3D",
    "zeus",
    "m3zume",

    # obliq
    "synloc",
    "synex",
    "metasyn",
    "obliqrt",
    "obliqparse",
    "obliqprint",
    "obliq",
    "obliqlibemb",
    "obliqlibm3",
    "obliqlibui",
    "obliqlibanim",
    # "obliqlib3D" # does not compile
    "obliqsrvstd",
    "obliqsrvui",
    "obliqbinmin",
    "obliqbinstd",
    "obliqbinui",
    "obliqbinanim",
    "visualobliq",
    "vocgi",
    "voquery",
    "vorun",

    # more graphics depending on obliq

    "webvbt",

    # more tools

    "recordheap",
    "rehearsecode",
    "replayheap",
    "showheap",
    "shownew",
    "showthread",
    # showthread needs ThreadEvent, which does not exist on win32

    # The Juno-2 graphical constraint based editor

    "pkl-fonts",
    "juno-machine",
    "juno-compiler",
    "juno-app",

    # demo programs

    "cube",
    "calculator",
    "fisheye",
    "mentor",
    ])

print("%s: Success." % os.path.basename(sys.argv[0]))
