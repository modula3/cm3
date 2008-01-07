#! /usr/bin/env python
# $Id: do-cm3-caltech-parser.py,v 1.1 2008-01-07 08:58:06 jkrell Exp $

from pylib import *
import sys

DoPackage(sys.argv, [
    "cit_common",
    "m3tmplhack",
    "cit_util",
    "term",
    "drawcontext",
    "hack",
    "m3browserhack",
    "paneman",
    "paneman/kemacs",
    "parserlib/ktoklib",
    "parserlib/klexlib",
    "parserlib/ktok",
    "parserlib/klex",
    "parserlib/kyacc",
    "parserlib/kext",
    "parserlib/parserlib",
    "parserlib/parserlib/test",
    ])

print("%s: Success." % os.path.basename(sys.argv[0]))
