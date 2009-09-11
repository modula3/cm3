#! /usr/bin/env python
# $Id$

import sys
import os.path
from pylib import *

SetupEnvironment()

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
    "parserlib/kyacclib",
    "parserlib/ktok",
    "parserlib/klex",
    "parserlib/kyacc",
    "parserlib/kext",
    "parserlib/parserlib",
    #"parserlib/parserlib/test",
    ])

print("%s: Success." % os.path.basename(sys.argv[0]))
