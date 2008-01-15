#! /usr/bin/env python
# $Id: do-cm3-std.py,v 1.5 2008-01-15 12:41:27 jkrell Exp $

from pylib import *
import pylib
import sys
import os.path

SetupEnvironment()

if not SearchPath("m3bundle"):
    DoPackage(sys.argv, ["m3bundle"])

DoPackage(sys.argv, pylib.PackageSets["std"]) or sys.exit(1)

print("%s: Success." % os.path.basename(sys.argv[0]))
