#! /usr/bin/env python
# $Id: do-cm3-std.py,v 1.3 2008-01-07 12:39:13 jkrell Exp $

from pylib import *
import pylib
import sys
import os.path

if not SearchPath("m3bundle"):
    DoPackage(["", "buildship", "m3bundle"])

DoPackage(sys.argv, pylib.PackageSets["std"]) or sys.exit(1)

print("%s: Success." % os.path.basename(sys.argv[0]))
