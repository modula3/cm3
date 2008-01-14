#! /usr/bin/env python
# $Id$

from pylib import *
import pylib
import sys
import os.path

if not SearchPath("m3bundle"):
    DoPackage(sys.argv, ["m3bundle"])

DoPackage(sys.argv, pylib.PackageSets["std"]) or sys.exit(1)

print("%s: Success." % os.path.basename(sys.argv[0]))
