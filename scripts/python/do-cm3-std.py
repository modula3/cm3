#! /usr/bin/env python
# $Id$

import sys
import os.path
import pylib
from pylib import *

SetupEnvironment()

#if not SearchPath("m3bundle"):
#    DoPackage(sys.argv, ["m3bundle"])

DoPackage(sys.argv, pylib.PackageSets["std"]) or sys.exit(1)

print("%s: Success." % os.path.basename(sys.argv[0]))
