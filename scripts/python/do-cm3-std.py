#! /usr/bin/env python
# $Id: do-cm3-std.py,v 1.6 2008-05-25 01:16:59 jkrell Exp $

import sys
import os.path
import pylib
from pylib import *

SetupEnvironment()

if not SearchPath("m3bundle"):
    DoPackage(sys.argv, ["m3bundle"])

DoPackage(sys.argv, pylib.PackageSets["std"]) or sys.exit(1)

print("%s: Success." % os.path.basename(sys.argv[0]))
