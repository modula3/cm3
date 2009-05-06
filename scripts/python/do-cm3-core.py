#! /usr/bin/env python
# $Id$

import sys
import os.path
import pylib
from pylib import *

SetupEnvironment()

DoPackage(sys.argv, pylib.PackageSets["core"]) or sys.exit(1)

print("%s: Success." % os.path.basename(sys.argv[0]))
