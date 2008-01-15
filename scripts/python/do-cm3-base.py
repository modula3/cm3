#! /usr/bin/env python
# $Id$

import sys
from pylib import *
import os.path

SetupEnvironment()

DoPackage(sys.argv, pylib.PackageSets["base"]) or sys.exit(1)

print("%s: Success." % os.path.basename(sys.argv[0]))
