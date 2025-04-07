#! /usr/bin/env python

import sys
import os.path
import pylib
from pylib import *

SetupEnvironment()

DoPackage(sys.argv, pylib.GetPackageSets()["min"]) or sys.exit(1)

print("%s: Success." % os.path.basename(sys.argv[0]))
