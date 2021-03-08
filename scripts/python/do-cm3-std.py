#! /usr/bin/env python2

import sys
import os.path
import pylib
from pylib import *

SetupEnvironment()

DoPackage(sys.argv, pylib.GetPackageSets()["all"]) or sys.exit(1)

print("%s: Success." % os.path.basename(sys.argv[0]))
