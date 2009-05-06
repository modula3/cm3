#! /usr/bin/env python
# $Id: do-cm3-core.py,v 1.13 2009-05-06 10:58:19 jkrell Exp $

import sys
import os.path
import pylib
from pylib import *

SetupEnvironment()

DoPackage(sys.argv, pylib.PackageSets["core"]) or sys.exit(1)

print("%s: Success." % os.path.basename(sys.argv[0]))
