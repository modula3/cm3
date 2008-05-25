#! /usr/bin/env python
# $Id: do-cm3-core.py,v 1.12 2008-05-25 01:16:59 jkrell Exp $

import sys
import os.path
from pylib import *

SetupEnvironment()

DoPackage(sys.argv, pylib.PackageSets["core"]) or sys.exit(1)

print("%s: Success." % os.path.basename(sys.argv[0]))
