#! /usr/bin/env python
# $Id: do-cm3-base.py,v 1.13 2008-01-15 12:41:27 jkrell Exp $

import sys
from pylib import *
import os.path

SetupEnvironment()

DoPackage(sys.argv, pylib.PackageSets["base"]) or sys.exit(1)

print("%s: Success." % os.path.basename(sys.argv[0]))
