#! /usr/bin/env python
# $Id: do-cm3-min.py,v 1.11 2008-05-25 01:16:59 jkrell Exp $

import sys
import os.path
import pylib
from pylib import *

SetupEnvironment()

DoPackage(sys.argv, pylib.PackageSets["min"]) or sys.exit(1)

print("%s: Success." % os.path.basename(sys.argv[0]))
