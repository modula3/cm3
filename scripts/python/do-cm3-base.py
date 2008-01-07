#! /usr/bin/env python
# $Id: do-cm3-base.py,v 1.9 2008-01-07 12:39:13 jkrell Exp $

import sys
from pylib import *

DoPackage(sys.argv, pylib.PackageSets["base"]) or sys.exit(1)

print("%s: Success." % os.path.basename(sys.argv[0]))
