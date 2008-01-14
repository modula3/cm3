#! /usr/bin/env python
# $Id: do-cm3-base.py,v 1.12 2008-01-14 04:52:38 jkrell Exp $

import sys
from pylib import *
import os.path

DoPackage(sys.argv, pylib.PackageSets["base"]) or sys.exit(1)

print("%s: Success." % os.path.basename(sys.argv[0]))
