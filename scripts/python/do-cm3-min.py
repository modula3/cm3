#! /usr/bin/env python
# $Id: do-cm3-min.py,v 1.9 2008-01-07 12:39:13 jkrell Exp $

from pylib import *
import pylib
import sys
import os.path

DoPackage(sys.argv, pylib.PackageSets["min"]) or sys.exit(1)

print("%s: Success." % os.path.basename(sys.argv[0]))
