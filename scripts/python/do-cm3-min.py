#! /usr/bin/env python
# $Id: do-cm3-min.py,v 1.10 2008-01-15 12:41:27 jkrell Exp $

from pylib import *
import pylib
import sys
import os.path

SetupEnvironment()

DoPackage(sys.argv, pylib.PackageSets["min"]) or sys.exit(1)

print("%s: Success." % os.path.basename(sys.argv[0]))
