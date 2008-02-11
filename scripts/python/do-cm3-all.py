#! /usr/bin/env python
# $Id: do-cm3-all.py,v 1.1 2008-02-11 09:49:33 jkrell Exp $

from pylib import *
import pylib
import sys
import os.path

SetupEnvironment()

DoPackage(sys.argv, PackageSets["all"]) or sys.exit(1)

print("%s: Success." % os.path.basename(sys.argv[0]))
