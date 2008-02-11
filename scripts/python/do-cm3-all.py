#! /usr/bin/env python
# $Id$

from pylib import *
import pylib
import sys
import os.path

SetupEnvironment()

DoPackage(sys.argv, PackageSets["all"]) or sys.exit(1)

print("%s: Success." % os.path.basename(sys.argv[0]))
