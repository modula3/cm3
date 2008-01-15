#! /usr/bin/env python
# $Id: do-pkg.py,v 1.13 2008-01-15 12:41:27 jkrell Exp $

import os.path
import sys
import pylib

SetupEnvironment()
DoPackage(sys.argv)

print("%s: Success." % os.path.basename(sys.argv[0]))
