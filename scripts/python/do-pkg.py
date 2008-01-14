#! /usr/bin/env python
# $Id: do-pkg.py,v 1.12 2008-01-14 03:30:38 jkrell Exp $

import os.path
import sys
import pylib

pylib.DoPackage(sys.argv)

print("%s: Success." % os.path.basename(sys.argv[0]))
