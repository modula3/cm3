#! /usr/bin/env python
# $Id: do-pkg.py,v 1.11 2008-01-07 09:44:50 jkrell Exp $

import sys
import pylib

pylib.DoPackage(sys.argv)

print("%s: Success." % os.path.basename(sys.argv[0]))
