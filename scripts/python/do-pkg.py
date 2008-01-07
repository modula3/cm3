#! /usr/bin/env python
# $Id$

import sys
import pylib

pylib.DoPackage(sys.argv)

print("%s: Success." % os.path.basename(sys.argv[0]))
