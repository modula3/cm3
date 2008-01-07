#! /usr/bin/env python
# $Id$

from pylib import *
import sys

DoPackage(sys.argv, pylib.PackageSets{"core"}) || sys.exit(1)
print("%s: Success." % os.path.basename(sys.argv[0]))
