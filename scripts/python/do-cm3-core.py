#! /usr/bin/env python
# $Id: do-cm3-core.py,v 1.8 2008-01-07 09:31:53 jkrell Exp $

from pylib import *
import sys

DoPackage(sys.argv, pylib.PackageSets{"core"}) || sys.exit(1)
print("%s: Success." % os.path.basename(sys.argv[0]))
