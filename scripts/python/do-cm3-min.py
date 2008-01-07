#! /usr/bin/env python
# $Id: do-cm3-min.py,v 1.8 2008-01-07 09:44:50 jkrell Exp $

import pylib
import sys

pylib.DoPackage(sys.argv, [
    "m3core",
    "libm3",
    ])

print("%s: Success." % os.path.basename(sys.argv[0]))
