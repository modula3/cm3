#! /usr/bin/env python
# $Id: do-cm3-min.py,v 1.7 2008-01-07 08:58:06 jkrell Exp $

import pylib
import sys

pylib.DoPackage(sys.argv, [
    "m3core",
    "libm3",
    ])

print("%s: Success." % os.path.basename(sys.argv[0]))
