#! /usr/bin/env python
# $Id$

import pylib
import sys

pylib.DoPackage(sys.argv, [
    "m3core",
    "libm3",
    ])

print("%s: Success." % os.path.basename(sys.argv[0]))
