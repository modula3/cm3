#! /usr/bin/env python
# $Id: make-msi.py,v 1.2 2009-09-11 15:10:43 jkrell Exp $

import sys
from pylib import *

if len(sys.argv) != 2:
    print("usage: %s rootdir" % GetLastPathElement(sys.argv[0]))
    sys.exit(1)

MakeMSIWithWix(sys.argv[1])

print("%s: Success, result is %s.*, particularly %s.msi" % (GetLastPathElementsys.argv[0], sys.argv[1], sys.argv[1]))
sys.exit(0)
