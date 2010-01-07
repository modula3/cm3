#! /usr/bin/env python
# $Id: make-msi.py,v 1.3 2009-09-11 15:32:41 jkrell Exp $

import sys
from pylib import *

program = GetLastPathElement(sys.argv[0])

if len(sys.argv) != 2:
    print("usage: %s rootdir" % program)
    sys.exit(1)

MakeMSIWithWix(sys.argv[1])

print("%s: Success, result is %s.*, particularly %s.msi" % (program, sys.argv[1], sys.argv[1]))
sys.exit(0)
