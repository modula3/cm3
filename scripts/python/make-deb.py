#! /usr/bin/env python
# $Id: make-deb.py,v 1.3.2.1 2009-09-27 00:33:21 jkrell Exp $

import sys
from pylib import *

program = GetLastPathElement(sys.argv[0])

if len(sys.argv) != 2:
    print("usage: %s rootdir" % program)
    sys.exit(1)

MakeDebianPackage(sys.argv[1], "/usr/local/cm3")

print("%s: Success, result is %s.*, particularly %s.deb" % (program, sys.argv[1], sys.argv[1]))
sys.exit(0)
