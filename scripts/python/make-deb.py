#! /usr/bin/env python2

import sys
from pylib import *

program = GetLastPathElement(sys.argv[0])

if len(sys.argv) != 2:
    print("usage: %s rootdir" % program)
    sys.exit(1)

MakeDebianPackage(sys.argv[1], "/usr/local/cm3")

print("%s: Success, result is %s.*, particularly %s.deb" % (program, sys.argv[1], sys.argv[1]))
sys.exit(0)
