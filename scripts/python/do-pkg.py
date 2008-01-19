#! /usr/bin/env python
# $Id: do-pkg.py,v 1.14 2008-01-19 00:01:51 jkrell Exp $

import os.path
import sys
import pylib
from pylib import *

SetupEnvironment()
DoPackage(sys.argv)

print("%s: Success." % os.path.basename(sys.argv[0]))
