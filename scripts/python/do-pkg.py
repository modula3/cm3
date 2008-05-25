#! /usr/bin/env python
# $Id: do-pkg.py,v 1.16 2008-05-25 01:16:59 jkrell Exp $

import sys
import os.path
from pylib import *

SetupEnvironment()
DoPackage(sys.argv)

print("%s: Success." % os.path.basename(sys.argv[0]))
