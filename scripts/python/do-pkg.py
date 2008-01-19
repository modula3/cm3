#! /usr/bin/env python
# $Id$

import os.path
import sys
import pylib
from pylib import *

SetupEnvironment()
DoPackage(sys.argv)

print("%s: Success." % os.path.basename(sys.argv[0]))
