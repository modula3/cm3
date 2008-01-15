#! /usr/bin/env python
# $Id$

import os.path
import sys
import pylib

SetupEnvironment()
DoPackage(sys.argv)

print("%s: Success." % os.path.basename(sys.argv[0]))
