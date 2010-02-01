#! /usr/bin/env python

import sys
import os.path
from pylib import *

SetupEnvironment()

DoPackage(sys.argv, [
    "tcp",
    "tapi",
    "serial",
    "netobj",
    "netobjd",
    "m3tk-misc",
    "m3tk",
    "stubgen",
    ])

print("%s: Success." % os.path.basename(sys.argv[0]))
