#! /usr/bin/env python
# $Id: do-cm3-comm.py,v 1.7 2008-05-25 01:16:59 jkrell Exp $

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
