#! /usr/bin/env python
# $Id: do-cm3-comm.py,v 1.4 2008-01-14 04:51:18 jkrell Exp $

from pylib import *
import sys
import os.path

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
