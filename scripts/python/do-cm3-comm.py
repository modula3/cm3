#! /usr/bin/env python
# $Id: do-cm3-comm.py,v 1.2 2008-01-07 09:44:50 jkrell Exp $

from pylib import *
import sys

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
