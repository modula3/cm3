#! /usr/bin/env python
# $Id: do-cm3-comm.py,v 1.1 2008-01-07 08:58:06 jkrell Exp $

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
