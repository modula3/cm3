#! /usr/bin/env python
# $Id: do-cm3-min.py,v 1.6 2008-01-03 22:08:52 jkrell Exp $

import pylib
import sys

pylib.do_pkg(sys.argv, [
    "m3core",
    "libm3",
    ])
