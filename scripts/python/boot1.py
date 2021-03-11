#! /usr/bin/env python2

import sys
import os.path
import pylib
from pylib import *

#
# usage: ./boot1.py target
#
# This produces an archive that can be copied to the target,
# extract, run make, producing a cm3.
#
# Copy that into place and proceed with boot2.py.
#
# The source trees on the two systems must be "compatible",
# essentially cm3/m3core/m3middle/etc. have to match.
#
# TODO: This should really produce the entire system
# in either assembly or C.
#

CopyConfigForDevelopment() or sys.exit(1)
Boot();
