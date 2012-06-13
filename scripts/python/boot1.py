#! /usr/bin/env python

import sys
import os.path
import pylib
from pylib import *

#
# Having used ".\do-cm3-front.py boot <platform>"
# this script archives up the files needed on the target,
# along with a makefile and script.
#
# It is assumed the target has a full source tree.
# The goal of bootstrapping is to produce a cm3 on the new target.
# From there, the tree can be rebuilt normally from cm3cg, m3core and on up.
#

CopyConfigForDevelopment() or sys.exit(1)
Boot();
