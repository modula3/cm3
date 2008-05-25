#! /usr/bin/env python
# $Id$

import sys
import os.path
import pylib
from pylib import *

#
#
# Having used python\do-cm3-front boot platform
# this script archives up the files needed on the target.
#
# It is assumed the target has a full source tree.
# It is assumed building a native cm3cg is easy enough.
# The trick/goal of bootstrapping is to produce a cm3 on the new target.
# From there, the tree can be rebuilt normally from m3core and on up.
# (We don't care about "shipping" during boot.)
#
#

Boot();
