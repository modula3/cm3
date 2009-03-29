#! /usr/bin/env python
# $Id: boot1.py,v 1.2 2009-03-29 02:50:32 jkrell Exp $

import sys
import os.path
import pylib
from pylib import *

#
#
# Having used ".\do-cm3-front.py boot <platform>"
# this script archives up the files needed on the target,
# along with a makefile and script.
#
# It is assumed the target has a full source tree.
# The goal of bootstrapping is to produce a cm3 on the new target.
# From there, the tree can be rebuilt normally from cm3cg, m3core and on up.
#
#

Boot();
