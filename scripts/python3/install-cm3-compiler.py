#! /usr/bin/env python

import sys
from pylib import *

ShipFront() or sys.exit(1)
CopyConfigForDistribution(InstallRoot) or sys.exit(1)
