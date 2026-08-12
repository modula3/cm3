#! /usr/bin/env python3

import sys
from pylib import *

ShipFront() or sys.exit(1)
CopyConfigForDistribution(InstallRoot) or sys.exit(1)
