#! /usr/bin/env python
# $Id: install-cm3-compiler.py,v 1.3 2008-05-25 01:16:59 jkrell Exp $

import sys
from pylib import *

ShipFront() or sys.exit(1)
CopyConfigForDevelopment() or sys.exit(1)
