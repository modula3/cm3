#! /usr/bin/env python
# $Id: install-cm3-compiler.py,v 1.2 2008-04-21 16:42:45 jkrell Exp $

import sys
import pylib
from pylib import *

ShipFront() or sys.exit(1)
CopyConfigForDevelopment() or sys.exit(1)
