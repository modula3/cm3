#! /usr/bin/env python
# $Id: install-cm3-compiler.py,v 1.1 2008-01-28 16:03:41 jkrell Exp $

import sys
import pylib
from pylib import *

ShipCompiler() or sys.exit(1)
CopyConfigForDevelopment() or sys.exit(1)
