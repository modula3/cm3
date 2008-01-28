#! /usr/bin/env python
# $Id$

import sys
import pylib
from pylib import *

ShipCompiler() or sys.exit(1)
CopyConfigForDevelopment() or sys.exit(1)
