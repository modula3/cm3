#! /usr/bin/env python
# $Id: install-config.py,v 1.2 2008-04-21 16:21:07 jkrell Exp $

import sys
import pylib
from pylib import *

CopyConfigForDevelopment() or sys.exit(1)
