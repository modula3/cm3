#! /usr/bin/env python
# $Id: do-cm3-gui.py,v 1.5 2008-05-25 01:16:59 jkrell Exp $

import sys
import os.path
from pylib import *

SetupEnvironment()

DoPackage(sys.argv, [
    "X11R4",
    "ui",
    "vbtkit",
    "tcp",
    "cmvbt",
    "jvideo",
    "videovbt",
    "web",
    "formsvbtpixmaps",
    "formsvbt",
    "formsview",
    "formsedit",
    ])

print("%s: Success." % os.path.basename(sys.argv[0]))
