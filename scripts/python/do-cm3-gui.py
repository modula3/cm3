#! /usr/bin/env python

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
