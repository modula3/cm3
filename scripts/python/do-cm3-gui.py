#! /usr/bin/env python
# $Id: do-cm3-gui.py,v 1.4 2008-01-15 12:41:27 jkrell Exp $

from pylib import *
import sys
import os.path

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
