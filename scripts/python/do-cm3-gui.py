#! /usr/bin/env python
# $Id: do-cm3-gui.py,v 1.3 2008-01-14 03:30:38 jkrell Exp $

from pylib import *
import sys
import os.path

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
