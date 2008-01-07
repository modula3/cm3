#! /usr/bin/env python
# $Id: do-cm3-gui.py,v 1.1 2008-01-07 08:58:06 jkrell Exp $

from pylib import *
import sys

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
