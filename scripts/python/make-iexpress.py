#! /usr/bin/env python
# $Id: make-iexpress.py,v 1.3 2009-06-27 20:27:26 jkrell Exp $

import sys
import os.path
import pylib
from pylib import *

#
# first run make-dist.py and then point make-iexpress.py at the results
#

SetEnvironmentVariable("STAGE", sys.argv[1])

for name in ["min", 
    #"std"
    ]:
    MakeIExpressPackage(FormInstallRoot(name), GetStage() + "/cm3-" + name + ".exe")

print("%s: Success." % os.path.basename(sys.argv[0]))
