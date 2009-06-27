#! /usr/bin/env python
# $Id$

import sys
import os.path
import pylib
from pylib import *

#
# first run make-dist.py and then point make-deb.py at the results
#

SetEnvironmentVariable("STAGE", sys.argv[1])

for name in ["min",
           # "std"
            ]:
    MakeDebianPackage(name, FormInstallRoot(name), GetStage() + "/cm3-" + name + ".deb", "/usr/local/cm3")

print("%s: Success." % os.path.basename(sys.argv[0]))
