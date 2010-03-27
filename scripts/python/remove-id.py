#
# remove $Id$, at least in cvsup/*3 files
#

import os
import os.path
import sys
import re

for root, dirs, files in os.walk("."):
    for f in files:
        if f.endswith(".i3") or f.endswith(".m3"):
            p = os.path.join(root, f)
            a = open(p, "r").read()
            if a.find("$Id") != -1:
                print(p)
                b = re.sub("\n \\*\n \\* \\$Id$]+\\$ \\*\\)", "\n *)", a)
                if a != b:
                    open(p, "w").write(b)
                    print(p)
