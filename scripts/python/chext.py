#! /usr/bin/env python
# $Id: chext.py,v 1.6 2008-01-03 22:08:52 jkrell Exp $

import glob
import os

for a in glob.glob("*.sh"):
    base = os.path.splitext(a)[0]
    dest = (base + ".py")
    if os.path.isfile(dest):
        print("skipping " + dest + " because it already exists")
    else:
        print(a + " => " + dest)
        os.rename(a, dest)
