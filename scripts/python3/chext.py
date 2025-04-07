#! /usr/bin/env python

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
