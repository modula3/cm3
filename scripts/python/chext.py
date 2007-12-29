#! /usr/bin/env python

import glob
import os

for a in glob.glob("*.sh"):
	os.rename(a, os.path.splitext(a)[0] + ".py")
