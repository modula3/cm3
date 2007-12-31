# $Id: chext.py,v 1.2 2007-12-31 10:11:16 jkrell Exp $

import glob
import os

for a in glob.glob("*.sh"):
	os.rename(a, os.path.splitext(a)[0] + ".py")
