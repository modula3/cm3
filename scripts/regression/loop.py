#! /usr/bin/env python
# $Id: loop.py,v 1.1 2009-07-27 05:36:53 jkrell Exp $

import os

while True:
   os.system("./tinderbox-build.sh ./cm3.build")
