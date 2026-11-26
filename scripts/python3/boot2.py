#! /usr/bin/env python3
#
# Run boot2.py on target for new cross builds, starting in boot1.py or boot1autotools.py.
# Target must have current source.
#
# This is almost the same as upgrade, but m3core/libm3 are presumed missing.
#
from pylib import *
PyRun("make-dist-cfg")
PyRun("do-cm3-all realclean")
PyRun("do-pkg m3core libm3 buildship")
PyRun("upgrade")
PyRun("do-cm3-all buildship")
