#! /usr/bin/env python3
#
# boot2 is meant to bring up a new clean system,
# starting in boot1.py or boot1autotools.py.
# Target must have current source.
#
# The work can be divided arbitrarily with boot1.
# boot1 could have a cross m3cc/cc/ld and produce an entire binary system,
# leaving boot2 to do nothing, or upgrade as a test.
#
# Presently the structure minimizes boot1, maximizes boot2, and
# relies on no cross tools. (Zig cross cc would be great.)
# This has prove historically very useful asis, but could work other ways.
#
from pylib import *
import pylib

PyRun("make-dist-cfg")

if not pylib.CBackend and pylib.env_OS != "Windows_NT":
    PyRun("do-pkg m3cc buildship")

# M3BOOT2 Inform HEAD config to try to be compatible with
# older compiler, which is not really use-case, but OK.
pylib.PyFlags += " -DM3BOOT2"
PyRun("do-cm3-all realclean skipgcc")
PyRun("do-pkg m3cc m3core libm3 buildship")
PyRun("upgrade skipgcc")
PyRun("do-cm3-all realclean skipgcc")
PyRun("do-cm3-all buildship")
