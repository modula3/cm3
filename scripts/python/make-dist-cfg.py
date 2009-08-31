#! /usr/bin/env python

import sys
import os
import os.path

def find_exe(file, path = None):
    if os.path.isdir(file):
        return file + "/cm3"
    if os.path.isfile(file):
        return file
    if path == None:
       path = os.getenv("PATH")
    for dir in path.split(os.path.pathsep):
        cand = dir + "/" + file
        for c in [cand + ".exe", cand]:
            if os.path.isfile(c):
                return c


cm3 = "cm3"
if len(sys.argv) >= 2:
  cm3 = sys.argv[1]

cm3 = find_exe(cm3)
cm3cfg = os.path.dirname(cm3) + "/cm3.cfg"
open(cm3cfg, "w")

for a in os.popen(cm3 + " -version").readlines():
    b = a.find(" host: ")
    if b != -1:
        host = a[b + 7:-1]
        a = open(os.path.dirname(cm3) + "/cm3.cfg", "w")
        a.write("INSTALL_ROOT = path() & \"/..\"\n")
        a.write("include(path() & \"/config/" + host + "\")\n")
