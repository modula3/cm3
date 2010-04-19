import os, sys, re

for root, dirs, files in os.walk("."):
    for f in files:
        if f != "EventRd.m3":
            #continue
            pass
        p = root + "/" + f
        if not p.endswith("3"):
            continue
        print(p)
        orig = open(p, "r").read()
        last_mod = orig.find("\n * Last Modified By: ")
        if last_mod == -1:
            continue
        orig = orig.replace("\n * \n * HISTORY", "\n *\n * HISTORY");
        hist = orig.find("\n *\n * HISTORY")
        if hist == -1:
            continue
        no_hist = orig.find("\n *\n * HISTORY\n *)\n")
        comment = orig.find("\n *)\n")
        new = orig
        if no_hist != -1:
            new = orig[0:last_mod] + orig[comment:]
        else:
            new = orig[0:last_mod] + orig[hist:]
            
        open(p + ".bak", "w").write(orig)
        open(p, "w").write(new)
