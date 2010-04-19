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
        new = orig
        new = re.sub("\n \\*\n \\*\n \\* \\$Log", "$Log", new);
        new = re.sub("\n \\* \\$Source: /opt/cvs/cm3/m3-comm/cleanup.py,v $]+\\$\n", "\n", new);
        new = re.sub("\n \\* \\$Date: 2010-04-19 16:20:25 $]+\\$\n", "\n", new);
        new = re.sub("\n \\* \\$Author: jkrell $]+\\$\n", "\n", new);
        new = re.sub("\n \\* \\$Revision: 1.2 $]+\\$\n", "\n", new);
        new = re.sub("\n \\* \\$Log: not supported by cvs2svn $]+\\$\n", "\n", new);
        new = re.sub("(\n \\* added: [^\n]+)+\n", "\n", new);
        new = re.sub("(\n \\* modified: [^\n]+)+\n", "\n", new);
        new = re.sub("compile\n \\*\n \\*\n", "compile\n *\n", new);
        if new == orig:
            continue            
        open(p + ".bak", "w").write(orig)
        open(p, "w").write(new)
