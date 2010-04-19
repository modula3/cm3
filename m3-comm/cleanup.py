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
        new = re.sub("\n \\*\n \\*\n \\* \\$" + "Log", "$" + "$Log", new);
        new = re.sub("\n \\* \\$" + "Source: [^$]+\\$\n", "\n", new);
        new = re.sub("\n \\* \\$" + "Date: [^$]+\\$\n", "\n", new);
        new = re.sub("\n \\* \\$" + "Author: [^$]+\\$\n", "\n", new);
        new = re.sub("\n \\* \\$" + "Revision: [^$]+\\$\n", "\n", new);
        new = re.sub("\n \\* \\$" + "Log: [^$]+\\$\n", "\n", new);
        new = re.sub("(\n \\* added: [^\n]+)+\n", "\n", new);
        new = re.sub("(\n \\* modified: [^\n]+)+\n", "\n", new);
        new = re.sub("compile\n \\*\n \\*\n", "compile\n *\n", new);
        if new == orig:
            continue            
        open(p + ".bak", "w").write(orig)
        open(p, "w").write(new)
