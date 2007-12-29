from sysinfo import ROOT
from pkginfo import *
import os
import sys
from os import getcwd, chdir
from os.path import isdir

# lame temporary
PKGS = [ ]
PKG_ACTION = ""
LIST_ONLY = False
NO_ACTION = False
KEEP_GOING = False

def exec_cmd(PKG):
    # lame temporary
    global PKG_ACTION, NO_ACTION
    print(" +++ %s +++" % PKG_ACTION)
    if (NO_ACTION):
        return 0
    a = "cd %s && %s" % (PKG, PKG_ACTION)
    if (os.name != "nt"):
        a = "/bin/sh -c \"" + a + "\""    
    #print(a)
    return os.system(a)
    

def pkgmap(args):
    # Which of these should be primed from the environment?
    global PKGS, PKG_ACTION, LIST_ONLY, NO_ACTION, KEEP_GOING
    PKGS = [ ]
    PKG_ACTION = ""
    LIST_ONLY = False
    NO_ACTION = False
    KEEP_GOING = False
    i = 0
    j = len(args)
    while (i != j):
        arg = args[i]
        while (True):
            if (arg == ""):
                break
            if (arg == "-k"):
                KEEP_GOING = True
                break
            if (arg == "-n"):
                NO_ACTION = True
                break
            if (arg == "-l"):
                LIST_ONLY = True
                break
            if (arg == "-c"):
                i += 1
                if (i == j):
                    sys.stderr.write("missing parameter to -c\n");
                    sys.exit(1)            
                if (PKG_ACTION):
                    PKG_ACTION += " ; "
                PKG_ACTION += args[i]
                break
            p = os.path.join(ROOT, arg)
            if (isdir(p)):
                #print("1 %(p)s" % vars())
                PKGS.append(p)
                break
            if (isdir(arg)):
                #print("2 %(arg)s" % vars())
                PKGS.append(arg)
                break
            p = pkgpath(arg)
            if (not p):
                sys.stderr.write(" *** cannot find package %(arg)s\n" % vars())
                sys.exit(1)
            if (isdir(p)):
                #print("3 %(p)s" % vars())
                PKGS.append(p)
                break
            p = os.path.join(ROOT, p)
            if (isdir(p)):
                #print("4 %(p)s" % vars())
                PKGS.append(p)
                break
            sys.stderr.write(" *** cannot find package %(arg)s / %(p)s\n" % vars())
            sys.exit(1)
        i += 1                    
    if (not PKG_ACTION):
        sys.stderr.write("no PKG_ACTION defined, aborting\n")
        sys.exit(1)
    if (not PKGS):
        sys.stderr.write("no packages\n")
        sys.exit(1)
    if (LIST_ONLY):
        listpkgs(PKGS)
        sys.exit(0)
    
    for PKG in PKGS:
        print("== package %(PKG)s ==" % vars())
        res = exec_cmd(PKG)
        if (res != 0):
            if (not KEEP_GOING):
                print(" *** execution of %(ACTION)s failed ***")
                sys.exit(1)
        if (KEEP_GOING):
            print(" ==> %s returned %s" % (PKG_ACTION, res))
        else:
            print(" ==> %(PKG)s done" % vars())


if __name__ == "__main__":
    #
    # run test code if module run directly
    #
    pkgmap(["-c"])
