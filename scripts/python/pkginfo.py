import sys
import os
from os.path import isfile, join, dirname
from sysinfo import PKGSDB, ROOT

#
# our exports
#
__all__ = [
    "pkg_defined",
    "pkgpath",
    "listpkgs",
    ]

def MakePackageDB():
    if (not isfile(PKGSDB)):
        #import 'find-packages' # import requires an identifier
        execfile(join(dirname(__file__), "find-packages.py"))
    
        if (not isfile(PKGSDB)):
            sys.stderr.write("cannot generate package list\n")
            sys.exit(1)

PackageDB = None

def ReadPackageDB():
    MakePackageDB();
    global PackageDB;
    PackageDB = (PackageDB or
            map(
                lambda(a): a.replace("\n", ""),
                open(PKGSDB).readlines(),
                ))

def pkg_defined(a):
    ReadPackageDB()
    a = ("/" + a)
    for i in PackageDB:
        if i.endswith(a):
            return True

def pkgpath(a):
    ReadPackageDB()
    b = ("/" + a)
    for i in PackageDB:
        if (i.endswith(b)):
            #print("pkgpath(%(a)s returning %(i)s (%(b)s)" % vars())
            return i
    sys.stderr.write("package " + a + " not found\n")

def listpkgs(pkgs):
    ReadPackageDB()
    Result = [ ]
    if pkgs:
        for pkg in pkgs.split(" "):
            # remove ROOT from the start
            if (pkg.startswith(ROOT + "/")):
                pkg = pkg[len(ROOT) + 1:]
                #print("1 " + pkg)
            # if no slashes, then need a leading slash
            if (pkg.find("/") == -1):
                pkg = ("/" + pkg)
                #print("2 " + pkg)
            for q in PackageDB:
                if (q.find(pkg) != -1):
                    #print("3 " + q)
                    Result.append(q)
                    break
    else:
        Result = PackageDB
    return map(lambda(a): (ROOT + "/" + a), Result)

#print(listpkgs("libm3"))
#print(listpkgs("m3-libs/libm3"))
#print(listpkgs(ROOT + "/m3-libs/libm3"))
