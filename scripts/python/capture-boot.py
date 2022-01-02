#! /usr/bin/env python

import sys, os.path, os, shutil, pylib, uuid
from os.path import isfile, isdir
from pylib import CopyFile, CreateDirectory

root = sys.argv[1] or "/cm3"
temp = "cm3temp-" + str(uuid.uuid4()).upper()
output = sys.argv[2] or "cm3boot"

def CopyRecursive(From, To):
    CopyCommand = "xcopy /fiverdyh "
    ToParent = os.path.dirname(To)
    if (os.name != "nt"):
        CopyCommand = "cp --preserve  --recursive "
    print("mkdir " + ToParent)
    print(CopyCommand + From + " " + To)
    if os.path.isdir(To):
        shutil.rmtree(To)
    else:
        CreateDirectory(ToParent)
    shutil.copytree(From, To, symlinks=True)

def copy(a):

    if isdir(os.path.join(root, "pkg", a)):
        CopyRecursive(os.path.join(root, "pkg", a),
                     os.path.join(temp, "pkg", a))

    if isfile(os.path.join(root, a)):
        pass

    elif isfile(os.path.join(root, "bin", a)):
        a = os.path.join("bin", a)

    elif isfile(os.path.join(root, "bin", a + ".exe")):
        a = os.path.join("bin", a + ".exe")
        
    # Take only static libs -- foo.lib for Windows, libfoo.a for Posix,
    # whichever exists.
    elif isfile(os.path.join(root, "lib", a + ".lib")):
        a = os.path.join("lib", a + ".lib")

    elif isfile(os.path.join(root, "lib", "lib" + a + ".a")):
        a = os.path.join("lib", "lib" + a + ".a")
        
    else:
        return
        
    CopyFile(os.path.join(root, a), os.path.join(temp, a))

CreateDirectory(os.path.join(temp, "lib"))
CreateDirectory(os.path.join(temp, "bin"))
CreateDirectory(os.path.join(temp, "pkg"))
copy("cm3")
copy("cm3.cfg")
copy("cm3cg")
copy("libm3")
copy("m3")
copy("m3core")
if isdir(os.path.join(root, "bin", "config")):
    CopyRecursive(os.path.join(root, "bin", "config"),
                  os.path.join(temp, "bin", "config"))

def Run(a):
    print(a + " in " + os.getcwd())
    #return True
    return (os.system(a) == 0)
    
os.chdir(temp)
Run("tar cfz ../" + output + ".tar.gz .")
