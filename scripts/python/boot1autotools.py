#! /usr/bin/env python

import sys
import os.path
import pylib
from pylib import *

# usage: ./boot1autotools.py target
#
# tar xf xx.tgz
# cd xx
# chmod +x *                                 # permissions are wrong on some files
# mkdir build
# cd build
# ../configure --disable-dependency-tracking # dependency tracking is sometimes slow
# make -j11
# mkdir -p $HOME/cm3/bin
# mv cm3 $HOME/cm3/bin
# PATH=$HOME/cm3/bin:$PATH
# cd ../..
# ./boot2.py
#
# Target need not be precise, only word size and endian matter.
# In future hopefully they will not.
#
CopyConfigForDevelopment() or sys.exit(1)

def BootAutoTools():

    pylib.BuildLocal += " -boot -no-m3ship-resolution -group-writable -keep -DM3_BACKEND_MODE=C"

    Version = CM3VERSION + "-" + time.strftime("%Y%m%d")
    BootDir = "./cm3-boot-" + BuildDir + "-" + Version

    #RemoveDirectoryRecursive(BootDir)
    CreateDirectory(BootDir)

    P = FilterPackages(["m3core", "libm3", "sysutils", "set", "m3middle", "m3quake", "m3objfile", "m3linker", "m3back", "m3front"])
    main_packages = ["cm3"]

    P += main_packages

    #DoPackage(["", "realclean"] + P) or sys.exit(1)
    DoPackage(["", "buildlocal"] + P) or sys.exit(1)

    open(os.path.join(BootDir, "early.m4"), "w").write("enable_dependency_tracking=no\n")
    open(os.path.join(BootDir, "late.m4"), "w").write("\n")

    Am = open(os.path.join(BootDir, "Makefile.am"), "w")

    readme = open(os.path.join(BootDir, "README"), "w");
    readme.write("""
tar xf xx.tgz
cd xx
# chmod +x * is sometimes needed as sometimes the tar used
# does not save permissions e.g. on configure
# Specifically Windows native tar does not work, and
# wsl tar against Windows filesystem is slow.
chmod +x *
mkdir build
cd build
../configure --disable-dependency-tracking # dependency tracking is sometimes slow
make -j22                                  # make up a number for parallelism
mkdir -p $HOME/cm3/bin
mv cm3 $HOME/cm3/bin
PATH=$HOME/cm3/bin:$PATH
cd ../..
./boot2.py # or boot2min.py
""")
    readme.close()

    # TODO: Make one bootstrap for 32bits and 64bits.
    #
    # Maybe TODO: We can use "any" configure.ac but then we get CXXFLAGS/LIBRARIES,
    # and the bootstrap is one flat directory.

    configure = "configure64.ac"
    cminstall = path.join(Root, "m3-sys","cminstall","src","config-no-install")
    if Config.find("32") != -1:
        configure = "configure32.ac"
    CopyFile(os.path.join(cminstall, configure), os.path.join(BootDir, "configure.ac"))

# For now we just build cm3 all at once and provide no install.
# In future we will build each library, selectively shared,
# and install, and cm3 will output such files,
# via a new generic target named autotools.

    Am.write("noinst_PROGRAMS = cm3\n")
    Am.write("cm3_SOURCES = ")

    for q in P:
        dir = GetPackagePath(q)
        for a in os.listdir(os.path.join(Root, dir, BuildDir)):
            ext = GetPathExtension(a)
            ext_c = (ext == "c")
            ext_cpp = (ext == "cpp")
            ext_h = (ext == "h")
            if not (ext_c or ext_cpp or ext_h):
                continue
            b = 0
            if False: # subdirs
                b = os.path.join(BootDir, q)
                CreateDirectory(b)
                Am.write(" \\\n " + q + "/" + a)
            else:
                b = BootDir
                Am.write(" \\\n " + a)
            CopyFile(os.path.join(Root, dir, BuildDir, a), b) # TODO async

    Am.close()
    CopyFile(cminstall + "/ax_pthread.m4", BootDir)

    os.chdir(BootDir)
    #?? a = "autoreconf -i " + BootDir
    #  Does not let AX_PTHREAD work. What is the right way?
    for a in [
            "aclocal -I .",
            "autoconf -f",
            "automake --add-missing --copy --foreign",
            "rm -rf autom4te.cache",
            #"chmod +x *",      # does not work
            #"rm Makefile.am",  # combat clock skew, but ugly
            #"rm configure.ac", # combat clock skew, but ugly
            ]:
        print(a)
        os.system(a)
        a = "wsl " + a
        print(a)
        os.system(a)
    os.chdir("..")

    pylib._MakeTGZ(BootDir[2:])

BootAutoTools();
