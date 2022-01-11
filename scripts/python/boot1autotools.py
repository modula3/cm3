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

    Am = open(os.path.join(BootDir, "Makefile.am"), "wb")
    Ac = open(os.path.join(BootDir, "configure.ac"), "wb")

    readme = open(os.path.join(BootDir, "README"), "wb");
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

    if Config.find("32") != -1:
        hpux_gcc_wordsize = "-milp32"
        hpux_cc_bits = "+DD32"
        solaris86_arch = "pentium_pro"
    else:
        hpux_gcc_wordsize = "-mlp64"
        hpux_cc_bits = "+DD64"
        solaris86_arch = "amd64"

    Ac.write("""
AC_INIT(cm3,1.0)
AC_CANONICAL_HOST
AM_INIT_AUTOMAKE([-Wportability -Wall -Werror foreign])
AM_MAINTAINER_MODE # to have it disabled by default due to clock skew
AC_PROG_CXX

AS_CASE([$host],
    [*-*-mingw* | *-*-cygwin*], [ # optimize configure
     CC="$CXX" # .c files are C++
    ],
    [AX_PTHREAD
     LIBS="$PTHREAD_LIBS $LIBS"
     CXXFLAGS="$CXXFLAGS $PTHREAD_CFLAGS"
     CC="$PTHREAD_CXX" # .c files are C++
     CXX="$PTHREAD_CXX"
    ])

# Carry forward historical CFLAGS, but this is probably entirely over-specified.
# TODO: -fPIC everywhere.
AS_CASE([$host],
    [x86_64*haiku],             [CXXFLAGS="$CXXFLAGS -m64 -fPIC"],
    [i?86*haiku],               [CXXFLAGS="$CXXFLAGS -fPIC"],           # -m32 works with gcc-x86 but not old gcc
    [x86_64*darwin*],           [CXXFLAGS="$CXXFLAGS -arch x86_64"],
    [i?86*darwin*],             [CXXFLAGS="$CXXFLAGS -arch i386"],
    [powerpc64*darwin*],        [CXXFLAGS="$CXXFLAGS -arch ppc64"],
    [powerpc*darwin*],          [CXXFLAGS="$CXXFLAGS -arch ppc"],
    [mips64*],                  [CXXFLAGS="$CXXFLAGS -mabi=64"],
    [i?86-*-solaris2*],         [CXXFLAGS="$CXXFLAGS -xarch=""" + solaris86_arch + """ -Kpic"],
    [i?86-* | *ilp32],          [CXXFLAGS="$CXXFLAGS -m32"],
    [x86_64* | amd64* | arm64*
     | aarch64*],               [CXXFLAGS="$CXXFLAGS -m64"],
    [arm*-darwin*],             [CXXFLAGS="$CXXFLAGS -march=armv6 -mcpu=arm1176jzf-s"], # Old unfinished 32bit iPhone support
    [sparc64* | sparcv9*],      [AS_IF([test x$GXX = xyes],
                                       [CXXFLAGS="$CXXFLAGS -m64 -mno-app-regs -pthread"],
                                       [CXXFLAGS="$CXXFLAGS -xarch=v9 -xcode=pic32 -xregs=no%appl -mt"])],
    [sparc*],                   [AS_IF([test x$GXX = xyes],
                                       [CXXFLAGS="$CXXFLAGS -m32 -mcpu=v9 -mno-app-regs -pthread"],
                                       [CXXFLAGS="$CXXFLAGS -xarch=v8plus -xcode=pic32 -xregs=no%appl -mt"])])

# Specify likely required compiler/linker flags where defaults
# do not likely suffice or really are not great (e.g. HPUX null deref, Alpha arounding mode).

AS_CASE([$host],
    [alpha*], # Set Alpha rounding mode etc.
    [AS_IF([test x$GXX = xyes],
           [CXXFLAGS="$CXXFLAGS -mfp-rounding-mode=d -mieee"],
           [CXXFLAGS="$CXXFLAGS -fprm d -readonly_strings -ieee_with_no_inexact"])])

<<<<<<< HEAD
=======
# AS_CASE can mostly be removed, but it speeds up configure
>>>>>>> 02d99512d (Cygwin support.)
AS_CASE([$host],
    [*-*-darwin*], [ ],
    [*haiku], [
        LIBS="$LIBS -lnetwork"
        ],
    [*-*-linux* | *-*-*bsd*], [
        LIBS="$LIBS -lm"
        ],
    # For Cygwin we must use LIBS here, not CXXFLAGS.
    # -lfoo bar.o does not work, but bar.o -lfoo does.
    [*-*-mingw* | *-*-cygwin*], [
        LIBS="$LIBS -liphlpapi -lrpcrt4 -lcomctl32 -lws2_32 -lgdi32 -luser32 -ladvapi32"
        ],
    [*-*-solaris*], [
        LIBS="$LIBS -lrt -lm -lnsl -lsocket -lc"
        ],
    [*-*-osf*], [
        LIBS="$LIBS -lrt -lm"
        # There is a problem on some installs such that linking with cxx fails, unless oldcxx is used.
        # One of the startup .o files is missing.
        # This really should be fixed otherwise.
        # Compiling with oldcxx fails, because it does not
        # define _LONGLONG and then INT64_ appends i64 or ui64 and that fails.
        # Fix the installs. Autoconf to detect problem.
        # and only workaround if needed (and remove gcc check).
        AS_IF([test x$GXX != xyes],
              [LDFLAGS="$LDFLAGS -oldcxx"
               CXXFLAGS="$CXXFLAGS -x cxx" # when applied to .c files
              ])],
    [*-*-hpux*], [
        # dcekt: uuid_create in MachineIDPosixC.c
        LIBS="$LIBS -lrt -lm -ldcekt -lc"
        CXXFLAGS="$CXXFLAGS -z" # -z so null derefence fails (vs. returns zero).
        # TODO: Make one bootstrap for 32bits and 64bits.
        AS_IF([test x$GXX = xyes],
              [CXXFLAGS="$CXXFLAGS """ + hpux_gcc_wordsize + """ "],
              [CXXFLAGS="$CXXFLAGS """ + hpux_cc_bits + """ "])],
    [LIBS="$LIBS -lm"] # default
    )

# user context access for getting IP from context in RTSignalC.c
AS_CASE([$host], [ia64-*-hpux*], [LIBS="$LIBS -luca"])

CFLAGS="$CFLAGS $CXXFLAGS" # .c files are C++

# Compile .c as C++.
# This is not very important now that C++ backend outputs .cpp files,
# but it affects m3core, etc.
AS_IF([test x$GCC = xyes],[CFLAGS="$CFLAGS -x c++"])

AC_CONFIG_FILES([Makefile])
AC_OUTPUT
""")

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
    Ac.close()
    CopyFile("./ax_pthread.m4", BootDir)

    os.chdir(BootDir)
    #?? a = "autoreconf -i " + BootDir
    #  Does not let AX_PTHREAD work. What is the right way?
    for a in [
            "aclocal -I .",
            "autoconf -f",
            "automake --add-missing --copy --foreign",
            "rm -rf autom4te.cache",
            #"chmod +x *",     # does not work
            "rm Makefile.am",  # combat clock skew
            "rm configure.ac", # combat clock skew
            ]:
        print(a)
        os.system(a)
        a = "wsl " + a
        print(a)
        os.system(a)
    os.chdir("..")

    pylib._MakeTGZ(BootDir[2:])

BootAutoTools();
