#! /usr/bin/env python

import sys
import os.path
import pylib
from pylib import *

# usage: ./boot1autotools.py target
#
# tar xf xx.tgz
# chmod +x xx/*
# mkdir build
# cd build
# ../xx/configure
# make
# mkdir -p $HOME/cm3/bin
# mv cm3 $HOME/cm3/bin
# PATH=$HOME/cm3/bin/bin:$PATH
# cd ..
# ../boot2.py
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
AC_PROG_CXX
AX_PTHREAD
LIBS="$PTHREAD_LIBS $LIBS"
CXXFLAGS="$CXXFLAGS $PTHREAD_CFLAGS"
CC="$PTHREAD_CXX"
CXX="$PTHREAD_CXX"

# Carry forward historical CFLAGS, but this is probably entirely over-specified.
case "$host" in
    x86_64*darwin*) CXXFLAGS="$CXXFLAGS -arch x86_64";;
    i?86*darwin*) CXXFLAGS="$CXXFLAGS -arch i386";;
    powerpc64*darwin*) CXXFLAGS="$CXXFLAGS -arch ppc64";;
    powerpc*darwin*) CXXFLAGS="$CXXFLAGS -arch ppc";;
    mips64*) CXXFLAGS="$CXXFLAGS -mabi=64";;
    i?86-*-solaris2*) CXXFLAGS="$CXXFLAGS -xarch=""" + solaris86_arch + """ -Kpic";;
    x86_64-* | amd64-*) CXXFLAGS="$CXXFLAGS -m64";;
    i?86-*) CXXFLAGS="$CXXFLAGS -m32";;
    aarch64*darwin*_ilp32) CXXFLAGS="$CXXFLAGS -m32";;
    aarch64*darwin* | arm64*darwin*) CXXFLAGS="$CXXFLAGS -m64";;
    # Old unfinished 32bit iPhone support
    arm*-darwin*) CXXFLAGS="$CXXFLAGS -march=armv6 -mcpu=arm1176jzf-s";;
    sparc64* | sparcv9*)
      if test "$GXX" = yes; then
        CXXFLAGS="$CXXFLAGS -m64 -mno-app-regs -pthread"
      else
        CXXFLAGS="$CXXFLAGS -xarch=v9 -xcode=pic32 -xregs=no%appl -mt"
      fi;;
    sparc*)
      if test "$GXX" = yes; then
        CXXFLAGS="$CXXFLAGS -m32 -mcpu=v9 -mno-app-regs -pthread"
      else
         CXXFLAGS="$CXXFLAGS -xarch=v8plus -xcode=pic32 -xregs=no%appl -mt"
      fi;;
esac

# Specify likely required compiler/linker flags where defaults
# do not likely suffice or really are not great (e.g. HPUX null deref, Alpha arounding mode).
case "$host" in
    *-*-osf*)
        # Set rounding mode.
        # TODO loop over the choices and chose which works.
        if test "$GXX" = yes; then
            CXXFLAGS="$CXXFLAGS -mfp-rounding-mode=d -pthread -mieee"
        else
            CXXFLAGS="$CXXFLAGS -fprm d -pthread -readonly_strings -ieee_with_no_inexact"
        fi
        ;;
    # Must be gcc or compatible. TODO: probe
    alpha*) CXXFLAGS="$CXXFLAGS -mfp-rounding-mode=d";;
esac
case "$host" in
    *-*-darwin*);;
    # TODO Higher level syntax for libraries?
    *-*-linux* | *-*-*bsd* | *-*-cygwin*) CXXFLAGS="$CXXFLAGS -lm -pthread";;
    *-*-mingw*) CXXFLAGS="$CXXFLAGS -liphlpapi -lrpcrt4 -lcomctl32 -lws2_32 -lgdi32 -luser32 -ladvapi32";;
    *-*-solaris*) CXXFLAGS="$CXXFLAGS -lpthread -lrt -lm -lnsl -lsocket -lc -pthread";;
    *-*-osf*)
        CXXFLAGS="$CXXFLAGS -lrt -lm -pthread"

        # There is a problem on some installs such that linking with cxx fails, unless oldcxx is used.
        # One of the startup .o files is missing.
        # This really should be fixed otherwise.
        # Compiling with oldcxx fails, because it does not
        # define _LONGLONG and then INT64_ appends i64 or ui64 and that fails.
        # Fix the installs. Autoconf to detect problem.
        # and only workaround if needed (and remove gcc check).
        if test "$GXX" != yes; then
            LDFLAGS="$LDFLAGS -oldcxx"
            CXXFLAGS="$CXXFLAGS -x cxx" # when applied to .c files
        fi
        ;;
    *-*-hpux*)
        # -ldcekt is for uuid_create in MachineIDPosixC.c
        # -pthread is not allowed
        # -z so null derefence fails (vs. returns zero).
        CXXFLAGS="$CXXFLAGS -mt -z -lrt -lm -lpthread -ldcekt"

        # TODO: Make one bootstrap for 32bits and 64bits.

        if test "$GXX" = yes; then
            CXXFLAGS="$CXXFLAGS """ + hpux_gcc_wordsize + """ "
        else
            CXXFLAGS="$CXXFLAGS """ + hpux_cc_bits + """ "
        fi
        ;;
    *)  CXXFLAGS="$CXXFLAGS -lm";;
esac
case "$host" in
    ia64-*-hpux*) # -luca is for getting IP from context in RTSignalC.c
                  CXXFLAGS="$CXXFLAGS -luca";;
esac

CFLAGS="$CFLAGS $CXXFLAGS"

# Compile as C++.
# This is not very important now that C++ backend outputs .cpp files,
# but it does affect m3core, etc.
if test "$GCC" = yes; then
    CFLAGS="$CFLAGS -x c++"
fi

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
            "chmod +x *"]:
        print(a)
        os.system(a)
        a = "wsl " + a
        print(a)
        os.system(a)
    os.chdir("..")

    pylib._MakeTGZ(BootDir[2:])

BootAutoTools();
