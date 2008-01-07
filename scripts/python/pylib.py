#! /usr/bin/env python
# $Id: pylib.py,v 1.12 2008-01-07 09:44:50 jkrell Exp $

import os
import os.path
import glob
import sys
import platform
import re
import tempfile
import shutil

# print("loading pylib..")

env_OS = os.getenv("OS")
if env_OS == "Windows_NT":
    def uname():
        PROCESSOR_ARCHITECTURE = os.getenv("PROCESSOR_ARCHITECTURE")
        return (env_OS, "", PROCESSOR_ARCHITECTURE, "", PROCESSOR_ARCHITECTURE)
else:
    from os import uname

PackageDB = None

#
# User can override all these from environment, as in sh.
# The environment variable names are all UPPERCASE.
#
Variables = [

    #
    # typically just "cm3", subject to path search,
    # but sometimes set to a specific cm3
    #
    "CM3",

    #
    # where to look for tar.exe, gzip.exe when building Win32 distributions
    # This should go away and we should just use zip.exe from that path.
    #
    "CM3BINSEARCHPATH",

    #
    # the root of the source code (ROOT), but with
    # backward slashes doubled,
    # e.g. c:\\dev2\\cm3 or /dev2/cm3
    # The same as ROOT on Unix.
    # This is easily computed from __file__.
    #
    "CM3ROOT",

    "CM3VERSION",

    #
    # used in making distribution
    #
    "DEV_BIN",
    "DEV_LIB",

    #
    # an empty string for Posix, ".exe" for Win32
    # appended to executable paths to form actual file paths
    #
    "EXE",

    #
    # True or False -- does this platform use the gcc backend.
    # True for all but NT386
    #
    "GCC_BACKEND",

    # the root if the cm3 install, e.g. /cm3 or /usr/local/cm3
    # This is easily computed by searching for cm3 or cm3.exe
    # in $PATH.
    "INSTALLROOT",

    #
    # True or False -- should we build m3gdb.
    #
    "M3GDB",

    #
    # WIN32 or POSIX -- used to help decide what
    # packages to build
    #
    "M3OSTYPE",

    #
    # path to the pkgsdb file, usually __file__/../pkgsdb
    #
    "PKGSDB",

    #
    # the root of the source code, e.g. /dev2/cm3
    # This is __file__/../../..
    #
    "ROOT",

    #
    # a slash to use in file paths, e.g. \ or /
    # Forward slashes often work on Win32 so
    # this can probably go away.
    #
    "SL",

    #
    # A temporary "staging" location? For
    # building distributions?
    #
    "STAGE",

    #
    # directory that contains kernel32.lib, etc. that
    # are included in the Win32 distributation
    # This can go away.
    #
    "SYSLIBDIR",

    #
    # files in SYSLIBDIR to include in the Win32 distribution
    # kernel32.lib etc.
    # This can go away.
    #
    "SYSLIBS",

    #
    # tar to use when building distributions
    # This can go away and just use "tar" from $PATH
    #
    "TAR",

    #
    # very important -- what operating system/processor architecture
    # we are building for
    #
    "TARGET",

    "BuildArgs",
    "CleanArgs",
    "ShipArgs",

    "M3Ship",
    "M3Build",

    "BuildLocal",
    "CleanLocal",
    "BuildGlobal",
    "CleanGlobal",
    "Ship",

    "CM3_BuildLocal",
    "CM3_CleanLocal",
    "CM3_BuildGlobal",
    "CM3_CleanGlobal",
    "CM3_Ship",

    "PM3_BuildLocal",
    "PM3_CleanLocal",
    "PM3_BuildGlobal",
    "PM3_CleanGlobal",
    "PM3_Ship",

    "SRC_BuildLocal",
    "SRC_CleanLocal",
    "SRC_BuildGlobal",
    "SRC_CleanGlobal",
    "SRC_Ship",

    "RealClean",
    #"IGNORE_MISS",
    "HAVE_TCL",
    "HAVE_SERIAL",
    "OMIT_GCC",

    # whether or not this program should print extra stuff
    "CM3_Debug",

    # build all packages, generally does not work
    "CM3_ALL",
]

DefaultsFromSh = {
    "CM3VERSION" : None,
    "CM3VERSIONNUM" : None,
    "CM3LASTCHANGED" : None,
    }

Variables += DefaultsFromSh.keys()

#
# Ensure all variables have some value.
#
b = ""
for a in Variables:
    b += ("%s = os.getenv(\"%s\") or \"\"\n" % (a, a.upper()))
exec(b)

# print("pylib: INSTALLROOT is "  + INSTALLROOT)
# print("pylib: env_INSTALLROOT is " + (os.environ.get("INSTALLROOT") or ""))

for a in DefaultsFromSh.keys():
    DefaultsFromSh[a] = eval(a)

#-----------------------------------------------------------------------------
# output functions

def debug(a):
    if (os.getenv("CM3_DEBUG") or CM3_Debug):
        print(a + " is " + eval("str(" + a + ")"))

def header(a):
    print("")
    print( "----------------------------------------------------------------------------")
    print(a)
    print("----------------------------------------------------------------------------")
    print("")

debug("sys.platform")
debug("platform.machine()")
debug("platform.processor()")
debug("platform.release()")

uname_tuple = uname()
debug("uname_tuple")
UNAME = uname_tuple[0].lower()
UNAME_P = platform.processor().lower()
UNAME_M = uname_tuple[4].lower()
UNAME_R = uname_tuple[2].lower()

debug("UNAME")
debug("UNAME_M")
debug("UNAME_P")
debug("UNAME_R")

#-----------------------------------------------------------------------------
# set some defaults

def GetDefaultFromSh(Key):
    #
    # Only read the file if an environment variable is "missing" (they
    # usually all are, ok), and only read it once.
    #
    #print("WriteVariablesIntoEnvironment:3")
    Value = DefaultsFromSh.get(Key)
    if (Value):
        return Value
    #
    # CM3VERSION=${CM3VERSION:-"d5.5.1"}
    # CM3VERSIONNUM=${CM3VERSIONNUM:-"050501"}
    # CM3LASTCHANGED=${CM3LASTCHANGED:-"2007-12-30"}
    #
    RegExp = re.compile("(" + "|".join(DefaultsFromSh.keys()) + ")=\\$\\{\\1:-\"([^\"]+)\"\\}$")
    ShFilePath = os.path.join(os.path.dirname(os.path.dirname(os.path.abspath(__file__))), "sysinfo.sh")
    for Line in open(ShFilePath):
        Match = RegExp.match(Line)
        if (Match):
            MatchKey = Match.group(1)
            #
            # We are here because one of them wasn't found, but we should be
            # sure only to overwrite what we don't have.
            #
            if (not DefaultsFromSh[MatchKey]):
                Value = Match.group(2)
                DefaultsFromSh[MatchKey] = Value
                exec("%s = \"%s\"" % (MatchKey, Value), locals(), globals())

    #
    # Make sure we found every key in the file (at least those
    # not defined in the environment)
    #
    MissingKey = None
    for Item in DefaultsFromSh.iteritems():
        #print(Item)
        if (Item[1] is None):
            MissingKey = Item[0]
            File = __file__
            sys.stderr.write("%(File)s: %(MissingKey)s not found in %(ShFilePath)s\n" % vars())

    if (MissingKey):
        sys.exit(1)

    return DefaultsFromSh.get(Key)

CM3VERSION = CM3VERSION or GetDefaultFromSh("CM3VERSION")
CM3VERSIONNUM = CM3VERSIONNUM or GetDefaultFromSh("CM3VERSIONNUM")
CM3LASTCHANGED = CM3LASTCHANGED or GetDefaultFromSh("CM3LASTCHANGED")

CM3_GCC_BACKEND = True
CM3_GDB = False

debug("CM3VERSION")

#
# if CM3_INSTALL is not set, and cm3 is in $PATH, cm3's directory's directory is CM3_INSTALL,
# else CM3_DEFAULTS defaults to /usr/local/cm3
#
def ExeName(a):
    debug("os.name")
    if (os.name == "nt"):
        a += ".exe"
    return a

#
# http://aspn.activestate.com/ASPN/Cookbook/Python/Recipe/52224
#
def SearchPath(name, paths = os.getenv("PATH")):
    #Given a search path, find file
    if (os.name == "nt"):
        # support for $PATHEXT might be nice
        if (name.find(".") == -1):
            name += ".exe"
    for path in paths.split(os.path.pathsep):
        candidate = os.path.join(path, name)
        # print("candidate is " + candidate)
        if (os.path.isfile(candidate)):
            return os.path.abspath(candidate)

CM3_INSTALL = (
    os.getenv("CM3_INSTALL")
    or os.path.dirname(os.path.dirname(SearchPath("cm3") or ""))
    or "/usr/local/cm3"
    )

debug("CM3_INSTALL")

CM3 = CM3 or ExeName("cm3")
M3Build = M3Build or ExeName("m3build")
M3Ship = M3Ship or "m3ship"
EXE = "" # executable extension, ".exe" or empty
SL = "/" # path slash, forward or backward
Q = "'"

SYSLIBDIR = os.path.join(CM3_INSTALL, "lib")
XDEV_LIB = ""
XDEV_BIN = ""
TAR = "tar"

#-----------------------------------------------------------------------------
# some localization functions

def find_file(file, dirs):
    for dir in dirs:
        if (os.path.isdir(dir)):
            a = os.path.join(dir, file)
            if (os.path.isfile(a)):
                return a

#-----------------------------------------------------------------------------
# abstraction functions

def cygpath(a, b):
    #print("cygpath:return b")
    return b

def strip_exe(a):
    #print("strip_exe:os.system(\"strip " + a + "\")")
    os.system("strip " + a)

#-----------------------------------------------------------------------------
# evaluate uname ermation

#
# This is all a bit wonky.
# Data derives from both uname and optional overrides in environment variables.
# If use sets TARGET environment variable, then user is obligated to
# to get other things correct such as M3OSTYPE
#

TARGET = os.getenv("TARGET") or ""
CM3_OSTYPE = "POSIX"

if (UNAME.startswith("windows")
        or UNAME.startswith("winnt")
        or UNAME.startswith("cygwin")
        or TARGET.startswith("NT386")
    ):

    if (TARGET.startswith("NT386GNU")):

        CM3_TARGET = "NT386GNU"
        GMAKE = os.getenv("GMAKE") or "make"

        def cygpath(a, b):
            #print("cygpath:os.popen(/usr/bin/cygpath " + a + " " + b + ").read().replace(\"\\n\", \"\")")
            #return b
            return os.popen("/usr/bin/cygpath " + a + " " + b).read().replace("\n", "")

    else:

        CM3_OSTYPE = "WIN32"
        CM3_TARGET = "NT386"
        CM3_INSTALL = "c:/cm3"
        CM3_GCC_BACKEND = False
        HAVE_SERIAL = True
        EXE = ".exe"
        SL = "\\"
        Q = ""
        SYSLIBS =  ["ADVAPI32.LIB", "GDI32.LIB", "KERNEL32.LIB", "ODBC32.LIB"]
        SYSLIBS += [" OPENGL32.LIB", "WSOCK32.LIB", "COMDLG32.LIB"]
        SYSLIBS += [" GLU32.LIB", "NETAPI32.LIB", "ODBCCP32.LIB", "USER32.LIB"]
        L =  ["c:/cm3/bin", "d:/cm3/bin e:/cm3/bin", "c:/reactor5/bin", "d:/reactor5/bin"]
        L += ["e:/reactor5/bin", "c:/reactor/bin", "d:/reactor/bin"]
        L += ["e:/reactor/bin", "/usr/local/cm3/bin", "/usr/local/reactor/bin"]
        L += ["/usr/cm3/bin", "/usr/reactor/bin"]
        CM3BINSEARCHPATH = L
        f = find_file("KERNEL32.LIB", L)
        if (f):
            SYSLIBDIR = os.path.dirname(f)
        else:
            SYSLIBDIR = "unknown"

        D = ["c:/msdev/bin", "d:/msdev/bin", "e:/msdev/bin", "f:/msdev/bin", "g:/msdev/bin"]
        f = find_file("cl.exe", D)
        if (f):
            XDEV_BIN = os.path.dirname(f)
            XDEV_LIB = os.path.join(XDEV_BIN, "lib")

        f = "/usr/bin/tar.exe"
        if (os.path.isfile(f)):
            TAR = f
        GMAKE = os.getenv("GMAKE") or "make"

        def strip_exe(a):
            #print("strip_exe:pass")
            pass

elif (UNAME.startswith("freebsd")):

    if (UNAME_M == "i386"):
        if (UNAME_R.startswith("1")):
            CM3_TARGET = "FreeBSD"
        elif (UNAME_R.startswith("2")):
            CM3_TARGET = "FreeBSD2"
        elif (UNAME_R.startswith("3")):
            CM3_TARGET = "FreeBSD3"
        elif (UNAME_R.startswith("4")):
            CM3_TARGET = "FreeBSD4"
        else:
            CM3_TARGET = "FreeBSD4"
    else:
        CM3_TARGET = "FBSD_ALPHA"

elif (UNAME.startswith("darwin")):

    # detect the m3 platform (Darwin runs on ppc and ix86)
    if (UNAME_P.startswith("powerpc")):
        CM3_TARGET = "PPC_DARWIN"
    elif (re.match("i[3456]86", UNAME_P)):
        CM3_TARGET = "I386_DARWIN"
    GMAKE = os.getenv("GMAKE") or "make"

elif (UNAME.startswith("sunos")):

    CM3_TARGET = "SOLgnu"
    #CM3_TARGET = "SOLsun"

elif (UNAME.startswith("linux")):

    GMAKE = os.getenv("GMAKE") or "make"
    GCWRAPFLAGS = "-Wl,--wrap,adjtime,--wrap,getdirentries,--wrap,readv,--wrap,utimes,--wrap,wait3"
    if (UNAME_M == "ppc"):
        CM3_TARGET = "PPC_LINUX"
    else:
        CM3_TARGET = "LINUXLIBC6"

elif (UNAME.startswith("netbsd")):

    GMAKE = os.getenv("GMAKE") or "make"
    CM3_TARGET = "NetBSD2_i386" # only arch/version combination supported yet

else:

    # more need to be added here, I haven't got all the platform info ready
    pass

DEV_BIN = (DEV_BIN or XDEV_BIN)
DEV_LIB = (DEV_LIB or XDEV_LIB)

#-----------------------------------------------------------------------------
# define the exported values

#
# ROOT is two levels above this program.
#

ROOT = (ROOT or os.path.dirname(os.path.dirname(os.path.dirname(os.path.abspath(__file__)))))

INSTALLROOT = (INSTALLROOT or CM3_INSTALL)

M3GDB = (M3GDB or CM3_GDB)
M3OSTYPE = (M3OSTYPE or CM3_OSTYPE)
TARGET = (TARGET or CM3_TARGET)
GCC_BACKEND = (GCC_BACKEND or CM3_GCC_BACKEND)
PKGSDB = (PKGSDB or os.path.join(os.path.dirname(os.path.abspath(__file__)), "PKGS"))
GMAKE = (GMAKE or "gmake")

if (M3OSTYPE == "WIN32"):
    CM3ROOT = cygpath("-w", ROOT).replace("\\", "\\\\")
else:
    CM3ROOT = ROOT

#-----------------------------------------------------------------------------
# elego customizations
#
# comment these if they interfere with your environment

if (not os.getenv("STAGE")):

    if ((M3OSTYPE == "POSIX")
            and os.system("type domainname > /dev/null 2>/dev/null")
            and (popen("domainname 2>/dev/null").read.replace("\n", "") == "elegoohm")):

        STAGE = "/pub/lang/m3/cm3-dist"

    elif (M3OSTYPE == "WIN32" and (os.getenv("HOSTNAME") == "FIR")):

        STAGE = "c:/tmp/cm3stage"

#-----------------------------------------------------------------------------
# debug output

debug("ROOT")
debug("M3GDB")
debug("M3OSTYPE")
debug("TARGET")
debug("GCC_BACKEND")
debug("INSTALLROOT")
debug("PKGSDB")
debug("GMAKE")
debug("EXE")
debug("SL")
debug("SYSLIBDIR")
debug("SYSLIBS")
debug("DEV_BIN")
debug("DEV_LIB")
debug("TAR")
debug("CM3ROOT")

# define build and ship programs for Critical Mass Modula-3

DEFS = "-DROOT=%(Q)s%(CM3ROOT)s%(Q)s"
DEFS += " -DCM3_VERSION_TEXT=%(Q)s%(CM3VERSION)s%(Q)s"
DEFS += " -DCM3_VERSION_NUMBER=%(Q)s%(CM3VERSIONNUM)s%(Q)s"
DEFS += " -DCM3_LAST_CHANGED=%(Q)s%(CM3LASTCHANGED)s%(Q)s"
DEFS = (DEFS % vars())

if BuildArgs:
    BuildArgs = " " + BuildArgs

if CleanArgs:
    CleanArgs = " " + CleanArgs

if ShipArgs:
    ShipArgs = " " + ShipArgs

CM3_BuildLocal = BuildLocal or "%(CM3)s -build -override %(DEFS)s%(BuildArgs)s"
CM3_CleanLocal = CleanLocal or "%(CM3)s -clean -build -override %(DEFS)s%(CleanArgs)s"
CM3_BuildGlobal = BuildGlobal or "%(CM3)s -build %(DEFS)s%(BuildArgs)s"
CM3_CleanGlobal = CleanGlobal or "%(CM3)s -clean %(DEFS)s%(CleanArgs)s"
CM3_Ship = Ship or "%(CM3)s -ship %(DEFS)s%(CleanArgs)s"

# define build and ship programs for Poly. Modula-3 from Montreal

PM3_BuildLocal = BuildLocal or "%(M3Build)s -O %(DEFS)s%(BuildArgs)s"
PM3_CleanLocal = CleanLocal or "%(M3Build)s clean -O %(DEFS)s%(CleanArgs)s"
PM3_BuildGlobal = BuildGlobal or "%(M3Build)s %(DEFS)s %(BuildArgs)s)s"
PM3_CleanGlobal = CleanGlobal or "%(M3Build)s clean %(DEFS)s%(CleanArgs)s"
PM3_Ship = Ship or "%(M3Ship)s %(DEFS)s%(ShipArgs)s"

# define build and ship programs for DEC SRC Modula-3

SRC_BuildLocal = BuildLocal or "%(M3Build)s -O %(DEFS)s%(BuildArgs)s"
SRC_CleanLocal = CleanLocal or "%(M3Build)s clean -O %(DEFS)s%(CleanArgs)s"
SRC_BuildGlobal = BuildGlobal or "%(M3Build)s %(DEFS)s%(BuildArgs)s"
SRC_CleanGlobal = CleanGlobal or "%(M3Build)s clean %(DEFS)s%(CleanArgs)s"
SRC_Ship = Ship or "%(M3Ship)s %(DEFS)s%(ShipArgs)s"

# other commands

if (os.name == "nt"):
    RealClean = RealClean or "if exist %(TARGET)s rmdir /q/s %(TARGET)s"
else:
    RealClean = RealClean or "rm -rf %(TARGET)s"

RealClean = (RealClean % vars())

# choose the compiler to use

if (SearchPath(CM3)):
    BuildLocal = CM3_BuildLocal
    CleanLocal = CM3_CleanLocal
    BuildGlobal = CM3_BuildGlobal
    CleanGlobal = CM3_CleanGlobal
    Ship = CM3_Ship
elif (SearchPath(M3Build)):
    BuildLocal = PM3_BuildLocal
    CleanLocal = PM3_CleanLocal
    BuildGlobal = PM3_BuildGlobal
    CleanGlobal = PM3_CleanGlobal
    Ship = CM3_Ship
else:
    if (not BuildLocal or not BuildGlobal or not Ship):
        File = __file__
        sys.stderr.write(
            "%(File)s: %(CM3)s or %(M3Build)s not found in your path, don't know how to compile\n"
            % vars())
        sys.exit(1)

BuildLocal = BuildLocal.strip() % vars()
CleanLocal = CleanLocal.strip() % vars()
BuildGlobal = BuildGlobal.strip() % vars()
CleanGlobal = CleanGlobal.strip() % vars()
Ship = Ship.strip() % vars()

def format_one(width, string):
    return ("%-*s" % (width, string))

def print_list(strings, NumberOfColumns):
    Result = ""
    Width = (72 / NumberOfColumns)
    Length = len(strings)
    i = 0
    while (i != Length):
        j = 0
        while ((i != Length) and (j != NumberOfColumns)):
            if (j == 0):
                if (i != 0):
                    Result += "\n"
                Result += "  "
            Result += format_one(Width, strings[i])
            i += 1
            j += 1
    return Result

def PrintList2(strings):
    return print_list(strings, 2)

def PrintList4(strings):
    return print_list(strings, 4)

def ShowUsage(args, Usage, P):
    for arg in args[1:]:
        if (arg in ["-h", "-help", "--help", "-?"]):
            print("")
            print("usage " + os.path.basename(args[0]) + ":")
            if (Usage):
                BaseName = os.path.basename(args[0])
                GenericCommands = """
  build | buildlocal          build a package with local overrides (default)
  buildglobal | buildship     build a package without overrides and ship it
  ship                        ship a package
  clean | cleanlocal          clean a package with local overrides
  cleanglobal                 clean a package without overrides
  realclean                   remove the TARGET directory of a package
"""

                GenericOptions = """
  -n                          no action (do not execute anything)
  -k                          keep going (ignore errors if possible)
"""
                if (P):
                    N = len(P)
                    Packages = PrintList4(P)
                print(Usage % vars())
            else:
                print("")
                print("No specific usage notes available.")
                print("")
                print("Generic commands:")
                print(GenericCommands)
                print("Generic options:")
                print(GenericOptions)
            sys.exit(0)

def MakePackageDB():
    if (not os.path.isfile(PKGSDB)):
        #
        # Look for all files src/m3makefile in the CM3 source
        # and write their relative paths from ROOT to PKGSDB.
        #
        def Callback(Result, Directory, Names):
            if (os.path.basename(Directory) != "src"):
                return
            if (Directory.find("_darcs") != -1):
                return
            if not "m3makefile" in Names:
                return
            if (not os.path.isfile(os.path.join(Directory, "m3makefile"))):
                return
            Result.append(Directory[len(ROOT) + 1:-4].replace(os.path.sep, "/") + "\n")

        print("making " + PKGSDB + ".. (slow but rare)")
        Result = [ ]

        os.path.walk(
            ROOT,
            Callback,
            Result
            )

        Result.sort()
        open(PKGSDB, "w").writelines(Result)

        if (not os.path.isfile(PKGSDB)):
            File = __file__
            sys.stderr.write("%(File)s: cannot generate package list\n" % vars())
            sys.exit(1)

def ReadPackageDB():
    MakePackageDB()
    global PackageDB
    PackageDB = (PackageDB or
            map(
                lambda(a): a.replace("\n", "").replace("/", os.path.sep),
                open(PKGSDB)
                ))

def IsPackageDefined(a):
    ReadPackageDB()
    a = (SL + a)
    for i in PackageDB:
        if i.endswith(a):
            return True

def GetPackagePath(a):
    ReadPackageDB()
    b = (SL + a)
    for i in PackageDB:
        if (i.endswith(b)):
            #print("pkgpath(%(a)s returning %(i)s (%(b)s)" % vars())
            return i
    File = __file__
    sys.stderr.write("%(File)s: package %(a)s not found (%(b)s)\n" % vars())

def ListPackages(pkgs):
    ReadPackageDB()
    Result = [ ]
    if pkgs:
        for pkg in pkgs:
            # remove ROOT from the start
            if (pkg.startswith(ROOT + SL)):
                pkg = pkg[len(ROOT) + 1:]
                #print("1 " + pkg)
            # if no slashes, then need a leading slash
            if (pkg.find(SL) == -1):
                pkg = (SL + pkg)
                #print("2 " + pkg)
            for q in PackageDB:
                if (q.find(pkg) != -1):
                    #print("3 " + q)
                    Result.append(q)
                    break
    else:
        Result = PackageDB
    return map(lambda(a): (ROOT + SL + a), Result)

def _Run(NoAction, Actions, PackageDirectory):

    print(" +++ %s +++" % Actions)

    if (NoAction):
        return 0

    PreviousDirectory = os.getcwd()
    os.chdir(PackageDirectory)

    for a in Actions:
        Result = os.system(a)
        if (Result != 0):
            break

    os.chdir(PreviousDirectory)
    return Result

ActionInfo = {
    "build":
    {
        "Commands": [BuildLocal],
    },
    "buildlocal":
    {
        "Commands": [BuildLocal],
    },
    "buildglobal":
    {
        "Commands": [BuildGlobal, Ship],
    },
    "buildship":
    {
        "Commands": [BuildGlobal, Ship],
    },
    "ship":
    {
        "Commands": [Ship],
    },
    "clean":
    {
        "Commands": [CleanLocal],
        "KeepGoing": True,
    },
    "cleanlocal":
    {
        "Commands": [CleanLocal],
        "KeepGoing": True,
    },
    "cleanglobal":
    {
        "Commands": [CleanGlobal],
        "KeepGoing": True,
    },
    "realclean":
    {
        "Commands": [RealClean],
        "KeepGoing": True,
    },
}

def _FilterPackage(Package):
    PackageConditions = {
        "m3gdb":
            (M3GDB or CM3_GDB) and
            {"FreeBSD4": True,
            "LINUXLIBC6" : True,
            "SOLgnu" : True,
            "NetBSD2_i386" : True
            }.get(TARGET, False),

        "m3objfile": CM3_ALL or M3OSTYPE == "WIN32",
        "mklib": CM3_ALL or M3OSTYPE == "WIN32",
        "dll2lib": CM3_ALL or M3OSTYPE == "WIN32",
        "fix_nl": CM3_ALL or M3OSTYPE == "WIN32",
        "libdump": CM3_ALL or M3OSTYPE == "WIN32",
        "import-libs": CM3_ALL or M3OSTYPE == "WIN32",

        "tcl": CM3_ALL or HAVE_TCL,
        "udp": CM3_ALL or M3OSTYPE == "POSIX",
        "tapi": CM3_ALL or M3OSTYPE == "POSIX",
        "serial": CM3_ALL or HAVE_SERIAL,

        "X11R4": CM3_ALL or M3OSTYPE != "WIN32",
        "showthread": CM3_ALL or M3OSTYPE != "WIN32",
        "pkl-fonts": CM3_ALL or M3OSTYPE != "WIN32",
        "juno-machine": CM3_ALL or M3OSTYPE != "WIN32",
        "juno-compiler": CM3_ALL or M3OSTYPE != "WIN32",
        "juno-app": CM3_ALL or M3OSTYPE != "WIN32",

        "m3back": not GCC_BACKEND,
        "m3staloneback": not GCC_BACKEND,
        "m3cc": GCC_BACKEND and not OMIT_GCC,
    }
    return PackageConditions.get(Package, True)

def FilterPackages(Packages):
    Packages = filter(_FilterPackage, Packages)
    return Packages

PackageSets = {
    "core" :
        [
        "m3core",
        "libm3",
        "patternmatching",
        "m3middle",
        "m3objfile",
        "m3linker",
        "m3back",
        "m3staloneback",
        "m3front",
        "m3quake",
        "m3cc",
        "cm3",
        "m3scanner",
        "m3tools",
        "m3cgcat",
        "m3cggen",
        "m3gdb",
        "m3bundle",
        "mklib",
        "dll2lib",
        "fix_nl",
        "libdump",
        "bitvector",
        "digraph",
        "parseparams",
        "realgeometry",
        "set",
        "slisp",
        "sortedtableextras",
        "table-list",
        "tempfiles",
        "tcl",
        ]
}

def DoPackage(args, PackagesFromCaller = None):

    # print("args is " + str(args))
    # sys.stdout.flush()

    if PackagesFromCaller:
        PackagesFromCaller = FilterPackages(PackagesFromCaller)

    if PackagesFromCaller:
        Usage = \
"""
%(BaseName)s [ GenericOptions ] [ GenericCommand ]

  will apply the given symbolic command to the following %(N)s packages:

%(Packages)s

GenericOptions:
%(GenericOptions)s

GenericCommand:
%(GenericCommands)s"""
    else:
        Usage = \
"""
%(BaseName)s [ GenericOptions ] [ GenericCommand ] pkg+

will apply the given symbolic command to one or more CM3 packages.

GenericOptions:
%(GenericOptions)s

GenericCommand:
%(GenericCommands)s"""

    ShowUsage(
        args,
        Usage,
        PackagesFromCaller,
        )

    if (not PackagesFromCaller and not args[1:]):
        print("no actions and no packages specified\n")
        sys.stdout.flush()
        sys.exit(1)

    PackagesFromCommandLine = []
    ActionCommands = []
    Packages = None
    ListOnly = False
    KeepGoing = False
    NoAction = False
    for arg in args[1:]:
        if arg == "":
            continue
        if (arg.startswith("-")):
            if arg == "-l":
                ListOnly = True
            elif arg == "-k":
                KeepGoing = True
            elif arg == "-n":
                NoAction = True
            else:
                ExtraArgs += arg
        else:
            if not ActionCommands:
                Action = ActionInfo.get(arg)
                if Action:
                    ActionCommands = Action["Commands"]
                    KeepGoing = Action.get("KeepGoing", False)
                else:
                    PackagesFromCommandLine.append(arg)
            else:
                PackagesFromCommandLine.append(arg)

    if not ActionCommands:
        if PackagesFromCaller:
            ActionCommands = ActionInfo["build"]["Commands"]
        else:
            print("no actions specified " + args[0])
            sys.stdout.flush()
            sys.exit(1)

    if PackagesFromCaller:
        if PackagesFromCommandLine:
            print("cannot specify packages on command line with " + args[0])
            sys.stdout.flush()
            sys.exit(1)
        Packages = PackagesFromCaller
    else: # not PackagesFromCaller:
        if not PackagesFromCommandLine:
            print("no packages specified " + args[0])
            sys.stdout.flush()
            sys.exit(1)
        Packages = PackagesFromCommandLine

    PackageDirectories = [ ]

    for p in Packages:

        q = p
        if (os.path.isdir(q)):
            PackageDirectories.append(q)
            continue

        q = os.path.join(ROOT, p)
        if (os.path.isdir(q)):
            PackageDirectories.append(q)
            continue

        q = GetPackagePath(p)
        if (not q):
            File = __file__
            sys.stderr.write("%(File)s *** cannot find package %(p)s\n" % vars())
            sys.exit(1)

        if (os.path.isdir(q)):
            PackageDirectories.append(q)
            continue

        q = os.path.join(ROOT, q)
        if (os.path.isdir(q)):
            PackageDirectories.append(q)
            continue

        File = __file__
        sys.stderr.write("%(File)s *** cannot find package %(p)s / %(q)s\n" % vars())
        sys.exit(1)

    if (not ActionCommands):
        File = __file__
        sys.stderr.write("%(File)s: no action defined, aborting\n" % vars())
        sys.exit(1)
        #return False

    if (not PackageDirectories):
        File = __file__
        sys.stderr.write("%(File)s: no packages\n" % vars())
        sys.exit(1)
        #return False

    if (ListOnly):
        ListPackage(PackageDirectories)
        sys.exit(0)
        #return True

    Success = True

    for p in PackageDirectories:
        print("== package %(p)s ==" % vars())
        ExitCode = _Run(NoAction, ActionCommands, p)
        if (ExitCode != 0):
            Success = False
            if (not KeepGoing):
                print(" *** execution of %s failed ***" % (str(ActionCommands)))
                sys.exit(1)
        if (KeepGoing):
            print(" ==> %s returned %s" % (str(ActionCommands), ExitCode))
        else:
            print(" ==> %(p)s done" % vars())

    return Success

def GetConfig(Root, Target):
#
# Favor the config-no-install directory, else fallback to config.
#
    a = os.path.join(Root, "m3-sys", "cminstall", "src")
    b = os.path.join(a, "config-no-install", Target)
    if (os.path.isfile(b)):
        return b
    b = os.path.join(a, "config", Target)
    return b

def CreateDirectory(a):
    if (not os.path.isdir(a)):
        os.makedirs(a)
    return True

def CopyFile(From, To):
    if (os.path.isdir(To)):
        To = os.path.join(To, os.path.basename(From))
    if (os.path.isfile(To)):
        os.remove(To)
    CopyCommand = "copy"
    if (os.name != "nt"):
        CopyCommand = "cp -Pv"
    print(CopyCommand + " " + From + " " + To)
    shutil.copy(From, To)
    return True

def CopyFileIfExist(From, To):
    if (os.path.isfile(From)):
        return CopyFile(From, To)
    return True

def ShipCompiler():
    #
    # The compiler has trouble shipping itself currently because it in use.
    # This is easily dealt with on NT by moving it away to a unique location, MoveFileEx to delete (admin-only).
    # Do it manually for now.
    # Experimentation is needed on doing better than MoveFileEx, in particular, an .exe can unmapped, and then
    # probably deleted, as long as you don't return to it. Or PERHAPS save the memory away and remap it.
    #
    FromSys = os.path.join(ROOT, "m3-sys")
    FromBin = os.path.join(FromSys, "cm3", TARGET)
    ToBin = os.path.join(INSTALLROOT, "bin")
    CreateDirectory(ToBin)
    CopyFile(os.path.join(FromBin, "cm3" + EXE), ToBin) or FatalError()
    CopyFile(GetConfig(ROOT, TARGET), os.path.join(ToBin, "cm3.cfg")) or FatalError()
    CopyFileIfExist(os.path.join(FromBin, "cm3cg" + EXE), ToBin) or FatalError()
    if (os.name == "nt"):
        CopyFile       (os.path.join(FromBin, "cm3.pdb"), ToBin) or FatalError()
        CopyFileIfExist(os.path.join(FromBin, "cm3.exe.manifest"), ToBin) or FatalError()
    return True

if __name__ == "__main__":
    #
    # run test code if module run directly
    #

    GetDefaultFromSh("CM3VERSION")
    #sys.stdout.flush()
    os.system("set")

    sys.exit(1)

    #print(listpkgs("libm3"))
    #print(listpkgs("m3-libs/libm3"))
    #print(listpkgs(ROOT + "/m3-libs/libm3"))

    PrintList2(["a"])
    print("PrintList2------------------------------")
    PrintList2(["a", "b"])
    print("PrintList2------------------------------")
    PrintList2(["a", "b", "c"])
    print("PrintList2------------------------------")
    PrintList2(["a", "b", "c", "d"])
    print("PrintList2------------------------------")
    PrintList2(["a", "b", "c", "d", "e"])
    print("PrintList2------------------------------")

    PrintList4(["a"])
    print("PrintList4------------------------------")
    PrintList4(["a", "b"])
    print("PrintList4------------------------------")
    PrintList4(["a", "b", "c"])
    print("PrintList4------------------------------")
    PrintList4(["a", "b", "c", "d"])
    print("PrintList4------------------------------")
    PrintList4(["a", "b", "c", "d", "e"])
    print("PrintList4------------------------------")

    CommandLines = [
        [],
        ["build"],
        ["buildlocal"],
        ["buildglobal"],
        ["buildship"],
        ["ship"],
        ["clean"],
        ["cleanlocal"],
        ["cleanglobal"],
        ["realclean"],
        ["-foo", "build"],
        #["unknown"],
        ["clean", "-bar"],
        ["-a", "-b", "-c"],

        #["m3core", "libm3"],
        ["build", "m3core", "libm3"],
        ["buildlocal", "m3core", "libm3"],
        ["buildglobal", "m3core", "libm3"],
        ["buildship", "m3core", "libm3"],
        ["ship", "m3core", "libm3"],
        ["clean", "m3core", "libm3"],
        ["cleanlocal", "m3core", "libm3"],
        ["cleanglobal", "m3core", "libm3"],
        ["realclean", "m3core", "libm3"],
        ["-foo", "build", "m3core", "libm3"],
        #["unknown", "m3core", "libm3"],
        ["clean", "-bar", "m3core", "libm3"],
        #["-a", "-b", "-c", "m3core", "libm3"],
        ]

    Functions = [
        map_action,
        add_action_opts,
        extract_options,
        get_args,
        ]

    Width = 0
    for CommandLine in CommandLines:
        Length = 0
        for Arg in CommandLine:
            Length += 4
            Length += len(Arg)
        if (Length > Width):
            Width = Length

    for Function in Functions:
        for CommandLine in CommandLines:
            print("%s(%-*s): %s" % (Function.__name__, Width, CommandLine, Function(CommandLine)))

    pkgmap(["-c"])

    cygpath("a", "b")
    strip_exe("c")
