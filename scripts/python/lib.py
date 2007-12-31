# $Id: lib.py,v 1.13 2007-12-31 10:11:16 jkrell Exp $

import os
from os import getcwd, chdir, getenv
import os.path
from os.path import isfile, isdir, join, abspath, dirname, pathsep
import glob
import sys
import copy
import platform

env_OS = getenv("OS")
if env_OS == "Windows_NT":
    def uname():
        PROCESSOR_ARCHITECTURE = getenv("PROCESSOR_ARCHITECTURE")
        return (env_OS, "", PROCESSOR_ARCHITECTURE, "", PROCESSOR_ARCHITECTURE)
else:
    from os import uname

# Should these be initialized from environment?
PKGS = [ ]
PKG_ACTION = ""
LIST_ONLY = False
NO_ACTION = False
KEEP_GOING = False

PackageDB = None

#
# User can override all these from environment, as in sh.
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

    #
    # link flags particular to Linux
    #
    "GCWRAPFLAGS",

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
    # This is easily computed from __file__.
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

    "TMPDIR",

    "BUILDARGS",
    "CLEANARGS",
    "SHIPARGS",

    "M3SHIP",
    "M3BUILD",

    "BUILDLOCAL",
    "CLEANLOCAL",
    "BUILDGLOBAL",
    "CLEANGLOBAL",
    "SHIP",

    "CM3_BUILDLOCAL",
    "CM3_CLEANLOCAL",
    "CM3_BUILDGLOBAL",
    "CM3_CLEANGLOBAL",
    "CM3_SHIP",

    "PM3_BUILDLOCAL",
    "PM3_CLEANLOCAL",
    "PM3_BUILDGLOBAL",
    "PM3_CLEANGLOBAL",
    "PM3_SHIP",

    "SRC_BUILDLOCAL",
    "SRC_CLEANLOCAL",
    "SRC_BUILDGLOBAL",
    "SRC_CLEANGLOBAL",
    "SRC_SHIP",

    "REALCLEAN",
    "IGNORE_MISS",
    "HAVE_TCL",
    "HAVE_SERIAL",
    "OMIT_GCC",
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
    b += ("%s = getenv(\"%s\") or \"\"\n" % (a, a))
exec(b)

for a in DefaultsFromSh.keys():
    DefaultsFromSh[a] = eval(a)

#def WriteVariablesIntoEnvironment():
#    #print("2: CM3VERSION is " + CM3VERSION)
#    for a in Variables:
#        #print("1: %s = %s" % (a, eval(a, locals(), globals())))
#        os.putenv(a, eval(a))

#CM3_DEBUG = 1
CM3_DEBUG = 0

#-----------------------------------------------------------------------------
# output functions

def debug(a):
    if (getenv("CM3_DEBUG") or CM3_DEBUG):
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
UNAME = uname_tuple[0]
UNAME_P = platform.processor()
UNAME_M = uname_tuple[4]
UNAME_R = uname_tuple[2]

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
    import re
    #
    # CM3VERSION=${CM3VERSION:-"d5.5.1"}
    # CM3VERSIONNUM=${CM3VERSIONNUM:-"050501"}
    # CM3LASTCHANGED=${CM3LASTCHANGED:-"2007-12-30"}
    #
    RegExp = re.compile("(" + "|".join(DefaultsFromSh.keys()) + ")=\\$\\{\\1:-\"([^\"]+)\"\\}$")
    ShFilePath = join(dirname(dirname(abspath(__file__))), "sysinfo.sh")
    for Line in open(join(dirname(dirname(abspath(__file__))), "sysinfo.sh")):
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

    #print("WriteVariablesIntoEnvironment:2")

    # not needed, whatever is needed is passed on the cm3 command line
    #WriteVariablesIntoEnvironment()

    return DefaultsFromSh.get(Key)

CM3VERSION = getenv("CM3VERSION") or GetDefaultFromSh("CM3VERSION")
#print("3: CM3VERSION is " + CM3VERSION)
CM3VERSIONNUM = getenv("CM3VERSIONNUM") or GetDefaultFromSh("CM3VERSIONNUM")
CM3LASTCHANGED = getenv("CM3LASTCHANGED") or GetDefaultFromSh("CM3LASTCHANGED")

CM3_GCC_BACKEND = True
CM3_GDB = False

debug("CM3VERSION")

#
# if CM3_INSTALL is not set, and cm3 is in $PATH, cm3's directory's directory is CM3_INSTALL,
# else CM3_DEFAULTS defaults to /usr/local/cm3
# Unfortunately, which always prints something and always succeeds, so we have to sniff
# out its error message -- given "which foo", it prints "no foo in ..", where .. is
# space delimited elements of $PATH -- at least on Mac OSX 10.4.
#
def ExeName(a):
    debug("os.name")
    if (os.name == "nt"):
        a += ".exe"
    return a

#
# http://aspn.activestate.com/ASPN/Cookbook/Python/Recipe/52224
#
def SearchPath(name, paths = getenv("PATH")):
    #Given a search path, find file
    if (os.name == "nt"):
        # support for $PATHEXT might be nice
        if (name.find(".") == -1):
            name += ".exe"
    for path in paths.split(pathsep):
        candidate = join(path, name)
        # print("candidate is " + candidate)
        if (isfile(candidate)):
            return abspath(candidate)

CM3_INSTALL = (
    getenv("CM3_INSTALL")
    or dirname(dirname(SearchPath("cm3") or ""))
    or "/usr/local/cm3"
    )

debug("CM3_INSTALL")

CM3 = CM3 or ExeName("cm3")
M3BUILD = M3BUILD or ExeName("m3build")
M3SHIP = M3SHIP or "m3ship"
EXE = "" # executable extension, ".exe" or empty
SL = "/" # path slash, forward or backward
Q = "'"

SYSLIBDIR = join(CM3_INSTALL, "lib")
SYSLIBS = ""
XDEV_LIB = " "
XDEV_BIN = ""
TAR = "tar"

for TMPDIR in [
        TMPDIR,
        getenv("TMP"),
        getenv("TEMP"),
        "/var/tmp",
        "/usr/tmp",
        "/tmp",
        "c:/tmp",
        "d:/tmp",
        "e:/tmp",
        "c:/temp",
        "d:/temp",
        "e:/temp",
        ]:
    if (TMPDIR and isdir(TMPDIR)):
        break

if (not TMPDIR):
    File = __file__
    sys.stderr.write("%(File)s please define TMPDIR\n" % vars())
    sys.exit(1)

debug("TMPDIR")

GCWRAPFLAGS = ""

#-----------------------------------------------------------------------------
# some localization functions

def find_file(file, dirs):
    for dir in dirs.split(" "):
        if (isdir(dir)):
            a = join(dir, file)
            if (isfile(a)):
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
# evaluate uname information

#
# This is all a bit wonky.
# Data derives from both uname and optional overrides in environment variables.
# If use sets TARGET environment variable, then user is obligated to
# to get other things correct such as M3OSTYPE
#

TARGET = getenv("TARGET") or ""

if (UNAME.startswith("Windows")
        or UNAME.startswith("WinNT")
        or UNAME.startswith("Cygwin")
        or UNAME.startswith("CYGWIN")
        or TARGET.startswith("NT386")
    ):

    if (TARGET.startswith("NT386GNU")):

        CM3_OSTYPE = "POSIX"
        CM3_TARGET = "NT386GNU"
        GMAKE = getenv("GMAKE") or "make"

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
        # consider using arrays here
        # Or just remove all this stuff and rely on $LIB
        SYSLIBS =  "ADVAPI32.LIB GDI32.LIB KERNEL32.LIB ODBC32.LIB"
        SYSLIBS += " OPENGL32.LIB WSOCK32.LIB COMDLG32.LIB"
        SYSLIBS += " GLU32.LIB NETAPI32.LIB ODBCCP32.LIB USER32.LIB"
        L =  "c:/cm3/bin d:/cm3/bin e:/cm3/bin c:/reactor5/bin d:/reactor5/bin"
        L += " e:/reactor5/bin c:/reactor/bin d:/reactor/bin"
        L += " e:/reactor/bin /usr/local/cm3/bin /usr/local/reactor/bin"
        L += " /usr/cm3/bin /usr/reactor/bin"
        CM3BINSEARCHPATH = L
        f = find_file("KERNEL32.LIB", L)
        if (f):
            SYSLIBDIR = dirname(f)
        else:
            SYSLIBDIR = "unknown"

        D = "c:/msdev/bin d:/msdev/bin e:/msdev/bin f:/msdev/bin g:/msdev/bin"
        f = find_file("cl.exe", D)
        if (f):
            XDEV_BIN = dirname(f)
            XDEV_LIB = join(XDEV_BIN, "lib")
        else:
            XDEV_LIB = ""
            XDEV_BIN = ""

        f = "/usr/bin/tar.exe"
        if (isfile(f)):
            TAR = f
        GMAKE = getenv("GMAKE") or "make"

        def strip_exe(a):
            #print("strip_exe:pass")
            pass

elif (UNAME.startswith("FreeBSD")):

    CM3_OSTYPE = "POSIX"
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

elif (UNAME.startswith("Darwin")):

    CM3_OSTYPE = "POSIX"
    # detect the m3 platform (Darwin runs on ppc and ix86
    if (UNAME_P.startswith("powerpc")):
        CM3_TARGET = "PPC_DARWIN"
    elif (re.match("i[3456]86", UNAME_P)):
        CM3_TARGET = "I386_DARWIN"
    GMAKE = getenv("GMAKE") or "make"

elif (UNAME.startswith("SunOS")):

    CM3_OSTYPE = "POSIX"
    CM3_TARGET = "SOLgnu"
    #CM3_TARGET = "SOLsun"

elif (UNAME.startswith("Linux")):

    CM3_OSTYPE = "POSIX"
    GMAKE = getenv("GMAKE") or "make"
    GCWRAPFLAGS = "-Wl,--wrap,adjtime,--wrap,getdirentries,--wrap,readv,--wrap,utimes,--wrap,wait3"
    if (UNAME_M == "ppc"):
        CM3_TARGET = "PPC_LINUX"
    else:
        CM3_TARGET = "LINUXLIBC6"

elif (UNAME.startswith("NetBSD")):

    CM3_OSTYPE = "POSIX"
    GMAKE = getenv("GMAKE") or "make"
    CM3_TARGET = "NetBSD2_i386" # only arch/version combination supported yet

else:

    # more need to be added here, I haven't got all the platform info ready
    pass

DEV_BIN = (getenv("DEV_BIN") or XDEV_BIN)
DEV_LIB = (getenv("DEV_LIB") or XDEV_LIB)

#-----------------------------------------------------------------------------
# define the exported values

#
# ROOT is two levels above this program.
#

ROOT = (ROOT or dirname(dirname(dirname(abspath(__file__)))))

INSTALLROOT = (INSTALLROOT or CM3_INSTALL)

M3GDB = (M3GDB or CM3_GDB)
M3OSTYPE = (M3OSTYPE or CM3_OSTYPE)
TARGET = (TARGET or CM3_TARGET)
GCC_BACKEND = (GCC_BACKEND or CM3_GCC_BACKEND)
PKGSDB = (PKGSDB or join(dirname(abspath(__file__)), "PKGS"))
GMAKE = (GMAKE or "gmake")

if (M3OSTYPE == "WIN32"):
    # quoteing madness.. is it correct?
    CM3ROOT = cygpath("-w", ROOT).replace("\\", "\\\\")
else:
    CM3ROOT = ROOT

#-----------------------------------------------------------------------------
# elego customizations
#
# comment these if they interfere with your environment

if (not getenv("STAGE")):

    if ((M3OSTYPE == "POSIX")
            and os.system("type domainname > /dev/null 2>/dev/null")
            and (popen("domainname 2>/dev/null").read.replace("\n", "") == "elegoohm")):

        STAGE = "/pub/lang/m3/cm3-dist"

    elif (M3OSTYPE == "WIN32" and (getenv("HOSTNAME") == "FIR")):

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
debug("TMPDIR")
debug("EXE")
debug("SL")
debug("SYSLIBDIR")
debug("SYSLIBS")
debug("DEV_BIN")
debug("DEV_LIB")
debug("TAR")
debug("CM3ROOT")

# define build and ship programs for Critical Mass Modula-3

DEFS = "-DROOT=%(Q)s%(CM3ROOT)s%(Q)s" % vars()
DEFS += " -DCM3_VERSION_TEXT=%(Q)s%(CM3VERSION)s%(Q)s" % vars()
DEFS += " -DCM3_VERSION_NUMBER=%(Q)s%(CM3VERSIONNUM)s" % vars()
DEFS += " -DCM3_LAST_CHANGED=%(Q)s%(CM3LASTCHANGED)s%(Q)s" % vars()

CM3_BUILDLOCAL = BUILDLOCAL or "%(CM3)s -build -override %(DEFS)s %(BUILDARGS)s" % vars()
CM3_CLEANLOCAL = CLEANLOCAL or "%(CM3)s -clean -build -override %(DEFS)s %(CLEANARGS)s" % vars()
CM3_BUILDGLOBAL = BUILDGLOBAL or "%(CM3)s -build %(DEFS)s %(BUILDARGS)s" % vars()
CM3_CLEANGLOBAL = CLEANGLOBAL or "%(CM3)s -clean %(DEFS)s %(CLEANARGS)s" % vars()
CM3_SHIP = SHIP or "%(CM3)s -ship %(DEFS)s %(CLEANARGS)s" % vars()

# define build and ship programs for Poly. Modula-3 from Montreal

PM3_BUILDLOCAL = BUILDLOCAL or "%(M3BUILD)s -O %(DEFS)s %(BUILDARGS)s" % vars()
PM3_CLEANLOCAL = CLEANLOCAL or "%(M3BUILD)s clean -O %(DEFS)s %(CLEANARGS)s" % vars()
PM3_BUILDGLOBAL = BUILDGLOBAL or "%(M3BUILD)s %(DEFS)s %(BUILDARGS)s)s" % vars()
PM3_CLEANGLOBAL = CLEANGLOBAL or "%(M3BUILD)s clean %(DEFS)s %(CLEANARGS)s" % vars()
PM3_SHIP = SHIP or "%(M3SHIP)s %(DEFS)s %(SHIPARGS)s" % vars()

# define build and ship programs for DEC SRC Modula-3

SRC_BUILDLOCAL = BUILDLOCAL or "%(M3BUILD)s -O %(DEFS)s %(BUILDARGS)s" % vars()
SRC_CLEANLOCAL = CLEANLOCAL or "%(M3BUILD)s clean -O %(DEFS)s %(CLEANARGS)s" % vars()
SRC_BUILDGLOBAL = BUILDGLOBAL or "%(M3BUILD)s %(DEFS)s %(BUILDARGS)s" % vars()
SRC_CLEANGLOBAL = CLEANGLOBAL or "%(M3BUILD)s clean %(DEFS)s %(CLEANARGS)s" % vars()
SRC_SHIP = SHIP or "%(M3SHIP)s %(DEFS)s %(SHIPARGS)s" % vars()

# other commands

if (os.name == "nt"):
    REALCLEAN = REALCLEAN or "if exist %(TARGET)s rmdir /q/s %(TARGET)s" % vars()
else:
    REALCLEAN = REALCLEAN or "rm -rf %(TARGET)s" % vars()

# choose the compiler to use

if (SearchPath(CM3)):
    BUILDLOCAL = CM3_BUILDLOCAL
    CLEANLOCAL = CM3_CLEANLOCAL
    BUILDGLOBAL = CM3_BUILDGLOBAL
    CLEANGLOBAL = CM3_CLEANGLOBAL
    SHIP = CM3_SHIP
elif (SearchPath(M3BUILD)):
    BUILDLOCAL = PM3_BUILDLOCAL
    CLEANLOCAL = PM3_CLEANLOCAL
    BUILDGLOBAL = PM3_BUILDGLOBAL
    CLEANGLOBAL = PM3_CLEANGLOBAL
    SHIP = CM3_SHIP
else:
    if (not BUILDLOCAL or not BUILDGLOBAL or not SHIP):
        File = __file__
        sys.stderr.write("%(File)s: %(CM3)s or %(M3BUILD)s not found in your path, don't know how to compile\n" % vars())
        sys.exit(1)

BUILDLOCAL = BUILDLOCAL.strip()
CLEANLOCAL = CLEANLOCAL.strip()
BUILDGLOBAL = BUILDGLOBAL.strip()
CLEANGLOBAL = CLEANGLOBAL.strip()
SHIP = SHIP.strip()

def map_action(args):
    arg = "build"
    for a in args:
        if (not a.startswith("-")):
            arg = a
            break
    ACTION = {
        "build": BUILDLOCAL,
        "buildlocal": BUILDLOCAL,
        "buildglobal": BUILDGLOBAL + " && " + SHIP,
        "buildship": BUILDGLOBAL + " && " + SHIP,
        "ship": SHIP,
        "clean": CLEANLOCAL,
        "cleanlocal": CLEANLOCAL,
        "cleanglobal": CLEANGLOBAL,
        "realclean": REALCLEAN,
    }.get(arg)
    if (not ACTION):
        if (IGNORE_MISS):
            ACTION = BUILDLOCAL
        else:
            File = __file__
            sys.stderr.write("%(File)s: unknown action %(arg)s\n" % vars())
            sys.exit(1)
    return ACTION

def add_action_opts(args):
    arg = "build"
    for a in args:
        if (not a.startswith("-")):
            arg = a
            break
    ARGS = {
        "clean": "-k",
        "cleanlocal": "-k",
        "cleanglobal": "-k",
        "realclean": "-k",
    }.get(arg, "")
    return ARGS

def extract_options(args):
    RES = ""
    for a in args:
        if (a.startswith("-")):
            if (RES):
                RES += " "
            RES += a
    return RES

def get_args(args):
    ARGS = [ ]
    i = 0
    j = len(args)
    while (i != j):
        if (not args[i].startswith("-")):
            break
        i += 1
    if ((i != j)
            and (args[i] in {
                "build": None,
                "buildlocal": None,
                "buildglobal": None,
                "buildship": None,
                "ship": None,
                "clean": None,
                "cleanlocal": None,
                "cleanglobal": None,
                "realclean": None,
            })):
        i += 1
    while (i != j):
        arg = args[i]
        if (arg.startswith("-")):
            File = __file__
            sys.stderr.write("%(File)s: encountered option after command: %(arg)s (%(i)s)\n" % vars())
        else:
            ARGS += [arg]
        i += 1
    return ARGS

def format_one(width, string):
    return ("%-*s" % (width, string))

def print_list2(strings):
    Newline = ""
    Result = ""
    if (len(strings) % 2 != 0):
        strings = copy.copy(strings) # unfortunate expense
        strings.append("")
    width = 36 # roughly 80 / 2
    for i in range(0, len(strings) / 2):
        Result += (
            Newline
            + "  "
            + format_one(width, strings[i * 2])
            + format_one(width, strings[i * 2 + 1])
            )
        Newline = "\n"
    return Result

def print_list4(strings):
    Newline = ""
    Result = ""
    width = 18 # roughly 80 / 4
    if (len(strings) % 4 != 0):
        strings = copy.copy(strings) # unfortunate expense
        while (len(strings) % 4 != 0):
            strings.append("")
    for i in range(0, len(strings) / 4):
        Result += (
            Newline
            + "  "
            + format_one(width, strings[i * 4])
            + format_one(width, strings[i * 4 + 1])
            + format_one(width, strings[i * 4 + 2])
            + format_one(width, strings[i * 4 + 3])
            )
        Newline = "\n"
    return Result

def show_usage(args, USAGE, P):
    for arg in args[1:]:
        if (arg in ["-h", "-help", "--help", "-?"]):
            print("")
            print("usage " + os.path.split(args[0])[1] + ":")
            if (USAGE):
                basename = os.path.basename(args[0])
                GEN_CMDS = """
  build | buildlocal          build a package with local overrides (default)
  buildglobal | buildship     build a package without overrides and ship it
  ship                        ship a package
  clean | cleanlocal          clean a package with local overrides
  cleanglobal                 clean a package without overrides
  realclean                   remove the TARGET directory of a package
"""

                GEN_OPTS = """
  -n                          no action (do not execute anything)
  -k                          keep going (ignore errors if possible)
"""
                if (P):
                    N = len(P)
                    P = print_list4(P)
                print(USAGE % vars())
            else:
                print("")
                print("No specific usage notes available.")
                print("")
                print("Generic commands:")
                print(GEN_CMDS)
                print("Generic options:")
                print(GEN_OPTS)
            sys.exit(0)

def MakePackageDB():
    if (not isfile(PKGSDB)):
        #
        # Look for all files src/m3makefile in the CM3 source
        # and write their relative paths from ROOT to PKGSDB.
        #
        def Callback(Result, Directory, Names):
            if (os.path.split(Directory)[1] != "src"):
                return
            if (Directory.find("_darcs") != -1):
                return
            if not "m3makefile" in Names:
                return
            if (not os.path.isfile(os.path.join(Directory, "m3makefile"))):
                return
            Result.append(Directory[len(ROOT) + 1:-4] + "\n")

        print("making " + PKGSDB + "..")
        Result = [ ]

        os.path.walk(
            ROOT,
            Callback,
            Result
            )

        Result.sort()
        open(PKGSDB, "w").writelines(Result)

        if (not isfile(PKGSDB)):
            File = __file__
            sys.stderr.write("%(File)s: cannot generate package list\n" % vars())
            sys.exit(1)

def ReadPackageDB():
    MakePackageDB()
    global PackageDB
    PackageDB = (PackageDB or
            map(
                lambda(a): a.replace("\n", ""),
                open(PKGSDB).readlines(),
                ))

def pkg_defined(a):
    ReadPackageDB()
    a = (SL + a)
    for i in PackageDB:
        if i.endswith(a):
            return True

def pkgpath(a):
    ReadPackageDB()
    b = (SL + a)
    for i in PackageDB:
        if (i.endswith(b)):
            #print("pkgpath(%(a)s returning %(i)s (%(b)s)" % vars())
            return i
    File = __file__
    sys.stderr.write("%(File)s: package %(a)s not found (%(b)s)\n" % vars())

def listpkgs(pkgs):
    ReadPackageDB()
    Result = [ ]
    if pkgs:
        for pkg in pkgs.split(" "):
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

def exec_cmd(PKG):
    # lame temporary
    global PKG_ACTION, NO_ACTION
    print(" +++ %s +++" % PKG_ACTION)
    if (NO_ACTION):
        return 0

    PreviousDirectory = getcwd()

    chdir(PKG)
    Result = os.system(PKG_ACTION)

    chdir(PreviousDirectory)

    return Result

def pkgmap(args):
    # Which of these should be primed from the environment?
    global PKGS, PKG_ACTION, LIST_ONLY, NO_ACTION, KEEP_GOING, ACTION
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
                    File = __file__
                    sys.stderr.write("%(File)s: missing parameter to -c\n" % vars())
                    sys.exit(1)
                if (PKG_ACTION):
                    PKG_ACTION += " ; "
                PKG_ACTION += args[i]
                break
            p = os.path.join(ROOT, arg)

            #print("ROOT is " + ROOT)
            #print("p is " + p)

            if (isdir(p)):
                #print("1 %(p)s" % vars())
                PKGS.append(p)
                break
            if (isdir(arg)):
                #print("2 %(arg)s" % vars())
                PKGS.append(arg)
                break
            #print("arg is " + arg)

            #print("p is " + p)

            p = pkgpath(arg)
            if (not p):
                File = __file__
                sys.stderr.write("%(File)s *** cannot find package %(arg)s\n" % vars())
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
            File = __file__
            sys.stderr.write("%(File)s *** cannot find package %(arg)s / %(p)s\n" % vars())
            sys.exit(1)
        i += 1
    if (not PKG_ACTION):
        File = __file__
        sys.stderr.write("%(File)s: no PKG_ACTION defined, aborting\n" % vars())
        sys.exit(1)
    if (not PKGS):
        File = __file__
        sys.stderr.write("%(File)s: no packages\n" % vars())
        sys.exit(1)
    if (LIST_ONLY):
        listpkgs(PKGS)
        sys.exit(0)

    for PKG in PKGS:
        print("== package %(PKG)s ==" % vars())
        res = exec_cmd(PKG)
        if (res != 0):
            if (not KEEP_GOING):
                v = vars()
                v.update(globals())
                print(" *** execution of %(ACTION)s failed ***" % v)
                sys.exit(1)
        if (KEEP_GOING):
            print(" ==> %s returned %s" % (PKG_ACTION, res))
        else:
            print(" ==> %(PKG)s done" % vars())

def do_pkg(args, P = None):

    if (P):
        USAGE = \
"""
%(basename)s [ generic_options ] [ generic_cmd ]

  will apply the given symbolic command to the following %(N)s packages:

%(P)s

generic_options:
%(GEN_OPTS)s

generic_cmd:
%(GEN_CMDS)s"""
    else:
        USAGE = \
"""
%(basename)s [ generic_options ] [ generic_cmd ] pkg+

will apply the given symbolic command to one or more CM3 packages.

generic_options:
%(GEN_OPTS)s

generic_cmd:
%(GEN_CMDS)s"""

    show_usage(
        args,
        USAGE,
        P,
        )

    OPTIONS = extract_options(args[1:])
    global IGNORE_MISS, ACTION
    if (not P):
        IGNORE_MISS = True
    ACTION = map_action(args[1:])
    ADDARGS = add_action_opts(args[1:])
    if (not P):
        P = get_args(args[1:])

    v = vars()
    v.update(globals())
    a = ("pkgmap %(OPTIONS)s %(ADDARGS)s -c \"%(ACTION)s\" %(P)s" % v)
    a = a.replace("  ", " ")
    a = a.replace("  ", " ")
    print(a)

    pkgmap([OPTIONS, ADDARGS, "-c", ACTION] + P)

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

    print_list2(["a"])
    print("print_list2------------------------------")
    print_list2(["a", "b"])
    print("print_list2------------------------------")
    print_list2(["a", "b", "c"])
    print("print_list2------------------------------")
    print_list2(["a", "b", "c", "d"])
    print("print_list2------------------------------")
    print_list2(["a", "b", "c", "d", "e"])
    print("print_list2------------------------------")

    print_list4(["a"])
    print("print_list4------------------------------")
    print_list4(["a", "b"])
    print("print_list4------------------------------")
    print_list4(["a", "b", "c"])
    print("print_list4------------------------------")
    print_list4(["a", "b", "c", "d"])
    print("print_list4------------------------------")
    print_list4(["a", "b", "c", "d", "e"])
    print("print_list4------------------------------")

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
