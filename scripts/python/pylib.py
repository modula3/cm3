#! /usr/bin/env python
# $Id$

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
    DefaultInstall = "c:/cm3"
else:
    from os import uname
    DefaultInstall = "/usr/local/cm3"

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
    # the root of the source code (Root), but with
    # backward slashes doubled,
    # e.g. c:\\dev2\\cm3 or /dev2/cm3
    # The same as Root on Unix.
    # This is easily computed from __file__.
    #
    "CM3Root",

    "CM3VERSION",

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
    "InstallRoot",

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
    "Root",

    #
    # A temporary "staging" location? For
    # building distributions?
    #
    "STAGE",

    #
    # very important -- what operating system/processor architecture
    # we are building for
    #
    "Target",

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

# print("pylib: InstallRoot is "  + InstallRoot)
# print("pylib: env_InstallRoot is " + (os.environ.get("InstallRoot") or ""))

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
    # CM3VERSIONNUM=${CM3VersionNum:-"050501"}
    # CM3LASTCHANGED=${CM3LASTCHANGED:-"2007-12-30"}
    #
    RegExp = re.compile("(" + "|".join(DefaultsFromSh.keys()) + ")=\\$\\{\\1:-\"([^\"]+)\"\\}$", re.IGNORECASE)
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
    or DefaultInstall # c:/cm3 or /usr/local/cm3
    )

debug("CM3_INSTALL")

CM3 = CM3 or ExeName("cm3")
M3Build = M3Build or ExeName("m3build")
M3Ship = M3Ship or "m3ship"
EXE = "" # executable extension, ".exe" or empty
Q = "'"

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

def strip_exe(a):
    os.system("strip " + a)

#-----------------------------------------------------------------------------
# evaluate uname information

#
# This is all a bit wonky.
# Data derives from both uname and optional overrides in environment variables.
# If use sets TARGET environment variable, then user is obligated to
# to get other things correct such as M3OSTYPE
#

Target = os.getenv("CM3_TARGET") or os.getenv("TARGET") or ""
M3OSTYPE = os.getenv("CM3_OSTYPE") or os.getenv("M3OSTYPE") or ""

if (UNAME.startswith("windows")
        or UNAME.startswith("winnt")
        or UNAME.startswith("cygwin")
        or Target.startswith("NT386")
    ):

    M3OSTYPE = M3OSTYPE or "WIN32"
    EXE = ".exe"
    Q = ""
    HAVE_SERIAL = True
    GMAKE = os.getenv("GMAKE") or "make"

    if Target.startswith("NT386GNU"):
        Target = Target or "NT386GNU"
    else:
        Target = Target or "NT386"
        CM3_GCC_BACKEND = False

        def strip_exe(a):
            pass

elif (UNAME.startswith("freebsd")):

    if (UNAME_M == "i386"):
        if (UNAME_R.startswith("1")):
            Target = "FreeBSD"
        elif (UNAME_R.startswith("2")):
            Target = "FreeBSD2"
        elif (UNAME_R.startswith("3")):
            Target = "FreeBSD3"
        elif (UNAME_R.startswith("4")):
            Target = "FreeBSD4"
        else:
            Target = "FreeBSD4"
    else:
        Target = "FBSD_ALPHA"

elif (UNAME.startswith("darwin")):

    # detect the m3 platform (Darwin runs on ppc and ix86)
    if (UNAME_P.startswith("powerpc")):
        Target = "PPC_DARWIN"
    elif (re.match("i[3456]86", UNAME_P)):
        Target = "I386_DARWIN"
    GMAKE = os.getenv("GMAKE") or "make"

elif (UNAME.startswith("sunos")):

    Target = "SOLgnu"
    #Target = "SOLsun"

elif (UNAME.startswith("linux")):

    GMAKE = os.getenv("GMAKE") or "make"
    GCWRAPFLAGS = "-Wl,--wrap,adjtime,--wrap,getdirentries,--wrap,readv,--wrap,utimes,--wrap,wait3"
    if (UNAME_M == "ppc"):
        Target = "PPC_LINUX"
    else:
        Target = "LINUXLIBC6"

elif (UNAME.startswith("netbsd")):

    GMAKE = os.getenv("GMAKE") or "make"
    Target = "NetBSD2_i386" # only arch/version combination supported yet

else:

    # more need to be added here, I haven't got all the platform info ready
    pass

#-----------------------------------------------------------------------------
# define the exported values

#
# Root is two levels above this program.
#

Root = (Root or os.path.dirname(os.path.dirname(os.path.dirname(os.path.abspath(__file__)))))

InstallRoot = (InstallRoot or CM3_INSTALL)

M3GDB = (M3GDB or CM3_GDB)
M3OSTYPE = (M3OSTYPE or "POSIX")
GCC_BACKEND = (GCC_BACKEND or CM3_GCC_BACKEND)
PKGSDB = (PKGSDB or os.path.join(os.path.dirname(os.path.abspath(__file__)), "PKGS"))
GMAKE = (GMAKE or "gmake")

CM3Root = Root.replace("\\", os.path.sep)
CM3Root = Root.replace("/", os.path.sep)

#-----------------------------------------------------------------------------
# elego customizations
#
# comment these if they interfere with your environment

if (not os.getenv("STAGE")):

    if ((M3OSTYPE == "POSIX")
            and os.system("type domainname > /dev/null 2>/dev/null")
            and (os.popen("domainname 2>/dev/null").read().replace("\n", "") == "elegoohm")):

        STAGE = "/pub/lang/m3/cm3-dist"

    elif (M3OSTYPE == "WIN32" and (os.getenv("HOSTNAME") == "FIR")):

        STAGE = "c:/tmp/cm3stage"

#-----------------------------------------------------------------------------
# debug output

debug("Root")
debug("M3GDB")
debug("M3OSTYPE")
debug("Target")
debug("GCC_BACKEND")
debug("InstallRoot")
debug("PKGSDB")
debug("GMAKE")
debug("EXE")
debug("CM3Root")

# define build and ship programs for Critical Mass Modula-3

DEFS = "-DROOT=%(Q)s%(CM3Root)s%(Q)s"
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
    RealClean = RealClean or "if exist %(Target)s rmdir /q/s %(Target)s"
else:
    RealClean = RealClean or "rm -rf %(Target)s"

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
        # and write their relative paths from Root to PKGSDB.
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
            Result.append(Directory[len(Root) + 1:-4].replace('\\', "/") + "\n")

        print("making " + PKGSDB + ".. (slow but rare)")
        Result = [ ]

        os.path.walk(
            Root,
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
                lambda(a): a.replace("\n", "").replace('\\', '/'),
                open(PKGSDB)
                ))

def IsPackageDefined(a):
    a = a.replace('\\', '/')
    ReadPackageDB()
    a = ('/' + a)
    for i in PackageDB:
        if i.endswith(a):
            return True

def GetPackagePath(a):
    a = a.replace('\\', '/')
    ReadPackageDB()
    b = ('/' + a)
    for i in PackageDB:
        if (i.endswith(b)):
            return i.replace('/', os.path.sep)
    File = __file__
    sys.stderr.write("%(File)s: package %(a)s not found (%(b)s)\n" % vars())

def ListPackages(pkgs):
    ReadPackageDB()
    Result = [ ]
    if pkgs:
        for pkg in pkgs:
            pkg = pkg.replace('\\', '/')
            # remove Root from the start
            if (pkg.startswith(Root + '/')):
                pkg = pkg[len(Root) + 1:]
                #print("1 " + pkg)
            # if no slashes, then need a leading slash
            if (pkg.find('/') == -1):
                pkg = ('/' + pkg)
                #print("2 " + pkg)
            for q in PackageDB:
                q = q.replace('\\', '/')
                if (q.find(pkg) != -1):
                    #print("3 " + q)
                    Result.append(q)
                    break
    else:
        Result = PackageDB
    return map(lambda(a): (Root + '/' + a), Result)

def _Run(NoAction, Actions, PackageDirectory):

    print(" +++ %s +++" % Actions)

    if (NoAction):
        return 0

    PreviousDirectory = os.getcwd()
    os.chdir(PackageDirectory.replace('/', os.path.sep))

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
            }.get(Target, False),

        "m3objfile": CM3_ALL or M3OSTYPE == "WIN32",
        "mklib": CM3_ALL or M3OSTYPE == "WIN32",
        "fix_nl": CM3_ALL or M3OSTYPE == "WIN32",
        "libdump": CM3_ALL or M3OSTYPE == "WIN32",
        "import-libs": CM3_ALL or M3OSTYPE == "WIN32",

        "tcl": CM3_ALL or HAVE_TCL,
        "udp": CM3_ALL or M3OSTYPE == "POSIX",
        "tapi": CM3_ALL or M3OSTYPE == "WIN32",
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
#
# These lists are deliberately in a jumbled order
# in order to depend on OrderPackages working.
#
# This needs to be still further data driven,
# and a full ordering is not necessarily, only
# a partial ordering -- stuff can be build in parallel.
#
    "min" :
        [
        "import-libs",
        "libm3",
        "m3core",
        ],

    "std" : # This is in the proper order
            # and is used to order core and base.
        [
    # demo programs

        "cube",
        "calculator",
        "fisheye",
        "mentor",

    # base libraries

        "import-libs",
        "libm3",
        "patternmatching",
        "m3core",

    # system / compiler libraries and tools

        "m3quake",
        "m3middle",
        "m3scanner",
        "m3tools",
        "m3cgcat",
        "m3cggen",

        "m3gdb",
        "m3bundle",
        "mklib",
        "fix_nl",
        "libdump",

    # more useful quasi-standard libraries

        "arithmetic",
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
        "tcp",
        "udp",
        "libsio",
        "libbuf",
        "debug",
        "listfuncs",
        "embutils",
        "m3tk-misc",
        "http",
        "binIO",
        # "deepcopy",
        # "sgml",
        "commandrw",

        # some CM3 communication extensions

        "tapi",
        "serial",

        # tools

        "m3tk",
        "mtex",
        "m3totex",
        "m3tohtml",
        "m3scan",
        "m3markup",
        "m3browser",
        "cmpdir",
        "cmpfp",
        "dirfp",
        "uniq",
        # "pp" # needs lex and yacc or flex and bison
        # "kate"   # can be shipped only on systems with KDE
        # "nedit",

        # network objects -- distributed programming

        "netobj",
        "netobjd",
        "stubgen",
        "events",
        "rdwr",
        "sharedobj",
        "sharedobjgen",

        # database packages

        "odbc",
        "postgres95",
        "db",
        "smalldb",
        "stable",
        "stablegen",

        # the standard graphical user interface: trestle and formsvbt

        "X11R4",
        "ui",
        "PEX",
        "vbtkit",
        "cmvbt",
        "jvideo",
        "videovbt",
        "web",
        "formsvbtpixmaps",
        "formsvbt",
        "formsview",
        "formsedit",
        "codeview",
        "mg",
        "mgkit",
        "opengl",
        "anim3D",
        "zeus",
        "m3zume",

        # obliq
        "synloc",
        "synex",
        "metasyn",
        "obliqrt",
        "obliqparse",
        "obliqprint",
        "obliq",
        "obliqlibemb",
        "obliqlibm3",
        "obliqlibui",
        "obliqlibanim",
        # "obliqlib3D" # does not compile
        "obliqsrvstd",
        "obliqsrvui",
        "obliqbinmin",
        "obliqbinstd",
        "obliqbinui",
        "obliqbinanim",
        "visualobliq",
        "vocgi",
        "voquery",
        "vorun",

        # more graphics depending on obliq

        "webvbt",

        # more tools

        "recordheap",
        "rehearsecode",
        "replayheap",
        "showheap",
        "shownew",
        "showthread",
        # showthread needs ThreadEvent, which does not exist on win32

        # The Juno-2 graphical constraint based editor

        "pkl-fonts",
        "juno-machine",
        "juno-compiler",
        "juno-app",
        ],


    "all": # in order
        [
    # base libraries

        "import-libs",
        "m3core",
        "libm3",
        "patternmatching",

    # system / compiler libraries and tools

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
        "fix_nl",
        "libdump",

    # more useful quasi-standard libraries

        "arithmetic",
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
        "tcp",
        "udp",
        "libsio",
        "libbuf",
        "debug",
        "listfuncs",
        "embutils",
        "m3tk-misc",
        "http",
        "binIO",
        # "deepcopy",
        # "sgml",
        "commandrw",

        # some CM3 communication extensions

        "tapi",
        "serial",

        # tools

        "m3tk",
        "mtex",
        "m3totex",
        "m3tohtml",
        "m3scan",
        "m3markup",
        "m3browser",
        "cmpdir",
        "cmpfp",
        "dirfp",
        "uniq",
        # "pp" # needs lex and yacc or flex and bison
        # "kate"   # can be shipped only on systems with KDE
        # "nedit",

        # network objects -- distributed programming

        "netobj",
        "netobjd",
        "stubgen",
        "events",
        "rdwr",
        "sharedobj",
        "sharedobjgen",

        # database packages

        "odbc",
        "postgres95",
        "db",
        "smalldb",
        "stable",
        "stablegen",

        # the standard graphical user interface: trestle and formsvbt

        "X11R4",
        "ui",
        "PEX",
        "vbtkit",
        "cmvbt",
        "jvideo",
        "videovbt",
        "web",
        "formsvbtpixmaps",
        "formsvbt",
        "formsview",
        "formsedit",
        "codeview",
        "mg",
        "mgkit",
        "opengl",
        "anim3D",
        "zeus",
        "m3zume",

        # obliq
        "synloc",
        "synex",
        "metasyn",
        "obliqrt",
        "obliqparse",
        "obliqprint",
        "obliq",
        "obliqlibemb",
        "obliqlibm3",
        "obliqlibui",
        "obliqlibanim",
        # "obliqlib3D" # does not compile
        "obliqsrvstd",
        "obliqsrvui",
        "obliqbinmin",
        "obliqbinstd",
        "obliqbinui",
        "obliqbinanim",
        "visualobliq",
        "vocgi",
        "voquery",
        "vorun",

        # more graphics depending on obliq

        "webvbt",

        # more tools

        "recordheap",
        "rehearsecode",
        "replayheap",
        "showheap",
        "shownew",
        "showthread",
        # showthread needs ThreadEvent, which does not exist on win32

        # The Juno-2 graphical constraint based editor

        "pkl-fonts",
        "juno-machine",
        "juno-compiler",
        "juno-app",

        # demo programs

        "cube",
        "calculator",
        "fisheye",
        "mentor",
        ],
}

PackageSets_CoreBaseCommon = [
    "import-libs",
    "m3core",
    "libm3",
    "m3middle",
    "m3quake",
    "m3scanner",
    "m3tools",
    "m3cgcat",
    "m3cggen",
    "m3gdb",
    "m3bundle",
    "mklib",
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

PackageSets["core"] = PackageSets_CoreBaseCommon
PackageSets["base"] = PackageSets_CoreBaseCommon

PackageSets["core"] += [
    "patternmatching",
    "m3objfile",
    "m3linker",
    "m3back",
    "m3staloneback",
    "m3cc",
    "cm3",
    "m3front",
	"m3gdb",
    ]

PackageSets["base"] += [
    "tcp",
    "tapi",
    "serial",
    ]

def OrderPackages(Packages):
    AllPackagesInOrder = PackageSets["all"]
    AllPackagesHashed = dict.fromkeys(AllPackagesInOrder)
    PackagesInOrder =  [ ]
    PackagesHashed = dict.fromkeys(Packages)

    #
    # Make sure everything is in AllPackagesInOrder.
    #
    for Package in Packages:
        if not Package in AllPackagesHashed:
            print("ERROR: " + Package + " is not in PackageSets[\"all\"]")
            sys.exit(1)

    for Package in AllPackagesInOrder:
        if Package in PackagesHashed:
            PackagesInOrder += [Package]
            del PackagesHashed[Package]

    return PackagesInOrder

def DoPackage(args, PackagesFromCaller = None):

    # print("args is " + str(args))
    # sys.stdout.flush()

    if PackagesFromCaller:
        PackagesFromCaller = FilterPackages(PackagesFromCaller)
        PackagesFromCaller = OrderPackages(PackagesFromCaller)

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

        q = os.path.join(Root, p)
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

        q = os.path.join(Root, q)
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

def DeleteFile(a):
    if os.path.isfile(a):
        os.remove(a)

def CreateDirectory(a):
    if (not os.path.isdir(a)):
        os.makedirs(a)
    return True

def MakeTempDir():
    if os.getenv("TEMP") and not os.path.exists(os.getenv("TEMP")):
        CreateDirectory(os.getenv("TEMP"))

MakeTempDir()

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

def GetConfigForDistribution():
#
# Favor the config-no-install directory, else fallback to config.
#
    a = os.path.join(Root, "m3-sys", "cminstall", "src")
    b = os.path.join(a, "config-no-install", Target)
    if (os.path.isfile(b)):
        return b
    b = os.path.join(a, "config", Target)
    return b

def GetConfigForDevelopment():
    return os.path.join(Root, "m3-sys", "cminstall", "src", "config", "cm3.cfg")

def CopyConfigForDevelopment():
    CopyFile(GetConfigForDevelopment(), os.path.join(InstallRoot, "bin", "cm3.cfg")) or FatalError()
    return True

def CopyConfigForDistribution(To):
    CopyFile(GetConfigForDistribution(), os.path.join(To, "bin", "cm3.cfg")) or FatalError()
    return True

def _CopyCompiler(From, To):
    CreateDirectory(To)
    CopyFile(os.path.join(From, "cm3" + EXE), To) or FatalError()
    CopyFileIfExist(os.path.join(From, "cm3cg" + EXE), To) or FatalError()
    CopyFileIfExist(os.path.join(From, "cm3.pdb"), To) or FatalError()
    return True

def ShipCompiler():
    #
    # The compiler has trouble shipping itself currently because it in use.
    #
    return _CopyCompiler(
        os.path.join(Root, "m3-sys", "cm3", Target),
        os.path.join(InstallRoot, "bin"));

def CopyMklib(From, To):
    #
    # Copy mklib from one InstallRoot to another, possibly having cleaned out the intermediate directories.
    #
    From = os.path.join(From, "bin")
    To = os.path.join(To, "bin")
    CreateDirectory(To)
    if (Target == "NT386"):
        CopyFile(os.path.join(From, "mklib" + EXE), To) or FatalError()
    else:
        CopyFileIfExist(os.path.join(From, "mklib" + EXE), To) or FatalError()
    CopyFileIfExist(os.path.join(From, "mklib.pdb"), To) or FatalError()
    return True

def CopyCompiler(From, To):
    #
    # Copy the compiler from one InstallRoot to another, possibly having cleaned out the intermediate directories.
    # The config file always comes right out of the source tree.
    #
    _CopyCompiler(os.path.join(From, "bin"), os.path.join(To, "bin"))
    CopyMklib(From, To) or FatalError()
    return True

if __name__ == "__main__":
    #
    # run test code if module run directly
    #

    print("\n\ncore: " + str(OrderPackages(PackageSets["core"])))
    print("\n\nbase: " + str(OrderPackages(PackageSets["base"])))
    print("\n\nmin: " + str(OrderPackages(PackageSets["min"])))
    print("\n\nstd: " + str(OrderPackages(PackageSets["std"])))
    sys.exit(1)

    GetDefaultFromSh("CM3VERSION")
    #sys.stdout.flush()
    os.system("set")

    sys.exit(1)

    #print(listpkgs("libm3"))
    #print(listpkgs("m3-libs/libm3"))
    #print(listpkgs(Root + "/m3-libs/libm3"))

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

    strip_exe("c")
