#! /usr/bin/env python
# $Id: pylib.py,v 1.33 2008-01-19 00:02:46 jkrell Exp $

import os
from os import getenv
from os import path
import os.path
import glob
import sys
import platform
import re
import tempfile
import shutil

#
# Several important variables are gotten from the environment or probed.
# The probing is usually 100% correct, and the environment variable names
# Are sometimes a bit generic, but the environment still wins.
#
# CM3_TARGET / TARGET
#    probed with $OS and uname
#
# CM3_OSTYPE / M3OSTYPE
#    follows from Target
#
# CM3ROOT / ROOT
#    the root of the source, computed from the path to this file
#
# CM3_INSTALL / INSTALLROOT
#    the root of the installation, computed from finding cm3 in $PATH
#

# print("loading pylib..")

env_OS = getenv("OS")
if env_OS == "Windows_NT":
    def uname():
        PROCESSOR_ARCHITECTURE = getenv("PROCESSOR_ARCHITECTURE")
        return (env_OS, "", PROCESSOR_ARCHITECTURE, "", PROCESSOR_ARCHITECTURE)
else:
    from os import uname

def ExeName(a):
    if os.name == "nt":
        a += ".exe"
    return a

#
# http://aspn.activestate.com/ASPN/Cookbook/Python/Recipe/52224
#
def SearchPath(name, paths = getenv("PATH")):
    #Given a search path, find file
    if (name.find("/") != -1) or (name.find("\\") != -1):
        if os.path.isfile(name):
            return name
    if paths == "":
        return None
    if os.name == "nt":
        # support for $PATHEXT might be nice
        if name.find(".") == -1:
            name += ".exe"
    for path in paths.split(os.path.pathsep):
        candidate = os.path.join(path, name)
        if os.path.isfile(candidate):
            return os.path.abspath(candidate)

#
# the root of the installation
#
InstallRoot = os.path.dirname(os.path.dirname(SearchPath("cm3") or ""))

#
# the root of the source tree
#
Root = os.path.dirname(os.path.dirname(os.path.dirname(os.path.abspath(__file__))))
Root = Root.replace("\\", os.path.sep).replace("/", os.path.sep).replace("\\", "\\\\")

BuildAll = getenv("CM3_ALL") or False

#
# User can override all these from environment, as in sh.
# The environment variable names are all UPPERCASE.
# Ideally this array gets emptied or at least reduced.
#
# THIS IS MOSTLY NOT INTERESTING AS THE DEFAULTS AND PROBING ARE GOOD.
#
Variables = [

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
    # True or False -- should we build m3gdb.
    #
    "M3GDB",

    #
    # A temporary "staging" location? For
    # building distributions?
    #
    "STAGE",

    "BuildArgs",
    "CleanArgs",
    "ShipArgs",

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

for a in DefaultsFromSh.keys():
    DefaultsFromSh[a] = eval(a)

#-----------------------------------------------------------------------------
# output functions

def header(a):
    print("")
    print( "----------------------------------------------------------------------------")
    print(a)
    print("----------------------------------------------------------------------------")
    print("")

UNameTuple = uname()
UNameCommand = os.popen("uname").read().lower()
UName = UNameTuple[0].lower()
UNameArchP = platform.processor().lower()
UNameArchM = UNameTuple[4].lower()
UNameRevision = UNameTuple[2].lower()

#-----------------------------------------------------------------------------
# set some defaults

def GetDefaultFromSh(Key):
    #
    # Only read the file if an environment variable is "missing" (they
    # usually all are, ok), and only read it once.
    #
    #print("WriteVariablesIntoEnvironment:3")
    Value = DefaultsFromSh.get(Key)
    if Value:
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
        if Match:
            MatchKey = Match.group(1)
            #
            # We are here because one of them wasn't found, but we should be
            # sure only to overwrite what we don't have.
            #
            if not DefaultsFromSh[MatchKey]:
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
        if Item[1] is None:
            MissingKey = Item[0]
            File = __file__
            sys.stderr.write("%(File)s: %(MissingKey)s not found in %(ShFilePath)s\n" % vars())

    if MissingKey:
        sys.exit(1)

    return DefaultsFromSh.get(Key)

CM3VERSION = getenv("CM3VERSION") or GetDefaultFromSh("CM3VERSION")
CM3VERSIONNUM = getenv("CM3VERSIONNUM") or GetDefaultFromSh("CM3VERSIONNUM")
CM3LASTCHANGED = getenv("CM3LASTCHANGED") or GetDefaultFromSh("CM3LASTCHANGED")

CM3_GCC_BACKEND = True
CM3_GDB = False

CM3 = getenv("CM3") or ExeName("cm3")
M3Build = getenv("M3BUILD") or ExeName("m3build")
M3Ship = getenv("M3SHIP") or "m3ship"
EXE = "" # executable extension, ".exe" or empty
Q = "'"

#-----------------------------------------------------------------------------
# abstraction functions

def strip_exe(a):
    os.system("strip " + a)

#-----------------------------------------------------------------------------
# evaluate uname information

#
# very important -- what operating system/processor architecture
# we are building for
#
Target = getenv("CM3_TARGET") or getenv("TARGET") or ""
OSType = getenv("CM3_OSTYPE") or getenv("M3OSTYPE") or ""

if (UName.startswith("windows")
        or Target.startswith("NT386")
        or UNameCommand.startswith("mingw32_nt-")
        or UNameCommand.startswith("cygwin_nt-")
    ):

    OSType = OSType or "WIN32"
    EXE = ".exe"
    Q = ""
    HAVE_SERIAL = True
    GMAKE = getenv("GMAKE") or "make"

    if (Target.startswith("NT386GNU")
        or UNameCommand.startswith("mingw32_nt-")
        or UNameCommand.startswith("cygwin_nt-")):
        Target = Target or "NT386GNU"
    else:
        Target = Target or "NT386"
        CM3_GCC_BACKEND = False

        def strip_exe(a):
            pass

elif UName.startswith("freebsd"):

    if UNameArchM == "i386":
        if UNameRevision.startswith("1"):
            Target = "FreeBSD"
        elif UNameRevision.startswith("2"):
            Target = "FreeBSD2"
        elif UNameRevision.startswith("3"):
            Target = "FreeBSD3"
        elif UNameRevision.startswith("4"):
            Target = "FreeBSD4"
        else:
            Target = "FreeBSD4"
    else:
        Target = "FBSD_ALPHA"

elif UName.startswith("darwin"):

    # detect the m3 platform (Darwin runs on ppc and ix86)
    if UNameArchP.startswith("powerpc"):
        Target = "PPC_DARWIN"
    elif re.match("i[3456]86", UNameArchP):
        Target = "I386_DARWIN"
    GMAKE = getenv("GMAKE") or "make"

elif UName.startswith("sunos"):

    Target = "SOLgnu"
    #Target = "SOLsun"

elif UName.startswith("linux"):

    GMAKE = getenv("GMAKE") or "make"
    GCWRAPFLAGS = "-Wl,--wrap,adjtime,--wrap,getdirentries,--wrap,readv,--wrap,utimes,--wrap,wait3"
    if UNameArchM == "ppc":
        Target = "PPC_LINUX"
    else:
        Target = "LINUXLIBC6"

elif UName.startswith("netbsd"):

    GMAKE = getenv("GMAKE") or "make"
    Target = "NetBSD2_i386" # only arch/version combination supported yet

else:

    # more need to be added here, I haven't got all the platform info ready
    pass

#-----------------------------------------------------------------------------
# define the exported values

M3GDB = (M3GDB or CM3_GDB)
OSType = (OSType or "POSIX")
GCC_BACKEND = (GCC_BACKEND or CM3_GCC_BACKEND)
PKGSDB = (getenv("PKGSDB") or os.path.join(os.path.dirname(os.path.abspath(__file__)), "PKGS"))
GMAKE = (GMAKE or "gmake")

os.environ["CM3_TARGET"] = Target
os.environ["CM3_ROOT"] = Root

#-----------------------------------------------------------------------------
# elego customizations
#
# comment these if they interfere with your environment

if not getenv("STAGE"):

    if ((OSType == "POSIX")
            and os.system("type domainname > /dev/null 2>/dev/null")
            and (os.popen("domainname 2>/dev/null").read().replace("\n", "") == "elegoohm")):

        STAGE = "/pub/lang/m3/cm3-dist"

    elif (OSType == "WIN32") and (getenv("HOSTNAME") == "FIR"):

        STAGE = "c:/tmp/cm3stage"

#-----------------------------------------------------------------------------

# define build and ship programs for Critical Mass Modula-3

DEFS = "-DROOT=%(Q)s%(Root)s%(Q)s"
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

if os.name == "nt":
    RealClean = RealClean or "if exist %(Target)s rmdir /q/s %(Target)s"
else:
    RealClean = RealClean or "rm -rf %(Target)s"

RealClean = (RealClean % vars())

# choose the compiler to use

if SearchPath(CM3):
    BuildLocal = CM3_BuildLocal
    CleanLocal = CM3_CleanLocal
    BuildGlobal = CM3_BuildGlobal
    CleanGlobal = CM3_CleanGlobal
    Ship = CM3_Ship
elif SearchPath(M3Build):
    BuildLocal = PM3_BuildLocal
    CleanLocal = PM3_CleanLocal
    BuildGlobal = PM3_BuildGlobal
    CleanGlobal = PM3_CleanGlobal
    Ship = CM3_Ship
else:
    if (not BuildLocal) or (not BuildGlobal) or (not Ship):
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
            if j == 0:
                if i != 0:
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
        if arg in ["-h", "-help", "--help", "-?"]:
            print("")
            print("usage " + os.path.basename(args[0]) + ":")
            if Usage:
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
                if P:
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
    if not os.path.isfile(PKGSDB):
        #
        # Look for all files src/m3makefile in the CM3 source
        # and write their relative paths from Root to PKGSDB.
        #
        def Callback(Result, Directory, Names):
            if os.path.basename(Directory) != "src":
                return
            if Directory.find("_darcs") != -1:
                return
            if not "m3makefile" in Names:
                return
            if not os.path.isfile(os.path.join(Directory, "m3makefile")):
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

        if not os.path.isfile(PKGSDB):
            File = __file__
            sys.stderr.write("%(File)s: cannot generate package list\n" % vars())
            sys.exit(1)

PackageDB = None

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
        if i.endswith(b):
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
            if pkg.startswith(Root + '/'):
                pkg = pkg[len(Root) + 1:]
                #print("1 " + pkg)
            # if no slashes, then need a leading slash
            if pkg.find('/') == -1:
                pkg = ('/' + pkg)
                #print("2 " + pkg)
            for q in PackageDB:
                q = q.replace('\\', '/')
                if q.find(pkg) != -1:
                    #print("3 " + q)
                    Result.append(q)
                    break
    else:
        Result = PackageDB
    return map(lambda(a): (Root + '/' + a), Result)

def _Run(NoAction, Actions, PackageDirectory):

    print(" +++ %s +++" % Actions)

    if NoAction:
        return 0

    PreviousDirectory = os.getcwd()
    os.chdir(PackageDirectory.replace('/', os.path.sep))

    for a in Actions:
        Result = os.system(a)
        if Result != 0:
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

        "m3objfile": BuildAll or OSType == "WIN32",
        "mklib": BuildAll or OSType == "WIN32",
        "fix_nl": BuildAll or OSType == "WIN32",
        "libdump": BuildAll or OSType == "WIN32",
        "import-libs": BuildAll or OSType == "WIN32",

        "tcl": BuildAll or HAVE_TCL,
        "udp": BuildAll or OSType == "POSIX",
        "tapi": BuildAll or OSType == "WIN32",
        "serial": BuildAll or HAVE_SERIAL,

        "X11R4": BuildAll or OSType != "WIN32",
        "showthread": BuildAll or OSType != "WIN32",
        "pkl-fonts": BuildAll or OSType != "WIN32",
        "juno-machine": BuildAll or OSType != "WIN32",
        "juno-compiler": BuildAll or OSType != "WIN32",
        "juno-app": BuildAll or OSType != "WIN32",

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

    if not PackagesFromCaller is None:
        PackagesFromCaller = FilterPackages(PackagesFromCaller)
        PackagesFromCaller = OrderPackages(PackagesFromCaller)
        if not PackagesFromCaller:
            print("no packages left")
            return True

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

    if (not PackagesFromCaller) and (not args[1:]):
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
        if arg.startswith("-"):
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
        if os.path.isdir(q):
            PackageDirectories.append(q)
            continue

        q = os.path.join(Root, p)
        if os.path.isdir(q):
            PackageDirectories.append(q)
            continue

        q = GetPackagePath(p)
        if not q:
            File = __file__
            sys.stderr.write("%(File)s *** cannot find package %(p)s\n" % vars())
            sys.exit(1)

        if os.path.isdir(q):
            PackageDirectories.append(q)
            continue

        q = os.path.join(Root, q)
        if os.path.isdir(q):
            PackageDirectories.append(q)
            continue

        File = __file__
        sys.stderr.write("%(File)s *** cannot find package %(p)s / %(q)s\n" % vars())
        sys.exit(1)

    if not ActionCommands:
        File = __file__
        sys.stderr.write("%(File)s: no action defined, aborting\n" % vars())
        sys.exit(1)
        #return False

    if not PackageDirectories:
        File = __file__
        sys.stderr.write("%(File)s: no packages\n" % vars())
        sys.exit(1)
        #return False

    if ListOnly:
        ListPackage(PackageDirectories)
        sys.exit(0)
        #return True

    Success = True

    for p in PackageDirectories:
        print("== package %(p)s ==" % vars())
        ExitCode = _Run(NoAction, ActionCommands, p)
        if ExitCode != 0:
            Success = False
            if not KeepGoing:
                print(" *** execution of %s failed ***" % (str(ActionCommands)))
                sys.exit(1)
        if KeepGoing:
            print(" ==> %s returned %s" % (str(ActionCommands), ExitCode))
        else:
            print(" ==> %(p)s done" % vars())

    return Success

def DeleteFile(a):
    if os.path.isfile(a):
        os.remove(a)

def CreateDirectory(a):
    if not os.path.isdir(a):
        os.makedirs(a)
    return True

def MakeTempDir():
    if getenv("TEMP") and not os.path.exists(getenv("TEMP")):
        CreateDirectory(getenv("TEMP"))

MakeTempDir()

def CopyFile(From, To):
    if os.path.isdir(To):
        To = os.path.join(To, os.path.basename(From))
    if os.path.isfile(To):
        os.remove(To)
    CopyCommand = "copy"
    if os.name != "nt":
        CopyCommand = "cp -Pv"
    print(CopyCommand + " " + From + " " + To)
    shutil.copy(From, To)
    return True

def CopyFileIfExist(From, To):
    if os.path.isfile(From):
        return CopyFile(From, To)
    return True

def GetConfigForDistribution():
#
# Favor the config-no-install directory, else fallback to config.
#
    a = os.path.join(Root, "m3-sys", "cminstall", "src")
    b = os.path.join(a, "config-no-install", Target)
    if os.path.isfile(b):
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
        os.path.join(InstallRoot, "bin"))

def CopyMklib(From, To):
    #
    # Copy mklib from one InstallRoot to another, possibly having cleaned out the intermediate directories.
    #
    From = os.path.join(From, "bin")
    To = os.path.join(To, "bin")
    CreateDirectory(To)
    if Target == "NT386":
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

def _FormatEnvironmentVariable(Name):
    if os.name == "nt":
        return "%" + Name + "%"
    else:
        return "$" + Name

def _SetupEnvironmentVariableAll(Name, RequiredFiles, Attempt):
    AnyMissing = False
    Value = os.environ.get(Name)
    if Value:
        for File in RequiredFiles:
            if not SearchPath(File, Value):
                AnyMissing = True
                break
    else:
        AnyMissing = True
    if AnyMissing:
        if Value:
            NewValue = Attempt + os.path.pathsep + Value
        else:
            NewValue = Attempt
        for File in RequiredFiles:
            if not SearchPath(File, NewValue):
                print("ERROR: " + File + " not found in " + _FormatEnvironmentVariable(Name) + "(" + NewValue + ")")
                sys.exit(1)
        os.environ[Name] = NewValue
        if Value:
            print(Name + "=" + Attempt + os.pathsep + _FormatEnvironmentVariable(Name))
        else:
            print(Name + "=" + Attempt)


def _SetupEnvironmentVariableAny(Name, RequiredFiles, Attempt):
    Value = os.environ.get(Name)
    if Value:
        for File in RequiredFiles:
            if SearchPath(File, Value):
                return
    if Value:
        NewValue = Attempt + os.path.pathsep + Value
    else:
        NewValue = Attempt
    for File in RequiredFiles:
        if SearchPath(File, NewValue):
            os.environ[Name] = NewValue
            if Value:
                print(Name + "=" + NewValue + os.pathsep + _FormatEnvironmentVariable(Name))
            else:
                print(Name + "=" + NewValue)
            return
    print("ERROR: " + _FormatEnvironmentVariable(Name) + " does not have any of " + RequiredFiles)
    sys.exit(1)

def _ClearEnvironmentVariable(Name):
    if Name in os.environ:
        del(os.environ[Name])
        print("set " + Name + "=")


#
# Need to figure out how to do this properly, if at all.
#
#
# Pretty specific to my setup.
# We could also honor MSDevDir, MSVCDir, VCToolkitInstallDir, VCINSTALLDIR, etc.
# see scripts\win\sysinfo.cmd. Some of these are set always by those installers.
# (Though I delete them. :) )
#
def SetupEnvironment():
    SystemDrive = os.environ.get("SystemDrive", "")
    if os.environ.get("OS") == "Windows_NT":
        NT = True
    else:
        NT = False

    SystemDrive = os.environ.get("SystemDrive")
    if SystemDrive:
        SystemDrive += os.path.sep

    if Target == "NT386" and NT:

        VCBin = ""
        VCInc = ""
        VCLib = ""
        MspdbDir = ""

        # 4.0 e:\MSDEV
        # 5.0 E:\Program Files\DevStudio\SharedIDE
        MSDevDir = os.environ.get("MSDevDir")

        # 5.0
        MSVCDir = os.environ.get("MSVCDir") # E:\Program Files\DevStudio\VC

        # 7.1 Express
        VCToolkitInstallDir = os.environ.get("VCToolkitInstallDir") # E:\Program Files\Microsoft Visual C++ Toolkit 2003 (not set by vcvars32)

        # 8.0 Express
        # E:\Program Files\Microsoft Visual Studio 8\VC
        # E:\Program Files\Microsoft Visual Studio 8\Common7\Tools
        DevEnvDir = os.environ.get("DevEnvDir") # E:\Program Files\Microsoft Visual Studio 8\Common7\IDE
        VSInstallDir = os.environ.get("VSINSTALLDIR") # E:\Program Files\Microsoft Visual Studio 8
        # VS80CommonTools = os.environ.get("VS80COMNTOOLS") # E:\Program Files\Microsoft Visual Studio 8\Common7\Tools
        VCInstallDir = os.environ.get("VCINSTALLDIR") # E:\Program Files\Microsoft Visual Studio 8\VC

        #
        # untested prototype..
        #
        if VSInstallDir:
            if not VCInstallDir:
                VCInstallDir = os.path.join(VSInstallDir, "VC")
            if not DevEnvDir:
                DevEnvDir = os.path.join(VSInstallDir, "Common7", "IDE")

            MspdbDir = DevEnvDir

        elif VCToolkitInstallDir:
            pass # do more research

        elif MSVCDir:
            pass # do more research

        elif MSDevDir:
            pass # do more research

        else:
            #
            # what really happens on my machine for 8.0
            # need to guide pylib.py to other versions.
            # (setting the environment ahead of time should make it leave it alone)
            #
            Msdev = os.path.join(SystemDrive, "msdev", "80")
            VCInstallDir = os.path.join(Msdev, "VC")
            DevEnvDir = os.path.join(Msdev, "Common7", "IDE")

        if VCInstallDir:
            VCBin = os.path.join(VCInstallDir, "bin")
            VCLib = os.path.join(VCInstallDir, "lib")
            VCInc = os.path.join(VCInstallDir, "include")

        if DevEnvDir:
            MspdbDir = DevEnvDir

        _SetupEnvironmentVariableAll("INCLUDE", ["errno.h"], VCInc)

        _SetupEnvironmentVariableAll(
            "LIB",
            ["kernel32.lib", "libcmt.lib"],
            VCLib + os.path.pathsep + os.path.join(InstallRoot, "lib"))

        _SetupEnvironmentVariableAny(
            "PATH",
            ["mspdb80.dll", "mspdb71.dll", "mspdb70.dll", "mspdb60.dll", "mspdb50.dll", "mspdb40.dll" ],
            MspdbDir)

        _SetupEnvironmentVariableAll("PATH", ["cl", "link"], VCBin)

    elif Target == "NT386GNU":

        _ClearEnvironmentVariable("LIB")
        _ClearEnvironmentVariable("INCLUDE")

        _SetupEnvironmentVariableAll(
            "PATH",
            ["gcc", "as", "ld"],
            os.path.join(SystemDrive, "mingw", "bin"))


        # ensure msys make is ahead of mingwin make

        _SetupEnvironmentVariableAll(
            "PATH",
            ["sh", "sed", "gawk", "make"],
            os.path.join(SystemDrive, "msys", "1.0", "bin"))

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
        if Length > Width:
            Width = Length

    for Function in Functions:
        for CommandLine in CommandLines:
            print("%s(%-*s): %s" % (Function.__name__, Width, CommandLine, Function(CommandLine)))

    pkgmap(["-c"])

    strip_exe("c")
