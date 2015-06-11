#! /usr/bin/env python

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
import time

#-----------------------------------------------------------------------------
# Several important variables are gotten from the environment or probed.
# The probing is usually 100% correct.
#
# CM3_TARGET
#    probed with $OS and uname
#
# CM3_ROOT
#    the root of the source, computed from the path to this file
#
# CM3_INSTALL
#    the root of the installation, computed from finding cm3 in $PATH
#
# M3CONFIG
#    the config file, probed based on a few factors
#

# print("loading pylib..")

if os.environ.get("M3CONFIG", "").lower().find("m3-syscminstallsrcconfig") != -1:
    print("backslash problem; environment variable M3CONFIG is " + getenv("M3CONFIG"))
    sys.exit(1)

def IsInterix():
    return os.name == "posix" and os.uname()[0].lower().startswith("interix")

env_OS = getenv("OS")

DevNull = "/dev/null"

if env_OS == "Windows_NT" and not IsInterix():
    DevNull = "nul:"
    def uname():
        PROCESSOR_ARCHITECTURE = getenv("PROCESSOR_ARCHITECTURE")
        return (env_OS, "", PROCESSOR_ARCHITECTURE, "", PROCESSOR_ARCHITECTURE)
    #
    # cmd can run extensionless executables if this code is enabled.
    # This can be useful for example with I386_CYGWIN following more Posix-ish
    # naming styles than even Cygwin usually does.
    #
    #pathext = getenv("PATHEXT")
    #if pathext and not "." in pathext.split(";"):
    #    pathext = ".;" + pathext
    #    os.environ["PATHEXT"] = pathext
    #    print("set PATHEXT=.;%PATHEXT%")
else:
    from os import uname

#-----------------------------------------------------------------------------

_Program = os.path.basename(sys.argv[0])

#-----------------------------------------------------------------------------

def FatalError(a = ""):
    # logs don't work yet
    #print("ERROR: see " + Logs)
    print("fatal error " + a)
    if __name__ != "__main__":
        sys.exit(1)

def RemoveTrailingSlashes(a):
    while len(a) > 0 and (a[-1] == '\\' or a[-1] == '/'):
        a = a[:-1]
    return a

def RemoveTrailingSlash(a):
    if len(a) > 0 and (a[-1] == '\\' or a[-1] == '/'):
        a = a[:-1]
    return a

def StringContains(a, b):
    return a.find(b) != -1

def StringContainsI(a, b):
    return a.lower().find(b.lower()) != -1

def Tagged(a, b):
    return a.startswith(b + "_") or a.endswith("_" + b) or StringContains(a, "_" + b + "_") or a == b

#print("RemoveTrailingSlash(a\\/):" + RemoveTrailingSlash("a\\/"))
#print("RemoveTrailingSlash(a/\\):" + RemoveTrailingSlash("a/\\"))
#print("RemoveTrailingSlash(a):" + RemoveTrailingSlash("a"))
#print("RemoveTrailingSlashes(a\\\\):" + RemoveTrailingSlashes("a\\\\"))
#print("RemoveTrailingSlashes(a//):" + RemoveTrailingSlashes("a//"))
#print("RemoveTrailingSlashes(a):" + RemoveTrailingSlashes("a"))
#sys.exit(1)

def GetLastPathElement(a):
    a = RemoveTrailingSlashes(a)
    return a[max(a.rfind("/"), a.rfind("\\")) + 1:]

def RemoveLastPathElement(a):
    a = RemoveTrailingSlashes(a)
    return a[0:max(a.rfind("/"), a.rfind("\\"))]

def GetPathExtension(a):
    a = GetLastPathElement(a)
    b = a.rfind(".")
    if b < 0:
        return ""
    return a[b + 1:]

def RemovePathExtension(a):
    return a[:a.rfind(".")]

def _GetObjectName(a, obj):
    return GetPathBaseName(a) + "." + {"c" : obj, "s" : obj, "ms" : "mo", "is" : "io"}[GetPathExtension(a)]

def GetPathBaseName(a):
    a = GetLastPathElement(a)
    b = a.rfind(".")
    if b == -1:
        return a
    return a[0:b]

# print("1:" + GetPathExtension("a"))
# print("2:" + GetPathExtension("a.b"))
# print("3:" + GetPathExtension("a.b/c.d"))
# print("4:" + GetPathExtension("a.b/c"))
# sys.exit(1)
# print("1:" + GetPathBaseName("a"))
# print("2:" + GetPathBaseName("a.b"))
# print("3:" + GetPathBaseName("a.b/c.d"))
# print("4:" + GetPathBaseName("a.b/c"))
# sys.exit(1)


#-----------------------------------------------------------------------------

def ConvertToCygwinPath(a):
    if IsInterix() or env_OS != "Windows_NT" or a == None:
        return a
    if (a.find('\\') == -1) and (a.find(':') == -1):
        return a
    a = a.replace("\\", "/")
    if a.find(":/") == 1:
        a = "/cygdrive/" + a[0:1] + a[2:]
    return a

#-----------------------------------------------------------------------------

def ConvertFromCygwinPath(a):
    if IsInterix() or env_OS != "Windows_NT" or a == None:
        return a
    a = a.replace("/", "\\")
    #a = a.replace("\\", "/")
    if a.startswith("\\cygdrive\\"):
        a = a[10] + ":" + a[11:]
    elif a.startswith("\\home\\elego\\"):
        a = "c:\\cygwin\\" + a
    return a

def GetFullPath(a):
    # find what separator it as (might be ambiguous)
    if a.find("/") != -1:
        sep = "/"
    elif a.find("\\") != -1:
        sep = "\\"
    # convert to what Python expects, both due to ambiguity
    a = a.replace("/", os.path.sep)
    a = a.replace("\\", os.path.sep)
    a = os.path.abspath(a)          # have Python do the work
    a = a.replace(os.path.sep, sep) # put back the original separators
    return a

def ConvertPathForWin32(a):
    return ConvertFromCygwinPath(a)

if os.name == "posix":
    def ConvertPathForPython(a):
        return ConvertToCygwinPath(a)
else:
    def ConvertPathForPython(a):
        return ConvertFromCygwinPath(a)

#-----------------------------------------------------------------------------

def isfile(a):
    return os.path.isfile(ConvertPathForPython(a))

def isdir(a):
    return os.path.isdir(ConvertPathForPython(a))

def FileExists(a):
    return isfile(a)

def DirectoryExists(a):
    return isdir(a)

#-----------------------------------------------------------------------------
#
# http://aspn.activestate.com/ASPN/Cookbook/Python/Recipe/52224
#

def SearchPath(name, paths = getenv("PATH")):
    #Given a search path, find file
    if (name.find("/") != -1) or (name.find("\\") != -1):
        if isfile(name):
            return name
    if paths == "":
        print("SearchPath returning None 1")
        return None
    (base, exts) = os.path.splitext(name)
    if not exts and not IsInterix():
        exts = (getenv("PATHEXT") or "").lower()
    for ext in exts.split(";"):
        if ext == ".":
            ext = ""
        name = (base + ext)
        seps = [os.pathsep]
        # use ; for portable separator where possible
        if os.pathsep != ';':
            seps.append(';')
        for sep in seps:
            for path in paths.split(sep):
                candidate = os.path.join(path, name)
                if isfile(candidate):
                    return os.path.abspath(candidate)
    #print("SearchPath " + name + " returning None 2")
    return None

#-----------------------------------------------------------------------------

def _FormatEnvironmentVariable(Name):
    if os.name == "nt":
        return "%" + Name + "%"
    else:
        return "$" + Name

#-----------------------------------------------------------------------------

def _CheckSetupEnvironmentVariableAll(Name, RequiredFiles, Attempt, pathsep = os.pathsep):
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
            NewValue = Attempt + pathsep + Value
        else:
            NewValue = Attempt
        for File in RequiredFiles:
            if not SearchPath(File, NewValue):
                return "ERROR: " + File + " not found in " + _FormatEnvironmentVariable(Name) + "(" + NewValue + ")"
        os.environ[Name] = NewValue
        if Value:
            print(Name + "=" + Attempt + pathsep + _FormatEnvironmentVariable(Name))
        else:
            print(Name + "=" + Attempt)
    return None

def _SetupEnvironmentVariableAll(Name, RequiredFiles, Attempt, pathsep = os.pathsep):
    Error = _CheckSetupEnvironmentVariableAll(Name, RequiredFiles, Attempt, pathsep)
    if Error:
        print(Error)
        if __name__ != "__main__":
            sys.exit(1)

#-----------------------------------------------------------------------------

def _SetupEnvironmentVariableAny(Name, RequiredFiles, Attempt, pathsep = os.pathsep):
    Value = os.environ.get(Name)
    if Value:
        for File in RequiredFiles:
            if SearchPath(File, Value):
                return
    if Value:
        NewValue = Attempt + pathsep + Value
    else:
        NewValue = Attempt
    for File in RequiredFiles:
        if SearchPath(File, NewValue):
            os.environ[Name] = NewValue
            if Value:
                print(Name + "=" + NewValue + pathsep + _FormatEnvironmentVariable(Name))
            else:
                print(Name + "=" + NewValue)
            return
    print("ERROR: " + _FormatEnvironmentVariable(Name) + " does not have any of " + " ".join(RequiredFiles))
    if __name__ != "__main__":
        sys.exit(1)

#-----------------------------------------------------------------------------

def _ClearEnvironmentVariable(Name):
    if Name in os.environ:
        del(os.environ[Name])
        if os.name == "posix":
            print("export " + Name + "=")
        else:
            print("set " + Name + "=")

#-----------------------------------------------------------------------------

def _MapTarget(a):

    # don't map -- can't then cleanup the old

    return a

    # Convert sensible names that the user might provide on the
    # command line into the legacy names other code knows about.

    return {
        "LINUXLIBC6"    : "I386_LINUX",
        "NT386"         : "I386_NT",
        "PPC32_DARWIN"  : "PPC_DARWIN",
        "PPC32_LINUX"   : "PPC_LINUX",
        "FreeBSD4"      : "I386_FREEBSD",

        # both options sensible, double HP a bit redundant in the HPUX names

        "HPPA32_HPUX"  : "PA32_HPUX",
        "HPPA64_HPUX"  : "PA64_HPUX",
        "HPPA32_LINUX" : "PA32_LINUX",
        "HPPA64_LINUX" : "PA64_LINUX",
    }.get(a) or a

#-----------------------------------------------------------------------------

def _GetAllTargets():

    # legacy naming

    Targets = { }
    for target in [ "NT386", "LINUXLIBC6", "SOLsun", "SOLgnu", "FreeBSD4" ]:
        Targets[target] = target
        Targets[target.lower()] = target
        Targets[target.upper()] = target

    # systematic naming

    for proc in ["ALPHA", "ALPHA32", "ALPHA64", "AMD64", "ARM", "ARMEL", "IA64", "I386", "PPC", "PPC32",
                 "PPC64", "SPARC", "SPARC32", "SPARC64", "MIPS32", "MIPS64EL", "MIPS64", "PA32", "PA64", "SH"]:
        for os in ["AIX",  "CE", "CYGWIN", "DARWIN",  "FREEBSD", "HPUX", "INTERIX", "IRIX",
                   "LINUX", "MINGW", "NETBSD", "NT", "OPENBSD", "OSF", "SOLARIS", "VMS"]:
                   # "BEOS", "MSDOS" (DJGPP), "OS2" (EMX), "PLAN9"
            target = proc + "_" + os
            Targets[target] = target
            Targets[target.lower()] = target
            Targets[target.upper()] = target

    return Targets

#-----------------------------------------------------------------------------

_CBackend = "c" in sys.argv
_BuildDirC = ["", "c"][_CBackend]
_PossibleCm3Flags = ["boot", "keep", "override", "commands", "verbose", "why"]
_SkipGccFlags = ["nogcc", "skipgcc", "omitgcc"]
_PossiblePylibFlags = ["noclean", "nocleangcc", "c"] + _SkipGccFlags + _PossibleCm3Flags

CM3_FLAGS = ""
for a in _PossibleCm3Flags:
    if a in sys.argv:
        CM3_FLAGS = CM3_FLAGS + " -" + a

CM3 = ConvertPathForPython(getenv("CM3")) or "cm3"
CM3 = SearchPath(CM3)

#-----------------------------------------------------------------------------
# the root of the installation
# This can be sniffed by finding cm3 in $PATH, else defaulted
# if the defaults contain a cm3.
#

InstallRoot = ConvertPathForPython(getenv("CM3_INSTALL"))
# print("InstallRoot is " + InstallRoot)

if not CM3 and not InstallRoot:
    for a in ["c:\\cm3\\bin\\cm3.exe", "d:\\cm3\\bin\\cm3.exe", "/cm3/bin/cm3", "/usr/local/bin/cm3"]:
        if isfile(a):
            CM3 = a
            bin = os.path.dirname(CM3)
            print("using " + CM3)
            InstallRoot = os.path.dirname(bin)
            _SetupEnvironmentVariableAll("PATH", ["cm3"], bin)
            break

if not InstallRoot:
    if CM3:
        InstallRoot = os.path.dirname(os.path.dirname(CM3))
    else:
        if "realclean" in sys.argv:
            #
            # Realclean does not require knowing CM3_INSTALL or
            # being able to run cm3, so just set dummy values.
            #
            CM3 = __file__
            InstallRoot = __file__
        else:
            FatalError("environment variable CM3_INSTALL not set AND cm3 not found in PATH; please fix")

#-----------------------------------------------------------------------------
# the root of the source tree
# This is always correctly and simply found based on the location of this very code.
#
Root = os.path.dirname(os.path.dirname(os.path.dirname(os.path.abspath(__file__))))

#-----------------------------------------------------------------------------
# If the current working drive is the same drive as Root is in, then the Win32
# path can take on a valid working Posix form -- no colon, all forward slashes.
# Plenty of Win32 code accepts forward slashes.
#
# If the current working drive and the drive of Root are different, then this
# is flawed. However, we can and probably should completely fix that by
# merely cd'ing to a directory on the drive of Root, if other paths either
# are also on that drive, or have their drive letter not removed.
#

# This is all well and good for current cm3, but older Win32 cm3 doesn't like
# these paths (even though underlying Win32 does). So try leaving things alone.

# if Root.find(":\\") == 1:
#   Root = Root[2:]
#
# Root = Root.replace("\\\\", "/")
# Root = Root.replace("\\", "/")

#-----------------------------------------------------------------------------

#
# User can override all these from environment, as in sh.
# The environment variable names are all UPPERCASE.
# Ideally this array gets emptied or at least reduced.
#
# THIS IS MOSTLY NOT INTERESTING AS THE DEFAULTS AND PROBING ARE GOOD.
#
Variables = [

    #
    # True or False -- should we build m3gdb.
    #
    "M3GDB",

    #
    # a temporary "staging" location for building distributions
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

    "RealClean",
    "HAVE_TCL",
    "HAVE_SERIAL",
    "OMIT_GCC",
]

#-----------------------------------------------------------------------------

Versions = { "CM3VERSION" : None,
             "CM3VERSIONNUM" : None,
             "CM3LASTCHANGED" : None }

Variables += Versions.keys()

#
# Ensure all variables have some value.
#
b = ""
for a in Variables:
    b += ("%s = os.getenv(\"%s\") or \"\"\n" % (a, a.upper()))
exec(b)

for a in Versions.keys():
    Versions[a] = eval(a)

#-----------------------------------------------------------------------------

def header(a):
    print("")
    print( "----------------------------------------------------------------------------")
    print(a)
    print("----------------------------------------------------------------------------")
    print("")

#-----------------------------------------------------------------------------
# set some defaults

def GetVersion(Key):
    #
    # Only read the file if an environment variable is "missing" (they
    # usually all are, ok), and only read it once.
    #
    #print("WriteVariablesIntoEnvironment:3")
    Value = Versions.get(Key)
    if Value:
        return Value
    #
    # CM3VERSION d5.7.1
    # CM3VERSIONNUM 050701
    # CM3LASTCHANGED 2009-01-21
    #
    RegExp = re.compile("(" + "|".join(Versions.keys()) + ") (.+)$", re.IGNORECASE)
    ShFilePath = os.path.join(os.path.dirname(os.path.dirname(os.path.abspath(__file__))), "version")
    for Line in open(ShFilePath):
        Match = RegExp.match(Line)
        if Match:
            MatchKey = Match.group(1)
            #
            # We are here because one of them wasn't found, but we should be
            # sure only to overwrite what we don't have.
            #
            if not Versions[MatchKey]:
                Value = Match.group(2)
                Versions[MatchKey] = Value
                exec("%s = \"%s\"" % (MatchKey, Value), locals(), globals())
    #
    # Make sure we found every key in the file (at least those
    # not defined in the environment)
    #
    MissingKey = None
    for Item in Versions.iteritems():
        #print(Item)
        if Item[1] is None:
            MissingKey = Item[0]
            File = __file__
            sys.stderr.write("%(File)s: %(MissingKey)s not found in %(ShFilePath)s\n" % vars())

    if MissingKey:
        sys.exit(1)

    return Versions.get(Key)

CM3VERSION = getenv("CM3VERSION") or GetVersion("CM3VERSION").replace("pre-", "pre")
CM3VERSIONNUM = getenv("CM3VERSIONNUM") or GetVersion("CM3VERSIONNUM")
CM3LASTCHANGED = getenv("CM3LASTCHANGED") or GetVersion("CM3LASTCHANGED")

CM3_GDB = False


#-----------------------------------------------------------------------------
#
# some dumb detail as to where quotes are needed on command lines
#

Q = "'"
Q = "" # TBD

#-----------------------------------------------------------------------------
#
# GCC_BACKEND is almost always true.
#

GCC_BACKEND = not _CBackend

#-----------------------------------------------------------------------------
#
# Sniff to determine host, which is the default target.
#

Host = None
for a in os.popen(CM3 + " -version 2>" + DevNull):
  if StringContains(a, "host:" ):
    Host = a.replace("\r", "").replace("\n", "").replace(" ", "").replace("host:", "")
    break

#-----------------------------------------------------------------------------
#
# Target is:
#   - any parameter on the command line that is a platform
#   - CM3_TARGET environment variable
#   - defaults to host
#

Target = None
_AllTargets = _GetAllTargets()
for a in sys.argv:
    Target = _MapTarget(_AllTargets.get(a.lower()))
    if Target:
        break
Target = Target or getenv("CM3_TARGET") or Host

#-----------------------------------------------------------------------------
#
# TargetOS is almost always POSIX, the user cannot set it, it is changed to WIN32 sometimes later
#

TargetOS = "POSIX"

#-----------------------------------------------------------------------------
#
# Usually Config == Target, except NT386 has multiple configurations.
#

Config = Target

#-----------------------------------------------------------------------------
#
# Set data that derives from target/config.
#

#
# Is this the right default?
#
HAVE_SERIAL = False

if Target.startswith("NT386"):

    if Target == "NT386":
        TargetOS = "WIN32"
        HAVE_SERIAL = True

    if Target == "NT386":
        GCC_BACKEND = False

    Config = Target

    # From the front ends point of view, NT386, NT386GNU, NT386MINGNU
    # can be the same thing -- little endian, 32bit, same jmpbuf size.
    # The "Config" -- how to compile/link, etc. is different.
    Target = "NT386"

if Target.endswith("_NT"):
    GCC_BACKEND = False

if Target.endswith("_MINGW") or Target.endswith("_NT"):
    TargetOS = "WIN32"
    HAVE_SERIAL = True

if Host.endswith("_NT") or Host == "NT386":
    Q = "" # q for quote: This is probably about the host, not the target.

#-----------------------------------------------------------------------------

_BuildDir = ("%(Config)s%(_BuildDirC)s" % vars())
M3GDB = (M3GDB or CM3_GDB)
Scripts = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))
PKGSDB = os.path.join(Scripts, "PKGS")

#-----------------------------------------------------------------------------

def GetConfigForDistribution(Target):
    b = os.path.join(Root, "m3-sys", "cminstall", "src", "config-no-install", Target)
    # print("GetConfigForDistribution:" + b)
    return b

#-----------------------------------------------------------------------------

def SetEnvironmentVariable(Name, Value):
    if not os.environ.get(Name) or (os.environ[Name] != Value):
        os.environ[Name] = Value
        if os.name == "posix":
            print(Name + "=" + Value + "\nexport " + Name)
        else:
            print("set " + Name + "=" + Value)

#-----------------------------------------------------------------------------

def IsCygwinBinary(a):
    if IsInterix() or env_OS != "Windows_NT":
        return False
    if not isfile(a):
        FatalError(a + " does not exist")
    a = a.replace("/cygdrive/c/", "c:\\")
    a = a.replace("/cygdrive/d/", "d:\\")
    a = a.replace("/", "\\")
    a = ConvertFromCygwinPath(a)
    #print("a is " + a)
    return (os.system("findstr 2>&1 >" + os.devnull + " /m cygwin1.dll \"" + a + "\"") == 0)

#-----------------------------------------------------------------------------

if _Program != "make-msi.py":
# general problem of way too much stuff at global scope
# workaround some of it
    if IsCygwinBinary(CM3):
        CM3IsCygwin = True
        def ConvertPathForCM3(a):
            return ConvertToCygwinPath(a)
    else:
        CM3IsCygwin = False
        def ConvertPathForCM3(a):
            return ConvertFromCygwinPath(a)

#-----------------------------------------------------------------------------
#
# reflect what we decided back into the environment
#

if _Program != "make-msi.py":
# general problem of way too much stuff at global scope
# workaround some of it
    SetEnvironmentVariable("CM3_TARGET", Target)
    SetEnvironmentVariable("CM3_INSTALL", ConvertPathForCM3(InstallRoot))
    #SetEnvironmentVariable("M3CONFIG", ConvertPathForCM3(os.environ.get("M3CONFIG") or GetConfigForDistribution(Config)))
    #SetEnvironmentVariable("CM3_ROOT", ConvertPathForCM3(Root).replace("\\", "\\\\"))
    SetEnvironmentVariable("CM3_ROOT", ConvertPathForCM3(Root).replace("\\", "/"))

# sys.exit(1)

#-----------------------------------------------------------------------------
# define build and ship programs for Critical Mass Modula-3
#

if _Program != "make-msi.py":
# general problem of way too much stuff at global scope
# workaround some of it
    DEFS = "-DROOT=%(Q)s%(Root)s%(Q)s"

    NativeRoot = Root
    #Root = ConvertPathForCM3(Root).replace("\\", "\\\\")
    Root = ConvertPathForCM3(Root).replace("\\", "/")
    DEFS = (DEFS % vars())
    Root = NativeRoot

#-----------------------------------------------------------------------------
# Make sure these variables all start with a space if they are non-empty.
#

if _Program != "make-msi.py":
# general problem of way too much stuff at global scope
# workaround some of it
    if BuildArgs:
        BuildArgs = " " + BuildArgs

    if CleanArgs:
        CleanArgs = " " + CleanArgs

    if ShipArgs:
        ShipArgs = " " + ShipArgs

#-----------------------------------------------------------------------------
# form the various commands we might run
#

if _Program != "make-msi.py":
# general problem of way too much stuff at global scope
# workaround some of it
    Debug = " " # " -debug "
    CM3_BuildLocal = CM3_BuildLocal or BuildLocal or "%(CM3)s %(CM3_FLAGS)s " + Debug + " -build -override %(DEFS)s%(BuildArgs)s"
    CM3_CleanLocal = CM3_CleanLocal or CleanLocal or "%(CM3)s %(CM3_FLAGS)s " + Debug + " -clean -build -override %(DEFS)s%(CleanArgs)s"
    CM3_BuildGlobal = CM3_BuildGlobal or BuildGlobal or "%(CM3)s %(CM3_FLAGS)s " + Debug + " -build %(DEFS)s%(BuildArgs)s"
    CM3_CleanGlobal = CM3_CleanGlobal or CleanGlobal or "%(CM3)s %(CM3_FLAGS)s " + Debug + " -clean %(DEFS)s%(CleanArgs)s"
    CM3_Ship = CM3_Ship or Ship or "%(CM3)s %(CM3_FLAGS)s -ship %(DEFS)s%(ShipArgs)s"

# other commands

    if os.name == "nt":
        RealClean = RealClean or "if exist %(_BuildDir)s rmdir /q/s %(_BuildDir)s"
    else:
        RealClean = RealClean or "rm -rf %(_BuildDir)s"

    RealClean = (RealClean % vars())

#-----------------------------------------------------------------------------
# choose the compiler to use
# pm3/dec/m3build is not tested and likely cm3 is all that works (heavily used)
#

if _Program != "make-msi.py":
# general problem of way too much stuff at global scope
# workaround some of it
    if SearchPath(CM3):
        BuildLocal = CM3_BuildLocal
        CleanLocal = CM3_CleanLocal
        BuildGlobal = CM3_BuildGlobal
        CleanGlobal = CM3_CleanGlobal
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

#-----------------------------------------------------------------------------

def format_one(width, string):
    return ("%-*s" % (width, string))

#-----------------------------------------------------------------------------

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

#-----------------------------------------------------------------------------

def PrintList2(strings):
    return print_list(strings, 2)

#-----------------------------------------------------------------------------

def PrintList4(strings):
    return print_list(strings, 4)

#-----------------------------------------------------------------------------

def ShowUsage(args, Usage, P):
    for arg in args:
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

#-----------------------------------------------------------------------------

def MakePackageDB():
    if not isfile(PKGSDB):
        #
        # Look for all files src/m3makefile in the CM3 source
        # and write their relative paths from Root to PKGSDB.
        #
        def Callback(Result, Directory, Names):
            if (os.path.basename(Directory) != "src"
                or Directory.find("_darcs") != -1
                or Directory.find("examples/web") != -1
                or Directory.find("examples\\web") != -1
                or (not "m3makefile" in Names)
                or (not isfile(os.path.join(Directory, "m3makefile")))):
                return
            Result.append(Directory[len(Root) + 1:-4].replace('\\', "/") + "\n")

        print("making " + PKGSDB + ".. (slow but rare)")
        Result = [ ]

        os.path.walk(Root, Callback, Result)

        Result.sort()
        open(PKGSDB, "w").writelines(Result)

        if not isfile(PKGSDB):
            File = __file__
            sys.stderr.write("%(File)s: cannot generate package list\n" % vars())
            sys.exit(1)

#-----------------------------------------------------------------------------

PackageDB = None

def ReadPackageDB():
    MakePackageDB()
    global PackageDB
    PackageDB = (PackageDB or map(lambda(a): a.replace("\n", "").replace('\\', '/').replace("\r", ""), open(PKGSDB)))

#-----------------------------------------------------------------------------

def IsPackageDefined(a):
    a = a.replace('\\', '/').lower()
    ReadPackageDB()
    a = ('/' + a)
    for i in PackageDB:
        if i.lower().endswith(a):
            return True

#-----------------------------------------------------------------------------

def GetPackagePath(a):
    a = a.replace('\\', '/').lower()
    ReadPackageDB()
    b = ('/' + a)
    for i in PackageDB:
        j = i.lower()
        if j.endswith(b) or j == a:
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
            if pkg.lower().startswith(Root.lower() + '/'):
                pkg = pkg[len(Root) + 1:]
            # if no slashes, then need a leading slash
            if pkg.find('/') == -1:
                pkg = ('/' + pkg)
            for q in PackageDB:
                q = q.replace('\\', '/')
                if q.lower().find(pkg.lower()) != -1:
                    Result.append(q)
                    break
    else:
        Result = PackageDB
    return map(lambda(a): (Root + '/' + a), Result)

#-----------------------------------------------------------------------------

def _Run(NoAction, Actions, PackageDirectory):

    print(" +++ %s +++" % Actions)

    if NoAction:
        return 0

    #return 0

    PreviousDirectory = os.getcwd()
    os.chdir(PackageDirectory.replace('/', os.path.sep))

    Result = os.system(Actions)

    os.chdir(PreviousDirectory)
    return Result

#-----------------------------------------------------------------------------

def _BuildLocalFunction(NoAction, PackageDirectory):
    return _Run(NoAction, BuildLocal, PackageDirectory)

#-----------------------------------------------------------------------------

def _BuildGlobalFunction(NoAction, PackageDirectory):
    return _Run(NoAction, BuildGlobal, PackageDirectory)

#-----------------------------------------------------------------------------

def _ShipFunction(NoAction, PackageDirectory):
    return _Run(NoAction, Ship, PackageDirectory)

#-----------------------------------------------------------------------------

def _CleanLocalFunction(NoAction, PackageDirectory):
    return _Run(NoAction, CleanLocal, PackageDirectory)

#-----------------------------------------------------------------------------

def _CleanGlobalFunction(NoAction, PackageDirectory):
    return _Run(NoAction, CleanGlobal, PackageDirectory)

#-----------------------------------------------------------------------------

def _RealCleanFunction(NoAction, PackageDirectory):
#
# This in particular need not run commands but can be implemented
# directly in Python.
#
    return _Run(NoAction, RealClean, PackageDirectory)

#-----------------------------------------------------------------------------

def _MakeTGZ(a):
    out = a + ".tgz"
    DeleteFile(out)
    Tar = SearchPath("gtar") or SearchPath("tar")
    b = Tar + " cfz " + out + " " + a
    print(b + "\n")
    os.system(b)
    
def _MakeZip(a):
    out = a + ".zip"
    DeleteFile(out)
    b = "zip -r " + out + "  " + a
    print(b + "\n")
    os.system(b)

#-----------------------------------------------------------------------------

def _SqueezeSpaces(a):
    # squeeze runs of spaces and spaces at ends
    a = re.sub("  +", " ", a)
    a = re.sub(" +$", "", a)
    a = re.sub("^ +", "", a)
    return a

def Boot():

    global BuildLocal
    BuildLocal += " -boot -keep -DM3CC_TARGET=" + Config

    Version = CM3VERSION + "-" + time.strftime("%Y%m%d")
    BootDir = "./cm3-boot-" + Target + "-" + Version

    try:
        shutil.rmtree(BootDir)
    except:
        pass
    try:
        os.mkdir(BootDir)
    except:
        pass

    # This information is duplicated from the config files.
    # TBD: put it only in one place.
    # The older bootstraping method does get that right.

    vms = StringContainsI(Target, "VMS")
    nt = Config.startswith("NT") or Config.endswith("NT") or Tagged(Config, "NT")
    darwin = StringContainsI(Target, "DARWIN")
    mingw = StringContainsI(Target, "MINGW")
    solaris = StringContainsI(Target, "SOLARIS")
    sol = Target.startswith("SOL")
    hpux = StringContainsI(Target, "HPUX")
    osf = StringContainsI(Target, "OSF")
    interix = StringContainsI(Target, "INTERIX")
    alpha32 = StringContainsI(Target, "ALPHA32")
    alpha64 = StringContainsI(Target, "ALPHA64")
    alpha32vms = vms and alpha32
    alpha64vms = vms and alpha64
    solsun = Config == "SOLsun"
    freebsd = StringContainsI(Target, "FreeBSD")
    netbsd = StringContainsI(Target, "NetBSD")
    openbsd = StringContainsI(Target, "OpenBSD")
    cygwin = StringContainsI(Target, "Cygwin")
    linux = StringContainsI(Target, "Linux")
    alpha = StringContainsI(Target, "ALPHA")
    thirtytwo = StringContainsI(Target, "32")
    sixtyfour = StringContainsI(Target, "64")
    bsd = StringContainsI(Target, "BSD")

    # pick the compiler
    
    c = "c" in sys.argv
    
    CCompilerOut = ""

    if alpha32vms:
        CCompiler = "cc"
        CCompilerFlags = " "
    elif alpha64vms:
        CCompiler = "cc"
        CCompilerFlags = "/pointer_size=64 "
    elif solaris or solsun:
        CCompiler = "/usr/bin/cc"
        CCompilerFlags = " -g -mt -xldscope=symbolic "
        CCompiler = "./c_compiler"
        CopyFile("./c_compiler", BootDir)
    elif osf:
        CCompiler = "/usr/bin/cc"
        CCompilerFlags = " -g -pthread "
    else:
        # gcc and other platforms
        CCompiler = {
            "SOLgnu" : "/usr/sfw/bin/gcc",
            "AMD64_NT"      : "cl",
            }.get(Config) or "gcc"

        CCompilerFlags = {
            "I386_INTERIX"  : " -g ", # gcc -fPIC generates incorrect code on Interix
            "SOLgnu"        : " -g ", # -fPIC?
            #"AMD64_NT"      : " -Zi -MD ",
            "AMD64_NT"      : " -Zi ", # hack some problem with exception handling and alignment otherwise
            }.get(Config) or " -g -fPIC "

        CCompilerOut = {
            "AMD64_NT"      : "-Fo./",
            }.get(Config) or "-o $@"

    CCompilerFlags = CCompilerFlags + ({
        "AMD64_LINUX"     : " -m64 ",
        "AMD64_DARWIN"    : " -arch x86_64 ",
        "PPC64_DARWIN"    : " -arch ppc64 ",
        "ARM_DARWIN"      : " -march=armv6 -mcpu=arm1176jzf-s ",
        "LINUXLIBC6"      : " -m32 ",
        "I386_LINUX"      : " -m32 ",
        "MIPS64_OPENBSD"  : " -mabi=64 ",
        "I386_SOLARIS"    : " -xarch=pentium_pro -Kpic ",
        "AMD64_SOLARIS"   : " -xarch=amd64       -Kpic ",
        "SOLsun"          : " -xarch=v8plus -xcode=pic32 -xregs=no%appl ",
        "SPARC32_SOLARIS" : " -xarch=v8plus -xcode=pic32 -xregs=no%appl ",
        "SPARC64_SOLARIS" : " -xarch=v9     -xcode=pic32 -xregs=no%appl ",
        "SOLgnu"          : " -m32 -mcpu=v9 -mno-app-regs ",
        "SPARC32_LINUX"   : " -m32 -mcpu=v9 -mno-app-regs ",
        "SPARC64_LINUX"   : " -m64 -mno-app-regs ",
        }.get(Config) or " ")

    obj = ["o", "obj"][nt]
    Link = "$(CC) $(CFLAGS) *." + obj + [" *.mo *.io ", " "][_CBackend]

    # link flags
    
    # TBD: add more and retest, e.g. Irix, AIX, HPUX, Android
    # http://www.openldap.org/lists/openldap-bugs/200006/msg00070.html
    # http://www.gnu.org/software/autoconf-archive/ax_pthread.html#ax_pthread

    if darwin:
        pass
    elif mingw:
        pass
    elif solaris or sol:
        Link = Link  +  " -lpthread -lrt -lm -lnsl -lsocket -lc "
    elif hpux:
        Link = Link + " -lrt -lm -lpthread "
    elif osf:
        Link = Link + " -lrt -lm "
    elif interix:
        Link = Link + " -lm " # -pthread?
    elif nt:
        if _CBackend:
            #Link = "link /incremental:no /debug /pdb:$(@R).pdb *." + obj + " "
            Link = "link /incremental:no /debug /pdb:$(@R).pdb $** "
            # be sure to get a .pdb out
            #open("empty.c", "w")
            #Link = CCompiler + CCompilerFlags + "empty.c /" + Link
        Link = Link + " user32.lib kernel32.lib wsock32.lib comctl32.lib gdi32.lib advapi32.lib netapi32.lib iphlpapi.lib "
    # not all of these tested esp. Cygwin, NetBSD
    elif freebsd or netbsd or openbsd or cygwin or linux:
        Link = Link  +  " -lm -pthread "
    else:
        Link = Link + " -lm -lpthread "
    
    # add -c to compiler but not link (i.e. not CCompilerFlags)

    Compile = "$(CC) $(CFLAGS) " + [" -c ", ""][vms]

    AssembleOnTarget = not vms
    AssembleOnHost = not AssembleOnTarget

    # pick assembler

    if vms and AssembleOnTarget:
        Assembler = "macro" # not right, come back to it later
        AssemblerFlags = "/alpha " # not right, come back to it later
    elif Target == "AMD64_SOLARIS":
        # see http://gcc.gnu.org/ml/gcc/2010-05/msg00155.html
        Assembler = "/usr/ccs/bin/as"
    elif solaris or sol:
        # see http://gcc.gnu.org/ml/gcc/2010-05/msg00155.html
        Assembler = "./assembler"
        CopyFile("./assembler", BootDir)
    elif osf:
        Assembler = "/usr/bin/as"
    else:
        Assembler = "as"

    # set assembler flags

    AssemblerFlags = " "

    if not Target in ["PPC32_OPENBSD", "PPC_LINUX", "ARM_LINUX", "ARMEL_LINUX", "ALPHA_LINUX", "ALPHA_OPENBSD"]:
        if linux or bsd:
            if sixtyfour or (alpha and not alpha32):
                AssemblerFlags = AssemblerFlags + " --64"
            else:
                AssemblerFlags = AssemblerFlags + " --32"

    AssemblerFlags = (AssemblerFlags + ({
        "ALPHA_OSF"         : " -nocpp ",
        "AMD64_DARWIN"      : " -arch x86_64 ",
        "PPC64_DARWIN"      : " -arch ppc64 ",
        "ARM_DARWIN"        : " -arch armv6 ",
        "I386_SOLARIS"      : " -Qy -s ",
        "AMD64_SOLARIS"     : " -Qy -s        -xarch=generic64 ",
        "SOLgnu"            : " -Qy -s -K PIC -xarch=v8plus ", # Sun assembler
        "SOLsun"            : " -Qy -s -K PIC -xarch=v8plus ",
        "SPARC32_SOLARIS"   : " -Qy -s -K PIC -xarch=v8plus ",
        "SPARC64_SOLARIS"   : " -Qy -s -K PIC -xarch=v9 ",
        "SPARC32_LINUX"     : " -Qy -s -KPIC -Av9a -32 -relax ",
        "SPARC64_LINUX"     : " -Qy -s -KPIC -Av9a -64 -no-undeclared-regs -relax ",
        }.get(Target) or ""))

    is_main_table =  { }
    for a in ["Main.m3"]:
        for c in ["c", "cpp"]:
            is_main_table[a + "." + c] = True

    GnuPlatformPrefix = {
        "ARM_DARWIN"    : "arm-apple-darwin8-",
        # "ARMEL_LINUX" : "arm-linux-gnueabihf-",
        # "ARM_LINUX"   : "arm-linux-gnueabihf-",
        "ALPHA32_VMS"   : "alpha-dec-vms-",
        "ALPHA64_VMS"   : "alpha64-dec-vms-",
        }.get(Target) or ""

    DeleteCommand = ["rm -rf", "del /f"][nt]

    if not vms:
        CCompiler = GnuPlatformPrefix + CCompiler
    if (not vms) or AssembleOnHost:
        Assembler = GnuPlatformPrefix + Assembler

    # squeeze runs of spaces and spaces at ends

    Compile = _SqueezeSpaces(Compile)
    CCompilerFlags = _SqueezeSpaces(CCompilerFlags)
    Link = _SqueezeSpaces(Link)
    Assembler = _SqueezeSpaces(Assembler)
    AssemblerFlags = _SqueezeSpaces(AssemblerFlags)

    P = FilterPackages([ "m3cc", "import-libs", "m3core", "libm3", "sysutils",
          "m3middle", "m3quake", "m3objfile", "m3linker", "m3back",
          "m3front" ])
    main_packages = ["cm3"]
    if nt:
        main_packages += ["mklib"]
    P += main_packages

    #DoPackage(["", "realclean"] + P) or sys.exit(1)
    DoPackage(["", "buildlocal"] + P) or sys.exit(1)

    #
    # This would probably be a good use of XSL (xml style sheets)
    #
    
    NL = ["\n", "\r\n"][nt]
    NL2 = NL + NL
    EXE = ["", ".exe"][nt]
    Make = open(os.path.join(BootDir, "make.sh"), "wb")
    VmsMake  = open(os.path.join(BootDir, "vmsmake.com"), "wb")
    VmsLink  = open(os.path.join(BootDir, "vmslink.opt"), "wb")
    Makefile = open(os.path.join(BootDir, "Makefile"), "wb")
    UpdateSource = open(os.path.join(BootDir, "updatesource.sh"), "wb")
    Objects = { }
    ObjectsExceptMain = { }

    for a in [Makefile]:
        a.write("# edit up here" + NL2
                + "CC=" + CCompiler + NL
                + "CFLAGS=" + CCompilerFlags + NL)
        a.write("Compile=" + Compile + NL)
        if not _CBackend:
            a.write("Assemble=" + Assembler + " " + AssemblerFlags + NL)
        a.write("Link=" + Link + NL
                + NL + "# no more editing should be needed" + NL2)

    if True: #not _CBackend:
        Makefile.write(".SUFFIXES:" + NL
                       + ".SUFFIXES: .c .is .ms .s .o .obj .io .mo" + NL2)

    Makefile.write("all: ")    
    for pkg in main_packages:
        Makefile.write(pkg + EXE + " ")
    Makefile.write(NL2)

    Makefile.write("clean:" + NL
                   + "\t" + DeleteCommand + " *.io *.mo *.o *.obj" + NL2)

    for a in [UpdateSource, Make]:
        a.write("#!/bin/sh\n\n"
                + "set -e\n"
                + "set -x\n\n")

    for a in [Make]:
        a.write("# edit up here\n\n"
                + "CC=${CC:-" + CCompiler + "}\n"
                + "CFLAGS=${CFLAGS:-" + CCompilerFlags + "}\n"
                + "Compile=" + Compile + "\n")
        if not _CBackend:
            a.write("Assemble=" + Assembler + " " + AssemblerFlags + "\n")
        a.write("Link=" + Link + "\n"
                + "\n# no more editing should be needed\n\n")

    for q in P:
        dir = GetPackagePath(q)
        for a in os.listdir(os.path.join(Root, dir, Config)):
            ext_c = a.endswith(".c")
            ext_cpp = a.endswith(".cpp")
            ext_h = a.endswith(".h")
            ext_s = a.endswith(".s")
            ext_ms = a.endswith(".ms")
            ext_is = a.endswith(".is")
            ext_io = a.endswith(".io")
            ext_mo = a.endswith(".mo")
            if not (ext_c or ext_cpp or ext_h or ext_s or ext_ms or ext_is or ext_io or ext_mo):
                continue
            leaf = GetLastPathElement(a)
            dest = os.path.join(BootDir, leaf)
            is_main = (ext_c or ext_cpp) and is_main_table.get(leaf, False)
            if is_main:
                dest = os.path.join(BootDir, q + "_" + leaf)
            fullpath = os.path.join(Root, dir, Config, a)
            if ext_h or ext_c or not vms or AssembleOnTarget or ext_io or ext_mo:
                CopyFile(fullpath, dest)
            if ext_h or ext_io or ext_mo:
                continue
            Object = _GetObjectName(a, obj)
            if Objects.get(Object):
                continue
            Objects[Object] = 1
            if not is_main:
                ObjectsExceptMain[Object] = 1
            if ext_c:
                VmsMake.write("$ " + Compile + " " + a + "\n")
            else:
                if AssembleOnHost:
                    # must have cross assembler
                    a = Assembler + " " + fullpath + " -o " + BootDir + "/" + Object
                    print(a)
                    os.system(a)
                else:
                    VmsMake.write("$ " + Assembler + " " + a + "\n")                    
            VmsLink.write(Object + "/SELECTIVE_SEARCH\n")
            
    # double colon batches and is much faster
    colon = [":", "::"][nt]

    if _CBackend or not nt:
        # write inference rules: .c => .o, .c => .obj, .cpp => .o, .cpp => .obj
        for c in ["c", "cpp"]:
            for o in ["o", "obj"]:
                Makefile.write("." + c + "." + o + colon + NL + "\t$(Compile) " + CCompilerOut + " $<" + NL2)
            
        # write inference rules: .is => .io, .s => .o, .ms => .mo
        if not _CBackend:
            for source_obj in [["is", "io"], ["s", "o"], ["ms", "mo"]]:
                source = source_obj[0]
                obj = source_obj[1]
                Makefile.write("." + source + "." + obj + ":" + NL + "\t$(Assemble) -o $@ $<" + NL2)

    Makefile.write("OBJECTS=")
    Objects = ObjectsExceptMain.keys()
    Objects.sort()
    k = 8
    for a in Objects:
        k = k + 1 + len(a)
        if k > 76: # line wrap
            Makefile.write(" \\" + NL)
            k = 1 + len(a)
        Makefile.write(" " + a)

    Makefile.write(NL2)
 
    LinkOut = [" -o ", " -out:"][nt]

    for pkg in main_packages:
        Makefile.write(pkg + EXE + ":")
        main = pkg + "_Main.m3." + obj
        if main in ObjectsExceptMain:
            Makefile.write(" " + main)
        Makefile.write(" " + "$(OBJECTS)")
        Makefile.write(NL + "\t")
        Makefile.write("$(Link) " + LinkOut + "$@" + NL)

    VmsMake.write("$ set file/attr=(rfm=var,rat=none) *.o\n")
    VmsMake.write("$ set file/attr=(rfm=var,rat=none) *.obj\n")
    VmsMake.write("$ set file/attr=(rfm=var,rat=none) *.mo\n")
    VmsMake.write("$ set file/attr=(rfm=var,rat=none) *.io\n")
    VmsMake.write("$ link /executable=cm3.exe vmslink/options\n")
    
    for a in [Make]:
        a.write("$(Link) " + LinkOut + "$@" + NL)
        
    if False:
        for a in [
            #
            # Add to this list as needed.
            # Adding more than necessary is ok -- assume the target system has no changes,
            # so we can replace whatever is there.
            #
            "m3-libs/libm3/src/os/POSIX/OSConfigPosix.m3",
            "m3-libs/libm3/src/random/m3makefile",
            "m3-libs/m3core/src/m3makefile",
            "m3-libs/m3core/src/Uwaitpid.quake",
            "m3-libs/m3core/src/thread.quake",
            "m3-libs/m3core/src/C/m3makefile",
            "m3-libs/m3core/src/C/" + Target + "/m3makefile",
            "m3-libs/m3core/src/C/Common/m3makefile",
            "m3-libs/m3core/src/Csupport/m3makefile",
            "m3-libs/m3core/src/float/m3makefile",
            "m3-libs/m3core/src/runtime/m3makefile",
            "m3-libs/m3core/src/runtime/common/m3makefile",
            "m3-libs/m3core/src/runtime/common/Compiler.tmpl",
            "m3-libs/m3core/src/runtime/common/m3text.h",
            "m3-libs/m3core/src/runtime/common/RTError.h",
            "m3-libs/m3core/src/runtime/common/RTMachine.i3",
            "m3-libs/m3core/src/runtime/common/RTProcess.h",
            "m3-libs/m3core/src/runtime/common/RTSignalC.c",
            "m3-libs/m3core/src/runtime/common/RTSignalC.h",
            "m3-libs/m3core/src/runtime/common/RTSignalC.i3",
            "m3-libs/m3core/src/runtime/common/RTSignal.i3",
            "m3-libs/m3core/src/runtime/common/RTSignal.m3",
            "m3-libs/m3core/src/runtime/" + Target + "/m3makefile",
            "m3-libs/m3core/src/runtime/" + Target + "/RTMachine.m3",
            "m3-libs/m3core/src/runtime/" + Target + "/RTSignal.m3",
            "m3-libs/m3core/src/runtime/" + Target + "/RTThread.m3",
            "m3-libs/m3core/src/text/TextLiteral.i3",
            "m3-libs/m3core/src/thread/m3makefile",
            "m3-libs/m3core/src/thread/PTHREAD/m3makefile",
            "m3-libs/m3core/src/thread/PTHREAD/ThreadPThread.m3",
            "m3-libs/m3core/src/thread/PTHREAD/ThreadPThreadC.i3",
            "m3-libs/m3core/src/thread/PTHREAD/ThreadPThreadC.c",
            "m3-libs/m3core/src/time/POSIX/m3makefile",
            "m3-libs/m3core/src/unix/m3makefile",
            "m3-libs/m3core/src/unix/Common/m3makefile",
            "m3-libs/m3core/src/unix/Common/m3unix.h",
            "m3-libs/m3core/src/unix/Common/Udir.i3",
            "m3-libs/m3core/src/unix/Common/UdirC.c",
            "m3-libs/m3core/src/unix/Common/Usignal.i3",
            "m3-libs/m3core/src/unix/Common/Ustat.i3",
            "m3-libs/m3core/src/unix/Common/UstatC.c",
            "m3-libs/m3core/src/unix/Common/UtimeC.c",
            "m3-libs/m3core/src/unix/Common/Uucontext.i3",
            "m3-sys/cminstall/src/config-no-install/SOLgnu",
            "m3-sys/cminstall/src/config-no-install/SOLsun",
            "m3-sys/cminstall/src/config-no-install/Solaris.common",
            "m3-sys/cminstall/src/config-no-install/Unix.common",
            "m3-sys/cminstall/src/config-no-install/cm3cfg.common",
            "m3-sys/cminstall/src/config-no-install/" + Target,
            "m3-sys/m3cc/src/m3makefile",
            "m3-sys/m3cc/src/gcc/m3cg/parse.c",
            "m3-sys/m3middle/src/Target.i3",
            "m3-sys/m3middle/src/Target.m3",
            "scripts/python/pylib.py",
            "m3-libs/m3core/src/C/" + Target + "/Csetjmp.i3",
            "m3-libs/m3core/src/C/" + Target + "/m3makefile",
            "m3-libs/m3core/src/C/Common/Csetjmp.i3",
            "m3-libs/m3core/src/C/Common/Csignal.i3",
            "m3-libs/m3core/src/C/Common/Cstdio.i3",
            "m3-libs/m3core/src/C/Common/Cstring.i3",
            "m3-libs/m3core/src/C/Common/m3makefile",
            ]:
            source = os.path.join(Root, a)
            if FileExists(source):
                name = GetLastPathElement(a)
                reldir = RemoveLastPathElement(a)
                destdir = os.path.join(BootDir, reldir)
                dest = os.path.join(destdir, name)
                try:
                    os.makedirs(destdir)
                except:
                    pass
                CopyFile(source, dest)

                for b in [UpdateSource, Make]:
                    b.write("mkdir -p /dev2/cm3/" + reldir + "\n")
                    b.write("cp " + a + " /dev2/cm3/" + a + "\n")

    for a in [UpdateSource, Make, Makefile, VmsMake, VmsLink]:
        a.close()
    Make.close()
        
    # write entirely new custom makefile for NT
    # We always have object files so just compile and link in one fell swoop.

    if nt:
        DeleteFile("updatesource.sh")
        DeleteFile("make.sh")
        if not _CBackend:
            Makefile = open(os.path.join(BootDir, "Makefile"), "wb")
            Makefile.write("cm3" + EXE + ": *.io *.mo *.c\r\n"
            + " cl -Zi -MD *.c -link *.mo *.io -out:$@ user32.lib kernel32.lib wsock32.lib comctl32.lib gdi32.lib advapi32.lib netapi32.lib iphlpapi.lib\r\n")
            Makefile.close()

    if vms or nt:
        _MakeZip(BootDir[2:])
    else:
        _MakeTGZ(BootDir[2:])

#-----------------------------------------------------------------------------
# map action names to code and possibly other data

if _Program != "make-msi.py":
# general problem of way too much stuff at global scope
# workaround some of it
    ActionInfo = {
        "build":        { "Commands": [_BuildLocalFunction], },
        "buildlocal":   { "Commands": [_BuildLocalFunction], },
        "buildglobal":  { "Commands": [_BuildGlobalFunction, _ShipFunction], },
        "buildship":    { "Commands": [_BuildGlobalFunction, _ShipFunction], },
        "ship":         { "Commands": [_ShipFunction], },
        "clean":
        {
            "Commands": [_CleanLocalFunction],
            "KeepGoing": True,
        },
        "cleanlocal":
        {
            "Commands": [_CleanLocalFunction],
            "KeepGoing": True,
        },
        "cleanglobal":
        {
            "Commands": [_CleanGlobalFunction],
            "KeepGoing": True,
        },
        "realclean":
        {
            "Commands": [_RealCleanFunction],
            "KeepGoing": True,
        },
    }

#-----------------------------------------------------------------------------

if _Program != "make-msi.py":
# general problem of way too much stuff at global scope
# workaround some of it
    BuildAll = getenv("CM3_ALL") or False

def _FilterPackage(Package):
    PackageConditions = {
        "m3gdb": (M3GDB or CM3_GDB) and
                    {"FreeBSD4": True,
                    "LINUXLIBC6" : True,
                    "SOLgnu" : True,
                    "I386_CYGWIN" : True,
                    "I386_FREEBSD" : True,
                    "I386_NETBSD" : True,
                    "I386_LINUX" : True,
                    }.get(Target, False),
        "tcl": BuildAll or HAVE_TCL,
        "tapi": BuildAll or TargetOS == "WIN32",
        "serial": BuildAll or HAVE_SERIAL,
        "X11R4": BuildAll or TargetOS != "WIN32",
        "m3cc": (GCC_BACKEND and (not OMIT_GCC) and not [a for a in _SkipGccFlags if a in sys.argv])
    }
    return PackageConditions.get(Package, True)

#-----------------------------------------------------------------------------

def FilterPackages(Packages):
    Packages = filter(_FilterPackage, Packages)
    return Packages

def GroupsToPackages(Packages):
    result = [ ]
    global PackageSets
    for Package in Packages:
        result += PackageSets.get(Package, [Package])
    return result

#-----------------------------------------------------------------------------

PackageSets = None

#-----------------------------------------------------------------------------

def GetPackageSets():
    if PackageSets:
        return PackageSets
    #print("reading pkginfo.txt")
    result = { }
    for line in open(os.path.join(Scripts, "pkginfo.txt")):
        line = line.replace("\n", "").replace("\r", "")
        fields = line.split(" ")
        name1 = fields[0]
        name2 = name1.split("/")[-1]
        for group in ["all"] + fields[1:]:
            if not (group in result):
                result[group] = [ ]
            result[group] += [name1]
            if name1 != name2:
                result[group] += [name2]
    #print("all:" + str(result["all"]))
    #sys.exit(1)
    result["std"] = result["all"]
    return result

if _Program != "make-msi.py":
# general problem of way too much stuff at global scope
# workaround some of it
    PackageSets = GetPackageSets()

#-----------------------------------------------------------------------------

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

#-----------------------------------------------------------------------------

def DoPackage(args, PackagesFromCaller = None):

    SetupEnvironment()

    args = filter(lambda(a): not a.startswith(os.path.dirname(__file__)) and a != "" and not a.endswith(".py"), args)

    # print("args is " + str(args))
    # sys.stdout.flush()

    if not (PackagesFromCaller is None):
        PackagesFromCaller = OrderPackages(FilterPackages(GroupsToPackages(PackagesFromCaller)))
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
        PackagesFromCaller)

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
    for arg in args:
        if ((arg == "")
            or (arg.lower() in _AllTargets)
            or (arg in _PossiblePylibFlags)
            ):
            continue
        if arg.startswith("-"):
            if arg == "-l":
                ListOnly = True
            elif arg == "-k":
                KeepGoing = True
            elif arg == "-n":
                NoAction = True
            else:
                global ExtraArgs
                ExtraArgs = [ ]
                ExtraArgs += arg
        else:
            Action = ActionInfo.get(arg)
            if Action:
                ActionCommands = Action["Commands"]
                KeepGoing = Action.get("KeepGoing", False)
            else:
                PackagesFromCommandLine.append(arg)

    PackagesFromCommandLine = OrderPackages(FilterPackages(GroupsToPackages(PackagesFromCommandLine)))

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

        q = GetPackagePath(p)
        if not q:
            File = __file__
            sys.stderr.write("%(File)s *** cannot find package %(p)s\n" % vars())
            sys.exit(1)

        q = os.path.join(Root, q)
        if isfile(os.path.join(q, "src", "m3makefile")):
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
        print("")
        for a in ActionCommands:
            ExitCode = a(NoAction, p)
            if ExitCode != 0:
                Success = False
                if not KeepGoing:
                    print(" *** execution of %s failed ***" % (str(ActionCommands)))
                    sys.exit(1)
            if KeepGoing:
                print(" ==> %s returned %s" % (str(ActionCommands), ExitCode))
                print("")
            else:
                print(" ==> %(p)s done" % vars())
                print("")

    return Success

#-----------------------------------------------------------------------------

def DeleteFile(a):
    if os.name != "nt":
        print("rm -f " + a)
    else:
        print("del /f /a " + a)
    if isfile(a):
        os.chmod(ConvertPathForPython(a), 0700)
        os.remove(ConvertPathForPython(a))

def MoveFile(a, b):
    if os.name != "nt":
        print("mv " + a + " " + b)
    else:
        print("move " + a + " " + b)
    shutil.move(ConvertPathForPython(a), ConvertPathForPython(b))

#-----------------------------------------------------------------------------

def RemoveDirectoryRecursive(a):
    if os.name != "nt":
        print("rm -rf " + a)
    else:
        print("rmdir /q/s " + a)
    if isdir(a):
        shutil.rmtree(a)
    return True

def CreateDirectory(a):
    if os.name != "nt":
        print("mkdir -p " + a)
    else:
        print("mkdir " + a)
    if not isdir(a):
        os.makedirs(a)
    return True

#-----------------------------------------------------------------------------

def MakeTempDir():
    if getenv("TMPDIR"):
        if not os.path.exists(getenv("TMPDIR")):
            CreateDirectory(getenv("TMPDIR"))
        return
    if getenv("TEMP") and not os.path.exists(getenv("TEMP")):
        CreateDirectory(getenv("TEMP"))

MakeTempDir()

#-----------------------------------------------------------------------------

def CopyFile(From, To):
    if isdir(To):
        To = os.path.join(To, os.path.basename(From))
    # Cygwin says foo exists when only foo.exe exists, and then remove fails.
    if isfile(To):
        try:
            os.remove(ConvertPathForPython(To))
        except:
            pass
    CopyCommand = "copy"
    if os.name != "nt":
        CopyCommand = "cp -Pv"
    print(CopyCommand + " " + From + " " + To)
    if os.path.islink(ConvertPathForPython(From)):
        os.symlink(os.readlink(ConvertPathForPython(From)), ConvertPathForPython(To))
    else:
        shutil.copy(ConvertPathForPython(From), ConvertPathForPython(To))
    return True

#-----------------------------------------------------------------------------

def CopyFileIfExist(From, To):
    if isfile(From):
        return CopyFile(From, To)
    return True

#-----------------------------------------------------------------------------

def DeleteConfig(To):
    a = os.path.join(Root, "m3-sys", "cminstall", "src")
    Bin  = os.path.join(To, "bin")
    RemoveDirectoryRecursive(os.path.join(Bin, "config"))
    for b in ["config", "config-no-install"]:
        for File in glob.glob(os.path.join(a, b, "*")):
            if isfile(File):
                DeleteFile(os.path.join(Bin, os.path.basename(File)))

#-----------------------------------------------------------------------------

def CopyConfigForDevelopment():
    #
    # The development environment is easily reconfigured
    # for different targets based on environment variables and `uname`.
    # The use of `uname` is not fully fleshed out (yet).
    #
    # The development environment depends on having a source tree, at least the cminstall\src\config directory.
    #

    To = os.path.join(InstallRoot, "bin")
    a = os.path.join(Root, "m3-sys", "cminstall", "src")

    #
    # First delete all the config files.
    #
    DeleteConfig(InstallRoot)

    # CopyFile(os.path.join(Root, a, "config", "cm3.cfg"), To) or FatalError()
    CopyFile(os.path.join(Root, a, "config-no-install", "cm3.cfg"), To) or FatalError()
    return True

#-----------------------------------------------------------------------------

#def CopyDirectoryNonRecursive(From, To):
#    CreateDirectory(To)
#    for File in glob.glob(os.path.join(From, "*")):
#        if isfile(File):
#            print(File + " => " + To + "\n")
#            CopyFile(File, To)
#    return True

#-----------------------------------------------------------------------------

def CopyConfigForDistribution(To):
    Bin  = os.path.join(To, "bin")
    dir = os.path.join(Bin, "config")
    DeleteConfig(To)
    CreateDirectory(dir)
    for File in glob.glob(os.path.join(Root, "m3-sys", "cminstall", "src", "config-no-install", "*")):
        if isfile(File):
            #print(File + " => " + dir + "\n")
            CopyFile(File, dir)
    open(os.path.join(Bin, "cm3.cfg"), "w").write("\
if not defined(\"SL\") SL = \"/\" end\n\
if not defined(\"HOST\") HOST = \"" + Config + "\" end\n\
if not defined(\"TARGET\") TARGET = HOST end\n\
INSTALL_ROOT = (path() & SL & \"..\")\n\
include(path() & SL & \"config\" & SL & TARGET" + ")")
    return True

#-----------------------------------------------------------------------------

def _CopyCompiler(From, To):

    any_exist = False
    froms = [ ]
    for a in ["cm3", "mklib", "cm3cg", "mips-tfile"]:
        for b in [".exe", ""]:
            c = os.path.join(From, a + b)
            froms += [c]
            if not any_exist:
                any_exist = FileExists(c)

    if not any_exist:
        FatalError("none of " + ", ".join(froms) + " exist")

    CreateDirectory(To)

    for a in ["cm3", "cm3cg", "mips-tfile"]:
        # .exe should come first due to Cygwin wierdness
        f = os.path.join(From, a)
        fexe = f + ".exe"
        fpdb = f + ".pdb"
        if FileExists(fexe):
          f = fexe
        if FileExists(f):
          CopyFile(f, To) or FatalError("3")
          if FileExists(fpdb):
            CopyFile(fpdb, To) or FatalError("5")

    return True

#-----------------------------------------------------------------------------

def PickBuildDir(a):
    if DirectoryExists(a):
        return a
    if a.endswith("c"):
        a = a[:-1]
        if DirectoryExists(a):
            return a
    elif DirectoryExists(a + "c"):
        return a + "c"
    FatalError("no BuildDir:" + a)

def ShipBack():
    if  not GCC_BACKEND:
        return True
    return _CopyCompiler(os.path.join(Root, "m3-sys", "m3cc", PickBuildDir(_BuildDir)),
                         os.path.join(InstallRoot, "bin"))

#-----------------------------------------------------------------------------

def ShipFront():
    return _CopyCompiler(os.path.join(Root, "m3-sys", "cm3", PickBuildDir(_BuildDir)),
                         os.path.join(InstallRoot, "bin"))

#-----------------------------------------------------------------------------

def ShipCompiler():
    return ShipBack() and ShipFront()

#-----------------------------------------------------------------------------

def CopyCompiler(From, To):
    #
    # copy cm3, cm3cg, mklib, mips-tfile from one installroot/bin to another
    #
    _CopyCompiler(os.path.join(From, "bin"), os.path.join(To, "bin"))
    return True

#-----------------------------------------------------------------------------

def GetProgramFiles():
    # Look for Program Files.
    # This is expensive and callers are expected to cache it.
    ProgramFiles = []
    for d in ["PROGRAMFILES", "PROGRAMFILES(X86)", "PROGRAMW6432"]:
        e = os.environ.get(d)
        if e and (not (e in ProgramFiles)) and isdir(e):
            ProgramFiles.append(e)
    if len(ProgramFiles) == 0:
        SystemDrive = os.environ.get("SystemDrive", "")
        a = os.path.join(SystemDrive, "Program Files")
        if isdir(a):
            ProgramFiles.append(a)
    return ProgramFiles

def GetVisualCPlusPlusVersion():
    a = os.popen("cl 2>&1 >nul").read().lower()
    if a.find("9.00") != -1:
        return "20"
    if a.find("10.00") != -1:
        return "40"
    if a.find("10.10") != -1:
        return "41"
    if a.find("10.20") != -1:
        return "42"
    if a.find("11.00") != -1:
        return "50"
    if a.find("12.00") != -1:
        return "60"
    if a.find("13.00") != -1:
        return "70"
    if a.find("13.10") != -1:
        return "71"
    if a.find("14.00") != -1:
        return "80"
    if a.find("15.00") != -1:
        return "90"
    if a.find("16.00") != -1:
        return "100"
    if a.find("17.00") != -1:
        return "110"
    FatalError("unable to detect Visual C++ version, maybe cl is not in %PATH%?")

def IsCygwinHostTarget(): # confused
    return Host.endswith("_CYGWIN") or (Host == "NT386" and GCC_BACKEND and TargetOS == "POSIX")

def IsMinGWHostTarget():
    return (Target == "NT386" and GCC_BACKEND and TargetOS == "WIN32") or Target.endswith("_MINGW")

_HostIsNT = (os.environ.get("OS") == "Windows_NT")

def IsNativeNTHostTarget(): # confused
    return ((Target == "NT386" or Target.endswith("_NT"))
            and _HostIsNT
            and (Config == "NT386" or Config.endswith("_NT"))
            and (not GCC_BACKEND) and TargetOS == "WIN32")

def SetupEnvironment():
    SystemDrive = os.environ.get("SYSTEMDRIVE")
    if SystemDrive:
        SystemDrive += os.path.sep

    # Do this earlier so that its link isn't a problem.
    # Looking in the registry HKEY_LOCAL_MACHINE\SOFTWARE\Cygnus Solutions\Cygwin\mounts v2
    # would be reasonable here.

    if CM3IsCygwin:
        _SetupEnvironmentVariableAll(
            "PATH",
            ["cygwin1.dll"],
            os.path.join(SystemDrive, "cygwin", "bin"))

    # some host/target confusion here..

    if IsNativeNTHostTarget():

        VCBin = ""
        VCInc = ""
        VCLib = ""
        MspdbDir = ""

        # 4.0 e:\MSDEV
        # 5.0 E:\Program Files\DevStudio\SharedIDE
        MSDevDir = os.environ.get("MSDEVDIR")

        # 5.0
        MSVCDir = os.environ.get("MSVCDIR") # E:\Program Files\DevStudio\VC

        # 7.1 Express
        VCToolkitInstallDir = os.environ.get("VCTOOLKITINSTALLDIR") # E:\Program Files\Microsoft Visual C++ Toolkit 2003 (not set by vcvars32)

        # 8.0 Express
        # E:\Program Files\Microsoft Visual Studio 8\VC
        # E:\Program Files\Microsoft Visual Studio 8\Common7\Tools
        DevEnvDir = os.environ.get("DevEnvDir") # E:\Program Files\Microsoft Visual Studio 8\Common7\IDE
        VSInstallDir = os.environ.get("VSINSTALLDIR") # E:\Program Files\Microsoft Visual Studio 8
        # VS80CommonTools = os.environ.get("VS80COMNTOOLS") # E:\Program Files\Microsoft Visual Studio 8\Common7\Tools
        VCInstallDir = os.environ.get("VCINSTALLDIR") # E:\Program Files\Microsoft Visual Studio 8\VC

        # 9.0 Express
        # always, global
        #VS90COMNTOOLS=D:\msdev\90\Common7\Tools\
        # after running the shortcut
        #VCINSTALLDIR=D:\msdev\90\VC
        #VSINSTALLDIR=D:\msdev\90

        VSCommonTools = ""
        for a in os.environ:
            if a.startswith("VS") and a.endswith("COMNTOOLS"):
                candidateVer = int(a[2:-9])
                if not VSCommonTools or candidateVer > VSVer:
                    VSVer = candidateVer
                    VSCommonTools = os.environ[a]

        if VSCommonTools and not VSInstallDir:
            VSInstallDir = RemoveLastPathElement(RemoveLastPathElement(VSCommonTools))

        # The Windows SDK is carried with the express edition and tricky to find.
        # Best if folks just run the installed shortcut probably.
        # We do a pretty good job now of finding it, be need to encode
        # more paths to known versions.

        # This is not yet finished.
        #
        # Probe the partly version-specific less-polluting environment variables,
        # from newest to oldest.
        # That is, having setup alter PATH, INCLUDE, and LIB system-wide is not
        # a great idea, but having setup set DevEnvDir, VSINSTALLDIR, VS80COMNTOOLS, etc.
        # isn't so bad and we can temporarily establish the first set from the second
        # set.

        if VSInstallDir:
            # Visual C++ 2005/8.0, at least the Express Edition, free download
            # also Visual C++ 2008/9.0 Express Edition

            if not VCInstallDir:
                VCInstallDir = os.path.join(VSInstallDir, "VC")
                #print("VCInstallDir:" + VCInstallDir)
            if not DevEnvDir:
                DevEnvDir = os.path.join(VSInstallDir, "Common7", "IDE")
                #print("DevEnvDir:" + DevEnvDir)

            MspdbDir = DevEnvDir

        elif VCToolkitInstallDir:
            # free download Visual C++ 2003; no longer available

            VCInstallDir = VCToolkitInstallDir

        elif MSVCDir and MSDevDir:
            # Visual C++ 5.0

            pass # do more research
            # VCInstallDir = MSVCDir

        elif MSDevDir:
            # Visual C++ 4.0, 5.0

            pass # do more research
            # VCInstallDir = MSDevDir

        else:
            # This is what really happens on my machine, for 8.0.
            # It might be good to guide pylib.py to other versions,
            # however setting things up manually suffices and I have, um,
            # well automated.

            Msdev = os.path.join(SystemDrive, "msdev", "80")
            VCInstallDir = os.path.join(Msdev, "VC")
            DevEnvDir = os.path.join(Msdev, "Common7", "IDE")

        if VCInstallDir:
            VCBin = os.path.join(VCInstallDir, "bin")
            VCLib = os.path.join(VCInstallDir, "lib")
            VCInc = os.path.join(VCInstallDir, "include")

        if DevEnvDir:
            MspdbDir = DevEnvDir
        #elif VCBin:
        #    MspdbDir = VCBin

        # Look for SDKs.
        # expand this as they are released/discovered
        # ordering is from newest to oldest

        PossibleSDKs = [os.path.join("Windows Kits", "8.0"),
                        os.path.join("Microsoft SDKs", "Windows", "v7.0A"),
                        os.path.join("Microsoft SDKs", "Windows", "v6.0A"),
                        "Microsoft Platform SDK for Windows Server 2003 R2"
                       ]
        SDKs = []

        for a in GetProgramFiles():
            #print("checking " + a)
            for b in PossibleSDKs:
                c = os.path.join(a, b)
                #print("checking " + c)
                if isdir(c) and not (c in SDKs):
                    SDKs.append(c)
                    #print("SDKs.append(%s)" % c)

        # Make sure %INCLUDE% contains errno.h and windows.h.
        # This doesn't work correctly for Cygwin Python, ok.

        if _CheckSetupEnvironmentVariableAll("INCLUDE", ["errno.h", "windows.h"], VCInc):
            for a in SDKs:
                b = os.path.join(a, "include")
                c = os.path.join(a, "include", "um")
                d = os.path.join(a, "include", "shared")
                e = os.path.join(a, "include", "winrt")
                if (   isfile(os.path.join(b, "windows.h"))
                        or isfile(os.path.join(c, "windows.h"))
                        or isfile(os.path.join(d, "windows.h"))
                        or isfile(os.path.join(e, "windows.h"))):
                    _SetupEnvironmentVariableAll("INCLUDE", ["errno.h", "windows.h"], b + ";" + c + ";" + d + ";" + e + ";" + VCInc, ";")
                    break
                else:
                    pass # print("skipping %s" % a)

        #print(os.environ["INCLUDE"])
        #exit(1)

        # Make sure %LIB% contains kernel32.lib and libcmt.lib.
        # We carry our own kernel32.lib so we don't look in the SDKs.
        # We usually use msvcrt.lib and not libcmt.lib, but Express 2003 had libcmt.lib and not msvcrt.lib
        # I think, and libcmt.lib is always present.

        _SetupEnvironmentVariableAll(
            "LIB",
            ["kernel32.lib", "libcmt.lib"],
            VCLib + ";" + os.path.join(InstallRoot, "lib"))

        # Check that cl.exe and link.exe are in path, and if not, add VCBin to it,
        # checking that they are in it.
        #
        # Do this before mspdb*dll because it sometimes gets it in the path.
        # (Why do we care?)

        _SetupEnvironmentVariableAll("PATH", ["cl", "link"], VCBin)

        # If none of mspdb*.dll are in PATH, add MpsdbDir to PATH, and check that one of them is in it.
        # TODO generalize for any mspdb*dll.

        if True:
            _SetupEnvironmentVariableAny(
                "PATH",
                ["mspdbsrv.exe", "mspdbst.dll", "mspdbcore.dll",
                 "mspdb140.dll",
                 "mspdb110.dll", "mspdb100.dll", "mspdb90.dll",
                 "mspdb80.dll", "mspdb71.dll", "mspdb70.dll",
                 "mspdb60.dll", "mspdb50.dll", "mspdb41.dll", "mspdb40.dll",
                 "dbi.dll"],
                MspdbDir)

        # Try to get mt.exe in %PATH% if it isn't already.
        # We only need this for certain toolsets.

        if not SearchPath("mt.exe", os.environ.get("PATH")):
            for a in SDKs:
                b = os.path.join(a, "bin")
                if isfile(os.path.join(b, "mt.exe")):
                    SetEnvironmentVariable("PATH", os.environ.get("PATH") + os.pathsep + b)
                    break

        # sys.exit(1)

        # The free Visual C++ 2003 has neither delayimp.lib nor msvcrt.lib.
        # Very old toolsets have no delayimp.lib.
        # The Quake config file checks these environment variables.

        Lib = os.environ.get("LIB")
        if not SearchPath("delayimp.lib", Lib):
            os.environ["USE_DELAYLOAD"] = "0"
            print("set USE_DELAYLOAD=0")

        if not SearchPath("msvcrt.lib", Lib):
            os.environ["USE_MSVCRT"] = "0"
            print("set USE_MSVCRT=0")

    # some host/target confusion here..

    if IsMinGWHostTarget():

        _ClearEnvironmentVariable("LIB")
        _ClearEnvironmentVariable("INCLUDE")

        _SetupEnvironmentVariableAll(
            "PATH",
            ["gcc", "as", "ld"],
            os.path.join(SystemDrive, "mingw", "bin"))

        # need to probe for ld that accepts response files.
        # For example, this version does not:
        # C:\dev2\cm3\scripts\python>ld -v
        # GNU ld version 2.15.91 20040904
        # This comes with Qt I think (free Windows version)
        #
        # This version works:
        # C:\dev2\cm3\scripts\python>ld -v
        # GNU ld version 2.17.50 20060824

        # Ensure msys make is ahead of mingwin make, by adding
        # msys to the start of the path after adding mingw to the
        # start of the path. Modula-3 does not generally use
        # make, but this might matter when building m3cg, and
        # is usually the right thing.

        _SetupEnvironmentVariableAll(
            "PATH",
            ["sh", "sed", "gawk", "make"],
            os.path.join(SystemDrive, "msys", "1.0", "bin"))

    # some host/target confusion here..

    if IsCygwinHostTarget():

        #_ClearEnvironmentVariable("LIB")
        #_ClearEnvironmentVariable("INCLUDE")

        #if _HostIsNT:
        #    _SetupEnvironmentVariableAll(
        #        "PATH",
        #        ["cygX11-6.dll"],
        #        os.path.join(SystemDrive, "cygwin", "usr", "X11R6", "bin"))

        _SetupEnvironmentVariableAll(
            "PATH",
            ["gcc", "as", "ld"],
            os.path.join(SystemDrive, "cygwin", "bin"))

#-----------------------------------------------------------------------------

# ported from scripts/win/sysinfo.cmd
# not currently used

def CheckForLinkSwitch(Switch):
    EnvName = "USE_" + Switch
    EnvValue = "0"
    if os.system("link | findstr /i /c:\" /" + Switch + "\" >" + os.devnull) == 0:
        EnvValue = "1"
    os.environ[EnvName] = EnvValue
    print("set " + EnvName + "=" + EnvValue)

#-----------------------------------------------------------------------------

# packaging support

def InstallLicense(Root, InstallRoot):

    license = os.path.join(InstallRoot, "license")
    CreateDirectory(license)

    for a in glob.glob(os.path.join(Root, "COPYRIGHT*")):
        CopyFile(a, os.path.join(license, GetLastPathElement(a))) or FatalError()

    CopyFile(os.path.join(Root, "m3-libs", "arithmetic", "copyrite.txt"), os.path.join(license, "COPYRIGHT-M3NA")) or FatalError()
    CopyFile(os.path.join(Root, "m3-tools", "cvsup", "License"), os.path.join(license, "COPYRIGHT-JDP-CVSUP")) or FatalError()
    CopyFile(os.path.join(Root, "m3-sys", "COPYRIGHT-CMASS"), os.path.join(license, "COPYRIGHT-CMASS")) or FatalError()

    open(os.path.join(license, "COPYRIGHT-ELEGO-SYSUTILS"), "w").write(
"""Copyright 1999-2002 elego Software Solutions GmbH, Berlin, Germany.
All rights reserved.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions
are met:
1. Redistributions of source code must retain the above copyright
   notice, this list of conditions and the following disclaimer.
2. Redistributions in binary form must reproduce the above copyright
   notice, this list of conditions and the following disclaimer in the
   documentation and/or other materials provided with the distribution.

THIS SOFTWARE IS PROVIDED BY THE AUTHOR ``AS IS'' AND ANY EXPRESS OR
IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES
OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED.
IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY DIRECT, INDIRECT,
INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT
NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
(INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF
THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
""")

    open(os.path.join(license, "COPYRIGHT-OLIVETTI"), "w").write(
"""                      Copyright (C) Olivetti 1989
                          All Rights reserved

Use and copy of this software and preparation of derivative works based
upon this software are permitted to any person, provided this same
copyright notice and the following Olivetti warranty disclaimer are
included in any copy of the software or any modification thereof or
derivative work therefrom made by any person.

This software is made available AS IS and Olivetti disclaims all
warranties with respect to this software, whether expressed or implied
under any law, including all implied warranties of merchantibility and
fitness for any purpose. In no event shall Olivetti be liable for any
damages whatsoever resulting from loss of use, data or profits or
otherwise arising out of or in connection with the use or performance
of this software.
""")

    class State:
        pass

    state = State()
    state.id = 0

    def Callback(state, dir, entries):
        for a in entries:
            if a == "COPYRIGHT":
                state.id += 1
                CopyFile(os.path.join(dir, a), os.path.join(license, "COPYRIGHT-CALTECH-" + str(state.id)))

    os.path.walk(os.path.join(Root, "caltech-parser"), Callback, state)

def GetStage():
    global STAGE
    STAGE = ConvertPathForPython(getenv("STAGE"))

    if (not STAGE):
        #tempfile.tempdir = os.path.join(tempfile.gettempdir(), "cm3", "make-dist")
        #CreateDirectory(tempfile.tempdir)
        STAGE = tempfile.mkdtemp()
        SetEnvironmentVariable("STAGE", STAGE)
    return STAGE

# The way this SHOULD work is we build the union of all desired,
# and then pick and chose from the output into the .zip/.tar.bz2.
# For now though, we only build min.

def FormInstallRoot(PackageSetName):
    AltConfig = {"NT386":"x86"}.get(Config, Config)
    a = os.path.join(GetStage(), "cm3-" + PackageSetName + "-" + AltConfig + "-" + CM3VERSION)
    if Config == "NT386" or Config.endswith("_NT"):
        a = a + "-VC" + GetVisualCPlusPlusVersion()
    else:
        b = os.popen("uname -sr").read()
        b = re.sub("Linux 2\.4\..+$", "Linux2.4", b)
        b = re.sub("Linux 2\.6\..+$", "", b)
        b = re.sub("-.*$", "", b)
        b = re.sub("SunOS", "Solaris", b)
        b = re.sub("[ \r\n]", "", b)
        if len(b) > 0:
            a = a + "-" + b
    a = a + "-" + time.strftime("%Y%m%d")
    return a

def MakeMSIWithWix(input):
# input is a directory such as c:\stage1\cm3-min-NT386-d5.8.1-VC90
# The output goes to input + ".msi" and other temporary files go similarly (.wix, .wixobj)
# (We edit the filename slightly for friendlyness.)
    import uuid

    InstallLicense(Root, input)

    wix = open(input + ".wxs", "w")
    wix.write("""<?xml version='1.0' encoding='windows-1252'?>
<Wix xmlns='http://schemas.microsoft.com/wix/2006/wi'>
    <Product Name='Modula-3' Id='%s' Language='1033' Codepage='1252' Version='1.0.0' Manufacturer='.'>
        <Package Id='*' Keywords='.' Description="." Comments='.' Manufacturer='.' InstallerVersion='100' Languages='1033' Compressed='yes' SummaryCodepage='1252'/>
        <Media Id='1' Cabinet='Sample.cab' EmbedCab='yes'/>
        <Directory Id='TARGETDIR' Name='SourceDir'>
            <Directory Id='INSTALLDIR' Name='cm3'>""" % (str(uuid.uuid4()).upper()))

    class State:
        pass

    state = State()
    state.dirID = 0
    state.fileID = 0
    state.componentID = 0

    def HandleDir(state, dir):
        for a in os.listdir(dir):
            b = os.path.join(dir, a)
            if isdir(b):
                wix.write("""<Directory Id='d%d' Name='%s'>\n""" % (state.dirID, ConvertPathForWin32(a)))
                state.dirID += 1
                HandleDir(state, b) # recursion!
                wix.write("</Directory>\n")
            else:
                wix.write("""<Component Id='c%s' Guid='%s'>\n""" % (str(state.componentID), str(uuid.uuid4())))
                state.componentID += 1
                if state.componentID == 1:
                    wix.write("""<Environment Id="envPath" Action="set" Name="PATH" Part="last" Permanent="no" Separator=";" Value='[INSTALLDIR]bin'/>\n""")

                wix.write("""<File Id='f%d' Name='%s' Source='%s'/>\n""" % (state.fileID, a, ConvertPathForWin32(b)))
                state.fileID += 1
                wix.write("</Component>\n")

    HandleDir(state, input)

    wix.write("</Directory>\n")
    wix.write("</Directory>\n")

    wix.write("<Feature Id='Complete' Title='Modula-3' Description='everything' Display='expand' Level='1' ConfigurableDirectory='INSTALLDIR'>\n")

    for a in range(0, state.componentID):
        wix.write("<ComponentRef Id='c%d'/>\n" % a)

    # WixUI_Advanced
    # WixUI_Mondo
    # WixUI_InstallDir
    # WixUI_FeatureTree
    # are all good, but we need the package sets for some of them to make more sense

    wix.write("""
        </Feature>
        <Property Id="WIXUI_INSTALLDIR" Value="INSTALLDIR"/>
        <UIRef Id="WixUI_InstallDir" />
        <UIRef Id="WixUI_ErrorProgressText" />
    </Product>
</Wix>
""")

    wix.close()

    print("SearchPath candle light")

    if not SearchPath("candle") or not SearchPath("light"):
        for a in GetProgramFiles():
            b = os.path.join(ConvertPathForPython(a), "Windows Installer XML v3", "bin")
            if isdir(b):
                SetEnvironmentVariable("PATH", b + os.pathsep + os.environ["PATH"])
                break

    command = "candle " + ConvertPathForWin32(input) + ".wxs -out " + ConvertPathForWin32(input) + ".wixobj" + " 2>&1"
    if os.name == "posix":
        command = command.replace("\\", "\\\\")
    print(command)
    os.system(command)
    DeleteFile(input + ".msi")

    # This is similar to the toplevel README in the source tree.
    licenseText = \
"""The Critical Mass Modula-3 Software Distribution may be
freely distributed as open source according to the various
copyrights under which different parts of the sources are
placed. Please read the files found in the license directory."""

    license = input + "-license.rtf"
    open(license, "w").write(
"""{\\rtf1\\ansi\\deff0{\\fonttbl{\\f0\\fnil\\fcharset0 Courier New;}}
{\\*\\generator Msftedit 5.41.15.1515;}\\viewkind4\\uc1\\pard\\lang1033\\f0\\fs20""" + licenseText.replace("\n", " ")
+ "}")

    command = "light -out " + ConvertPathForWin32(input) + ".msi " + ConvertPathForWin32(input) + ".wixobj -ext WixUIExtension -cultures:en-us -dWixUILicenseRtf=" + ConvertPathForWin32(license) + " 2>&1"
    if os.name == "posix":
        command = command.replace("\\", "\\\\")
    print(command)
    os.system(command)

#MakeMSIWithWix("C:\\stage1\\cm3-min-NT386-d5.8.1")
#sys.exit(1)

def DiscoverHardLinks(r):
#
# return a hash of inode to array of paths
#
    result = { }
    for root, dirs, files in os.walk(r):
        for f in files:
            p = root + "/" + f
            if not os.path.islink(p):
                result.setdefault(os.stat(p).st_ino, []).append(p)
    return result

def BreakHardLinks(links):
#
# take the output from DiscoverHardLinks and replace all but the first
# element in each array with a zero size file
#
    for inode in links:
        first = links[inode][0]
        for other in links[inode][1:]:
            print("breaking link " + other + " <=> " + first)
            os.remove(ConvertPathForPython(other))
            open(ConvertPathForPython(other), "w")

def RestoreHardLinks(links):
#
# take the output of DiscoverHardLinks and reestablish the links
#
    for inode in links:
        first = links[inode][0]
        for other in links[inode][1:]:
            print("restoring link " + other + " <=> " + first)
            os.remove(ConvertPathForPython(other))
            os.link(ConvertPathForPython(first), ConvertPathForPython(other))

def MoveSkel(prefix):
#
# Move the usual toplevel cm3 directories under another directory.
# for example:
#  /tmp/stage/cm3-min/bin
#  /tmp/stage/cm3-min/lib
# =>
#  /tmp/stage/cm3-min/usr/local/cm3/bin
#  /tmp/stage/cm3-min/usr/local/cm3/lib
#
# This is temporarily destructive, for the purposes of building Debian packages.
# It is reversed by RestoreSkel.
#
    CreateDirectory("." + prefix)
    for a in ["bin", "pkg", "lib", "www", "man", "etc"]:
        if isdir(a):
            print("mv " + a + " ." + prefix + "/" + a)
            os.rename(a, "." + prefix + "/" + a)

def RestoreSkel(prefix):
#
# Undo the work of MoveSkel;
#
    for a in ["bin", "pkg", "lib", "www", "man", "etc"]:
        if isdir("." + prefix + "/" + a):
            print("mv ." + prefix + "/" + a + " " + a)
            os.rename("." + prefix + "/" + a, a)

# Debian architecture strings:
# see http://www.debian.org/doc/debian-policy/footnotes.html#f73

DebianArchitecture = {
  "LINUXLIBC6" : "i386",
  "FreeBSD4" : "i386",
  "NT386" : "i386",
  "I386" : "i386",
  "IA64" : "ia64",
  "ALPHA" : "alpha",
  "AMD64" : "amd64",
  "HPPA" : "hppa",
  "PA32" : "hppa",
  "PA64" : "hppa",
  "MIPS" : "mips",
  "MIPS32" : "mips",
  "MIPS64" : "mips",
  "PPC" : "powerpc",
  "PPC32" : "powerpc",
  "PPC64" : "ppc",
  "SOLsun" : "sparc",
  "SOLgnu" : "sparc",
  "SPARC" : "sparc",
  "SPARC32" : "sparc",
  "SPARC64" : "sparc",
  }

def MakeDebianPackage(input, prefix):
#
# .deb file format:
# an ar archive containing (I think the order matters):
#   debian-binary:
#     text file that just says "2.0\n"
#   control.tar.gz:
#     metadata, minimum is control file
#   data.tar.gz or .bz2 or .lzma
#     payload
# User has no choice where the install goes.
#
    if SearchPath("lzma"):
        compresser = "lzma"
        compressed_extension = "lzma"
    elif isfile("/home/jkrell/bin/lzma"):
        compresser = "/home/jkrell/bin/lzma"
        compressed_extension = "lzma"
    else:
        compresser = "gzip"
        compressed_extension = "gz"
    # while testing, gzip is much faster
    # compresser = "gzip"
    # compressed_extension = "gz"
    print("cd " + input)
    os.chdir(input)
    CreateDirectory("./debian")
    MoveSkel(prefix)
    newline = "\012" # take no chances
    open("./debian-binary", "w").write("2.0" + newline)
    os.chdir("./debian")
    architecture = DebianArchitecture.get(Target) or DebianArchitecture.get(Target[:Target.index("_")])
    control = (
      "Package: cm3-" + Target + "-" + CM3VERSION + newline
    + "Version: 1.0" + newline
    + "Maintainer: somebody@somewhere.com" + newline
    + "Architecture: " + architecture + newline
    + "Description: good stuff" + newline)
    print("control:" + control)
    open("./control", "w").write(control)

    command = "tar cf ../control.tar ."
    print(command)
    os.system(command)

    command = "gzip ../control.tar"
    print(command)
    os.system(command)

    os.chdir(input)
    command = "tar cf data.tar ." + prefix
    if isfile("data.tar." + compressed_extension) or isfile("data.tar"):
        print("skipping " + command)
    else:
        print(command)
        os.system(command)
    command = compresser + " data.tar"
    if isfile("data.tar." + compressed_extension):
        print("skipping " + command)
    else:
        print(command)
        os.system(command)

    command = "ar cr " + input + ".deb debian-binary control.tar.gz data.tar." + compressed_extension
    print(command)
    os.system(command)
    RestoreSkel(prefix)

#-----------------------------------------------------------------------------

if __name__ == "__main__":

    #
    # run test code if module run directly
    #

    print("GetVisualCPlusPlusVersion:" + GetVisualCPlusPlusVersion())
    sys.exit(1)

    print("CM3VERSION is " + GetVersion("CM3VERSION"))
    print("CM3VERSIONNUM is " + GetVersion("CM3VERSIONNUM"))
    print("CM3LASTCHANGED is " + GetVersion("CM3LASTCHANGED"))
    #sys.stdout.flush()
    #os.system("set")
    sys.exit(1)

    CopyConfigForDevelopment()
    sys.exit(1)

    CheckForLinkSwitch("DELAYLOAD")
    sys.exit(1)

    for a in [
            "a", "a/b", "a\\b", "\\a",
            "\\a/b", "\\a\\b", "/a", "/a/b",
            "/a\\b", "/a\\", "/a/b\\", "/a\\b\\",
            "ac", "a/bc", "a\\bc", "\\ac",
            "\\a/bc", "\\a\\bc", "/ac", "/a/bc",
            "/a\\bc", "/ac\\", "/a/bc\\", "/a\\bc\\",
            "a", "ac/b", "ac\\b", "\\a",
            "\\ac/b", "\\ac\\b", "/a", "/ac/b",
            "/ac\\b", "/a\\", "/ac/b\\", "/ac\\b\\",
            ]:
        print("RemoveLastPathElement(%s):%s" % (a, RemoveLastPathElement(a)))
        print("GetLastPathElement(%s):%s" % (a, GetLastPathElement(a)))
    sys.exit(1)

    print(ConvertFromCygwinPath("\\cygdrive/c/foo"))
    print(ConvertFromCygwinPath("//foo"))
    sys.exit(1)

    print(SearchPath("juno"))
    sys.exit(1)

    print(ConvertToCygwinPath("a"))
    print(ConvertToCygwinPath("a\\b"))
    print(ConvertToCygwinPath("//a\\b"))
    print(ConvertToCygwinPath("c:\\b"))
    print(ConvertToCygwinPath("c:/b"))
    print(ConvertToCygwinPath("/b"))
    print(ConvertToCygwinPath("\\b"))
    sys.exit(1)
    print(IsCygwinBinary("c:\\cygwin\\bin\\gcc.exe"))
    print(IsCygwinBinary("c:\\bin\\cdb.exe"))
    sys.exit(1)

    print("\n\ncore: " + str(OrderPackages(PackageSets["core"])))
    print("\n\nbase: " + str(OrderPackages(PackageSets["base"])))
    print("\n\nmin: " + str(OrderPackages(PackageSets["min"])))
    print("\n\nstd: " + str(OrderPackages(PackageSets["std"])))
    print("\n\nall: " + str(OrderPackages(PackageSets["all"])))
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
