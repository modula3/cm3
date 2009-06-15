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

env_OS = getenv("OS")
if env_OS == "Windows_NT":
    def uname():
        PROCESSOR_ARCHITECTURE = getenv("PROCESSOR_ARCHITECTURE")
        return (env_OS, "", PROCESSOR_ARCHITECTURE, "", PROCESSOR_ARCHITECTURE)
    #
    # cmd can run extensionless executables if this code is enabled.
    # This can be useful for example with NT386GNU following more Posix-ish
    # naming styles than even Cygwin usually does.
    #
    #pathext = getenv("PATHEXT");
    #if pathext and not "." in pathext.split(";"):
    #    pathext = ".;" + pathext
    #    os.environ["PATHEXT"] = pathext
    #    print("set PATHEXT=.;%PATHEXT%")
else:
    from os import uname

#-----------------------------------------------------------------------------

def FatalError(a = ""):
    # logs don't work yet
    #print("ERROR: see " + Logs)
    print("fatal error " + a)
    if __name__ != "__main__":
        sys.exit(1)

def GetLastPathElement(a):
    return a[max(a.rfind("/"), a.rfind("\\")) + 1:]

def RemoveLastPathElement(a):
    return a[0:max(a.rfind("/"), a.rfind("\\"))]

#-----------------------------------------------------------------------------
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
    (base, exts) = os.path.splitext(name)
    if not exts:
        exts = (getenv("PATHEXT") or "").lower()
    for ext in exts.split(";"):
        if ext == ".":
            ext = ""
        name = (base + ext)
        for path in paths.split(os.path.pathsep):
            candidate = os.path.join(path, name)
            if os.path.isfile(candidate):
                return os.path.abspath(candidate)

#-----------------------------------------------------------------------------

def _FormatEnvironmentVariable(Name):
    if os.name == "nt":
        return "%" + Name + "%"
    else:
        return "$" + Name

#-----------------------------------------------------------------------------

def _CheckSetupEnvironmentVariableAll(Name, RequiredFiles, Attempt):
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
                return "ERROR: " + File + " not found in " + _FormatEnvironmentVariable(Name) + "(" + NewValue + ")"
        os.environ[Name] = NewValue
        if Value:
            print(Name + "=" + Attempt + os.pathsep + _FormatEnvironmentVariable(Name))
        else:
            print(Name + "=" + Attempt)
    return None

def _SetupEnvironmentVariableAll(Name, RequiredFiles, Attempt):
    Error = _CheckSetupEnvironmentVariableAll(Name, RequiredFiles, Attempt)
    if Error:
        print(Error)
        if __name__ != "__main__":
            sys.exit(1)

#-----------------------------------------------------------------------------

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
    print("ERROR: " + _FormatEnvironmentVariable(Name) + " does not have any of " + " ".join(RequiredFiles))
    if __name__ != "__main__":
        sys.exit(1)

#-----------------------------------------------------------------------------

def _ClearEnvironmentVariable(Name):
    if Name in os.environ:
        del(os.environ[Name])
        print("set " + Name + "=")

#-----------------------------------------------------------------------------

def _MapTarget(a):
    #
    # Convert sensible names that the user might provide on the
    # command line into the legacy names other code knows about.
    #
    return {
        "I386_LINUX" : "LINUXLIBC6",
        "I386_NT" : "NT386",
        "I386_CYGWIN" : "NT386GNU",
        "I386_MINGW" : "NT386MINGNU",
        "PPC32_DARWIN" : "PPC_DARWIN",
        "PPC32_LINUX" : "PPC_LINUX",
        "I386_FREEBSD" : "FreeBSD4",
        "I386_NETBSD" : "NetBSD2_i386",

        #
        # both options sensible, double HP a bit redundant in the HPUX names
        #
        "HPPA32_HPUX"  : "PA32_HPUX",
        "HPPA64_HPUX"  : "PA64_HPUX",
        "HPPA32_LINUX" : "PA32_LINUX",
        "HPPA64_LINUX" : "PA64_LINUX",
    }.get(a) or a

#-----------------------------------------------------------------------------

def _GetAllTargets():

    # legacy naming

    Targets = [ "NT386", "NT386GNU", "NT386MINGNU", "LINUXLIBC6", "SOLsun", "SOLgnu", "FreeBSD4", "NetBSD2_i386" ]

    for proc in [ "PPC", ]:
        for os in [ "OPENBSD", "NETBSD", "FREEBSD", "DARWIN", "LINUX", "NT" ]:
            Targets += [proc + "_" + os]

    # systematic naming

    for proc in [ "I386", "AMD64", "PPC32", "PPC64", "ARM" ]:
        for os in [ "DARWIN" ]:
            Targets += [proc + "_" + os]

    for proc in [ "MIPS64", "I386", "AMD64", "PPC32", "PPC64", "SPARC32", "SPARC64" ]:
        for os in [ "OPENBSD", "NETBSD", "FREEBSD" ]:
            Targets += [proc + "_" + os]

    for proc in [ "MIPS64", "I386", "AMD64", "PPC32", "PPC64", "SPARC32", "SPARC64", "ARM", "PA32", "PA64"]:
        for os in [ "LINUX" ]:
            Targets += [proc + "_" + os]

    for proc in [ "MIPS32", "MIPS64" ]:
        for os in [ "IRIX" ]:
            Targets += [proc + "_" + os]

    for proc in [ "PPC32", "PPC64" ]:
        for os in [ "AIX" ]:
            Targets += [proc + "_" + os]

    for proc in [ "I386", "PPC32", "MIPS32", "ARM", "SH" ]:
        for os in [ "CE" ]:
            Targets += [proc + "_" + os]

    for proc in [ "I386", "AMD64", "IA64" ]:
        for os in [ "CYGWIN", "INTERIX", "NT" ]:
            Targets += [proc + "_" + os]

    for proc in [ "I386", "AMD64", "SPARC32", "SPARC64" ]:
        for os in [ "SOLARIS" ]:
            Targets += [proc + "_" + os]

    for proc in [ "PA32", "PA64", "IA64" ]:
        for os in [ "HPUX" ]:
            Targets += [proc + "_" + os]

    return Targets

#-----------------------------------------------------------------------------

CM3_FLAGS = ""
if "boot" in sys.argv:
    CM3_FLAGS = CM3_FLAGS + " -boot"
if "keep" in sys.argv:
    CM3_FLAGS = CM3_FLAGS + " -keep"

CM3 = getenv("CM3") or "cm3"
CM3 = SearchPath(CM3)

#-----------------------------------------------------------------------------
# the root of the installation
# This can be sniffed by finding cm3 in $PATH, else defaulted
# if the defaults contain a cm3.
#

InstallRoot = getenv("CM3_INSTALL")
# print("InstallRoot is " + InstallRoot)

if not CM3 and not InstallRoot:
    for a in ["c:\\cm3\\bin\\cm3.exe", "d:\\cm3\\bin\\cm3.exe", "/cm3/bin/cm3", "/usr/local/bin/cm3"]:
        if os.path.isfile(a):
            CM3 = a
            bin = os.path.dirname(CM3)
            print("using " + CM3)
            InstallRoot = os.path.dirname(bin)
            _SetupEnvironmentVariableAll("PATH", ["cm3"], bin)
            break;

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
#	Root = Root[2:]
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

#-----------------------------------------------------------------------------

Versions = {
    "CM3VERSION" : None,
    "CM3VERSIONNUM" : None,
    "CM3LASTCHANGED" : None,
    }

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

CM3VERSION = getenv("CM3VERSION") or GetVersion("CM3VERSION")
CM3VERSIONNUM = getenv("CM3VERSIONNUM") or GetVersion("CM3VERSIONNUM")
CM3LASTCHANGED = getenv("CM3LASTCHANGED") or GetVersion("CM3LASTCHANGED")

CM3_GDB = False

#-----------------------------------------------------------------------------
#
# nascent support for building via SRC or PM3, probably will never work
#

M3Build = getenv("M3BUILD") or "m3build"
M3Ship = getenv("M3SHIP") or "m3ship"


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

GCC_BACKEND = True

#-----------------------------------------------------------------------------
#
# Sniff to determine host.
#

UNameCommand = os.popen("uname").read().lower()
UNameTuple = uname()
UName = UNameTuple[0].lower()
UNameArchP = platform.processor().lower()
UNameArchM = UNameTuple[4].lower()
UNameRevision = UNameTuple[2].lower()

Host = None
TurnOffGarbageCollection = False

if (UName.startswith("windows")
        or UNameCommand.startswith("mingw")
        or UNameCommand.startswith("cygwin")):

    Host = "NT386"
    # Host = "I386_NT"

elif UName.startswith("freebsd"):

    if UNameArchM == "i386":
        if UNameRevision.startswith("1"):
            Host = "FreeBSD"
        elif UNameRevision.startswith("2"):
            Host = "FreeBSD2"
        elif UNameRevision.startswith("3"):
            Host = "FreeBSD3"
        else:
            Host = "FreeBSD4"
        # Host = "I386_FREEBSD"
    elif UNameArchM == "amd64":
        Host = "AMD64_FREEBSD"
    else:
        Host = "FBSD_ALPHA"
        # Host = "ALPHA64_FREEBSD"

elif UName.startswith("openbsd"):

    if UNameArchM == "sparc64":
        Host = "SPARC64_OPENBSD"
    elif UNameArchM == "macppc":
        Host = "PPC32_OPENBSD"
    elif UNameArchM == "i386":
        Host = "I386_OPENBSD"
    else:
        FatalError("unknown OpenBSD platform")

elif UName.startswith("darwin"):

    # detect the m3 platform (Darwin runs on ppc32, ppc64, x86, amd64)
    if UNameArchP == "powerpc":
        Host = "PPC_DARWIN"
    elif re.match("i[3456]86", UNameArchP):
        Host = "I386_DARWIN"
    elif UNameArchP == "x86-64":
        Host = "AMD64_DARWIN"
    elif UNameArchP == "powerpc64":
        Host = "PPC64_DARWIN"

elif UName.startswith("sunos"):

    Host = "SOLgnu"
    #Host = "SOLsun"
    #Host = "SPARC32_SOLARIS"

elif UName.startswith("linux"):

    if UNameArchM == "ppc":
        Host = "PPC_LINUX"
    elif UNameArchM == "x86_64":
        Host = "AMD64_LINUX"
    elif UNameArchM == "sparc64":
        Host = "SPARC32_LINUX"
    else:
        # Host = "I386_LINUX"
        Host = "LINUXLIBC6"

elif UName.startswith("netbsd"):

    # Host = "I386_NETBSD"
    Host = "NetBSD2_i386" # only arch/version combination supported yet

elif UName.startswith("irix"):

    Host = "MIPS32_IRIX"
    # later
    # if UName.startswith("irix64"):
    #   Host = "MIPS64_IRIX"

elif UName.startswith("hp-ux"):

    Host = "PA32_HPUX"
    #
    # no good way found to sniff for 64bit, not even from 64bit Python
    # user will have to say PA64_HPUX manually on the command line
    #

else:
    # more need to be added here, I haven't got all the platform info ready
    pass

#-----------------------------------------------------------------------------
#
# Target is:
#   - any parameter on the command line that is a platform
#   - CM3_TARGET environment variable
#   - defaults to host
#

Target = None
for a in _GetAllTargets():
    if a in sys.argv:
        Target = _MapTarget(a)
Target = Target or getenv("CM3_TARGET") or Host

#-----------------------------------------------------------------------------
#
# OSType is almost always POSIX, the user cannot set it, it is changed to WIN32 sometimes later
#

OSType = "POSIX"

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

    # q for quote: This is probably about the host, not the target.
    Q = ""

    HAVE_SERIAL = True

    #
    # TBD:
    # If cl is not in the path, or link not in the path (Cygwin link doesn't count)
    # then error toward GNU, and probe uname and gcc -v.
    #
    if Target == "NT386GNU":
        Config = "NT386GNU"
        OSType = "POSIX"

    elif Target == "NT386MINGNU":
        Config = "NT386MINGNU"
        OSType = "WIN32"

    else:
        Config = "NT386"
        OSType = "WIN32"
        GCC_BACKEND = False

    Target = "NT386"

elif Target.find("LINUX") != -1:
    #
    # Support for bootstrapping from older toolsets.
    # Expand and reduce this through time.
    # Latest PPC_LINUX release is 5.2.6 and it requires these flags.
    #
    if (Target.find("AMD64") == -1) and (Target.find("SPARC") == -1):
        GCWRAPFLAGS = "-Wl,--wrap,adjtime,--wrap,getdirentries,--wrap,readv,--wrap,utimes,--wrap,wait3"

#-----------------------------------------------------------------------------

GMAKE = "gmake"

if ((Host.find("DARWIN") != -1)
    or (Host.find("LINUX") != -1)
    or (Host.find("NT386") != -1)):

    GMAKE = "make"

else:
    pass

#-----------------------------------------------------------------------------

M3GDB = (M3GDB or CM3_GDB)
PKGSDB = os.path.join(os.path.dirname(os.path.dirname(os.path.abspath(__file__))), "PKGS")

#-----------------------------------------------------------------------------

def GetConfigForDistribution(Target):
#
# Favor the config-no-install directory, else fallback to config.
#
    a = os.path.join(Root, "m3-sys", "cminstall", "src")
    b = os.path.join(a, "config-no-install", Target)
    if os.path.isfile(b):
        return b
    b = os.path.join(a, "config", Target)
    # print("GetConfigForDistribution:" + b)
    return b

#-----------------------------------------------------------------------------

def SetEnvironmentVariable(Name, Value):
    if not os.environ.get(Name) or (os.environ[Name] != Value):
        os.environ[Name] = Value
        print("set " + Name + "=" + Value);

#-----------------------------------------------------------------------------

def IsCygwinBinary(a):
    if env_OS != "Windows_NT":
        return False
    if not os.path.isfile(a):
        FatalError(a + " does not exist")
    a = a.replace("/cygdrive/c/", "c:\\")
    a = a.replace("/cygdrive/d/", "d:\\")
    a = a.replace("/", "\\")
    #print("a is " + a)
    return (os.system("findstr 2>&1 >nul /m cygwin1.dll \"" + a + "\"") == 0)

#-----------------------------------------------------------------------------

def _ConvertToCygwinPath(a):
    if env_OS != "Windows_NT":
        return a
    if (a.find('\\') == -1) and (a.find(':') == -1):
        return a
    a = a.replace("\\", "/")
    if a.find(":/") == 1:
        a = "/cygdrive/" + a[0:1] + a[2:]
    return a

#-----------------------------------------------------------------------------

def _ConvertFromCygwinPath(a):
    if env_OS != "Windows_NT":
        return a
    a = a.replace("/", "\\")
    #a = a.replace("\\", "/")
    if (a.find("\\cygdrive\\") == 0):
        a = a[10] + ":" + a[11:]
    return a

#-----------------------------------------------------------------------------

if IsCygwinBinary(CM3):

    HostIsCygwin = True

    def ConvertToCygwinPath(a):
        return _ConvertToCygwinPath(a)

    def ConvertFromCygwinPath(a):
        return a

else:

    HostIsCygwin = False

    def ConvertToCygwinPath(a):
        return a

    def ConvertFromCygwinPath(a):
        return _ConvertFromCygwinPath(a)

#-----------------------------------------------------------------------------

def ConvertPath(a):
    # This isn't "roundtrip", this is one (or both) is a nop.
    b = ConvertFromCygwinPath(ConvertToCygwinPath(a))
    # print("converted " + a + " to " + b)
    return b

#-----------------------------------------------------------------------------
#
# reflect what we decided back into the environment
#

SetEnvironmentVariable("CM3_TARGET", Target);
SetEnvironmentVariable("CM3_INSTALL", ConvertPath(InstallRoot))
SetEnvironmentVariable("M3CONFIG", ConvertPath(os.environ.get("M3CONFIG") or GetConfigForDistribution(Config)))
#SetEnvironmentVariable("CM3_ROOT", ConvertPath(Root).replace("\\", "\\\\"))
SetEnvironmentVariable("CM3_ROOT", ConvertPath(Root).replace("\\", "/"))

# sys.exit(1)

#-----------------------------------------------------------------------------
# define build and ship programs for Critical Mass Modula-3
#

DEFS = "-DROOT=%(Q)s%(Root)s%(Q)s"
DEFS += " -DCM3_VERSION_TEXT=%(Q)s%(CM3VERSION)s%(Q)s"
DEFS += " -DCM3_VERSION_NUMBER=%(Q)s%(CM3VERSIONNUM)s%(Q)s"
DEFS += " -DCM3_LAST_CHANGED=%(Q)s%(CM3LASTCHANGED)s%(Q)s"

NativeRoot = Root
#Root = ConvertPath(Root).replace("\\", "\\\\")
Root = ConvertPath(Root).replace("\\", "/")
DEFS = (DEFS % vars())
Root = NativeRoot

#-----------------------------------------------------------------------------
# workaround crash when booting from 5.1.3 that is
# difficult to debug -- crashes earlier in debugger
# without this switch, no repro with this switch
#
# This has no effect with current tools/libraries.
#

DEFS += " @M3novm"

if TurnOffGarbageCollection:
    DEFS += " @M3nogc"

#
#
#

#-----------------------------------------------------------------------------
# Make sure these variables all start with a space if they are non-empty.
#

if BuildArgs:
    BuildArgs = " " + BuildArgs

if CleanArgs:
    CleanArgs = " " + CleanArgs

if ShipArgs:
    ShipArgs = " " + ShipArgs

#-----------------------------------------------------------------------------
# form the various commands we might run
#

CM3_BuildLocal = BuildLocal or "%(CM3)s %(CM3_FLAGS)s -build -override %(DEFS)s%(BuildArgs)s"
CM3_CleanLocal = CleanLocal or "%(CM3)s %(CM3_FLAGS)s -clean -build -override %(DEFS)s%(CleanArgs)s"
CM3_BuildGlobal = BuildGlobal or "%(CM3)s %(CM3_FLAGS)s -build %(DEFS)s%(BuildArgs)s"
CM3_CleanGlobal = CleanGlobal or "%(CM3)s %(CM3_FLAGS)s -clean %(DEFS)s%(CleanArgs)s"
CM3_Ship = Ship or "%(CM3)s %(CM3_FLAGS)s -ship %(DEFS)s%(ShipArgs)s"

#-----------------------------------------------------------------------------
# define build and ship programs for Poly. Modula-3 from Montreal
# This will not likely ever work, but maybe.
#

PM3_BuildLocal = BuildLocal or "%(M3Build)s -O %(DEFS)s%(BuildArgs)s"
PM3_CleanLocal = CleanLocal or "%(M3Build)s clean -O %(DEFS)s%(CleanArgs)s"
PM3_BuildGlobal = BuildGlobal or "%(M3Build)s %(DEFS)s %(BuildArgs)s)s"
PM3_CleanGlobal = CleanGlobal or "%(M3Build)s clean %(DEFS)s%(CleanArgs)s"
PM3_Ship = Ship or "%(M3Ship)s %(DEFS)s%(ShipArgs)s"

#-----------------------------------------------------------------------------
# define build and ship programs for DEC SRC Modula-3
# This will not likely ever work, but maybe.
#

SRC_BuildLocal = BuildLocal or "%(M3Build)s -O %(DEFS)s%(BuildArgs)s"
SRC_CleanLocal = CleanLocal or "%(M3Build)s clean -O %(DEFS)s%(CleanArgs)s"
SRC_BuildGlobal = BuildGlobal or "%(M3Build)s %(DEFS)s%(BuildArgs)s"
SRC_CleanGlobal = CleanGlobal or "%(M3Build)s clean %(DEFS)s%(CleanArgs)s"
SRC_Ship = Ship or "%(M3Ship)s %(DEFS)s%(ShipArgs)s"

# other commands

if os.name == "nt":
    RealClean = RealClean or "if exist %(Config)s rmdir /q/s %(Config)s"
else:
    RealClean = RealClean or "rm -rf %(Config)s"

RealClean = (RealClean % vars())

#-----------------------------------------------------------------------------
# choose the compiler to use
# pm3/dec/m3build is not tested and likely cm3 is all that works (heavily used)
#

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

#-----------------------------------------------------------------------------

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
            if Directory.find("examples/web") != -1:
                return
            if Directory.find("examples\\web") != -1:
                return
            if not "m3makefile" in Names:
                return
            if not os.path.isfile(os.path.join(Directory, "m3makefile")):
                return
            Result.append(Directory[len(Root) + 1:-4].replace('\\', "/") + "\n")

        print("making " + PKGSDB + ".. (slow but rare)")
        Result = [ ]

        os.path.walk(Root, Callback, Result)

        Result.sort()
        open(PKGSDB, "w").writelines(Result)

        if not os.path.isfile(PKGSDB):
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
    a = a.replace('\\', '/')
    ReadPackageDB()
    b = ('/' + a).lower()
    for i in PackageDB:
        if i.lower().endswith(b):
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

def _MakeArchive(a):
    #
    # OpenBSD doesn't have bzip2 in base, so use gzip instead.
    #
    DeleteFile(a + ".tar.gz")
    b = "tar cfz " + a + ".tar.gz " + a
    print(b + "\n");
    os.system(b)

#-----------------------------------------------------------------------------

def Boot():

    global BuildLocal
    BuildLocal += " -boot -keep "

    Version = "1"

    # This information is duplicated from the config files.
    # TBD: put it only in one place.
    # The older bootstraping method does get that right.

    GnuCompile = "gcc -g -fPIC "
    SunCompile = "cc -g -mt -xcode=pic32 -xldscope=symbolic "

    Compile = {
        "SOLsun"          : SunCompile,
        "SPARC64_SOLARIS" : SunCompile,
        }.get(Target) or GnuCompile

    Compile = Compile + ({
        "AMD64_LINUX"     : " -m64 -mno-align-double ",
        "ARM_DARWIN"      : " -march=armv6 -mcpu=arm1176jzf-s ",
        "LINUXLIBC6"      : " -m32 -mno-align-double ",
        "MIPS64_OPENBSD"  : " -mabi=64 ",
        "SOLsun"          : " -xarch=v8plus ",
        "SPARC32_LINUX"   : " -m32 -munaligned-doubles ",
        "SPARC64_LINUX"   : " -m64 -munaligned-doubles ",
        "SPARC64_SOLARIS" : " -xarch=v9 ",
        }.get(Target) or " ")

    SunLink = " -lrt -lm -lnsl -lsocket -lpthread "

    Link = Compile + ({
        "ARM_DARWIN"      : " ",
        "AMD64_DARWIN"    : " ",
        "I386_DARWIN"     : " ",
        "PPC_DARWIN"      : " ",
        "PPC64_DARWIN"    : " ",
        # SOLgnu?
        "SOLsun"          : SunLink,
        "SPARC64_SOLARIS" : SunLink,
        "PA32_HPUX"       : " -lrt -lm ",
        }.get(Target) or " -lm -lpthread ")

    # not in Link
    Compile += " -c "

    Assemble = ("as " + ({
        "AMD64_LINUX"       : " --64 ",
        "ARM_DARWIN"        : " -arch armv6 ",
        "LINUXLIBC6"        : " --32 ",
        "SPARC32_LINUX"     : " -32 ",
        "SPARC64_LINUX"     : " -64 ",
        "SPARC64_LINUX"     : " -64 -Av9 ",
        "SOLsun"            : " -s -K PIC -xarch=v8plus ",
        "SPARC64_SOLARIS"   : " -s -K PIC -xarch=v9 ",
        }.get(Target) or ""))

    GnuPlatformPrefix = {
        "ARM_DARWIN"      : "arm-apple-darwin8-",
        }.get(Target) or ""

    Compile = GnuPlatformPrefix + Compile
    Link = GnuPlatformPrefix + Link
    Assemble = GnuPlatformPrefix + Assemble

    #
    # squeeze runs of spaces and spaces at end
    #
    Compile = re.sub("  +", " ", Compile)
    Compile = re.sub(" +$", "", Compile)
    Link = re.sub("  +", " ", Link)
    Link = re.sub(" +$", "", Link)
    Assemble = re.sub("  +", " ", Assemble)
    Assemble = re.sub(" +$", "", Assemble)

    BootDir = "./cm3-boot-" + Target + "-" + Version

    P = [ "import-libs", "m3core", "libm3", "sysutils", "m3middle", "m3quake",
          "m3objfile", "m3linker", "m3back", "m3front", "cm3" ]
    if Target == "NT386":
        P += ["mklib"]

    #DoPackage(["", "realclean"] + P) or sys.exit(1)
    DoPackage(["", "buildlocal"] + P) or sys.exit(1)
        
    try:
        shutil.rmtree(BootDir)
    except:
        pass
    try:
        os.mkdir(BootDir)
    except:
        pass

    #
    # This would probably be a good use of XSL (xml style sheets)
    #
    Make = open(os.path.join(BootDir, "make.sh"), "wb")
    Makefile = open(os.path.join(BootDir, "Makefile"), "wb")
    UpdateSource = open(os.path.join(BootDir, "updatesource.sh"), "wb")

    Makefile.write(".SUFFIXES:\nall: cm3\n\n")

    for a in [UpdateSource, Make]:
        a.write("#!/bin/sh\n\nset -e\nset -x\n\n")

    for a in [Makefile]:
        a.write("# edit up here\n\n")
        a.write("Assemble=" + Assemble + "\nCompile=" + Compile + "\nLink=" + Link + "\n")
        a.write("\n\n# no more editing should be needed\n\n")

    for a in [Make]:
        a.write("Assemble=\"" + Assemble + "\"\nCompile=\"" + Compile + "\"\nLink=\"" + Link + "\"\n")

    for q in P:
        dir = GetPackagePath(q)
        for a in os.listdir(os.path.join(Root, dir, Config)):
            if (a.endswith(".ms") or a.endswith(".is") or a.endswith(".s") or a.endswith(".c")):
                CopyFile(os.path.join(Root, dir, Config, a), BootDir)
                Makefile.write("Objects += " + a + ".o\n" + a + ".o: " + a + "\n\t")
                if a.endswith(".c"):
                    Command = "Compile"
                else:
                    Command = "Assemble"
                for b in [Make, Makefile]:
                    b.write("${" + Command + "} " + a + " -o " + a + ".o\n")
            if a.endswith(".h"):
                CopyFile(os.path.join(Root, dir, Config, a), BootDir)

    Makefile.write("cm3: $(Objects)\n\t")    
    for a in [Make, Makefile]:
        a.write("$(Link) -o cm3 *.o\n")

    Common = "Common"

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
            "m3-libs/m3core/src/C/" + Common + "/m3makefile",
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
            "m3-libs/m3core/src/unix/linux-32/m3makefile",
            "m3-libs/m3core/src/unix/linux-64/m3makefile",
            "m3-libs/m3core/src/unix/freebsd-common/m3makefile",
            "m3-libs/m3core/src/unix/freebsd-common/Uerror.i3",
            "m3-libs/m3core/src/unix/freebsd-common/Usysdep.i3",
            "m3-libs/m3core/src/unix/freebsd-common/Uucontext.i3",
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
            "m3-libs/m3core/src/C/" + Common + "/Csetjmp.i3",
            "m3-libs/m3core/src/C/" + Common + "/Csignal.i3",
            "m3-libs/m3core/src/C/" + Common + "/Cstdio.i3",
            "m3-libs/m3core/src/C/" + Common + "/Cstring.i3",
            "m3-libs/m3core/src/C/" + Common + "/m3makefile",
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

    for a in [UpdateSource, Make, Makefile]:
        a.close()

    _MakeArchive(BootDir[2:])

#-----------------------------------------------------------------------------
# map action names to code and possibly other data

ActionInfo = {
    "build":
    {
        "Commands": [_BuildLocalFunction],
    },
    "buildlocal":
    {
        "Commands": [_BuildLocalFunction],
    },
    "buildglobal":
    {
        "Commands": [_BuildGlobalFunction, _ShipFunction],
    },
    "buildship":
    {
        "Commands": [_BuildGlobalFunction, _ShipFunction],
    },
    "ship":
    {
        "Commands": [_ShipFunction],
    },
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

BuildAll = getenv("CM3_ALL") or False

def _FilterPackage(Package):
    PackageConditions = {
        "m3gdb":
            (M3GDB or CM3_GDB) and
            {"FreeBSD4": True,
            "LINUXLIBC6" : True,
            "SOLgnu" : True,
            "NetBSD2_i386" : True,
            "NT386GNU" : True,
            }.get(Target, False),

        "fix_nl": BuildAll or OSType == "WIN32",
        "tcl": BuildAll or HAVE_TCL,
        "tapi": BuildAll or OSType == "WIN32",
        "serial": BuildAll or HAVE_SERIAL,
        "X11R4": BuildAll or OSType != "WIN32",
        "m3cc": (GCC_BACKEND and (not OMIT_GCC) and (not "skipgcc" in sys.argv) and (not "omitgcc" in sys.argv) and (not "nogcc" in sys.argv)),
    }
    return PackageConditions.get(Package, True)

#-----------------------------------------------------------------------------

def FilterPackages(Packages):
    Packages = filter(_FilterPackage, Packages)
    return Packages

#-----------------------------------------------------------------------------

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

    "std" :
        [
    # demo programs

        "cube",
        "calculator",
        "fisheye",
        "mentor",

    # base libraries

        "import-libs",
        "libm3",
        "windowsResources",
        "sysutils",
        "patternmatching",
        "m3core",

    # system / compiler libraries and tools

        "m3quake",
        "m3middle",
        "m3scanner",
        "m3tools",
        "m3cgcat",
        "m3cggen",
        "m3cc",
        "m3objfile",
        "m3linker",
        "m3back",
        "m3staloneback",
        "m3front",
        "cm3",
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
        "obliqlib3D",
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

        # The Juno-2 graphical constraint based editor

        "pkl-fonts",
        "juno-machine",
        "juno-compiler",
        "juno-app",

        "deckscape",
        "webscape",
        "webcat",
        ],


    "all": # in order
        [
    # backend
        "m3cc",

    # base libraries

        "import-libs",
        "m3core",
        "libm3",
        "windowsResources",
        "sysutils",
        "patternmatching",

    # system / compiler libraries and tools

        "m3middle",
        "m3objfile",
        "m3linker",
        "m3back",
        "m3staloneback",
        "m3front",
        "m3quake",
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
        "deepcopy",
        "sgml",
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
        "obliqlib3D",
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

        "cit_common",
        "m3tmplhack",
        "cit_util",
        #"term",
        "deepcopy",
        #"paneman",
        #"paneman/kemacs",
        "drawcontext",
        #"drawcontext/dcpane",
        #"drawcontext/kgv",
        "hack",
        "m3browserhack",
        "parserlib/ktoklib",
        "parserlib/klexlib",
        "parserlib/kyacclib",
        "parserlib/ktok",
        #"parserlib/klex",
        #"parserlib/kyacc",
        "parserlib/kext",
        "parserlib/parserlib",
        #"parserlib/parserlib/test",
        #"pp",
        #"kate",
        "sgml",

        "deckscape",
        "webscape",
        "webcat",
        ],
}

PackageSets_CoreBaseCommon = [
    "import-libs",
    "m3core",
    "libm3",
    "windowsResources",
    "sysutils",
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

    # print("args is " + str(args))
    # sys.stdout.flush()

    if not (PackagesFromCaller is None):
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
    AllTargets = _GetAllTargets()
    for arg in args[1:]:
        if ((arg == "")
            or (arg in AllTargets)
            or (arg == "keep")
            or (arg == "noclean")
            or (arg == "nocleangcc")
            or (arg == "nogcc")
            or (arg == "skipgcc")
            or (arg == "omitgcc")
            or (arg == "boot")):
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

        q = GetPackagePath(p)
        if not q:
            File = __file__
            sys.stderr.write("%(File)s *** cannot find package %(p)s\n" % vars())
            sys.exit(1)

        q = os.path.join(Root, q)
        if os.path.isfile(os.path.join(q, "src", "m3makefile")):
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
    if os.path.isfile(a):
        os.chmod(a, 0700)
        os.remove(a)

#-----------------------------------------------------------------------------

def CreateDirectory(a):
    if os.name != "nt":
        print("mkdir -p " + a)
    else:
        print("mkdir " + a)
    if not os.path.isdir(a):
        os.makedirs(a)
    return True

#-----------------------------------------------------------------------------

def MakeTempDir():
    if getenv("TEMP") and not os.path.exists(getenv("TEMP")):
        CreateDirectory(getenv("TEMP"))

MakeTempDir()

#-----------------------------------------------------------------------------

def FileExists(a):
    return os.path.isfile(a)

#-----------------------------------------------------------------------------

def CopyFile(From, To):
    if os.path.isdir(To):
        To = os.path.join(To, os.path.basename(From))
    # Cygwin says foo exists when only foo.exe exists, and then remove fails.
    if os.path.isfile(To):
        try:
            os.remove(To)
        except:
            pass
    CopyCommand = "copy"
    if os.name != "nt":
        CopyCommand = "cp -Pv"
    print(CopyCommand + " " + From + " " + To)
    shutil.copy(From, To)
    return True

#-----------------------------------------------------------------------------

def CopyFileIfExist(From, To):
    if os.path.isfile(From):
        return CopyFile(From, To)
    return True

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

    #
    # First delete all the "distribution config files".
    #
    a = os.path.join(Root, "m3-sys", "cminstall", "src")

    for b in ["config", "config-no-install"]: 
        for File in glob.glob(os.path.join(a, b, "*")):
            if os.path.isfile(File):
                DeleteFile(os.path.join(To, os.path.basename(File)))

    CopyFile(os.path.join(Root, a, "config", "cm3.cfg"), To) or FatalError()
    return True

#-----------------------------------------------------------------------------

#def CopyDirectoryNonRecursive(From, To):
#    CreateDirectory(To)
#    for File in glob.glob(os.path.join(From, "*")):
#        if os.path.isfile(File):
#            print(File + " => " + To + "\n")
#            CopyFile(File, To)
#    return True

#-----------------------------------------------------------------------------

def CopyConfigForDistribution(To):
    #
    # The distributed environment is minimal and targets only one
    # or a small number of targets (e.g. NT386*).
    #
    # The distributed environment does not require a source tree.
    #
    a = os.path.join(Root, "m3-sys", "cminstall", "src")
    To = os.path.join(To, "bin")
    for File in glob.glob(os.path.join(a, "config", Target + "*")):
        if os.path.isfile(File):
            #print(File + " => " + To + "\n")
            CopyFile(File, To)
    for File in glob.glob(os.path.join(a, "config-no-install", Target + "*")):
        if os.path.isfile(File):
            #print(File + " => " + To + "\n")
            CopyFile(File, To)
    for File in glob.glob(os.path.join(a, "config-no-install", "*.common")):
        if os.path.isfile(File):
            #print(File + " => " + To + "\n")
            CopyFile(File, To)
    open(os.path.join(To, "cm3.cfg"), "w").write("include(\"" + Config + "\")\n")
    return True

#-----------------------------------------------------------------------------

def _CopyCompiler(From, To):

    CreateDirectory(To)

    from_cm3 = os.path.join(From, "cm3")
    from_cm3exe = os.path.join(From, "cm3.exe")
    from_cm3cg = os.path.join(From, "cm3cg")
    from_cm3cgexe = os.path.join(From, "cm3cg.exe")

    if (Config != "NT386"
            and not FileExists(from_cm3)
            and not FileExists(from_cm3exe)
            and not FileExists(from_cm3cg)
            and not FileExists(from_cm3cgexe)):
        FatalError("none of " + from_cm3 + ", " + from_cm3exe + ", " + from_cm3cg + ", " + from_cm3cgexe + " exist")

    #
    # check .exe first to avoid being fooled by Cygwin
    #
    if FileExists(from_cm3exe):
        from_cm3 = from_cm3exe
    elif FileExists(from_cm3):
        pass
    else:
        from_cm3 = None
        from_cm3exe = None

    if from_cm3:
        #
        # delete .exe first to avoid being fooled by Cygwin
        #
        DeleteFile(os.path.join(To, "cm3.exe"))
        DeleteFile(os.path.join(To, "cm3"))
        CopyFile(from_cm3, To) or FatalError("3")
        CopyFileIfExist(os.path.join(From, "cm3.pdb"), To) or FatalError("5")

    if FileExists(from_cm3cgexe):
        from_cm3cg = from_cm3cgexe
    elif FileExists(from_cm3cg):
        pass
    else:
        from_cm3cg = None
        from_cm3cgexe = None
    if from_cm3cg:
        #
        # delete .exe first to avoid being fooled by Cygwin
        #
        DeleteFile(os.path.join(To, "cm3cg.exe"))
        DeleteFile(os.path.join(To, "cm3cg"))
        CopyFile(from_cm3cg, To) or FatalError("4")

    return True

#-----------------------------------------------------------------------------

def ShipBack():
    return _CopyCompiler(os.path.join(Root, "m3-sys", "m3cc", Config),
                         os.path.join(InstallRoot, "bin"))

#-----------------------------------------------------------------------------

def ShipFront():
    return _CopyCompiler(os.path.join(Root, "m3-sys", "cm3", Config),
                         os.path.join(InstallRoot, "bin"))

#-----------------------------------------------------------------------------

def ShipCompiler():
    return ShipBack() and ShipFront()

#-----------------------------------------------------------------------------

def CopyMklib(From, To):
    #
    # Copy mklib from one InstallRoot to another, possibly having cleaned out the intermediate directories.
    #
    From = os.path.join(From, "bin")
    To = os.path.join(To, "bin")
    CreateDirectory(To)

    mklib = os.path.join(From, "mklib")
    mklibexe = os.path.join(From, "mklib.exe")
    if FileExists(mklibexe):
        mklib = mklibexe
    elif FileExists(mklib):
        pass
    else:
        FatalError("neither " + mklib + " nor " + mklibexe + " exist")

    # important to delete mklib.exe ahead of mklib for Cygwin
    DeleteFile(os.path.join(To, "mklib.exe"))
    DeleteFile(os.path.join(To, "mklib"))

    CopyFile(mklib, To) or FatalError("6")

    CopyFileIfExist(os.path.join(From, "mklib.pdb"), To) or FatalError("8")
    return True

#-----------------------------------------------------------------------------

def CopyCompiler(From, To):
    #
    # Copy the compiler from one InstallRoot to another, possibly having cleaned out the intermediate directories.
    # The config file always comes right out of the source tree.
    #
    _CopyCompiler(os.path.join(From, "bin"), os.path.join(To, "bin"))
    CopyMklib(From, To) or FatalError("9")
    return True

#-----------------------------------------------------------------------------
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
        HostIsNT = True
    else:
        HostIsNT = False

    SystemDrive = os.environ.get("SYSTEMDRIVE")
    if SystemDrive:
        SystemDrive += os.path.sep

    #
    # Do this earlier so that its link isn't a problem.
    # Looking in the registry HKEY_LOCAL_MACHINE\SOFTWARE\Cygnus Solutions\Cygwin\mounts v2
    # would be reasonable here.
    #
    if HostIsCygwin:
        _SetupEnvironmentVariableAll(
            "PATH",
            ["cygwin1.dll"],
            os.path.join(SystemDrive, "cygwin", "bin"))

    #
    # some host/target confusion here..
    #
    if Target == "NT386" and HostIsNT and Config == "NT386" and (not GCC_BACKEND) and OSType == "WIN32":

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

		# 9.0 Express
		# always, global
		#VS90COMNTOOLS=D:\msdev\90\Common7\Tools\
		# after running the shortcut
		#VCINSTALLDIR=D:\msdev\90\VC
		#VSINSTALLDIR=D:\msdev\90
		#
		# The Windows SDK is carried with the express edition and tricky to find.
		# Best if folks just run the installed shortcut probably.
		#

        #
        # This is not yet finished.
        #
        # Probe the partly version-specific less-polluting environment variables,
        # from newest to oldest.
        # That is, having setup alter PATH, INCLUDE, and LIB system-wide is not
        # a great idea, but having setup set DevEnvDir, VSINSTALLDIR, VS80COMNTOOLS, etc.
        # isn't so bad and we can temporarily establish the first set from the second
        # set.
        #
        if VSInstallDir:
            #
            # Visual C++ 2005/8.0, at least the Express Edition, free download
            #
            if not VCInstallDir:
                VCInstallDir = os.path.join(VSInstallDir, "VC")
            if not DevEnvDir:
                DevEnvDir = os.path.join(VSInstallDir, "Common7", "IDE")

            MspdbDir = DevEnvDir

        elif VCToolkitInstallDir:
            #
            # free download Visual C++ 2003; no longer available
            #
            VCInstallDir = VCToolkitInstallDir

        elif MSVCDir and MSDevDir:
            #
            # Visual C++ 5.0
            #
            pass # do more research
            # VCInstallDir = MSVCDir

        elif MSDevDir:
            #
            # Visual C++ 4.0, 5.0
            #
            pass # do more research
            # VCInstallDir = MSDevDir

        else:
            #
            # This is what really happens on my machine, for 8.0.
            # It might be good to guide pylib.py to other versions,
            # however setting things up manually suffices and I have, um,
            # well automated.
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
        #elif VCBin:
        #    MspdbDir = VCBin

        if _CheckSetupEnvironmentVariableAll("INCLUDE", ["errno.h", "windows.h"], VCInc):
            a = os.path.join(SystemDrive, "Program Files")
            b = os.path.join(a, "Microsoft Platform SDK for Windows Server 2003 R2", "Include")
            c = os.path.join(a, "Microsoft SDKs", "Windows", "v6.0A", "Include")
            _SetupEnvironmentVariableAll("INCLUDE", ["errno.h", "windows.h"], VCInc + ";" + c + ";" + b)

        _SetupEnvironmentVariableAll(
            "LIB",
            ["kernel32.lib", "libcmt.lib"],
            VCLib + os.path.pathsep + os.path.join(InstallRoot, "lib"))

        #
        # Do this before mspdb*dll because it sometimes gets it in the path.
        #
        _SetupEnvironmentVariableAll("PATH", ["cl", "link"], VCBin)

        _SetupEnvironmentVariableAny(
            "PATH",
            ["mspdb80.dll", "mspdb71.dll", "mspdb70.dll", "mspdb60.dll", "mspdb50.dll", "mspdb41.dll", "mspdb40.dll", "dbi.dll"],
            MspdbDir)

        _SetupEnvironmentVariableAny(
            "PATH",
            ["msobj80.dll", "msobj71.dll", "msobj10.dll", "msobj10.dll", "mspdb50.dll", "mspdb41.dll", "mspdb40.dll", "dbi.dll"],
            MspdbDir)

        #
        # The free Visual C++ 2003 has neither delayimp.lib nor msvcrt.lib.
        # Very old toolsets have no delayimp.lib.
        # The Quake config file checks these environment variables.
        #
        Lib = os.environ.get("LIB")
        if not SearchPath("delayimp.lib", Lib):
            os.environ["USE_DELAYLOAD"] = "0"
            print("set USE_DELAYLOAD=0")

        if not SearchPath("msvcrt.lib", Lib):
            os.environ["USE_MSVCRT"] = "0"
            print("set USE_MSVCRT=0")

    #
    # some host/target confusion here..
    #
    if Target == "NT386MINGNU" or (Target == "NT386" and GCC_BACKEND and OSType == "WIN32"):

        _ClearEnvironmentVariable("LIB")
        _ClearEnvironmentVariable("INCLUDE")

        _SetupEnvironmentVariableAll(
            "PATH",
            ["gcc", "as", "ld"],
            os.path.join(SystemDrive, "mingw", "bin"))

        #
        # need to probe for ld that accepts response files.
        # For example, this version does not:
        # C:\dev2\cm3\scripts\python>ld -v
        # GNU ld version 2.15.91 20040904
        # This comes with Qt I think (free Windows version)
        #
        # This version works:
        # C:\dev2\cm3\scripts\python>ld -v
        # GNU ld version 2.17.50 20060824

        #
        # Ensure msys make is ahead of mingwin make, by adding
        # msys to the start of the path after adding mingw to the
        # start of the path. Modula-3 does not generally use
        # make, but this might matter when building m3cg, and
        # is usually the right thing.
        #
        _SetupEnvironmentVariableAll(
            "PATH",
            ["sh", "sed", "gawk", "make"],
            os.path.join(SystemDrive, "msys", "1.0", "bin"))

    #
    # some host/target confusion here..
    #
    if Target == "NT386GNU" or (Target == "NT386" and GCC_BACKEND and OSType == "POSIX"):

        #_ClearEnvironmentVariable("LIB")
        #_ClearEnvironmentVariable("INCLUDE")

        #if HostIsNT:
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
    if os.system("link | findstr /i /c:\" /" + Switch + "\" > nul") == 0:
        EnvValue = "1"
    os.environ[EnvName] = EnvValue
    print("set " + EnvName + "=" + EnvValue)

#-----------------------------------------------------------------------------

if __name__ == "__main__":

    #
    # run test code if module run directly
    #

    print("CM3VERSION is " + GetVersion("CM3VERSION"))
    print("CM3VERSIONNUM is " + GetVersion("CM3VERSIONNUM"))
    print("CM3LASTCHANGED is " + GetVersion("CM3LASTCHANGED"))
    #sys.stdout.flush()
    #os.system("set")
    sys.exit(1)

    CopyConfigForDevelopment();
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

    print(_ConvertFromCygwinPath("\\cygdrive/c/foo"))
    print(_ConvertFromCygwinPath("//foo"))
    sys.exit(1)

    print(SearchPath("juno"));
    sys.exit(1)

    print(_ConvertToCygwinPath("a"));
    print(_ConvertToCygwinPath("a\\b"));
    print(_ConvertToCygwinPath("//a\\b"));
    print(_ConvertToCygwinPath("c:\\b"));
    print(_ConvertToCygwinPath("c:/b"));
    print(_ConvertToCygwinPath("/b"));
    print(_ConvertToCygwinPath("\\b"));
    sys.exit(1)
    print(IsCygwinBinary("c:\\cygwin\\bin\\gcc.exe"))
    print(IsCygwinBinary("c:\\bin\\cdb.exe"))
    sys.exit(1)

    print("\n\ncore: " + str(OrderPackages(PackageSets["core"])))
    print("\n\nbase: " + str(OrderPackages(PackageSets["base"])))
    print("\n\nmin: " + str(OrderPackages(PackageSets["min"])))
    print("\n\nstd: " + str(OrderPackages(PackageSets["std"])))
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
