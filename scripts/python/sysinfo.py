import sys
import platform
import os;
from os import getenv, uname
from os.path import isfile, isdir, join, abspath, dirname, pathsep

#
# our exports / globals
#
# Some of these are set by other modules.
#
# In order to support the sh semantics, here we list
# every variable the sh scripts read/write, and set them
# to the environment variable, if set, else empty string.
#
Globals = [

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
    
    #
    # grep to use in building pkgsdb
    # This will be unused in the Python scripts.
    #
    "GREP",

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
    # path to the pkgsdb file
    # usually __file__/../pkgsdb
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
    "USAGE",
]

__all__ = Globals + [
    #"ExeName",
    "SearchPath",
    ]

#
# Ensure all exports have some value.
#
b = ""
for a in Globals:
    b += ("%s = getenv(\"%s\") or \"\"\n" % (a, a))
exec(b)

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

uname_tuple = os.uname()
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

CM3VERSION =  getenv("CM3VERSION") or "d5.5.0"
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
    sys.stderr.write("please define TMPDIR\n")
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
    return b

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

TARGET = getenv("TARGET") or ""

if (UNAME.startswith("Windows")
        or UNAME.startswith("WinNT")
        or UNAME.startswith("Cygwin")
        or UNAME.startswith("CYGWIN")
        or TARGET.startswith("NT386GNU")
    ):

    if (TARGET.startswith("NT386GNU")):

        CM3_OSTYPE = "POSIX"
        CM3_TARGET = "NT386GNU"
        GMAKE = getenv("GMAKE") or "make"

    else:

        CM3_OSTYPE = "WIN32"
        CM3_TARGET = "NT386"
        CM3_INSTALL = "c:/cm3"
        CM3_GCC_BACKEND = False
        HAVE_SERIAL = True
        EXE = ".exe"
        SL = "\\\\"
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

        def cygpath(a, b):
            return os.popen("/usr/bin/cygpath " + a + " " + b).read().replace("\n", "")

        def strip_exe(a):
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
ROOT = (ROOT or dirname(dirname(dirname(__file__))))

INSTALLROOT = (INSTALLROOT or CM3_INSTALL)
M3GDB = (M3GDB or CM3_GDB)
M3OSTYPE = (M3OSTYPE or CM3_OSTYPE)
TARGET = (TARGET or CM3_TARGET)
GCC_BACKEND = (GCC_BACKEND or CM3_GCC_BACKEND)
PKGSDB = (PKGSDB or join(dirname(__file__), "PKGS"))
GREP = (GREP or "egrep")
GMAKE = (GMAKE or "gmake")

def qgrep():
    pass

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
debug("GREP")
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
