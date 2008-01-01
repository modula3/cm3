# $Id$

import sys
import os.path
import os
import shutil
import pylib
from pylib import *

def Done():
    global exe, zip, symbols

    print("Success.")
    if (exe):
        exe = os.path.join(STAGE, "min", exe)
        if (os.path.isfile(exe)):
            print("Output is " + exe)
    print("Output is " + os.path.join(STAGE, "min", zip))
    print("Output is " + os.path.join(STAGE, "min", symbols))
    print("Lots of intermediate state remains in " + STAGE)
    sys.exit(0)

def Run(a):
    return (os.system(a) == 0)
    # @call :IncrementLogCounter
    # @echo.
    # setlocal
    # remove some extraneous spaces that come
    # concating possibly empty variables with
    # spaces between them
    # set x=%*
    # set x=%x:  = %
    # set x=%x:  = %
    # echo %TIME%>>%STAGE%\logs\%LogCounter%_%~n1.log
    # echo.>> %STAGE%\logs\all.log
    # echo.>> %STAGE%\logs\%LogCounter%_%~n1.log
    # echo %x% >> %STAGE%\logs\%LogCounter%_%~n1.log
    # echo %x% ^>^> %STAGE%\logs\%LogCounter%_%~n1.log
    # call %x% >> %STAGE%\logs\%LogCounter%_%~n1.log or (
    #     echo %TIME%>>%STAGE%\logs\%LogCounter%_%~n1.log
    #     type %STAGE%\logs\%LogCounter%_%~n1.log >> %STAGE%\logs\all.log
	#     echo ERROR: %x% failed
	#     exit /b 1
    # )
    # echo %TIME%>>%STAGE%\logs\%LogCounter%_%~n1.log
    # type %STAGE%\logs\%LogCounter%_%~n1.log >> %STAGE%\logs\all.log
    # endlocal
    # goto :eof

def Zip():
    global exe, zip, symbols

    os.chdir(INSTALLROOT_MIN)
    CopyFile(os.path.join(ROOT, "m3-sys", "COPYRIGHT-CMASS"), os.path.curdir) or FatalError()
    os.chdir(os.path.pardir)
    CreateDirectory("symbols")

    def Callback(Arg, Directory, Names):
        if (os.path.basename(Directory) == "symbols"):
            return
        for Name in Names:
            Extension = os.path.splitext(Name)[1].lower()
            if (Extension == ".pdb"):
                shutil.move(os.path.join(Directory, Name), "symbols")
            elif (Extension == ".m3"
                    or Extension == ".m3web"
                    # Can be useful for bootstrapping standalone cm3.exe in future.
                    #or Extension == ".sa
                    ):
                os.remove(os.path.join(Directory, Name))

    os.path.walk(os.path.curdir, Callback, None)

    map(os.remove, reduce(lambda a, b:(a + b), map(glob.glob, map(lambda a:"*." + a, ["bz2", "zip", "exe", "tar"])), [ ]))

    symbols = "cm3-min-" + M3OSTYPE + "-" + TARGET + "-" + CM3VERSION + "-symbols.tar.bz2"
    # os.system("tar cfvj " + symbols + " symbols")

    symbols = "cm3-min-" + M3OSTYPE + "-" + TARGET + "-" + CM3VERSION + "-symbols.zip"
    Run("zip -9 -r -D -X " + symbols + " symbols")

    zip = "cm3-min-" + M3OSTYPE + "-" + TARGET + "-" + CM3VERSION + ".zip"
    Run("zip -9 -r -D -X " + zip + " cm3")

    #
    # HACK ALERT
    #
    # ON MY MACHINE \bin\unzipsfx.exe is a
    # Win32 x86 unzip self extracting archive prefix,
    # with an MS-DOS unzip self extracting archive prefix for a stub.
    # As such, you can do several things with it.
    #  Run it under MS-DOS. However long file names are probably used.
    #  Run it from a Windows command line.
    #  Open it with various archive utilities, including maybe Explorer (might need to rename it to end in .zip).
    #
    # .tar.bz2 is generally significantly smaller, but .zip is currently used
    # for ease of Windows users.
    #
    # I built this unzipsfx from the publically available source. That source
    # and building of it is not in the CM3 tree, and probably should be
    # if this path is to be used. In fact, that license may make
    # these tools favorable over tar/bzip2, despite the compression loss.
    #

    exe = "cm3-min-" + M3OSTYPE + "-" + TARGET + "-" + CM3VERSION + EXE
    if (not os.path.isfile("\\bin\\unzipsfx" + EXE)):
        print("\\bin\\unzipsfx" + EXE + " does not exist, skipping making self extracting .zip")
    else:
        open(exe, "wb").write(open("\\bin\\unzipsfx" + EXE, "rb").read() + open(zip, "rb").read())
        Run("zip -A " + exe)
    Done()

def TarGzip():
    return True
#    pushd %INSTALLROOT_MIN%
#    if not exist symbols mkdir symbols
#    for /f %%a in ('dir /s/b/a-d *.pdb') do move %%a symbols
#    if exist system.tgz del system.tgz
#    tar cvzf system.tgz bin lib pkg or FatalError()
#    call :CopyFile %ROOT%\m3-sys\COPYRIGHT-CMASS . or FatalError()
#    if exist cm3-min-%M3OSTYPE%-%TARGET%-%CM3VERSION%.tgz del cm3-min-%M3OSTYPE%-%TARGET%-%CM3VERSION%.tgz
#    tar cvzf cm3-min-%M3OSTYPE%-%TARGET%-%CM3VERSION%.tgz cminstall.exe COPYRIGHT-CMASS system.tgz tar.exe gzip.exe cygwin.dll or FatalError()
#    tar cvzf cm3-min-%M3OSTYPE%-%TARGET%-%CM3VERSION%-symbols.tgz symbols or FatalError()
#    popd

#    echo Success.
#    echo Output is %INSTALLROOT_MIN%\cm3-min-%M3OSTYPE%-%TARGET%-%CM3VERSION%.tgz
#    echo Output is %INSTALLROOT_MIN%\cm3-min-%M3OSTYPE%-%TARGET%-%CM3VERSION%-symbols.tgz
#    echo Lots of intermediate state remains in %STAGE%.
#    goto :eof

def TarBzip2():
    return True
#    pushd %INSTALLROOT_MIN%
#    call :CopyFile %ROOT%\m3-sys\COPYRIGHT-CMASS . or FatalError()
#    cd ..
#    if not exist symbols mkdir symbols
#    for /f %%a in ('dir /s/b/a-d *.pdb') do move %%a symbols
#    set symbols=cm3-min-%M3OSTYPE%-%TARGET%-%CM3VERSION%-symbols.tar.bz2
#    set zip=cm3-min-%M3OSTYPE%-%TARGET%-%CM3VERSION%.tar.bz2
#    if exist %zip% del %zip%
#    if exist %symbols% del %symbols%
#    tar cfvj %symbols% symbols
#    tar cfvj %zip% cm3
#    popd
#    goto :done


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
    CopyFile(os.path.join(FromSys, "cminstall", "src", "config", TARGET), os.path.join(ToBin, "cm3.cfg")) or FatalError()
    if (os.name == "nt"):
        CopyFile       (os.path.join(FromBin, "cm3.pdb"), ToBin) or FatalError()
        CopyFileIfExist(os.path.join(FromBin, "cm3.exe.manifest"), ToBin) or FatalError()
    return True

def CopyCompiler(From, To):
    #
    # Copy the compiler from one INSTALLROOT to another, possibly having cleaned out the intermediate directories.
    # The config file always comes right out of the source tree.
    #
    FromBin = os.path.join(From, "bin")
    ToBin = os.path.join(To, "bin")
    CreateDirectory(ToBin)
    CopyFile           (os.path.join(FromBin, "cm3" + EXE), ToBin) or FatalError()
    CopyFile           (os.path.join(ROOT, "m3-sys", "cminstall", "src", "config", TARGET), os.path.join(ToBin, "cm3.cfg")) or FatalError()
    if (os.name == "nt"):
        CopyFile       (os.path.join(FromBin, "cm3.pdb"         ), ToBin) or FatalError()
        CopyFileIfExist(os.path.join(FromBin, "cm3.exe.manifest"), ToBin) or FatalError()
    CopyMklib(From, To) or FatalError()
    return True

def CopyMklib(From, To):
    #
    # Copy mklib from one INSTALLROOT to another, possibly having cleaned out the intermediate directories.
    #
    From = os.path.join(From, "bin")
    To = os.path.join(To, "bin")
    CreateDirectory(To)
    CopyFile(os.path.join(From, "mklib" + EXE), To) or FatalError()
    if (os.name == "nt"):
        CopyFileIfExist(os.path.join(From, "mklib.pdb"         ), To) or FatalError()
        CopyFileIfExist(os.path.join(From, "mklib.exe.manifest"), To) or FatalError()
    return True

def CopyFile(From, To):
    if (os.path.isdir(To)):
        To = os.path.join(To, os.path.basename(From))
    if (os.path.isfile(To)):
        os.remove(To)
    print("copy " + From + " " + To)
    shutil.copyfile(From, To)
    return True

def CopyFileIfExist(From, To):
    if (os.path.isfile(From)):
        return CopyFile(From, To)
    return True

def BuildShip(Packages):
    # This is more indirect than necessary.
    CreateSkel()
    return Do("buildship", Packages)

def RealClean(Packages):
    # This is more indirect than necessary.
    CreateSkel()
    return Do("realclean", Packages)

def CreateSkel():
    for a in ("bin", "lib", "pkg"):
        CreateDirectory(os.path.join(INSTALLROOT, a)) or FatalError()
    return True

def Do(Command, Packages):
    # This is more indirect than necessary.
    return do_pkg(["foo", Command], Packages)

def CreateDirectory(a):
    if (not os.path.isdir(a)):
        os.makedirs(a)
    return True

def FatalError():
    # logs don't work yet
    #print("ERROR: see " + Logs)
    sys.exit(1)

# Start with the installed cm3.
# cm3 should not be set in the environment, or should be set to merely "cm3" or "cm3.exe"

if (not STAGE):
    tempfile.tempdir = os.path.join(tempfile.gettempdir(), "cm3", "make-dist")
    CreateDirectory(tempfile.tempdir)
    STAGE = tempfile.mkdtemp()
    debug("STAGE")

# doesn't work yet
#Logs = os.path.join(STAGE, "logs")
#os.makedirs(Logs)

#LogCounter = 1

INSTALLROOT_PREVIOUS = INSTALLROOT

INSTALLROOT_COMPILER_WITH_PREVIOUS = os.path.join(STAGE, "compiler_with_previous")
INSTALLROOT_COMPILER_WITH_SELF = os.path.join(STAGE, "compiler_with_self")

#
# The way this SHOULD work is we build the union of all desired,
# and then pick and chose from the output into the .zip/.tar.bz2.
# For now though, we only build min.
#
INSTALLROOT_MIN = os.path.join(STAGE, "min", "cm3")
INSTALLROOT_STD = os.path.join(STAGE, "std", "cm3")
INSTALLROOT_CORE = os.path.join(STAGE, "core", "cm3")
INSTALLROOT_BASE = os.path.join(STAGE, "base", "cm3")

OriginalLIB = os.getenv("LIB")
if OriginalLIB:
    OriginalLIB = (os.path.pathsep + OriginalLIB)

OriginalPATH = os.getenv("PATH")
if OriginalPATH:
    OriginalPATH = (os.path.pathsep + OriginalPATH)

# for incremental runs to recover at this step..
# if /i "%1" == "goto_tar" shift & goto :TarGzip
# if /i "%1" == "goto_min" shift & goto :min
# if /i "%1" == "goto_zip" shift & goto :Zip
# if /i "%1" == "goto_tarbzip2" shift & goto :TarBzip2

# ------------------------------------------------------------------------------------------------------------------------
print("build new compiler with old compiler and old runtime (%(INSTALLROOT_PREVIOUS)s to %(INSTALLROOT_COMPILER_WITH_PREVIOUS)s)" % vars())
# ------------------------------------------------------------------------------------------------------------------------

# build just compiler this pass, not the runtime
Packages = [
    "import-libs",
    "m3middle",
    "m3linker",
    "m3front",
    "m3quake",
    "m3objfile",
    "m3back",
    "m3staloneback",
    "m3objfile",
    "cm3",
    ]

def CopyRecursive(From, To):
    print("xcopy /fiveryh " + From + " " + To)
    shutil.copytree(From, To)

# copy over runtime package store from old to new
for a in ["libm3", "m3core"]:
    CopyRecursive(os.path.join(INSTALLROOT, "pkg", a), os.path.join(INSTALLROOT_COMPILER_WITH_PREVIOUS, "pkg", a))

#
# cm3 is run out of %path%, but mklib is not, so we have to copy it..
#
if (not CopyMklib(INSTALLROOT, INSTALLROOT_COMPILER_WITH_PREVIOUS)):
    sys.exit(1)

INSTALLROOT = INSTALLROOT_COMPILER_WITH_PREVIOUS
os.putenv("INSTALLROOT", INSTALLROOT)
os.putenv("LIB", os.path.join(INSTALLROOT, "lib") + OriginalLIB)
reload(pylib)
RealClean(Packages) or FatalError()
BuildShip(Packages) or FatalError()
ShipCompiler() or FatalError()
RealClean(Packages) or FatalError()

# ----------------------------------------------------------------------------------------------------------------------------------
print("build new compiler and new runtime with new compiler (%(INSTALLROOT_COMPILER_WITH_PREVIOUS)s to %(INSTALLROOT_COMPILER_WITH_SELF)s)" % vars())
# ----------------------------------------------------------------------------------------------------------------------------------


Packages = [
    "import-libs",
    "m3core",
    "libm3",
    "patternmatching",
    "m3bundle",
    "m3middle",
    "m3objfile",
    "m3linker",
    "m3back",
    "m3staloneback",
    "m3front",
    "m3quake",
    "cm3",
    "mklib",
    ]

INSTALLROOT = INSTALLROOT_COMPILER_WITH_SELF
os.putenv("INSTALLROOT", INSTALLROOT)
os.putenv("LIB", os.path.join(INSTALLROOT, "lib") + OriginalLIB)
os.putenv("PATH", os.path.join(INSTALLROOT_COMPILER_WITH_PREVIOUS, "bin") + OriginalPATH)
reload(pylib)

RealClean(Packages) or FatalError()
BuildShip(Packages) or FatalError()
ShipCompiler() or FatalError()

# ----------------------------------------------------------------------------------------------------------------------------------
print("build minimal packages with new compiler")
# ----------------------------------------------------------------------------------------------------------------------------------

#:min

INSTALLROOT = INSTALLROOT_MIN
os.putenv("INSTALLROOT", INSTALLROOT)
os.putenv("LIB", os.path.join(INSTALLROOT, "lib") + OriginalLIB)
os.putenv("PATH", os.path.join(INSTALLROOT, "bin") + OriginalPATH)
CopyCompiler(INSTALLROOT_COMPILER_WITH_SELF, INSTALLROOT) or FatalError()
reload(pylib)

Packages = ["m3core", "libm3"]
RealClean(Packages) or FatalError()
BuildShip(Packages) or FatalError()
RealClean(Packages) or FatalError()

# ----------------------------------------------------------------------------------------------------------------------------------
print("build core packages with new compiler")
# ----------------------------------------------------------------------------------------------------------------------------------

print("skipping..")

if (False):

    INSTALLROOT = INSTALLROOT_CORE
    os.putenv("INSTALLROOT", INSTALLROOT)
    os.putenv("LIB", os.path.join(INSTALLROOT, "lib") + OriginalLIB)
    os.putenv("PATH", os.path.join(INSTALLROOT, "bin") + OriginalPATH)
    CopyCompiler(INSTALLROOT_COMPILER_WITH_SELF, INSTALLROOT) or FatalError()
    reload(pylib)

    Packages = [ ]
    RealClean(Packages) or FatalError()
    BuildShip(Packages) or FatalError()
    RealClean(Packages) or FatalError()


# ----------------------------------------------------------------------------------------------------------------------------------
print("build standard packages with new compiler")
# ----------------------------------------------------------------------------------------------------------------------------------

print("skipping..")

if (False):

    INSTALLROOT = INSTALLROOT_STD
    os.putenv("INSTALLROOT", INSTALLROOT)
    os.putenv("LIB", os.path.join(INSTALLROOT, "lib") + OriginalLIB)
    os.putenv("PATH", os.path.join(INSTALLROOT, "bin") + OriginalPATH)
    CopyCompiler(INSTALLROOT_COMPILER_WITH_SELF, INSTALLROOT) or FatalError()
    reload(pylib)

    Packages = [ ]
    RealClean(Packages) or FatalError()
    BuildShip(Packages) or FatalError()
    RealClean(Packages) or FatalError()

# ----------------------------------------------------------------------------------------------------------------------------------

# print("INSTALLROOT_MIN=" + INSTALLROOT_MIN)
# print("INSTALLROOT_STD=" + INSTALLROOT_STD)
# print("INSTALLROOT_CORE=" + INSTALLROOT_CORE)
# print("INSTALLROOT_BASE=" + INSTALLROOT_BASE)

Zip()
