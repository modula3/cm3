#! /usr/bin/env python
# $Id: make-dist.py,v 1.60 2009-06-28 14:13:09 jkrell Exp $

import sys
import os.path
import os
import shutil
import pylib
from pylib import *

def Echo(a):
    print("")
    print("=============================================================================")
    print(a)
    print("=============================================================================")
    print("")

def Run(a):

    print(a + " in " + os.getcwd())
    #return True
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

def MakeArchive(PackageSetName, Command, Extension):

    InstallRoot = FormInstallRoot(PackageSetName)
    SymbolsRoot = FormInstallRoot(PackageSetName) + "-symbols"

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

    class State():
        pass

    state = State()
    state.id = 0

    def Callback(state, dir, entries):
        for a in entries:
            if a == "COPYRIGHT":
                state.id += 1
                CopyFile(os.path.join(dir, a), os.path.join(license, "COPYRIGHT-CALTECH-" + str(state.id)))

    os.path.walk(os.path.join(Root, "caltech-parser"), Callback, state)

    #
    # delete .m3 and .m3web files, they aren't needed
    # move .pdb files into the symbols directory
    # TBD: use strip and --add-gnu-debuglink
    #
    def Callback(Arg, Directory, Names):
        if (os.path.basename(Directory).endswith("-symbols")):
            return
        for Name in Names:
            Extension = os.path.splitext(Name)[1].lower()
            if (Extension == ".pdb"):
                shutil.move(os.path.join(Directory, Name), SymbolsRoot)
            elif (Extension == ".m3" or Extension == ".m3web" or Extension == ".sa"):
                #
                # Keep m3.lib.sa, m3core.lib.sa, sysutils.lib.sa for bootstrapping of cm3.
                # This check is loose to allow for multiple naming schemes.
                #
                if ((Name.find("m3") == -1) and (Name.find("sysutils") == -1)):
                    DeleteFile(os.path.join(Directory, Name))

    CreateDirectory(SymbolsRoot)
    os.path.walk(InstallRoot, Callback, None)

    os.chdir(os.path.dirname(InstallRoot))

    if not os.listdir(SymbolsRoot):
        os.rmdir(SymbolsRoot)
    else:
        Symbols = FormArchiveName(PackageSetName, "-symbols." + Extension)
        DeleteFile(Symbols)
        Run(Command + " " + os.path.basename(Symbols) + " " + os.path.basename(SymbolsRoot))

    Archive = FormArchiveName(PackageSetName, "." + Extension)
    DeleteFile(Archive)
    Run(Command + " " + os.path.basename(Archive) + " " + os.path.basename(InstallRoot))

    #
    # Building a self extracting .exe is very easy but not present for now.
    # It is available in history if desired.
    # I think it'd be more valuable if it was a gui. tbd?
    #

def Zip(PackageSetName):
    MakeArchive(PackageSetName, "zip -9 -r -D -X", "zip")

def TarGzip(PackageSetName):
    MakeArchive(PackageSetName, "tar cfvz", "tar.gz")

def TarBzip2(PackageSetName):
    MakeArchive(PackageSetName, "tar cfvj", "tar.bz2")

def MakeArchives():
    for PackageSetName in ["min", "std"]:
        if Config == "NT386":
            Zip(PackageSetName)
        else:
            TarGzip(PackageSetName)

def BuildShip(Packages):
    # This is more indirect than necessary.
    CreateSkel()
    return Do("buildship", Packages)

def RealClean(Packages):
    # This is more indirect than necessary.
    #
    CreateSkel()
    #
    # RealClean is mostly unnecessary and a nuisance for make-dist.
    # Either STAGE is unique and there's nothing to clean,
    # or STAGE is explicit and not unique and incrementality
    # is desired. Er, then again, this doesn't touch STAGE,
    # it touches the output directories in the source tree.
    #
    return True
    #return Do("realclean", Packages)

def CreateSkel():
    for a in ("bin", "pkg"):
        CreateDirectory(os.path.join(InstallRoot, a)) or FatalError()
    return True

def Do(Command, Packages):
    # This is more indirect than necessary.
    return DoPackage(["", Command], Packages)

def FatalError():
    # logs don't work yet
    #print("ERROR: see " + Logs)
    print("fatal error")
    sys.exit(1)

# doesn't work yet
#Logs = os.path.join(STAGE, "logs")
#os.makedirs(Logs)

#LogCounter = 1

InstallRoot_Previous = InstallRoot

STAGE = GetStage()

InstallRoot_CompilerWithPrevious = os.path.join(STAGE, "compiler_with_previous")
InstallRoot_CompilerWithSelf = os.path.join(STAGE, "compiler_with_self")

def FormArchiveName(PackageSetName, Suffix):
    return os.path.join(STAGE, "cm3-" + PackageSetName + "-" + Config + "-" + CM3VERSION + Suffix)

InstallRoot_Min = FormInstallRoot("min")
InstallRoot_Standard = FormInstallRoot("std")

InstallRoots = [
    InstallRoot_Min,
    InstallRoot_Standard,
   ]

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

#MakeArchives()
#sys.exit(0)

# ------------------------------------------------------------------------------------------------------------------------
Echo("build new compiler with old compiler and old runtime (%(InstallRoot_Previous)s to %(InstallRoot_CompilerWithPrevious)s)" % vars())
# ------------------------------------------------------------------------------------------------------------------------

# build just compiler this pass, not the runtime
# That is, assuming we have m3core and libm3, build the rest of the compiler.
# We don't build mklib here because it can't be built against older non-Windows m3core, and
# because we don't need it to build the compiler.

Packages = [
    "import-libs",
    "sysutils",
    "m3middle",
    "m3linker",
    "m3front",
    "m3quake",
    "m3objfile",
    "m3back",
    "m3staloneback",
    "m3objfile",
    "m3cc",
    "cm3",
    ]

def CopyRecursive(From, To):
    CopyCommand = "xcopy /fiverdh "
    ToParent = os.path.dirname(To)
    if (os.name != "nt"):
        CopyCommand = "cp --preserve  --recursive "
    print("mkdir " + ToParent)
    print(CopyCommand + From + " " + To)
    if os.path.isdir(To):
        shutil.rmtree(To)
    else:
        CreateDirectory(ToParent)
    shutil.copytree(From, To, symlinks=True)
    return True

#
# copy over runtime package store from old to new
# It would be nice to make this optionally incremental.
#
RuntimeToCopy = ["libm3", "m3core"]
for a in RuntimeToCopy:
    CopyRecursive(
        os.path.join(InstallRoot, "pkg", a),
        os.path.join(InstallRoot_CompilerWithPrevious, "pkg", a)) or FatalError()

NewLib = os.path.join(InstallRoot_CompilerWithPrevious, "lib")
CreateDirectory(NewLib)

if Config != "NT386":
    for a in glob.glob(os.path.join(InstallRoot, "lib", "libm3gcdefs.*")):
        CopyFile(a, NewLib) or FatalError()

for a in glob.glob(os.path.join(InstallRoot, "lib", "*.obj")):
    CopyFile(a, NewLib) or FatalError()

#
# cm3 is run out of %path%, but mklib is not, so we have to copy it.
#
CopyMklib(InstallRoot, InstallRoot_CompilerWithPrevious) or FatalError()

def Setup(ExistingCompilerRoot, NewRoot):
    global InstallRoot
    InstallRoot = NewRoot
    os.environ["CM3_INSTALL"] = NewRoot
    if (OriginalLIB): # This is Windows-only thing.
        os.environ["LIB"] = os.path.join(NewRoot, "lib") + OriginalLIB
    os.environ["PATH"] = (os.path.join(NewRoot, "bin") + OriginalPATH)

    CopyCompiler(ExistingCompilerRoot, NewRoot) or FatalError()

    if NewRoot == InstallRoot_CompilerWithPrevious:
        NewLib = os.path.join(NewRoot, "lib")
        CreateDirectory(NewLib)
        for a in glob.glob(os.path.join(ExistingCompilerRoot, "lib", "*.obj")):
            CopyFile(a, NewLib) or FatalError()

    CopyConfigForDistribution(NewRoot) or sys.exit(1)

    reload(pylib) or FatalError()

    os.environ["CM3_INSTALL"] = ConvertToCygwinPath(NewRoot)

Setup(InstallRoot, InstallRoot_CompilerWithPrevious)
RealClean(Packages) or FatalError()
BuildShip(Packages) or FatalError()
ShipCompiler() or FatalError()
if "m3cc" in Packages:
    Packages.remove("m3cc")
RealClean(Packages) or FatalError()

# ----------------------------------------------------------------------------------------------------------------------------------
Echo("build new compiler and new runtime with new compiler (%(InstallRoot_CompilerWithPrevious)s to %(InstallRoot_CompilerWithSelf)s)" % vars())
# ----------------------------------------------------------------------------------------------------------------------------------

#
# This time build the entire compiler from m3core and on up, and the
# occasional build tools m3bundle, mklib, but don't bother building cm3cg again.
#

Packages += ["m3core", "libm3", "m3bundle", "mklib" ]
if "m3cc" in Packages:
    Packages.remove("m3cc")
Setup(InstallRoot_CompilerWithPrevious, InstallRoot_CompilerWithSelf)
RealClean(Packages) or FatalError()
BuildShip(Packages) or FatalError()
ShipCompiler() or FatalError()

AllPackages = pylib.PackageSets["all"]
for a in ["m3cc", "cm3"]:
    if a in AllPackages:
        AllPackages.remove(a)
RealClean(AllPackages) or FatalError()

# ----------------------------------------------------------------------------------------------------------------------------------
Echo("build minimal packages with new compiler")
# ----------------------------------------------------------------------------------------------------------------------------------

#:min

Setup(InstallRoot_CompilerWithSelf, InstallRoot_Min)
Packages = pylib.PackageSets["min"]
if "m3cc" in Packages:
    Packages.remove("m3cc")
RealClean(Packages) or FatalError()
BuildShip(Packages) or FatalError()
ShipCompiler() or FatalError()
RealClean(Packages) or FatalError()

# ----------------------------------------------------------------------------------------------------------------------------------
Echo("build standard packages with new compiler")
# ----------------------------------------------------------------------------------------------------------------------------------

if False:

    print("skipping..")

else:

    Setup(InstallRoot_CompilerWithSelf, InstallRoot_Standard)
    Packages = pylib.FilterPackages(pylib.PackageSets["std"])
    if "m3cc" in Packages:
        Packages.remove("m3cc")
    RealClean(Packages) or FatalError()
    BuildShip(Packages) or FatalError()
    ShipCompiler() or FatalError()
    RealClean(Packages) or FatalError()

# ----------------------------------------------------------------------------------------------------------------------------------

MakeArchives()

t = Target.lower()

def contains(s, t):
    return s.find(t) != -1

if contains(t, "linux"):
    for name in ["min", "std"]:
        MakeDebianPackage(name, FormInstallRoot(name), GetStage() + "/cm3-" + name + ".deb", "/usr/local/cm3")

if contains(t, "nt386") or contains(t, "interix") or contains(t, "cygwin") or contains(t, "mingw")  or contains(t, "uwin") or t.endswith("_nt"):
    for name in ["min", "std"]:
        pass
        #MakeMSIWithWix(FormInstallRoot(name))

for a in glob.glob(os.path.join(STAGE, "*")):
    if (a and os.path.isfile(a)):
        print("Output is " + a)
print("Much intermediate state remains in " + STAGE)
print("%s: Success." % os.path.basename(sys.argv[0]))
sys.exit(0)
