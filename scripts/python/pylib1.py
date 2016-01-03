#! /usr/bin/env python

import pylib
from pylib import *

class Sdk:
    def __init__(self):
        self.include = [ ]
        self.lib = [ ]
        self.path = [ ]

    def fromEnv():
        if not os.environ.get("INCLUDE") and not os.environ.get("LIB"):
            return None
        self = Sdk()
        self.include = os.environ["INCLUDE"].split(";")
        self.lib = os.environ["LIB"].split(";")
        self.path = os.environ["PATH"].split(";")
        return self

    def check(self, path, file):
        for i in path:
            if os.path.isfile(os.path.join(i, file)):
                return True
        return False

    def checkInc(self, file):
        return self.check(self, self.include, file)

    def checkLib(self, file):
        return self.check(self, self.lib, file)

    def checkPath(self, file):
        return self.check(self, self.path, file)


def FindWindowsSdkInternal(cruntime):
# A WindowsSdk needs include\windows.h, lib\kernel32.lib, and otionally path\mt.exe.
# CRuntime and WindowsSdk are sometimes colocated, esp. with older Visual C++

# cruntime is required, except for testing

    def FindWindowsSdk1(include, lib):
        if os.path.isfile(os.path.join(include, subver, "windows.h")):
            if os.path.isfile(os.path.join(lib, subver, "kernel32.lib")):
                return True
        return False

    if cruntime != None:
        if cruntime.checkInc("windows.h") and cunrtime.checkLib("kernel32.lib"):
            return cruntime

    for candidate in [[["Windows Kits", "10.0"], [ "10.0.10240.0", "10.0.10150.0" ]],
                      [["Windows Kits", "8.1"]],
                      [["Windows Kits", "8.0"]],
                      [["Microsoft SDKs", "Windows", "v7.0A"]],
                      [["Microsoft SDKs", "Windows", "v6.0A"]],
                      ["Microsoft Platform SDK for Windows Server 2003 R2"]]:
        for prog in GetProgramFiles():
            subvers = ["."]
            if not os.path.isdir(prog):
                next
            a = candidate[0]
            if len(a) < 10:
                a = str(os.sep).join(a)
            if len(candidate) > 1:
                subvers = candidate[1]
            for subver in subvers:
                a = str(os.sep).join([prog, a])
                b = GetFullPath(str(os.sep).join([a, subver]))
                print("checking "  + a)
                print("checking "  + b)

    return None

def FindWindowsSdk(cruntime):
# A WindowsSdk needs include\windows.h, lib\kernel32.lib, and otionally path\mt.exe.
# CRuntime and WindowsSdk are sometimes colocated, esp. with older Visual C++
    return FindWindowsSdkInternal(cruntime)

def FindCRuntime():
# A CRuntime needs include\errno.h, lib\libcmt.lib or lib\msvcrt.lib, path\cl.exe, path\link.exe
# and at least one of path\mspdb*dll or path\dbi.dll
#
# Ordering should be newest to oldest, but isn't necessarily.

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

    pass

def FindIncludeLib():
# IncludeLib is a map of {"include", "lib"}, arrays, the union
# of FindWindowsSdk and FindCRuntime (which can be equal, i.e.
# for older Visual C++ releases
    pass


#-----------------------------------------------------------------------------

if __name__ == "__main__":

    # run test code if module run directly

    FindWindowsSdkInternal(None)

    sys.exit(1)

