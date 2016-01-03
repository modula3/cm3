@echo off && for /f "tokens=*" %%a in ('@python -x %~f0 %*') do @%%a

rem = """

@echo on

set CM3_VS2015_OR_NEWER=
set INCLUDE=
set LIB=
set LIBPATH=
set PATH=%PATH:\;=;%

@goto :eof

rem """

# Remove any directory that contains any of: cl.exe, link.exe, c1.dll, c2.dll
# Merely always prepending does not work to quash later elements -- old cl.exe
# seems to run new c2.dll.

import os
import sys

newpath = ""

def verbose(a):
    #print(a)
    pass

for a in os.environ["PATH"].split(";"):
    #print(a)
    if os.path.isfile(os.path.join(a, "cl.exe")):
        verbose("echo 1 skipping " + a)
        continue
    if a.endswith("\\WINNT") and os.path.isfile(os.path.join(a, "..\\cl.exe")):
        verbose("echo 2 skipping " + a)
        continue
    if os.path.isfile(os.path.join(a, "link.exe")):
        verbose("echo 3 skipping " + a)
        continue
    if os.path.isfile(os.path.join(a, "c1.exe")):
        verbose("echo 4 skipping " + a)
        continue
    if os.path.isfile(os.path.join(a, "c2.exe")):
        verbose("echo 5 skipping " + a)
        continue
    if os.path.isfile(os.path.join(a, "c2.dll")):
        verbose("echo 6 skipping " + a)
        continue
    if os.path.isfile(os.path.join(a, "c1.dll")):
        verbose("echo 7 skipping " + a)
        continue
    if os.path.isfile(os.path.join(a, "c1xx.dll")):
        verbose("echo 8 skipping " + a)
        continue
    if os.path.isfile(os.path.join(a, "dbi.dll")):
        verbose("echo 9 skipping " + a)
        continue
    if os.path.isfile(os.path.join(a, "mspdb50.dll")):
        verbose("echo 10 skipping " + a)
        continue
    if os.path.isfile(os.path.join(a, "msdis100.dll")):
        verbose("echo 11 skipping " + a)
        continue
    if a.find("\\Microsoft SDKs\\") != -1:
        verbose("echo 12 skipping " + a)
        continue
    if a.find("\\Microsoft Visual Studio") != -1:
        verbose("echo 13 skipping " + a)
        continue
    if a.find("\\HTML Help Workshop") != -1:
        verbose("echo 14 skipping " + a)
        continue
    if a.find("\\Windows Kits\\") != -1:
        verbose("echo 15 skipping " + a)
        continue
    if a.find("\\MSBuild\\") != -1:
        verbose("echo 16 skipping " + a)
        continue
    if newpath == "":
        newpath = a
    else:
        newpath = newpath + ";" + a

#print("echo set PATH=" + newpath)
print("set PATH=" + newpath)

sys.exit(0)
