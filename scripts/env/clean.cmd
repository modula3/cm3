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

# Remove any directory that contains any of: cl.exe, link.exe, c1.dll, c2.dll.
# Remove various other directories.
# Merely always prepending does not work to quash later elements -- old cl.exe
# seems to run new c2.dll.

import os
import sys

newpath = ""

def verbose(a):
    #print("echo " + a)
    pass

for a in os.environ["PATH"].split(";"):
    b = a.lower()
    verbose(a)
    if os.path.isfile(os.path.join(a, "cl.exe")):
        verbose("1 skipping " + a)
        continue
    if b.endswith("\\winnt") and os.path.isfile(os.path.join(a, "..\\cl.exe")):
        verbose("2 skipping " + a)
        continue
    if os.path.isfile(os.path.join(a, "link.exe")):
        verbose("3 skipping " + a)
        continue
    if os.path.isfile(os.path.join(a, "c1.exe")):
        verbose("4 skipping " + a)
        continue
    if os.path.isfile(os.path.join(a, "c2.exe")):
        verbose("5 skipping " + a)
        continue
    if os.path.isfile(os.path.join(a, "c2.dll")):
        verbose("6 skipping " + a)
        continue
    if os.path.isfile(os.path.join(a, "c1.dll")):
        verbose("7 skipping " + a)
        continue
    if os.path.isfile(os.path.join(a, "c1xx.dll")):
        verbose("8 skipping " + a)
        continue
    if os.path.isfile(os.path.join(a, "dbi.dll")):
        verbose("9 skipping " + a)
        continue
    if os.path.isfile(os.path.join(a, "mspdb50.dll")):
        verbose("10 skipping " + a)
        continue
    if os.path.isfile(os.path.join(a, "msdis100.dll")):
        verbose("11 skipping " + a)
        continue
    if b.find("\\microsoft sdks\\") != -1:
        verbose("12 skipping " + a)
        continue
    if b.find("\\microsoft visual studio") != -1:
        verbose("13 skipping " + a)
        continue
    if b.find("\\html help workshop") != -1:
        verbose("14 skipping " + a)
        continue
    if b.find("\\windows kits\\") != -1:
        verbose("15 skipping " + a)
        continue
    if b.find("\\msbuild\\") != -1:
        verbose("16 skipping " + a)
        continue
    if newpath == "":
        newpath = a
    else:
        newpath = newpath + ";" + a

newpath = "set PATH=" + newpath
verbose(newpath)
print(newpath)

sys.exit(0)
