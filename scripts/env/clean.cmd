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

for a in os.environ["PATH"].split(";"):
    #print(a)
    if os.path.isfile(os.path.join(a, "cl.exe")):
        #print("echo skipping " + a)
        continue
    if a.endswith("\\WINNT") and os.path.isfile(os.path.join(a, "..\\cl.exe")):
        #print("echo skipping " + a)
        continue
    if os.path.isfile(os.path.join(a, "link.exe")):
        #print("echo skipping " + a)
        continue
    if os.path.isfile(os.path.join(a, "c1.exe")):
        #print("echo skipping " + a)
        continue
    if os.path.isfile(os.path.join(a, "c2.exe")):
        #print("echo skipping " + a)
        continue
    if os.path.isfile(os.path.join(a, "c2.dll")):
        #print("echo skipping " + a)
        continue
    if os.path.isfile(os.path.join(a, "c1.dll")):
        #print("echo skipping " + a)
        continue
    if os.path.isfile(os.path.join(a, "c1xx.dll")):
        #print("echo skipping " + a)
        continue
    if os.path.isfile(os.path.join(a, "dbi.dll")):
        #print("echo skipping " + a)
        continue
    if os.path.isfile(os.path.join(a, "mspdb50.dll")):
        #print("echo skipping " + a)
        continue
    if os.path.isfile(os.path.join(a, "msdis100.dll")):
        #print("echo skipping " + a)
        continue
    if a.find("\\Microsoft SDKs") != -1:
        #print("echo skipping " + a)
        continue
    if a.find("\\Microsoft Visual Studio") != -1:
        #print("echo skipping " + a)
        continue
    if newpath == "":
        newpath = a
    else:
        newpath = newpath + ";" + a

#print("echo set PATH=" + newpath)
print("set PATH=" + newpath)

sys.exit(0)
