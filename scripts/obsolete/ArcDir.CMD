@echo off
REM ArcDir.CMD, v2.0, R.Coleburn, 2008_0608
REM Requires use of Cygwin's tar & gzip utilities.

echo _____________________________________
echo.
echo ArcDir = Archive a Directory (Folder)
echo _____________________________________
echo.

:LocateUtilities
REM TAR.EXE & GZIP.EXE expected in same folder.
REM Look first in the path; then C:\cygwin\bin; then current folder
set ArcDirBin=
for /d %%I in (tar.exe) do set ArcDirBin=%%~dp$PATH:I
if (%ArcDirBin%)==() set ArcDirBin=C:\cygwin\bin\
if not exist %ArcDirBin%tar.exe set ArcDirBin=C:\cygwin\bin\
if not exist %ArcDirBin%tar.exe set ArcDirBin=%CD%\
if not exist %ArcDirBin%tar.exe goto NoTar
if not exist %ArcDirBin%gzip.exe goto NoGzip
%ArcDirBin%tar --version >>NUL:
if errorlevel 1 goto NoTar
%ArcDirBin%gzip --version >>NUL:
if errorlevel 1 goto NoGzip
echo Using TAR and GZIP located in "%ArcDirBin%"

:Check0
if (%1)==() goto Usage
if not exist %1\*.* goto Empty

:Check1
if not exist %1.tar goto Check2
echo WARNING:  File %1.tar already exists and will be overwritten.

:Check2
if not exist %1.tgz goto Check3
echo WARNING:  File %1.tgz already exists and will be overwritten.

:Check3
if not exist %1.tar.gz goto Confirm
echo WARNING:  File %1.tar.gz already exists and will be overwritten.

:Confirm
echo.
pushd %1
echo Ready to archive "%CD%" as "%1.tar".
popd
set Answ=
set /p Answ=".   Continue [y=yes (default), n=no] ? "
if /i (%Answ%)==(n) goto Abort
if /i (%Answ%) NEQ (y) if (%Answ%) NEQ () goto Confirm

:DoIt
echo.
echo Creating archive...
if exist %1.tar del %1.tar
if exist %1.tar.gz del %1.tar.gz
if exist %1.tgz del %1.tgz
%ArcDirBin%tar cvf %1.tar %1
echo.
echo Compressing archive...
if not exist %1.tar goto TarFailure
%ArcDirBin%gzip %1.tar
if not exist %1.tar.gz goto GzipFailure
ren %1.tar.gz %1.tgz
echo.
echo Done.  Archive is in:  "%1.tgz"
goto END

:TarFailure
echo FAILURE:  Can't find "%1.tar" !!!!
goto END

:GzipFailure
echo FAILURE:  Can't find "%1.tar.gz" !!!!
goto END

:NoTar
echo Unable to locate TAR.EXE utility.
goto END

:NoGzip
echo Unable to locate GZIP.EXE utility.
goto END

:Empty
echo Folder "%1" is empty or does not exist.
goto End

:Abort
echo ...aborting operation...
goto End

:Usage
echo Usage:  ArcDir folder

:END
set ArcDirBin=
set Answ=
echo on
