@echo off
REM UnArc.CMD, v2.0, R.Coleburn, 2008_0608
REM Requires use of Cygwin's tar & gzip utilities.

echo _________________
echo.
echo UnArc = Unarchive
echo _________________
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

:Check1
set ArcDirSrc=%1.tgz
if not exist %ArcDirSrc% set ArcDirSrc=%1.tar.gz
if not exist %ArcDirSrc% goto NoFile

:Check2
if not exist %1\*.* goto DoIt
echo.
echo WARNING:  Folder "%1" already exists and will be overwritten.
set Answ=
set /p Answ=".         Continue [y=yes (default), n=no] ? "
if /i (%Answ%)==(n) goto Abort
if /i (%Answ%) NEQ (y) if (%Answ%) NEQ () goto Check2

:DoIt
echo.
echo Unzipping "%ArcDirSrc%" ...
%ArcDirBin%gzip -d %ArcDirSrc%
if not exist %1.tar goto GzipFailure
echo.
echo Extracting "%1.tar" ...
%ArcDirBin%tar xvf %1.tar

:ZapTar
echo.
set Answ=
set /p Answ="OK to delete %1.tar [y=yes (default), n=no] ? "
if /i (%Answ%)==(n) goto Done
if /i (%Answ%) NEQ (y) if not (%Answ%)==() goto ZapTar
del %1.tar

:Done
echo.
echo Done.
goto END

:GzipFailure
echo FAILURE:  Can't find "%1.tar" !!!!
goto END

:NoTar
echo Unable to locate TAR.EXE utility.
goto END

:NoGzip
echo Unable to locate GZIP.EXE utility.
goto END

:Abort
echo ...aborting operation...
goto END

:NoFile
echo Unable to locate source file "%1.tgz".
echo Unable to locate source file "%1.tar.gz".

:Usage
echo Usage:  UnArc filename
echo         (where "filename.tgz" or "filename.tar.gz" is assumed)

:END
set ArcDirBin=
set ArcDirSrc=
set Answ=
echo on
