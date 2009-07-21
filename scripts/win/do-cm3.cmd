@echo off
REM do-cm3, written by Randy Coleburn, 07/20/2009
REM ===========================================================================
REM Version History
REM ---------------
REM v1.00, 07/20/2009, R.Coleburn
REM v1.01, 07/21/2009, R.Coleburn, minor formatting changes for readability, add error log summary
REM ===========================================================================

:Init
:----
REM Initialize variables.
set CM3_Answ=
set CM3_Arg=
set CM3_CM3Args=
set CM3_CM3Failure=
set CM3_Group=
set CM3_Pkg=
set CM3_PkgInfo=
set CM3_PkgPath=
set CM3_PkgTree=
set CM3_TMP1=
set CM3_TempFile=



:ParseParams
:-----------
REM Parse parameters, see Usage.
if (%1)==() goto Usage
goto ExamineArg1

:NextArg
shift

:ExamineArg1
if (%1)==() goto ArgEnd
if /I %1==HELP goto Help
if /I %1==(-P) goto Arg_P
if /I %1==buildship goto Arg_buildship

rem check to see if %1 is a group tag
if (%CM3_Group%)==() for %%a in (min core std base all) do if /I %%a==%1 set CM3_Group=%1
if (%CM3_Group%)==() for %%a in (anim caltech-parser comm cvsup database demo devlib front game) do if /I %%a==%1 set CM3_Group=%1
if (%CM3_Group%)==() for %%a in (gui juno m3devtool m3gdb m3gnudevtool math obliq tool webdev) do if /I %%a==%1 set CM3_Group=%1
if /I (%CM3_Group%)==(%1) goto NextArg

rem check to see if %1 is a cm3 compiler mode argument
set CM3_Arg=
for %%a in (find depend realclean clean build ship buildship) do if /I %%a==%1 set CM3_Arg=-%1
if (%CM3_Arg%)==() echo ERROR:  Unknown or unsupported argument:  %1
if (%CM3_Arg%)==() goto Usage
set CM3_CM3Args=%CM3_CM3Args% %CM3_Arg%
goto NextArg

:Arg_buildship
rem handle special case of buildship as alias for -build followed by -ship
set CM3_CM3Args=%CM3_CM3Args% -build -ship
goto NextArg

:Arg_P
rem we've seen -P, now get the path
shift
if (%1)==() echo ERROR:  Missing path after -P argument.
if (%1)==() goto Usage
set CM3_PkgInfo=%1
goto NextArg

:ArgEnd
rem no more parameters, so make sure we've got the minimum required
if (%CM3_Group%)==() echo ERROR:  Must specify a package group, e.g., min, core, std, all, etc.
if (%CM3_Group%)==() goto Usage



:SetupCM3
:--------
REM Ensure CM3 command line environment has been setup.
REM !*** This check can be skipped by setting CM3_DoneSetup=TRUE, (remove REM on next line) ***!
REM set CM3_DoneSetup=TRUE
if (%CM3_DoneSetup%)==(TRUE) goto Welcome
if not exist "c:\cm3\bin\cm3SetupCmdEnv.CMD" goto FatalSetupCM3
call c:\cm3\bin\cm3SetupCmdEnv.CMD
@echo off
if not (%CM3_DoneSetup%)==(TRUE) goto FatalSetupCM3



:Welcome
:-------
REM Identify this script.
echo.
echo ====== --------------------------------
echo do-cm3, v1.1, 7/21/2009, Randy Coleburn
echo ====== --------------------------------
echo.



:FindPkgInfo
:-----------
REM Cause CM3_PkgInfo to represent the path to PkgInfo.txt.
rem ---first check to see if user-specified -P option is valid
if not (%CM3_PkgInfo%)==() if exist "%CM3_PkgInfo%" goto FindSourceTree
if not (%CM3_PkgInfo%)==() goto NoPkgInfo

rem ---next, see if located in current directory
if exist ".\PkgInfo.txt" call :FN_FullPath .\PkgInfo.txt CM3_PkgInfo
if not (%CM3_PkgInfo%)==() if exist "%CM3_PkgInfo%" goto FindSourceTree

rem ---next, see if located in parent of current directory
if exist "..\PkgInfo.txt" call :FN_FullPath ..\PkgInfo.txt CM3_PkgInfo
if not (%CM3_PkgInfo%)==() if exist "%CM3_PkgInfo%" goto FindSourceTree

:NoPkgInfo
echo ERROR:  Unable to locate PkgInfo.txt file.
goto END



:FindSourceTree
:--------------
REM Locate the package source tree and store in CM3_PkgTree.
set CM3_PkgTree=

rem --- first try parent of %CM3_PkgInfo%
call :FN_DriveAndPathOnly %CM3_PkgInfo% CM3_PkgTree
pushd %CM3_PkgTree%
cd ..
set CM3_PkgTree=%CD%\
popd
if exist "%CM3_PkgTree%m3-sys\cm3\src" goto Prepare

rem --- next try current directory
set CM3_PkgTree=%CD%\
if exist "%CM3_PkgTree%m3-sys\cm3\src" goto Prepare

rem --- next try parent of current directory
pushd ..
set CM3_PkgTree=%CD%\
popd
if exist "%CM3_PkgTree%m3-sys\cm3\src" goto Prepare

rem --- next try grandparent of current directory
pushd ..
cd ..
set CM3_PkgTree=%CD%\
popd
if exist "%CM3_PkgTree%m3-sys\cm3\src" goto Prepare

echo ERROR:  Unable to locate package source tree.
goto END



:Prepare
:-------
REM Create a tempory file containing the names of all packages to be processed.
set CM3_TempFile="%CD%\Temp%RANDOM%.txt"
if not (%CM3_TempFile%)==() if exist %CM3_TempFile% del %CM3_TempFile%
echo CM3 ARGS = %CM3_CM3Args%
echo  PkgInfo = %CM3_PkgInfo%
echo Pkg Tree = %CM3_PkgTree%
echo    Group = %CM3_Group%
echo.
echo Searching for packages in group "%CM3_Group%" ...
echo -------------------------------
FOR /F "tokens=1* delims= " %%i in (%CM3_PkgInfo%) do call :FN_CheckPkg %%i %%j
echo.
echo Packages to be processed:
echo ------------------------
if exist %CM3_TempFile% type %CM3_TempFile%
echo ---END-of-List---
echo.
:AskContinue
set /P CM3_Answ=Do you want to continue (y=yes, n=no) ? 
if /I (%CM3_Answ%)==(Y) goto DoIt
if /I (%CM3_Answ%)==(N) goto END
goto AskContinue



:DoIt
:----
REM Process each of the packages named in the temporary file.
if not exist %CM3_TempFile% goto END
set CM3_ErrLog="%CD%\Temp%RANDOM%ErrLog.txt"
if not (%CM3_ErrLog%)==() if exist %CM3_ErrLog% del %CM3_ErrLog%
set CM3_CM3Failure=
call :FN_RemoveQuotes %CM3_TempFile% CM3_TMP1
FOR /F "tokens=1 delims=" %%i in (%CM3_TMP1%) do call :FN_DoPkg %%i
echo.
if (%CM3_CM3Failure%)==() goto END
echo WARNING:  One or packages experienced a failure.  See result detail above.
echo.
echo ERROR LOG SUMMARY:
echo -----------------
if exist %CM3_ErrLog% type %CM3_ErrLog%
if exist %CM3_ErrLog% del %CM3_ErrLog%
echo ---END-of-List---
echo.
goto END



:FatalSetupCM3
:-------------
echo ERROR:  Unable to successfully run "c:\cm3\bin\cm3SetupCmdEnv.CMD"
echo         Please edit this .CMD file to provide correct path.
goto END



:FN_FullPath
:-----------
REM Make a full (non-relative) path.
REM 1=relative path, 2=env var for result
set %2=%~f1
goto :EOF



:FN_DriveAndPathOnly
:-------------------
REM Convert %1 to a drive letter and path only.
REM 1=path, 2=env var for result
set %2=%~dp1
goto :EOF



:FN_RemoveQuotes
:---------------
REM Remove quotes surrounding path.
REM 1=quoted path, 2=env var for result
set %2=%~1
goto :EOF



:FN_CheckPkg
:-----------
REM If this package (%1) should be processed and can be found, add it to the temporary file.
REM 1=package, 2*=group tags for this package

rem ---first see if this package is tagged with the desired group
set CM3_Pkg=
for %%a in (%*) do if /I %%a==%CM3_Group% set CM3_Pkg=%1
if /I %CM3_Group%==all set CM3_Pkg=%1
if (%CM3_Pkg%)==() goto :EOF

rem ---sometimes the package has a relative path in unix-style, so convert it to DOS-style
call :FN_Normalize_CM3_Pkg
rem echo normalized = %CM3_Pkg%

rem ---make sure we can find this package in the source tree
set CM3_PkgPath=
pushd %CM3_PkgTree%

rem ------first try the package itself as a relative path
if exist "%CM3_Pkg%\src" set CM3_PkgPath=%CM3_Pkg%

rem ------if that doesn't work, look in the various m3-* folders
if (%CM3_PkgPath%)==() for /f "usebackq" %%i in (`dir /b m3-* caltech*`) do if exist "%%i\%CM3_Pkg%\src" set CM3_PkgPath=%%i\%CM3_Pkg%

rem ------if we found it, great, otherwise report we are skipping it
popd
if not (%CM3_PkgPath%)==() goto foundPkg
echo WARNING:  Unable to locate package "%CM3_Pkg%" in "%CM3_PkgTree%"
echo           (this package will be skipped)
echo.
goto :EOF

:foundPkg
echo %CM3_Pkg%, found "%CM3_Group%" in (%*), at "%CM3_PkgPath%"
echo %CM3_PkgPath% >>%CM3_TempFile%
goto :EOF



:FN_Normalize_CM3_Pkg
:--------------------
REM convert %CM3_Pkg% from unix-style path to DOS-style path
echo %CM3_Pkg%|find "/" >>NUL:
if errorlevel 1 goto :EOF
for /f "tokens=1* delims=/" %%i in ("%CM3_Pkg%") do set CM3_Pkg=%%i\%%j
goto FN_Normalize_CM3_Pkg



:FN_DoPkg
:--------
REM Process this package (%1).
if (%CM3_CM3Failure%)==(STOP) goto :EOF
echo.
echo --- processing package %1 ---
pushd %CM3_PkgTree%%1
for %%z in (%CM3_CM3Args%) do call :FN_DoCM3 %%z %1
popd
goto :EOF



:FN_DoCM3
:--------
REM Invoke CM3
REM 1=CM3 mode (e.g. -build, -ship, -find, -depend, -clean, -realclean)
REM 2=package
if (%CM3_CM3Failure%)==(STOP) goto :EOF
cm3 %1
if not errorlevel 1 goto :EOF
set CM3_CM3Failure=TRUE
echo.
echo WARNING:  Encountered an error when processing package %2 for %1
echo WARNING:  Errors in package %2 for %1>>%CM3_ErrLog%
echo.

:AskStop
set /P CM3_Answ=Do you want to stop (y=yes, n=no) ? 
if /I (%CM3_Answ%)==(N) goto :EOF
if /I (%CM3_Answ%)==(Y) set CM3_CM3Failure=STOP
if (%CM3_CM3Failure%)==(STOP) goto :EOF
goto AskStop
goto :EOF



:Help
:----
echo --- HELP for do-cm3.CMD ---
echo.
echo do-cm3.CMD is intended to replicate on Windows 2000/XP the functionality of the
echo various "do-cm3-*.sh" scripts.  
echo.
echo As such, it invokes cm3 on a group of packages.  Packages and their group 
echo associations are defined in the PkgInfo.txt file.  
echo.
echo The PkgInfo.txt file can be specified via the -P argument, or it is searched 
echo for in the current directory, then the parent directory.
echo.
echo The package source tree is located relative to the PkgInfo.txt file (parent
echo folder), or in the current directory, or in the parent directory.



:Usage
:-----
echo.
echo Usage:  do-cm3 {help} [min core std all] {-p path} {cm3args}
echo.
echo help    = display help, then exit.
echo.
echo [min core std all] = must specify only one package group.  The list of groups
echo                      shown here is not exhaustive.
echo.
echo -p path = specify location of PkgInfo.txt
echo           (if not specified, looks in ".\PkgInfo.txt" then "..\PkgInfo.txt")
echo.
echo cm3args = zero or more arguments to the cm3 builder, e.g., clean, build, ship
echo           Multiple arguments are possible.  They will be performed in the
echo           order given.  
echo           "buildship" is shorthand for "build" followed by "ship".
echo.



:END
:---
REM Remove environment variables and temporary files, then exit.
rem echo CM3_Answ=%CM3_Answ%
set CM3_Answ=
rem echo CM3_Arg=%CM3_Arg%
set CM3_Arg=
rem echo CM3_CM3Args=%CM3_CM3Args%
set CM3_CM3Args=
rem echo CM3_CM3Failure=%CM3_CM3Failure%
set CM3_CM3Failure=
rem echo CM3_ErrLog=%CM3_ErrLog%
set CM3_ErrLog=
rem echo CM3_Group=%CM3_Group%
set CM3_Group=
rem echo CM3_Pkg=%CM3_Pkg%
set CM3_Pkg=
rem echo CM3_PkgInfo=%CM3_PkgInfo%
set CM3_PkgInfo=
rem echo CM3_PkgPath=%CM3_PkgPath%
set CM3_PkgPath=
rem echo CM3_PkgTree=%CM3_PkgTree%
set CM3_PkgTree=
rem echo CM3_TMP1=%CM3_TMP1%
set CM3_TMP1=
rem echo CM3_TempFile=%CM3_TempFile%
if (%CM3_TempFile%)==() goto E2
if exist %CM3_TempFile% del %CM3_TempFile%
set CM3_TempFile=
:E2
echo ===END do-cm3===
echo on
