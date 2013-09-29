@echo off
REM do-cm3, written by Randy Coleburn, 07/20/2009
REM ===========================================================================
REM Version History
REM ---------------
REM v1.00, 07/20/2009, R.Coleburn
REM v1.01, 07/21/2009, R.Coleburn, minor formatting changes for readability, add error log summary
REM v1.02, 07/27/2009, R.Coleburn, add noPause option; optimize a bit, including removing FN_Normalize_z_Pkg in favor of optimization suggested by Jay Krell
REM v1.03, 07/28/2009, R.Coleburn, add showTags and verbose options
REM v1.04, 07/29/2009, R.Coleburn, fix minor problems and force missing packages to show up in the error log when noPause option specified
REM v1.05, 07/29/2009, R.Coleburn, minor fixups
REM v1.06, 07/29/2009, R.Coleburn, optimizations
REM v1.07, 07/29/2009, R.Coleburn, repair bug introduced with prior round of edits
REM v1.08, 08/02/2009, R.Coleburn, rename CM3_Pkg to CM3_Package to prevent overloaded use of CM3_Pkg with cm3SetupCmdEnv.cmd
REM v1.10, 10/26/2009, R.Coleburn, adapt to work with new cm3CommandShell.CMD.
REM v1.11, 10/29/2009, R.Coleburn, various optimizations
REM v1.12, 01/12/2010, R.Coleburn, repair multiple bugs in "if defined xxx if exist %xxx% del %xxx%" construct
REM v1.20, 01/13/2010, R.Coleburn, add "-skip" directive.  Force argument keywords to be prefixed by "-".
REM v1.21, 09/12/2010, R.Coleburn, add Windows7 OS detection.
REM v1.22, 05/13/2012, R.Coleburn, improved error detection when invoking cm3.
rem v1.30, 02/06/2013, R.Coleburn, Improve OS detection.  Add error exit codes.  
rem                                Fix bug of not setting _cm3_CM3Failure=TRUE on fatal error for relay in environment to other cooperating CMD files.
rem v1.31, 09/22/2013, R.Coleburn, Fix bug of not reseting error condition for retry of operation.
rem ---------------------------------------------------------------------------
rem EXIT CODES:
rem ----------
rem    0=ok; no errors
rem    1=Unsupported OS
rem    2=Usage error, e.g., Invalid command line options
rem    3=Invalid system environment settings
rem    4=Invalid cm3 environment settings
rem    5=Fatal error, e.g., Unable to create file/folder
rem    6=User abort
REM ===========================================================================



set _z_ExitCode=0
goto Welcome



:FN_DelzTempFile
:---------------
if not defined _z_TempFile goto :EOF
if exist %_z_TempFile% del %_z_TempFile%
goto :EOF



:FN_DelzErrLog
:-------------
if not defined _z_ErrLog goto :EOF
if exist %_z_ErrLog% del %_z_ErrLog%
goto :EOF



:Welcome
:-------
REM Identify this script.
echo.
echo ====== ----------------------------------
echo do-cm3, v1.31, 09/22/2013, Randy Coleburn
echo ====== ----------------------------------
echo.



:CheckVerOS
:----------
REM Ensure that the underlying operating system is supported by this script.
REM
if /I not "%OS%"=="Windows_NT" goto UnsupportedOS
if /I "%_cm3_CommandReady%"=="TRUE" if not "%_cm3_ThisOS%"=="" goto ver_END

set _cm3_ThisOS=
ver | (find /I "2003" >NUL:) && ((set _cm3_ThisOS=2003) & goto ver_2003)
ver | (find /I "XP"   >NUL:) && ((set _cm3_ThisOS=XP)   & goto ver_XP)
ver | (find /I "2000" >NUL:) && ((set _cm3_ThisOS=2000) & goto ver_2000)
ver | (find /I "NT"   >NUL:) && ((set _cm3_ThisOS=NT)   & goto ver_NT)

if NOT exist %SystemRoot%\system32\systeminfo.exe goto ver_NoSysInfo
FOR /F "usebackq tokens=3* delims=: " %%i IN (`systeminfo ^| find /I "OS Name"`) DO set _cm3_ThisOS_Long=%%i %%j
echo %_cm3_ThisOS_Long% | (find /I "Windows Vista"       >NUL:) && ((set _cm3_ThisOS=Vista) & goto ver_Vista)
echo %_cm3_ThisOS_Long% | (find /I "Windows 7"           >NUL:) && ((set _cm3_ThisOS=Win7)  & goto ver_7)
echo %_cm3_ThisOS_Long% | (find /I "Windows Server 2008" >NUL:) && ((set _cm3_ThisOS=2008)  & goto ver_2008)

:ver_NoSysInfo
ver | (find /I "Version 5.2"   >NUL:) && ((set _cm3_ThisOS=XP)    & goto ver_XP)
ver | (find /I "Version 6.0.6" >NUL:) && ((set _cm3_ThisOS=Vista) & goto ver_Vista)
ver | (find /I "Version 6.1.7" >NUL:) && ((set _cm3_ThisOS=Win7)  & goto ver_7)

:UnsupportedOS
echo.
echo %0
echo.
if "%_cm3_ThisOS%"=="" echo ERROR:  Unable to determine host operating system.
echo ERROR:  This script supports only Microsoft Windows 2000/XP/Vista/7 and Server 2003/2008.
if NOT "%_cm3_ThisOS%"=="" echo         This computer is operating:  (%_cm3_ThisOS%) %_cm3_ThisOS_Long%
ver
set _z_ExitCode=1
goto END

:FN_GetNumBitsThisOS
set _cm3_bitsOS=64
REG.EXE Query "HKLM\Hardware\Description\System\CentralProcessor\0" | (find /I "x86" >NUL:) && (set _cm3_bitsOS=32)
goto :EOF

:ver_NT
set _cm3_ThisOS_Long=Microsoft Windows NT
goto UnsupportedOS
goto ver_END

:ver_2000
set _cm3_ThisOS_Long=Microsoft Windows 2000
goto ver_END

:ver_XP
set _cm3_ThisOS_Long=Microsoft Windows XP
ver | (find /I "Version 5.2"   >NUL:) && ((set _cm3_bitsOS=64) & (set _cm3_ThisOS_Long=Microsoft Windows XP 64-bit Edition) & goto ver_END)
goto ver_END

:ver_Vista
if "%_cm3_ThisOS_Long%"=="" set _cm3_ThisOS_Long=Microsoft Windows Vista (%_cm3_bitsOS%-bit)
goto ver_END

:ver_7
if "%_cm3_ThisOS_Long%"=="" set _cm3_ThisOS_Long=Microsoft Windows 7 (%_cm3_bitsOS%-bit)
goto ver_END

:ver_2003
call :FN_GetNumBitsThisOS
set _cm3_ThisOS_Long=Microsoft Windows Server 2003 (%_cm3_bitsOS%-bit)
goto ver_END

:ver_2008
call :FN_GetNumBitsThisOS
set _cm3_ThisOS_Long=Microsoft Windows Server 2008 (%_cm3_bitsOS%-bit)
goto ver_END

:ver_END
rem echo _cm3_ThisOS=%_cm3_ThisOS%
rem echo _cm3_ThisOS_Long=%_cm3_ThisOS_Long%
rem echo This computer is operating:  (%_cm3_ThisOS%) %_cm3_ThisOS_Long%



:Init
:----
REM Initialize variables.
if "%_cm3_ThisOS%"=="" goto UnsupportedOS
set _z_Answ=
set _z_Arg=
set _z_CM3Args=
set _z_CM3Failure=
set _z_Fail=
set _z_Group=
set _z_Package=
set _z_PkgInfo=
set _z_PkgPath=
set _z_PkgTree=
set _z_TMP1=
set _z_Skip=
set _z_TempFile=
set _z_NoPause=FALSE
set _z_Verbose=FALSE



:ParseParams
:-----------
REM Parse parameters, see Usage.
goto ExamineArg1

:NextArg
shift

:ExamineArg1
if "%1"=="" goto ArgEnd
if /I "%1"=="HELP" goto Help
if /I "%1"=="-HELP" goto Help
if /I "%1"=="?" goto Help
if /I "%1"=="-NOPAUSE" (set _z_NoPause=TRUE) & goto NextArg
if /I "%1"=="-VERBOSE" (set _z_Verbose=TRUE) & goto NextArg
if /I "%1"=="-SHOWTAGS" if "%_z_CM3Args%"=="" ((set _z_CM3Args=%1) & goto NextArg) ELSE goto Usage
if /I "%1"=="-P" goto Arg_P
if /I "%1"=="-SKIP" goto Arg_Skip
if /I "%1"=="-BUILDSHIP" (set _z_CM3Args=%_z_CM3Args% -build -ship) & goto NextArg

rem check to see if %1 is a group tag
if "%_z_Group%"=="" for %%a in (all base core min std) do if /I %%a==%1 set _z_Group=%1
if "%_z_Group%"=="" for %%a in (anim caltech-parser comm cvsup database demo devlib front game) do if /I %%a==%1 set _z_Group=%1
if "%_z_Group%"=="" for %%a in (gui juno m3devtool m3gdb m3gnudevtool math obliq tool webdev) do if /I %%a==%1 set _z_Group=%1
if /I "%_z_Group%"=="%1" goto NextArg

rem check to see if %1 is a cm3 compiler mode argument
if /I "%_z_CM3Args%"=="SHOWTAGS" echo ERROR:  Parameter "%1" not valid with "ShowTags". & goto Usage
set _z_Arg=
for %%a in (find depend realclean clean build ship buildship) do if /I %%a==%1 set _z_Arg=-%%a
for %%a in (-find -depend -realclean -clean -build -ship -buildship) do if /I %%a==%1 set _z_Arg=%%a
if "%_z_Arg%"=="" echo ERROR:  Unknown or unsupported argument:  "%1". & goto Usage
set _z_CM3Args=%_z_CM3Args% %_z_Arg%
goto NextArg

:Arg_P
rem we've seen -P, now get the path
shift
if "%1"=="" echo ERROR:  Missing path after -P argument. & goto Usage
set _z_PkgInfo=%1
goto NextArg

:Arg_Skip
rem we've seen -SKIP, now get the package name to be skipped
shift
if "%1"=="" echo ERROR:  Missing package name after -SKIP argument. & goto Usage
IF defined _z_Skip (set _z_Skip=%_z_Skip%; %1) ELSE (set _z_Skip=%1)
goto NextArg

:ArgEnd
rem no more parameters, so make sure we've got the minimum required
if /I not "%_z_CM3Args%"=="SHOWTAGS" if "%_z_Group%"=="" echo ERROR:  Must specify a package group, e.g., min, core, std, all, etc. & goto Usage
if "%_z_CM3Args%"=="" set _z_CM3Args=-build



:SetupCM3
:--------
REM Ensure CM3 command line environment has been setup.
REM
REM !**********************************************************************************************!
REM !*** This check can be skipped by setting _cm3_CommandReady=TRUE, (remove REM on next line) ***!
REM !**********************************************************************************************!
REM set _cm3_CommandReady=TRUE
REM !******************************************************************************************!
REM
if /I "%_cm3_CommandReady%"=="TRUE" goto FindPkgInfo
if not exist "c:\cm3\bin\cm3CommandShell.CMD" goto FatalSetupCM3
call c:\cm3\bin\cm3CommandShell.CMD SameWindow
@echo off
if /I not "%_cm3_CommandReady%"=="TRUE" goto FatalSetupCM3



:FindPkgInfo
:-----------
REM Cause _z_PkgInfo to represent the path to PkgInfo.txt.

rem ---first check to see if user-specified -P option is valid
if not "%_z_PkgInfo%"=="" call :FN_PkgInfo %_z_PkgInfo%
if not "%_z_PkgInfo%"=="" if exist "%_z_PkgInfo%" call :FN_FullPath %_z_PkgInfo% _z_PkgInfo & goto FindSourceTree
if not "%_z_PkgInfo%"=="" goto NoPkgInfo

rem ---next, see if located in current directory
if exist ".\PkgInfo.txt" call :FN_FullPath .\PkgInfo.txt _z_PkgInfo
if not "%_z_PkgInfo%"=="" if exist "%_z_PkgInfo%" goto FindSourceTree

rem ---next, see if located in parent of current directory
pushd ..
   if exist ".\PkgInfo.txt" call :FN_FullPath .\PkgInfo.txt _z_PkgInfo
popd
if not "%_z_PkgInfo%"=="" if exist "%_z_PkgInfo%" goto FindSourceTree

rem ---next, see if located in grandparent of current directory
pushd ..
   cd ..
   if exist ".\PkgInfo.txt" call :FN_FullPath .\PkgInfo.txt _z_PkgInfo
popd
if not "%_z_PkgInfo%"=="" if exist "%_z_PkgInfo%" goto FindSourceTree

:NoPkgInfo
echo ERROR:  Unable to locate "PkgInfo.txt" file.
set _z_ExitCode=5
goto END

:FN_PkgInfo
rem ensure _z_PkgInfo points to a file, not a path
rem echo dp=%~dp1
rem echo nx=%~nx1
if /I "%~nx1"=="PKGINFO.TXT" goto :EOF
set _z_PkgInfo=%~dp1
if not "%~nx1"=="" set _z_PkgInfo=%_z_PkgInfo%%~nx1\
set _z_PkgInfo=%_z_PkgInfo%PkgInfo.txt
rem echo %_z_PkgInfo%
goto :EOF



:FindSourceTree
:--------------
REM Locate the package source tree and store in _z_PkgTree.
set _z_PkgTree=

rem --- first try parent of %_z_PkgInfo%
call :FN_DriveAndPathOnly %_z_PkgInfo% _z_PkgTree
pushd %_z_PkgTree%
   cd ..
   set _z_PkgTree=%CD%\
popd
if exist "%_z_PkgTree%m3-sys\cm3\src" goto Prepare

rem --- next try current directory
set _z_PkgTree=%CD%\
if exist "%_z_PkgTree%m3-sys\cm3\src" goto Prepare

rem --- next try parent of current directory
pushd ..
   set _z_PkgTree=%CD%\
popd
if exist "%_z_PkgTree%m3-sys\cm3\src" goto Prepare

rem --- next try grandparent of current directory
pushd ..
   cd ..
   set _z_PkgTree=%CD%\
popd
if exist "%_z_PkgTree%m3-sys\cm3\src" goto Prepare

echo ERROR:  Unable to locate package source tree.
set _z_ExitCode=5
goto END



:Prepare
:-------
REM Create a tempory file containing the names of all packages to be processed.
set _z_TempFile="%CD%\Temp%RANDOM%.txt"
call :FN_DelzTempFile
set _z_ErrLog="%CD%\Temp%RANDOM%ErrLog.txt"
call :FN_DelzErrLog
echo CM3 ARGS = %_z_CM3Args%
echo  PkgInfo = %_z_PkgInfo%
echo Pkg Tree = %_z_PkgTree%
echo    Group = %_z_Group%
if defined _z_Skip echo SkipList = %_z_Skip%
if /I "%_z_NoPause%"=="TRUE" echo "NoPause" Option in Effect.
if /I "%_z_Verbose%"=="TRUE" echo "Verbose" Option in Effect.
echo.
if /I "%_z_CM3Args%"=="SHOWTAGS" goto ShowTags
echo Searching for packages in group "%_z_Group%" ...
echo ... one moment please ....
FOR /F "tokens=1* delims= " %%i in (%_z_PkgInfo%) do call :FN_CheckPkg %%i %%j
echo.
echo Packages to be processed:
echo ------------------------
if not defined _z_TempFile goto EOL_Prepare
if exist %_z_TempFile% type %_z_TempFile%

:EOL_Prepare
if defined _z_Skip echo But, skipping packages: %_z_Skip%
echo ---END-of-List---
echo.
if /I "%_z_NoPause%"=="TRUE" goto DoIt

:AskContinue
set /P _z_Answ=Do you want to continue (y=yes, n=no) ? 
if /I "%_z_Answ%"=="Y" goto DoIt
if /I "%_z_Answ%"=="N" ((set _z_ExitCode=6) & goto END)
goto AskContinue



:ShowTags
:--------
echo Enumerating group tags in  "%_z_PkgInfo%" ...
echo ... one moment please ...
FOR /F "tokens=1* delims= " %%i in (%_z_PkgInfo%) do call :FN_AddTags %%j
echo.
echo Group Tags:
echo ----------
for %%t in (%_z_TMP1%) do echo %%t
echo ---END-of-LIST---
echo.
if "%_z_Group%"=="" goto END
echo Enumerating group "%_z_Group%" ... one moment please ...
if /I not "%_z_Group%"=="ALL" set _z_TMP1=%_z_Group%
for %%t in (%_z_TMP1%) do call :FN_ShowGroupPkgs %%t
set _z_TMP1=
goto END

:FN_AddTags
rem %1 is list of tags for a given package
for %%z in (%*) do call :FN_ChkTag %%z
goto :EOF

:FN_ChkTag
rem %1 is a tag name; see if it is in the list, if not add it to the list
set _z_Answ=0
for %%y in (%_z_TMP1%) do if /I "%%y"=="%1" set _z_Answ=1
if "%_z_Answ%"=="0" set _z_TMP1=%_z_TMP1%;%1
goto :EOF

:FN_ShowGroupPkgs
rem %1 is group tag
set _z_Group=%1
echo.
echo Packages in Group="%_z_Group%":
echo ------------------------------------------------------------------------------
FOR /F "tokens=1* delims= " %%i in (%_z_PkgInfo%) do call :FN_EnumGroup %%i %%j
echo ---END-of-List---
echo.
goto :EOF

:FN_EnumGroup
rem %1=package, %2*=list of groups in this package
set _z_Package=
for %%a in (%*) do if /I %%a==%_z_Group% set _z_Package=%1
if /I "%_z_Group%"=="ALL" set _z_Package=%1
if not "%_z_Package%"=="" echo %_z_Package%
goto :EOF



:DoIt
:----
REM Process each of the packages named in the temporary file.
if not exist %_z_TempFile% goto END
set _z_CM3Failure=
call :FN_RemoveQuotes %_z_TempFile% _z_TMP1
FOR /F "tokens=1 delims=" %%i in (%_z_TMP1%) do call :FN_DoPkg %%i
echo.
if not "%_z_CM3Failure%"=="" echo WARNING:  One or packages experienced a failure.  See result detail above.& echo.& set _cm3_FAILURE=TRUE
if not exist %_z_ErrLog% goto END
echo ERROR LOG SUMMARY:
echo -----------------
if exist %_z_ErrLog% type %_z_ErrLog% & del %_z_ErrLog%
echo ---END-of-List---
echo.
goto END



:FatalSetupCM3
:-------------
set _z_ExitCode=3
echo ERROR:  Unable to successfully run "c:\cm3\bin\cm3SetupCmdEnv.CMD".
echo         Please edit this .CMD file to provide correct path.
goto END



:FN_FullPath
:-----------
REM Make a full (non-relative) path.
REM %1=relative path, %2=env var for result
set %2=%~f1
goto :EOF



:FN_DriveAndPathOnly
:-------------------
REM Convert %1 to a drive letter and path only.
REM %1=path, %2=env var for result
set %2=%~dp1
goto :EOF



:FN_RemoveQuotes
:---------------
REM Remove quotes surrounding path.
REM %1=quoted path, %2=env var for result
set %2=%~1
goto :EOF



:FN_CheckPkg
:-----------
REM If this package (%1) should be processed and can be found, add it to the temporary file.
REM %1=package, %2*=group tags for this package

rem ---first see if this package is tagged with the desired group
set _z_Package=
for %%a in (%*) do if /I %%a==%_z_Group% set _z_Package=%1
if /I "%_z_Group%"=="ALL" set _z_Package=%1
if "%_z_Package%"=="" goto :EOF

rem ---sometimes the package has a relative path in unix-style, so convert it to DOS-style
if not "%_z_Package%"=="" set _z_Package=%_z_Package:/=\%
rem echo normalized = %_z_Package%

rem ---make sure we can find this package in the source tree
set _z_PkgPath=
pushd %_z_PkgTree%

   rem ------first try the package itself as a relative path
   if exist "%_z_Package%\src" set _z_PkgPath=%_z_Package%

   rem ------if that doesn't work, look in the various m3-* folders
   if "%_z_PkgPath%"=="" for /f %%i in ('dir /b m3-* caltech*') do if exist "%%i\%_z_Package%\src" set _z_PkgPath=%%i\%_z_Package%

popd

rem ------if we found it, great, otherwise report we are skipping it
if not "%_z_PkgPath%"=="" goto foundPkg
echo WARNING:  Unable to locate package "%_z_Package%" in "%_z_PkgTree%"
echo           (this package will be skipped)
echo.
echo WARNING:  Package "%_z_Package%" was not found.>>%_z_ErrLog%
if /I "%_z_NoPause%"=="TRUE" goto :EOF
pause
goto :EOF

:foundPkg
echo %_z_PkgPath% >>%_z_TempFile%
if /I "%_z_Verbose%"=="TRUE" echo ### found "%_z_Group%" in (%*), at "%_z_PkgPath%"
goto :EOF



:FN_ChkSkip
:----------
REM %1 is a package name; see if it is in the skip list: if no goto %2, if yes goto %3
set _z_Answ=0
if not defined _z_Skip goto %2
for %%z in (%_z_Skip%) do if /I "%%z"=="%~nx1" set _z_Answ=1
IF "%_z_Answ%"=="0" ( goto %2 ) ELSE ( goto %3 )
echo ERROR: Failure in FN_ChkSkip function.
set _z_CM3Failure=STOP
set _z_ExitCode=5
goto :EOF



:FN_DoPkg
:--------
REM Process this package (%1).
if /I "%_z_CM3Failure%"=="STOP" goto :EOF
echo.
echo ------------------------------------------------------------------------------
call :FN_ChkSkip %1 OkToProcess SkipThisPkg
goto :EOF

:SkipThisPkg
echo --- skipping package "%1" ---
goto :EOF

:OkToProcess
echo --- processing package "%1" ---
pushd %_z_PkgTree%%1
   for %%z in (%_z_CM3Args%) do call :FN_DoCM3 %%z %1
popd
goto :EOF



:FN_DoCM3
:--------
REM Invoke CM3
REM %1=CM3 mode (e.g. -build, -ship, -find, -depend, -clean, -realclean)
REM %2=package
if /I "%_z_CM3Failure%"=="STOP" goto :EOF

:Retry
set _z_Fail=TRUE
(cm3 %1) && (set _z_Fail=)
if errorlevel 1 set _z_Fail=TRUE
if "%_z_Fail%"=="" goto :EOF
set _z_CM3Failure=TRUE
set _z_ExitCode=5
echo.
echo WARNING:  Encountered an error when processing package "%2" for "%1".
echo WARNING:  Errors in package "%2" for "%1">>%_z_ErrLog%
echo.
if /I "%_z_NoPause%"=="TRUE" goto :EOF

:AskRetry
set /P _z_Answ=Do you want to retry the failed operation (y=yes, n=no) ? 
echo.
if /I "%_z_Answ%"=="Y" (set _z_CM3Failure=) & (set _z_ExitCode=0) & goto Retry
if /I "%_z_Answ%"=="N" goto AskStop
goto AskRetry

:AskStop
set /P _z_Answ=Do you want to continue (y=yes, n=no) ? 
echo.
if /I "%_z_Answ%"=="Y" goto :EOF
if /I "%_z_Answ%"=="N" ((set _z_CM3Failure=STOP) & (set _z_ExitCode=6))
if /I "%_z_CM3Failure%"=="STOP" goto :EOF
goto AskStop
goto :EOF



:Help
:----
echo ---------------------------
echo --- HELP for do-cm3.CMD ---
echo ---------------------------
echo.
echo do-cm3.CMD is intended to replicate on Windows 2000/XP the functionality of the
echo various "do-cm3-*.sh" scripts.  
echo.
echo As such, it invokes cm3 on a group of packages.  Packages and their group 
echo associations are defined in the "PkgInfo.txt" file.  
echo.
echo The "PkgInfo.txt" file can be specified via the -P argument, or it is searched 
echo for in the current directory, then the parent and grandparent directory.
echo.
echo The package source tree is located relative to the "PkgInfo.txt" file (parent
echo folder), or in the current directory, or in the parent or grandparent folder.
goto U2



:Usage
:-----
set _z_ExitCode=2
echo =====   -----------------------------------------------------------------------

:U2
echo.
echo -----  ------------------------------------------------------------------------
echo Usage: do-cm3 {-help -showTags} [group-tag] {cm3args} {-noPause -verbose} 
echo -----         {-p path} {-skip pkgName}
echo        ------------------------------------------------------------------------
echo.
echo    -help  = display help, then exit.
echo.
echo -showTags = display list of all group tags found in PkgInfo.txt.
echo             If a group-tag is given, also list the packages in that group.
echo             No cm3 action is taken.
echo.
echo group-tag = must specify only one package group, e.g., min, core, std, all.
echo             (use showTags to discover list of all groups)
echo.
echo   cm3args = zero or more arguments to the cm3 builder, e.g., clean, build, ship
echo             Multiple arguments are possible.  They will be performed in the
echo             order given.  If no argument is given, "-build" is assumed.
echo             "-buildship" is shorthand for "-build" followed by "-ship".
echo.
echo  -noPause = Don't ask whether to continue; just keep on going.
echo.
echo  -verbose = Increase the amount of messages given.
echo.
echo   -p path = specify location of "PkgInfo.txt" file.
echo             (if not specified, searchs in path=(".\"; "..\"; "..\..")
echo.
echo -skip pkgName = if pkgName is in the specified group-tag, skip this package.
echo                 Multiple -skip argments may be given.
echo.


:END
:---
REM Remove environment variables and temporary files, then exit.
if not "%_z_ExitCode%"=="0" if /I "%_z_Verbose%"=="TRUE" set _z_
if /I "%_z_CM3Failure%"=="TRUE" set _cm3_CM3Failure=TRUE
if not "%_z_ExitCode%"=="0" set _cm3_CM3Failure=TRUE
set _z_Answ=
set _z_Arg=
set _z_CM3Args=
set _z_CM3Failure=
call :FN_DelzErrLog
set _z_ErrLog=
set _z_Fail=
set _z_Group=
set _z_NoPause=
set _z_Package=
set _z_PkgInfo=
set _z_PkgPath=
set _z_PkgTree=
set _z_Skip=
set _z_TMP1=
call :FN_DelzTempFile
set _z_TempFile=
set _z_Verbose=

echo ===END do-cm3===
echo on
@EXIT /B %_z_ExitCode%
