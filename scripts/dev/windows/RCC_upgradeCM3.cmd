@echo off
REM RCC_upgradeCM3, written by Randy Coleburn, 07/28/2009
REM ===========================================================================
REM Version History
REM ---------------
REM v1.0, 07/29/2009, R.Coleburn
REM v1.1, 08/02/2009, R.Coleburn, min is not sufficient, use m3front
REM v1.2, 08/03/2009, R.Coleburn, change cm3.cfg to replace HOST by "NT386"
REM v1.3, 10/29/2009, R.Coleburn, adapt to work with new cm3CommandShell.CMD.
REM v1.4, 01/13/2010, R.Coleburn, skip m3core, libm3, and mklib in 1st phase; using revised do-cm3.cmd.  Force argument keywords to be prefixed by "-".  Pass thru control args to do-cm3.cmd.  Add -all argument keyword.
REM v1.5, 01/15/2010, R.Coleburn, add extra checks at end of each stage to ensure new cm3.exe was produced and copied to target bin folder.
REM v1.6, 03/11/2010, R.Coleburn, add feature to search PATH env var when trying to locate root of cm3 installation
REM v1.7, 11/28/2010, R.Coleburn, add "-skip m3cc" to all build stages because this package isn't used on Windows
rem v1.8, 02/06/2013, R.Coleburn, Add error exit codes.  Fix bug of attempting cm3.exe install after build error.
rem v1.9, 09/08/2013, R.Coleburn, Don't skip mklib during phase 1.
rem v2.0, 01/13/2014, R.Coleburn, Adjust to conform to full pathnames for packages to be skipped.  Also skip caltech-parser test package.
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



:Init
:----
REM Initialize variables.
set _cm3_Answ=
set _cm3_Arg=
set _cm3_CM3Args=-realclean -clean -build -ship
set _cm3_CM3Failure=
set _cm3_ExitCode=0
set _cm3_Group=min
set _cm3_PkgInfo=
set _cm3_PkgPath=
set _cm3_PkgTree=
set _cm3_TMP1=%~dp0
set _cm3_TempFile=
set _cm3_NoPause=TRUE
set _cm3_Verbose=FALSE
set _cm3_CfgDone=FALSE
set _cm3_Repeat=1
set _cm3_DO=
set _z_ctrlArgs=
set _z_Stage3=min



:ParseParams
:-----------
REM Parse parameters, see Usage.
if "%1"=="" goto ArgEnd
goto ExamineArg1

:NextArg
shift

:ExamineArg1
if "%1"=="" goto ArgEnd
if /I "%1"=="HELP" goto Help
if /I "%1"=="-HELP" goto Help
if /I "%1"=="?" goto Help
if /I "%1"=="-PAUSE" set _cm3_NoPause=FALSE& goto NextArg
if /I "%1"=="-VERBOSE" set _cm3_Verbose=TRUE& goto NextArg
if /I "%1"=="-P" goto Arg_P
if /I "%1"=="-ALL" set _z_Stage3=all& goto NextArg
echo.
echo ERROR:  Unknown or unsupported argument:  %1 
goto Usage

:Arg_P
rem we've seen -P, now get the path
shift
if "%1"=="" echo ERROR:  Missing path after -P argument.  & goto Usage
set _cm3_PkgInfo=%1
goto NextArg

:ArgEnd
rem no more parameters, so make sure we've got the minimum required



:Welcome
:-------
REM Identify this script.
echo.
echo =============== ---------------------------------
echo  RCC_upgradeCM3, v2.0, 01/13/2014, Randy Coleburn
echo =============== ---------------------------------
echo.
if /I "%_z_NoPause%"=="TRUE" echo "NoPause" Option in Effect.
if /I "%_z_Verbose%"=="TRUE" echo "Verbose" Option in Effect.
if /I "%_z_Stage3%"=="MIN" (echo Stage3=produce minimal distribution.) ELSE (echo Stage3=produce complete distribution, i.e., all packages.)
echo.



:SetupCM3
:--------
REM Find root of CM3 installation, copy required files to bin, and Ensure CM3 command line environment has been setup.
if not "%CM3_ROOT%"=="" if exist "%CM3_ROOT%\bin\cm3.exe" if exist "%CM3_ROOT%\pkg" goto FoundRoot
if not "%CM3_ROOT%"=="" echo ERROR:  Specified CM3_ROOT (%CM3_ROOT%) is missing a required folder. & (set _cm3_ExitCode=4) & goto END

pushd ..
cd ..
cd ..
if exist "bin\cm3.exe" if exist "pkg" set CM3_ROOT=%CD%& popd & goto FoundRoot
cd ..
if exist "bin\cm3.exe" if exist "pkg" set CM3_ROOT=%CD%& popd & goto FoundRoot
if exist "C:\cm3\bin\cm3.exe" if exist "C:\cm3\pkg" set CM3_ROOT=C:\cm3& popd & goto FoundRoot
popd
rem otherwise, search the existing PATH environment variable to try and find the root of the cm3 installation
for %%F in (cm3.exe) do set CM3_ROOT=%%~dp$PATH:F..
if defined CM3_ROOT if exist "%CM3_ROOT%\bin\cm3.exe" call :FN_FQPathNoSpaces CM3_ROOT "%_cm3_Root%"
if defined CM3_ROOT if exist "%CM3_ROOT%\bin\cm3.exe" if exist "%CM3_ROOT%\pkg" goto FoundRoot
set CM3_ROOT=
echo ERROR:  Could not find root of CM3 installation.
set _cm3_ExitCode=4
goto END

:FoundRoot
echo CM3 Root = %CM3_ROOT%
echo.
set _cm3_DO=%_cm3_TMP1%do-cm3.cmd
if not exist "%_cm3_DO%" echo ERROR:  Missing script "%_cm3_DO%" & (set _cm3_ExitCode=5) & goto END
echo Copying .CMD scripts and documentation to %CM3_ROOT%\bin ...
for %%f in (Documentation_cm3Proj.pdf Documentation_cm3CommandShell.pdf Documentation_CM3StartIDE.pdf) do if exist "..\..\doc\%%f" copy /y "..\..\doc\%%f" "%CM3_ROOT%\bin"
for %%f in (Documentation_cm3Proj.htm Documentation_cm3CommandShell.htm Documentation_CM3StartIDE.htm) do if exist "..\..\doc\%%f" copy /y "..\..\doc\%%f" "%CM3_ROOT%\bin"
for %%f in (cm3CommandShell.CMD cm3StartIDE.CMD cm3Proj.cmd) do if exist "..\..\install\windows\%%f" copy /y "..\..\install\windows\%%f" "%CM3_ROOT%\bin"
for %%f in (cm3CommandShell.CMD cm3StartIDE.CMD cm3Proj.cmd) do if not exist "%CM3_ROOT%\bin\%%f" goto FatalSetupCM3
echo.
if /I "%_cm3_CommandReady%"=="TRUE" goto FindPkgInfo
echo Setting up environment variables for CM3 ...
echo.
call c:\cm3\bin\cm3CommandShell.CMD SameWindow
@echo off
if /I not "%_cm3_CommandReady%"=="TRUE" goto FatalSetupCM3



:FindPkgInfo
:-----------
REM Cause _cm3_PkgInfo to represent the path to PkgInfo.txt.
rem ---first check to see if user-specified -P option is valid
if not "%_cm3_PkgInfo%"=="" call :FN_PkgInfo %_cm3_PkgInfo%
if not "%_cm3_PkgInfo%"=="" if exist "%_cm3_PkgInfo%" call :FN_FullPath %_cm3_PkgInfo% _cm3_PkgInfo & goto FindSourceTree
if not "%_cm3_PkgInfo%"=="" goto NoPkgInfo

rem ---next, see if located in current directory
if exist ".\PkgInfo.txt" call :FN_FullPath .\PkgInfo.txt _cm3_PkgInfo
if not "%_cm3_PkgInfo%"=="" if exist "%_cm3_PkgInfo%" goto FindSourceTree

rem ---next, see if located in parent of current directory
pushd ..
if exist ".\PkgInfo.txt" call :FN_FullPath .\PkgInfo.txt _cm3_PkgInfo
popd
if not "%_cm3_PkgInfo%"=="" if exist "%_cm3_PkgInfo%" goto FindSourceTree

rem ---next, see if located in grandparent of current directory
pushd ..
cd ..
if exist ".\PkgInfo.txt" call :FN_FullPath .\PkgInfo.txt _cm3_PkgInfo
popd
if not "%_cm3_PkgInfo%"=="" if exist "%_cm3_PkgInfo%" goto FindSourceTree

:NoPkgInfo
echo ERROR:  Unable to locate PkgInfo.txt file.
set _cm3_ExitCode=5
goto END

:FN_PkgInfo
rem ensure _cm3_PkgInfo points to a file, not a path
rem echo dp=%~dp1
rem echo nx=%~nx1
if /I "%~nx1"=="PKGINFO.TXT" goto :EOF
set _cm3_PkgInfo=%~dp1
if not "%~nx1"=="" set _cm3_PkgInfo=%_cm3_PkgInfo%%~nx1\
set _cm3_PkgInfo=%_cm3_PkgInfo%PkgInfo.txt
rem echo %_cm3_PkgInfo%
goto :EOF



:FindSourceTree
:--------------
REM Locate the package source tree and store in _cm3_PkgTree.
set _cm3_PkgTree=

rem --- first try parent of %_cm3_PkgInfo%
call :FN_DriveAndPathOnly %_cm3_PkgInfo% _cm3_PkgTree
pushd %_cm3_PkgTree%
cd ..
set _cm3_PkgTree=%CD%\
popd
if exist "%_cm3_PkgTree%m3-sys\cm3\src" goto Prepare

rem --- next try current directory
set _cm3_PkgTree=%CD%\
if exist "%_cm3_PkgTree%m3-sys\cm3\src" goto Prepare

rem --- next try parent of current directory
pushd ..
set _cm3_PkgTree=%CD%\
popd
if exist "%_cm3_PkgTree%m3-sys\cm3\src" goto Prepare

rem --- next try grandparent of current directory
pushd ..
cd ..
set _cm3_PkgTree=%CD%\
popd
if exist "%_cm3_PkgTree%m3-sys\cm3\src" goto Prepare

echo ERROR:  Unable to locate package source tree.
set _cm3_ExitCode=5
goto END



:Prepare
:-------
set _z_ctrlArgs=
if "%_cm3_Verbose%"=="TRUE" set _z_ctrlArgs=%_z_ctrlArgs% -verbose
if "%_cm3_NoPause%"=="TRUE" set _z_ctrlArgs=%_z_ctrlArgs% -nopause
if /I not "%_cm3_CfgDone%"=="TRUE" call :FN_UpdateConfig
echo Creating config ...
if not exist "%CM3_ROOT%\bin\config" mkdir %CM3_ROOT%\bin\config
if not exist "%CM3_ROOT%\bin\config" echo ERROR:  Unable to create folder "%CM3_ROOT%\bin\config" & (set _cm3_ExitCode=5) & goto END
copy /y %_cm3_PkgTree%m3-sys\cminstall\src\config\* %CM3_ROOT%\bin\config
if errorlevel 1 echo ERROR:  Problem copying files. & (set _cm3_ExitCode=5) & goto END
echo Creating "%CM3_ROOT%\bin\cm3.cfg" ...
if exist "%CM3_ROOT%\bin\cm3.cfg" del /f %CM3_ROOT%\bin\cm3.cfg
REM OLD: echo INSTALL_ROOT = path() ^& "/..">%CM3_ROOT%\bin\cm3.cfg
REM OLD: echo include(path() ^& "/config/NT386")>>%CM3_ROOT%\bin\cm3.cfg
copy /y %_cm3_PkgTree%m3-sys\cminstall\src\config\cm3.cfg %CM3_ROOT%\bin\cm3.cfg
if errorlevel 1 echo ERROR:  Problem copying files. & (set _cm3_ExitCode=5) & goto END
if not exist "%CM3_ROOT%\bin\cm3.cfg" echo ERROR:  Problem copying files. & (set _cm3_ExitCode=5) & goto END
if not "%_cm3_Verbose%"=="TRUE" goto SkipCFGlist
echo.
echo %CM3_ROOT%\bin\cm3.cfg
echo -------------------
type %CM3_ROOT%\bin\cm3.cfg
echo ------------------------------------------------------------------------------
:SkipCFGlist
echo.
echo ========================================================================
echo STAGE-1:  BUILDING "front", EXCEPT FOR "m3core", "libm3", and "m3cc"
echo ========================================================================
echo.
call %_cm3_DO% front -skip "m3-libs\m3core" -skip "m3-libs\libm3" -skip "m3-sys\m3cc" -realclean -clean -build -ship %_z_ctrlArgs%
@echo off
call :FN_FinishStage
if "%_cm3_CM3Failure%"=="TRUE" goto END
echo.
echo ========================================================================
echo STAGE-2:  REPEATING BUILD, but this time ALL of "front", except "m3cc"
echo ========================================================================
echo.
call %_cm3_DO% front -skip "m3-sys\m3cc" -realclean -clean -build -ship %_z_ctrlArgs%
@echo off
call :FN_FinishStage
if "%_cm3_CM3Failure%"=="TRUE" goto END
echo.
echo ========================================================================
echo STAGE-3:  BUILDING DISTRIBUTION "%_z_Stage3%", except "m3cc"
echo ========================================================================
echo.
rem call %_cm3_DO% %_z_Stage3% -skip "m3-sys\m3cc" -realclean -clean -build -ship %_z_ctrlArgs%
call %_cm3_DO% %_z_Stage3% -skip "m3-sys\m3cc" -skip "caltech-parser\parserlib\parserlib\test" -realclean -clean -build -ship %_z_ctrlArgs%
@echo off
call :FN_FinishStage
if "%_cm3_CM3Failure%"=="TRUE" goto END
echo.
echo ========================================================================
cm3 -version
echo ========================================================================
goto END



:FN_FinishStage
:--------------
if "%_cm3_CM3Failure%"=="TRUE" echo ...skipping installation of cm3.exe due to errors... & goto :EOF
if not exist "%_cm3_PkgTree%m3-sys\cm3\NT386\cm3.exe" goto StageFailure
echo ...installing new cm3.exe as "%CM3_ROOT%\bin\cm3.exe" ...
if exist "%CM3_ROOT%\bin\cm3.exe" del /f "%CM3_ROOT%\bin\cm3.exe"
if exist "%CM3_ROOT%\bin\cm3.pdb" del /f "%CM3_ROOT%\bin\cm3.pdb"
copy "%_cm3_PkgTree%m3-sys\cm3\NT386\cm3.exe" "%CM3_ROOT%\bin\cm3.exe"
if exist "%_cm3_PkgTree%m3-sys\cm3\NT386\cm3.pdb" copy "%_cm3_PkgTree%m3-sys\cm3\NT386\cm3.pdb" "%CM3_ROOT%\bin\cm3.pdb"
if not exist "%CM3_ROOT%\bin\cm3.exe" goto StageFailure
goto :EOF

:StageFailure
set _cm3_CM3Failure=TRUE
set _z_ExitCode=5
echo ERROR: Stage Failure--cm3.exe was not created successfully!  Aborting...
goto :EOF



:FN_UpdateConfig
:---------------
echo Removing obsolete configuration files from %CM3_ROOT%\bin ...
for %%f in (%_cm3_PkgTree%m3-sys\cminstall\src\config\*) do call :Zap %%f %CM3_ROOT%\bin
echo.
set _cm3_CfgDone=TRUE
goto :EOF

:Zap
rem 1=filespec, 2=folder
if exist "%2\%~nx1" del /f "%2\%~nx1"
goto :EOF



:FatalSetupCM3
:-------------
set _z_ExitCode=3
echo ERROR:  Unable to successfully run "c:\cm3\bin\cm3CommandShell.CMD"
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




:Help
:----
echo -----------------------------------
echo --- HELP for RCC_upgradeCM3.CMD ---
echo -----------------------------------
echo.
echo RCC_upgradeCM3.CMD is intended to replicate on Windows 2000/XP/7 the 
echo functionality of the "upgrade.sh" script.  
echo.
echo As such, it invokes cm3 on a group of packages.  Packages and their group 
echo associations are defined in the PkgInfo.txt file.  
echo.
echo The PkgInfo.txt file can be specified via the -P argument, or it is searched 
echo for in the current directory, then the parent and grandparent directory.
echo.
echo The package source tree is located relative to the PkgInfo.txt file (parent
echo folder), or in the current directory, or in the parent or grandparent folder.
echo.
echo Note that the upgrade process requires 3 build stages:  
echo    1. "front" group, minus packages "m3core", "libm3", and "m3cc"
echo    2. "front" group, minus packages "m3cc"
echo    3. a distribution (either min or all), minus packages "m3cc"
goto U2



:Usage
:-----
set _cm3_ExitCode=2
echo =====   -----------------------------------------------------------------------

:U2
echo.
echo -----  ------------------------------------------------------------------------
echo Usage: RCC_upgradeCM3 {-help -pause -verbose -all} {-p path}
echo -----  ------------------------------------------------------------------------
echo.
echo     -help  = display help, then exit.
echo.
echo     -pause = ask whether to continue, esp. if an error occurs.
echo              If not specified, the default is to keep on going, i.e., noPause.
echo              (Be careful using this option because at each stage you are given
echo               the option to continue.  Skipping one stage, but proceeding with
echo               the next may cause unpredictable results.)
echo.
echo   -verbose = Increase the amount of messages given.
echo.
echo       -all = build all packages (a complete distribution) in stage 3.
echo              If not specified, the default is to build a MINimal distribution.
echo.
echo    -p path = specify location of "PkgInfo.txt" file.
echo              (if not specified, searchs in path=(".\"; "..\"; "..\..")
echo.



:END
:---
REM Remove environment variables and temporary files, then exit.
if "%_cm3_CM3Failure%"=="TRUE" if "%_cm3_ExitCode%"=="0" set _cm3_ExitCode=5
if not "%_cm3_ExitCode%"=="0" if /I "%_cm3_Verbose%"=="TRUE" set _cm3_

rem echo _cm3_Answ=%_cm3_Answ%
set _cm3_Answ=

rem echo _cm3_Arg=%_cm3_Arg%
set _cm3_Arg=

rem echo _cm3_CfgDone=%_cm3_CfgDone%
set _cm3_CfgDone=

rem echo _cm3_CM3Args=%_cm3_CM3Args%
set _cm3_CM3Args=

rem echo _cm3_CM3Failure=%_cm3_CM3Failure%
set _cm3_CM3Failure=

rem echo _cm3_DO=%_cm3_DO%
set _cm3_DO=

rem echo _cm3_ErrLog=%_cm3_ErrLog%
if "%_cm3_ErrLog%"=="" goto E1
if exist %_cm3_ErrLog% del %_cm3_ErrLog%
set _cm3_ErrLog=
:E1

rem echo _cm3_Group=%_cm3_Group%
set _cm3_Group=

rem echo _cm3_NoPause=%_cm3_NoPause%
set _cm3_NoPause=

rem echo _cm3_PkgInfo=%_cm3_PkgInfo%
set _cm3_PkgInfo=

rem echo _cm3_PkgPath=%_cm3_PkgPath%
set _cm3_PkgPath=

rem echo _cm3_PkgTree=%_cm3_PkgTree%
set _cm3_PkgTree=

rem echo _cm3_TMP1=%_cm3_TMP1%
set _cm3_TMP1=

rem echo _cm3_Repeat=%_cm3_Repeat%
set _cm3_Repeat=

rem echo _cm3_TempFile=%_cm3_TempFile%
if "%_cm3_TempFile%"=="" goto E2
if exist %_cm3_TempFile% del %_cm3_TempFile%
set _cm3_TempFile=
:E2

rem echo _cm3_Verbose=%_cm3_Verbose%
set _cm3_Verbose=

rem echo _z_ctrlArgs=%_z_ctrlArgs%
set _z_ctrlArgs=

echo ===END RCC_upgradeCM3===
echo on
@EXIT /B %_cm3_ExitCode%
