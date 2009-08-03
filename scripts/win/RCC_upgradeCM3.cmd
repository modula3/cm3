@echo off
REM RCC_upgradeCM3, written by Randy Coleburn, 07/28/2009
REM ===========================================================================
REM Version History
REM ---------------
REM v1.0, 07/29/2009, R.Coleburn
REM v1.1, 08/02/2009, R.Coleburn, min is not sufficient, use m3front
REM ===========================================================================

:Init
:----
REM Initialize variables.
set CM3_Answ=
set CM3_Arg=
set CM3_CM3Args=-realclean -clean -build -ship
set CM3_CM3Failure=
set CM3_Group=min
set CM3_Pkg=
set CM3_PkgInfo=
set CM3_PkgPath=
set CM3_PkgTree=
set CM3_TMP1=%~dp0
set CM3_TempFile=
set CM3_NoPause=TRUE
set CM3_Verbose=FALSE
set CM3_CfgDone=FALSE
set CM3_Repeat=1
set CM3_DO=



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
if /I "%1"=="NOPAUSE" set CM3_NoPause=TRUE& goto NextArg
if /I "%1"=="VERBOSE" set CM3_Verbose=TRUE& goto NextArg
if /I "%1"=="-P" goto Arg_P
echo ERROR:  Unknown or unsupported argument:  %1
goto Usage

:Arg_P
rem we've seen -P, now get the path
shift
if "%1"=="" echo ERROR:  Missing path after -P argument. & goto Usage
set CM3_PkgInfo=%1
goto NextArg

:ArgEnd
rem no more parameters, so make sure we've got the minimum required



:Welcome
:-------
REM Identify this script.
echo.
echo =============== ---------------------------------
echo  RCC_upgradeCM3, v1.10, 8/02/2009, Randy Coleburn
echo =============== ---------------------------------
echo.



:SetupCM3
:--------
REM Find root of CM3 installation, copy required files to bin, and Ensure CM3 command line environment has been setup.
if not "%CM3_ROOT%"=="" if exist "%CM3_ROOT%\bin" if exist "%CM3_ROOT%\pkg" goto FoundRoot
if not "%CM3_ROOT%"=="" echo ERROR:  Specified CM3_ROOT (%CM3_ROOT%) is missing a required folder. & goto END
pushd ..
cd ..
cd ..
if exist "bin" if exist "pkg" set CM3_ROOT=%CD%& popd & goto :FoundRoot
cd ..
if exist "bin" if exist "pkg" set CM3_ROOT=%CD%& popd & goto :FoundRoot
if exist "C:\cm3\bin" if exist "C:\cm3\pkg" set CM3_ROOT=C:\cm3& popd & goto :FoundRoot
popd
echo ERROR:  Could not find root of CM3 installation.
goto END

:FoundRoot
echo CM3 Root = %CM3_ROOT%
echo.
set CM3_DO=%CM3_TMP1%do-cm3.cmd
if not exist "%CM3_DO%" echo ERROR:  Missing script "%CM3_DO%" & goto END
echo Copying .CMD scripts and documentation to %CM3_ROOT%\bin ...
for %%f in (Documentation_cm3Proj.pdf Documentation_CM3SetupCmdEnv.pdf Documentation_CM3StartIDE.pdf) do if exist "..\%%f" copy /y "..\%%f" "%CM3_ROOT%\bin"
for %%f in (Documentation_cm3Proj.htm Documentation_CM3SetupCmdEnv.htm Documentation_CM3StartIDE.htm) do if exist "..\%%f" copy /y "..\%%f" "%CM3_ROOT%\bin"
for %%f in (cm3Proj.cmd cm3SetupCmdEnv.cmd cm3StartIDE.CMD) do if exist "..\%%f" copy /y "..\%%f" "%CM3_ROOT%\bin"
for %%f in (cm3Proj.cmd cm3SetupCmdEnv.cmd cm3StartIDE.CMD) do if not exist "%CM3_ROOT%\bin\%%f" goto FatalSetupCM3
echo.
echo Setting up environment variables for CM3 ...
echo.
call c:\cm3\bin\cm3SetupCmdEnv.CMD
@echo off
if /I not "%CM3_DoneSetup%"=="TRUE" goto FatalSetupCM3



:FindPkgInfo
:-----------
REM Cause CM3_PkgInfo to represent the path to PkgInfo.txt.
rem ---first check to see if user-specified -P option is valid
if not "%CM3_PkgInfo%"=="" call :FN_PkgInfo %CM3_PkgInfo%
if not "%CM3_PkgInfo%"=="" if exist "%CM3_PkgInfo%" call :FN_FullPath %CM3_PkgInfo% CM3_PkgInfo & goto FindSourceTree
if not "%CM3_PkgInfo%"=="" goto NoPkgInfo

rem ---next, see if located in current directory
if exist ".\PkgInfo.txt" call :FN_FullPath .\PkgInfo.txt CM3_PkgInfo
if not "%CM3_PkgInfo%"=="" if exist "%CM3_PkgInfo%" goto FindSourceTree

rem ---next, see if located in parent of current directory
pushd ..
if exist ".\PkgInfo.txt" call :FN_FullPath .\PkgInfo.txt CM3_PkgInfo
popd
if not "%CM3_PkgInfo%"=="" if exist "%CM3_PkgInfo%" goto FindSourceTree

rem ---next, see if located in grandparent of current directory
pushd ..
cd ..
if exist ".\PkgInfo.txt" call :FN_FullPath .\PkgInfo.txt CM3_PkgInfo
popd
if not "%CM3_PkgInfo%"=="" if exist "%CM3_PkgInfo%" goto FindSourceTree

:NoPkgInfo
echo ERROR:  Unable to locate PkgInfo.txt file.
goto END

:FN_PkgInfo
rem ensure CM3_PkgInfo points to a file, not a path
rem echo dp=%~dp1
rem echo nx=%~nx1
if /I "%~nx1"=="PKGINFO.TXT" goto :EOF
set CM3_PkgInfo=%~dp1
if not "%~nx1"=="" set CM3_PkgInfo=%CM3_PkgInfo%%~nx1\
set CM3_PkgInfo=%CM3_PkgInfo%PkgInfo.txt
rem echo %CM3_PkgInfo%
goto :EOF



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
if /I not "%CM3_CfgDone%"=="TRUE" call :FN_UpdateConfig
echo Creating config ...
if not exist "%CM3_ROOT%\bin\config" mkdir %CM3_ROOT%\bin\config
if not exist "%CM3_ROOT%\bin\config" echo ERROR:  Unable to create folder "%CM3_ROOT%\bin\config" & goto END
copy /y %CM3_PkgTree%m3-sys\cminstall\src\config-no-install\* %CM3_ROOT%\bin\config
if errorlevel 1 echo ERROR:  Problem copying files. & goto END
if exist "%CM3_ROOT%\bin\cm3.cfg" del /f %CM3_ROOT%\bin\cm3.cfg
echo INSTALL_ROOT = path() ^& "/..">%CM3_ROOT%\bin\cm3.cfg
echo include(path() ^& "/config/" ^& HOST)>>%CM3_ROOT%\bin\cm3.cfg
echo.
echo %CM3_ROOT%\bin\cm3.cfg
echo -------------------
type %CM3_ROOT%\bin\cm3.cfg
echo ------------------------------------------------------------------------------
echo.
echo ========================================================================
echo STAGE-1:  Building CM3 Compiler
echo ========================================================================
echo.
call %CM3_DO% front -realclean -clean -build -ship nopause
@echo off
echo.
echo ========================================================================
echo STAGE-2:  REPEATING BUILD to Ensure New Compiler Is Used to Build Itself
echo ========================================================================
echo.
call %CM3_DO% front -realclean -clean -build -ship nopause
@echo off
echo.
echo ========================================================================
echo STAGE-3:  Building minimal distribution "min"
echo ========================================================================
echo.
call %CM3_DO% min -realclean -clean -build -ship nopause
@echo off
echo.
echo ========================================================================
cm3 -version
echo ========================================================================
goto END

:FN_UpdateConfig
:---------------
echo Removing obsolete configuration files from %CM3_ROOT%\bin ...
for %%f in (%CM3_PkgTree%m3-sys\cminstall\src\config-no-install\*) do call :Zap %%f %CM3_ROOT%\bin
for %%f in (%CM3_PkgTree%m3-sys\cminstall\src\config\*) do call :Zap %%f %CM3_ROOT%\bin
echo.
set CM3_CfgDone=TRUE
goto :EOF

:Zap
rem 1=filespec, 2=folder
if exist "%2\%~nx1" del /f "%2\%~nx1"
goto :EOF



:FatalSetupCM3
:-------------
echo ERROR:  Unable to successfully run "c:\cm3\bin\cm3SetupCmdEnv.CMD"
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
echo RCC_upgradeCM3.CMD is intended to replicate on Windows 2000/XP the 
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
goto U2



:Usage
:-----
echo =====   -----------------------------------------------------------------------
:U2
echo.
echo -----  ------------------------------------------------------------------------
echo Usage: RCC_upgradeCM3 {help noPause verbose} {-p path}
echo -----  ------------------------------------------------------------------------
echo.
echo     help  = display help, then exit.
echo.
echo   noPause = Don't ask whether to continue; just keep on going.
echo.
echo   verbose = Increase the amount of messages given.
echo.
echo   -p path = specify location of "PkgInfo.txt" file.
echo             (if not specified, searchs in path=(".\"; "..\"; "..\..")
echo.


:END
:---
REM Remove environment variables and temporary files, then exit.

rem echo CM3_Answ=%CM3_Answ%
set CM3_Answ=

rem echo CM3_Arg=%CM3_Arg%
set CM3_Arg=

rem echo CM3_CfgDone=%CM3_CfgDone%
set CM3_CfgDone=

rem echo CM3_CM3Args=%CM3_CM3Args%
set CM3_CM3Args=

rem echo CM3_CM3Failure=%CM3_CM3Failure%
set CM3_CM3Failure=

rem echo CM3_DO=%CM3_DO%
set CM3_DO=

rem echo CM3_ErrLog=%CM3_ErrLog%
if "%CM3_ErrLog%"=="" goto E1
if exist %CM3_ErrLog% del %CM3_ErrLog%
set CM3_ErrLog=
:E1

rem echo CM3_Group=%CM3_Group%
set CM3_Group=

rem echo CM3_NoPause=%CM3_NoPause%
set CM3_NoPause=

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

rem echo CM3_Repeat=%CM3_Repeat%
set CM3_Repeat=

rem echo CM3_TempFile=%CM3_TempFile%
if "%CM3_TempFile%"=="" goto E2
if exist %CM3_TempFile% del %CM3_TempFile%
set CM3_TempFile=
:E2

rem echo CM3_Verbose=%CM3_Verbose%
set CM3_Verbose=

echo ===END do-cm3===
echo on
