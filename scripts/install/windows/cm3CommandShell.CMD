@echo off
rem ===================  ------------------------------------------------------
rem cm3CommandShell.CMD, written by R.C.Coleburn
rem ===================  ------------------------------------------------------
rem
rem ---------------------------------------------------------------------------
rem This command script file is used to open a new Windows Command Shell 
rem (cmd.exe) and prepare its environment for use with Critical Mass Modula-3 
rem (CM3).  Can also be used to display or modify the CM3 environment settings 
rem when invoked from an extant CM3 command shell window.  Can also start the
rem Integrated Development Environment (IDE).
rem ---------------------------------------------------------------------------
rem VERSION HISTORY:
rem ---------------
rem v1.00, 10/26/2009, R.Coleburn, Initial Release.  This version is an "improved" replacement for my prior "cm3SetupCmdEnv.CMD" script.  It also adds support for Windows Vista.
rem v1.10, 03/11/2010, R.Coleburn, check location of the running script, and also the PATH environment variable, as a potential locations for "bin/cm3.exe" when looking for the root of the cm3 installation.
rem v1.20, 09/12/2010, R.Coleburn, Make Visual Studio 2010 Express the default. Adjust to deal with Vista and Windows7 OS identification.
rem v1.30, 02/06/2013, R.Coleburn, Make Visual Studio 2012 Express the default.  Improve OS detection.
rem ---------------------------------------------------------------------------
rem EXIT CODES:
rem ----------
rem    0=ok; no errors
rem    1=Unsupported OS
rem    2=Usage error, e.g., Invalid command line options
rem    3=Invalid system environment settings
rem    4=Invalid cm3 environment settings
rem    5=Fatal error, e.g., Unable to create file/folder
rem ---------------------------------------------------------------------------
REM CM3 Historical Info:
rem -------------------
rem    The Critical Mass v4.1 commercial release was known as "Reactor", owing 
rem    to its flagship Integrated Development Environment (IDE), "Reactor.EXE".
REM    The IDE for the open source Critical Mass Modula-3 release is "cm3IDE.EXE".
REM ---------------------------------------------------------------------------



REM !!!===!!!===!!!===!!!===!!!===!!!===!!!===!!!===!!!===!!!===!!!===!!!===!!!
REM !!!                                                                     !!!
REM !!! IMPORTANT:  Change Defaults As Warranted.  See Explanation below.   !!!
REM !!!                                                                     !!!
REM !!!===!!!===!!!===!!!===!!!===!!!===!!!===!!!===!!!===!!!===!!!===!!!===!!!

:DEFAULTS
:--------
set _cm3_VCSetup="C:\Program Files (x86)\Microsoft Visual Studio 11.0\VC\vcvarsall.bat" x86
set _cm3_SDKSetup=
REM ---------------------------------------------------------------------------
REM EXPLANATION (_cm3_VCSetup)
REM    Set to absolute path of BAT/CMD that sets up Microsoft Visual Studio/C/C++
REM    Set to empty to disable calling this auto setup routine.
REM    Examples:
REM       set _cm3_VCSetup="C:\Program Files (x86)\Microsoft Visual Studio 11.0\VC\vcvarsall.bat" x86
REM       set _cm3_VCSetup="C:\Program Files (x86)\Microsoft Visual Studio 10.0\VC\vcvarsall.bat" x86
REM       set _cm3_VCSetup="C:\Program Files\Microsoft Visual Studio 9.0\VC\vcvarsall.bat" x86
REM       set _cm3_VCSetup="C:\Program Files\Microsoft Visual Studio 8\VC\bin\VCVars32.bat"
REM       set _cm3_VCSetup="C:\Program Files\Microsoft Visual C++ Toolkit 2003\VCVars32.bat"
REM       set _cm3_VCSetup="C:\Program Files\Microsoft Visual Studio\VC98\Bin\VCVars32.bat"
REM ---------------------------------------------------------------------------
REM EXPLANATION (_cm3_SDKSetup)
REM    Set to absolute command line for establishing the Microsoft Platform SDK 
REM    environment settings.  Set it to empty to disable calling this routine.
REM    Example: set _cm3_SDKSetup="C:\Program Files\Microsoft Platform SDK for Windows Server 2003 R2\SetEnv.CMD" /XP32 /RETAIL
REM ---------------------------------------------------------------------------
REM !!!===!!!===!!!===!!!===!!!===!!!===!!!===!!!===!!!===!!!===!!!===!!!===!!!



:Welcome
:-------
set _cm3_ExitCode=0
echo.
echo +------------------------------------------------------+
echo ^|                 cm3CommandShell.CMD                  ^|
echo ^|                 -------------------                  ^|
echo ^|  v1.30, February 6, 2012, written by Randy Coleburn  ^|
echo +------------------------------------------------------+
echo.



:CheckVerOS
:----------
REM Ensure that the underlying operating system is supported by this script.
REM

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
set _cm3_ExitCode=1
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
if "%_cm3_ThisOS%"=="" goto UnsupportedOS
if "%_cm3_IDE%"=="" if defined CM3_IDE for %%a in (cm3IDE Reactor) do if /I "%%a"=="%CM3_IDE%" set _cm3_IDE=%%a
if "%_cm3_IDE%"=="" set _cm3_IDE=cm3IDE
if /I "%_cm3_IDE%"=="cm3IDE" if defined CM3_IDE_HOME if exist "%CM3_IDE_HOME%" set _cm3_HOME=%CM3_IDE_HOME%
if /I "%_cm3_IDE%"=="Reactor" if defined REACTOR_HOME if exist "%REACTOR_HOME%" set _cm3_HOME=%REACTOR_HOME%

rem if CM3_ROOT env var is defined and valid, use it as root of cm3 installation
if "%_cm3_Root%"=="" if defined CM3_ROOT if exist "%CM3_ROOT%\bin\cm3.exe" set _cm3_Root=%CM3_ROOT%

rem otherwise, see if this CMD file is being run out of the cm3\bin folder, and if so, use this to locate the root of the cm3 installation
if "%_cm3_Root%"=="" if exist "%~dp0..\bin\cm3.exe" call :FN_FQPathNoSpaces _cm3_Root "%~dp0.."
if defined _cm3_Root if NOT exist "%_cm3_Root%\bin\cm3.exe" set _cm3_Root=
if NOT "%_cm3_Root%"=="" goto LaunchControl

rem otherwise, search the existing PATH environment variable to try and find the root of the cm3 installation
for %%F in (cm3.exe) do set _cm3_Root=%%~dp$PATH:F..
if defined _cm3_Root if exist "%_cm3_Root%\bin\cm3.exe" call :FN_FQPathNoSpaces _cm3_Root "%_cm3_Root%"
if defined _cm3_Root if NOT exist "%_cm3_Root%\bin\cm3.exe" set _cm3_Root=

rem otherwise, we will default later on to "%SystemDrive%\cm3" for _cm3_Root



:LaunchControl
:-------------
REM The goal here is to open up a new command prompt window the first time we 
REM are able to successfully set up the CM3 command shell environment, but to 
REM keep this same window for subsequent invocations of this script.  Also, 
REM for use of the HELP, SHOW, and RESET options, there is no need to open a 
REM new window.
REM _cm3_LaunchNewWindow tells whether a new window should be created.
REM _cm3_Relaunch captures the script invocation command so that it is not 
REM lost due to subsequent 'shift' operations.
REM
rem echo !!! LaunchControl !!!
rem Assume we need a new window.
set _cm3_LaunchNewWindow=TRUE
rem If _cm3_CommandReady is defined, we know the script has been run before in this environment, so no need for a new window.
if defined _cm3_CommandReady set _cm3_LaunchNewWindow=FALSE
rem Capture the script invocation command in _cm3_Relaunch
call :FN_FQPathNoSpaces _cm3_Relaunch "%~0"
rem On a relaunch, the first command line option is 'MaxWin', so if we see this, we know this instance represents a relaunch in a new window, so no need to launch again.  Also, no need to parse parameters again.
if "%1"=="MaxWin" shift && set _cm3_LaunchNewWindow=FALSE&& goto SetCM3Env
goto InitEnviron



:FN_KillTempVars
:---------------
REM Be careful not to remove vars that must survive, e.g., _cm3_CommandReady
set _cm3_bitsOS=
set _cm3_CD=
set _cm3_CommonProfile=
set _cm3_DocsHome=
set _cm3_HOME=
set _cm3_IDE=
set _cm3_LaunchNewWindow=
set _cm3_PriorHome=
set _cm3_PriorIDE=
set _cm3_Relaunch=
set _cm3_Root=
set _cm3_SDKSetup=
set _cm3_SeenHomeOpt=
set _cm3_SeenIDEOpt=
set _cm3_SeenRootOpt=
set _cm3_SeenWinOpt=
set _cm3_startIDE=
set _cm3_Temp=
set _cm3_ThisOS=
set _cm3_ThisOS_Long=
set _cm3_VCSetup=
goto :EOF



:FN_FQPathNoSpaces
:-----------------
REM Function (FQPathNoSpaces)
REM params:  1=env var name, 2=path surrounded by quotes
REM result:  env var (1) is set to the fully qualified equiv of path (2), but this path will use the mangled short equivalent if it contains spaces.
REM
set %1=%~f2
echo %~f2|find " ">NUL:&&set %1=%~fs2
goto :EOF



:FN_RemovePath
:-------------
REM Function (RemovePath)
REM params:  1=searchPath
REM result:  remove all occurances of searchPath from PATH env var
REM
call :FN_RemPath2 "set PATH=%%PATH:%~1;=%%"
call :FN_RemPath2 "set PATH=%%PATH:;%~1=%%"
goto :EOF
:FN_RemPath2
%~1
goto :EOF



:FN_ShowSysEnv
:-------------
echo System Environment:
echo ------------------
ver
echo.
echo                 OS = %_cm3_ThisOS% (%_cm3_ThisOS_Long%)
echo        SystemDrive = %SystemDrive%
echo         SystemRoot = %SystemRoot%
echo    SystemDirectory = %SystemDirectory%
echo       ProgramFiles = %ProgramFiles%
echo      CommonProfile = %_cm3_CommonProfile%
echo        UserProfile = %UserProfile%
echo               TEMP = %TEMP%
echo                TMP = %TMP%
echo        Current Dir = %_cm3_CD%\
echo.
echo PATH=%PATH%
goto :EOF



:FN_ShowCM3Env
:-------------
REM Displays the current environment settings for cm3
REM
echo cm3 Environment:
echo ---------------
echo.
echo        CM3_ROOT = %CM3_ROOT%
echo         CM3_BIN = %CM3_BIN%
echo         CM3_PKG = %CM3_PKG%
echo         CM3_IDE = %CM3_IDE%
echo     CM3_IDE_EXE = %CM3_IDE_EXE%
if /I "%CM3_IDE%"=="cm3IDE" echo    CM3_IDE_HOME = %CM3_IDE_HOME%
if /I "%CM3_IDE%"=="Reactor" echo    REACTOR_HOME = %REACTOR_HOME%
echo.
if defined CM3_BIN if exist "%CM3_BIN%\cm3.exe" "%CM3_BIN%\cm3.exe" -version
if "%_cm3_CommandReady%"=="TRUE" echo === Command Prompt Ready for CM3 ===&echo.
goto :EOF



:InitEnviron
:-----------
REM Initialize the system environment.
REM
rem echo !!! InitEnviron !!!
if "%SystemDrive%"=="" set SystemDrive=C:
if not exist "%SystemDrive%\" goto BadEnviron

if "%SystemRoot%"=="" set SystemRoot=%SystemDrive%\Windows
if not exist "%SystemRoot%" set SystemRoot=%SystemDrive%\WINNT
if not exist "%SystemRoot%" set SystemRoot=%SystemDrive%\WINDOWS
if not exist "%SystemRoot%" goto BadEnviron

if "%SystemDirectory%"=="" set SystemDirectory=%SystemRoot%\system32
if not exist "%SystemDirectory%" goto BadEnviron

if "%ProgramFiles%"=="" set ProgramFiles=%SystemDrive%\Program Files
if not exist "%ProgramFiles%" goto BadEnviron

if "%UserProfile%"=="" goto BadEnviron
if not exist "%UserProfile%" goto BadEnviron

if /I not "%_cm3_ThisOS%"=="Vista" if /I not "%_cm3_ThisOS%"=="Win7" if /I not "%_cm3_ThisOS%"=="2008" goto PreVistaEnviron

:VistaOr7Environ
if "%Public%"=="" set Public=%SystemDrive%\Users\Public
if not exist "%Public%" goto BadEnviron
set _cm3_CommonProfile=%Public%
if "%TEMP%"=="" set TEMP=%UserProfile%\AppData\Local\Temp
if not exist "%TEMP%" goto BadEnviron
goto FinishEnviron

:PreVistaEnviron
if "%AllUsersProfile%"=="" set AllUsersProfile=%SystemDrive%\Documents and Settings\All Users
if not exist "%AllUsersProfile%" goto BadEnviron
set _cm3_CommonProfile=%AllUsersProfile%
if "%TEMP%"=="" set TEMP=%UserProfile%\Local Settings\Temp
if not exist "%TEMP%" goto BadEnviron

:FinishEnviron
call :FN_FQPathNoSpaces TEMP "%TEMP%"
if "%TMP%"==""  set TMP=%TEMP%
if not exist "%TMP%" set TMP=%TEMP%
call :FN_FQPathNoSpaces TMP "%TMP%"
set _cm3_CD=%cd%
if "%_cm3_CD%"=="" goto FatalError
rem remove trailing "\" if any
if "%_cm3_CD:~0,-1%\"=="%_cm3_CD%" set _cm3_CD=%_cm3_CD:~0,-1%



:ParseOptions
:------------
REM Parse command line options, see Usage.
rem echo !!! ParseOptions !!!
if defined _cm3_IDE set _cm3_PriorIDE=%_cm3_IDE%
if defined _cm3_HOME set _cm3_PriorHome=%_cm3_HOME%
goto ExamineOpt1

:NextOpt
shift

:ExamineOpt1
if "%1"=="" goto OptEnd
if /I "%1"=="?" goto OptHelp
for %%a in (Help Show Reset Root Home IDE StartIDE SameWindow NewWindow) do if /I %%a==%1 goto Opt%%a
goto OptUnexpected

:OptHelp
shift
if not "%1"=="" echo Ignoring extra command line options: %1 %2 %3 %4 %5 %6 %7 %8 %9&echo.
goto Help

:OptShow
shift
if not "%1"=="" goto OptUnexpected
echo.
call :FN_ShowSysEnv
echo.
call :FN_ShowCM3Env
goto END

:OptReset
shift
if not "%1"=="" goto OptUnexpected
if defined CM3_BIN call :FN_RemovePath %CM3_BIN%
if defined CM3_BIN pushd %CM3_BIN% && echo Changed current directory to %CM3_BIN%. && echo Use 'popd' to return to prior directory.  && echo.
set CM3_ROOT=
set CM3_BIN=
set CM3_PKG=
set CM3_IDE=
set CM3_IDE_EXE=
set CM3_IDE_HOME=
set REACTOR_HOME=
if /I "%_cm3_CommandReady%"=="TRUE" title Windows Command Prompt Shell
if defined _cm3_CommandReady set _cm3_CommandReady=FALSE
echo RESET complete.  Command prompt shell is no longer ready for CM3.
echo.
goto END

:OptRoot
if /I "%_cm3_SeenRootOpt%"=="TRUE" echo ERROR:  Only one 'Root' option allowed. & goto Usage
set _cm3_SeenRootOpt=TRUE
shift
if "%1"=="" echo ERROR:  ROOT option requires path argument. & goto Usage
if not exist "%1\bin\cm3.exe" echo ERROR:  Invalid ROOT path "%1" & echo         "%1\bin\cm3.exe" not found. & goto Usage
set _cm3_Root=%~f1
goto NextOpt

:OptHome
if /I "%_cm3_SeenHomeOpt%"=="TRUE" echo ERROR:  Only one 'Home' option allowed. & goto Usage
set _cm3_SeenHomeOpt=TRUE
shift
if "%1"=="" echo ERROR:  HOME option requires path argument. & goto Usage
if not exist "%1" echo ERROR:  Invalid HOME path "%1" & goto Usage
set _cm3_HOME=%~f1
goto NextOpt

:OptIDE
if /I "%_cm3_SeenIDEOpt%"=="TRUE" echo ERROR:  Only one 'IDE' option allowed. & goto Usage
set _cm3_SeenIDEOpt=TRUE
set _cm3_IDE=
shift
for %%a in (cm3IDE Reactor) do if /I "%%a"=="%1" set _cm3_IDE=%%a
if "%_cm3_IDE%"=="" echo ERROR:  IDE option requires 'cm3IDE' or 'Reactor' argument. & goto Usage
goto NextOpt

:OptStartIDE
if /I "%_cm3_startIDE%"=="TRUE" echo ERROR:  Only one 'StartIDE' option allowed. & goto Usage
set _cm3_startIDE=TRUE
goto NextOpt

:OptSameWindow
if /I "%_cm3_SeenWinOpt%"=="TRUE" echo ERROR:  Only one 'SameWindow' or 'NewWindow' option allowed. & goto Usage
set _cm3_LaunchNewWindow=FALSE
set _cm3_SeenWinOpt=TRUE
goto NextOpt

:OptNewWindow
if /I "%_cm3_SeenWinOpt%"=="TRUE" echo ERROR:  Only one 'SameWindow' or 'NewWindow' option allowed. & goto Usage
set _cm3_LaunchNewWindow=TRUE
set _cm3_SeenWinOpt=TRUE
goto NextOpt

:OptUnexpected
echo ERROR:  Unexpected option "%1"
goto Usage

:OptEnd
REM no more options allowed
if not "%1"=="" goto OptUnexpected
if /I not "%_cm3_LaunchNewWindow%"=="TRUE" goto SetCM3Env
echo ...LAUNCHING:  start "Windows Command Prompt Shell" /MAX %_cm3_Relaunch% MaxWin
start "Windows Command Prompt Shell" /MAX %_cm3_Relaunch% MaxWin
call :FN_KillTempVars
set _cm3_ExitCode=
@echo on
@goto :EOF



:SetCM3Env
:---------
REM CM3_ROOT is the root of the CM3 installation, typically "C:\cm3"
REM CM3_PKG is the root of the public package repository, "C:\cm3\pkg"
REM CM3_BIN is where the CM3 binaries are kept, typically "C:\cm3\bin"
REM CM3_IDE_EXE is path to the desired IDE, typically "C:\cm3\bin\CM3IDE.EXE"
REM CM3_IDE_HOME is the folder where CM3IDE.EXE looks for its private repository
REM REACTOR_HOME is the folder where REACTOR.EXE looks for its private repository
REM

rem In the event we are changing from one root to another, the first order of
rem business is to remove existing cm3 paths.
if defined CM3_BIN call :FN_RemovePath %CM3_BIN%

set CM3_ROOT=%_cm3_Root%
if "%CM3_ROOT%"=="" set CM3_ROOT=%SystemDrive%\cm3
call :FN_FQPathNoSpaces CM3_ROOT "%CM3_ROOT%"
set CM3_PKG=%CM3_ROOT%\pkg
set CM3_BIN=%CM3_ROOT%\bin
set CM3_IDE=%_cm3_IDE%
set CM3_IDE_EXE=%CM3_BIN%\%CM3_IDE%.EXE

rem Next line forces default assignment of IDE HOME folder if user has changed the IDE without specifying a new home folder.
if defined _cm3_HOME if defined _cm3_PriorIDE if not "%_cm3_PriorIDE%"=="%CM3_IDE%" if "%_cm3_PriorHome%"=="%_cm3_HOME%" set _cm3_HOME=

set _cm3_DocsHome=%UserProfile%\Documents
if /I not "%_cm3_ThisOS%"=="Vista" if /I not "%_cm3_ThisOS%"=="Win7" if /I not "%_cm3_ThisOS%"=="2008" set _cm3_DocsHome=%UserProfile%\My Documents
if "%_cm3_HOME%"=="" if /I "%CM3_IDE%"=="cm3IDE" set _cm3_HOME=%_cm3_DocsHome%\CM3_IDE_HOME
if "%_cm3_HOME%"=="" if /I "%CM3_IDE%"=="Reactor" set _cm3_HOME=%_cm3_DocsHome%\REACTOR_HOME
if not exist "%_cm3_HOME%" echo ...Creating folder "%_cm3_HOME%" & mkdir "%_cm3_HOME%"
if not exist "%_cm3_HOME%" echo ERROR:  Unable to create folder "%_cm3_HOME%" & goto FatalError
call :FN_FQPathNoSpaces _cm3_HOME "%_cm3_HOME%"
if /I "%CM3_IDE%"=="cm3IDE" set CM3_IDE_HOME=%_cm3_HOME%
if /I "%CM3_IDE%"=="Reactor" set REACTOR_HOME=%_cm3_HOME%

if not exist "%CM3_ROOT%" goto FATAL_Env
if not exist "%CM3_PKG%" goto FATAL_Env
if not exist "%CM3_BIN%\cm3.exe" goto FATAL_Env
if not exist "%CM3_IDE_EXE%" goto FATAL_Env
REM fall thru to MSVisualC



:MSVisualC
:---------
REM cm3 needs the Microsoft Visual Studio/C/C++ linker and tools.  Microsoft 
REM includes a VCVars32.BAT file to setup the environment for VisualC/C++.
REM Typically, the environment is set up when the user installs VisualC/C++, but
REM if not, the VCVars32.BAT can be used to set it up at any time.
REM The following code attempts to determine if VC is already set up and,
REM if so, does not try to set it up twice.
REM
REM Edit the following lines if necessary to conform to the installed version 
REM of your Microsoft Visual Studio/C/C++.
REM
if defined VCINSTALLDIR goto Skip_MSVisualC
if defined _cm3_VCSetup call %_cm3_VCSetup%
REM if not defined MsDevDir call %_cm3_VCSetup%"
if defined _cm3_SDKSetup call %_cm3_SDKSetup%
:Skip_MSVisualC
REM fall thru to SetPath



:SetPath
:-------
REM Augment the PATH to find the cm3 binaries/dlls.
REM
path %CM3_BIN%;%path%
rem fall thru to Done



:Done
:----
REM Everthing is good; we are done.  Show the user what has been achived.
REM
title CM3 Command Prompt Shell
for /f "tokens=1-5" %%a in ('%CM3_BIN%\cm3.exe -version') do if /I (%%d)==(version) title %%a %%b %%c %%d %%e Command Prompt Shell
set _cm3_CommandReady=TRUE
echo.
call :FN_ShowSysEnv
echo.
call :FN_ShowCM3Env
if /I "%_cm3_startIDE%"=="TRUE" call :FN_StartIDE & echo === Command Prompt Ready for CM3 ===
goto END



:FN_StartIDE
:-----------
REM Ok, we are aready to launch the IDE...
REM
echo ...starting %CM3_IDE% (%CM3_IDE_EXE%)...
echo.
echo (Note:  Do not close the %CM3_IDE%'s "Console Output" window.
echo         If you want to stop the IDE, just close the browser window.)
echo.
pushd \
cd /D %CM3_BIN%
REM
REM NOTE we will use START to launch the IDE in a new window.  The environ 
REM we so carefully setup will be passed on to the new window.
REM
start "%CM3_IDE% Console Output" /MIN %CM3_IDE_EXE%
popd
goto :EOF



:BadEnviron
:----------
REM Bad system environment settings.
REM
echo.
echo ERROR:  System Environment does not conform to standard.
call :FN_ShowSysEnv
if defined _cm3_CommandReady set _cm3_CommandReady=FALSE
set _cm3_ExitCode=3
goto END



:FATAL_Env
:---------
REM A fatal problem has been detected in the environment settings.
REM
echo.
echo FATAL ERROR:  Unable to find proper CM3 installation.
echo     CM3_ROOT expected in folder %CM3_ROOT%
echo      CM3_PKG expected in folder %CM3_PKG%
echo      CM3_BIN expected in folder %CM3_BIN%
echo      CM3.EXE expected in file   %CM3_BIN%\cm3.exe
echo   %CM3_IDE%.EXE expected in file   %CM3_IDE_EXE%
if defined _cm3_CommandReady set _cm3_CommandReady=FALSE
set CM3_ROOT=
set CM3_BIN=
set CM3_PKG=
set CM3_IDE_EXE=
set CM3_IDE_HOME=
set REACTOR_HOME=
set _cm3_ExitCode=4
goto END



:FatalError
:----------
REM A fatal problem has been reported already, so abort operation.
REM
echo ...Aborting...
if defined _cm3_CommandReady set _cm3_CommandReady=FALSE
set _cm3_ExitCode=5
goto END



:Help
:----
echo HELP:
echo.
echo Open a new Windows Command Shell (cmd.exe) and prepare its environment for use
echo with Critical Mass Modula-3 (CM3).  This script also can be used to display or
echo modify the CM3 environment settings when invoked from an extant CM3 command
echo shell window.  In addition, this script can start the Integrated Development
echo Environment (IDE).
call :FN_ShowUsage
echo Unless overriden by command line options, the defaults for this system are:
echo        CM3_ROOT=%%CM3_ROOT%%, if defined; otherwise, %%SystemDrive%%\cm3
echo         CM3_BIN=%%CM3_ROOT%%\bin
echo         CM3_PKG=%%CM3_ROOT%%\pkg
echo         CM3_IDE=cm3IDE
echo    CM3_IDE_EXE =%%CM3_ROOT%%\bin\cm3ide.exe
echo    CM3_IDE_HOME=%%UserProfile%%\My Documents\CM3_IDE_HOME
if defined _cm3_VCSetup echo Microsoft Visual C Setup=%_cm3_VCSetup%
if defined _cm3_SDKSetup echo Microsoft Software Development Kit Setup=%_cm3_SDKSetup%
echo.
goto END



:FN_ShowUsage
:------------
echo.
echo USAGE:
echo.
echo    cm3CommandShell.CMD [Help ^| ? ^| Show ^| Reset]
echo    cm3CommandShell.CMD [Root path] [Home path] [IDE cm3IDE ^| Reactor]
echo                        [StartIDE] [SameWindow ^| NewWindow]
echo.
echo    Help = Display help.
echo       ? = Display help.
echo.
echo    Show = Display current system and CM3 environment settings, if any.
echo.
echo   Reset = Remove %%CM3_BIN%% from path, then cause environent variables CM3_ROOT,
echo           CM3_BIN, CM3_PKG, CM3_IDE_EXE, CM3_IDE_HOME, and REACTOR_HOME to 
echo           become undefined, i.e., remove these environment settings.
echo.
echo      Root path = Define the CM3 installation to be rooted at path.
echo.
echo      Home path = Define the path to the IDE's private repository.
echo.
echo     IDE cm3IDE = Use cm3IDE.EXE as the IDE.
echo    IDE Reactor = Use Reactor.EXE as the IDE.
echo.
echo       StartIDE = Also start up the Integrated Development Environment (IDE)
echo.
echo     SameWindow = Don't open a new window; just use the current one.
echo.
echo      NewWindow = Force a new window to be opened and used.
echo.
goto :EOF



:Usage
:-----
set _cm3_ExitCode=2
call :FN_ShowUsage
goto END



:END
:---
REM Clean up before exiting script.
REM
rem if not "%_cm3_ExitCode%"=="0" set _cm3_
call :FN_KillTempVars
@echo on
@EXIT /B %_cm3_ExitCode%
