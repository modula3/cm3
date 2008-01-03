@echo off
set CM3Setup_SavePrompt=
:ShowRemarks
REM ===========================================================================
REM cm3SetupCmdEnv.CMD, written by R.C.Coleburn, 08/12/2003
REM v1.10--08/13/2003 by RCC
REM v1.20--08/15/2003 by RCC
REM v1.30--08/29/2003 by RCC, added optional HOME argument, and
REM                           revamped argument handling
REM ===========================================================================
REM PURPOSE:
REM    This Windows batch/command file sets up the environment for using cm3.
REM    External documentation is available as Documentation_cm3SetupCmdEnv.htm
REM ---------------------------------------------------------------------------
REM AUTHOR/DISTRIBUTION RIGHTS:
REM    Randy Coleburn, Senior Systems Engineer, Scientific Research Corporation
REM    2300 Windy Ridge Parkway, Suite 400 South, Atlanta, GA 30339
REM    Author grants free use/modification of this batch/command file to anyone
REM    Author does not warrant this batch/command file.  Use at your own risk.
REM ---------------------------------------------------------------------------
REM CAVEATS:
REM    You must be running WindowsNT4SP6, Windows2000, or Windows XP with
REM    command extensions enabled, otherwise this command file will fail.
REM ---------------------------------------------------------------------------
REM SETUP:
REM    Store this file in the bin directory of your cm3 installation.
REM    Place a shortcut to this file on your desktop, then modify its 
REM    properties to put "cmd /k" at the front of the command line.  Now, when
REM    you invoke this shortcut, you will get a windows command prompt window 
REM    whose environment is set up for using cm3.  You can also call this 
REM    command file from a command prompt window at any time to set up the 
REM    environment.
REM ---------------------------------------------------------------------------
REM ENVIRONMENT VARIABLE USAGE:
REM    CM3_ROOT is the root of the CM3 installation, typically "C:\cm3".
REM    CM3_BIN is where the CM3 binaries are kept, typically "C:\cm3\bin".
REM    CM3_PKG is the location of the public package repository, 
REM       typically "C:\cm3\pkg"
REM    CM3_DoneSetup indicates if this command file completed successfully.
REM    REACTOR_HOME is where Reactor looks for the user's home folder.
REM    CM3Setup_RP, CM3Setup_HP, and CM3Setup_SavePrompt are temporary 
REM    environment variables used internally.
REM THE FOLLOWING ENV VARS DEFINE DEFAULT LOCATIONS; EDIT THE DEFAULT_LOCATIONS
REM SECTION BELOW TO CHANGE THEM:
REM    CM3Setup_DefaultInstallRoot
REM    CM3Setup_DefaultTemp
REM    CM3Setup_DefaultVCSetup
REM ---------------------------------------------------------------------------
REM OPTIONAL COMMAND LINE ARGUMENTS TO THIS BATCH/COMMAND FILE:
REM    REMARKS
REM       Display useful remarks located at the beginning of this command file.
REM       REMARKS cannot be combined with other arguments.
REM    HELP
REM       Display a summary of helpful information.
REM       HELP cannot be combined with other arguments.
REM    UNDO
REM       Indicates you want to undo the effects of the most recent invocation
REM       of this batch/command file.  Note that mangling of the TEMP and TMP
REM       variables can not be undone.  Consequtive invocations using UNDO is
REM       pointless, but won't hurt anything.
REM       UNDO cannot be combined with other arguments.
REM    ROOT path
REM       The "ROOT" argument indicates you want to specify the cm3 
REM       installation root.  The "path" argument is optional, but if given,
REM       should be the path to the cm3 installation root.  If the "path"
REM       argument is omitted, a default will be used (typically, C:\cm3).  
REM       The "ROOT path" option is useful if you have multiple cm3 
REM       installation roots (e.g., different versions) and you want to switch
REM       between them, or if your installation is not in the default location.
REM    HOME path
REM       The "HOME" argument indicates you want to specify the location of 
REM       the Reactor home folder for this user.  The "path" argument is 
REM       optional, but if given, should be the path to the home folder.  If
REM       the "path" argument is omitted, a default will be used, typically
REM       "%USERPROFILE%\ReactorHome".
REM All keyword arguments are case-insensitive.
REM ---------------------------------------------------------------------------
REM USAGE EXAMPLES:
REM    cm3SetupCmdEnv                         cm3SetupCmdEnv UNDO
REM    cm3SetupCmdEnv HELP                    cm3SetupCmdEnv REMARKS
REM    cm3SetupCmdEnv ROOT path               cm3SetupCmdEnv ROOT
REM    cm3SetupCmdEnv ROOT path HOME path     cm3SetupCmdEnv ROOT HOME path
REM    cm3SetupCmdEnv HOME path               cm3SetupCmdEnv HOME
REM    cm3SetupCmdEnv HOME path ROOT path     cm3SetupCmdEnv HOME ROOT path
REM    cm3SetupCmdEnv ROOT path HOME          cm3SetupCmdEnv ROOT HOME
REM    cm3SetupCmdEnv HOME path ROOT          cm3SetupCmdEnv HOME ROOT
REM ---------------------------------------------------------------------------
REM NOTES:
REM    When completed successfully, the variable CM3_DoneSetup=TRUE is set.  
REM    You can check this variable in other batch/command files to avoid 
REM    calling cm3SetupCmdEnv.CMD again unnecessarily.
REM ===========================================================================
@echo off
if defined CM3Setup_SavePrompt goto End

:DEFAULT_LOCATIONS
:-----------------
REM ===========================================================================
REM Change the defaults below, if desired
REM ===========================================================================
set CM3Setup_DefaultInstallRoot=C:\cm3
set CM3Setup_DefaultVCSetup="C:\Program Files\Microsoft Visual Studio\VC98\Bin\VCVars32.bat"
set CM3Setup_DefaultTemp=C:\Temp
if defined USERPROFILE set CM3Setup_DefaultTemp=%USERPROFILE%\Local Settings\Temp
REM ===========================================================================
rem fall thru to Begin

:Begin
:-----
echo -------------------------------------------------------------------------------
echo cm3SetupCmdEnv.CMD, written by R.C.Coleburn 08/12/2003, v1.30 08/29/2003 by RCC
echo -------------------------------------------------------------------------------
rem fall thru to Args1

:Args1
:-----
if /I (%1)==(HELP) goto Help
if /I (%1)==(REMARKS) goto Remarks
if /I (%1)==(UNDO) call :CleanUpPriorCM3
if /I (%1)==(UNDO) goto KillEnvVars
if not (%5)==() goto FATAL_InvalidArguments
set CM3Setup_RP=(NO_RESET)
set CM3Setup_HP=(NO_RESET)
rem fall thru to Args2

:Args2
:-----
rem The ROOT and HOME arguments are optional, may themselves have an optional
rem path argument, and may appear in any order.
if (%1)==() goto Args3
for %%A in (Help Remarks Undo) do if /I (%1)==(%%A) goto FATAL_InvalidArguments
if /I (%1)==(ROOT) goto ArgRoot
if /I (%1)==(HOME) goto ArgHome
goto FATAL_InvalidArguments

:ArgRoot
:-------
shift
for %%A in (Help Remarks Undo) do if /I (%1)==(%%A) goto FATAL_InvalidArguments
set CM3Setup_RP=%1
if (%1)==() set CM3Setup_RP=
if /I (%1)==(HOME) set CM3Setup_RP=
if /I not (%1)==(HOME) shift
goto Args2

:ArgHome
:-------
shift
for %%A in (Help Remarks Undo) do if /I (%1)==(%%A) goto FATAL_InvalidArguments
set CM3Setup_HP=%1
if (%1)==() set CM3Setup_HP=
if /I (%1)==(ROOT) set CM3Setup_HP=
if /I not (%1)==(ROOT) shift
goto Args2

:Args3
:-----
if "%CM3Setup_RP%"==" " set CM3Setup_RP=
if "%CM3Setup_HP%"==" " set CM3Setup_HP=
rem fall thru to Temp

:Temp
:----
REM The TEMP env var is usually set to a path with embedded spaces like, 
REM "C:\Documents and Settings\rcolebur\Local Settings\Temp".  The embedded 
REM spaces cause problems for cm3 in storing intermediate/temporary files, so 
REM the following code transforms the path into the mangled short path name form.
REM If no TEMP is defined, the default is used.
if not defined TEMP set TEMP=%CM3Setup_DefaultTemp%
if not exist "%TEMP%" echo Creating TEMP folder: %TEMP%
if not exist "%TEMP%" mkdir "%TEMP%"
call :FN_MakeShort TEMP "%TEMP%"
REM Not sure that cm3 uses TMP, but let's do it just in case.
if defined TMP call :FN_MakeShort TMP "%TMP%"
rem fall thru to SetRoot

:SetRoot
:-------
REM If the ROOT command line argument is specified, force setting of a new
REM root.  Note that the ROOT path argument can be empty, in which case
REM the default will be selected in BasicEnv below.
if (%CM3Setup_RP%)==((NO_RESET)) goto BasicEnv
call :CleanUpPriorCM3
set CM3_ROOT=%CM3Setup_RP%
rem fall thru to BasicEnv

:BasicEnv
:--------
REM CM3_ROOT is the root of the CM3 installation, typically "C:\cm3"
REM CM3_BIN is where the CM3 binaries are kept, typically "C:\cm3\bin"
REM CM3_PKG is the root of the public package repository, "C:\cm3\pkg"
if not defined CM3_ROOT set CM3_ROOT=%CM3Setup_DefaultInstallRoot%
set CM3_BIN=%CM3_ROOT%\bin
set CM3_PKG=%CM3_ROOT%\pkg
if not exist "%CM3_ROOT%" goto FATAL_Env
if not exist "%CM3_BIN%\cm3.exe" goto FATAL_Env
if not exist "%CM3_PKG%" goto FATAL_Env
REM Make sure these paths don't contain spaces, if so, use the mangled short equivalents.
call :FN_MakeShort CM3_ROOT "%CM3_ROOT%"
call :FN_MakeShort CM3_BIN "%CM3_BIN%"
call :FN_MakeShort CM3_PKG "%CM3_PKG%"
rem optional: cd /d %CM3_BIN%
rem fall thru to MSVisualC

:MSVisualC
:---------
REM cm3 needs the Microsoft Visual Studio/C/C++ linker and tools.  Microsoft 
REM includes a VCVars32.BAT file to setup the environment for VisualC/C++.
REM Typically, the environment is set up when the user installs VisualC/C++, but
REM if not, the VCVars32.BAT can be used to set it up at any time.
REM Edit the following line if necessary to conform to the version and 
REM    installation location of your Microsoft Visual Studio/C/C++.
if not defined MsDevDir call %CM3Setup_DefaultVCSetup%"
rem fall thru to SetPath

:SetPath
:-------
REM Augment the PATH to find the cm3 binaries/dlls.
REM First make sure to remove any existing cm3 paths
call :FN_RemovePath %CM3_BIN%
REM Now, make CM3_BIN the first path
path %CM3_BIN%;%path%
rem fall thru to Reactor

:Reactor
:-------
REM Reactor, the browser-based IDE for cm3, uses a special folder as the user's 
REM home project directory.  This folder is specified using the REACTOR_HOME 
REM variable.  If REACTOR_HOME is already defined, the following code will not
REM change it, unless the user specified a change via the optional HOME 
REM argument.  Otherwise, %USERPROFILE%\ReactorHome is used, unless 
REM USER_PROFILE is not defined, in which case %CM3_ROOT%\ReactorHome is used
REM as a last resort.
if not (%CM3Setup_HP%)==((NO_RESET)) set REACTOR_HOME=%CM3Setup_HP%
if defined REACTOR_HOME if not exist "%REACTOR_HOME%" echo Creating folder:  %REACTOR_HOME%
if defined REACTOR_HOME if not exist "%REACTOR_HOME%" mkdir "%REACTOR_HOME%"
if defined REACTOR_HOME if exist "%REACTOR_HOME%" goto ReactorShort
if defined USERPROFILE set REACTOR_HOME=%USERPROFILE%\ReactorHome
if not defined REACTOR_HOME set REACTOR_HOME=%CM3_ROOT%\ReactorHome
if not exist "%REACTOR_HOME%" echo Creating folder:  %REACTOR_HOME%
if not exist "%REACTOR_HOME%" mkdir "%REACTOR_HOME%"
rem fall thru to ReactorShort

:ReactorShort
call :FN_MakeShort REACTOR_HOME "%REACTOR_HOME%"
goto Done

:CleanUpPriorCM3
:---------------
REM In the event we are changing from one root to another, the first order of
REM business is to remove existing cm3 paths.
if defined CM3_BIN call :FN_RemovePath %CM3_BIN%
goto :EOF

:KillEnvVars
:-----------
title Command Prompt
set CM3_ROOT=
set CM3_BIN=
set CM3_PKG=
set CM3_DoneSetup=
set REACTOR_HOME=
echo.
echo === Results of UNDO Option ===
echo.
echo CM3_ROOT=%CM3_ROOT%
echo.
echo CM3_BIN=%CM3_BIN%
echo.
echo CM3_PKG=%CM3_PKG%
echo.
echo CM3_DoneSetup=%CM3_DoneSetup%
echo.
echo REACTOR_HOME=%REACTOR_HOME%
echo.
echo TEMP=%TEMP%
if defined TMP echo.
if defined TMP echo TMP=%TMP%
echo.
echo PATH=%PATH%
echo.
goto End

:Remarks
:-------
set CM3Setup_SavePrompt=%prompt%
prompt $H
echo HELP TEXT (REMARKS) FOLLOWS:
@echo on
@goto ShowRemarks

:FATAL_Env
:---------
set CM3_DoneSetup=
echo FATAL ERROR:  Unable to find CM3 installation.
echo CM3_ROOT expected in folder %CM3_ROOT%
echo CM3_BIN  expected in folder %CM3_BIN%
echo CM3_PKG  expected in folder %CM3_PKG%
echo CM3.EXE  expected in file   %CM3_BIN%\cm3.exe
goto End

:FATAL_InvalidArguments
:-----------------------
echo FATAL ERROR (Invalid arguments):  %*
rem fall thru to Help

:Help
:----
echo ---------------------------------------------------------------------------
echo USAGE:  cm3SetupCmdEnv [Help Remarks Undo [Root [path]] [Home [path]]]
echo ---------------------------------------------------------------------------
echo OPTIONAL COMMAND LINE PARAMETERS:
echo    REMARKS
echo       Display useful remarks located at the beginning of this command file.
echo       REMARKS cannot be combined with other arguments.
echo    HELP
echo       Display this summary of helpful information.
echo       HELP cannot be combined with other arguments.
echo    UNDO
echo       Indicates you want to undo the effects of the most recent invocation
echo       of this batch/command file.  Note that mangling of the TEMP and TMP
echo       variables can not be undone.  Consequtive invocations using UNDO is
echo       pointless, but won't hurt anything.
echo       UNDO cannot be combined with other arguments.
echo    ROOT path
echo       The "ROOT" argument indicates you want to specify the cm3 
echo       installation root.  The "path" argument is optional, but if given,
echo       should be the path to the cm3 installation root.  If the "path"
echo       argument is omitted, a default will be used (typicall, C:\cm3).  
echo       The "ROOT path" option is useful if you have multiple cm3 
echo       installation roots (e.g., different versions) and you want to switch
echo       between them, or if your installation is not in the default location.
echo    HOME path
echo       The "HOME" argument indicates you want to specify the location of 
echo       the Reactor home folder for this user.  The "path" argument is 
echo       optional, but if given, should be the path to the home folder.  If
echo       the "path" argument is omitted, a default will be used, typically
echo       "%%USERPROFILE%%\ReactorHome".
echo All keyword arguments are case-insensitive.
echo ---------------------------------------------------------------------------
echo EXAMPLES:
echo    cm3SetupCmdEnv                         cm3SetupCmdEnv UNDO
echo    cm3SetupCmdEnv HELP                    cm3SetupCmdEnv REMARKS
echo    cm3SetupCmdEnv ROOT path               cm3SetupCmdEnv ROOT
echo    cm3SetupCmdEnv ROOT path HOME path     cm3SetupCmdEnv ROOT HOME path
echo    cm3SetupCmdEnv HOME path               cm3SetupCmdEnv HOME
echo    cm3SetupCmdEnv HOME path ROOT path     cm3SetupCmdEnv HOME ROOT path
echo    cm3SetupCmdEnv ROOT path HOME          cm3SetupCmdEnv ROOT HOME
echo    cm3SetupCmdEnv HOME path ROOT          cm3SetupCmdEnv HOME ROOT
goto End

:FN_MakeShort
:------------
rem Function (MakeShort)
rem params:  1=env var name, 2=long path surrounded by quotes
rem result:  env var (1) is set to the mangled short equiv of long path (2)
set %1=%~s2
goto :EOF

:FN_StringReplace
:----------------
rem Function (StringReplace)
rem params:  1=env var name, 2=str1, 3=str2 
rem          (make sure to enclose str1 and str2 in double quotes) 
rem result:  replace all occurances of str1 (2) in env var (1) with str2 (3)
call :FN_StrRep2 "set %1=%%%1:%~2=%~3%%"
goto :EOF
:FN_StrRep2
%~1
goto :EOF

:FN_RemovePath
:-------------
rem Function (RemovePath)
rem params:  1=searchPath
rem result:  remove all occurances of searchPath from PATH env var
call :FN_RemPath2 "set PATH=%%PATH:%~1;=%%"
call :FN_RemPath2 "set PATH=%%PATH:;%~1=%%"
goto :EOF
:FN_RemPath2
%~1
goto :EOF

:Done
:----
title cm3 Command Prompt
set CM3_DoneSetup=TRUE
echo.
echo === Command Prompt Ready for CM3 ===
echo.
echo CM3_ROOT=%CM3_ROOT%
echo.
echo CM3_BIN=%CM3_BIN%
echo.
echo CM3_PKG=%CM3_PKG%
echo.
echo CM3_DoneSetup=%CM3_DoneSetup%
echo.
echo REACTOR_HOME=%REACTOR_HOME%
echo.
echo TEMP=%TEMP%
if defined TMP echo.
if defined TMP echo TMP=%TMP%
echo.
echo PATH=%PATH%
echo.
cm3 -version
rem fall thru to End

:End
:---
set CM3Setup_RP=
set CM3Setup_HP=
set CM3Setup_DefaultInstallRoot=
set CM3Setup_DefaultTemp=
set CM3Setup_DefaultVCSetup=
if defined CM3Setup_SavePrompt prompt %CM3Setup_SavePrompt%
set CM3Setup_SavePrompt=
echo -------------------------------------------------------------------------------
@echo on
