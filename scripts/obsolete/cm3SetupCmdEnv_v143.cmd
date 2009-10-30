@echo off
set CM3Setup_SavePrompt=
set CM3Setup_ExitCode=0
:ShowRemarks
REM ===========================================================================
REM cm3SetupCmdEnv.CMD, written by R.C.Coleburn, 08/12/2003
REM v1.10--08/13/2003 by RCC
REM v1.20--08/15/2003 by RCC
REM v1.30--08/29/2003 by RCC, added optional HOME argument, and
REM                           revamped argument handling
REM v1.31--12/06/2006 by RCC, added CM3Setup_DefaultSDKSetup
REM v1.40--01/20/2008 by RCC, added IDE option, added SHOW option, misc enhance
REM v1.41--01/24/2008 by RCC, set Visual Studio defaults to match 2008 Express
REM v1.42--04/10/2008 by RCC, change default search names for CM3-IDE (Olaf likes lower case, e.g. cm3ide)
REM v1.43--06/12/2008 by RCC, put cm3 version number in window title
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
REM    CM3_IDE_EXE is name of the IDE executable, e.g., Reactor, CM3IDE, Catalyst
REM    CM3_DoneSetup indicates if this command file completed successfully.
REM    CM3_IDE_HOME is where the CM3_IDE looks for the user's proj folder
REM    REACTOR_HOME is where Reactor looks for the user's home folder.
REM    CM3Setup_RP, CM3Setup_HP, CM3Setup_IDE, CM3Setup_SavePrompt, and 
REM    CM3Setup_ExitCode are temporary environment variables used internally.
REM THE FOLLOWING ENV VARS DEFINE DEFAULT LOCATIONS; EDIT THE DEFAULT_LOCATIONS
REM SECTION BELOW TO CHANGE THEM:
REM    CM3Setup_DefaultInstallRoot
REM    CM3Setup_DefaultTemp
REM    CM3Setup_DefaultVCSetup
REM    CM3Setup_DefaultSDKSetup
REM ---------------------------------------------------------------------------
REM OPTIONAL COMMAND LINE ARGUMENTS TO THIS BATCH/COMMAND FILE:
REM    REMARKS
REM       Display useful remarks located at the beginning of this command file.
REM       REMARKS cannot be combined with other arguments.
REM    HELP
REM       Display a summary of helpful information.
REM       HELP cannot be combined with other arguments.
REM    SHOW
REM       Display a summary of the current CM3 environment variable values.
REM       In this case, no variables are changed, they are just displayed.
REM       SHOW cannot be combined with other arguments.
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
REM    IDE exeName
REM       The "IDE" argument indicates you want to specify which IDE should be
REM       set as the primary one to launch via subsequent calls to cm3StartIDE.
REM       The "exeName" argument is required and should specify the name of the
REM       executable program minus the .exe extension, e.g., "reactor", 
REM       "cm3ide", "catalyst".  This program must exist in the bin folder of
REM       the cm3 installation root.  If no IDE argument is given, the default
REM       action is to search for one in the ROOT\bin folder.
REM    HOME path
REM       The "HOME" argument indicates you want to specify the location of 
REM       the IDE's (e.g., Reactor, CM3IDE) home folder for this user.  
REM       The "path" argument is optional, but if given, should be the path to 
REM       the home folder.  If the "path" argument is omitted, a default will 
REM       be used, typically "%USERPROFILE%\My Documents\ReactorHome" or 
REM       "%USERPROFILE%\My Documents\CM3_IDE_Home".
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
REM    cm3SetupCmdEnv ROOT path IDE name      cm3SetupCmdEnv IDE name
REM    cm3SetupCmdEnv HOME path IDE name      cm3SetupCmdEnv HOME IDE name
REM    cm3SetupCmdEnv ROOT path HOME path IDE name
REM    cm3SetupCmdEnv SHOW
REM ---------------------------------------------------------------------------
REM EXIT CODES:
REM    0 = Ok
REM    1 = Invalid Arguments
REM    2 = Invalid Environment
REM    3..255 = Unknown error occured
REM ---------------------------------------------------------------------------
REM NOTES:
REM    When completed successfully, the variable CM3_DoneSetup=TRUE is set.  
REM    You can check this variable in other batch/command files to avoid 
REM    calling cm3SetupCmdEnv.CMD again unnecessarily.
REM ===========================================================================
@echo off
if defined CM3Setup_SavePrompt goto End

REM
REM !=!=!=!=!=!=!=!=!=!=!=!=!=!=!=!=!=!=!=!=!=!=!=!=!=!=!=!=!=!=!=!=!=!=!=!=!=!
REM !=!=!=!=!=!=!=!=!=!=!=!=!=!=!=!=!=!=!=!=!=!=!=!=!=!=!=!=!=!=!=!=!=!=!=!=!=!

:-----------------
:DEFAULT_LOCATIONS
:-----------------
REM
REM ===========================================================================
REM Change the defaults below, if desired
REM ===========================================================================
REM
REM ---------------------------------------------------------------------------
REM DEFAULT (CM3Setup_DefaultInstallRoot)
REM    Set to absolute path of CM3 Installation Root, e.g. C:\cm3
REM
set CM3Setup_DefaultInstallRoot=C:\cm3
REM ---------------------------------------------------------------------------

REM ---------------------------------------------------------------------------
REM DEFAULT (CM3Setup_DefaultVCSetup)
REM    Set to absolute path of BAT/CMD that sets up Microsoft Visual Studio/C/C++
REM    Set to empty to disable calling this auto setup routine.
REM    Example: set CM3Setup_DefaultVCSetup="C:\Program Files\Microsoft Visual Studio 8\VC\bin\VCVars32.bat"
REM 
set CM3Setup_DefaultVCSetup="C:\Program Files\Microsoft Visual Studio 9.0\VC\vcvarsall.bat" x86
REM ---------------------------------------------------------------------------
REM ...some other variants shown here for examples...
REM set CM3Setup_DefaultVCSetup="C:\Program Files\Microsoft Visual Studio 9.0\VC\vcvarsall.bat"
REM set CM3Setup_DefaultVCSetup="C:\Program Files\Microsoft Visual Studio 8\VC\bin\VCVars32.bat"
REM set CM3Setup_DefaultVCSetup="C:\Program Files\Microsoft Visual C++ Toolkit 2003\VCVars32.bat"
REM set CM3Setup_DefaultVCSetup="C:\Program Files\Microsoft Visual Studio\VC98\Bin\VCVars32.bat"

REM ---------------------------------------------------------------------------
REM DEFAULT (CM3Setup_DefaultSDKSetup)
REM    Set to absolute command line for establishing the Microsoft Platform SDK 
REM    environment settings.  Set it to empty to disable calling this routine.
REM    Example: set CM3Setup_DefaultSDKSetup="C:\Program Files\Microsoft Platform SDK for Windows Server 2003 R2\SetEnv.CMD" /XP32 /RETAIL
REM
set CM3Setup_DefaultSDKSetup=
REM ---------------------------------------------------------------------------

REM ---------------------------------------------------------------------------
REM DEFAULT (CM3Setup_DefaultTemp)
REM    Set to absolute path of default folder location for temporary files.
REM    This default is used only if the TEMP var not set already.
REM
set CM3Setup_DefaultTemp=C:\Temp
if defined USERPROFILE set CM3Setup_DefaultTemp=%USERPROFILE%\Local Settings\Temp
REM ---------------------------------------------------------------------------

REM ===========================================================================
REM
REM !=!=!=!=!=!=!=!=!=!=!=!=!=!=!=!=!=!=!=!=!=!=!=!=!=!=!=!=!=!=!=!=!=!=!=!=!=!
REM !=!=!=!=!=!=!=!=!=!=!=!=!=!=!=!=!=!=!=!=!=!=!=!=!=!=!=!=!=!=!=!=!=!=!=!=!=!
REM
REM fall thru to Begin

:Begin
:-----
echo.
echo -------------------------------------------------------------------------------
echo cm3SetupCmdEnv.CMD, written by R.C.Coleburn 08/12/2003, v1.43 06/12/2008 by RCC
echo -------------------------------------------------------------------------------
echo.
REM fall thru to Args1

:Args1
:-----
REM First stage of command line option processing.  
REM Deal with singleton command line arguments.
REM
if /I (%1)==(HELP) goto Help
if /I (%1)==(REMARKS) goto Remarks
if /I (%1)==(SHOW) goto ShowCurrent
if /I (%1)==(UNDO) call :CleanUpPriorCM3
if /I (%1)==(UNDO) goto KillEnvVars
if not (%7)==() goto FATAL_InvalidArguments
set CM3Setup_RP=(NO_RESET)
set CM3Setup_HP=(NO_RESET)
set CM3Setup_IDE=(NO_RESET)
REM fall thru to Args2

:Args2
:-----
REM Second stage of command line option processing.
REM Deal with remaining types of options and their optional/required arguments.
REM Note this stage is repeated for each option set.  As each set is processed, 
REM the command line is "shifted" to consume the option and go to the next.
REM
REM The ROOT, HOME, and IDE arguments are optional, may themselves have an 
REM optional or required argument, and may appear in any order.
REM
if (%1)==() goto ArgsEnd
for %%A in (Help Remarks Undo Show) do if /I (%1)==(%%A) goto FATAL_InvalidArguments
if /I (%1)==(ROOT) goto ArgRoot
if /I (%1)==(HOME) goto ArgHome
if /I (%1)==(IDE) goto ArgIDE
goto FATAL_InvalidArguments

:ArgRoot
:-------
REM Ok, ROOT was specified.  Let's see if there is an optional "path" argument.
REM
shift
for %%A in (Help Remarks Undo Show) do if /I (%1)==(%%A) goto FATAL_InvalidArguments
set CM3Setup_RP=%1
if (%1)==() set CM3Setup_RP=
if /I (%1)==(HOME) set CM3Setup_RP=
if /I (%1)==(IDE) set CM3Setup_RP=
if /I not (%1)==(HOME) if /I not (%1)==(IDE) shift
goto Args2

:ArgHome
:-------
REM Ok, HOME was specified.  Let's see if there is an optional "path" argument.
REM
shift
for %%A in (Help Remarks Undo Show) do if /I (%1)==(%%A) goto FATAL_InvalidArguments
set CM3Setup_HP=%1
if (%1)==() set CM3Setup_HP=
if /I (%1)==(ROOT) set CM3Setup_HP=
if /I (%1)==(IDE) set CM3Setup_HP=
if /I not (%1)==(ROOT) if /I not (%1)==(IDE) shift
goto Args2

:ArgIDE
:------
REM Ok, IDE was specified.  Let's get the required "exeName" argument.
REM
shift
if (%1)==() goto FATAL_InvalidArguments
for %%A in (Help Remarks Undo Show Root Home) do if /I (%1)==(%%A) goto FATAL_InvalidArguments
set CM3Setup_IDE=%1
shift
goto Args2

:ArgsEnd
:-------
if "%CM3Setup_RP%"==" " set CM3Setup_RP=
if "%CM3Setup_HP%"==" " set CM3Setup_HP=
if "%CM3Setup_IDE%"==" " set CM3Setup_IDE=
REM fall thru to Temp

:Temp
:----
REM The TEMP env var is usually set to a path with embedded spaces like, 
REM "C:\Documents and Settings\rcolebur\Local Settings\Temp".  The embedded 
REM spaces cause problems for cm3 in storing intermediate/temporary files, so 
REM the following code transforms the path into the mangled short path name form.
REM If no TEMP is defined, the default is used.
REM
if not defined TEMP set TEMP=%CM3Setup_DefaultTemp%
if not exist "%TEMP%" echo Creating TEMP folder: %TEMP%
if not exist "%TEMP%" mkdir "%TEMP%"
call :FN_MakeShort TEMP "%TEMP%"
REM
REM Not sure that cm3 uses TMP, but let's do it just in case.
REM
if defined TMP call :FN_MakeShort TMP "%TMP%"
if not defined TMP set TMP=%TEMP%
REM fall thru to SetRoot

:SetRoot
:-------
REM If the ROOT command line argument is specified, force setting of a new
REM root.  Note that the ROOT path argument can be empty, in which case
REM the default will be selected in BasicEnv below.
REM
if (%CM3Setup_RP%)==((NO_RESET)) goto BasicEnv
call :CleanUpPriorCM3
set CM3_ROOT=%CM3Setup_RP%
REM fall thru to BasicEnv

:BasicEnv
:--------
REM CM3_ROOT is the root of the CM3 installation, typically "C:\cm3"
REM CM3_BIN is where the CM3 binaries are kept, typically "C:\cm3\bin"
REM CM3_PKG is the root of the public package repository, "C:\cm3\pkg"
REM
if not defined CM3_ROOT set CM3_ROOT=%CM3Setup_DefaultInstallRoot%
set CM3_BIN=%CM3_ROOT%\bin
set CM3_PKG=%CM3_ROOT%\pkg
if not exist "%CM3_ROOT%" goto FATAL_Env
if not exist "%CM3_BIN%\cm3.exe" goto FATAL_Env
if not exist "%CM3_PKG%" goto FATAL_Env
REM
REM Make sure these paths don't contain spaces, if so, use the mangled short equivalents.
REM
call :FN_MakeShort CM3_ROOT "%CM3_ROOT%"
call :FN_MakeShort CM3_BIN "%CM3_BIN%"
call :FN_MakeShort CM3_PKG "%CM3_PKG%"
REM
REM optional: cd /d %CM3_BIN%
REM
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
if defined VCINSTALLDIR goto SetPath
if defined CM3Setup_DefaultVCSetup call %CM3Setup_DefaultVCSetup%
REM if not defined MsDevDir call %CM3Setup_DefaultVCSetup%"
if defined CM3Setup_DefaultSDKSetup call %CM3Setup_DefaultSDKSetup%
REM fall thru to SetPath

:SetPath
:-------
REM Augment the PATH to find the cm3 binaries/dlls.
REM First make sure to remove any existing cm3 paths
REM
call :FN_RemovePath %CM3_BIN%
REM
REM Now, make CM3_BIN the first path
REM
path %CM3_BIN%;%path%
REM fall thru to SetUpIDE

:SetUpIDE
:--------
REM First, we need to decide which IDE executable will be the default for
REM subsequent use by cm3StartIDE.CMD.  This choice is specified by the 
REM CM3_IDE_EXE environment variable.  If CM3_IDE_EXE is already defined, the
REM following code will not change it, unless of course the program can't be
REM found, or unless the user specified a change via the optional IDE argument.
REM
if not (%CM3Setup_IDE%)==((NO_RESET)) set CM3_IDE_EXE=%CM3Setup_IDE%
if not defined CM3_IDE_EXE set CM3_IDE_EXE=cm3ide
REM
REM the default search order is as shown below.  To change it, just reorder the lines
REM
if not exist "%CM3_BIN%\%CM3_IDE_EXE%.exe" set CM3_IDE_EXE=cm3ide
if not exist "%CM3_BIN%\%CM3_IDE_EXE%.exe" set CM3_IDE_EXE=CM3IDE
if not exist "%CM3_BIN%\%CM3_IDE_EXE%.exe" set CM3_IDE_EXE=CM3_IDE
if not exist "%CM3_BIN%\%CM3_IDE_EXE%.exe" set CM3_IDE_EXE=Catalyst
if not exist "%CM3_BIN%\%CM3_IDE_EXE%.exe" set CM3_IDE_EXE=Reactor
if not exist "%CM3_BIN%\%CM3_IDE_EXE%.exe" set CM3_IDE_EXE=
REM
REM Reactor (or CM3IDE or Catalyst), the browser-based IDE for cm3, uses a 
REM special folder as the user's home project directory.  This folder is 
REM specified using the REACTOR_HOME and/or CM3_IDE_HOME environment
REM variable.  If REACTOR_HOME or CM3_IDE_HOME is already defined, the 
REM following code will not change it, unless the user specified a change via 
REM the optional HOME argument.  Otherwise, %USERPROFILE%\My Documents\ReactorHome
REM and/or %USERPROFILE%\My Documents\CM3_IDE_HOME are used, unless USER_PROFILE
REM is not defined, in which case %CM3_ROOT%\ReactorHome and 
REM %CM3_ROOT%\CM3_IDE_Home are used as a last resort.
REM
REM First, we set up the home folder for Reactor because it uses a different
REM environment variable than CM3_IDE.  Reactor is a legacy CM3 v4.1 product
REM but we want to accommodate it for those users who still use it.
REM
if not (%CM3Setup_HP%)==((NO_RESET)) set REACTOR_HOME=%CM3Setup_HP%
if defined REACTOR_HOME if not exist "%REACTOR_HOME%" echo Creating folder:  %REACTOR_HOME%
if defined REACTOR_HOME if not exist "%REACTOR_HOME%" mkdir "%REACTOR_HOME%"
if defined REACTOR_HOME if exist "%REACTOR_HOME%" goto ReactorShort
if defined USERPROFILE set REACTOR_HOME=%USERPROFILE%\My Documents\ReactorHome
if not defined REACTOR_HOME set REACTOR_HOME=%CM3_ROOT%\ReactorHome
if not exist "%REACTOR_HOME%" echo Creating folder:  %REACTOR_HOME%
if not exist "%REACTOR_HOME%" mkdir "%REACTOR_HOME%"
REM fall thru to ReactorShort

:ReactorShort
call :FN_MakeShort REACTOR_HOME "%REACTOR_HOME%"
goto Check_CM3_IDE
goto Done

:Check_CM3_IDE
if not (%CM3Setup_HP%)==((NO_RESET)) set CM3_IDE_HOME=%CM3Setup_HP%
if defined CM3_IDE_HOME if not exist "%CM3_IDE_HOME%" echo Creating folder:  %CM3_IDE_HOME%
if defined CM3_IDE_HOME if not exist "%CM3_IDE_HOME%" mkdir "%CM3_IDE_HOME%"
if defined CM3_IDE_HOME if exist "%CM3_IDE_HOME%" goto CM3_IDE_Short
if defined USERPROFILE set CM3_IDE_HOME=%USERPROFILE%\My Documents\CM3_IDE_Home
if not defined CM3_IDE_HOME set CM3_IDE_HOME=%CM3_ROOT%\CM3_IDE_Home
if not exist "%CM3_IDE_HOME%" echo Creating folder:  %CM3_IDE_HOME%
if not exist "%CM3_IDE_HOME%" mkdir "%CM3_IDE_HOME%"
REM fall thru to CM3_IDE_Short

:CM3_IDE_Short
call :FN_MakeShort CM3_IDE_HOME "%CM3_IDE_HOME%"
goto Done

:CleanUpPriorCM3
:---------------
REM In the event we are changing from one root to another, the first order of
REM business is to remove existing cm3 paths.
REM
if defined CM3_BIN call :FN_RemovePath %CM3_BIN%
goto :EOF

:KillEnvVars
:-----------
REM Perform UNDO option.
REM Remove all CM3 environment variable settings made by this CMD file.
REM Also, return the title of the window back to "Command Prompt".
REM
title Command Prompt
set CM3_ROOT=
set CM3_BIN=
set CM3_PKG=
set CM3_IDE_EXE=
set CM3_IDE_HOME=
set REACTOR_HOME=
set CM3_DoneSetup=
echo.
echo === Results of UNDO Option ===
call :ShowEnv
goto End

:Remarks
:-------
REM Perform REMARKS option.
REM
set CM3Setup_SavePrompt=%prompt%
prompt $H
echo HELP TEXT (REMARKS) FOLLOWS:
@echo on
@goto ShowRemarks

:ShowCurrent
:-----------
REM Perform SHOW option.
REM
echo.
echo === Current Environment for CM3 ===
call :ShowEnv
goto End

:FATAL_Env
:---------
@REM A fatal problem has been detected in the environment settings.
@REM
@echo off
set CM3_DoneSetup=
echo FATAL ERROR:  Unable to find proper CM3 installation.
echo ===========
echo     CM3_ROOT expected in folder %CM3_ROOT%
echo     CM3_BIN  expected in folder %CM3_BIN%
echo     CM3_PKG  expected in folder %CM3_PKG%
echo     CM3.EXE  expected in file   %CM3_BIN%\cm3.exe
echo CM3 IDE EXE  expected in file   %CM3_BIN%\%CM3_IDE_EXE%.exe
echo              or no recognized IDE variant found in %CM3_BIN%
echo.
set CM3Setup_ExitCode=2
goto End

:FATAL_InvalidArguments
:----------------------
@REM A fatal problem has been detected in parsing the command line.
@REM The user has made a mistake here, so try to point him in right direction.
@REM
@echo off
echo FATAL ERROR (Invalid arguments):  %*
echo ===========
echo.
set CM3Setup_ExitCode=1
REM fall thru to Help

:Help
:----
echo -------------------------------------------------------------------------------
echo USAGE:  cm3SetupCmdEnv [Help Remarks Show Undo [Root [path]] [Home [path]] [IDE name]]
echo -------------------------------------------------------------------------------
echo OPTIONAL COMMAND LINE PARAMETERS:
echo    REMARKS
echo       Display useful remarks located at the beginning of this command file.
echo       REMARKS cannot be combined with other arguments.
echo    HELP
echo       Display this summary of helpful information.
echo       HELP cannot be combined with other arguments.
echo    SHOW
echo       Display a summary of the current CM3 environment variable values.
echo       In this case, no variables are changed, they are just displayed.
echo       SHOW cannot be combined with other arguments.
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
echo       argument is omitted, a default will be used (typically, C:\cm3).  
echo       The "ROOT path" option is useful if you have multiple cm3 
echo       installation roots (e.g., different versions) and you want to switch
echo       between them, or if your installation is not in the default location.
echo    IDE exeName
echo       The "IDE" argument indicates you want to specify which IDE should be
echo       set as the primary one to launch via subsequent calls to cm3StartIDE.
echo       The "exeName" argument is required and should specify the name of the
echo       executable program minus the .exe extension, e.g., "reactor", 
echo       "cm3ide", "catalyst".  This program must exist in the bin folder of
echo       the cm3 installation root.  If no IDE argument is given, the default
echo       action is to search for one in the ROOT\bin folder.
echo    HOME path
echo       The "HOME" argument indicates you want to specify the location of 
echo       the IDE's (e.g., Reactor, CM3IDE) home folder for this user.  
echo       The "path" argument is optional, but if given, should be the path to 
echo       the home folder.  If the "path" argument is omitted, a default will 
echo       be used, typically "%%USERPROFILE%%\My Documents\ReactorHome" or 
echo       "%%USERPROFILE%%\My Documents\CM3_IDE_Home".
echo All keyword arguments are case-insensitive.
echo -------------------------------------------------------------------------------
echo EXAMPLES:
echo    cm3SetupCmdEnv                                cm3SetupCmdEnv UNDO
echo    cm3SetupCmdEnv HELP                           cm3SetupCmdEnv REMARKS
echo    cm3SetupCmdEnv ROOT path                      cm3SetupCmdEnv ROOT
echo    cm3SetupCmdEnv ROOT path HOME path            cm3SetupCmdEnv ROOT HOME path
echo    cm3SetupCmdEnv HOME path                      cm3SetupCmdEnv HOME
echo    cm3SetupCmdEnv HOME path ROOT path            cm3SetupCmdEnv HOME ROOT path
echo    cm3SetupCmdEnv ROOT path HOME                 cm3SetupCmdEnv ROOT HOME
echo    cm3SetupCmdEnv HOME path ROOT                 cm3SetupCmdEnv HOME ROOT
echo    cm3SetupCmdEnv ROOT path IDE name             cm3SetupCmdEnv IDE name
echo    cm3SetupCmdEnv HOME path IDE name             cm3SetupCmdEnv HOME IDE name
echo    cm3SetupCmdEnv ROOT path HOME path IDE name   cm3SetupCmdEnv SHOW
goto End

:FN_MakeShort
:------------
REM Function (MakeShort)
REM params:  1=env var name, 2=long path surrounded by quotes
REM result:  env var (1) is set to the mangled short equiv of long path (2)
REM
set %1=%~s2
goto :EOF

:FN_StringReplace
:----------------
REM Function (StringReplace)
REM params:  1=env var name, 2=str1, 3=str2 
REM          (make sure to enclose str1 and str2 in double quotes) 
REM result:  replace all occurances of str1 (2) in env var (1) with str2 (3)
REM
call :FN_StrRep2 "set %1=%%%1:%~2=%~3%%"
goto :EOF
:FN_StrRep2
%~1
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

:ShowEnv
:-------
REM Displays the current environment settings for cm3, then returns
REM (Make sure to use CALL :ShowEnv instead of GOTO ShowEnv)
REM
echo.
echo CM3_ROOT=%CM3_ROOT%
echo.
echo CM3_BIN=%CM3_BIN%
echo.
echo CM3_PKG=%CM3_PKG%
echo.
echo CM3_IDE_EXE=%CM3_IDE_EXE%
echo.
echo CM3_IDE_HOME=%CM3_IDE_HOME%
echo.
echo REACTOR_HOME=%REACTOR_HOME%
echo.
echo CM3_DoneSetup=%CM3_DoneSetup%
echo.
echo TEMP=%TEMP%
if defined TMP echo.
if defined TMP echo TMP=%TMP%
echo.
echo PATH=%PATH%
echo.
goto :EOF

:SetExitCode
:-----------
@if (%CM3Setup_ExitCode%)==(0) goto :Exit0
@if (%CM3Setup_ExitCode%)==(1) goto :Exit1
@if (%CM3Setup_ExitCode%)==(2) goto :Exit2
:UnknownExitCode
@set CM3Setup_ExitCode=
@exit /b 255
:Exit0
@set CM3Setup_ExitCode=
@exit /b 0
:Exit1
@set CM3Setup_ExitCode=
@exit /b 1
:Exit2
@set CM3Setup_ExitCode=
@exit /b 2
goto :EOF

:Done
:----
REM Everthing is good; we are done.  Show the user what has been achived.
REM
title cm3 Command Prompt
for /f "tokens=1-5" %%a in ('c:\cm3\bin\cm3.exe -version') do if /I (%%d)==(version) title %%a %%b %%c %%d %%e
set CM3_DoneSetup=TRUE
echo.
echo === Command Prompt Ready for CM3 ===
call :ShowEnv
%CM3_BIN%\cm3 -version
REM fall thru to End

:End
:---
REM Clean up temporary environment var usage before exiting.
REM
set CM3Setup_RP=
set CM3Setup_HP=
set CM3Setup_IDE=
set CM3Setup_DefaultInstallRoot=
set CM3Setup_DefaultTemp=
set CM3Setup_DefaultVCSetup=
set CM3Setup_DefaultSDKSetup=
if defined CM3Setup_SavePrompt prompt %CM3Setup_SavePrompt%
set CM3Setup_SavePrompt=
echo -------------------------------------------------------------------------------
@echo on
@goto SetExitCode

