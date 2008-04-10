@echo off
set CM3_IDE_SavePrompt=
:ShowRemarks
REM ===========================================================================
REM cm3StartIDE.CMD, written by R.C.Coleburn, 08/13/2003
REM v1.10--08/15/2003 by RCC
REM v1.11--08/18/2003 by RCC
REM v1.12--08/19/2003 by RCC
REM v1.13--08/29/2003 by RCC, adjusted for v1.30 of cm3SetupCmdEnv
REM v2.00--01/20/2008 by RCC, renamed startReactor to cm3StartIDE, adjusted to 
REM                           find the IDE, e.g. Reactor, CM3_IDE, Catalyst and
REM                           to be compatible with v1.40 of cm3SetupCmdEnv.CMD
REM v2.01--04/10/2008 by RCC, change default search names for CM3-IDE (Olaf likes lower case, e.g. cm3ide)
REM ===========================================================================
REM PURPOSE:
REM    This Windows batch/command file sets up the environment for using cm3
REM    then launches the browser-based IDE for cm3.
REM    External documentation is available as "Documentation_cm3StartIDE.htm"
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
REM INSTALLATION/SETUP:
REM    Store this file in the bin directory of your cm3 installation.
REM    Place a shortcut to this file on your desktop.  Change the properties of
REM    this shortcut to run it in a minimized window.
REM ---------------------------------------------------------------------------
REM DEPENDENCIES:
REM    The cm3SetupCmdEnv.CMD file must be in the bin directory of your cm3
REM    installation.  It is used by this command file.
REM ---------------------------------------------------------------------------
REM ENVIRONMENT VARIABLE USAGE:
REM    CM3_ROOT is the root of the CM3 installation, default is "C:\cm3"
REM    CM3_BIN is where the CM3 binaries are kept, typically "C:\cm3\bin"
REM    CM3_IDE_EXE is name of the IDE executable, e.g., Reactor, CM3_IDE, Catalyst
REM    CM3_IDE_HOME is where the CM3_IDE looks for the user's proj folder
REM    REACTOR_HOME is where the Reactor IDE looks for the user's proj folder
REM    CM3_IDE_SavePrompt and CM3_IDE_DefaultInstallRoot are temporary 
REM    environment variables used internally.
REM ---------------------------------------------------------------------------
REM OPTIONAL COMMAND LINE ARGUMENTS TO THIS BATCH/COMMAND FILE:
REM    REMARKS
REM       Display useful remarks located at the beginning of this command file.
REM       REMARKS cannot be combined with other arguments.
REM    HELP
REM       Display a summary of helpful information.
REM       HELP cannot be combined with other arguments.
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
REM Note that the optional command line arguments for cm3StartIDE.CMD and for
REM    cm3SetupCmdEnv.CMD must be identical, except that the UNDO argument is
REM    not supported by cm3StartIDE.CMD.
REM ---------------------------------------------------------------------------
REM USAGE EXAMPLES:
REM    cm3StartIDE
REM    cm3StartIDE HELP                           cm3StartIDE REMARKS
REM    cm3StartIDE ROOT path IDE  name            cm3StartIDE name
REM    cm3StartIDE ROOT path                      cm3StartIDE ROOT
REM    cm3StartIDE ROOT path HOME path            cm3StartIDE ROOT HOME path
REM    cm3StartIDE HOME path                      cm3StartIDE HOME
REM    cm3StartIDE HOME path ROOT path            cm3StartIDE HOME ROOT path
REM    cm3StartIDE ROOT path HOME                 cm3StartIDE ROOT HOME
REM    cm3StartIDE HOME path ROOT                 cm3StartIDE HOME ROOT
REM    cm3StartIDE ROOT path IDE name             cm3StartIDE IDE name
REM    cm3StartIDE HOME path IDE name             cm3StartIDE HOME IDE name
REM    cm3StartIDE ROOT path HOME path IDE name
REM ===========================================================================
@echo off
if defined CM3_IDE_SavePrompt goto End

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
set CM3_IDE_DefaultInstallRoot=C:\cm3
if not defined CM3_IDE_EXE set CM3_IDE_EXE=cm3ide
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
echo cm3StartIDE.CMD, written by R.C.Coleburn 08/13/2003, rev 2.01 04/10/2008 by RCC
echo -------------------------------------------------------------------------------
echo.

:Params1
:-------
REM First stage of command line option processing.  
REM
if /I (%1)==(HELP) goto Help
if /I (%1)==(REMARKS) goto Remarks
REM
REM Since we will pass the command line arguments to cm3SetupCmdEnv.CMD, we don't 
REM want to allow any of the following to be passed on:  HELP, REMARKS, UNDO, SHOW
REM
for %%C in (Help Remarks Undo Show) do if /I (%1)==(%%C) goto FATAL_InvalidArguments
for %%C in (Help Remarks Undo Show) do if /I (%2)==(%%C) goto FATAL_InvalidArguments
for %%C in (Help Remarks Undo Show) do if /I (%3)==(%%C) goto FATAL_InvalidArguments
for %%C in (Help Remarks Undo Show) do if /I (%4)==(%%C) goto FATAL_InvalidArguments
for %%C in (Help Remarks Undo Show) do if /I (%5)==(%%C) goto FATAL_InvalidArguments
for %%C in (Help Remarks Undo Show) do if /I (%6)==(%%C) goto FATAL_InvalidArguments
if not (%7)==() goto FATAL_InvalidArguments
REM fall thru to CheckBasicSetup

:CheckBasicSetup
:---------------
REM CM3_ROOT is the root of the CM3 installation, default is "C:\cm3"
REM CM3_BIN is where the CM3 binaries are kept, typically "C:\cm3\bin"
REM
if not defined CM3_ROOT set CM3_ROOT=%CM3_IDE_DefaultInstallRoot%
set CM3_BIN=%CM3_ROOT%\bin
if not exist "%CM3_BIN%\cm3SetupCmdEnv.CMD" goto FATAL_Env
REM fall thru to Params2

:Params2
:-------
REM Second stage of command line processing.
REM Basically, if there are command line arguments, they need to be passed on
REM to cm3SetupCmdEnv.CMD for processing.  This is true even if cm3SetupCmdEnv
REM has been run previously and the environ is already setup because the new
REM command line arguments may necessitate a change.
REM
REM At this point, we are in one of three states:
REM (1) the user passed in some arguments that may change the environment so we
REM     need to call cm3SetupCmdEnv to ensure the environment is set properly; OR
REM (2) cm3SetupCmdEnv has not been run, so we need to run it to set up the
REM     environment now; OR
REM (3) the user has ran cm3SetupCmdEnv already, i.e., before calling cm3StartIDE
REM     and we don't need to make any changes to the environment.
REM So, for cases #1 and #2 we need to run cm3SetupCmdEnv now.
REM
REM Handle Case #1:
if not (%1)==() call "%CM3_BIN%\cm3SetupCmdEnv.CMD" %*
@if ERRORLEVEL 1 goto End
@echo off
if not (%1)==() if /i not (%CM3_DoneSetup%)==(TRUE) goto FATAL_Env
REM
REM Handle Case #2:
if /i not (%CM3_DoneSetup%)==(TRUE) call "%CM3_BIN%\cm3SetupCmdEnv.CMD" %*
@echo off
REM fall thru to CheckSetup

:CheckSetup
:----------
REM Verify that the environment is good.
REM
if /i not (%CM3_DoneSetup%)==(TRUE) goto FATAL_Env
if not defined CM3_ROOT goto FATAL_Env
if not defined CM3_BIN goto FATAL_Env
if not exist "%CM3_ROOT%" goto FATAL_Env
if not exist "%CM3_BIN%\cm3.exe" goto FATAL_Env
if not exist "%CM3_BIN%\cm3SetupCmdEnv.CMD" goto FATAL_Env
if not defined CM3_IDE_EXE set CM3_IDE_EXE=cm3ide
REM
REM Now, try to locate the IDE
REM the default search order is as shown below.  To change it, just reorder the lines
REM
if not exist "%CM3_BIN%\%CM3_IDE_EXE%.exe" set CM3_IDE_EXE=cm3ide
if not exist "%CM3_BIN%\%CM3_IDE_EXE%.exe" set CM3_IDE_EXE=CM3IDE
if not exist "%CM3_BIN%\%CM3_IDE_EXE%.exe" set CM3_IDE_EXE=CM3_IDE
if not exist "%CM3_BIN%\%CM3_IDE_EXE%.exe" set CM3_IDE_EXE=Catalyst
if not exist "%CM3_BIN%\%CM3_IDE_EXE%.exe" set CM3_IDE_EXE=Reactor
if not exist "%CM3_BIN%\%CM3_IDE_EXE%.exe" goto FATAL_Env
goto StartIDE

:Remarks
:-------
REM Perform the REMARKS option.
REM
set CM3_IDE_SavePrompt=%prompt%
prompt $H
echo HELP TEXT (REMARKS) FOLLOWS:
@echo on
@goto ShowRemarks

:FATAL_Env
:--------
@REM A fatal problem has been detected in the environment settings.
@REM
@echo off
set CM3_DoneSetup=
echo FATAL ERROR:  Unable to find a proper CM3 installation.
echo ===========
echo     CM3_ROOT expected in folder %CM3_ROOT%
echo     CM3_BIN  expected in folder %CM3_BIN%
echo     CM3_PKG  expected in folder %CM3_PKG%
echo     CM3.EXE  expected in file   %CM3_BIN%\cm3.exe
echo CM3 IDE EXE  expected in file   %CM3_BIN%\%CM3_IDE_EXE%.exe
echo              or no recognized IDE variant found in %CM3_BIN%
echo cm3SetupCmdEnv.CMD expected in file   %CM3_BIN%\cm3SetupCmdEnv.CMD
echo.
goto End

:FATAL_InvalidArguments
:-----------------------
@REM A fatal problem has been detected in parsing the command line.
@REM The user has made a mistake here, so try to point him in right direction.
@REM
@echo off
echo FATAL ERROR (Invalid arguments):  %0 %*
echo ===========
echo.
REM fall thru to Help

:Help
:----
echo -------------------------------------------------------------------------------
echo USAGE:  cm3StartIDE [Help Remarks [Root [path]] [Home [path]] [IDE name]]
echo -------------------------------------------------------------------------------
echo OPTIONAL COMMAND LINE PARAMETERS:
echo    REMARKS
echo       Display useful remarks located at the beginning of this command file.
echo       REMARKS cannot be combined with other arguments.
echo    HELP
echo       Display this summary of helpful information.
echo       HELP cannot be combined with other arguments.
echo    ROOT path
echo       The "ROOT" argument indicates you want to specify the cm3 installation 
echo       root.  The "path" argument is optional, but if given, should be the path 
echo       to the cm3 installation root.  If the "path" argument is omitted, a 
echo       default will be used (typically, C:\cm3).  The "ROOT path" option is 
echo       useful if you have multiple cm3 installation roots (e.g., different 
echo       versions) and you want to switch between them, or if your installation is
echo       not in the default location.
echo    IDE exeName
echo       The "IDE" argument indicates you want to specify which IDE should be set
echo       as the primary one to launch via subsequent calls to cm3StartIDE.  The
echo       "exeName" argument is required and should specify the name of the 
echo       executable program minus the .exe extension, e.g., "reactor", "cm3ide", 
echo       "catalyst".  This program must exist in the bin folder of the cm3 
echo       installation root.  If no IDE argument is given, the default action is to
echo       search for one in the ROOT\bin folder.
echo    HOME path
echo       The "HOME" argument indicates you want to specify the location of the 
echo       IDE's (e.g., Reactor, CM3IDE) home folder for this user.  The "path"
echo       argument is optional, but if given, should be the path to the home 
echo       folder.  If the "path" argument is omitted, a default will be used, 
echo       typically "%%USERPROFILE%%\My Documents\ReactorHome" or 
echo       "%%USERPROFILE%%\My Documents\CM3_IDE_Home".
echo All keyword arguments are case-insensitive.
echo -------------------------------------------------------------------------------
echo EXAMPLES:
echo    cm3StartIDE
echo    cm3StartIDE HELP                           cm3StartIDE REMARKS
echo    cm3StartIDE ROOT path                      cm3StartIDE ROOT
echo    cm3StartIDE ROOT path HOME path            cm3StartIDE ROOT HOME path
echo    cm3StartIDE HOME path                      cm3StartIDE HOME
echo    cm3StartIDE HOME path ROOT path            cm3StartIDE HOME ROOT path
echo    cm3StartIDE ROOT path HOME                 cm3StartIDE ROOT HOME
echo    cm3StartIDE HOME path ROOT                 cm3StartIDE HOME ROOT
echo    cm3StartIDE ROOT path IDE name             cm3StartIDE IDE name
echo    cm3StartIDE HOME path IDE name             cm3StartIDE HOME IDE name
echo    cm3StartIDE ROOT path HOME path IDE name
goto End

:StartIDE
:--------
REM Ok, we are aready to launch the IDE...
REM
echo.
echo Launching IDE for CM3 (%CM3_BIN%\%CM3_IDE_EXE%.exe). . .
echo.
echo (Note:  Do not close the "%CM3_IDE_EXE% Console Output" window.
echo         If you want to stop the IDE, just close the browser window.)
echo.
pushd \
cd /D %CM3_BIN%
REM
REM NOTE we will use START to launch the IDE in a new window.  The environ 
REM we so carefully setup will be passed on to the new window.
REM
start "%CM3_IDE_EXE% Console Output" /MIN %CM3_BIN%\%CM3_IDE_EXE%.exe
popd
REM fall thru to End

:End
:---
@REM Clean up temporary environment var usage before exiting.
@REM
@echo off
set CM3_IDE_DefaultInstallRoot=
if defined CM3_IDE_SavePrompt prompt %CM3_IDE_SavePrompt%
set CM3_IDE_SavePrompt=
echo -------------------------------------------------------------------------------
@echo on

