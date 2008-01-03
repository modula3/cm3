@echo off
set CM3_SR_SavePrompt=
:ShowRemarks
REM ===========================================================================
REM startReactor.CMD, written by R.C.Coleburn, 08/13/2003
REM v1.10--08/15/2003 by RCC
REM v1.11--08/18/2003 by RCC
REM v1.12--08/19/2003 by RCC
REM v1.13--08/29/2003 by RCC, adjusted for v1.30 of cm3SetupCmdEnv
REM ===========================================================================
REM PURPOSE:
REM    This Windows batch/command file sets up the environment for using cm3
REM    then launches Reactor, the browser-based IDE for cm3.
REM    External documentation is available as "Documentation_startReactor.htm"
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
REM    REACTOR_HOME is where Reactor looks for the user's proj folder
REM    CM3_SR_SavePrompt and CM3_SR_DefaultInstallRoot are temporary 
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
REM    HOME path
REM       The "HOME" argument indicates you want to specify the location of 
REM       the Reactor home folder for this user.  The "path" argument is 
REM       optional, but if given, should be the path to the home folder.  If
REM       the "path" argument is omitted, a default will be used, typically
REM       "%USERPROFILE%\ReactorHome".
REM All keyword arguments are case-insensitive.
REM Note that the optional command line arguments for startReactor.CMD and for
REM    cm3SetupCmdEnv.CMD must be identical, except that the UNDO argument is
REM    not supported by startReactor.CMD.
REM ---------------------------------------------------------------------------
REM USAGE EXAMPLES:
REM    startReactor
REM    startReactor HELP                    startReactor REMARKS
REM    startReactor ROOT path               startReactor ROOT
REM    startReactor ROOT path HOME path     startReactor ROOT HOME path
REM    startReactor HOME path               startReactor HOME
REM    startReactor HOME path ROOT path     startReactor HOME ROOT path
REM    startReactor ROOT path HOME          startReactor ROOT HOME
REM    startReactor HOME path ROOT          startReactor HOME ROOT
REM ===========================================================================
@echo off
if defined CM3_SR_SavePrompt goto End

:DEFAULT_LOCATIONS
:-----------------
REM ===========================================================================
REM Change the defaults below, if desired
REM ===========================================================================
set CM3_SR_DefaultInstallRoot=C:\cm3
REM ===========================================================================
rem fall thru to Begin

:Begin
:-----
echo -------------------------------------------------------------------------------
echo startReactor.CMD, written by R.C.Coleburn 08/13/2003, v1.13 08/29/2003 by RCC
echo -------------------------------------------------------------------------------

:Params1
:-------
if /I (%1)==(HELP) goto Help
if /I (%1)==(REMARKS) goto Remarks
if /I (%1)==(UNDO) goto FATAL_InvalidArguments
for %%C in (Help Remarks Undo) do if /I (%2)==(%%C) goto FATAL_InvalidArguments
for %%C in (Help Remarks Undo) do if /I (%3)==(%%C) goto FATAL_InvalidArguments
for %%C in (Help Remarks Undo) do if /I (%4)==(%%C) goto FATAL_InvalidArguments
rem fall thru to CheckBasicSetup

:CheckBasicSetup
:---------------
REM CM3_ROOT is the root of the CM3 installation, default is "C:\cm3"
REM CM3_BIN is where the CM3 binaries are kept, typically "C:\cm3\bin"
if not defined CM3_ROOT set CM3_ROOT=%CM3_SR_DefaultInstallRoot%
set CM3_BIN=%CM3_ROOT%\bin
if not exist "%CM3_BIN%\cm3SetupCmdEnv.CMD" goto FATAL_Env
rem fall thru to Params2

:Params2
:-------
if not (%1)==() call "%CM3_BIN%\cm3SetupCmdEnv.CMD" %*
@echo off
if not (%1)==() if /i not (%CM3_DoneSetup%)==(TRUE) goto FATAL_InvalidArguments
rem fall thru to CheckSetup

:CheckSetup
:----------
if /i not (%CM3_DoneSetup%)==(TRUE) call "%CM3_BIN%\cm3SetupCmdEnv.CMD" %*
@echo off
if /i not (%CM3_DoneSetup%)==(TRUE) goto FATAL_Env
if not defined CM3_ROOT goto FATAL_Env
if not defined CM3_BIN goto FATAL_Env
if not exist "%CM3_ROOT%" goto FATAL_Env
if not exist "%CM3_BIN%\cm3.exe" goto FATAL_Env
if not exist "%CM3_BIN%\reactor.exe" goto FATAL_Env
if not exist "%CM3_BIN%\cm3SetupCmdEnv.CMD" goto FATAL_Env
goto StartReactor

:Remarks
:-------
set CM3_SR_SavePrompt=%prompt%
prompt $H
echo HELP TEXT (REMARKS) FOLLOWS:
@echo on
@goto ShowRemarks

:FATAL_Env
:--------
set CM3_DoneSetup=
echo FATAL ERROR:  Unable to find CM3 installation.
echo           CM3_ROOT expected in folder %CM3_ROOT%
echo           CM3_BIN  expected in folder %CM3_BIN%
echo           CM3.EXE  expected in file   %CM3_BIN%\cm3.exe
echo        Reactor.EXE expected in file   %CM3_BIN%\reactor.exe
echo cm3SetupCmdEnv.CMD expected in file   %CM3_BIN%\cm3SetupCmdEnv.CMD
goto End

:FATAL_InvalidArguments
:-----------------------
echo FATAL ERROR:  Invalid arguments.
echo               %0 %*
rem fall thru to Help

:Help
:----
echo ---------------------------------------------------------------------------
echo USAGE:  startReactor [Help Remarks [Root [path]] [Home [path]]]
echo ---------------------------------------------------------------------------
echo OPTIONAL COMMAND LINE PARAMETERS:
echo    REMARKS
echo       Display useful remarks located at the beginning of this command file.
echo       REMARKS cannot be combined with other arguments.
echo    HELP
echo       Display this summary of helpful information.
echo       HELP cannot be combined with other arguments.
echo    ROOT path
echo       The "ROOT" argument indicates you want to specify the cm3 
echo       installation root.  The "path" argument is optional, but if given,
echo       should be the path to the cm3 installation root.  If the "path"
echo       argument is omitted, a default will be used (typically, C:\cm3).  
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
echo    startReactor
echo    startReactor HELP                    startReactor REMARKS
echo    startReactor ROOT path               startReactor ROOT
echo    startReactor ROOT path HOME path     startReactor ROOT HOME path
echo    startReactor HOME path               startReactor HOME
echo    startReactor HOME path ROOT path     startReactor HOME ROOT path
echo    startReactor ROOT path HOME          startReactor ROOT HOME
echo    startReactor HOME path ROOT          startReactor HOME ROOT
goto End

:StartReactor
:------------
echo.
echo Starting Reactor . . .
title Reactor Console Output
%CM3_BIN%\reactor
echo.
echo Reactor has stopped.
rem fall thru to End

:End
:---
set CM3_SR_DefaultInstallRoot=
if defined CM3_SR_SavePrompt prompt %CM3_SR_SavePrompt%
set CM3_SR_SavePrompt=
echo -------------------------------------------------------------------------------
@echo on
