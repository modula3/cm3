@echo off
set CM3P_SavePrompt=
:ShowRemarks
REM ===========================================================================
REM cm3Proj.CMD, written by R.C.Coleburn, 08/16/2003
REM v0.00--08/13/2003 by RCC
REM v1.00--08/16/2003 by RCC
REM v1.01--08/18/2003 by RCC
REM v1.10--08/29/2003 by RCC, completed nested PROJ capability
REM ===========================================================================
REM PURPOSE:
REM    This Windows batch/command file aids users is building, shipping, 
REM    cleaning, and archiving sources for a set of cm3 packages rooted at a 
REM    common folder/path.
REM    External documentation is available as "Documentation_cm3Proj.htm"
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
REM ---------------------------------------------------------------------------
REM DEPENDENCIES:
REM    The cm3SetupCmdEnv.CMD file, cm3.exe, TAR.exe, and GZip.exe must be in
REM    the bin directory of your cm3 installation.  They are used by this 
REM    command file.
REM ---------------------------------------------------------------------------
REM ENVIRONMENT VARIABLE USAGE:
REM    CM3_ROOT is the root of the CM3 installation, default is "C:\cm3"
REM    CM3_BIN is where the CM3 binaries are kept, typically "C:\cm3\bin"
REM    CM3_PKG is the location of the public repository, typically "C:\cm3\pkg"
REM
REM    CM3P_Answer, CM3P_Archive, CM3P_cm3Opts, CM3P_DefaultInstallRoot, 
REM    CM3P_Fatal, CM3P_File, CM3P_List, CM3P_Make, CM3P_Nest, CM3P_Pause, 
REM    CM3P_PriorFolder, CM3P_ProjName, CM3P_ProjRoot, and CM3P_SavePrompt are
REM    temporary env vars used internally.
REM ---------------------------------------------------------------------------
REM FILE USAGE:
REM    This batch/command file uses text files named "name.CM3P" where "name"
REM    is the name of the project.  These files are stored in the root project
REM    folder (i.e., the folder containing the various package folders 
REM    comprising the project).  These files specify the contents and build 
REM    order for the packages in the project.  See Help text for details.
REM ===========================================================================
@echo off
set CM3P_Fatal=
if defined CM3P_SavePrompt goto End

:Nest
:----
if not (%1)==((NEST)) goto Begin
set CM3P_Nest=TRUE
shift
echo ===============================================================================
goto Params1

:DEFAULT_LOCATIONS
:-----------------
REM ===========================================================================
REM Change the defaults below, if desired
REM ===========================================================================
set CM3P_DefaultInstallRoot=C:\cm3
REM ===========================================================================
rem fall thru to Begin

:Begin
:-----
echo ===============================================================================
echo cm3Proj.CMD, written by R.C.Coleburn 08/13/2003, v1.10 08/29/2003 by RCC
echo ===============================================================================

:Params1
:-------
for %%C in (Help Remarks) do if /I (%1)==(%%C) goto %%C
rem fall thru to CheckSetup

:CheckSetup
:----------
REM CM3_ROOT is the root of the CM3 installation, default is "C:\cm3"
REM CM3_BIN is where the CM3 binaries are kept, typically "C:\cm3\bin"
if not defined CM3_ROOT set CM3_ROOT=%CM3P_DefaultInstallRoot%
if not defined CM3_BIN set CM3_BIN=%CM3_ROOT%\bin
if not exist "%CM3_BIN%\cm3SetupCmdEnv.CMD" goto FATAL_Env
if /i not (%CM3_DoneSetup%)==(TRUE) call "%CM3_BIN%\cm3SetupCmdEnv.CMD"
@echo off
if /i not (%CM3_DoneSetup%)==(TRUE) goto FATAL_Env
if not exist "%CM3_ROOT%" goto FATAL_Env
if not exist "%CM3_PKG%" goto FATAL_Env
if not exist "%CM3_BIN%\cm3.exe" goto FATAL_Env
if not exist "%CM3_BIN%\cm3Proj.CMD" goto FATAL_Env
if not exist "%CM3_BIN%\cm3SetupCmdEnv.CMD" goto FATAL_Env
if not exist "%CM3_BIN%\tar.exe" goto FATAL_Env
if not exist "%CM3_BIN%\gzip.exe" goto FATAL_Env
rem fall thru to Params2

:Params2
:-------
rem recall usage:  cm3Proj Project Command Options cm3Options
if (%2)==() goto Usage
if (%1)==() goto Usage
if not exist "%1.CM3P" if /I not (%2)==(Make) goto FATAL_NoProject
if exist "%1.CM3P" if /I (%2)==(Make) goto FATAL_ProjExists
if /I (%3)==(NoPause) (set CM3P_Pause=) ELSE set CM3P_Pause=TRUE
call :GetCM3opts %*
for %%C in (Help Remarks) do if /I %2==%%C goto %%C
for %%C in (Make Build Clean Ship Find Spotless ArcSrc ZapBak DelNT386 List Local Depend) do if /I %2==%%C goto Params3
goto Usage

:GetCM3opts
:----------
rem recall usage:  cm3Proj Project Command Options cm3Options
rem There are two required command line params followed by any OPTIONS followed by any cm3Options.
set CM3P_cm3Opts=
if defined CM3P_Nest goto :nestedOpts
if not defined CM3P_Pause for /f "tokens=3*" %%A in ("%*") do set CM3P_cm3Opts=%%B
if defined CM3P_Pause for /f "tokens=2*" %%A in ("%*") do set CM3P_cm3Opts=%%B
goto :EOF

:nestedOpts
rem have to add 1 to the token positions due to presence of (NEST) as 1st argument
if not defined CM3P_Pause for /f "tokens=4*" %%A in ("%*") do set CM3P_cm3Opts=%%B
if defined CM3P_Pause for /f "tokens=3*" %%A in ("%*") do set CM3P_cm3Opts=%%B
goto :EOF

:Params3
:-------
rem extract project name (i.e., omit path, if any)
for /F %%I in ("%1") do set CM3P_ProjName=%%~nI
rem
rem extract drive and path (omit filename)
for /F %%I in ("%1") do set CM3P_ProjRoot=%%~dpI
rem
set CM3P_File=%CM3P_ProjRoot%%CM3P_ProjName%.CM3P
if /I (%2)==(Make) if not (%CD%\)==(%CM3P_ProjRoot%) goto CheckRoot
rem fall thru to Params4

:Params4
:-------
echo   Project Name = %CM3P_ProjName%
echo   Project Root = %CM3P_ProjRoot%
echo   Project File = %CM3P_File%
echo        Command = %2
echo    CM3 options = %CM3P_cm3Opts%
rem fall thru to Params5

:Params5
:-------
set CM3P_PriorFolder=%CD%
cd /d %CM3P_ProjRoot%
for %%C in (Make Build Clean Ship Find Spotless ArcSrc ZapBak DelNT386 List Local Depend) do if /I %2==%%C goto %%C
goto FATAL_Control

:CheckRoot
:---------
echo   Project Name = %CM3P_ProjName%
echo   Project Root = %CD%\
echo   Project File = %CM3P_File%
echo        Command = %2
echo    CM3 options = %CM3P_cm3Opts%
echo WARNING:  When making a new project file, the current folder is considered the 
echo           project root.  The current folder is "%CD%\",
echo           but you've specified to store the project file in folder 
echo           "%CM3P_ProjRoot%".  
call :FN_GetYesNo CM3P_Answer "Would you like to change the project root to be this folder"
if %CM3P_Answer%==1 goto Params4
set CM3P_ProjRoot=%CD%\
goto Params5

:Make
:----
echo Command Action = MAKE (create new CM3P project file)
echo.
echo *** Scanning for packages in %CM3P_ProjRoot%
echo     to create project; One moment please...
rem
rem output first two lines to file
echo ;Created by cm3Proj.CMD on %DATE% at %TIME% for user %username%>"%CM3P_File%"
echo NAME %CM3P_ProjName%>>"%CM3P_File%"
rem
rem walk the project root folder tree looking for packages
for /R %CM3P_ProjRoot% %%I in (m3makefile) do call :Make2 %%I
echo.
echo Resulting Project File "%CM3P_File%" is:
echo -------------------------------------------------------------------------------
more /e <"%CM3P_File%"
goto End

:Make2
rem Make2's purpose:  If %1 represents a package, add it to the project file
rem
rem first, if %1 does not end in "\src\m3makefile" it isn't a package
set CM3P_Make=%1
set CM3P_Make=%CM3P_Make:~-15%
if /I not (%CM3P_Make%)==(\src\m3makefile) goto :EOF
rem
rem extract the path to %1, relative to %CM3P_ProjRoot%
set CM3P_Make=%1
set CM3P_Make=%CM3P_Make:~0,-15%
call :FN_StringReplace CM3P_Make "%CM3P_ProjRoot%" ""
rem
rem add this pkg to the file
echo PKG %CM3P_Make%>>"%CM3P_File%"
goto :EOF

:Build
:-----
echo Command Action = BUILD and install packages [cm3 -build; cm3 -ship]
goto doCM3command

:Clean
:-----
echo Command Action = CLEAN [cm3 -clean] (clean packages to remove derived files)
goto doCM3command

:Ship
:----
echo Command Action = SHIP [cm3 -ship] (install packages)
goto doCM3command

:Find
:----
echo Command Action = FIND [cm3 -find] (locate source files)
goto doCM3command

:List
:----
echo Command Action = LIST (list files comprising each package)
goto doCM3command

:Local
:-----
echo Command Action = LOCAL [cm3 -build] (build locally; do not install packages)
goto doCM3command

:Depend
:------
echo Command Action = DEPEND [cm3 -depend] (show package dependencies)
goto doCM3command

:ShowFile
:--------
echo.
echo Project File "%CM3P_File%" contains:
echo -------------------------------------------------------------------------------
if defined CM3P_Pause more /e <"%CM3P_File%"
if not defined CM3P_Pause type %CM3P_File%
goto :EOF

:doCM3command
:------------
call :ShowFile
for /F "eol=; tokens=1,2,3" %%A in (%CM3P_File%) do call :processDirective %%A %%B %2 doCM3pkg %%C
goto End

:processDirective
:----------------
rem params:  1=keyword (NAME PKG PROJ)
rem          2=path to package/project
rem          3=action (build, clean, ship, find, list, spotless, zapbak, arcsrc, delnt386, local, depend)
rem          4=label for goto for processing each line
rem          5=project name (only if this is a nested PROJ specification)
if not (%1)==(PKG) if not (%1)==(PROJ) if not (%1)==(NAME) goto FATAL_ProjFileErr
if (%1)==(NAME) if /I not (%2)==(%CM3P_ProjName%) goto FATAL_ProjFileErr
if (%1)==(NAME) goto :EOF
if (%1)==(PROJ) goto doProj
if not (%5)==() goto FATAL_ProjFileErr
if (%4)==() goto FATAL_Control
for %%C in (Build Clean Ship Find List Spotless ZapBak ArcSrc DelNT386 Local Depend) do if /I %3==%%C goto %4
goto FATAL_Control

:doProj
:------
if (%5)==() goto FATAL_ProjFileErr
if not (%6)==() goto FATAL_Control
echo -------------------------------------------------------------------------------
echo *** Processing nested PROJ directive for project:  %5 
echo     rooted at:  %2
echo     (i.e., %2\%5.CM3P) ...
if (%CM3P_Pause%)==(TRUE) goto doProj2_pause

:doProj1_noPause
rem echo ***(HANDLING NESTED PROJ)***: cmd /c "%CM3_BIN%\cm3Proj.CMD (NEST) %2\%5 %3 NoPause %CM3P_cm3Opts%"
cmd /c "%CM3_BIN%\cm3Proj.CMD (NEST) %2\%5 %3 NoPause %CM3P_cm3Opts%"
goto :EOF

:doProj2_pause
rem echo ***(HANDLING NESTED PROJ)***: cmd /c "%CM3_BIN%\cm3Proj.CMD (NEST) %2\%5 %3 %CM3P_cm3Opts%"
cmd /c "%CM3_BIN%\cm3Proj.CMD (NEST) %2\%5 %3 %CM3P_cm3Opts%"
goto :EOF

:doCM3pkg
:--------
echo -------------------------------------------------------------------------------
if not exist %2\src goto MissingPackage
pushd %2\src
if /I (%3)==(local) goto localPkg
if /I (%3)==(depend) goto dependPkg
echo *** %3ing package %2 ...
if /I (%3)==(list) goto listPkg
if /I (%3)==(build) goto buildPkg
%CM3_BIN%\cm3 -%3 %CM3P_cm3Opts%
if defined CM3P_Pause if errorlevel 1 pause
popd
goto :EOF

:dependPkg
echo *** checking package dependencies for %2 ...
%CM3_BIN%\cm3 -depend %CM3P_cm3Opts%
if defined CM3P_Pause if errorlevel 1 pause
popd
goto :EOF

:localPkg
echo *** building package %2 ...
%CM3_BIN%\cm3 -build %CM3P_cm3Opts%
if defined CM3P_Pause if errorlevel 1 pause
popd
echo *** Not shipping, because local build specified.
goto :EOF

:listPkg
rem Note, an alternate format is:  dir /d | more /e +5
cd ..
for /R %%F in (*) do call :listPkg2 "%%~fF" %2 "%%~tF" "%%~zF"
popd
if defined CM3P_Pause pause
goto :EOF
rem
:listPkg2
rem params: 1=quoted fully qualified path string, 2=path prefix string to remove, 
rem         3=quoted date/time string, 4=quoted size (in bytes) string
set CM3P_List=%~1
call :FN_StringReplace CM3P_List "%CM3P_ProjRoot%%2\" ""
rem display date/time, size, hard tab, relative path to file
echo    %~3      %~4 bytes 	%CM3P_List%
goto :EOF

:buildPkg
%CM3_BIN%\cm3 -build %CM3P_cm3Opts%
if errorlevel 1 goto skipShip
echo.
echo *** shipping %2 ...
cm3 -ship
if defined CM3P_Pause if errorlevel 1 pause
popd
goto :EOF

:skipShip
echo.
echo *** not installing package (%2) because of build errors ***
if defined CM3P_Pause pause
popd
goto :EOF

:Spotless
:--------
echo Command Action = SPOTLESS (remove derived NT386 folders and 
echo                            package folders from respository)
call :ShowFile
echo -------------------------------------------------------------------------------
call :FN_GetYesNo CM3P_Answer "Are you sure you want to make this project Spotless"
if %CM3P_Answer%==0 goto Abort
for /F "eol=; tokens=1,2,3" %%A in (%CM3P_File%) do call :processDirective %%A %%B %2 spotlessPkg %%C
goto End

:spotlessPkg
echo.
echo *** deleting derived folder: %2\NT386
if exist %2\NT386 rmdir /s/q %2\NT386
echo *** deleting public repository package folder: %CM3_PKG%\%~n2
if exist %CM3_PKG%\%~n2 rmdir /s/q %CM3_PKG%\%~n2
goto :EOF

:DelNT386
:--------
echo Command Action = DelNT386 (remove derived NT386 folders)
call :ShowFile
echo -------------------------------------------------------------------------------
call :FN_GetYesNo CM3P_Answer "Are you sure you want to do remove this project's derived NT386 folders"
if %CM3P_Answer%==0 goto Abort
for /F "eol=; tokens=1,2,3" %%A in (%CM3P_File%) do call :processDirective %%A %%B %2 delNT386pkg %%C
goto End

:delNT386pkg
echo.
echo *** deleting derived folder: %2\NT386
if exist %2\NT386 rmdir /s/q %2\NT386
goto :EOF

:ZapBak
:------
echo Command Action = ZAPBAK (remove *.bak files)
call :ShowFile
echo -------------------------------------------------------------------------------
call :FN_GetYesNo CM3P_Answer "Are you sure you want to remove this project's .bak files"
if %CM3P_Answer%==0 goto Abort
for /F "eol=; tokens=1,2,3" %%A in (%CM3P_File%) do call :processDirective %%A %%B %2 zapBakPkg %%C
goto End

:zapBakPkg
echo.
echo *** deleting backup files in folder tree: %2\src
if not exist %2\src goto MissingPackage
del /s/q %2\src\*.bak
goto :EOF

:ArcSrc
:------
echo Command Action = ARCSRC (archive all project sources)
call :ShowFile
echo -------------------------------------------------------------------------------
set CM3P_Archive=%CM3P_ProjName%_Sources
if exist %CM3P_Archive%.tgz goto FATAL_ArchiveExists
if exist %TEMP%\%CM3P_Archive% rmdir /s/q %TEMP%\%CM3P_Archive%
mkdir %TEMP%\%CM3P_Archive%
if not exist %TEMP%\%CM3P_Archive% goto FATAL_NoArchiveTemp
echo.
echo *** copying project root's ReadMe files ...
if exist read* xcopy /q read* %TEMP%\%CM3P_Archive%
for /F "eol=; tokens=1,2,3" %%A in (%CM3P_File%) do call :processDirective %%A %%B %2 arcPkg %%C
echo.
echo *** deleting .bak and .asv files from temporary storage ...
del /s/q %TEMP%\%CM3P_Archive%\*.bak >>NUL:
del /s/q %TEMP%\%CM3P_Archive%\*.asv >>NUL:
echo.
echo *** creating tar archive ...
pushd %TEMP%
tar --create --totals --file %CM3P_Archive%.tar %CM3P_Archive%
echo.
echo *** compressing archive ...
gzip %CM3P_Archive%.tar
popd
echo.
echo *** storing tgz archive as %CM3P_Archive%.tgz ...
move %TEMP%\%CM3P_Archive%.tar.gz %CM3P_Archive%.tgz
echo.
echo *** removing temporary files ...
rmdir /s/q %TEMP%\%CM3P_Archive%
goto End

:arcPkg
echo.
echo *** copying sources for package %2 ...
if not exist %2\src goto MissingPackage
mkdir %TEMP%\%CM3P_Archive%\%2
xcopy /s /i /q %2\src %TEMP%\%CM3P_Archive%\%2\src
goto :EOF

:MissingPackage
:--------------
echo ERROR:  Package not found:  %2\src
if defined CM3P_Pause pause
goto :EOF

:Abort
:-----
echo Command action aborted.
goto End

:FATAL_NotImplementedYet
:-----------------------
set CM3P_Fatal=TRUE
echo.
echo FATAL ERROR:  An unimplemented feature has been attempted.
goto End

:FATAL_Control
:-------------
set CM3P_Fatal=TRUE
echo.
echo FATAL ERROR:  Invalid control parameters, %*
goto End

:FATAL_ArchiveExists
:-------------------
set CM3P_Fatal=TRUE
echo.
echo FATAL ERROR:  Unable to create new source code archive because an 
echo               archive already exists.
dir %CM3P_Archive%.tgz | more +4
goto End

:FATAL_NoArchiveTemp
:-------------------
set CM3P_Fatal=TRUE
echo.
echo FATAL ERROR:  Unable to create temporary folder for archive at
echo               %TEMP%\%CM3P_Archive%
goto End

:FATAL_NoProject
:---------------
set CM3P_Fatal=TRUE
echo.
echo FATAL ERROR:  Unable to find project definition file "%1.CM3P"
goto End

:FATAL_ProjFileErr
:-----------------
set CM3P_Fatal=TRUE
echo.
echo FATAL ERROR:  Project file is invalid/corrupt
echo               at line:  %1 %2 %5
echo Valid project file lines are of the form:
echo    ;comment
echo    NAME ThisProject
echo    PKG PathToPackageRoot
echo    PROJ PathToProjectRoot NestedProject
echo Use the 'help' command for details.
goto End

:FATAL_ProjExists
:----------------
set CM3P_Fatal=TRUE
echo.
echo FATAL ERROR:  Unable to create project file "%1.CM3P" because file 
echo               already exists.
goto End

:FATAL_Env
:---------
set CM3P_Fatal=TRUE
echo.
echo FATAL ERROR:  Unable to find CM3 installation.
echo           CM3_ROOT expected in folder %CM3_ROOT%
echo            CM3_BIN expected in folder %CM3_BIN%
echo            CM3_PKG expected in folder %CM3_PKG%
echo            CM3.EXE expected in file   %CM3_BIN%\cm3.exe
echo            TAR.EXE expected in file   %CM3_BIN%\TAR.exe
echo           GZip.EXE expected in file   %CM3_BIN%\GZip.exe
echo cm3SetupCmdEnv.CMD expected in file   %CM3_BIN%\cm3SetupCmdEnv.CMD
echo        cm3Proj.CMD expected in file   %CM3_BIN%\cm3Proj.CMD
goto End

:Remarks
:-------
set CM3P_SavePrompt=%prompt%
prompt $H
echo REMARKS FOLLOW:
@echo on
@goto ShowRemarks

:Usage
:-----
echo Usage:  cm3Proj Project Command Option cm3Options
echo         cm3Proj Help
echo         cm3Proj Remarks
echo where, 
echo    "Project" is the pathname (absolute or relative to current folder) of 
echo              the .CM3P project file, without the .CM3P extension, 
echo              e.g. myPath\myProject
echo    "Command" is one of:  Make, Build, Clean, Ship, Find, Spotless, ArcSrc,
echo              ZapBak, DelNT386, List, Local, Depend, Help, or Remarks
echo    "Option"  is either omitted, or one of:  NoPause
echo              Unless "NoPause" is specified, whenever cm3.exe returns an 
echo              error, the user is prompted to "Press any key to continue..."
echo              Omitting the "NoPause" option is useful to highlight problems
echo              with individual packages when building large numbers of 
echo              packages.
echo              When used with the "List" command, "NoPause" prevents the user
echo              from being prompted to "Press any key to continue..." after 
echo              each package listing.
echo "cm3Options" are any valid command line options to cm3.exe.  These will be
echo              passed to cm3.exe without verification.
echo Case of commands and options is not significant.  
echo Use "cm3Proj Help" or "cm3Proj Remarks" for more helpful information.
if /I (%1)==(HELP) goto Help2
if /I (%2)==(HELP) goto Help2
goto End

:Help
:----
echo Command Action = HELP
echo ---------------------
echo.
goto Usage

:Help2
echo -------------------------------------------------------------------------------
echo.
echo For each package/project (yes you can nest projects) specified in the 
echo    "Project.CM3P" file, "Command" is carried out.  
echo The meaning of each command is as follows:
echo.
echo   ArcSrc = Using TAR / GZIP, create the source archive [Project]_Sources.tgz
echo            This compressed tar file contains all of the project's source files,
echo            i.e., those in the package src folders.  Note that the tree 
echo            structure of the packages is preserved.
echo.
echo    Build = cm3 -build; cm3 -ship  (ship is skipped if build unsuccessful)
echo.
echo    Clean = cm3 -clean
echo.
echo DelNT386 = Delete derived NT386 folders within the project's packages.
echo.
echo   Depend = cm3 -depend   (Display package dependencies)
echo.
echo     Find = cm3 -find   (Locate source files)
echo.
echo     Help = Display this summary of helpful information.
echo.
echo     List = List files comprising each package's folder tree.
echo.
echo    Local = cm3  (build locally; do not ship to repository)
echo.
echo     Make = Assume the current folder is the project root.  Scan this folder
echo            for packages.  Packages are detected by the presence of the file
echo            "pkg\src\m3makefile", where pkg is any subfolder within the file
echo            system tree rooted at the current folder.  Create a new project
echo            file named ".\Project.CM3P" containing all of the packages found.
echo            You should probably edit this file to reorder the packages in 
echo            the correct compilation order wrt package dependencies.
echo.
echo     Ship = cm3 -ship
echo            Install private packages in the public repository.
echo.
echo  Remarks = Display helpful remarks embedded at the beginning of this 
echo            command file.
echo.
echo Spotless = Delete derived NT386 folders within the packages and also delete
echo            the corresponding package folders from the public repository 
echo            (typically rooted at C:\cm3\pkg).  Note that for the public 
echo            package folders, the entire folder, including "src", "NT386", and
echo            other platform subfolders are deleted.
echo.
echo   ZapBak = Delete all backup files within the package folders.  A file is
echo            considered to be a backup file if its last extension is ".bak".
echo.
echo .CM3P File Format:
echo -----------------
echo The file contains one or more text lines.  Each line can be one of the
echo following four types, subject to the constraints and interpretation given:
echo    ;comment
echo    NAME ThisProject
echo    PKG PathToPackageRoot
echo    PROJ PathToProjectRoot NestedProject
echo.
echo *Spaces and tabs are considered delimiters, thus "ThisProject", 
echo     "NestedProject", "PathToPackageRoot", and "PathToProjectRoot" cannot 
echo     contain embedded spaces/tabs.  "ThisProject.CM3P" must be the actual 
echo     filename (no path) of the file.
echo *The semicolon (;) is treated as an end-of-line comment character.  That is,
echo     any characters following a semicolon are ignored.
echo *"NAME", "PKG", and "PROJ" are case-sensitive keywords.
echo *Only one NAME, PKG, or PROJ specification is permitted per line.
echo *The "NAME" keyword defines "ThisProject" as the name of this project.
echo     The NAME directive must be the first non-comment line in the file.
echo     Only one NAME directive is permitted per file.
echo *The "PKG" keyword defines "PathToPackageRoot" as a package.
echo     "PathToPackageRoot" is the absolute or relative path to the folder 
echo     containing the package's "src" subfolder.
echo *The "PROJ" keyword defines "NestedProject" as a project containing a
echo     "PathToProjectRoot\NestedProject.CM3P" file.  Nesting of projects is
echo     permitted up to whatever limit is imposed by the Windows command 
echo     processor.
echo *Multiple PKG and PROJ specificationlines can occur in the file.
echo *The order of the PKG and PROJ specification lines is the order that the 
echo     packages and projects will be considered by cm3Proj for each "Command"
echo     action.  A nested project is processed and completed before continuing
echo     further in the parent project file.
echo *WARNING:  A nested PROJ specification that specifies a project that is
echo     already part of the "tree of projects" will cause infinite recursion!
goto End

:InvalidYesNoResponse
:--------------------
echo Please answer 'y' or 'n'.
rem fall thru to FN_GetYesNo

:FN_GetYesNo
:-----------
rem Function (GetYesNo)
rem params:  1=env var to receive answer, 2=question enclosed in quotes
rem result:  question is displayed followed by " [y,n] ? "
rem          after user input is validated, the env var (1) is set to 1 for 
rem          affirmative (yes) or 0 for negative (no)
set /p %1=%~2 [y,n] ? 
for /f "usebackq" %%A in (`echo %%%1%%`) do if /I not (y)==(%%A) if /I not (n)==(%%A) goto InvalidYesNoResponse
for /f "usebackq" %%A in (`echo %%%1%%`) do if /I (y)==(%%A) (set %1=1) ELSE set %1=0
for /f "usebackq" %%A in (`echo %%%1%%`) do if /I (1)==(%%A) (echo yes) ELSE echo no
goto :EOF

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

:FatalEnd
:--------
set CM3P_Fatal=
EXIT /B 1
goto :EOF

:End
:---
rem Clean-up before exit
if (%CM3P_Fatal%)==(TRUE) if defined CM3P_Nest if defined CM3P_Pause pause
set CM3P_Answer=
set CM3P_Archive=
set CM3P_cm3Opts=
set CM3P_DefaultInstallRoot=
set CM3P_File=
set CM3P_List=
set CM3P_Make=
set CM3P_Pause=
set CM3P_ProjName=
set CM3P_ProjRoot=
if defined CM3P_PriorFolder cd /d %CM3P_PriorFolder%
set CM3P_PriorFolder=
if defined CM3P_SavePrompt prompt %CM3P_SavePrompt%
set CM3P_SavePrompt=
echo ===============================================================================
if (%CM3P_Fatal%)==(TRUE) goto FatalEnd
@echo on
