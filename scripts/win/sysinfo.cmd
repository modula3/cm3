@rem $Id: sysinfo.cmd,v 1.24 2009-07-21 08:30:58 jkrell Exp $

@if not "%1" == "" (shift & goto :%1)

@if "%SYSINFO_DONE%" == "yes" goto :eof

@rem
@rem This is meant to be called by other .cmd files and should not have
@rem setlocal/endlocal.
@rem

@rem
@rem Currently the user must run vcvars32 or vsvars32, to set %PATH%, %LIB%, and %INCLUDE%, and a
@rem static configuration will attempt to work with various versions.
@rem
@rem In the future the environment should perhaps be probed to the configuration
@rem made ideal. For example we should use /Zi and /MD with as many toolsets
@rem as possible (i.e.: all but the 2003 Express Edition). Or maybe just don't
@rem support that toolset.
@rem
@rem some data for the future:
@rem
@rem VC4:
@rem  MSDevDir=e:\MSDEV
@rem
@rem VC5:
@rem  MSDevDir=E:\Program Files\DevStudio\SharedIDE
@rem  MSVCDir=E:\Program Files\DevStudio\VC
@rem
@rem VC 2003 Express
@rem  VCToolkitInstallDir=E:\Program Files\Microsoft Visual C++ Toolkit 2003 (not set by vcvars32)
@rem
@rem VC 2005 Express
@rem  VCINSTALLDIR=E:\Program Files\Microsoft Visual Studio 8\VC
@rem  VS80COMNTOOLS=E:\Program Files\Microsoft Visual Studio 8\Common7\Tools
@rem  VSINSTALLDIR=E:\Program Files\Microsoft Visual Studio 8
@rem  DevEnvDir=E:\Program Files\Microsoft Visual Studio 8\Common7\IDE
@rem
@rem type 1.c
@rem _MSC_VER
@rem
@rem \msvc10\bin\cl /EP 1.c 2>nul
@rem 800
@rem \msvc15\bin\cl /EP 1.c 2>nul
@rem 800
@rem
@rem Note that VC 1.0 seems to have problems, it does not run on Windows XP for example,
@rem though it works in the excellent "DosBox" program.
@rem 1.0 and 1.5 target 16bit architectures. The 1.5 "logo" says 8.00c whereas 1.0 says 8.00.
@rem TBD: Find 1.52. See if we care about these older tools.
@rem
@rem \msvc20\bin\cl /EP 1.c 2>nul
@rem 900
@rem \msdev40\bin\cl /EP 1.c 2>nul
@rem 1000
@rem \msdev41\bin\cl /EP 1.c 2>nul
@rem 1010
@rem Presumably msdev42 => 1020 but I don't have this version yet.
@rem \msdev50\vc\bin\cl /EP 1.c 2>nul
@rem 1100
@rem \msdev60\vc\bin\cl /EP 1.c 2>nul
@rem 1200
@rem "\Program Files\Microsoft Visual Studio .NET\\Vc7\bin\cl" /EP 1.c 2>nul
@rem 1300
@rem "\Program Files\Microsoft Visual C++ Toolkit 2003\bin\cl" /EP 1.c 2>nul
@rem 1310
@rem "\Program Files\Microsoft Visual Studio 8\VC\bin\cl" /EP 1.c 2>nul
@rem 1400
@rem
@rem
@rem TBD: Support more compilers:
@rem  Digital Mars (Symantec)
@rem  Cygwin
@rem  MinGWin
@rem  Open Watcom
@rem

@if "%_echo%" == "" @echo off

@call %~dp0clearenv || exit /b 1

if defined CM3_TARGET if not "%CM3_TARGET%" == "NT386" if not "%CM3_TARGET%" == "NT386GNU" (
    @echo ERROR: If CM3_TARGET is defined, it must be NT386 or NT386GNU.
    @echo ERROR: Support is yet lacking for other NT architectures and cross builds.
    exit /b 1
)

if defined TARGET if not "%TARGET%" == "NT386" if not "%TARGET%" == "NT386GNU" (
    @echo ERROR: If TARGET is defined, it must be NT386 or NT386GNU.
    @echo ERROR: Support is yet lacking for other NT architectures and cross builds.
    exit /b 1
)

if not defined CM3_TARGET if defined TARGET (
    set CM3_TARGET=%TARGET%
)
if not defined TARGET if defined CM3_TARGET (
    set TARGET=%CM3_TARGET%
)

@rem
@rem ROOT is two levels above this program.
@rem

if "%ROOT%" == "" call :set_full_path ROOT %~dp0..\..

if not defined TARGET set TARGET=NT386

if "%TARGET%" == "NT386" (
    call :CheckAndConfigureNT386Environment || exit /b 1
)
if "%TARGET%" == "NT386GNU" (
    call :CheckAndConfigureNT386GNUEnvironment || exit /b 1
)

goto :end_CheckAndConfigureNT386GNUEnvironment
:CheckAndConfigureNT386GNUEnvironment
    call :environment_variables_must_be_set path || exit /b 1
    call :environment_variables_maybe_not_to_set lib include || exit /b 1
    call :environment_variable_must_contain_files path ld.exe gcc.exe cm3.exe || exit /b 1
    exit /b 0
:end_CheckAndConfigureNT386GNUEnvironment

@goto :end_CheckAndConfigureNT386Environment
:CheckAndConfigureNT386Environment
@rem
@rem The variabls %temp%, %LIB%, %PATH%, %INCLUDE%, must be set.
@rem

call :environment_variables_must_be_set temp lib path include || exit /b 1

@rem
@rem cl.exe, link.exe, cm3.exe must be in %PATH%.
@rem

call :environment_variable_must_contain_files path cl.exe link.exe cm3.exe || exit /b 1

@rem
@rem errno.h must be in %INCLUDE%, at least as a sanity check.
@rem

call :environment_variable_must_contain_files include errno.h || exit /b 1

@rem
@rem Libcmt.lib must be in %LIB%, at least as a sanity check.
@rem Msvcrt.lib would be good to require, but the 2003 Express Edition lacks it.
@rem

call :environment_variable_must_contain_files lib kernel32.lib libcmt.lib || exit /b 1

for %%a in (DELAYLOAD) do (
    if not defined USE_%%a (
        call :check_for_link_switch %%a
    )
)
for %%a in (MSVCRT) do (
    if not defined USE_%%a (
        call :check_for_lib %%a
    )
)
exit /b 0

:end_CheckAndConfigureNT386Environment

@goto :end_check_for_link_switch
:check_for_link_switch
    set USE_%1=0
    link | findstr /i /c:" /%1" > nul && (
         set USE_%1=1
    )
    @goto :eof
:end_check_for_link_switch

@goto :end_check_for_lib
:check_for_lib
    set USE_%1=1
    link %1.lib /nologo | findstr /i %1.lib >nul && (
         set USE_%1=0
    )
    @goto :eof
:end_check_for_lib

@rem
@rem The Microsoft Visual C++ 2003 and 2005 Express Editions
@rem do not include user32.lib, gdi32.lib, comctl32.lib. Therefore we make our own.
@rem Therefore we add %INSTALLROOT%\lib to %LIB%. Therefore we set %INSTALLROOT%
@rem based on where cm3.exe is in the path.
@rem

if defined INSTALLROOT goto :got_INSTALLROOT
setlocal
call :set_INSTALLROOT cm3.exe
endlocal & set INSTALLROOT=%INSTALLROOT%
goto :got_INSTALLROOT

:set_INSTALLROOT
@rem
@rem Walk up twice from \cm3\bin\cm3.exe to \cm3.
@rem
call :set_INSTALLROOT_2 %~$PATH:1\..\..
goto :eof
:set_INSTALLROOT_2
@set INSTALLROOT=%~f1
goto :eof

:got_INSTALLROOT
@rem @echo 3: INSTALLROOT=%INSTALLROOT%
@echo INSTALLROOT=%INSTALLROOT%

@rem
@rem The %INSTALLROOT% environment variable must be set.
@rem This is the root of the CM3 installation.
@rem

call :environment_variables_must_be_set INSTALLROOT || exit /b 1

@rem
@rem This is a warning because it can be confusing
@rem when switching between do-*.cmd and manually running
@rem cm3 that sysinfo.cmd changed the environment.
@rem
@rem That is -- you should not call sysinfo from the bare command line.
@rem It is only for use by other scripts.
@rem

if "%TARGET%" == "NT386" (
    @set LIB=%INSTALLROOT%\lib;%LIB%
    @echo LIB=%%INSTALLROOT%%\LIB;%%LIB%%
)

@rem
@rem TBD: Too many environment variables!
@rem TBD: Too much code here!
@rem TBD: More Modula-3, less sh/cmd/perl!
@rem TBD: Finish make-bin-dist-min
@rem

set EXE=.exe

call :set_if_empty M3BUILD m3build
call :set_if_empty M3SHIP m3ship

call :set_if_empty CM3         cm3
call :set_if_empty PKGSDB      %ROOT%\scripts\PKGS
call :set_if_empty M3GDB       no
call :set_if_empty M3OSTYPE    WIN32
call :set_if_empty TARGET      NT386
call :set_if_empty GCC_BACKEND no
call :set_if_empty INSTALLROOT c:\cm3

set CM3ROOT=%ROOT:\=\\%
echo CM3ROOT=%CM3ROOT%

call :GetVersions %~dp0..\version || exit /b 1

set SYSINFO_DONE=yes

goto :eof

:set_full_path
set %1=%~f2
goto :eof

:environment_variables_must_be_set
if "%1" == "" exit /b 0
if not defined %1 (
    echo ERROR: Environment variable %1 not defined
    exit /b 1
)
shift
goto :environment_variables_must_be_set

:environment_variables_maybe_not_to_set
if "%1" == "" exit /b 0
if defined %1 (
    echo warning: The environment variable %1 is set. This is ok, but perhaps unintended.
)
shift
goto :environment_variables_maybe_not_to_set

:environment_variable_must_contain_files_quiet
setlocal
set a=%1
for /f "tokens=*" %%i in ('echo %%%1%%') do set b=%%i
:environment_variable_must_contain_files_quiet_1
shift
if "%1" == "" (
  endlocal
  goto :eof
)
if "%~$b:1" == "" (
    endlocal
    exit /b 1
)
goto :environment_variable_must_contain_files_quiet_1

:environment_variable_must_contain_files
setlocal
set a=%1
for /f "tokens=*" %%i in ('echo %%%1%%') do set b=%%i
:environment_variable_must_contain_files_1
shift
if "%1" == "" (
  endlocal
  goto :eof
)
if "%~$b:1" == "" (
    echo %a% does not contain %1
    endlocal
    exit /b 1
)
goto :environment_variable_must_contain_files_1

:set_if_not_defined
:set_if_empty
if not defined %1 (
    set %1=%2
    @rem @echo 2: %1=%2
)
goto :eof

:Echo
    @setlocal
    @set a=%*
    @rem first and last character -- quotes
    @set a=%a:~1,-1%
    @echo %a%
    @endlocal
    @goto :eof

:GetVersions

@rem
@rem
@rem Look in ../sysinfo.sh for particularly formed lines, something like
@rem
@rem CM3VERSION d5.7.1
@rem CM3VERSIONNUM 050701
@rem CM3LASTCHANGED 2009-01-21
@rem
@rem
@rem Search for lines that start Name= (/b for beginging),
@rem for our particuliar names. Break up those lines at
@rem the colon and equals, taking the first and last token,
@rem removing the first and last two characters from the last token.
@rem
@rem Finding the quoted part was a bit difficult, but not impossible.
@rem
@rem Like the Python code, this is carefully written to only
@rem read the file once, if any one of the variables is not set,
@rem and to only overwrite what isn't already set.
@rem
@rem

set CM3Versions=^
    CM3VERSION ^
    CM3VERSIONNUM ^
    CM3LASTCHANGED

for %%a in (%CM3Versions%) do (
    if not defined %%a (
        @rem
        @rem We are forced to make a function call here
        @rem because cmd has problems with parentheses.
        @rem
        call :GetVersions_ReadFile %1 || (
            set CM3Versions=
            exit /b 1
        )
    )
)

set CM3Versions=

goto :eof

:GetVersions_ReadFile

@rem echo in %0 (should only happen once)

SetLocal EnableExtensions EnableDelayedExpansion

@rem
@rem Build up ONE findstr command line.
@rem
set SearchStrings=
for %%a in (%CM3Versions%) do (
    set SearchStrings=!SearchStrings! /c:%%a
)

set Result=
for /f "tokens=1,2" %%a in ('findstr /b !SearchStrings! %1') do (
    @rem echo 1: %%a
    @rem echo 2: %%b
    if not defined %%a (
        @rem
        @rem remove first and last two characters
        @rem
        set b=%%b
        @rem echo set %%a=%%b
        @rem
        @rem if you merely set %%a=!b:~2,-2! here, getting
        @rem the results past the EndLocal takes more work.
        @rem
        set Result=!Result! "%%a=%%b"
    )
)

EndLocal & for %%a in (%Result%) do set %%a

for %%a in (%CM3Versions%) do (
    if not defined %%a (
        @echo ERROR: %%a not found in %1
        exit /b 1
    )
)

goto :eof
