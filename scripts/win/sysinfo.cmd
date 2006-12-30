@rem $Id: sysinfo.cmd,v 1.1 2006-12-30 01:33:51 jkrell Exp $

@if "%SYSINFO_DONE%" == "yes" goto :eof

@rem
@rem This is meant to be called by other .cmd files and should not have
@rem setlocal/endlocal.
@rem

@rem
@rem Currently the user must run vcvars32 or vsvars32, to set %path%, %lib%, and %include%, and a
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

@rem
@rem ROOT is two levels above this program.
@rem

if "%ROOT%" == "" call :set_full_path ROOT %~dp0..\..

@rem
@rem The variabls %temp%, %lib%, %path%, %include%, must be set.
@rem

call :environment_variables_must_be_set temp lib path include || goto :eof

@rem
@rem cl.exe, link.exe, cm3.exe must be in %path%.
@rem

call :environment_variable_must_contain_files path cl.exe link.exe cm3.exe || goto :eof

@rem
@rem errno.h must be in %include%, at least as a sanity check.
@rem

call :environment_variable_must_contain_files include errno.h || goto :eof


@rem
@rem Libcmt.lib must be in %lib%, at least as a sanity check.
@rem Msvcrt.lib would be good to require, but the 2003 Express Edition lacks it.
@rem

call :environment_variable_must_contain_files lib kernel32.lib libcmt.lib || goto :eof

@rem
@rem The Microsoft Visual C++ 2003 and 2005 Express Editions
@rem do not include user32.lib, gdi32.lib, comctl32.lib. Therefore we make our own.
@rem Therefore we add %INSTALLROOT%\lib to %lib%. Therefore we set %INSTALLROOT%
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
@echo warning: set INSTALLROOT=%~f1
@set INSTALLROOT=%~f1
goto :eof

:got_INSTALLROOT

@rem
@rem The %INSTALLROOT% environment variable must be set.
@rem This is the root of the CM3 installation.
@rem

call :environment_variables_must_be_set INSTALLROOT || goto :eof

@rem
@rem Check the %lib% environment variable.
@rem

rem call :environment_variable_must_contain_files_quiet lib wsock32.lib gdi32.lib comctl32.lib user32.lib && goto got_other_libs

if not exist %INSTALLROOT%\lib (
    @echo %INSTALLROOT%\lib does not exist.
    exit /b 1
)

if not exist %INSTALLROOT%\lib\comctl32.lib (
    @echo %INSTALLROOT%\lib\comctl32.lib does not exist, run %~dp0make-lib-2.
    exit /b 1
)

if not exist %INSTALLROOT%\lib\user32.lib (
    @echo %INSTALLROOT%\lib\user32.lib does not exist, run %~dp0make-lib-2.
    exit /b 1
)

if not exist %INSTALLROOT%\lib\wsock32.lib (
    @echo %INSTALLROOT%\lib\wsock32.lib does not exist, run %~dp0make-lib-2.
    exit /b 1
)

if not exist %INSTALLROOT%\lib\gdi32.lib (
    @echo %INSTALLROOT%\lib\gdi32.lib does not exist, run %~dp0make-lib-2.
    exit /b 1
)

@rem
@rem This is a warning because it can be confusing
@rem when switching between do-*.cmd and manually running
@rem cm3 that sysinfo.cmd changed the environment.
@rem
@rem That is -- you should not call sysinfo from the bare command line.
@rem It is only for use by other scripts.
@rem

echo warning: set lib=%%INSTALLROOT%%\lib;%%lib%%
set lib=%INSTALLROOT%\lib;%lib%

:got_other_libs

@rem
@rem TBD: Too many environment variables!
@rem TBD: Too much code here!
@rem TBD: More Modula-3, less sh/cmd/perl!
@rem TBD: Support bootstrapping from DEC SRC Modula-3, PM3, etc.
@rem TBD: Finish make-bin-dist-min
@rem

if defined CM3LIBSEARCHPATH set CM3LIBSEARCHPATH=
if defined CM3BINSEARCHPATH set CM3BINSEARCHPATH=
if defined EXE          set EXE=
if defined GCWRAPFLAGS  set GCWRAPFLAGS=
if defined GREP         set GREP=
if defined SCRIPTS      set SCRIPTS=
if defined SL           set SL=
if defined SYSLIBDIR    set SYSLIBDIR=
if defined SYSLIBS      set SYSLIBS=
if defined TMPDIR       set TMPDIR=
if defined TAR          set TAR=

@rem
@rem TBD: Support bootstrapping from DEC SRC Modula-3, PM3, etc.
@rem
call :set_if_empty M3BUILD m3build
call :set_if_empty M3SHIP:m3ship

call :set_if_empty CM3         cm3
call :set_if_empty PKGSDB      %ROOT%\scripts\win\PKGS
call :set_if_empty M3GDB       no
call :set_if_empty M3OSTYPE    WIN32
call :set_if_empty TARGET      NT386
call :set_if_empty GCC_BACKEND no
call :set_if_empty INSTALLROOT c:\cm3

set CM3ROOT=%ROOT:\=\\%

call :set_if_empty CM3VERSION d5.3.2

set SYSINFO_DONE=yes

goto :eof

:set_full_path
set %1=%~f2
goto :eof

:environment_variables_must_be_set
if "%1" == "" goto :eof
(set %1 2>nul| findstr /i /b %1= >nul) || (
    echo Environment variable %1 not defined
    exit /b 1
)
shift
goto :environment_variables_must_be_set

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

:set_if_empty
(set %1 2>nul) || (
    set %1=%2
    goto :eof
)
(set %1 2>nul| findstr /i /b %1= >nul) || set %1=%2
goto :eof
