@REM #!cmd.exe
@REM $Id$

@if "%_echo%" == "" @echo off

if "%SYSINFO_DONE%" == "yes" goto :eof

call %~dp0clearenv.cmd

if "%ROOT%" == "" call :set_full_path ROOT %~dp0..

call :environment_variables_must_be_set temp lib path include || goto :eof
call :environment_variable_must_contain_files lib  kernel32.lib user32.lib gdi32.lib msvcrt.lib || goto :eof
call :environment_variable_must_contain_files path cl.exe link.exe lib.exe || goto :eof
call :environment_variable_must_contain_files include errno.h || goto :eof

call :set_if_empty CM3         cm3
call :set_if_empty M3BUILD     m3build
call :set_if_empty M3SHIP      m3ship

set GCWRAPFLAGS=
call :set_if_empty SCRIPTS     %ROOT%\scripts
call :set_if_empty PKGSDB      %ROOT%\scripts\PKGS
call :set_if_empty M3GDB       no
call :set_if_empty M3OSTYPE    WIN32
call :set_if_empty TARGET      NT386
call :set_if_empty GCC_BACKEND no
call :set_if_empty INSTALLROOT c:\cm3
call :set_if_empty GREP        egrep
set CM3ROOT=%ROOT:\=\\%

set CM3LIBSEARCHPATH=%LIB:;= %
set CM3BINSEARCHPATH=%PATH:;= %
call :set_if_empty CM3VERSION d5.3.2
set EXE=.exe
set SL=\
set SYSLIBDIR=unknown
set SYSLIBS=advapi32.lib gdi32.lib kernel32.lib odbc32.lib
set SYSLIBS=%SYSLIBS% opengl32.lib wsock32.lib comdlg32.lib
set SYSLIBS=%SYSLIBS% glu32.lib netapi32.lib odbccp32.lib user32.lib
set TAR=tar
set TMPDIR=%TEMP%

set SYSINFO_DONE=yes

goto :eof

:set_full_path
set %1=%~f2
goto :eof

:environment_variables_must_be_set
if "%1" == "" goto :eof
(set %1 2>nul| findstr /i /b %1= >nul) || (
 echo Environment variable %1 not defined
 error 2>nul
 goto :eof
)
shift
goto :environment_variables_must_be_set

:environment_variable_must_contain_files
setlocal
set a=%1
for /f %%i in ('echo %%%1%%') do set b=%%i
:environment_variable_must_contain_files_1
shift
if "%1" == "" (
  endlocal
  goto :eof
)
if "%~$b:1" == "" (
  echo %a% does not contain %1
  endlocal
  error 2>nul
  goto :eof
)
goto :environment_variable_must_contain_files_1

:set_if_empty
(set %1 2>nul) || (
  set %1=%2
  goto :eof
)
(set %1 2>nul| findstr /i /b %1= >nul) || set %1=%2
goto :eof
