@REM #!cmd.exe
@REM $Id: pkginfo.cmd,v 1.1 2004-04-27 15:04:39 sk Exp $

@if "%_echo%" == "" @echo off

call %~dp0sysinfo.cmd

if not exist %PKGSDB% (
  echo making %PKGSDB% with %~dp0find-packages.cmd
  call %~dp0find-packages.cmd
)

if not exist %PKGSDB% (
  echo cannot generate package list
  error 2>nul
  goto :eof
)

if not "%1" == "" (shift & goto :%1)

goto :eof

:pkgpath
(for /f %%i in ('findstr /e /i \%2 %PKGSDB%') do set %1=%%i) || (
  echo package %2 not found
  error 2>nul
)
goto :eof
