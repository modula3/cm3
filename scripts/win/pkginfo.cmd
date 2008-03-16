@rem $Id$

@if "%_echo%" == "" @echo off

setlocal

call %~dp0sysinfo || exit /b 1

if not exist %PKGSDB% (
    echo making %PKGSDB% with %~dp0find-packages
    call %~dp0find-packages || goto :eof
)

if not exist %PKGSDB% (
    echo cannot generate package list
    exit /b 1
)

if not "%1" == "" (shift & goto :%1)

endlocal

goto :eof

:pkgpath

(for /f %%i in ('findstr /e /i /c:/%2 %PKGSDB%') do endlocal & set %1=%%i) || (
    echo package %2 not found
    error 2>nul
)

goto :eof
