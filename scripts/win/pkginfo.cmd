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


:FilterOnePackage
    @rem goto :FilterOnePackage_%1
    for %%a in (import_libs tcl serial X11R4 m3cc m3gdb) do if /i "%1" == "%%a" goto :FilterOnePackage_%1
    exit /b 0
    goto :eof

:FilterOnePackage_import-libs
:FilterOnePackage_tapi
    if /i "%M3OSTYPE%" == "WIN32" exit /b 0
    exit /b 1
        
:FilterOnePackage_tcl:
    if /i "%HAVE_TCL%" == "yes" exit /b 0
    exit /b 1

:FilterOnePackage_serial
    if /i "%HAVE_SERIAL%" == "yes" exit /b 0
    exit /b 1

:FilterOnePackage_X11R4
    if /i "%M3OSTYPE%" == "WIN32" exit /b 1
    exit /b 0

:FilterOnePackage_m3cc
    if /i "%GCC_BACKEND%" == "yes" if "%OMIT_GCC%" == "" exit /b 0
    exit /b 1
 
:FilterOnePackage_m3gdb
    if /i "%M3GDB%" == "yes" exit /b 0
    if /i "%CM3_GDB%" == "yes" exit /b 0
    if /i "%TARGET%" == "FreeBSD4" exit /b 0
    if /i "%TARGET%" == "LINUXLIBC6" exit /b 0
    if /i "%TARGET%" == "SOLgnu" exit /b 0
    exit /b 1
