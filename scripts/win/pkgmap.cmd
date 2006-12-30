@rem $Id: pkgmap.cmd,v 1.2 2006-12-30 11:36:38 jkrell Exp $

@if "%_echo%" == "" @echo off

setlocal

call %~dp0sysinfo || exit /b 1
call %~dp0pkginfo || exit /b 1

set p=
set PKGS=
:pkgmap_1
if "%1" == "" (
  goto :pkgmap_2
)
if (%PKG_ACTION%) == (" ") set PKG_ACTION=
if "%1" == "-k" (
  set KEEP_GOING=yes
) else if "%1" == "-n" (
  set NO_ACTION=yes
) else if "%1" == "-l" (
  set LIST_ONLY=yes
) else if "%1" == "-c" (
  if not defined PKG_ACTION set PKG_ACTION=%2
  shift
) else if exist %ROOT%\%1\. (
  set PKGS=%PKGS% %ROOT%\%1
) else if exist (%1\.) (
  set PKGS=%PKGS% %1
) else (
    call %ROOT%\scripts\win\pkginfo pkgpath p %1 || (
        echo *** cannot find package %1 / %p%
        endlocal
	    exit /b 1
    )
)
if not "%p%" == "" (
    if exist %p%\. (
        set PKGS=%PKGS% %p%
    ) else if exist %ROOT%\%p% (
        set PKGS=%PKGS% %ROOT%\%p%
    ) else (
        echo *** cannot find package %1 / %p%
        endlocal
	    exit /b 1
    )
    set p=
)
shift
goto :pkgmap_1
:pkgmap_2
if not defined PKG_ACTION (
    echo no PKG_ACTION defined, aborting
    endlocal
	exit /b 1
)
if "%PKGS%" == "" (
    echo no packages
    endlocal
    exit /b 1
)
if "%LIST_ONLY%" == "yes" (
    REM call listpkgs %PKGS%
    echo error listpkgs not yet implemented
    endlocal
    exit /b 1
)
for %%p in (%PKGS%) do (
    echo === package %%p ===
    call :exec_cmd %%p || (
        if not "%KEEP_GOING%" == "yes" (
            echo *** execution of %ACTION% failed with %errorlevel% ***
            endlocal
            exit /b 1
        ) else (
            echo ==^> %PKG_ACTION% returned %res%
        )
    )
)
endlocal
goto :eof

:exec_cmd
echo +++ %PKG_ACTION% +++
if not "%NO_ACTION%" == "yes" (
  cd %1 && %PKG_ACTION:"=%
)
goto :eof
