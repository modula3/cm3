@rem $Id: install-cm3-compiler.cmd,v 1.1 2006-12-30 01:33:51 jkrell Exp $

@if "%_echo%" == "" @echo off

@rem
@rem This is based on install-cm3-compiler.sh.
@rem

setlocal

call %~dp0sysinfo || exit /b 1

set FRONTEND=%INSTALLROOT%\bin\cm3
set BACKEND=%INSTALLROOT%\bin\cm3cg
set FRONTEND_SRC=%ROOT%\m3-sys\cm3\%TARGET%\cm3
set BACKEND_SRC=%ROOT%\m3-sys\m3cc\%TARGET%\cm3cg

goto :main

:usage
    echo.
    echo install-cm3-compiler [ /n ] backup ^| newversion ^| upgrade
    echo install-cm3-compiler [ /n ] restore ^<version_number^>
    echo.
    echo   backup
    echo     will make copies of cm3 front-end and back-end with the cm3
    echo     version number as suffix, e.g.
    echo     %FRONTEND% --^> %FRONTEND%-x.y.z
    echo     %BACKEND% --^> %BACKEND%-x.y.z
    echo.
    echo   restore ^<version_number^>
    echo     will restore copies with suffixed version number as current
    echo     version, e.g.
    echo     %FRONTEND%-x.y.z --^> %FRONTEND%
    echo     %BACKEND%-x.y.z --^> %BACKEND%
    echo.
    echo   newversion
    echo     will install the version from the current workspace with
    echo     version number suffixes, e.g.
    echo     %FRONTEND_SRC% --^> %FRONTEND%-x.y.z
    echo     %BACKEND_SRC% --^> %BACKEND%-x.y.z
    echo.
    echo   upgrade
    echo     will backup the existing version, install new executables
    echo     with suffixes and restore them as the current version
    echo.
    echo   Beware: This script relies on the cm3 executable to correctly
    echo   identify its version (cm3 -version). If it does not, things will
    echo   get messed up.
    echo.
    goto :eof

:exit_if
    if /i "%NoAction%" == "yes" (
        echo error ignored due to -n
        goto :eof
    ) else (
        exit /b %1
    )
    goto :eof

:do_cp
    if /i not "%NoAction%" == "yes" (
        echo copy %1 %2
        copy %1 %2
        if errorlevel 1 call :exit_if 1
    ) else (
        echo would copy %1 %2
    )
    goto :eof

:cp_if
    if not exist %1 (
        echo cp_if: source does not exist: %1
        call :exit_if 1
        goto :eof
    )
    call :do_cp %1 %2
    goto :eof

:getversion
    @echo off
    for /f "tokens=5" %%i in ('%1 -version ^| findstr version') do echo %%i
    @if "%_echo%" == "" @echo off
    goto :eof

:install_local_as_version
    call :capture_output CM3VERSION "call %~dp0install-cm3-compiler getversion %FRONTEND_SRC%"
    set FRONTEND_CM3VERSION=%FRONTEND%-%CM3VERSION%
    set BACKEND_CM3VERSION=%BACKEND%-%CM3VERSION%
    set FRONTEND_DEST=%FRONTEND_CM3VERSION%
    set BACKEND_DEST=%BACKEND_CM3VERSION%
    call :cp_if %FRONTEND_SRC%.exe %FRONTEND_DEST%.exe
    call :cp_if %FRONTEND_SRC%.exe.manifest %FRONTEND_DEST%.exe.manifest
    call :cp_if %FRONTEND_SRC%.pdb %FRONTEND_DEST%.pdb
    if /i "%GCC_BACKEND%" == "yes" (
        rem call :cp_if %BACKEND_SRC% %BACKEND_DEST%
        call :cp_if %BACKEND_SRC%.exe %BACKEND_DEST%.exe
        call :cp_if %BACKEND_SRC%.manifest %BACKEND_DEST%.manifest
        call :cp_if %BACKEND_SRC%.pdb %BACKEND_DEST%.pdb
    )
    goto :eof

:backup_old
    call :capture_output OLDCM3VERSION "call %~dp0install-cm3-compiler getversion %FRONTEND%"
    call :cp_if %FRONTEND%.exe %FRONTEND%-%OLDCM3VERSION%.exe
    call :cp_if %FRONTEND%.exe.manifest %FRONTEND%-%OLDCM3VERSION%.exe.manifest
    call :cp_if %FRONTEND%.pdb %FRONTEND%-%OLDCM3VERSION%.pdb
    if /i "%GCC_BACKEND%" == "yes" (
        call :cp_if %BACKEND%.exe %BACKEND%-%OLDCM3VERSION%.exe
        call :cp_if %BACKEND%.exe.manifest %BACKEND%-%OLDCM3VERSION%.exe.manifest
        call :cp_if %BACKEND%.pdb %BACKEND%-%OLDCM3VERSION%.pdb
    )
    goto :eof

:rm_curent
    if /i not "%NoAction%" == "yes" (
        if exist %FRONTEND% del /f %FRONTEND%
        if exist %FRONTEND%.exe del /f %FRONTEND%.exe
        if exist %FRONTEND%.exe.manifest del /f %FRONTEND%.exe.manifest
        if exist %FRONTEND%.pdb del /f %FRONTEND%.pdb
        if /i "%GCC_BACKEND%" == "yes" (
            if exist %BACKEND% del /f %BACKEND%
            if exist %BACKEND%.exe del /f %BACKEND%.exe
            if exist %BACKEND%.exe.manifest del /f %BACKEND%.exe.manifest
            if exist %BACKEND%.pdb del /f %BACKEND%.pdb
        )
    )
    goto :eof

:cp_version
    call :cp_if %FRONTEND_CM3VERSION%.exe %FRONTEND%.exe
    call :cp_if %FRONTEND_CM3VERSION%.exe.manifest %FRONTEND%.exe.manifest
    call :cp_if %FRONTEND_CM3VERSION%.pdb %FRONTEND%.pdb
    if /i "%GCC_BACKEND%" == "yes" (
        rem call :cp_if %BACKEND_CM3VERSION% %BACKEND%
        call :cp_if %BACKEND_CM3VERSION%.exe %BACKEND%.exe
        call :cp_if %BACKEND_CM3VERSION%.exe.manifest %BACKEND%.exe.manifest
        call :cp_if %BACKEND_CM3VERSION%.pdb %BACKEND%.pdb
    )
    goto :eof

:upgrade
    call :backup_old
    call :install_local_as_version
    call :rm_curent
    call :cp_version
    goto :eof

:main
    if /i "%1" == "-n" (
        set NoAction=yes
        shift
	) else if /i "%1" == "/n" (
        set NoAction=yes
        shift
    )
    if /i "%1" == "upgrade" (
        call :upgrade
        goto :eof
    ) else if /i "%1" == "restore" (
        if "%2" == "" (
            echo please specify a version
            echo available versions are:
            dir /b %INSTALLROOT%\bin\cm3-*.exe
            exit /b 1
        )
        set CM3VERSION=%2
        set FRONTEND_CM3VERSION=%FRONTEND%-%2
        set BACKEND_CM3VERSION=%BACKEND%-%2
        call :cp_version
    ) else if /i "%1" == "newversion" (
        call :install_local_as_version
        goto :eof
    ) else if /i "%1" == "backup" (
        call :backup_old
        goto :eof
    ) else if /i "%1" == "help" (
        call :usage
        goto :eof
    ) else if /i "%1" == "getversion" (
        call :getversion %2
        goto :eof
    ) else (
        echo unknown command ^(try %0 help^)
	    exit /b 1
    )
    goto :eof

:capture_output
    for /f %%i in ('%2') do set %1=%%i
    goto :eof
