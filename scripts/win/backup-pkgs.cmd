@rem $Id$

@if "%_echo%" == "" @echo off

@rem
@rem This is based on backup-pkgs.sh and is used by install-cm3-compiler.
@rem

@setlocal EnableDelayedExpansion

@call %~dp0clearenv || exit /b 1
@call %~dp0sysinfo || exit /b 1

if defined FORCE set FORCE=
if /i "%1" == "-force" (
    set FORCE=yes
    shift
) else if /i "%1" == "-f" (
    set FORCE=yes
    shift
) else if /i "%1" == "/force" (
    set FORCE=yes
    shift
) else if /i "%1" == "/f" (
    set FORCE=yes
    shift
)

if defined RESTORE set RESTORE=
if /i "%1" == "-restore" (
    set RESTORE=yes
    shift
) else if /i "%1" == "-r" (
    set RESTORE=yes
    shift
) else if /i "%1" == "/restore" (
    set RESTORE=yes
    shift
) else if /i "%1" == "/r" (
    set RESTORE=yes
    shift
)

if defined BACKUPID set BACKUPID=
if not "%1" == "" (
    set BACKUPID=%1
    shift
)
if not defined BACKUPID (
    @set BACKUPID=%CM3VERSION%
)
if not defined BACKUPID (
    @echo %~n0 : BACKUPID not defined ^(not specified on command line and CM3VERSION not defined^)
    @exit /b 1
)

rem m3gc-simple
rem m3cc

@set p_runtime=^
m3gc-enhanced ^
m3core ^
libm3
@set p_compiler=^
m3middle ^
m3objfile ^
m3linker ^
m3back ^
m3staloneback ^
m3front ^
m3quake ^
cm3 ^
mklib
@set P=^
%p_runtime% ^
%p_compiler% ^
m3scanner ^
m3tools ^
m3cgcat ^
m3cggen ^
m3bundle ^
dll2lib ^
fix_nl ^
libdump

@echo cd /d %INSTALLROOT%
cd /d %INSTALLROOT% || exit /b 1

@rem
@rem delete if empty, to automatically recover from some error situations
@rem
rmdir pkg-%BACKUPID% 2>nul

if not defined RESTORE (
    set SRC=pkg
    set DEST=pkg-%BACKUPID%
    if exist pkg-%BACKUPID% (
        if not defined FORCE (
            call :check_complete_backup || exit /b 1
            echo %~n0 : %CD%\pkg-%BACKUPID% already exists, and appears complete, ok
            exit /b 0
        )
    ) else (
      mkdir pkg-%BACKUPID%
    )
) else (
    set SRC=pkg-%BACKUPID%
    set DEST=pkg
)

for %%a in (%P%) do (
    if not exist %SRC%\%%a (
        echo %SRC%\%%a does not exist
		echo Be sure to build everything with latest released tools
		echo before trying to upgrade the compiler and build it with itself.
		echo That is, run do-cm3-core buildship before running upgrade.
        exit /b 1
    )
    echo %CD%\%SRC%\%%a --^> %CD%\%DEST%\%%a
    (xcopy /iveryc %SRC%\%%a %DEST%\%%a | findstr copied) || exit /b 1
)

@endlocal

@goto :eof

:check_complete_backup
for %%a in (%P%) do (
    if not exist %DEST%\%%a (
        echo %~n0 : %CD%\%DEST% already exists but %CD%\%DEST%\%%a does not
        exit /b 1
    )
)
@goto :eof
