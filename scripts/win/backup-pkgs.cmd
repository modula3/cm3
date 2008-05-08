@rem $Id: backup-pkgs.cmd,v 1.6 2008-05-08 11:36:42 jkrell Exp $

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

rem m3cc

@set p_runtime=^
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
            call :check_complete_backup %0 || exit /b 1
            @echo %~n0 : %CD%\pkg-%BACKUPID% already exists, and appears complete, ok
            goto :handle_compiler
        )
    ) else (
      mkdir pkg-%BACKUPID%
    )
) else (
    set SRC=pkg-%BACKUPID%
    set DEST=pkg
)

for %%a in (%P%) do (
    if not exist %CD%\%SRC%\%%a call :nonexistant %0 %CD%\%SRC%\%%a || exit /b 1
    if exist %CD%\%SRC%\%%a call :existant %0 %CD%\%SRC%\%%a || exit /b 1
)

@rem
@rem Packages, at least some of them, go with the compiler that built them.
@rem In particular, if you were to run upgrade twice in a row, the 5.5.0 compiler
@rem chokes on the 5.2.6 m3core/libm3. Therefore backup/restore the compiler too.
@rem It would be nice if BACKUPID was derived from cm3 -version, however that
@rem doesn't allow for a new compiler to find old packages, so there is just
@rem one manually managed BACKUPID.
@rem

:handle_compiler

if not defined RESTORE (
    call :CopyFile bin\cm3.exe %DEST%\cm3.exe
) else (
    call :CopyFile %SRC%\cm3.exe bin\cm3.exe
)

@endlocal

@goto :eof

:existant
    @echo %CD%\%SRC%\%~nx2 --^> %CD%\%DEST%\%~nx2
    (xcopy /iveryc %CD%\%SRC%\%~nx2 %CD%\%DEST%\%~nx2 | findstr copied) || exit /b 1
    exit /b 0

:nonexistant
    if %~nx2 == libm3 (
        call :nonexistant_fatal %* || exit /b 1
    ) else if %~nx2 == m3core (
        call :nonexistant_fatal %* || exit /b 1
    ) else (
        call :nonexistant_warn %* || exit /b 1
    )
    exit /b 0

:nonexistant_warn
    @rem @echo %~n1 : %2 does not exist, will not backup/restore, ok
    exit /b 0

:nonexistant_fatal
    @echo %~n1 : %2 does not exist.
    @echo %~n1 : Be sure to build it with latest released tools
    @echo %~n1 : before trying to upgrade the compiler and build it with itself.
    @echo %~n1 : That is, run "do-pkg %~nx2 -buildship" before running upgrade.
    @echo %~n1 : OR get it from a binary release (as some older compilers (5.2.6)
    @echo %~n1 : cannot build newer runtimes (5.5.0 -- int64 support))
    exit /b 1

:check_nonexistant
    if %~nx2 == libm3 (
        call :check_nonexistant_fatal %* || exit /b 1
    ) else if %~nx2 == m3core (
        call :check_nonexistant_fatal %* || exit /b 1
    ) else (
        exit /b 0
    )

:check_nonexistant_warn
    @rem @echo %~n1 : %2 does not exist, will not restore, ok
    exit /b 0

:check_nonexistant_fatal
    @echo %~n1 : %CD%\%DEST% already exists but %1 does not.
    @echo %~n1 : Whatever you are doing, try backing up a few steps, sorry..
    @echo %~n1 : Perhaps rmdir /q/s %CD%\%DEST%.
    exit /b 1

:check_complete_backup
for %%a in (%P%) do (
    if not exist %CD%\%DEST%\%%a call :check_nonexistant %0 %CD%\%DEST%\%%a || exit /b 1
)
@goto :eof

:CopyFile
    setlocal
    set from=%1
    set to=%2
    if "%to%" == "." set to=.\%~nx1
    if exist %to% del %to% || exit /b 1
    call :Run copy %from% %to% || exit /b 1
    endlocal
    goto :eof

:Run
    setlocal
    set x=%*
    set x=%x:  = %
    set x=%x:  = %
    echo %x%
    %x% || goto :RunError
    endlocal
    goto :eof

:RunError
	echo ERROR: %x%
	exit /b 1
