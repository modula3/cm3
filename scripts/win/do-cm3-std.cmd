@rem $Id: do-cm3-std.cmd,v 1.4 2007-12-31 10:09:31 jkrell Exp $

@if "%_echo%" == "" @echo off

setlocal

call %~dp0clearenv || exit /b 1
call %~dp0sysinfo || exit /b 1
call %~dp0pkgcmds || exit /b 1

call %~dp0pkgcmds map_action %* || exit /b 1

call %~dp0def-std-pkgs || (
	echo error : def-std-pkgs failed
	exit /b 1
)

REM
REM UNDONE
REM
REM type m3bundle >/dev/null 2>/dev/null || \
REM   . "$ROOT/scripts/do-pkg.sh" buildship m3bundle

call %~dp0do-pkg %* %P% || exit /b 1

@echo %~n0 : Success.
