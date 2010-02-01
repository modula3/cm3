@if "%_echo%" == "" @echo off

SetLocal EnableExtensions EnableDelayedExpansion

call %~dp0clearenv || exit /b 1
call %~dp0sysinfo || exit /b 1
call %~dp0pkgcmds || exit /b 1

call %~dp0pkgcmds map_action %* || exit /b 1

set P=
for /f "tokens=1" %%a in ('findstr /c:" std" %~dp0..\pkginfo.txt') do set P=!P! %%a

REM
REM UNDONE
REM
REM type m3bundle >/dev/null 2>/dev/null || \
REM   . "$ROOT/scripts/do-pkg.sh" buildship m3bundle

call %~dp0do-pkg %* %P% || exit /b 1

@echo %~n0 : Success.
