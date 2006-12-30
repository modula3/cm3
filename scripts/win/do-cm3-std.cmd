@rem $Id: do-cm3-std.cmd,v 1.2 2006-12-30 11:36:38 jkrell Exp $

@if "%_echo%" == "" @echo off

@setlocal

@call %~dp0clearenv || exit /b 1
@call %~dp0sysinfo || exit /b 1
@call %~dp0pkginfo || exit /b 1
@call %~dp0pkgcmds || exit /b 1

call %~dp0def-std-pkgs || (
	echo error : def-std-pkgs failed
	exit /b 1
)

REM
REM UNDONE
REM
REM type m3bundle >/dev/null 2>/dev/null || \
REM   . "$ROOT/scripts/do-pkg.sh" buildship m3bundle

call %~dp0pkgcmds extract_options %* || (
	echo error : pkgcmds extract_options failed
	exit /b 1
)
call %~dp0pkgcmds map_action %* || (
	echo error : pkgcmds map_action failed
	exit /b 1
)
call %~dp0pkgcmds add_action_opts %* || (
	echo error : pkgcmds add_action_opts failed
	exit /b 1
)

call :Run call %~dp0pkgmap %OPTIONS% %ADDARGS% -c "%ACTION%" %P%

endlocal
goto :eof

:Run
setlocal
set x=%*
set x=%x:  = %
set x=%x:  = %
echo %x%
%x% || (
	echo error : %x% failed
	exit /b 1
)
endlocal
goto :eof
