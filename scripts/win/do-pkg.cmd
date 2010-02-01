@if "%_echo%" == "" @echo off

setlocal

call %~dp0sysinfo || exit /b 1
call %~dp0pkginfo || exit /b 1
call %~dp0pkgcmds || exit /b 1

call %~dp0pkgcmds extract_options %* || (
	echo error : pkgcmds extract_options failed
	exit /b 1
)

if not defined ACTION (
    set IGNORE_MISS=yes
    call %~dp0pkgcmds map_action %* || (
	    echo error : pkgcmds map_action failed
	    exit /b 1
    )
) else (
    @rem do-pkg is pretty useful, echo this
    @rem to educate people of its existance
    @echo %~n0 %*
)

call %~dp0pkgcmds add_action_opts %* || (
	echo error : pkgcmds add_action_opts failed
	exit /b 1
)

call %~dp0pkgcmds get_args %* || (
	echo error : pkgcmds get_args failed
	exit /b 1
)

call %~dp0pkgmap %OPTIONS% %ADDARGS% -c "%ACTION%" %ARGS% || exit /b 1

@echo %~n0 : Success.
