@rem $Id: do-cm3-min.cmd,v 1.6 2007-12-31 10:09:31 jkrell Exp $

@echo off
setlocal
call %~dp0clearenv || exit /b 1
call %~dp0sysinfo || exit /b 1
call %~dp0pkgcmds || exit /b 1

call %~dp0pkgcmds map_action %* || exit /b 1

call %~dp0do-pkg %* import-libs m3core libm3 || exit /b 1

@echo %~n0 : Success.
