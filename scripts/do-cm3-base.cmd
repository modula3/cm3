@REM #!cmd.exe
@REM $Id: do-cm3-base.cmd,v 1.1 2004-04-27 15:00:23 sk Exp $

@if "%_echo%" == "" @echo off

call %~dp0clearenv.cmd
setlocal
call %~dp0sysinfo.cmd
call %~dp0pkginfo.cmd
call %~dp0pkgcmds.cmd

set P=
set P=%P% m3core
set P=%P% libm3
set P=%P% m3middle
set P=%P% m3quake
set P=%P% m3scanner
set P=%P% m3tools
set P=%P% m3cgcat
set P=%P% m3cggen
if "%M3GDB%" == "yes" set P=%P% m3gdb
set P=%P% m3bundle
if "%M3OSTYPE%" == "WIN32" set P=%P% mklib
if "%M3OSTYPE%" == "WIN32" set P=%P% dll2lib
if "%M3OSTYPE%" == "WIN32" set P=%P% fix_nl
if "%M3OSTYPE%" == "WIN32" set P=%P% libdump
set P=%P% bitvector
set P=%P% digraph
set P=%P% parseparams
set P=%P% realgeometry
set P=%P% set
set P=%P% slisp
set P=%P% sortedtableextras
set P=%P% table-list
set P=%P% tempfiles
if "%HAVE_TCL%" == "yes" set P=%P% tcl
set P=%P% tcp
if "%M3OSTYPE%" == "WIN32" set P=%P% tapi
if "%HAVE_SERIAL%" == "yes" set P=%P% serial

call %~dp0pkgcmds extract_options %*
call %~dp0pkgcmds map_action %* || goto :eof
call %~dp0pkgcmds add_action_opts %*

echo %~dp0pkgmap.cmd %OPTIONS% %ADDARGS% -c "%ACTION%" %P%
call %~dp0pkgmap.cmd %OPTIONS% %ADDARGS% -c "%ACTION%" %P%

endlocal
