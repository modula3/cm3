@rem $Id: do-cm3-base.cmd,v 1.7 2007-12-31 10:09:31 jkrell Exp $

@if "%_echo%" == "" @echo off

@setlocal

call %~dp0clearenv || exit /b 1
call %~dp0sysinfo || exit /b 1
call %~dp0pkgcmds || exit /b 1

call %~dp0pkgcmds map_action %* || exit /b 1

set P=
set P=%P% import-libs
set P=%P% m3core
set P=%P% libm3
set P=%P% m3middle
set P=%P% m3quake
set P=%P% m3scanner
set P=%P% m3tools
set P=%P% m3cgcat
set P=%P% m3cggen
set P=%P% m3bundle
set P=%P% mklib
set P=%P% fix_nl
set P=%P% libdump
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
set P=%P% tapi
if "%HAVE_SERIAL%" == "yes" set P=%P% serial

call %~dp0do-pkg %* %P% || exit /b 1

@echo %~n0 : Success.
