@rem $Id$

@if "%_echo%" == "" @echo off

setlocal

call %~dp0clearenv || exit /b 1
call %~dp0sysinfo || exit /b 1
call %~dp0pkgcmds || exit /b 1

call %~dp0pkgcmds map_action %* || exit /b 1

set P=^
 import-libs ^
 m3core ^
 libm3 ^
 sysutils ^
 m3middle ^
 m3objfile ^
 m3linker ^
 m3back ^
 m3staloneback ^
 m3front ^
 m3quake ^
 cm3 ^
 patternmatching ^
 m3scanner ^
 m3tools ^
 m3cgcat ^
 m3cggen ^
 m3bundle ^
 mklib ^
 fix_nl ^
 libdump ^
 bitvector ^
 digraph ^
 parseparams ^
 realgeometry ^
 set ^
 slisp ^
 sortedtableextras ^
 table-list ^
 tempfiles

if "%HAVE_TCL%" == "yes" set P_TCL=%P% tcl

call %~dp0do-pkg %* %P% || exit /b 1

@echo %~n0 : Success.
