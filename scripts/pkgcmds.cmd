@REM #!cmd.exe
@REM $Id$

@if "%_echo%" == "" @echo off

if not "%1" == "" (shift & goto :%1)

call %~dp0sysinfo.cmd

@REM
@REM To be done -- support DEC SRC Modula-3 and PM3 like the .sh version does.
@REM
@REM  define build and ship programs for Critical Mass Modula-3
set BUILDLOCAL=%CM3% -build -override -DROOT=%CM3ROOT% %BUILDARGS%
set CLEANLOCAL=%CM3% -clean -override -DROOT=%CM3ROOT% %CLEANARGS%
set BUILDGLOBAL=%CM3% -build -DROOT=%CM3ROOT% %BUILDARGS%
set CLEANGLOBAL=%CM3% -clean -DROOT=%CM3ROOT% %CLEANARGS%
set SHIP=%CM3% -ship -DROOT=%CM3ROOT% %SHIPARGS%

@REM other commands
set REALCLEAN=if exist %TARGET% rmdir /q/s %TARGET%

call :trim_trailing_space BUILDLOCAL CLEANLOCAL BUILDGLOBAL CLEANGLOBAL SHIP

goto :eof

:get_args
setlocal
set skip=true
:get_args_1
if "%1" == "" (
  set skip=false
)
set a=%1
if "%a:~0,1%" == "-" (
  shift
) else (
  set skip=false
)
if "%skip%" == "true" (
  goto :get_args_1
)
if "%1" == "build" (
  shift
) else if "%1" == "buildlocal" (
  shift
) else if "%1" == "buildglobal" (
  shift
) else if "%1" == "buildship" (
  shift
) else if "%1" == "ship" (
  shift
) else if "%1" == "clean" (
  shift
) else if "%1" == "cleanlocal" (
  shift
) else if "%1" == "cleanglobal" (
  shift
) else if "%1" == "realclean" (
  shift
)
set ARGS=
:get_args_2
if "%1" == "" (
  goto :get_args_end
)
set a=%1
if "%a:~0,1%" == "-" (
  echo encountered option after command: %1
) else (
  set ARGS=%ARGS% %1
)
shift
goto :get_args_2
:get_args_end
endlocal & set ARGS=%ARGS%
if not "%ARGS%" == "" (
  echo %ARGS%
)
goto :eof

:add_action_opts
setlocal
set skip=true
:add_action_opts_1
if "%1" == "" (
  set skip=false
)
set a=%1
if "%a:~0,1%" == "-" (
  shift
) else (
  set skip=false
)
if "%skip%" == "true" (
  goto :add_action_opts_1
)
endlocal
set ADDARGS=
if "%1" == "clean" (
  set ADDARGS=-k
) else if "%1" == "cleanlocal" (
  set ADDARGS=-k
) else if "%1" == "cleanglobal" (
  set ADDARGS=-k
) else if "%1" == "realclean" (
  set ADDARGS=-k
)
if not "%ADDARGS%" == "" (
  echo %ADDARGS%
)
goto :eof

:extract_options
setlocal
set OPTIONS=
:extract_options_1
if "%1" == "" (
  endlocal
  set OPTIONS=%OPTIONS%
  goto :eof
)
set a=%1
if "%a:~0,1%" == "-" (
  set OPTIONS=%OPTIONS% %a%
)
shift
goto :extract_options_1

:map_action
setlocal
set ACTION=
set skip=true
:map_action_1
if "%1" == "" (
  set skip=false
)
set a=%1
if "%a:~0,1%" == "-" (
  shift
) else (
  set skip=false
)
if "%skip%" == "true" goto :map_action_1
if "%1" == "" (
  set ACTION=%BUILDLOCAL%
) else if "%1" == "build" (
  set ACTION=%BUILDLOCAL%
) else if "%1" == "buildlocal" (
  set ACTION=%BUILDLOCAL%
) else if "%1" == "buildglobal" (
  set ACTION=%BUIDLGLOBAL% && %SHIP%
) else if "%1" == "buildship" (
  set ACTION=%BUIDLGLOBAL% && %SHIP%
) else if "%1" == "ship" (
  set ACTION=%SHIP%
) else if "%1" == "clean" (
  set ACTION=%CLEANLOCAL%
) else if "%1" == "cleanlocal" (
  set ACTION=%CLEANLOCAL%
) else if "%1" == "cleanglobal" (
  set ACTION=%CLEANGLOBAL%
) else if "%1" == "realclean" (
  set ACTION=%REALCLEAN%
) else if "%IGNORE_MISS%" == "yes" (
  set ACTION=%BUILDLOCAL%
) else (
  echo error: unknown action %1
)
endlocal & set ACTION=%ACTION%
if "%ACTION%" == "" (
  error 2>nul
)
goto :eof

:trim_trailing_space_1
setlocal
for /f "tokens=*" %%i in ('echo %%%1%%') do set a=%%i
if "%a:~-1%" == " " set a=%a:~0,-1%
endlocal & set %1=%a%
goto :eof

:trim_trailing_space
if "%1" == "" goto :eof
call  :trim_trailing_space_1 %1
shift
goto  :trim_trailing_space
