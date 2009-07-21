@rem $Id: pkgcmds.cmd,v 1.5 2009-07-21 08:17:36 jkrell Exp $

@if "%_echo%" == "" @echo off

if not "%1" == "" (shift & goto :%1)

call %~dp0sysinfo || exit /b 1


@rem
@rem define build and ship programs for Critical Mass Modula-3
@rem

set DEFS=-DROOT=%CM3ROOT% -DCM3_VERSION_TEXT=%CM3VERSION% -DCM3_VERSION_NUMBER=%CM3VERSIONNUM% -DCM3_LAST_CHANGED=%CM3LASTCHANGED%

set BUILDLOCAL=%CM3% -build -override %DEFS% %BUILDARGS%
set CLEANLOCAL=%CM3% -clean -override %DEFS% %CLEANARGS%
set BUILDGLOBAL=%CM3% -build  %DEFS% %BUILDARGS%
set CLEANGLOBAL=%CM3% -clean %DEFS% %CLEANARGS%
set SHIP=%CM3% -ship %DEFS% %SHIPARGS%

@rem
@rem other commands
@rem

set REALCLEAN=if exist %TARGET% rmdir /q/s %TARGET%
set CLEANLINK=if exist %TARGET% del %TARGET%\*.exe %TARGET%\*.exe.manifest %TARGET%\*.dll %TARGET%\*.dll.manifest %TARGET%\*.pdb

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
) else if "%1" == "cleanlink" (
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
@rem
@rem -k means keep going after errors
@rem
if defined ADDARGS set ADDARGS=
if "%1" == "clean" (
  set ADDARGS=-k
) else if "%1" == "cleanlocal" (
  set ADDARGS=-k
) else if "%1" == "cleanglobal" (
  set ADDARGS=-k
) else if "%1" == "realclean" (
  set ADDARGS=-k
) else if "%1" == "cleanlink" (
  set ADDARGS=-k
)
if not "%ADDARGS%" == "" (
  rem echo %ADDARGS%
)
goto :eof

:extract_options
setlocal
if defined OPTIONS set OPTIONS=
:extract_options_1
if "%1" == "" (
  endlocal
  if defined OPTIONS set OPTIONS=%OPTIONS%
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
    set ACTION=%BUILDGLOBAL% ^&^& %SHIP%
) else if "%1" == "buildship" (
    set ACTION=%BUILDGLOBAL% ^&^& %SHIP%
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
) else if "%1" == "cleanlink" (
    set ACTION=%CLEANLINK%
) else if "%IGNORE_MISS%" == "yes" (
    set ACTION=%BUILDLOCAL%
) else (
    echo error: unknown action %1
)
if defined ACTION (
    endlocal & set ACTION=%ACTION:&=^&%
) else (
    endlocal & set ACTION=
)
if "%ACTION%" == "" (
    echo error : pkgcmds failed
    exit /b 1
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
