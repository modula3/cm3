@rem $Id: pkgcmds.cmd,v 1.2 2006-12-30 11:36:38 jkrell Exp $

@if "%_echo%" == "" @echo off

if not "%1" == "" (shift & goto :%1)

call %~dp0sysinfo || exit /b 1


@rem
@rem define build and ship programs for Critical Mass Modula-3
@rem

if not defined CM3_BUILDLOCAL (
    if not defined BUILDLOCAL (
        set CM3_BUILDLOCAL=%CM3% -build -override -DROOT=%CM3ROOT% %BUILDARGS%
    ) else (
        set CM3_BUILDLOCAL=%BUILDLOCAL%
    )
)
if not defined CM3_CLEANLOCAL (
    if not defined CLEANLOCAL (
        set CM3_CLEANLOCAL=%CM3% -clean -override -DROOT=%CM3ROOT% %CLEANARGS%
    ) else (
        set CM3_CLEANLOCAL=%CLEANLOCAL%
    )
)
if not defined CM3_BUILDGLOBAL (
    if not defined BUILDGLOBAL (
        set CM3_BUILDGLOBAL=%CM3% -build  -DROOT=%CM3ROOT% %BUILDARGS%
    ) else (
        set CM3_BUILDGLOBAL=%BUILDGLOBAL%
    )
)
if not defined CM3_CLEANGLOBAL (
    if not defined CLEANGLOBAL (
        set CM3_CLEANGLOBAL=%CM3% -clean -DROOT=%CM3ROOT% %CLEANARGS%
    ) else (
        set CM3_CLEANGLOBAL=%CLEANGLOBAL%
    )
)
if not defined CM3_SHIP (
    if not defined SHIP (
        set CM3_SHIP=%CM3% -ship -DROOT=%CM3ROOT% %SHIPARGS%
    ) else (
        set CM3_SHIP=%SHIP%
    )
)

@rem
@rem define build and ship programs for Poly. Modula-3 from Montreal
@rem

if not defined PM3_BUILDLOCAL (
    if not defined BUILDLOCAL (
        set PM3_BUILDLOCAL=%M3BUILD% -O -DROOT=%CM3ROOT% %BUILDARGS%
    ) else (
        set PM3_BUILDLOCAL=%BUILDLOCAL%
    )
)
if not defined PM3_CLEANLOCAL (
    if not defined CLEANLOCAL (
        set PM3_CLEANLOCAL=%M3BUILD% clean -O -DROOT=%CM3ROOT% %CLEANARGS%
    ) else (
        set PM3_CLEANLOCAL=%CLEANLOCAL%
    )
)
if not defined PM3_BUILDGLOBAL (
    if not defined BUILDGLOBAL (
        set PM3_BUILDGLOBAL=%M3BUILD% -DROOT=%CM3ROOT% %BUILDARGS%
    ) else (
        set PM3_BUILDGLOBAL=%BUILDGLOBAL%
    )
)
if not defined PM3_CLEANGLOBAL (
    if not defined CLEANGLOBAL (
        set PM3_CLEANGLOBAL=%M3BUILD% clean -DROOT=%CM3ROOT% %CLEANARGS%
    ) else (
        set PM3_CLEANGLOBAL=%CLEANGLOBAL%
    )
)
if not defined PM3_SHIP (
    if not defined SHIP (
        set PM3_SHIP=%M3SHIP% -DROOT=%CM3ROOT% %SHIPARGS%
    ) else (
        set PM3_SHIP=%SHIP%
    )
)

@rem
@rem define build and ship programs for DEC SRC Modula-3
@rem

if not defined SRC_BUILDLOCAL (
    if not defined BUILDLOCAL (
        set SRC_BUILDLOCAL=%M3BUILD% -O -DROOT=%CM3ROOT% %BUILDARGS%
    ) else (
        set SRC_BUILDLOCAL=%BUILDLOCAL%
    )
)
if not defined SRC_CLEANLOCAL (
    if not defined CLEANLOCAL (
        set SRC_CLEANLOCAL=%M3BUILD% clean -O -DROOT=%CM3ROOT% %CLEANARGS%
    ) else (
        set SRC_CLEANLOCAL=%CLEANLOCAL%
    )
)
if not defined SRC_BUILDGLOBAL (
    if not defined BUILDGLOBAL (
        set SRC_BUILDGLOBAL=%M3BUILD% -DROOT=%CM3ROOT% %BUILDARGS%
    ) else (
        set SRC_BUILDGLOBAL=%BUILDGLOBAL%
    )
)
if not defined SRC_CLEANGLOBAL (
    if not defined CLEANGLOBAL (
        set SRC_CLEANGLOBAL=%M3BUILD% clean -DROOT=%CM3ROOT% %CLEANARGS%
    ) else (
        set SRC_CLEANGLOBAL=%CLEANGLOBAL%
    )
)
if not defined SRC_SHIP (
    if not defined SHIP (
        set SRC_SHIP=%M3SHIP% -DROOT=%CM3ROOT% %SHIPARGS%
    ) else (
        set SRC_SHIP=%SHIP%
    )
)

@rem
@rem other commands
@rem
set REALCLEAN=if exist %TARGET% rmdir /q/s %TARGET%
set CLEANLINK=if exist %TARGET% del %TARGET%\*.exe %TARGET%\*.exe.manifest %TARGET%\*.dll %TARGET%\*.dll.manifest %TARGET%\*.pdb

@rem
@rem choose the compiler to use
@rem
if defined USE_SRC (
    set BUILDLOCAL=%SRC_BUILDLOCAL%
    set CLEANLOCAL=%SRC_CLEANLOCAL%
    set BUILDGLOBAL=%SRC_BUILDGLOBAL%
    set CLEANGLOBAL=%SRC_CLEANGLOBAL%
    set SHIP=%SRC_SHIP%
) else if defined USE_PM3 (
    set BUILDLOCAL=%PM3_BUILDLOCAL%
    set CLEANLOCAL=%PM3_CLEANLOCAL%
    set BUILDGLOBAL=%PM3_BUILDGLOBAL%
    set CLEANGLOBAL=%PM3_CLEANGLOBAL%
    set SHIP=%PM3_SHIP%
) else (
    set BUILDLOCAL=%CM3_BUILDLOCAL%
    set CLEANLOCAL=%CM3_CLEANLOCAL%
    set BUILDGLOBAL=%CM3_BUILDGLOBAL%
    set CLEANGLOBAL=%CM3_CLEANGLOBAL%
    set SHIP=%CM3_SHIP%
)

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
