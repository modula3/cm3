@if "%_echo%" == "" @echo off

setlocal

@rem Start with the installed cm3.
@if defined CM3 set CM3=

call %~dp0clearenv || call :ReportFatalError || exit /b 1
call %~dp0sysinfo || call :ReportFatalError || exit /b 1

if not defined STAGE (
    set STAGE=%TEMP%\cm3\%~n0\%random%
)
if /i "%1" == "" (
    if exist %STAGE% (
        echo ERROR: %%STAGE%% ^(%STAGE%^) must not exist ahead of time, in order that we may be safely destructive to it.
        exit /b 1
    )
)
@mkdir %STAGE%\logs 2>nul
@set LogCounter=0
@call :IncrementLogCounter_init
@goto :IncrementLogCounter_end

:IncrementLogCounter
@rem
@rem Setlocal interferes with incrementing this, so we are..creative..
@rem When your programming language has insufficient scoping rules,
@rem ferry globals around in the file system. Do not confuse an add
@rem instruction with opening/reading/writing a file.
@rem Oops.
@rem
@call %STAGE%\logs\LogCounter.bat
:IncrementLogCounter_init
@echo set /a LogCounter=%LogCounter% + 1 > %STAGE%\logs\LogCounter.bat
@goto :eof
:IncrementLogCounter_end

pushd %STAGE%

set INSTALLROOT_COMPILER_WITH_PREVIOUS=compiler_with_previous
set INSTALLROOT_COMPILER_WITH_SELF=compiler_with_self
set INSTALLROOT_MIN=cm3-min-%M3OSTYPE%-%TARGET%-%CM3VERSION%
set INSTALLROOT_STD=cm3-std-%M3OSTYPE%-%TARGET%-%CM3VERSION%

@rem for incremental runs to recover at this step..
if /i "%1" == "goto_min" shift & goto :min
if /i "%1" == "goto_zip" shift & goto :Zip

call %~dp0do-cm3-all realclean || call :ReportFatalError || exit /b 1

setlocal

@rem -------------------------------------------------------------------------
@rem Build new compiler with previous compiler, though
@rem previous compiler must be relatively up to date (else run upgrade.cmd).
@rem -------------------------------------------------------------------------

call :Setup %STAGE%\%INSTALLROOT_COMPILER_WITH_PREVIOUS% || call :ReportFatalError || exit /b 1
call :Run %~dp0do-cm3-all realclean || call :ReportFatalError || exit /b 1
call :Run %~dp0do-cm3-front buildship %P% || call :ReportFatalError || exit /b 1
call :ShipCompiler || call :ReportFatalError || exit /b 1
call :Run %~dp0do-cm3-all realclean || call :ReportFatalError || exit /b 1

endlocal && set INSTALLROOT=%INSTALLROOT%

@rem -------------------------------------------------------------------------
@rem Rebuild new compiler with itself.
@rem -------------------------------------------------------------------------

setlocal

call :Setup %STAGE%\%INSTALLROOT_COMPILER_WITH_SELF% || call :ReportFatalError || exit /b 1
call :Run %~dp0do-cm3-all realclean || call :ReportFatalError || exit /b 1
call :Run %~dp0do-cm3-front buildship %P% || call :ReportFatalError || exit /b 1
call :ShipCompiler || call :ReportFatalError || exit /b 1
call :Run %~dp0do-cm3-all realclean || call :ReportFatalError || exit /b 1

endlocal && set INSTALLROOT=%INSTALLROOT%

@rem ----------------------------------------------------------------------------------------------------------------------------------
@rem build minimal packages with new compiler
@rem ----------------------------------------------------------------------------------------------------------------------------------

setlocal
call :Setup %STAGE%\%INSTALLROOT_MIN% || call :ReportFatalError || exit /b 1
call :Run %~dp0do-cm3-all realclean || call :ReportFatalError || exit /b 1
call :Run %~dp0do-cm3-min buildship || call :ReportFatalError || exit /b 1
endlocal && set INSTALLROOT=%INSTALLROOT%

@rem
@rem Don't clean here, std will merely reship min rather than rebuild it.
@rem

@rem
@rem workaround ship not really working -- i.e. hand.obj missing
@rem

xcopy /fiverdyh %STAGE%\%INSTALLROOT_MIN% %STAGE%\%INSTALLROOT_STD%

@rem ----------------------------------------------------------------------------------------------------------------------------------
@rem build standard packages with new compiler
@rem ----------------------------------------------------------------------------------------------------------------------------------

setlocal

call :Setup %STAGE%\%INSTALLROOT_STD% || call :ReportFatalError || exit /b 1
call :Run %~dp0do-cm3-std buildship || call :ReportFatalError || exit /b 1

endlocal

@rem ----------------------------------------------------------------------------------------------------------------------------------

:Zip

mkdir symbols
move /y %INSTALLROOT_MIN%\bin\*.pdb symbols
move /y %INSTALLROOT_STD%\bin\*.pdb symbols

del *.bz2 *.zip *.exe *.tar

rem for source debugging?
del /s *.m3
del /s *.c
del /s *.h
del /s .m3web

rem
rem Static .libs are very bloated. Delete most of them.
rem Keep m3core/libm3/sysutils for purposes of bootstrapping
rem later compilers -- in case the runtime changes such as
rem to not be buildable with an older compiler, such as what
rem happened when LONGINT was introduced.
rem
for /f %%a in ('dir /s/b/a-d *.lib.sa') do (
    if /i not "%%~na" == "m3core.lib" if /i not "%%~na" == "libm3.lib" if /i not "%%~na" == "sysutils.lib" del %%a
)

call :Run zip -9 -r -D -X cm3-%TARGET%-%CM3VERSION%-symbols.zip symbols
call :Run zip -9 -r -D -X %INSTALLROOT_MIN%.zip %INSTALLROOT_MIN%
call :Run zip -9 -r -D -X %INSTALLROOT_STD%.zip %INSTALLROOT_STD%

@rem
@rem Building a self extracting .exe is very easy but not present for now.
@rem It is available in history if desired.
@rem I think it'd be more valuable if it was a gui. tbd?
@rem

popd

echo Success.
echo Output is %STAGE%\*.zip
echo Lots of intermediate state remains in %STAGE%.

goto :eof

:Setup
    @rem
    @rem copy compiler, mklib, and make config files
    @rem
    set INSTALLROOT_PREVIOUS=%INSTALLROOT%
    set INSTALLROOT=%1
    set CM3_INSTALL=%1
    mkdir %INSTALLROOT%\bin
    mkdir %INSTALLROOT%\lib
    mkdir %INSTALLROOT%\pkg
    copy %ROOT%\COPYRIGHT* %INSTALLROOT%
    set LIB=%INSTALLROOT%\lib;%LIB%
    set PATH=%INSTALLROOT%\bin;%PATH%
    call :CopyCompiler %INSTALLROOT_PREVIOUS% %INSTALLROOT% || call :ReportFatalError || exit /b 1
    goto :eof

:Run
    @call :IncrementLogCounter
    @echo.
    setlocal
    @rem remove some extraneous spaces that come
    @rem concating possibly empty variables with
    @rem spaces between them
    set x=%*
    set x=%x:  = %
    set x=%x:  = %
    echo %TIME%>>%STAGE%\logs\%LogCounter%_%~n1.log
    echo.>> %STAGE%\logs\all.log
    echo.>> %STAGE%\logs\%LogCounter%_%~n1.log
    echo %x% >> %STAGE%\logs\%LogCounter%_%~n1.log
    echo %x% ^>^> %STAGE%\logs\%LogCounter%_%~n1.log
    call %x% >> %STAGE%\logs\%LogCounter%_%~n1.log 2>&1 || (
        echo %TIME%>>%STAGE%\logs\%LogCounter%_%~n1.log
        type %STAGE%\logs\%LogCounter%_%~n1.log >> %STAGE%\logs\all.log
	    echo ERROR: %x% failed
	    exit /b 1
    )
    echo %TIME%>>%STAGE%\logs\%LogCounter%_%~n1.log
    type %STAGE%\logs\%LogCounter%_%~n1.log >> %STAGE%\logs\all.log
    endlocal
    goto :eof

:ShipCompiler
    @rem
    @rem The compiler has trouble shipping itself currently because it in use.
    @rem
    call :CreateDirectory %INSTALLROOT%\bin
    call :CopyFile        %ROOT%\m3-sys\cm3\%TARGET%\cm3.exe %INSTALLROOT%\bin\cm3.exe || call :ReportFatalError || exit /b 1
    call :CopyFileIfExist %ROOT%\m3-sys\cm3\%TARGET%\cm3.pdb %INSTALLROOT%\bin\cm3.pdb || call :ReportFatalError || exit /b 1
    copy /y %ROOT%\m3-sys\cminstall\src\config\%TARGET%* %2\bin || call :ReportFatalError || exit /b 1
    del %2\bin\%TARGET%.main
    echo include("%TARGET%")>%2\bin\cm3.cfg
    goto :eof

:CopyCompiler
    @rem
    @rem Copy the compiler from one INSTALLROOT to another, possibly having cleaned out the intermediate directories.
    @rem
    call :CreateDirectory %2\bin

    @rem
    @rem front end
    @rem
    call :CopyFile        %1\bin\cm3.exe   %2\bin\cm3.exe || call :ReportFatalError || exit /b 1
    call :CopyFileIfExist %1\bin\cm3.pdb   %2\bin\cm3.pdb || call :ReportFatalError || exit /b 1

    @rem
    @rem gcc back end
    @rem
    call :CopyFileIfExist %1\bin\cm3cg.exe %2\bin\cm3cg.exe || call :ReportFatalError || exit /b 1
    call :CopyFileIfExist %1\bin\cm3cg.pdb %2\bin\cm3cg.pdb || call :ReportFatalError || exit /b 1

    @rem
    @rem config files -- always from the source tree
    @rem
    copy /y %ROOT%\m3-sys\cminstall\src\config\%TARGET%* %2\bin || call :ReportFatalError || exit /b 1
    del %2\bin\%TARGET%.main
    echo include("%TARGET%")>%2\bin\cm3.cfg

    @rem
    @rem mklib
    @rem
    call :CopyFile        %1\bin\mklib.exe %2\bin\mklib.exe || call :ReportFatalError || exit /b 1
    call :CopyFileIfExist %1\bin\mklib.pdb %2\bin\mklib.pdb || call :ReportFatalError || exit /b 1
    goto :eof

:CopyFile
    setlocal
    set from=%1
    set to=%2
    if "%to%" == "." set to=.\%~nx1
    if exist %to% del %to% || call :ReportFatalError || exit /b 1
    call :Run copy %from% %to% || call :ReportFatalError || exit /b 1
    endlocal
    goto :eof

:CopyFileIfExist
    if exist %2 del %2 || call :ReportFatalError || exit /b 1
    if exist %1 call :Run copy %1 %2 || call :ReportFatalError || exit /b 1
    goto :eof

:CreateDirectory
    if not exist %1 mkdir %1
    goto :eof

:ReportFatalError
    setlocal
    set t=%time%
    set t=%t::=%
    set t=%t:.=%
    if exist %TEMP%\env.bat del %TEMP%\cm3\env%t%.bat
    for /f "tokens=*" %%a in ('set') do echo set %%a>>%TEMP%\cm3\env%t%.bat
    @rem This turns out not to be useful, because
    @rem we have already returned to the caller's directory.
    @rem echo ERROR: Current working directory was %CD%
    echo ERROR: Environment saved in %TEMP%\cm3\env%t%.bat
    echo ERROR: See %STAGE%\logs
    exit /b 1
