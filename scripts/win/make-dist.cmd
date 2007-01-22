@rem $Id$

@if "%_echo%" == "" @echo off

setlocal

call %~dp0clearenv || exit /b 1
call %~dp0sysinfo || exit /b 1
call %~dp0pkginfo || exit /b 1
call %~dp0pkgcmds || exit /b 1

if not defined STAGE (
    set STAGE=%TEMP%\cm3\%~n0\%random%
)

set INSTALLROOT_PREVIOUS=%INSTALLROOT%

set INSTALLROOT_COMPILER_WITH_PREVIOUS=%STAGE%\compiler_with_previous
set INSTALLROOT_COMPILER_WITH_SELF=%STAGE%\compiler_with_self

set INSTALLROOT_MIN=%STAGE%\min
set INSTALLROOT_STD=%STAGE%\std
set INSTALLROOT_CORE=%STAGE%\core
set INSTALLROOT_BASE=%STAGE%\base

@rem for incremental runs to recover at this step..
if /i "%1" == "tar" goto :TarGzip
if /i "%1" == "min" goto :min

@rem ------------------------------------------------------------------------------------------------------------------------
call :Echo build new compiler with old compiler (%INSTALLROOT_PREVIOUS% to %INSTALLROOT_COMPILER_WITH_PREVIOUS%)
@rem ------------------------------------------------------------------------------------------------------------------------

set P=^
 import-libs ^
 m3core ^
 libm3 ^
 patternmatching ^
 m3bundle ^
 m3middle ^
 m3objfile ^
 m3linker ^
 m3back ^
 m3staloneback ^
 m3front ^
 m3quake ^
 cm3 ^
 cminstall ^
 mklib

setlocal

@rem
@rem cm3 is run out of %path%, but mklib is not, so we have to copy it..
@rem
call :CopyMklib %INSTALLROOT% %INSTALLROOT_COMPILER_WITH_PREVIOUS%
set INSTALLROOT=%INSTALLROOT_COMPILER_WITH_PREVIOUS%
call :RealClean || exit /b 1
call :BuildShip || exit /b 1
call :ShipCompiler || exit /b 1
call :RealClean || exit /b 1

endlocal

@rem ----------------------------------------------------------------------------------------------------------------------------------
call :Echo build compiler with itself (%INSTALLROOT_COMPILER_WITH_PREVIOUS% to %INSTALLROOT_COMPILER_WITH_SELF%)
@rem ----------------------------------------------------------------------------------------------------------------------------------

setlocal

set INSTALLROOT=%INSTALLROOT_COMPILER_WITH_SELF%
set PATH=%INSTALLROOT_COMPILER_WITH_PREVIOUS%\bin;%PATH%
call :RealClean || exit /b 1
call :BuildShip || exit /b 1
call :ShipCompiler || exit /b 1
rem don't clean -- keep mklib and cminstall
rem call :RealClean || exit /b 1
@rem
@rem save cminstall.exe away for later
@rem
call :CopyFile %ROOT%\m3-sys\cminstall\%TARGET%\cminstall.exe %STAGE%\cminstall.exe || exit /b 1
call :RealClean || exit /b 1

endlocal

set P=

@rem ----------------------------------------------------------------------------------------------------------------------------------
call :Echo build minimal packages with new compiler
@rem ----------------------------------------------------------------------------------------------------------------------------------

:min

setlocal

set INSTALLROOT=%INSTALLROOT_MIN%
call :CopyCompiler %INSTALLROOT_COMPILER_WITH_SELF% %INSTALLROOT% || exit /b 1
set PATH=%INSTALLROOT%\bin;%PATH%

call %~dp0do-cm3-min realclean || exit /b 1
call %~dp0do-cm3-min buildship || exit /b 1
call %~dp0do-cm3-min realclean || exit /b 1

endlocal

@rem ----------------------------------------------------------------------------------------------------------------------------------
call :Echo build core packages with new compiler
@rem ----------------------------------------------------------------------------------------------------------------------------------

setlocal

set INSTALLROOT=%INSTALLROOT_CORE%
call :CopyCompiler %INSTALLROOT_COMPILER_WITH_SELF% %INSTALLROOT% || exit /b 1
set PATH=%INSTALLROOT%\bin;%PATH%

call :Echo skipping..
rem call %~dp0do-cm3-core realclean || exit /b 1
rem call %~dp0do-cm3-core buildship || exit /b 1
rem call %~dp0do-cm3-core realclean || exit /b 1

endlocal

@rem ----------------------------------------------------------------------------------------------------------------------------------
call :Echo build standard packages with new compiler
@rem ----------------------------------------------------------------------------------------------------------------------------------

setlocal

set INSTALLROOT=%INSTALLROOT_STD%
call :CopyCompiler %INSTALLROOT_COMPILER_WITH_SELF% %INSTALLROOT% || exit /b 1
set PATH=%INSTALLROOT%\bin;%PATH%

call :Echo skipping..
rem call %~dp0do-cm3-std realclean || exit /b 1
rem call %~dp0do-cm3-std buildship || exit /b 1
rem call %~dp0do-cm3-std realclean || exit /b 1

endlocal


@rem ----------------------------------------------------------------------------------------------------------------------------------

echo INSTALLROOT_MIN=%INSTALLROOT_MIN%
rem echo INSTALLROOT_STD=%INSTALLROOT_STD%
rem echo INSTALLROOT_CORE=%INSTALLROOT_CORE%
rem echo INSTALLROOT_BASE=%INSTALLROOT_BASE%


echo now need to tar/gzip it up
rem we want tar.exe, gzip.exe, cygwin.dll, cminstall.exe, copyright-cmass, and system.tgz, tar/gziped
rem into cm3-min-win32-nt386-<version>.tgz
rem bzip2 would be preferable for a smaller size

:TarGzip

pushd %INSTALLROOT_MIN%
if not exist symbols mkdir symbols
for /f %%a in ('dir /s/b/a-d *.pdb') do move %%a symbols
if exist system.tgz del system.tgz
tar cvzf system.tgz bin lib pkg || exit /b 1
call :CopyFile %ROOT%\m3-sys\cminstall\%TARGET%\cminstall.exe . || exit /b 1
call :CopyFile %ROOT%\m3-sys\COPYRIGHT-CMASS . || exit /b 1
call :CopyFile %ROOT%\tools\win32\tar.exe . || exit /b 1
call :CopyFile %ROOT%\tools\win32\gzip.exe . || exit /b 1
call :CopyFile %ROOT%\tools\win32\cygwin.dll . || exit /b 1
if exist cm3-min-%M3OSTYPE%-%TARGET%-%CM3VERSION%.tgz del cm3-min-%M3OSTYPE%-%TARGET%-%CM3VERSION%.tgz
tar cvzf cm3-min-%M3OSTYPE%-%TARGET%-%CM3VERSION%.tgz cminstall.exe COPYRIGHT-CMASS system.tgz tar.exe gzip.exe cygwin.dll || exit /b 1
tar cvzf cm3-min-%M3OSTYPE%-%TARGET%-%CM3VERSION%-symbols.tgz symbols || exit /b 1
popd

echo DONE!
echo Output is %INSTALLROOT_MIN%\cm3-min-%M3OSTYPE%-%TARGET%-%CM3VERSION%.tgz
echo Output is %INSTALLROOT_MIN%\cm3-min-%M3OSTYPE%-%TARGET%-%CM3VERSION%-symbols.tgz
echo Lots of intermediate state remains in %STAGE%.

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

:ShipCompiler
    @rem
    @rem The compiler has trouble shipping itself currently because it in use.
    @rem This is easily dealt with on NT by moving it away to a unique location, MoveFileEx to delete (admin-only).
    @rem Do it manually for now.
    @rem
    call :CreateDirectory %INSTALLROOT%
    call :CopyFile       %ROOT%\m3-sys\cm3\%TARGET%\cm3.exe %INSTALLROOT%\bin\cm3.exe || exit /b 1
    call :CopyFile       %ROOT%\m3-sys\cminstall\src\config\%TARGET% %INSTALLROOT%\bin\cm3.cfg || exit /b 1
    call :CopyFile       %ROOT%\m3-sys\cm3\%TARGET%\cm3.pdb %INSTALLROOT%\bin\cm3.pdb || exit /b 1
    call :CopyFileIfExist %ROOT%\m3-sys\cm3\%TARGET%\cm3.exe.manifest %INSTALLROOT%\bin\cm3.exe.manifest || exit /b 1
    call :CopyFile       %ROOT%\m3-sys\cminstall\src\config\%TARGET% %INSTALLROOT%\bin\cm3.cfg || exit /b 1
    goto :eof

:CopyCompiler
    @rem
    @rem Copy the compiler from one INSTALLROOT to another, possibly having cleaned out the intermediate directories.
    @rem The config file always comes right out of the source tree.
    @rem
    call :CreateDirectory %2\bin
    call :CopyFile       %1\bin\cm3.exe %2\bin\cm3.exe || exit /b 1
    call :CopyFile       %1\bin\cm3.pdb %2\bin\cm3.pdb || exit /b 1
    call :CopyFileIfExist %1\bin\cm3.exe.manifest %2\bin\cm3.exe.manifest || exit /b 1
    call :CopyFile       %ROOT%\m3-sys\cminstall\src\config\%TARGET% %2\bin\cm3.cfg || exit /b 1
    call :CopyMkLib %1 %2 || exit /b 1
    goto :eof

:CopyMklib
    @rem
    @rem Copy mklib from one INSTALLROOT to another, possibly having cleaned out the intermediate directories.
    @rem
    call :CreateDirectory %2\bin
    call :CopyFile        %1\bin\mklib.exe %2\bin\mklib.exe || exit /b 1
    call :CopyFile        %1\bin\mklib.pdb %2\bin\mklib.pdb || exit /b 1
    call :CopyFileIfExist %1\bin\mklib.exe.manifest %2\bin\mklib.exe.manifest || exit /b 1
    goto :eof

:CopyFile
    setlocal
    set from=%1
    set to=%2
    if "%to%" == "." set to=.\%~nx1
    if exist %to% del %to% || exit /b 1
    copy %from% %to% || exit /b 1
    endlocal
    goto :eof

:CopyFileIfExist
    if exist %2 del %2 || exit /b 1
    if exist %1 copy %1 %2 || exit /b 1
    goto :eof

:Echo
    @echo.
	@echo =============================================================================
	@echo %*
	@echo =============================================================================
    @echo.
	goto :eof

:BuildShip
    call :CreateSkel
    call :Do buildship || exit /b 1
    goto :eof

:RealClean
    call :CreateSkel
    call :Do realclean || exit /b 1
    goto :eof

:CreateSkel
    call :CreateDirectory %INSTALLROOT%\bin || exit /b 1
    call :CreateDirectory %INSTALLROOT%\lib || exit /b 1
    call :CreateDirectory %INSTALLROOT%\pkg || exit /b 1
    goto :eof

:Do
    call %~dp0pkgcmds extract_options %1 || (
	    echo error : pkgcmds extract_options failed
	    exit /b 1
    )
    call %~dp0pkgcmds map_action %1 || (
	    echo error : pkgcmds map_action failed
	    exit /b 1
    )
    call %~dp0pkgcmds add_action_opts %1 || (
	    echo error : pkgcmds add_action_opts failed
	    exit /b 1
    )
    call :Run call %~dp0pkgmap %OPTIONS% %ADDARGS% -c "%ACTION%" %P% || exit /b 1
    goto :eof

:CreateDirectory
    if not exist %1 mkdir %1
    goto :eof
