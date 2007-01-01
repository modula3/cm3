@rem $Id: make-dist.cmd,v 1.1 2007-01-01 19:19:13 jkrell Exp $

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

@rem ------------------------------------------------------------------------------------------------------------------------
call :header build new compiler with old compiler (%INSTALLROOT_PREVIOUS% to %INSTALLROOT_COMPILER_WITH_PREVIOUS%)
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

call :CopyMklib %INSTALLROOT% %INSTALLROOT_COMPILER_WITH_PREVIOUS%
set INSTALLROOT=%INSTALLROOT_COMPILER_WITH_PREVIOUS%
call :RealClean || exit /b 1
call :BuildShip || exit /b 1
call :ShipCompiler || exit /b 1
call :RealClean || exit /b 1

endlocal

@rem ----------------------------------------------------------------------------------------------------------------------------------
call :header build compiler with itself (%INSTALLROOT_COMPILER_WITH_PREVIOUS% to %INSTALLROOT_COMPILER_WITH_SELF%)
@rem ----------------------------------------------------------------------------------------------------------------------------------

setlocal

set INSTALLROOT=%INSTALLROOT_COMPILER_WITH_SELF%
set PATH=%INSTALLROOT_COMPILER_WITH_PREVIOUS%\bin;%PATH%
call :RealClean || exit /b 1
call :BuildShip || exit /b 1
call :ShipCompiler || exit /b 1
call :RealClean || exit /b 1

endlocal

set P=

@rem ----------------------------------------------------------------------------------------------------------------------------------
call :header build minimal packages with new compiler
@rem ----------------------------------------------------------------------------------------------------------------------------------

setlocal

set INSTALLROOT=%INSTALLROOT_MIN%
call :CopyCompiler %INSTALLROOT_COMPILER_WITH_SELF% %INSTALLROOT% || exit /b 1
set PATH=%INSTALLROOT%\bin;%PATH%

call %~dp0do-cm3-min realclean || exit /b 1
call %~dp0do-cm3-min buildship || exit /b 1
call %~dp0do-cm3-min realclean || exit /b 1

endlocal

@rem ----------------------------------------------------------------------------------------------------------------------------------
call :header build core packages with new compiler
@rem ----------------------------------------------------------------------------------------------------------------------------------

setlocal

set INSTALLROOT=%INSTALLROOT_CORE%
call :CopyCompiler %INSTALLROOT_COMPILER_WITH_SELF% %INSTALLROOT% || exit /b 1
set PATH=%INSTALLROOT%\bin;%PATH%

call %~dp0do-cm3-core realclean || exit /b 1
call %~dp0do-cm3-core buildship || exit /b 1
call %~dp0do-cm3-core realclean || exit /b 1

endlocal

@rem ----------------------------------------------------------------------------------------------------------------------------------
call :header build standard packages with new compiler
@rem ----------------------------------------------------------------------------------------------------------------------------------

setlocal

set INSTALLROOT=%INSTALLROOT_STD%
call :CopyCompiler %INSTALLROOT_COMPILER_WITH_SELF% %INSTALLROOT% || exit /b 1
set PATH=%INSTALLROOT%\bin;%PATH%

call %~dp0do-cm3-std realclean || exit /b 1
call %~dp0do-cm3-std buildship || exit /b 1
call %~dp0do-cm3-std realclean || exit /b 1

endlocal


@rem ----------------------------------------------------------------------------------------------------------------------------------

echo INSTALLROOT_MIN=%INSTALLROOT_MIN%
echo INSTALLROOT_STD=%INSTALLROOT_STD%
echo INSTALLROOT_CORE=%INSTALLROOT_CORE%
echo INSTALLROOT_BASE=%INSTALLROOT_BASE%

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
    call :my_mkdir %INSTALLROOT%
    call :my_copy       %ROOT%\m3-sys\cm3\%TARGET%\cm3.exe %INSTALLROOT%\bin\cm3.exe || exit /b 1
    call :my_copy       %ROOT%\m3-sys\cm3\%TARGET%\cm3.cfg %INSTALLROOT%\bin\cm3.cfg || exit /b 1
    call :my_copy       %ROOT%\m3-sys\cm3\%TARGET%\cm3.pdb %INSTALLROOT%\bin\cm3.pdb || exit /b 1
    call :copy_if_exist %ROOT%\m3-sys\cm3\%TARGET%\cm3.exe.manifest %INSTALLROOT%\bin\cm3.exe.manifest || exit /b 1
    call :my_copy       %ROOT%\m3-sys\cminstall\src\config\%TARGET% %INSTALLROOT%\bin\cm3.cfg || exit /b 1
    goto :eof

:CopyCompiler
    @rem
    @rem Copy the compiler from one INSTALLROOT to another, possibly having cleaned out the intermediate directories.
    @rem The config file always comes right out of the source tree.
    @rem
    call :my_mkdir %2\bin
    call :my_copy       %1\bin\cm3.exe %2\bin\cm3.exe || exit /b 1
    call :my_copy       %1\bin\cm3.pdb %2\bin\cm3.pdb || exit /b 1
    call :copy_if_exist %1\bin\cm3.exe.manifest %2\bin\cm3.exe.manifest || exit /b 1
    call :my_copy       %ROOT%\m3-sys\cminstall\src\config\%TARGET% %2\bin\cm3.cfg || exit /b 1
    goto :eof

:CopyMklib
    @rem
    @rem Copy mklib from one INSTALLROOT to another, possibly having cleaned out the intermediate directories.
    @rem
    call :my_mkdir %2\bin
    call :my_copy       %1\bin\mklib.exe %2\bin\mklib.exe || exit /b 1
    call :my_copy       %1\bin\mklib.pdb %2\bin\mklib.pdb || exit /b 1
    call :copy_if_exist %1\bin\mklib.exe.manifest %2\bin\mklib.exe.manifest || exit /b 1
    goto :eof

:my_copy
    if exist %2 del %2 || exit /b 1
    if exist %1 copy %1 %2 || exit /b 1
    goto :eof

:copy_if_exist
    if exist %2 del %2 || exit /b 1
    if exist %1 copy %1 %2 || exit /b 1
    goto :eof


:header
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
    call :my_mkdir %INSTALLROOT%\bin || exit /b 1
    call :my_mkdir %INSTALLROOT%\lib || exit /b 1
    call :my_mkdir %INSTALLROOT%\pkg || exit /b 1
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

:my_mkdir
    if not exist %1 mkdir %1
    goto :eof
