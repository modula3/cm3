@rem $Id$

@if "%_echo%" == "" @echo off

setlocal

@rem Start with the installed cm3.
@if defined CM3 set CM3=

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

set INSTALLROOT_MIN=%STAGE%\min\cm3
set INSTALLROOT_STD=%STAGE%\std\cm3
set INSTALLROOT_CORE=%STAGE%\core\cm3
set INSTALLROOT_BASE=%STAGE%\base\cm3

@rem for incremental runs to recover at this step..
if /i "%1" == "goto_tar" shift & goto :TarGzip
if /i "%1" == "goto_min" shift & goto :min
if /i "%1" == "goto_zip" shift & goto :Zip
if /i "%1" == "goto_tarbzip2" shift & goto :TarBzip2

@rem ------------------------------------------------------------------------------------------------------------------------
call :Echo build new compiler with old compiler and old runtime (%INSTALLROOT_PREVIOUS% to %INSTALLROOT_COMPILER_WITH_PREVIOUS%)
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

@rem build just compiler this pass, not the runtime
set P=import-libs m3middle m3linker m3front m3quake m3objfile m3back m3staloneback m3objfile cm3

@rem copy over runtime package store from old to new
for %%a in (libm3 m3core) do xcopy /fiveryh %INSTALLROOT%\pkg\%%a %INSTALLROOT_COMPILER_WITH_PREVIOUS%\pkg\%%a || exit /b 1

@rem
@rem cm3 is run out of %path%, but mklib is not, so we have to copy it..
@rem
call :Run call :CopyMklib %INSTALLROOT% %INSTALLROOT_COMPILER_WITH_PREVIOUS% || exit /b 1
set INSTALLROOT=%INSTALLROOT_COMPILER_WITH_PREVIOUS%
set LIB=%INSTALLROOT%\lib;%LIB%
call :RealClean || exit /b 1
call :BuildShip || exit /b 1
call :ShipCompiler || exit /b 1
call :RealClean || exit /b 1

endlocal

@rem ----------------------------------------------------------------------------------------------------------------------------------
call :Echo build new compiler and new runtime with new compiler (%INSTALLROOT_COMPILER_WITH_PREVIOUS% to %INSTALLROOT_COMPILER_WITH_SELF%)
@rem ----------------------------------------------------------------------------------------------------------------------------------

setlocal

set INSTALLROOT=%INSTALLROOT_COMPILER_WITH_SELF%
set LIB=%INSTALLROOT%\lib;%LIB%
set PATH=%INSTALLROOT_COMPILER_WITH_PREVIOUS%\bin;%PATH%
call :RealClean || exit /b 1
call :BuildShip || exit /b 1
call :ShipCompiler || exit /b 1
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
set LIB=%INSTALLROOT%\lib;%LIB%
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
set LIB=%INSTALLROOT%\lib;%LIB%
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
set LIB=%INSTALLROOT%\lib;%LIB%
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

goto :Zip

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
call :CopyFile %STAGE%\cminstall.exe . || exit /b 1
call :CopyFile %ROOT%\m3-sys\COPYRIGHT-CMASS . || exit /b 1
call :CopyFile %ROOT%\tools\win32\tar.exe . || exit /b 1
call :CopyFile %ROOT%\tools\win32\gzip.exe . || exit /b 1
call :CopyFile %ROOT%\tools\win32\cygwin.dll . || exit /b 1
if exist cm3-min-%M3OSTYPE%-%TARGET%-%CM3VERSION%.tgz del cm3-min-%M3OSTYPE%-%TARGET%-%CM3VERSION%.tgz
tar cvzf cm3-min-%M3OSTYPE%-%TARGET%-%CM3VERSION%.tgz cminstall.exe COPYRIGHT-CMASS system.tgz tar.exe gzip.exe cygwin.dll || exit /b 1
tar cvzf cm3-min-%M3OSTYPE%-%TARGET%-%CM3VERSION%-symbols.tgz symbols || exit /b 1
popd

echo Done.
echo Output is %INSTALLROOT_MIN%\cm3-min-%M3OSTYPE%-%TARGET%-%CM3VERSION%.tgz
echo Output is %INSTALLROOT_MIN%\cm3-min-%M3OSTYPE%-%TARGET%-%CM3VERSION%-symbols.tgz
echo Lots of intermediate state remains in %STAGE%.

goto :eof

:Zip

pushd %INSTALLROOT_MIN%
call :CopyFile %ROOT%\m3-sys\COPYRIGHT-CMASS . || exit /b 1
cd ..
if not exist symbols mkdir symbols
for /f %%a in ('dir /s/b/a-d *.pdb') do move %%a symbols

del *.bz2 *.zip *.exe *.tar

rem set symbols=cm3-min-%M3OSTYPE%-%TARGET%-%CM3VERSION%-symbols.tar.bz2
rem tar cfvj %symbols% symbols

set symbols=cm3-min-%M3OSTYPE%-%TARGET%-%CM3VERSION%-symbols.zip
call :RunZip -9 -r -D -X %symbols% symbols

set zip=cm3-min-%M3OSTYPE%-%TARGET%-%CM3VERSION%.zip
call :RunZip -9 -r -D -X %zip% cm3

@rem
@rem On my machine \bin\unzipsfx.exe is a
@rem Win32 x86 unzip self extracting archive prefix,
@rem with an MS-DOS unzip self extracting archive prefix for a stub.
@rem As such, you can do several things with it.
@rem  Run it under MS-DOS. However long file names are probably used.
@rem  Run it from a Windows command line.
@rem  Open it with various archive utilities, including maybe Explorer (might need to rename it to end in .zip).
@rem
@rem However, .tar.bz2 is generally significantly smaller than .zip and therefore used instead.
@rem
@rem I built this unzipsfx from the publically available source. That source
@rem and building of it is not in the CM3 tree, and probably should be
@rem if this path is to be used. In fact, that license may make
@rem these tools favorable over tar/bzip2, despite the compression loss.
@rem

set exe=cm3-min-%M3OSTYPE%-%TARGET%-%CM3VERSION%.exe
rem UPX proved unreliable after not much use.
rem copy /b \bin\unzipsfx-upx.exe + %zip% %exe%
copy /b \bin\upx\unzipsfx.exe + %zip% %exe%
call :RunZip -A %exe%

popd
goto :done

:TarBzip2

pushd %INSTALLROOT_MIN%
call :CopyFile %ROOT%\m3-sys\COPYRIGHT-CMASS . || exit /b 1
cd ..
if not exist symbols mkdir symbols
for /f %%a in ('dir /s/b/a-d *.pdb') do move %%a symbols
set symbols=cm3-min-%M3OSTYPE%-%TARGET%-%CM3VERSION%-symbols.tar.bz2
set zip=cm3-min-%M3OSTYPE%-%TARGET%-%CM3VERSION%.tar.bz2
if exist %zip% del %zip%
if exist %symbols% del %symbols%
tar cfvj %symbols% symbols
tar cfvj %zip% cm3
popd
goto :done

:done
echo Done.
if defined exe if exist %exe% echo Output is %STAGE%\min\%exe%
echo Output is %STAGE%\min\%zip%
echo Output is %STAGE%\min\%symbols%
echo Lots of intermediate state remains in %STAGE%.

goto :eof


:Run
    setlocal
    set x=%*
    set x=%x:  = %
    set x=%x:  = %
    echo %x%
    %x% || goto :RunError
    endlocal
    goto :eof

:RunError
	echo ERROR: %x%
	call :Where cm3.exe
	echo %STAGE% and/or %%TEMP%%\cm3 will be full of stuff.
	exit /b 1

:Where
@echo WHERE: %1 =^> %~$PATH:1
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
    call :Run call :CreateDirectory %2\bin || exit /b 1
    call :Run call :CopyFile        %1\bin\mklib.exe %2\bin\mklib.exe || exit /b 1
    call :Run call :CopyFileIfExist %1\bin\mklib.pdb %2\bin\mklib.pdb || exit /b 1
    call :Run call :CopyFileIfExist %1\bin\mklib.exe.manifest %2\bin\mklib.exe.manifest || exit /b 1
    goto :eof

:CopyFile
    setlocal
    set from=%1
    set to=%2
    if "%to%" == "." set to=.\%~nx1
    if exist %to% del %to% || exit /b 1
    call :Run copy %from% %to% || exit /b 1
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

:RunZip
    @rem lame workaround
    @rem zip appends %zip% to its command line
    @rem and I really want to use it for my own purposes
    setlocal
    set zip=
    zip %*
    endlocal
