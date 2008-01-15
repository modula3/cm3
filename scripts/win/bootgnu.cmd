@rem $Id$

@setlocal

@rem check that cl and link are in path
@rem check that sh, ld, as, gcc, sed, make, etc. are in path
@rem check that they are msys/mingwin versions, not cygwin (and msys make)

@rem set PATH=%SystemDrive%\mingw\bin;%PATH%
@rem set PATH=%SystemDrive%\msys\1.0\bin;%PATH%
@rem set PATH=%SystemDrive%\msdev\50\vc\bin;%PATH%
@rem set LIB=%SystemDrive%\msdev\50\vc\lib
@rem set INCLUDE=%SystemDrive%\msdev\50\vc\include

if "%CM3_ROOT%" == "" call :set_full_path CM3_ROOT %~dp0..\..

rmdir /q/s \cm3
xcopy /fivery \cm3-min-WIN32-NT386-5.1.3 \cm3
del \cm3\bin\cm3.cfg
copy %CM3_ROOT%\m3-sys\cminstall\src\config\cm3.cfg \cm3\bin\cm3.cfg

@rem
@rem Use the "real" code.
@rem
cd %CM3_ROOT%\scripts\python

set CM3_TARGET=NT386
call python .\do-cm3-std.py realclean
call python .\upgrade.py || exit /b 1

set CM3_TARGET=NT386GNU
set LIB=
set INCLUDE=
set P=^
    m3cc ^
    m3core ^
    libm3 ^
    m3core ^
    libm3 ^
    m3middle ^
    m3objfile ^
    m3linker ^
    m3back ^
    m3staloneback ^
    m3front ^
    m3quake ^
    cm3

call python .\do-cm3-std.py realclean
call python .\do-pkg.py realclean %P%
call python .\do-pkg.py buildship %P% || exit /b 1
call python .\do-cm3-std.py buildship || exit /b 1

goto :eof

:set_full_path
set %1=%~f2
goto :eof
