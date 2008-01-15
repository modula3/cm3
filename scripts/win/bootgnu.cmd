@rem $Id$

@setlocal

@rem check that cl and link are in path
@rem check that sh, ld, as, gcc, sed, make, etc. are in path

@rem set PATH=%SystemDrive%\msys\1.0\bin;%SystemDrive%\mingw\bin;%PATH%

if "%CM3_ROOT%" == "" call :set_full_path CM3_ROOT %~dp0..\..

rmdir /q/s \cm3
xcopy /fivery \cm3-min-WIN32-NT386-5.1.3 \cm3
del \cm3\bin\cm3.cfg
copy %CM3_ROOT%\m3-sys\cminstall\src\config\cm3.cfg \cm3\bin\cm3.cfg

cd %CM3_ROOT%\scripts\python
set CM3_TARGET=NT386
upgrade || exit /b 1

set CM3_TARGET=NT386GNU
do-pkg buildship ^
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

do-cm3-std realclean || exit /b 1
do-cm3-std buildship || exit /b 1

goto :eof

:set_full_path
set %1=%~f2
goto :eof
