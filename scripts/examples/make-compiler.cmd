SetLocal

if not defined CM3_VERSION (
    set CM3_VERSION=2009-05-01
)
set CM3_ROOT=C:\dev2\cm3.%CM3_VERSION%
set CM3=c:\cm3.%CM3_VERSION%\bin\cm3.exe
set CM3_FLAGS=-DROOT=%CM3_ROOT:\=/%
set TARGET=NT386
set PATH=c:\cm3.%CM3_VERSION%\bin;%PATH%

set p=^
    m3-libs/m3core ^
    m3-libs/libm3 ^
    m3-libs/sysutils ^
    m3-libs/set ^
    m3-sys/m3middle ^
    m3-sys/m3quake ^
    m3-sys/m3front ^
    m3-sys/m3linker ^
    m3-sys/m3objfile ^
    m3-sys/m3back ^
    m3-sys/cm3 ^
    m3-sys/mklib

@rem
@rem Do it twice so it builds itself.
@rem

for /l %%b in (1 1 2) do (
    rmdir /q/s %CM3_ROOT%\pkg
    for %%a in (%p%) do call :clean %%a || exit /b 1
    for %%a in (%p%) do call :buildship %%a || exit /b 1
    copy %CM3_ROOT%\m3-sys\cm3\%TARGET%\cm3.* %CM3%\..\bin
    copy %CM3_ROOT%\m3-sys\mklib\%TARGET%\mklib.* %CM3%\..\bin
)

goto :eof

:clean
SetLocal
set a=%1
set a=%a:/=\%
cd %CM3_ROOT%\%a% || exit /b 1
rmdir /q/s %TARGET%
goto :eof

:buildship
SetLocal
set a=%1
set a=%a:/=\%
cd %CM3_ROOT%\%a% || exit /b 1
%CM3% %CM3_FLAGS% || exit /b 1
%CM3% %CM3_FLAGS% -ship || exit /b 1
goto :eof
