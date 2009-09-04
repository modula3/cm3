SetLocal

if not defined CM3_VERSION (
    set CM3_VERSION=2009-05-01
)
set CM3_ROOT=C:\dev2\cm3.%CM3_VERSION%
set CM3=c:\cm3.%CM3_VERSION%\bin\cm3 -DROOT=%CM3_ROOT:\=/%
set TARGET=NT386

set p=^
    m3-libs/m3tk-misc ^
    m3-sys/m3scanner ^
    m3-sys/m3tools ^
    m3-tools/m3scan ^
    m3-tools/m3bundle ^
    m3-tools/m3tk ^
    m3-tools/mtex ^
    m3-comm/netobj ^
    m3-comm/stubgen ^
    m3-comm/tcp ^
    m3-ui/ui ^
    m3-ui/vbtkit ^
    m3-www/web ^
    m3-ui/jvideo ^
    m3-ui/videovbt ^
    m3-ui/formsvbtpixmaps ^
    m3-ui/formsvbt ^
    m3-ui/juno-2/juno-machine ^
    m3-ui/juno-2/juno-compiler ^
    m3-ui/juno-2/juno-app/pkl-fonts ^
    m3-ui/juno-2/juno-app

for %%a in (%p%) do call :clean %%a || exit /b 1
for %%a in (%p%) do call :buildship %%a || exit /b 1

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
%CM3% || exit /b 1
%CM3% -ship || exit /b 1
goto :eof
