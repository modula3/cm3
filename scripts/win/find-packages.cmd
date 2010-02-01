@if "%_echo%" == "" @echo off

setlocal

call %~dp0sysinfo || exit /b 1

dir /s/b %ROOT%\m3makefile > %PKGSDB%.1
findstr /e /i \src\m3makefile %PKGSDB%.1 > %PKGSDB%.2
sort < %PKGSDB%.2 > %PKGSDB%.1
del %PKGSDB%.2
for /f %%i in (%PKGSDB%.1) do call :F1 %%i\..\.. >> %PKGSDB%.2
move %PKGSDB%.2 %PKGSDB%
del %PKGSDB%.1
endlocal
goto :eof

:F1
set a=%~f1
for /f "tokens=*" %%b in ('cmd /c echo %%a:%ROOT%\^=%%') do set a=%%b
set a=%a:\=/%
echo %a%
@goto :eof
