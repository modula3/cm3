@REM #!cmd.exe
@REM $Id: find-packages.cmd,v 1.1 2004-04-27 15:03:37 sk Exp $

@if "%_echo%" == "" @echo off

setlocal

call %~dp0sysinfo.cmd

dir /s/b %ROOT%\m3makefile > %PKGSDB%.1
findstr /e /i src\m3makefile %PKGSDB%.1 > %PKGSDB%.2
sort %PKGSDB%.2 /output %PKGSDB%.1
del %PKGSDB%.2
for /f %%i in (%PKGSDB%.1) do call :F1 %%i\..\.. >> %PKGSDB%.2
move %PKGSDB%.2 %PKGSDB%
del %PKGSDB%.1
goto :eof

:F1
echo %~f1
goto :eof
