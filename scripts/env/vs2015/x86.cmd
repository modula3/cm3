set CM3_VS2015_OR_NEWER=1

if "%PROCESSOR_ARCHITECTURE%" == "AMD64" goto :x86
if "%PROCESSOR_ARCHITEW6432%" == "AMD64" goto :x86

call "%SystemDrive%\Program Files\Microsoft Visual Studio 14.0\Common7\Tools\VsDevCmd.bat"
goto :eof

:x86
call "%SystemDrive%\Program Files (x86)\Microsoft Visual Studio 14.0\Common7\Tools\VsDevCmd.bat"
goto :eof
