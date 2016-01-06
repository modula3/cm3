@rem Use the compiler, linker, headers, libraries in the
@rem Windows Device Driver Kit from Windows Server 2003, for x86.

@rem cderr.h is missing

@call :F1 c:\winddk\3790 wxp i386
@call :F3 c:\winddk\3790 wnet i386
@goto :eof

:F1
set INCLUDE=%1\inc\crt;%1\inc\%2
set LIB=%1\lib\%2\%3
set PATH=%1\bin\x86;%PATH%
set LIBPATH=
@goto :eof

:F3
rem set INCLUDE=%INCLUDE%;%1\inc\%2
goto :eof
