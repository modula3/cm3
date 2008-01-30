call :F1 Usignal.i3.c
call :F1 Upthread.i3.c
goto :eof

:F1
gcc %1
a > %~n1
dos2unix %~n1
