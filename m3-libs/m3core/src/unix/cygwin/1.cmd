call :F1 Utypes.i3.cpp
call :F1 Usignal.i3.c
goto :eof

:F1
gcc %1
dos2unix %1
a > %~n1
dos2unix %~n1
