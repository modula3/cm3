for %%a in (*.i3.c) do call :F1 %%a
goto :eof

:F1
gcc %1
a > %~n1
dos2unix %~n1
