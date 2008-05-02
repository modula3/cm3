SetLocal

for /f %%a in ('dir /s/b/a-d m3makefile') do call :F1 %%a
goto :eof

:F1
cd %1\..
dos2unix m3makefile
findstr Test.common m3makefile >nul && goto :eof
echo include ("../../Test.common") >> m3makefile
goto :eof
