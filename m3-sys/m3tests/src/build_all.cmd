@SetLocal

@echo off

set Errors=

for /f %%a in ('dir /s/b/a-d m3makefile') do call :F1 %%a
if defined Errors (
  echo failures in the following directories %Errors%
)

goto :eof

:F1
cd %1\..
@rem skip the top
if exist Test.common goto :eof
cm3 -build && goto :eof
call :F2 %~dp1..\.. %~dp1..
goto :eof

:F2
@rem
@rem add the last two path elements of the m3makefile's directory
@rem to the list of errors, e.g. p1\p123
@rem
set Errors=%Errors% %~n1\%~n2
goto :eof
