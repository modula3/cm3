@rem
@rem Set a bare environment that includes:
@rem  operating system (c:\windows et. al.)
@rem  git (in a fairly convention location, not the Cygwin one)
@rem  Python (in a fairly conventional location)
@rem  cm3 -- in a location corresponding to the checkout
@rem         containing this file.
@rem
@rem convention:
@rem   script at \dev2\cm3.2\scripts\env\clean2.cmd
@rem   => put \cm3.2\bin in %PATH%
@rem
@rem In this fashion, each git checkout has its own
@rem associated install.
@rem

@call :F1 %~dp0..
@goto :eof

:F1
@call :F2 %~dp1.
@goto :eof

:F2
set CM3_VS2015_OR_NEWER=
set INCLUDE=
set LIB=
set LIBPATH=
@set PATH=%PATH:\;=;%

set PATH=^
%~d1\%~nx1\bin;^
%~d1\Python27;^
%~d1\Python27\Scripts;^
%ProgramFiles%\Git\cmd;^
%SystemRoot%\system32;^
%SystemRoot%;^
%SystemRoot%\System32\Wbem;^
%SystemRoot%\System32\WindowsPowerShell\v1.0
