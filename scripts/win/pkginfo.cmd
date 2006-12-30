@rem $Id: pkginfo.cmd,v 1.1 2006-12-30 01:33:51 jkrell Exp $

@if "%_echo%" == "" @echo off

setlocal

call %~dp0sysinfo || exit /b 1

@rem
@rem
@rem Minimum sanity check.
@rem ROOT must appear in PKGS.
@rem This handles where source trees are move across drives
@rem or sometimes to different paths.
@rem This whole scheme does not handle network paths well.
@rem The paths need to relative, but that is hard to achieve
@rem directly in .cmd.
@rem
@rem

if exist %PKGSDB% (
	findstr /i /b %ROOT% %PKGSDB% 1>nul 2>&1 || (
		echo %PKGSDB% is invalid, deleting and regenerating.
		del %PKGSDB%
	)
)

if not exist %PKGSDB% (
    echo making %PKGSDB% with %~dp0find-packages
    call %~dp0find-packages || goto :eof
)

if not exist %PKGSDB% (
    echo cannot generate package list
    exit /b 1
)

if not "%1" == "" (shift & goto :%1)

endlocal

goto :eof

:pkgpath

(for /f %%i in ('findstr /e /i \%2 %PKGSDB%') do endlocal & set %1=%%i) || (
    echo package %2 not found
    error 2>nul
)

goto :eof
