@rem $Id: pkggnu_clean.cmd,v 1.2 2008-01-21 05:05:02 jkrell Exp $

@rem
@rem This is a little helper to bring up NT386GNU.
@rem It cleans out all the INSTALLROOT\pkg\*\NT386GNU directories.
@rem

@setlocal

set CM3_TARGET=NT386GNU

@call %~dp0clearenv || exit /b 1
@call %~dp0sysinfo || exit /b 1
@call %~dp0pkgcmds || exit /b 1

@echo on
echo INSTALLROOT is %INSTALLROOT%

for /d %%a in (%INSTALLROOT%\pkg\*) do (
    rmdir /q/s %%a\%CM3_TARGET%
)
