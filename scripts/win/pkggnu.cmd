@rem $Id: pkggnu.cmd,v 1.1 2008-01-14 04:42:32 jkrell Exp $

@rem
@rem This is a little helper to bring up NT386GNU.
@rem It "converts" INSTALLROOT\pkg\*\NT386 to INSTALLROOT\pkg\*\NT386GNU..
@rem

@setlocal

@call %~dp0clearenv || exit /b 1
@call %~dp0sysinfo || exit /b 1
@call %~dp0pkgcmds || exit /b 1

@echo on
echo INSTALLROOT is %INSTALLROOT%

for /d %%a in (%INSTALLROOT%\pkg\libm3 %INSTALLROOT%\pkg\m3core) do (
    for %%b in (%%a\NT386\*.lib) do (
        copy /y %%b %%~dpb\lib%%~nb.a
        copy /y %%b %%~dpb\%%~nb.a
    )
    for %%b in (%%a\NT386\*.m3x) do (
        copy /y %%b %%~dpb\lib%%~nb.m3x
    )
    mkdir %%a\NT386GNU 2>nul
    xcopy /fiverdy %%a\NT386 %%a\NT386GNU
)
