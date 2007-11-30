@rem $Id$

@if "%_echo%" == "" @echo off

@rem
@rem This is meant to be called by other .cmd files and should not have
@rem setlocal/endlocal.
@rem

for %%i in (ACTION ARGS BUILDGLOBAL BUILDLOCAL CLEANGLOBAL       ) do if defined %%i set %%i=
for %%i in (CLEANLOCAL CM3BINSEARCHPATH CM3LIBSEARCHPATH         ) do if defined %%i set %%i=
for %%i in (CM3ROOT CM3VERSION EXE GCC_BACKEND GREP IGNORE_MISS  ) do if defined %%i set %%i=
for %%i in (INSTALLROOT M3BUILD M3GDB M3OSTYPE M3SHIP PKG_ACTION ) do if defined %%i set %%i=
for %%i in (PKGS PKGSDB REALCLEAN ROOT SCRIPTS SYSINFO_DONE      ) do if defined %%i set %%i=
for %%i in (SHIP SL SYSLIBDIR SYSLIBS TAR TARGET TMPDIR          ) do if defined %%i set %%i=
for %%i in (CM3_SYSCALL_WRAPPERS_EXIST                           ) do if defined %%i set %%i=
call :Clear INSTALLROOT_PREVIOUS
call :Clear INSTALLROOT_COMPILER_WITH_PREVIOUS
call :Clear INSTALLROOT_COMPILER_WITH_SELF
call :Clear INSTALLROOT_MIN
call :Clear INSTALLROOT_STD
call :Clear INSTALLROOT_CORE
call :Clear INSTALLROOT_BASE

for %%i in (USE_LIBCMT USE_MSVCRT USE_DELAYLOAD USE_DELAYIMP) do if defined %%i set %%i=
for %%i in (SRC_BUILDGLOBAL SRC_BUILDLOCAL SRC_CLEANGLOBAL SRC_CLEANLOCAL SRC_SHIP) do if defined %%i set %%i=
for %%i in (CM3 CM3_BUILDGLOBAL CM3_BUILDLOCAL CM3_CLEANGLOBAL CM3_CLEANLOCAL CM3_SHIP) do if defined %%i set %%i=
for %%i in (PM3_BUILDGLOBAL PM3_BUILDLOCAL PM3_CLEANGLOBAL PM3_CLEANLOCAL PM3_SHIP) do if defined %%i set %%i=
for %%i in (DEST BACKUPID CLEANLINK ADDARGS RESTORE) do if defined %%i set %%i=
for %%i in (P p_compiler p_runtime) do if defined %%i set %%i=

goto :eof

:Clear
if defined %1 set %1=
goto :eof
