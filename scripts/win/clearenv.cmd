@rem $Id: clearenv.cmd,v 1.2 2006-12-30 11:36:38 jkrell Exp $

@if "%_echo%" == "" @echo off

@rem
@rem This is meant to be called by other .cmd files and should not have
@rem setlocal/endlocal.
@rem

for %%i in (ACTION ARGS BUILDGLOBAL BUILDLOCAL CLEANGLOBAL       ) do if defined %%i set %%i=
for %%i in (CLEANLOCAL CM3 CM3BINSEARCHPATH CM3LIBSEARCHPATH     ) do if defined %%i set %%i=
for %%i in (CM3ROOT CM3VERSION EXE GCC_BACKEND GREP IGNORE_MISS  ) do if defined %%i set %%i=
for %%i in (INSTALLROOT M3BUILD M3GDB M3OSTYPE M3SHIP PKG_ACTION ) do if defined %%i set %%i=
for %%i in (PKGS PKGSDB REALCLEAN ROOT SCRIPTS SYSINFO_DONE      ) do if defined %%i set %%i=
for %%i in (SHIP SL SYSLIBDIR SYSLIBS TAR TARGET TMPDIR          ) do if defined %%i set %%i=
for %%i in (CM3_SYSCALL_WRAPPERS_EXIST                           ) do if defined %%i set %%i=
