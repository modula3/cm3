@rem $Id$

@setlocal

@rem
@rem This is based on upgrade-5.3.sh.
@rem

@set p_runtime=^
m3gc-enhanced ^
m3core ^
libm3
@set p_compiler=^
m3middle ^
m3objfile ^
m3linker ^
m3back ^
m3staloneback ^
m3front ^
m3quake ^
cm3 ^
patternmatching ^
mklib

@call :header "backing up existing packages (once only)"
@rem does nothing if backup already exists
call %~dp0backup-pkgs || exit /b 1
@echo on

@call :header "restoring old packages from backup (for multiple runs)"
@rem for multiple runs of this script...
call %~dp0backup-pkgs -restore  || exit /b 1
@echo on

@call :header "cleaning core packages"
call %~dp0do-cm3-core realclean || exit /b 1
@echo on

@call :header "rebuilding runtime/compiler"
call %~dp0do-pkg buildship %p_runtime% %p_compiler% || exit /b 1
@echo on

@call :header "installing new compiler"
call %~dp0install-cm3-compiler upgrade || exit /b 1
@echo on

@call :header "cleaning core packages"
call %~dp0do-cm3-core realclean || exit /b 1
@echo on

@call :header "building core packages"
call %~dp0do-cm3-core buildship || exit /b 1
@echo on

@endlocal

@goto :eof

:header
    @echo.
	@echo "============================================================================="
	@echo "%*"
	@echo "============================================================================="
    @echo.
	@goto :eof
