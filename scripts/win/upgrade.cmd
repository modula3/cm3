@setlocal

@rem
@rem This is based on upgrade-5.3.sh.
@rem

@rem Start with the installed cm3.
@if defined CM3 set CM3=

@set p_runtime=^
 m3core ^
 libm3

@set p_compiler=^
 sysutils ^
 m3middle ^
 m3objfile ^
 m3linker ^
 m3back ^
 m3front ^
 m3quake ^
 cm3 ^
 mklib

@call :header cleaning core packages
call %~dp0do-cm3-core realclean || exit /b 1
@echo on

@call :header building just new compiler with old compiler (old compiler cannot necessarily build new runtime)
call %~dp0do-pkg buildship %p_compiler% || exit /b 1
@echo on

@call :header installing new compiler
call %~dp0install-cm3-compiler upgrade || exit /b 1
@echo on

@call :header cleaning core packages
call %~dp0do-cm3-core realclean || exit /b 1
@echo on

@call :header building compiler and runtime with new compiler
call %~dp0do-pkg buildship %p_runtime% %p_compiler% || exit /b 1
@echo on

@call :header installing new compiler
call %~dp0install-cm3-compiler upgrade || exit /b 1
@echo on

@call :header building core packages
call %~dp0do-cm3-core buildship || exit /b 1
@echo on

@echo %~n0 : Success.
@echo %~n0 : You should now have a current compiler, runtime, and core
@echo %~n0 : packages, and be able to build and/or ship "anything" up to and
@echo %~n0 : including "standard". i.e. do-cm3-std buildship, or cd around the
@echo %~n0 : source tree and cm3 or cm3 -ship.

@endlocal

@goto :eof

:header
    @echo.
	@echo =============================================================================
	@echo %*
	@echo =============================================================================
    @echo.
	@goto :eof
