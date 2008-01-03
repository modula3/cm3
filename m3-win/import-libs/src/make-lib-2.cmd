@echo This has been superceded by running cm3 which runs the Quake code in m3makefile.
@goto :eof

@echo off

@rem
@rem Jay Krell
@rem jaykrell@cornell.edu
@rem October 18, 2006
@rem

@rem 
@rem This is the second stage in producing Windows import .libs.
@rem Given the *.txt files output by make-lib-1.cmd, produce
@rem .c and .def files and compile and link them.
@rem

@setlocal

@pushd %~dp0

@rem @call :make-lib-2 "%~f0" winspool
@rem @goto :eof

@for %%i in (
    advapi32
    comctl32
    comdlg32
    gdi32
    glu32
    kernel32
    netapi32
    odbc32
    odbccp32
    opengl32
    user32
    winspool
    wsock32
) do @(
    @call :make-lib-2 "%~f0" %%i cl.exe link.exe
    @if errorlevel 1 goto :eof
)
@goto :eof

:make-lib-2
@rem
@rem usage: :make-lib-2 "%~f0" foo
@rem
@rem produce foo.lib and copy it to %INSTALLROOT%, which defaults to \cm3
@rem
@rem foo.txt is a text file as follows:
@rem
@rem  Export FunctionName NumberOfBytesOfParameters-or-__cdecl
@rem  # comment
@rem  Extension dll-or-drv
@rem
@rem Comments are allowed in the file via a pound sign as the first character.
@rem
@rem The input files are produced by make-lib-1.cmd from preexisting import .libs.
@rem The current working directory is used for temporary storage.
@rem

@setlocal

@if not exist %2.txt (
    @echo error : %2.txt must exist.
    @exit /b 1
)

@if not defined INSTALLROOT set INSTALLROOT=\cm3

@call :clean %2

@echo>%2.def ;This file was produced by %1 at %date% %time% on %computername% by %username%.
@echo>%2.c  /* This file was produced by %1 at %date% %time% on %computername% by %username%. */

@echo>>%2.def ;cl is "%~$PATH:3"
@echo>>%2.c  /* cl is "%~$PATH:3"" */

@echo>>%2.def ;link is "%~$PATH:4"
@echo>>%2.c  /* link is "%~$PATH:4" */

@echo>>%2.def EXPORTS
@echo>>%2.c void __stdcall Entry(void) { }
@echo>>%2.c typedef unsigned u;
@set Extension=dll

@rem
@rem Read and dispatch the data file.
@rem
@for /f "tokens=1-3" %%a in (%2.txt) do @call :%%a %2 %%b %%c

cl /c /nologo /W3 /WX /Z7 %2.c
@if errorlevel 1 goto :eof
link /nologo /debug /incremental:no /entry:Entry /subsystem:native /nodefaultlib /def:%2.def /out:%2.%Extension% %2.obj
@if errorlevel 1 goto :eof

@call :my_del %INSTALLROOT%\lib\%2.lib
copy %2.lib %INSTALLROOT%\lib

@call :clean_2 %2

@goto :eof

:#
@goto :eof

:Extension
@set Extension=%2
@goto :eof

:Export
@echo>>%1.def  %2
@call :%3 %1.c %2
@goto :eof

:0
@echo>>%1 void __stdcall %2(void) { }
@goto :eof

:4
@echo>>%1 void __stdcall %2(u a1) { }
@goto :eof

:8
@echo>>%1 void __stdcall %2(u a1, u a2) { }
@goto :eof

:12
@echo>>%1 void __stdcall %2(u a1, u a2, u a3) { }
@goto :eof

:16
@echo>>%1 void __stdcall %2(u a1, u a2, u a3, u a4) { }
@goto :eof

:20
@echo>>%1 void __stdcall %2(u a1, u a2, u a3, u a4, u a5) { }
@goto :eof

:24
@echo>>%1 void __stdcall %2(u a1, u a2, u a3, u a4, u a5, u a6) { }
@goto :eof

:28
@echo>>%1 void __stdcall %2(u a1, u a2, u a3, u a4, u a5, u a6, u a7) { }
@goto :eof

:32
@echo>>%1 void __stdcall %2(u a1, u a2, u a3, u a4, u a5, u a6, u a7, u a8) { }
@goto :eof

:36
@echo>>%1 void __stdcall %2(u a1, u a2, u a3, u a4, u a5, u a6, u a7, u a8, u a9) { }
@goto :eof

:40
@echo>>%1 void __stdcall %2(u a1, u a2, u a3, u a4, u a5, u a6, u a7, u a8, u a9, u a10) { }
@goto :eof

:44
@echo>>%1 void __stdcall %2(u a1, u a2, u a3, u a4, u a5, u a6, u a7, u a8, u a9, u a10, u a11) { }
@goto :eof

:48
@echo>>%1 void __stdcall %2(u a1, u a2, u a3, u a4, u a5, u a6, u a7, u a8, u a9, u a10, u a11, u a12) { }
@goto :eof

:52
@echo>>%1 void __stdcall %2(u a1, u a2, u a3, u a4, u a5, u a6, u a7, u a8, u a9, u a10, u a11, u a12, u a13) { }
@goto :eof

:56
@echo>>%1 void __stdcall %2(u a1, u a2, u a3, u a4, u a5, u a6, u a7, u a8, u a9, u a10, u a11, u a12, u a13, u a14) { }
@goto :eof

:60
@echo>>%1 void __stdcall %2(u a1, u a2, u a3, u a4, u a5, u a6, u a7, u a8, u a9, u a10, u a11, u a12, u a13, u a14, u a15) { }
@goto :eof

:64
@echo>>%1 void __stdcall %2(u a1, u a2, u a3, u a4, u a5, u a6, u a7, u a8, u a9, u a10, u a11, u a12, u a13, u a14, u a15, u a16) { }
@goto :eof

:68
@echo>>%1 void __stdcall %2(u a1, u a2, u a3, u a4, u a5, u a6, u a7, u a8, u a9, u a10, u a11, u a12, u a13, u a14, u a15, u a16, u a17) { }
@goto :eof

:72
@echo>>%1 void __stdcall %2(u a1, u a2, u a3, u a4, u a5, u a6, u a7, u a8, u a9, u a10, u a11, u a12, u a13, u a14, u a15, u a16, u a17, u a18) { }
@goto :eof

:__cdecl
@echo>>%1 void __cdecl %2() { }
@goto :eof

:clean
@call :my_del %1.c %1.def %1.obj %1.drv %1.dll %1.exp %1.pdb vc*.pdb %1.lib
@goto :eof

:clean_2
@call :my_del %1.c %1.def %1.obj %1.drv %1.dll %1.exp %1.pdb vc*.pdb %1.lib
@goto :eof

:my_del
@if (%1) == () @goto :eof
@if exist %1 del %1
@shift
@goto :my_del
