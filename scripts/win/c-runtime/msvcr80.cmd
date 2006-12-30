@rem
@rem This sets up msvcr80.dll appropriately for use with Modula-3.
@rem On many systems -- pre-Windows XP, or not using Visual C++ 8.0 --
@rem it does nothing, or its affects amount to nothing.
@rem

@rem
@rem The install source for msvcr80.dll should be like Program Files\Visual C++ 8.0\redist, but I am using
@rem the "Express Edition" which lacks that, so copy out of %SystemRoot%\WinSxS -- at least initially.
@rem

@echo off

setlocal

set Verbose=
if defined Verbose echo on

call :RemoveTrailingPathSeperator %~dp0
set MyDir=%a%
if defined Verbose echo MyDir is %MyDir%.

@rem
@rem common 1
@rem
set msvcr_dll_file=msvcr80.dll
set msvcr_appman_file=Microsoft.VC80.CRT.manifest

@rem
@rem Use copy from Windows directory.
@rem
set msvcr_id=x86_Microsoft.VC80.CRT_1fc8b3b9a1e18e3b_8.0.50727.42_x-ww_0de06acd
set msvcr_dll_dir=%SystemRoot%\WinSxS\%msvcr_id%
set msvcr_sysman_file=%msvcr_id%.manifest
set msvcr_sysman_dir=%SystemRoot%\WinSxS\Manifests

@rem
@rem Use local copy instead.
@rem
set msvcr_id=
set msvcr_dll_dir=%MyDir%
set msvcr_sysman_file=%msvcr_appman_file%
set msvcr_sysman_dir=%MyDir%


@rem
@rem common 2
@rem
set msvcr_dll=%msvcr_dll_dir%\%msvcr_dll_file%
set msvcr_sysman=%msvcr_sysman_dir%\%msvcr_sysman_file%


if not exist %msvcr_dll% (
    echo %msvcr_dll% does not exist
    goto :eof
)

if defined Verbose echo %msvcr_dll% exists.

@rem
@rem Find cm3.exe in the path.
@rem

set a=
call :SearchPath cm3.exe
if not defined a (
    echo cm3.exe not found in path
    goto :eof
)

set CM3_EXE=%a%
if defined Verbose echo cm3.exe is in the PATH at %CM3_EXE%.

@rem
@rem Remove last path element -- \cm3\bin\cm3.exe => \cm3\bin.
@rem

call :GetFullPathname %CM3_EXE%\..
set CM3_BIN=%a%

@rem
@rem Remove last path element -- \cm3\bin => \cm3
@rem

call :GetFullPathname %CM3_BIN%\..
set CM3_ROOT=%a%

if defined Verbose echo cm3's install root is %CM3_ROOT%.

@rem
@rem Validate the layout.
@rem
if not exist %CM3_ROOT%\bin (
    echo %a%\bin does not exist.
    goto :eof
)

@rem
@rem Initially use copy in source tree.
@rem
set use_msvcr_manifest=%MyDir%\use_msvcr80.manifest

@rem
@rem In case cm3 on different drive than Windows, use the "first" set of files as the source to hard link.
@rem

call :HandleFile %CM3_EXE%

set msvcr_dll=%CM3_BIN%\%msvcr_dll_file%
set msvcr_sysman=%CM3_BIN%\%msvcr_appman_file%
set use_msvcr_manifest=%CM3_EXE%.manifest

@rem
@rem Handle all .exes and their directories.
@rem

for /f %%b in ('dir /s/b/a-d %CM3_ROOT%\*.exe') do call :HandleFile %%b

goto :eof


:SearchPath
set a=%~$PATH:1
goto :eof


:RemoveTrailingPathSeperator
set a=%1
if "%a:~-1%" == "\" (
    set a=%a:~0,-1%
)
goto :eof


:GetFullPathname
set a=%~f1
goto :eof


:HandleFile
rem echo HandleFile %1 ("%~nx1") ("%msvcr_dll_file%")
rem goto :eof
if /i "%~nx1" == "%msvcr_dll_file%" (
    rem echo skipping %1
    goto :eof
)
call :CopyOrHardLink %use_msvcr_manifest% %1.manifest
call :GetFullPathName %1\..
call :HandleDirectory %a%
goto :eof


:HandleDirectory
rem echo HandleDirectory %1
rem goto :eof
call :CopyOrHardLink %msvcr_dll%    %1\%msvcr_dll_file%
call :CopyOrHardLink %msvcr_sysman% %1\%msvcr_appman_file%
goto :eof


:CopyOrHardLink
if not exist %2 fsutil hardlink create %2 %1
if not exist %2 copy %1 %2
goto :eof
