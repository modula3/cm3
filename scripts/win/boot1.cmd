@echo off

@rem
@rem
@rem Having used python\do-cm3-front with CM3_TARGET=x, CM3_FLAGS=-boot,
@rem this script archives up the files needed on the target.
@rem
@rem It is assumed the target has a full source tree.
@rem It is assumed building a native cm3cg is easy enough.
@rem The trick/goal of bootstrapping is to produce a cm3 and mklib on the new target.
@rem From there, the tree can be rebuilt normally from m3core and on up.
@rem (We don't care about "shipping" during boot.)
@rem
@rem

SetLocal

@rem
@rem Edit this to suit.
@rem
set PLATFORM=SPARC32_LINUX
set CC=gcc -m32 -gstabs+ -munaligned-doubles
set AS=as -32
set VERSION=1
set BOOT=cm3-boot-POSIX-%PLATFORM%-%VERSION%

@rem
@rem mklib and cm3 must come last since they are .exes
@rem mklib is only need for NT386* currently
@rem
set libs=m3 m3core sysutils m3front m3link m3middle m3objfile m3quake m3back
@rem set packages=%libs% mklib cm3
set packages=%libs% cm3

cd %~dp0..\..
rmdir /q/s \%BOOT% 2>nul
mkdir \%BOOT%

for %%a in (%packages%) do for /f %%b in ('dir /s/b/a-d make.%%a') do call :F1 %%b\.. %%b\..\..

cd \%BOOT%
del /s *.ic *.mc *.sh *.m3 *.i3 Makefile 2>nul

mkdir \%BOOT%\obj
mkdir \%BOOT%\exe
mkdir \%BOOT%\lib

echo all: exe/cm3 >> \%BOOT%\Makefile
echo AS=%AS% >> \%BOOT%\Makefile
echo CC=%CC% >> \%BOOT%\Makefile

rem
rem mkdir \%BOOT%\config
rem These files have by now been copied into the source tree of the other computer.
rem
rem if defined CM3_ROOT copy /y %CM3_ROOT:/=\%\m3-sys\cminstall\src\config \%BOOT%\config
rem if defined CM3_ROOT copy /y %CM3_ROOT:/=\%\m3-sys\cminstall\src\config-no-install \%BOOT%\config

for %%a in (%packages%) do call :F2 %%a

dos2unix \%BOOT%\makeverbose.sh
findstr /b /v echo \%BOOT%\makeverbose.sh > \%BOOT%\make.sh
dos2unix \%BOOT%\Makefile
cd \
del %BOOT%.tar.bz2 %BOOT%.tar.gz %BOOT%.tar 2>nul
tar cfjv %BOOT%.tar.bz2 %BOOT%

@rem
@rem for the really lazy:
@rem
del make.sh 2>nul
echo rm -rf %BOOT% >> make.sh
echo tar xvf %BOOT%.tar.bz2 >> make.sh
echo cd %BOOT% >> make.sh
echo . ./make.sh >> make.sh
echo cd .. >> make.sh
echo echo try this: ./%BOOT%/exe/cm3 >> make.sh
dos2unix make.sh
rem scp make.sh jay@192.168.2.4:~
rem scp %BOOT%.tar.bz2 jay@192.168.2.4:~

goto :eof

:F1
mkdir \%BOOT%\%~nx2
copy %1\*.c \%BOOT%\%~nx2
copy %1\*.h \%BOOT%\%~nx2
copy %1\*.is \%BOOT%\%~nx2
copy %1\*.ms \%BOOT%\%~nx2
goto :eof


:F2
if not exist %1 (
    if exist lib%1 (
        call :F2 lib%1
    )
    if exist %1er (
        call :F2 %1er
    )
    goto :eof
)
pushd %1
for %%a in (*.is *.ms) do call :assemble %1 %%a
for %%a in (*.c) do call :compile %1 %%a
if exist _m3main.c call :make_exe %1
popd
goto :eof

:assemble
echo echo %AS% %1/%2 -o obj/%2.obj >> \%BOOT%\makeverbose.sh
echo %AS% %1/%2 -o obj/%2.obj >> \%BOOT%\makeverbose.sh
echo OBJS += obj/%2.obj >> \%BOOT%\Makefile
echo obj/%2.obj: %1/%2>> \%BOOT%\Makefile
call :EchoTab "	$(AS) %1/%2 -o obj/%2.obj" >> \%BOOT%\Makefile
goto :eof

:compile
echo echo %CC% -c -I%1 %1/%2 -o obj/%2.obj >> \%BOOT%\makeverbose.sh
echo %CC% -c -I%1 %1/%2 -o obj/%2.obj >> \%BOOT%\makeverbose.sh
echo OBJS += obj/%2.obj >> \%BOOT%\Makefile
echo obj/%2.obj: %1/%2>> \%BOOT%\Makefile
call :EchoTab "	$(CC) -c -I%1 %1/%2 -o obj/%2.obj" >> \%BOOT%\Makefile
goto :eof

:make_exe
echo echo %CC% -o exe/%1 obj/*.obj -lm -lpthread >> \%BOOT%\makeverbose.sh
echo %CC% -o exe/%1 obj/*.obj -lm -lpthread >> \%BOOT%\makeverbose.sh
echo exe/cm3: $(OBJS) >> \%BOOT%\Makefile >> \%BOOT%\Makefile
call :EchoTab "	$(CC) -o exe/cm3 obj/*.obj -lm -lpthread" >> \%BOOT%\Makefile
goto :eof

:EchoTab
SetLocal
set a=%1
echo %a:"=%
EndLocal
goto :eof
