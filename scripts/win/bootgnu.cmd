@rem $Id$

@setlocal

@rem check that cl and link are in path
if not defined CM3_ROOT (
  echo ERROR
  goto :eof
)
rmdir /q/s \cm3
xcopy /fivery \cm3-min-WIN32-NT386-5.1.3 \cm3
del \cm3\bin\cm3.cfg
copy CM3_ROOT\m3-sys\cminstall\src\config\cm3.cfg \cm3\bin\cm3.cfg
cd %CM3_ROOT%\scripts\python
set CM3_TARGET=NT386
upgrade || exit /b 1
do-cm3-std realclean || exit /b 1
do-cm3-front realclean || exit /b 1
@rem need do-cm3-all

@rem check that sh, ld, as, gcc, sed, make, etc. are in path

set PATH=%SystemDrive%\msys\1.0\bin;%SystemDrive%\mingw\bin;%PATH%
set CM3_TARGET=NT386GNU
do-cm3-std realclean || exit /b 1
do-cm3-front realclean || exit /b 1
do-pkg realclean m3cc || exit /b 1
do-pkg buildship m3cc || exit /b 1
do-cm3-front buildship || exit /b 1
