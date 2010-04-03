rmdir /q/s \cm3
xcopy /fiverdy \net\mod3\cm3-min-WIN32-NT386-5.1.3a \cm3

rem call \dev2\j\env\cm3\cm3.vc20
rem .\do-cm3-all.py realclean
rem upgrade.py || goto :eof
rem make-dist.py || goto :eof

rem call \dev2\j\env\cm3\cm3.vc40
rem .\do-cm3-all.py realclean
rem upgrade.py || goto :eof
rem make-dist.py || goto :eof

rem call \dev2\j\env\cm3\cm3.vc41
rem .\do-cm3-all.py realclean
rem upgrade.py || goto :eof
rem make-dist.py || goto :eof

rem call \dev2\j\env\cm3\cm3.vc42
rem .\do-cm3-all.py realclean
rem upgrade.py || goto :eof
rem make-dist.py || goto :eof

call \dev2\j\env\cm3\cm3.vc50
.\do-cm3-all.py realclean
upgrade.py || goto :eof
make-dist.py || goto :eof

call \dev2\j\env\cm3\cm3.vc60
.\do-cm3-all.py realclean
upgrade.py || goto :eof
make-dist.py || goto :eof

call \dev2\j\env\cm3\cm3.vc70
.\do-cm3-all.py realclean
upgrade.py || goto :eof
make-dist.py || goto :eof

call \dev2\j\env\cm3\cm3.vc71
.\do-cm3-all.py realclean
upgrade.py || goto :eof
make-dist.py || goto :eof

call \dev2\j\env\cm3\cm3.vc80
.\do-cm3-all.py realclean
upgrade.py || goto :eof
make-dist.py || goto :eof

call \dev2\j\env\cm3\cm3.vc90
.\do-cm3-all.py realclean
upgrade.py || goto :eof
make-dist.py || goto :eof
