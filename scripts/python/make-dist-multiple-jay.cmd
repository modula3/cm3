SetLocal

pushd %~dp0
rmdir /q/s \cm3
xcopy /fiverdy \net\mod3\cm3-min-WIN32-NT386-5.1.3a \cm3

call \dev2\j\env\cm3\cm3.vc50
.\do-cm3-all.py realclean
set CM3_NO_SYMBOLS=1
.\upgrade.py || goto :eof
set CM3_NO_SYMBOLS=

call \dev2\j\env\cm3\cm3.vc20
.\do-cm3-all.py realclean
set CM3_NO_SYMBOLS=1
.\upgrade.py || goto :eof
set CM3_NO_SYMBOLS=
.\make-dist.py || goto :eof

call \dev2\j\env\cm3\cm3.vc40
.\do-cm3-all.py realclean
.\upgrade.py || goto :eof
.\make-dist.py || goto :eof

call \dev2\j\env\cm3\cm3.vc41
.\do-cm3-all.py realclean
.\upgrade.py || goto :eof
.\make-dist.py || goto :eof

call \dev2\j\env\cm3\cm3.vc42
.\do-cm3-all.py realclean
.\upgrade.py || goto :eof
.\make-dist.py || goto :eof

call \dev2\j\env\cm3\cm3.vc50
.\do-cm3-all.py realclean
.\upgrade.py || goto :eof
.\make-dist.py || goto :eof

call \dev2\j\env\cm3\cm3.vc60
.\do-cm3-all.py realclean
.\upgrade.py || goto :eof
.\make-dist.py || goto :eof

call \dev2\j\env\cm3\cm3.vc70
.\do-cm3-all.py realclean
.\upgrade.py || goto :eof
.\make-dist.py || goto :eof

call \dev2\j\env\cm3\cm3.vc71
.\do-cm3-all.py realclean
.\upgrade.py || goto :eof
.\make-dist.py || goto :eof

call \dev2\j\env\cm3\cm3.vc80
.\do-cm3-all.py realclean
.\upgrade.py || goto :eof
.\make-dist.py || goto :eof

call \dev2\j\env\cm3\cm3.vc90
.\do-cm3-all.py realclean
.\upgrade.py || goto :eof
.\make-dist.py || goto :eof
