SetLocal

pushd %~dp0
rmdir /q/s \cm3
xcopy /fiverdy \net\mod3\cm3-min-WIN32-NT386-5.1.3a \cm3

call \dev2\j\env\cm3\cm3.vc50
.\do-cm3-all.py realclean
.\upgrade.py || goto :eof

call \dev2\j\env\cm3\cm3.vc20
.\make-dist.py || goto :eof

call \dev2\j\env\cm3\cm3.vc40
.\make-dist.py || goto :eof

call \dev2\j\env\cm3\cm3.vc41
.\make-dist.py || goto :eof

call \dev2\j\env\cm3\cm3.vc42
.\make-dist.py || goto :eof

call \dev2\j\env\cm3\cm3.vc50
.\make-dist.py || goto :eof

call \dev2\j\env\cm3\cm3.vc60
.\make-dist.py || goto :eof

call \dev2\j\env\cm3\cm3.vc70
.\make-dist.py || goto :eof

call \dev2\j\env\cm3\cm3.vc71
.\make-dist.py || goto :eof

call \dev2\j\env\cm3\cm3.vc80
.\make-dist.py || goto :eof

call \dev2\j\env\cm3\cm3.vc90
.\make-dist.py || goto :eof
