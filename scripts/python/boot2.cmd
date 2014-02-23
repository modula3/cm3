.\make-dist-cfg.py || goto :eof
.\do-cm3-all.py realclean || goto :eof
.\do-pkg.py m3core libm3 buildship || goto :eof
.\upgrade.py skipgcc || goto :eof
.\do-cm3-all.py realclean skipgcc || goto :eof
.\do-cm3-all.py buildship || goto :eof
