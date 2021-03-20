@rem .\make-dist-cfg.py || goto :eof
@rem .\do-cm3-all.py realclean || goto :eof
@rem .\do-pkg.py m3core libm3 buildship || goto :eof
@rem .\upgrade.py skipgcc || goto :eof
@rem .\do-cm3-all.py realclean skipgcc || goto :eof
@rem .\do-cm3-all.py buildship || goto :eof

echo ERROR: Use boot2.py instead of boot2.cmd.
exit /b 1
