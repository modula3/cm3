SetLocal

pushd %~dp0
.\upgrade.py %* || goto :eof
.\do-cm3-all.py realclean skipgcc %* || goto :eof
.\do-cm3-all.py buildship %* || goto :eof
