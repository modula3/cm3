SetLocal

pushd %~dp0
python .\upgrade.py %* || goto :eof
python .\do-cm3-all.py realclean skipgcc %* || goto :eof
python .\do-cm3-all.py buildship %* || goto :eof
