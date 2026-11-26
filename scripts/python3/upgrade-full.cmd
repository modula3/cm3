SetLocal

::
:: Upgrade compiler and runtime and then build everything.
::

pushd %~dp0
python3 ./upgrade.py %* || goto :eof
python3 ./do-cm3-all.py realclean skipgcc %* || goto :eof
python3 ./do-cm3-all.py buildship %* || goto :eof
