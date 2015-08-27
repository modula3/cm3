#!/bin/sh

set -e
set -x

./make-dist-cfg.py
./do-pkg.py m3cc buildship
./do-cm3-all.py realclean skipgcc
./do-pkg.py m3cc m3core libm3 buildship
./upgrade.py skipgcc
./do-cm3-all.py realclean skipgcc
./do-cm3-all.py buildship
