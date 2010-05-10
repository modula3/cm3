#!/bin/sh

set -e
set -x

./do-cm3-all.py realclean skipgcc
./do-pkg.py m3cc m3core libm3 buildship
./upgrade.py skipgcc
./do-cm3-all.py realclean skipgcc
./do-cm3-all.py buildship
