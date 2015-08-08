#!/bin/sh

set -e
set -x

./upgrade.py $*
./do-cm3-all.py realclean skipgcc $*
./do-cm3-all.py buildship $*
