#!/bin/sh

#
# Upgrade compiler and runtime and then build everything.
#

set -e
set -x

./upgrade.py $*
./do-cm3-all.py realclean skipgcc $*
./do-cm3-all.py buildship $*
