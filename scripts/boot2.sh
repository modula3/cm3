#!/bin/sh

set -e
set -x

OMIT_GCC=yes ./do-cm3-all.sh realclean
./do-pkg.sh m3cc m3core libm3 buildship
OMIT_GCC=yes ./upgrade.sh
OMIT_GCC=yes ./do-cm3-all.sh realclean
./do-cm3-all.sh buildship
