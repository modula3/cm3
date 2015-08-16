#!/bin/sh

set -ex
/cm3/bin/cm3 \
    -DROOT=/dev2/cm3 \
    -DTARGET=PA64_HPUX \
    -DSYSTEM_AS=$HOME/hppa64-hpux11/bin/hppa64-hpux11-as \
    -keep \
    -boot \
    -x \
    $@
