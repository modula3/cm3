#!/bin/sh

if [ "x$CM3_VERSION" = "x" ]; then
    CM3_VERSION=2009-05-01
fi

CM3_ROOT=/dev2/cm3.$CM3_VERSION
CM3=/cm3/$CM3_VERSION/bin/cm3
CM3_FLAGS=-DROOT=$CM3_ROOT
TARGET=I386_DARWIN

PATH=$CM3_ROOT/bin:/cm3/bin:$PATH
export PATH

clean() {
    cd $CM3_ROOT/$1 || exit 1
    rm -rf $TARGET
}

buildship() {
	cd $CM3_ROOT/$1 || exit 1
	$CM3 $CM3_FLAGS || exit 1
	$CM3 $CM3_FLAGS -ship || exit 1
}

p=\
    m3-sys/m3cc

    rm -rf $CM3_ROOT/pkg
    for a in $p; do clean $a || exit 1; done
    for a in $p; do buildship $a || exit 1; done
    cp $CM3_ROOT/m3-sys/m3cc/$TARGET/cm3cg `dirname $CM3`/bin

p=\
    m3-libs/m3core \
    m3-libs/libm3 \
    m3-libs/sysutils \
    m3-sys/m3middle \
    m3-sys/m3quake \
    m3-sys/m3front \
    m3-sys/m3linker \
    m3-sys/m3objfile \
    m3-sys/m3back \
    m3-sys/cm3 \
    m3-sys/mklib

#
# Do it twice so it builds itself.
#

for b in 1 2; do
    rm -rf $CM3_ROOT/pkg
    for a in $p; do clean $a || exit 1; done
    for a in $p; do buildship $a || exit 1; done
    cp $CM3_ROOT/m3-sys/cm3/$TARGET/cm3 `dirname $CM3`/bin
    cp $CM3_ROOT/m3-sys/mklib/$TARGET/mklib `dirname $CM3`/bin
done
