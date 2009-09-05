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
    m3-libs/m3tk-misc \
    m3-sys/m3scanner \
    m3-sys/m3tools \
    m3-tools/m3scan \
    m3-tools/m3bundle \
    m3-tools/m3tk \
    m3-tools/mtex \
    m3-comm/tcp \
    m3-comm/netobj \
    m3-comm/stubgen \
    m3-ui/ui \
    m3-ui/vbtkit \
    m3-www/web \
    m3-ui/jvideo \
    m3-ui/videovbt \
    m3-ui/formsvbtpixmaps \
    m3-ui/formsvbt \
    m3-ui/juno-2/juno-machine \
    m3-ui/juno-2/juno-compiler \
    m3-ui/juno-2/juno-app/pkl-fonts \
    m3-ui/juno-2/juno-app

for a in $p; do clean $a || exit 1; done
for a in $p; do buildship $a || exit 1; done
