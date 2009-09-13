#!/bin/sh

#
# echo everything and fail on any error
#

set -e
set -x

#
# establish some parameters
#

if [ "x$CM3_VERSION" = "x" ]; then
    CM3_VERSION=2009-05-01
fi

SOURCE=/dev2/cm3.$CM3_VERSION
INSTALL=/cm3.$CM3_VERSION
CM3_FLAGS=-DROOT=$SOURCE
TARGET=I386_DARWIN

CM3_TARGET=$TARGET
export CM3_TARGET

#
# functions
#

copy_exe() {
  a=`basename $1`
  rm -f $2/$a $2/$a.exe
  cp $1.exe $2 && exit 0
  cp $1 $2 || exit 1
}

clean() {
    cd $SOURCE/$1 || exit 1
    rm -rf $TARGET
}

buildship() {
	cd $SOURCE/$1 || exit 1
	type cm3 || exit 1
	cm3 $CM3_FLAGS || exit 1
	cm3 $CM3_FLAGS -ship || exit 1
}

PATH="$INSTALL/bin:$PATH"
export PATH
hash -r || true

p=" m3-libs/m3tk-misc \
    m3-sys/m3scanner \
    m3-sys/m3tools \
    m3-tools/m3scan \
    m3-tools/m3bundle \
    m3-tools/m3tk \
    m3-tools/mtex \
    m3-comm/tcp \
    m3-comm/netobj \
    m3-comm/stubgen \
    m3-ui/X11R4 \
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
    m3-ui/juno-2/juno-app"

for a in $p; do clean $a || exit 1; done
for a in $p; do buildship $a || exit 1; done

echo success