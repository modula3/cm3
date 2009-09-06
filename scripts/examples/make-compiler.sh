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
    CM3_VERSION=2009-02-01
fi

#
# config files come from LATEST_SOURCE
# initial cm3/cm3cg come from BOOT
#

LATEST_SOURCE=/dev2/cm3
SOURCE=/dev2/cm3.$CM3_VERSION
INSTALL=/cm3.$CM3_VERSION
BOOT=/cm3
CM3_FLAGS=-DROOT=$SOURCE
TARGET=I386_DARWIN

CM3_TARGET=$TARGET
export CM3_TARGET

#
# workaround buggy text
#

if [ "x$CM3_VERSION" = "x2009-02-16-02-00" ]; then
  cp $LATEST_SOURCE/m3-libs/m3core/src/text/*3 $SOURCE/m3-libs/m3core/src/text
fi

#
# functions
#

copy_exe() {
  a=`basename $1`
  rm -f $2/$a $2/$a.exe
  if [ -f $1.exe ]; then
    cp $1.exe $2
    exit 0
  fi
  cp $1 $2
}

clean() {
    cd $SOURCE/$1 || exit 1
    rm -rf $TARGET
}

buildlocal() {
	cd $SOURCE/$1 || exit 1
	type cm3 || exit 1
	cm3 -x $CM3_FLAGS || exit 1
}

buildship() {
	cd $SOURCE/$1 || exit 1
	type cm3 || exit 1
	cm3 $CM3_FLAGS || exit 1
	cm3 $CM3_FLAGS -ship || exit 1
}


#for a in $LATEST_SOURCE $SOURCE; do
#  if [ -d $a/m3-sys ]; then : else
#    echo "$a/m3-sys does not exist"
#    exit 1
#  fi
#done

#
# create install directories
#

mkdir -p $INSTALL/bin/config || exit 1

#
# set PATH
#

PATH="$INSTALL/bin:$PATH"
export PATH


#
# create config files from current source
#

echo "
  INSTALL_ROOT = path() & \"/..\"
  include(path() & \"/config/$TARGET\")
" > $INSTALL/bin/cm3.cfg

for a in $LATEST_SOURCE/m3-sys/cminstall/src/config-no-install/*; do
  if [ -f $a ]; then
    cp $a $INSTALL/bin/config
  fi
done
rm $INSTALL/bin/config/cm3.cfg

#
# copy over cm3, cm3cg, mklib
#

for a in cm3 cm3cg mklib; do
  copy_exe $BOOT/bin/$a $INSTALL/bin
done

#
# build cm3cg
#

rm -rf $INSTALL/pkg
#clean m3-sys/m3cc || exit 1
buildship m3-sys/m3cc || exit 1
copy_exe $SOURCE/m3-sys/m3cc/$TARGET/cm3cg $INSTALL/bin

#
# build up to cm3 and mklib
#

p=" m3-libs/m3core \
    m3-libs/libm3 \
    m3-libs/sysutils \
    m3-sys/m3middle \
    m3-sys/m3quake \
    m3-sys/m3front \
    m3-sys/m3linker \
    m3-sys/m3objfile \
    m3-sys/m3back \
    m3-sys/cm3 \
    m3-sys/mklib"

#
# Do it twice so it builds itself.
#

rm -rf $INSTALL/pkg
for a in $p; do clean $a || exit 1; done
for a in $p; do buildlocal $a || exit 1; done
for a in cm3 mklib; do
  copy_exe $SOURCE/m3-sys/$a/$TARGET/$a $INSTALL/bin
done

rm -rf $INSTALL/pkg
for a in $p; do clean $a || exit 1; done
for a in $p; do buildship $a || exit 1; done
for a in cm3 mklib; do
  copy_exe $SOURCE/m3-sys/$a/$TARGET/$a $INSTALL/bin
done
