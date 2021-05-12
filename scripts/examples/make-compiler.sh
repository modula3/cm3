#!/bin/sh

# echo everything and fail on any error

set -e
set -x

# establish some parameters

if [ "x$CM3_VERSION" = "x" ]; then
    CM3_VERSION=2009-02-01
fi

# config files come from LATEST_SOURCE
# initial cm3/cm3cg/m3core/libm3 come from BOOT

LATEST_SOURCE=/dev2/cm3
SOURCE=/dev2/cm3.$CM3_VERSION
INSTALL=/cm3.$CM3_VERSION
BOOT=/cm3
CM3_FLAGS="-DROOT=$SOURCE -DGNU_CC='*' -DGNU_CFLAGS='*'"
CM3_FLAGS="-DROOT=$SOURCE"
CM3_TARGET="${CM3_TARGET:-SOLsun}"

TARGET=$CM3_TARGET
export CM3_TARGET TARGET

# workaround buggy text

if [ "x$CM3_VERSION" = "x2009-02-16-02-00" ]; then
  cp $LATEST_SOURCE/m3-libs/m3core/src/text/*3 $SOURCE/m3-libs/m3core/src/text
fi

if [ "x$CM3_VERSION" = "x2009-02-16-02-30" ]; then
  cp $LATEST_SOURCE/m3-libs/m3core/src/text/*3 $SOURCE/m3-libs/m3core/src/text
fi

# functions

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

# create install directories

mkdir -p $INSTALL/bin/config || exit 1

# set PATH

PATH="$INSTALL/bin:$PATH"
export PATH

# create config files from current source
# old compilers don't like my GetM3Back function, so
# make it simple

echo "
INSTALL_ROOT = path() & \"/..\"
m3back = \"@\" & INSTALL_ROOT & \"/bin/cm3cg\"
include(path() & \"/config/$TARGET\")
" > $INSTALL/bin/cm3.cfg

for a in $LATEST_SOURCE/m3-sys/cminstall/src/config-no-install/*; do
  if [ -f $a ]; then
    cp $a $INSTALL/bin/config
  fi
done
rm $INSTALL/bin/config/cm3.cfg

# copy over cm3, cm3cg, m3core, libm3

for a in cm3 cm3cg; do
  copy_exe $BOOT/bin/$a $INSTALL/bin
done
rm -rf $INSTALL/pkg
mkdir $INSTALL/pkg
for a in m3core libm3 sysutils; do
  cp -pR $BOOT/pkg/$a $INSTALL/pkg/$a
done

# build cm3cg, maybe
# It isn't working, it is slow, and it isn't likely relevant to current investigation.

rm -f $SOURCE/m3-sys/m3cc/$TARGET/cm3cg*
rm -f $SOURCE/m3-sys/m3cc/$TARGET/gcc/m3cgc1*
#rm -rf $INSTALL/pkg
#clean m3-sys/m3cc || exit 1
#buildship m3-sys/m3cc || exit 1
#copy_exe $SOURCE/m3-sys/m3cc/$TARGET/cm3cg $INSTALL/bin

# build up to cm3
# Do it twice so it builds itself.
# the first time, skip m3core, libm3

p=" m3-sys/m3middle \
    m3-sys/m3quake \
    m3-sys/m3front \
    m3-sys/m3linker \
    m3-sys/m3objfile \
    m3-sys/m3back \
    m3-sys/cm3"

q=" m3-libs/m3core \
    m3-libs/libm3 \
    m3-libs/sysutils \
    m3-libs/set \
    $p"

for a in $q; do clean $a || exit 1; done
for a in $p; do buildship $a || exit 1; done
for a in cm3; do
  copy_exe $SOURCE/m3-sys/$a/$TARGET/$a $INSTALL/bin
done

rm -rf $INSTALL/pkg
for a in $q; do clean $a || exit 1; done
for a in $q; do buildship $a || exit 1; done
for a in cm3; do
  copy_exe $SOURCE/m3-sys/$a/$TARGET/$a $INSTALL/bin
done

echo "success"
