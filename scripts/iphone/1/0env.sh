set -e
set -x

# This file SHOULD generally be user-edited.
# The rest shouldn't need be, not much.

export prefix=/usr/local
export prefix=/toolchain.2

# run these as root possibly

mkdir -p $prefix
chown jay:jay $prefix || true
# chown -R jay:jay $prefix || true

export iphoneip=192.168.2.10
export iphoneuser=jay
export iphonessh=$iphoneuser@$iphoneip

export target=arm-apple-darwin9
export target=arm-apple-darwin8
export sysroot=$prefix/$target/sys-root
export PATH=$prefix/bin:$PATH
export cctools=$prefix/src/cctools
export gcc=$prefix/src/gcc
export csu=$prefix/src/csu
export build=$prefix/obj

# 
export sudo=sudo
export sudo=echo sudo

# make this automatic
GMAKE=make
#GMAKE=gmake
