#!/bin/sh

WIN32=${WIN32:-yes}
UNAME=`uname`

if [ "${UNAME}" = FreeBSD ] ; then
  WIN32=no
  ROOT=${ROOT:-`pwd`}
elif [ "${UNAME}" = SunOS ] ; then
  WIN32=no
  ROOT=${ROOT:-`pwd`}
fi

export GCC_BACKEND WIN32 ROOT

# base libraries and compiler
PKGS=""
[ "${WIN32}" = yes ] && PKGS="${PKGS} m3-comm/tapi"
PKGS="${PKGS} m3-comm/tcp"
PKGS="${PKGS} m3-comm/serial"
PKGS="${PKGS} m3-comm/netobj"
PKGS="${PKGS} m3-comm/netobjd"
PKGS="${PKGS} m3-tools/m3tk"
PKGS="${PKGS} m3-comm/stubgen"

echo ./pkgmap.sh $@ ${PKGS}
exec ./pkgmap.sh $@ ${PKGS}
