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

export WIN32 ROOT

# base libraries and compiler
PKGS=""
[ "${WIN32}" != yes ] && PKGS="${PKGS} m3-ui/X11R4"
PKGS="${PKGS} m3-ui/ui"
PKGS="${PKGS} m3-ui/vbtkit"
PKGS="${PKGS} m3-comm/tcp"
PKGS="${PKGS} m3-ui/cmvbt"
PKGS="${PKGS} m3-ui/jvideo"
PKGS="${PKGS} m3-ui/videovbt"
PKGS="${PKGS} m3-www/web"
PKGS="${PKGS} m3-ui/formsvbtpixmaps"
PKGS="${PKGS} m3-ui/formsvbt"
PKGS="${PKGS} m3-ui/formsview"
PKGS="${PKGS} m3-ui/formsedit"

#[ "${WIN32}" = yes ] && PKGS="${PKGS} m3-comm/tapi"

echo ./pkgmap.sh $@ ${PKGS}
exec ./pkgmap.sh $@ ${PKGS}
