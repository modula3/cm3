#!/bin/sh

WIN32=${WIN32:-yes}

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
