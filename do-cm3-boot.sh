#!/bin/sh

GCC_BACKEND=${GCC_BACKEND:-no}
WIN32=${WIN32:-yes}
M3GDB=${M3GDB:-no}
BUILDARGS="-DBOOT"
UNAME=`uname`

if [ "${UNAME}" = FreeBSD -o "${UNAME}" = SunOS ] ; then
  GCC_BACKEND=yes
  WIN32=no
  ROOT=${ROOT:-`pwd`}
  BUILDLOCAL="${BUILDLOCAL:-m3build -O -DROOT=${ROOT} ${BUILDARGS}}"
  CLEANLOCAL="${CLEANLOCAL:-m3build clean -O -DROOT=${ROOT} ${CLEANARGS}}"
  BUILDGLOBAL="${BUILDGLOBAL:-m3build -DROOT=${ROOT} ${BUILDARGS}}"
  CLEANGLOBAL="${CLEANGLOBAL:-m3build clean -DROOT=${ROOT} ${CLEANARGS}}"
  SHIP="${SHIP:-m3ship -DROOT=${ROOT} ${SHIPARGS}}"
fi

export GCC_BACKEND WIN32 M3GDB BUILDARGS ROOT
export BUILDLOCAL CLEANLOCAL BUILDGLOBAL CLEANGLOBAL SHIP

# base libraries and compiler
PKGS=""
#PKGS="${PKGS} m3-libs/m3core"
#PKGS="${PKGS} m3-libs/libm3"
PKGS="${PKGS} m3-sys/m3middle"
[ "${WIN32}" = yes ] && PKGS="${PKGS} m3-sys/m3objfile"
PKGS="${PKGS} m3-sys/m3linker"
[ "${GCC_BACKEND}" != yes ] && PKGS="${PKGS} m3-sys/m3back"
[ "${GCC_BACKEND}" != yes ] && PKGS="${PKGS} m3-sys/m3staloneback"
PKGS="${PKGS} m3-sys/m3front"
PKGS="${PKGS} m3-sys/m3quake"
[ "${GCC_BACKEND}" = yes ] && PKGS="${PKGS} m3-sys/m3cc"
PKGS="${PKGS} m3-sys/cm3"

# cm3 tools (generic)
#PKGS="${PKGS} m3-sys/m3scanner"
#PKGS="${PKGS} m3-sys/m3tools"
#PKGS="${PKGS} m3-sys/m3cgcat"
#PKGS="${PKGS} m3-sys/m3cggen"
#[ "${M3GDB}" = yes ] && PKGS="${PKGS} m3-sys/m3gdb"
#PKGS="${PKGS} m3-tools/m3bundle"

# cm3 tools (win32)
# PKGS="${PKGS} m3-sys/m3loader"
#[ "${WIN32}" = yes ] && PKGS="${PKGS} m3-sys/mklib"
#[ "${WIN32}" = yes ] && PKGS="${PKGS} m3-sys/dll2lib"
#[ "${WIN32}" = yes ] && PKGS="${PKGS} m3-sys/fix_nl"
#[ "${WIN32}" = yes ] && PKGS="${PKGS} m3-sys/libdump"

# more libraries
#PKGS="${PKGS} m3-libs/bitvector"
#PKGS="${PKGS} m3-libs/digraph"
#PKGS="${PKGS} m3-libs/dps" # needs special postscript support
#PKGS="${PKGS} m3-libs/parseparams"
#PKGS="${PKGS} m3-libs/realgeometry"
#PKGS="${PKGS} m3-libs/set"
#PKGS="${PKGS} m3-libs/slisp"
#PKGS="${PKGS} m3-libs/sortedtableextras"
#PKGS="${PKGS} m3-libs/table-list"
#PKGS="${PKGS} m3-libs/tcl" # needs tcl library
#PKGS="${PKGS} m3-libs/tempfiles"

echo ./pkgmap.sh $@ ${PKGS}
exec ./pkgmap.sh $@ ${PKGS}
