#!/bin/sh
# $Id$

if [ -n "$ROOT" -a -d "$ROOT" ] ; then
  sysinfo="$ROOT/scripts/sysinfo.sh"
else
  root=`pwd`
  while [ -n "$root" -a ! -f "$root/scripts/sysinfo.sh" ] ; do
    root=`dirname $root`
  done
  sysinfo="$root/scripts/sysinfo.sh"
  if [ ! -f "$sysinfo" ] ; then
    echo "scripts/sysinfo.sh not found" 1>&2
    exit 1
  fi
  export root
fi

. "$sysinfo"
. "$ROOT/scripts/pkginfo.sh"

RSYNC=${RSYNC:-rsync}
DEST=${DEST:-lamancha.opendarwin.org:work/cm3}

if [ -z "$1" ] ; then
  echo "please specify a cross compilation target platform" 1>&2
  exit 1
fi

CROSS_TARGET="$1"
shift
if [ -n "$1" ] ; then
  PKGS="$@"
else
  PKGS=""
fi

P=""
P="${P} m3-libs/m3gc-simple"
P="${P} m3-libs/m3gc-enhanced"
P="${P} m3-libs/m3core"
P="${P} m3-libs/libm3"
P="${P} m3-sys/m3middle"
[ "${M3OSTYPE}" = "WIN32" ] && P="${P} m3-sys/m3objfile"
P="${P} m3-sys/m3linker"
[ "${GCC_BACKEND}" != yes ] && P="${P} m3-sys/m3back"
[ "${GCC_BACKEND}" != yes ] && P="${P} m3-sys/m3staloneback"
P="${P} m3-sys/m3front"
P="${P} m3-sys/m3quake"
P="${P} m3-sys/cm3"
#P="${P} m3scanner"
#P="${P} m3tools"
#P="${P} m3cgcat"
#P="${P} m3cggen"
#[ "${M3GDB}" = yes ] && P="${P} m3gdb"
P="${P} m3-tools/m3bundle"
#[ "${M3OSTYPE}" = "WIN32" ] && P="${P} mklib"
#[ "${M3OSTYPE}" = "WIN32" ] && P="${P} dll2lib"
#[ "${M3OSTYPE}" = "WIN32" ] && P="${P} fix_nl"
#[ "${M3OSTYPE}" = "WIN32" ] && P="${P} libdump"
#P="${P} bitvector"
#P="${P} digraph"
#P="${P} parseparams"
#P="${P} realgeometry"
#P="${P} set"
#P="${P} slisp"
#P="${P} sortedtableextras"
#P="${P} table-list"
#P="${P} tempfiles"

if [ -n "${PKGS}" ] ; then
  res=""
  for s in ${PKGS}; do
    for p in ${P}; do
      case ${p} in
        *${s}*) res="${res} ${p}";; # echo "res = ${res}";;
      esac
    done
  done
  P="${res}"
fi

for p in ${P}; do
  echo ${RSYNC} -avz ${ROOT}/${p}/${CROSS_TARGET}/ ${DEST}/PPC_DARWIN/${p}/${CROSS_TARGET}/
  ${RSYNC} -avz ${ROOT}/${p}/${CROSS_TARGET}/ ${DEST}/PPC_DARWIN/${p}/${CROSS_TARGET}/
done

