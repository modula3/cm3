#!/bin/sh
# $Id: pack-crossbuild.sh,v 1.2 2003-01-26 23:43:48 wagner Exp $

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
if [ -z "$1" ] ; then
  echo "please specify a cross compilation target platform" 1>&2
  exit 1
fi
CROSS_TARGET=$1
BUILDARGS=''
M3CONFIG=${ROOT}/m3-sys/cm3/src/config/${CROSS_TARGET}
export M3CONFIG
. "$ROOT/scripts/pkginfo.sh"
. "$ROOT/scripts/pkgcmds.sh"

P=""
P="${P} m3gc-simple"
P="${P} m3gc-enhanced"
P="${P} m3core"
P="${P} libm3"
P="${P} m3middle"
[ "${M3OSTYPE}" = "WIN32" ] && P="${P} m3objfile"
P="${P} m3linker"
[ "${GCC_BACKEND}" != yes ] && P="${P} m3back"
[ "${GCC_BACKEND}" != yes ] && P="${P} m3staloneback"
P="${P} m3front"
P="${P} m3quake"
P="${P} cm3"
P="${P} m3scanner"
P="${P} m3tools"
P="${P} m3cgcat"
P="${P} m3cggen"
[ "${M3GDB}" = yes ] && P="${P} m3gdb"
P="${P} m3bundle"
[ "${M3OSTYPE}" = "WIN32" ] && P="${P} mklib"
[ "${M3OSTYPE}" = "WIN32" ] && P="${P} dll2lib"
[ "${M3OSTYPE}" = "WIN32" ] && P="${P} fix_nl"
[ "${M3OSTYPE}" = "WIN32" ] && P="${P} libdump"
P="${P} bitvector"
P="${P} digraph"
P="${P} parseparams"
P="${P} realgeometry"
P="${P} set"
P="${P} slisp"
P="${P} sortedtableextras"
P="${P} table-list"
P="${P} tempfiles"
[ "${HAVE_TCL}" = "yes" ] && P="${P} tcl"

USAGE="
  `basename $0` cross_target

  will archive the following cross-compiled packages:

`print_list4 ${P}`

  The archives will be saved in a cm3/CROSS_TARGET.
"

show_usage $@
shift

if [ ! -d "${ROOT}/${CROSS_TARGET}" ] ; then
  mkdir -p "${ROOT}/${CROSS_TARGET}"
fi

for p in ${P}; do
  ${ROOT}/scripts/archive-pkg.sh ${CROSS_TARGET} ${p}
done

