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
if [ -z "$1" ] ; then
  echo "please specify a cross compilation target platform" 1>&2
  exit 1
fi
CROSS_TARGET=$1
BUILDARGS="-DM3_BOOTSTRAP=TRUE -keep"
M3CONFIG_SRC=${ROOT}/m3-sys/cm3/src/config/${CROSS_TARGET}
M3CONFIG=${ROOT}/scripts/${CROSS_TARGET}.cfg
if [ ! -f "${M3CONFIG}" -o "${M3CONFIG}" -ot "${M3CONFIG_SRC}" ] ; then
  sed -e "s;m3back.*=.*;m3back = \"@${ROOT}/m3-sys/m3cc/${TARGET}-${CROSS_TARGET}/cm3cg\";" \
    ${M3CONFIG_SRC} > ${M3CONFIG}
fi
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

  will cross-compile the following packages:

`print_list4 ${P}`
"

show_usage $@
shift

OPTIONS=`extract_options $@`
ACTION=`map_action $@`
ADDARGS=`add_action_opts $@`

echo "$ROOT/scripts/pkgmap.sh" ${OPTIONS} ${ADDARGS} -c \""${ACTION}"\" ${P}
"$ROOT/scripts/pkgmap.sh" ${OPTIONS} ${ADDARGS} -c "${ACTION}" ${P}

