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
. "$ROOT/scripts/pkgcmds.sh"

P=""
P="${P} m3core"
P="${P} libm3"
P="${P} m3middle"
P="${P} m3quake"
P="${P} m3scanner"
P="${P} m3tools"
P="${P} m3cgcat"
P="${P} m3cggen"
#[ "${M3GDB}" = yes ] && P="${P} m3gdb"
P="${P} m3bundle"
[ "${OSTYPE}" = "WIN32" ] && P="${P} mklib"
[ "${OSTYPE}" = "WIN32" ] && P="${P} dll2lib"
[ "${OSTYPE}" = "WIN32" ] && P="${P} fix_nl"
[ "${OSTYPE}" = "WIN32" ] && P="${P} libdump"
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
P="${P} tcp"
[ "${OSTYPE}" = "WIN32" ] && P="${P} tapi"
[ "${HAVE_SERIAL}" = "yes" ] && P="${P} serial"

USAGE="
  `basename $0` [ generic_options ] [ generic_cmd ]

  will apply the given symbolic command to the following packages:

`print_list4 ${P}`

  generic_options:
${GEN_OPTS}
  
  generic_cmd:
${GEN_CMDS}"

show_usage $@

OPTIONS=`extract_options $@`
ACTION=`map_action $@`
ADDARGS=`add_action_opts $@`

echo "$ROOT/scripts/pkgmap.sh" ${OPTIONS} ${ADDARGS} -c \""${ACTION}"\" ${P}
"$ROOT/scripts/pkgmap.sh" ${OPTIONS} ${ADDARGS} -c "${ACTION}" ${P}

