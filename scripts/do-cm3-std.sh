#!/bin/sh
# $Id: do-cm3-std.sh,v 1.3 2001-02-13 17:40:48 wagner Exp $

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
# base libraries
P="${P} m3core"
P="${P} libm3"

# system / compiler libraries and tools
P="${P} m3middle"
P="${P} m3quake"
P="${P} m3scanner"
P="${P} m3tools"
P="${P} m3cgcat"
P="${P} m3cggen"
#[ "${M3GDB}" = yes ] && P="${P} m3gdb" # not yet tested anywhere
P="${P} m3bundle"
[ "${M3OSTYPE}" = "WIN32" ] && P="${P} mklib"
[ "${M3OSTYPE}" = "WIN32" ] && P="${P} dll2lib"
[ "${M3OSTYPE}" = "WIN32" ] && P="${P} fix_nl"
[ "${M3OSTYPE}" = "WIN32" ] && P="${P} libdump"

# more useful quasi-standard libraries
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

# some CM3 communication extensions
[ "${M3OSTYPE}" = "WIN32" ] && P="${P} tapi"
[ "${HAVE_SERIAL}" = "yes" ] && P="${P} serial"

# network objects -- distributed programming
P="${P} netobj"
P="${P} netobjd"
P="${P} m3tk"
P="${P} stubgen"

# the standard graphical user interface: trestle and formsvbt
[ "${M3OSTYPE}" != "WIN32" ] && P="${P} X11R4"
P="${P} ui"
P="${P} vbtkit"
P="${P} tcp"
P="${P} cmvbt"
P="${P} jvideo"
P="${P} videovbt"
P="${P} web"
P="${P} formsvbtpixmaps"
P="${P} formsvbt"
P="${P} formsview"
P="${P} formsedit"

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

