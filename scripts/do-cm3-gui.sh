#!/bin/sh

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
[ "${OSTYPE}" != "WIN32" ] && P="${P} X11R4"
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

OPTIONS=`extract_options $@`
ACTION=`map_action $@`
ADDARGS=`add_action_args $@`

echo "$ROOT/scripts/pkgmap.sh" ${OPTIONS} ${ADDARGS} -c \""${ACTION}"\" ${P}
"$ROOT/scripts/pkgmap.sh" ${OPTIONS} ${ADDARGS} -c "${ACTION}" ${P}

