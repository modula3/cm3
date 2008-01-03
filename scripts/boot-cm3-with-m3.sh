#!/bin/sh
# $Id: boot-cm3-with-m3.sh,v 1.6 2001-02-16 09:52:25 wagner Exp $

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

if m="`type m3build 2>/dev/null`" ; then
  if echo $m | grep pm3 >/dev/null 2>/dev/null ; then
    IBUILDARGS="-DBOOT -DPM3"
  else
    IBUILDARGS="-DBOOT"
  fi
else
  IBUILDARGS="-DBOOT"
fi
BUILDARGS="${BUILDARGS:-${IBUILDARGS}}"

export BUILDARGS

. "$sysinfo"
. "$ROOT/scripts/pkginfo.sh"
. "$ROOT/scripts/pkgcmds.sh"

P=""
P="${P} m3middle"
[ "${M3OSTYPE}" = "WIN32" ] && P="${P} m3objfile"
P="${P} m3linker"
[ "${GCC_BACKEND}" != yes ] && P="${P} m3back"
[ "${GCC_BACKEND}" != yes ] && P="${P} m3staloneback"
P="${P} m3front"
P="${P} m3quake"
[ "${GCC_BACKEND}" = yes ] && P="${P} m3cc"
P="${P} cm3"
echo ${BUILDARGS} | grep PM3 >/dev/null 2>/dev/null || P="${P} cminstall"
[ "${M3OSTYPE}" = "WIN32" ] && P="${P} mklib"

USAGE="
  `basename $0` [ generic_options ] [ generic_cmd ]

  will apply the given symbolic command to the set of packages needed to
  compile the CM3 compiler itself using another existing M3 compiler:

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

