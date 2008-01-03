#!/bin/sh
# $Id: do-cm3-caltech-parser.sh,v 1.1 2001-09-19 15:40:10 wagner Exp $

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
P="${P} cit_common"
P="${P} m3tmplhack"
P="${P} cit_util"
P="${P} term"
P="${P} drawcontext"
P="${P} hack"
P="${P} m3browserhack"
P="${P} paneman"
P="${P} paneman/kemacs"
P="${P} parserlib/ktoklib"
P="${P} parserlib/klexlib"
P="${P} parserlib/ktok"
P="${P} parserlib/klex"
P="${P} parserlib/kyacc"
P="${P} parserlib/kext"
P="${P} parserlib/parserlib"
P="${P} parserlib/parserlib/test"

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

