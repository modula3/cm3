#!/bin/sh
# $Id: upgrade.sh,v 1.1 2003-07-19 18:37:22 wagner Exp $

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

USAGE="
  `basename $0` [ generic_options ] [ generic_cmd ]

  builds a new compiler with possibly different target platforms from a 
  new set of sources, using an existing compiler of an older version.

  generic_options:
${GEN_OPTS}"
  
show_usage $@

OPTIONS=`extract_options $@`
#ACTION=`map_action $@`
#ADDARGS=`add_action_opts $@`

echo "$ROOT/scripts/boot-cm3-with-m3.sh" "$@" "buildship"
. "$ROOT/scripts/boot-cm3-with-m3.sh" "$@" "buildship"

echo "$ROOT/scripts/install-cm3-compiler.sh" $OPTIONS upgrade
"$ROOT/scripts/install-cm3-compiler.sh" $OPTIONS upgrade

echo "$ROOT/scripts/do-cm3-core.sh" "$@" "buildship"
. "$ROOT/scripts/do-cm3-core.sh" "$@" "buildship"

echo "$ROOT/scripts/install-cm3-compiler.sh" $OPTIONS upgrade
"$ROOT/scripts/install-cm3-compiler.sh" $OPTIONS upgrade
