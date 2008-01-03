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
if [ "$1" = -f ] ; then
  FORCE=yes
  shift
else
  FORCE=no
fi
if [ -z "$1" ] ; then
  echo "please specify a cross compilation target platform" 1>&2
  exit 1
fi
BUILDARGS="-DM3CC_HOST=${TARGET} -DM3CC_TARGET=$1"
if [ "$FORCE" = yes ] ; then
  BUILDARGS="${BUILDARGS} -DFORCE=1"
fi
. "$ROOT/scripts/pkginfo.sh"
. "$ROOT/scripts/pkgcmds.sh"

USAGE="
  `basename $0` [-f] M3_CROSS_TARGET

    builds a cm3cg backend for target platform M3_CROSS_TARGET

  options:

    -f     force distclean before compilation
"

show_usage $@

shift

OPTIONS=`extract_options $@`
ACTION=`IGNORE_MISS=yes map_action $@`
ADDARGS=`add_action_opts $@`
P=`pkgpath m3cc`

if [ "$FORCE" = yes ] ; then
  (cd ${ROOT}/${P}/gcc && test -f Makefile && gmake distclean)
  (cd ${ROOT}/${P}/gcc/gcc && rm -f cm3cg)
fi
echo "$ROOT/scripts/pkgmap.sh" ${OPTIONS} ${ADDARGS} -c \""${ACTION}"\" ${P}
"$ROOT/scripts/pkgmap.sh" ${OPTIONS} ${ADDARGS} -c "${ACTION}" ${P}
