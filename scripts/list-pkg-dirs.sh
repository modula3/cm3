#!/bin/sh
# $Id: list-pkg-dirs.sh,v 1.1 2003-01-31 19:34:18 wagner Exp $

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
  `basename $0` <pattern>*

"

show_usage "$@"

if [ "x$1" = "x-p" ] ; then
  PREFIX="$2"
  shift
  shift
else
  PREFIX="dir "
fi

for d in `listpkgs "$@" | sed -e "s;\$;/src;"`; do
  find "$d" -type d \! -name CVS -print
done | sed -e "s;^;${PREFIX};"

#echo "

