#!/bin/sh
# $Id: find-packages.sh,v 1.3 2001-02-12 22:39:00 wagner Exp $

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

(
  cd "$ROOT" &&  find . -type f -name m3makefile -print | 
  grep src/m3makefile | sed -e 's;/src/m3makefile$;;' | 
  sort | uniq | sed -e "s;^./;;"
) > "$PKGSDB"

