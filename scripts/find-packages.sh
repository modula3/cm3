#!/bin/sh
# $Id: find-packages.sh,v 1.8 2009-03-07 18:44:33 jkrell Exp $

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
  cd "$ROOT" &&  find . -follow -type f -name m3makefile -print | 
  grep /src/m3makefile | grep -v examples | grep -v _darcs | sed -e 's;/src/m3makefile$;;' | 
  sort | uniq | sed -e "s;^./;;"
) > "$PKGSDB"

