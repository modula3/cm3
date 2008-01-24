#!/bin/sh
# $Id: make-src-dist-snapshots.sh,v 1.1 2008-01-24 23:45:32 wagner Exp $

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

DS=${DS:-`date -u +'%Y-%m-%d-%H-%M-%S' | tr -d '\\n'`}

${ROOT}/make-src-dist-all.sh
${ROOT}/make-src-dist-std.sh
${ROOT}/make-src-dist-sys.sh
${ROOT}/make-src-dist-gnu.sh
