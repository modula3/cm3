#!/bin/sh

CVSEXE=/opt/csw/cvs-feature/bin/cvs

if [ ! -x $CVSEXE ]; then
  CVSEXE=/opt/csw/bin/cvs
fi
if [ ! -x $CVSEXE ]; then
  echo "$CVSEXE not executable" 1>&2
  exit 1
fi
if [ `uname -n` = "current10x" ]; then
  #args=`echo "$@" | sed -e 's/-z3 update/update/'`
  args=''
  for p in "$@"; do
    if [ "x$p" = x-z3 ]; then
      if echo "$@" | grep -- "-z3 update" >/dev/null 2>/dev/null; then
        true
      else
        args="$args $p"
      fi
    else
      args="$args $p"
    fi
  done
  echo "$CVSEXE $args"
  exec $CVSEXE $args
else
  echo "$CVSEXE $@"
  exec $CVSEXE "$@"
fi
