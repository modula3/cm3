#!/bin/sh

# scripts to work around broken CVS compression on current10x.opencsw.org
# in Hudson; can be used to work around other CVS problems, too

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
    if [ "x$p" = x-z3 ]; then :
    else
      args="$args \"$p\""
    fi
  done
  echo "$CVSEXE $args"
  echo exec $CVSEXE $args
else
  echo "$CVSEXE $@"
  echo exec $CVSEXE "$@"
fi
