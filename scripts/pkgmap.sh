#!/bin/sh
# $Id$

#set -x
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

exec_cmd() {
  echo " +++ $PKG_ACTION +++"
  [ "$NO_ACTION" = yes ] || /bin/sh -c "cd $1 && $PKG_ACTION"
}

PKGS=""
while [ -n "$1" ] ; do
  if [ x-k = x"$1" ] ; then
    KEEP_GOING="yes"
  elif [ x-n = x"$1" ] ; then
    NO_ACTION="yes"
  elif [ x-c = x"$1" ] ; then
    if [ -z "${PKG_ACTION}" ] ; then
      PKG_ACTION="$2"
    else
      PKG_ACTION="${PKG_ACTION} ; $2"
    fi
    shift
  elif [ -d "$1" ] ; then
    PKGS="${PKGS} $1"
  elif [ -d "$ROOT/$1" ] ; then
    PKGS="${PKGS} $ROOT/$1"
  else
    p=`pkgpath $1`
    if [ -d "$p" ] ; then
      PKGS="${PKGS} $p"
    elif [ -n "$p" -a -d "$ROOT/$p" ] ; then
      PKGS="${PKGS} $ROOT/$p"
    else
      echo " *** cannot find package $1 / $p" 1>&2
      exit 1
    fi
  fi
  shift
done

if [ -z "$PKG_ACTION" ] ; then
  echo "no PKG_ACTION defined, aborting" 1>&2
  exit 1
fi

if [ -z "${PKGS}" ] ; then
  echo "no packages" 1>&2
  exit 1
fi

for PKG in ${PKGS} ; do
  echo " === package ${PKG} ==="
  exec_cmd "$PKG"
  res=$?
  if [ "$res" != "0" ] ; then
    if [ "${KEEP_GOING}" != "yes" ] ; then
      echo " *** execution of $ACTION failed ***" 
      exit 1
    fi
  fi
  if [ "${KEEP_GOING}" = "yes" ] ; then
    echo " ==> $PKG_ACTION returned $res"
  else
    echo " ==> ${PKG} done"
  fi
  echo ""
done
