#!/bin/sh
# $Id$

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
if [ -z "$1" ] ; then
  echo "please specify a (cross) compilation target platform" 1>&2
  exit 1
fi
CROSS_TARGET=$1
BUILDARGS=''

if [ -z "$2" ] ; then
  echo "please specify a package" 1>&2
  exit 1
fi

if [ -d "$ROOT/$2" ] ; then
  PKG="$ROOT/$2"
elif [ -d "$2" ] ; then
  PKG="$2"
else
  p=`pkgpath $2`
  if [ -d "$p" ] ; then
    PKG="$p"
  elif [ -n "$p" -a -d "$ROOT/$p" ] ; then
    PKG="$ROOT/$p"
  else
    echo " *** cannot find package $2 / $p" 1>&2
    exit 1
  fi
fi

PKG_REL=`basename ${PKG}`
PKG_NAME=${PKG_REL}
PKG_REST=`dirname ${PKG}`
while [ "${PKG_REST}" != "${ROOT}" ] ; do
  PKG_REL=`basename ${PKG_REST}`/${PKG_REL}
  PKG_REST=`dirname ${PKG_REST}`
  #echo PKG_REL=${PKG_REL}
done

USAGE="
  `basename $0` target package

  will archive one (cross-)compiled package.

  The archive will be saved in a cm3/TARGET.
"

show_usage $@
shift
shift

if [ ! -d "${ROOT}/${CROSS_TARGET}" ] ; then
  mkdir -p "${ROOT}/${CROSS_TARGET}"
fi

cd "${ROOT}" || exit 1
echo "archiving ${PKG_REL} into ${ROOT}/${CROSS_TARGET}/${PKG_NAME}.tgz"
PKG_DIRS="${PKG_REL}/src ${PKG_REL}/${CROSS_TARGET}"
if [ -d "${PKG_REL}/gcc" ] ; then
  PKG_DIRS="${PKG_DIRS} ${PKG_REL}/gcc"
fi
tar czf "${ROOT}/${CROSS_TARGET}/${PKG_NAME}.tgz" ${PKG_DIRS}
ls -l "${CROSS_TARGET}/${PKG_NAME}.tgz"
