#!/bin/sh
# $Id$

if [ -n "$ROOT" -a -d "$ROOT" ] ; then
  sysinfo="$ROOT/scripts/sysinfo.sh"
  root="${ROOT}"; export root
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

if [ ! -f "$PKGSDB" ] ; then
   echo "making $PKGSDB (slow but rare)"
  . "$ROOT/scripts/find-packages.sh"
fi

if [ ! -f "$PKGSDB" ] ; then
  echo "cannot generate package list" 1>&2
  exit 1
fi

UsePackage()
{
  case "$1" in
    tcl) [ "${HAVE_TCL}" = yes ] ;;
    serial) [ "${HAVE_SERIAL}" = yes ] ;;
    tapi) [ "${M3OSTYPE}" = "WIN32" ] ;;
    X11R4) [ "${M3OSTYPE}" != "WIN32" ] ;;
    m3cc) [ "${GCC_BACKEND}" = yes ] && [ "${OMIT_GCC}" = "" ] ;;
    m3gdb)
      ([ "${M3GDB}" = yes ] || [ "${CM3_GDB}" = yes ]) \
         && [ "${TARGET}" != "ARM_DARWIN" ] \
         && [ "${TARGET}" != "AMD64_DARWIN" ] \
         && [ "${TARGET}" != "I386_DARWIN" ] \
         && [ "${TARGET}" != "PPC_DARWIN" ] \
         && [ "${TARGET}" != "PPC64_DARWIN" ] \
         && [ "${TARGET}" != "NT386" ] ;;

    *) true;;
  esac
}

FilterPackages()
{
  if [ "${CM3_ALL}" = yes ] ; then
    echo "$@"
  else
    Result=""
    for Package in "$@" ; do
      if UsePackage ${Package} ; then
        if [ -z "${Result}" ] ; then
          Result="${Package}"
        else
          Result="${Result} ${Package}"
        fi
      fi
    done
    echo "${Result}"
  fi
}

pkg_defined() {
  for p in $* ; do
    qgrep /$p\$ "$PKGSDB" || return 1
  done
  return 0
}

pkgpath() {
  ok=true
  for p in $* ; do
    if res=`"$GREP" /"$p\$" "$PKGSDB"` ; then
      echo $res | head -1
    else
      echo "package $p not found" 1>&2
      ok=false
    fi
  done
  if [ $ok = true ] ; then
    return 0
  else
    return 1
  fi
}

listpkgs() {
  egrep "/$a\$" "$PKGSDB"
  if [ -n "$1" ] ; then
    while [ -n "$1" ] ; do
      a="$1"
      # remove ROOT from the start of a
      a=`echo $a | sed -e "s;^${ROOT}/;;"`
      # if a has no slashes, then it needs a leading slash
      a=`echo $a | sed -e '/\//!s;^;/;'`
      egrep "$a\$" "$PKGSDB"
      shift
    done | sed -e "s;^;${ROOT}/;"
  else
    cat "$PKGSDB" | sed -e "s;^;${ROOT}/;"
  fi
}
