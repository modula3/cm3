#!/bin/sh

ROOT=${ROOT:-$(pwd)}

BUILDLOCAL="${BUILDLOCAL:-cm3 -build -override -DROOT=${ROOT} ${BUILDARGS}}"
CLEANLOCAL="${CLEANLOCAL:-cm3 -clean -override -DROOT=${ROOT} ${CLEANARGS}}"
BUILDGLOBAL="${BUILDGLOBAL:-cm3 -build  -DROOT=${ROOT} ${BUILDARGS}}"
CLEANGLOBAL="${CLEANGLOBAL:-cm3 -clean -DROOT=${ROOT} ${CLEANARGS}}"
SHIP="${SHIP:-cm3 -ship -DROOT=${ROOT} ${SHIPARGS}}"

ACTION="build"
AREA=""

exec_cmd() {
  if [ "${ACTION}" = build ] ; then
    if [ "${AREA}" = global ] ; then
      ${BUILDGLOBAL} && ${SHIP}
    else
      ${BUILDLOCAL}
    fi
  elif [ "${ACTION}" = clean ] ; then
    if [ "${AREA}" = global ] ; then
      ${CLEANGLOBAL}
    else
      ${CLEANLOCAL}
    fi
  else
    echo "unknown action" 1>&2
    exit 1
  fi
}

usage() {
  ME=$(basename $0)
  echo "usage:"
  echo "  $ME -buildlocal|-build"
  echo "      build and import from workspace (default action)"
  echo "  $ME -buildglobal|-buildship|-ship|-install"
  echo "      build and ship; import from system pool"
  echo "  $ME -cleanlocal|-clean"
  echo "      clean packages with imports from workspace"
  echo "  $ME -cleanglobal"
  echo "      clean packages with imports from system pool"
}

if [ -n "$1" ] ; then
  case "$1" in
    -build|-buildlocal)
      ACTION="build"; AREA="local";  shift;;
    -buildglobal|-buildship|-ship|-install)
      ACTION="build"; AREA="global"; shift;;
    -clean|-cleanlocal)
      ACTION="clean"; AREA="local";  shift;;
    -cleanglobal)
      ACTION="clean"; AREA="global"; shift;;
    -help|-h)
      usage; exit 0;;
    -*)
      usage; exit 1;;
  esac
fi

while [ -n "$1" ] ; do
  PKGS="${PKGS} $1";
  shift
done

if [ -z "${PKGS}" ] ; then
  echo "no packages" 1>&2
  exit 1
fi

if [ "${ACTION}" =  "clean" ] ; then
  RPKGS=""
  for PKG in ${PKGS} ; do
    RPKGS="${PKG} ${RPKGS}"
  done
  PKGS="${RPKGS}"
fi

for PKG in ${PKGS} ; do
  echo "=== package ${PKG} ==="
  if cd ${ROOT}/${PKG} ; then
    if exec_cmd ; then
      echo " => ${PKG} done"
    else
      echo " *** ${ACTION}ing of ${PKG} failed ***" 
      [ "${ACTION}" = "clean" ] || exit 1
    fi
  else
    echo " *** package ${PKG} not found ***" 
    exit 1 
  fi
done
