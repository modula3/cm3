#!/bin/sh

ROOT=${ROOT:-$(pwd)}

BUILDLOCAL="cm3 -build -override -DROOT=${ROOT}"
CLEANLOCAL="cm3 -clean -override -DROOT=${ROOT}"
SHIP="cm3 -ship -DROOT=${ROOT}"
BUILDGLOBAL="cm3 -build  -DROOT=${ROOT} && ${SHIP} -DROOT=${ROOT}"
CLEANGLOBAL="cm3 -clean -DROOT=${ROOT}"

CMD="${BUILDLOCAL}"
ACTION="build"

usage() {
  ME=$(basename $0)
  echo "usage:"
  echo "  $ME -buildlocal|-build"
  echo "      build and import from workspace (default action)"
  echo "  $ME -buildglobal|-buildship"
  echo "      build and ship; import from system pool"
  echo "  $ME -cleanlocal|-clean"
  echo "      clean packages with imports from workspace"
  echo "  $ME -cleanglobal"
  echo "      clean packages with imports from system pool"
}

if [ -n "$1" ] ; then
  case "$1" in
    -build|-buildlocal) 	CMD="${BUILDLOCAL}"; ACTION="build"; shift;;
    -buildglobal|-buildship)	CMD="${BUILDGLOBAL}"; ACTION="build"; shift;;
    -clean|-cleanlocal)		CMD="${CLEANLOCAL}"; ACTION="clean"; shift;;
    -cleanglobal)		CMD="${CLEANGLOBAL}"; ACTION="clean"; shift;;
    -help|-h) 			usage; exit 0;;
    -*)				usage; exit 1;;
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
    if ${CMD} ; then
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
