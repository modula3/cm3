#!/bin/sh

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

CONFIG="${INSTALLROOT}/bin/config"
FRONTEND="${INSTALLROOT}/bin/cm3"
BACKEND="${INSTALLROOT}/bin/cm3cg"
FRONTEND_SRC="${ROOT}/m3-sys/cm3/${TARGET}/cm3"
BACKEND_SRC="${ROOT}/m3-sys/m3cc/${TARGET}/cm3cg"
CONFIG_SRC="${ROOT}/m3-sys/cminstall/src/config"

usage()
{
  echo ""
  echo "install-cm3-compiler.sh [ -n ] backup | newversion | upgrade"
  echo "install-cm3-compiler.sh [ -n ] restore <version_number>"
  echo ""
  echo "  backup"
  echo "    will make copies of cm3 front-end and back-end with the cm3"
  echo "    version number as suffix, e.g."
  echo "    ${FRONTEND} --> ${FRONTEND}-x.y.z"
  echo "    ${BACKEND} --> ${BACKEND}-x.y.z"
  echo "    ${CONFIG} --> ${CONFIG}-x.y.z"
  echo ""
  echo "  restore <version_number>"
  echo "    will restore copies with suffixed version number as current"
  echo "    version, e.g."
  echo "    ${FRONTEND}-x.y.z --> ${FRONTEND}"
  echo "    ${BACKEND}-x.y.z --> ${BACKEND}"
  echo "    ${CONFIG}-x.y.z --> ${CONFIG}"
  echo ""
  echo "  newversion"
  echo "    will install the version from the current workspace with"
  echo "    version number suffixes, e.g."
  echo "    ${FRONTEND_SRC} --> ${FRONTEND}-x.y.z"
  echo "    ${BACKEND_SRC} --> ${BACKEND}-x.y.z"
  echo "    ${CONFIG} --> ${CONFIG}-x.y.z"
  echo ""
  echo "  upgrade"
  echo "    will backup the existing version, install new executables"
  echo "    with suffixes and restore them as the current version"
  echo ""
  echo "  Beware: This script relies on the cm3 executable to correctly"
  echo "  identify its version (cm3 -version). If it does not, things will"
  echo "  get messed up."
  echo ""
}

if [ "${TARGET}" = "NT386" \
        -o "${TARGET}" = "I386_CYGWIN" \
        -o "${TARGET}" = "I386_MINGW" ]; then
  ext=".exe"
else
  ext=""
fi

exit_if()
{
  if [ "${NOACTION}" = yes ] ; then
    echo "error ignored due to -n" 1>&2
  else
    exit "$1"
  fi
}

do_cp()
{
  echo cp "$1${ext}" "$2${ext}"
  if [ "${NOACTION}" != yes ] ; then
    if cp "$1${ext}" "$2${ext}"; then
      true
    else
      exit_if 1
    fi
  fi
}

cp_if()
{
  if [ ! -r "$1${ext}" ] ; then
    echo "cp_if: source does not exist: $1" 1>&2 
    exit_if 1
  fi
  if [ -r "$2${ext}" ] ; then
    # destination exists
    if cmp "$1${ext}" "$2${ext}" >/dev/null 2>&1; then
      echo "cp_if: $1 and $2 identical" 1>&2
      true
    else
      if [ -w "$2${ext}" ] ; then
        do_cp "$1" "$2"
      else
        echo "cp_if: cannot write $2" 1>&2
        exit_if 1
      fi
    fi
  else
    do_cp "$1" "$2"
  fi
}

getversion()
{
  if [ -x "$1${ext}" ] ; then
    if "$1" -version > /dev/null; then
      "$1" -version | grep version | awk '{print $5}'
    else
      echo "$1 is broken" 1>&2
      exit 2
    fi
  else
    echo "$1 is not executable" 1>&2
    exit 1
  fi
}

install_local_as_version()
{
  CM3VERSION=`getversion "${FRONTEND_SRC}"`
  cp_if "${FRONTEND_SRC}" "${FRONTEND}-${CM3VERSION}"
  if [ "${GCC_BACKEND}" = yes ] ; then
    cp_if "${BACKEND_SRC}" "${BACKEND}-${CM3VERSION}"
  fi
  echo mkdir -p "${CONFIG}-${CM3VERSION}"
       mkdir -p "${CONFIG}-${CM3VERSION}"
  echo cp "${CONFIG_SRC}"/* "${CONFIG}-${CM3VERSION}"
       cp "${CONFIG_SRC}"/* "${CONFIG}-${CM3VERSION}"
}

backup_old()
{
  OLDCM3VERSION=`getversion "${FRONTEND}"`
  cp_if "${FRONTEND}" "${FRONTEND}-${OLDCM3VERSION}"
  if [ "${GCC_BACKEND}" = yes ] ; then
    cp_if "${BACKEND}" "${BACKEND}-${OLDCM3VERSION}"
  fi
  echo mkdir -p "${CONFIG}"
       mkdir -p "${CONFIG}"
  echo mkdir -p "${CONFIG}-${OLDCM3VERSION}"
       mkdir -p "${CONFIG}-${OLDCM3VERSION}"
  echo cp "${CONFIG}"/* "${CONFIG}-${OLDCM3VERSION}"
       cp "${CONFIG}"/* "${CONFIG}-${OLDCM3VERSION}"
}

rm_curent()
{
  if [ "${NOACTION}" != yes ] ; then
    rm -f "${FRONTEND}${ext}"
    if [ "${GCC_BACKEND}" = yes ] ; then
      rm -f "${BACKEND}${ext}"
    fi
    echo rm -rf "${CONFIG}"
         rm -rf "${CONFIG}"
    echo mkdir -p "${CONFIG}"
         mkdir -p "${CONFIG}"
  fi
}

cp_version()
{
  FE="${FRONTEND}-${CM3VERSION}"
  cp_if "${FE}" "${FRONTEND}"
  if [ "${GCC_BACKEND}" = yes ] ; then
    BE="${BACKEND}-${CM3VERSION}"
    cp_if "${BE}" "${BACKEND}"
  fi
  echo mkdir -p "${CONFIG}"
       mkdir -p "${CONFIG}"
  echo mkdir -p "${CONFIG}-${CM3VERSION}"
       mkdir -p "${CONFIG}-${CM3VERSION}"
  echo cp "${CONFIG}-${CM3VERSION}"/* "${CONFIG}"
       cp "${CONFIG}-${CM3VERSION}"/* "${CONFIG}"
}

upgrade()
{
  backup_old
  install_local_as_version
  FE="${FRONTEND}-${CM3VERSION}"
  if "${FE}" -version > /dev/null; then
    true
  else
    echo "${FE} is broken" 1>&2
    exit 2
  fi
  if [ "${GCC_BACKEND}" = yes ] ; then
    BE="${BACKEND}-${CM3VERSION}"
    if "${BE}" --version > /dev/null; then
      true
    else
      echo "${BE} is broken" 1>&2
      exit 2
    fi
  fi
  rm_curent
  cp_version
}

if [ "x${1}" = "x-n" ] ; then
  NOACTION=yes
  shift
fi

if [ "x${1}" = "xupgrade" ] ; then
  upgrade
elif [ "x${1}" = "xrestore" ] ; then
  if [ -z "$2" ] ; then
    echo "please specify a version" 1>&2 
    exit 1
  fi
  CM3VERSION="$2"
  cp_version
elif [ "x${1}" = "xnewversion" ] ; then
  install_local_as_version
elif [ "x${1}" = "xbackup" ] ; then
  backup_old
elif [ "x${1}" = "x" ] ; then
  usage
else
  echo "unknown command: $1" 1>&2
  exit 1
fi

exit 0
