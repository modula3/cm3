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

if [ "x$1" = "x-f" -o "x$1" = "x-force" ]; then
  FORCE="yes"
  shift
else
  FORCE=""
fi
if [ "x$1" = "x-r" -o "x$1" = "x-restore" ]; then
  RESTORE="yes"
  shift
else
  RESTORE=""
fi
if [ -n "$1" ]; then
  BACKUPID="$1"
  shift
fi

BACKUPID=${BACKUPID:-${CM3VERSION}}

. "$ROOT/scripts/pkginfo.sh"
. "$ROOT/scripts/pkgcmds.sh"

P=""
P="${P} m3core"
P="${P} libm3"
P="${P} m3middle"
P="${P} m3objfile"
P="${P} m3linker"
P="${P} m3back"
P="${P} m3staloneback"
P="${P} m3front"
P="${P} m3quake"
[ "${GCC_BACKEND}" = yes -a -z "$OMIT_GCC" ] && P="${P} m3cc"
P="${P} cm3"
P="${P} m3scanner"
P="${P} m3tools"
P="${P} m3cgcat"
P="${P} m3cggen"
P="${P} m3bundle"
P="${P} mklib"
P="${P} fix_nl"
P="${P} libdump"

USAGE="
  `basename $0` [ generic_options ] [ generic_cmd ]

  will make copies of the following packages in ${CM3_INSTALL}:

`print_list4 ${P}`
"

show_usage $@

cd ${INSTALLROOT} || exit 1
echo "${INSTALLROOT}:"

if [ -z "${RESTORE}" ]; then
  SRC="pkg"
  DEST="pkg-${BACKUPID}"
  if [ -d "${DEST}" ]; then
    if [ -z "${FORCE}" ] ; then
      echo "${DEST} already exists" 1>&2
      exit 1
    fi
  else
    mkdir ${DEST}
  fi
else
  SRC="pkg-${BACKUPID}"
  DEST="pkg"
fi
for p in ${P}; do
  echo "  ${SRC}/${p} --> ${DEST}/${p}"
  cp -pr ${SRC}/${p} ${DEST}
done
