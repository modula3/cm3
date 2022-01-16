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
. "$ROOT/scripts/pkginfo.sh"
. "$ROOT/scripts/pkgcmds.sh"

set -e
set -x

for f in ${ROOT}/m3-sys/cminstall/src/config/*; do
  rm -f "${INSTALLROOT}/bin/`basename ${f}`" > /dev/null
done
mkdir -p "${INSTALLROOT}/bin/config"
for f in ${ROOT}/m3-sys/cminstall/src/config/*; do
  [ -f ${f} ] && cp "${f}" "${INSTALLROOT}/bin/config"
done
(
  echo "INSTALL_ROOT = path() & \"/..\""
  echo "include(path() & \"/config/${TARGET}\")"
) > "${INSTALLROOT}/bin/cm3.cfg"
