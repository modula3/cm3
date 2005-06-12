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

${ROOT}/scripts/backup-pkgs.sh # does nothing if backup already exists
${ROOT}/scripts/backup-pkgs.sh -restore # for multiple runs of this script...

${ROOT}/scripts/do-cm3-core.sh realclean

CFG=${INSTALLROOT}/bin/cm3.cfg

perl -p -i -e 's/^\s*M3_FRONT_FLAGS\s*=.*$/M3_FRONT_FLAGS = [ ]/' ${CFG}

env -i PATH="${PATH}" INSTALLROOT="${INSTALLROOT}" \
  ${ROOT}/scripts/do-pkg.sh buildship m3middle m3front cm3 || exit 1

env -i PATH="${PATH}" INSTALLROOT="${INSTALLROOT}" \
  ${ROOT}/scripts/install-cm3-compiler.sh upgrade

${ROOT}/scripts/do-cm3-core.sh realclean

env -i PATH="${PATH}" INSTALLROOT="${INSTALLROOT}" \
  ${ROOT}/scripts/do-pkg.sh buildship \
  m3gc-simple m3core libm3 m3middle m3linker m3front m3quake cm3 || exit 1

env -i PATH="${PATH}" INSTALLROOT="${INSTALLROOT}" \
  ${ROOT}/scripts/install-cm3-compiler.sh upgrade

perl -p -i -e \
 's/^\s*M3_FRONT_FLAGS\s*=.*$/M3_FRONT_FLAGS = [ "-IncGC", "-GenGC" ]/' ${CFG}

${ROOT}/scripts/do-cm3-core.sh realclean

env -i PATH="${PATH}" INSTALLROOT="${INSTALLROOT}" \
  ${ROOT}/scripts/do-pkg.sh buildship \
  m3gc-simple m3core libm3 m3middle m3linker m3front m3quake cm3 || exit 1

env -i PATH="${PATH}" INSTALLROOT="${INSTALLROOT}" \
  ${ROOT}/scripts/install-cm3-compiler.sh upgrade
