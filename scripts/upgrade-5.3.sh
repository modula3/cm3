#!/bin/sh
# $Id: upgrade-5.3.sh,v 1.2 2005-08-31 20:31:58 wagner Exp $

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

header() {
  echo ""
  echo "============================================================================="
  echo "$@"
  echo "============================================================================="
  echo ""
}

header "step 1: backing up existing packages (once only)"
${ROOT}/scripts/backup-pkgs.sh # does nothing if backup already exists

header "step 2: restoring old packages from backup (for multiple runs)"
${ROOT}/scripts/backup-pkgs.sh -restore # for multiple runs of this script...

header "step 3: cleaning core packages"
${ROOT}/scripts/do-cm3-core.sh realclean

CFG=${M3CONFIG:-${INSTALLROOT}/bin/cm3.cfg}

header "step 4: removing M3_FRONT_FLAGS from ${CFG}"
perl -p -i -e 's/^\s*M3_FRONT_FLAGS\s*=.*$/M3_FRONT_FLAGS = [ ]/' ${CFG}

header "step 5: building new cm3 based on old runtime"
env -i PATH="${PATH}" INSTALLROOT="${INSTALLROOT}" M3CONFIG="${M3CONFIG}" \
  ${ROOT}/scripts/do-pkg.sh buildship m3cc m3middle m3front cm3 || exit 1

header "step 6: installing new compiler"
env -i PATH="${PATH}" INSTALLROOT="${INSTALLROOT}" M3CONFIG="${M3CONFIG}" \
  ${ROOT}/scripts/install-cm3-compiler.sh upgrade
ls -l ${INSTALLROOT}/bin/cm3*

header "step 7: cleaning core packages"
${ROOT}/scripts/do-cm3-core.sh realclean

header "step 8: building new compiler using new runtime"
env -i PATH="${PATH}" INSTALLROOT="${INSTALLROOT}" M3CONFIG="${M3CONFIG}" \
  M3GC_SIMPLE=yes \
  ${ROOT}/scripts/do-pkg.sh buildship \
  m3gc-simple m3core libm3 m3middle m3linker m3front m3quake cm3 || exit 1

header "step 9: installing new compiler"
env -i PATH="${PATH}" INSTALLROOT="${INSTALLROOT}" M3CONFIG="${M3CONFIG}" \
  M3GC_SIMPLE=yes \
  ${ROOT}/scripts/install-cm3-compiler.sh upgrade
ls -l ${INSTALLROOT}/bin/cm3*

header "step 10: setting M3_FRONT_FLAGS to -IncGC -GenGC"
perl -p -i -e \
 's/^\s*M3_FRONT_FLAGS\s*=.*$/M3_FRONT_FLAGS = [ "-IncGC", "-GenGC" ]/' ${CFG}

header "step 11: cleaning core packages"
${ROOT}/scripts/do-cm3-core.sh realclean

header "step 12: rebuilding runtime and compiler using new gc support"
env -i PATH="${PATH}" INSTALLROOT="${INSTALLROOT}" M3CONFIG="${M3CONFIG}" \
  M3GC_SIMPLE=yes \
  ${ROOT}/scripts/do-pkg.sh buildship \
  m3gc-simple m3core libm3 m3middle m3linker m3front m3quake cm3 || exit 1

header "step 13: installing new compiler"
env -i PATH="${PATH}" INSTALLROOT="${INSTALLROOT}" M3CONFIG="${M3CONFIG}" \
  M3GC_SIMPLE=yes \
  ${ROOT}/scripts/install-cm3-compiler.sh upgrade
ls -l ${INSTALLROOT}/bin/cm3*
