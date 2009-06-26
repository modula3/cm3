#!/bin/sh
# $Id: upgrade.sh,v 1.18 2009-06-26 06:39:17 jkrell Exp $

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

USAGE="
  `basename $0` [ generic_options ] [ generic_cmd ]

  builds a new compiler with possibly different target platforms from a 
  new set of sources, using an existing compiler of an older version.

  generic_options:
${GEN_OPTS}"
  
show_usage $@

OPTIONS=`extract_options $@`

run() {
  echo "$@"
  ( "$@" )
}

# Try to make sure that m3bundle and cminstall are available.
# These are only needed in case of cm3.cfg updates later. We need
# to build them in advance though, but ignore errors in this step.
P="sysutils m3bundle m3middle m3quake patternmatching cminstall"
run "$ROOT/scripts/do-pkg.sh" "$@" "buildship" ${P} || true

# Now build the compiler with the installed version of the runtime;
# do _not_ compile m3core and libm3 here.
# We start with the front end...
P=`FilterPackages sysutils m3middle m3objfile m3linker m3back m3staloneback \
   m3front m3quake cm3 mklib`
run "$ROOT/scripts/do-pkg.sh" "$@" "buildship" ${P} || exit 1

if [ "${GCC_BACKEND}" = yes ]; then
  # ... and continue with the backend, if needed
  run "$ROOT/scripts/do-pkg.sh" "$@" "build" m3cc || exit 1
fi

# Up to now, the compiler binaries have not been installed.
# We do this now but keep backups of the old ones.
run "$ROOT/scripts/install-cm3-compiler.sh" $OPTIONS upgrade || exit 1

# Now try the new compiler but building the core system (without
# m3cc, as this is written in C) from scratch with it.
OMIT_GCC=yes run "$ROOT/scripts/do-cm3-core.sh" "$@" "realclean" || exit 1

if [ "${UPGRADE_CM3_CFG}" != "yes" ]; then
  run "$ROOT/scripts/do-cm3-core.sh" "$@" "buildship"
  ret=$?
fi

if [ "${UPGRADE_CM3_CFG}" = "yes" -o "${ret}" != 0 ]; then (

  # If we fail, this may be caused by incompatible changes in cm3.cfg.
  # We try to install a new one with cminstall...
  if [ "${UPGRADE_CM3_CFG}" != "yes" ]; then
    echo "core compilation failed; trying cm3.cfg upgrade..."
  else
    echo "performing forced cm3.cfg upgrade..."
  fi

  DS=${DS:-`date -u +'%Y-%m-%d-%H-%M-%S' | tr -d '\\n'`}
  CFG="${INSTALLROOT}/bin/cm3.cfg"
  CFGBAK="${CFG}--${DS}"
  cp -p "${CFG}" "${CFGBAK}" || exit 1
  if grep m3_backend "${CFG}"; then
    "${INSTALLROOT}/pkg/cminstall/${TARGET}/cminstall" -c "${INSTALLROOT}" \
      -o > "${CFG}" || exit 1
    echo "new config file generated in ${CFG}, backup in ${CFGBAK}"
  else
    CFGS="${ROOT}/m3-sys/cminstall/src/config-no-install"
    for f in ${CFGS}/*; do
      b=`basename ${f}`
      cp -v ${f} ${CFGD}/${b}
    done
    ( echo "INSTALL_ROOT = \"${INSTALLROOT}\""
      echo "include(\"${TARGET}\")"
    ) > ${CFG}
    echo "new config files copied/generated in ${CFG}, backup in ${CFGBAK}"
  fi

  echo "trying recompile after cleanup..."

  OMIT_GCC=yes run "$ROOT/scripts/do-cm3-core.sh" "$@" "realclean" || exit 1

  run "$ROOT/scripts/do-cm3-core.sh" "$@" "buildship"

) fi || exit 1

# If everything has been successfull, we do another compiler upgrade.
run "$ROOT/scripts/install-cm3-compiler.sh" $OPTIONS upgrade || exit 1
