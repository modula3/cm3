#!/bin/sh
# $Id: upgrade.sh,v 1.9 2008-01-06 18:53:22 jkrell Exp $

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

USAGE="
  `basename $0` [ generic_options ] [ generic_cmd ]

  builds a new compiler with possibly different target platforms from a 
  new set of sources, using an existing compiler of an older version.

  generic_options:
${GEN_OPTS}"
  
show_usage $@

OPTIONS=`extract_options $@`
#ACTION=`map_action $@`
#ADDARGS=`add_action_opts $@`

#echo "$ROOT/scripts/boot-cm3-with-m3.sh" "$@" "buildship"
#. "$ROOT/scripts/boot-cm3-with-m3.sh" "$@" "buildship" || exit 1

# Try to make sure that m3bundle and cminstall are available.
# These are only needed in case of cm3.cfg updates later. We need
# to build them in advance though, but ignore errors in this step.
P="m3bundle m3middle m3quake patternmatching cminstall"
echo "$ROOT/scripts/do-pkg.sh" "buildship" ${P}
"$ROOT/scripts/do-pkg.sh" "buildship" ${P}

# Now build the compiler with the installed version of the runtime;
# do _not_ compile m3core and libm3 here.
# We start with the front end...
P=""
P="${P} m3middle"
[ "${M3OSTYPE}" = "WIN32" ] && P="${P} m3objfile"
P="${P} m3linker"
[ "${GCC_BACKEND}" != yes ] && P="${P} m3back"
[ "${GCC_BACKEND}" != yes ] && P="${P} m3staloneback"
P="${P} m3front"
P="${P} m3quake"
P="${P} cm3"
[ "${M3OSTYPE}" = "WIN32" ] && P="${P} mklib"

echo "$ROOT/scripts/do-pkg.sh" "$@" "buildship ${P}"
"$ROOT/scripts/do-pkg.sh" "$@" "buildship" ${P} || exit 1

if [ "${GCC_BACKEND}" = yes ]; then
  # ... and continue with the backend, if needed
  echo "$ROOT/scripts/do-pkg.sh" "$@" "build m3cc"
  "$ROOT/scripts/do-pkg.sh" "$@" "build" m3cc || exit 1
fi

# Up to now, the compiler binaries have not been installed.
# We do this now but keep backups of the old ones.
echo "$ROOT/scripts/install-cm3-compiler.sh" $OPTIONS upgrade
"$ROOT/scripts/install-cm3-compiler.sh" $OPTIONS upgrade || exit 1

# Now try the new compiler but building the core system (without
# m3cc, as this is written in C) from scratch with it.
echo "OMIT_GCC=yes $ROOT/scripts/do-cm3-core.sh" "$@" "realclean"
OMIT_GCC=yes "$ROOT/scripts/do-cm3-core.sh" "realclean" || exit 1

echo "$ROOT/scripts/do-cm3-core.sh" "$@" "buildship"
"$ROOT/scripts/do-cm3-core.sh" "$@" "buildship" || (

  # If we fail, this may be caused by incompatible changes in cm3.cfg.
  # We try to install a new one with cminstall...
  echo "core compilation failed; trying cm3.cfg upgrade..."

  DS=${DS:-`date -u +'%Y-%m-%d-%H-%M-%S' | tr -d '\\n'`}
  CFG="${INSTALLROOT}/bin/cm3.cfg"
  CFGBAK="${CFG}--${DS}"
  mv "${CFG}" "${CFGBAK}" || exit 1
  "${INSTALLROOT}/pkg/cminstall/${TARGET}/cminstall" -c "${INSTALLROOT}" \
    > "${CFG}" || exit 1
  echo "new config file generated in ${CFG}, backup in ${CFGBAK}"

  echo "trying recompile after cleanup..."

  echo "OMIT_GCC=yes $ROOT/scripts/do-cm3-core.sh" "$@" "realclean"
  OMIT_GCC=yes "$ROOT/scripts/do-cm3-core.sh" "realclean" || exit 1

  echo "$ROOT/scripts/do-cm3-core.sh" "$@" "buildship"
  "$ROOT/scripts/do-cm3-core.sh" "$@" "buildship"

) || exit 1

# If everything has been successfull, we do another compiler upgrade.
echo "$ROOT/scripts/install-cm3-compiler.sh" $OPTIONS upgrade
"$ROOT/scripts/install-cm3-compiler.sh" $OPTIONS upgrade || exit 1
