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

P=""
P="${P} m3middle"
[ "${M3OSTYPE}" = "WIN32" ] && P="${P} m3objfile"
P="${P} m3linker"
[ "${GCC_BACKEND}" != yes ] && P="${P} m3back"
[ "${GCC_BACKEND}" != yes ] && P="${P} m3staloneback"
P="${P} m3front"
P="${P} m3quake"
[ "${GCC_BACKEND}" = yes ] && P="${P} m3cc"
P="${P} cm3"
[ "${M3OSTYPE}" = "WIN32" ] && P="${P} mklib"

echo "$ROOT/scripts/do-pkg.sh" "$@" "buildship ${P}"
"$ROOT/scripts/do-pkg.sh" "$@" "buildship" ${P} || exit 1

echo "$ROOT/scripts/install-cm3-compiler.sh" $OPTIONS upgrade
"$ROOT/scripts/install-cm3-compiler.sh" $OPTIONS upgrade || exit 1

echo "OMIT_GCC=yes $ROOT/scripts/do-cm3-core.sh" "$@" "realclean"
OMIT_GCC=yes "$ROOT/scripts/do-cm3-core.sh" "realclean" || exit 1

echo "$ROOT/scripts/do-cm3-core.sh" "$@" "buildship"
"$ROOT/scripts/do-cm3-core.sh" "$@" "buildship" || (

  echo "core compilation failed; trying cm3.cfg upgrade..."

  echo "$ROOT/scripts/do-pkg.sh" "buildship cminstall"
  "$ROOT/scripts/do-pkg.sh" "buildship" cminstall || exit 1

  DS=${DS:-`date -u +'%Y-%m-%d-%H-%M-%S' | tr -d '\\n'`}
  CFG="${INSTALLROOT}/bin/cm3.cfg"
  CFGBAK="${CFG}--${DS}"
  mv "${CFG}" "${CFGBAK}" || exit 1
  "$ROOT/m3-sys/cminstall/${TARGET}/cminstall" -c > "${CFG}" || exit 1
  echo "new config file generated in ${CFG}, backup in ${CFGBAK}"

  echo "trying recompile after cleanup..."

  echo "OMIT_GCC=yes $ROOT/scripts/do-cm3-core.sh" "$@" "realclean"
  OMIT_GCC=yes "$ROOT/scripts/do-cm3-core.sh" "realclean" || exit 1

  echo "$ROOT/scripts/do-cm3-core.sh" "$@" "buildship"
  "$ROOT/scripts/do-cm3-core.sh" "$@" "buildship"

) || exit 1

echo "$ROOT/scripts/install-cm3-compiler.sh" $OPTIONS upgrade
"$ROOT/scripts/install-cm3-compiler.sh" $OPTIONS upgrade || exit 1
