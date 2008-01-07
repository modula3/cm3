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

#
# We pass around P as a global
# because we want to insert realclean or buildship
# between $@ and P as the previous version did.
#

Run()
{
    echo "$@"
    "$@"
}

DoPackage()
{
    P=`FilterPackages "${P}"`
    [ "${P}" = "" ] || Run "$ROOT/scripts/do-pkg.sh" "$@" "${P}"
}

Clean()
{
	CM3_ALL=yes DoPackage "$@" "realclean"
}

BuildShip()
{
    DoPackage "$@" "buildship"
}

CleanBuildShip()
{
    Clean "$@"
    BuildShip "$@"
}

#
# clean everything
P=" \
m3bundle \
m3middle \
m3quake \
patternmatching \
cminstall \
m3middle \
m3objfile \
m3linker \
m3back \
m3staloneback \
m3front \
m3quake \
cm3 \
mklib \
m3cc \
"

Clean "$@"

# Try to make sure that m3bundle and cminstall are available.
# These are only needed in case of cm3.cfg updates later. We need
# to build them in advance though, but ignore errors in this step.
P="m3bundle m3middle m3quake patternmatching cminstall"
BuildShip "$@"

# Now build the compiler with the installed version of the runtime;
# do _not_ compile m3core and libm3 here.
# We start with the front end...
P="\
m3gdb \
m3middle \
m3objfile \
m3linker \
m3back \
m3staloneback \
m3front \
m3quake \
cm3 \
mklib \
"

BuildShip "$@" || exit 1

# ... and continue with the backend, if needed
P="m3cc"
BuildShip "$@" || exit 1

# Up to now, the compiler binaries have not been installed.
# We do this now but keep backups of the old ones.
Run "$ROOT/scripts/install-cm3-compiler.sh" $OPTIONS upgrade || exit 1

# Now try the new compiler but building the core system (without
# m3cc, as this is written in C) from scratch with it.

OMIT_GCC=yes Run "$ROOT/scripts/do-cm3-core.sh" "$@" "realclean"
OMIT_GCC=yes Run "$ROOT/scripts/do-cm3-core.sh" "$@" "buildship" || (

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

  OMIT_GCC=yes Run "$ROOT/scripts/do-cm3-core.sh" "realclean" || exit 1

  Run "$ROOT/scripts/do-cm3-core.sh" "$@" "buildship"

) || exit 1

# If everything has been successfull, we do another compiler upgrade.
Run "$ROOT/scripts/install-cm3-compiler.sh" $OPTIONS upgrade || exit 1

echo "$0: Success."
