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
# if a datestamp is set for the build of snapshots, include this, too
. "$ROOT/scripts/pkginfo.sh"
. "$ROOT/scripts/pkgcmds.sh"

STAGE="${STAGE:-${TMPDIR}}"
[ -d "${STAGE}" ] || mkdir "${STAGE}" || mkdir -p "${STAGE}" || exit 1
INSTALLROOT="${STAGE}/cm3"
DIST="${DIST:-core}" # may be min, core, std, all
header "building CM3 installation in ${INSTALLROOT}"
NOCLEAN=${NOCLEAN:-""}

DS=${DS:-`date -u +'%Y-%m-%d-%H-%M-%S' | tr -d '\\n'`}
BF=`build_platform`

# keep short runpaths
M3_PORTABLE_RUN_PATH=1
export M3_PORTABLE_RUN_PATH

#-----------------------------------------------------------------------------
# create the basic directories

mkdir -p "${INSTALLROOT}/bin/config" || exit 1

#-----------------------------------------------------------------------------
# compile the core system
header "stage 1: building cm3 compiler"
P=""
[ "${GCC_BACKEND}" = yes ] && P="${P} m3cc"
P="${P} m3core"
P="${P} libm3"
P="${P} sysutils"
P="${P} patternmatching"
P="${P} m3bundle"
P="${P} m3middle"
P="${P} m3objfile"
P="${P} m3linker"
P="${P} m3back"
P="${P} m3front"
P="${P} m3quake"
P="${P} cm3"
P="${P} cminstall"
P="${P} mklib"

OPTIONS=""
ACTION="`map_action build`"
ADDARGS=""

# cminstall is not contained in core and needs to be cleaned after an
# upgrade
"$ROOT/scripts/do-pkg.sh" realclean cminstall
 
echo "$ROOT/scripts/pkgmap.sh" ${OPTIONS} ${ADDARGS} -c \""${ACTION}"\" ${P}
"$ROOT/scripts/pkgmap.sh" ${OPTIONS} ${ADDARGS} -c "${ACTION}" ${P}

#-----------------------------------------------------------------------------
# install the compiler
header "stage 2: installing cm3 compiler"
echo "installing ${INSTALLROOT}/bin/cm3${EXE}"
cp "${ROOT}/m3-sys/cm3/${TARGET}/cm3${EXE}" "${INSTALLROOT}/bin" || exit 1
strip_exe "${INSTALLROOT}/bin/cm3${EXE}"
if [ "${GCC_BACKEND}" = yes ] ; then
  echo "installing ${INSTALLROOT}/bin/cm3cg${EXE}"
  cp "${ROOT}/m3-sys/m3cc/${TARGET}/cm3cg${EXE}" "${INSTALLROOT}/bin" || exit 1
  strip_exe "${INSTALLROOT}/bin/cm3cg${EXE}"
fi
if [ "${TARGET}" = WIN32 ] ; then ## We must check for all Windows targets here. TODO, dd
  echo "installing ${INSTALLROOT}/bin/mklib${EXE}"
  cp "${ROOT}/m3-sys/mklib/${TARGET}/mklib${EXE}" "${INSTALLROOT}/bin" || exit 1
  strip_exe "${INSTALLROOT}/bin/mklib${EXE}"
fi

#-----------------------------------------------------------------------------

install_config

#-----------------------------------------------------------------------------
# clean everything
header "clean everything for build with new compiler"
OMIT_GCC=yes "${ROOT}/scripts/do-cm3-${DIST}.sh" realclean || exit 1

#-----------------------------------------------------------------------------
# compile and install all needed packages
header "stage 3: compiling libraries using new cm3 compiler"
CM3="${INSTALLROOT}/bin/cm3${EXE}"
BUILDLOCAL="${CM3} -build -override -DROOT='${CM3ROOT}'"
CLEANLOCAL="${CM3} -clean -override -DROOT='${CM3ROOT}'"
BUILDGLOBAL="${CM3} -build  -DROOT='${CM3ROOT}'"
CLEANGLOBAL="${CM3} -clean -DROOT='${CM3ROOT}'"
SHIP="${CM3} -ship -DROOT='${CM3ROOT}'"
export BUILDLOCAL CLEANLOCAL BUILDGLOBAL CLEANGLOBAL SHIP

echo "${ROOT}/scripts/do-cm3-${DIST}.sh" buildlocal
"${ROOT}/scripts/do-cm3-${DIST}.sh" buildlocal || exit 1

header "stage 4: installing libraries using new cm3 compiler"
echo "${ROOT}/scripts/do-cm3-${DIST}.sh" buildglobal
"${ROOT}/scripts/do-cm3-${DIST}.sh" buildglobal || exit 1

#-----------------------------------------------------------------------------
# build binary distribution archives
ARCHIVE1="system.tgz"
ARCHIVE2="cm3-bin-${DIST}-${TARGET}-${CM3VERSION}-${BF}-${DS}.tgz"
ABSARCH1="`cygpath -u ${STAGE}/${ARCHIVE1}`"
ABSARCH2="`cygpath -u ${STAGE}/${ARCHIVE2}`"
DUSK="du-sk"
ABSDUSK="`cygpath -u ${STAGE}/du-sk`"
INSTDATA="cminstall${EXE} COPYRIGHT-CMASS ${ARCHIVE1} ${DUSK}"
header "stage 6: building archive in ${ARCHIVE2}"
echo "creating system archive in ${ABSARCH1}"
du -sk "${INSTALLROOT}" > "${ABSDUSK}"
echo "cat ${ABSDUSK}"
cat "${ABSDUSK}"
${TAR} -C "${INSTALLROOT}" -czf "${ABSARCH1}" . || exit 1
echo ".../cminstall/${TARGET}/cminstall${EXE} -->" "${STAGE}"
cp "${ROOT}/m3-sys/cminstall/${TARGET}/cminstall${EXE}" "${STAGE}" ||  exit 1
strip_exe "${STAGE}/cminstall${EXE}"
cp "${ROOT}/m3-sys/COPYRIGHT-CMASS" "${STAGE}" || exit 1
echo "creating distribution archive ${ABSARCH2}"
${TAR} -C "${STAGE}" -czf ${ABSARCH2} ${INSTDATA} || exit 1
chmod 664 "${ABSARCH2}"
ls -l "${ABSARCH2}"

if [ -n "${DOSHIP}" ]; then
  if test "x${CM3CVSUSER}" != "x"; then
    CM3CVSUSER_AT="${CM3CVSUSER}@"
  else
    CM3CVSUSER_AT=""
  fi
  WWWSERVER=${WWWSERVER:-${CM3CVSUSER_AT}birch.elegosoft.com}
  WWWDEST=${WWWDEST:-${WWWSERVER}:/var/www/modula3.elegosoft.com/cm3/snaps}
  if type ship_www; then
    echo "ship_www ${ABSARCH2} cm3/snaps"
    ship_www "${ABSARCH2}" cm3/snaps
  else
    scp "${ABSARCH2}" "${WWWDEST}" < /dev/null
  fi
fi
if [ -z "${NOCLEAN}" ]; then
  echo "cleaning up"
  cd "${STAGE}" && rm -f ${INSTDATA}
  rm -rf "${INSTALLROOT}"
else
  echo "NOT cleaning up. Don't forget to"
  echo "cd ${STAGE} && rm -f ${INSTDATA}"
  echo "rm -rf ${INSTALLROOT}"
fi
