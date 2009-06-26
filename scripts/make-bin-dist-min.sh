#!/bin/sh
# $Id$

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
DIST="${DIST:-std}" # may be min, core, std, all
header "building CM3 installation in ${INSTALLROOT}"
NOCLEAN=${NOCLEAN:-""}

NEWCFG=${NEWCFG:-y}

DS=${DS:-`date -u +'%Y-%m-%d-%H-%M-%S' | tr -d '\\n'`}

# keep short runpaths
M3_PORTABLE_RUN_PATH=1
export M3_PORTABLE_RUN_PATH

#-----------------------------------------------------------------------------
# create the basic directories
. "${ROOT}/scripts/create-skel.sh"

#-----------------------------------------------------------------------------
# compile the core system
header "stage 1: building cm3 compiler"
P=""
P="${P} m3core"
P="${P} libm3"
P="${P} sysutils"
P="${P} patternmatching"
P="${P} m3bundle"
P="${P} m3middle"
P="${P} m3objfile"
P="${P} m3linker"
P="${P} m3back"
[ "${GCC_BACKEND}" != yes ] && P="${P} m3staloneback"
P="${P} m3front"
P="${P} m3quake"
[ "${GCC_BACKEND}" = yes ] && P="${P} m3cc"
P="${P} cm3"
P="${P} cminstall"
[ "${M3OSTYPE}" = "WIN32" ] && P="${P} mklib"

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
if [ "${M3OSTYPE}" = "WIN32" ] ; then
  echo "installing ${INSTALLROOT}/bin/mklib${EXE}"
  cp "${ROOT}/m3-sys/mklib/${TARGET}/mklib${EXE}" "${INSTALLROOT}/bin" \
    || exit 1
  strip_exe "${INSTALLROOT}/bin/mklib${EXE}"
fi
if [ -n "${SYSLIBS}" ] ; then
  echo "installing low-level system libraries"
  for f in ${SYSLIBS} ; do
    if [ -f "${SYSLIBDIR}/${f}" ] ; then
      echo "${SYSLIBDIR}/${f} -->" "${INSTALLROOT}/lib/${f}"
      cp "${SYSLIBDIR}/${f}" "${INSTALLROOT}/lib/${f}"
    else
      echo "${SYSLIBDIR}/${f} not found" 1>&2
      exit 1
    fi
  done
fi

#-----------------------------------------------------------------------------
# configure a temporary config file
echo configuring temporary config file "${INSTALLROOT}/bin/cm3.cfg"
if [ "${NEWCFG}" != "y" ]; then
  # old style installation
  if [ "${TARGET}" = "NT386" -o "${TARGET}" = "NT386GNU" ]; then
    CFG1="${ROOT}/m3-sys/cm3/src/config/${TARGET}.main"
    CFG2="${ROOT}/m3-sys/cminstall/src/config/${TARGET}.main"
    CFG3="${ROOT}/m3-sys/cminstall/src/config/${TARGET}.common"
    cp "${CFG3}" "${INSTALLROOT}/bin"
  else
    CFG1="${ROOT}/m3-sys/cm3/src/config/${TARGET}"
  fi
  sed -e '
    /^INSTALL_ROOT[ \t]*=/s;^.*$;INSTALL_ROOT = "'${INSTALLROOT}${SL}'";
    /^readonly DEV_LIB[ \t]*=/s;^.*$;readonly DEV_LIB = "'${DEV_LIB}${SL}'";
    /^readonly DEV_BIN[ \t]*=/s;^.*$;readonly DEV_BIN = "'${DEV_BIN}${SL}'";
  ' "${CFG1}" > "${INSTALLROOT}/bin/cm3.cfg"
else
  # delete old config files
  for f in ${ROOT}/m3-sys/cminstall/src/config-no-install/*; do
    b=`basename ${f}`
    if [ -f "${INSTALLROOT}/bin/${b}" ] ; then
      rm "${INSTALLROOT}/bin/${b}" > /dev/null
  done
  # new config files
  cp "${ROOT}/m3-sys/cminstall/src/config-no-install/"* "${INSTALLROOT}/bin/config"
  (
    echo "INSTALL_ROOT = (path() & SL & \"..\")"
    echo "include(path() & \"/config/${TARGET}\")"
  ) > "${INSTALLROOT}/bin/cm3.cfg"
fi

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

[ ${TARGET} != NT386 ] && "${ROOT}/scripts/do-pkg.sh" buildship
"${ROOT}/scripts/do-cm3-${DIST}.sh" buildlocal || exit 1

header "stage 4: installing libraries using new cm3 compiler"
"${ROOT}/scripts/do-cm3-${DIST}.sh" buildglobal || exit 1

if [ "${NEWCFG}" != "y" ]; then
  header "stage 5: re-adjusting cm3.cfg"
  echo "${CFG2} -->" "${INSTALLROOT}/bin/cm3.cfg"
  cp "${CFG2}" "${INSTALLROOT}/bin/cm3.cfg"
  echo "${CFG1} -->" "${INSTALLROOT}/bin/cm3.cfg--default"
  cp "${CFG1}" "${INSTALLROOT}/bin/cm3.cfg--default"
else
  echo "no new config"
fi

#-----------------------------------------------------------------------------
# build binary distribution archives
ARCHIVE1="system.tgz"
ARCHIVE2="cm3-bin-${DIST}-${TARGET}-${CM3VERSION}-${DS}.tgz"
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
if [ "${M3OSTYPE}" = "WIN32" ] ; then
  ITAR="`find_file tar.exe ${CM3BINSEARCHPATH}`"
  IGZIP="`find_file gzip.exe ${CM3BINSEARCHPATH}`"
  CYGWINDLL="`find_file cygwin.dll ${CM3BINSEARCHPATH}`"
  if [ -f "${ITAR}" ] ; then
    cp "${ITAR}" "${STAGE}"
    INSTDATA="${INSTDATA} tar.exe"
  else
    echo "no tar.exe found on WIN32 for installation archive" 1>&2
  fi
  if [ -f "${IGZIP}" ] ; then
    cp "${IGZIP}" "${STAGE}"
    INSTDATA="${INSTDATA} gzip.exe"
  else
    echo "no gzip.exe found on WIN32 for installation archive" 1>&2
  fi
  if [ -f "${CYGWINDLL}" ] ; then
    cp "${CYGWINDLL}" "${STAGE}"
    INSTDATA="${INSTDATA} cygwin.dll"
  else
    echo "no tar.exe found on WIN32 for installation archive" 1>&2
  fi
fi
echo "creating distribution archive ${ABSARCH2}"
${TAR} -C "${STAGE}" -czf ${ABSARCH2} ${INSTDATA} || exit 1
ls -l "${ABSARCH2}"
if [ -n "${DOSHIP}" ]; then
  WWWSERVER=${WWWSERVER:-birch.elegosoft.com}
  WWWDEST=${WWWDEST:-${WWWSERVER}:/var/www/modula3.elegosoft.com/cm3/snaps}
  scp "${ABSARCH2}" "${WWWDEST}" < /dev/null
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
