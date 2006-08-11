#!/bin/sh
# $Id: make-bin-dist-min.sh,v 1.21 2006-08-06 13:13:48 wagner Exp $

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

STAGE="${STAGE:-${TMPDIR}}"
[ -d "${STAGE}" ] || mkdir "${STAGE}" || mkdir -p "${STAGE}" || exit 1
INSTALLROOT="${STAGE}/cm3"
header "building CM3 installation in ${INSTALLROOT}"

#-----------------------------------------------------------------------------
# create the basic directories
. "${ROOT}/scripts/create-skel.sh"

#-----------------------------------------------------------------------------
# compile the core system
header "stage 1: building cm3 compiler"
P=""
[ ${TARGET} != NT386 ] && P="${P} m3gc-simple"
#if syscall_wrappers_exist && [ -z "$M3GC_SIMPLE" ] ; then
#  [ ${TARGET} != NT386 ] && P="${P} m3gc-enhanced"
#fi
P="${P} m3core"
P="${P} libm3"
P="${P} patternmatching"
P="${P} m3bundle"
P="${P} m3middle"
[ "${M3OSTYPE}" = "WIN32" ] && P="${P} m3objfile"
P="${P} m3linker"
[ "${GCC_BACKEND}" != yes ] && P="${P} m3back"
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
sed -e '
  /^INSTALL_ROOT[ \t]*=/s;^.*$;INSTALL_ROOT = "'${INSTALLROOT}${SL}'";
  /^readonly DEV_LIB[ \t]*=/s;^.*$;readonly DEV_LIB = "'${DEV_LIB}${SL}'";
  /^readonly DEV_BIN[ \t]*=/s;^.*$;readonly DEV_BIN = "'${DEV_BIN}${SL}'";
' "${ROOT}/m3-sys/cm3/src/config/${TARGET}" > "${INSTALLROOT}/bin/cm3.cfg"

#-----------------------------------------------------------------------------
# clean everything
header "clean evrything for build with new compiler"
"${ROOT}/scripts/do-cm3-min.sh" realclean || exit 1

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

"${ROOT}/scripts/do-cm3-min.sh" buildlocal || exit 1

header "stage 4: installing libraries using new cm3 compiler"
"${ROOT}/scripts/do-cm3-min.sh" buildglobal || exit 1

header "stage 4a: applying library fixups"
for d in m3core libm3; do
  f="${INSTALLROOT}/pkg/${d}/${TARGET}/.M3EXPORTS"
  echo "fixing ${f}"
  perl -i -p -e 's;_import_otherlib\("m3gcdefs".*;_import_otherlib("m3gcdefs", LIB_USE, IMPORTED);' "${f}"
done

header "stage 5: re-adjusting cm3.cfg"
echo ".../cminstall/src/config/${TARGET} -->" \
  "${INSTALLROOT}/bin/cm3.cfg"
cp "${ROOT}/m3-sys/cminstall/src/config/${TARGET}" \
  "${INSTALLROOT}/bin/cm3.cfg"
echo ".../cm3/src/config/${TARGET} -->" \
  "${INSTALLROOT}/bin/cm3.cfg--default"
cp "${ROOT}/m3-sys/cm3/src/config/${TARGET}" \
  "${INSTALLROOT}/bin/cm3.cfg--default"

#-----------------------------------------------------------------------------
# build binary distribution archives
ARCHIVE1="system.tgz"
ARCHIVE2="cm3-min-${M3OSTYPE}-${TARGET}-${CM3VERSION}.tgz"
ABSARCH1="`cygpath -u ${STAGE}/${ARCHIVE1}`"
ABSARCH2="`cygpath -u ${STAGE}/${ARCHIVE2}`"
INSTDATA="cminstall${EXE} COPYRIGHT-CMASS ${ARCHIVE1}"
header "stage 6: building archive in ${ARCHIVE2}"
echo "creating system archive in ${ABSARCH1}"
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
echo "cleaning up"
cd "${STAGE}" && rm -f ${INSTDATA}
rm -rf "${INSTALLROOT}"
