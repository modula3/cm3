#!/bin/sh
# $Id: make-bin-dist-min.sh,v 1.5 2001-02-14 16:35:36 wagner Exp $

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

STAGE="${STAGE:-${TMPDIR}}"
[ -d "${STAGE}" ] || mkdir "${STAGE}" || mkdir -p "${STAGE}" || exit 1
INSTALLROOT="${STAGE}/cm3"
head "building CM3 installation in ${INSTALLROOT}"

# create the basic directories
. "${ROOT}/scripts/create-skel.sh"

# compile the core system
head "stage 1: building cm3 compiler"
. "${ROOT}/scripts/boot-cm3-with-m3.sh" build || exit 1

# install the compiler
head "stage 2: installing cm3 compiler"
echo "installing ${INSTALLROOT}/bin/cm3${EXE}"
cp "${ROOT}/m3-sys/cm3/${TARGET}/cm3${EXE}" "${INSTALLROOT}/bin"
[ "${GCC_BACKEND}" = yes ] && \
  echo "installing ${INSTALLROOT}/bin/cm3cg${EXE}" && \
  cp "${ROOT}/m3-sys/m3cc/${TARGET}/cm3cg${EXE}" "${INSTALLROOT}/bin"
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

# configure a temporary config file
echo configuring temporary config file "${INSTALLROOT}/bin/cm3.cfg"
sed -e '
  /^INSTALL_ROOT[ \t]*=/s;^.*$;INSTALL_ROOT = "'${INSTALLROOT}${SL}'";
' "${ROOT}/m3-sys/cm3/src/config/${TARGET}" > "${INSTALLROOT}/bin/cm3.cfg"

# compile and install all needed packages
head "stage 3: compiling libraries using new cm3 compiler"
CM3="${INSTALLROOT}/bin/cm3${EXE}"
BUILDLOCAL="${CM3} -build -override -DROOT=${ROOT}"
CLEANLOCAL="${CM3} -clean -override -DROOT=${ROOT}"
BUILDGLOBAL="${CM3} -build  -DROOT=${ROOT}"
CLEANGLOBAL="${CM3} -clean -DROOT=${ROOT}"
SHIP="${CM3} -ship -DROOT=${ROOT}"
export BUILDLOCAL CLEANLOCAL BUILDGLOBAL CLEANGLOBAL SHIP

"${ROOT}/scripts/do-cm3-min.sh" buildlocal || exit 1

head "stage 4: installing libraries using new cm3 compiler"
"${ROOT}/scripts/do-cm3-min.sh" buildglobal || exit 1

head "stage 5: re-adjusting cm3.cfg"
echo ".../cminstall/src/config/${TARGET} -->" \
  "${INSTALLROOT}/bin/cm3.cfg"
cp "${ROOT}/m3-sys/cminstall/src/config/${TARGET}" \
  "${INSTALLROOT}/bin/cm3.cfg"
echo ".../cm3/src/config/${TARGET} -->" \
  "${INSTALLROOT}/bin/cm3.cfg--default"
cp "${ROOT}/m3-sys/cm3/src/config/${TARGET}" \
  "${INSTALLROOT}/bin/cm3.cfg--default"

ARCHIVE1="system.tgz"
ARCHIVE2="cm3-min-${M3OSTYPE}-${TARGET}-${CM3VERSION}.tgz"
head "stage 6: building archive in ${ARCHIVE2}"
echo "creating system archive in ${STAGE}/${ARCHIVE1}"
tar -C "${INSTALLROOT}" -czf "${STAGE}/${ARCHIVE1}" . || exit 1
echo ".../cminstall/${TARGET}/cminstall${EXE} -->" "${STAGE}"
cp "${ROOT}/m3-sys/cminstall/${TARGET}/cminstall${EXE}" "${STAGE}" ||  exit 1
cp "${ROOT}/m3-sys/COPYRIGHT-CMASS" "${STAGE}" || exit 1
echo "creating distribution archive ${STAGE}/${ARCHIVE2}"
tar -C "${STAGE}" -czf "${STAGE}/${ARCHIVE2}" cminstall${EXE} \
  COPYRIGHT-CMASS ${ARCHIVE1} || exit 1
ls -l "${STAGE}/${ARCHIVE2}"
echo "cleaning up"
rm -f "${STAGE}/cminstall${EXE}"
rm -rf "${INSTALLROOT}"
rm -f "${STAGE}/${ARCHIVE1}"
