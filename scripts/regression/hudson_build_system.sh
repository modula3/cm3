#/bin/sh

set -e
#set -x

ulimit -d unlimited || true

WS=${WORKSPACE}
for a in a m s p r; do
    echo uname -$a: `uname -$a`
done
for a in cc gcc g++ gcc-4.2 g++-4.2; do
  for b in "" /usr/bin/ /usr/local/bin/; do
    which $b$a && echo $b$a -v: `$b$a -v 2>&1 || true`
  done
done
date
cd ${WS}/cm3/scripts || {
  echo "cannot cd to ${WS}/cm3/scripts" 1>&2 
  exit 1
}
. ./sysinfo.sh
echo "WS=${WS}"
echo "CVS_BRANCH=${CVS_BRANCH}"
echo "BUILD_TAG=${BUILD_TAG}"
echo ""
cd ${WS}/cm3/scripts/regression || {
  echo "cannot cd to ${WS}/cm3/scripts/regression" 1>&2 
  exit 1
}

for a in cm3cg gcc/m3cgc1; do
    for b in "" .exe; do
        rm ${WS}/cm3/m3-sys/m3cc/${TARGET}/$a$b || true
    done
done

# echo 1 CM3CG is ${CM3CG}
# if [ -z "${CM3CG}" ]; then
#   case "${WS}" in
#     *workspace)
#       CM3CG=${WS}/../../cm3-current-m3cc-${TARGET}/workspace/cm3/m3-sys/m3cc/${TARGET}/cm3cg
#     ;;
#     *)
#       CM3CG=${WS}/../cm3-current-m3cc-${TARGET}/cm3/m3-sys/m3cc/${TARGET}/cm3cg
#     ;;
#   esac
# fi
# echo 2 CM3CG is ${CM3CG}

# if [ "x$CLEAN" = "xfalse" ]; then
#   if [ "x$USE_PREBUILT_CM3CG" = "xtrue" ]; then
#     if [ -x "${CM3CG}" ]; then
#       echo "checking for working pre-built cm3cg in ${CM3CG}"
#       cp -p "${CM3CG}" ${WS}/cm3cg
#       if ${WS}/cm3cg --version; then
#         echo "using PREBUILT_CM3CG=${WS}/cm3cg"
#         PREBUILT_CM3CG=${WS}/cm3cg
#         export PREBUILT_CM3CG
#       else
#         echo "NOT using ${WS}/cm3cg"
#       fi
#     else
#       echo "no executable ${CM3CG}"
#     fi
#   fi
# fi
. ./defs.sh

[ "${WS}/cm3/scripts/pkginfo.txt" -nt "${WS}/cm3/scripts/PKGS" ] && {
  echo "deleting outdated packages cache ${WS}/cm3/scripts/PKGS"
  rm -f "${WS}/cm3/scripts/PKGS"
}

rm -rf "${INSTROOT_CUR}"
mkdir -p "${INSTROOT_CUR}"
# echo setup lastok version as a start
# echo cp -pR ${INSTROOT_LOK}/* ${INSTROOT_CUR}
# cp -pR ${INSTROOT_LOK}/* ${INSTROOT_CUR}
echo setup lastrel version as a start
echo cp -pR ${INSTROOT_REL}/* ${INSTROOT_CUR}
cp -pR ${INSTROOT_REL}/* ${INSTROOT_CUR}

export TMPDIR=${WS}

test_build_system || {
  echo "test_build_system failed" 1>&2
  exit 1
}

if [ "$BUILD_SNAPSHOT" = "true" ]; then
  # debug
  #set -x
  cd ${WS}/cm3/scripts/ || exit 1
  STAGE=${HOME}/${TESTHOSTNAME}/stage
  DIST=core
  NOCLEAN=
  export STAGE DIST NOCLEAN
  mkdir -p ${STAGE}
  test_make_bin_dist
else
  true
fi
