#/bin/sh

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

if [ -z "${CM3CG}" ]; then
  case "${WS}" in
    *workspace)
      CM3CG=${WS}/../../cm3-current-m3cc-${TARGET}/workspace/cm3/m3-sys/m3cc/${TARGET}/cm3cg
    ;;
    *)
      CM3CG=${WS}/../cm3-current-m3cc-${TARGET}/cm3/m3-sys/m3cc/${TARGET}/cm3cg
    ;;
  esac
fi
if [ "$CLEAN" = "false" ]; then
  if [ -x "${CM3CG}" ]; then
    echo "checking for working pre-built cm3cg in ${CM3CG}"
    cp -p "${CM3CG}" ${WS}/cm3cg
    if ${WS}/cm3cg --version; then
      echo "using PREBUILT_CM3CG=${WS}/cm3cg"
      PREBUILT_CM3CG=${WS}/cm3cg
      export PREBUILT_CM3CG
    else
      echo "NOT using ${WS}/cm3cg"
    fi
  else
    echo "no executable ${CM3CG}"
  fi
fi
. ./defs.sh

[ "$CLEAN" = "true" ] && {
  OMIT_GCC=
  ${WS}/cm3/scripts/do-cm3-all.sh realclean
}
[ "${WS}/cm3/scripts/pkginfo.txt" -nt "${WS}/cm3/scripts/PKGS" ] && {
  echo "deleting outdated packages cache ${WS}/cm3/scripts/PKGS"
  rm -f "${WS}/cm3/scripts/PKGS"
}

# perform tests on last ok version
INSTROOT_CUR="${INSTROOT_CUR}--all-pkgs"

# setup lastok version as a start
rm -rf ${INSTROOT_CUR}
cp -pR ${INSTROOT_LOK} ${INSTROOT_CUR}

export TMPDIR=${WS}
test_m3_all_pkgs || {
  echo "test_m3_all_pkgs did not return 0" 1>&2
}
cd ${WS}
mkdir -p ${HOME}/work
cp cm3-pkg-report-${TARGET}.xml \
   cm3-pkg-test-report-${TARGET}.xml \
   cm3-pkg-report-${TARGET}.html ${HOME}/work

