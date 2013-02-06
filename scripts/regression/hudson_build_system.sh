#/bin/sh

#set -e
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

. ./defs.sh

[ "${WS}/cm3/scripts/pkginfo.txt" -nt "${WS}/cm3/scripts/PKGS" ] && {
  echo "deleting outdated packages cache ${WS}/cm3/scripts/PKGS"
  rm -f "${WS}/cm3/scripts/PKGS"
}

rm -rf "${INSTROOT_CUR}"
mkdir -p "${INSTROOT_CUR}"
echo setup lastok version as a start
echo cp -pR ${INSTROOT_LOK}/* ${INSTROOT_CUR}
cp -pR ${INSTROOT_LOK}/* ${INSTROOT_CUR}

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
