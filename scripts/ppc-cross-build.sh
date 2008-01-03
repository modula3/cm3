#!/bin/sh

# This script is an example of how to automate a cross-compilation for
# bootstrapping cm3 on a new target platform. Values are set for
# cross-compilation from FreeBSD4 to PPC_DARWIN running on 
# lamancha.opendarwin.org.

TARGET=${TARGET:-PPC_DARWIN}
WORK=${WORK:-/Users/wagner/work}
CM3DEST=${CM3DEST:-/Users/wagner/local/cm3}
CM3DESTBIN=${CM3DEST}/bin
CM3DESTHOST=${CM3DESTHOST:-lamancha.opendarwin.org}

./boot-cm3-core.sh ${TARGET} || exit 1
./copy-bootarchives.sh ${TARGET} || exit 1
ssh ${CM3DESTHOST} \
  cd ${WORK}/cm3/scripts '&&' \
  ./boot-cm3-build-on-target.sh ${TARGET} \; \
  [ -f ${CM3DESTBIN}/cm3 ] '&&' mv ${CM3DESTBIN}/cm3 ${CM3DESTBIN}/cm3.bak \;\
  cp ${WORK}/cm3/${TARGET}/m3-sys/cm3/${TARGET}/cm3 ${CM3DESTBIN}
