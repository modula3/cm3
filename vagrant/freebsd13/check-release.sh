#!/bin/sh

VERSION=d5.11.4
DISTRIBUTION="cm3-dist-AMD64_LINUX-${VERSION}"

mkdir work
cd work
wget "https://github.com/modula3/cm3/releases/download/${VERSION}/${DISTRIBUTION}.tar.xz"
gtar Jxf "${DISTRIBUTION}.tar.xz"
sed -i -e 's/^SYSTEM_CC.*/SYSTEM_CC = "c++ -fPIC"/' "${DISTRIBUTION}/m3-sys/cminstall/src/config-no-install/AMD64_FREEBSD"
mkdir build
cd build
"../${DISTRIBUTION}/scripts/concierge.py" install --prefix "${HOME}/cm3" --target AMD64_FREEBSD
PATH="${HOME}/cm3/bin:${PATH}"
cd "../${DISTRIBUTION}/m3-sys/m3tests"
cm3 -DHTML
head -1 m3tests-results.xml
