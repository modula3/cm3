#!/bin/sh

VERSION=d5.11.4
DISTRIBUTION="cm3-dist-AMD64_LINUX-${VERSION}"

sudo ln -s /usr/pkg/bin/python3.8 /usr/pkg/bin/python3

mkdir work
cd work
wget --no-check-certificate "https://github.com/modula3/cm3/releases/download/${VERSION}/${DISTRIBUTION}.tar.xz"
gtar Jxf "${DISTRIBUTION}.tar.xz"
mkdir build
cd build
"../${DISTRIBUTION}/scripts/concierge.py" install --prefix "${HOME}/cm3" --target AMD64_NETBSD
PATH="${HOME}/cm3/bin:${PATH}"
cd "../${DISTRIBUTION}/m3-sys/m3tests"
cm3 -DHTML
head -1 m3tests-results.xml
