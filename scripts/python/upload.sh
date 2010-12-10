#!/bin/sh

if test "x${CM3CVSUSER}" != "x"; then
  CM3CVSUSER_AT="${CM3CVSUSER}@"
elif test "x${USER}" = "xjaykrell" \
       -o "x${USER}" = "xjay" \
       -o "x${USER}" = "xjayk"; then
  CM3CVSUSER_AT="jkrell@"
else
  CM3CVSUSER_AT=""
fi

DESTHOST=${DESTHOST:-${CM3CVSUSER_AT}birch.elegosoft.com}
DEST=${DEST:-/var/www/modula3.elegosoft.com/cm3/uploaded-archives}
chmod 664 *.tar.gz *.tar.xz *.tgz *.deb
scp *.tar.gz *.tar.xz *.tgz *.deb $DESTHOST:$DEST
