#!/bin/sh
# $Id: make-src-dist-sys.sh,v 1.1 2001-02-16 17:13:53 wagner Exp $

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
#. "$ROOT/scripts/pkginfo.sh"
#. "$ROOT/scripts/pkgcmds.sh"

STAGE="${STAGE:-${TMPDIR}}"
[ -d "${STAGE}" ] || mkdir "${STAGE}" || mkdir -p "${STAGE}" || exit 1
ARCHIVE="${STAGE}/cm3-src-sys-${CM3VERSION}.tgz"
header "building CM3 source distribution in ${ARCHIVE}"

#-----------------------------------------------------------------------------
# build the source distribution archive
#

cd "${ROOT}" || exit 1

/bin/ls -1d COPYRIGHT-CMASS COPYRIGHT-DEC scripts m3-sys > .tar-include
/bin/ls -1d m3-libs/m3core m3-libs/libm3 >> .tar-include
/bin/ls -1d m3-*/*/${TARGET} > .tar-exclude
echo "building exclude list..."
find . \( -name '*~' -or -name '*.bak' -or -name '*.orig' -or \
          -name '*.rej' -or -name 'cvs-nq-up' -or -name '*-diffs' -or \
          \( -name 'CVS' -a -type d \) \) -print | \
  sed -e 's;^./;;' >> .tar-exclude

echo "archiving..."
export GZIP="-9 -v"
${TAR} -czf ${ARCHIVE} --files-from .tar-include --exclude-from .tar-exclude \
 || exit 1
ls -l ${ARCHIVE}
echo "done"
exit 0

