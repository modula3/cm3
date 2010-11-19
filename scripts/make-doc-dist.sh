#!/bin/sh

if [ -n "$ROOT" -a -d "$ROOT" ] ; then
  sysinfo="$ROOT/scripts/sysinfo.sh"
  root="${ROOT}"; export root
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
ARCHIVE="${STAGE}/cm3-doc-${CM3VERSION}.tgz"
header "building CM3 doc distribution in ${ARCHIVE}"

#-----------------------------------------------------------------------------
# build the doc distribution archive
#

if [ "$1" = "-gen" ] ; then
  cd "${ROOT}/doc/help/gen_html" || exit 1
  rm -f html_gen/m3db 
  m3tohtml -v -root /usr/local/cm3/pkg `cd /usr/local/cm3/pkg && ls -d *`
fi

cd "${ROOT}" || exit 1
ls -1d COPYRIGHT-CMASS COPYRIGHT-DEC doc > .tar-include
ls -1d m3-*/*/${TARGET} > .tar-exclude
ls -1d doc/help/reactor >> .tar-exclude
ls -1d doc/src_reports/*.ps >> .tar-exclude
ls -1d doc/src_reports/*.ps.gz >> .tar-exclude
echo "building exclude list..."
$FIND doc \( -name '*~' -or -name '*.bak' -or -name '*.orig' -or \
          -name '*.rej'  -or -name 'cvs-nq-up' -or -name '*-diffs' -or \
          \( -name 'CVS' -a -type d \) \) -print | \
  sed -e 's;^./;;' >> .tar-exclude

echo "archiving..."
export GZIP="-9 -v"
${TAR} -czf ${ARCHIVE} --files-from .tar-include --exclude-from .tar-exclude \
 || exit 1
ls -l ${ARCHIVE}
echo "done"
exit 0
