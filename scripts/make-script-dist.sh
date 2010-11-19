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

DATESTR=`date +"%Y-%m-%d"`
STAGE="${STAGE:-${TMPDIR}}"
[ -d "${STAGE}" ] || mkdir "${STAGE}" || mkdir -p "${STAGE}" || exit 1
ARCHIVE="${STAGE}/cm3-scripts-${DATESTR}.tgz"
header "building CM3 scripts distribution in ${ARCHIVE}"

#-----------------------------------------------------------------------------
# build the scripts distribution archive
#

cd "${ROOT}" || exit 1
ls -1d m3overrides COPYRIGHT-CMASS COPYRIGHT-DEC scripts > .tar-include
echo "building exclude list..."
$FIND scripts \( -name '*~' -or -name '*.bak' -or -name '*.orig' -or \
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
