#!/bin/sh
# $Id$

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
. "$ROOT/scripts/pkginfo.sh"
. "$ROOT/scripts/pkgcmds.sh"

P=""
for p in $@ ; do
  if pkg_defined "$p" ; then
    NP=`pkgpath "${p}"`
    echo "adding package ${NP}"
    P="${P} ${NP}"
  elif [ -d "${ROOT}/${p}" ] ; then
    echo "adding directory ${p}"
    P="${P} ${p}"
  elif [ -f "${ROOT}/${p}" ] ; then
    echo "adding file ${p}"
    P="${P} ${p}"
  else
    echo "${p} not found, skipping" 1>&2
  fi
done

DATESTR=`date +"%Y-%m-%d"`
STAGE="${STAGE:-${TMPDIR}}"
[ -d "${STAGE}" ] || mkdir "${STAGE}" || mkdir -p "${STAGE}" || exit 1
ARCHIVE="${STAGE}/src-update-${DATESTR}.tgz"
header "building CM3 src-update distribution in ${ARCHIVE}"
echo "using packages $P"

#-----------------------------------------------------------------------------
# build the scripts distribution archive
#

cd "${ROOT}" || exit 1
/bin/ls -1d COPYRIGHT-CMASS COPYRIGHT-DEC ${P} > .tar-include
echo "building exclude list..."
for p in ${P} ; do
  find ${p} \( -name '*~' -or -name '*.bak' -or -name '*.orig' -or \
            -name '*.rej'  -or -name 'cvs-nq-up' -or -name '*-diffs' -or \
            \( -name 'CVS' -a -type d \) \) -print | \
    sed -e 's;^./;;' >> .tar-exclude
done

echo "archiving..."
export GZIP="-9 -v"
${TAR} -czf ${ARCHIVE} --files-from .tar-include --exclude-from .tar-exclude \
 || exit 1
ls -l ${ARCHIVE}
echo "done"
exit 0

