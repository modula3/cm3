
#----------------------------------------------------------------------------
# global definitions

# nice, but need more testing
#set -e
#set -x

# our hostname
TESTHOSTNAME=${TESTHOSTNAME:-`hostname | sed -e 's/\..*//'`}
# a user-local temporary files directory
HTMP=${HTMP:-${HOME}/tmp/cm3/${TESTHOSTNAME}}

# datestamp and workspace
DS=${DS:-`date -u +'%Y-%m-%d-%H-%M-%S' | tr -d '\\n'`}
WS=${WS:-${HOME}/work/cm3-ws/${TESTHOSTNAME}-${DS}}

# version to check out from repository (usually the main trunk's head)
COVERSION=${COVERSION:-"-AP"} # version to checkout, default current

# CMINSTALL: set this to override the installer binary (full path)
# NOCLEAN: set to avoid cleaning for re-starts

# last release for installation
LASTREL=${LASTREL:-5.4.0}

# CM3 installation directories
INSTBASE=${INSTBASE:-${HOME}/work/cm3-inst/${TESTHOSTNAME}}
INSTROOT_REL=${INSTROOT_REL:-${INSTBASE}/rel-${LASTREL}}
INSTROOT_LOK=${INSTROOT_OK:-${INSTBASE}/last-ok}
INSTROOT_POK=${INSTROOT_OK:-${INSTBASE}/prev-ok}
INSTROOT_CUR=${INSTROOT_CUR:-${INSTBASE}/current}

# repository definitions
if test "x${CM3CVSUSER}" != "x"; then
  CM3CVSUSER_AT="${CM3CVSUSER}@"
else
  CM3CVSUSER_AT=""
fi
CM3CVSSERVER=${CM3CVSSERVER:-${CM3CVSUSER_AT}birch.elegosoft.com}
CM3CVSROOT=${CM3CVSROOT:-${CM3CVSSERVER}:/usr/cvs}

# WWW server site
WWWSERVER=${WWWSERVER:-birch.elegosoft.com}

# the whole test log
RLOG=${RLOG:-${HTMP}/cm3-rlog-${DS}}
# number of last run results to keep (for cleanup in main)
CM3_NKEEP=${CM3_NKEEP:-7}

UNAME=${UNAME:-`uname`}
UNAMEM=${UNAMEM:-`uname -m`}

TMP=${TMP:-/tmp}
TMPDIR=${TMPDIR:-${TMP}}
if [ ! -d "${TMPDIR}" ]; then
  TMPDIR="${HTMP}"
fi

case "${UNAME}" in

  Windows*|WinNT*|Cygwin*|CYGWIN*)
    if [ x$TARGET = xNT386GNU ] ; then
      CM3_OSTYPE=POSIX
      CM3_TARGET=NT386GNU
    else
      CM3_OSTYPE=WIN32
      CM3_TARGET=NT386
    fi
  ;;

  NT386GNU*)
    CM3_OSTYPE=POSIX
    CM3_TARGET=NT386GNU
  ;;

  FreeBSD*)
    CM3_OSTYPE=POSIX
    if [ "`uname -m`" = i386 ] ; then
      case "`uname -r`" in
        1*) CM3_TARGET=FreeBSD;;
        2*) CM3_TARGET=FreeBSD2;;
        3*) CM3_TARGET=FreeBSD3;;
        4*) CM3_TARGET=FreeBSD4;;
        *) CM3_TARGET=FreeBSD4;;
      esac
    else
      CM3_TARGET=FBSD_ALPHA
    fi
  ;;

  Darwin*)
    CM3_OSTYPE=POSIX
    case "`uname -p`" in
      powerpc*)
        CM3_TARGET=PPC_DARWIN;;
      i[3456]86*)
        CM3_TARGET=I386_DARWIN;;
    esac
  ;;

  SunOS*)
    CM3_OSTYPE=POSIX
    CM3_TARGET=SOLgnu
  ;;

  Linux*)
    CM3_OSTYPE=POSIX
    GMAKE=${GMAKE:-make}
    if [ "${UNAMEM}" = "ppc" ] ; then
      CM3_TARGET=PPC_LINUX
    elif [ "${UNAMEM}" = "x86_64" ] ; then
      CM3_TARGET=AMD64_LINUX
    elif [ "${UNAMEM}" = "sparc64" ] ; then
      CM3_TARGET=SPARC32_LINUX
    else
      CM3_TARGET=LINUXLIBC6
    fi
  ;;

  NetBSD*)
    CM3_OSTYPE=POSIX
    CM3_TARGET=NetBSD2_i386 # only arch/version combination supported yet
  ;;
esac

# files for result aggregation of m3-sys/m3tests
M3TOUT=${M3TOUT:-${HTMP}/m3tests-${DS}.stdout}
M3TERR=${M3TERR:-${HTMP}/m3tests-${DS}.stderr}

# the next three are just for documentation and cleanup
CM3_VERSION=${CM3_VERSION:-"*"}
CM3_SNAPSHOT=${CM3_SNAPSHOT:-"${HTMP}/cm3-min-${CM3_OSTYPE}-${CM3_TARGET}-${CM3_VERSION}-${DS}.tgz"}
HTML_REPORT="${HTML_REPORT:-${TMPDIR}/cm3-pkg-report-${CM3_TARGET}-${DS}.html}"

# the binary installation archive to install from
BINDISTMIN_NAME=${BINDISTMIN_NAME:-"cm3-min-${CM3_OSTYPE}-${CM3_TARGET}-${LASTREL}.tgz"}
BINDISTMIN_LOC=${BINDISTMIN_LOC-"${HOME}/work"}
BINDISTMIN=${BINDISTMIN:-"${BINDISTMIN_LOC}/${BINDISTMIN_NAME}"}
BINDISTMIN_URL=${BINDISTMIN_URL:-"http://modula3.elegosoft.com/cm3"}

# display some important settings
echo "TESTHOSTNAME=${TESTHOSTNAME}"
echo "WS=${WS}"
echo "LASTREL=${LASTREL}"
echo "INSTROOT_REL=${INSTROOT_REL}"
echo "INSTROOT_POK=${INSTROOT_POK}"
echo "INSTROOT_LOK=${INSTROOT_LOK}"
echo "INSTROOT_CUR=${INSTROOT_CUR}"
echo "CM3_OSTYPE=${CM3_OSTYPE}"
echo "CM3_TARGET=${CM3_TARGET}"
echo "BINDISTMIN=${BINDISTMIN}"
echo "CM3CVSSERVER=${CM3CVSSERVER}"
echo "CM3CVSROOT=${CM3CVSROOT}"
echo "BINDISTMIN_NAME=${BINDISTMIN_NAME}"
echo "BINDISTMIN=${BINDISTMIN}"
echo "CM3CVSUSER=${CM3CVSUSER}"

#----------------------------------------------------------------------------
# checks

if type cvs >/dev/null; then
  true
else
  echo "cvs not found" 1>&2
  exit 1
fi

echo "testing ssh ${CM3CVSSERVER}.."
if ssh ${CM3CVSSERVER} true; then
  echo "ssh ${CM3CVSSERVER} ok"
  true
else
  echo "no ssh connection to ${CM3CVSSERVER}" 1>&2
  exit 1
fi

if [ ! -d "${HTMP}" ]; then
  if mkdir -p "${HTMP}"; then
    true
  else
    echo "cannot mkdir -p ${HTMP}" 1>2
    exit 1
  fi
fi


#----------------------------------------------------------------------------
# path functions

pathelems()
{
  echo $1 | tr ':' ' '
}

makepath()
{
  #local p
  p="$1"
  shift
  while [ -n "$1" ] ; do
    p="${p}:${1}"
    shift
  done
  echo $p
}

  # $1 elem, $2 path
delpathelem()
{
  #local e
  #local p
  p=""
  if [ -z "$1" ] ; then
    return
  fi
  if [ -z "$2" ] ; then
    return
  fi
  for e in `pathelems $2` ; do
    if [ "$1" != "$e" ] ; then
      if [ -z "$p" ] ; then
         p=$e
      else
         p="${p}:${e}"
      fi
    fi
  done
  echo $p
}

prependpathelem()
{
  # $1 elem, $2 path
  echo "${1}:${2}"
}

appendpathelem()
{
  # $1 elem, $2 path
  echo "${2}:${1}"
}

delpath()
{
  # $1 elem to delete from path
  PATH=`delpathelem $1 $PATH`
  export PATH
}

appendpath()
{
  # $1 elem to append to the path
  PATH=`appendpathelem $1 $PATH`
  export PATH
}

prependpath()
{
  # $1 elem to append to the path
  PATH=`prependpathelem $1 $PATH`
  export PATH
}

#----------------------------------------------------------------------------
# CVS functions

checkout()
{
  echo " === `date -u +'%Y-%m-%d %H:%M:%S'` checkout cm3 to ${WS}"
  if [ ! -d "${WS}" ]; then
    mkdir -p "${WS}"
  fi
  cd "${WS}" || exit 1
  (cvs -z3 -q -d ${CM3CVSROOT} checkout ${COVERSION} cm3 2>&1 | tee cvs-co.log) || exit 1
  echo " >>> OK checkout ${DS} ${WS} ${COVERSION}"
  echo " === `date -u +'%Y-%m-%d %H:%M:%S'` checkout cm3 done"
}

#----------------------------------------------------------------------------
# misc. support functions

cm3config() {
  f="$1/bin/cm3.cfg"
  if [ "$CM3_TARGET" = "NT386" ]; then
    R=`cygpath -w $1 | sed -e 's/\\\\/\\\\\\\\\\\\\\\\/g'`
    SL='\\\\'
  else
    R="$1"
    SL=/
  fi
  if [ -d "$1" -a -f "${f}" ]; then
    if perl -p -i -e 's;^INSTALL_ROOT[ \t]*=.*$;INSTALL_ROOT = "'${R}${SL}'";' "$f";
      then true
    else
      echo "INSTALL_ROOT substitution failed for ${f}" 1>2
      exit 1
    fi
  else
    echo "no cm3 installation in $1" 1>2
    exit 1
  fi
}

logfilter() {
  egrep ' >>> | === 2'
}

lines() {
  wc | awk '{print $1}'
}

all_but_last_n() {
  n=$1
  t=${TMPDIR}/all_but_$$
  cat > ${t}
  m=`cat ${t} | lines`
  if [ $m -gt $n ]; then
    d=`expr $m - $n`
    head -n $d ${t}
  fi
  rm -f ${t}
}

cleanup_all_but_last_n() { 
  # Beware! This may ruin your disk with wrong standard input...
  dirs=`all_but_last_n $1`
  echo ${dirs} | xargs rm -rf
}

cleanup_all() {
  if [ -z "$1" ]; then
    n=3
  else
    n=$1
  fi

  echo "cleaning CM3 workspaces..."
  pat=`echo "${WS}" | sed -e "s/${DS}/*/"`
  echo "${pat}"
  ls -1d ${pat} | cleanup_all_but_last_n ${n}
  echo

  echo "cleaning regression test log files..."
  pat=`echo "${RLOG}" | sed -e "s/${DS}/*/"`
  echo "${pat}"
  ls -1d ${pat} | cleanup_all_but_last_n ${n}
  echo

  echo "cleaning m3test log files..."
  pat=`echo "${M3TOUT}" | sed -e "s/${DS}/*/"`
  echo "${pat}"
  ls -1d ${pat} | cleanup_all_but_last_n ${n}
  echo

  pat=`echo "${M3TERR}" | sed -e "s/${DS}/*/"`
  echo "${pat}"
  ls -1d ${pat} | cleanup_all_but_last_n ${n}
  echo

  pat=`echo "${M3TERR}.extract" | sed -e "s/${DS}/*/"`
  echo "${pat}"
  ls -1d ${pat} | cleanup_all_but_last_n ${n}
  echo

  echo "cleaning snapshot files..."
  pat=`echo "${CM3_SNAPSHOT}" | sed -e "s/${DS}/*/"`
  echo "${pat}"
  ls -1d ${pat} | cleanup_all_but_last_n ${n}
  echo

  echo "cleaning package reports..."
  pat=`echo "${HTML_REPORT}" | sed -e "s/${DS}/*/"`
  echo "${pat}"
  ls -1d ${pat} | cleanup_all_but_last_n ${n}
  echo
}


#----------------------------------------------------------------------------
# installation

install_bin_dist() {
  echo " === `date -u +'%Y-%m-%d %H:%M:%S'` installing ${BINDISTMIN} into ${INSTROOT_REL}"
  if [ -x "${INSTROOT_REL}/bin/cm3" ]; then
    echo " === nothing to be done; remove directory for re-installation"
    return
  fi
  mkdir -p "${INSTROOT_REL}" || exit 1
  cd "${HTMP}" || exit 1
  if [ ! -r "${BINDISTMIN}" ]; then
    echo "cannot read ${BINDISTMIN}! Maybe try \"( . defs.sh ; download_bin_dist )\"."
    exit 1
  fi
  tar xzf "${BINDISTMIN}"
  if [ -f "${CMINSTALL}" ]; then
    echo " === using ${CMINSTALL} for installation"
    cp "${CMINSTALL}" ./cminstall
  fi
  if ./cminstall -help | grep interactive; then
    ./cminstall "${INSTROOT_REL}"
  else
    echo "manual installation required. Install into ${INSTROOT_REL}!"
    ./cminstall 
  fi
  if [ "$?" = 0 ]; then
    echo " >>> OK install ${DS} ${BINDISTMIN} ${INSTROOT_REL} ${CMINSTALL}"
  fi
  echo " === `date -u +'%Y-%m-%d %H:%M:%S'` installation done"
}

download_bin_dist() {
  echo " === `date -u +'%Y-%m-%d %H:%M:%S'` downloading ${BINDISTMIN_URL}/${BINDISTMIN_NAME} into ${BINDISTMIN_LOC}"

  mkdir -p "${BINDISTMIN_LOC}" || true
  if type wget >/dev/null; then
    wget "${BINDISTMIN_URL}/${BINDISTMIN_NAME}" -O "${BINDISTMIN_LOC}/${BINDISTMIN_NAME}"
  else
    (cd "${BINDISTMIN_LOC}"; ftp "${BINDISTMIN_URL}/${BINDISTMIN_NAME}")
  fi
  
  if [ "$?" = 0 ]; then
    echo " >>> OK"
  fi
  echo " === `date -u +'%Y-%m-%d %H:%M:%S'` downloading done"
}

#----------------------------------------------------------------------------
# tests

test_build_current() # this in an internal function: $1 = rel | lastok | std
{
  if [ "$1" = "std" ]; then
    BSET="std"
  else
    BSET="core"
  fi
  cm3config ${INSTROOT_CUR}
  prependpath ${INSTROOT_CUR}/bin
  LD_LIBRARY_PATH=${INSTROOT_CUR}/lib
  DYLD_LIBRARY_PATH=${LD_LIBRARY_PATH}
  INSTALLROOT=${INSTROOT_CUR}
  export LD_LIBRARY_PATH DYLD_LIBRARY_PATH INSTALLROOT

  if type cm3 > /dev/null; then
    true
  else
    echo "cm3 not found" 1>&2
    exit 1
  fi

  cm3 -version

  # checkout must have been done before
  if cd "${WS}/cm3"; then
    true
  else
    echo "cannot cd to ${WS}/cm3" 1>2
    exit 1
  fi

  if [ "$1" = "rel" ]; then
    echo " === clean up before cm3 upgrade "
    if [ -z "$NOCLEAN" ]; then
      OMIT_GCC=yes ./scripts/do-cm3-core.sh realclean || exit 1
      ./scripts/do-pkg.sh realclean cminstall
    fi
    echo " === perform cm3 upgrade "
    UPGRADE_CM3_CFG=yes ./scripts/upgrade.sh || exit 1
    echo " >>> OK build_${1}_upgrade ${DS} ${WS}"
  fi

  echo pwd
  pwd
  echo ls
  ls
  echo ls ./scripts
  ls ./scripts

  echo " === build ${BSET} system with current compiler"
  BUILDSCRIPT="./scripts/do-cm3-${BSET}.sh"
  if [ "$1" = "rel" ]; then
    if [ -z "$NOCLEAN" ]; then
      OMIT_GCC=yes ./scripts/do-cm3-core.sh realclean || exit 1
    fi
  else
    if [ -z "$NOCLEAN" ]; then
      $BUILDSCRIPT realclean || exit 1
    fi
  fi
  $BUILDSCRIPT buildship || exit 1
  echo " >>> OK build_${1}_${BSET} ${DS} ${WS}"

  if [ -d ${INSTROOT_POK} ]; then
    echo " === remove previous ok version at ${INSTROOT_POK}"
    rm -rf ${INSTROOT_POK}
  fi

  echo " === move last ok version at ${INSTROOT_LOK} to previous ok version"
  if [ -d "${INSTROOT_POK}" ]; then
    mv ${INSTROOT_POK} ${INSTROOT_POK}-rm
  else
    echo " === no installation in ${INSTROOT_POK}"
  fi
  if [ -d "${INSTROOT_LOK}" ]; then
    mv ${INSTROOT_LOK} ${INSTROOT_POK}
    cm3config ${INSTROOT_POK}
  else
    echo " === no installation in ${INSTROOT_LOK}"
  fi
  if [ -d "${INSTROOT_POK}-rm" ]; then
    rm -rf ${INSTROOT_POK}-rm
  fi

  echo " === update last ok from ${INSTROOT_CUR}"
  cp -pR ${INSTROOT_CUR} ${INSTROOT_LOK}
  cm3config ${INSTROOT_LOK}
  echo " >>> OK build_${1}_${BSET}_lastok ${DS} ${WS} ${INSTROOT_LOK}"
  echo " === `date -u +'%Y-%m-%d %H:%M:%S'` cm3 release build done"
}

test_build_core_lastok()
{
  echo " === `date -u +'%Y-%m-%d %H:%M:%S'` build cm3 core in ${WS} with lastok version"
  if [ -d "${INSTROOT_CUR}" ]; then
    rm -rf ${INSTROOT_CUR}
  fi
  if [ -d "${INSTROOT_LOK}" ]; then
    cp -pR ${INSTROOT_LOK} ${INSTROOT_CUR}
    test_build_current "lastok"
  else
    echo " >>> no last-ok version in ${INSTROOT_LOK}; skipping test"
  fi
}

test_build_core_rel()
{
  echo " === `date -u +'%Y-%m-%d %H:%M:%S'` build cm3 core in ${WS} with last release ${INSTROOT_REL}"
  if [ -d "${INSTROOT_CUR}" ]; then
    rm -rf ${INSTROOT_CUR}
  fi
  cp -pR ${INSTROOT_REL} ${INSTROOT_CUR}
  test_build_current "rel"
}

test_build_std_lastok()
{
  echo " === `date -u +'%Y-%m-%d %H:%M:%S'` build cm3 std in ${WS} with lastok version"
  if [ -d "${INSTROOT_CUR}" ]; then
    rm -rf ${INSTROOT_CUR}
  fi
  if [ -d "${INSTROOT_LOK}" ]; then
    cp -pR ${INSTROOT_LOK} ${INSTROOT_CUR}
    test_build_current "std"
  else
    echo " >>> no last-ok version in ${INSTROOT_LOK}; skipping test"
  fi
}

test_make_bin_dist()
{
  echo " === `date -u +'%Y-%m-%d %H:%M:%S'` build cm3 bindist snapshot in ${WS} with lastok version"
  if [ -d "${INSTROOT_CUR}" ]; then
    rm -rf ${INSTROOT_CUR}
  fi
  cp -pR ${INSTROOT_LOK} ${INSTROOT_CUR}

  cm3config ${INSTROOT_CUR}
  prependpath ${INSTROOT_CUR}/bin
  LD_LIBRARY_PATH=${INSTROOT_CUR}/lib
  DYLD_LIBRARY_PATH=${LD_LIBRARY_PATH}
  INSTALLROOT=${INSTROOT_CUR}
  STAGE=${HTMP}
  export LD_LIBRARY_PATH DYLD_LIBRARY_PATH INSTALLROOT STAGE DS

  if type cm3 > /dev/null; then
    true
  else
    echo "cm3 not found" 1>&2
    exit 1
  fi

  cm3 -version

  # checkout must have been done before
  if cd "${WS}/cm3"; then
    true
  else
    echo "cannot cd to ${WS}/cm3" 1>2
    exit 1
  fi

  DOSHIP=yes ./scripts/make-bin-dist-min.sh || exit 1
  echo " >>> OK make_bin_dist_lastok ${DS} ${WS}"
  echo " === `date -u +'%Y-%m-%d %H:%M:%S'` cm3 snapshot build done"
}

make_src_dist_snapshots()
{
  echo " === `date -u +'%Y-%m-%d %H:%M:%S'` build cm3 source dist snapshots in ${WS}"
  STAGE=${HTMP}
  # checkout must have been done before
  if cd "${WS}/cm3"; then
    true
  else
    echo "cannot cd to ${WS}/cm3" 1>2
    exit 1
  fi

  export STAGE
  DOSHIP=yes ./scripts/make-src-dist-snapshots.sh || exit 1
  echo " >>> OK make_src_dist_snapshots ${DS} ${WS}"
  echo " === `date -u +'%Y-%m-%d %H:%M:%S'` cm3 source snapshot build done"
}

test_m3tests()
{
  echo " === `date -u +'%Y-%m-%d %H:%M:%S'` run cm3 compiler and runtime regression test suite in ${WS} with lastok version"
  prependpath ${INSTROOT_CUR}/bin
  LD_LIBRARY_PATH=${INSTROOT_CUR}/lib:${WS}/cm3/m3-sys/m3tests/${CM3_TARGET}
  DYLD_LIBRARY_PATH=${LD_LIBRARY_PATH}
  INSTALLROOT=${INSTROOT_CUR}
  export LD_LIBRARY_PATH DYLD_LIBRARY_PATH INSTALLROOT

  if type cm3 > /dev/null; then
    true
  else
    echo "cm3 not found" 1>&2
    exit 1
  fi

  cm3 -version

  # checkout must have been done before
  if cd "${WS}/cm3"; then
    true
  else
    echo "cannot cd to ${WS}/cm3" 1>2
    exit 1
  fi

  cd "${WS}/cm3/m3-sys/m3tests" || exit 1

  cm3 -build -DHTML 2>&1 | tee ${M3TERR}
  WWWDEST=${WWWDEST:-${WWWSERVER}:/var/www/modula3.elegosoft.com/cm3/m3tests}
  if [ -r "m3tests.html" ]; then
    scp "m3tests.html" "${WWWDEST}/m3tests-${CM3_TARGET}-${DS}.html" \
      < /dev/null
  fi
  
  echo " >>> test_m3tests error extract:"
  find ${CM3_TARGET} -type f -name stderr.pgm -print | \
    xargs egrep '^\*\*|error.*and.*warning|fail' | grep -v '0 error' | \
    tee ${M3TERR}.extract 1>&2

  if [ -r ${CM3_TARGET}/res.ko ]; then
    nerrs=`wc ${CM3_TARGET}/res.ko | awk '{print $1}'`
    echo " >>> failed tests: `cat ${CM3_TARGET}/res.ko | xargs`"
  else
    # try old approach
    nerrs=`awk -F: '{print $1}' ${M3TERR}.extract | sort -u | 
           wc | awk '{print $1}'`
  fi
  if [ 0 = "${nerrs}" ]; then
    echo " >>> OK test_m3tests ${DS} ${WS}"
    echo " === `date -u +'%Y-%m-%d %H:%M:%S'` cm3 m3tests run done"
    true
  else
    echo " >>> ${nerrs} in test_m3tests ${DS} ${WS}"
    echo " === `date -u +'%Y-%m-%d %H:%M:%S'` cm3 m3tests run done"
    false
  fi
}


test_m3_all_pkgs()
{
  echo " === `date -u +'%Y-%m-%d %H:%M:%S'` build all packages and generate report in ${WS} with lastok version"
  prependpath ${INSTROOT_CUR}/bin
  LD_LIBRARY_PATH=${INSTROOT_CUR}/lib
  DYLD_LIBRARY_PATH=${LD_LIBRARY_PATH}
  INSTALLROOT=${INSTROOT_CUR}
  export LD_LIBRARY_PATH DYLD_LIBRARY_PATH INSTALLROOT

  if type cm3 > /dev/null; then
    true
  else
    echo "cm3 not found" 1>&2
    exit 1
  fi

  cm3 -version

  # checkout must have been done before
  if cd "${WS}/cm3"; then
    true
  else
    echo "cannot cd to ${WS}/cm3" 1>2
    exit 1
  fi

  cd "${WS}/cm3/" || exit 1

  DOSHIP=yes ./scripts/do-cm3-all.sh -k -report buildship
  res=$?
  
  if [ 0 = "${res}" ]; then
    echo " >>> OK test_m3_all_pkgs ${DS} ${WS}"
    echo " === `date -u +'%Y-%m-%d %H:%M:%S'` cm3 package status run done"
    true
  else
    echo " >>> ${res} errors in test_m3_all_pkgs ${DS} ${WS}"
    echo " === `date -u +'%Y-%m-%d %H:%M:%S'` cm3 package status run done"
    false
  fi
}


test_m3tohtml()
{
  echo " === `date -u +'%Y-%m-%d %H:%M:%S'` build HTML package doc in ${WS} with lastok version"
  prependpath ${INSTROOT_CUR}/bin
  LD_LIBRARY_PATH=${INSTROOT_CUR}/lib
  DYLD_LIBRARY_PATH=${LD_LIBRARY_PATH}
  INSTALLROOT=${INSTROOT_CUR}
  export LD_LIBRARY_PATH DYLD_LIBRARY_PATH INSTALLROOT

  if type m3tohtml > /dev/null; then
    true
  else
    echo "m3tohtml not found" 1>&2
    exit 1
  fi

  # checkout must have been done before
  if cd "${WS}/cm3/m3-tools/m3tohtml"; then
    true
  else
    echo "cannot cd to ${WS}/cm3/m3-tools/m3tohtml" 1>2
    exit 1
  fi

  pkgs=`awk '{print $1}' ${WS}/cm3/scripts/pkginfo.txt | xargs -n 1 basename`
  yes | m3tohtml -dir html $pkgs
  res=$?
  
  if [ 0 = "${res}" ]; then
    if [ "${TESTHOSTNAME}" = "birch.elegosoft.com" -a `who -m | cut -d ' ' -f1` = "m3" ]; then
      DOCDEST=/var/www/modula3.elegosoft.com/cm3/doc/help/gen_html
      if [ -d "${DOCDEST}" ]; then
        mv html "${DOCDEST}.new"
        mv "${DOCDEST}" "${DOCDEST}.old" && 
        mv "${DOCDEST}.new" "${DOCDEST}" &&
        rm -rf "${DOCDEST}.old"
      fi
    fi
    echo " >>> OK test_m3tohtml ${DS} ${WS}"
    echo " === `date -u +'%Y-%m-%d %H:%M:%S'` m3tohtml run done"
    true
  else
    echo " >>> errors in test_m3tohtml ${DS} ${WS}"
    echo " === `date -u +'%Y-%m-%d %H:%M:%S'` m3tohtml run done"
    false
  fi
}


#----------------------------------------------------------------------------
# testall -- checkout and perform all tests

testall()
{
  # checkout into new workspace
  if [ -z "${NOCO}" ]; then
    checkout
  fi
  # build everything with the last-ok version
  if ( test_build_core_lastok ); then
    true
  else
    echo " >>> KO: simple build with last version failed, full upgrade needed..."
    true
  fi

  # try to build everything with the last release / perform regular upgrade
  test_build_core_rel

  # try to build a snapshot
  test_make_bin_dist

  # build all standard packages
  test_build_std_lastok

  # m3 regression tests
  test_m3tests

  # m3 all package regression tests and reports
  test_m3_all_pkgs

  # test m3tohtml
  test_m3tohtml
}

#----------------------------------------------------------------------------
# standard tests for cm3.build
std_tests() {
  ( test_make_bin_dist ) 2>&1
  r0=$?
  ( test_build_std_lastok ) 2>&1
  r1=$?
  ( test_m3tests ) 2>&1
  r2=$?
  ( test_m3_all_pkgs ) 2>&1
  r3=$?
  ( test_m3tohtml ) 2>&1
  r4=$?
  return `expr ${r0} + ${r1} + ${r2} + ${r3} + ${r4}`
}

#----------------------------------------------------------------------------
# main

main()
{
  time testall 2>&1 | tee ${RLOG}
  cleanup_all ${CM3_NKEEP}
}

#----------------------------------------------------------------------------
# sample use in a regression test script
##!/bin/sh
#
#(
#  cd ${HOME}/work/cm3
#  #export CM3CVSSERVER=localhost # if local replica exists
#  . ./scripts/regression/defs.sh
#  main 2>&1 | logfilter
#) | mail -s "CM3 regression test from `hostname` at `date`" test@my.org
