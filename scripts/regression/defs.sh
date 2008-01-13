
#----------------------------------------------------------------------------
# global definitions

TESTHOSTNAME=${TESTHOSTNAME:-`hostname -s`}
DS=${DS:-`date -u +'%Y-%m-%d-%H-%M-%S' | tr -d '\\n'`}
WS=${WS:-${HOME}/work/cm3-ws/${TESTHOSTNAME}-${DS}}

COVERSION=${COVERSION:-"-AP"} # version to checkout, default current

# CMINSTALL: set this to override the installer binary (full path)
# NOCLEAN: set to avoid cleaning for re-starts

LASTREL=${LASTREL:-5.4.0}
INSTBASE=${INSTBASE:-${HOME}/work/cm3-inst/${TESTHOSTNAME}}
INSTROOT_REL=${INSTROOT_REL:-${INSTBASE}/rel-${LASTREL}}
INSTROOT_LOK=${INSTROOT_OK:-${INSTBASE}/last-ok}
INSTROOT_POK=${INSTROOT_OK:-${INSTBASE}/prev-ok}
INSTROOT_CUR=${INSTROOT_CUR:-${INSTBASE}/current}

CM3CVSSERVER=${CM3CVSSERVER:-birch.elegosoft.com}
CM3CVSROOT=${CM3CVSROOT:-${CM3CVSSERVER}:/usr/cvs}

UNAME=${UNAME:-`uname`}

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
    if [ "${UNAMEM}" = "ppc" ] ; then
      CM3_TARGET=PPC_LINUX
    else
      CM3_TARGET=LINUXLIBC6
    fi
  ;;

  NetBSD*)
    CM3_OSTYPE=POSIX
    CM3_TARGET=NetBSD2_i386 # only arch/version combination supported yet
  ;;
esac

HTMP=${HTMP:-${HOME}/tmp/cm3/${TESTHOSTNAME}}

BINDISTMIN=${BINDISTMIN:-${HOME}/cm3-min-${CM3_OSTYPE}-${CM3_TARGET}-${LASTREL}.tgz}
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


#----------------------------------------------------------------------------
# checks

if type cvs >/dev/null; then
  true
else
  echo "cvs not found" 1>&2
  exit 1
fi

if ssh ${CM3CVSSERVER} true; then
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
  cvs -q -d ${CM3CVSROOT} checkout ${COVERSION} cm3 2>&1 | tee cvs-co.log
  echo " >>> OK checkout ${DS} ${WS} ${COVERSION}"
  echo " === `date -u +'%Y-%m-%d %H:%M:%S'` checkout cm3 done"
}

#----------------------------------------------------------------------------
# misc. support functions

cm3config() {
  f="$1/bin/cm3.cfg"
  if [ -d "$1" -a -f "${f}" ]; then
    if perl -p -i -e 's;^INSTALL_ROOT[ \t]*=.*$;INSTALL_ROOT = "'$1'/";' "$f";
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
    echo "cannot read ${BINDISTMIN}" 1>2
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
  INSTALLROOT=${INSTROOT_CUR}
  export LD_LIBRARY_PATH INSTALLROOT

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
    echo " === perform cm3 upgrade "
    ./scripts/upgrade.sh || exit 1
    echo " >>> OK build_${1}_upgrade ${DS} ${WS}"
  fi

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
  INSTALLROOT=${INSTROOT_CUR}
  STAGE=${HTMP}
  export LD_LIBRARY_PATH INSTALLROOT STAGE DS

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

  ./scripts/make-bin-dist-min.sh || exit 1
  echo " >>> OK make_bin_dist_lastok ${DS} ${WS}"
  echo " === `date -u +'%Y-%m-%d %H:%M:%S'` cm3 snapshot build done"
}

test_m3tests()
{
  echo " === `date -u +'%Y-%m-%d %H:%M:%S'` run cm3 compiler and runtime regression test suite in ${WS} with lastok version"
  prependpath ${INSTROOT_CUR}/bin
  LD_LIBRARY_PATH=${INSTROOT_CUR}/lib
  INSTALLROOT=${INSTROOT_CUR}
  export LD_LIBRARY_PATH INSTALLROOT

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
  ./scripts/do-pkg.sh build m3tests 2>${HTMP}/m3tests.stderr | \
                                 tee  ${HTMP}/m3tests.stdout

  find ${CM3_TARGET} -type f -name stderr.pgm -print | \
    xargs egrep '^\*\*|error.*and.*warning|fail' | grep -v '0 error' | \
    tee ${HTMP}/m3tests.stderr.extract 1>2

  echo " >>> OK test_m3tests ${DS} ${WS}"
  echo " === `date -u +'%Y-%m-%d %H:%M:%S'` cm3 m3tests run done"
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
  ( test_build_core_lastok ) || \
  echo " >>> KO: simple build with last version failed, full upgrade needed..."

  # try to build everything with the last release / perform regular upgrade
  test_build_core_rel

  # try to build a snapshot
  test_make_bin_dist

  # build all standard packages
  test_build_std_lastok

  # m3 regression tests
  test_m3tests
}

#----------------------------------------------------------------------------
# main

main()
{
  time testall 2>&1 | tee ${HTMP}/cm3-rlog-${DS}
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
