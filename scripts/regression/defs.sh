
#-----------------------------------------------------------------------------

find_in_list() {
    a="x`eval echo \\$$1`"
    if [ "$a" = "x" ]; then
        for a in $2; do
            for b in $a ${a}.exe; do
                if type $b >/dev/null 2>/dev/null; then
                    echo $1=$b
                    eval $1=$b
                    echo export $1
                    export $1
                    return
                fi
            done
        done
        echo "none of $2 found"
        exit 1
    fi
}

#----------------------------------------------------------------------------
# global definitions

# nice, but need more testing
#set -e
#set -x

#
# Look for GNU make and GNU tar.
# TODO: run them and grep for GNU tar and GNU make
#
# /usr/pkg is NetBSD default
# /usr/sfw is Solaris default (Sun FreeWare)
# /usr/local is FreeBSD and OpenBSD default and popular otherwise
#

find_in_list GMAKE "gmake gnumake /usr/pkg/bin/gmake /usr/sfw/bin/gmake /usr/local/gmake /usr/local/gnumake make" || exit 1
find_in_list TAR "gtar gnutar /usr/pkg/bin/gtar /usr/sfw/bin/gtar /usr/local/gtar /usr/local/gnutar tar" || exit 1

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

if [ "x$CLEAN" = "xtrue" ]; then # Hudson CLEAN support
  unset NOCLEAN || true
else
  NOCLEAN=yes
  export NOCLEAN
fi

# last release for installation
LASTREL=${LASTREL:-5.8.6}

# CM3 installation directories
INSTBASE=${INSTBASE:-${HOME}/work/cm3-inst/${TESTHOSTNAME}}
INSTROOT_REL=${INSTROOT_REL:-${INSTBASE}/rel-${LASTREL}}
INSTROOT_LOK=${INSTROOT_LOK:-${INSTBASE}/last-ok}
INSTROOT_POK=${INSTROOT_POK:-${INSTBASE}/prev-ok}
INSTROOT_CUR=${INSTROOT_CUR:-${INSTBASE}/current}
export INSTBASE INSTROOT_CUR INSTROOT_REL INSTROOT_POK INSTROOT_LOK

# repository definitions
if test "x${CM3CVSUSER}" = "x"; then
  CM3CVSUSER_AT=""
else
  CM3CVSUSER_AT="${CM3CVSUSER}@"
fi
CM3CVSSERVER=${CM3CVSSERVER:-${CM3CVSUSER_AT}birch.elegosoft.com}
CM3CVSROOT=${CM3CVSROOT:-${CM3CVSSERVER}:/usr/cvs}

# WWW server site
WWWSERVER=${WWWSERVER:-${CM3CVSUSER_AT}birch.elegosoft.com}
export WWWSERVER CM3CVSUSER CM3CVSSERVER CM3CVSROOT

# the whole test log
RLOG=${RLOG:-${HTMP}/cm3-rlog-${DS}}
# number of last run results to keep (for cleanup in main)
CM3_NKEEP=${CM3_NKEEP:-7}

#-----------------------------------------------------------------------------

TMP=${TMP:-/tmp}
TMPDIR=${TMPDIR:-${TMP}}
if [ ! -d "${TMPDIR}" ]; then
  TMPDIR="${HTMP}"
fi
export TMPDIR

# default TARGET and CM3_TARGET to each other
TARGET=${TARGET:-$CM3_TARGET}
CM3_TARGET=${CM3_TARGET:-$TARGET}

if [ "x$CM3_TARGET" = "x$TARGET" ] ; then :; else
  echo "if TARGET and CM3_TARGET are both set, they must be equal ($TARGET, $CM3_TARGET)"
  exit 1
fi

if [ "x$CM3_TARGET" = "x" ] ; then
  case "`uname`" in

    Windows*|WinNT*|Cygwin*|CYGWIN*) CM3_TARGET=NT386;;
                                     #CM3_TARGET=I386_NT;;

    OSF1) CM3_TARGET=ALPHA_OSF;;

    FreeBSD)
      case "`uname -p`" in
        amd64)  CM3_TARGET=AMD64_FREEBSD;;
        i386)   CM3_TARGET=FreeBSD4;;
                #CM3_TARGET=I386_FREEBSD;;
      esac;;

    Darwin)
      case "`uname -p`" in
        powerpc)
          CM3_TARGET=PPC_DARWIN
          rm -rf ./m3ppc64
          echo "int main() { return 0; }" | (gcc -arch ppc64 -x c - -o ./m3ppc64) || true
          if ./m3ppc64 2/dev/null; then
            true
            #CM3_TARGET=PPC64_DARWIN
          fi
          rm -rf ./m3ppc64;;
        i386)
          if [ "x`sysctl hw.cpu64bit_capable`" = "xhw.cpu64bit_capable: 1" ]; then
            CM3_TARGET=AMD64_DARWIN
          else
            CM3_TARGET=I386_DARWIN
          fi;;
      esac;;

    SunOS)
      case "`uname -p`" in
          i86pc)
            case "`isainfo`" in
              *amd64*) CM3_TARGET=AMD64_SOLARIS;;
              *) CM3_TARGET=I386_SOLARIS;;
            esac;;
          sparc)
            case "`isainfo`" in
              #*sparcv9*) CM3_TARGET=SPARC64_SOLARIS;;
              *) CM3_TARGET=SOLgnu;;
                 #CM3_TARGET=SOLsun;;
                 #CM3_TARGET=SPARC32_SOLARIS;;
            esac;;
      esac;;

    Interix) CM3_TARGET=I386_INTERIX;;

    Linux)
      case "`uname -m`" in
        alpha)   CM3_TARGET=ALPHA_LINUX;;
        ppc)     CM3_TARGET=PPC_LINUX;;
        ppc64)   CM3_TARGET=PPC_LINUX;; # ?
        x86_64)  CM3_TARGET=AMD64_LINUX;;
        sparc64) CM3_TARGET=SPARC32_LINUX;;
        i*86)    CM3_TARGET=LINUXLIBC6;;
                 #CM3_TARGET=I386_LINUX;;
      esac;;

    NetBSD)
        case "`uname -m`" in
          x86_64) CM3_TARGET=AMD64_NETBSD;;
          amd64) CM3_TARGET=AMD64_NETBSD;;
          i386) CM3_TARGET=I386_NETBSD;;
        esac;;

    OpenBSD)
      case "`arch -s`" in
        alpha)    CM3_TARGET=ALPHA_OPENBSD;;
        powerpc)  CM3_TARGET=PPC32_OPENBSD;;
        x86_64)   CM3_TARGET=AMD64_OPENBSD;;
        amd64)    CM3_TARGET=AMD64_OPENBSD;;
        sparc64)  CM3_TARGET=SPARC64_OPENBSD;;
        mips64)   CM3_TARGET=MIPS64_OPENBSD;;
        mips64el) CM3_TARGET=MIPS64EL_OPENBSD;;
        i386)     CM3_TARGET=I386_OPENBSD;;
      esac;;
  esac
fi

if [ "x$CM3_TARGET" = "x" ] ; then
  echo "$0 does not know about `uname -a`"
  exit 1
fi

CM3_OSTYPE=POSIX

case "$CM3_TARGET" in
  *DARWIN) CM3_GDB=no;;
  NT386|I386_NT) CM3_OSTYPE=WIN32;;
esac

export CM3_OSTYPE CM3_TARGET

# files for result aggregation of m3-sys/m3tests
M3TOUT=${M3TOUT:-${HTMP}/m3tests-${DS}.stdout}
M3TERR=${M3TERR:-${HTMP}/m3tests-${DS}.stderr}
export M3TOUT M3TERR

# the next three are just for documentation and cleanup
CM3_VERSION=${CM3_VERSION:-"*"}
CM3_SNAPSHOT=${CM3_SNAPSHOT:-"${HTMP}/cm3-min-${CM3_OSTYPE}-${CM3_TARGET}-${CM3_VERSION}-${DS}.tgz"}
HTML_REPORT="${HTML_REPORT:-${TMPDIR}/cm3-pkg-report-${CM3_TARGET}-${DS}.html}"

# the binary installation archive to install from
BINDISTMIN_NAME=${BINDISTMIN_NAME:-"cm3-bin-min-${CM3_TARGET}-${LASTREL}.tgz"}
BINDISTMIN_LOC=${BINDISTMIN_LOC-"${HOME}/work"}
BINDISTMIN=${BINDISTMIN:-"${BINDISTMIN_LOC}/${BINDISTMIN_NAME}"}
BINDISTMIN_URL=${BINDISTMIN_URL:-"http://modula3.elegosoft.com/cm3/releng"}
export BINDISTMIN_NAME BINDISTMIN_LOC BINDISTMIN_URL BINDISTMIN

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

trace_cm3_installations() {
  echo "INSTROOT_CUR = ${INSTROOT_CUR}"
  ls -l "${INSTROOT_CUR}/bin/cm3*"
  [ -x "${INSTROOT_CUR}/bin/cm3" ] && {
    "${INSTROOT_CUR}/bin/cm3" -version
  }
  [ -x "${INSTROOT_CUR}/bin/cm3cg" ] && {
    echo "" "${INSTROOT_CUR}/bin/cm3cg" -version 2>&1 | grep -v 'fatal'
  }
  echo ""
  echo "INSTROOT_LOK = ${INSTROOT_LOK}"
  ls -l "${INSTROOT_LOK}/bin/cm3*"
  [ -x "${INSTROOT_LOK}/bin/cm3" ] && {
    "${INSTROOT_LOK}/bin/cm3" -version
  }
  [ -x "${INSTROOT_LOK}/bin/cm3cg" ] && {
    echo "" "${INSTROOT_LOK}/bin/cm3cg" -version 2>&1 | grep -v 'fatal'
  }
  echo ""
  echo "INSTROOT_REL = ${INSTROOT_REL}"
  ls -l "${INSTROOT_REL}/bin/cm3*"
  [ -x "${INSTROOT_REL}/bin/cm3" ] && {
    "${INSTROOT_REL}/bin/cm3" -version
  }
  [ -x "${INSTROOT_REL}/bin/cm3cg" ] && {
    echo "" "${INSTROOT_REL}/bin/cm3cg" -version 2>&1 | grep -v 'fatal'
  }
  echo ""
}

check_cm3_installations() {
  if type cm3 > /dev/null; then
    true
  else
    echo "cm3 not found" 1>&2
    trace_cm3_installations
    exit 1
  fi
  cm3 -version || {
    trace_cm3_installations
    exit 1
  }
  if [ "$CM3_OSTYPE" != "WIN32" ]; then
    if type cm3cg > /dev/null; then
      true
    else
      echo "cm3cg not found" 1>&2
      trace_cm3_installations
      exit 1
    fi
  fi
}

#----------------------------------------------------------------------------
# checks

if type cvs >/dev/null; then
  true
else
  echo "cvs not found" 1>&2
  exit 1
fi

SSH="ssh"; export SSH
if [ "$TESTHOSTNAME" = "current10s" -o "$TESTHOSTNAME" = "current9s" -o \
     "$TESTHOSTNAME" = "current10x" -o "$TESTHOSTNAME" = "current9x" ]; then
  SSH="ssh login.opencsw.org ssh -l hudson"
  ship_www() {
    # $1: file to ship (absolute path) (exactly one)
    # WWWDEST: destination path to /var/www/modula3.elegosoft.com/
    WWWDEST=${WWWDEST:-${WWWSERVER}:/var/www/modula3.elegosoft.com/}
    if [ ! -f "$1" ]; then
      echo "$1 not found" 1>&2
      return 1
    elif [ ! -r "$1" ]; then
      echo "$1 not readable" 1>&2
      return 1
    fi
    echo ssh login.opencsw.org scp "$1" "hudson@${WWWDEST}"
    ssh login.opencsw.org scp "$1" "hudson@${WWWDEST}" < /dev/null
  }
else
  ship_www() {
    # $1: file to ship (exactly one)
    # WWWDEST: destination path to /var/www/modula3.elegosoft.com/
    WWWDEST=${WWWDEST:-${WWWSERVER}:/var/www/modula3.elegosoft.com/}
    if [ ! -f "$1" ]; then
      echo "$1 not found" 1>&2
      return 1
    elif [ ! -r "$1" ]; then
      echo "$1 not readable" 1>&2
      return 1
    fi
    echo scp "$1" "${WWWDEST}"
    scp "$1" "${WWWDEST}" < /dev/null
  }
fi
echo "testing ${SSH} ${CM3CVSSERVER}..."
if ${SSH} ${CM3CVSSERVER} true; then
  echo "${SSH} ${CM3CVSSERVER} ok"
  true
else
  echo "no ssh connection to ${CM3CVSSERVER}" 1>&2
  exit 1
fi

if [ ! -d "${HTMP}" ]; then
  if mkdir -p "${HTMP}"; then
    true
  else
    echo "cannot mkdir -p ${HTMP}" 1>&2
    exit 1
  fi
fi


#----------------------------------------------------------------------------
# path functions

prependpathelem()
{
  # $1 elem, $2 path
  echo "${1}:${2}"
}

prependpath()
{
  # $1 elem to prepend to the path
  PATH="$1:$PATH"
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
  echo "cvs -z1 -q -d ${CM3CVSROOT} checkout ${COVERSION} cm3"
  cvs -z1 -q -d ${CM3CVSROOT} checkout ${COVERSION} cm3 >cvs-co.log 2>&1
  rc=$?
  cat cvs-co.log
  if [ $rc = 0 ]; then
    echo " >>> OK checkout ${DS} ${WS} ${COVERSION}"
    echo " === `date -u +'%Y-%m-%d %H:%M:%S'` checkout cm3 done"
  else
    echo " === `date -u +'%Y-%m-%d %H:%M:%S'` checkout cm3 failed"
  fi
  return $rc
}

#----------------------------------------------------------------------------
# misc. support functions

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
  echo cleanup_all_but_last_n
  dirs=`all_but_last_n $1`
  echo cleanup_all_but_last_n ${dirs}
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
  echo done with cleanup_all
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
  BSET="core"
  if [ "$1" = "std" ]; then
    BSET="std"
  fi
  prependpath ${INSTROOT_CUR}/bin
  LD_LIBRARY_PATH=${INSTROOT_CUR}/lib
  DYLD_LIBRARY_PATH=${LD_LIBRARY_PATH}
  INSTALLROOT=${INSTROOT_CUR}
  export LD_LIBRARY_PATH DYLD_LIBRARY_PATH INSTALLROOT

  check_cm3_installations

  # checkout must have been done before
  if cd "${WS}/cm3"; then
    true
  else
    echo "cannot cd to ${WS}/cm3" 1>&2
    exit 1
  fi

  BUILDSCRIPT="./scripts/do-cm3-${BSET}.sh"

  echo " === clean up before cm3 upgrade "
  if [ -z "$NOCLEAN" ]; then
    OMIT_GCC=yes $BUILDSCRIPT realclean || exit 1
    ./scripts/do-pkg.sh realclean cminstall
  fi
  echo " === perform cm3 upgrade "
  ./scripts/upgrade.sh || exit 1
  echo " >>> OK build_${1}_upgrade ${DS} ${WS}"

  echo " === build ${BSET} system with current compiler"
  if [ -z "$NOCLEAN" ]; then
    OMIT_GCC=yes $BUILDSCRIPT realclean || exit 1
  fi
  $BUILDSCRIPT buildship || exit 1
  echo " >>> OK build_${1}_${BSET} ${DS} ${WS}"

  echo " === build intermediate lastok in ${INSTROOT_LOK}.$$"
  [ -d ${INSTROOT_LOK}.$$ ] && rm -rf ${INSTROOT_LOK}.$$
  [ -d ${INSTROOT_LOK} ] && cp -pR ${INSTROOT_LOK} ${INSTROOT_LOK}.$$
  mkdir -p ${INSTROOT_LOK}.$$
  [ -d ${INSTROOT_CUR} ] && cp -pR ${INSTROOT_CUR}/* ${INSTROOT_LOK}.$$

  if [ -d ${INSTROOT_LOK} -a -d ${INSTROOT_POK} ]; then
    echo " === remove previous ok version at ${INSTROOT_POK}"
    rm -rf ${INSTROOT_POK}
  fi

  echo " === move last ok version at ${INSTROOT_LOK} to previous ok version"
  if [ -d "${INSTROOT_LOK}" ]; then
    mv ${INSTROOT_LOK} ${INSTROOT_POK}
  else
    echo " === no installation in ${INSTROOT_LOK}"
  fi

  echo " === move intermediate lastok ${INSTROOT_LOK}.$$ to ${INSTROOT_LOK}"
  mv ${INSTROOT_LOK}.$$ ${INSTROOT_LOK} || {
    sleep 30
    echo "update of ${INSTROOT_LOK} failed... trying to restore..."
    [ -d ${INSTROOT_POK} -a ! -d ${INSTROOT_LOK} ] && \
    mv ${INSTROOT_POK} ${INSTROOT_LOK} || {
      echo "restore of ${INSTROOT_LOK} failed!" 1>&2
    }
    # try again later...
    sleep 30
    [ -d ${INSTROOT_POK} -a ! -d ${INSTROOT_LOK} ] && \
    mv ${INSTROOT_POK} ${INSTROOT_LOK} || {
      echo "HELP: restore of ${INSTROOT_LOK} failed!" 1>&2
    }
  }
  echo " >>> OK build_${1}_${BSET}_lastok ${DS} ${WS} ${INSTROOT_LOK}"
  echo " === `date -u +'%Y-%m-%d %H:%M:%S'` cm3 release build done"
}

test_build_system()
{
  prependpath ${INSTROOT_CUR}/bin
  LD_LIBRARY_PATH=${INSTROOT_CUR}/lib
  DYLD_LIBRARY_PATH=${LD_LIBRARY_PATH}
  INSTALLROOT=${INSTROOT_CUR}
  export LD_LIBRARY_PATH DYLD_LIBRARY_PATH INSTALLROOT

  check_cm3_installations

  # checkout must have been done before
  if cd "${WS}/cm3"; then
    true
  else
    echo "cannot cd to ${WS}/cm3" 1>&2
    exit 1
  fi

  echo " === build core system with current compiler"
  echo type cm3cg
       type cm3cg
  echo cm3cg -version
       cm3cg -version </dev/null
  echo type cm3
       type cm3
  echo cm3 -version
       cm3 -version
  BUILDSCRIPT="./scripts/do-cm3-core.sh"
  if [ -z "$NOCLEAN" ]; then
    echo OMIT_GCC=yes $BUILDSCRIPT realclean
         OMIT_GCC=yes $BUILDSCRIPT realclean || exit 1
  fi
  echo " === perform cm3 upgrade after cleaning everything"
  echo OMIT_GCC=yes $BUILDSCRIPT realclean || exit 1
       OMIT_GCC=yes $BUILDSCRIPT realclean || exit 1
  ./scripts/upgrade.sh || exit 1
  echo " >>> OK build_upgrade ${DS} ${WS}"

  echo " === build intermediate lastok in ${INSTROOT_LOK}.$$"
  [ -d ${INSTROOT_LOK}.$$ ] && rm -rf ${INSTROOT_LOK}.$$
  [ -d ${INSTROOT_LOK} ] && cp -pR ${INSTROOT_LOK} ${INSTROOT_LOK}.$$
  mkdir -p ${INSTROOT_LOK}.$$
  [ -d ${INSTROOT_CUR} ] && cp -pR ${INSTROOT_CUR}/* ${INSTROOT_LOK}.$$

  if [ -d ${INSTROOT_LOK} -a -d ${INSTROOT_POK} ]; then
    echo " === remove previous ok version at ${INSTROOT_POK}"
    rm -rf ${INSTROOT_POK}
  fi

  echo " === move last ok version at ${INSTROOT_LOK} to previous ok version"
  if [ -d "${INSTROOT_LOK}" ]; then
    mv ${INSTROOT_LOK} ${INSTROOT_POK}
  else
    echo " === no installation in ${INSTROOT_LOK}"
  fi

  echo " === move intermediate lastok ${INSTROOT_LOK}.$$ to ${INSTROOT_LOK}"
  mv ${INSTROOT_LOK}.$$ ${INSTROOT_LOK} || {
    sleep 30
    echo "update of ${INSTROOT_LOK} failed... trying to restore..."
    [ -d ${INSTROOT_POK} -a ! -d ${INSTROOT_LOK} ] && \
    mv ${INSTROOT_POK} ${INSTROOT_LOK} || {
      echo "restore of ${INSTROOT_LOK} failed!" 1>&2
    }
    # try again later...
    sleep 30
    [ -d ${INSTROOT_POK} -a ! -d ${INSTROOT_LOK} ] && \
    mv ${INSTROOT_POK} ${INSTROOT_LOK} || {
      echo "HELP: restore of ${INSTROOT_LOK} failed!" 1>&2
    }
  }
  echo " >>> OK build_system ${DS} ${WS} ${INSTROOT_LOK}"
  echo " === `date -u +'%Y-%m-%d %H:%M:%S'` cm3 build done"
}

test_build_core_lastok()
{
  echo " === `date -u +'%Y-%m-%d %H:%M:%S'` build cm3 core in ${WS} with lastok version"
  rm -rf "${INSTROOT_CUR}"
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
  rm -rf "${INSTROOT_CUR}"
  cp -pR ${INSTROOT_REL} ${INSTROOT_CUR}
  test_build_current "rel"
}

test_build_std_lastok()
{
  echo " === `date -u +'%Y-%m-%d %H:%M:%S'` build cm3 std in ${WS} with lastok version"
  rm -rf "${INSTROOT_CUR}"
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
  rm -rf "${INSTROOT_CUR}"
  cp -pR ${INSTROOT_LOK} ${INSTROOT_CUR}

  prependpath ${INSTROOT_CUR}/bin
  LD_LIBRARY_PATH=${INSTROOT_CUR}/lib
  DYLD_LIBRARY_PATH=${LD_LIBRARY_PATH}
  INSTALLROOT=${INSTROOT_CUR}
  STAGE=${STAGE:-${HTMP}}
  export LD_LIBRARY_PATH DYLD_LIBRARY_PATH INSTALLROOT STAGE DS

  check_cm3_installations

  # checkout must have been done before
  if cd "${WS}/cm3"; then
    true
  else
    echo "cannot cd to ${WS}/cm3" 1>&2
    exit 1
  fi

  DOSHIP=yes
  . ./scripts/make-bin-dist-min.sh || exit 1
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
    echo "cannot cd to ${WS}/cm3" 1>&2
    exit 1
  fi

  export STAGE
  DOSHIP=yes ./scripts/make-src-dist-snapshots.sh || exit 1
  echo " >>> OK make_src_dist_snapshots ${DS} ${WS}"
  echo " === `date -u +'%Y-%m-%d %H:%M:%S'` cm3 source snapshot build done"
}

# NT has \windows\system32\find.exe, completely different
FIND=find
if [ -x /usr/bin/find ] ; then
  FIND=/usr/bin/find
fi

test_m3tests()
{
  echo " === `date -u +'%Y-%m-%d %H:%M:%S'` run cm3 compiler and runtime regression test suite in ${WS} with lastok version"
  prependpath ${INSTROOT_CUR}/bin
  LD_LIBRARY_PATH=${INSTROOT_CUR}/lib:${WS}/cm3/m3-sys/m3tests/${CM3_TARGET}
  DYLD_LIBRARY_PATH=${LD_LIBRARY_PATH}
  INSTALLROOT=${INSTROOT_CUR}
  export LD_LIBRARY_PATH DYLD_LIBRARY_PATH INSTALLROOT

  check_cm3_installations

  # checkout must have been done before
  if cd "${WS}/cm3"; then
    true
  else
    echo "cannot cd to ${WS}/cm3" 1>&2
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
  $FIND ${CM3_TARGET} -type f -name stderr.pgm -print | \
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

  check_cm3_installations

  # checkout must have been done before
  if cd "${WS}/cm3"; then
    true
  else
    echo "cannot cd to ${WS}/cm3" 1>&2
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
    echo "cannot cd to ${WS}/cm3/m3-tools/m3tohtml" 1>&2
    exit 1
  fi

  pkgs=`awk '{print $1}' ${WS}/cm3/scripts/pkginfo.txt | xargs -n 1 basename`
  echo pwd
  pwd
  echo ls
  ls
  echo m3tohtml -dir html $pkgs
  yes | m3tohtml -dir html $pkgs
  res=$?
  
  if [ 0 = "${res}" ]; then
    if [ "${TESTHOSTNAME}" = "birch.elegosoft.com" ]; then
      if [ "x${USER}" = "xm3" ]; then
        DOCDEST=/var/www/modula3.elegosoft.com/cm3/doc/help/gen_html
        if [ -d "${DOCDEST}" ]; then
          mv html "${DOCDEST}.new"
          mv "${DOCDEST}" "${DOCDEST}.old" && 
          mv "${DOCDEST}.new" "${DOCDEST}" &&
          rm -rf "${DOCDEST}.old"
        fi
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
install_latest_release() {
  if [ ! -r "${BINDISTMIN}" ]; then
    ( download_bin_dist ) || {
      echo "downloading ${BINDISTMIN_URL} failed" 1>&2
      echo "continuing nonetheless..."
    }
  fi
  ( install_bin_dist ) || {
    echo "installation ${BINDISTMIN} of into ${INSTROOT_REL} failed" 1>&2
    echo "continuing nonetheless..."
  }
}

# install last release if needed
if [ ! -d "${INSTROOT_REL}" ]; then
  if [ -z "$HUDSON_HOME" ]; then
    install_latest_release
  fi
fi

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
  echo done with main
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
