#!/bin/sh
# $Id: sysinfo.sh,v 1.47.2.1 2006-10-11 15:44:11 stsp Exp $

if [ "$SYSINFO_DONE" != "yes" ] ; then

SYSINFO_DONE="yes"

UNAME=${UNAME:-`uname`}
UNAMEM=${UNAMEM:=`uname -m`}

PRJ_ROOT=${PRJ_ROOT:-${HOME}/work}

#-----------------------------------------------------------------------------
# set some defaults
CM3VERSION=${CM3VERSION:-"5.4-0"}
CM3_GCC_BACKEND=yes
CM3_GDB=no
CM3_INSTALL=/usr/local/cm3
CM3=${CM3:-cm3}
M3BUILD=${M3BUILD:-m3build}
M3SHIP=${M3SHIP:-m3ship}
EXE=""
SL="/"
SYSLIBDIR="/usr/local/cm3/lib"
SYSLIBS=""
XDEV_LIB=""
XDEV_BIN=""
TAR=tar
if [ -z "${M3GC_ENHANCED}" -o "x${M3GC_ENHANCED}" = "xno" -o \
     "x${M3GC_ENHANCED}" = "xNO" -o \
     "x${M3GC_ENHANCED}" = "xN" -o \
     "x${M3GC_ENHANCED}" = "x0" ]; then
  M3GC_SIMPLE=yes
fi
export M3GC_SIMPLE

if [ -z "$TMPDIR" -o ! -d "$TMPDIR" ] ; then
  if [ -n "$TMP" -a -d "$TMP" ] ; then
    TMPDIR="$TMP"
  elif [ -n "$TEMP" -a -d "$TEMP" ] ; then
    TMPDIR="$TEMP"
  elif [ -d "/var/tmp" ] ; then
    TMPDIR=/var/tmp
  elif [ -d "/usr/tmp" ] ; then
    TMPDIR=/usr/tmp
  elif [ -d "/tmp" ] ; then
    TMPDIR=/tmp
  elif [ -d "c:/tmp" ] ; then
    TMPDIR="c:/tmp"
  elif [ -d "d:/tmp" ] ; then
    TMPDIR="d:/tmp"
  elif [ -d "e:/tmp" ] ; then
    TMPDIR="e:/tmp"
  elif [ -d "c:/temp" ] ; then
    TMPDIR="c:/temp"
  elif [ -d "d:/temp" ] ; then
    TMPDIR="d:/temp"
  elif [ -d "e:/temp" ] ; then
    TMPDIR="e:/temp"
  else
    echo "please define TMPDIR" 1>&2
    exit 1
  fi
fi

#-----------------------------------------------------------------------------
# some localization functions
find_dir() {
  for d in $@ ; do
    if [ -d "$d" ] ; then
      echo "$d"
      return 0
    fi
  done
  return 1
}

find_file() {
  f="$1"
  shift
  for d in $@ ; do
    if [ -d "$d" -a -f "$d/$f" ] ; then
      echo "$d/$f"
      return 0
    fi
  done
  echo "$f"
  return 1
}

#-----------------------------------------------------------------------------
# abstraction functions
cygpath() {
  echo "$2"
}

strip_exe() {
  strip $@
}

#-----------------------------------------------------------------------------
# evaluate uname information
GCWRAPFLAGS=""
export GCWRAPFLAGS
case "${UNAME}" in

  Windows*|WinNT*|Cygwin*|CYGWIN*)
    if [ x$TARGET = xNT386GNU ] ; then
      CM3_OSTYPE=POSIX
      CM3_TARGET=NT386GNU
      GMAKE=${GMAKE:-make}
    else
      CM3_OSTYPE=WIN32
      CM3_TARGET=NT386
      CM3_INSTALL="c:/cm3"
      CM3_GCC_BACKEND=no
      HAVE_SERIAL=yes
      EXE=".exe"
      SL='\\\\'
      SYSLIBS="ADVAPI32.LIB GDI32.LIB KERNEL32.LIB ODBC32.LIB"
      SYSLIBS="${SYSLIBS} OPENGL32.LIB WSOCK32.LIB COMDLG32.LIB"
      SYSLIBS="${SYSLIBS} GLU32.LIB NETAPI32.LIB ODBCCP32.LIB USER32.LIB"
      L="c:/cm3/lib d:/cm3/lib e:/cm3/lib c:/reactor5/lib d:/reactor5/lib"
      L="${L} e:/reactor5/lib c:/reactor/lib d:/reactor/lib"
      L="${L} e:/reactor/lib /usr/local/cm3/lib /usr/local/reactor/lib"
      L="${L} /usr/cm3/lib /usr/reactor/lib"
      CM3LIBSEARCHPATH="${L}"
      CM3BINSEARCHPATH="`echo ${L} | sed -e 's/lib/bin/g'`"
      if f="`find_file KERNEL32.LIB ${L}`" ; then
        SYSLIBDIR="`dirname $f`"
      else
        SYSLIBDIR="unknown"
      fi
      D="c:/msdev/bin d:/msdev/bin e:/msdev/bin f:/msdev/bin g:/msdev/bin"
      if f="`find_file cl.exe ${D}`" ; then
        XDEV_BIN="`dirname ${f}`"
        XDEV_LIB="`dirname ${XDEV_BIN}`/lib"
      else
        XDEV_LIB=""
        XDEV_BIN=""
      fi
      if [ -f /usr/bin/tar.exe ] ; then
        TAR=/usr/bin/tar.exe
      fi
      GMAKE=${GMAKE:-make}

      cygpath() {
        /usr/bin/cygpath $@
      }
      strip_exe() {
        return 0;
      }
    fi
  ;;

  NT386GNU*)
    CM3_OSTYPE=POSIX
    CM3_TARGET=NT386GNU
    GMAKE=${GMAKE:-make}
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
    # detect the m3 platform (Darwin runs on ppc and ix86
    case "`uname -p`" in
      powerpc*)
        CM3_TARGET=PPC_DARWIN;;
      i[3456]86*)
        M3GC_SIMPLE=yes
        export M3GC_SIMPLE
        CM3_TARGET=I386_DARWIN;;
    esac
    # disable system call wrappers and enhanced gc on older systems
    case "`uname -r`" in
      6.8)
        M3GC_SIMPLE=yes
        export M3GC_SIMPLE;;
    esac
    GMAKE=${GMAKE:-make}
  ;;

  SunOS*)
    CM3_OSTYPE=POSIX
    CM3_TARGET=SOLgnu
    #CM3_TARGET=SOLsun
  ;;

  Linux*)
    CM3_OSTYPE=POSIX
    GMAKE=${GMAKE:-make}
    GCWRAPFLAGS="-Wl,--wrap,adjtime,--wrap,getdirentries,--wrap,readv,--wrap,utimes,--wrap,wait3"
    if [ "${UNAMEM}" = "ppc" ] ; then
      CM3_TARGET=PPC_LINUX
    else
      CM3_TARGET=LINUXLIBC6
    fi
  ;;

  NetBSD*)
    CM3_OSTYPE=POSIX
    GMAKE=${GMAKE:-gmake}
    CM3_TARGET=NetBSD2_i386 # only arch/version combination supported yet
  ;;
  # more need to be added here, I haven't got all the platform info ready
esac

DEV_BIN=${DEV_BIN:-${XDEV_BIN}}
DEV_LIB=${DEV_LIB:-${XDEV_LIB}}

#-----------------------------------------------------------------------------
# define the exported values
if [ -n "$root" ] ; then
  ROOT=${ROOT:-${root}}
else
  ROOT=${ROOT:-${PRJ_ROOT}/cm3}
fi
SCRIPTS=${SCRIPTS:-${ROOT}/scripts}
M3GDB=${M3GDB:-${CM3_GDB}}
M3OSTYPE=${M3OSTYPE:-${CM3_OSTYPE}}
TARGET=${TARGET:-${CM3_TARGET}}
GCC_BACKEND=${GCC_BACKEND:-${CM3_GCC_BACKEND}}
INSTALLROOT=${INSTALLROOT:-${CM3_INSTALL}}
PKGSDB=${PKGSDB:-$ROOT/scripts/PKGS}
GREP=${GREP:-egrep}
GMAKE=${GMAKE:-gmake}

qgrep() {
  egrep $@ >/dev/null 2>/dev/null
}

if [ "${M3OSTYPE}" = "WIN32" ] ; then
  CM3ROOT="`cygpath -w ${ROOT} | sed -e 's;\\\;\\\\\\\\;g'`"
else
  CM3ROOT="${ROOT}"
fi


#-----------------------------------------------------------------------------
# output functions
debug() {
  if [ -n "$CM3_DEBUG" ] ; then
    echo "$*"
  fi
}

header() {
  echo ""
  echo '----------------------------------------------------------------------------'
  echo $@
  echo '----------------------------------------------------------------------------'
  echo ""
}

#-----------------------------------------------------------------------------
# elego customizations
#
# comment these if they interfere with your environment
if type domainname > /dev/null 2>/dev/null && \
   [ "${M3OSTYPE}" = "POSIX" -a \
     "`domainname 2>/dev/null`" = "elegoohm" ] ; then
  STAGE=${STAGE:-/pub/lang/m3/cm3-dist}
  export STAGE
fi
if [ "${M3OSTYPE}" = "WIN32" -a "${HOSTNAME}" = "FIR" ] ; then
  STAGE=${STAGE:-c:/tmp/cm3stage}
  export STAGE
fi

#-----------------------------------------------------------------------------
# debug output
debug "ROOT        = $ROOT"
debug "SCRIPTS     = $SCRIPTS"
debug "M3GDB       = $M3GDB"
debug "M3OSTYPE    = $M3OSTYPE"
debug "TARGET      = $TARGET"
debug "GCC_BACKEND = $GCC_BACKEND"
debug "INSTALLROOT = $INSTALLROOT"
debug "PKGSDB      = $PKGSDB"
debug "GREP        = $GREP"
debug "GMAKE       = $GMAKE"
debug "TMPDIR      = $TMPDIR"
debug "EXE         = $EXE"
debug "SL          = $SL"
debug "SYSLIBDIR   = $SYSLIBDIR"
debug "SYSLIBS     = $SYSLIBS"
debug "DEV_BIN     = $DEV_BIN"
debug "DEV_LIB     = $DEV_LIB"
debug "TAR         = $TAR"
debug "CM3ROOT     = $CM3ROOT"

export ROOT SCRIPTS M3GDB M3OSTYPE TARGET GCC_BACKEND INSTALLROOT PKGSDB
export GREP TMPDIR EXE SL CM3VERSION SYSLIBDIR SYSLIB DEV_BIN DEV_LIB TAR
export CM3LIBSEARCHPATH CM3BINSEARCHPATH CM3ROOT
export SYSINFO_DONE

fi
