#!/bin/sh
# $Id: sysinfo.sh,v 1.81 2009-08-25 09:21:35 jkrell Exp $

if [ "$SYSINFO_DONE" != "yes" ] ; then

SYSINFO_DONE="yes"

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
# Utility function to find first occurrence of executable file in
# $PATH.
# Arguments are filename and default pathname.
# Check for argument count and protect against passing a path as a
# filename. Try to not mangle pathnames with spaces.
# 
find_exe() {
  if [ $# -eq 2 -a `basename "$1"` = "$1" ] ; then
    file="$1"
    default="$2"
    IFS=:
    for path in $PATH ; do
      if [ -f "$path/$file" -a -x "$path/$file" ]; then
       echo "$path"
       return 0
      fi
    done
    echo "$default"
  else
    echo
    return 1
  fi
}

#-----------------------------------------------------------------------------

qgrep() {
  egrep $@ >/dev/null 2>/dev/null
}

#-----------------------------------------------------------------------------
# abstraction functions
cygpath() {
  echo "$2"
}

strip_exe() {
  if [ -x /usr/ccs/bin/strip ]; then
    /usr/ccs/bin/strip $@
  else
    strip $@
  fi
}

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

#-----------------------------------------------------------------------------

UNAME=${UNAME:-`uname`}
UNAMEM=${UNAMEM:=`uname -m`}

PRJ_ROOT=${PRJ_ROOT:-${HOME}/work}

#-----------------------------------------------------------------------------
# set some defaults
#

eval `awk '{ print "default_" $1 "=" $2 }' < $root/scripts/version`

CM3VERSION=${CM3VERSION:-${default_CM3VERSION}}
CM3VERSIONNUM=${CM3VERSIONNUM:-${default_CM3VERSIONNUM}}
CM3LASTCHANGED=${CM3LASTCHANGED:-${default_CM3LASTCHANGED}}

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


CM3_GCC_BACKEND=yes
CM3_GDB=${CM3_GDB:-yes}

#
# If CM3_INSTALL is not set, it will be set to the the parent directory
# of the first path element where an executable cm3 is found.
# Otherwise CM3_INSTALL defaults to /usr/local/cm3. (Make sure dirname
# has a trailing directory to strip.)
#
CM3_INSTALL=${CM3_INSTALL:-`dirname \`find_exe cm3 /usr/local/cm3/\ \``}
CM3=${CM3:-cm3}
EXE=""
SL="/"

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
# evaluate uname information
case "${UNAME}" in

  Windows*|WinNT*|Cygwin*|CYGWIN*)
    if [ x$TARGET = xNT386GNU ] ; then
      CM3_OSTYPE=POSIX
      CM3_TARGET=NT386GNU
    else
      CM3_OSTYPE=WIN32
      CM3_TARGET=NT386
      CM3_INSTALL="c:/cm3"
      CM3_GCC_BACKEND=no
      HAVE_SERIAL=yes
      EXE=".exe"
      SL='\\\\'

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
        CM3_TARGET=I386_DARWIN;;
    esac
  ;;

  SunOS*)
    CM3_OSTYPE=POSIX
    CM3_TARGET=${CM3_TARGET:-"SOLgnu"}
    #CM3_TARGET=${CM3_TARGET:-"SOLsun"}
  ;;

  Interix*)
    CM3_OSTYPE=POSIX
    CM3_TARGET=I386_INTERIX
  ;;

  Linux*)
    CM3_OSTYPE=POSIX
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

  OpenBSD*)
    CM3_OSTYPE=POSIX
    ARCH=`arch -s`
    if [ "${UNAMEM}" = "macppc" ] ; then
      CM3_TARGET=PPC32_OPENBSD
    elif [ "${UNAMEM}" = "sparc64" ] ; then
      CM3_TARGET=SPARC64_OPENBSD
    elif [ "${ARCH}" = "mips64" ] ; then
      CM3_TARGET=MIPS64_OPENBSD
    elif [ "${ARCH}" = "i386" ] ; then
      CM3_TARGET=I386_OPENBSD
    else
      echo Update $0 for ${ARCH}
      exit 1
    fi
  ;;

  # more need to be added here, I haven't got all the platform info ready

esac

#-----------------------------------------------------------------------------
# define the exported values
if [ -n "$root" ] ; then
  ROOT=${ROOT:-${root}}
else
  ROOT=${ROOT:-${PRJ_ROOT}/cm3}
fi
CM3_ROOT=${CM3_ROOT:-${ROOT}}
M3GDB=${M3GDB:-${CM3_GDB}}
M3OSTYPE=${M3OSTYPE:-${CM3_OSTYPE}}
TARGET=${TARGET:-${CM3_TARGET}}
GCC_BACKEND=${GCC_BACKEND:-${CM3_GCC_BACKEND}}
INSTALLROOT=${INSTALLROOT:-${CM3_INSTALL}}
PKGSDB=${PKGSDB:-$ROOT/scripts/PKGS}
GREP=${GREP:-egrep}

if [ "${M3OSTYPE}" = "WIN32" ] ; then
  CM3ROOT="`cygpath -w ${ROOT} | sed -e 's;\\\;\\\\\\\\;g'`"
else
  CM3ROOT="${ROOT}"
fi

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
debug "CM3_ROOT    = $CM3_ROOT"
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
debug "TAR         = $TAR"
debug "CM3ROOT     = $CM3ROOT"
debug "CM3VERSION  = $CM3VERSION"
debug "CM3VERSIONNUM = $CM3VERSIONNUM"
debug "CM3LASTCHANGED = $CM3LASTCHANGED"

export ROOT M3GDB M3OSTYPE TARGET GCC_BACKEND INSTALLROOT PKGSDB
export GREP TMPDIR EXE SL CM3VERSION TAR
export CM3ROOT CM3 CM3_ROOT
export SYSINFO_DONE CM3VERSIONNUM CM3LASTCHANGED

fi
