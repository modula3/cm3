#!/bin/sh
# $Id$

if [ "$SYSINFO_DONE" = "yes" ] ; then
  return 0
fi
SYSINFO_DONE="yes"

UNAME=`uname`

PRJ_ROOT=${PRJ_ROOT:-${HOME}/work}

# set some defaults
CM3_GCC_BACKEND=yes
CM3_GDB=no
CM3_INSTALL=/usr/local/cm3

# evaluate uname information
case "${UNAME}" in

  Windows*|WinNT*|Cygwin*)
    CM3_OSTYPE=WIN32
    CM3_TARGET=NT386
    CM3_INSTALL="c:/cm3"
    CM3_GCC_BACKEND=no
    HAVE_SERIAL=yes
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

  SunOS*)
    CM3_OSTYPE=POSIX
    CM3_TARGET=SOLgnu
    #CM3_TARGET=SOLsun
  ;;

  Linux*)
    CM3_OSTYPE=POSIX
    CM3_TARGET=LINUXLIBC6
  ;;

  # more need to be added here, I haven't got all the platform info ready
esac

# define the exported values
if [ -n "$root" ] ; then
  ROOT=${ROOT:-${root}}
else
  ROOT=${ROOT:-${PRJ_ROOT}/cm3}
fi
SCRIPTS=${SCRIPTS:-${ROOT}/scripts}
M3GDB=${M3GDB:-${CM3_GDB}}
OSTYPE=${OSTYPE:-${CM3_OSTYPE}}
TARGET=${TARGET:-${CM3_TARGET}}
GCC_BACKEND=${GCC_BACKEND:-${CM3_GCC_BACKEND}}
INSTALLROOT=${INSTALLROOT:-${CM3_INSTALL}}
PKGSDB=${PKGSDB:-$ROOT/scripts/PKGS}
QGREP=${QGREP:-"egrep >/dev/null 2>/dev/null"}
GREP=${GREP:-egrep}

debug() {
  if [ -n "$CM3_DEBUG" ] ; then
    echo "$*"
  fi
}

debug "ROOT        = $ROOT"
debug "SCRIPTS     = $SCRIPTS"
debug "M3GDB       = $M3GDB"
debug "OSTYPE      = $OSTYPE"
debug "TARGET      = $TARGET"
debug "GCC_BACKEND = $GCC_BACKEND"
debug "INSTALLROOT = $INSTALLROOT"
debug "PKGSDB      = $PKGSDB"
debug "QGREP       = $QGREP"
debug "GREP        = $GREP"

export ROOT SCRIPTS M3GDB OSTYPE TARGET GCC_BACKEND INSTALLROOT PKGSDB QGREP
export GREP
export SYSINFO_DONE

