#!/bin/sh

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
ROOT=${ROOT:-${PRJ_ROOT}/cm3}
SCRIPTS=${SCRIPTS:-${ROOT}/scripts}
M3GDB=${M3GDB:-${CM3_GDB}}
OSTYPE=${OSTYPE:-${CM3_OSTYPE}}
TARGET=${TARGET:-${CM3_TARGET}}
GCC_BACKEND=${GCC_BACKEND:-${CM3_GCC_BACKEND}}
INSTALLROOT=${INSTALLROOT:-${CM3_INSTALL}}

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
export ROOT SCRIPTS M3GDB OSTYPE TARGET GCC_BACKEND INSTALLROOT

