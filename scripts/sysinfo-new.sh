#!/bin/sh

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

check2() {
    # $1 and $2 are names of variables that mean the same thing and default to each other
    # $3 optionally is what to default them both to, if neither are set
    # $a is the value of $1
    # $b is the value of $2

    a="`eval echo \\$$1`"
    b="`eval echo \\$$2`"

    if [ "xx$a$b" = "xx" ]; then
        if [ "x" = "x$3" ]; then
            echo "neither $1 nor $2 are set, ok"
        else
            echo "defaulting both $1 and $2 to $3, ok"
            eval $1=$3
            eval $2=$3
            export $1
            export $2
        fi
        return
    fi
    if [ "x$a" = "x" ]; then
        echo "$1 is not set and $2 is, defaulting $1 to $2 ($b)"
        eval $1=$b
        export $1
        return
    fi
    if [ "x$b" = "x" ]; then
        echo "$2 is not set and $1 is, defaulting $2 to $1 ($a)"
        eval $2=$a
        export $2
        return
    fi
    if [ "$a" = "$b" ]; then
        echo "$1 and $2 both set and equal ($a), ok"
        return
    fi
    echo "if $1 and $2 are both set, they must be equal but they are $a and $b"
    exit 1
}

#-----------------------------------------------------------------------------

find_in_list() {
    a="x`eval echo \\$$1`"
    if [ "$a" = "x" ]; then
        for a in $2; do
            for b in $a ${a}.exe; do
                if type $b >/dev/null 2>/dev/null; then
                    eval $1=$b
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

PRJ_ROOT=${PRJ_ROOT:-${HOME}/work}

#-----------------------------------------------------------------------------
# set some defaults
#

eval `awk '{ print "default_" $1 "=" $2 }' < $root/scripts/version`

CM3VERSION=${CM3VERSION:-${default_CM3VERSION}}
CM3VERSIONNUM=${CM3VERSIONNUM:-${default_CM3VERSIONNUM}}
CM3LASTCHANGED=${CM3LASTCHANGED:-${default_CM3LASTCHANGED}}

check2 TARGET CM3_TARGET || exit 1
check2 M3GDB CM3_GDB yes || exit 1
check2 M3OSTYPE CM3_OSTYPE || exit 1

#
# If neither CM3_INSTALL nor INSTALLROOT is set, they will be set to the the parent directory
# of the first path element where an executable cm3 is found.
# Otherwise default to /usr/local/cm3. (Make sure dirname
# has a trailing directory to strip.)
#

if [ "xx$CM3_INSTALL$INSTALLROOT" = "xx" ]; then
    CM3_INSTALL=`dirname \`find_exe cm3 /usr/local/cm3/\ \``
    INSTALLROOT=$CM3_INSTALL
    echo defaulting CM3_INSTALL and INSTALLROOT to $CM3_INSTALL
else
    check2 CM3_INSTALL INSTALLROOT || exit 1
fi

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

# The vast majority of platforms, overridden later:
CM3_GCC_BACKEND=yes
CM3_OSTYPE=POSIX
EXE=""
SL="/"

CM3=${CM3:-cm3}

# NT has \windows\system32\find.exe, completely different
FIND=find
if [ -x /usr/bin/find ] ; then
  FIND=/usr/bin/find
fi

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

#
# If on NT and OSTYPE and/or BACKEND are set and TARGET isn't:
#

if [ "x$CM3_TARGET$OS" = "xWindows_NT" ]; then

    case "a${CM3_OSTYPE}b${CM3_GCC_BACKEND}" in

        # Each value has two values plus not set, for 3x3 = 9 possibilities.

        # default to native if OSTYPE=WIN32 and gcc backend not specified or no,
        # or OSTYPE not specified and gcc backend is no

        ab|aWIN32b|aWIN32bno|abno)
            CM3_TARGET=NT386
            ;;

        # WIN32 + gcc backend or unspecified + gcc backend

        aWIN32byes|abyes)
            CM3_TARGET=NT386MINGNU
            ;;

        # POSIX and gcc backend not specified or yes

        aPOSIXb|aPOSIXbyes)
            CM3_TARGET=NT386GNU
            ;;

        aPOSIXbno)
            echo "ERROR: On NT, OSTYPE=POSIX implies gcc backend"
            exit 1
            ;;
    esac
fi

# now translate CM3_TARGET on NT back to NT386 plus the other variables

case "x$CM3_TARGET" in
    xI386_NT|xNT386|xI386_MINGW|xNT386MINGNU)
        case "$CM3_TARGET" in
            I386_NT|NT386)
                CM3_GCC_BACKEND=no
                ;;
            I386_MINGW|NT386MINGNU)
                CM3_GCC_BACKEND=yes
                ;;
        esac

        CM3_TARGET=NT386

        CM3_INSTALL=${CM3_INSTALL:-"c:/cm3"}
        CM3_OSTYPE=WIN32
        CM3_GDB=no
        CM3_TARGET=NT386
        HAVE_SERIAL=yes
        EXE=".exe"
        SL='\\\\'

        cygpath() {
            /usr/bin/cygpath $@
        }

        strip_exe() {
            return 0;
        }
        ;;

    xI386_CYGWIN|xNT386GNU)
        CM3_TARGET=NT386
        CM3_OSTYPE=POSIX
        HAVE_SERIAL=yes
        EXE=".exe"
        ;;
esac

if [ "x$CM3_TARGET" = "x" ]; then
  case "`uname`" in
    FreeBSD)
      case "`uname -m`" in
        i386) CM3_TARGET=I386_FREEBSD;;
        amd64)  CM3_TARGET=AMD64_FREEBSD;;
        x86_64) CM3_TARGET=AMD64_FREEBSD;;
        alpha) CM3_TARGET=ALPHA_FREEBSD;;
      esac;;

    Darwin)
      case "`uname -p`" in
        powerpc)
          CM3_TARGET=PPC_DARWIN;;
          rm -rf ./m3ppc64
          echo "int main() { return 0; }" | (gcc -arch ppc64 -x c - -o ./m3ppc64) || true
          if ./m3ppc64 2/dev/null; then
            #CM3_TARGET=PPC64_DARWIN;;
          fi;;
          rm -rf ./m3ppc64
        i386)
          if [ "x`sysctl hw.cpu64bit_capable`" = "xhw.cpu64bit_capable: 1" ]; then
            CM3_TARGET=AMD64_DARWIN
          else
            CM3_TARGET=I386_DARWIN
          fi;;
      esac;;

    SunOS)
      case "`uname -p`" in
        i386)
          case "`isainfo`" in
            *amd64*) CM3_TARGET=AMD64_SOLARIS;;
            *) CM3_TARGET=I386_SOLARIS;;
          esac;;
        sparc)
          case "`isainfo`" in
            *sparcv9*) CM3_TARGET=SPARC64_SOLARIS;;
            *) CM3_TARGET=SPARC32_SOLARIS;;
               #CM3_TARGET=SOLsun;;
               #CM3_TARGET=SOLgnu;;
          esac;;
      esac;;

    Interix) CM3_TARGET=I386_INTERIX;;

    Linux)
      case "`uname -m`" in
        alpha) CM3_TARGET=ALPHA_LINUX;;
        ppc) CM3_TARGET=PPC_LINUX;;
        x86_64) CM3_TARGET=AMD64_LINUX;;
         amd64) CM3_TARGET=AMD64_LINUX;;
        sparc64) CM3_TARGET=SPARC32_LINUX;;
        i*86)    CM3_TARGET=I386_LINUX;;
                 #CM3_TARGET=LINUXLIBC6;;
      esac;;

    NetBSD)
      case "`uname -m`" in
        x86_64) CM3_TARGET=AMD64_NETBSD;;
         amd64) CM3_TARGET=AMD64_NETBSD;;
        i386) CM3_TARGET=I386_NETBSD;;
      esac;;

    OpenBSD)
      case "`arch -s`" in
        alpha) CM3_TARGET=ALPHA_OPENBSD;;
        powerpc) CM3_TARGET=PPC_LINUX;;
        x86_64) CM3_TARGET=AMD64_LINUX;;
        amd64) CM3_TARGET=AMD64_LINUX;;
        sparc64) CM3_TARGET=SPARC64_OPENBSD;;
        mips64) CM3_TARGET=MIPS64_OPENBSD;;
        mips64el) CM3_TARGET=MIPS64EL_OPENBSD;;
        i386) CM3_TARGET=I386_OPENBSD;;
      esac;;

    *) echo "$0 doesn't know about `uname -a`"
       exit 1;;
  esac
fi

case "x$CM3_TARGET" in
  *DARWIN) CM3_GDB=no;;
esac

#-----------------------------------------------------------------------------
# define the exported values
if [ -n "$root" ] ; then
  ROOT=${ROOT:-${root}}
else
  ROOT=${ROOT:-${PRJ_ROOT}/cm3}
fi

M3GDB=$CM3_GDB
M3OSTYPE=$CM3_OSTYPE
TARGET=$CM3_TARGET
GCC_BACKEND=$CM3_GCC_BACKEND
INSTALLROOT=$CM3_INSTALL

CM3_ROOT=${CM3_ROOT:-${ROOT}}
PKGSDB=${PKGSDB:-$ROOT/scripts/PKGS}
GREP=${GREP:-egrep}

EGREP=egrep
if [ -x /usr/sfw/bin/gegrep ] ; then
  EGREP=/usr/sfw/bin/gegrep
fi

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
export CM3ROOT CM3 CM3_ROOT FIND EGREP
export SYSINFO_DONE CM3VERSIONNUM CM3LASTCHANGED

fi
