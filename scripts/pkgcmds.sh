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

# define build and ship programs for Critical Mass Modula-3
CM3_BUILDLOCAL="${BUILDLOCAL:-cm3 -build -override -DROOT=${ROOT} ${BUILDARGS}}"
CM3_CLEANLOCAL="${CLEANLOCAL:-cm3 -clean -override -DROOT=${ROOT} ${CLEANARGS}}"
CM3_BUILDGLOBAL="${BUILDGLOBAL:-cm3 -build  -DROOT=${ROOT} ${BUILDARGS}}"
CM3_CLEANGLOBAL="${CLEANGLOBAL:-cm3 -clean -DROOT=${ROOT} ${CLEANARGS}}"
CM3_SHIP="${SHIP:-cm3 -ship -DROOT=${ROOT} ${SHIPARGS}}"

# define build and ship programs for Poly. Modula-3 from Montreal
PM3_BUILDLOCAL="${BUILDLOCAL:-m3build -O -DROOT=${ROOT} ${BUILDARGS}}"
PM3_CLEANLOCAL="${CLEANLOCAL:-m3build clean -O -DROOT=${ROOT} ${CLEANARGS}}"
PM3_BUILDGLOBAL="${BUILDGLOBAL:-m3build -DROOT=${ROOT} ${BUILDARGS}}"
PM3_CLEANGLOBAL="${CLEANGLOBAL:-m3build clean -DROOT=${ROOT} ${CLEANARGS}}"
PM3_SHIP="${SHIP:-m3ship -DROOT=${ROOT} ${SHIPARGS}}"

# define build and ship programs for DEC SRC Modula-3
SRC_BUILDLOCAL="${BUILDLOCAL:-m3build -O -DROOT=${ROOT} ${BUILDARGS}}"
SRC_CLEANLOCAL="${CLEANLOCAL:-m3build clean -O -DROOT=${ROOT} ${CLEANARGS}}"
SRC_BUILDGLOBAL="${BUILDGLOBAL:-m3build -DROOT=${ROOT} ${BUILDARGS}}"
SRC_CLEANGLOBAL="${CLEANGLOBAL:-m3build clean -DROOT=${ROOT} ${CLEANARGS}}"
SRC_SHIP="${SHIP:-m3ship -DROOT=${ROOT} ${SHIPARGS}}"

# other commands
REALCLEAN="${REALCLEAN:-rm -rf ${TARGET}}"

# choose the compiler to use
if type cm3 2>/dev/null >/dev/null ; then
  BUILDLOCAL="${CM3_BUILDLOCAL}"
  CLEANLOCAL="${CM3_CLEANLOCAL}"
  BUILDGLOBAL="${CM3_BUILDGLOBAL}"
  CLEANGLOBAL="${CM3_CLEANGLOBAL}"
  SHIP="{$CM3_SHIP}"
elif type m3build 2>/dev/null >/dev/null ; then
  BUILDLOCAL="${PM3_BUILDLOCAL}"
  CLEANLOCAL="${PM3_CLEANLOCAL}"
  BUILDGLOBAL="${PM3_BUILDGLOBAL}"
  CLEANGLOBAL="${PM3_CLEANGLOBAL}"
  SHIP="{$CM3_SHIP}"
else
  if [ -z "${BUILDLOCAL}" -o -z "${BUILDGLOBAL}" -o -z "${SHIP}" ] ; then
    echo "cm3 or m3build not found in your path, don't know how to compile" \
      1>&2
    exit 1
  fi
fi

map_action() {
  skip=true
  while [ -n "$1" -a "$skip" = "true" ] ; do
    case "$1" in
      -*) shift;;
      *) skip=false
    esac
  done
  if [ -z "$1" ] ; then
    ACTION="${BUILDLOCAL}"
  elif [ "$1" = "build" -o "$1" = "buildlocal" ] ; then
    ACTION="${BUILDLOCAL}"
  elif [ "$1" = "buildglobal" -o "$1" = "buildship" ] ; then
    ACTION="${BUILDGLOBAL} && ${SHIP}"
  elif [ "$1" = "ship" ] ; then
    ACTION="${SHIP}"
  elif [ "$1" = "clean" -o "$1" = "cleanlocal" ] ; then
    ACTION="${CLEANLOCAL}"
  elif [ "$1" = "cleanglobal" ] ; then
    ACTION="${CLEANGLOBAL}"
  elif [ "$1" = "realclean" ] ; then
    ACTION="${REALCLEAN}"
  else
    if [ "${IGNORE_MISS}" = "yes" ] ; then
      ACTION="${BUILDLOCAL}"
    else
      echo "unknown action: $1" 1>&2
      exit 1
    fi
  fi
  echo "${ACTION}"
}

add_action_opts() {
  skip=true
  while [ -n "$1" -a "$skip" = "true" ] ; do
    case "$1" in
      -*) shift;;
      *) skip=false
    esac
  done
  ARGS="";
  if [ "$1" = "clean" -o "$1" = "cleanlocal" ] ; then
    ARGS="-k"
  elif [ "$1" = "cleanglobal" ] ; then
    ARGS="-k"
  elif [ "$1" = "realclean" ] ; then
    ARGS="-k"
  fi
  echo "${ARGS}"
}

extract_options() {
  RES=""
  while [ -n "$1" ] ; do
    case "$1" in
      -*) RES="${RES} $1";;
    esac
    shift
  done
  echo "$RES"
}

get_args() {
  skip=true
  while [ -n "$1" -a "$skip" = "true" ] ; do
    case "$1" in
      -*) shift;;
      *) skip=false
    esac
  done
  if [ "$1" = "build" -o "$1" = "buildlocal" ] ; then
    shift
  elif [ "$1" = "buildglobal" -o "$1" = "buildship" ] ; then
    shift
  elif [ "$1" = "ship" ] ; then
    shift
  elif [ "$1" = "clean" -o "$1" = "cleanlocal" ] ; then
    shift
  elif [ "$1" = "cleanglobal" ] ; then
    shift
  elif [ "$1" = "realclean" ] ; then
    shift
  fi
  ARGS=""
  while [ -n "$1" ] ; do
    case "$1" in
      -*) echo "encountered option after command: $1" 1>&2;;
      *) ARGS="${ARGS} $1";;
    esac
    shift
  done
  echo "$ARGS"
}

GEN_CMDS='
  build | buildlocal          build a package with local overrides (default)
  buildglobal | buildship     build a package without overrides and ship it
  ship                        ship a package
  clean | cleanlocal          clean a package with local overrides
  cleanglobal                 clean a package without overrides
  realclean                   remove the TARGET directory of a package
'
GEN_OPTS='
  -n                          no action (do not execute anything)
  -k                          keep going (ignore errors if possible)
'

format_one() {
  N=${N:-20}
  if type printf > /dev/null 2>/dev/null ; then
    printf "%-${N}s" $1
  else
    echo $1
  fi
}

print_list() {
  INDENT=${INDENT:-"  "}
  while [ -n "$1" ] ; do
    echo "${INDENT}$1"
    shift
  done
}

print_list2() {
  INDENT=${INDENT:-"  "}
  N=36; export N
  while [ -n "$1" ] ; do
    echo "${INDENT} `format_one $1` `format_one $2`"
    shift; shift
  done
}

print_list4() {
  INDENT=${INDENT:-"  "}
  N=18; export N
  while [ -n "$1" ] ; do
    echo "${INDENT} `format_one $1` `format_one $2` `format_one $3` `format_one $4`"
    shift; shift
  done
}

show_usage() {
  if [ "x$1" = "x-h" -o "x$1" = "x-help" -o "x$1" = "x--help" ] ; then
    echo ""
    echo "usage `basename $0:`"
    if [ -n "$USAGE" ] ; then
      echo "$USAGE"
    else
      echo ""
      echo "No specific usage notes available."
      echo ""
      echo "Generic commands:"
      echo "${GEN_CMDS}"
      echo "Generic options:"
      echo "${GEN_OPTS}"
    fi
    exit 0
  fi
}