#!/bin/sh
# $Id: boot-cm3-build-on-target.sh,v 1.4 2003-06-25 15:03:41 wagner Exp $

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
if [ -z "$1" ] ; then
  echo "please specify a cross compilation target platform" 1>&2
  exit 1
fi
CROSS_TARGET=$1
if [ ! -d "${ROOT}/${CROSS_TARGET}" ] ; then
  echo "cross compiled packages not found at ${ROOT}/${CROSS_TARGET}" 1>&2
  exit 1
fi

. "$ROOT/scripts/pkginfo.sh"
. "$ROOT/scripts/pkgcmds.sh"

L=""
P=""
if [ $CROSS_TARGET != NT386GNU ] ; then
  L="${L} m3-libs/m3gc-simple"
fi
#L="${L} m3-libs/m3gc-enhanced"
L="${L} m3-libs/m3core"
L="${L} m3-libs/libm3"
L="${L} m3-sys/m3middle"
[ "${M3OSTYPE}" = "WIN32" ] && L="${L} m3-sys/m3objfile"
L="${L} m3-sys/m3linker"
[ "${GCC_BACKEND}" != yes ] && L="${L} m3-sys/m3back"
[ "${GCC_BACKEND}" != yes ] && L="${L} m3-sys/m3staloneback"
L="${L} m3-sys/m3front"
L="${L} m3-sys/m3quake"
P="${P} m3-sys/cm3"
L="${L} m3-sys/m3scanner"
L="${L} m3-sys/m3tools"
#P="${P} m3-sys/m3cgcat"
#P="${P} m3-sys/m3cggen"
P="${P} m3-tools/m3bundle"
#[ "${M3OSTYPE}" = "WIN32" ] && P="${P} m3-sys/mklib"
#[ "${M3OSTYPE}" = "WIN32" ] && P="${P} m3-sys/dll2lib"
#[ "${M3OSTYPE}" = "WIN32" ] && P="${P} m3-sys/fix_nl"
#[ "${M3OSTYPE}" = "WIN32" ] && P="${P} m3-sys/libdump"
#L="${L} m3-libs/bitvector"
#L="${L} m3-libs/digraph"
#L="${L} m3-libs/parseparams"
#L="${L} m3-libs/realgeometry"
#L="${L} m3-libs/set"
#L="${L} m3-libs/slisp"
#L="${L} m3-libs/sortedtableextras"
#L="${L} m3-libs/table-list"
#L="${L} m3-libs/tempfiles"
#[ "${HAVE_TCL}" = "yes" ] && L="${L} m3-libs/tcl"

USAGE="
  `basename $0` cross_target

  will build cross-compiled core packages on the target platform.

"

show_usage $@
shift

get_libs() {
  p="${ROOT}/${CROSS_TARGET}"
  case $1 in
    cm3)
      echo -n "${p}/m3-sys/m3quake/${CROSS_TARGET}/libm3quake.a ";
      echo -n "${p}/m3-sys/m3front/${CROSS_TARGET}/libm3front.a ";
      echo -n "${p}/m3-sys/m3linker/${CROSS_TARGET}/libm3linker.a ";
      echo -n "${p}/m3-sys/m3middle/${CROSS_TARGET}/libm3middle.a ";
      echo -n "${p}/m3-libs/libm3/${CROSS_TARGET}/libm3.a ";
      echo -n "${p}/m3-libs/m3core/${CROSS_TARGET}/libm3core.a ";
      echo -n "${p}/m3-libs/m3gc-simple/${CROSS_TARGET}/libm3gc-simple.a ";
      echo -n "-lm ";
    ;;
    m3bundle)
      echo -n "${p}/m3-libs/libm3/${CROSS_TARGET}/libm3.a ";
      echo -n "${p}/m3-libs/m3core/${CROSS_TARGET}/libm3core.a ";
      echo -n "${p}/m3-libs/m3gc-simple/${CROSS_TARGET}/libm3gc-simple.a ";
      echo -n "-lm ";
    ;;
  esac
}

# build all libraries
for p in ${L}; do
  cd ${ROOT}/${CROSS_TARGET} || exit 1
  pkg=`basename ${p}`
  bdir="${ROOT}/${CROSS_TARGET}/${p}/${CROSS_TARGET}"
  case ${pkg} in
    lib*) lib="${pkg}.a";;
    *)    lib="lib${pkg}.a";;
  esac
  header "building in ${bdir}" 
  cp ${ROOT}/scripts/boot-cm3-makefile-lib.tmpl ${bdir}/Makefile
  cd ${bdir}
  if [ "$DOCLEAN" = yes ] ; then
    ${GMAKE} LIBNAME="${lib}" clean
  fi
  if [ "$DOCLEAN" = obj ] ; then
    ${GMAKE} LIBNAME="${lib}" clean-obj
  fi
  ${GMAKE} LIBNAME="${lib}" all || exit 1
done

# build all programs
for p in ${P}; do
  cd ${ROOT}/${CROSS_TARGET} || exit 1
  pkg=`basename ${p}`
  bdir="${ROOT}/${CROSS_TARGET}/${p}/${CROSS_TARGET}"
  header "building in ${bdir}" 
  cp ${ROOT}/scripts/boot-cm3-makefile-prog.tmpl ${bdir}/Makefile
  cd ${bdir}
  if [ "$DOCLEAN" = yes ] ; then
    ${GMAKE} PROGNAME="${pkg}" LIBS="`get_libs ${pkg}`" clean
  fi
  if [ "$DOCLEAN" = obj ] ; then
    ${GMAKE} PROGNAME="${pkg}" LIBS="`get_libs ${pkg}`" clean-obj
  fi
  ${GMAKE} PROGNAME="${pkg}" LIBS="`get_libs ${pkg}`" all
done

