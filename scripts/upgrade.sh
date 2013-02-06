#!/bin/sh

set -e
set -x

if [ -n "$ROOT" -a -d "$ROOT" ] ; then
  sysinfo="$ROOT/scripts/sysinfo.sh"
  root="${ROOT}"; export root
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
. "$root/scripts/pkginfo.sh"
. "$root/scripts/pkgcmds.sh"

USAGE="
  `basename $0` [ generic_options ] [ generic_cmd ]

  builds a new compiler from a new set of sources, using an existing compiler
  possibly an old compiler

  generic_options:
${GEN_OPTS}"
  
show_usage $@

OPTIONS=`extract_options $@`

EXE=""
if [ "${TARGET}" = "NT386" \
    -o "${TARGET}" = "I386_CYGWIN"  -o "${TARGET}" = "I386_NT"  -o "${TARGET}" = "I386_MINGW" \
    -o "${TARGET}" = "AMD64_CYGWIN" -o "${TARGET}" = "AMD64_NT" -o "${TARGET}" = "AMD64_MINGW" ]; then
  EXE=".exe"
fi

install_compiler() {
  M3CONFIG="${INSTALLROOT}/bin/cm3.cfg" "${root}/m3-sys/cm3/${TARGET}/cm3${EXE}" --version || exit 1
  if [ "x${GCC_BACKEND}" = xyes ]; then
    "${root}/m3-sys/m3cc/${TARGET}/cm3cg${EXE}" --help -version >/dev/null || exit 1
  fi
  mkdir -p "${INSTALLROOT}/bin" || exit 1
  cp "${root}/m3-sys/cm3/${TARGET}/cm3${EXE}" "${INSTALLROOT}/bin/cm3${EXE}" || exit 1
  if [ "x${GCC_BACKEND}" = xyes ]; then
    cp "${root}/m3-sys/m3cc/${TARGET}/cm3cg${EXE}" "${INSTALLROOT}/bin/cm3cg${EXE}" || exit 1
  fi
  install_config || exit 1
}

# delete lingering cm3cg in case old compiler/config uses it
for a in cm3cg gcc/m3cgc1; do
    for b in "" .exe; do
        rm "$root/m3-sys/m3cc/${TARGET}/$a$b" || true
    done
done

# Build the compiler with the installed version of the runtime;
# do _not_ compile m3core and libm3 here.
# Start with the front end.
P="import-libs sysutils m3middle m3objfile m3linker m3back \
   m3front m3quake cm3 mklib m3cggen"
"$root/scripts/do-pkg.sh" "$@" "realclean" ${P} || exit 1
"$root/scripts/do-pkg.sh" "$@" "buildship" ${P} || exit 1

if [ "x${GCC_BACKEND}" = xyes ]; then
  # Continue with the backend, if needed. Deliberately don't ship it!
  # "$root/m3-sys/m3cggen/${TARGET}/m3cggen" > "$root/m3-sys/m3cc/gcc/gcc/m3cg/m3cg.h"
  "$root/scripts/do-pkg.sh" "$@" "build" m3cc || exit 1
fi

# Install the compiler binaries all together (cm3, cm3cg, config).
install_compiler || exit 1

# Rebuild with the new compiler.
# No need for m3cc since it is written in C (ie no compiler change).

P="m3core libm3 ${P}"
"$root/scripts/do-pkg.sh" "$@" "realclean" ${P} || exit 1
"$root/scripts/do-pkg.sh" "$@" "buildship" ${P} || exit 1

# Do another compiler upgrade.
install_compiler || exit 1
