#!/bin/sh

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
. "$ROOT/scripts/pkginfo.sh"

ALL=`awk "{print \\$1}" ${ROOT}/scripts/pkginfo.txt | tr '\\n' ' '`
ALLDEPS=${ROOT}/scripts/all-deps
#echo "ALLDEPS=${ALLDEPS}"
#echo "ALL=${ALL}"
PKG_COLLECTIONS="core devlib gui webdev m3gdb m3devtool anim database cvsup obliq juno caltech-parser caltech-other demo tool math game"

[ ! -f ${ALLDEPS} ] && {
  CM3_ALL=yes \
  ${ROOT}/scripts/pkgmap.sh -c m3dep $ALL 2>/dev/null | 
  egrep -v '^ |^$' | 
  sed -e "s;${ROOT}/;;" \
      -e 's;=== package ;|;' \
      -e 's; ===; ;' | 
  tr -d '\n' | tr '|' '\n' |
  sed -e 's;^./;;' > ${ALLDEPS}
}

list_contains() {
  str="$1"
  elem="$2"
  for f in ${str}; do
    [ "$f" = "${elem}" ] && return 0
  done
  return 1
}

COREPKGS=`fgrep " core" $ROOT/scripts/pkginfo.txt | awk "{print \\$1}" | tr '\\n' ' '`

for c in ${PKG_COLLECTIONS}; do
  P=`fgrep " $c" $ROOT/scripts/pkginfo.txt | awk "{print \\$1}" | tr '\\n' ' '`
  for p in $P; do
    echo ${p}
  done > coll-pkgs-$c
done

for c in ${PKG_COLLECTIONS}; do
  P=`fgrep " $c" $ROOT/scripts/pkginfo.txt | awk "{print \\$1}" | tr '\\n' ' '`
  PKGS=""
  cdeps=""
  adeps=""
  for x in $P; do
    p="$x"
    if [ -d "$x" ] ; then
      PKGS="${PKGS} $x"
    else
      p=`pkgpath $x`
      if [ -d "$p" ] ; then
        PKGS="${PKGS} $x"
      else
        echo " *** cannot find package $x / $p" 1>&2
        exit 1
      fi
    fi
    depline=`grep "^${p} " ${ALLDEPS}`
    pdeps=`echo ${depline} | sed -e 's/.*://'`
    adeps="${adeps}${depline}|"
    for dep in ${pdeps}; do
      #echo "  ${dep} ? ${cdeps}"
      list_contains "${cdeps}" "${dep}" || cdeps="${cdeps} ${dep}"
    done
    #echo "p=${p} cdeps = ${cdeps}"
  done
  rcdeps=""
  for dep in ${cdeps}; do
    list_contains "${PKGS}" "${dep}" || rcdeps="${rcdeps} ${dep}"
  done
  rc2deps=""
  for dep in ${rcdeps}; do
    list_contains "${COREPKGS}" "${dep}" || rc2deps="${rc2deps} ${dep}"
  done
  colldeps=""
  colldepsreason=""
  for dep in ${rcdeps}; do
    for cc in ${PKG_COLLECTIONS}; do
      [ "$c" = "$cc" ] || {
        grep -q "^${dep}$" coll-pkgs-${cc} && {
          list_contains "${colldeps}" "${cc}" || {
            colldeps="${colldeps} ${cc}"
            colldepsreason="${colldepsreason} ${cc}(${dep})"
          }
        }
      }
    done
  done
  echo "collection ${c}"
  echo "  PKGS=${PKGS}"
  echo "  EXTDEPS=${rcdeps}"
  echo "  EXTDEPS-CORE=${rc2deps}"
  echo "  COLLDEPS=${colldeps}"
  echo "  CAUSES: ${colldepsreason}"
  #echo "${adeps}" | tr '|' '\n'
  echo ""
done
