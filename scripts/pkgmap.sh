#!/bin/sh
# $Id: pkgmap.sh,v 1.31 2009-05-17 03:40:35 jkrell Exp $

#set -x
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
SYSINFO_DONE=""
. "$sysinfo"
. "$ROOT/scripts/pkginfo.sh"

exec_cmd() {
  echo " +++ $PKG_ACTION +++"
  [ "$NO_ACTION" = yes ] || /bin/sh -c "cd $1 && $PKG_ACTION"
}

PKGS=""
RARGS=""
while [ -n "$1" ] ; do
  case "$1" in
    -*)
      if [ x-k = x"$1" ] ; then
        KEEP_GOING="yes"
      elif [ x-n = x"$1" ] ; then
        NO_ACTION="yes"
      elif [ x-report = x"$1" ] ; then
        REPORT="yes"
      elif [ x-l = x"$1" ] ; then
        LIST_ONLY="yes"
      elif [ x-c = x"$1" ] ; then
        if [ -z "${PKG_ACTION}" ] ; then
          PKG_ACTION="$2"
        else
          PKG_ACTION="${PKG_ACTION}; $2"
        fi
        shift
      else
        # collect RARGS
        RARGS="${RARGS} $1"
      fi
      shift
    ;;
    *)
      if [ -d "$ROOT/$1" ] ; then
        PKGS="${PKGS} $ROOT/$1"
      elif [ -d "$1" ] ; then
        PKGS="${PKGS} $1"
      else
        p=`pkgpath $1`
        if [ -d "$p" ] ; then
          PKGS="${PKGS} $p"
        elif [ -n "$p" -a -d "$ROOT/$p" ] ; then
          PKGS="${PKGS} $ROOT/$p"
        else
          echo " *** cannot find package $1 / $p" 1>&2
          exit 1
        fi
      fi
      shift
    ;;
  esac
done
if [ -n "${RARGS}" ]; then
  export RARGS
fi

if [ -n "${REPORT}" ]; then
  DS=${DS:-`date -u +'%Y-%m-%d-%H-%M-%S' | tr -d '\\n'`}
  R="${HTML_REPORT:-${TMPDIR}/cm3-pkg-report-${TARGET}-${DS}.html}"
  R2="`basename ${R} .html`.part2}"
  ERRS=""
  REDPKGS=""
  GREENPKGS=""
  YELLOWPKGS=""
fi

if [ -z "$PKG_ACTION" ] ; then
  echo "no PKG_ACTION defined, aborting" 1>&2
  exit 1
fi

if [ -z "${PKGS}" ] ; then
  echo "no packages" 1>&2
  exit 1
fi

if [ "yes" = "$LIST_ONLY" ] ; then
  listpkgs ${PKGS}
  exit 0
fi

report_header() {
  (
    cat << EOF
<!DOCTYPE HTML PUBLIC "-//W3C//DTD HTML 4.01 Transitional//EN">
<html>
  <head>
    <title>CM3 Package Status for target platform ${TARGET}</title>
    <META HTTP-EQUIV="Content-Type" CONTENT="text/html">
    <META HTTP-EQUIV="Content-Style-Type" CONTENT="text/css">
    <META HTTP-EQUIV="Resource-type" CONTENT="document"> 
    <META HTTP-EQUIV="Reply-to" CONTENT="m3-support@elego.de"> 
    <LINK HREF="../normal.css" REL="stylesheet" TYPE="text/css">
    <META NAME="robots" content="noindex">
    <style type="text/css">
    <!--
    .white    { color:#FFFFFF; }
    .bgred    { background:#FF4444; }
    .bggreen  { background:#44FF44; }
    .bgblue   { background:#5555FF; }
    .bgyellow { background:#FFFF00; }
    .bgorange { background:#FFAA00; }
    a:link    { color: #0077ff; }
    a:visited { color: #0077ff; }
    a:active  { color: #4090ff; }
    a:hover   { color: #4090ff; }
    .small    { font-size:8pt; }
    .tl       { text-align: left; vertical-align: top; }
    //-->
    </style>
  </head>

  <body bgcolor="#ffffff">
    <h2>CM3 Package Status for Target Platform ${TARGET}</h2>

    <p>
       report generated at `date` on `hostname`
    </p>
    <table border="2" cellspacing="1" cellpadding="4">
      <thead>
        <tr class="bgblue white">
          <td>
            <b>Package</b>
          </td>
          <td>
            <b>Status</b>
          </td>
          <td>
            <b>Errors</b>
          </td>
          <td>
            <b>Tests</b>
          </td>
        </tr>
      </thead>
      <tbody>

EOF
  ) > "${R}"
  (
    echo "<hr>"
    echo "<h3>Package Test Result Details</hr>"
  ) > "${R2}"
}

report_footer() {
  (
    cat << EOF

      </tbody>
    </table>
EOF
  ) >> "${R}"
  cat "${R2}" >> "${R}"
  rm "${R2}"
  (
    cat << EOF

    <hr>
    <address><a href="mailto:m3-support{at}elego.de">m3-support{at}elego.de</a></address>
<!-- Created: Fri Feb 16 15:27:10 MET 2001 -->
  </body>
</html>
EOF
  ) >> "${R}"
}

write_pkg_report() {
  res=""
  # evaluate package build status
  pname=`echo $1 | sed -e 's;/;-;g'`
  errlines=`egrep '^".*, line .*:|warning:|version stamp mismatch|bad version stamps|Fatal Error|failed|quake runtime error|ignoring override|unsupported' "$1/${TARGET}/stdout.log"`
  if [ "$2" = "0" ] ; then
    echo "<tr class=\"bggreen\">"
    bgt="bggreen"
  elif [ "$2" = "2" ] ; then
    echo "<tr class=\"bgyellow\">"
    bgt="bgyellow"
  else
    echo "<tr class=\"bgred\">"
    bgt="bgred"
    ERRS=`printf "${ERRS}${errlines}\\n"`
    res=`printf "${PKG}${ERRS}\\n"`
  fi >> "${R}"

  # evaluate package test status
  tbgt="${bgt}"
  tmsg="$3"
  terrlines=`echo "$3" | egrep -i 'version stamp mismatch|bad version stamps|Fatal Error|quake runtime error|test failed'`
  if [ "${bgt}" = "bgred" ]; then
    tbgt="bgyellow"
    tmsg="not tried"
  elif [ "$3" = "no tests" -o "$3" = "no src/m3makefile" -o \
       "$3" = "not supported on ${TARGET}" ]; then
    tbgt="bgyellow"
  elif [ -n "$4" ]; then
    tbgt="bgorange"
  fi
  if [ -n "${terrlines}" ]; then
    tbgt="bgred"
  fi

  # write table fields for build and test part
  (
    echo "  <td class=\"tl\">$1</td>"
    if [ "$2" = "0" ] ; then
      echo "  <td class=\"tl\">build OK</td>"
    elif [ "$2" = "2" ] ; then
      echo "  <td class=\"tl\">not supported on ${TARGET}</td>"
    else
      echo "  <td class=\"tl\">build failed</td>"
    fi
    echo "  <td class=\"small\"><pre>"
    if FOLD="`find_exe fold /usr/bin`/fold" ; then
      echo "$errlines" | ${FOLD} -s -w 64
    else
      echo "$errlines"
    fi
    echo "  </pre></td>"
    echo "  <td class=\"${tbgt}\">"
    if [ "${tbgt}" = "bgyellow" ]; then
      echo "${tmsg}"
    else
      echo "    <a href=\"#tr_${pname}\" id=\"ref_tr_${pname}\">"
      echo "      test result details for $1"
      echo "    </a>"
    fi
    echo "  </td>"
    echo "</tr>"
  ) >> "${R}"
  # log errors to stdout
  echo "${ERRS}"

  # gather detailed test output at the end of the report
  (
    if [ "${tbgt}" != "bgyellow" ]; then
      echo "<hr><h4><a id=\"tr_${pname}\" href=\"#ref_tr_${pname}\">"
      echo "  Test Result Details for $1"
      echo "</a></h4>"
      echo "<div class=\"$tbgt\">"
      echo "  <pre class=\"small\">"
      echo "$3"
      if [ -n "$4" ]; then
        echo "stderr:"
        echo "$4"
      fi
      echo "  </pre>"
      echo "</div>"
    fi
  ) >> "${R2}"
}

if [ -n "${REPORT}" ]; then
  report_header
fi

M3GDB=yes
OK=yes

for PKG in ${PKGS} ; do
  echo "=== package ${PKG} ==="
  tres="not supported on ${TARGET}"
  terr=""
  mkdir -p "${PKG}/${TARGET}"
  STDOUTLOG="${PKG}/${TARGET}/stdout.log"
  if [ "${REPORT}" = "yes" ] ; then
    rm -f "${STDOUTLOG}"
    if UsePackage `basename "${PKG}"` || [ "${CM3_ALL}" = yes ]; then
      exec_cmd "$PKG" > "${STDOUTLOG}" 2>&1
      res=$?
      cat "${STDOUTLOG}"
      if grep 'Fatal Error:' "${STDOUTLOG}" >/dev/null 2>&1; then
        res=1
        OK=""
        REDPKGS=`printf "${REDPKGS}${PKG}\\\\\\n"`
      elif [ "${res}" = "1" ]; then
        OK=""
        REDPKGS=`printf "${REDPKGS}${PKG}\\\\\\n"`
      else
        GREENPKGS=`printf "${GREENPKGS}${PKG}\\\\\\n"`
        HERE=`pwd`
        tres=""
        cd ${PKG}
        if [ -d "test" ]; then
          cd test
        elif [ -d "tests" ]; then
          cd tests
        else
          tres="no tests"
        fi
        if [ -z "${tres}" ]; then
          if [ -r "src/m3makefile" ]; then
            echo "=== tests in `pwd` ==="
            echo " +++ cm3 -build -override -DTEST -DRUN -DROOT=$ROOT +++"
            LD_LIBRARY_PATH="$LD_LIBRARY_PATH:${PKG}/${TARGET}"
            DYLD_LIBRARY_PATH="$LD_LIBRARY_PATH"
            export LD_LIBRARY_PATH DYLD_LIBRARY_PATH
            tres=`cm3 -build -override -DTEST -DRUN -DROOT="${ROOT}" 2> stderr`
            terr=`cat stderr`
          else
            tres="no src/m3makefile"
          fi
        fi
        cd "${HERE}"
      fi
    else
      touch "${STDOUTLOG}"
      echo "=== package omitted on this platform ==="
      res=2
      YELLOWPKGS=`printf "${YELLOWPKGS}${PKG}\\\\\\n"`
    fi
    #deps=`cd "${PKG}" && cm3 -depend | head -1`
  else
    if UsePackage `basename "${PKG}"` || [ "${CM3_ALL}" = yes ]; then
      exec_cmd "$PKG"
      res=$?
    else
      echo "=== package omitted on this platform ==="
      res=0
    fi
  fi
  if [ -n "${REPORT}" ]; then
    #if grep ': imported interface' "${STDOUTLOG}" >/dev/null 2>&1; then
    #  res=2
    #fi
    ERRS=`write_pkg_report "${PKG}" "${res}" "${tres}" "${terr}"`
  fi
  if [ "$res" != "0" -a "$res" != "2" ] ; then
    if [ "${KEEP_GOING}" != "yes" ] ; then
      echo " *** execution of $PKG_ACTION failed ***" 
      exit 1
    fi
  fi
  if [ "$res" != "0" -a "${KEEP_GOING}" = "yes" ] ; then
    echo " ==> $PKG_ACTION returned $res"
  else
    echo " ==> ${PKG} done"
  fi
  echo ""
done

if [ -n "${REPORT}" ]; then
  report_footer
  if [ -n "${ERRS}" ]; then
    echo "errors:" 1>&2
    printf "${ERRS}" 1>&2
    echo ""
    echo "compilation failed for the following packages:"
    printf "${REDPKGS}"
  fi
  echo "HTML package report in $R"
  if [ -n "${DOSHIP}" ]; then
    WWWSERVER=${WWWSERVER:-birch.elegosoft.com}
    WWWDEST=${WWWDEST:-${WWWSERVER}:/var/www/modula3.elegosoft.com/cm3/logs}
    scp "${R}" "${WWWDEST}" < /dev/null
  fi
fi

[ -n "${OK}" ] || exit `printf "{$REDPKGS}" | wc | awk '{print $1}'`
