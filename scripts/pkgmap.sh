#!/bin/sh

#set -x
if [ -n "$ROOT" -a -d "$ROOT" ]; then
  sysinfo="$ROOT/scripts/sysinfo.sh"
  root="${ROOT}"; export root
else
  root=`pwd`
  while [ -n "$root" -a ! -f "$root/scripts/sysinfo.sh" ]; do
    root=`dirname $root`
  done
  sysinfo="$root/scripts/sysinfo.sh"
  if [ ! -f "$sysinfo" ]; then
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
  [ "$NO_ACTION" = yes ] || {
    if [ -n "${WORKSPACE}" -o -n "${CM3_CLEAN_RETRY}" ]; then
      SOL="$1/tmp-stdout-$$"
      /bin/sh -c "cd $1 && $PKG_ACTION" >"${SOL}"
      rc=$?
      if [ "$rc" != 0 ]; then
        if echo "${PKG_ACTION}" | grep -q -- -build >/dev/null 2>&1; then
          if [ -n "${WORKSPACE}" -o -n "${CM3_CLEAN_RETRY}" ]; then
            echo "retry build after cleaning" >"${SOL}"
            /bin/sh -c "cd $1 && cm3 -clean" >>"${SOL}"
            /bin/sh -c "cd $1 && $PKG_ACTION" >>"${SOL}"
            rc=$?
          fi
        fi
      fi
      cat "${SOL}"
      rm -f "${SOL}"
    else
      /bin/sh -c "cd $1 && $PKG_ACTION"
      rc=$?
    fi
    return $rc
  }
}

PKGS=""
RARGS=""
while [ -n "$1" ]; do
  case "$1" in
    -*)
      if [ x-k = x"$1" ]; then
        KEEP_GOING="yes"
      elif [ x-n = x"$1" ]; then
        NO_ACTION="yes"
      elif [ x-report = x"$1" ]; then
        REPORT="yes"
      elif [ x-l = x"$1" ]; then
        LIST_ONLY="yes"
      elif [ x-c = x"$1" ]; then
        if [ -z "${PKG_ACTION}" ]; then
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
      if [ -d "$ROOT/$1" ]; then
        PKGS="${PKGS} $ROOT/$1"
      elif [ -d "$1" ]; then
        PKGS="${PKGS} $1"
      else
        p=`pkgpath $1`
        if [ -d "$p" ]; then
          PKGS="${PKGS} $p"
        elif [ -n "$p" -a -d "$ROOT/$p" ]; then
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
  RJ="${XML_REPORT:-${TMPDIR}/cm3-pkg-report-${TARGET}-${DS}.xml}"
  RJT="${XML_REPORT:-${TMPDIR}/cm3-pkg-test-report-${TARGET}-${DS}.xml}"
  RW="${WORKSPACE}/cm3-pkg-report-${TARGET}.html"
  RJW="${WORKSPACE}/cm3-pkg-report-${TARGET}.xml"
  RJTW="${WORKSPACE}/cm3-pkg-test-report-${TARGET}.xml"
  R2="`basename ${R} .html`.part2"
  ERRS=""
  REDPKGS=""
  GREENPKGS=""
  YELLOWPKGS=""
fi

if [ -z "$PKG_ACTION" ]; then
  echo "no PKG_ACTION defined, aborting" 1>&2
  exit 1
fi

if [ -z "${PKGS}" ]; then
  echo "no packages" 1>&2
  exit 1
fi

if [ "yes" = "$LIST_ONLY" ]; then
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
  echo "" >${RJ}
  echo "" >${RJT}
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

quote_xml() {
  while [ -n "$1" ]; do
    echo "$1" | sed -e 's/&/&amp;/g' \
                    -e 's/</\&lt;/g' \
                    -e 's/>/\&gt;/g' \
              | tr -d '\001\002\003\004\005\006\007\010'
    shift
  done
}

pall=0
pko=0
tall=0
tko=0
psk=0
tsk=0

write_pkg_report() {
  res=""
  # evaluate package build status
  pname=`echo $1 | sed -e 's;/;-;g'`
  errlines=`egrep '^".*, line .*:|warning:|version stamp mismatch|bad version stamps|Fatal Error|failed|quake runtime error|ignoring override|unsupported' "$1/${TARGET}/stdout.log"`
  if [ "$2" = "0" ]; then
    echo "<tr class=\"bggreen\">" >> "${R}"
    bgt="bggreen"
  elif [ "$2" = "2" ]; then
    echo "<tr class=\"bgyellow\">" >> "${R}"
    bgt="bgyellow"
  else
    echo "<tr class=\"bgred\">" >> "${R}"
    bgt="bgred"
    ERRS=`printf "${ERRS}${errlines}\\n"`
    res=`printf "${PKG}${ERRS}\\n"`
  fi

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
    if [ "$2" = "0" ]; then
      echo "  <td class=\"tl\">build OK</td>"
    elif [ "$2" = "2" ]; then
      echo "  <td class=\"tl\">not supported on ${TARGET}</td>"
    else
      echo "  <td class=\"tl\">build failed</td>"
    fi
    echo "  <td class=\"small\"><pre>"
    if FOLD="`find_exe fold /usr/bin`/fold"; then
      quote_xml "`echo "$errlines" | ${FOLD} -s -w 64`"
    else
      quote_xml "$errlines"
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
  (
    echo "  <testcase name=\"$1\" time=\"$5\">"
    if [ "$2" = "0" ]; then
      echo "  build OK"
    elif [ "$2" = "2" ]; then
      echo "  not supported on ${TARGET} (skipped)"
      echo "  <skipped type=\"NEX\" message=\"not supported on ${TARGET}\"/>"
    else
      echo "  <failure type=\"build failed\">"
      quote_xml "$errlines"
      echo "  </failure>"
    fi
    echo "  </testcase>"
  ) >> "${RJ}"
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
      quote_xml "$3"
      if [ -n "$4" ]; then
        echo "stderr:"
        quote_xml "$4"
      fi
      echo "  </pre>"
      echo "</div>"
    fi
  ) >> "${R2}"

  # write test report for packages tests
  (
    if [ "${tbgt}" != "bgyellow" ]; then
      echo "<testcase name=\"${pname} tests\" time=\"$6\">"
      # echo "  Test Result Details for $1" #
      # quote_xml "$3" # FIXME: leave out stdout test output for now
      if [ -n "$4" ]; then
        echo "    <failure type=\"package tests failed\">"
        quote_xml "$4"
        echo "</failure>"
      fi
      echo "</testcase>"
    fi
  ) >> "${RJT}"
}

if [ -n "${REPORT}" ]; then
  report_header

  make_date() {
    cat >date.c <<End
#include <stdio.h>
#include <time.h>
int main()
{
  printf("%lu\n", (unsigned long)time(NULL));
  return 0;
}
End
    rm -f m3date m3date.exe
    for cc in /opt/SUNWspro/bin/cc /usr/sfw/bin/gcc /usr/bin/cc /usr/bin/gcc cc gcc; do
      echo type $cc
      if type $cc; then
        echo "$cc date.c -o m3date"
        $cc date.c -o m3date
        if [ -x m3date ]; then
          return
        fi
      fi
    done
    echo "no C compiler found"
    exit 1
  }
  make_date
  M3DATE=`pwd`/m3date
fi

M3GDB=yes
OK=yes

for PKG in ${PKGS}; do
  if [ `hostname` = "plin" ]; then
    echo ls -l /home/hudson/workspace/cm3-release-build-PPC_LINUX
         ls -l /home/hudson/workspace/cm3-release-build-PPC_LINUX
    echo ls -l /home/hudson/workspace
         ls -l /home/hudson/workspace
    echo ls -l /home/hudson
         ls -l /home/hudson
  end
  echo "=== package ${PKG} ==="
  tres="not supported on ${TARGET}"
  terr=""
  mkdir -p "${PKG}/${TARGET}"
  STDOUTLOG="${PKG}/${TARGET}/stdout.log"
  if [ "${REPORT}" = "yes" ]; then
    rm -f "${STDOUTLOG}"
    if UsePackage `basename "${PKG}"` || [ "${CM3_ALL}" = yes ]; then
      pstart=`$M3DATE +%s`
      exec_cmd "$PKG" > "${STDOUTLOG}" 2>&1
      res=$?
      pend=`$M3DATE +%s`
      ptime=`expr \( $pend - $pstart \) \* 1000 + 500`
      ptime=`echo "scale=3; ${ptime} / 1000" | bc`
      cat "${STDOUTLOG}"
      if grep 'Fatal Error:' "${STDOUTLOG}" >/dev/null 2>&1; then
        res=1
        OK=""
        REDPKGS=`printf "${REDPKGS}${PKG}\\\\\\n"`
        pko=`expr $pko + 1`
      elif [ "${res}" = "1" ]; then
        OK=""
        REDPKGS=`printf "${REDPKGS}${PKG}\\\\\\n"`
        pko=`expr $pko + 1`
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
            tall=`expr $tall + 1`
            echo "=== tests in `pwd` ==="
            echo " +++ cm3 -build -override -DTEST -DRUN -DROOT=$ROOT +++"
            LD_LIBRARY_PATH="$LD_LIBRARY_PATH:${PKG}/${TARGET}"
            DYLD_LIBRARY_PATH="$LD_LIBRARY_PATH"
            export LD_LIBRARY_PATH DYLD_LIBRARY_PATH
            tstart=`$M3DATE +%s`
            tres=`cm3 -build -override -DTEST -DRUN -DROOT="${ROOT}" 2> stderr`
            tend=`$M3DATE +%s`
            ttime=`expr \( $tend - $tstart \) \* 1000 + 500`
            ttime=`echo "scale=3; ${ttime} / 1000" | bc`
            terr=`cat stderr`
            if [ -n "${terr}" ]; then
              tko=`expr $tko + 1`
            fi
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
      psk=`expr $psk + 1`
      HERE=`pwd`
      cd "${PKG}"
      echo "write(\"package ${PKG} not supported on this platform\" & EOL)" \
        > ${TARGET}/.M3SHIP
      cd "${HERE}"
    fi
    #deps=`cd "${PKG}" && cm3 -depend | head -1`
  else
    if UsePackage `basename "${PKG}"` || [ "${CM3_ALL}" = yes ]; then
      exec_cmd "$PKG"
      res=$?
    else
      echo "=== package omitted on this platform ==="
      HERE=`pwd`
      cd "${PKG}"
      echo "write(\"package ${PKG} not supported on this platform\" & EOL)" \
        > ${TARGET}/.M3SHIP
      cd "${HERE}"
      res=0
    fi
  fi
  if [ -n "${REPORT}" ]; then
    #if grep ': imported interface' "${STDOUTLOG}" >/dev/null 2>&1; then
    #  res=2
    #fi
    ERRS=`write_pkg_report "${PKG}" "${res}" "${tres}" "${terr}" "${ptime}" "${ttime}"`
    pall=`expr $pall + 1`
  fi
  if [ "$res" != "0" -a "$res" != "2" ]; then
    if [ "${KEEP_GOING}" != "yes" ]; then
      echo " *** execution of $PKG_ACTION failed ***" 
      exit 1
    fi
  fi
  if [ "$res" != "0" -a "${KEEP_GOING}" = "yes" ]; then
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

  rj=`cat ${RJ}`
  rjt=`cat ${RJT}`

  echo '<?xml version="1.0" encoding="ISO-8859-1"?>' > ${RJ}
  echo '<?xml version="1.0" encoding="ISO-8859-1"?>' > ${RJT}
  echo "<testsuite tests=\"${pall}\" failures=\"${pko}\" skipped=\"${psk}\" name=\"CM3 package build status\">" >> ${RJ}
  echo "<testsuite tests=\"${tall}\" failures=\"${tko}\" skipped=\"${tsk}\" name=\"CM3 package tests status\">" >> ${RJT}
  echo "${rj}" >> ${RJ}
  echo "${rjt}" >> ${RJT}
  echo "</testsuite>" >> ${RJ}
  echo "</testsuite>" >> ${RJT}

  echo "XML package status report in ${RJ}"
  echo "XML package test report in ${RJT}"

  if [ -n "${DOSHIP}" ]; then
    if test "x${CM3CVSUSER}" != "x"; then
      CM3CVSUSER_AT="${CM3CVSUSER}@"
    else
      CM3CVSUSER_AT=""
    fi
    WWWSERVER=${WWWSERVER:-${CM3CVSUSER_AT}birch.elegosoft.com}
    WWWDEST=${WWWDEST:-${WWWSERVER}:/var/www/modula3.elegosoft.com/cm3/logs}
    scp "${R}" "${WWWDEST}" < /dev/null
  fi
  if [ -n "${WORKSPACE}" ]; then
    echo "moving report to ${RW}"
    mv "${R}" "${RW}"
    mv "${RJ}" "${RJW}"
    mv "${RJT}" "${RJTW}"
  fi
fi

[ -n "${OK}" ] || exit `printf "${REDPKGS}" | wc | awk '{print $1}'`
