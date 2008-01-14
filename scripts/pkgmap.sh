#!/bin/sh
# $Id$

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
. "$sysinfo"
. "$ROOT/scripts/pkginfo.sh"

exec_cmd() {
  echo " +++ $PKG_ACTION +++"
  [ "$NO_ACTION" = yes ] || /bin/sh -c "cd $1 && $PKG_ACTION"
}

PKGS=""
while [ -n "$1" ] ; do
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
      PKG_ACTION="${PKG_ACTION} ; $2"
    fi
    shift
  elif [ -d "$ROOT/$1" ] ; then
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
done

if [ -n "S{REPORT}" ]; then
  DS=${DS:-`date -u +'%Y-%m-%d-%H-%M-%S' | tr -d '\\n'`}
  R="${HTML_REPORT:-${TMPDIR}/cm3-pkg-report-${TARGET}-${DS}.html}"
  ERRS=""
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
    <LINK HREF="normal.css" REL="stylesheet" TYPE="text/css">
    <META NAME="robots" content="noindex">
    <style type="text/css">
    <!--
    .white    { color:#FFFFFF; }
    .bgred    { background:#FF4444; }
    .bggreen  { background:#44FF44; }
    .bgblue   { background:#5555FF; }
    .bgyellow { background:#FFFF00; }
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
        </tr>
      </thead>
      <tbody>

EOF
  ) > "${R}"
}

report_footer() {
  (
    cat << EOF

      </tbody>
    </table>

    <hr>
    <address><a href="mailto:m3-support{at}elego.de">m3-support{at}elego.de</a></address>
<!-- Created: Fri Feb 16 15:27:10 MET 2001 -->
  </body>
</html>
EOF
  ) >> "${R}"
}

write_pkg_report() {
  (
    errlines=`egrep '^".*, line .*:|warning:|version stamp mismatch|bad version stamps|Fatal Error|failed|quake runtime error|ignoring override|unsupported' "$1/stdout.log"`

    if [ "$2" = "0" ] ; then
      echo "<tr class=\"bggreen\">"
    elif [ "$2" = "2" ] ; then
      echo "<tr class=\"bgyellow\">"
    else
      echo "<tr class=\"bgred\">"
      ERRS="${ERRS}${errlines}"
    fi
    echo "  <td class=\"tl\">$1</td>"
    if [ "$2" = "0" ] ; then
      echo "  <td class=\"tl\">build OK</td>"
    elif [ "$2" = "2" ] ; then
      echo "  <td class=\"tl\">not supported on ${TARGET}</td>"
    else
      echo "  <td class=\"tl\">build failed</td>"
    fi
    echo "  <td class=\"small\"><pre>"
    echo "$errlines"
    echo "  </pre></td>"
    echo "</tr>"
  ) >> "${R}"
}

if [ -n "S{REPORT}" ]; then
  report_header
fi

M3GDB=yes
OK=yes

for PKG in ${PKGS} ; do
  echo "=== package ${PKG} ==="
  if [ "${REPORT}" = "yes" ] ; then
    rm -f "${PKG}/stdout.log"
    if UsePackage `basename "${PKG}"` || [ "${CM3_ALL}" = yes ]; then
      exec_cmd "$PKG" > "${PKG}/stdout.log" 2>&1
      res=$?
      cat "${PKG}/stdout.log"
      if grep 'Fatal Error:' "${PKG}/stdout.log" >/dev/null 2>&1; then
        res=1
        OK=""
      elif [ "${res}" = "1" ]; then
        OK=""
      fi
    else
      touch "${PKG}/stdout.log"
      echo "=== package omitted on this platform ==="
      res=2
    fi
    #deps=`cd "${PKG}" && cm3 -depend | head -1`
  else
    exec_cmd "$PKG"
    res=$?
  fi
  if [ -n "S{REPORT}" ]; then
    #if grep ': imported interface' "${PKG}/stdout.log" >/dev/null 2>&1; then
    #  res=2
    #fi
    write_pkg_report "${PKG}" $res
  fi
  if [ "$res" != "0" ] ; then
    if [ "${KEEP_GOING}" != "yes" ] ; then
      echo " *** execution of $ACTION failed ***" 
      exit 1
    fi
  fi
  if [ "${KEEP_GOING}" = "yes" ] ; then
    echo " ==> $PKG_ACTION returned $res"
  else
    echo " ==> ${PKG} done"
  fi
  echo ""
done

if [ -n "S{REPORT}" ]; then
  report_footer
  if [ -n "${ERRS}" ]; then
    echo "errors:" 1>&2
    echo "${ERRS}" 1>&2
  fi
  echo "HTML package report in $R"
fi

[ -n "${OK}" ]
