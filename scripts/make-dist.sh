#bash

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

if test "x${CM3CVSUSER}" != "x"; then
  CM3CVSUSER_AT="${CM3CVSUSER}@"
else
  CM3CVSUSER_AT=""
fi

DESTHOST=${DESTHOST:-${CM3CVSUSER_AT}birch.elegosoft.com}

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

#DS=${DS:-"RC4"}; export DS
DS=${DS:-`date -u +'%Y-%m-%d-%H-%M-%S' | tr -d '\\n'`}; export DS
if [ "x$TARGET" = "xNT386" ]; then
  BF="-"
else
  BF="-`build_platform`-"
fi
STAGE="${STAGE:-${TMPDIR}}"
INSTALLROOT="${STAGE}/cm3"
rm -rf ${INSTALLROOT}
COLLDEPS="${ROOT}/www/releng/collection-deps.txt"

cd "${ROOT}" || exit 1

#if [ -z "${OMIT_UPDATE}" ]; then
#  case ${DS} in
#    RC*)
#        echo cvs -q up -r release_CM3_5_8_${DS} -dP
#        cvs -q up -r release_CM3_5_8_${DS} -dP
#        N=`echo ${DS} | sed -e 's/RC//'`
#        VERSION="5.8.${N}"
#        CM3VERSION="${VERSION}"
#        export VERSION CM3VERSION
#    ;;
#    *)
#        echo cvs -q up -r release_branch_cm3_5_8 -dP
#        cvs -q up -r release_branch_cm3_5_8 -dP;;
#  esac
#fi

# no hardcoded paths in runpath, just $ORIGIN
M3_PORTABLE_RUN_PATH=1
export M3_PORTABLE_RUN_PATH

ERROR_INDICATORS='version stamp mismatch|bad version stamps|Fatal Error|package build failed|quake runtime error|collect2: ld returned|librarian failed building'
if [ -z "${NOBUILD}" ]; then
  echo DIST=min NOCLEAN=yes SYSINFO_DONE="" "$ROOT/scripts/make-bin-dist-min.sh"
  DIST=min NOCLEAN=yes SYSINFO_DONE="" "$ROOT/scripts/make-bin-dist-min.sh" 2>&1 | tee build-min.log
  if egrep "${ERROR_INDICATORS}" build-min.log; then
    echo "building cm3-bin-min archive failed" 1>&2
    exit 1
  fi
  echo DIST=core NOCLEAN=yes SYSINFO_DONE="" "$ROOT/scripts/make-bin-dist-min.sh"
  DIST=core NOCLEAN=yes SYSINFO_DONE="" "$ROOT/scripts/make-bin-dist-min.sh" 2>&1 | tee build-core.log
  if egrep "${ERROR_INDICATORS}" build-core.log; then
    echo "building cm3-bin-core archive failed" 1>&2
    exit 1
  fi
  if [ `hostname` = 'birch' ]; then
    SYSINFO_DONE="" "$ROOT/scripts/make-src-dist-all.sh"
  fi
  echo PATH="${INSTALLROOT}/bin:${PATH}"
  PATH="${INSTALLROOT}/bin:${PATH}"
  echo "$ROOT/scripts/do-cm3-all.sh" buildship -no-m3ship-resolution -group-writable
  "$ROOT/scripts/do-cm3-all.sh" buildship -no-m3ship-resolution -group-writable 2>&1 | tee build-all.log
  if egrep "${ERROR_INDICATORS}" build-all.log; then
    echo "errors during build-all; some packages will be missing" 1>&2
  fi
fi

if [ `uname` = 'Interix' ]; then
  PKG_COLLECTIONS="devlib m3devtool webdev obliq caltech-parser tool math game core"
else
  PKG_COLLECTIONS="min core devlib gui webdev m3gdb m3devtool anim database cvsup obliq juno caltech-parser demo tool math game"
fi

DESC_devlib='<p>
Miscellaneous development libraries
</p>
<p>Related documents:</p>
<p>none found</p>
'
DESC_m3devtool='<p>
Modula-3 development tools
</p>
<p>Related documents:</p>
<ul>
<li><a href="http://www.opencm3.net/doc/src_reports/src-115.pdf">Network Objects</a></li>
<li><a href="http://www.opencm3.net/doc/src_reports/blue_paper/index.html">CM3 IDE</a></li>
<li><a href=""></a></li>
</ul>
'
DESC_m3gdb='<p>
The GNU debugger with Modula-3 support
</p>
<p>Related documents:</p>
<ul>
<li><a href="http://www.opencm3.net/doc/help/m3gdb/m3gdb-onepage.html">m3gdb</a></li>
</ul>
'
DESC_webdev='<p>
Packages for web development
</p>
<p>Related documents:</p>
<p>none found</p>
'
DESC_gui='<p>
Graphical User Interface Libraries
</p>
<p>Related documents:</p>
<ul>
<li><a href="http://www.opencm3.net/doc/src_reports/src-069.pdf">Trestle Tutorial</a></li>
<li><a href="http://www.opencm3.net/doc/src_reports/src-068.pdf">Trestle Reference Manual</a></li>
<li><a href="http://www.opencm3.net/doc/src_reports/vbtkit.pdf">VBTKit Reference Manual</a></li>
<li><a href="http://www.opencm3.net/doc/src_reports/formsvbt.pdf">FormsVBT Reference Manual</a></li>
<li><a href=""></a></li>
</ul>
'
DESC_anim='<p>
Animation Libraries
</p>
<p>Related documents:</p>
<ul>
<li><a href="ftp://gatekeeper.research.compaq.com/pub/DEC/SRC/research-reports/abstracts/src-rr-076a.html">Color and Sound in Algorithm Animation</a></li>
<li><a href="ftp://gatekeeper.research.compaq.com/pub/DEC/SRC/research-reports/abstracts/src-rr-075.html">Zeus: A System for Algorithm Animation and Multi-View Editing</a></li>
<li><a href="ftp://gatekeeper.research.compaq.com/pub/DEC/SRC/research-reports/abstracts/src-rr-098.html">The 1992 SRC Algorithm Animation Festival</a></li>
<li><a href="ftp://gatekeeper.research.compaq.com/pub/DEC/SRC/research-reports/abstracts/src-rr-126.html">The 1993 SRC Algorithm Animation Festival</a></li>
<li><a href=""></a></li>
</ul>
'
DESC_database='<p>
Database interfaces and implementations
</p>
<p>Related documents:</p>
<p>none found</p>
'
DESC_cvsup='<p>
Efficient CVS repository and general file replication program.
See <a href="http://www.cvsup.org">www.cvsup.org<a>.
</p>
<p>Related documents:</p>
<p>none found</p>
'
DESC_obliq='<p>
The Obliq language
</p>
<p>Related documents:</p>
<ul>
<li><a href="http://www.vlsi.polymtl.ca/dagenais/if515/main/main.html">Building Distributed OO Applications: Modula-3 Objects at Work</a></li>
<li><a href="ftp://gatekeeper.research.compaq.com/pub/DEC/SRC/research-reports/abstracts/src-rr-129.html">Obliq-3D Tutorial and Reference Manual</a></li>
<li><a href=""></a></li>
</ul>
'
DESC_juno='<p>
A constraint-based graphical editor
</p>
<p>Related documents:</p>
<ul>
<li><a href="ftp://gatekeeper.research.compaq.com/pub/DEC/SRC/research-reports/abstracts/src-rr-131a.html">The Juno-2 Constraint-Based Drawing Editor</a></li>
<li><a href="http://www.research.digital.com/SRC/juno-2/">Juno-2 Home Page</a></li>
</ul>
'
DESC_caltechparser='<p>
Parser Generator in Modula-3
</p>
<p>Related documents:</p>
<p>none found</p>
'
DESC_demo='<p>
Miscellaneous demo programs
</p>
<p>Related documents:</p>
<p>none found</p>
'
DESC_tool='<p>
Miscellaneous small tools
</p>
<p>Related documents:</p>
<p>none found</p>
'
DESC_math='<p>
Arithmetic and other mathematical support packages
</p>
<p>Related documents:</p>
<p>none found</p>
'
DESC_game='<p>
Games written in Modula-3
</p>
<p>Related documents:</p>
<p>none found</p>
'
DESC_core='<p>
The core packages comprise the actual compiler packages, quake,
support tools like m3bundle and some general useful libraries.
</p>
<p>Related documents:</p>
<ul>
<li><a href="http://www.opencm3.net/doc/reference/index.html">M3 Language Reference</a></li>
<li><a href="http://www.opencm3.net/doc/help/cm3/cm3.html">CM3 Operations Guide</a></li>
<li><a href="http://www.opencm3.net/doc/help/cm3/cm3-quickref.html">CM3 Quick Reference</a></li>
<li><a href="http://www.opencm3.net/doc/help/interfaces.html">CM3 Interface Index</a></li>
<li><a href="http://www.opencm3.net/doc/help/cm3/quake.html">Quake Reference</a></li>
<li><a href="http://www.opencm3.net/doc/tutorial/m3/m3_toc.html">M3 Tutorial</a></li>
<li><a href="http://www.opencm3.net/doc/src_reports/blue_paper/index.html">CM3 IDE</a></li>
<li><a href="http://www.opencm3.net/doc/src_reports/src-113.pdf">Some Useful Modula-3 Interfaces</a></li>
<li><a href="http://www.opencm3.net/doc/src_reports/m3poster.pdf">M3 Syntax Diagrams Poster</a></li>
<li><a href="http://www.opencm3.net/doc/src_reports/m3syntax.pdf">M3 Syntax Diagrams, Non-Terminal</a></li>
<li><a href="http://www.opencm3.net/doc/src_reports/m3tokens.pdf">M3 Syntax Diagrams, Terminals</a></li>
<li><a href="http://www.cs.columbia.edu/graphics/modula3/tutorial/.index.html">Modula-3 Reference and Tutorial</a></li>
</ul>
'

#
# Windows setup is one constant setup.cmd, with a setup.txt
# next to it, containing relative directories from $ROOT.
# Unix setup is a generated install.sh at least for now.
#

cd "${ROOT}"
for c in ${PKG_COLLECTIONS}; do
  rm -f setup.txt
  P=`fgrep " $c" $ROOT/scripts/pkginfo.txt | awk "{print \\$1}" | tr '\\n' ' '`
  PKGS=""
  for x in $P; do
    p="$x"
    if [ -d "$x" ] ; then
      PKGS="${PKGS} $x"
      echo "$x" >> setup.txt
    else
      p=`pkgpath $x`
      if [ -d "$p" ] ; then
        PKGS="${PKGS} $p"
        echo "$p" >> setup.txt
      else
        echo " *** cannot find package $x / $p" 1>&2
        exit 1
      fi
    fi
    m3ship="${p}/${TARGET}/.M3SHIP"
    if [ -f ${m3ship} ]; then
      if res=`egrep '/home|/var|/tmp' ${m3ship}`; then
        echo "${m3ship} seems to be broken:" 1>&2
        echo "${res}" 1>&2
        exit 1
      fi
    fi
  done
  (
    echo '#!/bin/sh'
    echo 'HERE=`pwd`'
    echo 'ACTION=${ACTION:-"cm3 -ship"}'
    echo "for p in ${PKGS}; do"
      echo 'echo "installing package ${p}"'
      echo 'cd $p'
      echo '${ACTION} ${SHIPARGS} || {'
      echo '  echo "installation of ${p} failed" 1>&2'
      echo '}'
      echo 'cd $HERE'
    echo "done"
  ) > install.sh
  chmod 755 install.sh
  cp -p $ROOT/scripts/win/setup.cmd $ROOT/setup.cmd
  chmod 755 setup.cmd
  (
    echo "<html>"
    cat <<EOF
  <head>
    <title>CM3 Package Collection $c</title>
    <META HTTP-EQUIV="Content-Type" CONTENT="text/html">
    <META HTTP-EQUIV="Content-Style-Type" CONTENT="text/css">
    <META HTTP-EQUIV="Resource-type" CONTENT="document">
    <META HTTP-EQUIV="Reply-to" CONTENT="m3-support@elego.de">
    <LINK HREF="http://www.opencm3.net/normal.css" REL="stylesheet" TYPE="text/css">
    <META NAME="robots" content="noindex">
  </head>
  <body>
    <h1>CM3 Package Collection $c</h1>
EOF
    if [ -r ${COLLDEPS} ]; then
      CHEADER=`${EGREP} -C 5 "^collection $c" ${COLLDEPS} | tail -5`
      echo "<h2 style=\"text-align:left\">Dependencies</h2>"
      echo "<pre>${CHEADER}</pre>"
    fi
    echo "<h2 style=\"text-align:left\">Description</h2>"
    ddd=${ddd:=DESC_${c}}
    echo ${ddd}
    echo "<h2 style=\"text-align:left\">Package Details</h2>"
    echo "<ul>"
    for p in ${PKGS}; do
      b=`basename ${p}`
      # FIXME: something like this should be in the style sheet normal.css
      echo "<dt style=\"font-size:1.2em; font-weight:bold; padding-top:0.5em;\">$p</dt>"
      echo "<dd>"
      if [ -r ${p}/DESC ]; then
        cat ${p}/DESC
      fi
      if [ -r ${p}/index.html ]; then
        echo "<a href=\"ws/${p}/index.html\">Description</a><br>"
      fi
      readmes=`$FIND "${p}" -type f -name README -print`
      if [ -n "$readmes" ]; then
        for f in $readmes; do
          if [ -r ${f} ]; then
            echo "<a href=\"ws/${f}\">${f}</a><br>"
          fi
        done
      fi
      if [ "$b" != "m3cc" -a "$b" != "m3gdb" ]; then
        echo "<a href=\"http://www.opencm3.net/doc/help/gen_html/${b}/INDEX.html\">Browse Sources Online</a><br>"
      fi
      for section in 1 5 6 7 8; do
        manpages=`$FIND ${p}/src -name "[A-Za-z]*.${section}" -print`
        [ ${p} = m3-tools/m3tk ] && manpages="" # only fragments in m3tk, ignore
        if [ -n "${manpages}" ]; then
          for m in ${manpages}; do
            mb=`basename ${m} .${section}`
            echo "<a href=\"http://modula3.elegosoft.com/cm3/cgi-bin/man2html.cgi?local=/usr/local/cm3/man/man${section}/${mb}.${section}\">Manual Page ${mb}</a><br>"
          done
        fi
      done
      echo "</dd>"
    done
    echo "</ul>"
    echo "</body></html>"
  ) > collection-${c}.html
  echo "collection-${c}.html"
  ARCHIVE="${STAGE}/cm3-bin-ws-${c}-${TARGET}-${CM3VERSION}${BF}${DS}.tgz"
  if [ -z "${NOARCHIVE}" -a "${c}" != "min" ]; then
    "${TAR}"  --exclude '*.o' --exclude '*.mo' --exclude '*.io' \
      --exclude '*/CVS/*' --exclude '*/CVS' --exclude '*~' \
      --exclude '*.tar.*' --exclude '*.tgz' --exclude "*/${TARGET}/gcc" \
      --exclude "*/${TARGET}/*/*" \
      -czf "${ARCHIVE}" collection-${c}.html install.sh setup.txt setup.cmd ${PKGS}
      ls -l "${ARCHIVE}"
  fi
done

set -x

# cleanup after previous failed runs of package making
rm -rf $STAGE/usr/local/cm3

if type python; then
  if [ "x$TARGET" = "xNT386" ]; then
    python "$ROOT/scripts/python/make-msi.py" "$INSTALLROOT"
    mv "$INSTALLROOT.msi" "$STAGE/cm3-$TARGET${BF}${DS}.msi"
  else
    python "$ROOT/scripts/python/make-deb.py" "$INSTALLROOT"
    mv "$INSTALLROOT.deb" "$STAGE/cm3-$TARGET${BF}${DS}.deb"
  fi
else
  echo "python not available, skipping .msi and .deb creation"
fi

echo "hostname=`hostname`"
echo "SHIPRC=$SHIPRC"

if [ `hostname` = 'birch' ]; then
  ARCHIVE="${STAGE}/cm3-scripts-${CM3VERSION}${BF}${DS}.tgz"
  "${TAR}"  --exclude '*.o' --exclude '*.mo' --exclude '*.io' \
    -czf "${ARCHIVE}" scripts
  ls -l "${ARCHIVE}"

  ARCHIVE="${STAGE}/cm3-doc-${CM3VERSION}${BF}${DS}.tgz"
  "${TAR}"  --exclude '*.o' --exclude '*.mo' --exclude '*.io' \
    -czf "${ARCHIVE}" doc www
  ls -l "${ARCHIVE}"
fi

if [ "$SHIPRC" = "y" -o "$SHIPRC" = "yes" ]; then
  if [ "x$RSYNC" = "x" ]; then
    find_in_list RSYNC "/opt/csw/bin/rsync rsync scp" || exit 1
    if [ "x$RSYNC" = "xscp" ]; then :; else
      RSYNC="${RSYNC} -vu"
    fi
  fi
  false; while [ $? != 0 ]; do
    $RSYNC ${STAGE}/cm3-*${BF}${DS}.tgz $DESTHOST:/var/www/modula3.elegosoft.com/cm3/releng
  done
  for ext in msi deb; do
    if [ -r "$STAGE/cm3-$TARGET${BF}${DS}.$ext" ]; then
      false; while [ $? != 0 ]; do
        $RSYNC "$STAGE/cm3-$TARGET${BF}${DS}.$ext" $DESTHOST:/var/www/modula3.elegosoft.com/cm3/releng
      done
    fi
  done
  if [ `hostname` = 'birch' ]; then
    false; while [ $? != 0 ]; do
      $RSYNC collection-*.html $DESTHOST:/var/www/modula3.elegosoft.com/cm3/releng
    done
  fi
fi
