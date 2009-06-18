#bash
# $Id: make-dist.sh,v 1.11 2009-06-18 22:26:59 wagner Exp $

DESTHOST=${DESTHOST:-birch,elegosoft.com}

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

STAGE="${STAGE:-${TMPDIR}}"
INSTALLROOT="${STAGE}/cm3"
rm -rf ${INSTALLROOT}

DS="RC1"; export DS
if [ -z "${NOBUILD}" ]; then
  DIST=min  NOCLEAN=yes SYSINFO_DONE="" "$ROOT/scripts/make-bin-dist-min.sh"
  DIST=core NOCLEAN=yes SYSINFO_DONE="" "$ROOT/scripts/make-bin-dist-min.sh"
  if [ `hostname` = 'birch' ]; then
    SYSINFO_DONE="" "$ROOT/scripts/make-src-dist-all.sh"
  fi
  PATH="${INSTALLROOT}/bin:${PATH}"
  "$ROOT/scripts/do-cm3-all.sh" buildship -no-m3ship-resolution -group-writable
fi
# keep short runpaths
export M3_PORTABLE_RUN_PATH=1

PKG_COLLECTIONS="devlib m3devtool m3gdb webdev gui anim database cvsup obliq juno caltech-parser demo tool math game core"

DESC_devlib='<p>
Miscellaneous development libraries
</p>
<p>Related documents:</p>
<ul>
<li><a href=""></a></li>
</ul>
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
<ul>
<li><a href=""></a></li>
</ul>
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
<ul>
<li><a href=""></a></li>
</ul>
'
DESC_cvsup='<p>
Efficient CVS repository and general file replication program.
See <a href="http://www.cvsup.org">www.cvsup.org<a>.
</p>
<p>Related documents:</p>
<ul>
<li><a href=""></a></li>
</ul>
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
<ul>
<li><a href=""></a></li>
</ul>
'
DESC_demo='<p>
Miscellaneous demo programs
</p>
<p>Related documents:</p>
<ul>
<li><a href=""></a></li>
</ul>
'
DESC_tool='<p>
Miscellaneous small tools
</p>
<p>Related documents:</p>
<ul>
<li><a href=""></a></li>
</ul>
'
DESC_math='<p>
Arithmetic and other mathematical support packages
</p>
<p>Related documents:</p>
<ul>
<li><a href=""></a></li>
</ul>
'
DESC_game='<p>
Games written in Modula-3
</p>
<p>Related documents:</p>
<ul>
<li><a href=""></a></li>
</ul>
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
<li><a href="http://www.opencm3.net/doc/src_reports/src-113.pdf">Some Useful odula-3 Interfaces</a></li>
<li><a href="http://www.opencm3.net/doc/src_reports/m3poster.pdf">M3 Syntax Diagrams Poster</a></li>
<li><a href="http://www.opencm3.net/doc/src_reports/m3syntax.pdf">M3 Syntax Diagrams, Non-Terminal</a></li>
<li><a href="http://www.opencm3.net/doc/src_reports/m3tokens.pdf">M3 Syntax Diagrams, Terminals</a></li>
<li><a href="http://www.cs.columbia.edu/graphics/modula3/tutorial/.index.html">Modula-3 Reference and Tutorial</a></li>
</ul>
'

cd "${ROOT}"
for c in ${PKG_COLLECTIONS}; do
  P=`fgrep " $c" $ROOT/scripts/pkginfo.txt | awk "{print \\$1}" | tr '\\n' ' '`
  PKGS=""
  for x in $P; do
    if [ -d "$x" ] ; then
      PKGS="${PKGS} $x"
    else
      p=`pkgpath $x`
      if [ -d "$p" ] ; then
        PKGS="${PKGS} $p"
      else
        echo " *** cannot find package $x / $p" 1>&2
        exit 1
      fi
    fi
  done
  (
    echo '#!/bin/sh'
    echo 'HERE=`pwd`'
    echo "for p in ${PKGS}; do"
      echo 'cd $p'
      echo 'cm3 -ship ${SHIPARGS}'
      echo 'cd $HERE'
    echo "done"
  ) > install.sh
  chmod 755 install.sh
  (
    echo "<html><body><h1>CM3 Package Collection $c</h1>"
    ddd=''
    ddd=${ddd:=DESC_${c}}
    echo ${!ddd}
    echo "<p>This collections contains the following packages:</p>"
    echo "<ul>"
    for p in ${PKGS}; do
      b=`basename ${p}`
      echo "<dt>$p</dt>"
      echo "<dd>"
      if [ -r ${p}/DESC ]; then
        cat ${p}/DESC
      fi
      if [ -r ${p}/index.html ]; then
        echo "<a href=\"${p}/index.html\">Description</a><br>"
      fi
      if [ -r ${p}/README ]; then
        echo "<a href=\"${p}/README\">README</a><br>"
      fi
      echo "<a href=\"http://www.opencm3.net/doc/help/gen_html/${b}/INDEX.html\">Browse Sources Online</a><br>"
      for section in 1 5 6 8; do
        manpages=`find ${p}/src -name "*.${section}" -print`
        if [ -n "${manpages}" ]; then
          for m in ${manpages}; do
            mb=`basename ${m} .${section}`
            echo "<a href=\"http://www.opencm3.net/man/man${section}/${mb}.${section}\">Manual Page ${mb}</a><br>"
          done
        fi
      done
      echo "</dd>"
    done
    echo "</ul>"
    echo "</body></html>"
  ) > collection-${c}.html
  ARCHIVE="${STAGE}/cm3-bin-ws-${c}-${TARGET}-${CM3VERSION}-${DS}.tgz"
  "${TAR}"  --exclude '*.o' --exclude '*.mo' --exclude '*.io' \
    -czf "${ARCHIVE}" collection-${c}.html install.sh ${PKGS}
  ls -l "${ARCHIVE}"
done

if [ `hostname` = 'birch' ]; then
  ARCHIVE="${STAGE}/cm3-scripts-${CM3VERSION}-${DS}.tgz"
  "${TAR}"  --exclude '*.o' --exclude '*.mo' --exclude '*.io' \
    -czf "${ARCHIVE}" scripts
  ls -l "${ARCHIVE}"

  ARCHIVE="${STAGE}/cm3-doc-${CM3VERSION}-${DS}.tgz"
  "${TAR}"  --exclude '*.o' --exclude '*.mo' --exclude '*.io' \
    -czf "${ARCHIVE}" doc www
  ls -l "${ARCHIVE}"
fi
if [ "$SHIPRC" = "y" -o "$SHIPRC" = "yes" ]; then
  scp ${STAGE}/cm3-*-${DS}.tgz $DESTHOST:/var/www/modula3.elegosoft.com/cm3/releng
  if [ `hostname` = 'birch' ]; then
    scp collection-*.html $DESTHOST:/var/www/modula3.elegosoft.com/cm3/releng
  fi
fi
