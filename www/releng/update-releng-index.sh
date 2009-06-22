#!/bin/sh

NKEEP=${NKEEP:-100}
if [ -r defs.sh ]; then
  . ./defs.sh >/dev/null
elif [ -r ${HOME}/work/cm3/scripts/regression/defs.sh ]; then
  . ${HOME}/work/cm3/scripts/regression/defs.sh >/dev/null
elif [ -r ${HOME}/cm3/cm3/scripts/regression/defs.sh ]; then
  . ${HOME}/cm3/cm3/scripts/regression/defs.sh >/dev/null
fi

SNAPS=${SNAPS:-/var/www/modula3.elegosoft.com/cm3/releng}
TARGETS=`ls *RC*.tgz | egrep cm3-bin- | 
         sed -e 's/cm3-bin-.*-\([^\-]*\)-[^\-]*-RC[0-9]*.tgz/\1/' |
         sort -u`
echo $TARGETS

INDEX=${INDEX:-index.html}

if [ -f "${INDEX}" ]; then
  mv ${INDEX} ${INDEX}.old
fi
cp index-frag.html ${INDEX}

tablerow() {
  f="$1"
  echo "<tr>"
  ls -hl "$f" | awk ' {
    printf "<td width=\"15%%\" align=\"right\">\n"
    printf "%s", $6
    printf "</td><td width=\"6%%\" align=\"left\">\n"
    printf "%s", $7
    printf "</td><td width=\"6%%\" align=\"right\">\n"
    printf "%s", $5
  }'
  echo "</td><td width=\"63%\" align=\"left\">"
  echo "<a href=\"$f\">$f</a>"
  echo "</td><td width=\"10%\" align=\"center\">"
  if [ -r "$f.README" ]; then
    echo "<a href=\"$f.README\">README</a>"
  elif [ -r "$f.html" ]; then
    echo "<a href=\"$f.html\">Notes</a>"
  elif [ -r "$f.txt" ]; then
    echo "<a href=\"$f.txt\">Notes</a>"
  else
    case "$f" in
      *-bin-core-*)
        echo '<a href="collection-core.html">standard, recommended</a>';;
      *-bin-min-*)
        echo '<a href="collection-min.html">minimal</a>';;
      *-bin-ws-m3devtool*)
        echo '<a href="collection-m3devtool.html">recommended</a>';;
      *-bin-ws-*)
        coll=`echo $f | awk -F- '{print $4}'`
        echo "<a href=\"collection-${coll}.html\">optional</a>"
      ;;
      *) echo "-";;
    esac
  fi
  echo "</td></tr>"
}

for rc in RC1 RC2 RC3 RC4 RC5 RC6 RC7 RC8 RC9; do

  if [ "`echo *${rc}*.tgz`" = '*'${rc}'*.tgz' ]; then
    break
  fi

  echo "  <h2><a name=\"doc-${rc}\">Documentation and Support Scripts ${rc}</a></h2>" >> ${INDEX}
  echo "" >> ${INDEX}
  echo "<table border=\"3\" cellspacing=\"2\" cellpadding=\"4\" width=\"95%\"><tbody>" >> ${INDEX}
  for f in cm3-src-*.tgz cm3-doc-*.tgz cm3-scripts-*.tgz; do
    tablerow $f
  done >> ${INDEX}
  echo "</tbody></table>" >> ${INDEX}

  echo "  <h2><a name=\"bin-${rc}\">Binary Distribution Archives ${rc}</a></h2>" >> ${INDEX}
  echo "" >> ${INDEX}


  for t in ${TARGETS}; do
    all=`ls -1 cm3-bin-*-${t}-*-${rc}.tgz cm3-bin-*-${t}-*-${rc}.tar.gz cm3-bin-*-${t}-*-${rc}.tar.lzma cm3-bin-*-${t}-*-${rc}.tar.xz`
    #ln -sf "${last}" "${FNPAT1}${t}${FNPATSUF}"
    echo "<h3>Target Platform ${t}</h3>"
    echo "<table border=\"3\" cellspacing=\"2\" cellpadding=\"4\" width=\"95%\"><tbody>"
    for f in ${all}; do
      tablerow $f
    done
    echo "</tbody></table>"
    echo "<p></p>"
  done >> ${INDEX}

# (
#   echo "<hr>"
#   echo "<h3>Source Archives</h3>"
#   echo "<p style=\"margin-left:2em\">"
#   echo "ALL -- all sources<br>"
#   echo "STD -- sources for the standard set of packages<br>"
#   echo "GNU -- sources for all GNU packages<br>"
#   echo "SYS -- sources for all CM3 system packages<br>"
#   echo "</p>"
#   echo "<hr>"
# ) >> ${INDEX}

# for d in all std gnu sys; do
#   DIST=`echo ${d} | tr '[:lower:]' '[:upper:]'`
#   echo "<h4>Source Archives ${DIST}</h4>"

#   echo "<table border=\"3\" cellspacing=\"2\" cellpadding=\"4\" width=\"95%\"><tbody>"
#   for f in `ls -1t ${FNPATSRCSTART}-${d}-${FNPATSRCEND}`; do
#     tablerow $f
#   done
#   echo "</tbody></table>"
#   echo ""
# done >> ${INDEX}

done

cat >> ${INDEX} <<EOF
    <hr>
    <address><a href="mailto:m3-support{at}elego.de">m3-support{at}elego.de</a></address>
<!-- Created: `date` -->
  </body>
</html>
EOF

rm -f ${INDEX}.old
