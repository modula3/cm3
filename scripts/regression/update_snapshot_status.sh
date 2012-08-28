#!/bin/sh
set -x
NKEEP=${NKEEP:-10}
if [ -r defs.sh ]; then
  . ./defs.sh >/dev/null
elif [ -r ${HOME}/work/cm3/scripts/regression/defs.sh ]; then
  . ${HOME}/work/cm3/scripts/regression/defs.sh >/dev/null
elif [ -r ${HOME}/cm3/cm3/scripts/regression/defs.sh ]; then
  . ${HOME}/cm3/cm3/scripts/regression/defs.sh >/dev/null
fi

SNAPS=${SNAPS:-/var/www/modula3.elegosoft.com/cm3/snaps}
CM3_OSTYPE=${CM3_OSTYPE:-POSIX}
FNPAT1=${FNPAT1:-"cm3-min-"}
FNPAT2=${FNPAT2:-"cm3-bin-*-"}
FNPAT2s=${FNPAT2s:-"cm3-bin-[^-]*-"}
FNPATSUF=${FNPATSUF:-.tgz}
FNPATLS=${FNPATLS:-${FNPAT1}'*-*'${FNPATSUF}}
FNPATLS2=${FNPATLS2:-${FNPAT2}'*-*'${FNPATSUF}}
FNPATSRCSTART=${FNPATSRCSTART:-cm3-src}
FNPATSRCEND=${FNPATSRCEND:-*.tgz}
FNPATSRC=${FNPATSRC:-${FNPATSRCHEAD}-*.tgz}
INDEX=${INDEX:-snapshot-index.html}
cd $SNAPS || exit 1

TARGETS1=`ls -1 ${FNPATLS} |
  sed -e "s/${FNPAT1}\([A-Za-z0-9_]*\)-.*${FNPATSUF}/\1/" |
  sort -u`
TARGETS2=`ls -1 ${FNPATLS2} |
  sed -e "s/${FNPAT2s}\([A-Za-z0-9_]*\)-.*${FNPATSUF}/\1/" |
  sort -u`
TARGETS=`echo ${TARGETS1} ${TARGETS2} | xargs -n 1 echo | sort -u`
echo $TARGETS

if [ -f "${INDEX}" ]; then
  mv ${INDEX} ${INDEX}.old
fi
cat > ${INDEX} << EOF
<!DOCTYPE HTML PUBLIC "-//W3C//DTD HTML 4.01 Transitional//EN">
<html>
  <head>
    <title>CM3 Snapshots</title>
    <META HTTP-EQUIV="Content-Type" CONTENT="text/html">
    <META HTTP-EQUIV="Content-Style-Type" CONTENT="text/css">
    <META HTTP-EQUIV="Resource-type" CONTENT="document"> 
    <META HTTP-EQUIV="Reply-to" CONTENT="m3-support@elego.de"> 
    <LINK HREF="../normal.css" REL="stylesheet" TYPE="text/css">
    <META NAME="robots" content="noindex">
  </head>

  <body>
    <h2>CM3 Snapshots</h2>

    <h3>Binary Distribution Archives</h3>

EOF

tablerow() {
  f="$1"
  echo "<tr>"
  LANG=en_US.ISO-8859-15; export LANG
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
    echo "-"
  fi
  echo "</td></tr>"
}

for t in ${TARGETS}; do
  all=`ls -1rt ${FNPAT1}${t}-*${FNPATSUF} ${FNPAT2}${t}-*${FNPATSUF}`
  last=`ls -1rt ${FNPAT1}${t}-*${FNPATSUF} ${FNPAT2}${t}-*${FNPATSUF} | tail -1`
  last10=`ls -1rt ${FNPAT1}${t}-*${FNPATSUF} ${FNPAT2}${t}-*${FNPATSUF}| tail -10`
  ln -sf "${last}" "${FNPAT1}${t}${FNPATSUF}"
  echo "<h4>Target Platform ${t}</h4>"
  echo "<table border=\"3\" cellspacing=\"2\" cellpadding=\"4\" width=\"95%\"><tbody>"
  for f in ${last10}; do
    tablerow $f
  done
  echo "</tbody></table>"
  echo "<p></p>"
done >> ${INDEX}

(
  echo "<hr>"
  echo "<h3>Source Archives</h3>"
  echo "<p style=\"margin-left:2em\">"
  echo "ALL -- all sources<br>"
  echo "STD -- sources for the standard set of packages<br>"
  echo "GNU -- sources for all GNU packages<br>"
  echo "SYS -- sources for all CM3 system packages<br>"
  echo "</p>"
  echo "<hr>"
) >> ${INDEX}

for d in all std gnu sys; do
  DIST=`echo ${d} | tr '[:lower:]' '[:upper:]'`
  echo "<h4>Source Archives ${DIST}</h4>"

  echo "<table border=\"3\" cellspacing=\"2\" cellpadding=\"4\" width=\"95%\"><tbody>"
  for f in `ls -1t ${FNPATSRCSTART}-${d}-${FNPATSRCEND}`; do
    tablerow $f
  done
  echo "</tbody></table>"
  echo ""
done >> ${INDEX}

# cleanup

for t in ${TARGETS}; do
  pat="${FNPAT1}${t}-*${FNPATSUF}"
  ls -1rtd ${pat} | cleanup_all_but_last_n ${NKEEP}
done

for t in ${TARGETS}; do
  pat="${FNPAT2}${t}-*${FNPATSUF}"
  ls -1rtd ${pat} | cleanup_all_but_last_n ${NKEEP}
done

for d in all std gnu sys; do
  pat="${FNPATSRCSTART}-${d}-${FNPATSRCEND}"
  ls -1rtd ${pat} | cleanup_all_but_last_n ${NKEEP}
done

cat >> ${INDEX} <<EOF
    <hr>
    <address><a href="mailto:m3-support{at}elego.de">m3-support{at}elego.de</a></address>
<!-- Created: `date` -->
  </body>
</html>
EOF

rm -f ${INDEX}.old
