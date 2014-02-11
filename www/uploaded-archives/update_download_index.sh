#!/bin/bash

SNAPS=${SNAPS:-/var/www/modula3.elegosoft.com/cm3/uploaded-archives}
INDEX=${INDEX:-index.html}
cd $SNAPS || exit 1

for t in `cat targets.txt`; do
  if ls -1t cm3-*${t}-* >/dev/null 2>/dev/null; then
    TARGETS="${TARGETS} ${t}"
  fi
done

if [ -f "${INDEX}" ]; then
  mv ${INDEX} ${INDEX}.old
fi
cat > ${INDEX} << EOF
<!DOCTYPE HTML PUBLIC "-//W3C//DTD HTML 4.01 Transitional//EN">
<html>
  <head>
    <title>CM3 Uploaded Archives</title>
    <META HTTP-EQUIV="Content-Type" CONTENT="text/html">
    <META HTTP-EQUIV="Content-Style-Type" CONTENT="text/css">
    <META HTTP-EQUIV="Resource-type" CONTENT="document"> 
    <META HTTP-EQUIV="Reply-to" CONTENT="m3-support@elego.de"> 
    <LINK HREF="../normal.css" REL="stylesheet" TYPE="text/css">
    <META NAME="robots" content="noindex">
  </head>

  <body>
    <h2>CM3 Uploaded Archives</h2>

    <p>
      This directory contains archives contributed by members of the
      CM3 project. They may be of various quality as they are not
      tested before placed here. Use at your own risk.
    </p>

EOF

chmod -x *.msi *.zip *.gz
# echo $TARGETS
for t in ${TARGETS}; do
  echo "1$t" >> 1.txt
  if [ $t = NT386 ]; then
    all=`ls -1t cm3*msi cm3*zip cm3-*${t}-* 2>/dev/null | grep -v AMD64 | sort`
  else
    all=`ls -1t cm3-*${t}-* 2>/dev/null | sort`
  fi
  echo "<h3>Target Platform ${t}</h3>"
  echo "<table border=\"3\" cellspacing=\"2\" cellpadding=\"4\" width=\"95%\"><tbody>"
  for f in ${all}; do
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
      echo "-"
    fi
    echo "</td></tr>"
  done
  echo "</tbody></table>"
done >> ${INDEX}

# copied from snapshot index script in a hurry
FNPAT1=${FNPAT1:-"cm3-min-${CM3_OSTYPE}-"}
FNPATSUF=${FNPATSUF:-.tgz}
FNPATLS=${FNPAT:-${FNPAT1}'*-*'${FNPATSUF}}
FNPATSRCSTART=${FNPATSRCSTART:-cm3-src}
FNPATSRCEND=${FNPATSRCEND:-*.tgz}
FNPATSRC=${FNPATSRC:-${FNPATSRCHEAD}-*.tgz}

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
    echo "-"
  fi
  echo "</td></tr>"
}

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
  for f in `ls -1t -- ${FNPATSRCSTART}-${d}-${FNPATSRCEND}`; do
    tablerow $f
  done
  echo "</tbody></table>"
  echo ""
done >> ${INDEX}

cat >> ${INDEX} <<EOF
    <hr>
    <address><a href="mailto:m3-support{at}elego.de">m3-support{at}elego.de</a></address>
<!-- Created: `date` -->
  </body>
</html>
EOF

rm -f ${INDEX}.old
