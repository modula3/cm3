#!/bin/sh

if [ -r defs.sh ]; then
  . ./defs.sh >/dev/null
elif [ -r ${HOME}/work/cm3/scripts/regression/defs.sh ]; then
  . ${HOME}/work/cm3/scripts/regression/defs.sh >/dev/null
elif [ -r ${HOME}/cm3/cm3/scripts/regression/defs.sh ]; then
  . ${HOME}/cm3/cm3/scripts/regression/defs.sh >/dev/null
fi

LOGS=${LOGS:-/var/www/modula3.elegosoft.com/cm3/}
FNPAT1=${FNPAT1:-'ChangeLog'}
FNPATSUF=${FNPATSUF:-}
FNPATLS=${FNPAT:-${FNPAT1}'*'${FNPATSUF}}
INDEX=${INDEX:-changelog-index.html}
cd $LOGS || exit 1

if [ -f "${INDEX}" ]; then
  mv ${INDEX} ${INDEX}.old
fi
cat > ${INDEX} << EOF
<!DOCTYPE HTML PUBLIC "-//W3C//DTD HTML 4.01 Transitional//EN">
<html>
  <head>
    <title>CM3 Change Log</title>
    <META HTTP-EQUIV="Content-Type" CONTENT="text/html">
    <META HTTP-EQUIV="Content-Style-Type" CONTENT="text/css">
    <META HTTP-EQUIV="Resource-type" CONTENT="document"> 
    <META HTTP-EQUIV="Reply-to" CONTENT="m3-support@elego.de"> 
    <LINK HREF="normal.css" REL="stylesheet" TYPE="text/css">
    <META NAME="robots" content="noindex">
  </head>

  <body bgcolor="#ffffff">
    <h2>CM3 Change Log</h2>

EOF

all=`ls -1 ${FNPAT1}-[0-9]*`
last=`ls -1 ${FNPAT1}-[0-9]* | tail -1`
for f in ${all}; do
  echo "<a href=\"$f\">$f</a><br>"
done >> ${INDEX}

cat >> ${INDEX} <<EOF
    <hr>
    <address><a href="mailto:m3-support{at}elego.de">m3-support{at}elego.de</a></address>
<!-- Created: `date` -->
  </body>
</html>
EOF

rm -f ${INDEX}.old
