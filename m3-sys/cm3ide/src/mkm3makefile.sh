#!/bin/sh

. ../../../scripts/sysinfo.sh

#DIRS=`find examples -type d -print | egrep -v "CVS|${TARGET}"`
FILES=`find examples -type f -print | \
       egrep -v "CVS|${TARGET}|examples/m3makefile|~$|.bak"`

M=${M:-examples/m3makefile}

echo "if defined(\"RootExport\")" > $M

for f in $FILES; do
  d=`dirname $f`
  s=`echo $f | sed -e 's;examples/;;'`
  echo "  RootExport(\"$s\", \"$d\")"
done >> $M

echo "else" >> $M
echo "  write(\"This compiler is too old to ship the examples.\" & EOL)" >> $M
echo "  write(\"Please copy them manually or upgrade cm3.\" & EOL)" >> $M
echo "end" >> $M


