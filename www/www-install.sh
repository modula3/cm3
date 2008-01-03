#!/bin/sh

if [ -z "$FILES" -a -n "$1" ] ; then
  FILES=$@
fi

FILES=${FILES:-*.html}

for f in $FILES; do
  if [ ! -f $f ] ; then
     echo "$f not found" 1>&2
     exit 1
  fi
done

DEST=""
DEST="$DEST root@www-he:/var/www/html/cm3/"
#DEST="$DEST web27@www-mw:web/cm3/"
DEST="$DEST m3@www.m3.org:html/cm3/"

for d in $DEST; do
  echo "copying files to $d"
  if scp ${FILES} ${d} ; then
    echo "done [ok]"
  else
    echo "file transfer failed" 1>&2
  fi
done

