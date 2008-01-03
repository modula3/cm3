#!/bin/sh
# $Id: dirsexist.sh,v 1.2 2001-09-19 15:39:38 wagner Exp $
#
# dirsexist.sh -- take $* and print it, but only for those words that are
# existing directories

PRINTDIRECTION=$1
shift
DIRS=$*
DIRSEXIST=""

if [ -x /usr/bin/tac ]; then
	REVCMD=/usr/bin/tac
else
	REVCMD="tail -r"
fi

for dir in $DIRS; do
	if [ -d $dir ]; then
		DIRSEXIST="${DIRSEXIST} ${dir}"
	fi
done

if [ "X${PRINTDIRECTION}" = "X-f" ]; then
	echo ${DIRSEXIST}
elif [ "X${PRINTDIRECTION}" = "X-r" ]; then 
	echo ${DIRSEXIST} | xargs -n1 echo | ${REVCMD} | xargs echo
else
	echo "ERROR, wrong args"
	exit 1
fi

