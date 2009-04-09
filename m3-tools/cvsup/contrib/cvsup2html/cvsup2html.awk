#!/usr/bin/awk -f
#
# Convert a CVSup logfile to HTML.  Note that this script requires nawk
# or better (gawk will do, and is the default on FreeBSD).
#
# From: cvsup2html.awk,v 1.3 1999/02/22 11:04:40 dom Exp
#

BEGIN	{
	url="http://www.freebsd.org/cgi/cvsweb.cgi"
	print "<PRE>"
}
$1 == "Edit" || $1 == "Checkout" {
	urlfile=$2
	sub(/,v$/, "", urlfile)
	link = "<a href=\"" url "/" urlfile "\">" $2 "</a>"
	print " " $1, link
	next
}
{
	print
}
END	{
	print "</PRE>"
}
