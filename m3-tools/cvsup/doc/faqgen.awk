#! /usr/bin/awk -f
#
# Copyright 1998-2003 John D. Polstra
# All rights reserved.
# $Id$

BEGIN {
    secH = "H3";
}

# A comment line beginning with "!#"
/^!#/ {
    next;
}

# Id line
/^![Ii]/ {
    id = gensub(/^[^     ]+[     ]+/, "", 1);
    next;
}

# Section
/^![Ss]/ {
    inQuestion = 0;
    ++secCount;
    secName[secCount] = gensub(/^[^ 	]+[ 	]+/, "", 1);
    secFirst[secCount] = qCount + 1;
    next;
}

# Question (normal or new)
/^![QqNn]/ {
    inQuestion = 1;
    ++qCount;
    if (NF >= 2)
	labels[qCount] = $2;
    else
	labels[qCount] = qCount;
    if ($1 ~ /![Nn]/)
	new[qCount] = 1;
    next;
}

# A line of the question
inQuestion && /[^ 	]/ {
    if (qCount in questions)
	questions[qCount] = questions[qCount] "\n" $0;
    else
	questions[qCount] = $0;
    next;
}

# Blank line terminating question
inQuestion && /^[ 	]*$/ {
    inQuestion = 0;
    next;
}

# A line of the answer
{
    answers[qCount] = answers[qCount] $0 "\n";
    next;
}

END {
    secFirst[secCount+1] = qCount + 1;

    # Emit the table of contents.
    for (sec = 1;  sec <= secCount;  sec++) {
	printf "<%s>%s</%s>\n", secH, secName[sec], secH;
	printf "<OL START=%d>\n", secFirst[sec];
	for (q = secFirst[sec];  q < secFirst[sec+1];  q++) {
	    printf "<LI><A HREF=\"#%s\">%s</A>\n", labels[q], questions[q];
	    if (q in new) {
		printf "<img src=\"images/yelnew.gif\" width=26";
		printf " height=12 alt=\"NEW\">\n";
	    }
	}
	printf "</OL>\n";
    }

    # Emit a separator.
    printf "<HR>\n"

    # Emit the body of the FAQ.
    for (sec = 1;  sec <= secCount;  sec++) {
	printf "<%s>%s</%s>\n", secH, secName[sec], secH;
	printf "<OL START=%d>\n", secFirst[sec];
	for (q = secFirst[sec];  q < secFirst[sec+1];  q++) {
	    printf "<LI><A NAME=\"%s\"><EM>%s</EM></A>\n", labels[q],\
		questions[q];
	    printf "<P>\n%s<P>\n", answers[q];
	}
	printf "</OL>\n";
    }

    # Emit the horizontal rule.
    printf "<HR>\n";

    # Emit the Id line.
    if (id) {
	printf "<EM>%s</EM><BR>\n", id
    }
}
