/* Copyright (C) 1994, Digital Equipment Corporation           */
/* All rights reserved.                                        */
/* See the file COPYRIGHT for a full description.              */
/*                                                             */
/***************************************************************/
/* File: strlib.c                                              */
/* Last modified on Wed Jan 25 10:01:19 PST 1995 by kalsow     */
/*      modified on Thu May 12 14:34:19 PDT 1994 by wobber     */
/*      modified on Fri May 17 13:09:28 1991 by hisgen         */
/*      modified on Thu Jun 19 12:59:27 1986 by roberts        */
/* ----------------------------------------------------------- */
/*     The strlib package contains the implementations for     */
/* several routines that use dynamically-allocated string      */
/* storage.  Since these routines tend to fill up memory,      */
/* this package is not suitable for use in applications        */
/* which will run for extended periods or which require        */
/* tight memory control.  Nonetheless, this package does       */
/* provide an easy-to-use interface which is applicable        */
/* to a wide range of programs.                                */
/*                                                             */
/*     The following routines are defined by strlib:           */
/*                                                             */
/*       getmem(nbytes)      malloc with error checking        */
/*       scopy(source)       returns a copy of source          */
/*       sconc(s1,s2)        concatenates its arguments        */
/*       substr(s, p1, p2)   returns substring from p1-p2      */
/*       findstr(text, pat)  finds index of pat in text        */
/*       format(str, ...)    dynamic version of sprintf        */
/***************************************************************/

#include <stdio.h>
#include <stdarg.h>
#include "SRCstdlib.h"
#include "strlib.h"

/***************************************************************/
/* Package variables and definitions                           */
/***************************************************************/

#define MAXFORMAT 1024

static char fmtbuf[MAXFORMAT];



/***************************************************************/
/* ptr = (type) getmem(nbytes);                                */
/*                                                             */
/*     This routine is exactly like malloc except that (1) it  */
/* checks for no memory errors, and (2) it is defined to take  */
/* an integer rather than an unsigned to keep lint happier.    */
/***************************************************************/

char *getmem(nbytes)
int nbytes;
{
    char *result;

    result = malloc((unsigned) nbytes);
    if (result == NULL) error("No memory available [getmem]");
    return (result);
}



/***************************************************************/
/* s = scopy(t);                                               */
/*                                                             */
/*     Copies the string t into dynamically-allocated storage. */
/***************************************************************/

string scopy(s)
string s;
{
    string result;

    result = (string) getmem(strlen(s)+1);
    strcpy(result, s);
    return (result);
}



/***************************************************************/
/* s = sconc(s1, s2);                                          */
/*                                                             */
/*     Concatenates two strings and returns the result in      */
/* dynamically-allocated storage.                              */
/***************************************************************/

string sconc(s1, s2)
string s1, s2;
{
    int l;
    string result;

    result = (string) getmem((l = strlen(s1)) + strlen(s2) + 1);
    strcpy(result, s1);
    strcpy(result+l, s2);
    return (result);
}



/***************************************************************/
/* s = substr(s, p1, p2);                                      */
/*                                                             */
/*     Returns the substring of s extending from the integer   */
/* indices p1 and p2 (inclusive).  The following edge cases    */
/* apply:                                                      */
/*                                                             */
/*      if p1 < 0 then p1 <- 0;                                */
/*      if p2 > strlen(s) then p2 <- strlen(s);                */
/*      if p1 > p2 then return "";                             */
/***************************************************************/

string substr(s, p1, p2)
string s;
int p1, p2;
{
    int l, i;
    string result;

    l = strlen(s);
    if (p1 < 0) p1 = 0;
    if (p2 >= l) p2 = l - 1;
    if ((l = p2 - p1 + 1) <= 0) return ("");
    result = (string) getmem(l + 1);
    for (i = 0; i < l; i++)
	result[i] = s[p1+i];
    result[l] = 0;
    return (result);
}



/***************************************************************/
/* p = findstr(text, pat);                                     */
/*                                                             */
/*     Searches for the string pat in text and returns the     */
/* first index at which it appears, or -1 if no match is       */
/* found.  This function executes a simple compare and         */
/* advance algorithm, and is inappropriate if text contains    */
/* a very long string.                                         */
/***************************************************************/

int findstr(text, pat)
string text, pat;
{
    string s;
    int nch;

    nch = strlen(pat);
    for (s = text; *s; s++)
	if (strncmp(s, pat, nch) == 0)
	    return (s - text);
    return (-1);
}



string format(string control, ...)
{
    int res;
    va_list args;

    va_start(args, control);
    res = (int) sprintf(fmtbuf, control, args);
    if (res < 0) abort();
    va_end(args);

    if (strlen(fmtbuf) >= MAXFORMAT) error("Buffer space exceeded [format]");
    return (scopy(fmtbuf));
}
