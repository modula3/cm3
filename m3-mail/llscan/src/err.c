/* Copyright (C) 1994, Digital Equipment Corporation           */
/* All rights reserved.                                        */
/* See the file COPYRIGHT for a full description.              */
/***************************************************************/
/* File: err.c                                                 */
/* Last modified on Fri Apr  7 12:05:17 PDT 1995 by kalsow     */
/*      modified on Thu May 12 14:37:29 PDT 1994 by wobber     */
/*      modified on Fri May 17 13:09:27 1991 by hisgen         */
/*      modified on Wed Sep  7 10:27:43 PDT 1988 by roberts    */
/* ----------------------------------------------------------- */
/*     The err package implements a simple error routine.      */
/***************************************************************/

#if defined(__INTERIX) && !defined(_REENTRANT)
#define _REENTRANT
#endif

#include <stdio.h>
#include <stdarg.h>
#include <errno.h>
#include "SRCstdlib.h"
#include "err.h"

#define MAXERRMSG 1024

/***************************************************************/
/* Global variables                                            */
/***************************************************************/

static char errfmt[MAXERRMSG];
static char errbuf[MAXERRMSG];

/***************************************************************/
/* Local function declarations                                 */
/***************************************************************/

static void preprocess(/* msg */);



/***************************************************************/
/* error(msg, args);                                           */
/*                                                             */
/*     The error routine is used for error reporting througout */
/* the library packages and in most applications.  To the      */
/* user, error looks like printf and simply prints a message   */
/* to stderr of the form                                       */
/*                                                             */
/*                 Error: <error-message>\n                    */
/*                                                             */
/* where <error-message> is composed of the string msg after   */
/* substitution of any % parameters in the standard printf     */
/* style.  After printing the message, error calls exit with   */
/* status 1.                                                   */
/***************************************************************/

void error(string msg, ...)
{
    int res;
    va_list args;

    va_start(args, msg);
    preprocess(msg);
    res = (int) sprintf(errbuf, errfmt, args);
    if (res < 0) abort();
    va_end(args);

    fprintf(stderr, "Error: %s\n", errbuf);
    exit(1);
}



/***************************************************************/
/* preprocess(msg)                                             */
/*                                                             */
/*     Copies msg into errfmt, replacing any legal occurrences */
/* of %M with the current error message.                       */
/***************************************************************/

static void preprocess(msg)
string msg;
{
    char *src, *dst;
    const char *cp;
    char c;

    src = msg;
    dst = errfmt;
    while ((c = *src++) && dst < &errfmt[MAXERRMSG-4]) {
	switch (c) {
	    case '%':
		if ((c = *src++) == 'M') {
		    if (errno >= sys_nerr)
			cp = "Unknown error";
		    else
			cp = sys_errlist[errno];
		    while ((c = *cp++) && dst < &errfmt[MAXERRMSG-4])
			 *dst++ = c;
		} else {
		    *dst++ = '%';
		    *dst++ = c;
		}
		break;
	    case '\\':
		*dst++ = c;
		*dst++ = *src++;
		break;
	    default:
		*dst++ = c;
	}
    }
    if (dst == &errfmt[MAXERRMSG-4])
	strcpy(dst, "...");
    else
	*dst = '\0';
}
