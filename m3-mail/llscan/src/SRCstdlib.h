/* Copyright (C) 1994, Digital Equipment Corporation           */
/* All rights reserved.                                        */
/* See the file COPYRIGHT for a full description.              */
/*                                                             */
/***************************************************************/
/* File: SRCstdlib.h.  Original name was "stdlib.h" but we     */
/* changed it in May 1991 to avoid naming conflicts with the   */
/* standard C world, especially on MIPS.		       */
/*       						       */
/* Last modified on Thu Jun  2 15:39:45 PDT 1994 by kalsow     */
/*      modified on Fri May 17 12:56:00 1991 by hisgen         */
/*      modified on Mon Feb  2 09:16:22 1987 by roberts        */
/* ----------------------------------------------------------- */
/*     This file contains a small subset of the definitions    */
/* used in the C library package designed by Eric Roberts      */
/* and is intended for explicit inclusion in packages which    */
/* do not need the full generality of the extended C library.  */
/* Most of the definitions are simply additional primitive     */
/* types (such as string and bool) designed for readability.   */
/***************************************************************/

#ifndef _SRCstdlib_h
#define _SRCstdlib_h

#define TRUE  1
#define FALSE 0

typedef int bool;
typedef char *string;
typedef FILE *stream;
typedef int (*proc)();

#include <strings.h>
#define streq(s,t) (!strcmp(s,t))

char *malloc();
string getenv();

extern void error();

#endif
