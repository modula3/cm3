/* Copyright (C) 1994, Digital Equipment Corporation           */
/* All rights reserved.                                        */
/* See the file COPYRIGHT for a full description.              */
/*                                                             */
/***************************************************************/
/* File: strlib.h                                              */
/* Last modified on Thu Jun  2 15:40:17 PDT 1994 by kalsow     */
/*      modified on Fri May 17 13:04:06 1991 by hisgen         */
/*      modified on Thu Jun 19 12:59:28 1986 by roberts        */
/* ----------------------------------------------------------- */
/*     The strlib.h file defines the interface for several     */
/* routines that use dynamically-allocated string storage.     */
/* Since these routines tend to fill up memory, this package   */
/* is not suitable for use in applications which will run for  */
/* extended periods or which require tight memory control.     */
/*                                                             */
/* Contents:                                                   */
/*                                                             */
/*       getmem(nbytes)      malloc with error checking        */
/*       scopy(source)       returns a copy of source          */
/*       sconc(s1,s2)        concatenates its arguments        */
/*       substr(s, p1, p2)   returns substring from p1-p2      */
/*       findstr(text, pat)  finds index of pat in text        */
/*       format(str, ...)    dynamic version of sprintf        */
/***************************************************************/

#ifndef _strlib_h
#define _strlib_h
#ifndef  _SRCstdlib_h
#include <SRCstdlib.h>
#endif

char *getmem(/* nbytes */);
char *scopy(/* s */);
char *sconc(/* s1, s2 */);
char *substr(/* s, p1, p2 */);
int findstr(/* text, pat */);

char *format(/* control, ... */);

#endif
