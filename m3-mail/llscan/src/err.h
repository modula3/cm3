/* Copyright (C) 1994, Digital Equipment Corporation           */
/* All rights reserved.                                        */
/* See the file COPYRIGHT for a full description.              */
/***************************************************************/
/* File: err.h                                                 */
/* Last modified on Fri Apr  7 11:55:53 PDT 1995 by kalsow     */
/*      modified on Fri May  6 14:14:32 PDT 1994 by wobber     */
/*      modified on Fri May 17 13:04:05 1991 by hisgen         */
/*      modified on Mon Feb  2 09:14:54 1987 by roberts        */
/* ----------------------------------------------------------- */
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
/* style, plus the special %M specification for the text of    */
/* the system error specified in errno.  After printing the    */
/* message, error calls exit with status 1.                    */
/*                                                             */
/***************************************************************/



#ifndef _err_h
#define _err_h

void error(/* msg, ... */);

#endif
