/* CYGNUS LOCAL mpw (entire file) */
/* Macintosh MPW host definitions for GNU C Compiler.
   Copyright (C) 1993, 1994, 1995 Free Software Foundation, Inc.

This file is part of GNU CC.

GNU CC is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2, or (at your option)
any later version.

GNU CC is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GNU CC; see the file COPYING.  If not, write to
the Free Software Foundation, 59 Temple Place - Suite 330,
Boston, MA 02111-1307, USA.  */

/* The definitions here apply to both 68K and PPC Macs running MPW. */

#include <Types.h>
#include <Errors.h>
#include <stdio.h>

/* MPW's stdio.h defines size_t, potentially conflicting with stddef.h,
   if we don't suppress it by defining __SIZE_T.  A fixed include
   might help here, but fixincludes doesn't yet work for MPW. */
#ifdef MPW
#define __SIZE_T
#endif

/* Include general GNU-for-MPW definitions. */

#include <mpw.h>

/* They are included after the specific processor's tm.h, so any
   definitions that should affect tm.h must appear in the processor-
   specific xm-mpw.h.  */

#define bzero(a,b) memset(a,0,b)
#define bcopy(a,b,c) memcpy(b,a,c)
#define bcmp(a,b,c) memcmp(a,b,c)

#define HAVE_PUTENV

#define HAVE_STRERROR

#define NO_STAB_H

/* This tweaks the preprocessor to like double slash as a comment. */

#define CPLUSPLUS

#ifndef __GNUC__
#define ONLY_INT_FIELDS
#endif

/* Arguments to use with `exit'.  */
#define SUCCESS_EXIT_CODE 0
#define FATAL_EXIT_CODE 33

/* If compiled with GNU C, use the built-in alloca */
#ifdef __GNUC__
/* Use an arg in this macro because that's what some other
   system does--let's avoid conflict.  */
#define alloca(x) __builtin_alloca(x)
#else
/* ...but most Mac compilers don't know about alloca. */
#define USE_C_ALLOCA
extern void *alloca ();
#endif

#define index strchr
#define rindex strrchr

#define SHORT_ENUM_BUG

/* MPW uses a comma to separate the directories in a search path. */

#define PATH_SEPARATOR ','

/* Display file and line info as a clickable command.  */

#define PRINT_FILE_AND_LINE(FILE,LINE) \
  fprintf (stderr, "File \"%s\"; Line %d\t# ", (FILE), (LINE))
