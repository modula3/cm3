/* CYGNUS LOCAL mpw (entire file) */
/* M68K-based Macintosh MPW host definitions for GNU C Compiler.
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

#ifndef MPW
#define MPW
#endif

#include <m68k/xm-m68k.h>

/* Use angle brackets so that the common xm-mpw.h is found.  */

#include <xm-mpw.h>

/* This stops the MPW C compiler from complaining invalidly.  */

#ifdef MPW_C
#define __PTR_TO_INT(P) ((int)(P))
#define __INT_TO_PTR(P) ((char *)(P))
#endif /* MPW_C */
