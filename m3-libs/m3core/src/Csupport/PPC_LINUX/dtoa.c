/* Copyright (C) 1992, Digital Equipment Corporation                         */
/* All rights reserved.                                                      */
/* See the file COPYRIGHT for a full description.                            */

/* Last modified on Mon Oct 12 13:50:54 PDT 1992 by muller                   */


#ifndef KR_headers
#define KR_headers
#endif

#ifndef IEEE_MC68k
#define IEEE_MC68k
#endif

#ifdef  IEEE_8087
#undef  IEEE_8087
#endif

/* FIXME: why? */
#if 0
#define strtod m3_strtod
#endif

#include "dtoa.h"
