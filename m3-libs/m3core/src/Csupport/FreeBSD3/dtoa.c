/* Copyright (C) 1992, Digital Equipment Corporation          */
/* All rights reserved.                                       */
/* See the file COPYRIGHT for a full description.             */

/* Last modified on Wed Apr  5 08:44:12 PDT 1995 by kalsow    */
/*      modified on Mon Oct 12 13:50:54 PDT 1992 by muller    */

#ifndef KR_headers
#define KR_headers
#endif

#ifndef IEEE_8087
#define IEEE_8087
#endif

#include "dtoa.h"

/* Apparently libc defines both "__dtoa" and "dtoa".  ???  */

char * __dtoa       
#ifdef KR_headers
        (d, mode, ndigits, decpt, sign, rve)
        double d; int mode, ndigits, *decpt, *sign; char **rve;
#else 
        (double d, int mode, int ndigits, int *decpt, int *sign, char **rve)
#endif
{
  return dtoa(d, mode, ndigits, decpt, sign, rve);
}
