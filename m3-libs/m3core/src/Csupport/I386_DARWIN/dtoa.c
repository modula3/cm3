/* Copyright according to COPYRIGHT-CMASS. */
/* FIXME: copied from FreeBSD3 target. Probably needs to be changed. */

#ifndef KR_headers
#define KR_headers
#endif

#ifndef IEEE_8087
#define IEEE_8087
#endif

#include "dtoa.h"

/* Apparently libc defines both "__dtoa" and "dtoa".  ???  */
#if 0
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
#endif
