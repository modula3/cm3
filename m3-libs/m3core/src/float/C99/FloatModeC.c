/* 
	 C glue for C99 implementation of Modula-3 FloatMode 
 
   Copyright (c) 2011 Generation Capital, Ltd.  All rights reserved.

   Permission to use, copy, modify, and distribute this software
   and its documentation for any purpose and without fee is hereby
   granted, provided that the above copyright notice appear in all
   copies.  Generation Capital, Ltd. makes no representations
   about the suitability of this software for any purpose. It is
   provided "as is" without express or implied warranty.

   Author : Mika Nystrom <mika@alum.mit.edu> 
   April 2011

*/

#include <fenv.h>

#ifdef __FreeBSD__
# pragma STDC FENV_ACCESS ON /* its sloppy to just have this here */
#endif

#ifdef __cplusplus
extern "C" {
#endif

int
FloatModeC__get_FE_TONEAREST(void) { return FE_TONEAREST; }

int
FloatModeC__get_FE_DOWNWARD(void) { return FE_DOWNWARD; }

int
FloatModeC__get_FE_UPWARD(void) { return FE_UPWARD; }

int
FloatModeC__get_FE_TOWARDZERO(void) { return FE_TOWARDZERO; }



int
FloatModeC__get_FE_DIVBYZERO(void) { return FE_DIVBYZERO; }

int
FloatModeC__get_FE_INEXACT(void) { return FE_INEXACT; }

int
FloatModeC__get_FE_INVALID(void) { return FE_INVALID; }

int
FloatModeC__get_FE_OVERFLOW(void) { return FE_OVERFLOW; }

int
FloatModeC__get_FE_UNDERFLOW(void) { return FE_UNDERFLOW; }

/**********************************************************************/

/* the following routines are normally inlined by the C compiler, but
	 we cannot do that here so we have to wrap them. */

int 
FloatModeC__fegetround(void) { return fegetround(); }

void
FloatModeC__fesetround(int round) { fesetround(round); }

void
FloatModeC__feclearexcept(int excepts) { feclearexcept(excepts); }

void
FloatModeC__feraiseexcept(int excepts) { feraiseexcept(excepts); }

int
FloatModeC__fetestexcept(int excepts) { return fetestexcept(excepts); }

#ifdef __cplusplus
} /* extern "C" */
#endif
