/* Copyright (C) 1992, Digital Equipment Corporation        */
/* All rights reserved.                                     */
/* See the file COPYRIGHT for a full description.           */
/*                                                          */
/* Last modified on Thu Feb  1 09:36:52 PST 1996 by heydon  */
/*      modified on Tue Jan 10 15:48:28 PST 1995 by kalsow  */
/*      modified on Tue Feb 11 15:18:40 PST 1992 by muller  */

/*
This old code has a few problems.

  gcc-4.2 -Wstrict-overflow=4 -c old_divmod.c -O2
old_divmod.c: In function ‘m3_div’:
old_divmod.c:35: warning: assuming signed overflow does not occur when distributing negation across division
old_divmod.c:36: warning: assuming signed overflow does not occur when distributing negation across division
old_divmod.c: In function ‘m3_div64’:
old_divmod.c:58: warning: assuming signed overflow does not occur when distributing negation across division
old_divmod.c:59: warning: assuming signed overflow does not occur when distributing negation across division

  INT_MIN mod -1 generates an exception even though it should just return 0
    -O3 makes it work.
    INT_MIN div -1 also generates an exception, but that is correct.
    This is arguable, since div and mod are defined in terms of each other,
    if one fails, maybe the other should too?
*/

#include <stddef.h>
#if __INITIAL_POINTER_SIZE == 64
typedef __int64 INTEGER;
typedef unsigned __int64 WORD_T;
#else
typedef ptrdiff_t INTEGER;
typedef size_t WORD_T;
#endif
#if defined(_MSC_VER) || defined(__DECC)
typedef   signed __int64  INT64;
typedef unsigned __int64 UINT64;
#else
typedef   signed long long  INT64;
typedef unsigned long long UINT64;
#endif

#ifdef __cplusplus
extern "C"
{
#endif

INTEGER m3_div(INTEGER b, INTEGER a)
{
  typedef  INTEGER ST; /* signed type */
  register ST c;
  if ((a == 0) && (b != 0))  {  c = 0;
  } else if (a > 0)  {  c = (b >= 0) ? (a) / (b) : -1 - (a-1) / (-b);
  } else /* a < 0 */ {  c = (b >= 0) ? -1 - (-1-a) / (b) : (-a) / (-b);
  }
  return c;
}

INTEGER m3_mod(INTEGER b, INTEGER a)
{
  typedef  INTEGER ST; /* signed type */
  register ST c;
  if ((a == 0) && (b != 0)) {  c = 0;
  } else if (a > 0)  {  c = (b >= 0) ? a % b : b + 1 + (a-1) % (-b);
  } else /* a < 0 */ {  c = (b >= 0) ? b - 1 - (-1-a) % (b) : - ((-a) % (-b));
  }
  return c;
}

INT64 m3_div64(INT64 b, INT64 a)
{
  typedef  INT64 ST; /* signed type */
  typedef UINT64 UT; /* unsigned type */
  register ST c;
  if ((a == 0) && (b != 0))  {  c = 0;
  } else if (a > 0)  {  c = (b >= 0) ? (a) / (b) : -1 - (a-1) / (-b);
  } else /* a < 0 */ {  c = (b >= 0) ? -1 - (-1-a) / (b) : (-a) / (-b);
  }
  return c;
}

INT64 m3_mod64(INT64 b, INT64 a)
{
  typedef  INT64 ST; /* signed type */
  register ST c;
  if ((a == 0) && (b != 0)) {  c = 0;
  } else if (a > 0)  {  c = (b >= 0) ? a % b : b + 1 + (a-1) % (-b);
  } else /* a < 0 */ {  c = (b >= 0) ? b - 1 - (-1-a) % (b) : - ((-a) % (-b));
  }
  return c;
}

#include <limits.h>
#include <stdio.h>

int main()
{
  printf("%d\n", m3_mod(-1, INT_MIN));
}

#ifdef __cplusplus
} /* extern "C" */
#endif
