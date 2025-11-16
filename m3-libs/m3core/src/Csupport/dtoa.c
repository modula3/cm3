#ifndef INCLUDED_M3CORE_H
#include "m3core.h"
#endif

/****************************************************************

The author of this software is David M. Gay.

Copyright (C) 1998-2000 by Lucent Technologies
All Rights Reserved

Permission to use, copy, modify, and distribute this software and
its documentation for any purpose and without fee is hereby
granted, provided that the above copyright notice appear in all
copies and that both that the copyright notice and this
permission notice and warranty disclaimer appear in supporting
documentation, and that the name of Lucent or any of its entities
not be used in advertising or publicity pertaining to
distribution of the software without specific, written prior
permission.

LUCENT DISCLAIMS ALL WARRANTIES WITH REGARD TO THIS SOFTWARE,
INCLUDING ALL IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS.
IN NO EVENT SHALL LUCENT OR ANY OF ITS ENTITIES BE LIABLE FOR ANY
SPECIAL, INDIRECT OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER
IN AN ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION,
ARISING OUT OF OR IN CONNECTION WITH THE USE OR PERFORMANCE OF
THIS SOFTWARE.

****************************************************************/

/* This is a variation on dtoa.c that converts arbitary binary
   floating-point formats to and from decimal notation.  It uses
   double-precision arithmetic internally, so there are still
   various #ifdefs that adapt the calculations to the native
   double-precision arithmetic (any of IEEE, VAX D_floating,
   or IBM mainframe arithmetic).

   Please send bug reports to David M. Gay (dmg at acm dot org,
   with " at " changed at "@" and " dot " changed to ".").
 */

/* On a machine with IEEE extended-precision registers, it is
 * necessary to specify double-precision (53-bit) rounding precision
 * before invoking strtod or dtoa.  If the machine uses (the equivalent
 * of) Intel 80x87 arithmetic, the call
 *	_control87(PC_53, MCW_PC);
 * does this with many compilers.  Whether this or another call is
 * appropriate depends on the compiler; for this to work, it may be
 * necessary to #include "float.h" or another system-dependent header
 * file.
 */

/* strtod for IEEE-, VAX-, and IBM-arithmetic machines.
 *
 * This strtod returns a nearest machine number to the input decimal
 * string (or sets errno to ERANGE).  With IEEE arithmetic, ties are
 * broken by the IEEE round-even rule.  Otherwise ties are broken by
 * biased rounding (add half and chop).
 *
 * Inspired loosely by William D. Clinger's paper "How to Read Floating
 * Point Numbers Accurately" [Proc. ACM SIGPLAN '90, pp. 112-126].
 *
 * Modifications:
 *
 *	1. We only require IEEE, IBM, or VAX double-precision
 *		arithmetic (not IEEE double-extended).
 *	2. We get by with floating-point arithmetic in a case that
 *		Clinger missed -- when we're computing d * 10^n
 *		for a small integer d and the integer n is not too
 *		much larger than 22 (the maximum integer k for which
 *		we can represent 10^k exactly), we may be able to
 *		compute (d*10^k) * 10^(e-k) with just one roundoff.
 *	3. Rather than a bit-at-a-time adjustment of the binary
 *		result in the hard case, we use floating-point
 *		arithmetic to determine the adjustment to within
 *		one bit; only in really hard cases do we need to
 *		compute a second residual.
 *	4. Because of 3., we don't need a large table of powers of 10
 *		for ten-to-e (just some small tables, e.g. of 10^k
 *		for 0 <= k <= 22).
 */

/*
 * #define IEEE_8087 for IEEE-arithmetic machines where the least
 *	significant byte has the lowest address.
 * #define IEEE_MC68k for IEEE-arithmetic machines where the most
 *	significant byte has the lowest address.
 * #define Long int on machines with 32-bit ints and 64-bit longs.
 * #define Sudden_Underflow for IEEE-format machines without gradual
 *	underflow (i.e., that flush to zero on underflow).
 * #define IBM for IBM mainframe-style floating-point arithmetic.
 * #define VAX for VAX-style floating-point arithmetic (D_floating).
 * #define No_leftright to omit left-right logic in fast floating-point
 *	computation of dtoa.
 * #define Check_FLT_ROUNDS if FLT_ROUNDS can assume the values 2 or 3.
 * #define RND_PRODQUOT to use rnd_prod and rnd_quot (assembly routines
 *	that use extended-precision instructions to compute rounded
 *	products and quotients) with IBM.
 * #define ROUND_BIASED for IEEE-format with biased rounding.
 * #define Inaccurate_Divide for IEEE-format with correctly rounded
 *	products but inaccurate quotients, e.g., for Intel i860.
 * #define NO_LONG_LONG on machines that do not have a "long long"
 *	integer type (of >= 64 bits).  On such machines, you can
 *	#define Just_16 to store 16 bits per 32-bit Long when doing
 *	high-precision integer arithmetic.  Whether this speeds things
 *	up or slows things down depends on the machine and the number
 *	being converted.  If long long is available and the name is
 *	something other than "long long", #define Llong to be the name,
 *	and if "unsigned Llong" does not work as an unsigned version of
 *	Llong, #define #ULLong to be the corresponding unsigned type.
 * #define KR_headers for old-style C function headers.
 * #define Bad_float_h if your system lacks a float.h or if it does not
 *	define some or all of DBL_DIG, DBL_MAX_10_EXP, DBL_MAX_EXP,
 *	FLT_RADIX, FLT_ROUNDS, and DBL_MAX.
 * #define MALLOC your_malloc, where your_malloc(n) acts like malloc(n)
 *	if memory is available and otherwise does something you deem
 *	appropriate.  If MALLOC is undefined, malloc will be invoked
 *	directly -- and assumed always to succeed.
 * #define Omit_Private_Memory to omit logic (added Jan. 1998) for making
 *	memory allocations from a private pool of memory when possible.
 *	When used, the private pool is PRIVATE_MEM bytes long:  2304 bytes,
 *	unless #defined to be a different length.  This default length
 *	suffices to get rid of MALLOC calls except for unusual cases,
 *	such as decimal-to-binary conversion of a very long string of
 *	digits.  When converting IEEE double precision values, the
 *	longest string gdtoa can return is about 751 bytes long.  For
 *	conversions by strtod of strings of 800 digits and all gdtoa
 *	conversions of IEEE doubles in single-threaded executions with
 *	8-byte pointers, PRIVATE_MEM >= 7400 appears to suffice; with
 *	4-byte pointers, PRIVATE_MEM >= 7112 appears adequate.
 * #define INFNAN_CHECK on IEEE systems to cause strtod to check for
 *	Infinity and NaN (case insensitively).
 *	When INFNAN_CHECK is #defined and No_Hex_NaN is not #defined,
 *	strtodg also accepts (case insensitively) strings of the form
 *	NaN(x), where x is a string of hexadecimal digits (optionally
 *	preceded by 0x or 0X) and spaces; if there is only one string
 *	of hexadecimal digits, it is taken for the fraction bits of the
 *	resulting NaN; if there are two or more strings of hexadecimal
 *	digits, each string is assigned to the next available sequence
 *	of 32-bit words of fractions bits (starting with the most
 *	significant), right-aligned in each sequence.
 *	Unless GDTOA_NON_PEDANTIC_NANCHECK is #defined, input "NaN(...)"
 *	is consumed even when ... has the wrong form (in which case the
 *	"(...)" is consumed but ignored).
 * #define MULTIPLE_THREADS if the system offers preemptively scheduled
 *	multiple threads.  In this case, you must provide (or suitably
 *	#define) two locks, acquired by ACQUIRE_DTOA_LOCK(n) and freed
 *	by FREE_DTOA_LOCK(n) for n = 0 or 1.  (The second lock, accessed
 *	in pow5mult, ensures lazy evaluation of only one copy of high
 *	powers of 5; omitting this lock would introduce a small
 *	probability of wasting memory, but would otherwise be harmless.)
 *	You must also invoke freedtoa(s) to free the value s returned by
 *	dtoa.  You may do so whether or not MULTIPLE_THREADS is #defined.
 * #define IMPRECISE_INEXACT if you do not care about the setting of
 *	the STRTOG_Inexact bits in the special case of doing IEEE double
 *	precision conversions (which could also be done by the strtog in
 *	dtoa.c).
 * #define NO_HEX_FP to disable recognition of C9x's hexadecimal
 *	floating-point constants.
 * #define -DNO_ERRNO to suppress setting errno (in strtod.c and
 *	strtodg.c).
 * #define NO_STRING_H to use private versions of memcpy.
 *	On some K&R systems, it may also be necessary to
 *	#define DECLARE_SIZE_T in this case.
 * #define YES_ALIAS to permit aliasing certain double values with
 *	arrays of ULongs.  This leads to slightly better code with
 *	some compilers and was always used prior to 19990916, but it
 *	is not strictly legal and can cause trouble with aggressively
 *	optimizing compilers (e.g., gcc 2.95.1 under -O2).
 * #define USE_LOCALE to use the current locale's decimal_point value.
 */

#ifndef GDTOAIMP_H_INCLUDED
#define GDTOAIMP_H_INCLUDED

//#include "gdtoa.h"

#ifndef GDTOA_H_INCLUDED
#define GDTOA_H_INCLUDED

//#include "arith.h"
#define IEEE_8087
#define Arith_Kind_ASL 1
#define Long int
#define Intcast (int)(long)
#define Double_Align
#define X64_bit_pointers

#ifndef Long
#define Long long
#endif
#ifndef ULong
typedef unsigned Long ULong;
#endif
#ifndef UShort
typedef unsigned short UShort;
#endif

#ifndef ANSI
#ifdef KR_headers
#define ANSI(x) ()
#define Void /*nothing*/
#else
#define ANSI(x) x
#define Void void
#endif
#endif /* ANSI */

#ifndef CONST
#ifdef KR_headers
#define CONST /* blank */
#else
#define CONST const
#endif
#endif /* CONST */

enum { /* return values from strtodg */
       STRTOG_Zero = 0,
       STRTOG_Normal = 1,
       STRTOG_Denormal = 2,
       STRTOG_Infinite = 3,
       STRTOG_NaN = 4,
       STRTOG_NaNbits = 5,
       STRTOG_NoNumber = 6,
       STRTOG_Retmask = 7,

       /* The following may be or-ed into one of the above values. */

       STRTOG_Neg = 0x08,
       STRTOG_Inexlo = 0x10,
       STRTOG_Inexhi = 0x20,
       STRTOG_Inexact = 0x30,
       STRTOG_Underflow = 0x40,
       STRTOG_Overflow = 0x80
};

typedef struct FPI {
  int nbits;
  int emin;
  int emax;
  int rounding;
  int sudden_underflow;
} FPI;

enum { /* FPI.rounding values: same as FLT_ROUNDS */
       FPI_Round_zero = 0,
       FPI_Round_near = 1,
       FPI_Round_up = 2,
       FPI_Round_down = 3
};

#define MULTIPLE_THREADS
#define ACQUIRE_DTOA_LOCK(n) CConvert__Acquire(n)
#define FREE_DTOA_LOCK(n) CConvert__Release(n)

#ifdef __cplusplus
extern "C" {
#endif


char *m3_ftoa(float f, int mode, int ndigits, int *decpt, int *asign, char **se);
char *m3_dtoa(double d, int mode, int ndigits, int *decpt, int *asign, char **se);
char *m3_qtoa(long double V, int mode, int ndigits, int *decpt, int *asign, char **se);

float m3_strtof(CONST char *s, char **sp);
//int m3_strtof(CONST char *s, char **sp, int rounding, float *f);
double m3_strtod(CONST char *s, char **sp);
//int m3_strtod(CONST char *s, char **sp, int rounding, double *d);
int m3_strtoq(CONST char *s, char **sp, int rounding, void *L);
//long double m3_strtoq(CONST char *s, char **sp);

void m3_freedtoa(char *s);

void __cdecl CConvert__Acquire(INTEGER);
void __cdecl CConvert__Release(INTEGER);

#ifdef __cplusplus
}
#endif

#ifdef _MSC_VER
#pragma warning(disable:4514) /* unused inline function removed */
#pragma warning(disable:4242) /* possible loss of data */
#pragma warning(disable:4244) /* possible loss of data */
#pragma warning(disable:4018) /* signed/unsigned mismatch */
#pragma warning(disable:4365) /* signed/unsigned mismatch */
#pragma warning(disable:4245) /* signed/unsigned mismatch */
#pragma warning(disable:4127) /* conditional expression is constant */
#pragma warning(disable:4706) /* assignment within conditional */
#pragma warning(disable:4701) /* possibly uninitialized local used */
#pragma warning(disable:4711) /* function selected for automatic inline expansion */
#pragma warning(disable:4255) /* () changed to (void) */
#endif

#endif /* GDTOA_H_INCLUDED */

//#include "gd_qnan.h"
#define f_QNAN 0xffc00000
#define d_QNAN0 0x0
#define d_QNAN1 0xfff80000
#define ld_QNAN0 0x0
#define ld_QNAN1 0xc0000000
#define ld_QNAN2 0xffff
#define ld_QNAN3 0x0
#define ldus_QNAN0 0x0
#define ldus_QNAN1 0x0
#define ldus_QNAN2 0x0
#define ldus_QNAN3 0xc000
#define ldus_QNAN4 0xffff

#ifdef DEBUG
#include "stdio.h"
#define Bug(x)                                                                 \
  {                                                                            \
    fprintf(stderr, "%s\n", x);                                                \
    exit(1);                                                                   \
  }
#endif

#include "stdlib.h"
#include "string.h"

#ifdef KR_headers
#define Char char
#else
#define Char void
#endif

#ifdef MALLOC
extern Char *MALLOC ANSI((size_t));
#else
#define MALLOC malloc
#endif

#undef IEEE_Arith
#undef Avoid_Underflow
#ifdef IEEE_MC68k
#define IEEE_Arith
#endif
#ifdef IEEE_8087
#define IEEE_Arith
#endif

#include "errno.h"
#ifdef Bad_float_h

#ifdef IEEE_Arith
#define DBL_DIG 15
#define DBL_MAX_10_EXP 308
#define DBL_MAX_EXP 1024
#define FLT_RADIX 2
#define DBL_MAX 1.7976931348623157e+308
#endif

#ifdef IBM
#define DBL_DIG 16
#define DBL_MAX_10_EXP 75
#define DBL_MAX_EXP 63
#define FLT_RADIX 16
#define DBL_MAX 7.2370055773322621e+75
#endif

#ifdef VAX
#define DBL_DIG 16
#define DBL_MAX_10_EXP 38
#define DBL_MAX_EXP 127
#define FLT_RADIX 2
#define DBL_MAX 1.7014118346046923e+38
#define n_bigtens 2
#endif

#ifndef LONG_MAX
#define LONG_MAX 2147483647
#endif

#else /* ifndef Bad_float_h */
#include "float.h"
#endif /* Bad_float_h */

#ifdef IEEE_Arith
#define Scale_Bit 0x10
#define n_bigtens 5
#endif

#ifdef IBM
#define n_bigtens 3
#endif

#ifdef VAX
#define n_bigtens 2
#endif

#ifndef __MATH_H__
#include "math.h"
#endif

#ifdef __cplusplus
extern "C" {
#endif

#if defined(IEEE_8087) + defined(IEEE_MC68k) + defined(VAX) + defined(IBM) != 1
Exactly one of IEEE_8087, IEEE_MC68k, VAX, or IBM should be defined.
#endif

typedef union {
  double d;
  ULong L[2];
} U;

// runtime endian switch
// on big endian DTOA_0 == 0, DTOA_1 == 1, DTOA_2 == 2, DTOA_3 == 3
// on little endian DTOA_0 == 3, DTOA_1 == 2, DTOA_2 == 1, DTOA_3 == 0
static const union {
    unsigned int i;
    unsigned char b[4];
} LIndices = { 3 + (2 << 8) + (1 << 16) };
#define  _0 (LIndices.b[0])
#define  _1 (LIndices.b[1])
#define  _2 (LIndices.b[2])
#define  _3 (LIndices.b[3])

// runtime endian switch
// on big endian DTOA_0 == 0, DTOA_1 == 1
// on little endian DTOA_0 == 1, DTOA_1 == 0
static const union {
    unsigned short i;
    unsigned char b[2];
} DtoaIndices = { 1 };
#define _00 (DtoaIndices.b[0])
#define _11 (DtoaIndices.b[1])

#ifdef YES_ALIAS
#define dval(x) x
#define word0(x) ((ULong *)&x)[_00]
#define word1(x) ((ULong *)&x)[_11]
#else
#define word0(x) ((U*)&x)->L[_00]
#define word1(x) ((U*)&x)->L[_11]
#define dval(x) ((U*)&x)->d
#endif

/* old delete
#ifdef YES_ALIAS
#define dval(x) x
#ifdef IEEE_8087
#define word0(x) ((ULong *)&x)[1]
#define word1(x) ((ULong *)&x)[0]
#else
#define word0(x) ((ULong *)&x)[0]
#define word1(x) ((ULong *)&x)[1]
#endif
#else // !YES_ALIAS
#ifdef IEEE_8087
#define word0(x) ((U *)&x)->L[1]
#define word1(x) ((U *)&x)->L[0]
#else
#define word0(x) ((U *)&x)->L[0]
#define word1(x) ((U *)&x)->L[1]
#endif
#define dval(x) ((U *)&x)->d
#endif // YES_ALIAS
*/

static const union {
    unsigned long i;
    unsigned int b[2];
} EN = { d_QNAN1 };

#define NAN_WORD0 (EN.b[_00])
#define NAN_WORD1 (EN.b[_11])

void M3DtoaStoreincFunction (ULong** a, ULong hi, ULong lo)
{
    *((*a)++) = (((hi & 0xFFFF) << 16) | (lo & 0xFFFF));
}

#define Storeinc(a, hi, lo) (M3DtoaStoreincFunction(&(a), (hi), (lo)))

/* delete ??
 * The following definition of Storeinc is appropriate for MIPS processors.
 * An alternative that might be better on some machines is
 * #define Storeinc(a,b,c) (*a++ = b << 16 | c & 0xffff)
 */
/*
#if defined(IEEE_8087) + defined(VAX)
#define Storeinc(a, b, c)                                                      \
  (((unsigned short *)a)[1] = (unsigned short)b,                               \
   ((unsigned short *)a)[0] = (unsigned short)c, a++)
#else
#define Storeinc(a, b, c)                                                      \
  (((unsigned short *)a)[0] = (unsigned short)b,                               \
   ((unsigned short *)a)[1] = (unsigned short)c, a++)
#endif
*/

/* #define P DBL_MANT_DIG */
/* Ten_pmax = floor(P*log(2)/log(5)) */
/* Bletch = (highest power of 2 < DBL_MAX_10_EXP) / 16 */
/* Quick_max = floor((P-1)*log(FLT_RADIX)/log(10) - 1) */
/* Int_max = floor(P*log(FLT_RADIX)/log(10) - 1) */

#ifdef IEEE_Arith
#define Exp_shift 20
#define Exp_shift1 20
#define Exp_msk1 0x100000
#define Exp_msk11 0x100000
#define Exp_mask 0x7ff00000
#define P 53
#define Bias 1023
#define Emin (-1022)
#define Exp_1 0x3ff00000
#define Exp_11 0x3ff00000
#define Ebits 11
#define Frac_mask 0xfffff
#define Frac_mask1 0xfffff
#define Ten_pmax 22
#define Bletch 0x10
#define Bndry_mask 0xfffff
#define Bndry_mask1 0xfffff
#define LSB 1
#define Sign_bit 0x80000000
#define Log2P 1
#define Tiny0 0
#define Tiny1 1
#define Quick_max 14
#define Int_max 14

#ifndef Flt_Rounds
#ifdef FLT_ROUNDS
#define Flt_Rounds FLT_ROUNDS
#else
#define Flt_Rounds 1
#endif
#endif /*Flt_Rounds*/

#else /* ifndef IEEE_Arith */
#undef Sudden_Underflow
#define Sudden_Underflow
#ifdef IBM
#undef Flt_Rounds
#define Flt_Rounds 0
#define Exp_shift 24
#define Exp_shift1 24
#define Exp_msk1 0x1000000
#define Exp_msk11 0x1000000
#define Exp_mask 0x7f000000
#define P 14
#define Bias 65
#define Exp_1 0x41000000
#define Exp_11 0x41000000
#define Ebits 8 /* exponent has 7 bits, but 8 is the right value in b2d */
#define Frac_mask 0xffffff
#define Frac_mask1 0xffffff
#define Bletch 4
#define Ten_pmax 22
#define Bndry_mask 0xefffff
#define Bndry_mask1 0xffffff
#define LSB 1
#define Sign_bit 0x80000000
#define Log2P 4
#define Tiny0 0x100000
#define Tiny1 0
#define Quick_max 14
#define Int_max 15
#else /* VAX */
#undef Flt_Rounds
#define Flt_Rounds 1
#define Exp_shift 23
#define Exp_shift1 7
#define Exp_msk1 0x80
#define Exp_msk11 0x800000
#define Exp_mask 0x7f80
#define P 56
#define Bias 129
#define Exp_1 0x40800000
#define Exp_11 0x4080
#define Ebits 8
#define Frac_mask 0x7fffff
#define Frac_mask1 0xffff007f
#define Ten_pmax 24
#define Bletch 2
#define Bndry_mask 0xffff007f
#define Bndry_mask1 0xffff007f
#define LSB 0x10000
#define Sign_bit 0x8000
#define Log2P 1
#define Tiny0 0x80
#define Tiny1 0
#define Quick_max 15
#define Int_max 15
#endif /* IBM, VAX */
#endif /* IEEE_Arith */

#ifndef IEEE_Arith
#define ROUND_BIASED
#endif

#ifdef RND_PRODQUOT
#define rounded_product(a, b) a = rnd_prod(a, b)
#define rounded_quotient(a, b) a = rnd_quot(a, b)
#ifdef KR_headers
extern double rnd_prod(), rnd_quot();
#else
extern double rnd_prod(double, double), rnd_quot(double, double);
#endif
#else
#define rounded_product(a, b) a *= b
#define rounded_quotient(a, b) a /= b
#endif

#define Big0 (Frac_mask1 | Exp_msk1 * (DBL_MAX_EXP + Bias - 1))
#define Big1 0xffffffff

#undef Pack_16
#ifndef Pack_32
#define Pack_32
#endif

#ifdef NO_LONG_LONG
#undef ULLong
#ifdef Just_16
#undef Pack_32
#define Pack_16
/* When Pack_32 is not defined, we store 16 bits per 32-bit Long.
 * This makes some inner loops simpler and sometimes saves work
 * during multiplications, but it often seems to make things slightly
 * slower.  Hence the default is now to store 32 bits per Long.
 */
#endif
#else /* long long available */
#ifndef Llong
#define Llong long long
#endif
#ifndef ULLong
#define ULLong unsigned Llong
#endif
#endif /* NO_LONG_LONG */

#ifdef Pack_32
#define ULbits 32
#define kshift 5
#define kmask 31
#define ALL_ON 0xffffffff
#else
#define ULbits 16
#define kshift 4
#define kmask 15
#define ALL_ON 0xffff
#endif

#ifndef MULTIPLE_THREADS
#define ACQUIRE_DTOA_LOCK(n) /*nothing*/
#define FREE_DTOA_LOCK(n)    /*nothing*/
#endif

#define Kmax 15

struct Bigint {
  struct Bigint *next;
  int k, maxwds, sign, wds;
  ULong x[1];
};

typedef struct Bigint Bigint;

#ifdef NO_STRING_H
#ifdef DECLARE_SIZE_T
typedef unsigned int size_t;
#endif
extern void memcpy_D2A ANSI((void *, const void *, size_t));
#define Bcopy_D2A(x, y)                                                        \
  memcpy_D2A(&x->sign, &y->sign, y->wds * sizeof(ULong) + 2 * sizeof(int))
#else /* !NO_STRING_H */
#define Bcopy_D2A(x, y)                                                        \
  memcpy(&x->sign, &y->sign, y->wds * sizeof(ULong) + 2 * sizeof(int))
#endif /* NO_STRING_H */

extern char *dtoa_result;
extern CONST double bigtens[], tens[], tinytens[];
extern unsigned char hexdig[];

extern Bigint *Balloc ANSI((int));
extern void Bfree ANSI((Bigint *));
extern void ULtof ANSI((ULong *, ULong *, Long, int));
extern void ULtod ANSI((ULong *, ULong *, Long, int));
extern void ULtodd ANSI((ULong *, ULong *, Long, int));
extern void ULtoQ ANSI((ULong *, ULong *, Long, int));
extern void ULtox ANSI((UShort *, ULong *, Long, int));
extern void ULtoxL ANSI((ULong *, ULong *, Long, int));
extern ULong any_on ANSI((Bigint *, int));
extern double b2d ANSI((Bigint *, int *));
extern int cmp ANSI((Bigint *, Bigint *));
extern void copybits ANSI((ULong *, int, Bigint *));
extern Bigint *d2b ANSI((double, int *, int *));
extern int decrement ANSI((Bigint *));
extern Bigint *diff ANSI((Bigint *, Bigint *));
extern char *dtoa ANSI((double d, int mode, int ndigits, int *decpt, int *sign,
                        char **rve));
extern char *g__fmt ANSI((char *, char *, char *, int, ULong));
extern int gethex ANSI((CONST char **, FPI *, Long *, Bigint **, int));
extern void hexdig_init_D2A(Void);
extern int hexnan ANSI((CONST char **, FPI *, ULong *));
extern int hi0bits_D2A ANSI((ULong));
extern Bigint *i2b ANSI((int));
extern Bigint *increment ANSI((Bigint *));
extern int lo0bits ANSI((ULong *));
extern Bigint *lshift ANSI((Bigint *, int));
extern int match ANSI((CONST char **, char *));
extern Bigint *mult ANSI((Bigint *, Bigint *));
extern Bigint *multadd ANSI((Bigint *, int, int));
extern char *nrv_alloc ANSI((const char *, char **, int));
extern Bigint *pow5mult ANSI((Bigint *, int));
extern int quorem ANSI((Bigint *, Bigint *));
extern double ratio ANSI((Bigint *, Bigint *));
extern void rshift ANSI((Bigint *, int));
extern char *rv_alloc ANSI((int));
extern Bigint *s2b ANSI((CONST char *, int, int, ULong));
extern Bigint *set_ones ANSI((Bigint *, int));
extern char *strcp ANSI((char *, const char *));
extern int strtoIg ANSI((CONST char *, char **, FPI *, Long *, Bigint **,
                         int *));
//extern double strtod ANSI((const char *s00, char **se));
extern Bigint *sum ANSI((Bigint *, Bigint *));
extern int trailz ANSI((Bigint *));
extern double ulp ANSI((double));

/*
 * NAN_WORD0 and NAN_WORD1 are only referenced in strtod.c.  Prior to
 * 20050115, they used to be hard-wired here (to 0x7ff80000 and 0,
 * respectively), but now are determined by compiling and running
 * qnan.c to generate gd_qnan.h, which specifies d_QNAN0 and d_QNAN1.
 * Formerly gdtoaimp.h recommended supplying suitable -DNAN_WORD0=...
 * and -DNAN_WORD1=...  values if necessary.  This should still work.
 * (On HP Series 700/800 machines, -DNAN_WORD0=0x7ff40000 works.)
 */
/*
#ifdef IEEE_Arith
#ifdef IEEE_MC68k
#define _00 0
#define _11 1
#ifndef NAN_WORD0
#define NAN_WORD0 d_QNAN0
#endif
#ifndef NAN_WORD1
#define NAN_WORD1 d_QNAN1
#endif
#else
#define _00 1
#define _11 0
#ifndef NAN_WORD0
#define NAN_WORD0 d_QNAN1
#endif
#ifndef NAN_WORD1
#define NAN_WORD1 d_QNAN0
#endif
#endif
#else
#undef INFNAN_CHECK
#endif
*/

#undef SI
#ifdef Sudden_Underflow
#define SI 1
#else
#define SI 0
#endif

#endif /* GDTOAIMP_H_INCLUDED */

// misc.c

static Bigint *freelist[Kmax + 1];
#ifndef Omit_Private_Memory
#ifndef PRIVATE_MEM
#define PRIVATE_MEM 2304
#endif
#define PRIVATE_mem ((PRIVATE_MEM + sizeof(double) - 1) / sizeof(double))
static double private_mem[PRIVATE_mem], *pmem_next = private_mem;
#endif

Bigint *Balloc_D2A(int k) {
  int x;
  Bigint *rv;
#ifndef Omit_Private_Memory
  unsigned int len;
#endif

  ACQUIRE_DTOA_LOCK(0);
  if ((rv = freelist[k]) != 0) {
    freelist[k] = rv->next;
  } else {
    x = 1 << k;
#ifdef Omit_Private_Memory
    rv = (Bigint *)MALLOC(sizeof(Bigint) + (x - 1) * sizeof(ULong));
#else
    len = (sizeof(Bigint) + (x - 1) * sizeof(ULong) + sizeof(double) - 1) /
          sizeof(double);
    if (pmem_next - private_mem + len <= PRIVATE_mem) {
      rv = (Bigint *)pmem_next;
      pmem_next += len;
    } else
      rv = (Bigint *)MALLOC(len * sizeof(double));
#endif
    rv->k = k;
    rv->maxwds = x;
  }
  FREE_DTOA_LOCK(0);
  rv->sign = rv->wds = 0;
  return rv;
}

void Bfree_D2A(Bigint *v) {
  if (v) {
    ACQUIRE_DTOA_LOCK(0);
    v->next = freelist[v->k];
    freelist[v->k] = v;
    FREE_DTOA_LOCK(0);
  }
}

int lo0bits_D2A(ULong *y) {
  int k;
  ULong x = *y;

  if (x & 7) {
    if (x & 1)
      return 0;
    if (x & 2) {
      *y = x >> 1;
      return 1;
    }
    *y = x >> 2;
    return 2;
  }
  k = 0;
  if (!(x & 0xffff)) {
    k = 16;
    x >>= 16;
  }
  if (!(x & 0xff)) {
    k += 8;
    x >>= 8;
  }
  if (!(x & 0xf)) {
    k += 4;
    x >>= 4;
  }
  if (!(x & 0x3)) {
    k += 2;
    x >>= 2;
  }
  if (!(x & 1)) {
    k++;
    x >>= 1;
    if (!x)
      return 32;
  }
  *y = x;
  return k;
}

Bigint *multadd_D2A(Bigint *b, int m, int a) /* multiply by m and add a */
{
  int i, wds;
#ifdef ULLong
  ULong *x;
  ULLong carry, y;
#else
  ULong carry, *x, y;
#ifdef Pack_32
  ULong xi, z;
#endif
#endif
  Bigint *b1;

  wds = b->wds;
  x = b->x;
  i = 0;
  carry = a;
  do {
#ifdef ULLong
    y = *x * (ULLong)m + carry;
    carry = y >> 32;
    *x++ = y & 0xffffffffUL;
#else
#ifdef Pack_32
    xi = *x;
    y = (xi & 0xffff) * m + carry;
    z = (xi >> 16) * m + (y >> 16);
    carry = z >> 16;
    *x++ = (z << 16) + (y & 0xffff);
#else
    y = *x * m + carry;
    carry = y >> 16;
    *x++ = y & 0xffff;
#endif
#endif
  } while (++i < wds);
  if (carry) {
    if (wds >= b->maxwds) {
      b1 = Balloc_D2A(b->k + 1);
      Bcopy_D2A(b1, b);
      Bfree_D2A(b);
      b = b1;
    }
    b->x[wds++] = carry;
    b->wds = wds;
  }
  return b;
}

int hi0bits_D2A(ULong x) {
  int k = 0;

  if (!(x & 0xffff0000)) {
    k = 16;
    x <<= 16;
  }
  if (!(x & 0xff000000)) {
    k += 8;
    x <<= 8;
  }
  if (!(x & 0xf0000000)) {
    k += 4;
    x <<= 4;
  }
  if (!(x & 0xc0000000)) {
    k += 2;
    x <<= 2;
  }
  if (!(x & 0x80000000)) {
    k++;
    if (!(x & 0x40000000))
      return 32;
  }
  return k;
}

Bigint *i2b_D2A(int i) {
  Bigint *b;

  b = Balloc_D2A(1);
  b->x[0] = i;
  b->wds = 1;
  return b;
}

Bigint *mult_D2A(Bigint *a, Bigint *b) {
  Bigint *c;
  int k, wa, wb, wc;
  ULong *x, *xa, *xae, *xb, *xbe, *xc, *xc0;
  ULong y;
#ifdef ULLong
  ULLong carry, z;
#else
  ULong carry, z;
#ifdef Pack_32
  ULong z2;
#endif
#endif

  if (a->wds < b->wds) {
    c = a;
    a = b;
    b = c;
  }
  k = a->k;
  wa = a->wds;
  wb = b->wds;
  wc = wa + wb;
  if (wc > a->maxwds)
    k++;
  c = Balloc_D2A(k);
  for (x = c->x, xa = x + wc; x < xa; x++)
    *x = 0;
  xa = a->x;
  xae = xa + wa;
  xb = b->x;
  xbe = xb + wb;
  xc0 = c->x;
#ifdef ULLong
  for (; xb < xbe; xc0++) {
    if ((y = *xb++) != 0) {
      x = xa;
      xc = xc0;
      carry = 0;
      do {
        z = *x++ * (ULLong)y + *xc + carry;
        carry = z >> 32;
        *xc++ = z & 0xffffffffUL;
      } while (x < xae);
      *xc = carry;
    }
  }
#else
#ifdef Pack_32
  for (; xb < xbe; xb++, xc0++) {
    if ((y = *xb & 0xffff) != 0) {
      x = xa;
      xc = xc0;
      carry = 0;
      do {
        z = (*x & 0xffff) * y + (*xc & 0xffff) + carry;
        carry = z >> 16;
        z2 = (*x++ >> 16) * y + (*xc >> 16) + carry;
        carry = z2 >> 16;
        Storeinc(xc, z2, z);
      } while (x < xae);
      *xc = carry;
    }
    if ((y = *xb >> 16) != 0) {
      x = xa;
      xc = xc0;
      carry = 0;
      z2 = *xc;
      do {
        z = (*x & 0xffff) * y + (*xc >> 16) + carry;
        carry = z >> 16;
        Storeinc(xc, z, z2);
        z2 = (*x++ >> 16) * y + (*xc & 0xffff) + carry;
        carry = z2 >> 16;
      } while (x < xae);
      *xc = z2;
    }
  }
#else
  for (; xb < xbe; xc0++) {
    if ((y = *xb++) != 0) {
      x = xa;
      xc = xc0;
      carry = 0;
      do {
        z = *x++ * y + *xc + carry;
        carry = z >> 16;
        *xc++ = z & 0xffff;
      } while (x < xae);
      *xc = carry;
    }
  }
#endif
#endif
  for (xc0 = c->x, xc = xc0 + wc; wc > 0 && !*--xc; --wc)
    ;
  c->wds = wc;
  return c;
}

static Bigint *p5s;

Bigint *pow5mult_D2A(Bigint *b, int k) {
  Bigint *b1, *p5, *p51;
  int i;
  static int p05[3] = {5, 25, 125};

  if ((i = k & 3) != 0)
    b = multadd_D2A(b, p05[i - 1], 0);

  if (!(k >>= 2))
    return b;
  if ((p5 = p5s) == 0) {
    /* first time */
#ifdef MULTIPLE_THREADS
    ACQUIRE_DTOA_LOCK(1);
    if (!(p5 = p5s)) {
      p5 = p5s = i2b_D2A(625);
      p5->next = 0;
    }
    FREE_DTOA_LOCK(1);
#else
    p5 = p5s = i2b_D2A(625);
    p5->next = 0;
#endif
  }
  for (;;) {
    if (k & 1) {
      b1 = mult_D2A(b, p5);
      Bfree_D2A(b);
      b = b1;
    }
    if (!(k >>= 1))
      break;
    if ((p51 = p5->next) == 0) {
#ifdef MULTIPLE_THREADS
      ACQUIRE_DTOA_LOCK(1);
      if (!(p51 = p5->next)) {
        p51 = p5->next = mult_D2A(p5, p5);
        p51->next = 0;
      }
      FREE_DTOA_LOCK(1);
#else
      p51 = p5->next = mult_D2A(p5, p5);
      p51->next = 0;
#endif
    }
    p5 = p51;
  }
  return b;
}

Bigint *lshift_D2A(Bigint *b, int k) {
  int i, k1, n, n1;
  Bigint *b1;
  ULong *x, *x1, *xe, z;

  n = k >> kshift;
  k1 = b->k;
  n1 = n + b->wds + 1;
  for (i = b->maxwds; n1 > i; i <<= 1)
    k1++;
  b1 = Balloc_D2A(k1);
  x1 = b1->x;
  for (i = 0; i < n; i++)
    *x1++ = 0;
  x = b->x;
  xe = x + b->wds;
  if (k &= kmask) {
#ifdef Pack_32
    k1 = 32 - k;
    z = 0;
    do {
      *x1++ = *x << k | z;
      z = *x++ >> k1;
    } while (x < xe);
    if ((*x1 = z) != 0)
      ++n1;
#else
    k1 = 16 - k;
    z = 0;
    do {
      *x1++ = *x << k & 0xffff | z;
      z = *x++ >> k1;
    } while (x < xe);
    if (*x1 = z)
      ++n1;
#endif
  } else
    do
      *x1++ = *x++;
    while (x < xe);
  b1->wds = n1 - 1;
  Bfree_D2A(b);
  return b1;
}

int cmp_D2A(Bigint *a, Bigint *b) {
  ULong *xa, *xa0, *xb, *xb0;
  int i, j;

  i = a->wds;
  j = b->wds;
#ifdef DEBUG
  if (i > 1 && !a->x[i - 1])
    Bug("cmp called with a->x[a->wds-1] == 0");
  if (j > 1 && !b->x[j - 1])
    Bug("cmp called with b->x[b->wds-1] == 0");
#endif
  if (i -= j)
    return i;
  xa0 = a->x;
  xa = xa0 + j;
  xb0 = b->x;
  xb = xb0 + j;
  for (;;) {
    if (*--xa != *--xb)
      return *xa < *xb ? -1 : 1;
    if (xa <= xa0)
      break;
  }
  return 0;
}

Bigint *diff_D2A(Bigint *a, Bigint *b) {
  Bigint *c;
  int i, wa, wb;
  ULong *xa, *xae, *xb, *xbe, *xc;
#ifdef ULLong
  ULLong borrow, y;
#else
  ULong borrow, y;
#ifdef Pack_32
  ULong z;
#endif
#endif

  i = cmp_D2A(a, b);
  if (!i) {
    c = Balloc_D2A(0);
    c->wds = 1;
    c->x[0] = 0;
    return c;
  }
  if (i < 0) {
    c = a;
    a = b;
    b = c;
    i = 1;
  } else
    i = 0;
  c = Balloc_D2A(a->k);
  c->sign = i;
  wa = a->wds;
  xa = a->x;
  xae = xa + wa;
  wb = b->wds;
  xb = b->x;
  xbe = xb + wb;
  xc = c->x;
  borrow = 0;
#ifdef ULLong
  do {
    y = (ULLong)*xa++ - *xb++ - borrow;
    borrow = y >> 32 & 1UL;
    *xc++ = y & 0xffffffffUL;
  } while (xb < xbe);
  while (xa < xae) {
    y = *xa++ - borrow;
    borrow = y >> 32 & 1UL;
    *xc++ = y & 0xffffffffUL;
  }
#else
#ifdef Pack_32
  do {
    y = (*xa & 0xffff) - (*xb & 0xffff) - borrow;
    borrow = (y & 0x10000) >> 16;
    z = (*xa++ >> 16) - (*xb++ >> 16) - borrow;
    borrow = (z & 0x10000) >> 16;
    Storeinc(xc, z, y);
  } while (xb < xbe);
  while (xa < xae) {
    y = (*xa & 0xffff) - borrow;
    borrow = (y & 0x10000) >> 16;
    z = (*xa++ >> 16) - borrow;
    borrow = (z & 0x10000) >> 16;
    Storeinc(xc, z, y);
  }
#else
  do {
    y = *xa++ - *xb++ - borrow;
    borrow = (y & 0x10000) >> 16;
    *xc++ = y & 0xffff;
  } while (xb < xbe);
  while (xa < xae) {
    y = *xa++ - borrow;
    borrow = (y & 0x10000) >> 16;
    *xc++ = y & 0xffff;
  }
#endif
#endif
  while (!*--xc)
    wa--;
  c->wds = wa;
  return c;
}

double b2d_D2A(Bigint *a, int *e) {
  ULong *xa, *xa0, w, y, z;
  int k;
  double d;
#ifdef VAX
  ULong d0, d1;
#else
#define d0 word0(d)
#define d1 word1(d)
#endif

  xa0 = a->x;
  xa = xa0 + a->wds;
  y = *--xa;

#ifdef DEBUG
  if (!y)
    Bug("zero y in b2d");
#endif
  k = hi0bits_D2A(y);
  *e = 32 - k;
#ifdef Pack_32
  if (k < Ebits) {
    d0 = Exp_1 | y >> (Ebits - k);
    w = xa > xa0 ? *--xa : 0;
    d1 = y << ((32 - Ebits) + k) | w >> (Ebits - k);
    goto ret_d;
  }
  z = xa > xa0 ? *--xa : 0;
  if (k -= Ebits) {
    d0 = Exp_1 | y << k | z >> (32 - k);
    y = xa > xa0 ? *--xa : 0;
    d1 = z << k | y >> (32 - k);
  } else {
    d0 = Exp_1 | y;
    d1 = z;
  }

#else
  if (k < Ebits + 16) {
    z = xa > xa0 ? *--xa : 0;
    d0 = Exp_1 | y << k - Ebits | z >> Ebits + 16 - k;
    w = xa > xa0 ? *--xa : 0;
    y = xa > xa0 ? *--xa : 0;
    d1 = z << k + 16 - Ebits | w << k - Ebits | y >> 16 + Ebits - k;
    goto ret_d;
  }
  z = xa > xa0 ? *--xa : 0;
  w = xa > xa0 ? *--xa : 0;
  k -= Ebits + 16;
  d0 = Exp_1 | y << k + 16 | z << k | w >> 16 - k;
  y = xa > xa0 ? *--xa : 0;
  d1 = w << k + 16 | y << k;
#endif
ret_d:
#ifdef VAX
  word0(d) = d0 >> 16 | d0 << 16;
  word1(d) = d1 >> 16 | d1 << 16;
#endif
  return dval(d);
}
#undef d0
#undef d1

Bigint *d2b_D2A(double d, int *e, int *bits) {
  Bigint *b;
#ifndef Sudden_Underflow
  int i;
#endif
  int de, k, de2;
  ULong *x, y, z;
  long int deb;
#ifdef VAX
  ULong d0, d1;
  d0 = word0(d) >> 16 | word0(d) << 16;
  d1 = word1(d) >> 16 | word1(d) << 16;
#else
#define d0 word0(d)
#define d1 word1(d)
#endif

#ifdef Pack_32
  b = Balloc_D2A(1);
#else
  b = Balloc_D2A(2);
#endif
  x = b->x;

  z = d0 & Frac_mask;
  d0 &= 0x7fffffff; /* clear sign bit, which we ignore */
#ifdef Sudden_Underflow
  de = (int)(d0 >> Exp_shift);
#ifndef IBM
  z |= Exp_msk11;
#endif
#else
  de2 = d0;
  de2 = Exp_shift;
  de2 = d0 >> Exp_shift;

  if ((de = (int)(d0 >> Exp_shift)) != 0)
    z |= Exp_msk1;
#endif
#ifdef Pack_32
  if ((y = d1) != 0) {
    if ((k = lo0bits_D2A(&y)) != 0) {
      x[0] = y | z << (32 - k);
      z >>= k;
    } else
      x[0] = y;
#ifndef Sudden_Underflow
    i =
#endif
        b->wds = (x[1] = z) != 0 ? 2 : 1;
  } else {
#ifdef DEBUG
    if (!z)
      Bug("Zero passed to d2b");
#endif
    k = lo0bits_D2A(&z);
    x[0] = z;
#ifndef Sudden_Underflow
    i =
#endif
        b->wds = 1;
    k += 32;
  }
#else
  if ((y = d1) != 0) {
    if ((k = lo0bits_D2A(&y)) != 0)
      if (k >= 16) {
        x[0] = y | z << 32 - k & 0xffff;
        x[1] = z >> k - 16 & 0xffff;
        x[2] = z >> k;
        i = 2;
      } else {
        x[0] = y & 0xffff;
        x[1] = y >> 16 | z << 16 - k & 0xffff;
        x[2] = z >> k & 0xffff;
        x[3] = z >> k + 16;
        i = 3;
      }
    else {
      x[0] = y & 0xffff;
      x[1] = y >> 16;
      x[2] = z & 0xffff;
      x[3] = z >> 16;
      i = 3;
    }
  } else {
#ifdef DEBUG
    if (!z)
      Bug("Zero passed to d2b");
#endif
    k = lo0bits_D2A(&z);
    if (k >= 16) {
      x[0] = z;
      i = 0;
    } else {
      x[0] = z & 0xffff;
      x[1] = z >> 16;
      i = 1;
    }
    k += 32;
  }
  while (!x[i])
    --i;
  b->wds = i + 1;
#endif
#ifndef Sudden_Underflow
  if (de) {
#endif
#ifdef IBM
    *e = (de - Bias - (P - 1) << 2) + k;
    *bits = 4 * P + 8 - k - hi0bits_D2A(word0(d) & Frac_mask);
#else
  *e = de - Bias - (P - 1) + k;
  *bits = P - k;
#endif
#ifndef Sudden_Underflow
  } else {
    *e = de - Bias - (P - 1) + 1 + k;
#ifdef Pack_32
    *bits = 32 * i - hi0bits_D2A(x[i - 1]);
#else
    *bits = (i + 2) * 16 - hi0bits_D2A(x[i]);
#endif
  }
#endif
  return b;
}
#undef d0
#undef d1

CONST double
#ifdef IEEE_Arith
    bigtens[] = {1e16, 1e32, 1e64, 1e128, 1e256};
CONST double tinytens[] = {1e-16, 1e-32, 1e-64, 1e-128, 1e-256};
#else
#ifdef IBM
    bigtens[] = {1e16, 1e32, 1e64};
CONST double tinytens[] = {1e-16, 1e-32, 1e-64};
#else
    bigtens[] = {1e16, 1e32};
CONST double tinytens[] = {1e-16, 1e-32};
#endif
#endif

CONST double tens[] = {1e0,
                       1e1,
                       1e2,
                       1e3,
                       1e4,
                       1e5,
                       1e6,
                       1e7,
                       1e8,
                       1e9,
                       1e10,
                       1e11,
                       1e12,
                       1e13,
                       1e14,
                       1e15,
                       1e16,
                       1e17,
                       1e18,
                       1e19,
                       1e20,
                       1e21,
                       1e22
#ifdef VAX
                       ,
                       1e23,
                       1e24
#endif
};

char *strcp_D2A(char *a, CONST char *b) {
  while (*a = *b++)
    a++;
  return a;
}

#ifdef NO_STRING_H

Char *memcpy_D2A(void *a1, void *b1, size_t len) {
  register char *a = (char *)a1, *ae = a + len;
  register char *b = (char *)b1, *a0 = a;
  while (a < ae)
    *a++ = *b++;
  return a0;
}

#endif /* NO_STRING_H */

// dmisc.c

#ifndef MULTIPLE_THREADS
char *dtoa_result;
#endif

char *rv_alloc_D2A(int i) {
  int j, k, *r;

  j = sizeof(ULong);
  for (k = 0; sizeof(Bigint) - sizeof(ULong) - sizeof(int) + j <= i; j <<= 1)
    k++;
  r = (int *)Balloc_D2A(k);
  *r = k;
  return
#ifndef MULTIPLE_THREADS
      dtoa_result =
#endif
          (char *)(r + 1);
}

char *nrv_alloc_D2A(const char *s, char **rve, int n) {
  char *rv, *t;

  t = rv = rv_alloc_D2A(n);
  while ((*t = *s++) != 0)
    t++;
  if (rve)
    *rve = t;
  return rv;
}

/* freedtoa(s) must be used to free values s returned by dtoa
 * when MULTIPLE_THREADS is #defined.  It should be used in all cases,
 * but for consistency with earlier versions of dtoa, it is optional
 * when MULTIPLE_THREADS is not defined.
 */

void freedtoa_D2A(char *s) {
  Bigint *b = (Bigint *)((int *)s - 1);
  b->maxwds = 1 << (b->k = *(int *)b);
  Bfree_D2A(b);
#ifndef MULTIPLE_THREADS
  if (s == dtoa_result)
    dtoa_result = 0;
#endif
}

int quorem_D2A(Bigint *b, Bigint *S) {
  int n;
  ULong *bx, *bxe, q, *sx, *sxe;
#ifdef ULLong
  ULLong borrow, carry, y, ys;
#else
  ULong borrow, carry, y, ys;
#ifdef Pack_32
  ULong si, z, zs;
#endif
#endif

  n = S->wds;
#ifdef DEBUG
  /*debug*/ if (b->wds > n)
    /*debug*/ Bug("oversize b in quorem");
#endif
  if (b->wds < n)
    return 0;
  sx = S->x;
  sxe = sx + --n;
  bx = b->x;
  bxe = bx + n;
  q = *bxe / (*sxe + 1); /* ensure q <= true quotient */
#ifdef DEBUG
  /*debug*/ if (q > 9)
    /*debug*/ Bug("oversized quotient in quorem");
#endif
  if (q) {
    borrow = 0;
    carry = 0;
    do {
#ifdef ULLong
      ys = *sx++ * (ULLong)q + carry;
      carry = ys >> 32;
      y = *bx - (ys & 0xffffffffUL) - borrow;
      borrow = y >> 32 & 1UL;
      *bx++ = y & 0xffffffffUL;
#else
#ifdef Pack_32
      si = *sx++;
      ys = (si & 0xffff) * q + carry;
      zs = (si >> 16) * q + (ys >> 16);
      carry = zs >> 16;
      y = (*bx & 0xffff) - (ys & 0xffff) - borrow;
      borrow = (y & 0x10000) >> 16;
      z = (*bx >> 16) - (zs & 0xffff) - borrow;
      borrow = (z & 0x10000) >> 16;
      Storeinc(bx, z, y);
#else
      ys = *sx++ * q + carry;
      carry = ys >> 16;
      y = *bx - (ys & 0xffff) - borrow;
      borrow = (y & 0x10000) >> 16;
      *bx++ = y & 0xffff;
#endif
#endif
    } while (sx <= sxe);
    if (!*bxe) {
      bx = b->x;
      while (--bxe > bx && !*bxe)
        --n;
      b->wds = n;
    }
  }
  if (cmp_D2A(b, S) >= 0) {
    q++;
    borrow = 0;
    carry = 0;
    bx = b->x;
    sx = S->x;
    do {
#ifdef ULLong
      ys = *sx++ + carry;
      carry = ys >> 32;
      y = *bx - (ys & 0xffffffffUL) - borrow;
      borrow = y >> 32 & 1UL;
      *bx++ = y & 0xffffffffUL;
#else
#ifdef Pack_32
      si = *sx++;
      ys = (si & 0xffff) + carry;
      zs = (si >> 16) + (ys >> 16);
      carry = zs >> 16;
      y = (*bx & 0xffff) - (ys & 0xffff) - borrow;
      borrow = (y & 0x10000) >> 16;
      z = (*bx >> 16) - (zs & 0xffff) - borrow;
      borrow = (z & 0x10000) >> 16;
      Storeinc(bx, z, y);
#else
      ys = *sx++ + carry;
      carry = ys >> 16;
      y = *bx - (ys & 0xffff) - borrow;
      borrow = (y & 0x10000) >> 16;
      *bx++ = y & 0xffff;
#endif
#endif
    } while (sx <= sxe);
    bx = b->x;
    bxe = bx + n;
    if (!*bxe) {
      while (--bxe > bx && !*bxe)
        --n;
      b->wds = n;
    }
  }
  return q;
}

// smisc.c

Bigint *s2b_D2A(CONST char *s, int nd0, int nd, ULong y9) {
  Bigint *b;
  int i, k;
  Long x, y;

  x = (nd + 8) / 9;
  for (k = 0, y = 1; x > y; y <<= 1, k++)
    ;
#ifdef Pack_32
  b = Balloc_D2A(k);
  b->x[0] = y9;
  b->wds = 1;
#else
  b = Balloc_D2A(k + 1);
  b->x[0] = y9 & 0xffff;
  b->wds = (b->x[1] = y9 >> 16) ? 2 : 1;
#endif

  i = 9;
  if (9 < nd0) {
    s += 9;
    do {
      b = multadd_D2A(b, 10, *s++ - '0');
    } while (++i < nd0);
    s++;
  } else {
    s += 10;
  }
  for (; i < nd; i++) {
    b = multadd_D2A(b, 10, *s++ - '0');
  }
  return b;
}

double ratio_D2A(Bigint *a, Bigint *b) {
  double da, db;
  int k, ka, kb;
  unsigned long int deba, debb;

  dval(da) = b2d_D2A(a, &ka);
  dval(db) = b2d_D2A(b, &kb);
  k = ka - kb + ULbits * (a->wds - b->wds);

#ifdef IBM
  if (k > 0) {
    word0(da) += (k >> 2) * Exp_msk1;
    if (k &= 3)
      dval(da) *= 1 << k;
  } else {
    k = -k;
    word0(db) += (k >> 2) * Exp_msk1;
    if (k &= 3)
      dval(db) *= 1 << k;
  }
#else
  if (k > 0) {
    word0(da) += k * Exp_msk1;
  } else {
    k = -k;
    word0(db) += k * Exp_msk1;
  }
#endif
  return dval(da) / dval(db);
}

#ifdef INFNAN_CHECK

int match_D2A(CONST char **sp, char *t) {
  int c, d;
  CONST char *s = *sp;

  while ((d = *t++) != 0) {
    if ((c = *++s) >= 'A' && c <= 'Z')
      c += 'a' - 'A';
    if (c != d)
      return 0;
  }
  *sp = s + 1;
  return 1;
}
#endif /* INFNAN_CHECK */

void copybits_D2A(ULong *c, int n, Bigint *b) {
  ULong *ce, *x, *xe;
#ifdef Pack_16
  int nw, nw1;
#endif

  ce = c + ((n - 1) >> kshift) + 1;
  x = b->x;
#ifdef Pack_32
  xe = x + b->wds;
  while (x < xe) {
    *c++ = *x++;
  }
#else
  nw = b->wds;
  nw1 = nw & 1;
  for (xe = x + (nw - nw1); x < xe; x += 2)
    Storeinc(c, x[1], x[0]);
  if (nw1)
    *c++ = *x;
#endif
  while (c < ce)
    *c++ = 0;
}

ULong any_on_D2A(Bigint *b, int k) {
  int n, nwds;
  ULong *x, *x0, x1, x2;

  x = b->x;
  nwds = b->wds;
  n = k >> kshift;
  if (n > nwds)
    n = nwds;
  else if (n < nwds && (k &= kmask)) {
    x1 = x2 = x[n];
    x1 >>= k;
    x1 <<= k;
    if (x1 != x2)
      return 1;
  }
  x0 = x;
  x += n;
  while (x > x0)
    if (*--x)
      return 1;
  return 0;
}

// gmisc.c

void rshift_D2A(Bigint *b, int k) {
  ULong *x, *x1, *xe, y;
  int n;

  x = x1 = b->x;
  n = k >> kshift;
  if (n < b->wds) {
    xe = x + b->wds;
    x += n;
    if (k &= kmask) {
      n = ULbits - k;
      y = *x++ >> k;
      while (x < xe) {
        *x1++ = (y | (*x << n)) & ALL_ON;
        y = *x++ >> k;
      }
      if ((*x1 = y) != 0)
        x1++;
    } else
      while (x < xe)
        *x1++ = *x++;
  }
  if ((b->wds = x1 - b->x) == 0)
    b->x[0] = 0;
}

int trailz_D2A(Bigint *b) {
  ULong L, *x, *xe;
  int n = 0;

  x = b->x;
  xe = x + b->wds;
  for (n = 0; x < xe && !*x; x++) {
    n += ULbits;
  }
  if (x < xe) {
    L = *x;
    n += lo0bits_D2A(&L);
  }
  return n;
}

// strdog.c common functions

Bigint *increment_D2A(Bigint *b) {
  ULong *x, *xe;
  Bigint *b1;
#ifdef Pack_16
  ULong carry = 1, y;
#endif

  x = b->x;
  xe = x + b->wds;
#ifdef Pack_32
  do {
    if (*x < (ULong)0xffffffffL) {
      ++*x;
      return b;
    }
    *x++ = 0;
  } while (x < xe);
#else
  do {
    y = *x + carry;
    carry = y >> 16;
    *x++ = y & 0xffff;
    if (!carry)
      return b;
  } while (x < xe);
  if (carry)
#endif
  {
    if (b->wds >= b->maxwds) {
      b1 = Balloc_D2A(b->k + 1);
      Bcopy_D2A(b1, b);
      Bfree_D2A(b);
      b = b1;
    }
    b->x[b->wds++] = 1;
  }
  return b;
}

int decrement_D2A(Bigint *b) {
  ULong *x, *xe;
#ifdef Pack_16
  ULong borrow = 1, y;
#endif

  x = b->x;
  xe = x + b->wds;
#ifdef Pack_32
  do {
    if (*x) {
      --*x;
      break;
    }
    *x++ = 0xffffffffL;
  } while (x < xe);
#else
  do {
    y = *x - borrow;
    borrow = (y & 0x10000) >> 16;
    *x++ = y & 0xffff;
  } while (borrow && x < xe);
#endif
  return STRTOG_Inexlo;
}

static int all_on_D2A(Bigint *b, int n) {
  ULong *x, *xe;

  x = b->x;
  xe = x + (n >> kshift);
  while (x < xe)
    if ((*x++ & ALL_ON) != ALL_ON)
      return 0;
  if (n &= kmask)
    return ((*x | (ALL_ON << n)) & ALL_ON) == ALL_ON;
  return 1;
}

Bigint *set_ones_D2A(Bigint *b, int n) {
  int k;
  ULong *x, *xe;

  k = (n + ((1 << kshift) - 1)) >> kshift;
  if (b->k < k) {
    Bfree_D2A(b);
    b = Balloc_D2A(k);
  }
  k = n >> kshift;
  if (n &= kmask)
    k++;
  b->wds = k;
  x = b->x;
  xe = x + k;
  while (x < xe)
    *x++ = ALL_ON;
  if (n)
    x[-1] >>= ULbits - n;
  return b;
}

// hdinit.c

unsigned char hexdig[256];

static void htinit(unsigned char *h, unsigned char *s, int inc) {
  int i, j;
  for (i = 0; (j = s[i]) != 0; i++)
    h[j] = i + inc;
}

void hexdig_init_D2A(Void) {
#define USC (unsigned char *)
  htinit(hexdig, USC "0123456789", 0x10);
  htinit(hexdig, USC "abcdef", 0x10 + 10);
  htinit(hexdig, USC "ABCDEF", 0x10 + 10);
}

// gethex.c

#ifdef USE_LOCALE
#include "locale.h"
#endif

int gethex_D2A(CONST char **sp, FPI *fpi, Long *exp, Bigint **bp, int sign) {
  Bigint *b;
  CONST unsigned char *decpt, *s0, *s, *s1;
  int esign, havedig, irv, k, n, nbits, up, zret;
  ULong L, lostbits, *x;
  Long e, e1;
#ifdef USE_LOCALE
  unsigned char decimalpoint = *localeconv()->decimal_point;
#else
#define decimalpoint '.'
#endif

  if (!hexdig['0'])
    hexdig_init_D2A();
  havedig = 0;
  s0 = *(CONST unsigned char **)sp + 2;
  while (s0[havedig] == '0')
    havedig++;
  s0 += havedig;
  s = s0;
  decpt = 0;
  zret = 0;
  e = 0;
  if (!hexdig[*s]) {
    zret = 1;
    if (*s != decimalpoint)
      goto pcheck;
    decpt = ++s;
    if (!hexdig[*s])
      goto pcheck;
    while (*s == '0')
      s++;
    if (hexdig[*s])
      zret = 0;
    havedig = 1;
    s0 = s;
  }
  while (hexdig[*s])
    s++;
  if (*s == decimalpoint && !decpt) {
    decpt = ++s;
    while (hexdig[*s])
      s++;
  }
  if (decpt)
    e = -(((Long)(s - decpt)) << 2);
pcheck:
  s1 = s;
  switch (*s) {
  case 'p':
  case 'P':
    esign = 0;
    switch (*++s) {
    case '-':
      esign = 1;
      /* no break */
    case '+':
      s++;
    }
    if ((n = hexdig[*s]) == 0 || n > 0x19) {
      s = s1;
      break;
    }
    e1 = n - 0x10;
    while ((n = hexdig[*++s]) != 0 && n <= 0x19)
      e1 = 10 * e1 + n - 0x10;
    if (esign)
      e1 = -e1;
    e += e1;
  }
  *sp = (char *)s;
  if (zret) {
    if (!havedig)
      *sp = (char *)s0 - 1;
    return STRTOG_Zero;
  }
  n = s1 - s0 - 1;
  for (k = 0; n > 7; n >>= 1)
    k++;
  b = Balloc_D2A(k);
  x = b->x;
  n = 0;
  L = 0;
  while (s1 > s0) {
    if (*--s1 == decimalpoint)
      continue;
    if (n == 32) {
      *x++ = L;
      L = 0;
      n = 0;
    }
    L |= (hexdig[*s1] & 0x0f) << n;
    n += 4;
  }
  *x++ = L;
  b->wds = n = x - b->x;
  n = 32 * n - hi0bits_D2A(L);
  nbits = fpi->nbits;
  lostbits = 0;
  x = b->x;
  if (n > nbits) {
    n -= nbits;
    if (any_on_D2A(b, n)) {
      lostbits = 1;
      k = n - 1;
      if (x[k >> kshift] & 1 << (k & kmask)) {
        lostbits = 2;
        if (k > 1 && any_on_D2A(b, k - 1))
          lostbits = 3;
      }
    }
    rshift_D2A(b, n);
    e += n;
  } else if (n < nbits) {
    n = nbits - n;
    b = lshift_D2A(b, n);
    e -= n;
    x = b->x;
  }
  if (e > fpi->emax) {
  ovfl:
    Bfree_D2A(b);
    *bp = 0;
    return STRTOG_Infinite | STRTOG_Overflow | STRTOG_Inexhi;
  }
  irv = STRTOG_Normal;
  if (e < fpi->emin) {
    irv = STRTOG_Denormal;
    n = fpi->emin - e;
    if (n >= nbits) {
      switch (fpi->rounding) {
      case FPI_Round_near:
        if (n == nbits && (n < 2 || any_on_D2A(b, n - 1)))
          goto one_bit;
        break;
      case FPI_Round_up:
        if (!sign)
          goto one_bit;
        break;
      case FPI_Round_down:
        if (sign) {
        one_bit:
          *exp = fpi->emin;
          x[0] = b->wds = 1;
          *bp = b;
          return STRTOG_Denormal | STRTOG_Inexhi | STRTOG_Underflow;
        }
      }
      Bfree_D2A(b);
      *bp = 0;
      return STRTOG_Zero | STRTOG_Inexlo | STRTOG_Underflow;
    }
    k = n - 1;
    if (lostbits)
      lostbits = 1;
    else if (k > 0)
      lostbits = any_on_D2A(b, k);
    if (x[k >> kshift] & 1 << (k & kmask))
      lostbits |= 2;
    nbits -= n;
    rshift_D2A(b, n);
    e = fpi->emin;
  }
  if (lostbits) {
    up = 0;
    switch (fpi->rounding) {
    case FPI_Round_zero:
      break;
    case FPI_Round_near:
      if (lostbits & 2 && (lostbits & 1) | x[0] & 1)
        up = 1;
      break;
    case FPI_Round_up:
      up = 1 - sign;
      break;
    case FPI_Round_down:
      up = sign;
    }
    if (up) {
      k = b->wds;
      b = increment_D2A(b);
      x = b->x;
      if (irv == STRTOG_Denormal) {
        if (nbits == fpi->nbits - 1 &&
            x[nbits >> kshift] & 1 << (nbits & kmask))
          irv = STRTOG_Normal;
      } else if (b->wds > k ||
                 (n = nbits & kmask) != 0 && hi0bits_D2A(x[k - 1]) < 32 - n) {
        rshift_D2A(b, 1);
        if (++e > fpi->emax)
          goto ovfl;
      }
      irv |= STRTOG_Inexhi;
    } else
      irv |= STRTOG_Inexlo;
  }
  *bp = b;
  *exp = e;
  return irv;
}

// sum.c

Bigint *sum_D2A(Bigint *a, Bigint *b) {
  Bigint *c;
  ULong carry, *xc, *xa, *xb, *xe, y;
#ifdef Pack_32
  ULong z;
#endif

  if (a->wds < b->wds) {
    c = b;
    b = a;
    a = c;
  }
  c = Balloc_D2A(a->k);
  c->wds = a->wds;
  carry = 0;
  xa = a->x;
  xb = b->x;
  xc = c->x;
  xe = xc + b->wds;
#ifdef Pack_32
  do {
    y = (*xa & 0xffff) + (*xb & 0xffff) + carry;
    carry = (y & 0x10000) >> 16;
    z = (*xa++ >> 16) + (*xb++ >> 16) + carry;
    carry = (z & 0x10000) >> 16;
    Storeinc(xc, z, y);
  } while (xc < xe);
  xe += a->wds - b->wds;
  while (xc < xe) {
    y = (*xa & 0xffff) + carry;
    carry = (y & 0x10000) >> 16;
    z = (*xa++ >> 16) + carry;
    carry = (z & 0x10000) >> 16;
    Storeinc(xc, z, y);
  }
#else
  do {
    y = *xa++ + *xb++ + carry;
    carry = (y & 0x10000) >> 16;
    *xc++ = y & 0xffff;
  } while (xc < xe);
  xe += a->wds - b->wds;
  while (xc < xe) {
    y = *xa++ + carry;
    carry = (y & 0x10000) >> 16;
    *xc++ = y & 0xffff;
  }
#endif
  if (carry) {
    if (c->wds == c->maxwds) {
      b = Balloc_D2A(c->k + 1);
      Bcopy_D2A(b, c);
      Bfree_D2A(c);
      c = b;
    }
    c->x[c->wds++] = 1;
  }
  return c;
}

// strtodg.c

#ifdef USE_LOCALE
#include "locale.h"
#endif

static CONST int fivesbits[] = {0,
                                3,
                                5,
                                7,
                                10,
                                12,
                                14,
                                17,
                                19,
                                21,
                                24,
                                26,
                                28,
                                31,
                                33,
                                35,
                                38,
                                40,
                                42,
                                45,
                                47,
                                49,
                                52
#ifdef VAX
                                ,
                                54,
                                56
#endif
};

static int rvOK_D2A(double d, FPI *fpi, Long *exp, ULong *bits, int exact,
                    int rd, int *irv) {
  Bigint *b;
  ULong carry, inex, lostbits;
  int bdif, e, j, k, k1, nb, rv;

  carry = rv = 0;
  b = d2b_D2A(d, &e, &bdif);
  bdif -= nb = fpi->nbits;
  e += bdif;
  if (bdif <= 0) {
    if (exact)
      goto trunc;
    goto ret;
  }
  if (P == nb) {
    if (
#ifndef IMPRECISE_INEXACT
        exact &&
#endif
        fpi->rounding ==
#ifdef RND_PRODQUOT
            FPI_Round_near
#else
        Flt_Rounds
#endif
    )
      goto trunc;
    goto ret;
  }
  switch (rd) {
  case 1:
    goto trunc;
  case 2:
    break;
  default: /* round near */
    k = bdif - 1;
    if (k < 0)
      goto trunc;
    if (!k) {
      if (!exact)
        goto ret;
      if (b->x[0] & 2)
        break;
      goto trunc;
    }
    if (b->x[k >> kshift] & ((ULong)1 << (k & kmask)))
      break;
    goto trunc;
  }
  /* "break" cases: round up 1 bit, then truncate; bdif > 0 */
  carry = 1;
trunc:
  inex = lostbits = 0;
  if (bdif > 0) {
    if ((lostbits = any_on_D2A(b, bdif)) != 0)
      inex = STRTOG_Inexlo;
    rshift_D2A(b, bdif);
    if (carry) {
      inex = STRTOG_Inexhi;
      b = increment_D2A(b);
      if ((j = nb & kmask) != 0)
        j = ULbits - j;
      if (hi0bits_D2A(b->x[b->wds - 1]) != j) {
        if (!lostbits)
          lostbits = b->x[0] & 1;
        rshift_D2A(b, 1);
        e++;
      }
    }
  } else if (bdif < 0) {
    b = lshift_D2A(b, -bdif);
  }
  if (e < fpi->emin) {
    k = fpi->emin - e;
    e = fpi->emin;
    if (k > nb || fpi->sudden_underflow) {
      b->wds = inex = 0;
      *irv = STRTOG_Underflow | STRTOG_Inexlo;
    } else {
      k1 = k - 1;
      if (k1 > 0 && !lostbits)
        lostbits = any_on_D2A(b, k1);
      if (!lostbits && !exact)
        goto ret;
      lostbits |= carry = b->x[k1 >> kshift] & (1 << (k1 & kmask));
      rshift_D2A(b, k);
      *irv = STRTOG_Denormal;
      if (carry) {
        b = increment_D2A(b);
        inex = STRTOG_Inexhi | STRTOG_Underflow;
      } else if (lostbits)
        inex = STRTOG_Inexlo | STRTOG_Underflow;
    }
  } else if (e > fpi->emax) {
    e = fpi->emax + 1;
    *irv = STRTOG_Infinite | STRTOG_Overflow | STRTOG_Inexhi;
#ifndef NO_ERRNO
    errno = ERANGE;
#endif
    b->wds = inex = 0;
  }

  *exp = e;
  copybits_D2A(bits, nb, b);
  *irv |= inex;
  rv = 1;
ret:
  Bfree_D2A(b);
  return rv;
}

static int mantbits_D2A(double d) {
  ULong L;
#ifdef VAX
  L = word1(d) << 16 | word1(d) >> 16;
  if (L)
#else
  if ((L = word1(d)) != 0)
#endif
    return P - lo0bits_D2A(&L);
#ifdef VAX
  L = word0(d) << 16 | word0(d) >> 16 | Exp_msk11;
#else
  L = word0(d) | Exp_msk1;
#endif
  return P - 32 - lo0bits_D2A(&L);
}

int strtodg_D2A(CONST char *s00, char **se, FPI *fpi, Long *exp, ULong *bits) {
  int abe, abits, asub;
  int bb0, bb2, bb5, bbe, bd2, bd5, bbbits, bs2, c, decpt, denorm;
  int dsign, e, e1, e2, emin, esign, finished, i, inex, irv;
  int j, k, nbits, nd, nd0, nf, nz, nz0, rd, rvbits, rve, rve1, sign;
  int sudden_underflow;
  CONST char *s, *s0, *s1;
  double adj, adj0, rv, tol;
  Long L;
  ULong y, z;
  Bigint *ab, *bb, *bb1, *bd, *bd0, *bs, *delta, *rvb, *rvb0;

  irv = STRTOG_Zero;
  denorm = sign = nz0 = nz = 0;
  dval(rv) = 0.;
  rvb = 0;
  nbits = fpi->nbits;
  for (s = s00;; s++)
    switch (*s) {
    case '-':
      sign = 1;
      /* no break */
    case '+':
      if (*++s)
        goto break2;
      /* no break */
    case 0:
      sign = 0;
      irv = STRTOG_NoNumber;
      s = s00;
      goto ret;
    case '\t':
    case '\n':
    case '\v':
    case '\f':
    case '\r':
    case ' ':
      continue;
    default:
      goto break2;
    }
break2:
  if (*s == '0') {
#ifndef NO_HEX_FP
    switch (s[1]) {
    case 'x':
    case 'X':
      irv = gethex_D2A(&s, fpi, exp, &rvb, sign);
      if (irv == STRTOG_NoNumber) {
        s = s00;
        sign = 0;
      }
      goto ret;
    }
#endif
    nz0 = 1;
    while (*++s == '0')
      ;
    if (!*s)
      goto ret;
  }
  sudden_underflow = fpi->sudden_underflow;
  s0 = s;
  y = z = 0;
  for (decpt = nd = nf = 0; (c = *s) >= '0' && c <= '9'; nd++, s++)
    if (nd < 9)
      y = 10 * y + c - '0';
    else if (nd < 16)
      z = 10 * z + c - '0';
  nd0 = nd;
#ifdef USE_LOCALE
  if (c == *localeconv()->decimal_point)
#else
  if (c == '.')
#endif
  {
    decpt = 1;
    c = *++s;
    if (!nd) {
      for (; c == '0'; c = *++s)
        nz++;
      if (c > '0' && c <= '9') {
        s0 = s;
        nf += nz;
        nz = 0;
        goto have_dig;
      }
      goto dig_done;
    }
    for (; c >= '0' && c <= '9'; c = *++s) {
    have_dig:
      nz++;
      if (c -= '0') {
        nf += nz;
        for (i = 1; i < nz; i++)
          if (nd++ < 9)
            y *= 10;
          else if (nd <= DBL_DIG + 1)
            z *= 10;
        if (nd++ < 9)
          y = 10 * y + c;
        else if (nd <= DBL_DIG + 1)
          z = 10 * z + c;
        nz = 0;
      }
    }
  }
dig_done:
  e = 0;
  if (c == 'e' || c == 'E') {
    if (!nd && !nz && !nz0) {
      irv = STRTOG_NoNumber;
      s = s00;
      goto ret;
    }
    s00 = s;
    esign = 0;
    switch (c = *++s) {
    case '-':
      esign = 1;
    case '+':
      c = *++s;
    }
    if (c >= '0' && c <= '9') {
      while (c == '0')
        c = *++s;
      if (c > '0' && c <= '9') {
        L = c - '0';
        s1 = s;
        while ((c = *++s) >= '0' && c <= '9')
          L = 10 * L + c - '0';
        if (s - s1 > 8 || L > 19999)
          /* Avoid confusion from exponents
           * so large that e might overflow.
           */
          e = 19999; /* safe for 16 bit ints */
        else
          e = (int)L;
        if (esign)
          e = -e;
      } else
        e = 0;
    } else
      s = s00;
  }
  if (!nd) {
    if (!nz && !nz0) {
#ifdef INFNAN_CHECK
      /* Check for Nan and Infinity */
      if (!decpt)
        switch (c) {
        case 'i':
        case 'I':
          if (match(&s, "nf")) {
            --s;
            if (!match(&s, "inity"))
              ++s;
            irv = STRTOG_Infinite;
            goto infnanexp;
          }
          break;
        case 'n':
        case 'N':
          if (match(&s, "an")) {
            irv = STRTOG_NaN;
            *exp = fpi->emax + 1;
#ifndef No_Hex_NaN
            if (*s == '(') /*)*/
              irv = hexnan(&s, fpi, bits);
#endif
            goto infnanexp;
          }
        }
#endif /* INFNAN_CHECK */
      irv = STRTOG_NoNumber;
      s = s00;
    }
    goto ret;
  }

  irv = STRTOG_Normal;
  e1 = e -= nf;
  rd = 0;
  switch (fpi->rounding & 3) {
  case FPI_Round_up:
    rd = 2 - sign;
    break;
  case FPI_Round_zero:
    rd = 1;
    break;
  case FPI_Round_down:
    rd = 1 + sign;
  }

  /* Now we have nd0 digits, starting at s0, followed by a
   * decimal point, followed by nd-nd0 digits.  The number we're
   * after is the integer represented by those digits times
   * 10**e */

  if (!nd0)
    nd0 = nd;
  k = nd < DBL_DIG + 1 ? nd : DBL_DIG + 1;
  dval(rv) = y;
  if (k > 9)
    dval(rv) = tens[k - 9] * dval(rv) + z;
  bd0 = 0;

  // this section for float and double

  if (nbits <= P && nd <= DBL_DIG) {
    if (!e) {
      if (rvOK_D2A(dval(rv), fpi, exp, bits, 1, rd, &irv)) {
        goto ret;
      }
    } else if (e > 0) {
      if (e <= Ten_pmax) {
#ifdef VAX
        goto vax_ovfl_check;
#else
        i = fivesbits[e] + mantbits_D2A(dval(rv)) <= P;
        /* rv = */ rounded_product(dval(rv), tens[e]);
        if (rvOK_D2A(dval(rv), fpi, exp, bits, i, rd, &irv))
          goto ret;
        e1 -= e;
        goto rv_notOK;
#endif
      }
      i = DBL_DIG - nd;
      if (e <= Ten_pmax + i) {
        /* A fancier test would sometimes let us do
         * this for larger i values.
         */
        e2 = e - i;
        e1 -= i;
        dval(rv) *= tens[i];
#ifdef VAX
        /* VAX exponent range is so narrow we must
         * worry about overflow here...
         */
      vax_ovfl_check:
        dval(adj) = dval(rv);
        word0(adj) -= P * Exp_msk1;
        /* adj = */ rounded_product(dval(adj), tens[e2]);
        if ((word0(adj) & Exp_mask) > Exp_msk1 * (DBL_MAX_EXP + Bias - 1 - P))
          goto rv_notOK;
        word0(adj) += P * Exp_msk1;
        dval(rv) = dval(adj);
#else
        /* rv = */ rounded_product(dval(rv), tens[e2]);
#endif
        if (rvOK_D2A(dval(rv), fpi, exp, bits, 0, rd, &irv))
          goto ret;
        e1 -= e2;
      }
    }
#ifndef Inaccurate_Divide
    else if (e >= -Ten_pmax) {
      /* rv = */ rounded_quotient(dval(rv), tens[-e]);
      if (rvOK_D2A(dval(rv), fpi, exp, bits, 0, rd, &irv))
        goto ret;
      e1 -= e;
    }
#endif
  }
rv_notOK:

  e1 += nd - k;

  /* Get starting approximation = rv * 10**e1 */

  e2 = 0;

  if (e1 > 0) {
    if ((i = e1 & 15) != 0) {
      dval(rv) *= tens[i];
    }
    if (e1 &= ~15) {
      e1 >>= 4;
      while (e1 >= (1 << n_bigtens - 1)) {
        e2 += ((word0(rv) & Exp_mask) >> Exp_shift1) - Bias;
        word0(rv) &= ~Exp_mask;
        word0(rv) |= Bias << Exp_shift1;
        dval(rv) *= bigtens[n_bigtens - 1];
        e1 -= 1 << n_bigtens - 1;
      }
      e2 += ((word0(rv) & Exp_mask) >> Exp_shift1) - Bias;
      word0(rv) &= ~Exp_mask;
      word0(rv) |= Bias << Exp_shift1;
      for (j = 0; e1 > 0; j++, e1 >>= 1)
        if (e1 & 1)
          dval(rv) *= bigtens[j];
    }
  } else if (e1 < 0) {
    e1 = -e1;
    if ((i = e1 & 15) != 0) {
      dval(rv) /= tens[i];
    }
    if (e1 &= ~15) {
      e1 >>= 4;
      while (e1 >= (1 << n_bigtens - 1)) {
        e2 += ((word0(rv) & Exp_mask) >> Exp_shift1) - Bias;
        word0(rv) &= ~Exp_mask;
        word0(rv) |= Bias << Exp_shift1;
        dval(rv) *= tinytens[n_bigtens - 1];
        e1 -= 1 << n_bigtens - 1;
      }

      e2 += ((word0(rv) & Exp_mask) >> Exp_shift1) - Bias;
      word0(rv) &= ~Exp_mask;
      word0(rv) |= Bias << Exp_shift1;
      for (j = 0; e1 > 0; j++, e1 >>= 1)
        if (e1 & 1)
          dval(rv) *= tinytens[j];
    }
  }

#ifdef IBM
  /* e2 is a correction to the (base 2) exponent of the return
   * value, reflecting adjustments above to avoid overflow in the
   * native arithmetic.  For native IBM (base 16) arithmetic, we
   * must multiply e2 by 4 to change from base 16 to 2.
   */
  e2 <<= 2;
#endif

  rvb = d2b_D2A(dval(rv), &rve, &rvbits); /* rv = rvb * 2^rve */

  rve += e2;
  if ((j = rvbits - nbits) > 0) {
    rshift_D2A(rvb, j);
    rvbits = nbits;
    rve += j;
  }
  bb0 = 0; /* trailing zero bits in rvb */
  e2 = rve + rvbits - nbits;

  if (e2 > fpi->emax + 1) {
    goto huge;
  }
  rve1 = rve + rvbits - nbits;

  if (e2 < (emin = fpi->emin)) {
    denorm = 1;
    j = rve - emin;
    if (j > 0) {
      rvb = lshift_D2A(rvb, j);
      rvbits += j;
    } else if (j < 0) {
      rvbits += j;
      if (rvbits <= 0) {
        if (rvbits < -1) {
        ufl:
          rvb->wds = 0;
          rvb->x[0] = 0;
          *exp = emin;
          irv = STRTOG_Underflow | STRTOG_Inexlo;
          goto ret;
        }
        rvb->x[0] = rvb->wds = rvbits = 1;
      } else {
        rshift_D2A(rvb, -j);
      }
    }
    rve = rve1 = emin;
    if (sudden_underflow && e2 + 1 < emin)
      goto ufl;
  }

  /* Now the hard part -- adjusting rv to the correct value.*/

  /* Put digits into bd: true value = bd * 10^e */

  bd0 = s2b_D2A(s0, nd0, nd, y);
  for (;;) {

    bd = Balloc_D2A(bd0->k);
    Bcopy_D2A(bd, bd0);
    bb = Balloc_D2A(rvb->k);
    Bcopy_D2A(bb, rvb);
    bbbits = rvbits - bb0;
    bbe = rve + bb0;
    bs = i2b_D2A(1);

    if (e >= 0) {
      bb2 = bb5 = 0;
      bd2 = bd5 = e;
    } else {
      bb2 = bb5 = -e;
      bd2 = bd5 = 0;
    }
    if (bbe >= 0)
      bb2 += bbe;
    else
      bd2 -= bbe;

    bs2 = bb2;
    j = nbits + 1 - bbbits;
    i = bbe + bbbits - nbits;
    if (i < emin) /* denormal */
      j += i - emin;
    bb2 += j;
    bd2 += j;
    i = bb2 < bd2 ? bb2 : bd2;
    if (i > bs2)
      i = bs2;
    if (i > 0) {
      bb2 -= i;
      bd2 -= i;
      bs2 -= i;
    }
    if (bb5 > 0) {
      bs = pow5mult_D2A(bs, bb5);
      bb1 = mult_D2A(bs, bb);
      Bfree_D2A(bb);
      bb = bb1;
    }
    bb2 -= bb0;
    if (bb2 > 0)
      bb = lshift_D2A(bb, bb2);
    else if (bb2 < 0)
      rshift_D2A(bb, -bb2);

    if (bd5 > 0) {
      bd = pow5mult_D2A(bd, bd5);
    }
    if (bd2 > 0) {
      bd = lshift_D2A(bd, bd2);
    }
    if (bs2 > 0)
      bs = lshift_D2A(bs, bs2);
    asub = 1;
    inex = STRTOG_Inexhi;
    delta = diff_D2A(bb, bd);
    if (delta->wds <= 1 && !delta->x[0]) {
      break;
    }

    dsign = delta->sign;

    delta->sign = finished = 0;
    L = 0;

    i = cmp_D2A(delta, bs);
    if (rd && i <= 0) {
      irv = STRTOG_Normal;
      if ((finished = dsign ^ (rd & 1)) != 0) {
        if (dsign != 0) {
          irv |= STRTOG_Inexhi;
          goto adj1;
        }
        irv |= STRTOG_Inexlo;
        if (rve1 == emin)
          goto adj1;

        for (i = 0, j = nbits; j >= ULbits; i++, j -= ULbits) {
          if (rvb->x[i] & ALL_ON)
            goto adj1;
        }
        if (j > 1 && lo0bits_D2A(rvb->x + i) < j - 1)
          goto adj1;
        rve = rve1 - 1;
        rvb = set_ones_D2A(rvb, rvbits = nbits);
        break;
      }
      irv |= dsign ? STRTOG_Inexlo : STRTOG_Inexhi;
      break;
    }
    if (i < 0) {
      /* Error is less than half an ulp -- check for
       * special case of mantissa a power of two.
       */
      irv =
          dsign ? STRTOG_Normal | STRTOG_Inexlo : STRTOG_Normal | STRTOG_Inexhi;
      if (dsign || bbbits > 1 || denorm || rve1 == emin)
        break;

      delta = lshift_D2A(delta, 1);
      if (cmp_D2A(delta, bs) > 0) {
        irv = STRTOG_Normal | STRTOG_Inexlo;
        goto drop_down;
      }
      break;
    }
    if (i == 0) {
      /* exactly half-way between */
      if (dsign) {
        if (denorm && all_on_D2A(rvb, rvbits)) {
          /*boundary case -- increment exponent*/
          rvb->wds = 1;
          rvb->x[0] = 1;
          rve = emin + nbits - (rvbits = 1);
          irv = STRTOG_Normal | STRTOG_Inexhi;
          denorm = 0;
          break;
        }
        irv = STRTOG_Normal | STRTOG_Inexlo;
      } else if (bbbits == 1) {
        irv = STRTOG_Normal;
      drop_down:
        /* boundary case -- decrement exponent */
        if (rve1 == emin) {
          irv = STRTOG_Normal | STRTOG_Inexhi;
          if (rvb->wds == 1 && rvb->x[0] == 1)
            sudden_underflow = 1;
          break;
        }
        rve -= nbits;
        rvb = set_ones_D2A(rvb, rvbits = nbits);
        break;
      } else
        irv = STRTOG_Normal | STRTOG_Inexhi;

      if (bbbits < nbits && !denorm || !(rvb->x[0] & 1))
        break;
      if (dsign) {
        rvb = increment_D2A(rvb);
        j = kmask & (ULbits - (rvbits & kmask));
        if (hi0bits_D2A(rvb->x[rvb->wds - 1]) != j) {
          rvbits++;
        }
        irv = STRTOG_Normal | STRTOG_Inexhi;
      } else {
        if (bbbits == 1)
          goto undfl;
        decrement_D2A(rvb);
        irv = STRTOG_Normal | STRTOG_Inexlo;
      }
      break;
    }
    if ((dval(adj) = ratio_D2A(delta, bs)) <= 2.) {
    adj1:

      inex = STRTOG_Inexlo;
      if (dsign) {
        asub = 0;
        inex = STRTOG_Inexhi;
      } else if (denorm && bbbits <= 1) {
      undfl:
        rvb->wds = 0;
        rve = emin;
        irv = STRTOG_Underflow | STRTOG_Inexlo;
        break;
      }
      adj0 = dval(adj) = 1.;
    } else {

      adj0 = dval(adj) *= 0.5;
      if (dsign) {
        asub = 0;
        inex = STRTOG_Inexlo;
      }
      if (dval(adj) < 2147483647.) {
        L = adj0;
        adj0 -= L;
        switch (rd) {
        case 0:
          if (adj0 >= .5)
            goto inc_L;
          break;
        case 1:
          if (asub && adj0 > 0.)
            goto inc_L;
          break;
        case 2:
          if (!asub && adj0 > 0.) {
          inc_L:
            L++;
            inex = STRTOG_Inexact - inex;
          }
        }
        dval(adj) = L;
      }
    }
    y = rve + rvbits;

    /* adj *= ulp(dval(rv)); */
    /* if (asub) rv -= adj; else rv += adj; */

    if (!denorm && rvbits < nbits) {
      rvb = lshift_D2A(rvb, j = nbits - rvbits);
      rve -= j;
      rvbits = nbits;
    }
    ab = d2b_D2A(dval(adj), &abe, &abits);
    if (abe < 0)
      rshift_D2A(ab, -abe);
    else if (abe > 0)
      ab = lshift_D2A(ab, abe);

    rvb0 = rvb;
    if (asub) {
      /* rv -= adj; */
      rvb = diff_D2A(rvb, ab);

      k = rvb0->wds - 1;

      if (denorm)
        /* do nothing */;
      else if (rvb->wds <= k ||
               hi0bits_D2A(rvb->x[k]) > hi0bits_D2A(rvb0->x[k])) {
        /* unlikely; can only have lost 1 high bit */
        if (rve1 == emin) {
          --rvbits;
          denorm = 1;
        } else {
          rvb = lshift_D2A(rvb, 1);
          --rve;
          --rve1;
          L = finished = 0;
        }
      }
    } else {
      rvb = sum_D2A(rvb, ab);

      k = rvb->wds - 1;

      if (k >= rvb0->wds || hi0bits_D2A(rvb->x[k]) < hi0bits_D2A(rvb0->x[k])) {
        if (denorm) {
          if (++rvbits == nbits)
            denorm = 0;
        } else {
          rshift_D2A(rvb, 1);
          rve++;
          rve1++;
          L = 0;
        }
      }
    }
    Bfree_D2A(ab);
    Bfree_D2A(rvb0);

    if (finished)
      break;

    z = rve + rvbits;
    if (y == z && L) {
      /* Can we stop now? */
      tol = dval(adj) * 5e-16; /* > max rel error */
      dval(adj) = adj0 - .5;
      if (dval(adj) < -tol) {
        if (adj0 > tol) {
          irv |= inex;
          break;
        }
      } else if (dval(adj) > tol && adj0 < 1. - tol) {
        irv |= inex;
        break;
      }
    }

    bb0 = denorm ? 0 : trailz_D2A(rvb);
    Bfree_D2A(bb);
    Bfree_D2A(bd);
    Bfree_D2A(bs);
    Bfree_D2A(delta);
  } // end main loop

  if (!denorm && (j = nbits - rvbits)) {
    if (j > 0)
      rvb = lshift_D2A(rvb, j);
    else
      rshift_D2A(rvb, -j);

    rve -= j;
  }
  *exp = rve;
  Bfree_D2A(bb);
  Bfree_D2A(bd);
  Bfree_D2A(bs);
  Bfree_D2A(bd0);
  Bfree_D2A(delta);
  if (rve > fpi->emax) {
  huge:
    rvb->wds = 0;
    irv = STRTOG_Infinite | STRTOG_Overflow | STRTOG_Inexhi;
#ifndef NO_ERRNO
    errno = ERANGE;
#endif
  infnanexp:
    *exp = fpi->emax + 1;
  }
ret:
  if (denorm) {
    if (sudden_underflow) {
      rvb->wds = 0;
      irv = STRTOG_Underflow | STRTOG_Inexlo;
    } else {
      irv = (irv & ~STRTOG_Retmask) |
            (rvb->wds > 0 ? STRTOG_Denormal : STRTOG_Zero);
      if (irv & STRTOG_Inexact)
        irv |= STRTOG_Underflow;
    }
  }
  if (se)
    *se = (char *)s;
  if (sign)
    irv |= STRTOG_Neg;
  if (rvb) {
    copybits_D2A(bits, nbits, rvb);
    Bfree_D2A(rvb);
  }
  return irv;
}

// gdtoa.c

static Bigint *bitstob_D2A(ULong *bits, int nbits, int *bbits) {
  int i, k;
  Bigint *b;
  ULong *be, *x, *x0;

  i = ULbits;
  k = 0;
  while (i < nbits) {
    i <<= 1;
    k++;
  }
#ifndef Pack_32
  if (!k)
    k = 1;
#endif
  b = Balloc_D2A(k);
  be = bits + ((nbits - 1) >> kshift);
  x = x0 = b->x;
  do {
    *x++ = *bits & ALL_ON;
#ifdef Pack_16
    *x++ = (*bits >> 16) & ALL_ON;
#endif
  } while (++bits <= be);
  i = x - x0;
  while (!x0[--i])
    if (!i) {
      b->wds = 0;
      *bbits = 0;
      goto ret;
    }
  b->wds = i + 1;
  *bbits = i * ULbits + 32 - hi0bits_D2A(b->x[i]);
ret:
  return b;
}

/* dtoa for IEEE arithmetic (dmg): convert double to ASCII string.
 *
 * Inspired by "How to Print Floating-Point Numbers Accurately" by
 * Guy L. Steele, Jr. and Jon L. White [Proc. ACM SIGPLAN '90, pp. 112-126].
 *
 * Modifications:
 *	1. Rather than iterating, we use a simple numeric overestimate
 *	   to determine k = floor(log10(d)).  We scale relevant
 *	   quantities using O(log2(k)) rather than O(k) multiplications.
 *	2. For some modes > 2 (corresponding to ecvt and fcvt), we don't
 *	   try to generate digits strictly left to right.  Instead, we
 *	   compute with fewer bits and propagate the carry if necessary
 *	   when rounding the final digit up.  This is often faster.
 *	3. Under the assumption that input will be rounded nearest,
 *	   mode 0 renders 1e23 as 1e23 rather than 9.999999999999999e22.
 *	   That is, we allow equality in stopping tests when the
 *	   round-nearest rule will give the same floating-point value
 *	   as would satisfaction of the stopping test with strict
 *	   inequality.
 *	4. We remove common factors of powers of 2 from relevant
 *	   quantities.
 *	5. When converting floating-point integers less than 1e16,
 *	   we use floating-point arithmetic rather than resorting
 *	   to multiple-precision integers.
 *	6. When asked to produce fewer than 15 digits, we first try
 *	   to get by with floating-point arithmetic; we resort to
 *	   multiple-precision integer arithmetic only if we cannot
 *	   guarantee that the floating-point calculation has given
 *	   the correctly rounded result.  For k requested digits and
 *	   "uniformly" distributed input, the probability is
 *	   something like 10^(k-15) that we must resort to the Long
 *	   calculation.
 */

char *gdtoa_D2A(FPI *fpi, int be, ULong *bits, int *kindp, int mode,
                int ndigits, int *decpt, char **rve) {
  /*	Arguments ndigits and decpt are similar to the second and third
         arguments of ecvt and fcvt; trailing zeros are suppressed from
         the returned string.  If not null, *rve is set to point
         to the end of the return value.  If d is +-Infinity or NaN,
         then *decpt is set to 9999.

         mode:
                 0 ==> shortest string that yields d when read in
                         and rounded to nearest.
                 1 ==> like 0, but with Steele & White stopping rule;
                         e.g. with IEEE P754 arithmetic , mode 0 gives
                         1e23 whereas mode 1 gives 9.999999999999999e22.
                 2 ==> max(1,ndigits) significant digits.  This gives a
                         return value similar to that of ecvt, except
                         that trailing zeros are suppressed.
                 3 ==> through ndigits past the decimal point.  This
                         gives a return value similar to that from fcvt,
                         except that trailing zeros are suppressed, and
                         ndigits can be negative.
                 4-9 should give the same return values as 2-3, i.e.,
                         4 <= mode <= 9 ==> same return as mode
                         2 + (mode & 1).  These modes are mainly for
                         debugging; often they run slower but sometimes
                         faster than modes 2-3.
                 4,5,8,9 ==> left-to-right digit generation.
                 6-9 ==> don't try fast floating-point estimate
                         (if applicable).

                 Values of mode other than 0-9 are treated as mode 0.

                 Sufficient space is allocated to the return value
                 to hold the suppressed trailing zeros.
         */

  int bbits, b2, b5, be0, dig, i, ieps, ilim, ilim0, ilim1, inex;
  int j, j1, k, k0, k_check, kind, leftright, m2, m5, nbits;
  int rdir, s2, s5, spec_case, try_quick;
  Long L;
  Bigint *b, *b1, *delta, *mlo, *mhi, *mhi1, *S;
  double d, d2, ds, eps;
  char *s, *s0;

#ifndef MULTIPLE_THREADS
  if (dtoa_result) {
    freedtoa_D2A(dtoa_result);
    dtoa_result = 0;
  }
#endif
  inex = 0;
  kind = *kindp &= ~STRTOG_Inexact;
  switch (kind & STRTOG_Retmask) {
  case STRTOG_Zero:
    goto ret_zero;
  case STRTOG_Normal:
  case STRTOG_Denormal:
    break;
  case STRTOG_Infinite:
    *decpt = -32768;
    return nrv_alloc_D2A("Infinity", rve, 8);
  case STRTOG_NaN:
    *decpt = -32768;
    return nrv_alloc_D2A("NaN", rve, 3);
  default:
    return 0;
  }
  b = bitstob_D2A(bits, nbits = fpi->nbits, &bbits);
  be0 = be;
  if ((i = trailz_D2A(b)) != 0) {
    rshift_D2A(b, i);
    be += i;
    bbits -= i;
  }
  if (!b->wds) {
    Bfree_D2A(b);
  ret_zero:
    *decpt = 1;
    return nrv_alloc_D2A("0", rve, 1);
  }

  dval(d) = b2d_D2A(b, &i);
  i = be + bbits - 1;
  word0(d) &= Frac_mask1;
  word0(d) |= Exp_11;
#ifdef IBM
  if ((j = 11 - hi0bits_D2A(word0(d) & Frac_mask)) != 0)
    dval(d) /= 1 << j;
#endif

    /* log(x)	~=~ log(1.5) + (x-1.5)/1.5
     * log10(x)	 =  log(x) / log(10)
     *		~=~ log(1.5)/log(10) + (x-1.5)/(1.5*log(10))
     * log10(d) = (i-Bias)*log(2)/log(10) + log10(d2)
     *
     * This suggests computing an approximation k to log10(d) by
     *
     * k = (i - Bias)*0.301029995663981
     *	+ ( (d2-1.5)*0.289529654602168 + 0.176091259055681 );
     *
     * We want k to be too large rather than too small.
     * The error in the first-order Taylor series approximation
     * is in our favor, so we just round up the constant enough
     * to compensate for any error in the multiplication of
     * (i - Bias) by 0.301029995663981; since |i - Bias| <= 1077,
     * and 1077 * 0.30103 * 2^-52 ~=~ 7.2e-14,
     * adding 1e-13 to the constant term more than suffices.
     * Hence we adjust the constant term to 0.1760912590558.
     * (We could get a more accurate k by invoking log10,
     *  but this is probably not worthwhile.)
     */
#ifdef IBM
  i <<= 2;
  i += j;
#endif
  ds = (dval(d) - 1.5) * 0.289529654602168 + 0.1760912590558 +
       i * 0.301029995663981;

  /* correct assumption about exponent range */
  if ((j = i) < 0)
    j = -j;
  if ((j -= 1077) > 0)
    ds += j * 7e-17;

  k = (int)ds;
  if (ds < 0. && ds != k)
    k--; /* want k = floor(ds) */
  k_check = 1;
#ifdef IBM
  j = be + bbits - 1;
  if ((j1 = j & 3) != 0)
    dval(d) *= 1 << j1;
  word0(d) += j << Exp_shift - 2 & Exp_mask;
#else
  word0(d) += (be + bbits - 1) << Exp_shift;
#endif
  if (k >= 0 && k <= Ten_pmax) {
    if (dval(d) < tens[k])
      k--;
    k_check = 0;
  }
  j = bbits - i - 1;
  if (j >= 0) {
    b2 = 0;
    s2 = j;
  } else {
    b2 = -j;
    s2 = 0;
  }
  if (k >= 0) {
    b5 = 0;
    s5 = k;
    s2 += k;
  } else {
    b2 -= k;
    b5 = -k;
    s5 = 0;
  }
  if (mode < 0 || mode > 9)
    mode = 0;
  try_quick = 1;
  if (mode > 5) {
    mode -= 4;
    try_quick = 0;
  }
  leftright = 1;
  switch (mode) {
  case 0:
  case 1:
    ilim = ilim1 = -1;
    i = (int)(nbits * .30103) + 3;
    ndigits = 0;
    break;
  case 2:
    leftright = 0;
    /* no break */
  case 4:
    if (ndigits <= 0)
      ndigits = 1;
    ilim = ilim1 = i = ndigits;
    break;
  case 3:
    leftright = 0;
    /* no break */
  case 5:
    i = ndigits + k + 1;
    ilim = i;
    ilim1 = i - 1;
    if (i <= 0)
      i = 1;
  }
  s = s0 = rv_alloc_D2A(i);

  if ((rdir = fpi->rounding - 1) != 0) {
    if (rdir < 0)
      rdir = 2;
    if (kind & STRTOG_Neg)
      rdir = 3 - rdir;
  }

  /* Now rdir = 0 ==> round near, 1 ==> round up, 2 ==> round down. */

  if (ilim >= 0 && ilim <= Quick_max && try_quick && !rdir
#ifndef IMPRECISE_INEXACT
      && k == 0
#endif
  ) {

    /* Try to get by with floating-point arithmetic. */

    i = 0;
    d2 = dval(d);
#ifdef IBM
    if ((j = 11 - hi0bits_D2A(word0(d) & Frac_mask)) != 0)
      dval(d) /= 1 << j;
#endif
    k0 = k;
    ilim0 = ilim;
    ieps = 2; /* conservative */
    if (k > 0) {
      ds = tens[k & 0xf];
      j = k >> 4;
      if (j & Bletch) {
        /* prevent overflows */
        j &= Bletch - 1;
        dval(d) /= bigtens[n_bigtens - 1];
        ieps++;
      }
      for (; j; j >>= 1, i++)
        if (j & 1) {
          ieps++;
          ds *= bigtens[i];
        }
    } else {
      ds = 1.;
      if ((j1 = -k) != 0) {
        dval(d) *= tens[j1 & 0xf];
        for (j = j1 >> 4; j; j >>= 1, i++)
          if (j & 1) {
            ieps++;
            dval(d) *= bigtens[i];
          }
      }
    }
    if (k_check && dval(d) < 1. && ilim > 0) {
      if (ilim1 <= 0)
        goto fast_failed;
      ilim = ilim1;
      k--;
      dval(d) *= 10.;
      ieps++;
    }
    dval(eps) = ieps * dval(d) + 7.;
    word0(eps) -= (P - 1) * Exp_msk1;
    if (ilim == 0) {
      S = mhi = 0;
      dval(d) -= 5.;
      if (dval(d) > dval(eps))
        goto one_digit;
      if (dval(d) < -dval(eps))
        goto no_digits;
      goto fast_failed;
    }
#ifndef No_leftright
    if (leftright) {
      /* Use Steele & White method of only
       * generating digits needed.
       */
      dval(eps) = ds * 0.5 / tens[ilim - 1] - dval(eps);
      for (i = 0;;) {
        L = (Long)(dval(d) / ds);
        dval(d) -= L * ds;
        *s++ = '0' + (int)L;
        if (dval(d) < dval(eps)) {
          if (dval(d))
            inex = STRTOG_Inexlo;
          goto ret1;
        }
        if (ds - dval(d) < dval(eps))
          goto bump_up;
        if (++i >= ilim)
          break;
        dval(eps) *= 10.;
        dval(d) *= 10.;
      }
    } else {
#endif
      /* Generate ilim digits, then fix them up. */
      dval(eps) *= tens[ilim - 1];
      for (i = 1;; i++, dval(d) *= 10.) {
        if ((L = (Long)(dval(d) / ds)) != 0)
          dval(d) -= L * ds;
        *s++ = '0' + (int)L;
        if (i == ilim) {
          ds *= 0.5;
          if (dval(d) > ds + dval(eps))
            goto bump_up;
          else if (dval(d) < ds - dval(eps)) {
            while (*--s == '0') {
            }
            s++;
            if (dval(d))
              inex = STRTOG_Inexlo;
            goto ret1;
          }
          break;
        }
      }
#ifndef No_leftright
    }
#endif
  fast_failed:
    s = s0;
    dval(d) = d2;
    k = k0;
    ilim = ilim0;
  }

  /* Do we have a "small" integer? */

  if (be >= 0 && k <= Int_max) {
    /* Yes. */
    ds = tens[k];
    if (ndigits < 0 && ilim <= 0) {
      S = mhi = 0;
      if (ilim < 0 || dval(d) <= 5 * ds)
        goto no_digits;
      goto one_digit;
    }
    for (i = 1;; i++, dval(d) *= 10.) {
      L = dval(d) / ds;
      dval(d) -= L * ds;
#ifdef Check_FLT_ROUNDS
      /* If FLT_ROUNDS == 2, L will usually be high by 1 */
      if (dval(d) < 0) {
        L--;
        dval(d) += ds;
      }
#endif
      *s++ = '0' + (int)L;
      if (dval(d) == 0.)
        break;
      if (i == ilim) {
        if (rdir) {
          if (rdir == 1)
            goto bump_up;
          inex = STRTOG_Inexlo;
          goto ret1;
        }
        dval(d) += dval(d);
        if (dval(d) > ds || dval(d) == ds && L & 1) {
        bump_up:
          inex = STRTOG_Inexhi;
          while (*--s == '9')
            if (s == s0) {
              k++;
              *s = '0';
              break;
            }
          ++*s++;
        } else
          inex = STRTOG_Inexlo;
        break;
      }
    }
    goto ret1;
  }

  m2 = b2;
  m5 = b5;
  mhi = mlo = 0;
  if (leftright) {
    if (mode < 2) {
      i = nbits - bbits;
      if (be - i++ < fpi->emin)
        /* denormal */
        i = be - fpi->emin + 1;
    } else {
      j = ilim - 1;
      if (m5 >= j)
        m5 -= j;
      else {
        s5 += j -= m5;
        b5 += j;
        m5 = 0;
      }
      if ((i = ilim) < 0) {
        m2 -= i;
        i = 0;
      }
    }
    b2 += i;
    s2 += i;
    mhi = i2b_D2A(1);
  }
  if (m2 > 0 && s2 > 0) {
    i = m2 < s2 ? m2 : s2;
    b2 -= i;
    m2 -= i;
    s2 -= i;
  }
  if (b5 > 0) {
    if (leftright) {
      if (m5 > 0) {
        mhi = pow5mult_D2A(mhi, m5);
        b1 = mult_D2A(mhi, b);
        Bfree_D2A(b);
        b = b1;
      }
      if ((j = b5 - m5) != 0)
        b = pow5mult_D2A(b, j);
    } else
      b = pow5mult_D2A(b, b5);
  }
  S = i2b_D2A(1);
  if (s5 > 0)
    S = pow5mult_D2A(S, s5);

  /* Check for special case that d is a normalized power of 2. */

  spec_case = 0;
  if (mode < 2) {
    if (bbits == 1 && be0 > fpi->emin + 1) {
      /* The special case */
      b2++;
      s2++;
      spec_case = 1;
    }
  }

  /* Arrange for convenient computation of quotients:
   * shift left if necessary so divisor has 4 leading 0 bits.
   *
   * Perhaps we should just compute leading 28 bits of S once
   * and for all and pass them and a shift to quorem, so it
   * can do shifts and ors to compute the numerator for q.
   */
#ifdef Pack_32
  if ((i = ((s5 ? 32 - hi0bits_D2A(S->x[S->wds - 1]) : 1) + s2) & 0x1f) != 0)
    i = 32 - i;
#else
  if ((i = ((s5 ? 32 - hi0bits_D2A(S->x[S->wds - 1]) : 1) + s2) & 0xf) != 0)
    i = 16 - i;
#endif
  if (i > 4) {
    i -= 4;
    b2 += i;
    m2 += i;
    s2 += i;
  } else if (i < 4) {
    i += 28;
    b2 += i;
    m2 += i;
    s2 += i;
  }
  if (b2 > 0)
    b = lshift_D2A(b, b2);
  if (s2 > 0)
    S = lshift_D2A(S, s2);
  if (k_check) {
    if (cmp_D2A(b, S) < 0) {
      k--;
      b = multadd_D2A(b, 10, 0); /* we botched the k estimate */
      if (leftright)
        mhi = multadd_D2A(mhi, 10, 0);
      ilim = ilim1;
    }
  }
  if (ilim <= 0 && mode > 2) {
    if (ilim < 0 || cmp_D2A(b, S = multadd_D2A(S, 5, 0)) <= 0) {
      /* no digits, fcvt style */
    no_digits:
      k = -1 - ndigits;
      inex = STRTOG_Inexlo;
      goto ret;
    }
  one_digit:
    inex = STRTOG_Inexhi;
    *s++ = '1';
    k++;
    goto ret;
  }
  if (leftright) {
    if (m2 > 0)
      mhi = lshift_D2A(mhi, m2);

    /* Compute mlo -- check for special case
     * that d is a normalized power of 2.
     */

    mlo = mhi;
    if (spec_case) {
      mhi = Balloc_D2A(mhi->k);
      Bcopy_D2A(mhi, mlo);
      mhi = lshift_D2A(mhi, 1);
    }

    for (i = 1;; i++) {
      dig = quorem_D2A(b, S) + '0';
      /* Do we yet have the shortest decimal string
       * that will round to d?
       */
      j = cmp_D2A(b, mlo);
      delta = diff_D2A(S, mhi);
      j1 = delta->sign ? 1 : cmp_D2A(b, delta);
      Bfree_D2A(delta);
#ifndef ROUND_BIASED
      if (j1 == 0 && !mode && !(bits[0] & 1) && !rdir) {
        if (dig == '9')
          goto round_9_up;
        if (j <= 0) {
          if (b->wds > 1 || b->x[0])
            inex = STRTOG_Inexlo;
        } else {
          dig++;
          inex = STRTOG_Inexhi;
        }
        *s++ = dig;
        goto ret;
      }
#endif
      if (j < 0 || j == 0 && !mode
#ifndef ROUND_BIASED
                       && !(bits[0] & 1)
#endif
      ) {
        if (rdir && (b->wds > 1 || b->x[0])) {
          if (rdir == 2) {
            inex = STRTOG_Inexlo;
            goto accept;
          }
          while (cmp_D2A(S, mhi) > 0) {
            *s++ = dig;
            mhi1 = multadd_D2A(mhi, 10, 0);
            if (mlo == mhi)
              mlo = mhi1;
            mhi = mhi1;
            b = multadd_D2A(b, 10, 0);
            dig = quorem_D2A(b, S) + '0';
          }
          if (dig++ == '9')
            goto round_9_up;
          inex = STRTOG_Inexhi;
          goto accept;
        }
        if (j1 > 0) {
          b = lshift_D2A(b, 1);
          j1 = cmp_D2A(b, S);
          if ((j1 > 0 || j1 == 0 && dig & 1) && dig++ == '9')
            goto round_9_up;
          inex = STRTOG_Inexhi;
        }
        if (b->wds > 1 || b->x[0])
          inex = STRTOG_Inexlo;
      accept:
        *s++ = dig;
        goto ret;
      }
      if (j1 > 0 && rdir != 2) {
        if (dig == '9') { /* possible if i == 1 */
        round_9_up:
          *s++ = '9';
          inex = STRTOG_Inexhi;
          goto roundoff;
        }
        inex = STRTOG_Inexhi;
        *s++ = dig + 1;
        goto ret;
      }
      *s++ = dig;
      if (i == ilim)
        break;
      b = multadd_D2A(b, 10, 0);
      if (mlo == mhi)
        mlo = mhi = multadd_D2A(mhi, 10, 0);
      else {
        mlo = multadd_D2A(mlo, 10, 0);
        mhi = multadd_D2A(mhi, 10, 0);
      }
    }
  } else
    for (i = 1;; i++) {
      *s++ = dig = quorem_D2A(b, S) + '0';
      if (i >= ilim)
        break;
      b = multadd_D2A(b, 10, 0);
    }

  /* Round off last digit */

  if (rdir) {
    if (rdir == 2 || b->wds <= 1 && !b->x[0])
      goto chopzeros;
    goto roundoff;
  }
  b = lshift_D2A(b, 1);
  j = cmp_D2A(b, S);
  if (j > 0 || j == 0 && dig & 1) {
  roundoff:
    inex = STRTOG_Inexhi;
    while (*--s == '9')
      if (s == s0) {
        k++;
        *s++ = '1';
        goto ret;
      }
    ++*s++;
  } else {
  chopzeros:
    if (b->wds > 1 || b->x[0])
      inex = STRTOG_Inexlo;
    while (*--s == '0') {
    }
    s++;
  }
ret:
  Bfree_D2A(S);
  if (mhi) {
    if (mlo && mlo != mhi)
      Bfree_D2A(mlo);
    Bfree_D2A(mhi);
  }
ret1:
  Bfree_D2A(b);
  *s = 0;
  *decpt = k + 1;
  if (rve)
    *rve = s;
  *kindp |= inex;
  return s0;
}

// binary to decimal -----------------------------------------

// binary to decimal helper

char *g__fmt(char *b, char *s, char *se, int decpt, ULong sign) {
  int i, j, k;
  char *s0 = s;
#ifdef USE_LOCALE
  char decimalpoint = *localeconv()->decimal_point;
#else
#define decimalpoint '.'
#endif
  if (sign)
    *b++ = '-';
  if (decpt <= -4 || decpt > se - s + 5) {
    *b++ = *s++;
    if (*s) {
      *b++ = decimalpoint;
      while ((*b = *s++) != 0)
        b++;
    }
    *b++ = 'e';
    /* sprintf(b, "%+.2d", decpt - 1); */
    if (--decpt < 0) {
      *b++ = '-';
      decpt = -decpt;
    } else
      *b++ = '+';
    for (j = 2, k = 10; 10 * k <= decpt; j++, k *= 10) {
    }
    for (;;) {
      i = decpt / k;
      *b++ = i + '0';
      if (--j <= 0)
        break;
      decpt -= i * k;
      decpt *= 10;
    }
    *b = 0;
  } else if (decpt <= 0) {
    *b++ = decimalpoint;
    for (; decpt < 0; decpt++)
      *b++ = '0';
    while ((*b = *s++) != 0)
      b++;
  } else {
    while ((*b = *s++) != 0) {
      b++;
      if (--decpt == 0 && *s)
        *b++ = decimalpoint;
    }
    for (; decpt > 0; decpt--)
      *b++ = '0';
    *b = 0;
  }
  freedtoa_D2A(s0);
  return b;
}

// binary float to decimal

char *g_ffmt(char *buf, float *f, int ndig, unsigned bufsize) {
  static FPI fpi = {24, 1 - 127 - 24 + 1, 254 - 127 - 24 + 1, 1, 0};
  char *b, *s, *se;
  ULong bits[1], *L, sign;
  int decpt, ex, i, mode;

  if (ndig < 0)
    ndig = 0;
  if (bufsize < ndig + 10)
    return 0;

  L = (ULong *)f;
  sign = L[0] & 0x80000000L;
  if ((L[0] & 0x7f800000) == 0x7f800000) {
    /* Infinity or NaN */
    if (L[0] & 0x7fffff) {
      return strcp_D2A(buf, "NaN");
    }
    b = buf;
    if (sign)
      *b++ = '-';
    return strcp_D2A(b, "Infinity");
  }
  if (*f == 0.) {
    b = buf;
#ifndef IGNORE_ZERO_SIGN
    if (L[0] & 0x80000000L)
      *b++ = '-';
#endif
    *b++ = '0';
    *b = 0;
    return b;
  }
  bits[0] = L[0] & 0x7fffff;
  if ((ex = (L[0] >> 23) & 0xff) != 0)
    bits[0] |= 0x800000;
  else
    ex = 1;
  ex -= 0x7f + 23;
  mode = 2;
  if (ndig <= 0) {
    if (bufsize < 16)
      return 0;
    mode = 0;
  }
  i = STRTOG_Normal;
  s = gdtoa_D2A(&fpi, ex, bits, &i, mode, ndig, &decpt, &se);
  return g__fmt(buf, s, se, decpt, sign);
}

char *m3_ftoa0(float *f, int mode, int ndigits, int *decpt, int *asign,
              char **se) {
  static FPI fpi = {24, 1 - 127 - 24 + 1, 254 - 127 - 24 + 1, 1, 0};
  char *b, *s;
  ULong bits[1], *L, sign;
  int ex, i;

  if (ndigits < 0) {
    ndigits = 0;
  }

  L = (ULong *)f;
  sign = L[0] & 0x80000000L;
  *asign = sign;
  if ((L[0] & 0x7f800000) == 0x7f800000) {
    /* Infinity or NaN */
    if (L[0] & 0x7fffff) {
      b = nrv_alloc_D2A("NaN", se, 3);
    } else {
      b = nrv_alloc_D2A("Infinity", se, 8);
    }
    *decpt = -32768;
    return b;
  }

  if (*f == 0.0) {
    b = nrv_alloc_D2A("0", se, 1);
    *decpt = 1;
    return b;
  }

  bits[0] = L[0] & 0x7fffff;
  if ((ex = (L[0] >> 23) & 0xff) != 0)
    bits[0] |= 0x800000;
  else
    ex = 1;
  ex -= 0x7f + 23;
  if (ndigits <= 0) {
    mode = 0;
  }

  i = STRTOG_Normal;
  s = gdtoa_D2A(&fpi, ex, bits, &i, mode, ndigits, decpt, se);
  return s;
}

char *m3_ftoa(float f, int mode, int ndigits,
              int *decpt, int *asign, char **se) {
  return m3_ftoa0(&f, mode, ndigits, decpt, asign, se);
}

// binary double to decimal

char *g_dfmt(char *buf, double *d, int ndig, unsigned bufsize) {
  static FPI fpi = {53, 1 - 1023 - 53 + 1, 2046 - 1023 - 53 + 1, 1, 0};
  char *b, *s, *se;
  ULong bits[2], *L, sign;
  int decpt, ex, i, mode;

  if (ndig < 0)
    ndig = 0;
  if (bufsize < ndig + 10)
    return 0;

  L = (ULong *)d;
  sign = L[_00] & 0x80000000L;
  if ((L[_00] & 0x7ff00000) == 0x7ff00000) {
    /* Infinity or NaN */
    if (L[_00] & 0xfffff || L[_11]) {
      return strcp_D2A(buf, "NaN");
    }
    b = buf;
    if (sign)
      *b++ = '-';
    return strcp_D2A(b, "Infinity");
  }
  if (L[_11] == 0 && (L[_00] ^ sign) == 0 /*d == 0.*/) {
    b = buf;
#ifndef IGNORE_ZERO_SIGN
    if (L[_00] & 0x80000000L)
      *b++ = '-';
#endif
    *b++ = '0';
    *b = 0;
    return b;
  }
  bits[0] = L[_11];
  bits[1] = L[_00] & 0xfffff;
  if ((ex = (L[_00] >> 20) & 0x7ff) != 0)
    bits[1] |= 0x100000;
  else
    ex = 1;
  ex -= 0x3ff + 52;
  mode = 2;
  if (ndig <= 0) {
    if (bufsize < 25)
      return 0;
    mode = 0;
  }
  i = STRTOG_Normal;
  s = gdtoa_D2A(&fpi, ex, bits, &i, mode, ndig, &decpt, &se);
  return g__fmt(buf, s, se, decpt, sign);
}

char *m3_dtoa0(double *d, int mode, int ndigits,
                  int *decpt, int *asign, char **se) {
  static FPI fpi = {53, 1 - 1023 - 53 + 1, 2046 - 1023 - 53 + 1, 1, 0};
  char *b, *s;
  ULong bits[2], *L, sign;
  int ex, i;

  if (ndigits < 0) {
    ndigits = 0;
  }

  L = (ULong *)d;

  sign = L[_00] & 0x80000000L;
  *asign = sign;

  if ((L[_00] & 0x7ff00000) == 0x7ff00000) {
    // Infinity or NaN
    if (L[_00] & 0xfffff || L[_11]) {
      b = nrv_alloc_D2A("NaN", se, 3);
    } else {
      b = nrv_alloc_D2A("Infinity", se, 8);
    }
    *decpt = -32768;
    return b;
  }

  if (L[_11] == 0 && (L[_00] ^ sign) == 0) {
    b = nrv_alloc_D2A("0", se, 1);
    *decpt = 1;
    return b;
  }

  bits[0] = L[_11];
  bits[1] = L[_00] & 0xfffff;

  if ((ex = (L[_00] >> 20) & 0x7ff) != 0)
    bits[1] |= 0x100000;
  else
    ex = 1;

  ex -= 0x3ff + 52;
  if (ndigits <= 0) {
    mode = 0;
  }

  i = STRTOG_Normal;
  s = gdtoa_D2A(&fpi, ex, bits, &i, mode, ndigits, decpt, se);
  return s;
}

char *m3_dtoa(double d, int mode, int ndigits,
              int *decpt, int *asign, char **se) {
  return m3_dtoa0(&d, mode, ndigits, decpt, asign, se);
}

// decimal to binary float helper

void ULtof(ULong *L, ULong *bits, Long exp, int k) {
  switch (k & STRTOG_Retmask) {
  case STRTOG_NoNumber:
  case STRTOG_Zero:
    *L = 0;
    break;

  case STRTOG_Normal:
  case STRTOG_NaNbits:
    L[0] = bits[0] & 0x7fffff | exp + 0x7f + 23 << 23;
    break;

  case STRTOG_Denormal:
    L[0] = bits[0];
    break;

  case STRTOG_Infinite:
    L[0] = 0x7f800000;
    break;

  case STRTOG_NaN:
    L[0] = f_QNAN;
  }
  if (k & STRTOG_Neg)
    L[0] |= 0x80000000L;
}

// decimal to binary double helper

void ULtod(ULong *L, ULong *bits, Long exp, int k) {
  switch (k & STRTOG_Retmask) {
  case STRTOG_NoNumber:
  case STRTOG_Zero:
    L[0] = L[1] = 0;
    break;

  case STRTOG_Denormal:
    L[_11] = bits[0];
    L[_00] = bits[1];
    break;

  case STRTOG_Normal:
  case STRTOG_NaNbits:
    L[_11] = bits[0];
    L[_00] = (bits[1] & ~0x100000) | ((exp + 0x3ff + 52) << 20);
    break;

  case STRTOG_Infinite:
    L[_00] = 0x7ff00000;
    L[_11] = 0;
    break;

  case STRTOG_NaN:
    L[0] = d_QNAN0;
    L[1] = d_QNAN1;
  }
  if (k & STRTOG_Neg)
    L[_00] |= 0x80000000L;
}

// decimal to binary quad helper

void ULtoQ(ULong *L, ULong *bits, Long exp, int k) {
  switch (k & STRTOG_Retmask) {
  case STRTOG_NoNumber:
  case STRTOG_Zero:
    L[0] = L[1] = L[2] = L[3] = 0;
    break;

  case STRTOG_Normal:
  case STRTOG_NaNbits:
    L[_3] = bits[0];
    L[_2] = bits[1];
    L[_1] = bits[2];
    L[_0] = (bits[3] & ~0x10000) | ((exp + 0x3fff + 112) << 16);
    break;

  case STRTOG_Denormal:
    L[_3] = bits[0];
    L[_2] = bits[1];
    L[_1] = bits[2];
    L[_0] = bits[3];
    break;

  case STRTOG_Infinite:
    L[_0] = 0x7fff0000;
    L[_1] = L[_2] = L[_3] = 0;
    break;

  case STRTOG_NaN:
    L[0] = ld_QNAN0;
    L[1] = ld_QNAN1;
    L[2] = ld_QNAN2;
    L[3] = ld_QNAN3;
  }
  if (k & STRTOG_Neg)
    L[_0] |= 0x80000000L;
}

/*
#undef _0
#undef _1
*/

/* one or the other of IEEE_MC68k or IEEE_8087 should be #defined */
/*
#ifdef IEEE_MC68k
#define _0 0
#define _1 1
#define _2 2
#define _3 3
#endif
#ifdef IEEE_8087
#define _0 3
#define _1 2
#define _2 1
#define _3 0
#endif
*/

// binary quad to decimal

char *g_Qfmt(char *buf, void *V, int ndig, unsigned bufsize) {
  static FPI fpi = {113, 1 - 16383 - 113 + 1, 32766 - 16383 - 113 + 1, 1, 0};
  char *b, *s, *se;
  ULong bits[4], *L, sign;
  int decpt, ex, i, mode;

  if (ndig < 0)
    ndig = 0;
  if (bufsize < ndig + 10)
    return 0;

  L = (ULong *)V;
  sign = L[_0] & 0x80000000L;
  bits[3] = L[_0] & 0xffff;
  bits[2] = L[_1];
  bits[1] = L[_2];
  bits[0] = L[_3];
  b = buf;
  if ((ex = (L[_0] & 0x7fff0000L) >> 16) != 0) {
    if (ex == 0x7fff) {
      /* Infinity or NaN */
      if (bits[0] | bits[1] | bits[2] | bits[3])
        b = strcp_D2A(b, "NaN");
      else {
        b = buf;
        if (sign)
          *b++ = '-';
        b = strcp_D2A(b, "Infinity");
      }
      return b;
    }
    i = STRTOG_Normal;
    bits[3] |= 0x10000;
  } else if (bits[0] | bits[1] | bits[2] | bits[3]) {
    i = STRTOG_Denormal;
    ex = 1;
  } else {
#ifndef IGNORE_ZERO_SIGN
    if (sign)
      *b++ = '-';
#endif
    *b++ = '0';
    *b = 0;
    return b;
  }
  ex -= 0x3fff + 112;
  mode = 2;
  if (ndig <= 0) {
    if (bufsize < 48)
      return 0;
    mode = 0;
  }
  s = gdtoa_D2A(&fpi, ex, bits, &i, mode, ndig, &decpt, &se);
  return g__fmt(buf, s, se, decpt, sign);
}

//char *m3_qtoa(void *V, int mode, int ndigits, int *decpt, int *asign, char **se)
char *m3_qtoa(long double V, int mode, int ndigits, int *decpt,
              int *asign, char **se)
{
  static FPI fpi = {113, 1 - 16383 - 113 + 1, 32766 - 16383 - 113 + 1, 1, 0};
  char *b, *s;
  ULong bits[4], *L, sign;
  int ex, i;

  if (ndigits < 0) {
    ndigits = 0;
  }

  //L = (ULong *)V;
  L = (ULong *)&V;
  sign = L[_0] & 0x80000000L;
  *asign = sign;

  bits[3] = L[_0] & 0xffff;
  bits[2] = L[_1];
  bits[1] = L[_2];
  bits[0] = L[_3];

  if ((ex = (L[_0] & 0x7fff0000L) >> 16) != 0) {
    if (ex == 0x7fff) {
      // Infinity or NaN
      if (bits[0] | bits[1] | bits[2] | bits[3]) {
        b = nrv_alloc_D2A("NaN", se, 3);
      } else {
        b = nrv_alloc_D2A("Infinity", se, 8);
      }
      *decpt = -32768;
      return b;
    }
    i = STRTOG_Normal;
    bits[3] |= 0x10000;
  } else if (bits[0] | bits[1] | bits[2] | bits[3]) {
    i = STRTOG_Denormal;
    ex = 1;
  } else {
    b = nrv_alloc_D2A("0", se, 1);
    *decpt = 1;
    return b;
  }

  ex -= 0x3fff + 112;
  if (ndigits <= 0) {
    mode = 0;
  }
  s = gdtoa_D2A(&fpi, ex, bits, &i, mode, ndigits, decpt, se);
  return s;
}

// ----------- decimal to binary --------------------------------------

// String to REAL

int strtof0(CONST char *s, char **sp, int rounding, float *f) {
  static FPI fpi0 = {24, 1 - 127 - 24 + 1, 254 - 127 - 24 + 1, 1, SI};
  FPI *fpi, fpi1;
  ULong bits[1];
  Long exp;
  int k;

  fpi = &fpi0;
  if (rounding != FPI_Round_near) {
    fpi1 = fpi0;
    fpi1.rounding = rounding;
    fpi = &fpi1;
  }

  k = strtodg_D2A(s, sp, fpi, &exp, bits);

  ULtof((ULong *)f, bits, exp, k);
  return k;
}

float m3_strtof(CONST char *s, char **sp) {
  float f;
  int flags;
  int rounding = FPI_Round_near;
  //ignoring flags
  flags = strtof0(s, sp, rounding, &f); 
  return f;
}

int strtod0(CONST char *s, char **sp, int rounding, double *d) {
  static FPI fpi0 = {53, 1 - 1023 - 53 + 1, 2046 - 1023 - 53 + 1, 1, SI};
  FPI *fpi, fpi1;
  ULong bits[2];
  Long exp;
  int k;

  fpi = &fpi0;
  if (rounding != FPI_Round_near) {
    fpi1 = fpi0;
    fpi1.rounding = rounding;
    fpi = &fpi1;
  }

  k = strtodg_D2A(s, sp, fpi, &exp, bits);

  ULtod((ULong *)d, bits, exp, k);
  return k;
}

// String to LONGREAL

double m3_strtod(CONST char *s, char **sp) {
  double d;
  int flags;
  int rounding = FPI_Round_near;
  //ignoring flags
  flags = strtod0(s, sp, rounding, &d); 
  return d;
}

// String to EXTENDED (Quad 128 bit)

int m3_strtoq(CONST char *s, char **sp, int rounding, void *L) {
//int strtoq0(CONST char *s, char **sp, int rounding, void *L) {
  static FPI fpi0 = {113, 1 - 16383 - 113 + 1, 32766 - 16383 - 113 + 1, 1, SI};
  FPI *fpi, fpi1;
  ULong bits[4];
  Long exp;
  int k;

  fpi = &fpi0;
  if (rounding != FPI_Round_near) {
    fpi1 = fpi0;
    fpi1.rounding = rounding;
    fpi = &fpi1;
  }

  k = strtodg_D2A(s, sp, fpi, &exp, bits);

  ULtoQ((ULong *)L, bits, exp, k);
  return k;
}

/*
   we dont want to return a 16 byte struct, there's possibility of mixup
   in various backends returning structs plus how the c compiler does it
long double m3_strtoq(CONST char *s, char **sp) {
  long double q;
  int flags;
  int rounding = FPI_Round_near;
  //ignoring flags
  flags = strtoq0(s, sp, rounding, &q); 
  return q;
}
*/

void m3_freedtoa(char *s) { freedtoa_D2A(s); }

#ifdef __cplusplus
}
#endif
