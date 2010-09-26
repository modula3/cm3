/* Include file for internal GNU MP types and definitions.

   THE CONTENTS OF THIS FILE ARE FOR INTERNAL USE AND ARE ALMOST CERTAIN TO
   BE SUBJECT TO INCOMPATIBLE CHANGES IN FUTURE GNU MP RELEASES.

Copyright 1991, 1993, 1994, 1995, 1996, 1997, 1999, 2000, 2001, 2002, 2003,
2004, 2005, 2006 Free Software Foundation, Inc.

This file is part of the GNU MP Library.

The GNU MP Library is free software; you can redistribute it and/or modify
it under the terms of the GNU Lesser General Public License as published by
the Free Software Foundation; either version 2.1 of the License, or (at your
option) any later version.

The GNU MP Library is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU Lesser General Public
License for more details.

You should have received a copy of the GNU Lesser General Public License
along with the GNU MP Library; see the file COPYING.LIB.  If not, write to
the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston,
MA 02110-1301, USA. */


/* __GMP_DECLSPEC must be given on any global data that will be accessed
   from outside libgmp, meaning from the test or development programs, or
   from libgmpxx.  Failing to do this will result in an incorrect address
   being used for the accesses.  On functions __GMP_DECLSPEC makes calls
   from outside libgmp more efficient, but they'll still work fine without
   it.  */


#ifndef __GMP_IMPL_H__
#define __GMP_IMPL_H__

#if defined _CRAY
#include <intrinsics.h>  /* for _popcnt */
#endif

/* limits.h is not used in general, since it's an ANSI-ism, and since on
   solaris gcc 2.95 under -mcpu=ultrasparc in ABI=32 ends up getting wrong
   values (the ABI=64 values).

   On Cray vector systems, however, we need the system limits.h since sizes
   of signed and unsigned types can differ there, depending on compiler
   options (eg. -hnofastmd), making our SHRT_MAX etc expressions fail.  For
   reference, int can be 46 or 64 bits, whereas uint is always 64 bits; and
   short can be 24, 32, 46 or 64 bits, and different for ushort.  */

#if defined _CRAY
#include <limits.h>
#endif

/* For fat.h and other fat binary stuff.
   No need for __GMP_ATTRIBUTE_PURE or __GMP_NOTHROW, since functions
   declared this way are only used to set function pointers in __gmp_cpuvec,
   they're not called directly.  */
#define DECL_add_n(name) \
  mp_limb_t name __GMP_PROTO ((mp_ptr, mp_srcptr, mp_srcptr, mp_size_t))
#define DECL_addmul_1(name) \
  mp_limb_t name __GMP_PROTO ((mp_ptr, mp_srcptr, mp_size_t, mp_limb_t))
#define DECL_copyd(name) \
  void name __GMP_PROTO ((mp_ptr, mp_srcptr, mp_size_t))
#define DECL_copyi(name) \
  DECL_copyd (name)
#define DECL_divexact_1(name) \
  mp_limb_t name __GMP_PROTO ((mp_ptr, mp_srcptr, mp_size_t, mp_limb_t))
#define DECL_divexact_by3c(name) \
  mp_limb_t name __GMP_PROTO ((mp_ptr, mp_srcptr, mp_size_t, mp_limb_t))
#define DECL_divrem_1(name) \
  mp_limb_t name __GMP_PROTO ((mp_ptr, mp_size_t, mp_srcptr, mp_size_t, mp_limb_t))
#define DECL_gcd_1(name) \
  mp_limb_t name __GMP_PROTO ((mp_srcptr, mp_size_t, mp_limb_t))
#define DECL_lshift(name) \
  mp_limb_t name __GMP_PROTO ((mp_ptr, mp_srcptr, mp_size_t, unsigned))
#define DECL_mod_1(name) \
  mp_limb_t name __GMP_PROTO ((mp_srcptr, mp_size_t, mp_limb_t))
#define DECL_mod_34lsub1(name) \
  mp_limb_t name __GMP_PROTO ((mp_srcptr, mp_size_t))
#define DECL_modexact_1c_odd(name) \
  mp_limb_t name __GMP_PROTO ((mp_srcptr, mp_size_t, mp_limb_t, mp_limb_t))
#define DECL_mul_1(name) \
  DECL_addmul_1 (name)
#define DECL_mul_basecase(name) \
  void name __GMP_PROTO ((mp_ptr, mp_srcptr, mp_size_t, mp_srcptr, mp_size_t))
#define DECL_preinv_divrem_1(name) \
  mp_limb_t name __GMP_PROTO ((mp_ptr, mp_size_t, mp_srcptr, mp_size_t, mp_limb_t, mp_limb_t, int))
#define DECL_preinv_mod_1(name) \
  mp_limb_t name __GMP_PROTO ((mp_srcptr, mp_size_t, mp_limb_t, mp_limb_t))
#define DECL_rshift(name) \
  DECL_lshift (name)
#define DECL_sqr_basecase(name) \
  void name __GMP_PROTO ((mp_ptr, mp_srcptr, mp_size_t))
#define DECL_sub_n(name) \
  DECL_add_n (name)
#define DECL_submul_1(name) \
  DECL_addmul_1 (name)

#if ! __GMP_WITHIN_CONFIGURE
#include "config.h"
#include "gmp-mparam.h"
#include "mp_bases.h"
#endif

#if HAVE_INTTYPES_H      /* for uint_least32_t */
# include <inttypes.h>
#else
# if HAVE_STDINT_H
#  include <stdint.h>
# endif
#endif

#ifdef __cplusplus
#include <cstring>  /* for strlen */
#include <string>   /* for std::string */
#endif


#ifndef WANT_TMP_DEBUG  /* for TMP_ALLOC_LIMBS_2 and others */
#define WANT_TMP_DEBUG 0
#endif


/* Might search and replace _PROTO to __GMP_PROTO internally one day, to
   avoid two names for one thing, but no hurry for that.  */
#define _PROTO(x)  __GMP_PROTO(x)

/* The following tries to get a good version of alloca.  The tests are
   adapted from autoconf AC_FUNC_ALLOCA, with a couple of additions.
   Whether this succeeds is tested by GMP_FUNC_ALLOCA and HAVE_ALLOCA will
   be setup appropriately.

   ifndef alloca - a cpp define might already exist.
       glibc <stdlib.h> includes <alloca.h> which uses GCC __builtin_alloca.
       HP cc +Olibcalls adds a #define of alloca to __builtin_alloca.

   GCC __builtin_alloca - preferred whenever available.

   _AIX pragma - IBM compilers need a #pragma in "each module that needs to
       use alloca".  Pragma indented to protect pre-ANSI cpp's.  _IBMR2 was
       used in past versions of GMP, retained still in case it matters.

       The autoconf manual says this pragma needs to be at the start of a C
       file, apart from comments and preprocessor directives.  Is that true?
       xlc on aix 4.xxx doesn't seem to mind it being after prototypes etc
       from gmp.h.
*/

#ifndef alloca
# ifdef __GNUC__
#  define alloca __builtin_alloca
# else
#  ifdef __DECC
#   define alloca(x) __ALLOCA(x)
#  else
#   ifdef _MSC_VER
#    include <malloc.h>
#    define alloca _alloca
#   else
#    if HAVE_ALLOCA_H
#     include <alloca.h>
#    else
#     if defined (_AIX) || defined (_IBMR2)
 #pragma alloca
#     else
       char *alloca ();
#     endif
#    endif
#   endif
#  endif
# endif
#endif


/* if not provided by gmp-mparam.h */
#ifndef BYTES_PER_MP_LIMB
#define BYTES_PER_MP_LIMB  SIZEOF_MP_LIMB_T
#endif
#ifndef BITS_PER_MP_LIMB
#define BITS_PER_MP_LIMB  (8 * SIZEOF_MP_LIMB_T)
#endif

#define BITS_PER_ULONG  (8 * SIZEOF_UNSIGNED_LONG)


/* gmp_uint_least32_t is an unsigned integer type with at least 32 bits. */
#if HAVE_UINT_LEAST32_T
typedef uint_least32_t      gmp_uint_least32_t;
#else
#if SIZEOF_UNSIGNED_SHORT >= 4
typedef unsigned short      gmp_uint_least32_t;
#else
#if SIZEOF_UNSIGNED >= 4
typedef unsigned            gmp_uint_least32_t;
#else
typedef unsigned long       gmp_uint_least32_t;
#endif
#endif
#endif


/* const and signed must match __gmp_const and __gmp_signed, so follow the
   decision made for those in gmp.h.    */
#if ! __GMP_HAVE_CONST
#define const   /* empty */
#define signed  /* empty */
#endif

/* "const" basically means a function does nothing but examine its arguments
   and give a return value, it doesn't read or write any memory (neither
   global nor pointed to by arguments), and has no other side-effects.  This
   is more restrictive than "pure".  See info node "(gcc)Function
   Attributes".  __GMP_NO_ATTRIBUTE_CONST_PURE lets tune/common.c etc turn
   this off when trying to write timing loops.  */
#if HAVE_ATTRIBUTE_CONST && ! defined (__GMP_NO_ATTRIBUTE_CONST_PURE)
#define ATTRIBUTE_CONST  __attribute__ ((const))
#else
#define ATTRIBUTE_CONST
#endif

#if HAVE_ATTRIBUTE_NORETURN
#define ATTRIBUTE_NORETURN  __attribute__ ((noreturn))
#else
#define ATTRIBUTE_NORETURN
#endif

/* "malloc" means a function behaves like malloc in that the pointer it
   returns doesn't alias anything.  */
#if HAVE_ATTRIBUTE_MALLOC
#define ATTRIBUTE_MALLOC  __attribute__ ((malloc))
#else
#define ATTRIBUTE_MALLOC
#endif


#if ! HAVE_STRCHR
#define strchr(s,c)  index(s,c)
#endif

#if ! HAVE_MEMSET
#define memset(p, c, n)                 \
  do {                                  \
    ASSERT ((n) >= 0);                  \
    char *__memset__p = (p);            \
    int  __i;                           \
    for (__i = 0; __i < (n); __i++)     \
      __memset__p[__i] = (c);           \
  } while (0)
#endif

/* va_copy is standard in C99, and gcc provides __va_copy when in strict C89
   mode.  Falling back to a memcpy will give maximum portability, since it
   works no matter whether va_list is a pointer, struct or array.  */
#if ! defined (va_copy) && defined (__va_copy)
#define va_copy(dst,src)  __va_copy(dst,src)
#endif
#if ! defined (va_copy)
#define va_copy(dst,src) \
  do { memcpy (&(dst), &(src), sizeof (va_list)); } while (0)
#endif


#if defined (__cplusplus)
extern "C" {
#endif


/* Usage: TMP_DECL;
          TMP_MARK;
          ptr = TMP_ALLOC (bytes);
          TMP_FREE;

   Small allocations should use TMP_SALLOC, big allocations should use
   TMP_BALLOC.  Allocations that might be small or big should use TMP_ALLOC.

   Functions that use just TMP_SALLOC should use TMP_SDECL, TMP_SMARK, and
   TMP_SFREE.

   TMP_DECL just declares a variable, but might be empty and so must be last
   in a list of variables.  TMP_MARK must be done before any TMP_ALLOC.
   TMP_ALLOC(0) is not allowed.  TMP_FREE doesn't need to be done if a
   TMP_MARK was made, but then no TMP_ALLOCs.  */

/* The alignment in bytes, used for TMP_ALLOCed blocks, when alloca or
   __gmp_allocate_func doesn't already determine it.  Currently TMP_ALLOC
   isn't used for "double"s, so that's not in the union.  */
union tmp_align_t {
  mp_limb_t  l;
  char       *p;
};
#define __TMP_ALIGN  sizeof (union tmp_align_t)

/* Return "a" rounded upwards to a multiple of "m", if it isn't already.
   "a" must be an unsigned type.
   This is designed for use with a compile-time constant "m".
   The POW2 case is expected to be usual, and gcc 3.0 and up recognises
   "(-(8*n))%8" or the like is always zero, which means the rounding up in
   the WANT_TMP_NOTREENTRANT version of TMP_ALLOC below will be a noop.  */
#define ROUND_UP_MULTIPLE(a,m)          \
  (POW2_P(m) ? (a) + (-(a))%(m)         \
   : (a)+(m)-1 - (((a)+(m)-1) % (m)))

#if defined (WANT_TMP_ALLOCA) || defined (WANT_TMP_REENTRANT)
struct tmp_reentrant_t {
  struct tmp_reentrant_t  *next;
  size_t		  size;	  /* bytes, including header */
};
void *__gmp_tmp_reentrant_alloc _PROTO ((struct tmp_reentrant_t **, size_t)) ATTRIBUTE_MALLOC;
void  __gmp_tmp_reentrant_free _PROTO ((struct tmp_reentrant_t *));
#endif

#if WANT_TMP_ALLOCA
#define TMP_SDECL
#define TMP_DECL		struct tmp_reentrant_t *__tmp_marker
#define TMP_SMARK
#define TMP_MARK		__tmp_marker = 0
#define TMP_SALLOC(n)		alloca(n)
#define TMP_BALLOC(n)		__gmp_tmp_reentrant_alloc (&__tmp_marker, n)
#define TMP_ALLOC(n)							\
  (LIKELY ((n) < 65536) ? TMP_SALLOC(n) : TMP_BALLOC(n))
#define TMP_SFREE
#define TMP_FREE							   \
  do {									   \
    if (UNLIKELY (__tmp_marker != 0)) __gmp_tmp_reentrant_free (__tmp_marker); \
  } while (0)
#endif

#if WANT_TMP_REENTRANT
#define TMP_SDECL		TMP_DECL
#define TMP_DECL		struct tmp_reentrant_t *__tmp_marker
#define TMP_SMARK		TMP_MARK
#define TMP_MARK		__tmp_marker = 0
#define TMP_SALLOC(n)		TMP_ALLOC(n)
#define TMP_BALLOC(n)		TMP_ALLOC(n)
#define TMP_ALLOC(n)		__gmp_tmp_reentrant_alloc (&__tmp_marker, n)
#define TMP_SFREE		TMP_FREE
#define TMP_FREE		__gmp_tmp_reentrant_free (__tmp_marker)
#endif


/* Allocating various types. */
#define TMP_ALLOC_TYPE(n,type)  ((type *) TMP_ALLOC ((n) * sizeof (type)))
#define TMP_SALLOC_TYPE(n,type) ((type *) TMP_SALLOC ((n) * sizeof (type)))
#define TMP_BALLOC_TYPE(n,type) ((type *) TMP_BALLOC ((n) * sizeof (type)))
#define TMP_ALLOC_LIMBS(n)      TMP_ALLOC_TYPE(n,mp_limb_t)
#define TMP_SALLOC_LIMBS(n)     TMP_SALLOC_TYPE(n,mp_limb_t)
#define TMP_BALLOC_LIMBS(n)     TMP_BALLOC_TYPE(n,mp_limb_t)
#define TMP_ALLOC_MP_PTRS(n)    TMP_ALLOC_TYPE(n,mp_ptr)
#define TMP_SALLOC_MP_PTRS(n)   TMP_SALLOC_TYPE(n,mp_ptr)
#define TMP_BALLOC_MP_PTRS(n)   TMP_BALLOC_TYPE(n,mp_ptr)

/* It's more efficient to allocate one block than two.  This is certainly
   true of the malloc methods, but it can even be true of alloca if that
   involves copying a chunk of stack (various RISCs), or a call to a stack
   bounds check (mingw).  In any case, when debugging keep separate blocks
   so a redzoning malloc debugger can protect each individually.  */
#define TMP_ALLOC_LIMBS_2(xp,xsize, yp,ysize)           \
  do {                                                  \
    if (WANT_TMP_DEBUG)                                 \
      {                                                 \
        (xp) = TMP_ALLOC_LIMBS (xsize);                 \
        (yp) = TMP_ALLOC_LIMBS (ysize);                 \
      }                                                 \
    else                                                \
      {                                                 \
        (xp) = TMP_ALLOC_LIMBS ((xsize) + (ysize));     \
        (yp) = (xp) + (xsize);                          \
      }                                                 \
  } while (0)


/* From gmp.h, nicer names for internal use. */
#define CRAY_Pragma(str)               __GMP_CRAY_Pragma(str)
#define MPN_CMP(result, xp, yp, size)  __GMPN_CMP(result, xp, yp, size)
#define LIKELY(cond)                   __GMP_LIKELY(cond)
#define UNLIKELY(cond)                 __GMP_UNLIKELY(cond)

#define ABS(x) ((x) >= 0 ? (x) : -(x))
#undef MIN
#define MIN(l,o) ((l) < (o) ? (l) : (o))
#undef MAX
#define MAX(h,i) ((h) > (i) ? (h) : (i))
#define numberof(x)  (sizeof (x) / sizeof ((x)[0]))

/* Field access macros.  */
#define SIZ(x) ((x)->_mp_size)
#define ABSIZ(x) ABS (SIZ (x))
#define PTR(x) ((x)->_mp_d)
#define LIMBS(x) ((x)->_mp_d)
#define EXP(x) ((x)->_mp_exp)
#define PREC(x) ((x)->_mp_prec)
#define ALLOC(x) ((x)->_mp_alloc)

/* n-1 inverts any low zeros and the lowest one bit.  If n&(n-1) leaves zero
   then that lowest one bit must have been the only bit set.  n==0 will
   return true though, so avoid that.  */
#define POW2_P(n)  (((n) & ((n) - 1)) == 0)


/* The "short" defines are a bit different because shorts are promoted to
   ints by ~ or >> etc.

   #ifndef's are used since on some systems (HP?) header files other than
   limits.h setup these defines.  We could forcibly #undef in that case, but
   there seems no need to worry about that.  */

#ifndef ULONG_MAX
#define ULONG_MAX   __GMP_ULONG_MAX
#endif
#ifndef UINT_MAX
#define UINT_MAX    __GMP_UINT_MAX
#endif
#ifndef USHRT_MAX
#define USHRT_MAX   __GMP_USHRT_MAX
#endif
#define MP_LIMB_T_MAX      (~ (mp_limb_t) 0)

/* Must cast ULONG_MAX etc to unsigned long etc, since they might not be
   unsigned on a K&R compiler.  In particular the HP-UX 10 bundled K&R cc
   treats the plain decimal values in <limits.h> as signed.  */
#define ULONG_HIGHBIT      (ULONG_MAX ^ ((unsigned long) ULONG_MAX >> 1))
#define UINT_HIGHBIT       (UINT_MAX ^ ((unsigned) UINT_MAX >> 1))
#define USHRT_HIGHBIT      ((unsigned short) (USHRT_MAX ^ ((unsigned short) USHRT_MAX >> 1)))
#define GMP_LIMB_HIGHBIT  (MP_LIMB_T_MAX ^ (MP_LIMB_T_MAX >> 1))

#ifndef LONG_MIN
#define LONG_MIN           ((long) ULONG_HIGHBIT)
#endif
#ifndef LONG_MAX
#define LONG_MAX           (-(LONG_MIN+1))
#endif

#ifndef INT_MIN
#define INT_MIN            ((int) UINT_HIGHBIT)
#endif
#ifndef INT_MAX
#define INT_MAX            (-(INT_MIN+1))
#endif

#ifndef SHRT_MIN
#define SHRT_MIN           ((short) USHRT_HIGHBIT)
#endif
#ifndef SHRT_MAX
#define SHRT_MAX           ((short) (-(SHRT_MIN+1)))
#endif

#if __GMP_MP_SIZE_T_INT
#define MP_SIZE_T_MAX      INT_MAX
#define MP_SIZE_T_MIN      INT_MIN
#else
#define MP_SIZE_T_MAX      LONG_MAX
#define MP_SIZE_T_MIN      LONG_MIN
#endif

/* mp_exp_t is the same as mp_size_t */
#define MP_EXP_T_MAX   MP_SIZE_T_MAX
#define MP_EXP_T_MIN   MP_SIZE_T_MIN

#define LONG_HIGHBIT       LONG_MIN
#define INT_HIGHBIT        INT_MIN
#define SHRT_HIGHBIT       SHRT_MIN


#define GMP_NUMB_HIGHBIT  (CNST_LIMB(1) << (GMP_NUMB_BITS-1))

#if GMP_NAIL_BITS == 0
#define GMP_NAIL_LOWBIT   CNST_LIMB(0)
#else
#define GMP_NAIL_LOWBIT   (CNST_LIMB(1) << GMP_NUMB_BITS)
#endif

#if GMP_NAIL_BITS != 0
/* Set various *_THRESHOLD values to be used for nails.  Thus we avoid using
   code that has not yet been qualified.  */

#undef DIV_SB_PREINV_THRESHOLD
#undef DIV_DC_THRESHOLD
#undef POWM_THRESHOLD
#define DIV_SB_PREINV_THRESHOLD           MP_SIZE_T_MAX
#define DIV_DC_THRESHOLD                 50
#define POWM_THRESHOLD                    0

#undef GCD_ACCEL_THRESHOLD
#define GCD_ACCEL_THRESHOLD               3

#undef DIVREM_1_NORM_THRESHOLD
#undef DIVREM_1_UNNORM_THRESHOLD
#undef MOD_1_NORM_THRESHOLD
#undef MOD_1_UNNORM_THRESHOLD
#undef USE_PREINV_DIVREM_1
#undef USE_PREINV_MOD_1
#undef DIVREM_2_THRESHOLD
#undef DIVEXACT_1_THRESHOLD
#undef MODEXACT_1_ODD_THRESHOLD
#define DIVREM_1_NORM_THRESHOLD           MP_SIZE_T_MAX  /* no preinv */
#define DIVREM_1_UNNORM_THRESHOLD         MP_SIZE_T_MAX  /* no preinv */
#define MOD_1_NORM_THRESHOLD              MP_SIZE_T_MAX  /* no preinv */
#define MOD_1_UNNORM_THRESHOLD            MP_SIZE_T_MAX  /* no preinv */
#define USE_PREINV_DIVREM_1               0  /* no preinv */
#define USE_PREINV_MOD_1                  0  /* no preinv */
#define DIVREM_2_THRESHOLD                MP_SIZE_T_MAX  /* no preinv */

#undef GET_STR_DC_THRESHOLD
#undef GET_STR_PRECOMPUTE_THRESHOLD
#undef SET_STR_THRESHOLD
#define GET_STR_DC_THRESHOLD             22
#define GET_STR_PRECOMPUTE_THRESHOLD     42
#define SET_STR_THRESHOLD              3259

/* mpn/generic/mul_fft.c is not nails-capable. */
#undef  MUL_FFT_THRESHOLD
#undef  SQR_FFT_THRESHOLD
#define MUL_FFT_THRESHOLD                MP_SIZE_T_MAX
#define SQR_FFT_THRESHOLD                MP_SIZE_T_MAX
#endif

/* Swap macros. */

#define MP_LIMB_T_SWAP(x, y)                    \
  do {                                          \
    mp_limb_t __mp_limb_t_swap__tmp = (x);      \
    (x) = (y);                                  \
    (y) = __mp_limb_t_swap__tmp;                \
  } while (0)
#define MP_SIZE_T_SWAP(x, y)                    \
  do {                                          \
    mp_size_t __mp_size_t_swap__tmp = (x);      \
    (x) = (y);                                  \
    (y) = __mp_size_t_swap__tmp;                \
  } while (0)

#define MP_PTR_SWAP(x, y)               \
  do {                                  \
    mp_ptr __mp_ptr_swap__tmp = (x);    \
    (x) = (y);                          \
    (y) = __mp_ptr_swap__tmp;           \
  } while (0)
#define MP_SRCPTR_SWAP(x, y)                    \
  do {                                          \
    mp_srcptr __mp_srcptr_swap__tmp = (x);      \
    (x) = (y);                                  \
    (y) = __mp_srcptr_swap__tmp;                \
  } while (0)

#define MPN_PTR_SWAP(xp,xs, yp,ys)      \
  do {                                  \
    MP_PTR_SWAP (xp, yp);               \
    MP_SIZE_T_SWAP (xs, ys);            \
  } while(0)
#define MPN_SRCPTR_SWAP(xp,xs, yp,ys)   \
  do {                                  \
    MP_SRCPTR_SWAP (xp, yp);            \
    MP_SIZE_T_SWAP (xs, ys);            \
  } while(0)

#define MPZ_PTR_SWAP(x, y)              \
  do {                                  \
    mpz_ptr __mpz_ptr_swap__tmp = (x);  \
    (x) = (y);                          \
    (y) = __mpz_ptr_swap__tmp;          \
  } while (0)
#define MPZ_SRCPTR_SWAP(x, y)                   \
  do {                                          \
    mpz_srcptr __mpz_srcptr_swap__tmp = (x);    \
    (x) = (y);                                  \
    (y) = __mpz_srcptr_swap__tmp;               \
  } while (0)


/* Enhancement: __gmp_allocate_func could have "__attribute__ ((malloc))",
   but current gcc (3.0) doesn't seem to support that.  */
__GMP_DECLSPEC extern void * (*__gmp_allocate_func) __GMP_PROTO ((size_t));
__GMP_DECLSPEC extern void * (*__gmp_reallocate_func) __GMP_PROTO ((void *, size_t, size_t));
__GMP_DECLSPEC extern void   (*__gmp_free_func) __GMP_PROTO ((void *, size_t));

void *__gmp_default_allocate _PROTO ((size_t));
void *__gmp_default_reallocate _PROTO ((void *, size_t, size_t));
void __gmp_default_free _PROTO ((void *, size_t));

#define __GMP_ALLOCATE_FUNC_TYPE(n,type) \
  ((type *) (*__gmp_allocate_func) ((n) * sizeof (type)))
#define __GMP_ALLOCATE_FUNC_LIMBS(n)   __GMP_ALLOCATE_FUNC_TYPE (n, mp_limb_t)

#define __GMP_REALLOCATE_FUNC_TYPE(p, old_size, new_size, type) \
  ((type *) (*__gmp_reallocate_func)                            \
   (p, (old_size) * sizeof (type), (new_size) * sizeof (type)))
#define __GMP_REALLOCATE_FUNC_LIMBS(p, old_size, new_size) \
  __GMP_REALLOCATE_FUNC_TYPE(p, old_size, new_size, mp_limb_t)

#define __GMP_FREE_FUNC_TYPE(p,n,type) (*__gmp_free_func) (p, (n) * sizeof (type))
#define __GMP_FREE_FUNC_LIMBS(p,n)     __GMP_FREE_FUNC_TYPE (p, n, mp_limb_t)

#define __GMP_REALLOCATE_FUNC_MAYBE(ptr, oldsize, newsize)      \
  do {                                                          \
    if ((oldsize) != (newsize))                                 \
      (ptr) = (*__gmp_reallocate_func) (ptr, oldsize, newsize); \
  } while (0)

#define __GMP_REALLOCATE_FUNC_MAYBE_TYPE(ptr, oldsize, newsize, type)   \
  do {                                                                  \
    if ((oldsize) != (newsize))                                         \
      (ptr) = (type *) (*__gmp_reallocate_func)                         \
        (ptr, (oldsize) * sizeof (type), (newsize) * sizeof (type));    \
  } while (0)


/* Dummy for non-gcc, code involving it will go dead. */
#if ! defined (__GNUC__) || __GNUC__ < 2
#define __builtin_constant_p(x)   0
#endif

#define USE_LEADING_REGPARM 0

/* Macros for altering parameter order according to regparm usage. */
#define REGPARM_2_1(a,b,x)    a,b,x
#define REGPARM_3_1(a,b,c,x)  a,b,c,x
#define REGPARM_ATTR(n)

void __gmpz_aorsmul_1 _PROTO ((REGPARM_3_1 (mpz_ptr w, mpz_srcptr u, mp_limb_t v, mp_size_t sub))) REGPARM_ATTR(1);
#define mpz_aorsmul_1(w,u,v,sub)  __gmpz_aorsmul_1 (REGPARM_3_1 (w, u, v, sub))

#define mpz_n_pow_ui __gmpz_n_pow_ui
void    mpz_n_pow_ui _PROTO ((mpz_ptr, mp_srcptr, mp_size_t, unsigned long));


#define mpn_add_nc __MPN(add_nc)
__GMP_DECLSPEC mp_limb_t mpn_add_nc __GMP_PROTO ((mp_ptr, mp_srcptr, mp_srcptr, mp_size_t, mp_limb_t));

#define mpn_addmul_1c __MPN(addmul_1c)
__GMP_DECLSPEC mp_limb_t mpn_addmul_1c __GMP_PROTO ((mp_ptr, mp_srcptr, mp_size_t, mp_limb_t, mp_limb_t));

#define mpn_addmul_2 __MPN(addmul_2)
__GMP_DECLSPEC mp_limb_t mpn_addmul_2 _PROTO ((mp_ptr, mp_srcptr, mp_size_t, mp_srcptr));

#define mpn_addmul_3 __MPN(addmul_3)
__GMP_DECLSPEC mp_limb_t mpn_addmul_3 _PROTO ((mp_ptr, mp_srcptr, mp_size_t, mp_srcptr));

#define mpn_addmul_4 __MPN(addmul_4)
__GMP_DECLSPEC mp_limb_t mpn_addmul_4 _PROTO ((mp_ptr, mp_srcptr, mp_size_t, mp_srcptr));

#define mpn_addmul_5 __MPN(addmul_5)
__GMP_DECLSPEC mp_limb_t mpn_addmul_5 _PROTO ((mp_ptr, mp_srcptr, mp_size_t, mp_srcptr));

#define mpn_addmul_6 __MPN(addmul_6)
__GMP_DECLSPEC mp_limb_t mpn_addmul_6 _PROTO ((mp_ptr, mp_srcptr, mp_size_t, mp_srcptr));

#define mpn_addmul_7 __MPN(addmul_7)
__GMP_DECLSPEC mp_limb_t mpn_addmul_7 _PROTO ((mp_ptr, mp_srcptr, mp_size_t, mp_srcptr));

#define mpn_addmul_8 __MPN(addmul_8)
__GMP_DECLSPEC mp_limb_t mpn_addmul_8 _PROTO ((mp_ptr, mp_srcptr, mp_size_t, mp_srcptr));

/* mpn_addlsh1_n(c,a,b,n), when it exists, sets {c,n} to {a,n}+2*{b,n}, and
   returns the carry out (0, 1 or 2).  */
#define mpn_addlsh1_n __MPN(addlsh1_n)
__GMP_DECLSPEC mp_limb_t mpn_addlsh1_n __GMP_PROTO ((mp_ptr, mp_srcptr, mp_srcptr, mp_size_t));

/* mpn_sublsh1_n(c,a,b,n), when it exists, sets {c,n} to {a,n}-2*{b,n}, and
   returns the borrow out (0, 1 or 2).  */
#define mpn_sublsh1_n __MPN(sublsh1_n)
__GMP_DECLSPEC mp_limb_t mpn_sublsh1_n __GMP_PROTO ((mp_ptr, mp_srcptr, mp_srcptr, mp_size_t));

/* mpn_rsh1add_n(c,a,b,n), when it exists, sets {c,n} to ({a,n} + {b,n}) >> 1,
   and returns the bit rshifted out (0 or 1).  */
#define mpn_rsh1add_n __MPN(rsh1add_n)
__GMP_DECLSPEC mp_limb_t mpn_rsh1add_n __GMP_PROTO ((mp_ptr, mp_srcptr, mp_srcptr, mp_size_t));

/* mpn_rsh1sub_n(c,a,b,n), when it exists, sets {c,n} to ({a,n} - {b,n}) >> 1,
   and returns the bit rshifted out (0 or 1).  If there's a borrow from the
   subtract, it's stored as a 1 in the high bit of c[n-1], like a twos
   complement negative.  */
#define mpn_rsh1sub_n __MPN(rsh1sub_n)
__GMP_DECLSPEC mp_limb_t mpn_rsh1sub_n __GMP_PROTO ((mp_ptr, mp_srcptr, mp_srcptr, mp_size_t));

#define mpn_addsub_n __MPN(addsub_n)
__GMP_DECLSPEC mp_limb_t mpn_addsub_n __GMP_PROTO ((mp_ptr, mp_ptr, mp_srcptr, mp_srcptr, mp_size_t));

#define mpn_addsub_nc __MPN(addsub_nc)
__GMP_DECLSPEC mp_limb_t mpn_addsub_nc __GMP_PROTO ((mp_ptr, mp_ptr, mp_srcptr, mp_srcptr, mp_size_t, mp_limb_t));

#define mpn_divrem_1c __MPN(divrem_1c)
__GMP_DECLSPEC mp_limb_t mpn_divrem_1c __GMP_PROTO ((mp_ptr, mp_size_t, mp_srcptr, mp_size_t, mp_limb_t, mp_limb_t));

#define mpn_dump __MPN(dump)
__GMP_DECLSPEC void mpn_dump __GMP_PROTO ((mp_srcptr, mp_size_t));

#define mpn_fib2_ui __MPN(fib2_ui)
mp_size_t mpn_fib2_ui _PROTO ((mp_ptr, mp_ptr, unsigned long));

/* Remap names of internal mpn functions.  */
#define __clz_tab               __MPN(clz_tab)
#define mpn_udiv_w_sdiv		__MPN(udiv_w_sdiv)

#define mpn_gcd_finda	__MPN(gcd_finda)
mp_limb_t mpn_gcd_finda _PROTO((const mp_limb_t cp[2])) __GMP_ATTRIBUTE_PURE;

#define mpn_jacobi_base __MPN(jacobi_base)
int mpn_jacobi_base _PROTO ((mp_limb_t a, mp_limb_t b, int result_bit1)) ATTRIBUTE_CONST;

#define mpn_mod_1c __MPN(mod_1c)
__GMP_DECLSPEC mp_limb_t mpn_mod_1c __GMP_PROTO ((mp_srcptr, mp_size_t, mp_limb_t, mp_limb_t)) __GMP_ATTRIBUTE_PURE;

#define mpn_mul_1c __MPN(mul_1c)
__GMP_DECLSPEC mp_limb_t mpn_mul_1c __GMP_PROTO ((mp_ptr, mp_srcptr, mp_size_t, mp_limb_t, mp_limb_t));

#define mpn_mul_2 __MPN(mul_2)
mp_limb_t mpn_mul_2 _PROTO ((mp_ptr, mp_srcptr, mp_size_t, mp_srcptr));

#ifndef mpn_mul_basecase  /* if not done with cpuvec in a fat binary */
#define mpn_mul_basecase __MPN(mul_basecase)
__GMP_DECLSPEC void mpn_mul_basecase __GMP_PROTO ((mp_ptr, mp_srcptr, mp_size_t, mp_srcptr, mp_size_t));
#endif

#define mpn_mullow_n __MPN(mullow_n)
__GMP_DECLSPEC void mpn_mullow_n __GMP_PROTO ((mp_ptr, mp_srcptr, mp_srcptr, mp_size_t));

#define mpn_mullow_basecase __MPN(mullow_basecase)
__GMP_DECLSPEC void mpn_mullow_basecase __GMP_PROTO ((mp_ptr, mp_srcptr, mp_srcptr, mp_size_t));

#define mpn_sqr_n __MPN(sqr_n)
__GMP_DECLSPEC void mpn_sqr_n __GMP_PROTO ((mp_ptr, mp_srcptr, mp_size_t));

#ifndef mpn_sqr_basecase  /* if not done with cpuvec in a fat binary */
#define mpn_sqr_basecase __MPN(sqr_basecase)
__GMP_DECLSPEC void mpn_sqr_basecase __GMP_PROTO ((mp_ptr, mp_srcptr, mp_size_t));
#endif

#define mpn_sub_nc __MPN(sub_nc)
__GMP_DECLSPEC mp_limb_t mpn_sub_nc __GMP_PROTO ((mp_ptr, mp_srcptr, mp_srcptr, mp_size_t, mp_limb_t));

#define mpn_submul_1c __MPN(submul_1c)
__GMP_DECLSPEC mp_limb_t mpn_submul_1c __GMP_PROTO ((mp_ptr, mp_srcptr, mp_size_t, mp_limb_t, mp_limb_t));

#define mpn_invert_2exp __MPN(invert_2exp)
__GMP_DECLSPEC void mpn_invert_2exp __GMP_PROTO ((mp_ptr, mp_srcptr, mp_size_t, mp_ptr));

#define mpn_redc_1 __MPN(redc_1)
__GMP_DECLSPEC void mpn_redc_1 __GMP_PROTO ((mp_ptr, mp_ptr, mp_srcptr, mp_size_t, mp_limb_t);)

#define mpn_redc_2 __MPN(redc_2)
__GMP_DECLSPEC void mpn_redc_2 __GMP_PROTO ((mp_ptr, mp_ptr, mp_srcptr, mp_size_t, mp_srcptr));


/* kara uses n+1 limbs of temporary space and then recurses with the balance,
   so need (n+1) + (ceil(n/2)+1) + (ceil(n/4)+1) + ...  This can be solved to
   2n + o(n).  Since n is very limited, o(n) in practice could be around 15.
   For now, assume n is arbitrarily large.  */
#define MPN_KARA_MUL_N_TSIZE(n)   (2*(n) + 2*GMP_LIMB_BITS)
#define MPN_KARA_SQR_N_TSIZE(n)   (2*(n) + 2*GMP_LIMB_BITS)

/* toom3 uses 2n + 2n/3 + o(n) limbs of temporary space if mpn_sublsh1_n is
   unavailable, but just 2n + o(n) if mpn_sublsh1_n is available.  It is hard
   to pin down the value of o(n), since it is a complex function of
   MUL_TOOM3_THRESHOLD and n.  Normally toom3 is used between kara and fft; in
   that case o(n) will be really limited.  If toom3 is used for arbitrarily
   large operands, o(n) will be larger.  These definitions handle operands of
   up to 8956264246117233 limbs.  A single multiplication using toom3 on the
   fastest hardware currently (2003) would need 100 million years, which
   suggests that these limits are acceptable.  */
#if WANT_FFT
#if HAVE_NATIVE_mpn_sublsh1_n
#define MPN_TOOM3_MUL_N_TSIZE(n)  (2*(n) + 63)
#define MPN_TOOM3_SQR_N_TSIZE(n)  (2*(n) + 63)
#else
#define MPN_TOOM3_MUL_N_TSIZE(n)  (2*(n) + 2*(n/3) + 63)
#define MPN_TOOM3_SQR_N_TSIZE(n)  (2*(n) + 2*(n/3) + 63)
#endif
#else /* WANT_FFT */
#if HAVE_NATIVE_mpn_sublsh1_n
#define MPN_TOOM3_MUL_N_TSIZE(n)  (2*(n) + 255)
#define MPN_TOOM3_SQR_N_TSIZE(n)  (2*(n) + 255)
#else
#define MPN_TOOM3_MUL_N_TSIZE(n)  (2*(n) + 2*(n/3) + 255)
#define MPN_TOOM3_SQR_N_TSIZE(n)  (2*(n) + 2*(n/3) + 255)
#endif
#define MPN_TOOM3_MAX_N 285405
#endif /* WANT_FFT */

/* need 2 so that n2>=1 */
#define MPN_KARA_MUL_N_MINSIZE    2
#define MPN_KARA_SQR_N_MINSIZE    2

/* Need l>=1, ls>=1, and 2*ls > l (the latter for the tD MPN_INCR_U) */
#define MPN_TOOM3_MUL_N_MINSIZE   17
#define MPN_TOOM3_SQR_N_MINSIZE   17

#define mpn_sqr_diagonal __MPN(sqr_diagonal)
void mpn_sqr_diagonal _PROTO ((mp_ptr, mp_srcptr, mp_size_t));

#define mpn_kara_mul_n	__MPN(kara_mul_n)
void mpn_kara_mul_n _PROTO((mp_ptr, mp_srcptr, mp_srcptr, mp_size_t, mp_ptr));

#define mpn_kara_sqr_n  __MPN(kara_sqr_n)
void mpn_kara_sqr_n _PROTO ((mp_ptr, mp_srcptr, mp_size_t, mp_ptr));

#define mpn_toom3_mul_n  __MPN(toom3_mul_n)
void mpn_toom3_mul_n _PROTO ((mp_ptr, mp_srcptr, mp_srcptr, mp_size_t,mp_ptr));

#define mpn_toom3_sqr_n  __MPN(toom3_sqr_n)
void mpn_toom3_sqr_n _PROTO((mp_ptr, mp_srcptr, mp_size_t, mp_ptr));


#define mpn_fft_best_k __MPN(fft_best_k)
int     mpn_fft_best_k _PROTO ((mp_size_t n, int sqr)) ATTRIBUTE_CONST;

#define mpn_mul_fft  __MPN(mul_fft)
int mpn_mul_fft _PROTO ((mp_ptr op, mp_size_t pl,
                          mp_srcptr n, mp_size_t nl,
                          mp_srcptr m, mp_size_t ml,
                          int k));

#define mpn_mul_fft_full  __MPN(mul_fft_full)
void mpn_mul_fft_full _PROTO ((mp_ptr op,
                               mp_srcptr n, mp_size_t nl,
                               mp_srcptr m, mp_size_t ml));

#define   mpn_fft_next_size __MPN(fft_next_size)
mp_size_t mpn_fft_next_size _PROTO ((mp_size_t pl, int k)) ATTRIBUTE_CONST;

#define mpn_sb_divrem_mn  __MPN(sb_divrem_mn)
mp_limb_t mpn_sb_divrem_mn _PROTO ((mp_ptr, mp_ptr, mp_size_t,
                                    mp_srcptr, mp_size_t));

#define mpn_dc_divrem_n  __MPN(dc_divrem_n)
mp_limb_t mpn_dc_divrem_n _PROTO ((mp_ptr, mp_ptr, mp_srcptr, mp_size_t));

/* #define mpn_tdiv_q  __MPN(tdiv_q) */
/* void mpn_tdiv_q _PROTO ((mp_ptr, mp_size_t, mp_srcptr, mp_size_t, mp_srcptr, mp_size_t)); */

#define mpz_divexact_gcd  __gmpz_divexact_gcd
void mpz_divexact_gcd _PROTO ((mpz_ptr q, mpz_srcptr a, mpz_srcptr d));

#define mpz_inp_str_nowhite __gmpz_inp_str_nowhite
#ifdef _GMP_H_HAVE_FILE
size_t mpz_inp_str_nowhite _PROTO ((mpz_ptr x, FILE *stream, int base, int c, size_t nread));
#endif

#define mpn_divisible_p __MPN(divisible_p)
int     mpn_divisible_p _PROTO ((mp_srcptr ap, mp_size_t asize,
                                 mp_srcptr dp, mp_size_t dsize)) __GMP_ATTRIBUTE_PURE;

#define mpn_rootrem __MPN(rootrem)
mp_size_t mpn_rootrem _PROTO ((mp_ptr, mp_ptr, mp_srcptr, mp_size_t, mp_limb_t));


#if defined (_CRAY)
#define MPN_COPY_INCR(dst, src, n)					\
  do {									\
    int __i;		/* Faster on some Crays with plain int */	\
    _Pragma ("_CRI ivdep");						\
    for (__i = 0; __i < (n); __i++)					\
      (dst)[__i] = (src)[__i];						\
  } while (0)
#endif

/* used by test programs, hence __GMP_DECLSPEC */
#ifndef mpn_copyi  /* if not done with cpuvec in a fat binary */
#define mpn_copyi __MPN(copyi)
__GMP_DECLSPEC void mpn_copyi _PROTO ((mp_ptr, mp_srcptr, mp_size_t));
#endif

#if ! defined (MPN_COPY_INCR) && HAVE_NATIVE_mpn_copyi
#define MPN_COPY_INCR(dst, src, size)                   \
  do {                                                  \
    ASSERT ((size) >= 0);                               \
    ASSERT (MPN_SAME_OR_INCR_P (dst, src, size));       \
    mpn_copyi (dst, src, size);                         \
  } while (0)
#endif

/* Copy N limbs from SRC to DST incrementing, N==0 allowed.  */
#if ! defined (MPN_COPY_INCR)
#define MPN_COPY_INCR(dst, src, n)                      \
  do {                                                  \
    ASSERT ((n) >= 0);                                  \
    ASSERT (MPN_SAME_OR_INCR_P (dst, src, n));          \
    if ((n) != 0)                                       \
      {                                                 \
	mp_size_t __n = (n) - 1;                        \
	mp_ptr __dst = (dst);                           \
	mp_srcptr __src = (src);                        \
	mp_limb_t __x;                                  \
	__x = *__src++;                                 \
	if (__n != 0)                                   \
	  {                                             \
	    do                                          \
	      {                                         \
		*__dst++ = __x;                         \
		__x = *__src++;                         \
	      }                                         \
	    while (--__n);                              \
	  }                                             \
	*__dst++ = __x;                                 \
      }                                                 \
  } while (0)
#endif


#if defined (_CRAY)
#define MPN_COPY_DECR(dst, src, n)					\
  do {									\
    int __i;		/* Faster on some Crays with plain int */	\
    _Pragma ("_CRI ivdep");						\
    for (__i = (n) - 1; __i >= 0; __i--)				\
      (dst)[__i] = (src)[__i];						\
  } while (0)
#endif

/* used by test programs, hence __GMP_DECLSPEC */
#ifndef mpn_copyd  /* if not done with cpuvec in a fat binary */
#define mpn_copyd __MPN(copyd)
__GMP_DECLSPEC void mpn_copyd _PROTO ((mp_ptr, mp_srcptr, mp_size_t));
#endif

#if ! defined (MPN_COPY_DECR) && HAVE_NATIVE_mpn_copyd
#define MPN_COPY_DECR(dst, src, size)                   \
  do {                                                  \
    ASSERT ((size) >= 0);                               \
    ASSERT (MPN_SAME_OR_DECR_P (dst, src, size));       \
    mpn_copyd (dst, src, size);                         \
  } while (0)
#endif

/* Copy N limbs from SRC to DST decrementing, N==0 allowed.  */
#if ! defined (MPN_COPY_DECR)
#define MPN_COPY_DECR(dst, src, n)                      \
  do {                                                  \
    ASSERT ((n) >= 0);                                  \
    ASSERT (MPN_SAME_OR_DECR_P (dst, src, n));          \
    if ((n) != 0)                                       \
      {                                                 \
	mp_size_t __n = (n) - 1;                        \
	mp_ptr __dst = (dst) + __n;                     \
	mp_srcptr __src = (src) + __n;                  \
	mp_limb_t __x;                                  \
	__x = *__src--;                                 \
	if (__n != 0)                                   \
	  {                                             \
	    do                                          \
	      {                                         \
		*__dst-- = __x;                         \
		__x = *__src--;                         \
	      }                                         \
	    while (--__n);                              \
	  }                                             \
	*__dst-- = __x;                                 \
      }                                                 \
  } while (0)
#endif


#ifndef MPN_COPY
#define MPN_COPY(d,s,n)                         \
  do {                                          \
    ASSERT (MPN_SAME_OR_SEPARATE_P (d, s, n));  \
    MPN_COPY_INCR (d, s, n);                    \
  } while (0)
#endif


/* Set {dst,size} to the limbs of {src,size} in reverse order. */
#define MPN_REVERSE(dst, src, size)                     \
  do {                                                  \
    mp_ptr     __dst = (dst);                           \
    mp_size_t  __size = (size);                         \
    mp_srcptr  __src = (src) + __size - 1;              \
    mp_size_t  __i;                                     \
    ASSERT ((size) >= 0);                               \
    ASSERT (! MPN_OVERLAP_P (dst, size, src, size));    \
    CRAY_Pragma ("_CRI ivdep");                         \
    for (__i = 0; __i < __size; __i++)                  \
      {                                                 \
        *__dst = *__src;                                \
        __dst++;                                        \
        __src--;                                        \
      }                                                 \
  } while (0)


/* Zero n limbs at dst.

   For power and powerpc we want an inline stu/bdnz loop for zeroing.  On
   ppc630 for instance this is optimal since it can sustain only 1 store per
   cycle.

   gcc 2.95.x (for powerpc64 -maix64, or powerpc32) doesn't recognise the
   "for" loop in the generic code below can become stu/bdnz.  The do/while
   here helps it get to that.  The same caveat about plain -mpowerpc64 mode
   applies here as to __GMPN_COPY_INCR in gmp.h.

   xlc 3.1 already generates stu/bdnz from the generic C, and does so from
   this loop too.

   Enhancement: GLIBC does some trickery with dcbz to zero whole cache lines
   at a time.  MPN_ZERO isn't all that important in GMP, so it might be more
   trouble than it's worth to do the same, though perhaps a call to memset
   would be good when on a GNU system.  */


#ifndef MPN_ZERO
#define MPN_ZERO(dst, n)			\
  do {						\
    ASSERT ((n) >= 0);				\
    if ((n) != 0)				\
      {						\
	mp_ptr __dst = (dst);			\
	mp_size_t __n = (n);			\
	do					\
	  *__dst++ = 0;				\
	while (--__n);				\
      }						\
  } while (0)
#endif


/* On the x86s repe/scasl doesn't seem useful, since it takes many cycles to
   start up and would need to strip a lot of zeros before it'd be faster
   than a simple cmpl loop.  Here are some times in cycles for
   std/repe/scasl/cld and cld/repe/scasl (the latter would be for stripping
   low zeros).

                std   cld
           P5    18    16
           P6    46    38
           K6    36    13
           K7    21    20
*/
#ifndef MPN_NORMALIZE
#define MPN_NORMALIZE(DST, NLIMBS) \
  do {									\
    while ((NLIMBS) > 0)                                                \
      {									\
	if ((DST)[(NLIMBS) - 1] != 0)					\
	  break;							\
	(NLIMBS)--;							\
      }									\
  } while (0)
#endif
#ifndef MPN_NORMALIZE_NOT_ZERO
#define MPN_NORMALIZE_NOT_ZERO(DST, NLIMBS)     \
  do {                                          \
    ASSERT ((NLIMBS) >= 1);                     \
    while (1)                                   \
      {                                         \
	if ((DST)[(NLIMBS) - 1] != 0)           \
	  break;                                \
	(NLIMBS)--;                             \
      }                                         \
  } while (0)
#endif

/* Strip least significant zero limbs from {ptr,size} by incrementing ptr
   and decrementing size.  low should be ptr[0], and will be the new ptr[0]
   on returning.  The number in {ptr,size} must be non-zero, ie. size!=0 and
   somewhere a non-zero limb.  */
#define MPN_STRIP_LOW_ZEROS_NOT_ZERO(ptr, size, low)    \
  do {                                                  \
    ASSERT ((size) >= 1);                               \
    ASSERT ((low) == (ptr)[0]);                         \
                                                        \
    while ((low) == 0)                                  \
      {                                                 \
        (size)--;                                       \
        ASSERT ((size) >= 1);                           \
        (ptr)++;                                        \
        (low) = *(ptr);                                 \
      }                                                 \
  } while (0)

/* Initialize X of type mpz_t with space for NLIMBS limbs.  X should be a
   temporary variable; it will be automatically cleared out at function
   return.  We use __x here to make it possible to accept both mpz_ptr and
   mpz_t arguments.  */
#define MPZ_TMP_INIT(X, NLIMBS)                                         \
  do {                                                                  \
    mpz_ptr __x = (X);                                                  \
    ASSERT ((NLIMBS) >= 1);                                             \
    __x->_mp_alloc = (NLIMBS);                                          \
    __x->_mp_d = (mp_ptr) TMP_ALLOC ((NLIMBS) * BYTES_PER_MP_LIMB);     \
  } while (0)

/* Realloc for an mpz_t WHAT if it has less than NEEDED limbs.  */
#define MPZ_REALLOC(z,n) (UNLIKELY ((n) > ALLOC(z))     \
                          ? (mp_ptr) _mpz_realloc(z,n)  \
                          : PTR(z))

#define MPZ_EQUAL_1_P(z)  (SIZ(z)==1 && PTR(z)[0] == 1)


/* For a threshold between algorithms A and B, size>=thresh is where B
   should be used.  Special value MP_SIZE_T_MAX means only ever use A, or
   value 0 means only ever use B.  The tests for these special values will
   be compile-time constants, so the compiler should be able to eliminate
   the code for the unwanted algorithm.  */

#define ABOVE_THRESHOLD(size,thresh)    \
  ((thresh) == 0                        \
   || ((thresh) != MP_SIZE_T_MAX        \
       && (size) >= (thresh)))
#define BELOW_THRESHOLD(size,thresh)  (! ABOVE_THRESHOLD (size, thresh))

/* Usage: int  use_foo = BELOW_THRESHOLD (size, FOO_THRESHOLD);
          ...
          if (CACHED_BELOW_THRESHOLD (use_foo, size, FOO_THRESHOLD))

   When "use_foo" is a constant (thresh is 0 or MP_SIZE_T), gcc prior to
   version 3.3 doesn't optimize away a test "if (use_foo)" when within a
   loop.  CACHED_BELOW_THRESHOLD helps it do so.  */

#define CACHED_ABOVE_THRESHOLD(cache, thresh)           \
  ((thresh) == 0 || (thresh) == MP_SIZE_T_MAX           \
   ? ABOVE_THRESHOLD (0, thresh)                        \
   : (cache))
#define CACHED_BELOW_THRESHOLD(cache, thresh)           \
  ((thresh) == 0 || (thresh) == MP_SIZE_T_MAX           \
   ? BELOW_THRESHOLD (0, thresh)                        \
   : (cache))


/* If MUL_KARATSUBA_THRESHOLD is not already defined, define it to a
   value which is good on most machines.  */
#ifndef MUL_KARATSUBA_THRESHOLD
#define MUL_KARATSUBA_THRESHOLD 32
#endif

/* If MUL_TOOM3_THRESHOLD is not already defined, define it to a
   value which is good on most machines.  */
#ifndef MUL_TOOM3_THRESHOLD
#define MUL_TOOM3_THRESHOLD 128
#endif

/* MUL_KARATSUBA_THRESHOLD_LIMIT is the maximum for MUL_KARATSUBA_THRESHOLD.
   In a normal build MUL_KARATSUBA_THRESHOLD is a constant and we use that.
   In a fat binary or tune program build MUL_KARATSUBA_THRESHOLD is a
   variable and a separate hard limit will have been defined.  Similarly for
   TOOM3.  */
#ifndef MUL_KARATSUBA_THRESHOLD_LIMIT
#define MUL_KARATSUBA_THRESHOLD_LIMIT  MUL_KARATSUBA_THRESHOLD
#endif
#ifndef MUL_TOOM3_THRESHOLD_LIMIT
#define MUL_TOOM3_THRESHOLD_LIMIT  MUL_TOOM3_THRESHOLD
#endif
#ifndef MULLOW_BASECASE_THRESHOLD_LIMIT
#define MULLOW_BASECASE_THRESHOLD_LIMIT  MULLOW_BASECASE_THRESHOLD
#endif

/* SQR_BASECASE_THRESHOLD is where mpn_sqr_basecase should take over from
   mpn_mul_basecase in mpn_sqr_n.  Default is to use mpn_sqr_basecase
   always.  (Note that we certainly always want it if there's a native
   assembler mpn_sqr_basecase.)

   If it turns out that mpn_kara_sqr_n becomes faster than mpn_mul_basecase
   before mpn_sqr_basecase does, then SQR_BASECASE_THRESHOLD is the
   karatsuba threshold and SQR_KARATSUBA_THRESHOLD is 0.  This oddity arises
   more or less because SQR_KARATSUBA_THRESHOLD represents the size up to
   which mpn_sqr_basecase should be used, and that may be never.  */

#ifndef SQR_BASECASE_THRESHOLD
#define SQR_BASECASE_THRESHOLD 0
#endif

#ifndef SQR_KARATSUBA_THRESHOLD
#define SQR_KARATSUBA_THRESHOLD (2*MUL_KARATSUBA_THRESHOLD)
#endif

#ifndef SQR_TOOM3_THRESHOLD
#define SQR_TOOM3_THRESHOLD 128
#endif

/* See comments above about MUL_TOOM3_THRESHOLD_LIMIT.  */
#ifndef SQR_TOOM3_THRESHOLD_LIMIT
#define SQR_TOOM3_THRESHOLD_LIMIT  SQR_TOOM3_THRESHOLD
#endif

/* First k to use for an FFT modF multiply.  A modF FFT is an order
   log(2^k)/log(2^(k-1)) algorithm, so k=3 is merely 1.5 like karatsuba,
   whereas k=4 is 1.33 which is faster than toom3 at 1.485.    */
#define FFT_FIRST_K  4

/* Threshold at which FFT should be used to do a modF NxN -> N multiply. */
#ifndef MUL_FFT_MODF_THRESHOLD
#define MUL_FFT_MODF_THRESHOLD   (MUL_TOOM3_THRESHOLD * 3)
#endif
#ifndef SQR_FFT_MODF_THRESHOLD
#define SQR_FFT_MODF_THRESHOLD   (SQR_TOOM3_THRESHOLD * 3)
#endif

/* Threshold at which FFT should be used to do an NxN -> 2N multiply.  This
   will be a size where FFT is using k=7 or k=8, since an FFT-k used for an
   NxN->2N multiply and not recursing into itself is an order
   log(2^k)/log(2^(k-2)) algorithm, so it'll be at least k=7 at 1.39 which
   is the first better than toom3.  */
#ifndef MUL_FFT_THRESHOLD
#define MUL_FFT_THRESHOLD   (MUL_FFT_MODF_THRESHOLD * 10)
#endif
#ifndef SQR_FFT_THRESHOLD
#define SQR_FFT_THRESHOLD   (SQR_FFT_MODF_THRESHOLD * 10)
#endif

/* Table of thresholds for successive modF FFT "k"s.  The first entry is
   where FFT_FIRST_K+1 should be used, the second FFT_FIRST_K+2,
   etc.  See mpn_fft_best_k(). */
#ifndef MUL_FFT_TABLE
#define MUL_FFT_TABLE                           \
  { MUL_TOOM3_THRESHOLD * 4,   /* k=5 */        \
    MUL_TOOM3_THRESHOLD * 8,   /* k=6 */        \
    MUL_TOOM3_THRESHOLD * 16,  /* k=7 */        \
    MUL_TOOM3_THRESHOLD * 32,  /* k=8 */        \
    MUL_TOOM3_THRESHOLD * 96,  /* k=9 */        \
    MUL_TOOM3_THRESHOLD * 288, /* k=10 */       \
    0 }
#endif
#ifndef SQR_FFT_TABLE
#define SQR_FFT_TABLE                           \
  { SQR_TOOM3_THRESHOLD * 4,   /* k=5 */        \
    SQR_TOOM3_THRESHOLD * 8,   /* k=6 */        \
    SQR_TOOM3_THRESHOLD * 16,  /* k=7 */        \
    SQR_TOOM3_THRESHOLD * 32,  /* k=8 */        \
    SQR_TOOM3_THRESHOLD * 96,  /* k=9 */        \
    SQR_TOOM3_THRESHOLD * 288, /* k=10 */       \
    0 }
#endif

#ifndef FFT_TABLE_ATTRS
#define FFT_TABLE_ATTRS   static const
#endif

#define MPN_FFT_TABLE_SIZE  16


/* mpn_dc_divrem_n(n) calls 2*mul(n/2)+2*div(n/2), thus to be faster than
   div(n) = 4*div(n/2), we need mul(n/2) to be faster than the classic way,
   i.e. n/2 >= MUL_KARATSUBA_THRESHOLD

   Measured values are between 2 and 4 times MUL_KARATSUBA_THRESHOLD, so go
   for 3 as an average.  */

#ifndef DIV_DC_THRESHOLD
#define DIV_DC_THRESHOLD    (3 * MUL_KARATSUBA_THRESHOLD)
#endif


/* Return non-zero if xp,xsize and yp,ysize overlap.
   If xp+xsize<=yp there's no overlap, or if yp+ysize<=xp there's no
   overlap.  If both these are false, there's an overlap. */
#define MPN_OVERLAP_P(xp, xsize, yp, ysize) \
  ((xp) + (xsize) > (yp) && (yp) + (ysize) > (xp))
#define MEM_OVERLAP_P(xp, xsize, yp, ysize)     \
  (   (char *) (xp) + (xsize) > (char *) (yp)   \
   && (char *) (yp) + (ysize) > (char *) (xp))

/* Return non-zero if xp,xsize and yp,ysize are either identical or not
   overlapping.  Return zero if they're partially overlapping. */
#define MPN_SAME_OR_SEPARATE_P(xp, yp, size)    \
  MPN_SAME_OR_SEPARATE2_P(xp, size, yp, size)
#define MPN_SAME_OR_SEPARATE2_P(xp, xsize, yp, ysize)           \
  ((xp) == (yp) || ! MPN_OVERLAP_P (xp, xsize, yp, ysize))

/* Return non-zero if dst,dsize and src,ssize are either identical or
   overlapping in a way suitable for an incrementing/decrementing algorithm.
   Return zero if they're partially overlapping in an unsuitable fashion. */
#define MPN_SAME_OR_INCR2_P(dst, dsize, src, ssize)             \
  ((dst) <= (src) || ! MPN_OVERLAP_P (dst, dsize, src, ssize))
#define MPN_SAME_OR_INCR_P(dst, src, size)      \
  MPN_SAME_OR_INCR2_P(dst, size, src, size)
#define MPN_SAME_OR_DECR2_P(dst, dsize, src, ssize)             \
  ((dst) >= (src) || ! MPN_OVERLAP_P (dst, dsize, src, ssize))
#define MPN_SAME_OR_DECR_P(dst, src, size)      \
  MPN_SAME_OR_DECR2_P(dst, size, src, size)


/* ASSERT() is a private assertion checking scheme, similar to <assert.h>.
   ASSERT() does the check only if WANT_ASSERT is selected, ASSERT_ALWAYS()
   does it always.  Generally assertions are meant for development, but
   might help when looking for a problem later too.

   Note that strings shouldn't be used within the ASSERT expression,
   eg. ASSERT(strcmp(s,"notgood")!=0), since the quotes upset the "expr"
   used in the !HAVE_STRINGIZE case (ie. K&R).  */

#ifdef __LINE__
#define ASSERT_LINE  __LINE__
#else
#define ASSERT_LINE  -1
#endif

#ifdef __FILE__
#define ASSERT_FILE  __FILE__
#else
#define ASSERT_FILE  ""
#endif

void __gmp_assert_header _PROTO ((const char *filename, int linenum));
__GMP_DECLSPEC void __gmp_assert_fail _PROTO ((const char *filename, int linenum, const char *expr)) ATTRIBUTE_NORETURN;

#if HAVE_STRINGIZE
#define ASSERT_FAIL(expr)  __gmp_assert_fail (ASSERT_FILE, ASSERT_LINE, #expr)
#else
#define ASSERT_FAIL(expr)  __gmp_assert_fail (ASSERT_FILE, ASSERT_LINE, "expr")
#endif

#define ASSERT_ALWAYS(expr)     \
  do {                          \
    if (!(expr))                \
      ASSERT_FAIL (expr);       \
  } while (0)

#if WANT_ASSERT
#define ASSERT(expr)   ASSERT_ALWAYS (expr)
#else
#define ASSERT(expr)   do {} while (0)
#endif


/* ASSERT_CARRY checks the expression is non-zero, and ASSERT_NOCARRY checks
   that it's zero.  In both cases if assertion checking is disabled the
   expression is still evaluated.  These macros are meant for use with
   routines like mpn_add_n() where the return value represents a carry or
   whatever that should or shouldn't occur in some context.  For example,
   ASSERT_NOCARRY (mpn_add_n (rp, s1p, s2p, size)); */
#if WANT_ASSERT
#define ASSERT_CARRY(expr)     ASSERT_ALWAYS ((expr) != 0)
#define ASSERT_NOCARRY(expr)   ASSERT_ALWAYS ((expr) == 0)
#else
#define ASSERT_CARRY(expr)     (expr)
#define ASSERT_NOCARRY(expr)   (expr)
#endif


/* ASSERT_CODE includes code when assertion checking is wanted.  This is the
   same as writing "#if WANT_ASSERT", but more compact.  */
#if WANT_ASSERT
#define ASSERT_CODE(expr)  expr
#else
#define ASSERT_CODE(expr)
#endif

/* Check that the nail parts are zero. */
#define ASSERT_ALWAYS_LIMB(limb)                \
  do {                                          \
    mp_limb_t  __nail = (limb) & GMP_NAIL_MASK; \
    ASSERT_ALWAYS (__nail == 0);                \
  } while (0)
#define ASSERT_ALWAYS_MPN(ptr, size)            \
  do {                                          \
    /* let whole loop go dead when no nails */  \
    if (GMP_NAIL_BITS != 0)                     \
      {                                         \
        mp_size_t  __i;                         \
        for (__i = 0; __i < (size); __i++)      \
          ASSERT_ALWAYS_LIMB ((ptr)[__i]);      \
      }                                         \
  } while (0)
#if WANT_ASSERT
#define ASSERT_LIMB(limb)       ASSERT_ALWAYS_LIMB (limb)
#define ASSERT_MPN(ptr, size)   ASSERT_ALWAYS_MPN (ptr, size)
#else
#define ASSERT_LIMB(limb)       do {} while (0)
#define ASSERT_MPN(ptr, size)   do {} while (0)
#endif


/* Assert that an mpn region {ptr,size} is zero, or non-zero.
   size==0 is allowed, and in that case {ptr,size} considered to be zero.  */
#if WANT_ASSERT
#define ASSERT_MPN_ZERO_P(ptr,size)     \
  do {                                  \
    mp_size_t  __i;                     \
    ASSERT ((size) >= 0);               \
    for (__i = 0; __i < (size); __i++)  \
      ASSERT ((ptr)[__i] == 0);         \
  } while (0)
#define ASSERT_MPN_NONZERO_P(ptr,size)  \
  do {                                  \
    mp_size_t  __i;                     \
    int        __nonzero = 0;           \
    ASSERT ((size) >= 0);               \
    for (__i = 0; __i < (size); __i++)  \
      if ((ptr)[__i] != 0)              \
        {                               \
          __nonzero = 1;                \
          break;                        \
        }                               \
    ASSERT (__nonzero);                 \
  } while (0)
#else
#define ASSERT_MPN_ZERO_P(ptr,size)     do {} while (0)
#define ASSERT_MPN_NONZERO_P(ptr,size)  do {} while (0)
#endif


#if HAVE_NATIVE_mpn_com_n
#define mpn_com_n __MPN(com_n)
void mpn_com_n _PROTO ((mp_ptr, mp_srcptr, mp_size_t));
#else
#define mpn_com_n(d,s,n)                                \
  do {                                                  \
    mp_ptr     __d = (d);                               \
    mp_srcptr  __s = (s);                               \
    mp_size_t  __n = (n);                               \
    ASSERT (__n >= 1);                                  \
    ASSERT (MPN_SAME_OR_SEPARATE_P (__d, __s, __n));    \
    do                                                  \
      *__d++ = (~ *__s++) & GMP_NUMB_MASK;              \
    while (--__n);                                      \
  } while (0)
#endif

#define MPN_LOGOPS_N_INLINE(d, s1, s2, n, operation)    \
  do {                                                  \
    mp_ptr	 __d = (d);                             \
    mp_srcptr	 __s1 = (s1);                           \
    mp_srcptr	 __s2 = (s2);                           \
    mp_size_t	 __n = (n);                             \
    ASSERT (__n >= 1);                                  \
    ASSERT (MPN_SAME_OR_SEPARATE_P (__d, __s1, __n));   \
    ASSERT (MPN_SAME_OR_SEPARATE_P (__d, __s2, __n));   \
    do                                                  \
      operation;                                        \
    while (--__n);                                      \
  } while (0)

/* ADDC_LIMB sets w=x+y and cout to 0 or 1 for a carry from that addition. */
#if GMP_NAIL_BITS == 0
#define ADDC_LIMB(cout, w, x, y)        \
  do {                                  \
    mp_limb_t  __x = (x);               \
    mp_limb_t  __y = (y);               \
    mp_limb_t  __w = __x + __y;         \
    (w) = __w;                          \
    (cout) = __w < __x;                 \
  } while (0)
#else
#define ADDC_LIMB(cout, w, x, y)        \
  do {                                  \
    mp_limb_t  __w;                     \
    ASSERT_LIMB (x);                    \
    ASSERT_LIMB (y);                    \
    __w = (x) + (y);                    \
    (w) = __w & GMP_NUMB_MASK;          \
    (cout) = __w >> GMP_NUMB_BITS;      \
  } while (0)
#endif

/* SUBC_LIMB sets w=x-y and cout to 0 or 1 for a borrow from that
   subtract.  */
#if GMP_NAIL_BITS == 0
#define SUBC_LIMB(cout, w, x, y)        \
  do {                                  \
    mp_limb_t  __x = (x);               \
    mp_limb_t  __y = (y);               \
    mp_limb_t  __w = __x - __y;         \
    (w) = __w;                          \
    (cout) = __w > __x;                 \
  } while (0)
#else
#define SUBC_LIMB(cout, w, x, y)        \
  do {                                  \
    mp_limb_t  __w = (x) - (y);         \
    (w) = __w & GMP_NUMB_MASK;          \
    (cout) = __w >> (GMP_LIMB_BITS-1);  \
  } while (0)
#endif


/* MPN_INCR_U does {ptr,size} += n, MPN_DECR_U does {ptr,size} -= n, both
   expecting no carry (or borrow) from that.

   The size parameter is only for the benefit of assertion checking.  In a
   normal build it's unused and the carry/borrow is just propagated as far
   as it needs to go.

   On random data, usually only one or two limbs of {ptr,size} get updated,
   so there's no need for any sophisticated looping, just something compact
   and sensible.

   FIXME: Switch all code from mpn_{incr,decr}_u to MPN_{INCR,DECR}_U,
   declaring their operand sizes, then remove the former.  This is purely
   for the benefit of assertion checking.  */


#if GMP_NAIL_BITS == 0
#ifndef mpn_incr_u
#define mpn_incr_u(p,incr)                              \
  do {                                                  \
    mp_limb_t __x;                                      \
    mp_ptr __p = (p);                                   \
    if (__builtin_constant_p (incr) && (incr) == 1)     \
      {                                                 \
        while (++(*(__p++)) == 0)                       \
          ;                                             \
      }                                                 \
    else                                                \
      {                                                 \
        __x = *__p + (incr);                            \
        *__p = __x;                                     \
        if (__x < (incr))                               \
          while (++(*(++__p)) == 0)                     \
            ;                                           \
      }                                                 \
  } while (0)
#endif
#ifndef mpn_decr_u
#define mpn_decr_u(p,incr)                              \
  do {                                                  \
    mp_limb_t __x;                                      \
    mp_ptr __p = (p);                                   \
    if (__builtin_constant_p (incr) && (incr) == 1)     \
      {                                                 \
        while ((*(__p++))-- == 0)                       \
          ;                                             \
      }                                                 \
    else                                                \
      {                                                 \
        __x = *__p;                                     \
        *__p = __x - (incr);                            \
        if (__x < (incr))                               \
          while ((*(++__p))-- == 0)                     \
            ;                                           \
      }                                                 \
  } while (0)
#endif
#endif

#if GMP_NAIL_BITS >= 1
#ifndef mpn_incr_u
#define mpn_incr_u(p,incr)                              \
  do {							\
    mp_limb_t __x;					\
    mp_ptr __p = (p);					\
    if (__builtin_constant_p (incr) && (incr) == 1)	\
      {							\
	do						\
	  {						\
	    __x = (*__p + 1) & GMP_NUMB_MASK;		\
	    *__p++ = __x;				\
	  }						\
	while (__x == 0);				\
      }							\
    else						\
      {							\
	__x = (*__p + (incr));				\
	*__p++ = __x & GMP_NUMB_MASK;			\
	if (__x >> GMP_NUMB_BITS != 0)			\
	  {						\
	    do						\
	      {						\
		__x = (*__p + 1) & GMP_NUMB_MASK;	\
		*__p++ = __x;				\
	      }						\
	    while (__x == 0);				\
	  }						\
      }							\
  } while (0)
#endif
#ifndef mpn_decr_u
#define mpn_decr_u(p,incr)				\
  do {							\
    mp_limb_t __x;					\
    mp_ptr __p = (p);					\
    if (__builtin_constant_p (incr) && (incr) == 1)	\
      {							\
	do						\
	  {						\
	    __x = *__p;					\
	    *__p++ = (__x - 1) & GMP_NUMB_MASK;		\
	  }						\
	while (__x == 0);				\
      }							\
    else						\
      {							\
	__x = *__p - (incr);				\
	*__p++ = __x & GMP_NUMB_MASK;			\
	if (__x >> GMP_NUMB_BITS != 0)			\
	  {						\
	    do						\
	      {						\
		__x = *__p;				\
		*__p++ = (__x - 1) & GMP_NUMB_MASK;	\
	      }						\
	    while (__x == 0);				\
	  }						\
      }							\
  } while (0)
#endif
#endif

#ifndef MPN_INCR_U
#if WANT_ASSERT
#define MPN_INCR_U(ptr, size, n)                        \
  do {                                                  \
    ASSERT ((size) >= 1);                               \
    ASSERT_NOCARRY (mpn_add_1 (ptr, ptr, size, n));     \
  } while (0)
#else
#define MPN_INCR_U(ptr, size, n)   mpn_incr_u (ptr, n)
#endif
#endif

#ifndef MPN_DECR_U
#if WANT_ASSERT
#define MPN_DECR_U(ptr, size, n)                        \
  do {                                                  \
    ASSERT ((size) >= 1);                               \
    ASSERT_NOCARRY (mpn_sub_1 (ptr, ptr, size, n));     \
  } while (0)
#else
#define MPN_DECR_U(ptr, size, n)   mpn_decr_u (ptr, n)
#endif
#endif


/* Structure for conversion between internal binary format and
   strings in base 2..36.  */
struct bases
{
  /* Number of digits in the conversion base that always fits in an mp_limb_t.
     For example, for base 10 on a machine where a mp_limb_t has 32 bits this
     is 9, since 10**9 is the largest number that fits into a mp_limb_t.  */
  int chars_per_limb;

  /* log(2)/log(conversion_base) */
  double chars_per_bit_exactly;

  /* base**chars_per_limb, i.e. the biggest number that fits a word, built by
     factors of base.  Exception: For 2, 4, 8, etc, big_base is log2(base),
     i.e. the number of bits used to represent each digit in the base.  */
  mp_limb_t big_base;

  /* A BITS_PER_MP_LIMB bit approximation to 1/big_base, represented as a
     fixed-point number.  Instead of dividing by big_base an application can
     choose to multiply by big_base_inverted.  */
  mp_limb_t big_base_inverted;
};

#define mp_bases __MPN(bases)
#define __mp_bases __MPN(bases)
__GMP_DECLSPEC extern const struct bases mp_bases[257];


/* For power of 2 bases this is exact.  For other bases the result is either
   exact or one too big.

   To be exact always it'd be necessary to examine all the limbs of the
   operand, since numbers like 100..000 and 99...999 generally differ only
   in the lowest limb.  It'd be possible to examine just a couple of high
   limbs to increase the probability of being exact, but that doesn't seem
   worth bothering with.  */

#define MPN_SIZEINBASE(result, ptr, size, base)                         \
  do {                                                                  \
    int       __lb_base, __cnt;                                         \
    size_t __totbits;                                                   \
                                                                        \
    ASSERT ((size) >= 0);                                               \
    ASSERT ((base) >= 2);                                               \
    ASSERT ((base) < numberof (mp_bases));                              \
                                                                        \
    /* Special case for X == 0.  */                                     \
    if ((size) == 0)                                                    \
      (result) = 1;                                                     \
    else                                                                \
      {                                                                 \
        /* Calculate the total number of significant bits of X.  */     \
        count_leading_zeros (__cnt, (ptr)[(size)-1]);                   \
        __totbits = (size_t) (size) * GMP_NUMB_BITS - (__cnt - GMP_NAIL_BITS);\
                                                                        \
        if (POW2_P (base))                                              \
          {                                                             \
            __lb_base = mp_bases[base].big_base;                        \
            (result) = (__totbits + __lb_base - 1) / __lb_base;         \
          }                                                             \
        else                                                            \
          (result) = (size_t)                                           \
            (__totbits * mp_bases[base].chars_per_bit_exactly) + 1;     \
      }                                                                 \
  } while (0)

/* eliminate mp_bases lookups for base==16 */
#define MPN_SIZEINBASE_16(result, ptr, size)                            \
  do {                                                                  \
    int       __cnt;                                                    \
    mp_size_t __totbits;                                                \
                                                                        \
    ASSERT ((size) >= 0);                                               \
                                                                        \
    /* Special case for X == 0.  */                                     \
    if ((size) == 0)                                                    \
      (result) = 1;                                                     \
    else                                                                \
      {                                                                 \
        /* Calculate the total number of significant bits of X.  */     \
        count_leading_zeros (__cnt, (ptr)[(size)-1]);                   \
        __totbits = (size_t) (size) * GMP_NUMB_BITS - (__cnt - GMP_NAIL_BITS);\
        (result) = (__totbits + 4 - 1) / 4;                             \
      }                                                                 \
  } while (0)

/* bit count to limb count, rounding up */
#define BITS_TO_LIMBS(n)  (((n) + (GMP_NUMB_BITS - 1)) / GMP_NUMB_BITS)

/* MPN_SET_UI sets an mpn (ptr, cnt) to given ui.  MPZ_FAKE_UI creates fake
   mpz_t from ui.  The zp argument must have room for LIMBS_PER_ULONG limbs
   in both cases (LIMBS_PER_ULONG is also defined here.) */
#if BITS_PER_ULONG <= GMP_NUMB_BITS /* need one limb per ulong */

#define LIMBS_PER_ULONG 1
#define MPN_SET_UI(zp, zn, u)   \
  (zp)[0] = (u);                \
  (zn) = ((zp)[0] != 0);
#define MPZ_FAKE_UI(z, zp, u)   \
  (zp)[0] = (u);                \
  PTR (z) = (zp);               \
  SIZ (z) = ((zp)[0] != 0);     \
  ASSERT_CODE (ALLOC (z) = 1);

#else /* need two limbs per ulong */

#define LIMBS_PER_ULONG 2
#define MPN_SET_UI(zp, zn, u)                          \
  (zp)[0] = (u) & GMP_NUMB_MASK;                       \
  (zp)[1] = (u) >> GMP_NUMB_BITS;                      \
  (zn) = ((zp)[1] != 0 ? 2 : (zp)[0] != 0 ? 1 : 0);
#define MPZ_FAKE_UI(z, zp, u)                          \
  (zp)[0] = (u) & GMP_NUMB_MASK;                       \
  (zp)[1] = (u) >> GMP_NUMB_BITS;                      \
  SIZ (z) = ((zp)[1] != 0 ? 2 : (zp)[0] != 0 ? 1 : 0); \
  PTR (z) = (zp);                                      \
  ASSERT_CODE (ALLOC (z) = 2);

#endif


/* LIMB_HIGHBIT_TO_MASK(n) examines the high bit of a limb value and turns 1
   or 0 there into a limb 0xFF..FF or 0 respectively.

   On most CPUs this is just an arithmetic right shift by GMP_LIMB_BITS-1,
   but C99 doesn't guarantee signed right shifts are arithmetic, so we have
   a little compile-time test and a fallback to a "? :" form.  The latter is
   necessary for instance on Cray vector systems.

   Recent versions of gcc (eg. 3.3) will in fact optimize a "? :" like this
   to an arithmetic right shift anyway, but it's good to get the desired
   shift on past versions too (in particular since an important use of
   LIMB_HIGHBIT_TO_MASK is in udiv_qrnnd_preinv).  */

#define LIMB_HIGHBIT_TO_MASK(n)                                 \
  (((mp_limb_signed_t) -1 >> 1) < 0                             \
   ? (mp_limb_signed_t) (n) >> (GMP_LIMB_BITS - 1)              \
   : (n) & GMP_LIMB_HIGHBIT ? MP_LIMB_T_MAX : CNST_LIMB(0))


/* Use a library function for invert_limb, if available. */
#define mpn_invert_limb  __MPN(invert_limb)
mp_limb_t mpn_invert_limb _PROTO ((mp_limb_t)) ATTRIBUTE_CONST;
#if ! defined (invert_limb) && HAVE_NATIVE_mpn_invert_limb
#define invert_limb(invxl,xl)           \
  do {                                  \
    (invxl) = mpn_invert_limb (xl);     \
  } while (0)
#endif

#ifndef invert_limb
#define invert_limb(invxl,xl)                   \
  do {                                          \
    mp_limb_t dummy;                            \
    ASSERT ((xl) != 0);                         \
    udiv_qrnnd (invxl, dummy, ~(xl), ~CNST_LIMB(0), xl);  \
  } while (0)
#endif

#ifndef udiv_qrnnd_preinv
#define udiv_qrnnd_preinv udiv_qrnnd_preinv2
#endif

/* Divide the two-limb number in (NH,,NL) by D, with DI being the largest
   limb not larger than (2**(2*BITS_PER_MP_LIMB))/D - (2**BITS_PER_MP_LIMB).
   If this would yield overflow, DI should be the largest possible number
   (i.e., only ones).  For correct operation, the most significant bit of D
   has to be set.  Put the quotient in Q and the remainder in R.  */

/* Like udiv_qrnnd_preinv, but branch-free. */
#define udiv_qrnnd_preinv2(q, r, nh, nl, d, di)				\
  do {									\
    mp_limb_t _n2, _n10, _nmask, _nadj, _q1;				\
    mp_limb_t _xh, _xl;							\
    _n2 = (nh);								\
    _n10 = (nl);							\
    _nmask = LIMB_HIGHBIT_TO_MASK (_n10);				\
    _nadj = _n10 + (_nmask & (d));					\
    umul_ppmm (_xh, _xl, di, _n2 - _nmask);				\
    add_ssaaaa (_xh, _xl, _xh, _xl, _n2, _nadj);			\
    _q1 = ~_xh;								\
    umul_ppmm (_xh, _xl, _q1, d);					\
    add_ssaaaa (_xh, _xl, _xh, _xl, nh, nl);				\
    _xh -= (d);					/* xh = 0 or -1 */	\
    (r) = _xl + ((d) & _xh);						\
    (q) = _xh - _q1;							\
  } while (0)


#ifndef mpn_preinv_divrem_1  /* if not done with cpuvec in a fat binary */
#define mpn_preinv_divrem_1  __MPN(preinv_divrem_1)
mp_limb_t mpn_preinv_divrem_1 _PROTO ((mp_ptr, mp_size_t, mp_srcptr, mp_size_t, mp_limb_t, mp_limb_t, int));
#endif


/* USE_PREINV_DIVREM_1 is whether to use mpn_preinv_divrem_1, as opposed to
   the plain mpn_divrem_1.  Likewise USE_PREINV_MOD_1 chooses between
   mpn_preinv_mod_1 and plain mpn_mod_1.  The default for both is yes, since
   the few CISC chips where preinv is not good have defines saying so.  */
#ifndef USE_PREINV_DIVREM_1
#define USE_PREINV_DIVREM_1   1
#endif
#ifndef USE_PREINV_MOD_1
#define USE_PREINV_MOD_1   1
#endif

#if USE_PREINV_DIVREM_1
#define MPN_DIVREM_OR_PREINV_DIVREM_1(qp,xsize,ap,size,d,dinv,shift)    \
  mpn_preinv_divrem_1 (qp, xsize, ap, size, d, dinv, shift)
#else
#define MPN_DIVREM_OR_PREINV_DIVREM_1(qp,xsize,ap,size,d,dinv,shift)    \
  mpn_divrem_1 (qp, xsize, ap, size, d)
#endif

#if USE_PREINV_MOD_1
#define MPN_MOD_OR_PREINV_MOD_1(src,size,divisor,inverse)       \
  mpn_preinv_mod_1 (src, size, divisor, inverse)
#else
#define MPN_MOD_OR_PREINV_MOD_1(src,size,divisor,inverse)       \
  mpn_mod_1 (src, size, divisor)
#endif


#ifndef mpn_mod_34lsub1  /* if not done with cpuvec in a fat binary */
#define mpn_mod_34lsub1 __MPN(mod_34lsub1)
mp_limb_t mpn_mod_34lsub1 _PROTO ((mp_srcptr, mp_size_t)) __GMP_ATTRIBUTE_PURE;
#endif


/* DIVEXACT_1_THRESHOLD is at what size to use mpn_divexact_1, as opposed to
   plain mpn_divrem_1.  Likewise MODEXACT_1_ODD_THRESHOLD for
   mpn_modexact_1_odd against plain mpn_mod_1.  On most CPUs divexact and
   modexact are faster at all sizes, so the defaults are 0.  Those CPUs
   where this is not right have a tuned threshold.  */
#ifndef DIVEXACT_1_THRESHOLD
#define DIVEXACT_1_THRESHOLD  0
#endif
#ifndef MODEXACT_1_ODD_THRESHOLD
#define MODEXACT_1_ODD_THRESHOLD  0
#endif

#ifndef mpn_divexact_1  /* if not done with cpuvec in a fat binary */
#define mpn_divexact_1 __MPN(divexact_1)
void    mpn_divexact_1 _PROTO ((mp_ptr, mp_srcptr, mp_size_t, mp_limb_t));
#endif

#define MPN_DIVREM_OR_DIVEXACT_1(dst, src, size, divisor)                     \
  do {                                                                        \
    if (BELOW_THRESHOLD (size, DIVEXACT_1_THRESHOLD))                         \
      ASSERT_NOCARRY (mpn_divrem_1 (dst, (mp_size_t) 0, src, size, divisor)); \
    else                                                                      \
      {                                                                       \
        ASSERT (mpn_mod_1 (src, size, divisor) == 0);                         \
        mpn_divexact_1 (dst, src, size, divisor);                             \
      }                                                                       \
  } while (0)

#ifndef mpn_modexact_1c_odd  /* if not done with cpuvec in a fat binary */
#define mpn_modexact_1c_odd  __MPN(modexact_1c_odd)
mp_limb_t mpn_modexact_1c_odd _PROTO ((mp_srcptr src, mp_size_t size,
                                       mp_limb_t divisor, mp_limb_t c)) __GMP_ATTRIBUTE_PURE;
#endif

/* Multiplicative inverse of 3, modulo 2^GMP_NUMB_BITS.
   Eg. 0xAAAAAAAB for 32 bits, 0xAAAAAAAAAAAAAAAB for 64 bits.
   GMP_NUMB_MAX/3*2+1 is right when GMP_NUMB_BITS is even, but when it's odd
   we need to start from GMP_NUMB_MAX>>1. */
#define MODLIMB_INVERSE_3 (((GMP_NUMB_MAX >> (GMP_NUMB_BITS % 2)) / 3) * 2 + 1)

/* ceil(GMP_NUMB_MAX/3) and ceil(2*GMP_NUMB_MAX/3).
   These expressions work because GMP_NUMB_MAX%3 != 0 for all GMP_NUMB_BITS. */
#define GMP_NUMB_CEIL_MAX_DIV3   (GMP_NUMB_MAX / 3 + 1)
#define GMP_NUMB_CEIL_2MAX_DIV3  ((GMP_NUMB_MAX>>1) / 3 + 1 + GMP_NUMB_HIGHBIT)


/* Set r to -a mod d.  a>=d is allowed.  Can give r>d.  All should be limbs.

   It's not clear whether this is the best way to do this calculation.
   Anything congruent to -a would be fine for the one limb congruence
   tests.  */

#define NEG_MOD(r, a, d)						\
  do {									\
    ASSERT ((d) != 0);							\
    ASSERT_LIMB (a);							\
    ASSERT_LIMB (d);							\
									\
    if ((a) <= (d))							\
      {									\
        /* small a is reasonably likely */				\
        (r) = (d) - (a);						\
      }									\
    else								\
      {									\
        unsigned   __twos;						\
        mp_limb_t  __dnorm;						\
        count_leading_zeros (__twos, d);				\
        __twos -= GMP_NAIL_BITS;					\
        __dnorm = (d) << __twos;					\
        (r) = ((a) <= __dnorm ? __dnorm : 2*__dnorm) - (a);		\
      }									\
									\
    ASSERT_LIMB (r);							\
  } while (0)

/* A bit mask of all the least significant zero bits of n, or -1 if n==0. */
#define LOW_ZEROS_MASK(n)  (((n) & -(n)) - 1)


/* ULONG_PARITY sets "p" to 1 if there's an odd number of 1 bits in "n", or
   to 0 if there's an even number.  "n" should be an unsigned long and "p"
   an int.  */

/* Cray intrinsic _popcnt. */
#ifdef _CRAY
#define ULONG_PARITY(p, n)      \
  do {                          \
    (p) = _popcnt (n) & 1;      \
  } while (0)
#endif

#if ! defined (ULONG_PARITY)
#define ULONG_PARITY(p, n)						\
  do {									\
    unsigned long  __n = (n);						\
    int  __p = 0;							\
    do									\
      {									\
        __p ^= 0x96696996L >> (__n & 0x1F);				\
        __n >>= 5;							\
      }									\
    while (__n != 0);							\
									\
    (p) = __p & 1;							\
  } while (0)
#endif


#if ! defined (BSWAP_LIMB)
#if BITS_PER_MP_LIMB == 8
#define BSWAP_LIMB(dst, src)            \
  do { (dst) = (src); } while (0)
#endif
#if BITS_PER_MP_LIMB == 16
#define BSWAP_LIMB(dst, src)                    \
  do {                                          \
    (dst) = ((src) << 8) + ((src) >> 8);        \
  } while (0)
#endif
#if BITS_PER_MP_LIMB == 32
#define BSWAP_LIMB(dst, src)    \
  do {                          \
    (dst) =                     \
      ((src) << 24)             \
      + (((src) & 0xFF00) << 8) \
      + (((src) >> 8) & 0xFF00) \
      + ((src) >> 24);          \
  } while (0)
#endif
#if BITS_PER_MP_LIMB == 64
#define BSWAP_LIMB(dst, src)            \
  do {                                  \
    (dst) =                             \
      ((src) << 56)                     \
      + (((src) & 0xFF00) << 40)        \
      + (((src) & 0xFF0000) << 24)      \
      + (((src) & 0xFF000000) << 8)     \
      + (((src) >> 8) & 0xFF000000)     \
      + (((src) >> 24) & 0xFF0000)      \
      + (((src) >> 40) & 0xFF00)        \
      + ((src) >> 56);                  \
  } while (0)
#endif
#endif

#if ! defined (BSWAP_LIMB)
#define BSWAP_LIMB(dst, src)                            \
  do {                                                  \
    mp_limb_t  __bswapl_src = (src);                    \
    mp_limb_t  __dst = 0;                               \
    int        __i;                                     \
    for (__i = 0; __i < BYTES_PER_MP_LIMB; __i++)       \
      {                                                 \
        __dst = (__dst << 8) | (__bswapl_src & 0xFF);   \
        __bswapl_src >>= 8;                             \
      }                                                 \
    (dst) = __dst;                                      \
  } while (0)
#endif


#if ! defined (BSWAP_LIMB_FETCH)
#define BSWAP_LIMB_FETCH(limb, src)  BSWAP_LIMB (limb, *(src))
#endif


#if ! defined (BSWAP_LIMB_STORE)
#define BSWAP_LIMB_STORE(dst, limb)  BSWAP_LIMB (*(dst), limb)
#endif


/* Byte swap limbs from {src,size} and store at {dst,size}. */
#define MPN_BSWAP(dst, src, size)                       \
  do {                                                  \
    mp_ptr     __dst = (dst);                           \
    mp_srcptr  __src = (src);                           \
    mp_size_t  __size = (size);                         \
    mp_size_t  __i;                                     \
    ASSERT ((size) >= 0);                               \
    ASSERT (MPN_SAME_OR_SEPARATE_P (dst, src, size));   \
    CRAY_Pragma ("_CRI ivdep");                         \
    for (__i = 0; __i < __size; __i++)                  \
      {                                                 \
        BSWAP_LIMB_FETCH (*__dst, __src);               \
        __dst++;                                        \
        __src++;                                        \
      }                                                 \
  } while (0)

/* Byte swap limbs from {dst,size} and store in reverse order at {src,size}. */
#define MPN_BSWAP_REVERSE(dst, src, size)               \
  do {                                                  \
    mp_ptr     __dst = (dst);                           \
    mp_size_t  __size = (size);                         \
    mp_srcptr  __src = (src) + __size - 1;              \
    mp_size_t  __i;                                     \
    ASSERT ((size) >= 0);                               \
    ASSERT (! MPN_OVERLAP_P (dst, size, src, size));    \
    CRAY_Pragma ("_CRI ivdep");                         \
    for (__i = 0; __i < __size; __i++)                  \
      {                                                 \
        BSWAP_LIMB_FETCH (*__dst, __src);               \
        __dst++;                                        \
        __src--;                                        \
      }                                                 \
  } while (0)


/* Cray intrinsic. */
#ifdef _CRAY
#define popc_limb(result, input)        \
  do {                                  \
    (result) = _popcnt (input);         \
  } while (0)
#endif

/* Cool population count of an mp_limb_t.
   You have to figure out how this works, We won't tell you!

   The constants could also be expressed as:
     0x55... = [2^N / 3]     = [(2^N-1)/3]
     0x33... = [2^N / 5]     = [(2^N-1)/5]
     0x0f... = [2^N / 17]    = [(2^N-1)/17]
     (N is GMP_LIMB_BITS, [] denotes truncation.) */

#if ! defined (popc_limb) && GMP_LIMB_BITS == 8
#define popc_limb(result, input)					\
  do {									\
    mp_limb_t  __x = (input);						\
    __x -= (__x >> 1) & MP_LIMB_T_MAX/3;				\
    __x = ((__x >> 2) & MP_LIMB_T_MAX/5) + (__x & MP_LIMB_T_MAX/5);	\
    __x = ((__x >> 4) + __x) & MP_LIMB_T_MAX/17;			\
    (result) = __x & 0xff;						\
  } while (0)
#endif

#if ! defined (popc_limb) && GMP_LIMB_BITS == 16
#define popc_limb(result, input)					\
  do {									\
    mp_limb_t  __x = (input);						\
    __x -= (__x >> 1) & MP_LIMB_T_MAX/3;				\
    __x = ((__x >> 2) & MP_LIMB_T_MAX/5) + (__x & MP_LIMB_T_MAX/5);	\
    __x = ((__x >> 4) + __x) & MP_LIMB_T_MAX/17;			\
    if (GMP_LIMB_BITS > 8)						\
      __x = ((__x >> 8) + __x);						\
    (result) = __x & 0xff;						\
  } while (0)
#endif

#if ! defined (popc_limb) && GMP_LIMB_BITS == 32
#define popc_limb(result, input)					\
  do {									\
    mp_limb_t  __x = (input);						\
    __x -= (__x >> 1) & MP_LIMB_T_MAX/3;				\
    __x = ((__x >> 2) & MP_LIMB_T_MAX/5) + (__x & MP_LIMB_T_MAX/5);	\
    __x = ((__x >> 4) + __x) & MP_LIMB_T_MAX/17;			\
    if (GMP_LIMB_BITS > 8)						\
      __x = ((__x >> 8) + __x);						\
    if (GMP_LIMB_BITS > 16)						\
      __x = ((__x >> 16) + __x);					\
    (result) = __x & 0xff;						\
  } while (0)
#endif

#if ! defined (popc_limb) && GMP_LIMB_BITS == 64
#define popc_limb(result, input)					\
  do {									\
    mp_limb_t  __x = (input);						\
    __x -= (__x >> 1) & MP_LIMB_T_MAX/3;				\
    __x = ((__x >> 2) & MP_LIMB_T_MAX/5) + (__x & MP_LIMB_T_MAX/5);	\
    __x = ((__x >> 4) + __x) & MP_LIMB_T_MAX/17;			\
    if (GMP_LIMB_BITS > 8)						\
      __x = ((__x >> 8) + __x);						\
    if (GMP_LIMB_BITS > 16)						\
      __x = ((__x >> 16) + __x);					\
    if (GMP_LIMB_BITS > 32)						\
      __x = ((__x >> 32) + __x);					\
    (result) = __x & 0xff;						\
  } while (0)
#endif


/* Define stuff for longlong.h.  */
#if HAVE_ATTRIBUTE_MODE
typedef unsigned int UQItype	__attribute__ ((mode (QI)));
typedef		 int SItype	__attribute__ ((mode (SI)));
typedef unsigned int USItype	__attribute__ ((mode (SI)));
typedef		 int DItype	__attribute__ ((mode (DI)));
typedef unsigned int UDItype	__attribute__ ((mode (DI)));
#else
typedef unsigned char UQItype;
typedef		 long SItype;
typedef unsigned long USItype;
#if HAVE_LONG_LONG
typedef	long long int DItype;
typedef unsigned long long int UDItype;
#else /* Assume `long' gives us a wide enough type.  Needed for hppa2.0w.  */
typedef long int DItype;
typedef unsigned long int UDItype;
#endif
#endif

typedef mp_limb_t UWtype;
typedef unsigned int UHWtype;
#define W_TYPE_SIZE BITS_PER_MP_LIMB

/* Define ieee_double_extract and _GMP_IEEE_FLOATS.

   Bit field packing is "implementation defined" according to C99, which
   leaves us at the compiler's mercy here.  For some systems packing is
   defined in the ABI (eg. x86).  In any case so far it seems universal that
   little endian systems pack from low to high, and big endian from high to
   low within the given type.

   Within the fields we rely on the integer endianness being the same as the
   float endianness, this is true everywhere we know of and it'd be a fairly
   strange system that did anything else.  */

#if HAVE_DOUBLE_IEEE_LITTLE_SWAPPED
#define _GMP_IEEE_FLOATS 1
union ieee_double_extract
{
  struct
    {
      gmp_uint_least32_t manh:20;
      gmp_uint_least32_t exp:11;
      gmp_uint_least32_t sig:1;
      gmp_uint_least32_t manl:32;
    } s;
  double d;
};
#endif

#if HAVE_DOUBLE_IEEE_LITTLE_ENDIAN
#define _GMP_IEEE_FLOATS 1
union ieee_double_extract
{
  struct
    {
      gmp_uint_least32_t manl:32;
      gmp_uint_least32_t manh:20;
      gmp_uint_least32_t exp:11;
      gmp_uint_least32_t sig:1;
    } s;
  double d;
};
#endif

#if HAVE_DOUBLE_IEEE_BIG_ENDIAN
#define _GMP_IEEE_FLOATS 1
union ieee_double_extract
{
  struct
    {
      gmp_uint_least32_t sig:1;
      gmp_uint_least32_t exp:11;
      gmp_uint_least32_t manh:20;
      gmp_uint_least32_t manl:32;
    } s;
  double d;
};
#endif


/* Use (4.0 * ...) instead of (2.0 * ...) to work around buggy compilers
   that don't convert ulong->double correctly (eg. SunOS 4 native cc).  */
#define MP_BASE_AS_DOUBLE (4.0 * ((mp_limb_t) 1 << (GMP_NUMB_BITS - 2)))
/* Maximum number of limbs it will take to store any `double'.
   We assume doubles have 53 mantissam bits.  */
#define LIMBS_PER_DOUBLE ((53 + GMP_NUMB_BITS - 1) / GMP_NUMB_BITS + 1)

int __gmp_extract_double _PROTO ((mp_ptr, double));

#define mpn_get_d __gmpn_get_d
double mpn_get_d __GMP_PROTO ((mp_srcptr, mp_size_t, mp_size_t, long)) __GMP_ATTRIBUTE_PURE;


extern int __gmp_junk;
extern const int __gmp_0;
void __gmp_exception _PROTO ((int)) ATTRIBUTE_NORETURN;
void __gmp_divide_by_zero _PROTO ((void)) ATTRIBUTE_NORETURN;
void __gmp_sqrt_of_negative _PROTO ((void)) ATTRIBUTE_NORETURN;
void __gmp_invalid_operation _PROTO ((void)) ATTRIBUTE_NORETURN;
#define GMP_ERROR(code)   __gmp_exception (code)
#define DIVIDE_BY_ZERO    __gmp_divide_by_zero ()
#define SQRT_OF_NEGATIVE  __gmp_sqrt_of_negative ()

#if defined _LONG_LONG_LIMB
#if __GMP_HAVE_TOKEN_PASTE
#define CNST_LIMB(C) ((mp_limb_t) C##LL)
#else
#define CNST_LIMB(C) ((mp_limb_t) C/**/LL)
#endif
#else /* not _LONG_LONG_LIMB */
#if __GMP_HAVE_TOKEN_PASTE
#define CNST_LIMB(C) ((mp_limb_t) C##L)
#else
#define CNST_LIMB(C) ((mp_limb_t) C/**/L)
#endif
#endif /* _LONG_LONG_LIMB */

/* Stuff used by mpn/generic/perfsqr.c and mpz/prime_p.c */
#if GMP_NUMB_BITS == 2
#define PP 0x3					/* 3 */
#define PP_FIRST_OMITTED 5
#endif
#if GMP_NUMB_BITS == 4
#define PP 0xF					/* 3 x 5 */
#define PP_FIRST_OMITTED 7
#endif
#if GMP_NUMB_BITS == 8
#define PP 0x69					/* 3 x 5 x 7 */
#define PP_FIRST_OMITTED 11
#endif
#if GMP_NUMB_BITS == 16
#define PP 0x3AA7				/* 3 x 5 x 7 x 11 x 13 */
#define PP_FIRST_OMITTED 17
#endif
#if GMP_NUMB_BITS == 32
#define PP 0xC0CFD797L				/* 3 x 5 x 7 x 11 x ... x 29 */
#define PP_INVERTED 0x53E5645CL
#define PP_FIRST_OMITTED 31
#endif
#if GMP_NUMB_BITS == 64
#define PP CNST_LIMB(0xE221F97C30E94E1D)	/* 3 x 5 x 7 x 11 x ... x 53 */
#define PP_INVERTED CNST_LIMB(0x21CFE6CFC938B36B)
#define PP_FIRST_OMITTED 59
#endif
#ifndef PP_FIRST_OMITTED
#define PP_FIRST_OMITTED 3
#endif


/* Decimal point string, from the current C locale.  Needs <langinfo.h> for
   nl_langinfo and constants, preferrably with _GNU_SOURCE defined to get
   DECIMAL_POINT from glibc, and needs <locale.h> for localeconv, each under
   their respective #if HAVE_FOO_H.

   GLIBC recommends nl_langinfo because getting only one facet can be
   faster, apparently. */

/* DECIMAL_POINT seems to need _GNU_SOURCE defined to get it from glibc. */
#if ! defined (GMP_DECIMAL_POINT)
#define GMP_DECIMAL_POINT  (".")
#endif


/* For testing and debugging.  */
#define MPZ_CHECK_FORMAT(z)					\
  do {                                                          \
    ASSERT_ALWAYS (SIZ(z) == 0 || PTR(z)[ABSIZ(z) - 1] != 0);	\
    ASSERT_ALWAYS (ALLOC(z) >= ABSIZ(z));			\
    ASSERT_ALWAYS_MPN (PTR(z), ABSIZ(z));                       \
  } while (0)


#define MPZ_PROVOKE_REALLOC(z)					\
  do { ALLOC(z) = ABSIZ(z); } while (0)


/* Enhancement: The "mod" and "gcd_1" functions below could have
   __GMP_ATTRIBUTE_PURE, but currently (gcc 3.3) that's not supported on
   function pointers, only actual functions.  It probably doesn't make much
   difference to the gmp code, since hopefully we arrange calls so there's
   no great need for the compiler to move things around.  */

void __gmpn_cpuvec_init __GMP_PROTO ((void));

/* Get a threshold "field" from __gmpn_cpuvec, running __gmpn_cpuvec_init()
   if that hasn't yet been done (to establish the right values).  */
#define CPUVEC_THRESHOLD(field)                                               \
  ((LIKELY (__gmpn_cpuvec.initialized) ? 0 : (__gmpn_cpuvec_init (), 0)),     \
   __gmpn_cpuvec.field)


#if defined (__cplusplus)
}
#endif


#ifdef __cplusplus

/* A little helper for a null-terminated __gmp_allocate_func string.
   The destructor ensures it's freed even if an exception is thrown.
   The len field is needed by the destructor, and can be used by anyone else
   to avoid a second strlen pass over the data.

   Since our input is a C string, using strlen is correct.  Perhaps it'd be
   more C++-ish style to use std::char_traits<char>::length, but char_traits
   isn't available in gcc 2.95.4.  */

class gmp_allocated_string {
 public:
  char *str;
  size_t len;
  gmp_allocated_string(char *arg)
  {
    str = arg;
    len = std::strlen (str);
  }
  ~gmp_allocated_string()
  {
    (*__gmp_free_func) (str, len+1);
  }
};

std::istream &__gmpz_operator_in_nowhite (std::istream &, mpz_ptr, char);
int __gmp_istream_set_base (std::istream &, char &, bool &, bool &);
void __gmp_istream_set_digits (std::string &, std::istream &, char &, bool &, int);

#endif /* __cplusplus */

#endif /* __GMP_IMPL_H__ */
