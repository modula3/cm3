#ifdef _MSC_VER
#undef _DLL
#ifndef _MT
#define _MT
#endif
#if _MSC_VER < 900
#error __int64 support is required.
/* avoid cascade */
typedef long int64;
#else
typedef __int64 int64;
typedef unsigned __int64 uint64;
#endif
#else
typedef long long int64;
typedef unsigned long long uint64;
#endif

#ifdef __cplusplus
extern "C"
{           
#endif

#if !defined(_MSC_VER) && !defined(__stdcall)
#define __stdcall /* nothing */
#endif

#ifdef _WIN32

 int64 __stdcall  m3_max64( int64 a,  int64 b) { return ((a > b) ? a : b); }
uint64 __stdcall m3_umax64(uint64 a, uint64 b) { return ((a > b) ? a : b); }
 int64 __stdcall  m3_min64( int64 a,  int64 b) { return ((a < b) ? a : b); }
uint64 __stdcall m3_umin64(uint64 a, uint64 b) { return ((a < b) ? a : b); }

uint64 __stdcall  m3_add64(uint64 a, uint64 b) { return (a + b); }
uint64 __stdcall  m3_sub64(uint64 a, uint64 b) { return (a - b); }
uint64 __stdcall m3_umul64(uint64 a, uint64 b) { return (a * b); }
uint64 __stdcall m3_udiv64(uint64 a, uint64 b) { return (a / b); }
uint64 __stdcall m3_umod64(uint64 a, uint64 b) { return (a % b); }

uint64 __stdcall m3_and64(uint64 a, uint64 b)  { return (a & b); }
uint64 __stdcall  m3_or64(uint64 a, uint64 b)  { return (a | b); }
uint64 __stdcall m3_xor64(uint64 a, uint64 b)  { return (a ^ b); }

uint64 __stdcall   m3_shift_left64(uint64 a, uint64 b)  { return (a << b); }
uint64 __stdcall  m3_shift_right64(uint64 a, uint64 b)  { return (a >> b); }
uint64 _rotl64(uint64 value, int shift);
uint64 _rotr64(uint64 value, int shift);
#pragma intrinsic(_rotl64)
#pragma intrinsic(_rotr64)
uint64 __stdcall  m3_rotate_left64(uint64 a, uint64 b)  { return _rotl64(a, (int)b); }
uint64 __stdcall m3_rotate_right64(uint64 a, uint64 b)  { return _rotr64(a, (int)b); }

uint64 __stdcall m3_shift64(uint64 a, int64 b)
{
    if (b >= 64 || b <= -64)
        a = 0;
    else if (b > 0)
        a <<= b;
    else if (b < 0)
        a >>= -b;
    return a;
}

uint64 __stdcall m3_rotate64(uint64 a, int64 b)
{
    b &= 63;
    if (b > 0)
        a = _rotl64(a, (int)b);
    else if (b < 0)
        a = _rotr64(a, (int)-b);
    return a;
}

int64 __stdcall m3_neg64(int64 a)    { return -a; }
int64 __stdcall m3_abs64(int64 a)    { return ((a < 0) ? -a : a); }
uint64 __stdcall m3_not64(uint64 a)  { return ~a; }

#define M3_POS(T, a) (((T)-((a) + 1)) + 1)

int64 __stdcall m3_div64(int64 b, int64 a)
{
  typedef  int64 ST; /* signed type */
  typedef uint64 UT; /* unsigned type */
  int aneg = (a < 0);
  int bneg = (b < 0);
  if (aneg == bneg || a == 0 || b == 0)
    return (a / b);
  else
  {
    /* round negative result down by rounding positive result up
       unsigned math is much better defined, see gcc -Wstrict-overflow=4 */
    UT ua = (aneg ? M3_POS(UT, a) : (UT)a);
    UT ub = (bneg ? M3_POS(UT, b) : (UT)b);
    return -(ST)((ua + ub - 1) / ub);
  }
}

int64 __stdcall m3_mod64(int64 b, int64 a)
{
  typedef  int64 ST; /* signed type */
  typedef uint64 UT; /* unsigned type */
  int aneg = (a < 0);
  int bneg = (b < 0);
  if (aneg == bneg || a == 0 || b == 0)
    return (a % b);
  else
  {
    UT ua = (aneg ? M3_POS(UT, a) : (UT)a);
    UT ub = (bneg ? M3_POS(UT, b) : (UT)b);
    a = (ST)(ub - 1 - (ua + ub - 1) % ub);
    return (bneg ? -a : a);
  }
}

#endif

#ifdef __cplusplus
} /* extern "C" */
#endif
