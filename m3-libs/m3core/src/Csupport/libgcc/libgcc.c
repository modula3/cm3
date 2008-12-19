/*
derived from gcc/gcc/config/darwin-64.c which is LGPL, like libgcc, ok.
*/
/*
This code is for the configuration of
using the gcc backend, without an actual gcc installation.
Specifically without libgcc.
Or for linking native linker.

INT64 is gcc's "double integer" aka 64 bit integer.
UINT64 is unsigned double integer.

The backend sometimes outputs calls to functions, such
as helpers for 64 bit math.
*/

#ifdef __GNUC__
#error This code must not be compiled with gcc, that would be circular.
#endif

typedef int word_type;

#ifdef _MSC_VER
typedef __int32 INT32;
typedef __int64 INT64;
typedef unsigned __int32 UINT32;
typedef unsigned __int64 UINT64;
#else
typedef long long INT64;
typedef unsigned long long UINT64;
typedef int INT32;
typedef unsigned int UINT32;
#endif

/*
SI is "single integer" -- 32 bits
DI is "double integer" -- 64 bits
u is unsigned
l is logical -- unsigned
a is arithmetic -- signed
*/

 INT64 __ashldi3 ( INT64 x, word_type c) { return x << c; }
 INT64 __ashrdi3 ( INT64 x, word_type c) { return x >> c; }
UINT64 __lshrdi3 (UINT64 x, word_type c) { return x >> c; }

 INT64  __divdi3 ( INT64 x,  INT64 y) { return x / y; }
UINT64 __udivdi3 (UINT64 x, UINT64 y) { return x / y; }

 INT64  __moddi3 ( INT64 x,  INT64 y) { return x % y; }
UINT64 __umoddi3 (UINT64 x, UINT64 y) { return x % y; }

/* signed and unsigned multiplication and negation are the same */
INT64 __muldi3 (INT64 x, INT64 y) { return x * y; }
INT64 __negdi2 (INT64 x) { return -x; }

UINT64 __udivmoddi4 (UINT64 x, UINT64 y, UINT64 *r) { *r = x % y; return x / y; }

word_type  __cmpdi2 ( INT64 x , INT64 y) { return x < y ? 0 : x == y ? 1 : 2; }
word_type __ucmpdi2 (UINT64 x, UINT64 y) { return x < y ? 0 : x == y ? 1 : 2; }

/*
count leading zeros?
int __clzsi2 (UINT32 x) { return __builtin_clz (x); }

count trailing zeros?
int __ctzsi2 (UINT32 x) { return __builtin_ctz (x); }

1 if an odd number of bits set, 0 if an even number of bits set?
int __paritysi2 (UINT32 x) { return __builtin_parity (x); }

count set bits?
int __popcountsi2 (UINT32 x) { return __builtin_popcount (x); }
*/
