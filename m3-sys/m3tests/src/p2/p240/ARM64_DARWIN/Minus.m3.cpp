// library:pgm
// source_base_name:Minus
// target_name:Minus.m3.cpp
 /* set_runtime_proc */
 /* begin unit */
#ifdef __SUNPRO_C
#pragma error_messages(off, E_STATEMENT_NOT_REACHED)
#endif
#define m3_eq(T, x, y) (((T)(x)) == ((T)(y)))
#define m3_ne(T, x, y) (((T)(x)) != ((T)(y)))
#define m3_gt(T, x, y) (((T)(x)) > ((T)(y)))
#define m3_ge(T, x, y) (((T)(x)) >= ((T)(y)))
#define m3_lt(T, x, y) (((T)(x)) < ((T)(y)))
#define m3_le(T, x, y) (((T)(x)) <= ((T)(y)))
#define m3_check_range(T, value, low, high) (((T)(value)) < ((T)(low)) || ((T)(high)) < ((T)(value)))
#define m3_xor(T, x, y) (((T)(x)) ^ ((T)(y)))
template<class T, class U> inline T m3_loophole(U u) { return *(T*)&u; }
#ifdef _MSC_VER
#define _CRT_SECURE_NO_DEPRECATE 1
#define _CRT_NONSTDC_NO_DEPRECATE 1
// pragma warming(error) is documented as turning a diagnostic into an error but as of year2022 it does not work.
// Error 4739 catches m3front problems. See M3CG_MultiPass.TypeVersusSize.
#pragma warning(error:4739)   // reference to variable exceeds its storage space
#pragma warning(disable:4616) /* there is no warning x (unavoidable if targeting multiple compiler versions) */
#pragma warning(disable:4619) /* there is no warning x (unavoidable if targeting multiple compiler versions) */
#pragma warning(disable:4100) /* unused parameter */
#pragma warning(disable:4115) /* named type definition in parentheses */
#pragma warning(disable:4127) /* conditional expression is constant */
#pragma warning(disable:4146) /* unary minus operator applied to unsigned type, result still unsigned */
#pragma warning(disable:4201) /* nonstandard extension: nameless struct/union */
#pragma warning(disable:4214) /* nonstandard extension: bitfield other than int */
#pragma warning(disable:4209) /* nonstandard extension: benign re-typedef */
#pragma warning(disable:4226) /* nonstandard extension: __export */
#pragma warning(disable:4242) /* 'return': conversion from '' to '', possible loss of data */
#pragma warning(disable:4244) /* 'return': conversion from '' to '', possible loss of data */
#pragma warning(disable:4255) /* () change to (void) */
#pragma warning(disable:4310) /* cast truncates constant value */
#pragma warning(disable:4514) /* unused inline function removed */
#pragma warning(disable:4668) /* #if of undefined symbol */
#pragma warning(disable:4705) /* statement has no effect for merely using assert() at -W4 */
#pragma warning(disable:4715) /* not all control paths return a value */
#pragma warning(disable:4716) /* must return a value */
#pragma warning(disable:4820) /* padding inserted */
#pragma warning(disable:5045) /* Compiler will insert Spectre mitigation for memory load if /Qspectre switch specified */

#pragma warning(error:4700)   // unitialized local variable used

#endif
#define ADDRESS ADDRESS
typedef char* ADDRESS;
typedef char* STRUCT;
typedef signed char INT8;
typedef unsigned char UINT8;
typedef short INT16;
typedef unsigned short UINT16;
typedef int INT32;
typedef unsigned int UINT32;
#if !defined(_LONGLONG) && (defined(_MSC_VER) || defined(__DECC) || defined(__DECCXX) || defined(__int64))
typedef __int64 INT64;
typedef unsigned __int64 UINT64;
#define  INT64_(x) x##I64
#define UINT64_(x) x##UI64
#else
typedef long long INT64;
typedef unsigned long long UINT64;
#define  INT64_(x) x##LL
#define UINT64_(x) x##ULL
#endif
#if defined(_WIN64)
typedef UINT64 size_t;
#elif defined(_WIN32)
typedef unsigned size_t;
#elif defined(__SIZE_TYPE__)
typedef __SIZE_TYPE__ size_t;
#elif defined(__APPLE__) /*|| defined(_LP64) || defined(__LP64__)*/
typedef unsigned long size_t;
#else
#include <stddef.h>
#endif
/* http://c.knowcoding.com/view/23699-portable-alloca.html */
/* Find a good version of alloca. */
#ifndef alloca
# ifdef __GNUC__
#  define alloca __builtin_alloca
# elif defined(_MSC_VER)
#ifdef __cplusplus
extern "C" {
#endif
   void * __cdecl _alloca(size_t size);
#ifdef __cplusplus
} /* extern "C" */
#endif
#  define alloca _alloca
# else
#  include <alloca.h>
# endif
#endif
#define REAL REAL
#define LONGREAL LONGREAL
#define EXTENDED EXTENDED
//#include <cmath>
#include <limits>
#define INFINITY (std::numeric_limits<double>::infinity() )
#define Infinity INFINITY
#define NAN (std::numeric_limits<double>::quiet_NaN() )
#define NaN NAN
typedef float REAL;
typedef double LONGREAL;
#ifdef __cplusplus
extern "C" {
#endif
#if !defined(_WIN32) && !defined(__CYGWIN__)
#undef __cdecl
#undef __stdcall
#define __cdecl /* nothing */
#define __stdcall /* nothing */
#endif
#define STRUCT(n) struct_##n##_t
#define STRUCT1(n) typedef struct { volatile char a[n]; }     STRUCT(n);
#define STRUCT2(n) typedef struct { volatile short a[n/2]; }  STRUCT(n);
#define STRUCT4(n) typedef struct { volatile int a[n/4]; }    STRUCT(n);
#define STRUCT8(n) typedef struct { volatile UINT64 a[n/8]; } STRUCT(n);
void __cdecl m3_memcpy(void* dest, const void* source, size_t n);
void __cdecl m3_memmove(void* dest, const void* source, size_t n);
void __cdecl m3_memset(void* dest, int fill, size_t count);
int  __cdecl m3_memcmp(const void* a, const void* b, size_t n);
} /* extern "C" */
struct _M3Exc { void* act; };
#ifdef _MSC_VER
static __declspec(thread) void* _m3_caught;
#else
static __thread void* _m3_caught __attribute__((unused));
#endif
extern "C" {

typedef double EXTENDED;
 /* begin: DeclareTypes */

#ifndef T195C2A74
#define T195C2A74 T195C2A74
/*type_typedef*/typedef INT64 T195C2A74;

#endif

#ifndef INTEGER
#define INTEGER INTEGER
typedef T195C2A74 INTEGER;
#endif

#ifndef CARDINAL
#define CARDINAL CARDINAL
typedef T195C2A74 CARDINAL;
#endif
 /* DeclareTypes_FlushOnce size:0 */

#ifndef T97E237E2
#define T97E237E2 T97E237E2
/*type_typedef*/typedef UINT64 T97E237E2;

#endif

#ifndef WORD_T
#define WORD_T WORD_T
typedef T97E237E2 WORD_T;
#endif
 /* DeclareTypes_FlushOnce size:0 */

#ifndef T5562176
#define T5562176 T5562176
/*type_typedef*/typedef INT64 T5562176;

#endif

#ifndef LONGINT
#define LONGINT LONGINT
typedef T5562176 LONGINT;
#endif
 /* DeclareTypes_FlushOnce size:0 */

#ifndef T48E16572
#define T48E16572 T48E16572
/*type_typedef*/typedef float T48E16572;

#endif

#ifndef REAL
#define REAL REAL
typedef T48E16572 REAL;
#endif
 /* DeclareTypes_FlushOnce size:0 */

#ifndef T94FE32F6
#define T94FE32F6 T94FE32F6
/*type_typedef*/typedef double T94FE32F6;

#endif

#ifndef LONGREAL
#define LONGREAL LONGREAL
typedef T94FE32F6 LONGREAL;
#endif
 /* DeclareTypes_FlushOnce size:0 */

#ifndef T9EE024E3
#define T9EE024E3 T9EE024E3
/*type_typedef*/typedef EXTENDED T9EE024E3;

#endif

#ifndef EXTENDED
#define EXTENDED EXTENDED
typedef T9EE024E3 EXTENDED;
#endif
 /* DeclareTypes_FlushOnce size:0 */

#ifndef T1E59237D
#define T1E59237D T1E59237D
/*type_typedef*/typedef UINT8 T1E59237D;

#endif

#ifndef BOOLEAN
#define BOOLEAN BOOLEAN
typedef T1E59237D BOOLEAN;
#endif
 /* DeclareTypes_FlushOnce size:0 */

#ifndef T56E16863
#define T56E16863 T56E16863
/*type_typedef*/typedef UINT8 T56E16863;

#endif

#ifndef UCHAR
#define UCHAR UCHAR
typedef T56E16863 UCHAR;
#endif
 /* DeclareTypes_FlushOnce size:0 */

#ifndef WIDECHAR
#define WIDECHAR WIDECHAR
/*type_typedef*/typedef UINT16 WIDECHAR;

#endif

#ifndef T1541F475
#define T1541F475 T1541F475
/*1addressType_define*/typedef ADDRESS T1541F475;

#endif

#ifndef MUTEX
#define MUTEX MUTEX
typedef T1541F475 MUTEX;
#endif

#ifndef T50F86574
#define T50F86574 T50F86574
/*1addressType_define*/typedef ADDRESS T50F86574;

#endif

#ifndef TEXT
#define TEXT TEXT
typedef T50F86574 TEXT;
#endif

#ifndef T9D8FB489
#define T9D8FB489 T9D8FB489
/*1addressType_define*/typedef ADDRESS T9D8FB489;

#endif

#ifndef ROOT
#define ROOT ROOT
typedef T9D8FB489 ROOT;
#endif

#ifndef T898EA789
#define T898EA789 T898EA789
/*1addressType_define*/typedef ADDRESS T898EA789;

#endif

#ifndef UNTRACED_ROOT
#define UNTRACED_ROOT UNTRACED_ROOT
typedef T898EA789 UNTRACED_ROOT;
#endif

#ifndef T1C1C45E6
#define T1C1C45E6 T1C1C45E6
/*1addressType_define*/typedef ADDRESS T1C1C45E6;

#endif

#ifndef REFANY
#define REFANY REFANY
typedef T1C1C45E6 REFANY;
#endif

#ifndef T8402063
#define T8402063 T8402063
/*1addressType_define*/typedef ADDRESS T8402063;

#endif

#ifndef ADDRESS
#define ADDRESS ADDRESS
typedef T8402063 ADDRESS;
#endif

#ifndef T48EC756E
#define T48EC756E T48EC756E
/*1addressType_define*/typedef ADDRESS T48EC756E;

#endif

#ifndef M3_NULL_T
#define M3_NULL_T M3_NULL_T
typedef T48EC756E M3_NULL_T;
#endif
 /* declare_subrange */
/*subrange_define*/typedef INT8 T66A2A904_8;
 /* declare_subrange */
/*subrange_define*/typedef INT32 TADC6066D_32;
 /* declare_subrange */
/*subrange_define*/typedef UINT16 TA4B285DE_16;
 /* declare_subrange */
/*subrange_define*/typedef INT64 T839F750E_64;
 /* declare_subrange */
/*subrange_define*/typedef INT16 T7300E1E8_16;
 /* declare_subrange */
/*subrange_define*/typedef UINT32 T6FA2E87D_32;
 /* declare_subrange */
/*subrange_define*/typedef UINT8 TB5B30AA_8;
 /* declare_proctype */

#if 0 /* avoid type hash collions */
typedef 
INTEGER(__cdecl*TEE9B4E5D)(void);
#else
typedef void (__cdecl*TEE9B4E5D)(void);
#endif
 /* declare_proctype */

#if 0 /* avoid type hash collions */
typedef 
T66A2A904_8(__cdecl*T2FD24D9D)(void);
#else
typedef void (__cdecl*T2FD24D9D)(void);
#endif
 /* declare_proctype */
 /* declare_formal */

#ifndef Minus__INT8
#define Minus__INT8 Minus__INT8
typedef T66A2A904_8 Minus__INT8;
#endif
 /* declare_formal */
 /* declare_proctype */
 /* declare_formal */
 /* declare_formal */
 /* declare_proctype */

#if 0 /* avoid type hash collions */
typedef 
INT64(__cdecl*TBF2A8E93)(void);
#else
typedef void (__cdecl*TBF2A8E93)(void);
#endif
 /* declare_proctype */
 /* declare_formal */
 /* declare_formal */

#ifndef Minus__UINT64
#define Minus__UINT64 Minus__UINT64
typedef INT64 Minus__UINT64;
#endif
 /* declare_proctype */
 /* declare_formal */
 /* declare_formal */

#ifndef Minus__INT32
#define Minus__INT32 Minus__INT32
typedef TADC6066D_32 Minus__INT32;
#endif
 /* declare_proctype */
 /* declare_formal */
 /* declare_formal */
 /* declare_subrange */
/*subrange_define*/typedef INT64 T9CED36E7_64;
 /* declare_proctype */
 /* declare_formal */
 /* declare_formal */

#ifndef LONGCARD
#define LONGCARD LONGCARD
typedef T9CED36E7_64 LONGCARD;
#endif
 /* declare_proctype */
 /* declare_formal */
 /* declare_formal */

#ifndef Minus__UINT16
#define Minus__UINT16 Minus__UINT16
typedef TA4B285DE_16 Minus__UINT16;
#endif
 /* declare_proctype */
 /* declare_formal */
 /* declare_formal */
 /* declare_proctype */
 /* declare_formal */
 /* declare_formal */
 /* declare_proctype */
 /* declare_formal */
 /* declare_formal */
 /* declare_proctype */
 /* declare_formal */
 /* declare_formal */

#ifndef Minus__INT64
#define Minus__INT64 Minus__INT64
typedef T839F750E_64 Minus__INT64;
#endif
 /* declare_proctype */
 /* declare_formal */
 /* declare_formal */

#ifndef Minus__INT16
#define Minus__INT16 Minus__INT16
typedef T7300E1E8_16 Minus__INT16;
#endif
 /* declare_proctype */
 /* declare_formal */
 /* declare_formal */
 /* declare_proctype */
 /* declare_formal */
 /* declare_formal */
 /* declare_proctype */
 /* declare_formal */
 /* declare_formal */
 /* declare_proctype */
 /* declare_formal */
 /* declare_formal */

#ifndef Minus__UINT32
#define Minus__UINT32 Minus__UINT32
typedef T6FA2E87D_32 Minus__UINT32;
#endif
 /* declare_proctype */
 /* declare_formal */
 /* declare_formal */
 /* declare_proctype */
 /* declare_formal */
 /* declare_formal */

#ifndef Minus__UINT8
#define Minus__UINT8 Minus__UINT8
typedef TB5B30AA_8 Minus__UINT8;
#endif
 /* declare_proctype */
 /* declare_formal */
 /* declare_formal */
 /* declare_proctype */
 /* declare_formal */
 /* declare_formal */
 /* declare_proctype */
 /* declare_formal */
 /* declare_formal */
 /* declare_proctype */
 /* declare_formal */
 /* declare_formal */
 /* declare_proctype */
 /* declare_formal */
 /* declare_formal */
 /* declare_proctype */
 /* declare_formal */
 /* declare_formal */
 /* declare_proctype */
 /* declare_formal */
 /* declare_formal */
 /* declare_proctype */
 /* declare_formal */
 /* declare_formal */
 /* declare_proctype */
 /* declare_formal */
 /* declare_formal */
 /* declare_proctype */
 /* declare_formal */
 /* declare_formal */
 /* declare_proctype */
 /* declare_formal */
 /* declare_formal */
 /* declare_proctype */
 /* declare_formal */
 /* declare_formal */
 /* declare_proctype */

#if 0 /* avoid type hash collions */
typedef 
double(__cdecl*TE4E28466)(void);
#else
typedef void (__cdecl*TE4E28466)(void);
#endif
 /* declare_proctype */
 /* declare_formal */

#ifndef Minus__FLOAT64
#define Minus__FLOAT64 Minus__FLOAT64
typedef double Minus__FLOAT64;
#endif
 /* declare_formal */
 /* declare_proctype */

#if 0 /* avoid type hash collions */
typedef 
TADC6066D_32(__cdecl*TFF82092F)(void);
#else
typedef void (__cdecl*TFF82092F)(void);
#endif
 /* declare_proctype */
 /* declare_formal */
 /* declare_formal */
 /* declare_proctype */
 /* declare_formal */
 /* declare_formal */
 /* declare_proctype */
 /* declare_formal */
 /* declare_formal */
 /* declare_proctype */
 /* declare_formal */
 /* declare_formal */
 /* declare_proctype */
 /* declare_formal */
 /* declare_formal */
 /* declare_proctype */
 /* declare_formal */
 /* declare_formal */
 /* declare_proctype */
 /* declare_formal */
 /* declare_formal */
 /* declare_proctype */
 /* declare_formal */
 /* declare_formal */
 /* declare_proctype */
 /* declare_formal */
 /* declare_formal */
 /* declare_proctype */
 /* declare_formal */
 /* declare_formal */
 /* declare_proctype */
 /* declare_formal */
 /* declare_formal */
 /* declare_proctype */
 /* declare_formal */
 /* declare_formal */
 /* declare_proctype */
 /* declare_formal */
 /* declare_formal */
 /* declare_proctype */
 /* declare_formal */
 /* declare_formal */
 /* declare_proctype */
 /* declare_formal */
 /* declare_formal */
 /* declare_proctype */
 /* declare_formal */
 /* declare_formal */
 /* declare_proctype */
 /* declare_formal */
 /* declare_formal */
 /* declare_proctype */
 /* declare_formal */
 /* declare_formal */
 /* declare_proctype */
 /* declare_formal */
 /* declare_formal */
 /* declare_proctype */
 /* declare_formal */
 /* declare_formal */
 /* declare_proctype */
 /* declare_formal */
 /* declare_formal */
 /* declare_proctype */
 /* declare_formal */
 /* declare_formal */
 /* declare_proctype */
 /* declare_formal */
 /* declare_formal */
 /* declare_proctype */
 /* declare_formal */
 /* declare_formal */
 /* declare_proctype */
 /* declare_formal */
 /* declare_formal */
 /* declare_proctype */
 /* declare_formal */
 /* declare_formal */
 /* declare_proctype */
 /* declare_formal */
 /* declare_formal */
 /* declare_proctype */
 /* declare_formal */
 /* declare_formal */
 /* declare_proctype */
 /* declare_formal */
 /* declare_formal */
 /* declare_proctype */
 /* declare_formal */
 /* declare_formal */
 /* declare_proctype */

#if 0 /* avoid type hash collions */
typedef 
TA4B285DE_16(__cdecl*TFD07BB9B)(void);
#else
typedef void (__cdecl*TFD07BB9B)(void);
#endif
 /* declare_proctype */
 /* declare_formal */
 /* declare_formal */
 /* declare_proctype */
 /* declare_formal */
 /* declare_formal */
 /* declare_proctype */
 /* declare_formal */
 /* declare_formal */
 /* declare_proctype */
 /* declare_formal */
 /* declare_formal */
 /* declare_proctype */
 /* declare_formal */
 /* declare_formal */
 /* declare_proctype */
 /* declare_formal */
 /* declare_formal */
 /* declare_proctype */
 /* declare_formal */
 /* declare_formal */
 /* declare_proctype */
 /* declare_formal */
 /* declare_formal */
 /* declare_proctype */
 /* declare_formal */
 /* declare_formal */
 /* declare_proctype */
 /* declare_formal */
 /* declare_formal */
 /* declare_proctype */
 /* declare_formal */
 /* declare_formal */
 /* declare_proctype */
 /* declare_formal */
 /* declare_formal */
 /* declare_proctype */
 /* declare_formal */
 /* declare_formal */
 /* declare_proctype */
 /* declare_formal */
 /* declare_formal */
 /* declare_proctype */
 /* declare_formal */
 /* declare_formal */
 /* declare_proctype */
 /* declare_formal */
 /* declare_formal */
 /* declare_proctype */
 /* declare_formal */
 /* declare_formal */
 /* declare_proctype */
 /* declare_formal */
 /* declare_formal */
 /* declare_proctype */
 /* declare_formal */
 /* declare_formal */
 /* declare_proctype */
 /* declare_formal */
 /* declare_formal */
 /* declare_proctype */
 /* declare_formal */
 /* declare_formal */
 /* declare_proctype */
 /* declare_formal */
 /* declare_formal */
 /* declare_proctype */
 /* declare_formal */
 /* declare_formal */
 /* declare_proctype */
 /* declare_formal */
 /* declare_formal */
 /* declare_proctype */
 /* declare_formal */
 /* declare_formal */
 /* declare_proctype */
 /* declare_formal */
 /* declare_formal */
 /* declare_proctype */
 /* declare_formal */
 /* declare_formal */
 /* declare_proctype */
 /* declare_formal */
 /* declare_formal */
 /* declare_proctype */
 /* declare_formal */
 /* declare_formal */
 /* declare_proctype */
 /* declare_formal */
 /* declare_formal */
 /* declare_proctype */
 /* declare_formal */
 /* declare_formal */
 /* declare_proctype */
 /* declare_formal */
 /* declare_formal */
 /* declare_proctype */
 /* declare_formal */
 /* declare_formal */
 /* declare_proctype */
 /* declare_formal */
 /* declare_formal */
 /* declare_proctype */
 /* declare_formal */
 /* declare_formal */
 /* declare_proctype */
 /* declare_formal */
 /* declare_formal */
 /* declare_proctype */
 /* declare_formal */
 /* declare_formal */
 /* declare_proctype */
 /* declare_formal */
 /* declare_formal */
 /* declare_proctype */
 /* declare_formal */
 /* declare_formal */
 /* declare_proctype */
 /* declare_formal */
 /* declare_formal */
 /* declare_proctype */
 /* declare_formal */
 /* declare_formal */
 /* declare_proctype */

#if 0 /* avoid type hash collions */
typedef 
float(__cdecl*T263D7EB0)(void);
#else
typedef void (__cdecl*T263D7EB0)(void);
#endif
 /* declare_proctype */
 /* declare_formal */

#ifndef Minus__FLOAT32
#define Minus__FLOAT32 Minus__FLOAT32
typedef float Minus__FLOAT32;
#endif
 /* declare_formal */
 /* declare_proctype */

#if 0 /* avoid type hash collions */
typedef 
T7300E1E8_16(__cdecl*T3F787B7)(void);
#else
typedef void (__cdecl*T3F787B7)(void);
#endif
 /* declare_proctype */
 /* declare_formal */
 /* declare_formal */
 /* declare_proctype */
 /* declare_formal */
 /* declare_formal */
 /* declare_proctype */
 /* declare_formal */
 /* declare_formal */
 /* declare_proctype */
 /* declare_formal */
 /* declare_formal */
 /* declare_proctype */
 /* declare_formal */
 /* declare_formal */
 /* declare_proctype */
 /* declare_formal */
 /* declare_formal */
 /* declare_proctype */
 /* declare_formal */
 /* declare_formal */
 /* declare_proctype */
 /* declare_formal */
 /* declare_formal */
 /* declare_proctype */
 /* declare_formal */
 /* declare_formal */
 /* declare_proctype */
 /* declare_formal */
 /* declare_formal */
 /* declare_proctype */
 /* declare_formal */
 /* declare_formal */
 /* declare_proctype */
 /* declare_formal */
 /* declare_formal */
 /* declare_proctype */
 /* declare_formal */
 /* declare_formal */
 /* declare_proctype */
 /* declare_formal */
 /* declare_formal */
 /* declare_proctype */
 /* declare_formal */
 /* declare_formal */
 /* declare_proctype */
 /* declare_formal */
 /* declare_formal */
 /* declare_proctype */
 /* declare_formal */
 /* declare_formal */
 /* declare_proctype */
 /* declare_formal */
 /* declare_formal */
 /* declare_proctype */
 /* declare_formal */
 /* declare_formal */
 /* declare_proctype */

#if 0 /* avoid type hash collions */
typedef 
WORD_T(__cdecl*T5C4C299E)(void);
#else
typedef void (__cdecl*T5C4C299E)(void);
#endif
 /* declare_proctype */
 /* declare_formal */
 /* declare_formal */
 /* declare_proctype */
 /* declare_formal */
 /* declare_formal */
 /* declare_proctype */
 /* declare_formal */
 /* declare_formal */
 /* declare_proctype */
 /* declare_formal */
 /* declare_formal */
 /* declare_proctype */
 /* declare_formal */
 /* declare_formal */
 /* declare_proctype */
 /* declare_formal */
 /* declare_formal */
 /* declare_proctype */
 /* declare_formal */
 /* declare_formal */
 /* declare_proctype */
 /* declare_formal */
 /* declare_formal */
 /* declare_proctype */
 /* declare_formal */
 /* declare_formal */
 /* declare_proctype */
 /* declare_formal */
 /* declare_formal */
 /* declare_proctype */
 /* declare_formal */
 /* declare_formal */
 /* declare_proctype */
 /* declare_formal */
 /* declare_formal */
 /* declare_proctype */
 /* declare_formal */
 /* declare_formal */
 /* declare_proctype */
 /* declare_formal */
 /* declare_formal */
 /* declare_proctype */
 /* declare_formal */
 /* declare_formal */
 /* declare_proctype */
 /* declare_formal */
 /* declare_formal */
 /* declare_proctype */
 /* declare_formal */
 /* declare_formal */
 /* declare_proctype */
 /* declare_formal */
 /* declare_formal */
 /* declare_proctype */
 /* declare_formal */
 /* declare_formal */
 /* declare_proctype */

#if 0 /* avoid type hash collions */
typedef 
T6FA2E87D_32(__cdecl*TB705F362)(void);
#else
typedef void (__cdecl*TB705F362)(void);
#endif
 /* declare_proctype */
 /* declare_formal */
 /* declare_formal */
 /* declare_proctype */
 /* declare_formal */
 /* declare_formal */
 /* declare_proctype */
 /* declare_formal */
 /* declare_formal */
 /* declare_proctype */
 /* declare_formal */
 /* declare_formal */
 /* declare_proctype */
 /* declare_formal */
 /* declare_formal */
 /* declare_proctype */
 /* declare_formal */
 /* declare_formal */
 /* declare_proctype */
 /* declare_formal */
 /* declare_formal */
 /* declare_proctype */
 /* declare_formal */
 /* declare_formal */
 /* declare_proctype */
 /* declare_formal */
 /* declare_formal */
 /* declare_proctype */
 /* declare_formal */
 /* declare_formal */
 /* declare_proctype */
 /* declare_formal */
 /* declare_formal */
 /* declare_proctype */
 /* declare_formal */
 /* declare_formal */
 /* declare_proctype */
 /* declare_formal */
 /* declare_formal */
 /* declare_proctype */
 /* declare_formal */
 /* declare_formal */
 /* declare_proctype */
 /* declare_formal */
 /* declare_formal */
 /* declare_proctype */
 /* declare_formal */
 /* declare_formal */
 /* declare_proctype */
 /* declare_formal */
 /* declare_formal */
 /* declare_proctype */
 /* declare_formal */
 /* declare_formal */
 /* declare_proctype */
 /* declare_formal */
 /* declare_formal */
 /* declare_proctype */

#if 0 /* avoid type hash collions */
typedef 
TB5B30AA_8(__cdecl*T38BAD830)(void);
#else
typedef void (__cdecl*T38BAD830)(void);
#endif
 /* declare_proctype */
 /* declare_formal */
 /* declare_formal */
 /* declare_proctype */
 /* declare_formal */
 /* declare_formal */
 /* declare_proctype */
 /* declare_formal */
 /* declare_formal */
 /* declare_proctype */
 /* declare_formal */
 /* declare_formal */
 /* declare_proctype */
 /* declare_formal */
 /* declare_formal */
 /* declare_proctype */
 /* declare_formal */
 /* declare_formal */
 /* declare_proctype */
 /* declare_formal */
 /* declare_formal */
 /* declare_proctype */
 /* declare_formal */
 /* declare_formal */
 /* declare_proctype */
 /* declare_formal */
 /* declare_formal */
 /* declare_proctype */
 /* declare_formal */
 /* declare_formal */
 /* declare_proctype */
 /* declare_formal */
 /* declare_formal */
 /* declare_proctype */
 /* declare_formal */
 /* declare_formal */
 /* declare_proctype */
 /* declare_formal */
 /* declare_formal */
 /* declare_proctype */
 /* declare_formal */
 /* declare_formal */
 /* declare_proctype */
 /* declare_formal */
 /* declare_formal */
 /* declare_proctype */
 /* declare_formal */
 /* declare_formal */
 /* declare_proctype */
 /* declare_formal */
 /* declare_formal */
 /* declare_proctype */
 /* declare_formal */
 /* declare_formal */
 /* declare_proctype */
 /* declare_formal */
 /* declare_formal */
 /* declare_proctype */
 /* declare_formal */
 /* declare_formal */
 /* declare_proctype */
 /* declare_formal */
 /* declare_formal */
 /* declare_proctype */
 /* declare_formal */
 /* declare_formal */
 /* declare_record */
 /* declare_record */
 /* declare_field */
 /* declare_field */
 /* declare_field */
 /* declare_field */
 /* declare_field */
 /* declare_field */
 /* declare_field */
 /* declare_field */
 /* declare_field */
 /* declare_field */
 /* declare_field */
 /* declare_field */
 /* declare_field */
 /* declare_field */
 /* declare_field */
 /* declare_field */
 /* DeclareTypes_FlushOnce size:182 */

#if 0 /* avoid type hash collions */
typedef 
INTEGER(__cdecl*TED2C8CAC)(Minus__INT8,Minus__INT8);
#else
typedef void (__cdecl*TED2C8CAC)(void);
#endif

#if 0 /* avoid type hash collions */
typedef 
T66A2A904_8(__cdecl*T2C658F6C)(Minus__INT8,Minus__INT8);
#else
typedef void (__cdecl*T2C658F6C)(void);
#endif

#if 0 /* avoid type hash collions */
typedef 
INT64(__cdecl*T263C553D)(Minus__INT8,Minus__UINT64);
#else
typedef void (__cdecl*T263C553D)(void);
#endif

#if 0 /* avoid type hash collions */
typedef 
INTEGER(__cdecl*TBEDF1BFD)(Minus__INT8,Minus__INT32);
#else
typedef void (__cdecl*TBEDF1BFD)(void);
#endif

#if 0 /* avoid type hash collions */
typedef 
T66A2A904_8(__cdecl*T7F96183D)(Minus__INT8,Minus__INT32);
#else
typedef void (__cdecl*T7F96183D)(void);
#endif

#if 0 /* avoid type hash collions */
typedef 
INT64(__cdecl*TFB68FC67)(Minus__INT8,LONGCARD);
#else
typedef void (__cdecl*TFB68FC67)(void);
#endif

#if 0 /* avoid type hash collions */
typedef 
INTEGER(__cdecl*T13F675A1)(Minus__INT8,Minus__UINT16);
#else
typedef void (__cdecl*T13F675A1)(void);
#endif

#if 0 /* avoid type hash collions */
typedef 
T66A2A904_8(__cdecl*TD2BF7661)(Minus__INT8,Minus__UINT16);
#else
typedef void (__cdecl*TD2BF7661)(void);
#endif

#if 0 /* avoid type hash collions */
typedef 
INTEGER(__cdecl*T505B8499)(Minus__INT8,INTEGER);
#else
typedef void (__cdecl*T505B8499)(void);
#endif

#if 0 /* avoid type hash collions */
typedef 
T66A2A904_8(__cdecl*T91128759)(Minus__INT8,INTEGER);
#else
typedef void (__cdecl*T91128759)(void);
#endif

#if 0 /* avoid type hash collions */
typedef 
INT64(__cdecl*TE90D5FA6)(Minus__INT8,Minus__INT64);
#else
typedef void (__cdecl*TE90D5FA6)(void);
#endif

#if 0 /* avoid type hash collions */
typedef 
INTEGER(__cdecl*T811E024F)(Minus__INT8,Minus__INT16);
#else
typedef void (__cdecl*T811E024F)(void);
#endif

#if 0 /* avoid type hash collions */
typedef 
T66A2A904_8(__cdecl*T4057018F)(Minus__INT8,Minus__INT16);
#else
typedef void (__cdecl*T4057018F)(void);
#endif

#if 0 /* avoid type hash collions */
typedef 
INTEGER(__cdecl*T2C2DD441)(Minus__INT8,CARDINAL);
#else
typedef void (__cdecl*T2C2DD441)(void);
#endif

#if 0 /* avoid type hash collions */
typedef 
T66A2A904_8(__cdecl*TED64D781)(Minus__INT8,CARDINAL);
#else
typedef void (__cdecl*TED64D781)(void);
#endif

#if 0 /* avoid type hash collions */
typedef 
INTEGER(__cdecl*TF97BB3EA)(Minus__INT8,Minus__UINT32);
#else
typedef void (__cdecl*TF97BB3EA)(void);
#endif

#if 0 /* avoid type hash collions */
typedef 
T66A2A904_8(__cdecl*T3832B02A)(Minus__INT8,Minus__UINT32);
#else
typedef void (__cdecl*T3832B02A)(void);
#endif

#if 0 /* avoid type hash collions */
typedef 
INTEGER(__cdecl*TFBF83D35)(Minus__INT8,Minus__UINT8);
#else
typedef void (__cdecl*TFBF83D35)(void);
#endif

#if 0 /* avoid type hash collions */
typedef 
T66A2A904_8(__cdecl*T3AB13EF5)(Minus__INT8,Minus__UINT8);
#else
typedef void (__cdecl*T3AB13EF5)(void);
#endif

#if 0 /* avoid type hash collions */
typedef 
INT64(__cdecl*TF86EAF82)(Minus__UINT64,Minus__INT8);
#else
typedef void (__cdecl*TF86EAF82)(void);
#endif

#if 0 /* avoid type hash collions */
typedef 
INT64(__cdecl*T62CFB6DD)(Minus__UINT64,Minus__UINT64);
#else
typedef void (__cdecl*T62CFB6DD)(void);
#endif

#if 0 /* avoid type hash collions */
typedef 
INT64(__cdecl*TAB9D38D3)(Minus__UINT64,Minus__INT32);
#else
typedef void (__cdecl*TAB9D38D3)(void);
#endif

#if 0 /* avoid type hash collions */
typedef 
INT64(__cdecl*TBF9B1F87)(Minus__UINT64,LONGCARD);
#else
typedef void (__cdecl*TBF9B1F87)(void);
#endif

#if 0 /* avoid type hash collions */
typedef 
INT64(__cdecl*T6B4568F)(Minus__UINT64,Minus__UINT16);
#else
typedef void (__cdecl*T6B4568F)(void);
#endif

#if 0 /* avoid type hash collions */
typedef 
INT64(__cdecl*T4519A7B7)(Minus__UINT64,INTEGER);
#else
typedef void (__cdecl*T4519A7B7)(void);
#endif

#if 0 /* avoid type hash collions */
typedef 
INT64(__cdecl*TADFEBC46)(Minus__UINT64,Minus__INT64);
#else
typedef void (__cdecl*TADFEBC46)(void);
#endif

#if 0 /* avoid type hash collions */
typedef 
INT64(__cdecl*T945C2161)(Minus__UINT64,Minus__INT16);
#else
typedef void (__cdecl*T945C2161)(void);
#endif

#if 0 /* avoid type hash collions */
typedef 
INT64(__cdecl*T396FF76F)(Minus__UINT64,CARDINAL);
#else
typedef void (__cdecl*T396FF76F)(void);
#endif

#if 0 /* avoid type hash collions */
typedef 
INT64(__cdecl*TEC3990C4)(Minus__UINT64,Minus__UINT32);
#else
typedef void (__cdecl*TEC3990C4)(void);
#endif

#if 0 /* avoid type hash collions */
typedef 
INT64(__cdecl*TEEBA1E1B)(Minus__UINT64,Minus__UINT8);
#else
typedef void (__cdecl*TEEBA1E1B)(void);
#endif

#if 0 /* avoid type hash collions */
typedef 
double(__cdecl*T53CE8E0F)(Minus__FLOAT64,Minus__FLOAT64);
#else
typedef void (__cdecl*T53CE8E0F)(void);
#endif

#if 0 /* avoid type hash collions */
typedef 
INTEGER(__cdecl*TE335D818)(Minus__INT32,Minus__INT8);
#else
typedef void (__cdecl*TE335D818)(void);
#endif

#if 0 /* avoid type hash collions */
typedef 
TADC6066D_32(__cdecl*TF22C9F6A)(Minus__INT32,Minus__INT8);
#else
typedef void (__cdecl*TF22C9F6A)(void);
#endif

#if 0 /* avoid type hash collions */
typedef 
INT64(__cdecl*T28250189)(Minus__INT32,Minus__UINT64);
#else
typedef void (__cdecl*T28250189)(void);
#endif

#if 0 /* avoid type hash collions */
typedef 
INTEGER(__cdecl*TB0C64F49)(Minus__INT32,Minus__INT32);
#else
typedef void (__cdecl*TB0C64F49)(void);
#endif

#if 0 /* avoid type hash collions */
typedef 
TADC6066D_32(__cdecl*TA1DF083B)(Minus__INT32,Minus__INT32);
#else
typedef void (__cdecl*TA1DF083B)(void);
#endif

#if 0 /* avoid type hash collions */
typedef 
INT64(__cdecl*TF571A8D3)(Minus__INT32,LONGCARD);
#else
typedef void (__cdecl*TF571A8D3)(void);
#endif

#if 0 /* avoid type hash collions */
typedef 
INTEGER(__cdecl*T1DEF2115)(Minus__INT32,Minus__UINT16);
#else
typedef void (__cdecl*T1DEF2115)(void);
#endif

#if 0 /* avoid type hash collions */
typedef 
TADC6066D_32(__cdecl*TCF66667)(Minus__INT32,Minus__UINT16);
#else
typedef void (__cdecl*TCF66667)(void);
#endif

#if 0 /* avoid type hash collions */
typedef 
INTEGER(__cdecl*T5E42D02D)(Minus__INT32,INTEGER);
#else
typedef void (__cdecl*T5E42D02D)(void);
#endif

#if 0 /* avoid type hash collions */
typedef 
TADC6066D_32(__cdecl*T4F5B975F)(Minus__INT32,INTEGER);
#else
typedef void (__cdecl*T4F5B975F)(void);
#endif

#if 0 /* avoid type hash collions */
typedef 
INT64(__cdecl*TE7140B12)(Minus__INT32,Minus__INT64);
#else
typedef void (__cdecl*TE7140B12)(void);
#endif

#if 0 /* avoid type hash collions */
typedef 
INTEGER(__cdecl*T8F0756FB)(Minus__INT32,Minus__INT16);
#else
typedef void (__cdecl*T8F0756FB)(void);
#endif

#if 0 /* avoid type hash collions */
typedef 
TADC6066D_32(__cdecl*T9E1E1189)(Minus__INT32,Minus__INT16);
#else
typedef void (__cdecl*T9E1E1189)(void);
#endif

#if 0 /* avoid type hash collions */
typedef 
INTEGER(__cdecl*T223480F5)(Minus__INT32,CARDINAL);
#else
typedef void (__cdecl*T223480F5)(void);
#endif

#if 0 /* avoid type hash collions */
typedef 
TADC6066D_32(__cdecl*T332DC787)(Minus__INT32,CARDINAL);
#else
typedef void (__cdecl*T332DC787)(void);
#endif

#if 0 /* avoid type hash collions */
typedef 
INTEGER(__cdecl*TF762E75E)(Minus__INT32,Minus__UINT32);
#else
typedef void (__cdecl*TF762E75E)(void);
#endif

#if 0 /* avoid type hash collions */
typedef 
TADC6066D_32(__cdecl*TE67BA02C)(Minus__INT32,Minus__UINT32);
#else
typedef void (__cdecl*TE67BA02C)(void);
#endif

#if 0 /* avoid type hash collions */
typedef 
INTEGER(__cdecl*TF5E16981)(Minus__INT32,Minus__UINT8);
#else
typedef void (__cdecl*TF5E16981)(void);
#endif

#if 0 /* avoid type hash collions */
typedef 
TADC6066D_32(__cdecl*TE4F82EF3)(Minus__INT32,Minus__UINT8);
#else
typedef void (__cdecl*TE4F82EF3)(void);
#endif

#if 0 /* avoid type hash collions */
typedef 
INT64(__cdecl*T45041AE9)(LONGCARD,Minus__INT8);
#else
typedef void (__cdecl*T45041AE9)(void);
#endif

#if 0 /* avoid type hash collions */
typedef 
INT64(__cdecl*TDFA503B6)(LONGCARD,Minus__UINT64);
#else
typedef void (__cdecl*TDFA503B6)(void);
#endif

#if 0 /* avoid type hash collions */
typedef 
INT64(__cdecl*T16F78DB8)(LONGCARD,Minus__INT32);
#else
typedef void (__cdecl*T16F78DB8)(void);
#endif

#if 0 /* avoid type hash collions */
typedef 
INT64(__cdecl*T2F1AAEC)(LONGCARD,LONGCARD);
#else
typedef void (__cdecl*T2F1AAEC)(void);
#endif

#if 0 /* avoid type hash collions */
typedef 
INT64(__cdecl*TBBDEE3E4)(LONGCARD,Minus__UINT16);
#else
typedef void (__cdecl*TBBDEE3E4)(void);
#endif

#if 0 /* avoid type hash collions */
typedef 
INT64(__cdecl*TF87312DC)(LONGCARD,INTEGER);
#else
typedef void (__cdecl*TF87312DC)(void);
#endif

#if 0 /* avoid type hash collions */
typedef 
INT64(__cdecl*T1094092D)(LONGCARD,Minus__INT64);
#else
typedef void (__cdecl*T1094092D)(void);
#endif

#if 0 /* avoid type hash collions */
typedef 
INT64(__cdecl*T2936940A)(LONGCARD,Minus__INT16);
#else
typedef void (__cdecl*T2936940A)(void);
#endif

#if 0 /* avoid type hash collions */
typedef 
INT64(__cdecl*T84054204)(LONGCARD,CARDINAL);
#else
typedef void (__cdecl*T84054204)(void);
#endif

#if 0 /* avoid type hash collions */
typedef 
INT64(__cdecl*T515325AF)(LONGCARD,Minus__UINT32);
#else
typedef void (__cdecl*T515325AF)(void);
#endif

#if 0 /* avoid type hash collions */
typedef 
INT64(__cdecl*T53D0AB70)(LONGCARD,Minus__UINT8);
#else
typedef void (__cdecl*T53D0AB70)(void);
#endif

#if 0 /* avoid type hash collions */
typedef 
INTEGER(__cdecl*TC6D7EF7C)(Minus__UINT16,Minus__INT8);
#else
typedef void (__cdecl*TC6D7EF7C)(void);
#endif

#if 0 /* avoid type hash collions */
typedef 
TA4B285DE_16(__cdecl*TD54B1ABA)(Minus__UINT16,Minus__INT8);
#else
typedef void (__cdecl*TD54B1ABA)(void);
#endif

#if 0 /* avoid type hash collions */
typedef 
INT64(__cdecl*TDC736ED)(Minus__UINT16,Minus__UINT64);
#else
typedef void (__cdecl*TDC736ED)(void);
#endif

#if 0 /* avoid type hash collions */
typedef 
INTEGER(__cdecl*T9524782D)(Minus__UINT16,Minus__INT32);
#else
typedef void (__cdecl*T9524782D)(void);
#endif

#if 0 /* avoid type hash collions */
typedef 
TA4B285DE_16(__cdecl*T86B88DEB)(Minus__UINT16,Minus__INT32);
#else
typedef void (__cdecl*T86B88DEB)(void);
#endif

#if 0 /* avoid type hash collions */
typedef 
INT64(__cdecl*TD0939FB7)(Minus__UINT16,LONGCARD);
#else
typedef void (__cdecl*TD0939FB7)(void);
#endif

#if 0 /* avoid type hash collions */
typedef 
INTEGER(__cdecl*T380D1671)(Minus__UINT16,Minus__UINT16);
#else
typedef void (__cdecl*T380D1671)(void);
#endif

#if 0 /* avoid type hash collions */
typedef 
TA4B285DE_16(__cdecl*T2B91E3B7)(Minus__UINT16,Minus__UINT16);
#else
typedef void (__cdecl*T2B91E3B7)(void);
#endif

#if 0 /* avoid type hash collions */
typedef 
INTEGER(__cdecl*T7BA0E749)(Minus__UINT16,INTEGER);
#else
typedef void (__cdecl*T7BA0E749)(void);
#endif

#if 0 /* avoid type hash collions */
typedef 
TA4B285DE_16(__cdecl*T683C128F)(Minus__UINT16,INTEGER);
#else
typedef void (__cdecl*T683C128F)(void);
#endif

#if 0 /* avoid type hash collions */
typedef 
INT64(__cdecl*TC2F63C76)(Minus__UINT16,Minus__INT64);
#else
typedef void (__cdecl*TC2F63C76)(void);
#endif

#if 0 /* avoid type hash collions */
typedef 
INTEGER(__cdecl*TAAE5619F)(Minus__UINT16,Minus__INT16);
#else
typedef void (__cdecl*TAAE5619F)(void);
#endif

#if 0 /* avoid type hash collions */
typedef 
TA4B285DE_16(__cdecl*TB9799459)(Minus__UINT16,Minus__INT16);
#else
typedef void (__cdecl*TB9799459)(void);
#endif

#if 0 /* avoid type hash collions */
typedef 
INTEGER(__cdecl*T7D6B791)(Minus__UINT16,CARDINAL);
#else
typedef void (__cdecl*T7D6B791)(void);
#endif

#if 0 /* avoid type hash collions */
typedef 
TA4B285DE_16(__cdecl*T144A4257)(Minus__UINT16,CARDINAL);
#else
typedef void (__cdecl*T144A4257)(void);
#endif

#if 0 /* avoid type hash collions */
typedef 
INTEGER(__cdecl*TD280D03A)(Minus__UINT16,Minus__UINT32);
#else
typedef void (__cdecl*TD280D03A)(void);
#endif

#if 0 /* avoid type hash collions */
typedef 
TA4B285DE_16(__cdecl*TC11C25FC)(Minus__UINT16,Minus__UINT32);
#else
typedef void (__cdecl*TC11C25FC)(void);
#endif

#if 0 /* avoid type hash collions */
typedef 
INTEGER(__cdecl*TD0035EE5)(Minus__UINT16,Minus__UINT8);
#else
typedef void (__cdecl*TD0035EE5)(void);
#endif

#if 0 /* avoid type hash collions */
typedef 
TA4B285DE_16(__cdecl*TC39FAB23)(Minus__UINT16,Minus__UINT8);
#else
typedef void (__cdecl*TC39FAB23)(void);
#endif

#if 0 /* avoid type hash collions */
typedef 
INTEGER(__cdecl*T10235009)(INTEGER,Minus__INT8);
#else
typedef void (__cdecl*T10235009)(void);
#endif

#if 0 /* avoid type hash collions */
typedef 
INT64(__cdecl*TDB338998)(INTEGER,Minus__UINT64);
#else
typedef void (__cdecl*TDB338998)(void);
#endif

#if 0 /* avoid type hash collions */
typedef 
INTEGER(__cdecl*T43D0C758)(INTEGER,Minus__INT32);
#else
typedef void (__cdecl*T43D0C758)(void);
#endif

#if 0 /* avoid type hash collions */
typedef 
INT64(__cdecl*T66720C2)(INTEGER,LONGCARD);
#else
typedef void (__cdecl*T66720C2)(void);
#endif

#if 0 /* avoid type hash collions */
typedef 
INTEGER(__cdecl*TEEF9A904)(INTEGER,Minus__UINT16);
#else
typedef void (__cdecl*TEEF9A904)(void);
#endif

#if 0 /* avoid type hash collions */
typedef 
INTEGER(__cdecl*TAD54583C)(INTEGER,INTEGER);
#else
typedef void (__cdecl*TAD54583C)(void);
#endif

#if 0 /* avoid type hash collions */
typedef 
INT64(__cdecl*T14028303)(INTEGER,Minus__INT64);
#else
typedef void (__cdecl*T14028303)(void);
#endif

#if 0 /* avoid type hash collions */
typedef 
INTEGER(__cdecl*T7C11DEEA)(INTEGER,Minus__INT16);
#else
typedef void (__cdecl*T7C11DEEA)(void);
#endif

#if 0 /* avoid type hash collions */
typedef 
INTEGER(__cdecl*TD12208E4)(INTEGER,CARDINAL);
#else
typedef void (__cdecl*TD12208E4)(void);
#endif

#if 0 /* avoid type hash collions */
typedef 
INTEGER(__cdecl*T4746F4F)(INTEGER,Minus__UINT32);
#else
typedef void (__cdecl*T4746F4F)(void);
#endif

#if 0 /* avoid type hash collions */
typedef 
INTEGER(__cdecl*T6F7E190)(INTEGER,Minus__UINT8);
#else
typedef void (__cdecl*T6F7E190)(void);
#endif

#if 0 /* avoid type hash collions */
typedef 
INT64(__cdecl*T7FE1BFE8)(Minus__INT64,Minus__INT8);
#else
typedef void (__cdecl*T7FE1BFE8)(void);
#endif

#if 0 /* avoid type hash collions */
typedef 
INT64(__cdecl*TE540A6B7)(Minus__INT64,Minus__UINT64);
#else
typedef void (__cdecl*TE540A6B7)(void);
#endif

#if 0 /* avoid type hash collions */
typedef 
INT64(__cdecl*T2C1228B9)(Minus__INT64,Minus__INT32);
#else
typedef void (__cdecl*T2C1228B9)(void);
#endif

#if 0 /* avoid type hash collions */
typedef 
INT64(__cdecl*T38140FED)(Minus__INT64,LONGCARD);
#else
typedef void (__cdecl*T38140FED)(void);
#endif

#if 0 /* avoid type hash collions */
typedef 
INT64(__cdecl*T813B46E5)(Minus__INT64,Minus__UINT16);
#else
typedef void (__cdecl*T813B46E5)(void);
#endif

#if 0 /* avoid type hash collions */
typedef 
INT64(__cdecl*TC296B7DD)(Minus__INT64,INTEGER);
#else
typedef void (__cdecl*TC296B7DD)(void);
#endif

#if 0 /* avoid type hash collions */
typedef 
INT64(__cdecl*T2A71AC2C)(Minus__INT64,Minus__INT64);
#else
typedef void (__cdecl*T2A71AC2C)(void);
#endif

#if 0 /* avoid type hash collions */
typedef 
INT64(__cdecl*T13D3310B)(Minus__INT64,Minus__INT16);
#else
typedef void (__cdecl*T13D3310B)(void);
#endif

#if 0 /* avoid type hash collions */
typedef 
INT64(__cdecl*TBEE0E705)(Minus__INT64,CARDINAL);
#else
typedef void (__cdecl*TBEE0E705)(void);
#endif

#if 0 /* avoid type hash collions */
typedef 
INT64(__cdecl*T6BB680AE)(Minus__INT64,Minus__UINT32);
#else
typedef void (__cdecl*T6BB680AE)(void);
#endif

#if 0 /* avoid type hash collions */
typedef 
INT64(__cdecl*T69350E71)(Minus__INT64,Minus__UINT8);
#else
typedef void (__cdecl*T69350E71)(void);
#endif

#if 0 /* avoid type hash collions */
typedef 
float(__cdecl*T232EBA0E)(Minus__FLOAT32,Minus__FLOAT32);
#else
typedef void (__cdecl*T232EBA0E)(void);
#endif

#if 0 /* avoid type hash collions */
typedef 
INTEGER(__cdecl*T11D684CC)(Minus__INT16,Minus__INT8);
#else
typedef void (__cdecl*T11D684CC)(void);
#endif

#if 0 /* avoid type hash collions */
typedef 
T7300E1E8_16(__cdecl*TFCBA4D26)(Minus__INT16,Minus__INT8);
#else
typedef void (__cdecl*TFCBA4D26)(void);
#endif

#if 0 /* avoid type hash collions */
typedef 
INT64(__cdecl*TDAC65D5D)(Minus__INT16,Minus__UINT64);
#else
typedef void (__cdecl*TDAC65D5D)(void);
#endif

#if 0 /* avoid type hash collions */
typedef 
INTEGER(__cdecl*T4225139D)(Minus__INT16,Minus__INT32);
#else
typedef void (__cdecl*T4225139D)(void);
#endif

#if 0 /* avoid type hash collions */
typedef 
T7300E1E8_16(__cdecl*TAF49DA77)(Minus__INT16,Minus__INT32);
#else
typedef void (__cdecl*TAF49DA77)(void);
#endif

#if 0 /* avoid type hash collions */
typedef 
INT64(__cdecl*T792F407)(Minus__INT16,LONGCARD);
#else
typedef void (__cdecl*T792F407)(void);
#endif

#if 0 /* avoid type hash collions */
typedef 
INTEGER(__cdecl*TEF0C7DC1)(Minus__INT16,Minus__UINT16);
#else
typedef void (__cdecl*TEF0C7DC1)(void);
#endif

#if 0 /* avoid type hash collions */
typedef 
T7300E1E8_16(__cdecl*T260B42B)(Minus__INT16,Minus__UINT16);
#else
typedef void (__cdecl*T260B42B)(void);
#endif

#if 0 /* avoid type hash collions */
typedef 
INTEGER(__cdecl*TACA18CF9)(Minus__INT16,INTEGER);
#else
typedef void (__cdecl*TACA18CF9)(void);
#endif

#if 0 /* avoid type hash collions */
typedef 
T7300E1E8_16(__cdecl*T41CD4513)(Minus__INT16,INTEGER);
#else
typedef void (__cdecl*T41CD4513)(void);
#endif

#if 0 /* avoid type hash collions */
typedef 
INT64(__cdecl*T15F757C6)(Minus__INT16,Minus__INT64);
#else
typedef void (__cdecl*T15F757C6)(void);
#endif

#if 0 /* avoid type hash collions */
typedef 
INTEGER(__cdecl*T7DE40A2F)(Minus__INT16,Minus__INT16);
#else
typedef void (__cdecl*T7DE40A2F)(void);
#endif

#if 0 /* avoid type hash collions */
typedef 
T7300E1E8_16(__cdecl*T9088C3C5)(Minus__INT16,Minus__INT16);
#else
typedef void (__cdecl*T9088C3C5)(void);
#endif

#if 0 /* avoid type hash collions */
typedef 
INTEGER(__cdecl*TD0D7DC21)(Minus__INT16,CARDINAL);
#else
typedef void (__cdecl*TD0D7DC21)(void);
#endif

#if 0 /* avoid type hash collions */
typedef 
T7300E1E8_16(__cdecl*T3DBB15CB)(Minus__INT16,CARDINAL);
#else
typedef void (__cdecl*T3DBB15CB)(void);
#endif

#if 0 /* avoid type hash collions */
typedef 
INTEGER(__cdecl*T581BB8A)(Minus__INT16,Minus__UINT32);
#else
typedef void (__cdecl*T581BB8A)(void);
#endif

#if 0 /* avoid type hash collions */
typedef 
T7300E1E8_16(__cdecl*TE8ED7260)(Minus__INT16,Minus__UINT32);
#else
typedef void (__cdecl*TE8ED7260)(void);
#endif

#if 0 /* avoid type hash collions */
typedef 
INTEGER(__cdecl*T7023555)(Minus__INT16,Minus__UINT8);
#else
typedef void (__cdecl*T7023555)(void);
#endif

#if 0 /* avoid type hash collions */
typedef 
T7300E1E8_16(__cdecl*TEA6EFCBF)(Minus__INT16,Minus__UINT8);
#else
typedef void (__cdecl*TEA6EFCBF)(void);
#endif

#if 0 /* avoid type hash collions */
typedef 
INTEGER(__cdecl*T78F97685)(CARDINAL,Minus__INT8);
#else
typedef void (__cdecl*T78F97685)(void);
#endif

#if 0 /* avoid type hash collions */
typedef 
WORD_T(__cdecl*TCA2E1146)(CARDINAL,Minus__INT8);
#else
typedef void (__cdecl*TCA2E1146)(void);
#endif

#if 0 /* avoid type hash collions */
typedef 
INT64(__cdecl*TB3E9AF14)(CARDINAL,Minus__UINT64);
#else
typedef void (__cdecl*TB3E9AF14)(void);
#endif

#if 0 /* avoid type hash collions */
typedef 
INTEGER(__cdecl*T2B0AE1D4)(CARDINAL,Minus__INT32);
#else
typedef void (__cdecl*T2B0AE1D4)(void);
#endif

#if 0 /* avoid type hash collions */
typedef 
WORD_T(__cdecl*T99DD8617)(CARDINAL,Minus__INT32);
#else
typedef void (__cdecl*T99DD8617)(void);
#endif

#if 0 /* avoid type hash collions */
typedef 
INT64(__cdecl*T6EBD064E)(CARDINAL,LONGCARD);
#else
typedef void (__cdecl*T6EBD064E)(void);
#endif

#if 0 /* avoid type hash collions */
typedef 
INTEGER(__cdecl*T86238F88)(CARDINAL,Minus__UINT16);
#else
typedef void (__cdecl*T86238F88)(void);
#endif

#if 0 /* avoid type hash collions */
typedef 
WORD_T(__cdecl*T34F4E84B)(CARDINAL,Minus__UINT16);
#else
typedef void (__cdecl*T34F4E84B)(void);
#endif

#if 0 /* avoid type hash collions */
typedef 
INTEGER(__cdecl*TC58E7EB0)(CARDINAL,INTEGER);
#else
typedef void (__cdecl*TC58E7EB0)(void);
#endif

#if 0 /* avoid type hash collions */
typedef 
WORD_T(__cdecl*T77591973)(CARDINAL,INTEGER);
#else
typedef void (__cdecl*T77591973)(void);
#endif

#if 0 /* avoid type hash collions */
typedef 
INT64(__cdecl*T7CD8A58F)(CARDINAL,Minus__INT64);
#else
typedef void (__cdecl*T7CD8A58F)(void);
#endif

#if 0 /* avoid type hash collions */
typedef 
INTEGER(__cdecl*T14CBF866)(CARDINAL,Minus__INT16);
#else
typedef void (__cdecl*T14CBF866)(void);
#endif

#if 0 /* avoid type hash collions */
typedef 
WORD_T(__cdecl*TA61C9FA5)(CARDINAL,Minus__INT16);
#else
typedef void (__cdecl*TA61C9FA5)(void);
#endif

#if 0 /* avoid type hash collions */
typedef 
INTEGER(__cdecl*TB9F82E68)(CARDINAL,CARDINAL);
#else
typedef void (__cdecl*TB9F82E68)(void);
#endif

#if 0 /* avoid type hash collions */
typedef 
WORD_T(__cdecl*TB2F49AB)(CARDINAL,CARDINAL);
#else
typedef void (__cdecl*TB2F49AB)(void);
#endif

#if 0 /* avoid type hash collions */
typedef 
INTEGER(__cdecl*T6CAE49C3)(CARDINAL,Minus__UINT32);
#else
typedef void (__cdecl*T6CAE49C3)(void);
#endif

#if 0 /* avoid type hash collions */
typedef 
WORD_T(__cdecl*TDE792E00)(CARDINAL,Minus__UINT32);
#else
typedef void (__cdecl*TDE792E00)(void);
#endif

#if 0 /* avoid type hash collions */
typedef 
INTEGER(__cdecl*T6E2DC71C)(CARDINAL,Minus__UINT8);
#else
typedef void (__cdecl*T6E2DC71C)(void);
#endif

#if 0 /* avoid type hash collions */
typedef 
WORD_T(__cdecl*TDCFAA0DF)(CARDINAL,Minus__UINT8);
#else
typedef void (__cdecl*TDCFAA0DF)(void);
#endif

#if 0 /* avoid type hash collions */
typedef 
INTEGER(__cdecl*T4E883AB)(Minus__UINT32,Minus__INT8);
#else
typedef void (__cdecl*T4E883AB)(void);
#endif

#if 0 /* avoid type hash collions */
typedef 
T6FA2E87D_32(__cdecl*T5D763E94)(Minus__UINT32,Minus__INT8);
#else
typedef void (__cdecl*T5D763E94)(void);
#endif

#if 0 /* avoid type hash collions */
typedef 
INT64(__cdecl*TCFF85A3A)(Minus__UINT32,Minus__UINT64);
#else
typedef void (__cdecl*TCFF85A3A)(void);
#endif

#if 0 /* avoid type hash collions */
typedef 
INTEGER(__cdecl*T571B14FA)(Minus__UINT32,Minus__INT32);
#else
typedef void (__cdecl*T571B14FA)(void);
#endif

#if 0 /* avoid type hash collions */
typedef 
T6FA2E87D_32(__cdecl*TE85A9C5)(Minus__UINT32,Minus__INT32);
#else
typedef void (__cdecl*TE85A9C5)(void);
#endif

#if 0 /* avoid type hash collions */
typedef 
INT64(__cdecl*T12ACF360)(Minus__UINT32,LONGCARD);
#else
typedef void (__cdecl*T12ACF360)(void);
#endif

#if 0 /* avoid type hash collions */
typedef 
INTEGER(__cdecl*TFA327AA6)(Minus__UINT32,Minus__UINT16);
#else
typedef void (__cdecl*TFA327AA6)(void);
#endif

#if 0 /* avoid type hash collions */
typedef 
T6FA2E87D_32(__cdecl*TA3ACC799)(Minus__UINT32,Minus__UINT16);
#else
typedef void (__cdecl*TA3ACC799)(void);
#endif

#if 0 /* avoid type hash collions */
typedef 
INTEGER(__cdecl*TB99F8B9E)(Minus__UINT32,INTEGER);
#else
typedef void (__cdecl*TB99F8B9E)(void);
#endif

#if 0 /* avoid type hash collions */
typedef 
T6FA2E87D_32(__cdecl*TE00136A1)(Minus__UINT32,INTEGER);
#else
typedef void (__cdecl*TE00136A1)(void);
#endif

#if 0 /* avoid type hash collions */
typedef 
INT64(__cdecl*TC950A1)(Minus__UINT32,Minus__INT64);
#else
typedef void (__cdecl*TC950A1)(void);
#endif

#if 0 /* avoid type hash collions */
typedef 
INTEGER(__cdecl*T68DA0D48)(Minus__UINT32,Minus__INT16);
#else
typedef void (__cdecl*T68DA0D48)(void);
#endif

#if 0 /* avoid type hash collions */
typedef 
T6FA2E87D_32(__cdecl*T3144B077)(Minus__UINT32,Minus__INT16);
#else
typedef void (__cdecl*T3144B077)(void);
#endif

#if 0 /* avoid type hash collions */
typedef 
INTEGER(__cdecl*TC5E9DB46)(Minus__UINT32,CARDINAL);
#else
typedef void (__cdecl*TC5E9DB46)(void);
#endif

#if 0 /* avoid type hash collions */
typedef 
T6FA2E87D_32(__cdecl*T9C776679)(Minus__UINT32,CARDINAL);
#else
typedef void (__cdecl*T9C776679)(void);
#endif

#if 0 /* avoid type hash collions */
typedef 
INTEGER(__cdecl*T10BFBCED)(Minus__UINT32,Minus__UINT32);
#else
typedef void (__cdecl*T10BFBCED)(void);
#endif

#if 0 /* avoid type hash collions */
typedef 
T6FA2E87D_32(__cdecl*T492101D2)(Minus__UINT32,Minus__UINT32);
#else
typedef void (__cdecl*T492101D2)(void);
#endif

#if 0 /* avoid type hash collions */
typedef 
INTEGER(__cdecl*T123C3232)(Minus__UINT32,Minus__UINT8);
#else
typedef void (__cdecl*T123C3232)(void);
#endif

#if 0 /* avoid type hash collions */
typedef 
T6FA2E87D_32(__cdecl*T4BA28F0D)(Minus__UINT32,Minus__UINT8);
#else
typedef void (__cdecl*T4BA28F0D)(void);
#endif

#if 0 /* avoid type hash collions */
typedef 
INTEGER(__cdecl*TFE472EDF)(Minus__UINT8,Minus__INT8);
#else
typedef void (__cdecl*TFE472EDF)(void);
#endif

#if 0 /* avoid type hash collions */
typedef 
TB5B30AA_8(__cdecl*T2866B8B2)(Minus__UINT8,Minus__INT8);
#else
typedef void (__cdecl*T2866B8B2)(void);
#endif

#if 0 /* avoid type hash collions */
typedef 
INT64(__cdecl*T3557F74E)(Minus__UINT8,Minus__UINT64);
#else
typedef void (__cdecl*T3557F74E)(void);
#endif

#if 0 /* avoid type hash collions */
typedef 
INTEGER(__cdecl*TADB4B98E)(Minus__UINT8,Minus__INT32);
#else
typedef void (__cdecl*TADB4B98E)(void);
#endif

#if 0 /* avoid type hash collions */
typedef 
TB5B30AA_8(__cdecl*T7B952FE3)(Minus__UINT8,Minus__INT32);
#else
typedef void (__cdecl*T7B952FE3)(void);
#endif

#if 0 /* avoid type hash collions */
typedef 
INT64(__cdecl*TE8035E14)(Minus__UINT8,LONGCARD);
#else
typedef void (__cdecl*TE8035E14)(void);
#endif

#if 0 /* avoid type hash collions */
typedef 
INTEGER(__cdecl*T9DD7D2)(Minus__UINT8,Minus__UINT16);
#else
typedef void (__cdecl*T9DD7D2)(void);
#endif

#if 0 /* avoid type hash collions */
typedef 
TB5B30AA_8(__cdecl*TD6BC41BF)(Minus__UINT8,Minus__UINT16);
#else
typedef void (__cdecl*TD6BC41BF)(void);
#endif

#if 0 /* avoid type hash collions */
typedef 
INTEGER(__cdecl*T433026EA)(Minus__UINT8,INTEGER);
#else
typedef void (__cdecl*T433026EA)(void);
#endif

#if 0 /* avoid type hash collions */
typedef 
TB5B30AA_8(__cdecl*T9511B087)(Minus__UINT8,INTEGER);
#else
typedef void (__cdecl*T9511B087)(void);
#endif

#if 0 /* avoid type hash collions */
typedef 
INT64(__cdecl*TFA66FDD5)(Minus__UINT8,Minus__INT64);
#else
typedef void (__cdecl*TFA66FDD5)(void);
#endif

#if 0 /* avoid type hash collions */
typedef 
INTEGER(__cdecl*T9275A03C)(Minus__UINT8,Minus__INT16);
#else
typedef void (__cdecl*T9275A03C)(void);
#endif

#if 0 /* avoid type hash collions */
typedef 
TB5B30AA_8(__cdecl*T44543651)(Minus__UINT8,Minus__INT16);
#else
typedef void (__cdecl*T44543651)(void);
#endif

#if 0 /* avoid type hash collions */
typedef 
INTEGER(__cdecl*T3F467632)(Minus__UINT8,CARDINAL);
#else
typedef void (__cdecl*T3F467632)(void);
#endif

#if 0 /* avoid type hash collions */
typedef 
TB5B30AA_8(__cdecl*TE967E05F)(Minus__UINT8,CARDINAL);
#else
typedef void (__cdecl*TE967E05F)(void);
#endif

#if 0 /* avoid type hash collions */
typedef 
INTEGER(__cdecl*TEA101199)(Minus__UINT8,Minus__UINT32);
#else
typedef void (__cdecl*TEA101199)(void);
#endif

#if 0 /* avoid type hash collions */
typedef 
TB5B30AA_8(__cdecl*T3C3187F4)(Minus__UINT8,Minus__UINT32);
#else
typedef void (__cdecl*T3C3187F4)(void);
#endif

#if 0 /* avoid type hash collions */
typedef 
INTEGER(__cdecl*TE8939F46)(Minus__UINT8,Minus__UINT8);
#else
typedef void (__cdecl*TE8939F46)(void);
#endif

#if 0 /* avoid type hash collions */
typedef 
TB5B30AA_8(__cdecl*T3EB2092B)(Minus__UINT8,Minus__UINT8);
#else
typedef void (__cdecl*T3EB2092B)(void);
#endif

#if 0 /* avoid type hash collions */
typedef 
void(__cdecl*TA4BB9882)(ADDRESS,INTEGER);
#else
typedef void (__cdecl*TA4BB9882)(void);
#endif

#if 0 /* avoid type hash collions */
typedef 
INT64(__cdecl*T1B69F2C8)(LONGINT,LONGINT);
#else
typedef void (__cdecl*T1B69F2C8)(void);
#endif

#if 0 /* avoid type hash collions */
typedef 
INTEGER(__cdecl*TD4F21C29)(INTEGER,INTEGER);
#else
typedef void (__cdecl*TD4F21C29)(void);
#endif
 /* DeclareTypes_FlushOnce size:0 */
 /* end: DeclareTypes */
 /* begin: helper functions */

#if __GNUC__ > 2 || __GNUC__ == 2 && __GNUC_MINOR__ >= 5
#define M3_ATTRIBUTE_NO_RETURN __attribute__((__noreturn__))
#else
#define M3_ATTRIBUTE_NO_RETURN
#endif
 /* end: helper functions */
 /* begin: imports */
 /* import_procedure */

#ifndef RT0__ModulePtr
#define RT0__ModulePtr RT0__ModulePtr
typedef ADDRESS /*TypeText3*/  RT0__ModulePtr;
#endif
/*Proc_ForwardDeclareFrameType*/struct Minus_I3_Frame_t;typedef struct Minus_I3_Frame_t Minus_I3_Frame_t;
 /* internal_declare_param */
RT0__ModulePtr
__cdecl
Minus_I3(
   /* Param_Type1 */ INTEGER /*TypeText1*/  mode_L_0);
 /* import_procedure */
/*Proc_ForwardDeclareFrameType*/struct Long_I3_Frame_t;typedef struct Long_I3_Frame_t Long_I3_Frame_t;
 /* internal_declare_param */
RT0__ModulePtr
__cdecl
Long_I3(
   /* Param_Type1 */ INTEGER /*TypeText1*/  mode_L_1);
 /* import_procedure */
/*Proc_ForwardDeclareFrameType*/struct Word_I3_Frame_t;typedef struct Word_I3_Frame_t Word_I3_Frame_t;
 /* internal_declare_param */
RT0__ModulePtr
__cdecl
Word_I3(
   /* Param_Type1 */ INTEGER /*TypeText1*/  mode_L_2);
 /* import_procedure */
/*Proc_ForwardDeclareFrameType*/struct Cstdint_I3_Frame_t;typedef struct Cstdint_I3_Frame_t Cstdint_I3_Frame_t;
 /* internal_declare_param */
RT0__ModulePtr
__cdecl
Cstdint_I3(
   /* Param_Type1 */ INTEGER /*TypeText1*/  mode_L_3);
 /* import_procedure */
/*Proc_ForwardDeclareFrameType*/struct RTHooks_I3_Frame_t;typedef struct RTHooks_I3_Frame_t RTHooks_I3_Frame_t;
 /* internal_declare_param */
RT0__ModulePtr
__cdecl
RTHooks_I3(
   /* Param_Type1 */ INTEGER /*TypeText1*/  mode_L_4);
 /* import_procedure */
/*Proc_ForwardDeclareFrameType*/struct RTHooks__ReportFault_Frame_t;typedef struct RTHooks__ReportFault_Frame_t RTHooks__ReportFault_Frame_t;
 /* internal_declare_param */
 /* internal_declare_param */
void /*TypeText3*/ 
__cdecl
RTHooks__ReportFault(
   /* Param_Type1 */ ADDRESS module_L_5,
   /* Param_Type1 */ INTEGER info_L_6) M3_ATTRIBUTE_NO_RETURN;
 /* import_procedure */
/*Proc_ForwardDeclareFrameType*/struct Long__Minus_Frame_t;typedef struct Long__Minus_Frame_t Long__Minus_Frame_t;
 /* internal_declare_param */
 /* internal_declare_param */
LONGINT
__cdecl
Long__Minus(
   /* Param_Type1 */ LONGINT x_L_7,
   /* Param_Type1 */ LONGINT y_L_8);
 /* import_procedure */
/*Proc_ForwardDeclareFrameType*/struct Word__Minus_Frame_t;typedef struct Word__Minus_Frame_t Word__Minus_Frame_t;
 /* internal_declare_param */
 /* internal_declare_param */
INTEGER
__cdecl
Word__Minus(
   /* Param_Type1 */ INTEGER x_L_9,
   /* Param_Type1 */ INTEGER y_L_10);
 /* end: imports */
 /* begin: locals */
 /* declare_segment name:<NIL> typeid:TFFFFFFFF const:TRUE */
/*declare_segment*/struct Minus_m_11_L_12_t;
/*declare_segment*/typedef struct Minus_m_11_L_12_t Minus_m_11_L_12_t;
 /* declare_segment name:M_Minus typeid:TFFFFFFFF const:FALSE */
 /* handler_name_prefixes:Minus_M3_LINE_ */
 /* handler_name_prefixes:Minus_I3_LINE_ */
/*declare_segment*/struct Minus_m_M_Minus_L_13_t;
/*declare_segment*/typedef struct Minus_m_M_Minus_L_13_t Minus_m_M_Minus_L_13_t;
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct Minus_M3_Frame_t;typedef struct Minus_M3_Frame_t Minus_M3_Frame_t;
 /* internal_declare_param */
RT0__ModulePtr
__cdecl
Minus_M3(
   /* Param_Type1 */ INTEGER /*TypeText1*/  mode_L_14);
 /* declare_procedure */

#ifndef Word__T
#define Word__T Word__T
typedef INTEGER /*TypeText1*/  Word__T;
#endif
/*Proc_ForwardDeclareFrameType*/struct Minus__uMinus_var_i8_i8_Frame_t;typedef struct Minus__uMinus_var_i8_i8_Frame_t Minus__uMinus_var_i8_i8_Frame_t;
Word__T
__cdecl
Minus__uMinus_var_i8_i8(void);
 /* declare_local */
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct Minus__Minus_var_i8_i8_Frame_t;typedef struct Minus__Minus_var_i8_i8_Frame_t Minus__Minus_var_i8_i8_Frame_t;
Minus__INT8
__cdecl
Minus__Minus_var_i8_i8(void);
 /* declare_local */
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct Minus__uMinus_param_i8_i8_Frame_t;typedef struct Minus__uMinus_param_i8_i8_Frame_t Minus__uMinus_param_i8_i8_Frame_t;
 /* declare_local */
 /* internal_declare_param */
 /* internal_declare_param */
Word__T
__cdecl
Minus__uMinus_param_i8_i8(
   /* Param_Type1 */ Minus__INT8 a_L_18,
   /* Param_Type1 */ Minus__INT8 b_L_19);
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct Minus__Minus_param_i8_i8_Frame_t;typedef struct Minus__Minus_param_i8_i8_Frame_t Minus__Minus_param_i8_i8_Frame_t;
 /* declare_local */
 /* internal_declare_param */
 /* internal_declare_param */
Minus__INT8
__cdecl
Minus__Minus_param_i8_i8(
   /* Param_Type1 */ Minus__INT8 a_L_21,
   /* Param_Type1 */ Minus__INT8 b_L_22);
 /* declare_procedure */

#ifndef Long__T
#define Long__T Long__T
typedef INT64 /*TypeText1*/  Long__T;
#endif
/*Proc_ForwardDeclareFrameType*/struct Minus__uMinus_var_i8_u64_Frame_t;typedef struct Minus__uMinus_var_i8_u64_Frame_t Minus__uMinus_var_i8_u64_Frame_t;
Long__T
__cdecl
Minus__uMinus_var_i8_u64(void);
 /* declare_local */
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct Minus__Minus_var_i8_u64_Frame_t;typedef struct Minus__Minus_var_i8_u64_Frame_t Minus__Minus_var_i8_u64_Frame_t;
LONGINT
__cdecl
Minus__Minus_var_i8_u64(void);
 /* declare_local */
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct Minus__uMinus_param_i8_u64_Frame_t;typedef struct Minus__uMinus_param_i8_u64_Frame_t Minus__uMinus_param_i8_u64_Frame_t;
 /* declare_local */
 /* internal_declare_param */
 /* internal_declare_param */
Long__T
__cdecl
Minus__uMinus_param_i8_u64(
   /* Param_Type1 */ Minus__INT8 a_L_26,
   /* Param_Type1 */ Minus__UINT64 b_L_27);
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct Minus__Minus_param_i8_u64_Frame_t;typedef struct Minus__Minus_param_i8_u64_Frame_t Minus__Minus_param_i8_u64_Frame_t;
 /* declare_local */
 /* internal_declare_param */
 /* internal_declare_param */
LONGINT
__cdecl
Minus__Minus_param_i8_u64(
   /* Param_Type1 */ Minus__INT8 a_L_29,
   /* Param_Type1 */ Minus__UINT64 b_L_30);
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct Minus__uMinus_var_i8_i32_Frame_t;typedef struct Minus__uMinus_var_i8_i32_Frame_t Minus__uMinus_var_i8_i32_Frame_t;
Word__T
__cdecl
Minus__uMinus_var_i8_i32(void);
 /* declare_local */
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct Minus__Minus_var_i8_i32_Frame_t;typedef struct Minus__Minus_var_i8_i32_Frame_t Minus__Minus_var_i8_i32_Frame_t;
Minus__INT8
__cdecl
Minus__Minus_var_i8_i32(void);
 /* declare_local */
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct Minus__uMinus_param_i8_i32_Frame_t;typedef struct Minus__uMinus_param_i8_i32_Frame_t Minus__uMinus_param_i8_i32_Frame_t;
 /* declare_local */
 /* internal_declare_param */
 /* internal_declare_param */
Word__T
__cdecl
Minus__uMinus_param_i8_i32(
   /* Param_Type1 */ Minus__INT8 a_L_34,
   /* Param_Type1 */ Minus__INT32 b_L_35);
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct Minus__Minus_param_i8_i32_Frame_t;typedef struct Minus__Minus_param_i8_i32_Frame_t Minus__Minus_param_i8_i32_Frame_t;
 /* declare_local */
 /* internal_declare_param */
 /* internal_declare_param */
Minus__INT8
__cdecl
Minus__Minus_param_i8_i32(
   /* Param_Type1 */ Minus__INT8 a_L_37,
   /* Param_Type1 */ Minus__INT32 b_L_38);
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct Minus__uMinus_var_i8_LC_Frame_t;typedef struct Minus__uMinus_var_i8_LC_Frame_t Minus__uMinus_var_i8_LC_Frame_t;
Long__T
__cdecl
Minus__uMinus_var_i8_LC(void);
 /* declare_local */
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct Minus__Minus_var_i8_LC_Frame_t;typedef struct Minus__Minus_var_i8_LC_Frame_t Minus__Minus_var_i8_LC_Frame_t;
LONGINT
__cdecl
Minus__Minus_var_i8_LC(void);
 /* declare_local */
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct Minus__uMinus_param_i8_LC_Frame_t;typedef struct Minus__uMinus_param_i8_LC_Frame_t Minus__uMinus_param_i8_LC_Frame_t;
 /* declare_local */
 /* internal_declare_param */
 /* internal_declare_param */
Long__T
__cdecl
Minus__uMinus_param_i8_LC(
   /* Param_Type1 */ Minus__INT8 a_L_42,
   /* Param_Type1 */ LONGCARD b_L_43);
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct Minus__Minus_param_i8_LC_Frame_t;typedef struct Minus__Minus_param_i8_LC_Frame_t Minus__Minus_param_i8_LC_Frame_t;
 /* declare_local */
 /* internal_declare_param */
 /* internal_declare_param */
LONGINT
__cdecl
Minus__Minus_param_i8_LC(
   /* Param_Type1 */ Minus__INT8 a_L_45,
   /* Param_Type1 */ LONGCARD b_L_46);
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct Minus__uMinus_var_i8_u16_Frame_t;typedef struct Minus__uMinus_var_i8_u16_Frame_t Minus__uMinus_var_i8_u16_Frame_t;
Word__T
__cdecl
Minus__uMinus_var_i8_u16(void);
 /* declare_local */
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct Minus__Minus_var_i8_u16_Frame_t;typedef struct Minus__Minus_var_i8_u16_Frame_t Minus__Minus_var_i8_u16_Frame_t;
Minus__INT8
__cdecl
Minus__Minus_var_i8_u16(void);
 /* declare_local */
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct Minus__uMinus_param_i8_u16_Frame_t;typedef struct Minus__uMinus_param_i8_u16_Frame_t Minus__uMinus_param_i8_u16_Frame_t;
 /* declare_local */
 /* internal_declare_param */
 /* internal_declare_param */
Word__T
__cdecl
Minus__uMinus_param_i8_u16(
   /* Param_Type1 */ Minus__INT8 a_L_50,
   /* Param_Type1 */ Minus__UINT16 b_L_51);
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct Minus__Minus_param_i8_u16_Frame_t;typedef struct Minus__Minus_param_i8_u16_Frame_t Minus__Minus_param_i8_u16_Frame_t;
 /* declare_local */
 /* internal_declare_param */
 /* internal_declare_param */
Minus__INT8
__cdecl
Minus__Minus_param_i8_u16(
   /* Param_Type1 */ Minus__INT8 a_L_53,
   /* Param_Type1 */ Minus__UINT16 b_L_54);
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct Minus__uMinus_var_i8_I_Frame_t;typedef struct Minus__uMinus_var_i8_I_Frame_t Minus__uMinus_var_i8_I_Frame_t;
Word__T
__cdecl
Minus__uMinus_var_i8_I(void);
 /* declare_local */
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct Minus__Minus_var_i8_I_Frame_t;typedef struct Minus__Minus_var_i8_I_Frame_t Minus__Minus_var_i8_I_Frame_t;
Minus__INT8
__cdecl
Minus__Minus_var_i8_I(void);
 /* declare_local */
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct Minus__uMinus_param_i8_I_Frame_t;typedef struct Minus__uMinus_param_i8_I_Frame_t Minus__uMinus_param_i8_I_Frame_t;
 /* declare_local */
 /* internal_declare_param */
 /* internal_declare_param */
Word__T
__cdecl
Minus__uMinus_param_i8_I(
   /* Param_Type1 */ Minus__INT8 a_L_58,
   /* Param_Type1 */ INTEGER b_L_59);
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct Minus__Minus_param_i8_I_Frame_t;typedef struct Minus__Minus_param_i8_I_Frame_t Minus__Minus_param_i8_I_Frame_t;
 /* declare_local */
 /* internal_declare_param */
 /* internal_declare_param */
Minus__INT8
__cdecl
Minus__Minus_param_i8_I(
   /* Param_Type1 */ Minus__INT8 a_L_61,
   /* Param_Type1 */ INTEGER b_L_62);
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct Minus__uMinus_var_i8_i64_Frame_t;typedef struct Minus__uMinus_var_i8_i64_Frame_t Minus__uMinus_var_i8_i64_Frame_t;
Long__T
__cdecl
Minus__uMinus_var_i8_i64(void);
 /* declare_local */
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct Minus__Minus_var_i8_i64_Frame_t;typedef struct Minus__Minus_var_i8_i64_Frame_t Minus__Minus_var_i8_i64_Frame_t;
LONGINT
__cdecl
Minus__Minus_var_i8_i64(void);
 /* declare_local */
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct Minus__uMinus_param_i8_i64_Frame_t;typedef struct Minus__uMinus_param_i8_i64_Frame_t Minus__uMinus_param_i8_i64_Frame_t;
 /* declare_local */
 /* internal_declare_param */
 /* internal_declare_param */
Long__T
__cdecl
Minus__uMinus_param_i8_i64(
   /* Param_Type1 */ Minus__INT8 a_L_66,
   /* Param_Type1 */ Minus__INT64 b_L_67);
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct Minus__Minus_param_i8_i64_Frame_t;typedef struct Minus__Minus_param_i8_i64_Frame_t Minus__Minus_param_i8_i64_Frame_t;
 /* declare_local */
 /* internal_declare_param */
 /* internal_declare_param */
LONGINT
__cdecl
Minus__Minus_param_i8_i64(
   /* Param_Type1 */ Minus__INT8 a_L_69,
   /* Param_Type1 */ Minus__INT64 b_L_70);
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct Minus__uMinus_var_i8_i16_Frame_t;typedef struct Minus__uMinus_var_i8_i16_Frame_t Minus__uMinus_var_i8_i16_Frame_t;
Word__T
__cdecl
Minus__uMinus_var_i8_i16(void);
 /* declare_local */
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct Minus__Minus_var_i8_i16_Frame_t;typedef struct Minus__Minus_var_i8_i16_Frame_t Minus__Minus_var_i8_i16_Frame_t;
Minus__INT8
__cdecl
Minus__Minus_var_i8_i16(void);
 /* declare_local */
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct Minus__uMinus_param_i8_i16_Frame_t;typedef struct Minus__uMinus_param_i8_i16_Frame_t Minus__uMinus_param_i8_i16_Frame_t;
 /* declare_local */
 /* internal_declare_param */
 /* internal_declare_param */
Word__T
__cdecl
Minus__uMinus_param_i8_i16(
   /* Param_Type1 */ Minus__INT8 a_L_74,
   /* Param_Type1 */ Minus__INT16 b_L_75);
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct Minus__Minus_param_i8_i16_Frame_t;typedef struct Minus__Minus_param_i8_i16_Frame_t Minus__Minus_param_i8_i16_Frame_t;
 /* declare_local */
 /* internal_declare_param */
 /* internal_declare_param */
Minus__INT8
__cdecl
Minus__Minus_param_i8_i16(
   /* Param_Type1 */ Minus__INT8 a_L_77,
   /* Param_Type1 */ Minus__INT16 b_L_78);
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct Minus__uMinus_var_i8_C_Frame_t;typedef struct Minus__uMinus_var_i8_C_Frame_t Minus__uMinus_var_i8_C_Frame_t;
Word__T
__cdecl
Minus__uMinus_var_i8_C(void);
 /* declare_local */
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct Minus__Minus_var_i8_C_Frame_t;typedef struct Minus__Minus_var_i8_C_Frame_t Minus__Minus_var_i8_C_Frame_t;
Minus__INT8
__cdecl
Minus__Minus_var_i8_C(void);
 /* declare_local */
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct Minus__uMinus_param_i8_C_Frame_t;typedef struct Minus__uMinus_param_i8_C_Frame_t Minus__uMinus_param_i8_C_Frame_t;
 /* declare_local */
 /* internal_declare_param */
 /* internal_declare_param */
Word__T
__cdecl
Minus__uMinus_param_i8_C(
   /* Param_Type1 */ Minus__INT8 a_L_82,
   /* Param_Type1 */ CARDINAL b_L_83);
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct Minus__Minus_param_i8_C_Frame_t;typedef struct Minus__Minus_param_i8_C_Frame_t Minus__Minus_param_i8_C_Frame_t;
 /* declare_local */
 /* internal_declare_param */
 /* internal_declare_param */
Minus__INT8
__cdecl
Minus__Minus_param_i8_C(
   /* Param_Type1 */ Minus__INT8 a_L_85,
   /* Param_Type1 */ CARDINAL b_L_86);
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct Minus__uMinus_var_i8_u32_Frame_t;typedef struct Minus__uMinus_var_i8_u32_Frame_t Minus__uMinus_var_i8_u32_Frame_t;
Word__T
__cdecl
Minus__uMinus_var_i8_u32(void);
 /* declare_local */
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct Minus__Minus_var_i8_u32_Frame_t;typedef struct Minus__Minus_var_i8_u32_Frame_t Minus__Minus_var_i8_u32_Frame_t;
Minus__INT8
__cdecl
Minus__Minus_var_i8_u32(void);
 /* declare_local */
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct Minus__uMinus_param_i8_u32_Frame_t;typedef struct Minus__uMinus_param_i8_u32_Frame_t Minus__uMinus_param_i8_u32_Frame_t;
 /* declare_local */
 /* internal_declare_param */
 /* internal_declare_param */
Word__T
__cdecl
Minus__uMinus_param_i8_u32(
   /* Param_Type1 */ Minus__INT8 a_L_90,
   /* Param_Type1 */ Minus__UINT32 b_L_91);
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct Minus__Minus_param_i8_u32_Frame_t;typedef struct Minus__Minus_param_i8_u32_Frame_t Minus__Minus_param_i8_u32_Frame_t;
 /* declare_local */
 /* internal_declare_param */
 /* internal_declare_param */
Minus__INT8
__cdecl
Minus__Minus_param_i8_u32(
   /* Param_Type1 */ Minus__INT8 a_L_93,
   /* Param_Type1 */ Minus__UINT32 b_L_94);
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct Minus__uMinus_var_i8_u8_Frame_t;typedef struct Minus__uMinus_var_i8_u8_Frame_t Minus__uMinus_var_i8_u8_Frame_t;
Word__T
__cdecl
Minus__uMinus_var_i8_u8(void);
 /* declare_local */
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct Minus__Minus_var_i8_u8_Frame_t;typedef struct Minus__Minus_var_i8_u8_Frame_t Minus__Minus_var_i8_u8_Frame_t;
Minus__INT8
__cdecl
Minus__Minus_var_i8_u8(void);
 /* declare_local */
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct Minus__uMinus_param_i8_u8_Frame_t;typedef struct Minus__uMinus_param_i8_u8_Frame_t Minus__uMinus_param_i8_u8_Frame_t;
 /* declare_local */
 /* internal_declare_param */
 /* internal_declare_param */
Word__T
__cdecl
Minus__uMinus_param_i8_u8(
   /* Param_Type1 */ Minus__INT8 a_L_98,
   /* Param_Type1 */ Minus__UINT8 b_L_99);
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct Minus__Minus_param_i8_u8_Frame_t;typedef struct Minus__Minus_param_i8_u8_Frame_t Minus__Minus_param_i8_u8_Frame_t;
 /* declare_local */
 /* internal_declare_param */
 /* internal_declare_param */
Minus__INT8
__cdecl
Minus__Minus_param_i8_u8(
   /* Param_Type1 */ Minus__INT8 a_L_101,
   /* Param_Type1 */ Minus__UINT8 b_L_102);
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct Minus__uMinus_var_i8_L_Frame_t;typedef struct Minus__uMinus_var_i8_L_Frame_t Minus__uMinus_var_i8_L_Frame_t;
Long__T
__cdecl
Minus__uMinus_var_i8_L(void);
 /* declare_local */
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct Minus__Minus_var_i8_L_Frame_t;typedef struct Minus__Minus_var_i8_L_Frame_t Minus__Minus_var_i8_L_Frame_t;
LONGINT
__cdecl
Minus__Minus_var_i8_L(void);
 /* declare_local */
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct Minus__uMinus_param_i8_L_Frame_t;typedef struct Minus__uMinus_param_i8_L_Frame_t Minus__uMinus_param_i8_L_Frame_t;
 /* declare_local */
 /* internal_declare_param */
 /* internal_declare_param */
Long__T
__cdecl
Minus__uMinus_param_i8_L(
   /* Param_Type1 */ Minus__INT8 a_L_106,
   /* Param_Type1 */ LONGINT b_L_107);
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct Minus__Minus_param_i8_L_Frame_t;typedef struct Minus__Minus_param_i8_L_Frame_t Minus__Minus_param_i8_L_Frame_t;
 /* declare_local */
 /* internal_declare_param */
 /* internal_declare_param */
LONGINT
__cdecl
Minus__Minus_param_i8_L(
   /* Param_Type1 */ Minus__INT8 a_L_109,
   /* Param_Type1 */ LONGINT b_L_110);
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct Minus__uMinus_var_u64_i8_Frame_t;typedef struct Minus__uMinus_var_u64_i8_Frame_t Minus__uMinus_var_u64_i8_Frame_t;
Long__T
__cdecl
Minus__uMinus_var_u64_i8(void);
 /* declare_local */
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct Minus__Minus_var_u64_i8_Frame_t;typedef struct Minus__Minus_var_u64_i8_Frame_t Minus__Minus_var_u64_i8_Frame_t;
LONGINT
__cdecl
Minus__Minus_var_u64_i8(void);
 /* declare_local */
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct Minus__uMinus_param_u64_i8_Frame_t;typedef struct Minus__uMinus_param_u64_i8_Frame_t Minus__uMinus_param_u64_i8_Frame_t;
 /* declare_local */
 /* internal_declare_param */
 /* internal_declare_param */
Long__T
__cdecl
Minus__uMinus_param_u64_i8(
   /* Param_Type1 */ Minus__UINT64 a_L_114,
   /* Param_Type1 */ Minus__INT8 b_L_115);
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct Minus__Minus_param_u64_i8_Frame_t;typedef struct Minus__Minus_param_u64_i8_Frame_t Minus__Minus_param_u64_i8_Frame_t;
 /* declare_local */
 /* internal_declare_param */
 /* internal_declare_param */
LONGINT
__cdecl
Minus__Minus_param_u64_i8(
   /* Param_Type1 */ Minus__UINT64 a_L_117,
   /* Param_Type1 */ Minus__INT8 b_L_118);
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct Minus__uMinus_var_u64_u64_Frame_t;typedef struct Minus__uMinus_var_u64_u64_Frame_t Minus__uMinus_var_u64_u64_Frame_t;
Long__T
__cdecl
Minus__uMinus_var_u64_u64(void);
 /* declare_local */
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct Minus__Minus_var_u64_u64_Frame_t;typedef struct Minus__Minus_var_u64_u64_Frame_t Minus__Minus_var_u64_u64_Frame_t;
LONGINT
__cdecl
Minus__Minus_var_u64_u64(void);
 /* declare_local */
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct Minus__uMinus_param_u64_u64_Frame_t;typedef struct Minus__uMinus_param_u64_u64_Frame_t Minus__uMinus_param_u64_u64_Frame_t;
 /* declare_local */
 /* internal_declare_param */
 /* internal_declare_param */
Long__T
__cdecl
Minus__uMinus_param_u64_u64(
   /* Param_Type1 */ Minus__UINT64 a_L_122,
   /* Param_Type1 */ Minus__UINT64 b_L_123);
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct Minus__Minus_param_u64_u64_Frame_t;typedef struct Minus__Minus_param_u64_u64_Frame_t Minus__Minus_param_u64_u64_Frame_t;
 /* declare_local */
 /* internal_declare_param */
 /* internal_declare_param */
LONGINT
__cdecl
Minus__Minus_param_u64_u64(
   /* Param_Type1 */ Minus__UINT64 a_L_125,
   /* Param_Type1 */ Minus__UINT64 b_L_126);
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct Minus__uMinus_var_u64_i32_Frame_t;typedef struct Minus__uMinus_var_u64_i32_Frame_t Minus__uMinus_var_u64_i32_Frame_t;
Long__T
__cdecl
Minus__uMinus_var_u64_i32(void);
 /* declare_local */
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct Minus__Minus_var_u64_i32_Frame_t;typedef struct Minus__Minus_var_u64_i32_Frame_t Minus__Minus_var_u64_i32_Frame_t;
LONGINT
__cdecl
Minus__Minus_var_u64_i32(void);
 /* declare_local */
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct Minus__uMinus_param_u64_i32_Frame_t;typedef struct Minus__uMinus_param_u64_i32_Frame_t Minus__uMinus_param_u64_i32_Frame_t;
 /* declare_local */
 /* internal_declare_param */
 /* internal_declare_param */
Long__T
__cdecl
Minus__uMinus_param_u64_i32(
   /* Param_Type1 */ Minus__UINT64 a_L_130,
   /* Param_Type1 */ Minus__INT32 b_L_131);
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct Minus__Minus_param_u64_i32_Frame_t;typedef struct Minus__Minus_param_u64_i32_Frame_t Minus__Minus_param_u64_i32_Frame_t;
 /* declare_local */
 /* internal_declare_param */
 /* internal_declare_param */
LONGINT
__cdecl
Minus__Minus_param_u64_i32(
   /* Param_Type1 */ Minus__UINT64 a_L_133,
   /* Param_Type1 */ Minus__INT32 b_L_134);
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct Minus__uMinus_var_u64_LC_Frame_t;typedef struct Minus__uMinus_var_u64_LC_Frame_t Minus__uMinus_var_u64_LC_Frame_t;
Long__T
__cdecl
Minus__uMinus_var_u64_LC(void);
 /* declare_local */
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct Minus__Minus_var_u64_LC_Frame_t;typedef struct Minus__Minus_var_u64_LC_Frame_t Minus__Minus_var_u64_LC_Frame_t;
LONGINT
__cdecl
Minus__Minus_var_u64_LC(void);
 /* declare_local */
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct Minus__uMinus_param_u64_LC_Frame_t;typedef struct Minus__uMinus_param_u64_LC_Frame_t Minus__uMinus_param_u64_LC_Frame_t;
 /* declare_local */
 /* internal_declare_param */
 /* internal_declare_param */
Long__T
__cdecl
Minus__uMinus_param_u64_LC(
   /* Param_Type1 */ Minus__UINT64 a_L_138,
   /* Param_Type1 */ LONGCARD b_L_139);
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct Minus__Minus_param_u64_LC_Frame_t;typedef struct Minus__Minus_param_u64_LC_Frame_t Minus__Minus_param_u64_LC_Frame_t;
 /* declare_local */
 /* internal_declare_param */
 /* internal_declare_param */
LONGINT
__cdecl
Minus__Minus_param_u64_LC(
   /* Param_Type1 */ Minus__UINT64 a_L_141,
   /* Param_Type1 */ LONGCARD b_L_142);
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct Minus__uMinus_var_u64_u16_Frame_t;typedef struct Minus__uMinus_var_u64_u16_Frame_t Minus__uMinus_var_u64_u16_Frame_t;
Long__T
__cdecl
Minus__uMinus_var_u64_u16(void);
 /* declare_local */
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct Minus__Minus_var_u64_u16_Frame_t;typedef struct Minus__Minus_var_u64_u16_Frame_t Minus__Minus_var_u64_u16_Frame_t;
LONGINT
__cdecl
Minus__Minus_var_u64_u16(void);
 /* declare_local */
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct Minus__uMinus_param_u64_u16_Frame_t;typedef struct Minus__uMinus_param_u64_u16_Frame_t Minus__uMinus_param_u64_u16_Frame_t;
 /* declare_local */
 /* internal_declare_param */
 /* internal_declare_param */
Long__T
__cdecl
Minus__uMinus_param_u64_u16(
   /* Param_Type1 */ Minus__UINT64 a_L_146,
   /* Param_Type1 */ Minus__UINT16 b_L_147);
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct Minus__Minus_param_u64_u16_Frame_t;typedef struct Minus__Minus_param_u64_u16_Frame_t Minus__Minus_param_u64_u16_Frame_t;
 /* declare_local */
 /* internal_declare_param */
 /* internal_declare_param */
LONGINT
__cdecl
Minus__Minus_param_u64_u16(
   /* Param_Type1 */ Minus__UINT64 a_L_149,
   /* Param_Type1 */ Minus__UINT16 b_L_150);
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct Minus__uMinus_var_u64_I_Frame_t;typedef struct Minus__uMinus_var_u64_I_Frame_t Minus__uMinus_var_u64_I_Frame_t;
Long__T
__cdecl
Minus__uMinus_var_u64_I(void);
 /* declare_local */
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct Minus__Minus_var_u64_I_Frame_t;typedef struct Minus__Minus_var_u64_I_Frame_t Minus__Minus_var_u64_I_Frame_t;
LONGINT
__cdecl
Minus__Minus_var_u64_I(void);
 /* declare_local */
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct Minus__uMinus_param_u64_I_Frame_t;typedef struct Minus__uMinus_param_u64_I_Frame_t Minus__uMinus_param_u64_I_Frame_t;
 /* declare_local */
 /* internal_declare_param */
 /* internal_declare_param */
Long__T
__cdecl
Minus__uMinus_param_u64_I(
   /* Param_Type1 */ Minus__UINT64 a_L_154,
   /* Param_Type1 */ INTEGER b_L_155);
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct Minus__Minus_param_u64_I_Frame_t;typedef struct Minus__Minus_param_u64_I_Frame_t Minus__Minus_param_u64_I_Frame_t;
 /* declare_local */
 /* internal_declare_param */
 /* internal_declare_param */
LONGINT
__cdecl
Minus__Minus_param_u64_I(
   /* Param_Type1 */ Minus__UINT64 a_L_157,
   /* Param_Type1 */ INTEGER b_L_158);
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct Minus__uMinus_var_u64_i64_Frame_t;typedef struct Minus__uMinus_var_u64_i64_Frame_t Minus__uMinus_var_u64_i64_Frame_t;
Long__T
__cdecl
Minus__uMinus_var_u64_i64(void);
 /* declare_local */
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct Minus__Minus_var_u64_i64_Frame_t;typedef struct Minus__Minus_var_u64_i64_Frame_t Minus__Minus_var_u64_i64_Frame_t;
LONGINT
__cdecl
Minus__Minus_var_u64_i64(void);
 /* declare_local */
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct Minus__uMinus_param_u64_i64_Frame_t;typedef struct Minus__uMinus_param_u64_i64_Frame_t Minus__uMinus_param_u64_i64_Frame_t;
 /* declare_local */
 /* internal_declare_param */
 /* internal_declare_param */
Long__T
__cdecl
Minus__uMinus_param_u64_i64(
   /* Param_Type1 */ Minus__UINT64 a_L_162,
   /* Param_Type1 */ Minus__INT64 b_L_163);
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct Minus__Minus_param_u64_i64_Frame_t;typedef struct Minus__Minus_param_u64_i64_Frame_t Minus__Minus_param_u64_i64_Frame_t;
 /* declare_local */
 /* internal_declare_param */
 /* internal_declare_param */
LONGINT
__cdecl
Minus__Minus_param_u64_i64(
   /* Param_Type1 */ Minus__UINT64 a_L_165,
   /* Param_Type1 */ Minus__INT64 b_L_166);
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct Minus__uMinus_var_u64_i16_Frame_t;typedef struct Minus__uMinus_var_u64_i16_Frame_t Minus__uMinus_var_u64_i16_Frame_t;
Long__T
__cdecl
Minus__uMinus_var_u64_i16(void);
 /* declare_local */
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct Minus__Minus_var_u64_i16_Frame_t;typedef struct Minus__Minus_var_u64_i16_Frame_t Minus__Minus_var_u64_i16_Frame_t;
LONGINT
__cdecl
Minus__Minus_var_u64_i16(void);
 /* declare_local */
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct Minus__uMinus_param_u64_i16_Frame_t;typedef struct Minus__uMinus_param_u64_i16_Frame_t Minus__uMinus_param_u64_i16_Frame_t;
 /* declare_local */
 /* internal_declare_param */
 /* internal_declare_param */
Long__T
__cdecl
Minus__uMinus_param_u64_i16(
   /* Param_Type1 */ Minus__UINT64 a_L_170,
   /* Param_Type1 */ Minus__INT16 b_L_171);
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct Minus__Minus_param_u64_i16_Frame_t;typedef struct Minus__Minus_param_u64_i16_Frame_t Minus__Minus_param_u64_i16_Frame_t;
 /* declare_local */
 /* internal_declare_param */
 /* internal_declare_param */
LONGINT
__cdecl
Minus__Minus_param_u64_i16(
   /* Param_Type1 */ Minus__UINT64 a_L_173,
   /* Param_Type1 */ Minus__INT16 b_L_174);
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct Minus__uMinus_var_u64_C_Frame_t;typedef struct Minus__uMinus_var_u64_C_Frame_t Minus__uMinus_var_u64_C_Frame_t;
Long__T
__cdecl
Minus__uMinus_var_u64_C(void);
 /* declare_local */
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct Minus__Minus_var_u64_C_Frame_t;typedef struct Minus__Minus_var_u64_C_Frame_t Minus__Minus_var_u64_C_Frame_t;
LONGINT
__cdecl
Minus__Minus_var_u64_C(void);
 /* declare_local */
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct Minus__uMinus_param_u64_C_Frame_t;typedef struct Minus__uMinus_param_u64_C_Frame_t Minus__uMinus_param_u64_C_Frame_t;
 /* declare_local */
 /* internal_declare_param */
 /* internal_declare_param */
Long__T
__cdecl
Minus__uMinus_param_u64_C(
   /* Param_Type1 */ Minus__UINT64 a_L_178,
   /* Param_Type1 */ CARDINAL b_L_179);
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct Minus__Minus_param_u64_C_Frame_t;typedef struct Minus__Minus_param_u64_C_Frame_t Minus__Minus_param_u64_C_Frame_t;
 /* declare_local */
 /* internal_declare_param */
 /* internal_declare_param */
LONGINT
__cdecl
Minus__Minus_param_u64_C(
   /* Param_Type1 */ Minus__UINT64 a_L_181,
   /* Param_Type1 */ CARDINAL b_L_182);
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct Minus__uMinus_var_u64_u32_Frame_t;typedef struct Minus__uMinus_var_u64_u32_Frame_t Minus__uMinus_var_u64_u32_Frame_t;
Long__T
__cdecl
Minus__uMinus_var_u64_u32(void);
 /* declare_local */
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct Minus__Minus_var_u64_u32_Frame_t;typedef struct Minus__Minus_var_u64_u32_Frame_t Minus__Minus_var_u64_u32_Frame_t;
LONGINT
__cdecl
Minus__Minus_var_u64_u32(void);
 /* declare_local */
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct Minus__uMinus_param_u64_u32_Frame_t;typedef struct Minus__uMinus_param_u64_u32_Frame_t Minus__uMinus_param_u64_u32_Frame_t;
 /* declare_local */
 /* internal_declare_param */
 /* internal_declare_param */
Long__T
__cdecl
Minus__uMinus_param_u64_u32(
   /* Param_Type1 */ Minus__UINT64 a_L_186,
   /* Param_Type1 */ Minus__UINT32 b_L_187);
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct Minus__Minus_param_u64_u32_Frame_t;typedef struct Minus__Minus_param_u64_u32_Frame_t Minus__Minus_param_u64_u32_Frame_t;
 /* declare_local */
 /* internal_declare_param */
 /* internal_declare_param */
LONGINT
__cdecl
Minus__Minus_param_u64_u32(
   /* Param_Type1 */ Minus__UINT64 a_L_189,
   /* Param_Type1 */ Minus__UINT32 b_L_190);
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct Minus__uMinus_var_u64_u8_Frame_t;typedef struct Minus__uMinus_var_u64_u8_Frame_t Minus__uMinus_var_u64_u8_Frame_t;
Long__T
__cdecl
Minus__uMinus_var_u64_u8(void);
 /* declare_local */
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct Minus__Minus_var_u64_u8_Frame_t;typedef struct Minus__Minus_var_u64_u8_Frame_t Minus__Minus_var_u64_u8_Frame_t;
LONGINT
__cdecl
Minus__Minus_var_u64_u8(void);
 /* declare_local */
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct Minus__uMinus_param_u64_u8_Frame_t;typedef struct Minus__uMinus_param_u64_u8_Frame_t Minus__uMinus_param_u64_u8_Frame_t;
 /* declare_local */
 /* internal_declare_param */
 /* internal_declare_param */
Long__T
__cdecl
Minus__uMinus_param_u64_u8(
   /* Param_Type1 */ Minus__UINT64 a_L_194,
   /* Param_Type1 */ Minus__UINT8 b_L_195);
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct Minus__Minus_param_u64_u8_Frame_t;typedef struct Minus__Minus_param_u64_u8_Frame_t Minus__Minus_param_u64_u8_Frame_t;
 /* declare_local */
 /* internal_declare_param */
 /* internal_declare_param */
LONGINT
__cdecl
Minus__Minus_param_u64_u8(
   /* Param_Type1 */ Minus__UINT64 a_L_197,
   /* Param_Type1 */ Minus__UINT8 b_L_198);
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct Minus__uMinus_var_u64_L_Frame_t;typedef struct Minus__uMinus_var_u64_L_Frame_t Minus__uMinus_var_u64_L_Frame_t;
Long__T
__cdecl
Minus__uMinus_var_u64_L(void);
 /* declare_local */
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct Minus__Minus_var_u64_L_Frame_t;typedef struct Minus__Minus_var_u64_L_Frame_t Minus__Minus_var_u64_L_Frame_t;
LONGINT
__cdecl
Minus__Minus_var_u64_L(void);
 /* declare_local */
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct Minus__uMinus_param_u64_L_Frame_t;typedef struct Minus__uMinus_param_u64_L_Frame_t Minus__uMinus_param_u64_L_Frame_t;
 /* declare_local */
 /* internal_declare_param */
 /* internal_declare_param */
Long__T
__cdecl
Minus__uMinus_param_u64_L(
   /* Param_Type1 */ Minus__UINT64 a_L_202,
   /* Param_Type1 */ LONGINT b_L_203);
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct Minus__Minus_param_u64_L_Frame_t;typedef struct Minus__Minus_param_u64_L_Frame_t Minus__Minus_param_u64_L_Frame_t;
 /* declare_local */
 /* internal_declare_param */
 /* internal_declare_param */
LONGINT
__cdecl
Minus__Minus_param_u64_L(
   /* Param_Type1 */ Minus__UINT64 a_L_205,
   /* Param_Type1 */ LONGINT b_L_206);
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct Minus__Minus_var_f64_f64_Frame_t;typedef struct Minus__Minus_var_f64_f64_Frame_t Minus__Minus_var_f64_f64_Frame_t;
Minus__FLOAT64
__cdecl
Minus__Minus_var_f64_f64(void);
 /* declare_local */
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct Minus__Minus_param_f64_f64_Frame_t;typedef struct Minus__Minus_param_f64_f64_Frame_t Minus__Minus_param_f64_f64_Frame_t;
 /* declare_local */
 /* internal_declare_param */
 /* internal_declare_param */
Minus__FLOAT64
__cdecl
Minus__Minus_param_f64_f64(
   /* Param_Type1 */ Minus__FLOAT64 a_L_209,
   /* Param_Type1 */ Minus__FLOAT64 b_L_210);
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct Minus__uMinus_var_i32_i8_Frame_t;typedef struct Minus__uMinus_var_i32_i8_Frame_t Minus__uMinus_var_i32_i8_Frame_t;
Word__T
__cdecl
Minus__uMinus_var_i32_i8(void);
 /* declare_local */
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct Minus__Minus_var_i32_i8_Frame_t;typedef struct Minus__Minus_var_i32_i8_Frame_t Minus__Minus_var_i32_i8_Frame_t;
Minus__INT32
__cdecl
Minus__Minus_var_i32_i8(void);
 /* declare_local */
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct Minus__uMinus_param_i32_i8_Frame_t;typedef struct Minus__uMinus_param_i32_i8_Frame_t Minus__uMinus_param_i32_i8_Frame_t;
 /* declare_local */
 /* internal_declare_param */
 /* internal_declare_param */
Word__T
__cdecl
Minus__uMinus_param_i32_i8(
   /* Param_Type1 */ Minus__INT32 a_L_214,
   /* Param_Type1 */ Minus__INT8 b_L_215);
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct Minus__Minus_param_i32_i8_Frame_t;typedef struct Minus__Minus_param_i32_i8_Frame_t Minus__Minus_param_i32_i8_Frame_t;
 /* declare_local */
 /* internal_declare_param */
 /* internal_declare_param */
Minus__INT32
__cdecl
Minus__Minus_param_i32_i8(
   /* Param_Type1 */ Minus__INT32 a_L_217,
   /* Param_Type1 */ Minus__INT8 b_L_218);
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct Minus__uMinus_var_i32_u64_Frame_t;typedef struct Minus__uMinus_var_i32_u64_Frame_t Minus__uMinus_var_i32_u64_Frame_t;
Long__T
__cdecl
Minus__uMinus_var_i32_u64(void);
 /* declare_local */
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct Minus__Minus_var_i32_u64_Frame_t;typedef struct Minus__Minus_var_i32_u64_Frame_t Minus__Minus_var_i32_u64_Frame_t;
LONGINT
__cdecl
Minus__Minus_var_i32_u64(void);
 /* declare_local */
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct Minus__uMinus_param_i32_u64_Frame_t;typedef struct Minus__uMinus_param_i32_u64_Frame_t Minus__uMinus_param_i32_u64_Frame_t;
 /* declare_local */
 /* internal_declare_param */
 /* internal_declare_param */
Long__T
__cdecl
Minus__uMinus_param_i32_u64(
   /* Param_Type1 */ Minus__INT32 a_L_222,
   /* Param_Type1 */ Minus__UINT64 b_L_223);
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct Minus__Minus_param_i32_u64_Frame_t;typedef struct Minus__Minus_param_i32_u64_Frame_t Minus__Minus_param_i32_u64_Frame_t;
 /* declare_local */
 /* internal_declare_param */
 /* internal_declare_param */
LONGINT
__cdecl
Minus__Minus_param_i32_u64(
   /* Param_Type1 */ Minus__INT32 a_L_225,
   /* Param_Type1 */ Minus__UINT64 b_L_226);
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct Minus__uMinus_var_i32_i32_Frame_t;typedef struct Minus__uMinus_var_i32_i32_Frame_t Minus__uMinus_var_i32_i32_Frame_t;
Word__T
__cdecl
Minus__uMinus_var_i32_i32(void);
 /* declare_local */
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct Minus__Minus_var_i32_i32_Frame_t;typedef struct Minus__Minus_var_i32_i32_Frame_t Minus__Minus_var_i32_i32_Frame_t;
Minus__INT32
__cdecl
Minus__Minus_var_i32_i32(void);
 /* declare_local */
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct Minus__uMinus_param_i32_i32_Frame_t;typedef struct Minus__uMinus_param_i32_i32_Frame_t Minus__uMinus_param_i32_i32_Frame_t;
 /* declare_local */
 /* internal_declare_param */
 /* internal_declare_param */
Word__T
__cdecl
Minus__uMinus_param_i32_i32(
   /* Param_Type1 */ Minus__INT32 a_L_230,
   /* Param_Type1 */ Minus__INT32 b_L_231);
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct Minus__Minus_param_i32_i32_Frame_t;typedef struct Minus__Minus_param_i32_i32_Frame_t Minus__Minus_param_i32_i32_Frame_t;
 /* declare_local */
 /* internal_declare_param */
 /* internal_declare_param */
Minus__INT32
__cdecl
Minus__Minus_param_i32_i32(
   /* Param_Type1 */ Minus__INT32 a_L_233,
   /* Param_Type1 */ Minus__INT32 b_L_234);
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct Minus__uMinus_var_i32_LC_Frame_t;typedef struct Minus__uMinus_var_i32_LC_Frame_t Minus__uMinus_var_i32_LC_Frame_t;
Long__T
__cdecl
Minus__uMinus_var_i32_LC(void);
 /* declare_local */
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct Minus__Minus_var_i32_LC_Frame_t;typedef struct Minus__Minus_var_i32_LC_Frame_t Minus__Minus_var_i32_LC_Frame_t;
LONGINT
__cdecl
Minus__Minus_var_i32_LC(void);
 /* declare_local */
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct Minus__uMinus_param_i32_LC_Frame_t;typedef struct Minus__uMinus_param_i32_LC_Frame_t Minus__uMinus_param_i32_LC_Frame_t;
 /* declare_local */
 /* internal_declare_param */
 /* internal_declare_param */
Long__T
__cdecl
Minus__uMinus_param_i32_LC(
   /* Param_Type1 */ Minus__INT32 a_L_238,
   /* Param_Type1 */ LONGCARD b_L_239);
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct Minus__Minus_param_i32_LC_Frame_t;typedef struct Minus__Minus_param_i32_LC_Frame_t Minus__Minus_param_i32_LC_Frame_t;
 /* declare_local */
 /* internal_declare_param */
 /* internal_declare_param */
LONGINT
__cdecl
Minus__Minus_param_i32_LC(
   /* Param_Type1 */ Minus__INT32 a_L_241,
   /* Param_Type1 */ LONGCARD b_L_242);
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct Minus__uMinus_var_i32_u16_Frame_t;typedef struct Minus__uMinus_var_i32_u16_Frame_t Minus__uMinus_var_i32_u16_Frame_t;
Word__T
__cdecl
Minus__uMinus_var_i32_u16(void);
 /* declare_local */
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct Minus__Minus_var_i32_u16_Frame_t;typedef struct Minus__Minus_var_i32_u16_Frame_t Minus__Minus_var_i32_u16_Frame_t;
Minus__INT32
__cdecl
Minus__Minus_var_i32_u16(void);
 /* declare_local */
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct Minus__uMinus_param_i32_u16_Frame_t;typedef struct Minus__uMinus_param_i32_u16_Frame_t Minus__uMinus_param_i32_u16_Frame_t;
 /* declare_local */
 /* internal_declare_param */
 /* internal_declare_param */
Word__T
__cdecl
Minus__uMinus_param_i32_u16(
   /* Param_Type1 */ Minus__INT32 a_L_246,
   /* Param_Type1 */ Minus__UINT16 b_L_247);
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct Minus__Minus_param_i32_u16_Frame_t;typedef struct Minus__Minus_param_i32_u16_Frame_t Minus__Minus_param_i32_u16_Frame_t;
 /* declare_local */
 /* internal_declare_param */
 /* internal_declare_param */
Minus__INT32
__cdecl
Minus__Minus_param_i32_u16(
   /* Param_Type1 */ Minus__INT32 a_L_249,
   /* Param_Type1 */ Minus__UINT16 b_L_250);
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct Minus__uMinus_var_i32_I_Frame_t;typedef struct Minus__uMinus_var_i32_I_Frame_t Minus__uMinus_var_i32_I_Frame_t;
Word__T
__cdecl
Minus__uMinus_var_i32_I(void);
 /* declare_local */
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct Minus__Minus_var_i32_I_Frame_t;typedef struct Minus__Minus_var_i32_I_Frame_t Minus__Minus_var_i32_I_Frame_t;
Minus__INT32
__cdecl
Minus__Minus_var_i32_I(void);
 /* declare_local */
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct Minus__uMinus_param_i32_I_Frame_t;typedef struct Minus__uMinus_param_i32_I_Frame_t Minus__uMinus_param_i32_I_Frame_t;
 /* declare_local */
 /* internal_declare_param */
 /* internal_declare_param */
Word__T
__cdecl
Minus__uMinus_param_i32_I(
   /* Param_Type1 */ Minus__INT32 a_L_254,
   /* Param_Type1 */ INTEGER b_L_255);
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct Minus__Minus_param_i32_I_Frame_t;typedef struct Minus__Minus_param_i32_I_Frame_t Minus__Minus_param_i32_I_Frame_t;
 /* declare_local */
 /* internal_declare_param */
 /* internal_declare_param */
Minus__INT32
__cdecl
Minus__Minus_param_i32_I(
   /* Param_Type1 */ Minus__INT32 a_L_257,
   /* Param_Type1 */ INTEGER b_L_258);
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct Minus__uMinus_var_i32_i64_Frame_t;typedef struct Minus__uMinus_var_i32_i64_Frame_t Minus__uMinus_var_i32_i64_Frame_t;
Long__T
__cdecl
Minus__uMinus_var_i32_i64(void);
 /* declare_local */
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct Minus__Minus_var_i32_i64_Frame_t;typedef struct Minus__Minus_var_i32_i64_Frame_t Minus__Minus_var_i32_i64_Frame_t;
LONGINT
__cdecl
Minus__Minus_var_i32_i64(void);
 /* declare_local */
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct Minus__uMinus_param_i32_i64_Frame_t;typedef struct Minus__uMinus_param_i32_i64_Frame_t Minus__uMinus_param_i32_i64_Frame_t;
 /* declare_local */
 /* internal_declare_param */
 /* internal_declare_param */
Long__T
__cdecl
Minus__uMinus_param_i32_i64(
   /* Param_Type1 */ Minus__INT32 a_L_262,
   /* Param_Type1 */ Minus__INT64 b_L_263);
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct Minus__Minus_param_i32_i64_Frame_t;typedef struct Minus__Minus_param_i32_i64_Frame_t Minus__Minus_param_i32_i64_Frame_t;
 /* declare_local */
 /* internal_declare_param */
 /* internal_declare_param */
LONGINT
__cdecl
Minus__Minus_param_i32_i64(
   /* Param_Type1 */ Minus__INT32 a_L_265,
   /* Param_Type1 */ Minus__INT64 b_L_266);
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct Minus__uMinus_var_i32_i16_Frame_t;typedef struct Minus__uMinus_var_i32_i16_Frame_t Minus__uMinus_var_i32_i16_Frame_t;
Word__T
__cdecl
Minus__uMinus_var_i32_i16(void);
 /* declare_local */
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct Minus__Minus_var_i32_i16_Frame_t;typedef struct Minus__Minus_var_i32_i16_Frame_t Minus__Minus_var_i32_i16_Frame_t;
Minus__INT32
__cdecl
Minus__Minus_var_i32_i16(void);
 /* declare_local */
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct Minus__uMinus_param_i32_i16_Frame_t;typedef struct Minus__uMinus_param_i32_i16_Frame_t Minus__uMinus_param_i32_i16_Frame_t;
 /* declare_local */
 /* internal_declare_param */
 /* internal_declare_param */
Word__T
__cdecl
Minus__uMinus_param_i32_i16(
   /* Param_Type1 */ Minus__INT32 a_L_270,
   /* Param_Type1 */ Minus__INT16 b_L_271);
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct Minus__Minus_param_i32_i16_Frame_t;typedef struct Minus__Minus_param_i32_i16_Frame_t Minus__Minus_param_i32_i16_Frame_t;
 /* declare_local */
 /* internal_declare_param */
 /* internal_declare_param */
Minus__INT32
__cdecl
Minus__Minus_param_i32_i16(
   /* Param_Type1 */ Minus__INT32 a_L_273,
   /* Param_Type1 */ Minus__INT16 b_L_274);
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct Minus__uMinus_var_i32_C_Frame_t;typedef struct Minus__uMinus_var_i32_C_Frame_t Minus__uMinus_var_i32_C_Frame_t;
Word__T
__cdecl
Minus__uMinus_var_i32_C(void);
 /* declare_local */
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct Minus__Minus_var_i32_C_Frame_t;typedef struct Minus__Minus_var_i32_C_Frame_t Minus__Minus_var_i32_C_Frame_t;
Minus__INT32
__cdecl
Minus__Minus_var_i32_C(void);
 /* declare_local */
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct Minus__uMinus_param_i32_C_Frame_t;typedef struct Minus__uMinus_param_i32_C_Frame_t Minus__uMinus_param_i32_C_Frame_t;
 /* declare_local */
 /* internal_declare_param */
 /* internal_declare_param */
Word__T
__cdecl
Minus__uMinus_param_i32_C(
   /* Param_Type1 */ Minus__INT32 a_L_278,
   /* Param_Type1 */ CARDINAL b_L_279);
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct Minus__Minus_param_i32_C_Frame_t;typedef struct Minus__Minus_param_i32_C_Frame_t Minus__Minus_param_i32_C_Frame_t;
 /* declare_local */
 /* internal_declare_param */
 /* internal_declare_param */
Minus__INT32
__cdecl
Minus__Minus_param_i32_C(
   /* Param_Type1 */ Minus__INT32 a_L_281,
   /* Param_Type1 */ CARDINAL b_L_282);
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct Minus__uMinus_var_i32_u32_Frame_t;typedef struct Minus__uMinus_var_i32_u32_Frame_t Minus__uMinus_var_i32_u32_Frame_t;
Word__T
__cdecl
Minus__uMinus_var_i32_u32(void);
 /* declare_local */
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct Minus__Minus_var_i32_u32_Frame_t;typedef struct Minus__Minus_var_i32_u32_Frame_t Minus__Minus_var_i32_u32_Frame_t;
Minus__INT32
__cdecl
Minus__Minus_var_i32_u32(void);
 /* declare_local */
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct Minus__uMinus_param_i32_u32_Frame_t;typedef struct Minus__uMinus_param_i32_u32_Frame_t Minus__uMinus_param_i32_u32_Frame_t;
 /* declare_local */
 /* internal_declare_param */
 /* internal_declare_param */
Word__T
__cdecl
Minus__uMinus_param_i32_u32(
   /* Param_Type1 */ Minus__INT32 a_L_286,
   /* Param_Type1 */ Minus__UINT32 b_L_287);
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct Minus__Minus_param_i32_u32_Frame_t;typedef struct Minus__Minus_param_i32_u32_Frame_t Minus__Minus_param_i32_u32_Frame_t;
 /* declare_local */
 /* internal_declare_param */
 /* internal_declare_param */
Minus__INT32
__cdecl
Minus__Minus_param_i32_u32(
   /* Param_Type1 */ Minus__INT32 a_L_289,
   /* Param_Type1 */ Minus__UINT32 b_L_290);
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct Minus__uMinus_var_i32_u8_Frame_t;typedef struct Minus__uMinus_var_i32_u8_Frame_t Minus__uMinus_var_i32_u8_Frame_t;
Word__T
__cdecl
Minus__uMinus_var_i32_u8(void);
 /* declare_local */
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct Minus__Minus_var_i32_u8_Frame_t;typedef struct Minus__Minus_var_i32_u8_Frame_t Minus__Minus_var_i32_u8_Frame_t;
Minus__INT32
__cdecl
Minus__Minus_var_i32_u8(void);
 /* declare_local */
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct Minus__uMinus_param_i32_u8_Frame_t;typedef struct Minus__uMinus_param_i32_u8_Frame_t Minus__uMinus_param_i32_u8_Frame_t;
 /* declare_local */
 /* internal_declare_param */
 /* internal_declare_param */
Word__T
__cdecl
Minus__uMinus_param_i32_u8(
   /* Param_Type1 */ Minus__INT32 a_L_294,
   /* Param_Type1 */ Minus__UINT8 b_L_295);
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct Minus__Minus_param_i32_u8_Frame_t;typedef struct Minus__Minus_param_i32_u8_Frame_t Minus__Minus_param_i32_u8_Frame_t;
 /* declare_local */
 /* internal_declare_param */
 /* internal_declare_param */
Minus__INT32
__cdecl
Minus__Minus_param_i32_u8(
   /* Param_Type1 */ Minus__INT32 a_L_297,
   /* Param_Type1 */ Minus__UINT8 b_L_298);
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct Minus__uMinus_var_i32_L_Frame_t;typedef struct Minus__uMinus_var_i32_L_Frame_t Minus__uMinus_var_i32_L_Frame_t;
Long__T
__cdecl
Minus__uMinus_var_i32_L(void);
 /* declare_local */
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct Minus__Minus_var_i32_L_Frame_t;typedef struct Minus__Minus_var_i32_L_Frame_t Minus__Minus_var_i32_L_Frame_t;
LONGINT
__cdecl
Minus__Minus_var_i32_L(void);
 /* declare_local */
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct Minus__uMinus_param_i32_L_Frame_t;typedef struct Minus__uMinus_param_i32_L_Frame_t Minus__uMinus_param_i32_L_Frame_t;
 /* declare_local */
 /* internal_declare_param */
 /* internal_declare_param */
Long__T
__cdecl
Minus__uMinus_param_i32_L(
   /* Param_Type1 */ Minus__INT32 a_L_302,
   /* Param_Type1 */ LONGINT b_L_303);
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct Minus__Minus_param_i32_L_Frame_t;typedef struct Minus__Minus_param_i32_L_Frame_t Minus__Minus_param_i32_L_Frame_t;
 /* declare_local */
 /* internal_declare_param */
 /* internal_declare_param */
LONGINT
__cdecl
Minus__Minus_param_i32_L(
   /* Param_Type1 */ Minus__INT32 a_L_305,
   /* Param_Type1 */ LONGINT b_L_306);
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct Minus__uMinus_var_LC_i8_Frame_t;typedef struct Minus__uMinus_var_LC_i8_Frame_t Minus__uMinus_var_LC_i8_Frame_t;
Long__T
__cdecl
Minus__uMinus_var_LC_i8(void);
 /* declare_local */
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct Minus__Minus_var_LC_i8_Frame_t;typedef struct Minus__Minus_var_LC_i8_Frame_t Minus__Minus_var_LC_i8_Frame_t;
LONGINT
__cdecl
Minus__Minus_var_LC_i8(void);
 /* declare_local */
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct Minus__uMinus_param_LC_i8_Frame_t;typedef struct Minus__uMinus_param_LC_i8_Frame_t Minus__uMinus_param_LC_i8_Frame_t;
 /* declare_local */
 /* internal_declare_param */
 /* internal_declare_param */
Long__T
__cdecl
Minus__uMinus_param_LC_i8(
   /* Param_Type1 */ LONGCARD a_L_310,
   /* Param_Type1 */ Minus__INT8 b_L_311);
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct Minus__Minus_param_LC_i8_Frame_t;typedef struct Minus__Minus_param_LC_i8_Frame_t Minus__Minus_param_LC_i8_Frame_t;
 /* declare_local */
 /* internal_declare_param */
 /* internal_declare_param */
LONGINT
__cdecl
Minus__Minus_param_LC_i8(
   /* Param_Type1 */ LONGCARD a_L_313,
   /* Param_Type1 */ Minus__INT8 b_L_314);
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct Minus__uMinus_var_LC_u64_Frame_t;typedef struct Minus__uMinus_var_LC_u64_Frame_t Minus__uMinus_var_LC_u64_Frame_t;
Long__T
__cdecl
Minus__uMinus_var_LC_u64(void);
 /* declare_local */
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct Minus__Minus_var_LC_u64_Frame_t;typedef struct Minus__Minus_var_LC_u64_Frame_t Minus__Minus_var_LC_u64_Frame_t;
LONGINT
__cdecl
Minus__Minus_var_LC_u64(void);
 /* declare_local */
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct Minus__uMinus_param_LC_u64_Frame_t;typedef struct Minus__uMinus_param_LC_u64_Frame_t Minus__uMinus_param_LC_u64_Frame_t;
 /* declare_local */
 /* internal_declare_param */
 /* internal_declare_param */
Long__T
__cdecl
Minus__uMinus_param_LC_u64(
   /* Param_Type1 */ LONGCARD a_L_318,
   /* Param_Type1 */ Minus__UINT64 b_L_319);
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct Minus__Minus_param_LC_u64_Frame_t;typedef struct Minus__Minus_param_LC_u64_Frame_t Minus__Minus_param_LC_u64_Frame_t;
 /* declare_local */
 /* internal_declare_param */
 /* internal_declare_param */
LONGINT
__cdecl
Minus__Minus_param_LC_u64(
   /* Param_Type1 */ LONGCARD a_L_321,
   /* Param_Type1 */ Minus__UINT64 b_L_322);
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct Minus__uMinus_var_LC_i32_Frame_t;typedef struct Minus__uMinus_var_LC_i32_Frame_t Minus__uMinus_var_LC_i32_Frame_t;
Long__T
__cdecl
Minus__uMinus_var_LC_i32(void);
 /* declare_local */
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct Minus__Minus_var_LC_i32_Frame_t;typedef struct Minus__Minus_var_LC_i32_Frame_t Minus__Minus_var_LC_i32_Frame_t;
LONGINT
__cdecl
Minus__Minus_var_LC_i32(void);
 /* declare_local */
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct Minus__uMinus_param_LC_i32_Frame_t;typedef struct Minus__uMinus_param_LC_i32_Frame_t Minus__uMinus_param_LC_i32_Frame_t;
 /* declare_local */
 /* internal_declare_param */
 /* internal_declare_param */
Long__T
__cdecl
Minus__uMinus_param_LC_i32(
   /* Param_Type1 */ LONGCARD a_L_326,
   /* Param_Type1 */ Minus__INT32 b_L_327);
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct Minus__Minus_param_LC_i32_Frame_t;typedef struct Minus__Minus_param_LC_i32_Frame_t Minus__Minus_param_LC_i32_Frame_t;
 /* declare_local */
 /* internal_declare_param */
 /* internal_declare_param */
LONGINT
__cdecl
Minus__Minus_param_LC_i32(
   /* Param_Type1 */ LONGCARD a_L_329,
   /* Param_Type1 */ Minus__INT32 b_L_330);
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct Minus__uMinus_var_LC_LC_Frame_t;typedef struct Minus__uMinus_var_LC_LC_Frame_t Minus__uMinus_var_LC_LC_Frame_t;
Long__T
__cdecl
Minus__uMinus_var_LC_LC(void);
 /* declare_local */
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct Minus__Minus_var_LC_LC_Frame_t;typedef struct Minus__Minus_var_LC_LC_Frame_t Minus__Minus_var_LC_LC_Frame_t;
LONGINT
__cdecl
Minus__Minus_var_LC_LC(void);
 /* declare_local */
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct Minus__uMinus_param_LC_LC_Frame_t;typedef struct Minus__uMinus_param_LC_LC_Frame_t Minus__uMinus_param_LC_LC_Frame_t;
 /* declare_local */
 /* internal_declare_param */
 /* internal_declare_param */
Long__T
__cdecl
Minus__uMinus_param_LC_LC(
   /* Param_Type1 */ LONGCARD a_L_334,
   /* Param_Type1 */ LONGCARD b_L_335);
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct Minus__Minus_param_LC_LC_Frame_t;typedef struct Minus__Minus_param_LC_LC_Frame_t Minus__Minus_param_LC_LC_Frame_t;
 /* declare_local */
 /* internal_declare_param */
 /* internal_declare_param */
LONGINT
__cdecl
Minus__Minus_param_LC_LC(
   /* Param_Type1 */ LONGCARD a_L_337,
   /* Param_Type1 */ LONGCARD b_L_338);
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct Minus__uMinus_var_LC_u16_Frame_t;typedef struct Minus__uMinus_var_LC_u16_Frame_t Minus__uMinus_var_LC_u16_Frame_t;
Long__T
__cdecl
Minus__uMinus_var_LC_u16(void);
 /* declare_local */
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct Minus__Minus_var_LC_u16_Frame_t;typedef struct Minus__Minus_var_LC_u16_Frame_t Minus__Minus_var_LC_u16_Frame_t;
LONGINT
__cdecl
Minus__Minus_var_LC_u16(void);
 /* declare_local */
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct Minus__uMinus_param_LC_u16_Frame_t;typedef struct Minus__uMinus_param_LC_u16_Frame_t Minus__uMinus_param_LC_u16_Frame_t;
 /* declare_local */
 /* internal_declare_param */
 /* internal_declare_param */
Long__T
__cdecl
Minus__uMinus_param_LC_u16(
   /* Param_Type1 */ LONGCARD a_L_342,
   /* Param_Type1 */ Minus__UINT16 b_L_343);
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct Minus__Minus_param_LC_u16_Frame_t;typedef struct Minus__Minus_param_LC_u16_Frame_t Minus__Minus_param_LC_u16_Frame_t;
 /* declare_local */
 /* internal_declare_param */
 /* internal_declare_param */
LONGINT
__cdecl
Minus__Minus_param_LC_u16(
   /* Param_Type1 */ LONGCARD a_L_345,
   /* Param_Type1 */ Minus__UINT16 b_L_346);
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct Minus__uMinus_var_LC_I_Frame_t;typedef struct Minus__uMinus_var_LC_I_Frame_t Minus__uMinus_var_LC_I_Frame_t;
Long__T
__cdecl
Minus__uMinus_var_LC_I(void);
 /* declare_local */
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct Minus__Minus_var_LC_I_Frame_t;typedef struct Minus__Minus_var_LC_I_Frame_t Minus__Minus_var_LC_I_Frame_t;
LONGINT
__cdecl
Minus__Minus_var_LC_I(void);
 /* declare_local */
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct Minus__uMinus_param_LC_I_Frame_t;typedef struct Minus__uMinus_param_LC_I_Frame_t Minus__uMinus_param_LC_I_Frame_t;
 /* declare_local */
 /* internal_declare_param */
 /* internal_declare_param */
Long__T
__cdecl
Minus__uMinus_param_LC_I(
   /* Param_Type1 */ LONGCARD a_L_350,
   /* Param_Type1 */ INTEGER b_L_351);
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct Minus__Minus_param_LC_I_Frame_t;typedef struct Minus__Minus_param_LC_I_Frame_t Minus__Minus_param_LC_I_Frame_t;
 /* declare_local */
 /* internal_declare_param */
 /* internal_declare_param */
LONGINT
__cdecl
Minus__Minus_param_LC_I(
   /* Param_Type1 */ LONGCARD a_L_353,
   /* Param_Type1 */ INTEGER b_L_354);
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct Minus__uMinus_var_LC_i64_Frame_t;typedef struct Minus__uMinus_var_LC_i64_Frame_t Minus__uMinus_var_LC_i64_Frame_t;
Long__T
__cdecl
Minus__uMinus_var_LC_i64(void);
 /* declare_local */
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct Minus__Minus_var_LC_i64_Frame_t;typedef struct Minus__Minus_var_LC_i64_Frame_t Minus__Minus_var_LC_i64_Frame_t;
LONGINT
__cdecl
Minus__Minus_var_LC_i64(void);
 /* declare_local */
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct Minus__uMinus_param_LC_i64_Frame_t;typedef struct Minus__uMinus_param_LC_i64_Frame_t Minus__uMinus_param_LC_i64_Frame_t;
 /* declare_local */
 /* internal_declare_param */
 /* internal_declare_param */
Long__T
__cdecl
Minus__uMinus_param_LC_i64(
   /* Param_Type1 */ LONGCARD a_L_358,
   /* Param_Type1 */ Minus__INT64 b_L_359);
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct Minus__Minus_param_LC_i64_Frame_t;typedef struct Minus__Minus_param_LC_i64_Frame_t Minus__Minus_param_LC_i64_Frame_t;
 /* declare_local */
 /* internal_declare_param */
 /* internal_declare_param */
LONGINT
__cdecl
Minus__Minus_param_LC_i64(
   /* Param_Type1 */ LONGCARD a_L_361,
   /* Param_Type1 */ Minus__INT64 b_L_362);
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct Minus__uMinus_var_LC_i16_Frame_t;typedef struct Minus__uMinus_var_LC_i16_Frame_t Minus__uMinus_var_LC_i16_Frame_t;
Long__T
__cdecl
Minus__uMinus_var_LC_i16(void);
 /* declare_local */
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct Minus__Minus_var_LC_i16_Frame_t;typedef struct Minus__Minus_var_LC_i16_Frame_t Minus__Minus_var_LC_i16_Frame_t;
LONGINT
__cdecl
Minus__Minus_var_LC_i16(void);
 /* declare_local */
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct Minus__uMinus_param_LC_i16_Frame_t;typedef struct Minus__uMinus_param_LC_i16_Frame_t Minus__uMinus_param_LC_i16_Frame_t;
 /* declare_local */
 /* internal_declare_param */
 /* internal_declare_param */
Long__T
__cdecl
Minus__uMinus_param_LC_i16(
   /* Param_Type1 */ LONGCARD a_L_366,
   /* Param_Type1 */ Minus__INT16 b_L_367);
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct Minus__Minus_param_LC_i16_Frame_t;typedef struct Minus__Minus_param_LC_i16_Frame_t Minus__Minus_param_LC_i16_Frame_t;
 /* declare_local */
 /* internal_declare_param */
 /* internal_declare_param */
LONGINT
__cdecl
Minus__Minus_param_LC_i16(
   /* Param_Type1 */ LONGCARD a_L_369,
   /* Param_Type1 */ Minus__INT16 b_L_370);
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct Minus__uMinus_var_LC_C_Frame_t;typedef struct Minus__uMinus_var_LC_C_Frame_t Minus__uMinus_var_LC_C_Frame_t;
Long__T
__cdecl
Minus__uMinus_var_LC_C(void);
 /* declare_local */
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct Minus__Minus_var_LC_C_Frame_t;typedef struct Minus__Minus_var_LC_C_Frame_t Minus__Minus_var_LC_C_Frame_t;
LONGINT
__cdecl
Minus__Minus_var_LC_C(void);
 /* declare_local */
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct Minus__uMinus_param_LC_C_Frame_t;typedef struct Minus__uMinus_param_LC_C_Frame_t Minus__uMinus_param_LC_C_Frame_t;
 /* declare_local */
 /* internal_declare_param */
 /* internal_declare_param */
Long__T
__cdecl
Minus__uMinus_param_LC_C(
   /* Param_Type1 */ LONGCARD a_L_374,
   /* Param_Type1 */ CARDINAL b_L_375);
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct Minus__Minus_param_LC_C_Frame_t;typedef struct Minus__Minus_param_LC_C_Frame_t Minus__Minus_param_LC_C_Frame_t;
 /* declare_local */
 /* internal_declare_param */
 /* internal_declare_param */
LONGINT
__cdecl
Minus__Minus_param_LC_C(
   /* Param_Type1 */ LONGCARD a_L_377,
   /* Param_Type1 */ CARDINAL b_L_378);
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct Minus__uMinus_var_LC_u32_Frame_t;typedef struct Minus__uMinus_var_LC_u32_Frame_t Minus__uMinus_var_LC_u32_Frame_t;
Long__T
__cdecl
Minus__uMinus_var_LC_u32(void);
 /* declare_local */
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct Minus__Minus_var_LC_u32_Frame_t;typedef struct Minus__Minus_var_LC_u32_Frame_t Minus__Minus_var_LC_u32_Frame_t;
LONGINT
__cdecl
Minus__Minus_var_LC_u32(void);
 /* declare_local */
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct Minus__uMinus_param_LC_u32_Frame_t;typedef struct Minus__uMinus_param_LC_u32_Frame_t Minus__uMinus_param_LC_u32_Frame_t;
 /* declare_local */
 /* internal_declare_param */
 /* internal_declare_param */
Long__T
__cdecl
Minus__uMinus_param_LC_u32(
   /* Param_Type1 */ LONGCARD a_L_382,
   /* Param_Type1 */ Minus__UINT32 b_L_383);
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct Minus__Minus_param_LC_u32_Frame_t;typedef struct Minus__Minus_param_LC_u32_Frame_t Minus__Minus_param_LC_u32_Frame_t;
 /* declare_local */
 /* internal_declare_param */
 /* internal_declare_param */
LONGINT
__cdecl
Minus__Minus_param_LC_u32(
   /* Param_Type1 */ LONGCARD a_L_385,
   /* Param_Type1 */ Minus__UINT32 b_L_387);
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct Minus__uMinus_var_LC_u8_Frame_t;typedef struct Minus__uMinus_var_LC_u8_Frame_t Minus__uMinus_var_LC_u8_Frame_t;
Long__T
__cdecl
Minus__uMinus_var_LC_u8(void);
 /* declare_local */
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct Minus__Minus_var_LC_u8_Frame_t;typedef struct Minus__Minus_var_LC_u8_Frame_t Minus__Minus_var_LC_u8_Frame_t;
LONGINT
__cdecl
Minus__Minus_var_LC_u8(void);
 /* declare_local */
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct Minus__uMinus_param_LC_u8_Frame_t;typedef struct Minus__uMinus_param_LC_u8_Frame_t Minus__uMinus_param_LC_u8_Frame_t;
 /* declare_local */
 /* internal_declare_param */
 /* internal_declare_param */
Long__T
__cdecl
Minus__uMinus_param_LC_u8(
   /* Param_Type1 */ LONGCARD a_L_391,
   /* Param_Type1 */ Minus__UINT8 b_L_392);
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct Minus__Minus_param_LC_u8_Frame_t;typedef struct Minus__Minus_param_LC_u8_Frame_t Minus__Minus_param_LC_u8_Frame_t;
 /* declare_local */
 /* internal_declare_param */
 /* internal_declare_param */
LONGINT
__cdecl
Minus__Minus_param_LC_u8(
   /* Param_Type1 */ LONGCARD a_L_394,
   /* Param_Type1 */ Minus__UINT8 b_L_395);
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct Minus__uMinus_var_LC_L_Frame_t;typedef struct Minus__uMinus_var_LC_L_Frame_t Minus__uMinus_var_LC_L_Frame_t;
Long__T
__cdecl
Minus__uMinus_var_LC_L(void);
 /* declare_local */
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct Minus__Minus_var_LC_L_Frame_t;typedef struct Minus__Minus_var_LC_L_Frame_t Minus__Minus_var_LC_L_Frame_t;
LONGINT
__cdecl
Minus__Minus_var_LC_L(void);
 /* declare_local */
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct Minus__uMinus_param_LC_L_Frame_t;typedef struct Minus__uMinus_param_LC_L_Frame_t Minus__uMinus_param_LC_L_Frame_t;
 /* declare_local */
 /* internal_declare_param */
 /* internal_declare_param */
Long__T
__cdecl
Minus__uMinus_param_LC_L(
   /* Param_Type1 */ LONGCARD a_L_399,
   /* Param_Type1 */ LONGINT b_L_400);
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct Minus__Minus_param_LC_L_Frame_t;typedef struct Minus__Minus_param_LC_L_Frame_t Minus__Minus_param_LC_L_Frame_t;
 /* declare_local */
 /* internal_declare_param */
 /* internal_declare_param */
LONGINT
__cdecl
Minus__Minus_param_LC_L(
   /* Param_Type1 */ LONGCARD a_L_402,
   /* Param_Type1 */ LONGINT b_L_403);
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct Minus__uMinus_var_u16_i8_Frame_t;typedef struct Minus__uMinus_var_u16_i8_Frame_t Minus__uMinus_var_u16_i8_Frame_t;
Word__T
__cdecl
Minus__uMinus_var_u16_i8(void);
 /* declare_local */
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct Minus__Minus_var_u16_i8_Frame_t;typedef struct Minus__Minus_var_u16_i8_Frame_t Minus__Minus_var_u16_i8_Frame_t;
Minus__UINT16
__cdecl
Minus__Minus_var_u16_i8(void);
 /* declare_local */
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct Minus__uMinus_param_u16_i8_Frame_t;typedef struct Minus__uMinus_param_u16_i8_Frame_t Minus__uMinus_param_u16_i8_Frame_t;
 /* declare_local */
 /* internal_declare_param */
 /* internal_declare_param */
Word__T
__cdecl
Minus__uMinus_param_u16_i8(
   /* Param_Type1 */ Minus__UINT16 a_L_407,
   /* Param_Type1 */ Minus__INT8 b_L_408);
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct Minus__Minus_param_u16_i8_Frame_t;typedef struct Minus__Minus_param_u16_i8_Frame_t Minus__Minus_param_u16_i8_Frame_t;
 /* declare_local */
 /* internal_declare_param */
 /* internal_declare_param */
Minus__UINT16
__cdecl
Minus__Minus_param_u16_i8(
   /* Param_Type1 */ Minus__UINT16 a_L_410,
   /* Param_Type1 */ Minus__INT8 b_L_411);
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct Minus__uMinus_var_u16_u64_Frame_t;typedef struct Minus__uMinus_var_u16_u64_Frame_t Minus__uMinus_var_u16_u64_Frame_t;
Long__T
__cdecl
Minus__uMinus_var_u16_u64(void);
 /* declare_local */
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct Minus__Minus_var_u16_u64_Frame_t;typedef struct Minus__Minus_var_u16_u64_Frame_t Minus__Minus_var_u16_u64_Frame_t;
LONGINT
__cdecl
Minus__Minus_var_u16_u64(void);
 /* declare_local */
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct Minus__uMinus_param_u16_u64_Frame_t;typedef struct Minus__uMinus_param_u16_u64_Frame_t Minus__uMinus_param_u16_u64_Frame_t;
 /* declare_local */
 /* internal_declare_param */
 /* internal_declare_param */
Long__T
__cdecl
Minus__uMinus_param_u16_u64(
   /* Param_Type1 */ Minus__UINT16 a_L_415,
   /* Param_Type1 */ Minus__UINT64 b_L_416);
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct Minus__Minus_param_u16_u64_Frame_t;typedef struct Minus__Minus_param_u16_u64_Frame_t Minus__Minus_param_u16_u64_Frame_t;
 /* declare_local */
 /* internal_declare_param */
 /* internal_declare_param */
LONGINT
__cdecl
Minus__Minus_param_u16_u64(
   /* Param_Type1 */ Minus__UINT16 a_L_418,
   /* Param_Type1 */ Minus__UINT64 b_L_419);
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct Minus__uMinus_var_u16_i32_Frame_t;typedef struct Minus__uMinus_var_u16_i32_Frame_t Minus__uMinus_var_u16_i32_Frame_t;
Word__T
__cdecl
Minus__uMinus_var_u16_i32(void);
 /* declare_local */
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct Minus__Minus_var_u16_i32_Frame_t;typedef struct Minus__Minus_var_u16_i32_Frame_t Minus__Minus_var_u16_i32_Frame_t;
Minus__UINT16
__cdecl
Minus__Minus_var_u16_i32(void);
 /* declare_local */
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct Minus__uMinus_param_u16_i32_Frame_t;typedef struct Minus__uMinus_param_u16_i32_Frame_t Minus__uMinus_param_u16_i32_Frame_t;
 /* declare_local */
 /* internal_declare_param */
 /* internal_declare_param */
Word__T
__cdecl
Minus__uMinus_param_u16_i32(
   /* Param_Type1 */ Minus__UINT16 a_L_423,
   /* Param_Type1 */ Minus__INT32 b_L_424);
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct Minus__Minus_param_u16_i32_Frame_t;typedef struct Minus__Minus_param_u16_i32_Frame_t Minus__Minus_param_u16_i32_Frame_t;
 /* declare_local */
 /* internal_declare_param */
 /* internal_declare_param */
Minus__UINT16
__cdecl
Minus__Minus_param_u16_i32(
   /* Param_Type1 */ Minus__UINT16 a_L_426,
   /* Param_Type1 */ Minus__INT32 b_L_427);
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct Minus__uMinus_var_u16_LC_Frame_t;typedef struct Minus__uMinus_var_u16_LC_Frame_t Minus__uMinus_var_u16_LC_Frame_t;
Long__T
__cdecl
Minus__uMinus_var_u16_LC(void);
 /* declare_local */
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct Minus__Minus_var_u16_LC_Frame_t;typedef struct Minus__Minus_var_u16_LC_Frame_t Minus__Minus_var_u16_LC_Frame_t;
LONGINT
__cdecl
Minus__Minus_var_u16_LC(void);
 /* declare_local */
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct Minus__uMinus_param_u16_LC_Frame_t;typedef struct Minus__uMinus_param_u16_LC_Frame_t Minus__uMinus_param_u16_LC_Frame_t;
 /* declare_local */
 /* internal_declare_param */
 /* internal_declare_param */
Long__T
__cdecl
Minus__uMinus_param_u16_LC(
   /* Param_Type1 */ Minus__UINT16 a_L_431,
   /* Param_Type1 */ LONGCARD b_L_432);
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct Minus__Minus_param_u16_LC_Frame_t;typedef struct Minus__Minus_param_u16_LC_Frame_t Minus__Minus_param_u16_LC_Frame_t;
 /* declare_local */
 /* internal_declare_param */
 /* internal_declare_param */
LONGINT
__cdecl
Minus__Minus_param_u16_LC(
   /* Param_Type1 */ Minus__UINT16 a_L_434,
   /* Param_Type1 */ LONGCARD b_L_435);
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct Minus__uMinus_var_u16_u16_Frame_t;typedef struct Minus__uMinus_var_u16_u16_Frame_t Minus__uMinus_var_u16_u16_Frame_t;
Word__T
__cdecl
Minus__uMinus_var_u16_u16(void);
 /* declare_local */
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct Minus__Minus_var_u16_u16_Frame_t;typedef struct Minus__Minus_var_u16_u16_Frame_t Minus__Minus_var_u16_u16_Frame_t;
Minus__UINT16
__cdecl
Minus__Minus_var_u16_u16(void);
 /* declare_local */
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct Minus__uMinus_param_u16_u16_Frame_t;typedef struct Minus__uMinus_param_u16_u16_Frame_t Minus__uMinus_param_u16_u16_Frame_t;
 /* declare_local */
 /* internal_declare_param */
 /* internal_declare_param */
Word__T
__cdecl
Minus__uMinus_param_u16_u16(
   /* Param_Type1 */ Minus__UINT16 a_L_439,
   /* Param_Type1 */ Minus__UINT16 b_L_440);
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct Minus__Minus_param_u16_u16_Frame_t;typedef struct Minus__Minus_param_u16_u16_Frame_t Minus__Minus_param_u16_u16_Frame_t;
 /* declare_local */
 /* internal_declare_param */
 /* internal_declare_param */
Minus__UINT16
__cdecl
Minus__Minus_param_u16_u16(
   /* Param_Type1 */ Minus__UINT16 a_L_442,
   /* Param_Type1 */ Minus__UINT16 b_L_443);
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct Minus__uMinus_var_u16_I_Frame_t;typedef struct Minus__uMinus_var_u16_I_Frame_t Minus__uMinus_var_u16_I_Frame_t;
Word__T
__cdecl
Minus__uMinus_var_u16_I(void);
 /* declare_local */
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct Minus__Minus_var_u16_I_Frame_t;typedef struct Minus__Minus_var_u16_I_Frame_t Minus__Minus_var_u16_I_Frame_t;
Minus__UINT16
__cdecl
Minus__Minus_var_u16_I(void);
 /* declare_local */
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct Minus__uMinus_param_u16_I_Frame_t;typedef struct Minus__uMinus_param_u16_I_Frame_t Minus__uMinus_param_u16_I_Frame_t;
 /* declare_local */
 /* internal_declare_param */
 /* internal_declare_param */
Word__T
__cdecl
Minus__uMinus_param_u16_I(
   /* Param_Type1 */ Minus__UINT16 a_L_447,
   /* Param_Type1 */ INTEGER b_L_448);
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct Minus__Minus_param_u16_I_Frame_t;typedef struct Minus__Minus_param_u16_I_Frame_t Minus__Minus_param_u16_I_Frame_t;
 /* declare_local */
 /* internal_declare_param */
 /* internal_declare_param */
Minus__UINT16
__cdecl
Minus__Minus_param_u16_I(
   /* Param_Type1 */ Minus__UINT16 a_L_450,
   /* Param_Type1 */ INTEGER b_L_451);
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct Minus__uMinus_var_u16_i64_Frame_t;typedef struct Minus__uMinus_var_u16_i64_Frame_t Minus__uMinus_var_u16_i64_Frame_t;
Long__T
__cdecl
Minus__uMinus_var_u16_i64(void);
 /* declare_local */
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct Minus__Minus_var_u16_i64_Frame_t;typedef struct Minus__Minus_var_u16_i64_Frame_t Minus__Minus_var_u16_i64_Frame_t;
LONGINT
__cdecl
Minus__Minus_var_u16_i64(void);
 /* declare_local */
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct Minus__uMinus_param_u16_i64_Frame_t;typedef struct Minus__uMinus_param_u16_i64_Frame_t Minus__uMinus_param_u16_i64_Frame_t;
 /* declare_local */
 /* internal_declare_param */
 /* internal_declare_param */
Long__T
__cdecl
Minus__uMinus_param_u16_i64(
   /* Param_Type1 */ Minus__UINT16 a_L_455,
   /* Param_Type1 */ Minus__INT64 b_L_456);
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct Minus__Minus_param_u16_i64_Frame_t;typedef struct Minus__Minus_param_u16_i64_Frame_t Minus__Minus_param_u16_i64_Frame_t;
 /* declare_local */
 /* internal_declare_param */
 /* internal_declare_param */
LONGINT
__cdecl
Minus__Minus_param_u16_i64(
   /* Param_Type1 */ Minus__UINT16 a_L_458,
   /* Param_Type1 */ Minus__INT64 b_L_459);
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct Minus__uMinus_var_u16_i16_Frame_t;typedef struct Minus__uMinus_var_u16_i16_Frame_t Minus__uMinus_var_u16_i16_Frame_t;
Word__T
__cdecl
Minus__uMinus_var_u16_i16(void);
 /* declare_local */
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct Minus__Minus_var_u16_i16_Frame_t;typedef struct Minus__Minus_var_u16_i16_Frame_t Minus__Minus_var_u16_i16_Frame_t;
Minus__UINT16
__cdecl
Minus__Minus_var_u16_i16(void);
 /* declare_local */
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct Minus__uMinus_param_u16_i16_Frame_t;typedef struct Minus__uMinus_param_u16_i16_Frame_t Minus__uMinus_param_u16_i16_Frame_t;
 /* declare_local */
 /* internal_declare_param */
 /* internal_declare_param */
Word__T
__cdecl
Minus__uMinus_param_u16_i16(
   /* Param_Type1 */ Minus__UINT16 a_L_463,
   /* Param_Type1 */ Minus__INT16 b_L_464);
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct Minus__Minus_param_u16_i16_Frame_t;typedef struct Minus__Minus_param_u16_i16_Frame_t Minus__Minus_param_u16_i16_Frame_t;
 /* declare_local */
 /* internal_declare_param */
 /* internal_declare_param */
Minus__UINT16
__cdecl
Minus__Minus_param_u16_i16(
   /* Param_Type1 */ Minus__UINT16 a_L_466,
   /* Param_Type1 */ Minus__INT16 b_L_467);
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct Minus__uMinus_var_u16_C_Frame_t;typedef struct Minus__uMinus_var_u16_C_Frame_t Minus__uMinus_var_u16_C_Frame_t;
Word__T
__cdecl
Minus__uMinus_var_u16_C(void);
 /* declare_local */
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct Minus__Minus_var_u16_C_Frame_t;typedef struct Minus__Minus_var_u16_C_Frame_t Minus__Minus_var_u16_C_Frame_t;
Minus__UINT16
__cdecl
Minus__Minus_var_u16_C(void);
 /* declare_local */
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct Minus__uMinus_param_u16_C_Frame_t;typedef struct Minus__uMinus_param_u16_C_Frame_t Minus__uMinus_param_u16_C_Frame_t;
 /* declare_local */
 /* internal_declare_param */
 /* internal_declare_param */
Word__T
__cdecl
Minus__uMinus_param_u16_C(
   /* Param_Type1 */ Minus__UINT16 a_L_471,
   /* Param_Type1 */ CARDINAL b_L_472);
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct Minus__Minus_param_u16_C_Frame_t;typedef struct Minus__Minus_param_u16_C_Frame_t Minus__Minus_param_u16_C_Frame_t;
 /* declare_local */
 /* internal_declare_param */
 /* internal_declare_param */
Minus__UINT16
__cdecl
Minus__Minus_param_u16_C(
   /* Param_Type1 */ Minus__UINT16 a_L_474,
   /* Param_Type1 */ CARDINAL b_L_475);
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct Minus__uMinus_var_u16_u32_Frame_t;typedef struct Minus__uMinus_var_u16_u32_Frame_t Minus__uMinus_var_u16_u32_Frame_t;
Word__T
__cdecl
Minus__uMinus_var_u16_u32(void);
 /* declare_local */
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct Minus__Minus_var_u16_u32_Frame_t;typedef struct Minus__Minus_var_u16_u32_Frame_t Minus__Minus_var_u16_u32_Frame_t;
Minus__UINT16
__cdecl
Minus__Minus_var_u16_u32(void);
 /* declare_local */
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct Minus__uMinus_param_u16_u32_Frame_t;typedef struct Minus__uMinus_param_u16_u32_Frame_t Minus__uMinus_param_u16_u32_Frame_t;
 /* declare_local */
 /* internal_declare_param */
 /* internal_declare_param */
Word__T
__cdecl
Minus__uMinus_param_u16_u32(
   /* Param_Type1 */ Minus__UINT16 a_L_479,
   /* Param_Type1 */ Minus__UINT32 b_L_480);
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct Minus__Minus_param_u16_u32_Frame_t;typedef struct Minus__Minus_param_u16_u32_Frame_t Minus__Minus_param_u16_u32_Frame_t;
 /* declare_local */
 /* internal_declare_param */
 /* internal_declare_param */
Minus__UINT16
__cdecl
Minus__Minus_param_u16_u32(
   /* Param_Type1 */ Minus__UINT16 a_L_482,
   /* Param_Type1 */ Minus__UINT32 b_L_483);
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct Minus__uMinus_var_u16_u8_Frame_t;typedef struct Minus__uMinus_var_u16_u8_Frame_t Minus__uMinus_var_u16_u8_Frame_t;
Word__T
__cdecl
Minus__uMinus_var_u16_u8(void);
 /* declare_local */
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct Minus__Minus_var_u16_u8_Frame_t;typedef struct Minus__Minus_var_u16_u8_Frame_t Minus__Minus_var_u16_u8_Frame_t;
Minus__UINT16
__cdecl
Minus__Minus_var_u16_u8(void);
 /* declare_local */
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct Minus__uMinus_param_u16_u8_Frame_t;typedef struct Minus__uMinus_param_u16_u8_Frame_t Minus__uMinus_param_u16_u8_Frame_t;
 /* declare_local */
 /* internal_declare_param */
 /* internal_declare_param */
Word__T
__cdecl
Minus__uMinus_param_u16_u8(
   /* Param_Type1 */ Minus__UINT16 a_L_487,
   /* Param_Type1 */ Minus__UINT8 b_L_488);
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct Minus__Minus_param_u16_u8_Frame_t;typedef struct Minus__Minus_param_u16_u8_Frame_t Minus__Minus_param_u16_u8_Frame_t;
 /* declare_local */
 /* internal_declare_param */
 /* internal_declare_param */
Minus__UINT16
__cdecl
Minus__Minus_param_u16_u8(
   /* Param_Type1 */ Minus__UINT16 a_L_490,
   /* Param_Type1 */ Minus__UINT8 b_L_491);
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct Minus__uMinus_var_u16_L_Frame_t;typedef struct Minus__uMinus_var_u16_L_Frame_t Minus__uMinus_var_u16_L_Frame_t;
Long__T
__cdecl
Minus__uMinus_var_u16_L(void);
 /* declare_local */
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct Minus__Minus_var_u16_L_Frame_t;typedef struct Minus__Minus_var_u16_L_Frame_t Minus__Minus_var_u16_L_Frame_t;
LONGINT
__cdecl
Minus__Minus_var_u16_L(void);
 /* declare_local */
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct Minus__uMinus_param_u16_L_Frame_t;typedef struct Minus__uMinus_param_u16_L_Frame_t Minus__uMinus_param_u16_L_Frame_t;
 /* declare_local */
 /* internal_declare_param */
 /* internal_declare_param */
Long__T
__cdecl
Minus__uMinus_param_u16_L(
   /* Param_Type1 */ Minus__UINT16 a_L_495,
   /* Param_Type1 */ LONGINT b_L_496);
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct Minus__Minus_param_u16_L_Frame_t;typedef struct Minus__Minus_param_u16_L_Frame_t Minus__Minus_param_u16_L_Frame_t;
 /* declare_local */
 /* internal_declare_param */
 /* internal_declare_param */
LONGINT
__cdecl
Minus__Minus_param_u16_L(
   /* Param_Type1 */ Minus__UINT16 a_L_498,
   /* Param_Type1 */ LONGINT b_L_499);
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct Minus__uMinus_var_I_i8_Frame_t;typedef struct Minus__uMinus_var_I_i8_Frame_t Minus__uMinus_var_I_i8_Frame_t;
Word__T
__cdecl
Minus__uMinus_var_I_i8(void);
 /* declare_local */
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct Minus__Minus_var_I_i8_Frame_t;typedef struct Minus__Minus_var_I_i8_Frame_t Minus__Minus_var_I_i8_Frame_t;
INTEGER
__cdecl
Minus__Minus_var_I_i8(void);
 /* declare_local */
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct Minus__uMinus_param_I_i8_Frame_t;typedef struct Minus__uMinus_param_I_i8_Frame_t Minus__uMinus_param_I_i8_Frame_t;
 /* declare_local */
 /* internal_declare_param */
 /* internal_declare_param */
Word__T
__cdecl
Minus__uMinus_param_I_i8(
   /* Param_Type1 */ INTEGER a_L_503,
   /* Param_Type1 */ Minus__INT8 b_L_504);
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct Minus__Minus_param_I_i8_Frame_t;typedef struct Minus__Minus_param_I_i8_Frame_t Minus__Minus_param_I_i8_Frame_t;
 /* declare_local */
 /* internal_declare_param */
 /* internal_declare_param */
INTEGER
__cdecl
Minus__Minus_param_I_i8(
   /* Param_Type1 */ INTEGER a_L_506,
   /* Param_Type1 */ Minus__INT8 b_L_507);
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct Minus__uMinus_var_I_u64_Frame_t;typedef struct Minus__uMinus_var_I_u64_Frame_t Minus__uMinus_var_I_u64_Frame_t;
Long__T
__cdecl
Minus__uMinus_var_I_u64(void);
 /* declare_local */
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct Minus__Minus_var_I_u64_Frame_t;typedef struct Minus__Minus_var_I_u64_Frame_t Minus__Minus_var_I_u64_Frame_t;
LONGINT
__cdecl
Minus__Minus_var_I_u64(void);
 /* declare_local */
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct Minus__uMinus_param_I_u64_Frame_t;typedef struct Minus__uMinus_param_I_u64_Frame_t Minus__uMinus_param_I_u64_Frame_t;
 /* declare_local */
 /* internal_declare_param */
 /* internal_declare_param */
Long__T
__cdecl
Minus__uMinus_param_I_u64(
   /* Param_Type1 */ INTEGER a_L_511,
   /* Param_Type1 */ Minus__UINT64 b_L_512);
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct Minus__Minus_param_I_u64_Frame_t;typedef struct Minus__Minus_param_I_u64_Frame_t Minus__Minus_param_I_u64_Frame_t;
 /* declare_local */
 /* internal_declare_param */
 /* internal_declare_param */
LONGINT
__cdecl
Minus__Minus_param_I_u64(
   /* Param_Type1 */ INTEGER a_L_514,
   /* Param_Type1 */ Minus__UINT64 b_L_515);
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct Minus__uMinus_var_I_i32_Frame_t;typedef struct Minus__uMinus_var_I_i32_Frame_t Minus__uMinus_var_I_i32_Frame_t;
Word__T
__cdecl
Minus__uMinus_var_I_i32(void);
 /* declare_local */
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct Minus__Minus_var_I_i32_Frame_t;typedef struct Minus__Minus_var_I_i32_Frame_t Minus__Minus_var_I_i32_Frame_t;
INTEGER
__cdecl
Minus__Minus_var_I_i32(void);
 /* declare_local */
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct Minus__uMinus_param_I_i32_Frame_t;typedef struct Minus__uMinus_param_I_i32_Frame_t Minus__uMinus_param_I_i32_Frame_t;
 /* declare_local */
 /* internal_declare_param */
 /* internal_declare_param */
Word__T
__cdecl
Minus__uMinus_param_I_i32(
   /* Param_Type1 */ INTEGER a_L_519,
   /* Param_Type1 */ Minus__INT32 b_L_520);
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct Minus__Minus_param_I_i32_Frame_t;typedef struct Minus__Minus_param_I_i32_Frame_t Minus__Minus_param_I_i32_Frame_t;
 /* declare_local */
 /* internal_declare_param */
 /* internal_declare_param */
INTEGER
__cdecl
Minus__Minus_param_I_i32(
   /* Param_Type1 */ INTEGER a_L_522,
   /* Param_Type1 */ Minus__INT32 b_L_523);
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct Minus__uMinus_var_I_LC_Frame_t;typedef struct Minus__uMinus_var_I_LC_Frame_t Minus__uMinus_var_I_LC_Frame_t;
Long__T
__cdecl
Minus__uMinus_var_I_LC(void);
 /* declare_local */
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct Minus__Minus_var_I_LC_Frame_t;typedef struct Minus__Minus_var_I_LC_Frame_t Minus__Minus_var_I_LC_Frame_t;
LONGINT
__cdecl
Minus__Minus_var_I_LC(void);
 /* declare_local */
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct Minus__uMinus_param_I_LC_Frame_t;typedef struct Minus__uMinus_param_I_LC_Frame_t Minus__uMinus_param_I_LC_Frame_t;
 /* declare_local */
 /* internal_declare_param */
 /* internal_declare_param */
Long__T
__cdecl
Minus__uMinus_param_I_LC(
   /* Param_Type1 */ INTEGER a_L_527,
   /* Param_Type1 */ LONGCARD b_L_528);
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct Minus__Minus_param_I_LC_Frame_t;typedef struct Minus__Minus_param_I_LC_Frame_t Minus__Minus_param_I_LC_Frame_t;
 /* declare_local */
 /* internal_declare_param */
 /* internal_declare_param */
LONGINT
__cdecl
Minus__Minus_param_I_LC(
   /* Param_Type1 */ INTEGER a_L_530,
   /* Param_Type1 */ LONGCARD b_L_531);
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct Minus__uMinus_var_I_u16_Frame_t;typedef struct Minus__uMinus_var_I_u16_Frame_t Minus__uMinus_var_I_u16_Frame_t;
Word__T
__cdecl
Minus__uMinus_var_I_u16(void);
 /* declare_local */
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct Minus__Minus_var_I_u16_Frame_t;typedef struct Minus__Minus_var_I_u16_Frame_t Minus__Minus_var_I_u16_Frame_t;
INTEGER
__cdecl
Minus__Minus_var_I_u16(void);
 /* declare_local */
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct Minus__uMinus_param_I_u16_Frame_t;typedef struct Minus__uMinus_param_I_u16_Frame_t Minus__uMinus_param_I_u16_Frame_t;
 /* declare_local */
 /* internal_declare_param */
 /* internal_declare_param */
Word__T
__cdecl
Minus__uMinus_param_I_u16(
   /* Param_Type1 */ INTEGER a_L_535,
   /* Param_Type1 */ Minus__UINT16 b_L_536);
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct Minus__Minus_param_I_u16_Frame_t;typedef struct Minus__Minus_param_I_u16_Frame_t Minus__Minus_param_I_u16_Frame_t;
 /* declare_local */
 /* internal_declare_param */
 /* internal_declare_param */
INTEGER
__cdecl
Minus__Minus_param_I_u16(
   /* Param_Type1 */ INTEGER a_L_538,
   /* Param_Type1 */ Minus__UINT16 b_L_539);
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct Minus__uMinus_var_I_I_Frame_t;typedef struct Minus__uMinus_var_I_I_Frame_t Minus__uMinus_var_I_I_Frame_t;
Word__T
__cdecl
Minus__uMinus_var_I_I(void);
 /* declare_local */
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct Minus__Minus_var_I_I_Frame_t;typedef struct Minus__Minus_var_I_I_Frame_t Minus__Minus_var_I_I_Frame_t;
INTEGER
__cdecl
Minus__Minus_var_I_I(void);
 /* declare_local */
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct Minus__uMinus_param_I_I_Frame_t;typedef struct Minus__uMinus_param_I_I_Frame_t Minus__uMinus_param_I_I_Frame_t;
 /* declare_local */
 /* internal_declare_param */
 /* internal_declare_param */
Word__T
__cdecl
Minus__uMinus_param_I_I(
   /* Param_Type1 */ INTEGER a_L_543,
   /* Param_Type1 */ INTEGER b_L_544);
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct Minus__Minus_param_I_I_Frame_t;typedef struct Minus__Minus_param_I_I_Frame_t Minus__Minus_param_I_I_Frame_t;
 /* declare_local */
 /* internal_declare_param */
 /* internal_declare_param */
INTEGER
__cdecl
Minus__Minus_param_I_I(
   /* Param_Type1 */ INTEGER a_L_546,
   /* Param_Type1 */ INTEGER b_L_547);
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct Minus__uMinus_var_I_i64_Frame_t;typedef struct Minus__uMinus_var_I_i64_Frame_t Minus__uMinus_var_I_i64_Frame_t;
Long__T
__cdecl
Minus__uMinus_var_I_i64(void);
 /* declare_local */
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct Minus__Minus_var_I_i64_Frame_t;typedef struct Minus__Minus_var_I_i64_Frame_t Minus__Minus_var_I_i64_Frame_t;
LONGINT
__cdecl
Minus__Minus_var_I_i64(void);
 /* declare_local */
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct Minus__uMinus_param_I_i64_Frame_t;typedef struct Minus__uMinus_param_I_i64_Frame_t Minus__uMinus_param_I_i64_Frame_t;
 /* declare_local */
 /* internal_declare_param */
 /* internal_declare_param */
Long__T
__cdecl
Minus__uMinus_param_I_i64(
   /* Param_Type1 */ INTEGER a_L_551,
   /* Param_Type1 */ Minus__INT64 b_L_552);
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct Minus__Minus_param_I_i64_Frame_t;typedef struct Minus__Minus_param_I_i64_Frame_t Minus__Minus_param_I_i64_Frame_t;
 /* declare_local */
 /* internal_declare_param */
 /* internal_declare_param */
LONGINT
__cdecl
Minus__Minus_param_I_i64(
   /* Param_Type1 */ INTEGER a_L_554,
   /* Param_Type1 */ Minus__INT64 b_L_555);
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct Minus__uMinus_var_I_i16_Frame_t;typedef struct Minus__uMinus_var_I_i16_Frame_t Minus__uMinus_var_I_i16_Frame_t;
Word__T
__cdecl
Minus__uMinus_var_I_i16(void);
 /* declare_local */
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct Minus__Minus_var_I_i16_Frame_t;typedef struct Minus__Minus_var_I_i16_Frame_t Minus__Minus_var_I_i16_Frame_t;
INTEGER
__cdecl
Minus__Minus_var_I_i16(void);
 /* declare_local */
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct Minus__uMinus_param_I_i16_Frame_t;typedef struct Minus__uMinus_param_I_i16_Frame_t Minus__uMinus_param_I_i16_Frame_t;
 /* declare_local */
 /* internal_declare_param */
 /* internal_declare_param */
Word__T
__cdecl
Minus__uMinus_param_I_i16(
   /* Param_Type1 */ INTEGER a_L_559,
   /* Param_Type1 */ Minus__INT16 b_L_560);
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct Minus__Minus_param_I_i16_Frame_t;typedef struct Minus__Minus_param_I_i16_Frame_t Minus__Minus_param_I_i16_Frame_t;
 /* declare_local */
 /* internal_declare_param */
 /* internal_declare_param */
INTEGER
__cdecl
Minus__Minus_param_I_i16(
   /* Param_Type1 */ INTEGER a_L_562,
   /* Param_Type1 */ Minus__INT16 b_L_563);
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct Minus__uMinus_var_I_C_Frame_t;typedef struct Minus__uMinus_var_I_C_Frame_t Minus__uMinus_var_I_C_Frame_t;
Word__T
__cdecl
Minus__uMinus_var_I_C(void);
 /* declare_local */
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct Minus__Minus_var_I_C_Frame_t;typedef struct Minus__Minus_var_I_C_Frame_t Minus__Minus_var_I_C_Frame_t;
INTEGER
__cdecl
Minus__Minus_var_I_C(void);
 /* declare_local */
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct Minus__uMinus_param_I_C_Frame_t;typedef struct Minus__uMinus_param_I_C_Frame_t Minus__uMinus_param_I_C_Frame_t;
 /* declare_local */
 /* internal_declare_param */
 /* internal_declare_param */
Word__T
__cdecl
Minus__uMinus_param_I_C(
   /* Param_Type1 */ INTEGER a_L_567,
   /* Param_Type1 */ CARDINAL b_L_568);
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct Minus__Minus_param_I_C_Frame_t;typedef struct Minus__Minus_param_I_C_Frame_t Minus__Minus_param_I_C_Frame_t;
 /* declare_local */
 /* internal_declare_param */
 /* internal_declare_param */
INTEGER
__cdecl
Minus__Minus_param_I_C(
   /* Param_Type1 */ INTEGER a_L_570,
   /* Param_Type1 */ CARDINAL b_L_571);
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct Minus__uMinus_var_I_u32_Frame_t;typedef struct Minus__uMinus_var_I_u32_Frame_t Minus__uMinus_var_I_u32_Frame_t;
Word__T
__cdecl
Minus__uMinus_var_I_u32(void);
 /* declare_local */
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct Minus__Minus_var_I_u32_Frame_t;typedef struct Minus__Minus_var_I_u32_Frame_t Minus__Minus_var_I_u32_Frame_t;
INTEGER
__cdecl
Minus__Minus_var_I_u32(void);
 /* declare_local */
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct Minus__uMinus_param_I_u32_Frame_t;typedef struct Minus__uMinus_param_I_u32_Frame_t Minus__uMinus_param_I_u32_Frame_t;
 /* declare_local */
 /* internal_declare_param */
 /* internal_declare_param */
Word__T
__cdecl
Minus__uMinus_param_I_u32(
   /* Param_Type1 */ INTEGER a_L_575,
   /* Param_Type1 */ Minus__UINT32 b_L_576);
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct Minus__Minus_param_I_u32_Frame_t;typedef struct Minus__Minus_param_I_u32_Frame_t Minus__Minus_param_I_u32_Frame_t;
 /* declare_local */
 /* internal_declare_param */
 /* internal_declare_param */
INTEGER
__cdecl
Minus__Minus_param_I_u32(
   /* Param_Type1 */ INTEGER a_L_578,
   /* Param_Type1 */ Minus__UINT32 b_L_579);
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct Minus__uMinus_var_I_u8_Frame_t;typedef struct Minus__uMinus_var_I_u8_Frame_t Minus__uMinus_var_I_u8_Frame_t;
Word__T
__cdecl
Minus__uMinus_var_I_u8(void);
 /* declare_local */
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct Minus__Minus_var_I_u8_Frame_t;typedef struct Minus__Minus_var_I_u8_Frame_t Minus__Minus_var_I_u8_Frame_t;
INTEGER
__cdecl
Minus__Minus_var_I_u8(void);
 /* declare_local */
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct Minus__uMinus_param_I_u8_Frame_t;typedef struct Minus__uMinus_param_I_u8_Frame_t Minus__uMinus_param_I_u8_Frame_t;
 /* declare_local */
 /* internal_declare_param */
 /* internal_declare_param */
Word__T
__cdecl
Minus__uMinus_param_I_u8(
   /* Param_Type1 */ INTEGER a_L_583,
   /* Param_Type1 */ Minus__UINT8 b_L_584);
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct Minus__Minus_param_I_u8_Frame_t;typedef struct Minus__Minus_param_I_u8_Frame_t Minus__Minus_param_I_u8_Frame_t;
 /* declare_local */
 /* internal_declare_param */
 /* internal_declare_param */
INTEGER
__cdecl
Minus__Minus_param_I_u8(
   /* Param_Type1 */ INTEGER a_L_586,
   /* Param_Type1 */ Minus__UINT8 b_L_587);
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct Minus__uMinus_var_I_L_Frame_t;typedef struct Minus__uMinus_var_I_L_Frame_t Minus__uMinus_var_I_L_Frame_t;
Long__T
__cdecl
Minus__uMinus_var_I_L(void);
 /* declare_local */
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct Minus__Minus_var_I_L_Frame_t;typedef struct Minus__Minus_var_I_L_Frame_t Minus__Minus_var_I_L_Frame_t;
LONGINT
__cdecl
Minus__Minus_var_I_L(void);
 /* declare_local */
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct Minus__uMinus_param_I_L_Frame_t;typedef struct Minus__uMinus_param_I_L_Frame_t Minus__uMinus_param_I_L_Frame_t;
 /* declare_local */
 /* internal_declare_param */
 /* internal_declare_param */
Long__T
__cdecl
Minus__uMinus_param_I_L(
   /* Param_Type1 */ INTEGER a_L_591,
   /* Param_Type1 */ LONGINT b_L_592);
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct Minus__Minus_param_I_L_Frame_t;typedef struct Minus__Minus_param_I_L_Frame_t Minus__Minus_param_I_L_Frame_t;
 /* declare_local */
 /* internal_declare_param */
 /* internal_declare_param */
LONGINT
__cdecl
Minus__Minus_param_I_L(
   /* Param_Type1 */ INTEGER a_L_594,
   /* Param_Type1 */ LONGINT b_L_595);
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct Minus__uMinus_var_i64_i8_Frame_t;typedef struct Minus__uMinus_var_i64_i8_Frame_t Minus__uMinus_var_i64_i8_Frame_t;
Long__T
__cdecl
Minus__uMinus_var_i64_i8(void);
 /* declare_local */
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct Minus__Minus_var_i64_i8_Frame_t;typedef struct Minus__Minus_var_i64_i8_Frame_t Minus__Minus_var_i64_i8_Frame_t;
LONGINT
__cdecl
Minus__Minus_var_i64_i8(void);
 /* declare_local */
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct Minus__uMinus_param_i64_i8_Frame_t;typedef struct Minus__uMinus_param_i64_i8_Frame_t Minus__uMinus_param_i64_i8_Frame_t;
 /* declare_local */
 /* internal_declare_param */
 /* internal_declare_param */
Long__T
__cdecl
Minus__uMinus_param_i64_i8(
   /* Param_Type1 */ Minus__INT64 a_L_599,
   /* Param_Type1 */ Minus__INT8 b_L_600);
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct Minus__Minus_param_i64_i8_Frame_t;typedef struct Minus__Minus_param_i64_i8_Frame_t Minus__Minus_param_i64_i8_Frame_t;
 /* declare_local */
 /* internal_declare_param */
 /* internal_declare_param */
LONGINT
__cdecl
Minus__Minus_param_i64_i8(
   /* Param_Type1 */ Minus__INT64 a_L_602,
   /* Param_Type1 */ Minus__INT8 b_L_603);
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct Minus__uMinus_var_i64_u64_Frame_t;typedef struct Minus__uMinus_var_i64_u64_Frame_t Minus__uMinus_var_i64_u64_Frame_t;
Long__T
__cdecl
Minus__uMinus_var_i64_u64(void);
 /* declare_local */
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct Minus__Minus_var_i64_u64_Frame_t;typedef struct Minus__Minus_var_i64_u64_Frame_t Minus__Minus_var_i64_u64_Frame_t;
LONGINT
__cdecl
Minus__Minus_var_i64_u64(void);
 /* declare_local */
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct Minus__uMinus_param_i64_u64_Frame_t;typedef struct Minus__uMinus_param_i64_u64_Frame_t Minus__uMinus_param_i64_u64_Frame_t;
 /* declare_local */
 /* internal_declare_param */
 /* internal_declare_param */
Long__T
__cdecl
Minus__uMinus_param_i64_u64(
   /* Param_Type1 */ Minus__INT64 a_L_607,
   /* Param_Type1 */ Minus__UINT64 b_L_608);
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct Minus__Minus_param_i64_u64_Frame_t;typedef struct Minus__Minus_param_i64_u64_Frame_t Minus__Minus_param_i64_u64_Frame_t;
 /* declare_local */
 /* internal_declare_param */
 /* internal_declare_param */
LONGINT
__cdecl
Minus__Minus_param_i64_u64(
   /* Param_Type1 */ Minus__INT64 a_L_610,
   /* Param_Type1 */ Minus__UINT64 b_L_611);
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct Minus__uMinus_var_i64_i32_Frame_t;typedef struct Minus__uMinus_var_i64_i32_Frame_t Minus__uMinus_var_i64_i32_Frame_t;
Long__T
__cdecl
Minus__uMinus_var_i64_i32(void);
 /* declare_local */
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct Minus__Minus_var_i64_i32_Frame_t;typedef struct Minus__Minus_var_i64_i32_Frame_t Minus__Minus_var_i64_i32_Frame_t;
LONGINT
__cdecl
Minus__Minus_var_i64_i32(void);
 /* declare_local */
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct Minus__uMinus_param_i64_i32_Frame_t;typedef struct Minus__uMinus_param_i64_i32_Frame_t Minus__uMinus_param_i64_i32_Frame_t;
 /* declare_local */
 /* internal_declare_param */
 /* internal_declare_param */
Long__T
__cdecl
Minus__uMinus_param_i64_i32(
   /* Param_Type1 */ Minus__INT64 a_L_615,
   /* Param_Type1 */ Minus__INT32 b_L_616);
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct Minus__Minus_param_i64_i32_Frame_t;typedef struct Minus__Minus_param_i64_i32_Frame_t Minus__Minus_param_i64_i32_Frame_t;
 /* declare_local */
 /* internal_declare_param */
 /* internal_declare_param */
LONGINT
__cdecl
Minus__Minus_param_i64_i32(
   /* Param_Type1 */ Minus__INT64 a_L_618,
   /* Param_Type1 */ Minus__INT32 b_L_619);
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct Minus__uMinus_var_i64_LC_Frame_t;typedef struct Minus__uMinus_var_i64_LC_Frame_t Minus__uMinus_var_i64_LC_Frame_t;
Long__T
__cdecl
Minus__uMinus_var_i64_LC(void);
 /* declare_local */
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct Minus__Minus_var_i64_LC_Frame_t;typedef struct Minus__Minus_var_i64_LC_Frame_t Minus__Minus_var_i64_LC_Frame_t;
LONGINT
__cdecl
Minus__Minus_var_i64_LC(void);
 /* declare_local */
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct Minus__uMinus_param_i64_LC_Frame_t;typedef struct Minus__uMinus_param_i64_LC_Frame_t Minus__uMinus_param_i64_LC_Frame_t;
 /* declare_local */
 /* internal_declare_param */
 /* internal_declare_param */
Long__T
__cdecl
Minus__uMinus_param_i64_LC(
   /* Param_Type1 */ Minus__INT64 a_L_623,
   /* Param_Type1 */ LONGCARD b_L_624);
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct Minus__Minus_param_i64_LC_Frame_t;typedef struct Minus__Minus_param_i64_LC_Frame_t Minus__Minus_param_i64_LC_Frame_t;
 /* declare_local */
 /* internal_declare_param */
 /* internal_declare_param */
LONGINT
__cdecl
Minus__Minus_param_i64_LC(
   /* Param_Type1 */ Minus__INT64 a_L_626,
   /* Param_Type1 */ LONGCARD b_L_627);
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct Minus__uMinus_var_i64_u16_Frame_t;typedef struct Minus__uMinus_var_i64_u16_Frame_t Minus__uMinus_var_i64_u16_Frame_t;
Long__T
__cdecl
Minus__uMinus_var_i64_u16(void);
 /* declare_local */
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct Minus__Minus_var_i64_u16_Frame_t;typedef struct Minus__Minus_var_i64_u16_Frame_t Minus__Minus_var_i64_u16_Frame_t;
LONGINT
__cdecl
Minus__Minus_var_i64_u16(void);
 /* declare_local */
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct Minus__uMinus_param_i64_u16_Frame_t;typedef struct Minus__uMinus_param_i64_u16_Frame_t Minus__uMinus_param_i64_u16_Frame_t;
 /* declare_local */
 /* internal_declare_param */
 /* internal_declare_param */
Long__T
__cdecl
Minus__uMinus_param_i64_u16(
   /* Param_Type1 */ Minus__INT64 a_L_631,
   /* Param_Type1 */ Minus__UINT16 b_L_632);
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct Minus__Minus_param_i64_u16_Frame_t;typedef struct Minus__Minus_param_i64_u16_Frame_t Minus__Minus_param_i64_u16_Frame_t;
 /* declare_local */
 /* internal_declare_param */
 /* internal_declare_param */
LONGINT
__cdecl
Minus__Minus_param_i64_u16(
   /* Param_Type1 */ Minus__INT64 a_L_634,
   /* Param_Type1 */ Minus__UINT16 b_L_635);
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct Minus__uMinus_var_i64_I_Frame_t;typedef struct Minus__uMinus_var_i64_I_Frame_t Minus__uMinus_var_i64_I_Frame_t;
Long__T
__cdecl
Minus__uMinus_var_i64_I(void);
 /* declare_local */
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct Minus__Minus_var_i64_I_Frame_t;typedef struct Minus__Minus_var_i64_I_Frame_t Minus__Minus_var_i64_I_Frame_t;
LONGINT
__cdecl
Minus__Minus_var_i64_I(void);
 /* declare_local */
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct Minus__uMinus_param_i64_I_Frame_t;typedef struct Minus__uMinus_param_i64_I_Frame_t Minus__uMinus_param_i64_I_Frame_t;
 /* declare_local */
 /* internal_declare_param */
 /* internal_declare_param */
Long__T
__cdecl
Minus__uMinus_param_i64_I(
   /* Param_Type1 */ Minus__INT64 a_L_639,
   /* Param_Type1 */ INTEGER b_L_640);
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct Minus__Minus_param_i64_I_Frame_t;typedef struct Minus__Minus_param_i64_I_Frame_t Minus__Minus_param_i64_I_Frame_t;
 /* declare_local */
 /* internal_declare_param */
 /* internal_declare_param */
LONGINT
__cdecl
Minus__Minus_param_i64_I(
   /* Param_Type1 */ Minus__INT64 a_L_642,
   /* Param_Type1 */ INTEGER b_L_643);
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct Minus__uMinus_var_i64_i64_Frame_t;typedef struct Minus__uMinus_var_i64_i64_Frame_t Minus__uMinus_var_i64_i64_Frame_t;
Long__T
__cdecl
Minus__uMinus_var_i64_i64(void);
 /* declare_local */
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct Minus__Minus_var_i64_i64_Frame_t;typedef struct Minus__Minus_var_i64_i64_Frame_t Minus__Minus_var_i64_i64_Frame_t;
LONGINT
__cdecl
Minus__Minus_var_i64_i64(void);
 /* declare_local */
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct Minus__uMinus_param_i64_i64_Frame_t;typedef struct Minus__uMinus_param_i64_i64_Frame_t Minus__uMinus_param_i64_i64_Frame_t;
 /* declare_local */
 /* internal_declare_param */
 /* internal_declare_param */
Long__T
__cdecl
Minus__uMinus_param_i64_i64(
   /* Param_Type1 */ Minus__INT64 a_L_647,
   /* Param_Type1 */ Minus__INT64 b_L_648);
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct Minus__Minus_param_i64_i64_Frame_t;typedef struct Minus__Minus_param_i64_i64_Frame_t Minus__Minus_param_i64_i64_Frame_t;
 /* declare_local */
 /* internal_declare_param */
 /* internal_declare_param */
LONGINT
__cdecl
Minus__Minus_param_i64_i64(
   /* Param_Type1 */ Minus__INT64 a_L_650,
   /* Param_Type1 */ Minus__INT64 b_L_651);
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct Minus__uMinus_var_i64_i16_Frame_t;typedef struct Minus__uMinus_var_i64_i16_Frame_t Minus__uMinus_var_i64_i16_Frame_t;
Long__T
__cdecl
Minus__uMinus_var_i64_i16(void);
 /* declare_local */
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct Minus__Minus_var_i64_i16_Frame_t;typedef struct Minus__Minus_var_i64_i16_Frame_t Minus__Minus_var_i64_i16_Frame_t;
LONGINT
__cdecl
Minus__Minus_var_i64_i16(void);
 /* declare_local */
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct Minus__uMinus_param_i64_i16_Frame_t;typedef struct Minus__uMinus_param_i64_i16_Frame_t Minus__uMinus_param_i64_i16_Frame_t;
 /* declare_local */
 /* internal_declare_param */
 /* internal_declare_param */
Long__T
__cdecl
Minus__uMinus_param_i64_i16(
   /* Param_Type1 */ Minus__INT64 a_L_655,
   /* Param_Type1 */ Minus__INT16 b_L_656);
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct Minus__Minus_param_i64_i16_Frame_t;typedef struct Minus__Minus_param_i64_i16_Frame_t Minus__Minus_param_i64_i16_Frame_t;
 /* declare_local */
 /* internal_declare_param */
 /* internal_declare_param */
LONGINT
__cdecl
Minus__Minus_param_i64_i16(
   /* Param_Type1 */ Minus__INT64 a_L_658,
   /* Param_Type1 */ Minus__INT16 b_L_659);
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct Minus__uMinus_var_i64_C_Frame_t;typedef struct Minus__uMinus_var_i64_C_Frame_t Minus__uMinus_var_i64_C_Frame_t;
Long__T
__cdecl
Minus__uMinus_var_i64_C(void);
 /* declare_local */
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct Minus__Minus_var_i64_C_Frame_t;typedef struct Minus__Minus_var_i64_C_Frame_t Minus__Minus_var_i64_C_Frame_t;
LONGINT
__cdecl
Minus__Minus_var_i64_C(void);
 /* declare_local */
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct Minus__uMinus_param_i64_C_Frame_t;typedef struct Minus__uMinus_param_i64_C_Frame_t Minus__uMinus_param_i64_C_Frame_t;
 /* declare_local */
 /* internal_declare_param */
 /* internal_declare_param */
Long__T
__cdecl
Minus__uMinus_param_i64_C(
   /* Param_Type1 */ Minus__INT64 a_L_663,
   /* Param_Type1 */ CARDINAL b_L_664);
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct Minus__Minus_param_i64_C_Frame_t;typedef struct Minus__Minus_param_i64_C_Frame_t Minus__Minus_param_i64_C_Frame_t;
 /* declare_local */
 /* internal_declare_param */
 /* internal_declare_param */
LONGINT
__cdecl
Minus__Minus_param_i64_C(
   /* Param_Type1 */ Minus__INT64 a_L_666,
   /* Param_Type1 */ CARDINAL b_L_667);
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct Minus__uMinus_var_i64_u32_Frame_t;typedef struct Minus__uMinus_var_i64_u32_Frame_t Minus__uMinus_var_i64_u32_Frame_t;
Long__T
__cdecl
Minus__uMinus_var_i64_u32(void);
 /* declare_local */
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct Minus__Minus_var_i64_u32_Frame_t;typedef struct Minus__Minus_var_i64_u32_Frame_t Minus__Minus_var_i64_u32_Frame_t;
LONGINT
__cdecl
Minus__Minus_var_i64_u32(void);
 /* declare_local */
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct Minus__uMinus_param_i64_u32_Frame_t;typedef struct Minus__uMinus_param_i64_u32_Frame_t Minus__uMinus_param_i64_u32_Frame_t;
 /* declare_local */
 /* internal_declare_param */
 /* internal_declare_param */
Long__T
__cdecl
Minus__uMinus_param_i64_u32(
   /* Param_Type1 */ Minus__INT64 a_L_671,
   /* Param_Type1 */ Minus__UINT32 b_L_672);
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct Minus__Minus_param_i64_u32_Frame_t;typedef struct Minus__Minus_param_i64_u32_Frame_t Minus__Minus_param_i64_u32_Frame_t;
 /* declare_local */
 /* internal_declare_param */
 /* internal_declare_param */
LONGINT
__cdecl
Minus__Minus_param_i64_u32(
   /* Param_Type1 */ Minus__INT64 a_L_674,
   /* Param_Type1 */ Minus__UINT32 b_L_675);
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct Minus__uMinus_var_i64_u8_Frame_t;typedef struct Minus__uMinus_var_i64_u8_Frame_t Minus__uMinus_var_i64_u8_Frame_t;
Long__T
__cdecl
Minus__uMinus_var_i64_u8(void);
 /* declare_local */
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct Minus__Minus_var_i64_u8_Frame_t;typedef struct Minus__Minus_var_i64_u8_Frame_t Minus__Minus_var_i64_u8_Frame_t;
LONGINT
__cdecl
Minus__Minus_var_i64_u8(void);
 /* declare_local */
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct Minus__uMinus_param_i64_u8_Frame_t;typedef struct Minus__uMinus_param_i64_u8_Frame_t Minus__uMinus_param_i64_u8_Frame_t;
 /* declare_local */
 /* internal_declare_param */
 /* internal_declare_param */
Long__T
__cdecl
Minus__uMinus_param_i64_u8(
   /* Param_Type1 */ Minus__INT64 a_L_679,
   /* Param_Type1 */ Minus__UINT8 b_L_680);
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct Minus__Minus_param_i64_u8_Frame_t;typedef struct Minus__Minus_param_i64_u8_Frame_t Minus__Minus_param_i64_u8_Frame_t;
 /* declare_local */
 /* internal_declare_param */
 /* internal_declare_param */
LONGINT
__cdecl
Minus__Minus_param_i64_u8(
   /* Param_Type1 */ Minus__INT64 a_L_682,
   /* Param_Type1 */ Minus__UINT8 b_L_683);
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct Minus__uMinus_var_i64_L_Frame_t;typedef struct Minus__uMinus_var_i64_L_Frame_t Minus__uMinus_var_i64_L_Frame_t;
Long__T
__cdecl
Minus__uMinus_var_i64_L(void);
 /* declare_local */
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct Minus__Minus_var_i64_L_Frame_t;typedef struct Minus__Minus_var_i64_L_Frame_t Minus__Minus_var_i64_L_Frame_t;
LONGINT
__cdecl
Minus__Minus_var_i64_L(void);
 /* declare_local */
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct Minus__uMinus_param_i64_L_Frame_t;typedef struct Minus__uMinus_param_i64_L_Frame_t Minus__uMinus_param_i64_L_Frame_t;
 /* declare_local */
 /* internal_declare_param */
 /* internal_declare_param */
Long__T
__cdecl
Minus__uMinus_param_i64_L(
   /* Param_Type1 */ Minus__INT64 a_L_687,
   /* Param_Type1 */ LONGINT b_L_688);
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct Minus__Minus_param_i64_L_Frame_t;typedef struct Minus__Minus_param_i64_L_Frame_t Minus__Minus_param_i64_L_Frame_t;
 /* declare_local */
 /* internal_declare_param */
 /* internal_declare_param */
LONGINT
__cdecl
Minus__Minus_param_i64_L(
   /* Param_Type1 */ Minus__INT64 a_L_690,
   /* Param_Type1 */ LONGINT b_L_691);
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct Minus__Minus_var_f32_f32_Frame_t;typedef struct Minus__Minus_var_f32_f32_Frame_t Minus__Minus_var_f32_f32_Frame_t;
Minus__FLOAT32
__cdecl
Minus__Minus_var_f32_f32(void);
 /* declare_local */
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct Minus__Minus_param_f32_f32_Frame_t;typedef struct Minus__Minus_param_f32_f32_Frame_t Minus__Minus_param_f32_f32_Frame_t;
 /* declare_local */
 /* internal_declare_param */
 /* internal_declare_param */
Minus__FLOAT32
__cdecl
Minus__Minus_param_f32_f32(
   /* Param_Type1 */ Minus__FLOAT32 a_L_694,
   /* Param_Type1 */ Minus__FLOAT32 b_L_695);
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct Minus__uMinus_var_i16_i8_Frame_t;typedef struct Minus__uMinus_var_i16_i8_Frame_t Minus__uMinus_var_i16_i8_Frame_t;
Word__T
__cdecl
Minus__uMinus_var_i16_i8(void);
 /* declare_local */
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct Minus__Minus_var_i16_i8_Frame_t;typedef struct Minus__Minus_var_i16_i8_Frame_t Minus__Minus_var_i16_i8_Frame_t;
Minus__INT16
__cdecl
Minus__Minus_var_i16_i8(void);
 /* declare_local */
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct Minus__uMinus_param_i16_i8_Frame_t;typedef struct Minus__uMinus_param_i16_i8_Frame_t Minus__uMinus_param_i16_i8_Frame_t;
 /* declare_local */
 /* internal_declare_param */
 /* internal_declare_param */
Word__T
__cdecl
Minus__uMinus_param_i16_i8(
   /* Param_Type1 */ Minus__INT16 a_L_699,
   /* Param_Type1 */ Minus__INT8 b_L_700);
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct Minus__Minus_param_i16_i8_Frame_t;typedef struct Minus__Minus_param_i16_i8_Frame_t Minus__Minus_param_i16_i8_Frame_t;
 /* declare_local */
 /* internal_declare_param */
 /* internal_declare_param */
Minus__INT16
__cdecl
Minus__Minus_param_i16_i8(
   /* Param_Type1 */ Minus__INT16 a_L_702,
   /* Param_Type1 */ Minus__INT8 b_L_703);
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct Minus__uMinus_var_i16_u64_Frame_t;typedef struct Minus__uMinus_var_i16_u64_Frame_t Minus__uMinus_var_i16_u64_Frame_t;
Long__T
__cdecl
Minus__uMinus_var_i16_u64(void);
 /* declare_local */
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct Minus__Minus_var_i16_u64_Frame_t;typedef struct Minus__Minus_var_i16_u64_Frame_t Minus__Minus_var_i16_u64_Frame_t;
LONGINT
__cdecl
Minus__Minus_var_i16_u64(void);
 /* declare_local */
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct Minus__uMinus_param_i16_u64_Frame_t;typedef struct Minus__uMinus_param_i16_u64_Frame_t Minus__uMinus_param_i16_u64_Frame_t;
 /* declare_local */
 /* internal_declare_param */
 /* internal_declare_param */
Long__T
__cdecl
Minus__uMinus_param_i16_u64(
   /* Param_Type1 */ Minus__INT16 a_L_707,
   /* Param_Type1 */ Minus__UINT64 b_L_708);
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct Minus__Minus_param_i16_u64_Frame_t;typedef struct Minus__Minus_param_i16_u64_Frame_t Minus__Minus_param_i16_u64_Frame_t;
 /* declare_local */
 /* internal_declare_param */
 /* internal_declare_param */
LONGINT
__cdecl
Minus__Minus_param_i16_u64(
   /* Param_Type1 */ Minus__INT16 a_L_710,
   /* Param_Type1 */ Minus__UINT64 b_L_711);
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct Minus__uMinus_var_i16_i32_Frame_t;typedef struct Minus__uMinus_var_i16_i32_Frame_t Minus__uMinus_var_i16_i32_Frame_t;
Word__T
__cdecl
Minus__uMinus_var_i16_i32(void);
 /* declare_local */
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct Minus__Minus_var_i16_i32_Frame_t;typedef struct Minus__Minus_var_i16_i32_Frame_t Minus__Minus_var_i16_i32_Frame_t;
Minus__INT16
__cdecl
Minus__Minus_var_i16_i32(void);
 /* declare_local */
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct Minus__uMinus_param_i16_i32_Frame_t;typedef struct Minus__uMinus_param_i16_i32_Frame_t Minus__uMinus_param_i16_i32_Frame_t;
 /* declare_local */
 /* internal_declare_param */
 /* internal_declare_param */
Word__T
__cdecl
Minus__uMinus_param_i16_i32(
   /* Param_Type1 */ Minus__INT16 a_L_715,
   /* Param_Type1 */ Minus__INT32 b_L_716);
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct Minus__Minus_param_i16_i32_Frame_t;typedef struct Minus__Minus_param_i16_i32_Frame_t Minus__Minus_param_i16_i32_Frame_t;
 /* declare_local */
 /* internal_declare_param */
 /* internal_declare_param */
Minus__INT16
__cdecl
Minus__Minus_param_i16_i32(
   /* Param_Type1 */ Minus__INT16 a_L_718,
   /* Param_Type1 */ Minus__INT32 b_L_719);
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct Minus__uMinus_var_i16_LC_Frame_t;typedef struct Minus__uMinus_var_i16_LC_Frame_t Minus__uMinus_var_i16_LC_Frame_t;
Long__T
__cdecl
Minus__uMinus_var_i16_LC(void);
 /* declare_local */
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct Minus__Minus_var_i16_LC_Frame_t;typedef struct Minus__Minus_var_i16_LC_Frame_t Minus__Minus_var_i16_LC_Frame_t;
LONGINT
__cdecl
Minus__Minus_var_i16_LC(void);
 /* declare_local */
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct Minus__uMinus_param_i16_LC_Frame_t;typedef struct Minus__uMinus_param_i16_LC_Frame_t Minus__uMinus_param_i16_LC_Frame_t;
 /* declare_local */
 /* internal_declare_param */
 /* internal_declare_param */
Long__T
__cdecl
Minus__uMinus_param_i16_LC(
   /* Param_Type1 */ Minus__INT16 a_L_723,
   /* Param_Type1 */ LONGCARD b_L_724);
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct Minus__Minus_param_i16_LC_Frame_t;typedef struct Minus__Minus_param_i16_LC_Frame_t Minus__Minus_param_i16_LC_Frame_t;
 /* declare_local */
 /* internal_declare_param */
 /* internal_declare_param */
LONGINT
__cdecl
Minus__Minus_param_i16_LC(
   /* Param_Type1 */ Minus__INT16 a_L_726,
   /* Param_Type1 */ LONGCARD b_L_727);
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct Minus__uMinus_var_i16_u16_Frame_t;typedef struct Minus__uMinus_var_i16_u16_Frame_t Minus__uMinus_var_i16_u16_Frame_t;
Word__T
__cdecl
Minus__uMinus_var_i16_u16(void);
 /* declare_local */
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct Minus__Minus_var_i16_u16_Frame_t;typedef struct Minus__Minus_var_i16_u16_Frame_t Minus__Minus_var_i16_u16_Frame_t;
Minus__INT16
__cdecl
Minus__Minus_var_i16_u16(void);
 /* declare_local */
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct Minus__uMinus_param_i16_u16_Frame_t;typedef struct Minus__uMinus_param_i16_u16_Frame_t Minus__uMinus_param_i16_u16_Frame_t;
 /* declare_local */
 /* internal_declare_param */
 /* internal_declare_param */
Word__T
__cdecl
Minus__uMinus_param_i16_u16(
   /* Param_Type1 */ Minus__INT16 a_L_731,
   /* Param_Type1 */ Minus__UINT16 b_L_732);
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct Minus__Minus_param_i16_u16_Frame_t;typedef struct Minus__Minus_param_i16_u16_Frame_t Minus__Minus_param_i16_u16_Frame_t;
 /* declare_local */
 /* internal_declare_param */
 /* internal_declare_param */
Minus__INT16
__cdecl
Minus__Minus_param_i16_u16(
   /* Param_Type1 */ Minus__INT16 a_L_734,
   /* Param_Type1 */ Minus__UINT16 b_L_735);
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct Minus__uMinus_var_i16_I_Frame_t;typedef struct Minus__uMinus_var_i16_I_Frame_t Minus__uMinus_var_i16_I_Frame_t;
Word__T
__cdecl
Minus__uMinus_var_i16_I(void);
 /* declare_local */
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct Minus__Minus_var_i16_I_Frame_t;typedef struct Minus__Minus_var_i16_I_Frame_t Minus__Minus_var_i16_I_Frame_t;
Minus__INT16
__cdecl
Minus__Minus_var_i16_I(void);
 /* declare_local */
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct Minus__uMinus_param_i16_I_Frame_t;typedef struct Minus__uMinus_param_i16_I_Frame_t Minus__uMinus_param_i16_I_Frame_t;
 /* declare_local */
 /* internal_declare_param */
 /* internal_declare_param */
Word__T
__cdecl
Minus__uMinus_param_i16_I(
   /* Param_Type1 */ Minus__INT16 a_L_739,
   /* Param_Type1 */ INTEGER b_L_740);
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct Minus__Minus_param_i16_I_Frame_t;typedef struct Minus__Minus_param_i16_I_Frame_t Minus__Minus_param_i16_I_Frame_t;
 /* declare_local */
 /* internal_declare_param */
 /* internal_declare_param */
Minus__INT16
__cdecl
Minus__Minus_param_i16_I(
   /* Param_Type1 */ Minus__INT16 a_L_742,
   /* Param_Type1 */ INTEGER b_L_743);
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct Minus__uMinus_var_i16_i64_Frame_t;typedef struct Minus__uMinus_var_i16_i64_Frame_t Minus__uMinus_var_i16_i64_Frame_t;
Long__T
__cdecl
Minus__uMinus_var_i16_i64(void);
 /* declare_local */
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct Minus__Minus_var_i16_i64_Frame_t;typedef struct Minus__Minus_var_i16_i64_Frame_t Minus__Minus_var_i16_i64_Frame_t;
LONGINT
__cdecl
Minus__Minus_var_i16_i64(void);
 /* declare_local */
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct Minus__uMinus_param_i16_i64_Frame_t;typedef struct Minus__uMinus_param_i16_i64_Frame_t Minus__uMinus_param_i16_i64_Frame_t;
 /* declare_local */
 /* internal_declare_param */
 /* internal_declare_param */
Long__T
__cdecl
Minus__uMinus_param_i16_i64(
   /* Param_Type1 */ Minus__INT16 a_L_747,
   /* Param_Type1 */ Minus__INT64 b_L_748);
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct Minus__Minus_param_i16_i64_Frame_t;typedef struct Minus__Minus_param_i16_i64_Frame_t Minus__Minus_param_i16_i64_Frame_t;
 /* declare_local */
 /* internal_declare_param */
 /* internal_declare_param */
LONGINT
__cdecl
Minus__Minus_param_i16_i64(
   /* Param_Type1 */ Minus__INT16 a_L_750,
   /* Param_Type1 */ Minus__INT64 b_L_751);
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct Minus__uMinus_var_i16_i16_Frame_t;typedef struct Minus__uMinus_var_i16_i16_Frame_t Minus__uMinus_var_i16_i16_Frame_t;
Word__T
__cdecl
Minus__uMinus_var_i16_i16(void);
 /* declare_local */
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct Minus__Minus_var_i16_i16_Frame_t;typedef struct Minus__Minus_var_i16_i16_Frame_t Minus__Minus_var_i16_i16_Frame_t;
Minus__INT16
__cdecl
Minus__Minus_var_i16_i16(void);
 /* declare_local */
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct Minus__uMinus_param_i16_i16_Frame_t;typedef struct Minus__uMinus_param_i16_i16_Frame_t Minus__uMinus_param_i16_i16_Frame_t;
 /* declare_local */
 /* internal_declare_param */
 /* internal_declare_param */
Word__T
__cdecl
Minus__uMinus_param_i16_i16(
   /* Param_Type1 */ Minus__INT16 a_L_755,
   /* Param_Type1 */ Minus__INT16 b_L_756);
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct Minus__Minus_param_i16_i16_Frame_t;typedef struct Minus__Minus_param_i16_i16_Frame_t Minus__Minus_param_i16_i16_Frame_t;
 /* declare_local */
 /* internal_declare_param */
 /* internal_declare_param */
Minus__INT16
__cdecl
Minus__Minus_param_i16_i16(
   /* Param_Type1 */ Minus__INT16 a_L_758,
   /* Param_Type1 */ Minus__INT16 b_L_759);
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct Minus__uMinus_var_i16_C_Frame_t;typedef struct Minus__uMinus_var_i16_C_Frame_t Minus__uMinus_var_i16_C_Frame_t;
Word__T
__cdecl
Minus__uMinus_var_i16_C(void);
 /* declare_local */
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct Minus__Minus_var_i16_C_Frame_t;typedef struct Minus__Minus_var_i16_C_Frame_t Minus__Minus_var_i16_C_Frame_t;
Minus__INT16
__cdecl
Minus__Minus_var_i16_C(void);
 /* declare_local */
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct Minus__uMinus_param_i16_C_Frame_t;typedef struct Minus__uMinus_param_i16_C_Frame_t Minus__uMinus_param_i16_C_Frame_t;
 /* declare_local */
 /* internal_declare_param */
 /* internal_declare_param */
Word__T
__cdecl
Minus__uMinus_param_i16_C(
   /* Param_Type1 */ Minus__INT16 a_L_763,
   /* Param_Type1 */ CARDINAL b_L_764);
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct Minus__Minus_param_i16_C_Frame_t;typedef struct Minus__Minus_param_i16_C_Frame_t Minus__Minus_param_i16_C_Frame_t;
 /* declare_local */
 /* internal_declare_param */
 /* internal_declare_param */
Minus__INT16
__cdecl
Minus__Minus_param_i16_C(
   /* Param_Type1 */ Minus__INT16 a_L_766,
   /* Param_Type1 */ CARDINAL b_L_767);
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct Minus__uMinus_var_i16_u32_Frame_t;typedef struct Minus__uMinus_var_i16_u32_Frame_t Minus__uMinus_var_i16_u32_Frame_t;
Word__T
__cdecl
Minus__uMinus_var_i16_u32(void);
 /* declare_local */
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct Minus__Minus_var_i16_u32_Frame_t;typedef struct Minus__Minus_var_i16_u32_Frame_t Minus__Minus_var_i16_u32_Frame_t;
Minus__INT16
__cdecl
Minus__Minus_var_i16_u32(void);
 /* declare_local */
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct Minus__uMinus_param_i16_u32_Frame_t;typedef struct Minus__uMinus_param_i16_u32_Frame_t Minus__uMinus_param_i16_u32_Frame_t;
 /* declare_local */
 /* internal_declare_param */
 /* internal_declare_param */
Word__T
__cdecl
Minus__uMinus_param_i16_u32(
   /* Param_Type1 */ Minus__INT16 a_L_771,
   /* Param_Type1 */ Minus__UINT32 b_L_772);
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct Minus__Minus_param_i16_u32_Frame_t;typedef struct Minus__Minus_param_i16_u32_Frame_t Minus__Minus_param_i16_u32_Frame_t;
 /* declare_local */
 /* internal_declare_param */
 /* internal_declare_param */
Minus__INT16
__cdecl
Minus__Minus_param_i16_u32(
   /* Param_Type1 */ Minus__INT16 a_L_774,
   /* Param_Type1 */ Minus__UINT32 b_L_775);
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct Minus__uMinus_var_i16_u8_Frame_t;typedef struct Minus__uMinus_var_i16_u8_Frame_t Minus__uMinus_var_i16_u8_Frame_t;
Word__T
__cdecl
Minus__uMinus_var_i16_u8(void);
 /* declare_local */
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct Minus__Minus_var_i16_u8_Frame_t;typedef struct Minus__Minus_var_i16_u8_Frame_t Minus__Minus_var_i16_u8_Frame_t;
Minus__INT16
__cdecl
Minus__Minus_var_i16_u8(void);
 /* declare_local */
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct Minus__uMinus_param_i16_u8_Frame_t;typedef struct Minus__uMinus_param_i16_u8_Frame_t Minus__uMinus_param_i16_u8_Frame_t;
 /* declare_local */
 /* internal_declare_param */
 /* internal_declare_param */
Word__T
__cdecl
Minus__uMinus_param_i16_u8(
   /* Param_Type1 */ Minus__INT16 a_L_779,
   /* Param_Type1 */ Minus__UINT8 b_L_780);
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct Minus__Minus_param_i16_u8_Frame_t;typedef struct Minus__Minus_param_i16_u8_Frame_t Minus__Minus_param_i16_u8_Frame_t;
 /* declare_local */
 /* internal_declare_param */
 /* internal_declare_param */
Minus__INT16
__cdecl
Minus__Minus_param_i16_u8(
   /* Param_Type1 */ Minus__INT16 a_L_782,
   /* Param_Type1 */ Minus__UINT8 b_L_783);
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct Minus__uMinus_var_i16_L_Frame_t;typedef struct Minus__uMinus_var_i16_L_Frame_t Minus__uMinus_var_i16_L_Frame_t;
Long__T
__cdecl
Minus__uMinus_var_i16_L(void);
 /* declare_local */
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct Minus__Minus_var_i16_L_Frame_t;typedef struct Minus__Minus_var_i16_L_Frame_t Minus__Minus_var_i16_L_Frame_t;
LONGINT
__cdecl
Minus__Minus_var_i16_L(void);
 /* declare_local */
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct Minus__uMinus_param_i16_L_Frame_t;typedef struct Minus__uMinus_param_i16_L_Frame_t Minus__uMinus_param_i16_L_Frame_t;
 /* declare_local */
 /* internal_declare_param */
 /* internal_declare_param */
Long__T
__cdecl
Minus__uMinus_param_i16_L(
   /* Param_Type1 */ Minus__INT16 a_L_787,
   /* Param_Type1 */ LONGINT b_L_788);
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct Minus__Minus_param_i16_L_Frame_t;typedef struct Minus__Minus_param_i16_L_Frame_t Minus__Minus_param_i16_L_Frame_t;
 /* declare_local */
 /* internal_declare_param */
 /* internal_declare_param */
LONGINT
__cdecl
Minus__Minus_param_i16_L(
   /* Param_Type1 */ Minus__INT16 a_L_790,
   /* Param_Type1 */ LONGINT b_L_791);
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct Minus__uMinus_var_C_i8_Frame_t;typedef struct Minus__uMinus_var_C_i8_Frame_t Minus__uMinus_var_C_i8_Frame_t;
Word__T
__cdecl
Minus__uMinus_var_C_i8(void);
 /* declare_local */
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct Minus__Minus_var_C_i8_Frame_t;typedef struct Minus__Minus_var_C_i8_Frame_t Minus__Minus_var_C_i8_Frame_t;
CARDINAL
__cdecl
Minus__Minus_var_C_i8(void);
 /* declare_local */
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct Minus__uMinus_param_C_i8_Frame_t;typedef struct Minus__uMinus_param_C_i8_Frame_t Minus__uMinus_param_C_i8_Frame_t;
 /* declare_local */
 /* internal_declare_param */
 /* internal_declare_param */
Word__T
__cdecl
Minus__uMinus_param_C_i8(
   /* Param_Type1 */ CARDINAL a_L_795,
   /* Param_Type1 */ Minus__INT8 b_L_796);
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct Minus__Minus_param_C_i8_Frame_t;typedef struct Minus__Minus_param_C_i8_Frame_t Minus__Minus_param_C_i8_Frame_t;
 /* declare_local */
 /* internal_declare_param */
 /* internal_declare_param */
CARDINAL
__cdecl
Minus__Minus_param_C_i8(
   /* Param_Type1 */ CARDINAL a_L_798,
   /* Param_Type1 */ Minus__INT8 b_L_799);
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct Minus__uMinus_var_C_u64_Frame_t;typedef struct Minus__uMinus_var_C_u64_Frame_t Minus__uMinus_var_C_u64_Frame_t;
Long__T
__cdecl
Minus__uMinus_var_C_u64(void);
 /* declare_local */
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct Minus__Minus_var_C_u64_Frame_t;typedef struct Minus__Minus_var_C_u64_Frame_t Minus__Minus_var_C_u64_Frame_t;
LONGINT
__cdecl
Minus__Minus_var_C_u64(void);
 /* declare_local */
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct Minus__uMinus_param_C_u64_Frame_t;typedef struct Minus__uMinus_param_C_u64_Frame_t Minus__uMinus_param_C_u64_Frame_t;
 /* declare_local */
 /* internal_declare_param */
 /* internal_declare_param */
Long__T
__cdecl
Minus__uMinus_param_C_u64(
   /* Param_Type1 */ CARDINAL a_L_803,
   /* Param_Type1 */ Minus__UINT64 b_L_804);
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct Minus__Minus_param_C_u64_Frame_t;typedef struct Minus__Minus_param_C_u64_Frame_t Minus__Minus_param_C_u64_Frame_t;
 /* declare_local */
 /* internal_declare_param */
 /* internal_declare_param */
LONGINT
__cdecl
Minus__Minus_param_C_u64(
   /* Param_Type1 */ CARDINAL a_L_806,
   /* Param_Type1 */ Minus__UINT64 b_L_807);
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct Minus__uMinus_var_C_i32_Frame_t;typedef struct Minus__uMinus_var_C_i32_Frame_t Minus__uMinus_var_C_i32_Frame_t;
Word__T
__cdecl
Minus__uMinus_var_C_i32(void);
 /* declare_local */
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct Minus__Minus_var_C_i32_Frame_t;typedef struct Minus__Minus_var_C_i32_Frame_t Minus__Minus_var_C_i32_Frame_t;
CARDINAL
__cdecl
Minus__Minus_var_C_i32(void);
 /* declare_local */
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct Minus__uMinus_param_C_i32_Frame_t;typedef struct Minus__uMinus_param_C_i32_Frame_t Minus__uMinus_param_C_i32_Frame_t;
 /* declare_local */
 /* internal_declare_param */
 /* internal_declare_param */
Word__T
__cdecl
Minus__uMinus_param_C_i32(
   /* Param_Type1 */ CARDINAL a_L_811,
   /* Param_Type1 */ Minus__INT32 b_L_812);
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct Minus__Minus_param_C_i32_Frame_t;typedef struct Minus__Minus_param_C_i32_Frame_t Minus__Minus_param_C_i32_Frame_t;
 /* declare_local */
 /* internal_declare_param */
 /* internal_declare_param */
CARDINAL
__cdecl
Minus__Minus_param_C_i32(
   /* Param_Type1 */ CARDINAL a_L_814,
   /* Param_Type1 */ Minus__INT32 b_L_815);
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct Minus__uMinus_var_C_LC_Frame_t;typedef struct Minus__uMinus_var_C_LC_Frame_t Minus__uMinus_var_C_LC_Frame_t;
Long__T
__cdecl
Minus__uMinus_var_C_LC(void);
 /* declare_local */
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct Minus__Minus_var_C_LC_Frame_t;typedef struct Minus__Minus_var_C_LC_Frame_t Minus__Minus_var_C_LC_Frame_t;
LONGINT
__cdecl
Minus__Minus_var_C_LC(void);
 /* declare_local */
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct Minus__uMinus_param_C_LC_Frame_t;typedef struct Minus__uMinus_param_C_LC_Frame_t Minus__uMinus_param_C_LC_Frame_t;
 /* declare_local */
 /* internal_declare_param */
 /* internal_declare_param */
Long__T
__cdecl
Minus__uMinus_param_C_LC(
   /* Param_Type1 */ CARDINAL a_L_819,
   /* Param_Type1 */ LONGCARD b_L_820);
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct Minus__Minus_param_C_LC_Frame_t;typedef struct Minus__Minus_param_C_LC_Frame_t Minus__Minus_param_C_LC_Frame_t;
 /* declare_local */
 /* internal_declare_param */
 /* internal_declare_param */
LONGINT
__cdecl
Minus__Minus_param_C_LC(
   /* Param_Type1 */ CARDINAL a_L_822,
   /* Param_Type1 */ LONGCARD b_L_823);
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct Minus__uMinus_var_C_u16_Frame_t;typedef struct Minus__uMinus_var_C_u16_Frame_t Minus__uMinus_var_C_u16_Frame_t;
Word__T
__cdecl
Minus__uMinus_var_C_u16(void);
 /* declare_local */
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct Minus__Minus_var_C_u16_Frame_t;typedef struct Minus__Minus_var_C_u16_Frame_t Minus__Minus_var_C_u16_Frame_t;
CARDINAL
__cdecl
Minus__Minus_var_C_u16(void);
 /* declare_local */
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct Minus__uMinus_param_C_u16_Frame_t;typedef struct Minus__uMinus_param_C_u16_Frame_t Minus__uMinus_param_C_u16_Frame_t;
 /* declare_local */
 /* internal_declare_param */
 /* internal_declare_param */
Word__T
__cdecl
Minus__uMinus_param_C_u16(
   /* Param_Type1 */ CARDINAL a_L_827,
   /* Param_Type1 */ Minus__UINT16 b_L_828);
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct Minus__Minus_param_C_u16_Frame_t;typedef struct Minus__Minus_param_C_u16_Frame_t Minus__Minus_param_C_u16_Frame_t;
 /* declare_local */
 /* internal_declare_param */
 /* internal_declare_param */
CARDINAL
__cdecl
Minus__Minus_param_C_u16(
   /* Param_Type1 */ CARDINAL a_L_830,
   /* Param_Type1 */ Minus__UINT16 b_L_831);
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct Minus__uMinus_var_C_I_Frame_t;typedef struct Minus__uMinus_var_C_I_Frame_t Minus__uMinus_var_C_I_Frame_t;
Word__T
__cdecl
Minus__uMinus_var_C_I(void);
 /* declare_local */
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct Minus__Minus_var_C_I_Frame_t;typedef struct Minus__Minus_var_C_I_Frame_t Minus__Minus_var_C_I_Frame_t;
CARDINAL
__cdecl
Minus__Minus_var_C_I(void);
 /* declare_local */
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct Minus__uMinus_param_C_I_Frame_t;typedef struct Minus__uMinus_param_C_I_Frame_t Minus__uMinus_param_C_I_Frame_t;
 /* declare_local */
 /* internal_declare_param */
 /* internal_declare_param */
Word__T
__cdecl
Minus__uMinus_param_C_I(
   /* Param_Type1 */ CARDINAL a_L_835,
   /* Param_Type1 */ INTEGER b_L_836);
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct Minus__Minus_param_C_I_Frame_t;typedef struct Minus__Minus_param_C_I_Frame_t Minus__Minus_param_C_I_Frame_t;
 /* declare_local */
 /* internal_declare_param */
 /* internal_declare_param */
CARDINAL
__cdecl
Minus__Minus_param_C_I(
   /* Param_Type1 */ CARDINAL a_L_838,
   /* Param_Type1 */ INTEGER b_L_839);
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct Minus__uMinus_var_C_i64_Frame_t;typedef struct Minus__uMinus_var_C_i64_Frame_t Minus__uMinus_var_C_i64_Frame_t;
Long__T
__cdecl
Minus__uMinus_var_C_i64(void);
 /* declare_local */
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct Minus__Minus_var_C_i64_Frame_t;typedef struct Minus__Minus_var_C_i64_Frame_t Minus__Minus_var_C_i64_Frame_t;
LONGINT
__cdecl
Minus__Minus_var_C_i64(void);
 /* declare_local */
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct Minus__uMinus_param_C_i64_Frame_t;typedef struct Minus__uMinus_param_C_i64_Frame_t Minus__uMinus_param_C_i64_Frame_t;
 /* declare_local */
 /* internal_declare_param */
 /* internal_declare_param */
Long__T
__cdecl
Minus__uMinus_param_C_i64(
   /* Param_Type1 */ CARDINAL a_L_843,
   /* Param_Type1 */ Minus__INT64 b_L_844);
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct Minus__Minus_param_C_i64_Frame_t;typedef struct Minus__Minus_param_C_i64_Frame_t Minus__Minus_param_C_i64_Frame_t;
 /* declare_local */
 /* internal_declare_param */
 /* internal_declare_param */
LONGINT
__cdecl
Minus__Minus_param_C_i64(
   /* Param_Type1 */ CARDINAL a_L_846,
   /* Param_Type1 */ Minus__INT64 b_L_847);
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct Minus__uMinus_var_C_i16_Frame_t;typedef struct Minus__uMinus_var_C_i16_Frame_t Minus__uMinus_var_C_i16_Frame_t;
Word__T
__cdecl
Minus__uMinus_var_C_i16(void);
 /* declare_local */
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct Minus__Minus_var_C_i16_Frame_t;typedef struct Minus__Minus_var_C_i16_Frame_t Minus__Minus_var_C_i16_Frame_t;
CARDINAL
__cdecl
Minus__Minus_var_C_i16(void);
 /* declare_local */
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct Minus__uMinus_param_C_i16_Frame_t;typedef struct Minus__uMinus_param_C_i16_Frame_t Minus__uMinus_param_C_i16_Frame_t;
 /* declare_local */
 /* internal_declare_param */
 /* internal_declare_param */
Word__T
__cdecl
Minus__uMinus_param_C_i16(
   /* Param_Type1 */ CARDINAL a_L_851,
   /* Param_Type1 */ Minus__INT16 b_L_852);
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct Minus__Minus_param_C_i16_Frame_t;typedef struct Minus__Minus_param_C_i16_Frame_t Minus__Minus_param_C_i16_Frame_t;
 /* declare_local */
 /* internal_declare_param */
 /* internal_declare_param */
CARDINAL
__cdecl
Minus__Minus_param_C_i16(
   /* Param_Type1 */ CARDINAL a_L_854,
   /* Param_Type1 */ Minus__INT16 b_L_855);
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct Minus__uMinus_var_C_C_Frame_t;typedef struct Minus__uMinus_var_C_C_Frame_t Minus__uMinus_var_C_C_Frame_t;
Word__T
__cdecl
Minus__uMinus_var_C_C(void);
 /* declare_local */
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct Minus__Minus_var_C_C_Frame_t;typedef struct Minus__Minus_var_C_C_Frame_t Minus__Minus_var_C_C_Frame_t;
CARDINAL
__cdecl
Minus__Minus_var_C_C(void);
 /* declare_local */
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct Minus__uMinus_param_C_C_Frame_t;typedef struct Minus__uMinus_param_C_C_Frame_t Minus__uMinus_param_C_C_Frame_t;
 /* declare_local */
 /* internal_declare_param */
 /* internal_declare_param */
Word__T
__cdecl
Minus__uMinus_param_C_C(
   /* Param_Type1 */ CARDINAL a_L_859,
   /* Param_Type1 */ CARDINAL b_L_860);
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct Minus__Minus_param_C_C_Frame_t;typedef struct Minus__Minus_param_C_C_Frame_t Minus__Minus_param_C_C_Frame_t;
 /* declare_local */
 /* internal_declare_param */
 /* internal_declare_param */
CARDINAL
__cdecl
Minus__Minus_param_C_C(
   /* Param_Type1 */ CARDINAL a_L_862,
   /* Param_Type1 */ CARDINAL b_L_863);
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct Minus__uMinus_var_C_u32_Frame_t;typedef struct Minus__uMinus_var_C_u32_Frame_t Minus__uMinus_var_C_u32_Frame_t;
Word__T
__cdecl
Minus__uMinus_var_C_u32(void);
 /* declare_local */
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct Minus__Minus_var_C_u32_Frame_t;typedef struct Minus__Minus_var_C_u32_Frame_t Minus__Minus_var_C_u32_Frame_t;
CARDINAL
__cdecl
Minus__Minus_var_C_u32(void);
 /* declare_local */
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct Minus__uMinus_param_C_u32_Frame_t;typedef struct Minus__uMinus_param_C_u32_Frame_t Minus__uMinus_param_C_u32_Frame_t;
 /* declare_local */
 /* internal_declare_param */
 /* internal_declare_param */
Word__T
__cdecl
Minus__uMinus_param_C_u32(
   /* Param_Type1 */ CARDINAL a_L_867,
   /* Param_Type1 */ Minus__UINT32 b_L_868);
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct Minus__Minus_param_C_u32_Frame_t;typedef struct Minus__Minus_param_C_u32_Frame_t Minus__Minus_param_C_u32_Frame_t;
 /* declare_local */
 /* internal_declare_param */
 /* internal_declare_param */
CARDINAL
__cdecl
Minus__Minus_param_C_u32(
   /* Param_Type1 */ CARDINAL a_L_870,
   /* Param_Type1 */ Minus__UINT32 b_L_871);
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct Minus__uMinus_var_C_u8_Frame_t;typedef struct Minus__uMinus_var_C_u8_Frame_t Minus__uMinus_var_C_u8_Frame_t;
Word__T
__cdecl
Minus__uMinus_var_C_u8(void);
 /* declare_local */
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct Minus__Minus_var_C_u8_Frame_t;typedef struct Minus__Minus_var_C_u8_Frame_t Minus__Minus_var_C_u8_Frame_t;
CARDINAL
__cdecl
Minus__Minus_var_C_u8(void);
 /* declare_local */
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct Minus__uMinus_param_C_u8_Frame_t;typedef struct Minus__uMinus_param_C_u8_Frame_t Minus__uMinus_param_C_u8_Frame_t;
 /* declare_local */
 /* internal_declare_param */
 /* internal_declare_param */
Word__T
__cdecl
Minus__uMinus_param_C_u8(
   /* Param_Type1 */ CARDINAL a_L_875,
   /* Param_Type1 */ Minus__UINT8 b_L_876);
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct Minus__Minus_param_C_u8_Frame_t;typedef struct Minus__Minus_param_C_u8_Frame_t Minus__Minus_param_C_u8_Frame_t;
 /* declare_local */
 /* internal_declare_param */
 /* internal_declare_param */
CARDINAL
__cdecl
Minus__Minus_param_C_u8(
   /* Param_Type1 */ CARDINAL a_L_878,
   /* Param_Type1 */ Minus__UINT8 b_L_879);
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct Minus__uMinus_var_C_L_Frame_t;typedef struct Minus__uMinus_var_C_L_Frame_t Minus__uMinus_var_C_L_Frame_t;
Long__T
__cdecl
Minus__uMinus_var_C_L(void);
 /* declare_local */
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct Minus__Minus_var_C_L_Frame_t;typedef struct Minus__Minus_var_C_L_Frame_t Minus__Minus_var_C_L_Frame_t;
LONGINT
__cdecl
Minus__Minus_var_C_L(void);
 /* declare_local */
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct Minus__uMinus_param_C_L_Frame_t;typedef struct Minus__uMinus_param_C_L_Frame_t Minus__uMinus_param_C_L_Frame_t;
 /* declare_local */
 /* internal_declare_param */
 /* internal_declare_param */
Long__T
__cdecl
Minus__uMinus_param_C_L(
   /* Param_Type1 */ CARDINAL a_L_883,
   /* Param_Type1 */ LONGINT b_L_884);
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct Minus__Minus_param_C_L_Frame_t;typedef struct Minus__Minus_param_C_L_Frame_t Minus__Minus_param_C_L_Frame_t;
 /* declare_local */
 /* internal_declare_param */
 /* internal_declare_param */
LONGINT
__cdecl
Minus__Minus_param_C_L(
   /* Param_Type1 */ CARDINAL a_L_886,
   /* Param_Type1 */ LONGINT b_L_887);
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct Minus__uMinus_var_u32_i8_Frame_t;typedef struct Minus__uMinus_var_u32_i8_Frame_t Minus__uMinus_var_u32_i8_Frame_t;
Word__T
__cdecl
Minus__uMinus_var_u32_i8(void);
 /* declare_local */
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct Minus__Minus_var_u32_i8_Frame_t;typedef struct Minus__Minus_var_u32_i8_Frame_t Minus__Minus_var_u32_i8_Frame_t;
Minus__UINT32
__cdecl
Minus__Minus_var_u32_i8(void);
 /* declare_local */
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct Minus__uMinus_param_u32_i8_Frame_t;typedef struct Minus__uMinus_param_u32_i8_Frame_t Minus__uMinus_param_u32_i8_Frame_t;
 /* declare_local */
 /* internal_declare_param */
 /* internal_declare_param */
Word__T
__cdecl
Minus__uMinus_param_u32_i8(
   /* Param_Type1 */ Minus__UINT32 a_L_891,
   /* Param_Type1 */ Minus__INT8 b_L_892);
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct Minus__Minus_param_u32_i8_Frame_t;typedef struct Minus__Minus_param_u32_i8_Frame_t Minus__Minus_param_u32_i8_Frame_t;
 /* declare_local */
 /* internal_declare_param */
 /* internal_declare_param */
Minus__UINT32
__cdecl
Minus__Minus_param_u32_i8(
   /* Param_Type1 */ Minus__UINT32 a_L_894,
   /* Param_Type1 */ Minus__INT8 b_L_895);
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct Minus__uMinus_var_u32_u64_Frame_t;typedef struct Minus__uMinus_var_u32_u64_Frame_t Minus__uMinus_var_u32_u64_Frame_t;
Long__T
__cdecl
Minus__uMinus_var_u32_u64(void);
 /* declare_local */
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct Minus__Minus_var_u32_u64_Frame_t;typedef struct Minus__Minus_var_u32_u64_Frame_t Minus__Minus_var_u32_u64_Frame_t;
LONGINT
__cdecl
Minus__Minus_var_u32_u64(void);
 /* declare_local */
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct Minus__uMinus_param_u32_u64_Frame_t;typedef struct Minus__uMinus_param_u32_u64_Frame_t Minus__uMinus_param_u32_u64_Frame_t;
 /* declare_local */
 /* internal_declare_param */
 /* internal_declare_param */
Long__T
__cdecl
Minus__uMinus_param_u32_u64(
   /* Param_Type1 */ Minus__UINT32 a_L_899,
   /* Param_Type1 */ Minus__UINT64 b_L_900);
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct Minus__Minus_param_u32_u64_Frame_t;typedef struct Minus__Minus_param_u32_u64_Frame_t Minus__Minus_param_u32_u64_Frame_t;
 /* declare_local */
 /* internal_declare_param */
 /* internal_declare_param */
LONGINT
__cdecl
Minus__Minus_param_u32_u64(
   /* Param_Type1 */ Minus__UINT32 a_L_902,
   /* Param_Type1 */ Minus__UINT64 b_L_903);
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct Minus__uMinus_var_u32_i32_Frame_t;typedef struct Minus__uMinus_var_u32_i32_Frame_t Minus__uMinus_var_u32_i32_Frame_t;
Word__T
__cdecl
Minus__uMinus_var_u32_i32(void);
 /* declare_local */
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct Minus__Minus_var_u32_i32_Frame_t;typedef struct Minus__Minus_var_u32_i32_Frame_t Minus__Minus_var_u32_i32_Frame_t;
Minus__UINT32
__cdecl
Minus__Minus_var_u32_i32(void);
 /* declare_local */
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct Minus__uMinus_param_u32_i32_Frame_t;typedef struct Minus__uMinus_param_u32_i32_Frame_t Minus__uMinus_param_u32_i32_Frame_t;
 /* declare_local */
 /* internal_declare_param */
 /* internal_declare_param */
Word__T
__cdecl
Minus__uMinus_param_u32_i32(
   /* Param_Type1 */ Minus__UINT32 a_L_907,
   /* Param_Type1 */ Minus__INT32 b_L_908);
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct Minus__Minus_param_u32_i32_Frame_t;typedef struct Minus__Minus_param_u32_i32_Frame_t Minus__Minus_param_u32_i32_Frame_t;
 /* declare_local */
 /* internal_declare_param */
 /* internal_declare_param */
Minus__UINT32
__cdecl
Minus__Minus_param_u32_i32(
   /* Param_Type1 */ Minus__UINT32 a_L_910,
   /* Param_Type1 */ Minus__INT32 b_L_911);
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct Minus__uMinus_var_u32_LC_Frame_t;typedef struct Minus__uMinus_var_u32_LC_Frame_t Minus__uMinus_var_u32_LC_Frame_t;
Long__T
__cdecl
Minus__uMinus_var_u32_LC(void);
 /* declare_local */
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct Minus__Minus_var_u32_LC_Frame_t;typedef struct Minus__Minus_var_u32_LC_Frame_t Minus__Minus_var_u32_LC_Frame_t;
LONGINT
__cdecl
Minus__Minus_var_u32_LC(void);
 /* declare_local */
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct Minus__uMinus_param_u32_LC_Frame_t;typedef struct Minus__uMinus_param_u32_LC_Frame_t Minus__uMinus_param_u32_LC_Frame_t;
 /* declare_local */
 /* internal_declare_param */
 /* internal_declare_param */
Long__T
__cdecl
Minus__uMinus_param_u32_LC(
   /* Param_Type1 */ Minus__UINT32 a_L_915,
   /* Param_Type1 */ LONGCARD b_L_916);
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct Minus__Minus_param_u32_LC_Frame_t;typedef struct Minus__Minus_param_u32_LC_Frame_t Minus__Minus_param_u32_LC_Frame_t;
 /* declare_local */
 /* internal_declare_param */
 /* internal_declare_param */
LONGINT
__cdecl
Minus__Minus_param_u32_LC(
   /* Param_Type1 */ Minus__UINT32 a_L_918,
   /* Param_Type1 */ LONGCARD b_L_919);
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct Minus__uMinus_var_u32_u16_Frame_t;typedef struct Minus__uMinus_var_u32_u16_Frame_t Minus__uMinus_var_u32_u16_Frame_t;
Word__T
__cdecl
Minus__uMinus_var_u32_u16(void);
 /* declare_local */
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct Minus__Minus_var_u32_u16_Frame_t;typedef struct Minus__Minus_var_u32_u16_Frame_t Minus__Minus_var_u32_u16_Frame_t;
Minus__UINT32
__cdecl
Minus__Minus_var_u32_u16(void);
 /* declare_local */
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct Minus__uMinus_param_u32_u16_Frame_t;typedef struct Minus__uMinus_param_u32_u16_Frame_t Minus__uMinus_param_u32_u16_Frame_t;
 /* declare_local */
 /* internal_declare_param */
 /* internal_declare_param */
Word__T
__cdecl
Minus__uMinus_param_u32_u16(
   /* Param_Type1 */ Minus__UINT32 a_L_923,
   /* Param_Type1 */ Minus__UINT16 b_L_924);
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct Minus__Minus_param_u32_u16_Frame_t;typedef struct Minus__Minus_param_u32_u16_Frame_t Minus__Minus_param_u32_u16_Frame_t;
 /* declare_local */
 /* internal_declare_param */
 /* internal_declare_param */
Minus__UINT32
__cdecl
Minus__Minus_param_u32_u16(
   /* Param_Type1 */ Minus__UINT32 a_L_926,
   /* Param_Type1 */ Minus__UINT16 b_L_927);
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct Minus__uMinus_var_u32_I_Frame_t;typedef struct Minus__uMinus_var_u32_I_Frame_t Minus__uMinus_var_u32_I_Frame_t;
Word__T
__cdecl
Minus__uMinus_var_u32_I(void);
 /* declare_local */
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct Minus__Minus_var_u32_I_Frame_t;typedef struct Minus__Minus_var_u32_I_Frame_t Minus__Minus_var_u32_I_Frame_t;
Minus__UINT32
__cdecl
Minus__Minus_var_u32_I(void);
 /* declare_local */
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct Minus__uMinus_param_u32_I_Frame_t;typedef struct Minus__uMinus_param_u32_I_Frame_t Minus__uMinus_param_u32_I_Frame_t;
 /* declare_local */
 /* internal_declare_param */
 /* internal_declare_param */
Word__T
__cdecl
Minus__uMinus_param_u32_I(
   /* Param_Type1 */ Minus__UINT32 a_L_931,
   /* Param_Type1 */ INTEGER b_L_932);
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct Minus__Minus_param_u32_I_Frame_t;typedef struct Minus__Minus_param_u32_I_Frame_t Minus__Minus_param_u32_I_Frame_t;
 /* declare_local */
 /* internal_declare_param */
 /* internal_declare_param */
Minus__UINT32
__cdecl
Minus__Minus_param_u32_I(
   /* Param_Type1 */ Minus__UINT32 a_L_934,
   /* Param_Type1 */ INTEGER b_L_935);
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct Minus__uMinus_var_u32_i64_Frame_t;typedef struct Minus__uMinus_var_u32_i64_Frame_t Minus__uMinus_var_u32_i64_Frame_t;
Long__T
__cdecl
Minus__uMinus_var_u32_i64(void);
 /* declare_local */
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct Minus__Minus_var_u32_i64_Frame_t;typedef struct Minus__Minus_var_u32_i64_Frame_t Minus__Minus_var_u32_i64_Frame_t;
LONGINT
__cdecl
Minus__Minus_var_u32_i64(void);
 /* declare_local */
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct Minus__uMinus_param_u32_i64_Frame_t;typedef struct Minus__uMinus_param_u32_i64_Frame_t Minus__uMinus_param_u32_i64_Frame_t;
 /* declare_local */
 /* internal_declare_param */
 /* internal_declare_param */
Long__T
__cdecl
Minus__uMinus_param_u32_i64(
   /* Param_Type1 */ Minus__UINT32 a_L_939,
   /* Param_Type1 */ Minus__INT64 b_L_940);
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct Minus__Minus_param_u32_i64_Frame_t;typedef struct Minus__Minus_param_u32_i64_Frame_t Minus__Minus_param_u32_i64_Frame_t;
 /* declare_local */
 /* internal_declare_param */
 /* internal_declare_param */
LONGINT
__cdecl
Minus__Minus_param_u32_i64(
   /* Param_Type1 */ Minus__UINT32 a_L_942,
   /* Param_Type1 */ Minus__INT64 b_L_943);
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct Minus__uMinus_var_u32_i16_Frame_t;typedef struct Minus__uMinus_var_u32_i16_Frame_t Minus__uMinus_var_u32_i16_Frame_t;
Word__T
__cdecl
Minus__uMinus_var_u32_i16(void);
 /* declare_local */
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct Minus__Minus_var_u32_i16_Frame_t;typedef struct Minus__Minus_var_u32_i16_Frame_t Minus__Minus_var_u32_i16_Frame_t;
Minus__UINT32
__cdecl
Minus__Minus_var_u32_i16(void);
 /* declare_local */
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct Minus__uMinus_param_u32_i16_Frame_t;typedef struct Minus__uMinus_param_u32_i16_Frame_t Minus__uMinus_param_u32_i16_Frame_t;
 /* declare_local */
 /* internal_declare_param */
 /* internal_declare_param */
Word__T
__cdecl
Minus__uMinus_param_u32_i16(
   /* Param_Type1 */ Minus__UINT32 a_L_947,
   /* Param_Type1 */ Minus__INT16 b_L_948);
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct Minus__Minus_param_u32_i16_Frame_t;typedef struct Minus__Minus_param_u32_i16_Frame_t Minus__Minus_param_u32_i16_Frame_t;
 /* declare_local */
 /* internal_declare_param */
 /* internal_declare_param */
Minus__UINT32
__cdecl
Minus__Minus_param_u32_i16(
   /* Param_Type1 */ Minus__UINT32 a_L_950,
   /* Param_Type1 */ Minus__INT16 b_L_951);
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct Minus__uMinus_var_u32_C_Frame_t;typedef struct Minus__uMinus_var_u32_C_Frame_t Minus__uMinus_var_u32_C_Frame_t;
Word__T
__cdecl
Minus__uMinus_var_u32_C(void);
 /* declare_local */
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct Minus__Minus_var_u32_C_Frame_t;typedef struct Minus__Minus_var_u32_C_Frame_t Minus__Minus_var_u32_C_Frame_t;
Minus__UINT32
__cdecl
Minus__Minus_var_u32_C(void);
 /* declare_local */
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct Minus__uMinus_param_u32_C_Frame_t;typedef struct Minus__uMinus_param_u32_C_Frame_t Minus__uMinus_param_u32_C_Frame_t;
 /* declare_local */
 /* internal_declare_param */
 /* internal_declare_param */
Word__T
__cdecl
Minus__uMinus_param_u32_C(
   /* Param_Type1 */ Minus__UINT32 a_L_955,
   /* Param_Type1 */ CARDINAL b_L_956);
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct Minus__Minus_param_u32_C_Frame_t;typedef struct Minus__Minus_param_u32_C_Frame_t Minus__Minus_param_u32_C_Frame_t;
 /* declare_local */
 /* internal_declare_param */
 /* internal_declare_param */
Minus__UINT32
__cdecl
Minus__Minus_param_u32_C(
   /* Param_Type1 */ Minus__UINT32 a_L_958,
   /* Param_Type1 */ CARDINAL b_L_959);
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct Minus__uMinus_var_u32_u32_Frame_t;typedef struct Minus__uMinus_var_u32_u32_Frame_t Minus__uMinus_var_u32_u32_Frame_t;
Word__T
__cdecl
Minus__uMinus_var_u32_u32(void);
 /* declare_local */
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct Minus__Minus_var_u32_u32_Frame_t;typedef struct Minus__Minus_var_u32_u32_Frame_t Minus__Minus_var_u32_u32_Frame_t;
Minus__UINT32
__cdecl
Minus__Minus_var_u32_u32(void);
 /* declare_local */
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct Minus__uMinus_param_u32_u32_Frame_t;typedef struct Minus__uMinus_param_u32_u32_Frame_t Minus__uMinus_param_u32_u32_Frame_t;
 /* declare_local */
 /* internal_declare_param */
 /* internal_declare_param */
Word__T
__cdecl
Minus__uMinus_param_u32_u32(
   /* Param_Type1 */ Minus__UINT32 a_L_963,
   /* Param_Type1 */ Minus__UINT32 b_L_964);
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct Minus__Minus_param_u32_u32_Frame_t;typedef struct Minus__Minus_param_u32_u32_Frame_t Minus__Minus_param_u32_u32_Frame_t;
 /* declare_local */
 /* internal_declare_param */
 /* internal_declare_param */
Minus__UINT32
__cdecl
Minus__Minus_param_u32_u32(
   /* Param_Type1 */ Minus__UINT32 a_L_966,
   /* Param_Type1 */ Minus__UINT32 b_L_967);
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct Minus__uMinus_var_u32_u8_Frame_t;typedef struct Minus__uMinus_var_u32_u8_Frame_t Minus__uMinus_var_u32_u8_Frame_t;
Word__T
__cdecl
Minus__uMinus_var_u32_u8(void);
 /* declare_local */
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct Minus__Minus_var_u32_u8_Frame_t;typedef struct Minus__Minus_var_u32_u8_Frame_t Minus__Minus_var_u32_u8_Frame_t;
Minus__UINT32
__cdecl
Minus__Minus_var_u32_u8(void);
 /* declare_local */
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct Minus__uMinus_param_u32_u8_Frame_t;typedef struct Minus__uMinus_param_u32_u8_Frame_t Minus__uMinus_param_u32_u8_Frame_t;
 /* declare_local */
 /* internal_declare_param */
 /* internal_declare_param */
Word__T
__cdecl
Minus__uMinus_param_u32_u8(
   /* Param_Type1 */ Minus__UINT32 a_L_971,
   /* Param_Type1 */ Minus__UINT8 b_L_972);
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct Minus__Minus_param_u32_u8_Frame_t;typedef struct Minus__Minus_param_u32_u8_Frame_t Minus__Minus_param_u32_u8_Frame_t;
 /* declare_local */
 /* internal_declare_param */
 /* internal_declare_param */
Minus__UINT32
__cdecl
Minus__Minus_param_u32_u8(
   /* Param_Type1 */ Minus__UINT32 a_L_974,
   /* Param_Type1 */ Minus__UINT8 b_L_975);
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct Minus__uMinus_var_u32_L_Frame_t;typedef struct Minus__uMinus_var_u32_L_Frame_t Minus__uMinus_var_u32_L_Frame_t;
Long__T
__cdecl
Minus__uMinus_var_u32_L(void);
 /* declare_local */
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct Minus__Minus_var_u32_L_Frame_t;typedef struct Minus__Minus_var_u32_L_Frame_t Minus__Minus_var_u32_L_Frame_t;
LONGINT
__cdecl
Minus__Minus_var_u32_L(void);
 /* declare_local */
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct Minus__uMinus_param_u32_L_Frame_t;typedef struct Minus__uMinus_param_u32_L_Frame_t Minus__uMinus_param_u32_L_Frame_t;
 /* declare_local */
 /* internal_declare_param */
 /* internal_declare_param */
Long__T
__cdecl
Minus__uMinus_param_u32_L(
   /* Param_Type1 */ Minus__UINT32 a_L_979,
   /* Param_Type1 */ LONGINT b_L_980);
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct Minus__Minus_param_u32_L_Frame_t;typedef struct Minus__Minus_param_u32_L_Frame_t Minus__Minus_param_u32_L_Frame_t;
 /* declare_local */
 /* internal_declare_param */
 /* internal_declare_param */
LONGINT
__cdecl
Minus__Minus_param_u32_L(
   /* Param_Type1 */ Minus__UINT32 a_L_982,
   /* Param_Type1 */ LONGINT b_L_983);
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct Minus__uMinus_var_u8_i8_Frame_t;typedef struct Minus__uMinus_var_u8_i8_Frame_t Minus__uMinus_var_u8_i8_Frame_t;
Word__T
__cdecl
Minus__uMinus_var_u8_i8(void);
 /* declare_local */
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct Minus__Minus_var_u8_i8_Frame_t;typedef struct Minus__Minus_var_u8_i8_Frame_t Minus__Minus_var_u8_i8_Frame_t;
Minus__UINT8
__cdecl
Minus__Minus_var_u8_i8(void);
 /* declare_local */
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct Minus__uMinus_param_u8_i8_Frame_t;typedef struct Minus__uMinus_param_u8_i8_Frame_t Minus__uMinus_param_u8_i8_Frame_t;
 /* declare_local */
 /* internal_declare_param */
 /* internal_declare_param */
Word__T
__cdecl
Minus__uMinus_param_u8_i8(
   /* Param_Type1 */ Minus__UINT8 a_L_987,
   /* Param_Type1 */ Minus__INT8 b_L_988);
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct Minus__Minus_param_u8_i8_Frame_t;typedef struct Minus__Minus_param_u8_i8_Frame_t Minus__Minus_param_u8_i8_Frame_t;
 /* declare_local */
 /* internal_declare_param */
 /* internal_declare_param */
Minus__UINT8
__cdecl
Minus__Minus_param_u8_i8(
   /* Param_Type1 */ Minus__UINT8 a_L_990,
   /* Param_Type1 */ Minus__INT8 b_L_991);
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct Minus__uMinus_var_u8_u64_Frame_t;typedef struct Minus__uMinus_var_u8_u64_Frame_t Minus__uMinus_var_u8_u64_Frame_t;
Long__T
__cdecl
Minus__uMinus_var_u8_u64(void);
 /* declare_local */
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct Minus__Minus_var_u8_u64_Frame_t;typedef struct Minus__Minus_var_u8_u64_Frame_t Minus__Minus_var_u8_u64_Frame_t;
LONGINT
__cdecl
Minus__Minus_var_u8_u64(void);
 /* declare_local */
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct Minus__uMinus_param_u8_u64_Frame_t;typedef struct Minus__uMinus_param_u8_u64_Frame_t Minus__uMinus_param_u8_u64_Frame_t;
 /* declare_local */
 /* internal_declare_param */
 /* internal_declare_param */
Long__T
__cdecl
Minus__uMinus_param_u8_u64(
   /* Param_Type1 */ Minus__UINT8 a_L_995,
   /* Param_Type1 */ Minus__UINT64 b_L_996);
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct Minus__Minus_param_u8_u64_Frame_t;typedef struct Minus__Minus_param_u8_u64_Frame_t Minus__Minus_param_u8_u64_Frame_t;
 /* declare_local */
 /* internal_declare_param */
 /* internal_declare_param */
LONGINT
__cdecl
Minus__Minus_param_u8_u64(
   /* Param_Type1 */ Minus__UINT8 a_L_998,
   /* Param_Type1 */ Minus__UINT64 b_L_999);
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct Minus__uMinus_var_u8_i32_Frame_t;typedef struct Minus__uMinus_var_u8_i32_Frame_t Minus__uMinus_var_u8_i32_Frame_t;
Word__T
__cdecl
Minus__uMinus_var_u8_i32(void);
 /* declare_local */
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct Minus__Minus_var_u8_i32_Frame_t;typedef struct Minus__Minus_var_u8_i32_Frame_t Minus__Minus_var_u8_i32_Frame_t;
Minus__UINT8
__cdecl
Minus__Minus_var_u8_i32(void);
 /* declare_local */
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct Minus__uMinus_param_u8_i32_Frame_t;typedef struct Minus__uMinus_param_u8_i32_Frame_t Minus__uMinus_param_u8_i32_Frame_t;
 /* declare_local */
 /* internal_declare_param */
 /* internal_declare_param */
Word__T
__cdecl
Minus__uMinus_param_u8_i32(
   /* Param_Type1 */ Minus__UINT8 a_L_1003,
   /* Param_Type1 */ Minus__INT32 b_L_1004);
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct Minus__Minus_param_u8_i32_Frame_t;typedef struct Minus__Minus_param_u8_i32_Frame_t Minus__Minus_param_u8_i32_Frame_t;
 /* declare_local */
 /* internal_declare_param */
 /* internal_declare_param */
Minus__UINT8
__cdecl
Minus__Minus_param_u8_i32(
   /* Param_Type1 */ Minus__UINT8 a_L_1006,
   /* Param_Type1 */ Minus__INT32 b_L_1007);
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct Minus__uMinus_var_u8_LC_Frame_t;typedef struct Minus__uMinus_var_u8_LC_Frame_t Minus__uMinus_var_u8_LC_Frame_t;
Long__T
__cdecl
Minus__uMinus_var_u8_LC(void);
 /* declare_local */
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct Minus__Minus_var_u8_LC_Frame_t;typedef struct Minus__Minus_var_u8_LC_Frame_t Minus__Minus_var_u8_LC_Frame_t;
LONGINT
__cdecl
Minus__Minus_var_u8_LC(void);
 /* declare_local */
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct Minus__uMinus_param_u8_LC_Frame_t;typedef struct Minus__uMinus_param_u8_LC_Frame_t Minus__uMinus_param_u8_LC_Frame_t;
 /* declare_local */
 /* internal_declare_param */
 /* internal_declare_param */
Long__T
__cdecl
Minus__uMinus_param_u8_LC(
   /* Param_Type1 */ Minus__UINT8 a_L_1011,
   /* Param_Type1 */ LONGCARD b_L_1012);
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct Minus__Minus_param_u8_LC_Frame_t;typedef struct Minus__Minus_param_u8_LC_Frame_t Minus__Minus_param_u8_LC_Frame_t;
 /* declare_local */
 /* internal_declare_param */
 /* internal_declare_param */
LONGINT
__cdecl
Minus__Minus_param_u8_LC(
   /* Param_Type1 */ Minus__UINT8 a_L_1014,
   /* Param_Type1 */ LONGCARD b_L_1015);
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct Minus__uMinus_var_u8_u16_Frame_t;typedef struct Minus__uMinus_var_u8_u16_Frame_t Minus__uMinus_var_u8_u16_Frame_t;
Word__T
__cdecl
Minus__uMinus_var_u8_u16(void);
 /* declare_local */
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct Minus__Minus_var_u8_u16_Frame_t;typedef struct Minus__Minus_var_u8_u16_Frame_t Minus__Minus_var_u8_u16_Frame_t;
Minus__UINT8
__cdecl
Minus__Minus_var_u8_u16(void);
 /* declare_local */
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct Minus__uMinus_param_u8_u16_Frame_t;typedef struct Minus__uMinus_param_u8_u16_Frame_t Minus__uMinus_param_u8_u16_Frame_t;
 /* declare_local */
 /* internal_declare_param */
 /* internal_declare_param */
Word__T
__cdecl
Minus__uMinus_param_u8_u16(
   /* Param_Type1 */ Minus__UINT8 a_L_1019,
   /* Param_Type1 */ Minus__UINT16 b_L_1020);
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct Minus__Minus_param_u8_u16_Frame_t;typedef struct Minus__Minus_param_u8_u16_Frame_t Minus__Minus_param_u8_u16_Frame_t;
 /* declare_local */
 /* internal_declare_param */
 /* internal_declare_param */
Minus__UINT8
__cdecl
Minus__Minus_param_u8_u16(
   /* Param_Type1 */ Minus__UINT8 a_L_1022,
   /* Param_Type1 */ Minus__UINT16 b_L_1023);
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct Minus__uMinus_var_u8_I_Frame_t;typedef struct Minus__uMinus_var_u8_I_Frame_t Minus__uMinus_var_u8_I_Frame_t;
Word__T
__cdecl
Minus__uMinus_var_u8_I(void);
 /* declare_local */
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct Minus__Minus_var_u8_I_Frame_t;typedef struct Minus__Minus_var_u8_I_Frame_t Minus__Minus_var_u8_I_Frame_t;
Minus__UINT8
__cdecl
Minus__Minus_var_u8_I(void);
 /* declare_local */
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct Minus__uMinus_param_u8_I_Frame_t;typedef struct Minus__uMinus_param_u8_I_Frame_t Minus__uMinus_param_u8_I_Frame_t;
 /* declare_local */
 /* internal_declare_param */
 /* internal_declare_param */
Word__T
__cdecl
Minus__uMinus_param_u8_I(
   /* Param_Type1 */ Minus__UINT8 a_L_1027,
   /* Param_Type1 */ INTEGER b_L_1028);
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct Minus__Minus_param_u8_I_Frame_t;typedef struct Minus__Minus_param_u8_I_Frame_t Minus__Minus_param_u8_I_Frame_t;
 /* declare_local */
 /* internal_declare_param */
 /* internal_declare_param */
Minus__UINT8
__cdecl
Minus__Minus_param_u8_I(
   /* Param_Type1 */ Minus__UINT8 a_L_1030,
   /* Param_Type1 */ INTEGER b_L_1031);
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct Minus__uMinus_var_u8_i64_Frame_t;typedef struct Minus__uMinus_var_u8_i64_Frame_t Minus__uMinus_var_u8_i64_Frame_t;
Long__T
__cdecl
Minus__uMinus_var_u8_i64(void);
 /* declare_local */
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct Minus__Minus_var_u8_i64_Frame_t;typedef struct Minus__Minus_var_u8_i64_Frame_t Minus__Minus_var_u8_i64_Frame_t;
LONGINT
__cdecl
Minus__Minus_var_u8_i64(void);
 /* declare_local */
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct Minus__uMinus_param_u8_i64_Frame_t;typedef struct Minus__uMinus_param_u8_i64_Frame_t Minus__uMinus_param_u8_i64_Frame_t;
 /* declare_local */
 /* internal_declare_param */
 /* internal_declare_param */
Long__T
__cdecl
Minus__uMinus_param_u8_i64(
   /* Param_Type1 */ Minus__UINT8 a_L_1035,
   /* Param_Type1 */ Minus__INT64 b_L_1036);
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct Minus__Minus_param_u8_i64_Frame_t;typedef struct Minus__Minus_param_u8_i64_Frame_t Minus__Minus_param_u8_i64_Frame_t;
 /* declare_local */
 /* internal_declare_param */
 /* internal_declare_param */
LONGINT
__cdecl
Minus__Minus_param_u8_i64(
   /* Param_Type1 */ Minus__UINT8 a_L_1038,
   /* Param_Type1 */ Minus__INT64 b_L_1039);
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct Minus__uMinus_var_u8_i16_Frame_t;typedef struct Minus__uMinus_var_u8_i16_Frame_t Minus__uMinus_var_u8_i16_Frame_t;
Word__T
__cdecl
Minus__uMinus_var_u8_i16(void);
 /* declare_local */
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct Minus__Minus_var_u8_i16_Frame_t;typedef struct Minus__Minus_var_u8_i16_Frame_t Minus__Minus_var_u8_i16_Frame_t;
Minus__UINT8
__cdecl
Minus__Minus_var_u8_i16(void);
 /* declare_local */
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct Minus__uMinus_param_u8_i16_Frame_t;typedef struct Minus__uMinus_param_u8_i16_Frame_t Minus__uMinus_param_u8_i16_Frame_t;
 /* declare_local */
 /* internal_declare_param */
 /* internal_declare_param */
Word__T
__cdecl
Minus__uMinus_param_u8_i16(
   /* Param_Type1 */ Minus__UINT8 a_L_1043,
   /* Param_Type1 */ Minus__INT16 b_L_1044);
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct Minus__Minus_param_u8_i16_Frame_t;typedef struct Minus__Minus_param_u8_i16_Frame_t Minus__Minus_param_u8_i16_Frame_t;
 /* declare_local */
 /* internal_declare_param */
 /* internal_declare_param */
Minus__UINT8
__cdecl
Minus__Minus_param_u8_i16(
   /* Param_Type1 */ Minus__UINT8 a_L_1046,
   /* Param_Type1 */ Minus__INT16 b_L_1047);
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct Minus__uMinus_var_u8_C_Frame_t;typedef struct Minus__uMinus_var_u8_C_Frame_t Minus__uMinus_var_u8_C_Frame_t;
Word__T
__cdecl
Minus__uMinus_var_u8_C(void);
 /* declare_local */
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct Minus__Minus_var_u8_C_Frame_t;typedef struct Minus__Minus_var_u8_C_Frame_t Minus__Minus_var_u8_C_Frame_t;
Minus__UINT8
__cdecl
Minus__Minus_var_u8_C(void);
 /* declare_local */
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct Minus__uMinus_param_u8_C_Frame_t;typedef struct Minus__uMinus_param_u8_C_Frame_t Minus__uMinus_param_u8_C_Frame_t;
 /* declare_local */
 /* internal_declare_param */
 /* internal_declare_param */
Word__T
__cdecl
Minus__uMinus_param_u8_C(
   /* Param_Type1 */ Minus__UINT8 a_L_1051,
   /* Param_Type1 */ CARDINAL b_L_1052);
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct Minus__Minus_param_u8_C_Frame_t;typedef struct Minus__Minus_param_u8_C_Frame_t Minus__Minus_param_u8_C_Frame_t;
 /* declare_local */
 /* internal_declare_param */
 /* internal_declare_param */
Minus__UINT8
__cdecl
Minus__Minus_param_u8_C(
   /* Param_Type1 */ Minus__UINT8 a_L_1054,
   /* Param_Type1 */ CARDINAL b_L_1055);
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct Minus__uMinus_var_u8_u32_Frame_t;typedef struct Minus__uMinus_var_u8_u32_Frame_t Minus__uMinus_var_u8_u32_Frame_t;
Word__T
__cdecl
Minus__uMinus_var_u8_u32(void);
 /* declare_local */
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct Minus__Minus_var_u8_u32_Frame_t;typedef struct Minus__Minus_var_u8_u32_Frame_t Minus__Minus_var_u8_u32_Frame_t;
Minus__UINT8
__cdecl
Minus__Minus_var_u8_u32(void);
 /* declare_local */
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct Minus__uMinus_param_u8_u32_Frame_t;typedef struct Minus__uMinus_param_u8_u32_Frame_t Minus__uMinus_param_u8_u32_Frame_t;
 /* declare_local */
 /* internal_declare_param */
 /* internal_declare_param */
Word__T
__cdecl
Minus__uMinus_param_u8_u32(
   /* Param_Type1 */ Minus__UINT8 a_L_1059,
   /* Param_Type1 */ Minus__UINT32 b_L_1060);
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct Minus__Minus_param_u8_u32_Frame_t;typedef struct Minus__Minus_param_u8_u32_Frame_t Minus__Minus_param_u8_u32_Frame_t;
 /* declare_local */
 /* internal_declare_param */
 /* internal_declare_param */
Minus__UINT8
__cdecl
Minus__Minus_param_u8_u32(
   /* Param_Type1 */ Minus__UINT8 a_L_1062,
   /* Param_Type1 */ Minus__UINT32 b_L_1063);
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct Minus__uMinus_var_u8_u8_Frame_t;typedef struct Minus__uMinus_var_u8_u8_Frame_t Minus__uMinus_var_u8_u8_Frame_t;
Word__T
__cdecl
Minus__uMinus_var_u8_u8(void);
 /* declare_local */
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct Minus__Minus_var_u8_u8_Frame_t;typedef struct Minus__Minus_var_u8_u8_Frame_t Minus__Minus_var_u8_u8_Frame_t;
Minus__UINT8
__cdecl
Minus__Minus_var_u8_u8(void);
 /* declare_local */
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct Minus__uMinus_param_u8_u8_Frame_t;typedef struct Minus__uMinus_param_u8_u8_Frame_t Minus__uMinus_param_u8_u8_Frame_t;
 /* declare_local */
 /* internal_declare_param */
 /* internal_declare_param */
Word__T
__cdecl
Minus__uMinus_param_u8_u8(
   /* Param_Type1 */ Minus__UINT8 a_L_1067,
   /* Param_Type1 */ Minus__UINT8 b_L_1068);
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct Minus__Minus_param_u8_u8_Frame_t;typedef struct Minus__Minus_param_u8_u8_Frame_t Minus__Minus_param_u8_u8_Frame_t;
 /* declare_local */
 /* internal_declare_param */
 /* internal_declare_param */
Minus__UINT8
__cdecl
Minus__Minus_param_u8_u8(
   /* Param_Type1 */ Minus__UINT8 a_L_1070,
   /* Param_Type1 */ Minus__UINT8 b_L_1071);
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct Minus__uMinus_var_u8_L_Frame_t;typedef struct Minus__uMinus_var_u8_L_Frame_t Minus__uMinus_var_u8_L_Frame_t;
Long__T
__cdecl
Minus__uMinus_var_u8_L(void);
 /* declare_local */
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct Minus__Minus_var_u8_L_Frame_t;typedef struct Minus__Minus_var_u8_L_Frame_t Minus__Minus_var_u8_L_Frame_t;
LONGINT
__cdecl
Minus__Minus_var_u8_L(void);
 /* declare_local */
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct Minus__uMinus_param_u8_L_Frame_t;typedef struct Minus__uMinus_param_u8_L_Frame_t Minus__uMinus_param_u8_L_Frame_t;
 /* declare_local */
 /* internal_declare_param */
 /* internal_declare_param */
Long__T
__cdecl
Minus__uMinus_param_u8_L(
   /* Param_Type1 */ Minus__UINT8 a_L_1075,
   /* Param_Type1 */ LONGINT b_L_1076);
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct Minus__Minus_param_u8_L_Frame_t;typedef struct Minus__Minus_param_u8_L_Frame_t Minus__Minus_param_u8_L_Frame_t;
 /* declare_local */
 /* internal_declare_param */
 /* internal_declare_param */
LONGINT
__cdecl
Minus__Minus_param_u8_L(
   /* Param_Type1 */ Minus__UINT8 a_L_1078,
   /* Param_Type1 */ LONGINT b_L_1079);
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct Minus__uMinus_var_L_i8_Frame_t;typedef struct Minus__uMinus_var_L_i8_Frame_t Minus__uMinus_var_L_i8_Frame_t;
Long__T
__cdecl
Minus__uMinus_var_L_i8(void);
 /* declare_local */
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct Minus__Minus_var_L_i8_Frame_t;typedef struct Minus__Minus_var_L_i8_Frame_t Minus__Minus_var_L_i8_Frame_t;
LONGINT
__cdecl
Minus__Minus_var_L_i8(void);
 /* declare_local */
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct Minus__uMinus_param_L_i8_Frame_t;typedef struct Minus__uMinus_param_L_i8_Frame_t Minus__uMinus_param_L_i8_Frame_t;
 /* declare_local */
 /* internal_declare_param */
 /* internal_declare_param */
Long__T
__cdecl
Minus__uMinus_param_L_i8(
   /* Param_Type1 */ LONGINT a_L_1083,
   /* Param_Type1 */ Minus__INT8 b_L_1084);
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct Minus__Minus_param_L_i8_Frame_t;typedef struct Minus__Minus_param_L_i8_Frame_t Minus__Minus_param_L_i8_Frame_t;
 /* declare_local */
 /* internal_declare_param */
 /* internal_declare_param */
LONGINT
__cdecl
Minus__Minus_param_L_i8(
   /* Param_Type1 */ LONGINT a_L_1086,
   /* Param_Type1 */ Minus__INT8 b_L_1087);
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct Minus__uMinus_var_L_u64_Frame_t;typedef struct Minus__uMinus_var_L_u64_Frame_t Minus__uMinus_var_L_u64_Frame_t;
Long__T
__cdecl
Minus__uMinus_var_L_u64(void);
 /* declare_local */
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct Minus__Minus_var_L_u64_Frame_t;typedef struct Minus__Minus_var_L_u64_Frame_t Minus__Minus_var_L_u64_Frame_t;
LONGINT
__cdecl
Minus__Minus_var_L_u64(void);
 /* declare_local */
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct Minus__uMinus_param_L_u64_Frame_t;typedef struct Minus__uMinus_param_L_u64_Frame_t Minus__uMinus_param_L_u64_Frame_t;
 /* declare_local */
 /* internal_declare_param */
 /* internal_declare_param */
Long__T
__cdecl
Minus__uMinus_param_L_u64(
   /* Param_Type1 */ LONGINT a_L_1091,
   /* Param_Type1 */ Minus__UINT64 b_L_1092);
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct Minus__Minus_param_L_u64_Frame_t;typedef struct Minus__Minus_param_L_u64_Frame_t Minus__Minus_param_L_u64_Frame_t;
 /* declare_local */
 /* internal_declare_param */
 /* internal_declare_param */
LONGINT
__cdecl
Minus__Minus_param_L_u64(
   /* Param_Type1 */ LONGINT a_L_1094,
   /* Param_Type1 */ Minus__UINT64 b_L_1095);
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct Minus__uMinus_var_L_i32_Frame_t;typedef struct Minus__uMinus_var_L_i32_Frame_t Minus__uMinus_var_L_i32_Frame_t;
Long__T
__cdecl
Minus__uMinus_var_L_i32(void);
 /* declare_local */
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct Minus__Minus_var_L_i32_Frame_t;typedef struct Minus__Minus_var_L_i32_Frame_t Minus__Minus_var_L_i32_Frame_t;
LONGINT
__cdecl
Minus__Minus_var_L_i32(void);
 /* declare_local */
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct Minus__uMinus_param_L_i32_Frame_t;typedef struct Minus__uMinus_param_L_i32_Frame_t Minus__uMinus_param_L_i32_Frame_t;
 /* declare_local */
 /* internal_declare_param */
 /* internal_declare_param */
Long__T
__cdecl
Minus__uMinus_param_L_i32(
   /* Param_Type1 */ LONGINT a_L_1099,
   /* Param_Type1 */ Minus__INT32 b_L_1100);
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct Minus__Minus_param_L_i32_Frame_t;typedef struct Minus__Minus_param_L_i32_Frame_t Minus__Minus_param_L_i32_Frame_t;
 /* declare_local */
 /* internal_declare_param */
 /* internal_declare_param */
LONGINT
__cdecl
Minus__Minus_param_L_i32(
   /* Param_Type1 */ LONGINT a_L_1102,
   /* Param_Type1 */ Minus__INT32 b_L_1103);
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct Minus__uMinus_var_L_LC_Frame_t;typedef struct Minus__uMinus_var_L_LC_Frame_t Minus__uMinus_var_L_LC_Frame_t;
Long__T
__cdecl
Minus__uMinus_var_L_LC(void);
 /* declare_local */
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct Minus__Minus_var_L_LC_Frame_t;typedef struct Minus__Minus_var_L_LC_Frame_t Minus__Minus_var_L_LC_Frame_t;
LONGINT
__cdecl
Minus__Minus_var_L_LC(void);
 /* declare_local */
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct Minus__uMinus_param_L_LC_Frame_t;typedef struct Minus__uMinus_param_L_LC_Frame_t Minus__uMinus_param_L_LC_Frame_t;
 /* declare_local */
 /* internal_declare_param */
 /* internal_declare_param */
Long__T
__cdecl
Minus__uMinus_param_L_LC(
   /* Param_Type1 */ LONGINT a_L_1107,
   /* Param_Type1 */ LONGCARD b_L_1108);
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct Minus__Minus_param_L_LC_Frame_t;typedef struct Minus__Minus_param_L_LC_Frame_t Minus__Minus_param_L_LC_Frame_t;
 /* declare_local */
 /* internal_declare_param */
 /* internal_declare_param */
LONGINT
__cdecl
Minus__Minus_param_L_LC(
   /* Param_Type1 */ LONGINT a_L_1110,
   /* Param_Type1 */ LONGCARD b_L_1111);
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct Minus__uMinus_var_L_u16_Frame_t;typedef struct Minus__uMinus_var_L_u16_Frame_t Minus__uMinus_var_L_u16_Frame_t;
Long__T
__cdecl
Minus__uMinus_var_L_u16(void);
 /* declare_local */
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct Minus__Minus_var_L_u16_Frame_t;typedef struct Minus__Minus_var_L_u16_Frame_t Minus__Minus_var_L_u16_Frame_t;
LONGINT
__cdecl
Minus__Minus_var_L_u16(void);
 /* declare_local */
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct Minus__uMinus_param_L_u16_Frame_t;typedef struct Minus__uMinus_param_L_u16_Frame_t Minus__uMinus_param_L_u16_Frame_t;
 /* declare_local */
 /* internal_declare_param */
 /* internal_declare_param */
Long__T
__cdecl
Minus__uMinus_param_L_u16(
   /* Param_Type1 */ LONGINT a_L_1115,
   /* Param_Type1 */ Minus__UINT16 b_L_1116);
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct Minus__Minus_param_L_u16_Frame_t;typedef struct Minus__Minus_param_L_u16_Frame_t Minus__Minus_param_L_u16_Frame_t;
 /* declare_local */
 /* internal_declare_param */
 /* internal_declare_param */
LONGINT
__cdecl
Minus__Minus_param_L_u16(
   /* Param_Type1 */ LONGINT a_L_1118,
   /* Param_Type1 */ Minus__UINT16 b_L_1119);
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct Minus__uMinus_var_L_I_Frame_t;typedef struct Minus__uMinus_var_L_I_Frame_t Minus__uMinus_var_L_I_Frame_t;
Long__T
__cdecl
Minus__uMinus_var_L_I(void);
 /* declare_local */
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct Minus__Minus_var_L_I_Frame_t;typedef struct Minus__Minus_var_L_I_Frame_t Minus__Minus_var_L_I_Frame_t;
LONGINT
__cdecl
Minus__Minus_var_L_I(void);
 /* declare_local */
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct Minus__uMinus_param_L_I_Frame_t;typedef struct Minus__uMinus_param_L_I_Frame_t Minus__uMinus_param_L_I_Frame_t;
 /* declare_local */
 /* internal_declare_param */
 /* internal_declare_param */
Long__T
__cdecl
Minus__uMinus_param_L_I(
   /* Param_Type1 */ LONGINT a_L_1123,
   /* Param_Type1 */ INTEGER b_L_1124);
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct Minus__Minus_param_L_I_Frame_t;typedef struct Minus__Minus_param_L_I_Frame_t Minus__Minus_param_L_I_Frame_t;
 /* declare_local */
 /* internal_declare_param */
 /* internal_declare_param */
LONGINT
__cdecl
Minus__Minus_param_L_I(
   /* Param_Type1 */ LONGINT a_L_1126,
   /* Param_Type1 */ INTEGER b_L_1127);
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct Minus__uMinus_var_L_i64_Frame_t;typedef struct Minus__uMinus_var_L_i64_Frame_t Minus__uMinus_var_L_i64_Frame_t;
Long__T
__cdecl
Minus__uMinus_var_L_i64(void);
 /* declare_local */
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct Minus__Minus_var_L_i64_Frame_t;typedef struct Minus__Minus_var_L_i64_Frame_t Minus__Minus_var_L_i64_Frame_t;
LONGINT
__cdecl
Minus__Minus_var_L_i64(void);
 /* declare_local */
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct Minus__uMinus_param_L_i64_Frame_t;typedef struct Minus__uMinus_param_L_i64_Frame_t Minus__uMinus_param_L_i64_Frame_t;
 /* declare_local */
 /* internal_declare_param */
 /* internal_declare_param */
Long__T
__cdecl
Minus__uMinus_param_L_i64(
   /* Param_Type1 */ LONGINT a_L_1131,
   /* Param_Type1 */ Minus__INT64 b_L_1132);
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct Minus__Minus_param_L_i64_Frame_t;typedef struct Minus__Minus_param_L_i64_Frame_t Minus__Minus_param_L_i64_Frame_t;
 /* declare_local */
 /* internal_declare_param */
 /* internal_declare_param */
LONGINT
__cdecl
Minus__Minus_param_L_i64(
   /* Param_Type1 */ LONGINT a_L_1134,
   /* Param_Type1 */ Minus__INT64 b_L_1135);
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct Minus__uMinus_var_L_i16_Frame_t;typedef struct Minus__uMinus_var_L_i16_Frame_t Minus__uMinus_var_L_i16_Frame_t;
Long__T
__cdecl
Minus__uMinus_var_L_i16(void);
 /* declare_local */
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct Minus__Minus_var_L_i16_Frame_t;typedef struct Minus__Minus_var_L_i16_Frame_t Minus__Minus_var_L_i16_Frame_t;
LONGINT
__cdecl
Minus__Minus_var_L_i16(void);
 /* declare_local */
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct Minus__uMinus_param_L_i16_Frame_t;typedef struct Minus__uMinus_param_L_i16_Frame_t Minus__uMinus_param_L_i16_Frame_t;
 /* declare_local */
 /* internal_declare_param */
 /* internal_declare_param */
Long__T
__cdecl
Minus__uMinus_param_L_i16(
   /* Param_Type1 */ LONGINT a_L_1139,
   /* Param_Type1 */ Minus__INT16 b_L_1140);
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct Minus__Minus_param_L_i16_Frame_t;typedef struct Minus__Minus_param_L_i16_Frame_t Minus__Minus_param_L_i16_Frame_t;
 /* declare_local */
 /* internal_declare_param */
 /* internal_declare_param */
LONGINT
__cdecl
Minus__Minus_param_L_i16(
   /* Param_Type1 */ LONGINT a_L_1142,
   /* Param_Type1 */ Minus__INT16 b_L_1143);
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct Minus__uMinus_var_L_C_Frame_t;typedef struct Minus__uMinus_var_L_C_Frame_t Minus__uMinus_var_L_C_Frame_t;
Long__T
__cdecl
Minus__uMinus_var_L_C(void);
 /* declare_local */
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct Minus__Minus_var_L_C_Frame_t;typedef struct Minus__Minus_var_L_C_Frame_t Minus__Minus_var_L_C_Frame_t;
LONGINT
__cdecl
Minus__Minus_var_L_C(void);
 /* declare_local */
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct Minus__uMinus_param_L_C_Frame_t;typedef struct Minus__uMinus_param_L_C_Frame_t Minus__uMinus_param_L_C_Frame_t;
 /* declare_local */
 /* internal_declare_param */
 /* internal_declare_param */
Long__T
__cdecl
Minus__uMinus_param_L_C(
   /* Param_Type1 */ LONGINT a_L_1147,
   /* Param_Type1 */ CARDINAL b_L_1148);
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct Minus__Minus_param_L_C_Frame_t;typedef struct Minus__Minus_param_L_C_Frame_t Minus__Minus_param_L_C_Frame_t;
 /* declare_local */
 /* internal_declare_param */
 /* internal_declare_param */
LONGINT
__cdecl
Minus__Minus_param_L_C(
   /* Param_Type1 */ LONGINT a_L_1150,
   /* Param_Type1 */ CARDINAL b_L_1151);
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct Minus__uMinus_var_L_u32_Frame_t;typedef struct Minus__uMinus_var_L_u32_Frame_t Minus__uMinus_var_L_u32_Frame_t;
Long__T
__cdecl
Minus__uMinus_var_L_u32(void);
 /* declare_local */
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct Minus__Minus_var_L_u32_Frame_t;typedef struct Minus__Minus_var_L_u32_Frame_t Minus__Minus_var_L_u32_Frame_t;
LONGINT
__cdecl
Minus__Minus_var_L_u32(void);
 /* declare_local */
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct Minus__uMinus_param_L_u32_Frame_t;typedef struct Minus__uMinus_param_L_u32_Frame_t Minus__uMinus_param_L_u32_Frame_t;
 /* declare_local */
 /* internal_declare_param */
 /* internal_declare_param */
Long__T
__cdecl
Minus__uMinus_param_L_u32(
   /* Param_Type1 */ LONGINT a_L_1155,
   /* Param_Type1 */ Minus__UINT32 b_L_1156);
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct Minus__Minus_param_L_u32_Frame_t;typedef struct Minus__Minus_param_L_u32_Frame_t Minus__Minus_param_L_u32_Frame_t;
 /* declare_local */
 /* internal_declare_param */
 /* internal_declare_param */
LONGINT
__cdecl
Minus__Minus_param_L_u32(
   /* Param_Type1 */ LONGINT a_L_1158,
   /* Param_Type1 */ Minus__UINT32 b_L_1159);
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct Minus__uMinus_var_L_u8_Frame_t;typedef struct Minus__uMinus_var_L_u8_Frame_t Minus__uMinus_var_L_u8_Frame_t;
Long__T
__cdecl
Minus__uMinus_var_L_u8(void);
 /* declare_local */
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct Minus__Minus_var_L_u8_Frame_t;typedef struct Minus__Minus_var_L_u8_Frame_t Minus__Minus_var_L_u8_Frame_t;
LONGINT
__cdecl
Minus__Minus_var_L_u8(void);
 /* declare_local */
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct Minus__uMinus_param_L_u8_Frame_t;typedef struct Minus__uMinus_param_L_u8_Frame_t Minus__uMinus_param_L_u8_Frame_t;
 /* declare_local */
 /* internal_declare_param */
 /* internal_declare_param */
Long__T
__cdecl
Minus__uMinus_param_L_u8(
   /* Param_Type1 */ LONGINT a_L_1163,
   /* Param_Type1 */ Minus__UINT8 b_L_1164);
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct Minus__Minus_param_L_u8_Frame_t;typedef struct Minus__Minus_param_L_u8_Frame_t Minus__Minus_param_L_u8_Frame_t;
 /* declare_local */
 /* internal_declare_param */
 /* internal_declare_param */
LONGINT
__cdecl
Minus__Minus_param_L_u8(
   /* Param_Type1 */ LONGINT a_L_1166,
   /* Param_Type1 */ Minus__UINT8 b_L_1167);
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct Minus__uMinus_var_L_L_Frame_t;typedef struct Minus__uMinus_var_L_L_Frame_t Minus__uMinus_var_L_L_Frame_t;
Long__T
__cdecl
Minus__uMinus_var_L_L(void);
 /* declare_local */
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct Minus__Minus_var_L_L_Frame_t;typedef struct Minus__Minus_var_L_L_Frame_t Minus__Minus_var_L_L_Frame_t;
LONGINT
__cdecl
Minus__Minus_var_L_L(void);
 /* declare_local */
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct Minus__uMinus_param_L_L_Frame_t;typedef struct Minus__uMinus_param_L_L_Frame_t Minus__uMinus_param_L_L_Frame_t;
 /* declare_local */
 /* internal_declare_param */
 /* internal_declare_param */
Long__T
__cdecl
Minus__uMinus_param_L_L(
   /* Param_Type1 */ LONGINT a_L_1171,
   /* Param_Type1 */ LONGINT b_L_1172);
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct Minus__Minus_param_L_L_Frame_t;typedef struct Minus__Minus_param_L_L_Frame_t Minus__Minus_param_L_L_Frame_t;
 /* declare_local */
 /* internal_declare_param */
 /* internal_declare_param */
LONGINT
__cdecl
Minus__Minus_param_L_L(
   /* Param_Type1 */ LONGINT a_L_1174,
   /* Param_Type1 */ LONGINT b_L_1175);
 /* Locals_begin_procedure */
 /* begin_block */
 /* Locals_end_procedure */
 /* end_block */
 /* Locals_begin_procedure */
 /* begin_block */
 /* AllocateTemps_check_range */
 /* AllocateTemps_common */
 /* declare_temp */
 /* declare_local */
 /* Locals_end_procedure */
 /* end_block */
 /* Locals_begin_procedure */
 /* begin_block */
 /* Locals_end_procedure */
 /* end_block */
 /* Locals_begin_procedure */
 /* begin_block */
 /* AllocateTemps_check_range */
 /* AllocateTemps_common */
 /* declare_temp */
 /* declare_local */
 /* Locals_end_procedure */
 /* end_block */
 /* Locals_begin_procedure */
 /* begin_block */
 /* Locals_end_procedure */
 /* end_block */
 /* Locals_begin_procedure */
 /* begin_block */
 /* Locals_end_procedure */
 /* end_block */
 /* Locals_begin_procedure */
 /* begin_block */
 /* Locals_end_procedure */
 /* end_block */
 /* Locals_begin_procedure */
 /* begin_block */
 /* Locals_end_procedure */
 /* end_block */
 /* Locals_begin_procedure */
 /* begin_block */
 /* Locals_end_procedure */
 /* end_block */
 /* Locals_begin_procedure */
 /* begin_block */
 /* AllocateTemps_check_range */
 /* AllocateTemps_common */
 /* declare_temp */
 /* declare_local */
 /* Locals_end_procedure */
 /* end_block */
 /* Locals_begin_procedure */
 /* begin_block */
 /* Locals_end_procedure */
 /* end_block */
 /* Locals_begin_procedure */
 /* begin_block */
 /* AllocateTemps_check_range */
 /* AllocateTemps_common */
 /* declare_temp */
 /* declare_local */
 /* Locals_end_procedure */
 /* end_block */
 /* Locals_begin_procedure */
 /* begin_block */
 /* Locals_end_procedure */
 /* end_block */
 /* Locals_begin_procedure */
 /* begin_block */
 /* Locals_end_procedure */
 /* end_block */
 /* Locals_begin_procedure */
 /* begin_block */
 /* Locals_end_procedure */
 /* end_block */
 /* Locals_begin_procedure */
 /* begin_block */
 /* Locals_end_procedure */
 /* end_block */
 /* Locals_begin_procedure */
 /* begin_block */
 /* Locals_end_procedure */
 /* end_block */
 /* Locals_begin_procedure */
 /* begin_block */
 /* AllocateTemps_check_lo */
 /* AllocateTemps_common */
 /* declare_temp */
 /* declare_local */
 /* Locals_end_procedure */
 /* end_block */
 /* Locals_begin_procedure */
 /* begin_block */
 /* Locals_end_procedure */
 /* end_block */
 /* Locals_begin_procedure */
 /* begin_block */
 /* AllocateTemps_check_lo */
 /* AllocateTemps_common */
 /* declare_temp */
 /* declare_local */
 /* Locals_end_procedure */
 /* end_block */
 /* Locals_begin_procedure */
 /* begin_block */
 /* Locals_end_procedure */
 /* end_block */
 /* Locals_begin_procedure */
 /* begin_block */
 /* AllocateTemps_check_range */
 /* AllocateTemps_common */
 /* declare_temp */
 /* declare_local */
 /* Locals_end_procedure */
 /* end_block */
 /* Locals_begin_procedure */
 /* begin_block */
 /* Locals_end_procedure */
 /* end_block */
 /* Locals_begin_procedure */
 /* begin_block */
 /* AllocateTemps_check_range */
 /* AllocateTemps_common */
 /* declare_temp */
 /* declare_local */
 /* Locals_end_procedure */
 /* end_block */
 /* Locals_begin_procedure */
 /* begin_block */
 /* Locals_end_procedure */
 /* end_block */
 /* Locals_begin_procedure */
 /* begin_block */
 /* Locals_end_procedure */
 /* end_block */
 /* Locals_begin_procedure */
 /* begin_block */
 /* Locals_end_procedure */
 /* end_block */
 /* Locals_begin_procedure */
 /* begin_block */
 /* Locals_end_procedure */
 /* end_block */
 /* Locals_begin_procedure */
 /* begin_block */
 /* Locals_end_procedure */
 /* end_block */
 /* Locals_begin_procedure */
 /* begin_block */
 /* AllocateTemps_check_range */
 /* AllocateTemps_common */
 /* declare_temp */
 /* declare_local */
 /* Locals_end_procedure */
 /* end_block */
 /* Locals_begin_procedure */
 /* begin_block */
 /* Locals_end_procedure */
 /* end_block */
 /* Locals_begin_procedure */
 /* begin_block */
 /* AllocateTemps_check_range */
 /* AllocateTemps_common */
 /* declare_temp */
 /* declare_local */
 /* Locals_end_procedure */
 /* end_block */
 /* Locals_begin_procedure */
 /* begin_block */
 /* Locals_end_procedure */
 /* end_block */
 /* Locals_begin_procedure */
 /* begin_block */
 /* AllocateTemps_check_range */
 /* AllocateTemps_common */
 /* declare_temp */
 /* declare_local */
 /* Locals_end_procedure */
 /* end_block */
 /* Locals_begin_procedure */
 /* begin_block */
 /* Locals_end_procedure */
 /* end_block */
 /* Locals_begin_procedure */
 /* begin_block */
 /* AllocateTemps_check_range */
 /* AllocateTemps_common */
 /* declare_temp */
 /* declare_local */
 /* Locals_end_procedure */
 /* end_block */
 /* Locals_begin_procedure */
 /* begin_block */
 /* Locals_end_procedure */
 /* end_block */
 /* Locals_begin_procedure */
 /* begin_block */
 /* AllocateTemps_check_lo */
 /* AllocateTemps_common */
 /* declare_temp */
 /* declare_local */
 /* Locals_end_procedure */
 /* end_block */
 /* Locals_begin_procedure */
 /* begin_block */
 /* Locals_end_procedure */
 /* end_block */
 /* Locals_begin_procedure */
 /* begin_block */
 /* AllocateTemps_check_lo */
 /* AllocateTemps_common */
 /* declare_temp */
 /* declare_local */
 /* Locals_end_procedure */
 /* end_block */
 /* Locals_begin_procedure */
 /* begin_block */
 /* Locals_end_procedure */
 /* end_block */
 /* Locals_begin_procedure */
 /* begin_block */
 /* AllocateTemps_check_lo */
 /* AllocateTemps_common */
 /* declare_temp */
 /* declare_local */
 /* Locals_end_procedure */
 /* end_block */
 /* Locals_begin_procedure */
 /* begin_block */
 /* Locals_end_procedure */
 /* end_block */
 /* Locals_begin_procedure */
 /* begin_block */
 /* AllocateTemps_check_lo */
 /* AllocateTemps_common */
 /* declare_temp */
 /* declare_local */
 /* Locals_end_procedure */
 /* end_block */
 /* Locals_begin_procedure */
 /* begin_block */
 /* Locals_end_procedure */
 /* end_block */
 /* Locals_begin_procedure */
 /* begin_block */
 /* Locals_end_procedure */
 /* end_block */
 /* Locals_begin_procedure */
 /* begin_block */
 /* Locals_end_procedure */
 /* end_block */
 /* Locals_begin_procedure */
 /* begin_block */
 /* Locals_end_procedure */
 /* end_block */
 /* Locals_begin_procedure */
 /* begin_block */
 /* Locals_end_procedure */
 /* end_block */
 /* Locals_begin_procedure */
 /* begin_block */
 /* Locals_end_procedure */
 /* end_block */
 /* Locals_begin_procedure */
 /* begin_block */
 /* Locals_end_procedure */
 /* end_block */
 /* Locals_begin_procedure */
 /* begin_block */
 /* Locals_end_procedure */
 /* end_block */
 /* Locals_begin_procedure */
 /* begin_block */
 /* Locals_end_procedure */
 /* end_block */
 /* Locals_begin_procedure */
 /* begin_block */
 /* Locals_end_procedure */
 /* end_block */
 /* Locals_begin_procedure */
 /* begin_block */
 /* Locals_end_procedure */
 /* end_block */
 /* Locals_begin_procedure */
 /* begin_block */
 /* Locals_end_procedure */
 /* end_block */
 /* Locals_begin_procedure */
 /* begin_block */
 /* Locals_end_procedure */
 /* end_block */
 /* Locals_begin_procedure */
 /* begin_block */
 /* Locals_end_procedure */
 /* end_block */
 /* Locals_begin_procedure */
 /* begin_block */
 /* Locals_end_procedure */
 /* end_block */
 /* Locals_begin_procedure */
 /* begin_block */
 /* Locals_end_procedure */
 /* end_block */
 /* Locals_begin_procedure */
 /* begin_block */
 /* Locals_end_procedure */
 /* end_block */
 /* Locals_begin_procedure */
 /* begin_block */
 /* Locals_end_procedure */
 /* end_block */
 /* Locals_begin_procedure */
 /* begin_block */
 /* Locals_end_procedure */
 /* end_block */
 /* Locals_begin_procedure */
 /* begin_block */
 /* Locals_end_procedure */
 /* end_block */
 /* Locals_begin_procedure */
 /* begin_block */
 /* Locals_end_procedure */
 /* end_block */
 /* Locals_begin_procedure */
 /* begin_block */
 /* Locals_end_procedure */
 /* end_block */
 /* Locals_begin_procedure */
 /* begin_block */
 /* Locals_end_procedure */
 /* end_block */
 /* Locals_begin_procedure */
 /* begin_block */
 /* Locals_end_procedure */
 /* end_block */
 /* Locals_begin_procedure */
 /* begin_block */
 /* Locals_end_procedure */
 /* end_block */
 /* Locals_begin_procedure */
 /* begin_block */
 /* Locals_end_procedure */
 /* end_block */
 /* Locals_begin_procedure */
 /* begin_block */
 /* Locals_end_procedure */
 /* end_block */
 /* Locals_begin_procedure */
 /* begin_block */
 /* Locals_end_procedure */
 /* end_block */
 /* Locals_begin_procedure */
 /* begin_block */
 /* Locals_end_procedure */
 /* end_block */
 /* Locals_begin_procedure */
 /* begin_block */
 /* Locals_end_procedure */
 /* end_block */
 /* Locals_begin_procedure */
 /* begin_block */
 /* Locals_end_procedure */
 /* end_block */
 /* Locals_begin_procedure */
 /* begin_block */
 /* Locals_end_procedure */
 /* end_block */
 /* Locals_begin_procedure */
 /* begin_block */
 /* Locals_end_procedure */
 /* end_block */
 /* Locals_begin_procedure */
 /* begin_block */
 /* Locals_end_procedure */
 /* end_block */
 /* Locals_begin_procedure */
 /* begin_block */
 /* Locals_end_procedure */
 /* end_block */
 /* Locals_begin_procedure */
 /* begin_block */
 /* Locals_end_procedure */
 /* end_block */
 /* Locals_begin_procedure */
 /* begin_block */
 /* Locals_end_procedure */
 /* end_block */
 /* Locals_begin_procedure */
 /* begin_block */
 /* Locals_end_procedure */
 /* end_block */
 /* Locals_begin_procedure */
 /* begin_block */
 /* Locals_end_procedure */
 /* end_block */
 /* Locals_begin_procedure */
 /* begin_block */
 /* Locals_end_procedure */
 /* end_block */
 /* Locals_begin_procedure */
 /* begin_block */
 /* Locals_end_procedure */
 /* end_block */
 /* Locals_begin_procedure */
 /* begin_block */
 /* Locals_end_procedure */
 /* end_block */
 /* Locals_begin_procedure */
 /* begin_block */
 /* Locals_end_procedure */
 /* end_block */
 /* Locals_begin_procedure */
 /* begin_block */
 /* Locals_end_procedure */
 /* end_block */
 /* Locals_begin_procedure */
 /* begin_block */
 /* Locals_end_procedure */
 /* end_block */
 /* Locals_begin_procedure */
 /* begin_block */
 /* Locals_end_procedure */
 /* end_block */
 /* Locals_begin_procedure */
 /* begin_block */
 /* Locals_end_procedure */
 /* end_block */
 /* Locals_begin_procedure */
 /* begin_block */
 /* Locals_end_procedure */
 /* end_block */
 /* Locals_begin_procedure */
 /* begin_block */
 /* Locals_end_procedure */
 /* end_block */
 /* Locals_begin_procedure */
 /* begin_block */
 /* Locals_end_procedure */
 /* end_block */
 /* Locals_begin_procedure */
 /* begin_block */
 /* Locals_end_procedure */
 /* end_block */
 /* Locals_begin_procedure */
 /* begin_block */
 /* Locals_end_procedure */
 /* end_block */
 /* Locals_begin_procedure */
 /* begin_block */
 /* Locals_end_procedure */
 /* end_block */
 /* Locals_begin_procedure */
 /* begin_block */
 /* Locals_end_procedure */
 /* end_block */
 /* Locals_begin_procedure */
 /* begin_block */
 /* Locals_end_procedure */
 /* end_block */
 /* Locals_begin_procedure */
 /* begin_block */
 /* AllocateTemps_check_range */
 /* AllocateTemps_common */
 /* declare_temp */
 /* declare_local */
 /* Locals_end_procedure */
 /* end_block */
 /* Locals_begin_procedure */
 /* begin_block */
 /* Locals_end_procedure */
 /* end_block */
 /* Locals_begin_procedure */
 /* begin_block */
 /* AllocateTemps_check_range */
 /* AllocateTemps_common */
 /* declare_temp */
 /* declare_local */
 /* Locals_end_procedure */
 /* end_block */
 /* Locals_begin_procedure */
 /* begin_block */
 /* Locals_end_procedure */
 /* end_block */
 /* Locals_begin_procedure */
 /* begin_block */
 /* Locals_end_procedure */
 /* end_block */
 /* Locals_begin_procedure */
 /* begin_block */
 /* Locals_end_procedure */
 /* end_block */
 /* Locals_begin_procedure */
 /* begin_block */
 /* Locals_end_procedure */
 /* end_block */
 /* Locals_begin_procedure */
 /* begin_block */
 /* Locals_end_procedure */
 /* end_block */
 /* Locals_begin_procedure */
 /* begin_block */
 /* AllocateTemps_check_range */
 /* AllocateTemps_common */
 /* declare_temp */
 /* declare_local */
 /* Locals_end_procedure */
 /* end_block */
 /* Locals_begin_procedure */
 /* begin_block */
 /* Locals_end_procedure */
 /* end_block */
 /* Locals_begin_procedure */
 /* begin_block */
 /* AllocateTemps_check_range */
 /* AllocateTemps_common */
 /* declare_temp */
 /* declare_local */
 /* Locals_end_procedure */
 /* end_block */
 /* Locals_begin_procedure */
 /* begin_block */
 /* Locals_end_procedure */
 /* end_block */
 /* Locals_begin_procedure */
 /* begin_block */
 /* Locals_end_procedure */
 /* end_block */
 /* Locals_begin_procedure */
 /* begin_block */
 /* Locals_end_procedure */
 /* end_block */
 /* Locals_begin_procedure */
 /* begin_block */
 /* Locals_end_procedure */
 /* end_block */
 /* Locals_begin_procedure */
 /* begin_block */
 /* Locals_end_procedure */
 /* end_block */
 /* Locals_begin_procedure */
 /* begin_block */
 /* AllocateTemps_check_lo */
 /* AllocateTemps_common */
 /* declare_temp */
 /* declare_local */
 /* Locals_end_procedure */
 /* end_block */
 /* Locals_begin_procedure */
 /* begin_block */
 /* Locals_end_procedure */
 /* end_block */
 /* Locals_begin_procedure */
 /* begin_block */
 /* AllocateTemps_check_lo */
 /* AllocateTemps_common */
 /* declare_temp */
 /* declare_local */
 /* Locals_end_procedure */
 /* end_block */
 /* Locals_begin_procedure */
 /* begin_block */
 /* Locals_end_procedure */
 /* end_block */
 /* Locals_begin_procedure */
 /* begin_block */
 /* AllocateTemps_check_range */
 /* AllocateTemps_common */
 /* declare_temp */
 /* declare_local */
 /* Locals_end_procedure */
 /* end_block */
 /* Locals_begin_procedure */
 /* begin_block */
 /* Locals_end_procedure */
 /* end_block */
 /* Locals_begin_procedure */
 /* begin_block */
 /* AllocateTemps_check_range */
 /* AllocateTemps_common */
 /* declare_temp */
 /* declare_local */
 /* Locals_end_procedure */
 /* end_block */
 /* Locals_begin_procedure */
 /* begin_block */
 /* Locals_end_procedure */
 /* end_block */
 /* Locals_begin_procedure */
 /* begin_block */
 /* Locals_end_procedure */
 /* end_block */
 /* Locals_begin_procedure */
 /* begin_block */
 /* Locals_end_procedure */
 /* end_block */
 /* Locals_begin_procedure */
 /* begin_block */
 /* Locals_end_procedure */
 /* end_block */
 /* Locals_begin_procedure */
 /* begin_block */
 /* Locals_end_procedure */
 /* end_block */
 /* Locals_begin_procedure */
 /* begin_block */
 /* AllocateTemps_check_range */
 /* AllocateTemps_common */
 /* declare_temp */
 /* declare_local */
 /* Locals_end_procedure */
 /* end_block */
 /* Locals_begin_procedure */
 /* begin_block */
 /* Locals_end_procedure */
 /* end_block */
 /* Locals_begin_procedure */
 /* begin_block */
 /* AllocateTemps_check_range */
 /* AllocateTemps_common */
 /* declare_temp */
 /* declare_local */
 /* Locals_end_procedure */
 /* end_block */
 /* Locals_begin_procedure */
 /* begin_block */
 /* Locals_end_procedure */
 /* end_block */
 /* Locals_begin_procedure */
 /* begin_block */
 /* AllocateTemps_check_range */
 /* AllocateTemps_common */
 /* declare_temp */
 /* declare_local */
 /* Locals_end_procedure */
 /* end_block */
 /* Locals_begin_procedure */
 /* begin_block */
 /* Locals_end_procedure */
 /* end_block */
 /* Locals_begin_procedure */
 /* begin_block */
 /* AllocateTemps_check_range */
 /* AllocateTemps_common */
 /* declare_temp */
 /* declare_local */
 /* Locals_end_procedure */
 /* end_block */
 /* Locals_begin_procedure */
 /* begin_block */
 /* Locals_end_procedure */
 /* end_block */
 /* Locals_begin_procedure */
 /* begin_block */
 /* AllocateTemps_check_lo */
 /* AllocateTemps_common */
 /* declare_temp */
 /* declare_local */
 /* Locals_end_procedure */
 /* end_block */
 /* Locals_begin_procedure */
 /* begin_block */
 /* Locals_end_procedure */
 /* end_block */
 /* Locals_begin_procedure */
 /* begin_block */
 /* AllocateTemps_check_lo */
 /* AllocateTemps_common */
 /* declare_temp */
 /* declare_local */
 /* Locals_end_procedure */
 /* end_block */
 /* Locals_begin_procedure */
 /* begin_block */
 /* Locals_end_procedure */
 /* end_block */
 /* Locals_begin_procedure */
 /* begin_block */
 /* AllocateTemps_check_lo */
 /* AllocateTemps_common */
 /* declare_temp */
 /* declare_local */
 /* Locals_end_procedure */
 /* end_block */
 /* Locals_begin_procedure */
 /* begin_block */
 /* Locals_end_procedure */
 /* end_block */
 /* Locals_begin_procedure */
 /* begin_block */
 /* AllocateTemps_check_lo */
 /* AllocateTemps_common */
 /* declare_temp */
 /* declare_local */
 /* Locals_end_procedure */
 /* end_block */
 /* Locals_begin_procedure */
 /* begin_block */
 /* Locals_end_procedure */
 /* end_block */
 /* Locals_begin_procedure */
 /* begin_block */
 /* Locals_end_procedure */
 /* end_block */
 /* Locals_begin_procedure */
 /* begin_block */
 /* Locals_end_procedure */
 /* end_block */
 /* Locals_begin_procedure */
 /* begin_block */
 /* Locals_end_procedure */
 /* end_block */
 /* Locals_begin_procedure */
 /* begin_block */
 /* Locals_end_procedure */
 /* end_block */
 /* Locals_begin_procedure */
 /* begin_block */
 /* Locals_end_procedure */
 /* end_block */
 /* Locals_begin_procedure */
 /* begin_block */
 /* Locals_end_procedure */
 /* end_block */
 /* Locals_begin_procedure */
 /* begin_block */
 /* Locals_end_procedure */
 /* end_block */
 /* Locals_begin_procedure */
 /* begin_block */
 /* Locals_end_procedure */
 /* end_block */
 /* Locals_begin_procedure */
 /* begin_block */
 /* Locals_end_procedure */
 /* end_block */
 /* Locals_begin_procedure */
 /* begin_block */
 /* Locals_end_procedure */
 /* end_block */
 /* Locals_begin_procedure */
 /* begin_block */
 /* Locals_end_procedure */
 /* end_block */
 /* Locals_begin_procedure */
 /* begin_block */
 /* Locals_end_procedure */
 /* end_block */
 /* Locals_begin_procedure */
 /* begin_block */
 /* Locals_end_procedure */
 /* end_block */
 /* Locals_begin_procedure */
 /* begin_block */
 /* Locals_end_procedure */
 /* end_block */
 /* Locals_begin_procedure */
 /* begin_block */
 /* Locals_end_procedure */
 /* end_block */
 /* Locals_begin_procedure */
 /* begin_block */
 /* Locals_end_procedure */
 /* end_block */
 /* Locals_begin_procedure */
 /* begin_block */
 /* Locals_end_procedure */
 /* end_block */
 /* Locals_begin_procedure */
 /* begin_block */
 /* Locals_end_procedure */
 /* end_block */
 /* Locals_begin_procedure */
 /* begin_block */
 /* Locals_end_procedure */
 /* end_block */
 /* Locals_begin_procedure */
 /* begin_block */
 /* Locals_end_procedure */
 /* end_block */
 /* Locals_begin_procedure */
 /* begin_block */
 /* Locals_end_procedure */
 /* end_block */
 /* Locals_begin_procedure */
 /* begin_block */
 /* Locals_end_procedure */
 /* end_block */
 /* Locals_begin_procedure */
 /* begin_block */
 /* Locals_end_procedure */
 /* end_block */
 /* Locals_begin_procedure */
 /* begin_block */
 /* Locals_end_procedure */
 /* end_block */
 /* Locals_begin_procedure */
 /* begin_block */
 /* Locals_end_procedure */
 /* end_block */
 /* Locals_begin_procedure */
 /* begin_block */
 /* Locals_end_procedure */
 /* end_block */
 /* Locals_begin_procedure */
 /* begin_block */
 /* Locals_end_procedure */
 /* end_block */
 /* Locals_begin_procedure */
 /* begin_block */
 /* Locals_end_procedure */
 /* end_block */
 /* Locals_begin_procedure */
 /* begin_block */
 /* Locals_end_procedure */
 /* end_block */
 /* Locals_begin_procedure */
 /* begin_block */
 /* Locals_end_procedure */
 /* end_block */
 /* Locals_begin_procedure */
 /* begin_block */
 /* Locals_end_procedure */
 /* end_block */
 /* Locals_begin_procedure */
 /* begin_block */
 /* Locals_end_procedure */
 /* end_block */
 /* Locals_begin_procedure */
 /* begin_block */
 /* Locals_end_procedure */
 /* end_block */
 /* Locals_begin_procedure */
 /* begin_block */
 /* Locals_end_procedure */
 /* end_block */
 /* Locals_begin_procedure */
 /* begin_block */
 /* Locals_end_procedure */
 /* end_block */
 /* Locals_begin_procedure */
 /* begin_block */
 /* Locals_end_procedure */
 /* end_block */
 /* Locals_begin_procedure */
 /* begin_block */
 /* Locals_end_procedure */
 /* end_block */
 /* Locals_begin_procedure */
 /* begin_block */
 /* Locals_end_procedure */
 /* end_block */
 /* Locals_begin_procedure */
 /* begin_block */
 /* Locals_end_procedure */
 /* end_block */
 /* Locals_begin_procedure */
 /* begin_block */
 /* Locals_end_procedure */
 /* end_block */
 /* Locals_begin_procedure */
 /* begin_block */
 /* Locals_end_procedure */
 /* end_block */
 /* Locals_begin_procedure */
 /* begin_block */
 /* Locals_end_procedure */
 /* end_block */
 /* Locals_begin_procedure */
 /* begin_block */
 /* Locals_end_procedure */
 /* end_block */
 /* Locals_begin_procedure */
 /* begin_block */
 /* Locals_end_procedure */
 /* end_block */
 /* Locals_begin_procedure */
 /* begin_block */
 /* Locals_end_procedure */
 /* end_block */
 /* Locals_begin_procedure */
 /* begin_block */
 /* Locals_end_procedure */
 /* end_block */
 /* Locals_begin_procedure */
 /* begin_block */
 /* Locals_end_procedure */
 /* end_block */
 /* Locals_begin_procedure */
 /* begin_block */
 /* Locals_end_procedure */
 /* end_block */
 /* Locals_begin_procedure */
 /* begin_block */
 /* Locals_end_procedure */
 /* end_block */
 /* Locals_begin_procedure */
 /* begin_block */
 /* Locals_end_procedure */
 /* end_block */
 /* Locals_begin_procedure */
 /* begin_block */
 /* Locals_end_procedure */
 /* end_block */
 /* Locals_begin_procedure */
 /* begin_block */
 /* Locals_end_procedure */
 /* end_block */
 /* Locals_begin_procedure */
 /* begin_block */
 /* AllocateTemps_check_range */
 /* AllocateTemps_common */
 /* declare_temp */
 /* declare_local */
 /* Locals_end_procedure */
 /* end_block */
 /* Locals_begin_procedure */
 /* begin_block */
 /* Locals_end_procedure */
 /* end_block */
 /* Locals_begin_procedure */
 /* begin_block */
 /* AllocateTemps_check_range */
 /* AllocateTemps_common */
 /* declare_temp */
 /* declare_local */
 /* Locals_end_procedure */
 /* end_block */
 /* Locals_begin_procedure */
 /* begin_block */
 /* Locals_end_procedure */
 /* end_block */
 /* Locals_begin_procedure */
 /* begin_block */
 /* Locals_end_procedure */
 /* end_block */
 /* Locals_begin_procedure */
 /* begin_block */
 /* Locals_end_procedure */
 /* end_block */
 /* Locals_begin_procedure */
 /* begin_block */
 /* Locals_end_procedure */
 /* end_block */
 /* Locals_begin_procedure */
 /* begin_block */
 /* Locals_end_procedure */
 /* end_block */
 /* Locals_begin_procedure */
 /* begin_block */
 /* AllocateTemps_check_range */
 /* AllocateTemps_common */
 /* declare_temp */
 /* declare_local */
 /* Locals_end_procedure */
 /* end_block */
 /* Locals_begin_procedure */
 /* begin_block */
 /* Locals_end_procedure */
 /* end_block */
 /* Locals_begin_procedure */
 /* begin_block */
 /* AllocateTemps_check_range */
 /* AllocateTemps_common */
 /* declare_temp */
 /* declare_local */
 /* Locals_end_procedure */
 /* end_block */
 /* Locals_begin_procedure */
 /* begin_block */
 /* Locals_end_procedure */
 /* end_block */
 /* Locals_begin_procedure */
 /* begin_block */
 /* Locals_end_procedure */
 /* end_block */
 /* Locals_begin_procedure */
 /* begin_block */
 /* Locals_end_procedure */
 /* end_block */
 /* Locals_begin_procedure */
 /* begin_block */
 /* Locals_end_procedure */
 /* end_block */
 /* Locals_begin_procedure */
 /* begin_block */
 /* Locals_end_procedure */
 /* end_block */
 /* Locals_begin_procedure */
 /* begin_block */
 /* AllocateTemps_check_lo */
 /* AllocateTemps_common */
 /* declare_temp */
 /* declare_local */
 /* Locals_end_procedure */
 /* end_block */
 /* Locals_begin_procedure */
 /* begin_block */
 /* Locals_end_procedure */
 /* end_block */
 /* Locals_begin_procedure */
 /* begin_block */
 /* AllocateTemps_check_lo */
 /* AllocateTemps_common */
 /* declare_temp */
 /* declare_local */
 /* Locals_end_procedure */
 /* end_block */
 /* Locals_begin_procedure */
 /* begin_block */
 /* Locals_end_procedure */
 /* end_block */
 /* Locals_begin_procedure */
 /* begin_block */
 /* AllocateTemps_check_range */
 /* AllocateTemps_common */
 /* declare_temp */
 /* declare_local */
 /* Locals_end_procedure */
 /* end_block */
 /* Locals_begin_procedure */
 /* begin_block */
 /* Locals_end_procedure */
 /* end_block */
 /* Locals_begin_procedure */
 /* begin_block */
 /* AllocateTemps_check_range */
 /* AllocateTemps_common */
 /* declare_temp */
 /* declare_local */
 /* Locals_end_procedure */
 /* end_block */
 /* Locals_begin_procedure */
 /* begin_block */
 /* Locals_end_procedure */
 /* end_block */
 /* Locals_begin_procedure */
 /* begin_block */
 /* Locals_end_procedure */
 /* end_block */
 /* Locals_begin_procedure */
 /* begin_block */
 /* Locals_end_procedure */
 /* end_block */
 /* Locals_begin_procedure */
 /* begin_block */
 /* Locals_end_procedure */
 /* end_block */
 /* Locals_begin_procedure */
 /* begin_block */
 /* Locals_end_procedure */
 /* end_block */
 /* Locals_begin_procedure */
 /* begin_block */
 /* AllocateTemps_check_range */
 /* AllocateTemps_common */
 /* declare_temp */
 /* declare_local */
 /* Locals_end_procedure */
 /* end_block */
 /* Locals_begin_procedure */
 /* begin_block */
 /* Locals_end_procedure */
 /* end_block */
 /* Locals_begin_procedure */
 /* begin_block */
 /* AllocateTemps_check_range */
 /* AllocateTemps_common */
 /* declare_temp */
 /* declare_local */
 /* Locals_end_procedure */
 /* end_block */
 /* Locals_begin_procedure */
 /* begin_block */
 /* Locals_end_procedure */
 /* end_block */
 /* Locals_begin_procedure */
 /* begin_block */
 /* AllocateTemps_check_lo */
 /* AllocateTemps_common */
 /* declare_temp */
 /* declare_local */
 /* Locals_end_procedure */
 /* end_block */
 /* Locals_begin_procedure */
 /* begin_block */
 /* Locals_end_procedure */
 /* end_block */
 /* Locals_begin_procedure */
 /* begin_block */
 /* AllocateTemps_check_lo */
 /* AllocateTemps_common */
 /* declare_temp */
 /* declare_local */
 /* Locals_end_procedure */
 /* end_block */
 /* Locals_begin_procedure */
 /* begin_block */
 /* Locals_end_procedure */
 /* end_block */
 /* Locals_begin_procedure */
 /* begin_block */
 /* AllocateTemps_check_lo */
 /* AllocateTemps_common */
 /* declare_temp */
 /* declare_local */
 /* Locals_end_procedure */
 /* end_block */
 /* Locals_begin_procedure */
 /* begin_block */
 /* Locals_end_procedure */
 /* end_block */
 /* Locals_begin_procedure */
 /* begin_block */
 /* AllocateTemps_check_lo */
 /* AllocateTemps_common */
 /* declare_temp */
 /* declare_local */
 /* Locals_end_procedure */
 /* end_block */
 /* Locals_begin_procedure */
 /* begin_block */
 /* Locals_end_procedure */
 /* end_block */
 /* Locals_begin_procedure */
 /* begin_block */
 /* AllocateTemps_check_lo */
 /* AllocateTemps_common */
 /* declare_temp */
 /* declare_local */
 /* Locals_end_procedure */
 /* end_block */
 /* Locals_begin_procedure */
 /* begin_block */
 /* Locals_end_procedure */
 /* end_block */
 /* Locals_begin_procedure */
 /* begin_block */
 /* AllocateTemps_check_lo */
 /* AllocateTemps_common */
 /* declare_temp */
 /* declare_local */
 /* Locals_end_procedure */
 /* end_block */
 /* Locals_begin_procedure */
 /* begin_block */
 /* Locals_end_procedure */
 /* end_block */
 /* Locals_begin_procedure */
 /* begin_block */
 /* Locals_end_procedure */
 /* end_block */
 /* Locals_begin_procedure */
 /* begin_block */
 /* Locals_end_procedure */
 /* end_block */
 /* Locals_begin_procedure */
 /* begin_block */
 /* Locals_end_procedure */
 /* end_block */
 /* Locals_begin_procedure */
 /* begin_block */
 /* Locals_end_procedure */
 /* end_block */
 /* Locals_begin_procedure */
 /* begin_block */
 /* Locals_end_procedure */
 /* end_block */
 /* Locals_begin_procedure */
 /* begin_block */
 /* Locals_end_procedure */
 /* end_block */
 /* Locals_begin_procedure */
 /* begin_block */
 /* Locals_end_procedure */
 /* end_block */
 /* Locals_begin_procedure */
 /* begin_block */
 /* Locals_end_procedure */
 /* end_block */
 /* Locals_begin_procedure */
 /* begin_block */
 /* Locals_end_procedure */
 /* end_block */
 /* Locals_begin_procedure */
 /* begin_block */
 /* Locals_end_procedure */
 /* end_block */
 /* Locals_begin_procedure */
 /* begin_block */
 /* Locals_end_procedure */
 /* end_block */
 /* Locals_begin_procedure */
 /* begin_block */
 /* Locals_end_procedure */
 /* end_block */
 /* Locals_begin_procedure */
 /* begin_block */
 /* Locals_end_procedure */
 /* end_block */
 /* Locals_begin_procedure */
 /* begin_block */
 /* Locals_end_procedure */
 /* end_block */
 /* Locals_begin_procedure */
 /* begin_block */
 /* Locals_end_procedure */
 /* end_block */
 /* Locals_begin_procedure */
 /* begin_block */
 /* Locals_end_procedure */
 /* end_block */
 /* Locals_begin_procedure */
 /* begin_block */
 /* Locals_end_procedure */
 /* end_block */
 /* Locals_begin_procedure */
 /* begin_block */
 /* Locals_end_procedure */
 /* end_block */
 /* Locals_begin_procedure */
 /* begin_block */
 /* Locals_end_procedure */
 /* end_block */
 /* Locals_begin_procedure */
 /* begin_block */
 /* Locals_end_procedure */
 /* end_block */
 /* Locals_begin_procedure */
 /* begin_block */
 /* Locals_end_procedure */
 /* end_block */
 /* Locals_begin_procedure */
 /* begin_block */
 /* Locals_end_procedure */
 /* end_block */
 /* Locals_begin_procedure */
 /* begin_block */
 /* Locals_end_procedure */
 /* end_block */
 /* Locals_begin_procedure */
 /* begin_block */
 /* Locals_end_procedure */
 /* end_block */
 /* Locals_begin_procedure */
 /* begin_block */
 /* Locals_end_procedure */
 /* end_block */
 /* Locals_begin_procedure */
 /* begin_block */
 /* Locals_end_procedure */
 /* end_block */
 /* Locals_begin_procedure */
 /* begin_block */
 /* Locals_end_procedure */
 /* end_block */
 /* Locals_begin_procedure */
 /* begin_block */
 /* Locals_end_procedure */
 /* end_block */
 /* Locals_begin_procedure */
 /* begin_block */
 /* Locals_end_procedure */
 /* end_block */
 /* Locals_begin_procedure */
 /* begin_block */
 /* Locals_end_procedure */
 /* end_block */
 /* Locals_begin_procedure */
 /* begin_block */
 /* Locals_end_procedure */
 /* end_block */
 /* Locals_begin_procedure */
 /* begin_block */
 /* Locals_end_procedure */
 /* end_block */
 /* Locals_begin_procedure */
 /* begin_block */
 /* Locals_end_procedure */
 /* end_block */
 /* Locals_begin_procedure */
 /* begin_block */
 /* Locals_end_procedure */
 /* end_block */
 /* Locals_begin_procedure */
 /* begin_block */
 /* Locals_end_procedure */
 /* end_block */
 /* Locals_begin_procedure */
 /* begin_block */
 /* Locals_end_procedure */
 /* end_block */
 /* Locals_begin_procedure */
 /* begin_block */
 /* Locals_end_procedure */
 /* end_block */
 /* Locals_begin_procedure */
 /* begin_block */
 /* Locals_end_procedure */
 /* end_block */
 /* Locals_begin_procedure */
 /* begin_block */
 /* Locals_end_procedure */
 /* end_block */
 /* Locals_begin_procedure */
 /* begin_block */
 /* Locals_end_procedure */
 /* end_block */
 /* Locals_begin_procedure */
 /* begin_block */
 /* Locals_end_procedure */
 /* end_block */
 /* Locals_begin_procedure */
 /* begin_block */
 /* Locals_end_procedure */
 /* end_block */
 /* Locals_begin_procedure */
 /* begin_block */
 /* Locals_end_procedure */
 /* end_block */
 /* Locals_begin_procedure */
 /* begin_block */
 /* Locals_end_procedure */
 /* end_block */
 /* Locals_begin_procedure */
 /* begin_block */
 /* Locals_end_procedure */
 /* end_block */
 /* Locals_begin_procedure */
 /* begin_block */
 /* Locals_end_procedure */
 /* end_block */
 /* Locals_begin_procedure */
 /* begin_block */
 /* Locals_end_procedure */
 /* end_block */
 /* Locals_begin_procedure */
 /* begin_block */
 /* Locals_end_procedure */
 /* end_block */
 /* Locals_begin_procedure */
 /* begin_block */
 /* Locals_end_procedure */
 /* end_block */
 /* Locals_begin_procedure */
 /* begin_block */
 /* Locals_end_procedure */
 /* end_block */
 /* Locals_begin_procedure */
 /* begin_block */
 /* Locals_end_procedure */
 /* end_block */
 /* Locals_begin_procedure */
 /* begin_block */
 /* Locals_end_procedure */
 /* end_block */
 /* Locals_begin_procedure */
 /* begin_block */
 /* Locals_end_procedure */
 /* end_block */
 /* Locals_begin_procedure */
 /* begin_block */
 /* Locals_end_procedure */
 /* end_block */
 /* Locals_begin_procedure */
 /* begin_block */
 /* Locals_end_procedure */
 /* end_block */
 /* Locals_begin_procedure */
 /* begin_block */
 /* Locals_end_procedure */
 /* end_block */
 /* Locals_begin_procedure */
 /* begin_block */
 /* Locals_end_procedure */
 /* end_block */
 /* Locals_begin_procedure */
 /* begin_block */
 /* Locals_end_procedure */
 /* end_block */
 /* Locals_begin_procedure */
 /* begin_block */
 /* Locals_end_procedure */
 /* end_block */
 /* Locals_begin_procedure */
 /* begin_block */
 /* Locals_end_procedure */
 /* end_block */
 /* Locals_begin_procedure */
 /* begin_block */
 /* Locals_end_procedure */
 /* end_block */
 /* Locals_begin_procedure */
 /* begin_block */
 /* Locals_end_procedure */
 /* end_block */
 /* Locals_begin_procedure */
 /* begin_block */
 /* Locals_end_procedure */
 /* end_block */
 /* Locals_begin_procedure */
 /* begin_block */
 /* Locals_end_procedure */
 /* end_block */
 /* Locals_begin_procedure */
 /* begin_block */
 /* Locals_end_procedure */
 /* end_block */
 /* Locals_begin_procedure */
 /* begin_block */
 /* Locals_end_procedure */
 /* end_block */
 /* Locals_begin_procedure */
 /* begin_block */
 /* Locals_end_procedure */
 /* end_block */
 /* Locals_begin_procedure */
 /* begin_block */
 /* Locals_end_procedure */
 /* end_block */
 /* Locals_begin_procedure */
 /* begin_block */
 /* Locals_end_procedure */
 /* end_block */
 /* Locals_begin_procedure */
 /* begin_block */
 /* Locals_end_procedure */
 /* end_block */
 /* Locals_begin_procedure */
 /* begin_block */
 /* Locals_end_procedure */
 /* end_block */
 /* Locals_begin_procedure */
 /* begin_block */
 /* Locals_end_procedure */
 /* end_block */
 /* Locals_begin_procedure */
 /* begin_block */
 /* Locals_end_procedure */
 /* end_block */
 /* Locals_begin_procedure */
 /* begin_block */
 /* Locals_end_procedure */
 /* end_block */
 /* Locals_begin_procedure */
 /* begin_block */
 /* Locals_end_procedure */
 /* end_block */
 /* Locals_begin_procedure */
 /* begin_block */
 /* Locals_end_procedure */
 /* end_block */
 /* Locals_begin_procedure */
 /* begin_block */
 /* Locals_end_procedure */
 /* end_block */
 /* Locals_begin_procedure */
 /* begin_block */
 /* Locals_end_procedure */
 /* end_block */
 /* Locals_begin_procedure */
 /* begin_block */
 /* Locals_end_procedure */
 /* end_block */
 /* Locals_begin_procedure */
 /* begin_block */
 /* Locals_end_procedure */
 /* end_block */
 /* Locals_begin_procedure */
 /* begin_block */
 /* Locals_end_procedure */
 /* end_block */
 /* Locals_begin_procedure */
 /* begin_block */
 /* Locals_end_procedure */
 /* end_block */
 /* Locals_begin_procedure */
 /* begin_block */
 /* Locals_end_procedure */
 /* end_block */
 /* Locals_begin_procedure */
 /* begin_block */
 /* Locals_end_procedure */
 /* end_block */
 /* Locals_begin_procedure */
 /* begin_block */
 /* Locals_end_procedure */
 /* end_block */
 /* Locals_begin_procedure */
 /* begin_block */
 /* Locals_end_procedure */
 /* end_block */
 /* Locals_begin_procedure */
 /* begin_block */
 /* Locals_end_procedure */
 /* end_block */
 /* Locals_begin_procedure */
 /* begin_block */
 /* Locals_end_procedure */
 /* end_block */
 /* Locals_begin_procedure */
 /* begin_block */
 /* Locals_end_procedure */
 /* end_block */
 /* Locals_begin_procedure */
 /* begin_block */
 /* Locals_end_procedure */
 /* end_block */
 /* Locals_begin_procedure */
 /* begin_block */
 /* Locals_end_procedure */
 /* end_block */
 /* Locals_begin_procedure */
 /* begin_block */
 /* Locals_end_procedure */
 /* end_block */
 /* Locals_begin_procedure */
 /* begin_block */
 /* Locals_end_procedure */
 /* end_block */
 /* Locals_begin_procedure */
 /* begin_block */
 /* Locals_end_procedure */
 /* end_block */
 /* Locals_begin_procedure */
 /* begin_block */
 /* Locals_end_procedure */
 /* end_block */
 /* Locals_begin_procedure */
 /* begin_block */
 /* Locals_end_procedure */
 /* end_block */
 /* Locals_begin_procedure */
 /* begin_block */
 /* Locals_end_procedure */
 /* end_block */
 /* Locals_begin_procedure */
 /* begin_block */
 /* Locals_end_procedure */
 /* end_block */
 /* Locals_begin_procedure */
 /* begin_block */
 /* Locals_end_procedure */
 /* end_block */
 /* Locals_begin_procedure */
 /* begin_block */
 /* Locals_end_procedure */
 /* end_block */
 /* Locals_begin_procedure */
 /* begin_block */
 /* Locals_end_procedure */
 /* end_block */
 /* Locals_begin_procedure */
 /* begin_block */
 /* Locals_end_procedure */
 /* end_block */
 /* Locals_begin_procedure */
 /* begin_block */
 /* AllocateTemps_check_range */
 /* AllocateTemps_common */
 /* declare_temp */
 /* declare_local */
 /* Locals_end_procedure */
 /* end_block */
 /* Locals_begin_procedure */
 /* begin_block */
 /* Locals_end_procedure */
 /* end_block */
 /* Locals_begin_procedure */
 /* begin_block */
 /* AllocateTemps_check_range */
 /* AllocateTemps_common */
 /* declare_temp */
 /* declare_local */
 /* Locals_end_procedure */
 /* end_block */
 /* Locals_begin_procedure */
 /* begin_block */
 /* Locals_end_procedure */
 /* end_block */
 /* Locals_begin_procedure */
 /* begin_block */
 /* Locals_end_procedure */
 /* end_block */
 /* Locals_begin_procedure */
 /* begin_block */
 /* Locals_end_procedure */
 /* end_block */
 /* Locals_begin_procedure */
 /* begin_block */
 /* Locals_end_procedure */
 /* end_block */
 /* Locals_begin_procedure */
 /* begin_block */
 /* Locals_end_procedure */
 /* end_block */
 /* Locals_begin_procedure */
 /* begin_block */
 /* AllocateTemps_check_range */
 /* AllocateTemps_common */
 /* declare_temp */
 /* declare_local */
 /* Locals_end_procedure */
 /* end_block */
 /* Locals_begin_procedure */
 /* begin_block */
 /* Locals_end_procedure */
 /* end_block */
 /* Locals_begin_procedure */
 /* begin_block */
 /* AllocateTemps_check_range */
 /* AllocateTemps_common */
 /* declare_temp */
 /* declare_local */
 /* Locals_end_procedure */
 /* end_block */
 /* Locals_begin_procedure */
 /* begin_block */
 /* Locals_end_procedure */
 /* end_block */
 /* Locals_begin_procedure */
 /* begin_block */
 /* Locals_end_procedure */
 /* end_block */
 /* Locals_begin_procedure */
 /* begin_block */
 /* Locals_end_procedure */
 /* end_block */
 /* Locals_begin_procedure */
 /* begin_block */
 /* Locals_end_procedure */
 /* end_block */
 /* Locals_begin_procedure */
 /* begin_block */
 /* Locals_end_procedure */
 /* end_block */
 /* Locals_begin_procedure */
 /* begin_block */
 /* AllocateTemps_check_lo */
 /* AllocateTemps_common */
 /* declare_temp */
 /* declare_local */
 /* Locals_end_procedure */
 /* end_block */
 /* Locals_begin_procedure */
 /* begin_block */
 /* Locals_end_procedure */
 /* end_block */
 /* Locals_begin_procedure */
 /* begin_block */
 /* AllocateTemps_check_lo */
 /* AllocateTemps_common */
 /* declare_temp */
 /* declare_local */
 /* Locals_end_procedure */
 /* end_block */
 /* Locals_begin_procedure */
 /* begin_block */
 /* Locals_end_procedure */
 /* end_block */
 /* Locals_begin_procedure */
 /* begin_block */
 /* AllocateTemps_check_range */
 /* AllocateTemps_common */
 /* declare_temp */
 /* declare_local */
 /* Locals_end_procedure */
 /* end_block */
 /* Locals_begin_procedure */
 /* begin_block */
 /* Locals_end_procedure */
 /* end_block */
 /* Locals_begin_procedure */
 /* begin_block */
 /* AllocateTemps_check_range */
 /* AllocateTemps_common */
 /* declare_temp */
 /* declare_local */
 /* Locals_end_procedure */
 /* end_block */
 /* Locals_begin_procedure */
 /* begin_block */
 /* Locals_end_procedure */
 /* end_block */
 /* Locals_begin_procedure */
 /* begin_block */
 /* Locals_end_procedure */
 /* end_block */
 /* Locals_begin_procedure */
 /* begin_block */
 /* Locals_end_procedure */
 /* end_block */
 /* Locals_begin_procedure */
 /* begin_block */
 /* Locals_end_procedure */
 /* end_block */
 /* Locals_begin_procedure */
 /* begin_block */
 /* Locals_end_procedure */
 /* end_block */
 /* Locals_begin_procedure */
 /* begin_block */
 /* AllocateTemps_check_range */
 /* AllocateTemps_common */
 /* declare_temp */
 /* declare_local */
 /* Locals_end_procedure */
 /* end_block */
 /* Locals_begin_procedure */
 /* begin_block */
 /* Locals_end_procedure */
 /* end_block */
 /* Locals_begin_procedure */
 /* begin_block */
 /* AllocateTemps_check_range */
 /* AllocateTemps_common */
 /* declare_temp */
 /* declare_local */
 /* Locals_end_procedure */
 /* end_block */
 /* Locals_begin_procedure */
 /* begin_block */
 /* Locals_end_procedure */
 /* end_block */
 /* Locals_begin_procedure */
 /* begin_block */
 /* AllocateTemps_check_range */
 /* AllocateTemps_common */
 /* declare_temp */
 /* declare_local */
 /* Locals_end_procedure */
 /* end_block */
 /* Locals_begin_procedure */
 /* begin_block */
 /* Locals_end_procedure */
 /* end_block */
 /* Locals_begin_procedure */
 /* begin_block */
 /* AllocateTemps_check_range */
 /* AllocateTemps_common */
 /* declare_temp */
 /* declare_local */
 /* Locals_end_procedure */
 /* end_block */
 /* Locals_begin_procedure */
 /* begin_block */
 /* Locals_end_procedure */
 /* end_block */
 /* Locals_begin_procedure */
 /* begin_block */
 /* AllocateTemps_check_lo */
 /* AllocateTemps_common */
 /* declare_temp */
 /* declare_local */
 /* Locals_end_procedure */
 /* end_block */
 /* Locals_begin_procedure */
 /* begin_block */
 /* Locals_end_procedure */
 /* end_block */
 /* Locals_begin_procedure */
 /* begin_block */
 /* AllocateTemps_check_lo */
 /* AllocateTemps_common */
 /* declare_temp */
 /* declare_local */
 /* Locals_end_procedure */
 /* end_block */
 /* Locals_begin_procedure */
 /* begin_block */
 /* Locals_end_procedure */
 /* end_block */
 /* Locals_begin_procedure */
 /* begin_block */
 /* AllocateTemps_check_lo */
 /* AllocateTemps_common */
 /* declare_temp */
 /* declare_local */
 /* Locals_end_procedure */
 /* end_block */
 /* Locals_begin_procedure */
 /* begin_block */
 /* Locals_end_procedure */
 /* end_block */
 /* Locals_begin_procedure */
 /* begin_block */
 /* AllocateTemps_check_lo */
 /* AllocateTemps_common */
 /* declare_temp */
 /* declare_local */
 /* Locals_end_procedure */
 /* end_block */
 /* Locals_begin_procedure */
 /* begin_block */
 /* Locals_end_procedure */
 /* end_block */
 /* Locals_begin_procedure */
 /* begin_block */
 /* Locals_end_procedure */
 /* end_block */
 /* Locals_begin_procedure */
 /* begin_block */
 /* Locals_end_procedure */
 /* end_block */
 /* Locals_begin_procedure */
 /* begin_block */
 /* Locals_end_procedure */
 /* end_block */
 /* Locals_begin_procedure */
 /* begin_block */
 /* Locals_end_procedure */
 /* end_block */
 /* Locals_begin_procedure */
 /* begin_block */
 /* AllocateTemps_check_lo */
 /* AllocateTemps_common */
 /* declare_temp */
 /* declare_local */
 /* Locals_end_procedure */
 /* end_block */
 /* Locals_begin_procedure */
 /* begin_block */
 /* Locals_end_procedure */
 /* end_block */
 /* Locals_begin_procedure */
 /* begin_block */
 /* AllocateTemps_check_lo */
 /* AllocateTemps_common */
 /* declare_temp */
 /* declare_local */
 /* Locals_end_procedure */
 /* end_block */
 /* Locals_begin_procedure */
 /* begin_block */
 /* Locals_end_procedure */
 /* end_block */
 /* Locals_begin_procedure */
 /* begin_block */
 /* Locals_end_procedure */
 /* end_block */
 /* Locals_begin_procedure */
 /* begin_block */
 /* Locals_end_procedure */
 /* end_block */
 /* Locals_begin_procedure */
 /* begin_block */
 /* Locals_end_procedure */
 /* end_block */
 /* Locals_begin_procedure */
 /* begin_block */
 /* Locals_end_procedure */
 /* end_block */
 /* Locals_begin_procedure */
 /* begin_block */
 /* AllocateTemps_check_lo */
 /* AllocateTemps_common */
 /* declare_temp */
 /* declare_local */
 /* Locals_end_procedure */
 /* end_block */
 /* Locals_begin_procedure */
 /* begin_block */
 /* Locals_end_procedure */
 /* end_block */
 /* Locals_begin_procedure */
 /* begin_block */
 /* AllocateTemps_check_lo */
 /* AllocateTemps_common */
 /* declare_temp */
 /* declare_local */
 /* Locals_end_procedure */
 /* end_block */
 /* Locals_begin_procedure */
 /* begin_block */
 /* Locals_end_procedure */
 /* end_block */
 /* Locals_begin_procedure */
 /* begin_block */
 /* Locals_end_procedure */
 /* end_block */
 /* Locals_begin_procedure */
 /* begin_block */
 /* Locals_end_procedure */
 /* end_block */
 /* Locals_begin_procedure */
 /* begin_block */
 /* Locals_end_procedure */
 /* end_block */
 /* Locals_begin_procedure */
 /* begin_block */
 /* Locals_end_procedure */
 /* end_block */
 /* Locals_begin_procedure */
 /* begin_block */
 /* AllocateTemps_check_lo */
 /* AllocateTemps_common */
 /* declare_temp */
 /* declare_local */
 /* Locals_end_procedure */
 /* end_block */
 /* Locals_begin_procedure */
 /* begin_block */
 /* Locals_end_procedure */
 /* end_block */
 /* Locals_begin_procedure */
 /* begin_block */
 /* AllocateTemps_check_lo */
 /* AllocateTemps_common */
 /* declare_temp */
 /* declare_local */
 /* Locals_end_procedure */
 /* end_block */
 /* Locals_begin_procedure */
 /* begin_block */
 /* Locals_end_procedure */
 /* end_block */
 /* Locals_begin_procedure */
 /* begin_block */
 /* AllocateTemps_check_lo */
 /* AllocateTemps_common */
 /* declare_temp */
 /* declare_local */
 /* Locals_end_procedure */
 /* end_block */
 /* Locals_begin_procedure */
 /* begin_block */
 /* Locals_end_procedure */
 /* end_block */
 /* Locals_begin_procedure */
 /* begin_block */
 /* AllocateTemps_check_lo */
 /* AllocateTemps_common */
 /* declare_temp */
 /* declare_local */
 /* Locals_end_procedure */
 /* end_block */
 /* Locals_begin_procedure */
 /* begin_block */
 /* Locals_end_procedure */
 /* end_block */
 /* Locals_begin_procedure */
 /* begin_block */
 /* Locals_end_procedure */
 /* end_block */
 /* Locals_begin_procedure */
 /* begin_block */
 /* Locals_end_procedure */
 /* end_block */
 /* Locals_begin_procedure */
 /* begin_block */
 /* Locals_end_procedure */
 /* end_block */
 /* Locals_begin_procedure */
 /* begin_block */
 /* Locals_end_procedure */
 /* end_block */
 /* Locals_begin_procedure */
 /* begin_block */
 /* AllocateTemps_check_lo */
 /* AllocateTemps_common */
 /* declare_temp */
 /* declare_local */
 /* Locals_end_procedure */
 /* end_block */
 /* Locals_begin_procedure */
 /* begin_block */
 /* Locals_end_procedure */
 /* end_block */
 /* Locals_begin_procedure */
 /* begin_block */
 /* AllocateTemps_check_lo */
 /* AllocateTemps_common */
 /* declare_temp */
 /* declare_local */
 /* Locals_end_procedure */
 /* end_block */
 /* Locals_begin_procedure */
 /* begin_block */
 /* Locals_end_procedure */
 /* end_block */
 /* Locals_begin_procedure */
 /* begin_block */
 /* AllocateTemps_check_lo */
 /* AllocateTemps_common */
 /* declare_temp */
 /* declare_local */
 /* Locals_end_procedure */
 /* end_block */
 /* Locals_begin_procedure */
 /* begin_block */
 /* Locals_end_procedure */
 /* end_block */
 /* Locals_begin_procedure */
 /* begin_block */
 /* AllocateTemps_check_lo */
 /* AllocateTemps_common */
 /* declare_temp */
 /* declare_local */
 /* Locals_end_procedure */
 /* end_block */
 /* Locals_begin_procedure */
 /* begin_block */
 /* Locals_end_procedure */
 /* end_block */
 /* Locals_begin_procedure */
 /* begin_block */
 /* AllocateTemps_check_lo */
 /* AllocateTemps_common */
 /* declare_temp */
 /* declare_local */
 /* Locals_end_procedure */
 /* end_block */
 /* Locals_begin_procedure */
 /* begin_block */
 /* Locals_end_procedure */
 /* end_block */
 /* Locals_begin_procedure */
 /* begin_block */
 /* AllocateTemps_check_lo */
 /* AllocateTemps_common */
 /* declare_temp */
 /* declare_local */
 /* Locals_end_procedure */
 /* end_block */
 /* Locals_begin_procedure */
 /* begin_block */
 /* Locals_end_procedure */
 /* end_block */
 /* Locals_begin_procedure */
 /* begin_block */
 /* AllocateTemps_check_lo */
 /* AllocateTemps_common */
 /* declare_temp */
 /* declare_local */
 /* Locals_end_procedure */
 /* end_block */
 /* Locals_begin_procedure */
 /* begin_block */
 /* Locals_end_procedure */
 /* end_block */
 /* Locals_begin_procedure */
 /* begin_block */
 /* AllocateTemps_check_lo */
 /* AllocateTemps_common */
 /* declare_temp */
 /* declare_local */
 /* Locals_end_procedure */
 /* end_block */
 /* Locals_begin_procedure */
 /* begin_block */
 /* Locals_end_procedure */
 /* end_block */
 /* Locals_begin_procedure */
 /* begin_block */
 /* Locals_end_procedure */
 /* end_block */
 /* Locals_begin_procedure */
 /* begin_block */
 /* Locals_end_procedure */
 /* end_block */
 /* Locals_begin_procedure */
 /* begin_block */
 /* Locals_end_procedure */
 /* end_block */
 /* Locals_begin_procedure */
 /* begin_block */
 /* Locals_end_procedure */
 /* end_block */
 /* Locals_begin_procedure */
 /* begin_block */
 /* AllocateTemps_check_range */
 /* AllocateTemps_common */
 /* declare_temp */
 /* declare_local */
 /* Locals_end_procedure */
 /* end_block */
 /* Locals_begin_procedure */
 /* begin_block */
 /* Locals_end_procedure */
 /* end_block */
 /* Locals_begin_procedure */
 /* begin_block */
 /* AllocateTemps_check_range */
 /* AllocateTemps_common */
 /* declare_temp */
 /* declare_local */
 /* Locals_end_procedure */
 /* end_block */
 /* Locals_begin_procedure */
 /* begin_block */
 /* Locals_end_procedure */
 /* end_block */
 /* Locals_begin_procedure */
 /* begin_block */
 /* Locals_end_procedure */
 /* end_block */
 /* Locals_begin_procedure */
 /* begin_block */
 /* Locals_end_procedure */
 /* end_block */
 /* Locals_begin_procedure */
 /* begin_block */
 /* Locals_end_procedure */
 /* end_block */
 /* Locals_begin_procedure */
 /* begin_block */
 /* Locals_end_procedure */
 /* end_block */
 /* Locals_begin_procedure */
 /* begin_block */
 /* AllocateTemps_check_range */
 /* AllocateTemps_common */
 /* declare_temp */
 /* declare_local */
 /* Locals_end_procedure */
 /* end_block */
 /* Locals_begin_procedure */
 /* begin_block */
 /* Locals_end_procedure */
 /* end_block */
 /* Locals_begin_procedure */
 /* begin_block */
 /* AllocateTemps_check_range */
 /* AllocateTemps_common */
 /* declare_temp */
 /* declare_local */
 /* Locals_end_procedure */
 /* end_block */
 /* Locals_begin_procedure */
 /* begin_block */
 /* Locals_end_procedure */
 /* end_block */
 /* Locals_begin_procedure */
 /* begin_block */
 /* Locals_end_procedure */
 /* end_block */
 /* Locals_begin_procedure */
 /* begin_block */
 /* Locals_end_procedure */
 /* end_block */
 /* Locals_begin_procedure */
 /* begin_block */
 /* Locals_end_procedure */
 /* end_block */
 /* Locals_begin_procedure */
 /* begin_block */
 /* Locals_end_procedure */
 /* end_block */
 /* Locals_begin_procedure */
 /* begin_block */
 /* AllocateTemps_check_lo */
 /* AllocateTemps_common */
 /* declare_temp */
 /* declare_local */
 /* Locals_end_procedure */
 /* end_block */
 /* Locals_begin_procedure */
 /* begin_block */
 /* Locals_end_procedure */
 /* end_block */
 /* Locals_begin_procedure */
 /* begin_block */
 /* AllocateTemps_check_lo */
 /* AllocateTemps_common */
 /* declare_temp */
 /* declare_local */
 /* Locals_end_procedure */
 /* end_block */
 /* Locals_begin_procedure */
 /* begin_block */
 /* Locals_end_procedure */
 /* end_block */
 /* Locals_begin_procedure */
 /* begin_block */
 /* AllocateTemps_check_range */
 /* AllocateTemps_common */
 /* declare_temp */
 /* declare_local */
 /* Locals_end_procedure */
 /* end_block */
 /* Locals_begin_procedure */
 /* begin_block */
 /* Locals_end_procedure */
 /* end_block */
 /* Locals_begin_procedure */
 /* begin_block */
 /* AllocateTemps_check_range */
 /* AllocateTemps_common */
 /* declare_temp */
 /* declare_local */
 /* Locals_end_procedure */
 /* end_block */
 /* Locals_begin_procedure */
 /* begin_block */
 /* Locals_end_procedure */
 /* end_block */
 /* Locals_begin_procedure */
 /* begin_block */
 /* Locals_end_procedure */
 /* end_block */
 /* Locals_begin_procedure */
 /* begin_block */
 /* Locals_end_procedure */
 /* end_block */
 /* Locals_begin_procedure */
 /* begin_block */
 /* Locals_end_procedure */
 /* end_block */
 /* Locals_begin_procedure */
 /* begin_block */
 /* Locals_end_procedure */
 /* end_block */
 /* Locals_begin_procedure */
 /* begin_block */
 /* AllocateTemps_check_range */
 /* AllocateTemps_common */
 /* declare_temp */
 /* declare_local */
 /* Locals_end_procedure */
 /* end_block */
 /* Locals_begin_procedure */
 /* begin_block */
 /* Locals_end_procedure */
 /* end_block */
 /* Locals_begin_procedure */
 /* begin_block */
 /* AllocateTemps_check_range */
 /* AllocateTemps_common */
 /* declare_temp */
 /* declare_local */
 /* Locals_end_procedure */
 /* end_block */
 /* Locals_begin_procedure */
 /* begin_block */
 /* Locals_end_procedure */
 /* end_block */
 /* Locals_begin_procedure */
 /* begin_block */
 /* AllocateTemps_check_lo */
 /* AllocateTemps_common */
 /* declare_temp */
 /* declare_local */
 /* Locals_end_procedure */
 /* end_block */
 /* Locals_begin_procedure */
 /* begin_block */
 /* Locals_end_procedure */
 /* end_block */
 /* Locals_begin_procedure */
 /* begin_block */
 /* AllocateTemps_check_lo */
 /* AllocateTemps_common */
 /* declare_temp */
 /* declare_local */
 /* Locals_end_procedure */
 /* end_block */
 /* Locals_begin_procedure */
 /* begin_block */
 /* Locals_end_procedure */
 /* end_block */
 /* Locals_begin_procedure */
 /* begin_block */
 /* AllocateTemps_check_lo */
 /* AllocateTemps_common */
 /* declare_temp */
 /* declare_local */
 /* Locals_end_procedure */
 /* end_block */
 /* Locals_begin_procedure */
 /* begin_block */
 /* Locals_end_procedure */
 /* end_block */
 /* Locals_begin_procedure */
 /* begin_block */
 /* AllocateTemps_check_lo */
 /* AllocateTemps_common */
 /* declare_temp */
 /* declare_local */
 /* Locals_end_procedure */
 /* end_block */
 /* Locals_begin_procedure */
 /* begin_block */
 /* Locals_end_procedure */
 /* end_block */
 /* Locals_begin_procedure */
 /* begin_block */
 /* AllocateTemps_check_lo */
 /* AllocateTemps_common */
 /* declare_temp */
 /* declare_local */
 /* Locals_end_procedure */
 /* end_block */
 /* Locals_begin_procedure */
 /* begin_block */
 /* Locals_end_procedure */
 /* end_block */
 /* Locals_begin_procedure */
 /* begin_block */
 /* AllocateTemps_check_lo */
 /* AllocateTemps_common */
 /* declare_temp */
 /* declare_local */
 /* Locals_end_procedure */
 /* end_block */
 /* Locals_begin_procedure */
 /* begin_block */
 /* Locals_end_procedure */
 /* end_block */
 /* Locals_begin_procedure */
 /* begin_block */
 /* Locals_end_procedure */
 /* end_block */
 /* Locals_begin_procedure */
 /* begin_block */
 /* Locals_end_procedure */
 /* end_block */
 /* Locals_begin_procedure */
 /* begin_block */
 /* Locals_end_procedure */
 /* end_block */
 /* Locals_begin_procedure */
 /* begin_block */
 /* Locals_end_procedure */
 /* end_block */
 /* Locals_begin_procedure */
 /* begin_block */
 /* AllocateTemps_check_range */
 /* AllocateTemps_common */
 /* declare_temp */
 /* declare_local */
 /* Locals_end_procedure */
 /* end_block */
 /* Locals_begin_procedure */
 /* begin_block */
 /* Locals_end_procedure */
 /* end_block */
 /* Locals_begin_procedure */
 /* begin_block */
 /* AllocateTemps_check_range */
 /* AllocateTemps_common */
 /* declare_temp */
 /* declare_local */
 /* Locals_end_procedure */
 /* end_block */
 /* Locals_begin_procedure */
 /* begin_block */
 /* Locals_end_procedure */
 /* end_block */
 /* Locals_begin_procedure */
 /* begin_block */
 /* Locals_end_procedure */
 /* end_block */
 /* Locals_begin_procedure */
 /* begin_block */
 /* Locals_end_procedure */
 /* end_block */
 /* Locals_begin_procedure */
 /* begin_block */
 /* Locals_end_procedure */
 /* end_block */
 /* Locals_begin_procedure */
 /* begin_block */
 /* Locals_end_procedure */
 /* end_block */
 /* Locals_begin_procedure */
 /* begin_block */
 /* AllocateTemps_check_range */
 /* AllocateTemps_common */
 /* declare_temp */
 /* declare_local */
 /* Locals_end_procedure */
 /* end_block */
 /* Locals_begin_procedure */
 /* begin_block */
 /* Locals_end_procedure */
 /* end_block */
 /* Locals_begin_procedure */
 /* begin_block */
 /* AllocateTemps_check_range */
 /* AllocateTemps_common */
 /* declare_temp */
 /* declare_local */
 /* Locals_end_procedure */
 /* end_block */
 /* Locals_begin_procedure */
 /* begin_block */
 /* Locals_end_procedure */
 /* end_block */
 /* Locals_begin_procedure */
 /* begin_block */
 /* Locals_end_procedure */
 /* end_block */
 /* Locals_begin_procedure */
 /* begin_block */
 /* Locals_end_procedure */
 /* end_block */
 /* Locals_begin_procedure */
 /* begin_block */
 /* Locals_end_procedure */
 /* end_block */
 /* Locals_begin_procedure */
 /* begin_block */
 /* Locals_end_procedure */
 /* end_block */
 /* Locals_begin_procedure */
 /* begin_block */
 /* AllocateTemps_check_lo */
 /* AllocateTemps_common */
 /* declare_temp */
 /* declare_local */
 /* Locals_end_procedure */
 /* end_block */
 /* Locals_begin_procedure */
 /* begin_block */
 /* Locals_end_procedure */
 /* end_block */
 /* Locals_begin_procedure */
 /* begin_block */
 /* AllocateTemps_check_lo */
 /* AllocateTemps_common */
 /* declare_temp */
 /* declare_local */
 /* Locals_end_procedure */
 /* end_block */
 /* Locals_begin_procedure */
 /* begin_block */
 /* Locals_end_procedure */
 /* end_block */
 /* Locals_begin_procedure */
 /* begin_block */
 /* AllocateTemps_check_range */
 /* AllocateTemps_common */
 /* declare_temp */
 /* declare_local */
 /* Locals_end_procedure */
 /* end_block */
 /* Locals_begin_procedure */
 /* begin_block */
 /* Locals_end_procedure */
 /* end_block */
 /* Locals_begin_procedure */
 /* begin_block */
 /* AllocateTemps_check_range */
 /* AllocateTemps_common */
 /* declare_temp */
 /* declare_local */
 /* Locals_end_procedure */
 /* end_block */
 /* Locals_begin_procedure */
 /* begin_block */
 /* Locals_end_procedure */
 /* end_block */
 /* Locals_begin_procedure */
 /* begin_block */
 /* Locals_end_procedure */
 /* end_block */
 /* Locals_begin_procedure */
 /* begin_block */
 /* Locals_end_procedure */
 /* end_block */
 /* Locals_begin_procedure */
 /* begin_block */
 /* Locals_end_procedure */
 /* end_block */
 /* Locals_begin_procedure */
 /* begin_block */
 /* Locals_end_procedure */
 /* end_block */
 /* Locals_begin_procedure */
 /* begin_block */
 /* AllocateTemps_check_range */
 /* AllocateTemps_common */
 /* declare_temp */
 /* declare_local */
 /* Locals_end_procedure */
 /* end_block */
 /* Locals_begin_procedure */
 /* begin_block */
 /* Locals_end_procedure */
 /* end_block */
 /* Locals_begin_procedure */
 /* begin_block */
 /* AllocateTemps_check_range */
 /* AllocateTemps_common */
 /* declare_temp */
 /* declare_local */
 /* Locals_end_procedure */
 /* end_block */
 /* Locals_begin_procedure */
 /* begin_block */
 /* Locals_end_procedure */
 /* end_block */
 /* Locals_begin_procedure */
 /* begin_block */
 /* AllocateTemps_check_lo */
 /* AllocateTemps_common */
 /* declare_temp */
 /* declare_local */
 /* Locals_end_procedure */
 /* end_block */
 /* Locals_begin_procedure */
 /* begin_block */
 /* Locals_end_procedure */
 /* end_block */
 /* Locals_begin_procedure */
 /* begin_block */
 /* AllocateTemps_check_lo */
 /* AllocateTemps_common */
 /* declare_temp */
 /* declare_local */
 /* Locals_end_procedure */
 /* end_block */
 /* Locals_begin_procedure */
 /* begin_block */
 /* Locals_end_procedure */
 /* end_block */
 /* Locals_begin_procedure */
 /* begin_block */
 /* AllocateTemps_check_lo */
 /* AllocateTemps_common */
 /* declare_temp */
 /* declare_local */
 /* Locals_end_procedure */
 /* end_block */
 /* Locals_begin_procedure */
 /* begin_block */
 /* Locals_end_procedure */
 /* end_block */
 /* Locals_begin_procedure */
 /* begin_block */
 /* AllocateTemps_check_lo */
 /* AllocateTemps_common */
 /* declare_temp */
 /* declare_local */
 /* Locals_end_procedure */
 /* end_block */
 /* Locals_begin_procedure */
 /* begin_block */
 /* Locals_end_procedure */
 /* end_block */
 /* Locals_begin_procedure */
 /* begin_block */
 /* AllocateTemps_check_lo */
 /* AllocateTemps_common */
 /* declare_temp */
 /* declare_local */
 /* Locals_end_procedure */
 /* end_block */
 /* Locals_begin_procedure */
 /* begin_block */
 /* Locals_end_procedure */
 /* end_block */
 /* Locals_begin_procedure */
 /* begin_block */
 /* AllocateTemps_check_lo */
 /* AllocateTemps_common */
 /* declare_temp */
 /* declare_local */
 /* Locals_end_procedure */
 /* end_block */
 /* Locals_begin_procedure */
 /* begin_block */
 /* Locals_end_procedure */
 /* end_block */
 /* Locals_begin_procedure */
 /* begin_block */
 /* Locals_end_procedure */
 /* end_block */
 /* Locals_begin_procedure */
 /* begin_block */
 /* Locals_end_procedure */
 /* end_block */
 /* Locals_begin_procedure */
 /* begin_block */
 /* Locals_end_procedure */
 /* end_block */
 /* Locals_begin_procedure */
 /* begin_block */
 /* Locals_end_procedure */
 /* end_block */
 /* Locals_begin_procedure */
 /* begin_block */
 /* Locals_end_procedure */
 /* end_block */
 /* Locals_begin_procedure */
 /* begin_block */
 /* Locals_end_procedure */
 /* end_block */
 /* Locals_begin_procedure */
 /* begin_block */
 /* Locals_end_procedure */
 /* end_block */
 /* Locals_begin_procedure */
 /* begin_block */
 /* Locals_end_procedure */
 /* end_block */
 /* Locals_begin_procedure */
 /* begin_block */
 /* Locals_end_procedure */
 /* end_block */
 /* Locals_begin_procedure */
 /* begin_block */
 /* Locals_end_procedure */
 /* end_block */
 /* Locals_begin_procedure */
 /* begin_block */
 /* Locals_end_procedure */
 /* end_block */
 /* Locals_begin_procedure */
 /* begin_block */
 /* Locals_end_procedure */
 /* end_block */
 /* Locals_begin_procedure */
 /* begin_block */
 /* Locals_end_procedure */
 /* end_block */
 /* Locals_begin_procedure */
 /* begin_block */
 /* Locals_end_procedure */
 /* end_block */
 /* Locals_begin_procedure */
 /* begin_block */
 /* Locals_end_procedure */
 /* end_block */
 /* Locals_begin_procedure */
 /* begin_block */
 /* Locals_end_procedure */
 /* end_block */
 /* Locals_begin_procedure */
 /* begin_block */
 /* Locals_end_procedure */
 /* end_block */
 /* Locals_begin_procedure */
 /* begin_block */
 /* Locals_end_procedure */
 /* end_block */
 /* Locals_begin_procedure */
 /* begin_block */
 /* Locals_end_procedure */
 /* end_block */
 /* Locals_begin_procedure */
 /* begin_block */
 /* Locals_end_procedure */
 /* end_block */
 /* Locals_begin_procedure */
 /* begin_block */
 /* Locals_end_procedure */
 /* end_block */
 /* Locals_begin_procedure */
 /* begin_block */
 /* Locals_end_procedure */
 /* end_block */
 /* Locals_begin_procedure */
 /* begin_block */
 /* Locals_end_procedure */
 /* end_block */
 /* Locals_begin_procedure */
 /* begin_block */
 /* Locals_end_procedure */
 /* end_block */
 /* Locals_begin_procedure */
 /* begin_block */
 /* Locals_end_procedure */
 /* end_block */
 /* Locals_begin_procedure */
 /* begin_block */
 /* Locals_end_procedure */
 /* end_block */
 /* Locals_begin_procedure */
 /* begin_block */
 /* Locals_end_procedure */
 /* end_block */
 /* Locals_begin_procedure */
 /* begin_block */
 /* Locals_end_procedure */
 /* end_block */
 /* Locals_begin_procedure */
 /* begin_block */
 /* Locals_end_procedure */
 /* end_block */
 /* Locals_begin_procedure */
 /* begin_block */
 /* Locals_end_procedure */
 /* end_block */
 /* Locals_begin_procedure */
 /* begin_block */
 /* Locals_end_procedure */
 /* end_block */
 /* Locals_begin_procedure */
 /* begin_block */
 /* Locals_end_procedure */
 /* end_block */
 /* Locals_begin_procedure */
 /* begin_block */
 /* Locals_end_procedure */
 /* end_block */
 /* Locals_begin_procedure */
 /* begin_block */
 /* Locals_end_procedure */
 /* end_block */
 /* Locals_begin_procedure */
 /* begin_block */
 /* Locals_end_procedure */
 /* end_block */
 /* Locals_begin_procedure */
 /* begin_block */
 /* Locals_end_procedure */
 /* end_block */
 /* Locals_begin_procedure */
 /* begin_block */
 /* Locals_end_procedure */
 /* end_block */
 /* Locals_begin_procedure */
 /* begin_block */
 /* Locals_end_procedure */
 /* end_block */
 /* Locals_begin_procedure */
 /* begin_block */
 /* Locals_end_procedure */
 /* end_block */
 /* Locals_begin_procedure */
 /* begin_block */
 /* Locals_end_procedure */
 /* end_block */
 /* Locals_begin_procedure */
 /* begin_block */
 /* Locals_end_procedure */
 /* end_block */
 /* Locals_begin_procedure */
 /* begin_block */
 /* Locals_end_procedure */
 /* end_block */
 /* Locals_begin_procedure */
 /* begin_block */
 /* Locals_end_procedure */
 /* end_block */
 /* Locals_begin_procedure */
 /* begin_block */
 /* Locals_end_procedure */
 /* end_block */
 /* Locals_begin_procedure */
 /* begin_block */
 /* Locals_end_procedure */
 /* end_block */
 /* Locals_begin_procedure */
 /* begin_block */
 /* Locals_end_procedure */
 /* end_block */
 /* Locals_begin_procedure */
 /* begin_block */
 /* Locals_end_procedure */
 /* end_block */
 /* Locals_begin_procedure */
 /* begin_block */
 /* Locals_end_procedure */
 /* end_block */
 /* Locals_begin_procedure */
 /* begin_block */
 /* Locals_end_procedure */
 /* end_block */
 /* Locals_begin_procedure */
 /* begin_block */
 /* Locals_end_procedure */
 /* end_block */
 /* Locals_begin_procedure */
 /* begin_block */
 /* Locals_end_procedure */
 /* end_block */
 /* Locals_begin_procedure */
 /* begin_block */
 /* Locals_end_procedure */
 /* end_block */
 /* end: locals */
 /* begin: segments/globals */
 /* bind_segment */
 /* begin_init */
 /* init_chars */
 /* init_chars */
 /* init_chars */
 /* init_chars */
 /* init_chars */
 /* init_chars */
 /* init_chars */
 /* init_chars */
 /* init_chars */
 /* init_chars */
 /* init_chars */
 /* init_chars */
 /* init_chars */
 /* init_chars */
 /* init_chars */
 /* init_chars */
 /* init_chars */
 /* init_chars */
 /* init_chars */
 /* init_chars */
 /* init_chars */
 /* init_chars */
 /* init_chars */
 /* init_chars */
 /* init_chars */
 /* init_chars */
 /* init_chars */
 /* init_chars */
 /* init_chars */
 /* init_chars */
 /* init_chars */
 /* init_chars */
 /* init_chars */
 /* init_chars */
 /* init_chars */
 /* init_chars */
 /* init_chars */
 /* init_chars */
 /* init_chars */
 /* init_chars */
 /* init_chars */
 /* init_chars */
 /* init_chars */
 /* init_chars */
 /* init_chars */
 /* init_chars */
 /* init_chars */
 /* init_chars */
 /* init_chars */
 /* init_chars */
 /* init_chars */
 /* init_chars */
 /* init_chars */
 /* init_chars */
 /* init_chars */
 /* init_chars */
 /* init_chars */
 /* init_chars */
 /* init_chars */
 /* init_chars */
 /* init_chars */
 /* init_chars */
 /* init_chars */
 /* init_chars */
 /* init_chars */
 /* init_chars */
 /* init_chars */
 /* init_chars */
 /* init_chars */
 /* init_chars */
 /* init_chars */
 /* init_chars */
 /* init_chars */
 /* init_chars */
 /* init_chars */
 /* init_chars */
 /* init_chars */
 /* init_chars */
 /* init_chars */
 /* init_chars */
 /* init_chars */
 /* init_chars */
 /* init_chars */
 /* init_chars */
 /* init_chars */
 /* init_chars */
 /* init_chars */
 /* init_chars */
 /* init_chars */
 /* init_chars */
 /* init_chars */
 /* init_chars */
 /* init_chars */
 /* init_chars */
 /* init_chars */
 /* init_chars */
 /* init_chars */
 /* init_chars */
 /* init_chars */
 /* init_chars */
 /* init_chars */
 /* init_chars */
 /* init_chars */
 /* init_chars */
 /* init_chars */
 /* init_chars */
 /* init_chars */
 /* init_chars */
 /* init_chars */
 /* init_chars */
 /* init_chars */
 /* init_chars */
 /* init_chars */
 /* init_chars */
 /* init_chars */
 /* init_chars */
 /* init_chars */
 /* init_chars */
 /* init_chars */
 /* init_chars */
 /* init_chars */
 /* init_chars */
 /* init_chars */
 /* init_chars */
 /* init_chars */
 /* init_chars */
 /* init_chars */
 /* init_chars */
 /* init_chars */
 /* init_chars */
 /* init_chars */
 /* init_chars */
 /* init_chars */
 /* init_chars */
 /* init_chars */
 /* init_chars */
 /* init_chars */
 /* init_chars */
 /* init_chars */
 /* init_chars */
 /* init_chars */
 /* init_chars */
 /* init_chars */
 /* init_chars */
 /* init_chars */
 /* init_chars */
 /* init_chars */
 /* init_chars */
 /* init_chars */
 /* init_chars */
 /* init_chars */
 /* init_chars */
 /* init_chars */
 /* init_chars */
 /* init_chars */
 /* init_chars */
 /* init_chars */
 /* init_chars */
 /* init_chars */
 /* init_chars */
 /* init_chars */
 /* init_chars */
 /* init_chars */
 /* init_chars */
 /* init_chars */
 /* init_chars */
 /* init_chars */
 /* init_chars */
 /* init_chars */
 /* init_chars */
 /* init_chars */
 /* init_chars */
 /* init_chars */
 /* init_chars */
 /* init_chars */
 /* init_chars */
 /* init_chars */
 /* init_chars */
 /* init_chars */
 /* init_chars */
 /* init_chars */
 /* init_chars */
 /* init_chars */
 /* init_chars */
 /* init_chars */
 /* init_chars */
 /* init_chars */
 /* init_chars */
 /* init_chars */
 /* init_chars */
 /* init_chars */
 /* init_chars */
 /* init_chars */
 /* init_chars */
 /* init_chars */
 /* init_chars */
 /* init_chars */
 /* init_chars */
 /* init_chars */
 /* init_chars */
 /* init_chars */
 /* init_chars */
 /* init_chars */
 /* init_chars */
 /* init_chars */
 /* init_chars */
 /* init_chars */
 /* init_chars */
 /* init_chars */
 /* init_chars */
 /* init_chars */
 /* init_chars */
 /* init_chars */
 /* init_chars */
 /* init_chars */
 /* init_chars */
 /* init_chars */
 /* init_chars */
 /* init_chars */
 /* init_chars */
 /* init_chars */
 /* init_chars */
 /* init_chars */
 /* init_chars */
 /* init_chars */
 /* init_chars */
 /* init_chars */
 /* init_chars */
 /* init_chars */
 /* init_chars */
 /* init_chars */
 /* init_chars */
 /* init_chars */
 /* init_chars */
 /* init_chars */
 /* init_chars */
 /* init_chars */
 /* init_chars */
 /* init_chars */
 /* init_chars */
 /* init_chars */
 /* init_chars */
 /* init_chars */
 /* init_chars */
 /* init_chars */
 /* init_chars */
 /* init_chars */
 /* init_chars */
 /* init_chars */
 /* init_chars */
 /* init_chars */
 /* init_chars */
 /* init_chars */
 /* init_chars */
 /* init_chars */
 /* init_chars */
 /* init_chars */
 /* init_chars */
 /* init_chars */
 /* init_chars */
 /* init_chars */
 /* init_chars */
 /* init_chars */
 /* init_chars */
 /* init_chars */
 /* init_chars */
 /* init_chars */
 /* init_chars */
 /* init_chars */
 /* init_chars */
 /* init_chars */
 /* init_chars */
 /* init_chars */
 /* init_chars */
 /* init_chars */
 /* init_chars */
 /* init_chars */
 /* init_chars */
 /* init_chars */
 /* init_chars */
 /* init_chars */
 /* init_chars */
 /* init_chars */
 /* init_chars */
 /* init_chars */
 /* init_chars */
 /* init_chars */
 /* init_chars */
 /* init_chars */
 /* init_chars */
 /* init_chars */
 /* init_chars */
 /* init_chars */
 /* init_chars */
 /* init_chars */
 /* init_chars */
 /* init_chars */
 /* init_chars */
 /* init_chars */
 /* init_chars */
 /* init_chars */
 /* init_chars */
 /* init_chars */
 /* init_chars */
 /* init_chars */
 /* init_chars */
 /* init_chars */
 /* init_chars */
 /* init_chars */
 /* init_chars */
 /* init_chars */
 /* init_chars */
 /* init_chars */
 /* init_chars */
 /* init_chars */
 /* init_chars */
 /* init_chars */
 /* init_chars */
 /* init_chars */
 /* init_chars */
 /* init_chars */
 /* init_chars */
 /* init_chars */
 /* init_chars */
 /* init_chars */
 /* init_chars */
 /* init_chars */
 /* init_chars */
 /* init_chars */
 /* init_chars */
 /* init_chars */
 /* init_chars */
 /* init_chars */
 /* init_chars */
 /* init_chars */
 /* init_chars */
 /* init_chars */
 /* init_chars */
 /* init_chars */
 /* init_chars */
 /* init_chars */
 /* init_chars */
 /* init_chars */
 /* init_chars */
 /* init_chars */
 /* init_chars */
 /* init_chars */
 /* init_chars */
 /* init_chars */
 /* init_chars */
 /* init_chars */
 /* init_chars */
 /* init_chars */
 /* init_chars */
 /* init_chars */
 /* init_chars */
 /* init_chars */
 /* init_chars */
 /* init_chars */
 /* init_chars */
 /* init_chars */
 /* init_chars */
 /* init_chars */
 /* init_chars */
 /* init_chars */
 /* init_chars */
 /* init_chars */
 /* init_chars */
 /* init_chars */
 /* init_chars */
 /* init_chars */
 /* init_chars */
 /* init_chars */
 /* init_chars */
 /* init_chars */
 /* init_chars */
 /* init_chars */
 /* init_chars */
 /* init_chars */
 /* init_chars */
 /* init_chars */
 /* init_chars */
 /* init_chars */
 /* init_chars */
 /* init_chars */
 /* init_chars */
 /* init_chars */
 /* init_chars */
 /* init_chars */
 /* init_chars */
 /* init_chars */
 /* init_chars */
 /* init_chars */
 /* init_chars */
 /* init_chars */
 /* init_chars */
 /* init_chars */
 /* init_chars */
 /* init_chars */
 /* init_chars */
 /* init_chars */
 /* init_chars */
 /* init_chars */
 /* init_chars */
 /* init_chars */
 /* init_chars */
 /* init_chars */
 /* init_chars */
 /* init_chars */
 /* init_chars */
 /* init_chars */
 /* init_chars */
 /* init_chars */
 /* init_chars */
 /* init_chars */
 /* init_chars */
 /* init_chars */
 /* init_chars */
 /* init_chars */
 /* init_chars */
 /* init_chars */
 /* init_chars */
 /* init_chars */
 /* init_chars */
 /* init_chars */
 /* init_chars */
 /* init_chars */
 /* init_chars */
 /* init_chars */
 /* init_chars */
 /* init_chars */
 /* init_chars */
 /* init_chars */
 /* init_chars */
 /* init_chars */
 /* init_chars */
 /* init_chars */
 /* init_chars */
 /* init_chars */
 /* init_chars */
 /* init_chars */
 /* init_chars */
 /* init_chars */
 /* init_chars */
 /* init_chars */
 /* init_chars */
 /* init_chars */
 /* init_chars */
 /* init_chars */
 /* init_chars */
 /* init_chars */
 /* init_chars */
 /* init_chars */
 /* init_chars */
 /* init_chars */
 /* init_chars */
 /* init_chars */
 /* init_chars */
 /* init_chars */
 /* init_chars */
 /* init_chars */
 /* init_chars */
 /* init_chars */
 /* init_chars */
 /* init_chars */
 /* init_chars */
 /* init_chars */
 /* init_chars */
 /* init_chars */
 /* init_chars */
 /* init_chars */
 /* init_chars */
 /* init_chars */
 /* init_chars */
 /* init_chars */
 /* init_chars */
 /* init_chars */
 /* init_chars */
 /* init_chars */
 /* init_chars */
 /* init_chars */
 /* init_chars */
 /* init_chars */
 /* init_chars */
 /* init_chars */
 /* init_chars */
 /* init_chars */
 /* init_chars */
 /* init_chars */
 /* init_chars */
 /* init_chars */
 /* init_chars */
 /* init_chars */
 /* init_chars */
 /* init_chars */
 /* init_chars */
 /* init_chars */
 /* init_chars */
 /* init_chars */
 /* init_chars */
 /* init_chars */
 /* init_chars */
 /* init_chars */
 /* init_chars */
 /* init_chars */
 /* init_chars */
 /* init_chars */
 /* init_chars */
 /* init_chars */
 /* init_chars */
 /* init_chars */
 /* init_chars */
 /* init_chars */
 /* init_chars */
 /* init_chars */
 /* init_chars */
 /* init_chars */
 /* init_chars */
 /* init_chars */
 /* init_chars */
 /* init_chars */
 /* init_chars */
 /* init_chars */
 /* init_chars */
 /* init_chars */
 /* init_chars */
 /* init_chars */
 /* init_chars */
 /* init_chars */
 /* init_chars */
 /* init_chars */
 /* init_chars */
 /* init_chars */
 /* init_chars */
 /* init_chars */
 /* init_chars */
 /* init_chars */
 /* init_chars */
 /* init_chars */
 /* init_chars */
 /* init_chars */
 /* init_chars */
 /* init_chars */
 /* init_chars */
 /* init_chars */
 /* init_chars */
 /* init_chars */
 /* init_chars */
 /* init_chars */
 /* init_chars */
 /* init_chars */
 /* init_chars */
 /* init_chars */
 /* init_chars */
 /* init_chars */
 /* init_chars */
 /* init_chars */
 /* init_chars */
 /* init_chars */
 /* init_chars */
 /* init_chars */
 /* init_chars */
 /* init_chars */
 /* init_chars */
 /* init_chars */
 /* init_chars */
 /* init_chars */
 /* init_chars */
 /* init_chars */
 /* init_chars */
 /* init_chars */
 /* init_chars */
 /* init_chars */
 /* init_chars */
 /* init_chars */
 /* init_chars */
 /* init_chars */
 /* init_chars */
 /* init_chars */
 /* init_chars */
 /* init_chars */
 /* init_proc */
 /* init_var */
 /* init_proc */
 /* init_var */
 /* init_proc */
 /* init_var */
 /* init_proc */
 /* init_var */
 /* init_proc */
 /* init_var */
 /* init_proc */
 /* init_var */
 /* init_proc */
 /* init_var */
 /* init_proc */
 /* init_var */
 /* init_proc */
 /* init_var */
 /* init_proc */
 /* init_var */
 /* init_proc */
 /* init_var */
 /* init_proc */
 /* init_var */
 /* init_proc */
 /* init_var */
 /* init_proc */
 /* init_var */
 /* init_proc */
 /* init_var */
 /* init_proc */
 /* init_var */
 /* init_proc */
 /* init_var */
 /* init_proc */
 /* init_var */
 /* init_proc */
 /* init_var */
 /* init_proc */
 /* init_var */
 /* init_proc */
 /* init_var */
 /* init_proc */
 /* init_var */
 /* init_proc */
 /* init_var */
 /* init_proc */
 /* init_var */
 /* init_proc */
 /* init_var */
 /* init_proc */
 /* init_var */
 /* init_proc */
 /* init_var */
 /* init_proc */
 /* init_var */
 /* init_proc */
 /* init_var */
 /* init_proc */
 /* init_var */
 /* init_proc */
 /* init_var */
 /* init_proc */
 /* init_var */
 /* init_proc */
 /* init_var */
 /* init_proc */
 /* init_var */
 /* init_proc */
 /* init_var */
 /* init_proc */
 /* init_var */
 /* init_proc */
 /* init_var */
 /* init_proc */
 /* init_var */
 /* init_proc */
 /* init_var */
 /* init_proc */
 /* init_var */
 /* init_proc */
 /* init_var */
 /* init_proc */
 /* init_var */
 /* init_proc */
 /* init_var */
 /* init_proc */
 /* init_var */
 /* init_proc */
 /* init_var */
 /* init_proc */
 /* init_var */
 /* init_proc */
 /* init_var */
 /* init_proc */
 /* init_var */
 /* init_proc */
 /* init_var */
 /* init_proc */
 /* init_var */
 /* init_proc */
 /* init_var */
 /* init_proc */
 /* init_var */
 /* init_proc */
 /* init_var */
 /* init_proc */
 /* init_var */
 /* init_proc */
 /* init_var */
 /* init_proc */
 /* init_var */
 /* init_proc */
 /* init_var */
 /* init_proc */
 /* init_var */
 /* init_proc */
 /* init_var */
 /* init_proc */
 /* init_var */
 /* init_proc */
 /* init_var */
 /* init_proc */
 /* init_var */
 /* init_proc */
 /* init_var */
 /* init_proc */
 /* init_var */
 /* init_proc */
 /* init_var */
 /* init_proc */
 /* init_var */
 /* init_proc */
 /* init_var */
 /* init_proc */
 /* init_var */
 /* init_proc */
 /* init_var */
 /* init_proc */
 /* init_var */
 /* init_proc */
 /* init_var */
 /* init_proc */
 /* init_var */
 /* init_proc */
 /* init_var */
 /* init_proc */
 /* init_var */
 /* init_proc */
 /* init_var */
 /* init_proc */
 /* init_var */
 /* init_proc */
 /* init_var */
 /* init_proc */
 /* init_var */
 /* init_proc */
 /* init_var */
 /* init_proc */
 /* init_var */
 /* init_proc */
 /* init_var */
 /* init_proc */
 /* init_var */
 /* init_proc */
 /* init_var */
 /* init_proc */
 /* init_var */
 /* init_proc */
 /* init_var */
 /* init_proc */
 /* init_var */
 /* init_proc */
 /* init_var */
 /* init_proc */
 /* init_var */
 /* init_proc */
 /* init_var */
 /* init_proc */
 /* init_var */
 /* init_proc */
 /* init_var */
 /* init_proc */
 /* init_var */
 /* init_proc */
 /* init_var */
 /* init_proc */
 /* init_var */
 /* init_proc */
 /* init_var */
 /* init_proc */
 /* init_var */
 /* init_proc */
 /* init_var */
 /* init_proc */
 /* init_var */
 /* init_proc */
 /* init_var */
 /* init_proc */
 /* init_var */
 /* init_proc */
 /* init_var */
 /* init_proc */
 /* init_var */
 /* init_proc */
 /* init_var */
 /* init_proc */
 /* init_var */
 /* init_proc */
 /* init_var */
 /* init_proc */
 /* init_var */
 /* init_proc */
 /* init_var */
 /* init_proc */
 /* init_var */
 /* init_proc */
 /* init_var */
 /* init_proc */
 /* init_var */
 /* init_proc */
 /* init_var */
 /* init_proc */
 /* init_var */
 /* init_proc */
 /* init_var */
 /* init_proc */
 /* init_var */
 /* init_proc */
 /* init_var */
 /* init_proc */
 /* init_var */
 /* init_proc */
 /* init_var */
 /* init_proc */
 /* init_var */
 /* init_proc */
 /* init_var */
 /* init_proc */
 /* init_var */
 /* init_proc */
 /* init_var */
 /* init_proc */
 /* init_var */
 /* init_proc */
 /* init_var */
 /* init_proc */
 /* init_var */
 /* init_proc */
 /* init_var */
 /* init_proc */
 /* init_var */
 /* init_proc */
 /* init_var */
 /* init_proc */
 /* init_var */
 /* init_proc */
 /* init_var */
 /* init_proc */
 /* init_var */
 /* init_proc */
 /* init_var */
 /* init_proc */
 /* init_var */
 /* init_proc */
 /* init_var */
 /* init_proc */
 /* init_var */
 /* init_proc */
 /* init_var */
 /* init_proc */
 /* init_var */
 /* init_proc */
 /* init_var */
 /* init_proc */
 /* init_var */
 /* init_proc */
 /* init_var */
 /* init_proc */
 /* init_var */
 /* init_proc */
 /* init_var */
 /* init_proc */
 /* init_var */
 /* init_proc */
 /* init_var */
 /* init_proc */
 /* init_var */
 /* init_proc */
 /* init_var */
 /* init_proc */
 /* init_var */
 /* init_proc */
 /* init_var */
 /* init_proc */
 /* init_var */
 /* init_proc */
 /* init_var */
 /* init_proc */
 /* init_var */
 /* init_proc */
 /* init_var */
 /* init_proc */
 /* init_var */
 /* init_proc */
 /* init_var */
 /* init_proc */
 /* init_var */
 /* init_proc */
 /* init_var */
 /* init_proc */
 /* init_var */
 /* init_proc */
 /* init_var */
 /* init_proc */
 /* init_var */
 /* init_proc */
 /* init_var */
 /* init_proc */
 /* init_var */
 /* init_proc */
 /* init_var */
 /* init_proc */
 /* init_var */
 /* init_proc */
 /* init_var */
 /* init_proc */
 /* init_var */
 /* init_proc */
 /* init_var */
 /* init_proc */
 /* init_var */
 /* init_proc */
 /* init_var */
 /* init_proc */
 /* init_var */
 /* init_proc */
 /* init_var */
 /* init_proc */
 /* init_var */
 /* init_proc */
 /* init_var */
 /* init_proc */
 /* init_var */
 /* init_proc */
 /* init_var */
 /* init_proc */
 /* init_var */
 /* init_proc */
 /* init_var */
 /* init_proc */
 /* init_var */
 /* init_proc */
 /* init_var */
 /* init_proc */
 /* init_var */
 /* init_proc */
 /* init_var */
 /* init_proc */
 /* init_var */
 /* init_proc */
 /* init_var */
 /* init_proc */
 /* init_var */
 /* init_proc */
 /* init_var */
 /* init_proc */
 /* init_var */
 /* init_proc */
 /* init_var */
 /* init_proc */
 /* init_var */
 /* init_proc */
 /* init_var */
 /* init_proc */
 /* init_var */
 /* init_proc */
 /* init_var */
 /* init_proc */
 /* init_var */
 /* init_proc */
 /* init_var */
 /* init_proc */
 /* init_var */
 /* init_proc */
 /* init_var */
 /* init_proc */
 /* init_var */
 /* init_proc */
 /* init_var */
 /* init_proc */
 /* init_var */
 /* init_proc */
 /* init_var */
 /* init_proc */
 /* init_var */
 /* init_proc */
 /* init_var */
 /* init_proc */
 /* init_var */
 /* init_proc */
 /* init_var */
 /* init_proc */
 /* init_var */
 /* init_proc */
 /* init_var */
 /* init_proc */
 /* init_var */
 /* init_proc */
 /* init_var */
 /* init_proc */
 /* init_var */
 /* init_proc */
 /* init_var */
 /* init_proc */
 /* init_var */
 /* init_proc */
 /* init_var */
 /* init_proc */
 /* init_var */
 /* init_proc */
 /* init_var */
 /* init_proc */
 /* init_var */
 /* init_proc */
 /* init_var */
 /* init_proc */
 /* init_var */
 /* init_proc */
 /* init_var */
 /* init_proc */
 /* init_var */
 /* init_proc */
 /* init_var */
 /* init_proc */
 /* init_var */
 /* init_proc */
 /* init_var */
 /* init_proc */
 /* init_var */
 /* init_proc */
 /* init_var */
 /* init_proc */
 /* init_var */
 /* init_proc */
 /* init_var */
 /* init_proc */
 /* init_var */
 /* init_proc */
 /* init_var */
 /* init_proc */
 /* init_var */
 /* init_proc */
 /* init_var */
 /* init_proc */
 /* init_var */
 /* init_proc */
 /* init_var */
 /* init_proc */
 /* init_var */
 /* init_proc */
 /* init_var */
 /* init_proc */
 /* init_var */
 /* init_proc */
 /* init_var */
 /* init_proc */
 /* init_var */
 /* init_proc */
 /* init_var */
 /* init_proc */
 /* init_var */
 /* init_proc */
 /* init_var */
 /* init_proc */
 /* init_var */
 /* init_proc */
 /* init_var */
 /* init_proc */
 /* init_var */
 /* init_proc */
 /* init_var */
 /* init_proc */
 /* init_var */
 /* init_proc */
 /* init_var */
 /* init_proc */
 /* init_var */
 /* init_proc */
 /* init_var */
 /* init_proc */
 /* init_var */
 /* init_proc */
 /* init_var */
 /* init_proc */
 /* init_var */
 /* init_proc */
 /* init_var */
 /* init_proc */
 /* init_var */
 /* init_proc */
 /* init_var */
 /* init_proc */
 /* init_var */
 /* init_proc */
 /* init_var */
 /* init_proc */
 /* init_var */
 /* init_proc */
 /* init_var */
 /* init_proc */
 /* init_var */
 /* init_proc */
 /* init_var */
 /* init_proc */
 /* init_var */
 /* init_proc */
 /* init_var */
 /* init_proc */
 /* init_var */
 /* init_proc */
 /* init_var */
 /* init_proc */
 /* init_var */
 /* init_proc */
 /* init_var */
 /* init_proc */
 /* init_var */
 /* init_proc */
 /* init_var */
 /* init_proc */
 /* init_var */
 /* init_proc */
 /* init_var */
 /* init_proc */
 /* init_var */
 /* init_proc */
 /* init_var */
 /* init_proc */
 /* init_var */
 /* init_proc */
 /* init_var */
 /* init_proc */
 /* init_var */
 /* init_proc */
 /* init_var */
 /* init_proc */
 /* init_var */
 /* init_proc */
 /* init_var */
 /* init_proc */
 /* init_var */
 /* init_proc */
 /* init_var */
 /* init_proc */
 /* init_var */
 /* init_proc */
 /* init_var */
 /* init_proc */
 /* init_var */
 /* init_proc */
 /* init_var */
 /* init_proc */
 /* init_var */
 /* init_proc */
 /* init_var */
 /* init_proc */
 /* init_var */
 /* init_proc */
 /* init_var */
 /* init_proc */
 /* init_var */
 /* init_proc */
 /* init_var */
 /* init_proc */
 /* init_var */
 /* init_proc */
 /* init_var */
 /* init_proc */
 /* init_var */
 /* init_proc */
 /* init_var */
 /* init_proc */
 /* init_var */
 /* init_proc */
 /* init_var */
 /* init_proc */
 /* init_var */
 /* init_proc */
 /* init_var */
 /* init_proc */
 /* init_var */
 /* init_proc */
 /* init_var */
 /* init_proc */
 /* init_var */
 /* init_proc */
 /* init_var */
 /* init_proc */
 /* init_var */
 /* init_proc */
 /* init_var */
 /* init_proc */
 /* init_var */
 /* init_proc */
 /* init_var */
 /* init_proc */
 /* init_var */
 /* init_proc */
 /* init_var */
 /* init_proc */
 /* init_var */
 /* init_proc */
 /* init_var */
 /* init_proc */
 /* init_var */
 /* init_proc */
 /* init_var */
 /* init_proc */
 /* init_var */
 /* init_proc */
 /* init_var */
 /* init_proc */
 /* init_var */
 /* init_proc */
 /* init_var */
 /* init_proc */
 /* init_var */
 /* init_proc */
 /* init_var */
 /* init_proc */
 /* init_var */
 /* init_proc */
 /* init_var */
 /* init_proc */
 /* init_var */
 /* init_proc */
 /* init_var */
 /* init_proc */
 /* init_var */
 /* init_proc */
 /* init_var */
 /* init_proc */
 /* init_var */
 /* init_proc */
 /* init_var */
 /* init_proc */
 /* init_var */
 /* init_proc */
 /* init_var */
 /* init_proc */
 /* init_var */
 /* init_proc */
 /* init_var */
 /* init_proc */
 /* init_var */
 /* init_proc */
 /* init_var */
 /* init_proc */
 /* init_var */
 /* init_proc */
 /* init_var */
 /* init_proc */
 /* init_var */
 /* init_proc */
 /* init_var */
 /* init_proc */
 /* init_var */
 /* init_proc */
 /* init_var */
 /* init_proc */
 /* init_var */
 /* init_proc */
 /* init_var */
 /* init_proc */
 /* init_var */
 /* init_proc */
 /* init_var */
 /* init_proc */
 /* init_var */
 /* init_proc */
 /* init_var */
 /* init_proc */
 /* init_var */
 /* init_proc */
 /* init_var */
 /* init_proc */
 /* init_var */
 /* init_proc */
 /* init_var */
 /* init_proc */
 /* init_var */
 /* init_proc */
 /* init_var */
 /* init_proc */
 /* init_var */
 /* init_proc */
 /* init_var */
 /* init_proc */
 /* init_var */
 /* init_proc */
 /* init_var */
 /* init_proc */
 /* init_var */
 /* init_proc */
 /* init_var */
 /* init_proc */
 /* init_var */
 /* init_proc */
 /* init_var */
 /* init_proc */
 /* init_var */
 /* init_proc */
 /* init_var */
 /* init_proc */
 /* init_var */
 /* init_proc */
 /* init_var */
 /* init_proc */
 /* init_var */
 /* init_proc */
 /* init_var */
 /* init_proc */
 /* init_var */
 /* init_proc */
 /* init_var */
 /* init_proc */
 /* init_var */
 /* init_proc */
 /* init_var */
 /* init_proc */
 /* init_var */
 /* init_proc */
 /* init_var */
 /* init_proc */
 /* init_var */
 /* init_proc */
 /* init_var */
 /* init_proc */
 /* init_var */
 /* init_proc */
 /* init_var */
 /* init_proc */
 /* init_var */
 /* init_proc */
 /* init_var */
 /* init_proc */
 /* init_var */
 /* init_proc */
 /* init_var */
 /* init_proc */
 /* init_var */
 /* init_proc */
 /* init_var */
 /* init_proc */
 /* init_var */
 /* init_proc */
 /* init_var */
 /* init_proc */
 /* init_var */
 /* init_proc */
 /* init_var */
 /* init_proc */
 /* init_var */
 /* init_proc */
 /* init_var */
 /* init_proc */
 /* init_var */
 /* init_proc */
 /* init_var */
 /* init_proc */
 /* init_var */
 /* init_proc */
 /* init_var */
 /* init_proc */
 /* init_var */
 /* init_proc */
 /* init_var */
 /* init_proc */
 /* init_var */
 /* init_proc */
 /* init_var */
 /* init_proc */
 /* init_var */
 /* init_proc */
 /* init_var */
 /* init_proc */
 /* init_var */
 /* init_proc */
 /* init_var */
 /* init_proc */
 /* init_var */
 /* init_proc */
 /* init_var */
 /* init_proc */
 /* init_var */
 /* init_proc */
 /* init_var */
 /* init_proc */
 /* init_var */
 /* init_proc */
 /* init_var */
 /* init_proc */
 /* init_var */
 /* init_proc */
 /* init_var */
 /* init_proc */
 /* init_var */
 /* init_proc */
 /* init_var */
 /* init_proc */
 /* init_var */
 /* init_proc */
 /* init_var */
 /* init_proc */
 /* init_var */
 /* init_proc */
 /* init_var */
 /* init_proc */
 /* init_var */
 /* init_proc */
 /* init_var */
 /* init_proc */
 /* init_var */
 /* init_proc */
 /* init_var */
 /* init_proc */
 /* init_var */
 /* init_proc */
 /* init_var */
 /* init_proc */
 /* init_var */
 /* init_proc */
 /* init_var */
 /* init_proc */
 /* init_var */
 /* init_proc */
 /* init_var */
 /* init_proc */
 /* init_var */
 /* init_proc */
 /* init_var */
 /* init_proc */
 /* init_var */
 /* init_proc */
 /* init_var */
 /* init_proc */
 /* init_var */
 /* init_proc */
 /* init_var */
 /* init_proc */
 /* init_var */
 /* init_proc */
 /* init_var */
 /* init_proc */
 /* init_var */
 /* init_proc */
 /* init_var */
 /* init_proc */
 /* init_var */
 /* init_proc */
 /* init_var */
 /* init_proc */
 /* init_var */
 /* init_proc */
 /* init_var */
 /* init_proc */
 /* init_var */
 /* init_proc */
 /* init_var */
 /* init_proc */
 /* init_var */
 /* init_proc */
 /* init_var */
 /* init_proc */
 /* init_var */
 /* init_proc */
 /* init_var */
 /* init_proc */
 /* init_var */
 /* init_proc */
 /* init_var */
 /* init_proc */
 /* init_var */
 /* init_proc */
 /* init_var */
 /* init_proc */
 /* init_var */
 /* init_proc */
 /* init_var */
 /* init_proc */
 /* init_var */
 /* init_proc */
 /* init_var */
 /* init_proc */
 /* init_var */
 /* init_proc */
 /* init_var */
 /* init_proc */
 /* init_var */
 /* init_proc */
 /* init_var */
 /* init_proc */
 /* init_var */
 /* init_proc */
 /* init_var */
 /* init_proc */
 /* init_var */
 /* init_proc */
 /* init_var */
 /* init_proc */
 /* init_var */
 /* init_proc */
 /* init_var */
 /* init_proc */
 /* init_var */
 /* init_proc */
 /* init_var */
 /* init_proc */
 /* init_var */
 /* init_proc */
 /* init_var */
 /* init_proc */
 /* init_var */
 /* init_proc */
 /* init_var */
 /* init_proc */
 /* init_var */
 /* init_proc */
 /* init_var */
 /* init_proc */
 /* init_var */
 /* init_proc */
 /* init_var */
 /* init_proc */
 /* init_var */
 /* init_proc */
 /* init_var */
 /* init_proc */
 /* init_var */
 /* init_proc */
 /* init_var */
 /* init_proc */
 /* init_var */
 /* init_proc */
 /* init_var */
 /* init_proc */
 /* init_var */
 /* init_proc */
 /* init_var */
 /* init_proc */
 /* init_var */
 /* init_proc */
 /* init_var */
 /* init_proc */
 /* init_var */
 /* init_proc */
 /* init_var */
 /* init_proc */
 /* init_var */
 /* init_proc */
 /* init_var */
 /* init_proc */
 /* init_var */
 /* init_proc */
 /* init_var */
 /* init_proc */
 /* init_var */
 /* init_proc */
 /* init_var */
 /* init_proc */
 /* init_var */
 /* init_proc */
 /* init_var */
 /* init_proc */
 /* init_var */
 /* init_proc */
 /* init_var */
 /* init_proc */
 /* init_var */
 /* init_proc */
 /* init_var */
 /* init_proc */
 /* init_var */
 /* init_proc */
 /* init_var */
 /* init_proc */
 /* init_var */
 /* init_proc */
 /* init_var */
 /* init_proc */
 /* init_var */
 /* init_proc */
 /* init_var */
 /* init_proc */
 /* init_var */
 /* init_proc */
 /* init_var */
 /* init_proc */
 /* init_var */
 /* init_proc */
 /* init_var */
 /* init_proc */
 /* init_var */
 /* init_proc */
 /* init_var */
 /* init_proc */
 /* init_var */
 /* init_proc */
 /* init_var */
 /* init_proc */
 /* init_var */
 /* init_proc */
 /* init_var */
 /* init_proc */
 /* init_var */
 /* init_proc */
 /* init_var */
 /* init_proc */
 /* init_var */
 /* init_proc */
 /* init_var */
 /* init_proc */
 /* init_var */
 /* init_proc */
 /* init_var */
 /* init_proc */
 /* init_var */
 /* init_proc */
 /* init_var */
 /* init_proc */
 /* init_var */
 /* init_proc */
 /* init_var */
 /* init_proc */
 /* init_var */
 /* init_proc */
 /* init_var */
 /* init_proc */
 /* init_var */
 /* init_proc */
 /* init_var */
 /* init_proc */
 /* init_var */
 /* init_proc */
 /* init_var */
 /* init_proc */
 /* init_var */
 /* init_proc */
 /* init_var */
 /* init_proc */
 /* init_var */
 /* init_proc */
 /* init_var */
 /* init_proc */
 /* init_var */
 /* init_proc */
 /* init_var */
 /* init_proc */
 /* init_var */
 /* init_proc */
 /* init_var */
 /* init_proc */
 /* init_var */
 /* init_proc */
 /* init_var */
 /* init_proc */
 /* init_var */
 /* init_proc */
 /* init_var */
 /* init_proc */
 /* init_var */
 /* init_proc */
 /* init_var */
 /* init_proc */
 /* init_var */
 /* init_proc */
 /* init_var */
 /* init_proc */
 /* init_var */
 /* init_proc */
 /* init_var */
 /* init_proc */
 /* init_var */
 /* init_proc */
 /* init_var */
 /* init_proc */
 /* init_var */
 /* init_proc */
 /* init_var */
 /* init_proc */
 /* init_var */
 /* init_proc */
 /* init_var */
 /* init_proc */
 /* init_var */
 /* init_proc */
 /* init_var */
 /* init_proc */
 /* init_var */
 /* init_proc */
 /* init_var */
 /* init_proc */
 /* init_var */
 /* init_proc */
 /* init_var */
 /* init_proc */
 /* init_var */
 /* init_proc */
 /* init_var */
 /* init_proc */
 /* init_var */
 /* init_proc */
 /* init_var */
 /* init_proc */
 /* init_var */
 /* init_proc */
 /* init_var */
 /* init_proc */
 /* init_var */
 /* init_proc */
 /* init_var */
 /* init_proc */
 /* init_var */
 /* init_proc */
 /* init_var */
 /* init_proc */
 /* init_var */
 /* init_proc */
 /* init_var */
 /* init_proc */
 /* init_var */
 /* init_proc */
 /* init_var */
 /* init_proc */
 /* init_var */
 /* init_proc */
 /* init_var */
 /* init_proc */
 /* init_var */
 /* init_proc */
 /* init_var */
 /* init_proc */
 /* init_var */
 /* init_proc */
 /* init_var */
 /* init_proc */
 /* init_var */
 /* init_proc */
 /* init_var */
 /* init_proc */
 /* init_var */
 /* init_proc */
 /* init_var */
 /* init_proc */
 /* init_var */
 /* init_proc */
 /* init_var */
 /* init_proc */
 /* init_var */
 /* init_proc */
 /* init_var */
 /* init_chars */
 /* end_init */
struct Minus_m_11_L_12_t{UINT8 L_1400[8];
char L_1401[1];
UINT8 L_1402[15];
char L_1403[1];
UINT8 L_1404[16];
char L_1405[1];
UINT8 L_1406[13];
char L_1407[1];
UINT8 L_1408[14];
char L_1409[1];
UINT8 L_1410[16];
char L_1411[1];
UINT8 L_1412[17];
char L_1413[1];
UINT8 L_1414[14];
char L_1415[1];
UINT8 L_1416[15];
char L_1417[1];
UINT8 L_1418[17];
char L_1419[1];
UINT8 L_1420[18];
char L_1421[1];
UINT8 L_1422[15];
char L_1423[1];
UINT8 L_1424[16];
char L_1425[1];
UINT8 L_1426[15];
char L_1427[1];
UINT8 L_1428[16];
char L_1429[1];
UINT8 L_1430[13];
char L_1431[1];
UINT8 L_1432[14];
char L_1433[1];
UINT8 L_1434[17];
char L_1435[1];
UINT8 L_1436[18];
char L_1437[1];
UINT8 L_1438[15];
char L_1439[1];
UINT8 L_1440[16];
char L_1441[1];
UINT8 L_1442[17];
char L_1443[1];
UINT8 L_1444[18];
char L_1445[1];
UINT8 L_1446[15];
char L_1447[1];
UINT8 L_1448[16];
char L_1449[1];
UINT8 L_1450[15];
char L_1451[1];
UINT8 L_1452[16];
char L_1453[1];
UINT8 L_1454[13];
char L_1455[1];
UINT8 L_1456[14];
char L_1457[1];
UINT8 L_1458[17];
char L_1459[1];
UINT8 L_1460[18];
char L_1461[1];
UINT8 L_1462[15];
char L_1463[1];
UINT8 L_1464[16];
char L_1465[1];
UINT8 L_1466[16];
char L_1467[1];
UINT8 L_1468[17];
char L_1469[1];
UINT8 L_1470[14];
char L_1471[1];
UINT8 L_1472[15];
char L_1473[1];
UINT8 L_1474[17];
char L_1475[1];
UINT8 L_1476[18];
char L_1477[1];
UINT8 L_1478[15];
char L_1479[1];
UINT8 L_1480[16];
char L_1481[1];
UINT8 L_1482[17];
char L_1483[1];
UINT8 L_1484[18];
char L_1485[1];
UINT8 L_1486[15];
char L_1487[1];
UINT8 L_1488[16];
char L_1489[1];
UINT8 L_1490[16];
char L_1491[1];
UINT8 L_1492[17];
char L_1493[1];
UINT8 L_1494[14];
char L_1495[1];
UINT8 L_1496[15];
char L_1497[1];
UINT8 L_1498[16];
char L_1499[1];
UINT8 L_1500[17];
char L_1501[1];
UINT8 L_1502[14];
char L_1503[1];
UINT8 L_1504[15];
char L_1505[1];
UINT8 L_1506[17];
char L_1507[1];
UINT8 L_1508[18];
char L_1509[1];
UINT8 L_1510[15];
char L_1511[1];
UINT8 L_1512[16];
char L_1513[1];
UINT8 L_1514[18];
char L_1515[1];
UINT8 L_1516[19];
char L_1517[1];
UINT8 L_1518[16];
char L_1519[1];
UINT8 L_1520[17];
char L_1521[1];
UINT8 L_1522[16];
char L_1523[1];
UINT8 L_1524[17];
char L_1525[1];
UINT8 L_1526[14];
char L_1527[1];
UINT8 L_1528[15];
char L_1529[1];
UINT8 L_1530[18];
char L_1531[1];
UINT8 L_1532[19];
char L_1533[1];
UINT8 L_1534[16];
char L_1535[1];
UINT8 L_1536[17];
char L_1537[1];
UINT8 L_1538[18];
char L_1539[1];
UINT8 L_1540[19];
char L_1541[1];
UINT8 L_1542[16];
char L_1543[1];
UINT8 L_1544[17];
char L_1545[1];
UINT8 L_1546[16];
char L_1547[1];
UINT8 L_1548[17];
char L_1549[1];
UINT8 L_1550[14];
char L_1551[1];
UINT8 L_1552[15];
char L_1553[1];
UINT8 L_1554[18];
char L_1555[1];
UINT8 L_1556[19];
char L_1557[1];
UINT8 L_1558[16];
char L_1559[1];
UINT8 L_1560[17];
char L_1561[1];
UINT8 L_1562[17];
char L_1563[1];
UINT8 L_1564[18];
char L_1565[1];
UINT8 L_1566[15];
char L_1567[1];
UINT8 L_1568[16];
char L_1569[1];
UINT8 L_1570[18];
char L_1571[1];
UINT8 L_1572[19];
char L_1573[1];
UINT8 L_1574[16];
char L_1575[1];
UINT8 L_1576[17];
char L_1577[1];
UINT8 L_1578[18];
char L_1579[1];
UINT8 L_1580[19];
char L_1581[1];
UINT8 L_1582[16];
char L_1583[1];
UINT8 L_1584[17];
char L_1585[1];
UINT8 L_1586[17];
char L_1587[1];
UINT8 L_1588[18];
char L_1589[1];
UINT8 L_1590[15];
char L_1591[1];
UINT8 L_1592[16];
char L_1593[1];
UINT8 L_1594[17];
char L_1595[1];
UINT8 L_1596[18];
char L_1597[1];
UINT8 L_1598[15];
char L_1599[1];
UINT8 L_1600[16];
char L_1601[1];
UINT8 L_1602[18];
char L_1603[1];
UINT8 L_1604[19];
char L_1605[1];
UINT8 L_1606[16];
char L_1607[1];
UINT8 L_1608[17];
char L_1609[1];
UINT8 L_1610[19];
char L_1611[1];
UINT8 L_1612[20];
char L_1613[1];
UINT8 L_1614[17];
char L_1615[1];
UINT8 L_1616[18];
char L_1617[1];
UINT8 L_1618[17];
char L_1619[1];
UINT8 L_1620[18];
char L_1621[1];
UINT8 L_1622[15];
char L_1623[1];
UINT8 L_1624[16];
char L_1625[1];
UINT8 L_1626[19];
char L_1627[1];
UINT8 L_1628[20];
char L_1629[1];
UINT8 L_1630[17];
char L_1631[1];
UINT8 L_1632[18];
char L_1633[1];
UINT8 L_1634[19];
char L_1635[1];
UINT8 L_1636[20];
char L_1637[1];
UINT8 L_1638[17];
char L_1639[1];
UINT8 L_1640[18];
char L_1641[1];
UINT8 L_1642[17];
char L_1643[1];
UINT8 L_1644[18];
char L_1645[1];
UINT8 L_1646[15];
char L_1647[1];
UINT8 L_1648[16];
char L_1649[1];
UINT8 L_1650[19];
char L_1651[1];
UINT8 L_1652[20];
char L_1653[1];
UINT8 L_1654[17];
char L_1655[1];
UINT8 L_1656[18];
char L_1657[1];
UINT8 L_1658[18];
char L_1659[1];
UINT8 L_1660[19];
char L_1661[1];
UINT8 L_1662[16];
char L_1663[1];
UINT8 L_1664[17];
char L_1665[1];
UINT8 L_1666[19];
char L_1667[1];
UINT8 L_1668[20];
char L_1669[1];
UINT8 L_1670[17];
char L_1671[1];
UINT8 L_1672[18];
char L_1673[1];
UINT8 L_1674[19];
char L_1675[1];
UINT8 L_1676[20];
char L_1677[1];
UINT8 L_1678[17];
char L_1679[1];
UINT8 L_1680[18];
char L_1681[1];
UINT8 L_1682[18];
char L_1683[1];
UINT8 L_1684[19];
char L_1685[1];
UINT8 L_1686[16];
char L_1687[1];
UINT8 L_1688[17];
char L_1689[1];
UINT8 L_1690[15];
char L_1691[1];
UINT8 L_1692[16];
char L_1693[1];
UINT8 L_1694[13];
char L_1695[1];
UINT8 L_1696[14];
char L_1697[1];
UINT8 L_1698[16];
char L_1699[1];
UINT8 L_1700[17];
char L_1701[1];
UINT8 L_1702[14];
char L_1703[1];
UINT8 L_1704[15];
char L_1705[1];
UINT8 L_1706[17];
char L_1707[1];
UINT8 L_1708[18];
char L_1709[1];
UINT8 L_1710[15];
char L_1711[1];
UINT8 L_1712[16];
char L_1713[1];
UINT8 L_1714[15];
char L_1715[1];
UINT8 L_1716[16];
char L_1717[1];
UINT8 L_1718[13];
char L_1719[1];
UINT8 L_1720[14];
char L_1721[1];
UINT8 L_1722[17];
char L_1723[1];
UINT8 L_1724[18];
char L_1725[1];
UINT8 L_1726[15];
char L_1727[1];
UINT8 L_1728[16];
char L_1729[1];
UINT8 L_1730[17];
char L_1731[1];
UINT8 L_1732[18];
char L_1733[1];
UINT8 L_1734[15];
char L_1735[1];
UINT8 L_1736[16];
char L_1737[1];
UINT8 L_1738[15];
char L_1739[1];
UINT8 L_1740[16];
char L_1741[1];
UINT8 L_1742[13];
char L_1743[1];
UINT8 L_1744[14];
char L_1745[1];
UINT8 L_1746[17];
char L_1747[1];
UINT8 L_1748[18];
char L_1749[1];
UINT8 L_1750[15];
char L_1751[1];
UINT8 L_1752[16];
char L_1753[1];
UINT8 L_1754[16];
char L_1755[1];
UINT8 L_1756[17];
char L_1757[1];
UINT8 L_1758[14];
char L_1759[1];
UINT8 L_1760[15];
char L_1761[1];
UINT8 L_1762[17];
char L_1763[1];
UINT8 L_1764[18];
char L_1765[1];
UINT8 L_1766[15];
char L_1767[1];
UINT8 L_1768[16];
char L_1769[1];
UINT8 L_1770[17];
char L_1771[1];
UINT8 L_1772[18];
char L_1773[1];
UINT8 L_1774[15];
char L_1775[1];
UINT8 L_1776[16];
char L_1777[1];
UINT8 L_1778[16];
char L_1779[1];
UINT8 L_1780[17];
char L_1781[1];
UINT8 L_1782[14];
char L_1783[1];
UINT8 L_1784[15];
char L_1785[1];
UINT8 L_1786[17];
char L_1787[1];
UINT8 L_1788[18];
char L_1789[1];
UINT8 L_1790[15];
char L_1791[1];
UINT8 L_1792[16];
char L_1793[1];
UINT8 L_1794[18];
char L_1795[1];
UINT8 L_1796[19];
char L_1797[1];
UINT8 L_1798[16];
char L_1799[1];
UINT8 L_1800[17];
char L_1801[1];
UINT8 L_1802[19];
char L_1803[1];
UINT8 L_1804[20];
char L_1805[1];
UINT8 L_1806[17];
char L_1807[1];
UINT8 L_1808[18];
char L_1809[1];
UINT8 L_1810[17];
char L_1811[1];
UINT8 L_1812[18];
char L_1813[1];
UINT8 L_1814[15];
char L_1815[1];
UINT8 L_1816[16];
char L_1817[1];
UINT8 L_1818[19];
char L_1819[1];
UINT8 L_1820[20];
char L_1821[1];
UINT8 L_1822[17];
char L_1823[1];
UINT8 L_1824[18];
char L_1825[1];
UINT8 L_1826[19];
char L_1827[1];
UINT8 L_1828[20];
char L_1829[1];
UINT8 L_1830[17];
char L_1831[1];
UINT8 L_1832[18];
char L_1833[1];
UINT8 L_1834[17];
char L_1835[1];
UINT8 L_1836[18];
char L_1837[1];
UINT8 L_1838[15];
char L_1839[1];
UINT8 L_1840[16];
char L_1841[1];
UINT8 L_1842[19];
char L_1843[1];
UINT8 L_1844[20];
char L_1845[1];
UINT8 L_1846[17];
char L_1847[1];
UINT8 L_1848[18];
char L_1849[1];
UINT8 L_1850[18];
char L_1851[1];
UINT8 L_1852[19];
char L_1853[1];
UINT8 L_1854[16];
char L_1855[1];
UINT8 L_1856[17];
char L_1857[1];
UINT8 L_1858[19];
char L_1859[1];
UINT8 L_1860[20];
char L_1861[1];
UINT8 L_1862[17];
char L_1863[1];
UINT8 L_1864[18];
char L_1865[1];
UINT8 L_1866[19];
char L_1867[1];
UINT8 L_1868[20];
char L_1869[1];
UINT8 L_1870[17];
char L_1871[1];
UINT8 L_1872[18];
char L_1873[1];
UINT8 L_1874[18];
char L_1875[1];
UINT8 L_1876[19];
char L_1877[1];
UINT8 L_1878[16];
char L_1879[1];
UINT8 L_1880[17];
char L_1881[1];
UINT8 L_1882[19];
char L_1883[1];
UINT8 L_1884[17];
char L_1885[1];
UINT8 L_1886[17];
char L_1887[1];
UINT8 L_1888[18];
char L_1889[1];
UINT8 L_1890[15];
char L_1891[1];
UINT8 L_1892[16];
char L_1893[1];
UINT8 L_1894[18];
char L_1895[1];
UINT8 L_1896[19];
char L_1897[1];
UINT8 L_1898[16];
char L_1899[1];
UINT8 L_1900[17];
char L_1901[1];
UINT8 L_1902[19];
char L_1903[1];
UINT8 L_1904[20];
char L_1905[1];
UINT8 L_1906[17];
char L_1907[1];
UINT8 L_1908[18];
char L_1909[1];
UINT8 L_1910[17];
char L_1911[1];
UINT8 L_1912[18];
char L_1913[1];
UINT8 L_1914[15];
char L_1915[1];
UINT8 L_1916[16];
char L_1917[1];
UINT8 L_1918[19];
char L_1919[1];
UINT8 L_1920[20];
char L_1921[1];
UINT8 L_1922[17];
char L_1923[1];
UINT8 L_1924[18];
char L_1925[1];
UINT8 L_1926[19];
char L_1927[1];
UINT8 L_1928[20];
char L_1929[1];
UINT8 L_1930[17];
char L_1931[1];
UINT8 L_1932[18];
char L_1933[1];
UINT8 L_1934[17];
char L_1935[1];
UINT8 L_1936[18];
char L_1937[1];
UINT8 L_1938[15];
char L_1939[1];
UINT8 L_1940[16];
char L_1941[1];
UINT8 L_1942[19];
char L_1943[1];
UINT8 L_1944[20];
char L_1945[1];
UINT8 L_1946[17];
char L_1947[1];
UINT8 L_1948[18];
char L_1949[1];
UINT8 L_1950[18];
char L_1951[1];
UINT8 L_1952[19];
char L_1953[1];
UINT8 L_1954[16];
char L_1955[1];
UINT8 L_1956[17];
char L_1957[1];
UINT8 L_1958[19];
char L_1959[1];
UINT8 L_1960[20];
char L_1961[1];
UINT8 L_1962[17];
char L_1963[1];
UINT8 L_1964[18];
char L_1965[1];
UINT8 L_1966[19];
char L_1967[1];
UINT8 L_1968[20];
char L_1969[1];
UINT8 L_1970[17];
char L_1971[1];
UINT8 L_1972[18];
char L_1973[1];
UINT8 L_1974[18];
char L_1975[1];
UINT8 L_1976[19];
char L_1977[1];
UINT8 L_1978[16];
char L_1979[1];
UINT8 L_1980[17];
char L_1981[1];
UINT8 L_1982[15];
char L_1983[1];
UINT8 L_1984[16];
char L_1985[1];
UINT8 L_1986[13];
char L_1987[1];
UINT8 L_1988[14];
char L_1989[1];
UINT8 L_1990[16];
char L_1991[1];
UINT8 L_1992[17];
char L_1993[1];
UINT8 L_1994[14];
char L_1995[1];
UINT8 L_1996[15];
char L_1997[1];
UINT8 L_1998[17];
char L_1999[1];
UINT8 L_2000[18];
char L_2001[1];
UINT8 L_2002[15];
char L_2003[1];
UINT8 L_2004[16];
char L_2005[1];
UINT8 L_2006[15];
char L_2007[1];
UINT8 L_2008[16];
char L_2009[1];
UINT8 L_2010[13];
char L_2011[1];
UINT8 L_2012[14];
char L_2013[1];
UINT8 L_2014[17];
char L_2015[1];
UINT8 L_2016[18];
char L_2017[1];
UINT8 L_2018[15];
char L_2019[1];
UINT8 L_2020[16];
char L_2021[1];
UINT8 L_2022[17];
char L_2023[1];
UINT8 L_2024[18];
char L_2025[1];
UINT8 L_2026[15];
char L_2027[1];
UINT8 L_2028[16];
char L_2029[1];
UINT8 L_2030[15];
char L_2031[1];
UINT8 L_2032[16];
char L_2033[1];
UINT8 L_2034[13];
char L_2035[1];
UINT8 L_2036[14];
char L_2037[1];
UINT8 L_2038[17];
char L_2039[1];
UINT8 L_2040[18];
char L_2041[1];
UINT8 L_2042[15];
char L_2043[1];
UINT8 L_2044[16];
char L_2045[1];
UINT8 L_2046[16];
char L_2047[1];
UINT8 L_2048[17];
char L_2049[1];
UINT8 L_2050[14];
char L_2051[1];
UINT8 L_2052[15];
char L_2053[1];
UINT8 L_2054[17];
char L_2055[1];
UINT8 L_2056[18];
char L_2057[1];
UINT8 L_2058[15];
char L_2059[1];
UINT8 L_2060[16];
char L_2061[1];
UINT8 L_2062[17];
char L_2063[1];
UINT8 L_2064[18];
char L_2065[1];
UINT8 L_2066[15];
char L_2067[1];
UINT8 L_2068[16];
char L_2069[1];
UINT8 L_2070[16];
char L_2071[1];
UINT8 L_2072[17];
char L_2073[1];
UINT8 L_2074[14];
char L_2075[1];
UINT8 L_2076[15];
char L_2077[1];
UINT8 L_2078[17];
char L_2079[1];
UINT8 L_2080[18];
char L_2081[1];
UINT8 L_2082[15];
char L_2083[1];
UINT8 L_2084[16];
char L_2085[1];
UINT8 L_2086[18];
char L_2087[1];
UINT8 L_2088[19];
char L_2089[1];
UINT8 L_2090[16];
char L_2091[1];
UINT8 L_2092[17];
char L_2093[1];
UINT8 L_2094[19];
char L_2095[1];
UINT8 L_2096[20];
char L_2097[1];
UINT8 L_2098[17];
char L_2099[1];
UINT8 L_2100[18];
char L_2101[1];
UINT8 L_2102[17];
char L_2103[1];
UINT8 L_2104[18];
char L_2105[1];
UINT8 L_2106[15];
char L_2107[1];
UINT8 L_2108[16];
char L_2109[1];
UINT8 L_2110[19];
char L_2111[1];
UINT8 L_2112[20];
char L_2113[1];
UINT8 L_2114[17];
char L_2115[1];
UINT8 L_2116[18];
char L_2117[1];
UINT8 L_2118[19];
char L_2119[1];
UINT8 L_2120[20];
char L_2121[1];
UINT8 L_2122[17];
char L_2123[1];
UINT8 L_2124[18];
char L_2125[1];
UINT8 L_2126[17];
char L_2127[1];
UINT8 L_2128[18];
char L_2129[1];
UINT8 L_2130[15];
char L_2131[1];
UINT8 L_2132[16];
char L_2133[1];
UINT8 L_2134[19];
char L_2135[1];
UINT8 L_2136[20];
char L_2137[1];
UINT8 L_2138[17];
char L_2139[1];
UINT8 L_2140[18];
char L_2141[1];
UINT8 L_2142[18];
char L_2143[1];
UINT8 L_2144[19];
char L_2145[1];
UINT8 L_2146[16];
char L_2147[1];
UINT8 L_2148[17];
char L_2149[1];
UINT8 L_2150[19];
char L_2151[1];
UINT8 L_2152[20];
char L_2153[1];
UINT8 L_2154[17];
char L_2155[1];
UINT8 L_2156[18];
char L_2157[1];
UINT8 L_2158[19];
char L_2159[1];
UINT8 L_2160[20];
char L_2161[1];
UINT8 L_2162[17];
char L_2163[1];
UINT8 L_2164[18];
char L_2165[1];
UINT8 L_2166[18];
char L_2167[1];
UINT8 L_2168[19];
char L_2169[1];
UINT8 L_2170[16];
char L_2171[1];
UINT8 L_2172[17];
char L_2173[1];
UINT8 L_2174[16];
char L_2175[1];
UINT8 L_2176[17];
char L_2177[1];
UINT8 L_2178[14];
char L_2179[1];
UINT8 L_2180[15];
char L_2181[1];
UINT8 L_2182[17];
char L_2183[1];
UINT8 L_2184[18];
char L_2185[1];
UINT8 L_2186[15];
char L_2187[1];
UINT8 L_2188[16];
char L_2189[1];
UINT8 L_2190[18];
char L_2191[1];
UINT8 L_2192[19];
char L_2193[1];
UINT8 L_2194[16];
char L_2195[1];
UINT8 L_2196[17];
char L_2197[1];
UINT8 L_2198[16];
char L_2199[1];
UINT8 L_2200[17];
char L_2201[1];
UINT8 L_2202[14];
char L_2203[1];
UINT8 L_2204[15];
char L_2205[1];
UINT8 L_2206[18];
char L_2207[1];
UINT8 L_2208[19];
char L_2209[1];
UINT8 L_2210[16];
char L_2211[1];
UINT8 L_2212[17];
char L_2213[1];
UINT8 L_2214[18];
char L_2215[1];
UINT8 L_2216[19];
char L_2217[1];
UINT8 L_2218[16];
char L_2219[1];
UINT8 L_2220[17];
char L_2221[1];
UINT8 L_2222[16];
char L_2223[1];
UINT8 L_2224[17];
char L_2225[1];
UINT8 L_2226[14];
char L_2227[1];
UINT8 L_2228[15];
char L_2229[1];
UINT8 L_2230[18];
char L_2231[1];
UINT8 L_2232[19];
char L_2233[1];
UINT8 L_2234[16];
char L_2235[1];
UINT8 L_2236[17];
char L_2237[1];
UINT8 L_2238[17];
char L_2239[1];
UINT8 L_2240[18];
char L_2241[1];
UINT8 L_2242[15];
char L_2243[1];
UINT8 L_2244[16];
char L_2245[1];
UINT8 L_2246[18];
char L_2247[1];
UINT8 L_2248[19];
char L_2249[1];
UINT8 L_2250[16];
char L_2251[1];
UINT8 L_2252[17];
char L_2253[1];
UINT8 L_2254[18];
char L_2255[1];
UINT8 L_2256[19];
char L_2257[1];
UINT8 L_2258[16];
char L_2259[1];
UINT8 L_2260[17];
char L_2261[1];
UINT8 L_2262[17];
char L_2263[1];
UINT8 L_2264[18];
char L_2265[1];
UINT8 L_2266[15];
char L_2267[1];
UINT8 L_2268[16];
char L_2269[1];
UINT8 L_2270[17];
char L_2271[1];
UINT8 L_2272[18];
char L_2273[1];
UINT8 L_2274[15];
char L_2275[1];
UINT8 L_2276[16];
char L_2277[1];
UINT8 L_2278[18];
char L_2279[1];
UINT8 L_2280[19];
char L_2281[1];
UINT8 L_2282[16];
char L_2283[1];
UINT8 L_2284[17];
char L_2285[1];
UINT8 L_2286[19];
char L_2287[1];
UINT8 L_2288[20];
char L_2289[1];
UINT8 L_2290[17];
char L_2291[1];
UINT8 L_2292[18];
char L_2293[1];
UINT8 L_2294[17];
char L_2295[1];
UINT8 L_2296[18];
char L_2297[1];
UINT8 L_2298[15];
char L_2299[1];
UINT8 L_2300[16];
char L_2301[1];
UINT8 L_2302[19];
char L_2303[1];
UINT8 L_2304[20];
char L_2305[1];
UINT8 L_2306[17];
char L_2307[1];
UINT8 L_2308[18];
char L_2309[1];
UINT8 L_2310[19];
char L_2311[1];
UINT8 L_2312[20];
char L_2313[1];
UINT8 L_2314[17];
char L_2315[1];
UINT8 L_2316[18];
char L_2317[1];
UINT8 L_2318[17];
char L_2319[1];
UINT8 L_2320[18];
char L_2321[1];
UINT8 L_2322[15];
char L_2323[1];
UINT8 L_2324[16];
char L_2325[1];
UINT8 L_2326[19];
char L_2327[1];
UINT8 L_2328[20];
char L_2329[1];
UINT8 L_2330[17];
char L_2331[1];
UINT8 L_2332[18];
char L_2333[1];
UINT8 L_2334[18];
char L_2335[1];
UINT8 L_2336[19];
char L_2337[1];
UINT8 L_2338[16];
char L_2339[1];
UINT8 L_2340[17];
char L_2341[1];
UINT8 L_2342[19];
char L_2343[1];
UINT8 L_2344[20];
char L_2345[1];
UINT8 L_2346[17];
char L_2347[1];
UINT8 L_2348[18];
char L_2349[1];
UINT8 L_2350[19];
char L_2351[1];
UINT8 L_2352[20];
char L_2353[1];
UINT8 L_2354[17];
char L_2355[1];
UINT8 L_2356[18];
char L_2357[1];
UINT8 L_2358[18];
char L_2359[1];
UINT8 L_2360[19];
char L_2361[1];
UINT8 L_2362[16];
char L_2363[1];
UINT8 L_2364[17];
char L_2365[1];
UINT8 L_2366[19];
char L_2367[1];
UINT8 L_2368[17];
char L_2369[1];
UINT8 L_2370[17];
char L_2371[1];
UINT8 L_2372[18];
char L_2373[1];
UINT8 L_2374[15];
char L_2375[1];
UINT8 L_2376[16];
char L_2377[1];
UINT8 L_2378[18];
char L_2379[1];
UINT8 L_2380[19];
char L_2381[1];
UINT8 L_2382[16];
char L_2383[1];
UINT8 L_2384[17];
char L_2385[1];
UINT8 L_2386[19];
char L_2387[1];
UINT8 L_2388[20];
char L_2389[1];
UINT8 L_2390[17];
char L_2391[1];
UINT8 L_2392[18];
char L_2393[1];
UINT8 L_2394[17];
char L_2395[1];
UINT8 L_2396[18];
char L_2397[1];
UINT8 L_2398[15];
char L_2399[1];
UINT8 L_2400[16];
char L_2401[1];
UINT8 L_2402[19];
char L_2403[1];
UINT8 L_2404[20];
char L_2405[1];
UINT8 L_2406[17];
char L_2407[1];
UINT8 L_2408[18];
char L_2409[1];
UINT8 L_2410[19];
char L_2411[1];
UINT8 L_2412[20];
char L_2413[1];
UINT8 L_2414[17];
char L_2415[1];
UINT8 L_2416[18];
char L_2417[1];
UINT8 L_2418[17];
char L_2419[1];
UINT8 L_2420[18];
char L_2421[1];
UINT8 L_2422[15];
char L_2423[1];
UINT8 L_2424[16];
char L_2425[1];
UINT8 L_2426[19];
char L_2427[1];
UINT8 L_2428[20];
char L_2429[1];
UINT8 L_2430[17];
char L_2431[1];
UINT8 L_2432[18];
char L_2433[1];
UINT8 L_2434[18];
char L_2435[1];
UINT8 L_2436[19];
char L_2437[1];
UINT8 L_2438[16];
char L_2439[1];
UINT8 L_2440[17];
char L_2441[1];
UINT8 L_2442[19];
char L_2443[1];
UINT8 L_2444[20];
char L_2445[1];
UINT8 L_2446[17];
char L_2447[1];
UINT8 L_2448[18];
char L_2449[1];
UINT8 L_2450[19];
char L_2451[1];
UINT8 L_2452[20];
char L_2453[1];
UINT8 L_2454[17];
char L_2455[1];
UINT8 L_2456[18];
char L_2457[1];
UINT8 L_2458[18];
char L_2459[1];
UINT8 L_2460[19];
char L_2461[1];
UINT8 L_2462[16];
char L_2463[1];
UINT8 L_2464[17];
char L_2465[1];
UINT8 L_2466[16];
char L_2467[1];
UINT8 L_2468[17];
char L_2469[1];
UINT8 L_2470[14];
char L_2471[1];
UINT8 L_2472[15];
char L_2473[1];
UINT8 L_2474[17];
char L_2475[1];
UINT8 L_2476[18];
char L_2477[1];
UINT8 L_2478[15];
char L_2479[1];
UINT8 L_2480[16];
char L_2481[1];
UINT8 L_2482[18];
char L_2483[1];
UINT8 L_2484[19];
char L_2485[1];
UINT8 L_2486[16];
char L_2487[1];
UINT8 L_2488[17];
char L_2489[1];
UINT8 L_2490[16];
char L_2491[1];
UINT8 L_2492[17];
char L_2493[1];
UINT8 L_2494[14];
char L_2495[1];
UINT8 L_2496[15];
char L_2497[1];
UINT8 L_2498[18];
char L_2499[1];
UINT8 L_2500[19];
char L_2501[1];
UINT8 L_2502[16];
char L_2503[1];
UINT8 L_2504[17];
char L_2505[1];
UINT8 L_2506[18];
char L_2507[1];
UINT8 L_2508[19];
char L_2509[1];
UINT8 L_2510[16];
char L_2511[1];
UINT8 L_2512[17];
char L_2513[1];
UINT8 L_2514[16];
char L_2515[1];
UINT8 L_2516[17];
char L_2517[1];
UINT8 L_2518[14];
char L_2519[1];
UINT8 L_2520[15];
char L_2521[1];
UINT8 L_2522[18];
char L_2523[1];
UINT8 L_2524[19];
char L_2525[1];
UINT8 L_2526[16];
char L_2527[1];
UINT8 L_2528[17];
char L_2529[1];
UINT8 L_2530[17];
char L_2531[1];
UINT8 L_2532[18];
char L_2533[1];
UINT8 L_2534[15];
char L_2535[1];
UINT8 L_2536[16];
char L_2537[1];
UINT8 L_2538[18];
char L_2539[1];
UINT8 L_2540[19];
char L_2541[1];
UINT8 L_2542[16];
char L_2543[1];
UINT8 L_2544[17];
char L_2545[1];
UINT8 L_2546[18];
char L_2547[1];
UINT8 L_2548[19];
char L_2549[1];
UINT8 L_2550[16];
char L_2551[1];
UINT8 L_2552[17];
char L_2553[1];
UINT8 L_2554[17];
char L_2555[1];
UINT8 L_2556[18];
char L_2557[1];
UINT8 L_2558[15];
char L_2559[1];
UINT8 L_2560[16];
char L_2561[4];
ADDRESS L_2562[1162];
char L_2563[8];
UINT8 L_2564[8];
char L_2565[8];
};
static  const Minus_m_11_L_12_t Minus_m_11_L_12={{'M','i','n','u','s','_','M','3'},{0 /* 1 */ ,},{'M','i','n','u','s','_','p','a','r','a','m','_','L','_','L'},{0 /* 1 */ ,},{'u','M','i','n','u','s','_','p','a','r','a','m','_','L','_','L'},{0 /* 1 */ ,},{'M','i','n','u','s','_','v','a','r','_','L','_','L'},{0 /* 1 */ ,},{'u','M','i','n','u','s','_','v','a','r','_','L','_','L'},{0 /* 1 */ ,},{'M','i','n','u','s','_','p','a','r','a','m','_','L','_','u','8'},{0 /* 1 */ ,},{'u','M','i','n','u','s','_','p','a','r','a','m','_','L','_','u','8'},{0 /* 1 */ ,},{'M','i','n','u','s','_','v','a','r','_','L','_','u','8'},{0 /* 1 */ ,},{'u','M','i','n','u','s','_','v','a','r','_','L','_','u','8'},{0 /* 1 */ ,},{'M','i','n','u','s','_','p','a','r','a','m','_','L','_','u','3','2'},{0 /* 1 */ ,},{'u','M','i','n','u','s','_','p','a','r','a','m','_','L','_','u','3','2'},{0 /* 1 */ ,},{'M','i','n','u','s','_','v','a','r','_','L','_','u','3','2'},{0 /* 1 */ ,},{'u','M','i','n','u','s','_','v','a','r','_','L'
,'_','u','3','2'},{0 /* 1 */ ,},{'M','i','n','u','s','_','p','a','r','a','m','_','L','_','C'},{0 /* 1 */ ,},{'u','M','i','n','u','s','_','p','a','r','a','m','_','L','_','C'},{0 /* 1 */ ,},{'M','i','n','u','s','_','v','a','r','_','L','_','C'},{0 /* 1 */ ,},{'u','M','i','n','u','s','_','v','a','r','_','L','_','C'},{0 /* 1 */ ,},{'M','i','n','u','s','_','p','a','r','a','m','_','L','_','i','1','6'},{0 /* 1 */ ,},{'u','M','i','n','u','s','_','p','a','r','a','m','_','L','_','i','1','6'},{0 /* 1 */ ,},{'M','i','n','u','s','_','v','a','r','_','L','_','i','1','6'},{0 /* 1 */ ,},{'u','M','i','n','u','s','_','v','a','r','_','L','_','i','1','6'},{0 /* 1 */ ,},{'M','i','n','u','s','_','p','a','r','a','m','_','L','_','i','6','4'},{0 /* 1 */ ,},{'u','M','i','n','u','s','_','p','a','r','a','m','_','L','_','i','6','4'},{0 /* 1 */ ,},{'M','i','n','u','s','_','v','a','r','_','L','_','i','6','4'},{0 /* 1 */ ,},{'u','M','i','n','u','s','_','v','a','r','_','L','_','i','6','4'},{0 /* 1 */ ,},{'M','i','n','u'
,'s','_','p','a','r','a','m','_','L','_','I'},{0 /* 1 */ ,},{'u','M','i','n','u','s','_','p','a','r','a','m','_','L','_','I'},{0 /* 1 */ ,},{'M','i','n','u','s','_','v','a','r','_','L','_','I'},{0 /* 1 */ ,},{'u','M','i','n','u','s','_','v','a','r','_','L','_','I'},{0 /* 1 */ ,},{'M','i','n','u','s','_','p','a','r','a','m','_','L','_','u','1','6'},{0 /* 1 */ ,},{'u','M','i','n','u','s','_','p','a','r','a','m','_','L','_','u','1','6'},{0 /* 1 */ ,},{'M','i','n','u','s','_','v','a','r','_','L','_','u','1','6'},{0 /* 1 */ ,},{'u','M','i','n','u','s','_','v','a','r','_','L','_','u','1','6'},{0 /* 1 */ ,},{'M','i','n','u','s','_','p','a','r','a','m','_','L','_','L','C'},{0 /* 1 */ ,},{'u','M','i','n','u','s','_','p','a','r','a','m','_','L','_','L','C'},{0 /* 1 */ ,},{'M','i','n','u','s','_','v','a','r','_','L','_','L','C'},{0 /* 1 */ ,},{'u','M','i','n','u','s','_','v','a','r','_','L','_','L','C'},{0 /* 1 */ ,},{'M','i','n','u','s','_','p','a','r','a','m','_','L','_','i','3','2'},{0 /* 1 */ 
,},{'u','M','i','n','u','s','_','p','a','r','a','m','_','L','_','i','3','2'},{0 /* 1 */ ,},{'M','i','n','u','s','_','v','a','r','_','L','_','i','3','2'},{0 /* 1 */ ,},{'u','M','i','n','u','s','_','v','a','r','_','L','_','i','3','2'},{0 /* 1 */ ,},{'M','i','n','u','s','_','p','a','r','a','m','_','L','_','u','6','4'},{0 /* 1 */ ,},{'u','M','i','n','u','s','_','p','a','r','a','m','_','L','_','u','6','4'},{0 /* 1 */ ,},{'M','i','n','u','s','_','v','a','r','_','L','_','u','6','4'},{0 /* 1 */ ,},{'u','M','i','n','u','s','_','v','a','r','_','L','_','u','6','4'},{0 /* 1 */ ,},{'M','i','n','u','s','_','p','a','r','a','m','_','L','_','i','8'},{0 /* 1 */ ,},{'u','M','i','n','u','s','_','p','a','r','a','m','_','L','_','i','8'},{0 /* 1 */ ,},{'M','i','n','u','s','_','v','a','r','_','L','_','i','8'},{0 /* 1 */ ,},{'u','M','i','n','u','s','_','v','a','r','_','L','_','i','8'},{0 /* 1 */ ,},{'M','i','n','u','s','_','p','a','r','a','m','_','u','8','_','L'},{0 /* 1 */ ,},{'u','M','i','n','u','s','_','p',
'a','r','a','m','_','u','8','_','L'},{0 /* 1 */ ,},{'M','i','n','u','s','_','v','a','r','_','u','8','_','L'},{0 /* 1 */ ,},{'u','M','i','n','u','s','_','v','a','r','_','u','8','_','L'},{0 /* 1 */ ,},{'M','i','n','u','s','_','p','a','r','a','m','_','u','8','_','u','8'},{0 /* 1 */ ,},{'u','M','i','n','u','s','_','p','a','r','a','m','_','u','8','_','u','8'},{0 /* 1 */ ,},{'M','i','n','u','s','_','v','a','r','_','u','8','_','u','8'},{0 /* 1 */ ,},{'u','M','i','n','u','s','_','v','a','r','_','u','8','_','u','8'},{0 /* 1 */ ,},{'M','i','n','u','s','_','p','a','r','a','m','_','u','8','_','u','3','2'},{0 /* 1 */ ,},{'u','M','i','n','u','s','_','p','a','r','a','m','_','u','8','_','u','3','2'},{0 /* 1 */ ,},{'M','i','n','u','s','_','v','a','r','_','u','8','_','u','3','2'},{0 /* 1 */ ,},{'u','M','i','n','u','s','_','v','a','r','_','u','8','_','u','3','2'},{0 /* 1 */ ,},{'M','i','n','u','s','_','p','a','r','a','m','_','u','8','_','C'},{0 /* 1 */ ,},{'u','M','i','n','u','s','_','p','a','r','a','m',
'_','u','8','_','C'},{0 /* 1 */ ,},{'M','i','n','u','s','_','v','a','r','_','u','8','_','C'},{0 /* 1 */ ,},{'u','M','i','n','u','s','_','v','a','r','_','u','8','_','C'},{0 /* 1 */ ,},{'M','i','n','u','s','_','p','a','r','a','m','_','u','8','_','i','1','6'},{0 /* 1 */ ,},{'u','M','i','n','u','s','_','p','a','r','a','m','_','u','8','_','i','1','6'},{0 /* 1 */ ,},{'M','i','n','u','s','_','v','a','r','_','u','8','_','i','1','6'},{0 /* 1 */ ,},{'u','M','i','n','u','s','_','v','a','r','_','u','8','_','i','1','6'},{0 /* 1 */ ,},{'M','i','n','u','s','_','p','a','r','a','m','_','u','8','_','i','6','4'},{0 /* 1 */ ,},{'u','M','i','n','u','s','_','p','a','r','a','m','_','u','8','_','i','6','4'},{0 /* 1 */ ,},{'M','i','n','u','s','_','v','a','r','_','u','8','_','i','6','4'},{0 /* 1 */ ,},{'u','M','i','n','u','s','_','v','a','r','_','u','8','_','i','6','4'},{0 /* 1 */ ,},{'M','i','n','u','s','_','p','a','r','a','m','_','u','8','_','I'},{0 /* 1 */ ,},{'u','M','i','n','u','s','_','p','a','r','a','m',
'_','u','8','_','I'},{0 /* 1 */ ,},{'M','i','n','u','s','_','v','a','r','_','u','8','_','I'},{0 /* 1 */ ,},{'u','M','i','n','u','s','_','v','a','r','_','u','8','_','I'},{0 /* 1 */ ,},{'M','i','n','u','s','_','p','a','r','a','m','_','u','8','_','u','1','6'},{0 /* 1 */ ,},{'u','M','i','n','u','s','_','p','a','r','a','m','_','u','8','_','u','1','6'},{0 /* 1 */ ,},{'M','i','n','u','s','_','v','a','r','_','u','8','_','u','1','6'},{0 /* 1 */ ,},{'u','M','i','n','u','s','_','v','a','r','_','u','8','_','u','1','6'},{0 /* 1 */ ,},{'M','i','n','u','s','_','p','a','r','a','m','_','u','8','_','L','C'},{0 /* 1 */ ,},{'u','M','i','n','u','s','_','p','a','r','a','m','_','u','8','_','L','C'},{0 /* 1 */ ,},{'M','i','n','u','s','_','v','a','r','_','u','8','_','L','C'},{0 /* 1 */ ,},{'u','M','i','n','u','s','_','v','a','r','_','u','8','_','L','C'},{0 /* 1 */ ,},{'M','i','n','u','s','_','p','a','r','a','m','_','u','8','_','i','3','2'},{0 /* 1 */ ,},{'u','M','i','n','u','s','_','p','a','r','a','m','_','u',
'8','_','i','3','2'},{0 /* 1 */ ,},{'M','i','n','u','s','_','v','a','r','_','u','8','_','i','3','2'},{0 /* 1 */ ,},{'u','M','i','n','u','s','_','v','a','r','_','u','8','_','i','3','2'},{0 /* 1 */ ,},{'M','i','n','u','s','_','p','a','r','a','m','_','u','8','_','u','6','4'},{0 /* 1 */ ,},{'u','M','i','n','u','s','_','p','a','r','a','m','_','u','8','_','u','6','4'},{0 /* 1 */ ,},{'M','i','n','u','s','_','v','a','r','_','u','8','_','u','6','4'},{0 /* 1 */ ,},{'u','M','i','n','u','s','_','v','a','r','_','u','8','_','u','6','4'},{0 /* 1 */ ,},{'M','i','n','u','s','_','p','a','r','a','m','_','u','8','_','i','8'},{0 /* 1 */ ,},{'u','M','i','n','u','s','_','p','a','r','a','m','_','u','8','_','i','8'},{0 /* 1 */ ,},{'M','i','n','u','s','_','v','a','r','_','u','8','_','i','8'},{0 /* 1 */ ,},{'u','M','i','n','u','s','_','v','a','r','_','u','8','_','i','8'},{0 /* 1 */ ,},{'M','i','n','u','s','_','p','a','r','a','m','_','u','3','2','_','L'},{0 /* 1 */ ,},{'u','M','i','n','u','s','_','p','a','r','a',
'm','_','u','3','2','_','L'},{0 /* 1 */ ,},{'M','i','n','u','s','_','v','a','r','_','u','3','2','_','L'},{0 /* 1 */ ,},{'u','M','i','n','u','s','_','v','a','r','_','u','3','2','_','L'},{0 /* 1 */ ,},{'M','i','n','u','s','_','p','a','r','a','m','_','u','3','2','_','u','8'},{0 /* 1 */ ,},{'u','M','i','n','u','s','_','p','a','r','a','m','_','u','3','2','_','u','8'},{0 /* 1 */ ,},{'M','i','n','u','s','_','v','a','r','_','u','3','2','_','u','8'},{0 /* 1 */ ,},{'u','M','i','n','u','s','_','v','a','r','_','u','3','2','_','u','8'},{0 /* 1 */ ,},{'M','i','n','u','s','_','p','a','r','a','m','_','u','3','2','_','u','3','2'},{0 /* 1 */ ,},{'u','M','i','n','u','s','_','p','a','r','a','m','_','u','3','2','_','u','3','2'},{0 /* 1 */ ,},{'M','i','n','u','s','_','v','a','r','_','u','3','2','_','u','3','2'},{0 /* 1 */ ,},{'u','M','i','n','u','s','_','v','a','r','_','u','3','2','_','u','3','2'},{0 /* 1 */ ,},{'M','i','n','u','s','_','p','a','r','a','m','_','u','3','2','_','C'},{0 /* 1 */ ,},{'u','M','i',
'n','u','s','_','p','a','r','a','m','_','u','3','2','_','C'},{0 /* 1 */ ,},{'M','i','n','u','s','_','v','a','r','_','u','3','2','_','C'},{0 /* 1 */ ,},{'u','M','i','n','u','s','_','v','a','r','_','u','3','2','_','C'},{0 /* 1 */ ,},{'M','i','n','u','s','_','p','a','r','a','m','_','u','3','2','_','i','1','6'},{0 /* 1 */ ,},{'u','M','i','n','u','s','_','p','a','r','a','m','_','u','3','2','_','i','1','6'},{0 /* 1 */ ,},{'M','i','n','u','s','_','v','a','r','_','u','3','2','_','i','1','6'},{0 /* 1 */ ,},{'u','M','i','n','u','s','_','v','a','r','_','u','3','2','_','i','1','6'},{0 /* 1 */ ,},{'M','i','n','u','s','_','p','a','r','a','m','_','u','3','2','_','i','6','4'},{0 /* 1 */ ,},{'u','M','i','n','u','s','_','p','a','r','a','m','_','u','3','2','_','i','6','4'},{0 /* 1 */ ,},{'M','i','n','u','s','_','v','a','r','_','u','3','2','_','i','6','4'},{0 /* 1 */ ,},{'u','M','i','n','u','s','_','v','a','r','_','u','3','2','_','i','6','4'},{0 /* 1 */ ,},{'M','i','n','u','s','_','p','a','r','a','m','_',
'u','3','2','_','I'},{0 /* 1 */ ,},{'u','M','i','n','u','s','_','p','a','r','a','m','_','u','3','2','_','I'},{0 /* 1 */ ,},{'M','i','n','u','s','_','v','a','r','_','u','3','2','_','I'},{0 /* 1 */ ,},{'u','M','i','n','u','s','_','v','a','r','_','u','3','2','_','I'},{0 /* 1 */ ,},{'M','i','n','u','s','_','p','a','r','a','m','_','u','3','2','_','u','1','6'},{0 /* 1 */ ,},{'u','M','i','n','u','s','_','p','a','r','a','m','_','u','3','2','_','u','1','6'},{0 /* 1 */ ,},{'M','i','n','u','s','_','v','a','r','_','u','3','2','_','u','1','6'},{0 /* 1 */ ,},{'u','M','i','n','u','s','_','v','a','r','_','u','3','2','_','u','1','6'},{0 /* 1 */ ,},{'M','i','n','u','s','_','p','a','r','a','m','_','u','3','2','_','L','C'},{0 /* 1 */ ,},{'u','M','i','n','u','s','_','p','a','r','a','m','_','u','3','2','_','L','C'},{0 /* 1 */ ,},{'M','i','n','u','s','_','v','a','r','_','u','3','2','_','L','C'},{0 /* 1 */ ,},{'u','M','i','n','u','s','_','v','a','r','_','u','3','2','_','L','C'},{0 /* 1 */ ,},{'M','i','n','u',
's','_','p','a','r','a','m','_','u','3','2','_','i','3','2'},{0 /* 1 */ ,},{'u','M','i','n','u','s','_','p','a','r','a','m','_','u','3','2','_','i','3','2'},{0 /* 1 */ ,},{'M','i','n','u','s','_','v','a','r','_','u','3','2','_','i','3','2'},{0 /* 1 */ ,},{'u','M','i','n','u','s','_','v','a','r','_','u','3','2','_','i','3','2'},{0 /* 1 */ ,},{'M','i','n','u','s','_','p','a','r','a','m','_','u','3','2','_','u','6','4'},{0 /* 1 */ ,},{'u','M','i','n','u','s','_','p','a','r','a','m','_','u','3','2','_','u','6','4'},{0 /* 1 */ ,},{'M','i','n','u','s','_','v','a','r','_','u','3','2','_','u','6','4'},{0 /* 1 */ ,},{'u','M','i','n','u','s','_','v','a','r','_','u','3','2','_','u','6','4'},{0 /* 1 */ ,},{'M','i','n','u','s','_','p','a','r','a','m','_','u','3','2','_','i','8'},{0 /* 1 */ ,},{'u','M','i','n','u','s','_','p','a','r','a','m','_','u','3','2','_','i','8'},{0 /* 1 */ ,},{'M','i','n','u','s','_','v','a','r','_','u','3','2','_','i','8'},{0 /* 1 */ ,},{'u','M','i','n','u','s','_','v','a',
'r','_','u','3','2','_','i','8'},{0 /* 1 */ ,},{'M','i','n','u','s','_','p','a','r','a','m','_','C','_','L'},{0 /* 1 */ ,},{'u','M','i','n','u','s','_','p','a','r','a','m','_','C','_','L'},{0 /* 1 */ ,},{'M','i','n','u','s','_','v','a','r','_','C','_','L'},{0 /* 1 */ ,},{'u','M','i','n','u','s','_','v','a','r','_','C','_','L'},{0 /* 1 */ ,},{'M','i','n','u','s','_','p','a','r','a','m','_','C','_','u','8'},{0 /* 1 */ ,},{'u','M','i','n','u','s','_','p','a','r','a','m','_','C','_','u','8'},{0 /* 1 */ ,},{'M','i','n','u','s','_','v','a','r','_','C','_','u','8'},{0 /* 1 */ ,},{'u','M','i','n','u','s','_','v','a','r','_','C','_','u','8'},{0 /* 1 */ ,},{'M','i','n','u','s','_','p','a','r','a','m','_','C','_','u','3','2'},{0 /* 1 */ ,},{'u','M','i','n','u','s','_','p','a','r','a','m','_','C','_','u','3','2'},{0 /* 1 */ ,},{'M','i','n','u','s','_','v','a','r','_','C','_','u','3','2'},{0 /* 1 */ ,},{'u','M','i','n','u','s','_','v','a','r','_','C','_','u','3','2'},{0 /* 1 */ ,},{'M','i','n','u',
's','_','p','a','r','a','m','_','C','_','C'},{0 /* 1 */ ,},{'u','M','i','n','u','s','_','p','a','r','a','m','_','C','_','C'},{0 /* 1 */ ,},{'M','i','n','u','s','_','v','a','r','_','C','_','C'},{0 /* 1 */ ,},{'u','M','i','n','u','s','_','v','a','r','_','C','_','C'},{0 /* 1 */ ,},{'M','i','n','u','s','_','p','a','r','a','m','_','C','_','i','1','6'},{0 /* 1 */ ,},{'u','M','i','n','u','s','_','p','a','r','a','m','_','C','_','i','1','6'},{0 /* 1 */ ,},{'M','i','n','u','s','_','v','a','r','_','C','_','i','1','6'},{0 /* 1 */ ,},{'u','M','i','n','u','s','_','v','a','r','_','C','_','i','1','6'},{0 /* 1 */ ,},{'M','i','n','u','s','_','p','a','r','a','m','_','C','_','i','6','4'},{0 /* 1 */ ,},{'u','M','i','n','u','s','_','p','a','r','a','m','_','C','_','i','6','4'},{0 /* 1 */ ,},{'M','i','n','u','s','_','v','a','r','_','C','_','i','6','4'},{0 /* 1 */ ,},{'u','M','i','n','u','s','_','v','a','r','_','C','_','i','6','4'},{0 /* 1 */ ,},{'M','i','n','u','s','_','p','a','r','a','m','_','C','_','I'},{0 /* 1 */ 
,},{'u','M','i','n','u','s','_','p','a','r','a','m','_','C','_','I'},{0 /* 1 */ ,},{'M','i','n','u','s','_','v','a','r','_','C','_','I'},{0 /* 1 */ ,},{'u','M','i','n','u','s','_','v','a','r','_','C','_','I'},{0 /* 1 */ ,},{'M','i','n','u','s','_','p','a','r','a','m','_','C','_','u','1','6'},{0 /* 1 */ ,},{'u','M','i','n','u','s','_','p','a','r','a','m','_','C','_','u','1','6'},{0 /* 1 */ ,},{'M','i','n','u','s','_','v','a','r','_','C','_','u','1','6'},{0 /* 1 */ ,},{'u','M','i','n','u','s','_','v','a','r','_','C','_','u','1','6'},{0 /* 1 */ ,},{'M','i','n','u','s','_','p','a','r','a','m','_','C','_','L','C'},{0 /* 1 */ ,},{'u','M','i','n','u','s','_','p','a','r','a','m','_','C','_','L','C'},{0 /* 1 */ ,},{'M','i','n','u','s','_','v','a','r','_','C','_','L','C'},{0 /* 1 */ ,},{'u','M','i','n','u','s','_','v','a','r','_','C','_','L','C'},{0 /* 1 */ ,},{'M','i','n','u','s','_','p','a','r','a','m','_','C','_','i','3','2'},{0 /* 1 */ ,},{'u','M','i','n','u','s','_','p','a','r','a','m','_',
'C','_','i','3','2'},{0 /* 1 */ ,},{'M','i','n','u','s','_','v','a','r','_','C','_','i','3','2'},{0 /* 1 */ ,},{'u','M','i','n','u','s','_','v','a','r','_','C','_','i','3','2'},{0 /* 1 */ ,},{'M','i','n','u','s','_','p','a','r','a','m','_','C','_','u','6','4'},{0 /* 1 */ ,},{'u','M','i','n','u','s','_','p','a','r','a','m','_','C','_','u','6','4'},{0 /* 1 */ ,},{'M','i','n','u','s','_','v','a','r','_','C','_','u','6','4'},{0 /* 1 */ ,},{'u','M','i','n','u','s','_','v','a','r','_','C','_','u','6','4'},{0 /* 1 */ ,},{'M','i','n','u','s','_','p','a','r','a','m','_','C','_','i','8'},{0 /* 1 */ ,},{'u','M','i','n','u','s','_','p','a','r','a','m','_','C','_','i','8'},{0 /* 1 */ ,},{'M','i','n','u','s','_','v','a','r','_','C','_','i','8'},{0 /* 1 */ ,},{'u','M','i','n','u','s','_','v','a','r','_','C','_','i','8'},{0 /* 1 */ ,},{'M','i','n','u','s','_','p','a','r','a','m','_','i','1','6','_','L'},{0 /* 1 */ ,},{'u','M','i','n','u','s','_','p','a','r','a','m','_','i','1','6','_','L'},{0 /* 1 */ 
,},{'M','i','n','u','s','_','v','a','r','_','i','1','6','_','L'},{0 /* 1 */ ,},{'u','M','i','n','u','s','_','v','a','r','_','i','1','6','_','L'},{0 /* 1 */ ,},{'M','i','n','u','s','_','p','a','r','a','m','_','i','1','6','_','u','8'},{0 /* 1 */ ,},{'u','M','i','n','u','s','_','p','a','r','a','m','_','i','1','6','_','u','8'},{0 /* 1 */ ,},{'M','i','n','u','s','_','v','a','r','_','i','1','6','_','u','8'},{0 /* 1 */ ,},{'u','M','i','n','u','s','_','v','a','r','_','i','1','6','_','u','8'},{0 /* 1 */ ,},{'M','i','n','u','s','_','p','a','r','a','m','_','i','1','6','_','u','3','2'},{0 /* 1 */ ,},{'u','M','i','n','u','s','_','p','a','r','a','m','_','i','1','6','_','u','3','2'},{0 /* 1 */ ,},{'M','i','n','u','s','_','v','a','r','_','i','1','6','_','u','3','2'},{0 /* 1 */ ,},{'u','M','i','n','u','s','_','v','a','r','_','i','1','6','_','u','3','2'},{0 /* 1 */ ,},{'M','i','n','u','s','_','p','a','r','a','m','_','i','1','6','_','C'},{0 /* 1 */ ,},{'u','M','i','n','u','s','_','p','a','r','a','m','_',
'i','1','6','_','C'},{0 /* 1 */ ,},{'M','i','n','u','s','_','v','a','r','_','i','1','6','_','C'},{0 /* 1 */ ,},{'u','M','i','n','u','s','_','v','a','r','_','i','1','6','_','C'},{0 /* 1 */ ,},{'M','i','n','u','s','_','p','a','r','a','m','_','i','1','6','_','i','1','6'},{0 /* 1 */ ,},{'u','M','i','n','u','s','_','p','a','r','a','m','_','i','1','6','_','i','1','6'},{0 /* 1 */ ,},{'M','i','n','u','s','_','v','a','r','_','i','1','6','_','i','1','6'},{0 /* 1 */ ,},{'u','M','i','n','u','s','_','v','a','r','_','i','1','6','_','i','1','6'},{0 /* 1 */ ,},{'M','i','n','u','s','_','p','a','r','a','m','_','i','1','6','_','i','6','4'},{0 /* 1 */ ,},{'u','M','i','n','u','s','_','p','a','r','a','m','_','i','1','6','_','i','6','4'},{0 /* 1 */ ,},{'M','i','n','u','s','_','v','a','r','_','i','1','6','_','i','6','4'},{0 /* 1 */ ,},{'u','M','i','n','u','s','_','v','a','r','_','i','1','6','_','i','6','4'},{0 /* 1 */ ,},{'M','i','n','u','s','_','p','a','r','a','m','_','i','1','6','_','I'},{0 /* 1 */ ,},{'u',
'M','i','n','u','s','_','p','a','r','a','m','_','i','1','6','_','I'},{0 /* 1 */ ,},{'M','i','n','u','s','_','v','a','r','_','i','1','6','_','I'},{0 /* 1 */ ,},{'u','M','i','n','u','s','_','v','a','r','_','i','1','6','_','I'},{0 /* 1 */ ,},{'M','i','n','u','s','_','p','a','r','a','m','_','i','1','6','_','u','1','6'},{0 /* 1 */ ,},{'u','M','i','n','u','s','_','p','a','r','a','m','_','i','1','6','_','u','1','6'},{0 /* 1 */ ,},{'M','i','n','u','s','_','v','a','r','_','i','1','6','_','u','1','6'},{0 /* 1 */ ,},{'u','M','i','n','u','s','_','v','a','r','_','i','1','6','_','u','1','6'},{0 /* 1 */ ,},{'M','i','n','u','s','_','p','a','r','a','m','_','i','1','6','_','L','C'},{0 /* 1 */ ,},{'u','M','i','n','u','s','_','p','a','r','a','m','_','i','1','6','_','L','C'},{0 /* 1 */ ,},{'M','i','n','u','s','_','v','a','r','_','i','1','6','_','L','C'},{0 /* 1 */ ,},{'u','M','i','n','u','s','_','v','a','r','_','i','1','6','_','L','C'},{0 /* 1 */ ,},{'M','i','n','u','s','_','p','a','r','a','m','_','i','1',
'6','_','i','3','2'},{0 /* 1 */ ,},{'u','M','i','n','u','s','_','p','a','r','a','m','_','i','1','6','_','i','3','2'},{0 /* 1 */ ,},{'M','i','n','u','s','_','v','a','r','_','i','1','6','_','i','3','2'},{0 /* 1 */ ,},{'u','M','i','n','u','s','_','v','a','r','_','i','1','6','_','i','3','2'},{0 /* 1 */ ,},{'M','i','n','u','s','_','p','a','r','a','m','_','i','1','6','_','u','6','4'},{0 /* 1 */ ,},{'u','M','i','n','u','s','_','p','a','r','a','m','_','i','1','6','_','u','6','4'},{0 /* 1 */ ,},{'M','i','n','u','s','_','v','a','r','_','i','1','6','_','u','6','4'},{0 /* 1 */ ,},{'u','M','i','n','u','s','_','v','a','r','_','i','1','6','_','u','6','4'},{0 /* 1 */ ,},{'M','i','n','u','s','_','p','a','r','a','m','_','i','1','6','_','i','8'},{0 /* 1 */ ,},{'u','M','i','n','u','s','_','p','a','r','a','m','_','i','1','6','_','i','8'},{0 /* 1 */ ,},{'M','i','n','u','s','_','v','a','r','_','i','1','6','_','i','8'},{0 /* 1 */ ,},{'u','M','i','n','u','s','_','v','a','r','_','i','1','6','_','i','8'},{0 /* 1 */ 
,},{'M','i','n','u','s','_','p','a','r','a','m','_','f','3','2','_','f','3','2'},{0 /* 1 */ ,},{'M','i','n','u','s','_','v','a','r','_','f','3','2','_','f','3','2'},{0 /* 1 */ ,},{'M','i','n','u','s','_','p','a','r','a','m','_','i','6','4','_','L'},{0 /* 1 */ ,},{'u','M','i','n','u','s','_','p','a','r','a','m','_','i','6','4','_','L'},{0 /* 1 */ ,},{'M','i','n','u','s','_','v','a','r','_','i','6','4','_','L'},{0 /* 1 */ ,},{'u','M','i','n','u','s','_','v','a','r','_','i','6','4','_','L'},{0 /* 1 */ ,},{'M','i','n','u','s','_','p','a','r','a','m','_','i','6','4','_','u','8'},{0 /* 1 */ ,},{'u','M','i','n','u','s','_','p','a','r','a','m','_','i','6','4','_','u','8'},{0 /* 1 */ ,},{'M','i','n','u','s','_','v','a','r','_','i','6','4','_','u','8'},{0 /* 1 */ ,},{'u','M','i','n','u','s','_','v','a','r','_','i','6','4','_','u','8'},{0 /* 1 */ ,},{'M','i','n','u','s','_','p','a','r','a','m','_','i','6','4','_','u','3','2'},{0 /* 1 */ ,},{'u','M','i','n','u','s','_','p','a','r','a','m','_','i',
'6','4','_','u','3','2'},{0 /* 1 */ ,},{'M','i','n','u','s','_','v','a','r','_','i','6','4','_','u','3','2'},{0 /* 1 */ ,},{'u','M','i','n','u','s','_','v','a','r','_','i','6','4','_','u','3','2'},{0 /* 1 */ ,},{'M','i','n','u','s','_','p','a','r','a','m','_','i','6','4','_','C'},{0 /* 1 */ ,},{'u','M','i','n','u','s','_','p','a','r','a','m','_','i','6','4','_','C'},{0 /* 1 */ ,},{'M','i','n','u','s','_','v','a','r','_','i','6','4','_','C'},{0 /* 1 */ ,},{'u','M','i','n','u','s','_','v','a','r','_','i','6','4','_','C'},{0 /* 1 */ ,},{'M','i','n','u','s','_','p','a','r','a','m','_','i','6','4','_','i','1','6'},{0 /* 1 */ ,},{'u','M','i','n','u','s','_','p','a','r','a','m','_','i','6','4','_','i','1','6'},{0 /* 1 */ ,},{'M','i','n','u','s','_','v','a','r','_','i','6','4','_','i','1','6'},{0 /* 1 */ ,},{'u','M','i','n','u','s','_','v','a','r','_','i','6','4','_','i','1','6'},{0 /* 1 */ ,},{'M','i','n','u','s','_','p','a','r','a','m','_','i','6','4','_','i','6','4'},{0 /* 1 */ ,},{'u','M',
'i','n','u','s','_','p','a','r','a','m','_','i','6','4','_','i','6','4'},{0 /* 1 */ ,},{'M','i','n','u','s','_','v','a','r','_','i','6','4','_','i','6','4'},{0 /* 1 */ ,},{'u','M','i','n','u','s','_','v','a','r','_','i','6','4','_','i','6','4'},{0 /* 1 */ ,},{'M','i','n','u','s','_','p','a','r','a','m','_','i','6','4','_','I'},{0 /* 1 */ ,},{'u','M','i','n','u','s','_','p','a','r','a','m','_','i','6','4','_','I'},{0 /* 1 */ ,},{'M','i','n','u','s','_','v','a','r','_','i','6','4','_','I'},{0 /* 1 */ ,},{'u','M','i','n','u','s','_','v','a','r','_','i','6','4','_','I'},{0 /* 1 */ ,},{'M','i','n','u','s','_','p','a','r','a','m','_','i','6','4','_','u','1','6'},{0 /* 1 */ ,},{'u','M','i','n','u','s','_','p','a','r','a','m','_','i','6','4','_','u','1','6'},{0 /* 1 */ ,},{'M','i','n','u','s','_','v','a','r','_','i','6','4','_','u','1','6'},{0 /* 1 */ ,},{'u','M','i','n','u','s','_','v','a','r','_','i','6','4','_','u','1','6'},{0 /* 1 */ ,},{'M','i','n','u','s','_','p','a','r','a','m','_','i',
'6','4','_','L','C'},{0 /* 1 */ ,},{'u','M','i','n','u','s','_','p','a','r','a','m','_','i','6','4','_','L','C'},{0 /* 1 */ ,},{'M','i','n','u','s','_','v','a','r','_','i','6','4','_','L','C'},{0 /* 1 */ ,},{'u','M','i','n','u','s','_','v','a','r','_','i','6','4','_','L','C'},{0 /* 1 */ ,},{'M','i','n','u','s','_','p','a','r','a','m','_','i','6','4','_','i','3','2'},{0 /* 1 */ ,},{'u','M','i','n','u','s','_','p','a','r','a','m','_','i','6','4','_','i','3','2'},{0 /* 1 */ ,},{'M','i','n','u','s','_','v','a','r','_','i','6','4','_','i','3','2'},{0 /* 1 */ ,},{'u','M','i','n','u','s','_','v','a','r','_','i','6','4','_','i','3','2'},{0 /* 1 */ ,},{'M','i','n','u','s','_','p','a','r','a','m','_','i','6','4','_','u','6','4'},{0 /* 1 */ ,},{'u','M','i','n','u','s','_','p','a','r','a','m','_','i','6','4','_','u','6','4'},{0 /* 1 */ ,},{'M','i','n','u','s','_','v','a','r','_','i','6','4','_','u','6','4'},{0 /* 1 */ ,},{'u','M','i','n','u','s','_','v','a','r','_','i','6','4','_','u','6','4'},{0 /* 1 */ 
,},{'M','i','n','u','s','_','p','a','r','a','m','_','i','6','4','_','i','8'},{0 /* 1 */ ,},{'u','M','i','n','u','s','_','p','a','r','a','m','_','i','6','4','_','i','8'},{0 /* 1 */ ,},{'M','i','n','u','s','_','v','a','r','_','i','6','4','_','i','8'},{0 /* 1 */ ,},{'u','M','i','n','u','s','_','v','a','r','_','i','6','4','_','i','8'},{0 /* 1 */ ,},{'M','i','n','u','s','_','p','a','r','a','m','_','I','_','L'},{0 /* 1 */ ,},{'u','M','i','n','u','s','_','p','a','r','a','m','_','I','_','L'},{0 /* 1 */ ,},{'M','i','n','u','s','_','v','a','r','_','I','_','L'},{0 /* 1 */ ,},{'u','M','i','n','u','s','_','v','a','r','_','I','_','L'},{0 /* 1 */ ,},{'M','i','n','u','s','_','p','a','r','a','m','_','I','_','u','8'},{0 /* 1 */ ,},{'u','M','i','n','u','s','_','p','a','r','a','m','_','I','_','u','8'},{0 /* 1 */ ,},{'M','i','n','u','s','_','v','a','r','_','I','_','u','8'},{0 /* 1 */ ,},{'u','M','i','n','u','s','_','v','a','r','_','I','_','u','8'},{0 /* 1 */ ,},{'M','i','n','u','s','_','p','a','r','a','m',
'_','I','_','u','3','2'},{0 /* 1 */ ,},{'u','M','i','n','u','s','_','p','a','r','a','m','_','I','_','u','3','2'},{0 /* 1 */ ,},{'M','i','n','u','s','_','v','a','r','_','I','_','u','3','2'},{0 /* 1 */ ,},{'u','M','i','n','u','s','_','v','a','r','_','I','_','u','3','2'},{0 /* 1 */ ,},{'M','i','n','u','s','_','p','a','r','a','m','_','I','_','C'},{0 /* 1 */ ,},{'u','M','i','n','u','s','_','p','a','r','a','m','_','I','_','C'},{0 /* 1 */ ,},{'M','i','n','u','s','_','v','a','r','_','I','_','C'},{0 /* 1 */ ,},{'u','M','i','n','u','s','_','v','a','r','_','I','_','C'},{0 /* 1 */ ,},{'M','i','n','u','s','_','p','a','r','a','m','_','I','_','i','1','6'},{0 /* 1 */ ,},{'u','M','i','n','u','s','_','p','a','r','a','m','_','I','_','i','1','6'},{0 /* 1 */ ,},{'M','i','n','u','s','_','v','a','r','_','I','_','i','1','6'},{0 /* 1 */ ,},{'u','M','i','n','u','s','_','v','a','r','_','I','_','i','1','6'},{0 /* 1 */ ,},{'M','i','n','u','s','_','p','a','r','a','m','_','I','_','i','6','4'},{0 /* 1 */ ,},{'u','M',
'i','n','u','s','_','p','a','r','a','m','_','I','_','i','6','4'},{0 /* 1 */ ,},{'M','i','n','u','s','_','v','a','r','_','I','_','i','6','4'},{0 /* 1 */ ,},{'u','M','i','n','u','s','_','v','a','r','_','I','_','i','6','4'},{0 /* 1 */ ,},{'M','i','n','u','s','_','p','a','r','a','m','_','I','_','I'},{0 /* 1 */ ,},{'u','M','i','n','u','s','_','p','a','r','a','m','_','I','_','I'},{0 /* 1 */ ,},{'M','i','n','u','s','_','v','a','r','_','I','_','I'},{0 /* 1 */ ,},{'u','M','i','n','u','s','_','v','a','r','_','I','_','I'},{0 /* 1 */ ,},{'M','i','n','u','s','_','p','a','r','a','m','_','I','_','u','1','6'},{0 /* 1 */ ,},{'u','M','i','n','u','s','_','p','a','r','a','m','_','I','_','u','1','6'},{0 /* 1 */ ,},{'M','i','n','u','s','_','v','a','r','_','I','_','u','1','6'},{0 /* 1 */ ,},{'u','M','i','n','u','s','_','v','a','r','_','I','_','u','1','6'},{0 /* 1 */ ,},{'M','i','n','u','s','_','p','a','r','a','m','_','I','_','L','C'},{0 /* 1 */ ,},{'u','M','i','n','u','s','_','p','a','r','a','m','_','I','_',
'L','C'},{0 /* 1 */ ,},{'M','i','n','u','s','_','v','a','r','_','I','_','L','C'},{0 /* 1 */ ,},{'u','M','i','n','u','s','_','v','a','r','_','I','_','L','C'},{0 /* 1 */ ,},{'M','i','n','u','s','_','p','a','r','a','m','_','I','_','i','3','2'},{0 /* 1 */ ,},{'u','M','i','n','u','s','_','p','a','r','a','m','_','I','_','i','3','2'},{0 /* 1 */ ,},{'M','i','n','u','s','_','v','a','r','_','I','_','i','3','2'},{0 /* 1 */ ,},{'u','M','i','n','u','s','_','v','a','r','_','I','_','i','3','2'},{0 /* 1 */ ,},{'M','i','n','u','s','_','p','a','r','a','m','_','I','_','u','6','4'},{0 /* 1 */ ,},{'u','M','i','n','u','s','_','p','a','r','a','m','_','I','_','u','6','4'},{0 /* 1 */ ,},{'M','i','n','u','s','_','v','a','r','_','I','_','u','6','4'},{0 /* 1 */ ,},{'u','M','i','n','u','s','_','v','a','r','_','I','_','u','6','4'},{0 /* 1 */ ,},{'M','i','n','u','s','_','p','a','r','a','m','_','I','_','i','8'},{0 /* 1 */ ,},{'u','M','i','n','u','s','_','p','a','r','a','m','_','I','_','i','8'},{0 /* 1 */ ,},{'M','i',
'n','u','s','_','v','a','r','_','I','_','i','8'},{0 /* 1 */ ,},{'u','M','i','n','u','s','_','v','a','r','_','I','_','i','8'},{0 /* 1 */ ,},{'M','i','n','u','s','_','p','a','r','a','m','_','u','1','6','_','L'},{0 /* 1 */ ,},{'u','M','i','n','u','s','_','p','a','r','a','m','_','u','1','6','_','L'},{0 /* 1 */ ,},{'M','i','n','u','s','_','v','a','r','_','u','1','6','_','L'},{0 /* 1 */ ,},{'u','M','i','n','u','s','_','v','a','r','_','u','1','6','_','L'},{0 /* 1 */ ,},{'M','i','n','u','s','_','p','a','r','a','m','_','u','1','6','_','u','8'},{0 /* 1 */ ,},{'u','M','i','n','u','s','_','p','a','r','a','m','_','u','1','6','_','u','8'},{0 /* 1 */ ,},{'M','i','n','u','s','_','v','a','r','_','u','1','6','_','u','8'},{0 /* 1 */ ,},{'u','M','i','n','u','s','_','v','a','r','_','u','1','6','_','u','8'},{0 /* 1 */ ,},{'M','i','n','u','s','_','p','a','r','a','m','_','u','1','6','_','u','3','2'},{0 /* 1 */ ,},{'u','M','i','n','u','s','_','p','a','r','a','m','_','u','1','6','_','u','3','2'},{0 /* 1 */ ,},{
'M','i','n','u','s','_','v','a','r','_','u','1','6','_','u','3','2'},{0 /* 1 */ ,},{'u','M','i','n','u','s','_','v','a','r','_','u','1','6','_','u','3','2'},{0 /* 1 */ ,},{'M','i','n','u','s','_','p','a','r','a','m','_','u','1','6','_','C'},{0 /* 1 */ ,},{'u','M','i','n','u','s','_','p','a','r','a','m','_','u','1','6','_','C'},{0 /* 1 */ ,},{'M','i','n','u','s','_','v','a','r','_','u','1','6','_','C'},{0 /* 1 */ ,},{'u','M','i','n','u','s','_','v','a','r','_','u','1','6','_','C'},{0 /* 1 */ ,},{'M','i','n','u','s','_','p','a','r','a','m','_','u','1','6','_','i','1','6'},{0 /* 1 */ ,},{'u','M','i','n','u','s','_','p','a','r','a','m','_','u','1','6','_','i','1','6'},{0 /* 1 */ ,},{'M','i','n','u','s','_','v','a','r','_','u','1','6','_','i','1','6'},{0 /* 1 */ ,},{'u','M','i','n','u','s','_','v','a','r','_','u','1','6','_','i','1','6'},{0 /* 1 */ ,},{'M','i','n','u','s','_','p','a','r','a','m','_','u','1','6','_','i','6','4'},{0 /* 1 */ ,},{'u','M','i','n','u','s','_','p','a','r','a','m',
'_','u','1','6','_','i','6','4'},{0 /* 1 */ ,},{'M','i','n','u','s','_','v','a','r','_','u','1','6','_','i','6','4'},{0 /* 1 */ ,},{'u','M','i','n','u','s','_','v','a','r','_','u','1','6','_','i','6','4'},{0 /* 1 */ ,},{'M','i','n','u','s','_','p','a','r','a','m','_','u','1','6','_','I'},{0 /* 1 */ ,},{'u','M','i','n','u','s','_','p','a','r','a','m','_','u','1','6','_','I'},{0 /* 1 */ ,},{'M','i','n','u','s','_','v','a','r','_','u','1','6','_','I'},{0 /* 1 */ ,},{'u','M','i','n','u','s','_','v','a','r','_','u','1','6','_','I'},{0 /* 1 */ ,},{'M','i','n','u','s','_','p','a','r','a','m','_','u','1','6','_','u','1','6'},{0 /* 1 */ ,},{'u','M','i','n','u','s','_','p','a','r','a','m','_','u','1','6','_','u','1','6'},{0 /* 1 */ ,},{'M','i','n','u','s','_','v','a','r','_','u','1','6','_','u','1','6'},{0 /* 1 */ ,},{'u','M','i','n','u','s','_','v','a','r','_','u','1','6','_','u','1','6'},{0 /* 1 */ ,},{'M','i','n','u','s','_','p','a','r','a','m','_','u','1','6','_','L','C'},{0 /* 1 */ ,},{'u',
'M','i','n','u','s','_','p','a','r','a','m','_','u','1','6','_','L','C'},{0 /* 1 */ ,},{'M','i','n','u','s','_','v','a','r','_','u','1','6','_','L','C'},{0 /* 1 */ ,},{'u','M','i','n','u','s','_','v','a','r','_','u','1','6','_','L','C'},{0 /* 1 */ ,},{'M','i','n','u','s','_','p','a','r','a','m','_','u','1','6','_','i','3','2'},{0 /* 1 */ ,},{'u','M','i','n','u','s','_','p','a','r','a','m','_','u','1','6','_','i','3','2'},{0 /* 1 */ ,},{'M','i','n','u','s','_','v','a','r','_','u','1','6','_','i','3','2'},{0 /* 1 */ ,},{'u','M','i','n','u','s','_','v','a','r','_','u','1','6','_','i','3','2'},{0 /* 1 */ ,},{'M','i','n','u','s','_','p','a','r','a','m','_','u','1','6','_','u','6','4'},{0 /* 1 */ ,},{'u','M','i','n','u','s','_','p','a','r','a','m','_','u','1','6','_','u','6','4'},{0 /* 1 */ ,},{'M','i','n','u','s','_','v','a','r','_','u','1','6','_','u','6','4'},{0 /* 1 */ ,},{'u','M','i','n','u','s','_','v','a','r','_','u','1','6','_','u','6','4'},{0 /* 1 */ ,},{'M','i','n','u','s','_','p',
'a','r','a','m','_','u','1','6','_','i','8'},{0 /* 1 */ ,},{'u','M','i','n','u','s','_','p','a','r','a','m','_','u','1','6','_','i','8'},{0 /* 1 */ ,},{'M','i','n','u','s','_','v','a','r','_','u','1','6','_','i','8'},{0 /* 1 */ ,},{'u','M','i','n','u','s','_','v','a','r','_','u','1','6','_','i','8'},{0 /* 1 */ ,},{'M','i','n','u','s','_','p','a','r','a','m','_','L','C','_','L'},{0 /* 1 */ ,},{'u','M','i','n','u','s','_','p','a','r','a','m','_','L','C','_','L'},{0 /* 1 */ ,},{'M','i','n','u','s','_','v','a','r','_','L','C','_','L'},{0 /* 1 */ ,},{'u','M','i','n','u','s','_','v','a','r','_','L','C','_','L'},{0 /* 1 */ ,},{'M','i','n','u','s','_','p','a','r','a','m','_','L','C','_','u','8'},{0 /* 1 */ ,},{'u','M','i','n','u','s','_','p','a','r','a','m','_','L','C','_','u','8'},{0 /* 1 */ ,},{'M','i','n','u','s','_','v','a','r','_','L','C','_','u','8'},{0 /* 1 */ ,},{'u','M','i','n','u','s','_','v','a','r','_','L','C','_','u','8'},{0 /* 1 */ ,},{'M','i','n','u','s','_','p','a','r','a','m',
'_','L','C','_','u','3','2'},{0 /* 1 */ ,},{'u','M','i','n','u','s','_','p','a','r','a','m','_','L','C','_','u','3','2'},{0 /* 1 */ ,},{'M','i','n','u','s','_','v','a','r','_','L','C','_','u','3','2'},{0 /* 1 */ ,},{'u','M','i','n','u','s','_','v','a','r','_','L','C','_','u','3','2'},{0 /* 1 */ ,},{'M','i','n','u','s','_','p','a','r','a','m','_','L','C','_','C'},{0 /* 1 */ ,},{'u','M','i','n','u','s','_','p','a','r','a','m','_','L','C','_','C'},{0 /* 1 */ ,},{'M','i','n','u','s','_','v','a','r','_','L','C','_','C'},{0 /* 1 */ ,},{'u','M','i','n','u','s','_','v','a','r','_','L','C','_','C'},{0 /* 1 */ ,},{'M','i','n','u','s','_','p','a','r','a','m','_','L','C','_','i','1','6'},{0 /* 1 */ ,},{'u','M','i','n','u','s','_','p','a','r','a','m','_','L','C','_','i','1','6'},{0 /* 1 */ ,},{'M','i','n','u','s','_','v','a','r','_','L','C','_','i','1','6'},{0 /* 1 */ ,},{'u','M','i','n','u','s','_','v','a','r','_','L','C','_','i','1','6'},{0 /* 1 */ ,},{'M','i','n','u','s','_','p','a','r','a','m',
'_','L','C','_','i','6','4'},{0 /* 1 */ ,},{'u','M','i','n','u','s','_','p','a','r','a','m','_','L','C','_','i','6','4'},{0 /* 1 */ ,},{'M','i','n','u','s','_','v','a','r','_','L','C','_','i','6','4'},{0 /* 1 */ ,},{'u','M','i','n','u','s','_','v','a','r','_','L','C','_','i','6','4'},{0 /* 1 */ ,},{'M','i','n','u','s','_','p','a','r','a','m','_','L','C','_','I'},{0 /* 1 */ ,},{'u','M','i','n','u','s','_','p','a','r','a','m','_','L','C','_','I'},{0 /* 1 */ ,},{'M','i','n','u','s','_','v','a','r','_','L','C','_','I'},{0 /* 1 */ ,},{'u','M','i','n','u','s','_','v','a','r','_','L','C','_','I'},{0 /* 1 */ ,},{'M','i','n','u','s','_','p','a','r','a','m','_','L','C','_','u','1','6'},{0 /* 1 */ ,},{'u','M','i','n','u','s','_','p','a','r','a','m','_','L','C','_','u','1','6'},{0 /* 1 */ ,},{'M','i','n','u','s','_','v','a','r','_','L','C','_','u','1','6'},{0 /* 1 */ ,},{'u','M','i','n','u','s','_','v','a','r','_','L','C','_','u','1','6'},{0 /* 1 */ ,},{'M','i','n','u','s','_','p','a','r','a','m',
'_','L','C','_','L','C'},{0 /* 1 */ ,},{'u','M','i','n','u','s','_','p','a','r','a','m','_','L','C','_','L','C'},{0 /* 1 */ ,},{'M','i','n','u','s','_','v','a','r','_','L','C','_','L','C'},{0 /* 1 */ ,},{'u','M','i','n','u','s','_','v','a','r','_','L','C','_','L','C'},{0 /* 1 */ ,},{'M','i','n','u','s','_','p','a','r','a','m','_','L','C','_','i','3','2'},{0 /* 1 */ ,},{'u','M','i','n','u','s','_','p','a','r','a','m','_','L','C','_','i','3','2'},{0 /* 1 */ ,},{'M','i','n','u','s','_','v','a','r','_','L','C','_','i','3','2'},{0 /* 1 */ ,},{'u','M','i','n','u','s','_','v','a','r','_','L','C','_','i','3','2'},{0 /* 1 */ ,},{'M','i','n','u','s','_','p','a','r','a','m','_','L','C','_','u','6','4'},{0 /* 1 */ ,},{'u','M','i','n','u','s','_','p','a','r','a','m','_','L','C','_','u','6','4'},{0 /* 1 */ ,},{'M','i','n','u','s','_','v','a','r','_','L','C','_','u','6','4'},{0 /* 1 */ ,},{'u','M','i','n','u','s','_','v','a','r','_','L','C','_','u','6','4'},{0 /* 1 */ ,},{'M','i','n','u','s','_','p',
'a','r','a','m','_','L','C','_','i','8'},{0 /* 1 */ ,},{'u','M','i','n','u','s','_','p','a','r','a','m','_','L','C','_','i','8'},{0 /* 1 */ ,},{'M','i','n','u','s','_','v','a','r','_','L','C','_','i','8'},{0 /* 1 */ ,},{'u','M','i','n','u','s','_','v','a','r','_','L','C','_','i','8'},{0 /* 1 */ ,},{'M','i','n','u','s','_','p','a','r','a','m','_','i','3','2','_','L'},{0 /* 1 */ ,},{'u','M','i','n','u','s','_','p','a','r','a','m','_','i','3','2','_','L'},{0 /* 1 */ ,},{'M','i','n','u','s','_','v','a','r','_','i','3','2','_','L'},{0 /* 1 */ ,},{'u','M','i','n','u','s','_','v','a','r','_','i','3','2','_','L'},{0 /* 1 */ ,},{'M','i','n','u','s','_','p','a','r','a','m','_','i','3','2','_','u','8'},{0 /* 1 */ ,},{'u','M','i','n','u','s','_','p','a','r','a','m','_','i','3','2','_','u','8'},{0 /* 1 */ ,},{'M','i','n','u','s','_','v','a','r','_','i','3','2','_','u','8'},{0 /* 1 */ ,},{'u','M','i','n','u','s','_','v','a','r','_','i','3','2','_','u','8'},{0 /* 1 */ ,},{'M','i','n','u','s','_','p',
'a','r','a','m','_','i','3','2','_','u','3','2'},{0 /* 1 */ ,},{'u','M','i','n','u','s','_','p','a','r','a','m','_','i','3','2','_','u','3','2'},{0 /* 1 */ ,},{'M','i','n','u','s','_','v','a','r','_','i','3','2','_','u','3','2'},{0 /* 1 */ ,},{'u','M','i','n','u','s','_','v','a','r','_','i','3','2','_','u','3','2'},{0 /* 1 */ ,},{'M','i','n','u','s','_','p','a','r','a','m','_','i','3','2','_','C'},{0 /* 1 */ ,},{'u','M','i','n','u','s','_','p','a','r','a','m','_','i','3','2','_','C'},{0 /* 1 */ ,},{'M','i','n','u','s','_','v','a','r','_','i','3','2','_','C'},{0 /* 1 */ ,},{'u','M','i','n','u','s','_','v','a','r','_','i','3','2','_','C'},{0 /* 1 */ ,},{'M','i','n','u','s','_','p','a','r','a','m','_','i','3','2','_','i','1','6'},{0 /* 1 */ ,},{'u','M','i','n','u','s','_','p','a','r','a','m','_','i','3','2','_','i','1','6'},{0 /* 1 */ ,},{'M','i','n','u','s','_','v','a','r','_','i','3','2','_','i','1','6'},{0 /* 1 */ ,},{'u','M','i','n','u','s','_','v','a','r','_','i','3','2','_','i','1',
'6'},{0 /* 1 */ ,},{'M','i','n','u','s','_','p','a','r','a','m','_','i','3','2','_','i','6','4'},{0 /* 1 */ ,},{'u','M','i','n','u','s','_','p','a','r','a','m','_','i','3','2','_','i','6','4'},{0 /* 1 */ ,},{'M','i','n','u','s','_','v','a','r','_','i','3','2','_','i','6','4'},{0 /* 1 */ ,},{'u','M','i','n','u','s','_','v','a','r','_','i','3','2','_','i','6','4'},{0 /* 1 */ ,},{'M','i','n','u','s','_','p','a','r','a','m','_','i','3','2','_','I'},{0 /* 1 */ ,},{'u','M','i','n','u','s','_','p','a','r','a','m','_','i','3','2','_','I'},{0 /* 1 */ ,},{'M','i','n','u','s','_','v','a','r','_','i','3','2','_','I'},{0 /* 1 */ ,},{'u','M','i','n','u','s','_','v','a','r','_','i','3','2','_','I'},{0 /* 1 */ ,},{'M','i','n','u','s','_','p','a','r','a','m','_','i','3','2','_','u','1','6'},{0 /* 1 */ ,},{'u','M','i','n','u','s','_','p','a','r','a','m','_','i','3','2','_','u','1','6'},{0 /* 1 */ ,},{'M','i','n','u','s','_','v','a','r','_','i','3','2','_','u','1','6'},{0 /* 1 */ ,},{'u','M','i','n','u',
's','_','v','a','r','_','i','3','2','_','u','1','6'},{0 /* 1 */ ,},{'M','i','n','u','s','_','p','a','r','a','m','_','i','3','2','_','L','C'},{0 /* 1 */ ,},{'u','M','i','n','u','s','_','p','a','r','a','m','_','i','3','2','_','L','C'},{0 /* 1 */ ,},{'M','i','n','u','s','_','v','a','r','_','i','3','2','_','L','C'},{0 /* 1 */ ,},{'u','M','i','n','u','s','_','v','a','r','_','i','3','2','_','L','C'},{0 /* 1 */ ,},{'M','i','n','u','s','_','p','a','r','a','m','_','i','3','2','_','i','3','2'},{0 /* 1 */ ,},{'u','M','i','n','u','s','_','p','a','r','a','m','_','i','3','2','_','i','3','2'},{0 /* 1 */ ,},{'M','i','n','u','s','_','v','a','r','_','i','3','2','_','i','3','2'},{0 /* 1 */ ,},{'u','M','i','n','u','s','_','v','a','r','_','i','3','2','_','i','3','2'},{0 /* 1 */ ,},{'M','i','n','u','s','_','p','a','r','a','m','_','i','3','2','_','u','6','4'},{0 /* 1 */ ,},{'u','M','i','n','u','s','_','p','a','r','a','m','_','i','3','2','_','u','6','4'},{0 /* 1 */ ,},{'M','i','n','u','s','_','v','a','r','_',
'i','3','2','_','u','6','4'},{0 /* 1 */ ,},{'u','M','i','n','u','s','_','v','a','r','_','i','3','2','_','u','6','4'},{0 /* 1 */ ,},{'M','i','n','u','s','_','p','a','r','a','m','_','i','3','2','_','i','8'},{0 /* 1 */ ,},{'u','M','i','n','u','s','_','p','a','r','a','m','_','i','3','2','_','i','8'},{0 /* 1 */ ,},{'M','i','n','u','s','_','v','a','r','_','i','3','2','_','i','8'},{0 /* 1 */ ,},{'u','M','i','n','u','s','_','v','a','r','_','i','3','2','_','i','8'},{0 /* 1 */ ,},{'M','i','n','u','s','_','p','a','r','a','m','_','f','6','4','_','f','6','4'},{0 /* 1 */ ,},{'M','i','n','u','s','_','v','a','r','_','f','6','4','_','f','6','4'},{0 /* 1 */ ,},{'M','i','n','u','s','_','p','a','r','a','m','_','u','6','4','_','L'},{0 /* 1 */ ,},{'u','M','i','n','u','s','_','p','a','r','a','m','_','u','6','4','_','L'},{0 /* 1 */ ,},{'M','i','n','u','s','_','v','a','r','_','u','6','4','_','L'},{0 /* 1 */ ,},{'u','M','i','n','u','s','_','v','a','r','_','u','6','4','_','L'},{0 /* 1 */ ,},{'M','i','n','u','s',
'_','p','a','r','a','m','_','u','6','4','_','u','8'},{0 /* 1 */ ,},{'u','M','i','n','u','s','_','p','a','r','a','m','_','u','6','4','_','u','8'},{0 /* 1 */ ,},{'M','i','n','u','s','_','v','a','r','_','u','6','4','_','u','8'},{0 /* 1 */ ,},{'u','M','i','n','u','s','_','v','a','r','_','u','6','4','_','u','8'},{0 /* 1 */ ,},{'M','i','n','u','s','_','p','a','r','a','m','_','u','6','4','_','u','3','2'},{0 /* 1 */ ,},{'u','M','i','n','u','s','_','p','a','r','a','m','_','u','6','4','_','u','3','2'},{0 /* 1 */ ,},{'M','i','n','u','s','_','v','a','r','_','u','6','4','_','u','3','2'},{0 /* 1 */ ,},{'u','M','i','n','u','s','_','v','a','r','_','u','6','4','_','u','3','2'},{0 /* 1 */ ,},{'M','i','n','u','s','_','p','a','r','a','m','_','u','6','4','_','C'},{0 /* 1 */ ,},{'u','M','i','n','u','s','_','p','a','r','a','m','_','u','6','4','_','C'},{0 /* 1 */ ,},{'M','i','n','u','s','_','v','a','r','_','u','6','4','_','C'},{0 /* 1 */ ,},{'u','M','i','n','u','s','_','v','a','r','_','u','6','4','_','C'},{0 /* 1 */ 
,},{'M','i','n','u','s','_','p','a','r','a','m','_','u','6','4','_','i','1','6'},{0 /* 1 */ ,},{'u','M','i','n','u','s','_','p','a','r','a','m','_','u','6','4','_','i','1','6'},{0 /* 1 */ ,},{'M','i','n','u','s','_','v','a','r','_','u','6','4','_','i','1','6'},{0 /* 1 */ ,},{'u','M','i','n','u','s','_','v','a','r','_','u','6','4','_','i','1','6'},{0 /* 1 */ ,},{'M','i','n','u','s','_','p','a','r','a','m','_','u','6','4','_','i','6','4'},{0 /* 1 */ ,},{'u','M','i','n','u','s','_','p','a','r','a','m','_','u','6','4','_','i','6','4'},{0 /* 1 */ ,},{'M','i','n','u','s','_','v','a','r','_','u','6','4','_','i','6','4'},{0 /* 1 */ ,},{'u','M','i','n','u','s','_','v','a','r','_','u','6','4','_','i','6','4'},{0 /* 1 */ ,},{'M','i','n','u','s','_','p','a','r','a','m','_','u','6','4','_','I'},{0 /* 1 */ ,},{'u','M','i','n','u','s','_','p','a','r','a','m','_','u','6','4','_','I'},{0 /* 1 */ ,},{'M','i','n','u','s','_','v','a','r','_','u','6','4','_','I'},{0 /* 1 */ ,},{'u','M','i','n','u','s','_',
'v','a','r','_','u','6','4','_','I'},{0 /* 1 */ ,},{'M','i','n','u','s','_','p','a','r','a','m','_','u','6','4','_','u','1','6'},{0 /* 1 */ ,},{'u','M','i','n','u','s','_','p','a','r','a','m','_','u','6','4','_','u','1','6'},{0 /* 1 */ ,},{'M','i','n','u','s','_','v','a','r','_','u','6','4','_','u','1','6'},{0 /* 1 */ ,},{'u','M','i','n','u','s','_','v','a','r','_','u','6','4','_','u','1','6'},{0 /* 1 */ ,},{'M','i','n','u','s','_','p','a','r','a','m','_','u','6','4','_','L','C'},{0 /* 1 */ ,},{'u','M','i','n','u','s','_','p','a','r','a','m','_','u','6','4','_','L','C'},{0 /* 1 */ ,},{'M','i','n','u','s','_','v','a','r','_','u','6','4','_','L','C'},{0 /* 1 */ ,},{'u','M','i','n','u','s','_','v','a','r','_','u','6','4','_','L','C'},{0 /* 1 */ ,},{'M','i','n','u','s','_','p','a','r','a','m','_','u','6','4','_','i','3','2'},{0 /* 1 */ ,},{'u','M','i','n','u','s','_','p','a','r','a','m','_','u','6','4','_','i','3','2'},{0 /* 1 */ ,},{'M','i','n','u','s','_','v','a','r','_','u','6','4','_',
'i','3','2'},{0 /* 1 */ ,},{'u','M','i','n','u','s','_','v','a','r','_','u','6','4','_','i','3','2'},{0 /* 1 */ ,},{'M','i','n','u','s','_','p','a','r','a','m','_','u','6','4','_','u','6','4'},{0 /* 1 */ ,},{'u','M','i','n','u','s','_','p','a','r','a','m','_','u','6','4','_','u','6','4'},{0 /* 1 */ ,},{'M','i','n','u','s','_','v','a','r','_','u','6','4','_','u','6','4'},{0 /* 1 */ ,},{'u','M','i','n','u','s','_','v','a','r','_','u','6','4','_','u','6','4'},{0 /* 1 */ ,},{'M','i','n','u','s','_','p','a','r','a','m','_','u','6','4','_','i','8'},{0 /* 1 */ ,},{'u','M','i','n','u','s','_','p','a','r','a','m','_','u','6','4','_','i','8'},{0 /* 1 */ ,},{'M','i','n','u','s','_','v','a','r','_','u','6','4','_','i','8'},{0 /* 1 */ ,},{'u','M','i','n','u','s','_','v','a','r','_','u','6','4','_','i','8'},{0 /* 1 */ ,},{'M','i','n','u','s','_','p','a','r','a','m','_','i','8','_','L'},{0 /* 1 */ ,},{'u','M','i','n','u','s','_','p','a','r','a','m','_','i','8','_','L'},{0 /* 1 */ ,},{'M','i','n','u',
's','_','v','a','r','_','i','8','_','L'},{0 /* 1 */ ,},{'u','M','i','n','u','s','_','v','a','r','_','i','8','_','L'},{0 /* 1 */ ,},{'M','i','n','u','s','_','p','a','r','a','m','_','i','8','_','u','8'},{0 /* 1 */ ,},{'u','M','i','n','u','s','_','p','a','r','a','m','_','i','8','_','u','8'},{0 /* 1 */ ,},{'M','i','n','u','s','_','v','a','r','_','i','8','_','u','8'},{0 /* 1 */ ,},{'u','M','i','n','u','s','_','v','a','r','_','i','8','_','u','8'},{0 /* 1 */ ,},{'M','i','n','u','s','_','p','a','r','a','m','_','i','8','_','u','3','2'},{0 /* 1 */ ,},{'u','M','i','n','u','s','_','p','a','r','a','m','_','i','8','_','u','3','2'},{0 /* 1 */ ,},{'M','i','n','u','s','_','v','a','r','_','i','8','_','u','3','2'},{0 /* 1 */ ,},{'u','M','i','n','u','s','_','v','a','r','_','i','8','_','u','3','2'},{0 /* 1 */ ,},{'M','i','n','u','s','_','p','a','r','a','m','_','i','8','_','C'},{0 /* 1 */ ,},{'u','M','i','n','u','s','_','p','a','r','a','m','_','i','8','_','C'},{0 /* 1 */ ,},{'M','i','n','u','s','_','v','a',
'r','_','i','8','_','C'},{0 /* 1 */ ,},{'u','M','i','n','u','s','_','v','a','r','_','i','8','_','C'},{0 /* 1 */ ,},{'M','i','n','u','s','_','p','a','r','a','m','_','i','8','_','i','1','6'},{0 /* 1 */ ,},{'u','M','i','n','u','s','_','p','a','r','a','m','_','i','8','_','i','1','6'},{0 /* 1 */ ,},{'M','i','n','u','s','_','v','a','r','_','i','8','_','i','1','6'},{0 /* 1 */ ,},{'u','M','i','n','u','s','_','v','a','r','_','i','8','_','i','1','6'},{0 /* 1 */ ,},{'M','i','n','u','s','_','p','a','r','a','m','_','i','8','_','i','6','4'},{0 /* 1 */ ,},{'u','M','i','n','u','s','_','p','a','r','a','m','_','i','8','_','i','6','4'},{0 /* 1 */ ,},{'M','i','n','u','s','_','v','a','r','_','i','8','_','i','6','4'},{0 /* 1 */ ,},{'u','M','i','n','u','s','_','v','a','r','_','i','8','_','i','6','4'},{0 /* 1 */ ,},{'M','i','n','u','s','_','p','a','r','a','m','_','i','8','_','I'},{0 /* 1 */ ,},{'u','M','i','n','u','s','_','p','a','r','a','m','_','i','8','_','I'},{0 /* 1 */ ,},{'M','i','n','u','s','_','v','a',
'r','_','i','8','_','I'},{0 /* 1 */ ,},{'u','M','i','n','u','s','_','v','a','r','_','i','8','_','I'},{0 /* 1 */ ,},{'M','i','n','u','s','_','p','a','r','a','m','_','i','8','_','u','1','6'},{0 /* 1 */ ,},{'u','M','i','n','u','s','_','p','a','r','a','m','_','i','8','_','u','1','6'},{0 /* 1 */ ,},{'M','i','n','u','s','_','v','a','r','_','i','8','_','u','1','6'},{0 /* 1 */ ,},{'u','M','i','n','u','s','_','v','a','r','_','i','8','_','u','1','6'},{0 /* 1 */ ,},{'M','i','n','u','s','_','p','a','r','a','m','_','i','8','_','L','C'},{0 /* 1 */ ,},{'u','M','i','n','u','s','_','p','a','r','a','m','_','i','8','_','L','C'},{0 /* 1 */ ,},{'M','i','n','u','s','_','v','a','r','_','i','8','_','L','C'},{0 /* 1 */ ,},{'u','M','i','n','u','s','_','v','a','r','_','i','8','_','L','C'},{0 /* 1 */ ,},{'M','i','n','u','s','_','p','a','r','a','m','_','i','8','_','i','3','2'},{0 /* 1 */ ,},{'u','M','i','n','u','s','_','p','a','r','a','m','_','i','8','_','i','3','2'},{0 /* 1 */ ,},{'M','i','n','u','s','_','v','a',
'r','_','i','8','_','i','3','2'},{0 /* 1 */ ,},{'u','M','i','n','u','s','_','v','a','r','_','i','8','_','i','3','2'},{0 /* 1 */ ,},{'M','i','n','u','s','_','p','a','r','a','m','_','i','8','_','u','6','4'},{0 /* 1 */ ,},{'u','M','i','n','u','s','_','p','a','r','a','m','_','i','8','_','u','6','4'},{0 /* 1 */ ,},{'M','i','n','u','s','_','v','a','r','_','i','8','_','u','6','4'},{0 /* 1 */ ,},{'u','M','i','n','u','s','_','v','a','r','_','i','8','_','u','6','4'},{0 /* 1 */ ,},{'M','i','n','u','s','_','p','a','r','a','m','_','i','8','_','i','8'},{0 /* 1 */ ,},{'u','M','i','n','u','s','_','p','a','r','a','m','_','i','8','_','i','8'},{0 /* 1 */ ,},{'M','i','n','u','s','_','v','a','r','_','i','8','_','i','8'},{0 /* 1 */ ,},{'u','M','i','n','u','s','_','v','a','r','_','i','8','_','i','8'},{0 /* 1 */ ,0 /* 2 */ ,0 /* 3 */ ,0 /* 4 */ ,},{(ADDRESS)&Minus_M3,(char*)&Minus_m_11_L_12,(ADDRESS)&Minus__Minus_param_L_L,9+(char*)&Minus_m_11_L_12,(ADDRESS)&Minus__uMinus_param_L_L,25+(char*)&Minus_m_11_L_12,
(ADDRESS)&Minus__Minus_var_L_L,42+(char*)&Minus_m_11_L_12,(ADDRESS)&Minus__uMinus_var_L_L,56+(char*)&Minus_m_11_L_12,(ADDRESS)&Minus__Minus_param_L_u8,71+(char*)&Minus_m_11_L_12,(ADDRESS)&Minus__uMinus_param_L_u8,88+(char*)&Minus_m_11_L_12,(ADDRESS)&Minus__Minus_var_L_u8,106+(char*)&Minus_m_11_L_12,(ADDRESS)&Minus__uMinus_var_L_u8,121+(char*)&Minus_m_11_L_12,(ADDRESS)&Minus__Minus_param_L_u32,137+(char*)&Minus_m_11_L_12,(ADDRESS)&Minus__uMinus_param_L_u32,155+(char*)&Minus_m_11_L_12,(ADDRESS)&Minus__Minus_var_L_u32,174+(char*)&Minus_m_11_L_12,(ADDRESS)&Minus__uMinus_var_L_u32,190+(char*)&Minus_m_11_L_12,(ADDRESS)&Minus__Minus_param_L_C,207+(char*)&Minus_m_11_L_12,(ADDRESS)&Minus__uMinus_param_L_C,223+(char*)&Minus_m_11_L_12,(ADDRESS)&Minus__Minus_var_L_C,240+(char*)&Minus_m_11_L_12,(ADDRESS)&Minus__uMinus_var_L_C,254+(char*)&Minus_m_11_L_12,(ADDRESS)&Minus__Minus_param_L_i16,269+(char*)&Minus_m_11_L_12,(ADDRESS)&Minus__uMinus_param_L_i16,287+(char*)&Minus_m_11_L_12,(ADDRESS)&Minus__Minus_var_L_i16
,306+(char*)&Minus_m_11_L_12,(ADDRESS)&Minus__uMinus_var_L_i16,322+(char*)&Minus_m_11_L_12,(ADDRESS)&Minus__Minus_param_L_i64,339+(char*)&Minus_m_11_L_12,(ADDRESS)&Minus__uMinus_param_L_i64,357+(char*)&Minus_m_11_L_12,(ADDRESS)&Minus__Minus_var_L_i64,376+(char*)&Minus_m_11_L_12,(ADDRESS)&Minus__uMinus_var_L_i64,392+(char*)&Minus_m_11_L_12,(ADDRESS)&Minus__Minus_param_L_I,409+(char*)&Minus_m_11_L_12,(ADDRESS)&Minus__uMinus_param_L_I,425+(char*)&Minus_m_11_L_12,(ADDRESS)&Minus__Minus_var_L_I,442+(char*)&Minus_m_11_L_12,(ADDRESS)&Minus__uMinus_var_L_I,456+(char*)&Minus_m_11_L_12,(ADDRESS)&Minus__Minus_param_L_u16,471+(char*)&Minus_m_11_L_12,(ADDRESS)&Minus__uMinus_param_L_u16,489+(char*)&Minus_m_11_L_12,(ADDRESS)&Minus__Minus_var_L_u16,508+(char*)&Minus_m_11_L_12,(ADDRESS)&Minus__uMinus_var_L_u16,524+(char*)&Minus_m_11_L_12,(ADDRESS)&Minus__Minus_param_L_LC,541+(char*)&Minus_m_11_L_12,(ADDRESS)&Minus__uMinus_param_L_LC,558+(char*)&Minus_m_11_L_12,(ADDRESS)&Minus__Minus_var_L_LC,576+(char*)&Minus_m_11_L_12
,(ADDRESS)&Minus__uMinus_var_L_LC,591+(char*)&Minus_m_11_L_12,(ADDRESS)&Minus__Minus_param_L_i32,607+(char*)&Minus_m_11_L_12,(ADDRESS)&Minus__uMinus_param_L_i32,625+(char*)&Minus_m_11_L_12,(ADDRESS)&Minus__Minus_var_L_i32,644+(char*)&Minus_m_11_L_12,(ADDRESS)&Minus__uMinus_var_L_i32,660+(char*)&Minus_m_11_L_12,(ADDRESS)&Minus__Minus_param_L_u64,677+(char*)&Minus_m_11_L_12,(ADDRESS)&Minus__uMinus_param_L_u64,695+(char*)&Minus_m_11_L_12,(ADDRESS)&Minus__Minus_var_L_u64,714+(char*)&Minus_m_11_L_12,(ADDRESS)&Minus__uMinus_var_L_u64,730+(char*)&Minus_m_11_L_12,(ADDRESS)&Minus__Minus_param_L_i8,747+(char*)&Minus_m_11_L_12,(ADDRESS)&Minus__uMinus_param_L_i8,764+(char*)&Minus_m_11_L_12,(ADDRESS)&Minus__Minus_var_L_i8,782+(char*)&Minus_m_11_L_12,(ADDRESS)&Minus__uMinus_var_L_i8,797+(char*)&Minus_m_11_L_12,(ADDRESS)&Minus__Minus_param_u8_L,813+(char*)&Minus_m_11_L_12,(ADDRESS)&Minus__uMinus_param_u8_L,830+(char*)&Minus_m_11_L_12,(ADDRESS)&Minus__Minus_var_u8_L,848+(char*)&Minus_m_11_L_12,(ADDRESS)&Minus__uMinus_var_u8_L
,863+(char*)&Minus_m_11_L_12,(ADDRESS)&Minus__Minus_param_u8_u8,879+(char*)&Minus_m_11_L_12,(ADDRESS)&Minus__uMinus_param_u8_u8,897+(char*)&Minus_m_11_L_12,(ADDRESS)&Minus__Minus_var_u8_u8,916+(char*)&Minus_m_11_L_12,(ADDRESS)&Minus__uMinus_var_u8_u8,932+(char*)&Minus_m_11_L_12,(ADDRESS)&Minus__Minus_param_u8_u32,949+(char*)&Minus_m_11_L_12,(ADDRESS)&Minus__uMinus_param_u8_u32,968+(char*)&Minus_m_11_L_12,(ADDRESS)&Minus__Minus_var_u8_u32,988+(char*)&Minus_m_11_L_12,(ADDRESS)&Minus__uMinus_var_u8_u32,1005+(char*)&Minus_m_11_L_12,(ADDRESS)&Minus__Minus_param_u8_C,1023+(char*)&Minus_m_11_L_12,(ADDRESS)&Minus__uMinus_param_u8_C,1040+(char*)&Minus_m_11_L_12,(ADDRESS)&Minus__Minus_var_u8_C,1058+(char*)&Minus_m_11_L_12,(ADDRESS)&Minus__uMinus_var_u8_C,1073+(char*)&Minus_m_11_L_12,(ADDRESS)&Minus__Minus_param_u8_i16,1089+(char*)&Minus_m_11_L_12,(ADDRESS)&Minus__uMinus_param_u8_i16,1108+(char*)&Minus_m_11_L_12,(ADDRESS)&Minus__Minus_var_u8_i16,1128+(char*)&Minus_m_11_L_12,(ADDRESS)&Minus__uMinus_var_u8_i16
,1145+(char*)&Minus_m_11_L_12,(ADDRESS)&Minus__Minus_param_u8_i64,1163+(char*)&Minus_m_11_L_12,(ADDRESS)&Minus__uMinus_param_u8_i64,1182+(char*)&Minus_m_11_L_12,(ADDRESS)&Minus__Minus_var_u8_i64,1202+(char*)&Minus_m_11_L_12,(ADDRESS)&Minus__uMinus_var_u8_i64,1219+(char*)&Minus_m_11_L_12,(ADDRESS)&Minus__Minus_param_u8_I,1237+(char*)&Minus_m_11_L_12,(ADDRESS)&Minus__uMinus_param_u8_I,1254+(char*)&Minus_m_11_L_12,(ADDRESS)&Minus__Minus_var_u8_I,1272+(char*)&Minus_m_11_L_12,(ADDRESS)&Minus__uMinus_var_u8_I,1287+(char*)&Minus_m_11_L_12,(ADDRESS)&Minus__Minus_param_u8_u16,1303+(char*)&Minus_m_11_L_12,(ADDRESS)&Minus__uMinus_param_u8_u16,1322+(char*)&Minus_m_11_L_12,(ADDRESS)&Minus__Minus_var_u8_u16,1342+(char*)&Minus_m_11_L_12,(ADDRESS)&Minus__uMinus_var_u8_u16,1359+(char*)&Minus_m_11_L_12,(ADDRESS)&Minus__Minus_param_u8_LC,1377+(char*)&Minus_m_11_L_12,(ADDRESS)&Minus__uMinus_param_u8_LC,1395+(char*)&Minus_m_11_L_12,(ADDRESS)&Minus__Minus_var_u8_LC,1414+(char*)&Minus_m_11_L_12,(ADDRESS)&Minus__uMinus_var_u8_LC
,1430+(char*)&Minus_m_11_L_12,(ADDRESS)&Minus__Minus_param_u8_i32,1447+(char*)&Minus_m_11_L_12,(ADDRESS)&Minus__uMinus_param_u8_i32,1466+(char*)&Minus_m_11_L_12,(ADDRESS)&Minus__Minus_var_u8_i32,1486+(char*)&Minus_m_11_L_12,(ADDRESS)&Minus__uMinus_var_u8_i32,1503+(char*)&Minus_m_11_L_12,(ADDRESS)&Minus__Minus_param_u8_u64,1521+(char*)&Minus_m_11_L_12,(ADDRESS)&Minus__uMinus_param_u8_u64,1540+(char*)&Minus_m_11_L_12,(ADDRESS)&Minus__Minus_var_u8_u64,1560+(char*)&Minus_m_11_L_12,(ADDRESS)&Minus__uMinus_var_u8_u64,1577+(char*)&Minus_m_11_L_12,(ADDRESS)&Minus__Minus_param_u8_i8,1595+(char*)&Minus_m_11_L_12,(ADDRESS)&Minus__uMinus_param_u8_i8,1613+(char*)&Minus_m_11_L_12,(ADDRESS)&Minus__Minus_var_u8_i8,1632+(char*)&Minus_m_11_L_12,(ADDRESS)&Minus__uMinus_var_u8_i8,1648+(char*)&Minus_m_11_L_12,(ADDRESS)&Minus__Minus_param_u32_L,1665+(char*)&Minus_m_11_L_12,(ADDRESS)&Minus__uMinus_param_u32_L,1683+(char*)&Minus_m_11_L_12,(ADDRESS)&Minus__Minus_var_u32_L,1702+(char*)&Minus_m_11_L_12,(ADDRESS)&Minus__uMinus_var_u32_L
,1718+(char*)&Minus_m_11_L_12,(ADDRESS)&Minus__Minus_param_u32_u8,1735+(char*)&Minus_m_11_L_12,(ADDRESS)&Minus__uMinus_param_u32_u8,1754+(char*)&Minus_m_11_L_12,(ADDRESS)&Minus__Minus_var_u32_u8,1774+(char*)&Minus_m_11_L_12,(ADDRESS)&Minus__uMinus_var_u32_u8,1791+(char*)&Minus_m_11_L_12,(ADDRESS)&Minus__Minus_param_u32_u32,1809+(char*)&Minus_m_11_L_12,(ADDRESS)&Minus__uMinus_param_u32_u32,1829+(char*)&Minus_m_11_L_12,(ADDRESS)&Minus__Minus_var_u32_u32,1850+(char*)&Minus_m_11_L_12,(ADDRESS)&Minus__uMinus_var_u32_u32,1868+(char*)&Minus_m_11_L_12,(ADDRESS)&Minus__Minus_param_u32_C,1887+(char*)&Minus_m_11_L_12,(ADDRESS)&Minus__uMinus_param_u32_C,1905+(char*)&Minus_m_11_L_12,(ADDRESS)&Minus__Minus_var_u32_C,1924+(char*)&Minus_m_11_L_12,(ADDRESS)&Minus__uMinus_var_u32_C,1940+(char*)&Minus_m_11_L_12,(ADDRESS)&Minus__Minus_param_u32_i16,1957+(char*)&Minus_m_11_L_12,(ADDRESS)&Minus__uMinus_param_u32_i16,1977+(char*)&Minus_m_11_L_12,(ADDRESS)&Minus__Minus_var_u32_i16,1998+(char*)&Minus_m_11_L_12
,(ADDRESS)&Minus__uMinus_var_u32_i16,2016+(char*)&Minus_m_11_L_12,(ADDRESS)&Minus__Minus_param_u32_i64,2035+(char*)&Minus_m_11_L_12,(ADDRESS)&Minus__uMinus_param_u32_i64,2055+(char*)&Minus_m_11_L_12,(ADDRESS)&Minus__Minus_var_u32_i64,2076+(char*)&Minus_m_11_L_12,(ADDRESS)&Minus__uMinus_var_u32_i64,2094+(char*)&Minus_m_11_L_12,(ADDRESS)&Minus__Minus_param_u32_I,2113+(char*)&Minus_m_11_L_12,(ADDRESS)&Minus__uMinus_param_u32_I,2131+(char*)&Minus_m_11_L_12,(ADDRESS)&Minus__Minus_var_u32_I,2150+(char*)&Minus_m_11_L_12,(ADDRESS)&Minus__uMinus_var_u32_I,2166+(char*)&Minus_m_11_L_12,(ADDRESS)&Minus__Minus_param_u32_u16,2183+(char*)&Minus_m_11_L_12,(ADDRESS)&Minus__uMinus_param_u32_u16,2203+(char*)&Minus_m_11_L_12,(ADDRESS)&Minus__Minus_var_u32_u16,2224+(char*)&Minus_m_11_L_12,(ADDRESS)&Minus__uMinus_var_u32_u16,2242+(char*)&Minus_m_11_L_12,(ADDRESS)&Minus__Minus_param_u32_LC,2261+(char*)&Minus_m_11_L_12,(ADDRESS)&Minus__uMinus_param_u32_LC,2280+(char*)&Minus_m_11_L_12,(ADDRESS)&Minus__Minus_var_u32_LC
,2300+(char*)&Minus_m_11_L_12,(ADDRESS)&Minus__uMinus_var_u32_LC,2317+(char*)&Minus_m_11_L_12,(ADDRESS)&Minus__Minus_param_u32_i32,2335+(char*)&Minus_m_11_L_12,(ADDRESS)&Minus__uMinus_param_u32_i32,2355+(char*)&Minus_m_11_L_12,(ADDRESS)&Minus__Minus_var_u32_i32,2376+(char*)&Minus_m_11_L_12,(ADDRESS)&Minus__uMinus_var_u32_i32,2394+(char*)&Minus_m_11_L_12,(ADDRESS)&Minus__Minus_param_u32_u64,2413+(char*)&Minus_m_11_L_12,(ADDRESS)&Minus__uMinus_param_u32_u64,2433+(char*)&Minus_m_11_L_12,(ADDRESS)&Minus__Minus_var_u32_u64,2454+(char*)&Minus_m_11_L_12,(ADDRESS)&Minus__uMinus_var_u32_u64,2472+(char*)&Minus_m_11_L_12,(ADDRESS)&Minus__Minus_param_u32_i8,2491+(char*)&Minus_m_11_L_12,(ADDRESS)&Minus__uMinus_param_u32_i8,2510+(char*)&Minus_m_11_L_12,(ADDRESS)&Minus__Minus_var_u32_i8,2530+(char*)&Minus_m_11_L_12,(ADDRESS)&Minus__uMinus_var_u32_i8,2547+(char*)&Minus_m_11_L_12,(ADDRESS)&Minus__Minus_param_C_L,2565+(char*)&Minus_m_11_L_12,(ADDRESS)&Minus__uMinus_param_C_L,2581+(char*)&Minus_m_11_L_12
,(ADDRESS)&Minus__Minus_var_C_L,2598+(char*)&Minus_m_11_L_12,(ADDRESS)&Minus__uMinus_var_C_L,2612+(char*)&Minus_m_11_L_12,(ADDRESS)&Minus__Minus_param_C_u8,2627+(char*)&Minus_m_11_L_12,(ADDRESS)&Minus__uMinus_param_C_u8,2644+(char*)&Minus_m_11_L_12,(ADDRESS)&Minus__Minus_var_C_u8,2662+(char*)&Minus_m_11_L_12,(ADDRESS)&Minus__uMinus_var_C_u8,2677+(char*)&Minus_m_11_L_12,(ADDRESS)&Minus__Minus_param_C_u32,2693+(char*)&Minus_m_11_L_12,(ADDRESS)&Minus__uMinus_param_C_u32,2711+(char*)&Minus_m_11_L_12,(ADDRESS)&Minus__Minus_var_C_u32,2730+(char*)&Minus_m_11_L_12,(ADDRESS)&Minus__uMinus_var_C_u32,2746+(char*)&Minus_m_11_L_12,(ADDRESS)&Minus__Minus_param_C_C,2763+(char*)&Minus_m_11_L_12,(ADDRESS)&Minus__uMinus_param_C_C,2779+(char*)&Minus_m_11_L_12,(ADDRESS)&Minus__Minus_var_C_C,2796+(char*)&Minus_m_11_L_12,(ADDRESS)&Minus__uMinus_var_C_C,2810+(char*)&Minus_m_11_L_12,(ADDRESS)&Minus__Minus_param_C_i16,2825+(char*)&Minus_m_11_L_12,(ADDRESS)&Minus__uMinus_param_C_i16,2843+(char*)&Minus_m_11_L_12
,(ADDRESS)&Minus__Minus_var_C_i16,2862+(char*)&Minus_m_11_L_12,(ADDRESS)&Minus__uMinus_var_C_i16,2878+(char*)&Minus_m_11_L_12,(ADDRESS)&Minus__Minus_param_C_i64,2895+(char*)&Minus_m_11_L_12,(ADDRESS)&Minus__uMinus_param_C_i64,2913+(char*)&Minus_m_11_L_12,(ADDRESS)&Minus__Minus_var_C_i64,2932+(char*)&Minus_m_11_L_12,(ADDRESS)&Minus__uMinus_var_C_i64,2948+(char*)&Minus_m_11_L_12,(ADDRESS)&Minus__Minus_param_C_I,2965+(char*)&Minus_m_11_L_12,(ADDRESS)&Minus__uMinus_param_C_I,2981+(char*)&Minus_m_11_L_12,(ADDRESS)&Minus__Minus_var_C_I,2998+(char*)&Minus_m_11_L_12,(ADDRESS)&Minus__uMinus_var_C_I,3012+(char*)&Minus_m_11_L_12,(ADDRESS)&Minus__Minus_param_C_u16,3027+(char*)&Minus_m_11_L_12,(ADDRESS)&Minus__uMinus_param_C_u16,3045+(char*)&Minus_m_11_L_12,(ADDRESS)&Minus__Minus_var_C_u16,3064+(char*)&Minus_m_11_L_12,(ADDRESS)&Minus__uMinus_var_C_u16,3080+(char*)&Minus_m_11_L_12,(ADDRESS)&Minus__Minus_param_C_LC,3097+(char*)&Minus_m_11_L_12,(ADDRESS)&Minus__uMinus_param_C_LC,3114+(char*)&Minus_m_11_L_12
,(ADDRESS)&Minus__Minus_var_C_LC,3132+(char*)&Minus_m_11_L_12,(ADDRESS)&Minus__uMinus_var_C_LC,3147+(char*)&Minus_m_11_L_12,(ADDRESS)&Minus__Minus_param_C_i32,3163+(char*)&Minus_m_11_L_12,(ADDRESS)&Minus__uMinus_param_C_i32,3181+(char*)&Minus_m_11_L_12,(ADDRESS)&Minus__Minus_var_C_i32,3200+(char*)&Minus_m_11_L_12,(ADDRESS)&Minus__uMinus_var_C_i32,3216+(char*)&Minus_m_11_L_12,(ADDRESS)&Minus__Minus_param_C_u64,3233+(char*)&Minus_m_11_L_12,(ADDRESS)&Minus__uMinus_param_C_u64,3251+(char*)&Minus_m_11_L_12,(ADDRESS)&Minus__Minus_var_C_u64,3270+(char*)&Minus_m_11_L_12,(ADDRESS)&Minus__uMinus_var_C_u64,3286+(char*)&Minus_m_11_L_12,(ADDRESS)&Minus__Minus_param_C_i8,3303+(char*)&Minus_m_11_L_12,(ADDRESS)&Minus__uMinus_param_C_i8,3320+(char*)&Minus_m_11_L_12,(ADDRESS)&Minus__Minus_var_C_i8,3338+(char*)&Minus_m_11_L_12,(ADDRESS)&Minus__uMinus_var_C_i8,3353+(char*)&Minus_m_11_L_12,(ADDRESS)&Minus__Minus_param_i16_L,3369+(char*)&Minus_m_11_L_12,(ADDRESS)&Minus__uMinus_param_i16_L,3387+(char*)&Minus_m_11_L_12
,(ADDRESS)&Minus__Minus_var_i16_L,3406+(char*)&Minus_m_11_L_12,(ADDRESS)&Minus__uMinus_var_i16_L,3422+(char*)&Minus_m_11_L_12,(ADDRESS)&Minus__Minus_param_i16_u8,3439+(char*)&Minus_m_11_L_12,(ADDRESS)&Minus__uMinus_param_i16_u8,3458+(char*)&Minus_m_11_L_12,(ADDRESS)&Minus__Minus_var_i16_u8,3478+(char*)&Minus_m_11_L_12,(ADDRESS)&Minus__uMinus_var_i16_u8,3495+(char*)&Minus_m_11_L_12,(ADDRESS)&Minus__Minus_param_i16_u32,3513+(char*)&Minus_m_11_L_12,(ADDRESS)&Minus__uMinus_param_i16_u32,3533+(char*)&Minus_m_11_L_12,(ADDRESS)&Minus__Minus_var_i16_u32,3554+(char*)&Minus_m_11_L_12,(ADDRESS)&Minus__uMinus_var_i16_u32,3572+(char*)&Minus_m_11_L_12,(ADDRESS)&Minus__Minus_param_i16_C,3591+(char*)&Minus_m_11_L_12,(ADDRESS)&Minus__uMinus_param_i16_C,3609+(char*)&Minus_m_11_L_12,(ADDRESS)&Minus__Minus_var_i16_C,3628+(char*)&Minus_m_11_L_12,(ADDRESS)&Minus__uMinus_var_i16_C,3644+(char*)&Minus_m_11_L_12,(ADDRESS)&Minus__Minus_param_i16_i16,3661+(char*)&Minus_m_11_L_12,(ADDRESS)&Minus__uMinus_param_i16_i16
,3681+(char*)&Minus_m_11_L_12,(ADDRESS)&Minus__Minus_var_i16_i16,3702+(char*)&Minus_m_11_L_12,(ADDRESS)&Minus__uMinus_var_i16_i16,3720+(char*)&Minus_m_11_L_12,(ADDRESS)&Minus__Minus_param_i16_i64,3739+(char*)&Minus_m_11_L_12,(ADDRESS)&Minus__uMinus_param_i16_i64,3759+(char*)&Minus_m_11_L_12,(ADDRESS)&Minus__Minus_var_i16_i64,3780+(char*)&Minus_m_11_L_12,(ADDRESS)&Minus__uMinus_var_i16_i64,3798+(char*)&Minus_m_11_L_12,(ADDRESS)&Minus__Minus_param_i16_I,3817+(char*)&Minus_m_11_L_12,(ADDRESS)&Minus__uMinus_param_i16_I,3835+(char*)&Minus_m_11_L_12,(ADDRESS)&Minus__Minus_var_i16_I,3854+(char*)&Minus_m_11_L_12,(ADDRESS)&Minus__uMinus_var_i16_I,3870+(char*)&Minus_m_11_L_12,(ADDRESS)&Minus__Minus_param_i16_u16,3887+(char*)&Minus_m_11_L_12,(ADDRESS)&Minus__uMinus_param_i16_u16,3907+(char*)&Minus_m_11_L_12,(ADDRESS)&Minus__Minus_var_i16_u16,3928+(char*)&Minus_m_11_L_12,(ADDRESS)&Minus__uMinus_var_i16_u16,3946+(char*)&Minus_m_11_L_12,(ADDRESS)&Minus__Minus_param_i16_LC,3965+(char*)&Minus_m_11_L_12
,(ADDRESS)&Minus__uMinus_param_i16_LC,3984+(char*)&Minus_m_11_L_12,(ADDRESS)&Minus__Minus_var_i16_LC,4004+(char*)&Minus_m_11_L_12,(ADDRESS)&Minus__uMinus_var_i16_LC,4021+(char*)&Minus_m_11_L_12,(ADDRESS)&Minus__Minus_param_i16_i32,4039+(char*)&Minus_m_11_L_12,(ADDRESS)&Minus__uMinus_param_i16_i32,4059+(char*)&Minus_m_11_L_12,(ADDRESS)&Minus__Minus_var_i16_i32,4080+(char*)&Minus_m_11_L_12,(ADDRESS)&Minus__uMinus_var_i16_i32,4098+(char*)&Minus_m_11_L_12,(ADDRESS)&Minus__Minus_param_i16_u64,4117+(char*)&Minus_m_11_L_12,(ADDRESS)&Minus__uMinus_param_i16_u64,4137+(char*)&Minus_m_11_L_12,(ADDRESS)&Minus__Minus_var_i16_u64,4158+(char*)&Minus_m_11_L_12,(ADDRESS)&Minus__uMinus_var_i16_u64,4176+(char*)&Minus_m_11_L_12,(ADDRESS)&Minus__Minus_param_i16_i8,4195+(char*)&Minus_m_11_L_12,(ADDRESS)&Minus__uMinus_param_i16_i8,4214+(char*)&Minus_m_11_L_12,(ADDRESS)&Minus__Minus_var_i16_i8,4234+(char*)&Minus_m_11_L_12,(ADDRESS)&Minus__uMinus_var_i16_i8,4251+(char*)&Minus_m_11_L_12,(ADDRESS)&Minus__Minus_param_f32_f32
,4269+(char*)&Minus_m_11_L_12,(ADDRESS)&Minus__Minus_var_f32_f32,4289+(char*)&Minus_m_11_L_12,(ADDRESS)&Minus__Minus_param_i64_L,4307+(char*)&Minus_m_11_L_12,(ADDRESS)&Minus__uMinus_param_i64_L,4325+(char*)&Minus_m_11_L_12,(ADDRESS)&Minus__Minus_var_i64_L,4344+(char*)&Minus_m_11_L_12,(ADDRESS)&Minus__uMinus_var_i64_L,4360+(char*)&Minus_m_11_L_12,(ADDRESS)&Minus__Minus_param_i64_u8,4377+(char*)&Minus_m_11_L_12,(ADDRESS)&Minus__uMinus_param_i64_u8,4396+(char*)&Minus_m_11_L_12,(ADDRESS)&Minus__Minus_var_i64_u8,4416+(char*)&Minus_m_11_L_12,(ADDRESS)&Minus__uMinus_var_i64_u8,4433+(char*)&Minus_m_11_L_12,(ADDRESS)&Minus__Minus_param_i64_u32,4451+(char*)&Minus_m_11_L_12,(ADDRESS)&Minus__uMinus_param_i64_u32,4471+(char*)&Minus_m_11_L_12,(ADDRESS)&Minus__Minus_var_i64_u32,4492+(char*)&Minus_m_11_L_12,(ADDRESS)&Minus__uMinus_var_i64_u32,4510+(char*)&Minus_m_11_L_12,(ADDRESS)&Minus__Minus_param_i64_C,4529+(char*)&Minus_m_11_L_12,(ADDRESS)&Minus__uMinus_param_i64_C,4547+(char*)&Minus_m_11_L_12,(ADDRESS)&Minus__Minus_var_i64_C
,4566+(char*)&Minus_m_11_L_12,(ADDRESS)&Minus__uMinus_var_i64_C,4582+(char*)&Minus_m_11_L_12,(ADDRESS)&Minus__Minus_param_i64_i16,4599+(char*)&Minus_m_11_L_12,(ADDRESS)&Minus__uMinus_param_i64_i16,4619+(char*)&Minus_m_11_L_12,(ADDRESS)&Minus__Minus_var_i64_i16,4640+(char*)&Minus_m_11_L_12,(ADDRESS)&Minus__uMinus_var_i64_i16,4658+(char*)&Minus_m_11_L_12,(ADDRESS)&Minus__Minus_param_i64_i64,4677+(char*)&Minus_m_11_L_12,(ADDRESS)&Minus__uMinus_param_i64_i64,4697+(char*)&Minus_m_11_L_12,(ADDRESS)&Minus__Minus_var_i64_i64,4718+(char*)&Minus_m_11_L_12,(ADDRESS)&Minus__uMinus_var_i64_i64,4736+(char*)&Minus_m_11_L_12,(ADDRESS)&Minus__Minus_param_i64_I,4755+(char*)&Minus_m_11_L_12,(ADDRESS)&Minus__uMinus_param_i64_I,4773+(char*)&Minus_m_11_L_12,(ADDRESS)&Minus__Minus_var_i64_I,4792+(char*)&Minus_m_11_L_12,(ADDRESS)&Minus__uMinus_var_i64_I,4808+(char*)&Minus_m_11_L_12,(ADDRESS)&Minus__Minus_param_i64_u16,4825+(char*)&Minus_m_11_L_12,(ADDRESS)&Minus__uMinus_param_i64_u16,4845+(char*)&Minus_m_11_L_12
,(ADDRESS)&Minus__Minus_var_i64_u16,4866+(char*)&Minus_m_11_L_12,(ADDRESS)&Minus__uMinus_var_i64_u16,4884+(char*)&Minus_m_11_L_12,(ADDRESS)&Minus__Minus_param_i64_LC,4903+(char*)&Minus_m_11_L_12,(ADDRESS)&Minus__uMinus_param_i64_LC,4922+(char*)&Minus_m_11_L_12,(ADDRESS)&Minus__Minus_var_i64_LC,4942+(char*)&Minus_m_11_L_12,(ADDRESS)&Minus__uMinus_var_i64_LC,4959+(char*)&Minus_m_11_L_12,(ADDRESS)&Minus__Minus_param_i64_i32,4977+(char*)&Minus_m_11_L_12,(ADDRESS)&Minus__uMinus_param_i64_i32,4997+(char*)&Minus_m_11_L_12,(ADDRESS)&Minus__Minus_var_i64_i32,5018+(char*)&Minus_m_11_L_12,(ADDRESS)&Minus__uMinus_var_i64_i32,5036+(char*)&Minus_m_11_L_12,(ADDRESS)&Minus__Minus_param_i64_u64,5055+(char*)&Minus_m_11_L_12,(ADDRESS)&Minus__uMinus_param_i64_u64,5075+(char*)&Minus_m_11_L_12,(ADDRESS)&Minus__Minus_var_i64_u64,5096+(char*)&Minus_m_11_L_12,(ADDRESS)&Minus__uMinus_var_i64_u64,5114+(char*)&Minus_m_11_L_12,(ADDRESS)&Minus__Minus_param_i64_i8,5133+(char*)&Minus_m_11_L_12,(ADDRESS)&Minus__uMinus_param_i64_i8
,5152+(char*)&Minus_m_11_L_12,(ADDRESS)&Minus__Minus_var_i64_i8,5172+(char*)&Minus_m_11_L_12,(ADDRESS)&Minus__uMinus_var_i64_i8,5189+(char*)&Minus_m_11_L_12,(ADDRESS)&Minus__Minus_param_I_L,5207+(char*)&Minus_m_11_L_12,(ADDRESS)&Minus__uMinus_param_I_L,5223+(char*)&Minus_m_11_L_12,(ADDRESS)&Minus__Minus_var_I_L,5240+(char*)&Minus_m_11_L_12,(ADDRESS)&Minus__uMinus_var_I_L,5254+(char*)&Minus_m_11_L_12,(ADDRESS)&Minus__Minus_param_I_u8,5269+(char*)&Minus_m_11_L_12,(ADDRESS)&Minus__uMinus_param_I_u8,5286+(char*)&Minus_m_11_L_12,(ADDRESS)&Minus__Minus_var_I_u8,5304+(char*)&Minus_m_11_L_12,(ADDRESS)&Minus__uMinus_var_I_u8,5319+(char*)&Minus_m_11_L_12,(ADDRESS)&Minus__Minus_param_I_u32,5335+(char*)&Minus_m_11_L_12,(ADDRESS)&Minus__uMinus_param_I_u32,5353+(char*)&Minus_m_11_L_12,(ADDRESS)&Minus__Minus_var_I_u32,5372+(char*)&Minus_m_11_L_12,(ADDRESS)&Minus__uMinus_var_I_u32,5388+(char*)&Minus_m_11_L_12,(ADDRESS)&Minus__Minus_param_I_C,5405+(char*)&Minus_m_11_L_12,(ADDRESS)&Minus__uMinus_param_I_C
,5421+(char*)&Minus_m_11_L_12,(ADDRESS)&Minus__Minus_var_I_C,5438+(char*)&Minus_m_11_L_12,(ADDRESS)&Minus__uMinus_var_I_C,5452+(char*)&Minus_m_11_L_12,(ADDRESS)&Minus__Minus_param_I_i16,5467+(char*)&Minus_m_11_L_12,(ADDRESS)&Minus__uMinus_param_I_i16,5485+(char*)&Minus_m_11_L_12,(ADDRESS)&Minus__Minus_var_I_i16,5504+(char*)&Minus_m_11_L_12,(ADDRESS)&Minus__uMinus_var_I_i16,5520+(char*)&Minus_m_11_L_12,(ADDRESS)&Minus__Minus_param_I_i64,5537+(char*)&Minus_m_11_L_12,(ADDRESS)&Minus__uMinus_param_I_i64,5555+(char*)&Minus_m_11_L_12,(ADDRESS)&Minus__Minus_var_I_i64,5574+(char*)&Minus_m_11_L_12,(ADDRESS)&Minus__uMinus_var_I_i64,5590+(char*)&Minus_m_11_L_12,(ADDRESS)&Minus__Minus_param_I_I,5607+(char*)&Minus_m_11_L_12,(ADDRESS)&Minus__uMinus_param_I_I,5623+(char*)&Minus_m_11_L_12,(ADDRESS)&Minus__Minus_var_I_I,5640+(char*)&Minus_m_11_L_12,(ADDRESS)&Minus__uMinus_var_I_I,5654+(char*)&Minus_m_11_L_12,(ADDRESS)&Minus__Minus_param_I_u16,5669+(char*)&Minus_m_11_L_12,(ADDRESS)&Minus__uMinus_param_I_u16
,5687+(char*)&Minus_m_11_L_12,(ADDRESS)&Minus__Minus_var_I_u16,5706+(char*)&Minus_m_11_L_12,(ADDRESS)&Minus__uMinus_var_I_u16,5722+(char*)&Minus_m_11_L_12,(ADDRESS)&Minus__Minus_param_I_LC,5739+(char*)&Minus_m_11_L_12,(ADDRESS)&Minus__uMinus_param_I_LC,5756+(char*)&Minus_m_11_L_12,(ADDRESS)&Minus__Minus_var_I_LC,5774+(char*)&Minus_m_11_L_12,(ADDRESS)&Minus__uMinus_var_I_LC,5789+(char*)&Minus_m_11_L_12,(ADDRESS)&Minus__Minus_param_I_i32,5805+(char*)&Minus_m_11_L_12,(ADDRESS)&Minus__uMinus_param_I_i32,5823+(char*)&Minus_m_11_L_12,(ADDRESS)&Minus__Minus_var_I_i32,5842+(char*)&Minus_m_11_L_12,(ADDRESS)&Minus__uMinus_var_I_i32,5858+(char*)&Minus_m_11_L_12,(ADDRESS)&Minus__Minus_param_I_u64,5875+(char*)&Minus_m_11_L_12,(ADDRESS)&Minus__uMinus_param_I_u64,5893+(char*)&Minus_m_11_L_12,(ADDRESS)&Minus__Minus_var_I_u64,5912+(char*)&Minus_m_11_L_12,(ADDRESS)&Minus__uMinus_var_I_u64,5928+(char*)&Minus_m_11_L_12,(ADDRESS)&Minus__Minus_param_I_i8,5945+(char*)&Minus_m_11_L_12,(ADDRESS)&Minus__uMinus_param_I_i8
,5962+(char*)&Minus_m_11_L_12,(ADDRESS)&Minus__Minus_var_I_i8,5980+(char*)&Minus_m_11_L_12,(ADDRESS)&Minus__uMinus_var_I_i8,5995+(char*)&Minus_m_11_L_12,(ADDRESS)&Minus__Minus_param_u16_L,6011+(char*)&Minus_m_11_L_12,(ADDRESS)&Minus__uMinus_param_u16_L,6029+(char*)&Minus_m_11_L_12,(ADDRESS)&Minus__Minus_var_u16_L,6048+(char*)&Minus_m_11_L_12,(ADDRESS)&Minus__uMinus_var_u16_L,6064+(char*)&Minus_m_11_L_12,(ADDRESS)&Minus__Minus_param_u16_u8,6081+(char*)&Minus_m_11_L_12,(ADDRESS)&Minus__uMinus_param_u16_u8,6100+(char*)&Minus_m_11_L_12,(ADDRESS)&Minus__Minus_var_u16_u8,6120+(char*)&Minus_m_11_L_12,(ADDRESS)&Minus__uMinus_var_u16_u8,6137+(char*)&Minus_m_11_L_12,(ADDRESS)&Minus__Minus_param_u16_u32,6155+(char*)&Minus_m_11_L_12,(ADDRESS)&Minus__uMinus_param_u16_u32,6175+(char*)&Minus_m_11_L_12,(ADDRESS)&Minus__Minus_var_u16_u32,6196+(char*)&Minus_m_11_L_12,(ADDRESS)&Minus__uMinus_var_u16_u32,6214+(char*)&Minus_m_11_L_12,(ADDRESS)&Minus__Minus_param_u16_C,6233+(char*)&Minus_m_11_L_12,(ADDRESS)&Minus__uMinus_param_u16_C
,6251+(char*)&Minus_m_11_L_12,(ADDRESS)&Minus__Minus_var_u16_C,6270+(char*)&Minus_m_11_L_12,(ADDRESS)&Minus__uMinus_var_u16_C,6286+(char*)&Minus_m_11_L_12,(ADDRESS)&Minus__Minus_param_u16_i16,6303+(char*)&Minus_m_11_L_12,(ADDRESS)&Minus__uMinus_param_u16_i16,6323+(char*)&Minus_m_11_L_12,(ADDRESS)&Minus__Minus_var_u16_i16,6344+(char*)&Minus_m_11_L_12,(ADDRESS)&Minus__uMinus_var_u16_i16,6362+(char*)&Minus_m_11_L_12,(ADDRESS)&Minus__Minus_param_u16_i64,6381+(char*)&Minus_m_11_L_12,(ADDRESS)&Minus__uMinus_param_u16_i64,6401+(char*)&Minus_m_11_L_12,(ADDRESS)&Minus__Minus_var_u16_i64,6422+(char*)&Minus_m_11_L_12,(ADDRESS)&Minus__uMinus_var_u16_i64,6440+(char*)&Minus_m_11_L_12,(ADDRESS)&Minus__Minus_param_u16_I,6459+(char*)&Minus_m_11_L_12,(ADDRESS)&Minus__uMinus_param_u16_I,6477+(char*)&Minus_m_11_L_12,(ADDRESS)&Minus__Minus_var_u16_I,6496+(char*)&Minus_m_11_L_12,(ADDRESS)&Minus__uMinus_var_u16_I,6512+(char*)&Minus_m_11_L_12,(ADDRESS)&Minus__Minus_param_u16_u16,6529+(char*)&Minus_m_11_L_12,(ADDRESS)&Minus__uMinus_param_u16_u16
,6549+(char*)&Minus_m_11_L_12,(ADDRESS)&Minus__Minus_var_u16_u16,6570+(char*)&Minus_m_11_L_12,(ADDRESS)&Minus__uMinus_var_u16_u16,6588+(char*)&Minus_m_11_L_12,(ADDRESS)&Minus__Minus_param_u16_LC,6607+(char*)&Minus_m_11_L_12,(ADDRESS)&Minus__uMinus_param_u16_LC,6626+(char*)&Minus_m_11_L_12,(ADDRESS)&Minus__Minus_var_u16_LC,6646+(char*)&Minus_m_11_L_12,(ADDRESS)&Minus__uMinus_var_u16_LC,6663+(char*)&Minus_m_11_L_12,(ADDRESS)&Minus__Minus_param_u16_i32,6681+(char*)&Minus_m_11_L_12,(ADDRESS)&Minus__uMinus_param_u16_i32,6701+(char*)&Minus_m_11_L_12,(ADDRESS)&Minus__Minus_var_u16_i32,6722+(char*)&Minus_m_11_L_12,(ADDRESS)&Minus__uMinus_var_u16_i32,6740+(char*)&Minus_m_11_L_12,(ADDRESS)&Minus__Minus_param_u16_u64,6759+(char*)&Minus_m_11_L_12,(ADDRESS)&Minus__uMinus_param_u16_u64,6779+(char*)&Minus_m_11_L_12,(ADDRESS)&Minus__Minus_var_u16_u64,6800+(char*)&Minus_m_11_L_12,(ADDRESS)&Minus__uMinus_var_u16_u64,6818+(char*)&Minus_m_11_L_12,(ADDRESS)&Minus__Minus_param_u16_i8,6837+(char*)&Minus_m_11_L_12
,(ADDRESS)&Minus__uMinus_param_u16_i8,6856+(char*)&Minus_m_11_L_12,(ADDRESS)&Minus__Minus_var_u16_i8,6876+(char*)&Minus_m_11_L_12,(ADDRESS)&Minus__uMinus_var_u16_i8,6893+(char*)&Minus_m_11_L_12,(ADDRESS)&Minus__Minus_param_LC_L,6911+(char*)&Minus_m_11_L_12,(ADDRESS)&Minus__uMinus_param_LC_L,6928+(char*)&Minus_m_11_L_12,(ADDRESS)&Minus__Minus_var_LC_L,6946+(char*)&Minus_m_11_L_12,(ADDRESS)&Minus__uMinus_var_LC_L,6961+(char*)&Minus_m_11_L_12,(ADDRESS)&Minus__Minus_param_LC_u8,6977+(char*)&Minus_m_11_L_12,(ADDRESS)&Minus__uMinus_param_LC_u8,6995+(char*)&Minus_m_11_L_12,(ADDRESS)&Minus__Minus_var_LC_u8,7014+(char*)&Minus_m_11_L_12,(ADDRESS)&Minus__uMinus_var_LC_u8,7030+(char*)&Minus_m_11_L_12,(ADDRESS)&Minus__Minus_param_LC_u32,7047+(char*)&Minus_m_11_L_12,(ADDRESS)&Minus__uMinus_param_LC_u32,7066+(char*)&Minus_m_11_L_12,(ADDRESS)&Minus__Minus_var_LC_u32,7086+(char*)&Minus_m_11_L_12,(ADDRESS)&Minus__uMinus_var_LC_u32,7103+(char*)&Minus_m_11_L_12,(ADDRESS)&Minus__Minus_param_LC_C,7121+(char*)&Minus_m_11_L_12
,(ADDRESS)&Minus__uMinus_param_LC_C,7138+(char*)&Minus_m_11_L_12,(ADDRESS)&Minus__Minus_var_LC_C,7156+(char*)&Minus_m_11_L_12,(ADDRESS)&Minus__uMinus_var_LC_C,7171+(char*)&Minus_m_11_L_12,(ADDRESS)&Minus__Minus_param_LC_i16,7187+(char*)&Minus_m_11_L_12,(ADDRESS)&Minus__uMinus_param_LC_i16,7206+(char*)&Minus_m_11_L_12,(ADDRESS)&Minus__Minus_var_LC_i16,7226+(char*)&Minus_m_11_L_12,(ADDRESS)&Minus__uMinus_var_LC_i16,7243+(char*)&Minus_m_11_L_12,(ADDRESS)&Minus__Minus_param_LC_i64,7261+(char*)&Minus_m_11_L_12,(ADDRESS)&Minus__uMinus_param_LC_i64,7280+(char*)&Minus_m_11_L_12,(ADDRESS)&Minus__Minus_var_LC_i64,7300+(char*)&Minus_m_11_L_12,(ADDRESS)&Minus__uMinus_var_LC_i64,7317+(char*)&Minus_m_11_L_12,(ADDRESS)&Minus__Minus_param_LC_I,7335+(char*)&Minus_m_11_L_12,(ADDRESS)&Minus__uMinus_param_LC_I,7352+(char*)&Minus_m_11_L_12,(ADDRESS)&Minus__Minus_var_LC_I,7370+(char*)&Minus_m_11_L_12,(ADDRESS)&Minus__uMinus_var_LC_I,7385+(char*)&Minus_m_11_L_12,(ADDRESS)&Minus__Minus_param_LC_u16,7401+(char*)&Minus_m_11_L_12
,(ADDRESS)&Minus__uMinus_param_LC_u16,7420+(char*)&Minus_m_11_L_12,(ADDRESS)&Minus__Minus_var_LC_u16,7440+(char*)&Minus_m_11_L_12,(ADDRESS)&Minus__uMinus_var_LC_u16,7457+(char*)&Minus_m_11_L_12,(ADDRESS)&Minus__Minus_param_LC_LC,7475+(char*)&Minus_m_11_L_12,(ADDRESS)&Minus__uMinus_param_LC_LC,7493+(char*)&Minus_m_11_L_12,(ADDRESS)&Minus__Minus_var_LC_LC,7512+(char*)&Minus_m_11_L_12,(ADDRESS)&Minus__uMinus_var_LC_LC,7528+(char*)&Minus_m_11_L_12,(ADDRESS)&Minus__Minus_param_LC_i32,7545+(char*)&Minus_m_11_L_12,(ADDRESS)&Minus__uMinus_param_LC_i32,7564+(char*)&Minus_m_11_L_12,(ADDRESS)&Minus__Minus_var_LC_i32,7584+(char*)&Minus_m_11_L_12,(ADDRESS)&Minus__uMinus_var_LC_i32,7601+(char*)&Minus_m_11_L_12,(ADDRESS)&Minus__Minus_param_LC_u64,7619+(char*)&Minus_m_11_L_12,(ADDRESS)&Minus__uMinus_param_LC_u64,7638+(char*)&Minus_m_11_L_12,(ADDRESS)&Minus__Minus_var_LC_u64,7658+(char*)&Minus_m_11_L_12,(ADDRESS)&Minus__uMinus_var_LC_u64,7675+(char*)&Minus_m_11_L_12,(ADDRESS)&Minus__Minus_param_LC_i8,7693+(char*)&Minus_m_11_L_12
,(ADDRESS)&Minus__uMinus_param_LC_i8,7711+(char*)&Minus_m_11_L_12,(ADDRESS)&Minus__Minus_var_LC_i8,7730+(char*)&Minus_m_11_L_12,(ADDRESS)&Minus__uMinus_var_LC_i8,7746+(char*)&Minus_m_11_L_12,(ADDRESS)&Minus__Minus_param_i32_L,7763+(char*)&Minus_m_11_L_12,(ADDRESS)&Minus__uMinus_param_i32_L,7781+(char*)&Minus_m_11_L_12,(ADDRESS)&Minus__Minus_var_i32_L,7800+(char*)&Minus_m_11_L_12,(ADDRESS)&Minus__uMinus_var_i32_L,7816+(char*)&Minus_m_11_L_12,(ADDRESS)&Minus__Minus_param_i32_u8,7833+(char*)&Minus_m_11_L_12,(ADDRESS)&Minus__uMinus_param_i32_u8,7852+(char*)&Minus_m_11_L_12,(ADDRESS)&Minus__Minus_var_i32_u8,7872+(char*)&Minus_m_11_L_12,(ADDRESS)&Minus__uMinus_var_i32_u8,7889+(char*)&Minus_m_11_L_12,(ADDRESS)&Minus__Minus_param_i32_u32,7907+(char*)&Minus_m_11_L_12,(ADDRESS)&Minus__uMinus_param_i32_u32,7927+(char*)&Minus_m_11_L_12,(ADDRESS)&Minus__Minus_var_i32_u32,7948+(char*)&Minus_m_11_L_12,(ADDRESS)&Minus__uMinus_var_i32_u32,7966+(char*)&Minus_m_11_L_12,(ADDRESS)&Minus__Minus_param_i32_C,
7985+(char*)&Minus_m_11_L_12,(ADDRESS)&Minus__uMinus_param_i32_C,8003+(char*)&Minus_m_11_L_12,(ADDRESS)&Minus__Minus_var_i32_C,8022+(char*)&Minus_m_11_L_12,(ADDRESS)&Minus__uMinus_var_i32_C,8038+(char*)&Minus_m_11_L_12,(ADDRESS)&Minus__Minus_param_i32_i16,8055+(char*)&Minus_m_11_L_12,(ADDRESS)&Minus__uMinus_param_i32_i16,8075+(char*)&Minus_m_11_L_12,(ADDRESS)&Minus__Minus_var_i32_i16,8096+(char*)&Minus_m_11_L_12,(ADDRESS)&Minus__uMinus_var_i32_i16,8114+(char*)&Minus_m_11_L_12,(ADDRESS)&Minus__Minus_param_i32_i64,8133+(char*)&Minus_m_11_L_12,(ADDRESS)&Minus__uMinus_param_i32_i64,8153+(char*)&Minus_m_11_L_12,(ADDRESS)&Minus__Minus_var_i32_i64,8174+(char*)&Minus_m_11_L_12,(ADDRESS)&Minus__uMinus_var_i32_i64,8192+(char*)&Minus_m_11_L_12,(ADDRESS)&Minus__Minus_param_i32_I,8211+(char*)&Minus_m_11_L_12,(ADDRESS)&Minus__uMinus_param_i32_I,8229+(char*)&Minus_m_11_L_12,(ADDRESS)&Minus__Minus_var_i32_I,8248+(char*)&Minus_m_11_L_12,(ADDRESS)&Minus__uMinus_var_i32_I,8264+(char*)&Minus_m_11_L_12,(ADDRESS)&Minus__Minus_param_i32_u16
,8281+(char*)&Minus_m_11_L_12,(ADDRESS)&Minus__uMinus_param_i32_u16,8301+(char*)&Minus_m_11_L_12,(ADDRESS)&Minus__Minus_var_i32_u16,8322+(char*)&Minus_m_11_L_12,(ADDRESS)&Minus__uMinus_var_i32_u16,8340+(char*)&Minus_m_11_L_12,(ADDRESS)&Minus__Minus_param_i32_LC,8359+(char*)&Minus_m_11_L_12,(ADDRESS)&Minus__uMinus_param_i32_LC,8378+(char*)&Minus_m_11_L_12,(ADDRESS)&Minus__Minus_var_i32_LC,8398+(char*)&Minus_m_11_L_12,(ADDRESS)&Minus__uMinus_var_i32_LC,8415+(char*)&Minus_m_11_L_12,(ADDRESS)&Minus__Minus_param_i32_i32,8433+(char*)&Minus_m_11_L_12,(ADDRESS)&Minus__uMinus_param_i32_i32,8453+(char*)&Minus_m_11_L_12,(ADDRESS)&Minus__Minus_var_i32_i32,8474+(char*)&Minus_m_11_L_12,(ADDRESS)&Minus__uMinus_var_i32_i32,8492+(char*)&Minus_m_11_L_12,(ADDRESS)&Minus__Minus_param_i32_u64,8511+(char*)&Minus_m_11_L_12,(ADDRESS)&Minus__uMinus_param_i32_u64,8531+(char*)&Minus_m_11_L_12,(ADDRESS)&Minus__Minus_var_i32_u64,8552+(char*)&Minus_m_11_L_12,(ADDRESS)&Minus__uMinus_var_i32_u64,8570+(char*)&Minus_m_11_L_12
,(ADDRESS)&Minus__Minus_param_i32_i8,8589+(char*)&Minus_m_11_L_12,(ADDRESS)&Minus__uMinus_param_i32_i8,8608+(char*)&Minus_m_11_L_12,(ADDRESS)&Minus__Minus_var_i32_i8,8628+(char*)&Minus_m_11_L_12,(ADDRESS)&Minus__uMinus_var_i32_i8,8645+(char*)&Minus_m_11_L_12,(ADDRESS)&Minus__Minus_param_f64_f64,8663+(char*)&Minus_m_11_L_12,(ADDRESS)&Minus__Minus_var_f64_f64,8683+(char*)&Minus_m_11_L_12,(ADDRESS)&Minus__Minus_param_u64_L,8701+(char*)&Minus_m_11_L_12,(ADDRESS)&Minus__uMinus_param_u64_L,8719+(char*)&Minus_m_11_L_12,(ADDRESS)&Minus__Minus_var_u64_L,8738+(char*)&Minus_m_11_L_12,(ADDRESS)&Minus__uMinus_var_u64_L,8754+(char*)&Minus_m_11_L_12,(ADDRESS)&Minus__Minus_param_u64_u8,8771+(char*)&Minus_m_11_L_12,(ADDRESS)&Minus__uMinus_param_u64_u8,8790+(char*)&Minus_m_11_L_12,(ADDRESS)&Minus__Minus_var_u64_u8,8810+(char*)&Minus_m_11_L_12,(ADDRESS)&Minus__uMinus_var_u64_u8,8827+(char*)&Minus_m_11_L_12,(ADDRESS)&Minus__Minus_param_u64_u32,8845+(char*)&Minus_m_11_L_12,(ADDRESS)&Minus__uMinus_param_u64_u32
,8865+(char*)&Minus_m_11_L_12,(ADDRESS)&Minus__Minus_var_u64_u32,8886+(char*)&Minus_m_11_L_12,(ADDRESS)&Minus__uMinus_var_u64_u32,8904+(char*)&Minus_m_11_L_12,(ADDRESS)&Minus__Minus_param_u64_C,8923+(char*)&Minus_m_11_L_12,(ADDRESS)&Minus__uMinus_param_u64_C,8941+(char*)&Minus_m_11_L_12,(ADDRESS)&Minus__Minus_var_u64_C,8960+(char*)&Minus_m_11_L_12,(ADDRESS)&Minus__uMinus_var_u64_C,8976+(char*)&Minus_m_11_L_12,(ADDRESS)&Minus__Minus_param_u64_i16,8993+(char*)&Minus_m_11_L_12,(ADDRESS)&Minus__uMinus_param_u64_i16,9013+(char*)&Minus_m_11_L_12,(ADDRESS)&Minus__Minus_var_u64_i16,9034+(char*)&Minus_m_11_L_12,(ADDRESS)&Minus__uMinus_var_u64_i16,9052+(char*)&Minus_m_11_L_12,(ADDRESS)&Minus__Minus_param_u64_i64,9071+(char*)&Minus_m_11_L_12,(ADDRESS)&Minus__uMinus_param_u64_i64,9091+(char*)&Minus_m_11_L_12,(ADDRESS)&Minus__Minus_var_u64_i64,9112+(char*)&Minus_m_11_L_12,(ADDRESS)&Minus__uMinus_var_u64_i64,9130+(char*)&Minus_m_11_L_12,(ADDRESS)&Minus__Minus_param_u64_I,9149+(char*)&Minus_m_11_L_12
,(ADDRESS)&Minus__uMinus_param_u64_I,9167+(char*)&Minus_m_11_L_12,(ADDRESS)&Minus__Minus_var_u64_I,9186+(char*)&Minus_m_11_L_12,(ADDRESS)&Minus__uMinus_var_u64_I,9202+(char*)&Minus_m_11_L_12,(ADDRESS)&Minus__Minus_param_u64_u16,9219+(char*)&Minus_m_11_L_12,(ADDRESS)&Minus__uMinus_param_u64_u16,9239+(char*)&Minus_m_11_L_12,(ADDRESS)&Minus__Minus_var_u64_u16,9260+(char*)&Minus_m_11_L_12,(ADDRESS)&Minus__uMinus_var_u64_u16,9278+(char*)&Minus_m_11_L_12,(ADDRESS)&Minus__Minus_param_u64_LC,9297+(char*)&Minus_m_11_L_12,(ADDRESS)&Minus__uMinus_param_u64_LC,9316+(char*)&Minus_m_11_L_12,(ADDRESS)&Minus__Minus_var_u64_LC,9336+(char*)&Minus_m_11_L_12,(ADDRESS)&Minus__uMinus_var_u64_LC,9353+(char*)&Minus_m_11_L_12,(ADDRESS)&Minus__Minus_param_u64_i32,9371+(char*)&Minus_m_11_L_12,(ADDRESS)&Minus__uMinus_param_u64_i32,9391+(char*)&Minus_m_11_L_12,(ADDRESS)&Minus__Minus_var_u64_i32,9412+(char*)&Minus_m_11_L_12,(ADDRESS)&Minus__uMinus_var_u64_i32,9430+(char*)&Minus_m_11_L_12,(ADDRESS)&Minus__Minus_param_u64_u64
,9449+(char*)&Minus_m_11_L_12,(ADDRESS)&Minus__uMinus_param_u64_u64,9469+(char*)&Minus_m_11_L_12,(ADDRESS)&Minus__Minus_var_u64_u64,9490+(char*)&Minus_m_11_L_12,(ADDRESS)&Minus__uMinus_var_u64_u64,9508+(char*)&Minus_m_11_L_12,(ADDRESS)&Minus__Minus_param_u64_i8,9527+(char*)&Minus_m_11_L_12,(ADDRESS)&Minus__uMinus_param_u64_i8,9546+(char*)&Minus_m_11_L_12,(ADDRESS)&Minus__Minus_var_u64_i8,9566+(char*)&Minus_m_11_L_12,(ADDRESS)&Minus__uMinus_var_u64_i8,9583+(char*)&Minus_m_11_L_12,(ADDRESS)&Minus__Minus_param_i8_L,9601+(char*)&Minus_m_11_L_12,(ADDRESS)&Minus__uMinus_param_i8_L,9618+(char*)&Minus_m_11_L_12,(ADDRESS)&Minus__Minus_var_i8_L,9636+(char*)&Minus_m_11_L_12,(ADDRESS)&Minus__uMinus_var_i8_L,9651+(char*)&Minus_m_11_L_12,(ADDRESS)&Minus__Minus_param_i8_u8,9667+(char*)&Minus_m_11_L_12,(ADDRESS)&Minus__uMinus_param_i8_u8,9685+(char*)&Minus_m_11_L_12,(ADDRESS)&Minus__Minus_var_i8_u8,9704+(char*)&Minus_m_11_L_12,(ADDRESS)&Minus__uMinus_var_i8_u8,9720+(char*)&Minus_m_11_L_12,(ADDRESS)&Minus__Minus_param_i8_u32
,9737+(char*)&Minus_m_11_L_12,(ADDRESS)&Minus__uMinus_param_i8_u32,9756+(char*)&Minus_m_11_L_12,(ADDRESS)&Minus__Minus_var_i8_u32,9776+(char*)&Minus_m_11_L_12,(ADDRESS)&Minus__uMinus_var_i8_u32,9793+(char*)&Minus_m_11_L_12,(ADDRESS)&Minus__Minus_param_i8_C,9811+(char*)&Minus_m_11_L_12,(ADDRESS)&Minus__uMinus_param_i8_C,9828+(char*)&Minus_m_11_L_12,(ADDRESS)&Minus__Minus_var_i8_C,9846+(char*)&Minus_m_11_L_12,(ADDRESS)&Minus__uMinus_var_i8_C,9861+(char*)&Minus_m_11_L_12,(ADDRESS)&Minus__Minus_param_i8_i16,9877+(char*)&Minus_m_11_L_12,(ADDRESS)&Minus__uMinus_param_i8_i16,9896+(char*)&Minus_m_11_L_12,(ADDRESS)&Minus__Minus_var_i8_i16,9916+(char*)&Minus_m_11_L_12,(ADDRESS)&Minus__uMinus_var_i8_i16,9933+(char*)&Minus_m_11_L_12,(ADDRESS)&Minus__Minus_param_i8_i64,9951+(char*)&Minus_m_11_L_12,(ADDRESS)&Minus__uMinus_param_i8_i64,9970+(char*)&Minus_m_11_L_12,(ADDRESS)&Minus__Minus_var_i8_i64,9990+(char*)&Minus_m_11_L_12,(ADDRESS)&Minus__uMinus_var_i8_i64,10007+(char*)&Minus_m_11_L_12,(ADDRESS)&Minus__Minus_param_i8_I
,10025+(char*)&Minus_m_11_L_12,(ADDRESS)&Minus__uMinus_param_i8_I,10042+(char*)&Minus_m_11_L_12,(ADDRESS)&Minus__Minus_var_i8_I,10060+(char*)&Minus_m_11_L_12,(ADDRESS)&Minus__uMinus_var_i8_I,10075+(char*)&Minus_m_11_L_12,(ADDRESS)&Minus__Minus_param_i8_u16,10091+(char*)&Minus_m_11_L_12,(ADDRESS)&Minus__uMinus_param_i8_u16,10110+(char*)&Minus_m_11_L_12,(ADDRESS)&Minus__Minus_var_i8_u16,10130+(char*)&Minus_m_11_L_12,(ADDRESS)&Minus__uMinus_var_i8_u16,10147+(char*)&Minus_m_11_L_12,(ADDRESS)&Minus__Minus_param_i8_LC,10165+(char*)&Minus_m_11_L_12,(ADDRESS)&Minus__uMinus_param_i8_LC,10183+(char*)&Minus_m_11_L_12,(ADDRESS)&Minus__Minus_var_i8_LC,10202+(char*)&Minus_m_11_L_12,(ADDRESS)&Minus__uMinus_var_i8_LC,10218+(char*)&Minus_m_11_L_12,(ADDRESS)&Minus__Minus_param_i8_i32,10235+(char*)&Minus_m_11_L_12,(ADDRESS)&Minus__uMinus_param_i8_i32,10254+(char*)&Minus_m_11_L_12,(ADDRESS)&Minus__Minus_var_i8_i32,10274+(char*)&Minus_m_11_L_12,(ADDRESS)&Minus__uMinus_var_i8_i32,10291+(char*)&Minus_m_11_L_12
,(ADDRESS)&Minus__Minus_param_i8_u64,10309+(char*)&Minus_m_11_L_12,(ADDRESS)&Minus__uMinus_param_i8_u64,10328+(char*)&Minus_m_11_L_12,(ADDRESS)&Minus__Minus_var_i8_u64,10348+(char*)&Minus_m_11_L_12,(ADDRESS)&Minus__uMinus_var_i8_u64,10365+(char*)&Minus_m_11_L_12,(ADDRESS)&Minus__Minus_param_i8_i8,10383+(char*)&Minus_m_11_L_12,(ADDRESS)&Minus__uMinus_param_i8_i8,10401+(char*)&Minus_m_11_L_12,(ADDRESS)&Minus__Minus_var_i8_i8,10420+(char*)&Minus_m_11_L_12,(ADDRESS)&Minus__uMinus_var_i8_i8,10436+(char*)&Minus_m_11_L_12},{0 /* 1 */ ,0 /* 2 */ ,0 /* 3 */ ,0 /* 4 */ ,0 /* 5 */ ,0 /* 6 */ ,0 /* 7 */ ,0 /* 8 */ ,},{'M','i','n','u','s','.','m','3'},{0 /* 1 */ ,0 /* 2 */ ,0 /* 3 */ ,0 /* 4 */ ,0 /* 5 */ ,0 /* 6 */ ,0 /* 7 */ ,0 /* 8 */ ,}};
 /* bind_segment */
 /* begin_init */
 /* init_var */
 /* init_var */
 /* init_var */
 /* init_proc */
 /* init_int */
 /* init_int */
 /* init_int */
 /* init_float */
 /* init_int */
 /* init_int */
 /* init_int */
 /* init_int */
 /* init_int */
 /* init_float */
 /* init_int */
 /* init_int */
 /* init_int */
 /* init_int */
 /* init_int */
 /* init_proc */
 /* init_var */
 /* init_proc */
 /* init_var */
 /* init_proc */
 /* init_var */
 /* init_proc */
 /* init_var */
 /* init_proc */
 /* end_init */
struct Minus_m_M_Minus_L_13_t{ADDRESS L_2566[1];
char L_2567[32];
ADDRESS L_2568[1];
char L_2569[24];
ADDRESS L_2570[1];
char L_2571[8];
ADDRESS L_2572[1];
INT64 L_2573[1];
UINT8 L_2574[1];
char L_2575[7];
INT64 L_2576[1];
double L_2577[1];
INT32 L_2578[1];
char L_2579[4];
INT64 L_2580[1];
INT16 L_2581[1];
char L_2582[6];
INT64 L_2583[2];
float L_2584[1];
INT16 L_2585[1];
char L_2586[2];
INT64 L_2587[1];
INT32 L_2588[1];
UINT8 L_2589[1];
char L_2590[3];
INT64 L_2591[1];
char L_2592[24];
ADDRESS L_2593[2];
char L_2594[8];
ADDRESS L_2595[2];
char L_2596[8];
ADDRESS L_2597[2];
char L_2598[8];
ADDRESS L_2599[2];
char L_2600[8];
ADDRESS L_2601[1];
char L_2602[8];
};
static Minus_m_M_Minus_L_13_t Minus_m_M_Minus_L_13={{19760+(char*)&Minus_m_11_L_12},{0 /* 1 */ ,0 /* 2 */ ,0 /* 3 */ ,0 /* 4 */ ,0 /* 5 */ ,0 /* 6 */ ,0 /* 7 */ ,0 /* 8 */ ,0 /* 9 */ ,0 /* 10 */ ,0 /* 11 */ ,0 /* 12 */ ,0 /* 13 */ ,0 /* 14 */ ,0 /* 15 */ ,0 /* 16 */ ,0 /* 17 */ ,0 /* 18 */ ,0 /* 19 */ ,0 /* 20 */ ,0 /* 21 */ ,0 /* 22 */ ,0 /* 23 */ ,0 /* 24 */ ,0 /* 25 */ ,0 /* 26 */ ,0 /* 27 */ ,0 /* 28 */ ,0 /* 29 */ ,0 /* 30 */ ,0 /* 31 */ ,0 /* 32 */ ,},{10456+(char*)&Minus_m_11_L_12},{0 /* 1 */ ,0 /* 2 */ ,0 /* 3 */ ,0 /* 4 */ ,0 /* 5 */ ,0 /* 6 */ ,0 /* 7 */ ,0 /* 8 */ ,0 /* 9 */ ,0 /* 10 */ ,0 /* 11 */ ,0 /* 12 */ ,0 /* 13 */ ,0 /* 14 */ ,0 /* 15 */ ,0 /* 16 */ ,0 /* 17 */ ,0 /* 18 */ ,0 /* 19 */ ,0 /* 20 */ ,0 /* 21 */ ,0 /* 22 */ ,0 /* 23 */ ,0 /* 24 */ ,},{216+(char*)&Minus_m_M_Minus_L_13},{0 /* 1 */ ,0 /* 2 */ ,0 /* 3 */ ,0 /* 4 */ ,0 /* 5 */ ,0 /* 6 */ ,0 /* 7 */ ,0 /* 8 */ ,},{(ADDRESS)&Minus_M3},{INT64_(3)},{209U},{0 /* 1 */ ,0 /* 2 */ ,0 /* 3 */ ,0 /* 4 */ ,0 /* 5 */ ,0 /* 6 */ 
,0 /* 7 */ ,},{INT64_(210)},{2.11211999999999989e2},{213},{0 /* 1 */ ,0 /* 2 */ ,0 /* 3 */ ,0 /* 4 */ ,},{INT64_(214)},{((INT16)215)},{0 /* 1 */ ,0 /* 2 */ ,0 /* 3 */ ,0 /* 4 */ ,0 /* 5 */ ,0 /* 6 */ ,},{INT64_(216),INT64_(217)},{2.1821899414062E2F},{((INT16)220)},{0 /* 1 */ ,0 /* 2 */ ,},{INT64_(221)},{222},{223U},{0 /* 1 */ ,0 /* 2 */ ,0 /* 3 */ ,},{INT64_(224)},{0 /* 1 */ ,0 /* 2 */ ,0 /* 3 */ ,0 /* 4 */ ,0 /* 5 */ ,0 /* 6 */ ,0 /* 7 */ ,0 /* 8 */ ,0 /* 9 */ ,0 /* 10 */ ,0 /* 11 */ ,0 /* 12 */ ,0 /* 13 */ ,0 /* 14 */ ,0 /* 15 */ ,0 /* 16 */ ,0 /* 17 */ ,0 /* 18 */ ,0 /* 19 */ ,0 /* 20 */ ,0 /* 21 */ ,0 /* 22 */ ,0 /* 23 */ ,0 /* 24 */ ,},{(ADDRESS)&Minus_I3,240+(char*)&Minus_m_M_Minus_L_13},{0 /* 1 */ ,0 /* 2 */ ,0 /* 3 */ ,0 /* 4 */ ,0 /* 5 */ ,0 /* 6 */ ,0 /* 7 */ ,0 /* 8 */ ,},{(ADDRESS)&Long_I3,264+(char*)&Minus_m_M_Minus_L_13},{0 /* 1 */ ,0 /* 2 */ ,0 /* 3 */ ,0 /* 4 */ ,0 /* 5 */ ,0 /* 6 */ ,0 /* 7 */ ,0 /* 8 */ ,},{(ADDRESS)&Word_I3,288+(char*)&Minus_m_M_Minus_L_13},{0 /* 1 */ 
,0 /* 2 */ ,0 /* 3 */ ,0 /* 4 */ ,0 /* 5 */ ,0 /* 6 */ ,0 /* 7 */ ,0 /* 8 */ ,},{(ADDRESS)&Cstdint_I3,312+(char*)&Minus_m_M_Minus_L_13},{0 /* 1 */ ,0 /* 2 */ ,0 /* 3 */ ,0 /* 4 */ ,0 /* 5 */ ,0 /* 6 */ ,0 /* 7 */ ,0 /* 8 */ ,},{(ADDRESS)&RTHooks_I3},{0 /* 1 */ ,0 /* 2 */ ,0 /* 3 */ ,0 /* 4 */ ,0 /* 5 */ ,0 /* 6 */ ,0 /* 7 */ ,0 /* 8 */ ,}};
static void __cdecl Minus_m_M_Minus_L_13_CRASH(WORD_T code) M3_ATTRIBUTE_NO_RETURN;
static void __cdecl Minus_m_M_Minus_L_13_CRASH(WORD_T code){RTHooks__ReportFault((ADDRESS)&Minus_m_M_Minus_L_13,code);} /* end: segments/globals */
 /* begin: mark used */
 /* end: mark used */
 /* set_source_file */
 /* set_source_line */
#line 1 "Minus.m3"
 /* module global constants */
#line 1 "Minus.m3"
 /* module global data */
#line 1 "Minus.m3"
 /* set_source_line */
#line 1 "Minus.m3"
#line 634 "Minus.m3"
 /* uMinus_var_i8_i8 */
#line 634 "Minus.m3"
 /* set_source_line */
#line 634 "Minus.m3"
#line 54 "Minus.m3"
 /* begin_procedure */
#line 54 "Minus.m3"
struct Minus__uMinus_var_i8_i8_Frame_t {
#line 54 "Minus.m3"
ADDRESS _unused;
#line 54 "Minus.m3"
};
#line 54 "Minus.m3"
Word__T
__cdecl
Minus__uMinus_var_i8_i8(void)
{
#line 54 "Minus.m3"
Minus__uMinus_var_i8_i8_Frame_t _frame;
#line 54 "Minus.m3"
_frame._unused=(ADDRESS)&_frame;
#line 54 "Minus.m3"
 /* load */
#line 54 "Minus.m3"
 /* load */
#line 54 "Minus.m3"
 /* subtract */
#line 54 "Minus.m3"
 /* exit_proc */
#line 54 "Minus.m3"
return ((UINT64)(((UINT64)(((INT64)(*((INT8*)(INT64_(104)+((ADDRESS)(&Minus_m_M_Minus_L_13))))))))-((UINT64)(((INT64)(*((INT8*)(INT64_(104)+((ADDRESS)(&Minus_m_M_Minus_L_13))))))))));
#line 54 "Minus.m3"
 /* end_procedure */
#line 54 "Minus.m3"
} /* Minus_var_i8_i8 */
#line 54 "Minus.m3"
 /* set_source_line */
#line 54 "Minus.m3"
#line 55 "Minus.m3"
 /* begin_procedure */
#line 55 "Minus.m3"
struct Minus__Minus_var_i8_i8_Frame_t {
#line 55 "Minus.m3"
ADDRESS _unused;
#line 55 "Minus.m3"
};
#line 55 "Minus.m3"
Minus__INT8
__cdecl
Minus__Minus_var_i8_i8(void)
{
#line 55 "Minus.m3"
 /* Var_Type2 */ INT64 Minus_m_1176_L_1177={0};//always-init
#line 55 "Minus.m3"
Minus__Minus_var_i8_i8_Frame_t _frame;
#line 55 "Minus.m3"
_frame._unused=(ADDRESS)&_frame;
#line 55 "Minus.m3"
 /* load */
#line 55 "Minus.m3"
 /* load */
#line 55 "Minus.m3"
 /* subtract */
#line 55 "Minus.m3"
 /* check_range */
#line 55 "Minus.m3"
 /* store */
#line 55 "Minus.m3"
(*(INT64*)(&Minus_m_1176_L_1177))=(INT64)( ((INT64)( ((INT64)(*((INT8*)(INT64_(104)+((ADDRESS)(&Minus_m_M_Minus_L_13))))))- ((INT64)(*((INT8*)(INT64_(104)+((ADDRESS)(&Minus_m_M_Minus_L_13)))))))));
#line 55 "Minus.m3"
 /* load */
#line 55 "Minus.m3"
if(m3_check_range(INT64,
Minus_m_1176_L_1177,
 INT64_(-128),
 INT64_(127)))
#line 55 "Minus.m3"
Minus_m_M_Minus_L_13_CRASH(1761);
#line 55 "Minus.m3"
 /* exit_proc */
#line 55 "Minus.m3"
return Minus_m_1176_L_1177;
#line 55 "Minus.m3"
 /* end_procedure */
#line 55 "Minus.m3"
} /* uMinus_param_i8_i8 */
#line 55 "Minus.m3"
 /* set_source_line */
#line 55 "Minus.m3"
#line 56 "Minus.m3"
 /* begin_procedure */
#line 56 "Minus.m3"
struct Minus__uMinus_param_i8_i8_Frame_t {
#line 56 "Minus.m3"
ADDRESS _unused;
#line 56 "Minus.m3"
};
#line 56 "Minus.m3"
Word__T
__cdecl
Minus__uMinus_param_i8_i8(
   /* Param_Type1 */ Minus__INT8 a_L_18,
   /* Param_Type1 */ Minus__INT8 b_L_19)
{
#line 56 "Minus.m3"
Minus__uMinus_param_i8_i8_Frame_t _frame;
#line 56 "Minus.m3"
_frame._unused=(ADDRESS)&_frame;
#line 56 "Minus.m3"
 /* load */
#line 56 "Minus.m3"
 /* load */
#line 56 "Minus.m3"
 /* subtract */
#line 56 "Minus.m3"
 /* exit_proc */
#line 56 "Minus.m3"
return ((UINT64)(((UINT64)(((INT64)(a_L_18))))-((UINT64)(((INT64)(b_L_19))))));
#line 56 "Minus.m3"
 /* end_procedure */
#line 56 "Minus.m3"
} /* Minus_param_i8_i8 */
#line 56 "Minus.m3"
 /* set_source_line */
#line 56 "Minus.m3"
#line 57 "Minus.m3"
 /* begin_procedure */
#line 57 "Minus.m3"
struct Minus__Minus_param_i8_i8_Frame_t {
#line 57 "Minus.m3"
ADDRESS _unused;
#line 57 "Minus.m3"
};
#line 57 "Minus.m3"
Minus__INT8
__cdecl
Minus__Minus_param_i8_i8(
   /* Param_Type1 */ Minus__INT8 a_L_21,
   /* Param_Type1 */ Minus__INT8 b_L_22)
{
#line 57 "Minus.m3"
 /* Var_Type2 */ INT64 Minus_m_1178_L_1179={0};//always-init
#line 57 "Minus.m3"
Minus__Minus_param_i8_i8_Frame_t _frame;
#line 57 "Minus.m3"
_frame._unused=(ADDRESS)&_frame;
#line 57 "Minus.m3"
 /* load */
#line 57 "Minus.m3"
 /* load */
#line 57 "Minus.m3"
 /* subtract */
#line 57 "Minus.m3"
 /* check_range */
#line 57 "Minus.m3"
 /* store */
#line 57 "Minus.m3"
(*(INT64*)(&Minus_m_1178_L_1179))=(INT64)( ((INT64)( ((INT64)(a_L_21))- ((INT64)(b_L_22)))));
#line 57 "Minus.m3"
 /* load */
#line 57 "Minus.m3"
if(m3_check_range(INT64,
Minus_m_1178_L_1179,
 INT64_(-128),
 INT64_(127)))
#line 57 "Minus.m3"
Minus_m_M_Minus_L_13_CRASH(1825);
#line 57 "Minus.m3"
 /* exit_proc */
#line 57 "Minus.m3"
return Minus_m_1178_L_1179;
#line 57 "Minus.m3"
 /* end_procedure */
#line 57 "Minus.m3"
} /* uMinus_var_i8_u64 */
#line 57 "Minus.m3"
 /* set_source_line */
#line 57 "Minus.m3"
#line 58 "Minus.m3"
 /* begin_procedure */
#line 58 "Minus.m3"
struct Minus__uMinus_var_i8_u64_Frame_t {
#line 58 "Minus.m3"
ADDRESS _unused;
#line 58 "Minus.m3"
};
#line 58 "Minus.m3"
Long__T
__cdecl
Minus__uMinus_var_i8_u64(void)
{
#line 58 "Minus.m3"
Minus__uMinus_var_i8_u64_Frame_t _frame;
#line 58 "Minus.m3"
_frame._unused=(ADDRESS)&_frame;
#line 58 "Minus.m3"
 /* load */
#line 58 "Minus.m3"
 /* loophole */
#line 58 "Minus.m3"
 /* load */
#line 58 "Minus.m3"
 /* subtract */
#line 58 "Minus.m3"
 /* exit_proc */
#line 58 "Minus.m3"
return ((UINT64)(((UINT64)((INT64)((INT64)(*((INT8*)(INT64_(104)+((ADDRESS)(&Minus_m_M_Minus_L_13))))))))-((UINT64)(*((INT64*)(INT64_(112)+((ADDRESS)(&Minus_m_M_Minus_L_13))))))));
#line 58 "Minus.m3"
 /* end_procedure */
#line 58 "Minus.m3"
} /* Minus_var_i8_u64 */
#line 58 "Minus.m3"
 /* set_source_line */
#line 58 "Minus.m3"
#line 59 "Minus.m3"
 /* begin_procedure */
#line 59 "Minus.m3"
struct Minus__Minus_var_i8_u64_Frame_t {
#line 59 "Minus.m3"
ADDRESS _unused;
#line 59 "Minus.m3"
};
#line 59 "Minus.m3"
LONGINT
__cdecl
Minus__Minus_var_i8_u64(void)
{
#line 59 "Minus.m3"
Minus__Minus_var_i8_u64_Frame_t _frame;
#line 59 "Minus.m3"
_frame._unused=(ADDRESS)&_frame;
#line 59 "Minus.m3"
 /* load */
#line 59 "Minus.m3"
 /* loophole */
#line 59 "Minus.m3"
 /* load */
#line 59 "Minus.m3"
 /* subtract */
#line 59 "Minus.m3"
 /* exit_proc */
#line 59 "Minus.m3"
return ((INT64)(((INT64)((INT64)((INT64)(*((INT8*)(INT64_(104)+((ADDRESS)(&Minus_m_M_Minus_L_13))))))))-((INT64)(*((INT64*)(INT64_(112)+((ADDRESS)(&Minus_m_M_Minus_L_13))))))));
#line 59 "Minus.m3"
 /* end_procedure */
#line 59 "Minus.m3"
} /* uMinus_param_i8_u64 */
#line 59 "Minus.m3"
 /* set_source_line */
#line 59 "Minus.m3"
#line 60 "Minus.m3"
 /* begin_procedure */
#line 60 "Minus.m3"
struct Minus__uMinus_param_i8_u64_Frame_t {
#line 60 "Minus.m3"
ADDRESS _unused;
#line 60 "Minus.m3"
};
#line 60 "Minus.m3"
Long__T
__cdecl
Minus__uMinus_param_i8_u64(
   /* Param_Type1 */ Minus__INT8 a_L_26,
   /* Param_Type1 */ Minus__UINT64 b_L_27)
{
#line 60 "Minus.m3"
Minus__uMinus_param_i8_u64_Frame_t _frame;
#line 60 "Minus.m3"
_frame._unused=(ADDRESS)&_frame;
#line 60 "Minus.m3"
 /* load */
#line 60 "Minus.m3"
 /* loophole */
#line 60 "Minus.m3"
 /* load */
#line 60 "Minus.m3"
 /* subtract */
#line 60 "Minus.m3"
 /* exit_proc */
#line 60 "Minus.m3"
return ((UINT64)(((UINT64)((INT64)((INT64)(a_L_26))))-((UINT64)(b_L_27))));
#line 60 "Minus.m3"
 /* end_procedure */
#line 60 "Minus.m3"
} /* Minus_param_i8_u64 */
#line 60 "Minus.m3"
 /* set_source_line */
#line 60 "Minus.m3"
#line 61 "Minus.m3"
 /* begin_procedure */
#line 61 "Minus.m3"
struct Minus__Minus_param_i8_u64_Frame_t {
#line 61 "Minus.m3"
ADDRESS _unused;
#line 61 "Minus.m3"
};
#line 61 "Minus.m3"
LONGINT
__cdecl
Minus__Minus_param_i8_u64(
   /* Param_Type1 */ Minus__INT8 a_L_29,
   /* Param_Type1 */ Minus__UINT64 b_L_30)
{
#line 61 "Minus.m3"
Minus__Minus_param_i8_u64_Frame_t _frame;
#line 61 "Minus.m3"
_frame._unused=(ADDRESS)&_frame;
#line 61 "Minus.m3"
 /* load */
#line 61 "Minus.m3"
 /* loophole */
#line 61 "Minus.m3"
 /* load */
#line 61 "Minus.m3"
 /* subtract */
#line 61 "Minus.m3"
 /* exit_proc */
#line 61 "Minus.m3"
return ((INT64)(((INT64)((INT64)((INT64)(a_L_29))))- b_L_30));
#line 61 "Minus.m3"
 /* end_procedure */
#line 61 "Minus.m3"
} /* uMinus_var_i8_i32 */
#line 61 "Minus.m3"
 /* set_source_line */
#line 61 "Minus.m3"
#line 62 "Minus.m3"
 /* begin_procedure */
#line 62 "Minus.m3"
struct Minus__uMinus_var_i8_i32_Frame_t {
#line 62 "Minus.m3"
ADDRESS _unused;
#line 62 "Minus.m3"
};
#line 62 "Minus.m3"
Word__T
__cdecl
Minus__uMinus_var_i8_i32(void)
{
#line 62 "Minus.m3"
Minus__uMinus_var_i8_i32_Frame_t _frame;
#line 62 "Minus.m3"
_frame._unused=(ADDRESS)&_frame;
#line 62 "Minus.m3"
 /* load */
#line 62 "Minus.m3"
 /* load */
#line 62 "Minus.m3"
 /* subtract */
#line 62 "Minus.m3"
 /* exit_proc */
#line 62 "Minus.m3"
return ((UINT64)(((UINT64)(((INT64)(*((INT8*)(INT64_(104)+((ADDRESS)(&Minus_m_M_Minus_L_13))))))))-((UINT64)(((INT64)(*((INT32*)(INT64_(128)+((ADDRESS)(&Minus_m_M_Minus_L_13))))))))));
#line 62 "Minus.m3"
 /* end_procedure */
#line 62 "Minus.m3"
} /* Minus_var_i8_i32 */
#line 62 "Minus.m3"
 /* set_source_line */
#line 62 "Minus.m3"
#line 63 "Minus.m3"
 /* begin_procedure */
#line 63 "Minus.m3"
struct Minus__Minus_var_i8_i32_Frame_t {
#line 63 "Minus.m3"
ADDRESS _unused;
#line 63 "Minus.m3"
};
#line 63 "Minus.m3"
Minus__INT8
__cdecl
Minus__Minus_var_i8_i32(void)
{
#line 63 "Minus.m3"
 /* Var_Type2 */ INT64 Minus_m_1180_L_1181={0};//always-init
#line 63 "Minus.m3"
Minus__Minus_var_i8_i32_Frame_t _frame;
#line 63 "Minus.m3"
_frame._unused=(ADDRESS)&_frame;
#line 63 "Minus.m3"
 /* load */
#line 63 "Minus.m3"
 /* load */
#line 63 "Minus.m3"
 /* subtract */
#line 63 "Minus.m3"
 /* check_range */
#line 63 "Minus.m3"
 /* store */
#line 63 "Minus.m3"
(*(INT64*)(&Minus_m_1180_L_1181))=(INT64)( ((INT64)( ((INT64)(*((INT8*)(INT64_(104)+((ADDRESS)(&Minus_m_M_Minus_L_13))))))- ((INT64)(*((INT32*)(INT64_(128)+((ADDRESS)(&Minus_m_M_Minus_L_13)))))))));
#line 63 "Minus.m3"
 /* load */
#line 63 "Minus.m3"
if(m3_check_range(INT64,
Minus_m_1180_L_1181,
 INT64_(-128),
 INT64_(127)))
#line 63 "Minus.m3"
Minus_m_M_Minus_L_13_CRASH(2017);
#line 63 "Minus.m3"
 /* exit_proc */
#line 63 "Minus.m3"
return Minus_m_1180_L_1181;
#line 63 "Minus.m3"
 /* end_procedure */
#line 63 "Minus.m3"
} /* uMinus_param_i8_i32 */
#line 63 "Minus.m3"
 /* set_source_line */
#line 63 "Minus.m3"
#line 64 "Minus.m3"
 /* begin_procedure */
#line 64 "Minus.m3"
struct Minus__uMinus_param_i8_i32_Frame_t {
#line 64 "Minus.m3"
ADDRESS _unused;
#line 64 "Minus.m3"
};
#line 64 "Minus.m3"
Word__T
__cdecl
Minus__uMinus_param_i8_i32(
   /* Param_Type1 */ Minus__INT8 a_L_34,
   /* Param_Type1 */ Minus__INT32 b_L_35)
{
#line 64 "Minus.m3"
Minus__uMinus_param_i8_i32_Frame_t _frame;
#line 64 "Minus.m3"
_frame._unused=(ADDRESS)&_frame;
#line 64 "Minus.m3"
 /* load */
#line 64 "Minus.m3"
 /* load */
#line 64 "Minus.m3"
 /* subtract */
#line 64 "Minus.m3"
 /* exit_proc */
#line 64 "Minus.m3"
return ((UINT64)(((UINT64)(((INT64)(a_L_34))))-((UINT64)(((INT64)(b_L_35))))));
#line 64 "Minus.m3"
 /* end_procedure */
#line 64 "Minus.m3"
} /* Minus_param_i8_i32 */
#line 64 "Minus.m3"
 /* set_source_line */
#line 64 "Minus.m3"
#line 65 "Minus.m3"
 /* begin_procedure */
#line 65 "Minus.m3"
struct Minus__Minus_param_i8_i32_Frame_t {
#line 65 "Minus.m3"
ADDRESS _unused;
#line 65 "Minus.m3"
};
#line 65 "Minus.m3"
Minus__INT8
__cdecl
Minus__Minus_param_i8_i32(
   /* Param_Type1 */ Minus__INT8 a_L_37,
   /* Param_Type1 */ Minus__INT32 b_L_38)
{
#line 65 "Minus.m3"
 /* Var_Type2 */ INT64 Minus_m_1182_L_1183={0};//always-init
#line 65 "Minus.m3"
Minus__Minus_param_i8_i32_Frame_t _frame;
#line 65 "Minus.m3"
_frame._unused=(ADDRESS)&_frame;
#line 65 "Minus.m3"
 /* load */
#line 65 "Minus.m3"
 /* load */
#line 65 "Minus.m3"
 /* subtract */
#line 65 "Minus.m3"
 /* check_range */
#line 65 "Minus.m3"
 /* store */
#line 65 "Minus.m3"
(*(INT64*)(&Minus_m_1182_L_1183))=(INT64)( ((INT64)( ((INT64)(a_L_37))- ((INT64)(b_L_38)))));
#line 65 "Minus.m3"
 /* load */
#line 65 "Minus.m3"
if(m3_check_range(INT64,
Minus_m_1182_L_1183,
 INT64_(-128),
 INT64_(127)))
#line 65 "Minus.m3"
Minus_m_M_Minus_L_13_CRASH(2081);
#line 65 "Minus.m3"
 /* exit_proc */
#line 65 "Minus.m3"
return Minus_m_1182_L_1183;
#line 65 "Minus.m3"
 /* end_procedure */
#line 65 "Minus.m3"
} /* uMinus_var_i8_LC */
#line 65 "Minus.m3"
 /* set_source_line */
#line 65 "Minus.m3"
#line 66 "Minus.m3"
 /* begin_procedure */
#line 66 "Minus.m3"
struct Minus__uMinus_var_i8_LC_Frame_t {
#line 66 "Minus.m3"
ADDRESS _unused;
#line 66 "Minus.m3"
};
#line 66 "Minus.m3"
Long__T
__cdecl
Minus__uMinus_var_i8_LC(void)
{
#line 66 "Minus.m3"
Minus__uMinus_var_i8_LC_Frame_t _frame;
#line 66 "Minus.m3"
_frame._unused=(ADDRESS)&_frame;
#line 66 "Minus.m3"
 /* load */
#line 66 "Minus.m3"
 /* loophole */
#line 66 "Minus.m3"
 /* load */
#line 66 "Minus.m3"
 /* subtract */
#line 66 "Minus.m3"
 /* exit_proc */
#line 66 "Minus.m3"
return ((UINT64)(((UINT64)((INT64)((INT64)(*((INT8*)(INT64_(104)+((ADDRESS)(&Minus_m_M_Minus_L_13))))))))-((UINT64)(((INT64)(*((UINT64*)(INT64_(136)+((ADDRESS)(&Minus_m_M_Minus_L_13))))))))));
#line 66 "Minus.m3"
 /* end_procedure */
#line 66 "Minus.m3"
} /* Minus_var_i8_LC */
#line 66 "Minus.m3"
 /* set_source_line */
#line 66 "Minus.m3"
#line 67 "Minus.m3"
 /* begin_procedure */
#line 67 "Minus.m3"
struct Minus__Minus_var_i8_LC_Frame_t {
#line 67 "Minus.m3"
ADDRESS _unused;
#line 67 "Minus.m3"
};
#line 67 "Minus.m3"
LONGINT
__cdecl
Minus__Minus_var_i8_LC(void)
{
#line 67 "Minus.m3"
Minus__Minus_var_i8_LC_Frame_t _frame;
#line 67 "Minus.m3"
_frame._unused=(ADDRESS)&_frame;
#line 67 "Minus.m3"
 /* load */
#line 67 "Minus.m3"
 /* loophole */
#line 67 "Minus.m3"
 /* load */
#line 67 "Minus.m3"
 /* subtract */
#line 67 "Minus.m3"
 /* exit_proc */
#line 67 "Minus.m3"
return ((INT64)(((INT64)((INT64)((INT64)(*((INT8*)(INT64_(104)+((ADDRESS)(&Minus_m_M_Minus_L_13))))))))- ((INT64)(*((UINT64*)(INT64_(136)+((ADDRESS)(&Minus_m_M_Minus_L_13))))))));
#line 67 "Minus.m3"
 /* end_procedure */
#line 67 "Minus.m3"
} /* uMinus_param_i8_LC */
#line 67 "Minus.m3"
 /* set_source_line */
#line 67 "Minus.m3"
#line 68 "Minus.m3"
 /* begin_procedure */
#line 68 "Minus.m3"
struct Minus__uMinus_param_i8_LC_Frame_t {
#line 68 "Minus.m3"
ADDRESS _unused;
#line 68 "Minus.m3"
};
#line 68 "Minus.m3"
Long__T
__cdecl
Minus__uMinus_param_i8_LC(
   /* Param_Type1 */ Minus__INT8 a_L_42,
   /* Param_Type1 */ LONGCARD b_L_43)
{
#line 68 "Minus.m3"
Minus__uMinus_param_i8_LC_Frame_t _frame;
#line 68 "Minus.m3"
_frame._unused=(ADDRESS)&_frame;
#line 68 "Minus.m3"
 /* load */
#line 68 "Minus.m3"
 /* loophole */
#line 68 "Minus.m3"
 /* load */
#line 68 "Minus.m3"
 /* subtract */
#line 68 "Minus.m3"
 /* exit_proc */
#line 68 "Minus.m3"
return ((UINT64)(((UINT64)((INT64)((INT64)(a_L_42))))-((UINT64)(((INT64)(b_L_43))))));
#line 68 "Minus.m3"
 /* end_procedure */
#line 68 "Minus.m3"
} /* Minus_param_i8_LC */
#line 68 "Minus.m3"
 /* set_source_line */
#line 68 "Minus.m3"
#line 69 "Minus.m3"
 /* begin_procedure */
#line 69 "Minus.m3"
struct Minus__Minus_param_i8_LC_Frame_t {
#line 69 "Minus.m3"
ADDRESS _unused;
#line 69 "Minus.m3"
};
#line 69 "Minus.m3"
LONGINT
__cdecl
Minus__Minus_param_i8_LC(
   /* Param_Type1 */ Minus__INT8 a_L_45,
   /* Param_Type1 */ LONGCARD b_L_46)
{
#line 69 "Minus.m3"
Minus__Minus_param_i8_LC_Frame_t _frame;
#line 69 "Minus.m3"
_frame._unused=(ADDRESS)&_frame;
#line 69 "Minus.m3"
 /* load */
#line 69 "Minus.m3"
 /* loophole */
#line 69 "Minus.m3"
 /* load */
#line 69 "Minus.m3"
 /* subtract */
#line 69 "Minus.m3"
 /* exit_proc */
#line 69 "Minus.m3"
return ((INT64)(((INT64)((INT64)((INT64)(a_L_45))))- ((INT64)(b_L_46))));
#line 69 "Minus.m3"
 /* end_procedure */
#line 69 "Minus.m3"
} /* uMinus_var_i8_u16 */
#line 69 "Minus.m3"
 /* set_source_line */
#line 69 "Minus.m3"
#line 70 "Minus.m3"
 /* begin_procedure */
#line 70 "Minus.m3"
struct Minus__uMinus_var_i8_u16_Frame_t {
#line 70 "Minus.m3"
ADDRESS _unused;
#line 70 "Minus.m3"
};
#line 70 "Minus.m3"
Word__T
__cdecl
Minus__uMinus_var_i8_u16(void)
{
#line 70 "Minus.m3"
Minus__uMinus_var_i8_u16_Frame_t _frame;
#line 70 "Minus.m3"
_frame._unused=(ADDRESS)&_frame;
#line 70 "Minus.m3"
 /* load */
#line 70 "Minus.m3"
 /* load */
#line 70 "Minus.m3"
 /* subtract */
#line 70 "Minus.m3"
 /* exit_proc */
#line 70 "Minus.m3"
return ((UINT64)(((UINT64)(((INT64)(*((INT8*)(INT64_(104)+((ADDRESS)(&Minus_m_M_Minus_L_13))))))))-((UINT64)(((INT64)(*((UINT16*)(INT64_(144)+((ADDRESS)(&Minus_m_M_Minus_L_13))))))))));
#line 70 "Minus.m3"
 /* end_procedure */
#line 70 "Minus.m3"
} /* Minus_var_i8_u16 */
#line 70 "Minus.m3"
 /* set_source_line */
#line 70 "Minus.m3"
#line 71 "Minus.m3"
 /* begin_procedure */
#line 71 "Minus.m3"
struct Minus__Minus_var_i8_u16_Frame_t {
#line 71 "Minus.m3"
ADDRESS _unused;
#line 71 "Minus.m3"
};
#line 71 "Minus.m3"
Minus__INT8
__cdecl
Minus__Minus_var_i8_u16(void)
{
#line 71 "Minus.m3"
 /* Var_Type2 */ INT64 Minus_m_1184_L_1185={0};//always-init
#line 71 "Minus.m3"
Minus__Minus_var_i8_u16_Frame_t _frame;
#line 71 "Minus.m3"
_frame._unused=(ADDRESS)&_frame;
#line 71 "Minus.m3"
 /* load */
#line 71 "Minus.m3"
 /* load */
#line 71 "Minus.m3"
 /* subtract */
#line 71 "Minus.m3"
 /* check_lo */
#line 71 "Minus.m3"
 /* store */
#line 71 "Minus.m3"
(*(INT64*)(&Minus_m_1184_L_1185))=(INT64)( ((INT64)( ((INT64)(*((INT8*)(INT64_(104)+((ADDRESS)(&Minus_m_M_Minus_L_13))))))- ((INT64)(*((UINT16*)(INT64_(144)+((ADDRESS)(&Minus_m_M_Minus_L_13)))))))));
#line 71 "Minus.m3"
 /* load */
#line 71 "Minus.m3"
/*check_lo*/if(Minus_m_1184_L_1185<INT64_(-128))Minus_m_M_Minus_L_13_CRASH(2273);
#line 71 "Minus.m3"
 /* exit_proc */
#line 71 "Minus.m3"
return Minus_m_1184_L_1185;
#line 71 "Minus.m3"
 /* end_procedure */
#line 71 "Minus.m3"
} /* uMinus_param_i8_u16 */
#line 71 "Minus.m3"
 /* set_source_line */
#line 71 "Minus.m3"
#line 72 "Minus.m3"
 /* begin_procedure */
#line 72 "Minus.m3"
struct Minus__uMinus_param_i8_u16_Frame_t {
#line 72 "Minus.m3"
ADDRESS _unused;
#line 72 "Minus.m3"
};
#line 72 "Minus.m3"
Word__T
__cdecl
Minus__uMinus_param_i8_u16(
   /* Param_Type1 */ Minus__INT8 a_L_50,
   /* Param_Type1 */ Minus__UINT16 b_L_51)
{
#line 72 "Minus.m3"
Minus__uMinus_param_i8_u16_Frame_t _frame;
#line 72 "Minus.m3"
_frame._unused=(ADDRESS)&_frame;
#line 72 "Minus.m3"
 /* load */
#line 72 "Minus.m3"
 /* load */
#line 72 "Minus.m3"
 /* subtract */
#line 72 "Minus.m3"
 /* exit_proc */
#line 72 "Minus.m3"
return ((UINT64)(((UINT64)(((INT64)(a_L_50))))-((UINT64)(((INT64)(b_L_51))))));
#line 72 "Minus.m3"
 /* end_procedure */
#line 72 "Minus.m3"
} /* Minus_param_i8_u16 */
#line 72 "Minus.m3"
 /* set_source_line */
#line 72 "Minus.m3"
#line 73 "Minus.m3"
 /* begin_procedure */
#line 73 "Minus.m3"
struct Minus__Minus_param_i8_u16_Frame_t {
#line 73 "Minus.m3"
ADDRESS _unused;
#line 73 "Minus.m3"
};
#line 73 "Minus.m3"
Minus__INT8
__cdecl
Minus__Minus_param_i8_u16(
   /* Param_Type1 */ Minus__INT8 a_L_53,
   /* Param_Type1 */ Minus__UINT16 b_L_54)
{
#line 73 "Minus.m3"
 /* Var_Type2 */ INT64 Minus_m_1186_L_1187={0};//always-init
#line 73 "Minus.m3"
Minus__Minus_param_i8_u16_Frame_t _frame;
#line 73 "Minus.m3"
_frame._unused=(ADDRESS)&_frame;
#line 73 "Minus.m3"
 /* load */
#line 73 "Minus.m3"
 /* load */
#line 73 "Minus.m3"
 /* subtract */
#line 73 "Minus.m3"
 /* check_lo */
#line 73 "Minus.m3"
 /* store */
#line 73 "Minus.m3"
(*(INT64*)(&Minus_m_1186_L_1187))=(INT64)( ((INT64)( ((INT64)(a_L_53))- ((INT64)(b_L_54)))));
#line 73 "Minus.m3"
 /* load */
#line 73 "Minus.m3"
/*check_lo*/if(Minus_m_1186_L_1187<INT64_(-128))Minus_m_M_Minus_L_13_CRASH(2337);
#line 73 "Minus.m3"
 /* exit_proc */
#line 73 "Minus.m3"
return Minus_m_1186_L_1187;
#line 73 "Minus.m3"
 /* end_procedure */
#line 73 "Minus.m3"
} /* uMinus_var_i8_I */
#line 73 "Minus.m3"
 /* set_source_line */
#line 73 "Minus.m3"
#line 74 "Minus.m3"
 /* begin_procedure */
#line 74 "Minus.m3"
struct Minus__uMinus_var_i8_I_Frame_t {
#line 74 "Minus.m3"
ADDRESS _unused;
#line 74 "Minus.m3"
};
#line 74 "Minus.m3"
Word__T
__cdecl
Minus__uMinus_var_i8_I(void)
{
#line 74 "Minus.m3"
Minus__uMinus_var_i8_I_Frame_t _frame;
#line 74 "Minus.m3"
_frame._unused=(ADDRESS)&_frame;
#line 74 "Minus.m3"
 /* load */
#line 74 "Minus.m3"
 /* load */
#line 74 "Minus.m3"
 /* subtract */
#line 74 "Minus.m3"
 /* exit_proc */
#line 74 "Minus.m3"
return ((UINT64)(((UINT64)(((INT64)(*((INT8*)(INT64_(104)+((ADDRESS)(&Minus_m_M_Minus_L_13))))))))-((UINT64)(*((INT64*)(INT64_(152)+((ADDRESS)(&Minus_m_M_Minus_L_13))))))));
#line 74 "Minus.m3"
 /* end_procedure */
#line 74 "Minus.m3"
} /* Minus_var_i8_I */
#line 74 "Minus.m3"
 /* set_source_line */
#line 74 "Minus.m3"
#line 75 "Minus.m3"
 /* begin_procedure */
#line 75 "Minus.m3"
struct Minus__Minus_var_i8_I_Frame_t {
#line 75 "Minus.m3"
ADDRESS _unused;
#line 75 "Minus.m3"
};
#line 75 "Minus.m3"
Minus__INT8
__cdecl
Minus__Minus_var_i8_I(void)
{
#line 75 "Minus.m3"
 /* Var_Type2 */ INT64 Minus_m_1188_L_1189={0};//always-init
#line 75 "Minus.m3"
Minus__Minus_var_i8_I_Frame_t _frame;
#line 75 "Minus.m3"
_frame._unused=(ADDRESS)&_frame;
#line 75 "Minus.m3"
 /* load */
#line 75 "Minus.m3"
 /* load */
#line 75 "Minus.m3"
 /* subtract */
#line 75 "Minus.m3"
 /* check_range */
#line 75 "Minus.m3"
 /* store */
#line 75 "Minus.m3"
(*(INT64*)(&Minus_m_1188_L_1189))=(INT64)( ((INT64)( ((INT64)(*((INT8*)(INT64_(104)+((ADDRESS)(&Minus_m_M_Minus_L_13))))))-((INT64)(*((INT64*)(INT64_(152)+((ADDRESS)(&Minus_m_M_Minus_L_13)))))))));
#line 75 "Minus.m3"
 /* load */
#line 75 "Minus.m3"
if(m3_check_range(INT64,
Minus_m_1188_L_1189,
 INT64_(-128),
 INT64_(127)))
#line 75 "Minus.m3"
Minus_m_M_Minus_L_13_CRASH(2401);
#line 75 "Minus.m3"
 /* exit_proc */
#line 75 "Minus.m3"
return Minus_m_1188_L_1189;
#line 75 "Minus.m3"
 /* end_procedure */
#line 75 "Minus.m3"
} /* uMinus_param_i8_I */
#line 75 "Minus.m3"
 /* set_source_line */
#line 75 "Minus.m3"
#line 76 "Minus.m3"
 /* begin_procedure */
#line 76 "Minus.m3"
struct Minus__uMinus_param_i8_I_Frame_t {
#line 76 "Minus.m3"
ADDRESS _unused;
#line 76 "Minus.m3"
};
#line 76 "Minus.m3"
Word__T
__cdecl
Minus__uMinus_param_i8_I(
   /* Param_Type1 */ Minus__INT8 a_L_58,
   /* Param_Type1 */ INTEGER b_L_59)
{
#line 76 "Minus.m3"
Minus__uMinus_param_i8_I_Frame_t _frame;
#line 76 "Minus.m3"
_frame._unused=(ADDRESS)&_frame;
#line 76 "Minus.m3"
 /* load */
#line 76 "Minus.m3"
 /* load */
#line 76 "Minus.m3"
 /* subtract */
#line 76 "Minus.m3"
 /* exit_proc */
#line 76 "Minus.m3"
return ((UINT64)(((UINT64)(((INT64)(a_L_58))))-((UINT64)(b_L_59))));
#line 76 "Minus.m3"
 /* end_procedure */
#line 76 "Minus.m3"
} /* Minus_param_i8_I */
#line 76 "Minus.m3"
 /* set_source_line */
#line 76 "Minus.m3"
#line 77 "Minus.m3"
 /* begin_procedure */
#line 77 "Minus.m3"
struct Minus__Minus_param_i8_I_Frame_t {
#line 77 "Minus.m3"
ADDRESS _unused;
#line 77 "Minus.m3"
};
#line 77 "Minus.m3"
Minus__INT8
__cdecl
Minus__Minus_param_i8_I(
   /* Param_Type1 */ Minus__INT8 a_L_61,
   /* Param_Type1 */ INTEGER b_L_62)
{
#line 77 "Minus.m3"
 /* Var_Type2 */ INT64 Minus_m_1190_L_1191={0};//always-init
#line 77 "Minus.m3"
Minus__Minus_param_i8_I_Frame_t _frame;
#line 77 "Minus.m3"
_frame._unused=(ADDRESS)&_frame;
#line 77 "Minus.m3"
 /* load */
#line 77 "Minus.m3"
 /* load */
#line 77 "Minus.m3"
 /* subtract */
#line 77 "Minus.m3"
 /* check_range */
#line 77 "Minus.m3"
 /* store */
#line 77 "Minus.m3"
(*(INT64*)(&Minus_m_1190_L_1191))=(INT64)( ((INT64)( ((INT64)(a_L_61))- b_L_62)));
#line 77 "Minus.m3"
 /* load */
#line 77 "Minus.m3"
if(m3_check_range(INT64,
Minus_m_1190_L_1191,
 INT64_(-128),
 INT64_(127)))
#line 77 "Minus.m3"
Minus_m_M_Minus_L_13_CRASH(2465);
#line 77 "Minus.m3"
 /* exit_proc */
#line 77 "Minus.m3"
return Minus_m_1190_L_1191;
#line 77 "Minus.m3"
 /* end_procedure */
#line 77 "Minus.m3"
} /* uMinus_var_i8_i64 */
#line 77 "Minus.m3"
 /* set_source_line */
#line 77 "Minus.m3"
#line 78 "Minus.m3"
 /* begin_procedure */
#line 78 "Minus.m3"
struct Minus__uMinus_var_i8_i64_Frame_t {
#line 78 "Minus.m3"
ADDRESS _unused;
#line 78 "Minus.m3"
};
#line 78 "Minus.m3"
Long__T
__cdecl
Minus__uMinus_var_i8_i64(void)
{
#line 78 "Minus.m3"
Minus__uMinus_var_i8_i64_Frame_t _frame;
#line 78 "Minus.m3"
_frame._unused=(ADDRESS)&_frame;
#line 78 "Minus.m3"
 /* load */
#line 78 "Minus.m3"
 /* loophole */
#line 78 "Minus.m3"
 /* load */
#line 78 "Minus.m3"
 /* subtract */
#line 78 "Minus.m3"
 /* exit_proc */
#line 78 "Minus.m3"
return ((UINT64)(((UINT64)((INT64)((INT64)(*((INT8*)(INT64_(104)+((ADDRESS)(&Minus_m_M_Minus_L_13))))))))-((UINT64)(*((INT64*)(INT64_(160)+((ADDRESS)(&Minus_m_M_Minus_L_13))))))));
#line 78 "Minus.m3"
 /* end_procedure */
#line 78 "Minus.m3"
} /* Minus_var_i8_i64 */
#line 78 "Minus.m3"
 /* set_source_line */
#line 78 "Minus.m3"
#line 79 "Minus.m3"
 /* begin_procedure */
#line 79 "Minus.m3"
struct Minus__Minus_var_i8_i64_Frame_t {
#line 79 "Minus.m3"
ADDRESS _unused;
#line 79 "Minus.m3"
};
#line 79 "Minus.m3"
LONGINT
__cdecl
Minus__Minus_var_i8_i64(void)
{
#line 79 "Minus.m3"
Minus__Minus_var_i8_i64_Frame_t _frame;
#line 79 "Minus.m3"
_frame._unused=(ADDRESS)&_frame;
#line 79 "Minus.m3"
 /* load */
#line 79 "Minus.m3"
 /* loophole */
#line 79 "Minus.m3"
 /* load */
#line 79 "Minus.m3"
 /* subtract */
#line 79 "Minus.m3"
 /* exit_proc */
#line 79 "Minus.m3"
return ((INT64)(((INT64)((INT64)((INT64)(*((INT8*)(INT64_(104)+((ADDRESS)(&Minus_m_M_Minus_L_13))))))))-((INT64)(*((INT64*)(INT64_(160)+((ADDRESS)(&Minus_m_M_Minus_L_13))))))));
#line 79 "Minus.m3"
 /* end_procedure */
#line 79 "Minus.m3"
} /* uMinus_param_i8_i64 */
#line 79 "Minus.m3"
 /* set_source_line */
#line 79 "Minus.m3"
#line 80 "Minus.m3"
 /* begin_procedure */
#line 80 "Minus.m3"
struct Minus__uMinus_param_i8_i64_Frame_t {
#line 80 "Minus.m3"
ADDRESS _unused;
#line 80 "Minus.m3"
};
#line 80 "Minus.m3"
Long__T
__cdecl
Minus__uMinus_param_i8_i64(
   /* Param_Type1 */ Minus__INT8 a_L_66,
   /* Param_Type1 */ Minus__INT64 b_L_67)
{
#line 80 "Minus.m3"
Minus__uMinus_param_i8_i64_Frame_t _frame;
#line 80 "Minus.m3"
_frame._unused=(ADDRESS)&_frame;
#line 80 "Minus.m3"
 /* load */
#line 80 "Minus.m3"
 /* loophole */
#line 80 "Minus.m3"
 /* load */
#line 80 "Minus.m3"
 /* subtract */
#line 80 "Minus.m3"
 /* exit_proc */
#line 80 "Minus.m3"
return ((UINT64)(((UINT64)((INT64)((INT64)(a_L_66))))-((UINT64)(b_L_67))));
#line 80 "Minus.m3"
 /* end_procedure */
#line 80 "Minus.m3"
} /* Minus_param_i8_i64 */
#line 80 "Minus.m3"
 /* set_source_line */
#line 80 "Minus.m3"
#line 81 "Minus.m3"
 /* begin_procedure */
#line 81 "Minus.m3"
struct Minus__Minus_param_i8_i64_Frame_t {
#line 81 "Minus.m3"
ADDRESS _unused;
#line 81 "Minus.m3"
};
#line 81 "Minus.m3"
LONGINT
__cdecl
Minus__Minus_param_i8_i64(
   /* Param_Type1 */ Minus__INT8 a_L_69,
   /* Param_Type1 */ Minus__INT64 b_L_70)
{
#line 81 "Minus.m3"
Minus__Minus_param_i8_i64_Frame_t _frame;
#line 81 "Minus.m3"
_frame._unused=(ADDRESS)&_frame;
#line 81 "Minus.m3"
 /* load */
#line 81 "Minus.m3"
 /* loophole */
#line 81 "Minus.m3"
 /* load */
#line 81 "Minus.m3"
 /* subtract */
#line 81 "Minus.m3"
 /* exit_proc */
#line 81 "Minus.m3"
return ((INT64)(((INT64)((INT64)((INT64)(a_L_69))))- b_L_70));
#line 81 "Minus.m3"
 /* end_procedure */
#line 81 "Minus.m3"
} /* uMinus_var_i8_i16 */
#line 81 "Minus.m3"
 /* set_source_line */
#line 81 "Minus.m3"
#line 82 "Minus.m3"
 /* begin_procedure */
#line 82 "Minus.m3"
struct Minus__uMinus_var_i8_i16_Frame_t {
#line 82 "Minus.m3"
ADDRESS _unused;
#line 82 "Minus.m3"
};
#line 82 "Minus.m3"
Word__T
__cdecl
Minus__uMinus_var_i8_i16(void)
{
#line 82 "Minus.m3"
Minus__uMinus_var_i8_i16_Frame_t _frame;
#line 82 "Minus.m3"
_frame._unused=(ADDRESS)&_frame;
#line 82 "Minus.m3"
 /* load */
#line 82 "Minus.m3"
 /* load */
#line 82 "Minus.m3"
 /* subtract */
#line 82 "Minus.m3"
 /* exit_proc */
#line 82 "Minus.m3"
return ((UINT64)(((UINT64)(((INT64)(*((INT8*)(INT64_(104)+((ADDRESS)(&Minus_m_M_Minus_L_13))))))))-((UINT64)(((INT64)(*((INT16*)(INT64_(172)+((ADDRESS)(&Minus_m_M_Minus_L_13))))))))));
#line 82 "Minus.m3"
 /* end_procedure */
#line 82 "Minus.m3"
} /* Minus_var_i8_i16 */
#line 82 "Minus.m3"
 /* set_source_line */
#line 82 "Minus.m3"
#line 83 "Minus.m3"
 /* begin_procedure */
#line 83 "Minus.m3"
struct Minus__Minus_var_i8_i16_Frame_t {
#line 83 "Minus.m3"
ADDRESS _unused;
#line 83 "Minus.m3"
};
#line 83 "Minus.m3"
Minus__INT8
__cdecl
Minus__Minus_var_i8_i16(void)
{
#line 83 "Minus.m3"
 /* Var_Type2 */ INT64 Minus_m_1192_L_1193={0};//always-init
#line 83 "Minus.m3"
Minus__Minus_var_i8_i16_Frame_t _frame;
#line 83 "Minus.m3"
_frame._unused=(ADDRESS)&_frame;
#line 83 "Minus.m3"
 /* load */
#line 83 "Minus.m3"
 /* load */
#line 83 "Minus.m3"
 /* subtract */
#line 83 "Minus.m3"
 /* check_range */
#line 83 "Minus.m3"
 /* store */
#line 83 "Minus.m3"
(*(INT64*)(&Minus_m_1192_L_1193))=(INT64)( ((INT64)( ((INT64)(*((INT8*)(INT64_(104)+((ADDRESS)(&Minus_m_M_Minus_L_13))))))- ((INT64)(*((INT16*)(INT64_(172)+((ADDRESS)(&Minus_m_M_Minus_L_13)))))))));
#line 83 "Minus.m3"
 /* load */
#line 83 "Minus.m3"
if(m3_check_range(INT64,
Minus_m_1192_L_1193,
 INT64_(-128),
 INT64_(127)))
#line 83 "Minus.m3"
Minus_m_M_Minus_L_13_CRASH(2657);
#line 83 "Minus.m3"
 /* exit_proc */
#line 83 "Minus.m3"
return Minus_m_1192_L_1193;
#line 83 "Minus.m3"
 /* end_procedure */
#line 83 "Minus.m3"
} /* uMinus_param_i8_i16 */
#line 83 "Minus.m3"
 /* set_source_line */
#line 83 "Minus.m3"
#line 84 "Minus.m3"
 /* begin_procedure */
#line 84 "Minus.m3"
struct Minus__uMinus_param_i8_i16_Frame_t {
#line 84 "Minus.m3"
ADDRESS _unused;
#line 84 "Minus.m3"
};
#line 84 "Minus.m3"
Word__T
__cdecl
Minus__uMinus_param_i8_i16(
   /* Param_Type1 */ Minus__INT8 a_L_74,
   /* Param_Type1 */ Minus__INT16 b_L_75)
{
#line 84 "Minus.m3"
Minus__uMinus_param_i8_i16_Frame_t _frame;
#line 84 "Minus.m3"
_frame._unused=(ADDRESS)&_frame;
#line 84 "Minus.m3"
 /* load */
#line 84 "Minus.m3"
 /* load */
#line 84 "Minus.m3"
 /* subtract */
#line 84 "Minus.m3"
 /* exit_proc */
#line 84 "Minus.m3"
return ((UINT64)(((UINT64)(((INT64)(a_L_74))))-((UINT64)(((INT64)(b_L_75))))));
#line 84 "Minus.m3"
 /* end_procedure */
#line 84 "Minus.m3"
} /* Minus_param_i8_i16 */
#line 84 "Minus.m3"
 /* set_source_line */
#line 84 "Minus.m3"
#line 85 "Minus.m3"
 /* begin_procedure */
#line 85 "Minus.m3"
struct Minus__Minus_param_i8_i16_Frame_t {
#line 85 "Minus.m3"
ADDRESS _unused;
#line 85 "Minus.m3"
};
#line 85 "Minus.m3"
Minus__INT8
__cdecl
Minus__Minus_param_i8_i16(
   /* Param_Type1 */ Minus__INT8 a_L_77,
   /* Param_Type1 */ Minus__INT16 b_L_78)
{
#line 85 "Minus.m3"
 /* Var_Type2 */ INT64 Minus_m_1194_L_1195={0};//always-init
#line 85 "Minus.m3"
Minus__Minus_param_i8_i16_Frame_t _frame;
#line 85 "Minus.m3"
_frame._unused=(ADDRESS)&_frame;
#line 85 "Minus.m3"
 /* load */
#line 85 "Minus.m3"
 /* load */
#line 85 "Minus.m3"
 /* subtract */
#line 85 "Minus.m3"
 /* check_range */
#line 85 "Minus.m3"
 /* store */
#line 85 "Minus.m3"
(*(INT64*)(&Minus_m_1194_L_1195))=(INT64)( ((INT64)( ((INT64)(a_L_77))- ((INT64)(b_L_78)))));
#line 85 "Minus.m3"
 /* load */
#line 85 "Minus.m3"
if(m3_check_range(INT64,
Minus_m_1194_L_1195,
 INT64_(-128),
 INT64_(127)))
#line 85 "Minus.m3"
Minus_m_M_Minus_L_13_CRASH(2721);
#line 85 "Minus.m3"
 /* exit_proc */
#line 85 "Minus.m3"
return Minus_m_1194_L_1195;
#line 85 "Minus.m3"
 /* end_procedure */
#line 85 "Minus.m3"
} /* uMinus_var_i8_C */
#line 85 "Minus.m3"
 /* set_source_line */
#line 85 "Minus.m3"
#line 86 "Minus.m3"
 /* begin_procedure */
#line 86 "Minus.m3"
struct Minus__uMinus_var_i8_C_Frame_t {
#line 86 "Minus.m3"
ADDRESS _unused;
#line 86 "Minus.m3"
};
#line 86 "Minus.m3"
Word__T
__cdecl
Minus__uMinus_var_i8_C(void)
{
#line 86 "Minus.m3"
Minus__uMinus_var_i8_C_Frame_t _frame;
#line 86 "Minus.m3"
_frame._unused=(ADDRESS)&_frame;
#line 86 "Minus.m3"
 /* load */
#line 86 "Minus.m3"
 /* load */
#line 86 "Minus.m3"
 /* subtract */
#line 86 "Minus.m3"
 /* exit_proc */
#line 86 "Minus.m3"
return ((UINT64)(((UINT64)(((INT64)(*((INT8*)(INT64_(104)+((ADDRESS)(&Minus_m_M_Minus_L_13))))))))-((UINT64)(((INT64)(*((UINT64*)(INT64_(176)+((ADDRESS)(&Minus_m_M_Minus_L_13))))))))));
#line 86 "Minus.m3"
 /* end_procedure */
#line 86 "Minus.m3"
} /* Minus_var_i8_C */
#line 86 "Minus.m3"
 /* set_source_line */
#line 86 "Minus.m3"
#line 87 "Minus.m3"
 /* begin_procedure */
#line 87 "Minus.m3"
struct Minus__Minus_var_i8_C_Frame_t {
#line 87 "Minus.m3"
ADDRESS _unused;
#line 87 "Minus.m3"
};
#line 87 "Minus.m3"
Minus__INT8
__cdecl
Minus__Minus_var_i8_C(void)
{
#line 87 "Minus.m3"
 /* Var_Type2 */ INT64 Minus_m_1196_L_1197={0};//always-init
#line 87 "Minus.m3"
Minus__Minus_var_i8_C_Frame_t _frame;
#line 87 "Minus.m3"
_frame._unused=(ADDRESS)&_frame;
#line 87 "Minus.m3"
 /* load */
#line 87 "Minus.m3"
 /* load */
#line 87 "Minus.m3"
 /* subtract */
#line 87 "Minus.m3"
 /* check_range */
#line 87 "Minus.m3"
 /* store */
#line 87 "Minus.m3"
(*(INT64*)(&Minus_m_1196_L_1197))=(INT64)( ((INT64)( ((INT64)(*((INT8*)(INT64_(104)+((ADDRESS)(&Minus_m_M_Minus_L_13))))))- ((INT64)(*((UINT64*)(INT64_(176)+((ADDRESS)(&Minus_m_M_Minus_L_13)))))))));
#line 87 "Minus.m3"
 /* load */
#line 87 "Minus.m3"
if(m3_check_range(INT64,
Minus_m_1196_L_1197,
 INT64_(-128),
 INT64_(127)))
#line 87 "Minus.m3"
Minus_m_M_Minus_L_13_CRASH(2785);
#line 87 "Minus.m3"
 /* exit_proc */
#line 87 "Minus.m3"
return Minus_m_1196_L_1197;
#line 87 "Minus.m3"
 /* end_procedure */
#line 87 "Minus.m3"
} /* uMinus_param_i8_C */
#line 87 "Minus.m3"
 /* set_source_line */
#line 87 "Minus.m3"
#line 88 "Minus.m3"
 /* begin_procedure */
#line 88 "Minus.m3"
struct Minus__uMinus_param_i8_C_Frame_t {
#line 88 "Minus.m3"
ADDRESS _unused;
#line 88 "Minus.m3"
};
#line 88 "Minus.m3"
Word__T
__cdecl
Minus__uMinus_param_i8_C(
   /* Param_Type1 */ Minus__INT8 a_L_82,
   /* Param_Type1 */ CARDINAL b_L_83)
{
#line 88 "Minus.m3"
Minus__uMinus_param_i8_C_Frame_t _frame;
#line 88 "Minus.m3"
_frame._unused=(ADDRESS)&_frame;
#line 88 "Minus.m3"
 /* load */
#line 88 "Minus.m3"
 /* load */
#line 88 "Minus.m3"
 /* subtract */
#line 88 "Minus.m3"
 /* exit_proc */
#line 88 "Minus.m3"
return ((UINT64)(((UINT64)(((INT64)(a_L_82))))-((UINT64)(((INT64)(b_L_83))))));
#line 88 "Minus.m3"
 /* end_procedure */
#line 88 "Minus.m3"
} /* Minus_param_i8_C */
#line 88 "Minus.m3"
 /* set_source_line */
#line 88 "Minus.m3"
#line 89 "Minus.m3"
 /* begin_procedure */
#line 89 "Minus.m3"
struct Minus__Minus_param_i8_C_Frame_t {
#line 89 "Minus.m3"
ADDRESS _unused;
#line 89 "Minus.m3"
};
#line 89 "Minus.m3"
Minus__INT8
__cdecl
Minus__Minus_param_i8_C(
   /* Param_Type1 */ Minus__INT8 a_L_85,
   /* Param_Type1 */ CARDINAL b_L_86)
{
#line 89 "Minus.m3"
 /* Var_Type2 */ INT64 Minus_m_1198_L_1199={0};//always-init
#line 89 "Minus.m3"
Minus__Minus_param_i8_C_Frame_t _frame;
#line 89 "Minus.m3"
_frame._unused=(ADDRESS)&_frame;
#line 89 "Minus.m3"
 /* load */
#line 89 "Minus.m3"
 /* load */
#line 89 "Minus.m3"
 /* subtract */
#line 89 "Minus.m3"
 /* check_range */
#line 89 "Minus.m3"
 /* store */
#line 89 "Minus.m3"
(*(INT64*)(&Minus_m_1198_L_1199))=(INT64)( ((INT64)( ((INT64)(a_L_85))- ((INT64)(b_L_86)))));
#line 89 "Minus.m3"
 /* load */
#line 89 "Minus.m3"
if(m3_check_range(INT64,
Minus_m_1198_L_1199,
 INT64_(-128),
 INT64_(127)))
#line 89 "Minus.m3"
Minus_m_M_Minus_L_13_CRASH(2849);
#line 89 "Minus.m3"
 /* exit_proc */
#line 89 "Minus.m3"
return Minus_m_1198_L_1199;
#line 89 "Minus.m3"
 /* end_procedure */
#line 89 "Minus.m3"
} /* uMinus_var_i8_u32 */
#line 89 "Minus.m3"
 /* set_source_line */
#line 89 "Minus.m3"
#line 90 "Minus.m3"
 /* begin_procedure */
#line 90 "Minus.m3"
struct Minus__uMinus_var_i8_u32_Frame_t {
#line 90 "Minus.m3"
ADDRESS _unused;
#line 90 "Minus.m3"
};
#line 90 "Minus.m3"
Word__T
__cdecl
Minus__uMinus_var_i8_u32(void)
{
#line 90 "Minus.m3"
Minus__uMinus_var_i8_u32_Frame_t _frame;
#line 90 "Minus.m3"
_frame._unused=(ADDRESS)&_frame;
#line 90 "Minus.m3"
 /* load */
#line 90 "Minus.m3"
 /* load */
#line 90 "Minus.m3"
 /* subtract */
#line 90 "Minus.m3"
 /* exit_proc */
#line 90 "Minus.m3"
return ((UINT64)(((UINT64)(((INT64)(*((INT8*)(INT64_(104)+((ADDRESS)(&Minus_m_M_Minus_L_13))))))))-((UINT64)(((INT64)(*((UINT32*)(INT64_(184)+((ADDRESS)(&Minus_m_M_Minus_L_13))))))))));
#line 90 "Minus.m3"
 /* end_procedure */
#line 90 "Minus.m3"
} /* Minus_var_i8_u32 */
#line 90 "Minus.m3"
 /* set_source_line */
#line 90 "Minus.m3"
#line 91 "Minus.m3"
 /* begin_procedure */
#line 91 "Minus.m3"
struct Minus__Minus_var_i8_u32_Frame_t {
#line 91 "Minus.m3"
ADDRESS _unused;
#line 91 "Minus.m3"
};
#line 91 "Minus.m3"
Minus__INT8
__cdecl
Minus__Minus_var_i8_u32(void)
{
#line 91 "Minus.m3"
 /* Var_Type2 */ INT64 Minus_m_1200_L_1201={0};//always-init
#line 91 "Minus.m3"
Minus__Minus_var_i8_u32_Frame_t _frame;
#line 91 "Minus.m3"
_frame._unused=(ADDRESS)&_frame;
#line 91 "Minus.m3"
 /* load */
#line 91 "Minus.m3"
 /* load */
#line 91 "Minus.m3"
 /* subtract */
#line 91 "Minus.m3"
 /* check_lo */
#line 91 "Minus.m3"
 /* store */
#line 91 "Minus.m3"
(*(INT64*)(&Minus_m_1200_L_1201))=(INT64)( ((INT64)( ((INT64)(*((INT8*)(INT64_(104)+((ADDRESS)(&Minus_m_M_Minus_L_13))))))- ((INT64)(*((UINT32*)(INT64_(184)+((ADDRESS)(&Minus_m_M_Minus_L_13)))))))));
#line 91 "Minus.m3"
 /* load */
#line 91 "Minus.m3"
/*check_lo*/if(Minus_m_1200_L_1201<INT64_(-128))Minus_m_M_Minus_L_13_CRASH(2913);
#line 91 "Minus.m3"
 /* exit_proc */
#line 91 "Minus.m3"
return Minus_m_1200_L_1201;
#line 91 "Minus.m3"
 /* end_procedure */
#line 91 "Minus.m3"
} /* uMinus_param_i8_u32 */
#line 91 "Minus.m3"
 /* set_source_line */
#line 91 "Minus.m3"
#line 92 "Minus.m3"
 /* begin_procedure */
#line 92 "Minus.m3"
struct Minus__uMinus_param_i8_u32_Frame_t {
#line 92 "Minus.m3"
ADDRESS _unused;
#line 92 "Minus.m3"
};
#line 92 "Minus.m3"
Word__T
__cdecl
Minus__uMinus_param_i8_u32(
   /* Param_Type1 */ Minus__INT8 a_L_90,
   /* Param_Type1 */ Minus__UINT32 b_L_91)
{
#line 92 "Minus.m3"
Minus__uMinus_param_i8_u32_Frame_t _frame;
#line 92 "Minus.m3"
_frame._unused=(ADDRESS)&_frame;
#line 92 "Minus.m3"
 /* load */
#line 92 "Minus.m3"
 /* load */
#line 92 "Minus.m3"
 /* subtract */
#line 92 "Minus.m3"
 /* exit_proc */
#line 92 "Minus.m3"
return ((UINT64)(((UINT64)(((INT64)(a_L_90))))-((UINT64)(((INT64)(b_L_91))))));
#line 92 "Minus.m3"
 /* end_procedure */
#line 92 "Minus.m3"
} /* Minus_param_i8_u32 */
#line 92 "Minus.m3"
 /* set_source_line */
#line 92 "Minus.m3"
#line 93 "Minus.m3"
 /* begin_procedure */
#line 93 "Minus.m3"
struct Minus__Minus_param_i8_u32_Frame_t {
#line 93 "Minus.m3"
ADDRESS _unused;
#line 93 "Minus.m3"
};
#line 93 "Minus.m3"
Minus__INT8
__cdecl
Minus__Minus_param_i8_u32(
   /* Param_Type1 */ Minus__INT8 a_L_93,
   /* Param_Type1 */ Minus__UINT32 b_L_94)
{
#line 93 "Minus.m3"
 /* Var_Type2 */ INT64 Minus_m_1202_L_1203={0};//always-init
#line 93 "Minus.m3"
Minus__Minus_param_i8_u32_Frame_t _frame;
#line 93 "Minus.m3"
_frame._unused=(ADDRESS)&_frame;
#line 93 "Minus.m3"
 /* load */
#line 93 "Minus.m3"
 /* load */
#line 93 "Minus.m3"
 /* subtract */
#line 93 "Minus.m3"
 /* check_lo */
#line 93 "Minus.m3"
 /* store */
#line 93 "Minus.m3"
(*(INT64*)(&Minus_m_1202_L_1203))=(INT64)( ((INT64)( ((INT64)(a_L_93))- ((INT64)(b_L_94)))));
#line 93 "Minus.m3"
 /* load */
#line 93 "Minus.m3"
/*check_lo*/if(Minus_m_1202_L_1203<INT64_(-128))Minus_m_M_Minus_L_13_CRASH(2977);
#line 93 "Minus.m3"
 /* exit_proc */
#line 93 "Minus.m3"
return Minus_m_1202_L_1203;
#line 93 "Minus.m3"
 /* end_procedure */
#line 93 "Minus.m3"
} /* uMinus_var_i8_u8 */
#line 93 "Minus.m3"
 /* set_source_line */
#line 93 "Minus.m3"
#line 94 "Minus.m3"
 /* begin_procedure */
#line 94 "Minus.m3"
struct Minus__uMinus_var_i8_u8_Frame_t {
#line 94 "Minus.m3"
ADDRESS _unused;
#line 94 "Minus.m3"
};
#line 94 "Minus.m3"
Word__T
__cdecl
Minus__uMinus_var_i8_u8(void)
{
#line 94 "Minus.m3"
Minus__uMinus_var_i8_u8_Frame_t _frame;
#line 94 "Minus.m3"
_frame._unused=(ADDRESS)&_frame;
#line 94 "Minus.m3"
 /* load */
#line 94 "Minus.m3"
 /* load */
#line 94 "Minus.m3"
 /* subtract */
#line 94 "Minus.m3"
 /* exit_proc */
#line 94 "Minus.m3"
return ((UINT64)(((UINT64)(((INT64)(*((INT8*)(INT64_(104)+((ADDRESS)(&Minus_m_M_Minus_L_13))))))))-((UINT64)(((INT64)(*((UINT8*)(INT64_(188)+((ADDRESS)(&Minus_m_M_Minus_L_13))))))))));
#line 94 "Minus.m3"
 /* end_procedure */
#line 94 "Minus.m3"
} /* Minus_var_i8_u8 */
#line 94 "Minus.m3"
 /* set_source_line */
#line 94 "Minus.m3"
#line 95 "Minus.m3"
 /* begin_procedure */
#line 95 "Minus.m3"
struct Minus__Minus_var_i8_u8_Frame_t {
#line 95 "Minus.m3"
ADDRESS _unused;
#line 95 "Minus.m3"
};
#line 95 "Minus.m3"
Minus__INT8
__cdecl
Minus__Minus_var_i8_u8(void)
{
#line 95 "Minus.m3"
 /* Var_Type2 */ INT64 Minus_m_1204_L_1205={0};//always-init
#line 95 "Minus.m3"
Minus__Minus_var_i8_u8_Frame_t _frame;
#line 95 "Minus.m3"
_frame._unused=(ADDRESS)&_frame;
#line 95 "Minus.m3"
 /* load */
#line 95 "Minus.m3"
 /* load */
#line 95 "Minus.m3"
 /* subtract */
#line 95 "Minus.m3"
 /* check_lo */
#line 95 "Minus.m3"
 /* store */
#line 95 "Minus.m3"
(*(INT64*)(&Minus_m_1204_L_1205))=(INT64)( ((INT64)( ((INT64)(*((INT8*)(INT64_(104)+((ADDRESS)(&Minus_m_M_Minus_L_13))))))- ((INT64)(*((UINT8*)(INT64_(188)+((ADDRESS)(&Minus_m_M_Minus_L_13)))))))));
#line 95 "Minus.m3"
 /* load */
#line 95 "Minus.m3"
/*check_lo*/if(Minus_m_1204_L_1205<INT64_(-128))Minus_m_M_Minus_L_13_CRASH(3041);
#line 95 "Minus.m3"
 /* exit_proc */
#line 95 "Minus.m3"
return Minus_m_1204_L_1205;
#line 95 "Minus.m3"
 /* end_procedure */
#line 95 "Minus.m3"
} /* uMinus_param_i8_u8 */
#line 95 "Minus.m3"
 /* set_source_line */
#line 95 "Minus.m3"
#line 96 "Minus.m3"
 /* begin_procedure */
#line 96 "Minus.m3"
struct Minus__uMinus_param_i8_u8_Frame_t {
#line 96 "Minus.m3"
ADDRESS _unused;
#line 96 "Minus.m3"
};
#line 96 "Minus.m3"
Word__T
__cdecl
Minus__uMinus_param_i8_u8(
   /* Param_Type1 */ Minus__INT8 a_L_98,
   /* Param_Type1 */ Minus__UINT8 b_L_99)
{
#line 96 "Minus.m3"
Minus__uMinus_param_i8_u8_Frame_t _frame;
#line 96 "Minus.m3"
_frame._unused=(ADDRESS)&_frame;
#line 96 "Minus.m3"
 /* load */
#line 96 "Minus.m3"
 /* load */
#line 96 "Minus.m3"
 /* subtract */
#line 96 "Minus.m3"
 /* exit_proc */
#line 96 "Minus.m3"
return ((UINT64)(((UINT64)(((INT64)(a_L_98))))-((UINT64)(((INT64)(b_L_99))))));
#line 96 "Minus.m3"
 /* end_procedure */
#line 96 "Minus.m3"
} /* Minus_param_i8_u8 */
#line 96 "Minus.m3"
 /* set_source_line */
#line 96 "Minus.m3"
#line 97 "Minus.m3"
 /* begin_procedure */
#line 97 "Minus.m3"
struct Minus__Minus_param_i8_u8_Frame_t {
#line 97 "Minus.m3"
ADDRESS _unused;
#line 97 "Minus.m3"
};
#line 97 "Minus.m3"
Minus__INT8
__cdecl
Minus__Minus_param_i8_u8(
   /* Param_Type1 */ Minus__INT8 a_L_101,
   /* Param_Type1 */ Minus__UINT8 b_L_102)
{
#line 97 "Minus.m3"
 /* Var_Type2 */ INT64 Minus_m_1206_L_1207={0};//always-init
#line 97 "Minus.m3"
Minus__Minus_param_i8_u8_Frame_t _frame;
#line 97 "Minus.m3"
_frame._unused=(ADDRESS)&_frame;
#line 97 "Minus.m3"
 /* load */
#line 97 "Minus.m3"
 /* load */
#line 97 "Minus.m3"
 /* subtract */
#line 97 "Minus.m3"
 /* check_lo */
#line 97 "Minus.m3"
 /* store */
#line 97 "Minus.m3"
(*(INT64*)(&Minus_m_1206_L_1207))=(INT64)( ((INT64)( ((INT64)(a_L_101))- ((INT64)(b_L_102)))));
#line 97 "Minus.m3"
 /* load */
#line 97 "Minus.m3"
/*check_lo*/if(Minus_m_1206_L_1207<INT64_(-128))Minus_m_M_Minus_L_13_CRASH(3105);
#line 97 "Minus.m3"
 /* exit_proc */
#line 97 "Minus.m3"
return Minus_m_1206_L_1207;
#line 97 "Minus.m3"
 /* end_procedure */
#line 97 "Minus.m3"
} /* uMinus_var_i8_L */
#line 97 "Minus.m3"
 /* set_source_line */
#line 97 "Minus.m3"
#line 98 "Minus.m3"
 /* begin_procedure */
#line 98 "Minus.m3"
struct Minus__uMinus_var_i8_L_Frame_t {
#line 98 "Minus.m3"
ADDRESS _unused;
#line 98 "Minus.m3"
};
#line 98 "Minus.m3"
Long__T
__cdecl
Minus__uMinus_var_i8_L(void)
{
#line 98 "Minus.m3"
Minus__uMinus_var_i8_L_Frame_t _frame;
#line 98 "Minus.m3"
_frame._unused=(ADDRESS)&_frame;
#line 98 "Minus.m3"
 /* load */
#line 98 "Minus.m3"
 /* loophole */
#line 98 "Minus.m3"
 /* load */
#line 98 "Minus.m3"
 /* subtract */
#line 98 "Minus.m3"
 /* exit_proc */
#line 98 "Minus.m3"
return ((UINT64)(((UINT64)((INT64)((INT64)(*((INT8*)(INT64_(104)+((ADDRESS)(&Minus_m_M_Minus_L_13))))))))-((UINT64)(*((INT64*)(INT64_(192)+((ADDRESS)(&Minus_m_M_Minus_L_13))))))));
#line 98 "Minus.m3"
 /* end_procedure */
#line 98 "Minus.m3"
} /* Minus_var_i8_L */
#line 98 "Minus.m3"
 /* set_source_line */
#line 98 "Minus.m3"
#line 99 "Minus.m3"
 /* begin_procedure */
#line 99 "Minus.m3"
struct Minus__Minus_var_i8_L_Frame_t {
#line 99 "Minus.m3"
ADDRESS _unused;
#line 99 "Minus.m3"
};
#line 99 "Minus.m3"
LONGINT
__cdecl
Minus__Minus_var_i8_L(void)
{
#line 99 "Minus.m3"
Minus__Minus_var_i8_L_Frame_t _frame;
#line 99 "Minus.m3"
_frame._unused=(ADDRESS)&_frame;
#line 99 "Minus.m3"
 /* load */
#line 99 "Minus.m3"
 /* loophole */
#line 99 "Minus.m3"
 /* load */
#line 99 "Minus.m3"
 /* subtract */
#line 99 "Minus.m3"
 /* exit_proc */
#line 99 "Minus.m3"
return ((INT64)(((INT64)((INT64)((INT64)(*((INT8*)(INT64_(104)+((ADDRESS)(&Minus_m_M_Minus_L_13))))))))-((INT64)(*((INT64*)(INT64_(192)+((ADDRESS)(&Minus_m_M_Minus_L_13))))))));
#line 99 "Minus.m3"
 /* end_procedure */
#line 99 "Minus.m3"
} /* uMinus_param_i8_L */
#line 99 "Minus.m3"
 /* set_source_line */
#line 99 "Minus.m3"
#line 100 "Minus.m3"
 /* begin_procedure */
#line 100 "Minus.m3"
struct Minus__uMinus_param_i8_L_Frame_t {
#line 100 "Minus.m3"
ADDRESS _unused;
#line 100 "Minus.m3"
};
#line 100 "Minus.m3"
Long__T
__cdecl
Minus__uMinus_param_i8_L(
   /* Param_Type1 */ Minus__INT8 a_L_106,
   /* Param_Type1 */ LONGINT b_L_107)
{
#line 100 "Minus.m3"
Minus__uMinus_param_i8_L_Frame_t _frame;
#line 100 "Minus.m3"
_frame._unused=(ADDRESS)&_frame;
#line 100 "Minus.m3"
 /* load */
#line 100 "Minus.m3"
 /* loophole */
#line 100 "Minus.m3"
 /* load */
#line 100 "Minus.m3"
 /* subtract */
#line 100 "Minus.m3"
 /* exit_proc */
#line 100 "Minus.m3"
return ((UINT64)(((UINT64)((INT64)((INT64)(a_L_106))))-((UINT64)(b_L_107))));
#line 100 "Minus.m3"
 /* end_procedure */
#line 100 "Minus.m3"
} /* Minus_param_i8_L */
#line 100 "Minus.m3"
 /* set_source_line */
#line 100 "Minus.m3"
#line 101 "Minus.m3"
 /* begin_procedure */
#line 101 "Minus.m3"
struct Minus__Minus_param_i8_L_Frame_t {
#line 101 "Minus.m3"
ADDRESS _unused;
#line 101 "Minus.m3"
};
#line 101 "Minus.m3"
LONGINT
__cdecl
Minus__Minus_param_i8_L(
   /* Param_Type1 */ Minus__INT8 a_L_109,
   /* Param_Type1 */ LONGINT b_L_110)
{
#line 101 "Minus.m3"
Minus__Minus_param_i8_L_Frame_t _frame;
#line 101 "Minus.m3"
_frame._unused=(ADDRESS)&_frame;
#line 101 "Minus.m3"
 /* load */
#line 101 "Minus.m3"
 /* loophole */
#line 101 "Minus.m3"
 /* load */
#line 101 "Minus.m3"
 /* subtract */
#line 101 "Minus.m3"
 /* exit_proc */
#line 101 "Minus.m3"
return ((INT64)(((INT64)((INT64)((INT64)(a_L_109))))- b_L_110));
#line 101 "Minus.m3"
 /* end_procedure */
#line 101 "Minus.m3"
} /* uMinus_var_u64_i8 */
#line 101 "Minus.m3"
 /* set_source_line */
#line 101 "Minus.m3"
#line 102 "Minus.m3"
 /* begin_procedure */
#line 102 "Minus.m3"
struct Minus__uMinus_var_u64_i8_Frame_t {
#line 102 "Minus.m3"
ADDRESS _unused;
#line 102 "Minus.m3"
};
#line 102 "Minus.m3"
Long__T
__cdecl
Minus__uMinus_var_u64_i8(void)
{
#line 102 "Minus.m3"
Minus__uMinus_var_u64_i8_Frame_t _frame;
#line 102 "Minus.m3"
_frame._unused=(ADDRESS)&_frame;
#line 102 "Minus.m3"
 /* load */
#line 102 "Minus.m3"
 /* loophole */
#line 102 "Minus.m3"
 /* load */
#line 102 "Minus.m3"
 /* swap */
#line 102 "Minus.m3"
 /* subtract */
#line 102 "Minus.m3"
 /* exit_proc */
#line 102 "Minus.m3"
return ((UINT64)(((UINT64)(*((INT64*)(INT64_(112)+((ADDRESS)(&Minus_m_M_Minus_L_13))))))-((UINT64)((INT64)((INT64)(*((INT8*)(INT64_(104)+((ADDRESS)(&Minus_m_M_Minus_L_13))))))))));
#line 102 "Minus.m3"
 /* end_procedure */
#line 102 "Minus.m3"
} /* Minus_var_u64_i8 */
#line 102 "Minus.m3"
 /* set_source_line */
#line 102 "Minus.m3"
#line 103 "Minus.m3"
 /* begin_procedure */
#line 103 "Minus.m3"
struct Minus__Minus_var_u64_i8_Frame_t {
#line 103 "Minus.m3"
ADDRESS _unused;
#line 103 "Minus.m3"
};
#line 103 "Minus.m3"
LONGINT
__cdecl
Minus__Minus_var_u64_i8(void)
{
#line 103 "Minus.m3"
Minus__Minus_var_u64_i8_Frame_t _frame;
#line 103 "Minus.m3"
_frame._unused=(ADDRESS)&_frame;
#line 103 "Minus.m3"
 /* load */
#line 103 "Minus.m3"
 /* loophole */
#line 103 "Minus.m3"
 /* load */
#line 103 "Minus.m3"
 /* swap */
#line 103 "Minus.m3"
 /* subtract */
#line 103 "Minus.m3"
 /* exit_proc */
#line 103 "Minus.m3"
return ((INT64)(((INT64)(*((INT64*)(INT64_(112)+((ADDRESS)(&Minus_m_M_Minus_L_13))))))-((INT64)((INT64)((INT64)(*((INT8*)(INT64_(104)+((ADDRESS)(&Minus_m_M_Minus_L_13))))))))));
#line 103 "Minus.m3"
 /* end_procedure */
#line 103 "Minus.m3"
} /* uMinus_param_u64_i8 */
#line 103 "Minus.m3"
 /* set_source_line */
#line 103 "Minus.m3"
#line 104 "Minus.m3"
 /* begin_procedure */
#line 104 "Minus.m3"
struct Minus__uMinus_param_u64_i8_Frame_t {
#line 104 "Minus.m3"
ADDRESS _unused;
#line 104 "Minus.m3"
};
#line 104 "Minus.m3"
Long__T
__cdecl
Minus__uMinus_param_u64_i8(
   /* Param_Type1 */ Minus__UINT64 a_L_114,
   /* Param_Type1 */ Minus__INT8 b_L_115)
{
#line 104 "Minus.m3"
Minus__uMinus_param_u64_i8_Frame_t _frame;
#line 104 "Minus.m3"
_frame._unused=(ADDRESS)&_frame;
#line 104 "Minus.m3"
 /* load */
#line 104 "Minus.m3"
 /* loophole */
#line 104 "Minus.m3"
 /* load */
#line 104 "Minus.m3"
 /* swap */
#line 104 "Minus.m3"
 /* subtract */
#line 104 "Minus.m3"
 /* exit_proc */
#line 104 "Minus.m3"
return ((UINT64)(((UINT64)(a_L_114))-((UINT64)((INT64)((INT64)(b_L_115))))));
#line 104 "Minus.m3"
 /* end_procedure */
#line 104 "Minus.m3"
} /* Minus_param_u64_i8 */
#line 104 "Minus.m3"
 /* set_source_line */
#line 104 "Minus.m3"
#line 105 "Minus.m3"
 /* begin_procedure */
#line 105 "Minus.m3"
struct Minus__Minus_param_u64_i8_Frame_t {
#line 105 "Minus.m3"
ADDRESS _unused;
#line 105 "Minus.m3"
};
#line 105 "Minus.m3"
LONGINT
__cdecl
Minus__Minus_param_u64_i8(
   /* Param_Type1 */ Minus__UINT64 a_L_117,
   /* Param_Type1 */ Minus__INT8 b_L_118)
{
#line 105 "Minus.m3"
Minus__Minus_param_u64_i8_Frame_t _frame;
#line 105 "Minus.m3"
_frame._unused=(ADDRESS)&_frame;
#line 105 "Minus.m3"
 /* load */
#line 105 "Minus.m3"
 /* loophole */
#line 105 "Minus.m3"
 /* load */
#line 105 "Minus.m3"
 /* swap */
#line 105 "Minus.m3"
 /* subtract */
#line 105 "Minus.m3"
 /* exit_proc */
#line 105 "Minus.m3"
return ((INT64)( a_L_117-((INT64)((INT64)((INT64)(b_L_118))))));
#line 105 "Minus.m3"
 /* end_procedure */
#line 105 "Minus.m3"
} /* uMinus_var_u64_u64 */
#line 105 "Minus.m3"
 /* set_source_line */
#line 105 "Minus.m3"
#line 106 "Minus.m3"
 /* begin_procedure */
#line 106 "Minus.m3"
struct Minus__uMinus_var_u64_u64_Frame_t {
#line 106 "Minus.m3"
ADDRESS _unused;
#line 106 "Minus.m3"
};
#line 106 "Minus.m3"
Long__T
__cdecl
Minus__uMinus_var_u64_u64(void)
{
#line 106 "Minus.m3"
Minus__uMinus_var_u64_u64_Frame_t _frame;
#line 106 "Minus.m3"
_frame._unused=(ADDRESS)&_frame;
#line 106 "Minus.m3"
 /* load */
#line 106 "Minus.m3"
 /* load */
#line 106 "Minus.m3"
 /* subtract */
#line 106 "Minus.m3"
 /* exit_proc */
#line 106 "Minus.m3"
return ((UINT64)(((UINT64)(*((INT64*)(INT64_(112)+((ADDRESS)(&Minus_m_M_Minus_L_13))))))-((UINT64)(*((INT64*)(INT64_(112)+((ADDRESS)(&Minus_m_M_Minus_L_13))))))));
#line 106 "Minus.m3"
 /* end_procedure */
#line 106 "Minus.m3"
} /* Minus_var_u64_u64 */
#line 106 "Minus.m3"
 /* set_source_line */
#line 106 "Minus.m3"
#line 107 "Minus.m3"
 /* begin_procedure */
#line 107 "Minus.m3"
struct Minus__Minus_var_u64_u64_Frame_t {
#line 107 "Minus.m3"
ADDRESS _unused;
#line 107 "Minus.m3"
};
#line 107 "Minus.m3"
LONGINT
__cdecl
Minus__Minus_var_u64_u64(void)
{
#line 107 "Minus.m3"
Minus__Minus_var_u64_u64_Frame_t _frame;
#line 107 "Minus.m3"
_frame._unused=(ADDRESS)&_frame;
#line 107 "Minus.m3"
 /* load */
#line 107 "Minus.m3"
 /* load */
#line 107 "Minus.m3"
 /* subtract */
#line 107 "Minus.m3"
 /* exit_proc */
#line 107 "Minus.m3"
return ((INT64)(((INT64)(*((INT64*)(INT64_(112)+((ADDRESS)(&Minus_m_M_Minus_L_13))))))-((INT64)(*((INT64*)(INT64_(112)+((ADDRESS)(&Minus_m_M_Minus_L_13))))))));
#line 107 "Minus.m3"
 /* end_procedure */
#line 107 "Minus.m3"
} /* uMinus_param_u64_u64 */
#line 107 "Minus.m3"
 /* set_source_line */
#line 107 "Minus.m3"
#line 108 "Minus.m3"
 /* begin_procedure */
#line 108 "Minus.m3"
struct Minus__uMinus_param_u64_u64_Frame_t {
#line 108 "Minus.m3"
ADDRESS _unused;
#line 108 "Minus.m3"
};
#line 108 "Minus.m3"
Long__T
__cdecl
Minus__uMinus_param_u64_u64(
   /* Param_Type1 */ Minus__UINT64 a_L_122,
   /* Param_Type1 */ Minus__UINT64 b_L_123)
{
#line 108 "Minus.m3"
Minus__uMinus_param_u64_u64_Frame_t _frame;
#line 108 "Minus.m3"
_frame._unused=(ADDRESS)&_frame;
#line 108 "Minus.m3"
 /* load */
#line 108 "Minus.m3"
 /* load */
#line 108 "Minus.m3"
 /* subtract */
#line 108 "Minus.m3"
 /* exit_proc */
#line 108 "Minus.m3"
return ((UINT64)(((UINT64)(a_L_122))-((UINT64)(b_L_123))));
#line 108 "Minus.m3"
 /* end_procedure */
#line 108 "Minus.m3"
} /* Minus_param_u64_u64 */
#line 108 "Minus.m3"
 /* set_source_line */
#line 108 "Minus.m3"
#line 109 "Minus.m3"
 /* begin_procedure */
#line 109 "Minus.m3"
struct Minus__Minus_param_u64_u64_Frame_t {
#line 109 "Minus.m3"
ADDRESS _unused;
#line 109 "Minus.m3"
};
#line 109 "Minus.m3"
LONGINT
__cdecl
Minus__Minus_param_u64_u64(
   /* Param_Type1 */ Minus__UINT64 a_L_125,
   /* Param_Type1 */ Minus__UINT64 b_L_126)
{
#line 109 "Minus.m3"
Minus__Minus_param_u64_u64_Frame_t _frame;
#line 109 "Minus.m3"
_frame._unused=(ADDRESS)&_frame;
#line 109 "Minus.m3"
 /* load */
#line 109 "Minus.m3"
 /* load */
#line 109 "Minus.m3"
 /* subtract */
#line 109 "Minus.m3"
 /* exit_proc */
#line 109 "Minus.m3"
return ((INT64)( a_L_125- b_L_126));
#line 109 "Minus.m3"
 /* end_procedure */
#line 109 "Minus.m3"
} /* uMinus_var_u64_i32 */
#line 109 "Minus.m3"
 /* set_source_line */
#line 109 "Minus.m3"
#line 110 "Minus.m3"
 /* begin_procedure */
#line 110 "Minus.m3"
struct Minus__uMinus_var_u64_i32_Frame_t {
#line 110 "Minus.m3"
ADDRESS _unused;
#line 110 "Minus.m3"
};
#line 110 "Minus.m3"
Long__T
__cdecl
Minus__uMinus_var_u64_i32(void)
{
#line 110 "Minus.m3"
Minus__uMinus_var_u64_i32_Frame_t _frame;
#line 110 "Minus.m3"
_frame._unused=(ADDRESS)&_frame;
#line 110 "Minus.m3"
 /* load */
#line 110 "Minus.m3"
 /* loophole */
#line 110 "Minus.m3"
 /* load */
#line 110 "Minus.m3"
 /* swap */
#line 110 "Minus.m3"
 /* subtract */
#line 110 "Minus.m3"
 /* exit_proc */
#line 110 "Minus.m3"
return ((UINT64)(((UINT64)(*((INT64*)(INT64_(112)+((ADDRESS)(&Minus_m_M_Minus_L_13))))))-((UINT64)((INT64)((INT64)(*((INT32*)(INT64_(128)+((ADDRESS)(&Minus_m_M_Minus_L_13))))))))));
#line 110 "Minus.m3"
 /* end_procedure */
#line 110 "Minus.m3"
} /* Minus_var_u64_i32 */
#line 110 "Minus.m3"
 /* set_source_line */
#line 110 "Minus.m3"
#line 111 "Minus.m3"
 /* begin_procedure */
#line 111 "Minus.m3"
struct Minus__Minus_var_u64_i32_Frame_t {
#line 111 "Minus.m3"
ADDRESS _unused;
#line 111 "Minus.m3"
};
#line 111 "Minus.m3"
LONGINT
__cdecl
Minus__Minus_var_u64_i32(void)
{
#line 111 "Minus.m3"
Minus__Minus_var_u64_i32_Frame_t _frame;
#line 111 "Minus.m3"
_frame._unused=(ADDRESS)&_frame;
#line 111 "Minus.m3"
 /* load */
#line 111 "Minus.m3"
 /* loophole */
#line 111 "Minus.m3"
 /* load */
#line 111 "Minus.m3"
 /* swap */
#line 111 "Minus.m3"
 /* subtract */
#line 111 "Minus.m3"
 /* exit_proc */
#line 111 "Minus.m3"
return ((INT64)(((INT64)(*((INT64*)(INT64_(112)+((ADDRESS)(&Minus_m_M_Minus_L_13))))))-((INT64)((INT64)((INT64)(*((INT32*)(INT64_(128)+((ADDRESS)(&Minus_m_M_Minus_L_13))))))))));
#line 111 "Minus.m3"
 /* end_procedure */
#line 111 "Minus.m3"
} /* uMinus_param_u64_i32 */
#line 111 "Minus.m3"
 /* set_source_line */
#line 111 "Minus.m3"
#line 112 "Minus.m3"
 /* begin_procedure */
#line 112 "Minus.m3"
struct Minus__uMinus_param_u64_i32_Frame_t {
#line 112 "Minus.m3"
ADDRESS _unused;
#line 112 "Minus.m3"
};
#line 112 "Minus.m3"
Long__T
__cdecl
Minus__uMinus_param_u64_i32(
   /* Param_Type1 */ Minus__UINT64 a_L_130,
   /* Param_Type1 */ Minus__INT32 b_L_131)
{
#line 112 "Minus.m3"
Minus__uMinus_param_u64_i32_Frame_t _frame;
#line 112 "Minus.m3"
_frame._unused=(ADDRESS)&_frame;
#line 112 "Minus.m3"
 /* load */
#line 112 "Minus.m3"
 /* loophole */
#line 112 "Minus.m3"
 /* load */
#line 112 "Minus.m3"
 /* swap */
#line 112 "Minus.m3"
 /* subtract */
#line 112 "Minus.m3"
 /* exit_proc */
#line 112 "Minus.m3"
return ((UINT64)(((UINT64)(a_L_130))-((UINT64)((INT64)((INT64)(b_L_131))))));
#line 112 "Minus.m3"
 /* end_procedure */
#line 112 "Minus.m3"
} /* Minus_param_u64_i32 */
#line 112 "Minus.m3"
 /* set_source_line */
#line 112 "Minus.m3"
#line 113 "Minus.m3"
 /* begin_procedure */
#line 113 "Minus.m3"
struct Minus__Minus_param_u64_i32_Frame_t {
#line 113 "Minus.m3"
ADDRESS _unused;
#line 113 "Minus.m3"
};
#line 113 "Minus.m3"
LONGINT
__cdecl
Minus__Minus_param_u64_i32(
   /* Param_Type1 */ Minus__UINT64 a_L_133,
   /* Param_Type1 */ Minus__INT32 b_L_134)
{
#line 113 "Minus.m3"
Minus__Minus_param_u64_i32_Frame_t _frame;
#line 113 "Minus.m3"
_frame._unused=(ADDRESS)&_frame;
#line 113 "Minus.m3"
 /* load */
#line 113 "Minus.m3"
 /* loophole */
#line 113 "Minus.m3"
 /* load */
#line 113 "Minus.m3"
 /* swap */
#line 113 "Minus.m3"
 /* subtract */
#line 113 "Minus.m3"
 /* exit_proc */
#line 113 "Minus.m3"
return ((INT64)( a_L_133-((INT64)((INT64)((INT64)(b_L_134))))));
#line 113 "Minus.m3"
 /* end_procedure */
#line 113 "Minus.m3"
} /* uMinus_var_u64_LC */
#line 113 "Minus.m3"
 /* set_source_line */
#line 113 "Minus.m3"
#line 114 "Minus.m3"
 /* begin_procedure */
#line 114 "Minus.m3"
struct Minus__uMinus_var_u64_LC_Frame_t {
#line 114 "Minus.m3"
ADDRESS _unused;
#line 114 "Minus.m3"
};
#line 114 "Minus.m3"
Long__T
__cdecl
Minus__uMinus_var_u64_LC(void)
{
#line 114 "Minus.m3"
Minus__uMinus_var_u64_LC_Frame_t _frame;
#line 114 "Minus.m3"
_frame._unused=(ADDRESS)&_frame;
#line 114 "Minus.m3"
 /* load */
#line 114 "Minus.m3"
 /* load */
#line 114 "Minus.m3"
 /* subtract */
#line 114 "Minus.m3"
 /* exit_proc */
#line 114 "Minus.m3"
return ((UINT64)(((UINT64)(*((INT64*)(INT64_(112)+((ADDRESS)(&Minus_m_M_Minus_L_13))))))-((UINT64)(((INT64)(*((UINT64*)(INT64_(136)+((ADDRESS)(&Minus_m_M_Minus_L_13))))))))));
#line 114 "Minus.m3"
 /* end_procedure */
#line 114 "Minus.m3"
} /* Minus_var_u64_LC */
#line 114 "Minus.m3"
 /* set_source_line */
#line 114 "Minus.m3"
#line 115 "Minus.m3"
 /* begin_procedure */
#line 115 "Minus.m3"
struct Minus__Minus_var_u64_LC_Frame_t {
#line 115 "Minus.m3"
ADDRESS _unused;
#line 115 "Minus.m3"
};
#line 115 "Minus.m3"
LONGINT
__cdecl
Minus__Minus_var_u64_LC(void)
{
#line 115 "Minus.m3"
Minus__Minus_var_u64_LC_Frame_t _frame;
#line 115 "Minus.m3"
_frame._unused=(ADDRESS)&_frame;
#line 115 "Minus.m3"
 /* load */
#line 115 "Minus.m3"
 /* load */
#line 115 "Minus.m3"
 /* subtract */
#line 115 "Minus.m3"
 /* exit_proc */
#line 115 "Minus.m3"
return ((INT64)(((INT64)(*((INT64*)(INT64_(112)+((ADDRESS)(&Minus_m_M_Minus_L_13))))))- ((INT64)(*((UINT64*)(INT64_(136)+((ADDRESS)(&Minus_m_M_Minus_L_13))))))));
#line 115 "Minus.m3"
 /* end_procedure */
#line 115 "Minus.m3"
} /* uMinus_param_u64_LC */
#line 115 "Minus.m3"
 /* set_source_line */
#line 115 "Minus.m3"
#line 116 "Minus.m3"
 /* begin_procedure */
#line 116 "Minus.m3"
struct Minus__uMinus_param_u64_LC_Frame_t {
#line 116 "Minus.m3"
ADDRESS _unused;
#line 116 "Minus.m3"
};
#line 116 "Minus.m3"
Long__T
__cdecl
Minus__uMinus_param_u64_LC(
   /* Param_Type1 */ Minus__UINT64 a_L_138,
   /* Param_Type1 */ LONGCARD b_L_139)
{
#line 116 "Minus.m3"
Minus__uMinus_param_u64_LC_Frame_t _frame;
#line 116 "Minus.m3"
_frame._unused=(ADDRESS)&_frame;
#line 116 "Minus.m3"
 /* load */
#line 116 "Minus.m3"
 /* load */
#line 116 "Minus.m3"
 /* subtract */
#line 116 "Minus.m3"
 /* exit_proc */
#line 116 "Minus.m3"
return ((UINT64)(((UINT64)(a_L_138))-((UINT64)(((INT64)(b_L_139))))));
#line 116 "Minus.m3"
 /* end_procedure */
#line 116 "Minus.m3"
} /* Minus_param_u64_LC */
#line 116 "Minus.m3"
 /* set_source_line */
#line 116 "Minus.m3"
#line 117 "Minus.m3"
 /* begin_procedure */
#line 117 "Minus.m3"
struct Minus__Minus_param_u64_LC_Frame_t {
#line 117 "Minus.m3"
ADDRESS _unused;
#line 117 "Minus.m3"
};
#line 117 "Minus.m3"
LONGINT
__cdecl
Minus__Minus_param_u64_LC(
   /* Param_Type1 */ Minus__UINT64 a_L_141,
   /* Param_Type1 */ LONGCARD b_L_142)
{
#line 117 "Minus.m3"
Minus__Minus_param_u64_LC_Frame_t _frame;
#line 117 "Minus.m3"
_frame._unused=(ADDRESS)&_frame;
#line 117 "Minus.m3"
 /* load */
#line 117 "Minus.m3"
 /* load */
#line 117 "Minus.m3"
 /* subtract */
#line 117 "Minus.m3"
 /* exit_proc */
#line 117 "Minus.m3"
return ((INT64)( a_L_141- ((INT64)(b_L_142))));
#line 117 "Minus.m3"
 /* end_procedure */
#line 117 "Minus.m3"
} /* uMinus_var_u64_u16 */
#line 117 "Minus.m3"
 /* set_source_line */
#line 117 "Minus.m3"
#line 118 "Minus.m3"
 /* begin_procedure */
#line 118 "Minus.m3"
struct Minus__uMinus_var_u64_u16_Frame_t {
#line 118 "Minus.m3"
ADDRESS _unused;
#line 118 "Minus.m3"
};
#line 118 "Minus.m3"
Long__T
__cdecl
Minus__uMinus_var_u64_u16(void)
{
#line 118 "Minus.m3"
Minus__uMinus_var_u64_u16_Frame_t _frame;
#line 118 "Minus.m3"
_frame._unused=(ADDRESS)&_frame;
#line 118 "Minus.m3"
 /* load */
#line 118 "Minus.m3"
 /* loophole */
#line 118 "Minus.m3"
 /* load */
#line 118 "Minus.m3"
 /* swap */
#line 118 "Minus.m3"
 /* subtract */
#line 118 "Minus.m3"
 /* exit_proc */
#line 118 "Minus.m3"
return ((UINT64)(((UINT64)(*((INT64*)(INT64_(112)+((ADDRESS)(&Minus_m_M_Minus_L_13))))))-((UINT64)((INT64)((INT64)(*((UINT16*)(INT64_(144)+((ADDRESS)(&Minus_m_M_Minus_L_13))))))))));
#line 118 "Minus.m3"
 /* end_procedure */
#line 118 "Minus.m3"
} /* Minus_var_u64_u16 */
#line 118 "Minus.m3"
 /* set_source_line */
#line 118 "Minus.m3"
#line 119 "Minus.m3"
 /* begin_procedure */
#line 119 "Minus.m3"
struct Minus__Minus_var_u64_u16_Frame_t {
#line 119 "Minus.m3"
ADDRESS _unused;
#line 119 "Minus.m3"
};
#line 119 "Minus.m3"
LONGINT
__cdecl
Minus__Minus_var_u64_u16(void)
{
#line 119 "Minus.m3"
Minus__Minus_var_u64_u16_Frame_t _frame;
#line 119 "Minus.m3"
_frame._unused=(ADDRESS)&_frame;
#line 119 "Minus.m3"
 /* load */
#line 119 "Minus.m3"
 /* loophole */
#line 119 "Minus.m3"
 /* load */
#line 119 "Minus.m3"
 /* swap */
#line 119 "Minus.m3"
 /* subtract */
#line 119 "Minus.m3"
 /* exit_proc */
#line 119 "Minus.m3"
return ((INT64)(((INT64)(*((INT64*)(INT64_(112)+((ADDRESS)(&Minus_m_M_Minus_L_13))))))-((INT64)((INT64)((INT64)(*((UINT16*)(INT64_(144)+((ADDRESS)(&Minus_m_M_Minus_L_13))))))))));
#line 119 "Minus.m3"
 /* end_procedure */
#line 119 "Minus.m3"
} /* uMinus_param_u64_u16 */
#line 119 "Minus.m3"
 /* set_source_line */
#line 119 "Minus.m3"
#line 120 "Minus.m3"
 /* begin_procedure */
#line 120 "Minus.m3"
struct Minus__uMinus_param_u64_u16_Frame_t {
#line 120 "Minus.m3"
ADDRESS _unused;
#line 120 "Minus.m3"
};
#line 120 "Minus.m3"
Long__T
__cdecl
Minus__uMinus_param_u64_u16(
   /* Param_Type1 */ Minus__UINT64 a_L_146,
   /* Param_Type1 */ Minus__UINT16 b_L_147)
{
#line 120 "Minus.m3"
Minus__uMinus_param_u64_u16_Frame_t _frame;
#line 120 "Minus.m3"
_frame._unused=(ADDRESS)&_frame;
#line 120 "Minus.m3"
 /* load */
#line 120 "Minus.m3"
 /* loophole */
#line 120 "Minus.m3"
 /* load */
#line 120 "Minus.m3"
 /* swap */
#line 120 "Minus.m3"
 /* subtract */
#line 120 "Minus.m3"
 /* exit_proc */
#line 120 "Minus.m3"
return ((UINT64)(((UINT64)(a_L_146))-((UINT64)((INT64)((INT64)(b_L_147))))));
#line 120 "Minus.m3"
 /* end_procedure */
#line 120 "Minus.m3"
} /* Minus_param_u64_u16 */
#line 120 "Minus.m3"
 /* set_source_line */
#line 120 "Minus.m3"
#line 121 "Minus.m3"
 /* begin_procedure */
#line 121 "Minus.m3"
struct Minus__Minus_param_u64_u16_Frame_t {
#line 121 "Minus.m3"
ADDRESS _unused;
#line 121 "Minus.m3"
};
#line 121 "Minus.m3"
LONGINT
__cdecl
Minus__Minus_param_u64_u16(
   /* Param_Type1 */ Minus__UINT64 a_L_149,
   /* Param_Type1 */ Minus__UINT16 b_L_150)
{
#line 121 "Minus.m3"
Minus__Minus_param_u64_u16_Frame_t _frame;
#line 121 "Minus.m3"
_frame._unused=(ADDRESS)&_frame;
#line 121 "Minus.m3"
 /* load */
#line 121 "Minus.m3"
 /* loophole */
#line 121 "Minus.m3"
 /* load */
#line 121 "Minus.m3"
 /* swap */
#line 121 "Minus.m3"
 /* subtract */
#line 121 "Minus.m3"
 /* exit_proc */
#line 121 "Minus.m3"
return ((INT64)( a_L_149-((INT64)((INT64)((INT64)(b_L_150))))));
#line 121 "Minus.m3"
 /* end_procedure */
#line 121 "Minus.m3"
} /* uMinus_var_u64_I */
#line 121 "Minus.m3"
 /* set_source_line */
#line 121 "Minus.m3"
#line 122 "Minus.m3"
 /* begin_procedure */
#line 122 "Minus.m3"
struct Minus__uMinus_var_u64_I_Frame_t {
#line 122 "Minus.m3"
ADDRESS _unused;
#line 122 "Minus.m3"
};
#line 122 "Minus.m3"
Long__T
__cdecl
Minus__uMinus_var_u64_I(void)
{
#line 122 "Minus.m3"
Minus__uMinus_var_u64_I_Frame_t _frame;
#line 122 "Minus.m3"
_frame._unused=(ADDRESS)&_frame;
#line 122 "Minus.m3"
 /* load */
#line 122 "Minus.m3"
 /* loophole */
#line 122 "Minus.m3"
 /* load */
#line 122 "Minus.m3"
 /* swap */
#line 122 "Minus.m3"
 /* subtract */
#line 122 "Minus.m3"
 /* exit_proc */
#line 122 "Minus.m3"
return ((UINT64)(((UINT64)(*((INT64*)(INT64_(112)+((ADDRESS)(&Minus_m_M_Minus_L_13))))))-((UINT64)((INT64)*((INT64*)(INT64_(152)+((ADDRESS)(&Minus_m_M_Minus_L_13))))))));
#line 122 "Minus.m3"
 /* end_procedure */
#line 122 "Minus.m3"
} /* Minus_var_u64_I */
#line 122 "Minus.m3"
 /* set_source_line */
#line 122 "Minus.m3"
#line 123 "Minus.m3"
 /* begin_procedure */
#line 123 "Minus.m3"
struct Minus__Minus_var_u64_I_Frame_t {
#line 123 "Minus.m3"
ADDRESS _unused;
#line 123 "Minus.m3"
};
#line 123 "Minus.m3"
LONGINT
__cdecl
Minus__Minus_var_u64_I(void)
{
#line 123 "Minus.m3"
Minus__Minus_var_u64_I_Frame_t _frame;
#line 123 "Minus.m3"
_frame._unused=(ADDRESS)&_frame;
#line 123 "Minus.m3"
 /* load */
#line 123 "Minus.m3"
 /* loophole */
#line 123 "Minus.m3"
 /* load */
#line 123 "Minus.m3"
 /* swap */
#line 123 "Minus.m3"
 /* subtract */
#line 123 "Minus.m3"
 /* exit_proc */
#line 123 "Minus.m3"
return ((INT64)(((INT64)(*((INT64*)(INT64_(112)+((ADDRESS)(&Minus_m_M_Minus_L_13))))))-((INT64)((INT64)*((INT64*)(INT64_(152)+((ADDRESS)(&Minus_m_M_Minus_L_13))))))));
#line 123 "Minus.m3"
 /* end_procedure */
#line 123 "Minus.m3"
} /* uMinus_param_u64_I */
#line 123 "Minus.m3"
 /* set_source_line */
#line 123 "Minus.m3"
#line 124 "Minus.m3"
 /* begin_procedure */
#line 124 "Minus.m3"
struct Minus__uMinus_param_u64_I_Frame_t {
#line 124 "Minus.m3"
ADDRESS _unused;
#line 124 "Minus.m3"
};
#line 124 "Minus.m3"
Long__T
__cdecl
Minus__uMinus_param_u64_I(
   /* Param_Type1 */ Minus__UINT64 a_L_154,
   /* Param_Type1 */ INTEGER b_L_155)
{
#line 124 "Minus.m3"
Minus__uMinus_param_u64_I_Frame_t _frame;
#line 124 "Minus.m3"
_frame._unused=(ADDRESS)&_frame;
#line 124 "Minus.m3"
 /* load */
#line 124 "Minus.m3"
 /* loophole */
#line 124 "Minus.m3"
 /* load */
#line 124 "Minus.m3"
 /* swap */
#line 124 "Minus.m3"
 /* subtract */
#line 124 "Minus.m3"
 /* exit_proc */
#line 124 "Minus.m3"
return ((UINT64)(((UINT64)(a_L_154))-((UINT64)((INT64)b_L_155))));
#line 124 "Minus.m3"
 /* end_procedure */
#line 124 "Minus.m3"
} /* Minus_param_u64_I */
#line 124 "Minus.m3"
 /* set_source_line */
#line 124 "Minus.m3"
#line 125 "Minus.m3"
 /* begin_procedure */
#line 125 "Minus.m3"
struct Minus__Minus_param_u64_I_Frame_t {
#line 125 "Minus.m3"
ADDRESS _unused;
#line 125 "Minus.m3"
};
#line 125 "Minus.m3"
LONGINT
__cdecl
Minus__Minus_param_u64_I(
   /* Param_Type1 */ Minus__UINT64 a_L_157,
   /* Param_Type1 */ INTEGER b_L_158)
{
#line 125 "Minus.m3"
Minus__Minus_param_u64_I_Frame_t _frame;
#line 125 "Minus.m3"
_frame._unused=(ADDRESS)&_frame;
#line 125 "Minus.m3"
 /* load */
#line 125 "Minus.m3"
 /* loophole */
#line 125 "Minus.m3"
 /* load */
#line 125 "Minus.m3"
 /* swap */
#line 125 "Minus.m3"
 /* subtract */
#line 125 "Minus.m3"
 /* exit_proc */
#line 125 "Minus.m3"
return ((INT64)( a_L_157-((INT64)((INT64)b_L_158))));
#line 125 "Minus.m3"
 /* end_procedure */
#line 125 "Minus.m3"
} /* uMinus_var_u64_i64 */
#line 125 "Minus.m3"
 /* set_source_line */
#line 125 "Minus.m3"
#line 126 "Minus.m3"
 /* begin_procedure */
#line 126 "Minus.m3"
struct Minus__uMinus_var_u64_i64_Frame_t {
#line 126 "Minus.m3"
ADDRESS _unused;
#line 126 "Minus.m3"
};
#line 126 "Minus.m3"
Long__T
__cdecl
Minus__uMinus_var_u64_i64(void)
{
#line 126 "Minus.m3"
Minus__uMinus_var_u64_i64_Frame_t _frame;
#line 126 "Minus.m3"
_frame._unused=(ADDRESS)&_frame;
#line 126 "Minus.m3"
 /* load */
#line 126 "Minus.m3"
 /* load */
#line 126 "Minus.m3"
 /* subtract */
#line 126 "Minus.m3"
 /* exit_proc */
#line 126 "Minus.m3"
return ((UINT64)(((UINT64)(*((INT64*)(INT64_(112)+((ADDRESS)(&Minus_m_M_Minus_L_13))))))-((UINT64)(*((INT64*)(INT64_(160)+((ADDRESS)(&Minus_m_M_Minus_L_13))))))));
#line 126 "Minus.m3"
 /* end_procedure */
#line 126 "Minus.m3"
} /* Minus_var_u64_i64 */
#line 126 "Minus.m3"
 /* set_source_line */
#line 126 "Minus.m3"
#line 127 "Minus.m3"
 /* begin_procedure */
#line 127 "Minus.m3"
struct Minus__Minus_var_u64_i64_Frame_t {
#line 127 "Minus.m3"
ADDRESS _unused;
#line 127 "Minus.m3"
};
#line 127 "Minus.m3"
LONGINT
__cdecl
Minus__Minus_var_u64_i64(void)
{
#line 127 "Minus.m3"
Minus__Minus_var_u64_i64_Frame_t _frame;
#line 127 "Minus.m3"
_frame._unused=(ADDRESS)&_frame;
#line 127 "Minus.m3"
 /* load */
#line 127 "Minus.m3"
 /* load */
#line 127 "Minus.m3"
 /* subtract */
#line 127 "Minus.m3"
 /* exit_proc */
#line 127 "Minus.m3"
return ((INT64)(((INT64)(*((INT64*)(INT64_(112)+((ADDRESS)(&Minus_m_M_Minus_L_13))))))-((INT64)(*((INT64*)(INT64_(160)+((ADDRESS)(&Minus_m_M_Minus_L_13))))))));
#line 127 "Minus.m3"
 /* end_procedure */
#line 127 "Minus.m3"
} /* uMinus_param_u64_i64 */
#line 127 "Minus.m3"
 /* set_source_line */
#line 127 "Minus.m3"
#line 128 "Minus.m3"
 /* begin_procedure */
#line 128 "Minus.m3"
struct Minus__uMinus_param_u64_i64_Frame_t {
#line 128 "Minus.m3"
ADDRESS _unused;
#line 128 "Minus.m3"
};
#line 128 "Minus.m3"
Long__T
__cdecl
Minus__uMinus_param_u64_i64(
   /* Param_Type1 */ Minus__UINT64 a_L_162,
   /* Param_Type1 */ Minus__INT64 b_L_163)
{
#line 128 "Minus.m3"
Minus__uMinus_param_u64_i64_Frame_t _frame;
#line 128 "Minus.m3"
_frame._unused=(ADDRESS)&_frame;
#line 128 "Minus.m3"
 /* load */
#line 128 "Minus.m3"
 /* load */
#line 128 "Minus.m3"
 /* subtract */
#line 128 "Minus.m3"
 /* exit_proc */
#line 128 "Minus.m3"
return ((UINT64)(((UINT64)(a_L_162))-((UINT64)(b_L_163))));
#line 128 "Minus.m3"
 /* end_procedure */
#line 128 "Minus.m3"
} /* Minus_param_u64_i64 */
#line 128 "Minus.m3"
 /* set_source_line */
#line 128 "Minus.m3"
#line 129 "Minus.m3"
 /* begin_procedure */
#line 129 "Minus.m3"
struct Minus__Minus_param_u64_i64_Frame_t {
#line 129 "Minus.m3"
ADDRESS _unused;
#line 129 "Minus.m3"
};
#line 129 "Minus.m3"
LONGINT
__cdecl
Minus__Minus_param_u64_i64(
   /* Param_Type1 */ Minus__UINT64 a_L_165,
   /* Param_Type1 */ Minus__INT64 b_L_166)
{
#line 129 "Minus.m3"
Minus__Minus_param_u64_i64_Frame_t _frame;
#line 129 "Minus.m3"
_frame._unused=(ADDRESS)&_frame;
#line 129 "Minus.m3"
 /* load */
#line 129 "Minus.m3"
 /* load */
#line 129 "Minus.m3"
 /* subtract */
#line 129 "Minus.m3"
 /* exit_proc */
#line 129 "Minus.m3"
return ((INT64)( a_L_165- b_L_166));
#line 129 "Minus.m3"
 /* end_procedure */
#line 129 "Minus.m3"
} /* uMinus_var_u64_i16 */
#line 129 "Minus.m3"
 /* set_source_line */
#line 129 "Minus.m3"
#line 130 "Minus.m3"
 /* begin_procedure */
#line 130 "Minus.m3"
struct Minus__uMinus_var_u64_i16_Frame_t {
#line 130 "Minus.m3"
ADDRESS _unused;
#line 130 "Minus.m3"
};
#line 130 "Minus.m3"
Long__T
__cdecl
Minus__uMinus_var_u64_i16(void)
{
#line 130 "Minus.m3"
Minus__uMinus_var_u64_i16_Frame_t _frame;
#line 130 "Minus.m3"
_frame._unused=(ADDRESS)&_frame;
#line 130 "Minus.m3"
 /* load */
#line 130 "Minus.m3"
 /* loophole */
#line 130 "Minus.m3"
 /* load */
#line 130 "Minus.m3"
 /* swap */
#line 130 "Minus.m3"
 /* subtract */
#line 130 "Minus.m3"
 /* exit_proc */
#line 130 "Minus.m3"
return ((UINT64)(((UINT64)(*((INT64*)(INT64_(112)+((ADDRESS)(&Minus_m_M_Minus_L_13))))))-((UINT64)((INT64)((INT64)(*((INT16*)(INT64_(172)+((ADDRESS)(&Minus_m_M_Minus_L_13))))))))));
#line 130 "Minus.m3"
 /* end_procedure */
#line 130 "Minus.m3"
} /* Minus_var_u64_i16 */
#line 130 "Minus.m3"
 /* set_source_line */
#line 130 "Minus.m3"
#line 131 "Minus.m3"
 /* begin_procedure */
#line 131 "Minus.m3"
struct Minus__Minus_var_u64_i16_Frame_t {
#line 131 "Minus.m3"
ADDRESS _unused;
#line 131 "Minus.m3"
};
#line 131 "Minus.m3"
LONGINT
__cdecl
Minus__Minus_var_u64_i16(void)
{
#line 131 "Minus.m3"
Minus__Minus_var_u64_i16_Frame_t _frame;
#line 131 "Minus.m3"
_frame._unused=(ADDRESS)&_frame;
#line 131 "Minus.m3"
 /* load */
#line 131 "Minus.m3"
 /* loophole */
#line 131 "Minus.m3"
 /* load */
#line 131 "Minus.m3"
 /* swap */
#line 131 "Minus.m3"
 /* subtract */
#line 131 "Minus.m3"
 /* exit_proc */
#line 131 "Minus.m3"
return ((INT64)(((INT64)(*((INT64*)(INT64_(112)+((ADDRESS)(&Minus_m_M_Minus_L_13))))))-((INT64)((INT64)((INT64)(*((INT16*)(INT64_(172)+((ADDRESS)(&Minus_m_M_Minus_L_13))))))))));
#line 131 "Minus.m3"
 /* end_procedure */
#line 131 "Minus.m3"
} /* uMinus_param_u64_i16 */
#line 131 "Minus.m3"
 /* set_source_line */
#line 131 "Minus.m3"
#line 132 "Minus.m3"
 /* begin_procedure */
#line 132 "Minus.m3"
struct Minus__uMinus_param_u64_i16_Frame_t {
#line 132 "Minus.m3"
ADDRESS _unused;
#line 132 "Minus.m3"
};
#line 132 "Minus.m3"
Long__T
__cdecl
Minus__uMinus_param_u64_i16(
   /* Param_Type1 */ Minus__UINT64 a_L_170,
   /* Param_Type1 */ Minus__INT16 b_L_171)
{
#line 132 "Minus.m3"
Minus__uMinus_param_u64_i16_Frame_t _frame;
#line 132 "Minus.m3"
_frame._unused=(ADDRESS)&_frame;
#line 132 "Minus.m3"
 /* load */
#line 132 "Minus.m3"
 /* loophole */
#line 132 "Minus.m3"
 /* load */
#line 132 "Minus.m3"
 /* swap */
#line 132 "Minus.m3"
 /* subtract */
#line 132 "Minus.m3"
 /* exit_proc */
#line 132 "Minus.m3"
return ((UINT64)(((UINT64)(a_L_170))-((UINT64)((INT64)((INT64)(b_L_171))))));
#line 132 "Minus.m3"
 /* end_procedure */
#line 132 "Minus.m3"
} /* Minus_param_u64_i16 */
#line 132 "Minus.m3"
 /* set_source_line */
#line 132 "Minus.m3"
#line 133 "Minus.m3"
 /* begin_procedure */
#line 133 "Minus.m3"
struct Minus__Minus_param_u64_i16_Frame_t {
#line 133 "Minus.m3"
ADDRESS _unused;
#line 133 "Minus.m3"
};
#line 133 "Minus.m3"
LONGINT
__cdecl
Minus__Minus_param_u64_i16(
   /* Param_Type1 */ Minus__UINT64 a_L_173,
   /* Param_Type1 */ Minus__INT16 b_L_174)
{
#line 133 "Minus.m3"
Minus__Minus_param_u64_i16_Frame_t _frame;
#line 133 "Minus.m3"
_frame._unused=(ADDRESS)&_frame;
#line 133 "Minus.m3"
 /* load */
#line 133 "Minus.m3"
 /* loophole */
#line 133 "Minus.m3"
 /* load */
#line 133 "Minus.m3"
 /* swap */
#line 133 "Minus.m3"
 /* subtract */
#line 133 "Minus.m3"
 /* exit_proc */
#line 133 "Minus.m3"
return ((INT64)( a_L_173-((INT64)((INT64)((INT64)(b_L_174))))));
#line 133 "Minus.m3"
 /* end_procedure */
#line 133 "Minus.m3"
} /* uMinus_var_u64_C */
#line 133 "Minus.m3"
 /* set_source_line */
#line 133 "Minus.m3"
#line 134 "Minus.m3"
 /* begin_procedure */
#line 134 "Minus.m3"
struct Minus__uMinus_var_u64_C_Frame_t {
#line 134 "Minus.m3"
ADDRESS _unused;
#line 134 "Minus.m3"
};
#line 134 "Minus.m3"
Long__T
__cdecl
Minus__uMinus_var_u64_C(void)
{
#line 134 "Minus.m3"
Minus__uMinus_var_u64_C_Frame_t _frame;
#line 134 "Minus.m3"
_frame._unused=(ADDRESS)&_frame;
#line 134 "Minus.m3"
 /* load */
#line 134 "Minus.m3"
 /* loophole */
#line 134 "Minus.m3"
 /* load */
#line 134 "Minus.m3"
 /* swap */
#line 134 "Minus.m3"
 /* subtract */
#line 134 "Minus.m3"
 /* exit_proc */
#line 134 "Minus.m3"
return ((UINT64)(((UINT64)(*((INT64*)(INT64_(112)+((ADDRESS)(&Minus_m_M_Minus_L_13))))))-((UINT64)((INT64)((INT64)(*((UINT64*)(INT64_(176)+((ADDRESS)(&Minus_m_M_Minus_L_13))))))))));
#line 134 "Minus.m3"
 /* end_procedure */
#line 134 "Minus.m3"
} /* Minus_var_u64_C */
#line 134 "Minus.m3"
 /* set_source_line */
#line 134 "Minus.m3"
#line 135 "Minus.m3"
 /* begin_procedure */
#line 135 "Minus.m3"
struct Minus__Minus_var_u64_C_Frame_t {
#line 135 "Minus.m3"
ADDRESS _unused;
#line 135 "Minus.m3"
};
#line 135 "Minus.m3"
LONGINT
__cdecl
Minus__Minus_var_u64_C(void)
{
#line 135 "Minus.m3"
Minus__Minus_var_u64_C_Frame_t _frame;
#line 135 "Minus.m3"
_frame._unused=(ADDRESS)&_frame;
#line 135 "Minus.m3"
 /* load */
#line 135 "Minus.m3"
 /* loophole */
#line 135 "Minus.m3"
 /* load */
#line 135 "Minus.m3"
 /* swap */
#line 135 "Minus.m3"
 /* subtract */
#line 135 "Minus.m3"
 /* exit_proc */
#line 135 "Minus.m3"
return ((INT64)(((INT64)(*((INT64*)(INT64_(112)+((ADDRESS)(&Minus_m_M_Minus_L_13))))))-((INT64)((INT64)((INT64)(*((UINT64*)(INT64_(176)+((ADDRESS)(&Minus_m_M_Minus_L_13))))))))));
#line 135 "Minus.m3"
 /* end_procedure */
#line 135 "Minus.m3"
} /* uMinus_param_u64_C */
#line 135 "Minus.m3"
 /* set_source_line */
#line 135 "Minus.m3"
#line 136 "Minus.m3"
 /* begin_procedure */
#line 136 "Minus.m3"
struct Minus__uMinus_param_u64_C_Frame_t {
#line 136 "Minus.m3"
ADDRESS _unused;
#line 136 "Minus.m3"
};
#line 136 "Minus.m3"
Long__T
__cdecl
Minus__uMinus_param_u64_C(
   /* Param_Type1 */ Minus__UINT64 a_L_178,
   /* Param_Type1 */ CARDINAL b_L_179)
{
#line 136 "Minus.m3"
Minus__uMinus_param_u64_C_Frame_t _frame;
#line 136 "Minus.m3"
_frame._unused=(ADDRESS)&_frame;
#line 136 "Minus.m3"
 /* load */
#line 136 "Minus.m3"
 /* loophole */
#line 136 "Minus.m3"
 /* load */
#line 136 "Minus.m3"
 /* swap */
#line 136 "Minus.m3"
 /* subtract */
#line 136 "Minus.m3"
 /* exit_proc */
#line 136 "Minus.m3"
return ((UINT64)(((UINT64)(a_L_178))-((UINT64)((INT64)((INT64)(b_L_179))))));
#line 136 "Minus.m3"
 /* end_procedure */
#line 136 "Minus.m3"
} /* Minus_param_u64_C */
#line 136 "Minus.m3"
 /* set_source_line */
#line 136 "Minus.m3"
#line 137 "Minus.m3"
 /* begin_procedure */
#line 137 "Minus.m3"
struct Minus__Minus_param_u64_C_Frame_t {
#line 137 "Minus.m3"
ADDRESS _unused;
#line 137 "Minus.m3"
};
#line 137 "Minus.m3"
LONGINT
__cdecl
Minus__Minus_param_u64_C(
   /* Param_Type1 */ Minus__UINT64 a_L_181,
   /* Param_Type1 */ CARDINAL b_L_182)
{
#line 137 "Minus.m3"
Minus__Minus_param_u64_C_Frame_t _frame;
#line 137 "Minus.m3"
_frame._unused=(ADDRESS)&_frame;
#line 137 "Minus.m3"
 /* load */
#line 137 "Minus.m3"
 /* loophole */
#line 137 "Minus.m3"
 /* load */
#line 137 "Minus.m3"
 /* swap */
#line 137 "Minus.m3"
 /* subtract */
#line 137 "Minus.m3"
 /* exit_proc */
#line 137 "Minus.m3"
return ((INT64)( a_L_181-((INT64)((INT64)((INT64)(b_L_182))))));
#line 137 "Minus.m3"
 /* end_procedure */
#line 137 "Minus.m3"
} /* uMinus_var_u64_u32 */
#line 137 "Minus.m3"
 /* set_source_line */
#line 137 "Minus.m3"
#line 138 "Minus.m3"
 /* begin_procedure */
#line 138 "Minus.m3"
struct Minus__uMinus_var_u64_u32_Frame_t {
#line 138 "Minus.m3"
ADDRESS _unused;
#line 138 "Minus.m3"
};
#line 138 "Minus.m3"
Long__T
__cdecl
Minus__uMinus_var_u64_u32(void)
{
#line 138 "Minus.m3"
Minus__uMinus_var_u64_u32_Frame_t _frame;
#line 138 "Minus.m3"
_frame._unused=(ADDRESS)&_frame;
#line 138 "Minus.m3"
 /* load */
#line 138 "Minus.m3"
 /* loophole */
#line 138 "Minus.m3"
 /* load */
#line 138 "Minus.m3"
 /* swap */
#line 138 "Minus.m3"
 /* subtract */
#line 138 "Minus.m3"
 /* exit_proc */
#line 138 "Minus.m3"
return ((UINT64)(((UINT64)(*((INT64*)(INT64_(112)+((ADDRESS)(&Minus_m_M_Minus_L_13))))))-((UINT64)((INT64)((INT64)(*((UINT32*)(INT64_(184)+((ADDRESS)(&Minus_m_M_Minus_L_13))))))))));
#line 138 "Minus.m3"
 /* end_procedure */
#line 138 "Minus.m3"
} /* Minus_var_u64_u32 */
#line 138 "Minus.m3"
 /* set_source_line */
#line 138 "Minus.m3"
#line 139 "Minus.m3"
 /* begin_procedure */
#line 139 "Minus.m3"
struct Minus__Minus_var_u64_u32_Frame_t {
#line 139 "Minus.m3"
ADDRESS _unused;
#line 139 "Minus.m3"
};
#line 139 "Minus.m3"
LONGINT
__cdecl
Minus__Minus_var_u64_u32(void)
{
#line 139 "Minus.m3"
Minus__Minus_var_u64_u32_Frame_t _frame;
#line 139 "Minus.m3"
_frame._unused=(ADDRESS)&_frame;
#line 139 "Minus.m3"
 /* load */
#line 139 "Minus.m3"
 /* loophole */
#line 139 "Minus.m3"
 /* load */
#line 139 "Minus.m3"
 /* swap */
#line 139 "Minus.m3"
 /* subtract */
#line 139 "Minus.m3"
 /* exit_proc */
#line 139 "Minus.m3"
return ((INT64)(((INT64)(*((INT64*)(INT64_(112)+((ADDRESS)(&Minus_m_M_Minus_L_13))))))-((INT64)((INT64)((INT64)(*((UINT32*)(INT64_(184)+((ADDRESS)(&Minus_m_M_Minus_L_13))))))))));
#line 139 "Minus.m3"
 /* end_procedure */
#line 139 "Minus.m3"
} /* uMinus_param_u64_u32 */
#line 139 "Minus.m3"
 /* set_source_line */
#line 139 "Minus.m3"
#line 140 "Minus.m3"
 /* begin_procedure */
#line 140 "Minus.m3"
struct Minus__uMinus_param_u64_u32_Frame_t {
#line 140 "Minus.m3"
ADDRESS _unused;
#line 140 "Minus.m3"
};
#line 140 "Minus.m3"
Long__T
__cdecl
Minus__uMinus_param_u64_u32(
   /* Param_Type1 */ Minus__UINT64 a_L_186,
   /* Param_Type1 */ Minus__UINT32 b_L_187)
{
#line 140 "Minus.m3"
Minus__uMinus_param_u64_u32_Frame_t _frame;
#line 140 "Minus.m3"
_frame._unused=(ADDRESS)&_frame;
#line 140 "Minus.m3"
 /* load */
#line 140 "Minus.m3"
 /* loophole */
#line 140 "Minus.m3"
 /* load */
#line 140 "Minus.m3"
 /* swap */
#line 140 "Minus.m3"
 /* subtract */
#line 140 "Minus.m3"
 /* exit_proc */
#line 140 "Minus.m3"
return ((UINT64)(((UINT64)(a_L_186))-((UINT64)((INT64)((INT64)(b_L_187))))));
#line 140 "Minus.m3"
 /* end_procedure */
#line 140 "Minus.m3"
} /* Minus_param_u64_u32 */
#line 140 "Minus.m3"
 /* set_source_line */
#line 140 "Minus.m3"
#line 141 "Minus.m3"
 /* begin_procedure */
#line 141 "Minus.m3"
struct Minus__Minus_param_u64_u32_Frame_t {
#line 141 "Minus.m3"
ADDRESS _unused;
#line 141 "Minus.m3"
};
#line 141 "Minus.m3"
LONGINT
__cdecl
Minus__Minus_param_u64_u32(
   /* Param_Type1 */ Minus__UINT64 a_L_189,
   /* Param_Type1 */ Minus__UINT32 b_L_190)
{
#line 141 "Minus.m3"
Minus__Minus_param_u64_u32_Frame_t _frame;
#line 141 "Minus.m3"
_frame._unused=(ADDRESS)&_frame;
#line 141 "Minus.m3"
 /* load */
#line 141 "Minus.m3"
 /* loophole */
#line 141 "Minus.m3"
 /* load */
#line 141 "Minus.m3"
 /* swap */
#line 141 "Minus.m3"
 /* subtract */
#line 141 "Minus.m3"
 /* exit_proc */
#line 141 "Minus.m3"
return ((INT64)( a_L_189-((INT64)((INT64)((INT64)(b_L_190))))));
#line 141 "Minus.m3"
 /* end_procedure */
#line 141 "Minus.m3"
} /* uMinus_var_u64_u8 */
#line 141 "Minus.m3"
 /* set_source_line */
#line 141 "Minus.m3"
#line 142 "Minus.m3"
 /* begin_procedure */
#line 142 "Minus.m3"
struct Minus__uMinus_var_u64_u8_Frame_t {
#line 142 "Minus.m3"
ADDRESS _unused;
#line 142 "Minus.m3"
};
#line 142 "Minus.m3"
Long__T
__cdecl
Minus__uMinus_var_u64_u8(void)
{
#line 142 "Minus.m3"
Minus__uMinus_var_u64_u8_Frame_t _frame;
#line 142 "Minus.m3"
_frame._unused=(ADDRESS)&_frame;
#line 142 "Minus.m3"
 /* load */
#line 142 "Minus.m3"
 /* loophole */
#line 142 "Minus.m3"
 /* load */
#line 142 "Minus.m3"
 /* swap */
#line 142 "Minus.m3"
 /* subtract */
#line 142 "Minus.m3"
 /* exit_proc */
#line 142 "Minus.m3"
return ((UINT64)(((UINT64)(*((INT64*)(INT64_(112)+((ADDRESS)(&Minus_m_M_Minus_L_13))))))-((UINT64)((INT64)((INT64)(*((UINT8*)(INT64_(188)+((ADDRESS)(&Minus_m_M_Minus_L_13))))))))));
#line 142 "Minus.m3"
 /* end_procedure */
#line 142 "Minus.m3"
} /* Minus_var_u64_u8 */
#line 142 "Minus.m3"
 /* set_source_line */
#line 142 "Minus.m3"
#line 143 "Minus.m3"
 /* begin_procedure */
#line 143 "Minus.m3"
struct Minus__Minus_var_u64_u8_Frame_t {
#line 143 "Minus.m3"
ADDRESS _unused;
#line 143 "Minus.m3"
};
#line 143 "Minus.m3"
LONGINT
__cdecl
Minus__Minus_var_u64_u8(void)
{
#line 143 "Minus.m3"
Minus__Minus_var_u64_u8_Frame_t _frame;
#line 143 "Minus.m3"
_frame._unused=(ADDRESS)&_frame;
#line 143 "Minus.m3"
 /* load */
#line 143 "Minus.m3"
 /* loophole */
#line 143 "Minus.m3"
 /* load */
#line 143 "Minus.m3"
 /* swap */
#line 143 "Minus.m3"
 /* subtract */
#line 143 "Minus.m3"
 /* exit_proc */
#line 143 "Minus.m3"
return ((INT64)(((INT64)(*((INT64*)(INT64_(112)+((ADDRESS)(&Minus_m_M_Minus_L_13))))))-((INT64)((INT64)((INT64)(*((UINT8*)(INT64_(188)+((ADDRESS)(&Minus_m_M_Minus_L_13))))))))));
#line 143 "Minus.m3"
 /* end_procedure */
#line 143 "Minus.m3"
} /* uMinus_param_u64_u8 */
#line 143 "Minus.m3"
 /* set_source_line */
#line 143 "Minus.m3"
#line 144 "Minus.m3"
 /* begin_procedure */
#line 144 "Minus.m3"
struct Minus__uMinus_param_u64_u8_Frame_t {
#line 144 "Minus.m3"
ADDRESS _unused;
#line 144 "Minus.m3"
};
#line 144 "Minus.m3"
Long__T
__cdecl
Minus__uMinus_param_u64_u8(
   /* Param_Type1 */ Minus__UINT64 a_L_194,
   /* Param_Type1 */ Minus__UINT8 b_L_195)
{
#line 144 "Minus.m3"
Minus__uMinus_param_u64_u8_Frame_t _frame;
#line 144 "Minus.m3"
_frame._unused=(ADDRESS)&_frame;
#line 144 "Minus.m3"
 /* load */
#line 144 "Minus.m3"
 /* loophole */
#line 144 "Minus.m3"
 /* load */
#line 144 "Minus.m3"
 /* swap */
#line 144 "Minus.m3"
 /* subtract */
#line 144 "Minus.m3"
 /* exit_proc */
#line 144 "Minus.m3"
return ((UINT64)(((UINT64)(a_L_194))-((UINT64)((INT64)((INT64)(b_L_195))))));
#line 144 "Minus.m3"
 /* end_procedure */
#line 144 "Minus.m3"
} /* Minus_param_u64_u8 */
#line 144 "Minus.m3"
 /* set_source_line */
#line 144 "Minus.m3"
#line 145 "Minus.m3"
 /* begin_procedure */
#line 145 "Minus.m3"
struct Minus__Minus_param_u64_u8_Frame_t {
#line 145 "Minus.m3"
ADDRESS _unused;
#line 145 "Minus.m3"
};
#line 145 "Minus.m3"
LONGINT
__cdecl
Minus__Minus_param_u64_u8(
   /* Param_Type1 */ Minus__UINT64 a_L_197,
   /* Param_Type1 */ Minus__UINT8 b_L_198)
{
#line 145 "Minus.m3"
Minus__Minus_param_u64_u8_Frame_t _frame;
#line 145 "Minus.m3"
_frame._unused=(ADDRESS)&_frame;
#line 145 "Minus.m3"
 /* load */
#line 145 "Minus.m3"
 /* loophole */
#line 145 "Minus.m3"
 /* load */
#line 145 "Minus.m3"
 /* swap */
#line 145 "Minus.m3"
 /* subtract */
#line 145 "Minus.m3"
 /* exit_proc */
#line 145 "Minus.m3"
return ((INT64)( a_L_197-((INT64)((INT64)((INT64)(b_L_198))))));
#line 145 "Minus.m3"
 /* end_procedure */
#line 145 "Minus.m3"
} /* uMinus_var_u64_L */
#line 145 "Minus.m3"
 /* set_source_line */
#line 145 "Minus.m3"
#line 146 "Minus.m3"
 /* begin_procedure */
#line 146 "Minus.m3"
struct Minus__uMinus_var_u64_L_Frame_t {
#line 146 "Minus.m3"
ADDRESS _unused;
#line 146 "Minus.m3"
};
#line 146 "Minus.m3"
Long__T
__cdecl
Minus__uMinus_var_u64_L(void)
{
#line 146 "Minus.m3"
Minus__uMinus_var_u64_L_Frame_t _frame;
#line 146 "Minus.m3"
_frame._unused=(ADDRESS)&_frame;
#line 146 "Minus.m3"
 /* load */
#line 146 "Minus.m3"
 /* load */
#line 146 "Minus.m3"
 /* subtract */
#line 146 "Minus.m3"
 /* exit_proc */
#line 146 "Minus.m3"
return ((UINT64)(((UINT64)(*((INT64*)(INT64_(112)+((ADDRESS)(&Minus_m_M_Minus_L_13))))))-((UINT64)(*((INT64*)(INT64_(192)+((ADDRESS)(&Minus_m_M_Minus_L_13))))))));
#line 146 "Minus.m3"
 /* end_procedure */
#line 146 "Minus.m3"
} /* Minus_var_u64_L */
#line 146 "Minus.m3"
 /* set_source_line */
#line 146 "Minus.m3"
#line 147 "Minus.m3"
 /* begin_procedure */
#line 147 "Minus.m3"
struct Minus__Minus_var_u64_L_Frame_t {
#line 147 "Minus.m3"
ADDRESS _unused;
#line 147 "Minus.m3"
};
#line 147 "Minus.m3"
LONGINT
__cdecl
Minus__Minus_var_u64_L(void)
{
#line 147 "Minus.m3"
Minus__Minus_var_u64_L_Frame_t _frame;
#line 147 "Minus.m3"
_frame._unused=(ADDRESS)&_frame;
#line 147 "Minus.m3"
 /* load */
#line 147 "Minus.m3"
 /* load */
#line 147 "Minus.m3"
 /* subtract */
#line 147 "Minus.m3"
 /* exit_proc */
#line 147 "Minus.m3"
return ((INT64)(((INT64)(*((INT64*)(INT64_(112)+((ADDRESS)(&Minus_m_M_Minus_L_13))))))-((INT64)(*((INT64*)(INT64_(192)+((ADDRESS)(&Minus_m_M_Minus_L_13))))))));
#line 147 "Minus.m3"
 /* end_procedure */
#line 147 "Minus.m3"
} /* uMinus_param_u64_L */
#line 147 "Minus.m3"
 /* set_source_line */
#line 147 "Minus.m3"
#line 148 "Minus.m3"
 /* begin_procedure */
#line 148 "Minus.m3"
struct Minus__uMinus_param_u64_L_Frame_t {
#line 148 "Minus.m3"
ADDRESS _unused;
#line 148 "Minus.m3"
};
#line 148 "Minus.m3"
Long__T
__cdecl
Minus__uMinus_param_u64_L(
   /* Param_Type1 */ Minus__UINT64 a_L_202,
   /* Param_Type1 */ LONGINT b_L_203)
{
#line 148 "Minus.m3"
Minus__uMinus_param_u64_L_Frame_t _frame;
#line 148 "Minus.m3"
_frame._unused=(ADDRESS)&_frame;
#line 148 "Minus.m3"
 /* load */
#line 148 "Minus.m3"
 /* load */
#line 148 "Minus.m3"
 /* subtract */
#line 148 "Minus.m3"
 /* exit_proc */
#line 148 "Minus.m3"
return ((UINT64)(((UINT64)(a_L_202))-((UINT64)(b_L_203))));
#line 148 "Minus.m3"
 /* end_procedure */
#line 148 "Minus.m3"
} /* Minus_param_u64_L */
#line 148 "Minus.m3"
 /* set_source_line */
#line 148 "Minus.m3"
#line 149 "Minus.m3"
 /* begin_procedure */
#line 149 "Minus.m3"
struct Minus__Minus_param_u64_L_Frame_t {
#line 149 "Minus.m3"
ADDRESS _unused;
#line 149 "Minus.m3"
};
#line 149 "Minus.m3"
LONGINT
__cdecl
Minus__Minus_param_u64_L(
   /* Param_Type1 */ Minus__UINT64 a_L_205,
   /* Param_Type1 */ LONGINT b_L_206)
{
#line 149 "Minus.m3"
Minus__Minus_param_u64_L_Frame_t _frame;
#line 149 "Minus.m3"
_frame._unused=(ADDRESS)&_frame;
#line 149 "Minus.m3"
 /* load */
#line 149 "Minus.m3"
 /* load */
#line 149 "Minus.m3"
 /* subtract */
#line 149 "Minus.m3"
 /* exit_proc */
#line 149 "Minus.m3"
return ((INT64)( a_L_205- b_L_206));
#line 149 "Minus.m3"
 /* end_procedure */
#line 149 "Minus.m3"
} /* Minus_var_f64_f64 */
#line 149 "Minus.m3"
 /* set_source_line */
#line 149 "Minus.m3"
#line 150 "Minus.m3"
 /* begin_procedure */
#line 150 "Minus.m3"
struct Minus__Minus_var_f64_f64_Frame_t {
#line 150 "Minus.m3"
ADDRESS _unused;
#line 150 "Minus.m3"
};
#line 150 "Minus.m3"
Minus__FLOAT64
__cdecl
Minus__Minus_var_f64_f64(void)
{
#line 150 "Minus.m3"
Minus__Minus_var_f64_f64_Frame_t _frame;
#line 150 "Minus.m3"
_frame._unused=(ADDRESS)&_frame;
#line 150 "Minus.m3"
 /* load */
#line 150 "Minus.m3"
 /* load */
#line 150 "Minus.m3"
 /* subtract */
#line 150 "Minus.m3"
 /* exit_proc */
#line 150 "Minus.m3"
return ((double)(((double)(*((double*)(INT64_(120)+((ADDRESS)(&Minus_m_M_Minus_L_13))))))-((double)(*((double*)(INT64_(120)+((ADDRESS)(&Minus_m_M_Minus_L_13))))))));
#line 150 "Minus.m3"
 /* end_procedure */
#line 150 "Minus.m3"
} /* Minus_param_f64_f64 */
#line 150 "Minus.m3"
 /* set_source_line */
#line 150 "Minus.m3"
#line 151 "Minus.m3"
 /* begin_procedure */
#line 151 "Minus.m3"
struct Minus__Minus_param_f64_f64_Frame_t {
#line 151 "Minus.m3"
ADDRESS _unused;
#line 151 "Minus.m3"
};
#line 151 "Minus.m3"
Minus__FLOAT64
__cdecl
Minus__Minus_param_f64_f64(
   /* Param_Type1 */ Minus__FLOAT64 a_L_209,
   /* Param_Type1 */ Minus__FLOAT64 b_L_210)
{
#line 151 "Minus.m3"
Minus__Minus_param_f64_f64_Frame_t _frame;
#line 151 "Minus.m3"
_frame._unused=(ADDRESS)&_frame;
#line 151 "Minus.m3"
 /* load */
#line 151 "Minus.m3"
 /* load */
#line 151 "Minus.m3"
 /* subtract */
#line 151 "Minus.m3"
 /* exit_proc */
#line 151 "Minus.m3"
return ((double)( a_L_209- b_L_210));
#line 151 "Minus.m3"
 /* end_procedure */
#line 151 "Minus.m3"
} /* uMinus_var_i32_i8 */
#line 151 "Minus.m3"
 /* set_source_line */
#line 151 "Minus.m3"
#line 152 "Minus.m3"
 /* begin_procedure */
#line 152 "Minus.m3"
struct Minus__uMinus_var_i32_i8_Frame_t {
#line 152 "Minus.m3"
ADDRESS _unused;
#line 152 "Minus.m3"
};
#line 152 "Minus.m3"
Word__T
__cdecl
Minus__uMinus_var_i32_i8(void)
{
#line 152 "Minus.m3"
Minus__uMinus_var_i32_i8_Frame_t _frame;
#line 152 "Minus.m3"
_frame._unused=(ADDRESS)&_frame;
#line 152 "Minus.m3"
 /* load */
#line 152 "Minus.m3"
 /* load */
#line 152 "Minus.m3"
 /* subtract */
#line 152 "Minus.m3"
 /* exit_proc */
#line 152 "Minus.m3"
return ((UINT64)(((UINT64)(((INT64)(*((INT32*)(INT64_(128)+((ADDRESS)(&Minus_m_M_Minus_L_13))))))))-((UINT64)(((INT64)(*((INT8*)(INT64_(104)+((ADDRESS)(&Minus_m_M_Minus_L_13))))))))));
#line 152 "Minus.m3"
 /* end_procedure */
#line 152 "Minus.m3"
} /* Minus_var_i32_i8 */
#line 152 "Minus.m3"
 /* set_source_line */
#line 152 "Minus.m3"
#line 153 "Minus.m3"
 /* begin_procedure */
#line 153 "Minus.m3"
struct Minus__Minus_var_i32_i8_Frame_t {
#line 153 "Minus.m3"
ADDRESS _unused;
#line 153 "Minus.m3"
};
#line 153 "Minus.m3"
Minus__INT32
__cdecl
Minus__Minus_var_i32_i8(void)
{
#line 153 "Minus.m3"
 /* Var_Type2 */ INT64 Minus_m_1208_L_1209={0};//always-init
#line 153 "Minus.m3"
Minus__Minus_var_i32_i8_Frame_t _frame;
#line 153 "Minus.m3"
_frame._unused=(ADDRESS)&_frame;
#line 153 "Minus.m3"
 /* load */
#line 153 "Minus.m3"
 /* load */
#line 153 "Minus.m3"
 /* subtract */
#line 153 "Minus.m3"
 /* check_range */
#line 153 "Minus.m3"
 /* store */
#line 153 "Minus.m3"
(*(INT64*)(&Minus_m_1208_L_1209))=(INT64)( ((INT64)( ((INT64)(*((INT32*)(INT64_(128)+((ADDRESS)(&Minus_m_M_Minus_L_13))))))- ((INT64)(*((INT8*)(INT64_(104)+((ADDRESS)(&Minus_m_M_Minus_L_13)))))))));
#line 153 "Minus.m3"
 /* load */
#line 153 "Minus.m3"
if(m3_check_range(INT64,
Minus_m_1208_L_1209,
 INT64_(-2147483648),
 INT64_(2147483647)))
#line 153 "Minus.m3"
Minus_m_M_Minus_L_13_CRASH(4897);
#line 153 "Minus.m3"
 /* exit_proc */
#line 153 "Minus.m3"
return Minus_m_1208_L_1209;
#line 153 "Minus.m3"
 /* end_procedure */
#line 153 "Minus.m3"
} /* uMinus_param_i32_i8 */
#line 153 "Minus.m3"
 /* set_source_line */
#line 153 "Minus.m3"
#line 154 "Minus.m3"
 /* begin_procedure */
#line 154 "Minus.m3"
struct Minus__uMinus_param_i32_i8_Frame_t {
#line 154 "Minus.m3"
ADDRESS _unused;
#line 154 "Minus.m3"
};
#line 154 "Minus.m3"
Word__T
__cdecl
Minus__uMinus_param_i32_i8(
   /* Param_Type1 */ Minus__INT32 a_L_214,
   /* Param_Type1 */ Minus__INT8 b_L_215)
{
#line 154 "Minus.m3"
Minus__uMinus_param_i32_i8_Frame_t _frame;
#line 154 "Minus.m3"
_frame._unused=(ADDRESS)&_frame;
#line 154 "Minus.m3"
 /* load */
#line 154 "Minus.m3"
 /* load */
#line 154 "Minus.m3"
 /* subtract */
#line 154 "Minus.m3"
 /* exit_proc */
#line 154 "Minus.m3"
return ((UINT64)(((UINT64)(((INT64)(a_L_214))))-((UINT64)(((INT64)(b_L_215))))));
#line 154 "Minus.m3"
 /* end_procedure */
#line 154 "Minus.m3"
} /* Minus_param_i32_i8 */
#line 154 "Minus.m3"
 /* set_source_line */
#line 154 "Minus.m3"
#line 155 "Minus.m3"
 /* begin_procedure */
#line 155 "Minus.m3"
struct Minus__Minus_param_i32_i8_Frame_t {
#line 155 "Minus.m3"
ADDRESS _unused;
#line 155 "Minus.m3"
};
#line 155 "Minus.m3"
Minus__INT32
__cdecl
Minus__Minus_param_i32_i8(
   /* Param_Type1 */ Minus__INT32 a_L_217,
   /* Param_Type1 */ Minus__INT8 b_L_218)
{
#line 155 "Minus.m3"
 /* Var_Type2 */ INT64 Minus_m_1210_L_1211={0};//always-init
#line 155 "Minus.m3"
Minus__Minus_param_i32_i8_Frame_t _frame;
#line 155 "Minus.m3"
_frame._unused=(ADDRESS)&_frame;
#line 155 "Minus.m3"
 /* load */
#line 155 "Minus.m3"
 /* load */
#line 155 "Minus.m3"
 /* subtract */
#line 155 "Minus.m3"
 /* check_range */
#line 155 "Minus.m3"
 /* store */
#line 155 "Minus.m3"
(*(INT64*)(&Minus_m_1210_L_1211))=(INT64)( ((INT64)( ((INT64)(a_L_217))- ((INT64)(b_L_218)))));
#line 155 "Minus.m3"
 /* load */
#line 155 "Minus.m3"
if(m3_check_range(INT64,
Minus_m_1210_L_1211,
 INT64_(-2147483648),
 INT64_(2147483647)))
#line 155 "Minus.m3"
Minus_m_M_Minus_L_13_CRASH(4961);
#line 155 "Minus.m3"
 /* exit_proc */
#line 155 "Minus.m3"
return Minus_m_1210_L_1211;
#line 155 "Minus.m3"
 /* end_procedure */
#line 155 "Minus.m3"
} /* uMinus_var_i32_u64 */
#line 155 "Minus.m3"
 /* set_source_line */
#line 155 "Minus.m3"
#line 156 "Minus.m3"
 /* begin_procedure */
#line 156 "Minus.m3"
struct Minus__uMinus_var_i32_u64_Frame_t {
#line 156 "Minus.m3"
ADDRESS _unused;
#line 156 "Minus.m3"
};
#line 156 "Minus.m3"
Long__T
__cdecl
Minus__uMinus_var_i32_u64(void)
{
#line 156 "Minus.m3"
Minus__uMinus_var_i32_u64_Frame_t _frame;
#line 156 "Minus.m3"
_frame._unused=(ADDRESS)&_frame;
#line 156 "Minus.m3"
 /* load */
#line 156 "Minus.m3"
 /* loophole */
#line 156 "Minus.m3"
 /* load */
#line 156 "Minus.m3"
 /* subtract */
#line 156 "Minus.m3"
 /* exit_proc */
#line 156 "Minus.m3"
return ((UINT64)(((UINT64)((INT64)((INT64)(*((INT32*)(INT64_(128)+((ADDRESS)(&Minus_m_M_Minus_L_13))))))))-((UINT64)(*((INT64*)(INT64_(112)+((ADDRESS)(&Minus_m_M_Minus_L_13))))))));
#line 156 "Minus.m3"
 /* end_procedure */
#line 156 "Minus.m3"
} /* Minus_var_i32_u64 */
#line 156 "Minus.m3"
 /* set_source_line */
#line 156 "Minus.m3"
#line 157 "Minus.m3"
 /* begin_procedure */
#line 157 "Minus.m3"
struct Minus__Minus_var_i32_u64_Frame_t {
#line 157 "Minus.m3"
ADDRESS _unused;
#line 157 "Minus.m3"
};
#line 157 "Minus.m3"
LONGINT
__cdecl
Minus__Minus_var_i32_u64(void)
{
#line 157 "Minus.m3"
Minus__Minus_var_i32_u64_Frame_t _frame;
#line 157 "Minus.m3"
_frame._unused=(ADDRESS)&_frame;
#line 157 "Minus.m3"
 /* load */
#line 157 "Minus.m3"
 /* loophole */
#line 157 "Minus.m3"
 /* load */
#line 157 "Minus.m3"
 /* subtract */
#line 157 "Minus.m3"
 /* exit_proc */
#line 157 "Minus.m3"
return ((INT64)(((INT64)((INT64)((INT64)(*((INT32*)(INT64_(128)+((ADDRESS)(&Minus_m_M_Minus_L_13))))))))-((INT64)(*((INT64*)(INT64_(112)+((ADDRESS)(&Minus_m_M_Minus_L_13))))))));
#line 157 "Minus.m3"
 /* end_procedure */
#line 157 "Minus.m3"
} /* uMinus_param_i32_u64 */
#line 157 "Minus.m3"
 /* set_source_line */
#line 157 "Minus.m3"
#line 158 "Minus.m3"
 /* begin_procedure */
#line 158 "Minus.m3"
struct Minus__uMinus_param_i32_u64_Frame_t {
#line 158 "Minus.m3"
ADDRESS _unused;
#line 158 "Minus.m3"
};
#line 158 "Minus.m3"
Long__T
__cdecl
Minus__uMinus_param_i32_u64(
   /* Param_Type1 */ Minus__INT32 a_L_222,
   /* Param_Type1 */ Minus__UINT64 b_L_223)
{
#line 158 "Minus.m3"
Minus__uMinus_param_i32_u64_Frame_t _frame;
#line 158 "Minus.m3"
_frame._unused=(ADDRESS)&_frame;
#line 158 "Minus.m3"
 /* load */
#line 158 "Minus.m3"
 /* loophole */
#line 158 "Minus.m3"
 /* load */
#line 158 "Minus.m3"
 /* subtract */
#line 158 "Minus.m3"
 /* exit_proc */
#line 158 "Minus.m3"
return ((UINT64)(((UINT64)((INT64)((INT64)(a_L_222))))-((UINT64)(b_L_223))));
#line 158 "Minus.m3"
 /* end_procedure */
#line 158 "Minus.m3"
} /* Minus_param_i32_u64 */
#line 158 "Minus.m3"
 /* set_source_line */
#line 158 "Minus.m3"
#line 159 "Minus.m3"
 /* begin_procedure */
#line 159 "Minus.m3"
struct Minus__Minus_param_i32_u64_Frame_t {
#line 159 "Minus.m3"
ADDRESS _unused;
#line 159 "Minus.m3"
};
#line 159 "Minus.m3"
LONGINT
__cdecl
Minus__Minus_param_i32_u64(
   /* Param_Type1 */ Minus__INT32 a_L_225,
   /* Param_Type1 */ Minus__UINT64 b_L_226)
{
#line 159 "Minus.m3"
Minus__Minus_param_i32_u64_Frame_t _frame;
#line 159 "Minus.m3"
_frame._unused=(ADDRESS)&_frame;
#line 159 "Minus.m3"
 /* load */
#line 159 "Minus.m3"
 /* loophole */
#line 159 "Minus.m3"
 /* load */
#line 159 "Minus.m3"
 /* subtract */
#line 159 "Minus.m3"
 /* exit_proc */
#line 159 "Minus.m3"
return ((INT64)(((INT64)((INT64)((INT64)(a_L_225))))- b_L_226));
#line 159 "Minus.m3"
 /* end_procedure */
#line 159 "Minus.m3"
} /* uMinus_var_i32_i32 */
#line 159 "Minus.m3"
 /* set_source_line */
#line 159 "Minus.m3"
#line 160 "Minus.m3"
 /* begin_procedure */
#line 160 "Minus.m3"
struct Minus__uMinus_var_i32_i32_Frame_t {
#line 160 "Minus.m3"
ADDRESS _unused;
#line 160 "Minus.m3"
};
#line 160 "Minus.m3"
Word__T
__cdecl
Minus__uMinus_var_i32_i32(void)
{
#line 160 "Minus.m3"
Minus__uMinus_var_i32_i32_Frame_t _frame;
#line 160 "Minus.m3"
_frame._unused=(ADDRESS)&_frame;
#line 160 "Minus.m3"
 /* load */
#line 160 "Minus.m3"
 /* load */
#line 160 "Minus.m3"
 /* subtract */
#line 160 "Minus.m3"
 /* exit_proc */
#line 160 "Minus.m3"
return ((UINT64)(((UINT64)(((INT64)(*((INT32*)(INT64_(128)+((ADDRESS)(&Minus_m_M_Minus_L_13))))))))-((UINT64)(((INT64)(*((INT32*)(INT64_(128)+((ADDRESS)(&Minus_m_M_Minus_L_13))))))))));
#line 160 "Minus.m3"
 /* end_procedure */
#line 160 "Minus.m3"
} /* Minus_var_i32_i32 */
#line 160 "Minus.m3"
 /* set_source_line */
#line 160 "Minus.m3"
#line 161 "Minus.m3"
 /* begin_procedure */
#line 161 "Minus.m3"
struct Minus__Minus_var_i32_i32_Frame_t {
#line 161 "Minus.m3"
ADDRESS _unused;
#line 161 "Minus.m3"
};
#line 161 "Minus.m3"
Minus__INT32
__cdecl
Minus__Minus_var_i32_i32(void)
{
#line 161 "Minus.m3"
 /* Var_Type2 */ INT64 Minus_m_1212_L_1213={0};//always-init
#line 161 "Minus.m3"
Minus__Minus_var_i32_i32_Frame_t _frame;
#line 161 "Minus.m3"
_frame._unused=(ADDRESS)&_frame;
#line 161 "Minus.m3"
 /* load */
#line 161 "Minus.m3"
 /* load */
#line 161 "Minus.m3"
 /* subtract */
#line 161 "Minus.m3"
 /* check_range */
#line 161 "Minus.m3"
 /* store */
#line 161 "Minus.m3"
(*(INT64*)(&Minus_m_1212_L_1213))=(INT64)( ((INT64)( ((INT64)(*((INT32*)(INT64_(128)+((ADDRESS)(&Minus_m_M_Minus_L_13))))))- ((INT64)(*((INT32*)(INT64_(128)+((ADDRESS)(&Minus_m_M_Minus_L_13)))))))));
#line 161 "Minus.m3"
 /* load */
#line 161 "Minus.m3"
if(m3_check_range(INT64,
Minus_m_1212_L_1213,
 INT64_(-2147483648),
 INT64_(2147483647)))
#line 161 "Minus.m3"
Minus_m_M_Minus_L_13_CRASH(5153);
#line 161 "Minus.m3"
 /* exit_proc */
#line 161 "Minus.m3"
return Minus_m_1212_L_1213;
#line 161 "Minus.m3"
 /* end_procedure */
#line 161 "Minus.m3"
} /* uMinus_param_i32_i32 */
#line 161 "Minus.m3"
 /* set_source_line */
#line 161 "Minus.m3"
#line 162 "Minus.m3"
 /* begin_procedure */
#line 162 "Minus.m3"
struct Minus__uMinus_param_i32_i32_Frame_t {
#line 162 "Minus.m3"
ADDRESS _unused;
#line 162 "Minus.m3"
};
#line 162 "Minus.m3"
Word__T
__cdecl
Minus__uMinus_param_i32_i32(
   /* Param_Type1 */ Minus__INT32 a_L_230,
   /* Param_Type1 */ Minus__INT32 b_L_231)
{
#line 162 "Minus.m3"
Minus__uMinus_param_i32_i32_Frame_t _frame;
#line 162 "Minus.m3"
_frame._unused=(ADDRESS)&_frame;
#line 162 "Minus.m3"
 /* load */
#line 162 "Minus.m3"
 /* load */
#line 162 "Minus.m3"
 /* subtract */
#line 162 "Minus.m3"
 /* exit_proc */
#line 162 "Minus.m3"
return ((UINT64)(((UINT64)(((INT64)(a_L_230))))-((UINT64)(((INT64)(b_L_231))))));
#line 162 "Minus.m3"
 /* end_procedure */
#line 162 "Minus.m3"
} /* Minus_param_i32_i32 */
#line 162 "Minus.m3"
 /* set_source_line */
#line 162 "Minus.m3"
#line 163 "Minus.m3"
 /* begin_procedure */
#line 163 "Minus.m3"
struct Minus__Minus_param_i32_i32_Frame_t {
#line 163 "Minus.m3"
ADDRESS _unused;
#line 163 "Minus.m3"
};
#line 163 "Minus.m3"
Minus__INT32
__cdecl
Minus__Minus_param_i32_i32(
   /* Param_Type1 */ Minus__INT32 a_L_233,
   /* Param_Type1 */ Minus__INT32 b_L_234)
{
#line 163 "Minus.m3"
 /* Var_Type2 */ INT64 Minus_m_1214_L_1215={0};//always-init
#line 163 "Minus.m3"
Minus__Minus_param_i32_i32_Frame_t _frame;
#line 163 "Minus.m3"
_frame._unused=(ADDRESS)&_frame;
#line 163 "Minus.m3"
 /* load */
#line 163 "Minus.m3"
 /* load */
#line 163 "Minus.m3"
 /* subtract */
#line 163 "Minus.m3"
 /* check_range */
#line 163 "Minus.m3"
 /* store */
#line 163 "Minus.m3"
(*(INT64*)(&Minus_m_1214_L_1215))=(INT64)( ((INT64)( ((INT64)(a_L_233))- ((INT64)(b_L_234)))));
#line 163 "Minus.m3"
 /* load */
#line 163 "Minus.m3"
if(m3_check_range(INT64,
Minus_m_1214_L_1215,
 INT64_(-2147483648),
 INT64_(2147483647)))
#line 163 "Minus.m3"
Minus_m_M_Minus_L_13_CRASH(5217);
#line 163 "Minus.m3"
 /* exit_proc */
#line 163 "Minus.m3"
return Minus_m_1214_L_1215;
#line 163 "Minus.m3"
 /* end_procedure */
#line 163 "Minus.m3"
} /* uMinus_var_i32_LC */
#line 163 "Minus.m3"
 /* set_source_line */
#line 163 "Minus.m3"
#line 164 "Minus.m3"
 /* begin_procedure */
#line 164 "Minus.m3"
struct Minus__uMinus_var_i32_LC_Frame_t {
#line 164 "Minus.m3"
ADDRESS _unused;
#line 164 "Minus.m3"
};
#line 164 "Minus.m3"
Long__T
__cdecl
Minus__uMinus_var_i32_LC(void)
{
#line 164 "Minus.m3"
Minus__uMinus_var_i32_LC_Frame_t _frame;
#line 164 "Minus.m3"
_frame._unused=(ADDRESS)&_frame;
#line 164 "Minus.m3"
 /* load */
#line 164 "Minus.m3"
 /* loophole */
#line 164 "Minus.m3"
 /* load */
#line 164 "Minus.m3"
 /* subtract */
#line 164 "Minus.m3"
 /* exit_proc */
#line 164 "Minus.m3"
return ((UINT64)(((UINT64)((INT64)((INT64)(*((INT32*)(INT64_(128)+((ADDRESS)(&Minus_m_M_Minus_L_13))))))))-((UINT64)(((INT64)(*((UINT64*)(INT64_(136)+((ADDRESS)(&Minus_m_M_Minus_L_13))))))))));
#line 164 "Minus.m3"
 /* end_procedure */
#line 164 "Minus.m3"
} /* Minus_var_i32_LC */
#line 164 "Minus.m3"
 /* set_source_line */
#line 164 "Minus.m3"
#line 165 "Minus.m3"
 /* begin_procedure */
#line 165 "Minus.m3"
struct Minus__Minus_var_i32_LC_Frame_t {
#line 165 "Minus.m3"
ADDRESS _unused;
#line 165 "Minus.m3"
};
#line 165 "Minus.m3"
LONGINT
__cdecl
Minus__Minus_var_i32_LC(void)
{
#line 165 "Minus.m3"
Minus__Minus_var_i32_LC_Frame_t _frame;
#line 165 "Minus.m3"
_frame._unused=(ADDRESS)&_frame;
#line 165 "Minus.m3"
 /* load */
#line 165 "Minus.m3"
 /* loophole */
#line 165 "Minus.m3"
 /* load */
#line 165 "Minus.m3"
 /* subtract */
#line 165 "Minus.m3"
 /* exit_proc */
#line 165 "Minus.m3"
return ((INT64)(((INT64)((INT64)((INT64)(*((INT32*)(INT64_(128)+((ADDRESS)(&Minus_m_M_Minus_L_13))))))))- ((INT64)(*((UINT64*)(INT64_(136)+((ADDRESS)(&Minus_m_M_Minus_L_13))))))));
#line 165 "Minus.m3"
 /* end_procedure */
#line 165 "Minus.m3"
} /* uMinus_param_i32_LC */
#line 165 "Minus.m3"
 /* set_source_line */
#line 165 "Minus.m3"
#line 166 "Minus.m3"
 /* begin_procedure */
#line 166 "Minus.m3"
struct Minus__uMinus_param_i32_LC_Frame_t {
#line 166 "Minus.m3"
ADDRESS _unused;
#line 166 "Minus.m3"
};
#line 166 "Minus.m3"
Long__T
__cdecl
Minus__uMinus_param_i32_LC(
   /* Param_Type1 */ Minus__INT32 a_L_238,
   /* Param_Type1 */ LONGCARD b_L_239)
{
#line 166 "Minus.m3"
Minus__uMinus_param_i32_LC_Frame_t _frame;
#line 166 "Minus.m3"
_frame._unused=(ADDRESS)&_frame;
#line 166 "Minus.m3"
 /* load */
#line 166 "Minus.m3"
 /* loophole */
#line 166 "Minus.m3"
 /* load */
#line 166 "Minus.m3"
 /* subtract */
#line 166 "Minus.m3"
 /* exit_proc */
#line 166 "Minus.m3"
return ((UINT64)(((UINT64)((INT64)((INT64)(a_L_238))))-((UINT64)(((INT64)(b_L_239))))));
#line 166 "Minus.m3"
 /* end_procedure */
#line 166 "Minus.m3"
} /* Minus_param_i32_LC */
#line 166 "Minus.m3"
 /* set_source_line */
#line 166 "Minus.m3"
#line 167 "Minus.m3"
 /* begin_procedure */
#line 167 "Minus.m3"
struct Minus__Minus_param_i32_LC_Frame_t {
#line 167 "Minus.m3"
ADDRESS _unused;
#line 167 "Minus.m3"
};
#line 167 "Minus.m3"
LONGINT
__cdecl
Minus__Minus_param_i32_LC(
   /* Param_Type1 */ Minus__INT32 a_L_241,
   /* Param_Type1 */ LONGCARD b_L_242)
{
#line 167 "Minus.m3"
Minus__Minus_param_i32_LC_Frame_t _frame;
#line 167 "Minus.m3"
_frame._unused=(ADDRESS)&_frame;
#line 167 "Minus.m3"
 /* load */
#line 167 "Minus.m3"
 /* loophole */
#line 167 "Minus.m3"
 /* load */
#line 167 "Minus.m3"
 /* subtract */
#line 167 "Minus.m3"
 /* exit_proc */
#line 167 "Minus.m3"
return ((INT64)(((INT64)((INT64)((INT64)(a_L_241))))- ((INT64)(b_L_242))));
#line 167 "Minus.m3"
 /* end_procedure */
#line 167 "Minus.m3"
} /* uMinus_var_i32_u16 */
#line 167 "Minus.m3"
 /* set_source_line */
#line 167 "Minus.m3"
#line 168 "Minus.m3"
 /* begin_procedure */
#line 168 "Minus.m3"
struct Minus__uMinus_var_i32_u16_Frame_t {
#line 168 "Minus.m3"
ADDRESS _unused;
#line 168 "Minus.m3"
};
#line 168 "Minus.m3"
Word__T
__cdecl
Minus__uMinus_var_i32_u16(void)
{
#line 168 "Minus.m3"
Minus__uMinus_var_i32_u16_Frame_t _frame;
#line 168 "Minus.m3"
_frame._unused=(ADDRESS)&_frame;
#line 168 "Minus.m3"
 /* load */
#line 168 "Minus.m3"
 /* load */
#line 168 "Minus.m3"
 /* subtract */
#line 168 "Minus.m3"
 /* exit_proc */
#line 168 "Minus.m3"
return ((UINT64)(((UINT64)(((INT64)(*((INT32*)(INT64_(128)+((ADDRESS)(&Minus_m_M_Minus_L_13))))))))-((UINT64)(((INT64)(*((UINT16*)(INT64_(144)+((ADDRESS)(&Minus_m_M_Minus_L_13))))))))));
#line 168 "Minus.m3"
 /* end_procedure */
#line 168 "Minus.m3"
} /* Minus_var_i32_u16 */
#line 168 "Minus.m3"
 /* set_source_line */
#line 168 "Minus.m3"
#line 169 "Minus.m3"
 /* begin_procedure */
#line 169 "Minus.m3"
struct Minus__Minus_var_i32_u16_Frame_t {
#line 169 "Minus.m3"
ADDRESS _unused;
#line 169 "Minus.m3"
};
#line 169 "Minus.m3"
Minus__INT32
__cdecl
Minus__Minus_var_i32_u16(void)
{
#line 169 "Minus.m3"
 /* Var_Type2 */ INT64 Minus_m_1216_L_1217={0};//always-init
#line 169 "Minus.m3"
Minus__Minus_var_i32_u16_Frame_t _frame;
#line 169 "Minus.m3"
_frame._unused=(ADDRESS)&_frame;
#line 169 "Minus.m3"
 /* load */
#line 169 "Minus.m3"
 /* load */
#line 169 "Minus.m3"
 /* subtract */
#line 169 "Minus.m3"
 /* check_lo */
#line 169 "Minus.m3"
 /* store */
#line 169 "Minus.m3"
(*(INT64*)(&Minus_m_1216_L_1217))=(INT64)( ((INT64)( ((INT64)(*((INT32*)(INT64_(128)+((ADDRESS)(&Minus_m_M_Minus_L_13))))))- ((INT64)(*((UINT16*)(INT64_(144)+((ADDRESS)(&Minus_m_M_Minus_L_13)))))))));
#line 169 "Minus.m3"
 /* load */
#line 169 "Minus.m3"
/*check_lo*/if(Minus_m_1216_L_1217<INT64_(-2147483648))Minus_m_M_Minus_L_13_CRASH(5409);
#line 169 "Minus.m3"
 /* exit_proc */
#line 169 "Minus.m3"
return Minus_m_1216_L_1217;
#line 169 "Minus.m3"
 /* end_procedure */
#line 169 "Minus.m3"
} /* uMinus_param_i32_u16 */
#line 169 "Minus.m3"
 /* set_source_line */
#line 169 "Minus.m3"
#line 170 "Minus.m3"
 /* begin_procedure */
#line 170 "Minus.m3"
struct Minus__uMinus_param_i32_u16_Frame_t {
#line 170 "Minus.m3"
ADDRESS _unused;
#line 170 "Minus.m3"
};
#line 170 "Minus.m3"
Word__T
__cdecl
Minus__uMinus_param_i32_u16(
   /* Param_Type1 */ Minus__INT32 a_L_246,
   /* Param_Type1 */ Minus__UINT16 b_L_247)
{
#line 170 "Minus.m3"
Minus__uMinus_param_i32_u16_Frame_t _frame;
#line 170 "Minus.m3"
_frame._unused=(ADDRESS)&_frame;
#line 170 "Minus.m3"
 /* load */
#line 170 "Minus.m3"
 /* load */
#line 170 "Minus.m3"
 /* subtract */
#line 170 "Minus.m3"
 /* exit_proc */
#line 170 "Minus.m3"
return ((UINT64)(((UINT64)(((INT64)(a_L_246))))-((UINT64)(((INT64)(b_L_247))))));
#line 170 "Minus.m3"
 /* end_procedure */
#line 170 "Minus.m3"
} /* Minus_param_i32_u16 */
#line 170 "Minus.m3"
 /* set_source_line */
#line 170 "Minus.m3"
#line 171 "Minus.m3"
 /* begin_procedure */
#line 171 "Minus.m3"
struct Minus__Minus_param_i32_u16_Frame_t {
#line 171 "Minus.m3"
ADDRESS _unused;
#line 171 "Minus.m3"
};
#line 171 "Minus.m3"
Minus__INT32
__cdecl
Minus__Minus_param_i32_u16(
   /* Param_Type1 */ Minus__INT32 a_L_249,
   /* Param_Type1 */ Minus__UINT16 b_L_250)
{
#line 171 "Minus.m3"
 /* Var_Type2 */ INT64 Minus_m_1218_L_1219={0};//always-init
#line 171 "Minus.m3"
Minus__Minus_param_i32_u16_Frame_t _frame;
#line 171 "Minus.m3"
_frame._unused=(ADDRESS)&_frame;
#line 171 "Minus.m3"
 /* load */
#line 171 "Minus.m3"
 /* load */
#line 171 "Minus.m3"
 /* subtract */
#line 171 "Minus.m3"
 /* check_lo */
#line 171 "Minus.m3"
 /* store */
#line 171 "Minus.m3"
(*(INT64*)(&Minus_m_1218_L_1219))=(INT64)( ((INT64)( ((INT64)(a_L_249))- ((INT64)(b_L_250)))));
#line 171 "Minus.m3"
 /* load */
#line 171 "Minus.m3"
/*check_lo*/if(Minus_m_1218_L_1219<INT64_(-2147483648))Minus_m_M_Minus_L_13_CRASH(5473);
#line 171 "Minus.m3"
 /* exit_proc */
#line 171 "Minus.m3"
return Minus_m_1218_L_1219;
#line 171 "Minus.m3"
 /* end_procedure */
#line 171 "Minus.m3"
} /* uMinus_var_i32_I */
#line 171 "Minus.m3"
 /* set_source_line */
#line 171 "Minus.m3"
#line 172 "Minus.m3"
 /* begin_procedure */
#line 172 "Minus.m3"
struct Minus__uMinus_var_i32_I_Frame_t {
#line 172 "Minus.m3"
ADDRESS _unused;
#line 172 "Minus.m3"
};
#line 172 "Minus.m3"
Word__T
__cdecl
Minus__uMinus_var_i32_I(void)
{
#line 172 "Minus.m3"
Minus__uMinus_var_i32_I_Frame_t _frame;
#line 172 "Minus.m3"
_frame._unused=(ADDRESS)&_frame;
#line 172 "Minus.m3"
 /* load */
#line 172 "Minus.m3"
 /* load */
#line 172 "Minus.m3"
 /* subtract */
#line 172 "Minus.m3"
 /* exit_proc */
#line 172 "Minus.m3"
return ((UINT64)(((UINT64)(((INT64)(*((INT32*)(INT64_(128)+((ADDRESS)(&Minus_m_M_Minus_L_13))))))))-((UINT64)(*((INT64*)(INT64_(152)+((ADDRESS)(&Minus_m_M_Minus_L_13))))))));
#line 172 "Minus.m3"
 /* end_procedure */
#line 172 "Minus.m3"
} /* Minus_var_i32_I */
#line 172 "Minus.m3"
 /* set_source_line */
#line 172 "Minus.m3"
#line 173 "Minus.m3"
 /* begin_procedure */
#line 173 "Minus.m3"
struct Minus__Minus_var_i32_I_Frame_t {
#line 173 "Minus.m3"
ADDRESS _unused;
#line 173 "Minus.m3"
};
#line 173 "Minus.m3"
Minus__INT32
__cdecl
Minus__Minus_var_i32_I(void)
{
#line 173 "Minus.m3"
 /* Var_Type2 */ INT64 Minus_m_1220_L_1221={0};//always-init
#line 173 "Minus.m3"
Minus__Minus_var_i32_I_Frame_t _frame;
#line 173 "Minus.m3"
_frame._unused=(ADDRESS)&_frame;
#line 173 "Minus.m3"
 /* load */
#line 173 "Minus.m3"
 /* load */
#line 173 "Minus.m3"
 /* subtract */
#line 173 "Minus.m3"
 /* check_range */
#line 173 "Minus.m3"
 /* store */
#line 173 "Minus.m3"
(*(INT64*)(&Minus_m_1220_L_1221))=(INT64)( ((INT64)( ((INT64)(*((INT32*)(INT64_(128)+((ADDRESS)(&Minus_m_M_Minus_L_13))))))-((INT64)(*((INT64*)(INT64_(152)+((ADDRESS)(&Minus_m_M_Minus_L_13)))))))));
#line 173 "Minus.m3"
 /* load */
#line 173 "Minus.m3"
if(m3_check_range(INT64,
Minus_m_1220_L_1221,
 INT64_(-2147483648),
 INT64_(2147483647)))
#line 173 "Minus.m3"
Minus_m_M_Minus_L_13_CRASH(5537);
#line 173 "Minus.m3"
 /* exit_proc */
#line 173 "Minus.m3"
return Minus_m_1220_L_1221;
#line 173 "Minus.m3"
 /* end_procedure */
#line 173 "Minus.m3"
} /* uMinus_param_i32_I */
#line 173 "Minus.m3"
 /* set_source_line */
#line 173 "Minus.m3"
#line 174 "Minus.m3"
 /* begin_procedure */
#line 174 "Minus.m3"
struct Minus__uMinus_param_i32_I_Frame_t {
#line 174 "Minus.m3"
ADDRESS _unused;
#line 174 "Minus.m3"
};
#line 174 "Minus.m3"
Word__T
__cdecl
Minus__uMinus_param_i32_I(
   /* Param_Type1 */ Minus__INT32 a_L_254,
   /* Param_Type1 */ INTEGER b_L_255)
{
#line 174 "Minus.m3"
Minus__uMinus_param_i32_I_Frame_t _frame;
#line 174 "Minus.m3"
_frame._unused=(ADDRESS)&_frame;
#line 174 "Minus.m3"
 /* load */
#line 174 "Minus.m3"
 /* load */
#line 174 "Minus.m3"
 /* subtract */
#line 174 "Minus.m3"
 /* exit_proc */
#line 174 "Minus.m3"
return ((UINT64)(((UINT64)(((INT64)(a_L_254))))-((UINT64)(b_L_255))));
#line 174 "Minus.m3"
 /* end_procedure */
#line 174 "Minus.m3"
} /* Minus_param_i32_I */
#line 174 "Minus.m3"
 /* set_source_line */
#line 174 "Minus.m3"
#line 175 "Minus.m3"
 /* begin_procedure */
#line 175 "Minus.m3"
struct Minus__Minus_param_i32_I_Frame_t {
#line 175 "Minus.m3"
ADDRESS _unused;
#line 175 "Minus.m3"
};
#line 175 "Minus.m3"
Minus__INT32
__cdecl
Minus__Minus_param_i32_I(
   /* Param_Type1 */ Minus__INT32 a_L_257,
   /* Param_Type1 */ INTEGER b_L_258)
{
#line 175 "Minus.m3"
 /* Var_Type2 */ INT64 Minus_m_1222_L_1223={0};//always-init
#line 175 "Minus.m3"
Minus__Minus_param_i32_I_Frame_t _frame;
#line 175 "Minus.m3"
_frame._unused=(ADDRESS)&_frame;
#line 175 "Minus.m3"
 /* load */
#line 175 "Minus.m3"
 /* load */
#line 175 "Minus.m3"
 /* subtract */
#line 175 "Minus.m3"
 /* check_range */
#line 175 "Minus.m3"
 /* store */
#line 175 "Minus.m3"
(*(INT64*)(&Minus_m_1222_L_1223))=(INT64)( ((INT64)( ((INT64)(a_L_257))- b_L_258)));
#line 175 "Minus.m3"
 /* load */
#line 175 "Minus.m3"
if(m3_check_range(INT64,
Minus_m_1222_L_1223,
 INT64_(-2147483648),
 INT64_(2147483647)))
#line 175 "Minus.m3"
Minus_m_M_Minus_L_13_CRASH(5601);
#line 175 "Minus.m3"
 /* exit_proc */
#line 175 "Minus.m3"
return Minus_m_1222_L_1223;
#line 175 "Minus.m3"
 /* end_procedure */
#line 175 "Minus.m3"
} /* uMinus_var_i32_i64 */
#line 175 "Minus.m3"
 /* set_source_line */
#line 175 "Minus.m3"
#line 176 "Minus.m3"
 /* begin_procedure */
#line 176 "Minus.m3"
struct Minus__uMinus_var_i32_i64_Frame_t {
#line 176 "Minus.m3"
ADDRESS _unused;
#line 176 "Minus.m3"
};
#line 176 "Minus.m3"
Long__T
__cdecl
Minus__uMinus_var_i32_i64(void)
{
#line 176 "Minus.m3"
Minus__uMinus_var_i32_i64_Frame_t _frame;
#line 176 "Minus.m3"
_frame._unused=(ADDRESS)&_frame;
#line 176 "Minus.m3"
 /* load */
#line 176 "Minus.m3"
 /* loophole */
#line 176 "Minus.m3"
 /* load */
#line 176 "Minus.m3"
 /* subtract */
#line 176 "Minus.m3"
 /* exit_proc */
#line 176 "Minus.m3"
return ((UINT64)(((UINT64)((INT64)((INT64)(*((INT32*)(INT64_(128)+((ADDRESS)(&Minus_m_M_Minus_L_13))))))))-((UINT64)(*((INT64*)(INT64_(160)+((ADDRESS)(&Minus_m_M_Minus_L_13))))))));
#line 176 "Minus.m3"
 /* end_procedure */
#line 176 "Minus.m3"
} /* Minus_var_i32_i64 */
#line 176 "Minus.m3"
 /* set_source_line */
#line 176 "Minus.m3"
#line 177 "Minus.m3"
 /* begin_procedure */
#line 177 "Minus.m3"
struct Minus__Minus_var_i32_i64_Frame_t {
#line 177 "Minus.m3"
ADDRESS _unused;
#line 177 "Minus.m3"
};
#line 177 "Minus.m3"
LONGINT
__cdecl
Minus__Minus_var_i32_i64(void)
{
#line 177 "Minus.m3"
Minus__Minus_var_i32_i64_Frame_t _frame;
#line 177 "Minus.m3"
_frame._unused=(ADDRESS)&_frame;
#line 177 "Minus.m3"
 /* load */
#line 177 "Minus.m3"
 /* loophole */
#line 177 "Minus.m3"
 /* load */
#line 177 "Minus.m3"
 /* subtract */
#line 177 "Minus.m3"
 /* exit_proc */
#line 177 "Minus.m3"
return ((INT64)(((INT64)((INT64)((INT64)(*((INT32*)(INT64_(128)+((ADDRESS)(&Minus_m_M_Minus_L_13))))))))-((INT64)(*((INT64*)(INT64_(160)+((ADDRESS)(&Minus_m_M_Minus_L_13))))))));
#line 177 "Minus.m3"
 /* end_procedure */
#line 177 "Minus.m3"
} /* uMinus_param_i32_i64 */
#line 177 "Minus.m3"
 /* set_source_line */
#line 177 "Minus.m3"
#line 178 "Minus.m3"
 /* begin_procedure */
#line 178 "Minus.m3"
struct Minus__uMinus_param_i32_i64_Frame_t {
#line 178 "Minus.m3"
ADDRESS _unused;
#line 178 "Minus.m3"
};
#line 178 "Minus.m3"
Long__T
__cdecl
Minus__uMinus_param_i32_i64(
   /* Param_Type1 */ Minus__INT32 a_L_262,
   /* Param_Type1 */ Minus__INT64 b_L_263)
{
#line 178 "Minus.m3"
Minus__uMinus_param_i32_i64_Frame_t _frame;
#line 178 "Minus.m3"
_frame._unused=(ADDRESS)&_frame;
#line 178 "Minus.m3"
 /* load */
#line 178 "Minus.m3"
 /* loophole */
#line 178 "Minus.m3"
 /* load */
#line 178 "Minus.m3"
 /* subtract */
#line 178 "Minus.m3"
 /* exit_proc */
#line 178 "Minus.m3"
return ((UINT64)(((UINT64)((INT64)((INT64)(a_L_262))))-((UINT64)(b_L_263))));
#line 178 "Minus.m3"
 /* end_procedure */
#line 178 "Minus.m3"
} /* Minus_param_i32_i64 */
#line 178 "Minus.m3"
 /* set_source_line */
#line 178 "Minus.m3"
#line 179 "Minus.m3"
 /* begin_procedure */
#line 179 "Minus.m3"
struct Minus__Minus_param_i32_i64_Frame_t {
#line 179 "Minus.m3"
ADDRESS _unused;
#line 179 "Minus.m3"
};
#line 179 "Minus.m3"
LONGINT
__cdecl
Minus__Minus_param_i32_i64(
   /* Param_Type1 */ Minus__INT32 a_L_265,
   /* Param_Type1 */ Minus__INT64 b_L_266)
{
#line 179 "Minus.m3"
Minus__Minus_param_i32_i64_Frame_t _frame;
#line 179 "Minus.m3"
_frame._unused=(ADDRESS)&_frame;
#line 179 "Minus.m3"
 /* load */
#line 179 "Minus.m3"
 /* loophole */
#line 179 "Minus.m3"
 /* load */
#line 179 "Minus.m3"
 /* subtract */
#line 179 "Minus.m3"
 /* exit_proc */
#line 179 "Minus.m3"
return ((INT64)(((INT64)((INT64)((INT64)(a_L_265))))- b_L_266));
#line 179 "Minus.m3"
 /* end_procedure */
#line 179 "Minus.m3"
} /* uMinus_var_i32_i16 */
#line 179 "Minus.m3"
 /* set_source_line */
#line 179 "Minus.m3"
#line 180 "Minus.m3"
 /* begin_procedure */
#line 180 "Minus.m3"
struct Minus__uMinus_var_i32_i16_Frame_t {
#line 180 "Minus.m3"
ADDRESS _unused;
#line 180 "Minus.m3"
};
#line 180 "Minus.m3"
Word__T
__cdecl
Minus__uMinus_var_i32_i16(void)
{
#line 180 "Minus.m3"
Minus__uMinus_var_i32_i16_Frame_t _frame;
#line 180 "Minus.m3"
_frame._unused=(ADDRESS)&_frame;
#line 180 "Minus.m3"
 /* load */
#line 180 "Minus.m3"
 /* load */
#line 180 "Minus.m3"
 /* subtract */
#line 180 "Minus.m3"
 /* exit_proc */
#line 180 "Minus.m3"
return ((UINT64)(((UINT64)(((INT64)(*((INT32*)(INT64_(128)+((ADDRESS)(&Minus_m_M_Minus_L_13))))))))-((UINT64)(((INT64)(*((INT16*)(INT64_(172)+((ADDRESS)(&Minus_m_M_Minus_L_13))))))))));
#line 180 "Minus.m3"
 /* end_procedure */
#line 180 "Minus.m3"
} /* Minus_var_i32_i16 */
#line 180 "Minus.m3"
 /* set_source_line */
#line 180 "Minus.m3"
#line 181 "Minus.m3"
 /* begin_procedure */
#line 181 "Minus.m3"
struct Minus__Minus_var_i32_i16_Frame_t {
#line 181 "Minus.m3"
ADDRESS _unused;
#line 181 "Minus.m3"
};
#line 181 "Minus.m3"
Minus__INT32
__cdecl
Minus__Minus_var_i32_i16(void)
{
#line 181 "Minus.m3"
 /* Var_Type2 */ INT64 Minus_m_1224_L_1225={0};//always-init
#line 181 "Minus.m3"
Minus__Minus_var_i32_i16_Frame_t _frame;
#line 181 "Minus.m3"
_frame._unused=(ADDRESS)&_frame;
#line 181 "Minus.m3"
 /* load */
#line 181 "Minus.m3"
 /* load */
#line 181 "Minus.m3"
 /* subtract */
#line 181 "Minus.m3"
 /* check_range */
#line 181 "Minus.m3"
 /* store */
#line 181 "Minus.m3"
(*(INT64*)(&Minus_m_1224_L_1225))=(INT64)( ((INT64)( ((INT64)(*((INT32*)(INT64_(128)+((ADDRESS)(&Minus_m_M_Minus_L_13))))))- ((INT64)(*((INT16*)(INT64_(172)+((ADDRESS)(&Minus_m_M_Minus_L_13)))))))));
#line 181 "Minus.m3"
 /* load */
#line 181 "Minus.m3"
if(m3_check_range(INT64,
Minus_m_1224_L_1225,
 INT64_(-2147483648),
 INT64_(2147483647)))
#line 181 "Minus.m3"
Minus_m_M_Minus_L_13_CRASH(5793);
#line 181 "Minus.m3"
 /* exit_proc */
#line 181 "Minus.m3"
return Minus_m_1224_L_1225;
#line 181 "Minus.m3"
 /* end_procedure */
#line 181 "Minus.m3"
} /* uMinus_param_i32_i16 */
#line 181 "Minus.m3"
 /* set_source_line */
#line 181 "Minus.m3"
#line 182 "Minus.m3"
 /* begin_procedure */
#line 182 "Minus.m3"
struct Minus__uMinus_param_i32_i16_Frame_t {
#line 182 "Minus.m3"
ADDRESS _unused;
#line 182 "Minus.m3"
};
#line 182 "Minus.m3"
Word__T
__cdecl
Minus__uMinus_param_i32_i16(
   /* Param_Type1 */ Minus__INT32 a_L_270,
   /* Param_Type1 */ Minus__INT16 b_L_271)
{
#line 182 "Minus.m3"
Minus__uMinus_param_i32_i16_Frame_t _frame;
#line 182 "Minus.m3"
_frame._unused=(ADDRESS)&_frame;
#line 182 "Minus.m3"
 /* load */
#line 182 "Minus.m3"
 /* load */
#line 182 "Minus.m3"
 /* subtract */
#line 182 "Minus.m3"
 /* exit_proc */
#line 182 "Minus.m3"
return ((UINT64)(((UINT64)(((INT64)(a_L_270))))-((UINT64)(((INT64)(b_L_271))))));
#line 182 "Minus.m3"
 /* end_procedure */
#line 182 "Minus.m3"
} /* Minus_param_i32_i16 */
#line 182 "Minus.m3"
 /* set_source_line */
#line 182 "Minus.m3"
#line 183 "Minus.m3"
 /* begin_procedure */
#line 183 "Minus.m3"
struct Minus__Minus_param_i32_i16_Frame_t {
#line 183 "Minus.m3"
ADDRESS _unused;
#line 183 "Minus.m3"
};
#line 183 "Minus.m3"
Minus__INT32
__cdecl
Minus__Minus_param_i32_i16(
   /* Param_Type1 */ Minus__INT32 a_L_273,
   /* Param_Type1 */ Minus__INT16 b_L_274)
{
#line 183 "Minus.m3"
 /* Var_Type2 */ INT64 Minus_m_1226_L_1227={0};//always-init
#line 183 "Minus.m3"
Minus__Minus_param_i32_i16_Frame_t _frame;
#line 183 "Minus.m3"
_frame._unused=(ADDRESS)&_frame;
#line 183 "Minus.m3"
 /* load */
#line 183 "Minus.m3"
 /* load */
#line 183 "Minus.m3"
 /* subtract */
#line 183 "Minus.m3"
 /* check_range */
#line 183 "Minus.m3"
 /* store */
#line 183 "Minus.m3"
(*(INT64*)(&Minus_m_1226_L_1227))=(INT64)( ((INT64)( ((INT64)(a_L_273))- ((INT64)(b_L_274)))));
#line 183 "Minus.m3"
 /* load */
#line 183 "Minus.m3"
if(m3_check_range(INT64,
Minus_m_1226_L_1227,
 INT64_(-2147483648),
 INT64_(2147483647)))
#line 183 "Minus.m3"
Minus_m_M_Minus_L_13_CRASH(5857);
#line 183 "Minus.m3"
 /* exit_proc */
#line 183 "Minus.m3"
return Minus_m_1226_L_1227;
#line 183 "Minus.m3"
 /* end_procedure */
#line 183 "Minus.m3"
} /* uMinus_var_i32_C */
#line 183 "Minus.m3"
 /* set_source_line */
#line 183 "Minus.m3"
#line 184 "Minus.m3"
 /* begin_procedure */
#line 184 "Minus.m3"
struct Minus__uMinus_var_i32_C_Frame_t {
#line 184 "Minus.m3"
ADDRESS _unused;
#line 184 "Minus.m3"
};
#line 184 "Minus.m3"
Word__T
__cdecl
Minus__uMinus_var_i32_C(void)
{
#line 184 "Minus.m3"
Minus__uMinus_var_i32_C_Frame_t _frame;
#line 184 "Minus.m3"
_frame._unused=(ADDRESS)&_frame;
#line 184 "Minus.m3"
 /* load */
#line 184 "Minus.m3"
 /* load */
#line 184 "Minus.m3"
 /* subtract */
#line 184 "Minus.m3"
 /* exit_proc */
#line 184 "Minus.m3"
return ((UINT64)(((UINT64)(((INT64)(*((INT32*)(INT64_(128)+((ADDRESS)(&Minus_m_M_Minus_L_13))))))))-((UINT64)(((INT64)(*((UINT64*)(INT64_(176)+((ADDRESS)(&Minus_m_M_Minus_L_13))))))))));
#line 184 "Minus.m3"
 /* end_procedure */
#line 184 "Minus.m3"
} /* Minus_var_i32_C */
#line 184 "Minus.m3"
 /* set_source_line */
#line 184 "Minus.m3"
#line 185 "Minus.m3"
 /* begin_procedure */
#line 185 "Minus.m3"
struct Minus__Minus_var_i32_C_Frame_t {
#line 185 "Minus.m3"
ADDRESS _unused;
#line 185 "Minus.m3"
};
#line 185 "Minus.m3"
Minus__INT32
__cdecl
Minus__Minus_var_i32_C(void)
{
#line 185 "Minus.m3"
 /* Var_Type2 */ INT64 Minus_m_1228_L_1229={0};//always-init
#line 185 "Minus.m3"
Minus__Minus_var_i32_C_Frame_t _frame;
#line 185 "Minus.m3"
_frame._unused=(ADDRESS)&_frame;
#line 185 "Minus.m3"
 /* load */
#line 185 "Minus.m3"
 /* load */
#line 185 "Minus.m3"
 /* subtract */
#line 185 "Minus.m3"
 /* check_range */
#line 185 "Minus.m3"
 /* store */
#line 185 "Minus.m3"
(*(INT64*)(&Minus_m_1228_L_1229))=(INT64)( ((INT64)( ((INT64)(*((INT32*)(INT64_(128)+((ADDRESS)(&Minus_m_M_Minus_L_13))))))- ((INT64)(*((UINT64*)(INT64_(176)+((ADDRESS)(&Minus_m_M_Minus_L_13)))))))));
#line 185 "Minus.m3"
 /* load */
#line 185 "Minus.m3"
if(m3_check_range(INT64,
Minus_m_1228_L_1229,
 INT64_(-2147483648),
 INT64_(2147483647)))
#line 185 "Minus.m3"
Minus_m_M_Minus_L_13_CRASH(5921);
#line 185 "Minus.m3"
 /* exit_proc */
#line 185 "Minus.m3"
return Minus_m_1228_L_1229;
#line 185 "Minus.m3"
 /* end_procedure */
#line 185 "Minus.m3"
} /* uMinus_param_i32_C */
#line 185 "Minus.m3"
 /* set_source_line */
#line 185 "Minus.m3"
#line 186 "Minus.m3"
 /* begin_procedure */
#line 186 "Minus.m3"
struct Minus__uMinus_param_i32_C_Frame_t {
#line 186 "Minus.m3"
ADDRESS _unused;
#line 186 "Minus.m3"
};
#line 186 "Minus.m3"
Word__T
__cdecl
Minus__uMinus_param_i32_C(
   /* Param_Type1 */ Minus__INT32 a_L_278,
   /* Param_Type1 */ CARDINAL b_L_279)
{
#line 186 "Minus.m3"
Minus__uMinus_param_i32_C_Frame_t _frame;
#line 186 "Minus.m3"
_frame._unused=(ADDRESS)&_frame;
#line 186 "Minus.m3"
 /* load */
#line 186 "Minus.m3"
 /* load */
#line 186 "Minus.m3"
 /* subtract */
#line 186 "Minus.m3"
 /* exit_proc */
#line 186 "Minus.m3"
return ((UINT64)(((UINT64)(((INT64)(a_L_278))))-((UINT64)(((INT64)(b_L_279))))));
#line 186 "Minus.m3"
 /* end_procedure */
#line 186 "Minus.m3"
} /* Minus_param_i32_C */
#line 186 "Minus.m3"
 /* set_source_line */
#line 186 "Minus.m3"
#line 187 "Minus.m3"
 /* begin_procedure */
#line 187 "Minus.m3"
struct Minus__Minus_param_i32_C_Frame_t {
#line 187 "Minus.m3"
ADDRESS _unused;
#line 187 "Minus.m3"
};
#line 187 "Minus.m3"
Minus__INT32
__cdecl
Minus__Minus_param_i32_C(
   /* Param_Type1 */ Minus__INT32 a_L_281,
   /* Param_Type1 */ CARDINAL b_L_282)
{
#line 187 "Minus.m3"
 /* Var_Type2 */ INT64 Minus_m_1230_L_1231={0};//always-init
#line 187 "Minus.m3"
Minus__Minus_param_i32_C_Frame_t _frame;
#line 187 "Minus.m3"
_frame._unused=(ADDRESS)&_frame;
#line 187 "Minus.m3"
 /* load */
#line 187 "Minus.m3"
 /* load */
#line 187 "Minus.m3"
 /* subtract */
#line 187 "Minus.m3"
 /* check_range */
#line 187 "Minus.m3"
 /* store */
#line 187 "Minus.m3"
(*(INT64*)(&Minus_m_1230_L_1231))=(INT64)( ((INT64)( ((INT64)(a_L_281))- ((INT64)(b_L_282)))));
#line 187 "Minus.m3"
 /* load */
#line 187 "Minus.m3"
if(m3_check_range(INT64,
Minus_m_1230_L_1231,
 INT64_(-2147483648),
 INT64_(2147483647)))
#line 187 "Minus.m3"
Minus_m_M_Minus_L_13_CRASH(5985);
#line 187 "Minus.m3"
 /* exit_proc */
#line 187 "Minus.m3"
return Minus_m_1230_L_1231;
#line 187 "Minus.m3"
 /* end_procedure */
#line 187 "Minus.m3"
} /* uMinus_var_i32_u32 */
#line 187 "Minus.m3"
 /* set_source_line */
#line 187 "Minus.m3"
#line 188 "Minus.m3"
 /* begin_procedure */
#line 188 "Minus.m3"
struct Minus__uMinus_var_i32_u32_Frame_t {
#line 188 "Minus.m3"
ADDRESS _unused;
#line 188 "Minus.m3"
};
#line 188 "Minus.m3"
Word__T
__cdecl
Minus__uMinus_var_i32_u32(void)
{
#line 188 "Minus.m3"
Minus__uMinus_var_i32_u32_Frame_t _frame;
#line 188 "Minus.m3"
_frame._unused=(ADDRESS)&_frame;
#line 188 "Minus.m3"
 /* load */
#line 188 "Minus.m3"
 /* load */
#line 188 "Minus.m3"
 /* subtract */
#line 188 "Minus.m3"
 /* exit_proc */
#line 188 "Minus.m3"
return ((UINT64)(((UINT64)(((INT64)(*((INT32*)(INT64_(128)+((ADDRESS)(&Minus_m_M_Minus_L_13))))))))-((UINT64)(((INT64)(*((UINT32*)(INT64_(184)+((ADDRESS)(&Minus_m_M_Minus_L_13))))))))));
#line 188 "Minus.m3"
 /* end_procedure */
#line 188 "Minus.m3"
} /* Minus_var_i32_u32 */
#line 188 "Minus.m3"
 /* set_source_line */
#line 188 "Minus.m3"
#line 189 "Minus.m3"
 /* begin_procedure */
#line 189 "Minus.m3"
struct Minus__Minus_var_i32_u32_Frame_t {
#line 189 "Minus.m3"
ADDRESS _unused;
#line 189 "Minus.m3"
};
#line 189 "Minus.m3"
Minus__INT32
__cdecl
Minus__Minus_var_i32_u32(void)
{
#line 189 "Minus.m3"
 /* Var_Type2 */ INT64 Minus_m_1232_L_1233={0};//always-init
#line 189 "Minus.m3"
Minus__Minus_var_i32_u32_Frame_t _frame;
#line 189 "Minus.m3"
_frame._unused=(ADDRESS)&_frame;
#line 189 "Minus.m3"
 /* load */
#line 189 "Minus.m3"
 /* load */
#line 189 "Minus.m3"
 /* subtract */
#line 189 "Minus.m3"
 /* check_lo */
#line 189 "Minus.m3"
 /* store */
#line 189 "Minus.m3"
(*(INT64*)(&Minus_m_1232_L_1233))=(INT64)( ((INT64)( ((INT64)(*((INT32*)(INT64_(128)+((ADDRESS)(&Minus_m_M_Minus_L_13))))))- ((INT64)(*((UINT32*)(INT64_(184)+((ADDRESS)(&Minus_m_M_Minus_L_13)))))))));
#line 189 "Minus.m3"
 /* load */
#line 189 "Minus.m3"
/*check_lo*/if(Minus_m_1232_L_1233<INT64_(-2147483648))Minus_m_M_Minus_L_13_CRASH(6049);
#line 189 "Minus.m3"
 /* exit_proc */
#line 189 "Minus.m3"
return Minus_m_1232_L_1233;
#line 189 "Minus.m3"
 /* end_procedure */
#line 189 "Minus.m3"
} /* uMinus_param_i32_u32 */
#line 189 "Minus.m3"
 /* set_source_line */
#line 189 "Minus.m3"
#line 190 "Minus.m3"
 /* begin_procedure */
#line 190 "Minus.m3"
struct Minus__uMinus_param_i32_u32_Frame_t {
#line 190 "Minus.m3"
ADDRESS _unused;
#line 190 "Minus.m3"
};
#line 190 "Minus.m3"
Word__T
__cdecl
Minus__uMinus_param_i32_u32(
   /* Param_Type1 */ Minus__INT32 a_L_286,
   /* Param_Type1 */ Minus__UINT32 b_L_287)
{
#line 190 "Minus.m3"
Minus__uMinus_param_i32_u32_Frame_t _frame;
#line 190 "Minus.m3"
_frame._unused=(ADDRESS)&_frame;
#line 190 "Minus.m3"
 /* load */
#line 190 "Minus.m3"
 /* load */
#line 190 "Minus.m3"
 /* subtract */
#line 190 "Minus.m3"
 /* exit_proc */
#line 190 "Minus.m3"
return ((UINT64)(((UINT64)(((INT64)(a_L_286))))-((UINT64)(((INT64)(b_L_287))))));
#line 190 "Minus.m3"
 /* end_procedure */
#line 190 "Minus.m3"
} /* Minus_param_i32_u32 */
#line 190 "Minus.m3"
 /* set_source_line */
#line 190 "Minus.m3"
#line 191 "Minus.m3"
 /* begin_procedure */
#line 191 "Minus.m3"
struct Minus__Minus_param_i32_u32_Frame_t {
#line 191 "Minus.m3"
ADDRESS _unused;
#line 191 "Minus.m3"
};
#line 191 "Minus.m3"
Minus__INT32
__cdecl
Minus__Minus_param_i32_u32(
   /* Param_Type1 */ Minus__INT32 a_L_289,
   /* Param_Type1 */ Minus__UINT32 b_L_290)
{
#line 191 "Minus.m3"
 /* Var_Type2 */ INT64 Minus_m_1234_L_1235={0};//always-init
#line 191 "Minus.m3"
Minus__Minus_param_i32_u32_Frame_t _frame;
#line 191 "Minus.m3"
_frame._unused=(ADDRESS)&_frame;
#line 191 "Minus.m3"
 /* load */
#line 191 "Minus.m3"
 /* load */
#line 191 "Minus.m3"
 /* subtract */
#line 191 "Minus.m3"
 /* check_lo */
#line 191 "Minus.m3"
 /* store */
#line 191 "Minus.m3"
(*(INT64*)(&Minus_m_1234_L_1235))=(INT64)( ((INT64)( ((INT64)(a_L_289))- ((INT64)(b_L_290)))));
#line 191 "Minus.m3"
 /* load */
#line 191 "Minus.m3"
/*check_lo*/if(Minus_m_1234_L_1235<INT64_(-2147483648))Minus_m_M_Minus_L_13_CRASH(6113);
#line 191 "Minus.m3"
 /* exit_proc */
#line 191 "Minus.m3"
return Minus_m_1234_L_1235;
#line 191 "Minus.m3"
 /* end_procedure */
#line 191 "Minus.m3"
} /* uMinus_var_i32_u8 */
#line 191 "Minus.m3"
 /* set_source_line */
#line 191 "Minus.m3"
#line 192 "Minus.m3"
 /* begin_procedure */
#line 192 "Minus.m3"
struct Minus__uMinus_var_i32_u8_Frame_t {
#line 192 "Minus.m3"
ADDRESS _unused;
#line 192 "Minus.m3"
};
#line 192 "Minus.m3"
Word__T
__cdecl
Minus__uMinus_var_i32_u8(void)
{
#line 192 "Minus.m3"
Minus__uMinus_var_i32_u8_Frame_t _frame;
#line 192 "Minus.m3"
_frame._unused=(ADDRESS)&_frame;
#line 192 "Minus.m3"
 /* load */
#line 192 "Minus.m3"
 /* load */
#line 192 "Minus.m3"
 /* subtract */
#line 192 "Minus.m3"
 /* exit_proc */
#line 192 "Minus.m3"
return ((UINT64)(((UINT64)(((INT64)(*((INT32*)(INT64_(128)+((ADDRESS)(&Minus_m_M_Minus_L_13))))))))-((UINT64)(((INT64)(*((UINT8*)(INT64_(188)+((ADDRESS)(&Minus_m_M_Minus_L_13))))))))));
#line 192 "Minus.m3"
 /* end_procedure */
#line 192 "Minus.m3"
} /* Minus_var_i32_u8 */
#line 192 "Minus.m3"
 /* set_source_line */
#line 192 "Minus.m3"
#line 193 "Minus.m3"
 /* begin_procedure */
#line 193 "Minus.m3"
struct Minus__Minus_var_i32_u8_Frame_t {
#line 193 "Minus.m3"
ADDRESS _unused;
#line 193 "Minus.m3"
};
#line 193 "Minus.m3"
Minus__INT32
__cdecl
Minus__Minus_var_i32_u8(void)
{
#line 193 "Minus.m3"
 /* Var_Type2 */ INT64 Minus_m_1236_L_1237={0};//always-init
#line 193 "Minus.m3"
Minus__Minus_var_i32_u8_Frame_t _frame;
#line 193 "Minus.m3"
_frame._unused=(ADDRESS)&_frame;
#line 193 "Minus.m3"
 /* load */
#line 193 "Minus.m3"
 /* load */
#line 193 "Minus.m3"
 /* subtract */
#line 193 "Minus.m3"
 /* check_lo */
#line 193 "Minus.m3"
 /* store */
#line 193 "Minus.m3"
(*(INT64*)(&Minus_m_1236_L_1237))=(INT64)( ((INT64)( ((INT64)(*((INT32*)(INT64_(128)+((ADDRESS)(&Minus_m_M_Minus_L_13))))))- ((INT64)(*((UINT8*)(INT64_(188)+((ADDRESS)(&Minus_m_M_Minus_L_13)))))))));
#line 193 "Minus.m3"
 /* load */
#line 193 "Minus.m3"
/*check_lo*/if(Minus_m_1236_L_1237<INT64_(-2147483648))Minus_m_M_Minus_L_13_CRASH(6177);
#line 193 "Minus.m3"
 /* exit_proc */
#line 193 "Minus.m3"
return Minus_m_1236_L_1237;
#line 193 "Minus.m3"
 /* end_procedure */
#line 193 "Minus.m3"
} /* uMinus_param_i32_u8 */
#line 193 "Minus.m3"
 /* set_source_line */
#line 193 "Minus.m3"
#line 194 "Minus.m3"
 /* begin_procedure */
#line 194 "Minus.m3"
struct Minus__uMinus_param_i32_u8_Frame_t {
#line 194 "Minus.m3"
ADDRESS _unused;
#line 194 "Minus.m3"
};
#line 194 "Minus.m3"
Word__T
__cdecl
Minus__uMinus_param_i32_u8(
   /* Param_Type1 */ Minus__INT32 a_L_294,
   /* Param_Type1 */ Minus__UINT8 b_L_295)
{
#line 194 "Minus.m3"
Minus__uMinus_param_i32_u8_Frame_t _frame;
#line 194 "Minus.m3"
_frame._unused=(ADDRESS)&_frame;
#line 194 "Minus.m3"
 /* load */
#line 194 "Minus.m3"
 /* load */
#line 194 "Minus.m3"
 /* subtract */
#line 194 "Minus.m3"
 /* exit_proc */
#line 194 "Minus.m3"
return ((UINT64)(((UINT64)(((INT64)(a_L_294))))-((UINT64)(((INT64)(b_L_295))))));
#line 194 "Minus.m3"
 /* end_procedure */
#line 194 "Minus.m3"
} /* Minus_param_i32_u8 */
#line 194 "Minus.m3"
 /* set_source_line */
#line 194 "Minus.m3"
#line 195 "Minus.m3"
 /* begin_procedure */
#line 195 "Minus.m3"
struct Minus__Minus_param_i32_u8_Frame_t {
#line 195 "Minus.m3"
ADDRESS _unused;
#line 195 "Minus.m3"
};
#line 195 "Minus.m3"
Minus__INT32
__cdecl
Minus__Minus_param_i32_u8(
   /* Param_Type1 */ Minus__INT32 a_L_297,
   /* Param_Type1 */ Minus__UINT8 b_L_298)
{
#line 195 "Minus.m3"
 /* Var_Type2 */ INT64 Minus_m_1238_L_1239={0};//always-init
#line 195 "Minus.m3"
Minus__Minus_param_i32_u8_Frame_t _frame;
#line 195 "Minus.m3"
_frame._unused=(ADDRESS)&_frame;
#line 195 "Minus.m3"
 /* load */
#line 195 "Minus.m3"
 /* load */
#line 195 "Minus.m3"
 /* subtract */
#line 195 "Minus.m3"
 /* check_lo */
#line 195 "Minus.m3"
 /* store */
#line 195 "Minus.m3"
(*(INT64*)(&Minus_m_1238_L_1239))=(INT64)( ((INT64)( ((INT64)(a_L_297))- ((INT64)(b_L_298)))));
#line 195 "Minus.m3"
 /* load */
#line 195 "Minus.m3"
/*check_lo*/if(Minus_m_1238_L_1239<INT64_(-2147483648))Minus_m_M_Minus_L_13_CRASH(6241);
#line 195 "Minus.m3"
 /* exit_proc */
#line 195 "Minus.m3"
return Minus_m_1238_L_1239;
#line 195 "Minus.m3"
 /* end_procedure */
#line 195 "Minus.m3"
} /* uMinus_var_i32_L */
#line 195 "Minus.m3"
 /* set_source_line */
#line 195 "Minus.m3"
#line 196 "Minus.m3"
 /* begin_procedure */
#line 196 "Minus.m3"
struct Minus__uMinus_var_i32_L_Frame_t {
#line 196 "Minus.m3"
ADDRESS _unused;
#line 196 "Minus.m3"
};
#line 196 "Minus.m3"
Long__T
__cdecl
Minus__uMinus_var_i32_L(void)
{
#line 196 "Minus.m3"
Minus__uMinus_var_i32_L_Frame_t _frame;
#line 196 "Minus.m3"
_frame._unused=(ADDRESS)&_frame;
#line 196 "Minus.m3"
 /* load */
#line 196 "Minus.m3"
 /* loophole */
#line 196 "Minus.m3"
 /* load */
#line 196 "Minus.m3"
 /* subtract */
#line 196 "Minus.m3"
 /* exit_proc */
#line 196 "Minus.m3"
return ((UINT64)(((UINT64)((INT64)((INT64)(*((INT32*)(INT64_(128)+((ADDRESS)(&Minus_m_M_Minus_L_13))))))))-((UINT64)(*((INT64*)(INT64_(192)+((ADDRESS)(&Minus_m_M_Minus_L_13))))))));
#line 196 "Minus.m3"
 /* end_procedure */
#line 196 "Minus.m3"
} /* Minus_var_i32_L */
#line 196 "Minus.m3"
 /* set_source_line */
#line 196 "Minus.m3"
#line 197 "Minus.m3"
 /* begin_procedure */
#line 197 "Minus.m3"
struct Minus__Minus_var_i32_L_Frame_t {
#line 197 "Minus.m3"
ADDRESS _unused;
#line 197 "Minus.m3"
};
#line 197 "Minus.m3"
LONGINT
__cdecl
Minus__Minus_var_i32_L(void)
{
#line 197 "Minus.m3"
Minus__Minus_var_i32_L_Frame_t _frame;
#line 197 "Minus.m3"
_frame._unused=(ADDRESS)&_frame;
#line 197 "Minus.m3"
 /* load */
#line 197 "Minus.m3"
 /* loophole */
#line 197 "Minus.m3"
 /* load */
#line 197 "Minus.m3"
 /* subtract */
#line 197 "Minus.m3"
 /* exit_proc */
#line 197 "Minus.m3"
return ((INT64)(((INT64)((INT64)((INT64)(*((INT32*)(INT64_(128)+((ADDRESS)(&Minus_m_M_Minus_L_13))))))))-((INT64)(*((INT64*)(INT64_(192)+((ADDRESS)(&Minus_m_M_Minus_L_13))))))));
#line 197 "Minus.m3"
 /* end_procedure */
#line 197 "Minus.m3"
} /* uMinus_param_i32_L */
#line 197 "Minus.m3"
 /* set_source_line */
#line 197 "Minus.m3"
#line 198 "Minus.m3"
 /* begin_procedure */
#line 198 "Minus.m3"
struct Minus__uMinus_param_i32_L_Frame_t {
#line 198 "Minus.m3"
ADDRESS _unused;
#line 198 "Minus.m3"
};
#line 198 "Minus.m3"
Long__T
__cdecl
Minus__uMinus_param_i32_L(
   /* Param_Type1 */ Minus__INT32 a_L_302,
   /* Param_Type1 */ LONGINT b_L_303)
{
#line 198 "Minus.m3"
Minus__uMinus_param_i32_L_Frame_t _frame;
#line 198 "Minus.m3"
_frame._unused=(ADDRESS)&_frame;
#line 198 "Minus.m3"
 /* load */
#line 198 "Minus.m3"
 /* loophole */
#line 198 "Minus.m3"
 /* load */
#line 198 "Minus.m3"
 /* subtract */
#line 198 "Minus.m3"
 /* exit_proc */
#line 198 "Minus.m3"
return ((UINT64)(((UINT64)((INT64)((INT64)(a_L_302))))-((UINT64)(b_L_303))));
#line 198 "Minus.m3"
 /* end_procedure */
#line 198 "Minus.m3"
} /* Minus_param_i32_L */
#line 198 "Minus.m3"
 /* set_source_line */
#line 198 "Minus.m3"
#line 199 "Minus.m3"
 /* begin_procedure */
#line 199 "Minus.m3"
struct Minus__Minus_param_i32_L_Frame_t {
#line 199 "Minus.m3"
ADDRESS _unused;
#line 199 "Minus.m3"
};
#line 199 "Minus.m3"
LONGINT
__cdecl
Minus__Minus_param_i32_L(
   /* Param_Type1 */ Minus__INT32 a_L_305,
   /* Param_Type1 */ LONGINT b_L_306)
{
#line 199 "Minus.m3"
Minus__Minus_param_i32_L_Frame_t _frame;
#line 199 "Minus.m3"
_frame._unused=(ADDRESS)&_frame;
#line 199 "Minus.m3"
 /* load */
#line 199 "Minus.m3"
 /* loophole */
#line 199 "Minus.m3"
 /* load */
#line 199 "Minus.m3"
 /* subtract */
#line 199 "Minus.m3"
 /* exit_proc */
#line 199 "Minus.m3"
return ((INT64)(((INT64)((INT64)((INT64)(a_L_305))))- b_L_306));
#line 199 "Minus.m3"
 /* end_procedure */
#line 199 "Minus.m3"
} /* uMinus_var_LC_i8 */
#line 199 "Minus.m3"
 /* set_source_line */
#line 199 "Minus.m3"
#line 200 "Minus.m3"
 /* begin_procedure */
#line 200 "Minus.m3"
struct Minus__uMinus_var_LC_i8_Frame_t {
#line 200 "Minus.m3"
ADDRESS _unused;
#line 200 "Minus.m3"
};
#line 200 "Minus.m3"
Long__T
__cdecl
Minus__uMinus_var_LC_i8(void)
{
#line 200 "Minus.m3"
Minus__uMinus_var_LC_i8_Frame_t _frame;
#line 200 "Minus.m3"
_frame._unused=(ADDRESS)&_frame;
#line 200 "Minus.m3"
 /* load */
#line 200 "Minus.m3"
 /* loophole */
#line 200 "Minus.m3"
 /* load */
#line 200 "Minus.m3"
 /* swap */
#line 200 "Minus.m3"
 /* subtract */
#line 200 "Minus.m3"
 /* exit_proc */
#line 200 "Minus.m3"
return ((UINT64)(((UINT64)(((INT64)(*((UINT64*)(INT64_(136)+((ADDRESS)(&Minus_m_M_Minus_L_13))))))))-((UINT64)((INT64)((INT64)(*((INT8*)(INT64_(104)+((ADDRESS)(&Minus_m_M_Minus_L_13))))))))));
#line 200 "Minus.m3"
 /* end_procedure */
#line 200 "Minus.m3"
} /* Minus_var_LC_i8 */
#line 200 "Minus.m3"
 /* set_source_line */
#line 200 "Minus.m3"
#line 201 "Minus.m3"
 /* begin_procedure */
#line 201 "Minus.m3"
struct Minus__Minus_var_LC_i8_Frame_t {
#line 201 "Minus.m3"
ADDRESS _unused;
#line 201 "Minus.m3"
};
#line 201 "Minus.m3"
LONGINT
__cdecl
Minus__Minus_var_LC_i8(void)
{
#line 201 "Minus.m3"
Minus__Minus_var_LC_i8_Frame_t _frame;
#line 201 "Minus.m3"
_frame._unused=(ADDRESS)&_frame;
#line 201 "Minus.m3"
 /* load */
#line 201 "Minus.m3"
 /* loophole */
#line 201 "Minus.m3"
 /* load */
#line 201 "Minus.m3"
 /* swap */
#line 201 "Minus.m3"
 /* subtract */
#line 201 "Minus.m3"
 /* exit_proc */
#line 201 "Minus.m3"
return ((INT64)( ((INT64)(*((UINT64*)(INT64_(136)+((ADDRESS)(&Minus_m_M_Minus_L_13))))))-((INT64)((INT64)((INT64)(*((INT8*)(INT64_(104)+((ADDRESS)(&Minus_m_M_Minus_L_13))))))))));
#line 201 "Minus.m3"
 /* end_procedure */
#line 201 "Minus.m3"
} /* uMinus_param_LC_i8 */
#line 201 "Minus.m3"
 /* set_source_line */
#line 201 "Minus.m3"
#line 202 "Minus.m3"
 /* begin_procedure */
#line 202 "Minus.m3"
struct Minus__uMinus_param_LC_i8_Frame_t {
#line 202 "Minus.m3"
ADDRESS _unused;
#line 202 "Minus.m3"
};
#line 202 "Minus.m3"
Long__T
__cdecl
Minus__uMinus_param_LC_i8(
   /* Param_Type1 */ LONGCARD a_L_310,
   /* Param_Type1 */ Minus__INT8 b_L_311)
{
#line 202 "Minus.m3"
Minus__uMinus_param_LC_i8_Frame_t _frame;
#line 202 "Minus.m3"
_frame._unused=(ADDRESS)&_frame;
#line 202 "Minus.m3"
 /* load */
#line 202 "Minus.m3"
 /* loophole */
#line 202 "Minus.m3"
 /* load */
#line 202 "Minus.m3"
 /* swap */
#line 202 "Minus.m3"
 /* subtract */
#line 202 "Minus.m3"
 /* exit_proc */
#line 202 "Minus.m3"
return ((UINT64)(((UINT64)(((INT64)(a_L_310))))-((UINT64)((INT64)((INT64)(b_L_311))))));
#line 202 "Minus.m3"
 /* end_procedure */
#line 202 "Minus.m3"
} /* Minus_param_LC_i8 */
#line 202 "Minus.m3"
 /* set_source_line */
#line 202 "Minus.m3"
#line 203 "Minus.m3"
 /* begin_procedure */
#line 203 "Minus.m3"
struct Minus__Minus_param_LC_i8_Frame_t {
#line 203 "Minus.m3"
ADDRESS _unused;
#line 203 "Minus.m3"
};
#line 203 "Minus.m3"
LONGINT
__cdecl
Minus__Minus_param_LC_i8(
   /* Param_Type1 */ LONGCARD a_L_313,
   /* Param_Type1 */ Minus__INT8 b_L_314)
{
#line 203 "Minus.m3"
Minus__Minus_param_LC_i8_Frame_t _frame;
#line 203 "Minus.m3"
_frame._unused=(ADDRESS)&_frame;
#line 203 "Minus.m3"
 /* load */
#line 203 "Minus.m3"
 /* loophole */
#line 203 "Minus.m3"
 /* load */
#line 203 "Minus.m3"
 /* swap */
#line 203 "Minus.m3"
 /* subtract */
#line 203 "Minus.m3"
 /* exit_proc */
#line 203 "Minus.m3"
return ((INT64)( ((INT64)(a_L_313))-((INT64)((INT64)((INT64)(b_L_314))))));
#line 203 "Minus.m3"
 /* end_procedure */
#line 203 "Minus.m3"
} /* uMinus_var_LC_u64 */
#line 203 "Minus.m3"
 /* set_source_line */
#line 203 "Minus.m3"
#line 204 "Minus.m3"
 /* begin_procedure */
#line 204 "Minus.m3"
struct Minus__uMinus_var_LC_u64_Frame_t {
#line 204 "Minus.m3"
ADDRESS _unused;
#line 204 "Minus.m3"
};
#line 204 "Minus.m3"
Long__T
__cdecl
Minus__uMinus_var_LC_u64(void)
{
#line 204 "Minus.m3"
Minus__uMinus_var_LC_u64_Frame_t _frame;
#line 204 "Minus.m3"
_frame._unused=(ADDRESS)&_frame;
#line 204 "Minus.m3"
 /* load */
#line 204 "Minus.m3"
 /* load */
#line 204 "Minus.m3"
 /* subtract */
#line 204 "Minus.m3"
 /* exit_proc */
#line 204 "Minus.m3"
return ((UINT64)(((UINT64)(((INT64)(*((UINT64*)(INT64_(136)+((ADDRESS)(&Minus_m_M_Minus_L_13))))))))-((UINT64)(*((INT64*)(INT64_(112)+((ADDRESS)(&Minus_m_M_Minus_L_13))))))));
#line 204 "Minus.m3"
 /* end_procedure */
#line 204 "Minus.m3"
} /* Minus_var_LC_u64 */
#line 204 "Minus.m3"
 /* set_source_line */
#line 204 "Minus.m3"
#line 205 "Minus.m3"
 /* begin_procedure */
#line 205 "Minus.m3"
struct Minus__Minus_var_LC_u64_Frame_t {
#line 205 "Minus.m3"
ADDRESS _unused;
#line 205 "Minus.m3"
};
#line 205 "Minus.m3"
LONGINT
__cdecl
Minus__Minus_var_LC_u64(void)
{
#line 205 "Minus.m3"
Minus__Minus_var_LC_u64_Frame_t _frame;
#line 205 "Minus.m3"
_frame._unused=(ADDRESS)&_frame;
#line 205 "Minus.m3"
 /* load */
#line 205 "Minus.m3"
 /* load */
#line 205 "Minus.m3"
 /* subtract */
#line 205 "Minus.m3"
 /* exit_proc */
#line 205 "Minus.m3"
return ((INT64)( ((INT64)(*((UINT64*)(INT64_(136)+((ADDRESS)(&Minus_m_M_Minus_L_13))))))-((INT64)(*((INT64*)(INT64_(112)+((ADDRESS)(&Minus_m_M_Minus_L_13))))))));
#line 205 "Minus.m3"
 /* end_procedure */
#line 205 "Minus.m3"
} /* uMinus_param_LC_u64 */
#line 205 "Minus.m3"
 /* set_source_line */
#line 205 "Minus.m3"
#line 206 "Minus.m3"
 /* begin_procedure */
#line 206 "Minus.m3"
struct Minus__uMinus_param_LC_u64_Frame_t {
#line 206 "Minus.m3"
ADDRESS _unused;
#line 206 "Minus.m3"
};
#line 206 "Minus.m3"
Long__T
__cdecl
Minus__uMinus_param_LC_u64(
   /* Param_Type1 */ LONGCARD a_L_318,
   /* Param_Type1 */ Minus__UINT64 b_L_319)
{
#line 206 "Minus.m3"
Minus__uMinus_param_LC_u64_Frame_t _frame;
#line 206 "Minus.m3"
_frame._unused=(ADDRESS)&_frame;
#line 206 "Minus.m3"
 /* load */
#line 206 "Minus.m3"
 /* load */
#line 206 "Minus.m3"
 /* subtract */
#line 206 "Minus.m3"
 /* exit_proc */
#line 206 "Minus.m3"
return ((UINT64)(((UINT64)(((INT64)(a_L_318))))-((UINT64)(b_L_319))));
#line 206 "Minus.m3"
 /* end_procedure */
#line 206 "Minus.m3"
} /* Minus_param_LC_u64 */
#line 206 "Minus.m3"
 /* set_source_line */
#line 206 "Minus.m3"
#line 207 "Minus.m3"
 /* begin_procedure */
#line 207 "Minus.m3"
struct Minus__Minus_param_LC_u64_Frame_t {
#line 207 "Minus.m3"
ADDRESS _unused;
#line 207 "Minus.m3"
};
#line 207 "Minus.m3"
LONGINT
__cdecl
Minus__Minus_param_LC_u64(
   /* Param_Type1 */ LONGCARD a_L_321,
   /* Param_Type1 */ Minus__UINT64 b_L_322)
{
#line 207 "Minus.m3"
Minus__Minus_param_LC_u64_Frame_t _frame;
#line 207 "Minus.m3"
_frame._unused=(ADDRESS)&_frame;
#line 207 "Minus.m3"
 /* load */
#line 207 "Minus.m3"
 /* load */
#line 207 "Minus.m3"
 /* subtract */
#line 207 "Minus.m3"
 /* exit_proc */
#line 207 "Minus.m3"
return ((INT64)( ((INT64)(a_L_321))- b_L_322));
#line 207 "Minus.m3"
 /* end_procedure */
#line 207 "Minus.m3"
} /* uMinus_var_LC_i32 */
#line 207 "Minus.m3"
 /* set_source_line */
#line 207 "Minus.m3"
#line 208 "Minus.m3"
 /* begin_procedure */
#line 208 "Minus.m3"
struct Minus__uMinus_var_LC_i32_Frame_t {
#line 208 "Minus.m3"
ADDRESS _unused;
#line 208 "Minus.m3"
};
#line 208 "Minus.m3"
Long__T
__cdecl
Minus__uMinus_var_LC_i32(void)
{
#line 208 "Minus.m3"
Minus__uMinus_var_LC_i32_Frame_t _frame;
#line 208 "Minus.m3"
_frame._unused=(ADDRESS)&_frame;
#line 208 "Minus.m3"
 /* load */
#line 208 "Minus.m3"
 /* loophole */
#line 208 "Minus.m3"
 /* load */
#line 208 "Minus.m3"
 /* swap */
#line 208 "Minus.m3"
 /* subtract */
#line 208 "Minus.m3"
 /* exit_proc */
#line 208 "Minus.m3"
return ((UINT64)(((UINT64)(((INT64)(*((UINT64*)(INT64_(136)+((ADDRESS)(&Minus_m_M_Minus_L_13))))))))-((UINT64)((INT64)((INT64)(*((INT32*)(INT64_(128)+((ADDRESS)(&Minus_m_M_Minus_L_13))))))))));
#line 208 "Minus.m3"
 /* end_procedure */
#line 208 "Minus.m3"
} /* Minus_var_LC_i32 */
#line 208 "Minus.m3"
 /* set_source_line */
#line 208 "Minus.m3"
#line 209 "Minus.m3"
 /* begin_procedure */
#line 209 "Minus.m3"
struct Minus__Minus_var_LC_i32_Frame_t {
#line 209 "Minus.m3"
ADDRESS _unused;
#line 209 "Minus.m3"
};
#line 209 "Minus.m3"
LONGINT
__cdecl
Minus__Minus_var_LC_i32(void)
{
#line 209 "Minus.m3"
Minus__Minus_var_LC_i32_Frame_t _frame;
#line 209 "Minus.m3"
_frame._unused=(ADDRESS)&_frame;
#line 209 "Minus.m3"
 /* load */
#line 209 "Minus.m3"
 /* loophole */
#line 209 "Minus.m3"
 /* load */
#line 209 "Minus.m3"
 /* swap */
#line 209 "Minus.m3"
 /* subtract */
#line 209 "Minus.m3"
 /* exit_proc */
#line 209 "Minus.m3"
return ((INT64)( ((INT64)(*((UINT64*)(INT64_(136)+((ADDRESS)(&Minus_m_M_Minus_L_13))))))-((INT64)((INT64)((INT64)(*((INT32*)(INT64_(128)+((ADDRESS)(&Minus_m_M_Minus_L_13))))))))));
#line 209 "Minus.m3"
 /* end_procedure */
#line 209 "Minus.m3"
} /* uMinus_param_LC_i32 */
#line 209 "Minus.m3"
 /* set_source_line */
#line 209 "Minus.m3"
#line 210 "Minus.m3"
 /* begin_procedure */
#line 210 "Minus.m3"
struct Minus__uMinus_param_LC_i32_Frame_t {
#line 210 "Minus.m3"
ADDRESS _unused;
#line 210 "Minus.m3"
};
#line 210 "Minus.m3"
Long__T
__cdecl
Minus__uMinus_param_LC_i32(
   /* Param_Type1 */ LONGCARD a_L_326,
   /* Param_Type1 */ Minus__INT32 b_L_327)
{
#line 210 "Minus.m3"
Minus__uMinus_param_LC_i32_Frame_t _frame;
#line 210 "Minus.m3"
_frame._unused=(ADDRESS)&_frame;
#line 210 "Minus.m3"
 /* load */
#line 210 "Minus.m3"
 /* loophole */
#line 210 "Minus.m3"
 /* load */
#line 210 "Minus.m3"
 /* swap */
#line 210 "Minus.m3"
 /* subtract */
#line 210 "Minus.m3"
 /* exit_proc */
#line 210 "Minus.m3"
return ((UINT64)(((UINT64)(((INT64)(a_L_326))))-((UINT64)((INT64)((INT64)(b_L_327))))));
#line 210 "Minus.m3"
 /* end_procedure */
#line 210 "Minus.m3"
} /* Minus_param_LC_i32 */
#line 210 "Minus.m3"
 /* set_source_line */
#line 210 "Minus.m3"
#line 211 "Minus.m3"
 /* begin_procedure */
#line 211 "Minus.m3"
struct Minus__Minus_param_LC_i32_Frame_t {
#line 211 "Minus.m3"
ADDRESS _unused;
#line 211 "Minus.m3"
};
#line 211 "Minus.m3"
LONGINT
__cdecl
Minus__Minus_param_LC_i32(
   /* Param_Type1 */ LONGCARD a_L_329,
   /* Param_Type1 */ Minus__INT32 b_L_330)
{
#line 211 "Minus.m3"
Minus__Minus_param_LC_i32_Frame_t _frame;
#line 211 "Minus.m3"
_frame._unused=(ADDRESS)&_frame;
#line 211 "Minus.m3"
 /* load */
#line 211 "Minus.m3"
 /* loophole */
#line 211 "Minus.m3"
 /* load */
#line 211 "Minus.m3"
 /* swap */
#line 211 "Minus.m3"
 /* subtract */
#line 211 "Minus.m3"
 /* exit_proc */
#line 211 "Minus.m3"
return ((INT64)( ((INT64)(a_L_329))-((INT64)((INT64)((INT64)(b_L_330))))));
#line 211 "Minus.m3"
 /* end_procedure */
#line 211 "Minus.m3"
} /* uMinus_var_LC_LC */
#line 211 "Minus.m3"
 /* set_source_line */
#line 211 "Minus.m3"
#line 212 "Minus.m3"
 /* begin_procedure */
#line 212 "Minus.m3"
struct Minus__uMinus_var_LC_LC_Frame_t {
#line 212 "Minus.m3"
ADDRESS _unused;
#line 212 "Minus.m3"
};
#line 212 "Minus.m3"
Long__T
__cdecl
Minus__uMinus_var_LC_LC(void)
{
#line 212 "Minus.m3"
Minus__uMinus_var_LC_LC_Frame_t _frame;
#line 212 "Minus.m3"
_frame._unused=(ADDRESS)&_frame;
#line 212 "Minus.m3"
 /* load */
#line 212 "Minus.m3"
 /* load */
#line 212 "Minus.m3"
 /* subtract */
#line 212 "Minus.m3"
 /* exit_proc */
#line 212 "Minus.m3"
return ((UINT64)(((UINT64)(((INT64)(*((UINT64*)(INT64_(136)+((ADDRESS)(&Minus_m_M_Minus_L_13))))))))-((UINT64)(((INT64)(*((UINT64*)(INT64_(136)+((ADDRESS)(&Minus_m_M_Minus_L_13))))))))));
#line 212 "Minus.m3"
 /* end_procedure */
#line 212 "Minus.m3"
} /* Minus_var_LC_LC */
#line 212 "Minus.m3"
 /* set_source_line */
#line 212 "Minus.m3"
#line 213 "Minus.m3"
 /* begin_procedure */
#line 213 "Minus.m3"
struct Minus__Minus_var_LC_LC_Frame_t {
#line 213 "Minus.m3"
ADDRESS _unused;
#line 213 "Minus.m3"
};
#line 213 "Minus.m3"
LONGINT
__cdecl
Minus__Minus_var_LC_LC(void)
{
#line 213 "Minus.m3"
Minus__Minus_var_LC_LC_Frame_t _frame;
#line 213 "Minus.m3"
_frame._unused=(ADDRESS)&_frame;
#line 213 "Minus.m3"
 /* load */
#line 213 "Minus.m3"
 /* load */
#line 213 "Minus.m3"
 /* subtract */
#line 213 "Minus.m3"
 /* exit_proc */
#line 213 "Minus.m3"
return ((INT64)( ((INT64)(*((UINT64*)(INT64_(136)+((ADDRESS)(&Minus_m_M_Minus_L_13))))))- ((INT64)(*((UINT64*)(INT64_(136)+((ADDRESS)(&Minus_m_M_Minus_L_13))))))));
#line 213 "Minus.m3"
 /* end_procedure */
#line 213 "Minus.m3"
} /* uMinus_param_LC_LC */
#line 213 "Minus.m3"
 /* set_source_line */
#line 213 "Minus.m3"
#line 214 "Minus.m3"
 /* begin_procedure */
#line 214 "Minus.m3"
struct Minus__uMinus_param_LC_LC_Frame_t {
#line 214 "Minus.m3"
ADDRESS _unused;
#line 214 "Minus.m3"
};
#line 214 "Minus.m3"
Long__T
__cdecl
Minus__uMinus_param_LC_LC(
   /* Param_Type1 */ LONGCARD a_L_334,
   /* Param_Type1 */ LONGCARD b_L_335)
{
#line 214 "Minus.m3"
Minus__uMinus_param_LC_LC_Frame_t _frame;
#line 214 "Minus.m3"
_frame._unused=(ADDRESS)&_frame;
#line 214 "Minus.m3"
 /* load */
#line 214 "Minus.m3"
 /* load */
#line 214 "Minus.m3"
 /* subtract */
#line 214 "Minus.m3"
 /* exit_proc */
#line 214 "Minus.m3"
return ((UINT64)(((UINT64)(((INT64)(a_L_334))))-((UINT64)(((INT64)(b_L_335))))));
#line 214 "Minus.m3"
 /* end_procedure */
#line 214 "Minus.m3"
} /* Minus_param_LC_LC */
#line 214 "Minus.m3"
 /* set_source_line */
#line 214 "Minus.m3"
#line 215 "Minus.m3"
 /* begin_procedure */
#line 215 "Minus.m3"
struct Minus__Minus_param_LC_LC_Frame_t {
#line 215 "Minus.m3"
ADDRESS _unused;
#line 215 "Minus.m3"
};
#line 215 "Minus.m3"
LONGINT
__cdecl
Minus__Minus_param_LC_LC(
   /* Param_Type1 */ LONGCARD a_L_337,
   /* Param_Type1 */ LONGCARD b_L_338)
{
#line 215 "Minus.m3"
Minus__Minus_param_LC_LC_Frame_t _frame;
#line 215 "Minus.m3"
_frame._unused=(ADDRESS)&_frame;
#line 215 "Minus.m3"
 /* load */
#line 215 "Minus.m3"
 /* load */
#line 215 "Minus.m3"
 /* subtract */
#line 215 "Minus.m3"
 /* exit_proc */
#line 215 "Minus.m3"
return ((INT64)( ((INT64)(a_L_337))- ((INT64)(b_L_338))));
#line 215 "Minus.m3"
 /* end_procedure */
#line 215 "Minus.m3"
} /* uMinus_var_LC_u16 */
#line 215 "Minus.m3"
 /* set_source_line */
#line 215 "Minus.m3"
#line 216 "Minus.m3"
 /* begin_procedure */
#line 216 "Minus.m3"
struct Minus__uMinus_var_LC_u16_Frame_t {
#line 216 "Minus.m3"
ADDRESS _unused;
#line 216 "Minus.m3"
};
#line 216 "Minus.m3"
Long__T
__cdecl
Minus__uMinus_var_LC_u16(void)
{
#line 216 "Minus.m3"
Minus__uMinus_var_LC_u16_Frame_t _frame;
#line 216 "Minus.m3"
_frame._unused=(ADDRESS)&_frame;
#line 216 "Minus.m3"
 /* load */
#line 216 "Minus.m3"
 /* loophole */
#line 216 "Minus.m3"
 /* load */
#line 216 "Minus.m3"
 /* swap */
#line 216 "Minus.m3"
 /* subtract */
#line 216 "Minus.m3"
 /* exit_proc */
#line 216 "Minus.m3"
return ((UINT64)(((UINT64)(((INT64)(*((UINT64*)(INT64_(136)+((ADDRESS)(&Minus_m_M_Minus_L_13))))))))-((UINT64)((INT64)((INT64)(*((UINT16*)(INT64_(144)+((ADDRESS)(&Minus_m_M_Minus_L_13))))))))));
#line 216 "Minus.m3"
 /* end_procedure */
#line 216 "Minus.m3"
} /* Minus_var_LC_u16 */
#line 216 "Minus.m3"
 /* set_source_line */
#line 216 "Minus.m3"
#line 217 "Minus.m3"
 /* begin_procedure */
#line 217 "Minus.m3"
struct Minus__Minus_var_LC_u16_Frame_t {
#line 217 "Minus.m3"
ADDRESS _unused;
#line 217 "Minus.m3"
};
#line 217 "Minus.m3"
LONGINT
__cdecl
Minus__Minus_var_LC_u16(void)
{
#line 217 "Minus.m3"
Minus__Minus_var_LC_u16_Frame_t _frame;
#line 217 "Minus.m3"
_frame._unused=(ADDRESS)&_frame;
#line 217 "Minus.m3"
 /* load */
#line 217 "Minus.m3"
 /* loophole */
#line 217 "Minus.m3"
 /* load */
#line 217 "Minus.m3"
 /* swap */
#line 217 "Minus.m3"
 /* subtract */
#line 217 "Minus.m3"
 /* exit_proc */
#line 217 "Minus.m3"
return ((INT64)( ((INT64)(*((UINT64*)(INT64_(136)+((ADDRESS)(&Minus_m_M_Minus_L_13))))))-((INT64)((INT64)((INT64)(*((UINT16*)(INT64_(144)+((ADDRESS)(&Minus_m_M_Minus_L_13))))))))));
#line 217 "Minus.m3"
 /* end_procedure */
#line 217 "Minus.m3"
} /* uMinus_param_LC_u16 */
#line 217 "Minus.m3"
 /* set_source_line */
#line 217 "Minus.m3"
#line 218 "Minus.m3"
 /* begin_procedure */
#line 218 "Minus.m3"
struct Minus__uMinus_param_LC_u16_Frame_t {
#line 218 "Minus.m3"
ADDRESS _unused;
#line 218 "Minus.m3"
};
#line 218 "Minus.m3"
Long__T
__cdecl
Minus__uMinus_param_LC_u16(
   /* Param_Type1 */ LONGCARD a_L_342,
   /* Param_Type1 */ Minus__UINT16 b_L_343)
{
#line 218 "Minus.m3"
Minus__uMinus_param_LC_u16_Frame_t _frame;
#line 218 "Minus.m3"
_frame._unused=(ADDRESS)&_frame;
#line 218 "Minus.m3"
 /* load */
#line 218 "Minus.m3"
 /* loophole */
#line 218 "Minus.m3"
 /* load */
#line 218 "Minus.m3"
 /* swap */
#line 218 "Minus.m3"
 /* subtract */
#line 218 "Minus.m3"
 /* exit_proc */
#line 218 "Minus.m3"
return ((UINT64)(((UINT64)(((INT64)(a_L_342))))-((UINT64)((INT64)((INT64)(b_L_343))))));
#line 218 "Minus.m3"
 /* end_procedure */
#line 218 "Minus.m3"
} /* Minus_param_LC_u16 */
#line 218 "Minus.m3"
 /* set_source_line */
#line 218 "Minus.m3"
#line 219 "Minus.m3"
 /* begin_procedure */
#line 219 "Minus.m3"
struct Minus__Minus_param_LC_u16_Frame_t {
#line 219 "Minus.m3"
ADDRESS _unused;
#line 219 "Minus.m3"
};
#line 219 "Minus.m3"
LONGINT
__cdecl
Minus__Minus_param_LC_u16(
   /* Param_Type1 */ LONGCARD a_L_345,
   /* Param_Type1 */ Minus__UINT16 b_L_346)
{
#line 219 "Minus.m3"
Minus__Minus_param_LC_u16_Frame_t _frame;
#line 219 "Minus.m3"
_frame._unused=(ADDRESS)&_frame;
#line 219 "Minus.m3"
 /* load */
#line 219 "Minus.m3"
 /* loophole */
#line 219 "Minus.m3"
 /* load */
#line 219 "Minus.m3"
 /* swap */
#line 219 "Minus.m3"
 /* subtract */
#line 219 "Minus.m3"
 /* exit_proc */
#line 219 "Minus.m3"
return ((INT64)( ((INT64)(a_L_345))-((INT64)((INT64)((INT64)(b_L_346))))));
#line 219 "Minus.m3"
 /* end_procedure */
#line 219 "Minus.m3"
} /* uMinus_var_LC_I */
#line 219 "Minus.m3"
 /* set_source_line */
#line 219 "Minus.m3"
#line 220 "Minus.m3"
 /* begin_procedure */
#line 220 "Minus.m3"
struct Minus__uMinus_var_LC_I_Frame_t {
#line 220 "Minus.m3"
ADDRESS _unused;
#line 220 "Minus.m3"
};
#line 220 "Minus.m3"
Long__T
__cdecl
Minus__uMinus_var_LC_I(void)
{
#line 220 "Minus.m3"
Minus__uMinus_var_LC_I_Frame_t _frame;
#line 220 "Minus.m3"
_frame._unused=(ADDRESS)&_frame;
#line 220 "Minus.m3"
 /* load */
#line 220 "Minus.m3"
 /* loophole */
#line 220 "Minus.m3"
 /* load */
#line 220 "Minus.m3"
 /* swap */
#line 220 "Minus.m3"
 /* subtract */
#line 220 "Minus.m3"
 /* exit_proc */
#line 220 "Minus.m3"
return ((UINT64)(((UINT64)(((INT64)(*((UINT64*)(INT64_(136)+((ADDRESS)(&Minus_m_M_Minus_L_13))))))))-((UINT64)((INT64)*((INT64*)(INT64_(152)+((ADDRESS)(&Minus_m_M_Minus_L_13))))))));
#line 220 "Minus.m3"
 /* end_procedure */
#line 220 "Minus.m3"
} /* Minus_var_LC_I */
#line 220 "Minus.m3"
 /* set_source_line */
#line 220 "Minus.m3"
#line 221 "Minus.m3"
 /* begin_procedure */
#line 221 "Minus.m3"
struct Minus__Minus_var_LC_I_Frame_t {
#line 221 "Minus.m3"
ADDRESS _unused;
#line 221 "Minus.m3"
};
#line 221 "Minus.m3"
LONGINT
__cdecl
Minus__Minus_var_LC_I(void)
{
#line 221 "Minus.m3"
Minus__Minus_var_LC_I_Frame_t _frame;
#line 221 "Minus.m3"
_frame._unused=(ADDRESS)&_frame;
#line 221 "Minus.m3"
 /* load */
#line 221 "Minus.m3"
 /* loophole */
#line 221 "Minus.m3"
 /* load */
#line 221 "Minus.m3"
 /* swap */
#line 221 "Minus.m3"
 /* subtract */
#line 221 "Minus.m3"
 /* exit_proc */
#line 221 "Minus.m3"
return ((INT64)( ((INT64)(*((UINT64*)(INT64_(136)+((ADDRESS)(&Minus_m_M_Minus_L_13))))))-((INT64)((INT64)*((INT64*)(INT64_(152)+((ADDRESS)(&Minus_m_M_Minus_L_13))))))));
#line 221 "Minus.m3"
 /* end_procedure */
#line 221 "Minus.m3"
} /* uMinus_param_LC_I */
#line 221 "Minus.m3"
 /* set_source_line */
#line 221 "Minus.m3"
#line 222 "Minus.m3"
 /* begin_procedure */
#line 222 "Minus.m3"
struct Minus__uMinus_param_LC_I_Frame_t {
#line 222 "Minus.m3"
ADDRESS _unused;
#line 222 "Minus.m3"
};
#line 222 "Minus.m3"
Long__T
__cdecl
Minus__uMinus_param_LC_I(
   /* Param_Type1 */ LONGCARD a_L_350,
   /* Param_Type1 */ INTEGER b_L_351)
{
#line 222 "Minus.m3"
Minus__uMinus_param_LC_I_Frame_t _frame;
#line 222 "Minus.m3"
_frame._unused=(ADDRESS)&_frame;
#line 222 "Minus.m3"
 /* load */
#line 222 "Minus.m3"
 /* loophole */
#line 222 "Minus.m3"
 /* load */
#line 222 "Minus.m3"
 /* swap */
#line 222 "Minus.m3"
 /* subtract */
#line 222 "Minus.m3"
 /* exit_proc */
#line 222 "Minus.m3"
return ((UINT64)(((UINT64)(((INT64)(a_L_350))))-((UINT64)((INT64)b_L_351))));
#line 222 "Minus.m3"
 /* end_procedure */
#line 222 "Minus.m3"
} /* Minus_param_LC_I */
#line 222 "Minus.m3"
 /* set_source_line */
#line 222 "Minus.m3"
#line 223 "Minus.m3"
 /* begin_procedure */
#line 223 "Minus.m3"
struct Minus__Minus_param_LC_I_Frame_t {
#line 223 "Minus.m3"
ADDRESS _unused;
#line 223 "Minus.m3"
};
#line 223 "Minus.m3"
LONGINT
__cdecl
Minus__Minus_param_LC_I(
   /* Param_Type1 */ LONGCARD a_L_353,
   /* Param_Type1 */ INTEGER b_L_354)
{
#line 223 "Minus.m3"
Minus__Minus_param_LC_I_Frame_t _frame;
#line 223 "Minus.m3"
_frame._unused=(ADDRESS)&_frame;
#line 223 "Minus.m3"
 /* load */
#line 223 "Minus.m3"
 /* loophole */
#line 223 "Minus.m3"
 /* load */
#line 223 "Minus.m3"
 /* swap */
#line 223 "Minus.m3"
 /* subtract */
#line 223 "Minus.m3"
 /* exit_proc */
#line 223 "Minus.m3"
return ((INT64)( ((INT64)(a_L_353))-((INT64)((INT64)b_L_354))));
#line 223 "Minus.m3"
 /* end_procedure */
#line 223 "Minus.m3"
} /* uMinus_var_LC_i64 */
#line 223 "Minus.m3"
 /* set_source_line */
#line 223 "Minus.m3"
#line 224 "Minus.m3"
 /* begin_procedure */
#line 224 "Minus.m3"
struct Minus__uMinus_var_LC_i64_Frame_t {
#line 224 "Minus.m3"
ADDRESS _unused;
#line 224 "Minus.m3"
};
#line 224 "Minus.m3"
Long__T
__cdecl
Minus__uMinus_var_LC_i64(void)
{
#line 224 "Minus.m3"
Minus__uMinus_var_LC_i64_Frame_t _frame;
#line 224 "Minus.m3"
_frame._unused=(ADDRESS)&_frame;
#line 224 "Minus.m3"
 /* load */
#line 224 "Minus.m3"
 /* load */
#line 224 "Minus.m3"
 /* subtract */
#line 224 "Minus.m3"
 /* exit_proc */
#line 224 "Minus.m3"
return ((UINT64)(((UINT64)(((INT64)(*((UINT64*)(INT64_(136)+((ADDRESS)(&Minus_m_M_Minus_L_13))))))))-((UINT64)(*((INT64*)(INT64_(160)+((ADDRESS)(&Minus_m_M_Minus_L_13))))))));
#line 224 "Minus.m3"
 /* end_procedure */
#line 224 "Minus.m3"
} /* Minus_var_LC_i64 */
#line 224 "Minus.m3"
 /* set_source_line */
#line 224 "Minus.m3"
#line 225 "Minus.m3"
 /* begin_procedure */
#line 225 "Minus.m3"
struct Minus__Minus_var_LC_i64_Frame_t {
#line 225 "Minus.m3"
ADDRESS _unused;
#line 225 "Minus.m3"
};
#line 225 "Minus.m3"
LONGINT
__cdecl
Minus__Minus_var_LC_i64(void)
{
#line 225 "Minus.m3"
Minus__Minus_var_LC_i64_Frame_t _frame;
#line 225 "Minus.m3"
_frame._unused=(ADDRESS)&_frame;
#line 225 "Minus.m3"
 /* load */
#line 225 "Minus.m3"
 /* load */
#line 225 "Minus.m3"
 /* subtract */
#line 225 "Minus.m3"
 /* exit_proc */
#line 225 "Minus.m3"
return ((INT64)( ((INT64)(*((UINT64*)(INT64_(136)+((ADDRESS)(&Minus_m_M_Minus_L_13))))))-((INT64)(*((INT64*)(INT64_(160)+((ADDRESS)(&Minus_m_M_Minus_L_13))))))));
#line 225 "Minus.m3"
 /* end_procedure */
#line 225 "Minus.m3"
} /* uMinus_param_LC_i64 */
#line 225 "Minus.m3"
 /* set_source_line */
#line 225 "Minus.m3"
#line 226 "Minus.m3"
 /* begin_procedure */
#line 226 "Minus.m3"
struct Minus__uMinus_param_LC_i64_Frame_t {
#line 226 "Minus.m3"
ADDRESS _unused;
#line 226 "Minus.m3"
};
#line 226 "Minus.m3"
Long__T
__cdecl
Minus__uMinus_param_LC_i64(
   /* Param_Type1 */ LONGCARD a_L_358,
   /* Param_Type1 */ Minus__INT64 b_L_359)
{
#line 226 "Minus.m3"
Minus__uMinus_param_LC_i64_Frame_t _frame;
#line 226 "Minus.m3"
_frame._unused=(ADDRESS)&_frame;
#line 226 "Minus.m3"
 /* load */
#line 226 "Minus.m3"
 /* load */
#line 226 "Minus.m3"
 /* subtract */
#line 226 "Minus.m3"
 /* exit_proc */
#line 226 "Minus.m3"
return ((UINT64)(((UINT64)(((INT64)(a_L_358))))-((UINT64)(b_L_359))));
#line 226 "Minus.m3"
 /* end_procedure */
#line 226 "Minus.m3"
} /* Minus_param_LC_i64 */
#line 226 "Minus.m3"
 /* set_source_line */
#line 226 "Minus.m3"
#line 227 "Minus.m3"
 /* begin_procedure */
#line 227 "Minus.m3"
struct Minus__Minus_param_LC_i64_Frame_t {
#line 227 "Minus.m3"
ADDRESS _unused;
#line 227 "Minus.m3"
};
#line 227 "Minus.m3"
LONGINT
__cdecl
Minus__Minus_param_LC_i64(
   /* Param_Type1 */ LONGCARD a_L_361,
   /* Param_Type1 */ Minus__INT64 b_L_362)
{
#line 227 "Minus.m3"
Minus__Minus_param_LC_i64_Frame_t _frame;
#line 227 "Minus.m3"
_frame._unused=(ADDRESS)&_frame;
#line 227 "Minus.m3"
 /* load */
#line 227 "Minus.m3"
 /* load */
#line 227 "Minus.m3"
 /* subtract */
#line 227 "Minus.m3"
 /* exit_proc */
#line 227 "Minus.m3"
return ((INT64)( ((INT64)(a_L_361))- b_L_362));
#line 227 "Minus.m3"
 /* end_procedure */
#line 227 "Minus.m3"
} /* uMinus_var_LC_i16 */
#line 227 "Minus.m3"
 /* set_source_line */
#line 227 "Minus.m3"
#line 228 "Minus.m3"
 /* begin_procedure */
#line 228 "Minus.m3"
struct Minus__uMinus_var_LC_i16_Frame_t {
#line 228 "Minus.m3"
ADDRESS _unused;
#line 228 "Minus.m3"
};
#line 228 "Minus.m3"
Long__T
__cdecl
Minus__uMinus_var_LC_i16(void)
{
#line 228 "Minus.m3"
Minus__uMinus_var_LC_i16_Frame_t _frame;
#line 228 "Minus.m3"
_frame._unused=(ADDRESS)&_frame;
#line 228 "Minus.m3"
 /* load */
#line 228 "Minus.m3"
 /* loophole */
#line 228 "Minus.m3"
 /* load */
#line 228 "Minus.m3"
 /* swap */
#line 228 "Minus.m3"
 /* subtract */
#line 228 "Minus.m3"
 /* exit_proc */
#line 228 "Minus.m3"
return ((UINT64)(((UINT64)(((INT64)(*((UINT64*)(INT64_(136)+((ADDRESS)(&Minus_m_M_Minus_L_13))))))))-((UINT64)((INT64)((INT64)(*((INT16*)(INT64_(172)+((ADDRESS)(&Minus_m_M_Minus_L_13))))))))));
#line 228 "Minus.m3"
 /* end_procedure */
#line 228 "Minus.m3"
} /* Minus_var_LC_i16 */
#line 228 "Minus.m3"
 /* set_source_line */
#line 228 "Minus.m3"
#line 229 "Minus.m3"
 /* begin_procedure */
#line 229 "Minus.m3"
struct Minus__Minus_var_LC_i16_Frame_t {
#line 229 "Minus.m3"
ADDRESS _unused;
#line 229 "Minus.m3"
};
#line 229 "Minus.m3"
LONGINT
__cdecl
Minus__Minus_var_LC_i16(void)
{
#line 229 "Minus.m3"
Minus__Minus_var_LC_i16_Frame_t _frame;
#line 229 "Minus.m3"
_frame._unused=(ADDRESS)&_frame;
#line 229 "Minus.m3"
 /* load */
#line 229 "Minus.m3"
 /* loophole */
#line 229 "Minus.m3"
 /* load */
#line 229 "Minus.m3"
 /* swap */
#line 229 "Minus.m3"
 /* subtract */
#line 229 "Minus.m3"
 /* exit_proc */
#line 229 "Minus.m3"
return ((INT64)( ((INT64)(*((UINT64*)(INT64_(136)+((ADDRESS)(&Minus_m_M_Minus_L_13))))))-((INT64)((INT64)((INT64)(*((INT16*)(INT64_(172)+((ADDRESS)(&Minus_m_M_Minus_L_13))))))))));
#line 229 "Minus.m3"
 /* end_procedure */
#line 229 "Minus.m3"
} /* uMinus_param_LC_i16 */
#line 229 "Minus.m3"
 /* set_source_line */
#line 229 "Minus.m3"
#line 230 "Minus.m3"
 /* begin_procedure */
#line 230 "Minus.m3"
struct Minus__uMinus_param_LC_i16_Frame_t {
#line 230 "Minus.m3"
ADDRESS _unused;
#line 230 "Minus.m3"
};
#line 230 "Minus.m3"
Long__T
__cdecl
Minus__uMinus_param_LC_i16(
   /* Param_Type1 */ LONGCARD a_L_366,
   /* Param_Type1 */ Minus__INT16 b_L_367)
{
#line 230 "Minus.m3"
Minus__uMinus_param_LC_i16_Frame_t _frame;
#line 230 "Minus.m3"
_frame._unused=(ADDRESS)&_frame;
#line 230 "Minus.m3"
 /* load */
#line 230 "Minus.m3"
 /* loophole */
#line 230 "Minus.m3"
 /* load */
#line 230 "Minus.m3"
 /* swap */
#line 230 "Minus.m3"
 /* subtract */
#line 230 "Minus.m3"
 /* exit_proc */
#line 230 "Minus.m3"
return ((UINT64)(((UINT64)(((INT64)(a_L_366))))-((UINT64)((INT64)((INT64)(b_L_367))))));
#line 230 "Minus.m3"
 /* end_procedure */
#line 230 "Minus.m3"
} /* Minus_param_LC_i16 */
#line 230 "Minus.m3"
 /* set_source_line */
#line 230 "Minus.m3"
#line 231 "Minus.m3"
 /* begin_procedure */
#line 231 "Minus.m3"
struct Minus__Minus_param_LC_i16_Frame_t {
#line 231 "Minus.m3"
ADDRESS _unused;
#line 231 "Minus.m3"
};
#line 231 "Minus.m3"
LONGINT
__cdecl
Minus__Minus_param_LC_i16(
   /* Param_Type1 */ LONGCARD a_L_369,
   /* Param_Type1 */ Minus__INT16 b_L_370)
{
#line 231 "Minus.m3"
Minus__Minus_param_LC_i16_Frame_t _frame;
#line 231 "Minus.m3"
_frame._unused=(ADDRESS)&_frame;
#line 231 "Minus.m3"
 /* load */
#line 231 "Minus.m3"
 /* loophole */
#line 231 "Minus.m3"
 /* load */
#line 231 "Minus.m3"
 /* swap */
#line 231 "Minus.m3"
 /* subtract */
#line 231 "Minus.m3"
 /* exit_proc */
#line 231 "Minus.m3"
return ((INT64)( ((INT64)(a_L_369))-((INT64)((INT64)((INT64)(b_L_370))))));
#line 231 "Minus.m3"
 /* end_procedure */
#line 231 "Minus.m3"
} /* uMinus_var_LC_C */
#line 231 "Minus.m3"
 /* set_source_line */
#line 231 "Minus.m3"
#line 232 "Minus.m3"
 /* begin_procedure */
#line 232 "Minus.m3"
struct Minus__uMinus_var_LC_C_Frame_t {
#line 232 "Minus.m3"
ADDRESS _unused;
#line 232 "Minus.m3"
};
#line 232 "Minus.m3"
Long__T
__cdecl
Minus__uMinus_var_LC_C(void)
{
#line 232 "Minus.m3"
Minus__uMinus_var_LC_C_Frame_t _frame;
#line 232 "Minus.m3"
_frame._unused=(ADDRESS)&_frame;
#line 232 "Minus.m3"
 /* load */
#line 232 "Minus.m3"
 /* loophole */
#line 232 "Minus.m3"
 /* load */
#line 232 "Minus.m3"
 /* swap */
#line 232 "Minus.m3"
 /* subtract */
#line 232 "Minus.m3"
 /* exit_proc */
#line 232 "Minus.m3"
return ((UINT64)(((UINT64)(((INT64)(*((UINT64*)(INT64_(136)+((ADDRESS)(&Minus_m_M_Minus_L_13))))))))-((UINT64)((INT64)((INT64)(*((UINT64*)(INT64_(176)+((ADDRESS)(&Minus_m_M_Minus_L_13))))))))));
#line 232 "Minus.m3"
 /* end_procedure */
#line 232 "Minus.m3"
} /* Minus_var_LC_C */
#line 232 "Minus.m3"
 /* set_source_line */
#line 232 "Minus.m3"
#line 233 "Minus.m3"
 /* begin_procedure */
#line 233 "Minus.m3"
struct Minus__Minus_var_LC_C_Frame_t {
#line 233 "Minus.m3"
ADDRESS _unused;
#line 233 "Minus.m3"
};
#line 233 "Minus.m3"
LONGINT
__cdecl
Minus__Minus_var_LC_C(void)
{
#line 233 "Minus.m3"
Minus__Minus_var_LC_C_Frame_t _frame;
#line 233 "Minus.m3"
_frame._unused=(ADDRESS)&_frame;
#line 233 "Minus.m3"
 /* load */
#line 233 "Minus.m3"
 /* loophole */
#line 233 "Minus.m3"
 /* load */
#line 233 "Minus.m3"
 /* swap */
#line 233 "Minus.m3"
 /* subtract */
#line 233 "Minus.m3"
 /* exit_proc */
#line 233 "Minus.m3"
return ((INT64)( ((INT64)(*((UINT64*)(INT64_(136)+((ADDRESS)(&Minus_m_M_Minus_L_13))))))-((INT64)((INT64)((INT64)(*((UINT64*)(INT64_(176)+((ADDRESS)(&Minus_m_M_Minus_L_13))))))))));
#line 233 "Minus.m3"
 /* end_procedure */
#line 233 "Minus.m3"
} /* uMinus_param_LC_C */
#line 233 "Minus.m3"
 /* set_source_line */
#line 233 "Minus.m3"
#line 234 "Minus.m3"
 /* begin_procedure */
#line 234 "Minus.m3"
struct Minus__uMinus_param_LC_C_Frame_t {
#line 234 "Minus.m3"
ADDRESS _unused;
#line 234 "Minus.m3"
};
#line 234 "Minus.m3"
Long__T
__cdecl
Minus__uMinus_param_LC_C(
   /* Param_Type1 */ LONGCARD a_L_374,
   /* Param_Type1 */ CARDINAL b_L_375)
{
#line 234 "Minus.m3"
Minus__uMinus_param_LC_C_Frame_t _frame;
#line 234 "Minus.m3"
_frame._unused=(ADDRESS)&_frame;
#line 234 "Minus.m3"
 /* load */
#line 234 "Minus.m3"
 /* loophole */
#line 234 "Minus.m3"
 /* load */
#line 234 "Minus.m3"
 /* swap */
#line 234 "Minus.m3"
 /* subtract */
#line 234 "Minus.m3"
 /* exit_proc */
#line 234 "Minus.m3"
return ((UINT64)(((UINT64)(((INT64)(a_L_374))))-((UINT64)((INT64)((INT64)(b_L_375))))));
#line 234 "Minus.m3"
 /* end_procedure */
#line 234 "Minus.m3"
} /* Minus_param_LC_C */
#line 234 "Minus.m3"
 /* set_source_line */
#line 234 "Minus.m3"
#line 235 "Minus.m3"
 /* begin_procedure */
#line 235 "Minus.m3"
struct Minus__Minus_param_LC_C_Frame_t {
#line 235 "Minus.m3"
ADDRESS _unused;
#line 235 "Minus.m3"
};
#line 235 "Minus.m3"
LONGINT
__cdecl
Minus__Minus_param_LC_C(
   /* Param_Type1 */ LONGCARD a_L_377,
   /* Param_Type1 */ CARDINAL b_L_378)
{
#line 235 "Minus.m3"
Minus__Minus_param_LC_C_Frame_t _frame;
#line 235 "Minus.m3"
_frame._unused=(ADDRESS)&_frame;
#line 235 "Minus.m3"
 /* load */
#line 235 "Minus.m3"
 /* loophole */
#line 235 "Minus.m3"
 /* load */
#line 235 "Minus.m3"
 /* swap */
#line 235 "Minus.m3"
 /* subtract */
#line 235 "Minus.m3"
 /* exit_proc */
#line 235 "Minus.m3"
return ((INT64)( ((INT64)(a_L_377))-((INT64)((INT64)((INT64)(b_L_378))))));
#line 235 "Minus.m3"
 /* end_procedure */
#line 235 "Minus.m3"
} /* uMinus_var_LC_u32 */
#line 235 "Minus.m3"
 /* set_source_line */
#line 235 "Minus.m3"
#line 236 "Minus.m3"
 /* begin_procedure */
#line 236 "Minus.m3"
struct Minus__uMinus_var_LC_u32_Frame_t {
#line 236 "Minus.m3"
ADDRESS _unused;
#line 236 "Minus.m3"
};
#line 236 "Minus.m3"
Long__T
__cdecl
Minus__uMinus_var_LC_u32(void)
{
#line 236 "Minus.m3"
Minus__uMinus_var_LC_u32_Frame_t _frame;
#line 236 "Minus.m3"
_frame._unused=(ADDRESS)&_frame;
#line 236 "Minus.m3"
 /* load */
#line 236 "Minus.m3"
 /* loophole */
#line 236 "Minus.m3"
 /* load */
#line 236 "Minus.m3"
 /* swap */
#line 236 "Minus.m3"
 /* subtract */
#line 236 "Minus.m3"
 /* exit_proc */
#line 236 "Minus.m3"
return ((UINT64)(((UINT64)(((INT64)(*((UINT64*)(INT64_(136)+((ADDRESS)(&Minus_m_M_Minus_L_13))))))))-((UINT64)((INT64)((INT64)(*((UINT32*)(INT64_(184)+((ADDRESS)(&Minus_m_M_Minus_L_13))))))))));
#line 236 "Minus.m3"
 /* end_procedure */
#line 236 "Minus.m3"
} /* Minus_var_LC_u32 */
#line 236 "Minus.m3"
 /* set_source_line */
#line 236 "Minus.m3"
#line 237 "Minus.m3"
 /* begin_procedure */
#line 237 "Minus.m3"
struct Minus__Minus_var_LC_u32_Frame_t {
#line 237 "Minus.m3"
ADDRESS _unused;
#line 237 "Minus.m3"
};
#line 237 "Minus.m3"
LONGINT
__cdecl
Minus__Minus_var_LC_u32(void)
{
#line 237 "Minus.m3"
Minus__Minus_var_LC_u32_Frame_t _frame;
#line 237 "Minus.m3"
_frame._unused=(ADDRESS)&_frame;
#line 237 "Minus.m3"
 /* load */
#line 237 "Minus.m3"
 /* loophole */
#line 237 "Minus.m3"
 /* load */
#line 237 "Minus.m3"
 /* swap */
#line 237 "Minus.m3"
 /* subtract */
#line 237 "Minus.m3"
 /* exit_proc */
#line 237 "Minus.m3"
return ((INT64)( ((INT64)(*((UINT64*)(INT64_(136)+((ADDRESS)(&Minus_m_M_Minus_L_13))))))-((INT64)((INT64)((INT64)(*((UINT32*)(INT64_(184)+((ADDRESS)(&Minus_m_M_Minus_L_13))))))))));
#line 237 "Minus.m3"
 /* end_procedure */
#line 237 "Minus.m3"
} /* uMinus_param_LC_u32 */
#line 237 "Minus.m3"
 /* set_source_line */
#line 237 "Minus.m3"
#line 238 "Minus.m3"
 /* begin_procedure */
#line 238 "Minus.m3"
struct Minus__uMinus_param_LC_u32_Frame_t {
#line 238 "Minus.m3"
ADDRESS _unused;
#line 238 "Minus.m3"
};
#line 238 "Minus.m3"
Long__T
__cdecl
Minus__uMinus_param_LC_u32(
   /* Param_Type1 */ LONGCARD a_L_382,
   /* Param_Type1 */ Minus__UINT32 b_L_383)
{
#line 238 "Minus.m3"
Minus__uMinus_param_LC_u32_Frame_t _frame;
#line 238 "Minus.m3"
_frame._unused=(ADDRESS)&_frame;
#line 238 "Minus.m3"
 /* load */
#line 238 "Minus.m3"
 /* loophole */
#line 238 "Minus.m3"
 /* load */
#line 238 "Minus.m3"
 /* swap */
#line 238 "Minus.m3"
 /* subtract */
#line 238 "Minus.m3"
 /* exit_proc */
#line 238 "Minus.m3"
return ((UINT64)(((UINT64)(((INT64)(a_L_382))))-((UINT64)((INT64)((INT64)(b_L_383))))));
#line 238 "Minus.m3"
 /* end_procedure */
#line 238 "Minus.m3"
} /* Minus_param_LC_u32 */
#line 238 "Minus.m3"
 /* set_source_line */
#line 238 "Minus.m3"
#line 239 "Minus.m3"
 /* begin_procedure */
#line 239 "Minus.m3"
struct Minus__Minus_param_LC_u32_Frame_t {
#line 239 "Minus.m3"
ADDRESS _unused;
#line 239 "Minus.m3"
};
#line 239 "Minus.m3"
LONGINT
__cdecl
Minus__Minus_param_LC_u32(
   /* Param_Type1 */ LONGCARD a_L_385,
   /* Param_Type1 */ Minus__UINT32 b_L_387)
{
#line 239 "Minus.m3"
Minus__Minus_param_LC_u32_Frame_t _frame;
#line 239 "Minus.m3"
_frame._unused=(ADDRESS)&_frame;
#line 239 "Minus.m3"
 /* load */
#line 239 "Minus.m3"
 /* loophole */
#line 239 "Minus.m3"
 /* load */
#line 239 "Minus.m3"
 /* swap */
#line 239 "Minus.m3"
 /* subtract */
#line 239 "Minus.m3"
 /* exit_proc */
#line 239 "Minus.m3"
return ((INT64)( ((INT64)(a_L_385))-((INT64)((INT64)((INT64)(b_L_387))))));
#line 239 "Minus.m3"
 /* end_procedure */
#line 239 "Minus.m3"
} /* uMinus_var_LC_u8 */
#line 239 "Minus.m3"
 /* set_source_line */
#line 239 "Minus.m3"
#line 240 "Minus.m3"
 /* begin_procedure */
#line 240 "Minus.m3"
struct Minus__uMinus_var_LC_u8_Frame_t {
#line 240 "Minus.m3"
ADDRESS _unused;
#line 240 "Minus.m3"
};
#line 240 "Minus.m3"
Long__T
__cdecl
Minus__uMinus_var_LC_u8(void)
{
#line 240 "Minus.m3"
Minus__uMinus_var_LC_u8_Frame_t _frame;
#line 240 "Minus.m3"
_frame._unused=(ADDRESS)&_frame;
#line 240 "Minus.m3"
 /* load */
#line 240 "Minus.m3"
 /* loophole */
#line 240 "Minus.m3"
 /* load */
#line 240 "Minus.m3"
 /* swap */
#line 240 "Minus.m3"
 /* subtract */
#line 240 "Minus.m3"
 /* exit_proc */
#line 240 "Minus.m3"
return ((UINT64)(((UINT64)(((INT64)(*((UINT64*)(INT64_(136)+((ADDRESS)(&Minus_m_M_Minus_L_13))))))))-((UINT64)((INT64)((INT64)(*((UINT8*)(INT64_(188)+((ADDRESS)(&Minus_m_M_Minus_L_13))))))))));
#line 240 "Minus.m3"
 /* end_procedure */
#line 240 "Minus.m3"
} /* Minus_var_LC_u8 */
#line 240 "Minus.m3"
 /* set_source_line */
#line 240 "Minus.m3"
#line 241 "Minus.m3"
 /* begin_procedure */
#line 241 "Minus.m3"
struct Minus__Minus_var_LC_u8_Frame_t {
#line 241 "Minus.m3"
ADDRESS _unused;
#line 241 "Minus.m3"
};
#line 241 "Minus.m3"
LONGINT
__cdecl
Minus__Minus_var_LC_u8(void)
{
#line 241 "Minus.m3"
Minus__Minus_var_LC_u8_Frame_t _frame;
#line 241 "Minus.m3"
_frame._unused=(ADDRESS)&_frame;
#line 241 "Minus.m3"
 /* load */
#line 241 "Minus.m3"
 /* loophole */
#line 241 "Minus.m3"
 /* load */
#line 241 "Minus.m3"
 /* swap */
#line 241 "Minus.m3"
 /* subtract */
#line 241 "Minus.m3"
 /* exit_proc */
#line 241 "Minus.m3"
return ((INT64)( ((INT64)(*((UINT64*)(INT64_(136)+((ADDRESS)(&Minus_m_M_Minus_L_13))))))-((INT64)((INT64)((INT64)(*((UINT8*)(INT64_(188)+((ADDRESS)(&Minus_m_M_Minus_L_13))))))))));
#line 241 "Minus.m3"
 /* end_procedure */
#line 241 "Minus.m3"
} /* uMinus_param_LC_u8 */
#line 241 "Minus.m3"
 /* set_source_line */
#line 241 "Minus.m3"
#line 242 "Minus.m3"
 /* begin_procedure */
#line 242 "Minus.m3"
struct Minus__uMinus_param_LC_u8_Frame_t {
#line 242 "Minus.m3"
ADDRESS _unused;
#line 242 "Minus.m3"
};
#line 242 "Minus.m3"
Long__T
__cdecl
Minus__uMinus_param_LC_u8(
   /* Param_Type1 */ LONGCARD a_L_391,
   /* Param_Type1 */ Minus__UINT8 b_L_392)
{
#line 242 "Minus.m3"
Minus__uMinus_param_LC_u8_Frame_t _frame;
#line 242 "Minus.m3"
_frame._unused=(ADDRESS)&_frame;
#line 242 "Minus.m3"
 /* load */
#line 242 "Minus.m3"
 /* loophole */
#line 242 "Minus.m3"
 /* load */
#line 242 "Minus.m3"
 /* swap */
#line 242 "Minus.m3"
 /* subtract */
#line 242 "Minus.m3"
 /* exit_proc */
#line 242 "Minus.m3"
return ((UINT64)(((UINT64)(((INT64)(a_L_391))))-((UINT64)((INT64)((INT64)(b_L_392))))));
#line 242 "Minus.m3"
 /* end_procedure */
#line 242 "Minus.m3"
} /* Minus_param_LC_u8 */
#line 242 "Minus.m3"
 /* set_source_line */
#line 242 "Minus.m3"
#line 243 "Minus.m3"
 /* begin_procedure */
#line 243 "Minus.m3"
struct Minus__Minus_param_LC_u8_Frame_t {
#line 243 "Minus.m3"
ADDRESS _unused;
#line 243 "Minus.m3"
};
#line 243 "Minus.m3"
LONGINT
__cdecl
Minus__Minus_param_LC_u8(
   /* Param_Type1 */ LONGCARD a_L_394,
   /* Param_Type1 */ Minus__UINT8 b_L_395)
{
#line 243 "Minus.m3"
Minus__Minus_param_LC_u8_Frame_t _frame;
#line 243 "Minus.m3"
_frame._unused=(ADDRESS)&_frame;
#line 243 "Minus.m3"
 /* load */
#line 243 "Minus.m3"
 /* loophole */
#line 243 "Minus.m3"
 /* load */
#line 243 "Minus.m3"
 /* swap */
#line 243 "Minus.m3"
 /* subtract */
#line 243 "Minus.m3"
 /* exit_proc */
#line 243 "Minus.m3"
return ((INT64)( ((INT64)(a_L_394))-((INT64)((INT64)((INT64)(b_L_395))))));
#line 243 "Minus.m3"
 /* end_procedure */
#line 243 "Minus.m3"
} /* uMinus_var_LC_L */
#line 243 "Minus.m3"
 /* set_source_line */
#line 243 "Minus.m3"
#line 244 "Minus.m3"
 /* begin_procedure */
#line 244 "Minus.m3"
struct Minus__uMinus_var_LC_L_Frame_t {
#line 244 "Minus.m3"
ADDRESS _unused;
#line 244 "Minus.m3"
};
#line 244 "Minus.m3"
Long__T
__cdecl
Minus__uMinus_var_LC_L(void)
{
#line 244 "Minus.m3"
Minus__uMinus_var_LC_L_Frame_t _frame;
#line 244 "Minus.m3"
_frame._unused=(ADDRESS)&_frame;
#line 244 "Minus.m3"
 /* load */
#line 244 "Minus.m3"
 /* load */
#line 244 "Minus.m3"
 /* subtract */
#line 244 "Minus.m3"
 /* exit_proc */
#line 244 "Minus.m3"
return ((UINT64)(((UINT64)(((INT64)(*((UINT64*)(INT64_(136)+((ADDRESS)(&Minus_m_M_Minus_L_13))))))))-((UINT64)(*((INT64*)(INT64_(192)+((ADDRESS)(&Minus_m_M_Minus_L_13))))))));
#line 244 "Minus.m3"
 /* end_procedure */
#line 244 "Minus.m3"
} /* Minus_var_LC_L */
#line 244 "Minus.m3"
 /* set_source_line */
#line 244 "Minus.m3"
#line 245 "Minus.m3"
 /* begin_procedure */
#line 245 "Minus.m3"
struct Minus__Minus_var_LC_L_Frame_t {
#line 245 "Minus.m3"
ADDRESS _unused;
#line 245 "Minus.m3"
};
#line 245 "Minus.m3"
LONGINT
__cdecl
Minus__Minus_var_LC_L(void)
{
#line 245 "Minus.m3"
Minus__Minus_var_LC_L_Frame_t _frame;
#line 245 "Minus.m3"
_frame._unused=(ADDRESS)&_frame;
#line 245 "Minus.m3"
 /* load */
#line 245 "Minus.m3"
 /* load */
#line 245 "Minus.m3"
 /* subtract */
#line 245 "Minus.m3"
 /* exit_proc */
#line 245 "Minus.m3"
return ((INT64)( ((INT64)(*((UINT64*)(INT64_(136)+((ADDRESS)(&Minus_m_M_Minus_L_13))))))-((INT64)(*((INT64*)(INT64_(192)+((ADDRESS)(&Minus_m_M_Minus_L_13))))))));
#line 245 "Minus.m3"
 /* end_procedure */
#line 245 "Minus.m3"
} /* uMinus_param_LC_L */
#line 245 "Minus.m3"
 /* set_source_line */
#line 245 "Minus.m3"
#line 246 "Minus.m3"
 /* begin_procedure */
#line 246 "Minus.m3"
struct Minus__uMinus_param_LC_L_Frame_t {
#line 246 "Minus.m3"
ADDRESS _unused;
#line 246 "Minus.m3"
};
#line 246 "Minus.m3"
Long__T
__cdecl
Minus__uMinus_param_LC_L(
   /* Param_Type1 */ LONGCARD a_L_399,
   /* Param_Type1 */ LONGINT b_L_400)
{
#line 246 "Minus.m3"
Minus__uMinus_param_LC_L_Frame_t _frame;
#line 246 "Minus.m3"
_frame._unused=(ADDRESS)&_frame;
#line 246 "Minus.m3"
 /* load */
#line 246 "Minus.m3"
 /* load */
#line 246 "Minus.m3"
 /* subtract */
#line 246 "Minus.m3"
 /* exit_proc */
#line 246 "Minus.m3"
return ((UINT64)(((UINT64)(((INT64)(a_L_399))))-((UINT64)(b_L_400))));
#line 246 "Minus.m3"
 /* end_procedure */
#line 246 "Minus.m3"
} /* Minus_param_LC_L */
#line 246 "Minus.m3"
 /* set_source_line */
#line 246 "Minus.m3"
#line 247 "Minus.m3"
 /* begin_procedure */
#line 247 "Minus.m3"
struct Minus__Minus_param_LC_L_Frame_t {
#line 247 "Minus.m3"
ADDRESS _unused;
#line 247 "Minus.m3"
};
#line 247 "Minus.m3"
LONGINT
__cdecl
Minus__Minus_param_LC_L(
   /* Param_Type1 */ LONGCARD a_L_402,
   /* Param_Type1 */ LONGINT b_L_403)
{
#line 247 "Minus.m3"
Minus__Minus_param_LC_L_Frame_t _frame;
#line 247 "Minus.m3"
_frame._unused=(ADDRESS)&_frame;
#line 247 "Minus.m3"
 /* load */
#line 247 "Minus.m3"
 /* load */
#line 247 "Minus.m3"
 /* subtract */
#line 247 "Minus.m3"
 /* exit_proc */
#line 247 "Minus.m3"
return ((INT64)( ((INT64)(a_L_402))- b_L_403));
#line 247 "Minus.m3"
 /* end_procedure */
#line 247 "Minus.m3"
} /* uMinus_var_u16_i8 */
#line 247 "Minus.m3"
 /* set_source_line */
#line 247 "Minus.m3"
#line 248 "Minus.m3"
 /* begin_procedure */
#line 248 "Minus.m3"
struct Minus__uMinus_var_u16_i8_Frame_t {
#line 248 "Minus.m3"
ADDRESS _unused;
#line 248 "Minus.m3"
};
#line 248 "Minus.m3"
Word__T
__cdecl
Minus__uMinus_var_u16_i8(void)
{
#line 248 "Minus.m3"
Minus__uMinus_var_u16_i8_Frame_t _frame;
#line 248 "Minus.m3"
_frame._unused=(ADDRESS)&_frame;
#line 248 "Minus.m3"
 /* load */
#line 248 "Minus.m3"
 /* load */
#line 248 "Minus.m3"
 /* subtract */
#line 248 "Minus.m3"
 /* exit_proc */
#line 248 "Minus.m3"
return ((UINT64)(((UINT64)(((INT64)(*((UINT16*)(INT64_(144)+((ADDRESS)(&Minus_m_M_Minus_L_13))))))))-((UINT64)(((INT64)(*((INT8*)(INT64_(104)+((ADDRESS)(&Minus_m_M_Minus_L_13))))))))));
#line 248 "Minus.m3"
 /* end_procedure */
#line 248 "Minus.m3"
} /* Minus_var_u16_i8 */
#line 248 "Minus.m3"
 /* set_source_line */
#line 248 "Minus.m3"
#line 249 "Minus.m3"
 /* begin_procedure */
#line 249 "Minus.m3"
struct Minus__Minus_var_u16_i8_Frame_t {
#line 249 "Minus.m3"
ADDRESS _unused;
#line 249 "Minus.m3"
};
#line 249 "Minus.m3"
Minus__UINT16
__cdecl
Minus__Minus_var_u16_i8(void)
{
#line 249 "Minus.m3"
 /* Var_Type2 */ INT64 Minus_m_1240_L_1241={0};//always-init
#line 249 "Minus.m3"
Minus__Minus_var_u16_i8_Frame_t _frame;
#line 249 "Minus.m3"
_frame._unused=(ADDRESS)&_frame;
#line 249 "Minus.m3"
 /* load */
#line 249 "Minus.m3"
 /* load */
#line 249 "Minus.m3"
 /* subtract */
#line 249 "Minus.m3"
 /* check_range */
#line 249 "Minus.m3"
 /* store */
#line 249 "Minus.m3"
(*(INT64*)(&Minus_m_1240_L_1241))=(INT64)( ((INT64)( ((INT64)(*((UINT16*)(INT64_(144)+((ADDRESS)(&Minus_m_M_Minus_L_13))))))- ((INT64)(*((INT8*)(INT64_(104)+((ADDRESS)(&Minus_m_M_Minus_L_13)))))))));
#line 249 "Minus.m3"
 /* load */
#line 249 "Minus.m3"
if(m3_check_range(INT64,
Minus_m_1240_L_1241,
 INT64_(0),
 INT64_(65535)))
#line 249 "Minus.m3"
Minus_m_M_Minus_L_13_CRASH(7969);
#line 249 "Minus.m3"
 /* exit_proc */
#line 249 "Minus.m3"
return Minus_m_1240_L_1241;
#line 249 "Minus.m3"
 /* end_procedure */
#line 249 "Minus.m3"
} /* uMinus_param_u16_i8 */
#line 249 "Minus.m3"
 /* set_source_line */
#line 249 "Minus.m3"
#line 250 "Minus.m3"
 /* begin_procedure */
#line 250 "Minus.m3"
struct Minus__uMinus_param_u16_i8_Frame_t {
#line 250 "Minus.m3"
ADDRESS _unused;
#line 250 "Minus.m3"
};
#line 250 "Minus.m3"
Word__T
__cdecl
Minus__uMinus_param_u16_i8(
   /* Param_Type1 */ Minus__UINT16 a_L_407,
   /* Param_Type1 */ Minus__INT8 b_L_408)
{
#line 250 "Minus.m3"
Minus__uMinus_param_u16_i8_Frame_t _frame;
#line 250 "Minus.m3"
_frame._unused=(ADDRESS)&_frame;
#line 250 "Minus.m3"
 /* load */
#line 250 "Minus.m3"
 /* load */
#line 250 "Minus.m3"
 /* subtract */
#line 250 "Minus.m3"
 /* exit_proc */
#line 250 "Minus.m3"
return ((UINT64)(((UINT64)(((INT64)(a_L_407))))-((UINT64)(((INT64)(b_L_408))))));
#line 250 "Minus.m3"
 /* end_procedure */
#line 250 "Minus.m3"
} /* Minus_param_u16_i8 */
#line 250 "Minus.m3"
 /* set_source_line */
#line 250 "Minus.m3"
#line 251 "Minus.m3"
 /* begin_procedure */
#line 251 "Minus.m3"
struct Minus__Minus_param_u16_i8_Frame_t {
#line 251 "Minus.m3"
ADDRESS _unused;
#line 251 "Minus.m3"
};
#line 251 "Minus.m3"
Minus__UINT16
__cdecl
Minus__Minus_param_u16_i8(
   /* Param_Type1 */ Minus__UINT16 a_L_410,
   /* Param_Type1 */ Minus__INT8 b_L_411)
{
#line 251 "Minus.m3"
 /* Var_Type2 */ INT64 Minus_m_1242_L_1243={0};//always-init
#line 251 "Minus.m3"
Minus__Minus_param_u16_i8_Frame_t _frame;
#line 251 "Minus.m3"
_frame._unused=(ADDRESS)&_frame;
#line 251 "Minus.m3"
 /* load */
#line 251 "Minus.m3"
 /* load */
#line 251 "Minus.m3"
 /* subtract */
#line 251 "Minus.m3"
 /* check_range */
#line 251 "Minus.m3"
 /* store */
#line 251 "Minus.m3"
(*(INT64*)(&Minus_m_1242_L_1243))=(INT64)( ((INT64)( ((INT64)(a_L_410))- ((INT64)(b_L_411)))));
#line 251 "Minus.m3"
 /* load */
#line 251 "Minus.m3"
if(m3_check_range(INT64,
Minus_m_1242_L_1243,
 INT64_(0),
 INT64_(65535)))
#line 251 "Minus.m3"
Minus_m_M_Minus_L_13_CRASH(8033);
#line 251 "Minus.m3"
 /* exit_proc */
#line 251 "Minus.m3"
return Minus_m_1242_L_1243;
#line 251 "Minus.m3"
 /* end_procedure */
#line 251 "Minus.m3"
} /* uMinus_var_u16_u64 */
#line 251 "Minus.m3"
 /* set_source_line */
#line 251 "Minus.m3"
#line 252 "Minus.m3"
 /* begin_procedure */
#line 252 "Minus.m3"
struct Minus__uMinus_var_u16_u64_Frame_t {
#line 252 "Minus.m3"
ADDRESS _unused;
#line 252 "Minus.m3"
};
#line 252 "Minus.m3"
Long__T
__cdecl
Minus__uMinus_var_u16_u64(void)
{
#line 252 "Minus.m3"
Minus__uMinus_var_u16_u64_Frame_t _frame;
#line 252 "Minus.m3"
_frame._unused=(ADDRESS)&_frame;
#line 252 "Minus.m3"
 /* load */
#line 252 "Minus.m3"
 /* loophole */
#line 252 "Minus.m3"
 /* load */
#line 252 "Minus.m3"
 /* subtract */
#line 252 "Minus.m3"
 /* exit_proc */
#line 252 "Minus.m3"
return ((UINT64)(((UINT64)((INT64)((INT64)(*((UINT16*)(INT64_(144)+((ADDRESS)(&Minus_m_M_Minus_L_13))))))))-((UINT64)(*((INT64*)(INT64_(112)+((ADDRESS)(&Minus_m_M_Minus_L_13))))))));
#line 252 "Minus.m3"
 /* end_procedure */
#line 252 "Minus.m3"
} /* Minus_var_u16_u64 */
#line 252 "Minus.m3"
 /* set_source_line */
#line 252 "Minus.m3"
#line 253 "Minus.m3"
 /* begin_procedure */
#line 253 "Minus.m3"
struct Minus__Minus_var_u16_u64_Frame_t {
#line 253 "Minus.m3"
ADDRESS _unused;
#line 253 "Minus.m3"
};
#line 253 "Minus.m3"
LONGINT
__cdecl
Minus__Minus_var_u16_u64(void)
{
#line 253 "Minus.m3"
Minus__Minus_var_u16_u64_Frame_t _frame;
#line 253 "Minus.m3"
_frame._unused=(ADDRESS)&_frame;
#line 253 "Minus.m3"
 /* load */
#line 253 "Minus.m3"
 /* loophole */
#line 253 "Minus.m3"
 /* load */
#line 253 "Minus.m3"
 /* subtract */
#line 253 "Minus.m3"
 /* exit_proc */
#line 253 "Minus.m3"
return ((INT64)(((INT64)((INT64)((INT64)(*((UINT16*)(INT64_(144)+((ADDRESS)(&Minus_m_M_Minus_L_13))))))))-((INT64)(*((INT64*)(INT64_(112)+((ADDRESS)(&Minus_m_M_Minus_L_13))))))));
#line 253 "Minus.m3"
 /* end_procedure */
#line 253 "Minus.m3"
} /* uMinus_param_u16_u64 */
#line 253 "Minus.m3"
 /* set_source_line */
#line 253 "Minus.m3"
#line 254 "Minus.m3"
 /* begin_procedure */
#line 254 "Minus.m3"
struct Minus__uMinus_param_u16_u64_Frame_t {
#line 254 "Minus.m3"
ADDRESS _unused;
#line 254 "Minus.m3"
};
#line 254 "Minus.m3"
Long__T
__cdecl
Minus__uMinus_param_u16_u64(
   /* Param_Type1 */ Minus__UINT16 a_L_415,
   /* Param_Type1 */ Minus__UINT64 b_L_416)
{
#line 254 "Minus.m3"
Minus__uMinus_param_u16_u64_Frame_t _frame;
#line 254 "Minus.m3"
_frame._unused=(ADDRESS)&_frame;
#line 254 "Minus.m3"
 /* load */
#line 254 "Minus.m3"
 /* loophole */
#line 254 "Minus.m3"
 /* load */
#line 254 "Minus.m3"
 /* subtract */
#line 254 "Minus.m3"
 /* exit_proc */
#line 254 "Minus.m3"
return ((UINT64)(((UINT64)((INT64)((INT64)(a_L_415))))-((UINT64)(b_L_416))));
#line 254 "Minus.m3"
 /* end_procedure */
#line 254 "Minus.m3"
} /* Minus_param_u16_u64 */
#line 254 "Minus.m3"
 /* set_source_line */
#line 254 "Minus.m3"
#line 255 "Minus.m3"
 /* begin_procedure */
#line 255 "Minus.m3"
struct Minus__Minus_param_u16_u64_Frame_t {
#line 255 "Minus.m3"
ADDRESS _unused;
#line 255 "Minus.m3"
};
#line 255 "Minus.m3"
LONGINT
__cdecl
Minus__Minus_param_u16_u64(
   /* Param_Type1 */ Minus__UINT16 a_L_418,
   /* Param_Type1 */ Minus__UINT64 b_L_419)
{
#line 255 "Minus.m3"
Minus__Minus_param_u16_u64_Frame_t _frame;
#line 255 "Minus.m3"
_frame._unused=(ADDRESS)&_frame;
#line 255 "Minus.m3"
 /* load */
#line 255 "Minus.m3"
 /* loophole */
#line 255 "Minus.m3"
 /* load */
#line 255 "Minus.m3"
 /* subtract */
#line 255 "Minus.m3"
 /* exit_proc */
#line 255 "Minus.m3"
return ((INT64)(((INT64)((INT64)((INT64)(a_L_418))))- b_L_419));
#line 255 "Minus.m3"
 /* end_procedure */
#line 255 "Minus.m3"
} /* uMinus_var_u16_i32 */
#line 255 "Minus.m3"
 /* set_source_line */
#line 255 "Minus.m3"
#line 256 "Minus.m3"
 /* begin_procedure */
#line 256 "Minus.m3"
struct Minus__uMinus_var_u16_i32_Frame_t {
#line 256 "Minus.m3"
ADDRESS _unused;
#line 256 "Minus.m3"
};
#line 256 "Minus.m3"
Word__T
__cdecl
Minus__uMinus_var_u16_i32(void)
{
#line 256 "Minus.m3"
Minus__uMinus_var_u16_i32_Frame_t _frame;
#line 256 "Minus.m3"
_frame._unused=(ADDRESS)&_frame;
#line 256 "Minus.m3"
 /* load */
#line 256 "Minus.m3"
 /* load */
#line 256 "Minus.m3"
 /* subtract */
#line 256 "Minus.m3"
 /* exit_proc */
#line 256 "Minus.m3"
return ((UINT64)(((UINT64)(((INT64)(*((UINT16*)(INT64_(144)+((ADDRESS)(&Minus_m_M_Minus_L_13))))))))-((UINT64)(((INT64)(*((INT32*)(INT64_(128)+((ADDRESS)(&Minus_m_M_Minus_L_13))))))))));
#line 256 "Minus.m3"
 /* end_procedure */
#line 256 "Minus.m3"
} /* Minus_var_u16_i32 */
#line 256 "Minus.m3"
 /* set_source_line */
#line 256 "Minus.m3"
#line 257 "Minus.m3"
 /* begin_procedure */
#line 257 "Minus.m3"
struct Minus__Minus_var_u16_i32_Frame_t {
#line 257 "Minus.m3"
ADDRESS _unused;
#line 257 "Minus.m3"
};
#line 257 "Minus.m3"
Minus__UINT16
__cdecl
Minus__Minus_var_u16_i32(void)
{
#line 257 "Minus.m3"
 /* Var_Type2 */ INT64 Minus_m_1244_L_1245={0};//always-init
#line 257 "Minus.m3"
Minus__Minus_var_u16_i32_Frame_t _frame;
#line 257 "Minus.m3"
_frame._unused=(ADDRESS)&_frame;
#line 257 "Minus.m3"
 /* load */
#line 257 "Minus.m3"
 /* load */
#line 257 "Minus.m3"
 /* subtract */
#line 257 "Minus.m3"
 /* check_range */
#line 257 "Minus.m3"
 /* store */
#line 257 "Minus.m3"
(*(INT64*)(&Minus_m_1244_L_1245))=(INT64)( ((INT64)( ((INT64)(*((UINT16*)(INT64_(144)+((ADDRESS)(&Minus_m_M_Minus_L_13))))))- ((INT64)(*((INT32*)(INT64_(128)+((ADDRESS)(&Minus_m_M_Minus_L_13)))))))));
#line 257 "Minus.m3"
 /* load */
#line 257 "Minus.m3"
if(m3_check_range(INT64,
Minus_m_1244_L_1245,
 INT64_(0),
 INT64_(65535)))
#line 257 "Minus.m3"
Minus_m_M_Minus_L_13_CRASH(8225);
#line 257 "Minus.m3"
 /* exit_proc */
#line 257 "Minus.m3"
return Minus_m_1244_L_1245;
#line 257 "Minus.m3"
 /* end_procedure */
#line 257 "Minus.m3"
} /* uMinus_param_u16_i32 */
#line 257 "Minus.m3"
 /* set_source_line */
#line 257 "Minus.m3"
#line 258 "Minus.m3"
 /* begin_procedure */
#line 258 "Minus.m3"
struct Minus__uMinus_param_u16_i32_Frame_t {
#line 258 "Minus.m3"
ADDRESS _unused;
#line 258 "Minus.m3"
};
#line 258 "Minus.m3"
Word__T
__cdecl
Minus__uMinus_param_u16_i32(
   /* Param_Type1 */ Minus__UINT16 a_L_423,
   /* Param_Type1 */ Minus__INT32 b_L_424)
{
#line 258 "Minus.m3"
Minus__uMinus_param_u16_i32_Frame_t _frame;
#line 258 "Minus.m3"
_frame._unused=(ADDRESS)&_frame;
#line 258 "Minus.m3"
 /* load */
#line 258 "Minus.m3"
 /* load */
#line 258 "Minus.m3"
 /* subtract */
#line 258 "Minus.m3"
 /* exit_proc */
#line 258 "Minus.m3"
return ((UINT64)(((UINT64)(((INT64)(a_L_423))))-((UINT64)(((INT64)(b_L_424))))));
#line 258 "Minus.m3"
 /* end_procedure */
#line 258 "Minus.m3"
} /* Minus_param_u16_i32 */
#line 258 "Minus.m3"
 /* set_source_line */
#line 258 "Minus.m3"
#line 259 "Minus.m3"
 /* begin_procedure */
#line 259 "Minus.m3"
struct Minus__Minus_param_u16_i32_Frame_t {
#line 259 "Minus.m3"
ADDRESS _unused;
#line 259 "Minus.m3"
};
#line 259 "Minus.m3"
Minus__UINT16
__cdecl
Minus__Minus_param_u16_i32(
   /* Param_Type1 */ Minus__UINT16 a_L_426,
   /* Param_Type1 */ Minus__INT32 b_L_427)
{
#line 259 "Minus.m3"
 /* Var_Type2 */ INT64 Minus_m_1246_L_1247={0};//always-init
#line 259 "Minus.m3"
Minus__Minus_param_u16_i32_Frame_t _frame;
#line 259 "Minus.m3"
_frame._unused=(ADDRESS)&_frame;
#line 259 "Minus.m3"
 /* load */
#line 259 "Minus.m3"
 /* load */
#line 259 "Minus.m3"
 /* subtract */
#line 259 "Minus.m3"
 /* check_range */
#line 259 "Minus.m3"
 /* store */
#line 259 "Minus.m3"
(*(INT64*)(&Minus_m_1246_L_1247))=(INT64)( ((INT64)( ((INT64)(a_L_426))- ((INT64)(b_L_427)))));
#line 259 "Minus.m3"
 /* load */
#line 259 "Minus.m3"
if(m3_check_range(INT64,
Minus_m_1246_L_1247,
 INT64_(0),
 INT64_(65535)))
#line 259 "Minus.m3"
Minus_m_M_Minus_L_13_CRASH(8289);
#line 259 "Minus.m3"
 /* exit_proc */
#line 259 "Minus.m3"
return Minus_m_1246_L_1247;
#line 259 "Minus.m3"
 /* end_procedure */
#line 259 "Minus.m3"
} /* uMinus_var_u16_LC */
#line 259 "Minus.m3"
 /* set_source_line */
#line 259 "Minus.m3"
#line 260 "Minus.m3"
 /* begin_procedure */
#line 260 "Minus.m3"
struct Minus__uMinus_var_u16_LC_Frame_t {
#line 260 "Minus.m3"
ADDRESS _unused;
#line 260 "Minus.m3"
};
#line 260 "Minus.m3"
Long__T
__cdecl
Minus__uMinus_var_u16_LC(void)
{
#line 260 "Minus.m3"
Minus__uMinus_var_u16_LC_Frame_t _frame;
#line 260 "Minus.m3"
_frame._unused=(ADDRESS)&_frame;
#line 260 "Minus.m3"
 /* load */
#line 260 "Minus.m3"
 /* loophole */
#line 260 "Minus.m3"
 /* load */
#line 260 "Minus.m3"
 /* subtract */
#line 260 "Minus.m3"
 /* exit_proc */
#line 260 "Minus.m3"
return ((UINT64)(((UINT64)((INT64)((INT64)(*((UINT16*)(INT64_(144)+((ADDRESS)(&Minus_m_M_Minus_L_13))))))))-((UINT64)(((INT64)(*((UINT64*)(INT64_(136)+((ADDRESS)(&Minus_m_M_Minus_L_13))))))))));
#line 260 "Minus.m3"
 /* end_procedure */
#line 260 "Minus.m3"
} /* Minus_var_u16_LC */
#line 260 "Minus.m3"
 /* set_source_line */
#line 260 "Minus.m3"
#line 261 "Minus.m3"
 /* begin_procedure */
#line 261 "Minus.m3"
struct Minus__Minus_var_u16_LC_Frame_t {
#line 261 "Minus.m3"
ADDRESS _unused;
#line 261 "Minus.m3"
};
#line 261 "Minus.m3"
LONGINT
__cdecl
Minus__Minus_var_u16_LC(void)
{
#line 261 "Minus.m3"
Minus__Minus_var_u16_LC_Frame_t _frame;
#line 261 "Minus.m3"
_frame._unused=(ADDRESS)&_frame;
#line 261 "Minus.m3"
 /* load */
#line 261 "Minus.m3"
 /* loophole */
#line 261 "Minus.m3"
 /* load */
#line 261 "Minus.m3"
 /* subtract */
#line 261 "Minus.m3"
 /* exit_proc */
#line 261 "Minus.m3"
return ((INT64)(((INT64)((INT64)((INT64)(*((UINT16*)(INT64_(144)+((ADDRESS)(&Minus_m_M_Minus_L_13))))))))- ((INT64)(*((UINT64*)(INT64_(136)+((ADDRESS)(&Minus_m_M_Minus_L_13))))))));
#line 261 "Minus.m3"
 /* end_procedure */
#line 261 "Minus.m3"
} /* uMinus_param_u16_LC */
#line 261 "Minus.m3"
 /* set_source_line */
#line 261 "Minus.m3"
#line 262 "Minus.m3"
 /* begin_procedure */
#line 262 "Minus.m3"
struct Minus__uMinus_param_u16_LC_Frame_t {
#line 262 "Minus.m3"
ADDRESS _unused;
#line 262 "Minus.m3"
};
#line 262 "Minus.m3"
Long__T
__cdecl
Minus__uMinus_param_u16_LC(
   /* Param_Type1 */ Minus__UINT16 a_L_431,
   /* Param_Type1 */ LONGCARD b_L_432)
{
#line 262 "Minus.m3"
Minus__uMinus_param_u16_LC_Frame_t _frame;
#line 262 "Minus.m3"
_frame._unused=(ADDRESS)&_frame;
#line 262 "Minus.m3"
 /* load */
#line 262 "Minus.m3"
 /* loophole */
#line 262 "Minus.m3"
 /* load */
#line 262 "Minus.m3"
 /* subtract */
#line 262 "Minus.m3"
 /* exit_proc */
#line 262 "Minus.m3"
return ((UINT64)(((UINT64)((INT64)((INT64)(a_L_431))))-((UINT64)(((INT64)(b_L_432))))));
#line 262 "Minus.m3"
 /* end_procedure */
#line 262 "Minus.m3"
} /* Minus_param_u16_LC */
#line 262 "Minus.m3"
 /* set_source_line */
#line 262 "Minus.m3"
#line 263 "Minus.m3"
 /* begin_procedure */
#line 263 "Minus.m3"
struct Minus__Minus_param_u16_LC_Frame_t {
#line 263 "Minus.m3"
ADDRESS _unused;
#line 263 "Minus.m3"
};
#line 263 "Minus.m3"
LONGINT
__cdecl
Minus__Minus_param_u16_LC(
   /* Param_Type1 */ Minus__UINT16 a_L_434,
   /* Param_Type1 */ LONGCARD b_L_435)
{
#line 263 "Minus.m3"
Minus__Minus_param_u16_LC_Frame_t _frame;
#line 263 "Minus.m3"
_frame._unused=(ADDRESS)&_frame;
#line 263 "Minus.m3"
 /* load */
#line 263 "Minus.m3"
 /* loophole */
#line 263 "Minus.m3"
 /* load */
#line 263 "Minus.m3"
 /* subtract */
#line 263 "Minus.m3"
 /* exit_proc */
#line 263 "Minus.m3"
return ((INT64)(((INT64)((INT64)((INT64)(a_L_434))))- ((INT64)(b_L_435))));
#line 263 "Minus.m3"
 /* end_procedure */
#line 263 "Minus.m3"
} /* uMinus_var_u16_u16 */
#line 263 "Minus.m3"
 /* set_source_line */
#line 263 "Minus.m3"
#line 264 "Minus.m3"
 /* begin_procedure */
#line 264 "Minus.m3"
struct Minus__uMinus_var_u16_u16_Frame_t {
#line 264 "Minus.m3"
ADDRESS _unused;
#line 264 "Minus.m3"
};
#line 264 "Minus.m3"
Word__T
__cdecl
Minus__uMinus_var_u16_u16(void)
{
#line 264 "Minus.m3"
Minus__uMinus_var_u16_u16_Frame_t _frame;
#line 264 "Minus.m3"
_frame._unused=(ADDRESS)&_frame;
#line 264 "Minus.m3"
 /* load */
#line 264 "Minus.m3"
 /* load */
#line 264 "Minus.m3"
 /* subtract */
#line 264 "Minus.m3"
 /* exit_proc */
#line 264 "Minus.m3"
return ((UINT64)(((UINT64)(((INT64)(*((UINT16*)(INT64_(144)+((ADDRESS)(&Minus_m_M_Minus_L_13))))))))-((UINT64)(((INT64)(*((UINT16*)(INT64_(144)+((ADDRESS)(&Minus_m_M_Minus_L_13))))))))));
#line 264 "Minus.m3"
 /* end_procedure */
#line 264 "Minus.m3"
} /* Minus_var_u16_u16 */
#line 264 "Minus.m3"
 /* set_source_line */
#line 264 "Minus.m3"
#line 265 "Minus.m3"
 /* begin_procedure */
#line 265 "Minus.m3"
struct Minus__Minus_var_u16_u16_Frame_t {
#line 265 "Minus.m3"
ADDRESS _unused;
#line 265 "Minus.m3"
};
#line 265 "Minus.m3"
Minus__UINT16
__cdecl
Minus__Minus_var_u16_u16(void)
{
#line 265 "Minus.m3"
 /* Var_Type2 */ INT64 Minus_m_1248_L_1249={0};//always-init
#line 265 "Minus.m3"
Minus__Minus_var_u16_u16_Frame_t _frame;
#line 265 "Minus.m3"
_frame._unused=(ADDRESS)&_frame;
#line 265 "Minus.m3"
 /* load */
#line 265 "Minus.m3"
 /* load */
#line 265 "Minus.m3"
 /* subtract */
#line 265 "Minus.m3"
 /* check_lo */
#line 265 "Minus.m3"
 /* store */
#line 265 "Minus.m3"
(*(INT64*)(&Minus_m_1248_L_1249))=(INT64)( ((INT64)( ((INT64)(*((UINT16*)(INT64_(144)+((ADDRESS)(&Minus_m_M_Minus_L_13))))))- ((INT64)(*((UINT16*)(INT64_(144)+((ADDRESS)(&Minus_m_M_Minus_L_13)))))))));
#line 265 "Minus.m3"
 /* load */
#line 265 "Minus.m3"
/*check_lo*/if(Minus_m_1248_L_1249<INT64_(0))Minus_m_M_Minus_L_13_CRASH(8481);
#line 265 "Minus.m3"
 /* exit_proc */
#line 265 "Minus.m3"
return Minus_m_1248_L_1249;
#line 265 "Minus.m3"
 /* end_procedure */
#line 265 "Minus.m3"
} /* uMinus_param_u16_u16 */
#line 265 "Minus.m3"
 /* set_source_line */
#line 265 "Minus.m3"
#line 266 "Minus.m3"
 /* begin_procedure */
#line 266 "Minus.m3"
struct Minus__uMinus_param_u16_u16_Frame_t {
#line 266 "Minus.m3"
ADDRESS _unused;
#line 266 "Minus.m3"
};
#line 266 "Minus.m3"
Word__T
__cdecl
Minus__uMinus_param_u16_u16(
   /* Param_Type1 */ Minus__UINT16 a_L_439,
   /* Param_Type1 */ Minus__UINT16 b_L_440)
{
#line 266 "Minus.m3"
Minus__uMinus_param_u16_u16_Frame_t _frame;
#line 266 "Minus.m3"
_frame._unused=(ADDRESS)&_frame;
#line 266 "Minus.m3"
 /* load */
#line 266 "Minus.m3"
 /* load */
#line 266 "Minus.m3"
 /* subtract */
#line 266 "Minus.m3"
 /* exit_proc */
#line 266 "Minus.m3"
return ((UINT64)(((UINT64)(((INT64)(a_L_439))))-((UINT64)(((INT64)(b_L_440))))));
#line 266 "Minus.m3"
 /* end_procedure */
#line 266 "Minus.m3"
} /* Minus_param_u16_u16 */
#line 266 "Minus.m3"
 /* set_source_line */
#line 266 "Minus.m3"
#line 267 "Minus.m3"
 /* begin_procedure */
#line 267 "Minus.m3"
struct Minus__Minus_param_u16_u16_Frame_t {
#line 267 "Minus.m3"
ADDRESS _unused;
#line 267 "Minus.m3"
};
#line 267 "Minus.m3"
Minus__UINT16
__cdecl
Minus__Minus_param_u16_u16(
   /* Param_Type1 */ Minus__UINT16 a_L_442,
   /* Param_Type1 */ Minus__UINT16 b_L_443)
{
#line 267 "Minus.m3"
 /* Var_Type2 */ INT64 Minus_m_1250_L_1251={0};//always-init
#line 267 "Minus.m3"
Minus__Minus_param_u16_u16_Frame_t _frame;
#line 267 "Minus.m3"
_frame._unused=(ADDRESS)&_frame;
#line 267 "Minus.m3"
 /* load */
#line 267 "Minus.m3"
 /* load */
#line 267 "Minus.m3"
 /* subtract */
#line 267 "Minus.m3"
 /* check_lo */
#line 267 "Minus.m3"
 /* store */
#line 267 "Minus.m3"
(*(INT64*)(&Minus_m_1250_L_1251))=(INT64)( ((INT64)( ((INT64)(a_L_442))- ((INT64)(b_L_443)))));
#line 267 "Minus.m3"
 /* load */
#line 267 "Minus.m3"
/*check_lo*/if(Minus_m_1250_L_1251<INT64_(0))Minus_m_M_Minus_L_13_CRASH(8545);
#line 267 "Minus.m3"
 /* exit_proc */
#line 267 "Minus.m3"
return Minus_m_1250_L_1251;
#line 267 "Minus.m3"
 /* end_procedure */
#line 267 "Minus.m3"
} /* uMinus_var_u16_I */
#line 267 "Minus.m3"
 /* set_source_line */
#line 267 "Minus.m3"
#line 268 "Minus.m3"
 /* begin_procedure */
#line 268 "Minus.m3"
struct Minus__uMinus_var_u16_I_Frame_t {
#line 268 "Minus.m3"
ADDRESS _unused;
#line 268 "Minus.m3"
};
#line 268 "Minus.m3"
Word__T
__cdecl
Minus__uMinus_var_u16_I(void)
{
#line 268 "Minus.m3"
Minus__uMinus_var_u16_I_Frame_t _frame;
#line 268 "Minus.m3"
_frame._unused=(ADDRESS)&_frame;
#line 268 "Minus.m3"
 /* load */
#line 268 "Minus.m3"
 /* load */
#line 268 "Minus.m3"
 /* subtract */
#line 268 "Minus.m3"
 /* exit_proc */
#line 268 "Minus.m3"
return ((UINT64)(((UINT64)(((INT64)(*((UINT16*)(INT64_(144)+((ADDRESS)(&Minus_m_M_Minus_L_13))))))))-((UINT64)(*((INT64*)(INT64_(152)+((ADDRESS)(&Minus_m_M_Minus_L_13))))))));
#line 268 "Minus.m3"
 /* end_procedure */
#line 268 "Minus.m3"
} /* Minus_var_u16_I */
#line 268 "Minus.m3"
 /* set_source_line */
#line 268 "Minus.m3"
#line 269 "Minus.m3"
 /* begin_procedure */
#line 269 "Minus.m3"
struct Minus__Minus_var_u16_I_Frame_t {
#line 269 "Minus.m3"
ADDRESS _unused;
#line 269 "Minus.m3"
};
#line 269 "Minus.m3"
Minus__UINT16
__cdecl
Minus__Minus_var_u16_I(void)
{
#line 269 "Minus.m3"
 /* Var_Type2 */ INT64 Minus_m_1252_L_1253={0};//always-init
#line 269 "Minus.m3"
Minus__Minus_var_u16_I_Frame_t _frame;
#line 269 "Minus.m3"
_frame._unused=(ADDRESS)&_frame;
#line 269 "Minus.m3"
 /* load */
#line 269 "Minus.m3"
 /* load */
#line 269 "Minus.m3"
 /* subtract */
#line 269 "Minus.m3"
 /* check_range */
#line 269 "Minus.m3"
 /* store */
#line 269 "Minus.m3"
(*(INT64*)(&Minus_m_1252_L_1253))=(INT64)( ((INT64)( ((INT64)(*((UINT16*)(INT64_(144)+((ADDRESS)(&Minus_m_M_Minus_L_13))))))-((INT64)(*((INT64*)(INT64_(152)+((ADDRESS)(&Minus_m_M_Minus_L_13)))))))));
#line 269 "Minus.m3"
 /* load */
#line 269 "Minus.m3"
if(m3_check_range(INT64,
Minus_m_1252_L_1253,
 INT64_(0),
 INT64_(65535)))
#line 269 "Minus.m3"
Minus_m_M_Minus_L_13_CRASH(8609);
#line 269 "Minus.m3"
 /* exit_proc */
#line 269 "Minus.m3"
return Minus_m_1252_L_1253;
#line 269 "Minus.m3"
 /* end_procedure */
#line 269 "Minus.m3"
} /* uMinus_param_u16_I */
#line 269 "Minus.m3"
 /* set_source_line */
#line 269 "Minus.m3"
#line 270 "Minus.m3"
 /* begin_procedure */
#line 270 "Minus.m3"
struct Minus__uMinus_param_u16_I_Frame_t {
#line 270 "Minus.m3"
ADDRESS _unused;
#line 270 "Minus.m3"
};
#line 270 "Minus.m3"
Word__T
__cdecl
Minus__uMinus_param_u16_I(
   /* Param_Type1 */ Minus__UINT16 a_L_447,
   /* Param_Type1 */ INTEGER b_L_448)
{
#line 270 "Minus.m3"
Minus__uMinus_param_u16_I_Frame_t _frame;
#line 270 "Minus.m3"
_frame._unused=(ADDRESS)&_frame;
#line 270 "Minus.m3"
 /* load */
#line 270 "Minus.m3"
 /* load */
#line 270 "Minus.m3"
 /* subtract */
#line 270 "Minus.m3"
 /* exit_proc */
#line 270 "Minus.m3"
return ((UINT64)(((UINT64)(((INT64)(a_L_447))))-((UINT64)(b_L_448))));
#line 270 "Minus.m3"
 /* end_procedure */
#line 270 "Minus.m3"
} /* Minus_param_u16_I */
#line 270 "Minus.m3"
 /* set_source_line */
#line 270 "Minus.m3"
#line 271 "Minus.m3"
 /* begin_procedure */
#line 271 "Minus.m3"
struct Minus__Minus_param_u16_I_Frame_t {
#line 271 "Minus.m3"
ADDRESS _unused;
#line 271 "Minus.m3"
};
#line 271 "Minus.m3"
Minus__UINT16
__cdecl
Minus__Minus_param_u16_I(
   /* Param_Type1 */ Minus__UINT16 a_L_450,
   /* Param_Type1 */ INTEGER b_L_451)
{
#line 271 "Minus.m3"
 /* Var_Type2 */ INT64 Minus_m_1254_L_1255={0};//always-init
#line 271 "Minus.m3"
Minus__Minus_param_u16_I_Frame_t _frame;
#line 271 "Minus.m3"
_frame._unused=(ADDRESS)&_frame;
#line 271 "Minus.m3"
 /* load */
#line 271 "Minus.m3"
 /* load */
#line 271 "Minus.m3"
 /* subtract */
#line 271 "Minus.m3"
 /* check_range */
#line 271 "Minus.m3"
 /* store */
#line 271 "Minus.m3"
(*(INT64*)(&Minus_m_1254_L_1255))=(INT64)( ((INT64)( ((INT64)(a_L_450))- b_L_451)));
#line 271 "Minus.m3"
 /* load */
#line 271 "Minus.m3"
if(m3_check_range(INT64,
Minus_m_1254_L_1255,
 INT64_(0),
 INT64_(65535)))
#line 271 "Minus.m3"
Minus_m_M_Minus_L_13_CRASH(8673);
#line 271 "Minus.m3"
 /* exit_proc */
#line 271 "Minus.m3"
return Minus_m_1254_L_1255;
#line 271 "Minus.m3"
 /* end_procedure */
#line 271 "Minus.m3"
} /* uMinus_var_u16_i64 */
#line 271 "Minus.m3"
 /* set_source_line */
#line 271 "Minus.m3"
#line 272 "Minus.m3"
 /* begin_procedure */
#line 272 "Minus.m3"
struct Minus__uMinus_var_u16_i64_Frame_t {
#line 272 "Minus.m3"
ADDRESS _unused;
#line 272 "Minus.m3"
};
#line 272 "Minus.m3"
Long__T
__cdecl
Minus__uMinus_var_u16_i64(void)
{
#line 272 "Minus.m3"
Minus__uMinus_var_u16_i64_Frame_t _frame;
#line 272 "Minus.m3"
_frame._unused=(ADDRESS)&_frame;
#line 272 "Minus.m3"
 /* load */
#line 272 "Minus.m3"
 /* loophole */
#line 272 "Minus.m3"
 /* load */
#line 272 "Minus.m3"
 /* subtract */
#line 272 "Minus.m3"
 /* exit_proc */
#line 272 "Minus.m3"
return ((UINT64)(((UINT64)((INT64)((INT64)(*((UINT16*)(INT64_(144)+((ADDRESS)(&Minus_m_M_Minus_L_13))))))))-((UINT64)(*((INT64*)(INT64_(160)+((ADDRESS)(&Minus_m_M_Minus_L_13))))))));
#line 272 "Minus.m3"
 /* end_procedure */
#line 272 "Minus.m3"
} /* Minus_var_u16_i64 */
#line 272 "Minus.m3"
 /* set_source_line */
#line 272 "Minus.m3"
#line 273 "Minus.m3"
 /* begin_procedure */
#line 273 "Minus.m3"
struct Minus__Minus_var_u16_i64_Frame_t {
#line 273 "Minus.m3"
ADDRESS _unused;
#line 273 "Minus.m3"
};
#line 273 "Minus.m3"
LONGINT
__cdecl
Minus__Minus_var_u16_i64(void)
{
#line 273 "Minus.m3"
Minus__Minus_var_u16_i64_Frame_t _frame;
#line 273 "Minus.m3"
_frame._unused=(ADDRESS)&_frame;
#line 273 "Minus.m3"
 /* load */
#line 273 "Minus.m3"
 /* loophole */
#line 273 "Minus.m3"
 /* load */
#line 273 "Minus.m3"
 /* subtract */
#line 273 "Minus.m3"
 /* exit_proc */
#line 273 "Minus.m3"
return ((INT64)(((INT64)((INT64)((INT64)(*((UINT16*)(INT64_(144)+((ADDRESS)(&Minus_m_M_Minus_L_13))))))))-((INT64)(*((INT64*)(INT64_(160)+((ADDRESS)(&Minus_m_M_Minus_L_13))))))));
#line 273 "Minus.m3"
 /* end_procedure */
#line 273 "Minus.m3"
} /* uMinus_param_u16_i64 */
#line 273 "Minus.m3"
 /* set_source_line */
#line 273 "Minus.m3"
#line 274 "Minus.m3"
 /* begin_procedure */
#line 274 "Minus.m3"
struct Minus__uMinus_param_u16_i64_Frame_t {
#line 274 "Minus.m3"
ADDRESS _unused;
#line 274 "Minus.m3"
};
#line 274 "Minus.m3"
Long__T
__cdecl
Minus__uMinus_param_u16_i64(
   /* Param_Type1 */ Minus__UINT16 a_L_455,
   /* Param_Type1 */ Minus__INT64 b_L_456)
{
#line 274 "Minus.m3"
Minus__uMinus_param_u16_i64_Frame_t _frame;
#line 274 "Minus.m3"
_frame._unused=(ADDRESS)&_frame;
#line 274 "Minus.m3"
 /* load */
#line 274 "Minus.m3"
 /* loophole */
#line 274 "Minus.m3"
 /* load */
#line 274 "Minus.m3"
 /* subtract */
#line 274 "Minus.m3"
 /* exit_proc */
#line 274 "Minus.m3"
return ((UINT64)(((UINT64)((INT64)((INT64)(a_L_455))))-((UINT64)(b_L_456))));
#line 274 "Minus.m3"
 /* end_procedure */
#line 274 "Minus.m3"
} /* Minus_param_u16_i64 */
#line 274 "Minus.m3"
 /* set_source_line */
#line 274 "Minus.m3"
#line 275 "Minus.m3"
 /* begin_procedure */
#line 275 "Minus.m3"
struct Minus__Minus_param_u16_i64_Frame_t {
#line 275 "Minus.m3"
ADDRESS _unused;
#line 275 "Minus.m3"
};
#line 275 "Minus.m3"
LONGINT
__cdecl
Minus__Minus_param_u16_i64(
   /* Param_Type1 */ Minus__UINT16 a_L_458,
   /* Param_Type1 */ Minus__INT64 b_L_459)
{
#line 275 "Minus.m3"
Minus__Minus_param_u16_i64_Frame_t _frame;
#line 275 "Minus.m3"
_frame._unused=(ADDRESS)&_frame;
#line 275 "Minus.m3"
 /* load */
#line 275 "Minus.m3"
 /* loophole */
#line 275 "Minus.m3"
 /* load */
#line 275 "Minus.m3"
 /* subtract */
#line 275 "Minus.m3"
 /* exit_proc */
#line 275 "Minus.m3"
return ((INT64)(((INT64)((INT64)((INT64)(a_L_458))))- b_L_459));
#line 275 "Minus.m3"
 /* end_procedure */
#line 275 "Minus.m3"
} /* uMinus_var_u16_i16 */
#line 275 "Minus.m3"
 /* set_source_line */
#line 275 "Minus.m3"
#line 276 "Minus.m3"
 /* begin_procedure */
#line 276 "Minus.m3"
struct Minus__uMinus_var_u16_i16_Frame_t {
#line 276 "Minus.m3"
ADDRESS _unused;
#line 276 "Minus.m3"
};
#line 276 "Minus.m3"
Word__T
__cdecl
Minus__uMinus_var_u16_i16(void)
{
#line 276 "Minus.m3"
Minus__uMinus_var_u16_i16_Frame_t _frame;
#line 276 "Minus.m3"
_frame._unused=(ADDRESS)&_frame;
#line 276 "Minus.m3"
 /* load */
#line 276 "Minus.m3"
 /* load */
#line 276 "Minus.m3"
 /* subtract */
#line 276 "Minus.m3"
 /* exit_proc */
#line 276 "Minus.m3"
return ((UINT64)(((UINT64)(((INT64)(*((UINT16*)(INT64_(144)+((ADDRESS)(&Minus_m_M_Minus_L_13))))))))-((UINT64)(((INT64)(*((INT16*)(INT64_(172)+((ADDRESS)(&Minus_m_M_Minus_L_13))))))))));
#line 276 "Minus.m3"
 /* end_procedure */
#line 276 "Minus.m3"
} /* Minus_var_u16_i16 */
#line 276 "Minus.m3"
 /* set_source_line */
#line 276 "Minus.m3"
#line 277 "Minus.m3"
 /* begin_procedure */
#line 277 "Minus.m3"
struct Minus__Minus_var_u16_i16_Frame_t {
#line 277 "Minus.m3"
ADDRESS _unused;
#line 277 "Minus.m3"
};
#line 277 "Minus.m3"
Minus__UINT16
__cdecl
Minus__Minus_var_u16_i16(void)
{
#line 277 "Minus.m3"
 /* Var_Type2 */ INT64 Minus_m_1256_L_1257={0};//always-init
#line 277 "Minus.m3"
Minus__Minus_var_u16_i16_Frame_t _frame;
#line 277 "Minus.m3"
_frame._unused=(ADDRESS)&_frame;
#line 277 "Minus.m3"
 /* load */
#line 277 "Minus.m3"
 /* load */
#line 277 "Minus.m3"
 /* subtract */
#line 277 "Minus.m3"
 /* check_range */
#line 277 "Minus.m3"
 /* store */
#line 277 "Minus.m3"
(*(INT64*)(&Minus_m_1256_L_1257))=(INT64)( ((INT64)( ((INT64)(*((UINT16*)(INT64_(144)+((ADDRESS)(&Minus_m_M_Minus_L_13))))))- ((INT64)(*((INT16*)(INT64_(172)+((ADDRESS)(&Minus_m_M_Minus_L_13)))))))));
#line 277 "Minus.m3"
 /* load */
#line 277 "Minus.m3"
if(m3_check_range(INT64,
Minus_m_1256_L_1257,
 INT64_(0),
 INT64_(65535)))
#line 277 "Minus.m3"
Minus_m_M_Minus_L_13_CRASH(8865);
#line 277 "Minus.m3"
 /* exit_proc */
#line 277 "Minus.m3"
return Minus_m_1256_L_1257;
#line 277 "Minus.m3"
 /* end_procedure */
#line 277 "Minus.m3"
} /* uMinus_param_u16_i16 */
#line 277 "Minus.m3"
 /* set_source_line */
#line 277 "Minus.m3"
#line 278 "Minus.m3"
 /* begin_procedure */
#line 278 "Minus.m3"
struct Minus__uMinus_param_u16_i16_Frame_t {
#line 278 "Minus.m3"
ADDRESS _unused;
#line 278 "Minus.m3"
};
#line 278 "Minus.m3"
Word__T
__cdecl
Minus__uMinus_param_u16_i16(
   /* Param_Type1 */ Minus__UINT16 a_L_463,
   /* Param_Type1 */ Minus__INT16 b_L_464)
{
#line 278 "Minus.m3"
Minus__uMinus_param_u16_i16_Frame_t _frame;
#line 278 "Minus.m3"
_frame._unused=(ADDRESS)&_frame;
#line 278 "Minus.m3"
 /* load */
#line 278 "Minus.m3"
 /* load */
#line 278 "Minus.m3"
 /* subtract */
#line 278 "Minus.m3"
 /* exit_proc */
#line 278 "Minus.m3"
return ((UINT64)(((UINT64)(((INT64)(a_L_463))))-((UINT64)(((INT64)(b_L_464))))));
#line 278 "Minus.m3"
 /* end_procedure */
#line 278 "Minus.m3"
} /* Minus_param_u16_i16 */
#line 278 "Minus.m3"
 /* set_source_line */
#line 278 "Minus.m3"
#line 279 "Minus.m3"
 /* begin_procedure */
#line 279 "Minus.m3"
struct Minus__Minus_param_u16_i16_Frame_t {
#line 279 "Minus.m3"
ADDRESS _unused;
#line 279 "Minus.m3"
};
#line 279 "Minus.m3"
Minus__UINT16
__cdecl
Minus__Minus_param_u16_i16(
   /* Param_Type1 */ Minus__UINT16 a_L_466,
   /* Param_Type1 */ Minus__INT16 b_L_467)
{
#line 279 "Minus.m3"
 /* Var_Type2 */ INT64 Minus_m_1258_L_1259={0};//always-init
#line 279 "Minus.m3"
Minus__Minus_param_u16_i16_Frame_t _frame;
#line 279 "Minus.m3"
_frame._unused=(ADDRESS)&_frame;
#line 279 "Minus.m3"
 /* load */
#line 279 "Minus.m3"
 /* load */
#line 279 "Minus.m3"
 /* subtract */
#line 279 "Minus.m3"
 /* check_range */
#line 279 "Minus.m3"
 /* store */
#line 279 "Minus.m3"
(*(INT64*)(&Minus_m_1258_L_1259))=(INT64)( ((INT64)( ((INT64)(a_L_466))- ((INT64)(b_L_467)))));
#line 279 "Minus.m3"
 /* load */
#line 279 "Minus.m3"
if(m3_check_range(INT64,
Minus_m_1258_L_1259,
 INT64_(0),
 INT64_(65535)))
#line 279 "Minus.m3"
Minus_m_M_Minus_L_13_CRASH(8929);
#line 279 "Minus.m3"
 /* exit_proc */
#line 279 "Minus.m3"
return Minus_m_1258_L_1259;
#line 279 "Minus.m3"
 /* end_procedure */
#line 279 "Minus.m3"
} /* uMinus_var_u16_C */
#line 279 "Minus.m3"
 /* set_source_line */
#line 279 "Minus.m3"
#line 280 "Minus.m3"
 /* begin_procedure */
#line 280 "Minus.m3"
struct Minus__uMinus_var_u16_C_Frame_t {
#line 280 "Minus.m3"
ADDRESS _unused;
#line 280 "Minus.m3"
};
#line 280 "Minus.m3"
Word__T
__cdecl
Minus__uMinus_var_u16_C(void)
{
#line 280 "Minus.m3"
Minus__uMinus_var_u16_C_Frame_t _frame;
#line 280 "Minus.m3"
_frame._unused=(ADDRESS)&_frame;
#line 280 "Minus.m3"
 /* load */
#line 280 "Minus.m3"
 /* load */
#line 280 "Minus.m3"
 /* subtract */
#line 280 "Minus.m3"
 /* exit_proc */
#line 280 "Minus.m3"
return ((UINT64)(((UINT64)(((INT64)(*((UINT16*)(INT64_(144)+((ADDRESS)(&Minus_m_M_Minus_L_13))))))))-((UINT64)(((INT64)(*((UINT64*)(INT64_(176)+((ADDRESS)(&Minus_m_M_Minus_L_13))))))))));
#line 280 "Minus.m3"
 /* end_procedure */
#line 280 "Minus.m3"
} /* Minus_var_u16_C */
#line 280 "Minus.m3"
 /* set_source_line */
#line 280 "Minus.m3"
#line 281 "Minus.m3"
 /* begin_procedure */
#line 281 "Minus.m3"
struct Minus__Minus_var_u16_C_Frame_t {
#line 281 "Minus.m3"
ADDRESS _unused;
#line 281 "Minus.m3"
};
#line 281 "Minus.m3"
Minus__UINT16
__cdecl
Minus__Minus_var_u16_C(void)
{
#line 281 "Minus.m3"
 /* Var_Type2 */ INT64 Minus_m_1260_L_1261={0};//always-init
#line 281 "Minus.m3"
Minus__Minus_var_u16_C_Frame_t _frame;
#line 281 "Minus.m3"
_frame._unused=(ADDRESS)&_frame;
#line 281 "Minus.m3"
 /* load */
#line 281 "Minus.m3"
 /* load */
#line 281 "Minus.m3"
 /* subtract */
#line 281 "Minus.m3"
 /* check_lo */
#line 281 "Minus.m3"
 /* store */
#line 281 "Minus.m3"
(*(INT64*)(&Minus_m_1260_L_1261))=(INT64)( ((INT64)( ((INT64)(*((UINT16*)(INT64_(144)+((ADDRESS)(&Minus_m_M_Minus_L_13))))))- ((INT64)(*((UINT64*)(INT64_(176)+((ADDRESS)(&Minus_m_M_Minus_L_13)))))))));
#line 281 "Minus.m3"
 /* load */
#line 281 "Minus.m3"
/*check_lo*/if(Minus_m_1260_L_1261<INT64_(0))Minus_m_M_Minus_L_13_CRASH(8993);
#line 281 "Minus.m3"
 /* exit_proc */
#line 281 "Minus.m3"
return Minus_m_1260_L_1261;
#line 281 "Minus.m3"
 /* end_procedure */
#line 281 "Minus.m3"
} /* uMinus_param_u16_C */
#line 281 "Minus.m3"
 /* set_source_line */
#line 281 "Minus.m3"
#line 282 "Minus.m3"
 /* begin_procedure */
#line 282 "Minus.m3"
struct Minus__uMinus_param_u16_C_Frame_t {
#line 282 "Minus.m3"
ADDRESS _unused;
#line 282 "Minus.m3"
};
#line 282 "Minus.m3"
Word__T
__cdecl
Minus__uMinus_param_u16_C(
   /* Param_Type1 */ Minus__UINT16 a_L_471,
   /* Param_Type1 */ CARDINAL b_L_472)
{
#line 282 "Minus.m3"
Minus__uMinus_param_u16_C_Frame_t _frame;
#line 282 "Minus.m3"
_frame._unused=(ADDRESS)&_frame;
#line 282 "Minus.m3"
 /* load */
#line 282 "Minus.m3"
 /* load */
#line 282 "Minus.m3"
 /* subtract */
#line 282 "Minus.m3"
 /* exit_proc */
#line 282 "Minus.m3"
return ((UINT64)(((UINT64)(((INT64)(a_L_471))))-((UINT64)(((INT64)(b_L_472))))));
#line 282 "Minus.m3"
 /* end_procedure */
#line 282 "Minus.m3"
} /* Minus_param_u16_C */
#line 282 "Minus.m3"
 /* set_source_line */
#line 282 "Minus.m3"
#line 283 "Minus.m3"
 /* begin_procedure */
#line 283 "Minus.m3"
struct Minus__Minus_param_u16_C_Frame_t {
#line 283 "Minus.m3"
ADDRESS _unused;
#line 283 "Minus.m3"
};
#line 283 "Minus.m3"
Minus__UINT16
__cdecl
Minus__Minus_param_u16_C(
   /* Param_Type1 */ Minus__UINT16 a_L_474,
   /* Param_Type1 */ CARDINAL b_L_475)
{
#line 283 "Minus.m3"
 /* Var_Type2 */ INT64 Minus_m_1262_L_1263={0};//always-init
#line 283 "Minus.m3"
Minus__Minus_param_u16_C_Frame_t _frame;
#line 283 "Minus.m3"
_frame._unused=(ADDRESS)&_frame;
#line 283 "Minus.m3"
 /* load */
#line 283 "Minus.m3"
 /* load */
#line 283 "Minus.m3"
 /* subtract */
#line 283 "Minus.m3"
 /* check_lo */
#line 283 "Minus.m3"
 /* store */
#line 283 "Minus.m3"
(*(INT64*)(&Minus_m_1262_L_1263))=(INT64)( ((INT64)( ((INT64)(a_L_474))- ((INT64)(b_L_475)))));
#line 283 "Minus.m3"
 /* load */
#line 283 "Minus.m3"
/*check_lo*/if(Minus_m_1262_L_1263<INT64_(0))Minus_m_M_Minus_L_13_CRASH(9057);
#line 283 "Minus.m3"
 /* exit_proc */
#line 283 "Minus.m3"
return Minus_m_1262_L_1263;
#line 283 "Minus.m3"
 /* end_procedure */
#line 283 "Minus.m3"
} /* uMinus_var_u16_u32 */
#line 283 "Minus.m3"
 /* set_source_line */
#line 283 "Minus.m3"
#line 284 "Minus.m3"
 /* begin_procedure */
#line 284 "Minus.m3"
struct Minus__uMinus_var_u16_u32_Frame_t {
#line 284 "Minus.m3"
ADDRESS _unused;
#line 284 "Minus.m3"
};
#line 284 "Minus.m3"
Word__T
__cdecl
Minus__uMinus_var_u16_u32(void)
{
#line 284 "Minus.m3"
Minus__uMinus_var_u16_u32_Frame_t _frame;
#line 284 "Minus.m3"
_frame._unused=(ADDRESS)&_frame;
#line 284 "Minus.m3"
 /* load */
#line 284 "Minus.m3"
 /* load */
#line 284 "Minus.m3"
 /* subtract */
#line 284 "Minus.m3"
 /* exit_proc */
#line 284 "Minus.m3"
return ((UINT64)(((UINT64)(((INT64)(*((UINT16*)(INT64_(144)+((ADDRESS)(&Minus_m_M_Minus_L_13))))))))-((UINT64)(((INT64)(*((UINT32*)(INT64_(184)+((ADDRESS)(&Minus_m_M_Minus_L_13))))))))));
#line 284 "Minus.m3"
 /* end_procedure */
#line 284 "Minus.m3"
} /* Minus_var_u16_u32 */
#line 284 "Minus.m3"
 /* set_source_line */
#line 284 "Minus.m3"
#line 285 "Minus.m3"
 /* begin_procedure */
#line 285 "Minus.m3"
struct Minus__Minus_var_u16_u32_Frame_t {
#line 285 "Minus.m3"
ADDRESS _unused;
#line 285 "Minus.m3"
};
#line 285 "Minus.m3"
Minus__UINT16
__cdecl
Minus__Minus_var_u16_u32(void)
{
#line 285 "Minus.m3"
 /* Var_Type2 */ INT64 Minus_m_1264_L_1265={0};//always-init
#line 285 "Minus.m3"
Minus__Minus_var_u16_u32_Frame_t _frame;
#line 285 "Minus.m3"
_frame._unused=(ADDRESS)&_frame;
#line 285 "Minus.m3"
 /* load */
#line 285 "Minus.m3"
 /* load */
#line 285 "Minus.m3"
 /* subtract */
#line 285 "Minus.m3"
 /* check_lo */
#line 285 "Minus.m3"
 /* store */
#line 285 "Minus.m3"
(*(INT64*)(&Minus_m_1264_L_1265))=(INT64)( ((INT64)( ((INT64)(*((UINT16*)(INT64_(144)+((ADDRESS)(&Minus_m_M_Minus_L_13))))))- ((INT64)(*((UINT32*)(INT64_(184)+((ADDRESS)(&Minus_m_M_Minus_L_13)))))))));
#line 285 "Minus.m3"
 /* load */
#line 285 "Minus.m3"
/*check_lo*/if(Minus_m_1264_L_1265<INT64_(0))Minus_m_M_Minus_L_13_CRASH(9121);
#line 285 "Minus.m3"
 /* exit_proc */
#line 285 "Minus.m3"
return Minus_m_1264_L_1265;
#line 285 "Minus.m3"
 /* end_procedure */
#line 285 "Minus.m3"
} /* uMinus_param_u16_u32 */
#line 285 "Minus.m3"
 /* set_source_line */
#line 285 "Minus.m3"
#line 286 "Minus.m3"
 /* begin_procedure */
#line 286 "Minus.m3"
struct Minus__uMinus_param_u16_u32_Frame_t {
#line 286 "Minus.m3"
ADDRESS _unused;
#line 286 "Minus.m3"
};
#line 286 "Minus.m3"
Word__T
__cdecl
Minus__uMinus_param_u16_u32(
   /* Param_Type1 */ Minus__UINT16 a_L_479,
   /* Param_Type1 */ Minus__UINT32 b_L_480)
{
#line 286 "Minus.m3"
Minus__uMinus_param_u16_u32_Frame_t _frame;
#line 286 "Minus.m3"
_frame._unused=(ADDRESS)&_frame;
#line 286 "Minus.m3"
 /* load */
#line 286 "Minus.m3"
 /* load */
#line 286 "Minus.m3"
 /* subtract */
#line 286 "Minus.m3"
 /* exit_proc */
#line 286 "Minus.m3"
return ((UINT64)(((UINT64)(((INT64)(a_L_479))))-((UINT64)(((INT64)(b_L_480))))));
#line 286 "Minus.m3"
 /* end_procedure */
#line 286 "Minus.m3"
} /* Minus_param_u16_u32 */
#line 286 "Minus.m3"
 /* set_source_line */
#line 286 "Minus.m3"
#line 287 "Minus.m3"
 /* begin_procedure */
#line 287 "Minus.m3"
struct Minus__Minus_param_u16_u32_Frame_t {
#line 287 "Minus.m3"
ADDRESS _unused;
#line 287 "Minus.m3"
};
#line 287 "Minus.m3"
Minus__UINT16
__cdecl
Minus__Minus_param_u16_u32(
   /* Param_Type1 */ Minus__UINT16 a_L_482,
   /* Param_Type1 */ Minus__UINT32 b_L_483)
{
#line 287 "Minus.m3"
 /* Var_Type2 */ INT64 Minus_m_1266_L_1267={0};//always-init
#line 287 "Minus.m3"
Minus__Minus_param_u16_u32_Frame_t _frame;
#line 287 "Minus.m3"
_frame._unused=(ADDRESS)&_frame;
#line 287 "Minus.m3"
 /* load */
#line 287 "Minus.m3"
 /* load */
#line 287 "Minus.m3"
 /* subtract */
#line 287 "Minus.m3"
 /* check_lo */
#line 287 "Minus.m3"
 /* store */
#line 287 "Minus.m3"
(*(INT64*)(&Minus_m_1266_L_1267))=(INT64)( ((INT64)( ((INT64)(a_L_482))- ((INT64)(b_L_483)))));
#line 287 "Minus.m3"
 /* load */
#line 287 "Minus.m3"
/*check_lo*/if(Minus_m_1266_L_1267<INT64_(0))Minus_m_M_Minus_L_13_CRASH(9185);
#line 287 "Minus.m3"
 /* exit_proc */
#line 287 "Minus.m3"
return Minus_m_1266_L_1267;
#line 287 "Minus.m3"
 /* end_procedure */
#line 287 "Minus.m3"
} /* uMinus_var_u16_u8 */
#line 287 "Minus.m3"
 /* set_source_line */
#line 287 "Minus.m3"
#line 288 "Minus.m3"
 /* begin_procedure */
#line 288 "Minus.m3"
struct Minus__uMinus_var_u16_u8_Frame_t {
#line 288 "Minus.m3"
ADDRESS _unused;
#line 288 "Minus.m3"
};
#line 288 "Minus.m3"
Word__T
__cdecl
Minus__uMinus_var_u16_u8(void)
{
#line 288 "Minus.m3"
Minus__uMinus_var_u16_u8_Frame_t _frame;
#line 288 "Minus.m3"
_frame._unused=(ADDRESS)&_frame;
#line 288 "Minus.m3"
 /* load */
#line 288 "Minus.m3"
 /* load */
#line 288 "Minus.m3"
 /* subtract */
#line 288 "Minus.m3"
 /* exit_proc */
#line 288 "Minus.m3"
return ((UINT64)(((UINT64)(((INT64)(*((UINT16*)(INT64_(144)+((ADDRESS)(&Minus_m_M_Minus_L_13))))))))-((UINT64)(((INT64)(*((UINT8*)(INT64_(188)+((ADDRESS)(&Minus_m_M_Minus_L_13))))))))));
#line 288 "Minus.m3"
 /* end_procedure */
#line 288 "Minus.m3"
} /* Minus_var_u16_u8 */
#line 288 "Minus.m3"
 /* set_source_line */
#line 288 "Minus.m3"
#line 289 "Minus.m3"
 /* begin_procedure */
#line 289 "Minus.m3"
struct Minus__Minus_var_u16_u8_Frame_t {
#line 289 "Minus.m3"
ADDRESS _unused;
#line 289 "Minus.m3"
};
#line 289 "Minus.m3"
Minus__UINT16
__cdecl
Minus__Minus_var_u16_u8(void)
{
#line 289 "Minus.m3"
 /* Var_Type2 */ INT64 Minus_m_1268_L_1269={0};//always-init
#line 289 "Minus.m3"
Minus__Minus_var_u16_u8_Frame_t _frame;
#line 289 "Minus.m3"
_frame._unused=(ADDRESS)&_frame;
#line 289 "Minus.m3"
 /* load */
#line 289 "Minus.m3"
 /* load */
#line 289 "Minus.m3"
 /* subtract */
#line 289 "Minus.m3"
 /* check_lo */
#line 289 "Minus.m3"
 /* store */
#line 289 "Minus.m3"
(*(INT64*)(&Minus_m_1268_L_1269))=(INT64)( ((INT64)( ((INT64)(*((UINT16*)(INT64_(144)+((ADDRESS)(&Minus_m_M_Minus_L_13))))))- ((INT64)(*((UINT8*)(INT64_(188)+((ADDRESS)(&Minus_m_M_Minus_L_13)))))))));
#line 289 "Minus.m3"
 /* load */
#line 289 "Minus.m3"
/*check_lo*/if(Minus_m_1268_L_1269<INT64_(0))Minus_m_M_Minus_L_13_CRASH(9249);
#line 289 "Minus.m3"
 /* exit_proc */
#line 289 "Minus.m3"
return Minus_m_1268_L_1269;
#line 289 "Minus.m3"
 /* end_procedure */
#line 289 "Minus.m3"
} /* uMinus_param_u16_u8 */
#line 289 "Minus.m3"
 /* set_source_line */
#line 289 "Minus.m3"
#line 290 "Minus.m3"
 /* begin_procedure */
#line 290 "Minus.m3"
struct Minus__uMinus_param_u16_u8_Frame_t {
#line 290 "Minus.m3"
ADDRESS _unused;
#line 290 "Minus.m3"
};
#line 290 "Minus.m3"
Word__T
__cdecl
Minus__uMinus_param_u16_u8(
   /* Param_Type1 */ Minus__UINT16 a_L_487,
   /* Param_Type1 */ Minus__UINT8 b_L_488)
{
#line 290 "Minus.m3"
Minus__uMinus_param_u16_u8_Frame_t _frame;
#line 290 "Minus.m3"
_frame._unused=(ADDRESS)&_frame;
#line 290 "Minus.m3"
 /* load */
#line 290 "Minus.m3"
 /* load */
#line 290 "Minus.m3"
 /* subtract */
#line 290 "Minus.m3"
 /* exit_proc */
#line 290 "Minus.m3"
return ((UINT64)(((UINT64)(((INT64)(a_L_487))))-((UINT64)(((INT64)(b_L_488))))));
#line 290 "Minus.m3"
 /* end_procedure */
#line 290 "Minus.m3"
} /* Minus_param_u16_u8 */
#line 290 "Minus.m3"
 /* set_source_line */
#line 290 "Minus.m3"
#line 291 "Minus.m3"
 /* begin_procedure */
#line 291 "Minus.m3"
struct Minus__Minus_param_u16_u8_Frame_t {
#line 291 "Minus.m3"
ADDRESS _unused;
#line 291 "Minus.m3"
};
#line 291 "Minus.m3"
Minus__UINT16
__cdecl
Minus__Minus_param_u16_u8(
   /* Param_Type1 */ Minus__UINT16 a_L_490,
   /* Param_Type1 */ Minus__UINT8 b_L_491)
{
#line 291 "Minus.m3"
 /* Var_Type2 */ INT64 Minus_m_1270_L_1271={0};//always-init
#line 291 "Minus.m3"
Minus__Minus_param_u16_u8_Frame_t _frame;
#line 291 "Minus.m3"
_frame._unused=(ADDRESS)&_frame;
#line 291 "Minus.m3"
 /* load */
#line 291 "Minus.m3"
 /* load */
#line 291 "Minus.m3"
 /* subtract */
#line 291 "Minus.m3"
 /* check_lo */
#line 291 "Minus.m3"
 /* store */
#line 291 "Minus.m3"
(*(INT64*)(&Minus_m_1270_L_1271))=(INT64)( ((INT64)( ((INT64)(a_L_490))- ((INT64)(b_L_491)))));
#line 291 "Minus.m3"
 /* load */
#line 291 "Minus.m3"
/*check_lo*/if(Minus_m_1270_L_1271<INT64_(0))Minus_m_M_Minus_L_13_CRASH(9313);
#line 291 "Minus.m3"
 /* exit_proc */
#line 291 "Minus.m3"
return Minus_m_1270_L_1271;
#line 291 "Minus.m3"
 /* end_procedure */
#line 291 "Minus.m3"
} /* uMinus_var_u16_L */
#line 291 "Minus.m3"
 /* set_source_line */
#line 291 "Minus.m3"
#line 292 "Minus.m3"
 /* begin_procedure */
#line 292 "Minus.m3"
struct Minus__uMinus_var_u16_L_Frame_t {
#line 292 "Minus.m3"
ADDRESS _unused;
#line 292 "Minus.m3"
};
#line 292 "Minus.m3"
Long__T
__cdecl
Minus__uMinus_var_u16_L(void)
{
#line 292 "Minus.m3"
Minus__uMinus_var_u16_L_Frame_t _frame;
#line 292 "Minus.m3"
_frame._unused=(ADDRESS)&_frame;
#line 292 "Minus.m3"
 /* load */
#line 292 "Minus.m3"
 /* loophole */
#line 292 "Minus.m3"
 /* load */
#line 292 "Minus.m3"
 /* subtract */
#line 292 "Minus.m3"
 /* exit_proc */
#line 292 "Minus.m3"
return ((UINT64)(((UINT64)((INT64)((INT64)(*((UINT16*)(INT64_(144)+((ADDRESS)(&Minus_m_M_Minus_L_13))))))))-((UINT64)(*((INT64*)(INT64_(192)+((ADDRESS)(&Minus_m_M_Minus_L_13))))))));
#line 292 "Minus.m3"
 /* end_procedure */
#line 292 "Minus.m3"
} /* Minus_var_u16_L */
#line 292 "Minus.m3"
 /* set_source_line */
#line 292 "Minus.m3"
#line 293 "Minus.m3"
 /* begin_procedure */
#line 293 "Minus.m3"
struct Minus__Minus_var_u16_L_Frame_t {
#line 293 "Minus.m3"
ADDRESS _unused;
#line 293 "Minus.m3"
};
#line 293 "Minus.m3"
LONGINT
__cdecl
Minus__Minus_var_u16_L(void)
{
#line 293 "Minus.m3"
Minus__Minus_var_u16_L_Frame_t _frame;
#line 293 "Minus.m3"
_frame._unused=(ADDRESS)&_frame;
#line 293 "Minus.m3"
 /* load */
#line 293 "Minus.m3"
 /* loophole */
#line 293 "Minus.m3"
 /* load */
#line 293 "Minus.m3"
 /* subtract */
#line 293 "Minus.m3"
 /* exit_proc */
#line 293 "Minus.m3"
return ((INT64)(((INT64)((INT64)((INT64)(*((UINT16*)(INT64_(144)+((ADDRESS)(&Minus_m_M_Minus_L_13))))))))-((INT64)(*((INT64*)(INT64_(192)+((ADDRESS)(&Minus_m_M_Minus_L_13))))))));
#line 293 "Minus.m3"
 /* end_procedure */
#line 293 "Minus.m3"
} /* uMinus_param_u16_L */
#line 293 "Minus.m3"
 /* set_source_line */
#line 293 "Minus.m3"
#line 294 "Minus.m3"
 /* begin_procedure */
#line 294 "Minus.m3"
struct Minus__uMinus_param_u16_L_Frame_t {
#line 294 "Minus.m3"
ADDRESS _unused;
#line 294 "Minus.m3"
};
#line 294 "Minus.m3"
Long__T
__cdecl
Minus__uMinus_param_u16_L(
   /* Param_Type1 */ Minus__UINT16 a_L_495,
   /* Param_Type1 */ LONGINT b_L_496)
{
#line 294 "Minus.m3"
Minus__uMinus_param_u16_L_Frame_t _frame;
#line 294 "Minus.m3"
_frame._unused=(ADDRESS)&_frame;
#line 294 "Minus.m3"
 /* load */
#line 294 "Minus.m3"
 /* loophole */
#line 294 "Minus.m3"
 /* load */
#line 294 "Minus.m3"
 /* subtract */
#line 294 "Minus.m3"
 /* exit_proc */
#line 294 "Minus.m3"
return ((UINT64)(((UINT64)((INT64)((INT64)(a_L_495))))-((UINT64)(b_L_496))));
#line 294 "Minus.m3"
 /* end_procedure */
#line 294 "Minus.m3"
} /* Minus_param_u16_L */
#line 294 "Minus.m3"
 /* set_source_line */
#line 294 "Minus.m3"
#line 295 "Minus.m3"
 /* begin_procedure */
#line 295 "Minus.m3"
struct Minus__Minus_param_u16_L_Frame_t {
#line 295 "Minus.m3"
ADDRESS _unused;
#line 295 "Minus.m3"
};
#line 295 "Minus.m3"
LONGINT
__cdecl
Minus__Minus_param_u16_L(
   /* Param_Type1 */ Minus__UINT16 a_L_498,
   /* Param_Type1 */ LONGINT b_L_499)
{
#line 295 "Minus.m3"
Minus__Minus_param_u16_L_Frame_t _frame;
#line 295 "Minus.m3"
_frame._unused=(ADDRESS)&_frame;
#line 295 "Minus.m3"
 /* load */
#line 295 "Minus.m3"
 /* loophole */
#line 295 "Minus.m3"
 /* load */
#line 295 "Minus.m3"
 /* subtract */
#line 295 "Minus.m3"
 /* exit_proc */
#line 295 "Minus.m3"
return ((INT64)(((INT64)((INT64)((INT64)(a_L_498))))- b_L_499));
#line 295 "Minus.m3"
 /* end_procedure */
#line 295 "Minus.m3"
} /* uMinus_var_I_i8 */
#line 295 "Minus.m3"
 /* set_source_line */
#line 295 "Minus.m3"
#line 296 "Minus.m3"
 /* begin_procedure */
#line 296 "Minus.m3"
struct Minus__uMinus_var_I_i8_Frame_t {
#line 296 "Minus.m3"
ADDRESS _unused;
#line 296 "Minus.m3"
};
#line 296 "Minus.m3"
Word__T
__cdecl
Minus__uMinus_var_I_i8(void)
{
#line 296 "Minus.m3"
Minus__uMinus_var_I_i8_Frame_t _frame;
#line 296 "Minus.m3"
_frame._unused=(ADDRESS)&_frame;
#line 296 "Minus.m3"
 /* load */
#line 296 "Minus.m3"
 /* load */
#line 296 "Minus.m3"
 /* subtract */
#line 296 "Minus.m3"
 /* exit_proc */
#line 296 "Minus.m3"
return ((UINT64)(((UINT64)(*((INT64*)(INT64_(152)+((ADDRESS)(&Minus_m_M_Minus_L_13))))))-((UINT64)(((INT64)(*((INT8*)(INT64_(104)+((ADDRESS)(&Minus_m_M_Minus_L_13))))))))));
#line 296 "Minus.m3"
 /* end_procedure */
#line 296 "Minus.m3"
} /* Minus_var_I_i8 */
#line 296 "Minus.m3"
 /* set_source_line */
#line 296 "Minus.m3"
#line 297 "Minus.m3"
 /* begin_procedure */
#line 297 "Minus.m3"
struct Minus__Minus_var_I_i8_Frame_t {
#line 297 "Minus.m3"
ADDRESS _unused;
#line 297 "Minus.m3"
};
#line 297 "Minus.m3"
INTEGER
__cdecl
Minus__Minus_var_I_i8(void)
{
#line 297 "Minus.m3"
Minus__Minus_var_I_i8_Frame_t _frame;
#line 297 "Minus.m3"
_frame._unused=(ADDRESS)&_frame;
#line 297 "Minus.m3"
 /* load */
#line 297 "Minus.m3"
 /* load */
#line 297 "Minus.m3"
 /* subtract */
#line 297 "Minus.m3"
 /* exit_proc */
#line 297 "Minus.m3"
return ((INT64)(((INT64)(*((INT64*)(INT64_(152)+((ADDRESS)(&Minus_m_M_Minus_L_13))))))- ((INT64)(*((INT8*)(INT64_(104)+((ADDRESS)(&Minus_m_M_Minus_L_13))))))));
#line 297 "Minus.m3"
 /* end_procedure */
#line 297 "Minus.m3"
} /* uMinus_param_I_i8 */
#line 297 "Minus.m3"
 /* set_source_line */
#line 297 "Minus.m3"
#line 298 "Minus.m3"
 /* begin_procedure */
#line 298 "Minus.m3"
struct Minus__uMinus_param_I_i8_Frame_t {
#line 298 "Minus.m3"
ADDRESS _unused;
#line 298 "Minus.m3"
};
#line 298 "Minus.m3"
Word__T
__cdecl
Minus__uMinus_param_I_i8(
   /* Param_Type1 */ INTEGER a_L_503,
   /* Param_Type1 */ Minus__INT8 b_L_504)
{
#line 298 "Minus.m3"
Minus__uMinus_param_I_i8_Frame_t _frame;
#line 298 "Minus.m3"
_frame._unused=(ADDRESS)&_frame;
#line 298 "Minus.m3"
 /* load */
#line 298 "Minus.m3"
 /* load */
#line 298 "Minus.m3"
 /* subtract */
#line 298 "Minus.m3"
 /* exit_proc */
#line 298 "Minus.m3"
return ((UINT64)(((UINT64)(a_L_503))-((UINT64)(((INT64)(b_L_504))))));
#line 298 "Minus.m3"
 /* end_procedure */
#line 298 "Minus.m3"
} /* Minus_param_I_i8 */
#line 298 "Minus.m3"
 /* set_source_line */
#line 298 "Minus.m3"
#line 299 "Minus.m3"
 /* begin_procedure */
#line 299 "Minus.m3"
struct Minus__Minus_param_I_i8_Frame_t {
#line 299 "Minus.m3"
ADDRESS _unused;
#line 299 "Minus.m3"
};
#line 299 "Minus.m3"
INTEGER
__cdecl
Minus__Minus_param_I_i8(
   /* Param_Type1 */ INTEGER a_L_506,
   /* Param_Type1 */ Minus__INT8 b_L_507)
{
#line 299 "Minus.m3"
Minus__Minus_param_I_i8_Frame_t _frame;
#line 299 "Minus.m3"
_frame._unused=(ADDRESS)&_frame;
#line 299 "Minus.m3"
 /* load */
#line 299 "Minus.m3"
 /* load */
#line 299 "Minus.m3"
 /* subtract */
#line 299 "Minus.m3"
 /* exit_proc */
#line 299 "Minus.m3"
return ((INT64)( a_L_506- ((INT64)(b_L_507))));
#line 299 "Minus.m3"
 /* end_procedure */
#line 299 "Minus.m3"
} /* uMinus_var_I_u64 */
#line 299 "Minus.m3"
 /* set_source_line */
#line 299 "Minus.m3"
#line 300 "Minus.m3"
 /* begin_procedure */
#line 300 "Minus.m3"
struct Minus__uMinus_var_I_u64_Frame_t {
#line 300 "Minus.m3"
ADDRESS _unused;
#line 300 "Minus.m3"
};
#line 300 "Minus.m3"
Long__T
__cdecl
Minus__uMinus_var_I_u64(void)
{
#line 300 "Minus.m3"
Minus__uMinus_var_I_u64_Frame_t _frame;
#line 300 "Minus.m3"
_frame._unused=(ADDRESS)&_frame;
#line 300 "Minus.m3"
 /* load */
#line 300 "Minus.m3"
 /* loophole */
#line 300 "Minus.m3"
 /* load */
#line 300 "Minus.m3"
 /* subtract */
#line 300 "Minus.m3"
 /* exit_proc */
#line 300 "Minus.m3"
return ((UINT64)(((UINT64)((INT64)*((INT64*)(INT64_(152)+((ADDRESS)(&Minus_m_M_Minus_L_13))))))-((UINT64)(*((INT64*)(INT64_(112)+((ADDRESS)(&Minus_m_M_Minus_L_13))))))));
#line 300 "Minus.m3"
 /* end_procedure */
#line 300 "Minus.m3"
} /* Minus_var_I_u64 */
#line 300 "Minus.m3"
 /* set_source_line */
#line 300 "Minus.m3"
#line 301 "Minus.m3"
 /* begin_procedure */
#line 301 "Minus.m3"
struct Minus__Minus_var_I_u64_Frame_t {
#line 301 "Minus.m3"
ADDRESS _unused;
#line 301 "Minus.m3"
};
#line 301 "Minus.m3"
LONGINT
__cdecl
Minus__Minus_var_I_u64(void)
{
#line 301 "Minus.m3"
Minus__Minus_var_I_u64_Frame_t _frame;
#line 301 "Minus.m3"
_frame._unused=(ADDRESS)&_frame;
#line 301 "Minus.m3"
 /* load */
#line 301 "Minus.m3"
 /* loophole */
#line 301 "Minus.m3"
 /* load */
#line 301 "Minus.m3"
 /* subtract */
#line 301 "Minus.m3"
 /* exit_proc */
#line 301 "Minus.m3"
return ((INT64)(((INT64)((INT64)*((INT64*)(INT64_(152)+((ADDRESS)(&Minus_m_M_Minus_L_13))))))-((INT64)(*((INT64*)(INT64_(112)+((ADDRESS)(&Minus_m_M_Minus_L_13))))))));
#line 301 "Minus.m3"
 /* end_procedure */
#line 301 "Minus.m3"
} /* uMinus_param_I_u64 */
#line 301 "Minus.m3"
 /* set_source_line */
#line 301 "Minus.m3"
#line 302 "Minus.m3"
 /* begin_procedure */
#line 302 "Minus.m3"
struct Minus__uMinus_param_I_u64_Frame_t {
#line 302 "Minus.m3"
ADDRESS _unused;
#line 302 "Minus.m3"
};
#line 302 "Minus.m3"
Long__T
__cdecl
Minus__uMinus_param_I_u64(
   /* Param_Type1 */ INTEGER a_L_511,
   /* Param_Type1 */ Minus__UINT64 b_L_512)
{
#line 302 "Minus.m3"
Minus__uMinus_param_I_u64_Frame_t _frame;
#line 302 "Minus.m3"
_frame._unused=(ADDRESS)&_frame;
#line 302 "Minus.m3"
 /* load */
#line 302 "Minus.m3"
 /* loophole */
#line 302 "Minus.m3"
 /* load */
#line 302 "Minus.m3"
 /* subtract */
#line 302 "Minus.m3"
 /* exit_proc */
#line 302 "Minus.m3"
return ((UINT64)(((UINT64)((INT64)a_L_511))-((UINT64)(b_L_512))));
#line 302 "Minus.m3"
 /* end_procedure */
#line 302 "Minus.m3"
} /* Minus_param_I_u64 */
#line 302 "Minus.m3"
 /* set_source_line */
#line 302 "Minus.m3"
#line 303 "Minus.m3"
 /* begin_procedure */
#line 303 "Minus.m3"
struct Minus__Minus_param_I_u64_Frame_t {
#line 303 "Minus.m3"
ADDRESS _unused;
#line 303 "Minus.m3"
};
#line 303 "Minus.m3"
LONGINT
__cdecl
Minus__Minus_param_I_u64(
   /* Param_Type1 */ INTEGER a_L_514,
   /* Param_Type1 */ Minus__UINT64 b_L_515)
{
#line 303 "Minus.m3"
Minus__Minus_param_I_u64_Frame_t _frame;
#line 303 "Minus.m3"
_frame._unused=(ADDRESS)&_frame;
#line 303 "Minus.m3"
 /* load */
#line 303 "Minus.m3"
 /* loophole */
#line 303 "Minus.m3"
 /* load */
#line 303 "Minus.m3"
 /* subtract */
#line 303 "Minus.m3"
 /* exit_proc */
#line 303 "Minus.m3"
return ((INT64)(((INT64)((INT64)a_L_514))- b_L_515));
#line 303 "Minus.m3"
 /* end_procedure */
#line 303 "Minus.m3"
} /* uMinus_var_I_i32 */
#line 303 "Minus.m3"
 /* set_source_line */
#line 303 "Minus.m3"
#line 304 "Minus.m3"
 /* begin_procedure */
#line 304 "Minus.m3"
struct Minus__uMinus_var_I_i32_Frame_t {
#line 304 "Minus.m3"
ADDRESS _unused;
#line 304 "Minus.m3"
};
#line 304 "Minus.m3"
Word__T
__cdecl
Minus__uMinus_var_I_i32(void)
{
#line 304 "Minus.m3"
Minus__uMinus_var_I_i32_Frame_t _frame;
#line 304 "Minus.m3"
_frame._unused=(ADDRESS)&_frame;
#line 304 "Minus.m3"
 /* load */
#line 304 "Minus.m3"
 /* load */
#line 304 "Minus.m3"
 /* subtract */
#line 304 "Minus.m3"
 /* exit_proc */
#line 304 "Minus.m3"
return ((UINT64)(((UINT64)(*((INT64*)(INT64_(152)+((ADDRESS)(&Minus_m_M_Minus_L_13))))))-((UINT64)(((INT64)(*((INT32*)(INT64_(128)+((ADDRESS)(&Minus_m_M_Minus_L_13))))))))));
#line 304 "Minus.m3"
 /* end_procedure */
#line 304 "Minus.m3"
} /* Minus_var_I_i32 */
#line 304 "Minus.m3"
 /* set_source_line */
#line 304 "Minus.m3"
#line 305 "Minus.m3"
 /* begin_procedure */
#line 305 "Minus.m3"
struct Minus__Minus_var_I_i32_Frame_t {
#line 305 "Minus.m3"
ADDRESS _unused;
#line 305 "Minus.m3"
};
#line 305 "Minus.m3"
INTEGER
__cdecl
Minus__Minus_var_I_i32(void)
{
#line 305 "Minus.m3"
Minus__Minus_var_I_i32_Frame_t _frame;
#line 305 "Minus.m3"
_frame._unused=(ADDRESS)&_frame;
#line 305 "Minus.m3"
 /* load */
#line 305 "Minus.m3"
 /* load */
#line 305 "Minus.m3"
 /* subtract */
#line 305 "Minus.m3"
 /* exit_proc */
#line 305 "Minus.m3"
return ((INT64)(((INT64)(*((INT64*)(INT64_(152)+((ADDRESS)(&Minus_m_M_Minus_L_13))))))- ((INT64)(*((INT32*)(INT64_(128)+((ADDRESS)(&Minus_m_M_Minus_L_13))))))));
#line 305 "Minus.m3"
 /* end_procedure */
#line 305 "Minus.m3"
} /* uMinus_param_I_i32 */
#line 305 "Minus.m3"
 /* set_source_line */
#line 305 "Minus.m3"
#line 306 "Minus.m3"
 /* begin_procedure */
#line 306 "Minus.m3"
struct Minus__uMinus_param_I_i32_Frame_t {
#line 306 "Minus.m3"
ADDRESS _unused;
#line 306 "Minus.m3"
};
#line 306 "Minus.m3"
Word__T
__cdecl
Minus__uMinus_param_I_i32(
   /* Param_Type1 */ INTEGER a_L_519,
   /* Param_Type1 */ Minus__INT32 b_L_520)
{
#line 306 "Minus.m3"
Minus__uMinus_param_I_i32_Frame_t _frame;
#line 306 "Minus.m3"
_frame._unused=(ADDRESS)&_frame;
#line 306 "Minus.m3"
 /* load */
#line 306 "Minus.m3"
 /* load */
#line 306 "Minus.m3"
 /* subtract */
#line 306 "Minus.m3"
 /* exit_proc */
#line 306 "Minus.m3"
return ((UINT64)(((UINT64)(a_L_519))-((UINT64)(((INT64)(b_L_520))))));
#line 306 "Minus.m3"
 /* end_procedure */
#line 306 "Minus.m3"
} /* Minus_param_I_i32 */
#line 306 "Minus.m3"
 /* set_source_line */
#line 306 "Minus.m3"
#line 307 "Minus.m3"
 /* begin_procedure */
#line 307 "Minus.m3"
struct Minus__Minus_param_I_i32_Frame_t {
#line 307 "Minus.m3"
ADDRESS _unused;
#line 307 "Minus.m3"
};
#line 307 "Minus.m3"
INTEGER
__cdecl
Minus__Minus_param_I_i32(
   /* Param_Type1 */ INTEGER a_L_522,
   /* Param_Type1 */ Minus__INT32 b_L_523)
{
#line 307 "Minus.m3"
Minus__Minus_param_I_i32_Frame_t _frame;
#line 307 "Minus.m3"
_frame._unused=(ADDRESS)&_frame;
#line 307 "Minus.m3"
 /* load */
#line 307 "Minus.m3"
 /* load */
#line 307 "Minus.m3"
 /* subtract */
#line 307 "Minus.m3"
 /* exit_proc */
#line 307 "Minus.m3"
return ((INT64)( a_L_522- ((INT64)(b_L_523))));
#line 307 "Minus.m3"
 /* end_procedure */
#line 307 "Minus.m3"
} /* uMinus_var_I_LC */
#line 307 "Minus.m3"
 /* set_source_line */
#line 307 "Minus.m3"
#line 308 "Minus.m3"
 /* begin_procedure */
#line 308 "Minus.m3"
struct Minus__uMinus_var_I_LC_Frame_t {
#line 308 "Minus.m3"
ADDRESS _unused;
#line 308 "Minus.m3"
};
#line 308 "Minus.m3"
Long__T
__cdecl
Minus__uMinus_var_I_LC(void)
{
#line 308 "Minus.m3"
Minus__uMinus_var_I_LC_Frame_t _frame;
#line 308 "Minus.m3"
_frame._unused=(ADDRESS)&_frame;
#line 308 "Minus.m3"
 /* load */
#line 308 "Minus.m3"
 /* loophole */
#line 308 "Minus.m3"
 /* load */
#line 308 "Minus.m3"
 /* subtract */
#line 308 "Minus.m3"
 /* exit_proc */
#line 308 "Minus.m3"
return ((UINT64)(((UINT64)((INT64)*((INT64*)(INT64_(152)+((ADDRESS)(&Minus_m_M_Minus_L_13))))))-((UINT64)(((INT64)(*((UINT64*)(INT64_(136)+((ADDRESS)(&Minus_m_M_Minus_L_13))))))))));
#line 308 "Minus.m3"
 /* end_procedure */
#line 308 "Minus.m3"
} /* Minus_var_I_LC */
#line 308 "Minus.m3"
 /* set_source_line */
#line 308 "Minus.m3"
#line 309 "Minus.m3"
 /* begin_procedure */
#line 309 "Minus.m3"
struct Minus__Minus_var_I_LC_Frame_t {
#line 309 "Minus.m3"
ADDRESS _unused;
#line 309 "Minus.m3"
};
#line 309 "Minus.m3"
LONGINT
__cdecl
Minus__Minus_var_I_LC(void)
{
#line 309 "Minus.m3"
Minus__Minus_var_I_LC_Frame_t _frame;
#line 309 "Minus.m3"
_frame._unused=(ADDRESS)&_frame;
#line 309 "Minus.m3"
 /* load */
#line 309 "Minus.m3"
 /* loophole */
#line 309 "Minus.m3"
 /* load */
#line 309 "Minus.m3"
 /* subtract */
#line 309 "Minus.m3"
 /* exit_proc */
#line 309 "Minus.m3"
return ((INT64)(((INT64)((INT64)*((INT64*)(INT64_(152)+((ADDRESS)(&Minus_m_M_Minus_L_13))))))- ((INT64)(*((UINT64*)(INT64_(136)+((ADDRESS)(&Minus_m_M_Minus_L_13))))))));
#line 309 "Minus.m3"
 /* end_procedure */
#line 309 "Minus.m3"
} /* uMinus_param_I_LC */
#line 309 "Minus.m3"
 /* set_source_line */
#line 309 "Minus.m3"
#line 310 "Minus.m3"
 /* begin_procedure */
#line 310 "Minus.m3"
struct Minus__uMinus_param_I_LC_Frame_t {
#line 310 "Minus.m3"
ADDRESS _unused;
#line 310 "Minus.m3"
};
#line 310 "Minus.m3"
Long__T
__cdecl
Minus__uMinus_param_I_LC(
   /* Param_Type1 */ INTEGER a_L_527,
   /* Param_Type1 */ LONGCARD b_L_528)
{
#line 310 "Minus.m3"
Minus__uMinus_param_I_LC_Frame_t _frame;
#line 310 "Minus.m3"
_frame._unused=(ADDRESS)&_frame;
#line 310 "Minus.m3"
 /* load */
#line 310 "Minus.m3"
 /* loophole */
#line 310 "Minus.m3"
 /* load */
#line 310 "Minus.m3"
 /* subtract */
#line 310 "Minus.m3"
 /* exit_proc */
#line 310 "Minus.m3"
return ((UINT64)(((UINT64)((INT64)a_L_527))-((UINT64)(((INT64)(b_L_528))))));
#line 310 "Minus.m3"
 /* end_procedure */
#line 310 "Minus.m3"
} /* Minus_param_I_LC */
#line 310 "Minus.m3"
 /* set_source_line */
#line 310 "Minus.m3"
#line 311 "Minus.m3"
 /* begin_procedure */
#line 311 "Minus.m3"
struct Minus__Minus_param_I_LC_Frame_t {
#line 311 "Minus.m3"
ADDRESS _unused;
#line 311 "Minus.m3"
};
#line 311 "Minus.m3"
LONGINT
__cdecl
Minus__Minus_param_I_LC(
   /* Param_Type1 */ INTEGER a_L_530,
   /* Param_Type1 */ LONGCARD b_L_531)
{
#line 311 "Minus.m3"
Minus__Minus_param_I_LC_Frame_t _frame;
#line 311 "Minus.m3"
_frame._unused=(ADDRESS)&_frame;
#line 311 "Minus.m3"
 /* load */
#line 311 "Minus.m3"
 /* loophole */
#line 311 "Minus.m3"
 /* load */
#line 311 "Minus.m3"
 /* subtract */
#line 311 "Minus.m3"
 /* exit_proc */
#line 311 "Minus.m3"
return ((INT64)(((INT64)((INT64)a_L_530))- ((INT64)(b_L_531))));
#line 311 "Minus.m3"
 /* end_procedure */
#line 311 "Minus.m3"
} /* uMinus_var_I_u16 */
#line 311 "Minus.m3"
 /* set_source_line */
#line 311 "Minus.m3"
#line 312 "Minus.m3"
 /* begin_procedure */
#line 312 "Minus.m3"
struct Minus__uMinus_var_I_u16_Frame_t {
#line 312 "Minus.m3"
ADDRESS _unused;
#line 312 "Minus.m3"
};
#line 312 "Minus.m3"
Word__T
__cdecl
Minus__uMinus_var_I_u16(void)
{
#line 312 "Minus.m3"
Minus__uMinus_var_I_u16_Frame_t _frame;
#line 312 "Minus.m3"
_frame._unused=(ADDRESS)&_frame;
#line 312 "Minus.m3"
 /* load */
#line 312 "Minus.m3"
 /* load */
#line 312 "Minus.m3"
 /* subtract */
#line 312 "Minus.m3"
 /* exit_proc */
#line 312 "Minus.m3"
return ((UINT64)(((UINT64)(*((INT64*)(INT64_(152)+((ADDRESS)(&Minus_m_M_Minus_L_13))))))-((UINT64)(((INT64)(*((UINT16*)(INT64_(144)+((ADDRESS)(&Minus_m_M_Minus_L_13))))))))));
#line 312 "Minus.m3"
 /* end_procedure */
#line 312 "Minus.m3"
} /* Minus_var_I_u16 */
#line 312 "Minus.m3"
 /* set_source_line */
#line 312 "Minus.m3"
#line 313 "Minus.m3"
 /* begin_procedure */
#line 313 "Minus.m3"
struct Minus__Minus_var_I_u16_Frame_t {
#line 313 "Minus.m3"
ADDRESS _unused;
#line 313 "Minus.m3"
};
#line 313 "Minus.m3"
INTEGER
__cdecl
Minus__Minus_var_I_u16(void)
{
#line 313 "Minus.m3"
Minus__Minus_var_I_u16_Frame_t _frame;
#line 313 "Minus.m3"
_frame._unused=(ADDRESS)&_frame;
#line 313 "Minus.m3"
 /* load */
#line 313 "Minus.m3"
 /* load */
#line 313 "Minus.m3"
 /* subtract */
#line 313 "Minus.m3"
 /* exit_proc */
#line 313 "Minus.m3"
return ((INT64)(((INT64)(*((INT64*)(INT64_(152)+((ADDRESS)(&Minus_m_M_Minus_L_13))))))- ((INT64)(*((UINT16*)(INT64_(144)+((ADDRESS)(&Minus_m_M_Minus_L_13))))))));
#line 313 "Minus.m3"
 /* end_procedure */
#line 313 "Minus.m3"
} /* uMinus_param_I_u16 */
#line 313 "Minus.m3"
 /* set_source_line */
#line 313 "Minus.m3"
#line 314 "Minus.m3"
 /* begin_procedure */
#line 314 "Minus.m3"
struct Minus__uMinus_param_I_u16_Frame_t {
#line 314 "Minus.m3"
ADDRESS _unused;
#line 314 "Minus.m3"
};
#line 314 "Minus.m3"
Word__T
__cdecl
Minus__uMinus_param_I_u16(
   /* Param_Type1 */ INTEGER a_L_535,
   /* Param_Type1 */ Minus__UINT16 b_L_536)
{
#line 314 "Minus.m3"
Minus__uMinus_param_I_u16_Frame_t _frame;
#line 314 "Minus.m3"
_frame._unused=(ADDRESS)&_frame;
#line 314 "Minus.m3"
 /* load */
#line 314 "Minus.m3"
 /* load */
#line 314 "Minus.m3"
 /* subtract */
#line 314 "Minus.m3"
 /* exit_proc */
#line 314 "Minus.m3"
return ((UINT64)(((UINT64)(a_L_535))-((UINT64)(((INT64)(b_L_536))))));
#line 314 "Minus.m3"
 /* end_procedure */
#line 314 "Minus.m3"
} /* Minus_param_I_u16 */
#line 314 "Minus.m3"
 /* set_source_line */
#line 314 "Minus.m3"
#line 315 "Minus.m3"
 /* begin_procedure */
#line 315 "Minus.m3"
struct Minus__Minus_param_I_u16_Frame_t {
#line 315 "Minus.m3"
ADDRESS _unused;
#line 315 "Minus.m3"
};
#line 315 "Minus.m3"
INTEGER
__cdecl
Minus__Minus_param_I_u16(
   /* Param_Type1 */ INTEGER a_L_538,
   /* Param_Type1 */ Minus__UINT16 b_L_539)
{
#line 315 "Minus.m3"
Minus__Minus_param_I_u16_Frame_t _frame;
#line 315 "Minus.m3"
_frame._unused=(ADDRESS)&_frame;
#line 315 "Minus.m3"
 /* load */
#line 315 "Minus.m3"
 /* load */
#line 315 "Minus.m3"
 /* subtract */
#line 315 "Minus.m3"
 /* exit_proc */
#line 315 "Minus.m3"
return ((INT64)( a_L_538- ((INT64)(b_L_539))));
#line 315 "Minus.m3"
 /* end_procedure */
#line 315 "Minus.m3"
} /* uMinus_var_I_I */
#line 315 "Minus.m3"
 /* set_source_line */
#line 315 "Minus.m3"
#line 316 "Minus.m3"
 /* begin_procedure */
#line 316 "Minus.m3"
struct Minus__uMinus_var_I_I_Frame_t {
#line 316 "Minus.m3"
ADDRESS _unused;
#line 316 "Minus.m3"
};
#line 316 "Minus.m3"
Word__T
__cdecl
Minus__uMinus_var_I_I(void)
{
#line 316 "Minus.m3"
Minus__uMinus_var_I_I_Frame_t _frame;
#line 316 "Minus.m3"
_frame._unused=(ADDRESS)&_frame;
#line 316 "Minus.m3"
 /* load */
#line 316 "Minus.m3"
 /* load */
#line 316 "Minus.m3"
 /* subtract */
#line 316 "Minus.m3"
 /* exit_proc */
#line 316 "Minus.m3"
return ((UINT64)(((UINT64)(*((INT64*)(INT64_(152)+((ADDRESS)(&Minus_m_M_Minus_L_13))))))-((UINT64)(*((INT64*)(INT64_(152)+((ADDRESS)(&Minus_m_M_Minus_L_13))))))));
#line 316 "Minus.m3"
 /* end_procedure */
#line 316 "Minus.m3"
} /* Minus_var_I_I */
#line 316 "Minus.m3"
 /* set_source_line */
#line 316 "Minus.m3"
#line 317 "Minus.m3"
 /* begin_procedure */
#line 317 "Minus.m3"
struct Minus__Minus_var_I_I_Frame_t {
#line 317 "Minus.m3"
ADDRESS _unused;
#line 317 "Minus.m3"
};
#line 317 "Minus.m3"
INTEGER
__cdecl
Minus__Minus_var_I_I(void)
{
#line 317 "Minus.m3"
Minus__Minus_var_I_I_Frame_t _frame;
#line 317 "Minus.m3"
_frame._unused=(ADDRESS)&_frame;
#line 317 "Minus.m3"
 /* load */
#line 317 "Minus.m3"
 /* load */
#line 317 "Minus.m3"
 /* subtract */
#line 317 "Minus.m3"
 /* exit_proc */
#line 317 "Minus.m3"
return ((INT64)(((INT64)(*((INT64*)(INT64_(152)+((ADDRESS)(&Minus_m_M_Minus_L_13))))))-((INT64)(*((INT64*)(INT64_(152)+((ADDRESS)(&Minus_m_M_Minus_L_13))))))));
#line 317 "Minus.m3"
 /* end_procedure */
#line 317 "Minus.m3"
} /* uMinus_param_I_I */
#line 317 "Minus.m3"
 /* set_source_line */
#line 317 "Minus.m3"
#line 318 "Minus.m3"
 /* begin_procedure */
#line 318 "Minus.m3"
struct Minus__uMinus_param_I_I_Frame_t {
#line 318 "Minus.m3"
ADDRESS _unused;
#line 318 "Minus.m3"
};
#line 318 "Minus.m3"
Word__T
__cdecl
Minus__uMinus_param_I_I(
   /* Param_Type1 */ INTEGER a_L_543,
   /* Param_Type1 */ INTEGER b_L_544)
{
#line 318 "Minus.m3"
Minus__uMinus_param_I_I_Frame_t _frame;
#line 318 "Minus.m3"
_frame._unused=(ADDRESS)&_frame;
#line 318 "Minus.m3"
 /* load */
#line 318 "Minus.m3"
 /* load */
#line 318 "Minus.m3"
 /* subtract */
#line 318 "Minus.m3"
 /* exit_proc */
#line 318 "Minus.m3"
return ((UINT64)(((UINT64)(a_L_543))-((UINT64)(b_L_544))));
#line 318 "Minus.m3"
 /* end_procedure */
#line 318 "Minus.m3"
} /* Minus_param_I_I */
#line 318 "Minus.m3"
 /* set_source_line */
#line 318 "Minus.m3"
#line 319 "Minus.m3"
 /* begin_procedure */
#line 319 "Minus.m3"
struct Minus__Minus_param_I_I_Frame_t {
#line 319 "Minus.m3"
ADDRESS _unused;
#line 319 "Minus.m3"
};
#line 319 "Minus.m3"
INTEGER
__cdecl
Minus__Minus_param_I_I(
   /* Param_Type1 */ INTEGER a_L_546,
   /* Param_Type1 */ INTEGER b_L_547)
{
#line 319 "Minus.m3"
Minus__Minus_param_I_I_Frame_t _frame;
#line 319 "Minus.m3"
_frame._unused=(ADDRESS)&_frame;
#line 319 "Minus.m3"
 /* load */
#line 319 "Minus.m3"
 /* load */
#line 319 "Minus.m3"
 /* subtract */
#line 319 "Minus.m3"
 /* exit_proc */
#line 319 "Minus.m3"
return ((INT64)( a_L_546- b_L_547));
#line 319 "Minus.m3"
 /* end_procedure */
#line 319 "Minus.m3"
} /* uMinus_var_I_i64 */
#line 319 "Minus.m3"
 /* set_source_line */
#line 319 "Minus.m3"
#line 320 "Minus.m3"
 /* begin_procedure */
#line 320 "Minus.m3"
struct Minus__uMinus_var_I_i64_Frame_t {
#line 320 "Minus.m3"
ADDRESS _unused;
#line 320 "Minus.m3"
};
#line 320 "Minus.m3"
Long__T
__cdecl
Minus__uMinus_var_I_i64(void)
{
#line 320 "Minus.m3"
Minus__uMinus_var_I_i64_Frame_t _frame;
#line 320 "Minus.m3"
_frame._unused=(ADDRESS)&_frame;
#line 320 "Minus.m3"
 /* load */
#line 320 "Minus.m3"
 /* loophole */
#line 320 "Minus.m3"
 /* load */
#line 320 "Minus.m3"
 /* subtract */
#line 320 "Minus.m3"
 /* exit_proc */
#line 320 "Minus.m3"
return ((UINT64)(((UINT64)((INT64)*((INT64*)(INT64_(152)+((ADDRESS)(&Minus_m_M_Minus_L_13))))))-((UINT64)(*((INT64*)(INT64_(160)+((ADDRESS)(&Minus_m_M_Minus_L_13))))))));
#line 320 "Minus.m3"
 /* end_procedure */
#line 320 "Minus.m3"
} /* Minus_var_I_i64 */
#line 320 "Minus.m3"
 /* set_source_line */
#line 320 "Minus.m3"
#line 321 "Minus.m3"
 /* begin_procedure */
#line 321 "Minus.m3"
struct Minus__Minus_var_I_i64_Frame_t {
#line 321 "Minus.m3"
ADDRESS _unused;
#line 321 "Minus.m3"
};
#line 321 "Minus.m3"
LONGINT
__cdecl
Minus__Minus_var_I_i64(void)
{
#line 321 "Minus.m3"
Minus__Minus_var_I_i64_Frame_t _frame;
#line 321 "Minus.m3"
_frame._unused=(ADDRESS)&_frame;
#line 321 "Minus.m3"
 /* load */
#line 321 "Minus.m3"
 /* loophole */
#line 321 "Minus.m3"
 /* load */
#line 321 "Minus.m3"
 /* subtract */
#line 321 "Minus.m3"
 /* exit_proc */
#line 321 "Minus.m3"
return ((INT64)(((INT64)((INT64)*((INT64*)(INT64_(152)+((ADDRESS)(&Minus_m_M_Minus_L_13))))))-((INT64)(*((INT64*)(INT64_(160)+((ADDRESS)(&Minus_m_M_Minus_L_13))))))));
#line 321 "Minus.m3"
 /* end_procedure */
#line 321 "Minus.m3"
} /* uMinus_param_I_i64 */
#line 321 "Minus.m3"
 /* set_source_line */
#line 321 "Minus.m3"
#line 322 "Minus.m3"
 /* begin_procedure */
#line 322 "Minus.m3"
struct Minus__uMinus_param_I_i64_Frame_t {
#line 322 "Minus.m3"
ADDRESS _unused;
#line 322 "Minus.m3"
};
#line 322 "Minus.m3"
Long__T
__cdecl
Minus__uMinus_param_I_i64(
   /* Param_Type1 */ INTEGER a_L_551,
   /* Param_Type1 */ Minus__INT64 b_L_552)
{
#line 322 "Minus.m3"
Minus__uMinus_param_I_i64_Frame_t _frame;
#line 322 "Minus.m3"
_frame._unused=(ADDRESS)&_frame;
#line 322 "Minus.m3"
 /* load */
#line 322 "Minus.m3"
 /* loophole */
#line 322 "Minus.m3"
 /* load */
#line 322 "Minus.m3"
 /* subtract */
#line 322 "Minus.m3"
 /* exit_proc */
#line 322 "Minus.m3"
return ((UINT64)(((UINT64)((INT64)a_L_551))-((UINT64)(b_L_552))));
#line 322 "Minus.m3"
 /* end_procedure */
#line 322 "Minus.m3"
} /* Minus_param_I_i64 */
#line 322 "Minus.m3"
 /* set_source_line */
#line 322 "Minus.m3"
#line 323 "Minus.m3"
 /* begin_procedure */
#line 323 "Minus.m3"
struct Minus__Minus_param_I_i64_Frame_t {
#line 323 "Minus.m3"
ADDRESS _unused;
#line 323 "Minus.m3"
};
#line 323 "Minus.m3"
LONGINT
__cdecl
Minus__Minus_param_I_i64(
   /* Param_Type1 */ INTEGER a_L_554,
   /* Param_Type1 */ Minus__INT64 b_L_555)
{
#line 323 "Minus.m3"
Minus__Minus_param_I_i64_Frame_t _frame;
#line 323 "Minus.m3"
_frame._unused=(ADDRESS)&_frame;
#line 323 "Minus.m3"
 /* load */
#line 323 "Minus.m3"
 /* loophole */
#line 323 "Minus.m3"
 /* load */
#line 323 "Minus.m3"
 /* subtract */
#line 323 "Minus.m3"
 /* exit_proc */
#line 323 "Minus.m3"
return ((INT64)(((INT64)((INT64)a_L_554))- b_L_555));
#line 323 "Minus.m3"
 /* end_procedure */
#line 323 "Minus.m3"
} /* uMinus_var_I_i16 */
#line 323 "Minus.m3"
 /* set_source_line */
#line 323 "Minus.m3"
#line 324 "Minus.m3"
 /* begin_procedure */
#line 324 "Minus.m3"
struct Minus__uMinus_var_I_i16_Frame_t {
#line 324 "Minus.m3"
ADDRESS _unused;
#line 324 "Minus.m3"
};
#line 324 "Minus.m3"
Word__T
__cdecl
Minus__uMinus_var_I_i16(void)
{
#line 324 "Minus.m3"
Minus__uMinus_var_I_i16_Frame_t _frame;
#line 324 "Minus.m3"
_frame._unused=(ADDRESS)&_frame;
#line 324 "Minus.m3"
 /* load */
#line 324 "Minus.m3"
 /* load */
#line 324 "Minus.m3"
 /* subtract */
#line 324 "Minus.m3"
 /* exit_proc */
#line 324 "Minus.m3"
return ((UINT64)(((UINT64)(*((INT64*)(INT64_(152)+((ADDRESS)(&Minus_m_M_Minus_L_13))))))-((UINT64)(((INT64)(*((INT16*)(INT64_(172)+((ADDRESS)(&Minus_m_M_Minus_L_13))))))))));
#line 324 "Minus.m3"
 /* end_procedure */
#line 324 "Minus.m3"
} /* Minus_var_I_i16 */
#line 324 "Minus.m3"
 /* set_source_line */
#line 324 "Minus.m3"
#line 325 "Minus.m3"
 /* begin_procedure */
#line 325 "Minus.m3"
struct Minus__Minus_var_I_i16_Frame_t {
#line 325 "Minus.m3"
ADDRESS _unused;
#line 325 "Minus.m3"
};
#line 325 "Minus.m3"
INTEGER
__cdecl
Minus__Minus_var_I_i16(void)
{
#line 325 "Minus.m3"
Minus__Minus_var_I_i16_Frame_t _frame;
#line 325 "Minus.m3"
_frame._unused=(ADDRESS)&_frame;
#line 325 "Minus.m3"
 /* load */
#line 325 "Minus.m3"
 /* load */
#line 325 "Minus.m3"
 /* subtract */
#line 325 "Minus.m3"
 /* exit_proc */
#line 325 "Minus.m3"
return ((INT64)(((INT64)(*((INT64*)(INT64_(152)+((ADDRESS)(&Minus_m_M_Minus_L_13))))))- ((INT64)(*((INT16*)(INT64_(172)+((ADDRESS)(&Minus_m_M_Minus_L_13))))))));
#line 325 "Minus.m3"
 /* end_procedure */
#line 325 "Minus.m3"
} /* uMinus_param_I_i16 */
#line 325 "Minus.m3"
 /* set_source_line */
#line 325 "Minus.m3"
#line 326 "Minus.m3"
 /* begin_procedure */
#line 326 "Minus.m3"
struct Minus__uMinus_param_I_i16_Frame_t {
#line 326 "Minus.m3"
ADDRESS _unused;
#line 326 "Minus.m3"
};
#line 326 "Minus.m3"
Word__T
__cdecl
Minus__uMinus_param_I_i16(
   /* Param_Type1 */ INTEGER a_L_559,
   /* Param_Type1 */ Minus__INT16 b_L_560)
{
#line 326 "Minus.m3"
Minus__uMinus_param_I_i16_Frame_t _frame;
#line 326 "Minus.m3"
_frame._unused=(ADDRESS)&_frame;
#line 326 "Minus.m3"
 /* load */
#line 326 "Minus.m3"
 /* load */
#line 326 "Minus.m3"
 /* subtract */
#line 326 "Minus.m3"
 /* exit_proc */
#line 326 "Minus.m3"
return ((UINT64)(((UINT64)(a_L_559))-((UINT64)(((INT64)(b_L_560))))));
#line 326 "Minus.m3"
 /* end_procedure */
#line 326 "Minus.m3"
} /* Minus_param_I_i16 */
#line 326 "Minus.m3"
 /* set_source_line */
#line 326 "Minus.m3"
#line 327 "Minus.m3"
 /* begin_procedure */
#line 327 "Minus.m3"
struct Minus__Minus_param_I_i16_Frame_t {
#line 327 "Minus.m3"
ADDRESS _unused;
#line 327 "Minus.m3"
};
#line 327 "Minus.m3"
INTEGER
__cdecl
Minus__Minus_param_I_i16(
   /* Param_Type1 */ INTEGER a_L_562,
   /* Param_Type1 */ Minus__INT16 b_L_563)
{
#line 327 "Minus.m3"
Minus__Minus_param_I_i16_Frame_t _frame;
#line 327 "Minus.m3"
_frame._unused=(ADDRESS)&_frame;
#line 327 "Minus.m3"
 /* load */
#line 327 "Minus.m3"
 /* load */
#line 327 "Minus.m3"
 /* subtract */
#line 327 "Minus.m3"
 /* exit_proc */
#line 327 "Minus.m3"
return ((INT64)( a_L_562- ((INT64)(b_L_563))));
#line 327 "Minus.m3"
 /* end_procedure */
#line 327 "Minus.m3"
} /* uMinus_var_I_C */
#line 327 "Minus.m3"
 /* set_source_line */
#line 327 "Minus.m3"
#line 328 "Minus.m3"
 /* begin_procedure */
#line 328 "Minus.m3"
struct Minus__uMinus_var_I_C_Frame_t {
#line 328 "Minus.m3"
ADDRESS _unused;
#line 328 "Minus.m3"
};
#line 328 "Minus.m3"
Word__T
__cdecl
Minus__uMinus_var_I_C(void)
{
#line 328 "Minus.m3"
Minus__uMinus_var_I_C_Frame_t _frame;
#line 328 "Minus.m3"
_frame._unused=(ADDRESS)&_frame;
#line 328 "Minus.m3"
 /* load */
#line 328 "Minus.m3"
 /* load */
#line 328 "Minus.m3"
 /* subtract */
#line 328 "Minus.m3"
 /* exit_proc */
#line 328 "Minus.m3"
return ((UINT64)(((UINT64)(*((INT64*)(INT64_(152)+((ADDRESS)(&Minus_m_M_Minus_L_13))))))-((UINT64)(((INT64)(*((UINT64*)(INT64_(176)+((ADDRESS)(&Minus_m_M_Minus_L_13))))))))));
#line 328 "Minus.m3"
 /* end_procedure */
#line 328 "Minus.m3"
} /* Minus_var_I_C */
#line 328 "Minus.m3"
 /* set_source_line */
#line 328 "Minus.m3"
#line 329 "Minus.m3"
 /* begin_procedure */
#line 329 "Minus.m3"
struct Minus__Minus_var_I_C_Frame_t {
#line 329 "Minus.m3"
ADDRESS _unused;
#line 329 "Minus.m3"
};
#line 329 "Minus.m3"
INTEGER
__cdecl
Minus__Minus_var_I_C(void)
{
#line 329 "Minus.m3"
Minus__Minus_var_I_C_Frame_t _frame;
#line 329 "Minus.m3"
_frame._unused=(ADDRESS)&_frame;
#line 329 "Minus.m3"
 /* load */
#line 329 "Minus.m3"
 /* load */
#line 329 "Minus.m3"
 /* subtract */
#line 329 "Minus.m3"
 /* exit_proc */
#line 329 "Minus.m3"
return ((INT64)(((INT64)(*((INT64*)(INT64_(152)+((ADDRESS)(&Minus_m_M_Minus_L_13))))))- ((INT64)(*((UINT64*)(INT64_(176)+((ADDRESS)(&Minus_m_M_Minus_L_13))))))));
#line 329 "Minus.m3"
 /* end_procedure */
#line 329 "Minus.m3"
} /* uMinus_param_I_C */
#line 329 "Minus.m3"
 /* set_source_line */
#line 329 "Minus.m3"
#line 330 "Minus.m3"
 /* begin_procedure */
#line 330 "Minus.m3"
struct Minus__uMinus_param_I_C_Frame_t {
#line 330 "Minus.m3"
ADDRESS _unused;
#line 330 "Minus.m3"
};
#line 330 "Minus.m3"
Word__T
__cdecl
Minus__uMinus_param_I_C(
   /* Param_Type1 */ INTEGER a_L_567,
   /* Param_Type1 */ CARDINAL b_L_568)
{
#line 330 "Minus.m3"
Minus__uMinus_param_I_C_Frame_t _frame;
#line 330 "Minus.m3"
_frame._unused=(ADDRESS)&_frame;
#line 330 "Minus.m3"
 /* load */
#line 330 "Minus.m3"
 /* load */
#line 330 "Minus.m3"
 /* subtract */
#line 330 "Minus.m3"
 /* exit_proc */
#line 330 "Minus.m3"
return ((UINT64)(((UINT64)(a_L_567))-((UINT64)(((INT64)(b_L_568))))));
#line 330 "Minus.m3"
 /* end_procedure */
#line 330 "Minus.m3"
} /* Minus_param_I_C */
#line 330 "Minus.m3"
 /* set_source_line */
#line 330 "Minus.m3"
#line 331 "Minus.m3"
 /* begin_procedure */
#line 331 "Minus.m3"
struct Minus__Minus_param_I_C_Frame_t {
#line 331 "Minus.m3"
ADDRESS _unused;
#line 331 "Minus.m3"
};
#line 331 "Minus.m3"
INTEGER
__cdecl
Minus__Minus_param_I_C(
   /* Param_Type1 */ INTEGER a_L_570,
   /* Param_Type1 */ CARDINAL b_L_571)
{
#line 331 "Minus.m3"
Minus__Minus_param_I_C_Frame_t _frame;
#line 331 "Minus.m3"
_frame._unused=(ADDRESS)&_frame;
#line 331 "Minus.m3"
 /* load */
#line 331 "Minus.m3"
 /* load */
#line 331 "Minus.m3"
 /* subtract */
#line 331 "Minus.m3"
 /* exit_proc */
#line 331 "Minus.m3"
return ((INT64)( a_L_570- ((INT64)(b_L_571))));
#line 331 "Minus.m3"
 /* end_procedure */
#line 331 "Minus.m3"
} /* uMinus_var_I_u32 */
#line 331 "Minus.m3"
 /* set_source_line */
#line 331 "Minus.m3"
#line 332 "Minus.m3"
 /* begin_procedure */
#line 332 "Minus.m3"
struct Minus__uMinus_var_I_u32_Frame_t {
#line 332 "Minus.m3"
ADDRESS _unused;
#line 332 "Minus.m3"
};
#line 332 "Minus.m3"
Word__T
__cdecl
Minus__uMinus_var_I_u32(void)
{
#line 332 "Minus.m3"
Minus__uMinus_var_I_u32_Frame_t _frame;
#line 332 "Minus.m3"
_frame._unused=(ADDRESS)&_frame;
#line 332 "Minus.m3"
 /* load */
#line 332 "Minus.m3"
 /* load */
#line 332 "Minus.m3"
 /* subtract */
#line 332 "Minus.m3"
 /* exit_proc */
#line 332 "Minus.m3"
return ((UINT64)(((UINT64)(*((INT64*)(INT64_(152)+((ADDRESS)(&Minus_m_M_Minus_L_13))))))-((UINT64)(((INT64)(*((UINT32*)(INT64_(184)+((ADDRESS)(&Minus_m_M_Minus_L_13))))))))));
#line 332 "Minus.m3"
 /* end_procedure */
#line 332 "Minus.m3"
} /* Minus_var_I_u32 */
#line 332 "Minus.m3"
 /* set_source_line */
#line 332 "Minus.m3"
#line 333 "Minus.m3"
 /* begin_procedure */
#line 333 "Minus.m3"
struct Minus__Minus_var_I_u32_Frame_t {
#line 333 "Minus.m3"
ADDRESS _unused;
#line 333 "Minus.m3"
};
#line 333 "Minus.m3"
INTEGER
__cdecl
Minus__Minus_var_I_u32(void)
{
#line 333 "Minus.m3"
Minus__Minus_var_I_u32_Frame_t _frame;
#line 333 "Minus.m3"
_frame._unused=(ADDRESS)&_frame;
#line 333 "Minus.m3"
 /* load */
#line 333 "Minus.m3"
 /* load */
#line 333 "Minus.m3"
 /* subtract */
#line 333 "Minus.m3"
 /* exit_proc */
#line 333 "Minus.m3"
return ((INT64)(((INT64)(*((INT64*)(INT64_(152)+((ADDRESS)(&Minus_m_M_Minus_L_13))))))- ((INT64)(*((UINT32*)(INT64_(184)+((ADDRESS)(&Minus_m_M_Minus_L_13))))))));
#line 333 "Minus.m3"
 /* end_procedure */
#line 333 "Minus.m3"
} /* uMinus_param_I_u32 */
#line 333 "Minus.m3"
 /* set_source_line */
#line 333 "Minus.m3"
#line 334 "Minus.m3"
 /* begin_procedure */
#line 334 "Minus.m3"
struct Minus__uMinus_param_I_u32_Frame_t {
#line 334 "Minus.m3"
ADDRESS _unused;
#line 334 "Minus.m3"
};
#line 334 "Minus.m3"
Word__T
__cdecl
Minus__uMinus_param_I_u32(
   /* Param_Type1 */ INTEGER a_L_575,
   /* Param_Type1 */ Minus__UINT32 b_L_576)
{
#line 334 "Minus.m3"
Minus__uMinus_param_I_u32_Frame_t _frame;
#line 334 "Minus.m3"
_frame._unused=(ADDRESS)&_frame;
#line 334 "Minus.m3"
 /* load */
#line 334 "Minus.m3"
 /* load */
#line 334 "Minus.m3"
 /* subtract */
#line 334 "Minus.m3"
 /* exit_proc */
#line 334 "Minus.m3"
return ((UINT64)(((UINT64)(a_L_575))-((UINT64)(((INT64)(b_L_576))))));
#line 334 "Minus.m3"
 /* end_procedure */
#line 334 "Minus.m3"
} /* Minus_param_I_u32 */
#line 334 "Minus.m3"
 /* set_source_line */
#line 334 "Minus.m3"
#line 335 "Minus.m3"
 /* begin_procedure */
#line 335 "Minus.m3"
struct Minus__Minus_param_I_u32_Frame_t {
#line 335 "Minus.m3"
ADDRESS _unused;
#line 335 "Minus.m3"
};
#line 335 "Minus.m3"
INTEGER
__cdecl
Minus__Minus_param_I_u32(
   /* Param_Type1 */ INTEGER a_L_578,
   /* Param_Type1 */ Minus__UINT32 b_L_579)
{
#line 335 "Minus.m3"
Minus__Minus_param_I_u32_Frame_t _frame;
#line 335 "Minus.m3"
_frame._unused=(ADDRESS)&_frame;
#line 335 "Minus.m3"
 /* load */
#line 335 "Minus.m3"
 /* load */
#line 335 "Minus.m3"
 /* subtract */
#line 335 "Minus.m3"
 /* exit_proc */
#line 335 "Minus.m3"
return ((INT64)( a_L_578- ((INT64)(b_L_579))));
#line 335 "Minus.m3"
 /* end_procedure */
#line 335 "Minus.m3"
} /* uMinus_var_I_u8 */
#line 335 "Minus.m3"
 /* set_source_line */
#line 335 "Minus.m3"
#line 336 "Minus.m3"
 /* begin_procedure */
#line 336 "Minus.m3"
struct Minus__uMinus_var_I_u8_Frame_t {
#line 336 "Minus.m3"
ADDRESS _unused;
#line 336 "Minus.m3"
};
#line 336 "Minus.m3"
Word__T
__cdecl
Minus__uMinus_var_I_u8(void)
{
#line 336 "Minus.m3"
Minus__uMinus_var_I_u8_Frame_t _frame;
#line 336 "Minus.m3"
_frame._unused=(ADDRESS)&_frame;
#line 336 "Minus.m3"
 /* load */
#line 336 "Minus.m3"
 /* load */
#line 336 "Minus.m3"
 /* subtract */
#line 336 "Minus.m3"
 /* exit_proc */
#line 336 "Minus.m3"
return ((UINT64)(((UINT64)(*((INT64*)(INT64_(152)+((ADDRESS)(&Minus_m_M_Minus_L_13))))))-((UINT64)(((INT64)(*((UINT8*)(INT64_(188)+((ADDRESS)(&Minus_m_M_Minus_L_13))))))))));
#line 336 "Minus.m3"
 /* end_procedure */
#line 336 "Minus.m3"
} /* Minus_var_I_u8 */
#line 336 "Minus.m3"
 /* set_source_line */
#line 336 "Minus.m3"
#line 337 "Minus.m3"
 /* begin_procedure */
#line 337 "Minus.m3"
struct Minus__Minus_var_I_u8_Frame_t {
#line 337 "Minus.m3"
ADDRESS _unused;
#line 337 "Minus.m3"
};
#line 337 "Minus.m3"
INTEGER
__cdecl
Minus__Minus_var_I_u8(void)
{
#line 337 "Minus.m3"
Minus__Minus_var_I_u8_Frame_t _frame;
#line 337 "Minus.m3"
_frame._unused=(ADDRESS)&_frame;
#line 337 "Minus.m3"
 /* load */
#line 337 "Minus.m3"
 /* load */
#line 337 "Minus.m3"
 /* subtract */
#line 337 "Minus.m3"
 /* exit_proc */
#line 337 "Minus.m3"
return ((INT64)(((INT64)(*((INT64*)(INT64_(152)+((ADDRESS)(&Minus_m_M_Minus_L_13))))))- ((INT64)(*((UINT8*)(INT64_(188)+((ADDRESS)(&Minus_m_M_Minus_L_13))))))));
#line 337 "Minus.m3"
 /* end_procedure */
#line 337 "Minus.m3"
} /* uMinus_param_I_u8 */
#line 337 "Minus.m3"
 /* set_source_line */
#line 337 "Minus.m3"
#line 338 "Minus.m3"
 /* begin_procedure */
#line 338 "Minus.m3"
struct Minus__uMinus_param_I_u8_Frame_t {
#line 338 "Minus.m3"
ADDRESS _unused;
#line 338 "Minus.m3"
};
#line 338 "Minus.m3"
Word__T
__cdecl
Minus__uMinus_param_I_u8(
   /* Param_Type1 */ INTEGER a_L_583,
   /* Param_Type1 */ Minus__UINT8 b_L_584)
{
#line 338 "Minus.m3"
Minus__uMinus_param_I_u8_Frame_t _frame;
#line 338 "Minus.m3"
_frame._unused=(ADDRESS)&_frame;
#line 338 "Minus.m3"
 /* load */
#line 338 "Minus.m3"
 /* load */
#line 338 "Minus.m3"
 /* subtract */
#line 338 "Minus.m3"
 /* exit_proc */
#line 338 "Minus.m3"
return ((UINT64)(((UINT64)(a_L_583))-((UINT64)(((INT64)(b_L_584))))));
#line 338 "Minus.m3"
 /* end_procedure */
#line 338 "Minus.m3"
} /* Minus_param_I_u8 */
#line 338 "Minus.m3"
 /* set_source_line */
#line 338 "Minus.m3"
#line 339 "Minus.m3"
 /* begin_procedure */
#line 339 "Minus.m3"
struct Minus__Minus_param_I_u8_Frame_t {
#line 339 "Minus.m3"
ADDRESS _unused;
#line 339 "Minus.m3"
};
#line 339 "Minus.m3"
INTEGER
__cdecl
Minus__Minus_param_I_u8(
   /* Param_Type1 */ INTEGER a_L_586,
   /* Param_Type1 */ Minus__UINT8 b_L_587)
{
#line 339 "Minus.m3"
Minus__Minus_param_I_u8_Frame_t _frame;
#line 339 "Minus.m3"
_frame._unused=(ADDRESS)&_frame;
#line 339 "Minus.m3"
 /* load */
#line 339 "Minus.m3"
 /* load */
#line 339 "Minus.m3"
 /* subtract */
#line 339 "Minus.m3"
 /* exit_proc */
#line 339 "Minus.m3"
return ((INT64)( a_L_586- ((INT64)(b_L_587))));
#line 339 "Minus.m3"
 /* end_procedure */
#line 339 "Minus.m3"
} /* uMinus_var_I_L */
#line 339 "Minus.m3"
 /* set_source_line */
#line 339 "Minus.m3"
#line 340 "Minus.m3"
 /* begin_procedure */
#line 340 "Minus.m3"
struct Minus__uMinus_var_I_L_Frame_t {
#line 340 "Minus.m3"
ADDRESS _unused;
#line 340 "Minus.m3"
};
#line 340 "Minus.m3"
Long__T
__cdecl
Minus__uMinus_var_I_L(void)
{
#line 340 "Minus.m3"
Minus__uMinus_var_I_L_Frame_t _frame;
#line 340 "Minus.m3"
_frame._unused=(ADDRESS)&_frame;
#line 340 "Minus.m3"
 /* load */
#line 340 "Minus.m3"
 /* loophole */
#line 340 "Minus.m3"
 /* load */
#line 340 "Minus.m3"
 /* subtract */
#line 340 "Minus.m3"
 /* exit_proc */
#line 340 "Minus.m3"
return ((UINT64)(((UINT64)((INT64)*((INT64*)(INT64_(152)+((ADDRESS)(&Minus_m_M_Minus_L_13))))))-((UINT64)(*((INT64*)(INT64_(192)+((ADDRESS)(&Minus_m_M_Minus_L_13))))))));
#line 340 "Minus.m3"
 /* end_procedure */
#line 340 "Minus.m3"
} /* Minus_var_I_L */
#line 340 "Minus.m3"
 /* set_source_line */
#line 340 "Minus.m3"
#line 341 "Minus.m3"
 /* begin_procedure */
#line 341 "Minus.m3"
struct Minus__Minus_var_I_L_Frame_t {
#line 341 "Minus.m3"
ADDRESS _unused;
#line 341 "Minus.m3"
};
#line 341 "Minus.m3"
LONGINT
__cdecl
Minus__Minus_var_I_L(void)
{
#line 341 "Minus.m3"
Minus__Minus_var_I_L_Frame_t _frame;
#line 341 "Minus.m3"
_frame._unused=(ADDRESS)&_frame;
#line 341 "Minus.m3"
 /* load */
#line 341 "Minus.m3"
 /* loophole */
#line 341 "Minus.m3"
 /* load */
#line 341 "Minus.m3"
 /* subtract */
#line 341 "Minus.m3"
 /* exit_proc */
#line 341 "Minus.m3"
return ((INT64)(((INT64)((INT64)*((INT64*)(INT64_(152)+((ADDRESS)(&Minus_m_M_Minus_L_13))))))-((INT64)(*((INT64*)(INT64_(192)+((ADDRESS)(&Minus_m_M_Minus_L_13))))))));
#line 341 "Minus.m3"
 /* end_procedure */
#line 341 "Minus.m3"
} /* uMinus_param_I_L */
#line 341 "Minus.m3"
 /* set_source_line */
#line 341 "Minus.m3"
#line 342 "Minus.m3"
 /* begin_procedure */
#line 342 "Minus.m3"
struct Minus__uMinus_param_I_L_Frame_t {
#line 342 "Minus.m3"
ADDRESS _unused;
#line 342 "Minus.m3"
};
#line 342 "Minus.m3"
Long__T
__cdecl
Minus__uMinus_param_I_L(
   /* Param_Type1 */ INTEGER a_L_591,
   /* Param_Type1 */ LONGINT b_L_592)
{
#line 342 "Minus.m3"
Minus__uMinus_param_I_L_Frame_t _frame;
#line 342 "Minus.m3"
_frame._unused=(ADDRESS)&_frame;
#line 342 "Minus.m3"
 /* load */
#line 342 "Minus.m3"
 /* loophole */
#line 342 "Minus.m3"
 /* load */
#line 342 "Minus.m3"
 /* subtract */
#line 342 "Minus.m3"
 /* exit_proc */
#line 342 "Minus.m3"
return ((UINT64)(((UINT64)((INT64)a_L_591))-((UINT64)(b_L_592))));
#line 342 "Minus.m3"
 /* end_procedure */
#line 342 "Minus.m3"
} /* Minus_param_I_L */
#line 342 "Minus.m3"
 /* set_source_line */
#line 342 "Minus.m3"
#line 343 "Minus.m3"
 /* begin_procedure */
#line 343 "Minus.m3"
struct Minus__Minus_param_I_L_Frame_t {
#line 343 "Minus.m3"
ADDRESS _unused;
#line 343 "Minus.m3"
};
#line 343 "Minus.m3"
LONGINT
__cdecl
Minus__Minus_param_I_L(
   /* Param_Type1 */ INTEGER a_L_594,
   /* Param_Type1 */ LONGINT b_L_595)
{
#line 343 "Minus.m3"
Minus__Minus_param_I_L_Frame_t _frame;
#line 343 "Minus.m3"
_frame._unused=(ADDRESS)&_frame;
#line 343 "Minus.m3"
 /* load */
#line 343 "Minus.m3"
 /* loophole */
#line 343 "Minus.m3"
 /* load */
#line 343 "Minus.m3"
 /* subtract */
#line 343 "Minus.m3"
 /* exit_proc */
#line 343 "Minus.m3"
return ((INT64)(((INT64)((INT64)a_L_594))- b_L_595));
#line 343 "Minus.m3"
 /* end_procedure */
#line 343 "Minus.m3"
} /* uMinus_var_i64_i8 */
#line 343 "Minus.m3"
 /* set_source_line */
#line 343 "Minus.m3"
#line 344 "Minus.m3"
 /* begin_procedure */
#line 344 "Minus.m3"
struct Minus__uMinus_var_i64_i8_Frame_t {
#line 344 "Minus.m3"
ADDRESS _unused;
#line 344 "Minus.m3"
};
#line 344 "Minus.m3"
Long__T
__cdecl
Minus__uMinus_var_i64_i8(void)
{
#line 344 "Minus.m3"
Minus__uMinus_var_i64_i8_Frame_t _frame;
#line 344 "Minus.m3"
_frame._unused=(ADDRESS)&_frame;
#line 344 "Minus.m3"
 /* load */
#line 344 "Minus.m3"
 /* loophole */
#line 344 "Minus.m3"
 /* load */
#line 344 "Minus.m3"
 /* swap */
#line 344 "Minus.m3"
 /* subtract */
#line 344 "Minus.m3"
 /* exit_proc */
#line 344 "Minus.m3"
return ((UINT64)(((UINT64)(*((INT64*)(INT64_(160)+((ADDRESS)(&Minus_m_M_Minus_L_13))))))-((UINT64)((INT64)((INT64)(*((INT8*)(INT64_(104)+((ADDRESS)(&Minus_m_M_Minus_L_13))))))))));
#line 344 "Minus.m3"
 /* end_procedure */
#line 344 "Minus.m3"
} /* Minus_var_i64_i8 */
#line 344 "Minus.m3"
 /* set_source_line */
#line 344 "Minus.m3"
#line 345 "Minus.m3"
 /* begin_procedure */
#line 345 "Minus.m3"
struct Minus__Minus_var_i64_i8_Frame_t {
#line 345 "Minus.m3"
ADDRESS _unused;
#line 345 "Minus.m3"
};
#line 345 "Minus.m3"
LONGINT
__cdecl
Minus__Minus_var_i64_i8(void)
{
#line 345 "Minus.m3"
Minus__Minus_var_i64_i8_Frame_t _frame;
#line 345 "Minus.m3"
_frame._unused=(ADDRESS)&_frame;
#line 345 "Minus.m3"
 /* load */
#line 345 "Minus.m3"
 /* loophole */
#line 345 "Minus.m3"
 /* load */
#line 345 "Minus.m3"
 /* swap */
#line 345 "Minus.m3"
 /* subtract */
#line 345 "Minus.m3"
 /* exit_proc */
#line 345 "Minus.m3"
return ((INT64)(((INT64)(*((INT64*)(INT64_(160)+((ADDRESS)(&Minus_m_M_Minus_L_13))))))-((INT64)((INT64)((INT64)(*((INT8*)(INT64_(104)+((ADDRESS)(&Minus_m_M_Minus_L_13))))))))));
#line 345 "Minus.m3"
 /* end_procedure */
#line 345 "Minus.m3"
} /* uMinus_param_i64_i8 */
#line 345 "Minus.m3"
 /* set_source_line */
#line 345 "Minus.m3"
#line 346 "Minus.m3"
 /* begin_procedure */
#line 346 "Minus.m3"
struct Minus__uMinus_param_i64_i8_Frame_t {
#line 346 "Minus.m3"
ADDRESS _unused;
#line 346 "Minus.m3"
};
#line 346 "Minus.m3"
Long__T
__cdecl
Minus__uMinus_param_i64_i8(
   /* Param_Type1 */ Minus__INT64 a_L_599,
   /* Param_Type1 */ Minus__INT8 b_L_600)
{
#line 346 "Minus.m3"
Minus__uMinus_param_i64_i8_Frame_t _frame;
#line 346 "Minus.m3"
_frame._unused=(ADDRESS)&_frame;
#line 346 "Minus.m3"
 /* load */
#line 346 "Minus.m3"
 /* loophole */
#line 346 "Minus.m3"
 /* load */
#line 346 "Minus.m3"
 /* swap */
#line 346 "Minus.m3"
 /* subtract */
#line 346 "Minus.m3"
 /* exit_proc */
#line 346 "Minus.m3"
return ((UINT64)(((UINT64)(a_L_599))-((UINT64)((INT64)((INT64)(b_L_600))))));
#line 346 "Minus.m3"
 /* end_procedure */
#line 346 "Minus.m3"
} /* Minus_param_i64_i8 */
#line 346 "Minus.m3"
 /* set_source_line */
#line 346 "Minus.m3"
#line 347 "Minus.m3"
 /* begin_procedure */
#line 347 "Minus.m3"
struct Minus__Minus_param_i64_i8_Frame_t {
#line 347 "Minus.m3"
ADDRESS _unused;
#line 347 "Minus.m3"
};
#line 347 "Minus.m3"
LONGINT
__cdecl
Minus__Minus_param_i64_i8(
   /* Param_Type1 */ Minus__INT64 a_L_602,
   /* Param_Type1 */ Minus__INT8 b_L_603)
{
#line 347 "Minus.m3"
Minus__Minus_param_i64_i8_Frame_t _frame;
#line 347 "Minus.m3"
_frame._unused=(ADDRESS)&_frame;
#line 347 "Minus.m3"
 /* load */
#line 347 "Minus.m3"
 /* loophole */
#line 347 "Minus.m3"
 /* load */
#line 347 "Minus.m3"
 /* swap */
#line 347 "Minus.m3"
 /* subtract */
#line 347 "Minus.m3"
 /* exit_proc */
#line 347 "Minus.m3"
return ((INT64)( a_L_602-((INT64)((INT64)((INT64)(b_L_603))))));
#line 347 "Minus.m3"
 /* end_procedure */
#line 347 "Minus.m3"
} /* uMinus_var_i64_u64 */
#line 347 "Minus.m3"
 /* set_source_line */
#line 347 "Minus.m3"
#line 348 "Minus.m3"
 /* begin_procedure */
#line 348 "Minus.m3"
struct Minus__uMinus_var_i64_u64_Frame_t {
#line 348 "Minus.m3"
ADDRESS _unused;
#line 348 "Minus.m3"
};
#line 348 "Minus.m3"
Long__T
__cdecl
Minus__uMinus_var_i64_u64(void)
{
#line 348 "Minus.m3"
Minus__uMinus_var_i64_u64_Frame_t _frame;
#line 348 "Minus.m3"
_frame._unused=(ADDRESS)&_frame;
#line 348 "Minus.m3"
 /* load */
#line 348 "Minus.m3"
 /* load */
#line 348 "Minus.m3"
 /* subtract */
#line 348 "Minus.m3"
 /* exit_proc */
#line 348 "Minus.m3"
return ((UINT64)(((UINT64)(*((INT64*)(INT64_(160)+((ADDRESS)(&Minus_m_M_Minus_L_13))))))-((UINT64)(*((INT64*)(INT64_(112)+((ADDRESS)(&Minus_m_M_Minus_L_13))))))));
#line 348 "Minus.m3"
 /* end_procedure */
#line 348 "Minus.m3"
} /* Minus_var_i64_u64 */
#line 348 "Minus.m3"
 /* set_source_line */
#line 348 "Minus.m3"
#line 349 "Minus.m3"
 /* begin_procedure */
#line 349 "Minus.m3"
struct Minus__Minus_var_i64_u64_Frame_t {
#line 349 "Minus.m3"
ADDRESS _unused;
#line 349 "Minus.m3"
};
#line 349 "Minus.m3"
LONGINT
__cdecl
Minus__Minus_var_i64_u64(void)
{
#line 349 "Minus.m3"
Minus__Minus_var_i64_u64_Frame_t _frame;
#line 349 "Minus.m3"
_frame._unused=(ADDRESS)&_frame;
#line 349 "Minus.m3"
 /* load */
#line 349 "Minus.m3"
 /* load */
#line 349 "Minus.m3"
 /* subtract */
#line 349 "Minus.m3"
 /* exit_proc */
#line 349 "Minus.m3"
return ((INT64)(((INT64)(*((INT64*)(INT64_(160)+((ADDRESS)(&Minus_m_M_Minus_L_13))))))-((INT64)(*((INT64*)(INT64_(112)+((ADDRESS)(&Minus_m_M_Minus_L_13))))))));
#line 349 "Minus.m3"
 /* end_procedure */
#line 349 "Minus.m3"
} /* uMinus_param_i64_u64 */
#line 349 "Minus.m3"
 /* set_source_line */
#line 349 "Minus.m3"
#line 350 "Minus.m3"
 /* begin_procedure */
#line 350 "Minus.m3"
struct Minus__uMinus_param_i64_u64_Frame_t {
#line 350 "Minus.m3"
ADDRESS _unused;
#line 350 "Minus.m3"
};
#line 350 "Minus.m3"
Long__T
__cdecl
Minus__uMinus_param_i64_u64(
   /* Param_Type1 */ Minus__INT64 a_L_607,
   /* Param_Type1 */ Minus__UINT64 b_L_608)
{
#line 350 "Minus.m3"
Minus__uMinus_param_i64_u64_Frame_t _frame;
#line 350 "Minus.m3"
_frame._unused=(ADDRESS)&_frame;
#line 350 "Minus.m3"
 /* load */
#line 350 "Minus.m3"
 /* load */
#line 350 "Minus.m3"
 /* subtract */
#line 350 "Minus.m3"
 /* exit_proc */
#line 350 "Minus.m3"
return ((UINT64)(((UINT64)(a_L_607))-((UINT64)(b_L_608))));
#line 350 "Minus.m3"
 /* end_procedure */
#line 350 "Minus.m3"
} /* Minus_param_i64_u64 */
#line 350 "Minus.m3"
 /* set_source_line */
#line 350 "Minus.m3"
#line 351 "Minus.m3"
 /* begin_procedure */
#line 351 "Minus.m3"
struct Minus__Minus_param_i64_u64_Frame_t {
#line 351 "Minus.m3"
ADDRESS _unused;
#line 351 "Minus.m3"
};
#line 351 "Minus.m3"
LONGINT
__cdecl
Minus__Minus_param_i64_u64(
   /* Param_Type1 */ Minus__INT64 a_L_610,
   /* Param_Type1 */ Minus__UINT64 b_L_611)
{
#line 351 "Minus.m3"
Minus__Minus_param_i64_u64_Frame_t _frame;
#line 351 "Minus.m3"
_frame._unused=(ADDRESS)&_frame;
#line 351 "Minus.m3"
 /* load */
#line 351 "Minus.m3"
 /* load */
#line 351 "Minus.m3"
 /* subtract */
#line 351 "Minus.m3"
 /* exit_proc */
#line 351 "Minus.m3"
return ((INT64)( a_L_610- b_L_611));
#line 351 "Minus.m3"
 /* end_procedure */
#line 351 "Minus.m3"
} /* uMinus_var_i64_i32 */
#line 351 "Minus.m3"
 /* set_source_line */
#line 351 "Minus.m3"
#line 352 "Minus.m3"
 /* begin_procedure */
#line 352 "Minus.m3"
struct Minus__uMinus_var_i64_i32_Frame_t {
#line 352 "Minus.m3"
ADDRESS _unused;
#line 352 "Minus.m3"
};
#line 352 "Minus.m3"
Long__T
__cdecl
Minus__uMinus_var_i64_i32(void)
{
#line 352 "Minus.m3"
Minus__uMinus_var_i64_i32_Frame_t _frame;
#line 352 "Minus.m3"
_frame._unused=(ADDRESS)&_frame;
#line 352 "Minus.m3"
 /* load */
#line 352 "Minus.m3"
 /* loophole */
#line 352 "Minus.m3"
 /* load */
#line 352 "Minus.m3"
 /* swap */
#line 352 "Minus.m3"
 /* subtract */
#line 352 "Minus.m3"
 /* exit_proc */
#line 352 "Minus.m3"
return ((UINT64)(((UINT64)(*((INT64*)(INT64_(160)+((ADDRESS)(&Minus_m_M_Minus_L_13))))))-((UINT64)((INT64)((INT64)(*((INT32*)(INT64_(128)+((ADDRESS)(&Minus_m_M_Minus_L_13))))))))));
#line 352 "Minus.m3"
 /* end_procedure */
#line 352 "Minus.m3"
} /* Minus_var_i64_i32 */
#line 352 "Minus.m3"
 /* set_source_line */
#line 352 "Minus.m3"
#line 353 "Minus.m3"
 /* begin_procedure */
#line 353 "Minus.m3"
struct Minus__Minus_var_i64_i32_Frame_t {
#line 353 "Minus.m3"
ADDRESS _unused;
#line 353 "Minus.m3"
};
#line 353 "Minus.m3"
LONGINT
__cdecl
Minus__Minus_var_i64_i32(void)
{
#line 353 "Minus.m3"
Minus__Minus_var_i64_i32_Frame_t _frame;
#line 353 "Minus.m3"
_frame._unused=(ADDRESS)&_frame;
#line 353 "Minus.m3"
 /* load */
#line 353 "Minus.m3"
 /* loophole */
#line 353 "Minus.m3"
 /* load */
#line 353 "Minus.m3"
 /* swap */
#line 353 "Minus.m3"
 /* subtract */
#line 353 "Minus.m3"
 /* exit_proc */
#line 353 "Minus.m3"
return ((INT64)(((INT64)(*((INT64*)(INT64_(160)+((ADDRESS)(&Minus_m_M_Minus_L_13))))))-((INT64)((INT64)((INT64)(*((INT32*)(INT64_(128)+((ADDRESS)(&Minus_m_M_Minus_L_13))))))))));
#line 353 "Minus.m3"
 /* end_procedure */
#line 353 "Minus.m3"
} /* uMinus_param_i64_i32 */
#line 353 "Minus.m3"
 /* set_source_line */
#line 353 "Minus.m3"
#line 354 "Minus.m3"
 /* begin_procedure */
#line 354 "Minus.m3"
struct Minus__uMinus_param_i64_i32_Frame_t {
#line 354 "Minus.m3"
ADDRESS _unused;
#line 354 "Minus.m3"
};
#line 354 "Minus.m3"
Long__T
__cdecl
Minus__uMinus_param_i64_i32(
   /* Param_Type1 */ Minus__INT64 a_L_615,
   /* Param_Type1 */ Minus__INT32 b_L_616)
{
#line 354 "Minus.m3"
Minus__uMinus_param_i64_i32_Frame_t _frame;
#line 354 "Minus.m3"
_frame._unused=(ADDRESS)&_frame;
#line 354 "Minus.m3"
 /* load */
#line 354 "Minus.m3"
 /* loophole */
#line 354 "Minus.m3"
 /* load */
#line 354 "Minus.m3"
 /* swap */
#line 354 "Minus.m3"
 /* subtract */
#line 354 "Minus.m3"
 /* exit_proc */
#line 354 "Minus.m3"
return ((UINT64)(((UINT64)(a_L_615))-((UINT64)((INT64)((INT64)(b_L_616))))));
#line 354 "Minus.m3"
 /* end_procedure */
#line 354 "Minus.m3"
} /* Minus_param_i64_i32 */
#line 354 "Minus.m3"
 /* set_source_line */
#line 354 "Minus.m3"
#line 355 "Minus.m3"
 /* begin_procedure */
#line 355 "Minus.m3"
struct Minus__Minus_param_i64_i32_Frame_t {
#line 355 "Minus.m3"
ADDRESS _unused;
#line 355 "Minus.m3"
};
#line 355 "Minus.m3"
LONGINT
__cdecl
Minus__Minus_param_i64_i32(
   /* Param_Type1 */ Minus__INT64 a_L_618,
   /* Param_Type1 */ Minus__INT32 b_L_619)
{
#line 355 "Minus.m3"
Minus__Minus_param_i64_i32_Frame_t _frame;
#line 355 "Minus.m3"
_frame._unused=(ADDRESS)&_frame;
#line 355 "Minus.m3"
 /* load */
#line 355 "Minus.m3"
 /* loophole */
#line 355 "Minus.m3"
 /* load */
#line 355 "Minus.m3"
 /* swap */
#line 355 "Minus.m3"
 /* subtract */
#line 355 "Minus.m3"
 /* exit_proc */
#line 355 "Minus.m3"
return ((INT64)( a_L_618-((INT64)((INT64)((INT64)(b_L_619))))));
#line 355 "Minus.m3"
 /* end_procedure */
#line 355 "Minus.m3"
} /* uMinus_var_i64_LC */
#line 355 "Minus.m3"
 /* set_source_line */
#line 355 "Minus.m3"
#line 356 "Minus.m3"
 /* begin_procedure */
#line 356 "Minus.m3"
struct Minus__uMinus_var_i64_LC_Frame_t {
#line 356 "Minus.m3"
ADDRESS _unused;
#line 356 "Minus.m3"
};
#line 356 "Minus.m3"
Long__T
__cdecl
Minus__uMinus_var_i64_LC(void)
{
#line 356 "Minus.m3"
Minus__uMinus_var_i64_LC_Frame_t _frame;
#line 356 "Minus.m3"
_frame._unused=(ADDRESS)&_frame;
#line 356 "Minus.m3"
 /* load */
#line 356 "Minus.m3"
 /* load */
#line 356 "Minus.m3"
 /* subtract */
#line 356 "Minus.m3"
 /* exit_proc */
#line 356 "Minus.m3"
return ((UINT64)(((UINT64)(*((INT64*)(INT64_(160)+((ADDRESS)(&Minus_m_M_Minus_L_13))))))-((UINT64)(((INT64)(*((UINT64*)(INT64_(136)+((ADDRESS)(&Minus_m_M_Minus_L_13))))))))));
#line 356 "Minus.m3"
 /* end_procedure */
#line 356 "Minus.m3"
} /* Minus_var_i64_LC */
#line 356 "Minus.m3"
 /* set_source_line */
#line 356 "Minus.m3"
#line 357 "Minus.m3"
 /* begin_procedure */
#line 357 "Minus.m3"
struct Minus__Minus_var_i64_LC_Frame_t {
#line 357 "Minus.m3"
ADDRESS _unused;
#line 357 "Minus.m3"
};
#line 357 "Minus.m3"
LONGINT
__cdecl
Minus__Minus_var_i64_LC(void)
{
#line 357 "Minus.m3"
Minus__Minus_var_i64_LC_Frame_t _frame;
#line 357 "Minus.m3"
_frame._unused=(ADDRESS)&_frame;
#line 357 "Minus.m3"
 /* load */
#line 357 "Minus.m3"
 /* load */
#line 357 "Minus.m3"
 /* subtract */
#line 357 "Minus.m3"
 /* exit_proc */
#line 357 "Minus.m3"
return ((INT64)(((INT64)(*((INT64*)(INT64_(160)+((ADDRESS)(&Minus_m_M_Minus_L_13))))))- ((INT64)(*((UINT64*)(INT64_(136)+((ADDRESS)(&Minus_m_M_Minus_L_13))))))));
#line 357 "Minus.m3"
 /* end_procedure */
#line 357 "Minus.m3"
} /* uMinus_param_i64_LC */
#line 357 "Minus.m3"
 /* set_source_line */
#line 357 "Minus.m3"
#line 358 "Minus.m3"
 /* begin_procedure */
#line 358 "Minus.m3"
struct Minus__uMinus_param_i64_LC_Frame_t {
#line 358 "Minus.m3"
ADDRESS _unused;
#line 358 "Minus.m3"
};
#line 358 "Minus.m3"
Long__T
__cdecl
Minus__uMinus_param_i64_LC(
   /* Param_Type1 */ Minus__INT64 a_L_623,
   /* Param_Type1 */ LONGCARD b_L_624)
{
#line 358 "Minus.m3"
Minus__uMinus_param_i64_LC_Frame_t _frame;
#line 358 "Minus.m3"
_frame._unused=(ADDRESS)&_frame;
#line 358 "Minus.m3"
 /* load */
#line 358 "Minus.m3"
 /* load */
#line 358 "Minus.m3"
 /* subtract */
#line 358 "Minus.m3"
 /* exit_proc */
#line 358 "Minus.m3"
return ((UINT64)(((UINT64)(a_L_623))-((UINT64)(((INT64)(b_L_624))))));
#line 358 "Minus.m3"
 /* end_procedure */
#line 358 "Minus.m3"
} /* Minus_param_i64_LC */
#line 358 "Minus.m3"
 /* set_source_line */
#line 358 "Minus.m3"
#line 359 "Minus.m3"
 /* begin_procedure */
#line 359 "Minus.m3"
struct Minus__Minus_param_i64_LC_Frame_t {
#line 359 "Minus.m3"
ADDRESS _unused;
#line 359 "Minus.m3"
};
#line 359 "Minus.m3"
LONGINT
__cdecl
Minus__Minus_param_i64_LC(
   /* Param_Type1 */ Minus__INT64 a_L_626,
   /* Param_Type1 */ LONGCARD b_L_627)
{
#line 359 "Minus.m3"
Minus__Minus_param_i64_LC_Frame_t _frame;
#line 359 "Minus.m3"
_frame._unused=(ADDRESS)&_frame;
#line 359 "Minus.m3"
 /* load */
#line 359 "Minus.m3"
 /* load */
#line 359 "Minus.m3"
 /* subtract */
#line 359 "Minus.m3"
 /* exit_proc */
#line 359 "Minus.m3"
return ((INT64)( a_L_626- ((INT64)(b_L_627))));
#line 359 "Minus.m3"
 /* end_procedure */
#line 359 "Minus.m3"
} /* uMinus_var_i64_u16 */
#line 359 "Minus.m3"
 /* set_source_line */
#line 359 "Minus.m3"
#line 360 "Minus.m3"
 /* begin_procedure */
#line 360 "Minus.m3"
struct Minus__uMinus_var_i64_u16_Frame_t {
#line 360 "Minus.m3"
ADDRESS _unused;
#line 360 "Minus.m3"
};
#line 360 "Minus.m3"
Long__T
__cdecl
Minus__uMinus_var_i64_u16(void)
{
#line 360 "Minus.m3"
Minus__uMinus_var_i64_u16_Frame_t _frame;
#line 360 "Minus.m3"
_frame._unused=(ADDRESS)&_frame;
#line 360 "Minus.m3"
 /* load */
#line 360 "Minus.m3"
 /* loophole */
#line 360 "Minus.m3"
 /* load */
#line 360 "Minus.m3"
 /* swap */
#line 360 "Minus.m3"
 /* subtract */
#line 360 "Minus.m3"
 /* exit_proc */
#line 360 "Minus.m3"
return ((UINT64)(((UINT64)(*((INT64*)(INT64_(160)+((ADDRESS)(&Minus_m_M_Minus_L_13))))))-((UINT64)((INT64)((INT64)(*((UINT16*)(INT64_(144)+((ADDRESS)(&Minus_m_M_Minus_L_13))))))))));
#line 360 "Minus.m3"
 /* end_procedure */
#line 360 "Minus.m3"
} /* Minus_var_i64_u16 */
#line 360 "Minus.m3"
 /* set_source_line */
#line 360 "Minus.m3"
#line 361 "Minus.m3"
 /* begin_procedure */
#line 361 "Minus.m3"
struct Minus__Minus_var_i64_u16_Frame_t {
#line 361 "Minus.m3"
ADDRESS _unused;
#line 361 "Minus.m3"
};
#line 361 "Minus.m3"
LONGINT
__cdecl
Minus__Minus_var_i64_u16(void)
{
#line 361 "Minus.m3"
Minus__Minus_var_i64_u16_Frame_t _frame;
#line 361 "Minus.m3"
_frame._unused=(ADDRESS)&_frame;
#line 361 "Minus.m3"
 /* load */
#line 361 "Minus.m3"
 /* loophole */
#line 361 "Minus.m3"
 /* load */
#line 361 "Minus.m3"
 /* swap */
#line 361 "Minus.m3"
 /* subtract */
#line 361 "Minus.m3"
 /* exit_proc */
#line 361 "Minus.m3"
return ((INT64)(((INT64)(*((INT64*)(INT64_(160)+((ADDRESS)(&Minus_m_M_Minus_L_13))))))-((INT64)((INT64)((INT64)(*((UINT16*)(INT64_(144)+((ADDRESS)(&Minus_m_M_Minus_L_13))))))))));
#line 361 "Minus.m3"
 /* end_procedure */
#line 361 "Minus.m3"
} /* uMinus_param_i64_u16 */
#line 361 "Minus.m3"
 /* set_source_line */
#line 361 "Minus.m3"
#line 362 "Minus.m3"
 /* begin_procedure */
#line 362 "Minus.m3"
struct Minus__uMinus_param_i64_u16_Frame_t {
#line 362 "Minus.m3"
ADDRESS _unused;
#line 362 "Minus.m3"
};
#line 362 "Minus.m3"
Long__T
__cdecl
Minus__uMinus_param_i64_u16(
   /* Param_Type1 */ Minus__INT64 a_L_631,
   /* Param_Type1 */ Minus__UINT16 b_L_632)
{
#line 362 "Minus.m3"
Minus__uMinus_param_i64_u16_Frame_t _frame;
#line 362 "Minus.m3"
_frame._unused=(ADDRESS)&_frame;
#line 362 "Minus.m3"
 /* load */
#line 362 "Minus.m3"
 /* loophole */
#line 362 "Minus.m3"
 /* load */
#line 362 "Minus.m3"
 /* swap */
#line 362 "Minus.m3"
 /* subtract */
#line 362 "Minus.m3"
 /* exit_proc */
#line 362 "Minus.m3"
return ((UINT64)(((UINT64)(a_L_631))-((UINT64)((INT64)((INT64)(b_L_632))))));
#line 362 "Minus.m3"
 /* end_procedure */
#line 362 "Minus.m3"
} /* Minus_param_i64_u16 */
#line 362 "Minus.m3"
 /* set_source_line */
#line 362 "Minus.m3"
#line 363 "Minus.m3"
 /* begin_procedure */
#line 363 "Minus.m3"
struct Minus__Minus_param_i64_u16_Frame_t {
#line 363 "Minus.m3"
ADDRESS _unused;
#line 363 "Minus.m3"
};
#line 363 "Minus.m3"
LONGINT
__cdecl
Minus__Minus_param_i64_u16(
   /* Param_Type1 */ Minus__INT64 a_L_634,
   /* Param_Type1 */ Minus__UINT16 b_L_635)
{
#line 363 "Minus.m3"
Minus__Minus_param_i64_u16_Frame_t _frame;
#line 363 "Minus.m3"
_frame._unused=(ADDRESS)&_frame;
#line 363 "Minus.m3"
 /* load */
#line 363 "Minus.m3"
 /* loophole */
#line 363 "Minus.m3"
 /* load */
#line 363 "Minus.m3"
 /* swap */
#line 363 "Minus.m3"
 /* subtract */
#line 363 "Minus.m3"
 /* exit_proc */
#line 363 "Minus.m3"
return ((INT64)( a_L_634-((INT64)((INT64)((INT64)(b_L_635))))));
#line 363 "Minus.m3"
 /* end_procedure */
#line 363 "Minus.m3"
} /* uMinus_var_i64_I */
#line 363 "Minus.m3"
 /* set_source_line */
#line 363 "Minus.m3"
#line 364 "Minus.m3"
 /* begin_procedure */
#line 364 "Minus.m3"
struct Minus__uMinus_var_i64_I_Frame_t {
#line 364 "Minus.m3"
ADDRESS _unused;
#line 364 "Minus.m3"
};
#line 364 "Minus.m3"
Long__T
__cdecl
Minus__uMinus_var_i64_I(void)
{
#line 364 "Minus.m3"
Minus__uMinus_var_i64_I_Frame_t _frame;
#line 364 "Minus.m3"
_frame._unused=(ADDRESS)&_frame;
#line 364 "Minus.m3"
 /* load */
#line 364 "Minus.m3"
 /* loophole */
#line 364 "Minus.m3"
 /* load */
#line 364 "Minus.m3"
 /* swap */
#line 364 "Minus.m3"
 /* subtract */
#line 364 "Minus.m3"
 /* exit_proc */
#line 364 "Minus.m3"
return ((UINT64)(((UINT64)(*((INT64*)(INT64_(160)+((ADDRESS)(&Minus_m_M_Minus_L_13))))))-((UINT64)((INT64)*((INT64*)(INT64_(152)+((ADDRESS)(&Minus_m_M_Minus_L_13))))))));
#line 364 "Minus.m3"
 /* end_procedure */
#line 364 "Minus.m3"
} /* Minus_var_i64_I */
#line 364 "Minus.m3"
 /* set_source_line */
#line 364 "Minus.m3"
#line 365 "Minus.m3"
 /* begin_procedure */
#line 365 "Minus.m3"
struct Minus__Minus_var_i64_I_Frame_t {
#line 365 "Minus.m3"
ADDRESS _unused;
#line 365 "Minus.m3"
};
#line 365 "Minus.m3"
LONGINT
__cdecl
Minus__Minus_var_i64_I(void)
{
#line 365 "Minus.m3"
Minus__Minus_var_i64_I_Frame_t _frame;
#line 365 "Minus.m3"
_frame._unused=(ADDRESS)&_frame;
#line 365 "Minus.m3"
 /* load */
#line 365 "Minus.m3"
 /* loophole */
#line 365 "Minus.m3"
 /* load */
#line 365 "Minus.m3"
 /* swap */
#line 365 "Minus.m3"
 /* subtract */
#line 365 "Minus.m3"
 /* exit_proc */
#line 365 "Minus.m3"
return ((INT64)(((INT64)(*((INT64*)(INT64_(160)+((ADDRESS)(&Minus_m_M_Minus_L_13))))))-((INT64)((INT64)*((INT64*)(INT64_(152)+((ADDRESS)(&Minus_m_M_Minus_L_13))))))));
#line 365 "Minus.m3"
 /* end_procedure */
#line 365 "Minus.m3"
} /* uMinus_param_i64_I */
#line 365 "Minus.m3"
 /* set_source_line */
#line 365 "Minus.m3"
#line 366 "Minus.m3"
 /* begin_procedure */
#line 366 "Minus.m3"
struct Minus__uMinus_param_i64_I_Frame_t {
#line 366 "Minus.m3"
ADDRESS _unused;
#line 366 "Minus.m3"
};
#line 366 "Minus.m3"
Long__T
__cdecl
Minus__uMinus_param_i64_I(
   /* Param_Type1 */ Minus__INT64 a_L_639,
   /* Param_Type1 */ INTEGER b_L_640)
{
#line 366 "Minus.m3"
Minus__uMinus_param_i64_I_Frame_t _frame;
#line 366 "Minus.m3"
_frame._unused=(ADDRESS)&_frame;
#line 366 "Minus.m3"
 /* load */
#line 366 "Minus.m3"
 /* loophole */
#line 366 "Minus.m3"
 /* load */
#line 366 "Minus.m3"
 /* swap */
#line 366 "Minus.m3"
 /* subtract */
#line 366 "Minus.m3"
 /* exit_proc */
#line 366 "Minus.m3"
return ((UINT64)(((UINT64)(a_L_639))-((UINT64)((INT64)b_L_640))));
#line 366 "Minus.m3"
 /* end_procedure */
#line 366 "Minus.m3"
} /* Minus_param_i64_I */
#line 366 "Minus.m3"
 /* set_source_line */
#line 366 "Minus.m3"
#line 367 "Minus.m3"
 /* begin_procedure */
#line 367 "Minus.m3"
struct Minus__Minus_param_i64_I_Frame_t {
#line 367 "Minus.m3"
ADDRESS _unused;
#line 367 "Minus.m3"
};
#line 367 "Minus.m3"
LONGINT
__cdecl
Minus__Minus_param_i64_I(
   /* Param_Type1 */ Minus__INT64 a_L_642,
   /* Param_Type1 */ INTEGER b_L_643)
{
#line 367 "Minus.m3"
Minus__Minus_param_i64_I_Frame_t _frame;
#line 367 "Minus.m3"
_frame._unused=(ADDRESS)&_frame;
#line 367 "Minus.m3"
 /* load */
#line 367 "Minus.m3"
 /* loophole */
#line 367 "Minus.m3"
 /* load */
#line 367 "Minus.m3"
 /* swap */
#line 367 "Minus.m3"
 /* subtract */
#line 367 "Minus.m3"
 /* exit_proc */
#line 367 "Minus.m3"
return ((INT64)( a_L_642-((INT64)((INT64)b_L_643))));
#line 367 "Minus.m3"
 /* end_procedure */
#line 367 "Minus.m3"
} /* uMinus_var_i64_i64 */
#line 367 "Minus.m3"
 /* set_source_line */
#line 367 "Minus.m3"
#line 368 "Minus.m3"
 /* begin_procedure */
#line 368 "Minus.m3"
struct Minus__uMinus_var_i64_i64_Frame_t {
#line 368 "Minus.m3"
ADDRESS _unused;
#line 368 "Minus.m3"
};
#line 368 "Minus.m3"
Long__T
__cdecl
Minus__uMinus_var_i64_i64(void)
{
#line 368 "Minus.m3"
Minus__uMinus_var_i64_i64_Frame_t _frame;
#line 368 "Minus.m3"
_frame._unused=(ADDRESS)&_frame;
#line 368 "Minus.m3"
 /* load */
#line 368 "Minus.m3"
 /* load */
#line 368 "Minus.m3"
 /* subtract */
#line 368 "Minus.m3"
 /* exit_proc */
#line 368 "Minus.m3"
return ((UINT64)(((UINT64)(*((INT64*)(INT64_(160)+((ADDRESS)(&Minus_m_M_Minus_L_13))))))-((UINT64)(*((INT64*)(INT64_(160)+((ADDRESS)(&Minus_m_M_Minus_L_13))))))));
#line 368 "Minus.m3"
 /* end_procedure */
#line 368 "Minus.m3"
} /* Minus_var_i64_i64 */
#line 368 "Minus.m3"
 /* set_source_line */
#line 368 "Minus.m3"
#line 369 "Minus.m3"
 /* begin_procedure */
#line 369 "Minus.m3"
struct Minus__Minus_var_i64_i64_Frame_t {
#line 369 "Minus.m3"
ADDRESS _unused;
#line 369 "Minus.m3"
};
#line 369 "Minus.m3"
LONGINT
__cdecl
Minus__Minus_var_i64_i64(void)
{
#line 369 "Minus.m3"
Minus__Minus_var_i64_i64_Frame_t _frame;
#line 369 "Minus.m3"
_frame._unused=(ADDRESS)&_frame;
#line 369 "Minus.m3"
 /* load */
#line 369 "Minus.m3"
 /* load */
#line 369 "Minus.m3"
 /* subtract */
#line 369 "Minus.m3"
 /* exit_proc */
#line 369 "Minus.m3"
return ((INT64)(((INT64)(*((INT64*)(INT64_(160)+((ADDRESS)(&Minus_m_M_Minus_L_13))))))-((INT64)(*((INT64*)(INT64_(160)+((ADDRESS)(&Minus_m_M_Minus_L_13))))))));
#line 369 "Minus.m3"
 /* end_procedure */
#line 369 "Minus.m3"
} /* uMinus_param_i64_i64 */
#line 369 "Minus.m3"
 /* set_source_line */
#line 369 "Minus.m3"
#line 370 "Minus.m3"
 /* begin_procedure */
#line 370 "Minus.m3"
struct Minus__uMinus_param_i64_i64_Frame_t {
#line 370 "Minus.m3"
ADDRESS _unused;
#line 370 "Minus.m3"
};
#line 370 "Minus.m3"
Long__T
__cdecl
Minus__uMinus_param_i64_i64(
   /* Param_Type1 */ Minus__INT64 a_L_647,
   /* Param_Type1 */ Minus__INT64 b_L_648)
{
#line 370 "Minus.m3"
Minus__uMinus_param_i64_i64_Frame_t _frame;
#line 370 "Minus.m3"
_frame._unused=(ADDRESS)&_frame;
#line 370 "Minus.m3"
 /* load */
#line 370 "Minus.m3"
 /* load */
#line 370 "Minus.m3"
 /* subtract */
#line 370 "Minus.m3"
 /* exit_proc */
#line 370 "Minus.m3"
return ((UINT64)(((UINT64)(a_L_647))-((UINT64)(b_L_648))));
#line 370 "Minus.m3"
 /* end_procedure */
#line 370 "Minus.m3"
} /* Minus_param_i64_i64 */
#line 370 "Minus.m3"
 /* set_source_line */
#line 370 "Minus.m3"
#line 371 "Minus.m3"
 /* begin_procedure */
#line 371 "Minus.m3"
struct Minus__Minus_param_i64_i64_Frame_t {
#line 371 "Minus.m3"
ADDRESS _unused;
#line 371 "Minus.m3"
};
#line 371 "Minus.m3"
LONGINT
__cdecl
Minus__Minus_param_i64_i64(
   /* Param_Type1 */ Minus__INT64 a_L_650,
   /* Param_Type1 */ Minus__INT64 b_L_651)
{
#line 371 "Minus.m3"
Minus__Minus_param_i64_i64_Frame_t _frame;
#line 371 "Minus.m3"
_frame._unused=(ADDRESS)&_frame;
#line 371 "Minus.m3"
 /* load */
#line 371 "Minus.m3"
 /* load */
#line 371 "Minus.m3"
 /* subtract */
#line 371 "Minus.m3"
 /* exit_proc */
#line 371 "Minus.m3"
return ((INT64)( a_L_650- b_L_651));
#line 371 "Minus.m3"
 /* end_procedure */
#line 371 "Minus.m3"
} /* uMinus_var_i64_i16 */
#line 371 "Minus.m3"
 /* set_source_line */
#line 371 "Minus.m3"
#line 372 "Minus.m3"
 /* begin_procedure */
#line 372 "Minus.m3"
struct Minus__uMinus_var_i64_i16_Frame_t {
#line 372 "Minus.m3"
ADDRESS _unused;
#line 372 "Minus.m3"
};
#line 372 "Minus.m3"
Long__T
__cdecl
Minus__uMinus_var_i64_i16(void)
{
#line 372 "Minus.m3"
Minus__uMinus_var_i64_i16_Frame_t _frame;
#line 372 "Minus.m3"
_frame._unused=(ADDRESS)&_frame;
#line 372 "Minus.m3"
 /* load */
#line 372 "Minus.m3"
 /* loophole */
#line 372 "Minus.m3"
 /* load */
#line 372 "Minus.m3"
 /* swap */
#line 372 "Minus.m3"
 /* subtract */
#line 372 "Minus.m3"
 /* exit_proc */
#line 372 "Minus.m3"
return ((UINT64)(((UINT64)(*((INT64*)(INT64_(160)+((ADDRESS)(&Minus_m_M_Minus_L_13))))))-((UINT64)((INT64)((INT64)(*((INT16*)(INT64_(172)+((ADDRESS)(&Minus_m_M_Minus_L_13))))))))));
#line 372 "Minus.m3"
 /* end_procedure */
#line 372 "Minus.m3"
} /* Minus_var_i64_i16 */
#line 372 "Minus.m3"
 /* set_source_line */
#line 372 "Minus.m3"
#line 373 "Minus.m3"
 /* begin_procedure */
#line 373 "Minus.m3"
struct Minus__Minus_var_i64_i16_Frame_t {
#line 373 "Minus.m3"
ADDRESS _unused;
#line 373 "Minus.m3"
};
#line 373 "Minus.m3"
LONGINT
__cdecl
Minus__Minus_var_i64_i16(void)
{
#line 373 "Minus.m3"
Minus__Minus_var_i64_i16_Frame_t _frame;
#line 373 "Minus.m3"
_frame._unused=(ADDRESS)&_frame;
#line 373 "Minus.m3"
 /* load */
#line 373 "Minus.m3"
 /* loophole */
#line 373 "Minus.m3"
 /* load */
#line 373 "Minus.m3"
 /* swap */
#line 373 "Minus.m3"
 /* subtract */
#line 373 "Minus.m3"
 /* exit_proc */
#line 373 "Minus.m3"
return ((INT64)(((INT64)(*((INT64*)(INT64_(160)+((ADDRESS)(&Minus_m_M_Minus_L_13))))))-((INT64)((INT64)((INT64)(*((INT16*)(INT64_(172)+((ADDRESS)(&Minus_m_M_Minus_L_13))))))))));
#line 373 "Minus.m3"
 /* end_procedure */
#line 373 "Minus.m3"
} /* uMinus_param_i64_i16 */
#line 373 "Minus.m3"
 /* set_source_line */
#line 373 "Minus.m3"
#line 374 "Minus.m3"
 /* begin_procedure */
#line 374 "Minus.m3"
struct Minus__uMinus_param_i64_i16_Frame_t {
#line 374 "Minus.m3"
ADDRESS _unused;
#line 374 "Minus.m3"
};
#line 374 "Minus.m3"
Long__T
__cdecl
Minus__uMinus_param_i64_i16(
   /* Param_Type1 */ Minus__INT64 a_L_655,
   /* Param_Type1 */ Minus__INT16 b_L_656)
{
#line 374 "Minus.m3"
Minus__uMinus_param_i64_i16_Frame_t _frame;
#line 374 "Minus.m3"
_frame._unused=(ADDRESS)&_frame;
#line 374 "Minus.m3"
 /* load */
#line 374 "Minus.m3"
 /* loophole */
#line 374 "Minus.m3"
 /* load */
#line 374 "Minus.m3"
 /* swap */
#line 374 "Minus.m3"
 /* subtract */
#line 374 "Minus.m3"
 /* exit_proc */
#line 374 "Minus.m3"
return ((UINT64)(((UINT64)(a_L_655))-((UINT64)((INT64)((INT64)(b_L_656))))));
#line 374 "Minus.m3"
 /* end_procedure */
#line 374 "Minus.m3"
} /* Minus_param_i64_i16 */
#line 374 "Minus.m3"
 /* set_source_line */
#line 374 "Minus.m3"
#line 375 "Minus.m3"
 /* begin_procedure */
#line 375 "Minus.m3"
struct Minus__Minus_param_i64_i16_Frame_t {
#line 375 "Minus.m3"
ADDRESS _unused;
#line 375 "Minus.m3"
};
#line 375 "Minus.m3"
LONGINT
__cdecl
Minus__Minus_param_i64_i16(
   /* Param_Type1 */ Minus__INT64 a_L_658,
   /* Param_Type1 */ Minus__INT16 b_L_659)
{
#line 375 "Minus.m3"
Minus__Minus_param_i64_i16_Frame_t _frame;
#line 375 "Minus.m3"
_frame._unused=(ADDRESS)&_frame;
#line 375 "Minus.m3"
 /* load */
#line 375 "Minus.m3"
 /* loophole */
#line 375 "Minus.m3"
 /* load */
#line 375 "Minus.m3"
 /* swap */
#line 375 "Minus.m3"
 /* subtract */
#line 375 "Minus.m3"
 /* exit_proc */
#line 375 "Minus.m3"
return ((INT64)( a_L_658-((INT64)((INT64)((INT64)(b_L_659))))));
#line 375 "Minus.m3"
 /* end_procedure */
#line 375 "Minus.m3"
} /* uMinus_var_i64_C */
#line 375 "Minus.m3"
 /* set_source_line */
#line 375 "Minus.m3"
#line 376 "Minus.m3"
 /* begin_procedure */
#line 376 "Minus.m3"
struct Minus__uMinus_var_i64_C_Frame_t {
#line 376 "Minus.m3"
ADDRESS _unused;
#line 376 "Minus.m3"
};
#line 376 "Minus.m3"
Long__T
__cdecl
Minus__uMinus_var_i64_C(void)
{
#line 376 "Minus.m3"
Minus__uMinus_var_i64_C_Frame_t _frame;
#line 376 "Minus.m3"
_frame._unused=(ADDRESS)&_frame;
#line 376 "Minus.m3"
 /* load */
#line 376 "Minus.m3"
 /* loophole */
#line 376 "Minus.m3"
 /* load */
#line 376 "Minus.m3"
 /* swap */
#line 376 "Minus.m3"
 /* subtract */
#line 376 "Minus.m3"
 /* exit_proc */
#line 376 "Minus.m3"
return ((UINT64)(((UINT64)(*((INT64*)(INT64_(160)+((ADDRESS)(&Minus_m_M_Minus_L_13))))))-((UINT64)((INT64)((INT64)(*((UINT64*)(INT64_(176)+((ADDRESS)(&Minus_m_M_Minus_L_13))))))))));
#line 376 "Minus.m3"
 /* end_procedure */
#line 376 "Minus.m3"
} /* Minus_var_i64_C */
#line 376 "Minus.m3"
 /* set_source_line */
#line 376 "Minus.m3"
#line 377 "Minus.m3"
 /* begin_procedure */
#line 377 "Minus.m3"
struct Minus__Minus_var_i64_C_Frame_t {
#line 377 "Minus.m3"
ADDRESS _unused;
#line 377 "Minus.m3"
};
#line 377 "Minus.m3"
LONGINT
__cdecl
Minus__Minus_var_i64_C(void)
{
#line 377 "Minus.m3"
Minus__Minus_var_i64_C_Frame_t _frame;
#line 377 "Minus.m3"
_frame._unused=(ADDRESS)&_frame;
#line 377 "Minus.m3"
 /* load */
#line 377 "Minus.m3"
 /* loophole */
#line 377 "Minus.m3"
 /* load */
#line 377 "Minus.m3"
 /* swap */
#line 377 "Minus.m3"
 /* subtract */
#line 377 "Minus.m3"
 /* exit_proc */
#line 377 "Minus.m3"
return ((INT64)(((INT64)(*((INT64*)(INT64_(160)+((ADDRESS)(&Minus_m_M_Minus_L_13))))))-((INT64)((INT64)((INT64)(*((UINT64*)(INT64_(176)+((ADDRESS)(&Minus_m_M_Minus_L_13))))))))));
#line 377 "Minus.m3"
 /* end_procedure */
#line 377 "Minus.m3"
} /* uMinus_param_i64_C */
#line 377 "Minus.m3"
 /* set_source_line */
#line 377 "Minus.m3"
#line 378 "Minus.m3"
 /* begin_procedure */
#line 378 "Minus.m3"
struct Minus__uMinus_param_i64_C_Frame_t {
#line 378 "Minus.m3"
ADDRESS _unused;
#line 378 "Minus.m3"
};
#line 378 "Minus.m3"
Long__T
__cdecl
Minus__uMinus_param_i64_C(
   /* Param_Type1 */ Minus__INT64 a_L_663,
   /* Param_Type1 */ CARDINAL b_L_664)
{
#line 378 "Minus.m3"
Minus__uMinus_param_i64_C_Frame_t _frame;
#line 378 "Minus.m3"
_frame._unused=(ADDRESS)&_frame;
#line 378 "Minus.m3"
 /* load */
#line 378 "Minus.m3"
 /* loophole */
#line 378 "Minus.m3"
 /* load */
#line 378 "Minus.m3"
 /* swap */
#line 378 "Minus.m3"
 /* subtract */
#line 378 "Minus.m3"
 /* exit_proc */
#line 378 "Minus.m3"
return ((UINT64)(((UINT64)(a_L_663))-((UINT64)((INT64)((INT64)(b_L_664))))));
#line 378 "Minus.m3"
 /* end_procedure */
#line 378 "Minus.m3"
} /* Minus_param_i64_C */
#line 378 "Minus.m3"
 /* set_source_line */
#line 378 "Minus.m3"
#line 379 "Minus.m3"
 /* begin_procedure */
#line 379 "Minus.m3"
struct Minus__Minus_param_i64_C_Frame_t {
#line 379 "Minus.m3"
ADDRESS _unused;
#line 379 "Minus.m3"
};
#line 379 "Minus.m3"
LONGINT
__cdecl
Minus__Minus_param_i64_C(
   /* Param_Type1 */ Minus__INT64 a_L_666,
   /* Param_Type1 */ CARDINAL b_L_667)
{
#line 379 "Minus.m3"
Minus__Minus_param_i64_C_Frame_t _frame;
#line 379 "Minus.m3"
_frame._unused=(ADDRESS)&_frame;
#line 379 "Minus.m3"
 /* load */
#line 379 "Minus.m3"
 /* loophole */
#line 379 "Minus.m3"
 /* load */
#line 379 "Minus.m3"
 /* swap */
#line 379 "Minus.m3"
 /* subtract */
#line 379 "Minus.m3"
 /* exit_proc */
#line 379 "Minus.m3"
return ((INT64)( a_L_666-((INT64)((INT64)((INT64)(b_L_667))))));
#line 379 "Minus.m3"
 /* end_procedure */
#line 379 "Minus.m3"
} /* uMinus_var_i64_u32 */
#line 379 "Minus.m3"
 /* set_source_line */
#line 379 "Minus.m3"
#line 380 "Minus.m3"
 /* begin_procedure */
#line 380 "Minus.m3"
struct Minus__uMinus_var_i64_u32_Frame_t {
#line 380 "Minus.m3"
ADDRESS _unused;
#line 380 "Minus.m3"
};
#line 380 "Minus.m3"
Long__T
__cdecl
Minus__uMinus_var_i64_u32(void)
{
#line 380 "Minus.m3"
Minus__uMinus_var_i64_u32_Frame_t _frame;
#line 380 "Minus.m3"
_frame._unused=(ADDRESS)&_frame;
#line 380 "Minus.m3"
 /* load */
#line 380 "Minus.m3"
 /* loophole */
#line 380 "Minus.m3"
 /* load */
#line 380 "Minus.m3"
 /* swap */
#line 380 "Minus.m3"
 /* subtract */
#line 380 "Minus.m3"
 /* exit_proc */
#line 380 "Minus.m3"
return ((UINT64)(((UINT64)(*((INT64*)(INT64_(160)+((ADDRESS)(&Minus_m_M_Minus_L_13))))))-((UINT64)((INT64)((INT64)(*((UINT32*)(INT64_(184)+((ADDRESS)(&Minus_m_M_Minus_L_13))))))))));
#line 380 "Minus.m3"
 /* end_procedure */
#line 380 "Minus.m3"
} /* Minus_var_i64_u32 */
#line 380 "Minus.m3"
 /* set_source_line */
#line 380 "Minus.m3"
#line 381 "Minus.m3"
 /* begin_procedure */
#line 381 "Minus.m3"
struct Minus__Minus_var_i64_u32_Frame_t {
#line 381 "Minus.m3"
ADDRESS _unused;
#line 381 "Minus.m3"
};
#line 381 "Minus.m3"
LONGINT
__cdecl
Minus__Minus_var_i64_u32(void)
{
#line 381 "Minus.m3"
Minus__Minus_var_i64_u32_Frame_t _frame;
#line 381 "Minus.m3"
_frame._unused=(ADDRESS)&_frame;
#line 381 "Minus.m3"
 /* load */
#line 381 "Minus.m3"
 /* loophole */
#line 381 "Minus.m3"
 /* load */
#line 381 "Minus.m3"
 /* swap */
#line 381 "Minus.m3"
 /* subtract */
#line 381 "Minus.m3"
 /* exit_proc */
#line 381 "Minus.m3"
return ((INT64)(((INT64)(*((INT64*)(INT64_(160)+((ADDRESS)(&Minus_m_M_Minus_L_13))))))-((INT64)((INT64)((INT64)(*((UINT32*)(INT64_(184)+((ADDRESS)(&Minus_m_M_Minus_L_13))))))))));
#line 381 "Minus.m3"
 /* end_procedure */
#line 381 "Minus.m3"
} /* uMinus_param_i64_u32 */
#line 381 "Minus.m3"
 /* set_source_line */
#line 381 "Minus.m3"
#line 382 "Minus.m3"
 /* begin_procedure */
#line 382 "Minus.m3"
struct Minus__uMinus_param_i64_u32_Frame_t {
#line 382 "Minus.m3"
ADDRESS _unused;
#line 382 "Minus.m3"
};
#line 382 "Minus.m3"
Long__T
__cdecl
Minus__uMinus_param_i64_u32(
   /* Param_Type1 */ Minus__INT64 a_L_671,
   /* Param_Type1 */ Minus__UINT32 b_L_672)
{
#line 382 "Minus.m3"
Minus__uMinus_param_i64_u32_Frame_t _frame;
#line 382 "Minus.m3"
_frame._unused=(ADDRESS)&_frame;
#line 382 "Minus.m3"
 /* load */
#line 382 "Minus.m3"
 /* loophole */
#line 382 "Minus.m3"
 /* load */
#line 382 "Minus.m3"
 /* swap */
#line 382 "Minus.m3"
 /* subtract */
#line 382 "Minus.m3"
 /* exit_proc */
#line 382 "Minus.m3"
return ((UINT64)(((UINT64)(a_L_671))-((UINT64)((INT64)((INT64)(b_L_672))))));
#line 382 "Minus.m3"
 /* end_procedure */
#line 382 "Minus.m3"
} /* Minus_param_i64_u32 */
#line 382 "Minus.m3"
 /* set_source_line */
#line 382 "Minus.m3"
#line 383 "Minus.m3"
 /* begin_procedure */
#line 383 "Minus.m3"
struct Minus__Minus_param_i64_u32_Frame_t {
#line 383 "Minus.m3"
ADDRESS _unused;
#line 383 "Minus.m3"
};
#line 383 "Minus.m3"
LONGINT
__cdecl
Minus__Minus_param_i64_u32(
   /* Param_Type1 */ Minus__INT64 a_L_674,
   /* Param_Type1 */ Minus__UINT32 b_L_675)
{
#line 383 "Minus.m3"
Minus__Minus_param_i64_u32_Frame_t _frame;
#line 383 "Minus.m3"
_frame._unused=(ADDRESS)&_frame;
#line 383 "Minus.m3"
 /* load */
#line 383 "Minus.m3"
 /* loophole */
#line 383 "Minus.m3"
 /* load */
#line 383 "Minus.m3"
 /* swap */
#line 383 "Minus.m3"
 /* subtract */
#line 383 "Minus.m3"
 /* exit_proc */
#line 383 "Minus.m3"
return ((INT64)( a_L_674-((INT64)((INT64)((INT64)(b_L_675))))));
#line 383 "Minus.m3"
 /* end_procedure */
#line 383 "Minus.m3"
} /* uMinus_var_i64_u8 */
#line 383 "Minus.m3"
 /* set_source_line */
#line 383 "Minus.m3"
#line 384 "Minus.m3"
 /* begin_procedure */
#line 384 "Minus.m3"
struct Minus__uMinus_var_i64_u8_Frame_t {
#line 384 "Minus.m3"
ADDRESS _unused;
#line 384 "Minus.m3"
};
#line 384 "Minus.m3"
Long__T
__cdecl
Minus__uMinus_var_i64_u8(void)
{
#line 384 "Minus.m3"
Minus__uMinus_var_i64_u8_Frame_t _frame;
#line 384 "Minus.m3"
_frame._unused=(ADDRESS)&_frame;
#line 384 "Minus.m3"
 /* load */
#line 384 "Minus.m3"
 /* loophole */
#line 384 "Minus.m3"
 /* load */
#line 384 "Minus.m3"
 /* swap */
#line 384 "Minus.m3"
 /* subtract */
#line 384 "Minus.m3"
 /* exit_proc */
#line 384 "Minus.m3"
return ((UINT64)(((UINT64)(*((INT64*)(INT64_(160)+((ADDRESS)(&Minus_m_M_Minus_L_13))))))-((UINT64)((INT64)((INT64)(*((UINT8*)(INT64_(188)+((ADDRESS)(&Minus_m_M_Minus_L_13))))))))));
#line 384 "Minus.m3"
 /* end_procedure */
#line 384 "Minus.m3"
} /* Minus_var_i64_u8 */
#line 384 "Minus.m3"
 /* set_source_line */
#line 384 "Minus.m3"
#line 385 "Minus.m3"
 /* begin_procedure */
#line 385 "Minus.m3"
struct Minus__Minus_var_i64_u8_Frame_t {
#line 385 "Minus.m3"
ADDRESS _unused;
#line 385 "Minus.m3"
};
#line 385 "Minus.m3"
LONGINT
__cdecl
Minus__Minus_var_i64_u8(void)
{
#line 385 "Minus.m3"
Minus__Minus_var_i64_u8_Frame_t _frame;
#line 385 "Minus.m3"
_frame._unused=(ADDRESS)&_frame;
#line 385 "Minus.m3"
 /* load */
#line 385 "Minus.m3"
 /* loophole */
#line 385 "Minus.m3"
 /* load */
#line 385 "Minus.m3"
 /* swap */
#line 385 "Minus.m3"
 /* subtract */
#line 385 "Minus.m3"
 /* exit_proc */
#line 385 "Minus.m3"
return ((INT64)(((INT64)(*((INT64*)(INT64_(160)+((ADDRESS)(&Minus_m_M_Minus_L_13))))))-((INT64)((INT64)((INT64)(*((UINT8*)(INT64_(188)+((ADDRESS)(&Minus_m_M_Minus_L_13))))))))));
#line 385 "Minus.m3"
 /* end_procedure */
#line 385 "Minus.m3"
} /* uMinus_param_i64_u8 */
#line 385 "Minus.m3"
 /* set_source_line */
#line 385 "Minus.m3"
#line 386 "Minus.m3"
 /* begin_procedure */
#line 386 "Minus.m3"
struct Minus__uMinus_param_i64_u8_Frame_t {
#line 386 "Minus.m3"
ADDRESS _unused;
#line 386 "Minus.m3"
};
#line 386 "Minus.m3"
Long__T
__cdecl
Minus__uMinus_param_i64_u8(
   /* Param_Type1 */ Minus__INT64 a_L_679,
   /* Param_Type1 */ Minus__UINT8 b_L_680)
{
#line 386 "Minus.m3"
Minus__uMinus_param_i64_u8_Frame_t _frame;
#line 386 "Minus.m3"
_frame._unused=(ADDRESS)&_frame;
#line 386 "Minus.m3"
 /* load */
#line 386 "Minus.m3"
 /* loophole */
#line 386 "Minus.m3"
 /* load */
#line 386 "Minus.m3"
 /* swap */
#line 386 "Minus.m3"
 /* subtract */
#line 386 "Minus.m3"
 /* exit_proc */
#line 386 "Minus.m3"
return ((UINT64)(((UINT64)(a_L_679))-((UINT64)((INT64)((INT64)(b_L_680))))));
#line 386 "Minus.m3"
 /* end_procedure */
#line 386 "Minus.m3"
} /* Minus_param_i64_u8 */
#line 386 "Minus.m3"
 /* set_source_line */
#line 386 "Minus.m3"
#line 387 "Minus.m3"
 /* begin_procedure */
#line 387 "Minus.m3"
struct Minus__Minus_param_i64_u8_Frame_t {
#line 387 "Minus.m3"
ADDRESS _unused;
#line 387 "Minus.m3"
};
#line 387 "Minus.m3"
LONGINT
__cdecl
Minus__Minus_param_i64_u8(
   /* Param_Type1 */ Minus__INT64 a_L_682,
   /* Param_Type1 */ Minus__UINT8 b_L_683)
{
#line 387 "Minus.m3"
Minus__Minus_param_i64_u8_Frame_t _frame;
#line 387 "Minus.m3"
_frame._unused=(ADDRESS)&_frame;
#line 387 "Minus.m3"
 /* load */
#line 387 "Minus.m3"
 /* loophole */
#line 387 "Minus.m3"
 /* load */
#line 387 "Minus.m3"
 /* swap */
#line 387 "Minus.m3"
 /* subtract */
#line 387 "Minus.m3"
 /* exit_proc */
#line 387 "Minus.m3"
return ((INT64)( a_L_682-((INT64)((INT64)((INT64)(b_L_683))))));
#line 387 "Minus.m3"
 /* end_procedure */
#line 387 "Minus.m3"
} /* uMinus_var_i64_L */
#line 387 "Minus.m3"
 /* set_source_line */
#line 387 "Minus.m3"
#line 388 "Minus.m3"
 /* begin_procedure */
#line 388 "Minus.m3"
struct Minus__uMinus_var_i64_L_Frame_t {
#line 388 "Minus.m3"
ADDRESS _unused;
#line 388 "Minus.m3"
};
#line 388 "Minus.m3"
Long__T
__cdecl
Minus__uMinus_var_i64_L(void)
{
#line 388 "Minus.m3"
Minus__uMinus_var_i64_L_Frame_t _frame;
#line 388 "Minus.m3"
_frame._unused=(ADDRESS)&_frame;
#line 388 "Minus.m3"
 /* load */
#line 388 "Minus.m3"
 /* load */
#line 388 "Minus.m3"
 /* subtract */
#line 388 "Minus.m3"
 /* exit_proc */
#line 388 "Minus.m3"
return ((UINT64)(((UINT64)(*((INT64*)(INT64_(160)+((ADDRESS)(&Minus_m_M_Minus_L_13))))))-((UINT64)(*((INT64*)(INT64_(192)+((ADDRESS)(&Minus_m_M_Minus_L_13))))))));
#line 388 "Minus.m3"
 /* end_procedure */
#line 388 "Minus.m3"
} /* Minus_var_i64_L */
#line 388 "Minus.m3"
 /* set_source_line */
#line 388 "Minus.m3"
#line 389 "Minus.m3"
 /* begin_procedure */
#line 389 "Minus.m3"
struct Minus__Minus_var_i64_L_Frame_t {
#line 389 "Minus.m3"
ADDRESS _unused;
#line 389 "Minus.m3"
};
#line 389 "Minus.m3"
LONGINT
__cdecl
Minus__Minus_var_i64_L(void)
{
#line 389 "Minus.m3"
Minus__Minus_var_i64_L_Frame_t _frame;
#line 389 "Minus.m3"
_frame._unused=(ADDRESS)&_frame;
#line 389 "Minus.m3"
 /* load */
#line 389 "Minus.m3"
 /* load */
#line 389 "Minus.m3"
 /* subtract */
#line 389 "Minus.m3"
 /* exit_proc */
#line 389 "Minus.m3"
return ((INT64)(((INT64)(*((INT64*)(INT64_(160)+((ADDRESS)(&Minus_m_M_Minus_L_13))))))-((INT64)(*((INT64*)(INT64_(192)+((ADDRESS)(&Minus_m_M_Minus_L_13))))))));
#line 389 "Minus.m3"
 /* end_procedure */
#line 389 "Minus.m3"
} /* uMinus_param_i64_L */
#line 389 "Minus.m3"
 /* set_source_line */
#line 389 "Minus.m3"
#line 390 "Minus.m3"
 /* begin_procedure */
#line 390 "Minus.m3"
struct Minus__uMinus_param_i64_L_Frame_t {
#line 390 "Minus.m3"
ADDRESS _unused;
#line 390 "Minus.m3"
};
#line 390 "Minus.m3"
Long__T
__cdecl
Minus__uMinus_param_i64_L(
   /* Param_Type1 */ Minus__INT64 a_L_687,
   /* Param_Type1 */ LONGINT b_L_688)
{
#line 390 "Minus.m3"
Minus__uMinus_param_i64_L_Frame_t _frame;
#line 390 "Minus.m3"
_frame._unused=(ADDRESS)&_frame;
#line 390 "Minus.m3"
 /* load */
#line 390 "Minus.m3"
 /* load */
#line 390 "Minus.m3"
 /* subtract */
#line 390 "Minus.m3"
 /* exit_proc */
#line 390 "Minus.m3"
return ((UINT64)(((UINT64)(a_L_687))-((UINT64)(b_L_688))));
#line 390 "Minus.m3"
 /* end_procedure */
#line 390 "Minus.m3"
} /* Minus_param_i64_L */
#line 390 "Minus.m3"
 /* set_source_line */
#line 390 "Minus.m3"
#line 391 "Minus.m3"
 /* begin_procedure */
#line 391 "Minus.m3"
struct Minus__Minus_param_i64_L_Frame_t {
#line 391 "Minus.m3"
ADDRESS _unused;
#line 391 "Minus.m3"
};
#line 391 "Minus.m3"
LONGINT
__cdecl
Minus__Minus_param_i64_L(
   /* Param_Type1 */ Minus__INT64 a_L_690,
   /* Param_Type1 */ LONGINT b_L_691)
{
#line 391 "Minus.m3"
Minus__Minus_param_i64_L_Frame_t _frame;
#line 391 "Minus.m3"
_frame._unused=(ADDRESS)&_frame;
#line 391 "Minus.m3"
 /* load */
#line 391 "Minus.m3"
 /* load */
#line 391 "Minus.m3"
 /* subtract */
#line 391 "Minus.m3"
 /* exit_proc */
#line 391 "Minus.m3"
return ((INT64)( a_L_690- b_L_691));
#line 391 "Minus.m3"
 /* end_procedure */
#line 391 "Minus.m3"
} /* Minus_var_f32_f32 */
#line 391 "Minus.m3"
 /* set_source_line */
#line 391 "Minus.m3"
#line 392 "Minus.m3"
 /* begin_procedure */
#line 392 "Minus.m3"
struct Minus__Minus_var_f32_f32_Frame_t {
#line 392 "Minus.m3"
ADDRESS _unused;
#line 392 "Minus.m3"
};
#line 392 "Minus.m3"
Minus__FLOAT32
__cdecl
Minus__Minus_var_f32_f32(void)
{
#line 392 "Minus.m3"
Minus__Minus_var_f32_f32_Frame_t _frame;
#line 392 "Minus.m3"
_frame._unused=(ADDRESS)&_frame;
#line 392 "Minus.m3"
 /* load */
#line 392 "Minus.m3"
 /* load */
#line 392 "Minus.m3"
 /* subtract */
#line 392 "Minus.m3"
 /* exit_proc */
#line 392 "Minus.m3"
return ((float)(((float)(*((float*)(INT64_(168)+((ADDRESS)(&Minus_m_M_Minus_L_13))))))-((float)(*((float*)(INT64_(168)+((ADDRESS)(&Minus_m_M_Minus_L_13))))))));
#line 392 "Minus.m3"
 /* end_procedure */
#line 392 "Minus.m3"
} /* Minus_param_f32_f32 */
#line 392 "Minus.m3"
 /* set_source_line */
#line 392 "Minus.m3"
#line 393 "Minus.m3"
 /* begin_procedure */
#line 393 "Minus.m3"
struct Minus__Minus_param_f32_f32_Frame_t {
#line 393 "Minus.m3"
ADDRESS _unused;
#line 393 "Minus.m3"
};
#line 393 "Minus.m3"
Minus__FLOAT32
__cdecl
Minus__Minus_param_f32_f32(
   /* Param_Type1 */ Minus__FLOAT32 a_L_694,
   /* Param_Type1 */ Minus__FLOAT32 b_L_695)
{
#line 393 "Minus.m3"
Minus__Minus_param_f32_f32_Frame_t _frame;
#line 393 "Minus.m3"
_frame._unused=(ADDRESS)&_frame;
#line 393 "Minus.m3"
 /* load */
#line 393 "Minus.m3"
 /* load */
#line 393 "Minus.m3"
 /* subtract */
#line 393 "Minus.m3"
 /* exit_proc */
#line 393 "Minus.m3"
return ((float)( a_L_694- b_L_695));
#line 393 "Minus.m3"
 /* end_procedure */
#line 393 "Minus.m3"
} /* uMinus_var_i16_i8 */
#line 393 "Minus.m3"
 /* set_source_line */
#line 393 "Minus.m3"
#line 394 "Minus.m3"
 /* begin_procedure */
#line 394 "Minus.m3"
struct Minus__uMinus_var_i16_i8_Frame_t {
#line 394 "Minus.m3"
ADDRESS _unused;
#line 394 "Minus.m3"
};
#line 394 "Minus.m3"
Word__T
__cdecl
Minus__uMinus_var_i16_i8(void)
{
#line 394 "Minus.m3"
Minus__uMinus_var_i16_i8_Frame_t _frame;
#line 394 "Minus.m3"
_frame._unused=(ADDRESS)&_frame;
#line 394 "Minus.m3"
 /* load */
#line 394 "Minus.m3"
 /* load */
#line 394 "Minus.m3"
 /* subtract */
#line 394 "Minus.m3"
 /* exit_proc */
#line 394 "Minus.m3"
return ((UINT64)(((UINT64)(((INT64)(*((INT16*)(INT64_(172)+((ADDRESS)(&Minus_m_M_Minus_L_13))))))))-((UINT64)(((INT64)(*((INT8*)(INT64_(104)+((ADDRESS)(&Minus_m_M_Minus_L_13))))))))));
#line 394 "Minus.m3"
 /* end_procedure */
#line 394 "Minus.m3"
} /* Minus_var_i16_i8 */
#line 394 "Minus.m3"
 /* set_source_line */
#line 394 "Minus.m3"
#line 395 "Minus.m3"
 /* begin_procedure */
#line 395 "Minus.m3"
struct Minus__Minus_var_i16_i8_Frame_t {
#line 395 "Minus.m3"
ADDRESS _unused;
#line 395 "Minus.m3"
};
#line 395 "Minus.m3"
Minus__INT16
__cdecl
Minus__Minus_var_i16_i8(void)
{
#line 395 "Minus.m3"
 /* Var_Type2 */ INT64 Minus_m_1272_L_1273={0};//always-init
#line 395 "Minus.m3"
Minus__Minus_var_i16_i8_Frame_t _frame;
#line 395 "Minus.m3"
_frame._unused=(ADDRESS)&_frame;
#line 395 "Minus.m3"
 /* load */
#line 395 "Minus.m3"
 /* load */
#line 395 "Minus.m3"
 /* subtract */
#line 395 "Minus.m3"
 /* check_range */
#line 395 "Minus.m3"
 /* store */
#line 395 "Minus.m3"
(*(INT64*)(&Minus_m_1272_L_1273))=(INT64)( ((INT64)( ((INT64)(*((INT16*)(INT64_(172)+((ADDRESS)(&Minus_m_M_Minus_L_13))))))- ((INT64)(*((INT8*)(INT64_(104)+((ADDRESS)(&Minus_m_M_Minus_L_13)))))))));
#line 395 "Minus.m3"
 /* load */
#line 395 "Minus.m3"
if(m3_check_range(INT64,
Minus_m_1272_L_1273,
 INT64_(-32768),
 INT64_(32767)))
#line 395 "Minus.m3"
Minus_m_M_Minus_L_13_CRASH(12641);
#line 395 "Minus.m3"
 /* exit_proc */
#line 395 "Minus.m3"
return Minus_m_1272_L_1273;
#line 395 "Minus.m3"
 /* end_procedure */
#line 395 "Minus.m3"
} /* uMinus_param_i16_i8 */
#line 395 "Minus.m3"
 /* set_source_line */
#line 395 "Minus.m3"
#line 396 "Minus.m3"
 /* begin_procedure */
#line 396 "Minus.m3"
struct Minus__uMinus_param_i16_i8_Frame_t {
#line 396 "Minus.m3"
ADDRESS _unused;
#line 396 "Minus.m3"
};
#line 396 "Minus.m3"
Word__T
__cdecl
Minus__uMinus_param_i16_i8(
   /* Param_Type1 */ Minus__INT16 a_L_699,
   /* Param_Type1 */ Minus__INT8 b_L_700)
{
#line 396 "Minus.m3"
Minus__uMinus_param_i16_i8_Frame_t _frame;
#line 396 "Minus.m3"
_frame._unused=(ADDRESS)&_frame;
#line 396 "Minus.m3"
 /* load */
#line 396 "Minus.m3"
 /* load */
#line 396 "Minus.m3"
 /* subtract */
#line 396 "Minus.m3"
 /* exit_proc */
#line 396 "Minus.m3"
return ((UINT64)(((UINT64)(((INT64)(a_L_699))))-((UINT64)(((INT64)(b_L_700))))));
#line 396 "Minus.m3"
 /* end_procedure */
#line 396 "Minus.m3"
} /* Minus_param_i16_i8 */
#line 396 "Minus.m3"
 /* set_source_line */
#line 396 "Minus.m3"
#line 397 "Minus.m3"
 /* begin_procedure */
#line 397 "Minus.m3"
struct Minus__Minus_param_i16_i8_Frame_t {
#line 397 "Minus.m3"
ADDRESS _unused;
#line 397 "Minus.m3"
};
#line 397 "Minus.m3"
Minus__INT16
__cdecl
Minus__Minus_param_i16_i8(
   /* Param_Type1 */ Minus__INT16 a_L_702,
   /* Param_Type1 */ Minus__INT8 b_L_703)
{
#line 397 "Minus.m3"
 /* Var_Type2 */ INT64 Minus_m_1274_L_1275={0};//always-init
#line 397 "Minus.m3"
Minus__Minus_param_i16_i8_Frame_t _frame;
#line 397 "Minus.m3"
_frame._unused=(ADDRESS)&_frame;
#line 397 "Minus.m3"
 /* load */
#line 397 "Minus.m3"
 /* load */
#line 397 "Minus.m3"
 /* subtract */
#line 397 "Minus.m3"
 /* check_range */
#line 397 "Minus.m3"
 /* store */
#line 397 "Minus.m3"
(*(INT64*)(&Minus_m_1274_L_1275))=(INT64)( ((INT64)( ((INT64)(a_L_702))- ((INT64)(b_L_703)))));
#line 397 "Minus.m3"
 /* load */
#line 397 "Minus.m3"
if(m3_check_range(INT64,
Minus_m_1274_L_1275,
 INT64_(-32768),
 INT64_(32767)))
#line 397 "Minus.m3"
Minus_m_M_Minus_L_13_CRASH(12705);
#line 397 "Minus.m3"
 /* exit_proc */
#line 397 "Minus.m3"
return Minus_m_1274_L_1275;
#line 397 "Minus.m3"
 /* end_procedure */
#line 397 "Minus.m3"
} /* uMinus_var_i16_u64 */
#line 397 "Minus.m3"
 /* set_source_line */
#line 397 "Minus.m3"
#line 398 "Minus.m3"
 /* begin_procedure */
#line 398 "Minus.m3"
struct Minus__uMinus_var_i16_u64_Frame_t {
#line 398 "Minus.m3"
ADDRESS _unused;
#line 398 "Minus.m3"
};
#line 398 "Minus.m3"
Long__T
__cdecl
Minus__uMinus_var_i16_u64(void)
{
#line 398 "Minus.m3"
Minus__uMinus_var_i16_u64_Frame_t _frame;
#line 398 "Minus.m3"
_frame._unused=(ADDRESS)&_frame;
#line 398 "Minus.m3"
 /* load */
#line 398 "Minus.m3"
 /* loophole */
#line 398 "Minus.m3"
 /* load */
#line 398 "Minus.m3"
 /* subtract */
#line 398 "Minus.m3"
 /* exit_proc */
#line 398 "Minus.m3"
return ((UINT64)(((UINT64)((INT64)((INT64)(*((INT16*)(INT64_(172)+((ADDRESS)(&Minus_m_M_Minus_L_13))))))))-((UINT64)(*((INT64*)(INT64_(112)+((ADDRESS)(&Minus_m_M_Minus_L_13))))))));
#line 398 "Minus.m3"
 /* end_procedure */
#line 398 "Minus.m3"
} /* Minus_var_i16_u64 */
#line 398 "Minus.m3"
 /* set_source_line */
#line 398 "Minus.m3"
#line 399 "Minus.m3"
 /* begin_procedure */
#line 399 "Minus.m3"
struct Minus__Minus_var_i16_u64_Frame_t {
#line 399 "Minus.m3"
ADDRESS _unused;
#line 399 "Minus.m3"
};
#line 399 "Minus.m3"
LONGINT
__cdecl
Minus__Minus_var_i16_u64(void)
{
#line 399 "Minus.m3"
Minus__Minus_var_i16_u64_Frame_t _frame;
#line 399 "Minus.m3"
_frame._unused=(ADDRESS)&_frame;
#line 399 "Minus.m3"
 /* load */
#line 399 "Minus.m3"
 /* loophole */
#line 399 "Minus.m3"
 /* load */
#line 399 "Minus.m3"
 /* subtract */
#line 399 "Minus.m3"
 /* exit_proc */
#line 399 "Minus.m3"
return ((INT64)(((INT64)((INT64)((INT64)(*((INT16*)(INT64_(172)+((ADDRESS)(&Minus_m_M_Minus_L_13))))))))-((INT64)(*((INT64*)(INT64_(112)+((ADDRESS)(&Minus_m_M_Minus_L_13))))))));
#line 399 "Minus.m3"
 /* end_procedure */
#line 399 "Minus.m3"
} /* uMinus_param_i16_u64 */
#line 399 "Minus.m3"
 /* set_source_line */
#line 399 "Minus.m3"
#line 400 "Minus.m3"
 /* begin_procedure */
#line 400 "Minus.m3"
struct Minus__uMinus_param_i16_u64_Frame_t {
#line 400 "Minus.m3"
ADDRESS _unused;
#line 400 "Minus.m3"
};
#line 400 "Minus.m3"
Long__T
__cdecl
Minus__uMinus_param_i16_u64(
   /* Param_Type1 */ Minus__INT16 a_L_707,
   /* Param_Type1 */ Minus__UINT64 b_L_708)
{
#line 400 "Minus.m3"
Minus__uMinus_param_i16_u64_Frame_t _frame;
#line 400 "Minus.m3"
_frame._unused=(ADDRESS)&_frame;
#line 400 "Minus.m3"
 /* load */
#line 400 "Minus.m3"
 /* loophole */
#line 400 "Minus.m3"
 /* load */
#line 400 "Minus.m3"
 /* subtract */
#line 400 "Minus.m3"
 /* exit_proc */
#line 400 "Minus.m3"
return ((UINT64)(((UINT64)((INT64)((INT64)(a_L_707))))-((UINT64)(b_L_708))));
#line 400 "Minus.m3"
 /* end_procedure */
#line 400 "Minus.m3"
} /* Minus_param_i16_u64 */
#line 400 "Minus.m3"
 /* set_source_line */
#line 400 "Minus.m3"
#line 401 "Minus.m3"
 /* begin_procedure */
#line 401 "Minus.m3"
struct Minus__Minus_param_i16_u64_Frame_t {
#line 401 "Minus.m3"
ADDRESS _unused;
#line 401 "Minus.m3"
};
#line 401 "Minus.m3"
LONGINT
__cdecl
Minus__Minus_param_i16_u64(
   /* Param_Type1 */ Minus__INT16 a_L_710,
   /* Param_Type1 */ Minus__UINT64 b_L_711)
{
#line 401 "Minus.m3"
Minus__Minus_param_i16_u64_Frame_t _frame;
#line 401 "Minus.m3"
_frame._unused=(ADDRESS)&_frame;
#line 401 "Minus.m3"
 /* load */
#line 401 "Minus.m3"
 /* loophole */
#line 401 "Minus.m3"
 /* load */
#line 401 "Minus.m3"
 /* subtract */
#line 401 "Minus.m3"
 /* exit_proc */
#line 401 "Minus.m3"
return ((INT64)(((INT64)((INT64)((INT64)(a_L_710))))- b_L_711));
#line 401 "Minus.m3"
 /* end_procedure */
#line 401 "Minus.m3"
} /* uMinus_var_i16_i32 */
#line 401 "Minus.m3"
 /* set_source_line */
#line 401 "Minus.m3"
#line 402 "Minus.m3"
 /* begin_procedure */
#line 402 "Minus.m3"
struct Minus__uMinus_var_i16_i32_Frame_t {
#line 402 "Minus.m3"
ADDRESS _unused;
#line 402 "Minus.m3"
};
#line 402 "Minus.m3"
Word__T
__cdecl
Minus__uMinus_var_i16_i32(void)
{
#line 402 "Minus.m3"
Minus__uMinus_var_i16_i32_Frame_t _frame;
#line 402 "Minus.m3"
_frame._unused=(ADDRESS)&_frame;
#line 402 "Minus.m3"
 /* load */
#line 402 "Minus.m3"
 /* load */
#line 402 "Minus.m3"
 /* subtract */
#line 402 "Minus.m3"
 /* exit_proc */
#line 402 "Minus.m3"
return ((UINT64)(((UINT64)(((INT64)(*((INT16*)(INT64_(172)+((ADDRESS)(&Minus_m_M_Minus_L_13))))))))-((UINT64)(((INT64)(*((INT32*)(INT64_(128)+((ADDRESS)(&Minus_m_M_Minus_L_13))))))))));
#line 402 "Minus.m3"
 /* end_procedure */
#line 402 "Minus.m3"
} /* Minus_var_i16_i32 */
#line 402 "Minus.m3"
 /* set_source_line */
#line 402 "Minus.m3"
#line 403 "Minus.m3"
 /* begin_procedure */
#line 403 "Minus.m3"
struct Minus__Minus_var_i16_i32_Frame_t {
#line 403 "Minus.m3"
ADDRESS _unused;
#line 403 "Minus.m3"
};
#line 403 "Minus.m3"
Minus__INT16
__cdecl
Minus__Minus_var_i16_i32(void)
{
#line 403 "Minus.m3"
 /* Var_Type2 */ INT64 Minus_m_1276_L_1277={0};//always-init
#line 403 "Minus.m3"
Minus__Minus_var_i16_i32_Frame_t _frame;
#line 403 "Minus.m3"
_frame._unused=(ADDRESS)&_frame;
#line 403 "Minus.m3"
 /* load */
#line 403 "Minus.m3"
 /* load */
#line 403 "Minus.m3"
 /* subtract */
#line 403 "Minus.m3"
 /* check_range */
#line 403 "Minus.m3"
 /* store */
#line 403 "Minus.m3"
(*(INT64*)(&Minus_m_1276_L_1277))=(INT64)( ((INT64)( ((INT64)(*((INT16*)(INT64_(172)+((ADDRESS)(&Minus_m_M_Minus_L_13))))))- ((INT64)(*((INT32*)(INT64_(128)+((ADDRESS)(&Minus_m_M_Minus_L_13)))))))));
#line 403 "Minus.m3"
 /* load */
#line 403 "Minus.m3"
if(m3_check_range(INT64,
Minus_m_1276_L_1277,
 INT64_(-32768),
 INT64_(32767)))
#line 403 "Minus.m3"
Minus_m_M_Minus_L_13_CRASH(12897);
#line 403 "Minus.m3"
 /* exit_proc */
#line 403 "Minus.m3"
return Minus_m_1276_L_1277;
#line 403 "Minus.m3"
 /* end_procedure */
#line 403 "Minus.m3"
} /* uMinus_param_i16_i32 */
#line 403 "Minus.m3"
 /* set_source_line */
#line 403 "Minus.m3"
#line 404 "Minus.m3"
 /* begin_procedure */
#line 404 "Minus.m3"
struct Minus__uMinus_param_i16_i32_Frame_t {
#line 404 "Minus.m3"
ADDRESS _unused;
#line 404 "Minus.m3"
};
#line 404 "Minus.m3"
Word__T
__cdecl
Minus__uMinus_param_i16_i32(
   /* Param_Type1 */ Minus__INT16 a_L_715,
   /* Param_Type1 */ Minus__INT32 b_L_716)
{
#line 404 "Minus.m3"
Minus__uMinus_param_i16_i32_Frame_t _frame;
#line 404 "Minus.m3"
_frame._unused=(ADDRESS)&_frame;
#line 404 "Minus.m3"
 /* load */
#line 404 "Minus.m3"
 /* load */
#line 404 "Minus.m3"
 /* subtract */
#line 404 "Minus.m3"
 /* exit_proc */
#line 404 "Minus.m3"
return ((UINT64)(((UINT64)(((INT64)(a_L_715))))-((UINT64)(((INT64)(b_L_716))))));
#line 404 "Minus.m3"
 /* end_procedure */
#line 404 "Minus.m3"
} /* Minus_param_i16_i32 */
#line 404 "Minus.m3"
 /* set_source_line */
#line 404 "Minus.m3"
#line 405 "Minus.m3"
 /* begin_procedure */
#line 405 "Minus.m3"
struct Minus__Minus_param_i16_i32_Frame_t {
#line 405 "Minus.m3"
ADDRESS _unused;
#line 405 "Minus.m3"
};
#line 405 "Minus.m3"
Minus__INT16
__cdecl
Minus__Minus_param_i16_i32(
   /* Param_Type1 */ Minus__INT16 a_L_718,
   /* Param_Type1 */ Minus__INT32 b_L_719)
{
#line 405 "Minus.m3"
 /* Var_Type2 */ INT64 Minus_m_1278_L_1279={0};//always-init
#line 405 "Minus.m3"
Minus__Minus_param_i16_i32_Frame_t _frame;
#line 405 "Minus.m3"
_frame._unused=(ADDRESS)&_frame;
#line 405 "Minus.m3"
 /* load */
#line 405 "Minus.m3"
 /* load */
#line 405 "Minus.m3"
 /* subtract */
#line 405 "Minus.m3"
 /* check_range */
#line 405 "Minus.m3"
 /* store */
#line 405 "Minus.m3"
(*(INT64*)(&Minus_m_1278_L_1279))=(INT64)( ((INT64)( ((INT64)(a_L_718))- ((INT64)(b_L_719)))));
#line 405 "Minus.m3"
 /* load */
#line 405 "Minus.m3"
if(m3_check_range(INT64,
Minus_m_1278_L_1279,
 INT64_(-32768),
 INT64_(32767)))
#line 405 "Minus.m3"
Minus_m_M_Minus_L_13_CRASH(12961);
#line 405 "Minus.m3"
 /* exit_proc */
#line 405 "Minus.m3"
return Minus_m_1278_L_1279;
#line 405 "Minus.m3"
 /* end_procedure */
#line 405 "Minus.m3"
} /* uMinus_var_i16_LC */
#line 405 "Minus.m3"
 /* set_source_line */
#line 405 "Minus.m3"
#line 406 "Minus.m3"
 /* begin_procedure */
#line 406 "Minus.m3"
struct Minus__uMinus_var_i16_LC_Frame_t {
#line 406 "Minus.m3"
ADDRESS _unused;
#line 406 "Minus.m3"
};
#line 406 "Minus.m3"
Long__T
__cdecl
Minus__uMinus_var_i16_LC(void)
{
#line 406 "Minus.m3"
Minus__uMinus_var_i16_LC_Frame_t _frame;
#line 406 "Minus.m3"
_frame._unused=(ADDRESS)&_frame;
#line 406 "Minus.m3"
 /* load */
#line 406 "Minus.m3"
 /* loophole */
#line 406 "Minus.m3"
 /* load */
#line 406 "Minus.m3"
 /* subtract */
#line 406 "Minus.m3"
 /* exit_proc */
#line 406 "Minus.m3"
return ((UINT64)(((UINT64)((INT64)((INT64)(*((INT16*)(INT64_(172)+((ADDRESS)(&Minus_m_M_Minus_L_13))))))))-((UINT64)(((INT64)(*((UINT64*)(INT64_(136)+((ADDRESS)(&Minus_m_M_Minus_L_13))))))))));
#line 406 "Minus.m3"
 /* end_procedure */
#line 406 "Minus.m3"
} /* Minus_var_i16_LC */
#line 406 "Minus.m3"
 /* set_source_line */
#line 406 "Minus.m3"
#line 407 "Minus.m3"
 /* begin_procedure */
#line 407 "Minus.m3"
struct Minus__Minus_var_i16_LC_Frame_t {
#line 407 "Minus.m3"
ADDRESS _unused;
#line 407 "Minus.m3"
};
#line 407 "Minus.m3"
LONGINT
__cdecl
Minus__Minus_var_i16_LC(void)
{
#line 407 "Minus.m3"
Minus__Minus_var_i16_LC_Frame_t _frame;
#line 407 "Minus.m3"
_frame._unused=(ADDRESS)&_frame;
#line 407 "Minus.m3"
 /* load */
#line 407 "Minus.m3"
 /* loophole */
#line 407 "Minus.m3"
 /* load */
#line 407 "Minus.m3"
 /* subtract */
#line 407 "Minus.m3"
 /* exit_proc */
#line 407 "Minus.m3"
return ((INT64)(((INT64)((INT64)((INT64)(*((INT16*)(INT64_(172)+((ADDRESS)(&Minus_m_M_Minus_L_13))))))))- ((INT64)(*((UINT64*)(INT64_(136)+((ADDRESS)(&Minus_m_M_Minus_L_13))))))));
#line 407 "Minus.m3"
 /* end_procedure */
#line 407 "Minus.m3"
} /* uMinus_param_i16_LC */
#line 407 "Minus.m3"
 /* set_source_line */
#line 407 "Minus.m3"
#line 408 "Minus.m3"
 /* begin_procedure */
#line 408 "Minus.m3"
struct Minus__uMinus_param_i16_LC_Frame_t {
#line 408 "Minus.m3"
ADDRESS _unused;
#line 408 "Minus.m3"
};
#line 408 "Minus.m3"
Long__T
__cdecl
Minus__uMinus_param_i16_LC(
   /* Param_Type1 */ Minus__INT16 a_L_723,
   /* Param_Type1 */ LONGCARD b_L_724)
{
#line 408 "Minus.m3"
Minus__uMinus_param_i16_LC_Frame_t _frame;
#line 408 "Minus.m3"
_frame._unused=(ADDRESS)&_frame;
#line 408 "Minus.m3"
 /* load */
#line 408 "Minus.m3"
 /* loophole */
#line 408 "Minus.m3"
 /* load */
#line 408 "Minus.m3"
 /* subtract */
#line 408 "Minus.m3"
 /* exit_proc */
#line 408 "Minus.m3"
return ((UINT64)(((UINT64)((INT64)((INT64)(a_L_723))))-((UINT64)(((INT64)(b_L_724))))));
#line 408 "Minus.m3"
 /* end_procedure */
#line 408 "Minus.m3"
} /* Minus_param_i16_LC */
#line 408 "Minus.m3"
 /* set_source_line */
#line 408 "Minus.m3"
#line 409 "Minus.m3"
 /* begin_procedure */
#line 409 "Minus.m3"
struct Minus__Minus_param_i16_LC_Frame_t {
#line 409 "Minus.m3"
ADDRESS _unused;
#line 409 "Minus.m3"
};
#line 409 "Minus.m3"
LONGINT
__cdecl
Minus__Minus_param_i16_LC(
   /* Param_Type1 */ Minus__INT16 a_L_726,
   /* Param_Type1 */ LONGCARD b_L_727)
{
#line 409 "Minus.m3"
Minus__Minus_param_i16_LC_Frame_t _frame;
#line 409 "Minus.m3"
_frame._unused=(ADDRESS)&_frame;
#line 409 "Minus.m3"
 /* load */
#line 409 "Minus.m3"
 /* loophole */
#line 409 "Minus.m3"
 /* load */
#line 409 "Minus.m3"
 /* subtract */
#line 409 "Minus.m3"
 /* exit_proc */
#line 409 "Minus.m3"
return ((INT64)(((INT64)((INT64)((INT64)(a_L_726))))- ((INT64)(b_L_727))));
#line 409 "Minus.m3"
 /* end_procedure */
#line 409 "Minus.m3"
} /* uMinus_var_i16_u16 */
#line 409 "Minus.m3"
 /* set_source_line */
#line 409 "Minus.m3"
#line 410 "Minus.m3"
 /* begin_procedure */
#line 410 "Minus.m3"
struct Minus__uMinus_var_i16_u16_Frame_t {
#line 410 "Minus.m3"
ADDRESS _unused;
#line 410 "Minus.m3"
};
#line 410 "Minus.m3"
Word__T
__cdecl
Minus__uMinus_var_i16_u16(void)
{
#line 410 "Minus.m3"
Minus__uMinus_var_i16_u16_Frame_t _frame;
#line 410 "Minus.m3"
_frame._unused=(ADDRESS)&_frame;
#line 410 "Minus.m3"
 /* load */
#line 410 "Minus.m3"
 /* load */
#line 410 "Minus.m3"
 /* subtract */
#line 410 "Minus.m3"
 /* exit_proc */
#line 410 "Minus.m3"
return ((UINT64)(((UINT64)(((INT64)(*((INT16*)(INT64_(172)+((ADDRESS)(&Minus_m_M_Minus_L_13))))))))-((UINT64)(((INT64)(*((UINT16*)(INT64_(144)+((ADDRESS)(&Minus_m_M_Minus_L_13))))))))));
#line 410 "Minus.m3"
 /* end_procedure */
#line 410 "Minus.m3"
} /* Minus_var_i16_u16 */
#line 410 "Minus.m3"
 /* set_source_line */
#line 410 "Minus.m3"
#line 411 "Minus.m3"
 /* begin_procedure */
#line 411 "Minus.m3"
struct Minus__Minus_var_i16_u16_Frame_t {
#line 411 "Minus.m3"
ADDRESS _unused;
#line 411 "Minus.m3"
};
#line 411 "Minus.m3"
Minus__INT16
__cdecl
Minus__Minus_var_i16_u16(void)
{
#line 411 "Minus.m3"
 /* Var_Type2 */ INT64 Minus_m_1280_L_1281={0};//always-init
#line 411 "Minus.m3"
Minus__Minus_var_i16_u16_Frame_t _frame;
#line 411 "Minus.m3"
_frame._unused=(ADDRESS)&_frame;
#line 411 "Minus.m3"
 /* load */
#line 411 "Minus.m3"
 /* load */
#line 411 "Minus.m3"
 /* subtract */
#line 411 "Minus.m3"
 /* check_lo */
#line 411 "Minus.m3"
 /* store */
#line 411 "Minus.m3"
(*(INT64*)(&Minus_m_1280_L_1281))=(INT64)( ((INT64)( ((INT64)(*((INT16*)(INT64_(172)+((ADDRESS)(&Minus_m_M_Minus_L_13))))))- ((INT64)(*((UINT16*)(INT64_(144)+((ADDRESS)(&Minus_m_M_Minus_L_13)))))))));
#line 411 "Minus.m3"
 /* load */
#line 411 "Minus.m3"
/*check_lo*/if(Minus_m_1280_L_1281<INT64_(-32768))Minus_m_M_Minus_L_13_CRASH(13153);
#line 411 "Minus.m3"
 /* exit_proc */
#line 411 "Minus.m3"
return Minus_m_1280_L_1281;
#line 411 "Minus.m3"
 /* end_procedure */
#line 411 "Minus.m3"
} /* uMinus_param_i16_u16 */
#line 411 "Minus.m3"
 /* set_source_line */
#line 411 "Minus.m3"
#line 412 "Minus.m3"
 /* begin_procedure */
#line 412 "Minus.m3"
struct Minus__uMinus_param_i16_u16_Frame_t {
#line 412 "Minus.m3"
ADDRESS _unused;
#line 412 "Minus.m3"
};
#line 412 "Minus.m3"
Word__T
__cdecl
Minus__uMinus_param_i16_u16(
   /* Param_Type1 */ Minus__INT16 a_L_731,
   /* Param_Type1 */ Minus__UINT16 b_L_732)
{
#line 412 "Minus.m3"
Minus__uMinus_param_i16_u16_Frame_t _frame;
#line 412 "Minus.m3"
_frame._unused=(ADDRESS)&_frame;
#line 412 "Minus.m3"
 /* load */
#line 412 "Minus.m3"
 /* load */
#line 412 "Minus.m3"
 /* subtract */
#line 412 "Minus.m3"
 /* exit_proc */
#line 412 "Minus.m3"
return ((UINT64)(((UINT64)(((INT64)(a_L_731))))-((UINT64)(((INT64)(b_L_732))))));
#line 412 "Minus.m3"
 /* end_procedure */
#line 412 "Minus.m3"
} /* Minus_param_i16_u16 */
#line 412 "Minus.m3"
 /* set_source_line */
#line 412 "Minus.m3"
#line 413 "Minus.m3"
 /* begin_procedure */
#line 413 "Minus.m3"
struct Minus__Minus_param_i16_u16_Frame_t {
#line 413 "Minus.m3"
ADDRESS _unused;
#line 413 "Minus.m3"
};
#line 413 "Minus.m3"
Minus__INT16
__cdecl
Minus__Minus_param_i16_u16(
   /* Param_Type1 */ Minus__INT16 a_L_734,
   /* Param_Type1 */ Minus__UINT16 b_L_735)
{
#line 413 "Minus.m3"
 /* Var_Type2 */ INT64 Minus_m_1282_L_1283={0};//always-init
#line 413 "Minus.m3"
Minus__Minus_param_i16_u16_Frame_t _frame;
#line 413 "Minus.m3"
_frame._unused=(ADDRESS)&_frame;
#line 413 "Minus.m3"
 /* load */
#line 413 "Minus.m3"
 /* load */
#line 413 "Minus.m3"
 /* subtract */
#line 413 "Minus.m3"
 /* check_lo */
#line 413 "Minus.m3"
 /* store */
#line 413 "Minus.m3"
(*(INT64*)(&Minus_m_1282_L_1283))=(INT64)( ((INT64)( ((INT64)(a_L_734))- ((INT64)(b_L_735)))));
#line 413 "Minus.m3"
 /* load */
#line 413 "Minus.m3"
/*check_lo*/if(Minus_m_1282_L_1283<INT64_(-32768))Minus_m_M_Minus_L_13_CRASH(13217);
#line 413 "Minus.m3"
 /* exit_proc */
#line 413 "Minus.m3"
return Minus_m_1282_L_1283;
#line 413 "Minus.m3"
 /* end_procedure */
#line 413 "Minus.m3"
} /* uMinus_var_i16_I */
#line 413 "Minus.m3"
 /* set_source_line */
#line 413 "Minus.m3"
#line 414 "Minus.m3"
 /* begin_procedure */
#line 414 "Minus.m3"
struct Minus__uMinus_var_i16_I_Frame_t {
#line 414 "Minus.m3"
ADDRESS _unused;
#line 414 "Minus.m3"
};
#line 414 "Minus.m3"
Word__T
__cdecl
Minus__uMinus_var_i16_I(void)
{
#line 414 "Minus.m3"
Minus__uMinus_var_i16_I_Frame_t _frame;
#line 414 "Minus.m3"
_frame._unused=(ADDRESS)&_frame;
#line 414 "Minus.m3"
 /* load */
#line 414 "Minus.m3"
 /* load */
#line 414 "Minus.m3"
 /* subtract */
#line 414 "Minus.m3"
 /* exit_proc */
#line 414 "Minus.m3"
return ((UINT64)(((UINT64)(((INT64)(*((INT16*)(INT64_(172)+((ADDRESS)(&Minus_m_M_Minus_L_13))))))))-((UINT64)(*((INT64*)(INT64_(152)+((ADDRESS)(&Minus_m_M_Minus_L_13))))))));
#line 414 "Minus.m3"
 /* end_procedure */
#line 414 "Minus.m3"
} /* Minus_var_i16_I */
#line 414 "Minus.m3"
 /* set_source_line */
#line 414 "Minus.m3"
#line 415 "Minus.m3"
 /* begin_procedure */
#line 415 "Minus.m3"
struct Minus__Minus_var_i16_I_Frame_t {
#line 415 "Minus.m3"
ADDRESS _unused;
#line 415 "Minus.m3"
};
#line 415 "Minus.m3"
Minus__INT16
__cdecl
Minus__Minus_var_i16_I(void)
{
#line 415 "Minus.m3"
 /* Var_Type2 */ INT64 Minus_m_1284_L_1285={0};//always-init
#line 415 "Minus.m3"
Minus__Minus_var_i16_I_Frame_t _frame;
#line 415 "Minus.m3"
_frame._unused=(ADDRESS)&_frame;
#line 415 "Minus.m3"
 /* load */
#line 415 "Minus.m3"
 /* load */
#line 415 "Minus.m3"
 /* subtract */
#line 415 "Minus.m3"
 /* check_range */
#line 415 "Minus.m3"
 /* store */
#line 415 "Minus.m3"
(*(INT64*)(&Minus_m_1284_L_1285))=(INT64)( ((INT64)( ((INT64)(*((INT16*)(INT64_(172)+((ADDRESS)(&Minus_m_M_Minus_L_13))))))-((INT64)(*((INT64*)(INT64_(152)+((ADDRESS)(&Minus_m_M_Minus_L_13)))))))));
#line 415 "Minus.m3"
 /* load */
#line 415 "Minus.m3"
if(m3_check_range(INT64,
Minus_m_1284_L_1285,
 INT64_(-32768),
 INT64_(32767)))
#line 415 "Minus.m3"
Minus_m_M_Minus_L_13_CRASH(13281);
#line 415 "Minus.m3"
 /* exit_proc */
#line 415 "Minus.m3"
return Minus_m_1284_L_1285;
#line 415 "Minus.m3"
 /* end_procedure */
#line 415 "Minus.m3"
} /* uMinus_param_i16_I */
#line 415 "Minus.m3"
 /* set_source_line */
#line 415 "Minus.m3"
#line 416 "Minus.m3"
 /* begin_procedure */
#line 416 "Minus.m3"
struct Minus__uMinus_param_i16_I_Frame_t {
#line 416 "Minus.m3"
ADDRESS _unused;
#line 416 "Minus.m3"
};
#line 416 "Minus.m3"
Word__T
__cdecl
Minus__uMinus_param_i16_I(
   /* Param_Type1 */ Minus__INT16 a_L_739,
   /* Param_Type1 */ INTEGER b_L_740)
{
#line 416 "Minus.m3"
Minus__uMinus_param_i16_I_Frame_t _frame;
#line 416 "Minus.m3"
_frame._unused=(ADDRESS)&_frame;
#line 416 "Minus.m3"
 /* load */
#line 416 "Minus.m3"
 /* load */
#line 416 "Minus.m3"
 /* subtract */
#line 416 "Minus.m3"
 /* exit_proc */
#line 416 "Minus.m3"
return ((UINT64)(((UINT64)(((INT64)(a_L_739))))-((UINT64)(b_L_740))));
#line 416 "Minus.m3"
 /* end_procedure */
#line 416 "Minus.m3"
} /* Minus_param_i16_I */
#line 416 "Minus.m3"
 /* set_source_line */
#line 416 "Minus.m3"
#line 417 "Minus.m3"
 /* begin_procedure */
#line 417 "Minus.m3"
struct Minus__Minus_param_i16_I_Frame_t {
#line 417 "Minus.m3"
ADDRESS _unused;
#line 417 "Minus.m3"
};
#line 417 "Minus.m3"
Minus__INT16
__cdecl
Minus__Minus_param_i16_I(
   /* Param_Type1 */ Minus__INT16 a_L_742,
   /* Param_Type1 */ INTEGER b_L_743)
{
#line 417 "Minus.m3"
 /* Var_Type2 */ INT64 Minus_m_1286_L_1287={0};//always-init
#line 417 "Minus.m3"
Minus__Minus_param_i16_I_Frame_t _frame;
#line 417 "Minus.m3"
_frame._unused=(ADDRESS)&_frame;
#line 417 "Minus.m3"
 /* load */
#line 417 "Minus.m3"
 /* load */
#line 417 "Minus.m3"
 /* subtract */
#line 417 "Minus.m3"
 /* check_range */
#line 417 "Minus.m3"
 /* store */
#line 417 "Minus.m3"
(*(INT64*)(&Minus_m_1286_L_1287))=(INT64)( ((INT64)( ((INT64)(a_L_742))- b_L_743)));
#line 417 "Minus.m3"
 /* load */
#line 417 "Minus.m3"
if(m3_check_range(INT64,
Minus_m_1286_L_1287,
 INT64_(-32768),
 INT64_(32767)))
#line 417 "Minus.m3"
Minus_m_M_Minus_L_13_CRASH(13345);
#line 417 "Minus.m3"
 /* exit_proc */
#line 417 "Minus.m3"
return Minus_m_1286_L_1287;
#line 417 "Minus.m3"
 /* end_procedure */
#line 417 "Minus.m3"
} /* uMinus_var_i16_i64 */
#line 417 "Minus.m3"
 /* set_source_line */
#line 417 "Minus.m3"
#line 418 "Minus.m3"
 /* begin_procedure */
#line 418 "Minus.m3"
struct Minus__uMinus_var_i16_i64_Frame_t {
#line 418 "Minus.m3"
ADDRESS _unused;
#line 418 "Minus.m3"
};
#line 418 "Minus.m3"
Long__T
__cdecl
Minus__uMinus_var_i16_i64(void)
{
#line 418 "Minus.m3"
Minus__uMinus_var_i16_i64_Frame_t _frame;
#line 418 "Minus.m3"
_frame._unused=(ADDRESS)&_frame;
#line 418 "Minus.m3"
 /* load */
#line 418 "Minus.m3"
 /* loophole */
#line 418 "Minus.m3"
 /* load */
#line 418 "Minus.m3"
 /* subtract */
#line 418 "Minus.m3"
 /* exit_proc */
#line 418 "Minus.m3"
return ((UINT64)(((UINT64)((INT64)((INT64)(*((INT16*)(INT64_(172)+((ADDRESS)(&Minus_m_M_Minus_L_13))))))))-((UINT64)(*((INT64*)(INT64_(160)+((ADDRESS)(&Minus_m_M_Minus_L_13))))))));
#line 418 "Minus.m3"
 /* end_procedure */
#line 418 "Minus.m3"
} /* Minus_var_i16_i64 */
#line 418 "Minus.m3"
 /* set_source_line */
#line 418 "Minus.m3"
#line 419 "Minus.m3"
 /* begin_procedure */
#line 419 "Minus.m3"
struct Minus__Minus_var_i16_i64_Frame_t {
#line 419 "Minus.m3"
ADDRESS _unused;
#line 419 "Minus.m3"
};
#line 419 "Minus.m3"
LONGINT
__cdecl
Minus__Minus_var_i16_i64(void)
{
#line 419 "Minus.m3"
Minus__Minus_var_i16_i64_Frame_t _frame;
#line 419 "Minus.m3"
_frame._unused=(ADDRESS)&_frame;
#line 419 "Minus.m3"
 /* load */
#line 419 "Minus.m3"
 /* loophole */
#line 419 "Minus.m3"
 /* load */
#line 419 "Minus.m3"
 /* subtract */
#line 419 "Minus.m3"
 /* exit_proc */
#line 419 "Minus.m3"
return ((INT64)(((INT64)((INT64)((INT64)(*((INT16*)(INT64_(172)+((ADDRESS)(&Minus_m_M_Minus_L_13))))))))-((INT64)(*((INT64*)(INT64_(160)+((ADDRESS)(&Minus_m_M_Minus_L_13))))))));
#line 419 "Minus.m3"
 /* end_procedure */
#line 419 "Minus.m3"
} /* uMinus_param_i16_i64 */
#line 419 "Minus.m3"
 /* set_source_line */
#line 419 "Minus.m3"
#line 420 "Minus.m3"
 /* begin_procedure */
#line 420 "Minus.m3"
struct Minus__uMinus_param_i16_i64_Frame_t {
#line 420 "Minus.m3"
ADDRESS _unused;
#line 420 "Minus.m3"
};
#line 420 "Minus.m3"
Long__T
__cdecl
Minus__uMinus_param_i16_i64(
   /* Param_Type1 */ Minus__INT16 a_L_747,
   /* Param_Type1 */ Minus__INT64 b_L_748)
{
#line 420 "Minus.m3"
Minus__uMinus_param_i16_i64_Frame_t _frame;
#line 420 "Minus.m3"
_frame._unused=(ADDRESS)&_frame;
#line 420 "Minus.m3"
 /* load */
#line 420 "Minus.m3"
 /* loophole */
#line 420 "Minus.m3"
 /* load */
#line 420 "Minus.m3"
 /* subtract */
#line 420 "Minus.m3"
 /* exit_proc */
#line 420 "Minus.m3"
return ((UINT64)(((UINT64)((INT64)((INT64)(a_L_747))))-((UINT64)(b_L_748))));
#line 420 "Minus.m3"
 /* end_procedure */
#line 420 "Minus.m3"
} /* Minus_param_i16_i64 */
#line 420 "Minus.m3"
 /* set_source_line */
#line 420 "Minus.m3"
#line 421 "Minus.m3"
 /* begin_procedure */
#line 421 "Minus.m3"
struct Minus__Minus_param_i16_i64_Frame_t {
#line 421 "Minus.m3"
ADDRESS _unused;
#line 421 "Minus.m3"
};
#line 421 "Minus.m3"
LONGINT
__cdecl
Minus__Minus_param_i16_i64(
   /* Param_Type1 */ Minus__INT16 a_L_750,
   /* Param_Type1 */ Minus__INT64 b_L_751)
{
#line 421 "Minus.m3"
Minus__Minus_param_i16_i64_Frame_t _frame;
#line 421 "Minus.m3"
_frame._unused=(ADDRESS)&_frame;
#line 421 "Minus.m3"
 /* load */
#line 421 "Minus.m3"
 /* loophole */
#line 421 "Minus.m3"
 /* load */
#line 421 "Minus.m3"
 /* subtract */
#line 421 "Minus.m3"
 /* exit_proc */
#line 421 "Minus.m3"
return ((INT64)(((INT64)((INT64)((INT64)(a_L_750))))- b_L_751));
#line 421 "Minus.m3"
 /* end_procedure */
#line 421 "Minus.m3"
} /* uMinus_var_i16_i16 */
#line 421 "Minus.m3"
 /* set_source_line */
#line 421 "Minus.m3"
#line 422 "Minus.m3"
 /* begin_procedure */
#line 422 "Minus.m3"
struct Minus__uMinus_var_i16_i16_Frame_t {
#line 422 "Minus.m3"
ADDRESS _unused;
#line 422 "Minus.m3"
};
#line 422 "Minus.m3"
Word__T
__cdecl
Minus__uMinus_var_i16_i16(void)
{
#line 422 "Minus.m3"
Minus__uMinus_var_i16_i16_Frame_t _frame;
#line 422 "Minus.m3"
_frame._unused=(ADDRESS)&_frame;
#line 422 "Minus.m3"
 /* load */
#line 422 "Minus.m3"
 /* load */
#line 422 "Minus.m3"
 /* subtract */
#line 422 "Minus.m3"
 /* exit_proc */
#line 422 "Minus.m3"
return ((UINT64)(((UINT64)(((INT64)(*((INT16*)(INT64_(172)+((ADDRESS)(&Minus_m_M_Minus_L_13))))))))-((UINT64)(((INT64)(*((INT16*)(INT64_(172)+((ADDRESS)(&Minus_m_M_Minus_L_13))))))))));
#line 422 "Minus.m3"
 /* end_procedure */
#line 422 "Minus.m3"
} /* Minus_var_i16_i16 */
#line 422 "Minus.m3"
 /* set_source_line */
#line 422 "Minus.m3"
#line 423 "Minus.m3"
 /* begin_procedure */
#line 423 "Minus.m3"
struct Minus__Minus_var_i16_i16_Frame_t {
#line 423 "Minus.m3"
ADDRESS _unused;
#line 423 "Minus.m3"
};
#line 423 "Minus.m3"
Minus__INT16
__cdecl
Minus__Minus_var_i16_i16(void)
{
#line 423 "Minus.m3"
 /* Var_Type2 */ INT64 Minus_m_1288_L_1289={0};//always-init
#line 423 "Minus.m3"
Minus__Minus_var_i16_i16_Frame_t _frame;
#line 423 "Minus.m3"
_frame._unused=(ADDRESS)&_frame;
#line 423 "Minus.m3"
 /* load */
#line 423 "Minus.m3"
 /* load */
#line 423 "Minus.m3"
 /* subtract */
#line 423 "Minus.m3"
 /* check_range */
#line 423 "Minus.m3"
 /* store */
#line 423 "Minus.m3"
(*(INT64*)(&Minus_m_1288_L_1289))=(INT64)( ((INT64)( ((INT64)(*((INT16*)(INT64_(172)+((ADDRESS)(&Minus_m_M_Minus_L_13))))))- ((INT64)(*((INT16*)(INT64_(172)+((ADDRESS)(&Minus_m_M_Minus_L_13)))))))));
#line 423 "Minus.m3"
 /* load */
#line 423 "Minus.m3"
if(m3_check_range(INT64,
Minus_m_1288_L_1289,
 INT64_(-32768),
 INT64_(32767)))
#line 423 "Minus.m3"
Minus_m_M_Minus_L_13_CRASH(13537);
#line 423 "Minus.m3"
 /* exit_proc */
#line 423 "Minus.m3"
return Minus_m_1288_L_1289;
#line 423 "Minus.m3"
 /* end_procedure */
#line 423 "Minus.m3"
} /* uMinus_param_i16_i16 */
#line 423 "Minus.m3"
 /* set_source_line */
#line 423 "Minus.m3"
#line 424 "Minus.m3"
 /* begin_procedure */
#line 424 "Minus.m3"
struct Minus__uMinus_param_i16_i16_Frame_t {
#line 424 "Minus.m3"
ADDRESS _unused;
#line 424 "Minus.m3"
};
#line 424 "Minus.m3"
Word__T
__cdecl
Minus__uMinus_param_i16_i16(
   /* Param_Type1 */ Minus__INT16 a_L_755,
   /* Param_Type1 */ Minus__INT16 b_L_756)
{
#line 424 "Minus.m3"
Minus__uMinus_param_i16_i16_Frame_t _frame;
#line 424 "Minus.m3"
_frame._unused=(ADDRESS)&_frame;
#line 424 "Minus.m3"
 /* load */
#line 424 "Minus.m3"
 /* load */
#line 424 "Minus.m3"
 /* subtract */
#line 424 "Minus.m3"
 /* exit_proc */
#line 424 "Minus.m3"
return ((UINT64)(((UINT64)(((INT64)(a_L_755))))-((UINT64)(((INT64)(b_L_756))))));
#line 424 "Minus.m3"
 /* end_procedure */
#line 424 "Minus.m3"
} /* Minus_param_i16_i16 */
#line 424 "Minus.m3"
 /* set_source_line */
#line 424 "Minus.m3"
#line 425 "Minus.m3"
 /* begin_procedure */
#line 425 "Minus.m3"
struct Minus__Minus_param_i16_i16_Frame_t {
#line 425 "Minus.m3"
ADDRESS _unused;
#line 425 "Minus.m3"
};
#line 425 "Minus.m3"
Minus__INT16
__cdecl
Minus__Minus_param_i16_i16(
   /* Param_Type1 */ Minus__INT16 a_L_758,
   /* Param_Type1 */ Minus__INT16 b_L_759)
{
#line 425 "Minus.m3"
 /* Var_Type2 */ INT64 Minus_m_1290_L_1291={0};//always-init
#line 425 "Minus.m3"
Minus__Minus_param_i16_i16_Frame_t _frame;
#line 425 "Minus.m3"
_frame._unused=(ADDRESS)&_frame;
#line 425 "Minus.m3"
 /* load */
#line 425 "Minus.m3"
 /* load */
#line 425 "Minus.m3"
 /* subtract */
#line 425 "Minus.m3"
 /* check_range */
#line 425 "Minus.m3"
 /* store */
#line 425 "Minus.m3"
(*(INT64*)(&Minus_m_1290_L_1291))=(INT64)( ((INT64)( ((INT64)(a_L_758))- ((INT64)(b_L_759)))));
#line 425 "Minus.m3"
 /* load */
#line 425 "Minus.m3"
if(m3_check_range(INT64,
Minus_m_1290_L_1291,
 INT64_(-32768),
 INT64_(32767)))
#line 425 "Minus.m3"
Minus_m_M_Minus_L_13_CRASH(13601);
#line 425 "Minus.m3"
 /* exit_proc */
#line 425 "Minus.m3"
return Minus_m_1290_L_1291;
#line 425 "Minus.m3"
 /* end_procedure */
#line 425 "Minus.m3"
} /* uMinus_var_i16_C */
#line 425 "Minus.m3"
 /* set_source_line */
#line 425 "Minus.m3"
#line 426 "Minus.m3"
 /* begin_procedure */
#line 426 "Minus.m3"
struct Minus__uMinus_var_i16_C_Frame_t {
#line 426 "Minus.m3"
ADDRESS _unused;
#line 426 "Minus.m3"
};
#line 426 "Minus.m3"
Word__T
__cdecl
Minus__uMinus_var_i16_C(void)
{
#line 426 "Minus.m3"
Minus__uMinus_var_i16_C_Frame_t _frame;
#line 426 "Minus.m3"
_frame._unused=(ADDRESS)&_frame;
#line 426 "Minus.m3"
 /* load */
#line 426 "Minus.m3"
 /* load */
#line 426 "Minus.m3"
 /* subtract */
#line 426 "Minus.m3"
 /* exit_proc */
#line 426 "Minus.m3"
return ((UINT64)(((UINT64)(((INT64)(*((INT16*)(INT64_(172)+((ADDRESS)(&Minus_m_M_Minus_L_13))))))))-((UINT64)(((INT64)(*((UINT64*)(INT64_(176)+((ADDRESS)(&Minus_m_M_Minus_L_13))))))))));
#line 426 "Minus.m3"
 /* end_procedure */
#line 426 "Minus.m3"
} /* Minus_var_i16_C */
#line 426 "Minus.m3"
 /* set_source_line */
#line 426 "Minus.m3"
#line 427 "Minus.m3"
 /* begin_procedure */
#line 427 "Minus.m3"
struct Minus__Minus_var_i16_C_Frame_t {
#line 427 "Minus.m3"
ADDRESS _unused;
#line 427 "Minus.m3"
};
#line 427 "Minus.m3"
Minus__INT16
__cdecl
Minus__Minus_var_i16_C(void)
{
#line 427 "Minus.m3"
 /* Var_Type2 */ INT64 Minus_m_1292_L_1293={0};//always-init
#line 427 "Minus.m3"
Minus__Minus_var_i16_C_Frame_t _frame;
#line 427 "Minus.m3"
_frame._unused=(ADDRESS)&_frame;
#line 427 "Minus.m3"
 /* load */
#line 427 "Minus.m3"
 /* load */
#line 427 "Minus.m3"
 /* subtract */
#line 427 "Minus.m3"
 /* check_range */
#line 427 "Minus.m3"
 /* store */
#line 427 "Minus.m3"
(*(INT64*)(&Minus_m_1292_L_1293))=(INT64)( ((INT64)( ((INT64)(*((INT16*)(INT64_(172)+((ADDRESS)(&Minus_m_M_Minus_L_13))))))- ((INT64)(*((UINT64*)(INT64_(176)+((ADDRESS)(&Minus_m_M_Minus_L_13)))))))));
#line 427 "Minus.m3"
 /* load */
#line 427 "Minus.m3"
if(m3_check_range(INT64,
Minus_m_1292_L_1293,
 INT64_(-32768),
 INT64_(32767)))
#line 427 "Minus.m3"
Minus_m_M_Minus_L_13_CRASH(13665);
#line 427 "Minus.m3"
 /* exit_proc */
#line 427 "Minus.m3"
return Minus_m_1292_L_1293;
#line 427 "Minus.m3"
 /* end_procedure */
#line 427 "Minus.m3"
} /* uMinus_param_i16_C */
#line 427 "Minus.m3"
 /* set_source_line */
#line 427 "Minus.m3"
#line 428 "Minus.m3"
 /* begin_procedure */
#line 428 "Minus.m3"
struct Minus__uMinus_param_i16_C_Frame_t {
#line 428 "Minus.m3"
ADDRESS _unused;
#line 428 "Minus.m3"
};
#line 428 "Minus.m3"
Word__T
__cdecl
Minus__uMinus_param_i16_C(
   /* Param_Type1 */ Minus__INT16 a_L_763,
   /* Param_Type1 */ CARDINAL b_L_764)
{
#line 428 "Minus.m3"
Minus__uMinus_param_i16_C_Frame_t _frame;
#line 428 "Minus.m3"
_frame._unused=(ADDRESS)&_frame;
#line 428 "Minus.m3"
 /* load */
#line 428 "Minus.m3"
 /* load */
#line 428 "Minus.m3"
 /* subtract */
#line 428 "Minus.m3"
 /* exit_proc */
#line 428 "Minus.m3"
return ((UINT64)(((UINT64)(((INT64)(a_L_763))))-((UINT64)(((INT64)(b_L_764))))));
#line 428 "Minus.m3"
 /* end_procedure */
#line 428 "Minus.m3"
} /* Minus_param_i16_C */
#line 428 "Minus.m3"
 /* set_source_line */
#line 428 "Minus.m3"
#line 429 "Minus.m3"
 /* begin_procedure */
#line 429 "Minus.m3"
struct Minus__Minus_param_i16_C_Frame_t {
#line 429 "Minus.m3"
ADDRESS _unused;
#line 429 "Minus.m3"
};
#line 429 "Minus.m3"
Minus__INT16
__cdecl
Minus__Minus_param_i16_C(
   /* Param_Type1 */ Minus__INT16 a_L_766,
   /* Param_Type1 */ CARDINAL b_L_767)
{
#line 429 "Minus.m3"
 /* Var_Type2 */ INT64 Minus_m_1294_L_1295={0};//always-init
#line 429 "Minus.m3"
Minus__Minus_param_i16_C_Frame_t _frame;
#line 429 "Minus.m3"
_frame._unused=(ADDRESS)&_frame;
#line 429 "Minus.m3"
 /* load */
#line 429 "Minus.m3"
 /* load */
#line 429 "Minus.m3"
 /* subtract */
#line 429 "Minus.m3"
 /* check_range */
#line 429 "Minus.m3"
 /* store */
#line 429 "Minus.m3"
(*(INT64*)(&Minus_m_1294_L_1295))=(INT64)( ((INT64)( ((INT64)(a_L_766))- ((INT64)(b_L_767)))));
#line 429 "Minus.m3"
 /* load */
#line 429 "Minus.m3"
if(m3_check_range(INT64,
Minus_m_1294_L_1295,
 INT64_(-32768),
 INT64_(32767)))
#line 429 "Minus.m3"
Minus_m_M_Minus_L_13_CRASH(13729);
#line 429 "Minus.m3"
 /* exit_proc */
#line 429 "Minus.m3"
return Minus_m_1294_L_1295;
#line 429 "Minus.m3"
 /* end_procedure */
#line 429 "Minus.m3"
} /* uMinus_var_i16_u32 */
#line 429 "Minus.m3"
 /* set_source_line */
#line 429 "Minus.m3"
#line 430 "Minus.m3"
 /* begin_procedure */
#line 430 "Minus.m3"
struct Minus__uMinus_var_i16_u32_Frame_t {
#line 430 "Minus.m3"
ADDRESS _unused;
#line 430 "Minus.m3"
};
#line 430 "Minus.m3"
Word__T
__cdecl
Minus__uMinus_var_i16_u32(void)
{
#line 430 "Minus.m3"
Minus__uMinus_var_i16_u32_Frame_t _frame;
#line 430 "Minus.m3"
_frame._unused=(ADDRESS)&_frame;
#line 430 "Minus.m3"
 /* load */
#line 430 "Minus.m3"
 /* load */
#line 430 "Minus.m3"
 /* subtract */
#line 430 "Minus.m3"
 /* exit_proc */
#line 430 "Minus.m3"
return ((UINT64)(((UINT64)(((INT64)(*((INT16*)(INT64_(172)+((ADDRESS)(&Minus_m_M_Minus_L_13))))))))-((UINT64)(((INT64)(*((UINT32*)(INT64_(184)+((ADDRESS)(&Minus_m_M_Minus_L_13))))))))));
#line 430 "Minus.m3"
 /* end_procedure */
#line 430 "Minus.m3"
} /* Minus_var_i16_u32 */
#line 430 "Minus.m3"
 /* set_source_line */
#line 430 "Minus.m3"
#line 431 "Minus.m3"
 /* begin_procedure */
#line 431 "Minus.m3"
struct Minus__Minus_var_i16_u32_Frame_t {
#line 431 "Minus.m3"
ADDRESS _unused;
#line 431 "Minus.m3"
};
#line 431 "Minus.m3"
Minus__INT16
__cdecl
Minus__Minus_var_i16_u32(void)
{
#line 431 "Minus.m3"
 /* Var_Type2 */ INT64 Minus_m_1296_L_1297={0};//always-init
#line 431 "Minus.m3"
Minus__Minus_var_i16_u32_Frame_t _frame;
#line 431 "Minus.m3"
_frame._unused=(ADDRESS)&_frame;
#line 431 "Minus.m3"
 /* load */
#line 431 "Minus.m3"
 /* load */
#line 431 "Minus.m3"
 /* subtract */
#line 431 "Minus.m3"
 /* check_lo */
#line 431 "Minus.m3"
 /* store */
#line 431 "Minus.m3"
(*(INT64*)(&Minus_m_1296_L_1297))=(INT64)( ((INT64)( ((INT64)(*((INT16*)(INT64_(172)+((ADDRESS)(&Minus_m_M_Minus_L_13))))))- ((INT64)(*((UINT32*)(INT64_(184)+((ADDRESS)(&Minus_m_M_Minus_L_13)))))))));
#line 431 "Minus.m3"
 /* load */
#line 431 "Minus.m3"
/*check_lo*/if(Minus_m_1296_L_1297<INT64_(-32768))Minus_m_M_Minus_L_13_CRASH(13793);
#line 431 "Minus.m3"
 /* exit_proc */
#line 431 "Minus.m3"
return Minus_m_1296_L_1297;
#line 431 "Minus.m3"
 /* end_procedure */
#line 431 "Minus.m3"
} /* uMinus_param_i16_u32 */
#line 431 "Minus.m3"
 /* set_source_line */
#line 431 "Minus.m3"
#line 432 "Minus.m3"
 /* begin_procedure */
#line 432 "Minus.m3"
struct Minus__uMinus_param_i16_u32_Frame_t {
#line 432 "Minus.m3"
ADDRESS _unused;
#line 432 "Minus.m3"
};
#line 432 "Minus.m3"
Word__T
__cdecl
Minus__uMinus_param_i16_u32(
   /* Param_Type1 */ Minus__INT16 a_L_771,
   /* Param_Type1 */ Minus__UINT32 b_L_772)
{
#line 432 "Minus.m3"
Minus__uMinus_param_i16_u32_Frame_t _frame;
#line 432 "Minus.m3"
_frame._unused=(ADDRESS)&_frame;
#line 432 "Minus.m3"
 /* load */
#line 432 "Minus.m3"
 /* load */
#line 432 "Minus.m3"
 /* subtract */
#line 432 "Minus.m3"
 /* exit_proc */
#line 432 "Minus.m3"
return ((UINT64)(((UINT64)(((INT64)(a_L_771))))-((UINT64)(((INT64)(b_L_772))))));
#line 432 "Minus.m3"
 /* end_procedure */
#line 432 "Minus.m3"
} /* Minus_param_i16_u32 */
#line 432 "Minus.m3"
 /* set_source_line */
#line 432 "Minus.m3"
#line 433 "Minus.m3"
 /* begin_procedure */
#line 433 "Minus.m3"
struct Minus__Minus_param_i16_u32_Frame_t {
#line 433 "Minus.m3"
ADDRESS _unused;
#line 433 "Minus.m3"
};
#line 433 "Minus.m3"
Minus__INT16
__cdecl
Minus__Minus_param_i16_u32(
   /* Param_Type1 */ Minus__INT16 a_L_774,
   /* Param_Type1 */ Minus__UINT32 b_L_775)
{
#line 433 "Minus.m3"
 /* Var_Type2 */ INT64 Minus_m_1298_L_1299={0};//always-init
#line 433 "Minus.m3"
Minus__Minus_param_i16_u32_Frame_t _frame;
#line 433 "Minus.m3"
_frame._unused=(ADDRESS)&_frame;
#line 433 "Minus.m3"
 /* load */
#line 433 "Minus.m3"
 /* load */
#line 433 "Minus.m3"
 /* subtract */
#line 433 "Minus.m3"
 /* check_lo */
#line 433 "Minus.m3"
 /* store */
#line 433 "Minus.m3"
(*(INT64*)(&Minus_m_1298_L_1299))=(INT64)( ((INT64)( ((INT64)(a_L_774))- ((INT64)(b_L_775)))));
#line 433 "Minus.m3"
 /* load */
#line 433 "Minus.m3"
/*check_lo*/if(Minus_m_1298_L_1299<INT64_(-32768))Minus_m_M_Minus_L_13_CRASH(13857);
#line 433 "Minus.m3"
 /* exit_proc */
#line 433 "Minus.m3"
return Minus_m_1298_L_1299;
#line 433 "Minus.m3"
 /* end_procedure */
#line 433 "Minus.m3"
} /* uMinus_var_i16_u8 */
#line 433 "Minus.m3"
 /* set_source_line */
#line 433 "Minus.m3"
#line 434 "Minus.m3"
 /* begin_procedure */
#line 434 "Minus.m3"
struct Minus__uMinus_var_i16_u8_Frame_t {
#line 434 "Minus.m3"
ADDRESS _unused;
#line 434 "Minus.m3"
};
#line 434 "Minus.m3"
Word__T
__cdecl
Minus__uMinus_var_i16_u8(void)
{
#line 434 "Minus.m3"
Minus__uMinus_var_i16_u8_Frame_t _frame;
#line 434 "Minus.m3"
_frame._unused=(ADDRESS)&_frame;
#line 434 "Minus.m3"
 /* load */
#line 434 "Minus.m3"
 /* load */
#line 434 "Minus.m3"
 /* subtract */
#line 434 "Minus.m3"
 /* exit_proc */
#line 434 "Minus.m3"
return ((UINT64)(((UINT64)(((INT64)(*((INT16*)(INT64_(172)+((ADDRESS)(&Minus_m_M_Minus_L_13))))))))-((UINT64)(((INT64)(*((UINT8*)(INT64_(188)+((ADDRESS)(&Minus_m_M_Minus_L_13))))))))));
#line 434 "Minus.m3"
 /* end_procedure */
#line 434 "Minus.m3"
} /* Minus_var_i16_u8 */
#line 434 "Minus.m3"
 /* set_source_line */
#line 434 "Minus.m3"
#line 435 "Minus.m3"
 /* begin_procedure */
#line 435 "Minus.m3"
struct Minus__Minus_var_i16_u8_Frame_t {
#line 435 "Minus.m3"
ADDRESS _unused;
#line 435 "Minus.m3"
};
#line 435 "Minus.m3"
Minus__INT16
__cdecl
Minus__Minus_var_i16_u8(void)
{
#line 435 "Minus.m3"
 /* Var_Type2 */ INT64 Minus_m_1300_L_1301={0};//always-init
#line 435 "Minus.m3"
Minus__Minus_var_i16_u8_Frame_t _frame;
#line 435 "Minus.m3"
_frame._unused=(ADDRESS)&_frame;
#line 435 "Minus.m3"
 /* load */
#line 435 "Minus.m3"
 /* load */
#line 435 "Minus.m3"
 /* subtract */
#line 435 "Minus.m3"
 /* check_lo */
#line 435 "Minus.m3"
 /* store */
#line 435 "Minus.m3"
(*(INT64*)(&Minus_m_1300_L_1301))=(INT64)( ((INT64)( ((INT64)(*((INT16*)(INT64_(172)+((ADDRESS)(&Minus_m_M_Minus_L_13))))))- ((INT64)(*((UINT8*)(INT64_(188)+((ADDRESS)(&Minus_m_M_Minus_L_13)))))))));
#line 435 "Minus.m3"
 /* load */
#line 435 "Minus.m3"
/*check_lo*/if(Minus_m_1300_L_1301<INT64_(-32768))Minus_m_M_Minus_L_13_CRASH(13921);
#line 435 "Minus.m3"
 /* exit_proc */
#line 435 "Minus.m3"
return Minus_m_1300_L_1301;
#line 435 "Minus.m3"
 /* end_procedure */
#line 435 "Minus.m3"
} /* uMinus_param_i16_u8 */
#line 435 "Minus.m3"
 /* set_source_line */
#line 435 "Minus.m3"
#line 436 "Minus.m3"
 /* begin_procedure */
#line 436 "Minus.m3"
struct Minus__uMinus_param_i16_u8_Frame_t {
#line 436 "Minus.m3"
ADDRESS _unused;
#line 436 "Minus.m3"
};
#line 436 "Minus.m3"
Word__T
__cdecl
Minus__uMinus_param_i16_u8(
   /* Param_Type1 */ Minus__INT16 a_L_779,
   /* Param_Type1 */ Minus__UINT8 b_L_780)
{
#line 436 "Minus.m3"
Minus__uMinus_param_i16_u8_Frame_t _frame;
#line 436 "Minus.m3"
_frame._unused=(ADDRESS)&_frame;
#line 436 "Minus.m3"
 /* load */
#line 436 "Minus.m3"
 /* load */
#line 436 "Minus.m3"
 /* subtract */
#line 436 "Minus.m3"
 /* exit_proc */
#line 436 "Minus.m3"
return ((UINT64)(((UINT64)(((INT64)(a_L_779))))-((UINT64)(((INT64)(b_L_780))))));
#line 436 "Minus.m3"
 /* end_procedure */
#line 436 "Minus.m3"
} /* Minus_param_i16_u8 */
#line 436 "Minus.m3"
 /* set_source_line */
#line 436 "Minus.m3"
#line 437 "Minus.m3"
 /* begin_procedure */
#line 437 "Minus.m3"
struct Minus__Minus_param_i16_u8_Frame_t {
#line 437 "Minus.m3"
ADDRESS _unused;
#line 437 "Minus.m3"
};
#line 437 "Minus.m3"
Minus__INT16
__cdecl
Minus__Minus_param_i16_u8(
   /* Param_Type1 */ Minus__INT16 a_L_782,
   /* Param_Type1 */ Minus__UINT8 b_L_783)
{
#line 437 "Minus.m3"
 /* Var_Type2 */ INT64 Minus_m_1302_L_1303={0};//always-init
#line 437 "Minus.m3"
Minus__Minus_param_i16_u8_Frame_t _frame;
#line 437 "Minus.m3"
_frame._unused=(ADDRESS)&_frame;
#line 437 "Minus.m3"
 /* load */
#line 437 "Minus.m3"
 /* load */
#line 437 "Minus.m3"
 /* subtract */
#line 437 "Minus.m3"
 /* check_lo */
#line 437 "Minus.m3"
 /* store */
#line 437 "Minus.m3"
(*(INT64*)(&Minus_m_1302_L_1303))=(INT64)( ((INT64)( ((INT64)(a_L_782))- ((INT64)(b_L_783)))));
#line 437 "Minus.m3"
 /* load */
#line 437 "Minus.m3"
/*check_lo*/if(Minus_m_1302_L_1303<INT64_(-32768))Minus_m_M_Minus_L_13_CRASH(13985);
#line 437 "Minus.m3"
 /* exit_proc */
#line 437 "Minus.m3"
return Minus_m_1302_L_1303;
#line 437 "Minus.m3"
 /* end_procedure */
#line 437 "Minus.m3"
} /* uMinus_var_i16_L */
#line 437 "Minus.m3"
 /* set_source_line */
#line 437 "Minus.m3"
#line 438 "Minus.m3"
 /* begin_procedure */
#line 438 "Minus.m3"
struct Minus__uMinus_var_i16_L_Frame_t {
#line 438 "Minus.m3"
ADDRESS _unused;
#line 438 "Minus.m3"
};
#line 438 "Minus.m3"
Long__T
__cdecl
Minus__uMinus_var_i16_L(void)
{
#line 438 "Minus.m3"
Minus__uMinus_var_i16_L_Frame_t _frame;
#line 438 "Minus.m3"
_frame._unused=(ADDRESS)&_frame;
#line 438 "Minus.m3"
 /* load */
#line 438 "Minus.m3"
 /* loophole */
#line 438 "Minus.m3"
 /* load */
#line 438 "Minus.m3"
 /* subtract */
#line 438 "Minus.m3"
 /* exit_proc */
#line 438 "Minus.m3"
return ((UINT64)(((UINT64)((INT64)((INT64)(*((INT16*)(INT64_(172)+((ADDRESS)(&Minus_m_M_Minus_L_13))))))))-((UINT64)(*((INT64*)(INT64_(192)+((ADDRESS)(&Minus_m_M_Minus_L_13))))))));
#line 438 "Minus.m3"
 /* end_procedure */
#line 438 "Minus.m3"
} /* Minus_var_i16_L */
#line 438 "Minus.m3"
 /* set_source_line */
#line 438 "Minus.m3"
#line 439 "Minus.m3"
 /* begin_procedure */
#line 439 "Minus.m3"
struct Minus__Minus_var_i16_L_Frame_t {
#line 439 "Minus.m3"
ADDRESS _unused;
#line 439 "Minus.m3"
};
#line 439 "Minus.m3"
LONGINT
__cdecl
Minus__Minus_var_i16_L(void)
{
#line 439 "Minus.m3"
Minus__Minus_var_i16_L_Frame_t _frame;
#line 439 "Minus.m3"
_frame._unused=(ADDRESS)&_frame;
#line 439 "Minus.m3"
 /* load */
#line 439 "Minus.m3"
 /* loophole */
#line 439 "Minus.m3"
 /* load */
#line 439 "Minus.m3"
 /* subtract */
#line 439 "Minus.m3"
 /* exit_proc */
#line 439 "Minus.m3"
return ((INT64)(((INT64)((INT64)((INT64)(*((INT16*)(INT64_(172)+((ADDRESS)(&Minus_m_M_Minus_L_13))))))))-((INT64)(*((INT64*)(INT64_(192)+((ADDRESS)(&Minus_m_M_Minus_L_13))))))));
#line 439 "Minus.m3"
 /* end_procedure */
#line 439 "Minus.m3"
} /* uMinus_param_i16_L */
#line 439 "Minus.m3"
 /* set_source_line */
#line 439 "Minus.m3"
#line 440 "Minus.m3"
 /* begin_procedure */
#line 440 "Minus.m3"
struct Minus__uMinus_param_i16_L_Frame_t {
#line 440 "Minus.m3"
ADDRESS _unused;
#line 440 "Minus.m3"
};
#line 440 "Minus.m3"
Long__T
__cdecl
Minus__uMinus_param_i16_L(
   /* Param_Type1 */ Minus__INT16 a_L_787,
   /* Param_Type1 */ LONGINT b_L_788)
{
#line 440 "Minus.m3"
Minus__uMinus_param_i16_L_Frame_t _frame;
#line 440 "Minus.m3"
_frame._unused=(ADDRESS)&_frame;
#line 440 "Minus.m3"
 /* load */
#line 440 "Minus.m3"
 /* loophole */
#line 440 "Minus.m3"
 /* load */
#line 440 "Minus.m3"
 /* subtract */
#line 440 "Minus.m3"
 /* exit_proc */
#line 440 "Minus.m3"
return ((UINT64)(((UINT64)((INT64)((INT64)(a_L_787))))-((UINT64)(b_L_788))));
#line 440 "Minus.m3"
 /* end_procedure */
#line 440 "Minus.m3"
} /* Minus_param_i16_L */
#line 440 "Minus.m3"
 /* set_source_line */
#line 440 "Minus.m3"
#line 441 "Minus.m3"
 /* begin_procedure */
#line 441 "Minus.m3"
struct Minus__Minus_param_i16_L_Frame_t {
#line 441 "Minus.m3"
ADDRESS _unused;
#line 441 "Minus.m3"
};
#line 441 "Minus.m3"
LONGINT
__cdecl
Minus__Minus_param_i16_L(
   /* Param_Type1 */ Minus__INT16 a_L_790,
   /* Param_Type1 */ LONGINT b_L_791)
{
#line 441 "Minus.m3"
Minus__Minus_param_i16_L_Frame_t _frame;
#line 441 "Minus.m3"
_frame._unused=(ADDRESS)&_frame;
#line 441 "Minus.m3"
 /* load */
#line 441 "Minus.m3"
 /* loophole */
#line 441 "Minus.m3"
 /* load */
#line 441 "Minus.m3"
 /* subtract */
#line 441 "Minus.m3"
 /* exit_proc */
#line 441 "Minus.m3"
return ((INT64)(((INT64)((INT64)((INT64)(a_L_790))))- b_L_791));
#line 441 "Minus.m3"
 /* end_procedure */
#line 441 "Minus.m3"
} /* uMinus_var_C_i8 */
#line 441 "Minus.m3"
 /* set_source_line */
#line 441 "Minus.m3"
#line 442 "Minus.m3"
 /* begin_procedure */
#line 442 "Minus.m3"
struct Minus__uMinus_var_C_i8_Frame_t {
#line 442 "Minus.m3"
ADDRESS _unused;
#line 442 "Minus.m3"
};
#line 442 "Minus.m3"
Word__T
__cdecl
Minus__uMinus_var_C_i8(void)
{
#line 442 "Minus.m3"
Minus__uMinus_var_C_i8_Frame_t _frame;
#line 442 "Minus.m3"
_frame._unused=(ADDRESS)&_frame;
#line 442 "Minus.m3"
 /* load */
#line 442 "Minus.m3"
 /* load */
#line 442 "Minus.m3"
 /* subtract */
#line 442 "Minus.m3"
 /* exit_proc */
#line 442 "Minus.m3"
return ((UINT64)(((UINT64)(((INT64)(*((UINT64*)(INT64_(176)+((ADDRESS)(&Minus_m_M_Minus_L_13))))))))-((UINT64)(((INT64)(*((INT8*)(INT64_(104)+((ADDRESS)(&Minus_m_M_Minus_L_13))))))))));
#line 442 "Minus.m3"
 /* end_procedure */
#line 442 "Minus.m3"
} /* Minus_var_C_i8 */
#line 442 "Minus.m3"
 /* set_source_line */
#line 442 "Minus.m3"
#line 443 "Minus.m3"
 /* begin_procedure */
#line 443 "Minus.m3"
struct Minus__Minus_var_C_i8_Frame_t {
#line 443 "Minus.m3"
ADDRESS _unused;
#line 443 "Minus.m3"
};
#line 443 "Minus.m3"
CARDINAL
__cdecl
Minus__Minus_var_C_i8(void)
{
#line 443 "Minus.m3"
 /* Var_Type2 */ INT64 Minus_m_1304_L_1305={0};//always-init
#line 443 "Minus.m3"
Minus__Minus_var_C_i8_Frame_t _frame;
#line 443 "Minus.m3"
_frame._unused=(ADDRESS)&_frame;
#line 443 "Minus.m3"
 /* load */
#line 443 "Minus.m3"
 /* load */
#line 443 "Minus.m3"
 /* subtract */
#line 443 "Minus.m3"
 /* check_lo */
#line 443 "Minus.m3"
 /* store */
#line 443 "Minus.m3"
(*(INT64*)(&Minus_m_1304_L_1305))=(INT64)( ((INT64)( ((INT64)(*((UINT64*)(INT64_(176)+((ADDRESS)(&Minus_m_M_Minus_L_13))))))- ((INT64)(*((INT8*)(INT64_(104)+((ADDRESS)(&Minus_m_M_Minus_L_13)))))))));
#line 443 "Minus.m3"
 /* load */
#line 443 "Minus.m3"
/*check_lo*/if(Minus_m_1304_L_1305<INT64_(0))Minus_m_M_Minus_L_13_CRASH(14177);
#line 443 "Minus.m3"
 /* exit_proc */
#line 443 "Minus.m3"
return Minus_m_1304_L_1305;
#line 443 "Minus.m3"
 /* end_procedure */
#line 443 "Minus.m3"
} /* uMinus_param_C_i8 */
#line 443 "Minus.m3"
 /* set_source_line */
#line 443 "Minus.m3"
#line 444 "Minus.m3"
 /* begin_procedure */
#line 444 "Minus.m3"
struct Minus__uMinus_param_C_i8_Frame_t {
#line 444 "Minus.m3"
ADDRESS _unused;
#line 444 "Minus.m3"
};
#line 444 "Minus.m3"
Word__T
__cdecl
Minus__uMinus_param_C_i8(
   /* Param_Type1 */ CARDINAL a_L_795,
   /* Param_Type1 */ Minus__INT8 b_L_796)
{
#line 444 "Minus.m3"
Minus__uMinus_param_C_i8_Frame_t _frame;
#line 444 "Minus.m3"
_frame._unused=(ADDRESS)&_frame;
#line 444 "Minus.m3"
 /* load */
#line 444 "Minus.m3"
 /* load */
#line 444 "Minus.m3"
 /* subtract */
#line 444 "Minus.m3"
 /* exit_proc */
#line 444 "Minus.m3"
return ((UINT64)(((UINT64)(((INT64)(a_L_795))))-((UINT64)(((INT64)(b_L_796))))));
#line 444 "Minus.m3"
 /* end_procedure */
#line 444 "Minus.m3"
} /* Minus_param_C_i8 */
#line 444 "Minus.m3"
 /* set_source_line */
#line 444 "Minus.m3"
#line 445 "Minus.m3"
 /* begin_procedure */
#line 445 "Minus.m3"
struct Minus__Minus_param_C_i8_Frame_t {
#line 445 "Minus.m3"
ADDRESS _unused;
#line 445 "Minus.m3"
};
#line 445 "Minus.m3"
CARDINAL
__cdecl
Minus__Minus_param_C_i8(
   /* Param_Type1 */ CARDINAL a_L_798,
   /* Param_Type1 */ Minus__INT8 b_L_799)
{
#line 445 "Minus.m3"
 /* Var_Type2 */ INT64 Minus_m_1306_L_1307={0};//always-init
#line 445 "Minus.m3"
Minus__Minus_param_C_i8_Frame_t _frame;
#line 445 "Minus.m3"
_frame._unused=(ADDRESS)&_frame;
#line 445 "Minus.m3"
 /* load */
#line 445 "Minus.m3"
 /* load */
#line 445 "Minus.m3"
 /* subtract */
#line 445 "Minus.m3"
 /* check_lo */
#line 445 "Minus.m3"
 /* store */
#line 445 "Minus.m3"
(*(INT64*)(&Minus_m_1306_L_1307))=(INT64)( ((INT64)( ((INT64)(a_L_798))- ((INT64)(b_L_799)))));
#line 445 "Minus.m3"
 /* load */
#line 445 "Minus.m3"
/*check_lo*/if(Minus_m_1306_L_1307<INT64_(0))Minus_m_M_Minus_L_13_CRASH(14241);
#line 445 "Minus.m3"
 /* exit_proc */
#line 445 "Minus.m3"
return Minus_m_1306_L_1307;
#line 445 "Minus.m3"
 /* end_procedure */
#line 445 "Minus.m3"
} /* uMinus_var_C_u64 */
#line 445 "Minus.m3"
 /* set_source_line */
#line 445 "Minus.m3"
#line 446 "Minus.m3"
 /* begin_procedure */
#line 446 "Minus.m3"
struct Minus__uMinus_var_C_u64_Frame_t {
#line 446 "Minus.m3"
ADDRESS _unused;
#line 446 "Minus.m3"
};
#line 446 "Minus.m3"
Long__T
__cdecl
Minus__uMinus_var_C_u64(void)
{
#line 446 "Minus.m3"
Minus__uMinus_var_C_u64_Frame_t _frame;
#line 446 "Minus.m3"
_frame._unused=(ADDRESS)&_frame;
#line 446 "Minus.m3"
 /* load */
#line 446 "Minus.m3"
 /* loophole */
#line 446 "Minus.m3"
 /* load */
#line 446 "Minus.m3"
 /* subtract */
#line 446 "Minus.m3"
 /* exit_proc */
#line 446 "Minus.m3"
return ((UINT64)(((UINT64)((INT64)((INT64)(*((UINT64*)(INT64_(176)+((ADDRESS)(&Minus_m_M_Minus_L_13))))))))-((UINT64)(*((INT64*)(INT64_(112)+((ADDRESS)(&Minus_m_M_Minus_L_13))))))));
#line 446 "Minus.m3"
 /* end_procedure */
#line 446 "Minus.m3"
} /* Minus_var_C_u64 */
#line 446 "Minus.m3"
 /* set_source_line */
#line 446 "Minus.m3"
#line 447 "Minus.m3"
 /* begin_procedure */
#line 447 "Minus.m3"
struct Minus__Minus_var_C_u64_Frame_t {
#line 447 "Minus.m3"
ADDRESS _unused;
#line 447 "Minus.m3"
};
#line 447 "Minus.m3"
LONGINT
__cdecl
Minus__Minus_var_C_u64(void)
{
#line 447 "Minus.m3"
Minus__Minus_var_C_u64_Frame_t _frame;
#line 447 "Minus.m3"
_frame._unused=(ADDRESS)&_frame;
#line 447 "Minus.m3"
 /* load */
#line 447 "Minus.m3"
 /* loophole */
#line 447 "Minus.m3"
 /* load */
#line 447 "Minus.m3"
 /* subtract */
#line 447 "Minus.m3"
 /* exit_proc */
#line 447 "Minus.m3"
return ((INT64)(((INT64)((INT64)((INT64)(*((UINT64*)(INT64_(176)+((ADDRESS)(&Minus_m_M_Minus_L_13))))))))-((INT64)(*((INT64*)(INT64_(112)+((ADDRESS)(&Minus_m_M_Minus_L_13))))))));
#line 447 "Minus.m3"
 /* end_procedure */
#line 447 "Minus.m3"
} /* uMinus_param_C_u64 */
#line 447 "Minus.m3"
 /* set_source_line */
#line 447 "Minus.m3"
#line 448 "Minus.m3"
 /* begin_procedure */
#line 448 "Minus.m3"
struct Minus__uMinus_param_C_u64_Frame_t {
#line 448 "Minus.m3"
ADDRESS _unused;
#line 448 "Minus.m3"
};
#line 448 "Minus.m3"
Long__T
__cdecl
Minus__uMinus_param_C_u64(
   /* Param_Type1 */ CARDINAL a_L_803,
   /* Param_Type1 */ Minus__UINT64 b_L_804)
{
#line 448 "Minus.m3"
Minus__uMinus_param_C_u64_Frame_t _frame;
#line 448 "Minus.m3"
_frame._unused=(ADDRESS)&_frame;
#line 448 "Minus.m3"
 /* load */
#line 448 "Minus.m3"
 /* loophole */
#line 448 "Minus.m3"
 /* load */
#line 448 "Minus.m3"
 /* subtract */
#line 448 "Minus.m3"
 /* exit_proc */
#line 448 "Minus.m3"
return ((UINT64)(((UINT64)((INT64)((INT64)(a_L_803))))-((UINT64)(b_L_804))));
#line 448 "Minus.m3"
 /* end_procedure */
#line 448 "Minus.m3"
} /* Minus_param_C_u64 */
#line 448 "Minus.m3"
 /* set_source_line */
#line 448 "Minus.m3"
#line 449 "Minus.m3"
 /* begin_procedure */
#line 449 "Minus.m3"
struct Minus__Minus_param_C_u64_Frame_t {
#line 449 "Minus.m3"
ADDRESS _unused;
#line 449 "Minus.m3"
};
#line 449 "Minus.m3"
LONGINT
__cdecl
Minus__Minus_param_C_u64(
   /* Param_Type1 */ CARDINAL a_L_806,
   /* Param_Type1 */ Minus__UINT64 b_L_807)
{
#line 449 "Minus.m3"
Minus__Minus_param_C_u64_Frame_t _frame;
#line 449 "Minus.m3"
_frame._unused=(ADDRESS)&_frame;
#line 449 "Minus.m3"
 /* load */
#line 449 "Minus.m3"
 /* loophole */
#line 449 "Minus.m3"
 /* load */
#line 449 "Minus.m3"
 /* subtract */
#line 449 "Minus.m3"
 /* exit_proc */
#line 449 "Minus.m3"
return ((INT64)(((INT64)((INT64)((INT64)(a_L_806))))- b_L_807));
#line 449 "Minus.m3"
 /* end_procedure */
#line 449 "Minus.m3"
} /* uMinus_var_C_i32 */
#line 449 "Minus.m3"
 /* set_source_line */
#line 449 "Minus.m3"
#line 450 "Minus.m3"
 /* begin_procedure */
#line 450 "Minus.m3"
struct Minus__uMinus_var_C_i32_Frame_t {
#line 450 "Minus.m3"
ADDRESS _unused;
#line 450 "Minus.m3"
};
#line 450 "Minus.m3"
Word__T
__cdecl
Minus__uMinus_var_C_i32(void)
{
#line 450 "Minus.m3"
Minus__uMinus_var_C_i32_Frame_t _frame;
#line 450 "Minus.m3"
_frame._unused=(ADDRESS)&_frame;
#line 450 "Minus.m3"
 /* load */
#line 450 "Minus.m3"
 /* load */
#line 450 "Minus.m3"
 /* subtract */
#line 450 "Minus.m3"
 /* exit_proc */
#line 450 "Minus.m3"
return ((UINT64)(((UINT64)(((INT64)(*((UINT64*)(INT64_(176)+((ADDRESS)(&Minus_m_M_Minus_L_13))))))))-((UINT64)(((INT64)(*((INT32*)(INT64_(128)+((ADDRESS)(&Minus_m_M_Minus_L_13))))))))));
#line 450 "Minus.m3"
 /* end_procedure */
#line 450 "Minus.m3"
} /* Minus_var_C_i32 */
#line 450 "Minus.m3"
 /* set_source_line */
#line 450 "Minus.m3"
#line 451 "Minus.m3"
 /* begin_procedure */
#line 451 "Minus.m3"
struct Minus__Minus_var_C_i32_Frame_t {
#line 451 "Minus.m3"
ADDRESS _unused;
#line 451 "Minus.m3"
};
#line 451 "Minus.m3"
CARDINAL
__cdecl
Minus__Minus_var_C_i32(void)
{
#line 451 "Minus.m3"
 /* Var_Type2 */ INT64 Minus_m_1308_L_1309={0};//always-init
#line 451 "Minus.m3"
Minus__Minus_var_C_i32_Frame_t _frame;
#line 451 "Minus.m3"
_frame._unused=(ADDRESS)&_frame;
#line 451 "Minus.m3"
 /* load */
#line 451 "Minus.m3"
 /* load */
#line 451 "Minus.m3"
 /* subtract */
#line 451 "Minus.m3"
 /* check_lo */
#line 451 "Minus.m3"
 /* store */
#line 451 "Minus.m3"
(*(INT64*)(&Minus_m_1308_L_1309))=(INT64)( ((INT64)( ((INT64)(*((UINT64*)(INT64_(176)+((ADDRESS)(&Minus_m_M_Minus_L_13))))))- ((INT64)(*((INT32*)(INT64_(128)+((ADDRESS)(&Minus_m_M_Minus_L_13)))))))));
#line 451 "Minus.m3"
 /* load */
#line 451 "Minus.m3"
/*check_lo*/if(Minus_m_1308_L_1309<INT64_(0))Minus_m_M_Minus_L_13_CRASH(14433);
#line 451 "Minus.m3"
 /* exit_proc */
#line 451 "Minus.m3"
return Minus_m_1308_L_1309;
#line 451 "Minus.m3"
 /* end_procedure */
#line 451 "Minus.m3"
} /* uMinus_param_C_i32 */
#line 451 "Minus.m3"
 /* set_source_line */
#line 451 "Minus.m3"
#line 452 "Minus.m3"
 /* begin_procedure */
#line 452 "Minus.m3"
struct Minus__uMinus_param_C_i32_Frame_t {
#line 452 "Minus.m3"
ADDRESS _unused;
#line 452 "Minus.m3"
};
#line 452 "Minus.m3"
Word__T
__cdecl
Minus__uMinus_param_C_i32(
   /* Param_Type1 */ CARDINAL a_L_811,
   /* Param_Type1 */ Minus__INT32 b_L_812)
{
#line 452 "Minus.m3"
Minus__uMinus_param_C_i32_Frame_t _frame;
#line 452 "Minus.m3"
_frame._unused=(ADDRESS)&_frame;
#line 452 "Minus.m3"
 /* load */
#line 452 "Minus.m3"
 /* load */
#line 452 "Minus.m3"
 /* subtract */
#line 452 "Minus.m3"
 /* exit_proc */
#line 452 "Minus.m3"
return ((UINT64)(((UINT64)(((INT64)(a_L_811))))-((UINT64)(((INT64)(b_L_812))))));
#line 452 "Minus.m3"
 /* end_procedure */
#line 452 "Minus.m3"
} /* Minus_param_C_i32 */
#line 452 "Minus.m3"
 /* set_source_line */
#line 452 "Minus.m3"
#line 453 "Minus.m3"
 /* begin_procedure */
#line 453 "Minus.m3"
struct Minus__Minus_param_C_i32_Frame_t {
#line 453 "Minus.m3"
ADDRESS _unused;
#line 453 "Minus.m3"
};
#line 453 "Minus.m3"
CARDINAL
__cdecl
Minus__Minus_param_C_i32(
   /* Param_Type1 */ CARDINAL a_L_814,
   /* Param_Type1 */ Minus__INT32 b_L_815)
{
#line 453 "Minus.m3"
 /* Var_Type2 */ INT64 Minus_m_1310_L_1311={0};//always-init
#line 453 "Minus.m3"
Minus__Minus_param_C_i32_Frame_t _frame;
#line 453 "Minus.m3"
_frame._unused=(ADDRESS)&_frame;
#line 453 "Minus.m3"
 /* load */
#line 453 "Minus.m3"
 /* load */
#line 453 "Minus.m3"
 /* subtract */
#line 453 "Minus.m3"
 /* check_lo */
#line 453 "Minus.m3"
 /* store */
#line 453 "Minus.m3"
(*(INT64*)(&Minus_m_1310_L_1311))=(INT64)( ((INT64)( ((INT64)(a_L_814))- ((INT64)(b_L_815)))));
#line 453 "Minus.m3"
 /* load */
#line 453 "Minus.m3"
/*check_lo*/if(Minus_m_1310_L_1311<INT64_(0))Minus_m_M_Minus_L_13_CRASH(14497);
#line 453 "Minus.m3"
 /* exit_proc */
#line 453 "Minus.m3"
return Minus_m_1310_L_1311;
#line 453 "Minus.m3"
 /* end_procedure */
#line 453 "Minus.m3"
} /* uMinus_var_C_LC */
#line 453 "Minus.m3"
 /* set_source_line */
#line 453 "Minus.m3"
#line 454 "Minus.m3"
 /* begin_procedure */
#line 454 "Minus.m3"
struct Minus__uMinus_var_C_LC_Frame_t {
#line 454 "Minus.m3"
ADDRESS _unused;
#line 454 "Minus.m3"
};
#line 454 "Minus.m3"
Long__T
__cdecl
Minus__uMinus_var_C_LC(void)
{
#line 454 "Minus.m3"
Minus__uMinus_var_C_LC_Frame_t _frame;
#line 454 "Minus.m3"
_frame._unused=(ADDRESS)&_frame;
#line 454 "Minus.m3"
 /* load */
#line 454 "Minus.m3"
 /* loophole */
#line 454 "Minus.m3"
 /* load */
#line 454 "Minus.m3"
 /* subtract */
#line 454 "Minus.m3"
 /* exit_proc */
#line 454 "Minus.m3"
return ((UINT64)(((UINT64)((INT64)((INT64)(*((UINT64*)(INT64_(176)+((ADDRESS)(&Minus_m_M_Minus_L_13))))))))-((UINT64)(((INT64)(*((UINT64*)(INT64_(136)+((ADDRESS)(&Minus_m_M_Minus_L_13))))))))));
#line 454 "Minus.m3"
 /* end_procedure */
#line 454 "Minus.m3"
} /* Minus_var_C_LC */
#line 454 "Minus.m3"
 /* set_source_line */
#line 454 "Minus.m3"
#line 455 "Minus.m3"
 /* begin_procedure */
#line 455 "Minus.m3"
struct Minus__Minus_var_C_LC_Frame_t {
#line 455 "Minus.m3"
ADDRESS _unused;
#line 455 "Minus.m3"
};
#line 455 "Minus.m3"
LONGINT
__cdecl
Minus__Minus_var_C_LC(void)
{
#line 455 "Minus.m3"
Minus__Minus_var_C_LC_Frame_t _frame;
#line 455 "Minus.m3"
_frame._unused=(ADDRESS)&_frame;
#line 455 "Minus.m3"
 /* load */
#line 455 "Minus.m3"
 /* loophole */
#line 455 "Minus.m3"
 /* load */
#line 455 "Minus.m3"
 /* subtract */
#line 455 "Minus.m3"
 /* exit_proc */
#line 455 "Minus.m3"
return ((INT64)(((INT64)((INT64)((INT64)(*((UINT64*)(INT64_(176)+((ADDRESS)(&Minus_m_M_Minus_L_13))))))))- ((INT64)(*((UINT64*)(INT64_(136)+((ADDRESS)(&Minus_m_M_Minus_L_13))))))));
#line 455 "Minus.m3"
 /* end_procedure */
#line 455 "Minus.m3"
} /* uMinus_param_C_LC */
#line 455 "Minus.m3"
 /* set_source_line */
#line 455 "Minus.m3"
#line 456 "Minus.m3"
 /* begin_procedure */
#line 456 "Minus.m3"
struct Minus__uMinus_param_C_LC_Frame_t {
#line 456 "Minus.m3"
ADDRESS _unused;
#line 456 "Minus.m3"
};
#line 456 "Minus.m3"
Long__T
__cdecl
Minus__uMinus_param_C_LC(
   /* Param_Type1 */ CARDINAL a_L_819,
   /* Param_Type1 */ LONGCARD b_L_820)
{
#line 456 "Minus.m3"
Minus__uMinus_param_C_LC_Frame_t _frame;
#line 456 "Minus.m3"
_frame._unused=(ADDRESS)&_frame;
#line 456 "Minus.m3"
 /* load */
#line 456 "Minus.m3"
 /* loophole */
#line 456 "Minus.m3"
 /* load */
#line 456 "Minus.m3"
 /* subtract */
#line 456 "Minus.m3"
 /* exit_proc */
#line 456 "Minus.m3"
return ((UINT64)(((UINT64)((INT64)((INT64)(a_L_819))))-((UINT64)(((INT64)(b_L_820))))));
#line 456 "Minus.m3"
 /* end_procedure */
#line 456 "Minus.m3"
} /* Minus_param_C_LC */
#line 456 "Minus.m3"
 /* set_source_line */
#line 456 "Minus.m3"
#line 457 "Minus.m3"
 /* begin_procedure */
#line 457 "Minus.m3"
struct Minus__Minus_param_C_LC_Frame_t {
#line 457 "Minus.m3"
ADDRESS _unused;
#line 457 "Minus.m3"
};
#line 457 "Minus.m3"
LONGINT
__cdecl
Minus__Minus_param_C_LC(
   /* Param_Type1 */ CARDINAL a_L_822,
   /* Param_Type1 */ LONGCARD b_L_823)
{
#line 457 "Minus.m3"
Minus__Minus_param_C_LC_Frame_t _frame;
#line 457 "Minus.m3"
_frame._unused=(ADDRESS)&_frame;
#line 457 "Minus.m3"
 /* load */
#line 457 "Minus.m3"
 /* loophole */
#line 457 "Minus.m3"
 /* load */
#line 457 "Minus.m3"
 /* subtract */
#line 457 "Minus.m3"
 /* exit_proc */
#line 457 "Minus.m3"
return ((INT64)(((INT64)((INT64)((INT64)(a_L_822))))- ((INT64)(b_L_823))));
#line 457 "Minus.m3"
 /* end_procedure */
#line 457 "Minus.m3"
} /* uMinus_var_C_u16 */
#line 457 "Minus.m3"
 /* set_source_line */
#line 457 "Minus.m3"
#line 458 "Minus.m3"
 /* begin_procedure */
#line 458 "Minus.m3"
struct Minus__uMinus_var_C_u16_Frame_t {
#line 458 "Minus.m3"
ADDRESS _unused;
#line 458 "Minus.m3"
};
#line 458 "Minus.m3"
Word__T
__cdecl
Minus__uMinus_var_C_u16(void)
{
#line 458 "Minus.m3"
Minus__uMinus_var_C_u16_Frame_t _frame;
#line 458 "Minus.m3"
_frame._unused=(ADDRESS)&_frame;
#line 458 "Minus.m3"
 /* load */
#line 458 "Minus.m3"
 /* load */
#line 458 "Minus.m3"
 /* subtract */
#line 458 "Minus.m3"
 /* exit_proc */
#line 458 "Minus.m3"
return ((UINT64)(((UINT64)(((INT64)(*((UINT64*)(INT64_(176)+((ADDRESS)(&Minus_m_M_Minus_L_13))))))))-((UINT64)(((INT64)(*((UINT16*)(INT64_(144)+((ADDRESS)(&Minus_m_M_Minus_L_13))))))))));
#line 458 "Minus.m3"
 /* end_procedure */
#line 458 "Minus.m3"
} /* Minus_var_C_u16 */
#line 458 "Minus.m3"
 /* set_source_line */
#line 458 "Minus.m3"
#line 459 "Minus.m3"
 /* begin_procedure */
#line 459 "Minus.m3"
struct Minus__Minus_var_C_u16_Frame_t {
#line 459 "Minus.m3"
ADDRESS _unused;
#line 459 "Minus.m3"
};
#line 459 "Minus.m3"
CARDINAL
__cdecl
Minus__Minus_var_C_u16(void)
{
#line 459 "Minus.m3"
 /* Var_Type2 */ INT64 Minus_m_1312_L_1313={0};//always-init
#line 459 "Minus.m3"
Minus__Minus_var_C_u16_Frame_t _frame;
#line 459 "Minus.m3"
_frame._unused=(ADDRESS)&_frame;
#line 459 "Minus.m3"
 /* load */
#line 459 "Minus.m3"
 /* load */
#line 459 "Minus.m3"
 /* subtract */
#line 459 "Minus.m3"
 /* check_lo */
#line 459 "Minus.m3"
 /* store */
#line 459 "Minus.m3"
(*(INT64*)(&Minus_m_1312_L_1313))=(INT64)( ((INT64)( ((INT64)(*((UINT64*)(INT64_(176)+((ADDRESS)(&Minus_m_M_Minus_L_13))))))- ((INT64)(*((UINT16*)(INT64_(144)+((ADDRESS)(&Minus_m_M_Minus_L_13)))))))));
#line 459 "Minus.m3"
 /* load */
#line 459 "Minus.m3"
/*check_lo*/if(Minus_m_1312_L_1313<INT64_(0))Minus_m_M_Minus_L_13_CRASH(14689);
#line 459 "Minus.m3"
 /* exit_proc */
#line 459 "Minus.m3"
return Minus_m_1312_L_1313;
#line 459 "Minus.m3"
 /* end_procedure */
#line 459 "Minus.m3"
} /* uMinus_param_C_u16 */
#line 459 "Minus.m3"
 /* set_source_line */
#line 459 "Minus.m3"
#line 460 "Minus.m3"
 /* begin_procedure */
#line 460 "Minus.m3"
struct Minus__uMinus_param_C_u16_Frame_t {
#line 460 "Minus.m3"
ADDRESS _unused;
#line 460 "Minus.m3"
};
#line 460 "Minus.m3"
Word__T
__cdecl
Minus__uMinus_param_C_u16(
   /* Param_Type1 */ CARDINAL a_L_827,
   /* Param_Type1 */ Minus__UINT16 b_L_828)
{
#line 460 "Minus.m3"
Minus__uMinus_param_C_u16_Frame_t _frame;
#line 460 "Minus.m3"
_frame._unused=(ADDRESS)&_frame;
#line 460 "Minus.m3"
 /* load */
#line 460 "Minus.m3"
 /* load */
#line 460 "Minus.m3"
 /* subtract */
#line 460 "Minus.m3"
 /* exit_proc */
#line 460 "Minus.m3"
return ((UINT64)(((UINT64)(((INT64)(a_L_827))))-((UINT64)(((INT64)(b_L_828))))));
#line 460 "Minus.m3"
 /* end_procedure */
#line 460 "Minus.m3"
} /* Minus_param_C_u16 */
#line 460 "Minus.m3"
 /* set_source_line */
#line 460 "Minus.m3"
#line 461 "Minus.m3"
 /* begin_procedure */
#line 461 "Minus.m3"
struct Minus__Minus_param_C_u16_Frame_t {
#line 461 "Minus.m3"
ADDRESS _unused;
#line 461 "Minus.m3"
};
#line 461 "Minus.m3"
CARDINAL
__cdecl
Minus__Minus_param_C_u16(
   /* Param_Type1 */ CARDINAL a_L_830,
   /* Param_Type1 */ Minus__UINT16 b_L_831)
{
#line 461 "Minus.m3"
 /* Var_Type2 */ INT64 Minus_m_1314_L_1315={0};//always-init
#line 461 "Minus.m3"
Minus__Minus_param_C_u16_Frame_t _frame;
#line 461 "Minus.m3"
_frame._unused=(ADDRESS)&_frame;
#line 461 "Minus.m3"
 /* load */
#line 461 "Minus.m3"
 /* load */
#line 461 "Minus.m3"
 /* subtract */
#line 461 "Minus.m3"
 /* check_lo */
#line 461 "Minus.m3"
 /* store */
#line 461 "Minus.m3"
(*(INT64*)(&Minus_m_1314_L_1315))=(INT64)( ((INT64)( ((INT64)(a_L_830))- ((INT64)(b_L_831)))));
#line 461 "Minus.m3"
 /* load */
#line 461 "Minus.m3"
/*check_lo*/if(Minus_m_1314_L_1315<INT64_(0))Minus_m_M_Minus_L_13_CRASH(14753);
#line 461 "Minus.m3"
 /* exit_proc */
#line 461 "Minus.m3"
return Minus_m_1314_L_1315;
#line 461 "Minus.m3"
 /* end_procedure */
#line 461 "Minus.m3"
} /* uMinus_var_C_I */
#line 461 "Minus.m3"
 /* set_source_line */
#line 461 "Minus.m3"
#line 462 "Minus.m3"
 /* begin_procedure */
#line 462 "Minus.m3"
struct Minus__uMinus_var_C_I_Frame_t {
#line 462 "Minus.m3"
ADDRESS _unused;
#line 462 "Minus.m3"
};
#line 462 "Minus.m3"
Word__T
__cdecl
Minus__uMinus_var_C_I(void)
{
#line 462 "Minus.m3"
Minus__uMinus_var_C_I_Frame_t _frame;
#line 462 "Minus.m3"
_frame._unused=(ADDRESS)&_frame;
#line 462 "Minus.m3"
 /* load */
#line 462 "Minus.m3"
 /* load */
#line 462 "Minus.m3"
 /* subtract */
#line 462 "Minus.m3"
 /* exit_proc */
#line 462 "Minus.m3"
return ((UINT64)(((UINT64)(((INT64)(*((UINT64*)(INT64_(176)+((ADDRESS)(&Minus_m_M_Minus_L_13))))))))-((UINT64)(*((INT64*)(INT64_(152)+((ADDRESS)(&Minus_m_M_Minus_L_13))))))));
#line 462 "Minus.m3"
 /* end_procedure */
#line 462 "Minus.m3"
} /* Minus_var_C_I */
#line 462 "Minus.m3"
 /* set_source_line */
#line 462 "Minus.m3"
#line 463 "Minus.m3"
 /* begin_procedure */
#line 463 "Minus.m3"
struct Minus__Minus_var_C_I_Frame_t {
#line 463 "Minus.m3"
ADDRESS _unused;
#line 463 "Minus.m3"
};
#line 463 "Minus.m3"
CARDINAL
__cdecl
Minus__Minus_var_C_I(void)
{
#line 463 "Minus.m3"
 /* Var_Type2 */ INT64 Minus_m_1316_L_1317={0};//always-init
#line 463 "Minus.m3"
Minus__Minus_var_C_I_Frame_t _frame;
#line 463 "Minus.m3"
_frame._unused=(ADDRESS)&_frame;
#line 463 "Minus.m3"
 /* load */
#line 463 "Minus.m3"
 /* load */
#line 463 "Minus.m3"
 /* subtract */
#line 463 "Minus.m3"
 /* check_lo */
#line 463 "Minus.m3"
 /* store */
#line 463 "Minus.m3"
(*(INT64*)(&Minus_m_1316_L_1317))=(INT64)( ((INT64)( ((INT64)(*((UINT64*)(INT64_(176)+((ADDRESS)(&Minus_m_M_Minus_L_13))))))-((INT64)(*((INT64*)(INT64_(152)+((ADDRESS)(&Minus_m_M_Minus_L_13)))))))));
#line 463 "Minus.m3"
 /* load */
#line 463 "Minus.m3"
/*check_lo*/if(Minus_m_1316_L_1317<INT64_(0))Minus_m_M_Minus_L_13_CRASH(14817);
#line 463 "Minus.m3"
 /* exit_proc */
#line 463 "Minus.m3"
return Minus_m_1316_L_1317;
#line 463 "Minus.m3"
 /* end_procedure */
#line 463 "Minus.m3"
} /* uMinus_param_C_I */
#line 463 "Minus.m3"
 /* set_source_line */
#line 463 "Minus.m3"
#line 464 "Minus.m3"
 /* begin_procedure */
#line 464 "Minus.m3"
struct Minus__uMinus_param_C_I_Frame_t {
#line 464 "Minus.m3"
ADDRESS _unused;
#line 464 "Minus.m3"
};
#line 464 "Minus.m3"
Word__T
__cdecl
Minus__uMinus_param_C_I(
   /* Param_Type1 */ CARDINAL a_L_835,
   /* Param_Type1 */ INTEGER b_L_836)
{
#line 464 "Minus.m3"
Minus__uMinus_param_C_I_Frame_t _frame;
#line 464 "Minus.m3"
_frame._unused=(ADDRESS)&_frame;
#line 464 "Minus.m3"
 /* load */
#line 464 "Minus.m3"
 /* load */
#line 464 "Minus.m3"
 /* subtract */
#line 464 "Minus.m3"
 /* exit_proc */
#line 464 "Minus.m3"
return ((UINT64)(((UINT64)(((INT64)(a_L_835))))-((UINT64)(b_L_836))));
#line 464 "Minus.m3"
 /* end_procedure */
#line 464 "Minus.m3"
} /* Minus_param_C_I */
#line 464 "Minus.m3"
 /* set_source_line */
#line 464 "Minus.m3"
#line 465 "Minus.m3"
 /* begin_procedure */
#line 465 "Minus.m3"
struct Minus__Minus_param_C_I_Frame_t {
#line 465 "Minus.m3"
ADDRESS _unused;
#line 465 "Minus.m3"
};
#line 465 "Minus.m3"
CARDINAL
__cdecl
Minus__Minus_param_C_I(
   /* Param_Type1 */ CARDINAL a_L_838,
   /* Param_Type1 */ INTEGER b_L_839)
{
#line 465 "Minus.m3"
 /* Var_Type2 */ INT64 Minus_m_1318_L_1319={0};//always-init
#line 465 "Minus.m3"
Minus__Minus_param_C_I_Frame_t _frame;
#line 465 "Minus.m3"
_frame._unused=(ADDRESS)&_frame;
#line 465 "Minus.m3"
 /* load */
#line 465 "Minus.m3"
 /* load */
#line 465 "Minus.m3"
 /* subtract */
#line 465 "Minus.m3"
 /* check_lo */
#line 465 "Minus.m3"
 /* store */
#line 465 "Minus.m3"
(*(INT64*)(&Minus_m_1318_L_1319))=(INT64)( ((INT64)( ((INT64)(a_L_838))- b_L_839)));
#line 465 "Minus.m3"
 /* load */
#line 465 "Minus.m3"
/*check_lo*/if(Minus_m_1318_L_1319<INT64_(0))Minus_m_M_Minus_L_13_CRASH(14881);
#line 465 "Minus.m3"
 /* exit_proc */
#line 465 "Minus.m3"
return Minus_m_1318_L_1319;
#line 465 "Minus.m3"
 /* end_procedure */
#line 465 "Minus.m3"
} /* uMinus_var_C_i64 */
#line 465 "Minus.m3"
 /* set_source_line */
#line 465 "Minus.m3"
#line 466 "Minus.m3"
 /* begin_procedure */
#line 466 "Minus.m3"
struct Minus__uMinus_var_C_i64_Frame_t {
#line 466 "Minus.m3"
ADDRESS _unused;
#line 466 "Minus.m3"
};
#line 466 "Minus.m3"
Long__T
__cdecl
Minus__uMinus_var_C_i64(void)
{
#line 466 "Minus.m3"
Minus__uMinus_var_C_i64_Frame_t _frame;
#line 466 "Minus.m3"
_frame._unused=(ADDRESS)&_frame;
#line 466 "Minus.m3"
 /* load */
#line 466 "Minus.m3"
 /* loophole */
#line 466 "Minus.m3"
 /* load */
#line 466 "Minus.m3"
 /* subtract */
#line 466 "Minus.m3"
 /* exit_proc */
#line 466 "Minus.m3"
return ((UINT64)(((UINT64)((INT64)((INT64)(*((UINT64*)(INT64_(176)+((ADDRESS)(&Minus_m_M_Minus_L_13))))))))-((UINT64)(*((INT64*)(INT64_(160)+((ADDRESS)(&Minus_m_M_Minus_L_13))))))));
#line 466 "Minus.m3"
 /* end_procedure */
#line 466 "Minus.m3"
} /* Minus_var_C_i64 */
#line 466 "Minus.m3"
 /* set_source_line */
#line 466 "Minus.m3"
#line 467 "Minus.m3"
 /* begin_procedure */
#line 467 "Minus.m3"
struct Minus__Minus_var_C_i64_Frame_t {
#line 467 "Minus.m3"
ADDRESS _unused;
#line 467 "Minus.m3"
};
#line 467 "Minus.m3"
LONGINT
__cdecl
Minus__Minus_var_C_i64(void)
{
#line 467 "Minus.m3"
Minus__Minus_var_C_i64_Frame_t _frame;
#line 467 "Minus.m3"
_frame._unused=(ADDRESS)&_frame;
#line 467 "Minus.m3"
 /* load */
#line 467 "Minus.m3"
 /* loophole */
#line 467 "Minus.m3"
 /* load */
#line 467 "Minus.m3"
 /* subtract */
#line 467 "Minus.m3"
 /* exit_proc */
#line 467 "Minus.m3"
return ((INT64)(((INT64)((INT64)((INT64)(*((UINT64*)(INT64_(176)+((ADDRESS)(&Minus_m_M_Minus_L_13))))))))-((INT64)(*((INT64*)(INT64_(160)+((ADDRESS)(&Minus_m_M_Minus_L_13))))))));
#line 467 "Minus.m3"
 /* end_procedure */
#line 467 "Minus.m3"
} /* uMinus_param_C_i64 */
#line 467 "Minus.m3"
 /* set_source_line */
#line 467 "Minus.m3"
#line 468 "Minus.m3"
 /* begin_procedure */
#line 468 "Minus.m3"
struct Minus__uMinus_param_C_i64_Frame_t {
#line 468 "Minus.m3"
ADDRESS _unused;
#line 468 "Minus.m3"
};
#line 468 "Minus.m3"
Long__T
__cdecl
Minus__uMinus_param_C_i64(
   /* Param_Type1 */ CARDINAL a_L_843,
   /* Param_Type1 */ Minus__INT64 b_L_844)
{
#line 468 "Minus.m3"
Minus__uMinus_param_C_i64_Frame_t _frame;
#line 468 "Minus.m3"
_frame._unused=(ADDRESS)&_frame;
#line 468 "Minus.m3"
 /* load */
#line 468 "Minus.m3"
 /* loophole */
#line 468 "Minus.m3"
 /* load */
#line 468 "Minus.m3"
 /* subtract */
#line 468 "Minus.m3"
 /* exit_proc */
#line 468 "Minus.m3"
return ((UINT64)(((UINT64)((INT64)((INT64)(a_L_843))))-((UINT64)(b_L_844))));
#line 468 "Minus.m3"
 /* end_procedure */
#line 468 "Minus.m3"
} /* Minus_param_C_i64 */
#line 468 "Minus.m3"
 /* set_source_line */
#line 468 "Minus.m3"
#line 469 "Minus.m3"
 /* begin_procedure */
#line 469 "Minus.m3"
struct Minus__Minus_param_C_i64_Frame_t {
#line 469 "Minus.m3"
ADDRESS _unused;
#line 469 "Minus.m3"
};
#line 469 "Minus.m3"
LONGINT
__cdecl
Minus__Minus_param_C_i64(
   /* Param_Type1 */ CARDINAL a_L_846,
   /* Param_Type1 */ Minus__INT64 b_L_847)
{
#line 469 "Minus.m3"
Minus__Minus_param_C_i64_Frame_t _frame;
#line 469 "Minus.m3"
_frame._unused=(ADDRESS)&_frame;
#line 469 "Minus.m3"
 /* load */
#line 469 "Minus.m3"
 /* loophole */
#line 469 "Minus.m3"
 /* load */
#line 469 "Minus.m3"
 /* subtract */
#line 469 "Minus.m3"
 /* exit_proc */
#line 469 "Minus.m3"
return ((INT64)(((INT64)((INT64)((INT64)(a_L_846))))- b_L_847));
#line 469 "Minus.m3"
 /* end_procedure */
#line 469 "Minus.m3"
} /* uMinus_var_C_i16 */
#line 469 "Minus.m3"
 /* set_source_line */
#line 469 "Minus.m3"
#line 470 "Minus.m3"
 /* begin_procedure */
#line 470 "Minus.m3"
struct Minus__uMinus_var_C_i16_Frame_t {
#line 470 "Minus.m3"
ADDRESS _unused;
#line 470 "Minus.m3"
};
#line 470 "Minus.m3"
Word__T
__cdecl
Minus__uMinus_var_C_i16(void)
{
#line 470 "Minus.m3"
Minus__uMinus_var_C_i16_Frame_t _frame;
#line 470 "Minus.m3"
_frame._unused=(ADDRESS)&_frame;
#line 470 "Minus.m3"
 /* load */
#line 470 "Minus.m3"
 /* load */
#line 470 "Minus.m3"
 /* subtract */
#line 470 "Minus.m3"
 /* exit_proc */
#line 470 "Minus.m3"
return ((UINT64)(((UINT64)(((INT64)(*((UINT64*)(INT64_(176)+((ADDRESS)(&Minus_m_M_Minus_L_13))))))))-((UINT64)(((INT64)(*((INT16*)(INT64_(172)+((ADDRESS)(&Minus_m_M_Minus_L_13))))))))));
#line 470 "Minus.m3"
 /* end_procedure */
#line 470 "Minus.m3"
} /* Minus_var_C_i16 */
#line 470 "Minus.m3"
 /* set_source_line */
#line 470 "Minus.m3"
#line 471 "Minus.m3"
 /* begin_procedure */
#line 471 "Minus.m3"
struct Minus__Minus_var_C_i16_Frame_t {
#line 471 "Minus.m3"
ADDRESS _unused;
#line 471 "Minus.m3"
};
#line 471 "Minus.m3"
CARDINAL
__cdecl
Minus__Minus_var_C_i16(void)
{
#line 471 "Minus.m3"
 /* Var_Type2 */ INT64 Minus_m_1320_L_1321={0};//always-init
#line 471 "Minus.m3"
Minus__Minus_var_C_i16_Frame_t _frame;
#line 471 "Minus.m3"
_frame._unused=(ADDRESS)&_frame;
#line 471 "Minus.m3"
 /* load */
#line 471 "Minus.m3"
 /* load */
#line 471 "Minus.m3"
 /* subtract */
#line 471 "Minus.m3"
 /* check_lo */
#line 471 "Minus.m3"
 /* store */
#line 471 "Minus.m3"
(*(INT64*)(&Minus_m_1320_L_1321))=(INT64)( ((INT64)( ((INT64)(*((UINT64*)(INT64_(176)+((ADDRESS)(&Minus_m_M_Minus_L_13))))))- ((INT64)(*((INT16*)(INT64_(172)+((ADDRESS)(&Minus_m_M_Minus_L_13)))))))));
#line 471 "Minus.m3"
 /* load */
#line 471 "Minus.m3"
/*check_lo*/if(Minus_m_1320_L_1321<INT64_(0))Minus_m_M_Minus_L_13_CRASH(15073);
#line 471 "Minus.m3"
 /* exit_proc */
#line 471 "Minus.m3"
return Minus_m_1320_L_1321;
#line 471 "Minus.m3"
 /* end_procedure */
#line 471 "Minus.m3"
} /* uMinus_param_C_i16 */
#line 471 "Minus.m3"
 /* set_source_line */
#line 471 "Minus.m3"
#line 472 "Minus.m3"
 /* begin_procedure */
#line 472 "Minus.m3"
struct Minus__uMinus_param_C_i16_Frame_t {
#line 472 "Minus.m3"
ADDRESS _unused;
#line 472 "Minus.m3"
};
#line 472 "Minus.m3"
Word__T
__cdecl
Minus__uMinus_param_C_i16(
   /* Param_Type1 */ CARDINAL a_L_851,
   /* Param_Type1 */ Minus__INT16 b_L_852)
{
#line 472 "Minus.m3"
Minus__uMinus_param_C_i16_Frame_t _frame;
#line 472 "Minus.m3"
_frame._unused=(ADDRESS)&_frame;
#line 472 "Minus.m3"
 /* load */
#line 472 "Minus.m3"
 /* load */
#line 472 "Minus.m3"
 /* subtract */
#line 472 "Minus.m3"
 /* exit_proc */
#line 472 "Minus.m3"
return ((UINT64)(((UINT64)(((INT64)(a_L_851))))-((UINT64)(((INT64)(b_L_852))))));
#line 472 "Minus.m3"
 /* end_procedure */
#line 472 "Minus.m3"
} /* Minus_param_C_i16 */
#line 472 "Minus.m3"
 /* set_source_line */
#line 472 "Minus.m3"
#line 473 "Minus.m3"
 /* begin_procedure */
#line 473 "Minus.m3"
struct Minus__Minus_param_C_i16_Frame_t {
#line 473 "Minus.m3"
ADDRESS _unused;
#line 473 "Minus.m3"
};
#line 473 "Minus.m3"
CARDINAL
__cdecl
Minus__Minus_param_C_i16(
   /* Param_Type1 */ CARDINAL a_L_854,
   /* Param_Type1 */ Minus__INT16 b_L_855)
{
#line 473 "Minus.m3"
 /* Var_Type2 */ INT64 Minus_m_1322_L_1323={0};//always-init
#line 473 "Minus.m3"
Minus__Minus_param_C_i16_Frame_t _frame;
#line 473 "Minus.m3"
_frame._unused=(ADDRESS)&_frame;
#line 473 "Minus.m3"
 /* load */
#line 473 "Minus.m3"
 /* load */
#line 473 "Minus.m3"
 /* subtract */
#line 473 "Minus.m3"
 /* check_lo */
#line 473 "Minus.m3"
 /* store */
#line 473 "Minus.m3"
(*(INT64*)(&Minus_m_1322_L_1323))=(INT64)( ((INT64)( ((INT64)(a_L_854))- ((INT64)(b_L_855)))));
#line 473 "Minus.m3"
 /* load */
#line 473 "Minus.m3"
/*check_lo*/if(Minus_m_1322_L_1323<INT64_(0))Minus_m_M_Minus_L_13_CRASH(15137);
#line 473 "Minus.m3"
 /* exit_proc */
#line 473 "Minus.m3"
return Minus_m_1322_L_1323;
#line 473 "Minus.m3"
 /* end_procedure */
#line 473 "Minus.m3"
} /* uMinus_var_C_C */
#line 473 "Minus.m3"
 /* set_source_line */
#line 473 "Minus.m3"
#line 474 "Minus.m3"
 /* begin_procedure */
#line 474 "Minus.m3"
struct Minus__uMinus_var_C_C_Frame_t {
#line 474 "Minus.m3"
ADDRESS _unused;
#line 474 "Minus.m3"
};
#line 474 "Minus.m3"
Word__T
__cdecl
Minus__uMinus_var_C_C(void)
{
#line 474 "Minus.m3"
Minus__uMinus_var_C_C_Frame_t _frame;
#line 474 "Minus.m3"
_frame._unused=(ADDRESS)&_frame;
#line 474 "Minus.m3"
 /* load */
#line 474 "Minus.m3"
 /* load */
#line 474 "Minus.m3"
 /* subtract */
#line 474 "Minus.m3"
 /* exit_proc */
#line 474 "Minus.m3"
return ((UINT64)(((UINT64)(((INT64)(*((UINT64*)(INT64_(176)+((ADDRESS)(&Minus_m_M_Minus_L_13))))))))-((UINT64)(((INT64)(*((UINT64*)(INT64_(176)+((ADDRESS)(&Minus_m_M_Minus_L_13))))))))));
#line 474 "Minus.m3"
 /* end_procedure */
#line 474 "Minus.m3"
} /* Minus_var_C_C */
#line 474 "Minus.m3"
 /* set_source_line */
#line 474 "Minus.m3"
#line 475 "Minus.m3"
 /* begin_procedure */
#line 475 "Minus.m3"
struct Minus__Minus_var_C_C_Frame_t {
#line 475 "Minus.m3"
ADDRESS _unused;
#line 475 "Minus.m3"
};
#line 475 "Minus.m3"
CARDINAL
__cdecl
Minus__Minus_var_C_C(void)
{
#line 475 "Minus.m3"
 /* Var_Type2 */ INT64 Minus_m_1324_L_1325={0};//always-init
#line 475 "Minus.m3"
Minus__Minus_var_C_C_Frame_t _frame;
#line 475 "Minus.m3"
_frame._unused=(ADDRESS)&_frame;
#line 475 "Minus.m3"
 /* load */
#line 475 "Minus.m3"
 /* load */
#line 475 "Minus.m3"
 /* subtract */
#line 475 "Minus.m3"
 /* check_lo */
#line 475 "Minus.m3"
 /* store */
#line 475 "Minus.m3"
(*(INT64*)(&Minus_m_1324_L_1325))=(INT64)( ((INT64)( ((INT64)(*((UINT64*)(INT64_(176)+((ADDRESS)(&Minus_m_M_Minus_L_13))))))- ((INT64)(*((UINT64*)(INT64_(176)+((ADDRESS)(&Minus_m_M_Minus_L_13)))))))));
#line 475 "Minus.m3"
 /* load */
#line 475 "Minus.m3"
/*check_lo*/if(Minus_m_1324_L_1325<INT64_(0))Minus_m_M_Minus_L_13_CRASH(15201);
#line 475 "Minus.m3"
 /* exit_proc */
#line 475 "Minus.m3"
return Minus_m_1324_L_1325;
#line 475 "Minus.m3"
 /* end_procedure */
#line 475 "Minus.m3"
} /* uMinus_param_C_C */
#line 475 "Minus.m3"
 /* set_source_line */
#line 475 "Minus.m3"
#line 476 "Minus.m3"
 /* begin_procedure */
#line 476 "Minus.m3"
struct Minus__uMinus_param_C_C_Frame_t {
#line 476 "Minus.m3"
ADDRESS _unused;
#line 476 "Minus.m3"
};
#line 476 "Minus.m3"
Word__T
__cdecl
Minus__uMinus_param_C_C(
   /* Param_Type1 */ CARDINAL a_L_859,
   /* Param_Type1 */ CARDINAL b_L_860)
{
#line 476 "Minus.m3"
Minus__uMinus_param_C_C_Frame_t _frame;
#line 476 "Minus.m3"
_frame._unused=(ADDRESS)&_frame;
#line 476 "Minus.m3"
 /* load */
#line 476 "Minus.m3"
 /* load */
#line 476 "Minus.m3"
 /* subtract */
#line 476 "Minus.m3"
 /* exit_proc */
#line 476 "Minus.m3"
return ((UINT64)(((UINT64)(((INT64)(a_L_859))))-((UINT64)(((INT64)(b_L_860))))));
#line 476 "Minus.m3"
 /* end_procedure */
#line 476 "Minus.m3"
} /* Minus_param_C_C */
#line 476 "Minus.m3"
 /* set_source_line */
#line 476 "Minus.m3"
#line 477 "Minus.m3"
 /* begin_procedure */
#line 477 "Minus.m3"
struct Minus__Minus_param_C_C_Frame_t {
#line 477 "Minus.m3"
ADDRESS _unused;
#line 477 "Minus.m3"
};
#line 477 "Minus.m3"
CARDINAL
__cdecl
Minus__Minus_param_C_C(
   /* Param_Type1 */ CARDINAL a_L_862,
   /* Param_Type1 */ CARDINAL b_L_863)
{
#line 477 "Minus.m3"
 /* Var_Type2 */ INT64 Minus_m_1326_L_1327={0};//always-init
#line 477 "Minus.m3"
Minus__Minus_param_C_C_Frame_t _frame;
#line 477 "Minus.m3"
_frame._unused=(ADDRESS)&_frame;
#line 477 "Minus.m3"
 /* load */
#line 477 "Minus.m3"
 /* load */
#line 477 "Minus.m3"
 /* subtract */
#line 477 "Minus.m3"
 /* check_lo */
#line 477 "Minus.m3"
 /* store */
#line 477 "Minus.m3"
(*(INT64*)(&Minus_m_1326_L_1327))=(INT64)( ((INT64)( ((INT64)(a_L_862))- ((INT64)(b_L_863)))));
#line 477 "Minus.m3"
 /* load */
#line 477 "Minus.m3"
/*check_lo*/if(Minus_m_1326_L_1327<INT64_(0))Minus_m_M_Minus_L_13_CRASH(15265);
#line 477 "Minus.m3"
 /* exit_proc */
#line 477 "Minus.m3"
return Minus_m_1326_L_1327;
#line 477 "Minus.m3"
 /* end_procedure */
#line 477 "Minus.m3"
} /* uMinus_var_C_u32 */
#line 477 "Minus.m3"
 /* set_source_line */
#line 477 "Minus.m3"
#line 478 "Minus.m3"
 /* begin_procedure */
#line 478 "Minus.m3"
struct Minus__uMinus_var_C_u32_Frame_t {
#line 478 "Minus.m3"
ADDRESS _unused;
#line 478 "Minus.m3"
};
#line 478 "Minus.m3"
Word__T
__cdecl
Minus__uMinus_var_C_u32(void)
{
#line 478 "Minus.m3"
Minus__uMinus_var_C_u32_Frame_t _frame;
#line 478 "Minus.m3"
_frame._unused=(ADDRESS)&_frame;
#line 478 "Minus.m3"
 /* load */
#line 478 "Minus.m3"
 /* load */
#line 478 "Minus.m3"
 /* subtract */
#line 478 "Minus.m3"
 /* exit_proc */
#line 478 "Minus.m3"
return ((UINT64)(((UINT64)(((INT64)(*((UINT64*)(INT64_(176)+((ADDRESS)(&Minus_m_M_Minus_L_13))))))))-((UINT64)(((INT64)(*((UINT32*)(INT64_(184)+((ADDRESS)(&Minus_m_M_Minus_L_13))))))))));
#line 478 "Minus.m3"
 /* end_procedure */
#line 478 "Minus.m3"
} /* Minus_var_C_u32 */
#line 478 "Minus.m3"
 /* set_source_line */
#line 478 "Minus.m3"
#line 479 "Minus.m3"
 /* begin_procedure */
#line 479 "Minus.m3"
struct Minus__Minus_var_C_u32_Frame_t {
#line 479 "Minus.m3"
ADDRESS _unused;
#line 479 "Minus.m3"
};
#line 479 "Minus.m3"
CARDINAL
__cdecl
Minus__Minus_var_C_u32(void)
{
#line 479 "Minus.m3"
 /* Var_Type2 */ INT64 Minus_m_1328_L_1329={0};//always-init
#line 479 "Minus.m3"
Minus__Minus_var_C_u32_Frame_t _frame;
#line 479 "Minus.m3"
_frame._unused=(ADDRESS)&_frame;
#line 479 "Minus.m3"
 /* load */
#line 479 "Minus.m3"
 /* load */
#line 479 "Minus.m3"
 /* subtract */
#line 479 "Minus.m3"
 /* check_lo */
#line 479 "Minus.m3"
 /* store */
#line 479 "Minus.m3"
(*(INT64*)(&Minus_m_1328_L_1329))=(INT64)( ((INT64)( ((INT64)(*((UINT64*)(INT64_(176)+((ADDRESS)(&Minus_m_M_Minus_L_13))))))- ((INT64)(*((UINT32*)(INT64_(184)+((ADDRESS)(&Minus_m_M_Minus_L_13)))))))));
#line 479 "Minus.m3"
 /* load */
#line 479 "Minus.m3"
/*check_lo*/if(Minus_m_1328_L_1329<INT64_(0))Minus_m_M_Minus_L_13_CRASH(15329);
#line 479 "Minus.m3"
 /* exit_proc */
#line 479 "Minus.m3"
return Minus_m_1328_L_1329;
#line 479 "Minus.m3"
 /* end_procedure */
#line 479 "Minus.m3"
} /* uMinus_param_C_u32 */
#line 479 "Minus.m3"
 /* set_source_line */
#line 479 "Minus.m3"
#line 480 "Minus.m3"
 /* begin_procedure */
#line 480 "Minus.m3"
struct Minus__uMinus_param_C_u32_Frame_t {
#line 480 "Minus.m3"
ADDRESS _unused;
#line 480 "Minus.m3"
};
#line 480 "Minus.m3"
Word__T
__cdecl
Minus__uMinus_param_C_u32(
   /* Param_Type1 */ CARDINAL a_L_867,
   /* Param_Type1 */ Minus__UINT32 b_L_868)
{
#line 480 "Minus.m3"
Minus__uMinus_param_C_u32_Frame_t _frame;
#line 480 "Minus.m3"
_frame._unused=(ADDRESS)&_frame;
#line 480 "Minus.m3"
 /* load */
#line 480 "Minus.m3"
 /* load */
#line 480 "Minus.m3"
 /* subtract */
#line 480 "Minus.m3"
 /* exit_proc */
#line 480 "Minus.m3"
return ((UINT64)(((UINT64)(((INT64)(a_L_867))))-((UINT64)(((INT64)(b_L_868))))));
#line 480 "Minus.m3"
 /* end_procedure */
#line 480 "Minus.m3"
} /* Minus_param_C_u32 */
#line 480 "Minus.m3"
 /* set_source_line */
#line 480 "Minus.m3"
#line 481 "Minus.m3"
 /* begin_procedure */
#line 481 "Minus.m3"
struct Minus__Minus_param_C_u32_Frame_t {
#line 481 "Minus.m3"
ADDRESS _unused;
#line 481 "Minus.m3"
};
#line 481 "Minus.m3"
CARDINAL
__cdecl
Minus__Minus_param_C_u32(
   /* Param_Type1 */ CARDINAL a_L_870,
   /* Param_Type1 */ Minus__UINT32 b_L_871)
{
#line 481 "Minus.m3"
 /* Var_Type2 */ INT64 Minus_m_1330_L_1331={0};//always-init
#line 481 "Minus.m3"
Minus__Minus_param_C_u32_Frame_t _frame;
#line 481 "Minus.m3"
_frame._unused=(ADDRESS)&_frame;
#line 481 "Minus.m3"
 /* load */
#line 481 "Minus.m3"
 /* load */
#line 481 "Minus.m3"
 /* subtract */
#line 481 "Minus.m3"
 /* check_lo */
#line 481 "Minus.m3"
 /* store */
#line 481 "Minus.m3"
(*(INT64*)(&Minus_m_1330_L_1331))=(INT64)( ((INT64)( ((INT64)(a_L_870))- ((INT64)(b_L_871)))));
#line 481 "Minus.m3"
 /* load */
#line 481 "Minus.m3"
/*check_lo*/if(Minus_m_1330_L_1331<INT64_(0))Minus_m_M_Minus_L_13_CRASH(15393);
#line 481 "Minus.m3"
 /* exit_proc */
#line 481 "Minus.m3"
return Minus_m_1330_L_1331;
#line 481 "Minus.m3"
 /* end_procedure */
#line 481 "Minus.m3"
} /* uMinus_var_C_u8 */
#line 481 "Minus.m3"
 /* set_source_line */
#line 481 "Minus.m3"
#line 482 "Minus.m3"
 /* begin_procedure */
#line 482 "Minus.m3"
struct Minus__uMinus_var_C_u8_Frame_t {
#line 482 "Minus.m3"
ADDRESS _unused;
#line 482 "Minus.m3"
};
#line 482 "Minus.m3"
Word__T
__cdecl
Minus__uMinus_var_C_u8(void)
{
#line 482 "Minus.m3"
Minus__uMinus_var_C_u8_Frame_t _frame;
#line 482 "Minus.m3"
_frame._unused=(ADDRESS)&_frame;
#line 482 "Minus.m3"
 /* load */
#line 482 "Minus.m3"
 /* load */
#line 482 "Minus.m3"
 /* subtract */
#line 482 "Minus.m3"
 /* exit_proc */
#line 482 "Minus.m3"
return ((UINT64)(((UINT64)(((INT64)(*((UINT64*)(INT64_(176)+((ADDRESS)(&Minus_m_M_Minus_L_13))))))))-((UINT64)(((INT64)(*((UINT8*)(INT64_(188)+((ADDRESS)(&Minus_m_M_Minus_L_13))))))))));
#line 482 "Minus.m3"
 /* end_procedure */
#line 482 "Minus.m3"
} /* Minus_var_C_u8 */
#line 482 "Minus.m3"
 /* set_source_line */
#line 482 "Minus.m3"
#line 483 "Minus.m3"
 /* begin_procedure */
#line 483 "Minus.m3"
struct Minus__Minus_var_C_u8_Frame_t {
#line 483 "Minus.m3"
ADDRESS _unused;
#line 483 "Minus.m3"
};
#line 483 "Minus.m3"
CARDINAL
__cdecl
Minus__Minus_var_C_u8(void)
{
#line 483 "Minus.m3"
 /* Var_Type2 */ INT64 Minus_m_1332_L_1333={0};//always-init
#line 483 "Minus.m3"
Minus__Minus_var_C_u8_Frame_t _frame;
#line 483 "Minus.m3"
_frame._unused=(ADDRESS)&_frame;
#line 483 "Minus.m3"
 /* load */
#line 483 "Minus.m3"
 /* load */
#line 483 "Minus.m3"
 /* subtract */
#line 483 "Minus.m3"
 /* check_lo */
#line 483 "Minus.m3"
 /* store */
#line 483 "Minus.m3"
(*(INT64*)(&Minus_m_1332_L_1333))=(INT64)( ((INT64)( ((INT64)(*((UINT64*)(INT64_(176)+((ADDRESS)(&Minus_m_M_Minus_L_13))))))- ((INT64)(*((UINT8*)(INT64_(188)+((ADDRESS)(&Minus_m_M_Minus_L_13)))))))));
#line 483 "Minus.m3"
 /* load */
#line 483 "Minus.m3"
/*check_lo*/if(Minus_m_1332_L_1333<INT64_(0))Minus_m_M_Minus_L_13_CRASH(15457);
#line 483 "Minus.m3"
 /* exit_proc */
#line 483 "Minus.m3"
return Minus_m_1332_L_1333;
#line 483 "Minus.m3"
 /* end_procedure */
#line 483 "Minus.m3"
} /* uMinus_param_C_u8 */
#line 483 "Minus.m3"
 /* set_source_line */
#line 483 "Minus.m3"
#line 484 "Minus.m3"
 /* begin_procedure */
#line 484 "Minus.m3"
struct Minus__uMinus_param_C_u8_Frame_t {
#line 484 "Minus.m3"
ADDRESS _unused;
#line 484 "Minus.m3"
};
#line 484 "Minus.m3"
Word__T
__cdecl
Minus__uMinus_param_C_u8(
   /* Param_Type1 */ CARDINAL a_L_875,
   /* Param_Type1 */ Minus__UINT8 b_L_876)
{
#line 484 "Minus.m3"
Minus__uMinus_param_C_u8_Frame_t _frame;
#line 484 "Minus.m3"
_frame._unused=(ADDRESS)&_frame;
#line 484 "Minus.m3"
 /* load */
#line 484 "Minus.m3"
 /* load */
#line 484 "Minus.m3"
 /* subtract */
#line 484 "Minus.m3"
 /* exit_proc */
#line 484 "Minus.m3"
return ((UINT64)(((UINT64)(((INT64)(a_L_875))))-((UINT64)(((INT64)(b_L_876))))));
#line 484 "Minus.m3"
 /* end_procedure */
#line 484 "Minus.m3"
} /* Minus_param_C_u8 */
#line 484 "Minus.m3"
 /* set_source_line */
#line 484 "Minus.m3"
#line 485 "Minus.m3"
 /* begin_procedure */
#line 485 "Minus.m3"
struct Minus__Minus_param_C_u8_Frame_t {
#line 485 "Minus.m3"
ADDRESS _unused;
#line 485 "Minus.m3"
};
#line 485 "Minus.m3"
CARDINAL
__cdecl
Minus__Minus_param_C_u8(
   /* Param_Type1 */ CARDINAL a_L_878,
   /* Param_Type1 */ Minus__UINT8 b_L_879)
{
#line 485 "Minus.m3"
 /* Var_Type2 */ INT64 Minus_m_1334_L_1335={0};//always-init
#line 485 "Minus.m3"
Minus__Minus_param_C_u8_Frame_t _frame;
#line 485 "Minus.m3"
_frame._unused=(ADDRESS)&_frame;
#line 485 "Minus.m3"
 /* load */
#line 485 "Minus.m3"
 /* load */
#line 485 "Minus.m3"
 /* subtract */
#line 485 "Minus.m3"
 /* check_lo */
#line 485 "Minus.m3"
 /* store */
#line 485 "Minus.m3"
(*(INT64*)(&Minus_m_1334_L_1335))=(INT64)( ((INT64)( ((INT64)(a_L_878))- ((INT64)(b_L_879)))));
#line 485 "Minus.m3"
 /* load */
#line 485 "Minus.m3"
/*check_lo*/if(Minus_m_1334_L_1335<INT64_(0))Minus_m_M_Minus_L_13_CRASH(15521);
#line 485 "Minus.m3"
 /* exit_proc */
#line 485 "Minus.m3"
return Minus_m_1334_L_1335;
#line 485 "Minus.m3"
 /* end_procedure */
#line 485 "Minus.m3"
} /* uMinus_var_C_L */
#line 485 "Minus.m3"
 /* set_source_line */
#line 485 "Minus.m3"
#line 486 "Minus.m3"
 /* begin_procedure */
#line 486 "Minus.m3"
struct Minus__uMinus_var_C_L_Frame_t {
#line 486 "Minus.m3"
ADDRESS _unused;
#line 486 "Minus.m3"
};
#line 486 "Minus.m3"
Long__T
__cdecl
Minus__uMinus_var_C_L(void)
{
#line 486 "Minus.m3"
Minus__uMinus_var_C_L_Frame_t _frame;
#line 486 "Minus.m3"
_frame._unused=(ADDRESS)&_frame;
#line 486 "Minus.m3"
 /* load */
#line 486 "Minus.m3"
 /* loophole */
#line 486 "Minus.m3"
 /* load */
#line 486 "Minus.m3"
 /* subtract */
#line 486 "Minus.m3"
 /* exit_proc */
#line 486 "Minus.m3"
return ((UINT64)(((UINT64)((INT64)((INT64)(*((UINT64*)(INT64_(176)+((ADDRESS)(&Minus_m_M_Minus_L_13))))))))-((UINT64)(*((INT64*)(INT64_(192)+((ADDRESS)(&Minus_m_M_Minus_L_13))))))));
#line 486 "Minus.m3"
 /* end_procedure */
#line 486 "Minus.m3"
} /* Minus_var_C_L */
#line 486 "Minus.m3"
 /* set_source_line */
#line 486 "Minus.m3"
#line 487 "Minus.m3"
 /* begin_procedure */
#line 487 "Minus.m3"
struct Minus__Minus_var_C_L_Frame_t {
#line 487 "Minus.m3"
ADDRESS _unused;
#line 487 "Minus.m3"
};
#line 487 "Minus.m3"
LONGINT
__cdecl
Minus__Minus_var_C_L(void)
{
#line 487 "Minus.m3"
Minus__Minus_var_C_L_Frame_t _frame;
#line 487 "Minus.m3"
_frame._unused=(ADDRESS)&_frame;
#line 487 "Minus.m3"
 /* load */
#line 487 "Minus.m3"
 /* loophole */
#line 487 "Minus.m3"
 /* load */
#line 487 "Minus.m3"
 /* subtract */
#line 487 "Minus.m3"
 /* exit_proc */
#line 487 "Minus.m3"
return ((INT64)(((INT64)((INT64)((INT64)(*((UINT64*)(INT64_(176)+((ADDRESS)(&Minus_m_M_Minus_L_13))))))))-((INT64)(*((INT64*)(INT64_(192)+((ADDRESS)(&Minus_m_M_Minus_L_13))))))));
#line 487 "Minus.m3"
 /* end_procedure */
#line 487 "Minus.m3"
} /* uMinus_param_C_L */
#line 487 "Minus.m3"
 /* set_source_line */
#line 487 "Minus.m3"
#line 488 "Minus.m3"
 /* begin_procedure */
#line 488 "Minus.m3"
struct Minus__uMinus_param_C_L_Frame_t {
#line 488 "Minus.m3"
ADDRESS _unused;
#line 488 "Minus.m3"
};
#line 488 "Minus.m3"
Long__T
__cdecl
Minus__uMinus_param_C_L(
   /* Param_Type1 */ CARDINAL a_L_883,
   /* Param_Type1 */ LONGINT b_L_884)
{
#line 488 "Minus.m3"
Minus__uMinus_param_C_L_Frame_t _frame;
#line 488 "Minus.m3"
_frame._unused=(ADDRESS)&_frame;
#line 488 "Minus.m3"
 /* load */
#line 488 "Minus.m3"
 /* loophole */
#line 488 "Minus.m3"
 /* load */
#line 488 "Minus.m3"
 /* subtract */
#line 488 "Minus.m3"
 /* exit_proc */
#line 488 "Minus.m3"
return ((UINT64)(((UINT64)((INT64)((INT64)(a_L_883))))-((UINT64)(b_L_884))));
#line 488 "Minus.m3"
 /* end_procedure */
#line 488 "Minus.m3"
} /* Minus_param_C_L */
#line 488 "Minus.m3"
 /* set_source_line */
#line 488 "Minus.m3"
#line 489 "Minus.m3"
 /* begin_procedure */
#line 489 "Minus.m3"
struct Minus__Minus_param_C_L_Frame_t {
#line 489 "Minus.m3"
ADDRESS _unused;
#line 489 "Minus.m3"
};
#line 489 "Minus.m3"
LONGINT
__cdecl
Minus__Minus_param_C_L(
   /* Param_Type1 */ CARDINAL a_L_886,
   /* Param_Type1 */ LONGINT b_L_887)
{
#line 489 "Minus.m3"
Minus__Minus_param_C_L_Frame_t _frame;
#line 489 "Minus.m3"
_frame._unused=(ADDRESS)&_frame;
#line 489 "Minus.m3"
 /* load */
#line 489 "Minus.m3"
 /* loophole */
#line 489 "Minus.m3"
 /* load */
#line 489 "Minus.m3"
 /* subtract */
#line 489 "Minus.m3"
 /* exit_proc */
#line 489 "Minus.m3"
return ((INT64)(((INT64)((INT64)((INT64)(a_L_886))))- b_L_887));
#line 489 "Minus.m3"
 /* end_procedure */
#line 489 "Minus.m3"
} /* uMinus_var_u32_i8 */
#line 489 "Minus.m3"
 /* set_source_line */
#line 489 "Minus.m3"
#line 490 "Minus.m3"
 /* begin_procedure */
#line 490 "Minus.m3"
struct Minus__uMinus_var_u32_i8_Frame_t {
#line 490 "Minus.m3"
ADDRESS _unused;
#line 490 "Minus.m3"
};
#line 490 "Minus.m3"
Word__T
__cdecl
Minus__uMinus_var_u32_i8(void)
{
#line 490 "Minus.m3"
Minus__uMinus_var_u32_i8_Frame_t _frame;
#line 490 "Minus.m3"
_frame._unused=(ADDRESS)&_frame;
#line 490 "Minus.m3"
 /* load */
#line 490 "Minus.m3"
 /* load */
#line 490 "Minus.m3"
 /* subtract */
#line 490 "Minus.m3"
 /* exit_proc */
#line 490 "Minus.m3"
return ((UINT64)(((UINT64)(((INT64)(*((UINT32*)(INT64_(184)+((ADDRESS)(&Minus_m_M_Minus_L_13))))))))-((UINT64)(((INT64)(*((INT8*)(INT64_(104)+((ADDRESS)(&Minus_m_M_Minus_L_13))))))))));
#line 490 "Minus.m3"
 /* end_procedure */
#line 490 "Minus.m3"
} /* Minus_var_u32_i8 */
#line 490 "Minus.m3"
 /* set_source_line */
#line 490 "Minus.m3"
#line 491 "Minus.m3"
 /* begin_procedure */
#line 491 "Minus.m3"
struct Minus__Minus_var_u32_i8_Frame_t {
#line 491 "Minus.m3"
ADDRESS _unused;
#line 491 "Minus.m3"
};
#line 491 "Minus.m3"
Minus__UINT32
__cdecl
Minus__Minus_var_u32_i8(void)
{
#line 491 "Minus.m3"
 /* Var_Type2 */ INT64 Minus_m_1336_L_1337={0};//always-init
#line 491 "Minus.m3"
Minus__Minus_var_u32_i8_Frame_t _frame;
#line 491 "Minus.m3"
_frame._unused=(ADDRESS)&_frame;
#line 491 "Minus.m3"
 /* load */
#line 491 "Minus.m3"
 /* load */
#line 491 "Minus.m3"
 /* subtract */
#line 491 "Minus.m3"
 /* check_range */
#line 491 "Minus.m3"
 /* store */
#line 491 "Minus.m3"
(*(INT64*)(&Minus_m_1336_L_1337))=(INT64)( ((INT64)( ((INT64)(*((UINT32*)(INT64_(184)+((ADDRESS)(&Minus_m_M_Minus_L_13))))))- ((INT64)(*((INT8*)(INT64_(104)+((ADDRESS)(&Minus_m_M_Minus_L_13)))))))));
#line 491 "Minus.m3"
 /* load */
#line 491 "Minus.m3"
if(m3_check_range(INT64,
Minus_m_1336_L_1337,
 INT64_(0),
 INT64_(4294967295)))
#line 491 "Minus.m3"
Minus_m_M_Minus_L_13_CRASH(15713);
#line 491 "Minus.m3"
 /* exit_proc */
#line 491 "Minus.m3"
return Minus_m_1336_L_1337;
#line 491 "Minus.m3"
 /* end_procedure */
#line 491 "Minus.m3"
} /* uMinus_param_u32_i8 */
#line 491 "Minus.m3"
 /* set_source_line */
#line 491 "Minus.m3"
#line 492 "Minus.m3"
 /* begin_procedure */
#line 492 "Minus.m3"
struct Minus__uMinus_param_u32_i8_Frame_t {
#line 492 "Minus.m3"
ADDRESS _unused;
#line 492 "Minus.m3"
};
#line 492 "Minus.m3"
Word__T
__cdecl
Minus__uMinus_param_u32_i8(
   /* Param_Type1 */ Minus__UINT32 a_L_891,
   /* Param_Type1 */ Minus__INT8 b_L_892)
{
#line 492 "Minus.m3"
Minus__uMinus_param_u32_i8_Frame_t _frame;
#line 492 "Minus.m3"
_frame._unused=(ADDRESS)&_frame;
#line 492 "Minus.m3"
 /* load */
#line 492 "Minus.m3"
 /* load */
#line 492 "Minus.m3"
 /* subtract */
#line 492 "Minus.m3"
 /* exit_proc */
#line 492 "Minus.m3"
return ((UINT64)(((UINT64)(((INT64)(a_L_891))))-((UINT64)(((INT64)(b_L_892))))));
#line 492 "Minus.m3"
 /* end_procedure */
#line 492 "Minus.m3"
} /* Minus_param_u32_i8 */
#line 492 "Minus.m3"
 /* set_source_line */
#line 492 "Minus.m3"
#line 493 "Minus.m3"
 /* begin_procedure */
#line 493 "Minus.m3"
struct Minus__Minus_param_u32_i8_Frame_t {
#line 493 "Minus.m3"
ADDRESS _unused;
#line 493 "Minus.m3"
};
#line 493 "Minus.m3"
Minus__UINT32
__cdecl
Minus__Minus_param_u32_i8(
   /* Param_Type1 */ Minus__UINT32 a_L_894,
   /* Param_Type1 */ Minus__INT8 b_L_895)
{
#line 493 "Minus.m3"
 /* Var_Type2 */ INT64 Minus_m_1338_L_1339={0};//always-init
#line 493 "Minus.m3"
Minus__Minus_param_u32_i8_Frame_t _frame;
#line 493 "Minus.m3"
_frame._unused=(ADDRESS)&_frame;
#line 493 "Minus.m3"
 /* load */
#line 493 "Minus.m3"
 /* load */
#line 493 "Minus.m3"
 /* subtract */
#line 493 "Minus.m3"
 /* check_range */
#line 493 "Minus.m3"
 /* store */
#line 493 "Minus.m3"
(*(INT64*)(&Minus_m_1338_L_1339))=(INT64)( ((INT64)( ((INT64)(a_L_894))- ((INT64)(b_L_895)))));
#line 493 "Minus.m3"
 /* load */
#line 493 "Minus.m3"
if(m3_check_range(INT64,
Minus_m_1338_L_1339,
 INT64_(0),
 INT64_(4294967295)))
#line 493 "Minus.m3"
Minus_m_M_Minus_L_13_CRASH(15777);
#line 493 "Minus.m3"
 /* exit_proc */
#line 493 "Minus.m3"
return Minus_m_1338_L_1339;
#line 493 "Minus.m3"
 /* end_procedure */
#line 493 "Minus.m3"
} /* uMinus_var_u32_u64 */
#line 493 "Minus.m3"
 /* set_source_line */
#line 493 "Minus.m3"
#line 494 "Minus.m3"
 /* begin_procedure */
#line 494 "Minus.m3"
struct Minus__uMinus_var_u32_u64_Frame_t {
#line 494 "Minus.m3"
ADDRESS _unused;
#line 494 "Minus.m3"
};
#line 494 "Minus.m3"
Long__T
__cdecl
Minus__uMinus_var_u32_u64(void)
{
#line 494 "Minus.m3"
Minus__uMinus_var_u32_u64_Frame_t _frame;
#line 494 "Minus.m3"
_frame._unused=(ADDRESS)&_frame;
#line 494 "Minus.m3"
 /* load */
#line 494 "Minus.m3"
 /* loophole */
#line 494 "Minus.m3"
 /* load */
#line 494 "Minus.m3"
 /* subtract */
#line 494 "Minus.m3"
 /* exit_proc */
#line 494 "Minus.m3"
return ((UINT64)(((UINT64)((INT64)((INT64)(*((UINT32*)(INT64_(184)+((ADDRESS)(&Minus_m_M_Minus_L_13))))))))-((UINT64)(*((INT64*)(INT64_(112)+((ADDRESS)(&Minus_m_M_Minus_L_13))))))));
#line 494 "Minus.m3"
 /* end_procedure */
#line 494 "Minus.m3"
} /* Minus_var_u32_u64 */
#line 494 "Minus.m3"
 /* set_source_line */
#line 494 "Minus.m3"
#line 495 "Minus.m3"
 /* begin_procedure */
#line 495 "Minus.m3"
struct Minus__Minus_var_u32_u64_Frame_t {
#line 495 "Minus.m3"
ADDRESS _unused;
#line 495 "Minus.m3"
};
#line 495 "Minus.m3"
LONGINT
__cdecl
Minus__Minus_var_u32_u64(void)
{
#line 495 "Minus.m3"
Minus__Minus_var_u32_u64_Frame_t _frame;
#line 495 "Minus.m3"
_frame._unused=(ADDRESS)&_frame;
#line 495 "Minus.m3"
 /* load */
#line 495 "Minus.m3"
 /* loophole */
#line 495 "Minus.m3"
 /* load */
#line 495 "Minus.m3"
 /* subtract */
#line 495 "Minus.m3"
 /* exit_proc */
#line 495 "Minus.m3"
return ((INT64)(((INT64)((INT64)((INT64)(*((UINT32*)(INT64_(184)+((ADDRESS)(&Minus_m_M_Minus_L_13))))))))-((INT64)(*((INT64*)(INT64_(112)+((ADDRESS)(&Minus_m_M_Minus_L_13))))))));
#line 495 "Minus.m3"
 /* end_procedure */
#line 495 "Minus.m3"
} /* uMinus_param_u32_u64 */
#line 495 "Minus.m3"
 /* set_source_line */
#line 495 "Minus.m3"
#line 496 "Minus.m3"
 /* begin_procedure */
#line 496 "Minus.m3"
struct Minus__uMinus_param_u32_u64_Frame_t {
#line 496 "Minus.m3"
ADDRESS _unused;
#line 496 "Minus.m3"
};
#line 496 "Minus.m3"
Long__T
__cdecl
Minus__uMinus_param_u32_u64(
   /* Param_Type1 */ Minus__UINT32 a_L_899,
   /* Param_Type1 */ Minus__UINT64 b_L_900)
{
#line 496 "Minus.m3"
Minus__uMinus_param_u32_u64_Frame_t _frame;
#line 496 "Minus.m3"
_frame._unused=(ADDRESS)&_frame;
#line 496 "Minus.m3"
 /* load */
#line 496 "Minus.m3"
 /* loophole */
#line 496 "Minus.m3"
 /* load */
#line 496 "Minus.m3"
 /* subtract */
#line 496 "Minus.m3"
 /* exit_proc */
#line 496 "Minus.m3"
return ((UINT64)(((UINT64)((INT64)((INT64)(a_L_899))))-((UINT64)(b_L_900))));
#line 496 "Minus.m3"
 /* end_procedure */
#line 496 "Minus.m3"
} /* Minus_param_u32_u64 */
#line 496 "Minus.m3"
 /* set_source_line */
#line 496 "Minus.m3"
#line 497 "Minus.m3"
 /* begin_procedure */
#line 497 "Minus.m3"
struct Minus__Minus_param_u32_u64_Frame_t {
#line 497 "Minus.m3"
ADDRESS _unused;
#line 497 "Minus.m3"
};
#line 497 "Minus.m3"
LONGINT
__cdecl
Minus__Minus_param_u32_u64(
   /* Param_Type1 */ Minus__UINT32 a_L_902,
   /* Param_Type1 */ Minus__UINT64 b_L_903)
{
#line 497 "Minus.m3"
Minus__Minus_param_u32_u64_Frame_t _frame;
#line 497 "Minus.m3"
_frame._unused=(ADDRESS)&_frame;
#line 497 "Minus.m3"
 /* load */
#line 497 "Minus.m3"
 /* loophole */
#line 497 "Minus.m3"
 /* load */
#line 497 "Minus.m3"
 /* subtract */
#line 497 "Minus.m3"
 /* exit_proc */
#line 497 "Minus.m3"
return ((INT64)(((INT64)((INT64)((INT64)(a_L_902))))- b_L_903));
#line 497 "Minus.m3"
 /* end_procedure */
#line 497 "Minus.m3"
} /* uMinus_var_u32_i32 */
#line 497 "Minus.m3"
 /* set_source_line */
#line 497 "Minus.m3"
#line 498 "Minus.m3"
 /* begin_procedure */
#line 498 "Minus.m3"
struct Minus__uMinus_var_u32_i32_Frame_t {
#line 498 "Minus.m3"
ADDRESS _unused;
#line 498 "Minus.m3"
};
#line 498 "Minus.m3"
Word__T
__cdecl
Minus__uMinus_var_u32_i32(void)
{
#line 498 "Minus.m3"
Minus__uMinus_var_u32_i32_Frame_t _frame;
#line 498 "Minus.m3"
_frame._unused=(ADDRESS)&_frame;
#line 498 "Minus.m3"
 /* load */
#line 498 "Minus.m3"
 /* load */
#line 498 "Minus.m3"
 /* subtract */
#line 498 "Minus.m3"
 /* exit_proc */
#line 498 "Minus.m3"
return ((UINT64)(((UINT64)(((INT64)(*((UINT32*)(INT64_(184)+((ADDRESS)(&Minus_m_M_Minus_L_13))))))))-((UINT64)(((INT64)(*((INT32*)(INT64_(128)+((ADDRESS)(&Minus_m_M_Minus_L_13))))))))));
#line 498 "Minus.m3"
 /* end_procedure */
#line 498 "Minus.m3"
} /* Minus_var_u32_i32 */
#line 498 "Minus.m3"
 /* set_source_line */
#line 498 "Minus.m3"
#line 499 "Minus.m3"
 /* begin_procedure */
#line 499 "Minus.m3"
struct Minus__Minus_var_u32_i32_Frame_t {
#line 499 "Minus.m3"
ADDRESS _unused;
#line 499 "Minus.m3"
};
#line 499 "Minus.m3"
Minus__UINT32
__cdecl
Minus__Minus_var_u32_i32(void)
{
#line 499 "Minus.m3"
 /* Var_Type2 */ INT64 Minus_m_1340_L_1341={0};//always-init
#line 499 "Minus.m3"
Minus__Minus_var_u32_i32_Frame_t _frame;
#line 499 "Minus.m3"
_frame._unused=(ADDRESS)&_frame;
#line 499 "Minus.m3"
 /* load */
#line 499 "Minus.m3"
 /* load */
#line 499 "Minus.m3"
 /* subtract */
#line 499 "Minus.m3"
 /* check_range */
#line 499 "Minus.m3"
 /* store */
#line 499 "Minus.m3"
(*(INT64*)(&Minus_m_1340_L_1341))=(INT64)( ((INT64)( ((INT64)(*((UINT32*)(INT64_(184)+((ADDRESS)(&Minus_m_M_Minus_L_13))))))- ((INT64)(*((INT32*)(INT64_(128)+((ADDRESS)(&Minus_m_M_Minus_L_13)))))))));
#line 499 "Minus.m3"
 /* load */
#line 499 "Minus.m3"
if(m3_check_range(INT64,
Minus_m_1340_L_1341,
 INT64_(0),
 INT64_(4294967295)))
#line 499 "Minus.m3"
Minus_m_M_Minus_L_13_CRASH(15969);
#line 499 "Minus.m3"
 /* exit_proc */
#line 499 "Minus.m3"
return Minus_m_1340_L_1341;
#line 499 "Minus.m3"
 /* end_procedure */
#line 499 "Minus.m3"
} /* uMinus_param_u32_i32 */
#line 499 "Minus.m3"
 /* set_source_line */
#line 499 "Minus.m3"
#line 500 "Minus.m3"
 /* begin_procedure */
#line 500 "Minus.m3"
struct Minus__uMinus_param_u32_i32_Frame_t {
#line 500 "Minus.m3"
ADDRESS _unused;
#line 500 "Minus.m3"
};
#line 500 "Minus.m3"
Word__T
__cdecl
Minus__uMinus_param_u32_i32(
   /* Param_Type1 */ Minus__UINT32 a_L_907,
   /* Param_Type1 */ Minus__INT32 b_L_908)
{
#line 500 "Minus.m3"
Minus__uMinus_param_u32_i32_Frame_t _frame;
#line 500 "Minus.m3"
_frame._unused=(ADDRESS)&_frame;
#line 500 "Minus.m3"
 /* load */
#line 500 "Minus.m3"
 /* load */
#line 500 "Minus.m3"
 /* subtract */
#line 500 "Minus.m3"
 /* exit_proc */
#line 500 "Minus.m3"
return ((UINT64)(((UINT64)(((INT64)(a_L_907))))-((UINT64)(((INT64)(b_L_908))))));
#line 500 "Minus.m3"
 /* end_procedure */
#line 500 "Minus.m3"
} /* Minus_param_u32_i32 */
#line 500 "Minus.m3"
 /* set_source_line */
#line 500 "Minus.m3"
#line 501 "Minus.m3"
 /* begin_procedure */
#line 501 "Minus.m3"
struct Minus__Minus_param_u32_i32_Frame_t {
#line 501 "Minus.m3"
ADDRESS _unused;
#line 501 "Minus.m3"
};
#line 501 "Minus.m3"
Minus__UINT32
__cdecl
Minus__Minus_param_u32_i32(
   /* Param_Type1 */ Minus__UINT32 a_L_910,
   /* Param_Type1 */ Minus__INT32 b_L_911)
{
#line 501 "Minus.m3"
 /* Var_Type2 */ INT64 Minus_m_1342_L_1343={0};//always-init
#line 501 "Minus.m3"
Minus__Minus_param_u32_i32_Frame_t _frame;
#line 501 "Minus.m3"
_frame._unused=(ADDRESS)&_frame;
#line 501 "Minus.m3"
 /* load */
#line 501 "Minus.m3"
 /* load */
#line 501 "Minus.m3"
 /* subtract */
#line 501 "Minus.m3"
 /* check_range */
#line 501 "Minus.m3"
 /* store */
#line 501 "Minus.m3"
(*(INT64*)(&Minus_m_1342_L_1343))=(INT64)( ((INT64)( ((INT64)(a_L_910))- ((INT64)(b_L_911)))));
#line 501 "Minus.m3"
 /* load */
#line 501 "Minus.m3"
if(m3_check_range(INT64,
Minus_m_1342_L_1343,
 INT64_(0),
 INT64_(4294967295)))
#line 501 "Minus.m3"
Minus_m_M_Minus_L_13_CRASH(16033);
#line 501 "Minus.m3"
 /* exit_proc */
#line 501 "Minus.m3"
return Minus_m_1342_L_1343;
#line 501 "Minus.m3"
 /* end_procedure */
#line 501 "Minus.m3"
} /* uMinus_var_u32_LC */
#line 501 "Minus.m3"
 /* set_source_line */
#line 501 "Minus.m3"
#line 502 "Minus.m3"
 /* begin_procedure */
#line 502 "Minus.m3"
struct Minus__uMinus_var_u32_LC_Frame_t {
#line 502 "Minus.m3"
ADDRESS _unused;
#line 502 "Minus.m3"
};
#line 502 "Minus.m3"
Long__T
__cdecl
Minus__uMinus_var_u32_LC(void)
{
#line 502 "Minus.m3"
Minus__uMinus_var_u32_LC_Frame_t _frame;
#line 502 "Minus.m3"
_frame._unused=(ADDRESS)&_frame;
#line 502 "Minus.m3"
 /* load */
#line 502 "Minus.m3"
 /* loophole */
#line 502 "Minus.m3"
 /* load */
#line 502 "Minus.m3"
 /* subtract */
#line 502 "Minus.m3"
 /* exit_proc */
#line 502 "Minus.m3"
return ((UINT64)(((UINT64)((INT64)((INT64)(*((UINT32*)(INT64_(184)+((ADDRESS)(&Minus_m_M_Minus_L_13))))))))-((UINT64)(((INT64)(*((UINT64*)(INT64_(136)+((ADDRESS)(&Minus_m_M_Minus_L_13))))))))));
#line 502 "Minus.m3"
 /* end_procedure */
#line 502 "Minus.m3"
} /* Minus_var_u32_LC */
#line 502 "Minus.m3"
 /* set_source_line */
#line 502 "Minus.m3"
#line 503 "Minus.m3"
 /* begin_procedure */
#line 503 "Minus.m3"
struct Minus__Minus_var_u32_LC_Frame_t {
#line 503 "Minus.m3"
ADDRESS _unused;
#line 503 "Minus.m3"
};
#line 503 "Minus.m3"
LONGINT
__cdecl
Minus__Minus_var_u32_LC(void)
{
#line 503 "Minus.m3"
Minus__Minus_var_u32_LC_Frame_t _frame;
#line 503 "Minus.m3"
_frame._unused=(ADDRESS)&_frame;
#line 503 "Minus.m3"
 /* load */
#line 503 "Minus.m3"
 /* loophole */
#line 503 "Minus.m3"
 /* load */
#line 503 "Minus.m3"
 /* subtract */
#line 503 "Minus.m3"
 /* exit_proc */
#line 503 "Minus.m3"
return ((INT64)(((INT64)((INT64)((INT64)(*((UINT32*)(INT64_(184)+((ADDRESS)(&Minus_m_M_Minus_L_13))))))))- ((INT64)(*((UINT64*)(INT64_(136)+((ADDRESS)(&Minus_m_M_Minus_L_13))))))));
#line 503 "Minus.m3"
 /* end_procedure */
#line 503 "Minus.m3"
} /* uMinus_param_u32_LC */
#line 503 "Minus.m3"
 /* set_source_line */
#line 503 "Minus.m3"
#line 504 "Minus.m3"
 /* begin_procedure */
#line 504 "Minus.m3"
struct Minus__uMinus_param_u32_LC_Frame_t {
#line 504 "Minus.m3"
ADDRESS _unused;
#line 504 "Minus.m3"
};
#line 504 "Minus.m3"
Long__T
__cdecl
Minus__uMinus_param_u32_LC(
   /* Param_Type1 */ Minus__UINT32 a_L_915,
   /* Param_Type1 */ LONGCARD b_L_916)
{
#line 504 "Minus.m3"
Minus__uMinus_param_u32_LC_Frame_t _frame;
#line 504 "Minus.m3"
_frame._unused=(ADDRESS)&_frame;
#line 504 "Minus.m3"
 /* load */
#line 504 "Minus.m3"
 /* loophole */
#line 504 "Minus.m3"
 /* load */
#line 504 "Minus.m3"
 /* subtract */
#line 504 "Minus.m3"
 /* exit_proc */
#line 504 "Minus.m3"
return ((UINT64)(((UINT64)((INT64)((INT64)(a_L_915))))-((UINT64)(((INT64)(b_L_916))))));
#line 504 "Minus.m3"
 /* end_procedure */
#line 504 "Minus.m3"
} /* Minus_param_u32_LC */
#line 504 "Minus.m3"
 /* set_source_line */
#line 504 "Minus.m3"
#line 505 "Minus.m3"
 /* begin_procedure */
#line 505 "Minus.m3"
struct Minus__Minus_param_u32_LC_Frame_t {
#line 505 "Minus.m3"
ADDRESS _unused;
#line 505 "Minus.m3"
};
#line 505 "Minus.m3"
LONGINT
__cdecl
Minus__Minus_param_u32_LC(
   /* Param_Type1 */ Minus__UINT32 a_L_918,
   /* Param_Type1 */ LONGCARD b_L_919)
{
#line 505 "Minus.m3"
Minus__Minus_param_u32_LC_Frame_t _frame;
#line 505 "Minus.m3"
_frame._unused=(ADDRESS)&_frame;
#line 505 "Minus.m3"
 /* load */
#line 505 "Minus.m3"
 /* loophole */
#line 505 "Minus.m3"
 /* load */
#line 505 "Minus.m3"
 /* subtract */
#line 505 "Minus.m3"
 /* exit_proc */
#line 505 "Minus.m3"
return ((INT64)(((INT64)((INT64)((INT64)(a_L_918))))- ((INT64)(b_L_919))));
#line 505 "Minus.m3"
 /* end_procedure */
#line 505 "Minus.m3"
} /* uMinus_var_u32_u16 */
#line 505 "Minus.m3"
 /* set_source_line */
#line 505 "Minus.m3"
#line 506 "Minus.m3"
 /* begin_procedure */
#line 506 "Minus.m3"
struct Minus__uMinus_var_u32_u16_Frame_t {
#line 506 "Minus.m3"
ADDRESS _unused;
#line 506 "Minus.m3"
};
#line 506 "Minus.m3"
Word__T
__cdecl
Minus__uMinus_var_u32_u16(void)
{
#line 506 "Minus.m3"
Minus__uMinus_var_u32_u16_Frame_t _frame;
#line 506 "Minus.m3"
_frame._unused=(ADDRESS)&_frame;
#line 506 "Minus.m3"
 /* load */
#line 506 "Minus.m3"
 /* load */
#line 506 "Minus.m3"
 /* subtract */
#line 506 "Minus.m3"
 /* exit_proc */
#line 506 "Minus.m3"
return ((UINT64)(((UINT64)(((INT64)(*((UINT32*)(INT64_(184)+((ADDRESS)(&Minus_m_M_Minus_L_13))))))))-((UINT64)(((INT64)(*((UINT16*)(INT64_(144)+((ADDRESS)(&Minus_m_M_Minus_L_13))))))))));
#line 506 "Minus.m3"
 /* end_procedure */
#line 506 "Minus.m3"
} /* Minus_var_u32_u16 */
#line 506 "Minus.m3"
 /* set_source_line */
#line 506 "Minus.m3"
#line 507 "Minus.m3"
 /* begin_procedure */
#line 507 "Minus.m3"
struct Minus__Minus_var_u32_u16_Frame_t {
#line 507 "Minus.m3"
ADDRESS _unused;
#line 507 "Minus.m3"
};
#line 507 "Minus.m3"
Minus__UINT32
__cdecl
Minus__Minus_var_u32_u16(void)
{
#line 507 "Minus.m3"
 /* Var_Type2 */ INT64 Minus_m_1344_L_1345={0};//always-init
#line 507 "Minus.m3"
Minus__Minus_var_u32_u16_Frame_t _frame;
#line 507 "Minus.m3"
_frame._unused=(ADDRESS)&_frame;
#line 507 "Minus.m3"
 /* load */
#line 507 "Minus.m3"
 /* load */
#line 507 "Minus.m3"
 /* subtract */
#line 507 "Minus.m3"
 /* check_lo */
#line 507 "Minus.m3"
 /* store */
#line 507 "Minus.m3"
(*(INT64*)(&Minus_m_1344_L_1345))=(INT64)( ((INT64)( ((INT64)(*((UINT32*)(INT64_(184)+((ADDRESS)(&Minus_m_M_Minus_L_13))))))- ((INT64)(*((UINT16*)(INT64_(144)+((ADDRESS)(&Minus_m_M_Minus_L_13)))))))));
#line 507 "Minus.m3"
 /* load */
#line 507 "Minus.m3"
/*check_lo*/if(Minus_m_1344_L_1345<INT64_(0))Minus_m_M_Minus_L_13_CRASH(16225);
#line 507 "Minus.m3"
 /* exit_proc */
#line 507 "Minus.m3"
return Minus_m_1344_L_1345;
#line 507 "Minus.m3"
 /* end_procedure */
#line 507 "Minus.m3"
} /* uMinus_param_u32_u16 */
#line 507 "Minus.m3"
 /* set_source_line */
#line 507 "Minus.m3"
#line 508 "Minus.m3"
 /* begin_procedure */
#line 508 "Minus.m3"
struct Minus__uMinus_param_u32_u16_Frame_t {
#line 508 "Minus.m3"
ADDRESS _unused;
#line 508 "Minus.m3"
};
#line 508 "Minus.m3"
Word__T
__cdecl
Minus__uMinus_param_u32_u16(
   /* Param_Type1 */ Minus__UINT32 a_L_923,
   /* Param_Type1 */ Minus__UINT16 b_L_924)
{
#line 508 "Minus.m3"
Minus__uMinus_param_u32_u16_Frame_t _frame;
#line 508 "Minus.m3"
_frame._unused=(ADDRESS)&_frame;
#line 508 "Minus.m3"
 /* load */
#line 508 "Minus.m3"
 /* load */
#line 508 "Minus.m3"
 /* subtract */
#line 508 "Minus.m3"
 /* exit_proc */
#line 508 "Minus.m3"
return ((UINT64)(((UINT64)(((INT64)(a_L_923))))-((UINT64)(((INT64)(b_L_924))))));
#line 508 "Minus.m3"
 /* end_procedure */
#line 508 "Minus.m3"
} /* Minus_param_u32_u16 */
#line 508 "Minus.m3"
 /* set_source_line */
#line 508 "Minus.m3"
#line 509 "Minus.m3"
 /* begin_procedure */
#line 509 "Minus.m3"
struct Minus__Minus_param_u32_u16_Frame_t {
#line 509 "Minus.m3"
ADDRESS _unused;
#line 509 "Minus.m3"
};
#line 509 "Minus.m3"
Minus__UINT32
__cdecl
Minus__Minus_param_u32_u16(
   /* Param_Type1 */ Minus__UINT32 a_L_926,
   /* Param_Type1 */ Minus__UINT16 b_L_927)
{
#line 509 "Minus.m3"
 /* Var_Type2 */ INT64 Minus_m_1346_L_1347={0};//always-init
#line 509 "Minus.m3"
Minus__Minus_param_u32_u16_Frame_t _frame;
#line 509 "Minus.m3"
_frame._unused=(ADDRESS)&_frame;
#line 509 "Minus.m3"
 /* load */
#line 509 "Minus.m3"
 /* load */
#line 509 "Minus.m3"
 /* subtract */
#line 509 "Minus.m3"
 /* check_lo */
#line 509 "Minus.m3"
 /* store */
#line 509 "Minus.m3"
(*(INT64*)(&Minus_m_1346_L_1347))=(INT64)( ((INT64)( ((INT64)(a_L_926))- ((INT64)(b_L_927)))));
#line 509 "Minus.m3"
 /* load */
#line 509 "Minus.m3"
/*check_lo*/if(Minus_m_1346_L_1347<INT64_(0))Minus_m_M_Minus_L_13_CRASH(16289);
#line 509 "Minus.m3"
 /* exit_proc */
#line 509 "Minus.m3"
return Minus_m_1346_L_1347;
#line 509 "Minus.m3"
 /* end_procedure */
#line 509 "Minus.m3"
} /* uMinus_var_u32_I */
#line 509 "Minus.m3"
 /* set_source_line */
#line 509 "Minus.m3"
#line 510 "Minus.m3"
 /* begin_procedure */
#line 510 "Minus.m3"
struct Minus__uMinus_var_u32_I_Frame_t {
#line 510 "Minus.m3"
ADDRESS _unused;
#line 510 "Minus.m3"
};
#line 510 "Minus.m3"
Word__T
__cdecl
Minus__uMinus_var_u32_I(void)
{
#line 510 "Minus.m3"
Minus__uMinus_var_u32_I_Frame_t _frame;
#line 510 "Minus.m3"
_frame._unused=(ADDRESS)&_frame;
#line 510 "Minus.m3"
 /* load */
#line 510 "Minus.m3"
 /* load */
#line 510 "Minus.m3"
 /* subtract */
#line 510 "Minus.m3"
 /* exit_proc */
#line 510 "Minus.m3"
return ((UINT64)(((UINT64)(((INT64)(*((UINT32*)(INT64_(184)+((ADDRESS)(&Minus_m_M_Minus_L_13))))))))-((UINT64)(*((INT64*)(INT64_(152)+((ADDRESS)(&Minus_m_M_Minus_L_13))))))));
#line 510 "Minus.m3"
 /* end_procedure */
#line 510 "Minus.m3"
} /* Minus_var_u32_I */
#line 510 "Minus.m3"
 /* set_source_line */
#line 510 "Minus.m3"
#line 511 "Minus.m3"
 /* begin_procedure */
#line 511 "Minus.m3"
struct Minus__Minus_var_u32_I_Frame_t {
#line 511 "Minus.m3"
ADDRESS _unused;
#line 511 "Minus.m3"
};
#line 511 "Minus.m3"
Minus__UINT32
__cdecl
Minus__Minus_var_u32_I(void)
{
#line 511 "Minus.m3"
 /* Var_Type2 */ INT64 Minus_m_1348_L_1349={0};//always-init
#line 511 "Minus.m3"
Minus__Minus_var_u32_I_Frame_t _frame;
#line 511 "Minus.m3"
_frame._unused=(ADDRESS)&_frame;
#line 511 "Minus.m3"
 /* load */
#line 511 "Minus.m3"
 /* load */
#line 511 "Minus.m3"
 /* subtract */
#line 511 "Minus.m3"
 /* check_range */
#line 511 "Minus.m3"
 /* store */
#line 511 "Minus.m3"
(*(INT64*)(&Minus_m_1348_L_1349))=(INT64)( ((INT64)( ((INT64)(*((UINT32*)(INT64_(184)+((ADDRESS)(&Minus_m_M_Minus_L_13))))))-((INT64)(*((INT64*)(INT64_(152)+((ADDRESS)(&Minus_m_M_Minus_L_13)))))))));
#line 511 "Minus.m3"
 /* load */
#line 511 "Minus.m3"
if(m3_check_range(INT64,
Minus_m_1348_L_1349,
 INT64_(0),
 INT64_(4294967295)))
#line 511 "Minus.m3"
Minus_m_M_Minus_L_13_CRASH(16353);
#line 511 "Minus.m3"
 /* exit_proc */
#line 511 "Minus.m3"
return Minus_m_1348_L_1349;
#line 511 "Minus.m3"
 /* end_procedure */
#line 511 "Minus.m3"
} /* uMinus_param_u32_I */
#line 511 "Minus.m3"
 /* set_source_line */
#line 511 "Minus.m3"
#line 512 "Minus.m3"
 /* begin_procedure */
#line 512 "Minus.m3"
struct Minus__uMinus_param_u32_I_Frame_t {
#line 512 "Minus.m3"
ADDRESS _unused;
#line 512 "Minus.m3"
};
#line 512 "Minus.m3"
Word__T
__cdecl
Minus__uMinus_param_u32_I(
   /* Param_Type1 */ Minus__UINT32 a_L_931,
   /* Param_Type1 */ INTEGER b_L_932)
{
#line 512 "Minus.m3"
Minus__uMinus_param_u32_I_Frame_t _frame;
#line 512 "Minus.m3"
_frame._unused=(ADDRESS)&_frame;
#line 512 "Minus.m3"
 /* load */
#line 512 "Minus.m3"
 /* load */
#line 512 "Minus.m3"
 /* subtract */
#line 512 "Minus.m3"
 /* exit_proc */
#line 512 "Minus.m3"
return ((UINT64)(((UINT64)(((INT64)(a_L_931))))-((UINT64)(b_L_932))));
#line 512 "Minus.m3"
 /* end_procedure */
#line 512 "Minus.m3"
} /* Minus_param_u32_I */
#line 512 "Minus.m3"
 /* set_source_line */
#line 512 "Minus.m3"
#line 513 "Minus.m3"
 /* begin_procedure */
#line 513 "Minus.m3"
struct Minus__Minus_param_u32_I_Frame_t {
#line 513 "Minus.m3"
ADDRESS _unused;
#line 513 "Minus.m3"
};
#line 513 "Minus.m3"
Minus__UINT32
__cdecl
Minus__Minus_param_u32_I(
   /* Param_Type1 */ Minus__UINT32 a_L_934,
   /* Param_Type1 */ INTEGER b_L_935)
{
#line 513 "Minus.m3"
 /* Var_Type2 */ INT64 Minus_m_1350_L_1351={0};//always-init
#line 513 "Minus.m3"
Minus__Minus_param_u32_I_Frame_t _frame;
#line 513 "Minus.m3"
_frame._unused=(ADDRESS)&_frame;
#line 513 "Minus.m3"
 /* load */
#line 513 "Minus.m3"
 /* load */
#line 513 "Minus.m3"
 /* subtract */
#line 513 "Minus.m3"
 /* check_range */
#line 513 "Minus.m3"
 /* store */
#line 513 "Minus.m3"
(*(INT64*)(&Minus_m_1350_L_1351))=(INT64)( ((INT64)( ((INT64)(a_L_934))- b_L_935)));
#line 513 "Minus.m3"
 /* load */
#line 513 "Minus.m3"
if(m3_check_range(INT64,
Minus_m_1350_L_1351,
 INT64_(0),
 INT64_(4294967295)))
#line 513 "Minus.m3"
Minus_m_M_Minus_L_13_CRASH(16417);
#line 513 "Minus.m3"
 /* exit_proc */
#line 513 "Minus.m3"
return Minus_m_1350_L_1351;
#line 513 "Minus.m3"
 /* end_procedure */
#line 513 "Minus.m3"
} /* uMinus_var_u32_i64 */
#line 513 "Minus.m3"
 /* set_source_line */
#line 513 "Minus.m3"
#line 514 "Minus.m3"
 /* begin_procedure */
#line 514 "Minus.m3"
struct Minus__uMinus_var_u32_i64_Frame_t {
#line 514 "Minus.m3"
ADDRESS _unused;
#line 514 "Minus.m3"
};
#line 514 "Minus.m3"
Long__T
__cdecl
Minus__uMinus_var_u32_i64(void)
{
#line 514 "Minus.m3"
Minus__uMinus_var_u32_i64_Frame_t _frame;
#line 514 "Minus.m3"
_frame._unused=(ADDRESS)&_frame;
#line 514 "Minus.m3"
 /* load */
#line 514 "Minus.m3"
 /* loophole */
#line 514 "Minus.m3"
 /* load */
#line 514 "Minus.m3"
 /* subtract */
#line 514 "Minus.m3"
 /* exit_proc */
#line 514 "Minus.m3"
return ((UINT64)(((UINT64)((INT64)((INT64)(*((UINT32*)(INT64_(184)+((ADDRESS)(&Minus_m_M_Minus_L_13))))))))-((UINT64)(*((INT64*)(INT64_(160)+((ADDRESS)(&Minus_m_M_Minus_L_13))))))));
#line 514 "Minus.m3"
 /* end_procedure */
#line 514 "Minus.m3"
} /* Minus_var_u32_i64 */
#line 514 "Minus.m3"
 /* set_source_line */
#line 514 "Minus.m3"
#line 515 "Minus.m3"
 /* begin_procedure */
#line 515 "Minus.m3"
struct Minus__Minus_var_u32_i64_Frame_t {
#line 515 "Minus.m3"
ADDRESS _unused;
#line 515 "Minus.m3"
};
#line 515 "Minus.m3"
LONGINT
__cdecl
Minus__Minus_var_u32_i64(void)
{
#line 515 "Minus.m3"
Minus__Minus_var_u32_i64_Frame_t _frame;
#line 515 "Minus.m3"
_frame._unused=(ADDRESS)&_frame;
#line 515 "Minus.m3"
 /* load */
#line 515 "Minus.m3"
 /* loophole */
#line 515 "Minus.m3"
 /* load */
#line 515 "Minus.m3"
 /* subtract */
#line 515 "Minus.m3"
 /* exit_proc */
#line 515 "Minus.m3"
return ((INT64)(((INT64)((INT64)((INT64)(*((UINT32*)(INT64_(184)+((ADDRESS)(&Minus_m_M_Minus_L_13))))))))-((INT64)(*((INT64*)(INT64_(160)+((ADDRESS)(&Minus_m_M_Minus_L_13))))))));
#line 515 "Minus.m3"
 /* end_procedure */
#line 515 "Minus.m3"
} /* uMinus_param_u32_i64 */
#line 515 "Minus.m3"
 /* set_source_line */
#line 515 "Minus.m3"
#line 516 "Minus.m3"
 /* begin_procedure */
#line 516 "Minus.m3"
struct Minus__uMinus_param_u32_i64_Frame_t {
#line 516 "Minus.m3"
ADDRESS _unused;
#line 516 "Minus.m3"
};
#line 516 "Minus.m3"
Long__T
__cdecl
Minus__uMinus_param_u32_i64(
   /* Param_Type1 */ Minus__UINT32 a_L_939,
   /* Param_Type1 */ Minus__INT64 b_L_940)
{
#line 516 "Minus.m3"
Minus__uMinus_param_u32_i64_Frame_t _frame;
#line 516 "Minus.m3"
_frame._unused=(ADDRESS)&_frame;
#line 516 "Minus.m3"
 /* load */
#line 516 "Minus.m3"
 /* loophole */
#line 516 "Minus.m3"
 /* load */
#line 516 "Minus.m3"
 /* subtract */
#line 516 "Minus.m3"
 /* exit_proc */
#line 516 "Minus.m3"
return ((UINT64)(((UINT64)((INT64)((INT64)(a_L_939))))-((UINT64)(b_L_940))));
#line 516 "Minus.m3"
 /* end_procedure */
#line 516 "Minus.m3"
} /* Minus_param_u32_i64 */
#line 516 "Minus.m3"
 /* set_source_line */
#line 516 "Minus.m3"
#line 517 "Minus.m3"
 /* begin_procedure */
#line 517 "Minus.m3"
struct Minus__Minus_param_u32_i64_Frame_t {
#line 517 "Minus.m3"
ADDRESS _unused;
#line 517 "Minus.m3"
};
#line 517 "Minus.m3"
LONGINT
__cdecl
Minus__Minus_param_u32_i64(
   /* Param_Type1 */ Minus__UINT32 a_L_942,
   /* Param_Type1 */ Minus__INT64 b_L_943)
{
#line 517 "Minus.m3"
Minus__Minus_param_u32_i64_Frame_t _frame;
#line 517 "Minus.m3"
_frame._unused=(ADDRESS)&_frame;
#line 517 "Minus.m3"
 /* load */
#line 517 "Minus.m3"
 /* loophole */
#line 517 "Minus.m3"
 /* load */
#line 517 "Minus.m3"
 /* subtract */
#line 517 "Minus.m3"
 /* exit_proc */
#line 517 "Minus.m3"
return ((INT64)(((INT64)((INT64)((INT64)(a_L_942))))- b_L_943));
#line 517 "Minus.m3"
 /* end_procedure */
#line 517 "Minus.m3"
} /* uMinus_var_u32_i16 */
#line 517 "Minus.m3"
 /* set_source_line */
#line 517 "Minus.m3"
#line 518 "Minus.m3"
 /* begin_procedure */
#line 518 "Minus.m3"
struct Minus__uMinus_var_u32_i16_Frame_t {
#line 518 "Minus.m3"
ADDRESS _unused;
#line 518 "Minus.m3"
};
#line 518 "Minus.m3"
Word__T
__cdecl
Minus__uMinus_var_u32_i16(void)
{
#line 518 "Minus.m3"
Minus__uMinus_var_u32_i16_Frame_t _frame;
#line 518 "Minus.m3"
_frame._unused=(ADDRESS)&_frame;
#line 518 "Minus.m3"
 /* load */
#line 518 "Minus.m3"
 /* load */
#line 518 "Minus.m3"
 /* subtract */
#line 518 "Minus.m3"
 /* exit_proc */
#line 518 "Minus.m3"
return ((UINT64)(((UINT64)(((INT64)(*((UINT32*)(INT64_(184)+((ADDRESS)(&Minus_m_M_Minus_L_13))))))))-((UINT64)(((INT64)(*((INT16*)(INT64_(172)+((ADDRESS)(&Minus_m_M_Minus_L_13))))))))));
#line 518 "Minus.m3"
 /* end_procedure */
#line 518 "Minus.m3"
} /* Minus_var_u32_i16 */
#line 518 "Minus.m3"
 /* set_source_line */
#line 518 "Minus.m3"
#line 519 "Minus.m3"
 /* begin_procedure */
#line 519 "Minus.m3"
struct Minus__Minus_var_u32_i16_Frame_t {
#line 519 "Minus.m3"
ADDRESS _unused;
#line 519 "Minus.m3"
};
#line 519 "Minus.m3"
Minus__UINT32
__cdecl
Minus__Minus_var_u32_i16(void)
{
#line 519 "Minus.m3"
 /* Var_Type2 */ INT64 Minus_m_1352_L_1353={0};//always-init
#line 519 "Minus.m3"
Minus__Minus_var_u32_i16_Frame_t _frame;
#line 519 "Minus.m3"
_frame._unused=(ADDRESS)&_frame;
#line 519 "Minus.m3"
 /* load */
#line 519 "Minus.m3"
 /* load */
#line 519 "Minus.m3"
 /* subtract */
#line 519 "Minus.m3"
 /* check_range */
#line 519 "Minus.m3"
 /* store */
#line 519 "Minus.m3"
(*(INT64*)(&Minus_m_1352_L_1353))=(INT64)( ((INT64)( ((INT64)(*((UINT32*)(INT64_(184)+((ADDRESS)(&Minus_m_M_Minus_L_13))))))- ((INT64)(*((INT16*)(INT64_(172)+((ADDRESS)(&Minus_m_M_Minus_L_13)))))))));
#line 519 "Minus.m3"
 /* load */
#line 519 "Minus.m3"
if(m3_check_range(INT64,
Minus_m_1352_L_1353,
 INT64_(0),
 INT64_(4294967295)))
#line 519 "Minus.m3"
Minus_m_M_Minus_L_13_CRASH(16609);
#line 519 "Minus.m3"
 /* exit_proc */
#line 519 "Minus.m3"
return Minus_m_1352_L_1353;
#line 519 "Minus.m3"
 /* end_procedure */
#line 519 "Minus.m3"
} /* uMinus_param_u32_i16 */
#line 519 "Minus.m3"
 /* set_source_line */
#line 519 "Minus.m3"
#line 520 "Minus.m3"
 /* begin_procedure */
#line 520 "Minus.m3"
struct Minus__uMinus_param_u32_i16_Frame_t {
#line 520 "Minus.m3"
ADDRESS _unused;
#line 520 "Minus.m3"
};
#line 520 "Minus.m3"
Word__T
__cdecl
Minus__uMinus_param_u32_i16(
   /* Param_Type1 */ Minus__UINT32 a_L_947,
   /* Param_Type1 */ Minus__INT16 b_L_948)
{
#line 520 "Minus.m3"
Minus__uMinus_param_u32_i16_Frame_t _frame;
#line 520 "Minus.m3"
_frame._unused=(ADDRESS)&_frame;
#line 520 "Minus.m3"
 /* load */
#line 520 "Minus.m3"
 /* load */
#line 520 "Minus.m3"
 /* subtract */
#line 520 "Minus.m3"
 /* exit_proc */
#line 520 "Minus.m3"
return ((UINT64)(((UINT64)(((INT64)(a_L_947))))-((UINT64)(((INT64)(b_L_948))))));
#line 520 "Minus.m3"
 /* end_procedure */
#line 520 "Minus.m3"
} /* Minus_param_u32_i16 */
#line 520 "Minus.m3"
 /* set_source_line */
#line 520 "Minus.m3"
#line 521 "Minus.m3"
 /* begin_procedure */
#line 521 "Minus.m3"
struct Minus__Minus_param_u32_i16_Frame_t {
#line 521 "Minus.m3"
ADDRESS _unused;
#line 521 "Minus.m3"
};
#line 521 "Minus.m3"
Minus__UINT32
__cdecl
Minus__Minus_param_u32_i16(
   /* Param_Type1 */ Minus__UINT32 a_L_950,
   /* Param_Type1 */ Minus__INT16 b_L_951)
{
#line 521 "Minus.m3"
 /* Var_Type2 */ INT64 Minus_m_1354_L_1355={0};//always-init
#line 521 "Minus.m3"
Minus__Minus_param_u32_i16_Frame_t _frame;
#line 521 "Minus.m3"
_frame._unused=(ADDRESS)&_frame;
#line 521 "Minus.m3"
 /* load */
#line 521 "Minus.m3"
 /* load */
#line 521 "Minus.m3"
 /* subtract */
#line 521 "Minus.m3"
 /* check_range */
#line 521 "Minus.m3"
 /* store */
#line 521 "Minus.m3"
(*(INT64*)(&Minus_m_1354_L_1355))=(INT64)( ((INT64)( ((INT64)(a_L_950))- ((INT64)(b_L_951)))));
#line 521 "Minus.m3"
 /* load */
#line 521 "Minus.m3"
if(m3_check_range(INT64,
Minus_m_1354_L_1355,
 INT64_(0),
 INT64_(4294967295)))
#line 521 "Minus.m3"
Minus_m_M_Minus_L_13_CRASH(16673);
#line 521 "Minus.m3"
 /* exit_proc */
#line 521 "Minus.m3"
return Minus_m_1354_L_1355;
#line 521 "Minus.m3"
 /* end_procedure */
#line 521 "Minus.m3"
} /* uMinus_var_u32_C */
#line 521 "Minus.m3"
 /* set_source_line */
#line 521 "Minus.m3"
#line 522 "Minus.m3"
 /* begin_procedure */
#line 522 "Minus.m3"
struct Minus__uMinus_var_u32_C_Frame_t {
#line 522 "Minus.m3"
ADDRESS _unused;
#line 522 "Minus.m3"
};
#line 522 "Minus.m3"
Word__T
__cdecl
Minus__uMinus_var_u32_C(void)
{
#line 522 "Minus.m3"
Minus__uMinus_var_u32_C_Frame_t _frame;
#line 522 "Minus.m3"
_frame._unused=(ADDRESS)&_frame;
#line 522 "Minus.m3"
 /* load */
#line 522 "Minus.m3"
 /* load */
#line 522 "Minus.m3"
 /* subtract */
#line 522 "Minus.m3"
 /* exit_proc */
#line 522 "Minus.m3"
return ((UINT64)(((UINT64)(((INT64)(*((UINT32*)(INT64_(184)+((ADDRESS)(&Minus_m_M_Minus_L_13))))))))-((UINT64)(((INT64)(*((UINT64*)(INT64_(176)+((ADDRESS)(&Minus_m_M_Minus_L_13))))))))));
#line 522 "Minus.m3"
 /* end_procedure */
#line 522 "Minus.m3"
} /* Minus_var_u32_C */
#line 522 "Minus.m3"
 /* set_source_line */
#line 522 "Minus.m3"
#line 523 "Minus.m3"
 /* begin_procedure */
#line 523 "Minus.m3"
struct Minus__Minus_var_u32_C_Frame_t {
#line 523 "Minus.m3"
ADDRESS _unused;
#line 523 "Minus.m3"
};
#line 523 "Minus.m3"
Minus__UINT32
__cdecl
Minus__Minus_var_u32_C(void)
{
#line 523 "Minus.m3"
 /* Var_Type2 */ INT64 Minus_m_1356_L_1357={0};//always-init
#line 523 "Minus.m3"
Minus__Minus_var_u32_C_Frame_t _frame;
#line 523 "Minus.m3"
_frame._unused=(ADDRESS)&_frame;
#line 523 "Minus.m3"
 /* load */
#line 523 "Minus.m3"
 /* load */
#line 523 "Minus.m3"
 /* subtract */
#line 523 "Minus.m3"
 /* check_lo */
#line 523 "Minus.m3"
 /* store */
#line 523 "Minus.m3"
(*(INT64*)(&Minus_m_1356_L_1357))=(INT64)( ((INT64)( ((INT64)(*((UINT32*)(INT64_(184)+((ADDRESS)(&Minus_m_M_Minus_L_13))))))- ((INT64)(*((UINT64*)(INT64_(176)+((ADDRESS)(&Minus_m_M_Minus_L_13)))))))));
#line 523 "Minus.m3"
 /* load */
#line 523 "Minus.m3"
/*check_lo*/if(Minus_m_1356_L_1357<INT64_(0))Minus_m_M_Minus_L_13_CRASH(16737);
#line 523 "Minus.m3"
 /* exit_proc */
#line 523 "Minus.m3"
return Minus_m_1356_L_1357;
#line 523 "Minus.m3"
 /* end_procedure */
#line 523 "Minus.m3"
} /* uMinus_param_u32_C */
#line 523 "Minus.m3"
 /* set_source_line */
#line 523 "Minus.m3"
#line 524 "Minus.m3"
 /* begin_procedure */
#line 524 "Minus.m3"
struct Minus__uMinus_param_u32_C_Frame_t {
#line 524 "Minus.m3"
ADDRESS _unused;
#line 524 "Minus.m3"
};
#line 524 "Minus.m3"
Word__T
__cdecl
Minus__uMinus_param_u32_C(
   /* Param_Type1 */ Minus__UINT32 a_L_955,
   /* Param_Type1 */ CARDINAL b_L_956)
{
#line 524 "Minus.m3"
Minus__uMinus_param_u32_C_Frame_t _frame;
#line 524 "Minus.m3"
_frame._unused=(ADDRESS)&_frame;
#line 524 "Minus.m3"
 /* load */
#line 524 "Minus.m3"
 /* load */
#line 524 "Minus.m3"
 /* subtract */
#line 524 "Minus.m3"
 /* exit_proc */
#line 524 "Minus.m3"
return ((UINT64)(((UINT64)(((INT64)(a_L_955))))-((UINT64)(((INT64)(b_L_956))))));
#line 524 "Minus.m3"
 /* end_procedure */
#line 524 "Minus.m3"
} /* Minus_param_u32_C */
#line 524 "Minus.m3"
 /* set_source_line */
#line 524 "Minus.m3"
#line 525 "Minus.m3"
 /* begin_procedure */
#line 525 "Minus.m3"
struct Minus__Minus_param_u32_C_Frame_t {
#line 525 "Minus.m3"
ADDRESS _unused;
#line 525 "Minus.m3"
};
#line 525 "Minus.m3"
Minus__UINT32
__cdecl
Minus__Minus_param_u32_C(
   /* Param_Type1 */ Minus__UINT32 a_L_958,
   /* Param_Type1 */ CARDINAL b_L_959)
{
#line 525 "Minus.m3"
 /* Var_Type2 */ INT64 Minus_m_1358_L_1359={0};//always-init
#line 525 "Minus.m3"
Minus__Minus_param_u32_C_Frame_t _frame;
#line 525 "Minus.m3"
_frame._unused=(ADDRESS)&_frame;
#line 525 "Minus.m3"
 /* load */
#line 525 "Minus.m3"
 /* load */
#line 525 "Minus.m3"
 /* subtract */
#line 525 "Minus.m3"
 /* check_lo */
#line 525 "Minus.m3"
 /* store */
#line 525 "Minus.m3"
(*(INT64*)(&Minus_m_1358_L_1359))=(INT64)( ((INT64)( ((INT64)(a_L_958))- ((INT64)(b_L_959)))));
#line 525 "Minus.m3"
 /* load */
#line 525 "Minus.m3"
/*check_lo*/if(Minus_m_1358_L_1359<INT64_(0))Minus_m_M_Minus_L_13_CRASH(16801);
#line 525 "Minus.m3"
 /* exit_proc */
#line 525 "Minus.m3"
return Minus_m_1358_L_1359;
#line 525 "Minus.m3"
 /* end_procedure */
#line 525 "Minus.m3"
} /* uMinus_var_u32_u32 */
#line 525 "Minus.m3"
 /* set_source_line */
#line 525 "Minus.m3"
#line 526 "Minus.m3"
 /* begin_procedure */
#line 526 "Minus.m3"
struct Minus__uMinus_var_u32_u32_Frame_t {
#line 526 "Minus.m3"
ADDRESS _unused;
#line 526 "Minus.m3"
};
#line 526 "Minus.m3"
Word__T
__cdecl
Minus__uMinus_var_u32_u32(void)
{
#line 526 "Minus.m3"
Minus__uMinus_var_u32_u32_Frame_t _frame;
#line 526 "Minus.m3"
_frame._unused=(ADDRESS)&_frame;
#line 526 "Minus.m3"
 /* load */
#line 526 "Minus.m3"
 /* load */
#line 526 "Minus.m3"
 /* subtract */
#line 526 "Minus.m3"
 /* exit_proc */
#line 526 "Minus.m3"
return ((UINT64)(((UINT64)(((INT64)(*((UINT32*)(INT64_(184)+((ADDRESS)(&Minus_m_M_Minus_L_13))))))))-((UINT64)(((INT64)(*((UINT32*)(INT64_(184)+((ADDRESS)(&Minus_m_M_Minus_L_13))))))))));
#line 526 "Minus.m3"
 /* end_procedure */
#line 526 "Minus.m3"
} /* Minus_var_u32_u32 */
#line 526 "Minus.m3"
 /* set_source_line */
#line 526 "Minus.m3"
#line 527 "Minus.m3"
 /* begin_procedure */
#line 527 "Minus.m3"
struct Minus__Minus_var_u32_u32_Frame_t {
#line 527 "Minus.m3"
ADDRESS _unused;
#line 527 "Minus.m3"
};
#line 527 "Minus.m3"
Minus__UINT32
__cdecl
Minus__Minus_var_u32_u32(void)
{
#line 527 "Minus.m3"
 /* Var_Type2 */ INT64 Minus_m_1360_L_1361={0};//always-init
#line 527 "Minus.m3"
Minus__Minus_var_u32_u32_Frame_t _frame;
#line 527 "Minus.m3"
_frame._unused=(ADDRESS)&_frame;
#line 527 "Minus.m3"
 /* load */
#line 527 "Minus.m3"
 /* load */
#line 527 "Minus.m3"
 /* subtract */
#line 527 "Minus.m3"
 /* check_lo */
#line 527 "Minus.m3"
 /* store */
#line 527 "Minus.m3"
(*(INT64*)(&Minus_m_1360_L_1361))=(INT64)( ((INT64)( ((INT64)(*((UINT32*)(INT64_(184)+((ADDRESS)(&Minus_m_M_Minus_L_13))))))- ((INT64)(*((UINT32*)(INT64_(184)+((ADDRESS)(&Minus_m_M_Minus_L_13)))))))));
#line 527 "Minus.m3"
 /* load */
#line 527 "Minus.m3"
/*check_lo*/if(Minus_m_1360_L_1361<INT64_(0))Minus_m_M_Minus_L_13_CRASH(16865);
#line 527 "Minus.m3"
 /* exit_proc */
#line 527 "Minus.m3"
return Minus_m_1360_L_1361;
#line 527 "Minus.m3"
 /* end_procedure */
#line 527 "Minus.m3"
} /* uMinus_param_u32_u32 */
#line 527 "Minus.m3"
 /* set_source_line */
#line 527 "Minus.m3"
#line 528 "Minus.m3"
 /* begin_procedure */
#line 528 "Minus.m3"
struct Minus__uMinus_param_u32_u32_Frame_t {
#line 528 "Minus.m3"
ADDRESS _unused;
#line 528 "Minus.m3"
};
#line 528 "Minus.m3"
Word__T
__cdecl
Minus__uMinus_param_u32_u32(
   /* Param_Type1 */ Minus__UINT32 a_L_963,
   /* Param_Type1 */ Minus__UINT32 b_L_964)
{
#line 528 "Minus.m3"
Minus__uMinus_param_u32_u32_Frame_t _frame;
#line 528 "Minus.m3"
_frame._unused=(ADDRESS)&_frame;
#line 528 "Minus.m3"
 /* load */
#line 528 "Minus.m3"
 /* load */
#line 528 "Minus.m3"
 /* subtract */
#line 528 "Minus.m3"
 /* exit_proc */
#line 528 "Minus.m3"
return ((UINT64)(((UINT64)(((INT64)(a_L_963))))-((UINT64)(((INT64)(b_L_964))))));
#line 528 "Minus.m3"
 /* end_procedure */
#line 528 "Minus.m3"
} /* Minus_param_u32_u32 */
#line 528 "Minus.m3"
 /* set_source_line */
#line 528 "Minus.m3"
#line 529 "Minus.m3"
 /* begin_procedure */
#line 529 "Minus.m3"
struct Minus__Minus_param_u32_u32_Frame_t {
#line 529 "Minus.m3"
ADDRESS _unused;
#line 529 "Minus.m3"
};
#line 529 "Minus.m3"
Minus__UINT32
__cdecl
Minus__Minus_param_u32_u32(
   /* Param_Type1 */ Minus__UINT32 a_L_966,
   /* Param_Type1 */ Minus__UINT32 b_L_967)
{
#line 529 "Minus.m3"
 /* Var_Type2 */ INT64 Minus_m_1362_L_1363={0};//always-init
#line 529 "Minus.m3"
Minus__Minus_param_u32_u32_Frame_t _frame;
#line 529 "Minus.m3"
_frame._unused=(ADDRESS)&_frame;
#line 529 "Minus.m3"
 /* load */
#line 529 "Minus.m3"
 /* load */
#line 529 "Minus.m3"
 /* subtract */
#line 529 "Minus.m3"
 /* check_lo */
#line 529 "Minus.m3"
 /* store */
#line 529 "Minus.m3"
(*(INT64*)(&Minus_m_1362_L_1363))=(INT64)( ((INT64)( ((INT64)(a_L_966))- ((INT64)(b_L_967)))));
#line 529 "Minus.m3"
 /* load */
#line 529 "Minus.m3"
/*check_lo*/if(Minus_m_1362_L_1363<INT64_(0))Minus_m_M_Minus_L_13_CRASH(16929);
#line 529 "Minus.m3"
 /* exit_proc */
#line 529 "Minus.m3"
return Minus_m_1362_L_1363;
#line 529 "Minus.m3"
 /* end_procedure */
#line 529 "Minus.m3"
} /* uMinus_var_u32_u8 */
#line 529 "Minus.m3"
 /* set_source_line */
#line 529 "Minus.m3"
#line 530 "Minus.m3"
 /* begin_procedure */
#line 530 "Minus.m3"
struct Minus__uMinus_var_u32_u8_Frame_t {
#line 530 "Minus.m3"
ADDRESS _unused;
#line 530 "Minus.m3"
};
#line 530 "Minus.m3"
Word__T
__cdecl
Minus__uMinus_var_u32_u8(void)
{
#line 530 "Minus.m3"
Minus__uMinus_var_u32_u8_Frame_t _frame;
#line 530 "Minus.m3"
_frame._unused=(ADDRESS)&_frame;
#line 530 "Minus.m3"
 /* load */
#line 530 "Minus.m3"
 /* load */
#line 530 "Minus.m3"
 /* subtract */
#line 530 "Minus.m3"
 /* exit_proc */
#line 530 "Minus.m3"
return ((UINT64)(((UINT64)(((INT64)(*((UINT32*)(INT64_(184)+((ADDRESS)(&Minus_m_M_Minus_L_13))))))))-((UINT64)(((INT64)(*((UINT8*)(INT64_(188)+((ADDRESS)(&Minus_m_M_Minus_L_13))))))))));
#line 530 "Minus.m3"
 /* end_procedure */
#line 530 "Minus.m3"
} /* Minus_var_u32_u8 */
#line 530 "Minus.m3"
 /* set_source_line */
#line 530 "Minus.m3"
#line 531 "Minus.m3"
 /* begin_procedure */
#line 531 "Minus.m3"
struct Minus__Minus_var_u32_u8_Frame_t {
#line 531 "Minus.m3"
ADDRESS _unused;
#line 531 "Minus.m3"
};
#line 531 "Minus.m3"
Minus__UINT32
__cdecl
Minus__Minus_var_u32_u8(void)
{
#line 531 "Minus.m3"
 /* Var_Type2 */ INT64 Minus_m_1364_L_1365={0};//always-init
#line 531 "Minus.m3"
Minus__Minus_var_u32_u8_Frame_t _frame;
#line 531 "Minus.m3"
_frame._unused=(ADDRESS)&_frame;
#line 531 "Minus.m3"
 /* load */
#line 531 "Minus.m3"
 /* load */
#line 531 "Minus.m3"
 /* subtract */
#line 531 "Minus.m3"
 /* check_lo */
#line 531 "Minus.m3"
 /* store */
#line 531 "Minus.m3"
(*(INT64*)(&Minus_m_1364_L_1365))=(INT64)( ((INT64)( ((INT64)(*((UINT32*)(INT64_(184)+((ADDRESS)(&Minus_m_M_Minus_L_13))))))- ((INT64)(*((UINT8*)(INT64_(188)+((ADDRESS)(&Minus_m_M_Minus_L_13)))))))));
#line 531 "Minus.m3"
 /* load */
#line 531 "Minus.m3"
/*check_lo*/if(Minus_m_1364_L_1365<INT64_(0))Minus_m_M_Minus_L_13_CRASH(16993);
#line 531 "Minus.m3"
 /* exit_proc */
#line 531 "Minus.m3"
return Minus_m_1364_L_1365;
#line 531 "Minus.m3"
 /* end_procedure */
#line 531 "Minus.m3"
} /* uMinus_param_u32_u8 */
#line 531 "Minus.m3"
 /* set_source_line */
#line 531 "Minus.m3"
#line 532 "Minus.m3"
 /* begin_procedure */
#line 532 "Minus.m3"
struct Minus__uMinus_param_u32_u8_Frame_t {
#line 532 "Minus.m3"
ADDRESS _unused;
#line 532 "Minus.m3"
};
#line 532 "Minus.m3"
Word__T
__cdecl
Minus__uMinus_param_u32_u8(
   /* Param_Type1 */ Minus__UINT32 a_L_971,
   /* Param_Type1 */ Minus__UINT8 b_L_972)
{
#line 532 "Minus.m3"
Minus__uMinus_param_u32_u8_Frame_t _frame;
#line 532 "Minus.m3"
_frame._unused=(ADDRESS)&_frame;
#line 532 "Minus.m3"
 /* load */
#line 532 "Minus.m3"
 /* load */
#line 532 "Minus.m3"
 /* subtract */
#line 532 "Minus.m3"
 /* exit_proc */
#line 532 "Minus.m3"
return ((UINT64)(((UINT64)(((INT64)(a_L_971))))-((UINT64)(((INT64)(b_L_972))))));
#line 532 "Minus.m3"
 /* end_procedure */
#line 532 "Minus.m3"
} /* Minus_param_u32_u8 */
#line 532 "Minus.m3"
 /* set_source_line */
#line 532 "Minus.m3"
#line 533 "Minus.m3"
 /* begin_procedure */
#line 533 "Minus.m3"
struct Minus__Minus_param_u32_u8_Frame_t {
#line 533 "Minus.m3"
ADDRESS _unused;
#line 533 "Minus.m3"
};
#line 533 "Minus.m3"
Minus__UINT32
__cdecl
Minus__Minus_param_u32_u8(
   /* Param_Type1 */ Minus__UINT32 a_L_974,
   /* Param_Type1 */ Minus__UINT8 b_L_975)
{
#line 533 "Minus.m3"
 /* Var_Type2 */ INT64 Minus_m_1366_L_1367={0};//always-init
#line 533 "Minus.m3"
Minus__Minus_param_u32_u8_Frame_t _frame;
#line 533 "Minus.m3"
_frame._unused=(ADDRESS)&_frame;
#line 533 "Minus.m3"
 /* load */
#line 533 "Minus.m3"
 /* load */
#line 533 "Minus.m3"
 /* subtract */
#line 533 "Minus.m3"
 /* check_lo */
#line 533 "Minus.m3"
 /* store */
#line 533 "Minus.m3"
(*(INT64*)(&Minus_m_1366_L_1367))=(INT64)( ((INT64)( ((INT64)(a_L_974))- ((INT64)(b_L_975)))));
#line 533 "Minus.m3"
 /* load */
#line 533 "Minus.m3"
/*check_lo*/if(Minus_m_1366_L_1367<INT64_(0))Minus_m_M_Minus_L_13_CRASH(17057);
#line 533 "Minus.m3"
 /* exit_proc */
#line 533 "Minus.m3"
return Minus_m_1366_L_1367;
#line 533 "Minus.m3"
 /* end_procedure */
#line 533 "Minus.m3"
} /* uMinus_var_u32_L */
#line 533 "Minus.m3"
 /* set_source_line */
#line 533 "Minus.m3"
#line 534 "Minus.m3"
 /* begin_procedure */
#line 534 "Minus.m3"
struct Minus__uMinus_var_u32_L_Frame_t {
#line 534 "Minus.m3"
ADDRESS _unused;
#line 534 "Minus.m3"
};
#line 534 "Minus.m3"
Long__T
__cdecl
Minus__uMinus_var_u32_L(void)
{
#line 534 "Minus.m3"
Minus__uMinus_var_u32_L_Frame_t _frame;
#line 534 "Minus.m3"
_frame._unused=(ADDRESS)&_frame;
#line 534 "Minus.m3"
 /* load */
#line 534 "Minus.m3"
 /* loophole */
#line 534 "Minus.m3"
 /* load */
#line 534 "Minus.m3"
 /* subtract */
#line 534 "Minus.m3"
 /* exit_proc */
#line 534 "Minus.m3"
return ((UINT64)(((UINT64)((INT64)((INT64)(*((UINT32*)(INT64_(184)+((ADDRESS)(&Minus_m_M_Minus_L_13))))))))-((UINT64)(*((INT64*)(INT64_(192)+((ADDRESS)(&Minus_m_M_Minus_L_13))))))));
#line 534 "Minus.m3"
 /* end_procedure */
#line 534 "Minus.m3"
} /* Minus_var_u32_L */
#line 534 "Minus.m3"
 /* set_source_line */
#line 534 "Minus.m3"
#line 535 "Minus.m3"
 /* begin_procedure */
#line 535 "Minus.m3"
struct Minus__Minus_var_u32_L_Frame_t {
#line 535 "Minus.m3"
ADDRESS _unused;
#line 535 "Minus.m3"
};
#line 535 "Minus.m3"
LONGINT
__cdecl
Minus__Minus_var_u32_L(void)
{
#line 535 "Minus.m3"
Minus__Minus_var_u32_L_Frame_t _frame;
#line 535 "Minus.m3"
_frame._unused=(ADDRESS)&_frame;
#line 535 "Minus.m3"
 /* load */
#line 535 "Minus.m3"
 /* loophole */
#line 535 "Minus.m3"
 /* load */
#line 535 "Minus.m3"
 /* subtract */
#line 535 "Minus.m3"
 /* exit_proc */
#line 535 "Minus.m3"
return ((INT64)(((INT64)((INT64)((INT64)(*((UINT32*)(INT64_(184)+((ADDRESS)(&Minus_m_M_Minus_L_13))))))))-((INT64)(*((INT64*)(INT64_(192)+((ADDRESS)(&Minus_m_M_Minus_L_13))))))));
#line 535 "Minus.m3"
 /* end_procedure */
#line 535 "Minus.m3"
} /* uMinus_param_u32_L */
#line 535 "Minus.m3"
 /* set_source_line */
#line 535 "Minus.m3"
#line 536 "Minus.m3"
 /* begin_procedure */
#line 536 "Minus.m3"
struct Minus__uMinus_param_u32_L_Frame_t {
#line 536 "Minus.m3"
ADDRESS _unused;
#line 536 "Minus.m3"
};
#line 536 "Minus.m3"
Long__T
__cdecl
Minus__uMinus_param_u32_L(
   /* Param_Type1 */ Minus__UINT32 a_L_979,
   /* Param_Type1 */ LONGINT b_L_980)
{
#line 536 "Minus.m3"
Minus__uMinus_param_u32_L_Frame_t _frame;
#line 536 "Minus.m3"
_frame._unused=(ADDRESS)&_frame;
#line 536 "Minus.m3"
 /* load */
#line 536 "Minus.m3"
 /* loophole */
#line 536 "Minus.m3"
 /* load */
#line 536 "Minus.m3"
 /* subtract */
#line 536 "Minus.m3"
 /* exit_proc */
#line 536 "Minus.m3"
return ((UINT64)(((UINT64)((INT64)((INT64)(a_L_979))))-((UINT64)(b_L_980))));
#line 536 "Minus.m3"
 /* end_procedure */
#line 536 "Minus.m3"
} /* Minus_param_u32_L */
#line 536 "Minus.m3"
 /* set_source_line */
#line 536 "Minus.m3"
#line 537 "Minus.m3"
 /* begin_procedure */
#line 537 "Minus.m3"
struct Minus__Minus_param_u32_L_Frame_t {
#line 537 "Minus.m3"
ADDRESS _unused;
#line 537 "Minus.m3"
};
#line 537 "Minus.m3"
LONGINT
__cdecl
Minus__Minus_param_u32_L(
   /* Param_Type1 */ Minus__UINT32 a_L_982,
   /* Param_Type1 */ LONGINT b_L_983)
{
#line 537 "Minus.m3"
Minus__Minus_param_u32_L_Frame_t _frame;
#line 537 "Minus.m3"
_frame._unused=(ADDRESS)&_frame;
#line 537 "Minus.m3"
 /* load */
#line 537 "Minus.m3"
 /* loophole */
#line 537 "Minus.m3"
 /* load */
#line 537 "Minus.m3"
 /* subtract */
#line 537 "Minus.m3"
 /* exit_proc */
#line 537 "Minus.m3"
return ((INT64)(((INT64)((INT64)((INT64)(a_L_982))))- b_L_983));
#line 537 "Minus.m3"
 /* end_procedure */
#line 537 "Minus.m3"
} /* uMinus_var_u8_i8 */
#line 537 "Minus.m3"
 /* set_source_line */
#line 537 "Minus.m3"
#line 538 "Minus.m3"
 /* begin_procedure */
#line 538 "Minus.m3"
struct Minus__uMinus_var_u8_i8_Frame_t {
#line 538 "Minus.m3"
ADDRESS _unused;
#line 538 "Minus.m3"
};
#line 538 "Minus.m3"
Word__T
__cdecl
Minus__uMinus_var_u8_i8(void)
{
#line 538 "Minus.m3"
Minus__uMinus_var_u8_i8_Frame_t _frame;
#line 538 "Minus.m3"
_frame._unused=(ADDRESS)&_frame;
#line 538 "Minus.m3"
 /* load */
#line 538 "Minus.m3"
 /* load */
#line 538 "Minus.m3"
 /* subtract */
#line 538 "Minus.m3"
 /* exit_proc */
#line 538 "Minus.m3"
return ((UINT64)(((UINT64)(((INT64)(*((UINT8*)(INT64_(188)+((ADDRESS)(&Minus_m_M_Minus_L_13))))))))-((UINT64)(((INT64)(*((INT8*)(INT64_(104)+((ADDRESS)(&Minus_m_M_Minus_L_13))))))))));
#line 538 "Minus.m3"
 /* end_procedure */
#line 538 "Minus.m3"
} /* Minus_var_u8_i8 */
#line 538 "Minus.m3"
 /* set_source_line */
#line 538 "Minus.m3"
#line 539 "Minus.m3"
 /* begin_procedure */
#line 539 "Minus.m3"
struct Minus__Minus_var_u8_i8_Frame_t {
#line 539 "Minus.m3"
ADDRESS _unused;
#line 539 "Minus.m3"
};
#line 539 "Minus.m3"
Minus__UINT8
__cdecl
Minus__Minus_var_u8_i8(void)
{
#line 539 "Minus.m3"
 /* Var_Type2 */ INT64 Minus_m_1368_L_1369={0};//always-init
#line 539 "Minus.m3"
Minus__Minus_var_u8_i8_Frame_t _frame;
#line 539 "Minus.m3"
_frame._unused=(ADDRESS)&_frame;
#line 539 "Minus.m3"
 /* load */
#line 539 "Minus.m3"
 /* load */
#line 539 "Minus.m3"
 /* subtract */
#line 539 "Minus.m3"
 /* check_range */
#line 539 "Minus.m3"
 /* store */
#line 539 "Minus.m3"
(*(INT64*)(&Minus_m_1368_L_1369))=(INT64)( ((INT64)( ((INT64)(*((UINT8*)(INT64_(188)+((ADDRESS)(&Minus_m_M_Minus_L_13))))))- ((INT64)(*((INT8*)(INT64_(104)+((ADDRESS)(&Minus_m_M_Minus_L_13)))))))));
#line 539 "Minus.m3"
 /* load */
#line 539 "Minus.m3"
if(m3_check_range(INT64,
Minus_m_1368_L_1369,
 INT64_(0),
 INT64_(255)))
#line 539 "Minus.m3"
Minus_m_M_Minus_L_13_CRASH(17249);
#line 539 "Minus.m3"
 /* exit_proc */
#line 539 "Minus.m3"
return Minus_m_1368_L_1369;
#line 539 "Minus.m3"
 /* end_procedure */
#line 539 "Minus.m3"
} /* uMinus_param_u8_i8 */
#line 539 "Minus.m3"
 /* set_source_line */
#line 539 "Minus.m3"
#line 540 "Minus.m3"
 /* begin_procedure */
#line 540 "Minus.m3"
struct Minus__uMinus_param_u8_i8_Frame_t {
#line 540 "Minus.m3"
ADDRESS _unused;
#line 540 "Minus.m3"
};
#line 540 "Minus.m3"
Word__T
__cdecl
Minus__uMinus_param_u8_i8(
   /* Param_Type1 */ Minus__UINT8 a_L_987,
   /* Param_Type1 */ Minus__INT8 b_L_988)
{
#line 540 "Minus.m3"
Minus__uMinus_param_u8_i8_Frame_t _frame;
#line 540 "Minus.m3"
_frame._unused=(ADDRESS)&_frame;
#line 540 "Minus.m3"
 /* load */
#line 540 "Minus.m3"
 /* load */
#line 540 "Minus.m3"
 /* subtract */
#line 540 "Minus.m3"
 /* exit_proc */
#line 540 "Minus.m3"
return ((UINT64)(((UINT64)(((INT64)(a_L_987))))-((UINT64)(((INT64)(b_L_988))))));
#line 540 "Minus.m3"
 /* end_procedure */
#line 540 "Minus.m3"
} /* Minus_param_u8_i8 */
#line 540 "Minus.m3"
 /* set_source_line */
#line 540 "Minus.m3"
#line 541 "Minus.m3"
 /* begin_procedure */
#line 541 "Minus.m3"
struct Minus__Minus_param_u8_i8_Frame_t {
#line 541 "Minus.m3"
ADDRESS _unused;
#line 541 "Minus.m3"
};
#line 541 "Minus.m3"
Minus__UINT8
__cdecl
Minus__Minus_param_u8_i8(
   /* Param_Type1 */ Minus__UINT8 a_L_990,
   /* Param_Type1 */ Minus__INT8 b_L_991)
{
#line 541 "Minus.m3"
 /* Var_Type2 */ INT64 Minus_m_1370_L_1371={0};//always-init
#line 541 "Minus.m3"
Minus__Minus_param_u8_i8_Frame_t _frame;
#line 541 "Minus.m3"
_frame._unused=(ADDRESS)&_frame;
#line 541 "Minus.m3"
 /* load */
#line 541 "Minus.m3"
 /* load */
#line 541 "Minus.m3"
 /* subtract */
#line 541 "Minus.m3"
 /* check_range */
#line 541 "Minus.m3"
 /* store */
#line 541 "Minus.m3"
(*(INT64*)(&Minus_m_1370_L_1371))=(INT64)( ((INT64)( ((INT64)(a_L_990))- ((INT64)(b_L_991)))));
#line 541 "Minus.m3"
 /* load */
#line 541 "Minus.m3"
if(m3_check_range(INT64,
Minus_m_1370_L_1371,
 INT64_(0),
 INT64_(255)))
#line 541 "Minus.m3"
Minus_m_M_Minus_L_13_CRASH(17313);
#line 541 "Minus.m3"
 /* exit_proc */
#line 541 "Minus.m3"
return Minus_m_1370_L_1371;
#line 541 "Minus.m3"
 /* end_procedure */
#line 541 "Minus.m3"
} /* uMinus_var_u8_u64 */
#line 541 "Minus.m3"
 /* set_source_line */
#line 541 "Minus.m3"
#line 542 "Minus.m3"
 /* begin_procedure */
#line 542 "Minus.m3"
struct Minus__uMinus_var_u8_u64_Frame_t {
#line 542 "Minus.m3"
ADDRESS _unused;
#line 542 "Minus.m3"
};
#line 542 "Minus.m3"
Long__T
__cdecl
Minus__uMinus_var_u8_u64(void)
{
#line 542 "Minus.m3"
Minus__uMinus_var_u8_u64_Frame_t _frame;
#line 542 "Minus.m3"
_frame._unused=(ADDRESS)&_frame;
#line 542 "Minus.m3"
 /* load */
#line 542 "Minus.m3"
 /* loophole */
#line 542 "Minus.m3"
 /* load */
#line 542 "Minus.m3"
 /* subtract */
#line 542 "Minus.m3"
 /* exit_proc */
#line 542 "Minus.m3"
return ((UINT64)(((UINT64)((INT64)((INT64)(*((UINT8*)(INT64_(188)+((ADDRESS)(&Minus_m_M_Minus_L_13))))))))-((UINT64)(*((INT64*)(INT64_(112)+((ADDRESS)(&Minus_m_M_Minus_L_13))))))));
#line 542 "Minus.m3"
 /* end_procedure */
#line 542 "Minus.m3"
} /* Minus_var_u8_u64 */
#line 542 "Minus.m3"
 /* set_source_line */
#line 542 "Minus.m3"
#line 543 "Minus.m3"
 /* begin_procedure */
#line 543 "Minus.m3"
struct Minus__Minus_var_u8_u64_Frame_t {
#line 543 "Minus.m3"
ADDRESS _unused;
#line 543 "Minus.m3"
};
#line 543 "Minus.m3"
LONGINT
__cdecl
Minus__Minus_var_u8_u64(void)
{
#line 543 "Minus.m3"
Minus__Minus_var_u8_u64_Frame_t _frame;
#line 543 "Minus.m3"
_frame._unused=(ADDRESS)&_frame;
#line 543 "Minus.m3"
 /* load */
#line 543 "Minus.m3"
 /* loophole */
#line 543 "Minus.m3"
 /* load */
#line 543 "Minus.m3"
 /* subtract */
#line 543 "Minus.m3"
 /* exit_proc */
#line 543 "Minus.m3"
return ((INT64)(((INT64)((INT64)((INT64)(*((UINT8*)(INT64_(188)+((ADDRESS)(&Minus_m_M_Minus_L_13))))))))-((INT64)(*((INT64*)(INT64_(112)+((ADDRESS)(&Minus_m_M_Minus_L_13))))))));
#line 543 "Minus.m3"
 /* end_procedure */
#line 543 "Minus.m3"
} /* uMinus_param_u8_u64 */
#line 543 "Minus.m3"
 /* set_source_line */
#line 543 "Minus.m3"
#line 544 "Minus.m3"
 /* begin_procedure */
#line 544 "Minus.m3"
struct Minus__uMinus_param_u8_u64_Frame_t {
#line 544 "Minus.m3"
ADDRESS _unused;
#line 544 "Minus.m3"
};
#line 544 "Minus.m3"
Long__T
__cdecl
Minus__uMinus_param_u8_u64(
   /* Param_Type1 */ Minus__UINT8 a_L_995,
   /* Param_Type1 */ Minus__UINT64 b_L_996)
{
#line 544 "Minus.m3"
Minus__uMinus_param_u8_u64_Frame_t _frame;
#line 544 "Minus.m3"
_frame._unused=(ADDRESS)&_frame;
#line 544 "Minus.m3"
 /* load */
#line 544 "Minus.m3"
 /* loophole */
#line 544 "Minus.m3"
 /* load */
#line 544 "Minus.m3"
 /* subtract */
#line 544 "Minus.m3"
 /* exit_proc */
#line 544 "Minus.m3"
return ((UINT64)(((UINT64)((INT64)((INT64)(a_L_995))))-((UINT64)(b_L_996))));
#line 544 "Minus.m3"
 /* end_procedure */
#line 544 "Minus.m3"
} /* Minus_param_u8_u64 */
#line 544 "Minus.m3"
 /* set_source_line */
#line 544 "Minus.m3"
#line 545 "Minus.m3"
 /* begin_procedure */
#line 545 "Minus.m3"
struct Minus__Minus_param_u8_u64_Frame_t {
#line 545 "Minus.m3"
ADDRESS _unused;
#line 545 "Minus.m3"
};
#line 545 "Minus.m3"
LONGINT
__cdecl
Minus__Minus_param_u8_u64(
   /* Param_Type1 */ Minus__UINT8 a_L_998,
   /* Param_Type1 */ Minus__UINT64 b_L_999)
{
#line 545 "Minus.m3"
Minus__Minus_param_u8_u64_Frame_t _frame;
#line 545 "Minus.m3"
_frame._unused=(ADDRESS)&_frame;
#line 545 "Minus.m3"
 /* load */
#line 545 "Minus.m3"
 /* loophole */
#line 545 "Minus.m3"
 /* load */
#line 545 "Minus.m3"
 /* subtract */
#line 545 "Minus.m3"
 /* exit_proc */
#line 545 "Minus.m3"
return ((INT64)(((INT64)((INT64)((INT64)(a_L_998))))- b_L_999));
#line 545 "Minus.m3"
 /* end_procedure */
#line 545 "Minus.m3"
} /* uMinus_var_u8_i32 */
#line 545 "Minus.m3"
 /* set_source_line */
#line 545 "Minus.m3"
#line 546 "Minus.m3"
 /* begin_procedure */
#line 546 "Minus.m3"
struct Minus__uMinus_var_u8_i32_Frame_t {
#line 546 "Minus.m3"
ADDRESS _unused;
#line 546 "Minus.m3"
};
#line 546 "Minus.m3"
Word__T
__cdecl
Minus__uMinus_var_u8_i32(void)
{
#line 546 "Minus.m3"
Minus__uMinus_var_u8_i32_Frame_t _frame;
#line 546 "Minus.m3"
_frame._unused=(ADDRESS)&_frame;
#line 546 "Minus.m3"
 /* load */
#line 546 "Minus.m3"
 /* load */
#line 546 "Minus.m3"
 /* subtract */
#line 546 "Minus.m3"
 /* exit_proc */
#line 546 "Minus.m3"
return ((UINT64)(((UINT64)(((INT64)(*((UINT8*)(INT64_(188)+((ADDRESS)(&Minus_m_M_Minus_L_13))))))))-((UINT64)(((INT64)(*((INT32*)(INT64_(128)+((ADDRESS)(&Minus_m_M_Minus_L_13))))))))));
#line 546 "Minus.m3"
 /* end_procedure */
#line 546 "Minus.m3"
} /* Minus_var_u8_i32 */
#line 546 "Minus.m3"
 /* set_source_line */
#line 546 "Minus.m3"
#line 547 "Minus.m3"
 /* begin_procedure */
#line 547 "Minus.m3"
struct Minus__Minus_var_u8_i32_Frame_t {
#line 547 "Minus.m3"
ADDRESS _unused;
#line 547 "Minus.m3"
};
#line 547 "Minus.m3"
Minus__UINT8
__cdecl
Minus__Minus_var_u8_i32(void)
{
#line 547 "Minus.m3"
 /* Var_Type2 */ INT64 Minus_m_1372_L_1373={0};//always-init
#line 547 "Minus.m3"
Minus__Minus_var_u8_i32_Frame_t _frame;
#line 547 "Minus.m3"
_frame._unused=(ADDRESS)&_frame;
#line 547 "Minus.m3"
 /* load */
#line 547 "Minus.m3"
 /* load */
#line 547 "Minus.m3"
 /* subtract */
#line 547 "Minus.m3"
 /* check_range */
#line 547 "Minus.m3"
 /* store */
#line 547 "Minus.m3"
(*(INT64*)(&Minus_m_1372_L_1373))=(INT64)( ((INT64)( ((INT64)(*((UINT8*)(INT64_(188)+((ADDRESS)(&Minus_m_M_Minus_L_13))))))- ((INT64)(*((INT32*)(INT64_(128)+((ADDRESS)(&Minus_m_M_Minus_L_13)))))))));
#line 547 "Minus.m3"
 /* load */
#line 547 "Minus.m3"
if(m3_check_range(INT64,
Minus_m_1372_L_1373,
 INT64_(0),
 INT64_(255)))
#line 547 "Minus.m3"
Minus_m_M_Minus_L_13_CRASH(17505);
#line 547 "Minus.m3"
 /* exit_proc */
#line 547 "Minus.m3"
return Minus_m_1372_L_1373;
#line 547 "Minus.m3"
 /* end_procedure */
#line 547 "Minus.m3"
} /* uMinus_param_u8_i32 */
#line 547 "Minus.m3"
 /* set_source_line */
#line 547 "Minus.m3"
#line 548 "Minus.m3"
 /* begin_procedure */
#line 548 "Minus.m3"
struct Minus__uMinus_param_u8_i32_Frame_t {
#line 548 "Minus.m3"
ADDRESS _unused;
#line 548 "Minus.m3"
};
#line 548 "Minus.m3"
Word__T
__cdecl
Minus__uMinus_param_u8_i32(
   /* Param_Type1 */ Minus__UINT8 a_L_1003,
   /* Param_Type1 */ Minus__INT32 b_L_1004)
{
#line 548 "Minus.m3"
Minus__uMinus_param_u8_i32_Frame_t _frame;
#line 548 "Minus.m3"
_frame._unused=(ADDRESS)&_frame;
#line 548 "Minus.m3"
 /* load */
#line 548 "Minus.m3"
 /* load */
#line 548 "Minus.m3"
 /* subtract */
#line 548 "Minus.m3"
 /* exit_proc */
#line 548 "Minus.m3"
return ((UINT64)(((UINT64)(((INT64)(a_L_1003))))-((UINT64)(((INT64)(b_L_1004))))));
#line 548 "Minus.m3"
 /* end_procedure */
#line 548 "Minus.m3"
} /* Minus_param_u8_i32 */
#line 548 "Minus.m3"
 /* set_source_line */
#line 548 "Minus.m3"
#line 549 "Minus.m3"
 /* begin_procedure */
#line 549 "Minus.m3"
struct Minus__Minus_param_u8_i32_Frame_t {
#line 549 "Minus.m3"
ADDRESS _unused;
#line 549 "Minus.m3"
};
#line 549 "Minus.m3"
Minus__UINT8
__cdecl
Minus__Minus_param_u8_i32(
   /* Param_Type1 */ Minus__UINT8 a_L_1006,
   /* Param_Type1 */ Minus__INT32 b_L_1007)
{
#line 549 "Minus.m3"
 /* Var_Type2 */ INT64 Minus_m_1374_L_1375={0};//always-init
#line 549 "Minus.m3"
Minus__Minus_param_u8_i32_Frame_t _frame;
#line 549 "Minus.m3"
_frame._unused=(ADDRESS)&_frame;
#line 549 "Minus.m3"
 /* load */
#line 549 "Minus.m3"
 /* load */
#line 549 "Minus.m3"
 /* subtract */
#line 549 "Minus.m3"
 /* check_range */
#line 549 "Minus.m3"
 /* store */
#line 549 "Minus.m3"
(*(INT64*)(&Minus_m_1374_L_1375))=(INT64)( ((INT64)( ((INT64)(a_L_1006))- ((INT64)(b_L_1007)))));
#line 549 "Minus.m3"
 /* load */
#line 549 "Minus.m3"
if(m3_check_range(INT64,
Minus_m_1374_L_1375,
 INT64_(0),
 INT64_(255)))
#line 549 "Minus.m3"
Minus_m_M_Minus_L_13_CRASH(17569);
#line 549 "Minus.m3"
 /* exit_proc */
#line 549 "Minus.m3"
return Minus_m_1374_L_1375;
#line 549 "Minus.m3"
 /* end_procedure */
#line 549 "Minus.m3"
} /* uMinus_var_u8_LC */
#line 549 "Minus.m3"
 /* set_source_line */
#line 549 "Minus.m3"
#line 550 "Minus.m3"
 /* begin_procedure */
#line 550 "Minus.m3"
struct Minus__uMinus_var_u8_LC_Frame_t {
#line 550 "Minus.m3"
ADDRESS _unused;
#line 550 "Minus.m3"
};
#line 550 "Minus.m3"
Long__T
__cdecl
Minus__uMinus_var_u8_LC(void)
{
#line 550 "Minus.m3"
Minus__uMinus_var_u8_LC_Frame_t _frame;
#line 550 "Minus.m3"
_frame._unused=(ADDRESS)&_frame;
#line 550 "Minus.m3"
 /* load */
#line 550 "Minus.m3"
 /* loophole */
#line 550 "Minus.m3"
 /* load */
#line 550 "Minus.m3"
 /* subtract */
#line 550 "Minus.m3"
 /* exit_proc */
#line 550 "Minus.m3"
return ((UINT64)(((UINT64)((INT64)((INT64)(*((UINT8*)(INT64_(188)+((ADDRESS)(&Minus_m_M_Minus_L_13))))))))-((UINT64)(((INT64)(*((UINT64*)(INT64_(136)+((ADDRESS)(&Minus_m_M_Minus_L_13))))))))));
#line 550 "Minus.m3"
 /* end_procedure */
#line 550 "Minus.m3"
} /* Minus_var_u8_LC */
#line 550 "Minus.m3"
 /* set_source_line */
#line 550 "Minus.m3"
#line 551 "Minus.m3"
 /* begin_procedure */
#line 551 "Minus.m3"
struct Minus__Minus_var_u8_LC_Frame_t {
#line 551 "Minus.m3"
ADDRESS _unused;
#line 551 "Minus.m3"
};
#line 551 "Minus.m3"
LONGINT
__cdecl
Minus__Minus_var_u8_LC(void)
{
#line 551 "Minus.m3"
Minus__Minus_var_u8_LC_Frame_t _frame;
#line 551 "Minus.m3"
_frame._unused=(ADDRESS)&_frame;
#line 551 "Minus.m3"
 /* load */
#line 551 "Minus.m3"
 /* loophole */
#line 551 "Minus.m3"
 /* load */
#line 551 "Minus.m3"
 /* subtract */
#line 551 "Minus.m3"
 /* exit_proc */
#line 551 "Minus.m3"
return ((INT64)(((INT64)((INT64)((INT64)(*((UINT8*)(INT64_(188)+((ADDRESS)(&Minus_m_M_Minus_L_13))))))))- ((INT64)(*((UINT64*)(INT64_(136)+((ADDRESS)(&Minus_m_M_Minus_L_13))))))));
#line 551 "Minus.m3"
 /* end_procedure */
#line 551 "Minus.m3"
} /* uMinus_param_u8_LC */
#line 551 "Minus.m3"
 /* set_source_line */
#line 551 "Minus.m3"
#line 552 "Minus.m3"
 /* begin_procedure */
#line 552 "Minus.m3"
struct Minus__uMinus_param_u8_LC_Frame_t {
#line 552 "Minus.m3"
ADDRESS _unused;
#line 552 "Minus.m3"
};
#line 552 "Minus.m3"
Long__T
__cdecl
Minus__uMinus_param_u8_LC(
   /* Param_Type1 */ Minus__UINT8 a_L_1011,
   /* Param_Type1 */ LONGCARD b_L_1012)
{
#line 552 "Minus.m3"
Minus__uMinus_param_u8_LC_Frame_t _frame;
#line 552 "Minus.m3"
_frame._unused=(ADDRESS)&_frame;
#line 552 "Minus.m3"
 /* load */
#line 552 "Minus.m3"
 /* loophole */
#line 552 "Minus.m3"
 /* load */
#line 552 "Minus.m3"
 /* subtract */
#line 552 "Minus.m3"
 /* exit_proc */
#line 552 "Minus.m3"
return ((UINT64)(((UINT64)((INT64)((INT64)(a_L_1011))))-((UINT64)(((INT64)(b_L_1012))))));
#line 552 "Minus.m3"
 /* end_procedure */
#line 552 "Minus.m3"
} /* Minus_param_u8_LC */
#line 552 "Minus.m3"
 /* set_source_line */
#line 552 "Minus.m3"
#line 553 "Minus.m3"
 /* begin_procedure */
#line 553 "Minus.m3"
struct Minus__Minus_param_u8_LC_Frame_t {
#line 553 "Minus.m3"
ADDRESS _unused;
#line 553 "Minus.m3"
};
#line 553 "Minus.m3"
LONGINT
__cdecl
Minus__Minus_param_u8_LC(
   /* Param_Type1 */ Minus__UINT8 a_L_1014,
   /* Param_Type1 */ LONGCARD b_L_1015)
{
#line 553 "Minus.m3"
Minus__Minus_param_u8_LC_Frame_t _frame;
#line 553 "Minus.m3"
_frame._unused=(ADDRESS)&_frame;
#line 553 "Minus.m3"
 /* load */
#line 553 "Minus.m3"
 /* loophole */
#line 553 "Minus.m3"
 /* load */
#line 553 "Minus.m3"
 /* subtract */
#line 553 "Minus.m3"
 /* exit_proc */
#line 553 "Minus.m3"
return ((INT64)(((INT64)((INT64)((INT64)(a_L_1014))))- ((INT64)(b_L_1015))));
#line 553 "Minus.m3"
 /* end_procedure */
#line 553 "Minus.m3"
} /* uMinus_var_u8_u16 */
#line 553 "Minus.m3"
 /* set_source_line */
#line 553 "Minus.m3"
#line 554 "Minus.m3"
 /* begin_procedure */
#line 554 "Minus.m3"
struct Minus__uMinus_var_u8_u16_Frame_t {
#line 554 "Minus.m3"
ADDRESS _unused;
#line 554 "Minus.m3"
};
#line 554 "Minus.m3"
Word__T
__cdecl
Minus__uMinus_var_u8_u16(void)
{
#line 554 "Minus.m3"
Minus__uMinus_var_u8_u16_Frame_t _frame;
#line 554 "Minus.m3"
_frame._unused=(ADDRESS)&_frame;
#line 554 "Minus.m3"
 /* load */
#line 554 "Minus.m3"
 /* load */
#line 554 "Minus.m3"
 /* subtract */
#line 554 "Minus.m3"
 /* exit_proc */
#line 554 "Minus.m3"
return ((UINT64)(((UINT64)(((INT64)(*((UINT8*)(INT64_(188)+((ADDRESS)(&Minus_m_M_Minus_L_13))))))))-((UINT64)(((INT64)(*((UINT16*)(INT64_(144)+((ADDRESS)(&Minus_m_M_Minus_L_13))))))))));
#line 554 "Minus.m3"
 /* end_procedure */
#line 554 "Minus.m3"
} /* Minus_var_u8_u16 */
#line 554 "Minus.m3"
 /* set_source_line */
#line 554 "Minus.m3"
#line 555 "Minus.m3"
 /* begin_procedure */
#line 555 "Minus.m3"
struct Minus__Minus_var_u8_u16_Frame_t {
#line 555 "Minus.m3"
ADDRESS _unused;
#line 555 "Minus.m3"
};
#line 555 "Minus.m3"
Minus__UINT8
__cdecl
Minus__Minus_var_u8_u16(void)
{
#line 555 "Minus.m3"
 /* Var_Type2 */ INT64 Minus_m_1376_L_1377={0};//always-init
#line 555 "Minus.m3"
Minus__Minus_var_u8_u16_Frame_t _frame;
#line 555 "Minus.m3"
_frame._unused=(ADDRESS)&_frame;
#line 555 "Minus.m3"
 /* load */
#line 555 "Minus.m3"
 /* load */
#line 555 "Minus.m3"
 /* subtract */
#line 555 "Minus.m3"
 /* check_lo */
#line 555 "Minus.m3"
 /* store */
#line 555 "Minus.m3"
(*(INT64*)(&Minus_m_1376_L_1377))=(INT64)( ((INT64)( ((INT64)(*((UINT8*)(INT64_(188)+((ADDRESS)(&Minus_m_M_Minus_L_13))))))- ((INT64)(*((UINT16*)(INT64_(144)+((ADDRESS)(&Minus_m_M_Minus_L_13)))))))));
#line 555 "Minus.m3"
 /* load */
#line 555 "Minus.m3"
/*check_lo*/if(Minus_m_1376_L_1377<INT64_(0))Minus_m_M_Minus_L_13_CRASH(17761);
#line 555 "Minus.m3"
 /* exit_proc */
#line 555 "Minus.m3"
return Minus_m_1376_L_1377;
#line 555 "Minus.m3"
 /* end_procedure */
#line 555 "Minus.m3"
} /* uMinus_param_u8_u16 */
#line 555 "Minus.m3"
 /* set_source_line */
#line 555 "Minus.m3"
#line 556 "Minus.m3"
 /* begin_procedure */
#line 556 "Minus.m3"
struct Minus__uMinus_param_u8_u16_Frame_t {
#line 556 "Minus.m3"
ADDRESS _unused;
#line 556 "Minus.m3"
};
#line 556 "Minus.m3"
Word__T
__cdecl
Minus__uMinus_param_u8_u16(
   /* Param_Type1 */ Minus__UINT8 a_L_1019,
   /* Param_Type1 */ Minus__UINT16 b_L_1020)
{
#line 556 "Minus.m3"
Minus__uMinus_param_u8_u16_Frame_t _frame;
#line 556 "Minus.m3"
_frame._unused=(ADDRESS)&_frame;
#line 556 "Minus.m3"
 /* load */
#line 556 "Minus.m3"
 /* load */
#line 556 "Minus.m3"
 /* subtract */
#line 556 "Minus.m3"
 /* exit_proc */
#line 556 "Minus.m3"
return ((UINT64)(((UINT64)(((INT64)(a_L_1019))))-((UINT64)(((INT64)(b_L_1020))))));
#line 556 "Minus.m3"
 /* end_procedure */
#line 556 "Minus.m3"
} /* Minus_param_u8_u16 */
#line 556 "Minus.m3"
 /* set_source_line */
#line 556 "Minus.m3"
#line 557 "Minus.m3"
 /* begin_procedure */
#line 557 "Minus.m3"
struct Minus__Minus_param_u8_u16_Frame_t {
#line 557 "Minus.m3"
ADDRESS _unused;
#line 557 "Minus.m3"
};
#line 557 "Minus.m3"
Minus__UINT8
__cdecl
Minus__Minus_param_u8_u16(
   /* Param_Type1 */ Minus__UINT8 a_L_1022,
   /* Param_Type1 */ Minus__UINT16 b_L_1023)
{
#line 557 "Minus.m3"
 /* Var_Type2 */ INT64 Minus_m_1378_L_1379={0};//always-init
#line 557 "Minus.m3"
Minus__Minus_param_u8_u16_Frame_t _frame;
#line 557 "Minus.m3"
_frame._unused=(ADDRESS)&_frame;
#line 557 "Minus.m3"
 /* load */
#line 557 "Minus.m3"
 /* load */
#line 557 "Minus.m3"
 /* subtract */
#line 557 "Minus.m3"
 /* check_lo */
#line 557 "Minus.m3"
 /* store */
#line 557 "Minus.m3"
(*(INT64*)(&Minus_m_1378_L_1379))=(INT64)( ((INT64)( ((INT64)(a_L_1022))- ((INT64)(b_L_1023)))));
#line 557 "Minus.m3"
 /* load */
#line 557 "Minus.m3"
/*check_lo*/if(Minus_m_1378_L_1379<INT64_(0))Minus_m_M_Minus_L_13_CRASH(17825);
#line 557 "Minus.m3"
 /* exit_proc */
#line 557 "Minus.m3"
return Minus_m_1378_L_1379;
#line 557 "Minus.m3"
 /* end_procedure */
#line 557 "Minus.m3"
} /* uMinus_var_u8_I */
#line 557 "Minus.m3"
 /* set_source_line */
#line 557 "Minus.m3"
#line 558 "Minus.m3"
 /* begin_procedure */
#line 558 "Minus.m3"
struct Minus__uMinus_var_u8_I_Frame_t {
#line 558 "Minus.m3"
ADDRESS _unused;
#line 558 "Minus.m3"
};
#line 558 "Minus.m3"
Word__T
__cdecl
Minus__uMinus_var_u8_I(void)
{
#line 558 "Minus.m3"
Minus__uMinus_var_u8_I_Frame_t _frame;
#line 558 "Minus.m3"
_frame._unused=(ADDRESS)&_frame;
#line 558 "Minus.m3"
 /* load */
#line 558 "Minus.m3"
 /* load */
#line 558 "Minus.m3"
 /* subtract */
#line 558 "Minus.m3"
 /* exit_proc */
#line 558 "Minus.m3"
return ((UINT64)(((UINT64)(((INT64)(*((UINT8*)(INT64_(188)+((ADDRESS)(&Minus_m_M_Minus_L_13))))))))-((UINT64)(*((INT64*)(INT64_(152)+((ADDRESS)(&Minus_m_M_Minus_L_13))))))));
#line 558 "Minus.m3"
 /* end_procedure */
#line 558 "Minus.m3"
} /* Minus_var_u8_I */
#line 558 "Minus.m3"
 /* set_source_line */
#line 558 "Minus.m3"
#line 559 "Minus.m3"
 /* begin_procedure */
#line 559 "Minus.m3"
struct Minus__Minus_var_u8_I_Frame_t {
#line 559 "Minus.m3"
ADDRESS _unused;
#line 559 "Minus.m3"
};
#line 559 "Minus.m3"
Minus__UINT8
__cdecl
Minus__Minus_var_u8_I(void)
{
#line 559 "Minus.m3"
 /* Var_Type2 */ INT64 Minus_m_1380_L_1381={0};//always-init
#line 559 "Minus.m3"
Minus__Minus_var_u8_I_Frame_t _frame;
#line 559 "Minus.m3"
_frame._unused=(ADDRESS)&_frame;
#line 559 "Minus.m3"
 /* load */
#line 559 "Minus.m3"
 /* load */
#line 559 "Minus.m3"
 /* subtract */
#line 559 "Minus.m3"
 /* check_range */
#line 559 "Minus.m3"
 /* store */
#line 559 "Minus.m3"
(*(INT64*)(&Minus_m_1380_L_1381))=(INT64)( ((INT64)( ((INT64)(*((UINT8*)(INT64_(188)+((ADDRESS)(&Minus_m_M_Minus_L_13))))))-((INT64)(*((INT64*)(INT64_(152)+((ADDRESS)(&Minus_m_M_Minus_L_13)))))))));
#line 559 "Minus.m3"
 /* load */
#line 559 "Minus.m3"
if(m3_check_range(INT64,
Minus_m_1380_L_1381,
 INT64_(0),
 INT64_(255)))
#line 559 "Minus.m3"
Minus_m_M_Minus_L_13_CRASH(17889);
#line 559 "Minus.m3"
 /* exit_proc */
#line 559 "Minus.m3"
return Minus_m_1380_L_1381;
#line 559 "Minus.m3"
 /* end_procedure */
#line 559 "Minus.m3"
} /* uMinus_param_u8_I */
#line 559 "Minus.m3"
 /* set_source_line */
#line 559 "Minus.m3"
#line 560 "Minus.m3"
 /* begin_procedure */
#line 560 "Minus.m3"
struct Minus__uMinus_param_u8_I_Frame_t {
#line 560 "Minus.m3"
ADDRESS _unused;
#line 560 "Minus.m3"
};
#line 560 "Minus.m3"
Word__T
__cdecl
Minus__uMinus_param_u8_I(
   /* Param_Type1 */ Minus__UINT8 a_L_1027,
   /* Param_Type1 */ INTEGER b_L_1028)
{
#line 560 "Minus.m3"
Minus__uMinus_param_u8_I_Frame_t _frame;
#line 560 "Minus.m3"
_frame._unused=(ADDRESS)&_frame;
#line 560 "Minus.m3"
 /* load */
#line 560 "Minus.m3"
 /* load */
#line 560 "Minus.m3"
 /* subtract */
#line 560 "Minus.m3"
 /* exit_proc */
#line 560 "Minus.m3"
return ((UINT64)(((UINT64)(((INT64)(a_L_1027))))-((UINT64)(b_L_1028))));
#line 560 "Minus.m3"
 /* end_procedure */
#line 560 "Minus.m3"
} /* Minus_param_u8_I */
#line 560 "Minus.m3"
 /* set_source_line */
#line 560 "Minus.m3"
#line 561 "Minus.m3"
 /* begin_procedure */
#line 561 "Minus.m3"
struct Minus__Minus_param_u8_I_Frame_t {
#line 561 "Minus.m3"
ADDRESS _unused;
#line 561 "Minus.m3"
};
#line 561 "Minus.m3"
Minus__UINT8
__cdecl
Minus__Minus_param_u8_I(
   /* Param_Type1 */ Minus__UINT8 a_L_1030,
   /* Param_Type1 */ INTEGER b_L_1031)
{
#line 561 "Minus.m3"
 /* Var_Type2 */ INT64 Minus_m_1382_L_1383={0};//always-init
#line 561 "Minus.m3"
Minus__Minus_param_u8_I_Frame_t _frame;
#line 561 "Minus.m3"
_frame._unused=(ADDRESS)&_frame;
#line 561 "Minus.m3"
 /* load */
#line 561 "Minus.m3"
 /* load */
#line 561 "Minus.m3"
 /* subtract */
#line 561 "Minus.m3"
 /* check_range */
#line 561 "Minus.m3"
 /* store */
#line 561 "Minus.m3"
(*(INT64*)(&Minus_m_1382_L_1383))=(INT64)( ((INT64)( ((INT64)(a_L_1030))- b_L_1031)));
#line 561 "Minus.m3"
 /* load */
#line 561 "Minus.m3"
if(m3_check_range(INT64,
Minus_m_1382_L_1383,
 INT64_(0),
 INT64_(255)))
#line 561 "Minus.m3"
Minus_m_M_Minus_L_13_CRASH(17953);
#line 561 "Minus.m3"
 /* exit_proc */
#line 561 "Minus.m3"
return Minus_m_1382_L_1383;
#line 561 "Minus.m3"
 /* end_procedure */
#line 561 "Minus.m3"
} /* uMinus_var_u8_i64 */
#line 561 "Minus.m3"
 /* set_source_line */
#line 561 "Minus.m3"
#line 562 "Minus.m3"
 /* begin_procedure */
#line 562 "Minus.m3"
struct Minus__uMinus_var_u8_i64_Frame_t {
#line 562 "Minus.m3"
ADDRESS _unused;
#line 562 "Minus.m3"
};
#line 562 "Minus.m3"
Long__T
__cdecl
Minus__uMinus_var_u8_i64(void)
{
#line 562 "Minus.m3"
Minus__uMinus_var_u8_i64_Frame_t _frame;
#line 562 "Minus.m3"
_frame._unused=(ADDRESS)&_frame;
#line 562 "Minus.m3"
 /* load */
#line 562 "Minus.m3"
 /* loophole */
#line 562 "Minus.m3"
 /* load */
#line 562 "Minus.m3"
 /* subtract */
#line 562 "Minus.m3"
 /* exit_proc */
#line 562 "Minus.m3"
return ((UINT64)(((UINT64)((INT64)((INT64)(*((UINT8*)(INT64_(188)+((ADDRESS)(&Minus_m_M_Minus_L_13))))))))-((UINT64)(*((INT64*)(INT64_(160)+((ADDRESS)(&Minus_m_M_Minus_L_13))))))));
#line 562 "Minus.m3"
 /* end_procedure */
#line 562 "Minus.m3"
} /* Minus_var_u8_i64 */
#line 562 "Minus.m3"
 /* set_source_line */
#line 562 "Minus.m3"
#line 563 "Minus.m3"
 /* begin_procedure */
#line 563 "Minus.m3"
struct Minus__Minus_var_u8_i64_Frame_t {
#line 563 "Minus.m3"
ADDRESS _unused;
#line 563 "Minus.m3"
};
#line 563 "Minus.m3"
LONGINT
__cdecl
Minus__Minus_var_u8_i64(void)
{
#line 563 "Minus.m3"
Minus__Minus_var_u8_i64_Frame_t _frame;
#line 563 "Minus.m3"
_frame._unused=(ADDRESS)&_frame;
#line 563 "Minus.m3"
 /* load */
#line 563 "Minus.m3"
 /* loophole */
#line 563 "Minus.m3"
 /* load */
#line 563 "Minus.m3"
 /* subtract */
#line 563 "Minus.m3"
 /* exit_proc */
#line 563 "Minus.m3"
return ((INT64)(((INT64)((INT64)((INT64)(*((UINT8*)(INT64_(188)+((ADDRESS)(&Minus_m_M_Minus_L_13))))))))-((INT64)(*((INT64*)(INT64_(160)+((ADDRESS)(&Minus_m_M_Minus_L_13))))))));
#line 563 "Minus.m3"
 /* end_procedure */
#line 563 "Minus.m3"
} /* uMinus_param_u8_i64 */
#line 563 "Minus.m3"
 /* set_source_line */
#line 563 "Minus.m3"
#line 564 "Minus.m3"
 /* begin_procedure */
#line 564 "Minus.m3"
struct Minus__uMinus_param_u8_i64_Frame_t {
#line 564 "Minus.m3"
ADDRESS _unused;
#line 564 "Minus.m3"
};
#line 564 "Minus.m3"
Long__T
__cdecl
Minus__uMinus_param_u8_i64(
   /* Param_Type1 */ Minus__UINT8 a_L_1035,
   /* Param_Type1 */ Minus__INT64 b_L_1036)
{
#line 564 "Minus.m3"
Minus__uMinus_param_u8_i64_Frame_t _frame;
#line 564 "Minus.m3"
_frame._unused=(ADDRESS)&_frame;
#line 564 "Minus.m3"
 /* load */
#line 564 "Minus.m3"
 /* loophole */
#line 564 "Minus.m3"
 /* load */
#line 564 "Minus.m3"
 /* subtract */
#line 564 "Minus.m3"
 /* exit_proc */
#line 564 "Minus.m3"
return ((UINT64)(((UINT64)((INT64)((INT64)(a_L_1035))))-((UINT64)(b_L_1036))));
#line 564 "Minus.m3"
 /* end_procedure */
#line 564 "Minus.m3"
} /* Minus_param_u8_i64 */
#line 564 "Minus.m3"
 /* set_source_line */
#line 564 "Minus.m3"
#line 565 "Minus.m3"
 /* begin_procedure */
#line 565 "Minus.m3"
struct Minus__Minus_param_u8_i64_Frame_t {
#line 565 "Minus.m3"
ADDRESS _unused;
#line 565 "Minus.m3"
};
#line 565 "Minus.m3"
LONGINT
__cdecl
Minus__Minus_param_u8_i64(
   /* Param_Type1 */ Minus__UINT8 a_L_1038,
   /* Param_Type1 */ Minus__INT64 b_L_1039)
{
#line 565 "Minus.m3"
Minus__Minus_param_u8_i64_Frame_t _frame;
#line 565 "Minus.m3"
_frame._unused=(ADDRESS)&_frame;
#line 565 "Minus.m3"
 /* load */
#line 565 "Minus.m3"
 /* loophole */
#line 565 "Minus.m3"
 /* load */
#line 565 "Minus.m3"
 /* subtract */
#line 565 "Minus.m3"
 /* exit_proc */
#line 565 "Minus.m3"
return ((INT64)(((INT64)((INT64)((INT64)(a_L_1038))))- b_L_1039));
#line 565 "Minus.m3"
 /* end_procedure */
#line 565 "Minus.m3"
} /* uMinus_var_u8_i16 */
#line 565 "Minus.m3"
 /* set_source_line */
#line 565 "Minus.m3"
#line 566 "Minus.m3"
 /* begin_procedure */
#line 566 "Minus.m3"
struct Minus__uMinus_var_u8_i16_Frame_t {
#line 566 "Minus.m3"
ADDRESS _unused;
#line 566 "Minus.m3"
};
#line 566 "Minus.m3"
Word__T
__cdecl
Minus__uMinus_var_u8_i16(void)
{
#line 566 "Minus.m3"
Minus__uMinus_var_u8_i16_Frame_t _frame;
#line 566 "Minus.m3"
_frame._unused=(ADDRESS)&_frame;
#line 566 "Minus.m3"
 /* load */
#line 566 "Minus.m3"
 /* load */
#line 566 "Minus.m3"
 /* subtract */
#line 566 "Minus.m3"
 /* exit_proc */
#line 566 "Minus.m3"
return ((UINT64)(((UINT64)(((INT64)(*((UINT8*)(INT64_(188)+((ADDRESS)(&Minus_m_M_Minus_L_13))))))))-((UINT64)(((INT64)(*((INT16*)(INT64_(172)+((ADDRESS)(&Minus_m_M_Minus_L_13))))))))));
#line 566 "Minus.m3"
 /* end_procedure */
#line 566 "Minus.m3"
} /* Minus_var_u8_i16 */
#line 566 "Minus.m3"
 /* set_source_line */
#line 566 "Minus.m3"
#line 567 "Minus.m3"
 /* begin_procedure */
#line 567 "Minus.m3"
struct Minus__Minus_var_u8_i16_Frame_t {
#line 567 "Minus.m3"
ADDRESS _unused;
#line 567 "Minus.m3"
};
#line 567 "Minus.m3"
Minus__UINT8
__cdecl
Minus__Minus_var_u8_i16(void)
{
#line 567 "Minus.m3"
 /* Var_Type2 */ INT64 Minus_m_1384_L_1385={0};//always-init
#line 567 "Minus.m3"
Minus__Minus_var_u8_i16_Frame_t _frame;
#line 567 "Minus.m3"
_frame._unused=(ADDRESS)&_frame;
#line 567 "Minus.m3"
 /* load */
#line 567 "Minus.m3"
 /* load */
#line 567 "Minus.m3"
 /* subtract */
#line 567 "Minus.m3"
 /* check_range */
#line 567 "Minus.m3"
 /* store */
#line 567 "Minus.m3"
(*(INT64*)(&Minus_m_1384_L_1385))=(INT64)( ((INT64)( ((INT64)(*((UINT8*)(INT64_(188)+((ADDRESS)(&Minus_m_M_Minus_L_13))))))- ((INT64)(*((INT16*)(INT64_(172)+((ADDRESS)(&Minus_m_M_Minus_L_13)))))))));
#line 567 "Minus.m3"
 /* load */
#line 567 "Minus.m3"
if(m3_check_range(INT64,
Minus_m_1384_L_1385,
 INT64_(0),
 INT64_(255)))
#line 567 "Minus.m3"
Minus_m_M_Minus_L_13_CRASH(18145);
#line 567 "Minus.m3"
 /* exit_proc */
#line 567 "Minus.m3"
return Minus_m_1384_L_1385;
#line 567 "Minus.m3"
 /* end_procedure */
#line 567 "Minus.m3"
} /* uMinus_param_u8_i16 */
#line 567 "Minus.m3"
 /* set_source_line */
#line 567 "Minus.m3"
#line 568 "Minus.m3"
 /* begin_procedure */
#line 568 "Minus.m3"
struct Minus__uMinus_param_u8_i16_Frame_t {
#line 568 "Minus.m3"
ADDRESS _unused;
#line 568 "Minus.m3"
};
#line 568 "Minus.m3"
Word__T
__cdecl
Minus__uMinus_param_u8_i16(
   /* Param_Type1 */ Minus__UINT8 a_L_1043,
   /* Param_Type1 */ Minus__INT16 b_L_1044)
{
#line 568 "Minus.m3"
Minus__uMinus_param_u8_i16_Frame_t _frame;
#line 568 "Minus.m3"
_frame._unused=(ADDRESS)&_frame;
#line 568 "Minus.m3"
 /* load */
#line 568 "Minus.m3"
 /* load */
#line 568 "Minus.m3"
 /* subtract */
#line 568 "Minus.m3"
 /* exit_proc */
#line 568 "Minus.m3"
return ((UINT64)(((UINT64)(((INT64)(a_L_1043))))-((UINT64)(((INT64)(b_L_1044))))));
#line 568 "Minus.m3"
 /* end_procedure */
#line 568 "Minus.m3"
} /* Minus_param_u8_i16 */
#line 568 "Minus.m3"
 /* set_source_line */
#line 568 "Minus.m3"
#line 569 "Minus.m3"
 /* begin_procedure */
#line 569 "Minus.m3"
struct Minus__Minus_param_u8_i16_Frame_t {
#line 569 "Minus.m3"
ADDRESS _unused;
#line 569 "Minus.m3"
};
#line 569 "Minus.m3"
Minus__UINT8
__cdecl
Minus__Minus_param_u8_i16(
   /* Param_Type1 */ Minus__UINT8 a_L_1046,
   /* Param_Type1 */ Minus__INT16 b_L_1047)
{
#line 569 "Minus.m3"
 /* Var_Type2 */ INT64 Minus_m_1386_L_1387={0};//always-init
#line 569 "Minus.m3"
Minus__Minus_param_u8_i16_Frame_t _frame;
#line 569 "Minus.m3"
_frame._unused=(ADDRESS)&_frame;
#line 569 "Minus.m3"
 /* load */
#line 569 "Minus.m3"
 /* load */
#line 569 "Minus.m3"
 /* subtract */
#line 569 "Minus.m3"
 /* check_range */
#line 569 "Minus.m3"
 /* store */
#line 569 "Minus.m3"
(*(INT64*)(&Minus_m_1386_L_1387))=(INT64)( ((INT64)( ((INT64)(a_L_1046))- ((INT64)(b_L_1047)))));
#line 569 "Minus.m3"
 /* load */
#line 569 "Minus.m3"
if(m3_check_range(INT64,
Minus_m_1386_L_1387,
 INT64_(0),
 INT64_(255)))
#line 569 "Minus.m3"
Minus_m_M_Minus_L_13_CRASH(18209);
#line 569 "Minus.m3"
 /* exit_proc */
#line 569 "Minus.m3"
return Minus_m_1386_L_1387;
#line 569 "Minus.m3"
 /* end_procedure */
#line 569 "Minus.m3"
} /* uMinus_var_u8_C */
#line 569 "Minus.m3"
 /* set_source_line */
#line 569 "Minus.m3"
#line 570 "Minus.m3"
 /* begin_procedure */
#line 570 "Minus.m3"
struct Minus__uMinus_var_u8_C_Frame_t {
#line 570 "Minus.m3"
ADDRESS _unused;
#line 570 "Minus.m3"
};
#line 570 "Minus.m3"
Word__T
__cdecl
Minus__uMinus_var_u8_C(void)
{
#line 570 "Minus.m3"
Minus__uMinus_var_u8_C_Frame_t _frame;
#line 570 "Minus.m3"
_frame._unused=(ADDRESS)&_frame;
#line 570 "Minus.m3"
 /* load */
#line 570 "Minus.m3"
 /* load */
#line 570 "Minus.m3"
 /* subtract */
#line 570 "Minus.m3"
 /* exit_proc */
#line 570 "Minus.m3"
return ((UINT64)(((UINT64)(((INT64)(*((UINT8*)(INT64_(188)+((ADDRESS)(&Minus_m_M_Minus_L_13))))))))-((UINT64)(((INT64)(*((UINT64*)(INT64_(176)+((ADDRESS)(&Minus_m_M_Minus_L_13))))))))));
#line 570 "Minus.m3"
 /* end_procedure */
#line 570 "Minus.m3"
} /* Minus_var_u8_C */
#line 570 "Minus.m3"
 /* set_source_line */
#line 570 "Minus.m3"
#line 571 "Minus.m3"
 /* begin_procedure */
#line 571 "Minus.m3"
struct Minus__Minus_var_u8_C_Frame_t {
#line 571 "Minus.m3"
ADDRESS _unused;
#line 571 "Minus.m3"
};
#line 571 "Minus.m3"
Minus__UINT8
__cdecl
Minus__Minus_var_u8_C(void)
{
#line 571 "Minus.m3"
 /* Var_Type2 */ INT64 Minus_m_1388_L_1389={0};//always-init
#line 571 "Minus.m3"
Minus__Minus_var_u8_C_Frame_t _frame;
#line 571 "Minus.m3"
_frame._unused=(ADDRESS)&_frame;
#line 571 "Minus.m3"
 /* load */
#line 571 "Minus.m3"
 /* load */
#line 571 "Minus.m3"
 /* subtract */
#line 571 "Minus.m3"
 /* check_lo */
#line 571 "Minus.m3"
 /* store */
#line 571 "Minus.m3"
(*(INT64*)(&Minus_m_1388_L_1389))=(INT64)( ((INT64)( ((INT64)(*((UINT8*)(INT64_(188)+((ADDRESS)(&Minus_m_M_Minus_L_13))))))- ((INT64)(*((UINT64*)(INT64_(176)+((ADDRESS)(&Minus_m_M_Minus_L_13)))))))));
#line 571 "Minus.m3"
 /* load */
#line 571 "Minus.m3"
/*check_lo*/if(Minus_m_1388_L_1389<INT64_(0))Minus_m_M_Minus_L_13_CRASH(18273);
#line 571 "Minus.m3"
 /* exit_proc */
#line 571 "Minus.m3"
return Minus_m_1388_L_1389;
#line 571 "Minus.m3"
 /* end_procedure */
#line 571 "Minus.m3"
} /* uMinus_param_u8_C */
#line 571 "Minus.m3"
 /* set_source_line */
#line 571 "Minus.m3"
#line 572 "Minus.m3"
 /* begin_procedure */
#line 572 "Minus.m3"
struct Minus__uMinus_param_u8_C_Frame_t {
#line 572 "Minus.m3"
ADDRESS _unused;
#line 572 "Minus.m3"
};
#line 572 "Minus.m3"
Word__T
__cdecl
Minus__uMinus_param_u8_C(
   /* Param_Type1 */ Minus__UINT8 a_L_1051,
   /* Param_Type1 */ CARDINAL b_L_1052)
{
#line 572 "Minus.m3"
Minus__uMinus_param_u8_C_Frame_t _frame;
#line 572 "Minus.m3"
_frame._unused=(ADDRESS)&_frame;
#line 572 "Minus.m3"
 /* load */
#line 572 "Minus.m3"
 /* load */
#line 572 "Minus.m3"
 /* subtract */
#line 572 "Minus.m3"
 /* exit_proc */
#line 572 "Minus.m3"
return ((UINT64)(((UINT64)(((INT64)(a_L_1051))))-((UINT64)(((INT64)(b_L_1052))))));
#line 572 "Minus.m3"
 /* end_procedure */
#line 572 "Minus.m3"
} /* Minus_param_u8_C */
#line 572 "Minus.m3"
 /* set_source_line */
#line 572 "Minus.m3"
#line 573 "Minus.m3"
 /* begin_procedure */
#line 573 "Minus.m3"
struct Minus__Minus_param_u8_C_Frame_t {
#line 573 "Minus.m3"
ADDRESS _unused;
#line 573 "Minus.m3"
};
#line 573 "Minus.m3"
Minus__UINT8
__cdecl
Minus__Minus_param_u8_C(
   /* Param_Type1 */ Minus__UINT8 a_L_1054,
   /* Param_Type1 */ CARDINAL b_L_1055)
{
#line 573 "Minus.m3"
 /* Var_Type2 */ INT64 Minus_m_1390_L_1391={0};//always-init
#line 573 "Minus.m3"
Minus__Minus_param_u8_C_Frame_t _frame;
#line 573 "Minus.m3"
_frame._unused=(ADDRESS)&_frame;
#line 573 "Minus.m3"
 /* load */
#line 573 "Minus.m3"
 /* load */
#line 573 "Minus.m3"
 /* subtract */
#line 573 "Minus.m3"
 /* check_lo */
#line 573 "Minus.m3"
 /* store */
#line 573 "Minus.m3"
(*(INT64*)(&Minus_m_1390_L_1391))=(INT64)( ((INT64)( ((INT64)(a_L_1054))- ((INT64)(b_L_1055)))));
#line 573 "Minus.m3"
 /* load */
#line 573 "Minus.m3"
/*check_lo*/if(Minus_m_1390_L_1391<INT64_(0))Minus_m_M_Minus_L_13_CRASH(18337);
#line 573 "Minus.m3"
 /* exit_proc */
#line 573 "Minus.m3"
return Minus_m_1390_L_1391;
#line 573 "Minus.m3"
 /* end_procedure */
#line 573 "Minus.m3"
} /* uMinus_var_u8_u32 */
#line 573 "Minus.m3"
 /* set_source_line */
#line 573 "Minus.m3"
#line 574 "Minus.m3"
 /* begin_procedure */
#line 574 "Minus.m3"
struct Minus__uMinus_var_u8_u32_Frame_t {
#line 574 "Minus.m3"
ADDRESS _unused;
#line 574 "Minus.m3"
};
#line 574 "Minus.m3"
Word__T
__cdecl
Minus__uMinus_var_u8_u32(void)
{
#line 574 "Minus.m3"
Minus__uMinus_var_u8_u32_Frame_t _frame;
#line 574 "Minus.m3"
_frame._unused=(ADDRESS)&_frame;
#line 574 "Minus.m3"
 /* load */
#line 574 "Minus.m3"
 /* load */
#line 574 "Minus.m3"
 /* subtract */
#line 574 "Minus.m3"
 /* exit_proc */
#line 574 "Minus.m3"
return ((UINT64)(((UINT64)(((INT64)(*((UINT8*)(INT64_(188)+((ADDRESS)(&Minus_m_M_Minus_L_13))))))))-((UINT64)(((INT64)(*((UINT32*)(INT64_(184)+((ADDRESS)(&Minus_m_M_Minus_L_13))))))))));
#line 574 "Minus.m3"
 /* end_procedure */
#line 574 "Minus.m3"
} /* Minus_var_u8_u32 */
#line 574 "Minus.m3"
 /* set_source_line */
#line 574 "Minus.m3"
#line 575 "Minus.m3"
 /* begin_procedure */
#line 575 "Minus.m3"
struct Minus__Minus_var_u8_u32_Frame_t {
#line 575 "Minus.m3"
ADDRESS _unused;
#line 575 "Minus.m3"
};
#line 575 "Minus.m3"
Minus__UINT8
__cdecl
Minus__Minus_var_u8_u32(void)
{
#line 575 "Minus.m3"
 /* Var_Type2 */ INT64 Minus_m_1392_L_1393={0};//always-init
#line 575 "Minus.m3"
Minus__Minus_var_u8_u32_Frame_t _frame;
#line 575 "Minus.m3"
_frame._unused=(ADDRESS)&_frame;
#line 575 "Minus.m3"
 /* load */
#line 575 "Minus.m3"
 /* load */
#line 575 "Minus.m3"
 /* subtract */
#line 575 "Minus.m3"
 /* check_lo */
#line 575 "Minus.m3"
 /* store */
#line 575 "Minus.m3"
(*(INT64*)(&Minus_m_1392_L_1393))=(INT64)( ((INT64)( ((INT64)(*((UINT8*)(INT64_(188)+((ADDRESS)(&Minus_m_M_Minus_L_13))))))- ((INT64)(*((UINT32*)(INT64_(184)+((ADDRESS)(&Minus_m_M_Minus_L_13)))))))));
#line 575 "Minus.m3"
 /* load */
#line 575 "Minus.m3"
/*check_lo*/if(Minus_m_1392_L_1393<INT64_(0))Minus_m_M_Minus_L_13_CRASH(18401);
#line 575 "Minus.m3"
 /* exit_proc */
#line 575 "Minus.m3"
return Minus_m_1392_L_1393;
#line 575 "Minus.m3"
 /* end_procedure */
#line 575 "Minus.m3"
} /* uMinus_param_u8_u32 */
#line 575 "Minus.m3"
 /* set_source_line */
#line 575 "Minus.m3"
#line 576 "Minus.m3"
 /* begin_procedure */
#line 576 "Minus.m3"
struct Minus__uMinus_param_u8_u32_Frame_t {
#line 576 "Minus.m3"
ADDRESS _unused;
#line 576 "Minus.m3"
};
#line 576 "Minus.m3"
Word__T
__cdecl
Minus__uMinus_param_u8_u32(
   /* Param_Type1 */ Minus__UINT8 a_L_1059,
   /* Param_Type1 */ Minus__UINT32 b_L_1060)
{
#line 576 "Minus.m3"
Minus__uMinus_param_u8_u32_Frame_t _frame;
#line 576 "Minus.m3"
_frame._unused=(ADDRESS)&_frame;
#line 576 "Minus.m3"
 /* load */
#line 576 "Minus.m3"
 /* load */
#line 576 "Minus.m3"
 /* subtract */
#line 576 "Minus.m3"
 /* exit_proc */
#line 576 "Minus.m3"
return ((UINT64)(((UINT64)(((INT64)(a_L_1059))))-((UINT64)(((INT64)(b_L_1060))))));
#line 576 "Minus.m3"
 /* end_procedure */
#line 576 "Minus.m3"
} /* Minus_param_u8_u32 */
#line 576 "Minus.m3"
 /* set_source_line */
#line 576 "Minus.m3"
#line 577 "Minus.m3"
 /* begin_procedure */
#line 577 "Minus.m3"
struct Minus__Minus_param_u8_u32_Frame_t {
#line 577 "Minus.m3"
ADDRESS _unused;
#line 577 "Minus.m3"
};
#line 577 "Minus.m3"
Minus__UINT8
__cdecl
Minus__Minus_param_u8_u32(
   /* Param_Type1 */ Minus__UINT8 a_L_1062,
   /* Param_Type1 */ Minus__UINT32 b_L_1063)
{
#line 577 "Minus.m3"
 /* Var_Type2 */ INT64 Minus_m_1394_L_1395={0};//always-init
#line 577 "Minus.m3"
Minus__Minus_param_u8_u32_Frame_t _frame;
#line 577 "Minus.m3"
_frame._unused=(ADDRESS)&_frame;
#line 577 "Minus.m3"
 /* load */
#line 577 "Minus.m3"
 /* load */
#line 577 "Minus.m3"
 /* subtract */
#line 577 "Minus.m3"
 /* check_lo */
#line 577 "Minus.m3"
 /* store */
#line 577 "Minus.m3"
(*(INT64*)(&Minus_m_1394_L_1395))=(INT64)( ((INT64)( ((INT64)(a_L_1062))- ((INT64)(b_L_1063)))));
#line 577 "Minus.m3"
 /* load */
#line 577 "Minus.m3"
/*check_lo*/if(Minus_m_1394_L_1395<INT64_(0))Minus_m_M_Minus_L_13_CRASH(18465);
#line 577 "Minus.m3"
 /* exit_proc */
#line 577 "Minus.m3"
return Minus_m_1394_L_1395;
#line 577 "Minus.m3"
 /* end_procedure */
#line 577 "Minus.m3"
} /* uMinus_var_u8_u8 */
#line 577 "Minus.m3"
 /* set_source_line */
#line 577 "Minus.m3"
#line 578 "Minus.m3"
 /* begin_procedure */
#line 578 "Minus.m3"
struct Minus__uMinus_var_u8_u8_Frame_t {
#line 578 "Minus.m3"
ADDRESS _unused;
#line 578 "Minus.m3"
};
#line 578 "Minus.m3"
Word__T
__cdecl
Minus__uMinus_var_u8_u8(void)
{
#line 578 "Minus.m3"
Minus__uMinus_var_u8_u8_Frame_t _frame;
#line 578 "Minus.m3"
_frame._unused=(ADDRESS)&_frame;
#line 578 "Minus.m3"
 /* load */
#line 578 "Minus.m3"
 /* load */
#line 578 "Minus.m3"
 /* subtract */
#line 578 "Minus.m3"
 /* exit_proc */
#line 578 "Minus.m3"
return ((UINT64)(((UINT64)(((INT64)(*((UINT8*)(INT64_(188)+((ADDRESS)(&Minus_m_M_Minus_L_13))))))))-((UINT64)(((INT64)(*((UINT8*)(INT64_(188)+((ADDRESS)(&Minus_m_M_Minus_L_13))))))))));
#line 578 "Minus.m3"
 /* end_procedure */
#line 578 "Minus.m3"
} /* Minus_var_u8_u8 */
#line 578 "Minus.m3"
 /* set_source_line */
#line 578 "Minus.m3"
#line 579 "Minus.m3"
 /* begin_procedure */
#line 579 "Minus.m3"
struct Minus__Minus_var_u8_u8_Frame_t {
#line 579 "Minus.m3"
ADDRESS _unused;
#line 579 "Minus.m3"
};
#line 579 "Minus.m3"
Minus__UINT8
__cdecl
Minus__Minus_var_u8_u8(void)
{
#line 579 "Minus.m3"
 /* Var_Type2 */ INT64 Minus_m_1396_L_1397={0};//always-init
#line 579 "Minus.m3"
Minus__Minus_var_u8_u8_Frame_t _frame;
#line 579 "Minus.m3"
_frame._unused=(ADDRESS)&_frame;
#line 579 "Minus.m3"
 /* load */
#line 579 "Minus.m3"
 /* load */
#line 579 "Minus.m3"
 /* subtract */
#line 579 "Minus.m3"
 /* check_lo */
#line 579 "Minus.m3"
 /* store */
#line 579 "Minus.m3"
(*(INT64*)(&Minus_m_1396_L_1397))=(INT64)( ((INT64)( ((INT64)(*((UINT8*)(INT64_(188)+((ADDRESS)(&Minus_m_M_Minus_L_13))))))- ((INT64)(*((UINT8*)(INT64_(188)+((ADDRESS)(&Minus_m_M_Minus_L_13)))))))));
#line 579 "Minus.m3"
 /* load */
#line 579 "Minus.m3"
/*check_lo*/if(Minus_m_1396_L_1397<INT64_(0))Minus_m_M_Minus_L_13_CRASH(18529);
#line 579 "Minus.m3"
 /* exit_proc */
#line 579 "Minus.m3"
return Minus_m_1396_L_1397;
#line 579 "Minus.m3"
 /* end_procedure */
#line 579 "Minus.m3"
} /* uMinus_param_u8_u8 */
#line 579 "Minus.m3"
 /* set_source_line */
#line 579 "Minus.m3"
#line 580 "Minus.m3"
 /* begin_procedure */
#line 580 "Minus.m3"
struct Minus__uMinus_param_u8_u8_Frame_t {
#line 580 "Minus.m3"
ADDRESS _unused;
#line 580 "Minus.m3"
};
#line 580 "Minus.m3"
Word__T
__cdecl
Minus__uMinus_param_u8_u8(
   /* Param_Type1 */ Minus__UINT8 a_L_1067,
   /* Param_Type1 */ Minus__UINT8 b_L_1068)
{
#line 580 "Minus.m3"
Minus__uMinus_param_u8_u8_Frame_t _frame;
#line 580 "Minus.m3"
_frame._unused=(ADDRESS)&_frame;
#line 580 "Minus.m3"
 /* load */
#line 580 "Minus.m3"
 /* load */
#line 580 "Minus.m3"
 /* subtract */
#line 580 "Minus.m3"
 /* exit_proc */
#line 580 "Minus.m3"
return ((UINT64)(((UINT64)(((INT64)(a_L_1067))))-((UINT64)(((INT64)(b_L_1068))))));
#line 580 "Minus.m3"
 /* end_procedure */
#line 580 "Minus.m3"
} /* Minus_param_u8_u8 */
#line 580 "Minus.m3"
 /* set_source_line */
#line 580 "Minus.m3"
#line 581 "Minus.m3"
 /* begin_procedure */
#line 581 "Minus.m3"
struct Minus__Minus_param_u8_u8_Frame_t {
#line 581 "Minus.m3"
ADDRESS _unused;
#line 581 "Minus.m3"
};
#line 581 "Minus.m3"
Minus__UINT8
__cdecl
Minus__Minus_param_u8_u8(
   /* Param_Type1 */ Minus__UINT8 a_L_1070,
   /* Param_Type1 */ Minus__UINT8 b_L_1071)
{
#line 581 "Minus.m3"
 /* Var_Type2 */ INT64 Minus_m_1398_L_1399={0};//always-init
#line 581 "Minus.m3"
Minus__Minus_param_u8_u8_Frame_t _frame;
#line 581 "Minus.m3"
_frame._unused=(ADDRESS)&_frame;
#line 581 "Minus.m3"
 /* load */
#line 581 "Minus.m3"
 /* load */
#line 581 "Minus.m3"
 /* subtract */
#line 581 "Minus.m3"
 /* check_lo */
#line 581 "Minus.m3"
 /* store */
#line 581 "Minus.m3"
(*(INT64*)(&Minus_m_1398_L_1399))=(INT64)( ((INT64)( ((INT64)(a_L_1070))- ((INT64)(b_L_1071)))));
#line 581 "Minus.m3"
 /* load */
#line 581 "Minus.m3"
/*check_lo*/if(Minus_m_1398_L_1399<INT64_(0))Minus_m_M_Minus_L_13_CRASH(18593);
#line 581 "Minus.m3"
 /* exit_proc */
#line 581 "Minus.m3"
return Minus_m_1398_L_1399;
#line 581 "Minus.m3"
 /* end_procedure */
#line 581 "Minus.m3"
} /* uMinus_var_u8_L */
#line 581 "Minus.m3"
 /* set_source_line */
#line 581 "Minus.m3"
#line 582 "Minus.m3"
 /* begin_procedure */
#line 582 "Minus.m3"
struct Minus__uMinus_var_u8_L_Frame_t {
#line 582 "Minus.m3"
ADDRESS _unused;
#line 582 "Minus.m3"
};
#line 582 "Minus.m3"
Long__T
__cdecl
Minus__uMinus_var_u8_L(void)
{
#line 582 "Minus.m3"
Minus__uMinus_var_u8_L_Frame_t _frame;
#line 582 "Minus.m3"
_frame._unused=(ADDRESS)&_frame;
#line 582 "Minus.m3"
 /* load */
#line 582 "Minus.m3"
 /* loophole */
#line 582 "Minus.m3"
 /* load */
#line 582 "Minus.m3"
 /* subtract */
#line 582 "Minus.m3"
 /* exit_proc */
#line 582 "Minus.m3"
return ((UINT64)(((UINT64)((INT64)((INT64)(*((UINT8*)(INT64_(188)+((ADDRESS)(&Minus_m_M_Minus_L_13))))))))-((UINT64)(*((INT64*)(INT64_(192)+((ADDRESS)(&Minus_m_M_Minus_L_13))))))));
#line 582 "Minus.m3"
 /* end_procedure */
#line 582 "Minus.m3"
} /* Minus_var_u8_L */
#line 582 "Minus.m3"
 /* set_source_line */
#line 582 "Minus.m3"
#line 583 "Minus.m3"
 /* begin_procedure */
#line 583 "Minus.m3"
struct Minus__Minus_var_u8_L_Frame_t {
#line 583 "Minus.m3"
ADDRESS _unused;
#line 583 "Minus.m3"
};
#line 583 "Minus.m3"
LONGINT
__cdecl
Minus__Minus_var_u8_L(void)
{
#line 583 "Minus.m3"
Minus__Minus_var_u8_L_Frame_t _frame;
#line 583 "Minus.m3"
_frame._unused=(ADDRESS)&_frame;
#line 583 "Minus.m3"
 /* load */
#line 583 "Minus.m3"
 /* loophole */
#line 583 "Minus.m3"
 /* load */
#line 583 "Minus.m3"
 /* subtract */
#line 583 "Minus.m3"
 /* exit_proc */
#line 583 "Minus.m3"
return ((INT64)(((INT64)((INT64)((INT64)(*((UINT8*)(INT64_(188)+((ADDRESS)(&Minus_m_M_Minus_L_13))))))))-((INT64)(*((INT64*)(INT64_(192)+((ADDRESS)(&Minus_m_M_Minus_L_13))))))));
#line 583 "Minus.m3"
 /* end_procedure */
#line 583 "Minus.m3"
} /* uMinus_param_u8_L */
#line 583 "Minus.m3"
 /* set_source_line */
#line 583 "Minus.m3"
#line 584 "Minus.m3"
 /* begin_procedure */
#line 584 "Minus.m3"
struct Minus__uMinus_param_u8_L_Frame_t {
#line 584 "Minus.m3"
ADDRESS _unused;
#line 584 "Minus.m3"
};
#line 584 "Minus.m3"
Long__T
__cdecl
Minus__uMinus_param_u8_L(
   /* Param_Type1 */ Minus__UINT8 a_L_1075,
   /* Param_Type1 */ LONGINT b_L_1076)
{
#line 584 "Minus.m3"
Minus__uMinus_param_u8_L_Frame_t _frame;
#line 584 "Minus.m3"
_frame._unused=(ADDRESS)&_frame;
#line 584 "Minus.m3"
 /* load */
#line 584 "Minus.m3"
 /* loophole */
#line 584 "Minus.m3"
 /* load */
#line 584 "Minus.m3"
 /* subtract */
#line 584 "Minus.m3"
 /* exit_proc */
#line 584 "Minus.m3"
return ((UINT64)(((UINT64)((INT64)((INT64)(a_L_1075))))-((UINT64)(b_L_1076))));
#line 584 "Minus.m3"
 /* end_procedure */
#line 584 "Minus.m3"
} /* Minus_param_u8_L */
#line 584 "Minus.m3"
 /* set_source_line */
#line 584 "Minus.m3"
#line 585 "Minus.m3"
 /* begin_procedure */
#line 585 "Minus.m3"
struct Minus__Minus_param_u8_L_Frame_t {
#line 585 "Minus.m3"
ADDRESS _unused;
#line 585 "Minus.m3"
};
#line 585 "Minus.m3"
LONGINT
__cdecl
Minus__Minus_param_u8_L(
   /* Param_Type1 */ Minus__UINT8 a_L_1078,
   /* Param_Type1 */ LONGINT b_L_1079)
{
#line 585 "Minus.m3"
Minus__Minus_param_u8_L_Frame_t _frame;
#line 585 "Minus.m3"
_frame._unused=(ADDRESS)&_frame;
#line 585 "Minus.m3"
 /* load */
#line 585 "Minus.m3"
 /* loophole */
#line 585 "Minus.m3"
 /* load */
#line 585 "Minus.m3"
 /* subtract */
#line 585 "Minus.m3"
 /* exit_proc */
#line 585 "Minus.m3"
return ((INT64)(((INT64)((INT64)((INT64)(a_L_1078))))- b_L_1079));
#line 585 "Minus.m3"
 /* end_procedure */
#line 585 "Minus.m3"
} /* uMinus_var_L_i8 */
#line 585 "Minus.m3"
 /* set_source_line */
#line 585 "Minus.m3"
#line 586 "Minus.m3"
 /* begin_procedure */
#line 586 "Minus.m3"
struct Minus__uMinus_var_L_i8_Frame_t {
#line 586 "Minus.m3"
ADDRESS _unused;
#line 586 "Minus.m3"
};
#line 586 "Minus.m3"
Long__T
__cdecl
Minus__uMinus_var_L_i8(void)
{
#line 586 "Minus.m3"
Minus__uMinus_var_L_i8_Frame_t _frame;
#line 586 "Minus.m3"
_frame._unused=(ADDRESS)&_frame;
#line 586 "Minus.m3"
 /* load */
#line 586 "Minus.m3"
 /* loophole */
#line 586 "Minus.m3"
 /* load */
#line 586 "Minus.m3"
 /* swap */
#line 586 "Minus.m3"
 /* subtract */
#line 586 "Minus.m3"
 /* exit_proc */
#line 586 "Minus.m3"
return ((UINT64)(((UINT64)(*((INT64*)(INT64_(192)+((ADDRESS)(&Minus_m_M_Minus_L_13))))))-((UINT64)((INT64)((INT64)(*((INT8*)(INT64_(104)+((ADDRESS)(&Minus_m_M_Minus_L_13))))))))));
#line 586 "Minus.m3"
 /* end_procedure */
#line 586 "Minus.m3"
} /* Minus_var_L_i8 */
#line 586 "Minus.m3"
 /* set_source_line */
#line 586 "Minus.m3"
#line 587 "Minus.m3"
 /* begin_procedure */
#line 587 "Minus.m3"
struct Minus__Minus_var_L_i8_Frame_t {
#line 587 "Minus.m3"
ADDRESS _unused;
#line 587 "Minus.m3"
};
#line 587 "Minus.m3"
LONGINT
__cdecl
Minus__Minus_var_L_i8(void)
{
#line 587 "Minus.m3"
Minus__Minus_var_L_i8_Frame_t _frame;
#line 587 "Minus.m3"
_frame._unused=(ADDRESS)&_frame;
#line 587 "Minus.m3"
 /* load */
#line 587 "Minus.m3"
 /* loophole */
#line 587 "Minus.m3"
 /* load */
#line 587 "Minus.m3"
 /* swap */
#line 587 "Minus.m3"
 /* subtract */
#line 587 "Minus.m3"
 /* exit_proc */
#line 587 "Minus.m3"
return ((INT64)(((INT64)(*((INT64*)(INT64_(192)+((ADDRESS)(&Minus_m_M_Minus_L_13))))))-((INT64)((INT64)((INT64)(*((INT8*)(INT64_(104)+((ADDRESS)(&Minus_m_M_Minus_L_13))))))))));
#line 587 "Minus.m3"
 /* end_procedure */
#line 587 "Minus.m3"
} /* uMinus_param_L_i8 */
#line 587 "Minus.m3"
 /* set_source_line */
#line 587 "Minus.m3"
#line 588 "Minus.m3"
 /* begin_procedure */
#line 588 "Minus.m3"
struct Minus__uMinus_param_L_i8_Frame_t {
#line 588 "Minus.m3"
ADDRESS _unused;
#line 588 "Minus.m3"
};
#line 588 "Minus.m3"
Long__T
__cdecl
Minus__uMinus_param_L_i8(
   /* Param_Type1 */ LONGINT a_L_1083,
   /* Param_Type1 */ Minus__INT8 b_L_1084)
{
#line 588 "Minus.m3"
Minus__uMinus_param_L_i8_Frame_t _frame;
#line 588 "Minus.m3"
_frame._unused=(ADDRESS)&_frame;
#line 588 "Minus.m3"
 /* load */
#line 588 "Minus.m3"
 /* loophole */
#line 588 "Minus.m3"
 /* load */
#line 588 "Minus.m3"
 /* swap */
#line 588 "Minus.m3"
 /* subtract */
#line 588 "Minus.m3"
 /* exit_proc */
#line 588 "Minus.m3"
return ((UINT64)(((UINT64)(a_L_1083))-((UINT64)((INT64)((INT64)(b_L_1084))))));
#line 588 "Minus.m3"
 /* end_procedure */
#line 588 "Minus.m3"
} /* Minus_param_L_i8 */
#line 588 "Minus.m3"
 /* set_source_line */
#line 588 "Minus.m3"
#line 589 "Minus.m3"
 /* begin_procedure */
#line 589 "Minus.m3"
struct Minus__Minus_param_L_i8_Frame_t {
#line 589 "Minus.m3"
ADDRESS _unused;
#line 589 "Minus.m3"
};
#line 589 "Minus.m3"
LONGINT
__cdecl
Minus__Minus_param_L_i8(
   /* Param_Type1 */ LONGINT a_L_1086,
   /* Param_Type1 */ Minus__INT8 b_L_1087)
{
#line 589 "Minus.m3"
Minus__Minus_param_L_i8_Frame_t _frame;
#line 589 "Minus.m3"
_frame._unused=(ADDRESS)&_frame;
#line 589 "Minus.m3"
 /* load */
#line 589 "Minus.m3"
 /* loophole */
#line 589 "Minus.m3"
 /* load */
#line 589 "Minus.m3"
 /* swap */
#line 589 "Minus.m3"
 /* subtract */
#line 589 "Minus.m3"
 /* exit_proc */
#line 589 "Minus.m3"
return ((INT64)( a_L_1086-((INT64)((INT64)((INT64)(b_L_1087))))));
#line 589 "Minus.m3"
 /* end_procedure */
#line 589 "Minus.m3"
} /* uMinus_var_L_u64 */
#line 589 "Minus.m3"
 /* set_source_line */
#line 589 "Minus.m3"
#line 590 "Minus.m3"
 /* begin_procedure */
#line 590 "Minus.m3"
struct Minus__uMinus_var_L_u64_Frame_t {
#line 590 "Minus.m3"
ADDRESS _unused;
#line 590 "Minus.m3"
};
#line 590 "Minus.m3"
Long__T
__cdecl
Minus__uMinus_var_L_u64(void)
{
#line 590 "Minus.m3"
Minus__uMinus_var_L_u64_Frame_t _frame;
#line 590 "Minus.m3"
_frame._unused=(ADDRESS)&_frame;
#line 590 "Minus.m3"
 /* load */
#line 590 "Minus.m3"
 /* load */
#line 590 "Minus.m3"
 /* subtract */
#line 590 "Minus.m3"
 /* exit_proc */
#line 590 "Minus.m3"
return ((UINT64)(((UINT64)(*((INT64*)(INT64_(192)+((ADDRESS)(&Minus_m_M_Minus_L_13))))))-((UINT64)(*((INT64*)(INT64_(112)+((ADDRESS)(&Minus_m_M_Minus_L_13))))))));
#line 590 "Minus.m3"
 /* end_procedure */
#line 590 "Minus.m3"
} /* Minus_var_L_u64 */
#line 590 "Minus.m3"
 /* set_source_line */
#line 590 "Minus.m3"
#line 591 "Minus.m3"
 /* begin_procedure */
#line 591 "Minus.m3"
struct Minus__Minus_var_L_u64_Frame_t {
#line 591 "Minus.m3"
ADDRESS _unused;
#line 591 "Minus.m3"
};
#line 591 "Minus.m3"
LONGINT
__cdecl
Minus__Minus_var_L_u64(void)
{
#line 591 "Minus.m3"
Minus__Minus_var_L_u64_Frame_t _frame;
#line 591 "Minus.m3"
_frame._unused=(ADDRESS)&_frame;
#line 591 "Minus.m3"
 /* load */
#line 591 "Minus.m3"
 /* load */
#line 591 "Minus.m3"
 /* subtract */
#line 591 "Minus.m3"
 /* exit_proc */
#line 591 "Minus.m3"
return ((INT64)(((INT64)(*((INT64*)(INT64_(192)+((ADDRESS)(&Minus_m_M_Minus_L_13))))))-((INT64)(*((INT64*)(INT64_(112)+((ADDRESS)(&Minus_m_M_Minus_L_13))))))));
#line 591 "Minus.m3"
 /* end_procedure */
#line 591 "Minus.m3"
} /* uMinus_param_L_u64 */
#line 591 "Minus.m3"
 /* set_source_line */
#line 591 "Minus.m3"
#line 592 "Minus.m3"
 /* begin_procedure */
#line 592 "Minus.m3"
struct Minus__uMinus_param_L_u64_Frame_t {
#line 592 "Minus.m3"
ADDRESS _unused;
#line 592 "Minus.m3"
};
#line 592 "Minus.m3"
Long__T
__cdecl
Minus__uMinus_param_L_u64(
   /* Param_Type1 */ LONGINT a_L_1091,
   /* Param_Type1 */ Minus__UINT64 b_L_1092)
{
#line 592 "Minus.m3"
Minus__uMinus_param_L_u64_Frame_t _frame;
#line 592 "Minus.m3"
_frame._unused=(ADDRESS)&_frame;
#line 592 "Minus.m3"
 /* load */
#line 592 "Minus.m3"
 /* load */
#line 592 "Minus.m3"
 /* subtract */
#line 592 "Minus.m3"
 /* exit_proc */
#line 592 "Minus.m3"
return ((UINT64)(((UINT64)(a_L_1091))-((UINT64)(b_L_1092))));
#line 592 "Minus.m3"
 /* end_procedure */
#line 592 "Minus.m3"
} /* Minus_param_L_u64 */
#line 592 "Minus.m3"
 /* set_source_line */
#line 592 "Minus.m3"
#line 593 "Minus.m3"
 /* begin_procedure */
#line 593 "Minus.m3"
struct Minus__Minus_param_L_u64_Frame_t {
#line 593 "Minus.m3"
ADDRESS _unused;
#line 593 "Minus.m3"
};
#line 593 "Minus.m3"
LONGINT
__cdecl
Minus__Minus_param_L_u64(
   /* Param_Type1 */ LONGINT a_L_1094,
   /* Param_Type1 */ Minus__UINT64 b_L_1095)
{
#line 593 "Minus.m3"
Minus__Minus_param_L_u64_Frame_t _frame;
#line 593 "Minus.m3"
_frame._unused=(ADDRESS)&_frame;
#line 593 "Minus.m3"
 /* load */
#line 593 "Minus.m3"
 /* load */
#line 593 "Minus.m3"
 /* subtract */
#line 593 "Minus.m3"
 /* exit_proc */
#line 593 "Minus.m3"
return ((INT64)( a_L_1094- b_L_1095));
#line 593 "Minus.m3"
 /* end_procedure */
#line 593 "Minus.m3"
} /* uMinus_var_L_i32 */
#line 593 "Minus.m3"
 /* set_source_line */
#line 593 "Minus.m3"
#line 594 "Minus.m3"
 /* begin_procedure */
#line 594 "Minus.m3"
struct Minus__uMinus_var_L_i32_Frame_t {
#line 594 "Minus.m3"
ADDRESS _unused;
#line 594 "Minus.m3"
};
#line 594 "Minus.m3"
Long__T
__cdecl
Minus__uMinus_var_L_i32(void)
{
#line 594 "Minus.m3"
Minus__uMinus_var_L_i32_Frame_t _frame;
#line 594 "Minus.m3"
_frame._unused=(ADDRESS)&_frame;
#line 594 "Minus.m3"
 /* load */
#line 594 "Minus.m3"
 /* loophole */
#line 594 "Minus.m3"
 /* load */
#line 594 "Minus.m3"
 /* swap */
#line 594 "Minus.m3"
 /* subtract */
#line 594 "Minus.m3"
 /* exit_proc */
#line 594 "Minus.m3"
return ((UINT64)(((UINT64)(*((INT64*)(INT64_(192)+((ADDRESS)(&Minus_m_M_Minus_L_13))))))-((UINT64)((INT64)((INT64)(*((INT32*)(INT64_(128)+((ADDRESS)(&Minus_m_M_Minus_L_13))))))))));
#line 594 "Minus.m3"
 /* end_procedure */
#line 594 "Minus.m3"
} /* Minus_var_L_i32 */
#line 594 "Minus.m3"
 /* set_source_line */
#line 594 "Minus.m3"
#line 595 "Minus.m3"
 /* begin_procedure */
#line 595 "Minus.m3"
struct Minus__Minus_var_L_i32_Frame_t {
#line 595 "Minus.m3"
ADDRESS _unused;
#line 595 "Minus.m3"
};
#line 595 "Minus.m3"
LONGINT
__cdecl
Minus__Minus_var_L_i32(void)
{
#line 595 "Minus.m3"
Minus__Minus_var_L_i32_Frame_t _frame;
#line 595 "Minus.m3"
_frame._unused=(ADDRESS)&_frame;
#line 595 "Minus.m3"
 /* load */
#line 595 "Minus.m3"
 /* loophole */
#line 595 "Minus.m3"
 /* load */
#line 595 "Minus.m3"
 /* swap */
#line 595 "Minus.m3"
 /* subtract */
#line 595 "Minus.m3"
 /* exit_proc */
#line 595 "Minus.m3"
return ((INT64)(((INT64)(*((INT64*)(INT64_(192)+((ADDRESS)(&Minus_m_M_Minus_L_13))))))-((INT64)((INT64)((INT64)(*((INT32*)(INT64_(128)+((ADDRESS)(&Minus_m_M_Minus_L_13))))))))));
#line 595 "Minus.m3"
 /* end_procedure */
#line 595 "Minus.m3"
} /* uMinus_param_L_i32 */
#line 595 "Minus.m3"
 /* set_source_line */
#line 595 "Minus.m3"
#line 596 "Minus.m3"
 /* begin_procedure */
#line 596 "Minus.m3"
struct Minus__uMinus_param_L_i32_Frame_t {
#line 596 "Minus.m3"
ADDRESS _unused;
#line 596 "Minus.m3"
};
#line 596 "Minus.m3"
Long__T
__cdecl
Minus__uMinus_param_L_i32(
   /* Param_Type1 */ LONGINT a_L_1099,
   /* Param_Type1 */ Minus__INT32 b_L_1100)
{
#line 596 "Minus.m3"
Minus__uMinus_param_L_i32_Frame_t _frame;
#line 596 "Minus.m3"
_frame._unused=(ADDRESS)&_frame;
#line 596 "Minus.m3"
 /* load */
#line 596 "Minus.m3"
 /* loophole */
#line 596 "Minus.m3"
 /* load */
#line 596 "Minus.m3"
 /* swap */
#line 596 "Minus.m3"
 /* subtract */
#line 596 "Minus.m3"
 /* exit_proc */
#line 596 "Minus.m3"
return ((UINT64)(((UINT64)(a_L_1099))-((UINT64)((INT64)((INT64)(b_L_1100))))));
#line 596 "Minus.m3"
 /* end_procedure */
#line 596 "Minus.m3"
} /* Minus_param_L_i32 */
#line 596 "Minus.m3"
 /* set_source_line */
#line 596 "Minus.m3"
#line 597 "Minus.m3"
 /* begin_procedure */
#line 597 "Minus.m3"
struct Minus__Minus_param_L_i32_Frame_t {
#line 597 "Minus.m3"
ADDRESS _unused;
#line 597 "Minus.m3"
};
#line 597 "Minus.m3"
LONGINT
__cdecl
Minus__Minus_param_L_i32(
   /* Param_Type1 */ LONGINT a_L_1102,
   /* Param_Type1 */ Minus__INT32 b_L_1103)
{
#line 597 "Minus.m3"
Minus__Minus_param_L_i32_Frame_t _frame;
#line 597 "Minus.m3"
_frame._unused=(ADDRESS)&_frame;
#line 597 "Minus.m3"
 /* load */
#line 597 "Minus.m3"
 /* loophole */
#line 597 "Minus.m3"
 /* load */
#line 597 "Minus.m3"
 /* swap */
#line 597 "Minus.m3"
 /* subtract */
#line 597 "Minus.m3"
 /* exit_proc */
#line 597 "Minus.m3"
return ((INT64)( a_L_1102-((INT64)((INT64)((INT64)(b_L_1103))))));
#line 597 "Minus.m3"
 /* end_procedure */
#line 597 "Minus.m3"
} /* uMinus_var_L_LC */
#line 597 "Minus.m3"
 /* set_source_line */
#line 597 "Minus.m3"
#line 598 "Minus.m3"
 /* begin_procedure */
#line 598 "Minus.m3"
struct Minus__uMinus_var_L_LC_Frame_t {
#line 598 "Minus.m3"
ADDRESS _unused;
#line 598 "Minus.m3"
};
#line 598 "Minus.m3"
Long__T
__cdecl
Minus__uMinus_var_L_LC(void)
{
#line 598 "Minus.m3"
Minus__uMinus_var_L_LC_Frame_t _frame;
#line 598 "Minus.m3"
_frame._unused=(ADDRESS)&_frame;
#line 598 "Minus.m3"
 /* load */
#line 598 "Minus.m3"
 /* load */
#line 598 "Minus.m3"
 /* subtract */
#line 598 "Minus.m3"
 /* exit_proc */
#line 598 "Minus.m3"
return ((UINT64)(((UINT64)(*((INT64*)(INT64_(192)+((ADDRESS)(&Minus_m_M_Minus_L_13))))))-((UINT64)(((INT64)(*((UINT64*)(INT64_(136)+((ADDRESS)(&Minus_m_M_Minus_L_13))))))))));
#line 598 "Minus.m3"
 /* end_procedure */
#line 598 "Minus.m3"
} /* Minus_var_L_LC */
#line 598 "Minus.m3"
 /* set_source_line */
#line 598 "Minus.m3"
#line 599 "Minus.m3"
 /* begin_procedure */
#line 599 "Minus.m3"
struct Minus__Minus_var_L_LC_Frame_t {
#line 599 "Minus.m3"
ADDRESS _unused;
#line 599 "Minus.m3"
};
#line 599 "Minus.m3"
LONGINT
__cdecl
Minus__Minus_var_L_LC(void)
{
#line 599 "Minus.m3"
Minus__Minus_var_L_LC_Frame_t _frame;
#line 599 "Minus.m3"
_frame._unused=(ADDRESS)&_frame;
#line 599 "Minus.m3"
 /* load */
#line 599 "Minus.m3"
 /* load */
#line 599 "Minus.m3"
 /* subtract */
#line 599 "Minus.m3"
 /* exit_proc */
#line 599 "Minus.m3"
return ((INT64)(((INT64)(*((INT64*)(INT64_(192)+((ADDRESS)(&Minus_m_M_Minus_L_13))))))- ((INT64)(*((UINT64*)(INT64_(136)+((ADDRESS)(&Minus_m_M_Minus_L_13))))))));
#line 599 "Minus.m3"
 /* end_procedure */
#line 599 "Minus.m3"
} /* uMinus_param_L_LC */
#line 599 "Minus.m3"
 /* set_source_line */
#line 599 "Minus.m3"
#line 600 "Minus.m3"
 /* begin_procedure */
#line 600 "Minus.m3"
struct Minus__uMinus_param_L_LC_Frame_t {
#line 600 "Minus.m3"
ADDRESS _unused;
#line 600 "Minus.m3"
};
#line 600 "Minus.m3"
Long__T
__cdecl
Minus__uMinus_param_L_LC(
   /* Param_Type1 */ LONGINT a_L_1107,
   /* Param_Type1 */ LONGCARD b_L_1108)
{
#line 600 "Minus.m3"
Minus__uMinus_param_L_LC_Frame_t _frame;
#line 600 "Minus.m3"
_frame._unused=(ADDRESS)&_frame;
#line 600 "Minus.m3"
 /* load */
#line 600 "Minus.m3"
 /* load */
#line 600 "Minus.m3"
 /* subtract */
#line 600 "Minus.m3"
 /* exit_proc */
#line 600 "Minus.m3"
return ((UINT64)(((UINT64)(a_L_1107))-((UINT64)(((INT64)(b_L_1108))))));
#line 600 "Minus.m3"
 /* end_procedure */
#line 600 "Minus.m3"
} /* Minus_param_L_LC */
#line 600 "Minus.m3"
 /* set_source_line */
#line 600 "Minus.m3"
#line 601 "Minus.m3"
 /* begin_procedure */
#line 601 "Minus.m3"
struct Minus__Minus_param_L_LC_Frame_t {
#line 601 "Minus.m3"
ADDRESS _unused;
#line 601 "Minus.m3"
};
#line 601 "Minus.m3"
LONGINT
__cdecl
Minus__Minus_param_L_LC(
   /* Param_Type1 */ LONGINT a_L_1110,
   /* Param_Type1 */ LONGCARD b_L_1111)
{
#line 601 "Minus.m3"
Minus__Minus_param_L_LC_Frame_t _frame;
#line 601 "Minus.m3"
_frame._unused=(ADDRESS)&_frame;
#line 601 "Minus.m3"
 /* load */
#line 601 "Minus.m3"
 /* load */
#line 601 "Minus.m3"
 /* subtract */
#line 601 "Minus.m3"
 /* exit_proc */
#line 601 "Minus.m3"
return ((INT64)( a_L_1110- ((INT64)(b_L_1111))));
#line 601 "Minus.m3"
 /* end_procedure */
#line 601 "Minus.m3"
} /* uMinus_var_L_u16 */
#line 601 "Minus.m3"
 /* set_source_line */
#line 601 "Minus.m3"
#line 602 "Minus.m3"
 /* begin_procedure */
#line 602 "Minus.m3"
struct Minus__uMinus_var_L_u16_Frame_t {
#line 602 "Minus.m3"
ADDRESS _unused;
#line 602 "Minus.m3"
};
#line 602 "Minus.m3"
Long__T
__cdecl
Minus__uMinus_var_L_u16(void)
{
#line 602 "Minus.m3"
Minus__uMinus_var_L_u16_Frame_t _frame;
#line 602 "Minus.m3"
_frame._unused=(ADDRESS)&_frame;
#line 602 "Minus.m3"
 /* load */
#line 602 "Minus.m3"
 /* loophole */
#line 602 "Minus.m3"
 /* load */
#line 602 "Minus.m3"
 /* swap */
#line 602 "Minus.m3"
 /* subtract */
#line 602 "Minus.m3"
 /* exit_proc */
#line 602 "Minus.m3"
return ((UINT64)(((UINT64)(*((INT64*)(INT64_(192)+((ADDRESS)(&Minus_m_M_Minus_L_13))))))-((UINT64)((INT64)((INT64)(*((UINT16*)(INT64_(144)+((ADDRESS)(&Minus_m_M_Minus_L_13))))))))));
#line 602 "Minus.m3"
 /* end_procedure */
#line 602 "Minus.m3"
} /* Minus_var_L_u16 */
#line 602 "Minus.m3"
 /* set_source_line */
#line 602 "Minus.m3"
#line 603 "Minus.m3"
 /* begin_procedure */
#line 603 "Minus.m3"
struct Minus__Minus_var_L_u16_Frame_t {
#line 603 "Minus.m3"
ADDRESS _unused;
#line 603 "Minus.m3"
};
#line 603 "Minus.m3"
LONGINT
__cdecl
Minus__Minus_var_L_u16(void)
{
#line 603 "Minus.m3"
Minus__Minus_var_L_u16_Frame_t _frame;
#line 603 "Minus.m3"
_frame._unused=(ADDRESS)&_frame;
#line 603 "Minus.m3"
 /* load */
#line 603 "Minus.m3"
 /* loophole */
#line 603 "Minus.m3"
 /* load */
#line 603 "Minus.m3"
 /* swap */
#line 603 "Minus.m3"
 /* subtract */
#line 603 "Minus.m3"
 /* exit_proc */
#line 603 "Minus.m3"
return ((INT64)(((INT64)(*((INT64*)(INT64_(192)+((ADDRESS)(&Minus_m_M_Minus_L_13))))))-((INT64)((INT64)((INT64)(*((UINT16*)(INT64_(144)+((ADDRESS)(&Minus_m_M_Minus_L_13))))))))));
#line 603 "Minus.m3"
 /* end_procedure */
#line 603 "Minus.m3"
} /* uMinus_param_L_u16 */
#line 603 "Minus.m3"
 /* set_source_line */
#line 603 "Minus.m3"
#line 604 "Minus.m3"
 /* begin_procedure */
#line 604 "Minus.m3"
struct Minus__uMinus_param_L_u16_Frame_t {
#line 604 "Minus.m3"
ADDRESS _unused;
#line 604 "Minus.m3"
};
#line 604 "Minus.m3"
Long__T
__cdecl
Minus__uMinus_param_L_u16(
   /* Param_Type1 */ LONGINT a_L_1115,
   /* Param_Type1 */ Minus__UINT16 b_L_1116)
{
#line 604 "Minus.m3"
Minus__uMinus_param_L_u16_Frame_t _frame;
#line 604 "Minus.m3"
_frame._unused=(ADDRESS)&_frame;
#line 604 "Minus.m3"
 /* load */
#line 604 "Minus.m3"
 /* loophole */
#line 604 "Minus.m3"
 /* load */
#line 604 "Minus.m3"
 /* swap */
#line 604 "Minus.m3"
 /* subtract */
#line 604 "Minus.m3"
 /* exit_proc */
#line 604 "Minus.m3"
return ((UINT64)(((UINT64)(a_L_1115))-((UINT64)((INT64)((INT64)(b_L_1116))))));
#line 604 "Minus.m3"
 /* end_procedure */
#line 604 "Minus.m3"
} /* Minus_param_L_u16 */
#line 604 "Minus.m3"
 /* set_source_line */
#line 604 "Minus.m3"
#line 605 "Minus.m3"
 /* begin_procedure */
#line 605 "Minus.m3"
struct Minus__Minus_param_L_u16_Frame_t {
#line 605 "Minus.m3"
ADDRESS _unused;
#line 605 "Minus.m3"
};
#line 605 "Minus.m3"
LONGINT
__cdecl
Minus__Minus_param_L_u16(
   /* Param_Type1 */ LONGINT a_L_1118,
   /* Param_Type1 */ Minus__UINT16 b_L_1119)
{
#line 605 "Minus.m3"
Minus__Minus_param_L_u16_Frame_t _frame;
#line 605 "Minus.m3"
_frame._unused=(ADDRESS)&_frame;
#line 605 "Minus.m3"
 /* load */
#line 605 "Minus.m3"
 /* loophole */
#line 605 "Minus.m3"
 /* load */
#line 605 "Minus.m3"
 /* swap */
#line 605 "Minus.m3"
 /* subtract */
#line 605 "Minus.m3"
 /* exit_proc */
#line 605 "Minus.m3"
return ((INT64)( a_L_1118-((INT64)((INT64)((INT64)(b_L_1119))))));
#line 605 "Minus.m3"
 /* end_procedure */
#line 605 "Minus.m3"
} /* uMinus_var_L_I */
#line 605 "Minus.m3"
 /* set_source_line */
#line 605 "Minus.m3"
#line 606 "Minus.m3"
 /* begin_procedure */
#line 606 "Minus.m3"
struct Minus__uMinus_var_L_I_Frame_t {
#line 606 "Minus.m3"
ADDRESS _unused;
#line 606 "Minus.m3"
};
#line 606 "Minus.m3"
Long__T
__cdecl
Minus__uMinus_var_L_I(void)
{
#line 606 "Minus.m3"
Minus__uMinus_var_L_I_Frame_t _frame;
#line 606 "Minus.m3"
_frame._unused=(ADDRESS)&_frame;
#line 606 "Minus.m3"
 /* load */
#line 606 "Minus.m3"
 /* loophole */
#line 606 "Minus.m3"
 /* load */
#line 606 "Minus.m3"
 /* swap */
#line 606 "Minus.m3"
 /* subtract */
#line 606 "Minus.m3"
 /* exit_proc */
#line 606 "Minus.m3"
return ((UINT64)(((UINT64)(*((INT64*)(INT64_(192)+((ADDRESS)(&Minus_m_M_Minus_L_13))))))-((UINT64)((INT64)*((INT64*)(INT64_(152)+((ADDRESS)(&Minus_m_M_Minus_L_13))))))));
#line 606 "Minus.m3"
 /* end_procedure */
#line 606 "Minus.m3"
} /* Minus_var_L_I */
#line 606 "Minus.m3"
 /* set_source_line */
#line 606 "Minus.m3"
#line 607 "Minus.m3"
 /* begin_procedure */
#line 607 "Minus.m3"
struct Minus__Minus_var_L_I_Frame_t {
#line 607 "Minus.m3"
ADDRESS _unused;
#line 607 "Minus.m3"
};
#line 607 "Minus.m3"
LONGINT
__cdecl
Minus__Minus_var_L_I(void)
{
#line 607 "Minus.m3"
Minus__Minus_var_L_I_Frame_t _frame;
#line 607 "Minus.m3"
_frame._unused=(ADDRESS)&_frame;
#line 607 "Minus.m3"
 /* load */
#line 607 "Minus.m3"
 /* loophole */
#line 607 "Minus.m3"
 /* load */
#line 607 "Minus.m3"
 /* swap */
#line 607 "Minus.m3"
 /* subtract */
#line 607 "Minus.m3"
 /* exit_proc */
#line 607 "Minus.m3"
return ((INT64)(((INT64)(*((INT64*)(INT64_(192)+((ADDRESS)(&Minus_m_M_Minus_L_13))))))-((INT64)((INT64)*((INT64*)(INT64_(152)+((ADDRESS)(&Minus_m_M_Minus_L_13))))))));
#line 607 "Minus.m3"
 /* end_procedure */
#line 607 "Minus.m3"
} /* uMinus_param_L_I */
#line 607 "Minus.m3"
 /* set_source_line */
#line 607 "Minus.m3"
#line 608 "Minus.m3"
 /* begin_procedure */
#line 608 "Minus.m3"
struct Minus__uMinus_param_L_I_Frame_t {
#line 608 "Minus.m3"
ADDRESS _unused;
#line 608 "Minus.m3"
};
#line 608 "Minus.m3"
Long__T
__cdecl
Minus__uMinus_param_L_I(
   /* Param_Type1 */ LONGINT a_L_1123,
   /* Param_Type1 */ INTEGER b_L_1124)
{
#line 608 "Minus.m3"
Minus__uMinus_param_L_I_Frame_t _frame;
#line 608 "Minus.m3"
_frame._unused=(ADDRESS)&_frame;
#line 608 "Minus.m3"
 /* load */
#line 608 "Minus.m3"
 /* loophole */
#line 608 "Minus.m3"
 /* load */
#line 608 "Minus.m3"
 /* swap */
#line 608 "Minus.m3"
 /* subtract */
#line 608 "Minus.m3"
 /* exit_proc */
#line 608 "Minus.m3"
return ((UINT64)(((UINT64)(a_L_1123))-((UINT64)((INT64)b_L_1124))));
#line 608 "Minus.m3"
 /* end_procedure */
#line 608 "Minus.m3"
} /* Minus_param_L_I */
#line 608 "Minus.m3"
 /* set_source_line */
#line 608 "Minus.m3"
#line 609 "Minus.m3"
 /* begin_procedure */
#line 609 "Minus.m3"
struct Minus__Minus_param_L_I_Frame_t {
#line 609 "Minus.m3"
ADDRESS _unused;
#line 609 "Minus.m3"
};
#line 609 "Minus.m3"
LONGINT
__cdecl
Minus__Minus_param_L_I(
   /* Param_Type1 */ LONGINT a_L_1126,
   /* Param_Type1 */ INTEGER b_L_1127)
{
#line 609 "Minus.m3"
Minus__Minus_param_L_I_Frame_t _frame;
#line 609 "Minus.m3"
_frame._unused=(ADDRESS)&_frame;
#line 609 "Minus.m3"
 /* load */
#line 609 "Minus.m3"
 /* loophole */
#line 609 "Minus.m3"
 /* load */
#line 609 "Minus.m3"
 /* swap */
#line 609 "Minus.m3"
 /* subtract */
#line 609 "Minus.m3"
 /* exit_proc */
#line 609 "Minus.m3"
return ((INT64)( a_L_1126-((INT64)((INT64)b_L_1127))));
#line 609 "Minus.m3"
 /* end_procedure */
#line 609 "Minus.m3"
} /* uMinus_var_L_i64 */
#line 609 "Minus.m3"
 /* set_source_line */
#line 609 "Minus.m3"
#line 610 "Minus.m3"
 /* begin_procedure */
#line 610 "Minus.m3"
struct Minus__uMinus_var_L_i64_Frame_t {
#line 610 "Minus.m3"
ADDRESS _unused;
#line 610 "Minus.m3"
};
#line 610 "Minus.m3"
Long__T
__cdecl
Minus__uMinus_var_L_i64(void)
{
#line 610 "Minus.m3"
Minus__uMinus_var_L_i64_Frame_t _frame;
#line 610 "Minus.m3"
_frame._unused=(ADDRESS)&_frame;
#line 610 "Minus.m3"
 /* load */
#line 610 "Minus.m3"
 /* load */
#line 610 "Minus.m3"
 /* subtract */
#line 610 "Minus.m3"
 /* exit_proc */
#line 610 "Minus.m3"
return ((UINT64)(((UINT64)(*((INT64*)(INT64_(192)+((ADDRESS)(&Minus_m_M_Minus_L_13))))))-((UINT64)(*((INT64*)(INT64_(160)+((ADDRESS)(&Minus_m_M_Minus_L_13))))))));
#line 610 "Minus.m3"
 /* end_procedure */
#line 610 "Minus.m3"
} /* Minus_var_L_i64 */
#line 610 "Minus.m3"
 /* set_source_line */
#line 610 "Minus.m3"
#line 611 "Minus.m3"
 /* begin_procedure */
#line 611 "Minus.m3"
struct Minus__Minus_var_L_i64_Frame_t {
#line 611 "Minus.m3"
ADDRESS _unused;
#line 611 "Minus.m3"
};
#line 611 "Minus.m3"
LONGINT
__cdecl
Minus__Minus_var_L_i64(void)
{
#line 611 "Minus.m3"
Minus__Minus_var_L_i64_Frame_t _frame;
#line 611 "Minus.m3"
_frame._unused=(ADDRESS)&_frame;
#line 611 "Minus.m3"
 /* load */
#line 611 "Minus.m3"
 /* load */
#line 611 "Minus.m3"
 /* subtract */
#line 611 "Minus.m3"
 /* exit_proc */
#line 611 "Minus.m3"
return ((INT64)(((INT64)(*((INT64*)(INT64_(192)+((ADDRESS)(&Minus_m_M_Minus_L_13))))))-((INT64)(*((INT64*)(INT64_(160)+((ADDRESS)(&Minus_m_M_Minus_L_13))))))));
#line 611 "Minus.m3"
 /* end_procedure */
#line 611 "Minus.m3"
} /* uMinus_param_L_i64 */
#line 611 "Minus.m3"
 /* set_source_line */
#line 611 "Minus.m3"
#line 612 "Minus.m3"
 /* begin_procedure */
#line 612 "Minus.m3"
struct Minus__uMinus_param_L_i64_Frame_t {
#line 612 "Minus.m3"
ADDRESS _unused;
#line 612 "Minus.m3"
};
#line 612 "Minus.m3"
Long__T
__cdecl
Minus__uMinus_param_L_i64(
   /* Param_Type1 */ LONGINT a_L_1131,
   /* Param_Type1 */ Minus__INT64 b_L_1132)
{
#line 612 "Minus.m3"
Minus__uMinus_param_L_i64_Frame_t _frame;
#line 612 "Minus.m3"
_frame._unused=(ADDRESS)&_frame;
#line 612 "Minus.m3"
 /* load */
#line 612 "Minus.m3"
 /* load */
#line 612 "Minus.m3"
 /* subtract */
#line 612 "Minus.m3"
 /* exit_proc */
#line 612 "Minus.m3"
return ((UINT64)(((UINT64)(a_L_1131))-((UINT64)(b_L_1132))));
#line 612 "Minus.m3"
 /* end_procedure */
#line 612 "Minus.m3"
} /* Minus_param_L_i64 */
#line 612 "Minus.m3"
 /* set_source_line */
#line 612 "Minus.m3"
#line 613 "Minus.m3"
 /* begin_procedure */
#line 613 "Minus.m3"
struct Minus__Minus_param_L_i64_Frame_t {
#line 613 "Minus.m3"
ADDRESS _unused;
#line 613 "Minus.m3"
};
#line 613 "Minus.m3"
LONGINT
__cdecl
Minus__Minus_param_L_i64(
   /* Param_Type1 */ LONGINT a_L_1134,
   /* Param_Type1 */ Minus__INT64 b_L_1135)
{
#line 613 "Minus.m3"
Minus__Minus_param_L_i64_Frame_t _frame;
#line 613 "Minus.m3"
_frame._unused=(ADDRESS)&_frame;
#line 613 "Minus.m3"
 /* load */
#line 613 "Minus.m3"
 /* load */
#line 613 "Minus.m3"
 /* subtract */
#line 613 "Minus.m3"
 /* exit_proc */
#line 613 "Minus.m3"
return ((INT64)( a_L_1134- b_L_1135));
#line 613 "Minus.m3"
 /* end_procedure */
#line 613 "Minus.m3"
} /* uMinus_var_L_i16 */
#line 613 "Minus.m3"
 /* set_source_line */
#line 613 "Minus.m3"
#line 614 "Minus.m3"
 /* begin_procedure */
#line 614 "Minus.m3"
struct Minus__uMinus_var_L_i16_Frame_t {
#line 614 "Minus.m3"
ADDRESS _unused;
#line 614 "Minus.m3"
};
#line 614 "Minus.m3"
Long__T
__cdecl
Minus__uMinus_var_L_i16(void)
{
#line 614 "Minus.m3"
Minus__uMinus_var_L_i16_Frame_t _frame;
#line 614 "Minus.m3"
_frame._unused=(ADDRESS)&_frame;
#line 614 "Minus.m3"
 /* load */
#line 614 "Minus.m3"
 /* loophole */
#line 614 "Minus.m3"
 /* load */
#line 614 "Minus.m3"
 /* swap */
#line 614 "Minus.m3"
 /* subtract */
#line 614 "Minus.m3"
 /* exit_proc */
#line 614 "Minus.m3"
return ((UINT64)(((UINT64)(*((INT64*)(INT64_(192)+((ADDRESS)(&Minus_m_M_Minus_L_13))))))-((UINT64)((INT64)((INT64)(*((INT16*)(INT64_(172)+((ADDRESS)(&Minus_m_M_Minus_L_13))))))))));
#line 614 "Minus.m3"
 /* end_procedure */
#line 614 "Minus.m3"
} /* Minus_var_L_i16 */
#line 614 "Minus.m3"
 /* set_source_line */
#line 614 "Minus.m3"
#line 615 "Minus.m3"
 /* begin_procedure */
#line 615 "Minus.m3"
struct Minus__Minus_var_L_i16_Frame_t {
#line 615 "Minus.m3"
ADDRESS _unused;
#line 615 "Minus.m3"
};
#line 615 "Minus.m3"
LONGINT
__cdecl
Minus__Minus_var_L_i16(void)
{
#line 615 "Minus.m3"
Minus__Minus_var_L_i16_Frame_t _frame;
#line 615 "Minus.m3"
_frame._unused=(ADDRESS)&_frame;
#line 615 "Minus.m3"
 /* load */
#line 615 "Minus.m3"
 /* loophole */
#line 615 "Minus.m3"
 /* load */
#line 615 "Minus.m3"
 /* swap */
#line 615 "Minus.m3"
 /* subtract */
#line 615 "Minus.m3"
 /* exit_proc */
#line 615 "Minus.m3"
return ((INT64)(((INT64)(*((INT64*)(INT64_(192)+((ADDRESS)(&Minus_m_M_Minus_L_13))))))-((INT64)((INT64)((INT64)(*((INT16*)(INT64_(172)+((ADDRESS)(&Minus_m_M_Minus_L_13))))))))));
#line 615 "Minus.m3"
 /* end_procedure */
#line 615 "Minus.m3"
} /* uMinus_param_L_i16 */
#line 615 "Minus.m3"
 /* set_source_line */
#line 615 "Minus.m3"
#line 616 "Minus.m3"
 /* begin_procedure */
#line 616 "Minus.m3"
struct Minus__uMinus_param_L_i16_Frame_t {
#line 616 "Minus.m3"
ADDRESS _unused;
#line 616 "Minus.m3"
};
#line 616 "Minus.m3"
Long__T
__cdecl
Minus__uMinus_param_L_i16(
   /* Param_Type1 */ LONGINT a_L_1139,
   /* Param_Type1 */ Minus__INT16 b_L_1140)
{
#line 616 "Minus.m3"
Minus__uMinus_param_L_i16_Frame_t _frame;
#line 616 "Minus.m3"
_frame._unused=(ADDRESS)&_frame;
#line 616 "Minus.m3"
 /* load */
#line 616 "Minus.m3"
 /* loophole */
#line 616 "Minus.m3"
 /* load */
#line 616 "Minus.m3"
 /* swap */
#line 616 "Minus.m3"
 /* subtract */
#line 616 "Minus.m3"
 /* exit_proc */
#line 616 "Minus.m3"
return ((UINT64)(((UINT64)(a_L_1139))-((UINT64)((INT64)((INT64)(b_L_1140))))));
#line 616 "Minus.m3"
 /* end_procedure */
#line 616 "Minus.m3"
} /* Minus_param_L_i16 */
#line 616 "Minus.m3"
 /* set_source_line */
#line 616 "Minus.m3"
#line 617 "Minus.m3"
 /* begin_procedure */
#line 617 "Minus.m3"
struct Minus__Minus_param_L_i16_Frame_t {
#line 617 "Minus.m3"
ADDRESS _unused;
#line 617 "Minus.m3"
};
#line 617 "Minus.m3"
LONGINT
__cdecl
Minus__Minus_param_L_i16(
   /* Param_Type1 */ LONGINT a_L_1142,
   /* Param_Type1 */ Minus__INT16 b_L_1143)
{
#line 617 "Minus.m3"
Minus__Minus_param_L_i16_Frame_t _frame;
#line 617 "Minus.m3"
_frame._unused=(ADDRESS)&_frame;
#line 617 "Minus.m3"
 /* load */
#line 617 "Minus.m3"
 /* loophole */
#line 617 "Minus.m3"
 /* load */
#line 617 "Minus.m3"
 /* swap */
#line 617 "Minus.m3"
 /* subtract */
#line 617 "Minus.m3"
 /* exit_proc */
#line 617 "Minus.m3"
return ((INT64)( a_L_1142-((INT64)((INT64)((INT64)(b_L_1143))))));
#line 617 "Minus.m3"
 /* end_procedure */
#line 617 "Minus.m3"
} /* uMinus_var_L_C */
#line 617 "Minus.m3"
 /* set_source_line */
#line 617 "Minus.m3"
#line 618 "Minus.m3"
 /* begin_procedure */
#line 618 "Minus.m3"
struct Minus__uMinus_var_L_C_Frame_t {
#line 618 "Minus.m3"
ADDRESS _unused;
#line 618 "Minus.m3"
};
#line 618 "Minus.m3"
Long__T
__cdecl
Minus__uMinus_var_L_C(void)
{
#line 618 "Minus.m3"
Minus__uMinus_var_L_C_Frame_t _frame;
#line 618 "Minus.m3"
_frame._unused=(ADDRESS)&_frame;
#line 618 "Minus.m3"
 /* load */
#line 618 "Minus.m3"
 /* loophole */
#line 618 "Minus.m3"
 /* load */
#line 618 "Minus.m3"
 /* swap */
#line 618 "Minus.m3"
 /* subtract */
#line 618 "Minus.m3"
 /* exit_proc */
#line 618 "Minus.m3"
return ((UINT64)(((UINT64)(*((INT64*)(INT64_(192)+((ADDRESS)(&Minus_m_M_Minus_L_13))))))-((UINT64)((INT64)((INT64)(*((UINT64*)(INT64_(176)+((ADDRESS)(&Minus_m_M_Minus_L_13))))))))));
#line 618 "Minus.m3"
 /* end_procedure */
#line 618 "Minus.m3"
} /* Minus_var_L_C */
#line 618 "Minus.m3"
 /* set_source_line */
#line 618 "Minus.m3"
#line 619 "Minus.m3"
 /* begin_procedure */
#line 619 "Minus.m3"
struct Minus__Minus_var_L_C_Frame_t {
#line 619 "Minus.m3"
ADDRESS _unused;
#line 619 "Minus.m3"
};
#line 619 "Minus.m3"
LONGINT
__cdecl
Minus__Minus_var_L_C(void)
{
#line 619 "Minus.m3"
Minus__Minus_var_L_C_Frame_t _frame;
#line 619 "Minus.m3"
_frame._unused=(ADDRESS)&_frame;
#line 619 "Minus.m3"
 /* load */
#line 619 "Minus.m3"
 /* loophole */
#line 619 "Minus.m3"
 /* load */
#line 619 "Minus.m3"
 /* swap */
#line 619 "Minus.m3"
 /* subtract */
#line 619 "Minus.m3"
 /* exit_proc */
#line 619 "Minus.m3"
return ((INT64)(((INT64)(*((INT64*)(INT64_(192)+((ADDRESS)(&Minus_m_M_Minus_L_13))))))-((INT64)((INT64)((INT64)(*((UINT64*)(INT64_(176)+((ADDRESS)(&Minus_m_M_Minus_L_13))))))))));
#line 619 "Minus.m3"
 /* end_procedure */
#line 619 "Minus.m3"
} /* uMinus_param_L_C */
#line 619 "Minus.m3"
 /* set_source_line */
#line 619 "Minus.m3"
#line 620 "Minus.m3"
 /* begin_procedure */
#line 620 "Minus.m3"
struct Minus__uMinus_param_L_C_Frame_t {
#line 620 "Minus.m3"
ADDRESS _unused;
#line 620 "Minus.m3"
};
#line 620 "Minus.m3"
Long__T
__cdecl
Minus__uMinus_param_L_C(
   /* Param_Type1 */ LONGINT a_L_1147,
   /* Param_Type1 */ CARDINAL b_L_1148)
{
#line 620 "Minus.m3"
Minus__uMinus_param_L_C_Frame_t _frame;
#line 620 "Minus.m3"
_frame._unused=(ADDRESS)&_frame;
#line 620 "Minus.m3"
 /* load */
#line 620 "Minus.m3"
 /* loophole */
#line 620 "Minus.m3"
 /* load */
#line 620 "Minus.m3"
 /* swap */
#line 620 "Minus.m3"
 /* subtract */
#line 620 "Minus.m3"
 /* exit_proc */
#line 620 "Minus.m3"
return ((UINT64)(((UINT64)(a_L_1147))-((UINT64)((INT64)((INT64)(b_L_1148))))));
#line 620 "Minus.m3"
 /* end_procedure */
#line 620 "Minus.m3"
} /* Minus_param_L_C */
#line 620 "Minus.m3"
 /* set_source_line */
#line 620 "Minus.m3"
#line 621 "Minus.m3"
 /* begin_procedure */
#line 621 "Minus.m3"
struct Minus__Minus_param_L_C_Frame_t {
#line 621 "Minus.m3"
ADDRESS _unused;
#line 621 "Minus.m3"
};
#line 621 "Minus.m3"
LONGINT
__cdecl
Minus__Minus_param_L_C(
   /* Param_Type1 */ LONGINT a_L_1150,
   /* Param_Type1 */ CARDINAL b_L_1151)
{
#line 621 "Minus.m3"
Minus__Minus_param_L_C_Frame_t _frame;
#line 621 "Minus.m3"
_frame._unused=(ADDRESS)&_frame;
#line 621 "Minus.m3"
 /* load */
#line 621 "Minus.m3"
 /* loophole */
#line 621 "Minus.m3"
 /* load */
#line 621 "Minus.m3"
 /* swap */
#line 621 "Minus.m3"
 /* subtract */
#line 621 "Minus.m3"
 /* exit_proc */
#line 621 "Minus.m3"
return ((INT64)( a_L_1150-((INT64)((INT64)((INT64)(b_L_1151))))));
#line 621 "Minus.m3"
 /* end_procedure */
#line 621 "Minus.m3"
} /* uMinus_var_L_u32 */
#line 621 "Minus.m3"
 /* set_source_line */
#line 621 "Minus.m3"
#line 622 "Minus.m3"
 /* begin_procedure */
#line 622 "Minus.m3"
struct Minus__uMinus_var_L_u32_Frame_t {
#line 622 "Minus.m3"
ADDRESS _unused;
#line 622 "Minus.m3"
};
#line 622 "Minus.m3"
Long__T
__cdecl
Minus__uMinus_var_L_u32(void)
{
#line 622 "Minus.m3"
Minus__uMinus_var_L_u32_Frame_t _frame;
#line 622 "Minus.m3"
_frame._unused=(ADDRESS)&_frame;
#line 622 "Minus.m3"
 /* load */
#line 622 "Minus.m3"
 /* loophole */
#line 622 "Minus.m3"
 /* load */
#line 622 "Minus.m3"
 /* swap */
#line 622 "Minus.m3"
 /* subtract */
#line 622 "Minus.m3"
 /* exit_proc */
#line 622 "Minus.m3"
return ((UINT64)(((UINT64)(*((INT64*)(INT64_(192)+((ADDRESS)(&Minus_m_M_Minus_L_13))))))-((UINT64)((INT64)((INT64)(*((UINT32*)(INT64_(184)+((ADDRESS)(&Minus_m_M_Minus_L_13))))))))));
#line 622 "Minus.m3"
 /* end_procedure */
#line 622 "Minus.m3"
} /* Minus_var_L_u32 */
#line 622 "Minus.m3"
 /* set_source_line */
#line 622 "Minus.m3"
#line 623 "Minus.m3"
 /* begin_procedure */
#line 623 "Minus.m3"
struct Minus__Minus_var_L_u32_Frame_t {
#line 623 "Minus.m3"
ADDRESS _unused;
#line 623 "Minus.m3"
};
#line 623 "Minus.m3"
LONGINT
__cdecl
Minus__Minus_var_L_u32(void)
{
#line 623 "Minus.m3"
Minus__Minus_var_L_u32_Frame_t _frame;
#line 623 "Minus.m3"
_frame._unused=(ADDRESS)&_frame;
#line 623 "Minus.m3"
 /* load */
#line 623 "Minus.m3"
 /* loophole */
#line 623 "Minus.m3"
 /* load */
#line 623 "Minus.m3"
 /* swap */
#line 623 "Minus.m3"
 /* subtract */
#line 623 "Minus.m3"
 /* exit_proc */
#line 623 "Minus.m3"
return ((INT64)(((INT64)(*((INT64*)(INT64_(192)+((ADDRESS)(&Minus_m_M_Minus_L_13))))))-((INT64)((INT64)((INT64)(*((UINT32*)(INT64_(184)+((ADDRESS)(&Minus_m_M_Minus_L_13))))))))));
#line 623 "Minus.m3"
 /* end_procedure */
#line 623 "Minus.m3"
} /* uMinus_param_L_u32 */
#line 623 "Minus.m3"
 /* set_source_line */
#line 623 "Minus.m3"
#line 624 "Minus.m3"
 /* begin_procedure */
#line 624 "Minus.m3"
struct Minus__uMinus_param_L_u32_Frame_t {
#line 624 "Minus.m3"
ADDRESS _unused;
#line 624 "Minus.m3"
};
#line 624 "Minus.m3"
Long__T
__cdecl
Minus__uMinus_param_L_u32(
   /* Param_Type1 */ LONGINT a_L_1155,
   /* Param_Type1 */ Minus__UINT32 b_L_1156)
{
#line 624 "Minus.m3"
Minus__uMinus_param_L_u32_Frame_t _frame;
#line 624 "Minus.m3"
_frame._unused=(ADDRESS)&_frame;
#line 624 "Minus.m3"
 /* load */
#line 624 "Minus.m3"
 /* loophole */
#line 624 "Minus.m3"
 /* load */
#line 624 "Minus.m3"
 /* swap */
#line 624 "Minus.m3"
 /* subtract */
#line 624 "Minus.m3"
 /* exit_proc */
#line 624 "Minus.m3"
return ((UINT64)(((UINT64)(a_L_1155))-((UINT64)((INT64)((INT64)(b_L_1156))))));
#line 624 "Minus.m3"
 /* end_procedure */
#line 624 "Minus.m3"
} /* Minus_param_L_u32 */
#line 624 "Minus.m3"
 /* set_source_line */
#line 624 "Minus.m3"
#line 625 "Minus.m3"
 /* begin_procedure */
#line 625 "Minus.m3"
struct Minus__Minus_param_L_u32_Frame_t {
#line 625 "Minus.m3"
ADDRESS _unused;
#line 625 "Minus.m3"
};
#line 625 "Minus.m3"
LONGINT
__cdecl
Minus__Minus_param_L_u32(
   /* Param_Type1 */ LONGINT a_L_1158,
   /* Param_Type1 */ Minus__UINT32 b_L_1159)
{
#line 625 "Minus.m3"
Minus__Minus_param_L_u32_Frame_t _frame;
#line 625 "Minus.m3"
_frame._unused=(ADDRESS)&_frame;
#line 625 "Minus.m3"
 /* load */
#line 625 "Minus.m3"
 /* loophole */
#line 625 "Minus.m3"
 /* load */
#line 625 "Minus.m3"
 /* swap */
#line 625 "Minus.m3"
 /* subtract */
#line 625 "Minus.m3"
 /* exit_proc */
#line 625 "Minus.m3"
return ((INT64)( a_L_1158-((INT64)((INT64)((INT64)(b_L_1159))))));
#line 625 "Minus.m3"
 /* end_procedure */
#line 625 "Minus.m3"
} /* uMinus_var_L_u8 */
#line 625 "Minus.m3"
 /* set_source_line */
#line 625 "Minus.m3"
#line 626 "Minus.m3"
 /* begin_procedure */
#line 626 "Minus.m3"
struct Minus__uMinus_var_L_u8_Frame_t {
#line 626 "Minus.m3"
ADDRESS _unused;
#line 626 "Minus.m3"
};
#line 626 "Minus.m3"
Long__T
__cdecl
Minus__uMinus_var_L_u8(void)
{
#line 626 "Minus.m3"
Minus__uMinus_var_L_u8_Frame_t _frame;
#line 626 "Minus.m3"
_frame._unused=(ADDRESS)&_frame;
#line 626 "Minus.m3"
 /* load */
#line 626 "Minus.m3"
 /* loophole */
#line 626 "Minus.m3"
 /* load */
#line 626 "Minus.m3"
 /* swap */
#line 626 "Minus.m3"
 /* subtract */
#line 626 "Minus.m3"
 /* exit_proc */
#line 626 "Minus.m3"
return ((UINT64)(((UINT64)(*((INT64*)(INT64_(192)+((ADDRESS)(&Minus_m_M_Minus_L_13))))))-((UINT64)((INT64)((INT64)(*((UINT8*)(INT64_(188)+((ADDRESS)(&Minus_m_M_Minus_L_13))))))))));
#line 626 "Minus.m3"
 /* end_procedure */
#line 626 "Minus.m3"
} /* Minus_var_L_u8 */
#line 626 "Minus.m3"
 /* set_source_line */
#line 626 "Minus.m3"
#line 627 "Minus.m3"
 /* begin_procedure */
#line 627 "Minus.m3"
struct Minus__Minus_var_L_u8_Frame_t {
#line 627 "Minus.m3"
ADDRESS _unused;
#line 627 "Minus.m3"
};
#line 627 "Minus.m3"
LONGINT
__cdecl
Minus__Minus_var_L_u8(void)
{
#line 627 "Minus.m3"
Minus__Minus_var_L_u8_Frame_t _frame;
#line 627 "Minus.m3"
_frame._unused=(ADDRESS)&_frame;
#line 627 "Minus.m3"
 /* load */
#line 627 "Minus.m3"
 /* loophole */
#line 627 "Minus.m3"
 /* load */
#line 627 "Minus.m3"
 /* swap */
#line 627 "Minus.m3"
 /* subtract */
#line 627 "Minus.m3"
 /* exit_proc */
#line 627 "Minus.m3"
return ((INT64)(((INT64)(*((INT64*)(INT64_(192)+((ADDRESS)(&Minus_m_M_Minus_L_13))))))-((INT64)((INT64)((INT64)(*((UINT8*)(INT64_(188)+((ADDRESS)(&Minus_m_M_Minus_L_13))))))))));
#line 627 "Minus.m3"
 /* end_procedure */
#line 627 "Minus.m3"
} /* uMinus_param_L_u8 */
#line 627 "Minus.m3"
 /* set_source_line */
#line 627 "Minus.m3"
#line 628 "Minus.m3"
 /* begin_procedure */
#line 628 "Minus.m3"
struct Minus__uMinus_param_L_u8_Frame_t {
#line 628 "Minus.m3"
ADDRESS _unused;
#line 628 "Minus.m3"
};
#line 628 "Minus.m3"
Long__T
__cdecl
Minus__uMinus_param_L_u8(
   /* Param_Type1 */ LONGINT a_L_1163,
   /* Param_Type1 */ Minus__UINT8 b_L_1164)
{
#line 628 "Minus.m3"
Minus__uMinus_param_L_u8_Frame_t _frame;
#line 628 "Minus.m3"
_frame._unused=(ADDRESS)&_frame;
#line 628 "Minus.m3"
 /* load */
#line 628 "Minus.m3"
 /* loophole */
#line 628 "Minus.m3"
 /* load */
#line 628 "Minus.m3"
 /* swap */
#line 628 "Minus.m3"
 /* subtract */
#line 628 "Minus.m3"
 /* exit_proc */
#line 628 "Minus.m3"
return ((UINT64)(((UINT64)(a_L_1163))-((UINT64)((INT64)((INT64)(b_L_1164))))));
#line 628 "Minus.m3"
 /* end_procedure */
#line 628 "Minus.m3"
} /* Minus_param_L_u8 */
#line 628 "Minus.m3"
 /* set_source_line */
#line 628 "Minus.m3"
#line 629 "Minus.m3"
 /* begin_procedure */
#line 629 "Minus.m3"
struct Minus__Minus_param_L_u8_Frame_t {
#line 629 "Minus.m3"
ADDRESS _unused;
#line 629 "Minus.m3"
};
#line 629 "Minus.m3"
LONGINT
__cdecl
Minus__Minus_param_L_u8(
   /* Param_Type1 */ LONGINT a_L_1166,
   /* Param_Type1 */ Minus__UINT8 b_L_1167)
{
#line 629 "Minus.m3"
Minus__Minus_param_L_u8_Frame_t _frame;
#line 629 "Minus.m3"
_frame._unused=(ADDRESS)&_frame;
#line 629 "Minus.m3"
 /* load */
#line 629 "Minus.m3"
 /* loophole */
#line 629 "Minus.m3"
 /* load */
#line 629 "Minus.m3"
 /* swap */
#line 629 "Minus.m3"
 /* subtract */
#line 629 "Minus.m3"
 /* exit_proc */
#line 629 "Minus.m3"
return ((INT64)( a_L_1166-((INT64)((INT64)((INT64)(b_L_1167))))));
#line 629 "Minus.m3"
 /* end_procedure */
#line 629 "Minus.m3"
} /* uMinus_var_L_L */
#line 629 "Minus.m3"
 /* set_source_line */
#line 629 "Minus.m3"
#line 630 "Minus.m3"
 /* begin_procedure */
#line 630 "Minus.m3"
struct Minus__uMinus_var_L_L_Frame_t {
#line 630 "Minus.m3"
ADDRESS _unused;
#line 630 "Minus.m3"
};
#line 630 "Minus.m3"
Long__T
__cdecl
Minus__uMinus_var_L_L(void)
{
#line 630 "Minus.m3"
Minus__uMinus_var_L_L_Frame_t _frame;
#line 630 "Minus.m3"
_frame._unused=(ADDRESS)&_frame;
#line 630 "Minus.m3"
 /* load */
#line 630 "Minus.m3"
 /* load */
#line 630 "Minus.m3"
 /* subtract */
#line 630 "Minus.m3"
 /* exit_proc */
#line 630 "Minus.m3"
return ((UINT64)(((UINT64)(*((INT64*)(INT64_(192)+((ADDRESS)(&Minus_m_M_Minus_L_13))))))-((UINT64)(*((INT64*)(INT64_(192)+((ADDRESS)(&Minus_m_M_Minus_L_13))))))));
#line 630 "Minus.m3"
 /* end_procedure */
#line 630 "Minus.m3"
} /* Minus_var_L_L */
#line 630 "Minus.m3"
 /* set_source_line */
#line 630 "Minus.m3"
#line 631 "Minus.m3"
 /* begin_procedure */
#line 631 "Minus.m3"
struct Minus__Minus_var_L_L_Frame_t {
#line 631 "Minus.m3"
ADDRESS _unused;
#line 631 "Minus.m3"
};
#line 631 "Minus.m3"
LONGINT
__cdecl
Minus__Minus_var_L_L(void)
{
#line 631 "Minus.m3"
Minus__Minus_var_L_L_Frame_t _frame;
#line 631 "Minus.m3"
_frame._unused=(ADDRESS)&_frame;
#line 631 "Minus.m3"
 /* load */
#line 631 "Minus.m3"
 /* load */
#line 631 "Minus.m3"
 /* subtract */
#line 631 "Minus.m3"
 /* exit_proc */
#line 631 "Minus.m3"
return ((INT64)(((INT64)(*((INT64*)(INT64_(192)+((ADDRESS)(&Minus_m_M_Minus_L_13))))))-((INT64)(*((INT64*)(INT64_(192)+((ADDRESS)(&Minus_m_M_Minus_L_13))))))));
#line 631 "Minus.m3"
 /* end_procedure */
#line 631 "Minus.m3"
} /* uMinus_param_L_L */
#line 631 "Minus.m3"
 /* set_source_line */
#line 631 "Minus.m3"
#line 632 "Minus.m3"
 /* begin_procedure */
#line 632 "Minus.m3"
struct Minus__uMinus_param_L_L_Frame_t {
#line 632 "Minus.m3"
ADDRESS _unused;
#line 632 "Minus.m3"
};
#line 632 "Minus.m3"
Long__T
__cdecl
Minus__uMinus_param_L_L(
   /* Param_Type1 */ LONGINT a_L_1171,
   /* Param_Type1 */ LONGINT b_L_1172)
{
#line 632 "Minus.m3"
Minus__uMinus_param_L_L_Frame_t _frame;
#line 632 "Minus.m3"
_frame._unused=(ADDRESS)&_frame;
#line 632 "Minus.m3"
 /* load */
#line 632 "Minus.m3"
 /* load */
#line 632 "Minus.m3"
 /* subtract */
#line 632 "Minus.m3"
 /* exit_proc */
#line 632 "Minus.m3"
return ((UINT64)(((UINT64)(a_L_1171))-((UINT64)(b_L_1172))));
#line 632 "Minus.m3"
 /* end_procedure */
#line 632 "Minus.m3"
} /* Minus_param_L_L */
#line 632 "Minus.m3"
 /* set_source_line */
#line 632 "Minus.m3"
#line 633 "Minus.m3"
 /* begin_procedure */
#line 633 "Minus.m3"
struct Minus__Minus_param_L_L_Frame_t {
#line 633 "Minus.m3"
ADDRESS _unused;
#line 633 "Minus.m3"
};
#line 633 "Minus.m3"
LONGINT
__cdecl
Minus__Minus_param_L_L(
   /* Param_Type1 */ LONGINT a_L_1174,
   /* Param_Type1 */ LONGINT b_L_1175)
{
#line 633 "Minus.m3"
Minus__Minus_param_L_L_Frame_t _frame;
#line 633 "Minus.m3"
_frame._unused=(ADDRESS)&_frame;
#line 633 "Minus.m3"
 /* load */
#line 633 "Minus.m3"
 /* load */
#line 633 "Minus.m3"
 /* subtract */
#line 633 "Minus.m3"
 /* exit_proc */
#line 633 "Minus.m3"
return ((INT64)( a_L_1174- b_L_1175));
#line 633 "Minus.m3"
 /* end_procedure */
#line 633 "Minus.m3"
} /* Minus_M3 */
#line 633 "Minus.m3"
 /* module main body Minus_M3 */
#line 633 "Minus.m3"
 /* set_source_line */
#line 633 "Minus.m3"
#line 634 "Minus.m3"
 /* begin_procedure */
#line 634 "Minus.m3"
struct Minus_M3_Frame_t {
#line 634 "Minus.m3"
ADDRESS _unused;
#line 634 "Minus.m3"
};
#line 634 "Minus.m3"
RT0__ModulePtr
__cdecl
Minus_M3(
   /* Param_Type1 */ INTEGER /*TypeText1*/  mode_L_14)
{
#line 634 "Minus.m3"
Minus_M3_Frame_t _frame;
#line 634 "Minus.m3"
_frame._unused=(ADDRESS)&_frame;
#line 634 "Minus.m3"
 /* load */
#line 634 "Minus.m3"
 /* if_true_or_false */
#line 634 "Minus.m3"
 /* load_host_integer */
#line 634 "Minus.m3"
 /* load_integer */
#line 634 "Minus.m3"
 /* if_compare */
#line 634 "Minus.m3"
if(m3_eq(INT64,
  mode_L_14,
   INT64_(0)))goto L1;
#line 634 "Minus.m3"
 /* set_label */
#line 634 "Minus.m3"
L1:;
#line 634 "Minus.m3"
 /* load_address */
#line 634 "Minus.m3"
 /* exit_proc */
#line 634 "Minus.m3"
return (RT0__ModulePtr)(&Minus_m_M_Minus_L_13);
#line 634 "Minus.m3"
 /* end_procedure */
#line 634 "Minus.m3"
} /* global constant type descriptor */
#line 634 "Minus.m3"
 /* global data type descriptor */
#line 634 "Minus.m3"
 /* module global constants */
#line 634 "Minus.m3"
 /* procedure names */
#line 634 "Minus.m3"
 /* procedure table */
#line 634 "Minus.m3"
 /* file name */
#line 634 "Minus.m3"
 /* module global data */
#line 634 "Minus.m3"
 /* load map


 global data allocation for M_Minus
     0   104  8  *module info*
   104     1  1  Minus.vi8
   112     8  8  Minus.vu64
   120     8  8  Minus.vf64
   128     4  4  Minus.vi32
   136     8  8  Minus.vLC
   144     2  2  Minus.vu16
   152     8  8  Minus.vI
   160     8  8  Minus.vi64
   168     4  4  Minus.vf32
   172     2  2  Minus.vi16
   176     8  8  Minus.vC
   184     4  4  Minus.vu32
   188     1  1  Minus.vu8
   192     8  8  Minus.vL
   200     8  8  Minus.offset
   208     8  8  Minus.count
   216    24  8  import Minus
   240    24  8  import Long
   264    24  8  import Word
   288    24  8  import Cstdint
   312    24  8  import RTHooks
   336     0  8  *TOTAL*


 global constants for M_Minus
     0 10453  8  *proc names*
 10456  9304  8  *proc info*
 19760     9  1  *string*
 19776     0  8  *TOTAL*
 */
#line 634 "Minus.m3"
 /* end unit */
#line 634 "Minus.m3"

#ifdef __cplusplus

} /* extern "C" */
#endif
 /* set_runtime_proc */
 /* set_runtime_proc */
 /* set_runtime_proc */
