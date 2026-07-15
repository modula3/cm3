// library:pgm
// source_base_name:Main
// target_name:Main.m3.cpp
 /* set_runtime_proc */
 /* set_runtime_proc */
 /* set_runtime_proc */
 /* set_runtime_proc */
 /* set_runtime_proc */
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
 /* declare_proctype */

#if 0 /* avoid type hash collions */
typedef 
void(__cdecl*T7B78C34F)(void);
#else
typedef void (__cdecl*T7B78C34F)(void);
#endif
 /* declare_subrange */
/*subrange_define*/typedef UINT8 T8A2831D7_8;
 /* declare_array */
/*array_forwardDeclare*/struct T32EE4DC8;typedef struct T32EE4DC8 T32EE4DC8;

#ifndef T32EE4DC8
#define T32EE4DC8 T32EE4DC8
/*fixedArray_define*/struct T32EE4DC8{UCHAR _elts[1];};
#endif
 /* declare_record */
 /* declare_field */
 /* record_forwardDeclare Record_t{ typeid:T6A6EC077 text:NIL hash_text:T6A6EC077 base_text:NIL state:0} */
/*record_forwardDeclare*/struct T6A6EC077;typedef struct T6A6EC077 T6A6EC077;
 /* record_canBeDefined Record_t{ typeid:T6A6EC077 text:NIL hash_text:T6A6EC077 base_text:NIL state:0} */
 /* record_define Record_t{ typeid:T6A6EC077 text:NIL hash_text:T6A6EC077 base_text:NIL state:0} */

#ifndef T6A6EC077
#define T6A6EC077 T6A6EC077
/*record_define*/struct T6A6EC077{
T32EE4DC8 x0;
};
#endif
 /* declare_pointer */
typedef T6A6EC077*T98E80006;
 /* declare_subrange */
/*subrange_define*/typedef UINT8 T8B2831D7_8;
 /* declare_array */
/*array_forwardDeclare*/struct T8E6B09BA;typedef struct T8E6B09BA T8E6B09BA;

#ifndef T8E6B09BA
#define T8E6B09BA T8E6B09BA
/*fixedArray_define*/struct T8E6B09BA{UCHAR _elts[2];};
#endif
 /* declare_record */
 /* declare_field */
 /* record_forwardDeclare Record_t{ typeid:TEDC13487 text:NIL hash_text:TEDC13487 base_text:NIL state:0} */
/*record_forwardDeclare*/struct TEDC13487;typedef struct TEDC13487 TEDC13487;
 /* record_canBeDefined Record_t{ typeid:TEDC13487 text:NIL hash_text:TEDC13487 base_text:NIL state:0} */
 /* record_define Record_t{ typeid:TEDC13487 text:NIL hash_text:TEDC13487 base_text:NIL state:0} */

#ifndef TEDC13487
#define TEDC13487 TEDC13487
/*record_define*/struct TEDC13487{
T8E6B09BA x0;
};
#endif
 /* declare_pointer */
typedef TEDC13487*T84DB404A;
 /* declare_subrange */
/*subrange_define*/typedef UINT8 T882831D7_8;
 /* declare_array */
/*array_forwardDeclare*/struct TAE14F91B;typedef struct TAE14F91B TAE14F91B;

#ifndef TAE14F91B
#define TAE14F91B TAE14F91B
/*fixedArray_define*/struct TAE14F91B{UCHAR _elts[3];};
#endif
 /* declare_record */
 /* declare_field */
 /* record_forwardDeclare Record_t{ typeid:T45C03711 text:NIL hash_text:T45C03711 base_text:NIL state:0} */
/*record_forwardDeclare*/struct T45C03711;typedef struct T45C03711 T45C03711;
 /* record_canBeDefined Record_t{ typeid:T45C03711 text:NIL hash_text:T45C03711 base_text:NIL state:0} */
 /* record_define Record_t{ typeid:T45C03711 text:NIL hash_text:T45C03711 base_text:NIL state:0} */

#ifndef T45C03711
#define T45C03711 T45C03711
/*record_define*/struct T45C03711{
TAE14F91B x0;
};
#endif
 /* declare_pointer */
typedef T45C03711*T843F3291;
 /* declare_subrange */
/*subrange_define*/typedef UINT16 T4A3873BD_16;
 /* declare_array */
/*array_forwardDeclare*/struct T250890B4;typedef struct T250890B4 T250890B4;

#ifndef T250890B4
#define T250890B4 T250890B4
/*fixedArray_define*/struct T250890B4{UCHAR _elts[4095];};
#endif
 /* declare_record */
 /* declare_field */
 /* record_forwardDeclare Record_t{ typeid:T1DDF1C23 text:NIL hash_text:T1DDF1C23 base_text:NIL state:0} */
/*record_forwardDeclare*/struct T1DDF1C23;typedef struct T1DDF1C23 T1DDF1C23;
 /* record_canBeDefined Record_t{ typeid:T1DDF1C23 text:NIL hash_text:T1DDF1C23 base_text:NIL state:0} */
 /* record_define Record_t{ typeid:T1DDF1C23 text:NIL hash_text:T1DDF1C23 base_text:NIL state:0} */

#ifndef T1DDF1C23
#define T1DDF1C23 T1DDF1C23
/*record_define*/struct T1DDF1C23{
T250890B4 x0;
};
#endif
 /* declare_pointer */
typedef T1DDF1C23*T5FDE5EB7;
 /* declare_subrange */
/*subrange_define*/typedef UINT16 T4B3873BD_16;
 /* declare_array */
/*array_forwardDeclare*/struct T998DD4C6;typedef struct T998DD4C6 T998DD4C6;

#ifndef T998DD4C6
#define T998DD4C6 T998DD4C6
/*fixedArray_define*/struct T998DD4C6{UCHAR _elts[4096];};
#endif
 /* declare_record */
 /* declare_field */
 /* record_forwardDeclare Record_t{ typeid:T9A70E8D3 text:NIL hash_text:T9A70E8D3 base_text:NIL state:0} */
/*record_forwardDeclare*/struct T9A70E8D3;typedef struct T9A70E8D3 T9A70E8D3;
 /* record_canBeDefined Record_t{ typeid:T9A70E8D3 text:NIL hash_text:T9A70E8D3 base_text:NIL state:0} */
 /* record_define Record_t{ typeid:T9A70E8D3 text:NIL hash_text:T9A70E8D3 base_text:NIL state:0} */

#ifndef T9A70E8D3
#define T9A70E8D3 T9A70E8D3
/*record_define*/struct T9A70E8D3{
T998DD4C6 x0;
};
#endif
 /* declare_pointer */
typedef T9A70E8D3*T43ED1EFB;
 /* declare_subrange */
/*subrange_define*/typedef UINT16 T483873BD_16;
 /* declare_array */
/*array_forwardDeclare*/struct TB9F22467;typedef struct TB9F22467 TB9F22467;

#ifndef TB9F22467
#define TB9F22467 TB9F22467
/*fixedArray_define*/struct TB9F22467{UCHAR _elts[4097];};
#endif
 /* declare_record */
 /* declare_field */
 /* record_forwardDeclare Record_t{ typeid:T3271EB45 text:NIL hash_text:T3271EB45 base_text:NIL state:0} */
/*record_forwardDeclare*/struct T3271EB45;typedef struct T3271EB45 T3271EB45;
 /* record_canBeDefined Record_t{ typeid:T3271EB45 text:NIL hash_text:T3271EB45 base_text:NIL state:0} */
 /* record_define Record_t{ typeid:T3271EB45 text:NIL hash_text:T3271EB45 base_text:NIL state:0} */

#ifndef T3271EB45
#define T3271EB45 T3271EB45
/*record_define*/struct T3271EB45{
TB9F22467 x0;
};
#endif
 /* declare_pointer */
typedef T3271EB45*T43096C20;
 /* declare_subrange */
/*subrange_define*/typedef UINT16 T493873BD_16;
 /* declare_array */
/*array_forwardDeclare*/struct T5776015;typedef struct T5776015 T5776015;

#ifndef T5776015
#define T5776015 T5776015
/*fixedArray_define*/struct T5776015{UCHAR _elts[4098];};
#endif
 /* declare_record */
 /* declare_field */
 /* record_forwardDeclare Record_t{ typeid:TB5DE1FB5 text:NIL hash_text:TB5DE1FB5 base_text:NIL state:0} */
/*record_forwardDeclare*/struct TB5DE1FB5;typedef struct TB5DE1FB5 TB5DE1FB5;
 /* record_canBeDefined Record_t{ typeid:TB5DE1FB5 text:NIL hash_text:TB5DE1FB5 base_text:NIL state:0} */
 /* record_define Record_t{ typeid:TB5DE1FB5 text:NIL hash_text:TB5DE1FB5 base_text:NIL state:0} */

#ifndef TB5DE1FB5
#define TB5DE1FB5 TB5DE1FB5
/*record_define*/struct TB5DE1FB5{
T5776015 x0;
};
#endif
 /* declare_pointer */
typedef TB5DE1FB5*T5F3A2C6C;
 /* declare_subrange */
/*subrange_define*/typedef UINT16 TA2B787D9_16;
 /* declare_array */
/*array_forwardDeclare*/struct T5243890B;typedef struct T5243890B T5243890B;

#ifndef T5243890B
#define T5243890B T5243890B
/*fixedArray_define*/struct T5243890B{UCHAR _elts[32767];};
#endif
 /* declare_record */
 /* declare_field */
 /* record_forwardDeclare Record_t{ typeid:TB08DBACF text:NIL hash_text:TB08DBACF base_text:NIL state:0} */
/*record_forwardDeclare*/struct TB08DBACF;typedef struct TB08DBACF TB08DBACF;
 /* record_canBeDefined Record_t{ typeid:TB08DBACF text:NIL hash_text:TB08DBACF base_text:NIL state:0} */
 /* record_define Record_t{ typeid:TB08DBACF text:NIL hash_text:TB08DBACF base_text:NIL state:0} */

#ifndef TB08DBACF
#define TB08DBACF TB08DBACF
/*record_define*/struct TB08DBACF{
T5243890B x0;
};
#endif
 /* declare_pointer */
typedef TB08DBACF*T82E79CA7;
 /* declare_subrange */
/*subrange_define*/typedef UINT16 TA3B787D9_16;
 /* declare_array */
/*array_forwardDeclare*/struct TEEC6CD79;typedef struct TEEC6CD79 TEEC6CD79;

#ifndef TEEC6CD79
#define TEEC6CD79 TEEC6CD79
/*fixedArray_define*/struct TEEC6CD79{UCHAR _elts[32768];};
#endif
 /* declare_record */
 /* declare_field */
 /* record_forwardDeclare Record_t{ typeid:T37224E3F text:NIL hash_text:T37224E3F base_text:NIL state:0} */
/*record_forwardDeclare*/struct T37224E3F;typedef struct T37224E3F T37224E3F;
 /* record_canBeDefined Record_t{ typeid:T37224E3F text:NIL hash_text:T37224E3F base_text:NIL state:0} */
 /* record_define Record_t{ typeid:T37224E3F text:NIL hash_text:T37224E3F base_text:NIL state:0} */

#ifndef T37224E3F
#define T37224E3F T37224E3F
/*record_define*/struct T37224E3F{
TEEC6CD79 x0;
};
#endif
 /* declare_pointer */
typedef T37224E3F*T9ED4DCEB;
 /* declare_subrange */
/*subrange_define*/typedef UINT16 TACB787D9_16;
 /* declare_array */
/*array_forwardDeclare*/struct TA8E6FBA8;typedef struct TA8E6FBA8 TA8E6FBA8;

#ifndef TA8E6FBA8
#define TA8E6FBA8 TA8E6FBA8
/*fixedArray_define*/struct TA8E6FBA8{UCHAR _elts[32769];};
#endif
 /* declare_record */
 /* declare_field */
 /* record_forwardDeclare Record_t{ typeid:TDB726D9C text:NIL hash_text:TDB726D9C base_text:NIL state:0} */
/*record_forwardDeclare*/struct TDB726D9C;typedef struct TDB726D9C TDB726D9C;
 /* record_canBeDefined Record_t{ typeid:TDB726D9C text:NIL hash_text:TDB726D9C base_text:NIL state:0} */
 /* record_define Record_t{ typeid:TDB726D9C text:NIL hash_text:TDB726D9C base_text:NIL state:0} */

#ifndef TDB726D9C
#define TDB726D9C TDB726D9C
/*record_define*/struct TDB726D9C{
TA8E6FBA8 x0;
};
#endif
 /* declare_pointer */
typedef TDB726D9C*TE3F29D5B;
 /* declare_subrange */
/*subrange_define*/typedef UINT16 TADB787D9_16;
 /* declare_array */
/*array_forwardDeclare*/struct T1463BFDA;typedef struct T1463BFDA T1463BFDA;

#ifndef T1463BFDA
#define T1463BFDA T1463BFDA
/*fixedArray_define*/struct T1463BFDA{UCHAR _elts[32770];};
#endif
 /* declare_record */
 /* declare_field */
 /* record_forwardDeclare Record_t{ typeid:T5CDD996C text:NIL hash_text:T5CDD996C base_text:NIL state:0} */
/*record_forwardDeclare*/struct T5CDD996C;typedef struct T5CDD996C T5CDD996C;
 /* record_canBeDefined Record_t{ typeid:T5CDD996C text:NIL hash_text:T5CDD996C base_text:NIL state:0} */
 /* record_define Record_t{ typeid:T5CDD996C text:NIL hash_text:T5CDD996C base_text:NIL state:0} */

#ifndef T5CDD996C
#define T5CDD996C T5CDD996C
/*record_define*/struct T5CDD996C{
T1463BFDA x0;
};
#endif
 /* declare_pointer */
typedef T5CDD996C*TFFC1DD17;
 /* declare_subrange */
/*subrange_define*/typedef UINT16 TA4B687D9_16;
 /* declare_array */
/*array_forwardDeclare*/struct T25F65FAC;typedef struct T25F65FAC T25F65FAC;

#ifndef T25F65FAC
#define T25F65FAC T25F65FAC
/*fixedArray_define*/struct T25F65FAC{UCHAR _elts[32771];};
#endif
 /* declare_record */
 /* declare_field */
 /* record_forwardDeclare Record_t{ typeid:TC4134582 text:NIL hash_text:TC4134582 base_text:NIL state:0} */
/*record_forwardDeclare*/struct TC4134582;typedef struct TC4134582 TC4134582;
 /* record_canBeDefined Record_t{ typeid:TC4134582 text:NIL hash_text:TC4134582 base_text:NIL state:0} */
 /* record_define Record_t{ typeid:TC4134582 text:NIL hash_text:TC4134582 base_text:NIL state:0} */

#ifndef TC4134582
#define TC4134582 TC4134582
/*record_define*/struct TC4134582{
T25F65FAC x0;
};
#endif
 /* declare_pointer */
typedef TC4134582*TE2ADCACE;
 /* declare_proctype */
 /* declare_formal */
 /* declare_proctype */
 /* declare_formal */
 /* declare_proctype */
 /* declare_formal */
 /* declare_proctype */
 /* declare_formal */
 /* declare_proctype */
 /* declare_formal */
 /* declare_proctype */
 /* declare_formal */
 /* declare_proctype */
 /* declare_formal */
 /* declare_proctype */
 /* declare_formal */
 /* declare_proctype */
 /* declare_formal */
 /* declare_proctype */
 /* declare_formal */
 /* declare_proctype */
 /* declare_formal */
 /* declare_proctype */
 /* declare_formal */
 /* declare_proctype */
 /* declare_formal */
 /* declare_proctype */
 /* declare_formal */
 /* declare_opaque */

#ifndef T6D871B27
#define T6D871B27 T6D871B27
/*1addressType_define*/typedef ADDRESS T6D871B27;

#endif
 /* declare_record */
 /* declare_field */
 /* declare_field */
 /* declare_field */
 /* record_forwardDeclare Record_t{ typeid:TB750D684 text:NIL hash_text:TB750D684 base_text:NIL state:0} */
/*record_forwardDeclare*/struct TB750D684;typedef struct TB750D684 TB750D684;
 /* record_canBeDefined Record_t{ typeid:TB750D684 text:NIL hash_text:TB750D684 base_text:NIL state:0} */
 /* record_define Record_t{ typeid:TB750D684 text:NIL hash_text:TB750D684 base_text:NIL state:0} */

#ifndef TB750D684
#define TB750D684 TB750D684
/*record_define*/struct TB750D684{
ADDRESS start;
WORD_T length;
BOOLEAN wide;
UINT8 L_0[7];
};
#endif
 /* declare_indirect */

#ifndef RTHooks__TextInfo
#define RTHooks__TextInfo RTHooks__TextInfo
typedef TB750D684 RTHooks__TextInfo;
#endif
typedef RTHooks__TextInfo*T48AF297B;
 /* declare_proctype */
 /* declare_formal */

#ifndef RTHooks__TextLiteral
#define RTHooks__TextLiteral RTHooks__TextLiteral
typedef T6D871B27 RTHooks__TextLiteral;
#endif
 /* declare_formal */
 /* declare_proctype */
 /* declare_formal */
 /* declare_formal */
 /* declare_proctype */
 /* declare_formal */
 /* declare_formal */
 /* declare_open_array */
/*array_forwardDeclare*/struct T89CD34BD;typedef struct T89CD34BD T89CD34BD;

#ifndef T89CD34BD
#define T89CD34BD T89CD34BD
/*openArray_define*/struct T89CD34BD{
UCHAR*_elts;
CARDINAL _size;
};

#endif
 /* declare_indirect */
typedef T89CD34BD*T7632CB42;
 /* declare_proctype */
 /* declare_formal */
 /* declare_formal */
 /* declare_formal */
 /* declare_open_array */
/*array_forwardDeclare*/struct TA19BDC21;typedef struct TA19BDC21 TA19BDC21;

#ifndef TA19BDC21
#define TA19BDC21 TA19BDC21
/*openArray_define*/struct TA19BDC21{
WIDECHAR*_elts;
CARDINAL _size;
};

#endif
 /* declare_indirect */
typedef TA19BDC21*T5E6423DE;
 /* declare_proctype */
 /* declare_formal */
 /* declare_formal */
 /* declare_formal */
 /* declare_record */
 /* declare_record */
 /* DeclareTypes_FlushOnce size:19 */

#if 0 /* avoid type hash collions */
typedef 
REFANY(__cdecl*T7CFE252F)(ADDRESS);
#else
typedef void (__cdecl*T7CFE252F)(void);
#endif

#if 0 /* avoid type hash collions */
typedef 
UCHAR(__cdecl*T4A5F2BBD)(T6A6EC077*);
#else
typedef void (__cdecl*T4A5F2BBD)(void);
#endif

#if 0 /* avoid type hash collions */
typedef 
UCHAR(__cdecl*T267BCFA3)(TEDC13487*);
#else
typedef void (__cdecl*T267BCFA3)(void);
#endif

#if 0 /* avoid type hash collions */
typedef 
UCHAR(__cdecl*T9F49C722)(T45C03711*);
#else
typedef void (__cdecl*T9F49C722)(void);
#endif

#if 0 /* avoid type hash collions */
typedef 
UCHAR(__cdecl*TC47EF354)(T1DDF1C23*);
#else
typedef void (__cdecl*TC47EF354)(void);
#endif

#if 0 /* avoid type hash collions */
typedef 
UCHAR(__cdecl*TA85A174A)(T9A70E8D3*);
#else
typedef void (__cdecl*TA85A174A)(void);
#endif

#if 0 /* avoid type hash collions */
typedef 
UCHAR(__cdecl*T11681FCB)(T3271EB45*);
#else
typedef void (__cdecl*T11681FCB)(void);
#endif

#if 0 /* avoid type hash collions */
typedef 
UCHAR(__cdecl*T7D4CFBD5)(TB5DE1FB5*);
#else
typedef void (__cdecl*T7D4CFBD5)(void);
#endif

#if 0 /* avoid type hash collions */
typedef 
UCHAR(__cdecl*T5D5990C8)(TB08DBACF*);
#else
typedef void (__cdecl*T5D5990C8)(void);
#endif

#if 0 /* avoid type hash collions */
typedef 
UCHAR(__cdecl*T317D74D6)(T37224E3F*);
#else
typedef void (__cdecl*T317D74D6)(void);
#endif

#if 0 /* avoid type hash collions */
typedef 
UCHAR(__cdecl*TA2AC628F)(TDB726D9C*);
#else
typedef void (__cdecl*TA2AC628F)(void);
#endif

#if 0 /* avoid type hash collions */
typedef 
UCHAR(__cdecl*TCE888691)(T5CDD996C*);
#else
typedef void (__cdecl*TCE888691)(void);
#endif

#if 0 /* avoid type hash collions */
typedef 
UCHAR(__cdecl*T2CC5EF95)(TC4134582*);
#else
typedef void (__cdecl*T2CC5EF95)(void);
#endif

#if 0 /* avoid type hash collions */
typedef 
BOOLEAN(__cdecl*T7D5586EC)(TEXT);
#else
typedef void (__cdecl*T7D5586EC)(void);
#endif

#if 0 /* avoid type hash collions */
typedef 
void(__cdecl*T38DF5DF5)(RTHooks__TextLiteral,RTHooks__TextInfo*);
#else
typedef void (__cdecl*T38DF5DF5)(void);
#endif

#if 0 /* avoid type hash collions */
typedef 
UCHAR(__cdecl*TB964357B)(RTHooks__TextLiteral,CARDINAL);
#else
typedef void (__cdecl*TB964357B)(void);
#endif

#if 0 /* avoid type hash collions */
typedef 
WIDECHAR(__cdecl*T9132DDE7)(RTHooks__TextLiteral,CARDINAL);
#else
typedef void (__cdecl*T9132DDE7)(void);
#endif

#if 0 /* avoid type hash collions */
typedef 
void(__cdecl*TC9E236C8)(RTHooks__TextLiteral,T89CD34BD*,CARDINAL);
#else
typedef void (__cdecl*TC9E236C8)(void);
#endif

#if 0 /* avoid type hash collions */
typedef 
void(__cdecl*T966FE87F)(RTHooks__TextLiteral,TA19BDC21*,CARDINAL);
#else
typedef void (__cdecl*T966FE87F)(void);
#endif
 /* DeclareTypes_FlushOnce size:0 */
 /* end: DeclareTypes */
 /* begin: helper functions */
 /* end: helper functions */
 /* begin: imports */
 /* import_procedure */

#ifndef RT0__ModulePtr
#define RT0__ModulePtr RT0__ModulePtr
typedef ADDRESS /*TypeText3*/  RT0__ModulePtr;
#endif
/*Proc_ForwardDeclareFrameType*/struct Main_I3_Frame_t;typedef struct Main_I3_Frame_t Main_I3_Frame_t;
 /* internal_declare_param */
RT0__ModulePtr
__cdecl
Main_I3(
   /* Param_Type1 */ INTEGER /*TypeText1*/  mode_L_1);
 /* import_procedure */
/*Proc_ForwardDeclareFrameType*/struct RTParams_I3_Frame_t;typedef struct RTParams_I3_Frame_t RTParams_I3_Frame_t;
 /* internal_declare_param */
RT0__ModulePtr
__cdecl
RTParams_I3(
   /* Param_Type1 */ INTEGER /*TypeText1*/  mode_L_2);
 /* import_procedure */
/*Proc_ForwardDeclareFrameType*/struct F4096x8m2_I3_Frame_t;typedef struct F4096x8m2_I3_Frame_t F4096x8m2_I3_Frame_t;
 /* internal_declare_param */
RT0__ModulePtr
__cdecl
F4096x8m2_I3(
   /* Param_Type1 */ INTEGER /*TypeText1*/  mode_L_3);
 /* import_procedure */
/*Proc_ForwardDeclareFrameType*/struct F4096x8m1_I3_Frame_t;typedef struct F4096x8m1_I3_Frame_t F4096x8m1_I3_Frame_t;
 /* internal_declare_param */
RT0__ModulePtr
__cdecl
F4096x8m1_I3(
   /* Param_Type1 */ INTEGER /*TypeText1*/  mode_L_4);
 /* import_procedure */
/*Proc_ForwardDeclareFrameType*/struct F4096x8p2_I3_Frame_t;typedef struct F4096x8p2_I3_Frame_t F4096x8p2_I3_Frame_t;
 /* internal_declare_param */
RT0__ModulePtr
__cdecl
F4096x8p2_I3(
   /* Param_Type1 */ INTEGER /*TypeText1*/  mode_L_5);
 /* import_procedure */
/*Proc_ForwardDeclareFrameType*/struct F4096x8p1_I3_Frame_t;typedef struct F4096x8p1_I3_Frame_t F4096x8p1_I3_Frame_t;
 /* internal_declare_param */
RT0__ModulePtr
__cdecl
F4096x8p1_I3(
   /* Param_Type1 */ INTEGER /*TypeText1*/  mode_L_6);
 /* import_procedure */
/*Proc_ForwardDeclareFrameType*/struct F4096x8_I3_Frame_t;typedef struct F4096x8_I3_Frame_t F4096x8_I3_Frame_t;
 /* internal_declare_param */
RT0__ModulePtr
__cdecl
F4096x8_I3(
   /* Param_Type1 */ INTEGER /*TypeText1*/  mode_L_7);
 /* import_procedure */
/*Proc_ForwardDeclareFrameType*/struct F4097_I3_Frame_t;typedef struct F4097_I3_Frame_t F4097_I3_Frame_t;
 /* internal_declare_param */
RT0__ModulePtr
__cdecl
F4097_I3(
   /* Param_Type1 */ INTEGER /*TypeText1*/  mode_L_8);
 /* import_procedure */
/*Proc_ForwardDeclareFrameType*/struct F4096_I3_Frame_t;typedef struct F4096_I3_Frame_t F4096_I3_Frame_t;
 /* internal_declare_param */
RT0__ModulePtr
__cdecl
F4096_I3(
   /* Param_Type1 */ INTEGER /*TypeText1*/  mode_L_9);
 /* import_procedure */
/*Proc_ForwardDeclareFrameType*/struct F4095_I3_Frame_t;typedef struct F4095_I3_Frame_t F4095_I3_Frame_t;
 /* internal_declare_param */
RT0__ModulePtr
__cdecl
F4095_I3(
   /* Param_Type1 */ INTEGER /*TypeText1*/  mode_L_10);
 /* import_procedure */
/*Proc_ForwardDeclareFrameType*/struct F4094_I3_Frame_t;typedef struct F4094_I3_Frame_t F4094_I3_Frame_t;
 /* internal_declare_param */
RT0__ModulePtr
__cdecl
F4094_I3(
   /* Param_Type1 */ INTEGER /*TypeText1*/  mode_L_11);
 /* import_procedure */
/*Proc_ForwardDeclareFrameType*/struct F2_I3_Frame_t;typedef struct F2_I3_Frame_t F2_I3_Frame_t;
 /* internal_declare_param */
RT0__ModulePtr
__cdecl
F2_I3(
   /* Param_Type1 */ INTEGER /*TypeText1*/  mode_L_12);
 /* import_procedure */
/*Proc_ForwardDeclareFrameType*/struct F1_I3_Frame_t;typedef struct F1_I3_Frame_t F1_I3_Frame_t;
 /* internal_declare_param */
RT0__ModulePtr
__cdecl
F1_I3(
   /* Param_Type1 */ INTEGER /*TypeText1*/  mode_L_13);
 /* import_procedure */
/*Proc_ForwardDeclareFrameType*/struct F0_I3_Frame_t;typedef struct F0_I3_Frame_t F0_I3_Frame_t;
 /* internal_declare_param */
RT0__ModulePtr
__cdecl
F0_I3(
   /* Param_Type1 */ INTEGER /*TypeText1*/  mode_L_14);
 /* import_procedure */
/*Proc_ForwardDeclareFrameType*/struct RTHooks_I3_Frame_t;typedef struct RTHooks_I3_Frame_t RTHooks_I3_Frame_t;
 /* internal_declare_param */
RT0__ModulePtr
__cdecl
RTHooks_I3(
   /* Param_Type1 */ INTEGER /*TypeText1*/  mode_L_15);
 /* import_procedure */
/*Proc_ForwardDeclareFrameType*/struct RTHooks__AllocateTracedRef_Frame_t;typedef struct RTHooks__AllocateTracedRef_Frame_t RTHooks__AllocateTracedRef_Frame_t;
 /* internal_declare_param */
REFANY
__cdecl
RTHooks__AllocateTracedRef(
   /* Param_Type1 */ ADDRESS t_L_16);
 /* import_procedure */

#ifndef m3_CHAR
#define m3_CHAR m3_CHAR
typedef UCHAR /*TypeText1*/  m3_CHAR;
#endif
/*Proc_ForwardDeclareFrameType*/struct F0__F1_Frame_t;typedef struct F0__F1_Frame_t F0__F1_Frame_t;
 /* internal_declare_param */
m3_CHAR
__cdecl
F0__F1(
   /* Param_Type1 */ T6A6EC077* /*TypeText1*/  t_L_17);
 /* import_procedure */
/*Proc_ForwardDeclareFrameType*/struct F1__F1_Frame_t;typedef struct F1__F1_Frame_t F1__F1_Frame_t;
 /* internal_declare_param */
m3_CHAR
__cdecl
F1__F1(
   /* Param_Type1 */ TEDC13487* /*TypeText1*/  t_L_18);
 /* import_procedure */
/*Proc_ForwardDeclareFrameType*/struct F2__F1_Frame_t;typedef struct F2__F1_Frame_t F2__F1_Frame_t;
 /* internal_declare_param */
m3_CHAR
__cdecl
F2__F1(
   /* Param_Type1 */ T45C03711* /*TypeText1*/  t_L_19);
 /* import_procedure */
/*Proc_ForwardDeclareFrameType*/struct F4094__F1_Frame_t;typedef struct F4094__F1_Frame_t F4094__F1_Frame_t;
 /* internal_declare_param */
m3_CHAR
__cdecl
F4094__F1(
   /* Param_Type1 */ T1DDF1C23* /*TypeText1*/  t_L_20);
 /* import_procedure */
/*Proc_ForwardDeclareFrameType*/struct F4095__F1_Frame_t;typedef struct F4095__F1_Frame_t F4095__F1_Frame_t;
 /* internal_declare_param */
m3_CHAR
__cdecl
F4095__F1(
   /* Param_Type1 */ T9A70E8D3* /*TypeText1*/  t_L_21);
 /* import_procedure */
/*Proc_ForwardDeclareFrameType*/struct F4096__F1_Frame_t;typedef struct F4096__F1_Frame_t F4096__F1_Frame_t;
 /* internal_declare_param */
m3_CHAR
__cdecl
F4096__F1(
   /* Param_Type1 */ T3271EB45* /*TypeText1*/  t_L_22);
 /* import_procedure */
/*Proc_ForwardDeclareFrameType*/struct F4097__F1_Frame_t;typedef struct F4097__F1_Frame_t F4097__F1_Frame_t;
 /* internal_declare_param */
m3_CHAR
__cdecl
F4097__F1(
   /* Param_Type1 */ TB5DE1FB5* /*TypeText1*/  t_L_23);
 /* import_procedure */
/*Proc_ForwardDeclareFrameType*/struct F4096x8m2__F1_Frame_t;typedef struct F4096x8m2__F1_Frame_t F4096x8m2__F1_Frame_t;
 /* internal_declare_param */
m3_CHAR
__cdecl
F4096x8m2__F1(
   /* Param_Type1 */ TB08DBACF* /*TypeText1*/  t_L_24);
 /* import_procedure */
/*Proc_ForwardDeclareFrameType*/struct F4096x8m1__F1_Frame_t;typedef struct F4096x8m1__F1_Frame_t F4096x8m1__F1_Frame_t;
 /* internal_declare_param */
m3_CHAR
__cdecl
F4096x8m1__F1(
   /* Param_Type1 */ T37224E3F* /*TypeText1*/  t_L_25);
 /* import_procedure */
/*Proc_ForwardDeclareFrameType*/struct F4096x8__F1_Frame_t;typedef struct F4096x8__F1_Frame_t F4096x8__F1_Frame_t;
 /* internal_declare_param */
m3_CHAR
__cdecl
F4096x8__F1(
   /* Param_Type1 */ TDB726D9C* /*TypeText1*/  t_L_26);
 /* import_procedure */
/*Proc_ForwardDeclareFrameType*/struct F4096x8p1__F1_Frame_t;typedef struct F4096x8p1__F1_Frame_t F4096x8p1__F1_Frame_t;
 /* internal_declare_param */
m3_CHAR
__cdecl
F4096x8p1__F1(
   /* Param_Type1 */ T5CDD996C* /*TypeText1*/  t_L_27);
 /* import_procedure */
/*Proc_ForwardDeclareFrameType*/struct F4096x8p2__F1_Frame_t;typedef struct F4096x8p2__F1_Frame_t F4096x8p2__F1_Frame_t;
 /* internal_declare_param */
m3_CHAR
__cdecl
F4096x8p2__F1(
   /* Param_Type1 */ TC4134582* /*TypeText1*/  t_L_28);
 /* import_procedure */
/*Proc_ForwardDeclareFrameType*/struct RTParams__IsPresent_Frame_t;typedef struct RTParams__IsPresent_Frame_t RTParams__IsPresent_Frame_t;
 /* internal_declare_param */
BOOLEAN
__cdecl
RTParams__IsPresent(
   /* Param_Type1 */ TEXT n_L_29);
 /* import_procedure */
/*Proc_ForwardDeclareFrameType*/struct RTHooks__TextLitInfo_Frame_t;typedef struct RTHooks__TextLitInfo_Frame_t RTHooks__TextLitInfo_Frame_t;
 /* internal_declare_param */
 /* internal_declare_param */
void /*TypeText3*/ 
__cdecl
RTHooks__TextLitInfo(
   /* Param_Type1 */ RTHooks__TextLiteral t_L_30,
   /* Param_Type1 */ RTHooks__TextInfo* /*TypeText1*/  i_L_31);
 /* import_procedure */
/*Proc_ForwardDeclareFrameType*/struct RTHooks__TextLitGetChar_Frame_t;typedef struct RTHooks__TextLitGetChar_Frame_t RTHooks__TextLitGetChar_Frame_t;
 /* internal_declare_param */
 /* internal_declare_param */
m3_CHAR
__cdecl
RTHooks__TextLitGetChar(
   /* Param_Type1 */ RTHooks__TextLiteral t_L_32,
   /* Param_Type1 */ CARDINAL i_L_33);
 /* import_procedure */
/*Proc_ForwardDeclareFrameType*/struct RTHooks__TextLitGetWideChar_Frame_t;typedef struct RTHooks__TextLitGetWideChar_Frame_t RTHooks__TextLitGetWideChar_Frame_t;
 /* internal_declare_param */
 /* internal_declare_param */
WIDECHAR
__cdecl
RTHooks__TextLitGetWideChar(
   /* Param_Type1 */ RTHooks__TextLiteral t_L_34,
   /* Param_Type1 */ CARDINAL i_L_35);
 /* import_procedure */
/*Proc_ForwardDeclareFrameType*/struct RTHooks__TextLitGetChars_Frame_t;typedef struct RTHooks__TextLitGetChars_Frame_t RTHooks__TextLitGetChars_Frame_t;
 /* internal_declare_param */
 /* internal_declare_param */
 /* internal_declare_param */
void /*TypeText3*/ 
__cdecl
RTHooks__TextLitGetChars(
   /* Param_Type1 */ RTHooks__TextLiteral t_L_36,
   /* Param_Type1 */ T89CD34BD* /*TypeText1*/  a_L_37,
   /* Param_Type1 */ CARDINAL start_L_38);
 /* import_procedure */
/*Proc_ForwardDeclareFrameType*/struct RTHooks__TextLitGetWideChars_Frame_t;typedef struct RTHooks__TextLitGetWideChars_Frame_t RTHooks__TextLitGetWideChars_Frame_t;
 /* internal_declare_param */
 /* internal_declare_param */
 /* internal_declare_param */
void /*TypeText3*/ 
__cdecl
RTHooks__TextLitGetWideChars(
   /* Param_Type1 */ RTHooks__TextLiteral t_L_39,
   /* Param_Type1 */ TA19BDC21* /*TypeText1*/  a_L_40,
   /* Param_Type1 */ CARDINAL start_L_41);
 /* end: imports */
 /* begin: locals */
 /* declare_segment name:<NIL> typeid:TFFFFFFFF const:TRUE */
/*declare_segment*/struct Main_m_42_L_43_t;
/*declare_segment*/typedef struct Main_m_42_L_43_t Main_m_42_L_43_t;
 /* declare_segment name:M_Main typeid:TFFFFFFFF const:FALSE */
 /* handler_name_prefixes:Main_M3_LINE_ */
 /* handler_name_prefixes:Main_I3_LINE_ */
/*declare_segment*/struct Main_m_M_Main_L_44_t;
/*declare_segment*/typedef struct Main_m_M_Main_L_44_t Main_m_M_Main_L_44_t;
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct Main_M3_Frame_t;typedef struct Main_M3_Frame_t Main_M3_Frame_t;
 /* internal_declare_param */
RT0__ModulePtr
__cdecl
Main_M3(
   /* Param_Type1 */ INTEGER /*TypeText1*/  mode_L_45);
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct Main__F3_Frame_t;typedef struct Main__F3_Frame_t Main__F3_Frame_t;
void /*TypeText3*/ 
__cdecl
Main__F3(void);
 /* Locals_begin_procedure */
 /* begin_block */
 /* declare_temp */
 /* declare_local */
 /* declare_temp */
 /* declare_local */
 /* Locals_end_procedure */
 /* end_block */
 /* Locals_begin_procedure */
 /* begin_block */
 /* declare_temp */
 /* declare_local */
 /* Locals_end_procedure */
 /* end_block */
 /* end: locals */
 /* begin: segments/globals */
 /* bind_segment */
 /* begin_init */
 /* init_proc */
 /* init_proc */
 /* init_proc */
 /* init_proc */
 /* init_proc */
 /* init_int */
 /* init_var */
 /* init_int */
 /* init_chars */
 /* init_int */
 /* init_var */
 /* init_int */
 /* init_chars */
 /* init_chars */
 /* init_chars */
 /* init_proc */
 /* init_var */
 /* init_proc */
 /* init_var */
 /* init_chars */
 /* end_init */
struct Main_m_42_L_43_t{ADDRESS L_52[5];
INT64 L_53[1];
ADDRESS L_54[1];
INT64 L_55[1];
UINT8 L_56[7];
char L_57[1];
INT64 L_58[1];
ADDRESS L_59[1];
INT64 L_60[1];
UINT8 L_61[9];
char L_62[7];
UINT8 L_63[7];
char L_64[1];
UINT8 L_65[2];
char L_66[6];
ADDRESS L_67[4];
char L_68[8];
UINT8 L_69[10];
char L_70[14];
};
static  const Main_m_42_L_43_t Main_m_42_L_43={{(ADDRESS)&RTHooks__TextLitInfo,(ADDRESS)&RTHooks__TextLitGetChar,(ADDRESS)&RTHooks__TextLitGetWideChar,(ADDRESS)&RTHooks__TextLitGetChars,(ADDRESS)&RTHooks__TextLitGetWideChars},{INT64_(2)},{(char*)&Main_m_42_L_43},{INT64_(7)},{'c','h','e','c','k','e','d'},{0 /* 1 */ ,},{INT64_(2)},{(char*)&Main_m_42_L_43},{INT64_(9)},{'u','n','c','h','e','c','k','e','d'},{0 /* 1 */ ,0 /* 2 */ ,0 /* 3 */ ,0 /* 4 */ ,0 /* 5 */ ,0 /* 6 */ ,0 /* 7 */ ,},{'M','a','i','n','_','M','3'},{0 /* 1 */ ,},{'F','3'},{0 /* 1 */ ,0 /* 2 */ ,0 /* 3 */ ,0 /* 4 */ ,0 /* 5 */ ,0 /* 6 */ ,},{(ADDRESS)&Main_M3,112+(char*)&Main_m_42_L_43,(ADDRESS)&Main__F3,120+(char*)&Main_m_42_L_43},{0 /* 1 */ ,0 /* 2 */ ,0 /* 3 */ ,0 /* 4 */ ,0 /* 5 */ ,0 /* 6 */ ,0 /* 7 */ ,0 /* 8 */ ,},{'.','.','/','M','a','i','n','.','m','3'},{0 /* 1 */ ,0 /* 2 */ ,0 /* 3 */ ,0 /* 4 */ ,0 /* 5 */ ,0 /* 6 */ ,0 /* 7 */ ,0 /* 8 */ ,0 /* 9 */ ,0 /* 10 */ ,0 /* 11 */ ,0 /* 12 */ ,0 /* 13 */ ,0 /* 14 */ ,}};
 /* bind_segment */
 /* begin_init */
 /* init_var */
 /* init_var */
 /* init_var */
 /* init_var */
 /* init_proc */
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
 /* init_var */
 /* init_proc */
 /* init_var */
 /* init_proc */
 /* init_var */
 /* init_proc */
 /* init_var */
 /* init_proc */
 /* init_var */
 /* init_proc */
 /* init_var */
 /* init_proc */
 /* init_var */
 /* init_proc */
 /* init_var */
 /* init_proc */
 /* init_var */
 /* init_proc */
 /* init_var */
 /* init_proc */
 /* init_var */
 /* init_int */
 /* init_var */
 /* init_int */
 /* init_var */
 /* init_int */
 /* init_var */
 /* init_int */
 /* init_var */
 /* init_int */
 /* init_var */
 /* init_int */
 /* init_var */
 /* init_int */
 /* init_var */
 /* init_int */
 /* init_var */
 /* init_int */
 /* init_var */
 /* init_int */
 /* init_var */
 /* init_int */
 /* init_int */
 /* end_init */
struct Main_m_M_Main_L_44_t{ADDRESS L_71[1];
char L_72[8];
ADDRESS L_73[1];
char L_74[16];
ADDRESS L_75[1];
char L_76[24];
ADDRESS L_77[1];
char L_78[8];
ADDRESS L_79[1];
INT64 L_80[1];
char L_81[8];
ADDRESS L_82[2];
char L_83[8];
ADDRESS L_84[2];
char L_85[8];
ADDRESS L_86[2];
char L_87[8];
ADDRESS L_88[2];
char L_89[8];
ADDRESS L_90[2];
char L_91[8];
ADDRESS L_92[2];
char L_93[8];
ADDRESS L_94[2];
char L_95[8];
ADDRESS L_96[2];
char L_97[8];
ADDRESS L_98[2];
char L_99[8];
ADDRESS L_100[2];
char L_101[8];
ADDRESS L_102[2];
char L_103[8];
ADDRESS L_104[2];
char L_105[8];
ADDRESS L_106[2];
char L_107[8];
ADDRESS L_108[2];
char L_109[8];
ADDRESS L_110[1];
char L_111[8];
ADDRESS L_112[1];
INT64 L_113[1];
ADDRESS L_114[1];
INT64 L_115[1];
ADDRESS L_116[1];
INT64 L_117[1];
ADDRESS L_118[1];
INT64 L_119[1];
ADDRESS L_120[1];
INT64 L_121[1];
ADDRESS L_122[1];
INT64 L_123[1];
ADDRESS L_124[1];
INT64 L_125[1];
ADDRESS L_126[1];
INT64 L_127[1];
ADDRESS L_128[1];
INT64 L_129[1];
ADDRESS L_130[1];
INT64 L_131[1];
ADDRESS L_132[1];
INT64 L_133[1];
char L_134[8];
INT64 L_135[1];
};
static Main_m_M_Main_L_44_t Main_m_M_Main_L_44={{168+(char*)&Main_m_42_L_43},{0 /* 1 */ ,0 /* 2 */ ,0 /* 3 */ ,0 /* 4 */ ,0 /* 5 */ ,0 /* 6 */ ,0 /* 7 */ ,0 /* 8 */ ,},{464+(char*)&Main_m_M_Main_L_44},{0 /* 1 */ ,0 /* 2 */ ,0 /* 3 */ ,0 /* 4 */ ,0 /* 5 */ ,0 /* 6 */ ,0 /* 7 */ ,0 /* 8 */ ,0 /* 9 */ ,0 /* 10 */ ,0 /* 11 */ ,0 /* 12 */ ,0 /* 13 */ ,0 /* 14 */ ,0 /* 15 */ ,0 /* 16 */ ,},{128+(char*)&Main_m_42_L_43},{0 /* 1 */ ,0 /* 2 */ ,0 /* 3 */ ,0 /* 4 */ ,0 /* 5 */ ,0 /* 6 */ ,0 /* 7 */ ,0 /* 8 */ ,0 /* 9 */ ,0 /* 10 */ ,0 /* 11 */ ,0 /* 12 */ ,0 /* 13 */ ,0 /* 14 */ ,0 /* 15 */ ,0 /* 16 */ ,0 /* 17 */ ,0 /* 18 */ ,0 /* 19 */ ,0 /* 20 */ ,0 /* 21 */ ,0 /* 22 */ ,0 /* 23 */ ,0 /* 24 */ ,},{104+(char*)&Main_m_M_Main_L_44},{0 /* 1 */ ,0 /* 2 */ ,0 /* 3 */ ,0 /* 4 */ ,0 /* 5 */ ,0 /* 6 */ ,0 /* 7 */ ,0 /* 8 */ ,},{(ADDRESS)&Main_M3},{INT64_(3)},{0 /* 1 */ ,0 /* 2 */ ,0 /* 3 */ ,0 /* 4 */ ,0 /* 5 */ ,0 /* 6 */ ,0 /* 7 */ ,0 /* 8 */ ,},{(ADDRESS)&Main_I3,128+(char*)&Main_m_M_Main_L_44},{0 /* 1 */ 
,0 /* 2 */ ,0 /* 3 */ ,0 /* 4 */ ,0 /* 5 */ ,0 /* 6 */ ,0 /* 7 */ ,0 /* 8 */ ,},{(ADDRESS)&RTParams_I3,152+(char*)&Main_m_M_Main_L_44},{0 /* 1 */ ,0 /* 2 */ ,0 /* 3 */ ,0 /* 4 */ ,0 /* 5 */ ,0 /* 6 */ ,0 /* 7 */ ,0 /* 8 */ ,},{(ADDRESS)&F4096x8m2_I3,176+(char*)&Main_m_M_Main_L_44},{0 /* 1 */ ,0 /* 2 */ ,0 /* 3 */ ,0 /* 4 */ ,0 /* 5 */ ,0 /* 6 */ ,0 /* 7 */ ,0 /* 8 */ ,},{(ADDRESS)&F4096x8m1_I3,200+(char*)&Main_m_M_Main_L_44},{0 /* 1 */ ,0 /* 2 */ ,0 /* 3 */ ,0 /* 4 */ ,0 /* 5 */ ,0 /* 6 */ ,0 /* 7 */ ,0 /* 8 */ ,},{(ADDRESS)&F4096x8p2_I3,224+(char*)&Main_m_M_Main_L_44},{0 /* 1 */ ,0 /* 2 */ ,0 /* 3 */ ,0 /* 4 */ ,0 /* 5 */ ,0 /* 6 */ ,0 /* 7 */ ,0 /* 8 */ ,},{(ADDRESS)&F4096x8p1_I3,248+(char*)&Main_m_M_Main_L_44},{0 /* 1 */ ,0 /* 2 */ ,0 /* 3 */ ,0 /* 4 */ ,0 /* 5 */ ,0 /* 6 */ ,0 /* 7 */ ,0 /* 8 */ ,},{(ADDRESS)&F4096x8_I3,272+(char*)&Main_m_M_Main_L_44},{0 /* 1 */ ,0 /* 2 */ ,0 /* 3 */ ,0 /* 4 */ ,0 /* 5 */ ,0 /* 6 */ ,0 /* 7 */ ,0 /* 8 */ ,},{(ADDRESS)&F4097_I3,296+(char*)&Main_m_M_Main_L_44
},{0 /* 1 */ ,0 /* 2 */ ,0 /* 3 */ ,0 /* 4 */ ,0 /* 5 */ ,0 /* 6 */ ,0 /* 7 */ ,0 /* 8 */ ,},{(ADDRESS)&F4096_I3,320+(char*)&Main_m_M_Main_L_44},{0 /* 1 */ ,0 /* 2 */ ,0 /* 3 */ ,0 /* 4 */ ,0 /* 5 */ ,0 /* 6 */ ,0 /* 7 */ ,0 /* 8 */ ,},{(ADDRESS)&F4095_I3,344+(char*)&Main_m_M_Main_L_44},{0 /* 1 */ ,0 /* 2 */ ,0 /* 3 */ ,0 /* 4 */ ,0 /* 5 */ ,0 /* 6 */ ,0 /* 7 */ ,0 /* 8 */ ,},{(ADDRESS)&F4094_I3,368+(char*)&Main_m_M_Main_L_44},{0 /* 1 */ ,0 /* 2 */ ,0 /* 3 */ ,0 /* 4 */ ,0 /* 5 */ ,0 /* 6 */ ,0 /* 7 */ ,0 /* 8 */ ,},{(ADDRESS)&F2_I3,392+(char*)&Main_m_M_Main_L_44},{0 /* 1 */ ,0 /* 2 */ ,0 /* 3 */ ,0 /* 4 */ ,0 /* 5 */ ,0 /* 6 */ ,0 /* 7 */ ,0 /* 8 */ ,},{(ADDRESS)&F1_I3,416+(char*)&Main_m_M_Main_L_44},{0 /* 1 */ ,0 /* 2 */ ,0 /* 3 */ ,0 /* 4 */ ,0 /* 5 */ ,0 /* 6 */ ,0 /* 7 */ ,0 /* 8 */ ,},{(ADDRESS)&F0_I3,440+(char*)&Main_m_M_Main_L_44},{0 /* 1 */ ,0 /* 2 */ ,0 /* 3 */ ,0 /* 4 */ ,0 /* 5 */ ,0 /* 6 */ ,0 /* 7 */ ,0 /* 8 */ ,},{(ADDRESS)&RTHooks_I3},{0 /* 1 */ ,0 /* 2 */ ,0 /* 3 */ ,0 /* 4 */ 
,0 /* 5 */ ,0 /* 6 */ ,0 /* 7 */ ,0 /* 8 */ ,},{480+(char*)&Main_m_M_Main_L_44},{INT64_(-1729626106)},{496+(char*)&Main_m_M_Main_L_44},{INT64_(-2066005942)},{512+(char*)&Main_m_M_Main_L_44},{INT64_(-2076233071)},{528+(char*)&Main_m_M_Main_L_44},{INT64_(1608408759)},{544+(char*)&Main_m_M_Main_L_44},{INT64_(1139613435)},{560+(char*)&Main_m_M_Main_L_44},{INT64_(1124690976)},{576+(char*)&Main_m_M_Main_L_44},{INT64_(1597647980)},{592+(char*)&Main_m_M_Main_L_44},{INT64_(-2098750297)},{608+(char*)&Main_m_M_Main_L_44},{INT64_(-1630216981)},{624+(char*)&Main_m_M_Main_L_44},{INT64_(-470639269)},{640+(char*)&Main_m_M_Main_L_44},{INT64_(-4072169)},{0 /* 1 */ ,0 /* 2 */ ,0 /* 3 */ ,0 /* 4 */ ,0 /* 5 */ ,0 /* 6 */ ,0 /* 7 */ ,0 /* 8 */ ,},{INT64_(-491926834)}};
 /* end: segments/globals */
 /* begin: mark used */
 /* end: mark used */
 /* set_source_file */
 /* set_source_line */
#line 3 "../Main.m3"
 /* module global constants */
#line 3 "../Main.m3"
 /* module global data */
#line 3 "../Main.m3"
 /* set_source_line */
#line 3 "../Main.m3"
#line 30 "../Main.m3"
 /* F3 */
#line 30 "../Main.m3"
 /* set_source_line */
#line 30 "../Main.m3"
#line 10 "../Main.m3"
 /* begin_procedure */
#line 10 "../Main.m3"
struct Main__F3_Frame_t {
#line 10 "../Main.m3"
ADDRESS _unused;
#line 10 "../Main.m3"
};
#line 10 "../Main.m3"
void /*TypeText3*/ 
__cdecl
Main__F3(void)
{
#line 10 "../Main.m3"
 /* Var_Type2 */ ADDRESS Main_m_46_L_47={0};//always-init
#line 10 "../Main.m3"
 /* Var_Type2 */ INT64 Main_m_48_L_49={0};//always-init
#line 10 "../Main.m3"
Main__F3_Frame_t _frame;
#line 10 "../Main.m3"
_frame._unused=(ADDRESS)&_frame;
#line 10 "../Main.m3"
 /* set_source_line */
#line 10 "../Main.m3"
#line 11 "../Main.m3"
 /* set_source_line */
#line 11 "../Main.m3"
#line 12 "../Main.m3"
 /* start_call_direct */
#line 12 "../Main.m3"
 /* load */
#line 12 "../Main.m3"
 /* pop_param */
#line 12 "../Main.m3"
 /* call_direct */
#line 12 "../Main.m3"
 /* store */
#line 12 "../Main.m3"
(*(ADDRESS*)(&Main_m_46_L_47))=(ADDRESS)(((ADDRESS)(RTHooks__AllocateTracedRef(
  ( ADDRESS )(((ADDRESS)(*((ADDRESS*)(INT64_(464)+((ADDRESS)(&Main_m_M_Main_L_44)))))) )))));
#line 12 "../Main.m3"
 /* start_call_direct */
#line 12 "../Main.m3"
 /* load */
#line 12 "../Main.m3"
 /* pop_param */
#line 12 "../Main.m3"
 /* call_direct */
#line 12 "../Main.m3"
 /* store */
#line 12 "../Main.m3"
(*(INT64*)(&Main_m_48_L_49))=(INT64)(((INT64)(F0__F1(
  ( T6A6EC077* /*TypeText1*/  )(((ADDRESS)(Main_m_46_L_47)) )))));
#line 12 "../Main.m3"
 /* set_source_line */
#line 12 "../Main.m3"
#line 13 "../Main.m3"
 /* start_call_direct */
#line 13 "../Main.m3"
 /* load */
#line 13 "../Main.m3"
 /* pop_param */
#line 13 "../Main.m3"
 /* call_direct */
#line 13 "../Main.m3"
 /* store */
#line 13 "../Main.m3"
(*(ADDRESS*)(&Main_m_46_L_47))=(ADDRESS)(((ADDRESS)(RTHooks__AllocateTracedRef(
  ( ADDRESS )(((ADDRESS)(*((ADDRESS*)(INT64_(480)+((ADDRESS)(&Main_m_M_Main_L_44)))))) )))));
#line 13 "../Main.m3"
 /* start_call_direct */
#line 13 "../Main.m3"
 /* load */
#line 13 "../Main.m3"
 /* pop_param */
#line 13 "../Main.m3"
 /* call_direct */
#line 13 "../Main.m3"
 /* store */
#line 13 "../Main.m3"
(*(INT64*)(&Main_m_48_L_49))=(INT64)(((INT64)(F1__F1(
  ( TEDC13487* /*TypeText1*/  )(((ADDRESS)(Main_m_46_L_47)) )))));
#line 13 "../Main.m3"
 /* set_source_line */
#line 13 "../Main.m3"
#line 14 "../Main.m3"
 /* start_call_direct */
#line 14 "../Main.m3"
 /* load */
#line 14 "../Main.m3"
 /* pop_param */
#line 14 "../Main.m3"
 /* call_direct */
#line 14 "../Main.m3"
 /* store */
#line 14 "../Main.m3"
(*(ADDRESS*)(&Main_m_46_L_47))=(ADDRESS)(((ADDRESS)(RTHooks__AllocateTracedRef(
  ( ADDRESS )(((ADDRESS)(*((ADDRESS*)(INT64_(496)+((ADDRESS)(&Main_m_M_Main_L_44)))))) )))));
#line 14 "../Main.m3"
 /* start_call_direct */
#line 14 "../Main.m3"
 /* load */
#line 14 "../Main.m3"
 /* pop_param */
#line 14 "../Main.m3"
 /* call_direct */
#line 14 "../Main.m3"
 /* store */
#line 14 "../Main.m3"
(*(INT64*)(&Main_m_48_L_49))=(INT64)(((INT64)(F2__F1(
  ( T45C03711* /*TypeText1*/  )(((ADDRESS)(Main_m_46_L_47)) )))));
#line 14 "../Main.m3"
 /* set_source_line */
#line 14 "../Main.m3"
#line 16 "../Main.m3"
 /* start_call_direct */
#line 16 "../Main.m3"
 /* load */
#line 16 "../Main.m3"
 /* pop_param */
#line 16 "../Main.m3"
 /* call_direct */
#line 16 "../Main.m3"
 /* store */
#line 16 "../Main.m3"
(*(ADDRESS*)(&Main_m_46_L_47))=(ADDRESS)(((ADDRESS)(RTHooks__AllocateTracedRef(
  ( ADDRESS )(((ADDRESS)(*((ADDRESS*)(INT64_(512)+((ADDRESS)(&Main_m_M_Main_L_44)))))) )))));
#line 16 "../Main.m3"
 /* start_call_direct */
#line 16 "../Main.m3"
 /* load */
#line 16 "../Main.m3"
 /* pop_param */
#line 16 "../Main.m3"
 /* call_direct */
#line 16 "../Main.m3"
 /* store */
#line 16 "../Main.m3"
(*(INT64*)(&Main_m_48_L_49))=(INT64)(((INT64)(F4094__F1(
  ( T1DDF1C23* /*TypeText1*/  )(((ADDRESS)(Main_m_46_L_47)) )))));
#line 16 "../Main.m3"
 /* set_source_line */
#line 16 "../Main.m3"
#line 17 "../Main.m3"
 /* start_call_direct */
#line 17 "../Main.m3"
 /* load */
#line 17 "../Main.m3"
 /* pop_param */
#line 17 "../Main.m3"
 /* call_direct */
#line 17 "../Main.m3"
 /* store */
#line 17 "../Main.m3"
(*(ADDRESS*)(&Main_m_46_L_47))=(ADDRESS)(((ADDRESS)(RTHooks__AllocateTracedRef(
  ( ADDRESS )(((ADDRESS)(*((ADDRESS*)(INT64_(528)+((ADDRESS)(&Main_m_M_Main_L_44)))))) )))));
#line 17 "../Main.m3"
 /* start_call_direct */
#line 17 "../Main.m3"
 /* load */
#line 17 "../Main.m3"
 /* pop_param */
#line 17 "../Main.m3"
 /* call_direct */
#line 17 "../Main.m3"
 /* store */
#line 17 "../Main.m3"
(*(INT64*)(&Main_m_48_L_49))=(INT64)(((INT64)(F4095__F1(
  ( T9A70E8D3* /*TypeText1*/  )(((ADDRESS)(Main_m_46_L_47)) )))));
#line 17 "../Main.m3"
 /* set_source_line */
#line 17 "../Main.m3"
#line 18 "../Main.m3"
 /* start_call_direct */
#line 18 "../Main.m3"
 /* load */
#line 18 "../Main.m3"
 /* pop_param */
#line 18 "../Main.m3"
 /* call_direct */
#line 18 "../Main.m3"
 /* store */
#line 18 "../Main.m3"
(*(ADDRESS*)(&Main_m_46_L_47))=(ADDRESS)(((ADDRESS)(RTHooks__AllocateTracedRef(
  ( ADDRESS )(((ADDRESS)(*((ADDRESS*)(INT64_(544)+((ADDRESS)(&Main_m_M_Main_L_44)))))) )))));
#line 18 "../Main.m3"
 /* start_call_direct */
#line 18 "../Main.m3"
 /* load */
#line 18 "../Main.m3"
 /* pop_param */
#line 18 "../Main.m3"
 /* call_direct */
#line 18 "../Main.m3"
 /* store */
#line 18 "../Main.m3"
(*(INT64*)(&Main_m_48_L_49))=(INT64)(((INT64)(F4096__F1(
  ( T3271EB45* /*TypeText1*/  )(((ADDRESS)(Main_m_46_L_47)) )))));
#line 18 "../Main.m3"
 /* set_source_line */
#line 18 "../Main.m3"
#line 19 "../Main.m3"
 /* start_call_direct */
#line 19 "../Main.m3"
 /* load */
#line 19 "../Main.m3"
 /* pop_param */
#line 19 "../Main.m3"
 /* call_direct */
#line 19 "../Main.m3"
 /* store */
#line 19 "../Main.m3"
(*(ADDRESS*)(&Main_m_46_L_47))=(ADDRESS)(((ADDRESS)(RTHooks__AllocateTracedRef(
  ( ADDRESS )(((ADDRESS)(*((ADDRESS*)(INT64_(560)+((ADDRESS)(&Main_m_M_Main_L_44)))))) )))));
#line 19 "../Main.m3"
 /* start_call_direct */
#line 19 "../Main.m3"
 /* load */
#line 19 "../Main.m3"
 /* pop_param */
#line 19 "../Main.m3"
 /* call_direct */
#line 19 "../Main.m3"
 /* store */
#line 19 "../Main.m3"
(*(INT64*)(&Main_m_48_L_49))=(INT64)(((INT64)(F4097__F1(
  ( TB5DE1FB5* /*TypeText1*/  )(((ADDRESS)(Main_m_46_L_47)) )))));
#line 19 "../Main.m3"
 /* set_source_line */
#line 19 "../Main.m3"
#line 23 "../Main.m3"
 /* start_call_direct */
#line 23 "../Main.m3"
 /* load */
#line 23 "../Main.m3"
 /* pop_param */
#line 23 "../Main.m3"
 /* call_direct */
#line 23 "../Main.m3"
 /* store */
#line 23 "../Main.m3"
(*(ADDRESS*)(&Main_m_46_L_47))=(ADDRESS)(((ADDRESS)(RTHooks__AllocateTracedRef(
  ( ADDRESS )(((ADDRESS)(*((ADDRESS*)(INT64_(576)+((ADDRESS)(&Main_m_M_Main_L_44)))))) )))));
#line 23 "../Main.m3"
 /* start_call_direct */
#line 23 "../Main.m3"
 /* load */
#line 23 "../Main.m3"
 /* pop_param */
#line 23 "../Main.m3"
 /* call_direct */
#line 23 "../Main.m3"
 /* store */
#line 23 "../Main.m3"
(*(INT64*)(&Main_m_48_L_49))=(INT64)(((INT64)(F4096x8m2__F1(
  ( TB08DBACF* /*TypeText1*/  )(((ADDRESS)(Main_m_46_L_47)) )))));
#line 23 "../Main.m3"
 /* set_source_line */
#line 23 "../Main.m3"
#line 24 "../Main.m3"
 /* start_call_direct */
#line 24 "../Main.m3"
 /* load */
#line 24 "../Main.m3"
 /* pop_param */
#line 24 "../Main.m3"
 /* call_direct */
#line 24 "../Main.m3"
 /* store */
#line 24 "../Main.m3"
(*(ADDRESS*)(&Main_m_46_L_47))=(ADDRESS)(((ADDRESS)(RTHooks__AllocateTracedRef(
  ( ADDRESS )(((ADDRESS)(*((ADDRESS*)(INT64_(592)+((ADDRESS)(&Main_m_M_Main_L_44)))))) )))));
#line 24 "../Main.m3"
 /* start_call_direct */
#line 24 "../Main.m3"
 /* load */
#line 24 "../Main.m3"
 /* pop_param */
#line 24 "../Main.m3"
 /* call_direct */
#line 24 "../Main.m3"
 /* store */
#line 24 "../Main.m3"
(*(INT64*)(&Main_m_48_L_49))=(INT64)(((INT64)(F4096x8m1__F1(
  ( T37224E3F* /*TypeText1*/  )(((ADDRESS)(Main_m_46_L_47)) )))));
#line 24 "../Main.m3"
 /* set_source_line */
#line 24 "../Main.m3"
#line 25 "../Main.m3"
 /* start_call_direct */
#line 25 "../Main.m3"
 /* load */
#line 25 "../Main.m3"
 /* pop_param */
#line 25 "../Main.m3"
 /* call_direct */
#line 25 "../Main.m3"
 /* store */
#line 25 "../Main.m3"
(*(ADDRESS*)(&Main_m_46_L_47))=(ADDRESS)(((ADDRESS)(RTHooks__AllocateTracedRef(
  ( ADDRESS )(((ADDRESS)(*((ADDRESS*)(INT64_(608)+((ADDRESS)(&Main_m_M_Main_L_44)))))) )))));
#line 25 "../Main.m3"
 /* start_call_direct */
#line 25 "../Main.m3"
 /* load */
#line 25 "../Main.m3"
 /* pop_param */
#line 25 "../Main.m3"
 /* call_direct */
#line 25 "../Main.m3"
 /* store */
#line 25 "../Main.m3"
(*(INT64*)(&Main_m_48_L_49))=(INT64)(((INT64)(F4096x8__F1(
  ( TDB726D9C* /*TypeText1*/  )(((ADDRESS)(Main_m_46_L_47)) )))));
#line 25 "../Main.m3"
 /* set_source_line */
#line 25 "../Main.m3"
#line 26 "../Main.m3"
 /* start_call_direct */
#line 26 "../Main.m3"
 /* load */
#line 26 "../Main.m3"
 /* pop_param */
#line 26 "../Main.m3"
 /* call_direct */
#line 26 "../Main.m3"
 /* store */
#line 26 "../Main.m3"
(*(ADDRESS*)(&Main_m_46_L_47))=(ADDRESS)(((ADDRESS)(RTHooks__AllocateTracedRef(
  ( ADDRESS )(((ADDRESS)(*((ADDRESS*)(INT64_(624)+((ADDRESS)(&Main_m_M_Main_L_44)))))) )))));
#line 26 "../Main.m3"
 /* start_call_direct */
#line 26 "../Main.m3"
 /* load */
#line 26 "../Main.m3"
 /* pop_param */
#line 26 "../Main.m3"
 /* call_direct */
#line 26 "../Main.m3"
 /* store */
#line 26 "../Main.m3"
(*(INT64*)(&Main_m_48_L_49))=(INT64)(((INT64)(F4096x8p1__F1(
  ( T5CDD996C* /*TypeText1*/  )(((ADDRESS)(Main_m_46_L_47)) )))));
#line 26 "../Main.m3"
 /* set_source_line */
#line 26 "../Main.m3"
#line 27 "../Main.m3"
 /* start_call_direct */
#line 27 "../Main.m3"
 /* load */
#line 27 "../Main.m3"
 /* pop_param */
#line 27 "../Main.m3"
 /* call_direct */
#line 27 "../Main.m3"
 /* store */
#line 27 "../Main.m3"
(*(ADDRESS*)(&Main_m_46_L_47))=(ADDRESS)(((ADDRESS)(RTHooks__AllocateTracedRef(
  ( ADDRESS )(((ADDRESS)(*((ADDRESS*)(INT64_(640)+((ADDRESS)(&Main_m_M_Main_L_44)))))) )))));
#line 27 "../Main.m3"
 /* start_call_direct */
#line 27 "../Main.m3"
 /* load */
#line 27 "../Main.m3"
 /* pop_param */
#line 27 "../Main.m3"
 /* call_direct */
#line 27 "../Main.m3"
 /* store */
#line 27 "../Main.m3"
(*(INT64*)(&Main_m_48_L_49))=(INT64)(((INT64)(F4096x8p2__F1(
  ( TC4134582* /*TypeText1*/  )(((ADDRESS)(Main_m_46_L_47)) )))));
#line 27 "../Main.m3"
 /* set_source_line */
#line 27 "../Main.m3"
#line 28 "../Main.m3"
 /* exit_proc */
#line 28 "../Main.m3"
return;
#line 28 "../Main.m3"
 /* end_procedure */
#line 28 "../Main.m3"
} /* Main_M3 */
#line 28 "../Main.m3"
 /* module main body Main_M3 */
#line 28 "../Main.m3"
 /* set_source_line */
#line 28 "../Main.m3"
#line 30 "../Main.m3"
 /* begin_procedure */
#line 30 "../Main.m3"
struct Main_M3_Frame_t {
#line 30 "../Main.m3"
ADDRESS _unused;
#line 30 "../Main.m3"
};
#line 30 "../Main.m3"
RT0__ModulePtr
__cdecl
Main_M3(
   /* Param_Type1 */ INTEGER /*TypeText1*/  mode_L_45)
{
#line 30 "../Main.m3"
 /* Var_Type2 */ INT64 Main_m_50_L_51={0};//always-init
#line 30 "../Main.m3"
Main_M3_Frame_t _frame;
#line 30 "../Main.m3"
_frame._unused=(ADDRESS)&_frame;
#line 30 "../Main.m3"
 /* load */
#line 30 "../Main.m3"
 /* if_true_or_false */
#line 30 "../Main.m3"
 /* load_host_integer */
#line 30 "../Main.m3"
 /* load_integer */
#line 30 "../Main.m3"
 /* if_compare */
#line 30 "../Main.m3"
if(m3_eq(INT64,
  mode_L_45,
   INT64_(0)))goto L1;
#line 30 "../Main.m3"
 /* set_source_line */
#line 30 "../Main.m3"
#line 31 "../Main.m3"
 /* start_call_direct */
#line 31 "../Main.m3"
 /* call_direct */
#line 31 "../Main.m3"
Main__F3(
 );
#line 31 "../Main.m3"
 /* set_source_line */
#line 31 "../Main.m3"
#line 32 "../Main.m3"
 /* start_call_direct */
#line 32 "../Main.m3"
 /* load_address */
#line 32 "../Main.m3"
 /* pop_param */
#line 32 "../Main.m3"
 /* call_direct */
#line 32 "../Main.m3"
 /* store */
#line 32 "../Main.m3"
(*(INT64*)(&Main_m_50_L_51))=(INT64)(((INT64)(RTParams__IsPresent(
  ( TEXT )(((ADDRESS)(INT64_(48)+((ADDRESS)(&Main_m_42_L_43)))) )))));
#line 32 "../Main.m3"
 /* load */
#line 32 "../Main.m3"
 /* if_true_or_false */
#line 32 "../Main.m3"
 /* load_host_integer */
#line 32 "../Main.m3"
 /* load_integer */
#line 32 "../Main.m3"
 /* if_compare */
#line 32 "../Main.m3"
if(m3_eq(INT64,
  Main_m_50_L_51,
   INT64_(0)))goto L3;
#line 32 "../Main.m3"
 /* set_source_line */
#line 32 "../Main.m3"
#line 33 "../Main.m3"
 /* start_call_direct */
#line 33 "../Main.m3"
 /* load_nil */
#line 33 "../Main.m3"
 /* pop_param */
#line 33 "../Main.m3"
 /* call_direct */
#line 33 "../Main.m3"
 /* store */
#line 33 "../Main.m3"
(*(INT64*)(&Main_m_50_L_51))=(INT64)(((INT64)(F4096x8p2__F1(
  ( TC4134582* /*TypeText1*/  )(((ADDRESS)(0)) )))));
#line 33 "../Main.m3"
 /* set_label */
#line 33 "../Main.m3"
L3:;
#line 33 "../Main.m3"
 /* set_source_line */
#line 33 "../Main.m3"
#line 35 "../Main.m3"
 /* start_call_direct */
#line 35 "../Main.m3"
 /* load_address */
#line 35 "../Main.m3"
 /* pop_param */
#line 35 "../Main.m3"
 /* call_direct */
#line 35 "../Main.m3"
 /* store */
#line 35 "../Main.m3"
(*(INT64*)(&Main_m_50_L_51))=(INT64)(((INT64)(RTParams__IsPresent(
  ( TEXT )(((ADDRESS)(INT64_(80)+((ADDRESS)(&Main_m_42_L_43)))) )))));
#line 35 "../Main.m3"
 /* load */
#line 35 "../Main.m3"
 /* if_true_or_false */
#line 35 "../Main.m3"
 /* load_host_integer */
#line 35 "../Main.m3"
 /* load_integer */
#line 35 "../Main.m3"
 /* if_compare */
#line 35 "../Main.m3"
if(m3_eq(INT64,
  Main_m_50_L_51,
   INT64_(0)))goto L5;
#line 35 "../Main.m3"
 /* set_source_line */
#line 35 "../Main.m3"
#line 36 "../Main.m3"
 /* start_call_direct */
#line 36 "../Main.m3"
 /* load_nil */
#line 36 "../Main.m3"
 /* pop_param */
#line 36 "../Main.m3"
 /* call_direct */
#line 36 "../Main.m3"
 /* store */
#line 36 "../Main.m3"
(*(INT64*)(&Main_m_50_L_51))=(INT64)(((INT64)(F0__F1(
  ( T6A6EC077* /*TypeText1*/  )(((ADDRESS)(0)) )))));
#line 36 "../Main.m3"
 /* set_label */
#line 36 "../Main.m3"
L5:;
#line 36 "../Main.m3"
 /* set_label */
#line 36 "../Main.m3"
L1:;
#line 36 "../Main.m3"
 /* load_address */
#line 36 "../Main.m3"
 /* exit_proc */
#line 36 "../Main.m3"
return (RT0__ModulePtr)(&Main_m_M_Main_L_44);
#line 36 "../Main.m3"
 /* end_procedure */
#line 36 "../Main.m3"
} /* global constant type descriptor */
#line 36 "../Main.m3"
 /* global data type descriptor */
#line 36 "../Main.m3"
 /* module global constants */
#line 36 "../Main.m3"
 /* procedure names */
#line 36 "../Main.m3"
 /* procedure table */
#line 36 "../Main.m3"
 /* file name */
#line 36 "../Main.m3"
 /* module global data */
#line 36 "../Main.m3"
 /* load map


 global data allocation for M_Main
     0   104  8  *module info*
   104    24  8  import Main
   128    24  8  import RTParams
   152    24  8  import F4096x8m2
   176    24  8  import F4096x8m1
   200    24  8  import F4096x8p2
   224    24  8  import F4096x8p1
   248    24  8  import F4096x8
   272    24  8  import F4097
   296    24  8  import F4096
   320    24  8  import F4095
   344    24  8  import F4094
   368    24  8  import F2
   392    24  8  import F1
   416    24  8  import F0
   440    24  8  import RTHooks
   464    16  8  typecell ptr
   480    16  8  typecell ptr
   496    16  8  typecell ptr
   512    16  8  typecell ptr
   528    16  8  typecell ptr
   544    16  8  typecell ptr
   560    16  8  typecell ptr
   576    16  8  typecell ptr
   592    16  8  typecell ptr
   608    16  8  typecell ptr
   624    16  8  typecell ptr
   640    16  8  typecell ptr
   656     0  8  *TOTAL*


 global constants for M_Main
     0    40  8  TEXT literal methods
    40    32  8  *TEXT literal*
    72    34  8  *TEXT literal*
   112    11  8  *proc names*
   128    40  8  *proc info*
   168    11  1  *string*
   184     0  8  *TOTAL*
 */
#line 36 "../Main.m3"
 /* end unit */
#line 36 "../Main.m3"

#ifdef __cplusplus

} /* extern "C" */
#endif
 /* set_runtime_proc */
 /* set_runtime_proc */
 /* set_runtime_proc */
