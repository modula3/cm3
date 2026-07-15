// library:pgm
// source_base_name:F0
// target_name:F0.i3.cpp
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
typedef T6A6EC077*T98E80006;
 /* declare_proctype */
 /* declare_formal */
 /* declare_record */
 /* declare_record */
 /* DeclareTypes_FlushOnce size:1 */

#if 0 /* avoid type hash collions */
typedef 
UCHAR(__cdecl*T4A5F2BBD)(T6A6EC077*);
#else
typedef void (__cdecl*T4A5F2BBD)(void);
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
/*Proc_ForwardDeclareFrameType*/struct RTHooks_I3_Frame_t;typedef struct RTHooks_I3_Frame_t RTHooks_I3_Frame_t;
 /* internal_declare_param */
RT0__ModulePtr
__cdecl
RTHooks_I3(
   /* Param_Type1 */ INTEGER /*TypeText1*/  mode_L_0);
 /* import_procedure */
/*Proc_ForwardDeclareFrameType*/struct F0_M3_Frame_t;typedef struct F0_M3_Frame_t F0_M3_Frame_t;
 /* internal_declare_param */
RT0__ModulePtr
__cdecl
F0_M3(
   /* Param_Type1 */ INTEGER /*TypeText1*/  mode_L_1);
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
   /* Param_Type1 */ T6A6EC077* /*TypeText1*/  t_L_2);
 /* end: imports */
 /* begin: locals */
 /* declare_segment name:<NIL> typeid:TFFFFFFFF const:TRUE */
/*declare_segment*/struct F0_i_3_L_4_t;
/*declare_segment*/typedef struct F0_i_3_L_4_t F0_i_3_L_4_t;
 /* declare_segment name:I_F0 typeid:TFFFFFFFF const:FALSE */
 /* handler_name_prefixes:F0_M3_LINE_ */
 /* handler_name_prefixes:F0_I3_LINE_ */
/*declare_segment*/struct F0_i_I_F0_L_5_t;
/*declare_segment*/typedef struct F0_i_I_F0_L_5_t F0_i_I_F0_L_5_t;
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct F0_I3_Frame_t;typedef struct F0_I3_Frame_t F0_I3_Frame_t;
 /* internal_declare_param */
RT0__ModulePtr
__cdecl
F0_I3(
   /* Param_Type1 */ INTEGER /*TypeText1*/  mode_L_6);
 /* Locals_begin_procedure */
 /* begin_block */
 /* Locals_end_procedure */
 /* end_block */
 /* end: locals */
 /* begin: segments/globals */
 /* bind_segment */
 /* begin_init */
 /* init_chars */
 /* init_proc */
 /* init_var */
 /* init_chars */
 /* init_int */
 /* init_int */
 /* init_int */
 /* init_int */
 /* init_int */
 /* init_int */
 /* init_int */
 /* init_int */
 /* init_int */
 /* init_int */
 /* init_int */
 /* end_init */
struct F0_i_3_L_4_t{UINT8 L_7[5];
char L_8[3];
ADDRESS L_9[2];
char L_10[8];
UINT8 L_11[8];
char L_12[1];
INT8 L_13[11];
char L_14[12];
};
static  const F0_i_3_L_4_t F0_i_3_L_4={{'F','0','_','I','3'},{0 /* 1 */ ,0 /* 2 */ ,0 /* 3 */ ,},{(ADDRESS)&F0_I3,(char*)&F0_i_3_L_4},{0 /* 1 */ ,0 /* 2 */ ,0 /* 3 */ ,0 /* 4 */ ,0 /* 5 */ ,0 /* 6 */ ,0 /* 7 */ ,0 /* 8 */ ,},{'.','.','/','F','0','.','i','3'},{0 /* 1 */ ,},{((INT8)1),((INT8)16),((INT8)26),((INT8)1),((INT8)0),((INT8)3),((INT8)17),((INT8)1),((INT8)1),((INT8)1),((INT8)4)},{0 /* 1 */ ,0 /* 2 */ ,0 /* 3 */ ,0 /* 4 */ ,0 /* 5 */ ,0 /* 6 */ ,0 /* 7 */ ,0 /* 8 */ ,0 /* 9 */ ,0 /* 10 */ ,0 /* 11 */ ,0 /* 12 */ ,}};
 /* bind_segment */
 /* begin_init */
 /* init_var */
 /* init_var */
 /* init_var */
 /* init_var */
 /* init_proc */
 /* init_int */
 /* init_int */
 /* init_int */
 /* init_int */
 /* init_int */
 /* init_int */
 /* init_int */
 /* init_int */
 /* init_int */
 /* init_int */
 /* init_int */
 /* init_int */
 /* init_int */
 /* init_int */
 /* init_var */
 /* init_var */
 /* init_proc */
 /* init_var */
 /* init_proc */
 /* end_init */
struct F0_i_I_F0_L_5_t{ADDRESS L_15[2];
char L_16[24];
ADDRESS L_17[1];
char L_18[24];
ADDRESS L_19[1];
char L_20[8];
ADDRESS L_21[1];
INT64 L_22[1];
char L_23[8];
INT64 L_24[1];
INT8 L_25[1];
UINT8 L_26[1];
INT8 L_27[1];
UINT8 L_28[1];
INT8 L_29[1];
UINT8 L_30[2];
INT8 L_31[3];
char L_32[1];
INT8 L_33[1];
char L_34[4];
INT64 L_35[1];
ADDRESS L_36[1];
char L_37[8];
ADDRESS L_38[1];
char L_39[40];
ADDRESS L_40[2];
char L_41[8];
ADDRESS L_42[1];
char L_43[16];
};
static F0_i_I_F0_L_5_t F0_i_I_F0_L_5={{32+(char*)&F0_i_3_L_4,104+(char*)&F0_i_I_F0_L_5},{0 /* 1 */ ,0 /* 2 */ ,0 /* 3 */ ,0 /* 4 */ ,0 /* 5 */ ,0 /* 6 */ ,0 /* 7 */ ,0 /* 8 */ ,0 /* 9 */ ,0 /* 10 */ ,0 /* 11 */ ,0 /* 12 */ ,0 /* 13 */ ,0 /* 14 */ ,0 /* 15 */ ,0 /* 16 */ ,0 /* 17 */ ,0 /* 18 */ ,0 /* 19 */ ,0 /* 20 */ ,0 /* 21 */ ,0 /* 22 */ ,0 /* 23 */ ,0 /* 24 */ ,},{8+(char*)&F0_i_3_L_4},{0 /* 1 */ ,0 /* 2 */ ,0 /* 3 */ ,0 /* 4 */ ,0 /* 5 */ ,0 /* 6 */ ,0 /* 7 */ ,0 /* 8 */ ,0 /* 9 */ ,0 /* 10 */ ,0 /* 11 */ ,0 /* 12 */ ,0 /* 13 */ ,0 /* 14 */ ,0 /* 15 */ ,0 /* 16 */ ,0 /* 17 */ ,0 /* 18 */ ,0 /* 19 */ ,0 /* 20 */ ,0 /* 21 */ ,0 /* 22 */ ,0 /* 23 */ ,0 /* 24 */ ,},{200+(char*)&F0_i_I_F0_L_5},{0 /* 1 */ ,0 /* 2 */ ,0 /* 3 */ ,0 /* 4 */ ,0 /* 5 */ ,0 /* 6 */ ,0 /* 7 */ ,0 /* 8 */ ,},{(ADDRESS)&F0_I3},{INT64_(3)},{0 /* 1 */ ,0 /* 2 */ ,0 /* 3 */ ,0 /* 4 */ ,0 /* 5 */ ,0 /* 6 */ ,0 /* 7 */ ,0 /* 8 */ ,},{INT64_(-1729626106)},{((INT8)4)},{233U},{((INT8)21)},{141U},{((INT8)2)},{233U,253U},
{((INT8)21),((INT8)1),((INT8)1)},{0 /* 1 */ ,},{((INT8)1)},{0 /* 1 */ ,0 /* 2 */ ,0 /* 3 */ ,0 /* 4 */ ,},{INT64_(1)},{41+(char*)&F0_i_3_L_4},{0 /* 1 */ ,0 /* 2 */ ,0 /* 3 */ ,0 /* 4 */ ,0 /* 5 */ ,0 /* 6 */ ,0 /* 7 */ ,0 /* 8 */ ,},{46+(char*)&F0_i_3_L_4},{0 /* 1 */ ,0 /* 2 */ ,0 /* 3 */ ,0 /* 4 */ ,0 /* 5 */ ,0 /* 6 */ ,0 /* 7 */ ,0 /* 8 */ ,0 /* 9 */ ,0 /* 10 */ ,0 /* 11 */ ,0 /* 12 */ ,0 /* 13 */ ,0 /* 14 */ ,0 /* 15 */ ,0 /* 16 */ ,0 /* 17 */ ,0 /* 18 */ ,0 /* 19 */ ,0 /* 20 */ ,0 /* 21 */ ,0 /* 22 */ ,0 /* 23 */ ,0 /* 24 */ ,0 /* 25 */ ,0 /* 26 */ ,0 /* 27 */ ,0 /* 28 */ ,0 /* 29 */ ,0 /* 30 */ ,0 /* 31 */ ,0 /* 32 */ ,0 /* 33 */ ,0 /* 34 */ ,0 /* 35 */ ,0 /* 36 */ ,0 /* 37 */ ,0 /* 38 */ ,0 /* 39 */ ,0 /* 40 */ ,},{(ADDRESS)&RTHooks_I3,224+(char*)&F0_i_I_F0_L_5},{0 /* 1 */ ,0 /* 2 */ ,0 /* 3 */ ,0 /* 4 */ ,0 /* 5 */ ,0 /* 6 */ ,0 /* 7 */ ,0 /* 8 */ ,},{(ADDRESS)&F0_M3},{0 /* 1 */ ,0 /* 2 */ ,0 /* 3 */ ,0 /* 4 */ ,0 /* 5 */ ,0 /* 6 */ ,0 /* 7 */ ,0 /* 8 */ ,0 /* 9 */ ,0 /* 10 */ 
,0 /* 11 */ ,0 /* 12 */ ,0 /* 13 */ ,0 /* 14 */ ,0 /* 15 */ ,0 /* 16 */ ,}};
 /* end: segments/globals */
 /* begin: mark used */
 /* end: mark used */
 /* set_source_file */
 /* set_source_line */
#line 3 "../F0.i3"
 /* module global constants */
#line 3 "../F0.i3"
 /* module global data */
#line 3 "../F0.i3"
 /* set_source_line */
#line 3 "../F0.i3"
#line 24 "../F0.i3"
 /* F0_I3 */
#line 24 "../F0.i3"
 /* module main body F0_I3 */
#line 24 "../F0.i3"
 /* begin_procedure */
#line 24 "../F0.i3"
struct F0_I3_Frame_t {
#line 24 "../F0.i3"
ADDRESS _unused;
#line 24 "../F0.i3"
};
#line 24 "../F0.i3"
RT0__ModulePtr
__cdecl
F0_I3(
   /* Param_Type1 */ INTEGER /*TypeText1*/  mode_L_6)
{
#line 24 "../F0.i3"
F0_I3_Frame_t _frame;
#line 24 "../F0.i3"
_frame._unused=(ADDRESS)&_frame;
#line 24 "../F0.i3"
 /* load */
#line 24 "../F0.i3"
 /* if_true_or_false */
#line 24 "../F0.i3"
 /* load_host_integer */
#line 24 "../F0.i3"
 /* load_integer */
#line 24 "../F0.i3"
 /* if_compare */
#line 24 "../F0.i3"
if(m3_eq(INT64,
  mode_L_6,
   INT64_(0)))goto L1;
#line 24 "../F0.i3"
 /* set_label */
#line 24 "../F0.i3"
L1:;
#line 24 "../F0.i3"
 /* load_address */
#line 24 "../F0.i3"
 /* exit_proc */
#line 24 "../F0.i3"
return (RT0__ModulePtr)(&F0_i_I_F0_L_5);
#line 24 "../F0.i3"
 /* end_procedure */
#line 24 "../F0.i3"
} /* global constant type descriptor */
#line 24 "../F0.i3"
 /* global data type descriptor */
#line 24 "../F0.i3"
 /* module global constants */
#line 24 "../F0.i3"
 /* procedure names */
#line 24 "../F0.i3"
 /* procedure table */
#line 24 "../F0.i3"
 /* file name */
#line 24 "../F0.i3"
 /* type map for _t98e80006 */
#line 24 "../F0.i3"
 /* type description for _t98e80006 */
#line 24 "../F0.i3"
 /* module global data */
#line 24 "../F0.i3"
 /* typecell for _t98e80006 */
#line 24 "../F0.i3"
 /* load map


 global data allocation for I_F0
     0   104  8  *module info*
   104    96  8  typecell
   200    24  8  import RTHooks
   224    24  8  import F0
   248     0  8  *TOTAL*


 global constants for I_F0
     0     6  8  *proc names*
     8    24  8  *proc info*
    32     9  1  *string*
    41     5  1  type_map
    46     6  1  type_desc
    56     0  8  *TOTAL*
 */
#line 24 "../F0.i3"
 /* end unit */
#line 24 "../F0.i3"

#ifdef __cplusplus

} /* extern "C" */
#endif
 /* set_runtime_proc */
 /* set_runtime_proc */
 /* set_runtime_proc */
