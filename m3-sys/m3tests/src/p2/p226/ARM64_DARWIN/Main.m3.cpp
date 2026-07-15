// library:pgm
// source_base_name:Main
// target_name:Main.m3.cpp
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
 /* declare_record */
 /* declare_field */
 /* record_forwardDeclare Record_t{ typeid:T4E105FDA text:NIL hash_text:T4E105FDA base_text:NIL state:0} */
/*record_forwardDeclare*/struct T4E105FDA;typedef struct T4E105FDA T4E105FDA;
 /* record_canBeDefined Record_t{ typeid:T4E105FDA text:NIL hash_text:T4E105FDA base_text:NIL state:0} */
 /* record_define Record_t{ typeid:T4E105FDA text:NIL hash_text:T4E105FDA base_text:NIL state:0} */

#ifndef T4E105FDA
#define T4E105FDA T4E105FDA
/*record_define*/struct T4E105FDA{
BOOLEAN rep;
};
#endif
 /* declare_record */
 /* declare_field */
 /* record_forwardDeclare Record_t{ typeid:T8FED4546 text:NIL hash_text:T8FED4546 base_text:NIL state:0} */
/*record_forwardDeclare*/struct T8FED4546;typedef struct T8FED4546 T8FED4546;
 /* record_canBeDefined Record_t{ typeid:T8FED4546 text:NIL hash_text:T8FED4546 base_text:NIL state:0} */
 /* record_define Record_t{ typeid:T8FED4546 text:NIL hash_text:T8FED4546 base_text:NIL state:0} */

#ifndef T8FED4546
#define T8FED4546 T8FED4546
/*record_define*/struct T8FED4546{
UCHAR rep;
};
#endif
 /* declare_record */
 /* declare_field */
 /* record_forwardDeclare Record_t{ typeid:TF2208220 text:NIL hash_text:TF2208220 base_text:NIL state:0} */
/*record_forwardDeclare*/struct TF2208220;typedef struct TF2208220 TF2208220;
 /* record_canBeDefined Record_t{ typeid:TF2208220 text:NIL hash_text:TF2208220 base_text:NIL state:0} */
 /* record_define Record_t{ typeid:TF2208220 text:NIL hash_text:TF2208220 base_text:NIL state:0} */

#ifndef TF2208220
#define TF2208220 TF2208220
/*record_define*/struct TF2208220{
INTEGER rep;
};
#endif
 /* declare_record */
 /* declare_field */
 /* record_forwardDeclare Record_t{ typeid:TA39142EE text:NIL hash_text:TA39142EE base_text:NIL state:0} */
/*record_forwardDeclare*/struct TA39142EE;typedef struct TA39142EE TA39142EE;
 /* record_canBeDefined Record_t{ typeid:TA39142EE text:NIL hash_text:TA39142EE base_text:NIL state:0} */
 /* record_define Record_t{ typeid:TA39142EE text:NIL hash_text:TA39142EE base_text:NIL state:0} */

#ifndef TA39142EE
#define TA39142EE TA39142EE
/*record_define*/struct TA39142EE{
INT64 rep;
};
#endif
 /* declare_record */
 /* declare_field */
 /* record_forwardDeclare Record_t{ typeid:TFCF62C53 text:NIL hash_text:TFCF62C53 base_text:NIL state:0} */
/*record_forwardDeclare*/struct TFCF62C53;typedef struct TFCF62C53 TFCF62C53;
 /* record_canBeDefined Record_t{ typeid:TFCF62C53 text:NIL hash_text:TFCF62C53 base_text:NIL state:0} */
 /* record_define Record_t{ typeid:TFCF62C53 text:NIL hash_text:TFCF62C53 base_text:NIL state:0} */

#ifndef TFCF62C53
#define TFCF62C53 TFCF62C53
/*record_define*/struct TFCF62C53{
REFANY rep;
};
#endif
 /* declare_record */
 /* declare_field */
 /* record_forwardDeclare Record_t{ typeid:TA7BBADDA text:NIL hash_text:TA7BBADDA base_text:NIL state:0} */
/*record_forwardDeclare*/struct TA7BBADDA;typedef struct TA7BBADDA TA7BBADDA;
 /* record_canBeDefined Record_t{ typeid:TA7BBADDA text:NIL hash_text:TA7BBADDA base_text:NIL state:0} */
 /* record_define Record_t{ typeid:TA7BBADDA text:NIL hash_text:TA7BBADDA base_text:NIL state:0} */

#ifndef TA7BBADDA
#define TA7BBADDA TA7BBADDA
/*record_define*/struct TA7BBADDA{
WIDECHAR rep;
};
#endif
 /* declare_record */
 /* declare_field */
 /* record_forwardDeclare Record_t{ typeid:TB17D7B27 text:NIL hash_text:TB17D7B27 base_text:NIL state:0} */
/*record_forwardDeclare*/struct TB17D7B27;typedef struct TB17D7B27 TB17D7B27;
 /* record_canBeDefined Record_t{ typeid:TB17D7B27 text:NIL hash_text:TB17D7B27 base_text:NIL state:0} */
 /* record_define Record_t{ typeid:TB17D7B27 text:NIL hash_text:TB17D7B27 base_text:NIL state:0} */

#ifndef TB17D7B27
#define TB17D7B27 TB17D7B27
/*record_define*/struct TB17D7B27{
ADDRESS rep;
};
#endif
 /* declare_proctype */

#if 0 /* avoid type hash collions */
typedef 
void(__cdecl*T7B78C34F)(void);
#else
typedef void (__cdecl*T7B78C34F)(void);
#endif
 /* declare_proctype */

#if 0 /* avoid type hash collions */
typedef 
BOOLEAN(__cdecl*T52AB93A7)(void);
#else
typedef void (__cdecl*T52AB93A7)(void);
#endif
 /* declare_pointer */
typedef INTEGER*T50C57D3A;
 /* declare_proctype */
 /* declare_formal */
 /* declare_proctype */
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
 /* declare_field */
 /* declare_field */
 /* declare_field */
 /* declare_field */
 /* declare_field */
 /* declare_field */
 /* DeclareTypes_FlushOnce size:2 */

#if 0 /* avoid type hash collions */
typedef 
void(__cdecl*T37E50548)(REFANY);
#else
typedef void (__cdecl*T37E50548)(void);
#endif

#if 0 /* avoid type hash collions */
typedef 
REFANY(__cdecl*T7CFE252F)(ADDRESS);
#else
typedef void (__cdecl*T7CFE252F)(void);
#endif
 /* DeclareTypes_FlushOnce size:0 */
 /* end: DeclareTypes */
 /* begin: helper functions */
#define m3_extract(T, value, offset, count) ((((T)(value))>>((WORD_T)(offset)))&~(((~(T)0))<<((WORD_T)(count))))
#ifndef m3_fence
#ifdef _MSC_VER
long __cdecl _InterlockedExchange(volatile long*, long);
#pragma instrinsic(_InterlockedExchange)
static volatile long m3_fence_var;
#define m3_fence() _InterlockedExchange(&m3_fence_var, 0)
#else
#define m3_fence m3_fence
static void __stdcall m3_fence(void){}
#endif
#endif
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
   /* Param_Type1 */ INTEGER /*TypeText1*/  mode_L_0);
 /* import_procedure */
/*Proc_ForwardDeclareFrameType*/struct WideChar_I3_Frame_t;typedef struct WideChar_I3_Frame_t WideChar_I3_Frame_t;
 /* internal_declare_param */
RT0__ModulePtr
__cdecl
WideChar_I3(
   /* Param_Type1 */ INTEGER /*TypeText1*/  mode_L_1);
 /* import_procedure */
/*Proc_ForwardDeclareFrameType*/struct Refany_I3_Frame_t;typedef struct Refany_I3_Frame_t Refany_I3_Frame_t;
 /* internal_declare_param */
RT0__ModulePtr
__cdecl
Refany_I3(
   /* Param_Type1 */ INTEGER /*TypeText1*/  mode_L_2);
 /* import_procedure */
/*Proc_ForwardDeclareFrameType*/struct Longint_I3_Frame_t;typedef struct Longint_I3_Frame_t Longint_I3_Frame_t;
 /* internal_declare_param */
RT0__ModulePtr
__cdecl
Longint_I3(
   /* Param_Type1 */ INTEGER /*TypeText1*/  mode_L_3);
 /* import_procedure */
/*Proc_ForwardDeclareFrameType*/struct Integer_I3_Frame_t;typedef struct Integer_I3_Frame_t Integer_I3_Frame_t;
 /* internal_declare_param */
RT0__ModulePtr
__cdecl
Integer_I3(
   /* Param_Type1 */ INTEGER /*TypeText1*/  mode_L_4);
 /* import_procedure */
/*Proc_ForwardDeclareFrameType*/struct Char_I3_Frame_t;typedef struct Char_I3_Frame_t Char_I3_Frame_t;
 /* internal_declare_param */
RT0__ModulePtr
__cdecl
Char_I3(
   /* Param_Type1 */ INTEGER /*TypeText1*/  mode_L_5);
 /* import_procedure */
/*Proc_ForwardDeclareFrameType*/struct Boolean_I3_Frame_t;typedef struct Boolean_I3_Frame_t Boolean_I3_Frame_t;
 /* internal_declare_param */
RT0__ModulePtr
__cdecl
Boolean_I3(
   /* Param_Type1 */ INTEGER /*TypeText1*/  mode_L_6);
 /* import_procedure */
/*Proc_ForwardDeclareFrameType*/struct Address_I3_Frame_t;typedef struct Address_I3_Frame_t Address_I3_Frame_t;
 /* internal_declare_param */
RT0__ModulePtr
__cdecl
Address_I3(
   /* Param_Type1 */ INTEGER /*TypeText1*/  mode_L_7);
 /* import_procedure */
/*Proc_ForwardDeclareFrameType*/struct AtomicWideChar_I3_Frame_t;typedef struct AtomicWideChar_I3_Frame_t AtomicWideChar_I3_Frame_t;
 /* internal_declare_param */
RT0__ModulePtr
__cdecl
AtomicWideChar_I3(
   /* Param_Type1 */ INTEGER /*TypeText1*/  mode_L_8);
 /* import_procedure */
/*Proc_ForwardDeclareFrameType*/struct AtomicRefany_I3_Frame_t;typedef struct AtomicRefany_I3_Frame_t AtomicRefany_I3_Frame_t;
 /* internal_declare_param */
RT0__ModulePtr
__cdecl
AtomicRefany_I3(
   /* Param_Type1 */ INTEGER /*TypeText1*/  mode_L_9);
 /* import_procedure */
/*Proc_ForwardDeclareFrameType*/struct AtomicLongint_I3_Frame_t;typedef struct AtomicLongint_I3_Frame_t AtomicLongint_I3_Frame_t;
 /* internal_declare_param */
RT0__ModulePtr
__cdecl
AtomicLongint_I3(
   /* Param_Type1 */ INTEGER /*TypeText1*/  mode_L_10);
 /* import_procedure */
/*Proc_ForwardDeclareFrameType*/struct AtomicInteger_I3_Frame_t;typedef struct AtomicInteger_I3_Frame_t AtomicInteger_I3_Frame_t;
 /* internal_declare_param */
RT0__ModulePtr
__cdecl
AtomicInteger_I3(
   /* Param_Type1 */ INTEGER /*TypeText1*/  mode_L_11);
 /* import_procedure */
/*Proc_ForwardDeclareFrameType*/struct AtomicChar_I3_Frame_t;typedef struct AtomicChar_I3_Frame_t AtomicChar_I3_Frame_t;
 /* internal_declare_param */
RT0__ModulePtr
__cdecl
AtomicChar_I3(
   /* Param_Type1 */ INTEGER /*TypeText1*/  mode_L_12);
 /* import_procedure */
/*Proc_ForwardDeclareFrameType*/struct AtomicBoolean_I3_Frame_t;typedef struct AtomicBoolean_I3_Frame_t AtomicBoolean_I3_Frame_t;
 /* internal_declare_param */
RT0__ModulePtr
__cdecl
AtomicBoolean_I3(
   /* Param_Type1 */ INTEGER /*TypeText1*/  mode_L_13);
 /* import_procedure */
/*Proc_ForwardDeclareFrameType*/struct AtomicAddress_I3_Frame_t;typedef struct AtomicAddress_I3_Frame_t AtomicAddress_I3_Frame_t;
 /* internal_declare_param */
RT0__ModulePtr
__cdecl
AtomicAddress_I3(
   /* Param_Type1 */ INTEGER /*TypeText1*/  mode_L_14);
 /* import_procedure */
/*Proc_ForwardDeclareFrameType*/struct RTHooks_I3_Frame_t;typedef struct RTHooks_I3_Frame_t RTHooks_I3_Frame_t;
 /* internal_declare_param */
RT0__ModulePtr
__cdecl
RTHooks_I3(
   /* Param_Type1 */ INTEGER /*TypeText1*/  mode_L_15);
 /* import_procedure */
/*Proc_ForwardDeclareFrameType*/struct RTHooks__CheckLoadTracedRef_Frame_t;typedef struct RTHooks__CheckLoadTracedRef_Frame_t RTHooks__CheckLoadTracedRef_Frame_t;
 /* internal_declare_param */
void /*TypeText3*/ 
__cdecl
RTHooks__CheckLoadTracedRef(
   /* Param_Type1 */ REFANY ref_L_16);
 /* import_procedure */
/*Proc_ForwardDeclareFrameType*/struct RTHooks__AllocateTracedRef_Frame_t;typedef struct RTHooks__AllocateTracedRef_Frame_t RTHooks__AllocateTracedRef_Frame_t;
 /* internal_declare_param */
REFANY
__cdecl
RTHooks__AllocateTracedRef(
   /* Param_Type1 */ ADDRESS t_L_17);
 /* end: imports */
 /* begin: locals */
 /* declare_segment name:<NIL> typeid:TFFFFFFFF const:TRUE */
/*declare_segment*/struct Main_m_18_L_19_t;
/*declare_segment*/typedef struct Main_m_18_L_19_t Main_m_18_L_19_t;
 /* declare_segment name:M_Main typeid:TFFFFFFFF const:FALSE */
 /* handler_name_prefixes:Main_M3_LINE_ */
 /* handler_name_prefixes:Main_I3_LINE_ */
/*declare_segment*/struct Main_m_M_Main_L_20_t;
/*declare_segment*/typedef struct Main_m_M_Main_L_20_t Main_m_M_Main_L_20_t;
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct Main_M3_Frame_t;typedef struct Main_M3_Frame_t Main_M3_Frame_t;
 /* internal_declare_param */
RT0__ModulePtr
__cdecl
Main_M3(
   /* Param_Type1 */ INTEGER /*TypeText1*/  mode_L_21);
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct Main__Test_AtomicBoolean_Fence_Frame_t;typedef struct Main__Test_AtomicBoolean_Fence_Frame_t Main__Test_AtomicBoolean_Fence_Frame_t;
void /*TypeText3*/ 
__cdecl
Main__Test_AtomicBoolean_Fence(void);
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct Main__Test_AtomicBoolean_CompareSwap_Frame_t;typedef struct Main__Test_AtomicBoolean_CompareSwap_Frame_t Main__Test_AtomicBoolean_CompareSwap_Frame_t;
void /*TypeText3*/ 
__cdecl
Main__Test_AtomicBoolean_CompareSwap(void);
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct Main__Test_AtomicBoolean_FetchAnd_Frame_t;typedef struct Main__Test_AtomicBoolean_FetchAnd_Frame_t Main__Test_AtomicBoolean_FetchAnd_Frame_t;
void /*TypeText3*/ 
__cdecl
Main__Test_AtomicBoolean_FetchAnd(void);
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct Main__Test_AtomicBoolean_FetchDec_Frame_t;typedef struct Main__Test_AtomicBoolean_FetchDec_Frame_t Main__Test_AtomicBoolean_FetchDec_Frame_t;
void /*TypeText3*/ 
__cdecl
Main__Test_AtomicBoolean_FetchDec(void);
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct Main__Test_AtomicBoolean_FetchInc_Frame_t;typedef struct Main__Test_AtomicBoolean_FetchInc_Frame_t Main__Test_AtomicBoolean_FetchInc_Frame_t;
void /*TypeText3*/ 
__cdecl
Main__Test_AtomicBoolean_FetchInc(void);
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct Main__Test_AtomicBoolean_FetchOr_Frame_t;typedef struct Main__Test_AtomicBoolean_FetchOr_Frame_t Main__Test_AtomicBoolean_FetchOr_Frame_t;
void /*TypeText3*/ 
__cdecl
Main__Test_AtomicBoolean_FetchOr(void);
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct Main__Test_AtomicBoolean_FetchXor_Frame_t;typedef struct Main__Test_AtomicBoolean_FetchXor_Frame_t Main__Test_AtomicBoolean_FetchXor_Frame_t;
void /*TypeText3*/ 
__cdecl
Main__Test_AtomicBoolean_FetchXor(void);
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct Main__Test_AtomicBoolean_IsLockFree_Frame_t;typedef struct Main__Test_AtomicBoolean_IsLockFree_Frame_t Main__Test_AtomicBoolean_IsLockFree_Frame_t;
BOOLEAN
__cdecl
Main__Test_AtomicBoolean_IsLockFree(void);
 /* declare_local */
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct Main__Test_AtomicBoolean_LoadStore_Frame_t;typedef struct Main__Test_AtomicBoolean_LoadStore_Frame_t Main__Test_AtomicBoolean_LoadStore_Frame_t;
void /*TypeText3*/ 
__cdecl
Main__Test_AtomicBoolean_LoadStore(void);
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct Main__Test_AtomicBoolean_Swap_Frame_t;typedef struct Main__Test_AtomicBoolean_Swap_Frame_t Main__Test_AtomicBoolean_Swap_Frame_t;
void /*TypeText3*/ 
__cdecl
Main__Test_AtomicBoolean_Swap(void);
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct Main__Test_AtomicBoolean_Frame_t;typedef struct Main__Test_AtomicBoolean_Frame_t Main__Test_AtomicBoolean_Frame_t;
void /*TypeText3*/ 
__cdecl
Main__Test_AtomicBoolean(void);
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct Main__Test_AtomicChar_Fence_Frame_t;typedef struct Main__Test_AtomicChar_Fence_Frame_t Main__Test_AtomicChar_Fence_Frame_t;
void /*TypeText3*/ 
__cdecl
Main__Test_AtomicChar_Fence(void);
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct Main__Test_AtomicChar_CompareSwap_Frame_t;typedef struct Main__Test_AtomicChar_CompareSwap_Frame_t Main__Test_AtomicChar_CompareSwap_Frame_t;
void /*TypeText3*/ 
__cdecl
Main__Test_AtomicChar_CompareSwap(void);
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct Main__Test_AtomicChar_FetchAnd_Frame_t;typedef struct Main__Test_AtomicChar_FetchAnd_Frame_t Main__Test_AtomicChar_FetchAnd_Frame_t;
void /*TypeText3*/ 
__cdecl
Main__Test_AtomicChar_FetchAnd(void);
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct Main__Test_AtomicChar_FetchDec_Frame_t;typedef struct Main__Test_AtomicChar_FetchDec_Frame_t Main__Test_AtomicChar_FetchDec_Frame_t;
void /*TypeText3*/ 
__cdecl
Main__Test_AtomicChar_FetchDec(void);
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct Main__Test_AtomicChar_FetchInc_Frame_t;typedef struct Main__Test_AtomicChar_FetchInc_Frame_t Main__Test_AtomicChar_FetchInc_Frame_t;
void /*TypeText3*/ 
__cdecl
Main__Test_AtomicChar_FetchInc(void);
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct Main__Test_AtomicChar_FetchOr_Frame_t;typedef struct Main__Test_AtomicChar_FetchOr_Frame_t Main__Test_AtomicChar_FetchOr_Frame_t;
void /*TypeText3*/ 
__cdecl
Main__Test_AtomicChar_FetchOr(void);
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct Main__Test_AtomicChar_FetchXor_Frame_t;typedef struct Main__Test_AtomicChar_FetchXor_Frame_t Main__Test_AtomicChar_FetchXor_Frame_t;
void /*TypeText3*/ 
__cdecl
Main__Test_AtomicChar_FetchXor(void);
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct Main__Test_AtomicChar_IsLockFree_Frame_t;typedef struct Main__Test_AtomicChar_IsLockFree_Frame_t Main__Test_AtomicChar_IsLockFree_Frame_t;
BOOLEAN
__cdecl
Main__Test_AtomicChar_IsLockFree(void);
 /* declare_local */
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct Main__Test_AtomicChar_LoadStore_Frame_t;typedef struct Main__Test_AtomicChar_LoadStore_Frame_t Main__Test_AtomicChar_LoadStore_Frame_t;
void /*TypeText3*/ 
__cdecl
Main__Test_AtomicChar_LoadStore(void);
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct Main__Test_AtomicChar_Swap_Frame_t;typedef struct Main__Test_AtomicChar_Swap_Frame_t Main__Test_AtomicChar_Swap_Frame_t;
void /*TypeText3*/ 
__cdecl
Main__Test_AtomicChar_Swap(void);
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct Main__Test_AtomicChar_Frame_t;typedef struct Main__Test_AtomicChar_Frame_t Main__Test_AtomicChar_Frame_t;
void /*TypeText3*/ 
__cdecl
Main__Test_AtomicChar(void);
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct Main__Test_AtomicWidechar_Fence_Frame_t;typedef struct Main__Test_AtomicWidechar_Fence_Frame_t Main__Test_AtomicWidechar_Fence_Frame_t;
void /*TypeText3*/ 
__cdecl
Main__Test_AtomicWidechar_Fence(void);
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct Main__Test_AtomicWidechar_CompareSwap_Frame_t;typedef struct Main__Test_AtomicWidechar_CompareSwap_Frame_t Main__Test_AtomicWidechar_CompareSwap_Frame_t;
void /*TypeText3*/ 
__cdecl
Main__Test_AtomicWidechar_CompareSwap(void);
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct Main__Test_AtomicWidechar_FetchAnd_Frame_t;typedef struct Main__Test_AtomicWidechar_FetchAnd_Frame_t Main__Test_AtomicWidechar_FetchAnd_Frame_t;
void /*TypeText3*/ 
__cdecl
Main__Test_AtomicWidechar_FetchAnd(void);
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct Main__Test_AtomicWidechar_FetchDec_Frame_t;typedef struct Main__Test_AtomicWidechar_FetchDec_Frame_t Main__Test_AtomicWidechar_FetchDec_Frame_t;
void /*TypeText3*/ 
__cdecl
Main__Test_AtomicWidechar_FetchDec(void);
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct Main__Test_AtomicWidechar_FetchInc_Frame_t;typedef struct Main__Test_AtomicWidechar_FetchInc_Frame_t Main__Test_AtomicWidechar_FetchInc_Frame_t;
void /*TypeText3*/ 
__cdecl
Main__Test_AtomicWidechar_FetchInc(void);
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct Main__Test_AtomicWidechar_FetchOr_Frame_t;typedef struct Main__Test_AtomicWidechar_FetchOr_Frame_t Main__Test_AtomicWidechar_FetchOr_Frame_t;
void /*TypeText3*/ 
__cdecl
Main__Test_AtomicWidechar_FetchOr(void);
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct Main__Test_AtomicWidechar_FetchXor_Frame_t;typedef struct Main__Test_AtomicWidechar_FetchXor_Frame_t Main__Test_AtomicWidechar_FetchXor_Frame_t;
void /*TypeText3*/ 
__cdecl
Main__Test_AtomicWidechar_FetchXor(void);
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct Main__Test_AtomicWidechar_IsLockFree_Frame_t;typedef struct Main__Test_AtomicWidechar_IsLockFree_Frame_t Main__Test_AtomicWidechar_IsLockFree_Frame_t;
BOOLEAN
__cdecl
Main__Test_AtomicWidechar_IsLockFree(void);
 /* declare_local */
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct Main__Test_AtomicWidechar_LoadStore_Frame_t;typedef struct Main__Test_AtomicWidechar_LoadStore_Frame_t Main__Test_AtomicWidechar_LoadStore_Frame_t;
void /*TypeText3*/ 
__cdecl
Main__Test_AtomicWidechar_LoadStore(void);
 /* declare_local */
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct Main__Test_AtomicWidechar_Swap_Frame_t;typedef struct Main__Test_AtomicWidechar_Swap_Frame_t Main__Test_AtomicWidechar_Swap_Frame_t;
void /*TypeText3*/ 
__cdecl
Main__Test_AtomicWidechar_Swap(void);
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct Main__Test_AtomicWidechar_Frame_t;typedef struct Main__Test_AtomicWidechar_Frame_t Main__Test_AtomicWidechar_Frame_t;
void /*TypeText3*/ 
__cdecl
Main__Test_AtomicWidechar(void);
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct Main__Test_AtomicRefany_Fence_Frame_t;typedef struct Main__Test_AtomicRefany_Fence_Frame_t Main__Test_AtomicRefany_Fence_Frame_t;
void /*TypeText3*/ 
__cdecl
Main__Test_AtomicRefany_Fence(void);
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct Main__Test_AtomicRefany_CompareSwap_Frame_t;typedef struct Main__Test_AtomicRefany_CompareSwap_Frame_t Main__Test_AtomicRefany_CompareSwap_Frame_t;
void /*TypeText3*/ 
__cdecl
Main__Test_AtomicRefany_CompareSwap(void);
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct Main__Test_AtomicRefany_FetchAnd_Frame_t;typedef struct Main__Test_AtomicRefany_FetchAnd_Frame_t Main__Test_AtomicRefany_FetchAnd_Frame_t;
void /*TypeText3*/ 
__cdecl
Main__Test_AtomicRefany_FetchAnd(void);
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct Main__Test_AtomicRefany_FetchDec_Frame_t;typedef struct Main__Test_AtomicRefany_FetchDec_Frame_t Main__Test_AtomicRefany_FetchDec_Frame_t;
void /*TypeText3*/ 
__cdecl
Main__Test_AtomicRefany_FetchDec(void);
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct Main__Test_AtomicRefany_FetchInc_Frame_t;typedef struct Main__Test_AtomicRefany_FetchInc_Frame_t Main__Test_AtomicRefany_FetchInc_Frame_t;
void /*TypeText3*/ 
__cdecl
Main__Test_AtomicRefany_FetchInc(void);
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct Main__Test_AtomicRefany_FetchOr_Frame_t;typedef struct Main__Test_AtomicRefany_FetchOr_Frame_t Main__Test_AtomicRefany_FetchOr_Frame_t;
void /*TypeText3*/ 
__cdecl
Main__Test_AtomicRefany_FetchOr(void);
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct Main__Test_AtomicRefany_FetchXor_Frame_t;typedef struct Main__Test_AtomicRefany_FetchXor_Frame_t Main__Test_AtomicRefany_FetchXor_Frame_t;
void /*TypeText3*/ 
__cdecl
Main__Test_AtomicRefany_FetchXor(void);
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct Main__Test_AtomicRefany_IsLockFree_Frame_t;typedef struct Main__Test_AtomicRefany_IsLockFree_Frame_t Main__Test_AtomicRefany_IsLockFree_Frame_t;
BOOLEAN
__cdecl
Main__Test_AtomicRefany_IsLockFree(void);
 /* declare_local */
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct Main__Test_AtomicRefany_LoadStore_Frame_t;typedef struct Main__Test_AtomicRefany_LoadStore_Frame_t Main__Test_AtomicRefany_LoadStore_Frame_t;
void /*TypeText3*/ 
__cdecl
Main__Test_AtomicRefany_LoadStore(void);
 /* declare_local */
 /* declare_local */
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct Main__Test_AtomicRefany_Swap_Frame_t;typedef struct Main__Test_AtomicRefany_Swap_Frame_t Main__Test_AtomicRefany_Swap_Frame_t;
void /*TypeText3*/ 
__cdecl
Main__Test_AtomicRefany_Swap(void);
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct Main__Test_AtomicRefany_Frame_t;typedef struct Main__Test_AtomicRefany_Frame_t Main__Test_AtomicRefany_Frame_t;
void /*TypeText3*/ 
__cdecl
Main__Test_AtomicRefany(void);
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct Main__Test_AtomicAddress_Fence_Frame_t;typedef struct Main__Test_AtomicAddress_Fence_Frame_t Main__Test_AtomicAddress_Fence_Frame_t;
void /*TypeText3*/ 
__cdecl
Main__Test_AtomicAddress_Fence(void);
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct Main__Test_AtomicAddress_CompareSwap_Frame_t;typedef struct Main__Test_AtomicAddress_CompareSwap_Frame_t Main__Test_AtomicAddress_CompareSwap_Frame_t;
void /*TypeText3*/ 
__cdecl
Main__Test_AtomicAddress_CompareSwap(void);
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct Main__Test_AtomicAddress_FetchAnd_Frame_t;typedef struct Main__Test_AtomicAddress_FetchAnd_Frame_t Main__Test_AtomicAddress_FetchAnd_Frame_t;
void /*TypeText3*/ 
__cdecl
Main__Test_AtomicAddress_FetchAnd(void);
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct Main__Test_AtomicAddress_FetchDec_Frame_t;typedef struct Main__Test_AtomicAddress_FetchDec_Frame_t Main__Test_AtomicAddress_FetchDec_Frame_t;
void /*TypeText3*/ 
__cdecl
Main__Test_AtomicAddress_FetchDec(void);
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct Main__Test_AtomicAddress_FetchInc_Frame_t;typedef struct Main__Test_AtomicAddress_FetchInc_Frame_t Main__Test_AtomicAddress_FetchInc_Frame_t;
void /*TypeText3*/ 
__cdecl
Main__Test_AtomicAddress_FetchInc(void);
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct Main__Test_AtomicAddress_FetchOr_Frame_t;typedef struct Main__Test_AtomicAddress_FetchOr_Frame_t Main__Test_AtomicAddress_FetchOr_Frame_t;
void /*TypeText3*/ 
__cdecl
Main__Test_AtomicAddress_FetchOr(void);
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct Main__Test_AtomicAddress_FetchXor_Frame_t;typedef struct Main__Test_AtomicAddress_FetchXor_Frame_t Main__Test_AtomicAddress_FetchXor_Frame_t;
void /*TypeText3*/ 
__cdecl
Main__Test_AtomicAddress_FetchXor(void);
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct Main__Test_AtomicAddress_IsLockFree_Frame_t;typedef struct Main__Test_AtomicAddress_IsLockFree_Frame_t Main__Test_AtomicAddress_IsLockFree_Frame_t;
BOOLEAN
__cdecl
Main__Test_AtomicAddress_IsLockFree(void);
 /* declare_local */
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct Main__Test_AtomicAddress_LoadStore_Frame_t;typedef struct Main__Test_AtomicAddress_LoadStore_Frame_t Main__Test_AtomicAddress_LoadStore_Frame_t;
void /*TypeText3*/ 
__cdecl
Main__Test_AtomicAddress_LoadStore(void);
 /* declare_local */
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct Main__Test_AtomicAddress_Swap_Frame_t;typedef struct Main__Test_AtomicAddress_Swap_Frame_t Main__Test_AtomicAddress_Swap_Frame_t;
void /*TypeText3*/ 
__cdecl
Main__Test_AtomicAddress_Swap(void);
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct Main__Test_AtomicAddress_Frame_t;typedef struct Main__Test_AtomicAddress_Frame_t Main__Test_AtomicAddress_Frame_t;
void /*TypeText3*/ 
__cdecl
Main__Test_AtomicAddress(void);
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct Main__Test_AtomicInteger_Fence_Frame_t;typedef struct Main__Test_AtomicInteger_Fence_Frame_t Main__Test_AtomicInteger_Fence_Frame_t;
void /*TypeText3*/ 
__cdecl
Main__Test_AtomicInteger_Fence(void);
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct Main__Test_AtomicInteger_CompareSwap_Frame_t;typedef struct Main__Test_AtomicInteger_CompareSwap_Frame_t Main__Test_AtomicInteger_CompareSwap_Frame_t;
void /*TypeText3*/ 
__cdecl
Main__Test_AtomicInteger_CompareSwap(void);
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct Main__Test_AtomicInteger_FetchAnd_Frame_t;typedef struct Main__Test_AtomicInteger_FetchAnd_Frame_t Main__Test_AtomicInteger_FetchAnd_Frame_t;
void /*TypeText3*/ 
__cdecl
Main__Test_AtomicInteger_FetchAnd(void);
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct Main__Test_AtomicInteger_FetchDec_Frame_t;typedef struct Main__Test_AtomicInteger_FetchDec_Frame_t Main__Test_AtomicInteger_FetchDec_Frame_t;
void /*TypeText3*/ 
__cdecl
Main__Test_AtomicInteger_FetchDec(void);
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct Main__Test_AtomicInteger_FetchInc_Frame_t;typedef struct Main__Test_AtomicInteger_FetchInc_Frame_t Main__Test_AtomicInteger_FetchInc_Frame_t;
void /*TypeText3*/ 
__cdecl
Main__Test_AtomicInteger_FetchInc(void);
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct Main__Test_AtomicInteger_FetchOr_Frame_t;typedef struct Main__Test_AtomicInteger_FetchOr_Frame_t Main__Test_AtomicInteger_FetchOr_Frame_t;
void /*TypeText3*/ 
__cdecl
Main__Test_AtomicInteger_FetchOr(void);
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct Main__Test_AtomicInteger_FetchXor_Frame_t;typedef struct Main__Test_AtomicInteger_FetchXor_Frame_t Main__Test_AtomicInteger_FetchXor_Frame_t;
void /*TypeText3*/ 
__cdecl
Main__Test_AtomicInteger_FetchXor(void);
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct Main__Test_AtomicInteger_IsLockFree_Frame_t;typedef struct Main__Test_AtomicInteger_IsLockFree_Frame_t Main__Test_AtomicInteger_IsLockFree_Frame_t;
BOOLEAN
__cdecl
Main__Test_AtomicInteger_IsLockFree(void);
 /* declare_local */
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct Main__Test_AtomicInteger_LoadStore_Frame_t;typedef struct Main__Test_AtomicInteger_LoadStore_Frame_t Main__Test_AtomicInteger_LoadStore_Frame_t;
void /*TypeText3*/ 
__cdecl
Main__Test_AtomicInteger_LoadStore(void);
 /* declare_local */
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct Main__Test_AtomicInteger_Swap_Frame_t;typedef struct Main__Test_AtomicInteger_Swap_Frame_t Main__Test_AtomicInteger_Swap_Frame_t;
void /*TypeText3*/ 
__cdecl
Main__Test_AtomicInteger_Swap(void);
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct Main__Test_AtomicInteger_Frame_t;typedef struct Main__Test_AtomicInteger_Frame_t Main__Test_AtomicInteger_Frame_t;
void /*TypeText3*/ 
__cdecl
Main__Test_AtomicInteger(void);
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct Main__Test_AtomicLongint_Fence_Frame_t;typedef struct Main__Test_AtomicLongint_Fence_Frame_t Main__Test_AtomicLongint_Fence_Frame_t;
void /*TypeText3*/ 
__cdecl
Main__Test_AtomicLongint_Fence(void);
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct Main__Test_AtomicLongint_CompareSwap_Frame_t;typedef struct Main__Test_AtomicLongint_CompareSwap_Frame_t Main__Test_AtomicLongint_CompareSwap_Frame_t;
void /*TypeText3*/ 
__cdecl
Main__Test_AtomicLongint_CompareSwap(void);
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct Main__Test_AtomicLongint_FetchAnd_Frame_t;typedef struct Main__Test_AtomicLongint_FetchAnd_Frame_t Main__Test_AtomicLongint_FetchAnd_Frame_t;
void /*TypeText3*/ 
__cdecl
Main__Test_AtomicLongint_FetchAnd(void);
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct Main__Test_AtomicLongint_FetchDec_Frame_t;typedef struct Main__Test_AtomicLongint_FetchDec_Frame_t Main__Test_AtomicLongint_FetchDec_Frame_t;
void /*TypeText3*/ 
__cdecl
Main__Test_AtomicLongint_FetchDec(void);
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct Main__Test_AtomicLongint_FetchInc_Frame_t;typedef struct Main__Test_AtomicLongint_FetchInc_Frame_t Main__Test_AtomicLongint_FetchInc_Frame_t;
void /*TypeText3*/ 
__cdecl
Main__Test_AtomicLongint_FetchInc(void);
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct Main__Test_AtomicLongint_FetchOr_Frame_t;typedef struct Main__Test_AtomicLongint_FetchOr_Frame_t Main__Test_AtomicLongint_FetchOr_Frame_t;
void /*TypeText3*/ 
__cdecl
Main__Test_AtomicLongint_FetchOr(void);
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct Main__Test_AtomicLongint_FetchXor_Frame_t;typedef struct Main__Test_AtomicLongint_FetchXor_Frame_t Main__Test_AtomicLongint_FetchXor_Frame_t;
void /*TypeText3*/ 
__cdecl
Main__Test_AtomicLongint_FetchXor(void);
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct Main__Test_AtomicLongint_IsLockFree_Frame_t;typedef struct Main__Test_AtomicLongint_IsLockFree_Frame_t Main__Test_AtomicLongint_IsLockFree_Frame_t;
BOOLEAN
__cdecl
Main__Test_AtomicLongint_IsLockFree(void);
 /* declare_local */
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct Main__Test_AtomicLongint_Load_Frame_t;typedef struct Main__Test_AtomicLongint_Load_Frame_t Main__Test_AtomicLongint_Load_Frame_t;
void /*TypeText3*/ 
__cdecl
Main__Test_AtomicLongint_Load(void);
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct Main__Test_AtomicLongint_Store_Frame_t;typedef struct Main__Test_AtomicLongint_Store_Frame_t Main__Test_AtomicLongint_Store_Frame_t;
void /*TypeText3*/ 
__cdecl
Main__Test_AtomicLongint_Store(void);
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct Main__Test_AtomicLongint_Swap_Frame_t;typedef struct Main__Test_AtomicLongint_Swap_Frame_t Main__Test_AtomicLongint_Swap_Frame_t;
void /*TypeText3*/ 
__cdecl
Main__Test_AtomicLongint_Swap(void);
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct Main__Test_AtomicLongint_Frame_t;typedef struct Main__Test_AtomicLongint_Frame_t Main__Test_AtomicLongint_Frame_t;
void /*TypeText3*/ 
__cdecl
Main__Test_AtomicLongint(void);
 /* Locals_begin_procedure */
 /* begin_block */
 /* Locals_end_procedure */
 /* end_block */
 /* Locals_begin_procedure */
 /* begin_block */
 /* Locals_end_procedure */
 /* end_block */
 /* Locals_begin_procedure */
 /* begin_block */
 /* Locals_end_procedure */
 /* end_block */
 /* Locals_begin_procedure */
 /* begin_block */
 /* Locals_end_procedure */
 /* end_block */
 /* Locals_begin_procedure */
 /* begin_block */
 /* Locals_end_procedure */
 /* end_block */
 /* Locals_begin_procedure */
 /* begin_block */
 /* Locals_end_procedure */
 /* end_block */
 /* Locals_begin_procedure */
 /* begin_block */
 /* Locals_end_procedure */
 /* end_block */
 /* Locals_begin_procedure */
 /* begin_block */
 /* Locals_end_procedure */
 /* end_block */
 /* Locals_begin_procedure */
 /* begin_block */
 /* Locals_end_procedure */
 /* end_block */
 /* Locals_begin_procedure */
 /* begin_block */
 /* Locals_end_procedure */
 /* end_block */
 /* Locals_begin_procedure */
 /* begin_block */
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
 /* Locals_begin_procedure */
 /* begin_block */
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
 /* declare_temp */
 /* declare_local */
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
 /* init_proc */
 /* init_var */
 /* init_proc */
 /* init_var */
 /* init_proc */
 /* init_var */
 /* init_proc */
 /* init_var */
 /* init_proc */
 /* init_var */
 /* init_proc */
 /* init_var */
 /* init_proc */
 /* init_var */
 /* init_proc */
 /* init_var */
 /* init_proc */
 /* init_var */
 /* init_proc */
 /* init_var */
 /* init_proc */
 /* init_var */
 /* init_proc */
 /* init_var */
 /* init_proc */
 /* init_var */
 /* init_proc */
 /* init_var */
 /* init_proc */
 /* init_var */
 /* init_proc */
 /* init_var */
 /* init_proc */
 /* init_var */
 /* init_proc */
 /* init_var */
 /* init_proc */
 /* init_var */
 /* init_proc */
 /* init_var */
 /* init_proc */
 /* init_var */
 /* init_proc */
 /* init_var */
 /* init_proc */
 /* init_var */
 /* init_proc */
 /* init_var */
 /* init_proc */
 /* init_var */
 /* init_proc */
 /* init_var */
 /* init_proc */
 /* init_var */
 /* init_proc */
 /* init_var */
 /* init_proc */
 /* init_var */
 /* init_proc */
 /* init_var */
 /* init_proc */
 /* init_var */
 /* init_proc */
 /* init_var */
 /* init_proc */
 /* init_var */
 /* init_proc */
 /* init_var */
 /* init_proc */
 /* init_var */
 /* init_proc */
 /* init_var */
 /* init_proc */
 /* init_var */
 /* init_proc */
 /* init_var */
 /* init_proc */
 /* init_var */
 /* init_proc */
 /* init_var */
 /* init_proc */
 /* init_var */
 /* init_proc */
 /* init_var */
 /* init_proc */
 /* init_var */
 /* init_proc */
 /* init_var */
 /* init_proc */
 /* init_var */
 /* init_proc */
 /* init_var */
 /* init_proc */
 /* init_var */
 /* init_proc */
 /* init_var */
 /* init_proc */
 /* init_var */
 /* init_proc */
 /* init_var */
 /* init_proc */
 /* init_var */
 /* init_proc */
 /* init_var */
 /* init_proc */
 /* init_var */
 /* init_proc */
 /* init_var */
 /* init_proc */
 /* init_var */
 /* init_proc */
 /* init_var */
 /* init_proc */
 /* init_var */
 /* init_proc */
 /* init_var */
 /* init_proc */
 /* init_var */
 /* init_proc */
 /* init_var */
 /* init_proc */
 /* init_var */
 /* init_proc */
 /* init_var */
 /* init_proc */
 /* init_var */
 /* init_proc */
 /* init_var */
 /* init_proc */
 /* init_var */
 /* init_proc */
 /* init_var */
 /* init_proc */
 /* init_var */
 /* init_proc */
 /* init_var */
 /* init_proc */
 /* init_var */
 /* init_proc */
 /* init_var */
 /* init_proc */
 /* init_var */
 /* init_proc */
 /* init_var */
 /* init_proc */
 /* init_var */
 /* init_proc */
 /* init_var */
 /* init_proc */
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
 /* init_int */
 /* init_int */
 /* init_int */
 /* init_int */
 /* init_int */
 /* init_int */
 /* init_int */
 /* init_chars */
 /* end_init */
struct Main_m_18_L_19_t{UINT8 L_54[7];
char L_55[1];
UINT8 L_56[18];
char L_57[1];
UINT8 L_58[23];
char L_59[1];
UINT8 L_60[24];
char L_61[1];
UINT8 L_62[23];
char L_63[1];
UINT8 L_64[29];
char L_65[1];
UINT8 L_66[27];
char L_67[1];
UINT8 L_68[26];
char L_69[1];
UINT8 L_70[27];
char L_71[1];
UINT8 L_72[27];
char L_73[1];
UINT8 L_74[27];
char L_75[1];
UINT8 L_76[30];
char L_77[1];
UINT8 L_78[24];
char L_79[1];
UINT8 L_80[18];
char L_81[1];
UINT8 L_82[23];
char L_83[1];
UINT8 L_84[28];
char L_85[1];
UINT8 L_86[29];
char L_87[1];
UINT8 L_88[27];
char L_89[1];
UINT8 L_90[26];
char L_91[1];
UINT8 L_92[27];
char L_93[1];
UINT8 L_94[27];
char L_95[1];
UINT8 L_96[27];
char L_97[1];
UINT8 L_98[30];
char L_99[1];
UINT8 L_100[24];
char L_101[1];
UINT8 L_102[18];
char L_103[1];
UINT8 L_104[23];
char L_105[1];
UINT8 L_106[28];
char L_107[1];
UINT8 L_108[29];
char L_109[1];
UINT8 L_110[27];
char L_111[1];
UINT8 L_112[26];
char L_113[1];
UINT8 L_114[27];
char L_115[1];
UINT8 L_116[27];
char L_117[1];
UINT8 L_118[27];
char L_119[1];
UINT8 L_120[30];
char L_121[1];
UINT8 L_122[24];
char L_123[1];
UINT8 L_124[17];
char L_125[1];
UINT8 L_126[22];
char L_127[1];
UINT8 L_128[27];
char L_129[1];
UINT8 L_130[28];
char L_131[1];
UINT8 L_132[26];
char L_133[1];
UINT8 L_134[25];
char L_135[1];
UINT8 L_136[26];
char L_137[1];
UINT8 L_138[26];
char L_139[1];
UINT8 L_140[26];
char L_141[1];
UINT8 L_142[29];
char L_143[1];
UINT8 L_144[23];
char L_145[1];
UINT8 L_146[19];
char L_147[1];
UINT8 L_148[24];
char L_149[1];
UINT8 L_150[29];
char L_151[1];
UINT8 L_152[30];
char L_153[1];
UINT8 L_154[28];
char L_155[1];
UINT8 L_156[27];
char L_157[1];
UINT8 L_158[28];
char L_159[1];
UINT8 L_160[28];
char L_161[1];
UINT8 L_162[28];
char L_163[1];
UINT8 L_164[31];
char L_165[1];
UINT8 L_166[25];
char L_167[1];
UINT8 L_168[15];
char L_169[1];
UINT8 L_170[20];
char L_171[1];
UINT8 L_172[25];
char L_173[1];
UINT8 L_174[26];
char L_175[1];
UINT8 L_176[24];
char L_177[1];
UINT8 L_178[23];
char L_179[1];
UINT8 L_180[24];
char L_181[1];
UINT8 L_182[24];
char L_183[1];
UINT8 L_184[24];
char L_185[1];
UINT8 L_186[27];
char L_187[1];
UINT8 L_188[21];
char L_189[1];
UINT8 L_190[18];
char L_191[1];
UINT8 L_192[23];
char L_193[1];
UINT8 L_194[28];
char L_195[1];
UINT8 L_196[29];
char L_197[1];
UINT8 L_198[27];
char L_199[1];
UINT8 L_200[26];
char L_201[1];
UINT8 L_202[27];
char L_203[1];
UINT8 L_204[27];
char L_205[1];
UINT8 L_206[27];
char L_207[1];
UINT8 L_208[30];
char L_209[1];
UINT8 L_210[24];
char L_211[7];
ADDRESS L_212[158];
char L_213[8];
INT8 L_214[1];
UINT8 L_215[1];
INT8 L_216[6];
UINT8 L_217[10];
char L_218[6];
};
static  const Main_m_18_L_19_t Main_m_18_L_19={{'M','a','i','n','_','M','3'},{0 /* 1 */ ,},{'T','e','s','t','_','A','t','o','m','i','c','L','o','n','g','i','n','t'},{0 /* 1 */ ,},{'T','e','s','t','_','A','t','o','m','i','c','L','o','n','g','i','n','t','_','S','w','a','p'},{0 /* 1 */ ,},{'T','e','s','t','_','A','t','o','m','i','c','L','o','n','g','i','n','t','_','S','t','o','r','e'},{0 /* 1 */ ,},{'T','e','s','t','_','A','t','o','m','i','c','L','o','n','g','i','n','t','_','L','o','a','d'},{0 /* 1 */ ,},{'T','e','s','t','_','A','t','o','m','i','c','L','o','n','g','i','n','t','_','I','s','L','o','c','k','F','r','e','e'},{0 /* 1 */ ,},{'T','e','s','t','_','A','t','o','m','i','c','L','o','n','g','i','n','t','_','F','e','t','c','h','X','o','r'},{0 /* 1 */ ,},{'T','e','s','t','_','A','t','o','m','i','c','L','o','n','g','i','n','t','_','F','e','t','c','h','O','r'},{0 /* 1 */ ,},{'T','e','s','t','_','A','t','o','m','i','c','L','o','n','g','i','n','t','_','F','e','t','c','h','I','n','c'},{0 /* 1 */ 
,},{'T','e','s','t','_','A','t','o','m','i','c','L','o','n','g','i','n','t','_','F','e','t','c','h','D','e','c'},{0 /* 1 */ ,},{'T','e','s','t','_','A','t','o','m','i','c','L','o','n','g','i','n','t','_','F','e','t','c','h','A','n','d'},{0 /* 1 */ ,},{'T','e','s','t','_','A','t','o','m','i','c','L','o','n','g','i','n','t','_','C','o','m','p','a','r','e','S','w','a','p'},{0 /* 1 */ ,},{'T','e','s','t','_','A','t','o','m','i','c','L','o','n','g','i','n','t','_','F','e','n','c','e'},{0 /* 1 */ ,},{'T','e','s','t','_','A','t','o','m','i','c','I','n','t','e','g','e','r'},{0 /* 1 */ ,},{'T','e','s','t','_','A','t','o','m','i','c','I','n','t','e','g','e','r','_','S','w','a','p'},{0 /* 1 */ ,},{'T','e','s','t','_','A','t','o','m','i','c','I','n','t','e','g','e','r','_','L','o','a','d','S','t','o','r','e'},{0 /* 1 */ ,},{'T','e','s','t','_','A','t','o','m','i','c','I','n','t','e','g','e','r','_','I','s','L','o','c','k','F','r','e','e'},{0 /* 1 */ ,},{'T','e','s','t','_','A','t','o','m','i','c',
'I','n','t','e','g','e','r','_','F','e','t','c','h','X','o','r'},{0 /* 1 */ ,},{'T','e','s','t','_','A','t','o','m','i','c','I','n','t','e','g','e','r','_','F','e','t','c','h','O','r'},{0 /* 1 */ ,},{'T','e','s','t','_','A','t','o','m','i','c','I','n','t','e','g','e','r','_','F','e','t','c','h','I','n','c'},{0 /* 1 */ ,},{'T','e','s','t','_','A','t','o','m','i','c','I','n','t','e','g','e','r','_','F','e','t','c','h','D','e','c'},{0 /* 1 */ ,},{'T','e','s','t','_','A','t','o','m','i','c','I','n','t','e','g','e','r','_','F','e','t','c','h','A','n','d'},{0 /* 1 */ ,},{'T','e','s','t','_','A','t','o','m','i','c','I','n','t','e','g','e','r','_','C','o','m','p','a','r','e','S','w','a','p'},{0 /* 1 */ ,},{'T','e','s','t','_','A','t','o','m','i','c','I','n','t','e','g','e','r','_','F','e','n','c','e'},{0 /* 1 */ ,},{'T','e','s','t','_','A','t','o','m','i','c','A','d','d','r','e','s','s'},{0 /* 1 */ ,},{'T','e','s','t','_','A','t','o','m','i','c','A','d','d','r','e','s','s','_','S','w','a','p'}
,{0 /* 1 */ ,},{'T','e','s','t','_','A','t','o','m','i','c','A','d','d','r','e','s','s','_','L','o','a','d','S','t','o','r','e'},{0 /* 1 */ ,},{'T','e','s','t','_','A','t','o','m','i','c','A','d','d','r','e','s','s','_','I','s','L','o','c','k','F','r','e','e'},{0 /* 1 */ ,},{'T','e','s','t','_','A','t','o','m','i','c','A','d','d','r','e','s','s','_','F','e','t','c','h','X','o','r'},{0 /* 1 */ ,},{'T','e','s','t','_','A','t','o','m','i','c','A','d','d','r','e','s','s','_','F','e','t','c','h','O','r'},{0 /* 1 */ ,},{'T','e','s','t','_','A','t','o','m','i','c','A','d','d','r','e','s','s','_','F','e','t','c','h','I','n','c'},{0 /* 1 */ ,},{'T','e','s','t','_','A','t','o','m','i','c','A','d','d','r','e','s','s','_','F','e','t','c','h','D','e','c'},{0 /* 1 */ ,},{'T','e','s','t','_','A','t','o','m','i','c','A','d','d','r','e','s','s','_','F','e','t','c','h','A','n','d'},{0 /* 1 */ ,},{'T','e','s','t','_','A','t','o','m','i','c','A','d','d','r','e','s','s','_','C','o','m','p','a','r','e','S',
'w','a','p'},{0 /* 1 */ ,},{'T','e','s','t','_','A','t','o','m','i','c','A','d','d','r','e','s','s','_','F','e','n','c','e'},{0 /* 1 */ ,},{'T','e','s','t','_','A','t','o','m','i','c','R','e','f','a','n','y'},{0 /* 1 */ ,},{'T','e','s','t','_','A','t','o','m','i','c','R','e','f','a','n','y','_','S','w','a','p'},{0 /* 1 */ ,},{'T','e','s','t','_','A','t','o','m','i','c','R','e','f','a','n','y','_','L','o','a','d','S','t','o','r','e'},{0 /* 1 */ ,},{'T','e','s','t','_','A','t','o','m','i','c','R','e','f','a','n','y','_','I','s','L','o','c','k','F','r','e','e'},{0 /* 1 */ ,},{'T','e','s','t','_','A','t','o','m','i','c','R','e','f','a','n','y','_','F','e','t','c','h','X','o','r'},{0 /* 1 */ ,},{'T','e','s','t','_','A','t','o','m','i','c','R','e','f','a','n','y','_','F','e','t','c','h','O','r'},{0 /* 1 */ ,},{'T','e','s','t','_','A','t','o','m','i','c','R','e','f','a','n','y','_','F','e','t','c','h','I','n','c'},{0 /* 1 */ ,},{'T','e','s','t','_','A','t','o','m','i','c','R','e','f','a','n',
'y','_','F','e','t','c','h','D','e','c'},{0 /* 1 */ ,},{'T','e','s','t','_','A','t','o','m','i','c','R','e','f','a','n','y','_','F','e','t','c','h','A','n','d'},{0 /* 1 */ ,},{'T','e','s','t','_','A','t','o','m','i','c','R','e','f','a','n','y','_','C','o','m','p','a','r','e','S','w','a','p'},{0 /* 1 */ ,},{'T','e','s','t','_','A','t','o','m','i','c','R','e','f','a','n','y','_','F','e','n','c','e'},{0 /* 1 */ ,},{'T','e','s','t','_','A','t','o','m','i','c','W','i','d','e','c','h','a','r'},{0 /* 1 */ ,},{'T','e','s','t','_','A','t','o','m','i','c','W','i','d','e','c','h','a','r','_','S','w','a','p'},{0 /* 1 */ ,},{'T','e','s','t','_','A','t','o','m','i','c','W','i','d','e','c','h','a','r','_','L','o','a','d','S','t','o','r','e'},{0 /* 1 */ ,},{'T','e','s','t','_','A','t','o','m','i','c','W','i','d','e','c','h','a','r','_','I','s','L','o','c','k','F','r','e','e'},{0 /* 1 */ ,},{'T','e','s','t','_','A','t','o','m','i','c','W','i','d','e','c','h','a','r','_','F','e','t','c','h','X','o','r'}
,{0 /* 1 */ ,},{'T','e','s','t','_','A','t','o','m','i','c','W','i','d','e','c','h','a','r','_','F','e','t','c','h','O','r'},{0 /* 1 */ ,},{'T','e','s','t','_','A','t','o','m','i','c','W','i','d','e','c','h','a','r','_','F','e','t','c','h','I','n','c'},{0 /* 1 */ ,},{'T','e','s','t','_','A','t','o','m','i','c','W','i','d','e','c','h','a','r','_','F','e','t','c','h','D','e','c'},{0 /* 1 */ ,},{'T','e','s','t','_','A','t','o','m','i','c','W','i','d','e','c','h','a','r','_','F','e','t','c','h','A','n','d'},{0 /* 1 */ ,},{'T','e','s','t','_','A','t','o','m','i','c','W','i','d','e','c','h','a','r','_','C','o','m','p','a','r','e','S','w','a','p'},{0 /* 1 */ ,},{'T','e','s','t','_','A','t','o','m','i','c','W','i','d','e','c','h','a','r','_','F','e','n','c','e'},{0 /* 1 */ ,},{'T','e','s','t','_','A','t','o','m','i','c','C','h','a','r'},{0 /* 1 */ ,},{'T','e','s','t','_','A','t','o','m','i','c','C','h','a','r','_','S','w','a','p'},{0 /* 1 */ ,},{'T','e','s','t','_','A','t','o','m','i','c','C',
'h','a','r','_','L','o','a','d','S','t','o','r','e'},{0 /* 1 */ ,},{'T','e','s','t','_','A','t','o','m','i','c','C','h','a','r','_','I','s','L','o','c','k','F','r','e','e'},{0 /* 1 */ ,},{'T','e','s','t','_','A','t','o','m','i','c','C','h','a','r','_','F','e','t','c','h','X','o','r'},{0 /* 1 */ ,},{'T','e','s','t','_','A','t','o','m','i','c','C','h','a','r','_','F','e','t','c','h','O','r'},{0 /* 1 */ ,},{'T','e','s','t','_','A','t','o','m','i','c','C','h','a','r','_','F','e','t','c','h','I','n','c'},{0 /* 1 */ ,},{'T','e','s','t','_','A','t','o','m','i','c','C','h','a','r','_','F','e','t','c','h','D','e','c'},{0 /* 1 */ ,},{'T','e','s','t','_','A','t','o','m','i','c','C','h','a','r','_','F','e','t','c','h','A','n','d'},{0 /* 1 */ ,},{'T','e','s','t','_','A','t','o','m','i','c','C','h','a','r','_','C','o','m','p','a','r','e','S','w','a','p'},{0 /* 1 */ ,},{'T','e','s','t','_','A','t','o','m','i','c','C','h','a','r','_','F','e','n','c','e'},{0 /* 1 */ ,},{'T','e','s','t','_','A','t','o',
'm','i','c','B','o','o','l','e','a','n'},{0 /* 1 */ ,},{'T','e','s','t','_','A','t','o','m','i','c','B','o','o','l','e','a','n','_','S','w','a','p'},{0 /* 1 */ ,},{'T','e','s','t','_','A','t','o','m','i','c','B','o','o','l','e','a','n','_','L','o','a','d','S','t','o','r','e'},{0 /* 1 */ ,},{'T','e','s','t','_','A','t','o','m','i','c','B','o','o','l','e','a','n','_','I','s','L','o','c','k','F','r','e','e'},{0 /* 1 */ ,},{'T','e','s','t','_','A','t','o','m','i','c','B','o','o','l','e','a','n','_','F','e','t','c','h','X','o','r'},{0 /* 1 */ ,},{'T','e','s','t','_','A','t','o','m','i','c','B','o','o','l','e','a','n','_','F','e','t','c','h','O','r'},{0 /* 1 */ ,},{'T','e','s','t','_','A','t','o','m','i','c','B','o','o','l','e','a','n','_','F','e','t','c','h','I','n','c'},{0 /* 1 */ ,},{'T','e','s','t','_','A','t','o','m','i','c','B','o','o','l','e','a','n','_','F','e','t','c','h','D','e','c'},{0 /* 1 */ ,},{'T','e','s','t','_','A','t','o','m','i','c','B','o','o','l','e','a','n','_','F','e',
't','c','h','A','n','d'},{0 /* 1 */ ,},{'T','e','s','t','_','A','t','o','m','i','c','B','o','o','l','e','a','n','_','C','o','m','p','a','r','e','S','w','a','p'},{0 /* 1 */ ,},{'T','e','s','t','_','A','t','o','m','i','c','B','o','o','l','e','a','n','_','F','e','n','c','e'},{0 /* 1 */ ,0 /* 2 */ ,0 /* 3 */ ,0 /* 4 */ ,0 /* 5 */ ,0 /* 6 */ ,0 /* 7 */ ,},{(ADDRESS)&Main_M3,(char*)&Main_m_18_L_19,(ADDRESS)&Main__Test_AtomicLongint,8+(char*)&Main_m_18_L_19,(ADDRESS)&Main__Test_AtomicLongint_Swap,27+(char*)&Main_m_18_L_19,(ADDRESS)&Main__Test_AtomicLongint_Store,51+(char*)&Main_m_18_L_19,(ADDRESS)&Main__Test_AtomicLongint_Load,76+(char*)&Main_m_18_L_19,(ADDRESS)&Main__Test_AtomicLongint_IsLockFree,100+(char*)&Main_m_18_L_19,(ADDRESS)&Main__Test_AtomicLongint_FetchXor,130+(char*)&Main_m_18_L_19,(ADDRESS)&Main__Test_AtomicLongint_FetchOr,158+(char*)&Main_m_18_L_19,(ADDRESS)&Main__Test_AtomicLongint_FetchInc,185+(char*)&Main_m_18_L_19,(ADDRESS)&Main__Test_AtomicLongint_FetchDec,213+(char*)&Main_m_18_L_19
,(ADDRESS)&Main__Test_AtomicLongint_FetchAnd,241+(char*)&Main_m_18_L_19,(ADDRESS)&Main__Test_AtomicLongint_CompareSwap,269+(char*)&Main_m_18_L_19,(ADDRESS)&Main__Test_AtomicLongint_Fence,300+(char*)&Main_m_18_L_19,(ADDRESS)&Main__Test_AtomicInteger,325+(char*)&Main_m_18_L_19,(ADDRESS)&Main__Test_AtomicInteger_Swap,344+(char*)&Main_m_18_L_19,(ADDRESS)&Main__Test_AtomicInteger_LoadStore,368+(char*)&Main_m_18_L_19,(ADDRESS)&Main__Test_AtomicInteger_IsLockFree,397+(char*)&Main_m_18_L_19,(ADDRESS)&Main__Test_AtomicInteger_FetchXor,427+(char*)&Main_m_18_L_19,(ADDRESS)&Main__Test_AtomicInteger_FetchOr,455+(char*)&Main_m_18_L_19,(ADDRESS)&Main__Test_AtomicInteger_FetchInc,482+(char*)&Main_m_18_L_19,(ADDRESS)&Main__Test_AtomicInteger_FetchDec,510+(char*)&Main_m_18_L_19,(ADDRESS)&Main__Test_AtomicInteger_FetchAnd,538+(char*)&Main_m_18_L_19,(ADDRESS)&Main__Test_AtomicInteger_CompareSwap,566+(char*)&Main_m_18_L_19,(ADDRESS)&Main__Test_AtomicInteger_Fence,597+(char*)&Main_m_18_L_19,(ADDRESS)&Main__Test_AtomicAddress
,622+(char*)&Main_m_18_L_19,(ADDRESS)&Main__Test_AtomicAddress_Swap,641+(char*)&Main_m_18_L_19,(ADDRESS)&Main__Test_AtomicAddress_LoadStore,665+(char*)&Main_m_18_L_19,(ADDRESS)&Main__Test_AtomicAddress_IsLockFree,694+(char*)&Main_m_18_L_19,(ADDRESS)&Main__Test_AtomicAddress_FetchXor,724+(char*)&Main_m_18_L_19,(ADDRESS)&Main__Test_AtomicAddress_FetchOr,752+(char*)&Main_m_18_L_19,(ADDRESS)&Main__Test_AtomicAddress_FetchInc,779+(char*)&Main_m_18_L_19,(ADDRESS)&Main__Test_AtomicAddress_FetchDec,807+(char*)&Main_m_18_L_19,(ADDRESS)&Main__Test_AtomicAddress_FetchAnd,835+(char*)&Main_m_18_L_19,(ADDRESS)&Main__Test_AtomicAddress_CompareSwap,863+(char*)&Main_m_18_L_19,(ADDRESS)&Main__Test_AtomicAddress_Fence,894+(char*)&Main_m_18_L_19,(ADDRESS)&Main__Test_AtomicRefany,919+(char*)&Main_m_18_L_19,(ADDRESS)&Main__Test_AtomicRefany_Swap,937+(char*)&Main_m_18_L_19,(ADDRESS)&Main__Test_AtomicRefany_LoadStore,960+(char*)&Main_m_18_L_19,(ADDRESS)&Main__Test_AtomicRefany_IsLockFree,988+(char*)&Main_m_18_L_19
,(ADDRESS)&Main__Test_AtomicRefany_FetchXor,1017+(char*)&Main_m_18_L_19,(ADDRESS)&Main__Test_AtomicRefany_FetchOr,1044+(char*)&Main_m_18_L_19,(ADDRESS)&Main__Test_AtomicRefany_FetchInc,1070+(char*)&Main_m_18_L_19,(ADDRESS)&Main__Test_AtomicRefany_FetchDec,1097+(char*)&Main_m_18_L_19,(ADDRESS)&Main__Test_AtomicRefany_FetchAnd,1124+(char*)&Main_m_18_L_19,(ADDRESS)&Main__Test_AtomicRefany_CompareSwap,1151+(char*)&Main_m_18_L_19,(ADDRESS)&Main__Test_AtomicRefany_Fence,1181+(char*)&Main_m_18_L_19,(ADDRESS)&Main__Test_AtomicWidechar,1205+(char*)&Main_m_18_L_19,(ADDRESS)&Main__Test_AtomicWidechar_Swap,1225+(char*)&Main_m_18_L_19,(ADDRESS)&Main__Test_AtomicWidechar_LoadStore,1250+(char*)&Main_m_18_L_19,(ADDRESS)&Main__Test_AtomicWidechar_IsLockFree,1280+(char*)&Main_m_18_L_19,(ADDRESS)&Main__Test_AtomicWidechar_FetchXor,1311+(char*)&Main_m_18_L_19,(ADDRESS)&Main__Test_AtomicWidechar_FetchOr,1340+(char*)&Main_m_18_L_19,(ADDRESS)&Main__Test_AtomicWidechar_FetchInc,1368+(char*)&Main_m_18_L_19,(ADDRESS)&Main__Test_AtomicWidechar_FetchDec
,1397+(char*)&Main_m_18_L_19,(ADDRESS)&Main__Test_AtomicWidechar_FetchAnd,1426+(char*)&Main_m_18_L_19,(ADDRESS)&Main__Test_AtomicWidechar_CompareSwap,1455+(char*)&Main_m_18_L_19,(ADDRESS)&Main__Test_AtomicWidechar_Fence,1487+(char*)&Main_m_18_L_19,(ADDRESS)&Main__Test_AtomicChar,1513+(char*)&Main_m_18_L_19,(ADDRESS)&Main__Test_AtomicChar_Swap,1529+(char*)&Main_m_18_L_19,(ADDRESS)&Main__Test_AtomicChar_LoadStore,1550+(char*)&Main_m_18_L_19,(ADDRESS)&Main__Test_AtomicChar_IsLockFree,1576+(char*)&Main_m_18_L_19,(ADDRESS)&Main__Test_AtomicChar_FetchXor,1603+(char*)&Main_m_18_L_19,(ADDRESS)&Main__Test_AtomicChar_FetchOr,1628+(char*)&Main_m_18_L_19,(ADDRESS)&Main__Test_AtomicChar_FetchInc,1652+(char*)&Main_m_18_L_19,(ADDRESS)&Main__Test_AtomicChar_FetchDec,1677+(char*)&Main_m_18_L_19,(ADDRESS)&Main__Test_AtomicChar_FetchAnd,1702+(char*)&Main_m_18_L_19,(ADDRESS)&Main__Test_AtomicChar_CompareSwap,1727+(char*)&Main_m_18_L_19,(ADDRESS)&Main__Test_AtomicChar_Fence,1755+(char*)&Main_m_18_L_19,(ADDRESS)&Main__Test_AtomicBoolean
,1777+(char*)&Main_m_18_L_19,(ADDRESS)&Main__Test_AtomicBoolean_Swap,1796+(char*)&Main_m_18_L_19,(ADDRESS)&Main__Test_AtomicBoolean_LoadStore,1820+(char*)&Main_m_18_L_19,(ADDRESS)&Main__Test_AtomicBoolean_IsLockFree,1849+(char*)&Main_m_18_L_19,(ADDRESS)&Main__Test_AtomicBoolean_FetchXor,1879+(char*)&Main_m_18_L_19,(ADDRESS)&Main__Test_AtomicBoolean_FetchOr,1907+(char*)&Main_m_18_L_19,(ADDRESS)&Main__Test_AtomicBoolean_FetchInc,1934+(char*)&Main_m_18_L_19,(ADDRESS)&Main__Test_AtomicBoolean_FetchDec,1962+(char*)&Main_m_18_L_19,(ADDRESS)&Main__Test_AtomicBoolean_FetchAnd,1990+(char*)&Main_m_18_L_19,(ADDRESS)&Main__Test_AtomicBoolean_CompareSwap,2018+(char*)&Main_m_18_L_19,(ADDRESS)&Main__Test_AtomicBoolean_Fence,2049+(char*)&Main_m_18_L_19},{0 /* 1 */ ,0 /* 2 */ ,0 /* 3 */ ,0 /* 4 */ ,0 /* 5 */ ,0 /* 6 */ ,0 /* 7 */ ,0 /* 8 */ ,},{((INT8)42)},{128U},{((INT8)4),((INT8)42),((INT8)56),((INT8)4),((INT8)4),((INT8)0)},{'.','.','/','M','a','i','n','.','m','3'},{0 /* 1 */ ,0 /* 2 */ ,0 /* 3 */ ,0 /* 4 */ 
,0 /* 5 */ ,0 /* 6 */ ,}};
 /* bind_segment */
 /* begin_init */
 /* init_var */
 /* init_var */
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
 /* init_proc */
 /* init_int */
 /* end_init */
struct Main_m_M_Main_L_20_t{ADDRESS L_219[1];
char L_220[8];
ADDRESS L_221[1];
char L_222[16];
ADDRESS L_223[1];
char L_224[8];
ADDRESS L_225[3];
char L_226[8];
ADDRESS L_227[1];
INT64 L_228[1];
char L_229[144];
ADDRESS L_230[2];
char L_231[8];
ADDRESS L_232[2];
char L_233[8];
ADDRESS L_234[2];
char L_235[8];
ADDRESS L_236[2];
char L_237[8];
ADDRESS L_238[2];
char L_239[8];
ADDRESS L_240[2];
char L_241[8];
ADDRESS L_242[2];
char L_243[8];
ADDRESS L_244[2];
char L_245[8];
ADDRESS L_246[2];
char L_247[8];
ADDRESS L_248[2];
char L_249[8];
ADDRESS L_250[2];
char L_251[8];
ADDRESS L_252[2];
char L_253[8];
ADDRESS L_254[2];
char L_255[8];
ADDRESS L_256[2];
char L_257[8];
ADDRESS L_258[2];
char L_259[8];
ADDRESS L_260[1];
char L_261[16];
INT64 L_262[1];
};
static Main_m_M_Main_L_20_t Main_m_M_Main_L_20={{3360+(char*)&Main_m_18_L_19},{0 /* 1 */ ,0 /* 2 */ ,0 /* 3 */ ,0 /* 4 */ ,0 /* 5 */ ,0 /* 6 */ ,0 /* 7 */ ,0 /* 8 */ ,},{624+(char*)&Main_m_M_Main_L_20},{0 /* 1 */ ,0 /* 2 */ ,0 /* 3 */ ,0 /* 4 */ ,0 /* 5 */ ,0 /* 6 */ ,0 /* 7 */ ,0 /* 8 */ ,0 /* 9 */ ,0 /* 10 */ ,0 /* 11 */ ,0 /* 12 */ ,0 /* 13 */ ,0 /* 14 */ ,0 /* 15 */ ,0 /* 16 */ ,},{2080+(char*)&Main_m_18_L_19},{0 /* 1 */ ,0 /* 2 */ ,0 /* 3 */ ,0 /* 4 */ ,0 /* 5 */ ,0 /* 6 */ ,0 /* 7 */ ,0 /* 8 */ ,},{3352+(char*)&Main_m_18_L_19,3352+(char*)&Main_m_18_L_19,240+(char*)&Main_m_M_Main_L_20},{0 /* 1 */ ,0 /* 2 */ ,0 /* 3 */ ,0 /* 4 */ ,0 /* 5 */ ,0 /* 6 */ ,0 /* 7 */ ,0 /* 8 */ ,},{(ADDRESS)&Main_M3},{INT64_(3)},{0 /* 1 */ ,0 /* 2 */ ,0 /* 3 */ ,0 /* 4 */ ,0 /* 5 */ ,0 /* 6 */ ,0 /* 7 */ ,0 /* 8 */ ,0 /* 9 */ ,0 /* 10 */ ,0 /* 11 */ ,0 /* 12 */ ,0 /* 13 */ ,0 /* 14 */ ,0 /* 15 */ ,0 /* 16 */ ,0 /* 17 */ ,0 /* 18 */ ,0 /* 19 */ ,0 /* 20 */ ,0 /* 21 */ ,0 /* 22 */ ,0 /* 23 */ ,0 /* 24 */ 
,0 /* 25 */ ,0 /* 26 */ ,0 /* 27 */ ,0 /* 28 */ ,0 /* 29 */ ,0 /* 30 */ ,0 /* 31 */ ,0 /* 32 */ ,0 /* 33 */ ,0 /* 34 */ ,0 /* 35 */ ,0 /* 36 */ ,0 /* 37 */ ,0 /* 38 */ ,0 /* 39 */ ,0 /* 40 */ ,0 /* 41 */ ,0 /* 42 */ ,0 /* 43 */ ,0 /* 44 */ ,0 /* 45 */ ,0 /* 46 */ ,0 /* 47 */ ,0 /* 48 */ ,0 /* 49 */ ,0 /* 50 */ ,0 /* 51 */ ,0 /* 52 */ ,0 /* 53 */ ,0 /* 54 */ ,0 /* 55 */ ,0 /* 56 */ ,0 /* 57 */ ,0 /* 58 */ ,0 /* 59 */ ,0 /* 60 */ ,0 /* 61 */ ,0 /* 62 */ ,0 /* 63 */ ,0 /* 64 */ ,0 /* 65 */ ,0 /* 66 */ ,0 /* 67 */ ,0 /* 68 */ ,0 /* 69 */ ,0 /* 70 */ ,0 /* 71 */ ,0 /* 72 */ ,0 /* 73 */ ,0 /* 74 */ ,0 /* 75 */ ,0 /* 76 */ ,0 /* 77 */ ,0 /* 78 */ ,0 /* 79 */ ,0 /* 80 */ ,0 /* 81 */ ,0 /* 82 */ ,0 /* 83 */ ,0 /* 84 */ ,0 /* 85 */ ,0 /* 86 */ ,0 /* 87 */ ,0 /* 88 */ ,0 /* 89 */ ,0 /* 90 */ ,0 /* 91 */ ,0 /* 92 */ ,0 /* 93 */ ,0 /* 94 */ ,0 /* 95 */ ,0 /* 96 */ ,0 /* 97 */ ,0 /* 98 */ ,0 /* 99 */ ,0 /* 100 */ ,0 /* 101 */ ,0 /* 102 */ ,0 /* 103 */ ,0 /* 104 */ ,0 /* 105 */ ,0 /* 106 */ ,0 /* 107 */ 
,0 /* 108 */ ,0 /* 109 */ ,0 /* 110 */ ,0 /* 111 */ ,0 /* 112 */ ,0 /* 113 */ ,0 /* 114 */ ,0 /* 115 */ ,0 /* 116 */ ,0 /* 117 */ ,0 /* 118 */ ,0 /* 119 */ ,0 /* 120 */ ,0 /* 121 */ ,0 /* 122 */ ,0 /* 123 */ ,0 /* 124 */ ,0 /* 125 */ ,0 /* 126 */ ,0 /* 127 */ ,0 /* 128 */ ,0 /* 129 */ ,0 /* 130 */ ,0 /* 131 */ ,0 /* 132 */ ,0 /* 133 */ ,0 /* 134 */ ,0 /* 135 */ ,0 /* 136 */ ,0 /* 137 */ ,0 /* 138 */ ,0 /* 139 */ ,0 /* 140 */ ,0 /* 141 */ ,0 /* 142 */ ,0 /* 143 */ ,0 /* 144 */ ,},{(ADDRESS)&Main_I3,264+(char*)&Main_m_M_Main_L_20},{0 /* 1 */ ,0 /* 2 */ ,0 /* 3 */ ,0 /* 4 */ ,0 /* 5 */ ,0 /* 6 */ ,0 /* 7 */ ,0 /* 8 */ ,},{(ADDRESS)&WideChar_I3,288+(char*)&Main_m_M_Main_L_20},{0 /* 1 */ ,0 /* 2 */ ,0 /* 3 */ ,0 /* 4 */ ,0 /* 5 */ ,0 /* 6 */ ,0 /* 7 */ ,0 /* 8 */ ,},{(ADDRESS)&Refany_I3,312+(char*)&Main_m_M_Main_L_20},{0 /* 1 */ ,0 /* 2 */ ,0 /* 3 */ ,0 /* 4 */ ,0 /* 5 */ ,0 /* 6 */ ,0 /* 7 */ ,0 /* 8 */ ,},{(ADDRESS)&Longint_I3,336+(char*)&Main_m_M_Main_L_20},{0 /* 1 */ ,0 /* 2 */ ,0 /* 3 */ 
,0 /* 4 */ ,0 /* 5 */ ,0 /* 6 */ ,0 /* 7 */ ,0 /* 8 */ ,},{(ADDRESS)&Integer_I3,360+(char*)&Main_m_M_Main_L_20},{0 /* 1 */ ,0 /* 2 */ ,0 /* 3 */ ,0 /* 4 */ ,0 /* 5 */ ,0 /* 6 */ ,0 /* 7 */ ,0 /* 8 */ ,},{(ADDRESS)&Char_I3,384+(char*)&Main_m_M_Main_L_20},{0 /* 1 */ ,0 /* 2 */ ,0 /* 3 */ ,0 /* 4 */ ,0 /* 5 */ ,0 /* 6 */ ,0 /* 7 */ ,0 /* 8 */ ,},{(ADDRESS)&Boolean_I3,408+(char*)&Main_m_M_Main_L_20},{0 /* 1 */ ,0 /* 2 */ ,0 /* 3 */ ,0 /* 4 */ ,0 /* 5 */ ,0 /* 6 */ ,0 /* 7 */ ,0 /* 8 */ ,},{(ADDRESS)&Address_I3,432+(char*)&Main_m_M_Main_L_20},{0 /* 1 */ ,0 /* 2 */ ,0 /* 3 */ ,0 /* 4 */ ,0 /* 5 */ ,0 /* 6 */ ,0 /* 7 */ ,0 /* 8 */ ,},{(ADDRESS)&AtomicWideChar_I3,456+(char*)&Main_m_M_Main_L_20},{0 /* 1 */ ,0 /* 2 */ ,0 /* 3 */ ,0 /* 4 */ ,0 /* 5 */ ,0 /* 6 */ ,0 /* 7 */ ,0 /* 8 */ ,},{(ADDRESS)&AtomicRefany_I3,480+(char*)&Main_m_M_Main_L_20},{0 /* 1 */ ,0 /* 2 */ ,0 /* 3 */ ,0 /* 4 */ ,0 /* 5 */ ,0 /* 6 */ ,0 /* 7 */ ,0 /* 8 */ ,},{(ADDRESS)&AtomicLongint_I3,504+(char*)&Main_m_M_Main_L_20},{0 /* 1 */ 
,0 /* 2 */ ,0 /* 3 */ ,0 /* 4 */ ,0 /* 5 */ ,0 /* 6 */ ,0 /* 7 */ ,0 /* 8 */ ,},{(ADDRESS)&AtomicInteger_I3,528+(char*)&Main_m_M_Main_L_20},{0 /* 1 */ ,0 /* 2 */ ,0 /* 3 */ ,0 /* 4 */ ,0 /* 5 */ ,0 /* 6 */ ,0 /* 7 */ ,0 /* 8 */ ,},{(ADDRESS)&AtomicChar_I3,552+(char*)&Main_m_M_Main_L_20},{0 /* 1 */ ,0 /* 2 */ ,0 /* 3 */ ,0 /* 4 */ ,0 /* 5 */ ,0 /* 6 */ ,0 /* 7 */ ,0 /* 8 */ ,},{(ADDRESS)&AtomicBoolean_I3,576+(char*)&Main_m_M_Main_L_20},{0 /* 1 */ ,0 /* 2 */ ,0 /* 3 */ ,0 /* 4 */ ,0 /* 5 */ ,0 /* 6 */ ,0 /* 7 */ ,0 /* 8 */ ,},{(ADDRESS)&AtomicAddress_I3,600+(char*)&Main_m_M_Main_L_20},{0 /* 1 */ ,0 /* 2 */ ,0 /* 3 */ ,0 /* 4 */ ,0 /* 5 */ ,0 /* 6 */ ,0 /* 7 */ ,0 /* 8 */ ,},{(ADDRESS)&RTHooks_I3},{0 /* 1 */ ,0 /* 2 */ ,0 /* 3 */ ,0 /* 4 */ ,0 /* 5 */ ,0 /* 6 */ ,0 /* 7 */ ,0 /* 8 */ ,0 /* 9 */ ,0 /* 10 */ ,0 /* 11 */ ,0 /* 12 */ ,0 /* 13 */ ,0 /* 14 */ ,0 /* 15 */ ,0 /* 16 */ ,},{INT64_(1355119930)}};
 /* end: segments/globals */
 /* begin: mark used */
 /* end: mark used */
 /* set_source_file */
 /* set_source_line */
#line 1 "../Main.m3"
 /* module global constants */
#line 1 "../Main.m3"
 /* module global data */
#line 1 "../Main.m3"
 /* set_source_line */
#line 1 "../Main.m3"
#line 552 "../Main.m3"
 /* Test_AtomicBoolean_Fence */
#line 552 "../Main.m3"
 /* set_source_line */
#line 552 "../Main.m3"
#line 23 "../Main.m3"
 /* begin_procedure */
#line 23 "../Main.m3"
struct Main__Test_AtomicBoolean_Fence_Frame_t {
#line 23 "../Main.m3"
ADDRESS _unused;
#line 23 "../Main.m3"
};
#line 23 "../Main.m3"
void /*TypeText3*/ 
__cdecl
Main__Test_AtomicBoolean_Fence(void)
{
#line 23 "../Main.m3"
Main__Test_AtomicBoolean_Fence_Frame_t _frame;
#line 23 "../Main.m3"
_frame._unused=(ADDRESS)&_frame;
#line 23 "../Main.m3"
 /* set_source_line */
#line 23 "../Main.m3"
#line 24 "../Main.m3"
 /* set_source_line */
#line 24 "../Main.m3"
#line 25 "../Main.m3"
 /* fence */
#line 25 "../Main.m3"
m3_fence();
#line 25 "../Main.m3"
 /* set_source_line */
#line 25 "../Main.m3"
#line 26 "../Main.m3"
 /* exit_proc */
#line 26 "../Main.m3"
return;
#line 26 "../Main.m3"
 /* end_procedure */
#line 26 "../Main.m3"
} /* Test_AtomicBoolean_CompareSwap */
#line 26 "../Main.m3"
 /* set_source_line */
#line 26 "../Main.m3"
#line 28 "../Main.m3"
 /* begin_procedure */
#line 28 "../Main.m3"
struct Main__Test_AtomicBoolean_CompareSwap_Frame_t {
#line 28 "../Main.m3"
ADDRESS _unused;
#line 28 "../Main.m3"
};
#line 28 "../Main.m3"
void /*TypeText3*/ 
__cdecl
Main__Test_AtomicBoolean_CompareSwap(void)
{
#line 28 "../Main.m3"
Main__Test_AtomicBoolean_CompareSwap_Frame_t _frame;
#line 28 "../Main.m3"
_frame._unused=(ADDRESS)&_frame;
#line 28 "../Main.m3"
 /* set_source_line */
#line 28 "../Main.m3"
#line 29 "../Main.m3"
 /* set_source_line */
#line 29 "../Main.m3"
#line 30 "../Main.m3"
 /* load_address */
#line 30 "../Main.m3"
 /* load_address */
#line 30 "../Main.m3"
 /* load */
#line 30 "../Main.m3"
 /* compare_exchange */
#line 30 "../Main.m3"
 /* store */
#line 30 "../Main.m3"
(*(UINT8*)((232)+(char*)(&Main_m_M_Main_L_20)))=(INT64)( ((INT64)(*((UINT8*)(INT64_(169)+((ADDRESS)(&Main_m_M_Main_L_20)))))));
#line 30 "../Main.m3"
 /* set_source_line */
#line 30 "../Main.m3"
#line 31 "../Main.m3"
 /* exit_proc */
#line 31 "../Main.m3"
return;
#line 31 "../Main.m3"
 /* end_procedure */
#line 31 "../Main.m3"
} /* Test_AtomicBoolean_FetchAnd */
#line 31 "../Main.m3"
 /* set_source_line */
#line 31 "../Main.m3"
#line 33 "../Main.m3"
 /* begin_procedure */
#line 33 "../Main.m3"
struct Main__Test_AtomicBoolean_FetchAnd_Frame_t {
#line 33 "../Main.m3"
ADDRESS _unused;
#line 33 "../Main.m3"
};
#line 33 "../Main.m3"
void /*TypeText3*/ 
__cdecl
Main__Test_AtomicBoolean_FetchAnd(void)
{
#line 33 "../Main.m3"
Main__Test_AtomicBoolean_FetchAnd_Frame_t _frame;
#line 33 "../Main.m3"
_frame._unused=(ADDRESS)&_frame;
#line 33 "../Main.m3"
 /* set_source_line */
#line 33 "../Main.m3"
#line 34 "../Main.m3"
 /* set_source_line */
#line 34 "../Main.m3"
#line 35 "../Main.m3"
 /* load_address */
#line 35 "../Main.m3"
 /* load */
#line 35 "../Main.m3"
 /* fetch_and_op */
#line 35 "../Main.m3"
 /* store */
#line 35 "../Main.m3"
(*(UINT8*)((169)+(char*)(&Main_m_M_Main_L_20)))=(INT64)( ((INT64)(*((UINT8*)(INT64_(168)+((ADDRESS)(&Main_m_M_Main_L_20)))))));
#line 35 "../Main.m3"
 /* set_source_line */
#line 35 "../Main.m3"
#line 36 "../Main.m3"
 /* exit_proc */
#line 36 "../Main.m3"
return;
#line 36 "../Main.m3"
 /* end_procedure */
#line 36 "../Main.m3"
} /* Test_AtomicBoolean_FetchDec */
#line 36 "../Main.m3"
 /* set_source_line */
#line 36 "../Main.m3"
#line 38 "../Main.m3"
 /* begin_procedure */
#line 38 "../Main.m3"
struct Main__Test_AtomicBoolean_FetchDec_Frame_t {
#line 38 "../Main.m3"
ADDRESS _unused;
#line 38 "../Main.m3"
};
#line 38 "../Main.m3"
void /*TypeText3*/ 
__cdecl
Main__Test_AtomicBoolean_FetchDec(void)
{
#line 38 "../Main.m3"
Main__Test_AtomicBoolean_FetchDec_Frame_t _frame;
#line 38 "../Main.m3"
_frame._unused=(ADDRESS)&_frame;
#line 38 "../Main.m3"
 /* set_source_line */
#line 38 "../Main.m3"
#line 39 "../Main.m3"
 /* set_source_line */
#line 39 "../Main.m3"
#line 40 "../Main.m3"
 /* load_address */
#line 40 "../Main.m3"
 /* load_integer */
#line 40 "../Main.m3"
 /* fetch_and_op */
#line 40 "../Main.m3"
 /* store */
#line 40 "../Main.m3"
(*(UINT8*)((169)+(char*)(&Main_m_M_Main_L_20)))=(INT64)(  INT64_(1));
#line 40 "../Main.m3"
 /* set_source_line */
#line 40 "../Main.m3"
#line 41 "../Main.m3"
 /* exit_proc */
#line 41 "../Main.m3"
return;
#line 41 "../Main.m3"
 /* end_procedure */
#line 41 "../Main.m3"
} /* Test_AtomicBoolean_FetchInc */
#line 41 "../Main.m3"
 /* set_source_line */
#line 41 "../Main.m3"
#line 43 "../Main.m3"
 /* begin_procedure */
#line 43 "../Main.m3"
struct Main__Test_AtomicBoolean_FetchInc_Frame_t {
#line 43 "../Main.m3"
ADDRESS _unused;
#line 43 "../Main.m3"
};
#line 43 "../Main.m3"
void /*TypeText3*/ 
__cdecl
Main__Test_AtomicBoolean_FetchInc(void)
{
#line 43 "../Main.m3"
Main__Test_AtomicBoolean_FetchInc_Frame_t _frame;
#line 43 "../Main.m3"
_frame._unused=(ADDRESS)&_frame;
#line 43 "../Main.m3"
 /* set_source_line */
#line 43 "../Main.m3"
#line 44 "../Main.m3"
 /* set_source_line */
#line 44 "../Main.m3"
#line 45 "../Main.m3"
 /* load_address */
#line 45 "../Main.m3"
 /* load_integer */
#line 45 "../Main.m3"
 /* fetch_and_op */
#line 45 "../Main.m3"
 /* store */
#line 45 "../Main.m3"
(*(UINT8*)((169)+(char*)(&Main_m_M_Main_L_20)))=(INT64)(  INT64_(1));
#line 45 "../Main.m3"
 /* set_source_line */
#line 45 "../Main.m3"
#line 46 "../Main.m3"
 /* exit_proc */
#line 46 "../Main.m3"
return;
#line 46 "../Main.m3"
 /* end_procedure */
#line 46 "../Main.m3"
} /* Test_AtomicBoolean_FetchOr */
#line 46 "../Main.m3"
 /* set_source_line */
#line 46 "../Main.m3"
#line 48 "../Main.m3"
 /* begin_procedure */
#line 48 "../Main.m3"
struct Main__Test_AtomicBoolean_FetchOr_Frame_t {
#line 48 "../Main.m3"
ADDRESS _unused;
#line 48 "../Main.m3"
};
#line 48 "../Main.m3"
void /*TypeText3*/ 
__cdecl
Main__Test_AtomicBoolean_FetchOr(void)
{
#line 48 "../Main.m3"
Main__Test_AtomicBoolean_FetchOr_Frame_t _frame;
#line 48 "../Main.m3"
_frame._unused=(ADDRESS)&_frame;
#line 48 "../Main.m3"
 /* set_source_line */
#line 48 "../Main.m3"
#line 49 "../Main.m3"
 /* set_source_line */
#line 49 "../Main.m3"
#line 50 "../Main.m3"
 /* load_address */
#line 50 "../Main.m3"
 /* load */
#line 50 "../Main.m3"
 /* fetch_and_op */
#line 50 "../Main.m3"
 /* store */
#line 50 "../Main.m3"
(*(UINT8*)((169)+(char*)(&Main_m_M_Main_L_20)))=(INT64)( ((INT64)(*((UINT8*)(INT64_(168)+((ADDRESS)(&Main_m_M_Main_L_20)))))));
#line 50 "../Main.m3"
 /* set_source_line */
#line 50 "../Main.m3"
#line 51 "../Main.m3"
 /* exit_proc */
#line 51 "../Main.m3"
return;
#line 51 "../Main.m3"
 /* end_procedure */
#line 51 "../Main.m3"
} /* Test_AtomicBoolean_FetchXor */
#line 51 "../Main.m3"
 /* set_source_line */
#line 51 "../Main.m3"
#line 53 "../Main.m3"
 /* begin_procedure */
#line 53 "../Main.m3"
struct Main__Test_AtomicBoolean_FetchXor_Frame_t {
#line 53 "../Main.m3"
ADDRESS _unused;
#line 53 "../Main.m3"
};
#line 53 "../Main.m3"
void /*TypeText3*/ 
__cdecl
Main__Test_AtomicBoolean_FetchXor(void)
{
#line 53 "../Main.m3"
Main__Test_AtomicBoolean_FetchXor_Frame_t _frame;
#line 53 "../Main.m3"
_frame._unused=(ADDRESS)&_frame;
#line 53 "../Main.m3"
 /* set_source_line */
#line 53 "../Main.m3"
#line 54 "../Main.m3"
 /* set_source_line */
#line 54 "../Main.m3"
#line 55 "../Main.m3"
 /* load_address */
#line 55 "../Main.m3"
 /* load */
#line 55 "../Main.m3"
 /* fetch_and_op */
#line 55 "../Main.m3"
 /* store */
#line 55 "../Main.m3"
(*(UINT8*)((169)+(char*)(&Main_m_M_Main_L_20)))=(INT64)( ((INT64)(*((UINT8*)(INT64_(168)+((ADDRESS)(&Main_m_M_Main_L_20)))))));
#line 55 "../Main.m3"
 /* set_source_line */
#line 55 "../Main.m3"
#line 56 "../Main.m3"
 /* exit_proc */
#line 56 "../Main.m3"
return;
#line 56 "../Main.m3"
 /* end_procedure */
#line 56 "../Main.m3"
} /* Test_AtomicBoolean_IsLockFree */
#line 56 "../Main.m3"
 /* set_source_line */
#line 56 "../Main.m3"
#line 58 "../Main.m3"
 /* begin_procedure */
#line 58 "../Main.m3"
struct Main__Test_AtomicBoolean_IsLockFree_Frame_t {
#line 58 "../Main.m3"
ADDRESS _unused;
#line 58 "../Main.m3"
};
#line 58 "../Main.m3"
BOOLEAN
__cdecl
Main__Test_AtomicBoolean_IsLockFree(void)
{
#line 58 "../Main.m3"
Main__Test_AtomicBoolean_IsLockFree_Frame_t _frame;
#line 58 "../Main.m3"
_frame._unused=(ADDRESS)&_frame;
#line 58 "../Main.m3"
 /* set_source_line */
#line 58 "../Main.m3"
#line 59 "../Main.m3"
 /* set_source_line */
#line 59 "../Main.m3"
#line 60 "../Main.m3"
 /* load_integer */
#line 60 "../Main.m3"
 /* exit_proc */
#line 60 "../Main.m3"
return  INT64_(1);
#line 60 "../Main.m3"
 /* end_procedure */
#line 60 "../Main.m3"
} /* Test_AtomicBoolean_LoadStore */
#line 60 "../Main.m3"
 /* set_source_line */
#line 60 "../Main.m3"
#line 63 "../Main.m3"
 /* begin_procedure */
#line 63 "../Main.m3"
struct Main__Test_AtomicBoolean_LoadStore_Frame_t {
#line 63 "../Main.m3"
ADDRESS _unused;
#line 63 "../Main.m3"
};
#line 63 "../Main.m3"
void /*TypeText3*/ 
__cdecl
Main__Test_AtomicBoolean_LoadStore(void)
{
#line 63 "../Main.m3"
Main__Test_AtomicBoolean_LoadStore_Frame_t _frame;
#line 63 "../Main.m3"
_frame._unused=(ADDRESS)&_frame;
#line 63 "../Main.m3"
 /* set_source_line */
#line 63 "../Main.m3"
#line 64 "../Main.m3"
 /* set_source_line */
#line 64 "../Main.m3"
#line 65 "../Main.m3"
 /* load_address */
#line 65 "../Main.m3"
 /* load_ordered */
#line 65 "../Main.m3"
 /* load_indirect */
#line 65 "../Main.m3"
 /* store */
#line 65 "../Main.m3"
(*(UINT8*)((168)+(char*)(&Main_m_M_Main_L_20)))=(INT64)( ((INT64)(*((UINT8*)(INT64_(104)+((ADDRESS)(&Main_m_M_Main_L_20)))))));
#line 65 "../Main.m3"
 /* set_source_line */
#line 65 "../Main.m3"
#line 66 "../Main.m3"
 /* load_address */
#line 66 "../Main.m3"
 /* load_integer */
#line 66 "../Main.m3"
 /* store_ordered => store_helper */
#line 66 "../Main.m3"
(*(UINT8*)(INT64_(104)+((ADDRESS)(&Main_m_M_Main_L_20))))=(INT64)( INT64_(0));
#line 66 "../Main.m3"
 /* set_source_line */
#line 66 "../Main.m3"
#line 67 "../Main.m3"
 /* load_address */
#line 67 "../Main.m3"
 /* load_ordered */
#line 67 "../Main.m3"
 /* load_indirect */
#line 67 "../Main.m3"
 /* store */
#line 67 "../Main.m3"
(*(UINT8*)((168)+(char*)(&Main_m_M_Main_L_20)))=(INT64)( ((INT64)(*((UINT8*)(INT64_(104)+((ADDRESS)(&Main_m_M_Main_L_20)))))));
#line 67 "../Main.m3"
 /* set_source_line */
#line 67 "../Main.m3"
#line 68 "../Main.m3"
 /* load_address */
#line 68 "../Main.m3"
 /* load_integer */
#line 68 "../Main.m3"
 /* store_ordered => store_helper */
#line 68 "../Main.m3"
(*(UINT8*)(INT64_(104)+((ADDRESS)(&Main_m_M_Main_L_20))))=(INT64)( INT64_(1));
#line 68 "../Main.m3"
 /* set_source_line */
#line 68 "../Main.m3"
#line 70 "../Main.m3"
 /* load_address */
#line 70 "../Main.m3"
 /* load_ordered */
#line 70 "../Main.m3"
 /* load_indirect */
#line 70 "../Main.m3"
 /* store */
#line 70 "../Main.m3"
(*(UINT8*)((169)+(char*)(&Main_m_M_Main_L_20)))=(INT64)( ((INT64)(*((UINT8*)(INT64_(104)+((ADDRESS)(&Main_m_M_Main_L_20)))))));
#line 70 "../Main.m3"
 /* set_source_line */
#line 70 "../Main.m3"
#line 71 "../Main.m3"
 /* load_address */
#line 71 "../Main.m3"
 /* load_ordered */
#line 71 "../Main.m3"
 /* load_indirect */
#line 71 "../Main.m3"
 /* store */
#line 71 "../Main.m3"
(*(UINT8*)((169)+(char*)(&Main_m_M_Main_L_20)))=(INT64)( ((INT64)(*((UINT8*)(INT64_(104)+((ADDRESS)(&Main_m_M_Main_L_20)))))));
#line 71 "../Main.m3"
 /* set_source_line */
#line 71 "../Main.m3"
#line 72 "../Main.m3"
 /* exit_proc */
#line 72 "../Main.m3"
return;
#line 72 "../Main.m3"
 /* end_procedure */
#line 72 "../Main.m3"
} /* Test_AtomicBoolean_Swap */
#line 72 "../Main.m3"
 /* set_source_line */
#line 72 "../Main.m3"
#line 74 "../Main.m3"
 /* begin_procedure */
#line 74 "../Main.m3"
struct Main__Test_AtomicBoolean_Swap_Frame_t {
#line 74 "../Main.m3"
ADDRESS _unused;
#line 74 "../Main.m3"
};
#line 74 "../Main.m3"
void /*TypeText3*/ 
__cdecl
Main__Test_AtomicBoolean_Swap(void)
{
#line 74 "../Main.m3"
Main__Test_AtomicBoolean_Swap_Frame_t _frame;
#line 74 "../Main.m3"
_frame._unused=(ADDRESS)&_frame;
#line 74 "../Main.m3"
 /* set_source_line */
#line 74 "../Main.m3"
#line 75 "../Main.m3"
 /* set_source_line */
#line 75 "../Main.m3"
#line 76 "../Main.m3"
 /* load_address */
#line 76 "../Main.m3"
 /* load */
#line 76 "../Main.m3"
 /* exchange */
#line 76 "../Main.m3"
 /* store */
#line 76 "../Main.m3"
(*(UINT8*)((169)+(char*)(&Main_m_M_Main_L_20)))=(INT64)( ((INT64)(*((UINT8*)(INT64_(168)+((ADDRESS)(&Main_m_M_Main_L_20)))))));
#line 76 "../Main.m3"
 /* set_source_line */
#line 76 "../Main.m3"
#line 77 "../Main.m3"
 /* exit_proc */
#line 77 "../Main.m3"
return;
#line 77 "../Main.m3"
 /* end_procedure */
#line 77 "../Main.m3"
} /* Test_AtomicBoolean */
#line 77 "../Main.m3"
 /* set_source_line */
#line 77 "../Main.m3"
#line 79 "../Main.m3"
 /* begin_procedure */
#line 79 "../Main.m3"
struct Main__Test_AtomicBoolean_Frame_t {
#line 79 "../Main.m3"
ADDRESS _unused;
#line 79 "../Main.m3"
};
#line 79 "../Main.m3"
void /*TypeText3*/ 
__cdecl
Main__Test_AtomicBoolean(void)
{
#line 79 "../Main.m3"
 /* Var_Type2 */ INT64 Main_m_34_L_35={0};//always-init
#line 79 "../Main.m3"
Main__Test_AtomicBoolean_Frame_t _frame;
#line 79 "../Main.m3"
_frame._unused=(ADDRESS)&_frame;
#line 79 "../Main.m3"
 /* set_source_line */
#line 79 "../Main.m3"
#line 80 "../Main.m3"
 /* set_source_line */
#line 80 "../Main.m3"
#line 81 "../Main.m3"
 /* start_call_direct */
#line 81 "../Main.m3"
 /* call_direct */
#line 81 "../Main.m3"
Main__Test_AtomicBoolean_Fence(
 );
#line 81 "../Main.m3"
 /* set_source_line */
#line 81 "../Main.m3"
#line 82 "../Main.m3"
 /* start_call_direct */
#line 82 "../Main.m3"
 /* call_direct */
#line 82 "../Main.m3"
Main__Test_AtomicBoolean_CompareSwap(
 );
#line 82 "../Main.m3"
 /* set_source_line */
#line 82 "../Main.m3"
#line 83 "../Main.m3"
 /* start_call_direct */
#line 83 "../Main.m3"
 /* call_direct */
#line 83 "../Main.m3"
Main__Test_AtomicBoolean_FetchAnd(
 );
#line 83 "../Main.m3"
 /* set_source_line */
#line 83 "../Main.m3"
#line 84 "../Main.m3"
 /* start_call_direct */
#line 84 "../Main.m3"
 /* call_direct */
#line 84 "../Main.m3"
Main__Test_AtomicBoolean_FetchDec(
 );
#line 84 "../Main.m3"
 /* set_source_line */
#line 84 "../Main.m3"
#line 85 "../Main.m3"
 /* start_call_direct */
#line 85 "../Main.m3"
 /* call_direct */
#line 85 "../Main.m3"
Main__Test_AtomicBoolean_FetchInc(
 );
#line 85 "../Main.m3"
 /* set_source_line */
#line 85 "../Main.m3"
#line 86 "../Main.m3"
 /* start_call_direct */
#line 86 "../Main.m3"
 /* call_direct */
#line 86 "../Main.m3"
Main__Test_AtomicBoolean_FetchOr(
 );
#line 86 "../Main.m3"
 /* set_source_line */
#line 86 "../Main.m3"
#line 87 "../Main.m3"
 /* start_call_direct */
#line 87 "../Main.m3"
 /* call_direct */
#line 87 "../Main.m3"
Main__Test_AtomicBoolean_FetchXor(
 );
#line 87 "../Main.m3"
 /* set_source_line */
#line 87 "../Main.m3"
#line 88 "../Main.m3"
 /* start_call_direct */
#line 88 "../Main.m3"
 /* call_direct */
#line 88 "../Main.m3"
 /* store */
#line 88 "../Main.m3"
(*(INT64*)(&Main_m_34_L_35))=(INT64)(((INT64)(Main__Test_AtomicBoolean_IsLockFree(
 ))));
#line 88 "../Main.m3"
 /* set_source_line */
#line 88 "../Main.m3"
#line 89 "../Main.m3"
 /* start_call_direct */
#line 89 "../Main.m3"
 /* call_direct */
#line 89 "../Main.m3"
Main__Test_AtomicBoolean_LoadStore(
 );
#line 89 "../Main.m3"
 /* set_source_line */
#line 89 "../Main.m3"
#line 90 "../Main.m3"
 /* start_call_direct */
#line 90 "../Main.m3"
 /* call_direct */
#line 90 "../Main.m3"
Main__Test_AtomicBoolean_Swap(
 );
#line 90 "../Main.m3"
 /* set_source_line */
#line 90 "../Main.m3"
#line 91 "../Main.m3"
 /* exit_proc */
#line 91 "../Main.m3"
return;
#line 91 "../Main.m3"
 /* end_procedure */
#line 91 "../Main.m3"
} /* Test_AtomicChar_Fence */
#line 91 "../Main.m3"
 /* set_source_line */
#line 91 "../Main.m3"
#line 95 "../Main.m3"
 /* begin_procedure */
#line 95 "../Main.m3"
struct Main__Test_AtomicChar_Fence_Frame_t {
#line 95 "../Main.m3"
ADDRESS _unused;
#line 95 "../Main.m3"
};
#line 95 "../Main.m3"
void /*TypeText3*/ 
__cdecl
Main__Test_AtomicChar_Fence(void)
{
#line 95 "../Main.m3"
Main__Test_AtomicChar_Fence_Frame_t _frame;
#line 95 "../Main.m3"
_frame._unused=(ADDRESS)&_frame;
#line 95 "../Main.m3"
 /* set_source_line */
#line 95 "../Main.m3"
#line 96 "../Main.m3"
 /* set_source_line */
#line 96 "../Main.m3"
#line 97 "../Main.m3"
 /* fence */
#line 97 "../Main.m3"
m3_fence();
#line 97 "../Main.m3"
 /* set_source_line */
#line 97 "../Main.m3"
#line 98 "../Main.m3"
 /* exit_proc */
#line 98 "../Main.m3"
return;
#line 98 "../Main.m3"
 /* end_procedure */
#line 98 "../Main.m3"
} /* Test_AtomicChar_CompareSwap */
#line 98 "../Main.m3"
 /* set_source_line */
#line 98 "../Main.m3"
#line 100 "../Main.m3"
 /* begin_procedure */
#line 100 "../Main.m3"
struct Main__Test_AtomicChar_CompareSwap_Frame_t {
#line 100 "../Main.m3"
ADDRESS _unused;
#line 100 "../Main.m3"
};
#line 100 "../Main.m3"
void /*TypeText3*/ 
__cdecl
Main__Test_AtomicChar_CompareSwap(void)
{
#line 100 "../Main.m3"
Main__Test_AtomicChar_CompareSwap_Frame_t _frame;
#line 100 "../Main.m3"
_frame._unused=(ADDRESS)&_frame;
#line 100 "../Main.m3"
 /* set_source_line */
#line 100 "../Main.m3"
#line 101 "../Main.m3"
 /* set_source_line */
#line 101 "../Main.m3"
#line 102 "../Main.m3"
 /* load_address */
#line 102 "../Main.m3"
 /* load_address */
#line 102 "../Main.m3"
 /* load */
#line 102 "../Main.m3"
 /* compare_exchange */
#line 102 "../Main.m3"
 /* store */
#line 102 "../Main.m3"
(*(UINT8*)((232)+(char*)(&Main_m_M_Main_L_20)))=(INT64)( ((INT64)(*((UINT8*)(INT64_(171)+((ADDRESS)(&Main_m_M_Main_L_20)))))));
#line 102 "../Main.m3"
 /* set_source_line */
#line 102 "../Main.m3"
#line 103 "../Main.m3"
 /* exit_proc */
#line 103 "../Main.m3"
return;
#line 103 "../Main.m3"
 /* end_procedure */
#line 103 "../Main.m3"
} /* Test_AtomicChar_FetchAnd */
#line 103 "../Main.m3"
 /* set_source_line */
#line 103 "../Main.m3"
#line 105 "../Main.m3"
 /* begin_procedure */
#line 105 "../Main.m3"
struct Main__Test_AtomicChar_FetchAnd_Frame_t {
#line 105 "../Main.m3"
ADDRESS _unused;
#line 105 "../Main.m3"
};
#line 105 "../Main.m3"
void /*TypeText3*/ 
__cdecl
Main__Test_AtomicChar_FetchAnd(void)
{
#line 105 "../Main.m3"
Main__Test_AtomicChar_FetchAnd_Frame_t _frame;
#line 105 "../Main.m3"
_frame._unused=(ADDRESS)&_frame;
#line 105 "../Main.m3"
 /* set_source_line */
#line 105 "../Main.m3"
#line 106 "../Main.m3"
 /* set_source_line */
#line 106 "../Main.m3"
#line 107 "../Main.m3"
 /* load_address */
#line 107 "../Main.m3"
 /* load */
#line 107 "../Main.m3"
 /* fetch_and_op */
#line 107 "../Main.m3"
 /* store */
#line 107 "../Main.m3"
(*(UINT8*)((171)+(char*)(&Main_m_M_Main_L_20)))=(INT64)( ((INT64)(*((UINT8*)(INT64_(170)+((ADDRESS)(&Main_m_M_Main_L_20)))))));
#line 107 "../Main.m3"
 /* set_source_line */
#line 107 "../Main.m3"
#line 108 "../Main.m3"
 /* exit_proc */
#line 108 "../Main.m3"
return;
#line 108 "../Main.m3"
 /* end_procedure */
#line 108 "../Main.m3"
} /* Test_AtomicChar_FetchDec */
#line 108 "../Main.m3"
 /* set_source_line */
#line 108 "../Main.m3"
#line 110 "../Main.m3"
 /* begin_procedure */
#line 110 "../Main.m3"
struct Main__Test_AtomicChar_FetchDec_Frame_t {
#line 110 "../Main.m3"
ADDRESS _unused;
#line 110 "../Main.m3"
};
#line 110 "../Main.m3"
void /*TypeText3*/ 
__cdecl
Main__Test_AtomicChar_FetchDec(void)
{
#line 110 "../Main.m3"
Main__Test_AtomicChar_FetchDec_Frame_t _frame;
#line 110 "../Main.m3"
_frame._unused=(ADDRESS)&_frame;
#line 110 "../Main.m3"
 /* set_source_line */
#line 110 "../Main.m3"
#line 111 "../Main.m3"
 /* set_source_line */
#line 111 "../Main.m3"
#line 112 "../Main.m3"
 /* load_address */
#line 112 "../Main.m3"
 /* load_integer */
#line 112 "../Main.m3"
 /* fetch_and_op */
#line 112 "../Main.m3"
 /* store */
#line 112 "../Main.m3"
(*(UINT8*)((171)+(char*)(&Main_m_M_Main_L_20)))=(INT64)(  INT64_(1));
#line 112 "../Main.m3"
 /* set_source_line */
#line 112 "../Main.m3"
#line 113 "../Main.m3"
 /* exit_proc */
#line 113 "../Main.m3"
return;
#line 113 "../Main.m3"
 /* end_procedure */
#line 113 "../Main.m3"
} /* Test_AtomicChar_FetchInc */
#line 113 "../Main.m3"
 /* set_source_line */
#line 113 "../Main.m3"
#line 115 "../Main.m3"
 /* begin_procedure */
#line 115 "../Main.m3"
struct Main__Test_AtomicChar_FetchInc_Frame_t {
#line 115 "../Main.m3"
ADDRESS _unused;
#line 115 "../Main.m3"
};
#line 115 "../Main.m3"
void /*TypeText3*/ 
__cdecl
Main__Test_AtomicChar_FetchInc(void)
{
#line 115 "../Main.m3"
Main__Test_AtomicChar_FetchInc_Frame_t _frame;
#line 115 "../Main.m3"
_frame._unused=(ADDRESS)&_frame;
#line 115 "../Main.m3"
 /* set_source_line */
#line 115 "../Main.m3"
#line 116 "../Main.m3"
 /* set_source_line */
#line 116 "../Main.m3"
#line 117 "../Main.m3"
 /* load_address */
#line 117 "../Main.m3"
 /* load_integer */
#line 117 "../Main.m3"
 /* fetch_and_op */
#line 117 "../Main.m3"
 /* store */
#line 117 "../Main.m3"
(*(UINT8*)((171)+(char*)(&Main_m_M_Main_L_20)))=(INT64)(  INT64_(1));
#line 117 "../Main.m3"
 /* set_source_line */
#line 117 "../Main.m3"
#line 118 "../Main.m3"
 /* exit_proc */
#line 118 "../Main.m3"
return;
#line 118 "../Main.m3"
 /* end_procedure */
#line 118 "../Main.m3"
} /* Test_AtomicChar_FetchOr */
#line 118 "../Main.m3"
 /* set_source_line */
#line 118 "../Main.m3"
#line 120 "../Main.m3"
 /* begin_procedure */
#line 120 "../Main.m3"
struct Main__Test_AtomicChar_FetchOr_Frame_t {
#line 120 "../Main.m3"
ADDRESS _unused;
#line 120 "../Main.m3"
};
#line 120 "../Main.m3"
void /*TypeText3*/ 
__cdecl
Main__Test_AtomicChar_FetchOr(void)
{
#line 120 "../Main.m3"
Main__Test_AtomicChar_FetchOr_Frame_t _frame;
#line 120 "../Main.m3"
_frame._unused=(ADDRESS)&_frame;
#line 120 "../Main.m3"
 /* set_source_line */
#line 120 "../Main.m3"
#line 121 "../Main.m3"
 /* set_source_line */
#line 121 "../Main.m3"
#line 122 "../Main.m3"
 /* load_address */
#line 122 "../Main.m3"
 /* load */
#line 122 "../Main.m3"
 /* fetch_and_op */
#line 122 "../Main.m3"
 /* store */
#line 122 "../Main.m3"
(*(UINT8*)((171)+(char*)(&Main_m_M_Main_L_20)))=(INT64)( ((INT64)(*((UINT8*)(INT64_(170)+((ADDRESS)(&Main_m_M_Main_L_20)))))));
#line 122 "../Main.m3"
 /* set_source_line */
#line 122 "../Main.m3"
#line 123 "../Main.m3"
 /* exit_proc */
#line 123 "../Main.m3"
return;
#line 123 "../Main.m3"
 /* end_procedure */
#line 123 "../Main.m3"
} /* Test_AtomicChar_FetchXor */
#line 123 "../Main.m3"
 /* set_source_line */
#line 123 "../Main.m3"
#line 125 "../Main.m3"
 /* begin_procedure */
#line 125 "../Main.m3"
struct Main__Test_AtomicChar_FetchXor_Frame_t {
#line 125 "../Main.m3"
ADDRESS _unused;
#line 125 "../Main.m3"
};
#line 125 "../Main.m3"
void /*TypeText3*/ 
__cdecl
Main__Test_AtomicChar_FetchXor(void)
{
#line 125 "../Main.m3"
Main__Test_AtomicChar_FetchXor_Frame_t _frame;
#line 125 "../Main.m3"
_frame._unused=(ADDRESS)&_frame;
#line 125 "../Main.m3"
 /* set_source_line */
#line 125 "../Main.m3"
#line 126 "../Main.m3"
 /* set_source_line */
#line 126 "../Main.m3"
#line 127 "../Main.m3"
 /* load_address */
#line 127 "../Main.m3"
 /* load */
#line 127 "../Main.m3"
 /* fetch_and_op */
#line 127 "../Main.m3"
 /* store */
#line 127 "../Main.m3"
(*(UINT8*)((171)+(char*)(&Main_m_M_Main_L_20)))=(INT64)( ((INT64)(*((UINT8*)(INT64_(170)+((ADDRESS)(&Main_m_M_Main_L_20)))))));
#line 127 "../Main.m3"
 /* set_source_line */
#line 127 "../Main.m3"
#line 128 "../Main.m3"
 /* exit_proc */
#line 128 "../Main.m3"
return;
#line 128 "../Main.m3"
 /* end_procedure */
#line 128 "../Main.m3"
} /* Test_AtomicChar_IsLockFree */
#line 128 "../Main.m3"
 /* set_source_line */
#line 128 "../Main.m3"
#line 130 "../Main.m3"
 /* begin_procedure */
#line 130 "../Main.m3"
struct Main__Test_AtomicChar_IsLockFree_Frame_t {
#line 130 "../Main.m3"
ADDRESS _unused;
#line 130 "../Main.m3"
};
#line 130 "../Main.m3"
BOOLEAN
__cdecl
Main__Test_AtomicChar_IsLockFree(void)
{
#line 130 "../Main.m3"
Main__Test_AtomicChar_IsLockFree_Frame_t _frame;
#line 130 "../Main.m3"
_frame._unused=(ADDRESS)&_frame;
#line 130 "../Main.m3"
 /* set_source_line */
#line 130 "../Main.m3"
#line 131 "../Main.m3"
 /* set_source_line */
#line 131 "../Main.m3"
#line 132 "../Main.m3"
 /* load_integer */
#line 132 "../Main.m3"
 /* exit_proc */
#line 132 "../Main.m3"
return  INT64_(1);
#line 132 "../Main.m3"
 /* end_procedure */
#line 132 "../Main.m3"
} /* Test_AtomicChar_LoadStore */
#line 132 "../Main.m3"
 /* set_source_line */
#line 132 "../Main.m3"
#line 135 "../Main.m3"
 /* begin_procedure */
#line 135 "../Main.m3"
struct Main__Test_AtomicChar_LoadStore_Frame_t {
#line 135 "../Main.m3"
ADDRESS _unused;
#line 135 "../Main.m3"
};
#line 135 "../Main.m3"
void /*TypeText3*/ 
__cdecl
Main__Test_AtomicChar_LoadStore(void)
{
#line 135 "../Main.m3"
Main__Test_AtomicChar_LoadStore_Frame_t _frame;
#line 135 "../Main.m3"
_frame._unused=(ADDRESS)&_frame;
#line 135 "../Main.m3"
 /* set_source_line */
#line 135 "../Main.m3"
#line 136 "../Main.m3"
 /* set_source_line */
#line 136 "../Main.m3"
#line 137 "../Main.m3"
 /* load_address */
#line 137 "../Main.m3"
 /* load_ordered */
#line 137 "../Main.m3"
 /* load_indirect */
#line 137 "../Main.m3"
 /* store */
#line 137 "../Main.m3"
(*(UINT8*)((170)+(char*)(&Main_m_M_Main_L_20)))=(INT64)( ((INT64)(*((UINT8*)(INT64_(105)+((ADDRESS)(&Main_m_M_Main_L_20)))))));
#line 137 "../Main.m3"
 /* set_source_line */
#line 137 "../Main.m3"
#line 138 "../Main.m3"
 /* load_address */
#line 138 "../Main.m3"
 /* load_integer */
#line 138 "../Main.m3"
 /* store_ordered => store_helper */
#line 138 "../Main.m3"
(*(UINT8*)(INT64_(105)+((ADDRESS)(&Main_m_M_Main_L_20))))=(INT64)( INT64_(6));
#line 138 "../Main.m3"
 /* set_source_line */
#line 138 "../Main.m3"
#line 139 "../Main.m3"
 /* load_address */
#line 139 "../Main.m3"
 /* load_ordered */
#line 139 "../Main.m3"
 /* load_indirect */
#line 139 "../Main.m3"
 /* store */
#line 139 "../Main.m3"
(*(UINT8*)((170)+(char*)(&Main_m_M_Main_L_20)))=(INT64)( ((INT64)(*((UINT8*)(INT64_(105)+((ADDRESS)(&Main_m_M_Main_L_20)))))));
#line 139 "../Main.m3"
 /* set_source_line */
#line 139 "../Main.m3"
#line 140 "../Main.m3"
 /* load_address */
#line 140 "../Main.m3"
 /* load_integer */
#line 140 "../Main.m3"
 /* store_ordered => store_helper */
#line 140 "../Main.m3"
(*(UINT8*)(INT64_(105)+((ADDRESS)(&Main_m_M_Main_L_20))))=(INT64)( INT64_(6));
#line 140 "../Main.m3"
 /* set_source_line */
#line 140 "../Main.m3"
#line 142 "../Main.m3"
 /* load_address */
#line 142 "../Main.m3"
 /* load_ordered */
#line 142 "../Main.m3"
 /* load_indirect */
#line 142 "../Main.m3"
 /* store */
#line 142 "../Main.m3"
(*(UINT8*)((171)+(char*)(&Main_m_M_Main_L_20)))=(INT64)( ((INT64)(*((UINT8*)(INT64_(105)+((ADDRESS)(&Main_m_M_Main_L_20)))))));
#line 142 "../Main.m3"
 /* set_source_line */
#line 142 "../Main.m3"
#line 143 "../Main.m3"
 /* load_address */
#line 143 "../Main.m3"
 /* load_ordered */
#line 143 "../Main.m3"
 /* load_indirect */
#line 143 "../Main.m3"
 /* store */
#line 143 "../Main.m3"
(*(UINT8*)((171)+(char*)(&Main_m_M_Main_L_20)))=(INT64)( ((INT64)(*((UINT8*)(INT64_(105)+((ADDRESS)(&Main_m_M_Main_L_20)))))));
#line 143 "../Main.m3"
 /* set_source_line */
#line 143 "../Main.m3"
#line 144 "../Main.m3"
 /* exit_proc */
#line 144 "../Main.m3"
return;
#line 144 "../Main.m3"
 /* end_procedure */
#line 144 "../Main.m3"
} /* Test_AtomicChar_Swap */
#line 144 "../Main.m3"
 /* set_source_line */
#line 144 "../Main.m3"
#line 146 "../Main.m3"
 /* begin_procedure */
#line 146 "../Main.m3"
struct Main__Test_AtomicChar_Swap_Frame_t {
#line 146 "../Main.m3"
ADDRESS _unused;
#line 146 "../Main.m3"
};
#line 146 "../Main.m3"
void /*TypeText3*/ 
__cdecl
Main__Test_AtomicChar_Swap(void)
{
#line 146 "../Main.m3"
Main__Test_AtomicChar_Swap_Frame_t _frame;
#line 146 "../Main.m3"
_frame._unused=(ADDRESS)&_frame;
#line 146 "../Main.m3"
 /* set_source_line */
#line 146 "../Main.m3"
#line 147 "../Main.m3"
 /* set_source_line */
#line 147 "../Main.m3"
#line 148 "../Main.m3"
 /* load_address */
#line 148 "../Main.m3"
 /* load */
#line 148 "../Main.m3"
 /* exchange */
#line 148 "../Main.m3"
 /* store */
#line 148 "../Main.m3"
(*(UINT8*)((171)+(char*)(&Main_m_M_Main_L_20)))=(INT64)( ((INT64)(*((UINT8*)(INT64_(170)+((ADDRESS)(&Main_m_M_Main_L_20)))))));
#line 148 "../Main.m3"
 /* set_source_line */
#line 148 "../Main.m3"
#line 149 "../Main.m3"
 /* exit_proc */
#line 149 "../Main.m3"
return;
#line 149 "../Main.m3"
 /* end_procedure */
#line 149 "../Main.m3"
} /* Test_AtomicChar */
#line 149 "../Main.m3"
 /* set_source_line */
#line 149 "../Main.m3"
#line 151 "../Main.m3"
 /* begin_procedure */
#line 151 "../Main.m3"
struct Main__Test_AtomicChar_Frame_t {
#line 151 "../Main.m3"
ADDRESS _unused;
#line 151 "../Main.m3"
};
#line 151 "../Main.m3"
void /*TypeText3*/ 
__cdecl
Main__Test_AtomicChar(void)
{
#line 151 "../Main.m3"
 /* Var_Type2 */ INT64 Main_m_36_L_37={0};//always-init
#line 151 "../Main.m3"
Main__Test_AtomicChar_Frame_t _frame;
#line 151 "../Main.m3"
_frame._unused=(ADDRESS)&_frame;
#line 151 "../Main.m3"
 /* set_source_line */
#line 151 "../Main.m3"
#line 152 "../Main.m3"
 /* set_source_line */
#line 152 "../Main.m3"
#line 153 "../Main.m3"
 /* start_call_direct */
#line 153 "../Main.m3"
 /* call_direct */
#line 153 "../Main.m3"
Main__Test_AtomicChar_Fence(
 );
#line 153 "../Main.m3"
 /* set_source_line */
#line 153 "../Main.m3"
#line 154 "../Main.m3"
 /* start_call_direct */
#line 154 "../Main.m3"
 /* call_direct */
#line 154 "../Main.m3"
Main__Test_AtomicChar_CompareSwap(
 );
#line 154 "../Main.m3"
 /* set_source_line */
#line 154 "../Main.m3"
#line 155 "../Main.m3"
 /* start_call_direct */
#line 155 "../Main.m3"
 /* call_direct */
#line 155 "../Main.m3"
Main__Test_AtomicChar_FetchAnd(
 );
#line 155 "../Main.m3"
 /* set_source_line */
#line 155 "../Main.m3"
#line 156 "../Main.m3"
 /* start_call_direct */
#line 156 "../Main.m3"
 /* call_direct */
#line 156 "../Main.m3"
Main__Test_AtomicChar_FetchDec(
 );
#line 156 "../Main.m3"
 /* set_source_line */
#line 156 "../Main.m3"
#line 157 "../Main.m3"
 /* start_call_direct */
#line 157 "../Main.m3"
 /* call_direct */
#line 157 "../Main.m3"
Main__Test_AtomicChar_FetchInc(
 );
#line 157 "../Main.m3"
 /* set_source_line */
#line 157 "../Main.m3"
#line 158 "../Main.m3"
 /* start_call_direct */
#line 158 "../Main.m3"
 /* call_direct */
#line 158 "../Main.m3"
Main__Test_AtomicChar_FetchOr(
 );
#line 158 "../Main.m3"
 /* set_source_line */
#line 158 "../Main.m3"
#line 159 "../Main.m3"
 /* start_call_direct */
#line 159 "../Main.m3"
 /* call_direct */
#line 159 "../Main.m3"
Main__Test_AtomicChar_FetchXor(
 );
#line 159 "../Main.m3"
 /* set_source_line */
#line 159 "../Main.m3"
#line 160 "../Main.m3"
 /* start_call_direct */
#line 160 "../Main.m3"
 /* call_direct */
#line 160 "../Main.m3"
 /* store */
#line 160 "../Main.m3"
(*(INT64*)(&Main_m_36_L_37))=(INT64)(((INT64)(Main__Test_AtomicChar_IsLockFree(
 ))));
#line 160 "../Main.m3"
 /* set_source_line */
#line 160 "../Main.m3"
#line 161 "../Main.m3"
 /* start_call_direct */
#line 161 "../Main.m3"
 /* call_direct */
#line 161 "../Main.m3"
Main__Test_AtomicChar_LoadStore(
 );
#line 161 "../Main.m3"
 /* set_source_line */
#line 161 "../Main.m3"
#line 162 "../Main.m3"
 /* start_call_direct */
#line 162 "../Main.m3"
 /* call_direct */
#line 162 "../Main.m3"
Main__Test_AtomicChar_Swap(
 );
#line 162 "../Main.m3"
 /* set_source_line */
#line 162 "../Main.m3"
#line 163 "../Main.m3"
 /* exit_proc */
#line 163 "../Main.m3"
return;
#line 163 "../Main.m3"
 /* end_procedure */
#line 163 "../Main.m3"
} /* Test_AtomicWidechar_Fence */
#line 163 "../Main.m3"
 /* set_source_line */
#line 163 "../Main.m3"
#line 171 "../Main.m3"
 /* begin_procedure */
#line 171 "../Main.m3"
struct Main__Test_AtomicWidechar_Fence_Frame_t {
#line 171 "../Main.m3"
ADDRESS _unused;
#line 171 "../Main.m3"
};
#line 171 "../Main.m3"
void /*TypeText3*/ 
__cdecl
Main__Test_AtomicWidechar_Fence(void)
{
#line 171 "../Main.m3"
Main__Test_AtomicWidechar_Fence_Frame_t _frame;
#line 171 "../Main.m3"
_frame._unused=(ADDRESS)&_frame;
#line 171 "../Main.m3"
 /* set_source_line */
#line 171 "../Main.m3"
#line 172 "../Main.m3"
 /* set_source_line */
#line 172 "../Main.m3"
#line 173 "../Main.m3"
 /* fence */
#line 173 "../Main.m3"
m3_fence();
#line 173 "../Main.m3"
 /* set_source_line */
#line 173 "../Main.m3"
#line 174 "../Main.m3"
 /* exit_proc */
#line 174 "../Main.m3"
return;
#line 174 "../Main.m3"
 /* end_procedure */
#line 174 "../Main.m3"
} /* Test_AtomicWidechar_CompareSwap */
#line 174 "../Main.m3"
 /* set_source_line */
#line 174 "../Main.m3"
#line 176 "../Main.m3"
 /* begin_procedure */
#line 176 "../Main.m3"
struct Main__Test_AtomicWidechar_CompareSwap_Frame_t {
#line 176 "../Main.m3"
ADDRESS _unused;
#line 176 "../Main.m3"
};
#line 176 "../Main.m3"
void /*TypeText3*/ 
__cdecl
Main__Test_AtomicWidechar_CompareSwap(void)
{
#line 176 "../Main.m3"
Main__Test_AtomicWidechar_CompareSwap_Frame_t _frame;
#line 176 "../Main.m3"
_frame._unused=(ADDRESS)&_frame;
#line 176 "../Main.m3"
 /* set_source_line */
#line 176 "../Main.m3"
#line 177 "../Main.m3"
 /* set_source_line */
#line 177 "../Main.m3"
#line 178 "../Main.m3"
 /* load_address */
#line 178 "../Main.m3"
 /* load_address */
#line 178 "../Main.m3"
 /* load */
#line 178 "../Main.m3"
 /* compare_exchange */
#line 178 "../Main.m3"
 /* store */
#line 178 "../Main.m3"
(*(UINT8*)((232)+(char*)(&Main_m_M_Main_L_20)))=(INT64)( ((INT64)(*((UINT16*)(INT64_(210)+((ADDRESS)(&Main_m_M_Main_L_20)))))));
#line 178 "../Main.m3"
 /* set_source_line */
#line 178 "../Main.m3"
#line 179 "../Main.m3"
 /* exit_proc */
#line 179 "../Main.m3"
return;
#line 179 "../Main.m3"
 /* end_procedure */
#line 179 "../Main.m3"
} /* Test_AtomicWidechar_FetchAnd */
#line 179 "../Main.m3"
 /* set_source_line */
#line 179 "../Main.m3"
#line 181 "../Main.m3"
 /* begin_procedure */
#line 181 "../Main.m3"
struct Main__Test_AtomicWidechar_FetchAnd_Frame_t {
#line 181 "../Main.m3"
ADDRESS _unused;
#line 181 "../Main.m3"
};
#line 181 "../Main.m3"
void /*TypeText3*/ 
__cdecl
Main__Test_AtomicWidechar_FetchAnd(void)
{
#line 181 "../Main.m3"
Main__Test_AtomicWidechar_FetchAnd_Frame_t _frame;
#line 181 "../Main.m3"
_frame._unused=(ADDRESS)&_frame;
#line 181 "../Main.m3"
 /* set_source_line */
#line 181 "../Main.m3"
#line 182 "../Main.m3"
 /* set_source_line */
#line 182 "../Main.m3"
#line 183 "../Main.m3"
 /* load_address */
#line 183 "../Main.m3"
 /* load */
#line 183 "../Main.m3"
 /* fetch_and_op */
#line 183 "../Main.m3"
 /* store */
#line 183 "../Main.m3"
(*(UINT16*)((210)+(char*)(&Main_m_M_Main_L_20)))=(INT64)( ((INT64)(*((UINT16*)(INT64_(208)+((ADDRESS)(&Main_m_M_Main_L_20)))))));
#line 183 "../Main.m3"
 /* set_source_line */
#line 183 "../Main.m3"
#line 184 "../Main.m3"
 /* exit_proc */
#line 184 "../Main.m3"
return;
#line 184 "../Main.m3"
 /* end_procedure */
#line 184 "../Main.m3"
} /* Test_AtomicWidechar_FetchDec */
#line 184 "../Main.m3"
 /* set_source_line */
#line 184 "../Main.m3"
#line 186 "../Main.m3"
 /* begin_procedure */
#line 186 "../Main.m3"
struct Main__Test_AtomicWidechar_FetchDec_Frame_t {
#line 186 "../Main.m3"
ADDRESS _unused;
#line 186 "../Main.m3"
};
#line 186 "../Main.m3"
void /*TypeText3*/ 
__cdecl
Main__Test_AtomicWidechar_FetchDec(void)
{
#line 186 "../Main.m3"
Main__Test_AtomicWidechar_FetchDec_Frame_t _frame;
#line 186 "../Main.m3"
_frame._unused=(ADDRESS)&_frame;
#line 186 "../Main.m3"
 /* set_source_line */
#line 186 "../Main.m3"
#line 187 "../Main.m3"
 /* set_source_line */
#line 187 "../Main.m3"
#line 188 "../Main.m3"
 /* load_address */
#line 188 "../Main.m3"
 /* load_integer */
#line 188 "../Main.m3"
 /* fetch_and_op */
#line 188 "../Main.m3"
 /* store */
#line 188 "../Main.m3"
(*(UINT16*)((210)+(char*)(&Main_m_M_Main_L_20)))=(INT64)(  INT64_(1));
#line 188 "../Main.m3"
 /* set_source_line */
#line 188 "../Main.m3"
#line 189 "../Main.m3"
 /* exit_proc */
#line 189 "../Main.m3"
return;
#line 189 "../Main.m3"
 /* end_procedure */
#line 189 "../Main.m3"
} /* Test_AtomicWidechar_FetchInc */
#line 189 "../Main.m3"
 /* set_source_line */
#line 189 "../Main.m3"
#line 191 "../Main.m3"
 /* begin_procedure */
#line 191 "../Main.m3"
struct Main__Test_AtomicWidechar_FetchInc_Frame_t {
#line 191 "../Main.m3"
ADDRESS _unused;
#line 191 "../Main.m3"
};
#line 191 "../Main.m3"
void /*TypeText3*/ 
__cdecl
Main__Test_AtomicWidechar_FetchInc(void)
{
#line 191 "../Main.m3"
Main__Test_AtomicWidechar_FetchInc_Frame_t _frame;
#line 191 "../Main.m3"
_frame._unused=(ADDRESS)&_frame;
#line 191 "../Main.m3"
 /* set_source_line */
#line 191 "../Main.m3"
#line 192 "../Main.m3"
 /* set_source_line */
#line 192 "../Main.m3"
#line 193 "../Main.m3"
 /* load_address */
#line 193 "../Main.m3"
 /* load_integer */
#line 193 "../Main.m3"
 /* fetch_and_op */
#line 193 "../Main.m3"
 /* store */
#line 193 "../Main.m3"
(*(UINT16*)((210)+(char*)(&Main_m_M_Main_L_20)))=(INT64)(  INT64_(1));
#line 193 "../Main.m3"
 /* set_source_line */
#line 193 "../Main.m3"
#line 194 "../Main.m3"
 /* exit_proc */
#line 194 "../Main.m3"
return;
#line 194 "../Main.m3"
 /* end_procedure */
#line 194 "../Main.m3"
} /* Test_AtomicWidechar_FetchOr */
#line 194 "../Main.m3"
 /* set_source_line */
#line 194 "../Main.m3"
#line 196 "../Main.m3"
 /* begin_procedure */
#line 196 "../Main.m3"
struct Main__Test_AtomicWidechar_FetchOr_Frame_t {
#line 196 "../Main.m3"
ADDRESS _unused;
#line 196 "../Main.m3"
};
#line 196 "../Main.m3"
void /*TypeText3*/ 
__cdecl
Main__Test_AtomicWidechar_FetchOr(void)
{
#line 196 "../Main.m3"
Main__Test_AtomicWidechar_FetchOr_Frame_t _frame;
#line 196 "../Main.m3"
_frame._unused=(ADDRESS)&_frame;
#line 196 "../Main.m3"
 /* set_source_line */
#line 196 "../Main.m3"
#line 197 "../Main.m3"
 /* set_source_line */
#line 197 "../Main.m3"
#line 198 "../Main.m3"
 /* load_address */
#line 198 "../Main.m3"
 /* load */
#line 198 "../Main.m3"
 /* fetch_and_op */
#line 198 "../Main.m3"
 /* store */
#line 198 "../Main.m3"
(*(UINT16*)((210)+(char*)(&Main_m_M_Main_L_20)))=(INT64)( ((INT64)(*((UINT16*)(INT64_(208)+((ADDRESS)(&Main_m_M_Main_L_20)))))));
#line 198 "../Main.m3"
 /* set_source_line */
#line 198 "../Main.m3"
#line 199 "../Main.m3"
 /* exit_proc */
#line 199 "../Main.m3"
return;
#line 199 "../Main.m3"
 /* end_procedure */
#line 199 "../Main.m3"
} /* Test_AtomicWidechar_FetchXor */
#line 199 "../Main.m3"
 /* set_source_line */
#line 199 "../Main.m3"
#line 201 "../Main.m3"
 /* begin_procedure */
#line 201 "../Main.m3"
struct Main__Test_AtomicWidechar_FetchXor_Frame_t {
#line 201 "../Main.m3"
ADDRESS _unused;
#line 201 "../Main.m3"
};
#line 201 "../Main.m3"
void /*TypeText3*/ 
__cdecl
Main__Test_AtomicWidechar_FetchXor(void)
{
#line 201 "../Main.m3"
Main__Test_AtomicWidechar_FetchXor_Frame_t _frame;
#line 201 "../Main.m3"
_frame._unused=(ADDRESS)&_frame;
#line 201 "../Main.m3"
 /* set_source_line */
#line 201 "../Main.m3"
#line 202 "../Main.m3"
 /* set_source_line */
#line 202 "../Main.m3"
#line 203 "../Main.m3"
 /* load_address */
#line 203 "../Main.m3"
 /* load */
#line 203 "../Main.m3"
 /* fetch_and_op */
#line 203 "../Main.m3"
 /* store */
#line 203 "../Main.m3"
(*(UINT16*)((210)+(char*)(&Main_m_M_Main_L_20)))=(INT64)( ((INT64)(*((UINT16*)(INT64_(208)+((ADDRESS)(&Main_m_M_Main_L_20)))))));
#line 203 "../Main.m3"
 /* set_source_line */
#line 203 "../Main.m3"
#line 204 "../Main.m3"
 /* exit_proc */
#line 204 "../Main.m3"
return;
#line 204 "../Main.m3"
 /* end_procedure */
#line 204 "../Main.m3"
} /* Test_AtomicWidechar_IsLockFree */
#line 204 "../Main.m3"
 /* set_source_line */
#line 204 "../Main.m3"
#line 206 "../Main.m3"
 /* begin_procedure */
#line 206 "../Main.m3"
struct Main__Test_AtomicWidechar_IsLockFree_Frame_t {
#line 206 "../Main.m3"
ADDRESS _unused;
#line 206 "../Main.m3"
};
#line 206 "../Main.m3"
BOOLEAN
__cdecl
Main__Test_AtomicWidechar_IsLockFree(void)
{
#line 206 "../Main.m3"
Main__Test_AtomicWidechar_IsLockFree_Frame_t _frame;
#line 206 "../Main.m3"
_frame._unused=(ADDRESS)&_frame;
#line 206 "../Main.m3"
 /* set_source_line */
#line 206 "../Main.m3"
#line 207 "../Main.m3"
 /* set_source_line */
#line 207 "../Main.m3"
#line 208 "../Main.m3"
 /* load_integer */
#line 208 "../Main.m3"
 /* exit_proc */
#line 208 "../Main.m3"
return  INT64_(1);
#line 208 "../Main.m3"
 /* end_procedure */
#line 208 "../Main.m3"
} /* Test_AtomicWidechar_LoadStore */
#line 208 "../Main.m3"
 /* set_source_line */
#line 208 "../Main.m3"
#line 211 "../Main.m3"
 /* begin_procedure */
#line 211 "../Main.m3"
struct Main__Test_AtomicWidechar_LoadStore_Frame_t {
#line 211 "../Main.m3"
ADDRESS _unused;
#line 211 "../Main.m3"
};
#line 211 "../Main.m3"
void /*TypeText3*/ 
__cdecl
Main__Test_AtomicWidechar_LoadStore(void)
{
#line 211 "../Main.m3"
 /* Var_Type1 */ WIDECHAR integerC_L_25={0};//always-init
#line 211 "../Main.m3"
Main__Test_AtomicWidechar_LoadStore_Frame_t _frame;
#line 211 "../Main.m3"
_frame._unused=(ADDRESS)&_frame;
#line 211 "../Main.m3"
 /* set_source_line */
#line 211 "../Main.m3"
#line 212 "../Main.m3"
 /* set_source_line */
#line 212 "../Main.m3"
#line 214 "../Main.m3"
 /* load_address */
#line 214 "../Main.m3"
 /* load_ordered */
#line 214 "../Main.m3"
 /* load_indirect */
#line 214 "../Main.m3"
 /* store */
#line 214 "../Main.m3"
(*(UINT16*)((208)+(char*)(&Main_m_M_Main_L_20)))=(INT64)( ((INT64)(*((UINT16*)(INT64_(136)+((ADDRESS)(&Main_m_M_Main_L_20)))))));
#line 214 "../Main.m3"
 /* set_source_line */
#line 214 "../Main.m3"
#line 215 "../Main.m3"
 /* load_address */
#line 215 "../Main.m3"
 /* load_integer */
#line 215 "../Main.m3"
 /* store_ordered => store_helper */
#line 215 "../Main.m3"
(*(UINT16*)(INT64_(136)+((ADDRESS)(&Main_m_M_Main_L_20))))=(INT64)( INT64_(6));
#line 215 "../Main.m3"
 /* set_source_line */
#line 215 "../Main.m3"
#line 216 "../Main.m3"
 /* load_address */
#line 216 "../Main.m3"
 /* load_ordered */
#line 216 "../Main.m3"
 /* load_indirect */
#line 216 "../Main.m3"
 /* store */
#line 216 "../Main.m3"
(*(UINT16*)((208)+(char*)(&Main_m_M_Main_L_20)))=(INT64)( ((INT64)(*((UINT16*)(INT64_(136)+((ADDRESS)(&Main_m_M_Main_L_20)))))));
#line 216 "../Main.m3"
 /* set_source_line */
#line 216 "../Main.m3"
#line 217 "../Main.m3"
 /* load_address */
#line 217 "../Main.m3"
 /* load_integer */
#line 217 "../Main.m3"
 /* store_ordered => store_helper */
#line 217 "../Main.m3"
(*(UINT16*)(INT64_(136)+((ADDRESS)(&Main_m_M_Main_L_20))))=(INT64)( INT64_(6));
#line 217 "../Main.m3"
 /* set_source_line */
#line 217 "../Main.m3"
#line 219 "../Main.m3"
 /* load_address */
#line 219 "../Main.m3"
 /* load_ordered */
#line 219 "../Main.m3"
 /* load_indirect */
#line 219 "../Main.m3"
 /* store */
#line 219 "../Main.m3"
(*(UINT16*)(&integerC_L_25))=(INT64)( ((INT64)(*((UINT16*)(INT64_(136)+((ADDRESS)(&Main_m_M_Main_L_20)))))));
#line 219 "../Main.m3"
 /* set_source_line */
#line 219 "../Main.m3"
#line 220 "../Main.m3"
 /* load_address */
#line 220 "../Main.m3"
 /* load_ordered */
#line 220 "../Main.m3"
 /* load_indirect */
#line 220 "../Main.m3"
 /* store */
#line 220 "../Main.m3"
(*(UINT16*)(&integerC_L_25))=(INT64)( ((INT64)(*((UINT16*)(INT64_(136)+((ADDRESS)(&Main_m_M_Main_L_20)))))));
#line 220 "../Main.m3"
 /* set_source_line */
#line 220 "../Main.m3"
#line 221 "../Main.m3"
 /* exit_proc */
#line 221 "../Main.m3"
return;
#line 221 "../Main.m3"
 /* end_procedure */
#line 221 "../Main.m3"
} /* Test_AtomicWidechar_Swap */
#line 221 "../Main.m3"
 /* set_source_line */
#line 221 "../Main.m3"
#line 223 "../Main.m3"
 /* begin_procedure */
#line 223 "../Main.m3"
struct Main__Test_AtomicWidechar_Swap_Frame_t {
#line 223 "../Main.m3"
ADDRESS _unused;
#line 223 "../Main.m3"
};
#line 223 "../Main.m3"
void /*TypeText3*/ 
__cdecl
Main__Test_AtomicWidechar_Swap(void)
{
#line 223 "../Main.m3"
Main__Test_AtomicWidechar_Swap_Frame_t _frame;
#line 223 "../Main.m3"
_frame._unused=(ADDRESS)&_frame;
#line 223 "../Main.m3"
 /* set_source_line */
#line 223 "../Main.m3"
#line 224 "../Main.m3"
 /* set_source_line */
#line 224 "../Main.m3"
#line 225 "../Main.m3"
 /* load_address */
#line 225 "../Main.m3"
 /* load */
#line 225 "../Main.m3"
 /* exchange */
#line 225 "../Main.m3"
 /* store */
#line 225 "../Main.m3"
(*(UINT16*)((210)+(char*)(&Main_m_M_Main_L_20)))=(INT64)( ((INT64)(*((UINT16*)(INT64_(208)+((ADDRESS)(&Main_m_M_Main_L_20)))))));
#line 225 "../Main.m3"
 /* set_source_line */
#line 225 "../Main.m3"
#line 226 "../Main.m3"
 /* exit_proc */
#line 226 "../Main.m3"
return;
#line 226 "../Main.m3"
 /* end_procedure */
#line 226 "../Main.m3"
} /* Test_AtomicWidechar */
#line 226 "../Main.m3"
 /* set_source_line */
#line 226 "../Main.m3"
#line 228 "../Main.m3"
 /* begin_procedure */
#line 228 "../Main.m3"
struct Main__Test_AtomicWidechar_Frame_t {
#line 228 "../Main.m3"
ADDRESS _unused;
#line 228 "../Main.m3"
};
#line 228 "../Main.m3"
void /*TypeText3*/ 
__cdecl
Main__Test_AtomicWidechar(void)
{
#line 228 "../Main.m3"
 /* Var_Type2 */ INT64 Main_m_38_L_39={0};//always-init
#line 228 "../Main.m3"
Main__Test_AtomicWidechar_Frame_t _frame;
#line 228 "../Main.m3"
_frame._unused=(ADDRESS)&_frame;
#line 228 "../Main.m3"
 /* set_source_line */
#line 228 "../Main.m3"
#line 229 "../Main.m3"
 /* set_source_line */
#line 229 "../Main.m3"
#line 230 "../Main.m3"
 /* start_call_direct */
#line 230 "../Main.m3"
 /* call_direct */
#line 230 "../Main.m3"
Main__Test_AtomicWidechar_Fence(
 );
#line 230 "../Main.m3"
 /* set_source_line */
#line 230 "../Main.m3"
#line 231 "../Main.m3"
 /* start_call_direct */
#line 231 "../Main.m3"
 /* call_direct */
#line 231 "../Main.m3"
Main__Test_AtomicWidechar_CompareSwap(
 );
#line 231 "../Main.m3"
 /* set_source_line */
#line 231 "../Main.m3"
#line 232 "../Main.m3"
 /* start_call_direct */
#line 232 "../Main.m3"
 /* call_direct */
#line 232 "../Main.m3"
Main__Test_AtomicWidechar_FetchAnd(
 );
#line 232 "../Main.m3"
 /* set_source_line */
#line 232 "../Main.m3"
#line 233 "../Main.m3"
 /* start_call_direct */
#line 233 "../Main.m3"
 /* call_direct */
#line 233 "../Main.m3"
Main__Test_AtomicWidechar_FetchDec(
 );
#line 233 "../Main.m3"
 /* set_source_line */
#line 233 "../Main.m3"
#line 234 "../Main.m3"
 /* start_call_direct */
#line 234 "../Main.m3"
 /* call_direct */
#line 234 "../Main.m3"
Main__Test_AtomicWidechar_FetchInc(
 );
#line 234 "../Main.m3"
 /* set_source_line */
#line 234 "../Main.m3"
#line 235 "../Main.m3"
 /* start_call_direct */
#line 235 "../Main.m3"
 /* call_direct */
#line 235 "../Main.m3"
Main__Test_AtomicWidechar_FetchOr(
 );
#line 235 "../Main.m3"
 /* set_source_line */
#line 235 "../Main.m3"
#line 236 "../Main.m3"
 /* start_call_direct */
#line 236 "../Main.m3"
 /* call_direct */
#line 236 "../Main.m3"
Main__Test_AtomicWidechar_FetchXor(
 );
#line 236 "../Main.m3"
 /* set_source_line */
#line 236 "../Main.m3"
#line 237 "../Main.m3"
 /* start_call_direct */
#line 237 "../Main.m3"
 /* call_direct */
#line 237 "../Main.m3"
 /* store */
#line 237 "../Main.m3"
(*(INT64*)(&Main_m_38_L_39))=(INT64)(((INT64)(Main__Test_AtomicWidechar_IsLockFree(
 ))));
#line 237 "../Main.m3"
 /* set_source_line */
#line 237 "../Main.m3"
#line 238 "../Main.m3"
 /* start_call_direct */
#line 238 "../Main.m3"
 /* call_direct */
#line 238 "../Main.m3"
Main__Test_AtomicWidechar_LoadStore(
 );
#line 238 "../Main.m3"
 /* set_source_line */
#line 238 "../Main.m3"
#line 239 "../Main.m3"
 /* start_call_direct */
#line 239 "../Main.m3"
 /* call_direct */
#line 239 "../Main.m3"
Main__Test_AtomicWidechar_Swap(
 );
#line 239 "../Main.m3"
 /* set_source_line */
#line 239 "../Main.m3"
#line 240 "../Main.m3"
 /* exit_proc */
#line 240 "../Main.m3"
return;
#line 240 "../Main.m3"
 /* end_procedure */
#line 240 "../Main.m3"
} /* Test_AtomicRefany_Fence */
#line 240 "../Main.m3"
 /* set_source_line */
#line 240 "../Main.m3"
#line 246 "../Main.m3"
 /* begin_procedure */
#line 246 "../Main.m3"
struct Main__Test_AtomicRefany_Fence_Frame_t {
#line 246 "../Main.m3"
ADDRESS _unused;
#line 246 "../Main.m3"
};
#line 246 "../Main.m3"
void /*TypeText3*/ 
__cdecl
Main__Test_AtomicRefany_Fence(void)
{
#line 246 "../Main.m3"
Main__Test_AtomicRefany_Fence_Frame_t _frame;
#line 246 "../Main.m3"
_frame._unused=(ADDRESS)&_frame;
#line 246 "../Main.m3"
 /* set_source_line */
#line 246 "../Main.m3"
#line 247 "../Main.m3"
 /* set_source_line */
#line 247 "../Main.m3"
#line 248 "../Main.m3"
 /* fence */
#line 248 "../Main.m3"
m3_fence();
#line 248 "../Main.m3"
 /* set_source_line */
#line 248 "../Main.m3"
#line 249 "../Main.m3"
 /* exit_proc */
#line 249 "../Main.m3"
return;
#line 249 "../Main.m3"
 /* end_procedure */
#line 249 "../Main.m3"
} /* Test_AtomicRefany_CompareSwap */
#line 249 "../Main.m3"
 /* set_source_line */
#line 249 "../Main.m3"
#line 251 "../Main.m3"
 /* begin_procedure */
#line 251 "../Main.m3"
struct Main__Test_AtomicRefany_CompareSwap_Frame_t {
#line 251 "../Main.m3"
ADDRESS _unused;
#line 251 "../Main.m3"
};
#line 251 "../Main.m3"
void /*TypeText3*/ 
__cdecl
Main__Test_AtomicRefany_CompareSwap(void)
{
#line 251 "../Main.m3"
 /* Var_Type2 */ ADDRESS Main_m_40_L_41={0};//always-init
#line 251 "../Main.m3"
Main__Test_AtomicRefany_CompareSwap_Frame_t _frame;
#line 251 "../Main.m3"
_frame._unused=(ADDRESS)&_frame;
#line 251 "../Main.m3"
 /* set_source_line */
#line 251 "../Main.m3"
#line 252 "../Main.m3"
 /* set_source_line */
#line 252 "../Main.m3"
#line 253 "../Main.m3"
 /* load */
#line 253 "../Main.m3"
 /* store */
#line 253 "../Main.m3"
(*(ADDRESS*)(&Main_m_40_L_41))=(ADDRESS)(((ADDRESS)(*((ADDRESS*)(INT64_(200)+((ADDRESS)(&Main_m_M_Main_L_20)))))));
#line 253 "../Main.m3"
 /* load_nil */
#line 253 "../Main.m3"
 /* load */
#line 253 "../Main.m3"
 /* if_compare */
#line 253 "../Main.m3"
if(m3_eq(ADDRESS,
 ((ADDRESS)(0)),
 ((ADDRESS)(Main_m_40_L_41))))goto L1;
#line 253 "../Main.m3"
 /* load */
#line 253 "../Main.m3"
 /* loophole */
#line 253 "../Main.m3"
 /* load_integer */
#line 253 "../Main.m3"
 /* and */
#line 253 "../Main.m3"
 /* if_true_or_false */
#line 253 "../Main.m3"
 /* load_host_integer */
#line 253 "../Main.m3"
 /* load_integer */
#line 253 "../Main.m3"
 /* if_compare */
#line 253 "../Main.m3"
if(m3_ne(INT64,
  ((INT64)(((INT64)((INT64)Main_m_40_L_41))&  INT64_(1))),
   INT64_(0)))goto L1;
#line 253 "../Main.m3"
 /* load */
#line 253 "../Main.m3"
 /* load_indirect */
#line 253 "../Main.m3"
 /* extract_mn */
#line 253 "../Main.m3"
 /* load_host_integer */
#line 253 "../Main.m3"
 /* load_integer */
#line 253 "../Main.m3"
 /* load_host_integer */
#line 253 "../Main.m3"
 /* load_integer */
#line 253 "../Main.m3"
 /* extract */
#line 253 "../Main.m3"
 /* if_true_or_false */
#line 253 "../Main.m3"
 /* load_host_integer */
#line 253 "../Main.m3"
 /* load_integer */
#line 253 "../Main.m3"
 /* if_compare */
#line 253 "../Main.m3"
if(m3_eq(INT64,
 ((INT64)(m3_extract(
 UINT64,
  *((INT64*)(INT64_(-8)+((ADDRESS)(Main_m_40_L_41)))),
   UINT64_(22),
   UINT64_(1)))),
   INT64_(0)))goto L1;
#line 253 "../Main.m3"
 /* start_call_direct */
#line 253 "../Main.m3"
 /* load */
#line 253 "../Main.m3"
 /* pop_param */
#line 253 "../Main.m3"
 /* call_direct */
#line 253 "../Main.m3"
RTHooks__CheckLoadTracedRef(
  ( REFANY )(((ADDRESS)(Main_m_40_L_41)) ));
#line 253 "../Main.m3"
 /* set_label */
#line 253 "../Main.m3"
L1:;
#line 253 "../Main.m3"
 /* load_address */
#line 253 "../Main.m3"
 /* load_address */
#line 253 "../Main.m3"
 /* load */
#line 253 "../Main.m3"
 /* compare_exchange */
#line 253 "../Main.m3"
 /* store */
#line 253 "../Main.m3"
(*(UINT8*)((232)+(char*)(&Main_m_M_Main_L_20)))=(INT64)(((INT64)(Main_m_40_L_41)));
#line 253 "../Main.m3"
 /* set_source_line */
#line 253 "../Main.m3"
#line 254 "../Main.m3"
 /* exit_proc */
#line 254 "../Main.m3"
return;
#line 254 "../Main.m3"
 /* end_procedure */
#line 254 "../Main.m3"
} /* Test_AtomicRefany_FetchAnd */
#line 254 "../Main.m3"
 /* set_source_line */
#line 254 "../Main.m3"
#line 256 "../Main.m3"
 /* begin_procedure */
#line 256 "../Main.m3"
struct Main__Test_AtomicRefany_FetchAnd_Frame_t {
#line 256 "../Main.m3"
ADDRESS _unused;
#line 256 "../Main.m3"
};
#line 256 "../Main.m3"
void /*TypeText3*/ 
__cdecl
Main__Test_AtomicRefany_FetchAnd(void)
{
#line 256 "../Main.m3"
Main__Test_AtomicRefany_FetchAnd_Frame_t _frame;
#line 256 "../Main.m3"
_frame._unused=(ADDRESS)&_frame;
#line 256 "../Main.m3"
 /* set_source_line */
#line 256 "../Main.m3"
#line 257 "../Main.m3"
 /* set_source_line */
#line 257 "../Main.m3"
#line 259 "../Main.m3"
 /* exit_proc */
#line 259 "../Main.m3"
return;
#line 259 "../Main.m3"
 /* end_procedure */
#line 259 "../Main.m3"
} /* Test_AtomicRefany_FetchDec */
#line 259 "../Main.m3"
 /* set_source_line */
#line 259 "../Main.m3"
#line 261 "../Main.m3"
 /* begin_procedure */
#line 261 "../Main.m3"
struct Main__Test_AtomicRefany_FetchDec_Frame_t {
#line 261 "../Main.m3"
ADDRESS _unused;
#line 261 "../Main.m3"
};
#line 261 "../Main.m3"
void /*TypeText3*/ 
__cdecl
Main__Test_AtomicRefany_FetchDec(void)
{
#line 261 "../Main.m3"
Main__Test_AtomicRefany_FetchDec_Frame_t _frame;
#line 261 "../Main.m3"
_frame._unused=(ADDRESS)&_frame;
#line 261 "../Main.m3"
 /* set_source_line */
#line 261 "../Main.m3"
#line 262 "../Main.m3"
 /* set_source_line */
#line 262 "../Main.m3"
#line 264 "../Main.m3"
 /* exit_proc */
#line 264 "../Main.m3"
return;
#line 264 "../Main.m3"
 /* end_procedure */
#line 264 "../Main.m3"
} /* Test_AtomicRefany_FetchInc */
#line 264 "../Main.m3"
 /* set_source_line */
#line 264 "../Main.m3"
#line 266 "../Main.m3"
 /* begin_procedure */
#line 266 "../Main.m3"
struct Main__Test_AtomicRefany_FetchInc_Frame_t {
#line 266 "../Main.m3"
ADDRESS _unused;
#line 266 "../Main.m3"
};
#line 266 "../Main.m3"
void /*TypeText3*/ 
__cdecl
Main__Test_AtomicRefany_FetchInc(void)
{
#line 266 "../Main.m3"
Main__Test_AtomicRefany_FetchInc_Frame_t _frame;
#line 266 "../Main.m3"
_frame._unused=(ADDRESS)&_frame;
#line 266 "../Main.m3"
 /* set_source_line */
#line 266 "../Main.m3"
#line 267 "../Main.m3"
 /* set_source_line */
#line 267 "../Main.m3"
#line 269 "../Main.m3"
 /* exit_proc */
#line 269 "../Main.m3"
return;
#line 269 "../Main.m3"
 /* end_procedure */
#line 269 "../Main.m3"
} /* Test_AtomicRefany_FetchOr */
#line 269 "../Main.m3"
 /* set_source_line */
#line 269 "../Main.m3"
#line 271 "../Main.m3"
 /* begin_procedure */
#line 271 "../Main.m3"
struct Main__Test_AtomicRefany_FetchOr_Frame_t {
#line 271 "../Main.m3"
ADDRESS _unused;
#line 271 "../Main.m3"
};
#line 271 "../Main.m3"
void /*TypeText3*/ 
__cdecl
Main__Test_AtomicRefany_FetchOr(void)
{
#line 271 "../Main.m3"
Main__Test_AtomicRefany_FetchOr_Frame_t _frame;
#line 271 "../Main.m3"
_frame._unused=(ADDRESS)&_frame;
#line 271 "../Main.m3"
 /* set_source_line */
#line 271 "../Main.m3"
#line 272 "../Main.m3"
 /* set_source_line */
#line 272 "../Main.m3"
#line 274 "../Main.m3"
 /* exit_proc */
#line 274 "../Main.m3"
return;
#line 274 "../Main.m3"
 /* end_procedure */
#line 274 "../Main.m3"
} /* Test_AtomicRefany_FetchXor */
#line 274 "../Main.m3"
 /* set_source_line */
#line 274 "../Main.m3"
#line 276 "../Main.m3"
 /* begin_procedure */
#line 276 "../Main.m3"
struct Main__Test_AtomicRefany_FetchXor_Frame_t {
#line 276 "../Main.m3"
ADDRESS _unused;
#line 276 "../Main.m3"
};
#line 276 "../Main.m3"
void /*TypeText3*/ 
__cdecl
Main__Test_AtomicRefany_FetchXor(void)
{
#line 276 "../Main.m3"
Main__Test_AtomicRefany_FetchXor_Frame_t _frame;
#line 276 "../Main.m3"
_frame._unused=(ADDRESS)&_frame;
#line 276 "../Main.m3"
 /* set_source_line */
#line 276 "../Main.m3"
#line 277 "../Main.m3"
 /* set_source_line */
#line 277 "../Main.m3"
#line 279 "../Main.m3"
 /* exit_proc */
#line 279 "../Main.m3"
return;
#line 279 "../Main.m3"
 /* end_procedure */
#line 279 "../Main.m3"
} /* Test_AtomicRefany_IsLockFree */
#line 279 "../Main.m3"
 /* set_source_line */
#line 279 "../Main.m3"
#line 281 "../Main.m3"
 /* begin_procedure */
#line 281 "../Main.m3"
struct Main__Test_AtomicRefany_IsLockFree_Frame_t {
#line 281 "../Main.m3"
ADDRESS _unused;
#line 281 "../Main.m3"
};
#line 281 "../Main.m3"
BOOLEAN
__cdecl
Main__Test_AtomicRefany_IsLockFree(void)
{
#line 281 "../Main.m3"
Main__Test_AtomicRefany_IsLockFree_Frame_t _frame;
#line 281 "../Main.m3"
_frame._unused=(ADDRESS)&_frame;
#line 281 "../Main.m3"
 /* set_source_line */
#line 281 "../Main.m3"
#line 282 "../Main.m3"
 /* set_source_line */
#line 282 "../Main.m3"
#line 283 "../Main.m3"
 /* load_integer */
#line 283 "../Main.m3"
 /* exit_proc */
#line 283 "../Main.m3"
return  INT64_(1);
#line 283 "../Main.m3"
 /* end_procedure */
#line 283 "../Main.m3"
} /* Test_AtomicRefany_LoadStore */
#line 283 "../Main.m3"
 /* set_source_line */
#line 283 "../Main.m3"
#line 286 "../Main.m3"
 /* begin_procedure */
#line 286 "../Main.m3"
struct Main__Test_AtomicRefany_LoadStore_Frame_t {
#line 286 "../Main.m3"
ADDRESS _unused;
#line 286 "../Main.m3"
};
#line 286 "../Main.m3"
void /*TypeText3*/ 
__cdecl
Main__Test_AtomicRefany_LoadStore(void)
{
#line 286 "../Main.m3"
 /* Var_Type1 */ REFANY refanyC_L_27={0};//always-init
#line 286 "../Main.m3"
 /* Var_Type1 */ INTEGER* xxx_L_28={0};//always-init
#line 286 "../Main.m3"
 /* Var_Type2 */ ADDRESS Main_m_42_L_43={0};//always-init
#line 286 "../Main.m3"
Main__Test_AtomicRefany_LoadStore_Frame_t _frame;
#line 286 "../Main.m3"
_frame._unused=(ADDRESS)&_frame;
#line 286 "../Main.m3"
 /* set_source_line */
#line 286 "../Main.m3"
#line 290 "../Main.m3"
 /* load_nil */
#line 290 "../Main.m3"
 /* store */
#line 290 "../Main.m3"
(*(ADDRESS*)(&refanyC_L_27))=(ADDRESS)(((ADDRESS)(0)));
#line 290 "../Main.m3"
 /* set_source_line */
#line 290 "../Main.m3"
#line 291 "../Main.m3"
 /* start_call_direct */
#line 291 "../Main.m3"
 /* load */
#line 291 "../Main.m3"
 /* pop_param */
#line 291 "../Main.m3"
 /* call_direct */
#line 291 "../Main.m3"
 /* store */
#line 291 "../Main.m3"
(*(ADDRESS*)(&Main_m_42_L_43))=(ADDRESS)(((ADDRESS)(RTHooks__AllocateTracedRef(
  ( ADDRESS )(((ADDRESS)(*((ADDRESS*)(INT64_(624)+((ADDRESS)(&Main_m_M_Main_L_20)))))) )))));
#line 291 "../Main.m3"
 /* load */
#line 291 "../Main.m3"
 /* store */
#line 291 "../Main.m3"
(*(ADDRESS*)(&xxx_L_28))=(ADDRESS)(((ADDRESS)(Main_m_42_L_43)));
#line 291 "../Main.m3"
 /* set_source_line */
#line 291 "../Main.m3"
#line 287 "../Main.m3"
 /* set_source_line */
#line 287 "../Main.m3"
#line 293 "../Main.m3"
 /* load */
#line 293 "../Main.m3"
 /* load_integer */
#line 293 "../Main.m3"
 /* store_indirect */
#line 293 "../Main.m3"
(*(INT64*)(xxx_L_28))=(INT64)(  INT64_(23));
#line 293 "../Main.m3"
 /* set_source_line */
#line 293 "../Main.m3"
#line 294 "../Main.m3"
 /* load_address */
#line 294 "../Main.m3"
 /* load_ordered */
#line 294 "../Main.m3"
 /* load_indirect */
#line 294 "../Main.m3"
 /* store */
#line 294 "../Main.m3"
(*(ADDRESS*)((192)+(char*)(&Main_m_M_Main_L_20)))=(ADDRESS)(((ADDRESS)(*((ADDRESS*)(INT64_(128)+((ADDRESS)(&Main_m_M_Main_L_20)))))));
#line 294 "../Main.m3"
 /* set_source_line */
#line 294 "../Main.m3"
#line 295 "../Main.m3"
 /* load_address */
#line 295 "../Main.m3"
 /* load */
#line 295 "../Main.m3"
 /* store_ordered => store_helper */
#line 295 "../Main.m3"
(*(ADDRESS*)(INT64_(128)+((ADDRESS)(&Main_m_M_Main_L_20))))=(ADDRESS)(xxx_L_28);
#line 295 "../Main.m3"
 /* set_source_line */
#line 295 "../Main.m3"
#line 296 "../Main.m3"
 /* load_address */
#line 296 "../Main.m3"
 /* load_ordered */
#line 296 "../Main.m3"
 /* load_indirect */
#line 296 "../Main.m3"
 /* store */
#line 296 "../Main.m3"
(*(ADDRESS*)((192)+(char*)(&Main_m_M_Main_L_20)))=(ADDRESS)(((ADDRESS)(*((ADDRESS*)(INT64_(128)+((ADDRESS)(&Main_m_M_Main_L_20)))))));
#line 296 "../Main.m3"
 /* set_source_line */
#line 296 "../Main.m3"
#line 297 "../Main.m3"
 /* load_address */
#line 297 "../Main.m3"
 /* load */
#line 297 "../Main.m3"
 /* store_ordered => store_helper */
#line 297 "../Main.m3"
(*(ADDRESS*)(INT64_(128)+((ADDRESS)(&Main_m_M_Main_L_20))))=(ADDRESS)(xxx_L_28);
#line 297 "../Main.m3"
 /* set_source_line */
#line 297 "../Main.m3"
#line 299 "../Main.m3"
 /* load_address */
#line 299 "../Main.m3"
 /* load_ordered */
#line 299 "../Main.m3"
 /* load_indirect */
#line 299 "../Main.m3"
 /* store */
#line 299 "../Main.m3"
(*(ADDRESS*)(&refanyC_L_27))=(ADDRESS)(((ADDRESS)(*((ADDRESS*)(INT64_(128)+((ADDRESS)(&Main_m_M_Main_L_20)))))));
#line 299 "../Main.m3"
 /* set_source_line */
#line 299 "../Main.m3"
#line 300 "../Main.m3"
 /* load_address */
#line 300 "../Main.m3"
 /* load_ordered */
#line 300 "../Main.m3"
 /* load_indirect */
#line 300 "../Main.m3"
 /* store */
#line 300 "../Main.m3"
(*(ADDRESS*)(&refanyC_L_27))=(ADDRESS)(((ADDRESS)(*((ADDRESS*)(INT64_(128)+((ADDRESS)(&Main_m_M_Main_L_20)))))));
#line 300 "../Main.m3"
 /* set_source_line */
#line 300 "../Main.m3"
#line 301 "../Main.m3"
 /* exit_proc */
#line 301 "../Main.m3"
return;
#line 301 "../Main.m3"
 /* end_procedure */
#line 301 "../Main.m3"
} /* Test_AtomicRefany_Swap */
#line 301 "../Main.m3"
 /* set_source_line */
#line 301 "../Main.m3"
#line 303 "../Main.m3"
 /* begin_procedure */
#line 303 "../Main.m3"
struct Main__Test_AtomicRefany_Swap_Frame_t {
#line 303 "../Main.m3"
ADDRESS _unused;
#line 303 "../Main.m3"
};
#line 303 "../Main.m3"
void /*TypeText3*/ 
__cdecl
Main__Test_AtomicRefany_Swap(void)
{
#line 303 "../Main.m3"
 /* Var_Type2 */ ADDRESS Main_m_44_L_45={0};//always-init
#line 303 "../Main.m3"
Main__Test_AtomicRefany_Swap_Frame_t _frame;
#line 303 "../Main.m3"
_frame._unused=(ADDRESS)&_frame;
#line 303 "../Main.m3"
 /* set_source_line */
#line 303 "../Main.m3"
#line 304 "../Main.m3"
 /* set_source_line */
#line 304 "../Main.m3"
#line 305 "../Main.m3"
 /* load */
#line 305 "../Main.m3"
 /* store */
#line 305 "../Main.m3"
(*(ADDRESS*)(&Main_m_44_L_45))=(ADDRESS)(((ADDRESS)(*((ADDRESS*)(INT64_(192)+((ADDRESS)(&Main_m_M_Main_L_20)))))));
#line 305 "../Main.m3"
 /* load_nil */
#line 305 "../Main.m3"
 /* load */
#line 305 "../Main.m3"
 /* if_compare */
#line 305 "../Main.m3"
if(m3_eq(ADDRESS,
 ((ADDRESS)(0)),
 ((ADDRESS)(Main_m_44_L_45))))goto L2;
#line 305 "../Main.m3"
 /* load */
#line 305 "../Main.m3"
 /* loophole */
#line 305 "../Main.m3"
 /* load_integer */
#line 305 "../Main.m3"
 /* and */
#line 305 "../Main.m3"
 /* if_true_or_false */
#line 305 "../Main.m3"
 /* load_host_integer */
#line 305 "../Main.m3"
 /* load_integer */
#line 305 "../Main.m3"
 /* if_compare */
#line 305 "../Main.m3"
if(m3_ne(INT64,
  ((INT64)(((INT64)((INT64)Main_m_44_L_45))&  INT64_(1))),
   INT64_(0)))goto L2;
#line 305 "../Main.m3"
 /* load */
#line 305 "../Main.m3"
 /* load_indirect */
#line 305 "../Main.m3"
 /* extract_mn */
#line 305 "../Main.m3"
 /* load_host_integer */
#line 305 "../Main.m3"
 /* load_integer */
#line 305 "../Main.m3"
 /* load_host_integer */
#line 305 "../Main.m3"
 /* load_integer */
#line 305 "../Main.m3"
 /* extract */
#line 305 "../Main.m3"
 /* if_true_or_false */
#line 305 "../Main.m3"
 /* load_host_integer */
#line 305 "../Main.m3"
 /* load_integer */
#line 305 "../Main.m3"
 /* if_compare */
#line 305 "../Main.m3"
if(m3_eq(INT64,
 ((INT64)(m3_extract(
 UINT64,
  *((INT64*)(INT64_(-8)+((ADDRESS)(Main_m_44_L_45)))),
   UINT64_(22),
   UINT64_(1)))),
   INT64_(0)))goto L2;
#line 305 "../Main.m3"
 /* start_call_direct */
#line 305 "../Main.m3"
 /* load */
#line 305 "../Main.m3"
 /* pop_param */
#line 305 "../Main.m3"
 /* call_direct */
#line 305 "../Main.m3"
RTHooks__CheckLoadTracedRef(
  ( REFANY )(((ADDRESS)(Main_m_44_L_45)) ));
#line 305 "../Main.m3"
 /* set_label */
#line 305 "../Main.m3"
L2:;
#line 305 "../Main.m3"
 /* load_address */
#line 305 "../Main.m3"
 /* load */
#line 305 "../Main.m3"
 /* exchange */
#line 305 "../Main.m3"
 /* store */
#line 305 "../Main.m3"
(*(ADDRESS*)((200)+(char*)(&Main_m_M_Main_L_20)))=(ADDRESS)(((ADDRESS)(Main_m_44_L_45)));
#line 305 "../Main.m3"
 /* set_source_line */
#line 305 "../Main.m3"
#line 306 "../Main.m3"
 /* exit_proc */
#line 306 "../Main.m3"
return;
#line 306 "../Main.m3"
 /* end_procedure */
#line 306 "../Main.m3"
} /* Test_AtomicRefany */
#line 306 "../Main.m3"
 /* set_source_line */
#line 306 "../Main.m3"
#line 308 "../Main.m3"
 /* begin_procedure */
#line 308 "../Main.m3"
struct Main__Test_AtomicRefany_Frame_t {
#line 308 "../Main.m3"
ADDRESS _unused;
#line 308 "../Main.m3"
};
#line 308 "../Main.m3"
void /*TypeText3*/ 
__cdecl
Main__Test_AtomicRefany(void)
{
#line 308 "../Main.m3"
 /* Var_Type2 */ INT64 Main_m_46_L_47={0};//always-init
#line 308 "../Main.m3"
Main__Test_AtomicRefany_Frame_t _frame;
#line 308 "../Main.m3"
_frame._unused=(ADDRESS)&_frame;
#line 308 "../Main.m3"
 /* set_source_line */
#line 308 "../Main.m3"
#line 309 "../Main.m3"
 /* set_source_line */
#line 309 "../Main.m3"
#line 310 "../Main.m3"
 /* start_call_direct */
#line 310 "../Main.m3"
 /* call_direct */
#line 310 "../Main.m3"
Main__Test_AtomicRefany_Fence(
 );
#line 310 "../Main.m3"
 /* set_source_line */
#line 310 "../Main.m3"
#line 311 "../Main.m3"
 /* start_call_direct */
#line 311 "../Main.m3"
 /* call_direct */
#line 311 "../Main.m3"
Main__Test_AtomicRefany_CompareSwap(
 );
#line 311 "../Main.m3"
 /* set_source_line */
#line 311 "../Main.m3"
#line 312 "../Main.m3"
 /* start_call_direct */
#line 312 "../Main.m3"
 /* call_direct */
#line 312 "../Main.m3"
Main__Test_AtomicRefany_FetchAnd(
 );
#line 312 "../Main.m3"
 /* set_source_line */
#line 312 "../Main.m3"
#line 313 "../Main.m3"
 /* start_call_direct */
#line 313 "../Main.m3"
 /* call_direct */
#line 313 "../Main.m3"
Main__Test_AtomicRefany_FetchDec(
 );
#line 313 "../Main.m3"
 /* set_source_line */
#line 313 "../Main.m3"
#line 314 "../Main.m3"
 /* start_call_direct */
#line 314 "../Main.m3"
 /* call_direct */
#line 314 "../Main.m3"
Main__Test_AtomicRefany_FetchInc(
 );
#line 314 "../Main.m3"
 /* set_source_line */
#line 314 "../Main.m3"
#line 315 "../Main.m3"
 /* start_call_direct */
#line 315 "../Main.m3"
 /* call_direct */
#line 315 "../Main.m3"
Main__Test_AtomicRefany_FetchOr(
 );
#line 315 "../Main.m3"
 /* set_source_line */
#line 315 "../Main.m3"
#line 316 "../Main.m3"
 /* start_call_direct */
#line 316 "../Main.m3"
 /* call_direct */
#line 316 "../Main.m3"
Main__Test_AtomicRefany_FetchXor(
 );
#line 316 "../Main.m3"
 /* set_source_line */
#line 316 "../Main.m3"
#line 317 "../Main.m3"
 /* start_call_direct */
#line 317 "../Main.m3"
 /* call_direct */
#line 317 "../Main.m3"
 /* store */
#line 317 "../Main.m3"
(*(INT64*)(&Main_m_46_L_47))=(INT64)(((INT64)(Main__Test_AtomicRefany_IsLockFree(
 ))));
#line 317 "../Main.m3"
 /* set_source_line */
#line 317 "../Main.m3"
#line 318 "../Main.m3"
 /* start_call_direct */
#line 318 "../Main.m3"
 /* call_direct */
#line 318 "../Main.m3"
Main__Test_AtomicRefany_LoadStore(
 );
#line 318 "../Main.m3"
 /* set_source_line */
#line 318 "../Main.m3"
#line 319 "../Main.m3"
 /* start_call_direct */
#line 319 "../Main.m3"
 /* call_direct */
#line 319 "../Main.m3"
Main__Test_AtomicRefany_Swap(
 );
#line 319 "../Main.m3"
 /* set_source_line */
#line 319 "../Main.m3"
#line 320 "../Main.m3"
 /* exit_proc */
#line 320 "../Main.m3"
return;
#line 320 "../Main.m3"
 /* end_procedure */
#line 320 "../Main.m3"
} /* Test_AtomicAddress_Fence */
#line 320 "../Main.m3"
 /* set_source_line */
#line 320 "../Main.m3"
#line 326 "../Main.m3"
 /* begin_procedure */
#line 326 "../Main.m3"
struct Main__Test_AtomicAddress_Fence_Frame_t {
#line 326 "../Main.m3"
ADDRESS _unused;
#line 326 "../Main.m3"
};
#line 326 "../Main.m3"
void /*TypeText3*/ 
__cdecl
Main__Test_AtomicAddress_Fence(void)
{
#line 326 "../Main.m3"
Main__Test_AtomicAddress_Fence_Frame_t _frame;
#line 326 "../Main.m3"
_frame._unused=(ADDRESS)&_frame;
#line 326 "../Main.m3"
 /* set_source_line */
#line 326 "../Main.m3"
#line 327 "../Main.m3"
 /* set_source_line */
#line 327 "../Main.m3"
#line 328 "../Main.m3"
 /* fence */
#line 328 "../Main.m3"
m3_fence();
#line 328 "../Main.m3"
 /* set_source_line */
#line 328 "../Main.m3"
#line 329 "../Main.m3"
 /* exit_proc */
#line 329 "../Main.m3"
return;
#line 329 "../Main.m3"
 /* end_procedure */
#line 329 "../Main.m3"
} /* Test_AtomicAddress_CompareSwap */
#line 329 "../Main.m3"
 /* set_source_line */
#line 329 "../Main.m3"
#line 331 "../Main.m3"
 /* begin_procedure */
#line 331 "../Main.m3"
struct Main__Test_AtomicAddress_CompareSwap_Frame_t {
#line 331 "../Main.m3"
ADDRESS _unused;
#line 331 "../Main.m3"
};
#line 331 "../Main.m3"
void /*TypeText3*/ 
__cdecl
Main__Test_AtomicAddress_CompareSwap(void)
{
#line 331 "../Main.m3"
Main__Test_AtomicAddress_CompareSwap_Frame_t _frame;
#line 331 "../Main.m3"
_frame._unused=(ADDRESS)&_frame;
#line 331 "../Main.m3"
 /* set_source_line */
#line 331 "../Main.m3"
#line 332 "../Main.m3"
 /* set_source_line */
#line 332 "../Main.m3"
#line 333 "../Main.m3"
 /* load_address */
#line 333 "../Main.m3"
 /* load_address */
#line 333 "../Main.m3"
 /* load */
#line 333 "../Main.m3"
 /* compare_exchange */
#line 333 "../Main.m3"
 /* store */
#line 333 "../Main.m3"
(*(UINT8*)((232)+(char*)(&Main_m_M_Main_L_20)))=(INT64)(((INT64)(*((ADDRESS*)(INT64_(224)+((ADDRESS)(&Main_m_M_Main_L_20)))))));
#line 333 "../Main.m3"
 /* set_source_line */
#line 333 "../Main.m3"
#line 334 "../Main.m3"
 /* exit_proc */
#line 334 "../Main.m3"
return;
#line 334 "../Main.m3"
 /* end_procedure */
#line 334 "../Main.m3"
} /* Test_AtomicAddress_FetchAnd */
#line 334 "../Main.m3"
 /* set_source_line */
#line 334 "../Main.m3"
#line 336 "../Main.m3"
 /* begin_procedure */
#line 336 "../Main.m3"
struct Main__Test_AtomicAddress_FetchAnd_Frame_t {
#line 336 "../Main.m3"
ADDRESS _unused;
#line 336 "../Main.m3"
};
#line 336 "../Main.m3"
void /*TypeText3*/ 
__cdecl
Main__Test_AtomicAddress_FetchAnd(void)
{
#line 336 "../Main.m3"
Main__Test_AtomicAddress_FetchAnd_Frame_t _frame;
#line 336 "../Main.m3"
_frame._unused=(ADDRESS)&_frame;
#line 336 "../Main.m3"
 /* set_source_line */
#line 336 "../Main.m3"
#line 337 "../Main.m3"
 /* set_source_line */
#line 337 "../Main.m3"
#line 339 "../Main.m3"
 /* exit_proc */
#line 339 "../Main.m3"
return;
#line 339 "../Main.m3"
 /* end_procedure */
#line 339 "../Main.m3"
} /* Test_AtomicAddress_FetchDec */
#line 339 "../Main.m3"
 /* set_source_line */
#line 339 "../Main.m3"
#line 341 "../Main.m3"
 /* begin_procedure */
#line 341 "../Main.m3"
struct Main__Test_AtomicAddress_FetchDec_Frame_t {
#line 341 "../Main.m3"
ADDRESS _unused;
#line 341 "../Main.m3"
};
#line 341 "../Main.m3"
void /*TypeText3*/ 
__cdecl
Main__Test_AtomicAddress_FetchDec(void)
{
#line 341 "../Main.m3"
Main__Test_AtomicAddress_FetchDec_Frame_t _frame;
#line 341 "../Main.m3"
_frame._unused=(ADDRESS)&_frame;
#line 341 "../Main.m3"
 /* set_source_line */
#line 341 "../Main.m3"
#line 342 "../Main.m3"
 /* set_source_line */
#line 342 "../Main.m3"
#line 344 "../Main.m3"
 /* exit_proc */
#line 344 "../Main.m3"
return;
#line 344 "../Main.m3"
 /* end_procedure */
#line 344 "../Main.m3"
} /* Test_AtomicAddress_FetchInc */
#line 344 "../Main.m3"
 /* set_source_line */
#line 344 "../Main.m3"
#line 346 "../Main.m3"
 /* begin_procedure */
#line 346 "../Main.m3"
struct Main__Test_AtomicAddress_FetchInc_Frame_t {
#line 346 "../Main.m3"
ADDRESS _unused;
#line 346 "../Main.m3"
};
#line 346 "../Main.m3"
void /*TypeText3*/ 
__cdecl
Main__Test_AtomicAddress_FetchInc(void)
{
#line 346 "../Main.m3"
Main__Test_AtomicAddress_FetchInc_Frame_t _frame;
#line 346 "../Main.m3"
_frame._unused=(ADDRESS)&_frame;
#line 346 "../Main.m3"
 /* set_source_line */
#line 346 "../Main.m3"
#line 347 "../Main.m3"
 /* set_source_line */
#line 347 "../Main.m3"
#line 349 "../Main.m3"
 /* exit_proc */
#line 349 "../Main.m3"
return;
#line 349 "../Main.m3"
 /* end_procedure */
#line 349 "../Main.m3"
} /* Test_AtomicAddress_FetchOr */
#line 349 "../Main.m3"
 /* set_source_line */
#line 349 "../Main.m3"
#line 351 "../Main.m3"
 /* begin_procedure */
#line 351 "../Main.m3"
struct Main__Test_AtomicAddress_FetchOr_Frame_t {
#line 351 "../Main.m3"
ADDRESS _unused;
#line 351 "../Main.m3"
};
#line 351 "../Main.m3"
void /*TypeText3*/ 
__cdecl
Main__Test_AtomicAddress_FetchOr(void)
{
#line 351 "../Main.m3"
Main__Test_AtomicAddress_FetchOr_Frame_t _frame;
#line 351 "../Main.m3"
_frame._unused=(ADDRESS)&_frame;
#line 351 "../Main.m3"
 /* set_source_line */
#line 351 "../Main.m3"
#line 352 "../Main.m3"
 /* set_source_line */
#line 352 "../Main.m3"
#line 354 "../Main.m3"
 /* exit_proc */
#line 354 "../Main.m3"
return;
#line 354 "../Main.m3"
 /* end_procedure */
#line 354 "../Main.m3"
} /* Test_AtomicAddress_FetchXor */
#line 354 "../Main.m3"
 /* set_source_line */
#line 354 "../Main.m3"
#line 356 "../Main.m3"
 /* begin_procedure */
#line 356 "../Main.m3"
struct Main__Test_AtomicAddress_FetchXor_Frame_t {
#line 356 "../Main.m3"
ADDRESS _unused;
#line 356 "../Main.m3"
};
#line 356 "../Main.m3"
void /*TypeText3*/ 
__cdecl
Main__Test_AtomicAddress_FetchXor(void)
{
#line 356 "../Main.m3"
Main__Test_AtomicAddress_FetchXor_Frame_t _frame;
#line 356 "../Main.m3"
_frame._unused=(ADDRESS)&_frame;
#line 356 "../Main.m3"
 /* set_source_line */
#line 356 "../Main.m3"
#line 357 "../Main.m3"
 /* set_source_line */
#line 357 "../Main.m3"
#line 359 "../Main.m3"
 /* exit_proc */
#line 359 "../Main.m3"
return;
#line 359 "../Main.m3"
 /* end_procedure */
#line 359 "../Main.m3"
} /* Test_AtomicAddress_IsLockFree */
#line 359 "../Main.m3"
 /* set_source_line */
#line 359 "../Main.m3"
#line 361 "../Main.m3"
 /* begin_procedure */
#line 361 "../Main.m3"
struct Main__Test_AtomicAddress_IsLockFree_Frame_t {
#line 361 "../Main.m3"
ADDRESS _unused;
#line 361 "../Main.m3"
};
#line 361 "../Main.m3"
BOOLEAN
__cdecl
Main__Test_AtomicAddress_IsLockFree(void)
{
#line 361 "../Main.m3"
Main__Test_AtomicAddress_IsLockFree_Frame_t _frame;
#line 361 "../Main.m3"
_frame._unused=(ADDRESS)&_frame;
#line 361 "../Main.m3"
 /* set_source_line */
#line 361 "../Main.m3"
#line 362 "../Main.m3"
 /* set_source_line */
#line 362 "../Main.m3"
#line 363 "../Main.m3"
 /* load_integer */
#line 363 "../Main.m3"
 /* exit_proc */
#line 363 "../Main.m3"
return  INT64_(1);
#line 363 "../Main.m3"
 /* end_procedure */
#line 363 "../Main.m3"
} /* Test_AtomicAddress_LoadStore */
#line 363 "../Main.m3"
 /* set_source_line */
#line 363 "../Main.m3"
#line 366 "../Main.m3"
 /* begin_procedure */
#line 366 "../Main.m3"
struct Main__Test_AtomicAddress_LoadStore_Frame_t {
#line 366 "../Main.m3"
ADDRESS _unused;
#line 366 "../Main.m3"
};
#line 366 "../Main.m3"
void /*TypeText3*/ 
__cdecl
Main__Test_AtomicAddress_LoadStore(void)
{
#line 366 "../Main.m3"
 /* Var_Type1 */ ADDRESS addressC_L_30={0};//always-init
#line 366 "../Main.m3"
Main__Test_AtomicAddress_LoadStore_Frame_t _frame;
#line 366 "../Main.m3"
_frame._unused=(ADDRESS)&_frame;
#line 366 "../Main.m3"
 /* set_source_line */
#line 366 "../Main.m3"
#line 367 "../Main.m3"
 /* load_nil */
#line 367 "../Main.m3"
 /* store */
#line 367 "../Main.m3"
(*(ADDRESS*)(&addressC_L_30))=(ADDRESS)(((ADDRESS)(0)));
#line 367 "../Main.m3"
 /* set_source_line */
#line 367 "../Main.m3"
#line 369 "../Main.m3"
 /* load_address */
#line 369 "../Main.m3"
 /* load_ordered */
#line 369 "../Main.m3"
 /* load_indirect */
#line 369 "../Main.m3"
 /* store */
#line 369 "../Main.m3"
(*(ADDRESS*)((216)+(char*)(&Main_m_M_Main_L_20)))=(ADDRESS)(((ADDRESS)(*((ADDRESS*)(INT64_(144)+((ADDRESS)(&Main_m_M_Main_L_20)))))));
#line 369 "../Main.m3"
 /* set_source_line */
#line 369 "../Main.m3"
#line 370 "../Main.m3"
 /* load_integer */
#line 370 "../Main.m3"
 /* load_integer */
#line 370 "../Main.m3"
 /* add */
#line 370 "../Main.m3"
 /* load_integer */
#line 370 "../Main.m3"
 /* add */
#line 370 "../Main.m3"
 /* loophole */
#line 370 "../Main.m3"
 /* load_address */
#line 370 "../Main.m3"
 /* swap */
#line 370 "../Main.m3"
 /* store_ordered => store_helper */
#line 370 "../Main.m3"
(*(ADDRESS*)(INT64_(144)+((ADDRESS)(&Main_m_M_Main_L_20))))=(ADDRESS)((ADDRESS)((INT64)( ((INT64)(  INT64_(2)+  INT64_(1)))+  INT64_(3))));
#line 370 "../Main.m3"
 /* set_source_line */
#line 370 "../Main.m3"
#line 371 "../Main.m3"
 /* load_address */
#line 371 "../Main.m3"
 /* load_ordered */
#line 371 "../Main.m3"
 /* load_indirect */
#line 371 "../Main.m3"
 /* store */
#line 371 "../Main.m3"
(*(ADDRESS*)((216)+(char*)(&Main_m_M_Main_L_20)))=(ADDRESS)(((ADDRESS)(*((ADDRESS*)(INT64_(144)+((ADDRESS)(&Main_m_M_Main_L_20)))))));
#line 371 "../Main.m3"
 /* set_source_line */
#line 371 "../Main.m3"
#line 372 "../Main.m3"
 /* load_integer */
#line 372 "../Main.m3"
 /* load_integer */
#line 372 "../Main.m3"
 /* add */
#line 372 "../Main.m3"
 /* load_integer */
#line 372 "../Main.m3"
 /* add */
#line 372 "../Main.m3"
 /* loophole */
#line 372 "../Main.m3"
 /* load_address */
#line 372 "../Main.m3"
 /* swap */
#line 372 "../Main.m3"
 /* store_ordered => store_helper */
#line 372 "../Main.m3"
(*(ADDRESS*)(INT64_(144)+((ADDRESS)(&Main_m_M_Main_L_20))))=(ADDRESS)((ADDRESS)((INT64)( ((INT64)(  INT64_(2)+  INT64_(1)))+  INT64_(3))));
#line 372 "../Main.m3"
 /* set_source_line */
#line 372 "../Main.m3"
#line 374 "../Main.m3"
 /* load_address */
#line 374 "../Main.m3"
 /* load_ordered */
#line 374 "../Main.m3"
 /* load_indirect */
#line 374 "../Main.m3"
 /* store */
#line 374 "../Main.m3"
(*(ADDRESS*)(&addressC_L_30))=(ADDRESS)(((ADDRESS)(*((ADDRESS*)(INT64_(144)+((ADDRESS)(&Main_m_M_Main_L_20)))))));
#line 374 "../Main.m3"
 /* set_source_line */
#line 374 "../Main.m3"
#line 375 "../Main.m3"
 /* load_address */
#line 375 "../Main.m3"
 /* load_ordered */
#line 375 "../Main.m3"
 /* load_indirect */
#line 375 "../Main.m3"
 /* store */
#line 375 "../Main.m3"
(*(ADDRESS*)(&addressC_L_30))=(ADDRESS)(((ADDRESS)(*((ADDRESS*)(INT64_(144)+((ADDRESS)(&Main_m_M_Main_L_20)))))));
#line 375 "../Main.m3"
 /* set_source_line */
#line 375 "../Main.m3"
#line 376 "../Main.m3"
 /* exit_proc */
#line 376 "../Main.m3"
return;
#line 376 "../Main.m3"
 /* end_procedure */
#line 376 "../Main.m3"
} /* Test_AtomicAddress_Swap */
#line 376 "../Main.m3"
 /* set_source_line */
#line 376 "../Main.m3"
#line 378 "../Main.m3"
 /* begin_procedure */
#line 378 "../Main.m3"
struct Main__Test_AtomicAddress_Swap_Frame_t {
#line 378 "../Main.m3"
ADDRESS _unused;
#line 378 "../Main.m3"
};
#line 378 "../Main.m3"
void /*TypeText3*/ 
__cdecl
Main__Test_AtomicAddress_Swap(void)
{
#line 378 "../Main.m3"
Main__Test_AtomicAddress_Swap_Frame_t _frame;
#line 378 "../Main.m3"
_frame._unused=(ADDRESS)&_frame;
#line 378 "../Main.m3"
 /* set_source_line */
#line 378 "../Main.m3"
#line 379 "../Main.m3"
 /* set_source_line */
#line 379 "../Main.m3"
#line 380 "../Main.m3"
 /* load_address */
#line 380 "../Main.m3"
 /* load */
#line 380 "../Main.m3"
 /* exchange */
#line 380 "../Main.m3"
 /* store */
#line 380 "../Main.m3"
(*(ADDRESS*)((224)+(char*)(&Main_m_M_Main_L_20)))=(ADDRESS)(((ADDRESS)(*((ADDRESS*)(INT64_(216)+((ADDRESS)(&Main_m_M_Main_L_20)))))));
#line 380 "../Main.m3"
 /* set_source_line */
#line 380 "../Main.m3"
#line 381 "../Main.m3"
 /* exit_proc */
#line 381 "../Main.m3"
return;
#line 381 "../Main.m3"
 /* end_procedure */
#line 381 "../Main.m3"
} /* Test_AtomicAddress */
#line 381 "../Main.m3"
 /* set_source_line */
#line 381 "../Main.m3"
#line 383 "../Main.m3"
 /* begin_procedure */
#line 383 "../Main.m3"
struct Main__Test_AtomicAddress_Frame_t {
#line 383 "../Main.m3"
ADDRESS _unused;
#line 383 "../Main.m3"
};
#line 383 "../Main.m3"
void /*TypeText3*/ 
__cdecl
Main__Test_AtomicAddress(void)
{
#line 383 "../Main.m3"
 /* Var_Type2 */ INT64 Main_m_48_L_49={0};//always-init
#line 383 "../Main.m3"
Main__Test_AtomicAddress_Frame_t _frame;
#line 383 "../Main.m3"
_frame._unused=(ADDRESS)&_frame;
#line 383 "../Main.m3"
 /* set_source_line */
#line 383 "../Main.m3"
#line 384 "../Main.m3"
 /* set_source_line */
#line 384 "../Main.m3"
#line 385 "../Main.m3"
 /* start_call_direct */
#line 385 "../Main.m3"
 /* call_direct */
#line 385 "../Main.m3"
Main__Test_AtomicAddress_Fence(
 );
#line 385 "../Main.m3"
 /* set_source_line */
#line 385 "../Main.m3"
#line 386 "../Main.m3"
 /* start_call_direct */
#line 386 "../Main.m3"
 /* call_direct */
#line 386 "../Main.m3"
Main__Test_AtomicAddress_CompareSwap(
 );
#line 386 "../Main.m3"
 /* set_source_line */
#line 386 "../Main.m3"
#line 387 "../Main.m3"
 /* start_call_direct */
#line 387 "../Main.m3"
 /* call_direct */
#line 387 "../Main.m3"
Main__Test_AtomicAddress_FetchAnd(
 );
#line 387 "../Main.m3"
 /* set_source_line */
#line 387 "../Main.m3"
#line 388 "../Main.m3"
 /* start_call_direct */
#line 388 "../Main.m3"
 /* call_direct */
#line 388 "../Main.m3"
Main__Test_AtomicAddress_FetchDec(
 );
#line 388 "../Main.m3"
 /* set_source_line */
#line 388 "../Main.m3"
#line 389 "../Main.m3"
 /* start_call_direct */
#line 389 "../Main.m3"
 /* call_direct */
#line 389 "../Main.m3"
Main__Test_AtomicAddress_FetchInc(
 );
#line 389 "../Main.m3"
 /* set_source_line */
#line 389 "../Main.m3"
#line 390 "../Main.m3"
 /* start_call_direct */
#line 390 "../Main.m3"
 /* call_direct */
#line 390 "../Main.m3"
Main__Test_AtomicAddress_FetchOr(
 );
#line 390 "../Main.m3"
 /* set_source_line */
#line 390 "../Main.m3"
#line 391 "../Main.m3"
 /* start_call_direct */
#line 391 "../Main.m3"
 /* call_direct */
#line 391 "../Main.m3"
Main__Test_AtomicAddress_FetchXor(
 );
#line 391 "../Main.m3"
 /* set_source_line */
#line 391 "../Main.m3"
#line 392 "../Main.m3"
 /* start_call_direct */
#line 392 "../Main.m3"
 /* call_direct */
#line 392 "../Main.m3"
 /* store */
#line 392 "../Main.m3"
(*(INT64*)(&Main_m_48_L_49))=(INT64)(((INT64)(Main__Test_AtomicAddress_IsLockFree(
 ))));
#line 392 "../Main.m3"
 /* set_source_line */
#line 392 "../Main.m3"
#line 393 "../Main.m3"
 /* start_call_direct */
#line 393 "../Main.m3"
 /* call_direct */
#line 393 "../Main.m3"
Main__Test_AtomicAddress_LoadStore(
 );
#line 393 "../Main.m3"
 /* set_source_line */
#line 393 "../Main.m3"
#line 394 "../Main.m3"
 /* start_call_direct */
#line 394 "../Main.m3"
 /* call_direct */
#line 394 "../Main.m3"
Main__Test_AtomicAddress_Swap(
 );
#line 394 "../Main.m3"
 /* set_source_line */
#line 394 "../Main.m3"
#line 395 "../Main.m3"
 /* exit_proc */
#line 395 "../Main.m3"
return;
#line 395 "../Main.m3"
 /* end_procedure */
#line 395 "../Main.m3"
} /* Test_AtomicInteger_Fence */
#line 395 "../Main.m3"
 /* set_source_line */
#line 395 "../Main.m3"
#line 402 "../Main.m3"
 /* begin_procedure */
#line 402 "../Main.m3"
struct Main__Test_AtomicInteger_Fence_Frame_t {
#line 402 "../Main.m3"
ADDRESS _unused;
#line 402 "../Main.m3"
};
#line 402 "../Main.m3"
void /*TypeText3*/ 
__cdecl
Main__Test_AtomicInteger_Fence(void)
{
#line 402 "../Main.m3"
Main__Test_AtomicInteger_Fence_Frame_t _frame;
#line 402 "../Main.m3"
_frame._unused=(ADDRESS)&_frame;
#line 402 "../Main.m3"
 /* set_source_line */
#line 402 "../Main.m3"
#line 403 "../Main.m3"
 /* set_source_line */
#line 403 "../Main.m3"
#line 404 "../Main.m3"
 /* fence */
#line 404 "../Main.m3"
m3_fence();
#line 404 "../Main.m3"
 /* set_source_line */
#line 404 "../Main.m3"
#line 405 "../Main.m3"
 /* exit_proc */
#line 405 "../Main.m3"
return;
#line 405 "../Main.m3"
 /* end_procedure */
#line 405 "../Main.m3"
} /* Test_AtomicInteger_CompareSwap */
#line 405 "../Main.m3"
 /* set_source_line */
#line 405 "../Main.m3"
#line 407 "../Main.m3"
 /* begin_procedure */
#line 407 "../Main.m3"
struct Main__Test_AtomicInteger_CompareSwap_Frame_t {
#line 407 "../Main.m3"
ADDRESS _unused;
#line 407 "../Main.m3"
};
#line 407 "../Main.m3"
void /*TypeText3*/ 
__cdecl
Main__Test_AtomicInteger_CompareSwap(void)
{
#line 407 "../Main.m3"
Main__Test_AtomicInteger_CompareSwap_Frame_t _frame;
#line 407 "../Main.m3"
_frame._unused=(ADDRESS)&_frame;
#line 407 "../Main.m3"
 /* set_source_line */
#line 407 "../Main.m3"
#line 408 "../Main.m3"
 /* set_source_line */
#line 408 "../Main.m3"
#line 409 "../Main.m3"
 /* load_address */
#line 409 "../Main.m3"
 /* load_address */
#line 409 "../Main.m3"
 /* load */
#line 409 "../Main.m3"
 /* compare_exchange */
#line 409 "../Main.m3"
 /* store */
#line 409 "../Main.m3"
(*(UINT8*)((232)+(char*)(&Main_m_M_Main_L_20)))=(INT64)(((INT64)(*((INT64*)(INT64_(160)+((ADDRESS)(&Main_m_M_Main_L_20)))))));
#line 409 "../Main.m3"
 /* set_source_line */
#line 409 "../Main.m3"
#line 410 "../Main.m3"
 /* load_address */
#line 410 "../Main.m3"
 /* load */
#line 410 "../Main.m3"
 /* exchange */
#line 410 "../Main.m3"
 /* store */
#line 410 "../Main.m3"
(*(INT64*)((160)+(char*)(&Main_m_M_Main_L_20)))=(INT64)(((INT64)(*((INT64*)(INT64_(152)+((ADDRESS)(&Main_m_M_Main_L_20)))))));
#line 410 "../Main.m3"
 /* set_source_line */
#line 410 "../Main.m3"
#line 411 "../Main.m3"
 /* exit_proc */
#line 411 "../Main.m3"
return;
#line 411 "../Main.m3"
 /* end_procedure */
#line 411 "../Main.m3"
} /* Test_AtomicInteger_FetchAnd */
#line 411 "../Main.m3"
 /* set_source_line */
#line 411 "../Main.m3"
#line 413 "../Main.m3"
 /* begin_procedure */
#line 413 "../Main.m3"
struct Main__Test_AtomicInteger_FetchAnd_Frame_t {
#line 413 "../Main.m3"
ADDRESS _unused;
#line 413 "../Main.m3"
};
#line 413 "../Main.m3"
void /*TypeText3*/ 
__cdecl
Main__Test_AtomicInteger_FetchAnd(void)
{
#line 413 "../Main.m3"
Main__Test_AtomicInteger_FetchAnd_Frame_t _frame;
#line 413 "../Main.m3"
_frame._unused=(ADDRESS)&_frame;
#line 413 "../Main.m3"
 /* set_source_line */
#line 413 "../Main.m3"
#line 414 "../Main.m3"
 /* set_source_line */
#line 414 "../Main.m3"
#line 415 "../Main.m3"
 /* load_address */
#line 415 "../Main.m3"
 /* load */
#line 415 "../Main.m3"
 /* fetch_and_op */
#line 415 "../Main.m3"
 /* store */
#line 415 "../Main.m3"
(*(INT64*)((160)+(char*)(&Main_m_M_Main_L_20)))=(INT64)(((INT64)(*((INT64*)(INT64_(152)+((ADDRESS)(&Main_m_M_Main_L_20)))))));
#line 415 "../Main.m3"
 /* set_source_line */
#line 415 "../Main.m3"
#line 416 "../Main.m3"
 /* exit_proc */
#line 416 "../Main.m3"
return;
#line 416 "../Main.m3"
 /* end_procedure */
#line 416 "../Main.m3"
} /* Test_AtomicInteger_FetchDec */
#line 416 "../Main.m3"
 /* set_source_line */
#line 416 "../Main.m3"
#line 418 "../Main.m3"
 /* begin_procedure */
#line 418 "../Main.m3"
struct Main__Test_AtomicInteger_FetchDec_Frame_t {
#line 418 "../Main.m3"
ADDRESS _unused;
#line 418 "../Main.m3"
};
#line 418 "../Main.m3"
void /*TypeText3*/ 
__cdecl
Main__Test_AtomicInteger_FetchDec(void)
{
#line 418 "../Main.m3"
Main__Test_AtomicInteger_FetchDec_Frame_t _frame;
#line 418 "../Main.m3"
_frame._unused=(ADDRESS)&_frame;
#line 418 "../Main.m3"
 /* set_source_line */
#line 418 "../Main.m3"
#line 419 "../Main.m3"
 /* set_source_line */
#line 419 "../Main.m3"
#line 420 "../Main.m3"
 /* load_address */
#line 420 "../Main.m3"
 /* load_integer */
#line 420 "../Main.m3"
 /* fetch_and_op */
#line 420 "../Main.m3"
 /* store */
#line 420 "../Main.m3"
(*(INT64*)((160)+(char*)(&Main_m_M_Main_L_20)))=(INT64)(  INT64_(1));
#line 420 "../Main.m3"
 /* set_source_line */
#line 420 "../Main.m3"
#line 421 "../Main.m3"
 /* exit_proc */
#line 421 "../Main.m3"
return;
#line 421 "../Main.m3"
 /* end_procedure */
#line 421 "../Main.m3"
} /* Test_AtomicInteger_FetchInc */
#line 421 "../Main.m3"
 /* set_source_line */
#line 421 "../Main.m3"
#line 423 "../Main.m3"
 /* begin_procedure */
#line 423 "../Main.m3"
struct Main__Test_AtomicInteger_FetchInc_Frame_t {
#line 423 "../Main.m3"
ADDRESS _unused;
#line 423 "../Main.m3"
};
#line 423 "../Main.m3"
void /*TypeText3*/ 
__cdecl
Main__Test_AtomicInteger_FetchInc(void)
{
#line 423 "../Main.m3"
Main__Test_AtomicInteger_FetchInc_Frame_t _frame;
#line 423 "../Main.m3"
_frame._unused=(ADDRESS)&_frame;
#line 423 "../Main.m3"
 /* set_source_line */
#line 423 "../Main.m3"
#line 424 "../Main.m3"
 /* set_source_line */
#line 424 "../Main.m3"
#line 425 "../Main.m3"
 /* load_address */
#line 425 "../Main.m3"
 /* load_integer */
#line 425 "../Main.m3"
 /* fetch_and_op */
#line 425 "../Main.m3"
 /* store */
#line 425 "../Main.m3"
(*(INT64*)((160)+(char*)(&Main_m_M_Main_L_20)))=(INT64)(  INT64_(1));
#line 425 "../Main.m3"
 /* set_source_line */
#line 425 "../Main.m3"
#line 426 "../Main.m3"
 /* exit_proc */
#line 426 "../Main.m3"
return;
#line 426 "../Main.m3"
 /* end_procedure */
#line 426 "../Main.m3"
} /* Test_AtomicInteger_FetchOr */
#line 426 "../Main.m3"
 /* set_source_line */
#line 426 "../Main.m3"
#line 428 "../Main.m3"
 /* begin_procedure */
#line 428 "../Main.m3"
struct Main__Test_AtomicInteger_FetchOr_Frame_t {
#line 428 "../Main.m3"
ADDRESS _unused;
#line 428 "../Main.m3"
};
#line 428 "../Main.m3"
void /*TypeText3*/ 
__cdecl
Main__Test_AtomicInteger_FetchOr(void)
{
#line 428 "../Main.m3"
Main__Test_AtomicInteger_FetchOr_Frame_t _frame;
#line 428 "../Main.m3"
_frame._unused=(ADDRESS)&_frame;
#line 428 "../Main.m3"
 /* set_source_line */
#line 428 "../Main.m3"
#line 429 "../Main.m3"
 /* set_source_line */
#line 429 "../Main.m3"
#line 430 "../Main.m3"
 /* load_address */
#line 430 "../Main.m3"
 /* load */
#line 430 "../Main.m3"
 /* fetch_and_op */
#line 430 "../Main.m3"
 /* store */
#line 430 "../Main.m3"
(*(INT64*)((160)+(char*)(&Main_m_M_Main_L_20)))=(INT64)(((INT64)(*((INT64*)(INT64_(152)+((ADDRESS)(&Main_m_M_Main_L_20)))))));
#line 430 "../Main.m3"
 /* set_source_line */
#line 430 "../Main.m3"
#line 431 "../Main.m3"
 /* exit_proc */
#line 431 "../Main.m3"
return;
#line 431 "../Main.m3"
 /* end_procedure */
#line 431 "../Main.m3"
} /* Test_AtomicInteger_FetchXor */
#line 431 "../Main.m3"
 /* set_source_line */
#line 431 "../Main.m3"
#line 433 "../Main.m3"
 /* begin_procedure */
#line 433 "../Main.m3"
struct Main__Test_AtomicInteger_FetchXor_Frame_t {
#line 433 "../Main.m3"
ADDRESS _unused;
#line 433 "../Main.m3"
};
#line 433 "../Main.m3"
void /*TypeText3*/ 
__cdecl
Main__Test_AtomicInteger_FetchXor(void)
{
#line 433 "../Main.m3"
Main__Test_AtomicInteger_FetchXor_Frame_t _frame;
#line 433 "../Main.m3"
_frame._unused=(ADDRESS)&_frame;
#line 433 "../Main.m3"
 /* set_source_line */
#line 433 "../Main.m3"
#line 434 "../Main.m3"
 /* set_source_line */
#line 434 "../Main.m3"
#line 435 "../Main.m3"
 /* load_address */
#line 435 "../Main.m3"
 /* load */
#line 435 "../Main.m3"
 /* fetch_and_op */
#line 435 "../Main.m3"
 /* store */
#line 435 "../Main.m3"
(*(INT64*)((160)+(char*)(&Main_m_M_Main_L_20)))=(INT64)(((INT64)(*((INT64*)(INT64_(152)+((ADDRESS)(&Main_m_M_Main_L_20)))))));
#line 435 "../Main.m3"
 /* set_source_line */
#line 435 "../Main.m3"
#line 436 "../Main.m3"
 /* exit_proc */
#line 436 "../Main.m3"
return;
#line 436 "../Main.m3"
 /* end_procedure */
#line 436 "../Main.m3"
} /* Test_AtomicInteger_IsLockFree */
#line 436 "../Main.m3"
 /* set_source_line */
#line 436 "../Main.m3"
#line 438 "../Main.m3"
 /* begin_procedure */
#line 438 "../Main.m3"
struct Main__Test_AtomicInteger_IsLockFree_Frame_t {
#line 438 "../Main.m3"
ADDRESS _unused;
#line 438 "../Main.m3"
};
#line 438 "../Main.m3"
BOOLEAN
__cdecl
Main__Test_AtomicInteger_IsLockFree(void)
{
#line 438 "../Main.m3"
Main__Test_AtomicInteger_IsLockFree_Frame_t _frame;
#line 438 "../Main.m3"
_frame._unused=(ADDRESS)&_frame;
#line 438 "../Main.m3"
 /* set_source_line */
#line 438 "../Main.m3"
#line 439 "../Main.m3"
 /* set_source_line */
#line 439 "../Main.m3"
#line 440 "../Main.m3"
 /* load_integer */
#line 440 "../Main.m3"
 /* exit_proc */
#line 440 "../Main.m3"
return  INT64_(1);
#line 440 "../Main.m3"
 /* end_procedure */
#line 440 "../Main.m3"
} /* Test_AtomicInteger_LoadStore */
#line 440 "../Main.m3"
 /* set_source_line */
#line 440 "../Main.m3"
#line 443 "../Main.m3"
 /* begin_procedure */
#line 443 "../Main.m3"
struct Main__Test_AtomicInteger_LoadStore_Frame_t {
#line 443 "../Main.m3"
ADDRESS _unused;
#line 443 "../Main.m3"
};
#line 443 "../Main.m3"
void /*TypeText3*/ 
__cdecl
Main__Test_AtomicInteger_LoadStore(void)
{
#line 443 "../Main.m3"
 /* Var_Type1 */ INTEGER integerC_L_32={0};//always-init
#line 443 "../Main.m3"
Main__Test_AtomicInteger_LoadStore_Frame_t _frame;
#line 443 "../Main.m3"
_frame._unused=(ADDRESS)&_frame;
#line 443 "../Main.m3"
 /* set_source_line */
#line 443 "../Main.m3"
#line 444 "../Main.m3"
 /* set_source_line */
#line 444 "../Main.m3"
#line 446 "../Main.m3"
 /* load_address */
#line 446 "../Main.m3"
 /* load_ordered */
#line 446 "../Main.m3"
 /* load_indirect */
#line 446 "../Main.m3"
 /* store */
#line 446 "../Main.m3"
(*(INT64*)((152)+(char*)(&Main_m_M_Main_L_20)))=(INT64)( *((INT64*)(INT64_(112)+((ADDRESS)(&Main_m_M_Main_L_20)))));
#line 446 "../Main.m3"
 /* set_source_line */
#line 446 "../Main.m3"
#line 447 "../Main.m3"
 /* load_address */
#line 447 "../Main.m3"
 /* load_integer */
#line 447 "../Main.m3"
 /* store_ordered => store_helper */
#line 447 "../Main.m3"
(*(INT64*)(INT64_(112)+((ADDRESS)(&Main_m_M_Main_L_20))))=(INT64)( INT64_(6));
#line 447 "../Main.m3"
 /* set_source_line */
#line 447 "../Main.m3"
#line 448 "../Main.m3"
 /* load_address */
#line 448 "../Main.m3"
 /* load_ordered */
#line 448 "../Main.m3"
 /* load_indirect */
#line 448 "../Main.m3"
 /* store */
#line 448 "../Main.m3"
(*(INT64*)((152)+(char*)(&Main_m_M_Main_L_20)))=(INT64)( *((INT64*)(INT64_(112)+((ADDRESS)(&Main_m_M_Main_L_20)))));
#line 448 "../Main.m3"
 /* set_source_line */
#line 448 "../Main.m3"
#line 449 "../Main.m3"
 /* load_address */
#line 449 "../Main.m3"
 /* load_integer */
#line 449 "../Main.m3"
 /* store_ordered => store_helper */
#line 449 "../Main.m3"
(*(INT64*)(INT64_(112)+((ADDRESS)(&Main_m_M_Main_L_20))))=(INT64)( INT64_(10));
#line 449 "../Main.m3"
 /* set_source_line */
#line 449 "../Main.m3"
#line 451 "../Main.m3"
 /* load_address */
#line 451 "../Main.m3"
 /* load_ordered */
#line 451 "../Main.m3"
 /* load_indirect */
#line 451 "../Main.m3"
 /* store */
#line 451 "../Main.m3"
(*(INT64*)(&integerC_L_32))=(INT64)( *((INT64*)(INT64_(112)+((ADDRESS)(&Main_m_M_Main_L_20)))));
#line 451 "../Main.m3"
 /* set_source_line */
#line 451 "../Main.m3"
#line 452 "../Main.m3"
 /* load_address */
#line 452 "../Main.m3"
 /* load_ordered */
#line 452 "../Main.m3"
 /* load_indirect */
#line 452 "../Main.m3"
 /* store */
#line 452 "../Main.m3"
(*(INT64*)(&integerC_L_32))=(INT64)( *((INT64*)(INT64_(112)+((ADDRESS)(&Main_m_M_Main_L_20)))));
#line 452 "../Main.m3"
 /* set_source_line */
#line 452 "../Main.m3"
#line 453 "../Main.m3"
 /* exit_proc */
#line 453 "../Main.m3"
return;
#line 453 "../Main.m3"
 /* end_procedure */
#line 453 "../Main.m3"
} /* Test_AtomicInteger_Swap */
#line 453 "../Main.m3"
 /* set_source_line */
#line 453 "../Main.m3"
#line 455 "../Main.m3"
 /* begin_procedure */
#line 455 "../Main.m3"
struct Main__Test_AtomicInteger_Swap_Frame_t {
#line 455 "../Main.m3"
ADDRESS _unused;
#line 455 "../Main.m3"
};
#line 455 "../Main.m3"
void /*TypeText3*/ 
__cdecl
Main__Test_AtomicInteger_Swap(void)
{
#line 455 "../Main.m3"
Main__Test_AtomicInteger_Swap_Frame_t _frame;
#line 455 "../Main.m3"
_frame._unused=(ADDRESS)&_frame;
#line 455 "../Main.m3"
 /* set_source_line */
#line 455 "../Main.m3"
#line 456 "../Main.m3"
 /* set_source_line */
#line 456 "../Main.m3"
#line 457 "../Main.m3"
 /* load_address */
#line 457 "../Main.m3"
 /* load */
#line 457 "../Main.m3"
 /* exchange */
#line 457 "../Main.m3"
 /* store */
#line 457 "../Main.m3"
(*(INT64*)((160)+(char*)(&Main_m_M_Main_L_20)))=(INT64)(((INT64)(*((INT64*)(INT64_(152)+((ADDRESS)(&Main_m_M_Main_L_20)))))));
#line 457 "../Main.m3"
 /* set_source_line */
#line 457 "../Main.m3"
#line 458 "../Main.m3"
 /* exit_proc */
#line 458 "../Main.m3"
return;
#line 458 "../Main.m3"
 /* end_procedure */
#line 458 "../Main.m3"
} /* Test_AtomicInteger */
#line 458 "../Main.m3"
 /* set_source_line */
#line 458 "../Main.m3"
#line 460 "../Main.m3"
 /* begin_procedure */
#line 460 "../Main.m3"
struct Main__Test_AtomicInteger_Frame_t {
#line 460 "../Main.m3"
ADDRESS _unused;
#line 460 "../Main.m3"
};
#line 460 "../Main.m3"
void /*TypeText3*/ 
__cdecl
Main__Test_AtomicInteger(void)
{
#line 460 "../Main.m3"
 /* Var_Type2 */ INT64 Main_m_50_L_51={0};//always-init
#line 460 "../Main.m3"
Main__Test_AtomicInteger_Frame_t _frame;
#line 460 "../Main.m3"
_frame._unused=(ADDRESS)&_frame;
#line 460 "../Main.m3"
 /* set_source_line */
#line 460 "../Main.m3"
#line 461 "../Main.m3"
 /* set_source_line */
#line 461 "../Main.m3"
#line 462 "../Main.m3"
 /* start_call_direct */
#line 462 "../Main.m3"
 /* call_direct */
#line 462 "../Main.m3"
Main__Test_AtomicInteger_Fence(
 );
#line 462 "../Main.m3"
 /* set_source_line */
#line 462 "../Main.m3"
#line 463 "../Main.m3"
 /* start_call_direct */
#line 463 "../Main.m3"
 /* call_direct */
#line 463 "../Main.m3"
Main__Test_AtomicInteger_CompareSwap(
 );
#line 463 "../Main.m3"
 /* set_source_line */
#line 463 "../Main.m3"
#line 464 "../Main.m3"
 /* start_call_direct */
#line 464 "../Main.m3"
 /* call_direct */
#line 464 "../Main.m3"
Main__Test_AtomicInteger_FetchAnd(
 );
#line 464 "../Main.m3"
 /* set_source_line */
#line 464 "../Main.m3"
#line 465 "../Main.m3"
 /* start_call_direct */
#line 465 "../Main.m3"
 /* call_direct */
#line 465 "../Main.m3"
Main__Test_AtomicInteger_FetchDec(
 );
#line 465 "../Main.m3"
 /* set_source_line */
#line 465 "../Main.m3"
#line 466 "../Main.m3"
 /* start_call_direct */
#line 466 "../Main.m3"
 /* call_direct */
#line 466 "../Main.m3"
Main__Test_AtomicInteger_FetchInc(
 );
#line 466 "../Main.m3"
 /* set_source_line */
#line 466 "../Main.m3"
#line 467 "../Main.m3"
 /* start_call_direct */
#line 467 "../Main.m3"
 /* call_direct */
#line 467 "../Main.m3"
Main__Test_AtomicInteger_FetchOr(
 );
#line 467 "../Main.m3"
 /* set_source_line */
#line 467 "../Main.m3"
#line 468 "../Main.m3"
 /* start_call_direct */
#line 468 "../Main.m3"
 /* call_direct */
#line 468 "../Main.m3"
Main__Test_AtomicInteger_FetchXor(
 );
#line 468 "../Main.m3"
 /* set_source_line */
#line 468 "../Main.m3"
#line 469 "../Main.m3"
 /* start_call_direct */
#line 469 "../Main.m3"
 /* call_direct */
#line 469 "../Main.m3"
 /* store */
#line 469 "../Main.m3"
(*(INT64*)(&Main_m_50_L_51))=(INT64)(((INT64)(Main__Test_AtomicInteger_IsLockFree(
 ))));
#line 469 "../Main.m3"
 /* set_source_line */
#line 469 "../Main.m3"
#line 470 "../Main.m3"
 /* start_call_direct */
#line 470 "../Main.m3"
 /* call_direct */
#line 470 "../Main.m3"
Main__Test_AtomicInteger_LoadStore(
 );
#line 470 "../Main.m3"
 /* set_source_line */
#line 470 "../Main.m3"
#line 471 "../Main.m3"
 /* start_call_direct */
#line 471 "../Main.m3"
 /* call_direct */
#line 471 "../Main.m3"
Main__Test_AtomicInteger_Swap(
 );
#line 471 "../Main.m3"
 /* set_source_line */
#line 471 "../Main.m3"
#line 472 "../Main.m3"
 /* exit_proc */
#line 472 "../Main.m3"
return;
#line 472 "../Main.m3"
 /* end_procedure */
#line 472 "../Main.m3"
} /* Test_AtomicLongint_Fence */
#line 472 "../Main.m3"
 /* set_source_line */
#line 472 "../Main.m3"
#line 477 "../Main.m3"
 /* begin_procedure */
#line 477 "../Main.m3"
struct Main__Test_AtomicLongint_Fence_Frame_t {
#line 477 "../Main.m3"
ADDRESS _unused;
#line 477 "../Main.m3"
};
#line 477 "../Main.m3"
void /*TypeText3*/ 
__cdecl
Main__Test_AtomicLongint_Fence(void)
{
#line 477 "../Main.m3"
Main__Test_AtomicLongint_Fence_Frame_t _frame;
#line 477 "../Main.m3"
_frame._unused=(ADDRESS)&_frame;
#line 477 "../Main.m3"
 /* set_source_line */
#line 477 "../Main.m3"
#line 478 "../Main.m3"
 /* set_source_line */
#line 478 "../Main.m3"
#line 479 "../Main.m3"
 /* fence */
#line 479 "../Main.m3"
m3_fence();
#line 479 "../Main.m3"
 /* set_source_line */
#line 479 "../Main.m3"
#line 480 "../Main.m3"
 /* exit_proc */
#line 480 "../Main.m3"
return;
#line 480 "../Main.m3"
 /* end_procedure */
#line 480 "../Main.m3"
} /* Test_AtomicLongint_CompareSwap */
#line 480 "../Main.m3"
 /* set_source_line */
#line 480 "../Main.m3"
#line 482 "../Main.m3"
 /* begin_procedure */
#line 482 "../Main.m3"
struct Main__Test_AtomicLongint_CompareSwap_Frame_t {
#line 482 "../Main.m3"
ADDRESS _unused;
#line 482 "../Main.m3"
};
#line 482 "../Main.m3"
void /*TypeText3*/ 
__cdecl
Main__Test_AtomicLongint_CompareSwap(void)
{
#line 482 "../Main.m3"
Main__Test_AtomicLongint_CompareSwap_Frame_t _frame;
#line 482 "../Main.m3"
_frame._unused=(ADDRESS)&_frame;
#line 482 "../Main.m3"
 /* set_source_line */
#line 482 "../Main.m3"
#line 483 "../Main.m3"
 /* set_source_line */
#line 483 "../Main.m3"
#line 484 "../Main.m3"
 /* load_address */
#line 484 "../Main.m3"
 /* load_address */
#line 484 "../Main.m3"
 /* load */
#line 484 "../Main.m3"
 /* compare_exchange */
#line 484 "../Main.m3"
 /* store */
#line 484 "../Main.m3"
(*(UINT8*)((232)+(char*)(&Main_m_M_Main_L_20)))=(INT64)(((INT64)(*((INT64*)(INT64_(184)+((ADDRESS)(&Main_m_M_Main_L_20)))))));
#line 484 "../Main.m3"
 /* set_source_line */
#line 484 "../Main.m3"
#line 485 "../Main.m3"
 /* exit_proc */
#line 485 "../Main.m3"
return;
#line 485 "../Main.m3"
 /* end_procedure */
#line 485 "../Main.m3"
} /* Test_AtomicLongint_FetchAnd */
#line 485 "../Main.m3"
 /* set_source_line */
#line 485 "../Main.m3"
#line 487 "../Main.m3"
 /* begin_procedure */
#line 487 "../Main.m3"
struct Main__Test_AtomicLongint_FetchAnd_Frame_t {
#line 487 "../Main.m3"
ADDRESS _unused;
#line 487 "../Main.m3"
};
#line 487 "../Main.m3"
void /*TypeText3*/ 
__cdecl
Main__Test_AtomicLongint_FetchAnd(void)
{
#line 487 "../Main.m3"
Main__Test_AtomicLongint_FetchAnd_Frame_t _frame;
#line 487 "../Main.m3"
_frame._unused=(ADDRESS)&_frame;
#line 487 "../Main.m3"
 /* set_source_line */
#line 487 "../Main.m3"
#line 488 "../Main.m3"
 /* set_source_line */
#line 488 "../Main.m3"
#line 489 "../Main.m3"
 /* load_address */
#line 489 "../Main.m3"
 /* load */
#line 489 "../Main.m3"
 /* fetch_and_op */
#line 489 "../Main.m3"
 /* store */
#line 489 "../Main.m3"
(*(INT64*)((184)+(char*)(&Main_m_M_Main_L_20)))=(INT64)(((INT64)(*((INT64*)(INT64_(176)+((ADDRESS)(&Main_m_M_Main_L_20)))))));
#line 489 "../Main.m3"
 /* set_source_line */
#line 489 "../Main.m3"
#line 490 "../Main.m3"
 /* exit_proc */
#line 490 "../Main.m3"
return;
#line 490 "../Main.m3"
 /* end_procedure */
#line 490 "../Main.m3"
} /* Test_AtomicLongint_FetchDec */
#line 490 "../Main.m3"
 /* set_source_line */
#line 490 "../Main.m3"
#line 492 "../Main.m3"
 /* begin_procedure */
#line 492 "../Main.m3"
struct Main__Test_AtomicLongint_FetchDec_Frame_t {
#line 492 "../Main.m3"
ADDRESS _unused;
#line 492 "../Main.m3"
};
#line 492 "../Main.m3"
void /*TypeText3*/ 
__cdecl
Main__Test_AtomicLongint_FetchDec(void)
{
#line 492 "../Main.m3"
Main__Test_AtomicLongint_FetchDec_Frame_t _frame;
#line 492 "../Main.m3"
_frame._unused=(ADDRESS)&_frame;
#line 492 "../Main.m3"
 /* set_source_line */
#line 492 "../Main.m3"
#line 493 "../Main.m3"
 /* set_source_line */
#line 493 "../Main.m3"
#line 494 "../Main.m3"
 /* load_integer */
#line 494 "../Main.m3"
 /* loophole */
#line 494 "../Main.m3"
 /* load_address */
#line 494 "../Main.m3"
 /* swap */
#line 494 "../Main.m3"
 /* fetch_and_op */
#line 494 "../Main.m3"
 /* store */
#line 494 "../Main.m3"
(*(INT64*)((176)+(char*)(&Main_m_M_Main_L_20)))=(INT64)(((INT64)((INT64) INT64_(1))));
#line 494 "../Main.m3"
 /* set_source_line */
#line 494 "../Main.m3"
#line 495 "../Main.m3"
 /* exit_proc */
#line 495 "../Main.m3"
return;
#line 495 "../Main.m3"
 /* end_procedure */
#line 495 "../Main.m3"
} /* Test_AtomicLongint_FetchInc */
#line 495 "../Main.m3"
 /* set_source_line */
#line 495 "../Main.m3"
#line 497 "../Main.m3"
 /* begin_procedure */
#line 497 "../Main.m3"
struct Main__Test_AtomicLongint_FetchInc_Frame_t {
#line 497 "../Main.m3"
ADDRESS _unused;
#line 497 "../Main.m3"
};
#line 497 "../Main.m3"
void /*TypeText3*/ 
__cdecl
Main__Test_AtomicLongint_FetchInc(void)
{
#line 497 "../Main.m3"
Main__Test_AtomicLongint_FetchInc_Frame_t _frame;
#line 497 "../Main.m3"
_frame._unused=(ADDRESS)&_frame;
#line 497 "../Main.m3"
 /* set_source_line */
#line 497 "../Main.m3"
#line 498 "../Main.m3"
 /* set_source_line */
#line 498 "../Main.m3"
#line 499 "../Main.m3"
 /* load_integer */
#line 499 "../Main.m3"
 /* loophole */
#line 499 "../Main.m3"
 /* load_address */
#line 499 "../Main.m3"
 /* swap */
#line 499 "../Main.m3"
 /* fetch_and_op */
#line 499 "../Main.m3"
 /* store */
#line 499 "../Main.m3"
(*(INT64*)((176)+(char*)(&Main_m_M_Main_L_20)))=(INT64)(((INT64)((INT64) INT64_(1))));
#line 499 "../Main.m3"
 /* set_source_line */
#line 499 "../Main.m3"
#line 500 "../Main.m3"
 /* exit_proc */
#line 500 "../Main.m3"
return;
#line 500 "../Main.m3"
 /* end_procedure */
#line 500 "../Main.m3"
} /* Test_AtomicLongint_FetchOr */
#line 500 "../Main.m3"
 /* set_source_line */
#line 500 "../Main.m3"
#line 502 "../Main.m3"
 /* begin_procedure */
#line 502 "../Main.m3"
struct Main__Test_AtomicLongint_FetchOr_Frame_t {
#line 502 "../Main.m3"
ADDRESS _unused;
#line 502 "../Main.m3"
};
#line 502 "../Main.m3"
void /*TypeText3*/ 
__cdecl
Main__Test_AtomicLongint_FetchOr(void)
{
#line 502 "../Main.m3"
Main__Test_AtomicLongint_FetchOr_Frame_t _frame;
#line 502 "../Main.m3"
_frame._unused=(ADDRESS)&_frame;
#line 502 "../Main.m3"
 /* set_source_line */
#line 502 "../Main.m3"
#line 503 "../Main.m3"
 /* set_source_line */
#line 503 "../Main.m3"
#line 504 "../Main.m3"
 /* load_address */
#line 504 "../Main.m3"
 /* load */
#line 504 "../Main.m3"
 /* fetch_and_op */
#line 504 "../Main.m3"
 /* store */
#line 504 "../Main.m3"
(*(INT64*)((184)+(char*)(&Main_m_M_Main_L_20)))=(INT64)(((INT64)(*((INT64*)(INT64_(176)+((ADDRESS)(&Main_m_M_Main_L_20)))))));
#line 504 "../Main.m3"
 /* set_source_line */
#line 504 "../Main.m3"
#line 505 "../Main.m3"
 /* exit_proc */
#line 505 "../Main.m3"
return;
#line 505 "../Main.m3"
 /* end_procedure */
#line 505 "../Main.m3"
} /* Test_AtomicLongint_FetchXor */
#line 505 "../Main.m3"
 /* set_source_line */
#line 505 "../Main.m3"
#line 507 "../Main.m3"
 /* begin_procedure */
#line 507 "../Main.m3"
struct Main__Test_AtomicLongint_FetchXor_Frame_t {
#line 507 "../Main.m3"
ADDRESS _unused;
#line 507 "../Main.m3"
};
#line 507 "../Main.m3"
void /*TypeText3*/ 
__cdecl
Main__Test_AtomicLongint_FetchXor(void)
{
#line 507 "../Main.m3"
Main__Test_AtomicLongint_FetchXor_Frame_t _frame;
#line 507 "../Main.m3"
_frame._unused=(ADDRESS)&_frame;
#line 507 "../Main.m3"
 /* set_source_line */
#line 507 "../Main.m3"
#line 508 "../Main.m3"
 /* set_source_line */
#line 508 "../Main.m3"
#line 509 "../Main.m3"
 /* load_address */
#line 509 "../Main.m3"
 /* load */
#line 509 "../Main.m3"
 /* fetch_and_op */
#line 509 "../Main.m3"
 /* store */
#line 509 "../Main.m3"
(*(INT64*)((184)+(char*)(&Main_m_M_Main_L_20)))=(INT64)(((INT64)(*((INT64*)(INT64_(176)+((ADDRESS)(&Main_m_M_Main_L_20)))))));
#line 509 "../Main.m3"
 /* set_source_line */
#line 509 "../Main.m3"
#line 510 "../Main.m3"
 /* exit_proc */
#line 510 "../Main.m3"
return;
#line 510 "../Main.m3"
 /* end_procedure */
#line 510 "../Main.m3"
} /* Test_AtomicLongint_IsLockFree */
#line 510 "../Main.m3"
 /* set_source_line */
#line 510 "../Main.m3"
#line 512 "../Main.m3"
 /* begin_procedure */
#line 512 "../Main.m3"
struct Main__Test_AtomicLongint_IsLockFree_Frame_t {
#line 512 "../Main.m3"
ADDRESS _unused;
#line 512 "../Main.m3"
};
#line 512 "../Main.m3"
BOOLEAN
__cdecl
Main__Test_AtomicLongint_IsLockFree(void)
{
#line 512 "../Main.m3"
Main__Test_AtomicLongint_IsLockFree_Frame_t _frame;
#line 512 "../Main.m3"
_frame._unused=(ADDRESS)&_frame;
#line 512 "../Main.m3"
 /* set_source_line */
#line 512 "../Main.m3"
#line 513 "../Main.m3"
 /* set_source_line */
#line 513 "../Main.m3"
#line 514 "../Main.m3"
 /* load_integer */
#line 514 "../Main.m3"
 /* exit_proc */
#line 514 "../Main.m3"
return  INT64_(1);
#line 514 "../Main.m3"
 /* end_procedure */
#line 514 "../Main.m3"
} /* Test_AtomicLongint_Load */
#line 514 "../Main.m3"
 /* set_source_line */
#line 514 "../Main.m3"
#line 517 "../Main.m3"
 /* begin_procedure */
#line 517 "../Main.m3"
struct Main__Test_AtomicLongint_Load_Frame_t {
#line 517 "../Main.m3"
ADDRESS _unused;
#line 517 "../Main.m3"
};
#line 517 "../Main.m3"
void /*TypeText3*/ 
__cdecl
Main__Test_AtomicLongint_Load(void)
{
#line 517 "../Main.m3"
Main__Test_AtomicLongint_Load_Frame_t _frame;
#line 517 "../Main.m3"
_frame._unused=(ADDRESS)&_frame;
#line 517 "../Main.m3"
 /* set_source_line */
#line 517 "../Main.m3"
#line 518 "../Main.m3"
 /* set_source_line */
#line 518 "../Main.m3"
#line 519 "../Main.m3"
 /* load_address */
#line 519 "../Main.m3"
 /* load_ordered */
#line 519 "../Main.m3"
 /* load_indirect */
#line 519 "../Main.m3"
 /* store */
#line 519 "../Main.m3"
(*(INT64*)((176)+(char*)(&Main_m_M_Main_L_20)))=(INT64)( *((INT64*)(INT64_(120)+((ADDRESS)(&Main_m_M_Main_L_20)))));
#line 519 "../Main.m3"
 /* set_source_line */
#line 519 "../Main.m3"
#line 520 "../Main.m3"
 /* exit_proc */
#line 520 "../Main.m3"
return;
#line 520 "../Main.m3"
 /* end_procedure */
#line 520 "../Main.m3"
} /* Test_AtomicLongint_Store */
#line 520 "../Main.m3"
 /* set_source_line */
#line 520 "../Main.m3"
#line 522 "../Main.m3"
 /* begin_procedure */
#line 522 "../Main.m3"
struct Main__Test_AtomicLongint_Store_Frame_t {
#line 522 "../Main.m3"
ADDRESS _unused;
#line 522 "../Main.m3"
};
#line 522 "../Main.m3"
void /*TypeText3*/ 
__cdecl
Main__Test_AtomicLongint_Store(void)
{
#line 522 "../Main.m3"
Main__Test_AtomicLongint_Store_Frame_t _frame;
#line 522 "../Main.m3"
_frame._unused=(ADDRESS)&_frame;
#line 522 "../Main.m3"
 /* set_source_line */
#line 522 "../Main.m3"
#line 523 "../Main.m3"
 /* set_source_line */
#line 523 "../Main.m3"
#line 524 "../Main.m3"
 /* load_address */
#line 524 "../Main.m3"
 /* load */
#line 524 "../Main.m3"
 /* store_ordered => store_helper */
#line 524 "../Main.m3"
(*(INT64*)(INT64_(120)+((ADDRESS)(&Main_m_M_Main_L_20))))=(INT64)(*((INT64*)(INT64_(176)+((ADDRESS)(&Main_m_M_Main_L_20)))));
#line 524 "../Main.m3"
 /* set_source_line */
#line 524 "../Main.m3"
#line 525 "../Main.m3"
 /* exit_proc */
#line 525 "../Main.m3"
return;
#line 525 "../Main.m3"
 /* end_procedure */
#line 525 "../Main.m3"
} /* Test_AtomicLongint_Swap */
#line 525 "../Main.m3"
 /* set_source_line */
#line 525 "../Main.m3"
#line 527 "../Main.m3"
 /* begin_procedure */
#line 527 "../Main.m3"
struct Main__Test_AtomicLongint_Swap_Frame_t {
#line 527 "../Main.m3"
ADDRESS _unused;
#line 527 "../Main.m3"
};
#line 527 "../Main.m3"
void /*TypeText3*/ 
__cdecl
Main__Test_AtomicLongint_Swap(void)
{
#line 527 "../Main.m3"
Main__Test_AtomicLongint_Swap_Frame_t _frame;
#line 527 "../Main.m3"
_frame._unused=(ADDRESS)&_frame;
#line 527 "../Main.m3"
 /* set_source_line */
#line 527 "../Main.m3"
#line 528 "../Main.m3"
 /* set_source_line */
#line 528 "../Main.m3"
#line 529 "../Main.m3"
 /* load_address */
#line 529 "../Main.m3"
 /* load */
#line 529 "../Main.m3"
 /* exchange */
#line 529 "../Main.m3"
 /* store */
#line 529 "../Main.m3"
(*(INT64*)((184)+(char*)(&Main_m_M_Main_L_20)))=(INT64)(((INT64)(*((INT64*)(INT64_(176)+((ADDRESS)(&Main_m_M_Main_L_20)))))));
#line 529 "../Main.m3"
 /* set_source_line */
#line 529 "../Main.m3"
#line 530 "../Main.m3"
 /* exit_proc */
#line 530 "../Main.m3"
return;
#line 530 "../Main.m3"
 /* end_procedure */
#line 530 "../Main.m3"
} /* Test_AtomicLongint */
#line 530 "../Main.m3"
 /* set_source_line */
#line 530 "../Main.m3"
#line 533 "../Main.m3"
 /* begin_procedure */
#line 533 "../Main.m3"
struct Main__Test_AtomicLongint_Frame_t {
#line 533 "../Main.m3"
ADDRESS _unused;
#line 533 "../Main.m3"
};
#line 533 "../Main.m3"
void /*TypeText3*/ 
__cdecl
Main__Test_AtomicLongint(void)
{
#line 533 "../Main.m3"
 /* Var_Type2 */ INT64 Main_m_52_L_53={0};//always-init
#line 533 "../Main.m3"
Main__Test_AtomicLongint_Frame_t _frame;
#line 533 "../Main.m3"
_frame._unused=(ADDRESS)&_frame;
#line 533 "../Main.m3"
 /* set_source_line */
#line 533 "../Main.m3"
#line 534 "../Main.m3"
 /* set_source_line */
#line 534 "../Main.m3"
#line 535 "../Main.m3"
 /* start_call_direct */
#line 535 "../Main.m3"
 /* call_direct */
#line 535 "../Main.m3"
Main__Test_AtomicLongint_Fence(
 );
#line 535 "../Main.m3"
 /* set_source_line */
#line 535 "../Main.m3"
#line 536 "../Main.m3"
 /* start_call_direct */
#line 536 "../Main.m3"
 /* call_direct */
#line 536 "../Main.m3"
Main__Test_AtomicLongint_CompareSwap(
 );
#line 536 "../Main.m3"
 /* set_source_line */
#line 536 "../Main.m3"
#line 537 "../Main.m3"
 /* start_call_direct */
#line 537 "../Main.m3"
 /* call_direct */
#line 537 "../Main.m3"
Main__Test_AtomicLongint_FetchAnd(
 );
#line 537 "../Main.m3"
 /* set_source_line */
#line 537 "../Main.m3"
#line 538 "../Main.m3"
 /* start_call_direct */
#line 538 "../Main.m3"
 /* call_direct */
#line 538 "../Main.m3"
Main__Test_AtomicLongint_FetchDec(
 );
#line 538 "../Main.m3"
 /* set_source_line */
#line 538 "../Main.m3"
#line 539 "../Main.m3"
 /* start_call_direct */
#line 539 "../Main.m3"
 /* call_direct */
#line 539 "../Main.m3"
Main__Test_AtomicLongint_FetchInc(
 );
#line 539 "../Main.m3"
 /* set_source_line */
#line 539 "../Main.m3"
#line 540 "../Main.m3"
 /* start_call_direct */
#line 540 "../Main.m3"
 /* call_direct */
#line 540 "../Main.m3"
Main__Test_AtomicLongint_FetchOr(
 );
#line 540 "../Main.m3"
 /* set_source_line */
#line 540 "../Main.m3"
#line 541 "../Main.m3"
 /* start_call_direct */
#line 541 "../Main.m3"
 /* call_direct */
#line 541 "../Main.m3"
Main__Test_AtomicLongint_FetchXor(
 );
#line 541 "../Main.m3"
 /* set_source_line */
#line 541 "../Main.m3"
#line 542 "../Main.m3"
 /* start_call_direct */
#line 542 "../Main.m3"
 /* call_direct */
#line 542 "../Main.m3"
 /* store */
#line 542 "../Main.m3"
(*(INT64*)(&Main_m_52_L_53))=(INT64)(((INT64)(Main__Test_AtomicLongint_IsLockFree(
 ))));
#line 542 "../Main.m3"
 /* set_source_line */
#line 542 "../Main.m3"
#line 543 "../Main.m3"
 /* start_call_direct */
#line 543 "../Main.m3"
 /* call_direct */
#line 543 "../Main.m3"
Main__Test_AtomicLongint_Load(
 );
#line 543 "../Main.m3"
 /* set_source_line */
#line 543 "../Main.m3"
#line 544 "../Main.m3"
 /* start_call_direct */
#line 544 "../Main.m3"
 /* call_direct */
#line 544 "../Main.m3"
Main__Test_AtomicLongint_Store(
 );
#line 544 "../Main.m3"
 /* set_source_line */
#line 544 "../Main.m3"
#line 545 "../Main.m3"
 /* start_call_direct */
#line 545 "../Main.m3"
 /* call_direct */
#line 545 "../Main.m3"
Main__Test_AtomicLongint_Swap(
 );
#line 545 "../Main.m3"
 /* set_source_line */
#line 545 "../Main.m3"
#line 546 "../Main.m3"
 /* exit_proc */
#line 546 "../Main.m3"
return;
#line 546 "../Main.m3"
 /* end_procedure */
#line 546 "../Main.m3"
} /* Main_M3 */
#line 546 "../Main.m3"
 /* module main body Main_M3 */
#line 546 "../Main.m3"
 /* set_source_line */
#line 546 "../Main.m3"
#line 552 "../Main.m3"
 /* begin_procedure */
#line 552 "../Main.m3"
struct Main_M3_Frame_t {
#line 552 "../Main.m3"
ADDRESS _unused;
#line 552 "../Main.m3"
};
#line 552 "../Main.m3"
RT0__ModulePtr
__cdecl
Main_M3(
   /* Param_Type1 */ INTEGER /*TypeText1*/  mode_L_21)
{
#line 552 "../Main.m3"
Main_M3_Frame_t _frame;
#line 552 "../Main.m3"
_frame._unused=(ADDRESS)&_frame;
#line 552 "../Main.m3"
 /* load */
#line 552 "../Main.m3"
 /* if_true_or_false */
#line 552 "../Main.m3"
 /* load_host_integer */
#line 552 "../Main.m3"
 /* load_integer */
#line 552 "../Main.m3"
 /* if_compare */
#line 552 "../Main.m3"
if(m3_eq(INT64,
  mode_L_21,
   INT64_(0)))goto L3;
#line 552 "../Main.m3"
 /* set_source_line */
#line 552 "../Main.m3"
#line 554 "../Main.m3"
 /* start_call_direct */
#line 554 "../Main.m3"
 /* call_direct */
#line 554 "../Main.m3"
Main__Test_AtomicBoolean(
 );
#line 554 "../Main.m3"
 /* set_source_line */
#line 554 "../Main.m3"
#line 555 "../Main.m3"
 /* start_call_direct */
#line 555 "../Main.m3"
 /* call_direct */
#line 555 "../Main.m3"
Main__Test_AtomicChar(
 );
#line 555 "../Main.m3"
 /* set_source_line */
#line 555 "../Main.m3"
#line 556 "../Main.m3"
 /* start_call_direct */
#line 556 "../Main.m3"
 /* call_direct */
#line 556 "../Main.m3"
Main__Test_AtomicWidechar(
 );
#line 556 "../Main.m3"
 /* set_source_line */
#line 556 "../Main.m3"
#line 557 "../Main.m3"
 /* start_call_direct */
#line 557 "../Main.m3"
 /* call_direct */
#line 557 "../Main.m3"
Main__Test_AtomicLongint(
 );
#line 557 "../Main.m3"
 /* set_source_line */
#line 557 "../Main.m3"
#line 558 "../Main.m3"
 /* start_call_direct */
#line 558 "../Main.m3"
 /* call_direct */
#line 558 "../Main.m3"
Main__Test_AtomicInteger(
 );
#line 558 "../Main.m3"
 /* set_source_line */
#line 558 "../Main.m3"
#line 559 "../Main.m3"
 /* start_call_direct */
#line 559 "../Main.m3"
 /* call_direct */
#line 559 "../Main.m3"
Main__Test_AtomicAddress(
 );
#line 559 "../Main.m3"
 /* set_source_line */
#line 559 "../Main.m3"
#line 560 "../Main.m3"
 /* start_call_direct */
#line 560 "../Main.m3"
 /* call_direct */
#line 560 "../Main.m3"
Main__Test_AtomicRefany(
 );
#line 560 "../Main.m3"
 /* set_label */
#line 560 "../Main.m3"
L3:;
#line 560 "../Main.m3"
 /* load_address */
#line 560 "../Main.m3"
 /* exit_proc */
#line 560 "../Main.m3"
return (RT0__ModulePtr)(&Main_m_M_Main_L_20);
#line 560 "../Main.m3"
 /* end_procedure */
#line 560 "../Main.m3"
} /* global constant type descriptor */
#line 560 "../Main.m3"
 /* global data type descriptor */
#line 560 "../Main.m3"
 /* module global constants */
#line 560 "../Main.m3"
 /* procedure names */
#line 560 "../Main.m3"
 /* procedure table */
#line 560 "../Main.m3"
 /* global type map */
#line 560 "../Main.m3"
 /* file name */
#line 560 "../Main.m3"
 /* module global data */
#line 560 "../Main.m3"
 /* load map


 global data allocation for M_Main
     0   104  8  *module info*
   104     1  1  Main.atomicBooleanA
   105     1  1  Main.atomicCharA
   112     8  8  Main.atomicIntegerA
   120     8  8  Main.atomicLongintA
   128     8  8  Main.atomicRefanyA
   136     2  2  Main.atomicWidecharA
   144     8  8  Main.atomicAddressA
   152     8  8  Main.integerB
   160     8  8  Main.integerC
   168     1  1  Main.booleanB
   169     1  1  Main.booleanC
   170     1  1  Main.charB
   171     1  1  Main.charC
   176     8  8  Main.longintB
   184     8  8  Main.longintC
   192     8  8  Main.refanyB
   200     8  8  Main.refanyC
   208     2  2  Main.widecharB
   210     2  2  Main.widecharC
   216     8  8  Main.addressB
   224     8  8  Main.addressC
   232     1  1  Main.bool
   240    24  8  import Main
   264    24  8  import WideChar
   288    24  8  import Refany
   312    24  8  import Longint
   336    24  8  import Integer
   360    24  8  import Char
   384    24  8  import Boolean
   408    24  8  import Address
   432    24  8  import AtomicWideChar
   456    24  8  import AtomicRefany
   480    24  8  import AtomicLongint
   504    24  8  import AtomicInteger
   528    24  8  import AtomicChar
   552    24  8  import AtomicBoolean
   576    24  8  import AtomicAddress
   600    24  8  import RTHooks
   624    16  8  typecell ptr
   640     0  8  *TOTAL*


 global constants for M_Main
     0  2074  8  *proc names*
  2080  1272  8  *proc info*
  3352     8  1  type_map
  3360    11  1  *string*
  3376     0  8  *TOTAL*
 */
#line 560 "../Main.m3"
 /* end unit */
#line 560 "../Main.m3"

#ifdef __cplusplus

} /* extern "C" */
#endif
 /* set_runtime_proc */
 /* set_runtime_proc */
