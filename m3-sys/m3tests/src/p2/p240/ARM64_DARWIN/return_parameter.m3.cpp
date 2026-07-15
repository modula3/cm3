// library:pgm
// source_base_name:return_parameter
// target_name:return_parameter.m3.cpp
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
 /* declare_formal */

#ifndef return_parameter__INT8
#define return_parameter__INT8 return_parameter__INT8
typedef T66A2A904_8 return_parameter__INT8;
#endif
 /* declare_proctype */
 /* declare_formal */

#ifndef return_parameter__UINT64
#define return_parameter__UINT64 return_parameter__UINT64
typedef INT64 return_parameter__UINT64;
#endif
 /* declare_proctype */
 /* declare_formal */

#ifndef return_parameter__FLOAT64
#define return_parameter__FLOAT64 return_parameter__FLOAT64
typedef double return_parameter__FLOAT64;
#endif
 /* declare_proctype */
 /* declare_formal */

#ifndef return_parameter__INT32
#define return_parameter__INT32 return_parameter__INT32
typedef TADC6066D_32 return_parameter__INT32;
#endif
 /* declare_subrange */
/*subrange_define*/typedef INT64 T9CED36E7_64;
 /* declare_proctype */
 /* declare_formal */

#ifndef LONGCARD
#define LONGCARD LONGCARD
typedef T9CED36E7_64 LONGCARD;
#endif
 /* declare_proctype */
 /* declare_formal */

#ifndef return_parameter__UINT16
#define return_parameter__UINT16 return_parameter__UINT16
typedef TA4B285DE_16 return_parameter__UINT16;
#endif
 /* declare_proctype */
 /* declare_formal */
 /* declare_proctype */
 /* declare_formal */

#ifndef return_parameter__INT64
#define return_parameter__INT64 return_parameter__INT64
typedef T839F750E_64 return_parameter__INT64;
#endif
 /* declare_proctype */
 /* declare_formal */

#ifndef return_parameter__FLOAT32
#define return_parameter__FLOAT32 return_parameter__FLOAT32
typedef float return_parameter__FLOAT32;
#endif
 /* declare_proctype */
 /* declare_formal */

#ifndef return_parameter__INT16
#define return_parameter__INT16 return_parameter__INT16
typedef T7300E1E8_16 return_parameter__INT16;
#endif
 /* declare_proctype */
 /* declare_formal */
 /* declare_proctype */
 /* declare_formal */

#ifndef return_parameter__UINT32
#define return_parameter__UINT32 return_parameter__UINT32
typedef T6FA2E87D_32 return_parameter__UINT32;
#endif
 /* declare_proctype */
 /* declare_formal */

#ifndef return_parameter__UINT8
#define return_parameter__UINT8 return_parameter__UINT8
typedef TB5B30AA_8 return_parameter__UINT8;
#endif
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
 /* DeclareTypes_FlushOnce size:13 */

#if 0 /* avoid type hash collions */
typedef 
T66A2A904_8(__cdecl*TCED40579)(return_parameter__INT8);
#else
typedef void (__cdecl*TCED40579)(void);
#endif

#if 0 /* avoid type hash collions */
typedef 
INT64(__cdecl*TC48DDF28)(return_parameter__UINT64);
#else
typedef void (__cdecl*TC48DDF28)(void);
#endif

#if 0 /* avoid type hash collions */
typedef 
double(__cdecl*T6BBCAF6)(return_parameter__FLOAT64);
#else
typedef void (__cdecl*T6BBCAF6)(void);
#endif

#if 0 /* avoid type hash collions */
typedef 
TADC6066D_32(__cdecl*T4D77D69A)(return_parameter__INT32);
#else
typedef void (__cdecl*T4D77D69A)(void);
#endif

#if 0 /* avoid type hash collions */
typedef 
T9CED36E7_64(__cdecl*T2D901FAB)(LONGCARD);
#else
typedef void (__cdecl*T2D901FAB)(void);
#endif

#if 0 /* avoid type hash collions */
typedef 
TA4B285DE_16(__cdecl*TE2DB0A72)(return_parameter__UINT16);
#else
typedef void (__cdecl*TE2DB0A72)(void);
#endif

#if 0 /* avoid type hash collions */
typedef 
INTEGER(__cdecl*TB2EA0E8C)(INTEGER);
#else
typedef void (__cdecl*TB2EA0E8C)(void);
#endif

#if 0 /* avoid type hash collions */
typedef 
T839F750E_64(__cdecl*T2AE875D7)(return_parameter__INT64);
#else
typedef void (__cdecl*T2AE875D7)(void);
#endif

#if 0 /* avoid type hash collions */
typedef 
float(__cdecl*TE2C8A02D)(return_parameter__FLOAT32);
#else
typedef void (__cdecl*TE2C8A02D)(void);
#endif

#if 0 /* avoid type hash collions */
typedef 
T7300E1E8_16(__cdecl*T8EC341B0)(return_parameter__INT16);
#else
typedef void (__cdecl*T8EC341B0)(void);
#endif

#if 0 /* avoid type hash collions */
typedef 
WORD_T(__cdecl*T7C4B3997)(CARDINAL);
#else
typedef void (__cdecl*T7C4B3997)(void);
#endif

#if 0 /* avoid type hash collions */
typedef 
T6FA2E87D_32(__cdecl*T425484C0)(return_parameter__UINT32);
#else
typedef void (__cdecl*T425484C0)(void);
#endif

#if 0 /* avoid type hash collions */
typedef 
TB5B30AA_8(__cdecl*TCF68214D)(return_parameter__UINT8);
#else
typedef void (__cdecl*TCF68214D)(void);
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
/*Proc_ForwardDeclareFrameType*/struct return_parameter_I3_Frame_t;typedef struct return_parameter_I3_Frame_t return_parameter_I3_Frame_t;
 /* internal_declare_param */
RT0__ModulePtr
__cdecl
return_parameter_I3(
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
 /* end: imports */
 /* begin: locals */
 /* declare_segment name:<NIL> typeid:TFFFFFFFF const:TRUE */
/*declare_segment*/struct return_parameter_m_5_L_6_t;
/*declare_segment*/typedef struct return_parameter_m_5_L_6_t return_parameter_m_5_L_6_t;
 /* declare_segment name:M_return_parameter typeid:TFFFFFFFF const:FALSE */
 /* handler_name_prefixes:return_parameter_M3_LINE_ */
 /* handler_name_prefixes:return_parameter_I3_LINE_ */
/*declare_segment*/struct return_parameter_m_M_return_parameter_L_7_t;
/*declare_segment*/typedef struct return_parameter_m_M_return_parameter_L_7_t return_parameter_m_M_return_parameter_L_7_t;
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct return_parameter_M3_Frame_t;typedef struct return_parameter_M3_Frame_t return_parameter_M3_Frame_t;
 /* internal_declare_param */
RT0__ModulePtr
__cdecl
return_parameter_M3(
   /* Param_Type1 */ INTEGER /*TypeText1*/  mode_L_8);
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct return_parameter__ret_pi8_Frame_t;typedef struct return_parameter__ret_pi8_Frame_t return_parameter__ret_pi8_Frame_t;
 /* declare_local */
 /* internal_declare_param */
return_parameter__INT8
__cdecl
return_parameter__ret_pi8(
   /* Param_Type1 */ return_parameter__INT8 p_L_10);
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct return_parameter__ret_pu64_Frame_t;typedef struct return_parameter__ret_pu64_Frame_t return_parameter__ret_pu64_Frame_t;
 /* declare_local */
 /* internal_declare_param */
return_parameter__UINT64
__cdecl
return_parameter__ret_pu64(
   /* Param_Type1 */ return_parameter__UINT64 p_L_12);
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct return_parameter__ret_pf64_Frame_t;typedef struct return_parameter__ret_pf64_Frame_t return_parameter__ret_pf64_Frame_t;
 /* declare_local */
 /* internal_declare_param */
return_parameter__FLOAT64
__cdecl
return_parameter__ret_pf64(
   /* Param_Type1 */ return_parameter__FLOAT64 p_L_14);
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct return_parameter__ret_pi32_Frame_t;typedef struct return_parameter__ret_pi32_Frame_t return_parameter__ret_pi32_Frame_t;
 /* declare_local */
 /* internal_declare_param */
return_parameter__INT32
__cdecl
return_parameter__ret_pi32(
   /* Param_Type1 */ return_parameter__INT32 p_L_16);
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct return_parameter__ret_pLC_Frame_t;typedef struct return_parameter__ret_pLC_Frame_t return_parameter__ret_pLC_Frame_t;
 /* declare_local */
 /* internal_declare_param */
LONGCARD
__cdecl
return_parameter__ret_pLC(
   /* Param_Type1 */ LONGCARD p_L_18);
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct return_parameter__ret_pu16_Frame_t;typedef struct return_parameter__ret_pu16_Frame_t return_parameter__ret_pu16_Frame_t;
 /* declare_local */
 /* internal_declare_param */
return_parameter__UINT16
__cdecl
return_parameter__ret_pu16(
   /* Param_Type1 */ return_parameter__UINT16 p_L_20);
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct return_parameter__ret_pI_Frame_t;typedef struct return_parameter__ret_pI_Frame_t return_parameter__ret_pI_Frame_t;
 /* declare_local */
 /* internal_declare_param */
INTEGER
__cdecl
return_parameter__ret_pI(
   /* Param_Type1 */ INTEGER p_L_22);
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct return_parameter__ret_pi64_Frame_t;typedef struct return_parameter__ret_pi64_Frame_t return_parameter__ret_pi64_Frame_t;
 /* declare_local */
 /* internal_declare_param */
return_parameter__INT64
__cdecl
return_parameter__ret_pi64(
   /* Param_Type1 */ return_parameter__INT64 p_L_24);
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct return_parameter__ret_pf32_Frame_t;typedef struct return_parameter__ret_pf32_Frame_t return_parameter__ret_pf32_Frame_t;
 /* declare_local */
 /* internal_declare_param */
return_parameter__FLOAT32
__cdecl
return_parameter__ret_pf32(
   /* Param_Type1 */ return_parameter__FLOAT32 p_L_26);
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct return_parameter__ret_pi16_Frame_t;typedef struct return_parameter__ret_pi16_Frame_t return_parameter__ret_pi16_Frame_t;
 /* declare_local */
 /* internal_declare_param */
return_parameter__INT16
__cdecl
return_parameter__ret_pi16(
   /* Param_Type1 */ return_parameter__INT16 p_L_28);
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct return_parameter__ret_pC_Frame_t;typedef struct return_parameter__ret_pC_Frame_t return_parameter__ret_pC_Frame_t;
 /* declare_local */
 /* internal_declare_param */
CARDINAL
__cdecl
return_parameter__ret_pC(
   /* Param_Type1 */ CARDINAL p_L_30);
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct return_parameter__ret_pu32_Frame_t;typedef struct return_parameter__ret_pu32_Frame_t return_parameter__ret_pu32_Frame_t;
 /* declare_local */
 /* internal_declare_param */
return_parameter__UINT32
__cdecl
return_parameter__ret_pu32(
   /* Param_Type1 */ return_parameter__UINT32 p_L_32);
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct return_parameter__ret_pu8_Frame_t;typedef struct return_parameter__ret_pu8_Frame_t return_parameter__ret_pu8_Frame_t;
 /* declare_local */
 /* internal_declare_param */
return_parameter__UINT8
__cdecl
return_parameter__ret_pu8(
   /* Param_Type1 */ return_parameter__UINT8 p_L_34);
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct return_parameter__ret_pL_Frame_t;typedef struct return_parameter__ret_pL_Frame_t return_parameter__ret_pL_Frame_t;
 /* declare_local */
 /* internal_declare_param */
LONGINT
__cdecl
return_parameter__ret_pL(
   /* Param_Type1 */ LONGINT p_L_36);
 /* Locals_begin_procedure */
 /* begin_block */
 /* Locals_end_procedure */
 /* end_block */
 /* Locals_begin_procedure */
 /* begin_block */
 /* Locals_end_procedure */
 /* end_block */
 /* Locals_begin_procedure */
 /* begin_block */
 /* Locals_end_procedure */
 /* end_block */
 /* Locals_begin_procedure */
 /* begin_block */
 /* Locals_end_procedure */
 /* end_block */
 /* Locals_begin_procedure */
 /* begin_block */
 /* Locals_end_procedure */
 /* end_block */
 /* Locals_begin_procedure */
 /* begin_block */
 /* Locals_end_procedure */
 /* end_block */
 /* Locals_begin_procedure */
 /* begin_block */
 /* Locals_end_procedure */
 /* end_block */
 /* Locals_begin_procedure */
 /* begin_block */
 /* Locals_end_procedure */
 /* end_block */
 /* Locals_begin_procedure */
 /* begin_block */
 /* Locals_end_procedure */
 /* end_block */
 /* Locals_begin_procedure */
 /* begin_block */
 /* Locals_end_procedure */
 /* end_block */
 /* Locals_begin_procedure */
 /* begin_block */
 /* Locals_end_procedure */
 /* end_block */
 /* Locals_begin_procedure */
 /* begin_block */
 /* Locals_end_procedure */
 /* end_block */
 /* Locals_begin_procedure */
 /* begin_block */
 /* Locals_end_procedure */
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
 /* init_proc */
 /* init_var */
 /* init_proc */
 /* init_var */
 /* init_proc */
 /* init_var */
 /* init_proc */
 /* init_var */
 /* init_proc */
 /* init_var */
 /* init_proc */
 /* init_var */
 /* init_proc */
 /* init_var */
 /* init_proc */
 /* init_var */
 /* init_proc */
 /* init_var */
 /* init_proc */
 /* init_var */
 /* init_proc */
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
struct return_parameter_m_5_L_6_t{UINT8 L_37[19];
char L_38[1];
UINT8 L_39[6];
char L_40[1];
UINT8 L_41[7];
char L_42[1];
UINT8 L_43[8];
char L_44[1];
UINT8 L_45[6];
char L_46[1];
UINT8 L_47[8];
char L_48[1];
UINT8 L_49[8];
char L_50[1];
UINT8 L_51[8];
char L_52[1];
UINT8 L_53[6];
char L_54[1];
UINT8 L_55[8];
char L_56[1];
UINT8 L_57[7];
char L_58[1];
UINT8 L_59[8];
char L_60[1];
UINT8 L_61[8];
char L_62[1];
UINT8 L_63[8];
char L_64[1];
UINT8 L_65[7];
char L_66[8];
ADDRESS L_67[30];
char L_68[8];
UINT8 L_69[19];
char L_70[5];
};
static  const return_parameter_m_5_L_6_t return_parameter_m_5_L_6={{'r','e','t','u','r','n','_','p','a','r','a','m','e','t','e','r','_','M','3'},{0 /* 1 */ ,},{'r','e','t','_','p','L'},{0 /* 1 */ ,},{'r','e','t','_','p','u','8'},{0 /* 1 */ ,},{'r','e','t','_','p','u','3','2'},{0 /* 1 */ ,},{'r','e','t','_','p','C'},{0 /* 1 */ ,},{'r','e','t','_','p','i','1','6'},{0 /* 1 */ ,},{'r','e','t','_','p','f','3','2'},{0 /* 1 */ ,},{'r','e','t','_','p','i','6','4'},{0 /* 1 */ ,},{'r','e','t','_','p','I'},{0 /* 1 */ ,},{'r','e','t','_','p','u','1','6'},{0 /* 1 */ ,},{'r','e','t','_','p','L','C'},{0 /* 1 */ ,},{'r','e','t','_','p','i','3','2'},{0 /* 1 */ ,},{'r','e','t','_','p','f','6','4'},{0 /* 1 */ ,},{'r','e','t','_','p','u','6','4'},{0 /* 1 */ ,},{'r','e','t','_','p','i','8'},{0 /* 1 */ ,0 /* 2 */ ,0 /* 3 */ ,0 /* 4 */ ,0 /* 5 */ ,0 /* 6 */ ,0 /* 7 */ ,0 /* 8 */ ,},{(ADDRESS)&return_parameter_M3,(char*)&return_parameter_m_5_L_6,(ADDRESS)&return_parameter__ret_pL,20+(char*)&return_parameter_m_5_L_6
,(ADDRESS)&return_parameter__ret_pu8,27+(char*)&return_parameter_m_5_L_6,(ADDRESS)&return_parameter__ret_pu32,35+(char*)&return_parameter_m_5_L_6,(ADDRESS)&return_parameter__ret_pC,44+(char*)&return_parameter_m_5_L_6,(ADDRESS)&return_parameter__ret_pi16,51+(char*)&return_parameter_m_5_L_6,(ADDRESS)&return_parameter__ret_pf32,60+(char*)&return_parameter_m_5_L_6,(ADDRESS)&return_parameter__ret_pi64,69+(char*)&return_parameter_m_5_L_6,(ADDRESS)&return_parameter__ret_pI,78+(char*)&return_parameter_m_5_L_6,(ADDRESS)&return_parameter__ret_pu16,85+(char*)&return_parameter_m_5_L_6,(ADDRESS)&return_parameter__ret_pLC,94+(char*)&return_parameter_m_5_L_6,(ADDRESS)&return_parameter__ret_pi32,102+(char*)&return_parameter_m_5_L_6,(ADDRESS)&return_parameter__ret_pf64,111+(char*)&return_parameter_m_5_L_6,(ADDRESS)&return_parameter__ret_pu64,120+(char*)&return_parameter_m_5_L_6,(ADDRESS)&return_parameter__ret_pi8,129+(char*)&return_parameter_m_5_L_6},{0 /* 1 */ ,0 /* 2 */ ,0 /* 3 */ ,0 /* 4 */ ,0 /* 5 */ 
,0 /* 6 */ ,0 /* 7 */ ,0 /* 8 */ ,},{'r','e','t','u','r','n','_','p','a','r','a','m','e','t','e','r','.','m','3'},{0 /* 1 */ ,0 /* 2 */ ,0 /* 3 */ ,0 /* 4 */ ,0 /* 5 */ ,}};
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
struct return_parameter_m_M_return_parameter_L_7_t{ADDRESS L_71[1];
char L_72[32];
ADDRESS L_73[1];
char L_74[24];
ADDRESS L_75[1];
char L_76[8];
ADDRESS L_77[1];
INT64 L_78[1];
INT8 L_79[1];
char L_80[7];
INT64 L_81[1];
double L_82[1];
INT32 L_83[1];
char L_84[4];
INT64 L_85[1];
INT16 L_86[1];
char L_87[6];
INT64 L_88[2];
float L_89[1];
INT16 L_90[1];
char L_91[2];
INT64 L_92[1];
INT32 L_93[1];
INT8 L_94[1];
char L_95[3];
INT64 L_96[1];
char L_97[24];
ADDRESS L_98[2];
char L_99[8];
ADDRESS L_100[2];
char L_101[8];
ADDRESS L_102[2];
char L_103[8];
ADDRESS L_104[2];
char L_105[8];
ADDRESS L_106[1];
char L_107[8];
};
static return_parameter_m_M_return_parameter_L_7_t return_parameter_m_M_return_parameter_L_7={{392+(char*)&return_parameter_m_5_L_6},{0 /* 1 */ ,0 /* 2 */ ,0 /* 3 */ ,0 /* 4 */ ,0 /* 5 */ ,0 /* 6 */ ,0 /* 7 */ ,0 /* 8 */ ,0 /* 9 */ ,0 /* 10 */ ,0 /* 11 */ ,0 /* 12 */ ,0 /* 13 */ ,0 /* 14 */ ,0 /* 15 */ ,0 /* 16 */ ,0 /* 17 */ ,0 /* 18 */ ,0 /* 19 */ ,0 /* 20 */ ,0 /* 21 */ ,0 /* 22 */ ,0 /* 23 */ ,0 /* 24 */ ,0 /* 25 */ ,0 /* 26 */ ,0 /* 27 */ ,0 /* 28 */ ,0 /* 29 */ ,0 /* 30 */ ,0 /* 31 */ ,0 /* 32 */ ,},{144+(char*)&return_parameter_m_5_L_6},{0 /* 1 */ ,0 /* 2 */ ,0 /* 3 */ ,0 /* 4 */ ,0 /* 5 */ ,0 /* 6 */ ,0 /* 7 */ ,0 /* 8 */ ,0 /* 9 */ ,0 /* 10 */ ,0 /* 11 */ ,0 /* 12 */ ,0 /* 13 */ ,0 /* 14 */ ,0 /* 15 */ ,0 /* 16 */ ,0 /* 17 */ ,0 /* 18 */ ,0 /* 19 */ ,0 /* 20 */ ,0 /* 21 */ ,0 /* 22 */ ,0 /* 23 */ ,0 /* 24 */ ,},{216+(char*)&return_parameter_m_M_return_parameter_L_7},{0 /* 1 */ ,0 /* 2 */ ,0 /* 3 */ ,0 /* 4 */ ,0 /* 5 */ ,0 /* 6 */ ,0 /* 7 */ ,0 /* 8 */ ,},{(ADDRESS)&return_parameter_M3
},{INT64_(3)},{((INT8)81)},{0 /* 1 */ ,0 /* 2 */ ,0 /* 3 */ ,0 /* 4 */ ,0 /* 5 */ ,0 /* 6 */ ,0 /* 7 */ ,},{INT64_(82)},{8.38400000000000034e1},{85},{0 /* 1 */ ,0 /* 2 */ ,0 /* 3 */ ,0 /* 4 */ ,},{INT64_(86)},{((INT16)87)},{0 /* 1 */ ,0 /* 2 */ ,0 /* 3 */ ,0 /* 4 */ ,0 /* 5 */ ,0 /* 6 */ ,},{INT64_(88),INT64_(89)},{9.0910003662109E1F},{((INT16)92)},{0 /* 1 */ ,0 /* 2 */ ,},{INT64_(93)},{94},{((INT8)95)},{0 /* 1 */ ,0 /* 2 */ ,0 /* 3 */ ,},{INT64_(96)},{0 /* 1 */ ,0 /* 2 */ ,0 /* 3 */ ,0 /* 4 */ ,0 /* 5 */ ,0 /* 6 */ ,0 /* 7 */ ,0 /* 8 */ ,0 /* 9 */ ,0 /* 10 */ ,0 /* 11 */ ,0 /* 12 */ ,0 /* 13 */ ,0 /* 14 */ ,0 /* 15 */ ,0 /* 16 */ ,0 /* 17 */ ,0 /* 18 */ ,0 /* 19 */ ,0 /* 20 */ ,0 /* 21 */ ,0 /* 22 */ ,0 /* 23 */ ,0 /* 24 */ ,},{(ADDRESS)&return_parameter_I3,240+(char*)&return_parameter_m_M_return_parameter_L_7},{0 /* 1 */ ,0 /* 2 */ ,0 /* 3 */ ,0 /* 4 */ ,0 /* 5 */ ,0 /* 6 */ ,0 /* 7 */ ,0 /* 8 */ ,},{(ADDRESS)&Long_I3,264+(char*)&return_parameter_m_M_return_parameter_L_7},{0 /* 1 */ 
,0 /* 2 */ ,0 /* 3 */ ,0 /* 4 */ ,0 /* 5 */ ,0 /* 6 */ ,0 /* 7 */ ,0 /* 8 */ ,},{(ADDRESS)&Word_I3,288+(char*)&return_parameter_m_M_return_parameter_L_7},{0 /* 1 */ ,0 /* 2 */ ,0 /* 3 */ ,0 /* 4 */ ,0 /* 5 */ ,0 /* 6 */ ,0 /* 7 */ ,0 /* 8 */ ,},{(ADDRESS)&Cstdint_I3,312+(char*)&return_parameter_m_M_return_parameter_L_7},{0 /* 1 */ ,0 /* 2 */ ,0 /* 3 */ ,0 /* 4 */ ,0 /* 5 */ ,0 /* 6 */ ,0 /* 7 */ ,0 /* 8 */ ,},{(ADDRESS)&RTHooks_I3},{0 /* 1 */ ,0 /* 2 */ ,0 /* 3 */ ,0 /* 4 */ ,0 /* 5 */ ,0 /* 6 */ ,0 /* 7 */ ,0 /* 8 */ ,}};
 /* end: segments/globals */
 /* begin: mark used */
 /* end: mark used */
 /* set_source_file */
 /* set_source_line */
#line 1 "return_parameter.m3"
 /* module global constants */
#line 1 "return_parameter.m3"
 /* module global data */
#line 1 "return_parameter.m3"
 /* set_source_line */
#line 1 "return_parameter.m3"
#line 68 "return_parameter.m3"
 /* ret_pi8 */
#line 68 "return_parameter.m3"
 /* set_source_line */
#line 68 "return_parameter.m3"
#line 54 "return_parameter.m3"
 /* begin_procedure */
#line 54 "return_parameter.m3"
struct return_parameter__ret_pi8_Frame_t {
#line 54 "return_parameter.m3"
ADDRESS _unused;
#line 54 "return_parameter.m3"
};
#line 54 "return_parameter.m3"
return_parameter__INT8
__cdecl
return_parameter__ret_pi8(
   /* Param_Type1 */ return_parameter__INT8 p_L_10)
{
#line 54 "return_parameter.m3"
return_parameter__ret_pi8_Frame_t _frame;
#line 54 "return_parameter.m3"
_frame._unused=(ADDRESS)&_frame;
#line 54 "return_parameter.m3"
 /* load */
#line 54 "return_parameter.m3"
 /* exit_proc */
#line 54 "return_parameter.m3"
return ((INT64)(p_L_10));
#line 54 "return_parameter.m3"
 /* end_procedure */
#line 54 "return_parameter.m3"
} /* ret_pu64 */
#line 54 "return_parameter.m3"
 /* set_source_line */
#line 54 "return_parameter.m3"
#line 55 "return_parameter.m3"
 /* begin_procedure */
#line 55 "return_parameter.m3"
struct return_parameter__ret_pu64_Frame_t {
#line 55 "return_parameter.m3"
ADDRESS _unused;
#line 55 "return_parameter.m3"
};
#line 55 "return_parameter.m3"
return_parameter__UINT64
__cdecl
return_parameter__ret_pu64(
   /* Param_Type1 */ return_parameter__UINT64 p_L_12)
{
#line 55 "return_parameter.m3"
return_parameter__ret_pu64_Frame_t _frame;
#line 55 "return_parameter.m3"
_frame._unused=(ADDRESS)&_frame;
#line 55 "return_parameter.m3"
 /* load */
#line 55 "return_parameter.m3"
 /* exit_proc */
#line 55 "return_parameter.m3"
return p_L_12;
#line 55 "return_parameter.m3"
 /* end_procedure */
#line 55 "return_parameter.m3"
} /* ret_pf64 */
#line 55 "return_parameter.m3"
 /* set_source_line */
#line 55 "return_parameter.m3"
#line 56 "return_parameter.m3"
 /* begin_procedure */
#line 56 "return_parameter.m3"
struct return_parameter__ret_pf64_Frame_t {
#line 56 "return_parameter.m3"
ADDRESS _unused;
#line 56 "return_parameter.m3"
};
#line 56 "return_parameter.m3"
return_parameter__FLOAT64
__cdecl
return_parameter__ret_pf64(
   /* Param_Type1 */ return_parameter__FLOAT64 p_L_14)
{
#line 56 "return_parameter.m3"
return_parameter__ret_pf64_Frame_t _frame;
#line 56 "return_parameter.m3"
_frame._unused=(ADDRESS)&_frame;
#line 56 "return_parameter.m3"
 /* load */
#line 56 "return_parameter.m3"
 /* exit_proc */
#line 56 "return_parameter.m3"
return p_L_14;
#line 56 "return_parameter.m3"
 /* end_procedure */
#line 56 "return_parameter.m3"
} /* ret_pi32 */
#line 56 "return_parameter.m3"
 /* set_source_line */
#line 56 "return_parameter.m3"
#line 57 "return_parameter.m3"
 /* begin_procedure */
#line 57 "return_parameter.m3"
struct return_parameter__ret_pi32_Frame_t {
#line 57 "return_parameter.m3"
ADDRESS _unused;
#line 57 "return_parameter.m3"
};
#line 57 "return_parameter.m3"
return_parameter__INT32
__cdecl
return_parameter__ret_pi32(
   /* Param_Type1 */ return_parameter__INT32 p_L_16)
{
#line 57 "return_parameter.m3"
return_parameter__ret_pi32_Frame_t _frame;
#line 57 "return_parameter.m3"
_frame._unused=(ADDRESS)&_frame;
#line 57 "return_parameter.m3"
 /* load */
#line 57 "return_parameter.m3"
 /* exit_proc */
#line 57 "return_parameter.m3"
return ((INT64)(p_L_16));
#line 57 "return_parameter.m3"
 /* end_procedure */
#line 57 "return_parameter.m3"
} /* ret_pLC */
#line 57 "return_parameter.m3"
 /* set_source_line */
#line 57 "return_parameter.m3"
#line 58 "return_parameter.m3"
 /* begin_procedure */
#line 58 "return_parameter.m3"
struct return_parameter__ret_pLC_Frame_t {
#line 58 "return_parameter.m3"
ADDRESS _unused;
#line 58 "return_parameter.m3"
};
#line 58 "return_parameter.m3"
LONGCARD
__cdecl
return_parameter__ret_pLC(
   /* Param_Type1 */ LONGCARD p_L_18)
{
#line 58 "return_parameter.m3"
return_parameter__ret_pLC_Frame_t _frame;
#line 58 "return_parameter.m3"
_frame._unused=(ADDRESS)&_frame;
#line 58 "return_parameter.m3"
 /* load */
#line 58 "return_parameter.m3"
 /* exit_proc */
#line 58 "return_parameter.m3"
return ((INT64)(p_L_18));
#line 58 "return_parameter.m3"
 /* end_procedure */
#line 58 "return_parameter.m3"
} /* ret_pu16 */
#line 58 "return_parameter.m3"
 /* set_source_line */
#line 58 "return_parameter.m3"
#line 59 "return_parameter.m3"
 /* begin_procedure */
#line 59 "return_parameter.m3"
struct return_parameter__ret_pu16_Frame_t {
#line 59 "return_parameter.m3"
ADDRESS _unused;
#line 59 "return_parameter.m3"
};
#line 59 "return_parameter.m3"
return_parameter__UINT16
__cdecl
return_parameter__ret_pu16(
   /* Param_Type1 */ return_parameter__UINT16 p_L_20)
{
#line 59 "return_parameter.m3"
return_parameter__ret_pu16_Frame_t _frame;
#line 59 "return_parameter.m3"
_frame._unused=(ADDRESS)&_frame;
#line 59 "return_parameter.m3"
 /* load */
#line 59 "return_parameter.m3"
 /* exit_proc */
#line 59 "return_parameter.m3"
return ((INT64)(p_L_20));
#line 59 "return_parameter.m3"
 /* end_procedure */
#line 59 "return_parameter.m3"
} /* ret_pI */
#line 59 "return_parameter.m3"
 /* set_source_line */
#line 59 "return_parameter.m3"
#line 60 "return_parameter.m3"
 /* begin_procedure */
#line 60 "return_parameter.m3"
struct return_parameter__ret_pI_Frame_t {
#line 60 "return_parameter.m3"
ADDRESS _unused;
#line 60 "return_parameter.m3"
};
#line 60 "return_parameter.m3"
INTEGER
__cdecl
return_parameter__ret_pI(
   /* Param_Type1 */ INTEGER p_L_22)
{
#line 60 "return_parameter.m3"
return_parameter__ret_pI_Frame_t _frame;
#line 60 "return_parameter.m3"
_frame._unused=(ADDRESS)&_frame;
#line 60 "return_parameter.m3"
 /* load */
#line 60 "return_parameter.m3"
 /* exit_proc */
#line 60 "return_parameter.m3"
return p_L_22;
#line 60 "return_parameter.m3"
 /* end_procedure */
#line 60 "return_parameter.m3"
} /* ret_pi64 */
#line 60 "return_parameter.m3"
 /* set_source_line */
#line 60 "return_parameter.m3"
#line 61 "return_parameter.m3"
 /* begin_procedure */
#line 61 "return_parameter.m3"
struct return_parameter__ret_pi64_Frame_t {
#line 61 "return_parameter.m3"
ADDRESS _unused;
#line 61 "return_parameter.m3"
};
#line 61 "return_parameter.m3"
return_parameter__INT64
__cdecl
return_parameter__ret_pi64(
   /* Param_Type1 */ return_parameter__INT64 p_L_24)
{
#line 61 "return_parameter.m3"
return_parameter__ret_pi64_Frame_t _frame;
#line 61 "return_parameter.m3"
_frame._unused=(ADDRESS)&_frame;
#line 61 "return_parameter.m3"
 /* load */
#line 61 "return_parameter.m3"
 /* exit_proc */
#line 61 "return_parameter.m3"
return p_L_24;
#line 61 "return_parameter.m3"
 /* end_procedure */
#line 61 "return_parameter.m3"
} /* ret_pf32 */
#line 61 "return_parameter.m3"
 /* set_source_line */
#line 61 "return_parameter.m3"
#line 62 "return_parameter.m3"
 /* begin_procedure */
#line 62 "return_parameter.m3"
struct return_parameter__ret_pf32_Frame_t {
#line 62 "return_parameter.m3"
ADDRESS _unused;
#line 62 "return_parameter.m3"
};
#line 62 "return_parameter.m3"
return_parameter__FLOAT32
__cdecl
return_parameter__ret_pf32(
   /* Param_Type1 */ return_parameter__FLOAT32 p_L_26)
{
#line 62 "return_parameter.m3"
return_parameter__ret_pf32_Frame_t _frame;
#line 62 "return_parameter.m3"
_frame._unused=(ADDRESS)&_frame;
#line 62 "return_parameter.m3"
 /* load */
#line 62 "return_parameter.m3"
 /* exit_proc */
#line 62 "return_parameter.m3"
return p_L_26;
#line 62 "return_parameter.m3"
 /* end_procedure */
#line 62 "return_parameter.m3"
} /* ret_pi16 */
#line 62 "return_parameter.m3"
 /* set_source_line */
#line 62 "return_parameter.m3"
#line 63 "return_parameter.m3"
 /* begin_procedure */
#line 63 "return_parameter.m3"
struct return_parameter__ret_pi16_Frame_t {
#line 63 "return_parameter.m3"
ADDRESS _unused;
#line 63 "return_parameter.m3"
};
#line 63 "return_parameter.m3"
return_parameter__INT16
__cdecl
return_parameter__ret_pi16(
   /* Param_Type1 */ return_parameter__INT16 p_L_28)
{
#line 63 "return_parameter.m3"
return_parameter__ret_pi16_Frame_t _frame;
#line 63 "return_parameter.m3"
_frame._unused=(ADDRESS)&_frame;
#line 63 "return_parameter.m3"
 /* load */
#line 63 "return_parameter.m3"
 /* exit_proc */
#line 63 "return_parameter.m3"
return ((INT64)(p_L_28));
#line 63 "return_parameter.m3"
 /* end_procedure */
#line 63 "return_parameter.m3"
} /* ret_pC */
#line 63 "return_parameter.m3"
 /* set_source_line */
#line 63 "return_parameter.m3"
#line 64 "return_parameter.m3"
 /* begin_procedure */
#line 64 "return_parameter.m3"
struct return_parameter__ret_pC_Frame_t {
#line 64 "return_parameter.m3"
ADDRESS _unused;
#line 64 "return_parameter.m3"
};
#line 64 "return_parameter.m3"
CARDINAL
__cdecl
return_parameter__ret_pC(
   /* Param_Type1 */ CARDINAL p_L_30)
{
#line 64 "return_parameter.m3"
return_parameter__ret_pC_Frame_t _frame;
#line 64 "return_parameter.m3"
_frame._unused=(ADDRESS)&_frame;
#line 64 "return_parameter.m3"
 /* load */
#line 64 "return_parameter.m3"
 /* exit_proc */
#line 64 "return_parameter.m3"
return ((INT64)(p_L_30));
#line 64 "return_parameter.m3"
 /* end_procedure */
#line 64 "return_parameter.m3"
} /* ret_pu32 */
#line 64 "return_parameter.m3"
 /* set_source_line */
#line 64 "return_parameter.m3"
#line 65 "return_parameter.m3"
 /* begin_procedure */
#line 65 "return_parameter.m3"
struct return_parameter__ret_pu32_Frame_t {
#line 65 "return_parameter.m3"
ADDRESS _unused;
#line 65 "return_parameter.m3"
};
#line 65 "return_parameter.m3"
return_parameter__UINT32
__cdecl
return_parameter__ret_pu32(
   /* Param_Type1 */ return_parameter__UINT32 p_L_32)
{
#line 65 "return_parameter.m3"
return_parameter__ret_pu32_Frame_t _frame;
#line 65 "return_parameter.m3"
_frame._unused=(ADDRESS)&_frame;
#line 65 "return_parameter.m3"
 /* load */
#line 65 "return_parameter.m3"
 /* exit_proc */
#line 65 "return_parameter.m3"
return ((INT64)(p_L_32));
#line 65 "return_parameter.m3"
 /* end_procedure */
#line 65 "return_parameter.m3"
} /* ret_pu8 */
#line 65 "return_parameter.m3"
 /* set_source_line */
#line 65 "return_parameter.m3"
#line 66 "return_parameter.m3"
 /* begin_procedure */
#line 66 "return_parameter.m3"
struct return_parameter__ret_pu8_Frame_t {
#line 66 "return_parameter.m3"
ADDRESS _unused;
#line 66 "return_parameter.m3"
};
#line 66 "return_parameter.m3"
return_parameter__UINT8
__cdecl
return_parameter__ret_pu8(
   /* Param_Type1 */ return_parameter__UINT8 p_L_34)
{
#line 66 "return_parameter.m3"
return_parameter__ret_pu8_Frame_t _frame;
#line 66 "return_parameter.m3"
_frame._unused=(ADDRESS)&_frame;
#line 66 "return_parameter.m3"
 /* load */
#line 66 "return_parameter.m3"
 /* exit_proc */
#line 66 "return_parameter.m3"
return ((INT64)(p_L_34));
#line 66 "return_parameter.m3"
 /* end_procedure */
#line 66 "return_parameter.m3"
} /* ret_pL */
#line 66 "return_parameter.m3"
 /* set_source_line */
#line 66 "return_parameter.m3"
#line 67 "return_parameter.m3"
 /* begin_procedure */
#line 67 "return_parameter.m3"
struct return_parameter__ret_pL_Frame_t {
#line 67 "return_parameter.m3"
ADDRESS _unused;
#line 67 "return_parameter.m3"
};
#line 67 "return_parameter.m3"
LONGINT
__cdecl
return_parameter__ret_pL(
   /* Param_Type1 */ LONGINT p_L_36)
{
#line 67 "return_parameter.m3"
return_parameter__ret_pL_Frame_t _frame;
#line 67 "return_parameter.m3"
_frame._unused=(ADDRESS)&_frame;
#line 67 "return_parameter.m3"
 /* load */
#line 67 "return_parameter.m3"
 /* exit_proc */
#line 67 "return_parameter.m3"
return p_L_36;
#line 67 "return_parameter.m3"
 /* end_procedure */
#line 67 "return_parameter.m3"
} /* return_parameter_M3 */
#line 67 "return_parameter.m3"
 /* module main body return_parameter_M3 */
#line 67 "return_parameter.m3"
 /* set_source_line */
#line 67 "return_parameter.m3"
#line 68 "return_parameter.m3"
 /* begin_procedure */
#line 68 "return_parameter.m3"
struct return_parameter_M3_Frame_t {
#line 68 "return_parameter.m3"
ADDRESS _unused;
#line 68 "return_parameter.m3"
};
#line 68 "return_parameter.m3"
RT0__ModulePtr
__cdecl
return_parameter_M3(
   /* Param_Type1 */ INTEGER /*TypeText1*/  mode_L_8)
{
#line 68 "return_parameter.m3"
return_parameter_M3_Frame_t _frame;
#line 68 "return_parameter.m3"
_frame._unused=(ADDRESS)&_frame;
#line 68 "return_parameter.m3"
 /* load */
#line 68 "return_parameter.m3"
 /* if_true_or_false */
#line 68 "return_parameter.m3"
 /* load_host_integer */
#line 68 "return_parameter.m3"
 /* load_integer */
#line 68 "return_parameter.m3"
 /* if_compare */
#line 68 "return_parameter.m3"
if(m3_eq(INT64,
  mode_L_8,
   INT64_(0)))goto L1;
#line 68 "return_parameter.m3"
 /* set_label */
#line 68 "return_parameter.m3"
L1:;
#line 68 "return_parameter.m3"
 /* load_address */
#line 68 "return_parameter.m3"
 /* exit_proc */
#line 68 "return_parameter.m3"
return (RT0__ModulePtr)(&return_parameter_m_M_return_parameter_L_7);
#line 68 "return_parameter.m3"
 /* end_procedure */
#line 68 "return_parameter.m3"
} /* global constant type descriptor */
#line 68 "return_parameter.m3"
 /* global data type descriptor */
#line 68 "return_parameter.m3"
 /* module global constants */
#line 68 "return_parameter.m3"
 /* procedure names */
#line 68 "return_parameter.m3"
 /* procedure table */
#line 68 "return_parameter.m3"
 /* file name */
#line 68 "return_parameter.m3"
 /* module global data */
#line 68 "return_parameter.m3"
 /* load map


 global data allocation for M_return_parameter
     0   104  8  *module info*
   104     1  1  return_parameter.vi8
   112     8  8  return_parameter.vu64
   120     8  8  return_parameter.vf64
   128     4  4  return_parameter.vi32
   136     8  8  return_parameter.vLC
   144     2  2  return_parameter.vu16
   152     8  8  return_parameter.vI
   160     8  8  return_parameter.vi64
   168     4  4  return_parameter.vf32
   172     2  2  return_parameter.vi16
   176     8  8  return_parameter.vC
   184     4  4  return_parameter.vu32
   188     1  1  return_parameter.vu8
   192     8  8  return_parameter.vL
   200     8  8  return_parameter.offset
   208     8  8  return_parameter.count
   216    24  8  import return_parameter
   240    24  8  import Long
   264    24  8  import Word
   288    24  8  import Cstdint
   312    24  8  import RTHooks
   336     0  8  *TOTAL*


 global constants for M_return_parameter
     0   137  8  *proc names*
   144   248  8  *proc info*
   392    20  1  *string*
   416     0  8  *TOTAL*
 */
#line 68 "return_parameter.m3"
 /* end unit */
#line 68 "return_parameter.m3"

#ifdef __cplusplus

} /* extern "C" */
#endif
 /* set_runtime_proc */
 /* set_runtime_proc */
 /* set_runtime_proc */
