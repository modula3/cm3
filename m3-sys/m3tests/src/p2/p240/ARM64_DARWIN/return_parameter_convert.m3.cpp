// library:pgm
// source_base_name:return_parameter_convert
// target_name:return_parameter_convert.m3.cpp
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

#ifndef return_parameter_convert__UINT64
#define return_parameter_convert__UINT64 return_parameter_convert__UINT64
typedef INT64 return_parameter_convert__UINT64;
#endif
 /* declare_proctype */
 /* declare_formal */

#ifndef return_parameter_convert__INT8
#define return_parameter_convert__INT8 return_parameter_convert__INT8
typedef T66A2A904_8 return_parameter_convert__INT8;
#endif
 /* declare_proctype */
 /* declare_formal */
 /* declare_proctype */
 /* declare_formal */

#ifndef return_parameter_convert__INT32
#define return_parameter_convert__INT32 return_parameter_convert__INT32
typedef TADC6066D_32 return_parameter_convert__INT32;
#endif
 /* declare_proctype */
 /* declare_formal */
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

#ifndef return_parameter_convert__UINT16
#define return_parameter_convert__UINT16 return_parameter_convert__UINT16
typedef TA4B285DE_16 return_parameter_convert__UINT16;
#endif
 /* declare_proctype */
 /* declare_formal */
 /* declare_proctype */
 /* declare_formal */
 /* declare_proctype */
 /* declare_formal */
 /* declare_proctype */
 /* declare_formal */

#ifndef return_parameter_convert__INT64
#define return_parameter_convert__INT64 return_parameter_convert__INT64
typedef T839F750E_64 return_parameter_convert__INT64;
#endif
 /* declare_proctype */
 /* declare_formal */
 /* declare_proctype */
 /* declare_formal */
 /* declare_proctype */
 /* declare_formal */

#ifndef return_parameter_convert__INT16
#define return_parameter_convert__INT16 return_parameter_convert__INT16
typedef T7300E1E8_16 return_parameter_convert__INT16;
#endif
 /* declare_proctype */
 /* declare_formal */
 /* declare_proctype */
 /* declare_formal */

#ifndef return_parameter_convert__UINT32
#define return_parameter_convert__UINT32 return_parameter_convert__UINT32
typedef T6FA2E87D_32 return_parameter_convert__UINT32;
#endif
 /* declare_proctype */
 /* declare_formal */
 /* declare_proctype */
 /* declare_formal */

#ifndef return_parameter_convert__UINT8
#define return_parameter_convert__UINT8 return_parameter_convert__UINT8
typedef TB5B30AA_8 return_parameter_convert__UINT8;
#endif
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
 /* DeclareTypes_FlushOnce size:19 */

#if 0 /* avoid type hash collions */
typedef 
INT64(__cdecl*TAD19F0E0)(return_parameter_convert__UINT64);
#else
typedef void (__cdecl*TAD19F0E0)(void);
#endif

#if 0 /* avoid type hash collions */
typedef 
INT64(__cdecl*T37B8E9BF)(return_parameter_convert__INT8);
#else
typedef void (__cdecl*T37B8E9BF)(void);
#endif

#if 0 /* avoid type hash collions */
typedef 
INTEGER(__cdecl*T66092971)(return_parameter_convert__INT8);
#else
typedef void (__cdecl*T66092971)(void);
#endif

#if 0 /* avoid type hash collions */
typedef 
INT64(__cdecl*T644B7EEE)(return_parameter_convert__INT32);
#else
typedef void (__cdecl*T644B7EEE)(void);
#endif

#if 0 /* avoid type hash collions */
typedef 
INTEGER(__cdecl*T35FABE20)(return_parameter_convert__INT32);
#else
typedef void (__cdecl*T35FABE20)(void);
#endif

#if 0 /* avoid type hash collions */
typedef 
INT64(__cdecl*T704D59BA)(LONGCARD);
#else
typedef void (__cdecl*T704D59BA)(void);
#endif

#if 0 /* avoid type hash collions */
typedef 
INT64(__cdecl*TC96210B2)(return_parameter_convert__UINT16);
#else
typedef void (__cdecl*TC96210B2)(void);
#endif

#if 0 /* avoid type hash collions */
typedef 
INTEGER(__cdecl*T98D3D07C)(return_parameter_convert__UINT16);
#else
typedef void (__cdecl*T98D3D07C)(void);
#endif

#if 0 /* avoid type hash collions */
typedef 
INT64(__cdecl*T8ACFE18A)(INTEGER);
#else
typedef void (__cdecl*T8ACFE18A)(void);
#endif

#if 0 /* avoid type hash collions */
typedef 
INTEGER(__cdecl*TDB7E2144)(INTEGER);
#else
typedef void (__cdecl*TDB7E2144)(void);
#endif

#if 0 /* avoid type hash collions */
typedef 
INT64(__cdecl*T6228FA7B)(return_parameter_convert__INT64);
#else
typedef void (__cdecl*T6228FA7B)(void);
#endif

#if 0 /* avoid type hash collions */
typedef 
INT64(__cdecl*TF6B9B152)(CARDINAL);
#else
typedef void (__cdecl*TF6B9B152)(void);
#endif

#if 0 /* avoid type hash collions */
typedef 
INTEGER(__cdecl*TA708719C)(CARDINAL);
#else
typedef void (__cdecl*TA708719C)(void);
#endif

#if 0 /* avoid type hash collions */
typedef 
INT64(__cdecl*T5B8A675C)(return_parameter_convert__INT16);
#else
typedef void (__cdecl*T5B8A675C)(void);
#endif

#if 0 /* avoid type hash collions */
typedef 
INTEGER(__cdecl*TA3BA792)(return_parameter_convert__INT16);
#else
typedef void (__cdecl*TA3BA792)(void);
#endif

#if 0 /* avoid type hash collions */
typedef 
INT64(__cdecl*T23EFD6F9)(return_parameter_convert__UINT32);
#else
typedef void (__cdecl*T23EFD6F9)(void);
#endif

#if 0 /* avoid type hash collions */
typedef 
INTEGER(__cdecl*T725E1637)(return_parameter_convert__UINT32);
#else
typedef void (__cdecl*T725E1637)(void);
#endif

#if 0 /* avoid type hash collions */
typedef 
INT64(__cdecl*T216C5826)(return_parameter_convert__UINT8);
#else
typedef void (__cdecl*T216C5826)(void);
#endif

#if 0 /* avoid type hash collions */
typedef 
INTEGER(__cdecl*T70DD98E8)(return_parameter_convert__UINT8);
#else
typedef void (__cdecl*T70DD98E8)(void);
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
/*Proc_ForwardDeclareFrameType*/struct return_parameter_convert_I3_Frame_t;typedef struct return_parameter_convert_I3_Frame_t return_parameter_convert_I3_Frame_t;
 /* internal_declare_param */
RT0__ModulePtr
__cdecl
return_parameter_convert_I3(
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
/*declare_segment*/struct return_parameter_convert_m_5_L_6_t;
/*declare_segment*/typedef struct return_parameter_convert_m_5_L_6_t return_parameter_convert_m_5_L_6_t;
 /* declare_segment name:M_return_parameter_convert typeid:TFFFFFFFF const:FALSE */
 /* handler_name_prefixes:return_parameter_convert_M3_LINE_ */
 /* handler_name_prefixes:return_parameter_convert_I3_LINE_ */
/*declare_segment*/struct return_parameter_convert_m_M_return_parameter_convert_L_7_t;
/*declare_segment*/typedef struct return_parameter_convert_m_M_return_parameter_convert_L_7_t return_parameter_convert_m_M_return_parameter_convert_L_7_t;
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct return_parameter_convert_M3_Frame_t;typedef struct return_parameter_convert_M3_Frame_t return_parameter_convert_M3_Frame_t;
 /* internal_declare_param */
RT0__ModulePtr
__cdecl
return_parameter_convert_M3(
   /* Param_Type1 */ INTEGER /*TypeText1*/  mode_L_8);
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct return_parameter_convert__ret_u64_u64_Frame_t;typedef struct return_parameter_convert__ret_u64_u64_Frame_t return_parameter_convert__ret_u64_u64_Frame_t;
 /* declare_local */
 /* internal_declare_param */
LONGINT
__cdecl
return_parameter_convert__ret_u64_u64(
   /* Param_Type1 */ return_parameter_convert__UINT64 a_L_10);
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct return_parameter_convert__ret_u64_i8_Frame_t;typedef struct return_parameter_convert__ret_u64_i8_Frame_t return_parameter_convert__ret_u64_i8_Frame_t;
 /* declare_local */
 /* internal_declare_param */
LONGINT
__cdecl
return_parameter_convert__ret_u64_i8(
   /* Param_Type1 */ return_parameter_convert__UINT64 a_L_12);
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct return_parameter_convert__ret_u64_i32_Frame_t;typedef struct return_parameter_convert__ret_u64_i32_Frame_t return_parameter_convert__ret_u64_i32_Frame_t;
 /* declare_local */
 /* internal_declare_param */
LONGINT
__cdecl
return_parameter_convert__ret_u64_i32(
   /* Param_Type1 */ return_parameter_convert__UINT64 a_L_14);
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct return_parameter_convert__ret_u64_LC_Frame_t;typedef struct return_parameter_convert__ret_u64_LC_Frame_t return_parameter_convert__ret_u64_LC_Frame_t;
 /* declare_local */
 /* internal_declare_param */
LONGINT
__cdecl
return_parameter_convert__ret_u64_LC(
   /* Param_Type1 */ return_parameter_convert__UINT64 a_L_16);
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct return_parameter_convert__ret_u64_u16_Frame_t;typedef struct return_parameter_convert__ret_u64_u16_Frame_t return_parameter_convert__ret_u64_u16_Frame_t;
 /* declare_local */
 /* internal_declare_param */
LONGINT
__cdecl
return_parameter_convert__ret_u64_u16(
   /* Param_Type1 */ return_parameter_convert__UINT64 a_L_18);
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct return_parameter_convert__ret_u64_I_Frame_t;typedef struct return_parameter_convert__ret_u64_I_Frame_t return_parameter_convert__ret_u64_I_Frame_t;
 /* declare_local */
 /* internal_declare_param */
LONGINT
__cdecl
return_parameter_convert__ret_u64_I(
   /* Param_Type1 */ return_parameter_convert__UINT64 a_L_20);
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct return_parameter_convert__ret_u64_i64_Frame_t;typedef struct return_parameter_convert__ret_u64_i64_Frame_t return_parameter_convert__ret_u64_i64_Frame_t;
 /* declare_local */
 /* internal_declare_param */
LONGINT
__cdecl
return_parameter_convert__ret_u64_i64(
   /* Param_Type1 */ return_parameter_convert__UINT64 a_L_22);
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct return_parameter_convert__ret_u64_C_Frame_t;typedef struct return_parameter_convert__ret_u64_C_Frame_t return_parameter_convert__ret_u64_C_Frame_t;
 /* declare_local */
 /* internal_declare_param */
LONGINT
__cdecl
return_parameter_convert__ret_u64_C(
   /* Param_Type1 */ return_parameter_convert__UINT64 a_L_24);
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct return_parameter_convert__ret_u64_i16_Frame_t;typedef struct return_parameter_convert__ret_u64_i16_Frame_t return_parameter_convert__ret_u64_i16_Frame_t;
 /* declare_local */
 /* internal_declare_param */
LONGINT
__cdecl
return_parameter_convert__ret_u64_i16(
   /* Param_Type1 */ return_parameter_convert__UINT64 a_L_26);
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct return_parameter_convert__ret_u64_u32_Frame_t;typedef struct return_parameter_convert__ret_u64_u32_Frame_t return_parameter_convert__ret_u64_u32_Frame_t;
 /* declare_local */
 /* internal_declare_param */
LONGINT
__cdecl
return_parameter_convert__ret_u64_u32(
   /* Param_Type1 */ return_parameter_convert__UINT64 a_L_28);
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct return_parameter_convert__ret_u64_u8_Frame_t;typedef struct return_parameter_convert__ret_u64_u8_Frame_t return_parameter_convert__ret_u64_u8_Frame_t;
 /* declare_local */
 /* internal_declare_param */
LONGINT
__cdecl
return_parameter_convert__ret_u64_u8(
   /* Param_Type1 */ return_parameter_convert__UINT64 a_L_30);
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct return_parameter_convert__ret_u64_L_Frame_t;typedef struct return_parameter_convert__ret_u64_L_Frame_t return_parameter_convert__ret_u64_L_Frame_t;
 /* declare_local */
 /* internal_declare_param */
LONGINT
__cdecl
return_parameter_convert__ret_u64_L(
   /* Param_Type1 */ return_parameter_convert__UINT64 a_L_32);
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct return_parameter_convert__ret_i8_u64_Frame_t;typedef struct return_parameter_convert__ret_i8_u64_Frame_t return_parameter_convert__ret_i8_u64_Frame_t;
 /* declare_local */
 /* internal_declare_param */
LONGINT
__cdecl
return_parameter_convert__ret_i8_u64(
   /* Param_Type1 */ return_parameter_convert__INT8 a_L_34);
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct return_parameter_convert__ret_i8_i8_Frame_t;typedef struct return_parameter_convert__ret_i8_i8_Frame_t return_parameter_convert__ret_i8_i8_Frame_t;
 /* declare_local */
 /* internal_declare_param */
INTEGER
__cdecl
return_parameter_convert__ret_i8_i8(
   /* Param_Type1 */ return_parameter_convert__INT8 a_L_36);
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct return_parameter_convert__ret_i8_i32_Frame_t;typedef struct return_parameter_convert__ret_i8_i32_Frame_t return_parameter_convert__ret_i8_i32_Frame_t;
 /* declare_local */
 /* internal_declare_param */
INTEGER
__cdecl
return_parameter_convert__ret_i8_i32(
   /* Param_Type1 */ return_parameter_convert__INT8 a_L_38);
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct return_parameter_convert__ret_i8_LC_Frame_t;typedef struct return_parameter_convert__ret_i8_LC_Frame_t return_parameter_convert__ret_i8_LC_Frame_t;
 /* declare_local */
 /* internal_declare_param */
LONGINT
__cdecl
return_parameter_convert__ret_i8_LC(
   /* Param_Type1 */ return_parameter_convert__INT8 a_L_40);
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct return_parameter_convert__ret_i8_u16_Frame_t;typedef struct return_parameter_convert__ret_i8_u16_Frame_t return_parameter_convert__ret_i8_u16_Frame_t;
 /* declare_local */
 /* internal_declare_param */
INTEGER
__cdecl
return_parameter_convert__ret_i8_u16(
   /* Param_Type1 */ return_parameter_convert__INT8 a_L_42);
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct return_parameter_convert__ret_i8_I_Frame_t;typedef struct return_parameter_convert__ret_i8_I_Frame_t return_parameter_convert__ret_i8_I_Frame_t;
 /* declare_local */
 /* internal_declare_param */
INTEGER
__cdecl
return_parameter_convert__ret_i8_I(
   /* Param_Type1 */ return_parameter_convert__INT8 a_L_44);
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct return_parameter_convert__ret_i8_i64_Frame_t;typedef struct return_parameter_convert__ret_i8_i64_Frame_t return_parameter_convert__ret_i8_i64_Frame_t;
 /* declare_local */
 /* internal_declare_param */
LONGINT
__cdecl
return_parameter_convert__ret_i8_i64(
   /* Param_Type1 */ return_parameter_convert__INT8 a_L_46);
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct return_parameter_convert__ret_i8_C_Frame_t;typedef struct return_parameter_convert__ret_i8_C_Frame_t return_parameter_convert__ret_i8_C_Frame_t;
 /* declare_local */
 /* internal_declare_param */
INTEGER
__cdecl
return_parameter_convert__ret_i8_C(
   /* Param_Type1 */ return_parameter_convert__INT8 a_L_48);
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct return_parameter_convert__ret_i8_i16_Frame_t;typedef struct return_parameter_convert__ret_i8_i16_Frame_t return_parameter_convert__ret_i8_i16_Frame_t;
 /* declare_local */
 /* internal_declare_param */
INTEGER
__cdecl
return_parameter_convert__ret_i8_i16(
   /* Param_Type1 */ return_parameter_convert__INT8 a_L_50);
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct return_parameter_convert__ret_i8_u32_Frame_t;typedef struct return_parameter_convert__ret_i8_u32_Frame_t return_parameter_convert__ret_i8_u32_Frame_t;
 /* declare_local */
 /* internal_declare_param */
INTEGER
__cdecl
return_parameter_convert__ret_i8_u32(
   /* Param_Type1 */ return_parameter_convert__INT8 a_L_52);
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct return_parameter_convert__ret_i8_u8_Frame_t;typedef struct return_parameter_convert__ret_i8_u8_Frame_t return_parameter_convert__ret_i8_u8_Frame_t;
 /* declare_local */
 /* internal_declare_param */
INTEGER
__cdecl
return_parameter_convert__ret_i8_u8(
   /* Param_Type1 */ return_parameter_convert__INT8 a_L_54);
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct return_parameter_convert__ret_i8_L_Frame_t;typedef struct return_parameter_convert__ret_i8_L_Frame_t return_parameter_convert__ret_i8_L_Frame_t;
 /* declare_local */
 /* internal_declare_param */
LONGINT
__cdecl
return_parameter_convert__ret_i8_L(
   /* Param_Type1 */ return_parameter_convert__INT8 a_L_56);
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct return_parameter_convert__ret_i32_u64_Frame_t;typedef struct return_parameter_convert__ret_i32_u64_Frame_t return_parameter_convert__ret_i32_u64_Frame_t;
 /* declare_local */
 /* internal_declare_param */
LONGINT
__cdecl
return_parameter_convert__ret_i32_u64(
   /* Param_Type1 */ return_parameter_convert__INT32 a_L_58);
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct return_parameter_convert__ret_i32_i8_Frame_t;typedef struct return_parameter_convert__ret_i32_i8_Frame_t return_parameter_convert__ret_i32_i8_Frame_t;
 /* declare_local */
 /* internal_declare_param */
INTEGER
__cdecl
return_parameter_convert__ret_i32_i8(
   /* Param_Type1 */ return_parameter_convert__INT32 a_L_60);
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct return_parameter_convert__ret_i32_i32_Frame_t;typedef struct return_parameter_convert__ret_i32_i32_Frame_t return_parameter_convert__ret_i32_i32_Frame_t;
 /* declare_local */
 /* internal_declare_param */
INTEGER
__cdecl
return_parameter_convert__ret_i32_i32(
   /* Param_Type1 */ return_parameter_convert__INT32 a_L_62);
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct return_parameter_convert__ret_i32_LC_Frame_t;typedef struct return_parameter_convert__ret_i32_LC_Frame_t return_parameter_convert__ret_i32_LC_Frame_t;
 /* declare_local */
 /* internal_declare_param */
LONGINT
__cdecl
return_parameter_convert__ret_i32_LC(
   /* Param_Type1 */ return_parameter_convert__INT32 a_L_64);
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct return_parameter_convert__ret_i32_u16_Frame_t;typedef struct return_parameter_convert__ret_i32_u16_Frame_t return_parameter_convert__ret_i32_u16_Frame_t;
 /* declare_local */
 /* internal_declare_param */
INTEGER
__cdecl
return_parameter_convert__ret_i32_u16(
   /* Param_Type1 */ return_parameter_convert__INT32 a_L_66);
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct return_parameter_convert__ret_i32_I_Frame_t;typedef struct return_parameter_convert__ret_i32_I_Frame_t return_parameter_convert__ret_i32_I_Frame_t;
 /* declare_local */
 /* internal_declare_param */
INTEGER
__cdecl
return_parameter_convert__ret_i32_I(
   /* Param_Type1 */ return_parameter_convert__INT32 a_L_68);
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct return_parameter_convert__ret_i32_i64_Frame_t;typedef struct return_parameter_convert__ret_i32_i64_Frame_t return_parameter_convert__ret_i32_i64_Frame_t;
 /* declare_local */
 /* internal_declare_param */
LONGINT
__cdecl
return_parameter_convert__ret_i32_i64(
   /* Param_Type1 */ return_parameter_convert__INT32 a_L_70);
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct return_parameter_convert__ret_i32_C_Frame_t;typedef struct return_parameter_convert__ret_i32_C_Frame_t return_parameter_convert__ret_i32_C_Frame_t;
 /* declare_local */
 /* internal_declare_param */
INTEGER
__cdecl
return_parameter_convert__ret_i32_C(
   /* Param_Type1 */ return_parameter_convert__INT32 a_L_72);
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct return_parameter_convert__ret_i32_i16_Frame_t;typedef struct return_parameter_convert__ret_i32_i16_Frame_t return_parameter_convert__ret_i32_i16_Frame_t;
 /* declare_local */
 /* internal_declare_param */
INTEGER
__cdecl
return_parameter_convert__ret_i32_i16(
   /* Param_Type1 */ return_parameter_convert__INT32 a_L_74);
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct return_parameter_convert__ret_i32_u32_Frame_t;typedef struct return_parameter_convert__ret_i32_u32_Frame_t return_parameter_convert__ret_i32_u32_Frame_t;
 /* declare_local */
 /* internal_declare_param */
INTEGER
__cdecl
return_parameter_convert__ret_i32_u32(
   /* Param_Type1 */ return_parameter_convert__INT32 a_L_76);
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct return_parameter_convert__ret_i32_u8_Frame_t;typedef struct return_parameter_convert__ret_i32_u8_Frame_t return_parameter_convert__ret_i32_u8_Frame_t;
 /* declare_local */
 /* internal_declare_param */
INTEGER
__cdecl
return_parameter_convert__ret_i32_u8(
   /* Param_Type1 */ return_parameter_convert__INT32 a_L_78);
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct return_parameter_convert__ret_i32_L_Frame_t;typedef struct return_parameter_convert__ret_i32_L_Frame_t return_parameter_convert__ret_i32_L_Frame_t;
 /* declare_local */
 /* internal_declare_param */
LONGINT
__cdecl
return_parameter_convert__ret_i32_L(
   /* Param_Type1 */ return_parameter_convert__INT32 a_L_80);
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct return_parameter_convert__ret_LC_u64_Frame_t;typedef struct return_parameter_convert__ret_LC_u64_Frame_t return_parameter_convert__ret_LC_u64_Frame_t;
 /* declare_local */
 /* internal_declare_param */
LONGINT
__cdecl
return_parameter_convert__ret_LC_u64(
   /* Param_Type1 */ LONGCARD a_L_82);
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct return_parameter_convert__ret_LC_i8_Frame_t;typedef struct return_parameter_convert__ret_LC_i8_Frame_t return_parameter_convert__ret_LC_i8_Frame_t;
 /* declare_local */
 /* internal_declare_param */
LONGINT
__cdecl
return_parameter_convert__ret_LC_i8(
   /* Param_Type1 */ LONGCARD a_L_84);
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct return_parameter_convert__ret_LC_i32_Frame_t;typedef struct return_parameter_convert__ret_LC_i32_Frame_t return_parameter_convert__ret_LC_i32_Frame_t;
 /* declare_local */
 /* internal_declare_param */
LONGINT
__cdecl
return_parameter_convert__ret_LC_i32(
   /* Param_Type1 */ LONGCARD a_L_86);
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct return_parameter_convert__ret_LC_LC_Frame_t;typedef struct return_parameter_convert__ret_LC_LC_Frame_t return_parameter_convert__ret_LC_LC_Frame_t;
 /* declare_local */
 /* internal_declare_param */
LONGINT
__cdecl
return_parameter_convert__ret_LC_LC(
   /* Param_Type1 */ LONGCARD a_L_88);
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct return_parameter_convert__ret_LC_u16_Frame_t;typedef struct return_parameter_convert__ret_LC_u16_Frame_t return_parameter_convert__ret_LC_u16_Frame_t;
 /* declare_local */
 /* internal_declare_param */
LONGINT
__cdecl
return_parameter_convert__ret_LC_u16(
   /* Param_Type1 */ LONGCARD a_L_90);
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct return_parameter_convert__ret_LC_I_Frame_t;typedef struct return_parameter_convert__ret_LC_I_Frame_t return_parameter_convert__ret_LC_I_Frame_t;
 /* declare_local */
 /* internal_declare_param */
LONGINT
__cdecl
return_parameter_convert__ret_LC_I(
   /* Param_Type1 */ LONGCARD a_L_92);
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct return_parameter_convert__ret_LC_i64_Frame_t;typedef struct return_parameter_convert__ret_LC_i64_Frame_t return_parameter_convert__ret_LC_i64_Frame_t;
 /* declare_local */
 /* internal_declare_param */
LONGINT
__cdecl
return_parameter_convert__ret_LC_i64(
   /* Param_Type1 */ LONGCARD a_L_94);
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct return_parameter_convert__ret_LC_C_Frame_t;typedef struct return_parameter_convert__ret_LC_C_Frame_t return_parameter_convert__ret_LC_C_Frame_t;
 /* declare_local */
 /* internal_declare_param */
LONGINT
__cdecl
return_parameter_convert__ret_LC_C(
   /* Param_Type1 */ LONGCARD a_L_96);
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct return_parameter_convert__ret_LC_i16_Frame_t;typedef struct return_parameter_convert__ret_LC_i16_Frame_t return_parameter_convert__ret_LC_i16_Frame_t;
 /* declare_local */
 /* internal_declare_param */
LONGINT
__cdecl
return_parameter_convert__ret_LC_i16(
   /* Param_Type1 */ LONGCARD a_L_98);
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct return_parameter_convert__ret_LC_u32_Frame_t;typedef struct return_parameter_convert__ret_LC_u32_Frame_t return_parameter_convert__ret_LC_u32_Frame_t;
 /* declare_local */
 /* internal_declare_param */
LONGINT
__cdecl
return_parameter_convert__ret_LC_u32(
   /* Param_Type1 */ LONGCARD a_L_100);
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct return_parameter_convert__ret_LC_u8_Frame_t;typedef struct return_parameter_convert__ret_LC_u8_Frame_t return_parameter_convert__ret_LC_u8_Frame_t;
 /* declare_local */
 /* internal_declare_param */
LONGINT
__cdecl
return_parameter_convert__ret_LC_u8(
   /* Param_Type1 */ LONGCARD a_L_102);
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct return_parameter_convert__ret_LC_L_Frame_t;typedef struct return_parameter_convert__ret_LC_L_Frame_t return_parameter_convert__ret_LC_L_Frame_t;
 /* declare_local */
 /* internal_declare_param */
LONGINT
__cdecl
return_parameter_convert__ret_LC_L(
   /* Param_Type1 */ LONGCARD a_L_104);
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct return_parameter_convert__ret_u16_u64_Frame_t;typedef struct return_parameter_convert__ret_u16_u64_Frame_t return_parameter_convert__ret_u16_u64_Frame_t;
 /* declare_local */
 /* internal_declare_param */
LONGINT
__cdecl
return_parameter_convert__ret_u16_u64(
   /* Param_Type1 */ return_parameter_convert__UINT16 a_L_106);
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct return_parameter_convert__ret_u16_i8_Frame_t;typedef struct return_parameter_convert__ret_u16_i8_Frame_t return_parameter_convert__ret_u16_i8_Frame_t;
 /* declare_local */
 /* internal_declare_param */
INTEGER
__cdecl
return_parameter_convert__ret_u16_i8(
   /* Param_Type1 */ return_parameter_convert__UINT16 a_L_108);
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct return_parameter_convert__ret_u16_i32_Frame_t;typedef struct return_parameter_convert__ret_u16_i32_Frame_t return_parameter_convert__ret_u16_i32_Frame_t;
 /* declare_local */
 /* internal_declare_param */
INTEGER
__cdecl
return_parameter_convert__ret_u16_i32(
   /* Param_Type1 */ return_parameter_convert__UINT16 a_L_110);
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct return_parameter_convert__ret_u16_LC_Frame_t;typedef struct return_parameter_convert__ret_u16_LC_Frame_t return_parameter_convert__ret_u16_LC_Frame_t;
 /* declare_local */
 /* internal_declare_param */
LONGINT
__cdecl
return_parameter_convert__ret_u16_LC(
   /* Param_Type1 */ return_parameter_convert__UINT16 a_L_112);
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct return_parameter_convert__ret_u16_u16_Frame_t;typedef struct return_parameter_convert__ret_u16_u16_Frame_t return_parameter_convert__ret_u16_u16_Frame_t;
 /* declare_local */
 /* internal_declare_param */
INTEGER
__cdecl
return_parameter_convert__ret_u16_u16(
   /* Param_Type1 */ return_parameter_convert__UINT16 a_L_114);
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct return_parameter_convert__ret_u16_I_Frame_t;typedef struct return_parameter_convert__ret_u16_I_Frame_t return_parameter_convert__ret_u16_I_Frame_t;
 /* declare_local */
 /* internal_declare_param */
INTEGER
__cdecl
return_parameter_convert__ret_u16_I(
   /* Param_Type1 */ return_parameter_convert__UINT16 a_L_116);
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct return_parameter_convert__ret_u16_i64_Frame_t;typedef struct return_parameter_convert__ret_u16_i64_Frame_t return_parameter_convert__ret_u16_i64_Frame_t;
 /* declare_local */
 /* internal_declare_param */
LONGINT
__cdecl
return_parameter_convert__ret_u16_i64(
   /* Param_Type1 */ return_parameter_convert__UINT16 a_L_118);
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct return_parameter_convert__ret_u16_C_Frame_t;typedef struct return_parameter_convert__ret_u16_C_Frame_t return_parameter_convert__ret_u16_C_Frame_t;
 /* declare_local */
 /* internal_declare_param */
INTEGER
__cdecl
return_parameter_convert__ret_u16_C(
   /* Param_Type1 */ return_parameter_convert__UINT16 a_L_120);
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct return_parameter_convert__ret_u16_i16_Frame_t;typedef struct return_parameter_convert__ret_u16_i16_Frame_t return_parameter_convert__ret_u16_i16_Frame_t;
 /* declare_local */
 /* internal_declare_param */
INTEGER
__cdecl
return_parameter_convert__ret_u16_i16(
   /* Param_Type1 */ return_parameter_convert__UINT16 a_L_122);
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct return_parameter_convert__ret_u16_u32_Frame_t;typedef struct return_parameter_convert__ret_u16_u32_Frame_t return_parameter_convert__ret_u16_u32_Frame_t;
 /* declare_local */
 /* internal_declare_param */
INTEGER
__cdecl
return_parameter_convert__ret_u16_u32(
   /* Param_Type1 */ return_parameter_convert__UINT16 a_L_124);
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct return_parameter_convert__ret_u16_u8_Frame_t;typedef struct return_parameter_convert__ret_u16_u8_Frame_t return_parameter_convert__ret_u16_u8_Frame_t;
 /* declare_local */
 /* internal_declare_param */
INTEGER
__cdecl
return_parameter_convert__ret_u16_u8(
   /* Param_Type1 */ return_parameter_convert__UINT16 a_L_126);
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct return_parameter_convert__ret_u16_L_Frame_t;typedef struct return_parameter_convert__ret_u16_L_Frame_t return_parameter_convert__ret_u16_L_Frame_t;
 /* declare_local */
 /* internal_declare_param */
LONGINT
__cdecl
return_parameter_convert__ret_u16_L(
   /* Param_Type1 */ return_parameter_convert__UINT16 a_L_128);
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct return_parameter_convert__ret_I_u64_Frame_t;typedef struct return_parameter_convert__ret_I_u64_Frame_t return_parameter_convert__ret_I_u64_Frame_t;
 /* declare_local */
 /* internal_declare_param */
LONGINT
__cdecl
return_parameter_convert__ret_I_u64(
   /* Param_Type1 */ INTEGER a_L_130);
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct return_parameter_convert__ret_I_i8_Frame_t;typedef struct return_parameter_convert__ret_I_i8_Frame_t return_parameter_convert__ret_I_i8_Frame_t;
 /* declare_local */
 /* internal_declare_param */
INTEGER
__cdecl
return_parameter_convert__ret_I_i8(
   /* Param_Type1 */ INTEGER a_L_132);
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct return_parameter_convert__ret_I_i32_Frame_t;typedef struct return_parameter_convert__ret_I_i32_Frame_t return_parameter_convert__ret_I_i32_Frame_t;
 /* declare_local */
 /* internal_declare_param */
INTEGER
__cdecl
return_parameter_convert__ret_I_i32(
   /* Param_Type1 */ INTEGER a_L_134);
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct return_parameter_convert__ret_I_LC_Frame_t;typedef struct return_parameter_convert__ret_I_LC_Frame_t return_parameter_convert__ret_I_LC_Frame_t;
 /* declare_local */
 /* internal_declare_param */
LONGINT
__cdecl
return_parameter_convert__ret_I_LC(
   /* Param_Type1 */ INTEGER a_L_136);
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct return_parameter_convert__ret_I_u16_Frame_t;typedef struct return_parameter_convert__ret_I_u16_Frame_t return_parameter_convert__ret_I_u16_Frame_t;
 /* declare_local */
 /* internal_declare_param */
INTEGER
__cdecl
return_parameter_convert__ret_I_u16(
   /* Param_Type1 */ INTEGER a_L_138);
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct return_parameter_convert__ret_I_I_Frame_t;typedef struct return_parameter_convert__ret_I_I_Frame_t return_parameter_convert__ret_I_I_Frame_t;
 /* declare_local */
 /* internal_declare_param */
INTEGER
__cdecl
return_parameter_convert__ret_I_I(
   /* Param_Type1 */ INTEGER a_L_140);
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct return_parameter_convert__ret_I_i64_Frame_t;typedef struct return_parameter_convert__ret_I_i64_Frame_t return_parameter_convert__ret_I_i64_Frame_t;
 /* declare_local */
 /* internal_declare_param */
LONGINT
__cdecl
return_parameter_convert__ret_I_i64(
   /* Param_Type1 */ INTEGER a_L_142);
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct return_parameter_convert__ret_I_C_Frame_t;typedef struct return_parameter_convert__ret_I_C_Frame_t return_parameter_convert__ret_I_C_Frame_t;
 /* declare_local */
 /* internal_declare_param */
INTEGER
__cdecl
return_parameter_convert__ret_I_C(
   /* Param_Type1 */ INTEGER a_L_144);
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct return_parameter_convert__ret_I_i16_Frame_t;typedef struct return_parameter_convert__ret_I_i16_Frame_t return_parameter_convert__ret_I_i16_Frame_t;
 /* declare_local */
 /* internal_declare_param */
INTEGER
__cdecl
return_parameter_convert__ret_I_i16(
   /* Param_Type1 */ INTEGER a_L_146);
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct return_parameter_convert__ret_I_u32_Frame_t;typedef struct return_parameter_convert__ret_I_u32_Frame_t return_parameter_convert__ret_I_u32_Frame_t;
 /* declare_local */
 /* internal_declare_param */
INTEGER
__cdecl
return_parameter_convert__ret_I_u32(
   /* Param_Type1 */ INTEGER a_L_148);
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct return_parameter_convert__ret_I_u8_Frame_t;typedef struct return_parameter_convert__ret_I_u8_Frame_t return_parameter_convert__ret_I_u8_Frame_t;
 /* declare_local */
 /* internal_declare_param */
INTEGER
__cdecl
return_parameter_convert__ret_I_u8(
   /* Param_Type1 */ INTEGER a_L_150);
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct return_parameter_convert__ret_I_L_Frame_t;typedef struct return_parameter_convert__ret_I_L_Frame_t return_parameter_convert__ret_I_L_Frame_t;
 /* declare_local */
 /* internal_declare_param */
LONGINT
__cdecl
return_parameter_convert__ret_I_L(
   /* Param_Type1 */ INTEGER a_L_152);
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct return_parameter_convert__ret_i64_u64_Frame_t;typedef struct return_parameter_convert__ret_i64_u64_Frame_t return_parameter_convert__ret_i64_u64_Frame_t;
 /* declare_local */
 /* internal_declare_param */
LONGINT
__cdecl
return_parameter_convert__ret_i64_u64(
   /* Param_Type1 */ return_parameter_convert__INT64 a_L_154);
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct return_parameter_convert__ret_i64_i8_Frame_t;typedef struct return_parameter_convert__ret_i64_i8_Frame_t return_parameter_convert__ret_i64_i8_Frame_t;
 /* declare_local */
 /* internal_declare_param */
LONGINT
__cdecl
return_parameter_convert__ret_i64_i8(
   /* Param_Type1 */ return_parameter_convert__INT64 a_L_156);
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct return_parameter_convert__ret_i64_i32_Frame_t;typedef struct return_parameter_convert__ret_i64_i32_Frame_t return_parameter_convert__ret_i64_i32_Frame_t;
 /* declare_local */
 /* internal_declare_param */
LONGINT
__cdecl
return_parameter_convert__ret_i64_i32(
   /* Param_Type1 */ return_parameter_convert__INT64 a_L_158);
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct return_parameter_convert__ret_i64_LC_Frame_t;typedef struct return_parameter_convert__ret_i64_LC_Frame_t return_parameter_convert__ret_i64_LC_Frame_t;
 /* declare_local */
 /* internal_declare_param */
LONGINT
__cdecl
return_parameter_convert__ret_i64_LC(
   /* Param_Type1 */ return_parameter_convert__INT64 a_L_160);
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct return_parameter_convert__ret_i64_u16_Frame_t;typedef struct return_parameter_convert__ret_i64_u16_Frame_t return_parameter_convert__ret_i64_u16_Frame_t;
 /* declare_local */
 /* internal_declare_param */
LONGINT
__cdecl
return_parameter_convert__ret_i64_u16(
   /* Param_Type1 */ return_parameter_convert__INT64 a_L_162);
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct return_parameter_convert__ret_i64_I_Frame_t;typedef struct return_parameter_convert__ret_i64_I_Frame_t return_parameter_convert__ret_i64_I_Frame_t;
 /* declare_local */
 /* internal_declare_param */
LONGINT
__cdecl
return_parameter_convert__ret_i64_I(
   /* Param_Type1 */ return_parameter_convert__INT64 a_L_164);
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct return_parameter_convert__ret_i64_i64_Frame_t;typedef struct return_parameter_convert__ret_i64_i64_Frame_t return_parameter_convert__ret_i64_i64_Frame_t;
 /* declare_local */
 /* internal_declare_param */
LONGINT
__cdecl
return_parameter_convert__ret_i64_i64(
   /* Param_Type1 */ return_parameter_convert__INT64 a_L_166);
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct return_parameter_convert__ret_i64_C_Frame_t;typedef struct return_parameter_convert__ret_i64_C_Frame_t return_parameter_convert__ret_i64_C_Frame_t;
 /* declare_local */
 /* internal_declare_param */
LONGINT
__cdecl
return_parameter_convert__ret_i64_C(
   /* Param_Type1 */ return_parameter_convert__INT64 a_L_168);
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct return_parameter_convert__ret_i64_i16_Frame_t;typedef struct return_parameter_convert__ret_i64_i16_Frame_t return_parameter_convert__ret_i64_i16_Frame_t;
 /* declare_local */
 /* internal_declare_param */
LONGINT
__cdecl
return_parameter_convert__ret_i64_i16(
   /* Param_Type1 */ return_parameter_convert__INT64 a_L_170);
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct return_parameter_convert__ret_i64_u32_Frame_t;typedef struct return_parameter_convert__ret_i64_u32_Frame_t return_parameter_convert__ret_i64_u32_Frame_t;
 /* declare_local */
 /* internal_declare_param */
LONGINT
__cdecl
return_parameter_convert__ret_i64_u32(
   /* Param_Type1 */ return_parameter_convert__INT64 a_L_172);
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct return_parameter_convert__ret_i64_u8_Frame_t;typedef struct return_parameter_convert__ret_i64_u8_Frame_t return_parameter_convert__ret_i64_u8_Frame_t;
 /* declare_local */
 /* internal_declare_param */
LONGINT
__cdecl
return_parameter_convert__ret_i64_u8(
   /* Param_Type1 */ return_parameter_convert__INT64 a_L_174);
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct return_parameter_convert__ret_i64_L_Frame_t;typedef struct return_parameter_convert__ret_i64_L_Frame_t return_parameter_convert__ret_i64_L_Frame_t;
 /* declare_local */
 /* internal_declare_param */
LONGINT
__cdecl
return_parameter_convert__ret_i64_L(
   /* Param_Type1 */ return_parameter_convert__INT64 a_L_176);
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct return_parameter_convert__ret_C_u64_Frame_t;typedef struct return_parameter_convert__ret_C_u64_Frame_t return_parameter_convert__ret_C_u64_Frame_t;
 /* declare_local */
 /* internal_declare_param */
LONGINT
__cdecl
return_parameter_convert__ret_C_u64(
   /* Param_Type1 */ CARDINAL a_L_178);
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct return_parameter_convert__ret_C_i8_Frame_t;typedef struct return_parameter_convert__ret_C_i8_Frame_t return_parameter_convert__ret_C_i8_Frame_t;
 /* declare_local */
 /* internal_declare_param */
INTEGER
__cdecl
return_parameter_convert__ret_C_i8(
   /* Param_Type1 */ CARDINAL a_L_180);
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct return_parameter_convert__ret_C_i32_Frame_t;typedef struct return_parameter_convert__ret_C_i32_Frame_t return_parameter_convert__ret_C_i32_Frame_t;
 /* declare_local */
 /* internal_declare_param */
INTEGER
__cdecl
return_parameter_convert__ret_C_i32(
   /* Param_Type1 */ CARDINAL a_L_182);
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct return_parameter_convert__ret_C_LC_Frame_t;typedef struct return_parameter_convert__ret_C_LC_Frame_t return_parameter_convert__ret_C_LC_Frame_t;
 /* declare_local */
 /* internal_declare_param */
LONGINT
__cdecl
return_parameter_convert__ret_C_LC(
   /* Param_Type1 */ CARDINAL a_L_184);
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct return_parameter_convert__ret_C_u16_Frame_t;typedef struct return_parameter_convert__ret_C_u16_Frame_t return_parameter_convert__ret_C_u16_Frame_t;
 /* declare_local */
 /* internal_declare_param */
INTEGER
__cdecl
return_parameter_convert__ret_C_u16(
   /* Param_Type1 */ CARDINAL a_L_186);
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct return_parameter_convert__ret_C_I_Frame_t;typedef struct return_parameter_convert__ret_C_I_Frame_t return_parameter_convert__ret_C_I_Frame_t;
 /* declare_local */
 /* internal_declare_param */
INTEGER
__cdecl
return_parameter_convert__ret_C_I(
   /* Param_Type1 */ CARDINAL a_L_188);
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct return_parameter_convert__ret_C_i64_Frame_t;typedef struct return_parameter_convert__ret_C_i64_Frame_t return_parameter_convert__ret_C_i64_Frame_t;
 /* declare_local */
 /* internal_declare_param */
LONGINT
__cdecl
return_parameter_convert__ret_C_i64(
   /* Param_Type1 */ CARDINAL a_L_190);
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct return_parameter_convert__ret_C_C_Frame_t;typedef struct return_parameter_convert__ret_C_C_Frame_t return_parameter_convert__ret_C_C_Frame_t;
 /* declare_local */
 /* internal_declare_param */
INTEGER
__cdecl
return_parameter_convert__ret_C_C(
   /* Param_Type1 */ CARDINAL a_L_192);
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct return_parameter_convert__ret_C_i16_Frame_t;typedef struct return_parameter_convert__ret_C_i16_Frame_t return_parameter_convert__ret_C_i16_Frame_t;
 /* declare_local */
 /* internal_declare_param */
INTEGER
__cdecl
return_parameter_convert__ret_C_i16(
   /* Param_Type1 */ CARDINAL a_L_194);
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct return_parameter_convert__ret_C_u32_Frame_t;typedef struct return_parameter_convert__ret_C_u32_Frame_t return_parameter_convert__ret_C_u32_Frame_t;
 /* declare_local */
 /* internal_declare_param */
INTEGER
__cdecl
return_parameter_convert__ret_C_u32(
   /* Param_Type1 */ CARDINAL a_L_196);
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct return_parameter_convert__ret_C_u8_Frame_t;typedef struct return_parameter_convert__ret_C_u8_Frame_t return_parameter_convert__ret_C_u8_Frame_t;
 /* declare_local */
 /* internal_declare_param */
INTEGER
__cdecl
return_parameter_convert__ret_C_u8(
   /* Param_Type1 */ CARDINAL a_L_198);
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct return_parameter_convert__ret_C_L_Frame_t;typedef struct return_parameter_convert__ret_C_L_Frame_t return_parameter_convert__ret_C_L_Frame_t;
 /* declare_local */
 /* internal_declare_param */
LONGINT
__cdecl
return_parameter_convert__ret_C_L(
   /* Param_Type1 */ CARDINAL a_L_200);
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct return_parameter_convert__ret_i16_u64_Frame_t;typedef struct return_parameter_convert__ret_i16_u64_Frame_t return_parameter_convert__ret_i16_u64_Frame_t;
 /* declare_local */
 /* internal_declare_param */
LONGINT
__cdecl
return_parameter_convert__ret_i16_u64(
   /* Param_Type1 */ return_parameter_convert__INT16 a_L_202);
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct return_parameter_convert__ret_i16_i8_Frame_t;typedef struct return_parameter_convert__ret_i16_i8_Frame_t return_parameter_convert__ret_i16_i8_Frame_t;
 /* declare_local */
 /* internal_declare_param */
INTEGER
__cdecl
return_parameter_convert__ret_i16_i8(
   /* Param_Type1 */ return_parameter_convert__INT16 a_L_204);
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct return_parameter_convert__ret_i16_i32_Frame_t;typedef struct return_parameter_convert__ret_i16_i32_Frame_t return_parameter_convert__ret_i16_i32_Frame_t;
 /* declare_local */
 /* internal_declare_param */
INTEGER
__cdecl
return_parameter_convert__ret_i16_i32(
   /* Param_Type1 */ return_parameter_convert__INT16 a_L_206);
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct return_parameter_convert__ret_i16_LC_Frame_t;typedef struct return_parameter_convert__ret_i16_LC_Frame_t return_parameter_convert__ret_i16_LC_Frame_t;
 /* declare_local */
 /* internal_declare_param */
LONGINT
__cdecl
return_parameter_convert__ret_i16_LC(
   /* Param_Type1 */ return_parameter_convert__INT16 a_L_208);
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct return_parameter_convert__ret_i16_u16_Frame_t;typedef struct return_parameter_convert__ret_i16_u16_Frame_t return_parameter_convert__ret_i16_u16_Frame_t;
 /* declare_local */
 /* internal_declare_param */
INTEGER
__cdecl
return_parameter_convert__ret_i16_u16(
   /* Param_Type1 */ return_parameter_convert__INT16 a_L_210);
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct return_parameter_convert__ret_i16_I_Frame_t;typedef struct return_parameter_convert__ret_i16_I_Frame_t return_parameter_convert__ret_i16_I_Frame_t;
 /* declare_local */
 /* internal_declare_param */
INTEGER
__cdecl
return_parameter_convert__ret_i16_I(
   /* Param_Type1 */ return_parameter_convert__INT16 a_L_212);
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct return_parameter_convert__ret_i16_i64_Frame_t;typedef struct return_parameter_convert__ret_i16_i64_Frame_t return_parameter_convert__ret_i16_i64_Frame_t;
 /* declare_local */
 /* internal_declare_param */
LONGINT
__cdecl
return_parameter_convert__ret_i16_i64(
   /* Param_Type1 */ return_parameter_convert__INT16 a_L_214);
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct return_parameter_convert__ret_i16_C_Frame_t;typedef struct return_parameter_convert__ret_i16_C_Frame_t return_parameter_convert__ret_i16_C_Frame_t;
 /* declare_local */
 /* internal_declare_param */
INTEGER
__cdecl
return_parameter_convert__ret_i16_C(
   /* Param_Type1 */ return_parameter_convert__INT16 a_L_216);
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct return_parameter_convert__ret_i16_i16_Frame_t;typedef struct return_parameter_convert__ret_i16_i16_Frame_t return_parameter_convert__ret_i16_i16_Frame_t;
 /* declare_local */
 /* internal_declare_param */
INTEGER
__cdecl
return_parameter_convert__ret_i16_i16(
   /* Param_Type1 */ return_parameter_convert__INT16 a_L_218);
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct return_parameter_convert__ret_i16_u32_Frame_t;typedef struct return_parameter_convert__ret_i16_u32_Frame_t return_parameter_convert__ret_i16_u32_Frame_t;
 /* declare_local */
 /* internal_declare_param */
INTEGER
__cdecl
return_parameter_convert__ret_i16_u32(
   /* Param_Type1 */ return_parameter_convert__INT16 a_L_220);
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct return_parameter_convert__ret_i16_u8_Frame_t;typedef struct return_parameter_convert__ret_i16_u8_Frame_t return_parameter_convert__ret_i16_u8_Frame_t;
 /* declare_local */
 /* internal_declare_param */
INTEGER
__cdecl
return_parameter_convert__ret_i16_u8(
   /* Param_Type1 */ return_parameter_convert__INT16 a_L_222);
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct return_parameter_convert__ret_i16_L_Frame_t;typedef struct return_parameter_convert__ret_i16_L_Frame_t return_parameter_convert__ret_i16_L_Frame_t;
 /* declare_local */
 /* internal_declare_param */
LONGINT
__cdecl
return_parameter_convert__ret_i16_L(
   /* Param_Type1 */ return_parameter_convert__INT16 a_L_224);
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct return_parameter_convert__ret_u32_u64_Frame_t;typedef struct return_parameter_convert__ret_u32_u64_Frame_t return_parameter_convert__ret_u32_u64_Frame_t;
 /* declare_local */
 /* internal_declare_param */
LONGINT
__cdecl
return_parameter_convert__ret_u32_u64(
   /* Param_Type1 */ return_parameter_convert__UINT32 a_L_226);
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct return_parameter_convert__ret_u32_i8_Frame_t;typedef struct return_parameter_convert__ret_u32_i8_Frame_t return_parameter_convert__ret_u32_i8_Frame_t;
 /* declare_local */
 /* internal_declare_param */
INTEGER
__cdecl
return_parameter_convert__ret_u32_i8(
   /* Param_Type1 */ return_parameter_convert__UINT32 a_L_228);
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct return_parameter_convert__ret_u32_i32_Frame_t;typedef struct return_parameter_convert__ret_u32_i32_Frame_t return_parameter_convert__ret_u32_i32_Frame_t;
 /* declare_local */
 /* internal_declare_param */
INTEGER
__cdecl
return_parameter_convert__ret_u32_i32(
   /* Param_Type1 */ return_parameter_convert__UINT32 a_L_230);
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct return_parameter_convert__ret_u32_LC_Frame_t;typedef struct return_parameter_convert__ret_u32_LC_Frame_t return_parameter_convert__ret_u32_LC_Frame_t;
 /* declare_local */
 /* internal_declare_param */
LONGINT
__cdecl
return_parameter_convert__ret_u32_LC(
   /* Param_Type1 */ return_parameter_convert__UINT32 a_L_232);
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct return_parameter_convert__ret_u32_u16_Frame_t;typedef struct return_parameter_convert__ret_u32_u16_Frame_t return_parameter_convert__ret_u32_u16_Frame_t;
 /* declare_local */
 /* internal_declare_param */
INTEGER
__cdecl
return_parameter_convert__ret_u32_u16(
   /* Param_Type1 */ return_parameter_convert__UINT32 a_L_234);
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct return_parameter_convert__ret_u32_I_Frame_t;typedef struct return_parameter_convert__ret_u32_I_Frame_t return_parameter_convert__ret_u32_I_Frame_t;
 /* declare_local */
 /* internal_declare_param */
INTEGER
__cdecl
return_parameter_convert__ret_u32_I(
   /* Param_Type1 */ return_parameter_convert__UINT32 a_L_236);
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct return_parameter_convert__ret_u32_i64_Frame_t;typedef struct return_parameter_convert__ret_u32_i64_Frame_t return_parameter_convert__ret_u32_i64_Frame_t;
 /* declare_local */
 /* internal_declare_param */
LONGINT
__cdecl
return_parameter_convert__ret_u32_i64(
   /* Param_Type1 */ return_parameter_convert__UINT32 a_L_238);
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct return_parameter_convert__ret_u32_C_Frame_t;typedef struct return_parameter_convert__ret_u32_C_Frame_t return_parameter_convert__ret_u32_C_Frame_t;
 /* declare_local */
 /* internal_declare_param */
INTEGER
__cdecl
return_parameter_convert__ret_u32_C(
   /* Param_Type1 */ return_parameter_convert__UINT32 a_L_240);
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct return_parameter_convert__ret_u32_i16_Frame_t;typedef struct return_parameter_convert__ret_u32_i16_Frame_t return_parameter_convert__ret_u32_i16_Frame_t;
 /* declare_local */
 /* internal_declare_param */
INTEGER
__cdecl
return_parameter_convert__ret_u32_i16(
   /* Param_Type1 */ return_parameter_convert__UINT32 a_L_242);
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct return_parameter_convert__ret_u32_u32_Frame_t;typedef struct return_parameter_convert__ret_u32_u32_Frame_t return_parameter_convert__ret_u32_u32_Frame_t;
 /* declare_local */
 /* internal_declare_param */
INTEGER
__cdecl
return_parameter_convert__ret_u32_u32(
   /* Param_Type1 */ return_parameter_convert__UINT32 a_L_244);
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct return_parameter_convert__ret_u32_u8_Frame_t;typedef struct return_parameter_convert__ret_u32_u8_Frame_t return_parameter_convert__ret_u32_u8_Frame_t;
 /* declare_local */
 /* internal_declare_param */
INTEGER
__cdecl
return_parameter_convert__ret_u32_u8(
   /* Param_Type1 */ return_parameter_convert__UINT32 a_L_246);
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct return_parameter_convert__ret_u32_L_Frame_t;typedef struct return_parameter_convert__ret_u32_L_Frame_t return_parameter_convert__ret_u32_L_Frame_t;
 /* declare_local */
 /* internal_declare_param */
LONGINT
__cdecl
return_parameter_convert__ret_u32_L(
   /* Param_Type1 */ return_parameter_convert__UINT32 a_L_248);
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct return_parameter_convert__ret_u8_u64_Frame_t;typedef struct return_parameter_convert__ret_u8_u64_Frame_t return_parameter_convert__ret_u8_u64_Frame_t;
 /* declare_local */
 /* internal_declare_param */
LONGINT
__cdecl
return_parameter_convert__ret_u8_u64(
   /* Param_Type1 */ return_parameter_convert__UINT8 a_L_250);
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct return_parameter_convert__ret_u8_i8_Frame_t;typedef struct return_parameter_convert__ret_u8_i8_Frame_t return_parameter_convert__ret_u8_i8_Frame_t;
 /* declare_local */
 /* internal_declare_param */
INTEGER
__cdecl
return_parameter_convert__ret_u8_i8(
   /* Param_Type1 */ return_parameter_convert__UINT8 a_L_252);
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct return_parameter_convert__ret_u8_i32_Frame_t;typedef struct return_parameter_convert__ret_u8_i32_Frame_t return_parameter_convert__ret_u8_i32_Frame_t;
 /* declare_local */
 /* internal_declare_param */
INTEGER
__cdecl
return_parameter_convert__ret_u8_i32(
   /* Param_Type1 */ return_parameter_convert__UINT8 a_L_254);
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct return_parameter_convert__ret_u8_LC_Frame_t;typedef struct return_parameter_convert__ret_u8_LC_Frame_t return_parameter_convert__ret_u8_LC_Frame_t;
 /* declare_local */
 /* internal_declare_param */
LONGINT
__cdecl
return_parameter_convert__ret_u8_LC(
   /* Param_Type1 */ return_parameter_convert__UINT8 a_L_256);
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct return_parameter_convert__ret_u8_u16_Frame_t;typedef struct return_parameter_convert__ret_u8_u16_Frame_t return_parameter_convert__ret_u8_u16_Frame_t;
 /* declare_local */
 /* internal_declare_param */
INTEGER
__cdecl
return_parameter_convert__ret_u8_u16(
   /* Param_Type1 */ return_parameter_convert__UINT8 a_L_258);
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct return_parameter_convert__ret_u8_I_Frame_t;typedef struct return_parameter_convert__ret_u8_I_Frame_t return_parameter_convert__ret_u8_I_Frame_t;
 /* declare_local */
 /* internal_declare_param */
INTEGER
__cdecl
return_parameter_convert__ret_u8_I(
   /* Param_Type1 */ return_parameter_convert__UINT8 a_L_260);
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct return_parameter_convert__ret_u8_i64_Frame_t;typedef struct return_parameter_convert__ret_u8_i64_Frame_t return_parameter_convert__ret_u8_i64_Frame_t;
 /* declare_local */
 /* internal_declare_param */
LONGINT
__cdecl
return_parameter_convert__ret_u8_i64(
   /* Param_Type1 */ return_parameter_convert__UINT8 a_L_262);
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct return_parameter_convert__ret_u8_C_Frame_t;typedef struct return_parameter_convert__ret_u8_C_Frame_t return_parameter_convert__ret_u8_C_Frame_t;
 /* declare_local */
 /* internal_declare_param */
INTEGER
__cdecl
return_parameter_convert__ret_u8_C(
   /* Param_Type1 */ return_parameter_convert__UINT8 a_L_264);
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct return_parameter_convert__ret_u8_i16_Frame_t;typedef struct return_parameter_convert__ret_u8_i16_Frame_t return_parameter_convert__ret_u8_i16_Frame_t;
 /* declare_local */
 /* internal_declare_param */
INTEGER
__cdecl
return_parameter_convert__ret_u8_i16(
   /* Param_Type1 */ return_parameter_convert__UINT8 a_L_266);
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct return_parameter_convert__ret_u8_u32_Frame_t;typedef struct return_parameter_convert__ret_u8_u32_Frame_t return_parameter_convert__ret_u8_u32_Frame_t;
 /* declare_local */
 /* internal_declare_param */
INTEGER
__cdecl
return_parameter_convert__ret_u8_u32(
   /* Param_Type1 */ return_parameter_convert__UINT8 a_L_268);
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct return_parameter_convert__ret_u8_u8_Frame_t;typedef struct return_parameter_convert__ret_u8_u8_Frame_t return_parameter_convert__ret_u8_u8_Frame_t;
 /* declare_local */
 /* internal_declare_param */
INTEGER
__cdecl
return_parameter_convert__ret_u8_u8(
   /* Param_Type1 */ return_parameter_convert__UINT8 a_L_270);
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct return_parameter_convert__ret_u8_L_Frame_t;typedef struct return_parameter_convert__ret_u8_L_Frame_t return_parameter_convert__ret_u8_L_Frame_t;
 /* declare_local */
 /* internal_declare_param */
LONGINT
__cdecl
return_parameter_convert__ret_u8_L(
   /* Param_Type1 */ return_parameter_convert__UINT8 a_L_272);
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct return_parameter_convert__ret_L_u64_Frame_t;typedef struct return_parameter_convert__ret_L_u64_Frame_t return_parameter_convert__ret_L_u64_Frame_t;
 /* declare_local */
 /* internal_declare_param */
LONGINT
__cdecl
return_parameter_convert__ret_L_u64(
   /* Param_Type1 */ LONGINT a_L_274);
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct return_parameter_convert__ret_L_i8_Frame_t;typedef struct return_parameter_convert__ret_L_i8_Frame_t return_parameter_convert__ret_L_i8_Frame_t;
 /* declare_local */
 /* internal_declare_param */
LONGINT
__cdecl
return_parameter_convert__ret_L_i8(
   /* Param_Type1 */ LONGINT a_L_276);
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct return_parameter_convert__ret_L_i32_Frame_t;typedef struct return_parameter_convert__ret_L_i32_Frame_t return_parameter_convert__ret_L_i32_Frame_t;
 /* declare_local */
 /* internal_declare_param */
LONGINT
__cdecl
return_parameter_convert__ret_L_i32(
   /* Param_Type1 */ LONGINT a_L_278);
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct return_parameter_convert__ret_L_LC_Frame_t;typedef struct return_parameter_convert__ret_L_LC_Frame_t return_parameter_convert__ret_L_LC_Frame_t;
 /* declare_local */
 /* internal_declare_param */
LONGINT
__cdecl
return_parameter_convert__ret_L_LC(
   /* Param_Type1 */ LONGINT a_L_280);
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct return_parameter_convert__ret_L_u16_Frame_t;typedef struct return_parameter_convert__ret_L_u16_Frame_t return_parameter_convert__ret_L_u16_Frame_t;
 /* declare_local */
 /* internal_declare_param */
LONGINT
__cdecl
return_parameter_convert__ret_L_u16(
   /* Param_Type1 */ LONGINT a_L_282);
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct return_parameter_convert__ret_L_I_Frame_t;typedef struct return_parameter_convert__ret_L_I_Frame_t return_parameter_convert__ret_L_I_Frame_t;
 /* declare_local */
 /* internal_declare_param */
LONGINT
__cdecl
return_parameter_convert__ret_L_I(
   /* Param_Type1 */ LONGINT a_L_284);
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct return_parameter_convert__ret_L_i64_Frame_t;typedef struct return_parameter_convert__ret_L_i64_Frame_t return_parameter_convert__ret_L_i64_Frame_t;
 /* declare_local */
 /* internal_declare_param */
LONGINT
__cdecl
return_parameter_convert__ret_L_i64(
   /* Param_Type1 */ LONGINT a_L_286);
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct return_parameter_convert__ret_L_C_Frame_t;typedef struct return_parameter_convert__ret_L_C_Frame_t return_parameter_convert__ret_L_C_Frame_t;
 /* declare_local */
 /* internal_declare_param */
LONGINT
__cdecl
return_parameter_convert__ret_L_C(
   /* Param_Type1 */ LONGINT a_L_288);
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct return_parameter_convert__ret_L_i16_Frame_t;typedef struct return_parameter_convert__ret_L_i16_Frame_t return_parameter_convert__ret_L_i16_Frame_t;
 /* declare_local */
 /* internal_declare_param */
LONGINT
__cdecl
return_parameter_convert__ret_L_i16(
   /* Param_Type1 */ LONGINT a_L_290);
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct return_parameter_convert__ret_L_u32_Frame_t;typedef struct return_parameter_convert__ret_L_u32_Frame_t return_parameter_convert__ret_L_u32_Frame_t;
 /* declare_local */
 /* internal_declare_param */
LONGINT
__cdecl
return_parameter_convert__ret_L_u32(
   /* Param_Type1 */ LONGINT a_L_292);
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct return_parameter_convert__ret_L_u8_Frame_t;typedef struct return_parameter_convert__ret_L_u8_Frame_t return_parameter_convert__ret_L_u8_Frame_t;
 /* declare_local */
 /* internal_declare_param */
LONGINT
__cdecl
return_parameter_convert__ret_L_u8(
   /* Param_Type1 */ LONGINT a_L_294);
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct return_parameter_convert__ret_L_L_Frame_t;typedef struct return_parameter_convert__ret_L_L_Frame_t return_parameter_convert__ret_L_L_Frame_t;
 /* declare_local */
 /* internal_declare_param */
LONGINT
__cdecl
return_parameter_convert__ret_L_L(
   /* Param_Type1 */ LONGINT a_L_296);
 /* Locals_begin_procedure */
 /* begin_block */
 /* Locals_end_procedure */
 /* end_block */
 /* Locals_begin_procedure */
 /* begin_block */
 /* Locals_end_procedure */
 /* end_block */
 /* Locals_begin_procedure */
 /* begin_block */
 /* Locals_end_procedure */
 /* end_block */
 /* Locals_begin_procedure */
 /* begin_block */
 /* Locals_end_procedure */
 /* end_block */
 /* Locals_begin_procedure */
 /* begin_block */
 /* Locals_end_procedure */
 /* end_block */
 /* Locals_begin_procedure */
 /* begin_block */
 /* Locals_end_procedure */
 /* end_block */
 /* Locals_begin_procedure */
 /* begin_block */
 /* Locals_end_procedure */
 /* end_block */
 /* Locals_begin_procedure */
 /* begin_block */
 /* Locals_end_procedure */
 /* end_block */
 /* Locals_begin_procedure */
 /* begin_block */
 /* Locals_end_procedure */
 /* end_block */
 /* Locals_begin_procedure */
 /* begin_block */
 /* Locals_end_procedure */
 /* end_block */
 /* Locals_begin_procedure */
 /* begin_block */
 /* Locals_end_procedure */
 /* end_block */
 /* Locals_begin_procedure */
 /* begin_block */
 /* Locals_end_procedure */
 /* end_block */
 /* Locals_begin_procedure */
 /* begin_block */
 /* Locals_end_procedure */
 /* end_block */
 /* Locals_begin_procedure */
 /* begin_block */
 /* Locals_end_procedure */
 /* end_block */
 /* Locals_begin_procedure */
 /* begin_block */
 /* Locals_end_procedure */
 /* end_block */
 /* Locals_begin_procedure */
 /* begin_block */
 /* Locals_end_procedure */
 /* end_block */
 /* Locals_begin_procedure */
 /* begin_block */
 /* Locals_end_procedure */
 /* end_block */
 /* Locals_begin_procedure */
 /* begin_block */
 /* Locals_end_procedure */
 /* end_block */
 /* Locals_begin_procedure */
 /* begin_block */
 /* Locals_end_procedure */
 /* end_block */
 /* Locals_begin_procedure */
 /* begin_block */
 /* Locals_end_procedure */
 /* end_block */
 /* Locals_begin_procedure */
 /* begin_block */
 /* Locals_end_procedure */
 /* end_block */
 /* Locals_begin_procedure */
 /* begin_block */
 /* Locals_end_procedure */
 /* end_block */
 /* Locals_begin_procedure */
 /* begin_block */
 /* Locals_end_procedure */
 /* end_block */
 /* Locals_begin_procedure */
 /* begin_block */
 /* Locals_end_procedure */
 /* end_block */
 /* Locals_begin_procedure */
 /* begin_block */
 /* Locals_end_procedure */
 /* end_block */
 /* Locals_begin_procedure */
 /* begin_block */
 /* Locals_end_procedure */
 /* end_block */
 /* Locals_begin_procedure */
 /* begin_block */
 /* Locals_end_procedure */
 /* end_block */
 /* Locals_begin_procedure */
 /* begin_block */
 /* Locals_end_procedure */
 /* end_block */
 /* Locals_begin_procedure */
 /* begin_block */
 /* Locals_end_procedure */
 /* end_block */
 /* Locals_begin_procedure */
 /* begin_block */
 /* Locals_end_procedure */
 /* end_block */
 /* Locals_begin_procedure */
 /* begin_block */
 /* Locals_end_procedure */
 /* end_block */
 /* Locals_begin_procedure */
 /* begin_block */
 /* Locals_end_procedure */
 /* end_block */
 /* Locals_begin_procedure */
 /* begin_block */
 /* Locals_end_procedure */
 /* end_block */
 /* Locals_begin_procedure */
 /* begin_block */
 /* Locals_end_procedure */
 /* end_block */
 /* Locals_begin_procedure */
 /* begin_block */
 /* Locals_end_procedure */
 /* end_block */
 /* Locals_begin_procedure */
 /* begin_block */
 /* Locals_end_procedure */
 /* end_block */
 /* Locals_begin_procedure */
 /* begin_block */
 /* Locals_end_procedure */
 /* end_block */
 /* Locals_begin_procedure */
 /* begin_block */
 /* Locals_end_procedure */
 /* end_block */
 /* Locals_begin_procedure */
 /* begin_block */
 /* Locals_end_procedure */
 /* end_block */
 /* Locals_begin_procedure */
 /* begin_block */
 /* Locals_end_procedure */
 /* end_block */
 /* Locals_begin_procedure */
 /* begin_block */
 /* Locals_end_procedure */
 /* end_block */
 /* Locals_begin_procedure */
 /* begin_block */
 /* Locals_end_procedure */
 /* end_block */
 /* Locals_begin_procedure */
 /* begin_block */
 /* Locals_end_procedure */
 /* end_block */
 /* Locals_begin_procedure */
 /* begin_block */
 /* Locals_end_procedure */
 /* end_block */
 /* Locals_begin_procedure */
 /* begin_block */
 /* Locals_end_procedure */
 /* end_block */
 /* Locals_begin_procedure */
 /* begin_block */
 /* Locals_end_procedure */
 /* end_block */
 /* Locals_begin_procedure */
 /* begin_block */
 /* Locals_end_procedure */
 /* end_block */
 /* Locals_begin_procedure */
 /* begin_block */
 /* Locals_end_procedure */
 /* end_block */
 /* Locals_begin_procedure */
 /* begin_block */
 /* Locals_end_procedure */
 /* end_block */
 /* Locals_begin_procedure */
 /* begin_block */
 /* Locals_end_procedure */
 /* end_block */
 /* Locals_begin_procedure */
 /* begin_block */
 /* Locals_end_procedure */
 /* end_block */
 /* Locals_begin_procedure */
 /* begin_block */
 /* Locals_end_procedure */
 /* end_block */
 /* Locals_begin_procedure */
 /* begin_block */
 /* Locals_end_procedure */
 /* end_block */
 /* Locals_begin_procedure */
 /* begin_block */
 /* Locals_end_procedure */
 /* end_block */
 /* Locals_begin_procedure */
 /* begin_block */
 /* Locals_end_procedure */
 /* end_block */
 /* Locals_begin_procedure */
 /* begin_block */
 /* Locals_end_procedure */
 /* end_block */
 /* Locals_begin_procedure */
 /* begin_block */
 /* Locals_end_procedure */
 /* end_block */
 /* Locals_begin_procedure */
 /* begin_block */
 /* Locals_end_procedure */
 /* end_block */
 /* Locals_begin_procedure */
 /* begin_block */
 /* Locals_end_procedure */
 /* end_block */
 /* Locals_begin_procedure */
 /* begin_block */
 /* Locals_end_procedure */
 /* end_block */
 /* Locals_begin_procedure */
 /* begin_block */
 /* Locals_end_procedure */
 /* end_block */
 /* Locals_begin_procedure */
 /* begin_block */
 /* Locals_end_procedure */
 /* end_block */
 /* Locals_begin_procedure */
 /* begin_block */
 /* Locals_end_procedure */
 /* end_block */
 /* Locals_begin_procedure */
 /* begin_block */
 /* Locals_end_procedure */
 /* end_block */
 /* Locals_begin_procedure */
 /* begin_block */
 /* Locals_end_procedure */
 /* end_block */
 /* Locals_begin_procedure */
 /* begin_block */
 /* Locals_end_procedure */
 /* end_block */
 /* Locals_begin_procedure */
 /* begin_block */
 /* Locals_end_procedure */
 /* end_block */
 /* Locals_begin_procedure */
 /* begin_block */
 /* Locals_end_procedure */
 /* end_block */
 /* Locals_begin_procedure */
 /* begin_block */
 /* Locals_end_procedure */
 /* end_block */
 /* Locals_begin_procedure */
 /* begin_block */
 /* Locals_end_procedure */
 /* end_block */
 /* Locals_begin_procedure */
 /* begin_block */
 /* Locals_end_procedure */
 /* end_block */
 /* Locals_begin_procedure */
 /* begin_block */
 /* Locals_end_procedure */
 /* end_block */
 /* Locals_begin_procedure */
 /* begin_block */
 /* Locals_end_procedure */
 /* end_block */
 /* Locals_begin_procedure */
 /* begin_block */
 /* Locals_end_procedure */
 /* end_block */
 /* Locals_begin_procedure */
 /* begin_block */
 /* Locals_end_procedure */
 /* end_block */
 /* Locals_begin_procedure */
 /* begin_block */
 /* Locals_end_procedure */
 /* end_block */
 /* Locals_begin_procedure */
 /* begin_block */
 /* Locals_end_procedure */
 /* end_block */
 /* Locals_begin_procedure */
 /* begin_block */
 /* Locals_end_procedure */
 /* end_block */
 /* Locals_begin_procedure */
 /* begin_block */
 /* Locals_end_procedure */
 /* end_block */
 /* Locals_begin_procedure */
 /* begin_block */
 /* Locals_end_procedure */
 /* end_block */
 /* Locals_begin_procedure */
 /* begin_block */
 /* Locals_end_procedure */
 /* end_block */
 /* Locals_begin_procedure */
 /* begin_block */
 /* Locals_end_procedure */
 /* end_block */
 /* Locals_begin_procedure */
 /* begin_block */
 /* Locals_end_procedure */
 /* end_block */
 /* Locals_begin_procedure */
 /* begin_block */
 /* Locals_end_procedure */
 /* end_block */
 /* Locals_begin_procedure */
 /* begin_block */
 /* Locals_end_procedure */
 /* end_block */
 /* Locals_begin_procedure */
 /* begin_block */
 /* Locals_end_procedure */
 /* end_block */
 /* Locals_begin_procedure */
 /* begin_block */
 /* Locals_end_procedure */
 /* end_block */
 /* Locals_begin_procedure */
 /* begin_block */
 /* Locals_end_procedure */
 /* end_block */
 /* Locals_begin_procedure */
 /* begin_block */
 /* Locals_end_procedure */
 /* end_block */
 /* Locals_begin_procedure */
 /* begin_block */
 /* Locals_end_procedure */
 /* end_block */
 /* Locals_begin_procedure */
 /* begin_block */
 /* Locals_end_procedure */
 /* end_block */
 /* Locals_begin_procedure */
 /* begin_block */
 /* Locals_end_procedure */
 /* end_block */
 /* Locals_begin_procedure */
 /* begin_block */
 /* Locals_end_procedure */
 /* end_block */
 /* Locals_begin_procedure */
 /* begin_block */
 /* Locals_end_procedure */
 /* end_block */
 /* Locals_begin_procedure */
 /* begin_block */
 /* Locals_end_procedure */
 /* end_block */
 /* Locals_begin_procedure */
 /* begin_block */
 /* Locals_end_procedure */
 /* end_block */
 /* Locals_begin_procedure */
 /* begin_block */
 /* Locals_end_procedure */
 /* end_block */
 /* Locals_begin_procedure */
 /* begin_block */
 /* Locals_end_procedure */
 /* end_block */
 /* Locals_begin_procedure */
 /* begin_block */
 /* Locals_end_procedure */
 /* end_block */
 /* Locals_begin_procedure */
 /* begin_block */
 /* Locals_end_procedure */
 /* end_block */
 /* Locals_begin_procedure */
 /* begin_block */
 /* Locals_end_procedure */
 /* end_block */
 /* Locals_begin_procedure */
 /* begin_block */
 /* Locals_end_procedure */
 /* end_block */
 /* Locals_begin_procedure */
 /* begin_block */
 /* Locals_end_procedure */
 /* end_block */
 /* Locals_begin_procedure */
 /* begin_block */
 /* Locals_end_procedure */
 /* end_block */
 /* Locals_begin_procedure */
 /* begin_block */
 /* Locals_end_procedure */
 /* end_block */
 /* Locals_begin_procedure */
 /* begin_block */
 /* Locals_end_procedure */
 /* end_block */
 /* Locals_begin_procedure */
 /* begin_block */
 /* Locals_end_procedure */
 /* end_block */
 /* Locals_begin_procedure */
 /* begin_block */
 /* Locals_end_procedure */
 /* end_block */
 /* Locals_begin_procedure */
 /* begin_block */
 /* Locals_end_procedure */
 /* end_block */
 /* Locals_begin_procedure */
 /* begin_block */
 /* Locals_end_procedure */
 /* end_block */
 /* Locals_begin_procedure */
 /* begin_block */
 /* Locals_end_procedure */
 /* end_block */
 /* Locals_begin_procedure */
 /* begin_block */
 /* Locals_end_procedure */
 /* end_block */
 /* Locals_begin_procedure */
 /* begin_block */
 /* Locals_end_procedure */
 /* end_block */
 /* Locals_begin_procedure */
 /* begin_block */
 /* Locals_end_procedure */
 /* end_block */
 /* Locals_begin_procedure */
 /* begin_block */
 /* Locals_end_procedure */
 /* end_block */
 /* Locals_begin_procedure */
 /* begin_block */
 /* Locals_end_procedure */
 /* end_block */
 /* Locals_begin_procedure */
 /* begin_block */
 /* Locals_end_procedure */
 /* end_block */
 /* Locals_begin_procedure */
 /* begin_block */
 /* Locals_end_procedure */
 /* end_block */
 /* Locals_begin_procedure */
 /* begin_block */
 /* Locals_end_procedure */
 /* end_block */
 /* Locals_begin_procedure */
 /* begin_block */
 /* Locals_end_procedure */
 /* end_block */
 /* Locals_begin_procedure */
 /* begin_block */
 /* Locals_end_procedure */
 /* end_block */
 /* Locals_begin_procedure */
 /* begin_block */
 /* Locals_end_procedure */
 /* end_block */
 /* Locals_begin_procedure */
 /* begin_block */
 /* Locals_end_procedure */
 /* end_block */
 /* Locals_begin_procedure */
 /* begin_block */
 /* Locals_end_procedure */
 /* end_block */
 /* Locals_begin_procedure */
 /* begin_block */
 /* Locals_end_procedure */
 /* end_block */
 /* Locals_begin_procedure */
 /* begin_block */
 /* Locals_end_procedure */
 /* end_block */
 /* Locals_begin_procedure */
 /* begin_block */
 /* Locals_end_procedure */
 /* end_block */
 /* Locals_begin_procedure */
 /* begin_block */
 /* Locals_end_procedure */
 /* end_block */
 /* Locals_begin_procedure */
 /* begin_block */
 /* Locals_end_procedure */
 /* end_block */
 /* Locals_begin_procedure */
 /* begin_block */
 /* Locals_end_procedure */
 /* end_block */
 /* Locals_begin_procedure */
 /* begin_block */
 /* Locals_end_procedure */
 /* end_block */
 /* Locals_begin_procedure */
 /* begin_block */
 /* Locals_end_procedure */
 /* end_block */
 /* Locals_begin_procedure */
 /* begin_block */
 /* Locals_end_procedure */
 /* end_block */
 /* Locals_begin_procedure */
 /* begin_block */
 /* Locals_end_procedure */
 /* end_block */
 /* Locals_begin_procedure */
 /* begin_block */
 /* Locals_end_procedure */
 /* end_block */
 /* Locals_begin_procedure */
 /* begin_block */
 /* Locals_end_procedure */
 /* end_block */
 /* Locals_begin_procedure */
 /* begin_block */
 /* Locals_end_procedure */
 /* end_block */
 /* Locals_begin_procedure */
 /* begin_block */
 /* Locals_end_procedure */
 /* end_block */
 /* Locals_begin_procedure */
 /* begin_block */
 /* Locals_end_procedure */
 /* end_block */
 /* Locals_begin_procedure */
 /* begin_block */
 /* Locals_end_procedure */
 /* end_block */
 /* Locals_begin_procedure */
 /* begin_block */
 /* Locals_end_procedure */
 /* end_block */
 /* Locals_begin_procedure */
 /* begin_block */
 /* Locals_end_procedure */
 /* end_block */
 /* Locals_begin_procedure */
 /* begin_block */
 /* Locals_end_procedure */
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
 /* init_proc */
 /* init_var */
 /* init_proc */
 /* init_var */
 /* init_proc */
 /* init_var */
 /* init_proc */
 /* init_var */
 /* init_proc */
 /* init_var */
 /* init_proc */
 /* init_var */
 /* init_proc */
 /* init_var */
 /* init_proc */
 /* init_var */
 /* init_proc */
 /* init_var */
 /* init_proc */
 /* init_var */
 /* init_proc */
 /* init_var */
 /* init_proc */
 /* init_var */
 /* init_proc */
 /* init_var */
 /* init_proc */
 /* init_var */
 /* init_proc */
 /* init_var */
 /* init_proc */
 /* init_var */
 /* init_proc */
 /* init_var */
 /* init_proc */
 /* init_var */
 /* init_proc */
 /* init_var */
 /* init_proc */
 /* init_var */
 /* init_proc */
 /* init_var */
 /* init_proc */
 /* init_var */
 /* init_proc */
 /* init_var */
 /* init_proc */
 /* init_var */
 /* init_proc */
 /* init_var */
 /* init_proc */
 /* init_var */
 /* init_proc */
 /* init_var */
 /* init_proc */
 /* init_var */
 /* init_proc */
 /* init_var */
 /* init_proc */
 /* init_var */
 /* init_proc */
 /* init_var */
 /* init_proc */
 /* init_var */
 /* init_proc */
 /* init_var */
 /* init_proc */
 /* init_var */
 /* init_proc */
 /* init_var */
 /* init_proc */
 /* init_var */
 /* init_proc */
 /* init_var */
 /* init_proc */
 /* init_var */
 /* init_proc */
 /* init_var */
 /* init_proc */
 /* init_var */
 /* init_proc */
 /* init_var */
 /* init_proc */
 /* init_var */
 /* init_proc */
 /* init_var */
 /* init_proc */
 /* init_var */
 /* init_proc */
 /* init_var */
 /* init_proc */
 /* init_var */
 /* init_proc */
 /* init_var */
 /* init_proc */
 /* init_var */
 /* init_proc */
 /* init_var */
 /* init_proc */
 /* init_var */
 /* init_proc */
 /* init_var */
 /* init_proc */
 /* init_var */
 /* init_proc */
 /* init_var */
 /* init_proc */
 /* init_var */
 /* init_proc */
 /* init_var */
 /* init_proc */
 /* init_var */
 /* init_proc */
 /* init_var */
 /* init_proc */
 /* init_var */
 /* init_proc */
 /* init_var */
 /* init_proc */
 /* init_var */
 /* init_proc */
 /* init_var */
 /* init_proc */
 /* init_var */
 /* init_proc */
 /* init_var */
 /* init_proc */
 /* init_var */
 /* init_proc */
 /* init_var */
 /* init_proc */
 /* init_var */
 /* init_proc */
 /* init_var */
 /* init_proc */
 /* init_var */
 /* init_proc */
 /* init_var */
 /* init_proc */
 /* init_var */
 /* init_proc */
 /* init_var */
 /* init_proc */
 /* init_var */
 /* init_proc */
 /* init_var */
 /* init_proc */
 /* init_var */
 /* init_proc */
 /* init_var */
 /* init_proc */
 /* init_var */
 /* init_proc */
 /* init_var */
 /* init_proc */
 /* init_var */
 /* init_proc */
 /* init_var */
 /* init_proc */
 /* init_var */
 /* init_proc */
 /* init_var */
 /* init_proc */
 /* init_var */
 /* init_proc */
 /* init_var */
 /* init_proc */
 /* init_var */
 /* init_proc */
 /* init_var */
 /* init_proc */
 /* init_var */
 /* init_proc */
 /* init_var */
 /* init_proc */
 /* init_var */
 /* init_proc */
 /* init_var */
 /* init_proc */
 /* init_var */
 /* init_proc */
 /* init_var */
 /* init_proc */
 /* init_var */
 /* init_proc */
 /* init_var */
 /* init_proc */
 /* init_var */
 /* init_proc */
 /* init_var */
 /* init_proc */
 /* init_var */
 /* init_proc */
 /* init_var */
 /* init_proc */
 /* init_var */
 /* init_proc */
 /* init_var */
 /* init_proc */
 /* init_var */
 /* init_proc */
 /* init_var */
 /* init_proc */
 /* init_var */
 /* init_proc */
 /* init_var */
 /* init_proc */
 /* init_var */
 /* init_proc */
 /* init_var */
 /* init_proc */
 /* init_var */
 /* init_proc */
 /* init_var */
 /* init_proc */
 /* init_var */
 /* init_proc */
 /* init_var */
 /* init_proc */
 /* init_var */
 /* init_proc */
 /* init_var */
 /* init_proc */
 /* init_var */
 /* init_proc */
 /* init_var */
 /* init_proc */
 /* init_var */
 /* init_proc */
 /* init_var */
 /* init_proc */
 /* init_var */
 /* init_proc */
 /* init_var */
 /* init_proc */
 /* init_var */
 /* init_proc */
 /* init_var */
 /* init_proc */
 /* init_var */
 /* init_proc */
 /* init_var */
 /* init_proc */
 /* init_var */
 /* init_proc */
 /* init_var */
 /* init_proc */
 /* init_var */
 /* init_proc */
 /* init_var */
 /* init_proc */
 /* init_var */
 /* init_proc */
 /* init_var */
 /* init_proc */
 /* init_var */
 /* init_proc */
 /* init_var */
 /* init_proc */
 /* init_var */
 /* init_proc */
 /* init_var */
 /* init_proc */
 /* init_var */
 /* init_proc */
 /* init_var */
 /* init_proc */
 /* init_var */
 /* init_proc */
 /* init_var */
 /* init_proc */
 /* init_var */
 /* init_proc */
 /* init_var */
 /* init_proc */
 /* init_var */
 /* init_proc */
 /* init_var */
 /* init_proc */
 /* init_var */
 /* init_proc */
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
struct return_parameter_convert_m_5_L_6_t{UINT8 L_297[27];
char L_298[1];
UINT8 L_299[7];
char L_300[1];
UINT8 L_301[8];
char L_302[1];
UINT8 L_303[9];
char L_304[1];
UINT8 L_305[9];
char L_306[1];
UINT8 L_307[7];
char L_308[1];
UINT8 L_309[9];
char L_310[1];
UINT8 L_311[7];
char L_312[1];
UINT8 L_313[9];
char L_314[1];
UINT8 L_315[8];
char L_316[1];
UINT8 L_317[9];
char L_318[1];
UINT8 L_319[8];
char L_320[1];
UINT8 L_321[9];
char L_322[1];
UINT8 L_323[8];
char L_324[1];
UINT8 L_325[9];
char L_326[1];
UINT8 L_327[10];
char L_328[1];
UINT8 L_329[10];
char L_330[1];
UINT8 L_331[8];
char L_332[1];
UINT8 L_333[10];
char L_334[1];
UINT8 L_335[8];
char L_336[1];
UINT8 L_337[10];
char L_338[1];
UINT8 L_339[9];
char L_340[1];
UINT8 L_341[10];
char L_342[1];
UINT8 L_343[9];
char L_344[1];
UINT8 L_345[10];
char L_346[1];
UINT8 L_347[9];
char L_348[1];
UINT8 L_349[10];
char L_350[1];
UINT8 L_351[11];
char L_352[1];
UINT8 L_353[11];
char L_354[1];
UINT8 L_355[9];
char L_356[1];
UINT8 L_357[11];
char L_358[1];
UINT8 L_359[9];
char L_360[1];
UINT8 L_361[11];
char L_362[1];
UINT8 L_363[10];
char L_364[1];
UINT8 L_365[11];
char L_366[1];
UINT8 L_367[10];
char L_368[1];
UINT8 L_369[11];
char L_370[1];
UINT8 L_371[9];
char L_372[1];
UINT8 L_373[10];
char L_374[1];
UINT8 L_375[11];
char L_376[1];
UINT8 L_377[11];
char L_378[1];
UINT8 L_379[9];
char L_380[1];
UINT8 L_381[11];
char L_382[1];
UINT8 L_383[9];
char L_384[1];
UINT8 L_385[11];
char L_387[1];
UINT8 L_388[10];
char L_389[1];
UINT8 L_390[11];
char L_391[1];
UINT8 L_392[10];
char L_393[1];
UINT8 L_394[11];
char L_395[1];
UINT8 L_396[7];
char L_397[1];
UINT8 L_398[8];
char L_399[1];
UINT8 L_400[9];
char L_401[1];
UINT8 L_402[9];
char L_403[1];
UINT8 L_404[7];
char L_405[1];
UINT8 L_406[9];
char L_407[1];
UINT8 L_408[7];
char L_409[1];
UINT8 L_410[9];
char L_411[1];
UINT8 L_412[8];
char L_413[1];
UINT8 L_414[9];
char L_415[1];
UINT8 L_416[8];
char L_417[1];
UINT8 L_418[9];
char L_419[1];
UINT8 L_420[9];
char L_421[1];
UINT8 L_422[10];
char L_423[1];
UINT8 L_424[11];
char L_425[1];
UINT8 L_426[11];
char L_427[1];
UINT8 L_428[9];
char L_429[1];
UINT8 L_430[11];
char L_431[1];
UINT8 L_432[9];
char L_433[1];
UINT8 L_434[11];
char L_435[1];
UINT8 L_436[10];
char L_437[1];
UINT8 L_438[11];
char L_439[1];
UINT8 L_440[10];
char L_441[1];
UINT8 L_442[11];
char L_443[1];
UINT8 L_444[7];
char L_445[1];
UINT8 L_446[8];
char L_447[1];
UINT8 L_448[9];
char L_449[1];
UINT8 L_450[9];
char L_451[1];
UINT8 L_452[7];
char L_453[1];
UINT8 L_454[9];
char L_455[1];
UINT8 L_456[7];
char L_457[1];
UINT8 L_458[9];
char L_459[1];
UINT8 L_460[8];
char L_461[1];
UINT8 L_462[9];
char L_463[1];
UINT8 L_464[8];
char L_465[1];
UINT8 L_466[9];
char L_467[1];
UINT8 L_468[9];
char L_469[1];
UINT8 L_470[10];
char L_471[1];
UINT8 L_472[11];
char L_473[1];
UINT8 L_474[11];
char L_475[1];
UINT8 L_476[9];
char L_477[1];
UINT8 L_478[11];
char L_479[1];
UINT8 L_480[9];
char L_481[1];
UINT8 L_482[11];
char L_483[1];
UINT8 L_484[10];
char L_485[1];
UINT8 L_486[11];
char L_487[1];
UINT8 L_488[10];
char L_489[1];
UINT8 L_490[11];
char L_491[1];
UINT8 L_492[8];
char L_493[1];
UINT8 L_494[9];
char L_495[1];
UINT8 L_496[10];
char L_497[1];
UINT8 L_498[10];
char L_499[1];
UINT8 L_500[8];
char L_501[1];
UINT8 L_502[10];
char L_503[1];
UINT8 L_504[8];
char L_505[1];
UINT8 L_506[10];
char L_507[1];
UINT8 L_508[9];
char L_509[1];
UINT8 L_510[10];
char L_511[1];
UINT8 L_512[9];
char L_513[1];
UINT8 L_514[10];
char L_515[1];
UINT8 L_516[9];
char L_517[1];
UINT8 L_518[10];
char L_519[1];
UINT8 L_520[11];
char L_521[1];
UINT8 L_522[11];
char L_523[1];
UINT8 L_524[9];
char L_525[1];
UINT8 L_526[11];
char L_527[1];
UINT8 L_528[9];
char L_529[1];
UINT8 L_530[11];
char L_531[1];
UINT8 L_532[10];
char L_533[1];
UINT8 L_534[11];
char L_535[1];
UINT8 L_536[10];
char L_537[1];
UINT8 L_538[11];
char L_539[1];
UINT8 L_540[8];
char L_541[1];
UINT8 L_542[9];
char L_543[1];
UINT8 L_544[10];
char L_545[1];
UINT8 L_546[10];
char L_547[1];
UINT8 L_548[8];
char L_549[1];
UINT8 L_550[10];
char L_551[1];
UINT8 L_552[8];
char L_553[1];
UINT8 L_554[10];
char L_555[1];
UINT8 L_556[9];
char L_557[1];
UINT8 L_558[10];
char L_559[1];
UINT8 L_560[9];
char L_561[1];
UINT8 L_562[10];
char L_563[1];
UINT8 L_564[9];
char L_565[1];
UINT8 L_566[10];
char L_567[1];
UINT8 L_568[11];
char L_569[1];
UINT8 L_570[11];
char L_571[1];
UINT8 L_572[9];
char L_573[1];
UINT8 L_574[11];
char L_575[1];
UINT8 L_576[9];
char L_577[1];
UINT8 L_578[11];
char L_579[1];
UINT8 L_580[10];
char L_581[1];
UINT8 L_582[11];
char L_583[1];
UINT8 L_584[10];
char L_585[1];
UINT8 L_586[11];
char L_587[5];
ADDRESS L_588[290];
char L_589[8];
UINT8 L_590[27];
char L_591[5];
};
static  const return_parameter_convert_m_5_L_6_t return_parameter_convert_m_5_L_6={{'r','e','t','u','r','n','_','p','a','r','a','m','e','t','e','r','_','c','o','n','v','e','r','t','_','M','3'},{0 /* 1 */ ,},{'r','e','t','_','L','_','L'},{0 /* 1 */ ,},{'r','e','t','_','L','_','u','8'},{0 /* 1 */ ,},{'r','e','t','_','L','_','u','3','2'},{0 /* 1 */ ,},{'r','e','t','_','L','_','i','1','6'},{0 /* 1 */ ,},{'r','e','t','_','L','_','C'},{0 /* 1 */ ,},{'r','e','t','_','L','_','i','6','4'},{0 /* 1 */ ,},{'r','e','t','_','L','_','I'},{0 /* 1 */ ,},{'r','e','t','_','L','_','u','1','6'},{0 /* 1 */ ,},{'r','e','t','_','L','_','L','C'},{0 /* 1 */ ,},{'r','e','t','_','L','_','i','3','2'},{0 /* 1 */ ,},{'r','e','t','_','L','_','i','8'},{0 /* 1 */ ,},{'r','e','t','_','L','_','u','6','4'},{0 /* 1 */ ,},{'r','e','t','_','u','8','_','L'},{0 /* 1 */ ,},{'r','e','t','_','u','8','_','u','8'},{0 /* 1 */ ,},{'r','e','t','_','u','8','_','u','3','2'},{0 /* 1 */ ,},{'r','e','t','_','u','8','_','i','1','6'},{0 /* 1 */ 
,},{'r','e','t','_','u','8','_','C'},{0 /* 1 */ ,},{'r','e','t','_','u','8','_','i','6','4'},{0 /* 1 */ ,},{'r','e','t','_','u','8','_','I'},{0 /* 1 */ ,},{'r','e','t','_','u','8','_','u','1','6'},{0 /* 1 */ ,},{'r','e','t','_','u','8','_','L','C'},{0 /* 1 */ ,},{'r','e','t','_','u','8','_','i','3','2'},{0 /* 1 */ ,},{'r','e','t','_','u','8','_','i','8'},{0 /* 1 */ ,},{'r','e','t','_','u','8','_','u','6','4'},{0 /* 1 */ ,},{'r','e','t','_','u','3','2','_','L'},{0 /* 1 */ ,},{'r','e','t','_','u','3','2','_','u','8'},{0 /* 1 */ ,},{'r','e','t','_','u','3','2','_','u','3','2'},{0 /* 1 */ ,},{'r','e','t','_','u','3','2','_','i','1','6'},{0 /* 1 */ ,},{'r','e','t','_','u','3','2','_','C'},{0 /* 1 */ ,},{'r','e','t','_','u','3','2','_','i','6','4'},{0 /* 1 */ ,},{'r','e','t','_','u','3','2','_','I'},{0 /* 1 */ ,},{'r','e','t','_','u','3','2','_','u','1','6'},{0 /* 1 */ ,},{'r','e','t','_','u','3','2','_','L','C'},{0 /* 1 */ ,},{'r','e','t','_','u','3','2','_','i','3','2'},{0 /* 1 */ ,},{'r',
'e','t','_','u','3','2','_','i','8'},{0 /* 1 */ ,},{'r','e','t','_','u','3','2','_','u','6','4'},{0 /* 1 */ ,},{'r','e','t','_','i','1','6','_','L'},{0 /* 1 */ ,},{'r','e','t','_','i','1','6','_','u','8'},{0 /* 1 */ ,},{'r','e','t','_','i','1','6','_','u','3','2'},{0 /* 1 */ ,},{'r','e','t','_','i','1','6','_','i','1','6'},{0 /* 1 */ ,},{'r','e','t','_','i','1','6','_','C'},{0 /* 1 */ ,},{'r','e','t','_','i','1','6','_','i','6','4'},{0 /* 1 */ ,},{'r','e','t','_','i','1','6','_','I'},{0 /* 1 */ ,},{'r','e','t','_','i','1','6','_','u','1','6'},{0 /* 1 */ ,},{'r','e','t','_','i','1','6','_','L','C'},{0 /* 1 */ ,},{'r','e','t','_','i','1','6','_','i','3','2'},{0 /* 1 */ ,},{'r','e','t','_','i','1','6','_','i','8'},{0 /* 1 */ ,},{'r','e','t','_','i','1','6','_','u','6','4'},{0 /* 1 */ ,},{'r','e','t','_','C','_','L'},{0 /* 1 */ ,},{'r','e','t','_','C','_','u','8'},{0 /* 1 */ ,},{'r','e','t','_','C','_','u','3','2'},{0 /* 1 */ ,},{'r','e','t','_','C','_','i','1','6'},{0 /* 1 */ ,},{'r','e',
't','_','C','_','C'},{0 /* 1 */ ,},{'r','e','t','_','C','_','i','6','4'},{0 /* 1 */ ,},{'r','e','t','_','C','_','I'},{0 /* 1 */ ,},{'r','e','t','_','C','_','u','1','6'},{0 /* 1 */ ,},{'r','e','t','_','C','_','L','C'},{0 /* 1 */ ,},{'r','e','t','_','C','_','i','3','2'},{0 /* 1 */ ,},{'r','e','t','_','C','_','i','8'},{0 /* 1 */ ,},{'r','e','t','_','C','_','u','6','4'},{0 /* 1 */ ,},{'r','e','t','_','i','6','4','_','L'},{0 /* 1 */ ,},{'r','e','t','_','i','6','4','_','u','8'},{0 /* 1 */ ,},{'r','e','t','_','i','6','4','_','u','3','2'},{0 /* 1 */ ,},{'r','e','t','_','i','6','4','_','i','1','6'},{0 /* 1 */ ,},{'r','e','t','_','i','6','4','_','C'},{0 /* 1 */ ,},{'r','e','t','_','i','6','4','_','i','6','4'},{0 /* 1 */ ,},{'r','e','t','_','i','6','4','_','I'},{0 /* 1 */ ,},{'r','e','t','_','i','6','4','_','u','1','6'},{0 /* 1 */ ,},{'r','e','t','_','i','6','4','_','L','C'},{0 /* 1 */ ,},{'r','e','t','_','i','6','4','_','i','3','2'},{0 /* 1 */ ,},{'r','e','t','_','i','6','4','_','i','8'},{0 /* 1 */ 
,},{'r','e','t','_','i','6','4','_','u','6','4'},{0 /* 1 */ ,},{'r','e','t','_','I','_','L'},{0 /* 1 */ ,},{'r','e','t','_','I','_','u','8'},{0 /* 1 */ ,},{'r','e','t','_','I','_','u','3','2'},{0 /* 1 */ ,},{'r','e','t','_','I','_','i','1','6'},{0 /* 1 */ ,},{'r','e','t','_','I','_','C'},{0 /* 1 */ ,},{'r','e','t','_','I','_','i','6','4'},{0 /* 1 */ ,},{'r','e','t','_','I','_','I'},{0 /* 1 */ ,},{'r','e','t','_','I','_','u','1','6'},{0 /* 1 */ ,},{'r','e','t','_','I','_','L','C'},{0 /* 1 */ ,},{'r','e','t','_','I','_','i','3','2'},{0 /* 1 */ ,},{'r','e','t','_','I','_','i','8'},{0 /* 1 */ ,},{'r','e','t','_','I','_','u','6','4'},{0 /* 1 */ ,},{'r','e','t','_','u','1','6','_','L'},{0 /* 1 */ ,},{'r','e','t','_','u','1','6','_','u','8'},{0 /* 1 */ ,},{'r','e','t','_','u','1','6','_','u','3','2'},{0 /* 1 */ ,},{'r','e','t','_','u','1','6','_','i','1','6'},{0 /* 1 */ ,},{'r','e','t','_','u','1','6','_','C'},{0 /* 1 */ ,},{'r','e','t','_','u','1','6','_','i','6','4'},{0 /* 1 */ ,},{'r','e',
't','_','u','1','6','_','I'},{0 /* 1 */ ,},{'r','e','t','_','u','1','6','_','u','1','6'},{0 /* 1 */ ,},{'r','e','t','_','u','1','6','_','L','C'},{0 /* 1 */ ,},{'r','e','t','_','u','1','6','_','i','3','2'},{0 /* 1 */ ,},{'r','e','t','_','u','1','6','_','i','8'},{0 /* 1 */ ,},{'r','e','t','_','u','1','6','_','u','6','4'},{0 /* 1 */ ,},{'r','e','t','_','L','C','_','L'},{0 /* 1 */ ,},{'r','e','t','_','L','C','_','u','8'},{0 /* 1 */ ,},{'r','e','t','_','L','C','_','u','3','2'},{0 /* 1 */ ,},{'r','e','t','_','L','C','_','i','1','6'},{0 /* 1 */ ,},{'r','e','t','_','L','C','_','C'},{0 /* 1 */ ,},{'r','e','t','_','L','C','_','i','6','4'},{0 /* 1 */ ,},{'r','e','t','_','L','C','_','I'},{0 /* 1 */ ,},{'r','e','t','_','L','C','_','u','1','6'},{0 /* 1 */ ,},{'r','e','t','_','L','C','_','L','C'},{0 /* 1 */ ,},{'r','e','t','_','L','C','_','i','3','2'},{0 /* 1 */ ,},{'r','e','t','_','L','C','_','i','8'},{0 /* 1 */ ,},{'r','e','t','_','L','C','_','u','6','4'},{0 /* 1 */ ,},{'r','e','t','_','i','3','2',
'_','L'},{0 /* 1 */ ,},{'r','e','t','_','i','3','2','_','u','8'},{0 /* 1 */ ,},{'r','e','t','_','i','3','2','_','u','3','2'},{0 /* 1 */ ,},{'r','e','t','_','i','3','2','_','i','1','6'},{0 /* 1 */ ,},{'r','e','t','_','i','3','2','_','C'},{0 /* 1 */ ,},{'r','e','t','_','i','3','2','_','i','6','4'},{0 /* 1 */ ,},{'r','e','t','_','i','3','2','_','I'},{0 /* 1 */ ,},{'r','e','t','_','i','3','2','_','u','1','6'},{0 /* 1 */ ,},{'r','e','t','_','i','3','2','_','L','C'},{0 /* 1 */ ,},{'r','e','t','_','i','3','2','_','i','3','2'},{0 /* 1 */ ,},{'r','e','t','_','i','3','2','_','i','8'},{0 /* 1 */ ,},{'r','e','t','_','i','3','2','_','u','6','4'},{0 /* 1 */ ,},{'r','e','t','_','i','8','_','L'},{0 /* 1 */ ,},{'r','e','t','_','i','8','_','u','8'},{0 /* 1 */ ,},{'r','e','t','_','i','8','_','u','3','2'},{0 /* 1 */ ,},{'r','e','t','_','i','8','_','i','1','6'},{0 /* 1 */ ,},{'r','e','t','_','i','8','_','C'},{0 /* 1 */ ,},{'r','e','t','_','i','8','_','i','6','4'},{0 /* 1 */ ,},{'r','e','t','_','i','8','_',
'I'},{0 /* 1 */ ,},{'r','e','t','_','i','8','_','u','1','6'},{0 /* 1 */ ,},{'r','e','t','_','i','8','_','L','C'},{0 /* 1 */ ,},{'r','e','t','_','i','8','_','i','3','2'},{0 /* 1 */ ,},{'r','e','t','_','i','8','_','i','8'},{0 /* 1 */ ,},{'r','e','t','_','i','8','_','u','6','4'},{0 /* 1 */ ,},{'r','e','t','_','u','6','4','_','L'},{0 /* 1 */ ,},{'r','e','t','_','u','6','4','_','u','8'},{0 /* 1 */ ,},{'r','e','t','_','u','6','4','_','u','3','2'},{0 /* 1 */ ,},{'r','e','t','_','u','6','4','_','i','1','6'},{0 /* 1 */ ,},{'r','e','t','_','u','6','4','_','C'},{0 /* 1 */ ,},{'r','e','t','_','u','6','4','_','i','6','4'},{0 /* 1 */ ,},{'r','e','t','_','u','6','4','_','I'},{0 /* 1 */ ,},{'r','e','t','_','u','6','4','_','u','1','6'},{0 /* 1 */ ,},{'r','e','t','_','u','6','4','_','L','C'},{0 /* 1 */ ,},{'r','e','t','_','u','6','4','_','i','3','2'},{0 /* 1 */ ,},{'r','e','t','_','u','6','4','_','i','8'},{0 /* 1 */ ,},{'r','e','t','_','u','6','4','_','u','6','4'},{0 /* 1 */ ,0 /* 2 */ ,0 /* 3 */ ,0 /* 4 */ 
,0 /* 5 */ ,},{(ADDRESS)&return_parameter_convert_M3,(char*)&return_parameter_convert_m_5_L_6,(ADDRESS)&return_parameter_convert__ret_L_L,28+(char*)&return_parameter_convert_m_5_L_6,(ADDRESS)&return_parameter_convert__ret_L_u8,36+(char*)&return_parameter_convert_m_5_L_6,(ADDRESS)&return_parameter_convert__ret_L_u32,45+(char*)&return_parameter_convert_m_5_L_6,(ADDRESS)&return_parameter_convert__ret_L_i16,55+(char*)&return_parameter_convert_m_5_L_6,(ADDRESS)&return_parameter_convert__ret_L_C,65+(char*)&return_parameter_convert_m_5_L_6,(ADDRESS)&return_parameter_convert__ret_L_i64,73+(char*)&return_parameter_convert_m_5_L_6,(ADDRESS)&return_parameter_convert__ret_L_I,83+(char*)&return_parameter_convert_m_5_L_6,(ADDRESS)&return_parameter_convert__ret_L_u16,91+(char*)&return_parameter_convert_m_5_L_6,(ADDRESS)&return_parameter_convert__ret_L_LC,101+(char*)&return_parameter_convert_m_5_L_6,(ADDRESS)&return_parameter_convert__ret_L_i32,110+(char*)&return_parameter_convert_m_5_L_6,(ADDRESS)&return_parameter_convert__ret_L_i8
,120+(char*)&return_parameter_convert_m_5_L_6,(ADDRESS)&return_parameter_convert__ret_L_u64,129+(char*)&return_parameter_convert_m_5_L_6,(ADDRESS)&return_parameter_convert__ret_u8_L,139+(char*)&return_parameter_convert_m_5_L_6,(ADDRESS)&return_parameter_convert__ret_u8_u8,148+(char*)&return_parameter_convert_m_5_L_6,(ADDRESS)&return_parameter_convert__ret_u8_u32,158+(char*)&return_parameter_convert_m_5_L_6,(ADDRESS)&return_parameter_convert__ret_u8_i16,169+(char*)&return_parameter_convert_m_5_L_6,(ADDRESS)&return_parameter_convert__ret_u8_C,180+(char*)&return_parameter_convert_m_5_L_6,(ADDRESS)&return_parameter_convert__ret_u8_i64,189+(char*)&return_parameter_convert_m_5_L_6,(ADDRESS)&return_parameter_convert__ret_u8_I,200+(char*)&return_parameter_convert_m_5_L_6,(ADDRESS)&return_parameter_convert__ret_u8_u16,209+(char*)&return_parameter_convert_m_5_L_6,(ADDRESS)&return_parameter_convert__ret_u8_LC,220+(char*)&return_parameter_convert_m_5_L_6,(ADDRESS)&return_parameter_convert__ret_u8_i32
,230+(char*)&return_parameter_convert_m_5_L_6,(ADDRESS)&return_parameter_convert__ret_u8_i8,241+(char*)&return_parameter_convert_m_5_L_6,(ADDRESS)&return_parameter_convert__ret_u8_u64,251+(char*)&return_parameter_convert_m_5_L_6,(ADDRESS)&return_parameter_convert__ret_u32_L,262+(char*)&return_parameter_convert_m_5_L_6,(ADDRESS)&return_parameter_convert__ret_u32_u8,272+(char*)&return_parameter_convert_m_5_L_6,(ADDRESS)&return_parameter_convert__ret_u32_u32,283+(char*)&return_parameter_convert_m_5_L_6,(ADDRESS)&return_parameter_convert__ret_u32_i16,295+(char*)&return_parameter_convert_m_5_L_6,(ADDRESS)&return_parameter_convert__ret_u32_C,307+(char*)&return_parameter_convert_m_5_L_6,(ADDRESS)&return_parameter_convert__ret_u32_i64,317+(char*)&return_parameter_convert_m_5_L_6,(ADDRESS)&return_parameter_convert__ret_u32_I,329+(char*)&return_parameter_convert_m_5_L_6,(ADDRESS)&return_parameter_convert__ret_u32_u16,339+(char*)&return_parameter_convert_m_5_L_6,(ADDRESS)&return_parameter_convert__ret_u32_LC
,351+(char*)&return_parameter_convert_m_5_L_6,(ADDRESS)&return_parameter_convert__ret_u32_i32,362+(char*)&return_parameter_convert_m_5_L_6,(ADDRESS)&return_parameter_convert__ret_u32_i8,374+(char*)&return_parameter_convert_m_5_L_6,(ADDRESS)&return_parameter_convert__ret_u32_u64,385+(char*)&return_parameter_convert_m_5_L_6,(ADDRESS)&return_parameter_convert__ret_i16_L,397+(char*)&return_parameter_convert_m_5_L_6,(ADDRESS)&return_parameter_convert__ret_i16_u8,407+(char*)&return_parameter_convert_m_5_L_6,(ADDRESS)&return_parameter_convert__ret_i16_u32,418+(char*)&return_parameter_convert_m_5_L_6,(ADDRESS)&return_parameter_convert__ret_i16_i16,430+(char*)&return_parameter_convert_m_5_L_6,(ADDRESS)&return_parameter_convert__ret_i16_C,442+(char*)&return_parameter_convert_m_5_L_6,(ADDRESS)&return_parameter_convert__ret_i16_i64,452+(char*)&return_parameter_convert_m_5_L_6,(ADDRESS)&return_parameter_convert__ret_i16_I,464+(char*)&return_parameter_convert_m_5_L_6,(ADDRESS)&return_parameter_convert__ret_i16_u16
,474+(char*)&return_parameter_convert_m_5_L_6,(ADDRESS)&return_parameter_convert__ret_i16_LC,486+(char*)&return_parameter_convert_m_5_L_6,(ADDRESS)&return_parameter_convert__ret_i16_i32,497+(char*)&return_parameter_convert_m_5_L_6,(ADDRESS)&return_parameter_convert__ret_i16_i8,509+(char*)&return_parameter_convert_m_5_L_6,(ADDRESS)&return_parameter_convert__ret_i16_u64,520+(char*)&return_parameter_convert_m_5_L_6,(ADDRESS)&return_parameter_convert__ret_C_L,532+(char*)&return_parameter_convert_m_5_L_6,(ADDRESS)&return_parameter_convert__ret_C_u8,540+(char*)&return_parameter_convert_m_5_L_6,(ADDRESS)&return_parameter_convert__ret_C_u32,549+(char*)&return_parameter_convert_m_5_L_6,(ADDRESS)&return_parameter_convert__ret_C_i16,559+(char*)&return_parameter_convert_m_5_L_6,(ADDRESS)&return_parameter_convert__ret_C_C,569+(char*)&return_parameter_convert_m_5_L_6,(ADDRESS)&return_parameter_convert__ret_C_i64,577+(char*)&return_parameter_convert_m_5_L_6,(ADDRESS)&return_parameter_convert__ret_C_I
,587+(char*)&return_parameter_convert_m_5_L_6,(ADDRESS)&return_parameter_convert__ret_C_u16,595+(char*)&return_parameter_convert_m_5_L_6,(ADDRESS)&return_parameter_convert__ret_C_LC,605+(char*)&return_parameter_convert_m_5_L_6,(ADDRESS)&return_parameter_convert__ret_C_i32,614+(char*)&return_parameter_convert_m_5_L_6,(ADDRESS)&return_parameter_convert__ret_C_i8,624+(char*)&return_parameter_convert_m_5_L_6,(ADDRESS)&return_parameter_convert__ret_C_u64,633+(char*)&return_parameter_convert_m_5_L_6,(ADDRESS)&return_parameter_convert__ret_i64_L,643+(char*)&return_parameter_convert_m_5_L_6,(ADDRESS)&return_parameter_convert__ret_i64_u8,653+(char*)&return_parameter_convert_m_5_L_6,(ADDRESS)&return_parameter_convert__ret_i64_u32,664+(char*)&return_parameter_convert_m_5_L_6,(ADDRESS)&return_parameter_convert__ret_i64_i16,676+(char*)&return_parameter_convert_m_5_L_6,(ADDRESS)&return_parameter_convert__ret_i64_C,688+(char*)&return_parameter_convert_m_5_L_6,(ADDRESS)&return_parameter_convert__ret_i64_i64
,698+(char*)&return_parameter_convert_m_5_L_6,(ADDRESS)&return_parameter_convert__ret_i64_I,710+(char*)&return_parameter_convert_m_5_L_6,(ADDRESS)&return_parameter_convert__ret_i64_u16,720+(char*)&return_parameter_convert_m_5_L_6,(ADDRESS)&return_parameter_convert__ret_i64_LC,732+(char*)&return_parameter_convert_m_5_L_6,(ADDRESS)&return_parameter_convert__ret_i64_i32,743+(char*)&return_parameter_convert_m_5_L_6,(ADDRESS)&return_parameter_convert__ret_i64_i8,755+(char*)&return_parameter_convert_m_5_L_6,(ADDRESS)&return_parameter_convert__ret_i64_u64,766+(char*)&return_parameter_convert_m_5_L_6,(ADDRESS)&return_parameter_convert__ret_I_L,778+(char*)&return_parameter_convert_m_5_L_6,(ADDRESS)&return_parameter_convert__ret_I_u8,786+(char*)&return_parameter_convert_m_5_L_6,(ADDRESS)&return_parameter_convert__ret_I_u32,795+(char*)&return_parameter_convert_m_5_L_6,(ADDRESS)&return_parameter_convert__ret_I_i16,805+(char*)&return_parameter_convert_m_5_L_6,(ADDRESS)&return_parameter_convert__ret_I_C
,815+(char*)&return_parameter_convert_m_5_L_6,(ADDRESS)&return_parameter_convert__ret_I_i64,823+(char*)&return_parameter_convert_m_5_L_6,(ADDRESS)&return_parameter_convert__ret_I_I,833+(char*)&return_parameter_convert_m_5_L_6,(ADDRESS)&return_parameter_convert__ret_I_u16,841+(char*)&return_parameter_convert_m_5_L_6,(ADDRESS)&return_parameter_convert__ret_I_LC,851+(char*)&return_parameter_convert_m_5_L_6,(ADDRESS)&return_parameter_convert__ret_I_i32,860+(char*)&return_parameter_convert_m_5_L_6,(ADDRESS)&return_parameter_convert__ret_I_i8,870+(char*)&return_parameter_convert_m_5_L_6,(ADDRESS)&return_parameter_convert__ret_I_u64,879+(char*)&return_parameter_convert_m_5_L_6,(ADDRESS)&return_parameter_convert__ret_u16_L,889+(char*)&return_parameter_convert_m_5_L_6,(ADDRESS)&return_parameter_convert__ret_u16_u8,899+(char*)&return_parameter_convert_m_5_L_6,(ADDRESS)&return_parameter_convert__ret_u16_u32,910+(char*)&return_parameter_convert_m_5_L_6,(ADDRESS)&return_parameter_convert__ret_u16_i16
,922+(char*)&return_parameter_convert_m_5_L_6,(ADDRESS)&return_parameter_convert__ret_u16_C,934+(char*)&return_parameter_convert_m_5_L_6,(ADDRESS)&return_parameter_convert__ret_u16_i64,944+(char*)&return_parameter_convert_m_5_L_6,(ADDRESS)&return_parameter_convert__ret_u16_I,956+(char*)&return_parameter_convert_m_5_L_6,(ADDRESS)&return_parameter_convert__ret_u16_u16,966+(char*)&return_parameter_convert_m_5_L_6,(ADDRESS)&return_parameter_convert__ret_u16_LC,978+(char*)&return_parameter_convert_m_5_L_6,(ADDRESS)&return_parameter_convert__ret_u16_i32,989+(char*)&return_parameter_convert_m_5_L_6,(ADDRESS)&return_parameter_convert__ret_u16_i8,1001+(char*)&return_parameter_convert_m_5_L_6,(ADDRESS)&return_parameter_convert__ret_u16_u64,1012+(char*)&return_parameter_convert_m_5_L_6,(ADDRESS)&return_parameter_convert__ret_LC_L,1024+(char*)&return_parameter_convert_m_5_L_6,(ADDRESS)&return_parameter_convert__ret_LC_u8,1033+(char*)&return_parameter_convert_m_5_L_6,(ADDRESS)&return_parameter_convert__ret_LC_u32
,1043+(char*)&return_parameter_convert_m_5_L_6,(ADDRESS)&return_parameter_convert__ret_LC_i16,1054+(char*)&return_parameter_convert_m_5_L_6,(ADDRESS)&return_parameter_convert__ret_LC_C,1065+(char*)&return_parameter_convert_m_5_L_6,(ADDRESS)&return_parameter_convert__ret_LC_i64,1074+(char*)&return_parameter_convert_m_5_L_6,(ADDRESS)&return_parameter_convert__ret_LC_I,1085+(char*)&return_parameter_convert_m_5_L_6,(ADDRESS)&return_parameter_convert__ret_LC_u16,1094+(char*)&return_parameter_convert_m_5_L_6,(ADDRESS)&return_parameter_convert__ret_LC_LC,1105+(char*)&return_parameter_convert_m_5_L_6,(ADDRESS)&return_parameter_convert__ret_LC_i32,1115+(char*)&return_parameter_convert_m_5_L_6,(ADDRESS)&return_parameter_convert__ret_LC_i8,1126+(char*)&return_parameter_convert_m_5_L_6,(ADDRESS)&return_parameter_convert__ret_LC_u64,1136+(char*)&return_parameter_convert_m_5_L_6,(ADDRESS)&return_parameter_convert__ret_i32_L,1147+(char*)&return_parameter_convert_m_5_L_6,(ADDRESS)&return_parameter_convert__ret_i32_u8
,1157+(char*)&return_parameter_convert_m_5_L_6,(ADDRESS)&return_parameter_convert__ret_i32_u32,1168+(char*)&return_parameter_convert_m_5_L_6,(ADDRESS)&return_parameter_convert__ret_i32_i16,1180+(char*)&return_parameter_convert_m_5_L_6,(ADDRESS)&return_parameter_convert__ret_i32_C,1192+(char*)&return_parameter_convert_m_5_L_6,(ADDRESS)&return_parameter_convert__ret_i32_i64,1202+(char*)&return_parameter_convert_m_5_L_6,(ADDRESS)&return_parameter_convert__ret_i32_I,1214+(char*)&return_parameter_convert_m_5_L_6,(ADDRESS)&return_parameter_convert__ret_i32_u16,1224+(char*)&return_parameter_convert_m_5_L_6,(ADDRESS)&return_parameter_convert__ret_i32_LC,1236+(char*)&return_parameter_convert_m_5_L_6,(ADDRESS)&return_parameter_convert__ret_i32_i32,1247+(char*)&return_parameter_convert_m_5_L_6,(ADDRESS)&return_parameter_convert__ret_i32_i8,1259+(char*)&return_parameter_convert_m_5_L_6,(ADDRESS)&return_parameter_convert__ret_i32_u64,1270+(char*)&return_parameter_convert_m_5_L_6,(ADDRESS)&return_parameter_convert__ret_i8_L
,1282+(char*)&return_parameter_convert_m_5_L_6,(ADDRESS)&return_parameter_convert__ret_i8_u8,1291+(char*)&return_parameter_convert_m_5_L_6,(ADDRESS)&return_parameter_convert__ret_i8_u32,1301+(char*)&return_parameter_convert_m_5_L_6,(ADDRESS)&return_parameter_convert__ret_i8_i16,1312+(char*)&return_parameter_convert_m_5_L_6,(ADDRESS)&return_parameter_convert__ret_i8_C,1323+(char*)&return_parameter_convert_m_5_L_6,(ADDRESS)&return_parameter_convert__ret_i8_i64,1332+(char*)&return_parameter_convert_m_5_L_6,(ADDRESS)&return_parameter_convert__ret_i8_I,1343+(char*)&return_parameter_convert_m_5_L_6,(ADDRESS)&return_parameter_convert__ret_i8_u16,1352+(char*)&return_parameter_convert_m_5_L_6,(ADDRESS)&return_parameter_convert__ret_i8_LC,1363+(char*)&return_parameter_convert_m_5_L_6,(ADDRESS)&return_parameter_convert__ret_i8_i32,1373+(char*)&return_parameter_convert_m_5_L_6,(ADDRESS)&return_parameter_convert__ret_i8_i8,1384+(char*)&return_parameter_convert_m_5_L_6,(ADDRESS)&return_parameter_convert__ret_i8_u64
,1394+(char*)&return_parameter_convert_m_5_L_6,(ADDRESS)&return_parameter_convert__ret_u64_L,1405+(char*)&return_parameter_convert_m_5_L_6,(ADDRESS)&return_parameter_convert__ret_u64_u8,1415+(char*)&return_parameter_convert_m_5_L_6,(ADDRESS)&return_parameter_convert__ret_u64_u32,1426+(char*)&return_parameter_convert_m_5_L_6,(ADDRESS)&return_parameter_convert__ret_u64_i16,1438+(char*)&return_parameter_convert_m_5_L_6,(ADDRESS)&return_parameter_convert__ret_u64_C,1450+(char*)&return_parameter_convert_m_5_L_6,(ADDRESS)&return_parameter_convert__ret_u64_i64,1460+(char*)&return_parameter_convert_m_5_L_6,(ADDRESS)&return_parameter_convert__ret_u64_I,1472+(char*)&return_parameter_convert_m_5_L_6,(ADDRESS)&return_parameter_convert__ret_u64_u16,1482+(char*)&return_parameter_convert_m_5_L_6,(ADDRESS)&return_parameter_convert__ret_u64_LC,1494+(char*)&return_parameter_convert_m_5_L_6,(ADDRESS)&return_parameter_convert__ret_u64_i32,1505+(char*)&return_parameter_convert_m_5_L_6,(ADDRESS)&return_parameter_convert__ret_u64_i8
,1517+(char*)&return_parameter_convert_m_5_L_6,(ADDRESS)&return_parameter_convert__ret_u64_u64,1528+(char*)&return_parameter_convert_m_5_L_6},{0 /* 1 */ ,0 /* 2 */ ,0 /* 3 */ ,0 /* 4 */ ,0 /* 5 */ ,0 /* 6 */ ,0 /* 7 */ ,0 /* 8 */ ,},{'r','e','t','u','r','n','_','p','a','r','a','m','e','t','e','r','_','c','o','n','v','e','r','t','.','m','3'},{0 /* 1 */ ,0 /* 2 */ ,0 /* 3 */ ,0 /* 4 */ ,0 /* 5 */ ,}};
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
struct return_parameter_convert_m_M_return_parameter_convert_L_7_t{ADDRESS L_592[1];
char L_593[32];
ADDRESS L_594[1];
char L_595[24];
ADDRESS L_596[1];
char L_597[8];
ADDRESS L_598[1];
INT64 L_599[1];
INT8 L_600[1];
char L_601[7];
INT64 L_602[1];
double L_603[1];
INT32 L_604[1];
char L_605[4];
INT64 L_606[1];
INT16 L_607[1];
char L_608[6];
INT64 L_609[2];
float L_610[1];
INT16 L_611[1];
char L_612[2];
INT64 L_613[1];
INT32 L_614[1];
INT8 L_615[1];
char L_616[3];
INT64 L_617[1];
char L_618[24];
ADDRESS L_619[2];
char L_620[8];
ADDRESS L_621[2];
char L_622[8];
ADDRESS L_623[2];
char L_624[8];
ADDRESS L_625[2];
char L_626[8];
ADDRESS L_627[1];
char L_628[8];
};
static return_parameter_convert_m_M_return_parameter_convert_L_7_t return_parameter_convert_m_M_return_parameter_convert_L_7={{3872+(char*)&return_parameter_convert_m_5_L_6},{0 /* 1 */ ,0 /* 2 */ ,0 /* 3 */ ,0 /* 4 */ ,0 /* 5 */ ,0 /* 6 */ ,0 /* 7 */ ,0 /* 8 */ ,0 /* 9 */ ,0 /* 10 */ ,0 /* 11 */ ,0 /* 12 */ ,0 /* 13 */ ,0 /* 14 */ ,0 /* 15 */ ,0 /* 16 */ ,0 /* 17 */ ,0 /* 18 */ ,0 /* 19 */ ,0 /* 20 */ ,0 /* 21 */ ,0 /* 22 */ ,0 /* 23 */ ,0 /* 24 */ ,0 /* 25 */ ,0 /* 26 */ ,0 /* 27 */ ,0 /* 28 */ ,0 /* 29 */ ,0 /* 30 */ ,0 /* 31 */ ,0 /* 32 */ ,},{1544+(char*)&return_parameter_convert_m_5_L_6},{0 /* 1 */ ,0 /* 2 */ ,0 /* 3 */ ,0 /* 4 */ ,0 /* 5 */ ,0 /* 6 */ ,0 /* 7 */ ,0 /* 8 */ ,0 /* 9 */ ,0 /* 10 */ ,0 /* 11 */ ,0 /* 12 */ ,0 /* 13 */ ,0 /* 14 */ ,0 /* 15 */ ,0 /* 16 */ ,0 /* 17 */ ,0 /* 18 */ ,0 /* 19 */ ,0 /* 20 */ ,0 /* 21 */ ,0 /* 22 */ ,0 /* 23 */ ,0 /* 24 */ ,},{216+(char*)&return_parameter_convert_m_M_return_parameter_convert_L_7},{0 /* 1 */ ,0 /* 2 */ ,0 /* 3 */ ,0 /* 4 */ ,0 /* 5 */ 
,0 /* 6 */ ,0 /* 7 */ ,0 /* 8 */ ,},{(ADDRESS)&return_parameter_convert_M3},{INT64_(3)},{((INT8)113)},{0 /* 1 */ ,0 /* 2 */ ,0 /* 3 */ ,0 /* 4 */ ,0 /* 5 */ ,0 /* 6 */ ,0 /* 7 */ ,},{INT64_(114)},{1.15116000000000000e2},{117},{0 /* 1 */ ,0 /* 2 */ ,0 /* 3 */ ,0 /* 4 */ ,},{INT64_(118)},{((INT16)119)},{0 /* 1 */ ,0 /* 2 */ ,0 /* 3 */ ,0 /* 4 */ ,0 /* 5 */ ,0 /* 6 */ ,},{INT64_(120),INT64_(121)},{1.2212300109863E2F},{((INT16)124)},{0 /* 1 */ ,0 /* 2 */ ,},{INT64_(125)},{126},{((INT8)127)},{0 /* 1 */ ,0 /* 2 */ ,0 /* 3 */ ,},{INT64_(128)},{0 /* 1 */ ,0 /* 2 */ ,0 /* 3 */ ,0 /* 4 */ ,0 /* 5 */ ,0 /* 6 */ ,0 /* 7 */ ,0 /* 8 */ ,0 /* 9 */ ,0 /* 10 */ ,0 /* 11 */ ,0 /* 12 */ ,0 /* 13 */ ,0 /* 14 */ ,0 /* 15 */ ,0 /* 16 */ ,0 /* 17 */ ,0 /* 18 */ ,0 /* 19 */ ,0 /* 20 */ ,0 /* 21 */ ,0 /* 22 */ ,0 /* 23 */ ,0 /* 24 */ ,},{(ADDRESS)&return_parameter_convert_I3,240+(char*)&return_parameter_convert_m_M_return_parameter_convert_L_7},{0 /* 1 */ ,0 /* 2 */ ,0 /* 3 */ ,0 /* 4 */ ,0 /* 5 */ ,0 /* 6 */ 
,0 /* 7 */ ,0 /* 8 */ ,},{(ADDRESS)&Long_I3,264+(char*)&return_parameter_convert_m_M_return_parameter_convert_L_7},{0 /* 1 */ ,0 /* 2 */ ,0 /* 3 */ ,0 /* 4 */ ,0 /* 5 */ ,0 /* 6 */ ,0 /* 7 */ ,0 /* 8 */ ,},{(ADDRESS)&Word_I3,288+(char*)&return_parameter_convert_m_M_return_parameter_convert_L_7},{0 /* 1 */ ,0 /* 2 */ ,0 /* 3 */ ,0 /* 4 */ ,0 /* 5 */ ,0 /* 6 */ ,0 /* 7 */ ,0 /* 8 */ ,},{(ADDRESS)&Cstdint_I3,312+(char*)&return_parameter_convert_m_M_return_parameter_convert_L_7},{0 /* 1 */ ,0 /* 2 */ ,0 /* 3 */ ,0 /* 4 */ ,0 /* 5 */ ,0 /* 6 */ ,0 /* 7 */ ,0 /* 8 */ ,},{(ADDRESS)&RTHooks_I3},{0 /* 1 */ ,0 /* 2 */ ,0 /* 3 */ ,0 /* 4 */ ,0 /* 5 */ ,0 /* 6 */ ,0 /* 7 */ ,0 /* 8 */ ,}};
 /* end: segments/globals */
 /* begin: mark used */
 /* end: mark used */
 /* set_source_file */
 /* set_source_line */
#line 1 "return_parameter_convert.m3"
 /* module global constants */
#line 1 "return_parameter_convert.m3"
 /* module global data */
#line 1 "return_parameter_convert.m3"
 /* set_source_line */
#line 1 "return_parameter_convert.m3"
#line 198 "return_parameter_convert.m3"
 /* ret_u64_u64 */
#line 198 "return_parameter_convert.m3"
 /* set_source_line */
#line 198 "return_parameter_convert.m3"
#line 54 "return_parameter_convert.m3"
 /* begin_procedure */
#line 54 "return_parameter_convert.m3"
struct return_parameter_convert__ret_u64_u64_Frame_t {
#line 54 "return_parameter_convert.m3"
ADDRESS _unused;
#line 54 "return_parameter_convert.m3"
};
#line 54 "return_parameter_convert.m3"
LONGINT
__cdecl
return_parameter_convert__ret_u64_u64(
   /* Param_Type1 */ return_parameter_convert__UINT64 a_L_10)
{
#line 54 "return_parameter_convert.m3"
return_parameter_convert__ret_u64_u64_Frame_t _frame;
#line 54 "return_parameter_convert.m3"
_frame._unused=(ADDRESS)&_frame;
#line 54 "return_parameter_convert.m3"
 /* load */
#line 54 "return_parameter_convert.m3"
 /* exit_proc */
#line 54 "return_parameter_convert.m3"
return a_L_10;
#line 54 "return_parameter_convert.m3"
 /* end_procedure */
#line 54 "return_parameter_convert.m3"
} /* ret_u64_i8 */
#line 54 "return_parameter_convert.m3"
 /* set_source_line */
#line 54 "return_parameter_convert.m3"
#line 55 "return_parameter_convert.m3"
 /* begin_procedure */
#line 55 "return_parameter_convert.m3"
struct return_parameter_convert__ret_u64_i8_Frame_t {
#line 55 "return_parameter_convert.m3"
ADDRESS _unused;
#line 55 "return_parameter_convert.m3"
};
#line 55 "return_parameter_convert.m3"
LONGINT
__cdecl
return_parameter_convert__ret_u64_i8(
   /* Param_Type1 */ return_parameter_convert__UINT64 a_L_12)
{
#line 55 "return_parameter_convert.m3"
return_parameter_convert__ret_u64_i8_Frame_t _frame;
#line 55 "return_parameter_convert.m3"
_frame._unused=(ADDRESS)&_frame;
#line 55 "return_parameter_convert.m3"
 /* load */
#line 55 "return_parameter_convert.m3"
 /* exit_proc */
#line 55 "return_parameter_convert.m3"
return a_L_12;
#line 55 "return_parameter_convert.m3"
 /* end_procedure */
#line 55 "return_parameter_convert.m3"
} /* ret_u64_i32 */
#line 55 "return_parameter_convert.m3"
 /* set_source_line */
#line 55 "return_parameter_convert.m3"
#line 56 "return_parameter_convert.m3"
 /* begin_procedure */
#line 56 "return_parameter_convert.m3"
struct return_parameter_convert__ret_u64_i32_Frame_t {
#line 56 "return_parameter_convert.m3"
ADDRESS _unused;
#line 56 "return_parameter_convert.m3"
};
#line 56 "return_parameter_convert.m3"
LONGINT
__cdecl
return_parameter_convert__ret_u64_i32(
   /* Param_Type1 */ return_parameter_convert__UINT64 a_L_14)
{
#line 56 "return_parameter_convert.m3"
return_parameter_convert__ret_u64_i32_Frame_t _frame;
#line 56 "return_parameter_convert.m3"
_frame._unused=(ADDRESS)&_frame;
#line 56 "return_parameter_convert.m3"
 /* load */
#line 56 "return_parameter_convert.m3"
 /* exit_proc */
#line 56 "return_parameter_convert.m3"
return a_L_14;
#line 56 "return_parameter_convert.m3"
 /* end_procedure */
#line 56 "return_parameter_convert.m3"
} /* ret_u64_LC */
#line 56 "return_parameter_convert.m3"
 /* set_source_line */
#line 56 "return_parameter_convert.m3"
#line 57 "return_parameter_convert.m3"
 /* begin_procedure */
#line 57 "return_parameter_convert.m3"
struct return_parameter_convert__ret_u64_LC_Frame_t {
#line 57 "return_parameter_convert.m3"
ADDRESS _unused;
#line 57 "return_parameter_convert.m3"
};
#line 57 "return_parameter_convert.m3"
LONGINT
__cdecl
return_parameter_convert__ret_u64_LC(
   /* Param_Type1 */ return_parameter_convert__UINT64 a_L_16)
{
#line 57 "return_parameter_convert.m3"
return_parameter_convert__ret_u64_LC_Frame_t _frame;
#line 57 "return_parameter_convert.m3"
_frame._unused=(ADDRESS)&_frame;
#line 57 "return_parameter_convert.m3"
 /* load */
#line 57 "return_parameter_convert.m3"
 /* exit_proc */
#line 57 "return_parameter_convert.m3"
return a_L_16;
#line 57 "return_parameter_convert.m3"
 /* end_procedure */
#line 57 "return_parameter_convert.m3"
} /* ret_u64_u16 */
#line 57 "return_parameter_convert.m3"
 /* set_source_line */
#line 57 "return_parameter_convert.m3"
#line 58 "return_parameter_convert.m3"
 /* begin_procedure */
#line 58 "return_parameter_convert.m3"
struct return_parameter_convert__ret_u64_u16_Frame_t {
#line 58 "return_parameter_convert.m3"
ADDRESS _unused;
#line 58 "return_parameter_convert.m3"
};
#line 58 "return_parameter_convert.m3"
LONGINT
__cdecl
return_parameter_convert__ret_u64_u16(
   /* Param_Type1 */ return_parameter_convert__UINT64 a_L_18)
{
#line 58 "return_parameter_convert.m3"
return_parameter_convert__ret_u64_u16_Frame_t _frame;
#line 58 "return_parameter_convert.m3"
_frame._unused=(ADDRESS)&_frame;
#line 58 "return_parameter_convert.m3"
 /* load */
#line 58 "return_parameter_convert.m3"
 /* exit_proc */
#line 58 "return_parameter_convert.m3"
return a_L_18;
#line 58 "return_parameter_convert.m3"
 /* end_procedure */
#line 58 "return_parameter_convert.m3"
} /* ret_u64_I */
#line 58 "return_parameter_convert.m3"
 /* set_source_line */
#line 58 "return_parameter_convert.m3"
#line 59 "return_parameter_convert.m3"
 /* begin_procedure */
#line 59 "return_parameter_convert.m3"
struct return_parameter_convert__ret_u64_I_Frame_t {
#line 59 "return_parameter_convert.m3"
ADDRESS _unused;
#line 59 "return_parameter_convert.m3"
};
#line 59 "return_parameter_convert.m3"
LONGINT
__cdecl
return_parameter_convert__ret_u64_I(
   /* Param_Type1 */ return_parameter_convert__UINT64 a_L_20)
{
#line 59 "return_parameter_convert.m3"
return_parameter_convert__ret_u64_I_Frame_t _frame;
#line 59 "return_parameter_convert.m3"
_frame._unused=(ADDRESS)&_frame;
#line 59 "return_parameter_convert.m3"
 /* load */
#line 59 "return_parameter_convert.m3"
 /* exit_proc */
#line 59 "return_parameter_convert.m3"
return a_L_20;
#line 59 "return_parameter_convert.m3"
 /* end_procedure */
#line 59 "return_parameter_convert.m3"
} /* ret_u64_i64 */
#line 59 "return_parameter_convert.m3"
 /* set_source_line */
#line 59 "return_parameter_convert.m3"
#line 60 "return_parameter_convert.m3"
 /* begin_procedure */
#line 60 "return_parameter_convert.m3"
struct return_parameter_convert__ret_u64_i64_Frame_t {
#line 60 "return_parameter_convert.m3"
ADDRESS _unused;
#line 60 "return_parameter_convert.m3"
};
#line 60 "return_parameter_convert.m3"
LONGINT
__cdecl
return_parameter_convert__ret_u64_i64(
   /* Param_Type1 */ return_parameter_convert__UINT64 a_L_22)
{
#line 60 "return_parameter_convert.m3"
return_parameter_convert__ret_u64_i64_Frame_t _frame;
#line 60 "return_parameter_convert.m3"
_frame._unused=(ADDRESS)&_frame;
#line 60 "return_parameter_convert.m3"
 /* load */
#line 60 "return_parameter_convert.m3"
 /* exit_proc */
#line 60 "return_parameter_convert.m3"
return a_L_22;
#line 60 "return_parameter_convert.m3"
 /* end_procedure */
#line 60 "return_parameter_convert.m3"
} /* ret_u64_C */
#line 60 "return_parameter_convert.m3"
 /* set_source_line */
#line 60 "return_parameter_convert.m3"
#line 61 "return_parameter_convert.m3"
 /* begin_procedure */
#line 61 "return_parameter_convert.m3"
struct return_parameter_convert__ret_u64_C_Frame_t {
#line 61 "return_parameter_convert.m3"
ADDRESS _unused;
#line 61 "return_parameter_convert.m3"
};
#line 61 "return_parameter_convert.m3"
LONGINT
__cdecl
return_parameter_convert__ret_u64_C(
   /* Param_Type1 */ return_parameter_convert__UINT64 a_L_24)
{
#line 61 "return_parameter_convert.m3"
return_parameter_convert__ret_u64_C_Frame_t _frame;
#line 61 "return_parameter_convert.m3"
_frame._unused=(ADDRESS)&_frame;
#line 61 "return_parameter_convert.m3"
 /* load */
#line 61 "return_parameter_convert.m3"
 /* exit_proc */
#line 61 "return_parameter_convert.m3"
return a_L_24;
#line 61 "return_parameter_convert.m3"
 /* end_procedure */
#line 61 "return_parameter_convert.m3"
} /* ret_u64_i16 */
#line 61 "return_parameter_convert.m3"
 /* set_source_line */
#line 61 "return_parameter_convert.m3"
#line 62 "return_parameter_convert.m3"
 /* begin_procedure */
#line 62 "return_parameter_convert.m3"
struct return_parameter_convert__ret_u64_i16_Frame_t {
#line 62 "return_parameter_convert.m3"
ADDRESS _unused;
#line 62 "return_parameter_convert.m3"
};
#line 62 "return_parameter_convert.m3"
LONGINT
__cdecl
return_parameter_convert__ret_u64_i16(
   /* Param_Type1 */ return_parameter_convert__UINT64 a_L_26)
{
#line 62 "return_parameter_convert.m3"
return_parameter_convert__ret_u64_i16_Frame_t _frame;
#line 62 "return_parameter_convert.m3"
_frame._unused=(ADDRESS)&_frame;
#line 62 "return_parameter_convert.m3"
 /* load */
#line 62 "return_parameter_convert.m3"
 /* exit_proc */
#line 62 "return_parameter_convert.m3"
return a_L_26;
#line 62 "return_parameter_convert.m3"
 /* end_procedure */
#line 62 "return_parameter_convert.m3"
} /* ret_u64_u32 */
#line 62 "return_parameter_convert.m3"
 /* set_source_line */
#line 62 "return_parameter_convert.m3"
#line 63 "return_parameter_convert.m3"
 /* begin_procedure */
#line 63 "return_parameter_convert.m3"
struct return_parameter_convert__ret_u64_u32_Frame_t {
#line 63 "return_parameter_convert.m3"
ADDRESS _unused;
#line 63 "return_parameter_convert.m3"
};
#line 63 "return_parameter_convert.m3"
LONGINT
__cdecl
return_parameter_convert__ret_u64_u32(
   /* Param_Type1 */ return_parameter_convert__UINT64 a_L_28)
{
#line 63 "return_parameter_convert.m3"
return_parameter_convert__ret_u64_u32_Frame_t _frame;
#line 63 "return_parameter_convert.m3"
_frame._unused=(ADDRESS)&_frame;
#line 63 "return_parameter_convert.m3"
 /* load */
#line 63 "return_parameter_convert.m3"
 /* exit_proc */
#line 63 "return_parameter_convert.m3"
return a_L_28;
#line 63 "return_parameter_convert.m3"
 /* end_procedure */
#line 63 "return_parameter_convert.m3"
} /* ret_u64_u8 */
#line 63 "return_parameter_convert.m3"
 /* set_source_line */
#line 63 "return_parameter_convert.m3"
#line 64 "return_parameter_convert.m3"
 /* begin_procedure */
#line 64 "return_parameter_convert.m3"
struct return_parameter_convert__ret_u64_u8_Frame_t {
#line 64 "return_parameter_convert.m3"
ADDRESS _unused;
#line 64 "return_parameter_convert.m3"
};
#line 64 "return_parameter_convert.m3"
LONGINT
__cdecl
return_parameter_convert__ret_u64_u8(
   /* Param_Type1 */ return_parameter_convert__UINT64 a_L_30)
{
#line 64 "return_parameter_convert.m3"
return_parameter_convert__ret_u64_u8_Frame_t _frame;
#line 64 "return_parameter_convert.m3"
_frame._unused=(ADDRESS)&_frame;
#line 64 "return_parameter_convert.m3"
 /* load */
#line 64 "return_parameter_convert.m3"
 /* exit_proc */
#line 64 "return_parameter_convert.m3"
return a_L_30;
#line 64 "return_parameter_convert.m3"
 /* end_procedure */
#line 64 "return_parameter_convert.m3"
} /* ret_u64_L */
#line 64 "return_parameter_convert.m3"
 /* set_source_line */
#line 64 "return_parameter_convert.m3"
#line 65 "return_parameter_convert.m3"
 /* begin_procedure */
#line 65 "return_parameter_convert.m3"
struct return_parameter_convert__ret_u64_L_Frame_t {
#line 65 "return_parameter_convert.m3"
ADDRESS _unused;
#line 65 "return_parameter_convert.m3"
};
#line 65 "return_parameter_convert.m3"
LONGINT
__cdecl
return_parameter_convert__ret_u64_L(
   /* Param_Type1 */ return_parameter_convert__UINT64 a_L_32)
{
#line 65 "return_parameter_convert.m3"
return_parameter_convert__ret_u64_L_Frame_t _frame;
#line 65 "return_parameter_convert.m3"
_frame._unused=(ADDRESS)&_frame;
#line 65 "return_parameter_convert.m3"
 /* load */
#line 65 "return_parameter_convert.m3"
 /* exit_proc */
#line 65 "return_parameter_convert.m3"
return a_L_32;
#line 65 "return_parameter_convert.m3"
 /* end_procedure */
#line 65 "return_parameter_convert.m3"
} /* ret_i8_u64 */
#line 65 "return_parameter_convert.m3"
 /* set_source_line */
#line 65 "return_parameter_convert.m3"
#line 66 "return_parameter_convert.m3"
 /* begin_procedure */
#line 66 "return_parameter_convert.m3"
struct return_parameter_convert__ret_i8_u64_Frame_t {
#line 66 "return_parameter_convert.m3"
ADDRESS _unused;
#line 66 "return_parameter_convert.m3"
};
#line 66 "return_parameter_convert.m3"
LONGINT
__cdecl
return_parameter_convert__ret_i8_u64(
   /* Param_Type1 */ return_parameter_convert__INT8 a_L_34)
{
#line 66 "return_parameter_convert.m3"
return_parameter_convert__ret_i8_u64_Frame_t _frame;
#line 66 "return_parameter_convert.m3"
_frame._unused=(ADDRESS)&_frame;
#line 66 "return_parameter_convert.m3"
 /* load */
#line 66 "return_parameter_convert.m3"
 /* loophole */
#line 66 "return_parameter_convert.m3"
 /* exit_proc */
#line 66 "return_parameter_convert.m3"
return (INT64)((INT64)(a_L_34));
#line 66 "return_parameter_convert.m3"
 /* end_procedure */
#line 66 "return_parameter_convert.m3"
} /* ret_i8_i8 */
#line 66 "return_parameter_convert.m3"
 /* set_source_line */
#line 66 "return_parameter_convert.m3"
#line 67 "return_parameter_convert.m3"
 /* begin_procedure */
#line 67 "return_parameter_convert.m3"
struct return_parameter_convert__ret_i8_i8_Frame_t {
#line 67 "return_parameter_convert.m3"
ADDRESS _unused;
#line 67 "return_parameter_convert.m3"
};
#line 67 "return_parameter_convert.m3"
INTEGER
__cdecl
return_parameter_convert__ret_i8_i8(
   /* Param_Type1 */ return_parameter_convert__INT8 a_L_36)
{
#line 67 "return_parameter_convert.m3"
return_parameter_convert__ret_i8_i8_Frame_t _frame;
#line 67 "return_parameter_convert.m3"
_frame._unused=(ADDRESS)&_frame;
#line 67 "return_parameter_convert.m3"
 /* load */
#line 67 "return_parameter_convert.m3"
 /* exit_proc */
#line 67 "return_parameter_convert.m3"
return ((INT64)(a_L_36));
#line 67 "return_parameter_convert.m3"
 /* end_procedure */
#line 67 "return_parameter_convert.m3"
} /* ret_i8_i32 */
#line 67 "return_parameter_convert.m3"
 /* set_source_line */
#line 67 "return_parameter_convert.m3"
#line 68 "return_parameter_convert.m3"
 /* begin_procedure */
#line 68 "return_parameter_convert.m3"
struct return_parameter_convert__ret_i8_i32_Frame_t {
#line 68 "return_parameter_convert.m3"
ADDRESS _unused;
#line 68 "return_parameter_convert.m3"
};
#line 68 "return_parameter_convert.m3"
INTEGER
__cdecl
return_parameter_convert__ret_i8_i32(
   /* Param_Type1 */ return_parameter_convert__INT8 a_L_38)
{
#line 68 "return_parameter_convert.m3"
return_parameter_convert__ret_i8_i32_Frame_t _frame;
#line 68 "return_parameter_convert.m3"
_frame._unused=(ADDRESS)&_frame;
#line 68 "return_parameter_convert.m3"
 /* load */
#line 68 "return_parameter_convert.m3"
 /* exit_proc */
#line 68 "return_parameter_convert.m3"
return ((INT64)(a_L_38));
#line 68 "return_parameter_convert.m3"
 /* end_procedure */
#line 68 "return_parameter_convert.m3"
} /* ret_i8_LC */
#line 68 "return_parameter_convert.m3"
 /* set_source_line */
#line 68 "return_parameter_convert.m3"
#line 69 "return_parameter_convert.m3"
 /* begin_procedure */
#line 69 "return_parameter_convert.m3"
struct return_parameter_convert__ret_i8_LC_Frame_t {
#line 69 "return_parameter_convert.m3"
ADDRESS _unused;
#line 69 "return_parameter_convert.m3"
};
#line 69 "return_parameter_convert.m3"
LONGINT
__cdecl
return_parameter_convert__ret_i8_LC(
   /* Param_Type1 */ return_parameter_convert__INT8 a_L_40)
{
#line 69 "return_parameter_convert.m3"
return_parameter_convert__ret_i8_LC_Frame_t _frame;
#line 69 "return_parameter_convert.m3"
_frame._unused=(ADDRESS)&_frame;
#line 69 "return_parameter_convert.m3"
 /* load */
#line 69 "return_parameter_convert.m3"
 /* loophole */
#line 69 "return_parameter_convert.m3"
 /* exit_proc */
#line 69 "return_parameter_convert.m3"
return (INT64)((INT64)(a_L_40));
#line 69 "return_parameter_convert.m3"
 /* end_procedure */
#line 69 "return_parameter_convert.m3"
} /* ret_i8_u16 */
#line 69 "return_parameter_convert.m3"
 /* set_source_line */
#line 69 "return_parameter_convert.m3"
#line 70 "return_parameter_convert.m3"
 /* begin_procedure */
#line 70 "return_parameter_convert.m3"
struct return_parameter_convert__ret_i8_u16_Frame_t {
#line 70 "return_parameter_convert.m3"
ADDRESS _unused;
#line 70 "return_parameter_convert.m3"
};
#line 70 "return_parameter_convert.m3"
INTEGER
__cdecl
return_parameter_convert__ret_i8_u16(
   /* Param_Type1 */ return_parameter_convert__INT8 a_L_42)
{
#line 70 "return_parameter_convert.m3"
return_parameter_convert__ret_i8_u16_Frame_t _frame;
#line 70 "return_parameter_convert.m3"
_frame._unused=(ADDRESS)&_frame;
#line 70 "return_parameter_convert.m3"
 /* load */
#line 70 "return_parameter_convert.m3"
 /* exit_proc */
#line 70 "return_parameter_convert.m3"
return ((INT64)(a_L_42));
#line 70 "return_parameter_convert.m3"
 /* end_procedure */
#line 70 "return_parameter_convert.m3"
} /* ret_i8_I */
#line 70 "return_parameter_convert.m3"
 /* set_source_line */
#line 70 "return_parameter_convert.m3"
#line 71 "return_parameter_convert.m3"
 /* begin_procedure */
#line 71 "return_parameter_convert.m3"
struct return_parameter_convert__ret_i8_I_Frame_t {
#line 71 "return_parameter_convert.m3"
ADDRESS _unused;
#line 71 "return_parameter_convert.m3"
};
#line 71 "return_parameter_convert.m3"
INTEGER
__cdecl
return_parameter_convert__ret_i8_I(
   /* Param_Type1 */ return_parameter_convert__INT8 a_L_44)
{
#line 71 "return_parameter_convert.m3"
return_parameter_convert__ret_i8_I_Frame_t _frame;
#line 71 "return_parameter_convert.m3"
_frame._unused=(ADDRESS)&_frame;
#line 71 "return_parameter_convert.m3"
 /* load */
#line 71 "return_parameter_convert.m3"
 /* exit_proc */
#line 71 "return_parameter_convert.m3"
return ((INT64)(a_L_44));
#line 71 "return_parameter_convert.m3"
 /* end_procedure */
#line 71 "return_parameter_convert.m3"
} /* ret_i8_i64 */
#line 71 "return_parameter_convert.m3"
 /* set_source_line */
#line 71 "return_parameter_convert.m3"
#line 72 "return_parameter_convert.m3"
 /* begin_procedure */
#line 72 "return_parameter_convert.m3"
struct return_parameter_convert__ret_i8_i64_Frame_t {
#line 72 "return_parameter_convert.m3"
ADDRESS _unused;
#line 72 "return_parameter_convert.m3"
};
#line 72 "return_parameter_convert.m3"
LONGINT
__cdecl
return_parameter_convert__ret_i8_i64(
   /* Param_Type1 */ return_parameter_convert__INT8 a_L_46)
{
#line 72 "return_parameter_convert.m3"
return_parameter_convert__ret_i8_i64_Frame_t _frame;
#line 72 "return_parameter_convert.m3"
_frame._unused=(ADDRESS)&_frame;
#line 72 "return_parameter_convert.m3"
 /* load */
#line 72 "return_parameter_convert.m3"
 /* loophole */
#line 72 "return_parameter_convert.m3"
 /* exit_proc */
#line 72 "return_parameter_convert.m3"
return (INT64)((INT64)(a_L_46));
#line 72 "return_parameter_convert.m3"
 /* end_procedure */
#line 72 "return_parameter_convert.m3"
} /* ret_i8_C */
#line 72 "return_parameter_convert.m3"
 /* set_source_line */
#line 72 "return_parameter_convert.m3"
#line 73 "return_parameter_convert.m3"
 /* begin_procedure */
#line 73 "return_parameter_convert.m3"
struct return_parameter_convert__ret_i8_C_Frame_t {
#line 73 "return_parameter_convert.m3"
ADDRESS _unused;
#line 73 "return_parameter_convert.m3"
};
#line 73 "return_parameter_convert.m3"
INTEGER
__cdecl
return_parameter_convert__ret_i8_C(
   /* Param_Type1 */ return_parameter_convert__INT8 a_L_48)
{
#line 73 "return_parameter_convert.m3"
return_parameter_convert__ret_i8_C_Frame_t _frame;
#line 73 "return_parameter_convert.m3"
_frame._unused=(ADDRESS)&_frame;
#line 73 "return_parameter_convert.m3"
 /* load */
#line 73 "return_parameter_convert.m3"
 /* exit_proc */
#line 73 "return_parameter_convert.m3"
return ((INT64)(a_L_48));
#line 73 "return_parameter_convert.m3"
 /* end_procedure */
#line 73 "return_parameter_convert.m3"
} /* ret_i8_i16 */
#line 73 "return_parameter_convert.m3"
 /* set_source_line */
#line 73 "return_parameter_convert.m3"
#line 74 "return_parameter_convert.m3"
 /* begin_procedure */
#line 74 "return_parameter_convert.m3"
struct return_parameter_convert__ret_i8_i16_Frame_t {
#line 74 "return_parameter_convert.m3"
ADDRESS _unused;
#line 74 "return_parameter_convert.m3"
};
#line 74 "return_parameter_convert.m3"
INTEGER
__cdecl
return_parameter_convert__ret_i8_i16(
   /* Param_Type1 */ return_parameter_convert__INT8 a_L_50)
{
#line 74 "return_parameter_convert.m3"
return_parameter_convert__ret_i8_i16_Frame_t _frame;
#line 74 "return_parameter_convert.m3"
_frame._unused=(ADDRESS)&_frame;
#line 74 "return_parameter_convert.m3"
 /* load */
#line 74 "return_parameter_convert.m3"
 /* exit_proc */
#line 74 "return_parameter_convert.m3"
return ((INT64)(a_L_50));
#line 74 "return_parameter_convert.m3"
 /* end_procedure */
#line 74 "return_parameter_convert.m3"
} /* ret_i8_u32 */
#line 74 "return_parameter_convert.m3"
 /* set_source_line */
#line 74 "return_parameter_convert.m3"
#line 75 "return_parameter_convert.m3"
 /* begin_procedure */
#line 75 "return_parameter_convert.m3"
struct return_parameter_convert__ret_i8_u32_Frame_t {
#line 75 "return_parameter_convert.m3"
ADDRESS _unused;
#line 75 "return_parameter_convert.m3"
};
#line 75 "return_parameter_convert.m3"
INTEGER
__cdecl
return_parameter_convert__ret_i8_u32(
   /* Param_Type1 */ return_parameter_convert__INT8 a_L_52)
{
#line 75 "return_parameter_convert.m3"
return_parameter_convert__ret_i8_u32_Frame_t _frame;
#line 75 "return_parameter_convert.m3"
_frame._unused=(ADDRESS)&_frame;
#line 75 "return_parameter_convert.m3"
 /* load */
#line 75 "return_parameter_convert.m3"
 /* exit_proc */
#line 75 "return_parameter_convert.m3"
return ((INT64)(a_L_52));
#line 75 "return_parameter_convert.m3"
 /* end_procedure */
#line 75 "return_parameter_convert.m3"
} /* ret_i8_u8 */
#line 75 "return_parameter_convert.m3"
 /* set_source_line */
#line 75 "return_parameter_convert.m3"
#line 76 "return_parameter_convert.m3"
 /* begin_procedure */
#line 76 "return_parameter_convert.m3"
struct return_parameter_convert__ret_i8_u8_Frame_t {
#line 76 "return_parameter_convert.m3"
ADDRESS _unused;
#line 76 "return_parameter_convert.m3"
};
#line 76 "return_parameter_convert.m3"
INTEGER
__cdecl
return_parameter_convert__ret_i8_u8(
   /* Param_Type1 */ return_parameter_convert__INT8 a_L_54)
{
#line 76 "return_parameter_convert.m3"
return_parameter_convert__ret_i8_u8_Frame_t _frame;
#line 76 "return_parameter_convert.m3"
_frame._unused=(ADDRESS)&_frame;
#line 76 "return_parameter_convert.m3"
 /* load */
#line 76 "return_parameter_convert.m3"
 /* exit_proc */
#line 76 "return_parameter_convert.m3"
return ((INT64)(a_L_54));
#line 76 "return_parameter_convert.m3"
 /* end_procedure */
#line 76 "return_parameter_convert.m3"
} /* ret_i8_L */
#line 76 "return_parameter_convert.m3"
 /* set_source_line */
#line 76 "return_parameter_convert.m3"
#line 77 "return_parameter_convert.m3"
 /* begin_procedure */
#line 77 "return_parameter_convert.m3"
struct return_parameter_convert__ret_i8_L_Frame_t {
#line 77 "return_parameter_convert.m3"
ADDRESS _unused;
#line 77 "return_parameter_convert.m3"
};
#line 77 "return_parameter_convert.m3"
LONGINT
__cdecl
return_parameter_convert__ret_i8_L(
   /* Param_Type1 */ return_parameter_convert__INT8 a_L_56)
{
#line 77 "return_parameter_convert.m3"
return_parameter_convert__ret_i8_L_Frame_t _frame;
#line 77 "return_parameter_convert.m3"
_frame._unused=(ADDRESS)&_frame;
#line 77 "return_parameter_convert.m3"
 /* load */
#line 77 "return_parameter_convert.m3"
 /* loophole */
#line 77 "return_parameter_convert.m3"
 /* exit_proc */
#line 77 "return_parameter_convert.m3"
return (INT64)((INT64)(a_L_56));
#line 77 "return_parameter_convert.m3"
 /* end_procedure */
#line 77 "return_parameter_convert.m3"
} /* ret_i32_u64 */
#line 77 "return_parameter_convert.m3"
 /* set_source_line */
#line 77 "return_parameter_convert.m3"
#line 78 "return_parameter_convert.m3"
 /* begin_procedure */
#line 78 "return_parameter_convert.m3"
struct return_parameter_convert__ret_i32_u64_Frame_t {
#line 78 "return_parameter_convert.m3"
ADDRESS _unused;
#line 78 "return_parameter_convert.m3"
};
#line 78 "return_parameter_convert.m3"
LONGINT
__cdecl
return_parameter_convert__ret_i32_u64(
   /* Param_Type1 */ return_parameter_convert__INT32 a_L_58)
{
#line 78 "return_parameter_convert.m3"
return_parameter_convert__ret_i32_u64_Frame_t _frame;
#line 78 "return_parameter_convert.m3"
_frame._unused=(ADDRESS)&_frame;
#line 78 "return_parameter_convert.m3"
 /* load */
#line 78 "return_parameter_convert.m3"
 /* loophole */
#line 78 "return_parameter_convert.m3"
 /* exit_proc */
#line 78 "return_parameter_convert.m3"
return (INT64)((INT64)(a_L_58));
#line 78 "return_parameter_convert.m3"
 /* end_procedure */
#line 78 "return_parameter_convert.m3"
} /* ret_i32_i8 */
#line 78 "return_parameter_convert.m3"
 /* set_source_line */
#line 78 "return_parameter_convert.m3"
#line 79 "return_parameter_convert.m3"
 /* begin_procedure */
#line 79 "return_parameter_convert.m3"
struct return_parameter_convert__ret_i32_i8_Frame_t {
#line 79 "return_parameter_convert.m3"
ADDRESS _unused;
#line 79 "return_parameter_convert.m3"
};
#line 79 "return_parameter_convert.m3"
INTEGER
__cdecl
return_parameter_convert__ret_i32_i8(
   /* Param_Type1 */ return_parameter_convert__INT32 a_L_60)
{
#line 79 "return_parameter_convert.m3"
return_parameter_convert__ret_i32_i8_Frame_t _frame;
#line 79 "return_parameter_convert.m3"
_frame._unused=(ADDRESS)&_frame;
#line 79 "return_parameter_convert.m3"
 /* load */
#line 79 "return_parameter_convert.m3"
 /* exit_proc */
#line 79 "return_parameter_convert.m3"
return ((INT64)(a_L_60));
#line 79 "return_parameter_convert.m3"
 /* end_procedure */
#line 79 "return_parameter_convert.m3"
} /* ret_i32_i32 */
#line 79 "return_parameter_convert.m3"
 /* set_source_line */
#line 79 "return_parameter_convert.m3"
#line 80 "return_parameter_convert.m3"
 /* begin_procedure */
#line 80 "return_parameter_convert.m3"
struct return_parameter_convert__ret_i32_i32_Frame_t {
#line 80 "return_parameter_convert.m3"
ADDRESS _unused;
#line 80 "return_parameter_convert.m3"
};
#line 80 "return_parameter_convert.m3"
INTEGER
__cdecl
return_parameter_convert__ret_i32_i32(
   /* Param_Type1 */ return_parameter_convert__INT32 a_L_62)
{
#line 80 "return_parameter_convert.m3"
return_parameter_convert__ret_i32_i32_Frame_t _frame;
#line 80 "return_parameter_convert.m3"
_frame._unused=(ADDRESS)&_frame;
#line 80 "return_parameter_convert.m3"
 /* load */
#line 80 "return_parameter_convert.m3"
 /* exit_proc */
#line 80 "return_parameter_convert.m3"
return ((INT64)(a_L_62));
#line 80 "return_parameter_convert.m3"
 /* end_procedure */
#line 80 "return_parameter_convert.m3"
} /* ret_i32_LC */
#line 80 "return_parameter_convert.m3"
 /* set_source_line */
#line 80 "return_parameter_convert.m3"
#line 81 "return_parameter_convert.m3"
 /* begin_procedure */
#line 81 "return_parameter_convert.m3"
struct return_parameter_convert__ret_i32_LC_Frame_t {
#line 81 "return_parameter_convert.m3"
ADDRESS _unused;
#line 81 "return_parameter_convert.m3"
};
#line 81 "return_parameter_convert.m3"
LONGINT
__cdecl
return_parameter_convert__ret_i32_LC(
   /* Param_Type1 */ return_parameter_convert__INT32 a_L_64)
{
#line 81 "return_parameter_convert.m3"
return_parameter_convert__ret_i32_LC_Frame_t _frame;
#line 81 "return_parameter_convert.m3"
_frame._unused=(ADDRESS)&_frame;
#line 81 "return_parameter_convert.m3"
 /* load */
#line 81 "return_parameter_convert.m3"
 /* loophole */
#line 81 "return_parameter_convert.m3"
 /* exit_proc */
#line 81 "return_parameter_convert.m3"
return (INT64)((INT64)(a_L_64));
#line 81 "return_parameter_convert.m3"
 /* end_procedure */
#line 81 "return_parameter_convert.m3"
} /* ret_i32_u16 */
#line 81 "return_parameter_convert.m3"
 /* set_source_line */
#line 81 "return_parameter_convert.m3"
#line 82 "return_parameter_convert.m3"
 /* begin_procedure */
#line 82 "return_parameter_convert.m3"
struct return_parameter_convert__ret_i32_u16_Frame_t {
#line 82 "return_parameter_convert.m3"
ADDRESS _unused;
#line 82 "return_parameter_convert.m3"
};
#line 82 "return_parameter_convert.m3"
INTEGER
__cdecl
return_parameter_convert__ret_i32_u16(
   /* Param_Type1 */ return_parameter_convert__INT32 a_L_66)
{
#line 82 "return_parameter_convert.m3"
return_parameter_convert__ret_i32_u16_Frame_t _frame;
#line 82 "return_parameter_convert.m3"
_frame._unused=(ADDRESS)&_frame;
#line 82 "return_parameter_convert.m3"
 /* load */
#line 82 "return_parameter_convert.m3"
 /* exit_proc */
#line 82 "return_parameter_convert.m3"
return ((INT64)(a_L_66));
#line 82 "return_parameter_convert.m3"
 /* end_procedure */
#line 82 "return_parameter_convert.m3"
} /* ret_i32_I */
#line 82 "return_parameter_convert.m3"
 /* set_source_line */
#line 82 "return_parameter_convert.m3"
#line 83 "return_parameter_convert.m3"
 /* begin_procedure */
#line 83 "return_parameter_convert.m3"
struct return_parameter_convert__ret_i32_I_Frame_t {
#line 83 "return_parameter_convert.m3"
ADDRESS _unused;
#line 83 "return_parameter_convert.m3"
};
#line 83 "return_parameter_convert.m3"
INTEGER
__cdecl
return_parameter_convert__ret_i32_I(
   /* Param_Type1 */ return_parameter_convert__INT32 a_L_68)
{
#line 83 "return_parameter_convert.m3"
return_parameter_convert__ret_i32_I_Frame_t _frame;
#line 83 "return_parameter_convert.m3"
_frame._unused=(ADDRESS)&_frame;
#line 83 "return_parameter_convert.m3"
 /* load */
#line 83 "return_parameter_convert.m3"
 /* exit_proc */
#line 83 "return_parameter_convert.m3"
return ((INT64)(a_L_68));
#line 83 "return_parameter_convert.m3"
 /* end_procedure */
#line 83 "return_parameter_convert.m3"
} /* ret_i32_i64 */
#line 83 "return_parameter_convert.m3"
 /* set_source_line */
#line 83 "return_parameter_convert.m3"
#line 84 "return_parameter_convert.m3"
 /* begin_procedure */
#line 84 "return_parameter_convert.m3"
struct return_parameter_convert__ret_i32_i64_Frame_t {
#line 84 "return_parameter_convert.m3"
ADDRESS _unused;
#line 84 "return_parameter_convert.m3"
};
#line 84 "return_parameter_convert.m3"
LONGINT
__cdecl
return_parameter_convert__ret_i32_i64(
   /* Param_Type1 */ return_parameter_convert__INT32 a_L_70)
{
#line 84 "return_parameter_convert.m3"
return_parameter_convert__ret_i32_i64_Frame_t _frame;
#line 84 "return_parameter_convert.m3"
_frame._unused=(ADDRESS)&_frame;
#line 84 "return_parameter_convert.m3"
 /* load */
#line 84 "return_parameter_convert.m3"
 /* loophole */
#line 84 "return_parameter_convert.m3"
 /* exit_proc */
#line 84 "return_parameter_convert.m3"
return (INT64)((INT64)(a_L_70));
#line 84 "return_parameter_convert.m3"
 /* end_procedure */
#line 84 "return_parameter_convert.m3"
} /* ret_i32_C */
#line 84 "return_parameter_convert.m3"
 /* set_source_line */
#line 84 "return_parameter_convert.m3"
#line 85 "return_parameter_convert.m3"
 /* begin_procedure */
#line 85 "return_parameter_convert.m3"
struct return_parameter_convert__ret_i32_C_Frame_t {
#line 85 "return_parameter_convert.m3"
ADDRESS _unused;
#line 85 "return_parameter_convert.m3"
};
#line 85 "return_parameter_convert.m3"
INTEGER
__cdecl
return_parameter_convert__ret_i32_C(
   /* Param_Type1 */ return_parameter_convert__INT32 a_L_72)
{
#line 85 "return_parameter_convert.m3"
return_parameter_convert__ret_i32_C_Frame_t _frame;
#line 85 "return_parameter_convert.m3"
_frame._unused=(ADDRESS)&_frame;
#line 85 "return_parameter_convert.m3"
 /* load */
#line 85 "return_parameter_convert.m3"
 /* exit_proc */
#line 85 "return_parameter_convert.m3"
return ((INT64)(a_L_72));
#line 85 "return_parameter_convert.m3"
 /* end_procedure */
#line 85 "return_parameter_convert.m3"
} /* ret_i32_i16 */
#line 85 "return_parameter_convert.m3"
 /* set_source_line */
#line 85 "return_parameter_convert.m3"
#line 86 "return_parameter_convert.m3"
 /* begin_procedure */
#line 86 "return_parameter_convert.m3"
struct return_parameter_convert__ret_i32_i16_Frame_t {
#line 86 "return_parameter_convert.m3"
ADDRESS _unused;
#line 86 "return_parameter_convert.m3"
};
#line 86 "return_parameter_convert.m3"
INTEGER
__cdecl
return_parameter_convert__ret_i32_i16(
   /* Param_Type1 */ return_parameter_convert__INT32 a_L_74)
{
#line 86 "return_parameter_convert.m3"
return_parameter_convert__ret_i32_i16_Frame_t _frame;
#line 86 "return_parameter_convert.m3"
_frame._unused=(ADDRESS)&_frame;
#line 86 "return_parameter_convert.m3"
 /* load */
#line 86 "return_parameter_convert.m3"
 /* exit_proc */
#line 86 "return_parameter_convert.m3"
return ((INT64)(a_L_74));
#line 86 "return_parameter_convert.m3"
 /* end_procedure */
#line 86 "return_parameter_convert.m3"
} /* ret_i32_u32 */
#line 86 "return_parameter_convert.m3"
 /* set_source_line */
#line 86 "return_parameter_convert.m3"
#line 87 "return_parameter_convert.m3"
 /* begin_procedure */
#line 87 "return_parameter_convert.m3"
struct return_parameter_convert__ret_i32_u32_Frame_t {
#line 87 "return_parameter_convert.m3"
ADDRESS _unused;
#line 87 "return_parameter_convert.m3"
};
#line 87 "return_parameter_convert.m3"
INTEGER
__cdecl
return_parameter_convert__ret_i32_u32(
   /* Param_Type1 */ return_parameter_convert__INT32 a_L_76)
{
#line 87 "return_parameter_convert.m3"
return_parameter_convert__ret_i32_u32_Frame_t _frame;
#line 87 "return_parameter_convert.m3"
_frame._unused=(ADDRESS)&_frame;
#line 87 "return_parameter_convert.m3"
 /* load */
#line 87 "return_parameter_convert.m3"
 /* exit_proc */
#line 87 "return_parameter_convert.m3"
return ((INT64)(a_L_76));
#line 87 "return_parameter_convert.m3"
 /* end_procedure */
#line 87 "return_parameter_convert.m3"
} /* ret_i32_u8 */
#line 87 "return_parameter_convert.m3"
 /* set_source_line */
#line 87 "return_parameter_convert.m3"
#line 88 "return_parameter_convert.m3"
 /* begin_procedure */
#line 88 "return_parameter_convert.m3"
struct return_parameter_convert__ret_i32_u8_Frame_t {
#line 88 "return_parameter_convert.m3"
ADDRESS _unused;
#line 88 "return_parameter_convert.m3"
};
#line 88 "return_parameter_convert.m3"
INTEGER
__cdecl
return_parameter_convert__ret_i32_u8(
   /* Param_Type1 */ return_parameter_convert__INT32 a_L_78)
{
#line 88 "return_parameter_convert.m3"
return_parameter_convert__ret_i32_u8_Frame_t _frame;
#line 88 "return_parameter_convert.m3"
_frame._unused=(ADDRESS)&_frame;
#line 88 "return_parameter_convert.m3"
 /* load */
#line 88 "return_parameter_convert.m3"
 /* exit_proc */
#line 88 "return_parameter_convert.m3"
return ((INT64)(a_L_78));
#line 88 "return_parameter_convert.m3"
 /* end_procedure */
#line 88 "return_parameter_convert.m3"
} /* ret_i32_L */
#line 88 "return_parameter_convert.m3"
 /* set_source_line */
#line 88 "return_parameter_convert.m3"
#line 89 "return_parameter_convert.m3"
 /* begin_procedure */
#line 89 "return_parameter_convert.m3"
struct return_parameter_convert__ret_i32_L_Frame_t {
#line 89 "return_parameter_convert.m3"
ADDRESS _unused;
#line 89 "return_parameter_convert.m3"
};
#line 89 "return_parameter_convert.m3"
LONGINT
__cdecl
return_parameter_convert__ret_i32_L(
   /* Param_Type1 */ return_parameter_convert__INT32 a_L_80)
{
#line 89 "return_parameter_convert.m3"
return_parameter_convert__ret_i32_L_Frame_t _frame;
#line 89 "return_parameter_convert.m3"
_frame._unused=(ADDRESS)&_frame;
#line 89 "return_parameter_convert.m3"
 /* load */
#line 89 "return_parameter_convert.m3"
 /* loophole */
#line 89 "return_parameter_convert.m3"
 /* exit_proc */
#line 89 "return_parameter_convert.m3"
return (INT64)((INT64)(a_L_80));
#line 89 "return_parameter_convert.m3"
 /* end_procedure */
#line 89 "return_parameter_convert.m3"
} /* ret_LC_u64 */
#line 89 "return_parameter_convert.m3"
 /* set_source_line */
#line 89 "return_parameter_convert.m3"
#line 90 "return_parameter_convert.m3"
 /* begin_procedure */
#line 90 "return_parameter_convert.m3"
struct return_parameter_convert__ret_LC_u64_Frame_t {
#line 90 "return_parameter_convert.m3"
ADDRESS _unused;
#line 90 "return_parameter_convert.m3"
};
#line 90 "return_parameter_convert.m3"
LONGINT
__cdecl
return_parameter_convert__ret_LC_u64(
   /* Param_Type1 */ LONGCARD a_L_82)
{
#line 90 "return_parameter_convert.m3"
return_parameter_convert__ret_LC_u64_Frame_t _frame;
#line 90 "return_parameter_convert.m3"
_frame._unused=(ADDRESS)&_frame;
#line 90 "return_parameter_convert.m3"
 /* load */
#line 90 "return_parameter_convert.m3"
 /* exit_proc */
#line 90 "return_parameter_convert.m3"
return ((INT64)(a_L_82));
#line 90 "return_parameter_convert.m3"
 /* end_procedure */
#line 90 "return_parameter_convert.m3"
} /* ret_LC_i8 */
#line 90 "return_parameter_convert.m3"
 /* set_source_line */
#line 90 "return_parameter_convert.m3"
#line 91 "return_parameter_convert.m3"
 /* begin_procedure */
#line 91 "return_parameter_convert.m3"
struct return_parameter_convert__ret_LC_i8_Frame_t {
#line 91 "return_parameter_convert.m3"
ADDRESS _unused;
#line 91 "return_parameter_convert.m3"
};
#line 91 "return_parameter_convert.m3"
LONGINT
__cdecl
return_parameter_convert__ret_LC_i8(
   /* Param_Type1 */ LONGCARD a_L_84)
{
#line 91 "return_parameter_convert.m3"
return_parameter_convert__ret_LC_i8_Frame_t _frame;
#line 91 "return_parameter_convert.m3"
_frame._unused=(ADDRESS)&_frame;
#line 91 "return_parameter_convert.m3"
 /* load */
#line 91 "return_parameter_convert.m3"
 /* exit_proc */
#line 91 "return_parameter_convert.m3"
return ((INT64)(a_L_84));
#line 91 "return_parameter_convert.m3"
 /* end_procedure */
#line 91 "return_parameter_convert.m3"
} /* ret_LC_i32 */
#line 91 "return_parameter_convert.m3"
 /* set_source_line */
#line 91 "return_parameter_convert.m3"
#line 92 "return_parameter_convert.m3"
 /* begin_procedure */
#line 92 "return_parameter_convert.m3"
struct return_parameter_convert__ret_LC_i32_Frame_t {
#line 92 "return_parameter_convert.m3"
ADDRESS _unused;
#line 92 "return_parameter_convert.m3"
};
#line 92 "return_parameter_convert.m3"
LONGINT
__cdecl
return_parameter_convert__ret_LC_i32(
   /* Param_Type1 */ LONGCARD a_L_86)
{
#line 92 "return_parameter_convert.m3"
return_parameter_convert__ret_LC_i32_Frame_t _frame;
#line 92 "return_parameter_convert.m3"
_frame._unused=(ADDRESS)&_frame;
#line 92 "return_parameter_convert.m3"
 /* load */
#line 92 "return_parameter_convert.m3"
 /* exit_proc */
#line 92 "return_parameter_convert.m3"
return ((INT64)(a_L_86));
#line 92 "return_parameter_convert.m3"
 /* end_procedure */
#line 92 "return_parameter_convert.m3"
} /* ret_LC_LC */
#line 92 "return_parameter_convert.m3"
 /* set_source_line */
#line 92 "return_parameter_convert.m3"
#line 93 "return_parameter_convert.m3"
 /* begin_procedure */
#line 93 "return_parameter_convert.m3"
struct return_parameter_convert__ret_LC_LC_Frame_t {
#line 93 "return_parameter_convert.m3"
ADDRESS _unused;
#line 93 "return_parameter_convert.m3"
};
#line 93 "return_parameter_convert.m3"
LONGINT
__cdecl
return_parameter_convert__ret_LC_LC(
   /* Param_Type1 */ LONGCARD a_L_88)
{
#line 93 "return_parameter_convert.m3"
return_parameter_convert__ret_LC_LC_Frame_t _frame;
#line 93 "return_parameter_convert.m3"
_frame._unused=(ADDRESS)&_frame;
#line 93 "return_parameter_convert.m3"
 /* load */
#line 93 "return_parameter_convert.m3"
 /* exit_proc */
#line 93 "return_parameter_convert.m3"
return ((INT64)(a_L_88));
#line 93 "return_parameter_convert.m3"
 /* end_procedure */
#line 93 "return_parameter_convert.m3"
} /* ret_LC_u16 */
#line 93 "return_parameter_convert.m3"
 /* set_source_line */
#line 93 "return_parameter_convert.m3"
#line 94 "return_parameter_convert.m3"
 /* begin_procedure */
#line 94 "return_parameter_convert.m3"
struct return_parameter_convert__ret_LC_u16_Frame_t {
#line 94 "return_parameter_convert.m3"
ADDRESS _unused;
#line 94 "return_parameter_convert.m3"
};
#line 94 "return_parameter_convert.m3"
LONGINT
__cdecl
return_parameter_convert__ret_LC_u16(
   /* Param_Type1 */ LONGCARD a_L_90)
{
#line 94 "return_parameter_convert.m3"
return_parameter_convert__ret_LC_u16_Frame_t _frame;
#line 94 "return_parameter_convert.m3"
_frame._unused=(ADDRESS)&_frame;
#line 94 "return_parameter_convert.m3"
 /* load */
#line 94 "return_parameter_convert.m3"
 /* exit_proc */
#line 94 "return_parameter_convert.m3"
return ((INT64)(a_L_90));
#line 94 "return_parameter_convert.m3"
 /* end_procedure */
#line 94 "return_parameter_convert.m3"
} /* ret_LC_I */
#line 94 "return_parameter_convert.m3"
 /* set_source_line */
#line 94 "return_parameter_convert.m3"
#line 95 "return_parameter_convert.m3"
 /* begin_procedure */
#line 95 "return_parameter_convert.m3"
struct return_parameter_convert__ret_LC_I_Frame_t {
#line 95 "return_parameter_convert.m3"
ADDRESS _unused;
#line 95 "return_parameter_convert.m3"
};
#line 95 "return_parameter_convert.m3"
LONGINT
__cdecl
return_parameter_convert__ret_LC_I(
   /* Param_Type1 */ LONGCARD a_L_92)
{
#line 95 "return_parameter_convert.m3"
return_parameter_convert__ret_LC_I_Frame_t _frame;
#line 95 "return_parameter_convert.m3"
_frame._unused=(ADDRESS)&_frame;
#line 95 "return_parameter_convert.m3"
 /* load */
#line 95 "return_parameter_convert.m3"
 /* exit_proc */
#line 95 "return_parameter_convert.m3"
return ((INT64)(a_L_92));
#line 95 "return_parameter_convert.m3"
 /* end_procedure */
#line 95 "return_parameter_convert.m3"
} /* ret_LC_i64 */
#line 95 "return_parameter_convert.m3"
 /* set_source_line */
#line 95 "return_parameter_convert.m3"
#line 96 "return_parameter_convert.m3"
 /* begin_procedure */
#line 96 "return_parameter_convert.m3"
struct return_parameter_convert__ret_LC_i64_Frame_t {
#line 96 "return_parameter_convert.m3"
ADDRESS _unused;
#line 96 "return_parameter_convert.m3"
};
#line 96 "return_parameter_convert.m3"
LONGINT
__cdecl
return_parameter_convert__ret_LC_i64(
   /* Param_Type1 */ LONGCARD a_L_94)
{
#line 96 "return_parameter_convert.m3"
return_parameter_convert__ret_LC_i64_Frame_t _frame;
#line 96 "return_parameter_convert.m3"
_frame._unused=(ADDRESS)&_frame;
#line 96 "return_parameter_convert.m3"
 /* load */
#line 96 "return_parameter_convert.m3"
 /* exit_proc */
#line 96 "return_parameter_convert.m3"
return ((INT64)(a_L_94));
#line 96 "return_parameter_convert.m3"
 /* end_procedure */
#line 96 "return_parameter_convert.m3"
} /* ret_LC_C */
#line 96 "return_parameter_convert.m3"
 /* set_source_line */
#line 96 "return_parameter_convert.m3"
#line 97 "return_parameter_convert.m3"
 /* begin_procedure */
#line 97 "return_parameter_convert.m3"
struct return_parameter_convert__ret_LC_C_Frame_t {
#line 97 "return_parameter_convert.m3"
ADDRESS _unused;
#line 97 "return_parameter_convert.m3"
};
#line 97 "return_parameter_convert.m3"
LONGINT
__cdecl
return_parameter_convert__ret_LC_C(
   /* Param_Type1 */ LONGCARD a_L_96)
{
#line 97 "return_parameter_convert.m3"
return_parameter_convert__ret_LC_C_Frame_t _frame;
#line 97 "return_parameter_convert.m3"
_frame._unused=(ADDRESS)&_frame;
#line 97 "return_parameter_convert.m3"
 /* load */
#line 97 "return_parameter_convert.m3"
 /* exit_proc */
#line 97 "return_parameter_convert.m3"
return ((INT64)(a_L_96));
#line 97 "return_parameter_convert.m3"
 /* end_procedure */
#line 97 "return_parameter_convert.m3"
} /* ret_LC_i16 */
#line 97 "return_parameter_convert.m3"
 /* set_source_line */
#line 97 "return_parameter_convert.m3"
#line 98 "return_parameter_convert.m3"
 /* begin_procedure */
#line 98 "return_parameter_convert.m3"
struct return_parameter_convert__ret_LC_i16_Frame_t {
#line 98 "return_parameter_convert.m3"
ADDRESS _unused;
#line 98 "return_parameter_convert.m3"
};
#line 98 "return_parameter_convert.m3"
LONGINT
__cdecl
return_parameter_convert__ret_LC_i16(
   /* Param_Type1 */ LONGCARD a_L_98)
{
#line 98 "return_parameter_convert.m3"
return_parameter_convert__ret_LC_i16_Frame_t _frame;
#line 98 "return_parameter_convert.m3"
_frame._unused=(ADDRESS)&_frame;
#line 98 "return_parameter_convert.m3"
 /* load */
#line 98 "return_parameter_convert.m3"
 /* exit_proc */
#line 98 "return_parameter_convert.m3"
return ((INT64)(a_L_98));
#line 98 "return_parameter_convert.m3"
 /* end_procedure */
#line 98 "return_parameter_convert.m3"
} /* ret_LC_u32 */
#line 98 "return_parameter_convert.m3"
 /* set_source_line */
#line 98 "return_parameter_convert.m3"
#line 99 "return_parameter_convert.m3"
 /* begin_procedure */
#line 99 "return_parameter_convert.m3"
struct return_parameter_convert__ret_LC_u32_Frame_t {
#line 99 "return_parameter_convert.m3"
ADDRESS _unused;
#line 99 "return_parameter_convert.m3"
};
#line 99 "return_parameter_convert.m3"
LONGINT
__cdecl
return_parameter_convert__ret_LC_u32(
   /* Param_Type1 */ LONGCARD a_L_100)
{
#line 99 "return_parameter_convert.m3"
return_parameter_convert__ret_LC_u32_Frame_t _frame;
#line 99 "return_parameter_convert.m3"
_frame._unused=(ADDRESS)&_frame;
#line 99 "return_parameter_convert.m3"
 /* load */
#line 99 "return_parameter_convert.m3"
 /* exit_proc */
#line 99 "return_parameter_convert.m3"
return ((INT64)(a_L_100));
#line 99 "return_parameter_convert.m3"
 /* end_procedure */
#line 99 "return_parameter_convert.m3"
} /* ret_LC_u8 */
#line 99 "return_parameter_convert.m3"
 /* set_source_line */
#line 99 "return_parameter_convert.m3"
#line 100 "return_parameter_convert.m3"
 /* begin_procedure */
#line 100 "return_parameter_convert.m3"
struct return_parameter_convert__ret_LC_u8_Frame_t {
#line 100 "return_parameter_convert.m3"
ADDRESS _unused;
#line 100 "return_parameter_convert.m3"
};
#line 100 "return_parameter_convert.m3"
LONGINT
__cdecl
return_parameter_convert__ret_LC_u8(
   /* Param_Type1 */ LONGCARD a_L_102)
{
#line 100 "return_parameter_convert.m3"
return_parameter_convert__ret_LC_u8_Frame_t _frame;
#line 100 "return_parameter_convert.m3"
_frame._unused=(ADDRESS)&_frame;
#line 100 "return_parameter_convert.m3"
 /* load */
#line 100 "return_parameter_convert.m3"
 /* exit_proc */
#line 100 "return_parameter_convert.m3"
return ((INT64)(a_L_102));
#line 100 "return_parameter_convert.m3"
 /* end_procedure */
#line 100 "return_parameter_convert.m3"
} /* ret_LC_L */
#line 100 "return_parameter_convert.m3"
 /* set_source_line */
#line 100 "return_parameter_convert.m3"
#line 101 "return_parameter_convert.m3"
 /* begin_procedure */
#line 101 "return_parameter_convert.m3"
struct return_parameter_convert__ret_LC_L_Frame_t {
#line 101 "return_parameter_convert.m3"
ADDRESS _unused;
#line 101 "return_parameter_convert.m3"
};
#line 101 "return_parameter_convert.m3"
LONGINT
__cdecl
return_parameter_convert__ret_LC_L(
   /* Param_Type1 */ LONGCARD a_L_104)
{
#line 101 "return_parameter_convert.m3"
return_parameter_convert__ret_LC_L_Frame_t _frame;
#line 101 "return_parameter_convert.m3"
_frame._unused=(ADDRESS)&_frame;
#line 101 "return_parameter_convert.m3"
 /* load */
#line 101 "return_parameter_convert.m3"
 /* exit_proc */
#line 101 "return_parameter_convert.m3"
return ((INT64)(a_L_104));
#line 101 "return_parameter_convert.m3"
 /* end_procedure */
#line 101 "return_parameter_convert.m3"
} /* ret_u16_u64 */
#line 101 "return_parameter_convert.m3"
 /* set_source_line */
#line 101 "return_parameter_convert.m3"
#line 102 "return_parameter_convert.m3"
 /* begin_procedure */
#line 102 "return_parameter_convert.m3"
struct return_parameter_convert__ret_u16_u64_Frame_t {
#line 102 "return_parameter_convert.m3"
ADDRESS _unused;
#line 102 "return_parameter_convert.m3"
};
#line 102 "return_parameter_convert.m3"
LONGINT
__cdecl
return_parameter_convert__ret_u16_u64(
   /* Param_Type1 */ return_parameter_convert__UINT16 a_L_106)
{
#line 102 "return_parameter_convert.m3"
return_parameter_convert__ret_u16_u64_Frame_t _frame;
#line 102 "return_parameter_convert.m3"
_frame._unused=(ADDRESS)&_frame;
#line 102 "return_parameter_convert.m3"
 /* load */
#line 102 "return_parameter_convert.m3"
 /* loophole */
#line 102 "return_parameter_convert.m3"
 /* exit_proc */
#line 102 "return_parameter_convert.m3"
return (INT64)((INT64)(a_L_106));
#line 102 "return_parameter_convert.m3"
 /* end_procedure */
#line 102 "return_parameter_convert.m3"
} /* ret_u16_i8 */
#line 102 "return_parameter_convert.m3"
 /* set_source_line */
#line 102 "return_parameter_convert.m3"
#line 103 "return_parameter_convert.m3"
 /* begin_procedure */
#line 103 "return_parameter_convert.m3"
struct return_parameter_convert__ret_u16_i8_Frame_t {
#line 103 "return_parameter_convert.m3"
ADDRESS _unused;
#line 103 "return_parameter_convert.m3"
};
#line 103 "return_parameter_convert.m3"
INTEGER
__cdecl
return_parameter_convert__ret_u16_i8(
   /* Param_Type1 */ return_parameter_convert__UINT16 a_L_108)
{
#line 103 "return_parameter_convert.m3"
return_parameter_convert__ret_u16_i8_Frame_t _frame;
#line 103 "return_parameter_convert.m3"
_frame._unused=(ADDRESS)&_frame;
#line 103 "return_parameter_convert.m3"
 /* load */
#line 103 "return_parameter_convert.m3"
 /* exit_proc */
#line 103 "return_parameter_convert.m3"
return ((INT64)(a_L_108));
#line 103 "return_parameter_convert.m3"
 /* end_procedure */
#line 103 "return_parameter_convert.m3"
} /* ret_u16_i32 */
#line 103 "return_parameter_convert.m3"
 /* set_source_line */
#line 103 "return_parameter_convert.m3"
#line 104 "return_parameter_convert.m3"
 /* begin_procedure */
#line 104 "return_parameter_convert.m3"
struct return_parameter_convert__ret_u16_i32_Frame_t {
#line 104 "return_parameter_convert.m3"
ADDRESS _unused;
#line 104 "return_parameter_convert.m3"
};
#line 104 "return_parameter_convert.m3"
INTEGER
__cdecl
return_parameter_convert__ret_u16_i32(
   /* Param_Type1 */ return_parameter_convert__UINT16 a_L_110)
{
#line 104 "return_parameter_convert.m3"
return_parameter_convert__ret_u16_i32_Frame_t _frame;
#line 104 "return_parameter_convert.m3"
_frame._unused=(ADDRESS)&_frame;
#line 104 "return_parameter_convert.m3"
 /* load */
#line 104 "return_parameter_convert.m3"
 /* exit_proc */
#line 104 "return_parameter_convert.m3"
return ((INT64)(a_L_110));
#line 104 "return_parameter_convert.m3"
 /* end_procedure */
#line 104 "return_parameter_convert.m3"
} /* ret_u16_LC */
#line 104 "return_parameter_convert.m3"
 /* set_source_line */
#line 104 "return_parameter_convert.m3"
#line 105 "return_parameter_convert.m3"
 /* begin_procedure */
#line 105 "return_parameter_convert.m3"
struct return_parameter_convert__ret_u16_LC_Frame_t {
#line 105 "return_parameter_convert.m3"
ADDRESS _unused;
#line 105 "return_parameter_convert.m3"
};
#line 105 "return_parameter_convert.m3"
LONGINT
__cdecl
return_parameter_convert__ret_u16_LC(
   /* Param_Type1 */ return_parameter_convert__UINT16 a_L_112)
{
#line 105 "return_parameter_convert.m3"
return_parameter_convert__ret_u16_LC_Frame_t _frame;
#line 105 "return_parameter_convert.m3"
_frame._unused=(ADDRESS)&_frame;
#line 105 "return_parameter_convert.m3"
 /* load */
#line 105 "return_parameter_convert.m3"
 /* loophole */
#line 105 "return_parameter_convert.m3"
 /* exit_proc */
#line 105 "return_parameter_convert.m3"
return (INT64)((INT64)(a_L_112));
#line 105 "return_parameter_convert.m3"
 /* end_procedure */
#line 105 "return_parameter_convert.m3"
} /* ret_u16_u16 */
#line 105 "return_parameter_convert.m3"
 /* set_source_line */
#line 105 "return_parameter_convert.m3"
#line 106 "return_parameter_convert.m3"
 /* begin_procedure */
#line 106 "return_parameter_convert.m3"
struct return_parameter_convert__ret_u16_u16_Frame_t {
#line 106 "return_parameter_convert.m3"
ADDRESS _unused;
#line 106 "return_parameter_convert.m3"
};
#line 106 "return_parameter_convert.m3"
INTEGER
__cdecl
return_parameter_convert__ret_u16_u16(
   /* Param_Type1 */ return_parameter_convert__UINT16 a_L_114)
{
#line 106 "return_parameter_convert.m3"
return_parameter_convert__ret_u16_u16_Frame_t _frame;
#line 106 "return_parameter_convert.m3"
_frame._unused=(ADDRESS)&_frame;
#line 106 "return_parameter_convert.m3"
 /* load */
#line 106 "return_parameter_convert.m3"
 /* exit_proc */
#line 106 "return_parameter_convert.m3"
return ((INT64)(a_L_114));
#line 106 "return_parameter_convert.m3"
 /* end_procedure */
#line 106 "return_parameter_convert.m3"
} /* ret_u16_I */
#line 106 "return_parameter_convert.m3"
 /* set_source_line */
#line 106 "return_parameter_convert.m3"
#line 107 "return_parameter_convert.m3"
 /* begin_procedure */
#line 107 "return_parameter_convert.m3"
struct return_parameter_convert__ret_u16_I_Frame_t {
#line 107 "return_parameter_convert.m3"
ADDRESS _unused;
#line 107 "return_parameter_convert.m3"
};
#line 107 "return_parameter_convert.m3"
INTEGER
__cdecl
return_parameter_convert__ret_u16_I(
   /* Param_Type1 */ return_parameter_convert__UINT16 a_L_116)
{
#line 107 "return_parameter_convert.m3"
return_parameter_convert__ret_u16_I_Frame_t _frame;
#line 107 "return_parameter_convert.m3"
_frame._unused=(ADDRESS)&_frame;
#line 107 "return_parameter_convert.m3"
 /* load */
#line 107 "return_parameter_convert.m3"
 /* exit_proc */
#line 107 "return_parameter_convert.m3"
return ((INT64)(a_L_116));
#line 107 "return_parameter_convert.m3"
 /* end_procedure */
#line 107 "return_parameter_convert.m3"
} /* ret_u16_i64 */
#line 107 "return_parameter_convert.m3"
 /* set_source_line */
#line 107 "return_parameter_convert.m3"
#line 108 "return_parameter_convert.m3"
 /* begin_procedure */
#line 108 "return_parameter_convert.m3"
struct return_parameter_convert__ret_u16_i64_Frame_t {
#line 108 "return_parameter_convert.m3"
ADDRESS _unused;
#line 108 "return_parameter_convert.m3"
};
#line 108 "return_parameter_convert.m3"
LONGINT
__cdecl
return_parameter_convert__ret_u16_i64(
   /* Param_Type1 */ return_parameter_convert__UINT16 a_L_118)
{
#line 108 "return_parameter_convert.m3"
return_parameter_convert__ret_u16_i64_Frame_t _frame;
#line 108 "return_parameter_convert.m3"
_frame._unused=(ADDRESS)&_frame;
#line 108 "return_parameter_convert.m3"
 /* load */
#line 108 "return_parameter_convert.m3"
 /* loophole */
#line 108 "return_parameter_convert.m3"
 /* exit_proc */
#line 108 "return_parameter_convert.m3"
return (INT64)((INT64)(a_L_118));
#line 108 "return_parameter_convert.m3"
 /* end_procedure */
#line 108 "return_parameter_convert.m3"
} /* ret_u16_C */
#line 108 "return_parameter_convert.m3"
 /* set_source_line */
#line 108 "return_parameter_convert.m3"
#line 109 "return_parameter_convert.m3"
 /* begin_procedure */
#line 109 "return_parameter_convert.m3"
struct return_parameter_convert__ret_u16_C_Frame_t {
#line 109 "return_parameter_convert.m3"
ADDRESS _unused;
#line 109 "return_parameter_convert.m3"
};
#line 109 "return_parameter_convert.m3"
INTEGER
__cdecl
return_parameter_convert__ret_u16_C(
   /* Param_Type1 */ return_parameter_convert__UINT16 a_L_120)
{
#line 109 "return_parameter_convert.m3"
return_parameter_convert__ret_u16_C_Frame_t _frame;
#line 109 "return_parameter_convert.m3"
_frame._unused=(ADDRESS)&_frame;
#line 109 "return_parameter_convert.m3"
 /* load */
#line 109 "return_parameter_convert.m3"
 /* exit_proc */
#line 109 "return_parameter_convert.m3"
return ((INT64)(a_L_120));
#line 109 "return_parameter_convert.m3"
 /* end_procedure */
#line 109 "return_parameter_convert.m3"
} /* ret_u16_i16 */
#line 109 "return_parameter_convert.m3"
 /* set_source_line */
#line 109 "return_parameter_convert.m3"
#line 110 "return_parameter_convert.m3"
 /* begin_procedure */
#line 110 "return_parameter_convert.m3"
struct return_parameter_convert__ret_u16_i16_Frame_t {
#line 110 "return_parameter_convert.m3"
ADDRESS _unused;
#line 110 "return_parameter_convert.m3"
};
#line 110 "return_parameter_convert.m3"
INTEGER
__cdecl
return_parameter_convert__ret_u16_i16(
   /* Param_Type1 */ return_parameter_convert__UINT16 a_L_122)
{
#line 110 "return_parameter_convert.m3"
return_parameter_convert__ret_u16_i16_Frame_t _frame;
#line 110 "return_parameter_convert.m3"
_frame._unused=(ADDRESS)&_frame;
#line 110 "return_parameter_convert.m3"
 /* load */
#line 110 "return_parameter_convert.m3"
 /* exit_proc */
#line 110 "return_parameter_convert.m3"
return ((INT64)(a_L_122));
#line 110 "return_parameter_convert.m3"
 /* end_procedure */
#line 110 "return_parameter_convert.m3"
} /* ret_u16_u32 */
#line 110 "return_parameter_convert.m3"
 /* set_source_line */
#line 110 "return_parameter_convert.m3"
#line 111 "return_parameter_convert.m3"
 /* begin_procedure */
#line 111 "return_parameter_convert.m3"
struct return_parameter_convert__ret_u16_u32_Frame_t {
#line 111 "return_parameter_convert.m3"
ADDRESS _unused;
#line 111 "return_parameter_convert.m3"
};
#line 111 "return_parameter_convert.m3"
INTEGER
__cdecl
return_parameter_convert__ret_u16_u32(
   /* Param_Type1 */ return_parameter_convert__UINT16 a_L_124)
{
#line 111 "return_parameter_convert.m3"
return_parameter_convert__ret_u16_u32_Frame_t _frame;
#line 111 "return_parameter_convert.m3"
_frame._unused=(ADDRESS)&_frame;
#line 111 "return_parameter_convert.m3"
 /* load */
#line 111 "return_parameter_convert.m3"
 /* exit_proc */
#line 111 "return_parameter_convert.m3"
return ((INT64)(a_L_124));
#line 111 "return_parameter_convert.m3"
 /* end_procedure */
#line 111 "return_parameter_convert.m3"
} /* ret_u16_u8 */
#line 111 "return_parameter_convert.m3"
 /* set_source_line */
#line 111 "return_parameter_convert.m3"
#line 112 "return_parameter_convert.m3"
 /* begin_procedure */
#line 112 "return_parameter_convert.m3"
struct return_parameter_convert__ret_u16_u8_Frame_t {
#line 112 "return_parameter_convert.m3"
ADDRESS _unused;
#line 112 "return_parameter_convert.m3"
};
#line 112 "return_parameter_convert.m3"
INTEGER
__cdecl
return_parameter_convert__ret_u16_u8(
   /* Param_Type1 */ return_parameter_convert__UINT16 a_L_126)
{
#line 112 "return_parameter_convert.m3"
return_parameter_convert__ret_u16_u8_Frame_t _frame;
#line 112 "return_parameter_convert.m3"
_frame._unused=(ADDRESS)&_frame;
#line 112 "return_parameter_convert.m3"
 /* load */
#line 112 "return_parameter_convert.m3"
 /* exit_proc */
#line 112 "return_parameter_convert.m3"
return ((INT64)(a_L_126));
#line 112 "return_parameter_convert.m3"
 /* end_procedure */
#line 112 "return_parameter_convert.m3"
} /* ret_u16_L */
#line 112 "return_parameter_convert.m3"
 /* set_source_line */
#line 112 "return_parameter_convert.m3"
#line 113 "return_parameter_convert.m3"
 /* begin_procedure */
#line 113 "return_parameter_convert.m3"
struct return_parameter_convert__ret_u16_L_Frame_t {
#line 113 "return_parameter_convert.m3"
ADDRESS _unused;
#line 113 "return_parameter_convert.m3"
};
#line 113 "return_parameter_convert.m3"
LONGINT
__cdecl
return_parameter_convert__ret_u16_L(
   /* Param_Type1 */ return_parameter_convert__UINT16 a_L_128)
{
#line 113 "return_parameter_convert.m3"
return_parameter_convert__ret_u16_L_Frame_t _frame;
#line 113 "return_parameter_convert.m3"
_frame._unused=(ADDRESS)&_frame;
#line 113 "return_parameter_convert.m3"
 /* load */
#line 113 "return_parameter_convert.m3"
 /* loophole */
#line 113 "return_parameter_convert.m3"
 /* exit_proc */
#line 113 "return_parameter_convert.m3"
return (INT64)((INT64)(a_L_128));
#line 113 "return_parameter_convert.m3"
 /* end_procedure */
#line 113 "return_parameter_convert.m3"
} /* ret_I_u64 */
#line 113 "return_parameter_convert.m3"
 /* set_source_line */
#line 113 "return_parameter_convert.m3"
#line 114 "return_parameter_convert.m3"
 /* begin_procedure */
#line 114 "return_parameter_convert.m3"
struct return_parameter_convert__ret_I_u64_Frame_t {
#line 114 "return_parameter_convert.m3"
ADDRESS _unused;
#line 114 "return_parameter_convert.m3"
};
#line 114 "return_parameter_convert.m3"
LONGINT
__cdecl
return_parameter_convert__ret_I_u64(
   /* Param_Type1 */ INTEGER a_L_130)
{
#line 114 "return_parameter_convert.m3"
return_parameter_convert__ret_I_u64_Frame_t _frame;
#line 114 "return_parameter_convert.m3"
_frame._unused=(ADDRESS)&_frame;
#line 114 "return_parameter_convert.m3"
 /* load */
#line 114 "return_parameter_convert.m3"
 /* loophole */
#line 114 "return_parameter_convert.m3"
 /* exit_proc */
#line 114 "return_parameter_convert.m3"
return (INT64)a_L_130;
#line 114 "return_parameter_convert.m3"
 /* end_procedure */
#line 114 "return_parameter_convert.m3"
} /* ret_I_i8 */
#line 114 "return_parameter_convert.m3"
 /* set_source_line */
#line 114 "return_parameter_convert.m3"
#line 115 "return_parameter_convert.m3"
 /* begin_procedure */
#line 115 "return_parameter_convert.m3"
struct return_parameter_convert__ret_I_i8_Frame_t {
#line 115 "return_parameter_convert.m3"
ADDRESS _unused;
#line 115 "return_parameter_convert.m3"
};
#line 115 "return_parameter_convert.m3"
INTEGER
__cdecl
return_parameter_convert__ret_I_i8(
   /* Param_Type1 */ INTEGER a_L_132)
{
#line 115 "return_parameter_convert.m3"
return_parameter_convert__ret_I_i8_Frame_t _frame;
#line 115 "return_parameter_convert.m3"
_frame._unused=(ADDRESS)&_frame;
#line 115 "return_parameter_convert.m3"
 /* load */
#line 115 "return_parameter_convert.m3"
 /* exit_proc */
#line 115 "return_parameter_convert.m3"
return a_L_132;
#line 115 "return_parameter_convert.m3"
 /* end_procedure */
#line 115 "return_parameter_convert.m3"
} /* ret_I_i32 */
#line 115 "return_parameter_convert.m3"
 /* set_source_line */
#line 115 "return_parameter_convert.m3"
#line 116 "return_parameter_convert.m3"
 /* begin_procedure */
#line 116 "return_parameter_convert.m3"
struct return_parameter_convert__ret_I_i32_Frame_t {
#line 116 "return_parameter_convert.m3"
ADDRESS _unused;
#line 116 "return_parameter_convert.m3"
};
#line 116 "return_parameter_convert.m3"
INTEGER
__cdecl
return_parameter_convert__ret_I_i32(
   /* Param_Type1 */ INTEGER a_L_134)
{
#line 116 "return_parameter_convert.m3"
return_parameter_convert__ret_I_i32_Frame_t _frame;
#line 116 "return_parameter_convert.m3"
_frame._unused=(ADDRESS)&_frame;
#line 116 "return_parameter_convert.m3"
 /* load */
#line 116 "return_parameter_convert.m3"
 /* exit_proc */
#line 116 "return_parameter_convert.m3"
return a_L_134;
#line 116 "return_parameter_convert.m3"
 /* end_procedure */
#line 116 "return_parameter_convert.m3"
} /* ret_I_LC */
#line 116 "return_parameter_convert.m3"
 /* set_source_line */
#line 116 "return_parameter_convert.m3"
#line 117 "return_parameter_convert.m3"
 /* begin_procedure */
#line 117 "return_parameter_convert.m3"
struct return_parameter_convert__ret_I_LC_Frame_t {
#line 117 "return_parameter_convert.m3"
ADDRESS _unused;
#line 117 "return_parameter_convert.m3"
};
#line 117 "return_parameter_convert.m3"
LONGINT
__cdecl
return_parameter_convert__ret_I_LC(
   /* Param_Type1 */ INTEGER a_L_136)
{
#line 117 "return_parameter_convert.m3"
return_parameter_convert__ret_I_LC_Frame_t _frame;
#line 117 "return_parameter_convert.m3"
_frame._unused=(ADDRESS)&_frame;
#line 117 "return_parameter_convert.m3"
 /* load */
#line 117 "return_parameter_convert.m3"
 /* loophole */
#line 117 "return_parameter_convert.m3"
 /* exit_proc */
#line 117 "return_parameter_convert.m3"
return (INT64)a_L_136;
#line 117 "return_parameter_convert.m3"
 /* end_procedure */
#line 117 "return_parameter_convert.m3"
} /* ret_I_u16 */
#line 117 "return_parameter_convert.m3"
 /* set_source_line */
#line 117 "return_parameter_convert.m3"
#line 118 "return_parameter_convert.m3"
 /* begin_procedure */
#line 118 "return_parameter_convert.m3"
struct return_parameter_convert__ret_I_u16_Frame_t {
#line 118 "return_parameter_convert.m3"
ADDRESS _unused;
#line 118 "return_parameter_convert.m3"
};
#line 118 "return_parameter_convert.m3"
INTEGER
__cdecl
return_parameter_convert__ret_I_u16(
   /* Param_Type1 */ INTEGER a_L_138)
{
#line 118 "return_parameter_convert.m3"
return_parameter_convert__ret_I_u16_Frame_t _frame;
#line 118 "return_parameter_convert.m3"
_frame._unused=(ADDRESS)&_frame;
#line 118 "return_parameter_convert.m3"
 /* load */
#line 118 "return_parameter_convert.m3"
 /* exit_proc */
#line 118 "return_parameter_convert.m3"
return a_L_138;
#line 118 "return_parameter_convert.m3"
 /* end_procedure */
#line 118 "return_parameter_convert.m3"
} /* ret_I_I */
#line 118 "return_parameter_convert.m3"
 /* set_source_line */
#line 118 "return_parameter_convert.m3"
#line 119 "return_parameter_convert.m3"
 /* begin_procedure */
#line 119 "return_parameter_convert.m3"
struct return_parameter_convert__ret_I_I_Frame_t {
#line 119 "return_parameter_convert.m3"
ADDRESS _unused;
#line 119 "return_parameter_convert.m3"
};
#line 119 "return_parameter_convert.m3"
INTEGER
__cdecl
return_parameter_convert__ret_I_I(
   /* Param_Type1 */ INTEGER a_L_140)
{
#line 119 "return_parameter_convert.m3"
return_parameter_convert__ret_I_I_Frame_t _frame;
#line 119 "return_parameter_convert.m3"
_frame._unused=(ADDRESS)&_frame;
#line 119 "return_parameter_convert.m3"
 /* load */
#line 119 "return_parameter_convert.m3"
 /* exit_proc */
#line 119 "return_parameter_convert.m3"
return a_L_140;
#line 119 "return_parameter_convert.m3"
 /* end_procedure */
#line 119 "return_parameter_convert.m3"
} /* ret_I_i64 */
#line 119 "return_parameter_convert.m3"
 /* set_source_line */
#line 119 "return_parameter_convert.m3"
#line 120 "return_parameter_convert.m3"
 /* begin_procedure */
#line 120 "return_parameter_convert.m3"
struct return_parameter_convert__ret_I_i64_Frame_t {
#line 120 "return_parameter_convert.m3"
ADDRESS _unused;
#line 120 "return_parameter_convert.m3"
};
#line 120 "return_parameter_convert.m3"
LONGINT
__cdecl
return_parameter_convert__ret_I_i64(
   /* Param_Type1 */ INTEGER a_L_142)
{
#line 120 "return_parameter_convert.m3"
return_parameter_convert__ret_I_i64_Frame_t _frame;
#line 120 "return_parameter_convert.m3"
_frame._unused=(ADDRESS)&_frame;
#line 120 "return_parameter_convert.m3"
 /* load */
#line 120 "return_parameter_convert.m3"
 /* loophole */
#line 120 "return_parameter_convert.m3"
 /* exit_proc */
#line 120 "return_parameter_convert.m3"
return (INT64)a_L_142;
#line 120 "return_parameter_convert.m3"
 /* end_procedure */
#line 120 "return_parameter_convert.m3"
} /* ret_I_C */
#line 120 "return_parameter_convert.m3"
 /* set_source_line */
#line 120 "return_parameter_convert.m3"
#line 121 "return_parameter_convert.m3"
 /* begin_procedure */
#line 121 "return_parameter_convert.m3"
struct return_parameter_convert__ret_I_C_Frame_t {
#line 121 "return_parameter_convert.m3"
ADDRESS _unused;
#line 121 "return_parameter_convert.m3"
};
#line 121 "return_parameter_convert.m3"
INTEGER
__cdecl
return_parameter_convert__ret_I_C(
   /* Param_Type1 */ INTEGER a_L_144)
{
#line 121 "return_parameter_convert.m3"
return_parameter_convert__ret_I_C_Frame_t _frame;
#line 121 "return_parameter_convert.m3"
_frame._unused=(ADDRESS)&_frame;
#line 121 "return_parameter_convert.m3"
 /* load */
#line 121 "return_parameter_convert.m3"
 /* exit_proc */
#line 121 "return_parameter_convert.m3"
return a_L_144;
#line 121 "return_parameter_convert.m3"
 /* end_procedure */
#line 121 "return_parameter_convert.m3"
} /* ret_I_i16 */
#line 121 "return_parameter_convert.m3"
 /* set_source_line */
#line 121 "return_parameter_convert.m3"
#line 122 "return_parameter_convert.m3"
 /* begin_procedure */
#line 122 "return_parameter_convert.m3"
struct return_parameter_convert__ret_I_i16_Frame_t {
#line 122 "return_parameter_convert.m3"
ADDRESS _unused;
#line 122 "return_parameter_convert.m3"
};
#line 122 "return_parameter_convert.m3"
INTEGER
__cdecl
return_parameter_convert__ret_I_i16(
   /* Param_Type1 */ INTEGER a_L_146)
{
#line 122 "return_parameter_convert.m3"
return_parameter_convert__ret_I_i16_Frame_t _frame;
#line 122 "return_parameter_convert.m3"
_frame._unused=(ADDRESS)&_frame;
#line 122 "return_parameter_convert.m3"
 /* load */
#line 122 "return_parameter_convert.m3"
 /* exit_proc */
#line 122 "return_parameter_convert.m3"
return a_L_146;
#line 122 "return_parameter_convert.m3"
 /* end_procedure */
#line 122 "return_parameter_convert.m3"
} /* ret_I_u32 */
#line 122 "return_parameter_convert.m3"
 /* set_source_line */
#line 122 "return_parameter_convert.m3"
#line 123 "return_parameter_convert.m3"
 /* begin_procedure */
#line 123 "return_parameter_convert.m3"
struct return_parameter_convert__ret_I_u32_Frame_t {
#line 123 "return_parameter_convert.m3"
ADDRESS _unused;
#line 123 "return_parameter_convert.m3"
};
#line 123 "return_parameter_convert.m3"
INTEGER
__cdecl
return_parameter_convert__ret_I_u32(
   /* Param_Type1 */ INTEGER a_L_148)
{
#line 123 "return_parameter_convert.m3"
return_parameter_convert__ret_I_u32_Frame_t _frame;
#line 123 "return_parameter_convert.m3"
_frame._unused=(ADDRESS)&_frame;
#line 123 "return_parameter_convert.m3"
 /* load */
#line 123 "return_parameter_convert.m3"
 /* exit_proc */
#line 123 "return_parameter_convert.m3"
return a_L_148;
#line 123 "return_parameter_convert.m3"
 /* end_procedure */
#line 123 "return_parameter_convert.m3"
} /* ret_I_u8 */
#line 123 "return_parameter_convert.m3"
 /* set_source_line */
#line 123 "return_parameter_convert.m3"
#line 124 "return_parameter_convert.m3"
 /* begin_procedure */
#line 124 "return_parameter_convert.m3"
struct return_parameter_convert__ret_I_u8_Frame_t {
#line 124 "return_parameter_convert.m3"
ADDRESS _unused;
#line 124 "return_parameter_convert.m3"
};
#line 124 "return_parameter_convert.m3"
INTEGER
__cdecl
return_parameter_convert__ret_I_u8(
   /* Param_Type1 */ INTEGER a_L_150)
{
#line 124 "return_parameter_convert.m3"
return_parameter_convert__ret_I_u8_Frame_t _frame;
#line 124 "return_parameter_convert.m3"
_frame._unused=(ADDRESS)&_frame;
#line 124 "return_parameter_convert.m3"
 /* load */
#line 124 "return_parameter_convert.m3"
 /* exit_proc */
#line 124 "return_parameter_convert.m3"
return a_L_150;
#line 124 "return_parameter_convert.m3"
 /* end_procedure */
#line 124 "return_parameter_convert.m3"
} /* ret_I_L */
#line 124 "return_parameter_convert.m3"
 /* set_source_line */
#line 124 "return_parameter_convert.m3"
#line 125 "return_parameter_convert.m3"
 /* begin_procedure */
#line 125 "return_parameter_convert.m3"
struct return_parameter_convert__ret_I_L_Frame_t {
#line 125 "return_parameter_convert.m3"
ADDRESS _unused;
#line 125 "return_parameter_convert.m3"
};
#line 125 "return_parameter_convert.m3"
LONGINT
__cdecl
return_parameter_convert__ret_I_L(
   /* Param_Type1 */ INTEGER a_L_152)
{
#line 125 "return_parameter_convert.m3"
return_parameter_convert__ret_I_L_Frame_t _frame;
#line 125 "return_parameter_convert.m3"
_frame._unused=(ADDRESS)&_frame;
#line 125 "return_parameter_convert.m3"
 /* load */
#line 125 "return_parameter_convert.m3"
 /* loophole */
#line 125 "return_parameter_convert.m3"
 /* exit_proc */
#line 125 "return_parameter_convert.m3"
return (INT64)a_L_152;
#line 125 "return_parameter_convert.m3"
 /* end_procedure */
#line 125 "return_parameter_convert.m3"
} /* ret_i64_u64 */
#line 125 "return_parameter_convert.m3"
 /* set_source_line */
#line 125 "return_parameter_convert.m3"
#line 126 "return_parameter_convert.m3"
 /* begin_procedure */
#line 126 "return_parameter_convert.m3"
struct return_parameter_convert__ret_i64_u64_Frame_t {
#line 126 "return_parameter_convert.m3"
ADDRESS _unused;
#line 126 "return_parameter_convert.m3"
};
#line 126 "return_parameter_convert.m3"
LONGINT
__cdecl
return_parameter_convert__ret_i64_u64(
   /* Param_Type1 */ return_parameter_convert__INT64 a_L_154)
{
#line 126 "return_parameter_convert.m3"
return_parameter_convert__ret_i64_u64_Frame_t _frame;
#line 126 "return_parameter_convert.m3"
_frame._unused=(ADDRESS)&_frame;
#line 126 "return_parameter_convert.m3"
 /* load */
#line 126 "return_parameter_convert.m3"
 /* exit_proc */
#line 126 "return_parameter_convert.m3"
return a_L_154;
#line 126 "return_parameter_convert.m3"
 /* end_procedure */
#line 126 "return_parameter_convert.m3"
} /* ret_i64_i8 */
#line 126 "return_parameter_convert.m3"
 /* set_source_line */
#line 126 "return_parameter_convert.m3"
#line 127 "return_parameter_convert.m3"
 /* begin_procedure */
#line 127 "return_parameter_convert.m3"
struct return_parameter_convert__ret_i64_i8_Frame_t {
#line 127 "return_parameter_convert.m3"
ADDRESS _unused;
#line 127 "return_parameter_convert.m3"
};
#line 127 "return_parameter_convert.m3"
LONGINT
__cdecl
return_parameter_convert__ret_i64_i8(
   /* Param_Type1 */ return_parameter_convert__INT64 a_L_156)
{
#line 127 "return_parameter_convert.m3"
return_parameter_convert__ret_i64_i8_Frame_t _frame;
#line 127 "return_parameter_convert.m3"
_frame._unused=(ADDRESS)&_frame;
#line 127 "return_parameter_convert.m3"
 /* load */
#line 127 "return_parameter_convert.m3"
 /* exit_proc */
#line 127 "return_parameter_convert.m3"
return a_L_156;
#line 127 "return_parameter_convert.m3"
 /* end_procedure */
#line 127 "return_parameter_convert.m3"
} /* ret_i64_i32 */
#line 127 "return_parameter_convert.m3"
 /* set_source_line */
#line 127 "return_parameter_convert.m3"
#line 128 "return_parameter_convert.m3"
 /* begin_procedure */
#line 128 "return_parameter_convert.m3"
struct return_parameter_convert__ret_i64_i32_Frame_t {
#line 128 "return_parameter_convert.m3"
ADDRESS _unused;
#line 128 "return_parameter_convert.m3"
};
#line 128 "return_parameter_convert.m3"
LONGINT
__cdecl
return_parameter_convert__ret_i64_i32(
   /* Param_Type1 */ return_parameter_convert__INT64 a_L_158)
{
#line 128 "return_parameter_convert.m3"
return_parameter_convert__ret_i64_i32_Frame_t _frame;
#line 128 "return_parameter_convert.m3"
_frame._unused=(ADDRESS)&_frame;
#line 128 "return_parameter_convert.m3"
 /* load */
#line 128 "return_parameter_convert.m3"
 /* exit_proc */
#line 128 "return_parameter_convert.m3"
return a_L_158;
#line 128 "return_parameter_convert.m3"
 /* end_procedure */
#line 128 "return_parameter_convert.m3"
} /* ret_i64_LC */
#line 128 "return_parameter_convert.m3"
 /* set_source_line */
#line 128 "return_parameter_convert.m3"
#line 129 "return_parameter_convert.m3"
 /* begin_procedure */
#line 129 "return_parameter_convert.m3"
struct return_parameter_convert__ret_i64_LC_Frame_t {
#line 129 "return_parameter_convert.m3"
ADDRESS _unused;
#line 129 "return_parameter_convert.m3"
};
#line 129 "return_parameter_convert.m3"
LONGINT
__cdecl
return_parameter_convert__ret_i64_LC(
   /* Param_Type1 */ return_parameter_convert__INT64 a_L_160)
{
#line 129 "return_parameter_convert.m3"
return_parameter_convert__ret_i64_LC_Frame_t _frame;
#line 129 "return_parameter_convert.m3"
_frame._unused=(ADDRESS)&_frame;
#line 129 "return_parameter_convert.m3"
 /* load */
#line 129 "return_parameter_convert.m3"
 /* exit_proc */
#line 129 "return_parameter_convert.m3"
return a_L_160;
#line 129 "return_parameter_convert.m3"
 /* end_procedure */
#line 129 "return_parameter_convert.m3"
} /* ret_i64_u16 */
#line 129 "return_parameter_convert.m3"
 /* set_source_line */
#line 129 "return_parameter_convert.m3"
#line 130 "return_parameter_convert.m3"
 /* begin_procedure */
#line 130 "return_parameter_convert.m3"
struct return_parameter_convert__ret_i64_u16_Frame_t {
#line 130 "return_parameter_convert.m3"
ADDRESS _unused;
#line 130 "return_parameter_convert.m3"
};
#line 130 "return_parameter_convert.m3"
LONGINT
__cdecl
return_parameter_convert__ret_i64_u16(
   /* Param_Type1 */ return_parameter_convert__INT64 a_L_162)
{
#line 130 "return_parameter_convert.m3"
return_parameter_convert__ret_i64_u16_Frame_t _frame;
#line 130 "return_parameter_convert.m3"
_frame._unused=(ADDRESS)&_frame;
#line 130 "return_parameter_convert.m3"
 /* load */
#line 130 "return_parameter_convert.m3"
 /* exit_proc */
#line 130 "return_parameter_convert.m3"
return a_L_162;
#line 130 "return_parameter_convert.m3"
 /* end_procedure */
#line 130 "return_parameter_convert.m3"
} /* ret_i64_I */
#line 130 "return_parameter_convert.m3"
 /* set_source_line */
#line 130 "return_parameter_convert.m3"
#line 131 "return_parameter_convert.m3"
 /* begin_procedure */
#line 131 "return_parameter_convert.m3"
struct return_parameter_convert__ret_i64_I_Frame_t {
#line 131 "return_parameter_convert.m3"
ADDRESS _unused;
#line 131 "return_parameter_convert.m3"
};
#line 131 "return_parameter_convert.m3"
LONGINT
__cdecl
return_parameter_convert__ret_i64_I(
   /* Param_Type1 */ return_parameter_convert__INT64 a_L_164)
{
#line 131 "return_parameter_convert.m3"
return_parameter_convert__ret_i64_I_Frame_t _frame;
#line 131 "return_parameter_convert.m3"
_frame._unused=(ADDRESS)&_frame;
#line 131 "return_parameter_convert.m3"
 /* load */
#line 131 "return_parameter_convert.m3"
 /* exit_proc */
#line 131 "return_parameter_convert.m3"
return a_L_164;
#line 131 "return_parameter_convert.m3"
 /* end_procedure */
#line 131 "return_parameter_convert.m3"
} /* ret_i64_i64 */
#line 131 "return_parameter_convert.m3"
 /* set_source_line */
#line 131 "return_parameter_convert.m3"
#line 132 "return_parameter_convert.m3"
 /* begin_procedure */
#line 132 "return_parameter_convert.m3"
struct return_parameter_convert__ret_i64_i64_Frame_t {
#line 132 "return_parameter_convert.m3"
ADDRESS _unused;
#line 132 "return_parameter_convert.m3"
};
#line 132 "return_parameter_convert.m3"
LONGINT
__cdecl
return_parameter_convert__ret_i64_i64(
   /* Param_Type1 */ return_parameter_convert__INT64 a_L_166)
{
#line 132 "return_parameter_convert.m3"
return_parameter_convert__ret_i64_i64_Frame_t _frame;
#line 132 "return_parameter_convert.m3"
_frame._unused=(ADDRESS)&_frame;
#line 132 "return_parameter_convert.m3"
 /* load */
#line 132 "return_parameter_convert.m3"
 /* exit_proc */
#line 132 "return_parameter_convert.m3"
return a_L_166;
#line 132 "return_parameter_convert.m3"
 /* end_procedure */
#line 132 "return_parameter_convert.m3"
} /* ret_i64_C */
#line 132 "return_parameter_convert.m3"
 /* set_source_line */
#line 132 "return_parameter_convert.m3"
#line 133 "return_parameter_convert.m3"
 /* begin_procedure */
#line 133 "return_parameter_convert.m3"
struct return_parameter_convert__ret_i64_C_Frame_t {
#line 133 "return_parameter_convert.m3"
ADDRESS _unused;
#line 133 "return_parameter_convert.m3"
};
#line 133 "return_parameter_convert.m3"
LONGINT
__cdecl
return_parameter_convert__ret_i64_C(
   /* Param_Type1 */ return_parameter_convert__INT64 a_L_168)
{
#line 133 "return_parameter_convert.m3"
return_parameter_convert__ret_i64_C_Frame_t _frame;
#line 133 "return_parameter_convert.m3"
_frame._unused=(ADDRESS)&_frame;
#line 133 "return_parameter_convert.m3"
 /* load */
#line 133 "return_parameter_convert.m3"
 /* exit_proc */
#line 133 "return_parameter_convert.m3"
return a_L_168;
#line 133 "return_parameter_convert.m3"
 /* end_procedure */
#line 133 "return_parameter_convert.m3"
} /* ret_i64_i16 */
#line 133 "return_parameter_convert.m3"
 /* set_source_line */
#line 133 "return_parameter_convert.m3"
#line 134 "return_parameter_convert.m3"
 /* begin_procedure */
#line 134 "return_parameter_convert.m3"
struct return_parameter_convert__ret_i64_i16_Frame_t {
#line 134 "return_parameter_convert.m3"
ADDRESS _unused;
#line 134 "return_parameter_convert.m3"
};
#line 134 "return_parameter_convert.m3"
LONGINT
__cdecl
return_parameter_convert__ret_i64_i16(
   /* Param_Type1 */ return_parameter_convert__INT64 a_L_170)
{
#line 134 "return_parameter_convert.m3"
return_parameter_convert__ret_i64_i16_Frame_t _frame;
#line 134 "return_parameter_convert.m3"
_frame._unused=(ADDRESS)&_frame;
#line 134 "return_parameter_convert.m3"
 /* load */
#line 134 "return_parameter_convert.m3"
 /* exit_proc */
#line 134 "return_parameter_convert.m3"
return a_L_170;
#line 134 "return_parameter_convert.m3"
 /* end_procedure */
#line 134 "return_parameter_convert.m3"
} /* ret_i64_u32 */
#line 134 "return_parameter_convert.m3"
 /* set_source_line */
#line 134 "return_parameter_convert.m3"
#line 135 "return_parameter_convert.m3"
 /* begin_procedure */
#line 135 "return_parameter_convert.m3"
struct return_parameter_convert__ret_i64_u32_Frame_t {
#line 135 "return_parameter_convert.m3"
ADDRESS _unused;
#line 135 "return_parameter_convert.m3"
};
#line 135 "return_parameter_convert.m3"
LONGINT
__cdecl
return_parameter_convert__ret_i64_u32(
   /* Param_Type1 */ return_parameter_convert__INT64 a_L_172)
{
#line 135 "return_parameter_convert.m3"
return_parameter_convert__ret_i64_u32_Frame_t _frame;
#line 135 "return_parameter_convert.m3"
_frame._unused=(ADDRESS)&_frame;
#line 135 "return_parameter_convert.m3"
 /* load */
#line 135 "return_parameter_convert.m3"
 /* exit_proc */
#line 135 "return_parameter_convert.m3"
return a_L_172;
#line 135 "return_parameter_convert.m3"
 /* end_procedure */
#line 135 "return_parameter_convert.m3"
} /* ret_i64_u8 */
#line 135 "return_parameter_convert.m3"
 /* set_source_line */
#line 135 "return_parameter_convert.m3"
#line 136 "return_parameter_convert.m3"
 /* begin_procedure */
#line 136 "return_parameter_convert.m3"
struct return_parameter_convert__ret_i64_u8_Frame_t {
#line 136 "return_parameter_convert.m3"
ADDRESS _unused;
#line 136 "return_parameter_convert.m3"
};
#line 136 "return_parameter_convert.m3"
LONGINT
__cdecl
return_parameter_convert__ret_i64_u8(
   /* Param_Type1 */ return_parameter_convert__INT64 a_L_174)
{
#line 136 "return_parameter_convert.m3"
return_parameter_convert__ret_i64_u8_Frame_t _frame;
#line 136 "return_parameter_convert.m3"
_frame._unused=(ADDRESS)&_frame;
#line 136 "return_parameter_convert.m3"
 /* load */
#line 136 "return_parameter_convert.m3"
 /* exit_proc */
#line 136 "return_parameter_convert.m3"
return a_L_174;
#line 136 "return_parameter_convert.m3"
 /* end_procedure */
#line 136 "return_parameter_convert.m3"
} /* ret_i64_L */
#line 136 "return_parameter_convert.m3"
 /* set_source_line */
#line 136 "return_parameter_convert.m3"
#line 137 "return_parameter_convert.m3"
 /* begin_procedure */
#line 137 "return_parameter_convert.m3"
struct return_parameter_convert__ret_i64_L_Frame_t {
#line 137 "return_parameter_convert.m3"
ADDRESS _unused;
#line 137 "return_parameter_convert.m3"
};
#line 137 "return_parameter_convert.m3"
LONGINT
__cdecl
return_parameter_convert__ret_i64_L(
   /* Param_Type1 */ return_parameter_convert__INT64 a_L_176)
{
#line 137 "return_parameter_convert.m3"
return_parameter_convert__ret_i64_L_Frame_t _frame;
#line 137 "return_parameter_convert.m3"
_frame._unused=(ADDRESS)&_frame;
#line 137 "return_parameter_convert.m3"
 /* load */
#line 137 "return_parameter_convert.m3"
 /* exit_proc */
#line 137 "return_parameter_convert.m3"
return a_L_176;
#line 137 "return_parameter_convert.m3"
 /* end_procedure */
#line 137 "return_parameter_convert.m3"
} /* ret_C_u64 */
#line 137 "return_parameter_convert.m3"
 /* set_source_line */
#line 137 "return_parameter_convert.m3"
#line 138 "return_parameter_convert.m3"
 /* begin_procedure */
#line 138 "return_parameter_convert.m3"
struct return_parameter_convert__ret_C_u64_Frame_t {
#line 138 "return_parameter_convert.m3"
ADDRESS _unused;
#line 138 "return_parameter_convert.m3"
};
#line 138 "return_parameter_convert.m3"
LONGINT
__cdecl
return_parameter_convert__ret_C_u64(
   /* Param_Type1 */ CARDINAL a_L_178)
{
#line 138 "return_parameter_convert.m3"
return_parameter_convert__ret_C_u64_Frame_t _frame;
#line 138 "return_parameter_convert.m3"
_frame._unused=(ADDRESS)&_frame;
#line 138 "return_parameter_convert.m3"
 /* load */
#line 138 "return_parameter_convert.m3"
 /* loophole */
#line 138 "return_parameter_convert.m3"
 /* exit_proc */
#line 138 "return_parameter_convert.m3"
return (INT64)((INT64)(a_L_178));
#line 138 "return_parameter_convert.m3"
 /* end_procedure */
#line 138 "return_parameter_convert.m3"
} /* ret_C_i8 */
#line 138 "return_parameter_convert.m3"
 /* set_source_line */
#line 138 "return_parameter_convert.m3"
#line 139 "return_parameter_convert.m3"
 /* begin_procedure */
#line 139 "return_parameter_convert.m3"
struct return_parameter_convert__ret_C_i8_Frame_t {
#line 139 "return_parameter_convert.m3"
ADDRESS _unused;
#line 139 "return_parameter_convert.m3"
};
#line 139 "return_parameter_convert.m3"
INTEGER
__cdecl
return_parameter_convert__ret_C_i8(
   /* Param_Type1 */ CARDINAL a_L_180)
{
#line 139 "return_parameter_convert.m3"
return_parameter_convert__ret_C_i8_Frame_t _frame;
#line 139 "return_parameter_convert.m3"
_frame._unused=(ADDRESS)&_frame;
#line 139 "return_parameter_convert.m3"
 /* load */
#line 139 "return_parameter_convert.m3"
 /* exit_proc */
#line 139 "return_parameter_convert.m3"
return ((INT64)(a_L_180));
#line 139 "return_parameter_convert.m3"
 /* end_procedure */
#line 139 "return_parameter_convert.m3"
} /* ret_C_i32 */
#line 139 "return_parameter_convert.m3"
 /* set_source_line */
#line 139 "return_parameter_convert.m3"
#line 140 "return_parameter_convert.m3"
 /* begin_procedure */
#line 140 "return_parameter_convert.m3"
struct return_parameter_convert__ret_C_i32_Frame_t {
#line 140 "return_parameter_convert.m3"
ADDRESS _unused;
#line 140 "return_parameter_convert.m3"
};
#line 140 "return_parameter_convert.m3"
INTEGER
__cdecl
return_parameter_convert__ret_C_i32(
   /* Param_Type1 */ CARDINAL a_L_182)
{
#line 140 "return_parameter_convert.m3"
return_parameter_convert__ret_C_i32_Frame_t _frame;
#line 140 "return_parameter_convert.m3"
_frame._unused=(ADDRESS)&_frame;
#line 140 "return_parameter_convert.m3"
 /* load */
#line 140 "return_parameter_convert.m3"
 /* exit_proc */
#line 140 "return_parameter_convert.m3"
return ((INT64)(a_L_182));
#line 140 "return_parameter_convert.m3"
 /* end_procedure */
#line 140 "return_parameter_convert.m3"
} /* ret_C_LC */
#line 140 "return_parameter_convert.m3"
 /* set_source_line */
#line 140 "return_parameter_convert.m3"
#line 141 "return_parameter_convert.m3"
 /* begin_procedure */
#line 141 "return_parameter_convert.m3"
struct return_parameter_convert__ret_C_LC_Frame_t {
#line 141 "return_parameter_convert.m3"
ADDRESS _unused;
#line 141 "return_parameter_convert.m3"
};
#line 141 "return_parameter_convert.m3"
LONGINT
__cdecl
return_parameter_convert__ret_C_LC(
   /* Param_Type1 */ CARDINAL a_L_184)
{
#line 141 "return_parameter_convert.m3"
return_parameter_convert__ret_C_LC_Frame_t _frame;
#line 141 "return_parameter_convert.m3"
_frame._unused=(ADDRESS)&_frame;
#line 141 "return_parameter_convert.m3"
 /* load */
#line 141 "return_parameter_convert.m3"
 /* loophole */
#line 141 "return_parameter_convert.m3"
 /* exit_proc */
#line 141 "return_parameter_convert.m3"
return (INT64)((INT64)(a_L_184));
#line 141 "return_parameter_convert.m3"
 /* end_procedure */
#line 141 "return_parameter_convert.m3"
} /* ret_C_u16 */
#line 141 "return_parameter_convert.m3"
 /* set_source_line */
#line 141 "return_parameter_convert.m3"
#line 142 "return_parameter_convert.m3"
 /* begin_procedure */
#line 142 "return_parameter_convert.m3"
struct return_parameter_convert__ret_C_u16_Frame_t {
#line 142 "return_parameter_convert.m3"
ADDRESS _unused;
#line 142 "return_parameter_convert.m3"
};
#line 142 "return_parameter_convert.m3"
INTEGER
__cdecl
return_parameter_convert__ret_C_u16(
   /* Param_Type1 */ CARDINAL a_L_186)
{
#line 142 "return_parameter_convert.m3"
return_parameter_convert__ret_C_u16_Frame_t _frame;
#line 142 "return_parameter_convert.m3"
_frame._unused=(ADDRESS)&_frame;
#line 142 "return_parameter_convert.m3"
 /* load */
#line 142 "return_parameter_convert.m3"
 /* exit_proc */
#line 142 "return_parameter_convert.m3"
return ((INT64)(a_L_186));
#line 142 "return_parameter_convert.m3"
 /* end_procedure */
#line 142 "return_parameter_convert.m3"
} /* ret_C_I */
#line 142 "return_parameter_convert.m3"
 /* set_source_line */
#line 142 "return_parameter_convert.m3"
#line 143 "return_parameter_convert.m3"
 /* begin_procedure */
#line 143 "return_parameter_convert.m3"
struct return_parameter_convert__ret_C_I_Frame_t {
#line 143 "return_parameter_convert.m3"
ADDRESS _unused;
#line 143 "return_parameter_convert.m3"
};
#line 143 "return_parameter_convert.m3"
INTEGER
__cdecl
return_parameter_convert__ret_C_I(
   /* Param_Type1 */ CARDINAL a_L_188)
{
#line 143 "return_parameter_convert.m3"
return_parameter_convert__ret_C_I_Frame_t _frame;
#line 143 "return_parameter_convert.m3"
_frame._unused=(ADDRESS)&_frame;
#line 143 "return_parameter_convert.m3"
 /* load */
#line 143 "return_parameter_convert.m3"
 /* exit_proc */
#line 143 "return_parameter_convert.m3"
return ((INT64)(a_L_188));
#line 143 "return_parameter_convert.m3"
 /* end_procedure */
#line 143 "return_parameter_convert.m3"
} /* ret_C_i64 */
#line 143 "return_parameter_convert.m3"
 /* set_source_line */
#line 143 "return_parameter_convert.m3"
#line 144 "return_parameter_convert.m3"
 /* begin_procedure */
#line 144 "return_parameter_convert.m3"
struct return_parameter_convert__ret_C_i64_Frame_t {
#line 144 "return_parameter_convert.m3"
ADDRESS _unused;
#line 144 "return_parameter_convert.m3"
};
#line 144 "return_parameter_convert.m3"
LONGINT
__cdecl
return_parameter_convert__ret_C_i64(
   /* Param_Type1 */ CARDINAL a_L_190)
{
#line 144 "return_parameter_convert.m3"
return_parameter_convert__ret_C_i64_Frame_t _frame;
#line 144 "return_parameter_convert.m3"
_frame._unused=(ADDRESS)&_frame;
#line 144 "return_parameter_convert.m3"
 /* load */
#line 144 "return_parameter_convert.m3"
 /* loophole */
#line 144 "return_parameter_convert.m3"
 /* exit_proc */
#line 144 "return_parameter_convert.m3"
return (INT64)((INT64)(a_L_190));
#line 144 "return_parameter_convert.m3"
 /* end_procedure */
#line 144 "return_parameter_convert.m3"
} /* ret_C_C */
#line 144 "return_parameter_convert.m3"
 /* set_source_line */
#line 144 "return_parameter_convert.m3"
#line 145 "return_parameter_convert.m3"
 /* begin_procedure */
#line 145 "return_parameter_convert.m3"
struct return_parameter_convert__ret_C_C_Frame_t {
#line 145 "return_parameter_convert.m3"
ADDRESS _unused;
#line 145 "return_parameter_convert.m3"
};
#line 145 "return_parameter_convert.m3"
INTEGER
__cdecl
return_parameter_convert__ret_C_C(
   /* Param_Type1 */ CARDINAL a_L_192)
{
#line 145 "return_parameter_convert.m3"
return_parameter_convert__ret_C_C_Frame_t _frame;
#line 145 "return_parameter_convert.m3"
_frame._unused=(ADDRESS)&_frame;
#line 145 "return_parameter_convert.m3"
 /* load */
#line 145 "return_parameter_convert.m3"
 /* exit_proc */
#line 145 "return_parameter_convert.m3"
return ((INT64)(a_L_192));
#line 145 "return_parameter_convert.m3"
 /* end_procedure */
#line 145 "return_parameter_convert.m3"
} /* ret_C_i16 */
#line 145 "return_parameter_convert.m3"
 /* set_source_line */
#line 145 "return_parameter_convert.m3"
#line 146 "return_parameter_convert.m3"
 /* begin_procedure */
#line 146 "return_parameter_convert.m3"
struct return_parameter_convert__ret_C_i16_Frame_t {
#line 146 "return_parameter_convert.m3"
ADDRESS _unused;
#line 146 "return_parameter_convert.m3"
};
#line 146 "return_parameter_convert.m3"
INTEGER
__cdecl
return_parameter_convert__ret_C_i16(
   /* Param_Type1 */ CARDINAL a_L_194)
{
#line 146 "return_parameter_convert.m3"
return_parameter_convert__ret_C_i16_Frame_t _frame;
#line 146 "return_parameter_convert.m3"
_frame._unused=(ADDRESS)&_frame;
#line 146 "return_parameter_convert.m3"
 /* load */
#line 146 "return_parameter_convert.m3"
 /* exit_proc */
#line 146 "return_parameter_convert.m3"
return ((INT64)(a_L_194));
#line 146 "return_parameter_convert.m3"
 /* end_procedure */
#line 146 "return_parameter_convert.m3"
} /* ret_C_u32 */
#line 146 "return_parameter_convert.m3"
 /* set_source_line */
#line 146 "return_parameter_convert.m3"
#line 147 "return_parameter_convert.m3"
 /* begin_procedure */
#line 147 "return_parameter_convert.m3"
struct return_parameter_convert__ret_C_u32_Frame_t {
#line 147 "return_parameter_convert.m3"
ADDRESS _unused;
#line 147 "return_parameter_convert.m3"
};
#line 147 "return_parameter_convert.m3"
INTEGER
__cdecl
return_parameter_convert__ret_C_u32(
   /* Param_Type1 */ CARDINAL a_L_196)
{
#line 147 "return_parameter_convert.m3"
return_parameter_convert__ret_C_u32_Frame_t _frame;
#line 147 "return_parameter_convert.m3"
_frame._unused=(ADDRESS)&_frame;
#line 147 "return_parameter_convert.m3"
 /* load */
#line 147 "return_parameter_convert.m3"
 /* exit_proc */
#line 147 "return_parameter_convert.m3"
return ((INT64)(a_L_196));
#line 147 "return_parameter_convert.m3"
 /* end_procedure */
#line 147 "return_parameter_convert.m3"
} /* ret_C_u8 */
#line 147 "return_parameter_convert.m3"
 /* set_source_line */
#line 147 "return_parameter_convert.m3"
#line 148 "return_parameter_convert.m3"
 /* begin_procedure */
#line 148 "return_parameter_convert.m3"
struct return_parameter_convert__ret_C_u8_Frame_t {
#line 148 "return_parameter_convert.m3"
ADDRESS _unused;
#line 148 "return_parameter_convert.m3"
};
#line 148 "return_parameter_convert.m3"
INTEGER
__cdecl
return_parameter_convert__ret_C_u8(
   /* Param_Type1 */ CARDINAL a_L_198)
{
#line 148 "return_parameter_convert.m3"
return_parameter_convert__ret_C_u8_Frame_t _frame;
#line 148 "return_parameter_convert.m3"
_frame._unused=(ADDRESS)&_frame;
#line 148 "return_parameter_convert.m3"
 /* load */
#line 148 "return_parameter_convert.m3"
 /* exit_proc */
#line 148 "return_parameter_convert.m3"
return ((INT64)(a_L_198));
#line 148 "return_parameter_convert.m3"
 /* end_procedure */
#line 148 "return_parameter_convert.m3"
} /* ret_C_L */
#line 148 "return_parameter_convert.m3"
 /* set_source_line */
#line 148 "return_parameter_convert.m3"
#line 149 "return_parameter_convert.m3"
 /* begin_procedure */
#line 149 "return_parameter_convert.m3"
struct return_parameter_convert__ret_C_L_Frame_t {
#line 149 "return_parameter_convert.m3"
ADDRESS _unused;
#line 149 "return_parameter_convert.m3"
};
#line 149 "return_parameter_convert.m3"
LONGINT
__cdecl
return_parameter_convert__ret_C_L(
   /* Param_Type1 */ CARDINAL a_L_200)
{
#line 149 "return_parameter_convert.m3"
return_parameter_convert__ret_C_L_Frame_t _frame;
#line 149 "return_parameter_convert.m3"
_frame._unused=(ADDRESS)&_frame;
#line 149 "return_parameter_convert.m3"
 /* load */
#line 149 "return_parameter_convert.m3"
 /* loophole */
#line 149 "return_parameter_convert.m3"
 /* exit_proc */
#line 149 "return_parameter_convert.m3"
return (INT64)((INT64)(a_L_200));
#line 149 "return_parameter_convert.m3"
 /* end_procedure */
#line 149 "return_parameter_convert.m3"
} /* ret_i16_u64 */
#line 149 "return_parameter_convert.m3"
 /* set_source_line */
#line 149 "return_parameter_convert.m3"
#line 150 "return_parameter_convert.m3"
 /* begin_procedure */
#line 150 "return_parameter_convert.m3"
struct return_parameter_convert__ret_i16_u64_Frame_t {
#line 150 "return_parameter_convert.m3"
ADDRESS _unused;
#line 150 "return_parameter_convert.m3"
};
#line 150 "return_parameter_convert.m3"
LONGINT
__cdecl
return_parameter_convert__ret_i16_u64(
   /* Param_Type1 */ return_parameter_convert__INT16 a_L_202)
{
#line 150 "return_parameter_convert.m3"
return_parameter_convert__ret_i16_u64_Frame_t _frame;
#line 150 "return_parameter_convert.m3"
_frame._unused=(ADDRESS)&_frame;
#line 150 "return_parameter_convert.m3"
 /* load */
#line 150 "return_parameter_convert.m3"
 /* loophole */
#line 150 "return_parameter_convert.m3"
 /* exit_proc */
#line 150 "return_parameter_convert.m3"
return (INT64)((INT64)(a_L_202));
#line 150 "return_parameter_convert.m3"
 /* end_procedure */
#line 150 "return_parameter_convert.m3"
} /* ret_i16_i8 */
#line 150 "return_parameter_convert.m3"
 /* set_source_line */
#line 150 "return_parameter_convert.m3"
#line 151 "return_parameter_convert.m3"
 /* begin_procedure */
#line 151 "return_parameter_convert.m3"
struct return_parameter_convert__ret_i16_i8_Frame_t {
#line 151 "return_parameter_convert.m3"
ADDRESS _unused;
#line 151 "return_parameter_convert.m3"
};
#line 151 "return_parameter_convert.m3"
INTEGER
__cdecl
return_parameter_convert__ret_i16_i8(
   /* Param_Type1 */ return_parameter_convert__INT16 a_L_204)
{
#line 151 "return_parameter_convert.m3"
return_parameter_convert__ret_i16_i8_Frame_t _frame;
#line 151 "return_parameter_convert.m3"
_frame._unused=(ADDRESS)&_frame;
#line 151 "return_parameter_convert.m3"
 /* load */
#line 151 "return_parameter_convert.m3"
 /* exit_proc */
#line 151 "return_parameter_convert.m3"
return ((INT64)(a_L_204));
#line 151 "return_parameter_convert.m3"
 /* end_procedure */
#line 151 "return_parameter_convert.m3"
} /* ret_i16_i32 */
#line 151 "return_parameter_convert.m3"
 /* set_source_line */
#line 151 "return_parameter_convert.m3"
#line 152 "return_parameter_convert.m3"
 /* begin_procedure */
#line 152 "return_parameter_convert.m3"
struct return_parameter_convert__ret_i16_i32_Frame_t {
#line 152 "return_parameter_convert.m3"
ADDRESS _unused;
#line 152 "return_parameter_convert.m3"
};
#line 152 "return_parameter_convert.m3"
INTEGER
__cdecl
return_parameter_convert__ret_i16_i32(
   /* Param_Type1 */ return_parameter_convert__INT16 a_L_206)
{
#line 152 "return_parameter_convert.m3"
return_parameter_convert__ret_i16_i32_Frame_t _frame;
#line 152 "return_parameter_convert.m3"
_frame._unused=(ADDRESS)&_frame;
#line 152 "return_parameter_convert.m3"
 /* load */
#line 152 "return_parameter_convert.m3"
 /* exit_proc */
#line 152 "return_parameter_convert.m3"
return ((INT64)(a_L_206));
#line 152 "return_parameter_convert.m3"
 /* end_procedure */
#line 152 "return_parameter_convert.m3"
} /* ret_i16_LC */
#line 152 "return_parameter_convert.m3"
 /* set_source_line */
#line 152 "return_parameter_convert.m3"
#line 153 "return_parameter_convert.m3"
 /* begin_procedure */
#line 153 "return_parameter_convert.m3"
struct return_parameter_convert__ret_i16_LC_Frame_t {
#line 153 "return_parameter_convert.m3"
ADDRESS _unused;
#line 153 "return_parameter_convert.m3"
};
#line 153 "return_parameter_convert.m3"
LONGINT
__cdecl
return_parameter_convert__ret_i16_LC(
   /* Param_Type1 */ return_parameter_convert__INT16 a_L_208)
{
#line 153 "return_parameter_convert.m3"
return_parameter_convert__ret_i16_LC_Frame_t _frame;
#line 153 "return_parameter_convert.m3"
_frame._unused=(ADDRESS)&_frame;
#line 153 "return_parameter_convert.m3"
 /* load */
#line 153 "return_parameter_convert.m3"
 /* loophole */
#line 153 "return_parameter_convert.m3"
 /* exit_proc */
#line 153 "return_parameter_convert.m3"
return (INT64)((INT64)(a_L_208));
#line 153 "return_parameter_convert.m3"
 /* end_procedure */
#line 153 "return_parameter_convert.m3"
} /* ret_i16_u16 */
#line 153 "return_parameter_convert.m3"
 /* set_source_line */
#line 153 "return_parameter_convert.m3"
#line 154 "return_parameter_convert.m3"
 /* begin_procedure */
#line 154 "return_parameter_convert.m3"
struct return_parameter_convert__ret_i16_u16_Frame_t {
#line 154 "return_parameter_convert.m3"
ADDRESS _unused;
#line 154 "return_parameter_convert.m3"
};
#line 154 "return_parameter_convert.m3"
INTEGER
__cdecl
return_parameter_convert__ret_i16_u16(
   /* Param_Type1 */ return_parameter_convert__INT16 a_L_210)
{
#line 154 "return_parameter_convert.m3"
return_parameter_convert__ret_i16_u16_Frame_t _frame;
#line 154 "return_parameter_convert.m3"
_frame._unused=(ADDRESS)&_frame;
#line 154 "return_parameter_convert.m3"
 /* load */
#line 154 "return_parameter_convert.m3"
 /* exit_proc */
#line 154 "return_parameter_convert.m3"
return ((INT64)(a_L_210));
#line 154 "return_parameter_convert.m3"
 /* end_procedure */
#line 154 "return_parameter_convert.m3"
} /* ret_i16_I */
#line 154 "return_parameter_convert.m3"
 /* set_source_line */
#line 154 "return_parameter_convert.m3"
#line 155 "return_parameter_convert.m3"
 /* begin_procedure */
#line 155 "return_parameter_convert.m3"
struct return_parameter_convert__ret_i16_I_Frame_t {
#line 155 "return_parameter_convert.m3"
ADDRESS _unused;
#line 155 "return_parameter_convert.m3"
};
#line 155 "return_parameter_convert.m3"
INTEGER
__cdecl
return_parameter_convert__ret_i16_I(
   /* Param_Type1 */ return_parameter_convert__INT16 a_L_212)
{
#line 155 "return_parameter_convert.m3"
return_parameter_convert__ret_i16_I_Frame_t _frame;
#line 155 "return_parameter_convert.m3"
_frame._unused=(ADDRESS)&_frame;
#line 155 "return_parameter_convert.m3"
 /* load */
#line 155 "return_parameter_convert.m3"
 /* exit_proc */
#line 155 "return_parameter_convert.m3"
return ((INT64)(a_L_212));
#line 155 "return_parameter_convert.m3"
 /* end_procedure */
#line 155 "return_parameter_convert.m3"
} /* ret_i16_i64 */
#line 155 "return_parameter_convert.m3"
 /* set_source_line */
#line 155 "return_parameter_convert.m3"
#line 156 "return_parameter_convert.m3"
 /* begin_procedure */
#line 156 "return_parameter_convert.m3"
struct return_parameter_convert__ret_i16_i64_Frame_t {
#line 156 "return_parameter_convert.m3"
ADDRESS _unused;
#line 156 "return_parameter_convert.m3"
};
#line 156 "return_parameter_convert.m3"
LONGINT
__cdecl
return_parameter_convert__ret_i16_i64(
   /* Param_Type1 */ return_parameter_convert__INT16 a_L_214)
{
#line 156 "return_parameter_convert.m3"
return_parameter_convert__ret_i16_i64_Frame_t _frame;
#line 156 "return_parameter_convert.m3"
_frame._unused=(ADDRESS)&_frame;
#line 156 "return_parameter_convert.m3"
 /* load */
#line 156 "return_parameter_convert.m3"
 /* loophole */
#line 156 "return_parameter_convert.m3"
 /* exit_proc */
#line 156 "return_parameter_convert.m3"
return (INT64)((INT64)(a_L_214));
#line 156 "return_parameter_convert.m3"
 /* end_procedure */
#line 156 "return_parameter_convert.m3"
} /* ret_i16_C */
#line 156 "return_parameter_convert.m3"
 /* set_source_line */
#line 156 "return_parameter_convert.m3"
#line 157 "return_parameter_convert.m3"
 /* begin_procedure */
#line 157 "return_parameter_convert.m3"
struct return_parameter_convert__ret_i16_C_Frame_t {
#line 157 "return_parameter_convert.m3"
ADDRESS _unused;
#line 157 "return_parameter_convert.m3"
};
#line 157 "return_parameter_convert.m3"
INTEGER
__cdecl
return_parameter_convert__ret_i16_C(
   /* Param_Type1 */ return_parameter_convert__INT16 a_L_216)
{
#line 157 "return_parameter_convert.m3"
return_parameter_convert__ret_i16_C_Frame_t _frame;
#line 157 "return_parameter_convert.m3"
_frame._unused=(ADDRESS)&_frame;
#line 157 "return_parameter_convert.m3"
 /* load */
#line 157 "return_parameter_convert.m3"
 /* exit_proc */
#line 157 "return_parameter_convert.m3"
return ((INT64)(a_L_216));
#line 157 "return_parameter_convert.m3"
 /* end_procedure */
#line 157 "return_parameter_convert.m3"
} /* ret_i16_i16 */
#line 157 "return_parameter_convert.m3"
 /* set_source_line */
#line 157 "return_parameter_convert.m3"
#line 158 "return_parameter_convert.m3"
 /* begin_procedure */
#line 158 "return_parameter_convert.m3"
struct return_parameter_convert__ret_i16_i16_Frame_t {
#line 158 "return_parameter_convert.m3"
ADDRESS _unused;
#line 158 "return_parameter_convert.m3"
};
#line 158 "return_parameter_convert.m3"
INTEGER
__cdecl
return_parameter_convert__ret_i16_i16(
   /* Param_Type1 */ return_parameter_convert__INT16 a_L_218)
{
#line 158 "return_parameter_convert.m3"
return_parameter_convert__ret_i16_i16_Frame_t _frame;
#line 158 "return_parameter_convert.m3"
_frame._unused=(ADDRESS)&_frame;
#line 158 "return_parameter_convert.m3"
 /* load */
#line 158 "return_parameter_convert.m3"
 /* exit_proc */
#line 158 "return_parameter_convert.m3"
return ((INT64)(a_L_218));
#line 158 "return_parameter_convert.m3"
 /* end_procedure */
#line 158 "return_parameter_convert.m3"
} /* ret_i16_u32 */
#line 158 "return_parameter_convert.m3"
 /* set_source_line */
#line 158 "return_parameter_convert.m3"
#line 159 "return_parameter_convert.m3"
 /* begin_procedure */
#line 159 "return_parameter_convert.m3"
struct return_parameter_convert__ret_i16_u32_Frame_t {
#line 159 "return_parameter_convert.m3"
ADDRESS _unused;
#line 159 "return_parameter_convert.m3"
};
#line 159 "return_parameter_convert.m3"
INTEGER
__cdecl
return_parameter_convert__ret_i16_u32(
   /* Param_Type1 */ return_parameter_convert__INT16 a_L_220)
{
#line 159 "return_parameter_convert.m3"
return_parameter_convert__ret_i16_u32_Frame_t _frame;
#line 159 "return_parameter_convert.m3"
_frame._unused=(ADDRESS)&_frame;
#line 159 "return_parameter_convert.m3"
 /* load */
#line 159 "return_parameter_convert.m3"
 /* exit_proc */
#line 159 "return_parameter_convert.m3"
return ((INT64)(a_L_220));
#line 159 "return_parameter_convert.m3"
 /* end_procedure */
#line 159 "return_parameter_convert.m3"
} /* ret_i16_u8 */
#line 159 "return_parameter_convert.m3"
 /* set_source_line */
#line 159 "return_parameter_convert.m3"
#line 160 "return_parameter_convert.m3"
 /* begin_procedure */
#line 160 "return_parameter_convert.m3"
struct return_parameter_convert__ret_i16_u8_Frame_t {
#line 160 "return_parameter_convert.m3"
ADDRESS _unused;
#line 160 "return_parameter_convert.m3"
};
#line 160 "return_parameter_convert.m3"
INTEGER
__cdecl
return_parameter_convert__ret_i16_u8(
   /* Param_Type1 */ return_parameter_convert__INT16 a_L_222)
{
#line 160 "return_parameter_convert.m3"
return_parameter_convert__ret_i16_u8_Frame_t _frame;
#line 160 "return_parameter_convert.m3"
_frame._unused=(ADDRESS)&_frame;
#line 160 "return_parameter_convert.m3"
 /* load */
#line 160 "return_parameter_convert.m3"
 /* exit_proc */
#line 160 "return_parameter_convert.m3"
return ((INT64)(a_L_222));
#line 160 "return_parameter_convert.m3"
 /* end_procedure */
#line 160 "return_parameter_convert.m3"
} /* ret_i16_L */
#line 160 "return_parameter_convert.m3"
 /* set_source_line */
#line 160 "return_parameter_convert.m3"
#line 161 "return_parameter_convert.m3"
 /* begin_procedure */
#line 161 "return_parameter_convert.m3"
struct return_parameter_convert__ret_i16_L_Frame_t {
#line 161 "return_parameter_convert.m3"
ADDRESS _unused;
#line 161 "return_parameter_convert.m3"
};
#line 161 "return_parameter_convert.m3"
LONGINT
__cdecl
return_parameter_convert__ret_i16_L(
   /* Param_Type1 */ return_parameter_convert__INT16 a_L_224)
{
#line 161 "return_parameter_convert.m3"
return_parameter_convert__ret_i16_L_Frame_t _frame;
#line 161 "return_parameter_convert.m3"
_frame._unused=(ADDRESS)&_frame;
#line 161 "return_parameter_convert.m3"
 /* load */
#line 161 "return_parameter_convert.m3"
 /* loophole */
#line 161 "return_parameter_convert.m3"
 /* exit_proc */
#line 161 "return_parameter_convert.m3"
return (INT64)((INT64)(a_L_224));
#line 161 "return_parameter_convert.m3"
 /* end_procedure */
#line 161 "return_parameter_convert.m3"
} /* ret_u32_u64 */
#line 161 "return_parameter_convert.m3"
 /* set_source_line */
#line 161 "return_parameter_convert.m3"
#line 162 "return_parameter_convert.m3"
 /* begin_procedure */
#line 162 "return_parameter_convert.m3"
struct return_parameter_convert__ret_u32_u64_Frame_t {
#line 162 "return_parameter_convert.m3"
ADDRESS _unused;
#line 162 "return_parameter_convert.m3"
};
#line 162 "return_parameter_convert.m3"
LONGINT
__cdecl
return_parameter_convert__ret_u32_u64(
   /* Param_Type1 */ return_parameter_convert__UINT32 a_L_226)
{
#line 162 "return_parameter_convert.m3"
return_parameter_convert__ret_u32_u64_Frame_t _frame;
#line 162 "return_parameter_convert.m3"
_frame._unused=(ADDRESS)&_frame;
#line 162 "return_parameter_convert.m3"
 /* load */
#line 162 "return_parameter_convert.m3"
 /* loophole */
#line 162 "return_parameter_convert.m3"
 /* exit_proc */
#line 162 "return_parameter_convert.m3"
return (INT64)((INT64)(a_L_226));
#line 162 "return_parameter_convert.m3"
 /* end_procedure */
#line 162 "return_parameter_convert.m3"
} /* ret_u32_i8 */
#line 162 "return_parameter_convert.m3"
 /* set_source_line */
#line 162 "return_parameter_convert.m3"
#line 163 "return_parameter_convert.m3"
 /* begin_procedure */
#line 163 "return_parameter_convert.m3"
struct return_parameter_convert__ret_u32_i8_Frame_t {
#line 163 "return_parameter_convert.m3"
ADDRESS _unused;
#line 163 "return_parameter_convert.m3"
};
#line 163 "return_parameter_convert.m3"
INTEGER
__cdecl
return_parameter_convert__ret_u32_i8(
   /* Param_Type1 */ return_parameter_convert__UINT32 a_L_228)
{
#line 163 "return_parameter_convert.m3"
return_parameter_convert__ret_u32_i8_Frame_t _frame;
#line 163 "return_parameter_convert.m3"
_frame._unused=(ADDRESS)&_frame;
#line 163 "return_parameter_convert.m3"
 /* load */
#line 163 "return_parameter_convert.m3"
 /* exit_proc */
#line 163 "return_parameter_convert.m3"
return ((INT64)(a_L_228));
#line 163 "return_parameter_convert.m3"
 /* end_procedure */
#line 163 "return_parameter_convert.m3"
} /* ret_u32_i32 */
#line 163 "return_parameter_convert.m3"
 /* set_source_line */
#line 163 "return_parameter_convert.m3"
#line 164 "return_parameter_convert.m3"
 /* begin_procedure */
#line 164 "return_parameter_convert.m3"
struct return_parameter_convert__ret_u32_i32_Frame_t {
#line 164 "return_parameter_convert.m3"
ADDRESS _unused;
#line 164 "return_parameter_convert.m3"
};
#line 164 "return_parameter_convert.m3"
INTEGER
__cdecl
return_parameter_convert__ret_u32_i32(
   /* Param_Type1 */ return_parameter_convert__UINT32 a_L_230)
{
#line 164 "return_parameter_convert.m3"
return_parameter_convert__ret_u32_i32_Frame_t _frame;
#line 164 "return_parameter_convert.m3"
_frame._unused=(ADDRESS)&_frame;
#line 164 "return_parameter_convert.m3"
 /* load */
#line 164 "return_parameter_convert.m3"
 /* exit_proc */
#line 164 "return_parameter_convert.m3"
return ((INT64)(a_L_230));
#line 164 "return_parameter_convert.m3"
 /* end_procedure */
#line 164 "return_parameter_convert.m3"
} /* ret_u32_LC */
#line 164 "return_parameter_convert.m3"
 /* set_source_line */
#line 164 "return_parameter_convert.m3"
#line 165 "return_parameter_convert.m3"
 /* begin_procedure */
#line 165 "return_parameter_convert.m3"
struct return_parameter_convert__ret_u32_LC_Frame_t {
#line 165 "return_parameter_convert.m3"
ADDRESS _unused;
#line 165 "return_parameter_convert.m3"
};
#line 165 "return_parameter_convert.m3"
LONGINT
__cdecl
return_parameter_convert__ret_u32_LC(
   /* Param_Type1 */ return_parameter_convert__UINT32 a_L_232)
{
#line 165 "return_parameter_convert.m3"
return_parameter_convert__ret_u32_LC_Frame_t _frame;
#line 165 "return_parameter_convert.m3"
_frame._unused=(ADDRESS)&_frame;
#line 165 "return_parameter_convert.m3"
 /* load */
#line 165 "return_parameter_convert.m3"
 /* loophole */
#line 165 "return_parameter_convert.m3"
 /* exit_proc */
#line 165 "return_parameter_convert.m3"
return (INT64)((INT64)(a_L_232));
#line 165 "return_parameter_convert.m3"
 /* end_procedure */
#line 165 "return_parameter_convert.m3"
} /* ret_u32_u16 */
#line 165 "return_parameter_convert.m3"
 /* set_source_line */
#line 165 "return_parameter_convert.m3"
#line 166 "return_parameter_convert.m3"
 /* begin_procedure */
#line 166 "return_parameter_convert.m3"
struct return_parameter_convert__ret_u32_u16_Frame_t {
#line 166 "return_parameter_convert.m3"
ADDRESS _unused;
#line 166 "return_parameter_convert.m3"
};
#line 166 "return_parameter_convert.m3"
INTEGER
__cdecl
return_parameter_convert__ret_u32_u16(
   /* Param_Type1 */ return_parameter_convert__UINT32 a_L_234)
{
#line 166 "return_parameter_convert.m3"
return_parameter_convert__ret_u32_u16_Frame_t _frame;
#line 166 "return_parameter_convert.m3"
_frame._unused=(ADDRESS)&_frame;
#line 166 "return_parameter_convert.m3"
 /* load */
#line 166 "return_parameter_convert.m3"
 /* exit_proc */
#line 166 "return_parameter_convert.m3"
return ((INT64)(a_L_234));
#line 166 "return_parameter_convert.m3"
 /* end_procedure */
#line 166 "return_parameter_convert.m3"
} /* ret_u32_I */
#line 166 "return_parameter_convert.m3"
 /* set_source_line */
#line 166 "return_parameter_convert.m3"
#line 167 "return_parameter_convert.m3"
 /* begin_procedure */
#line 167 "return_parameter_convert.m3"
struct return_parameter_convert__ret_u32_I_Frame_t {
#line 167 "return_parameter_convert.m3"
ADDRESS _unused;
#line 167 "return_parameter_convert.m3"
};
#line 167 "return_parameter_convert.m3"
INTEGER
__cdecl
return_parameter_convert__ret_u32_I(
   /* Param_Type1 */ return_parameter_convert__UINT32 a_L_236)
{
#line 167 "return_parameter_convert.m3"
return_parameter_convert__ret_u32_I_Frame_t _frame;
#line 167 "return_parameter_convert.m3"
_frame._unused=(ADDRESS)&_frame;
#line 167 "return_parameter_convert.m3"
 /* load */
#line 167 "return_parameter_convert.m3"
 /* exit_proc */
#line 167 "return_parameter_convert.m3"
return ((INT64)(a_L_236));
#line 167 "return_parameter_convert.m3"
 /* end_procedure */
#line 167 "return_parameter_convert.m3"
} /* ret_u32_i64 */
#line 167 "return_parameter_convert.m3"
 /* set_source_line */
#line 167 "return_parameter_convert.m3"
#line 168 "return_parameter_convert.m3"
 /* begin_procedure */
#line 168 "return_parameter_convert.m3"
struct return_parameter_convert__ret_u32_i64_Frame_t {
#line 168 "return_parameter_convert.m3"
ADDRESS _unused;
#line 168 "return_parameter_convert.m3"
};
#line 168 "return_parameter_convert.m3"
LONGINT
__cdecl
return_parameter_convert__ret_u32_i64(
   /* Param_Type1 */ return_parameter_convert__UINT32 a_L_238)
{
#line 168 "return_parameter_convert.m3"
return_parameter_convert__ret_u32_i64_Frame_t _frame;
#line 168 "return_parameter_convert.m3"
_frame._unused=(ADDRESS)&_frame;
#line 168 "return_parameter_convert.m3"
 /* load */
#line 168 "return_parameter_convert.m3"
 /* loophole */
#line 168 "return_parameter_convert.m3"
 /* exit_proc */
#line 168 "return_parameter_convert.m3"
return (INT64)((INT64)(a_L_238));
#line 168 "return_parameter_convert.m3"
 /* end_procedure */
#line 168 "return_parameter_convert.m3"
} /* ret_u32_C */
#line 168 "return_parameter_convert.m3"
 /* set_source_line */
#line 168 "return_parameter_convert.m3"
#line 169 "return_parameter_convert.m3"
 /* begin_procedure */
#line 169 "return_parameter_convert.m3"
struct return_parameter_convert__ret_u32_C_Frame_t {
#line 169 "return_parameter_convert.m3"
ADDRESS _unused;
#line 169 "return_parameter_convert.m3"
};
#line 169 "return_parameter_convert.m3"
INTEGER
__cdecl
return_parameter_convert__ret_u32_C(
   /* Param_Type1 */ return_parameter_convert__UINT32 a_L_240)
{
#line 169 "return_parameter_convert.m3"
return_parameter_convert__ret_u32_C_Frame_t _frame;
#line 169 "return_parameter_convert.m3"
_frame._unused=(ADDRESS)&_frame;
#line 169 "return_parameter_convert.m3"
 /* load */
#line 169 "return_parameter_convert.m3"
 /* exit_proc */
#line 169 "return_parameter_convert.m3"
return ((INT64)(a_L_240));
#line 169 "return_parameter_convert.m3"
 /* end_procedure */
#line 169 "return_parameter_convert.m3"
} /* ret_u32_i16 */
#line 169 "return_parameter_convert.m3"
 /* set_source_line */
#line 169 "return_parameter_convert.m3"
#line 170 "return_parameter_convert.m3"
 /* begin_procedure */
#line 170 "return_parameter_convert.m3"
struct return_parameter_convert__ret_u32_i16_Frame_t {
#line 170 "return_parameter_convert.m3"
ADDRESS _unused;
#line 170 "return_parameter_convert.m3"
};
#line 170 "return_parameter_convert.m3"
INTEGER
__cdecl
return_parameter_convert__ret_u32_i16(
   /* Param_Type1 */ return_parameter_convert__UINT32 a_L_242)
{
#line 170 "return_parameter_convert.m3"
return_parameter_convert__ret_u32_i16_Frame_t _frame;
#line 170 "return_parameter_convert.m3"
_frame._unused=(ADDRESS)&_frame;
#line 170 "return_parameter_convert.m3"
 /* load */
#line 170 "return_parameter_convert.m3"
 /* exit_proc */
#line 170 "return_parameter_convert.m3"
return ((INT64)(a_L_242));
#line 170 "return_parameter_convert.m3"
 /* end_procedure */
#line 170 "return_parameter_convert.m3"
} /* ret_u32_u32 */
#line 170 "return_parameter_convert.m3"
 /* set_source_line */
#line 170 "return_parameter_convert.m3"
#line 171 "return_parameter_convert.m3"
 /* begin_procedure */
#line 171 "return_parameter_convert.m3"
struct return_parameter_convert__ret_u32_u32_Frame_t {
#line 171 "return_parameter_convert.m3"
ADDRESS _unused;
#line 171 "return_parameter_convert.m3"
};
#line 171 "return_parameter_convert.m3"
INTEGER
__cdecl
return_parameter_convert__ret_u32_u32(
   /* Param_Type1 */ return_parameter_convert__UINT32 a_L_244)
{
#line 171 "return_parameter_convert.m3"
return_parameter_convert__ret_u32_u32_Frame_t _frame;
#line 171 "return_parameter_convert.m3"
_frame._unused=(ADDRESS)&_frame;
#line 171 "return_parameter_convert.m3"
 /* load */
#line 171 "return_parameter_convert.m3"
 /* exit_proc */
#line 171 "return_parameter_convert.m3"
return ((INT64)(a_L_244));
#line 171 "return_parameter_convert.m3"
 /* end_procedure */
#line 171 "return_parameter_convert.m3"
} /* ret_u32_u8 */
#line 171 "return_parameter_convert.m3"
 /* set_source_line */
#line 171 "return_parameter_convert.m3"
#line 172 "return_parameter_convert.m3"
 /* begin_procedure */
#line 172 "return_parameter_convert.m3"
struct return_parameter_convert__ret_u32_u8_Frame_t {
#line 172 "return_parameter_convert.m3"
ADDRESS _unused;
#line 172 "return_parameter_convert.m3"
};
#line 172 "return_parameter_convert.m3"
INTEGER
__cdecl
return_parameter_convert__ret_u32_u8(
   /* Param_Type1 */ return_parameter_convert__UINT32 a_L_246)
{
#line 172 "return_parameter_convert.m3"
return_parameter_convert__ret_u32_u8_Frame_t _frame;
#line 172 "return_parameter_convert.m3"
_frame._unused=(ADDRESS)&_frame;
#line 172 "return_parameter_convert.m3"
 /* load */
#line 172 "return_parameter_convert.m3"
 /* exit_proc */
#line 172 "return_parameter_convert.m3"
return ((INT64)(a_L_246));
#line 172 "return_parameter_convert.m3"
 /* end_procedure */
#line 172 "return_parameter_convert.m3"
} /* ret_u32_L */
#line 172 "return_parameter_convert.m3"
 /* set_source_line */
#line 172 "return_parameter_convert.m3"
#line 173 "return_parameter_convert.m3"
 /* begin_procedure */
#line 173 "return_parameter_convert.m3"
struct return_parameter_convert__ret_u32_L_Frame_t {
#line 173 "return_parameter_convert.m3"
ADDRESS _unused;
#line 173 "return_parameter_convert.m3"
};
#line 173 "return_parameter_convert.m3"
LONGINT
__cdecl
return_parameter_convert__ret_u32_L(
   /* Param_Type1 */ return_parameter_convert__UINT32 a_L_248)
{
#line 173 "return_parameter_convert.m3"
return_parameter_convert__ret_u32_L_Frame_t _frame;
#line 173 "return_parameter_convert.m3"
_frame._unused=(ADDRESS)&_frame;
#line 173 "return_parameter_convert.m3"
 /* load */
#line 173 "return_parameter_convert.m3"
 /* loophole */
#line 173 "return_parameter_convert.m3"
 /* exit_proc */
#line 173 "return_parameter_convert.m3"
return (INT64)((INT64)(a_L_248));
#line 173 "return_parameter_convert.m3"
 /* end_procedure */
#line 173 "return_parameter_convert.m3"
} /* ret_u8_u64 */
#line 173 "return_parameter_convert.m3"
 /* set_source_line */
#line 173 "return_parameter_convert.m3"
#line 174 "return_parameter_convert.m3"
 /* begin_procedure */
#line 174 "return_parameter_convert.m3"
struct return_parameter_convert__ret_u8_u64_Frame_t {
#line 174 "return_parameter_convert.m3"
ADDRESS _unused;
#line 174 "return_parameter_convert.m3"
};
#line 174 "return_parameter_convert.m3"
LONGINT
__cdecl
return_parameter_convert__ret_u8_u64(
   /* Param_Type1 */ return_parameter_convert__UINT8 a_L_250)
{
#line 174 "return_parameter_convert.m3"
return_parameter_convert__ret_u8_u64_Frame_t _frame;
#line 174 "return_parameter_convert.m3"
_frame._unused=(ADDRESS)&_frame;
#line 174 "return_parameter_convert.m3"
 /* load */
#line 174 "return_parameter_convert.m3"
 /* loophole */
#line 174 "return_parameter_convert.m3"
 /* exit_proc */
#line 174 "return_parameter_convert.m3"
return (INT64)((INT64)(a_L_250));
#line 174 "return_parameter_convert.m3"
 /* end_procedure */
#line 174 "return_parameter_convert.m3"
} /* ret_u8_i8 */
#line 174 "return_parameter_convert.m3"
 /* set_source_line */
#line 174 "return_parameter_convert.m3"
#line 175 "return_parameter_convert.m3"
 /* begin_procedure */
#line 175 "return_parameter_convert.m3"
struct return_parameter_convert__ret_u8_i8_Frame_t {
#line 175 "return_parameter_convert.m3"
ADDRESS _unused;
#line 175 "return_parameter_convert.m3"
};
#line 175 "return_parameter_convert.m3"
INTEGER
__cdecl
return_parameter_convert__ret_u8_i8(
   /* Param_Type1 */ return_parameter_convert__UINT8 a_L_252)
{
#line 175 "return_parameter_convert.m3"
return_parameter_convert__ret_u8_i8_Frame_t _frame;
#line 175 "return_parameter_convert.m3"
_frame._unused=(ADDRESS)&_frame;
#line 175 "return_parameter_convert.m3"
 /* load */
#line 175 "return_parameter_convert.m3"
 /* exit_proc */
#line 175 "return_parameter_convert.m3"
return ((INT64)(a_L_252));
#line 175 "return_parameter_convert.m3"
 /* end_procedure */
#line 175 "return_parameter_convert.m3"
} /* ret_u8_i32 */
#line 175 "return_parameter_convert.m3"
 /* set_source_line */
#line 175 "return_parameter_convert.m3"
#line 176 "return_parameter_convert.m3"
 /* begin_procedure */
#line 176 "return_parameter_convert.m3"
struct return_parameter_convert__ret_u8_i32_Frame_t {
#line 176 "return_parameter_convert.m3"
ADDRESS _unused;
#line 176 "return_parameter_convert.m3"
};
#line 176 "return_parameter_convert.m3"
INTEGER
__cdecl
return_parameter_convert__ret_u8_i32(
   /* Param_Type1 */ return_parameter_convert__UINT8 a_L_254)
{
#line 176 "return_parameter_convert.m3"
return_parameter_convert__ret_u8_i32_Frame_t _frame;
#line 176 "return_parameter_convert.m3"
_frame._unused=(ADDRESS)&_frame;
#line 176 "return_parameter_convert.m3"
 /* load */
#line 176 "return_parameter_convert.m3"
 /* exit_proc */
#line 176 "return_parameter_convert.m3"
return ((INT64)(a_L_254));
#line 176 "return_parameter_convert.m3"
 /* end_procedure */
#line 176 "return_parameter_convert.m3"
} /* ret_u8_LC */
#line 176 "return_parameter_convert.m3"
 /* set_source_line */
#line 176 "return_parameter_convert.m3"
#line 177 "return_parameter_convert.m3"
 /* begin_procedure */
#line 177 "return_parameter_convert.m3"
struct return_parameter_convert__ret_u8_LC_Frame_t {
#line 177 "return_parameter_convert.m3"
ADDRESS _unused;
#line 177 "return_parameter_convert.m3"
};
#line 177 "return_parameter_convert.m3"
LONGINT
__cdecl
return_parameter_convert__ret_u8_LC(
   /* Param_Type1 */ return_parameter_convert__UINT8 a_L_256)
{
#line 177 "return_parameter_convert.m3"
return_parameter_convert__ret_u8_LC_Frame_t _frame;
#line 177 "return_parameter_convert.m3"
_frame._unused=(ADDRESS)&_frame;
#line 177 "return_parameter_convert.m3"
 /* load */
#line 177 "return_parameter_convert.m3"
 /* loophole */
#line 177 "return_parameter_convert.m3"
 /* exit_proc */
#line 177 "return_parameter_convert.m3"
return (INT64)((INT64)(a_L_256));
#line 177 "return_parameter_convert.m3"
 /* end_procedure */
#line 177 "return_parameter_convert.m3"
} /* ret_u8_u16 */
#line 177 "return_parameter_convert.m3"
 /* set_source_line */
#line 177 "return_parameter_convert.m3"
#line 178 "return_parameter_convert.m3"
 /* begin_procedure */
#line 178 "return_parameter_convert.m3"
struct return_parameter_convert__ret_u8_u16_Frame_t {
#line 178 "return_parameter_convert.m3"
ADDRESS _unused;
#line 178 "return_parameter_convert.m3"
};
#line 178 "return_parameter_convert.m3"
INTEGER
__cdecl
return_parameter_convert__ret_u8_u16(
   /* Param_Type1 */ return_parameter_convert__UINT8 a_L_258)
{
#line 178 "return_parameter_convert.m3"
return_parameter_convert__ret_u8_u16_Frame_t _frame;
#line 178 "return_parameter_convert.m3"
_frame._unused=(ADDRESS)&_frame;
#line 178 "return_parameter_convert.m3"
 /* load */
#line 178 "return_parameter_convert.m3"
 /* exit_proc */
#line 178 "return_parameter_convert.m3"
return ((INT64)(a_L_258));
#line 178 "return_parameter_convert.m3"
 /* end_procedure */
#line 178 "return_parameter_convert.m3"
} /* ret_u8_I */
#line 178 "return_parameter_convert.m3"
 /* set_source_line */
#line 178 "return_parameter_convert.m3"
#line 179 "return_parameter_convert.m3"
 /* begin_procedure */
#line 179 "return_parameter_convert.m3"
struct return_parameter_convert__ret_u8_I_Frame_t {
#line 179 "return_parameter_convert.m3"
ADDRESS _unused;
#line 179 "return_parameter_convert.m3"
};
#line 179 "return_parameter_convert.m3"
INTEGER
__cdecl
return_parameter_convert__ret_u8_I(
   /* Param_Type1 */ return_parameter_convert__UINT8 a_L_260)
{
#line 179 "return_parameter_convert.m3"
return_parameter_convert__ret_u8_I_Frame_t _frame;
#line 179 "return_parameter_convert.m3"
_frame._unused=(ADDRESS)&_frame;
#line 179 "return_parameter_convert.m3"
 /* load */
#line 179 "return_parameter_convert.m3"
 /* exit_proc */
#line 179 "return_parameter_convert.m3"
return ((INT64)(a_L_260));
#line 179 "return_parameter_convert.m3"
 /* end_procedure */
#line 179 "return_parameter_convert.m3"
} /* ret_u8_i64 */
#line 179 "return_parameter_convert.m3"
 /* set_source_line */
#line 179 "return_parameter_convert.m3"
#line 180 "return_parameter_convert.m3"
 /* begin_procedure */
#line 180 "return_parameter_convert.m3"
struct return_parameter_convert__ret_u8_i64_Frame_t {
#line 180 "return_parameter_convert.m3"
ADDRESS _unused;
#line 180 "return_parameter_convert.m3"
};
#line 180 "return_parameter_convert.m3"
LONGINT
__cdecl
return_parameter_convert__ret_u8_i64(
   /* Param_Type1 */ return_parameter_convert__UINT8 a_L_262)
{
#line 180 "return_parameter_convert.m3"
return_parameter_convert__ret_u8_i64_Frame_t _frame;
#line 180 "return_parameter_convert.m3"
_frame._unused=(ADDRESS)&_frame;
#line 180 "return_parameter_convert.m3"
 /* load */
#line 180 "return_parameter_convert.m3"
 /* loophole */
#line 180 "return_parameter_convert.m3"
 /* exit_proc */
#line 180 "return_parameter_convert.m3"
return (INT64)((INT64)(a_L_262));
#line 180 "return_parameter_convert.m3"
 /* end_procedure */
#line 180 "return_parameter_convert.m3"
} /* ret_u8_C */
#line 180 "return_parameter_convert.m3"
 /* set_source_line */
#line 180 "return_parameter_convert.m3"
#line 181 "return_parameter_convert.m3"
 /* begin_procedure */
#line 181 "return_parameter_convert.m3"
struct return_parameter_convert__ret_u8_C_Frame_t {
#line 181 "return_parameter_convert.m3"
ADDRESS _unused;
#line 181 "return_parameter_convert.m3"
};
#line 181 "return_parameter_convert.m3"
INTEGER
__cdecl
return_parameter_convert__ret_u8_C(
   /* Param_Type1 */ return_parameter_convert__UINT8 a_L_264)
{
#line 181 "return_parameter_convert.m3"
return_parameter_convert__ret_u8_C_Frame_t _frame;
#line 181 "return_parameter_convert.m3"
_frame._unused=(ADDRESS)&_frame;
#line 181 "return_parameter_convert.m3"
 /* load */
#line 181 "return_parameter_convert.m3"
 /* exit_proc */
#line 181 "return_parameter_convert.m3"
return ((INT64)(a_L_264));
#line 181 "return_parameter_convert.m3"
 /* end_procedure */
#line 181 "return_parameter_convert.m3"
} /* ret_u8_i16 */
#line 181 "return_parameter_convert.m3"
 /* set_source_line */
#line 181 "return_parameter_convert.m3"
#line 182 "return_parameter_convert.m3"
 /* begin_procedure */
#line 182 "return_parameter_convert.m3"
struct return_parameter_convert__ret_u8_i16_Frame_t {
#line 182 "return_parameter_convert.m3"
ADDRESS _unused;
#line 182 "return_parameter_convert.m3"
};
#line 182 "return_parameter_convert.m3"
INTEGER
__cdecl
return_parameter_convert__ret_u8_i16(
   /* Param_Type1 */ return_parameter_convert__UINT8 a_L_266)
{
#line 182 "return_parameter_convert.m3"
return_parameter_convert__ret_u8_i16_Frame_t _frame;
#line 182 "return_parameter_convert.m3"
_frame._unused=(ADDRESS)&_frame;
#line 182 "return_parameter_convert.m3"
 /* load */
#line 182 "return_parameter_convert.m3"
 /* exit_proc */
#line 182 "return_parameter_convert.m3"
return ((INT64)(a_L_266));
#line 182 "return_parameter_convert.m3"
 /* end_procedure */
#line 182 "return_parameter_convert.m3"
} /* ret_u8_u32 */
#line 182 "return_parameter_convert.m3"
 /* set_source_line */
#line 182 "return_parameter_convert.m3"
#line 183 "return_parameter_convert.m3"
 /* begin_procedure */
#line 183 "return_parameter_convert.m3"
struct return_parameter_convert__ret_u8_u32_Frame_t {
#line 183 "return_parameter_convert.m3"
ADDRESS _unused;
#line 183 "return_parameter_convert.m3"
};
#line 183 "return_parameter_convert.m3"
INTEGER
__cdecl
return_parameter_convert__ret_u8_u32(
   /* Param_Type1 */ return_parameter_convert__UINT8 a_L_268)
{
#line 183 "return_parameter_convert.m3"
return_parameter_convert__ret_u8_u32_Frame_t _frame;
#line 183 "return_parameter_convert.m3"
_frame._unused=(ADDRESS)&_frame;
#line 183 "return_parameter_convert.m3"
 /* load */
#line 183 "return_parameter_convert.m3"
 /* exit_proc */
#line 183 "return_parameter_convert.m3"
return ((INT64)(a_L_268));
#line 183 "return_parameter_convert.m3"
 /* end_procedure */
#line 183 "return_parameter_convert.m3"
} /* ret_u8_u8 */
#line 183 "return_parameter_convert.m3"
 /* set_source_line */
#line 183 "return_parameter_convert.m3"
#line 184 "return_parameter_convert.m3"
 /* begin_procedure */
#line 184 "return_parameter_convert.m3"
struct return_parameter_convert__ret_u8_u8_Frame_t {
#line 184 "return_parameter_convert.m3"
ADDRESS _unused;
#line 184 "return_parameter_convert.m3"
};
#line 184 "return_parameter_convert.m3"
INTEGER
__cdecl
return_parameter_convert__ret_u8_u8(
   /* Param_Type1 */ return_parameter_convert__UINT8 a_L_270)
{
#line 184 "return_parameter_convert.m3"
return_parameter_convert__ret_u8_u8_Frame_t _frame;
#line 184 "return_parameter_convert.m3"
_frame._unused=(ADDRESS)&_frame;
#line 184 "return_parameter_convert.m3"
 /* load */
#line 184 "return_parameter_convert.m3"
 /* exit_proc */
#line 184 "return_parameter_convert.m3"
return ((INT64)(a_L_270));
#line 184 "return_parameter_convert.m3"
 /* end_procedure */
#line 184 "return_parameter_convert.m3"
} /* ret_u8_L */
#line 184 "return_parameter_convert.m3"
 /* set_source_line */
#line 184 "return_parameter_convert.m3"
#line 185 "return_parameter_convert.m3"
 /* begin_procedure */
#line 185 "return_parameter_convert.m3"
struct return_parameter_convert__ret_u8_L_Frame_t {
#line 185 "return_parameter_convert.m3"
ADDRESS _unused;
#line 185 "return_parameter_convert.m3"
};
#line 185 "return_parameter_convert.m3"
LONGINT
__cdecl
return_parameter_convert__ret_u8_L(
   /* Param_Type1 */ return_parameter_convert__UINT8 a_L_272)
{
#line 185 "return_parameter_convert.m3"
return_parameter_convert__ret_u8_L_Frame_t _frame;
#line 185 "return_parameter_convert.m3"
_frame._unused=(ADDRESS)&_frame;
#line 185 "return_parameter_convert.m3"
 /* load */
#line 185 "return_parameter_convert.m3"
 /* loophole */
#line 185 "return_parameter_convert.m3"
 /* exit_proc */
#line 185 "return_parameter_convert.m3"
return (INT64)((INT64)(a_L_272));
#line 185 "return_parameter_convert.m3"
 /* end_procedure */
#line 185 "return_parameter_convert.m3"
} /* ret_L_u64 */
#line 185 "return_parameter_convert.m3"
 /* set_source_line */
#line 185 "return_parameter_convert.m3"
#line 186 "return_parameter_convert.m3"
 /* begin_procedure */
#line 186 "return_parameter_convert.m3"
struct return_parameter_convert__ret_L_u64_Frame_t {
#line 186 "return_parameter_convert.m3"
ADDRESS _unused;
#line 186 "return_parameter_convert.m3"
};
#line 186 "return_parameter_convert.m3"
LONGINT
__cdecl
return_parameter_convert__ret_L_u64(
   /* Param_Type1 */ LONGINT a_L_274)
{
#line 186 "return_parameter_convert.m3"
return_parameter_convert__ret_L_u64_Frame_t _frame;
#line 186 "return_parameter_convert.m3"
_frame._unused=(ADDRESS)&_frame;
#line 186 "return_parameter_convert.m3"
 /* load */
#line 186 "return_parameter_convert.m3"
 /* exit_proc */
#line 186 "return_parameter_convert.m3"
return a_L_274;
#line 186 "return_parameter_convert.m3"
 /* end_procedure */
#line 186 "return_parameter_convert.m3"
} /* ret_L_i8 */
#line 186 "return_parameter_convert.m3"
 /* set_source_line */
#line 186 "return_parameter_convert.m3"
#line 187 "return_parameter_convert.m3"
 /* begin_procedure */
#line 187 "return_parameter_convert.m3"
struct return_parameter_convert__ret_L_i8_Frame_t {
#line 187 "return_parameter_convert.m3"
ADDRESS _unused;
#line 187 "return_parameter_convert.m3"
};
#line 187 "return_parameter_convert.m3"
LONGINT
__cdecl
return_parameter_convert__ret_L_i8(
   /* Param_Type1 */ LONGINT a_L_276)
{
#line 187 "return_parameter_convert.m3"
return_parameter_convert__ret_L_i8_Frame_t _frame;
#line 187 "return_parameter_convert.m3"
_frame._unused=(ADDRESS)&_frame;
#line 187 "return_parameter_convert.m3"
 /* load */
#line 187 "return_parameter_convert.m3"
 /* exit_proc */
#line 187 "return_parameter_convert.m3"
return a_L_276;
#line 187 "return_parameter_convert.m3"
 /* end_procedure */
#line 187 "return_parameter_convert.m3"
} /* ret_L_i32 */
#line 187 "return_parameter_convert.m3"
 /* set_source_line */
#line 187 "return_parameter_convert.m3"
#line 188 "return_parameter_convert.m3"
 /* begin_procedure */
#line 188 "return_parameter_convert.m3"
struct return_parameter_convert__ret_L_i32_Frame_t {
#line 188 "return_parameter_convert.m3"
ADDRESS _unused;
#line 188 "return_parameter_convert.m3"
};
#line 188 "return_parameter_convert.m3"
LONGINT
__cdecl
return_parameter_convert__ret_L_i32(
   /* Param_Type1 */ LONGINT a_L_278)
{
#line 188 "return_parameter_convert.m3"
return_parameter_convert__ret_L_i32_Frame_t _frame;
#line 188 "return_parameter_convert.m3"
_frame._unused=(ADDRESS)&_frame;
#line 188 "return_parameter_convert.m3"
 /* load */
#line 188 "return_parameter_convert.m3"
 /* exit_proc */
#line 188 "return_parameter_convert.m3"
return a_L_278;
#line 188 "return_parameter_convert.m3"
 /* end_procedure */
#line 188 "return_parameter_convert.m3"
} /* ret_L_LC */
#line 188 "return_parameter_convert.m3"
 /* set_source_line */
#line 188 "return_parameter_convert.m3"
#line 189 "return_parameter_convert.m3"
 /* begin_procedure */
#line 189 "return_parameter_convert.m3"
struct return_parameter_convert__ret_L_LC_Frame_t {
#line 189 "return_parameter_convert.m3"
ADDRESS _unused;
#line 189 "return_parameter_convert.m3"
};
#line 189 "return_parameter_convert.m3"
LONGINT
__cdecl
return_parameter_convert__ret_L_LC(
   /* Param_Type1 */ LONGINT a_L_280)
{
#line 189 "return_parameter_convert.m3"
return_parameter_convert__ret_L_LC_Frame_t _frame;
#line 189 "return_parameter_convert.m3"
_frame._unused=(ADDRESS)&_frame;
#line 189 "return_parameter_convert.m3"
 /* load */
#line 189 "return_parameter_convert.m3"
 /* exit_proc */
#line 189 "return_parameter_convert.m3"
return a_L_280;
#line 189 "return_parameter_convert.m3"
 /* end_procedure */
#line 189 "return_parameter_convert.m3"
} /* ret_L_u16 */
#line 189 "return_parameter_convert.m3"
 /* set_source_line */
#line 189 "return_parameter_convert.m3"
#line 190 "return_parameter_convert.m3"
 /* begin_procedure */
#line 190 "return_parameter_convert.m3"
struct return_parameter_convert__ret_L_u16_Frame_t {
#line 190 "return_parameter_convert.m3"
ADDRESS _unused;
#line 190 "return_parameter_convert.m3"
};
#line 190 "return_parameter_convert.m3"
LONGINT
__cdecl
return_parameter_convert__ret_L_u16(
   /* Param_Type1 */ LONGINT a_L_282)
{
#line 190 "return_parameter_convert.m3"
return_parameter_convert__ret_L_u16_Frame_t _frame;
#line 190 "return_parameter_convert.m3"
_frame._unused=(ADDRESS)&_frame;
#line 190 "return_parameter_convert.m3"
 /* load */
#line 190 "return_parameter_convert.m3"
 /* exit_proc */
#line 190 "return_parameter_convert.m3"
return a_L_282;
#line 190 "return_parameter_convert.m3"
 /* end_procedure */
#line 190 "return_parameter_convert.m3"
} /* ret_L_I */
#line 190 "return_parameter_convert.m3"
 /* set_source_line */
#line 190 "return_parameter_convert.m3"
#line 191 "return_parameter_convert.m3"
 /* begin_procedure */
#line 191 "return_parameter_convert.m3"
struct return_parameter_convert__ret_L_I_Frame_t {
#line 191 "return_parameter_convert.m3"
ADDRESS _unused;
#line 191 "return_parameter_convert.m3"
};
#line 191 "return_parameter_convert.m3"
LONGINT
__cdecl
return_parameter_convert__ret_L_I(
   /* Param_Type1 */ LONGINT a_L_284)
{
#line 191 "return_parameter_convert.m3"
return_parameter_convert__ret_L_I_Frame_t _frame;
#line 191 "return_parameter_convert.m3"
_frame._unused=(ADDRESS)&_frame;
#line 191 "return_parameter_convert.m3"
 /* load */
#line 191 "return_parameter_convert.m3"
 /* exit_proc */
#line 191 "return_parameter_convert.m3"
return a_L_284;
#line 191 "return_parameter_convert.m3"
 /* end_procedure */
#line 191 "return_parameter_convert.m3"
} /* ret_L_i64 */
#line 191 "return_parameter_convert.m3"
 /* set_source_line */
#line 191 "return_parameter_convert.m3"
#line 192 "return_parameter_convert.m3"
 /* begin_procedure */
#line 192 "return_parameter_convert.m3"
struct return_parameter_convert__ret_L_i64_Frame_t {
#line 192 "return_parameter_convert.m3"
ADDRESS _unused;
#line 192 "return_parameter_convert.m3"
};
#line 192 "return_parameter_convert.m3"
LONGINT
__cdecl
return_parameter_convert__ret_L_i64(
   /* Param_Type1 */ LONGINT a_L_286)
{
#line 192 "return_parameter_convert.m3"
return_parameter_convert__ret_L_i64_Frame_t _frame;
#line 192 "return_parameter_convert.m3"
_frame._unused=(ADDRESS)&_frame;
#line 192 "return_parameter_convert.m3"
 /* load */
#line 192 "return_parameter_convert.m3"
 /* exit_proc */
#line 192 "return_parameter_convert.m3"
return a_L_286;
#line 192 "return_parameter_convert.m3"
 /* end_procedure */
#line 192 "return_parameter_convert.m3"
} /* ret_L_C */
#line 192 "return_parameter_convert.m3"
 /* set_source_line */
#line 192 "return_parameter_convert.m3"
#line 193 "return_parameter_convert.m3"
 /* begin_procedure */
#line 193 "return_parameter_convert.m3"
struct return_parameter_convert__ret_L_C_Frame_t {
#line 193 "return_parameter_convert.m3"
ADDRESS _unused;
#line 193 "return_parameter_convert.m3"
};
#line 193 "return_parameter_convert.m3"
LONGINT
__cdecl
return_parameter_convert__ret_L_C(
   /* Param_Type1 */ LONGINT a_L_288)
{
#line 193 "return_parameter_convert.m3"
return_parameter_convert__ret_L_C_Frame_t _frame;
#line 193 "return_parameter_convert.m3"
_frame._unused=(ADDRESS)&_frame;
#line 193 "return_parameter_convert.m3"
 /* load */
#line 193 "return_parameter_convert.m3"
 /* exit_proc */
#line 193 "return_parameter_convert.m3"
return a_L_288;
#line 193 "return_parameter_convert.m3"
 /* end_procedure */
#line 193 "return_parameter_convert.m3"
} /* ret_L_i16 */
#line 193 "return_parameter_convert.m3"
 /* set_source_line */
#line 193 "return_parameter_convert.m3"
#line 194 "return_parameter_convert.m3"
 /* begin_procedure */
#line 194 "return_parameter_convert.m3"
struct return_parameter_convert__ret_L_i16_Frame_t {
#line 194 "return_parameter_convert.m3"
ADDRESS _unused;
#line 194 "return_parameter_convert.m3"
};
#line 194 "return_parameter_convert.m3"
LONGINT
__cdecl
return_parameter_convert__ret_L_i16(
   /* Param_Type1 */ LONGINT a_L_290)
{
#line 194 "return_parameter_convert.m3"
return_parameter_convert__ret_L_i16_Frame_t _frame;
#line 194 "return_parameter_convert.m3"
_frame._unused=(ADDRESS)&_frame;
#line 194 "return_parameter_convert.m3"
 /* load */
#line 194 "return_parameter_convert.m3"
 /* exit_proc */
#line 194 "return_parameter_convert.m3"
return a_L_290;
#line 194 "return_parameter_convert.m3"
 /* end_procedure */
#line 194 "return_parameter_convert.m3"
} /* ret_L_u32 */
#line 194 "return_parameter_convert.m3"
 /* set_source_line */
#line 194 "return_parameter_convert.m3"
#line 195 "return_parameter_convert.m3"
 /* begin_procedure */
#line 195 "return_parameter_convert.m3"
struct return_parameter_convert__ret_L_u32_Frame_t {
#line 195 "return_parameter_convert.m3"
ADDRESS _unused;
#line 195 "return_parameter_convert.m3"
};
#line 195 "return_parameter_convert.m3"
LONGINT
__cdecl
return_parameter_convert__ret_L_u32(
   /* Param_Type1 */ LONGINT a_L_292)
{
#line 195 "return_parameter_convert.m3"
return_parameter_convert__ret_L_u32_Frame_t _frame;
#line 195 "return_parameter_convert.m3"
_frame._unused=(ADDRESS)&_frame;
#line 195 "return_parameter_convert.m3"
 /* load */
#line 195 "return_parameter_convert.m3"
 /* exit_proc */
#line 195 "return_parameter_convert.m3"
return a_L_292;
#line 195 "return_parameter_convert.m3"
 /* end_procedure */
#line 195 "return_parameter_convert.m3"
} /* ret_L_u8 */
#line 195 "return_parameter_convert.m3"
 /* set_source_line */
#line 195 "return_parameter_convert.m3"
#line 196 "return_parameter_convert.m3"
 /* begin_procedure */
#line 196 "return_parameter_convert.m3"
struct return_parameter_convert__ret_L_u8_Frame_t {
#line 196 "return_parameter_convert.m3"
ADDRESS _unused;
#line 196 "return_parameter_convert.m3"
};
#line 196 "return_parameter_convert.m3"
LONGINT
__cdecl
return_parameter_convert__ret_L_u8(
   /* Param_Type1 */ LONGINT a_L_294)
{
#line 196 "return_parameter_convert.m3"
return_parameter_convert__ret_L_u8_Frame_t _frame;
#line 196 "return_parameter_convert.m3"
_frame._unused=(ADDRESS)&_frame;
#line 196 "return_parameter_convert.m3"
 /* load */
#line 196 "return_parameter_convert.m3"
 /* exit_proc */
#line 196 "return_parameter_convert.m3"
return a_L_294;
#line 196 "return_parameter_convert.m3"
 /* end_procedure */
#line 196 "return_parameter_convert.m3"
} /* ret_L_L */
#line 196 "return_parameter_convert.m3"
 /* set_source_line */
#line 196 "return_parameter_convert.m3"
#line 197 "return_parameter_convert.m3"
 /* begin_procedure */
#line 197 "return_parameter_convert.m3"
struct return_parameter_convert__ret_L_L_Frame_t {
#line 197 "return_parameter_convert.m3"
ADDRESS _unused;
#line 197 "return_parameter_convert.m3"
};
#line 197 "return_parameter_convert.m3"
LONGINT
__cdecl
return_parameter_convert__ret_L_L(
   /* Param_Type1 */ LONGINT a_L_296)
{
#line 197 "return_parameter_convert.m3"
return_parameter_convert__ret_L_L_Frame_t _frame;
#line 197 "return_parameter_convert.m3"
_frame._unused=(ADDRESS)&_frame;
#line 197 "return_parameter_convert.m3"
 /* load */
#line 197 "return_parameter_convert.m3"
 /* exit_proc */
#line 197 "return_parameter_convert.m3"
return a_L_296;
#line 197 "return_parameter_convert.m3"
 /* end_procedure */
#line 197 "return_parameter_convert.m3"
} /* return_parameter_convert_M3 */
#line 197 "return_parameter_convert.m3"
 /* module main body return_parameter_convert_M3 */
#line 197 "return_parameter_convert.m3"
 /* set_source_line */
#line 197 "return_parameter_convert.m3"
#line 198 "return_parameter_convert.m3"
 /* begin_procedure */
#line 198 "return_parameter_convert.m3"
struct return_parameter_convert_M3_Frame_t {
#line 198 "return_parameter_convert.m3"
ADDRESS _unused;
#line 198 "return_parameter_convert.m3"
};
#line 198 "return_parameter_convert.m3"
RT0__ModulePtr
__cdecl
return_parameter_convert_M3(
   /* Param_Type1 */ INTEGER /*TypeText1*/  mode_L_8)
{
#line 198 "return_parameter_convert.m3"
return_parameter_convert_M3_Frame_t _frame;
#line 198 "return_parameter_convert.m3"
_frame._unused=(ADDRESS)&_frame;
#line 198 "return_parameter_convert.m3"
 /* load */
#line 198 "return_parameter_convert.m3"
 /* if_true_or_false */
#line 198 "return_parameter_convert.m3"
 /* load_host_integer */
#line 198 "return_parameter_convert.m3"
 /* load_integer */
#line 198 "return_parameter_convert.m3"
 /* if_compare */
#line 198 "return_parameter_convert.m3"
if(m3_eq(INT64,
  mode_L_8,
   INT64_(0)))goto L1;
#line 198 "return_parameter_convert.m3"
 /* set_label */
#line 198 "return_parameter_convert.m3"
L1:;
#line 198 "return_parameter_convert.m3"
 /* load_address */
#line 198 "return_parameter_convert.m3"
 /* exit_proc */
#line 198 "return_parameter_convert.m3"
return (RT0__ModulePtr)(&return_parameter_convert_m_M_return_parameter_convert_L_7);
#line 198 "return_parameter_convert.m3"
 /* end_procedure */
#line 198 "return_parameter_convert.m3"
} /* global constant type descriptor */
#line 198 "return_parameter_convert.m3"
 /* global data type descriptor */
#line 198 "return_parameter_convert.m3"
 /* module global constants */
#line 198 "return_parameter_convert.m3"
 /* procedure names */
#line 198 "return_parameter_convert.m3"
 /* procedure table */
#line 198 "return_parameter_convert.m3"
 /* file name */
#line 198 "return_parameter_convert.m3"
 /* module global data */
#line 198 "return_parameter_convert.m3"
 /* load map


 global data allocation for M_return_parameter_convert
     0   104  8  *module info*
   104     1  1  return_parameter_convert.vi8
   112     8  8  return_parameter_convert.vu64
   120     8  8  return_parameter_convert.vf64
   128     4  4  return_parameter_convert.vi32
   136     8  8  return_parameter_convert.vLC
   144     2  2  return_parameter_convert.vu16
   152     8  8  return_parameter_convert.vI
   160     8  8  return_parameter_convert.vi64
   168     4  4  return_parameter_convert.vf32
   172     2  2  return_parameter_convert.vi16
   176     8  8  return_parameter_convert.vC
   184     4  4  return_parameter_convert.vu32
   188     1  1  return_parameter_convert.vu8
   192     8  8  return_parameter_convert.vL
   200     8  8  return_parameter_convert.offset
   208     8  8  return_parameter_convert.count
   216    24  8  import return_parameter_convert
   240    24  8  import Long
   264    24  8  import Word
   288    24  8  import Cstdint
   312    24  8  import RTHooks
   336     0  8  *TOTAL*


 global constants for M_return_parameter_convert
     0  1540  8  *proc names*
  1544  2328  8  *proc info*
  3872    28  1  *string*
  3904     0  8  *TOTAL*
 */
#line 198 "return_parameter_convert.m3"
 /* end unit */
#line 198 "return_parameter_convert.m3"

#ifdef __cplusplus

} /* extern "C" */
#endif
 /* set_runtime_proc */
 /* set_runtime_proc */
 /* set_runtime_proc */
