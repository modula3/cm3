// library:pgm
// source_base_name:return_constant
// target_name:return_constant.m3.cpp
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
T66A2A904_8(__cdecl*T2FD24D9D)(void);
#else
typedef void (__cdecl*T2FD24D9D)(void);
#endif
 /* declare_proctype */

#if 0 /* avoid type hash collions */
typedef 
INT64(__cdecl*TBF2A8E93)(void);
#else
typedef void (__cdecl*TBF2A8E93)(void);
#endif
 /* declare_proctype */

#if 0 /* avoid type hash collions */
typedef 
double(__cdecl*TE4E28466)(void);
#else
typedef void (__cdecl*TE4E28466)(void);
#endif
 /* declare_proctype */

#if 0 /* avoid type hash collions */
typedef 
TADC6066D_32(__cdecl*TFF82092F)(void);
#else
typedef void (__cdecl*TFF82092F)(void);
#endif
 /* declare_subrange */
/*subrange_define*/typedef INT64 T9CED36E7_64;
 /* declare_proctype */

#if 0 /* avoid type hash collions */
typedef 
T9CED36E7_64(__cdecl*T8B63E74A)(void);
#else
typedef void (__cdecl*T8B63E74A)(void);
#endif
 /* declare_proctype */

#if 0 /* avoid type hash collions */
typedef 
TA4B285DE_16(__cdecl*TFD07BB9B)(void);
#else
typedef void (__cdecl*TFD07BB9B)(void);
#endif
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
T839F750E_64(__cdecl*T9E7E2EF7)(void);
#else
typedef void (__cdecl*T9E7E2EF7)(void);
#endif
 /* declare_proctype */

#if 0 /* avoid type hash collions */
typedef 
float(__cdecl*T263D7EB0)(void);
#else
typedef void (__cdecl*T263D7EB0)(void);
#endif
 /* declare_proctype */

#if 0 /* avoid type hash collions */
typedef 
T7300E1E8_16(__cdecl*T3F787B7)(void);
#else
typedef void (__cdecl*T3F787B7)(void);
#endif
 /* declare_proctype */

#if 0 /* avoid type hash collions */
typedef 
WORD_T(__cdecl*T5C4C299E)(void);
#else
typedef void (__cdecl*T5C4C299E)(void);
#endif
 /* declare_proctype */

#if 0 /* avoid type hash collions */
typedef 
T6FA2E87D_32(__cdecl*TB705F362)(void);
#else
typedef void (__cdecl*TB705F362)(void);
#endif
 /* declare_proctype */

#if 0 /* avoid type hash collions */
typedef 
TB5B30AA_8(__cdecl*T38BAD830)(void);
#else
typedef void (__cdecl*T38BAD830)(void);
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
/*Proc_ForwardDeclareFrameType*/struct return_constant_I3_Frame_t;typedef struct return_constant_I3_Frame_t return_constant_I3_Frame_t;
 /* internal_declare_param */
RT0__ModulePtr
__cdecl
return_constant_I3(
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
/*declare_segment*/struct return_constant_m_5_L_6_t;
/*declare_segment*/typedef struct return_constant_m_5_L_6_t return_constant_m_5_L_6_t;
 /* declare_segment name:M_return_constant typeid:TFFFFFFFF const:FALSE */
 /* handler_name_prefixes:return_constant_M3_LINE_ */
 /* handler_name_prefixes:return_constant_I3_LINE_ */
/*declare_segment*/struct return_constant_m_M_return_constant_L_7_t;
/*declare_segment*/typedef struct return_constant_m_M_return_constant_L_7_t return_constant_m_M_return_constant_L_7_t;
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct return_constant_M3_Frame_t;typedef struct return_constant_M3_Frame_t return_constant_M3_Frame_t;
 /* internal_declare_param */
RT0__ModulePtr
__cdecl
return_constant_M3(
   /* Param_Type1 */ INTEGER /*TypeText1*/  mode_L_8);
 /* declare_procedure */

#ifndef return_constant__INT8
#define return_constant__INT8 return_constant__INT8
typedef T66A2A904_8 /*TypeText1*/  return_constant__INT8;
#endif
/*Proc_ForwardDeclareFrameType*/struct return_constant__ret_ki8_Frame_t;typedef struct return_constant__ret_ki8_Frame_t return_constant__ret_ki8_Frame_t;
return_constant__INT8
__cdecl
return_constant__ret_ki8(void);
 /* declare_local */
 /* declare_procedure */

#ifndef return_constant__UINT64
#define return_constant__UINT64 return_constant__UINT64
typedef INT64 /*TypeText1*/  return_constant__UINT64;
#endif
/*Proc_ForwardDeclareFrameType*/struct return_constant__ret_ku64_Frame_t;typedef struct return_constant__ret_ku64_Frame_t return_constant__ret_ku64_Frame_t;
return_constant__UINT64
__cdecl
return_constant__ret_ku64(void);
 /* declare_local */
 /* declare_procedure */

#ifndef return_constant__FLOAT64
#define return_constant__FLOAT64 return_constant__FLOAT64
typedef double /*TypeText1*/  return_constant__FLOAT64;
#endif
/*Proc_ForwardDeclareFrameType*/struct return_constant__ret_kf64_Frame_t;typedef struct return_constant__ret_kf64_Frame_t return_constant__ret_kf64_Frame_t;
return_constant__FLOAT64
__cdecl
return_constant__ret_kf64(void);
 /* declare_local */
 /* declare_procedure */

#ifndef return_constant__INT32
#define return_constant__INT32 return_constant__INT32
typedef TADC6066D_32 /*TypeText1*/  return_constant__INT32;
#endif
/*Proc_ForwardDeclareFrameType*/struct return_constant__ret_ki32_Frame_t;typedef struct return_constant__ret_ki32_Frame_t return_constant__ret_ki32_Frame_t;
return_constant__INT32
__cdecl
return_constant__ret_ki32(void);
 /* declare_local */
 /* declare_procedure */

#ifndef LONGCARD
#define LONGCARD LONGCARD
typedef T9CED36E7_64 /*TypeText1*/  LONGCARD;
#endif
/*Proc_ForwardDeclareFrameType*/struct return_constant__ret_kLC_Frame_t;typedef struct return_constant__ret_kLC_Frame_t return_constant__ret_kLC_Frame_t;
LONGCARD
__cdecl
return_constant__ret_kLC(void);
 /* declare_local */
 /* declare_procedure */

#ifndef return_constant__UINT16
#define return_constant__UINT16 return_constant__UINT16
typedef TA4B285DE_16 /*TypeText1*/  return_constant__UINT16;
#endif
/*Proc_ForwardDeclareFrameType*/struct return_constant__ret_ku16_Frame_t;typedef struct return_constant__ret_ku16_Frame_t return_constant__ret_ku16_Frame_t;
return_constant__UINT16
__cdecl
return_constant__ret_ku16(void);
 /* declare_local */
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct return_constant__ret_kI_Frame_t;typedef struct return_constant__ret_kI_Frame_t return_constant__ret_kI_Frame_t;
INTEGER
__cdecl
return_constant__ret_kI(void);
 /* declare_local */
 /* declare_procedure */

#ifndef return_constant__INT64
#define return_constant__INT64 return_constant__INT64
typedef T839F750E_64 /*TypeText1*/  return_constant__INT64;
#endif
/*Proc_ForwardDeclareFrameType*/struct return_constant__ret_ki64_Frame_t;typedef struct return_constant__ret_ki64_Frame_t return_constant__ret_ki64_Frame_t;
return_constant__INT64
__cdecl
return_constant__ret_ki64(void);
 /* declare_local */
 /* declare_procedure */

#ifndef return_constant__FLOAT32
#define return_constant__FLOAT32 return_constant__FLOAT32
typedef float /*TypeText1*/  return_constant__FLOAT32;
#endif
/*Proc_ForwardDeclareFrameType*/struct return_constant__ret_kf32_Frame_t;typedef struct return_constant__ret_kf32_Frame_t return_constant__ret_kf32_Frame_t;
return_constant__FLOAT32
__cdecl
return_constant__ret_kf32(void);
 /* declare_local */
 /* declare_procedure */

#ifndef return_constant__INT16
#define return_constant__INT16 return_constant__INT16
typedef T7300E1E8_16 /*TypeText1*/  return_constant__INT16;
#endif
/*Proc_ForwardDeclareFrameType*/struct return_constant__ret_ki16_Frame_t;typedef struct return_constant__ret_ki16_Frame_t return_constant__ret_ki16_Frame_t;
return_constant__INT16
__cdecl
return_constant__ret_ki16(void);
 /* declare_local */
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct return_constant__ret_kC_Frame_t;typedef struct return_constant__ret_kC_Frame_t return_constant__ret_kC_Frame_t;
CARDINAL
__cdecl
return_constant__ret_kC(void);
 /* declare_local */
 /* declare_procedure */

#ifndef return_constant__UINT32
#define return_constant__UINT32 return_constant__UINT32
typedef T6FA2E87D_32 /*TypeText1*/  return_constant__UINT32;
#endif
/*Proc_ForwardDeclareFrameType*/struct return_constant__ret_ku32_Frame_t;typedef struct return_constant__ret_ku32_Frame_t return_constant__ret_ku32_Frame_t;
return_constant__UINT32
__cdecl
return_constant__ret_ku32(void);
 /* declare_local */
 /* declare_procedure */

#ifndef return_constant__UINT8
#define return_constant__UINT8 return_constant__UINT8
typedef TB5B30AA_8 /*TypeText1*/  return_constant__UINT8;
#endif
/*Proc_ForwardDeclareFrameType*/struct return_constant__ret_ku8_Frame_t;typedef struct return_constant__ret_ku8_Frame_t return_constant__ret_ku8_Frame_t;
return_constant__UINT8
__cdecl
return_constant__ret_ku8(void);
 /* declare_local */
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct return_constant__ret_kL_Frame_t;typedef struct return_constant__ret_kL_Frame_t return_constant__ret_kL_Frame_t;
LONGINT
__cdecl
return_constant__ret_kL(void);
 /* declare_local */
 /* Locals_begin_procedure */
 /* begin_block */
 /* Locals_end_procedure */
 /* end_block */
 /* Locals_begin_procedure */
 /* begin_block */
 /* Locals_end_procedure */
 /* end_block */
 /* Locals_begin_procedure */
 /* begin_block */
 /* Locals_end_procedure */
 /* end_block */
 /* Locals_begin_procedure */
 /* begin_block */
 /* Locals_end_procedure */
 /* end_block */
 /* Locals_begin_procedure */
 /* begin_block */
 /* Locals_end_procedure */
 /* end_block */
 /* Locals_begin_procedure */
 /* begin_block */
 /* Locals_end_procedure */
 /* end_block */
 /* Locals_begin_procedure */
 /* begin_block */
 /* Locals_end_procedure */
 /* end_block */
 /* Locals_begin_procedure */
 /* begin_block */
 /* Locals_end_procedure */
 /* end_block */
 /* Locals_begin_procedure */
 /* begin_block */
 /* Locals_end_procedure */
 /* end_block */
 /* Locals_begin_procedure */
 /* begin_block */
 /* Locals_end_procedure */
 /* end_block */
 /* Locals_begin_procedure */
 /* begin_block */
 /* Locals_end_procedure */
 /* end_block */
 /* Locals_begin_procedure */
 /* begin_block */
 /* Locals_end_procedure */
 /* end_block */
 /* Locals_begin_procedure */
 /* begin_block */
 /* Locals_end_procedure */
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
struct return_constant_m_5_L_6_t{UINT8 L_23[18];
char L_24[1];
UINT8 L_25[6];
char L_26[1];
UINT8 L_27[7];
char L_28[1];
UINT8 L_29[8];
char L_30[1];
UINT8 L_31[6];
char L_32[1];
UINT8 L_33[8];
char L_34[1];
UINT8 L_35[8];
char L_36[1];
UINT8 L_37[8];
char L_38[1];
UINT8 L_39[6];
char L_40[1];
UINT8 L_41[8];
char L_42[1];
UINT8 L_43[7];
char L_44[1];
UINT8 L_45[8];
char L_46[1];
UINT8 L_47[8];
char L_48[1];
UINT8 L_49[8];
char L_50[1];
UINT8 L_51[7];
char L_52[1];
ADDRESS L_53[30];
char L_54[8];
UINT8 L_55[18];
char L_56[14];
};
static  const return_constant_m_5_L_6_t return_constant_m_5_L_6={{'r','e','t','u','r','n','_','c','o','n','s','t','a','n','t','_','M','3'},{0 /* 1 */ ,},{'r','e','t','_','k','L'},{0 /* 1 */ ,},{'r','e','t','_','k','u','8'},{0 /* 1 */ ,},{'r','e','t','_','k','u','3','2'},{0 /* 1 */ ,},{'r','e','t','_','k','C'},{0 /* 1 */ ,},{'r','e','t','_','k','i','1','6'},{0 /* 1 */ ,},{'r','e','t','_','k','f','3','2'},{0 /* 1 */ ,},{'r','e','t','_','k','i','6','4'},{0 /* 1 */ ,},{'r','e','t','_','k','I'},{0 /* 1 */ ,},{'r','e','t','_','k','u','1','6'},{0 /* 1 */ ,},{'r','e','t','_','k','L','C'},{0 /* 1 */ ,},{'r','e','t','_','k','i','3','2'},{0 /* 1 */ ,},{'r','e','t','_','k','f','6','4'},{0 /* 1 */ ,},{'r','e','t','_','k','u','6','4'},{0 /* 1 */ ,},{'r','e','t','_','k','i','8'},{0 /* 1 */ ,},{(ADDRESS)&return_constant_M3,(char*)&return_constant_m_5_L_6,(ADDRESS)&return_constant__ret_kL,19+(char*)&return_constant_m_5_L_6,(ADDRESS)&return_constant__ret_ku8,26+(char*)&return_constant_m_5_L_6,(ADDRESS)&return_constant__ret_ku32
,34+(char*)&return_constant_m_5_L_6,(ADDRESS)&return_constant__ret_kC,43+(char*)&return_constant_m_5_L_6,(ADDRESS)&return_constant__ret_ki16,50+(char*)&return_constant_m_5_L_6,(ADDRESS)&return_constant__ret_kf32,59+(char*)&return_constant_m_5_L_6,(ADDRESS)&return_constant__ret_ki64,68+(char*)&return_constant_m_5_L_6,(ADDRESS)&return_constant__ret_kI,77+(char*)&return_constant_m_5_L_6,(ADDRESS)&return_constant__ret_ku16,84+(char*)&return_constant_m_5_L_6,(ADDRESS)&return_constant__ret_kLC,93+(char*)&return_constant_m_5_L_6,(ADDRESS)&return_constant__ret_ki32,101+(char*)&return_constant_m_5_L_6,(ADDRESS)&return_constant__ret_kf64,110+(char*)&return_constant_m_5_L_6,(ADDRESS)&return_constant__ret_ku64,119+(char*)&return_constant_m_5_L_6,(ADDRESS)&return_constant__ret_ki8,128+(char*)&return_constant_m_5_L_6},{0 /* 1 */ ,0 /* 2 */ ,0 /* 3 */ ,0 /* 4 */ ,0 /* 5 */ ,0 /* 6 */ ,0 /* 7 */ ,0 /* 8 */ ,},{'r','e','t','u','r','n','_','c','o','n','s','t','a','n','t','.','m','3'},{0 /* 1 */ ,0 /* 2 */ 
,0 /* 3 */ ,0 /* 4 */ ,0 /* 5 */ ,0 /* 6 */ ,0 /* 7 */ ,0 /* 8 */ ,0 /* 9 */ ,0 /* 10 */ ,0 /* 11 */ ,0 /* 12 */ ,0 /* 13 */ ,0 /* 14 */ ,}};
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
struct return_constant_m_M_return_constant_L_7_t{ADDRESS L_57[1];
char L_58[32];
ADDRESS L_59[1];
char L_60[24];
ADDRESS L_61[1];
char L_62[8];
ADDRESS L_63[1];
INT64 L_64[1];
INT8 L_65[1];
char L_66[7];
INT64 L_67[1];
double L_68[1];
INT32 L_69[1];
char L_70[4];
INT64 L_71[1];
INT16 L_72[1];
char L_73[6];
INT64 L_74[2];
float L_75[1];
INT16 L_76[1];
char L_77[2];
INT64 L_78[1];
INT32 L_79[1];
INT8 L_80[1];
char L_81[3];
INT64 L_82[1];
char L_83[24];
ADDRESS L_84[2];
char L_85[8];
ADDRESS L_86[2];
char L_87[8];
ADDRESS L_88[2];
char L_89[8];
ADDRESS L_90[2];
char L_91[8];
ADDRESS L_92[1];
char L_93[8];
};
static return_constant_m_M_return_constant_L_7_t return_constant_m_M_return_constant_L_7={{384+(char*)&return_constant_m_5_L_6},{0 /* 1 */ ,0 /* 2 */ ,0 /* 3 */ ,0 /* 4 */ ,0 /* 5 */ ,0 /* 6 */ ,0 /* 7 */ ,0 /* 8 */ ,0 /* 9 */ ,0 /* 10 */ ,0 /* 11 */ ,0 /* 12 */ ,0 /* 13 */ ,0 /* 14 */ ,0 /* 15 */ ,0 /* 16 */ ,0 /* 17 */ ,0 /* 18 */ ,0 /* 19 */ ,0 /* 20 */ ,0 /* 21 */ ,0 /* 22 */ ,0 /* 23 */ ,0 /* 24 */ ,0 /* 25 */ ,0 /* 26 */ ,0 /* 27 */ ,0 /* 28 */ ,0 /* 29 */ ,0 /* 30 */ ,0 /* 31 */ ,0 /* 32 */ ,},{136+(char*)&return_constant_m_5_L_6},{0 /* 1 */ ,0 /* 2 */ ,0 /* 3 */ ,0 /* 4 */ ,0 /* 5 */ ,0 /* 6 */ ,0 /* 7 */ ,0 /* 8 */ ,0 /* 9 */ ,0 /* 10 */ ,0 /* 11 */ ,0 /* 12 */ ,0 /* 13 */ ,0 /* 14 */ ,0 /* 15 */ ,0 /* 16 */ ,0 /* 17 */ ,0 /* 18 */ ,0 /* 19 */ ,0 /* 20 */ ,0 /* 21 */ ,0 /* 22 */ ,0 /* 23 */ ,0 /* 24 */ ,},{216+(char*)&return_constant_m_M_return_constant_L_7},{0 /* 1 */ ,0 /* 2 */ ,0 /* 3 */ ,0 /* 4 */ ,0 /* 5 */ ,0 /* 6 */ ,0 /* 7 */ ,0 /* 8 */ ,},{(ADDRESS)&return_constant_M3
},{INT64_(3)},{((INT8)17)},{0 /* 1 */ ,0 /* 2 */ ,0 /* 3 */ ,0 /* 4 */ ,0 /* 5 */ ,0 /* 6 */ ,0 /* 7 */ ,},{INT64_(18)},{1.91999999999999993e1},{21},{0 /* 1 */ ,0 /* 2 */ ,0 /* 3 */ ,0 /* 4 */ ,},{INT64_(22)},{((INT16)23)},{0 /* 1 */ ,0 /* 2 */ ,0 /* 3 */ ,0 /* 4 */ ,0 /* 5 */ ,0 /* 6 */ ,},{INT64_(24),INT64_(25)},{2.6270000457764E1F},{((INT16)28)},{0 /* 1 */ ,0 /* 2 */ ,},{INT64_(29)},{30},{((INT8)31)},{0 /* 1 */ ,0 /* 2 */ ,0 /* 3 */ ,},{INT64_(32)},{0 /* 1 */ ,0 /* 2 */ ,0 /* 3 */ ,0 /* 4 */ ,0 /* 5 */ ,0 /* 6 */ ,0 /* 7 */ ,0 /* 8 */ ,0 /* 9 */ ,0 /* 10 */ ,0 /* 11 */ ,0 /* 12 */ ,0 /* 13 */ ,0 /* 14 */ ,0 /* 15 */ ,0 /* 16 */ ,0 /* 17 */ ,0 /* 18 */ ,0 /* 19 */ ,0 /* 20 */ ,0 /* 21 */ ,0 /* 22 */ ,0 /* 23 */ ,0 /* 24 */ ,},{(ADDRESS)&return_constant_I3,240+(char*)&return_constant_m_M_return_constant_L_7},{0 /* 1 */ ,0 /* 2 */ ,0 /* 3 */ ,0 /* 4 */ ,0 /* 5 */ ,0 /* 6 */ ,0 /* 7 */ ,0 /* 8 */ ,},{(ADDRESS)&Long_I3,264+(char*)&return_constant_m_M_return_constant_L_7},{0 /* 1 */ ,0 /* 2 */ 
,0 /* 3 */ ,0 /* 4 */ ,0 /* 5 */ ,0 /* 6 */ ,0 /* 7 */ ,0 /* 8 */ ,},{(ADDRESS)&Word_I3,288+(char*)&return_constant_m_M_return_constant_L_7},{0 /* 1 */ ,0 /* 2 */ ,0 /* 3 */ ,0 /* 4 */ ,0 /* 5 */ ,0 /* 6 */ ,0 /* 7 */ ,0 /* 8 */ ,},{(ADDRESS)&Cstdint_I3,312+(char*)&return_constant_m_M_return_constant_L_7},{0 /* 1 */ ,0 /* 2 */ ,0 /* 3 */ ,0 /* 4 */ ,0 /* 5 */ ,0 /* 6 */ ,0 /* 7 */ ,0 /* 8 */ ,},{(ADDRESS)&RTHooks_I3},{0 /* 1 */ ,0 /* 2 */ ,0 /* 3 */ ,0 /* 4 */ ,0 /* 5 */ ,0 /* 6 */ ,0 /* 7 */ ,0 /* 8 */ ,}};
 /* end: segments/globals */
 /* begin: mark used */
 /* end: mark used */
 /* set_source_file */
 /* set_source_line */
#line 1 "return_constant.m3"
 /* module global constants */
#line 1 "return_constant.m3"
 /* module global data */
#line 1 "return_constant.m3"
 /* set_source_line */
#line 1 "return_constant.m3"
#line 68 "return_constant.m3"
 /* ret_ki8 */
#line 68 "return_constant.m3"
 /* set_source_line */
#line 68 "return_constant.m3"
#line 54 "return_constant.m3"
 /* begin_procedure */
#line 54 "return_constant.m3"
struct return_constant__ret_ki8_Frame_t {
#line 54 "return_constant.m3"
ADDRESS _unused;
#line 54 "return_constant.m3"
};
#line 54 "return_constant.m3"
return_constant__INT8
__cdecl
return_constant__ret_ki8(void)
{
#line 54 "return_constant.m3"
return_constant__ret_ki8_Frame_t _frame;
#line 54 "return_constant.m3"
_frame._unused=(ADDRESS)&_frame;
#line 54 "return_constant.m3"
 /* load_integer */
#line 54 "return_constant.m3"
 /* exit_proc */
#line 54 "return_constant.m3"
return  INT64_(1);
#line 54 "return_constant.m3"
 /* end_procedure */
#line 54 "return_constant.m3"
} /* ret_ku64 */
#line 54 "return_constant.m3"
 /* set_source_line */
#line 54 "return_constant.m3"
#line 55 "return_constant.m3"
 /* begin_procedure */
#line 55 "return_constant.m3"
struct return_constant__ret_ku64_Frame_t {
#line 55 "return_constant.m3"
ADDRESS _unused;
#line 55 "return_constant.m3"
};
#line 55 "return_constant.m3"
return_constant__UINT64
__cdecl
return_constant__ret_ku64(void)
{
#line 55 "return_constant.m3"
return_constant__ret_ku64_Frame_t _frame;
#line 55 "return_constant.m3"
_frame._unused=(ADDRESS)&_frame;
#line 55 "return_constant.m3"
 /* load_integer */
#line 55 "return_constant.m3"
 /* exit_proc */
#line 55 "return_constant.m3"
return  INT64_(2);
#line 55 "return_constant.m3"
 /* end_procedure */
#line 55 "return_constant.m3"
} /* ret_kf64 */
#line 55 "return_constant.m3"
 /* set_source_line */
#line 55 "return_constant.m3"
#line 56 "return_constant.m3"
 /* begin_procedure */
#line 56 "return_constant.m3"
struct return_constant__ret_kf64_Frame_t {
#line 56 "return_constant.m3"
ADDRESS _unused;
#line 56 "return_constant.m3"
};
#line 56 "return_constant.m3"
return_constant__FLOAT64
__cdecl
return_constant__ret_kf64(void)
{
#line 56 "return_constant.m3"
return_constant__ret_kf64_Frame_t _frame;
#line 56 "return_constant.m3"
_frame._unused=(ADDRESS)&_frame;
#line 56 "return_constant.m3"
 /* load_float */
#line 56 "return_constant.m3"
 /* exit_proc */
#line 56 "return_constant.m3"
return ((double)(3.39999999999999991e0));
#line 56 "return_constant.m3"
 /* end_procedure */
#line 56 "return_constant.m3"
} /* ret_ki32 */
#line 56 "return_constant.m3"
 /* set_source_line */
#line 56 "return_constant.m3"
#line 57 "return_constant.m3"
 /* begin_procedure */
#line 57 "return_constant.m3"
struct return_constant__ret_ki32_Frame_t {
#line 57 "return_constant.m3"
ADDRESS _unused;
#line 57 "return_constant.m3"
};
#line 57 "return_constant.m3"
return_constant__INT32
__cdecl
return_constant__ret_ki32(void)
{
#line 57 "return_constant.m3"
return_constant__ret_ki32_Frame_t _frame;
#line 57 "return_constant.m3"
_frame._unused=(ADDRESS)&_frame;
#line 57 "return_constant.m3"
 /* load_integer */
#line 57 "return_constant.m3"
 /* exit_proc */
#line 57 "return_constant.m3"
return  INT64_(5);
#line 57 "return_constant.m3"
 /* end_procedure */
#line 57 "return_constant.m3"
} /* ret_kLC */
#line 57 "return_constant.m3"
 /* set_source_line */
#line 57 "return_constant.m3"
#line 58 "return_constant.m3"
 /* begin_procedure */
#line 58 "return_constant.m3"
struct return_constant__ret_kLC_Frame_t {
#line 58 "return_constant.m3"
ADDRESS _unused;
#line 58 "return_constant.m3"
};
#line 58 "return_constant.m3"
LONGCARD
__cdecl
return_constant__ret_kLC(void)
{
#line 58 "return_constant.m3"
return_constant__ret_kLC_Frame_t _frame;
#line 58 "return_constant.m3"
_frame._unused=(ADDRESS)&_frame;
#line 58 "return_constant.m3"
 /* load_integer */
#line 58 "return_constant.m3"
 /* exit_proc */
#line 58 "return_constant.m3"
return  INT64_(6);
#line 58 "return_constant.m3"
 /* end_procedure */
#line 58 "return_constant.m3"
} /* ret_ku16 */
#line 58 "return_constant.m3"
 /* set_source_line */
#line 58 "return_constant.m3"
#line 59 "return_constant.m3"
 /* begin_procedure */
#line 59 "return_constant.m3"
struct return_constant__ret_ku16_Frame_t {
#line 59 "return_constant.m3"
ADDRESS _unused;
#line 59 "return_constant.m3"
};
#line 59 "return_constant.m3"
return_constant__UINT16
__cdecl
return_constant__ret_ku16(void)
{
#line 59 "return_constant.m3"
return_constant__ret_ku16_Frame_t _frame;
#line 59 "return_constant.m3"
_frame._unused=(ADDRESS)&_frame;
#line 59 "return_constant.m3"
 /* load_integer */
#line 59 "return_constant.m3"
 /* exit_proc */
#line 59 "return_constant.m3"
return  INT64_(7);
#line 59 "return_constant.m3"
 /* end_procedure */
#line 59 "return_constant.m3"
} /* ret_kI */
#line 59 "return_constant.m3"
 /* set_source_line */
#line 59 "return_constant.m3"
#line 60 "return_constant.m3"
 /* begin_procedure */
#line 60 "return_constant.m3"
struct return_constant__ret_kI_Frame_t {
#line 60 "return_constant.m3"
ADDRESS _unused;
#line 60 "return_constant.m3"
};
#line 60 "return_constant.m3"
INTEGER
__cdecl
return_constant__ret_kI(void)
{
#line 60 "return_constant.m3"
return_constant__ret_kI_Frame_t _frame;
#line 60 "return_constant.m3"
_frame._unused=(ADDRESS)&_frame;
#line 60 "return_constant.m3"
 /* load_integer */
#line 60 "return_constant.m3"
 /* exit_proc */
#line 60 "return_constant.m3"
return  INT64_(8);
#line 60 "return_constant.m3"
 /* end_procedure */
#line 60 "return_constant.m3"
} /* ret_ki64 */
#line 60 "return_constant.m3"
 /* set_source_line */
#line 60 "return_constant.m3"
#line 61 "return_constant.m3"
 /* begin_procedure */
#line 61 "return_constant.m3"
struct return_constant__ret_ki64_Frame_t {
#line 61 "return_constant.m3"
ADDRESS _unused;
#line 61 "return_constant.m3"
};
#line 61 "return_constant.m3"
return_constant__INT64
__cdecl
return_constant__ret_ki64(void)
{
#line 61 "return_constant.m3"
return_constant__ret_ki64_Frame_t _frame;
#line 61 "return_constant.m3"
_frame._unused=(ADDRESS)&_frame;
#line 61 "return_constant.m3"
 /* load_integer */
#line 61 "return_constant.m3"
 /* exit_proc */
#line 61 "return_constant.m3"
return  INT64_(9);
#line 61 "return_constant.m3"
 /* end_procedure */
#line 61 "return_constant.m3"
} /* ret_kf32 */
#line 61 "return_constant.m3"
 /* set_source_line */
#line 61 "return_constant.m3"
#line 62 "return_constant.m3"
 /* begin_procedure */
#line 62 "return_constant.m3"
struct return_constant__ret_kf32_Frame_t {
#line 62 "return_constant.m3"
ADDRESS _unused;
#line 62 "return_constant.m3"
};
#line 62 "return_constant.m3"
return_constant__FLOAT32
__cdecl
return_constant__ret_kf32(void)
{
#line 62 "return_constant.m3"
return_constant__ret_kf32_Frame_t _frame;
#line 62 "return_constant.m3"
_frame._unused=(ADDRESS)&_frame;
#line 62 "return_constant.m3"
 /* load_float */
#line 62 "return_constant.m3"
 /* exit_proc */
#line 62 "return_constant.m3"
return ((float)(1.0109999656677E1F));
#line 62 "return_constant.m3"
 /* end_procedure */
#line 62 "return_constant.m3"
} /* ret_ki16 */
#line 62 "return_constant.m3"
 /* set_source_line */
#line 62 "return_constant.m3"
#line 63 "return_constant.m3"
 /* begin_procedure */
#line 63 "return_constant.m3"
struct return_constant__ret_ki16_Frame_t {
#line 63 "return_constant.m3"
ADDRESS _unused;
#line 63 "return_constant.m3"
};
#line 63 "return_constant.m3"
return_constant__INT16
__cdecl
return_constant__ret_ki16(void)
{
#line 63 "return_constant.m3"
return_constant__ret_ki16_Frame_t _frame;
#line 63 "return_constant.m3"
_frame._unused=(ADDRESS)&_frame;
#line 63 "return_constant.m3"
 /* load_integer */
#line 63 "return_constant.m3"
 /* exit_proc */
#line 63 "return_constant.m3"
return  INT64_(12);
#line 63 "return_constant.m3"
 /* end_procedure */
#line 63 "return_constant.m3"
} /* ret_kC */
#line 63 "return_constant.m3"
 /* set_source_line */
#line 63 "return_constant.m3"
#line 64 "return_constant.m3"
 /* begin_procedure */
#line 64 "return_constant.m3"
struct return_constant__ret_kC_Frame_t {
#line 64 "return_constant.m3"
ADDRESS _unused;
#line 64 "return_constant.m3"
};
#line 64 "return_constant.m3"
CARDINAL
__cdecl
return_constant__ret_kC(void)
{
#line 64 "return_constant.m3"
return_constant__ret_kC_Frame_t _frame;
#line 64 "return_constant.m3"
_frame._unused=(ADDRESS)&_frame;
#line 64 "return_constant.m3"
 /* load_integer */
#line 64 "return_constant.m3"
 /* exit_proc */
#line 64 "return_constant.m3"
return  INT64_(13);
#line 64 "return_constant.m3"
 /* end_procedure */
#line 64 "return_constant.m3"
} /* ret_ku32 */
#line 64 "return_constant.m3"
 /* set_source_line */
#line 64 "return_constant.m3"
#line 65 "return_constant.m3"
 /* begin_procedure */
#line 65 "return_constant.m3"
struct return_constant__ret_ku32_Frame_t {
#line 65 "return_constant.m3"
ADDRESS _unused;
#line 65 "return_constant.m3"
};
#line 65 "return_constant.m3"
return_constant__UINT32
__cdecl
return_constant__ret_ku32(void)
{
#line 65 "return_constant.m3"
return_constant__ret_ku32_Frame_t _frame;
#line 65 "return_constant.m3"
_frame._unused=(ADDRESS)&_frame;
#line 65 "return_constant.m3"
 /* load_integer */
#line 65 "return_constant.m3"
 /* exit_proc */
#line 65 "return_constant.m3"
return  INT64_(14);
#line 65 "return_constant.m3"
 /* end_procedure */
#line 65 "return_constant.m3"
} /* ret_ku8 */
#line 65 "return_constant.m3"
 /* set_source_line */
#line 65 "return_constant.m3"
#line 66 "return_constant.m3"
 /* begin_procedure */
#line 66 "return_constant.m3"
struct return_constant__ret_ku8_Frame_t {
#line 66 "return_constant.m3"
ADDRESS _unused;
#line 66 "return_constant.m3"
};
#line 66 "return_constant.m3"
return_constant__UINT8
__cdecl
return_constant__ret_ku8(void)
{
#line 66 "return_constant.m3"
return_constant__ret_ku8_Frame_t _frame;
#line 66 "return_constant.m3"
_frame._unused=(ADDRESS)&_frame;
#line 66 "return_constant.m3"
 /* load_integer */
#line 66 "return_constant.m3"
 /* exit_proc */
#line 66 "return_constant.m3"
return  INT64_(15);
#line 66 "return_constant.m3"
 /* end_procedure */
#line 66 "return_constant.m3"
} /* ret_kL */
#line 66 "return_constant.m3"
 /* set_source_line */
#line 66 "return_constant.m3"
#line 67 "return_constant.m3"
 /* begin_procedure */
#line 67 "return_constant.m3"
struct return_constant__ret_kL_Frame_t {
#line 67 "return_constant.m3"
ADDRESS _unused;
#line 67 "return_constant.m3"
};
#line 67 "return_constant.m3"
LONGINT
__cdecl
return_constant__ret_kL(void)
{
#line 67 "return_constant.m3"
return_constant__ret_kL_Frame_t _frame;
#line 67 "return_constant.m3"
_frame._unused=(ADDRESS)&_frame;
#line 67 "return_constant.m3"
 /* load_integer */
#line 67 "return_constant.m3"
 /* exit_proc */
#line 67 "return_constant.m3"
return  INT64_(16);
#line 67 "return_constant.m3"
 /* end_procedure */
#line 67 "return_constant.m3"
} /* return_constant_M3 */
#line 67 "return_constant.m3"
 /* module main body return_constant_M3 */
#line 67 "return_constant.m3"
 /* set_source_line */
#line 67 "return_constant.m3"
#line 68 "return_constant.m3"
 /* begin_procedure */
#line 68 "return_constant.m3"
struct return_constant_M3_Frame_t {
#line 68 "return_constant.m3"
ADDRESS _unused;
#line 68 "return_constant.m3"
};
#line 68 "return_constant.m3"
RT0__ModulePtr
__cdecl
return_constant_M3(
   /* Param_Type1 */ INTEGER /*TypeText1*/  mode_L_8)
{
#line 68 "return_constant.m3"
return_constant_M3_Frame_t _frame;
#line 68 "return_constant.m3"
_frame._unused=(ADDRESS)&_frame;
#line 68 "return_constant.m3"
 /* load */
#line 68 "return_constant.m3"
 /* if_true_or_false */
#line 68 "return_constant.m3"
 /* load_host_integer */
#line 68 "return_constant.m3"
 /* load_integer */
#line 68 "return_constant.m3"
 /* if_compare */
#line 68 "return_constant.m3"
if(m3_eq(INT64,
  mode_L_8,
   INT64_(0)))goto L1;
#line 68 "return_constant.m3"
 /* set_label */
#line 68 "return_constant.m3"
L1:;
#line 68 "return_constant.m3"
 /* load_address */
#line 68 "return_constant.m3"
 /* exit_proc */
#line 68 "return_constant.m3"
return (RT0__ModulePtr)(&return_constant_m_M_return_constant_L_7);
#line 68 "return_constant.m3"
 /* end_procedure */
#line 68 "return_constant.m3"
} /* global constant type descriptor */
#line 68 "return_constant.m3"
 /* global data type descriptor */
#line 68 "return_constant.m3"
 /* module global constants */
#line 68 "return_constant.m3"
 /* procedure names */
#line 68 "return_constant.m3"
 /* procedure table */
#line 68 "return_constant.m3"
 /* file name */
#line 68 "return_constant.m3"
 /* module global data */
#line 68 "return_constant.m3"
 /* load map


 global data allocation for M_return_constant
     0   104  8  *module info*
   104     1  1  return_constant.vi8
   112     8  8  return_constant.vu64
   120     8  8  return_constant.vf64
   128     4  4  return_constant.vi32
   136     8  8  return_constant.vLC
   144     2  2  return_constant.vu16
   152     8  8  return_constant.vI
   160     8  8  return_constant.vi64
   168     4  4  return_constant.vf32
   172     2  2  return_constant.vi16
   176     8  8  return_constant.vC
   184     4  4  return_constant.vu32
   188     1  1  return_constant.vu8
   192     8  8  return_constant.vL
   200     8  8  return_constant.offset
   208     8  8  return_constant.count
   216    24  8  import return_constant
   240    24  8  import Long
   264    24  8  import Word
   288    24  8  import Cstdint
   312    24  8  import RTHooks
   336     0  8  *TOTAL*


 global constants for M_return_constant
     0   136  8  *proc names*
   136   248  8  *proc info*
   384    19  1  *string*
   408     0  8  *TOTAL*
 */
#line 68 "return_constant.m3"
 /* end unit */
#line 68 "return_constant.m3"

#ifdef __cplusplus

} /* extern "C" */
#endif
 /* set_runtime_proc */
 /* set_runtime_proc */
 /* set_runtime_proc */
