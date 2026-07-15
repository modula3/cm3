// library:pgm
// source_base_name:Main
// target_name:Main.m3.cpp
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
 /* declare_proctype */
 /* declare_formal */
 /* declare_formal */
 /* declare_subrange */
/*subrange_define*/typedef UINT8 TE5E33AA_8;
 /* declare_array */
/*array_forwardDeclare*/struct T81201307;typedef struct T81201307 T81201307;

#ifndef T81201307
#define T81201307 T81201307
/*fixedArray_define*/struct T81201307{INTEGER _elts[101];};
#endif
 /* declare_indirect */
typedef T81201307*T7EDFECF8;
 /* declare_record */
 /* declare_record */
 /* declare_field */
 /* declare_field */
 /* DeclareTypes_FlushOnce size:1 */

#if 0 /* avoid type hash collions */
typedef 
void(__cdecl*T47588B72)(INTEGER,INTEGER);
#else
typedef void (__cdecl*T47588B72)(void);
#endif
 /* DeclareTypes_FlushOnce size:0 */
 /* end: DeclareTypes */
 /* begin: helper functions */
 /* end: helper functions */

#ifndef struct_808_t
#define struct_808_t struct_808_t
STRUCT8(808)
#endif
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
/*Proc_ForwardDeclareFrameType*/struct RTHooks_I3_Frame_t;typedef struct RTHooks_I3_Frame_t RTHooks_I3_Frame_t;
 /* internal_declare_param */
RT0__ModulePtr
__cdecl
RTHooks_I3(
   /* Param_Type1 */ INTEGER /*TypeText1*/  mode_L_1);
 /* end: imports */
 /* begin: locals */
 /* declare_segment name:<NIL> typeid:TFFFFFFFF const:TRUE */
/*declare_segment*/struct Main_m_2_L_3_t;
/*declare_segment*/typedef struct Main_m_2_L_3_t Main_m_2_L_3_t;
 /* declare_segment name:M_Main typeid:TFFFFFFFF const:FALSE */
 /* handler_name_prefixes:Main_M3_LINE_ */
 /* handler_name_prefixes:Main_I3_LINE_ */
/*declare_segment*/struct Main_m_M_Main_L_4_t;
/*declare_segment*/typedef struct Main_m_M_Main_L_4_t Main_m_M_Main_L_4_t;
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct Main_M3_Frame_t;typedef struct Main_M3_Frame_t Main_M3_Frame_t;
 /* internal_declare_param */
RT0__ModulePtr
__cdecl
Main_M3(
   /* Param_Type1 */ INTEGER /*TypeText1*/  mode_L_5);
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct Main__no_nonvolatile_registers_Frame_t;typedef struct Main__no_nonvolatile_registers_Frame_t Main__no_nonvolatile_registers_Frame_t;
void /*TypeText3*/ 
__cdecl
Main__no_nonvolatile_registers(void);
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct Main__ebx_Frame_t;typedef struct Main__ebx_Frame_t Main__ebx_Frame_t;
 /* internal_declare_param */
 /* internal_declare_param */
void /*TypeText3*/ 
__cdecl
Main__ebx(
   /* Param_Type1 */ INTEGER a_L_6,
   /* Param_Type1 */ INTEGER b_L_7);
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct Main__esi_edi_Frame_t;typedef struct Main__esi_edi_Frame_t Main__esi_edi_Frame_t;
void /*TypeText3*/ 
__cdecl
Main__esi_edi(void);
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct Main__esi_edi_ebx_Frame_t;typedef struct Main__esi_edi_ebx_Frame_t Main__esi_edi_ebx_Frame_t;
 /* internal_declare_param */
 /* internal_declare_param */
void /*TypeText3*/ 
__cdecl
Main__esi_edi_ebx(
   /* Param_Type1 */ INTEGER a_L_8,
   /* Param_Type1 */ INTEGER b_L_9);
 /* Locals_begin_procedure */
 /* begin_block */
 /* Locals_end_procedure */
 /* end_block */
 /* Locals_begin_procedure */
 /* begin_block */
 /* Locals_end_procedure */
 /* end_block */
 /* Locals_begin_procedure */
 /* begin_block */
 /* Locals_end_procedure */
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
 /* declare_global */
 /* declare_global */
static  /* Var_Type3 */ STRUCT(808) g_L_10;
 /* declare_global */
 /* declare_global */
static  /* Var_Type3 */ STRUCT(808) h_L_11;
 /* bind_segment */
 /* begin_init */
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
 /* init_chars */
 /* end_init */
struct Main_m_2_L_3_t{UINT8 L_12[7];
char L_13[1];
UINT8 L_14[11];
char L_15[1];
UINT8 L_16[7];
char L_17[1];
UINT8 L_18[3];
char L_19[1];
UINT8 L_20[24];
char L_21[8];
ADDRESS L_22[10];
char L_23[8];
UINT8 L_24[10];
char L_25[14];
};
static  const Main_m_2_L_3_t Main_m_2_L_3={{'M','a','i','n','_','M','3'},{0 /* 1 */ ,},{'e','s','i','_','e','d','i','_','e','b','x'},{0 /* 1 */ ,},{'e','s','i','_','e','d','i'},{0 /* 1 */ ,},{'e','b','x'},{0 /* 1 */ ,},{'n','o','_','n','o','n','v','o','l','a','t','i','l','e','_','r','e','g','i','s','t','e','r','s'},{0 /* 1 */ ,0 /* 2 */ ,0 /* 3 */ ,0 /* 4 */ ,0 /* 5 */ ,0 /* 6 */ ,0 /* 7 */ ,0 /* 8 */ ,},{(ADDRESS)&Main_M3,(char*)&Main_m_2_L_3,(ADDRESS)&Main__esi_edi_ebx,8+(char*)&Main_m_2_L_3,(ADDRESS)&Main__esi_edi,20+(char*)&Main_m_2_L_3,(ADDRESS)&Main__ebx,28+(char*)&Main_m_2_L_3,(ADDRESS)&Main__no_nonvolatile_registers,32+(char*)&Main_m_2_L_3},{0 /* 1 */ ,0 /* 2 */ ,0 /* 3 */ ,0 /* 4 */ ,0 /* 5 */ ,0 /* 6 */ ,0 /* 7 */ ,0 /* 8 */ ,},{'.','.','/','M','a','i','n','.','m','3'},{0 /* 1 */ ,0 /* 2 */ ,0 /* 3 */ ,0 /* 4 */ ,0 /* 5 */ ,0 /* 6 */ ,0 /* 7 */ ,0 /* 8 */ ,0 /* 9 */ ,0 /* 10 */ ,0 /* 11 */ ,0 /* 12 */ ,0 /* 13 */ ,0 /* 14 */ ,}};
 /* bind_segment */
 /* begin_init */
 /* init_var */
 /* init_var */
 /* init_var */
 /* init_proc */
 /* init_int */
 /* init_var */
 /* init_var */
 /* init_proc */
 /* init_var */
 /* init_proc */
 /* end_init */
struct Main_m_M_Main_L_4_t{ADDRESS L_26[1];
char L_27[32];
ADDRESS L_28[1];
char L_29[24];
ADDRESS L_30[1];
char L_31[8];
ADDRESS L_32[1];
INT64 L_33[1];
ADDRESS L_34[2];
char L_35[8];
ADDRESS L_36[2];
char L_37[8];
ADDRESS L_38[1];
char L_39[16];
};
static Main_m_M_Main_L_4_t Main_m_M_Main_L_4={{152+(char*)&Main_m_2_L_3},{0 /* 1 */ ,0 /* 2 */ ,0 /* 3 */ ,0 /* 4 */ ,0 /* 5 */ ,0 /* 6 */ ,0 /* 7 */ ,0 /* 8 */ ,0 /* 9 */ ,0 /* 10 */ ,0 /* 11 */ ,0 /* 12 */ ,0 /* 13 */ ,0 /* 14 */ ,0 /* 15 */ ,0 /* 16 */ ,0 /* 17 */ ,0 /* 18 */ ,0 /* 19 */ ,0 /* 20 */ ,0 /* 21 */ ,0 /* 22 */ ,0 /* 23 */ ,0 /* 24 */ ,0 /* 25 */ ,0 /* 26 */ ,0 /* 27 */ ,0 /* 28 */ ,0 /* 29 */ ,0 /* 30 */ ,0 /* 31 */ ,0 /* 32 */ ,},{64+(char*)&Main_m_2_L_3},{0 /* 1 */ ,0 /* 2 */ ,0 /* 3 */ ,0 /* 4 */ ,0 /* 5 */ ,0 /* 6 */ ,0 /* 7 */ ,0 /* 8 */ ,0 /* 9 */ ,0 /* 10 */ ,0 /* 11 */ ,0 /* 12 */ ,0 /* 13 */ ,0 /* 14 */ ,0 /* 15 */ ,0 /* 16 */ ,0 /* 17 */ ,0 /* 18 */ ,0 /* 19 */ ,0 /* 20 */ ,0 /* 21 */ ,0 /* 22 */ ,0 /* 23 */ ,0 /* 24 */ ,},{120+(char*)&Main_m_M_Main_L_4},{0 /* 1 */ ,0 /* 2 */ ,0 /* 3 */ ,0 /* 4 */ ,0 /* 5 */ ,0 /* 6 */ ,0 /* 7 */ ,0 /* 8 */ ,},{(ADDRESS)&Main_M3},{INT64_(3)},{(char*)&g_L_10,(char*)&h_L_11},{0 /* 1 */ ,0 /* 2 */ ,0 /* 3 */ ,0 /* 4 */ ,0 /* 5 */ 
,0 /* 6 */ ,0 /* 7 */ ,0 /* 8 */ ,},{(ADDRESS)&Main_I3,144+(char*)&Main_m_M_Main_L_4},{0 /* 1 */ ,0 /* 2 */ ,0 /* 3 */ ,0 /* 4 */ ,0 /* 5 */ ,0 /* 6 */ ,0 /* 7 */ ,0 /* 8 */ ,},{(ADDRESS)&RTHooks_I3},{0 /* 1 */ ,0 /* 2 */ ,0 /* 3 */ ,0 /* 4 */ ,0 /* 5 */ ,0 /* 6 */ ,0 /* 7 */ ,0 /* 8 */ ,0 /* 9 */ ,0 /* 10 */ ,0 /* 11 */ ,0 /* 12 */ ,0 /* 13 */ ,0 /* 14 */ ,0 /* 15 */ ,0 /* 16 */ ,}};
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
#line 39 "../Main.m3"
 /* no_nonvolatile_registers */
#line 39 "../Main.m3"
 /* set_source_line */
#line 39 "../Main.m3"
#line 10 "../Main.m3"
 /* begin_procedure */
#line 10 "../Main.m3"
struct Main__no_nonvolatile_registers_Frame_t {
#line 10 "../Main.m3"
ADDRESS _unused;
#line 10 "../Main.m3"
};
#line 10 "../Main.m3"
void /*TypeText3*/ 
__cdecl
Main__no_nonvolatile_registers(void)
{
#line 10 "../Main.m3"
Main__no_nonvolatile_registers_Frame_t _frame;
#line 10 "../Main.m3"
_frame._unused=(ADDRESS)&_frame;
#line 10 "../Main.m3"
 /* set_source_line */
#line 10 "../Main.m3"
#line 11 "../Main.m3"
 /* exit_proc */
#line 11 "../Main.m3"
return;
#line 11 "../Main.m3"
 /* end_procedure */
#line 11 "../Main.m3"
} /* ebx */
#line 11 "../Main.m3"
 /* set_source_line */
#line 11 "../Main.m3"
#line 13 "../Main.m3"
 /* begin_procedure */
#line 13 "../Main.m3"
struct Main__ebx_Frame_t {
#line 13 "../Main.m3"
ADDRESS _unused;
#line 13 "../Main.m3"
};
#line 13 "../Main.m3"
void /*TypeText3*/ 
__cdecl
Main__ebx(
   /* Param_Type1 */ INTEGER a_L_6,
   /* Param_Type1 */ INTEGER b_L_7)
{
#line 13 "../Main.m3"
Main__ebx_Frame_t _frame;
#line 13 "../Main.m3"
_frame._unused=(ADDRESS)&_frame;
#line 13 "../Main.m3"
 /* set_source_line */
#line 13 "../Main.m3"
#line 17 "../Main.m3"
 /* set_source_line */
#line 17 "../Main.m3"
#line 18 "../Main.m3"
 /* load */
#line 18 "../Main.m3"
 /* store */
#line 18 "../Main.m3"
(*(INT64*)(&a_L_6))=(INT64)( b_L_7);
#line 18 "../Main.m3"
 /* set_source_line */
#line 18 "../Main.m3"
#line 19 "../Main.m3"
 /* exit_proc */
#line 19 "../Main.m3"
return;
#line 19 "../Main.m3"
 /* end_procedure */
#line 19 "../Main.m3"
} /* esi_edi */
#line 19 "../Main.m3"
 /* set_source_line */
#line 19 "../Main.m3"
#line 24 "../Main.m3"
 /* begin_procedure */
#line 24 "../Main.m3"
struct Main__esi_edi_Frame_t {
#line 24 "../Main.m3"
ADDRESS _unused;
#line 24 "../Main.m3"
};
#line 24 "../Main.m3"
void /*TypeText3*/ 
__cdecl
Main__esi_edi(void)
{
#line 24 "../Main.m3"
Main__esi_edi_Frame_t _frame;
#line 24 "../Main.m3"
_frame._unused=(ADDRESS)&_frame;
#line 24 "../Main.m3"
 /* set_source_line */
#line 24 "../Main.m3"
#line 26 "../Main.m3"
 /* set_source_line */
#line 26 "../Main.m3"
#line 27 "../Main.m3"
 /* load_address */
#line 27 "../Main.m3"
 /* load_address */
#line 27 "../Main.m3"
 /* copy */
#line 27 "../Main.m3"
m3_memmove(
 &g_L_10,
 &h_L_11,
 808);
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
} /* esi_edi_ebx */
#line 28 "../Main.m3"
 /* set_source_line */
#line 28 "../Main.m3"
#line 30 "../Main.m3"
 /* begin_procedure */
#line 30 "../Main.m3"
struct Main__esi_edi_ebx_Frame_t {
#line 30 "../Main.m3"
ADDRESS _unused;
#line 30 "../Main.m3"
};
#line 30 "../Main.m3"
void /*TypeText3*/ 
__cdecl
Main__esi_edi_ebx(
   /* Param_Type1 */ INTEGER a_L_8,
   /* Param_Type1 */ INTEGER b_L_9)
{
#line 30 "../Main.m3"
Main__esi_edi_ebx_Frame_t _frame;
#line 30 "../Main.m3"
_frame._unused=(ADDRESS)&_frame;
#line 30 "../Main.m3"
 /* set_source_line */
#line 30 "../Main.m3"
#line 34 "../Main.m3"
 /* set_source_line */
#line 34 "../Main.m3"
#line 35 "../Main.m3"
 /* load */
#line 35 "../Main.m3"
 /* store */
#line 35 "../Main.m3"
(*(INT64*)(&a_L_8))=(INT64)( b_L_9);
#line 35 "../Main.m3"
 /* set_source_line */
#line 35 "../Main.m3"
#line 36 "../Main.m3"
 /* load_address */
#line 36 "../Main.m3"
 /* load_address */
#line 36 "../Main.m3"
 /* copy */
#line 36 "../Main.m3"
m3_memmove(
 &g_L_10,
 &h_L_11,
 808);
#line 36 "../Main.m3"
 /* set_source_line */
#line 36 "../Main.m3"
#line 37 "../Main.m3"
 /* exit_proc */
#line 37 "../Main.m3"
return;
#line 37 "../Main.m3"
 /* end_procedure */
#line 37 "../Main.m3"
} /* Main_M3 */
#line 37 "../Main.m3"
 /* module main body Main_M3 */
#line 37 "../Main.m3"
 /* set_source_line */
#line 37 "../Main.m3"
#line 39 "../Main.m3"
 /* begin_procedure */
#line 39 "../Main.m3"
struct Main_M3_Frame_t {
#line 39 "../Main.m3"
ADDRESS _unused;
#line 39 "../Main.m3"
};
#line 39 "../Main.m3"
RT0__ModulePtr
__cdecl
Main_M3(
   /* Param_Type1 */ INTEGER /*TypeText1*/  mode_L_5)
{
#line 39 "../Main.m3"
Main_M3_Frame_t _frame;
#line 39 "../Main.m3"
_frame._unused=(ADDRESS)&_frame;
#line 39 "../Main.m3"
 /* load */
#line 39 "../Main.m3"
 /* if_true_or_false */
#line 39 "../Main.m3"
 /* load_host_integer */
#line 39 "../Main.m3"
 /* load_integer */
#line 39 "../Main.m3"
 /* if_compare */
#line 39 "../Main.m3"
if(m3_eq(INT64,
  mode_L_5,
   INT64_(0)))goto L1;
#line 39 "../Main.m3"
 /* set_source_line */
#line 39 "../Main.m3"
#line 40 "../Main.m3"
 /* start_call_direct */
#line 40 "../Main.m3"
 /* call_direct */
#line 40 "../Main.m3"
Main__no_nonvolatile_registers(
 );
#line 40 "../Main.m3"
 /* set_source_line */
#line 40 "../Main.m3"
#line 41 "../Main.m3"
 /* start_call_direct */
#line 41 "../Main.m3"
 /* load_integer */
#line 41 "../Main.m3"
 /* pop_param */
#line 41 "../Main.m3"
 /* load_integer */
#line 41 "../Main.m3"
 /* pop_param */
#line 41 "../Main.m3"
 /* call_direct */
#line 41 "../Main.m3"
Main__ebx(
  ( INTEGER )(  INT64_(1) ),
  ( INTEGER )(  INT64_(2) ));
#line 41 "../Main.m3"
 /* set_source_line */
#line 41 "../Main.m3"
#line 42 "../Main.m3"
 /* start_call_direct */
#line 42 "../Main.m3"
 /* call_direct */
#line 42 "../Main.m3"
Main__esi_edi(
 );
#line 42 "../Main.m3"
 /* set_source_line */
#line 42 "../Main.m3"
#line 43 "../Main.m3"
 /* start_call_direct */
#line 43 "../Main.m3"
 /* load_integer */
#line 43 "../Main.m3"
 /* pop_param */
#line 43 "../Main.m3"
 /* load_integer */
#line 43 "../Main.m3"
 /* pop_param */
#line 43 "../Main.m3"
 /* call_direct */
#line 43 "../Main.m3"
Main__esi_edi_ebx(
  ( INTEGER )(  INT64_(1) ),
  ( INTEGER )(  INT64_(2) ));
#line 43 "../Main.m3"
 /* set_label */
#line 43 "../Main.m3"
L1:;
#line 43 "../Main.m3"
 /* load_address */
#line 43 "../Main.m3"
 /* exit_proc */
#line 43 "../Main.m3"
return (RT0__ModulePtr)(&Main_m_M_Main_L_4);
#line 43 "../Main.m3"
 /* end_procedure */
#line 43 "../Main.m3"
} /* global constant type descriptor */
#line 43 "../Main.m3"
 /* global data type descriptor */
#line 43 "../Main.m3"
 /* module global constants */
#line 43 "../Main.m3"
 /* procedure names */
#line 43 "../Main.m3"
 /* procedure table */
#line 43 "../Main.m3"
 /* file name */
#line 43 "../Main.m3"
 /* module global data */
#line 43 "../Main.m3"
 /* load map


 global data allocation for M_Main
     0   104  8  *module info*
   104     8  8  Main.g_INDIRECT_
   112     8  8  Main.h_INDIRECT_
   120    24  8  import Main
   144    24  8  import RTHooks
   168     0  8  *TOTAL*


 global constants for M_Main
     0    57  8  *proc names*
    64    88  8  *proc info*
   152    11  1  *string*
   168     0  8  *TOTAL*
 */
#line 43 "../Main.m3"
 /* end unit */
#line 43 "../Main.m3"

#ifdef __cplusplus

} /* extern "C" */
#endif
 /* set_runtime_proc */
 /* set_runtime_proc */
 /* set_runtime_proc */
