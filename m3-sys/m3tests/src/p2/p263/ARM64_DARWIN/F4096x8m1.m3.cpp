// library:pgm
// source_base_name:F4096x8m1
// target_name:F4096x8m1.m3.cpp
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
 /* declare_proctype */
 /* declare_formal */
 /* declare_proctype */
 /* declare_formal */
 /* declare_formal */
 /* declare_record */
 /* declare_record */
 /* DeclareTypes_FlushOnce size:2 */

#if 0 /* avoid type hash collions */
typedef 
UCHAR(__cdecl*T317D74D6)(T37224E3F*);
#else
typedef void (__cdecl*T317D74D6)(void);
#endif

#if 0 /* avoid type hash collions */
typedef 
void(__cdecl*TA4BB9882)(ADDRESS,INTEGER);
#else
typedef void (__cdecl*TA4BB9882)(void);
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
/*Proc_ForwardDeclareFrameType*/struct F4096x8m1_I3_Frame_t;typedef struct F4096x8m1_I3_Frame_t F4096x8m1_I3_Frame_t;
 /* internal_declare_param */
RT0__ModulePtr
__cdecl
F4096x8m1_I3(
   /* Param_Type1 */ INTEGER /*TypeText1*/  mode_L_0);
 /* import_procedure */
/*Proc_ForwardDeclareFrameType*/struct F0_I3_Frame_t;typedef struct F0_I3_Frame_t F0_I3_Frame_t;
 /* internal_declare_param */
RT0__ModulePtr
__cdecl
F0_I3(
   /* Param_Type1 */ INTEGER /*TypeText1*/  mode_L_1);
 /* import_procedure */
/*Proc_ForwardDeclareFrameType*/struct RTHooks_I3_Frame_t;typedef struct RTHooks_I3_Frame_t RTHooks_I3_Frame_t;
 /* internal_declare_param */
RT0__ModulePtr
__cdecl
RTHooks_I3(
   /* Param_Type1 */ INTEGER /*TypeText1*/  mode_L_2);
 /* import_procedure */
/*Proc_ForwardDeclareFrameType*/struct RTHooks__ReportFault_Frame_t;typedef struct RTHooks__ReportFault_Frame_t RTHooks__ReportFault_Frame_t;
 /* internal_declare_param */
 /* internal_declare_param */
void /*TypeText3*/ 
__cdecl
RTHooks__ReportFault(
   /* Param_Type1 */ ADDRESS module_L_3,
   /* Param_Type1 */ INTEGER info_L_4) M3_ATTRIBUTE_NO_RETURN;
 /* end: imports */
 /* begin: locals */
 /* declare_segment name:<NIL> typeid:TFFFFFFFF const:TRUE */
/*declare_segment*/struct F4096x8m1_m_5_L_6_t;
/*declare_segment*/typedef struct F4096x8m1_m_5_L_6_t F4096x8m1_m_5_L_6_t;
 /* declare_segment name:M_F4096x8m1 typeid:TFFFFFFFF const:FALSE */
 /* handler_name_prefixes:F4096x8m1_M3_LINE_ */
 /* handler_name_prefixes:F4096x8m1_I3_LINE_ */
/*declare_segment*/struct F4096x8m1_m_M_F4096x8m1_L_7_t;
/*declare_segment*/typedef struct F4096x8m1_m_M_F4096x8m1_L_7_t F4096x8m1_m_M_F4096x8m1_L_7_t;
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct F4096x8m1_M3_Frame_t;typedef struct F4096x8m1_M3_Frame_t F4096x8m1_M3_Frame_t;
 /* internal_declare_param */
RT0__ModulePtr
__cdecl
F4096x8m1_M3(
   /* Param_Type1 */ INTEGER /*TypeText1*/  mode_L_8);
 /* declare_procedure */

#ifndef m3_CHAR
#define m3_CHAR m3_CHAR
typedef UCHAR /*TypeText1*/  m3_CHAR;
#endif
/*Proc_ForwardDeclareFrameType*/struct F4096x8m1__F1_Frame_t;typedef struct F4096x8m1__F1_Frame_t F4096x8m1__F1_Frame_t;
 /* declare_local */
 /* internal_declare_param */
m3_CHAR
__cdecl
F4096x8m1__F1(
   /* Param_Type1 */ T37224E3F* /*TypeText1*/  t_L_10);
 /* Locals_begin_procedure */
 /* begin_block */
 /* AllocateTemps_check_nil */
 /* AllocateTemps_common */
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
 /* init_proc */
 /* init_var */
 /* init_proc */
 /* init_var */
 /* init_chars */
 /* end_init */
struct F4096x8m1_m_5_L_6_t{UINT8 L_13[12];
char L_14[1];
UINT8 L_15[2];
char L_16[1];
ADDRESS L_17[4];
char L_18[8];
UINT8 L_19[15];
char L_20[9];
};
static  const F4096x8m1_m_5_L_6_t F4096x8m1_m_5_L_6={{'F','4','0','9','6','x','8','m','1','_','M','3'},{0 /* 1 */ ,},{'F','1'},{0 /* 1 */ ,},{(ADDRESS)&F4096x8m1_M3,(char*)&F4096x8m1_m_5_L_6,(ADDRESS)&F4096x8m1__F1,13+(char*)&F4096x8m1_m_5_L_6},{0 /* 1 */ ,0 /* 2 */ ,0 /* 3 */ ,0 /* 4 */ ,0 /* 5 */ ,0 /* 6 */ ,0 /* 7 */ ,0 /* 8 */ ,},{'.','.','/','F','4','0','9','6','x','8','m','1','.','m','3'},{0 /* 1 */ ,0 /* 2 */ ,0 /* 3 */ ,0 /* 4 */ ,0 /* 5 */ ,0 /* 6 */ ,0 /* 7 */ ,0 /* 8 */ ,0 /* 9 */ ,}};
 /* bind_segment */
 /* begin_init */
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
 /* end_init */
struct F4096x8m1_m_M_F4096x8m1_L_7_t{ADDRESS L_21[1];
char L_22[32];
ADDRESS L_23[1];
char L_24[24];
ADDRESS L_25[1];
char L_26[8];
ADDRESS L_27[1];
INT64 L_28[1];
char L_29[8];
ADDRESS L_30[2];
char L_31[8];
ADDRESS L_32[2];
char L_33[8];
ADDRESS L_34[1];
char L_35[8];
};
static F4096x8m1_m_M_F4096x8m1_L_7_t F4096x8m1_m_M_F4096x8m1_L_7={{56+(char*)&F4096x8m1_m_5_L_6},{0 /* 1 */ ,0 /* 2 */ ,0 /* 3 */ ,0 /* 4 */ ,0 /* 5 */ ,0 /* 6 */ ,0 /* 7 */ ,0 /* 8 */ ,0 /* 9 */ ,0 /* 10 */ ,0 /* 11 */ ,0 /* 12 */ ,0 /* 13 */ ,0 /* 14 */ ,0 /* 15 */ ,0 /* 16 */ ,0 /* 17 */ ,0 /* 18 */ ,0 /* 19 */ ,0 /* 20 */ ,0 /* 21 */ ,0 /* 22 */ ,0 /* 23 */ ,0 /* 24 */ ,0 /* 25 */ ,0 /* 26 */ ,0 /* 27 */ ,0 /* 28 */ ,0 /* 29 */ ,0 /* 30 */ ,0 /* 31 */ ,0 /* 32 */ ,},{16+(char*)&F4096x8m1_m_5_L_6},{0 /* 1 */ ,0 /* 2 */ ,0 /* 3 */ ,0 /* 4 */ ,0 /* 5 */ ,0 /* 6 */ ,0 /* 7 */ ,0 /* 8 */ ,0 /* 9 */ ,0 /* 10 */ ,0 /* 11 */ ,0 /* 12 */ ,0 /* 13 */ ,0 /* 14 */ ,0 /* 15 */ ,0 /* 16 */ ,0 /* 17 */ ,0 /* 18 */ ,0 /* 19 */ ,0 /* 20 */ ,0 /* 21 */ ,0 /* 22 */ ,0 /* 23 */ ,0 /* 24 */ ,},{104+(char*)&F4096x8m1_m_M_F4096x8m1_L_7},{0 /* 1 */ ,0 /* 2 */ ,0 /* 3 */ ,0 /* 4 */ ,0 /* 5 */ ,0 /* 6 */ ,0 /* 7 */ ,0 /* 8 */ ,},{(ADDRESS)&F4096x8m1_M3},{INT64_(3)},{0 /* 1 */ ,0 /* 2 */ ,0 /* 3 */ ,0 /* 4 */ 
,0 /* 5 */ ,0 /* 6 */ ,0 /* 7 */ ,0 /* 8 */ ,},{(ADDRESS)&F4096x8m1_I3,128+(char*)&F4096x8m1_m_M_F4096x8m1_L_7},{0 /* 1 */ ,0 /* 2 */ ,0 /* 3 */ ,0 /* 4 */ ,0 /* 5 */ ,0 /* 6 */ ,0 /* 7 */ ,0 /* 8 */ ,},{(ADDRESS)&F0_I3,152+(char*)&F4096x8m1_m_M_F4096x8m1_L_7},{0 /* 1 */ ,0 /* 2 */ ,0 /* 3 */ ,0 /* 4 */ ,0 /* 5 */ ,0 /* 6 */ ,0 /* 7 */ ,0 /* 8 */ ,},{(ADDRESS)&RTHooks_I3},{0 /* 1 */ ,0 /* 2 */ ,0 /* 3 */ ,0 /* 4 */ ,0 /* 5 */ ,0 /* 6 */ ,0 /* 7 */ ,0 /* 8 */ ,}};
static void __cdecl F4096x8m1_m_M_F4096x8m1_L_7_CRASH(WORD_T code) M3_ATTRIBUTE_NO_RETURN;
static void __cdecl F4096x8m1_m_M_F4096x8m1_L_7_CRASH(WORD_T code){RTHooks__ReportFault((ADDRESS)&F4096x8m1_m_M_F4096x8m1_L_7,code);} /* end: segments/globals */
 /* begin: mark used */
 /* end: mark used */
 /* set_source_file */
 /* set_source_line */
#line 3 "../F4096x8m1.m3"
 /* module global constants */
#line 3 "../F4096x8m1.m3"
 /* module global data */
#line 3 "../F4096x8m1.m3"
 /* set_source_line */
#line 3 "../F4096x8m1.m3"
#line 11 "../F4096x8m1.m3"
 /* F1 */
#line 11 "../F4096x8m1.m3"
 /* set_source_line */
#line 11 "../F4096x8m1.m3"
#line 6 "../F4096x8m1.m3"
 /* begin_procedure */
#line 6 "../F4096x8m1.m3"
struct F4096x8m1__F1_Frame_t {
#line 6 "../F4096x8m1.m3"
ADDRESS _unused;
#line 6 "../F4096x8m1.m3"
};
#line 6 "../F4096x8m1.m3"
m3_CHAR
__cdecl
F4096x8m1__F1(
   /* Param_Type1 */ T37224E3F* /*TypeText1*/  t_L_10)
{
#line 6 "../F4096x8m1.m3"
 /* Var_Type2 */ ADDRESS F4096x8m1_m_11_L_12={0};//always-init
#line 6 "../F4096x8m1.m3"
F4096x8m1__F1_Frame_t _frame;
#line 6 "../F4096x8m1.m3"
_frame._unused=(ADDRESS)&_frame;
#line 6 "../F4096x8m1.m3"
 /* set_source_line */
#line 6 "../F4096x8m1.m3"
#line 7 "../F4096x8m1.m3"
 /* set_source_line */
#line 7 "../F4096x8m1.m3"
#line 8 "../F4096x8m1.m3"
 /* load */
#line 8 "../F4096x8m1.m3"
 /* check_nil */
#line 8 "../F4096x8m1.m3"
 /* store */
#line 8 "../F4096x8m1.m3"
(*(ADDRESS*)(&F4096x8m1_m_11_L_12))=(ADDRESS)(((ADDRESS)(t_L_10)));
#line 8 "../F4096x8m1.m3"
 /* load */
#line 8 "../F4096x8m1.m3"
/*check_nil*/if(!F4096x8m1_m_11_L_12)F4096x8m1_m_M_F4096x8m1_L_7_CRASH(260);
#line 8 "../F4096x8m1.m3"
 /* load_indirect */
#line 8 "../F4096x8m1.m3"
 /* exit_proc */
#line 8 "../F4096x8m1.m3"
return ((INT64)(*((UINT8*)(F4096x8m1_m_11_L_12))));
#line 8 "../F4096x8m1.m3"
 /* end_procedure */
#line 8 "../F4096x8m1.m3"
} /* F4096x8m1_M3 */
#line 8 "../F4096x8m1.m3"
 /* module main body F4096x8m1_M3 */
#line 8 "../F4096x8m1.m3"
 /* set_source_line */
#line 8 "../F4096x8m1.m3"
#line 11 "../F4096x8m1.m3"
 /* begin_procedure */
#line 11 "../F4096x8m1.m3"
struct F4096x8m1_M3_Frame_t {
#line 11 "../F4096x8m1.m3"
ADDRESS _unused;
#line 11 "../F4096x8m1.m3"
};
#line 11 "../F4096x8m1.m3"
RT0__ModulePtr
__cdecl
F4096x8m1_M3(
   /* Param_Type1 */ INTEGER /*TypeText1*/  mode_L_8)
{
#line 11 "../F4096x8m1.m3"
F4096x8m1_M3_Frame_t _frame;
#line 11 "../F4096x8m1.m3"
_frame._unused=(ADDRESS)&_frame;
#line 11 "../F4096x8m1.m3"
 /* load */
#line 11 "../F4096x8m1.m3"
 /* if_true_or_false */
#line 11 "../F4096x8m1.m3"
 /* load_host_integer */
#line 11 "../F4096x8m1.m3"
 /* load_integer */
#line 11 "../F4096x8m1.m3"
 /* if_compare */
#line 11 "../F4096x8m1.m3"
if(m3_eq(INT64,
  mode_L_8,
   INT64_(0)))goto L1;
#line 11 "../F4096x8m1.m3"
 /* set_label */
#line 11 "../F4096x8m1.m3"
L1:;
#line 11 "../F4096x8m1.m3"
 /* load_address */
#line 11 "../F4096x8m1.m3"
 /* exit_proc */
#line 11 "../F4096x8m1.m3"
return (RT0__ModulePtr)(&F4096x8m1_m_M_F4096x8m1_L_7);
#line 11 "../F4096x8m1.m3"
 /* end_procedure */
#line 11 "../F4096x8m1.m3"
} /* global constant type descriptor */
#line 11 "../F4096x8m1.m3"
 /* global data type descriptor */
#line 11 "../F4096x8m1.m3"
 /* module global constants */
#line 11 "../F4096x8m1.m3"
 /* procedure names */
#line 11 "../F4096x8m1.m3"
 /* procedure table */
#line 11 "../F4096x8m1.m3"
 /* file name */
#line 11 "../F4096x8m1.m3"
 /* module global data */
#line 11 "../F4096x8m1.m3"
 /* load map


 global data allocation for M_F4096x8m1
     0   104  8  *module info*
   104    24  8  import F4096x8m1
   128    24  8  import F0
   152    24  8  import RTHooks
   176     0  8  *TOTAL*


 global constants for M_F4096x8m1
     0    16  8  *proc names*
    16    40  8  *proc info*
    56    16  1  *string*
    72     0  8  *TOTAL*
 */
#line 11 "../F4096x8m1.m3"
 /* end unit */
#line 11 "../F4096x8m1.m3"

#ifdef __cplusplus

} /* extern "C" */
#endif
 /* set_runtime_proc */
 /* set_runtime_proc */
 /* set_runtime_proc */
