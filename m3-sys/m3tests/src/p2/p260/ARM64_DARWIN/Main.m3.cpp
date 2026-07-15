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
 /* declare_open_array */
/*array_forwardDeclare*/struct TF400F3DB;typedef struct TF400F3DB TF400F3DB;

#ifndef TF400F3DB
#define TF400F3DB TF400F3DB
/*openArray_define*/struct TF400F3DB{
INTEGER*_elts;
CARDINAL _size;
};

#endif
 /* declare_pointer */
typedef TF400F3DB*T217FBA22;
 /* declare_indirect */
typedef TF400F3DB*TBFF0C24;
 /* declare_proctype */
 /* declare_formal */
 /* declare_proctype */
 /* declare_formal */
 /* declare_proctype */
 /* declare_formal */
 /* declare_proctype */

#if 0 /* avoid type hash collions */
typedef 
void(__cdecl*T7B78C34F)(void);
#else
typedef void (__cdecl*T7B78C34F)(void);
#endif
 /* declare_open_array */
/*array_forwardDeclare*/struct T85BA34C3;typedef struct T85BA34C3 T85BA34C3;

#ifndef T85BA34C3
#define T85BA34C3 T85BA34C3
/*openArray_define*/struct T85BA34C3{
TF400F3DB**_elts;
CARDINAL _sizes[2];
};

#endif
 /* declare_pointer */
typedef T85BA34C3*T7990149A;
 /* declare_open_array */
/*array_forwardDeclare*/struct TDD559A7B;typedef struct TDD559A7B TDD559A7B;

#ifndef TDD559A7B
#define TDD559A7B TDD559A7B
/*openArray_define*/struct TDD559A7B{
T85BA34C3***_elts;
CARDINAL _sizes[3];
};

#endif
 /* declare_pointer */
typedef TDD559A7B*TD90C01E6;
 /* declare_open_array */
/*array_forwardDeclare*/struct T7DC98F07;typedef struct T7DC98F07 T7DC98F07;

#ifndef T7DC98F07
#define T7DC98F07 T7DC98F07
/*openArray_define*/struct T7DC98F07{
TDD559A7B****_elts;
CARDINAL _sizes[4];
};

#endif
 /* declare_pointer */
typedef T7DC98F07*TC7F3037E;
 /* declare_proctype */

#if 0 /* avoid type hash collions */
typedef 
TF400F3DB*(__cdecl*T87A8BB5C)(void);
#else
typedef void (__cdecl*T87A8BB5C)(void);
#endif
 /* declare_proctype */
 /* declare_formal */
 /* declare_formal */
 /* declare_proctype */
 /* declare_formal */
 /* declare_formal */
 /* declare_record */
 /* declare_record */
 /* DeclareTypes_FlushOnce size:5 */

#if 0 /* avoid type hash collions */
typedef 
void(__cdecl*TA51D61EF)(TF400F3DB*);
#else
typedef void (__cdecl*TA51D61EF)(void);
#endif

#if 0 /* avoid type hash collions */
typedef 
void(__cdecl*T9F8096CF)(TF400F3DB*);
#else
typedef void (__cdecl*T9F8096CF)(void);
#endif

#if 0 /* avoid type hash collions */
typedef 
TF400F3DB*(__cdecl*T53D1E734)(TF400F3DB*);
#else
typedef void (__cdecl*T53D1E734)(void);
#endif

#if 0 /* avoid type hash collions */
typedef 
REFANY(__cdecl*T983B02E7)(ADDRESS,TF400F3DB*);
#else
typedef void (__cdecl*T983B02E7)(void);
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
#define m3_pop_T(T) static void __stdcall m3_pop_##T(volatile T a) { }

#ifndef m3_pop_INT64
#define m3_pop_INT64 m3_pop_INT64
m3_pop_T(INT64)
#endif
 /* end: helper functions */

#ifndef struct_16_t
#define struct_16_t struct_16_t
STRUCT8(16)
#endif

#ifndef struct_24_t
#define struct_24_t struct_24_t
STRUCT8(24)
#endif

#ifndef struct_32_t
#define struct_32_t struct_32_t
STRUCT8(32)
#endif

#ifndef struct_40_t
#define struct_40_t struct_40_t
STRUCT8(40)
#endif

#ifndef struct_48_t
#define struct_48_t struct_48_t
STRUCT8(48)
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
 /* import_procedure */
/*Proc_ForwardDeclareFrameType*/struct RTHooks__AllocateOpenArray_Frame_t;typedef struct RTHooks__AllocateOpenArray_Frame_t RTHooks__AllocateOpenArray_Frame_t;
 /* internal_declare_param */
 /* internal_declare_param */
REFANY
__cdecl
RTHooks__AllocateOpenArray(
   /* Param_Type1 */ ADDRESS t_L_2,
   /* Param_Type1 */ TF400F3DB* /*TypeText1*/  sizes_L_3);
 /* import_procedure */
/*Proc_ForwardDeclareFrameType*/struct RTHooks__ReportFault_Frame_t;typedef struct RTHooks__ReportFault_Frame_t RTHooks__ReportFault_Frame_t;
 /* internal_declare_param */
 /* internal_declare_param */
void /*TypeText3*/ 
__cdecl
RTHooks__ReportFault(
   /* Param_Type1 */ ADDRESS module_L_4,
   /* Param_Type1 */ INTEGER info_L_5) M3_ATTRIBUTE_NO_RETURN;
 /* end: imports */
 /* begin: locals */
 /* declare_segment name:<NIL> typeid:TFFFFFFFF const:TRUE */
/*declare_segment*/struct Main_m_6_L_7_t;
/*declare_segment*/typedef struct Main_m_6_L_7_t Main_m_6_L_7_t;
 /* declare_segment name:M_Main typeid:TFFFFFFFF const:FALSE */
 /* handler_name_prefixes:Main_M3_LINE_ */
 /* handler_name_prefixes:Main_I3_LINE_ */
/*declare_segment*/struct Main_m_M_Main_L_8_t;
/*declare_segment*/typedef struct Main_m_M_Main_L_8_t Main_m_M_Main_L_8_t;
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct Main_M3_Frame_t;typedef struct Main_M3_Frame_t Main_M3_Frame_t;
 /* internal_declare_param */
RT0__ModulePtr
__cdecl
Main_M3(
   /* Param_Type1 */ INTEGER /*TypeText1*/  mode_L_9);
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct Main__F2_Frame_t;typedef struct Main__F2_Frame_t Main__F2_Frame_t;
 /* internal_declare_param */
void /*TypeText3*/ 
__cdecl
Main__F2(
   /* Param_Type1 */ TF400F3DB* /*TypeText1*/  b_L_10);
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct Main__F3_Frame_t;typedef struct Main__F3_Frame_t Main__F3_Frame_t;
 /* internal_declare_param */
void /*TypeText3*/ 
__cdecl
Main__F3(
   /* Param_Type1 */ TF400F3DB* /*TypeText1*/  b_L_11);
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct Main__F4_Frame_t;typedef struct Main__F4_Frame_t Main__F4_Frame_t;
 /* declare_local */
 /* internal_declare_param */
TF400F3DB* /*TypeText1*/ 
__cdecl
Main__F4(
   /* Param_Type1 */ TF400F3DB* /*TypeText1*/  b_L_13);
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct Main__F5_Frame_t;typedef struct Main__F5_Frame_t Main__F5_Frame_t;
void /*TypeText3*/ 
__cdecl
Main__F5(void);
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct Main__F6_Frame_t;typedef struct Main__F6_Frame_t Main__F6_Frame_t;
void /*TypeText3*/ 
__cdecl
Main__F6(void);
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct Main__F6v_Frame_t;typedef struct Main__F6v_Frame_t Main__F6v_Frame_t;
void /*TypeText3*/ 
__cdecl
Main__F6v(void);
 /* declare_local */
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct Main__F6_3_Frame_t;typedef struct Main__F6_3_Frame_t Main__F6_3_Frame_t;
void /*TypeText3*/ 
__cdecl
Main__F6_3(void);
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct Main__F6_3v_Frame_t;typedef struct Main__F6_3v_Frame_t Main__F6_3v_Frame_t;
void /*TypeText3*/ 
__cdecl
Main__F6_3v(void);
 /* declare_local */
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct Main__F6_4v_Frame_t;typedef struct Main__F6_4v_Frame_t Main__F6_4v_Frame_t;
void /*TypeText3*/ 
__cdecl
Main__F6_4v(void);
 /* declare_local */
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct Main__F6_4_Frame_t;typedef struct Main__F6_4_Frame_t Main__F6_4_Frame_t;
void /*TypeText3*/ 
__cdecl
Main__F6_4(void);
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct Main__F7_Frame_t;typedef struct Main__F7_Frame_t Main__F7_Frame_t;
void /*TypeText3*/ 
__cdecl
Main__F7(void);
 /* declare_local */
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct Main__F1_Frame_t;typedef struct Main__F1_Frame_t Main__F1_Frame_t;
TF400F3DB* /*TypeText1*/ 
__cdecl
Main__F1(void);
 /* declare_local */
 /* declare_local */
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
 /* declare_temp */
 /* declare_local */
 /* Locals_end_procedure */
 /* end_block */
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
 /* declare_temp */
 /* declare_local */
 /* Locals_end_procedure */
 /* end_block */
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
 /* declare_temp */
 /* declare_local */
 /* Locals_end_procedure */
 /* end_block */
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
 /* declare_temp */
 /* declare_local */
 /* Locals_end_procedure */
 /* end_block */
 /* Locals_begin_procedure */
 /* begin_block */
 /* declare_temp */
 /* declare_local */
 /* declare_temp */
 /* declare_local */
 /* declare_temp */
 /* declare_local */
 /* AllocateTemps_check_hi */
 /* AllocateTemps_common */
 /* declare_temp */
 /* declare_local */
 /* declare_temp */
 /* declare_local */
 /* declare_temp */
 /* declare_local */
 /* AllocateTemps_check_hi */
 /* AllocateTemps_common */
 /* declare_temp */
 /* declare_local */
 /* Locals_end_procedure */
 /* end_block */
 /* Locals_begin_procedure */
 /* begin_block */
 /* declare_temp */
 /* declare_local */
 /* declare_temp */
 /* declare_local */
 /* declare_temp */
 /* declare_local */
 /* AllocateTemps_check_hi */
 /* AllocateTemps_common */
 /* declare_temp */
 /* declare_local */
 /* declare_temp */
 /* declare_local */
 /* declare_temp */
 /* declare_local */
 /* AllocateTemps_check_hi */
 /* AllocateTemps_common */
 /* declare_temp */
 /* declare_local */
 /* Locals_end_procedure */
 /* end_block */
 /* Locals_begin_procedure */
 /* begin_block */
 /* declare_temp */
 /* declare_local */
 /* declare_temp */
 /* declare_local */
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
 /* init_proc */
 /* init_var */
 /* init_proc */
 /* init_var */
 /* init_proc */
 /* init_var */
 /* init_proc */
 /* init_var */
 /* init_proc */
 /* init_var */
 /* init_proc */
 /* init_var */
 /* init_proc */
 /* init_var */
 /* init_proc */
 /* init_var */
 /* init_proc */
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
 /* end_init */
struct Main_m_6_L_7_t{UINT8 L_84[7];
char L_85[1];
UINT8 L_86[2];
char L_87[1];
UINT8 L_88[2];
char L_89[1];
UINT8 L_90[4];
char L_91[1];
UINT8 L_92[5];
char L_93[1];
UINT8 L_94[5];
char L_95[1];
UINT8 L_96[4];
char L_97[1];
UINT8 L_98[3];
char L_99[1];
UINT8 L_100[2];
char L_101[1];
UINT8 L_102[2];
char L_103[1];
UINT8 L_104[2];
char L_105[1];
UINT8 L_106[2];
char L_107[1];
UINT8 L_108[2];
char L_109[2];
ADDRESS L_110[26];
char L_111[8];
UINT8 L_112[10];
char L_113[1];
INT8 L_114[27];
char L_115[10];
};
static  const Main_m_6_L_7_t Main_m_6_L_7={{'M','a','i','n','_','M','3'},{0 /* 1 */ ,},{'F','1'},{0 /* 1 */ ,},{'F','7'},{0 /* 1 */ ,},{'F','6','_','4'},{0 /* 1 */ ,},{'F','6','_','4','v'},{0 /* 1 */ ,},{'F','6','_','3','v'},{0 /* 1 */ ,},{'F','6','_','3'},{0 /* 1 */ ,},{'F','6','v'},{0 /* 1 */ ,},{'F','6'},{0 /* 1 */ ,},{'F','5'},{0 /* 1 */ ,},{'F','4'},{0 /* 1 */ ,},{'F','3'},{0 /* 1 */ ,},{'F','2'},{0 /* 1 */ ,0 /* 2 */ ,},{(ADDRESS)&Main_M3,(char*)&Main_m_6_L_7,(ADDRESS)&Main__F1,8+(char*)&Main_m_6_L_7,(ADDRESS)&Main__F7,11+(char*)&Main_m_6_L_7,(ADDRESS)&Main__F6_4,14+(char*)&Main_m_6_L_7,(ADDRESS)&Main__F6_4v,19+(char*)&Main_m_6_L_7,(ADDRESS)&Main__F6_3v,25+(char*)&Main_m_6_L_7,(ADDRESS)&Main__F6_3,31+(char*)&Main_m_6_L_7,(ADDRESS)&Main__F6v,36+(char*)&Main_m_6_L_7,(ADDRESS)&Main__F6,40+(char*)&Main_m_6_L_7,(ADDRESS)&Main__F5,43+(char*)&Main_m_6_L_7,(ADDRESS)&Main__F4,46+(char*)&Main_m_6_L_7,(ADDRESS)&Main__F3,49+(char*)&Main_m_6_L_7,(ADDRESS)&Main__F2,52+(char*)&Main_m_6_L_7},{0 /* 1 */ 
,0 /* 2 */ ,0 /* 3 */ ,0 /* 4 */ ,0 /* 5 */ ,0 /* 6 */ ,0 /* 7 */ ,0 /* 8 */ ,},{'.','.','/','M','a','i','n','.','m','3'},{0 /* 1 */ ,},{((INT8)24),((INT8)4),((INT8)15),((INT8)0),((INT8)0),((INT8)2),((INT8)13),((INT8)4),((INT8)7),((INT8)24),((INT8)3),((INT8)15),((INT8)0),((INT8)0),((INT8)2),((INT8)13),((INT8)3),((INT8)7),((INT8)24),((INT8)2),((INT8)15),((INT8)0),((INT8)0),((INT8)2),((INT8)13),((INT8)2),((INT8)7)},{0 /* 1 */ ,0 /* 2 */ ,0 /* 3 */ ,0 /* 4 */ ,0 /* 5 */ ,0 /* 6 */ ,0 /* 7 */ ,0 /* 8 */ ,0 /* 9 */ ,0 /* 10 */ ,}};
 /* bind_segment */
 /* begin_init */
 /* init_var */
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
 /* init_var */
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
 /* init_int */
 /* init_var */
 /* init_var */
 /* init_var */
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
 /* init_int */
 /* init_var */
 /* init_var */
 /* init_int */
 /* init_int */
 /* init_proc */
 /* init_var */
 /* init_proc */
 /* init_var */
 /* init_int */
 /* init_var */
 /* init_int */
 /* init_var */
 /* init_int */
 /* init_int */
 /* end_init */
struct Main_m_M_Main_L_8_t{ADDRESS L_116[3];
char L_117[16];
ADDRESS L_118[1];
char L_119[24];
ADDRESS L_120[1];
char L_121[8];
ADDRESS L_122[1];
INT64 L_123[1];
char L_124[8];
INT64 L_125[1];
UINT8 L_126[3];
INT8 L_127[2];
UINT8 L_128[1];
INT8 L_129[4];
char L_130[1];
INT8 L_131[1];
char L_132[4];
INT64 L_133[1];
ADDRESS L_134[1];
char L_135[8];
ADDRESS L_136[1];
char L_137[24];
ADDRESS L_138[1];
INT64 L_139[2];
char L_140[8];
INT64 L_141[1];
UINT8 L_142[1];
INT8 L_143[1];
UINT8 L_144[2];
INT8 L_145[2];
UINT8 L_146[1];
INT8 L_147[3];
char L_148[1];
INT8 L_149[1];
char L_150[4];
INT64 L_151[1];
ADDRESS L_152[1];
char L_153[8];
ADDRESS L_154[1];
char L_155[24];
ADDRESS L_156[1];
INT64 L_157[2];
char L_158[8];
INT64 L_159[1];
INT8 L_160[1];
UINT8 L_161[3];
INT8 L_162[1];
UINT8 L_163[1];
INT8 L_164[4];
char L_165[1];
INT8 L_166[1];
char L_167[4];
INT64 L_168[1];
ADDRESS L_169[1];
char L_170[8];
ADDRESS L_171[1];
char L_172[32];
INT64 L_173[2];
char L_174[8];
ADDRESS L_175[2];
char L_176[8];
ADDRESS L_177[1];
char L_178[8];
ADDRESS L_179[1];
INT64 L_180[1];
ADDRESS L_181[1];
INT64 L_182[1];
ADDRESS L_183[1];
INT64 L_184[1];
char L_185[8];
INT64 L_186[1];
char L_187[8];
};
static Main_m_M_Main_L_8_t Main_m_M_Main_L_8={{272+(char*)&Main_m_6_L_7,104+(char*)&Main_m_M_Main_L_8,488+(char*)&Main_m_M_Main_L_8},{0 /* 1 */ ,0 /* 2 */ ,0 /* 3 */ ,0 /* 4 */ ,0 /* 5 */ ,0 /* 6 */ ,0 /* 7 */ ,0 /* 8 */ ,0 /* 9 */ ,0 /* 10 */ ,0 /* 11 */ ,0 /* 12 */ ,0 /* 13 */ ,0 /* 14 */ ,0 /* 15 */ ,0 /* 16 */ ,},{56+(char*)&Main_m_6_L_7},{0 /* 1 */ ,0 /* 2 */ ,0 /* 3 */ ,0 /* 4 */ ,0 /* 5 */ ,0 /* 6 */ ,0 /* 7 */ ,0 /* 8 */ ,0 /* 9 */ ,0 /* 10 */ ,0 /* 11 */ ,0 /* 12 */ ,0 /* 13 */ ,0 /* 14 */ ,0 /* 15 */ ,0 /* 16 */ ,0 /* 17 */ ,0 /* 18 */ ,0 /* 19 */ ,0 /* 20 */ ,0 /* 21 */ ,0 /* 22 */ ,0 /* 23 */ ,0 /* 24 */ ,},{440+(char*)&Main_m_M_Main_L_8},{0 /* 1 */ ,0 /* 2 */ ,0 /* 3 */ ,0 /* 4 */ ,0 /* 5 */ ,0 /* 6 */ ,0 /* 7 */ ,0 /* 8 */ ,},{(ADDRESS)&Main_M3},{INT64_(3)},{0 /* 1 */ ,0 /* 2 */ ,0 /* 3 */ ,0 /* 4 */ ,0 /* 5 */ ,0 /* 6 */ ,0 /* 7 */ ,0 /* 8 */ ,},{INT64_(2039485594)},{181U,153U,153U},{((INT8)123),((INT8)47)},{141U},{((INT8)9),((INT8)2),((INT8)1),((INT8)3)},{0 /* 1 */ ,},{
((INT8)8)},{0 /* 1 */ ,0 /* 2 */ ,0 /* 3 */ ,0 /* 4 */ ,},{INT64_(24)},{301+(char*)&Main_m_6_L_7},{0 /* 1 */ ,0 /* 2 */ ,0 /* 3 */ ,0 /* 4 */ ,0 /* 5 */ ,0 /* 6 */ ,0 /* 7 */ ,0 /* 8 */ ,},{306+(char*)&Main_m_6_L_7},{0 /* 1 */ ,0 /* 2 */ ,0 /* 3 */ ,0 /* 4 */ ,0 /* 5 */ ,0 /* 6 */ ,0 /* 7 */ ,0 /* 8 */ ,0 /* 9 */ ,0 /* 10 */ ,0 /* 11 */ ,0 /* 12 */ ,0 /* 13 */ ,0 /* 14 */ ,0 /* 15 */ ,0 /* 16 */ ,0 /* 17 */ ,0 /* 18 */ ,0 /* 19 */ ,0 /* 20 */ ,0 /* 21 */ ,0 /* 22 */ ,0 /* 23 */ ,0 /* 24 */ ,},{216+(char*)&Main_m_M_Main_L_8},{INT64_(2),INT64_(8)},{0 /* 1 */ ,0 /* 2 */ ,0 /* 3 */ ,0 /* 4 */ ,0 /* 5 */ ,0 /* 6 */ ,0 /* 7 */ ,0 /* 8 */ ,},{INT64_(-653524506)},{233U},{((INT8)53)},{237U,203U},{((INT8)15),((INT8)52)},{225U},{((INT8)18),((INT8)1),((INT8)3)},{0 /* 1 */ ,},{((INT8)8)},{0 /* 1 */ ,0 /* 2 */ ,0 /* 3 */ ,0 /* 4 */ ,},{INT64_(32)},{292+(char*)&Main_m_6_L_7},{0 /* 1 */ ,0 /* 2 */ ,0 /* 3 */ ,0 /* 4 */ ,0 /* 5 */ ,0 /* 6 */ ,0 /* 7 */ ,0 /* 8 */ ,},{297+(char*)&Main_m_6_L_7},{0 /* 1 */ 
,0 /* 2 */ ,0 /* 3 */ ,0 /* 4 */ ,0 /* 5 */ ,0 /* 6 */ ,0 /* 7 */ ,0 /* 8 */ ,0 /* 9 */ ,0 /* 10 */ ,0 /* 11 */ ,0 /* 12 */ ,0 /* 13 */ ,0 /* 14 */ ,0 /* 15 */ ,0 /* 16 */ ,0 /* 17 */ ,0 /* 18 */ ,0 /* 19 */ ,0 /* 20 */ ,0 /* 21 */ ,0 /* 22 */ ,0 /* 23 */ ,0 /* 24 */ ,},{328+(char*)&Main_m_M_Main_L_8},{INT64_(3),INT64_(8)},{0 /* 1 */ ,0 /* 2 */ ,0 /* 3 */ ,0 /* 4 */ ,0 /* 5 */ ,0 /* 6 */ ,0 /* 7 */ ,0 /* 8 */ ,},{INT64_(-940375170)},{((INT8)13)},{144U,210U,196U},{((INT8)115)},{147U},{((INT8)33),((INT8)3),((INT8)1),((INT8)3)},{0 /* 1 */ ,},{((INT8)8)},{0 /* 1 */ ,0 /* 2 */ ,0 /* 3 */ ,0 /* 4 */ ,},{INT64_(40)},{283+(char*)&Main_m_6_L_7},{0 /* 1 */ ,0 /* 2 */ ,0 /* 3 */ ,0 /* 4 */ ,0 /* 5 */ ,0 /* 6 */ ,0 /* 7 */ ,0 /* 8 */ ,},{288+(char*)&Main_m_6_L_7},{0 /* 1 */ ,0 /* 2 */ ,0 /* 3 */ ,0 /* 4 */ ,0 /* 5 */ ,0 /* 6 */ ,0 /* 7 */ ,0 /* 8 */ ,0 /* 9 */ ,0 /* 10 */ ,0 /* 11 */ ,0 /* 12 */ ,0 /* 13 */ ,0 /* 14 */ ,0 /* 15 */ ,0 /* 16 */ ,0 /* 17 */ ,0 /* 18 */ ,0 /* 19 */ ,0 /* 20 */ ,0 /* 21 */ 
,0 /* 22 */ ,0 /* 23 */ ,0 /* 24 */ ,0 /* 25 */ ,0 /* 26 */ ,0 /* 27 */ ,0 /* 28 */ ,0 /* 29 */ ,0 /* 30 */ ,0 /* 31 */ ,0 /* 32 */ ,},{INT64_(4),INT64_(8)},{0 /* 1 */ ,0 /* 2 */ ,0 /* 3 */ ,0 /* 4 */ ,0 /* 5 */ ,0 /* 6 */ ,0 /* 7 */ ,0 /* 8 */ ,},{(ADDRESS)&Main_I3,464+(char*)&Main_m_M_Main_L_8},{0 /* 1 */ ,0 /* 2 */ ,0 /* 3 */ ,0 /* 4 */ ,0 /* 5 */ ,0 /* 6 */ ,0 /* 7 */ ,0 /* 8 */ ,},{(ADDRESS)&RTHooks_I3},{0 /* 1 */ ,0 /* 2 */ ,0 /* 3 */ ,0 /* 4 */ ,0 /* 5 */ ,0 /* 6 */ ,0 /* 7 */ ,0 /* 8 */ ,},{504+(char*)&Main_m_M_Main_L_8},{INT64_(562018850)},{520+(char*)&Main_m_M_Main_L_8},{INT64_(2039485594)},{536+(char*)&Main_m_M_Main_L_8},{INT64_(-653524506)},{0 /* 1 */ ,0 /* 2 */ ,0 /* 3 */ ,0 /* 4 */ ,0 /* 5 */ ,0 /* 6 */ ,0 /* 7 */ ,0 /* 8 */ ,},{INT64_(-940375170)},{0 /* 1 */ ,0 /* 2 */ ,0 /* 3 */ ,0 /* 4 */ ,0 /* 5 */ ,0 /* 6 */ ,0 /* 7 */ ,0 /* 8 */ ,}};
static void __cdecl Main_m_M_Main_L_8_CRASH(WORD_T code) M3_ATTRIBUTE_NO_RETURN;
static void __cdecl Main_m_M_Main_L_8_CRASH(WORD_T code){RTHooks__ReportFault((ADDRESS)&Main_m_M_Main_L_8,code);} /* end: segments/globals */
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
#line 54 "../Main.m3"
 /* F2 */
#line 54 "../Main.m3"
 /* set_source_line */
#line 54 "../Main.m3"
#line 5 "../Main.m3"
 /* begin_procedure */
#line 5 "../Main.m3"
struct Main__F2_Frame_t {
#line 5 "../Main.m3"
ADDRESS _unused;
#line 5 "../Main.m3"
};
#line 5 "../Main.m3"
void /*TypeText3*/ 
__cdecl
Main__F2(
   /* Param_Type1 */ TF400F3DB* /*TypeText1*/  b_L_10)
{
#line 5 "../Main.m3"
 /* Var_Type3 */ STRUCT(16) Main_m_20_L_21={0};//always-init
#line 5 "../Main.m3"
 /* Var_Type2 */ ADDRESS Main_m_22_L_23={0};//always-init
#line 5 "../Main.m3"
Main__F2_Frame_t _frame;
#line 5 "../Main.m3"
_frame._unused=(ADDRESS)&_frame;
#line 5 "../Main.m3"
 /* load */
#line 5 "../Main.m3"
 /* add_offset */
#line 5 "../Main.m3"
 /* store */
#line 5 "../Main.m3"
(*(ADDRESS*)(&Main_m_20_L_21))=(ADDRESS)(((ADDRESS)(((8)+(char*)(((ADDRESS)(b_L_10)))))));
#line 5 "../Main.m3"
 /* load_integer */
#line 5 "../Main.m3"
 /* store */
#line 5 "../Main.m3"
(*(INT64*)((8)+(char*)(&Main_m_20_L_21)))=(INT64)(  INT64_(1));
#line 5 "../Main.m3"
 /* start_call_direct */
#line 5 "../Main.m3"
 /* load */
#line 5 "../Main.m3"
 /* pop_param */
#line 5 "../Main.m3"
 /* load_address */
#line 5 "../Main.m3"
 /* pop_param */
#line 5 "../Main.m3"
 /* call_direct */
#line 5 "../Main.m3"
 /* store */
#line 5 "../Main.m3"
(*(ADDRESS*)(&Main_m_22_L_23))=(ADDRESS)(((ADDRESS)(RTHooks__AllocateOpenArray(
  ( ADDRESS )(((ADDRESS)(*((ADDRESS*)(INT64_(488)+((ADDRESS)(&Main_m_M_Main_L_8)))))) ),
  ( TF400F3DB* /*TypeText1*/  )(((ADDRESS)(&Main_m_20_L_21)) )))));
#line 5 "../Main.m3"
 /* load */
#line 5 "../Main.m3"
 /* load_indirect */
#line 5 "../Main.m3"
 /* load */
#line 5 "../Main.m3"
 /* load_indirect */
#line 5 "../Main.m3"
 /* load */
#line 5 "../Main.m3"
 /* load_indirect */
#line 5 "../Main.m3"
 /* copy_n */
#line 5 "../Main.m3"
m3_memcpy(
 *((ADDRESS*)(Main_m_22_L_23)),
 *((ADDRESS*)(b_L_10)),
 8*(size_t) *((INT64*)(INT64_(8)+((ADDRESS)(b_L_10)))));
#line 5 "../Main.m3"
 /* load */
#line 5 "../Main.m3"
 /* store */
#line 5 "../Main.m3"
(*(ADDRESS*)(&b_L_10))=(ADDRESS)(((ADDRESS)(Main_m_22_L_23)));
#line 5 "../Main.m3"
 /* exit_proc */
#line 5 "../Main.m3"
return;
#line 5 "../Main.m3"
 /* end_procedure */
#line 5 "../Main.m3"
} /* F3 */
#line 5 "../Main.m3"
 /* set_source_line */
#line 5 "../Main.m3"
#line 6 "../Main.m3"
 /* begin_procedure */
#line 6 "../Main.m3"
struct Main__F3_Frame_t {
#line 6 "../Main.m3"
ADDRESS _unused;
#line 6 "../Main.m3"
};
#line 6 "../Main.m3"
void /*TypeText3*/ 
__cdecl
Main__F3(
   /* Param_Type1 */ TF400F3DB* /*TypeText1*/  b_L_11)
{
#line 6 "../Main.m3"
Main__F3_Frame_t _frame;
#line 6 "../Main.m3"
_frame._unused=(ADDRESS)&_frame;
#line 6 "../Main.m3"
 /* exit_proc */
#line 6 "../Main.m3"
return;
#line 6 "../Main.m3"
 /* end_procedure */
#line 6 "../Main.m3"
} /* F4 */
#line 6 "../Main.m3"
 /* set_source_line */
#line 6 "../Main.m3"
#line 7 "../Main.m3"
 /* begin_procedure */
#line 7 "../Main.m3"
struct Main__F4_Frame_t {
#line 7 "../Main.m3"
ADDRESS _unused;
#line 7 "../Main.m3"
};
#line 7 "../Main.m3"
TF400F3DB* /*TypeText1*/ 
__cdecl
Main__F4(
   /* Param_Type1 */ TF400F3DB* /*TypeText1*/  b_L_13)
{
#line 7 "../Main.m3"
Main__F4_Frame_t _frame;
#line 7 "../Main.m3"
_frame._unused=(ADDRESS)&_frame;
#line 7 "../Main.m3"
 /* load */
#line 7 "../Main.m3"
 /* exit_proc */
#line 7 "../Main.m3"
return (TF400F3DB* /*TypeText1*/ )(b_L_13);
#line 7 "../Main.m3"
 /* end_procedure */
#line 7 "../Main.m3"
} /* F5 */
#line 7 "../Main.m3"
 /* set_source_line */
#line 7 "../Main.m3"
#line 9 "../Main.m3"
 /* begin_procedure */
#line 9 "../Main.m3"
struct Main__F5_Frame_t {
#line 9 "../Main.m3"
ADDRESS _unused;
#line 9 "../Main.m3"
};
#line 9 "../Main.m3"
void /*TypeText3*/ 
__cdecl
Main__F5(void)
{
#line 9 "../Main.m3"
 /* Var_Type3 */ STRUCT(24) Main_m_24_L_25={0};//always-init
#line 9 "../Main.m3"
 /* Var_Type2 */ ADDRESS Main_m_26_L_27={0};//always-init
#line 9 "../Main.m3"
Main__F5_Frame_t _frame;
#line 9 "../Main.m3"
_frame._unused=(ADDRESS)&_frame;
#line 9 "../Main.m3"
 /* set_source_line */
#line 9 "../Main.m3"
#line 10 "../Main.m3"
 /* load_address */
#line 10 "../Main.m3"
 /* store */
#line 10 "../Main.m3"
(*(ADDRESS*)(&Main_m_24_L_25))=(ADDRESS)(((ADDRESS)(INT64_(16)+((ADDRESS)(&Main_m_24_L_25)))));
#line 10 "../Main.m3"
 /* load_integer */
#line 10 "../Main.m3"
 /* store */
#line 10 "../Main.m3"
(*(INT64*)((8)+(char*)(&Main_m_24_L_25)))=(INT64)(  INT64_(1));
#line 10 "../Main.m3"
 /* load_integer */
#line 10 "../Main.m3"
 /* store */
#line 10 "../Main.m3"
(*(INT64*)((16)+(char*)(&Main_m_24_L_25)))=(INT64)(  INT64_(11));
#line 10 "../Main.m3"
 /* start_call_direct */
#line 10 "../Main.m3"
 /* load */
#line 10 "../Main.m3"
 /* pop_param */
#line 10 "../Main.m3"
 /* load_address */
#line 10 "../Main.m3"
 /* pop_param */
#line 10 "../Main.m3"
 /* call_direct */
#line 10 "../Main.m3"
 /* store */
#line 10 "../Main.m3"
(*(ADDRESS*)(&Main_m_26_L_27))=(ADDRESS)(((ADDRESS)(RTHooks__AllocateOpenArray(
  ( ADDRESS )(((ADDRESS)(*((ADDRESS*)(INT64_(488)+((ADDRESS)(&Main_m_M_Main_L_8)))))) ),
  ( TF400F3DB* /*TypeText1*/  )(((ADDRESS)(&Main_m_24_L_25)) )))));
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
} /* F6 */
#line 11 "../Main.m3"
 /* set_source_line */
#line 11 "../Main.m3"
#line 13 "../Main.m3"
 /* begin_procedure */
#line 13 "../Main.m3"
struct Main__F6_Frame_t {
#line 13 "../Main.m3"
ADDRESS _unused;
#line 13 "../Main.m3"
};
#line 13 "../Main.m3"
void /*TypeText3*/ 
__cdecl
Main__F6(void)
{
#line 13 "../Main.m3"
 /* Var_Type3 */ STRUCT(32) Main_m_28_L_29={0};//always-init
#line 13 "../Main.m3"
 /* Var_Type2 */ ADDRESS Main_m_30_L_31={0};//always-init
#line 13 "../Main.m3"
Main__F6_Frame_t _frame;
#line 13 "../Main.m3"
_frame._unused=(ADDRESS)&_frame;
#line 13 "../Main.m3"
 /* set_source_line */
#line 13 "../Main.m3"
#line 14 "../Main.m3"
 /* load_address */
#line 14 "../Main.m3"
 /* store */
#line 14 "../Main.m3"
(*(ADDRESS*)(&Main_m_28_L_29))=(ADDRESS)(((ADDRESS)(INT64_(16)+((ADDRESS)(&Main_m_28_L_29)))));
#line 14 "../Main.m3"
 /* load_integer */
#line 14 "../Main.m3"
 /* store */
#line 14 "../Main.m3"
(*(INT64*)((8)+(char*)(&Main_m_28_L_29)))=(INT64)(  INT64_(2));
#line 14 "../Main.m3"
 /* load_integer */
#line 14 "../Main.m3"
 /* store */
#line 14 "../Main.m3"
(*(INT64*)((16)+(char*)(&Main_m_28_L_29)))=(INT64)(  INT64_(11));
#line 14 "../Main.m3"
 /* load_integer */
#line 14 "../Main.m3"
 /* store */
#line 14 "../Main.m3"
(*(INT64*)((24)+(char*)(&Main_m_28_L_29)))=(INT64)(  INT64_(12));
#line 14 "../Main.m3"
 /* start_call_direct */
#line 14 "../Main.m3"
 /* load */
#line 14 "../Main.m3"
 /* pop_param */
#line 14 "../Main.m3"
 /* load_address */
#line 14 "../Main.m3"
 /* pop_param */
#line 14 "../Main.m3"
 /* call_direct */
#line 14 "../Main.m3"
 /* store */
#line 14 "../Main.m3"
(*(ADDRESS*)(&Main_m_30_L_31))=(ADDRESS)(((ADDRESS)(RTHooks__AllocateOpenArray(
  ( ADDRESS )(((ADDRESS)(*((ADDRESS*)(INT64_(504)+((ADDRESS)(&Main_m_M_Main_L_8)))))) ),
  ( TF400F3DB* /*TypeText1*/  )(((ADDRESS)(&Main_m_28_L_29)) )))));
#line 14 "../Main.m3"
 /* set_source_line */
#line 14 "../Main.m3"
#line 15 "../Main.m3"
 /* exit_proc */
#line 15 "../Main.m3"
return;
#line 15 "../Main.m3"
 /* end_procedure */
#line 15 "../Main.m3"
} /* F6v */
#line 15 "../Main.m3"
 /* set_source_line */
#line 15 "../Main.m3"
#line 17 "../Main.m3"
 /* begin_procedure */
#line 17 "../Main.m3"
struct Main__F6v_Frame_t {
#line 17 "../Main.m3"
ADDRESS _unused;
#line 17 "../Main.m3"
};
#line 17 "../Main.m3"
void /*TypeText3*/ 
__cdecl
Main__F6v(void)
{
#line 17 "../Main.m3"
 /* Var_Type1 */ T85BA34C3* a_L_14={0};//always-init
#line 17 "../Main.m3"
 /* Var_Type3 */ STRUCT(32) Main_m_32_L_33={0};//always-init
#line 17 "../Main.m3"
 /* Var_Type2 */ ADDRESS Main_m_34_L_35={0};//always-init
#line 17 "../Main.m3"
Main__F6v_Frame_t _frame;
#line 17 "../Main.m3"
_frame._unused=(ADDRESS)&_frame;
#line 17 "../Main.m3"
 /* set_source_line */
#line 17 "../Main.m3"
#line 18 "../Main.m3"
 /* load_address */
#line 18 "../Main.m3"
 /* store */
#line 18 "../Main.m3"
(*(ADDRESS*)(&Main_m_32_L_33))=(ADDRESS)(((ADDRESS)(INT64_(16)+((ADDRESS)(&Main_m_32_L_33)))));
#line 18 "../Main.m3"
 /* load_integer */
#line 18 "../Main.m3"
 /* store */
#line 18 "../Main.m3"
(*(INT64*)((8)+(char*)(&Main_m_32_L_33)))=(INT64)(  INT64_(2));
#line 18 "../Main.m3"
 /* load_integer */
#line 18 "../Main.m3"
 /* store */
#line 18 "../Main.m3"
(*(INT64*)((16)+(char*)(&Main_m_32_L_33)))=(INT64)(  INT64_(13));
#line 18 "../Main.m3"
 /* load_integer */
#line 18 "../Main.m3"
 /* store */
#line 18 "../Main.m3"
(*(INT64*)((24)+(char*)(&Main_m_32_L_33)))=(INT64)(  INT64_(14));
#line 18 "../Main.m3"
 /* start_call_direct */
#line 18 "../Main.m3"
 /* load */
#line 18 "../Main.m3"
 /* pop_param */
#line 18 "../Main.m3"
 /* load_address */
#line 18 "../Main.m3"
 /* pop_param */
#line 18 "../Main.m3"
 /* call_direct */
#line 18 "../Main.m3"
 /* store */
#line 18 "../Main.m3"
(*(ADDRESS*)(&Main_m_34_L_35))=(ADDRESS)(((ADDRESS)(RTHooks__AllocateOpenArray(
  ( ADDRESS )(((ADDRESS)(*((ADDRESS*)(INT64_(504)+((ADDRESS)(&Main_m_M_Main_L_8)))))) ),
  ( TF400F3DB* /*TypeText1*/  )(((ADDRESS)(&Main_m_32_L_33)) )))));
#line 18 "../Main.m3"
 /* load */
#line 18 "../Main.m3"
 /* store */
#line 18 "../Main.m3"
(*(ADDRESS*)(&a_L_14))=(ADDRESS)(((ADDRESS)(Main_m_34_L_35)));
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
} /* F6_3 */
#line 19 "../Main.m3"
 /* set_source_line */
#line 19 "../Main.m3"
#line 21 "../Main.m3"
 /* begin_procedure */
#line 21 "../Main.m3"
struct Main__F6_3_Frame_t {
#line 21 "../Main.m3"
ADDRESS _unused;
#line 21 "../Main.m3"
};
#line 21 "../Main.m3"
void /*TypeText3*/ 
__cdecl
Main__F6_3(void)
{
#line 21 "../Main.m3"
 /* Var_Type3 */ STRUCT(40) Main_m_36_L_37={0};//always-init
#line 21 "../Main.m3"
 /* Var_Type2 */ ADDRESS Main_m_38_L_39={0};//always-init
#line 21 "../Main.m3"
Main__F6_3_Frame_t _frame;
#line 21 "../Main.m3"
_frame._unused=(ADDRESS)&_frame;
#line 21 "../Main.m3"
 /* set_source_line */
#line 21 "../Main.m3"
#line 22 "../Main.m3"
 /* load_address */
#line 22 "../Main.m3"
 /* store */
#line 22 "../Main.m3"
(*(ADDRESS*)(&Main_m_36_L_37))=(ADDRESS)(((ADDRESS)(INT64_(16)+((ADDRESS)(&Main_m_36_L_37)))));
#line 22 "../Main.m3"
 /* load_integer */
#line 22 "../Main.m3"
 /* store */
#line 22 "../Main.m3"
(*(INT64*)((8)+(char*)(&Main_m_36_L_37)))=(INT64)(  INT64_(3));
#line 22 "../Main.m3"
 /* load_integer */
#line 22 "../Main.m3"
 /* store */
#line 22 "../Main.m3"
(*(INT64*)((16)+(char*)(&Main_m_36_L_37)))=(INT64)(  INT64_(15));
#line 22 "../Main.m3"
 /* load_integer */
#line 22 "../Main.m3"
 /* store */
#line 22 "../Main.m3"
(*(INT64*)((24)+(char*)(&Main_m_36_L_37)))=(INT64)(  INT64_(2));
#line 22 "../Main.m3"
 /* load_integer */
#line 22 "../Main.m3"
 /* store */
#line 22 "../Main.m3"
(*(INT64*)((32)+(char*)(&Main_m_36_L_37)))=(INT64)(  INT64_(3));
#line 22 "../Main.m3"
 /* start_call_direct */
#line 22 "../Main.m3"
 /* load */
#line 22 "../Main.m3"
 /* pop_param */
#line 22 "../Main.m3"
 /* load_address */
#line 22 "../Main.m3"
 /* pop_param */
#line 22 "../Main.m3"
 /* call_direct */
#line 22 "../Main.m3"
 /* store */
#line 22 "../Main.m3"
(*(ADDRESS*)(&Main_m_38_L_39))=(ADDRESS)(((ADDRESS)(RTHooks__AllocateOpenArray(
  ( ADDRESS )(((ADDRESS)(*((ADDRESS*)(INT64_(520)+((ADDRESS)(&Main_m_M_Main_L_8)))))) ),
  ( TF400F3DB* /*TypeText1*/  )(((ADDRESS)(&Main_m_36_L_37)) )))));
#line 22 "../Main.m3"
 /* set_source_line */
#line 22 "../Main.m3"
#line 23 "../Main.m3"
 /* exit_proc */
#line 23 "../Main.m3"
return;
#line 23 "../Main.m3"
 /* end_procedure */
#line 23 "../Main.m3"
} /* F6_3v */
#line 23 "../Main.m3"
 /* set_source_line */
#line 23 "../Main.m3"
#line 25 "../Main.m3"
 /* begin_procedure */
#line 25 "../Main.m3"
struct Main__F6_3v_Frame_t {
#line 25 "../Main.m3"
ADDRESS _unused;
#line 25 "../Main.m3"
};
#line 25 "../Main.m3"
void /*TypeText3*/ 
__cdecl
Main__F6_3v(void)
{
#line 25 "../Main.m3"
 /* Var_Type1 */ TDD559A7B* a_L_15={0};//always-init
#line 25 "../Main.m3"
 /* Var_Type3 */ STRUCT(40) Main_m_40_L_41={0};//always-init
#line 25 "../Main.m3"
 /* Var_Type2 */ ADDRESS Main_m_42_L_43={0};//always-init
#line 25 "../Main.m3"
Main__F6_3v_Frame_t _frame;
#line 25 "../Main.m3"
_frame._unused=(ADDRESS)&_frame;
#line 25 "../Main.m3"
 /* set_source_line */
#line 25 "../Main.m3"
#line 26 "../Main.m3"
 /* load_address */
#line 26 "../Main.m3"
 /* store */
#line 26 "../Main.m3"
(*(ADDRESS*)(&Main_m_40_L_41))=(ADDRESS)(((ADDRESS)(INT64_(16)+((ADDRESS)(&Main_m_40_L_41)))));
#line 26 "../Main.m3"
 /* load_integer */
#line 26 "../Main.m3"
 /* store */
#line 26 "../Main.m3"
(*(INT64*)((8)+(char*)(&Main_m_40_L_41)))=(INT64)(  INT64_(3));
#line 26 "../Main.m3"
 /* load_integer */
#line 26 "../Main.m3"
 /* store */
#line 26 "../Main.m3"
(*(INT64*)((16)+(char*)(&Main_m_40_L_41)))=(INT64)(  INT64_(16));
#line 26 "../Main.m3"
 /* load_integer */
#line 26 "../Main.m3"
 /* store */
#line 26 "../Main.m3"
(*(INT64*)((24)+(char*)(&Main_m_40_L_41)))=(INT64)(  INT64_(4));
#line 26 "../Main.m3"
 /* load_integer */
#line 26 "../Main.m3"
 /* store */
#line 26 "../Main.m3"
(*(INT64*)((32)+(char*)(&Main_m_40_L_41)))=(INT64)(  INT64_(5));
#line 26 "../Main.m3"
 /* start_call_direct */
#line 26 "../Main.m3"
 /* load */
#line 26 "../Main.m3"
 /* pop_param */
#line 26 "../Main.m3"
 /* load_address */
#line 26 "../Main.m3"
 /* pop_param */
#line 26 "../Main.m3"
 /* call_direct */
#line 26 "../Main.m3"
 /* store */
#line 26 "../Main.m3"
(*(ADDRESS*)(&Main_m_42_L_43))=(ADDRESS)(((ADDRESS)(RTHooks__AllocateOpenArray(
  ( ADDRESS )(((ADDRESS)(*((ADDRESS*)(INT64_(520)+((ADDRESS)(&Main_m_M_Main_L_8)))))) ),
  ( TF400F3DB* /*TypeText1*/  )(((ADDRESS)(&Main_m_40_L_41)) )))));
#line 26 "../Main.m3"
 /* load */
#line 26 "../Main.m3"
 /* store */
#line 26 "../Main.m3"
(*(ADDRESS*)(&a_L_15))=(ADDRESS)(((ADDRESS)(Main_m_42_L_43)));
#line 26 "../Main.m3"
 /* set_source_line */
#line 26 "../Main.m3"
#line 28 "../Main.m3"
 /* exit_proc */
#line 28 "../Main.m3"
return;
#line 28 "../Main.m3"
 /* end_procedure */
#line 28 "../Main.m3"
} /* F6_4v */
#line 28 "../Main.m3"
 /* set_source_line */
#line 28 "../Main.m3"
#line 30 "../Main.m3"
 /* begin_procedure */
#line 30 "../Main.m3"
struct Main__F6_4v_Frame_t {
#line 30 "../Main.m3"
ADDRESS _unused;
#line 30 "../Main.m3"
};
#line 30 "../Main.m3"
void /*TypeText3*/ 
__cdecl
Main__F6_4v(void)
{
#line 30 "../Main.m3"
 /* Var_Type1 */ T7DC98F07* a_L_16={0};//always-init
#line 30 "../Main.m3"
 /* Var_Type3 */ STRUCT(48) Main_m_44_L_45={0};//always-init
#line 30 "../Main.m3"
 /* Var_Type2 */ ADDRESS Main_m_46_L_47={0};//always-init
#line 30 "../Main.m3"
Main__F6_4v_Frame_t _frame;
#line 30 "../Main.m3"
_frame._unused=(ADDRESS)&_frame;
#line 30 "../Main.m3"
 /* set_source_line */
#line 30 "../Main.m3"
#line 31 "../Main.m3"
 /* load_address */
#line 31 "../Main.m3"
 /* store */
#line 31 "../Main.m3"
(*(ADDRESS*)(&Main_m_44_L_45))=(ADDRESS)(((ADDRESS)(INT64_(16)+((ADDRESS)(&Main_m_44_L_45)))));
#line 31 "../Main.m3"
 /* load_integer */
#line 31 "../Main.m3"
 /* store */
#line 31 "../Main.m3"
(*(INT64*)((8)+(char*)(&Main_m_44_L_45)))=(INT64)(  INT64_(4));
#line 31 "../Main.m3"
 /* load_integer */
#line 31 "../Main.m3"
 /* store */
#line 31 "../Main.m3"
(*(INT64*)((16)+(char*)(&Main_m_44_L_45)))=(INT64)(  INT64_(17));
#line 31 "../Main.m3"
 /* load_integer */
#line 31 "../Main.m3"
 /* store */
#line 31 "../Main.m3"
(*(INT64*)((24)+(char*)(&Main_m_44_L_45)))=(INT64)(  INT64_(7));
#line 31 "../Main.m3"
 /* load_integer */
#line 31 "../Main.m3"
 /* store */
#line 31 "../Main.m3"
(*(INT64*)((32)+(char*)(&Main_m_44_L_45)))=(INT64)(  INT64_(5));
#line 31 "../Main.m3"
 /* load_integer */
#line 31 "../Main.m3"
 /* store */
#line 31 "../Main.m3"
(*(INT64*)((40)+(char*)(&Main_m_44_L_45)))=(INT64)(  INT64_(2));
#line 31 "../Main.m3"
 /* start_call_direct */
#line 31 "../Main.m3"
 /* load */
#line 31 "../Main.m3"
 /* pop_param */
#line 31 "../Main.m3"
 /* load_address */
#line 31 "../Main.m3"
 /* pop_param */
#line 31 "../Main.m3"
 /* call_direct */
#line 31 "../Main.m3"
 /* store */
#line 31 "../Main.m3"
(*(ADDRESS*)(&Main_m_46_L_47))=(ADDRESS)(((ADDRESS)(RTHooks__AllocateOpenArray(
  ( ADDRESS )(((ADDRESS)(*((ADDRESS*)(INT64_(536)+((ADDRESS)(&Main_m_M_Main_L_8)))))) ),
  ( TF400F3DB* /*TypeText1*/  )(((ADDRESS)(&Main_m_44_L_45)) )))));
#line 31 "../Main.m3"
 /* load */
#line 31 "../Main.m3"
 /* store */
#line 31 "../Main.m3"
(*(ADDRESS*)(&a_L_16))=(ADDRESS)(((ADDRESS)(Main_m_46_L_47)));
#line 31 "../Main.m3"
 /* set_source_line */
#line 31 "../Main.m3"
#line 33 "../Main.m3"
 /* exit_proc */
#line 33 "../Main.m3"
return;
#line 33 "../Main.m3"
 /* end_procedure */
#line 33 "../Main.m3"
} /* F6_4 */
#line 33 "../Main.m3"
 /* set_source_line */
#line 33 "../Main.m3"
#line 35 "../Main.m3"
 /* begin_procedure */
#line 35 "../Main.m3"
struct Main__F6_4_Frame_t {
#line 35 "../Main.m3"
ADDRESS _unused;
#line 35 "../Main.m3"
};
#line 35 "../Main.m3"
void /*TypeText3*/ 
__cdecl
Main__F6_4(void)
{
#line 35 "../Main.m3"
 /* Var_Type3 */ STRUCT(48) Main_m_48_L_49={0};//always-init
#line 35 "../Main.m3"
 /* Var_Type2 */ ADDRESS Main_m_50_L_51={0};//always-init
#line 35 "../Main.m3"
Main__F6_4_Frame_t _frame;
#line 35 "../Main.m3"
_frame._unused=(ADDRESS)&_frame;
#line 35 "../Main.m3"
 /* set_source_line */
#line 35 "../Main.m3"
#line 36 "../Main.m3"
 /* load_address */
#line 36 "../Main.m3"
 /* store */
#line 36 "../Main.m3"
(*(ADDRESS*)(&Main_m_48_L_49))=(ADDRESS)(((ADDRESS)(INT64_(16)+((ADDRESS)(&Main_m_48_L_49)))));
#line 36 "../Main.m3"
 /* load_integer */
#line 36 "../Main.m3"
 /* store */
#line 36 "../Main.m3"
(*(INT64*)((8)+(char*)(&Main_m_48_L_49)))=(INT64)(  INT64_(4));
#line 36 "../Main.m3"
 /* load_integer */
#line 36 "../Main.m3"
 /* store */
#line 36 "../Main.m3"
(*(INT64*)((16)+(char*)(&Main_m_48_L_49)))=(INT64)(  INT64_(11));
#line 36 "../Main.m3"
 /* load_integer */
#line 36 "../Main.m3"
 /* store */
#line 36 "../Main.m3"
(*(INT64*)((24)+(char*)(&Main_m_48_L_49)))=(INT64)(  INT64_(21));
#line 36 "../Main.m3"
 /* load_integer */
#line 36 "../Main.m3"
 /* store */
#line 36 "../Main.m3"
(*(INT64*)((32)+(char*)(&Main_m_48_L_49)))=(INT64)(  INT64_(31));
#line 36 "../Main.m3"
 /* load_integer */
#line 36 "../Main.m3"
 /* store */
#line 36 "../Main.m3"
(*(INT64*)((40)+(char*)(&Main_m_48_L_49)))=(INT64)(  INT64_(24));
#line 36 "../Main.m3"
 /* start_call_direct */
#line 36 "../Main.m3"
 /* load */
#line 36 "../Main.m3"
 /* pop_param */
#line 36 "../Main.m3"
 /* load_address */
#line 36 "../Main.m3"
 /* pop_param */
#line 36 "../Main.m3"
 /* call_direct */
#line 36 "../Main.m3"
 /* store */
#line 36 "../Main.m3"
(*(ADDRESS*)(&Main_m_50_L_51))=(ADDRESS)(((ADDRESS)(RTHooks__AllocateOpenArray(
  ( ADDRESS )(((ADDRESS)(*((ADDRESS*)(INT64_(536)+((ADDRESS)(&Main_m_M_Main_L_8)))))) ),
  ( TF400F3DB* /*TypeText1*/  )(((ADDRESS)(&Main_m_48_L_49)) )))));
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
} /* F7 */
#line 37 "../Main.m3"
 /* set_source_line */
#line 37 "../Main.m3"
#line 39 "../Main.m3"
 /* begin_procedure */
#line 39 "../Main.m3"
struct Main__F7_Frame_t {
#line 39 "../Main.m3"
ADDRESS _unused;
#line 39 "../Main.m3"
};
#line 39 "../Main.m3"
void /*TypeText3*/ 
__cdecl
Main__F7(void)
{
#line 39 "../Main.m3"
 /* Var_Type1 */ TF400F3DB* a_L_17={0};//always-init
#line 39 "../Main.m3"
 /* Var_Type3 */ STRUCT(24) Main_m_52_L_53={0};//always-init
#line 39 "../Main.m3"
 /* Var_Type2 */ ADDRESS Main_m_54_L_55={0};//always-init
#line 39 "../Main.m3"
 /* Var_Type3 */ STRUCT(16) Main_m_56_L_57={0};//always-init
#line 39 "../Main.m3"
 /* Var_Type2 */ INT64 Main_m_58_L_59={0};//always-init
#line 39 "../Main.m3"
 /* Var_Type3 */ STRUCT(16) Main_m_60_L_61={0};//always-init
#line 39 "../Main.m3"
 /* Var_Type2 */ ADDRESS Main_m_62_L_63={0};//always-init
#line 39 "../Main.m3"
 /* Var_Type2 */ INT64 Main_m_64_L_65={0};//always-init
#line 39 "../Main.m3"
Main__F7_Frame_t _frame;
#line 39 "../Main.m3"
_frame._unused=(ADDRESS)&_frame;
#line 39 "../Main.m3"
 /* set_source_line */
#line 39 "../Main.m3"
#line 40 "../Main.m3"
 /* load_address */
#line 40 "../Main.m3"
 /* store */
#line 40 "../Main.m3"
(*(ADDRESS*)(&Main_m_52_L_53))=(ADDRESS)(((ADDRESS)(INT64_(16)+((ADDRESS)(&Main_m_52_L_53)))));
#line 40 "../Main.m3"
 /* load_integer */
#line 40 "../Main.m3"
 /* store */
#line 40 "../Main.m3"
(*(INT64*)((8)+(char*)(&Main_m_52_L_53)))=(INT64)(  INT64_(1));
#line 40 "../Main.m3"
 /* load_integer */
#line 40 "../Main.m3"
 /* store */
#line 40 "../Main.m3"
(*(INT64*)((16)+(char*)(&Main_m_52_L_53)))=(INT64)(  INT64_(12));
#line 40 "../Main.m3"
 /* start_call_direct */
#line 40 "../Main.m3"
 /* load */
#line 40 "../Main.m3"
 /* pop_param */
#line 40 "../Main.m3"
 /* load_address */
#line 40 "../Main.m3"
 /* pop_param */
#line 40 "../Main.m3"
 /* call_direct */
#line 40 "../Main.m3"
 /* store */
#line 40 "../Main.m3"
(*(ADDRESS*)(&Main_m_54_L_55))=(ADDRESS)(((ADDRESS)(RTHooks__AllocateOpenArray(
  ( ADDRESS )(((ADDRESS)(*((ADDRESS*)(INT64_(488)+((ADDRESS)(&Main_m_M_Main_L_8)))))) ),
  ( TF400F3DB* /*TypeText1*/  )(((ADDRESS)(&Main_m_52_L_53)) )))));
#line 40 "../Main.m3"
 /* load */
#line 40 "../Main.m3"
 /* store */
#line 40 "../Main.m3"
(*(ADDRESS*)(&a_L_17))=(ADDRESS)(((ADDRESS)(Main_m_54_L_55)));
#line 40 "../Main.m3"
 /* set_source_line */
#line 40 "../Main.m3"
#line 42 "../Main.m3"
 /* load */
#line 42 "../Main.m3"
 /* store */
#line 42 "../Main.m3"
(*(ADDRESS*)(&Main_m_54_L_55))=(ADDRESS)(((ADDRESS)(a_L_17)));
#line 42 "../Main.m3"
 /* load_integer */
#line 42 "../Main.m3"
 /* store */
#line 42 "../Main.m3"
(*(INT64*)((8)+(char*)(&Main_m_56_L_57)))=(INT64)(  INT64_(4));
#line 42 "../Main.m3"
 /* load */
#line 42 "../Main.m3"
 /* load_indirect */
#line 42 "../Main.m3"
 /* load_integer */
#line 42 "../Main.m3"
 /* swap */
#line 42 "../Main.m3"
 /* subtract */
#line 42 "../Main.m3"
 /* check_hi */
#line 42 "../Main.m3"
 /* store */
#line 42 "../Main.m3"
(*(INT64*)(&Main_m_58_L_59))=(INT64)( ((INT64)(  INT64_(4)- *((INT64*)(INT64_(8)+((ADDRESS)(Main_m_54_L_55)))))));
#line 42 "../Main.m3"
 /* load */
#line 42 "../Main.m3"
/*check_hi*/if(INT64_(0)<Main_m_58_L_59)Main_m_M_Main_L_8_CRASH(1345);
#line 42 "../Main.m3"
 /* pop */
#line 42 "../Main.m3"
m3_pop_INT64( Main_m_58_L_59);
#line 42 "../Main.m3"
 /* load */
#line 42 "../Main.m3"
 /* load_indirect */
#line 42 "../Main.m3"
 /* store */
#line 42 "../Main.m3"
(*(ADDRESS*)(&Main_m_56_L_57))=(ADDRESS)(((ADDRESS)(*((ADDRESS*)(Main_m_54_L_55)))));
#line 42 "../Main.m3"
 /* load */
#line 42 "../Main.m3"
 /* store */
#line 42 "../Main.m3"
(*(ADDRESS*)(&Main_m_62_L_63))=(ADDRESS)(((ADDRESS)(a_L_17)));
#line 42 "../Main.m3"
 /* load_integer */
#line 42 "../Main.m3"
 /* store */
#line 42 "../Main.m3"
(*(INT64*)((8)+(char*)(&Main_m_60_L_61)))=(INT64)(  INT64_(4));
#line 42 "../Main.m3"
 /* load */
#line 42 "../Main.m3"
 /* load_indirect */
#line 42 "../Main.m3"
 /* load_integer */
#line 42 "../Main.m3"
 /* swap */
#line 42 "../Main.m3"
 /* subtract */
#line 42 "../Main.m3"
 /* check_hi */
#line 42 "../Main.m3"
 /* store */
#line 42 "../Main.m3"
(*(INT64*)(&Main_m_64_L_65))=(INT64)( ((INT64)(  INT64_(5)- *((INT64*)(INT64_(8)+((ADDRESS)(Main_m_62_L_63)))))));
#line 42 "../Main.m3"
 /* load */
#line 42 "../Main.m3"
/*check_hi*/if(INT64_(0)<Main_m_64_L_65)Main_m_M_Main_L_8_CRASH(1345);
#line 42 "../Main.m3"
 /* pop */
#line 42 "../Main.m3"
m3_pop_INT64( Main_m_64_L_65);
#line 42 "../Main.m3"
 /* load */
#line 42 "../Main.m3"
 /* load_indirect */
#line 42 "../Main.m3"
 /* add_offset */
#line 42 "../Main.m3"
 /* store */
#line 42 "../Main.m3"
(*(ADDRESS*)(&Main_m_60_L_61))=(ADDRESS)(((ADDRESS)(((8)+(char*)(((ADDRESS)(*((ADDRESS*)(Main_m_62_L_63)))))))));
#line 42 "../Main.m3"
 /* load */
#line 42 "../Main.m3"
 /* load */
#line 42 "../Main.m3"
 /* load */
#line 42 "../Main.m3"
 /* copy_n */
#line 42 "../Main.m3"
m3_memmove(
 *((ADDRESS*)(&Main_m_56_L_57)),
 *((ADDRESS*)(&Main_m_60_L_61)),
 8*(size_t)((INT64)(*((INT64*)(INT64_(8)+((ADDRESS)(&Main_m_56_L_57)))))));
#line 42 "../Main.m3"
 /* set_source_line */
#line 42 "../Main.m3"
#line 43 "../Main.m3"
 /* exit_proc */
#line 43 "../Main.m3"
return;
#line 43 "../Main.m3"
 /* end_procedure */
#line 43 "../Main.m3"
} /* F1 */
#line 43 "../Main.m3"
 /* set_source_line */
#line 43 "../Main.m3"
#line 45 "../Main.m3"
 /* begin_procedure */
#line 45 "../Main.m3"
struct Main__F1_Frame_t {
#line 45 "../Main.m3"
ADDRESS _unused;
#line 45 "../Main.m3"
};
#line 45 "../Main.m3"
TF400F3DB* /*TypeText1*/ 
__cdecl
Main__F1(void)
{
#line 45 "../Main.m3"
 /* Var_Type1 */ TF400F3DB* a_L_18={0};//always-init
#line 45 "../Main.m3"
 /* Var_Type3 */ STRUCT(24) Main_m_66_L_67={0};//always-init
#line 45 "../Main.m3"
 /* Var_Type2 */ ADDRESS Main_m_68_L_69={0};//always-init
#line 45 "../Main.m3"
 /* Var_Type3 */ STRUCT(16) Main_m_70_L_71={0};//always-init
#line 45 "../Main.m3"
 /* Var_Type2 */ INT64 Main_m_72_L_73={0};//always-init
#line 45 "../Main.m3"
 /* Var_Type3 */ STRUCT(16) Main_m_74_L_75={0};//always-init
#line 45 "../Main.m3"
 /* Var_Type2 */ ADDRESS Main_m_76_L_77={0};//always-init
#line 45 "../Main.m3"
 /* Var_Type2 */ INT64 Main_m_78_L_79={0};//always-init
#line 45 "../Main.m3"
Main__F1_Frame_t _frame;
#line 45 "../Main.m3"
_frame._unused=(ADDRESS)&_frame;
#line 45 "../Main.m3"
 /* set_source_line */
#line 45 "../Main.m3"
#line 46 "../Main.m3"
 /* load_address */
#line 46 "../Main.m3"
 /* store */
#line 46 "../Main.m3"
(*(ADDRESS*)(&Main_m_66_L_67))=(ADDRESS)(((ADDRESS)(INT64_(16)+((ADDRESS)(&Main_m_66_L_67)))));
#line 46 "../Main.m3"
 /* load_integer */
#line 46 "../Main.m3"
 /* store */
#line 46 "../Main.m3"
(*(INT64*)((8)+(char*)(&Main_m_66_L_67)))=(INT64)(  INT64_(1));
#line 46 "../Main.m3"
 /* load_integer */
#line 46 "../Main.m3"
 /* store */
#line 46 "../Main.m3"
(*(INT64*)((16)+(char*)(&Main_m_66_L_67)))=(INT64)(  INT64_(10));
#line 46 "../Main.m3"
 /* start_call_direct */
#line 46 "../Main.m3"
 /* load */
#line 46 "../Main.m3"
 /* pop_param */
#line 46 "../Main.m3"
 /* load_address */
#line 46 "../Main.m3"
 /* pop_param */
#line 46 "../Main.m3"
 /* call_direct */
#line 46 "../Main.m3"
 /* store */
#line 46 "../Main.m3"
(*(ADDRESS*)(&Main_m_68_L_69))=(ADDRESS)(((ADDRESS)(RTHooks__AllocateOpenArray(
  ( ADDRESS )(((ADDRESS)(*((ADDRESS*)(INT64_(488)+((ADDRESS)(&Main_m_M_Main_L_8)))))) ),
  ( TF400F3DB* /*TypeText1*/  )(((ADDRESS)(&Main_m_66_L_67)) )))));
#line 46 "../Main.m3"
 /* load */
#line 46 "../Main.m3"
 /* store */
#line 46 "../Main.m3"
(*(ADDRESS*)(&a_L_18))=(ADDRESS)(((ADDRESS)(Main_m_68_L_69)));
#line 46 "../Main.m3"
 /* set_source_line */
#line 46 "../Main.m3"
#line 48 "../Main.m3"
 /* start_call_direct */
#line 48 "../Main.m3"
 /* load */
#line 48 "../Main.m3"
 /* pop_param */
#line 48 "../Main.m3"
 /* call_direct */
#line 48 "../Main.m3"
Main__F2(
  ( TF400F3DB* /*TypeText1*/  )(((ADDRESS)(a_L_18)) ));
#line 48 "../Main.m3"
 /* set_source_line */
#line 48 "../Main.m3"
#line 49 "../Main.m3"
 /* start_call_direct */
#line 49 "../Main.m3"
 /* load */
#line 49 "../Main.m3"
 /* pop_param */
#line 49 "../Main.m3"
 /* call_direct */
#line 49 "../Main.m3"
Main__F3(
  ( TF400F3DB* /*TypeText1*/  )(((ADDRESS)(a_L_18)) ));
#line 49 "../Main.m3"
 /* set_source_line */
#line 49 "../Main.m3"
#line 50 "../Main.m3"
 /* load */
#line 50 "../Main.m3"
 /* store */
#line 50 "../Main.m3"
(*(ADDRESS*)(&Main_m_68_L_69))=(ADDRESS)(((ADDRESS)(a_L_18)));
#line 50 "../Main.m3"
 /* load_integer */
#line 50 "../Main.m3"
 /* store */
#line 50 "../Main.m3"
(*(INT64*)((8)+(char*)(&Main_m_70_L_71)))=(INT64)(  INT64_(4));
#line 50 "../Main.m3"
 /* load */
#line 50 "../Main.m3"
 /* load_indirect */
#line 50 "../Main.m3"
 /* load_integer */
#line 50 "../Main.m3"
 /* swap */
#line 50 "../Main.m3"
 /* subtract */
#line 50 "../Main.m3"
 /* check_hi */
#line 50 "../Main.m3"
 /* store */
#line 50 "../Main.m3"
(*(INT64*)(&Main_m_72_L_73))=(INT64)( ((INT64)(  INT64_(4)- *((INT64*)(INT64_(8)+((ADDRESS)(Main_m_68_L_69)))))));
#line 50 "../Main.m3"
 /* load */
#line 50 "../Main.m3"
/*check_hi*/if(INT64_(0)<Main_m_72_L_73)Main_m_M_Main_L_8_CRASH(1601);
#line 50 "../Main.m3"
 /* pop */
#line 50 "../Main.m3"
m3_pop_INT64( Main_m_72_L_73);
#line 50 "../Main.m3"
 /* load */
#line 50 "../Main.m3"
 /* load_indirect */
#line 50 "../Main.m3"
 /* store */
#line 50 "../Main.m3"
(*(ADDRESS*)(&Main_m_70_L_71))=(ADDRESS)(((ADDRESS)(*((ADDRESS*)(Main_m_68_L_69)))));
#line 50 "../Main.m3"
 /* load */
#line 50 "../Main.m3"
 /* store */
#line 50 "../Main.m3"
(*(ADDRESS*)(&Main_m_76_L_77))=(ADDRESS)(((ADDRESS)(a_L_18)));
#line 50 "../Main.m3"
 /* load_integer */
#line 50 "../Main.m3"
 /* store */
#line 50 "../Main.m3"
(*(INT64*)((8)+(char*)(&Main_m_74_L_75)))=(INT64)(  INT64_(4));
#line 50 "../Main.m3"
 /* load */
#line 50 "../Main.m3"
 /* load_indirect */
#line 50 "../Main.m3"
 /* load_integer */
#line 50 "../Main.m3"
 /* swap */
#line 50 "../Main.m3"
 /* subtract */
#line 50 "../Main.m3"
 /* check_hi */
#line 50 "../Main.m3"
 /* store */
#line 50 "../Main.m3"
(*(INT64*)(&Main_m_78_L_79))=(INT64)( ((INT64)(  INT64_(4)- *((INT64*)(INT64_(8)+((ADDRESS)(Main_m_76_L_77)))))));
#line 50 "../Main.m3"
 /* load */
#line 50 "../Main.m3"
/*check_hi*/if(INT64_(0)<Main_m_78_L_79)Main_m_M_Main_L_8_CRASH(1601);
#line 50 "../Main.m3"
 /* pop */
#line 50 "../Main.m3"
m3_pop_INT64( Main_m_78_L_79);
#line 50 "../Main.m3"
 /* load */
#line 50 "../Main.m3"
 /* load_indirect */
#line 50 "../Main.m3"
 /* store */
#line 50 "../Main.m3"
(*(ADDRESS*)(&Main_m_74_L_75))=(ADDRESS)(((ADDRESS)(*((ADDRESS*)(Main_m_76_L_77)))));
#line 50 "../Main.m3"
 /* load */
#line 50 "../Main.m3"
 /* load */
#line 50 "../Main.m3"
 /* load */
#line 50 "../Main.m3"
 /* copy_n */
#line 50 "../Main.m3"
m3_memmove(
 *((ADDRESS*)(&Main_m_70_L_71)),
 *((ADDRESS*)(&Main_m_74_L_75)),
 8*(size_t)((INT64)(*((INT64*)(INT64_(8)+((ADDRESS)(&Main_m_70_L_71)))))));
#line 50 "../Main.m3"
 /* set_source_line */
#line 50 "../Main.m3"
#line 51 "../Main.m3"
 /* start_call_direct */
#line 51 "../Main.m3"
 /* load */
#line 51 "../Main.m3"
 /* pop_param */
#line 51 "../Main.m3"
 /* call_direct */
#line 51 "../Main.m3"
 /* store */
#line 51 "../Main.m3"
(*(ADDRESS*)(&Main_m_76_L_77))=(ADDRESS)(((ADDRESS)(Main__F4(
  ( TF400F3DB* /*TypeText1*/  )(((ADDRESS)(a_L_18)) )))));
#line 51 "../Main.m3"
 /* load */
#line 51 "../Main.m3"
 /* exit_proc */
#line 51 "../Main.m3"
return (TF400F3DB* /*TypeText1*/ )(Main_m_76_L_77);
#line 51 "../Main.m3"
 /* end_procedure */
#line 51 "../Main.m3"
} /* Main_M3 */
#line 51 "../Main.m3"
 /* module main body Main_M3 */
#line 51 "../Main.m3"
 /* set_source_line */
#line 51 "../Main.m3"
#line 54 "../Main.m3"
 /* begin_procedure */
#line 54 "../Main.m3"
struct Main_M3_Frame_t {
#line 54 "../Main.m3"
ADDRESS _unused;
#line 54 "../Main.m3"
};
#line 54 "../Main.m3"
RT0__ModulePtr
__cdecl
Main_M3(
   /* Param_Type1 */ INTEGER /*TypeText1*/  mode_L_9)
{
#line 54 "../Main.m3"
 /* Var_Type2 */ ADDRESS Main_m_80_L_81={0};//always-init
#line 54 "../Main.m3"
 /* Var_Type2 */ ADDRESS Main_m_82_L_83={0};//always-init
#line 54 "../Main.m3"
Main_M3_Frame_t _frame;
#line 54 "../Main.m3"
_frame._unused=(ADDRESS)&_frame;
#line 54 "../Main.m3"
 /* load */
#line 54 "../Main.m3"
 /* if_true_or_false */
#line 54 "../Main.m3"
 /* load_host_integer */
#line 54 "../Main.m3"
 /* load_integer */
#line 54 "../Main.m3"
 /* if_compare */
#line 54 "../Main.m3"
if(m3_eq(INT64,
  mode_L_9,
   INT64_(0)))goto L1;
#line 54 "../Main.m3"
 /* set_source_line */
#line 54 "../Main.m3"
#line 55 "../Main.m3"
 /* start_call_direct */
#line 55 "../Main.m3"
 /* call_direct */
#line 55 "../Main.m3"
Main__F5(
 );
#line 55 "../Main.m3"
 /* set_source_line */
#line 55 "../Main.m3"
#line 56 "../Main.m3"
 /* start_call_direct */
#line 56 "../Main.m3"
 /* call_direct */
#line 56 "../Main.m3"
Main__F6(
 );
#line 56 "../Main.m3"
 /* set_source_line */
#line 56 "../Main.m3"
#line 57 "../Main.m3"
 /* start_call_direct */
#line 57 "../Main.m3"
 /* call_direct */
#line 57 "../Main.m3"
Main__F6v(
 );
#line 57 "../Main.m3"
 /* set_source_line */
#line 57 "../Main.m3"
#line 58 "../Main.m3"
 /* start_call_direct */
#line 58 "../Main.m3"
 /* call_direct */
#line 58 "../Main.m3"
Main__F6_3(
 );
#line 58 "../Main.m3"
 /* set_source_line */
#line 58 "../Main.m3"
#line 59 "../Main.m3"
 /* start_call_direct */
#line 59 "../Main.m3"
 /* call_direct */
#line 59 "../Main.m3"
Main__F6_3v(
 );
#line 59 "../Main.m3"
 /* set_source_line */
#line 59 "../Main.m3"
#line 60 "../Main.m3"
 /* start_call_direct */
#line 60 "../Main.m3"
 /* call_direct */
#line 60 "../Main.m3"
Main__F6_4(
 );
#line 60 "../Main.m3"
 /* set_source_line */
#line 60 "../Main.m3"
#line 61 "../Main.m3"
 /* start_call_direct */
#line 61 "../Main.m3"
 /* call_direct */
#line 61 "../Main.m3"
Main__F6_4v(
 );
#line 61 "../Main.m3"
 /* set_source_line */
#line 61 "../Main.m3"
#line 62 "../Main.m3"
 /* start_call_direct */
#line 62 "../Main.m3"
 /* call_direct */
#line 62 "../Main.m3"
Main__F7(
 );
#line 62 "../Main.m3"
 /* set_source_line */
#line 62 "../Main.m3"
#line 63 "../Main.m3"
 /* start_call_direct */
#line 63 "../Main.m3"
 /* call_direct */
#line 63 "../Main.m3"
 /* store */
#line 63 "../Main.m3"
(*(ADDRESS*)(&Main_m_80_L_81))=(ADDRESS)(((ADDRESS)(Main__F1(
 ))));
#line 63 "../Main.m3"
 /* start_call_direct */
#line 63 "../Main.m3"
 /* load */
#line 63 "../Main.m3"
 /* pop_param */
#line 63 "../Main.m3"
 /* call_direct */
#line 63 "../Main.m3"
 /* store */
#line 63 "../Main.m3"
(*(ADDRESS*)(&Main_m_82_L_83))=(ADDRESS)(((ADDRESS)(Main__F4(
  ( TF400F3DB* /*TypeText1*/  )(((ADDRESS)(Main_m_80_L_81)) )))));
#line 63 "../Main.m3"
 /* set_label */
#line 63 "../Main.m3"
L1:;
#line 63 "../Main.m3"
 /* load_address */
#line 63 "../Main.m3"
 /* exit_proc */
#line 63 "../Main.m3"
return (RT0__ModulePtr)(&Main_m_M_Main_L_8);
#line 63 "../Main.m3"
 /* end_procedure */
#line 63 "../Main.m3"
} /* global constant type descriptor */
#line 63 "../Main.m3"
 /* global data type descriptor */
#line 63 "../Main.m3"
 /* module global constants */
#line 63 "../Main.m3"
 /* procedure names */
#line 63 "../Main.m3"
 /* procedure table */
#line 63 "../Main.m3"
 /* file name */
#line 63 "../Main.m3"
 /* type map for _tc7f3037e */
#line 63 "../Main.m3"
 /* type description for _tc7f3037e */
#line 63 "../Main.m3"
 /* type map for _td90c01e6 */
#line 63 "../Main.m3"
 /* type description for _td90c01e6 */
#line 63 "../Main.m3"
 /* type map for _t7990149a */
#line 63 "../Main.m3"
 /* type description for _t7990149a */
#line 63 "../Main.m3"
 /* module global data */
#line 63 "../Main.m3"
 /* typecell for _t7990149a */
#line 63 "../Main.m3"
 /* typecell for _td90c01e6 */
#line 63 "../Main.m3"
 /* typecell for _tc7f3037e */
#line 63 "../Main.m3"
 /* load map


 global data allocation for M_Main
     0   104  8  *module info*
   104   112  8  typecell
   216   112  8  typecell
   328   112  8  typecell
   440    24  8  import Main
   464    24  8  import RTHooks
   488    16  8  typecell ptr
   504    16  8  typecell ptr
   520    16  8  typecell ptr
   536    16  8  typecell ptr
   552     0  8  *TOTAL*


 global constants for M_Main
     0    55  8  *proc names*
    56   216  8  *proc info*
   272    11  1  *string*
   283     5  1  type_map
   288     4  1  type_desc
   292     5  1  type_map
   297     4  1  type_desc
   301     5  1  type_map
   306     4  1  type_desc
   312     0  8  *TOTAL*
 */
#line 63 "../Main.m3"
 /* end unit */
#line 63 "../Main.m3"

#ifdef __cplusplus

} /* extern "C" */
#endif
 /* set_runtime_proc */
 /* set_runtime_proc */
 /* set_runtime_proc */
