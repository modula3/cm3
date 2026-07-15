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
 /* declare_proctype */

#if 0 /* avoid type hash collions */
typedef 
void(__cdecl*T7B78C34F)(void);
#else
typedef void (__cdecl*T7B78C34F)(void);
#endif
 /* declare_opaque */

#ifndef TF81917DF
#define TF81917DF TF81917DF
/*1addressType_define*/typedef ADDRESS TF81917DF;

#endif
 /* declare_proctype */
 /* declare_formal */
 /* declare_record */
 /* declare_field */
 /* record_forwardDeclare Record_t{ typeid:TE99B66B4 text:NIL hash_text:TE99B66B4 base_text:NIL state:0} */
/*record_forwardDeclare*/struct TE99B66B4;typedef struct TE99B66B4 TE99B66B4;
 /* record_canBeDefined Record_t{ typeid:TE99B66B4 text:NIL hash_text:TE99B66B4 base_text:NIL state:0} */
 /* record_define Record_t{ typeid:TE99B66B4 text:NIL hash_text:TE99B66B4 base_text:NIL state:0} */

#ifndef TE99B66B4
#define TE99B66B4 TE99B66B4
/*record_define*/struct TE99B66B4{
INTEGER Point_T_field;
};
#endif
 /* declare_record */
 /* declare_field */
 /* record_forwardDeclare Record_t{ typeid:TFCD63CAE text:NIL hash_text:TFCD63CAE base_text:NIL state:0} */
/*record_forwardDeclare*/struct TFCD63CAE;typedef struct TFCD63CAE TFCD63CAE;
 /* record_canBeDefined Record_t{ typeid:TFCD63CAE text:NIL hash_text:TFCD63CAE base_text:NIL state:0} */
 /* record_define Record_t{ typeid:TFCD63CAE text:NIL hash_text:TFCD63CAE base_text:NIL state:0} */

#ifndef TFCD63CAE
#define TFCD63CAE TFCD63CAE
/*record_define*/struct TFCD63CAE{
INTEGER PaintOp_T_field;
};
#endif
 /* declare_record */
 /* declare_field */
 /* record_forwardDeclare Record_t{ typeid:T5CCFFB05 text:NIL hash_text:T5CCFFB05 base_text:NIL state:0} */
/*record_forwardDeclare*/struct T5CCFFB05;typedef struct T5CCFFB05 T5CCFFB05;
 /* record_canBeDefined Record_t{ typeid:T5CCFFB05 text:NIL hash_text:T5CCFFB05 base_text:NIL state:0} */
 /* record_define Record_t{ typeid:T5CCFFB05 text:NIL hash_text:T5CCFFB05 base_text:NIL state:0} */

#ifndef T5CCFFB05
#define T5CCFFB05 T5CCFFB05
/*record_define*/struct T5CCFFB05{
INTEGER Pixmap_T_field;
};
#endif
 /* declare_subrange */
/*subrange_define*/typedef UINT8 T8A2831D7_8;
 /* declare_array */
/*array_forwardDeclare*/struct T4F238AAE;typedef struct T4F238AAE T4F238AAE;

#ifndef T4F238AAE
#define T4F238AAE T4F238AAE
/*fixedArray_define*/struct T4F238AAE{INTEGER _elts[1];};
#endif
 /* declare_subrange */
/*subrange_define*/typedef UINT16 T4A3373B8_16;
 /* declare_set */
 /* declare_array */
/*array_forwardDeclare*/struct T67A7B112;typedef struct T67A7B112 T67A7B112;

#ifndef T67A7B112
#define T67A7B112 T67A7B112
/*fixedArray_define*/struct T67A7B112{WORD_T _elts[17];};
#endif
 /* declare_subrange */
/*subrange_define*/typedef UINT8 T2CA4581D_8;
 /* declare_set */

#ifndef TDDB62BB7
#define TDDB62BB7 TDDB62BB7
/*type_typedef*/typedef UINT16 TDDB62BB7;

#endif
 /* declare_proctype */
 /* declare_formal */
 /* declare_formal */
 /* declare_record */
 /* declare_record */
 /* declare_field */
 /* DeclareTypes_FlushOnce size:2 */

#if 0 /* avoid type hash collions */
typedef 
ROOT(__cdecl*T58C1D1D1)(ADDRESS);
#else
typedef void (__cdecl*T58C1D1D1)(void);
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

#ifndef struct_8_t
#define struct_8_t struct_8_t
STRUCT8(8)
#endif

#ifndef struct_16_t
#define struct_16_t struct_16_t
STRUCT8(16)
#endif

#ifndef struct_136_t
#define struct_136_t struct_136_t
STRUCT8(136)
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
/*Proc_ForwardDeclareFrameType*/struct HighlightVBT_I3_Frame_t;typedef struct HighlightVBT_I3_Frame_t HighlightVBT_I3_Frame_t;
 /* internal_declare_param */
RT0__ModulePtr
__cdecl
HighlightVBT_I3(
   /* Param_Type1 */ INTEGER /*TypeText1*/  mode_L_1);
 /* import_procedure */
/*Proc_ForwardDeclareFrameType*/struct RTHooks_I3_Frame_t;typedef struct RTHooks_I3_Frame_t RTHooks_I3_Frame_t;
 /* internal_declare_param */
RT0__ModulePtr
__cdecl
RTHooks_I3(
   /* Param_Type1 */ INTEGER /*TypeText1*/  mode_L_2);
 /* import_procedure */
/*Proc_ForwardDeclareFrameType*/struct RTHooks__AllocateTracedObj_Frame_t;typedef struct RTHooks__AllocateTracedObj_Frame_t RTHooks__AllocateTracedObj_Frame_t;
 /* internal_declare_param */
ROOT
__cdecl
RTHooks__AllocateTracedObj(
   /* Param_Type1 */ ADDRESS t_L_3);
 /* import_procedure */
/*Proc_ForwardDeclareFrameType*/struct VBT_I3_Frame_t;typedef struct VBT_I3_Frame_t VBT_I3_Frame_t;
 /* internal_declare_param */
RT0__ModulePtr
__cdecl
VBT_I3(
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
 /* end: imports */
 /* begin: locals */
 /* declare_segment name:<NIL> typeid:TFFFFFFFF const:TRUE */
/*declare_segment*/struct Main_m_7_L_8_t;
/*declare_segment*/typedef struct Main_m_7_L_8_t Main_m_7_L_8_t;
 /* declare_segment name:M_Main typeid:TFFFFFFFF const:FALSE */
 /* handler_name_prefixes:Main_M3_LINE_ */
 /* handler_name_prefixes:Main_I3_LINE_ */
/*declare_segment*/struct Main_m_M_Main_L_9_t;
/*declare_segment*/typedef struct Main_m_M_Main_L_9_t Main_m_M_Main_L_9_t;
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct Main_M3_Frame_t;typedef struct Main_M3_Frame_t Main_M3_Frame_t;
 /* internal_declare_param */
RT0__ModulePtr
__cdecl
Main_M3(
   /* Param_Type1 */ INTEGER /*TypeText1*/  mode_L_10);
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct Main__Main_Frame_t;typedef struct Main__Main_Frame_t Main__Main_Frame_t;
void /*TypeText3*/ 
__cdecl
Main__Main(void);
 /* Locals_begin_procedure */
 /* begin_block */
 /* declare_temp */
 /* declare_local */
 /* declare_temp */
 /* declare_local */
 /* declare_temp */
 /* declare_local */
 /* declare_temp */
 /* declare_local */
 /* declare_temp */
 /* declare_local */
 /* declare_temp */
 /* declare_local */
 /* declare_temp */
 /* declare_local */
 /* declare_temp */
 /* declare_local */
 /* declare_temp */
 /* declare_local */
 /* declare_temp */
 /* declare_local */
 /* AllocateTemps_check_nil */
 /* AllocateTemps_common */
 /* declare_temp */
 /* declare_local */
 /* AllocateTemps_check_nil */
 /* AllocateTemps_common */
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
 /* init_int */
 /* init_int */
 /* init_chars */
 /* init_chars */
 /* init_proc */
 /* init_var */
 /* init_proc */
 /* init_var */
 /* init_chars */
 /* end_init */
struct Main_m_7_L_8_t{char L_37[8];
INT64 L_38[1];
char L_39[8];
INT64 L_40[1];
UINT8 L_41[7];
char L_42[1];
UINT8 L_43[4];
char L_44[4];
ADDRESS L_45[4];
char L_46[8];
UINT8 L_47[10];
char L_48[14];
};
static  const Main_m_7_L_8_t Main_m_7_L_8={{0 /* 1 */ ,0 /* 2 */ ,0 /* 3 */ ,0 /* 4 */ ,0 /* 5 */ ,0 /* 6 */ ,0 /* 7 */ ,0 /* 8 */ ,},{INT64_(1)},{0 /* 1 */ ,0 /* 2 */ ,0 /* 3 */ ,0 /* 4 */ ,0 /* 5 */ ,0 /* 6 */ ,0 /* 7 */ ,0 /* 8 */ ,},{INT64_(1)},{'M','a','i','n','_','M','3'},{0 /* 1 */ ,},{'M','a','i','n'},{0 /* 1 */ ,0 /* 2 */ ,0 /* 3 */ ,0 /* 4 */ ,},{(ADDRESS)&Main_M3,32+(char*)&Main_m_7_L_8,(ADDRESS)&Main__Main,40+(char*)&Main_m_7_L_8},{0 /* 1 */ ,0 /* 2 */ ,0 /* 3 */ ,0 /* 4 */ ,0 /* 5 */ ,0 /* 6 */ ,0 /* 7 */ ,0 /* 8 */ ,},{'.','.','/','M','a','i','n','.','m','3'},{0 /* 1 */ ,0 /* 2 */ ,0 /* 3 */ ,0 /* 4 */ ,0 /* 5 */ ,0 /* 6 */ ,0 /* 7 */ ,0 /* 8 */ ,0 /* 9 */ ,0 /* 10 */ ,0 /* 11 */ ,0 /* 12 */ ,0 /* 13 */ ,0 /* 14 */ ,}};
 /* bind_segment */
 /* begin_init */
 /* init_var */
 /* init_var */
 /* init_var */
 /* init_var */
 /* init_proc */
 /* init_int */
 /* init_int */
 /* init_proc */
 /* init_var */
 /* init_proc */
 /* init_var */
 /* init_proc */
 /* init_var */
 /* init_int */
 /* init_proc */
 /* end_init */
struct Main_m_M_Main_L_9_t{ADDRESS L_49[1];
char L_50[8];
ADDRESS L_51[1];
char L_52[16];
ADDRESS L_53[1];
char L_54[24];
ADDRESS L_55[1];
char L_56[8];
ADDRESS L_57[1];
INT64 L_58[2];
char L_59[8];
ADDRESS L_60[2];
char L_61[8];
ADDRESS L_62[2];
char L_63[8];
ADDRESS L_64[2];
char L_65[8];
INT64 L_66[1];
char L_67[8];
ADDRESS L_68[1];
char L_69[8];
};
static Main_m_M_Main_L_9_t Main_m_M_Main_L_9={{88+(char*)&Main_m_7_L_8},{0 /* 1 */ ,0 /* 2 */ ,0 /* 3 */ ,0 /* 4 */ ,0 /* 5 */ ,0 /* 6 */ ,0 /* 7 */ ,0 /* 8 */ ,},{184+(char*)&Main_m_M_Main_L_9},{0 /* 1 */ ,0 /* 2 */ ,0 /* 3 */ ,0 /* 4 */ ,0 /* 5 */ ,0 /* 6 */ ,0 /* 7 */ ,0 /* 8 */ ,0 /* 9 */ ,0 /* 10 */ ,0 /* 11 */ ,0 /* 12 */ ,0 /* 13 */ ,0 /* 14 */ ,0 /* 15 */ ,0 /* 16 */ ,},{48+(char*)&Main_m_7_L_8},{0 /* 1 */ ,0 /* 2 */ ,0 /* 3 */ ,0 /* 4 */ ,0 /* 5 */ ,0 /* 6 */ ,0 /* 7 */ ,0 /* 8 */ ,0 /* 9 */ ,0 /* 10 */ ,0 /* 11 */ ,0 /* 12 */ ,0 /* 13 */ ,0 /* 14 */ ,0 /* 15 */ ,0 /* 16 */ ,0 /* 17 */ ,0 /* 18 */ ,0 /* 19 */ ,0 /* 20 */ ,0 /* 21 */ ,0 /* 22 */ ,0 /* 23 */ ,0 /* 24 */ ,},{112+(char*)&Main_m_M_Main_L_9},{0 /* 1 */ ,0 /* 2 */ ,0 /* 3 */ ,0 /* 4 */ ,0 /* 5 */ ,0 /* 6 */ ,0 /* 7 */ ,0 /* 8 */ ,},{(ADDRESS)&Main_M3},{INT64_(3),INT64_(1)},{0 /* 1 */ ,0 /* 2 */ ,0 /* 3 */ ,0 /* 4 */ ,0 /* 5 */ ,0 /* 6 */ ,0 /* 7 */ ,0 /* 8 */ ,},{(ADDRESS)&Main_I3,136+(char*)&Main_m_M_Main_L_9},{0 /* 1 */ 
,0 /* 2 */ ,0 /* 3 */ ,0 /* 4 */ ,0 /* 5 */ ,0 /* 6 */ ,0 /* 7 */ ,0 /* 8 */ ,},{(ADDRESS)&HighlightVBT_I3,160+(char*)&Main_m_M_Main_L_9},{0 /* 1 */ ,0 /* 2 */ ,0 /* 3 */ ,0 /* 4 */ ,0 /* 5 */ ,0 /* 6 */ ,0 /* 7 */ ,0 /* 8 */ ,},{(ADDRESS)&RTHooks_I3,200+(char*)&Main_m_M_Main_L_9},{0 /* 1 */ ,0 /* 2 */ ,0 /* 3 */ ,0 /* 4 */ ,0 /* 5 */ ,0 /* 6 */ ,0 /* 7 */ ,0 /* 8 */ ,},{INT64_(-132573217)},{0 /* 1 */ ,0 /* 2 */ ,0 /* 3 */ ,0 /* 4 */ ,0 /* 5 */ ,0 /* 6 */ ,0 /* 7 */ ,0 /* 8 */ ,},{(ADDRESS)&VBT_I3},{0 /* 1 */ ,0 /* 2 */ ,0 /* 3 */ ,0 /* 4 */ ,0 /* 5 */ ,0 /* 6 */ ,0 /* 7 */ ,0 /* 8 */ ,}};
static void __cdecl Main_m_M_Main_L_9_CRASH(WORD_T code) M3_ATTRIBUTE_NO_RETURN;
static void __cdecl Main_m_M_Main_L_9_CRASH(WORD_T code){RTHooks__ReportFault((ADDRESS)&Main_m_M_Main_L_9,code);} /* end: segments/globals */
 /* begin: mark used */
 /* end: mark used */
 /* set_source_file */
 /* set_source_line */
#line 6 "../Main.m3"
 /* module global constants */
#line 6 "../Main.m3"
 /* module global data */
#line 6 "../Main.m3"
 /* set_source_line */
#line 6 "../Main.m3"
#line 17 "../Main.m3"
 /* Main */
#line 17 "../Main.m3"
 /* set_source_line */
#line 17 "../Main.m3"
#line 11 "../Main.m3"
 /* begin_procedure */
#line 11 "../Main.m3"
struct Main__Main_Frame_t {
#line 11 "../Main.m3"
ADDRESS _unused;
#line 11 "../Main.m3"
};
#line 11 "../Main.m3"
void /*TypeText3*/ 
__cdecl
Main__Main(void)
{
#line 11 "../Main.m3"
 /* Var_Type2 */ ADDRESS Main_m_11_L_12={0};//always-init
#line 11 "../Main.m3"
 /* Var_Type2 */ ADDRESS Main_m_13_L_14={0};//always-init
#line 11 "../Main.m3"
 /* Var_Type3 */ STRUCT(8) Main_m_15_L_16={0};//always-init
#line 11 "../Main.m3"
 /* Var_Type3 */ STRUCT(8) Main_m_17_L_18={0};//always-init
#line 11 "../Main.m3"
 /* Var_Type3 */ STRUCT(8) Main_m_19_L_20={0};//always-init
#line 11 "../Main.m3"
 /* Var_Type2 */ ADDRESS Main_m_21_L_22={0};//always-init
#line 11 "../Main.m3"
 /* Var_Type3 */ STRUCT(8) Main_m_23_L_24={0};//always-init
#line 11 "../Main.m3"
 /* Var_Type3 */ STRUCT(16) Main_m_25_L_26={0};//always-init
#line 11 "../Main.m3"
 /* Var_Type3 */ STRUCT(136) Main_m_27_L_28={0};//always-init
#line 11 "../Main.m3"
 /* Var_Type2 */ UINT16 Main_m_29_L_30={0};//always-init
#line 11 "../Main.m3"
 /* Var_Type2 */ ADDRESS Main_m_31_L_32={0};//always-init
#line 11 "../Main.m3"
 /* Var_Type2 */ ADDRESS Main_m_33_L_34={0};//always-init
#line 11 "../Main.m3"
Main__Main_Frame_t _frame;
#line 11 "../Main.m3"
_frame._unused=(ADDRESS)&_frame;
#line 11 "../Main.m3"
 /* set_source_line */
#line 11 "../Main.m3"
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
(*(ADDRESS*)(&Main_m_11_L_12))=(ADDRESS)(((ADDRESS)(RTHooks__AllocateTracedObj(
  ( ADDRESS )(((ADDRESS)(*((ADDRESS*)(INT64_(184)+((ADDRESS)(&Main_m_M_Main_L_9)))))) )))));
#line 13 "../Main.m3"
 /* load */
#line 13 "../Main.m3"
 /* load_indirect */
#line 13 "../Main.m3"
 /* load_indirect */
#line 13 "../Main.m3"
 /* store */
#line 13 "../Main.m3"
(*(ADDRESS*)(&Main_m_13_L_14))=(ADDRESS)(((ADDRESS)(*((ADDRESS*)(*((ADDRESS*)(Main_m_11_L_12)))))));
#line 13 "../Main.m3"
 /* start_call_indirect */
#line 13 "../Main.m3"
 /* load */
#line 13 "../Main.m3"
 /* pop_param */
#line 13 "../Main.m3"
 /* load */
#line 13 "../Main.m3"
 /* load_indirect */
#line 13 "../Main.m3"
 /* pop_struct */
#line 13 "../Main.m3"
 /* load */
#line 13 "../Main.m3"
 /* load_indirect */
#line 13 "../Main.m3"
 /* load_address */
#line 13 "../Main.m3"
 /* swap */
#line 13 "../Main.m3"
 /* copy */
#line 13 "../Main.m3"
m3_memcpy(
 &Main_m_15_L_16,
 *((ADDRESS*)(INT64_(296)+((ADDRESS)(*((ADDRESS*)(INT64_(200)+((ADDRESS)(&Main_m_M_Main_L_9)))))))),
 8);
#line 13 "../Main.m3"
 /* load_address */
#line 13 "../Main.m3"
 /* pop_param */
#line 13 "../Main.m3"
 /* load */
#line 13 "../Main.m3"
 /* load_indirect */
#line 13 "../Main.m3"
 /* pop_struct */
#line 13 "../Main.m3"
 /* load */
#line 13 "../Main.m3"
 /* load_indirect */
#line 13 "../Main.m3"
 /* load_address */
#line 13 "../Main.m3"
 /* swap */
#line 13 "../Main.m3"
 /* copy */
#line 13 "../Main.m3"
m3_memcpy(
 &Main_m_17_L_18,
 *((ADDRESS*)(INT64_(280)+((ADDRESS)(*((ADDRESS*)(INT64_(200)+((ADDRESS)(&Main_m_M_Main_L_9)))))))),
 8);
#line 13 "../Main.m3"
 /* load_address */
#line 13 "../Main.m3"
 /* pop_param */
#line 13 "../Main.m3"
 /* load_address */
#line 13 "../Main.m3"
 /* pop_struct */
#line 13 "../Main.m3"
 /* load_address */
#line 13 "../Main.m3"
 /* pop_param */
#line 13 "../Main.m3"
 /* load */
#line 13 "../Main.m3"
 /* load_indirect */
#line 13 "../Main.m3"
 /* pop_struct */
#line 13 "../Main.m3"
 /* load */
#line 13 "../Main.m3"
 /* load_indirect */
#line 13 "../Main.m3"
 /* load_address */
#line 13 "../Main.m3"
 /* swap */
#line 13 "../Main.m3"
 /* copy */
#line 13 "../Main.m3"
m3_memcpy(
 &Main_m_19_L_20,
 *((ADDRESS*)(INT64_(264)+((ADDRESS)(*((ADDRESS*)(INT64_(200)+((ADDRESS)(&Main_m_M_Main_L_9)))))))),
 8);
#line 13 "../Main.m3"
 /* load_address */
#line 13 "../Main.m3"
 /* pop_param */
#line 13 "../Main.m3"
 /* load */
#line 13 "../Main.m3"
 /* load_indirect */
#line 13 "../Main.m3"
 /* pop_param */
#line 13 "../Main.m3"
 /* load */
#line 13 "../Main.m3"
 /* load_indirect */
#line 13 "../Main.m3"
 /* store */
#line 13 "../Main.m3"
(*(ADDRESS*)(&Main_m_21_L_22))=(ADDRESS)(((ADDRESS)(*((ADDRESS*)(INT64_(272)+((ADDRESS)(*((ADDRESS*)(INT64_(200)+((ADDRESS)(&Main_m_M_Main_L_9)))))))))));
#line 13 "../Main.m3"
 /* load */
#line 13 "../Main.m3"
 /* load_indirect */
#line 13 "../Main.m3"
 /* load_address */
#line 13 "../Main.m3"
 /* swap */
#line 13 "../Main.m3"
 /* copy */
#line 13 "../Main.m3"
m3_memcpy(
 &Main_m_23_L_24,
 *((ADDRESS*)(Main_m_21_L_22)),
 8);
#line 13 "../Main.m3"
 /* load_address */
#line 13 "../Main.m3"
 /* store */
#line 13 "../Main.m3"
(*(ADDRESS*)(&Main_m_25_L_26))=(ADDRESS)(((ADDRESS)(&Main_m_23_L_24)));
#line 13 "../Main.m3"
 /* load */
#line 13 "../Main.m3"
 /* load_indirect */
#line 13 "../Main.m3"
 /* store */
#line 13 "../Main.m3"
(*(INT64*)((8)+(char*)(&Main_m_25_L_26)))=(INT64)( *((INT64*)(INT64_(8)+((ADDRESS)(Main_m_21_L_22)))));
#line 13 "../Main.m3"
 /* load_address */
#line 13 "../Main.m3"
 /* pop_param */
#line 13 "../Main.m3"
 /* load */
#line 13 "../Main.m3"
 /* load_indirect */
#line 13 "../Main.m3"
 /* pop_struct */
#line 13 "../Main.m3"
 /* load */
#line 13 "../Main.m3"
 /* load_indirect */
#line 13 "../Main.m3"
 /* load_address */
#line 13 "../Main.m3"
 /* swap */
#line 13 "../Main.m3"
 /* copy */
#line 13 "../Main.m3"
m3_memcpy(
 &Main_m_27_L_28,
 *((ADDRESS*)(INT64_(256)+((ADDRESS)(*((ADDRESS*)(INT64_(200)+((ADDRESS)(&Main_m_M_Main_L_9)))))))),
 136);
#line 13 "../Main.m3"
 /* load_address */
#line 13 "../Main.m3"
 /* pop_param */
#line 13 "../Main.m3"
 /* load_integer */
#line 13 "../Main.m3"
 /* pop_param */
#line 13 "../Main.m3"
 /* load_integer */
#line 13 "../Main.m3"
 /* store */
#line 13 "../Main.m3"
(*(UINT16*)(&Main_m_29_L_30))=(INT64)(  INT64_(2));
#line 13 "../Main.m3"
 /* load_address */
#line 13 "../Main.m3"
 /* pop_param */
#line 13 "../Main.m3"
 /* load */
#line 13 "../Main.m3"
 /* check_nil */
#line 13 "../Main.m3"
 /* store */
#line 13 "../Main.m3"
(*(ADDRESS*)(&Main_m_31_L_32))=(ADDRESS)(((ADDRESS)(Main_m_13_L_14)));
#line 13 "../Main.m3"
 /* load */
#line 13 "../Main.m3"
/*check_nil*/if(!Main_m_31_L_32)Main_m_M_Main_L_9_CRASH(420);
#line 13 "../Main.m3"
 /* call_indirect */
#line 13 "../Main.m3"
((void (__cdecl*)(void*,void*,void*,void*,void*,void*,void*,void*,void*,void*,void*,void*,void*,unsigned short,void*))Main_m_31_L_32)(
 ((ADDRESS)(Main_m_11_L_12)),
 ((TFCD63CAE*)(*((ADDRESS*)(INT64_(296)+((ADDRESS)(*((ADDRESS*)(INT64_(200)+((ADDRESS)(&Main_m_M_Main_L_9)))))))))),
 ((ADDRESS)(&Main_m_15_L_16)),
 ((T5CCFFB05*)(*((ADDRESS*)(INT64_(280)+((ADDRESS)(*((ADDRESS*)(INT64_(200)+((ADDRESS)(&Main_m_M_Main_L_9)))))))))),
 ((ADDRESS)(&Main_m_17_L_18)),
 ((TE99B66B4*)(&Main_m_7_L_8)),
 ((ADDRESS)(INT64_(8)+((ADDRESS)(&Main_m_7_L_8)))),
 ((T4F238AAE*)(*((ADDRESS*)(INT64_(264)+((ADDRESS)(*((ADDRESS*)(INT64_(200)+((ADDRESS)(&Main_m_M_Main_L_9)))))))))),
 ((ADDRESS)(&Main_m_19_L_20)),
 ((ADDRESS)(*((ADDRESS*)(INT64_(272)+((ADDRESS)(*((ADDRESS*)(INT64_(200)+((ADDRESS)(&Main_m_M_Main_L_9)))))))))),
 ((ADDRESS)(&Main_m_25_L_26)),
 ((T67A7B112*)(*((ADDRESS*)(INT64_(256)+((ADDRESS)(*((ADDRESS*)(INT64_(200)+((ADDRESS)(&Main_m_M_Main_L_9)))))))))),
 ((ADDRESS)(&Main_m_27_L_28)),
 ((UINT16)( INT64_(2))),
 ((ADDRESS)(&Main_m_29_L_30)));
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
(*(ADDRESS*)(&Main_m_21_L_22))=(ADDRESS)(((ADDRESS)(RTHooks__AllocateTracedObj(
  ( ADDRESS )(((ADDRESS)(*((ADDRESS*)(INT64_(184)+((ADDRESS)(&Main_m_M_Main_L_9)))))) )))));
#line 14 "../Main.m3"
 /* load */
#line 14 "../Main.m3"
 /* load_indirect */
#line 14 "../Main.m3"
 /* load_indirect */
#line 14 "../Main.m3"
 /* store */
#line 14 "../Main.m3"
(*(ADDRESS*)(&Main_m_13_L_14))=(ADDRESS)(((ADDRESS)(*((ADDRESS*)(*((ADDRESS*)(Main_m_21_L_22)))))));
#line 14 "../Main.m3"
 /* start_call_indirect */
#line 14 "../Main.m3"
 /* load */
#line 14 "../Main.m3"
 /* pop_param */
#line 14 "../Main.m3"
 /* load */
#line 14 "../Main.m3"
 /* load_indirect */
#line 14 "../Main.m3"
 /* pop_struct */
#line 14 "../Main.m3"
 /* load */
#line 14 "../Main.m3"
 /* load_indirect */
#line 14 "../Main.m3"
 /* load_address */
#line 14 "../Main.m3"
 /* swap */
#line 14 "../Main.m3"
 /* copy */
#line 14 "../Main.m3"
m3_memcpy(
 &Main_m_23_L_24,
 *((ADDRESS*)(INT64_(296)+((ADDRESS)(*((ADDRESS*)(INT64_(200)+((ADDRESS)(&Main_m_M_Main_L_9)))))))),
 8);
#line 14 "../Main.m3"
 /* load_address */
#line 14 "../Main.m3"
 /* pop_param */
#line 14 "../Main.m3"
 /* load */
#line 14 "../Main.m3"
 /* load_indirect */
#line 14 "../Main.m3"
 /* pop_struct */
#line 14 "../Main.m3"
 /* load */
#line 14 "../Main.m3"
 /* load_indirect */
#line 14 "../Main.m3"
 /* load_address */
#line 14 "../Main.m3"
 /* swap */
#line 14 "../Main.m3"
 /* copy */
#line 14 "../Main.m3"
m3_memcpy(
 &Main_m_19_L_20,
 *((ADDRESS*)(INT64_(280)+((ADDRESS)(*((ADDRESS*)(INT64_(200)+((ADDRESS)(&Main_m_M_Main_L_9)))))))),
 8);
#line 14 "../Main.m3"
 /* load_address */
#line 14 "../Main.m3"
 /* pop_param */
#line 14 "../Main.m3"
 /* load_address */
#line 14 "../Main.m3"
 /* pop_struct */
#line 14 "../Main.m3"
 /* load_address */
#line 14 "../Main.m3"
 /* pop_param */
#line 14 "../Main.m3"
 /* load */
#line 14 "../Main.m3"
 /* load_indirect */
#line 14 "../Main.m3"
 /* pop_struct */
#line 14 "../Main.m3"
 /* load */
#line 14 "../Main.m3"
 /* load_indirect */
#line 14 "../Main.m3"
 /* load_address */
#line 14 "../Main.m3"
 /* swap */
#line 14 "../Main.m3"
 /* copy */
#line 14 "../Main.m3"
m3_memcpy(
 &Main_m_17_L_18,
 *((ADDRESS*)(INT64_(264)+((ADDRESS)(*((ADDRESS*)(INT64_(200)+((ADDRESS)(&Main_m_M_Main_L_9)))))))),
 8);
#line 14 "../Main.m3"
 /* load_address */
#line 14 "../Main.m3"
 /* pop_param */
#line 14 "../Main.m3"
 /* load */
#line 14 "../Main.m3"
 /* load_indirect */
#line 14 "../Main.m3"
 /* pop_param */
#line 14 "../Main.m3"
 /* load */
#line 14 "../Main.m3"
 /* load_indirect */
#line 14 "../Main.m3"
 /* store */
#line 14 "../Main.m3"
(*(ADDRESS*)(&Main_m_11_L_12))=(ADDRESS)(((ADDRESS)(*((ADDRESS*)(INT64_(272)+((ADDRESS)(*((ADDRESS*)(INT64_(200)+((ADDRESS)(&Main_m_M_Main_L_9)))))))))));
#line 14 "../Main.m3"
 /* load */
#line 14 "../Main.m3"
 /* load_indirect */
#line 14 "../Main.m3"
 /* load_address */
#line 14 "../Main.m3"
 /* swap */
#line 14 "../Main.m3"
 /* copy */
#line 14 "../Main.m3"
m3_memcpy(
 &Main_m_15_L_16,
 *((ADDRESS*)(Main_m_11_L_12)),
 8);
#line 14 "../Main.m3"
 /* load_address */
#line 14 "../Main.m3"
 /* store */
#line 14 "../Main.m3"
(*(ADDRESS*)(&Main_m_25_L_26))=(ADDRESS)(((ADDRESS)(&Main_m_15_L_16)));
#line 14 "../Main.m3"
 /* load */
#line 14 "../Main.m3"
 /* load_indirect */
#line 14 "../Main.m3"
 /* store */
#line 14 "../Main.m3"
(*(INT64*)((8)+(char*)(&Main_m_25_L_26)))=(INT64)( *((INT64*)(INT64_(8)+((ADDRESS)(Main_m_11_L_12)))));
#line 14 "../Main.m3"
 /* load_address */
#line 14 "../Main.m3"
 /* pop_param */
#line 14 "../Main.m3"
 /* load */
#line 14 "../Main.m3"
 /* load_indirect */
#line 14 "../Main.m3"
 /* pop_struct */
#line 14 "../Main.m3"
 /* load */
#line 14 "../Main.m3"
 /* load_indirect */
#line 14 "../Main.m3"
 /* load_address */
#line 14 "../Main.m3"
 /* swap */
#line 14 "../Main.m3"
 /* copy */
#line 14 "../Main.m3"
m3_memcpy(
 &Main_m_27_L_28,
 *((ADDRESS*)(INT64_(256)+((ADDRESS)(*((ADDRESS*)(INT64_(200)+((ADDRESS)(&Main_m_M_Main_L_9)))))))),
 136);
#line 14 "../Main.m3"
 /* load_address */
#line 14 "../Main.m3"
 /* pop_param */
#line 14 "../Main.m3"
 /* load_integer */
#line 14 "../Main.m3"
 /* pop_param */
#line 14 "../Main.m3"
 /* load_integer */
#line 14 "../Main.m3"
 /* store */
#line 14 "../Main.m3"
(*(UINT16*)(&Main_m_29_L_30))=(INT64)(  INT64_(2));
#line 14 "../Main.m3"
 /* load_address */
#line 14 "../Main.m3"
 /* pop_param */
#line 14 "../Main.m3"
 /* load */
#line 14 "../Main.m3"
 /* check_nil */
#line 14 "../Main.m3"
 /* store */
#line 14 "../Main.m3"
(*(ADDRESS*)(&Main_m_33_L_34))=(ADDRESS)(((ADDRESS)(Main_m_13_L_14)));
#line 14 "../Main.m3"
 /* load */
#line 14 "../Main.m3"
/*check_nil*/if(!Main_m_33_L_34)Main_m_M_Main_L_9_CRASH(452);
#line 14 "../Main.m3"
 /* call_indirect */
#line 14 "../Main.m3"
((void (__cdecl*)(void*,void*,void*,void*,void*,void*,void*,void*,void*,void*,void*,void*,void*,unsigned short,void*))Main_m_33_L_34)(
 ((ADDRESS)(Main_m_21_L_22)),
 ((TFCD63CAE*)(*((ADDRESS*)(INT64_(296)+((ADDRESS)(*((ADDRESS*)(INT64_(200)+((ADDRESS)(&Main_m_M_Main_L_9)))))))))),
 ((ADDRESS)(&Main_m_23_L_24)),
 ((T5CCFFB05*)(*((ADDRESS*)(INT64_(280)+((ADDRESS)(*((ADDRESS*)(INT64_(200)+((ADDRESS)(&Main_m_M_Main_L_9)))))))))),
 ((ADDRESS)(&Main_m_19_L_20)),
 ((TE99B66B4*)(INT64_(16)+((ADDRESS)(&Main_m_7_L_8)))),
 ((ADDRESS)(INT64_(24)+((ADDRESS)(&Main_m_7_L_8)))),
 ((T4F238AAE*)(*((ADDRESS*)(INT64_(264)+((ADDRESS)(*((ADDRESS*)(INT64_(200)+((ADDRESS)(&Main_m_M_Main_L_9)))))))))),
 ((ADDRESS)(&Main_m_17_L_18)),
 ((ADDRESS)(*((ADDRESS*)(INT64_(272)+((ADDRESS)(*((ADDRESS*)(INT64_(200)+((ADDRESS)(&Main_m_M_Main_L_9)))))))))),
 ((ADDRESS)(&Main_m_25_L_26)),
 ((T67A7B112*)(*((ADDRESS*)(INT64_(256)+((ADDRESS)(*((ADDRESS*)(INT64_(200)+((ADDRESS)(&Main_m_M_Main_L_9)))))))))),
 ((ADDRESS)(&Main_m_27_L_28)),
 ((UINT16)( INT64_(2))),
 ((ADDRESS)(&Main_m_29_L_30)));
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
} /* Main_M3 */
#line 15 "../Main.m3"
 /* module main body Main_M3 */
#line 15 "../Main.m3"
 /* set_source_line */
#line 15 "../Main.m3"
#line 17 "../Main.m3"
 /* begin_procedure */
#line 17 "../Main.m3"
struct Main_M3_Frame_t {
#line 17 "../Main.m3"
ADDRESS _unused;
#line 17 "../Main.m3"
};
#line 17 "../Main.m3"
RT0__ModulePtr
__cdecl
Main_M3(
   /* Param_Type1 */ INTEGER /*TypeText1*/  mode_L_10)
{
#line 17 "../Main.m3"
 /* Var_Type2 */ ADDRESS Main_m_35_L_36={0};//always-init
#line 17 "../Main.m3"
Main_M3_Frame_t _frame;
#line 17 "../Main.m3"
_frame._unused=(ADDRESS)&_frame;
#line 17 "../Main.m3"
 /* load */
#line 17 "../Main.m3"
 /* if_true_or_false */
#line 17 "../Main.m3"
 /* load_host_integer */
#line 17 "../Main.m3"
 /* load_integer */
#line 17 "../Main.m3"
 /* if_compare */
#line 17 "../Main.m3"
if(m3_eq(INT64,
  mode_L_10,
   INT64_(0)))goto L1;
#line 17 "../Main.m3"
 /* set_source_line */
#line 17 "../Main.m3"
#line 18 "../Main.m3"
 /* load */
#line 18 "../Main.m3"
 /* store */
#line 18 "../Main.m3"
(*(ADDRESS*)(&Main_m_35_L_36))=(ADDRESS)(((ADDRESS)(*((ADDRESS*)(INT64_(136)+((ADDRESS)(&Main_m_M_Main_L_9)))))));
#line 18 "../Main.m3"
 /* load */
#line 18 "../Main.m3"
 /* load_indirect */
#line 18 "../Main.m3"
 /* load_integer */
#line 18 "../Main.m3"
 /* add */
#line 18 "../Main.m3"
 /* load */
#line 18 "../Main.m3"
 /* swap */
#line 18 "../Main.m3"
 /* store_indirect */
#line 18 "../Main.m3"
(*(INT64*)((104)+(char*)(Main_m_35_L_36)))=(INT64)( ((INT64)( *((INT64*)(INT64_(104)+((ADDRESS)(Main_m_35_L_36))))+  INT64_(1))));
#line 18 "../Main.m3"
 /* set_source_line */
#line 18 "../Main.m3"
#line 19 "../Main.m3"
 /* load_integer */
#line 19 "../Main.m3"
 /* load */
#line 19 "../Main.m3"
 /* add */
#line 19 "../Main.m3"
 /* store */
#line 19 "../Main.m3"
(*(INT64*)((104)+(char*)(&Main_m_M_Main_L_9)))=(INT64)( ((INT64)(  INT64_(1)+((INT64)(*((INT64*)(INT64_(104)+((ADDRESS)(&Main_m_M_Main_L_9)))))))));
#line 19 "../Main.m3"
 /* set_source_line */
#line 19 "../Main.m3"
#line 20 "../Main.m3"
 /* start_call_direct */
#line 20 "../Main.m3"
 /* call_direct */
#line 20 "../Main.m3"
Main__Main(
 );
#line 20 "../Main.m3"
 /* set_source_line */
#line 20 "../Main.m3"
#line 21 "../Main.m3"
 /* load */
#line 21 "../Main.m3"
 /* store */
#line 21 "../Main.m3"
(*(ADDRESS*)(&Main_m_35_L_36))=(ADDRESS)(((ADDRESS)(*((ADDRESS*)(INT64_(136)+((ADDRESS)(&Main_m_M_Main_L_9)))))));
#line 21 "../Main.m3"
 /* load */
#line 21 "../Main.m3"
 /* load_indirect */
#line 21 "../Main.m3"
 /* load_integer */
#line 21 "../Main.m3"
 /* add */
#line 21 "../Main.m3"
 /* load */
#line 21 "../Main.m3"
 /* swap */
#line 21 "../Main.m3"
 /* store_indirect */
#line 21 "../Main.m3"
(*(INT64*)((112)+(char*)(Main_m_35_L_36)))=(INT64)( ((INT64)( *((INT64*)(INT64_(112)+((ADDRESS)(Main_m_35_L_36))))+  INT64_(1))));
#line 21 "../Main.m3"
 /* set_label */
#line 21 "../Main.m3"
L1:;
#line 21 "../Main.m3"
 /* load_address */
#line 21 "../Main.m3"
 /* exit_proc */
#line 21 "../Main.m3"
return (RT0__ModulePtr)(&Main_m_M_Main_L_9);
#line 21 "../Main.m3"
 /* end_procedure */
#line 21 "../Main.m3"
} /* global constant type descriptor */
#line 21 "../Main.m3"
 /* global data type descriptor */
#line 21 "../Main.m3"
 /* module global constants */
#line 21 "../Main.m3"
 /* procedure names */
#line 21 "../Main.m3"
 /* procedure table */
#line 21 "../Main.m3"
 /* file name */
#line 21 "../Main.m3"
 /* module global data */
#line 21 "../Main.m3"
 /* load map


 global data allocation for M_Main
     0   104  8  *module info*
   104     8  8  Main.Main_var_b
   112    24  8  import Main
   136    24  8  import HighlightVBT
   160    24  8  import RTHooks
   184    16  8  typecell ptr
   200    24  8  import VBT
   224     0  8  *TOTAL*


 global constants for M_Main
     0     8  8  *recordConstructor*
     8     8  8  *recordConstructor*
    16     8  8  *recordConstructor*
    24     8  8  *recordConstructor*
    32    13  8  *proc names*
    48    40  8  *proc info*
    88    11  1  *string*
   104     0  8  *TOTAL*
 */
#line 21 "../Main.m3"
 /* end unit */
#line 21 "../Main.m3"

#ifdef __cplusplus

} /* extern "C" */
#endif
 /* set_runtime_proc */
 /* set_runtime_proc */
 /* set_runtime_proc */
