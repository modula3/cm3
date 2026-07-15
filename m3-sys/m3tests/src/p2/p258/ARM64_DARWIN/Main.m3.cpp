// library:pgm
// source_base_name:Main
// target_name:Main.m3.cpp
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
 /* declare_formal */
 /* declare_formal */
 /* declare_subrange */
/*subrange_define*/typedef UINT8 T2DA6581D_8;
 /* declare_set */

#ifndef TA1CC839C
#define TA1CC839C TA1CC839C
/*type_typedef*/typedef UINT32 TA1CC839C;

#endif
 /* declare_subrange */
/*subrange_define*/typedef UINT8 T2EA6581D_8;
 /* declare_set */

#ifndef TFA01F0E5
#define TFA01F0E5 TFA01F0E5
/*type_typedef*/typedef INT64 TFA01F0E5;

#endif
 /* declare_subrange */
/*subrange_define*/typedef UINT8 T2FA3581D_8;
 /* declare_set */

#ifndef T5B4F85B8
#define T5B4F85B8 T5B4F85B8
/*type_typedef*/typedef INT64 T5B4F85B8;

#endif
 /* declare_subrange */
/*subrange_define*/typedef UINT8 T28A3581D_8;
 /* declare_set */
 /* declare_array */
/*array_forwardDeclare*/struct TE4C005F6;typedef struct TE4C005F6 TE4C005F6;

#ifndef TE4C005F6
#define TE4C005F6 TE4C005F6
/*fixedArray_define*/struct TE4C005F6{WORD_T _elts[2];};
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
 /* declare_indirect */
typedef TF400F3DB*TBFF0C24;
 /* declare_proctype */
 /* declare_formal */
 /* declare_formal */
 /* declare_proctype */
 /* declare_formal */
 /* declare_formal */
 /* declare_formal */
 /* declare_formal */
 /* declare_formal */
 /* declare_formal */
 /* declare_record */
 /* declare_field */
 /* declare_record */
 /* declare_field */
 /* DeclareTypes_FlushOnce size:3 */

#if 0 /* avoid type hash collions */
typedef 
void(__cdecl*T1B5338C3)(INTEGER,INTEGER);
#else
typedef void (__cdecl*T1B5338C3)(void);
#endif

#if 0 /* avoid type hash collions */
typedef 
void(__cdecl*TA4BB9882)(ADDRESS,INTEGER);
#else
typedef void (__cdecl*TA4BB9882)(void);
#endif

#if 0 /* avoid type hash collions */
typedef 
void(__cdecl*TCA5A0C48)(INTEGER,INTEGER,INTEGER,INTEGER,INTEGER,ADDRESS);
#else
typedef void (__cdecl*TCA5A0C48)(void);
#endif
 /* DeclareTypes_FlushOnce size:0 */
 /* end: DeclareTypes */
 /* begin: helper functions */
typedef WORD_T* SET;
#define SET_GRAIN (sizeof(WORD_T) * 8)

#ifndef m3setset_range
#define m3setset_range m3setset_range
#define M3_HIGH_BITS(a) ((~(WORD_T)0) << (a))
#define M3_LOW_BITS(a)  ((~(WORD_T)0) >> (SET_GRAIN - (a) - 1))
static void __stdcall m3_set_range(WORD_T b, WORD_T a, WORD_T* s)
{
  if (a > b) {
    /* no bits to set */
  } else {
    WORD_T i = 0;
    WORD_T const a_word = a / SET_GRAIN;
    WORD_T const b_word = b / SET_GRAIN;
    WORD_T const high_bits = M3_HIGH_BITS(a % SET_GRAIN);
    WORD_T const low_bits = M3_LOW_BITS(b % SET_GRAIN);
    if (a_word == b_word)
    {
      s[a_word] |= (high_bits & low_bits);
    }
    else
    {
      s[a_word] |= high_bits;
      for (i = a_word + 1; i < b_word; ++i)
        s[i] = ~(WORD_T)0;
      s[b_word] |= low_bits;
    }
  }
}

#endif

#if __GNUC__ > 2 || __GNUC__ == 2 && __GNUC_MINOR__ >= 5
#define M3_ATTRIBUTE_NO_RETURN __attribute__((__noreturn__))
#else
#define M3_ATTRIBUTE_NO_RETURN
#endif
 /* end: helper functions */

#ifndef struct_16_t
#define struct_16_t struct_16_t
STRUCT8(16)
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
/*Proc_ForwardDeclareFrameType*/struct Dump_I3_Frame_t;typedef struct Dump_I3_Frame_t Dump_I3_Frame_t;
 /* internal_declare_param */
RT0__ModulePtr
__cdecl
Dump_I3(
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
 /* import_procedure */
/*Proc_ForwardDeclareFrameType*/struct Dump_Frame_t;typedef struct Dump_Frame_t Dump_Frame_t;
 /* internal_declare_param */
 /* internal_declare_param */
 /* internal_declare_param */
 /* internal_declare_param */
 /* internal_declare_param */
 /* internal_declare_param */
void /*TypeText3*/ 
__cdecl
Dump(
   /* Param_Type1 */ INTEGER type_L_5,
   /* Param_Type1 */ INTEGER offset_L_6,
   /* Param_Type1 */ INTEGER count_L_7,
   /* Param_Type1 */ INTEGER bitsize_L_8,
   /* Param_Type1 */ INTEGER bytesize_L_9,
   /* Param_Type1 */ ADDRESS address_L_10);
 /* end: imports */
 /* begin: locals */
 /* declare_segment name:<NIL> typeid:TFFFFFFFF const:TRUE */
/*declare_segment*/struct Main_m_11_L_12_t;
/*declare_segment*/typedef struct Main_m_11_L_12_t Main_m_11_L_12_t;
 /* declare_segment name:M_Main typeid:TFFFFFFFF const:FALSE */
 /* handler_name_prefixes:Main_M3_LINE_ */
 /* handler_name_prefixes:Main_I3_LINE_ */
/*declare_segment*/struct Main_m_M_Main_L_13_t;
/*declare_segment*/typedef struct Main_m_M_Main_L_13_t Main_m_M_Main_L_13_t;
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct Main_M3_Frame_t;typedef struct Main_M3_Frame_t Main_M3_Frame_t;
 /* internal_declare_param */
RT0__ModulePtr
__cdecl
Main_M3(
   /* Param_Type1 */ INTEGER /*TypeText1*/  mode_L_14);
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct Main__F31_Frame_t;typedef struct Main__F31_Frame_t Main__F31_Frame_t;
 /* declare_local */
 /* declare_local */
 /* internal_declare_param */
 /* internal_declare_param */
void /*TypeText3*/ 
__cdecl
Main__F31(
   /* Param_Type1 */ INTEGER start_L_17,
   /* Param_Type1 */ INTEGER count_L_18);
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct Main__F32_Frame_t;typedef struct Main__F32_Frame_t Main__F32_Frame_t;
 /* declare_local */
 /* declare_local */
 /* internal_declare_param */
 /* internal_declare_param */
void /*TypeText3*/ 
__cdecl
Main__F32(
   /* Param_Type1 */ INTEGER start_L_21,
   /* Param_Type1 */ INTEGER count_L_22);
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct Main__F63_Frame_t;typedef struct Main__F63_Frame_t Main__F63_Frame_t;
 /* declare_local */
 /* declare_local */
 /* internal_declare_param */
 /* internal_declare_param */
void /*TypeText3*/ 
__cdecl
Main__F63(
   /* Param_Type1 */ INTEGER start_L_25,
   /* Param_Type1 */ INTEGER count_L_26);
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct Main__F64_Frame_t;typedef struct Main__F64_Frame_t Main__F64_Frame_t;
 /* declare_local */
 /* declare_local */
 /* internal_declare_param */
 /* internal_declare_param */
void /*TypeText3*/ 
__cdecl
Main__F64(
   /* Param_Type1 */ INTEGER start_L_29,
   /* Param_Type1 */ INTEGER count_L_30);
 /* Locals_begin_procedure */
 /* begin_block */
 /* AllocateTemps_check_range */
 /* AllocateTemps_common */
 /* declare_temp */
 /* declare_local */
 /* AllocateTemps_check_range */
 /* AllocateTemps_common */
 /* declare_temp */
 /* declare_local */
 /* Locals_end_procedure */
 /* end_block */
 /* Locals_begin_procedure */
 /* begin_block */
 /* AllocateTemps_check_range */
 /* AllocateTemps_common */
 /* declare_temp */
 /* declare_local */
 /* AllocateTemps_check_range */
 /* AllocateTemps_common */
 /* declare_temp */
 /* declare_local */
 /* Locals_end_procedure */
 /* end_block */
 /* Locals_begin_procedure */
 /* begin_block */
 /* AllocateTemps_check_range */
 /* AllocateTemps_common */
 /* declare_temp */
 /* declare_local */
 /* AllocateTemps_check_range */
 /* AllocateTemps_common */
 /* declare_temp */
 /* declare_local */
 /* Locals_end_procedure */
 /* end_block */
 /* Locals_begin_procedure */
 /* begin_block */
 /* declare_temp */
 /* declare_local */
 /* AllocateTemps_check_range */
 /* AllocateTemps_common */
 /* declare_temp */
 /* declare_local */
 /* AllocateTemps_check_range */
 /* AllocateTemps_common */
 /* declare_temp */
 /* declare_local */
 /* Locals_end_procedure */
 /* end_block */
 /* Locals_begin_procedure */
 /* begin_block */
 /* declare_local */
 /* declare_local */
 /* AllocateTemps_check_index */
 /* AllocateTemps_common */
 /* declare_temp */
 /* declare_local */
 /* declare_temp */
 /* declare_local */
 /* declare_temp */
 /* declare_local */
 /* declare_temp */
 /* declare_local */
 /* declare_local */
 /* declare_local */
 /* Locals_end_procedure */
 /* end_block */
 /* end: locals */
 /* begin: segments/globals */
 /* bind_segment */
 /* begin_init */
 /* init_var */
 /* init_int */
 /* init_int */
 /* init_int */
 /* init_int */
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
struct Main_m_11_L_12_t{ADDRESS L_61[1];
INT64 L_62[4];
UINT8 L_63[7];
char L_64[1];
UINT8 L_65[3];
char L_66[1];
UINT8 L_67[3];
char L_68[1];
UINT8 L_69[3];
char L_70[1];
UINT8 L_71[3];
char L_72[1];
ADDRESS L_73[10];
char L_74[8];
UINT8 L_75[10];
char L_76[14];
};
static  const Main_m_11_L_12_t Main_m_11_L_12={{16+(char*)&Main_m_11_L_12},{INT64_(3),INT64_(0),INT64_(32),INT64_(64)},{'M','a','i','n','_','M','3'},{0 /* 1 */ ,},{'F','6','4'},{0 /* 1 */ ,},{'F','6','3'},{0 /* 1 */ ,},{'F','3','2'},{0 /* 1 */ ,},{'F','3','1'},{0 /* 1 */ ,},{(ADDRESS)&Main_M3,40+(char*)&Main_m_11_L_12,(ADDRESS)&Main__F64,48+(char*)&Main_m_11_L_12,(ADDRESS)&Main__F63,52+(char*)&Main_m_11_L_12,(ADDRESS)&Main__F32,56+(char*)&Main_m_11_L_12,(ADDRESS)&Main__F31,60+(char*)&Main_m_11_L_12},{0 /* 1 */ ,0 /* 2 */ ,0 /* 3 */ ,0 /* 4 */ ,0 /* 5 */ ,0 /* 6 */ ,0 /* 7 */ ,0 /* 8 */ ,},{'.','.','/','M','a','i','n','.','m','3'},{0 /* 1 */ ,0 /* 2 */ ,0 /* 3 */ ,0 /* 4 */ ,0 /* 5 */ ,0 /* 6 */ ,0 /* 7 */ ,0 /* 8 */ ,0 /* 9 */ ,0 /* 10 */ ,0 /* 11 */ ,0 /* 12 */ ,0 /* 13 */ ,0 /* 14 */ ,}};
 /* bind_segment */
 /* begin_init */
 /* init_var */
 /* init_var */
 /* init_var */
 /* init_proc */
 /* init_int */
 /* init_var */
 /* init_proc */
 /* init_var */
 /* init_proc */
 /* init_var */
 /* init_proc */
 /* end_init */
struct Main_m_M_Main_L_13_t{ADDRESS L_77[1];
char L_78[32];
ADDRESS L_79[1];
char L_80[24];
ADDRESS L_81[1];
char L_82[8];
ADDRESS L_83[1];
INT64 L_84[1];
ADDRESS L_85[1];
char L_86[8];
ADDRESS L_87[2];
char L_88[8];
ADDRESS L_89[2];
char L_90[8];
ADDRESS L_91[1];
char L_92[16];
};
static Main_m_M_Main_L_13_t Main_m_M_Main_L_13={{152+(char*)&Main_m_11_L_12},{0 /* 1 */ ,0 /* 2 */ ,0 /* 3 */ ,0 /* 4 */ ,0 /* 5 */ ,0 /* 6 */ ,0 /* 7 */ ,0 /* 8 */ ,0 /* 9 */ ,0 /* 10 */ ,0 /* 11 */ ,0 /* 12 */ ,0 /* 13 */ ,0 /* 14 */ ,0 /* 15 */ ,0 /* 16 */ ,0 /* 17 */ ,0 /* 18 */ ,0 /* 19 */ ,0 /* 20 */ ,0 /* 21 */ ,0 /* 22 */ ,0 /* 23 */ ,0 /* 24 */ ,0 /* 25 */ ,0 /* 26 */ ,0 /* 27 */ ,0 /* 28 */ ,0 /* 29 */ ,0 /* 30 */ ,0 /* 31 */ ,0 /* 32 */ ,},{64+(char*)&Main_m_11_L_12},{0 /* 1 */ ,0 /* 2 */ ,0 /* 3 */ ,0 /* 4 */ ,0 /* 5 */ ,0 /* 6 */ ,0 /* 7 */ ,0 /* 8 */ ,0 /* 9 */ ,0 /* 10 */ ,0 /* 11 */ ,0 /* 12 */ ,0 /* 13 */ ,0 /* 14 */ ,0 /* 15 */ ,0 /* 16 */ ,0 /* 17 */ ,0 /* 18 */ ,0 /* 19 */ ,0 /* 20 */ ,0 /* 21 */ ,0 /* 22 */ ,0 /* 23 */ ,0 /* 24 */ ,},{112+(char*)&Main_m_M_Main_L_13},{0 /* 1 */ ,0 /* 2 */ ,0 /* 3 */ ,0 /* 4 */ ,0 /* 5 */ ,0 /* 6 */ ,0 /* 7 */ ,0 /* 8 */ ,},{(ADDRESS)&Main_M3},{INT64_(3)},{(char*)&Main_m_11_L_12},{0 /* 1 */ ,0 /* 2 */ ,0 /* 3 */ ,0 /* 4 */ ,0 /* 5 */ 
,0 /* 6 */ ,0 /* 7 */ ,0 /* 8 */ ,},{(ADDRESS)&Main_I3,136+(char*)&Main_m_M_Main_L_13},{0 /* 1 */ ,0 /* 2 */ ,0 /* 3 */ ,0 /* 4 */ ,0 /* 5 */ ,0 /* 6 */ ,0 /* 7 */ ,0 /* 8 */ ,},{(ADDRESS)&Dump_I3,160+(char*)&Main_m_M_Main_L_13},{0 /* 1 */ ,0 /* 2 */ ,0 /* 3 */ ,0 /* 4 */ ,0 /* 5 */ ,0 /* 6 */ ,0 /* 7 */ ,0 /* 8 */ ,},{(ADDRESS)&RTHooks_I3},{0 /* 1 */ ,0 /* 2 */ ,0 /* 3 */ ,0 /* 4 */ ,0 /* 5 */ ,0 /* 6 */ ,0 /* 7 */ ,0 /* 8 */ ,0 /* 9 */ ,0 /* 10 */ ,0 /* 11 */ ,0 /* 12 */ ,0 /* 13 */ ,0 /* 14 */ ,0 /* 15 */ ,0 /* 16 */ ,}};
static void __cdecl Main_m_M_Main_L_13_CRASH(WORD_T code) M3_ATTRIBUTE_NO_RETURN;
static void __cdecl Main_m_M_Main_L_13_CRASH(WORD_T code){RTHooks__ReportFault((ADDRESS)&Main_m_M_Main_L_13,code);} /* end: segments/globals */
 /* begin: mark used */
 /* end: mark used */
 /* set_source_file */
 /* set_source_line */
#line 8 "../Main.m3"
 /* module global constants */
#line 8 "../Main.m3"
 /* module global data */
#line 8 "../Main.m3"
 /* set_source_line */
#line 8 "../Main.m3"
#line 63 "../Main.m3"
 /* F31 */
#line 63 "../Main.m3"
 /* set_source_line */
#line 63 "../Main.m3"
#line 11 "../Main.m3"
 /* begin_procedure */
#line 11 "../Main.m3"
struct Main__F31_Frame_t {
#line 11 "../Main.m3"
ADDRESS _unused;
#line 11 "../Main.m3"
};
#line 11 "../Main.m3"
void /*TypeText3*/ 
__cdecl
Main__F31(
   /* Param_Type1 */ INTEGER start_L_17,
   /* Param_Type1 */ INTEGER count_L_18)
{
#line 11 "../Main.m3"
 /* Var_Type1 */ INTEGER end_L_15={0};//always-init
#line 11 "../Main.m3"
 /* Var_Type1 */ TA1CC839C a_L_16={0};//always-init
#line 11 "../Main.m3"
 /* Var_Type2 */ INT64 Main_m_31_L_32={0};//always-init
#line 11 "../Main.m3"
 /* Var_Type2 */ INT64 Main_m_33_L_34={0};//always-init
#line 11 "../Main.m3"
Main__F31_Frame_t _frame;
#line 11 "../Main.m3"
_frame._unused=(ADDRESS)&_frame;
#line 11 "../Main.m3"
 /* set_source_line */
#line 11 "../Main.m3"
#line 15 "../Main.m3"
 /* load */
#line 15 "../Main.m3"
 /* load */
#line 15 "../Main.m3"
 /* add */
#line 15 "../Main.m3"
 /* load_integer */
#line 15 "../Main.m3"
 /* subtract */
#line 15 "../Main.m3"
 /* store */
#line 15 "../Main.m3"
(*(INT64*)(&end_L_15))=(INT64)( ((INT64)( ((INT64)( count_L_18+ start_L_17))-  INT64_(1))));
#line 15 "../Main.m3"
 /* set_source_line */
#line 15 "../Main.m3"
#line 16 "../Main.m3"
 /* load_integer */
#line 16 "../Main.m3"
 /* store */
#line 16 "../Main.m3"
(*(INT32*)(&a_L_16))=(INT64)(  INT64_(0));
#line 16 "../Main.m3"
 /* set_source_line */
#line 16 "../Main.m3"
#line 13 "../Main.m3"
 /* set_source_line */
#line 13 "../Main.m3"
#line 18 "../Main.m3"
 /* load_integer */
#line 18 "../Main.m3"
 /* load */
#line 18 "../Main.m3"
 /* if_compare */
#line 18 "../Main.m3"
if(m3_lt(INT64,
   INT64_(31),
  end_L_15))goto L3;
#line 18 "../Main.m3"
 /* load_integer */
#line 18 "../Main.m3"
 /* load */
#line 18 "../Main.m3"
 /* if_compare */
#line 18 "../Main.m3"
if(m3_gt(INT64,
   INT64_(0),
  end_L_15))goto L3;
#line 18 "../Main.m3"
 /* load_integer */
#line 18 "../Main.m3"
 /* load */
#line 18 "../Main.m3"
 /* if_compare */
#line 18 "../Main.m3"
if(m3_ge(INT64,
   INT64_(31),
  start_L_17))goto L2;
#line 18 "../Main.m3"
 /* set_label */
#line 18 "../Main.m3"
L3:;
#line 18 "../Main.m3"
 /* exit_proc */
#line 18 "../Main.m3"
return;
#line 18 "../Main.m3"
 /* set_label */
#line 18 "../Main.m3"
L2:;
#line 18 "../Main.m3"
 /* set_source_line */
#line 18 "../Main.m3"
#line 21 "../Main.m3"
 /* load_integer */
#line 21 "../Main.m3"
 /* load */
#line 21 "../Main.m3"
 /* check_range */
#line 21 "../Main.m3"
 /* store */
#line 21 "../Main.m3"
(*(INT64*)(&Main_m_31_L_32))=(INT64)( start_L_17);
#line 21 "../Main.m3"
 /* load */
#line 21 "../Main.m3"
if(m3_check_range(INT64,
Main_m_31_L_32,
 INT64_(0),
 INT64_(31)))
#line 21 "../Main.m3"
Main_m_M_Main_L_13_CRASH(673);
#line 21 "../Main.m3"
 /* loophole */
#line 21 "../Main.m3"
 /* load */
#line 21 "../Main.m3"
 /* check_range */
#line 21 "../Main.m3"
 /* store */
#line 21 "../Main.m3"
(*(INT64*)(&Main_m_33_L_34))=(INT64)( end_L_15);
#line 21 "../Main.m3"
 /* load */
#line 21 "../Main.m3"
if(m3_check_range(INT64,
Main_m_33_L_34,
 INT64_(0),
 INT64_(31)))
#line 21 "../Main.m3"
Main_m_M_Main_L_13_CRASH(673);
#line 21 "../Main.m3"
 /* loophole */
#line 21 "../Main.m3"
 /* load_integer */
#line 21 "../Main.m3"
 /* swap */
#line 21 "../Main.m3"
 /* load_integer */
#line 21 "../Main.m3"
 /* swap */
#line 21 "../Main.m3"
 /* subtract */
#line 21 "../Main.m3"
 /* shift_right */
#line 21 "../Main.m3"
 /* swap */
#line 21 "../Main.m3"
 /* load_integer */
#line 21 "../Main.m3"
 /* swap */
#line 21 "../Main.m3"
 /* shift_left */
#line 21 "../Main.m3"
 /* and */
#line 21 "../Main.m3"
 /* or */
#line 21 "../Main.m3"
 /* store */
#line 21 "../Main.m3"
(*(UINT32*)(&a_L_16))=(INT64)( ((INT64)(  INT64_(0)| ((INT64)(((INT64)(  INT64_(-1)>>((UINT64)(((INT64)(  INT64_(63)-((INT64)((INT64)Main_m_33_L_34))))))))& ((INT64)(((UINT64)(  INT64_(-1)))<<((UINT64)((INT64)Main_m_31_L_32)))))))));
#line 21 "../Main.m3"
 /* set_source_line */
#line 21 "../Main.m3"
#line 22 "../Main.m3"
 /* start_call_direct */
#line 22 "../Main.m3"
 /* load_integer */
#line 22 "../Main.m3"
 /* pop_param */
#line 22 "../Main.m3"
 /* load */
#line 22 "../Main.m3"
 /* pop_param */
#line 22 "../Main.m3"
 /* load */
#line 22 "../Main.m3"
 /* pop_param */
#line 22 "../Main.m3"
 /* load_integer */
#line 22 "../Main.m3"
 /* pop_param */
#line 22 "../Main.m3"
 /* load_integer */
#line 22 "../Main.m3"
 /* pop_param */
#line 22 "../Main.m3"
 /* load_address */
#line 22 "../Main.m3"
 /* pop_param */
#line 22 "../Main.m3"
 /* call_direct */
#line 22 "../Main.m3"
Dump(
  ( INTEGER )(  INT64_(31) ),
  ( INTEGER )( start_L_17 ),
  ( INTEGER )( count_L_18 ),
  ( INTEGER )(  INT64_(32) ),
  ( INTEGER )(  INT64_(4) ),
  ( ADDRESS )(((ADDRESS)(&a_L_16)) ));
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
} /* F32 */
#line 23 "../Main.m3"
 /* set_source_line */
#line 23 "../Main.m3"
#line 25 "../Main.m3"
 /* begin_procedure */
#line 25 "../Main.m3"
struct Main__F32_Frame_t {
#line 25 "../Main.m3"
ADDRESS _unused;
#line 25 "../Main.m3"
};
#line 25 "../Main.m3"
void /*TypeText3*/ 
__cdecl
Main__F32(
   /* Param_Type1 */ INTEGER start_L_21,
   /* Param_Type1 */ INTEGER count_L_22)
{
#line 25 "../Main.m3"
 /* Var_Type1 */ INTEGER end_L_19={0};//always-init
#line 25 "../Main.m3"
 /* Var_Type1 */ TFA01F0E5 a_L_20={0};//always-init
#line 25 "../Main.m3"
 /* Var_Type2 */ INT64 Main_m_35_L_36={0};//always-init
#line 25 "../Main.m3"
 /* Var_Type2 */ INT64 Main_m_37_L_38={0};//always-init
#line 25 "../Main.m3"
Main__F32_Frame_t _frame;
#line 25 "../Main.m3"
_frame._unused=(ADDRESS)&_frame;
#line 25 "../Main.m3"
 /* set_source_line */
#line 25 "../Main.m3"
#line 29 "../Main.m3"
 /* load */
#line 29 "../Main.m3"
 /* load */
#line 29 "../Main.m3"
 /* add */
#line 29 "../Main.m3"
 /* load_integer */
#line 29 "../Main.m3"
 /* subtract */
#line 29 "../Main.m3"
 /* store */
#line 29 "../Main.m3"
(*(INT64*)(&end_L_19))=(INT64)( ((INT64)( ((INT64)( count_L_22+ start_L_21))-  INT64_(1))));
#line 29 "../Main.m3"
 /* set_source_line */
#line 29 "../Main.m3"
#line 30 "../Main.m3"
 /* load_integer */
#line 30 "../Main.m3"
 /* store */
#line 30 "../Main.m3"
(*(INT64*)(&a_L_20))=(INT64)(  INT64_(0));
#line 30 "../Main.m3"
 /* set_source_line */
#line 30 "../Main.m3"
#line 27 "../Main.m3"
 /* set_source_line */
#line 27 "../Main.m3"
#line 32 "../Main.m3"
 /* load_integer */
#line 32 "../Main.m3"
 /* load */
#line 32 "../Main.m3"
 /* if_compare */
#line 32 "../Main.m3"
if(m3_lt(INT64,
   INT64_(32),
  end_L_19))goto L6;
#line 32 "../Main.m3"
 /* load_integer */
#line 32 "../Main.m3"
 /* load */
#line 32 "../Main.m3"
 /* if_compare */
#line 32 "../Main.m3"
if(m3_gt(INT64,
   INT64_(0),
  end_L_19))goto L6;
#line 32 "../Main.m3"
 /* load_integer */
#line 32 "../Main.m3"
 /* load */
#line 32 "../Main.m3"
 /* if_compare */
#line 32 "../Main.m3"
if(m3_ge(INT64,
   INT64_(32),
  start_L_21))goto L5;
#line 32 "../Main.m3"
 /* set_label */
#line 32 "../Main.m3"
L6:;
#line 32 "../Main.m3"
 /* exit_proc */
#line 32 "../Main.m3"
return;
#line 32 "../Main.m3"
 /* set_label */
#line 32 "../Main.m3"
L5:;
#line 32 "../Main.m3"
 /* set_source_line */
#line 32 "../Main.m3"
#line 33 "../Main.m3"
 /* load_integer */
#line 33 "../Main.m3"
 /* load */
#line 33 "../Main.m3"
 /* check_range */
#line 33 "../Main.m3"
 /* store */
#line 33 "../Main.m3"
(*(INT64*)(&Main_m_35_L_36))=(INT64)( start_L_21);
#line 33 "../Main.m3"
 /* load */
#line 33 "../Main.m3"
if(m3_check_range(INT64,
Main_m_35_L_36,
 INT64_(0),
 INT64_(32)))
#line 33 "../Main.m3"
Main_m_M_Main_L_13_CRASH(1057);
#line 33 "../Main.m3"
 /* loophole */
#line 33 "../Main.m3"
 /* load */
#line 33 "../Main.m3"
 /* check_range */
#line 33 "../Main.m3"
 /* store */
#line 33 "../Main.m3"
(*(INT64*)(&Main_m_37_L_38))=(INT64)( end_L_19);
#line 33 "../Main.m3"
 /* load */
#line 33 "../Main.m3"
if(m3_check_range(INT64,
Main_m_37_L_38,
 INT64_(0),
 INT64_(32)))
#line 33 "../Main.m3"
Main_m_M_Main_L_13_CRASH(1057);
#line 33 "../Main.m3"
 /* loophole */
#line 33 "../Main.m3"
 /* load_integer */
#line 33 "../Main.m3"
 /* swap */
#line 33 "../Main.m3"
 /* load_integer */
#line 33 "../Main.m3"
 /* swap */
#line 33 "../Main.m3"
 /* subtract */
#line 33 "../Main.m3"
 /* shift_right */
#line 33 "../Main.m3"
 /* swap */
#line 33 "../Main.m3"
 /* load_integer */
#line 33 "../Main.m3"
 /* swap */
#line 33 "../Main.m3"
 /* shift_left */
#line 33 "../Main.m3"
 /* and */
#line 33 "../Main.m3"
 /* or */
#line 33 "../Main.m3"
 /* store */
#line 33 "../Main.m3"
(*(UINT64*)(&a_L_20))=(INT64)( ((INT64)(  INT64_(0)| ((INT64)(((INT64)(  INT64_(-1)>>((UINT64)(((INT64)(  INT64_(63)-((INT64)((INT64)Main_m_37_L_38))))))))& ((INT64)(((UINT64)(  INT64_(-1)))<<((UINT64)((INT64)Main_m_35_L_36)))))))));
#line 33 "../Main.m3"
 /* set_source_line */
#line 33 "../Main.m3"
#line 34 "../Main.m3"
 /* start_call_direct */
#line 34 "../Main.m3"
 /* load_integer */
#line 34 "../Main.m3"
 /* pop_param */
#line 34 "../Main.m3"
 /* load */
#line 34 "../Main.m3"
 /* pop_param */
#line 34 "../Main.m3"
 /* load */
#line 34 "../Main.m3"
 /* pop_param */
#line 34 "../Main.m3"
 /* load_integer */
#line 34 "../Main.m3"
 /* pop_param */
#line 34 "../Main.m3"
 /* load_integer */
#line 34 "../Main.m3"
 /* pop_param */
#line 34 "../Main.m3"
 /* load_address */
#line 34 "../Main.m3"
 /* pop_param */
#line 34 "../Main.m3"
 /* call_direct */
#line 34 "../Main.m3"
Dump(
  ( INTEGER )(  INT64_(32) ),
  ( INTEGER )( start_L_21 ),
  ( INTEGER )( count_L_22 ),
  ( INTEGER )(  INT64_(64) ),
  ( INTEGER )(  INT64_(8) ),
  ( ADDRESS )(((ADDRESS)(&a_L_20)) ));
#line 34 "../Main.m3"
 /* set_source_line */
#line 34 "../Main.m3"
#line 35 "../Main.m3"
 /* exit_proc */
#line 35 "../Main.m3"
return;
#line 35 "../Main.m3"
 /* end_procedure */
#line 35 "../Main.m3"
} /* F63 */
#line 35 "../Main.m3"
 /* set_source_line */
#line 35 "../Main.m3"
#line 37 "../Main.m3"
 /* begin_procedure */
#line 37 "../Main.m3"
struct Main__F63_Frame_t {
#line 37 "../Main.m3"
ADDRESS _unused;
#line 37 "../Main.m3"
};
#line 37 "../Main.m3"
void /*TypeText3*/ 
__cdecl
Main__F63(
   /* Param_Type1 */ INTEGER start_L_25,
   /* Param_Type1 */ INTEGER count_L_26)
{
#line 37 "../Main.m3"
 /* Var_Type1 */ INTEGER end_L_23={0};//always-init
#line 37 "../Main.m3"
 /* Var_Type1 */ T5B4F85B8 a_L_24={0};//always-init
#line 37 "../Main.m3"
 /* Var_Type2 */ INT64 Main_m_39_L_40={0};//always-init
#line 37 "../Main.m3"
 /* Var_Type2 */ INT64 Main_m_41_L_42={0};//always-init
#line 37 "../Main.m3"
Main__F63_Frame_t _frame;
#line 37 "../Main.m3"
_frame._unused=(ADDRESS)&_frame;
#line 37 "../Main.m3"
 /* set_source_line */
#line 37 "../Main.m3"
#line 41 "../Main.m3"
 /* load */
#line 41 "../Main.m3"
 /* load */
#line 41 "../Main.m3"
 /* add */
#line 41 "../Main.m3"
 /* load_integer */
#line 41 "../Main.m3"
 /* subtract */
#line 41 "../Main.m3"
 /* store */
#line 41 "../Main.m3"
(*(INT64*)(&end_L_23))=(INT64)( ((INT64)( ((INT64)( count_L_26+ start_L_25))-  INT64_(1))));
#line 41 "../Main.m3"
 /* set_source_line */
#line 41 "../Main.m3"
#line 42 "../Main.m3"
 /* load_integer */
#line 42 "../Main.m3"
 /* store */
#line 42 "../Main.m3"
(*(INT64*)(&a_L_24))=(INT64)(  INT64_(0));
#line 42 "../Main.m3"
 /* set_source_line */
#line 42 "../Main.m3"
#line 39 "../Main.m3"
 /* set_source_line */
#line 39 "../Main.m3"
#line 44 "../Main.m3"
 /* load_integer */
#line 44 "../Main.m3"
 /* load */
#line 44 "../Main.m3"
 /* if_compare */
#line 44 "../Main.m3"
if(m3_lt(INT64,
   INT64_(63),
  end_L_23))goto L9;
#line 44 "../Main.m3"
 /* load_integer */
#line 44 "../Main.m3"
 /* load */
#line 44 "../Main.m3"
 /* if_compare */
#line 44 "../Main.m3"
if(m3_gt(INT64,
   INT64_(0),
  end_L_23))goto L9;
#line 44 "../Main.m3"
 /* load_integer */
#line 44 "../Main.m3"
 /* load */
#line 44 "../Main.m3"
 /* if_compare */
#line 44 "../Main.m3"
if(m3_ge(INT64,
   INT64_(63),
  start_L_25))goto L8;
#line 44 "../Main.m3"
 /* set_label */
#line 44 "../Main.m3"
L9:;
#line 44 "../Main.m3"
 /* exit_proc */
#line 44 "../Main.m3"
return;
#line 44 "../Main.m3"
 /* set_label */
#line 44 "../Main.m3"
L8:;
#line 44 "../Main.m3"
 /* set_source_line */
#line 44 "../Main.m3"
#line 45 "../Main.m3"
 /* load_integer */
#line 45 "../Main.m3"
 /* load */
#line 45 "../Main.m3"
 /* check_range */
#line 45 "../Main.m3"
 /* store */
#line 45 "../Main.m3"
(*(INT64*)(&Main_m_39_L_40))=(INT64)( start_L_25);
#line 45 "../Main.m3"
 /* load */
#line 45 "../Main.m3"
if(m3_check_range(INT64,
Main_m_39_L_40,
 INT64_(0),
 INT64_(63)))
#line 45 "../Main.m3"
Main_m_M_Main_L_13_CRASH(1441);
#line 45 "../Main.m3"
 /* loophole */
#line 45 "../Main.m3"
 /* load */
#line 45 "../Main.m3"
 /* check_range */
#line 45 "../Main.m3"
 /* store */
#line 45 "../Main.m3"
(*(INT64*)(&Main_m_41_L_42))=(INT64)( end_L_23);
#line 45 "../Main.m3"
 /* load */
#line 45 "../Main.m3"
if(m3_check_range(INT64,
Main_m_41_L_42,
 INT64_(0),
 INT64_(63)))
#line 45 "../Main.m3"
Main_m_M_Main_L_13_CRASH(1441);
#line 45 "../Main.m3"
 /* loophole */
#line 45 "../Main.m3"
 /* load_integer */
#line 45 "../Main.m3"
 /* swap */
#line 45 "../Main.m3"
 /* load_integer */
#line 45 "../Main.m3"
 /* swap */
#line 45 "../Main.m3"
 /* subtract */
#line 45 "../Main.m3"
 /* shift_right */
#line 45 "../Main.m3"
 /* swap */
#line 45 "../Main.m3"
 /* load_integer */
#line 45 "../Main.m3"
 /* swap */
#line 45 "../Main.m3"
 /* shift_left */
#line 45 "../Main.m3"
 /* and */
#line 45 "../Main.m3"
 /* or */
#line 45 "../Main.m3"
 /* store */
#line 45 "../Main.m3"
(*(UINT64*)(&a_L_24))=(INT64)( ((INT64)(  INT64_(0)| ((INT64)(((INT64)(  INT64_(-1)>>((UINT64)(((INT64)(  INT64_(63)-((INT64)((INT64)Main_m_41_L_42))))))))& ((INT64)(((UINT64)(  INT64_(-1)))<<((UINT64)((INT64)Main_m_39_L_40)))))))));
#line 45 "../Main.m3"
 /* set_source_line */
#line 45 "../Main.m3"
#line 46 "../Main.m3"
 /* start_call_direct */
#line 46 "../Main.m3"
 /* load_integer */
#line 46 "../Main.m3"
 /* pop_param */
#line 46 "../Main.m3"
 /* load */
#line 46 "../Main.m3"
 /* pop_param */
#line 46 "../Main.m3"
 /* load */
#line 46 "../Main.m3"
 /* pop_param */
#line 46 "../Main.m3"
 /* load_integer */
#line 46 "../Main.m3"
 /* pop_param */
#line 46 "../Main.m3"
 /* load_integer */
#line 46 "../Main.m3"
 /* pop_param */
#line 46 "../Main.m3"
 /* load_address */
#line 46 "../Main.m3"
 /* pop_param */
#line 46 "../Main.m3"
 /* call_direct */
#line 46 "../Main.m3"
Dump(
  ( INTEGER )(  INT64_(63) ),
  ( INTEGER )( start_L_25 ),
  ( INTEGER )( count_L_26 ),
  ( INTEGER )(  INT64_(64) ),
  ( INTEGER )(  INT64_(8) ),
  ( ADDRESS )(((ADDRESS)(&a_L_24)) ));
#line 46 "../Main.m3"
 /* set_source_line */
#line 46 "../Main.m3"
#line 47 "../Main.m3"
 /* exit_proc */
#line 47 "../Main.m3"
return;
#line 47 "../Main.m3"
 /* end_procedure */
#line 47 "../Main.m3"
} /* F64 */
#line 47 "../Main.m3"
 /* set_source_line */
#line 47 "../Main.m3"
#line 49 "../Main.m3"
 /* begin_procedure */
#line 49 "../Main.m3"
struct Main__F64_Frame_t {
#line 49 "../Main.m3"
ADDRESS _unused;
#line 49 "../Main.m3"
};
#line 49 "../Main.m3"
void /*TypeText3*/ 
__cdecl
Main__F64(
   /* Param_Type1 */ INTEGER start_L_29,
   /* Param_Type1 */ INTEGER count_L_30)
{
#line 49 "../Main.m3"
 /* Var_Type1 */ INTEGER end_L_27={0};//always-init
#line 49 "../Main.m3"
 /* Var_Type1 */ TE4C005F6 a_L_28={0};//always-init
#line 49 "../Main.m3"
 /* Var_Type3 */ STRUCT(16) Main_m_43_L_44={0};//always-init
#line 49 "../Main.m3"
 /* Var_Type2 */ INT64 Main_m_45_L_46={0};//always-init
#line 49 "../Main.m3"
 /* Var_Type2 */ INT64 Main_m_47_L_48={0};//always-init
#line 49 "../Main.m3"
Main__F64_Frame_t _frame;
#line 49 "../Main.m3"
_frame._unused=(ADDRESS)&_frame;
#line 49 "../Main.m3"
 /* set_source_line */
#line 49 "../Main.m3"
#line 54 "../Main.m3"
 /* load */
#line 54 "../Main.m3"
 /* load */
#line 54 "../Main.m3"
 /* add */
#line 54 "../Main.m3"
 /* load_integer */
#line 54 "../Main.m3"
 /* subtract */
#line 54 "../Main.m3"
 /* store */
#line 54 "../Main.m3"
(*(INT64*)(&end_L_27))=(INT64)( ((INT64)( ((INT64)( count_L_30+ start_L_29))-  INT64_(1))));
#line 54 "../Main.m3"
 /* set_source_line */
#line 54 "../Main.m3"
#line 55 "../Main.m3"
 /* load_integer */
#line 55 "../Main.m3"
 /* store */
#line 55 "../Main.m3"
(*(INT64*)(&a_L_28))=(INT64)(  INT64_(0));
#line 55 "../Main.m3"
 /* load_integer */
#line 55 "../Main.m3"
 /* store */
#line 55 "../Main.m3"
(*(INT64*)((8)+(char*)(&a_L_28)))=(INT64)(  INT64_(0));
#line 55 "../Main.m3"
 /* set_source_line */
#line 55 "../Main.m3"
#line 52 "../Main.m3"
 /* set_source_line */
#line 52 "../Main.m3"
#line 57 "../Main.m3"
 /* load_integer */
#line 57 "../Main.m3"
 /* load */
#line 57 "../Main.m3"
 /* if_compare */
#line 57 "../Main.m3"
if(m3_lt(INT64,
   INT64_(64),
  end_L_27))goto LC;
#line 57 "../Main.m3"
 /* load_integer */
#line 57 "../Main.m3"
 /* load */
#line 57 "../Main.m3"
 /* if_compare */
#line 57 "../Main.m3"
if(m3_gt(INT64,
   INT64_(0),
  end_L_27))goto LC;
#line 57 "../Main.m3"
 /* load_integer */
#line 57 "../Main.m3"
 /* load */
#line 57 "../Main.m3"
 /* if_compare */
#line 57 "../Main.m3"
if(m3_ge(INT64,
   INT64_(64),
  start_L_29))goto LB;
#line 57 "../Main.m3"
 /* set_label */
#line 57 "../Main.m3"
LC:;
#line 57 "../Main.m3"
 /* exit_proc */
#line 57 "../Main.m3"
return;
#line 57 "../Main.m3"
 /* set_label */
#line 57 "../Main.m3"
LB:;
#line 57 "../Main.m3"
 /* set_source_line */
#line 57 "../Main.m3"
#line 58 "../Main.m3"
 /* load_integer */
#line 58 "../Main.m3"
 /* store */
#line 58 "../Main.m3"
(*(INT64*)(&Main_m_43_L_44))=(INT64)(  INT64_(0));
#line 58 "../Main.m3"
 /* load_integer */
#line 58 "../Main.m3"
 /* store */
#line 58 "../Main.m3"
(*(INT64*)((8)+(char*)(&Main_m_43_L_44)))=(INT64)(  INT64_(0));
#line 58 "../Main.m3"
 /* load_address */
#line 58 "../Main.m3"
 /* load */
#line 58 "../Main.m3"
 /* check_range */
#line 58 "../Main.m3"
 /* store */
#line 58 "../Main.m3"
(*(INT64*)(&Main_m_45_L_46))=(INT64)( start_L_29);
#line 58 "../Main.m3"
 /* load */
#line 58 "../Main.m3"
if(m3_check_range(INT64,
Main_m_45_L_46,
 INT64_(0),
 INT64_(64)))
#line 58 "../Main.m3"
Main_m_M_Main_L_13_CRASH(1857);
#line 58 "../Main.m3"
 /* loophole */
#line 58 "../Main.m3"
 /* load */
#line 58 "../Main.m3"
 /* check_range */
#line 58 "../Main.m3"
 /* store */
#line 58 "../Main.m3"
(*(INT64*)(&Main_m_47_L_48))=(INT64)( end_L_27);
#line 58 "../Main.m3"
 /* load */
#line 58 "../Main.m3"
if(m3_check_range(INT64,
Main_m_47_L_48,
 INT64_(0),
 INT64_(64)))
#line 58 "../Main.m3"
Main_m_M_Main_L_13_CRASH(1857);
#line 58 "../Main.m3"
 /* loophole */
#line 58 "../Main.m3"
 /* set_range */
#line 58 "../Main.m3"
m3_set_range(((INT64)((INT64)Main_m_47_L_48)),
 ((INT64)((INT64)Main_m_45_L_46)),
 ((SET)(&Main_m_43_L_44)));
#line 58 "../Main.m3"
 /* load_address */
#line 58 "../Main.m3"
 /* load_address */
#line 58 "../Main.m3"
 /* copy */
#line 58 "../Main.m3"
m3_memmove(
 &a_L_28,
 &Main_m_43_L_44,
 16);
#line 58 "../Main.m3"
 /* set_source_line */
#line 58 "../Main.m3"
#line 59 "../Main.m3"
 /* start_call_direct */
#line 59 "../Main.m3"
 /* load_integer */
#line 59 "../Main.m3"
 /* pop_param */
#line 59 "../Main.m3"
 /* load */
#line 59 "../Main.m3"
 /* pop_param */
#line 59 "../Main.m3"
 /* load */
#line 59 "../Main.m3"
 /* pop_param */
#line 59 "../Main.m3"
 /* load_integer */
#line 59 "../Main.m3"
 /* pop_param */
#line 59 "../Main.m3"
 /* load_integer */
#line 59 "../Main.m3"
 /* pop_param */
#line 59 "../Main.m3"
 /* load_address */
#line 59 "../Main.m3"
 /* pop_param */
#line 59 "../Main.m3"
 /* call_direct */
#line 59 "../Main.m3"
Dump(
  ( INTEGER )(  INT64_(64) ),
  ( INTEGER )( start_L_29 ),
  ( INTEGER )( count_L_30 ),
  ( INTEGER )(  INT64_(128) ),
  ( INTEGER )(  INT64_(16) ),
  ( ADDRESS )(((ADDRESS)(&a_L_28)) ));
#line 59 "../Main.m3"
 /* set_source_line */
#line 59 "../Main.m3"
#line 60 "../Main.m3"
 /* exit_proc */
#line 60 "../Main.m3"
return;
#line 60 "../Main.m3"
 /* end_procedure */
#line 60 "../Main.m3"
} /* Main_M3 */
#line 60 "../Main.m3"
 /* module main body Main_M3 */
#line 60 "../Main.m3"
 /* set_source_line */
#line 60 "../Main.m3"
#line 63 "../Main.m3"
 /* begin_procedure */
#line 63 "../Main.m3"
struct Main_M3_Frame_t {
#line 63 "../Main.m3"
ADDRESS _unused;
#line 63 "../Main.m3"
};
#line 63 "../Main.m3"
RT0__ModulePtr
__cdecl
Main_M3(
   /* Param_Type1 */ INTEGER /*TypeText1*/  mode_L_14)
{
#line 63 "../Main.m3"
 /* Var_Type1 */ INTEGER offset_base_L_49={0};//always-init
#line 63 "../Main.m3"
 /* Var_Type1 */ INTEGER offset_offset_L_50={0};//always-init
#line 63 "../Main.m3"
 /* Var_Type2 */ INT64 Main_m_51_L_52={0};//always-init
#line 63 "../Main.m3"
 /* Var_Type2 */ INT64 Main_m_53_L_54={0};//always-init
#line 63 "../Main.m3"
 /* Var_Type2 */ ADDRESS Main_m_55_L_56={0};//always-init
#line 63 "../Main.m3"
 /* Var_Type2 */ INT64 Main_m_57_L_58={0};//always-init
#line 63 "../Main.m3"
 /* Var_Type1 */ INTEGER offset_L_59={0};//always-init
#line 63 "../Main.m3"
 /* Var_Type1 */ INTEGER count_L_60={0};//always-init
#line 63 "../Main.m3"
Main_M3_Frame_t _frame;
#line 63 "../Main.m3"
_frame._unused=(ADDRESS)&_frame;
#line 63 "../Main.m3"
 /* load */
#line 63 "../Main.m3"
 /* if_true_or_false */
#line 63 "../Main.m3"
 /* load_host_integer */
#line 63 "../Main.m3"
 /* load_integer */
#line 63 "../Main.m3"
 /* if_compare */
#line 63 "../Main.m3"
if(m3_eq(INT64,
  mode_L_14,
   INT64_(0)))goto LD;
#line 63 "../Main.m3"
 /* set_source_line */
#line 63 "../Main.m3"
#line 64 "../Main.m3"
 /* begin_block */
#line 64 "../Main.m3"
 /* load_integer */
#line 64 "../Main.m3"
 /* store */
#line 64 "../Main.m3"
(*(INT64*)(&offset_base_L_49))=(INT64)(  INT64_(0));
#line 64 "../Main.m3"
 /* set_label */
#line 64 "../Main.m3"
LE:;
#line 64 "../Main.m3"
 /* set_source_line */
#line 64 "../Main.m3"
#line 65 "../Main.m3"
 /* begin_block */
#line 65 "../Main.m3"
 /* load_integer */
#line 65 "../Main.m3"
 /* store */
#line 65 "../Main.m3"
(*(INT64*)(&offset_offset_L_50))=(INT64)(  INT64_(-1));
#line 65 "../Main.m3"
 /* set_label */
#line 65 "../Main.m3"
L11:;
#line 65 "../Main.m3"
 /* set_source_line */
#line 65 "../Main.m3"
#line 66 "../Main.m3"
 /* load */
#line 66 "../Main.m3"
 /* load */
#line 66 "../Main.m3"
 /* check_index */
#line 66 "../Main.m3"
 /* swap */
#line 66 "../Main.m3"
 /* store */
#line 66 "../Main.m3"
(*(INT64*)(&Main_m_51_L_52))=(INT64)( offset_base_L_49);
#line 66 "../Main.m3"
 /* load */
#line 66 "../Main.m3"
 /* swap */
#line 66 "../Main.m3"
/*check_index*/if(((UINT64)(*((INT64*)(INT64_(8)+((ADDRESS)(&Main_m_11_L_12))))))<=((UINT64)(Main_m_51_L_52)))Main_m_M_Main_L_13_CRASH(2114);
#line 66 "../Main.m3"
 /* store */
#line 66 "../Main.m3"
(*(INT64*)(&Main_m_53_L_54))=(INT64)( Main_m_51_L_52);
#line 66 "../Main.m3"
 /* load */
#line 66 "../Main.m3"
 /* load */
#line 66 "../Main.m3"
 /* index_address */
#line 66 "../Main.m3"
 /* store */
#line 66 "../Main.m3"
(*(ADDRESS*)(&Main_m_55_L_56))=(ADDRESS)(((ADDRESS)((((ADDRESS)(*((ADDRESS*)(&Main_m_11_L_12))))+(8*( Main_m_53_L_54))))));
#line 66 "../Main.m3"
 /* load */
#line 66 "../Main.m3"
 /* load_indirect */
#line 66 "../Main.m3"
 /* load */
#line 66 "../Main.m3"
 /* add */
#line 66 "../Main.m3"
 /* store */
#line 66 "../Main.m3"
(*(INT64*)(&Main_m_57_L_58))=(INT64)( ((INT64)( *((INT64*)(Main_m_55_L_56))+ offset_offset_L_50)));
#line 66 "../Main.m3"
 /* begin_block */
#line 66 "../Main.m3"
 /* load */
#line 66 "../Main.m3"
 /* store */
#line 66 "../Main.m3"
(*(INT64*)(&offset_L_59))=(INT64)( Main_m_57_L_58);
#line 66 "../Main.m3"
 /* set_source_line */
#line 66 "../Main.m3"
#line 67 "../Main.m3"
 /* load_integer */
#line 67 "../Main.m3"
 /* load */
#line 67 "../Main.m3"
 /* if_compare */
#line 67 "../Main.m3"
if(m3_gt(INT64,
   INT64_(0),
  offset_L_59))goto L15;
#line 67 "../Main.m3"
 /* set_source_line */
#line 67 "../Main.m3"
#line 68 "../Main.m3"
 /* begin_block */
#line 68 "../Main.m3"
 /* load_integer */
#line 68 "../Main.m3"
 /* store */
#line 68 "../Main.m3"
(*(INT64*)(&count_L_60))=(INT64)(  INT64_(0));
#line 68 "../Main.m3"
 /* set_label */
#line 68 "../Main.m3"
L16:;
#line 68 "../Main.m3"
 /* set_source_line */
#line 68 "../Main.m3"
#line 69 "../Main.m3"
 /* start_call_direct */
#line 69 "../Main.m3"
 /* load */
#line 69 "../Main.m3"
 /* pop_param */
#line 69 "../Main.m3"
 /* load */
#line 69 "../Main.m3"
 /* pop_param */
#line 69 "../Main.m3"
 /* call_direct */
#line 69 "../Main.m3"
Main__F31(
  ( INTEGER )( offset_L_59 ),
  ( INTEGER )( count_L_60 ));
#line 69 "../Main.m3"
 /* set_source_line */
#line 69 "../Main.m3"
#line 70 "../Main.m3"
 /* start_call_direct */
#line 70 "../Main.m3"
 /* load */
#line 70 "../Main.m3"
 /* pop_param */
#line 70 "../Main.m3"
 /* load */
#line 70 "../Main.m3"
 /* pop_param */
#line 70 "../Main.m3"
 /* call_direct */
#line 70 "../Main.m3"
Main__F32(
  ( INTEGER )( offset_L_59 ),
  ( INTEGER )( count_L_60 ));
#line 70 "../Main.m3"
 /* set_source_line */
#line 70 "../Main.m3"
#line 71 "../Main.m3"
 /* start_call_direct */
#line 71 "../Main.m3"
 /* load */
#line 71 "../Main.m3"
 /* pop_param */
#line 71 "../Main.m3"
 /* load */
#line 71 "../Main.m3"
 /* pop_param */
#line 71 "../Main.m3"
 /* call_direct */
#line 71 "../Main.m3"
Main__F63(
  ( INTEGER )( offset_L_59 ),
  ( INTEGER )( count_L_60 ));
#line 71 "../Main.m3"
 /* set_source_line */
#line 71 "../Main.m3"
#line 72 "../Main.m3"
 /* start_call_direct */
#line 72 "../Main.m3"
 /* load */
#line 72 "../Main.m3"
 /* pop_param */
#line 72 "../Main.m3"
 /* load */
#line 72 "../Main.m3"
 /* pop_param */
#line 72 "../Main.m3"
 /* call_direct */
#line 72 "../Main.m3"
Main__F64(
  ( INTEGER )( offset_L_59 ),
  ( INTEGER )( count_L_60 ));
#line 72 "../Main.m3"
 /* set_source_line */
#line 72 "../Main.m3"
#line 68 "../Main.m3"
 /* load_integer */
#line 68 "../Main.m3"
 /* load */
#line 68 "../Main.m3"
 /* add */
#line 68 "../Main.m3"
 /* store */
#line 68 "../Main.m3"
(*(INT64*)(&count_L_60))=(INT64)( ((INT64)(  INT64_(1)+ count_L_60)));
#line 68 "../Main.m3"
 /* set_label */
#line 68 "../Main.m3"
 /* load_integer */
#line 68 "../Main.m3"
 /* load */
#line 68 "../Main.m3"
 /* if_compare */
#line 68 "../Main.m3"
if(m3_ge(INT64,
   INT64_(2),
  count_L_60))goto L16;
#line 68 "../Main.m3"
 /* set_label */
#line 68 "../Main.m3"
 /* end_block */
#line 68 "../Main.m3"
 /* set_label */
#line 68 "../Main.m3"
L15:;
#line 68 "../Main.m3"
 /* end_block */
#line 68 "../Main.m3"
 /* set_source_line */
#line 68 "../Main.m3"
#line 65 "../Main.m3"
 /* load_integer */
#line 65 "../Main.m3"
 /* load */
#line 65 "../Main.m3"
 /* add */
#line 65 "../Main.m3"
 /* store */
#line 65 "../Main.m3"
(*(INT64*)(&offset_offset_L_50))=(INT64)( ((INT64)(  INT64_(1)+ offset_offset_L_50)));
#line 65 "../Main.m3"
 /* set_label */
#line 65 "../Main.m3"
 /* load_integer */
#line 65 "../Main.m3"
 /* load */
#line 65 "../Main.m3"
 /* if_compare */
#line 65 "../Main.m3"
if(m3_ge(INT64,
   INT64_(1),
  offset_offset_L_50))goto L11;
#line 65 "../Main.m3"
 /* set_label */
#line 65 "../Main.m3"
 /* end_block */
#line 65 "../Main.m3"
 /* set_source_line */
#line 65 "../Main.m3"
#line 64 "../Main.m3"
 /* load_integer */
#line 64 "../Main.m3"
 /* load */
#line 64 "../Main.m3"
 /* add */
#line 64 "../Main.m3"
 /* store */
#line 64 "../Main.m3"
(*(INT64*)(&offset_base_L_49))=(INT64)( ((INT64)(  INT64_(1)+ offset_base_L_49)));
#line 64 "../Main.m3"
 /* set_label */
#line 64 "../Main.m3"
 /* load_integer */
#line 64 "../Main.m3"
 /* load */
#line 64 "../Main.m3"
 /* if_compare */
#line 64 "../Main.m3"
if(m3_ge(INT64,
   INT64_(2),
  offset_base_L_49))goto LE;
#line 64 "../Main.m3"
 /* set_label */
#line 64 "../Main.m3"
 /* end_block */
#line 64 "../Main.m3"
 /* set_label */
#line 64 "../Main.m3"
LD:;
#line 64 "../Main.m3"
 /* load_address */
#line 64 "../Main.m3"
 /* exit_proc */
#line 64 "../Main.m3"
return (RT0__ModulePtr)(&Main_m_M_Main_L_13);
#line 64 "../Main.m3"
 /* end_procedure */
#line 64 "../Main.m3"
} /* global constant type descriptor */
#line 64 "../Main.m3"
 /* global data type descriptor */
#line 64 "../Main.m3"
 /* module global constants */
#line 64 "../Main.m3"
 /* Contents of constant Main.offset_bases */
#line 64 "../Main.m3"
 /* procedure names */
#line 64 "../Main.m3"
 /* procedure table */
#line 64 "../Main.m3"
 /* file name */
#line 64 "../Main.m3"
 /* module global data */
#line 64 "../Main.m3"
 /* Address of constant Main.offset_bases */
#line 64 "../Main.m3"
 /* load map


 global data allocation for M_Main
     0   104  8  *module info*
   104     8  8  constantMain.offset_bases_ADDR_
   112    24  8  import Main
   136    24  8  import Dump
   160    24  8  import RTHooks
   184     0  8  *TOTAL*


 global constants for M_Main
     0    16  8  constant Main.offset_bases
    16    24  8  StaticOpenArrayElements
    40    24  8  *proc names*
    64    88  8  *proc info*
   152    11  1  *string*
   168     0  8  *TOTAL*
 */
#line 64 "../Main.m3"
 /* end unit */
#line 64 "../Main.m3"

#ifdef __cplusplus

} /* extern "C" */
#endif
 /* set_runtime_proc */
 /* set_runtime_proc */
 /* set_runtime_proc */
