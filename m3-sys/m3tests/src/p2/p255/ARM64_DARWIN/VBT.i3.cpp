// library:pgm
// source_base_name:VBT
// target_name:VBT.i3.cpp
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
 /* declare_pointer */
typedef TFCD63CAE*TC87182A6;
 /* declare_indirect */
typedef TFCD63CAE*T329C351;
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
 /* declare_pointer */
typedef T5CCFFB05*T7F433FF9;
 /* declare_indirect */
typedef T5CCFFB05*TA33004FA;
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
 /* declare_pointer */
typedef TE99B66B4*TB0595BE;
 /* declare_indirect */
typedef TE99B66B4*T1664994B;
 /* declare_subrange */
/*subrange_define*/typedef UINT8 T8A2831D7_8;
 /* declare_array */
/*array_forwardDeclare*/struct T4F238AAE;typedef struct T4F238AAE T4F238AAE;

#ifndef T4F238AAE
#define T4F238AAE T4F238AAE
/*fixedArray_define*/struct T4F238AAE{INTEGER _elts[1];};
#endif
 /* declare_pointer */
typedef T4F238AAE*TB628F43C;
 /* declare_indirect */
typedef T4F238AAE*TB0DC7551;
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
 /* declare_subrange */
/*subrange_define*/typedef UINT16 T4A3373B8_16;
 /* declare_set */
 /* declare_array */
/*array_forwardDeclare*/struct T67A7B112;typedef struct T67A7B112 T67A7B112;

#ifndef T67A7B112
#define T67A7B112 T67A7B112
/*fixedArray_define*/struct T67A7B112{WORD_T _elts[17];};
#endif
 /* declare_pointer */
typedef T67A7B112*T4797DFDB;
 /* declare_indirect */
typedef T67A7B112*T98584EED;
 /* declare_subrange */
/*subrange_define*/typedef UINT8 T2CA4581D_8;
 /* declare_set */

#ifndef TDDB62BB7
#define TDDB62BB7 TDDB62BB7
/*type_typedef*/typedef UINT16 TDDB62BB7;

#endif
 /* declare_pointer */
typedef TDDB62BB7*TED1CF615;
 /* declare_indirect */
typedef TDDB62BB7*T2249D448;
 /* declare_proctype */
 /* declare_formal */
 /* declare_formal */
 /* declare_formal */
 /* declare_formal */
 /* declare_formal */
 /* declare_formal */
 /* declare_formal */
 /* declare_formal */
 /* declare_formal */
 /* declare_formal */
 /* declare_formal */
 /* declare_formal */
 /* declare_formal */
 /* declare_formal */
 /* declare_object */
 /* record_forwardDeclare Record_t{ typeid:TFFFFFFFF text:T341F663F_fields hash_text:NIL base_text:NIL state:0} */
/*record_forwardDeclare*/struct T341F663F_fields;typedef struct T341F663F_fields T341F663F_fields;
 /* record_canBeDefined Record_t{ typeid:TFFFFFFFF text:T341F663F_fields hash_text:NIL base_text:NIL state:0} */
 /* record_define Record_t{ typeid:TFFFFFFFF text:T341F663F_fields hash_text:NIL base_text:NIL state:0} */

#ifndef T341F663F_fields
#define T341F663F_fields T341F663F_fields
/*record_define*/struct T341F663F_fields{
UINT8 L_0[8];
};
#endif
typedef T341F663F_fields*T341F663F;
 /* declare_method */
 /* declare_record */
 /* declare_field */
 /* declare_field */
 /* declare_field */
 /* declare_field */
 /* declare_field */
 /* declare_field */
 /* declare_record */
 /* declare_field */
 /* declare_field */
 /* declare_field */
 /* declare_field */
 /* declare_field */
 /* declare_field */
 /* DeclareTypes_FlushOnce size:1 */

#if 0 /* avoid type hash collions */
typedef 
void(__cdecl*T87ECE3F8)(TFCD63CAE,TFCD63CAE*,T5CCFFB05,T5CCFFB05*,TE99B66B4,TE99B66B4*,T4F238AAE,T4F238AAE*,TF400F3DB*,TF400F3DB*,T67A7B112,T67A7B112*,TDDB62BB7,TDDB62BB7*);
#else
typedef void (__cdecl*T87ECE3F8)(void);
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
   /* Param_Type1 */ INTEGER /*TypeText1*/  mode_L_1);
 /* end: imports */
 /* begin: locals */
 /* declare_segment name:<NIL> typeid:TFFFFFFFF const:TRUE */
/*declare_segment*/struct VBT_i_2_L_3_t;
/*declare_segment*/typedef struct VBT_i_2_L_3_t VBT_i_2_L_3_t;
 /* declare_segment name:I_VBT typeid:TFFFFFFFF const:FALSE */
 /* handler_name_prefixes:VBT_M3_LINE_ */
 /* handler_name_prefixes:VBT_I3_LINE_ */
/*declare_segment*/struct VBT_i_I_VBT_L_4_t;
/*declare_segment*/typedef struct VBT_i_I_VBT_L_4_t VBT_i_I_VBT_L_4_t;
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct VBT_I3_Frame_t;typedef struct VBT_I3_Frame_t VBT_I3_Frame_t;
 /* internal_declare_param */
RT0__ModulePtr
__cdecl
VBT_I3(
   /* Param_Type1 */ INTEGER /*TypeText1*/  mode_L_5);
 /* Locals_begin_procedure */
 /* begin_block */
 /* Locals_end_procedure */
 /* end_block */
 /* end: locals */
 /* begin: segments/globals */
 /* bind_segment */
 /* begin_init */
 /* init_int */
 /* init_int */
 /* init_var */
 /* init_int */
 /* init_int */
 /* init_int */
 /* init_int */
 /* init_int */
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
 /* init_int */
 /* init_int */
 /* init_int */
 /* init_int */
 /* init_int */
 /* init_int */
 /* init_chars */
 /* end_init */
struct VBT_i_2_L_3_t{INT64 L_6[1];
char L_7[128];
INT64 L_8[1];
ADDRESS L_9[1];
INT64 L_10[5];
UINT8 L_11[6];
char L_12[2];
ADDRESS L_13[2];
char L_14[8];
UINT8 L_15[9];
char L_16[1];
INT8 L_17[7];
UINT8 L_18[1];
INT8 L_19[36];
UINT8 L_20[9];
char L_21[1];
};
static  const VBT_i_2_L_3_t VBT_i_2_L_3={{INT64_(2)},{0 /* 1 */ ,0 /* 2 */ ,0 /* 3 */ ,0 /* 4 */ ,0 /* 5 */ ,0 /* 6 */ ,0 /* 7 */ ,0 /* 8 */ ,0 /* 9 */ ,0 /* 10 */ ,0 /* 11 */ ,0 /* 12 */ ,0 /* 13 */ ,0 /* 14 */ ,0 /* 15 */ ,0 /* 16 */ ,0 /* 17 */ ,0 /* 18 */ ,0 /* 19 */ ,0 /* 20 */ ,0 /* 21 */ ,0 /* 22 */ ,0 /* 23 */ ,0 /* 24 */ ,0 /* 25 */ ,0 /* 26 */ ,0 /* 27 */ ,0 /* 28 */ ,0 /* 29 */ ,0 /* 30 */ ,0 /* 31 */ ,0 /* 32 */ ,0 /* 33 */ ,0 /* 34 */ ,0 /* 35 */ ,0 /* 36 */ ,0 /* 37 */ ,0 /* 38 */ ,0 /* 39 */ ,0 /* 40 */ ,0 /* 41 */ ,0 /* 42 */ ,0 /* 43 */ ,0 /* 44 */ ,0 /* 45 */ ,0 /* 46 */ ,0 /* 47 */ ,0 /* 48 */ ,0 /* 49 */ ,0 /* 50 */ ,0 /* 51 */ ,0 /* 52 */ ,0 /* 53 */ ,0 /* 54 */ ,0 /* 55 */ ,0 /* 56 */ ,0 /* 57 */ ,0 /* 58 */ ,0 /* 59 */ ,0 /* 60 */ ,0 /* 61 */ ,0 /* 62 */ ,0 /* 63 */ ,0 /* 64 */ ,0 /* 65 */ ,0 /* 66 */ ,0 /* 67 */ ,0 /* 68 */ ,0 /* 69 */ ,0 /* 70 */ ,0 /* 71 */ ,0 /* 72 */ ,0 /* 73 */ ,0 /* 74 */ ,0 /* 75 */ ,0 /* 76 */ ,0 /* 77 */ ,0 /* 78 */ ,0 /* 79 */ ,0 /* 80 */ 
,0 /* 81 */ ,0 /* 82 */ ,0 /* 83 */ ,0 /* 84 */ ,0 /* 85 */ ,0 /* 86 */ ,0 /* 87 */ ,0 /* 88 */ ,0 /* 89 */ ,0 /* 90 */ ,0 /* 91 */ ,0 /* 92 */ ,0 /* 93 */ ,0 /* 94 */ ,0 /* 95 */ ,0 /* 96 */ ,0 /* 97 */ ,0 /* 98 */ ,0 /* 99 */ ,0 /* 100 */ ,0 /* 101 */ ,0 /* 102 */ ,0 /* 103 */ ,0 /* 104 */ ,0 /* 105 */ ,0 /* 106 */ ,0 /* 107 */ ,0 /* 108 */ ,0 /* 109 */ ,0 /* 110 */ ,0 /* 111 */ ,0 /* 112 */ ,0 /* 113 */ ,0 /* 114 */ ,0 /* 115 */ ,0 /* 116 */ ,0 /* 117 */ ,0 /* 118 */ ,0 /* 119 */ ,0 /* 120 */ ,0 /* 121 */ ,0 /* 122 */ ,0 /* 123 */ ,0 /* 124 */ ,0 /* 125 */ ,0 /* 126 */ ,0 /* 127 */ ,0 /* 128 */ ,},{INT64_(1)},{184+(char*)&VBT_i_2_L_3},{INT64_(1),INT64_(1),INT64_(2),INT64_(3),INT64_(1)},{'V','B','T','_','I','3'},{0 /* 1 */ ,0 /* 2 */ ,},{(ADDRESS)&VBT_I3,192+(char*)&VBT_i_2_L_3},{0 /* 1 */ ,0 /* 2 */ ,0 /* 3 */ ,0 /* 4 */ ,0 /* 5 */ ,0 /* 6 */ ,0 /* 7 */ ,0 /* 8 */ ,},{'.','.','/','V','B','T','.','i','3'},{0 /* 1 */ ,},{((INT8)20),((INT8)2),((INT8)0),((INT8)1),((INT8)20),((INT8)11),((INT8)20)
},{136U},{((INT8)0),((INT8)1),((INT8)20),((INT8)66),((INT8)1),((INT8)4),((INT8)1),((INT8)15),((INT8)26),((INT8)1),((INT8)0),((INT8)2),((INT8)1),((INT8)1),((INT8)7),((INT8)15),((INT8)0),((INT8)2),((INT8)17),((INT8)1),((INT8)7),((INT8)15),((INT8)0),((INT8)2),((INT8)17),((INT8)1),((INT8)7),((INT8)15),((INT8)0),((INT8)2),((INT8)17),((INT8)1),((INT8)7),((INT8)1),((INT8)12),((INT8)0)},{'V','B','T','.','V','B','T','_','T'},{0 /* 1 */ ,}};
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
 /* init_var */
 /* init_int */
 /* init_int */
 /* init_var */
 /* init_var */
 /* init_var */
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
 /* init_var */
 /* init_var */
 /* init_proc */
 /* end_init */
struct VBT_i_I_VBT_L_4_t{ADDRESS L_22[2];
char L_23[24];
ADDRESS L_24[1];
char L_25[24];
ADDRESS L_26[1];
char L_27[8];
ADDRESS L_28[1];
INT64 L_29[1];
char L_30[8];
INT64 L_31[1];
UINT8 L_32[2];
INT8 L_33[2];
UINT8 L_34[2];
INT8 L_35[4];
char L_36[1];
INT8 L_37[1];
char L_38[4];
INT64 L_39[1];
char L_40[16];
ADDRESS L_41[1];
char L_42[16];
ADDRESS L_43[2];
INT64 L_44[1];
char L_45[24];
INT64 L_46[1];
char L_47[16];
ADDRESS L_48[6];
char L_49[8];
INT64 L_50[1];
UINT8 L_51[2];
INT8 L_52[1];
UINT8 L_53[1];
INT8 L_54[6];
char L_55[1];
INT8 L_56[1];
char L_57[4];
INT64 L_58[1];
ADDRESS L_59[1];
char L_60[8];
ADDRESS L_61[1];
char L_62[24];
ADDRESS L_63[1];
char L_64[8];
INT64 L_65[1];
UINT8 L_66[1];
INT8 L_67[9];
char L_68[1];
INT8 L_69[1];
char L_70[4];
INT64 L_71[1];
ADDRESS L_72[1];
char L_73[8];
ADDRESS L_74[1];
char L_75[24];
ADDRESS L_76[1];
char L_77[8];
INT64 L_78[1];
INT8 L_79[1];
UINT8 L_80[2];
INT8 L_81[1];
UINT8 L_82[1];
INT8 L_83[1];
UINT8 L_84[1];
INT8 L_85[3];
char L_86[1];
INT8 L_87[1];
char L_88[4];
INT64 L_89[1];
ADDRESS L_90[1];
char L_91[8];
ADDRESS L_92[1];
char L_93[24];
ADDRESS L_94[1];
char L_95[8];
INT64 L_96[1];
INT8 L_97[3];
UINT8 L_98[1];
INT8 L_99[1];
UINT8 L_100[1];
INT8 L_101[4];
char L_102[1];
INT8 L_103[1];
char L_104[4];
INT64 L_105[1];
ADDRESS L_106[1];
char L_107[8];
ADDRESS L_108[1];
char L_109[24];
ADDRESS L_110[1];
char L_111[8];
INT64 L_112[1];
UINT8 L_113[1];
INT8 L_114[1];
UINT8 L_115[1];
INT8 L_116[2];
UINT8 L_117[1];
INT8 L_118[4];
char L_119[1];
INT8 L_120[1];
char L_121[4];
INT64 L_122[1];
ADDRESS L_123[1];
char L_124[8];
ADDRESS L_125[1];
char L_126[24];
ADDRESS L_127[1];
char L_128[8];
INT64 L_129[1];
UINT8 L_130[1];
INT8 L_131[2];
UINT8 L_132[3];
INT8 L_133[4];
char L_134[1];
INT8 L_135[1];
char L_136[4];
INT64 L_137[1];
ADDRESS L_138[1];
char L_139[8];
ADDRESS L_140[1];
char L_141[40];
ADDRESS L_142[1];
char L_143[16];
};
static VBT_i_I_VBT_L_4_t VBT_i_I_VBT_L_4={{224+(char*)&VBT_i_2_L_3,104+(char*)&VBT_i_I_VBT_L_4},{0 /* 1 */ ,0 /* 2 */ ,0 /* 3 */ ,0 /* 4 */ ,0 /* 5 */ ,0 /* 6 */ ,0 /* 7 */ ,0 /* 8 */ ,0 /* 9 */ ,0 /* 10 */ ,0 /* 11 */ ,0 /* 12 */ ,0 /* 13 */ ,0 /* 14 */ ,0 /* 15 */ ,0 /* 16 */ ,0 /* 17 */ ,0 /* 18 */ ,0 /* 19 */ ,0 /* 20 */ ,0 /* 21 */ ,0 /* 22 */ ,0 /* 23 */ ,0 /* 24 */ ,},{200+(char*)&VBT_i_2_L_3},{0 /* 1 */ ,0 /* 2 */ ,0 /* 3 */ ,0 /* 4 */ ,0 /* 5 */ ,0 /* 6 */ ,0 /* 7 */ ,0 /* 8 */ ,0 /* 9 */ ,0 /* 10 */ ,0 /* 11 */ ,0 /* 12 */ ,0 /* 13 */ ,0 /* 14 */ ,0 /* 15 */ ,0 /* 16 */ ,0 /* 17 */ ,0 /* 18 */ ,0 /* 19 */ ,0 /* 20 */ ,0 /* 21 */ ,0 /* 22 */ ,0 /* 23 */ ,0 /* 24 */ ,},{880+(char*)&VBT_i_I_VBT_L_4},{0 /* 1 */ ,0 /* 2 */ ,0 /* 3 */ ,0 /* 4 */ ,0 /* 5 */ ,0 /* 6 */ ,0 /* 7 */ ,0 /* 8 */ ,},{(ADDRESS)&VBT_I3},{INT64_(3)},{0 /* 1 */ ,0 /* 2 */ ,0 /* 3 */ ,0 /* 4 */ ,0 /* 5 */ ,0 /* 6 */ ,0 /* 7 */ ,0 /* 8 */ ,},{INT64_(874473023)},{143U,242U},{((INT8)41),((INT8)50)},{176U,148U},{((INT8)54)
,((INT8)6),((INT8)1),((INT8)2)},{0 /* 1 */ ,},{((INT8)8)},{0 /* 1 */ ,0 /* 2 */ ,0 /* 3 */ ,0 /* 4 */ ,},{INT64_(0)},{0 /* 1 */ ,0 /* 2 */ ,0 /* 3 */ ,0 /* 4 */ ,0 /* 5 */ ,0 /* 6 */ ,0 /* 7 */ ,0 /* 8 */ ,0 /* 9 */ ,0 /* 10 */ ,0 /* 11 */ ,0 /* 12 */ ,0 /* 13 */ ,0 /* 14 */ ,0 /* 15 */ ,0 /* 16 */ ,},{275+(char*)&VBT_i_2_L_3},{0 /* 1 */ ,0 /* 2 */ ,0 /* 3 */ ,0 /* 4 */ ,0 /* 5 */ ,0 /* 6 */ ,0 /* 7 */ ,0 /* 8 */ ,0 /* 9 */ ,0 /* 10 */ ,0 /* 11 */ ,0 /* 12 */ ,0 /* 13 */ ,0 /* 14 */ ,0 /* 15 */ ,0 /* 16 */ ,},{278+(char*)&VBT_i_2_L_3,304+(char*)&VBT_i_I_VBT_L_4},{INT64_(-1651526519)},{0 /* 1 */ ,0 /* 2 */ ,0 /* 3 */ ,0 /* 4 */ ,0 /* 5 */ ,0 /* 6 */ ,0 /* 7 */ ,0 /* 8 */ ,0 /* 9 */ ,0 /* 10 */ ,0 /* 11 */ ,0 /* 12 */ ,0 /* 13 */ ,0 /* 14 */ ,0 /* 15 */ ,0 /* 16 */ ,0 /* 17 */ ,0 /* 18 */ ,0 /* 19 */ ,0 /* 20 */ ,0 /* 21 */ ,0 /* 22 */ ,0 /* 23 */ ,0 /* 24 */ ,},{INT64_(8)},{0 /* 1 */ ,0 /* 2 */ ,0 /* 3 */ ,0 /* 4 */ ,0 /* 5 */ ,0 /* 6 */ ,0 /* 7 */ ,0 /* 8 */ ,0 /* 9 */ ,0 /* 10 */ ,0 /* 11 */ 
,0 /* 12 */ ,0 /* 13 */ ,0 /* 14 */ ,0 /* 15 */ ,0 /* 16 */ ,},{(char*)&VBT_i_2_L_3,136+(char*)&VBT_i_2_L_3,144+(char*)&VBT_i_2_L_3,160+(char*)&VBT_i_2_L_3,168+(char*)&VBT_i_2_L_3,176+(char*)&VBT_i_2_L_3},{0 /* 1 */ ,0 /* 2 */ ,0 /* 3 */ ,0 /* 4 */ ,0 /* 5 */ ,0 /* 6 */ ,0 /* 7 */ ,0 /* 8 */ ,},{INT64_(-932085082)},{160U,187U},{((INT8)43)},{212U},{((INT8)6),((INT8)57),((INT8)90),((INT8)28),((INT8)1),((INT8)1)},{0 /* 1 */ ,},{((INT8)8)},{0 /* 1 */ ,0 /* 2 */ ,0 /* 3 */ ,0 /* 4 */ ,},{INT64_(8)},{269+(char*)&VBT_i_2_L_3},{0 /* 1 */ ,0 /* 2 */ ,0 /* 3 */ ,0 /* 4 */ ,0 /* 5 */ ,0 /* 6 */ ,0 /* 7 */ ,0 /* 8 */ ,},{271+(char*)&VBT_i_2_L_3},{0 /* 1 */ ,0 /* 2 */ ,0 /* 3 */ ,0 /* 4 */ ,0 /* 5 */ ,0 /* 6 */ ,0 /* 7 */ ,0 /* 8 */ ,0 /* 9 */ ,0 /* 10 */ ,0 /* 11 */ ,0 /* 12 */ ,0 /* 13 */ ,0 /* 14 */ ,0 /* 15 */ ,0 /* 16 */ ,0 /* 17 */ ,0 /* 18 */ ,0 /* 19 */ ,0 /* 20 */ ,0 /* 21 */ ,0 /* 22 */ ,0 /* 23 */ ,0 /* 24 */ ,},{400+(char*)&VBT_i_I_VBT_L_4},{0 /* 1 */ ,0 /* 2 */ ,0 /* 3 */ ,0 /* 4 */ ,0 /* 5 */ 
,0 /* 6 */ ,0 /* 7 */ ,0 /* 8 */ ,},{INT64_(2135113721)},{225U},{((INT8)127),((INT8)74),((INT8)111),((INT8)24),((INT8)64),((INT8)9),((INT8)16),((INT8)1),((INT8)1)},{0 /* 1 */ ,},{((INT8)8)},{0 /* 1 */ ,0 /* 2 */ ,0 /* 3 */ ,0 /* 4 */ ,},{INT64_(8)},{263+(char*)&VBT_i_2_L_3},{0 /* 1 */ ,0 /* 2 */ ,0 /* 3 */ ,0 /* 4 */ ,0 /* 5 */ ,0 /* 6 */ ,0 /* 7 */ ,0 /* 8 */ ,},{265+(char*)&VBT_i_2_L_3},{0 /* 1 */ ,0 /* 2 */ ,0 /* 3 */ ,0 /* 4 */ ,0 /* 5 */ ,0 /* 6 */ ,0 /* 7 */ ,0 /* 8 */ ,0 /* 9 */ ,0 /* 10 */ ,0 /* 11 */ ,0 /* 12 */ ,0 /* 13 */ ,0 /* 14 */ ,0 /* 15 */ ,0 /* 16 */ ,0 /* 17 */ ,0 /* 18 */ ,0 /* 19 */ ,0 /* 20 */ ,0 /* 21 */ ,0 /* 22 */ ,0 /* 23 */ ,0 /* 24 */ ,},{496+(char*)&VBT_i_I_VBT_L_4},{0 /* 1 */ ,0 /* 2 */ ,0 /* 3 */ ,0 /* 4 */ ,0 /* 5 */ ,0 /* 6 */ ,0 /* 7 */ ,0 /* 8 */ ,},{INT64_(184915390)},{((INT8)74)},{167U,145U},{((INT8)7)},{244U},{((INT8)50)},{148U},{((INT8)12),((INT8)1),((INT8)1)},{0 /* 1 */ ,},{((INT8)8)},{0 /* 1 */ ,0 /* 2 */ ,0 /* 3 */ ,0 /* 4 */ ,},{INT64_(8)},{257+(char*)&VBT_i_2_L_3
},{0 /* 1 */ ,0 /* 2 */ ,0 /* 3 */ ,0 /* 4 */ ,0 /* 5 */ ,0 /* 6 */ ,0 /* 7 */ ,0 /* 8 */ ,},{259+(char*)&VBT_i_2_L_3},{0 /* 1 */ ,0 /* 2 */ ,0 /* 3 */ ,0 /* 4 */ ,0 /* 5 */ ,0 /* 6 */ ,0 /* 7 */ ,0 /* 8 */ ,0 /* 9 */ ,0 /* 10 */ ,0 /* 11 */ ,0 /* 12 */ ,0 /* 13 */ ,0 /* 14 */ ,0 /* 15 */ ,0 /* 16 */ ,0 /* 17 */ ,0 /* 18 */ ,0 /* 19 */ ,0 /* 20 */ ,0 /* 21 */ ,0 /* 22 */ ,0 /* 23 */ ,0 /* 24 */ ,},{592+(char*)&VBT_i_I_VBT_L_4},{0 /* 1 */ ,0 /* 2 */ ,0 /* 3 */ ,0 /* 4 */ ,0 /* 5 */ ,0 /* 6 */ ,0 /* 7 */ ,0 /* 8 */ ,},{INT64_(-1238830020)},{((INT8)39),((INT8)64),((INT8)10)},{173U},{((INT8)27)},{180U},{((INT8)34),((INT8)27),((INT8)1),((INT8)1)},{0 /* 1 */ ,},{((INT8)8)},{0 /* 1 */ ,0 /* 2 */ ,0 /* 3 */ ,0 /* 4 */ ,},{INT64_(8)},{248+(char*)&VBT_i_2_L_3},{0 /* 1 */ ,0 /* 2 */ ,0 /* 3 */ ,0 /* 4 */ ,0 /* 5 */ ,0 /* 6 */ ,0 /* 7 */ ,0 /* 8 */ ,},{253+(char*)&VBT_i_2_L_3},{0 /* 1 */ ,0 /* 2 */ ,0 /* 3 */ ,0 /* 4 */ ,0 /* 5 */ ,0 /* 6 */ ,0 /* 7 */ ,0 /* 8 */ ,0 /* 9 */ ,0 /* 10 */ ,0 /* 11 */ 
,0 /* 12 */ ,0 /* 13 */ ,0 /* 14 */ ,0 /* 15 */ ,0 /* 16 */ ,0 /* 17 */ ,0 /* 18 */ ,0 /* 19 */ ,0 /* 20 */ ,0 /* 21 */ ,0 /* 22 */ ,0 /* 23 */ ,0 /* 24 */ ,},{688+(char*)&VBT_i_I_VBT_L_4},{0 /* 1 */ ,0 /* 2 */ ,0 /* 3 */ ,0 /* 4 */ ,0 /* 5 */ ,0 /* 6 */ ,0 /* 7 */ ,0 /* 8 */ ,},{INT64_(1201135579)},{219U},{((INT8)78)},{254U},{((INT8)89),((INT8)0)},{145U},{((INT8)105),((INT8)30),((INT8)1),((INT8)1)},{0 /* 1 */ ,},{((INT8)8)},{0 /* 1 */ ,0 /* 2 */ ,0 /* 3 */ ,0 /* 4 */ ,},{INT64_(136)},{240+(char*)&VBT_i_2_L_3},{0 /* 1 */ ,0 /* 2 */ ,0 /* 3 */ ,0 /* 4 */ ,0 /* 5 */ ,0 /* 6 */ ,0 /* 7 */ ,0 /* 8 */ ,},{243+(char*)&VBT_i_2_L_3},{0 /* 1 */ ,0 /* 2 */ ,0 /* 3 */ ,0 /* 4 */ ,0 /* 5 */ ,0 /* 6 */ ,0 /* 7 */ ,0 /* 8 */ ,0 /* 9 */ ,0 /* 10 */ ,0 /* 11 */ ,0 /* 12 */ ,0 /* 13 */ ,0 /* 14 */ ,0 /* 15 */ ,0 /* 16 */ ,0 /* 17 */ ,0 /* 18 */ ,0 /* 19 */ ,0 /* 20 */ ,0 /* 21 */ ,0 /* 22 */ ,0 /* 23 */ ,0 /* 24 */ ,},{784+(char*)&VBT_i_I_VBT_L_4},{0 /* 1 */ ,0 /* 2 */ ,0 /* 3 */ ,0 /* 4 */ ,0 /* 5 */ 
,0 /* 6 */ ,0 /* 7 */ ,0 /* 8 */ ,},{INT64_(-316869099)},{149U},{((INT8)119),((INT8)4)},{252U,128U,129U},{((INT8)24),((INT8)17),((INT8)1),((INT8)1)},{0 /* 1 */ ,},{((INT8)2)},{0 /* 1 */ ,0 /* 2 */ ,0 /* 3 */ ,0 /* 4 */ ,},{INT64_(2)},{234+(char*)&VBT_i_2_L_3},{0 /* 1 */ ,0 /* 2 */ ,0 /* 3 */ ,0 /* 4 */ ,0 /* 5 */ ,0 /* 6 */ ,0 /* 7 */ ,0 /* 8 */ ,},{237+(char*)&VBT_i_2_L_3},{0 /* 1 */ ,0 /* 2 */ ,0 /* 3 */ ,0 /* 4 */ ,0 /* 5 */ ,0 /* 6 */ ,0 /* 7 */ ,0 /* 8 */ ,0 /* 9 */ ,0 /* 10 */ ,0 /* 11 */ ,0 /* 12 */ ,0 /* 13 */ ,0 /* 14 */ ,0 /* 15 */ ,0 /* 16 */ ,0 /* 17 */ ,0 /* 18 */ ,0 /* 19 */ ,0 /* 20 */ ,0 /* 21 */ ,0 /* 22 */ ,0 /* 23 */ ,0 /* 24 */ ,0 /* 25 */ ,0 /* 26 */ ,0 /* 27 */ ,0 /* 28 */ ,0 /* 29 */ ,0 /* 30 */ ,0 /* 31 */ ,0 /* 32 */ ,0 /* 33 */ ,0 /* 34 */ ,0 /* 35 */ ,0 /* 36 */ ,0 /* 37 */ ,0 /* 38 */ ,0 /* 39 */ ,0 /* 40 */ ,},{(ADDRESS)&RTHooks_I3},{0 /* 1 */ ,0 /* 2 */ ,0 /* 3 */ ,0 /* 4 */ ,0 /* 5 */ ,0 /* 6 */ ,0 /* 7 */ ,0 /* 8 */ ,0 /* 9 */ ,0 /* 10 */ ,0 /* 11 */ ,0 /* 12 */ 
,0 /* 13 */ ,0 /* 14 */ ,0 /* 15 */ ,0 /* 16 */ ,}};
 /* end: segments/globals */
 /* begin: mark used */
 /* end: mark used */
 /* set_source_file */
 /* set_source_line */
#line 7 "../VBT.i3"
 /* module global constants */
#line 7 "../VBT.i3"
 /* module global data */
#line 7 "../VBT.i3"
 /* set_source_line */
#line 7 "../VBT.i3"
#line 49 "../VBT.i3"
 /* VBT_I3 */
#line 49 "../VBT.i3"
 /* module main body VBT_I3 */
#line 49 "../VBT.i3"
 /* begin_procedure */
#line 49 "../VBT.i3"
struct VBT_I3_Frame_t {
#line 49 "../VBT.i3"
ADDRESS _unused;
#line 49 "../VBT.i3"
};
#line 49 "../VBT.i3"
RT0__ModulePtr
__cdecl
VBT_I3(
   /* Param_Type1 */ INTEGER /*TypeText1*/  mode_L_5)
{
#line 49 "../VBT.i3"
VBT_I3_Frame_t _frame;
#line 49 "../VBT.i3"
_frame._unused=(ADDRESS)&_frame;
#line 49 "../VBT.i3"
 /* load */
#line 49 "../VBT.i3"
 /* if_true_or_false */
#line 49 "../VBT.i3"
 /* load_host_integer */
#line 49 "../VBT.i3"
 /* load_integer */
#line 49 "../VBT.i3"
 /* if_compare */
#line 49 "../VBT.i3"
if(m3_eq(INT64,
  mode_L_5,
   INT64_(0)))goto L1;
#line 49 "../VBT.i3"
 /* set_label */
#line 49 "../VBT.i3"
L1:;
#line 49 "../VBT.i3"
 /* load_address */
#line 49 "../VBT.i3"
 /* exit_proc */
#line 49 "../VBT.i3"
return (RT0__ModulePtr)(&VBT_i_I_VBT_L_4);
#line 49 "../VBT.i3"
 /* end_procedure */
#line 49 "../VBT.i3"
} /* global constant type descriptor */
#line 49 "../VBT.i3"
 /* global data type descriptor */
#line 49 "../VBT.i3"
 /* module global constants */
#line 49 "../VBT.i3"
 /* Contents of constant VBT.BigSet0 */
#line 49 "../VBT.i3"
 /* Contents of constant VBT.FixedArray0 */
#line 49 "../VBT.i3"
 /* Contents of constant VBT.OpenArray0 */
#line 49 "../VBT.i3"
 /* Contents of constant VBT.Pixmap_Gray */
#line 49 "../VBT.i3"
 /* Contents of constant VBT.PaintOp_Swap */
#line 49 "../VBT.i3"
 /* Contents of constant VBT.PaintOp_TransparentSwap */
#line 49 "../VBT.i3"
 /* procedure names */
#line 49 "../VBT.i3"
 /* procedure table */
#line 49 "../VBT.i3"
 /* file name */
#line 49 "../VBT.i3"
 /* type map for _ted1cf615 */
#line 49 "../VBT.i3"
 /* type description for _ted1cf615 */
#line 49 "../VBT.i3"
 /* type map for _t4797dfdb */
#line 49 "../VBT.i3"
 /* type description for _t4797dfdb */
#line 49 "../VBT.i3"
 /* type map for _tb628f43c */
#line 49 "../VBT.i3"
 /* type description for _tb628f43c */
#line 49 "../VBT.i3"
 /* type map for _t0b0595be */
#line 49 "../VBT.i3"
 /* type description for _t0b0595be */
#line 49 "../VBT.i3"
 /* type map for _t7f433ff9 */
#line 49 "../VBT.i3"
 /* type description for _t7f433ff9 */
#line 49 "../VBT.i3"
 /* type map for _tc87182a6 */
#line 49 "../VBT.i3"
 /* type description for _tc87182a6 */
#line 49 "../VBT.i3"
 /* type description for _t341f663f */
#line 49 "../VBT.i3"
 /* module global data */
#line 49 "../VBT.i3"
 /* typecell for _t341f663f */
#line 49 "../VBT.i3"
 /* Address of constant VBT.BigSet0 */
#line 49 "../VBT.i3"
 /* Address of constant VBT.FixedArray0 */
#line 49 "../VBT.i3"
 /* Address of constant VBT.OpenArray0 */
#line 49 "../VBT.i3"
 /* Address of constant VBT.Pixmap_Gray */
#line 49 "../VBT.i3"
 /* Address of constant VBT.PaintOp_Swap */
#line 49 "../VBT.i3"
 /* Address of constant VBT.PaintOp_TransparentSwap */
#line 49 "../VBT.i3"
 /* typecell for _tc87182a6 */
#line 49 "../VBT.i3"
 /* typecell for _t7f433ff9 */
#line 49 "../VBT.i3"
 /* typecell for _t0b0595be */
#line 49 "../VBT.i3"
 /* typecell for _tb628f43c */
#line 49 "../VBT.i3"
 /* typecell for _t4797dfdb */
#line 49 "../VBT.i3"
 /* typecell for _ted1cf615 */
#line 49 "../VBT.i3"
 /* load map


 global data allocation for I_VBT
     0   104  8  *module info*
   104   152  8  typecell
   256     8  8  constantVBT.BigSet0_ADDR_
   264     8  8  constantVBT.FixedArray0_ADDR_
   272     8  8  constantVBT.OpenArray0_ADDR_
   280     8  8  constantVBT.Pixmap_Gray_ADDR_
   288     8  8  constantVBT.PaintOp_Swap_ADDR_
   296     8  8  constantVBT.PaintOp_TransparentSwap_ADDR_
   304    96  8  typecell
   400    96  8  typecell
   496    96  8  typecell
   592    96  8  typecell
   688    96  8  typecell
   784    96  8  typecell
   880    24  8  import RTHooks
   904     0  8  *TOTAL*


 global constants for I_VBT
     0   136  8  constant VBT.BigSet0
   136     8  8  constant VBT.FixedArray0
   144    16  8  constant VBT.OpenArray0
   160     8  8  constant VBT.Pixmap_Gray
   168     8  8  constant VBT.PaintOp_Swap
   176     8  8  constant VBT.PaintOp_TransparentSwap
   184     8  8  StaticOpenArrayElements
   192     7  8  *proc names*
   200    24  8  *proc info*
   224    10  1  *string*
   234     3  1  type_map
   237     3  1  type_desc
   240     3  1  type_map
   243     5  1  type_desc
   248     5  1  type_map
   253     4  1  type_desc
   257     2  1  type_map
   259     4  1  type_desc
   263     2  1  type_map
   265     4  1  type_desc
   269     2  1  type_map
   271     4  1  type_desc
   275     3  1  type_desc
   278    10  1  *string*
   288     0  8  *TOTAL*
 */
#line 49 "../VBT.i3"
 /* end unit */
#line 49 "../VBT.i3"

#ifdef __cplusplus

} /* extern "C" */
#endif
 /* set_runtime_proc */
 /* set_runtime_proc */
 /* set_runtime_proc */
