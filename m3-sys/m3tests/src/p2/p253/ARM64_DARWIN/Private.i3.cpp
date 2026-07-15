// library:pgm
// source_base_name:Private
// target_name:Private.i3.cpp
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
 /* declare_object */
 /* declare_method */
 /* declare_field */
 /* record_forwardDeclare Record_t{ typeid:TFFFFFFFF text:TE6E69551_fields hash_text:NIL base_text:NIL state:0} */
/*record_forwardDeclare*/struct TE6E69551_fields;typedef struct TE6E69551_fields TE6E69551_fields;
 /* record_canBeDefined Record_t{ typeid:TFFFFFFFF text:TE6E69551_fields hash_text:NIL base_text:NIL state:0} */
 /* record_define Record_t{ typeid:TFFFFFFFF text:TE6E69551_fields hash_text:NIL base_text:NIL state:0} */

#ifndef TE6E69551_fields
#define TE6E69551_fields TE6E69551_fields
/*record_define*/struct TE6E69551_fields{
INTEGER b;
};
#endif
 /* declare_opaque */

#ifndef TF32BE1BE
#define TF32BE1BE TF32BE1BE
/*1addressType_define*/typedef ADDRESS TF32BE1BE;

#endif
 /* declare_proctype */
 /* declare_formal */

#ifndef Public__Private
#define Public__Private Public__Private
typedef TF32BE1BE Public__Private;
#endif
 /* declare_record */
 /* declare_record */
 /* DeclareTypes_FlushOnce size:2 */
typedef TE6E69551_fields*TE6E69551;

#if 0 /* avoid type hash collions */
typedef 
void(__cdecl*TCAEE76CE)(Public__Private);
#else
typedef void (__cdecl*TCAEE76CE)(void);
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
/*Proc_ForwardDeclareFrameType*/struct Public_I3_Frame_t;typedef struct Public_I3_Frame_t Public_I3_Frame_t;
 /* internal_declare_param */
RT0__ModulePtr
__cdecl
Public_I3(
   /* Param_Type1 */ INTEGER /*TypeText1*/  mode_L_0);
 /* import_procedure */
/*Proc_ForwardDeclareFrameType*/struct RTHooks_I3_Frame_t;typedef struct RTHooks_I3_Frame_t RTHooks_I3_Frame_t;
 /* internal_declare_param */
RT0__ModulePtr
__cdecl
RTHooks_I3(
   /* Param_Type1 */ INTEGER /*TypeText1*/  mode_L_1);
 /* import_procedure */
/*Proc_ForwardDeclareFrameType*/struct Private_M3_Frame_t;typedef struct Private_M3_Frame_t Private_M3_Frame_t;
 /* internal_declare_param */
RT0__ModulePtr
__cdecl
Private_M3(
   /* Param_Type1 */ INTEGER /*TypeText1*/  mode_L_2);
 /* import_procedure */
/*Proc_ForwardDeclareFrameType*/struct Private__F2_Frame_t;typedef struct Private__F2_Frame_t Private__F2_Frame_t;
 /* internal_declare_param */
void /*TypeText3*/ 
__cdecl
Private__F2(
   /* Param_Type1 */ Public__Private a_L_3);
 /* end: imports */
 /* begin: locals */
 /* declare_segment name:<NIL> typeid:TFFFFFFFF const:TRUE */
/*declare_segment*/struct Private_i_4_L_5_t;
/*declare_segment*/typedef struct Private_i_4_L_5_t Private_i_4_L_5_t;
 /* declare_segment name:I_Private typeid:TFFFFFFFF const:FALSE */
 /* handler_name_prefixes:Private_M3_LINE_ */
 /* handler_name_prefixes:Private_I3_LINE_ */
/*declare_segment*/struct Private_i_I_Private_L_6_t;
/*declare_segment*/typedef struct Private_i_I_Private_L_6_t Private_i_I_Private_L_6_t;
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct Private_I3_Frame_t;typedef struct Private_I3_Frame_t Private_I3_Frame_t;
 /* internal_declare_param */
RT0__ModulePtr
__cdecl
Private_I3(
   /* Param_Type1 */ INTEGER /*TypeText1*/  mode_L_7);
 /* Locals_begin_procedure */
 /* begin_block */
 /* Locals_end_procedure */
 /* end_block */
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct Private_I3_te6e69551_INIT_Frame_t;typedef struct Private_I3_te6e69551_INIT_Frame_t Private_I3_te6e69551_INIT_Frame_t;
 /* internal_declare_param */
void /*TypeText3*/ 
__cdecl
Private_I3_te6e69551_INIT(
   /* Param_Type1 */ TE6E69551_fields* /*TypeText1*/  Private_i_8_L_9);
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
 /* init_proc */
 /* init_var */
 /* init_chars */
 /* init_int */
 /* init_chars */
 /* init_int */
 /* init_int */
 /* init_int */
 /* init_int */
 /* init_int */
 /* init_int */
 /* init_proc */
 /* init_chars */
 /* end_init */
struct Private_i_4_L_5_t{INT64 L_12[2];
char L_13[8];
UINT8 L_14[10];
char L_15[6];
ADDRESS L_16[2];
char L_17[8];
UINT8 L_18[13];
char L_19[3];
INT64 L_20[1];
UINT8 L_21[14];
char L_22[1];
INT8 L_23[6];
char L_24[3];
ADDRESS L_25[1];
UINT8 L_26[14];
char L_27[10];
};
static  const Private_i_4_L_5_t Private_i_4_L_5={{INT64_(-215227970),INT64_(-421096111)},{0 /* 1 */ ,0 /* 2 */ ,0 /* 3 */ ,0 /* 4 */ ,0 /* 5 */ ,0 /* 6 */ ,0 /* 7 */ ,0 /* 8 */ ,},{'P','r','i','v','a','t','e','_','I','3'},{0 /* 1 */ ,0 /* 2 */ ,0 /* 3 */ ,0 /* 4 */ ,0 /* 5 */ ,0 /* 6 */ ,},{(ADDRESS)&Private_I3,24+(char*)&Private_i_4_L_5},{0 /* 1 */ ,0 /* 2 */ ,0 /* 3 */ ,0 /* 4 */ ,0 /* 5 */ ,0 /* 6 */ ,0 /* 7 */ ,0 /* 8 */ ,},{'.','.','/','P','r','i','v','a','t','e','.','i','3'},{0 /* 1 */ ,0 /* 2 */ ,0 /* 3 */ ,},{INT64_(14)},{'P','u','b','l','i','c','.','P','r','i','v','a','t','e'},{0 /* 1 */ ,},{((INT8)15),((INT8)0),((INT8)2),((INT8)12),((INT8)1),((INT8)7)},{0 /* 1 */ ,0 /* 2 */ ,0 /* 3 */ ,},{(ADDRESS)&Private__F2},{'P','u','b','l','i','c','.','P','r','i','v','a','t','e'},{0 /* 1 */ ,0 /* 2 */ ,0 /* 3 */ ,0 /* 4 */ ,0 /* 5 */ ,0 /* 6 */ ,0 /* 7 */ ,0 /* 8 */ ,0 /* 9 */ ,0 /* 10 */ ,}};
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
 /* init_proc */
 /* init_var */
 /* init_var */
 /* init_int */
 /* init_int */
 /* init_var */
 /* init_proc */
 /* init_var */
 /* init_proc */
 /* init_var */
 /* init_proc */
 /* end_init */
struct Private_i_I_Private_L_6_t{ADDRESS L_28[2];
char L_29[8];
ADDRESS L_30[1];
char L_31[8];
ADDRESS L_32[1];
char L_33[24];
ADDRESS L_34[1];
char L_35[8];
ADDRESS L_36[1];
INT64 L_37[1];
char L_38[8];
INT64 L_39[1];
UINT8 L_40[1];
INT8 L_41[1];
UINT8 L_42[4];
INT8 L_43[4];
char L_44[1];
INT8 L_45[1];
char L_46[4];
INT64 L_47[1];
ADDRESS L_48[1];
char L_49[8];
ADDRESS L_50[4];
char L_51[8];
INT64 L_52[1];
char L_53[24];
INT64 L_54[1];
ADDRESS L_55[1];
char L_56[16];
ADDRESS L_57[2];
char L_58[8];
ADDRESS L_59[2];
char L_60[8];
ADDRESS L_61[1];
char L_62[16];
};
static Private_i_I_Private_L_6_t Private_i_I_Private_L_6={{64+(char*)&Private_i_4_L_5,104+(char*)&Private_i_I_Private_L_6},{0 /* 1 */ ,0 /* 2 */ ,0 /* 3 */ ,0 /* 4 */ ,0 /* 5 */ ,0 /* 6 */ ,0 /* 7 */ ,0 /* 8 */ ,},{(char*)&Private_i_4_L_5},{0 /* 1 */ ,0 /* 2 */ ,0 /* 3 */ ,0 /* 4 */ ,0 /* 5 */ ,0 /* 6 */ ,0 /* 7 */ ,0 /* 8 */ ,},{40+(char*)&Private_i_4_L_5},{0 /* 1 */ ,0 /* 2 */ ,0 /* 3 */ ,0 /* 4 */ ,0 /* 5 */ ,0 /* 6 */ ,0 /* 7 */ ,0 /* 8 */ ,0 /* 9 */ ,0 /* 10 */ ,0 /* 11 */ ,0 /* 12 */ ,0 /* 13 */ ,0 /* 14 */ ,0 /* 15 */ ,0 /* 16 */ ,0 /* 17 */ ,0 /* 18 */ ,0 /* 19 */ ,0 /* 20 */ ,0 /* 21 */ ,0 /* 22 */ ,0 /* 23 */ ,0 /* 24 */ ,},{256+(char*)&Private_i_I_Private_L_6},{0 /* 1 */ ,0 /* 2 */ ,0 /* 3 */ ,0 /* 4 */ ,0 /* 5 */ ,0 /* 6 */ ,0 /* 7 */ ,0 /* 8 */ ,},{(ADDRESS)&Private_I3},{INT64_(3)},{0 /* 1 */ ,0 /* 2 */ ,0 /* 3 */ ,0 /* 4 */ ,0 /* 5 */ ,0 /* 6 */ ,0 /* 7 */ ,0 /* 8 */ ,},{INT64_(-421096111)},{159U},{((INT8)41)},{248U,249U,206U,188U},{((INT8)30),((INT8)31),((INT8)1),((INT8)2)
},{0 /* 1 */ ,},{((INT8)8)},{0 /* 1 */ ,0 /* 2 */ ,0 /* 3 */ ,0 /* 4 */ ,},{INT64_(8)},{103+(char*)&Private_i_4_L_5},{0 /* 1 */ ,0 /* 2 */ ,0 /* 3 */ ,0 /* 4 */ ,0 /* 5 */ ,0 /* 6 */ ,0 /* 7 */ ,0 /* 8 */ ,},{105+(char*)&Private_i_4_L_5,(ADDRESS)&Private_I3_te6e69551_INIT,80+(char*)&Private_i_4_L_5,120+(char*)&Private_i_4_L_5},{0 /* 1 */ ,0 /* 2 */ ,0 /* 3 */ ,0 /* 4 */ ,0 /* 5 */ ,0 /* 6 */ ,0 /* 7 */ ,0 /* 8 */ ,},{INT64_(-1651526519)},{0 /* 1 */ ,0 /* 2 */ ,0 /* 3 */ ,0 /* 4 */ ,0 /* 5 */ ,0 /* 6 */ ,0 /* 7 */ ,0 /* 8 */ ,0 /* 9 */ ,0 /* 10 */ ,0 /* 11 */ ,0 /* 12 */ ,0 /* 13 */ ,0 /* 14 */ ,0 /* 15 */ ,0 /* 16 */ ,0 /* 17 */ ,0 /* 18 */ ,0 /* 19 */ ,0 /* 20 */ ,0 /* 21 */ ,0 /* 22 */ ,0 /* 23 */ ,0 /* 24 */ ,},{INT64_(8)},{112+(char*)&Private_i_4_L_5},{0 /* 1 */ ,0 /* 2 */ ,0 /* 3 */ ,0 /* 4 */ ,0 /* 5 */ ,0 /* 6 */ ,0 /* 7 */ ,0 /* 8 */ ,0 /* 9 */ ,0 /* 10 */ ,0 /* 11 */ ,0 /* 12 */ ,0 /* 13 */ ,0 /* 14 */ ,0 /* 15 */ ,0 /* 16 */ ,},{(ADDRESS)&Public_I3,280+(char*)&Private_i_I_Private_L_6
},{0 /* 1 */ ,0 /* 2 */ ,0 /* 3 */ ,0 /* 4 */ ,0 /* 5 */ ,0 /* 6 */ ,0 /* 7 */ ,0 /* 8 */ ,},{(ADDRESS)&RTHooks_I3,304+(char*)&Private_i_I_Private_L_6},{0 /* 1 */ ,0 /* 2 */ ,0 /* 3 */ ,0 /* 4 */ ,0 /* 5 */ ,0 /* 6 */ ,0 /* 7 */ ,0 /* 8 */ ,},{(ADDRESS)&Private_M3},{0 /* 1 */ ,0 /* 2 */ ,0 /* 3 */ ,0 /* 4 */ ,0 /* 5 */ ,0 /* 6 */ ,0 /* 7 */ ,0 /* 8 */ ,0 /* 9 */ ,0 /* 10 */ ,0 /* 11 */ ,0 /* 12 */ ,0 /* 13 */ ,0 /* 14 */ ,0 /* 15 */ ,0 /* 16 */ ,}};
 /* end: segments/globals */
 /* begin: mark used */
 /* end: mark used */
 /* set_source_file */
 /* set_source_line */
#line 1 "../Private.i3"
 /* module global constants */
#line 1 "../Private.i3"
 /* module global data */
#line 1 "../Private.i3"
 /* set_source_line */
#line 1 "../Private.i3"
#line 12 "../Private.i3"
 /* Private_I3 */
#line 12 "../Private.i3"
 /* module main body Private_I3 */
#line 12 "../Private.i3"
 /* begin_procedure */
#line 12 "../Private.i3"
struct Private_I3_Frame_t {
#line 12 "../Private.i3"
ADDRESS _unused;
#line 12 "../Private.i3"
};
#line 12 "../Private.i3"
RT0__ModulePtr
__cdecl
Private_I3(
   /* Param_Type1 */ INTEGER /*TypeText1*/  mode_L_7)
{
#line 12 "../Private.i3"
Private_I3_Frame_t _frame;
#line 12 "../Private.i3"
_frame._unused=(ADDRESS)&_frame;
#line 12 "../Private.i3"
 /* load */
#line 12 "../Private.i3"
 /* if_true_or_false */
#line 12 "../Private.i3"
 /* load_host_integer */
#line 12 "../Private.i3"
 /* load_integer */
#line 12 "../Private.i3"
 /* if_compare */
#line 12 "../Private.i3"
if(m3_eq(INT64,
  mode_L_7,
   INT64_(0)))goto L1;
#line 12 "../Private.i3"
 /* set_label */
#line 12 "../Private.i3"
L1:;
#line 12 "../Private.i3"
 /* load_address */
#line 12 "../Private.i3"
 /* exit_proc */
#line 12 "../Private.i3"
return (RT0__ModulePtr)(&Private_i_I_Private_L_6);
#line 12 "../Private.i3"
 /* end_procedure */
#line 12 "../Private.i3"
} /* Private_I3_te6e69551_INIT (ObjectType) */
#line 12 "../Private.i3"
 /* set_source_line */
#line 12 "../Private.i3"
#line 4 "../Private.i3"
 /* begin_procedure */
#line 4 "../Private.i3"
struct Private_I3_te6e69551_INIT_Frame_t {
#line 4 "../Private.i3"
ADDRESS _unused;
#line 4 "../Private.i3"
};
#line 4 "../Private.i3"
void /*TypeText3*/ 
__cdecl
Private_I3_te6e69551_INIT(
   /* Param_Type1 */ TE6E69551_fields* /*TypeText1*/  Private_i_8_L_9)
{
#line 4 "../Private.i3"
 /* Var_Type2 */ ADDRESS Private_i_10_L_11={0};//always-init
#line 4 "../Private.i3"
Private_I3_te6e69551_INIT_Frame_t _frame;
#line 4 "../Private.i3"
_frame._unused=(ADDRESS)&_frame;
#line 4 "../Private.i3"
 /* load */
#line 4 "../Private.i3"
 /* store */
#line 4 "../Private.i3"
(*(ADDRESS*)(&Private_i_10_L_11))=(ADDRESS)(((ADDRESS)(Private_i_8_L_9)));
#line 4 "../Private.i3"
 /* load */
#line 4 "../Private.i3"
 /* load_integer */
#line 4 "../Private.i3"
 /* store_indirect */
#line 4 "../Private.i3"
(*(INT64*)((8)+(char*)(Private_i_10_L_11)))=(INT64)(  INT64_(2));
#line 4 "../Private.i3"
 /* exit_proc */
#line 4 "../Private.i3"
return;
#line 4 "../Private.i3"
 /* end_procedure */
#line 4 "../Private.i3"
} /* global constant type descriptor */
#line 4 "../Private.i3"
 /* global data type descriptor */
#line 4 "../Private.i3"
 /* module global constants */
#line 4 "../Private.i3"
 /* procedure names */
#line 4 "../Private.i3"
 /* procedure table */
#line 4 "../Private.i3"
 /* file name */
#line 4 "../Private.i3"
 /* type map for _te6e69551 */
#line 4 "../Private.i3"
 /* type description for _te6e69551 */
#line 4 "../Private.i3"
 /* module global data */
#line 4 "../Private.i3"
 /* typecell for _te6e69551 */
#line 4 "../Private.i3"
 /* load map


 global data allocation for I_Private
     0   104  8  *module info*
   104   152  8  typecell
   256    24  8  import Public
   280    24  8  import RTHooks
   304    24  8  import Private
   328     0  8  *TOTAL*


 global constants for I_Private
     0    24  8  revelations
    24    11  8  *proc names*
    40    24  8  *proc info*
    64    14  1  *string*
    80    23  8  brand
   103     2  1  type_map
   105     4  1  type_desc
   112     8  8  method list
   120    15  1  *string*
   136     0  8  *TOTAL*
 */
#line 4 "../Private.i3"
 /* end unit */
#line 4 "../Private.i3"

#ifdef __cplusplus

} /* extern "C" */
#endif
 /* set_runtime_proc */
 /* set_runtime_proc */
 /* set_runtime_proc */
