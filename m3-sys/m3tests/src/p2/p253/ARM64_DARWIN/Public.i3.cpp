// library:pgm
// source_base_name:Public
// target_name:Public.i3.cpp
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
 /* declare_opaque */

#ifndef TF32BE1BE
#define TF32BE1BE TF32BE1BE
/*1addressType_define*/typedef ADDRESS TF32BE1BE;

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
 /* declare_field */
 /* record_forwardDeclare Record_t{ typeid:TFFFFFFFF text:TB3D028BC_fields hash_text:NIL base_text:NIL state:0} */
/*record_forwardDeclare*/struct TB3D028BC_fields;typedef struct TB3D028BC_fields TB3D028BC_fields;
 /* record_canBeDefined Record_t{ typeid:TFFFFFFFF text:TB3D028BC_fields hash_text:NIL base_text:NIL state:0} */
 /* record_define Record_t{ typeid:TFFFFFFFF text:TB3D028BC_fields hash_text:NIL base_text:NIL state:0} */

#ifndef TB3D028BC_fields
#define TB3D028BC_fields TB3D028BC_fields
/*record_define*/struct TB3D028BC_fields{
INTEGER a;
INTEGER c;
};
#endif
 /* declare_subrange */
/*subrange_define*/typedef INT8 T66A2A904_8;
 /* declare_pointer */
typedef T66A2A904_8*TAB374D58;
 /* declare_indirect */
typedef INTEGER*TE6A3D58B;
 /* declare_proctype */
 /* declare_formal */

#ifndef Ctypes__char_star
#define Ctypes__char_star Ctypes__char_star
typedef TAB374D58 Ctypes__char_star;
#endif
 /* declare_formal */
 /* declare_proctype */
 /* declare_formal */
 /* declare_formal */
 /* declare_proctype */
 /* declare_formal */
 /* declare_record */
 /* declare_record */
 /* DeclareTypes_FlushOnce size:5 */
typedef TB3D028BC_fields*TB3D028BC;

#if 0 /* avoid type hash collions */
typedef 
void(__cdecl*T89FFB839)(Ctypes__char_star,INTEGER*);
#else
typedef void (__cdecl*T89FFB839)(void);
#endif

#ifndef Public__T
#define Public__T Public__T
typedef TB3D028BC Public__T;
#endif

#if 0 /* avoid type hash collions */
typedef 
void(__cdecl*T31B239E9)(Public__T);
#else
typedef void (__cdecl*T31B239E9)(void);
#endif
 /* DeclareTypes_FlushOnce size:1 */

#if 0 /* avoid type hash collions */
typedef 
void(__cdecl*T5146E3F7)(Ctypes__char_star,Public__T);
#else
typedef void (__cdecl*T5146E3F7)(void);
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
/*Proc_ForwardDeclareFrameType*/struct Ctypes_I3_Frame_t;typedef struct Ctypes_I3_Frame_t Ctypes_I3_Frame_t;
 /* internal_declare_param */
RT0__ModulePtr
__cdecl
Ctypes_I3(
   /* Param_Type1 */ INTEGER /*TypeText1*/  mode_L_0);
 /* import_procedure */
/*Proc_ForwardDeclareFrameType*/struct RTHooks_I3_Frame_t;typedef struct RTHooks_I3_Frame_t RTHooks_I3_Frame_t;
 /* internal_declare_param */
RT0__ModulePtr
__cdecl
RTHooks_I3(
   /* Param_Type1 */ INTEGER /*TypeText1*/  mode_L_1);
 /* import_procedure */
/*Proc_ForwardDeclareFrameType*/struct Public_M3_Frame_t;typedef struct Public_M3_Frame_t Public_M3_Frame_t;
 /* internal_declare_param */
RT0__ModulePtr
__cdecl
Public_M3(
   /* Param_Type1 */ INTEGER /*TypeText1*/  mode_L_2);
 /* import_procedure */
/*Proc_ForwardDeclareFrameType*/struct put_adr_Frame_t;typedef struct put_adr_Frame_t put_adr_Frame_t;
 /* internal_declare_param */
 /* internal_declare_param */
void /*TypeText3*/ 
__cdecl
put_adr(
   /* Param_Type1 */ Ctypes__char_star t_L_3,
   /* Param_Type1 */ INTEGER* /*TypeText1*/  a_L_4);
 /* import_procedure */
/*Proc_ForwardDeclareFrameType*/struct Public__F1_Frame_t;typedef struct Public__F1_Frame_t Public__F1_Frame_t;
 /* internal_declare_param */
void /*TypeText3*/ 
__cdecl
Public__F1(
   /* Param_Type1 */ Public__T a_L_5);
 /* end: imports */
 /* begin: locals */
 /* declare_segment name:<NIL> typeid:TFFFFFFFF const:TRUE */
/*declare_segment*/struct Public_i_6_L_7_t;
/*declare_segment*/typedef struct Public_i_6_L_7_t Public_i_6_L_7_t;
 /* declare_segment name:I_Public typeid:TFFFFFFFF const:FALSE */
 /* handler_name_prefixes:Public_M3_LINE_ */
 /* handler_name_prefixes:Public_I3_LINE_ */
/*declare_segment*/struct Public_i_I_Public_L_8_t;
/*declare_segment*/typedef struct Public_i_I_Public_L_8_t Public_i_I_Public_L_8_t;
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct Public_I3_Frame_t;typedef struct Public_I3_Frame_t Public_I3_Frame_t;
 /* internal_declare_param */
RT0__ModulePtr
__cdecl
Public_I3(
   /* Param_Type1 */ INTEGER /*TypeText1*/  mode_L_9);
 /* Locals_begin_procedure */
 /* begin_block */
 /* Locals_end_procedure */
 /* end_block */
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct Public_I3_tb3d028bc_INIT_Frame_t;typedef struct Public_I3_tb3d028bc_INIT_Frame_t Public_I3_tb3d028bc_INIT_Frame_t;
 /* internal_declare_param */
void /*TypeText3*/ 
__cdecl
Public_I3_tb3d028bc_INIT(
   /* Param_Type1 */ TB3D028BC_fields* /*TypeText1*/  Public_i_10_L_11);
 /* Locals_begin_procedure */
 /* begin_block */
 /* declare_temp */
 /* declare_local */
 /* declare_temp */
 /* declare_local */
 /* Locals_end_procedure */
 /* end_block */
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct Public_I3_tb3d028bc_LINK_Frame_t;typedef struct Public_I3_tb3d028bc_LINK_Frame_t Public_I3_tb3d028bc_LINK_Frame_t;
 /* internal_declare_param */
void /*TypeText3*/ 
__cdecl
Public_I3_tb3d028bc_LINK(
   /* Param_Type1 */ ADDRESS /*TypeText1*/  Public_i_16_L_17);
 /* Locals_begin_procedure */
 /* begin_block */
 /* declare_temp */
 /* declare_local */
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
 /* init_chars */
 /* end_init */
struct Public_i_6_L_7_t{UINT8 L_24[9];
char L_25[7];
ADDRESS L_26[2];
char L_27[8];
UINT8 L_28[12];
char L_29[1];
INT8 L_30[8];
UINT8 L_31[8];
char L_32[11];
};
static  const Public_i_6_L_7_t Public_i_6_L_7={{'P','u','b','l','i','c','_','I','3'},{0 /* 1 */ ,0 /* 2 */ ,0 /* 3 */ ,0 /* 4 */ ,0 /* 5 */ ,0 /* 6 */ ,0 /* 7 */ ,},{(ADDRESS)&Public_I3,(char*)&Public_i_6_L_7},{0 /* 1 */ ,0 /* 2 */ ,0 /* 3 */ ,0 /* 4 */ ,0 /* 5 */ ,0 /* 6 */ ,0 /* 7 */ ,0 /* 8 */ ,},{'.','.','/','P','u','b','l','i','c','.','i','3'},{0 /* 1 */ ,},{((INT8)15),((INT8)15),((INT8)0),((INT8)3),((INT8)12),((INT8)2),((INT8)7),((INT8)25)},{'P','u','b','l','i','c','.','T'},{0 /* 1 */ ,0 /* 2 */ ,0 /* 3 */ ,0 /* 4 */ ,0 /* 5 */ ,0 /* 6 */ ,0 /* 7 */ ,0 /* 8 */ ,0 /* 9 */ ,0 /* 10 */ ,0 /* 11 */ ,}};
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
 /* init_int */
 /* init_proc */
 /* init_int */
 /* init_proc */
 /* init_var */
 /* init_proc */
 /* init_var */
 /* init_proc */
 /* init_int */
 /* end_init */
struct Public_i_I_Public_L_8_t{ADDRESS L_33[3];
char L_34[16];
ADDRESS L_35[1];
char L_36[24];
ADDRESS L_37[1];
char L_38[8];
ADDRESS L_39[1];
INT64 L_40[1];
char L_41[8];
INT64 L_42[1];
UINT8 L_43[4];
INT8 L_44[1];
UINT8 L_45[1];
INT8 L_46[4];
char L_47[1];
INT8 L_48[1];
char L_49[4];
INT64 L_50[1];
ADDRESS L_51[1];
char L_52[8];
ADDRESS L_53[2];
char L_54[8];
ADDRESS L_55[1];
char L_56[8];
INT64 L_57[1];
ADDRESS L_58[1];
char L_59[16];
INT64 L_60[1];
char L_61[24];
ADDRESS L_62[2];
char L_63[8];
ADDRESS L_64[2];
char L_65[8];
ADDRESS L_66[1];
char L_67[16];
INT64 L_68[1];
char L_69[8];
};
static Public_i_I_Public_L_8_t Public_i_I_Public_L_8={{40+(char*)&Public_i_6_L_7,104+(char*)&Public_i_I_Public_L_8,328+(char*)&Public_i_I_Public_L_8},{0 /* 1 */ ,0 /* 2 */ ,0 /* 3 */ ,0 /* 4 */ ,0 /* 5 */ ,0 /* 6 */ ,0 /* 7 */ ,0 /* 8 */ ,0 /* 9 */ ,0 /* 10 */ ,0 /* 11 */ ,0 /* 12 */ ,0 /* 13 */ ,0 /* 14 */ ,0 /* 15 */ ,0 /* 16 */ ,},{16+(char*)&Public_i_6_L_7},{0 /* 1 */ ,0 /* 2 */ ,0 /* 3 */ ,0 /* 4 */ ,0 /* 5 */ ,0 /* 6 */ ,0 /* 7 */ ,0 /* 8 */ ,0 /* 9 */ ,0 /* 10 */ ,0 /* 11 */ ,0 /* 12 */ ,0 /* 13 */ ,0 /* 14 */ ,0 /* 15 */ ,0 /* 16 */ ,0 /* 17 */ ,0 /* 18 */ ,0 /* 19 */ ,0 /* 20 */ ,0 /* 21 */ ,0 /* 22 */ ,0 /* 23 */ ,0 /* 24 */ ,},{256+(char*)&Public_i_I_Public_L_8},{0 /* 1 */ ,0 /* 2 */ ,0 /* 3 */ ,0 /* 4 */ ,0 /* 5 */ ,0 /* 6 */ ,0 /* 7 */ ,0 /* 8 */ ,},{(ADDRESS)&Public_I3},{INT64_(3)},{0 /* 1 */ ,0 /* 2 */ ,0 /* 3 */ ,0 /* 4 */ ,0 /* 5 */ ,0 /* 6 */ ,0 /* 7 */ ,0 /* 8 */ ,},{INT64_(-1278203716)},{206U,149U,219U,165U},{((INT8)114)},{189U},{((INT8)11),((INT8)22),((INT8)1),((INT8)2)
},{0 /* 1 */ ,},{((INT8)8)},{0 /* 1 */ ,0 /* 2 */ ,0 /* 3 */ ,0 /* 4 */ ,},{INT64_(16)},{53+(char*)&Public_i_6_L_7},{0 /* 1 */ ,0 /* 2 */ ,0 /* 3 */ ,0 /* 4 */ ,0 /* 5 */ ,0 /* 6 */ ,0 /* 7 */ ,0 /* 8 */ ,},{56+(char*)&Public_i_6_L_7,(ADDRESS)&Public_I3_tb3d028bc_INIT},{0 /* 1 */ ,0 /* 2 */ ,0 /* 3 */ ,0 /* 4 */ ,0 /* 5 */ ,0 /* 6 */ ,0 /* 7 */ ,0 /* 8 */ ,},{61+(char*)&Public_i_6_L_7},{0 /* 1 */ ,0 /* 2 */ ,0 /* 3 */ ,0 /* 4 */ ,0 /* 5 */ ,0 /* 6 */ ,0 /* 7 */ ,0 /* 8 */ ,},{INT64_(-215227970)},{(ADDRESS)&Public_I3_tb3d028bc_LINK},{0 /* 1 */ ,0 /* 2 */ ,0 /* 3 */ ,0 /* 4 */ ,0 /* 5 */ ,0 /* 6 */ ,0 /* 7 */ ,0 /* 8 */ ,0 /* 9 */ ,0 /* 10 */ ,0 /* 11 */ ,0 /* 12 */ ,0 /* 13 */ ,0 /* 14 */ ,0 /* 15 */ ,0 /* 16 */ ,},{INT64_(8)},{0 /* 1 */ ,0 /* 2 */ ,0 /* 3 */ ,0 /* 4 */ ,0 /* 5 */ ,0 /* 6 */ ,0 /* 7 */ ,0 /* 8 */ ,0 /* 9 */ ,0 /* 10 */ ,0 /* 11 */ ,0 /* 12 */ ,0 /* 13 */ ,0 /* 14 */ ,0 /* 15 */ ,0 /* 16 */ ,0 /* 17 */ ,0 /* 18 */ ,0 /* 19 */ ,0 /* 20 */ ,0 /* 21 */ ,0 /* 22 */ ,0 /* 23 */ 
,0 /* 24 */ ,},{(ADDRESS)&Ctypes_I3,280+(char*)&Public_i_I_Public_L_8},{0 /* 1 */ ,0 /* 2 */ ,0 /* 3 */ ,0 /* 4 */ ,0 /* 5 */ ,0 /* 6 */ ,0 /* 7 */ ,0 /* 8 */ ,},{(ADDRESS)&RTHooks_I3,304+(char*)&Public_i_I_Public_L_8},{0 /* 1 */ ,0 /* 2 */ ,0 /* 3 */ ,0 /* 4 */ ,0 /* 5 */ ,0 /* 6 */ ,0 /* 7 */ ,0 /* 8 */ ,},{(ADDRESS)&Public_M3},{0 /* 1 */ ,0 /* 2 */ ,0 /* 3 */ ,0 /* 4 */ ,0 /* 5 */ ,0 /* 6 */ ,0 /* 7 */ ,0 /* 8 */ ,0 /* 9 */ ,0 /* 10 */ ,0 /* 11 */ ,0 /* 12 */ ,0 /* 13 */ ,0 /* 14 */ ,0 /* 15 */ ,0 /* 16 */ ,},{INT64_(-1278203716)},{0 /* 1 */ ,0 /* 2 */ ,0 /* 3 */ ,0 /* 4 */ ,0 /* 5 */ ,0 /* 6 */ ,0 /* 7 */ ,0 /* 8 */ ,}};
 /* end: segments/globals */
 /* begin: mark used */
 /* end: mark used */
 /* set_source_file */
 /* set_source_line */
#line 1 "../Public.i3"
 /* module global constants */
#line 1 "../Public.i3"
 /* module global data */
#line 1 "../Public.i3"
 /* set_source_line */
#line 1 "../Public.i3"
#line 18 "../Public.i3"
 /* Public_I3 */
#line 18 "../Public.i3"
 /* module main body Public_I3 */
#line 18 "../Public.i3"
 /* begin_procedure */
#line 18 "../Public.i3"
struct Public_I3_Frame_t {
#line 18 "../Public.i3"
ADDRESS _unused;
#line 18 "../Public.i3"
};
#line 18 "../Public.i3"
RT0__ModulePtr
__cdecl
Public_I3(
   /* Param_Type1 */ INTEGER /*TypeText1*/  mode_L_9)
{
#line 18 "../Public.i3"
Public_I3_Frame_t _frame;
#line 18 "../Public.i3"
_frame._unused=(ADDRESS)&_frame;
#line 18 "../Public.i3"
 /* load */
#line 18 "../Public.i3"
 /* if_true_or_false */
#line 18 "../Public.i3"
 /* load_host_integer */
#line 18 "../Public.i3"
 /* load_integer */
#line 18 "../Public.i3"
 /* if_compare */
#line 18 "../Public.i3"
if(m3_eq(INT64,
  mode_L_9,
   INT64_(0)))goto L1;
#line 18 "../Public.i3"
 /* set_label */
#line 18 "../Public.i3"
L1:;
#line 18 "../Public.i3"
 /* load_address */
#line 18 "../Public.i3"
 /* exit_proc */
#line 18 "../Public.i3"
return (RT0__ModulePtr)(&Public_i_I_Public_L_8);
#line 18 "../Public.i3"
 /* end_procedure */
#line 18 "../Public.i3"
} /* Public_I3_tb3d028bc_INIT (ObjectType) */
#line 18 "../Public.i3"
 /* set_source_line */
#line 18 "../Public.i3"
#line 6 "../Public.i3"
 /* begin_procedure */
#line 6 "../Public.i3"
struct Public_I3_tb3d028bc_INIT_Frame_t {
#line 6 "../Public.i3"
ADDRESS _unused;
#line 6 "../Public.i3"
};
#line 6 "../Public.i3"
void /*TypeText3*/ 
__cdecl
Public_I3_tb3d028bc_INIT(
   /* Param_Type1 */ TB3D028BC_fields* /*TypeText1*/  Public_i_10_L_11)
{
#line 6 "../Public.i3"
 /* Var_Type2 */ INT64 Public_i_12_L_13={0};//always-init
#line 6 "../Public.i3"
 /* Var_Type2 */ ADDRESS Public_i_14_L_15={0};//always-init
#line 6 "../Public.i3"
Public_I3_tb3d028bc_INIT_Frame_t _frame;
#line 6 "../Public.i3"
_frame._unused=(ADDRESS)&_frame;
#line 6 "../Public.i3"
 /* load */
#line 6 "../Public.i3"
 /* load_indirect */
#line 6 "../Public.i3"
 /* store */
#line 6 "../Public.i3"
(*(INT64*)(&Public_i_12_L_13))=(INT64)( *((INT64*)(INT64_(112)+((ADDRESS)(*((ADDRESS*)(INT64_(328)+((ADDRESS)(&Public_i_I_Public_L_8)))))))));
#line 6 "../Public.i3"
 /* load */
#line 6 "../Public.i3"
 /* load */
#line 6 "../Public.i3"
 /* index_address */
#line 6 "../Public.i3"
 /* store */
#line 6 "../Public.i3"
(*(ADDRESS*)(&Public_i_14_L_15))=(ADDRESS)(((ADDRESS)((((ADDRESS)(Public_i_10_L_11))+( Public_i_12_L_13)))));
#line 6 "../Public.i3"
 /* load */
#line 6 "../Public.i3"
 /* load_integer */
#line 6 "../Public.i3"
 /* store_indirect */
#line 6 "../Public.i3"
(*(INT64*)(Public_i_14_L_15))=(INT64)(  INT64_(1));
#line 6 "../Public.i3"
 /* load */
#line 6 "../Public.i3"
 /* load_integer */
#line 6 "../Public.i3"
 /* store_indirect */
#line 6 "../Public.i3"
(*(INT64*)((8)+(char*)(Public_i_14_L_15)))=(INT64)(  INT64_(3));
#line 6 "../Public.i3"
 /* exit_proc */
#line 6 "../Public.i3"
return;
#line 6 "../Public.i3"
 /* end_procedure */
#line 6 "../Public.i3"
} /* link-time setup code for _tb3d028bc */
#line 6 "../Public.i3"
 /* Public_I3_tb3d028bc_LINK */
#line 6 "../Public.i3"
 /* begin_procedure */
#line 6 "../Public.i3"
struct Public_I3_tb3d028bc_LINK_Frame_t {
#line 6 "../Public.i3"
ADDRESS _unused;
#line 6 "../Public.i3"
};
#line 6 "../Public.i3"
void /*TypeText3*/ 
__cdecl
Public_I3_tb3d028bc_LINK(
   /* Param_Type1 */ ADDRESS /*TypeText1*/  Public_i_16_L_17)
{
#line 6 "../Public.i3"
 /* Var_Type2 */ ADDRESS Public_i_18_L_19={0};//always-init
#line 6 "../Public.i3"
 /* Var_Type2 */ INT64 Public_i_20_L_21={0};//always-init
#line 6 "../Public.i3"
 /* Var_Type2 */ ADDRESS Public_i_22_L_23={0};//always-init
#line 6 "../Public.i3"
Public_I3_tb3d028bc_LINK_Frame_t _frame;
#line 6 "../Public.i3"
_frame._unused=(ADDRESS)&_frame;
#line 6 "../Public.i3"
 /* load */
#line 6 "../Public.i3"
 /* load_indirect */
#line 6 "../Public.i3"
 /* store */
#line 6 "../Public.i3"
(*(ADDRESS*)(&Public_i_18_L_19))=(ADDRESS)(((ADDRESS)(*((ADDRESS*)(INT64_(136)+((ADDRESS)(Public_i_16_L_17)))))));
#line 6 "../Public.i3"
 /* load */
#line 6 "../Public.i3"
 /* load_indirect */
#line 6 "../Public.i3"
 /* store */
#line 6 "../Public.i3"
(*(INT64*)(&Public_i_20_L_21))=(INT64)( *((INT64*)(INT64_(120)+((ADDRESS)(*((ADDRESS*)(INT64_(328)+((ADDRESS)(&Public_i_I_Public_L_8)))))))));
#line 6 "../Public.i3"
 /* load */
#line 6 "../Public.i3"
 /* load */
#line 6 "../Public.i3"
 /* index_address */
#line 6 "../Public.i3"
 /* store */
#line 6 "../Public.i3"
(*(ADDRESS*)(&Public_i_22_L_23))=(ADDRESS)(((ADDRESS)((((ADDRESS)(Public_i_18_L_19))+( Public_i_20_L_21)))));
#line 6 "../Public.i3"
 /* load_procedure */
#line 6 "../Public.i3"
 /* load */
#line 6 "../Public.i3"
 /* swap */
#line 6 "../Public.i3"
 /* store_indirect */
#line 6 "../Public.i3"
(*(ADDRESS*)(Public_i_22_L_23))=(ADDRESS)(((ADDRESS)(Public__F1)));
#line 6 "../Public.i3"
 /* exit_proc */
#line 6 "../Public.i3"
return;
#line 6 "../Public.i3"
 /* end_procedure */
#line 6 "../Public.i3"
} /* global constant type descriptor */
#line 6 "../Public.i3"
 /* global data type descriptor */
#line 6 "../Public.i3"
 /* module global constants */
#line 6 "../Public.i3"
 /* procedure names */
#line 6 "../Public.i3"
 /* procedure table */
#line 6 "../Public.i3"
 /* file name */
#line 6 "../Public.i3"
 /* type map for _tb3d028bc */
#line 6 "../Public.i3"
 /* type description for _tb3d028bc */
#line 6 "../Public.i3"
 /* module global data */
#line 6 "../Public.i3"
 /* typecell for _tb3d028bc */
#line 6 "../Public.i3"
 /* load map


 global data allocation for I_Public
     0   104  8  *module info*
   104   152  8  typecell
   256    24  8  import Ctypes
   280    24  8  import RTHooks
   304    24  8  import Public
   328    16  8  typecell ptr
   344     0  8  *TOTAL*


 global constants for I_Public
     0    10  8  *proc names*
    16    24  8  *proc info*
    40    13  1  *string*
    53     3  1  type_map
    56     5  1  type_desc
    61     9  1  *string*
    72     0  8  *TOTAL*
 */
#line 6 "../Public.i3"
 /* end unit */
#line 6 "../Public.i3"

#ifdef __cplusplus

} /* extern "C" */
#endif
 /* set_runtime_proc */
 /* set_runtime_proc */
 /* set_runtime_proc */
