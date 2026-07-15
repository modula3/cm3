// library:pgm
// source_base_name:Public
// target_name:Public.m3.cpp
 /* set_runtime_proc */
 /* set_runtime_proc */
 /* set_runtime_proc */
 /* set_runtime_proc */
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
 /* declare_proctype */
 /* declare_formal */
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
 /* declare_opaque */

#ifndef T6D871B27
#define T6D871B27 T6D871B27
/*1addressType_define*/typedef ADDRESS T6D871B27;

#endif
 /* declare_record */
 /* declare_field */
 /* declare_field */
 /* declare_field */
 /* record_forwardDeclare Record_t{ typeid:TB750D684 text:NIL hash_text:TB750D684 base_text:NIL state:0} */
/*record_forwardDeclare*/struct TB750D684;typedef struct TB750D684 TB750D684;
 /* record_canBeDefined Record_t{ typeid:TB750D684 text:NIL hash_text:TB750D684 base_text:NIL state:0} */
 /* record_define Record_t{ typeid:TB750D684 text:NIL hash_text:TB750D684 base_text:NIL state:0} */

#ifndef TB750D684
#define TB750D684 TB750D684
/*record_define*/struct TB750D684{
ADDRESS start;
WORD_T length;
BOOLEAN wide;
UINT8 L_0[7];
};
#endif
 /* declare_indirect */

#ifndef RTHooks__TextInfo
#define RTHooks__TextInfo RTHooks__TextInfo
typedef TB750D684 RTHooks__TextInfo;
#endif
typedef RTHooks__TextInfo*T48AF297B;
 /* declare_proctype */
 /* declare_formal */

#ifndef RTHooks__TextLiteral
#define RTHooks__TextLiteral RTHooks__TextLiteral
typedef T6D871B27 RTHooks__TextLiteral;
#endif
 /* declare_formal */
 /* declare_proctype */
 /* declare_formal */
 /* declare_formal */
 /* declare_proctype */
 /* declare_formal */
 /* declare_formal */
 /* declare_open_array */
/*array_forwardDeclare*/struct T89CD34BD;typedef struct T89CD34BD T89CD34BD;

#ifndef T89CD34BD
#define T89CD34BD T89CD34BD
/*openArray_define*/struct T89CD34BD{
UCHAR*_elts;
CARDINAL _size;
};

#endif
 /* declare_indirect */
typedef T89CD34BD*T7632CB42;
 /* declare_proctype */
 /* declare_formal */
 /* declare_formal */
 /* declare_formal */
 /* declare_open_array */
/*array_forwardDeclare*/struct TA19BDC21;typedef struct TA19BDC21 TA19BDC21;

#ifndef TA19BDC21
#define TA19BDC21 TA19BDC21
/*openArray_define*/struct TA19BDC21{
WIDECHAR*_elts;
CARDINAL _size;
};

#endif
 /* declare_indirect */
typedef TA19BDC21*T5E6423DE;
 /* declare_proctype */
 /* declare_formal */
 /* declare_formal */
 /* declare_formal */
 /* declare_proctype */
 /* declare_formal */
 /* declare_formal */
 /* declare_record */
 /* declare_record */
 /* DeclareTypes_FlushOnce size:12 */
typedef TB3D028BC_fields*TB3D028BC;

#ifndef Public__T
#define Public__T Public__T
typedef TB3D028BC Public__T;
#endif

#if 0 /* avoid type hash collions */
typedef 
void(__cdecl*T89FFB839)(Ctypes__char_star,INTEGER*);
#else
typedef void (__cdecl*T89FFB839)(void);
#endif

#if 0 /* avoid type hash collions */
typedef 
void(__cdecl*T5146E3F7)(Ctypes__char_star,Public__T);
#else
typedef void (__cdecl*T5146E3F7)(void);
#endif

#if 0 /* avoid type hash collions */
typedef 
T66A2A904_8*(__cdecl*TFA94CDA9)(TEXT);
#else
typedef void (__cdecl*TFA94CDA9)(void);
#endif

#if 0 /* avoid type hash collions */
typedef 
void(__cdecl*T38DF5DF5)(RTHooks__TextLiteral,RTHooks__TextInfo*);
#else
typedef void (__cdecl*T38DF5DF5)(void);
#endif

#if 0 /* avoid type hash collions */
typedef 
UCHAR(__cdecl*TB964357B)(RTHooks__TextLiteral,CARDINAL);
#else
typedef void (__cdecl*TB964357B)(void);
#endif

#if 0 /* avoid type hash collions */
typedef 
WIDECHAR(__cdecl*T9132DDE7)(RTHooks__TextLiteral,CARDINAL);
#else
typedef void (__cdecl*T9132DDE7)(void);
#endif

#if 0 /* avoid type hash collions */
typedef 
void(__cdecl*TC9E236C8)(RTHooks__TextLiteral,T89CD34BD*,CARDINAL);
#else
typedef void (__cdecl*TC9E236C8)(void);
#endif

#if 0 /* avoid type hash collions */
typedef 
void(__cdecl*T966FE87F)(RTHooks__TextLiteral,TA19BDC21*,CARDINAL);
#else
typedef void (__cdecl*T966FE87F)(void);
#endif

#if 0 /* avoid type hash collions */
typedef 
void(__cdecl*TA4BB9882)(ADDRESS,INTEGER);
#else
typedef void (__cdecl*TA4BB9882)(void);
#endif
 /* DeclareTypes_FlushOnce size:1 */

#if 0 /* avoid type hash collions */
typedef 
void(__cdecl*T31B239E9)(Public__T);
#else
typedef void (__cdecl*T31B239E9)(void);
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
/*Proc_ForwardDeclareFrameType*/struct Public_I3_Frame_t;typedef struct Public_I3_Frame_t Public_I3_Frame_t;
 /* internal_declare_param */
RT0__ModulePtr
__cdecl
Public_I3(
   /* Param_Type1 */ INTEGER /*TypeText1*/  mode_L_1);
 /* import_procedure */
/*Proc_ForwardDeclareFrameType*/struct M3toC_I3_Frame_t;typedef struct M3toC_I3_Frame_t M3toC_I3_Frame_t;
 /* internal_declare_param */
RT0__ModulePtr
__cdecl
M3toC_I3(
   /* Param_Type1 */ INTEGER /*TypeText1*/  mode_L_2);
 /* import_procedure */
/*Proc_ForwardDeclareFrameType*/struct RTHooks_I3_Frame_t;typedef struct RTHooks_I3_Frame_t RTHooks_I3_Frame_t;
 /* internal_declare_param */
RT0__ModulePtr
__cdecl
RTHooks_I3(
   /* Param_Type1 */ INTEGER /*TypeText1*/  mode_L_3);
 /* import_procedure */
/*Proc_ForwardDeclareFrameType*/struct put_adr_Frame_t;typedef struct put_adr_Frame_t put_adr_Frame_t;
 /* internal_declare_param */
 /* internal_declare_param */
void /*TypeText3*/ 
__cdecl
put_adr(
   /* Param_Type1 */ Ctypes__char_star t_L_4,
   /* Param_Type1 */ INTEGER* /*TypeText1*/  a_L_5);
 /* import_procedure */

#ifndef Ctypes__const_char_star
#define Ctypes__const_char_star Ctypes__const_char_star
typedef T66A2A904_8* /*TypeText1*/  Ctypes__const_char_star;
#endif
/*Proc_ForwardDeclareFrameType*/struct M3toC__FlatTtoS_Frame_t;typedef struct M3toC__FlatTtoS_Frame_t M3toC__FlatTtoS_Frame_t;
 /* internal_declare_param */
Ctypes__const_char_star
__cdecl
M3toC__FlatTtoS(
   /* Param_Type1 */ TEXT t_L_6);
 /* import_procedure */
/*Proc_ForwardDeclareFrameType*/struct RTHooks__TextLitInfo_Frame_t;typedef struct RTHooks__TextLitInfo_Frame_t RTHooks__TextLitInfo_Frame_t;
 /* internal_declare_param */
 /* internal_declare_param */
void /*TypeText3*/ 
__cdecl
RTHooks__TextLitInfo(
   /* Param_Type1 */ RTHooks__TextLiteral t_L_7,
   /* Param_Type1 */ RTHooks__TextInfo* /*TypeText1*/  i_L_8);
 /* import_procedure */

#ifndef m3_CHAR
#define m3_CHAR m3_CHAR
typedef UCHAR /*TypeText1*/  m3_CHAR;
#endif
/*Proc_ForwardDeclareFrameType*/struct RTHooks__TextLitGetChar_Frame_t;typedef struct RTHooks__TextLitGetChar_Frame_t RTHooks__TextLitGetChar_Frame_t;
 /* internal_declare_param */
 /* internal_declare_param */
m3_CHAR
__cdecl
RTHooks__TextLitGetChar(
   /* Param_Type1 */ RTHooks__TextLiteral t_L_9,
   /* Param_Type1 */ CARDINAL i_L_10);
 /* import_procedure */
/*Proc_ForwardDeclareFrameType*/struct RTHooks__TextLitGetWideChar_Frame_t;typedef struct RTHooks__TextLitGetWideChar_Frame_t RTHooks__TextLitGetWideChar_Frame_t;
 /* internal_declare_param */
 /* internal_declare_param */
WIDECHAR
__cdecl
RTHooks__TextLitGetWideChar(
   /* Param_Type1 */ RTHooks__TextLiteral t_L_11,
   /* Param_Type1 */ CARDINAL i_L_12);
 /* import_procedure */
/*Proc_ForwardDeclareFrameType*/struct RTHooks__TextLitGetChars_Frame_t;typedef struct RTHooks__TextLitGetChars_Frame_t RTHooks__TextLitGetChars_Frame_t;
 /* internal_declare_param */
 /* internal_declare_param */
 /* internal_declare_param */
void /*TypeText3*/ 
__cdecl
RTHooks__TextLitGetChars(
   /* Param_Type1 */ RTHooks__TextLiteral t_L_13,
   /* Param_Type1 */ T89CD34BD* /*TypeText1*/  a_L_14,
   /* Param_Type1 */ CARDINAL start_L_15);
 /* import_procedure */
/*Proc_ForwardDeclareFrameType*/struct RTHooks__TextLitGetWideChars_Frame_t;typedef struct RTHooks__TextLitGetWideChars_Frame_t RTHooks__TextLitGetWideChars_Frame_t;
 /* internal_declare_param */
 /* internal_declare_param */
 /* internal_declare_param */
void /*TypeText3*/ 
__cdecl
RTHooks__TextLitGetWideChars(
   /* Param_Type1 */ RTHooks__TextLiteral t_L_16,
   /* Param_Type1 */ TA19BDC21* /*TypeText1*/  a_L_17,
   /* Param_Type1 */ CARDINAL start_L_18);
 /* import_procedure */
/*Proc_ForwardDeclareFrameType*/struct RTHooks__ReportFault_Frame_t;typedef struct RTHooks__ReportFault_Frame_t RTHooks__ReportFault_Frame_t;
 /* internal_declare_param */
 /* internal_declare_param */
void /*TypeText3*/ 
__cdecl
RTHooks__ReportFault(
   /* Param_Type1 */ ADDRESS module_L_19,
   /* Param_Type1 */ INTEGER info_L_20) M3_ATTRIBUTE_NO_RETURN;
 /* end: imports */
 /* begin: locals */
 /* declare_segment name:<NIL> typeid:TFFFFFFFF const:TRUE */
/*declare_segment*/struct Public_m_21_L_22_t;
/*declare_segment*/typedef struct Public_m_21_L_22_t Public_m_21_L_22_t;
 /* declare_segment name:M_Public typeid:TFFFFFFFF const:FALSE */
 /* handler_name_prefixes:Public_M3_LINE_ */
 /* handler_name_prefixes:Public_I3_LINE_ */
/*declare_segment*/struct Public_m_M_Public_L_23_t;
/*declare_segment*/typedef struct Public_m_M_Public_L_23_t Public_m_M_Public_L_23_t;
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct Public_M3_Frame_t;typedef struct Public_M3_Frame_t Public_M3_Frame_t;
 /* internal_declare_param */
RT0__ModulePtr
__cdecl
Public_M3(
   /* Param_Type1 */ INTEGER /*TypeText1*/  mode_L_24);
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct Public__F1_Frame_t;typedef struct Public__F1_Frame_t Public__F1_Frame_t;
 /* internal_declare_param */
void /*TypeText3*/ 
__cdecl
Public__F1(
   /* Param_Type1 */ Public__T a_L_25);
 /* Locals_begin_procedure */
 /* begin_block */
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
 /* Locals_end_procedure */
 /* end_block */
 /* end: locals */
 /* begin: segments/globals */
 /* bind_segment */
 /* begin_init */
 /* init_proc */
 /* init_proc */
 /* init_proc */
 /* init_proc */
 /* init_proc */
 /* init_int */
 /* init_var */
 /* init_int */
 /* init_chars */
 /* init_int */
 /* init_var */
 /* init_int */
 /* init_chars */
 /* init_int */
 /* init_var */
 /* init_int */
 /* init_chars */
 /* init_chars */
 /* init_chars */
 /* init_proc */
 /* init_var */
 /* init_proc */
 /* init_var */
 /* init_chars */
 /* end_init */
struct Public_m_21_L_22_t{ADDRESS L_32[5];
INT64 L_33[1];
ADDRESS L_34[1];
INT64 L_35[1];
UINT8 L_36[11];
char L_37[5];
INT64 L_38[1];
ADDRESS L_39[1];
INT64 L_40[1];
UINT8 L_41[13];
char L_42[3];
INT64 L_43[1];
ADDRESS L_44[1];
INT64 L_45[1];
UINT8 L_46[13];
char L_47[3];
UINT8 L_48[9];
char L_49[1];
UINT8 L_50[2];
char L_51[4];
ADDRESS L_52[4];
char L_53[8];
UINT8 L_54[12];
char L_55[12];
};
static  const Public_m_21_L_22_t Public_m_21_L_22={{(ADDRESS)&RTHooks__TextLitInfo,(ADDRESS)&RTHooks__TextLitGetChar,(ADDRESS)&RTHooks__TextLitGetWideChar,(ADDRESS)&RTHooks__TextLitGetChars,(ADDRESS)&RTHooks__TextLitGetWideChars},{INT64_(2)},{(char*)&Public_m_21_L_22},{INT64_(11)},{'P','u','b','l','i','c','.','F','1','.','a'},{0 /* 1 */ ,0 /* 2 */ ,0 /* 3 */ ,0 /* 4 */ ,0 /* 5 */ ,},{INT64_(2)},{(char*)&Public_m_21_L_22},{INT64_(13)},{'P','u','b','l','i','c','.','F','1','.','a','.','a'},{0 /* 1 */ ,0 /* 2 */ ,0 /* 3 */ ,},{INT64_(2)},{(char*)&Public_m_21_L_22},{INT64_(13)},{'P','u','b','l','i','c','.','F','1','.','a','.','c'},{0 /* 1 */ ,0 /* 2 */ ,0 /* 3 */ ,},{'P','u','b','l','i','c','_','M','3'},{0 /* 1 */ ,},{'F','1'},{0 /* 1 */ ,0 /* 2 */ ,0 /* 3 */ ,0 /* 4 */ ,},{(ADDRESS)&Public_M3,160+(char*)&Public_m_21_L_22,(ADDRESS)&Public__F1,170+(char*)&Public_m_21_L_22},{0 /* 1 */ ,0 /* 2 */ ,0 /* 3 */ ,0 /* 4 */ ,0 /* 5 */ ,0 /* 6 */ ,0 /* 7 */ ,0 /* 8 */ ,},{'.','.','/','P','u','b','l',
'i','c','.','m','3'},{0 /* 1 */ ,0 /* 2 */ ,0 /* 3 */ ,0 /* 4 */ ,0 /* 5 */ ,0 /* 6 */ ,0 /* 7 */ ,0 /* 8 */ ,0 /* 9 */ ,0 /* 10 */ ,0 /* 11 */ ,0 /* 12 */ ,}};
 /* bind_segment */
 /* begin_init */
 /* init_var */
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
 /* init_int */
 /* end_init */
struct Public_m_M_Public_L_23_t{ADDRESS L_56[1];
char L_57[8];
ADDRESS L_58[1];
char L_59[16];
ADDRESS L_60[1];
char L_61[24];
ADDRESS L_62[1];
char L_63[8];
ADDRESS L_64[1];
INT64 L_65[1];
char L_66[8];
ADDRESS L_67[2];
char L_68[8];
ADDRESS L_69[2];
char L_70[8];
ADDRESS L_71[1];
char L_72[16];
INT64 L_73[1];
};
static Public_m_M_Public_L_23_t Public_m_M_Public_L_23={{216+(char*)&Public_m_21_L_22},{0 /* 1 */ ,0 /* 2 */ ,0 /* 3 */ ,0 /* 4 */ ,0 /* 5 */ ,0 /* 6 */ ,0 /* 7 */ ,0 /* 8 */ ,},{176+(char*)&Public_m_M_Public_L_23},{0 /* 1 */ ,0 /* 2 */ ,0 /* 3 */ ,0 /* 4 */ ,0 /* 5 */ ,0 /* 6 */ ,0 /* 7 */ ,0 /* 8 */ ,0 /* 9 */ ,0 /* 10 */ ,0 /* 11 */ ,0 /* 12 */ ,0 /* 13 */ ,0 /* 14 */ ,0 /* 15 */ ,0 /* 16 */ ,},{176+(char*)&Public_m_21_L_22},{0 /* 1 */ ,0 /* 2 */ ,0 /* 3 */ ,0 /* 4 */ ,0 /* 5 */ ,0 /* 6 */ ,0 /* 7 */ ,0 /* 8 */ ,0 /* 9 */ ,0 /* 10 */ ,0 /* 11 */ ,0 /* 12 */ ,0 /* 13 */ ,0 /* 14 */ ,0 /* 15 */ ,0 /* 16 */ ,0 /* 17 */ ,0 /* 18 */ ,0 /* 19 */ ,0 /* 20 */ ,0 /* 21 */ ,0 /* 22 */ ,0 /* 23 */ ,0 /* 24 */ ,},{104+(char*)&Public_m_M_Public_L_23},{0 /* 1 */ ,0 /* 2 */ ,0 /* 3 */ ,0 /* 4 */ ,0 /* 5 */ ,0 /* 6 */ ,0 /* 7 */ ,0 /* 8 */ ,},{(ADDRESS)&Public_M3},{INT64_(3)},{0 /* 1 */ ,0 /* 2 */ ,0 /* 3 */ ,0 /* 4 */ ,0 /* 5 */ ,0 /* 6 */ ,0 /* 7 */ ,0 /* 8 */ ,},{(ADDRESS)&Public_I3,128+(char*)&Public_m_M_Public_L_23
},{0 /* 1 */ ,0 /* 2 */ ,0 /* 3 */ ,0 /* 4 */ ,0 /* 5 */ ,0 /* 6 */ ,0 /* 7 */ ,0 /* 8 */ ,},{(ADDRESS)&M3toC_I3,152+(char*)&Public_m_M_Public_L_23},{0 /* 1 */ ,0 /* 2 */ ,0 /* 3 */ ,0 /* 4 */ ,0 /* 5 */ ,0 /* 6 */ ,0 /* 7 */ ,0 /* 8 */ ,},{(ADDRESS)&RTHooks_I3},{0 /* 1 */ ,0 /* 2 */ ,0 /* 3 */ ,0 /* 4 */ ,0 /* 5 */ ,0 /* 6 */ ,0 /* 7 */ ,0 /* 8 */ ,0 /* 9 */ ,0 /* 10 */ ,0 /* 11 */ ,0 /* 12 */ ,0 /* 13 */ ,0 /* 14 */ ,0 /* 15 */ ,0 /* 16 */ ,},{INT64_(-1278203716)}};
static void __cdecl Public_m_M_Public_L_23_CRASH(WORD_T code) M3_ATTRIBUTE_NO_RETURN;
static void __cdecl Public_m_M_Public_L_23_CRASH(WORD_T code){RTHooks__ReportFault((ADDRESS)&Public_m_M_Public_L_23,code);} /* end: segments/globals */
 /* begin: mark used */
 /* end: mark used */
 /* set_source_file */
 /* set_source_line */
#line 1 "../Public.m3"
 /* module global constants */
#line 1 "../Public.m3"
 /* module global data */
#line 1 "../Public.m3"
 /* set_source_line */
#line 1 "../Public.m3"
#line 12 "../Public.m3"
 /* F1 */
#line 12 "../Public.m3"
 /* set_source_line */
#line 12 "../Public.m3"
#line 5 "../Public.m3"
 /* begin_procedure */
#line 5 "../Public.m3"
struct Public__F1_Frame_t {
#line 5 "../Public.m3"
ADDRESS _unused;
#line 5 "../Public.m3"
};
#line 5 "../Public.m3"
void /*TypeText3*/ 
__cdecl
Public__F1(
   /* Param_Type1 */ Public__T a_L_25)
{
#line 5 "../Public.m3"
 /* Var_Type2 */ ADDRESS Public_m_26_L_27={0};//always-init
#line 5 "../Public.m3"
 /* Var_Type2 */ ADDRESS Public_m_28_L_29={0};//always-init
#line 5 "../Public.m3"
 /* Var_Type2 */ ADDRESS Public_m_30_L_31={0};//always-init
#line 5 "../Public.m3"
Public__F1_Frame_t _frame;
#line 5 "../Public.m3"
_frame._unused=(ADDRESS)&_frame;
#line 5 "../Public.m3"
 /* set_source_line */
#line 5 "../Public.m3"
#line 6 "../Public.m3"
 /* set_source_line */
#line 6 "../Public.m3"
#line 7 "../Public.m3"
 /* start_call_direct */
#line 7 "../Public.m3"
 /* load_address */
#line 7 "../Public.m3"
 /* pop_param */
#line 7 "../Public.m3"
 /* call_direct */
#line 7 "../Public.m3"
 /* store */
#line 7 "../Public.m3"
(*(ADDRESS*)(&Public_m_26_L_27))=(ADDRESS)(((ADDRESS)(M3toC__FlatTtoS(
  ( TEXT )(((ADDRESS)(INT64_(48)+((ADDRESS)(&Public_m_21_L_22)))) )))));
#line 7 "../Public.m3"
 /* start_call_direct */
#line 7 "../Public.m3"
 /* load */
#line 7 "../Public.m3"
 /* pop_param */
#line 7 "../Public.m3"
 /* load */
#line 7 "../Public.m3"
 /* pop_param */
#line 7 "../Public.m3"
 /* call_direct */
#line 7 "../Public.m3"
put_adr(
  ( Ctypes__char_star )(((ADDRESS)(Public_m_26_L_27)) ),
  ( INTEGER* /*TypeText1*/  )(((ADDRESS)(a_L_25)) ));
#line 7 "../Public.m3"
 /* set_source_line */
#line 7 "../Public.m3"
#line 8 "../Public.m3"
 /* start_call_direct */
#line 8 "../Public.m3"
 /* load_address */
#line 8 "../Public.m3"
 /* pop_param */
#line 8 "../Public.m3"
 /* call_direct */
#line 8 "../Public.m3"
 /* store */
#line 8 "../Public.m3"
(*(ADDRESS*)(&Public_m_26_L_27))=(ADDRESS)(((ADDRESS)(M3toC__FlatTtoS(
  ( TEXT )(((ADDRESS)(INT64_(88)+((ADDRESS)(&Public_m_21_L_22)))) )))));
#line 8 "../Public.m3"
 /* start_call_direct */
#line 8 "../Public.m3"
 /* load */
#line 8 "../Public.m3"
 /* pop_param */
#line 8 "../Public.m3"
 /* load */
#line 8 "../Public.m3"
 /* check_nil */
#line 8 "../Public.m3"
 /* store */
#line 8 "../Public.m3"
(*(ADDRESS*)(&Public_m_28_L_29))=(ADDRESS)(((ADDRESS)(a_L_25)));
#line 8 "../Public.m3"
 /* load */
#line 8 "../Public.m3"
/*check_nil*/if(!Public_m_28_L_29)Public_m_M_Public_L_23_CRASH(260);
#line 8 "../Public.m3"
 /* load */
#line 8 "../Public.m3"
 /* load_indirect */
#line 8 "../Public.m3"
 /* index_address */
#line 8 "../Public.m3"
 /* pop_param */
#line 8 "../Public.m3"
 /* call_direct */
#line 8 "../Public.m3"
put_adr(
  ( Ctypes__char_star )(((ADDRESS)(Public_m_26_L_27)) ),
  ( INTEGER* /*TypeText1*/  )(((ADDRESS)((((ADDRESS)(Public_m_28_L_29))+( *((INT64*)(INT64_(112)+((ADDRESS)(*((ADDRESS*)(INT64_(176)+((ADDRESS)(&Public_m_M_Public_L_23)))))))))))) ));
#line 8 "../Public.m3"
 /* set_source_line */
#line 8 "../Public.m3"
#line 9 "../Public.m3"
 /* start_call_direct */
#line 9 "../Public.m3"
 /* load_address */
#line 9 "../Public.m3"
 /* pop_param */
#line 9 "../Public.m3"
 /* call_direct */
#line 9 "../Public.m3"
 /* store */
#line 9 "../Public.m3"
(*(ADDRESS*)(&Public_m_26_L_27))=(ADDRESS)(((ADDRESS)(M3toC__FlatTtoS(
  ( TEXT )(((ADDRESS)(INT64_(128)+((ADDRESS)(&Public_m_21_L_22)))) )))));
#line 9 "../Public.m3"
 /* start_call_direct */
#line 9 "../Public.m3"
 /* load */
#line 9 "../Public.m3"
 /* pop_param */
#line 9 "../Public.m3"
 /* load */
#line 9 "../Public.m3"
 /* check_nil */
#line 9 "../Public.m3"
 /* store */
#line 9 "../Public.m3"
(*(ADDRESS*)(&Public_m_30_L_31))=(ADDRESS)(((ADDRESS)(a_L_25)));
#line 9 "../Public.m3"
 /* load */
#line 9 "../Public.m3"
/*check_nil*/if(!Public_m_30_L_31)Public_m_M_Public_L_23_CRASH(292);
#line 9 "../Public.m3"
 /* load */
#line 9 "../Public.m3"
 /* load_indirect */
#line 9 "../Public.m3"
 /* index_address */
#line 9 "../Public.m3"
 /* add_offset */
#line 9 "../Public.m3"
 /* pop_param */
#line 9 "../Public.m3"
 /* call_direct */
#line 9 "../Public.m3"
put_adr(
  ( Ctypes__char_star )(((ADDRESS)(Public_m_26_L_27)) ),
  ( INTEGER* /*TypeText1*/  )(((ADDRESS)(((8)+(char*)(((ADDRESS)((((ADDRESS)(Public_m_30_L_31))+( *((INT64*)(INT64_(112)+((ADDRESS)(*((ADDRESS*)(INT64_(176)+((ADDRESS)(&Public_m_M_Public_L_23)))))))))))))))) ));
#line 9 "../Public.m3"
 /* set_source_line */
#line 9 "../Public.m3"
#line 10 "../Public.m3"
 /* exit_proc */
#line 10 "../Public.m3"
return;
#line 10 "../Public.m3"
 /* end_procedure */
#line 10 "../Public.m3"
} /* Public_M3 */
#line 10 "../Public.m3"
 /* module main body Public_M3 */
#line 10 "../Public.m3"
 /* set_source_line */
#line 10 "../Public.m3"
#line 12 "../Public.m3"
 /* begin_procedure */
#line 12 "../Public.m3"
struct Public_M3_Frame_t {
#line 12 "../Public.m3"
ADDRESS _unused;
#line 12 "../Public.m3"
};
#line 12 "../Public.m3"
RT0__ModulePtr
__cdecl
Public_M3(
   /* Param_Type1 */ INTEGER /*TypeText1*/  mode_L_24)
{
#line 12 "../Public.m3"
Public_M3_Frame_t _frame;
#line 12 "../Public.m3"
_frame._unused=(ADDRESS)&_frame;
#line 12 "../Public.m3"
 /* load */
#line 12 "../Public.m3"
 /* if_true_or_false */
#line 12 "../Public.m3"
 /* load_host_integer */
#line 12 "../Public.m3"
 /* load_integer */
#line 12 "../Public.m3"
 /* if_compare */
#line 12 "../Public.m3"
if(m3_eq(INT64,
  mode_L_24,
   INT64_(0)))goto L1;
#line 12 "../Public.m3"
 /* set_label */
#line 12 "../Public.m3"
L1:;
#line 12 "../Public.m3"
 /* load_address */
#line 12 "../Public.m3"
 /* exit_proc */
#line 12 "../Public.m3"
return (RT0__ModulePtr)(&Public_m_M_Public_L_23);
#line 12 "../Public.m3"
 /* end_procedure */
#line 12 "../Public.m3"
} /* global constant type descriptor */
#line 12 "../Public.m3"
 /* global data type descriptor */
#line 12 "../Public.m3"
 /* module global constants */
#line 12 "../Public.m3"
 /* procedure names */
#line 12 "../Public.m3"
 /* procedure table */
#line 12 "../Public.m3"
 /* file name */
#line 12 "../Public.m3"
 /* module global data */
#line 12 "../Public.m3"
 /* load map


 global data allocation for M_Public
     0   104  8  *module info*
   104    24  8  import Public
   128    24  8  import M3toC
   152    24  8  import RTHooks
   176    16  8  typecell ptr
   192     0  8  *TOTAL*


 global constants for M_Public
     0    40  8  TEXT literal methods
    40    36  8  *TEXT literal*
    80    38  8  *TEXT literal*
   120    38  8  *TEXT literal*
   160    13  8  *proc names*
   176    40  8  *proc info*
   216    13  1  *string*
   232     0  8  *TOTAL*
 */
#line 12 "../Public.m3"
 /* end unit */
#line 12 "../Public.m3"

#ifdef __cplusplus

} /* extern "C" */
#endif
 /* set_runtime_proc */
 /* set_runtime_proc */
 /* set_runtime_proc */
