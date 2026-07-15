// library:pgm
// source_base_name:Main
// target_name:Main.m3.cpp
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
 /* declare_proctype */

#if 0 /* avoid type hash collions */
typedef 
void(__cdecl*T7B78C34F)(void);
#else
typedef void (__cdecl*T7B78C34F)(void);
#endif
 /* declare_record */
 /* declare_field */
 /* record_forwardDeclare Record_t{ typeid:T6981C397 text:NIL hash_text:T6981C397 base_text:NIL state:0} */
/*record_forwardDeclare*/struct T6981C397;typedef struct T6981C397 T6981C397;
 /* record_canBeDefined Record_t{ typeid:T6981C397 text:NIL hash_text:T6981C397 base_text:NIL state:0} */
 /* record_define Record_t{ typeid:T6981C397 text:NIL hash_text:T6981C397 base_text:NIL state:0} */

#ifndef T6981C397
#define T6981C397 T6981C397
/*record_define*/struct T6981C397{
INTEGER a;
};
#endif
 /* declare_proctype */
 /* declare_formal */
 /* declare_proctype */
 /* declare_formal */

#ifndef Main__P1
#define Main__P1 Main__P1
typedef T7B78C34F Main__P1;
#endif
 /* declare_indirect */
typedef INTEGER*TE6A3D58B;
 /* declare_pointer */
typedef INTEGER*T50C57D3A;
 /* declare_indirect */

#ifndef Main__R1
#define Main__R1 Main__R1
typedef T6981C397 Main__R1;
#endif
typedef Main__R1*T967E3C68;
 /* declare_pointer */
typedef T6981C397*T17F32A76;
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
 /* DeclareTypes_FlushOnce size:11 */

#if 0 /* avoid type hash collions */
typedef 
void(__cdecl*TD7A7A6F7)(INTEGER);
#else
typedef void (__cdecl*TD7A7A6F7)(void);
#endif

#if 0 /* avoid type hash collions */
typedef 
void(__cdecl*T28EF45CA)(Main__P1);
#else
typedef void (__cdecl*T28EF45CA)(void);
#endif

#if 0 /* avoid type hash collions */
typedef 
void(__cdecl*TC7C92523)(INTEGER,INTEGER,INTEGER*,INTEGER*,INTEGER*,INTEGER*,Main__R1,Main__R1,Main__R1*,Main__R1*,Main__R1*,Main__R1*);
#else
typedef void (__cdecl*TC7C92523)(void);
#endif

#if 0 /* avoid type hash collions */
typedef 
void(__cdecl*T27B0F303)(INTEGER,INTEGER);
#else
typedef void (__cdecl*T27B0F303)(void);
#endif

#if 0 /* avoid type hash collions */
typedef 
void(__cdecl*TABBEB60B)(TEXT);
#else
typedef void (__cdecl*TABBEB60B)(void);
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

#ifndef struct_24_t
#define struct_24_t struct_24_t
STRUCT8(24)
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
   /* Param_Type1 */ INTEGER /*TypeText1*/  mode_L_1);
 /* import_procedure */
/*Proc_ForwardDeclareFrameType*/struct RTIO_I3_Frame_t;typedef struct RTIO_I3_Frame_t RTIO_I3_Frame_t;
 /* internal_declare_param */
RT0__ModulePtr
__cdecl
RTIO_I3(
   /* Param_Type1 */ INTEGER /*TypeText1*/  mode_L_2);
 /* import_procedure */
/*Proc_ForwardDeclareFrameType*/struct RTHooks_I3_Frame_t;typedef struct RTHooks_I3_Frame_t RTHooks_I3_Frame_t;
 /* internal_declare_param */
RT0__ModulePtr
__cdecl
RTHooks_I3(
   /* Param_Type1 */ INTEGER /*TypeText1*/  mode_L_3);
 /* import_procedure */
/*Proc_ForwardDeclareFrameType*/struct RTIO__PutInt_Frame_t;typedef struct RTIO__PutInt_Frame_t RTIO__PutInt_Frame_t;
 /* internal_declare_param */
 /* internal_declare_param */
void /*TypeText3*/ 
__cdecl
RTIO__PutInt(
   /* Param_Type1 */ INTEGER i_L_4,
   /* Param_Type1 */ INTEGER width_L_5);
 /* import_procedure */
/*Proc_ForwardDeclareFrameType*/struct RTIO__PutText_Frame_t;typedef struct RTIO__PutText_Frame_t RTIO__PutText_Frame_t;
 /* internal_declare_param */
void /*TypeText3*/ 
__cdecl
RTIO__PutText(
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
 /* import_procedure */
/*Proc_ForwardDeclareFrameType*/struct RTIO__Flush_Frame_t;typedef struct RTIO__Flush_Frame_t RTIO__Flush_Frame_t;
void /*TypeText3*/ 
__cdecl
RTIO__Flush(void);
 /* end: imports */
 /* begin: locals */
 /* declare_segment name:<NIL> typeid:TFFFFFFFF const:TRUE */
/*declare_segment*/struct Main_m_21_L_22_t;
/*declare_segment*/typedef struct Main_m_21_L_22_t Main_m_21_L_22_t;
 /* declare_segment name:M_Main typeid:TFFFFFFFF const:FALSE */
 /* handler_name_prefixes:Main_M3_LINE_ */
 /* handler_name_prefixes:Main_I3_LINE_ */
/*declare_segment*/struct Main_m_M_Main_L_23_t;
/*declare_segment*/typedef struct Main_m_M_Main_L_23_t Main_m_M_Main_L_23_t;
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct Main_M3_Frame_t;typedef struct Main_M3_Frame_t Main_M3_Frame_t;
 /* internal_declare_param */
RT0__ModulePtr
__cdecl
Main_M3(
   /* Param_Type1 */ INTEGER /*TypeText1*/  mode_L_24);
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct Main__A_Frame_t;typedef struct Main__A_Frame_t Main__A_Frame_t;
 /* internal_declare_param */
void /*TypeText3*/ 
__cdecl
Main__A(
   /* Param_Type1 */ INTEGER a_L_25);
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct Main__F2_Frame_t;typedef struct Main__F2_Frame_t Main__F2_Frame_t;
 /* internal_declare_param */
void /*TypeText3*/ 
__cdecl
Main__F2(
   /* Param_Type1 */ Main__P1 p_L_26);
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct Main__Main_Frame_t;typedef struct Main__Main_Frame_t Main__Main_Frame_t;
 /* declare_local */
 /* declare_local */
 /* declare_local */
 /* declare_local */
 /* internal_declare_param */
 /* internal_declare_param */
 /* internal_declare_param */
 /* internal_declare_param */
 /* internal_declare_param */
 /* internal_declare_param */
 /* internal_declare_param */
 /* internal_declare_param */
 /* internal_declare_param */
 /* internal_declare_param */
 /* internal_declare_param */
 /* internal_declare_param */
 /* internal_declare_param */
void /*TypeText3*/ 
__cdecl
Main__Main(
   /* Param_Type1 */ Main__R1* /*TypeText1*/  _result_L_31,
   /* Param_Type1 */ INTEGER param_integer_L_32,
   /* Param_Type1 */ INTEGER param_integer_uplevel_L_33,
   /* Param_Type1 */ INTEGER* /*TypeText1*/  var_param_integer_L_34,
   /* Param_Type1 */ INTEGER* /*TypeText1*/  var_param_integer_uplevel_L_35,
   /* Param_Type1 */ INTEGER* /*TypeText1*/  readonly_param_integer_L_36,
   /* Param_Type1 */ INTEGER* /*TypeText1*/  readonly_param_integer_uplevel_L_37,
   /* Param_Type1 */ Main__R1*_param_struct_pointer_param_record_L_38,
   /* Param_Type1 */ Main__R1*_param_struct_pointer_param_record_uplevel_L_39,
   /* Param_Type1 */ Main__R1* /*TypeText1*/  var_param_record_L_40,
   /* Param_Type1 */ Main__R1* /*TypeText1*/  var_param_record_uplevel_L_41,
   /* Param_Type1 */ Main__R1* /*TypeText1*/  readonly_param_record_L_42,
   /* Param_Type1 */ Main__R1* /*TypeText1*/  readonly_param_record_uplevel_L_43);
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct Main__Main__F1_Frame_t;typedef struct Main__Main__F1_Frame_t Main__Main__F1_Frame_t;
 /* internal_declare_param */
void /*TypeText3*/ 
__cdecl
Main__Main__F1(
   /* Param_Type1 */ Main__Main_Frame_t* _static_link);
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
 /* AllocateTemps_check_nil */
 /* AllocateTemps_common */
 /* declare_temp */
 /* declare_local */
 /* Locals_end_procedure */
 /* end_block */
 /* Locals_begin_procedure */
 /* begin_block */
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
 /* declare_temp */
 /* declare_local */
 /* Locals_end_procedure */
 /* end_block */
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct Main_M3_t17f32a76_INIT_Frame_t;typedef struct Main_M3_t17f32a76_INIT_Frame_t Main_M3_t17f32a76_INIT_Frame_t;
 /* internal_declare_param */
void /*TypeText3*/ 
__cdecl
Main_M3_t17f32a76_INIT(
   /* Param_Type1 */ T6981C397* /*TypeText1*/  Main_m_56_L_57);
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
 /* init_int */
 /* init_int */
 /* init_int */
 /* init_int */
 /* init_int */
 /* init_int */
 /* end_init */
struct Main_m_21_L_22_t{ADDRESS L_58[5];
INT64 L_59[1];
ADDRESS L_60[1];
INT64 L_61[1];
UINT8 L_62[1];
char L_63[7];
INT64 L_64[2];
UINT8 L_65[7];
char L_66[1];
UINT8 L_67[4];
char L_68[1];
UINT8 L_69[7];
char L_70[1];
UINT8 L_71[2];
char L_72[1];
UINT8 L_73[1];
char L_74[7];
ADDRESS L_75[10];
char L_76[8];
UINT8 L_77[10];
char L_78[1];
INT8 L_79[6];
char L_80[15];
};
static  const Main_m_21_L_22_t Main_m_21_L_22={{(ADDRESS)&RTHooks__TextLitInfo,(ADDRESS)&RTHooks__TextLitGetChar,(ADDRESS)&RTHooks__TextLitGetWideChar,(ADDRESS)&RTHooks__TextLitGetChars,(ADDRESS)&RTHooks__TextLitGetWideChars},{INT64_(2)},{(char*)&Main_m_21_L_22},{INT64_(1)},{10},{0 /* 1 */ ,0 /* 2 */ ,0 /* 3 */ ,0 /* 4 */ ,0 /* 5 */ ,0 /* 6 */ ,0 /* 7 */ ,},{INT64_(999),INT64_(999)},{'M','a','i','n','_','M','3'},{0 /* 1 */ ,},{'M','a','i','n'},{0 /* 1 */ ,},{'M','a','i','n','.','F','1'},{0 /* 1 */ ,},{'F','2'},{0 /* 1 */ ,},{'A'},{0 /* 1 */ ,0 /* 2 */ ,0 /* 3 */ ,0 /* 4 */ ,0 /* 5 */ ,0 /* 6 */ ,0 /* 7 */ ,},{(ADDRESS)&Main_M3,88+(char*)&Main_m_21_L_22,(ADDRESS)&Main__Main,96+(char*)&Main_m_21_L_22,(ADDRESS)&Main__Main__F1,101+(char*)&Main_m_21_L_22,(ADDRESS)&Main__F2,109+(char*)&Main_m_21_L_22,(ADDRESS)&Main__A,112+(char*)&Main_m_21_L_22},{0 /* 1 */ ,0 /* 2 */ ,0 /* 3 */ ,0 /* 4 */ ,0 /* 5 */ ,0 /* 6 */ ,0 /* 7 */ ,0 /* 8 */ ,},{'.','.','/','M','a','i','n','.','m','3'},{0 /* 1 */ ,},{
((INT8)15),((INT8)0),((INT8)2),((INT8)17),((INT8)1),((INT8)7)},{0 /* 1 */ ,0 /* 2 */ ,0 /* 3 */ ,0 /* 4 */ ,0 /* 5 */ ,0 /* 6 */ ,0 /* 7 */ ,0 /* 8 */ ,0 /* 9 */ ,0 /* 10 */ ,0 /* 11 */ ,0 /* 12 */ ,0 /* 13 */ ,0 /* 14 */ ,0 /* 15 */ ,}};
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
 /* init_proc */
 /* init_var */
 /* init_proc */
 /* init_var */
 /* init_proc */
 /* end_init */
struct Main_m_M_Main_L_23_t{ADDRESS L_81[2];
char L_82[24];
ADDRESS L_83[1];
char L_84[24];
ADDRESS L_85[1];
char L_86[8];
ADDRESS L_87[1];
INT64 L_88[1];
char L_89[8];
INT64 L_90[1];
INT8 L_91[2];
UINT8 L_92[1];
INT8 L_93[7];
char L_94[1];
INT8 L_95[1];
char L_96[4];
INT64 L_97[1];
ADDRESS L_98[1];
char L_99[8];
ADDRESS L_100[2];
char L_101[24];
INT64 L_102[12];
char L_103[8];
ADDRESS L_104[2];
char L_105[8];
ADDRESS L_106[2];
char L_107[8];
ADDRESS L_108[1];
char L_109[8];
};
static Main_m_M_Main_L_23_t Main_m_M_Main_L_23={{208+(char*)&Main_m_21_L_22,104+(char*)&Main_m_M_Main_L_23},{0 /* 1 */ ,0 /* 2 */ ,0 /* 3 */ ,0 /* 4 */ ,0 /* 5 */ ,0 /* 6 */ ,0 /* 7 */ ,0 /* 8 */ ,0 /* 9 */ ,0 /* 10 */ ,0 /* 11 */ ,0 /* 12 */ ,0 /* 13 */ ,0 /* 14 */ ,0 /* 15 */ ,0 /* 16 */ ,0 /* 17 */ ,0 /* 18 */ ,0 /* 19 */ ,0 /* 20 */ ,0 /* 21 */ ,0 /* 22 */ ,0 /* 23 */ ,0 /* 24 */ ,},{120+(char*)&Main_m_21_L_22},{0 /* 1 */ ,0 /* 2 */ ,0 /* 3 */ ,0 /* 4 */ ,0 /* 5 */ ,0 /* 6 */ ,0 /* 7 */ ,0 /* 8 */ ,0 /* 9 */ ,0 /* 10 */ ,0 /* 11 */ ,0 /* 12 */ ,0 /* 13 */ ,0 /* 14 */ ,0 /* 15 */ ,0 /* 16 */ ,0 /* 17 */ ,0 /* 18 */ ,0 /* 19 */ ,0 /* 20 */ ,0 /* 21 */ ,0 /* 22 */ ,0 /* 23 */ ,0 /* 24 */ ,},{296+(char*)&Main_m_M_Main_L_23},{0 /* 1 */ ,0 /* 2 */ ,0 /* 3 */ ,0 /* 4 */ ,0 /* 5 */ ,0 /* 6 */ ,0 /* 7 */ ,0 /* 8 */ ,},{(ADDRESS)&Main_M3},{INT64_(3)},{0 /* 1 */ ,0 /* 2 */ ,0 /* 3 */ ,0 /* 4 */ ,0 /* 5 */ ,0 /* 6 */ ,0 /* 7 */ ,0 /* 8 */ ,},{INT64_(401812086)},{((INT8)41),((INT8)88)},{234U},{
((INT8)20),((INT8)95),((INT8)114),((INT8)25),((INT8)3),((INT8)1),((INT8)1)},{0 /* 1 */ ,},{((INT8)8)},{0 /* 1 */ ,0 /* 2 */ ,0 /* 3 */ ,0 /* 4 */ ,},{INT64_(8)},{219+(char*)&Main_m_21_L_22},{0 /* 1 */ ,0 /* 2 */ ,0 /* 3 */ ,0 /* 4 */ ,0 /* 5 */ ,0 /* 6 */ ,0 /* 7 */ ,0 /* 8 */ ,},{221+(char*)&Main_m_21_L_22,(ADDRESS)&Main_M3_t17f32a76_INIT},{0 /* 1 */ ,0 /* 2 */ ,0 /* 3 */ ,0 /* 4 */ ,0 /* 5 */ ,0 /* 6 */ ,0 /* 7 */ ,0 /* 8 */ ,0 /* 9 */ ,0 /* 10 */ ,0 /* 11 */ ,0 /* 12 */ ,0 /* 13 */ ,0 /* 14 */ ,0 /* 15 */ ,0 /* 16 */ ,0 /* 17 */ ,0 /* 18 */ ,0 /* 19 */ ,0 /* 20 */ ,0 /* 21 */ ,0 /* 22 */ ,0 /* 23 */ ,0 /* 24 */ ,},{INT64_(1000),INT64_(2000),INT64_(3000),INT64_(4000),INT64_(5000),INT64_(6000),INT64_(7000),INT64_(8000),INT64_(9000),INT64_(10000),INT64_(11000),INT64_(12000)},{0 /* 1 */ ,0 /* 2 */ ,0 /* 3 */ ,0 /* 4 */ ,0 /* 5 */ ,0 /* 6 */ ,0 /* 7 */ ,0 /* 8 */ ,},{(ADDRESS)&Main_I3,320+(char*)&Main_m_M_Main_L_23},{0 /* 1 */ ,0 /* 2 */ ,0 /* 3 */ ,0 /* 4 */ ,0 /* 5 */ ,0 /* 6 */ ,0 /* 7 */ 
,0 /* 8 */ ,},{(ADDRESS)&RTIO_I3,344+(char*)&Main_m_M_Main_L_23},{0 /* 1 */ ,0 /* 2 */ ,0 /* 3 */ ,0 /* 4 */ ,0 /* 5 */ ,0 /* 6 */ ,0 /* 7 */ ,0 /* 8 */ ,},{(ADDRESS)&RTHooks_I3},{0 /* 1 */ ,0 /* 2 */ ,0 /* 3 */ ,0 /* 4 */ ,0 /* 5 */ ,0 /* 6 */ ,0 /* 7 */ ,0 /* 8 */ ,}};
static void __cdecl Main_m_M_Main_L_23_CRASH(WORD_T code) M3_ATTRIBUTE_NO_RETURN;
static void __cdecl Main_m_M_Main_L_23_CRASH(WORD_T code){RTHooks__ReportFault((ADDRESS)&Main_m_M_Main_L_23,code);} /* end: segments/globals */
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
#line 100 "../Main.m3"
 /* A */
#line 100 "../Main.m3"
 /* set_source_line */
#line 100 "../Main.m3"
#line 7 "../Main.m3"
 /* begin_procedure */
#line 7 "../Main.m3"
struct Main__A_Frame_t {
#line 7 "../Main.m3"
ADDRESS _unused;
#line 7 "../Main.m3"
};
#line 7 "../Main.m3"
void /*TypeText3*/ 
__cdecl
Main__A(
   /* Param_Type1 */ INTEGER a_L_25)
{
#line 7 "../Main.m3"
Main__A_Frame_t _frame;
#line 7 "../Main.m3"
_frame._unused=(ADDRESS)&_frame;
#line 7 "../Main.m3"
 /* set_source_line */
#line 7 "../Main.m3"
#line 8 "../Main.m3"
 /* set_source_line */
#line 8 "../Main.m3"
#line 9 "../Main.m3"
 /* start_call_direct */
#line 9 "../Main.m3"
 /* load */
#line 9 "../Main.m3"
 /* pop_param */
#line 9 "../Main.m3"
 /* load_integer */
#line 9 "../Main.m3"
 /* pop_param */
#line 9 "../Main.m3"
 /* call_direct */
#line 9 "../Main.m3"
RTIO__PutInt(
  ( INTEGER )( a_L_25 ),
  ( INTEGER )(  INT64_(0) ));
#line 9 "../Main.m3"
 /* set_source_line */
#line 9 "../Main.m3"
#line 10 "../Main.m3"
 /* start_call_direct */
#line 10 "../Main.m3"
 /* load_address */
#line 10 "../Main.m3"
 /* pop_param */
#line 10 "../Main.m3"
 /* call_direct */
#line 10 "../Main.m3"
RTIO__PutText(
  ( TEXT )(((ADDRESS)(INT64_(48)+((ADDRESS)(&Main_m_21_L_22)))) ));
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
} /* F2 */
#line 11 "../Main.m3"
 /* set_source_line */
#line 11 "../Main.m3"
#line 13 "../Main.m3"
 /* begin_procedure */
#line 13 "../Main.m3"
struct Main__F2_Frame_t {
#line 13 "../Main.m3"
ADDRESS _unused;
#line 13 "../Main.m3"
};
#line 13 "../Main.m3"
void /*TypeText3*/ 
__cdecl
Main__F2(
   /* Param_Type1 */ Main__P1 p_L_26)
{
#line 13 "../Main.m3"
 /* Var_Type2 */ ADDRESS Main_m_44_L_45={0};//always-init
#line 13 "../Main.m3"
 /* Var_Type2 */ ADDRESS Main_m_46_L_47={0};//always-init
#line 13 "../Main.m3"
 /* Var_Type2 */ ADDRESS Main_m_48_L_49={0};//always-init
#line 13 "../Main.m3"
Main__F2_Frame_t _frame;
#line 13 "../Main.m3"
_frame._unused=(ADDRESS)&_frame;
#line 13 "../Main.m3"
 /* set_source_line */
#line 13 "../Main.m3"
#line 14 "../Main.m3"
 /* set_source_line */
#line 14 "../Main.m3"
#line 15 "../Main.m3"
 /* load */
#line 15 "../Main.m3"
 /* store */
#line 15 "../Main.m3"
(*(ADDRESS*)(&Main_m_44_L_45))=(ADDRESS)(((ADDRESS)(p_L_26)));
#line 15 "../Main.m3"
 /* start_call_indirect */
#line 15 "../Main.m3"
 /* load */
#line 15 "../Main.m3"
 /* loophole */
#line 15 "../Main.m3"
 /* load_integer */
#line 15 "../Main.m3"
 /* and */
#line 15 "../Main.m3"
 /* load_integer */
#line 15 "../Main.m3"
 /* if_compare */
#line 15 "../Main.m3"
if(m3_ne(INT64,
  ((INT64)(((INT64)((INT64)Main_m_44_L_45))&  INT64_(7))),
   INT64_(0)))goto L1;
#line 15 "../Main.m3"
 /* load */
#line 15 "../Main.m3"
 /* load_nil */
#line 15 "../Main.m3"
 /* if_compare */
#line 15 "../Main.m3"
if(m3_eq(ADDRESS,
 ((ADDRESS)(Main_m_44_L_45)),
 ((ADDRESS)(0))))goto L1;
#line 15 "../Main.m3"
 /* load */
#line 15 "../Main.m3"
 /* load_indirect */
#line 15 "../Main.m3"
 /* load_integer */
#line 15 "../Main.m3"
 /* if_compare */
#line 15 "../Main.m3"
if(m3_ne(INT64,
  *((INT64*)(Main_m_44_L_45)),
   INT64_(-1)))goto L1;
#line 15 "../Main.m3"
 /* set_label */
#line 15 "../Main.m3"
 /* load */
#line 15 "../Main.m3"
 /* load_indirect */
#line 15 "../Main.m3"
 /* pop_static_link */
#line 15 "../Main.m3"
 /* store */
#line 15 "../Main.m3"
(*(ADDRESS*)(&Main_m_46_L_47))=(ADDRESS)(((ADDRESS)(*((ADDRESS*)(INT64_(16)+((ADDRESS)(Main_m_44_L_45)))))));
#line 15 "../Main.m3"
 /* load */
#line 15 "../Main.m3"
 /* load_indirect */
#line 15 "../Main.m3"
 /* store */
#line 15 "../Main.m3"
(*(ADDRESS*)(&Main_m_44_L_45))=(ADDRESS)(((ADDRESS)(*((ADDRESS*)(INT64_(8)+((ADDRESS)(Main_m_44_L_45)))))));
#line 15 "../Main.m3"
 /* set_label */
#line 15 "../Main.m3"
L1:;
#line 15 "../Main.m3"
 /* load */
#line 15 "../Main.m3"
 /* check_nil */
#line 15 "../Main.m3"
 /* store */
#line 15 "../Main.m3"
(*(ADDRESS*)(&Main_m_48_L_49))=(ADDRESS)(((ADDRESS)(Main_m_44_L_45)));
#line 15 "../Main.m3"
 /* load */
#line 15 "../Main.m3"
/*check_nil*/if(!Main_m_48_L_49)Main_m_M_Main_L_23_CRASH(484);
#line 15 "../Main.m3"
 /* call_indirect */
#line 15 "../Main.m3"
 /* free_temp */
#line 15 "../Main.m3"
((void (__cdecl*)(void*))Main_m_48_L_49)(
 Main_m_46_L_47);
#line 15 "../Main.m3"
 /* set_source_line */
#line 15 "../Main.m3"
#line 16 "../Main.m3"
 /* exit_proc */
#line 16 "../Main.m3"
return;
#line 16 "../Main.m3"
 /* end_procedure */
#line 16 "../Main.m3"
} /* Main */
#line 16 "../Main.m3"
 /* set_source_line */
#line 16 "../Main.m3"
#line 18 "../Main.m3"
 /* begin_procedure */
#line 18 "../Main.m3"
struct Main__Main_Frame_t {
#line 18 "../Main.m3"
ADDRESS _unused;
#line 18 "../Main.m3"
 /* Var_Type1 */ INTEGER local_integer_uplevel_L_28;
#line 18 "../Main.m3"
 /* Var_Type1 */ T6981C397 local_record_uplevel_L_30;
#line 18 "../Main.m3"
 /* Var_Type1 */ INTEGER param_integer_uplevel_L_33;
#line 18 "../Main.m3"
 /* Var_Type1 */ INTEGER* /*TypeText1*/  var_param_integer_uplevel_L_35;
#line 18 "../Main.m3"
 /* Var_Type1 */ INTEGER* /*TypeText1*/  readonly_param_integer_uplevel_L_37;
#line 18 "../Main.m3"
 /* Var_Type1 */ Main__R1 param_record_uplevel_L_39;
#line 18 "../Main.m3"
 /* Var_Type1 */ Main__R1* /*TypeText1*/  var_param_record_uplevel_L_41;
#line 18 "../Main.m3"
 /* Var_Type1 */ Main__R1* /*TypeText1*/  readonly_param_record_uplevel_L_43;
#line 18 "../Main.m3"
};
#line 18 "../Main.m3"
void /*TypeText3*/ 
__cdecl
Main__Main(
   /* Param_Type1 */ Main__R1* /*TypeText1*/  _result_L_31,
   /* Param_Type1 */ INTEGER param_integer_L_32,
   /* Param_Type1 */ INTEGER param_integer_uplevel_L_33,
   /* Param_Type1 */ INTEGER* /*TypeText1*/  var_param_integer_L_34,
   /* Param_Type1 */ INTEGER* /*TypeText1*/  var_param_integer_uplevel_L_35,
   /* Param_Type1 */ INTEGER* /*TypeText1*/  readonly_param_integer_L_36,
   /* Param_Type1 */ INTEGER* /*TypeText1*/  readonly_param_integer_uplevel_L_37,
   /* Param_Type1 */ Main__R1*_param_struct_pointer_param_record_L_38,
   /* Param_Type1 */ Main__R1*_param_struct_pointer_param_record_uplevel_L_39,
   /* Param_Type1 */ Main__R1* /*TypeText1*/  var_param_record_L_40,
   /* Param_Type1 */ Main__R1* /*TypeText1*/  var_param_record_uplevel_L_41,
   /* Param_Type1 */ Main__R1* /*TypeText1*/  readonly_param_record_L_42,
   /* Param_Type1 */ Main__R1* /*TypeText1*/  readonly_param_record_uplevel_L_43)
{
#line 18 "../Main.m3"
 /* Var_Type1 */ INTEGER local_integer_L_27={0};//always-init
#line 18 "../Main.m3"
 /* Var_Type1 */ T6981C397 local_record_L_29={0};//always-init
#line 18 "../Main.m3"
 /* Var_Type3 */ STRUCT(24) Main_m_52_L_53={0};//always-init
#line 18 "../Main.m3"
 /* Var_Type1 */ Main__R1 param_record_L_38;
#line 18 "../Main.m3"
Main__Main_Frame_t _frame;
#line 18 "../Main.m3"
_frame.param_integer_uplevel_L_33=param_integer_uplevel_L_33;
#line 18 "../Main.m3"
_frame.var_param_integer_uplevel_L_35=var_param_integer_uplevel_L_35;
#line 18 "../Main.m3"
_frame.readonly_param_integer_uplevel_L_37=readonly_param_integer_uplevel_L_37;
#line 18 "../Main.m3"
_frame.param_record_uplevel_L_39=*_param_struct_pointer_param_record_uplevel_L_39;
#line 18 "../Main.m3"
_frame.var_param_record_uplevel_L_41=var_param_record_uplevel_L_41;
#line 18 "../Main.m3"
_frame.readonly_param_record_uplevel_L_43=readonly_param_record_uplevel_L_43;
#line 18 "../Main.m3"
_frame._unused=(ADDRESS)&_frame;
#line 18 "../Main.m3"
param_record_L_38=*_param_struct_pointer_param_record_L_38;
#line 18 "../Main.m3"
 /* set_source_line */
#line 18 "../Main.m3"
#line 35 "../Main.m3"
 /* load_integer */
#line 35 "../Main.m3"
 /* store */
#line 35 "../Main.m3"
(*(INT64*)(&local_record_L_29))=(INT64)(  INT64_(999));
#line 35 "../Main.m3"
 /* set_source_line */
#line 35 "../Main.m3"
#line 36 "../Main.m3"
 /* load_integer */
#line 36 "../Main.m3"
 /* store */
#line 36 "../Main.m3"
(*(INT64*)(&_frame.local_record_uplevel_L_30))=(INT64)(  INT64_(999));
#line 36 "../Main.m3"
 /* note_procedure_origin */
#line 36 "../Main.m3"
 /* set_source_line */
#line 36 "../Main.m3"
#line 32 "../Main.m3"
 /* set_source_line */
#line 32 "../Main.m3"
#line 47 "../Main.m3"
 /* load */
#line 47 "../Main.m3"
 /* load_indirect */
#line 47 "../Main.m3"
 /* load_integer */
#line 47 "../Main.m3"
 /* add */
#line 47 "../Main.m3"
 /* load */
#line 47 "../Main.m3"
 /* add */
#line 47 "../Main.m3"
 /* store */
#line 47 "../Main.m3"
(*(INT64*)(&_frame.param_integer_uplevel_L_33))=(INT64)( ((INT64)( ((INT64)( *((INT64*)(_frame.readonly_param_integer_uplevel_L_37))+  INT64_(1)))+ _frame.param_integer_uplevel_L_33)));
#line 47 "../Main.m3"
 /* set_source_line */
#line 47 "../Main.m3"
#line 48 "../Main.m3"
 /* load */
#line 48 "../Main.m3"
 /* load_indirect */
#line 48 "../Main.m3"
 /* load */
#line 48 "../Main.m3"
 /* load_indirect */
#line 48 "../Main.m3"
 /* load_integer */
#line 48 "../Main.m3"
 /* add */
#line 48 "../Main.m3"
 /* add */
#line 48 "../Main.m3"
 /* load */
#line 48 "../Main.m3"
 /* swap */
#line 48 "../Main.m3"
 /* store_indirect */
#line 48 "../Main.m3"
(*(INT64*)(_frame.var_param_integer_uplevel_L_35))=(INT64)( ((INT64)( *((INT64*)(_frame.var_param_integer_uplevel_L_35))+ ((INT64)( *((INT64*)(_frame.readonly_param_integer_uplevel_L_37))+  INT64_(2))))));
#line 48 "../Main.m3"
 /* set_source_line */
#line 48 "../Main.m3"
#line 49 "../Main.m3"
 /* load */
#line 49 "../Main.m3"
 /* load_indirect */
#line 49 "../Main.m3"
 /* load_integer */
#line 49 "../Main.m3"
 /* add */
#line 49 "../Main.m3"
 /* load */
#line 49 "../Main.m3"
 /* add */
#line 49 "../Main.m3"
 /* store */
#line 49 "../Main.m3"
(*(INT64*)(&_frame.param_record_uplevel_L_39))=(INT64)( ((INT64)( ((INT64)( *((INT64*)(_frame.readonly_param_record_uplevel_L_43))+  INT64_(3)))+((INT64)(*((INT64*)(&_frame.param_record_uplevel_L_39)))))));
#line 49 "../Main.m3"
 /* set_source_line */
#line 49 "../Main.m3"
#line 50 "../Main.m3"
 /* load */
#line 50 "../Main.m3"
 /* load_indirect */
#line 50 "../Main.m3"
 /* load */
#line 50 "../Main.m3"
 /* load_indirect */
#line 50 "../Main.m3"
 /* load_integer */
#line 50 "../Main.m3"
 /* add */
#line 50 "../Main.m3"
 /* add */
#line 50 "../Main.m3"
 /* load */
#line 50 "../Main.m3"
 /* swap */
#line 50 "../Main.m3"
 /* store_indirect */
#line 50 "../Main.m3"
(*(INT64*)(_frame.var_param_record_uplevel_L_41))=(INT64)( ((INT64)( *((INT64*)(_frame.var_param_record_uplevel_L_41))+ ((INT64)( *((INT64*)(_frame.readonly_param_record_uplevel_L_43))+  INT64_(4))))));
#line 50 "../Main.m3"
 /* set_source_line */
#line 50 "../Main.m3"
#line 51 "../Main.m3"
 /* load_integer */
#line 51 "../Main.m3"
 /* load */
#line 51 "../Main.m3"
 /* add */
#line 51 "../Main.m3"
 /* store */
#line 51 "../Main.m3"
(*(INT64*)(&_frame.local_integer_uplevel_L_28))=(INT64)( ((INT64)(  INT64_(5)+ _frame.local_integer_uplevel_L_28)));
#line 51 "../Main.m3"
 /* set_source_line */
#line 51 "../Main.m3"
#line 52 "../Main.m3"
 /* load_integer */
#line 52 "../Main.m3"
 /* load */
#line 52 "../Main.m3"
 /* add */
#line 52 "../Main.m3"
 /* store */
#line 52 "../Main.m3"
(*(INT64*)(&_frame.local_record_uplevel_L_30))=(INT64)( ((INT64)(  INT64_(6)+((INT64)(*((INT64*)(&_frame.local_record_uplevel_L_30)))))));
#line 52 "../Main.m3"
 /* set_source_line */
#line 52 "../Main.m3"
#line 53 "../Main.m3"
 /* load */
#line 53 "../Main.m3"
 /* load_indirect */
#line 53 "../Main.m3"
 /* load_integer */
#line 53 "../Main.m3"
 /* add */
#line 53 "../Main.m3"
 /* load */
#line 53 "../Main.m3"
 /* add */
#line 53 "../Main.m3"
 /* store */
#line 53 "../Main.m3"
(*(INT64*)(&param_integer_L_32))=(INT64)( ((INT64)( ((INT64)( *((INT64*)(readonly_param_integer_L_36))+  INT64_(7)))+ param_integer_L_32)));
#line 53 "../Main.m3"
 /* set_source_line */
#line 53 "../Main.m3"
#line 54 "../Main.m3"
 /* load */
#line 54 "../Main.m3"
 /* load_indirect */
#line 54 "../Main.m3"
 /* load */
#line 54 "../Main.m3"
 /* load_indirect */
#line 54 "../Main.m3"
 /* load_integer */
#line 54 "../Main.m3"
 /* add */
#line 54 "../Main.m3"
 /* add */
#line 54 "../Main.m3"
 /* load */
#line 54 "../Main.m3"
 /* swap */
#line 54 "../Main.m3"
 /* store_indirect */
#line 54 "../Main.m3"
(*(INT64*)(var_param_integer_L_34))=(INT64)( ((INT64)( *((INT64*)(var_param_integer_L_34))+ ((INT64)( *((INT64*)(readonly_param_integer_L_36))+  INT64_(8))))));
#line 54 "../Main.m3"
 /* set_source_line */
#line 54 "../Main.m3"
#line 55 "../Main.m3"
 /* load */
#line 55 "../Main.m3"
 /* load_indirect */
#line 55 "../Main.m3"
 /* load_integer */
#line 55 "../Main.m3"
 /* add */
#line 55 "../Main.m3"
 /* load */
#line 55 "../Main.m3"
 /* add */
#line 55 "../Main.m3"
 /* store */
#line 55 "../Main.m3"
(*(INT64*)(&param_record_L_38))=(INT64)( ((INT64)( ((INT64)( *((INT64*)(readonly_param_record_L_42))+  INT64_(9)))+((INT64)(*((INT64*)(&param_record_L_38)))))));
#line 55 "../Main.m3"
 /* set_source_line */
#line 55 "../Main.m3"
#line 56 "../Main.m3"
 /* load */
#line 56 "../Main.m3"
 /* load_indirect */
#line 56 "../Main.m3"
 /* load */
#line 56 "../Main.m3"
 /* load_indirect */
#line 56 "../Main.m3"
 /* load_integer */
#line 56 "../Main.m3"
 /* add */
#line 56 "../Main.m3"
 /* add */
#line 56 "../Main.m3"
 /* load */
#line 56 "../Main.m3"
 /* swap */
#line 56 "../Main.m3"
 /* store_indirect */
#line 56 "../Main.m3"
(*(INT64*)(var_param_record_L_40))=(INT64)( ((INT64)( *((INT64*)(var_param_record_L_40))+ ((INT64)( *((INT64*)(readonly_param_record_L_42))+  INT64_(10))))));
#line 56 "../Main.m3"
 /* set_source_line */
#line 56 "../Main.m3"
#line 57 "../Main.m3"
 /* load_integer */
#line 57 "../Main.m3"
 /* load */
#line 57 "../Main.m3"
 /* add */
#line 57 "../Main.m3"
 /* store */
#line 57 "../Main.m3"
(*(INT64*)(&local_integer_L_27))=(INT64)( ((INT64)(  INT64_(11)+ local_integer_L_27)));
#line 57 "../Main.m3"
 /* set_source_line */
#line 57 "../Main.m3"
#line 58 "../Main.m3"
 /* load_integer */
#line 58 "../Main.m3"
 /* load */
#line 58 "../Main.m3"
 /* add */
#line 58 "../Main.m3"
 /* store */
#line 58 "../Main.m3"
(*(INT64*)(&local_record_L_29))=(INT64)( ((INT64)(  INT64_(12)+((INT64)(*((INT64*)(&local_record_L_29)))))));
#line 58 "../Main.m3"
 /* set_source_line */
#line 58 "../Main.m3"
#line 59 "../Main.m3"
 /* start_call_direct */
#line 59 "../Main.m3"
 /* call_direct */
#line 59 "../Main.m3"
 /* get_static_link */
#line 59 "../Main.m3"
Main__Main__F1(
  ( Main__Main_Frame_t* )(&_frame ));
#line 59 "../Main.m3"
 /* set_source_line */
#line 59 "../Main.m3"
#line 60 "../Main.m3"
 /* start_call_direct */
#line 60 "../Main.m3"
 /* load_procedure */
#line 60 "../Main.m3"
 /* store */
#line 60 "../Main.m3"
(*(ADDRESS*)((8)+(char*)(&Main_m_52_L_53)))=(ADDRESS)(((ADDRESS)(Main__Main__F1)));
#line 60 "../Main.m3"
 /* load_integer */
#line 60 "../Main.m3"
 /* store */
#line 60 "../Main.m3"
(*(INT64*)(&Main_m_52_L_53))=(INT64)(  INT64_(-1));
#line 60 "../Main.m3"
 /* load_static_link */
#line 60 "../Main.m3"
 /* get_static_link */
#line 60 "../Main.m3"
 /* store */
#line 60 "../Main.m3"
(*(ADDRESS*)((16)+(char*)(&Main_m_52_L_53)))=(ADDRESS)(((ADDRESS)(&_frame)));
#line 60 "../Main.m3"
 /* load_address */
#line 60 "../Main.m3"
 /* pop_param */
#line 60 "../Main.m3"
 /* call_direct */
#line 60 "../Main.m3"
Main__F2(
  ( Main__P1 )(((ADDRESS)(&Main_m_52_L_53)) ));
#line 60 "../Main.m3"
 /* set_source_line */
#line 60 "../Main.m3"
#line 62 "../Main.m3"
 /* start_call_direct */
#line 62 "../Main.m3"
 /* load */
#line 62 "../Main.m3"
 /* pop_param */
#line 62 "../Main.m3"
 /* call_direct */
#line 62 "../Main.m3"
Main__A(
  ( INTEGER )( param_integer_L_32 ));
#line 62 "../Main.m3"
 /* set_source_line */
#line 62 "../Main.m3"
#line 63 "../Main.m3"
 /* start_call_direct */
#line 63 "../Main.m3"
 /* load */
#line 63 "../Main.m3"
 /* pop_param */
#line 63 "../Main.m3"
 /* call_direct */
#line 63 "../Main.m3"
Main__A(
  ( INTEGER )( _frame.param_integer_uplevel_L_33 ));
#line 63 "../Main.m3"
 /* set_source_line */
#line 63 "../Main.m3"
#line 64 "../Main.m3"
 /* start_call_direct */
#line 64 "../Main.m3"
 /* load */
#line 64 "../Main.m3"
 /* load_indirect */
#line 64 "../Main.m3"
 /* pop_param */
#line 64 "../Main.m3"
 /* call_direct */
#line 64 "../Main.m3"
Main__A(
  ( INTEGER )( *((INT64*)(var_param_integer_L_34)) ));
#line 64 "../Main.m3"
 /* set_source_line */
#line 64 "../Main.m3"
#line 65 "../Main.m3"
 /* start_call_direct */
#line 65 "../Main.m3"
 /* load */
#line 65 "../Main.m3"
 /* load_indirect */
#line 65 "../Main.m3"
 /* pop_param */
#line 65 "../Main.m3"
 /* call_direct */
#line 65 "../Main.m3"
Main__A(
  ( INTEGER )( *((INT64*)(_frame.var_param_integer_uplevel_L_35)) ));
#line 65 "../Main.m3"
 /* set_source_line */
#line 65 "../Main.m3"
#line 66 "../Main.m3"
 /* start_call_direct */
#line 66 "../Main.m3"
 /* load */
#line 66 "../Main.m3"
 /* load_indirect */
#line 66 "../Main.m3"
 /* pop_param */
#line 66 "../Main.m3"
 /* call_direct */
#line 66 "../Main.m3"
Main__A(
  ( INTEGER )( *((INT64*)(readonly_param_integer_L_36)) ));
#line 66 "../Main.m3"
 /* set_source_line */
#line 66 "../Main.m3"
#line 67 "../Main.m3"
 /* start_call_direct */
#line 67 "../Main.m3"
 /* load */
#line 67 "../Main.m3"
 /* load_indirect */
#line 67 "../Main.m3"
 /* pop_param */
#line 67 "../Main.m3"
 /* call_direct */
#line 67 "../Main.m3"
Main__A(
  ( INTEGER )( *((INT64*)(_frame.readonly_param_integer_uplevel_L_37)) ));
#line 67 "../Main.m3"
 /* set_source_line */
#line 67 "../Main.m3"
#line 68 "../Main.m3"
 /* start_call_direct */
#line 68 "../Main.m3"
 /* load */
#line 68 "../Main.m3"
 /* pop_param */
#line 68 "../Main.m3"
 /* call_direct */
#line 68 "../Main.m3"
Main__A(
  ( INTEGER )(((INT64)(*((INT64*)(&param_record_L_38)))) ));
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
 /* call_direct */
#line 69 "../Main.m3"
Main__A(
  ( INTEGER )(((INT64)(*((INT64*)(&_frame.param_record_uplevel_L_39)))) ));
#line 69 "../Main.m3"
 /* set_source_line */
#line 69 "../Main.m3"
#line 70 "../Main.m3"
 /* start_call_direct */
#line 70 "../Main.m3"
 /* load */
#line 70 "../Main.m3"
 /* load_indirect */
#line 70 "../Main.m3"
 /* pop_param */
#line 70 "../Main.m3"
 /* call_direct */
#line 70 "../Main.m3"
Main__A(
  ( INTEGER )( *((INT64*)(var_param_record_L_40)) ));
#line 70 "../Main.m3"
 /* set_source_line */
#line 70 "../Main.m3"
#line 71 "../Main.m3"
 /* start_call_direct */
#line 71 "../Main.m3"
 /* load */
#line 71 "../Main.m3"
 /* load_indirect */
#line 71 "../Main.m3"
 /* pop_param */
#line 71 "../Main.m3"
 /* call_direct */
#line 71 "../Main.m3"
Main__A(
  ( INTEGER )( *((INT64*)(_frame.var_param_record_uplevel_L_41)) ));
#line 71 "../Main.m3"
 /* set_source_line */
#line 71 "../Main.m3"
#line 72 "../Main.m3"
 /* start_call_direct */
#line 72 "../Main.m3"
 /* load */
#line 72 "../Main.m3"
 /* load_indirect */
#line 72 "../Main.m3"
 /* pop_param */
#line 72 "../Main.m3"
 /* call_direct */
#line 72 "../Main.m3"
Main__A(
  ( INTEGER )( *((INT64*)(readonly_param_record_L_42)) ));
#line 72 "../Main.m3"
 /* set_source_line */
#line 72 "../Main.m3"
#line 73 "../Main.m3"
 /* start_call_direct */
#line 73 "../Main.m3"
 /* load */
#line 73 "../Main.m3"
 /* load_indirect */
#line 73 "../Main.m3"
 /* pop_param */
#line 73 "../Main.m3"
 /* call_direct */
#line 73 "../Main.m3"
Main__A(
  ( INTEGER )( *((INT64*)(_frame.readonly_param_record_uplevel_L_43)) ));
#line 73 "../Main.m3"
 /* set_source_line */
#line 73 "../Main.m3"
#line 75 "../Main.m3"
 /* load_integer */
#line 75 "../Main.m3"
 /* if_true_or_false */
#line 75 "../Main.m3"
 /* load_host_integer */
#line 75 "../Main.m3"
 /* load_integer */
#line 75 "../Main.m3"
 /* if_compare */
#line 75 "../Main.m3"
if(m3_eq(INT64,
   INT64_(0),
   INT64_(0)))goto L4;
#line 75 "../Main.m3"
 /* load */
#line 75 "../Main.m3"
 /* load_address */
#line 75 "../Main.m3"
 /* copy */
#line 75 "../Main.m3"
m3_memmove(
 _result_L_31,
 INT64_(72)+((ADDRESS)(&Main_m_21_L_22)),
 8);
#line 75 "../Main.m3"
 /* exit_proc */
#line 75 "../Main.m3"
return;
#line 75 "../Main.m3"
 /* set_label */
#line 75 "../Main.m3"
L4:;
#line 75 "../Main.m3"
 /* set_source_line */
#line 75 "../Main.m3"
#line 76 "../Main.m3"
 /* load_integer */
#line 76 "../Main.m3"
 /* if_true_or_false */
#line 76 "../Main.m3"
 /* load_host_integer */
#line 76 "../Main.m3"
 /* load_integer */
#line 76 "../Main.m3"
 /* if_compare */
#line 76 "../Main.m3"
if(m3_eq(INT64,
   INT64_(0),
   INT64_(0)))goto L6;
#line 76 "../Main.m3"
 /* load */
#line 76 "../Main.m3"
 /* load_address */
#line 76 "../Main.m3"
 /* copy */
#line 76 "../Main.m3"
m3_memmove(
 _result_L_31,
 &param_record_L_38,
 8);
#line 76 "../Main.m3"
 /* exit_proc */
#line 76 "../Main.m3"
return;
#line 76 "../Main.m3"
 /* set_label */
#line 76 "../Main.m3"
L6:;
#line 76 "../Main.m3"
 /* set_source_line */
#line 76 "../Main.m3"
#line 77 "../Main.m3"
 /* load_integer */
#line 77 "../Main.m3"
 /* if_true_or_false */
#line 77 "../Main.m3"
 /* load_host_integer */
#line 77 "../Main.m3"
 /* load_integer */
#line 77 "../Main.m3"
 /* if_compare */
#line 77 "../Main.m3"
if(m3_eq(INT64,
   INT64_(0),
   INT64_(0)))goto L8;
#line 77 "../Main.m3"
 /* load */
#line 77 "../Main.m3"
 /* load_address */
#line 77 "../Main.m3"
 /* copy */
#line 77 "../Main.m3"
m3_memmove(
 _result_L_31,
 &_frame.param_record_uplevel_L_39,
 8);
#line 77 "../Main.m3"
 /* exit_proc */
#line 77 "../Main.m3"
return;
#line 77 "../Main.m3"
 /* set_label */
#line 77 "../Main.m3"
L8:;
#line 77 "../Main.m3"
 /* set_source_line */
#line 77 "../Main.m3"
#line 78 "../Main.m3"
 /* load_integer */
#line 78 "../Main.m3"
 /* if_true_or_false */
#line 78 "../Main.m3"
 /* load_host_integer */
#line 78 "../Main.m3"
 /* load_integer */
#line 78 "../Main.m3"
 /* if_compare */
#line 78 "../Main.m3"
if(m3_eq(INT64,
   INT64_(0),
   INT64_(0)))goto LA;
#line 78 "../Main.m3"
 /* load */
#line 78 "../Main.m3"
 /* load */
#line 78 "../Main.m3"
 /* copy */
#line 78 "../Main.m3"
m3_memmove(
 _result_L_31,
 var_param_record_L_40,
 8);
#line 78 "../Main.m3"
 /* exit_proc */
#line 78 "../Main.m3"
return;
#line 78 "../Main.m3"
 /* set_label */
#line 78 "../Main.m3"
LA:;
#line 78 "../Main.m3"
 /* set_source_line */
#line 78 "../Main.m3"
#line 79 "../Main.m3"
 /* load_integer */
#line 79 "../Main.m3"
 /* if_true_or_false */
#line 79 "../Main.m3"
 /* load_host_integer */
#line 79 "../Main.m3"
 /* load_integer */
#line 79 "../Main.m3"
 /* if_compare */
#line 79 "../Main.m3"
if(m3_eq(INT64,
   INT64_(0),
   INT64_(0)))goto LC;
#line 79 "../Main.m3"
 /* load */
#line 79 "../Main.m3"
 /* load */
#line 79 "../Main.m3"
 /* copy */
#line 79 "../Main.m3"
m3_memmove(
 _result_L_31,
 _frame.var_param_record_uplevel_L_41,
 8);
#line 79 "../Main.m3"
 /* exit_proc */
#line 79 "../Main.m3"
return;
#line 79 "../Main.m3"
 /* set_label */
#line 79 "../Main.m3"
LC:;
#line 79 "../Main.m3"
 /* set_source_line */
#line 79 "../Main.m3"
#line 80 "../Main.m3"
 /* load_integer */
#line 80 "../Main.m3"
 /* if_true_or_false */
#line 80 "../Main.m3"
 /* load_host_integer */
#line 80 "../Main.m3"
 /* load_integer */
#line 80 "../Main.m3"
 /* if_compare */
#line 80 "../Main.m3"
if(m3_eq(INT64,
   INT64_(0),
   INT64_(0)))goto LE;
#line 80 "../Main.m3"
 /* load */
#line 80 "../Main.m3"
 /* load */
#line 80 "../Main.m3"
 /* copy */
#line 80 "../Main.m3"
m3_memmove(
 _result_L_31,
 readonly_param_record_L_42,
 8);
#line 80 "../Main.m3"
 /* exit_proc */
#line 80 "../Main.m3"
return;
#line 80 "../Main.m3"
 /* set_label */
#line 80 "../Main.m3"
LE:;
#line 80 "../Main.m3"
 /* set_source_line */
#line 80 "../Main.m3"
#line 81 "../Main.m3"
 /* load_integer */
#line 81 "../Main.m3"
 /* if_true_or_false */
#line 81 "../Main.m3"
 /* load_host_integer */
#line 81 "../Main.m3"
 /* load_integer */
#line 81 "../Main.m3"
 /* if_compare */
#line 81 "../Main.m3"
if(m3_eq(INT64,
   INT64_(0),
   INT64_(0)))goto L10;
#line 81 "../Main.m3"
 /* load */
#line 81 "../Main.m3"
 /* load */
#line 81 "../Main.m3"
 /* copy */
#line 81 "../Main.m3"
m3_memmove(
 _result_L_31,
 _frame.readonly_param_record_uplevel_L_43,
 8);
#line 81 "../Main.m3"
 /* exit_proc */
#line 81 "../Main.m3"
return;
#line 81 "../Main.m3"
 /* set_label */
#line 81 "../Main.m3"
L10:;
#line 81 "../Main.m3"
 /* set_source_line */
#line 81 "../Main.m3"
#line 82 "../Main.m3"
 /* load_integer */
#line 82 "../Main.m3"
 /* if_true_or_false */
#line 82 "../Main.m3"
 /* load_host_integer */
#line 82 "../Main.m3"
 /* load_integer */
#line 82 "../Main.m3"
 /* if_compare */
#line 82 "../Main.m3"
if(m3_eq(INT64,
   INT64_(0),
   INT64_(0)))goto L12;
#line 82 "../Main.m3"
 /* load */
#line 82 "../Main.m3"
 /* load_address */
#line 82 "../Main.m3"
 /* copy */
#line 82 "../Main.m3"
m3_memmove(
 _result_L_31,
 &_frame.local_record_uplevel_L_30,
 8);
#line 82 "../Main.m3"
 /* exit_proc */
#line 82 "../Main.m3"
return;
#line 82 "../Main.m3"
 /* set_label */
#line 82 "../Main.m3"
L12:;
#line 82 "../Main.m3"
 /* set_source_line */
#line 82 "../Main.m3"
#line 83 "../Main.m3"
 /* load_integer */
#line 83 "../Main.m3"
 /* if_true_or_false */
#line 83 "../Main.m3"
 /* load_host_integer */
#line 83 "../Main.m3"
 /* load_integer */
#line 83 "../Main.m3"
 /* if_compare */
#line 83 "../Main.m3"
if(m3_eq(INT64,
   INT64_(0),
   INT64_(0)))goto L14;
#line 83 "../Main.m3"
 /* load */
#line 83 "../Main.m3"
 /* load_address */
#line 83 "../Main.m3"
 /* copy */
#line 83 "../Main.m3"
m3_memmove(
 _result_L_31,
 &local_record_L_29,
 8);
#line 83 "../Main.m3"
 /* exit_proc */
#line 83 "../Main.m3"
return;
#line 83 "../Main.m3"
 /* set_label */
#line 83 "../Main.m3"
L14:;
#line 83 "../Main.m3"
 /* set_source_line */
#line 83 "../Main.m3"
#line 84 "../Main.m3"
 /* load */
#line 84 "../Main.m3"
 /* load_address */
#line 84 "../Main.m3"
 /* copy */
#line 84 "../Main.m3"
m3_memmove(
 _result_L_31,
 INT64_(80)+((ADDRESS)(&Main_m_21_L_22)),
 8);
#line 84 "../Main.m3"
 /* exit_proc */
#line 84 "../Main.m3"
return;
#line 84 "../Main.m3"
 /* end_procedure */
#line 84 "../Main.m3"
} /* Main.F1 */
#line 84 "../Main.m3"
 /* set_source_line */
#line 84 "../Main.m3"
#line 37 "../Main.m3"
 /* begin_procedure */
#line 37 "../Main.m3"
struct Main__Main__F1_Frame_t {
#line 37 "../Main.m3"
ADDRESS _unused;
#line 37 "../Main.m3"
 /* Var_Type1 */ Main__Main_Frame_t* _static_link;
#line 37 "../Main.m3"
};
#line 37 "../Main.m3"
void /*TypeText3*/ 
__cdecl
Main__Main__F1(
   /* Param_Type1 */ Main__Main_Frame_t* _static_link)
{
#line 37 "../Main.m3"
Main__Main__F1_Frame_t _frame;
#line 37 "../Main.m3"
_frame._static_link=_static_link;
#line 37 "../Main.m3"
_frame._unused=(ADDRESS)&_frame;
#line 37 "../Main.m3"
 /* set_source_line */
#line 37 "../Main.m3"
#line 38 "../Main.m3"
 /* set_source_line */
#line 38 "../Main.m3"
#line 39 "../Main.m3"
 /* load */
#line 39 "../Main.m3"
 /* load_indirect */
#line 39 "../Main.m3"
 /* load_integer */
#line 39 "../Main.m3"
 /* add */
#line 39 "../Main.m3"
 /* load */
#line 39 "../Main.m3"
 /* add */
#line 39 "../Main.m3"
 /* store */
#line 39 "../Main.m3"
(*(INT64*)(&_static_link->param_integer_uplevel_L_33))=(INT64)( ((INT64)( ((INT64)( *((INT64*)(_static_link->readonly_param_integer_uplevel_L_37))+  INT64_(100)))+ _static_link->param_integer_uplevel_L_33)));
#line 39 "../Main.m3"
 /* set_source_line */
#line 39 "../Main.m3"
#line 40 "../Main.m3"
 /* load */
#line 40 "../Main.m3"
 /* load_indirect */
#line 40 "../Main.m3"
 /* load */
#line 40 "../Main.m3"
 /* load_indirect */
#line 40 "../Main.m3"
 /* load_integer */
#line 40 "../Main.m3"
 /* add */
#line 40 "../Main.m3"
 /* add */
#line 40 "../Main.m3"
 /* load */
#line 40 "../Main.m3"
 /* swap */
#line 40 "../Main.m3"
 /* store_indirect */
#line 40 "../Main.m3"
(*(INT64*)(_static_link->var_param_integer_uplevel_L_35))=(INT64)( ((INT64)( *((INT64*)(_static_link->var_param_integer_uplevel_L_35))+ ((INT64)( *((INT64*)(_static_link->readonly_param_integer_uplevel_L_37))+  INT64_(200))))));
#line 40 "../Main.m3"
 /* set_source_line */
#line 40 "../Main.m3"
#line 41 "../Main.m3"
 /* load */
#line 41 "../Main.m3"
 /* load_indirect */
#line 41 "../Main.m3"
 /* load_integer */
#line 41 "../Main.m3"
 /* add */
#line 41 "../Main.m3"
 /* load */
#line 41 "../Main.m3"
 /* add */
#line 41 "../Main.m3"
 /* store */
#line 41 "../Main.m3"
(*(INT64*)(&_static_link->param_record_uplevel_L_39))=(INT64)( ((INT64)( ((INT64)( *((INT64*)(_static_link->readonly_param_record_uplevel_L_43))+  INT64_(300)))+((INT64)(*((INT64*)(&_static_link->param_record_uplevel_L_39)))))));
#line 41 "../Main.m3"
 /* set_source_line */
#line 41 "../Main.m3"
#line 42 "../Main.m3"
 /* load */
#line 42 "../Main.m3"
 /* load_indirect */
#line 42 "../Main.m3"
 /* load */
#line 42 "../Main.m3"
 /* load_indirect */
#line 42 "../Main.m3"
 /* load_integer */
#line 42 "../Main.m3"
 /* add */
#line 42 "../Main.m3"
 /* add */
#line 42 "../Main.m3"
 /* load */
#line 42 "../Main.m3"
 /* swap */
#line 42 "../Main.m3"
 /* store_indirect */
#line 42 "../Main.m3"
(*(INT64*)(_static_link->var_param_record_uplevel_L_41))=(INT64)( ((INT64)( *((INT64*)(_static_link->var_param_record_uplevel_L_41))+ ((INT64)( *((INT64*)(_static_link->readonly_param_record_uplevel_L_43))+  INT64_(400))))));
#line 42 "../Main.m3"
 /* set_source_line */
#line 42 "../Main.m3"
#line 43 "../Main.m3"
 /* load_integer */
#line 43 "../Main.m3"
 /* load */
#line 43 "../Main.m3"
 /* add */
#line 43 "../Main.m3"
 /* store */
#line 43 "../Main.m3"
(*(INT64*)(&_static_link->local_integer_uplevel_L_28))=(INT64)( ((INT64)(  INT64_(600)+ _static_link->local_integer_uplevel_L_28)));
#line 43 "../Main.m3"
 /* set_source_line */
#line 43 "../Main.m3"
#line 44 "../Main.m3"
 /* load_integer */
#line 44 "../Main.m3"
 /* load */
#line 44 "../Main.m3"
 /* add */
#line 44 "../Main.m3"
 /* store */
#line 44 "../Main.m3"
(*(INT64*)(&_static_link->local_record_uplevel_L_30))=(INT64)( ((INT64)(  INT64_(700)+((INT64)(*((INT64*)(&_static_link->local_record_uplevel_L_30)))))));
#line 44 "../Main.m3"
 /* set_source_line */
#line 44 "../Main.m3"
#line 45 "../Main.m3"
 /* exit_proc */
#line 45 "../Main.m3"
return;
#line 45 "../Main.m3"
 /* end_procedure */
#line 45 "../Main.m3"
} /* Main_M3 */
#line 45 "../Main.m3"
 /* module main body Main_M3 */
#line 45 "../Main.m3"
 /* set_source_line */
#line 45 "../Main.m3"
#line 100 "../Main.m3"
 /* begin_procedure */
#line 100 "../Main.m3"
struct Main_M3_Frame_t {
#line 100 "../Main.m3"
ADDRESS _unused;
#line 100 "../Main.m3"
};
#line 100 "../Main.m3"
RT0__ModulePtr
__cdecl
Main_M3(
   /* Param_Type1 */ INTEGER /*TypeText1*/  mode_L_24)
{
#line 100 "../Main.m3"
 /* Var_Type3 */ STRUCT(8) Main_m_54_L_55={0};//always-init
#line 100 "../Main.m3"
Main_M3_Frame_t _frame;
#line 100 "../Main.m3"
_frame._unused=(ADDRESS)&_frame;
#line 100 "../Main.m3"
 /* load */
#line 100 "../Main.m3"
 /* if_true_or_false */
#line 100 "../Main.m3"
 /* load_host_integer */
#line 100 "../Main.m3"
 /* load_integer */
#line 100 "../Main.m3"
 /* if_compare */
#line 100 "../Main.m3"
if(m3_eq(INT64,
  mode_L_24,
   INT64_(0)))goto L15;
#line 100 "../Main.m3"
 /* set_source_line */
#line 100 "../Main.m3"
#line 101 "../Main.m3"
 /* start_call_direct */
#line 101 "../Main.m3"
 /* load_address */
#line 101 "../Main.m3"
 /* pop_param */
#line 101 "../Main.m3"
 /* load */
#line 101 "../Main.m3"
 /* pop_param */
#line 101 "../Main.m3"
 /* load */
#line 101 "../Main.m3"
 /* pop_param */
#line 101 "../Main.m3"
 /* load_address */
#line 101 "../Main.m3"
 /* pop_param */
#line 101 "../Main.m3"
 /* load_address */
#line 101 "../Main.m3"
 /* pop_param */
#line 101 "../Main.m3"
 /* load_address */
#line 101 "../Main.m3"
 /* pop_param */
#line 101 "../Main.m3"
 /* load_address */
#line 101 "../Main.m3"
 /* pop_param */
#line 101 "../Main.m3"
 /* load_address */
#line 101 "../Main.m3"
 /* pop_struct */
#line 101 "../Main.m3"
 /* load_address */
#line 101 "../Main.m3"
 /* pop_struct */
#line 101 "../Main.m3"
 /* load_address */
#line 101 "../Main.m3"
 /* pop_param */
#line 101 "../Main.m3"
 /* load_address */
#line 101 "../Main.m3"
 /* pop_param */
#line 101 "../Main.m3"
 /* load_address */
#line 101 "../Main.m3"
 /* pop_param */
#line 101 "../Main.m3"
 /* load_address */
#line 101 "../Main.m3"
 /* pop_param */
#line 101 "../Main.m3"
 /* call_direct */
#line 101 "../Main.m3"
Main__Main(
  ( Main__R1* /*TypeText1*/  )(((ADDRESS)(&Main_m_54_L_55)) ),
  ( INTEGER )(((INT64)(*((INT64*)(INT64_(200)+((ADDRESS)(&Main_m_M_Main_L_23)))))) ),
  ( INTEGER )(((INT64)(*((INT64*)(INT64_(208)+((ADDRESS)(&Main_m_M_Main_L_23)))))) ),
  ( INTEGER* /*TypeText1*/  )(((ADDRESS)(INT64_(216)+((ADDRESS)(&Main_m_M_Main_L_23)))) ),
  ( INTEGER* /*TypeText1*/  )(((ADDRESS)(INT64_(224)+((ADDRESS)(&Main_m_M_Main_L_23)))) ),
  ( INTEGER* /*TypeText1*/  )(((ADDRESS)(INT64_(232)+((ADDRESS)(&Main_m_M_Main_L_23)))) ),
  ( INTEGER* /*TypeText1*/  )(((ADDRESS)(INT64_(240)+((ADDRESS)(&Main_m_M_Main_L_23)))) ),
 ((T6981C397*)(INT64_(248)+((ADDRESS)(&Main_m_M_Main_L_23)))),
 ((T6981C397*)(INT64_(256)+((ADDRESS)(&Main_m_M_Main_L_23)))),
  ( Main__R1* /*TypeText1*/  )(((ADDRESS)(INT64_(264)+((ADDRESS)(&Main_m_M_Main_L_23)))) ),
  ( Main__R1* /*TypeText1*/  )(((ADDRESS)(INT64_(272)+((ADDRESS)(&Main_m_M_Main_L_23)))) ),
  ( Main__R1* /*TypeText1*/  )(((ADDRESS)(INT64_(280)+((ADDRESS)(&Main_m_M_Main_L_23)))) ),
  ( Main__R1* /*TypeText1*/  )(((ADDRESS)(INT64_(288)+((ADDRESS)(&Main_m_M_Main_L_23)))) ));
#line 101 "../Main.m3"
 /* set_source_line */
#line 101 "../Main.m3"
#line 116 "../Main.m3"
 /* start_call_direct */
#line 116 "../Main.m3"
 /* load_address */
#line 116 "../Main.m3"
 /* pop_param */
#line 116 "../Main.m3"
 /* call_direct */
#line 116 "../Main.m3"
RTIO__PutText(
  ( TEXT )(((ADDRESS)(INT64_(48)+((ADDRESS)(&Main_m_21_L_22)))) ));
#line 116 "../Main.m3"
 /* set_source_line */
#line 116 "../Main.m3"
#line 117 "../Main.m3"
 /* start_call_direct */
#line 117 "../Main.m3"
 /* load */
#line 117 "../Main.m3"
 /* pop_param */
#line 117 "../Main.m3"
 /* call_direct */
#line 117 "../Main.m3"
Main__A(
  ( INTEGER )(((INT64)(*((INT64*)(INT64_(200)+((ADDRESS)(&Main_m_M_Main_L_23)))))) ));
#line 117 "../Main.m3"
 /* set_source_line */
#line 117 "../Main.m3"
#line 118 "../Main.m3"
 /* start_call_direct */
#line 118 "../Main.m3"
 /* load */
#line 118 "../Main.m3"
 /* pop_param */
#line 118 "../Main.m3"
 /* call_direct */
#line 118 "../Main.m3"
Main__A(
  ( INTEGER )(((INT64)(*((INT64*)(INT64_(208)+((ADDRESS)(&Main_m_M_Main_L_23)))))) ));
#line 118 "../Main.m3"
 /* set_source_line */
#line 118 "../Main.m3"
#line 119 "../Main.m3"
 /* start_call_direct */
#line 119 "../Main.m3"
 /* load */
#line 119 "../Main.m3"
 /* pop_param */
#line 119 "../Main.m3"
 /* call_direct */
#line 119 "../Main.m3"
Main__A(
  ( INTEGER )(((INT64)(*((INT64*)(INT64_(216)+((ADDRESS)(&Main_m_M_Main_L_23)))))) ));
#line 119 "../Main.m3"
 /* set_source_line */
#line 119 "../Main.m3"
#line 120 "../Main.m3"
 /* start_call_direct */
#line 120 "../Main.m3"
 /* load */
#line 120 "../Main.m3"
 /* pop_param */
#line 120 "../Main.m3"
 /* call_direct */
#line 120 "../Main.m3"
Main__A(
  ( INTEGER )(((INT64)(*((INT64*)(INT64_(224)+((ADDRESS)(&Main_m_M_Main_L_23)))))) ));
#line 120 "../Main.m3"
 /* set_source_line */
#line 120 "../Main.m3"
#line 121 "../Main.m3"
 /* start_call_direct */
#line 121 "../Main.m3"
 /* load */
#line 121 "../Main.m3"
 /* pop_param */
#line 121 "../Main.m3"
 /* call_direct */
#line 121 "../Main.m3"
Main__A(
  ( INTEGER )(((INT64)(*((INT64*)(INT64_(232)+((ADDRESS)(&Main_m_M_Main_L_23)))))) ));
#line 121 "../Main.m3"
 /* set_source_line */
#line 121 "../Main.m3"
#line 122 "../Main.m3"
 /* start_call_direct */
#line 122 "../Main.m3"
 /* load */
#line 122 "../Main.m3"
 /* pop_param */
#line 122 "../Main.m3"
 /* call_direct */
#line 122 "../Main.m3"
Main__A(
  ( INTEGER )(((INT64)(*((INT64*)(INT64_(240)+((ADDRESS)(&Main_m_M_Main_L_23)))))) ));
#line 122 "../Main.m3"
 /* set_source_line */
#line 122 "../Main.m3"
#line 123 "../Main.m3"
 /* start_call_direct */
#line 123 "../Main.m3"
 /* load */
#line 123 "../Main.m3"
 /* pop_param */
#line 123 "../Main.m3"
 /* call_direct */
#line 123 "../Main.m3"
Main__A(
  ( INTEGER )(((INT64)(*((INT64*)(INT64_(248)+((ADDRESS)(&Main_m_M_Main_L_23)))))) ));
#line 123 "../Main.m3"
 /* set_source_line */
#line 123 "../Main.m3"
#line 124 "../Main.m3"
 /* start_call_direct */
#line 124 "../Main.m3"
 /* load */
#line 124 "../Main.m3"
 /* pop_param */
#line 124 "../Main.m3"
 /* call_direct */
#line 124 "../Main.m3"
Main__A(
  ( INTEGER )(((INT64)(*((INT64*)(INT64_(256)+((ADDRESS)(&Main_m_M_Main_L_23)))))) ));
#line 124 "../Main.m3"
 /* set_source_line */
#line 124 "../Main.m3"
#line 125 "../Main.m3"
 /* start_call_direct */
#line 125 "../Main.m3"
 /* load */
#line 125 "../Main.m3"
 /* pop_param */
#line 125 "../Main.m3"
 /* call_direct */
#line 125 "../Main.m3"
Main__A(
  ( INTEGER )(((INT64)(*((INT64*)(INT64_(264)+((ADDRESS)(&Main_m_M_Main_L_23)))))) ));
#line 125 "../Main.m3"
 /* set_source_line */
#line 125 "../Main.m3"
#line 126 "../Main.m3"
 /* start_call_direct */
#line 126 "../Main.m3"
 /* load */
#line 126 "../Main.m3"
 /* pop_param */
#line 126 "../Main.m3"
 /* call_direct */
#line 126 "../Main.m3"
Main__A(
  ( INTEGER )(((INT64)(*((INT64*)(INT64_(272)+((ADDRESS)(&Main_m_M_Main_L_23)))))) ));
#line 126 "../Main.m3"
 /* set_source_line */
#line 126 "../Main.m3"
#line 127 "../Main.m3"
 /* start_call_direct */
#line 127 "../Main.m3"
 /* load */
#line 127 "../Main.m3"
 /* pop_param */
#line 127 "../Main.m3"
 /* call_direct */
#line 127 "../Main.m3"
Main__A(
  ( INTEGER )(((INT64)(*((INT64*)(INT64_(280)+((ADDRESS)(&Main_m_M_Main_L_23)))))) ));
#line 127 "../Main.m3"
 /* set_source_line */
#line 127 "../Main.m3"
#line 128 "../Main.m3"
 /* start_call_direct */
#line 128 "../Main.m3"
 /* load */
#line 128 "../Main.m3"
 /* pop_param */
#line 128 "../Main.m3"
 /* call_direct */
#line 128 "../Main.m3"
Main__A(
  ( INTEGER )(((INT64)(*((INT64*)(INT64_(288)+((ADDRESS)(&Main_m_M_Main_L_23)))))) ));
#line 128 "../Main.m3"
 /* set_source_line */
#line 128 "../Main.m3"
#line 129 "../Main.m3"
 /* start_call_direct */
#line 129 "../Main.m3"
 /* call_direct */
#line 129 "../Main.m3"
RTIO__Flush(
 );
#line 129 "../Main.m3"
 /* set_label */
#line 129 "../Main.m3"
L15:;
#line 129 "../Main.m3"
 /* load_address */
#line 129 "../Main.m3"
 /* exit_proc */
#line 129 "../Main.m3"
return (RT0__ModulePtr)(&Main_m_M_Main_L_23);
#line 129 "../Main.m3"
 /* end_procedure */
#line 129 "../Main.m3"
} /* set_source_line */
#line 129 "../Main.m3"
#line 29 "../Main.m3"
 /* Main_M3_t17f32a76_INIT (RefType) */
#line 29 "../Main.m3"
 /* begin_procedure */
#line 29 "../Main.m3"
struct Main_M3_t17f32a76_INIT_Frame_t {
#line 29 "../Main.m3"
ADDRESS _unused;
#line 29 "../Main.m3"
};
#line 29 "../Main.m3"
void /*TypeText3*/ 
__cdecl
Main_M3_t17f32a76_INIT(
   /* Param_Type1 */ T6981C397* /*TypeText1*/  Main_m_56_L_57)
{
#line 29 "../Main.m3"
Main_M3_t17f32a76_INIT_Frame_t _frame;
#line 29 "../Main.m3"
_frame._unused=(ADDRESS)&_frame;
#line 29 "../Main.m3"
 /* load */
#line 29 "../Main.m3"
 /* load_integer */
#line 29 "../Main.m3"
 /* store_indirect */
#line 29 "../Main.m3"
(*(INT64*)(Main_m_56_L_57))=(INT64)(  INT64_(999));
#line 29 "../Main.m3"
 /* exit_proc */
#line 29 "../Main.m3"
return;
#line 29 "../Main.m3"
 /* end_procedure */
#line 29 "../Main.m3"
} /* global constant type descriptor */
#line 29 "../Main.m3"
 /* global data type descriptor */
#line 29 "../Main.m3"
 /* module global constants */
#line 29 "../Main.m3"
 /* procedure names */
#line 29 "../Main.m3"
 /* procedure table */
#line 29 "../Main.m3"
 /* file name */
#line 29 "../Main.m3"
 /* type map for _t17f32a76 */
#line 29 "../Main.m3"
 /* type description for _t17f32a76 */
#line 29 "../Main.m3"
 /* module global data */
#line 29 "../Main.m3"
 /* typecell for _t17f32a76 */
#line 29 "../Main.m3"
 /* load map


 global data allocation for M_Main
     0   104  8  *module info*
   104    96  8  typecell
   200     8  8  Main.xparam_integer
   208     8  8  Main.xparam_integer_uplevel
   216     8  8  Main.xvar_param_integer
   224     8  8  Main.xvar_param_integer_uplevel
   232     8  8  Main.xreadonly_param_integer
   240     8  8  Main.xreadonly_param_integer_uplevel
   248     8  8  Main.xparam_record
   256     8  8  Main.xparam_record_uplevel
   264     8  8  Main.xvar_param_record
   272     8  8  Main.xvar_param_record_uplevel
   280     8  8  Main.xreadonly_param_record
   288     8  8  Main.xreadonly_param_record_uplevel
   296    24  8  import Main
   320    24  8  import RTIO
   344    24  8  import RTHooks
   368     0  8  *TOTAL*


 global constants for M_Main
     0    40  8  TEXT literal methods
    40    26  8  *TEXT literal*
    72     8  8  *recordConstructor*
    80     8  8  *recordConstructor*
    88    26  8  *proc names*
   120    88  8  *proc info*
   208    11  1  *string*
   219     2  1  type_map
   221     4  1  type_desc
   232     0  8  *TOTAL*
 */
#line 29 "../Main.m3"
 /* end unit */
#line 29 "../Main.m3"

#ifdef __cplusplus

} /* extern "C" */
#endif
 /* set_runtime_proc */
 /* set_runtime_proc */
 /* set_runtime_proc */
