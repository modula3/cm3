// library:pgm
// source_base_name:Main
// target_name:Main.m3.cpp
 /* set_runtime_proc */
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
 /* declare_formal */
 /* declare_subrange */
/*subrange_define*/typedef INT64 T9CED36E7_64;
 /* declare_proctype */
 /* declare_formal */

#ifndef LONGCARD
#define LONGCARD LONGCARD
typedef T9CED36E7_64 LONGCARD;
#endif
 /* declare_subrange */
/*subrange_define*/typedef UINT8 T8B2831D7_8;
 /* declare_subrange */
/*subrange_define*/typedef UINT8 T892833D7_8;
 /* declare_proctype */
 /* declare_formal */
 /* declare_formal */
 /* declare_subrange */
/*subrange_define*/typedef UINT8 T882830D7_8;
 /* declare_proctype */
 /* declare_formal */
 /* declare_formal */
 /* declare_proctype */
 /* declare_formal */
 /* declare_formal */
 /* declare_proctype */
 /* declare_formal */
 /* declare_formal */
 /* declare_enum */
 /* declare_enum_elt */
 /* declare_enum_elt: NUMBER(self.enum.names^):5 */
 /* declare_enum_elt: enum_element_count:5 */
 /* declare_enum_elt */
 /* declare_enum_elt: NUMBER(self.enum.names^):5 */
 /* declare_enum_elt: enum_element_count:5 */
 /* declare_enum_elt */
 /* declare_enum_elt: NUMBER(self.enum.names^):5 */
 /* declare_enum_elt: enum_element_count:5 */
 /* declare_enum_elt */
 /* declare_enum_elt: NUMBER(self.enum.names^):5 */
 /* declare_enum_elt: enum_element_count:5 */
 /* declare_enum_elt */
 /* declare_enum_elt: NUMBER(self.enum.names^):5 */
 /* declare_enum_elt: enum_element_count:5 */
/*enum_define*/typedef UINT8 T8659C383; /*declare_enum*/
#define T8659C383_Zero ((UINT8)0) /*declare_enum_elt*/
#define T8659C383_One ((UINT8)1) /*declare_enum_elt*/
#define T8659C383_Two ((UINT8)2) /*declare_enum_elt*/
#define T8659C383_Three ((UINT8)3) /*declare_enum_elt*/
#define T8659C383_Four ((UINT8)4) /*declare_enum_elt*/
 /* declare_subrange */
/*subrange_define*/typedef T8659C383 T7A403943_8;
 /* declare_subrange */
/*subrange_define*/typedef T8659C383 TBB96C7EF_8;
 /* declare_subrange */
/*subrange_define*/typedef T8659C383 T49DADAC0_8;
 /* declare_proctype */
 /* declare_formal */

#ifndef Main__LowNumber
#define Main__LowNumber Main__LowNumber
typedef T7A403943_8 Main__LowNumber;
#endif
 /* declare_formal */

#ifndef Main__HighNumber
#define Main__HighNumber Main__HighNumber
typedef TBB96C7EF_8 Main__HighNumber;
#endif
 /* declare_proctype */
 /* declare_formal */
 /* declare_formal */

#ifndef Main__MiddleNumber
#define Main__MiddleNumber Main__MiddleNumber
typedef T49DADAC0_8 Main__MiddleNumber;
#endif
 /* declare_proctype */
 /* declare_formal */
 /* declare_formal */
 /* declare_proctype */
 /* declare_formal */
 /* declare_formal */
 /* declare_subrange */
/*subrange_define*/typedef UINT8 T8A2831D7_8;
 /* declare_proctype */
 /* declare_formal */
 /* declare_formal */
 /* declare_proctype */
 /* declare_formal */

#ifndef Main__Number
#define Main__Number Main__Number
typedef T8659C383 Main__Number;
#endif
 /* declare_proctype */
 /* declare_formal */
 /* declare_proctype */
 /* declare_formal */
 /* declare_formal */
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
 /* DeclareTypes_FlushOnce size:20 */

#if 0 /* avoid type hash collions */
typedef 
BOOLEAN(__cdecl*T1B38AC66)(CARDINAL);
#else
typedef void (__cdecl*T1B38AC66)(void);
#endif

#if 0 /* avoid type hash collions */
typedef 
BOOLEAN(__cdecl*T9DCC448E)(LONGCARD);
#else
typedef void (__cdecl*T9DCC448E)(void);
#endif

#if 0 /* avoid type hash collions */
typedef 
BOOLEAN(__cdecl*TAAE53B35)(T8B2831D7_8,T892833D7_8);
#else
typedef void (__cdecl*TAAE53B35)(void);
#endif

#if 0 /* avoid type hash collions */
typedef 
BOOLEAN(__cdecl*TDB75E979)(T8B2831D7_8,T882830D7_8);
#else
typedef void (__cdecl*TDB75E979)(void);
#endif

#if 0 /* avoid type hash collions */
typedef 
BOOLEAN(__cdecl*T99264225)(T892833D7_8,T8B2831D7_8);
#else
typedef void (__cdecl*T99264225)(void);
#endif

#if 0 /* avoid type hash collions */
typedef 
BOOLEAN(__cdecl*T28AA905D)(T882830D7_8,T8B2831D7_8);
#else
typedef void (__cdecl*T28AA905D)(void);
#endif

#if 0 /* avoid type hash collions */
typedef 
BOOLEAN(__cdecl*T5CC64B4D)(Main__LowNumber,Main__HighNumber);
#else
typedef void (__cdecl*T5CC64B4D)(void);
#endif

#if 0 /* avoid type hash collions */
typedef 
BOOLEAN(__cdecl*TC3603531)(Main__LowNumber,Main__MiddleNumber);
#else
typedef void (__cdecl*TC3603531)(void);
#endif

#if 0 /* avoid type hash collions */
typedef 
BOOLEAN(__cdecl*T73731A73)(Main__HighNumber,Main__LowNumber);
#else
typedef void (__cdecl*T73731A73)(void);
#endif

#if 0 /* avoid type hash collions */
typedef 
BOOLEAN(__cdecl*TF6440F29)(Main__MiddleNumber,Main__LowNumber);
#else
typedef void (__cdecl*TF6440F29)(void);
#endif

#if 0 /* avoid type hash collions */
typedef 
BOOLEAN(__cdecl*TA2EB68A4)(T8A2831D7_8,T8A2831D7_8);
#else
typedef void (__cdecl*TA2EB68A4)(void);
#endif

#if 0 /* avoid type hash collions */
typedef 
BOOLEAN(__cdecl*T896053F1)(Main__Number);
#else
typedef void (__cdecl*T896053F1)(void);
#endif

#if 0 /* avoid type hash collions */
typedef 
BOOLEAN(__cdecl*T674EFCBE)(INTEGER);
#else
typedef void (__cdecl*T674EFCBE)(void);
#endif

#if 0 /* avoid type hash collions */
typedef 
void(__cdecl*TFCB2B387)(ADDRESS,INTEGER,TEXT);
#else
typedef void (__cdecl*TFCB2B387)(void);
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
#define m3_abs_T(T) static T __stdcall m3_abs_##T(T a) { return ((a < 0) ? (-a) : a); }

#ifndef m3_abs_INT64
#define m3_abs_INT64 m3_abs_INT64
m3_abs_T(INT64)
#endif
 /* end: helper functions */
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
/*Proc_ForwardDeclareFrameType*/struct RTHooks_I3_Frame_t;typedef struct RTHooks_I3_Frame_t RTHooks_I3_Frame_t;
 /* internal_declare_param */
RT0__ModulePtr
__cdecl
RTHooks_I3(
   /* Param_Type1 */ INTEGER /*TypeText1*/  mode_L_2);
 /* import_procedure */

#if __GNUC__ > 2 || __GNUC__ == 2 && __GNUC_MINOR__ >= 5
#define M3_ATTRIBUTE_NO_RETURN __attribute__((__noreturn__))
#else
#define M3_ATTRIBUTE_NO_RETURN
#endif
/*Proc_ForwardDeclareFrameType*/struct RTHooks__AssertFailed_Frame_t;typedef struct RTHooks__AssertFailed_Frame_t RTHooks__AssertFailed_Frame_t;
 /* internal_declare_param */
 /* internal_declare_param */
 /* internal_declare_param */
void /*TypeText3*/ 
__cdecl
RTHooks__AssertFailed(
   /* Param_Type1 */ ADDRESS module_L_3,
   /* Param_Type1 */ INTEGER line_L_4,
   /* Param_Type1 */ TEXT msg_L_5) M3_ATTRIBUTE_NO_RETURN;
 /* import_procedure */
/*Proc_ForwardDeclareFrameType*/struct RTHooks__TextLitInfo_Frame_t;typedef struct RTHooks__TextLitInfo_Frame_t RTHooks__TextLitInfo_Frame_t;
 /* internal_declare_param */
 /* internal_declare_param */
void /*TypeText3*/ 
__cdecl
RTHooks__TextLitInfo(
   /* Param_Type1 */ RTHooks__TextLiteral t_L_6,
   /* Param_Type1 */ RTHooks__TextInfo* /*TypeText1*/  i_L_7);
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
   /* Param_Type1 */ RTHooks__TextLiteral t_L_8,
   /* Param_Type1 */ CARDINAL i_L_9);
 /* import_procedure */
/*Proc_ForwardDeclareFrameType*/struct RTHooks__TextLitGetWideChar_Frame_t;typedef struct RTHooks__TextLitGetWideChar_Frame_t RTHooks__TextLitGetWideChar_Frame_t;
 /* internal_declare_param */
 /* internal_declare_param */
WIDECHAR
__cdecl
RTHooks__TextLitGetWideChar(
   /* Param_Type1 */ RTHooks__TextLiteral t_L_10,
   /* Param_Type1 */ CARDINAL i_L_11);
 /* import_procedure */
/*Proc_ForwardDeclareFrameType*/struct RTHooks__TextLitGetChars_Frame_t;typedef struct RTHooks__TextLitGetChars_Frame_t RTHooks__TextLitGetChars_Frame_t;
 /* internal_declare_param */
 /* internal_declare_param */
 /* internal_declare_param */
void /*TypeText3*/ 
__cdecl
RTHooks__TextLitGetChars(
   /* Param_Type1 */ RTHooks__TextLiteral t_L_12,
   /* Param_Type1 */ T89CD34BD* /*TypeText1*/  a_L_13,
   /* Param_Type1 */ CARDINAL start_L_14);
 /* import_procedure */
/*Proc_ForwardDeclareFrameType*/struct RTHooks__TextLitGetWideChars_Frame_t;typedef struct RTHooks__TextLitGetWideChars_Frame_t RTHooks__TextLitGetWideChars_Frame_t;
 /* internal_declare_param */
 /* internal_declare_param */
 /* internal_declare_param */
void /*TypeText3*/ 
__cdecl
RTHooks__TextLitGetWideChars(
   /* Param_Type1 */ RTHooks__TextLiteral t_L_15,
   /* Param_Type1 */ TA19BDC21* /*TypeText1*/  a_L_16,
   /* Param_Type1 */ CARDINAL start_L_17);
 /* import_procedure */
/*Proc_ForwardDeclareFrameType*/struct RTHooks__ReportFault_Frame_t;typedef struct RTHooks__ReportFault_Frame_t RTHooks__ReportFault_Frame_t;
 /* internal_declare_param */
 /* internal_declare_param */
void /*TypeText3*/ 
__cdecl
RTHooks__ReportFault(
   /* Param_Type1 */ ADDRESS module_L_18,
   /* Param_Type1 */ INTEGER info_L_19) M3_ATTRIBUTE_NO_RETURN;
 /* end: imports */
 /* begin: locals */
 /* declare_segment name:<NIL> typeid:TFFFFFFFF const:TRUE */
/*declare_segment*/struct Main_m_20_L_21_t;
/*declare_segment*/typedef struct Main_m_20_L_21_t Main_m_20_L_21_t;
 /* declare_segment name:M_Main typeid:TFFFFFFFF const:FALSE */
 /* handler_name_prefixes:Main_M3_LINE_ */
 /* handler_name_prefixes:Main_I3_LINE_ */
/*declare_segment*/struct Main_m_M_Main_L_22_t;
/*declare_segment*/typedef struct Main_m_M_Main_L_22_t Main_m_M_Main_L_22_t;
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct Main_M3_Frame_t;typedef struct Main_M3_Frame_t Main_M3_Frame_t;
 /* internal_declare_param */
RT0__ModulePtr
__cdecl
Main_M3(
   /* Param_Type1 */ INTEGER /*TypeText1*/  mode_L_23);
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct Main__CardinalLT0_false_Frame_t;typedef struct Main__CardinalLT0_false_Frame_t Main__CardinalLT0_false_Frame_t;
 /* declare_local */
 /* internal_declare_param */
BOOLEAN
__cdecl
Main__CardinalLT0_false(
   /* Param_Type1 */ CARDINAL a_L_25);
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct Main__CardinalGE0_true_Frame_t;typedef struct Main__CardinalGE0_true_Frame_t Main__CardinalGE0_true_Frame_t;
 /* declare_local */
 /* internal_declare_param */
BOOLEAN
__cdecl
Main__CardinalGE0_true(
   /* Param_Type1 */ CARDINAL a_L_27);
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct Main__CardinalLTNeg1_false_Frame_t;typedef struct Main__CardinalLTNeg1_false_Frame_t Main__CardinalLTNeg1_false_Frame_t;
 /* declare_local */
 /* internal_declare_param */
BOOLEAN
__cdecl
Main__CardinalLTNeg1_false(
   /* Param_Type1 */ CARDINAL a_L_29);
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct Main__CardinalLENeg1_false_Frame_t;typedef struct Main__CardinalLENeg1_false_Frame_t Main__CardinalLENeg1_false_Frame_t;
 /* declare_local */
 /* internal_declare_param */
BOOLEAN
__cdecl
Main__CardinalLENeg1_false(
   /* Param_Type1 */ CARDINAL a_L_31);
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct Main__CardinalGTNeg1_true_Frame_t;typedef struct Main__CardinalGTNeg1_true_Frame_t Main__CardinalGTNeg1_true_Frame_t;
 /* declare_local */
 /* internal_declare_param */
BOOLEAN
__cdecl
Main__CardinalGTNeg1_true(
   /* Param_Type1 */ CARDINAL a_L_33);
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct Main__CardinalGENeg1_true_Frame_t;typedef struct Main__CardinalGENeg1_true_Frame_t Main__CardinalGENeg1_true_Frame_t;
 /* declare_local */
 /* internal_declare_param */
BOOLEAN
__cdecl
Main__CardinalGENeg1_true(
   /* Param_Type1 */ CARDINAL a_L_35);
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct Main__CardinalNENeg1_true_Frame_t;typedef struct Main__CardinalNENeg1_true_Frame_t Main__CardinalNENeg1_true_Frame_t;
 /* declare_local */
 /* internal_declare_param */
BOOLEAN
__cdecl
Main__CardinalNENeg1_true(
   /* Param_Type1 */ CARDINAL a_L_37);
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct Main__CardinalEQNeg1_false_Frame_t;typedef struct Main__CardinalEQNeg1_false_Frame_t Main__CardinalEQNeg1_false_Frame_t;
 /* declare_local */
 /* internal_declare_param */
BOOLEAN
__cdecl
Main__CardinalEQNeg1_false(
   /* Param_Type1 */ CARDINAL a_L_39);
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct Main__CardinalLTNeg2_false_Frame_t;typedef struct Main__CardinalLTNeg2_false_Frame_t Main__CardinalLTNeg2_false_Frame_t;
 /* declare_local */
 /* internal_declare_param */
BOOLEAN
__cdecl
Main__CardinalLTNeg2_false(
   /* Param_Type1 */ CARDINAL a_L_41);
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct Main__CardinalLENeg2_false_Frame_t;typedef struct Main__CardinalLENeg2_false_Frame_t Main__CardinalLENeg2_false_Frame_t;
 /* declare_local */
 /* internal_declare_param */
BOOLEAN
__cdecl
Main__CardinalLENeg2_false(
   /* Param_Type1 */ CARDINAL a_L_43);
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct Main__CardinalGTNeg2_true_Frame_t;typedef struct Main__CardinalGTNeg2_true_Frame_t Main__CardinalGTNeg2_true_Frame_t;
 /* declare_local */
 /* internal_declare_param */
BOOLEAN
__cdecl
Main__CardinalGTNeg2_true(
   /* Param_Type1 */ CARDINAL a_L_45);
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct Main__CardinalGENeg2_true_Frame_t;typedef struct Main__CardinalGENeg2_true_Frame_t Main__CardinalGENeg2_true_Frame_t;
 /* declare_local */
 /* internal_declare_param */
BOOLEAN
__cdecl
Main__CardinalGENeg2_true(
   /* Param_Type1 */ CARDINAL a_L_47);
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct Main__CardinalNENeg2_true_Frame_t;typedef struct Main__CardinalNENeg2_true_Frame_t Main__CardinalNENeg2_true_Frame_t;
 /* declare_local */
 /* internal_declare_param */
BOOLEAN
__cdecl
Main__CardinalNENeg2_true(
   /* Param_Type1 */ CARDINAL a_L_49);
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct Main__CardinalEQNeg2_false_Frame_t;typedef struct Main__CardinalEQNeg2_false_Frame_t Main__CardinalEQNeg2_false_Frame_t;
 /* declare_local */
 /* internal_declare_param */
BOOLEAN
__cdecl
Main__CardinalEQNeg2_false(
   /* Param_Type1 */ CARDINAL a_L_51);
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct Main__LongcardLT0_false_Frame_t;typedef struct Main__LongcardLT0_false_Frame_t Main__LongcardLT0_false_Frame_t;
 /* declare_local */
 /* internal_declare_param */
BOOLEAN
__cdecl
Main__LongcardLT0_false(
   /* Param_Type1 */ LONGCARD a_L_53);
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct Main__LongcardGE0_true_Frame_t;typedef struct Main__LongcardGE0_true_Frame_t Main__LongcardGE0_true_Frame_t;
 /* declare_local */
 /* internal_declare_param */
BOOLEAN
__cdecl
Main__LongcardGE0_true(
   /* Param_Type1 */ LONGCARD a_L_55);
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct Main__no_overlap_less_LT_true_Frame_t;typedef struct Main__no_overlap_less_LT_true_Frame_t Main__no_overlap_less_LT_true_Frame_t;
 /* declare_local */
 /* internal_declare_param */
 /* internal_declare_param */
BOOLEAN
__cdecl
Main__no_overlap_less_LT_true(
   /* Param_Type1 */ T8B2831D7_8 /*TypeText1*/  a_L_57,
   /* Param_Type1 */ T892833D7_8 /*TypeText1*/  b_L_58);
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct Main__no_overlap_less_LE_true_Frame_t;typedef struct Main__no_overlap_less_LE_true_Frame_t Main__no_overlap_less_LE_true_Frame_t;
 /* declare_local */
 /* internal_declare_param */
 /* internal_declare_param */
BOOLEAN
__cdecl
Main__no_overlap_less_LE_true(
   /* Param_Type1 */ T8B2831D7_8 /*TypeText1*/  a_L_60,
   /* Param_Type1 */ T892833D7_8 /*TypeText1*/  b_L_61);
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct Main__no_overlap_less_GT_false_Frame_t;typedef struct Main__no_overlap_less_GT_false_Frame_t Main__no_overlap_less_GT_false_Frame_t;
 /* declare_local */
 /* internal_declare_param */
 /* internal_declare_param */
BOOLEAN
__cdecl
Main__no_overlap_less_GT_false(
   /* Param_Type1 */ T8B2831D7_8 /*TypeText1*/  a_L_63,
   /* Param_Type1 */ T892833D7_8 /*TypeText1*/  b_L_64);
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct Main__no_overlap_less_GE_false_Frame_t;typedef struct Main__no_overlap_less_GE_false_Frame_t Main__no_overlap_less_GE_false_Frame_t;
 /* declare_local */
 /* internal_declare_param */
 /* internal_declare_param */
BOOLEAN
__cdecl
Main__no_overlap_less_GE_false(
   /* Param_Type1 */ T8B2831D7_8 /*TypeText1*/  a_L_66,
   /* Param_Type1 */ T892833D7_8 /*TypeText1*/  b_L_67);
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct Main__no_overlap_less_EQ_false_Frame_t;typedef struct Main__no_overlap_less_EQ_false_Frame_t Main__no_overlap_less_EQ_false_Frame_t;
 /* declare_local */
 /* internal_declare_param */
 /* internal_declare_param */
BOOLEAN
__cdecl
Main__no_overlap_less_EQ_false(
   /* Param_Type1 */ T8B2831D7_8 /*TypeText1*/  a_L_69,
   /* Param_Type1 */ T892833D7_8 /*TypeText1*/  b_L_70);
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct Main__no_overlap_less_NE_true_Frame_t;typedef struct Main__no_overlap_less_NE_true_Frame_t Main__no_overlap_less_NE_true_Frame_t;
 /* declare_local */
 /* internal_declare_param */
 /* internal_declare_param */
BOOLEAN
__cdecl
Main__no_overlap_less_NE_true(
   /* Param_Type1 */ T8B2831D7_8 /*TypeText1*/  a_L_72,
   /* Param_Type1 */ T892833D7_8 /*TypeText1*/  b_L_73);
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct Main__minimum_overlap_less_LE_true_Frame_t;typedef struct Main__minimum_overlap_less_LE_true_Frame_t Main__minimum_overlap_less_LE_true_Frame_t;
 /* declare_local */
 /* internal_declare_param */
 /* internal_declare_param */
BOOLEAN
__cdecl
Main__minimum_overlap_less_LE_true(
   /* Param_Type1 */ T8B2831D7_8 /*TypeText1*/  a_L_75,
   /* Param_Type1 */ T882830D7_8 /*TypeText1*/  b_L_76);
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct Main__minimum_overlap_less_GT_false_Frame_t;typedef struct Main__minimum_overlap_less_GT_false_Frame_t Main__minimum_overlap_less_GT_false_Frame_t;
 /* declare_local */
 /* internal_declare_param */
 /* internal_declare_param */
BOOLEAN
__cdecl
Main__minimum_overlap_less_GT_false(
   /* Param_Type1 */ T8B2831D7_8 /*TypeText1*/  a_L_78,
   /* Param_Type1 */ T882830D7_8 /*TypeText1*/  b_L_79);
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct Main__no_overlap_greater_LT_false_Frame_t;typedef struct Main__no_overlap_greater_LT_false_Frame_t Main__no_overlap_greater_LT_false_Frame_t;
 /* declare_local */
 /* internal_declare_param */
 /* internal_declare_param */
BOOLEAN
__cdecl
Main__no_overlap_greater_LT_false(
   /* Param_Type1 */ T892833D7_8 /*TypeText1*/  a_L_81,
   /* Param_Type1 */ T8B2831D7_8 /*TypeText1*/  b_L_82);
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct Main__no_overlap_greater_LE_false_Frame_t;typedef struct Main__no_overlap_greater_LE_false_Frame_t Main__no_overlap_greater_LE_false_Frame_t;
 /* declare_local */
 /* internal_declare_param */
 /* internal_declare_param */
BOOLEAN
__cdecl
Main__no_overlap_greater_LE_false(
   /* Param_Type1 */ T892833D7_8 /*TypeText1*/  a_L_84,
   /* Param_Type1 */ T8B2831D7_8 /*TypeText1*/  b_L_85);
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct Main__no_overlap_greater_GT_true_Frame_t;typedef struct Main__no_overlap_greater_GT_true_Frame_t Main__no_overlap_greater_GT_true_Frame_t;
 /* declare_local */
 /* internal_declare_param */
 /* internal_declare_param */
BOOLEAN
__cdecl
Main__no_overlap_greater_GT_true(
   /* Param_Type1 */ T892833D7_8 /*TypeText1*/  a_L_87,
   /* Param_Type1 */ T8B2831D7_8 /*TypeText1*/  b_L_88);
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct Main__no_overlap_greater_GE_true_Frame_t;typedef struct Main__no_overlap_greater_GE_true_Frame_t Main__no_overlap_greater_GE_true_Frame_t;
 /* declare_local */
 /* internal_declare_param */
 /* internal_declare_param */
BOOLEAN
__cdecl
Main__no_overlap_greater_GE_true(
   /* Param_Type1 */ T892833D7_8 /*TypeText1*/  a_L_90,
   /* Param_Type1 */ T8B2831D7_8 /*TypeText1*/  b_L_91);
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct Main__no_overlap_greater_EQ_false_Frame_t;typedef struct Main__no_overlap_greater_EQ_false_Frame_t Main__no_overlap_greater_EQ_false_Frame_t;
 /* declare_local */
 /* internal_declare_param */
 /* internal_declare_param */
BOOLEAN
__cdecl
Main__no_overlap_greater_EQ_false(
   /* Param_Type1 */ T892833D7_8 /*TypeText1*/  a_L_93,
   /* Param_Type1 */ T8B2831D7_8 /*TypeText1*/  b_L_94);
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct Main__no_overlap_greater_NE_true_Frame_t;typedef struct Main__no_overlap_greater_NE_true_Frame_t Main__no_overlap_greater_NE_true_Frame_t;
 /* declare_local */
 /* internal_declare_param */
 /* internal_declare_param */
BOOLEAN
__cdecl
Main__no_overlap_greater_NE_true(
   /* Param_Type1 */ T892833D7_8 /*TypeText1*/  a_L_96,
   /* Param_Type1 */ T8B2831D7_8 /*TypeText1*/  b_L_97);
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct Main__minimum_overlap_greater_LT_false_Frame_t;typedef struct Main__minimum_overlap_greater_LT_false_Frame_t Main__minimum_overlap_greater_LT_false_Frame_t;
 /* declare_local */
 /* internal_declare_param */
 /* internal_declare_param */
BOOLEAN
__cdecl
Main__minimum_overlap_greater_LT_false(
   /* Param_Type1 */ T882830D7_8 /*TypeText1*/  a_L_99,
   /* Param_Type1 */ T8B2831D7_8 /*TypeText1*/  b_L_100);
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct Main__minimum_overlap_greater_GE_true_Frame_t;typedef struct Main__minimum_overlap_greater_GE_true_Frame_t Main__minimum_overlap_greater_GE_true_Frame_t;
 /* declare_local */
 /* internal_declare_param */
 /* internal_declare_param */
BOOLEAN
__cdecl
Main__minimum_overlap_greater_GE_true(
   /* Param_Type1 */ T882830D7_8 /*TypeText1*/  a_L_102,
   /* Param_Type1 */ T8B2831D7_8 /*TypeText1*/  b_L_103);
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct Main__no_overlap_less_enum_LT_true_Frame_t;typedef struct Main__no_overlap_less_enum_LT_true_Frame_t Main__no_overlap_less_enum_LT_true_Frame_t;
 /* declare_local */
 /* internal_declare_param */
 /* internal_declare_param */
BOOLEAN
__cdecl
Main__no_overlap_less_enum_LT_true(
   /* Param_Type1 */ Main__LowNumber a_L_105,
   /* Param_Type1 */ Main__HighNumber b_L_106);
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct Main__no_overlap_less_enum_LE_true_Frame_t;typedef struct Main__no_overlap_less_enum_LE_true_Frame_t Main__no_overlap_less_enum_LE_true_Frame_t;
 /* declare_local */
 /* internal_declare_param */
 /* internal_declare_param */
BOOLEAN
__cdecl
Main__no_overlap_less_enum_LE_true(
   /* Param_Type1 */ Main__LowNumber a_L_108,
   /* Param_Type1 */ Main__HighNumber b_L_109);
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct Main__no_overlap_less_enum_GT_false_Frame_t;typedef struct Main__no_overlap_less_enum_GT_false_Frame_t Main__no_overlap_less_enum_GT_false_Frame_t;
 /* declare_local */
 /* internal_declare_param */
 /* internal_declare_param */
BOOLEAN
__cdecl
Main__no_overlap_less_enum_GT_false(
   /* Param_Type1 */ Main__LowNumber a_L_111,
   /* Param_Type1 */ Main__HighNumber b_L_112);
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct Main__no_overlap_less_enum_GE_false_Frame_t;typedef struct Main__no_overlap_less_enum_GE_false_Frame_t Main__no_overlap_less_enum_GE_false_Frame_t;
 /* declare_local */
 /* internal_declare_param */
 /* internal_declare_param */
BOOLEAN
__cdecl
Main__no_overlap_less_enum_GE_false(
   /* Param_Type1 */ Main__LowNumber a_L_114,
   /* Param_Type1 */ Main__HighNumber b_L_115);
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct Main__no_overlap_less_enum_EQ_false_Frame_t;typedef struct Main__no_overlap_less_enum_EQ_false_Frame_t Main__no_overlap_less_enum_EQ_false_Frame_t;
 /* declare_local */
 /* internal_declare_param */
 /* internal_declare_param */
BOOLEAN
__cdecl
Main__no_overlap_less_enum_EQ_false(
   /* Param_Type1 */ Main__LowNumber a_L_117,
   /* Param_Type1 */ Main__HighNumber b_L_118);
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct Main__no_overlap_less_enum_NE_true_Frame_t;typedef struct Main__no_overlap_less_enum_NE_true_Frame_t Main__no_overlap_less_enum_NE_true_Frame_t;
 /* declare_local */
 /* internal_declare_param */
 /* internal_declare_param */
BOOLEAN
__cdecl
Main__no_overlap_less_enum_NE_true(
   /* Param_Type1 */ Main__LowNumber a_L_120,
   /* Param_Type1 */ Main__HighNumber b_L_121);
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct Main__minimum_overlap_less_enum_LE_true_Frame_t;typedef struct Main__minimum_overlap_less_enum_LE_true_Frame_t Main__minimum_overlap_less_enum_LE_true_Frame_t;
 /* declare_local */
 /* internal_declare_param */
 /* internal_declare_param */
BOOLEAN
__cdecl
Main__minimum_overlap_less_enum_LE_true(
   /* Param_Type1 */ Main__LowNumber a_L_123,
   /* Param_Type1 */ Main__MiddleNumber b_L_124);
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct Main__minimum_overlap_less_enum_GT_false_Frame_t;typedef struct Main__minimum_overlap_less_enum_GT_false_Frame_t Main__minimum_overlap_less_enum_GT_false_Frame_t;
 /* declare_local */
 /* internal_declare_param */
 /* internal_declare_param */
BOOLEAN
__cdecl
Main__minimum_overlap_less_enum_GT_false(
   /* Param_Type1 */ Main__LowNumber a_L_126,
   /* Param_Type1 */ Main__MiddleNumber b_L_127);
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct Main__no_overlap_greater_enum_LT_false_Frame_t;typedef struct Main__no_overlap_greater_enum_LT_false_Frame_t Main__no_overlap_greater_enum_LT_false_Frame_t;
 /* declare_local */
 /* internal_declare_param */
 /* internal_declare_param */
BOOLEAN
__cdecl
Main__no_overlap_greater_enum_LT_false(
   /* Param_Type1 */ Main__HighNumber a_L_129,
   /* Param_Type1 */ Main__LowNumber b_L_130);
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct Main__no_overlap_greater_enum_LE_false_Frame_t;typedef struct Main__no_overlap_greater_enum_LE_false_Frame_t Main__no_overlap_greater_enum_LE_false_Frame_t;
 /* declare_local */
 /* internal_declare_param */
 /* internal_declare_param */
BOOLEAN
__cdecl
Main__no_overlap_greater_enum_LE_false(
   /* Param_Type1 */ Main__HighNumber a_L_132,
   /* Param_Type1 */ Main__LowNumber b_L_133);
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct Main__no_overlap_greater_enum_GT_true_Frame_t;typedef struct Main__no_overlap_greater_enum_GT_true_Frame_t Main__no_overlap_greater_enum_GT_true_Frame_t;
 /* declare_local */
 /* internal_declare_param */
 /* internal_declare_param */
BOOLEAN
__cdecl
Main__no_overlap_greater_enum_GT_true(
   /* Param_Type1 */ Main__HighNumber a_L_135,
   /* Param_Type1 */ Main__LowNumber b_L_136);
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct Main__no_overlap_greater_enum_GE_true_Frame_t;typedef struct Main__no_overlap_greater_enum_GE_true_Frame_t Main__no_overlap_greater_enum_GE_true_Frame_t;
 /* declare_local */
 /* internal_declare_param */
 /* internal_declare_param */
BOOLEAN
__cdecl
Main__no_overlap_greater_enum_GE_true(
   /* Param_Type1 */ Main__HighNumber a_L_138,
   /* Param_Type1 */ Main__LowNumber b_L_139);
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct Main__no_overlap_greater_enum_EQ_false_Frame_t;typedef struct Main__no_overlap_greater_enum_EQ_false_Frame_t Main__no_overlap_greater_enum_EQ_false_Frame_t;
 /* declare_local */
 /* internal_declare_param */
 /* internal_declare_param */
BOOLEAN
__cdecl
Main__no_overlap_greater_enum_EQ_false(
   /* Param_Type1 */ Main__HighNumber a_L_141,
   /* Param_Type1 */ Main__LowNumber b_L_142);
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct Main__no_overlap_greater_enum_NE_true_Frame_t;typedef struct Main__no_overlap_greater_enum_NE_true_Frame_t Main__no_overlap_greater_enum_NE_true_Frame_t;
 /* declare_local */
 /* internal_declare_param */
 /* internal_declare_param */
BOOLEAN
__cdecl
Main__no_overlap_greater_enum_NE_true(
   /* Param_Type1 */ Main__HighNumber a_L_144,
   /* Param_Type1 */ Main__LowNumber b_L_145);
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct Main__minimum_overlap_greater_enum_LT_false_Frame_t;typedef struct Main__minimum_overlap_greater_enum_LT_false_Frame_t Main__minimum_overlap_greater_enum_LT_false_Frame_t;
 /* declare_local */
 /* internal_declare_param */
 /* internal_declare_param */
BOOLEAN
__cdecl
Main__minimum_overlap_greater_enum_LT_false(
   /* Param_Type1 */ Main__MiddleNumber a_L_147,
   /* Param_Type1 */ Main__LowNumber b_L_148);
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct Main__minimum_overlap_greater_enum_GE_true_Frame_t;typedef struct Main__minimum_overlap_greater_enum_GE_true_Frame_t Main__minimum_overlap_greater_enum_GE_true_Frame_t;
 /* declare_local */
 /* internal_declare_param */
 /* internal_declare_param */
BOOLEAN
__cdecl
Main__minimum_overlap_greater_enum_GE_true(
   /* Param_Type1 */ Main__MiddleNumber a_L_150,
   /* Param_Type1 */ Main__LowNumber b_L_151);
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct Main__overlap_1_LT_false_Frame_t;typedef struct Main__overlap_1_LT_false_Frame_t Main__overlap_1_LT_false_Frame_t;
 /* declare_local */
 /* internal_declare_param */
 /* internal_declare_param */
BOOLEAN
__cdecl
Main__overlap_1_LT_false(
   /* Param_Type1 */ T8A2831D7_8 /*TypeText1*/  a_L_153,
   /* Param_Type1 */ T8A2831D7_8 /*TypeText1*/  b_L_154);
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct Main__overlap_1_LE_true_Frame_t;typedef struct Main__overlap_1_LE_true_Frame_t Main__overlap_1_LE_true_Frame_t;
 /* declare_local */
 /* internal_declare_param */
 /* internal_declare_param */
BOOLEAN
__cdecl
Main__overlap_1_LE_true(
   /* Param_Type1 */ T8A2831D7_8 /*TypeText1*/  a_L_156,
   /* Param_Type1 */ T8A2831D7_8 /*TypeText1*/  b_L_157);
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct Main__overlap_1_GT_false_Frame_t;typedef struct Main__overlap_1_GT_false_Frame_t Main__overlap_1_GT_false_Frame_t;
 /* declare_local */
 /* internal_declare_param */
 /* internal_declare_param */
BOOLEAN
__cdecl
Main__overlap_1_GT_false(
   /* Param_Type1 */ T8A2831D7_8 /*TypeText1*/  a_L_159,
   /* Param_Type1 */ T8A2831D7_8 /*TypeText1*/  b_L_160);
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct Main__overlap_1_GE_true_Frame_t;typedef struct Main__overlap_1_GE_true_Frame_t Main__overlap_1_GE_true_Frame_t;
 /* declare_local */
 /* internal_declare_param */
 /* internal_declare_param */
BOOLEAN
__cdecl
Main__overlap_1_GE_true(
   /* Param_Type1 */ T8A2831D7_8 /*TypeText1*/  a_L_162,
   /* Param_Type1 */ T8A2831D7_8 /*TypeText1*/  b_L_163);
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct Main__overlap_1_EQ_true_Frame_t;typedef struct Main__overlap_1_EQ_true_Frame_t Main__overlap_1_EQ_true_Frame_t;
 /* declare_local */
 /* internal_declare_param */
 /* internal_declare_param */
BOOLEAN
__cdecl
Main__overlap_1_EQ_true(
   /* Param_Type1 */ T8A2831D7_8 /*TypeText1*/  a_L_165,
   /* Param_Type1 */ T8A2831D7_8 /*TypeText1*/  b_L_166);
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct Main__overlap_1_NE_false_Frame_t;typedef struct Main__overlap_1_NE_false_Frame_t Main__overlap_1_NE_false_Frame_t;
 /* declare_local */
 /* internal_declare_param */
 /* internal_declare_param */
BOOLEAN
__cdecl
Main__overlap_1_NE_false(
   /* Param_Type1 */ T8A2831D7_8 /*TypeText1*/  a_L_168,
   /* Param_Type1 */ T8A2831D7_8 /*TypeText1*/  b_L_169);
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct Main__ord_enum_vs_negative_LT_false_Frame_t;typedef struct Main__ord_enum_vs_negative_LT_false_Frame_t Main__ord_enum_vs_negative_LT_false_Frame_t;
 /* declare_local */
 /* internal_declare_param */
BOOLEAN
__cdecl
Main__ord_enum_vs_negative_LT_false(
   /* Param_Type1 */ Main__Number a_L_171);
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct Main__ord_enum_vs_negative_LE_false_Frame_t;typedef struct Main__ord_enum_vs_negative_LE_false_Frame_t Main__ord_enum_vs_negative_LE_false_Frame_t;
 /* declare_local */
 /* internal_declare_param */
BOOLEAN
__cdecl
Main__ord_enum_vs_negative_LE_false(
   /* Param_Type1 */ Main__Number a_L_173);
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct Main__ord_enum_vs_negative_GT_true_Frame_t;typedef struct Main__ord_enum_vs_negative_GT_true_Frame_t Main__ord_enum_vs_negative_GT_true_Frame_t;
 /* declare_local */
 /* internal_declare_param */
BOOLEAN
__cdecl
Main__ord_enum_vs_negative_GT_true(
   /* Param_Type1 */ Main__Number a_L_175);
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct Main__ord_enum_vs_negative_GE_true_Frame_t;typedef struct Main__ord_enum_vs_negative_GE_true_Frame_t Main__ord_enum_vs_negative_GE_true_Frame_t;
 /* declare_local */
 /* internal_declare_param */
BOOLEAN
__cdecl
Main__ord_enum_vs_negative_GE_true(
   /* Param_Type1 */ Main__Number a_L_177);
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct Main__ord_enum_vs_negative_EQ_false_Frame_t;typedef struct Main__ord_enum_vs_negative_EQ_false_Frame_t Main__ord_enum_vs_negative_EQ_false_Frame_t;
 /* declare_local */
 /* internal_declare_param */
BOOLEAN
__cdecl
Main__ord_enum_vs_negative_EQ_false(
   /* Param_Type1 */ Main__Number a_L_179);
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct Main__ord_enum_vs_negative_NE_true_Frame_t;typedef struct Main__ord_enum_vs_negative_NE_true_Frame_t Main__ord_enum_vs_negative_NE_true_Frame_t;
 /* declare_local */
 /* internal_declare_param */
BOOLEAN
__cdecl
Main__ord_enum_vs_negative_NE_true(
   /* Param_Type1 */ Main__Number a_L_181);
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct Main__abs_vs_negative_LT_false_Frame_t;typedef struct Main__abs_vs_negative_LT_false_Frame_t Main__abs_vs_negative_LT_false_Frame_t;
 /* declare_local */
 /* internal_declare_param */
BOOLEAN
__cdecl
Main__abs_vs_negative_LT_false(
   /* Param_Type1 */ INTEGER a_L_183);
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct Main__abs_vs_negative_LE_false_Frame_t;typedef struct Main__abs_vs_negative_LE_false_Frame_t Main__abs_vs_negative_LE_false_Frame_t;
 /* declare_local */
 /* internal_declare_param */
BOOLEAN
__cdecl
Main__abs_vs_negative_LE_false(
   /* Param_Type1 */ INTEGER a_L_185);
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct Main__abs_vs_negative_GT_true_Frame_t;typedef struct Main__abs_vs_negative_GT_true_Frame_t Main__abs_vs_negative_GT_true_Frame_t;
 /* declare_local */
 /* internal_declare_param */
BOOLEAN
__cdecl
Main__abs_vs_negative_GT_true(
   /* Param_Type1 */ INTEGER a_L_187);
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct Main__abs_vs_negative_GE_true_Frame_t;typedef struct Main__abs_vs_negative_GE_true_Frame_t Main__abs_vs_negative_GE_true_Frame_t;
 /* declare_local */
 /* internal_declare_param */
BOOLEAN
__cdecl
Main__abs_vs_negative_GE_true(
   /* Param_Type1 */ INTEGER a_L_189);
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct Main__abs_vs_negative_EQ_false_Frame_t;typedef struct Main__abs_vs_negative_EQ_false_Frame_t Main__abs_vs_negative_EQ_false_Frame_t;
 /* declare_local */
 /* internal_declare_param */
BOOLEAN
__cdecl
Main__abs_vs_negative_EQ_false(
   /* Param_Type1 */ INTEGER a_L_191);
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct Main__abs_vs_negative_NE_true_Frame_t;typedef struct Main__abs_vs_negative_NE_true_Frame_t Main__abs_vs_negative_NE_true_Frame_t;
 /* declare_local */
 /* internal_declare_param */
BOOLEAN
__cdecl
Main__abs_vs_negative_NE_true(
   /* Param_Type1 */ INTEGER a_L_193);
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct Main__abs_vs_zero_LT_false_Frame_t;typedef struct Main__abs_vs_zero_LT_false_Frame_t Main__abs_vs_zero_LT_false_Frame_t;
 /* declare_local */
 /* internal_declare_param */
BOOLEAN
__cdecl
Main__abs_vs_zero_LT_false(
   /* Param_Type1 */ INTEGER a_L_195);
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct Main__abs_vs_zero_GE_true_Frame_t;typedef struct Main__abs_vs_zero_GE_true_Frame_t Main__abs_vs_zero_GE_true_Frame_t;
 /* declare_local */
 /* internal_declare_param */
BOOLEAN
__cdecl
Main__abs_vs_zero_GE_true(
   /* Param_Type1 */ INTEGER a_L_197);
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct Main__neg_abs_vs_zero_LE_true_Frame_t;typedef struct Main__neg_abs_vs_zero_LE_true_Frame_t Main__neg_abs_vs_zero_LE_true_Frame_t;
 /* declare_local */
 /* internal_declare_param */
BOOLEAN
__cdecl
Main__neg_abs_vs_zero_LE_true(
   /* Param_Type1 */ INTEGER a_L_199);
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct Main__neg_abs_vs_zero_GT_false_Frame_t;typedef struct Main__neg_abs_vs_zero_GT_false_Frame_t Main__neg_abs_vs_zero_GT_false_Frame_t;
 /* declare_local */
 /* internal_declare_param */
BOOLEAN
__cdecl
Main__neg_abs_vs_zero_GT_false(
   /* Param_Type1 */ INTEGER a_L_201);
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct Main__neg_abs_vs_one_LT_true_Frame_t;typedef struct Main__neg_abs_vs_one_LT_true_Frame_t Main__neg_abs_vs_one_LT_true_Frame_t;
 /* declare_local */
 /* internal_declare_param */
BOOLEAN
__cdecl
Main__neg_abs_vs_one_LT_true(
   /* Param_Type1 */ INTEGER a_L_203);
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct Main__neg_abs_vs_one_LE_true_Frame_t;typedef struct Main__neg_abs_vs_one_LE_true_Frame_t Main__neg_abs_vs_one_LE_true_Frame_t;
 /* declare_local */
 /* internal_declare_param */
BOOLEAN
__cdecl
Main__neg_abs_vs_one_LE_true(
   /* Param_Type1 */ INTEGER a_L_205);
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct Main__neg_abs_vs_one_GT_false_Frame_t;typedef struct Main__neg_abs_vs_one_GT_false_Frame_t Main__neg_abs_vs_one_GT_false_Frame_t;
 /* declare_local */
 /* internal_declare_param */
BOOLEAN
__cdecl
Main__neg_abs_vs_one_GT_false(
   /* Param_Type1 */ INTEGER a_L_207);
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct Main__neg_abs_vs_one_GE_false_Frame_t;typedef struct Main__neg_abs_vs_one_GE_false_Frame_t Main__neg_abs_vs_one_GE_false_Frame_t;
 /* declare_local */
 /* internal_declare_param */
BOOLEAN
__cdecl
Main__neg_abs_vs_one_GE_false(
   /* Param_Type1 */ INTEGER a_L_209);
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct Main__neg_abs_vs_one_EQ_false_Frame_t;typedef struct Main__neg_abs_vs_one_EQ_false_Frame_t Main__neg_abs_vs_one_EQ_false_Frame_t;
 /* declare_local */
 /* internal_declare_param */
BOOLEAN
__cdecl
Main__neg_abs_vs_one_EQ_false(
   /* Param_Type1 */ INTEGER a_L_211);
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct Main__neg_abs_vs_one_NE_true_Frame_t;typedef struct Main__neg_abs_vs_one_NE_true_Frame_t Main__neg_abs_vs_one_NE_true_Frame_t;
 /* declare_local */
 /* internal_declare_param */
BOOLEAN
__cdecl
Main__neg_abs_vs_one_NE_true(
   /* Param_Type1 */ INTEGER a_L_213);
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
 /* init_int */
 /* init_var */
 /* init_int */
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
 /* end_init */
struct Main_m_20_L_21_t{ADDRESS L_216[5];
INT64 L_217[1];
ADDRESS L_218[1];
INT64 L_219[1];
UINT8 L_220[25];
char L_221[7];
INT64 L_222[1];
ADDRESS L_223[1];
INT64 L_224[1];
UINT8 L_225[20];
char L_226[4];
INT64 L_227[1];
ADDRESS L_228[1];
INT64 L_229[1];
UINT8 L_230[28];
char L_231[4];
INT64 L_232[1];
ADDRESS L_233[1];
INT64 L_234[1];
UINT8 L_235[28];
char L_236[4];
INT64 L_237[1];
ADDRESS L_238[1];
INT64 L_239[1];
UINT8 L_240[23];
char L_241[1];
INT64 L_242[1];
ADDRESS L_243[1];
INT64 L_244[1];
UINT8 L_245[23];
char L_246[1];
INT64 L_247[1];
ADDRESS L_248[1];
INT64 L_249[1];
UINT8 L_250[23];
char L_251[1];
INT64 L_252[1];
ADDRESS L_253[1];
INT64 L_254[1];
UINT8 L_255[28];
char L_256[4];
INT64 L_257[1];
ADDRESS L_258[1];
INT64 L_259[1];
UINT8 L_260[28];
char L_261[4];
INT64 L_262[1];
ADDRESS L_263[1];
INT64 L_264[1];
UINT8 L_265[28];
char L_266[4];
INT64 L_267[1];
ADDRESS L_268[1];
INT64 L_269[1];
UINT8 L_270[23];
char L_271[1];
INT64 L_272[1];
ADDRESS L_273[1];
INT64 L_274[1];
UINT8 L_275[23];
char L_276[1];
INT64 L_277[1];
ADDRESS L_278[1];
INT64 L_279[1];
UINT8 L_280[23];
char L_281[1];
INT64 L_282[1];
ADDRESS L_283[1];
INT64 L_284[1];
UINT8 L_285[28];
char L_286[4];
INT64 L_287[1];
ADDRESS L_288[1];
INT64 L_289[1];
UINT8 L_290[26];
char L_291[6];
INT64 L_292[1];
ADDRESS L_293[1];
INT64 L_294[1];
UINT8 L_295[21];
char L_296[3];
INT64 L_297[1];
ADDRESS L_298[1];
INT64 L_299[1];
UINT8 L_300[29];
char L_301[3];
INT64 L_302[1];
ADDRESS L_303[1];
INT64 L_304[1];
UINT8 L_305[29];
char L_306[3];
INT64 L_307[1];
ADDRESS L_308[1];
INT64 L_309[1];
UINT8 L_310[34];
char L_311[6];
INT64 L_312[1];
ADDRESS L_313[1];
INT64 L_314[1];
UINT8 L_315[34];
char L_316[6];
INT64 L_317[1];
ADDRESS L_318[1];
INT64 L_319[1];
UINT8 L_320[34];
char L_321[6];
INT64 L_322[1];
ADDRESS L_323[1];
INT64 L_324[1];
UINT8 L_325[29];
char L_326[3];
INT64 L_327[1];
ADDRESS L_328[1];
INT64 L_329[1];
UINT8 L_330[34];
char L_331[6];
INT64 L_332[1];
ADDRESS L_333[1];
INT64 L_334[1];
UINT8 L_335[39];
char L_336[1];
INT64 L_337[1];
ADDRESS L_338[1];
INT64 L_339[1];
UINT8 L_340[37];
char L_341[3];
INT64 L_342[1];
ADDRESS L_343[1];
INT64 L_344[1];
UINT8 L_345[37];
char L_346[3];
INT64 L_347[1];
ADDRESS L_348[1];
INT64 L_349[1];
UINT8 L_350[32];
char L_351[8];
INT64 L_352[1];
ADDRESS L_353[1];
INT64 L_354[1];
UINT8 L_355[32];
char L_356[8];
INT64 L_357[1];
ADDRESS L_358[1];
INT64 L_359[1];
UINT8 L_360[37];
char L_361[3];
INT64 L_362[1];
ADDRESS L_363[1];
INT64 L_364[1];
UINT8 L_365[32];
char L_366[8];
INT64 L_367[1];
ADDRESS L_368[1];
INT64 L_369[1];
UINT8 L_370[42];
char L_371[6];
INT64 L_372[1];
ADDRESS L_373[1];
INT64 L_374[1];
UINT8 L_375[37];
char L_376[3];
INT64 L_377[1];
ADDRESS L_378[1];
INT64 L_379[1];
UINT8 L_380[55];
char L_381[1];
INT64 L_382[1];
ADDRESS L_383[1];
INT64 L_384[1];
UINT8 L_385[55];
char L_387[1];
INT64 L_388[1];
ADDRESS L_389[1];
INT64 L_390[1];
UINT8 L_391[60];
char L_392[4];
INT64 L_393[1];
ADDRESS L_394[1];
INT64 L_395[1];
UINT8 L_396[60];
char L_397[4];
INT64 L_398[1];
ADDRESS L_399[1];
INT64 L_400[1];
UINT8 L_401[60];
char L_402[4];
INT64 L_403[1];
ADDRESS L_404[1];
INT64 L_405[1];
UINT8 L_406[55];
char L_407[1];
INT64 L_408[1];
ADDRESS L_409[1];
INT64 L_410[1];
UINT8 L_411[59];
char L_412[5];
INT64 L_413[1];
ADDRESS L_414[1];
INT64 L_415[1];
UINT8 L_416[64];
char L_417[8];
INT64 L_418[1];
ADDRESS L_419[1];
INT64 L_420[1];
UINT8 L_421[63];
char L_422[1];
INT64 L_423[1];
ADDRESS L_424[1];
INT64 L_425[1];
UINT8 L_426[63];
char L_427[1];
INT64 L_428[1];
ADDRESS L_429[1];
INT64 L_430[1];
UINT8 L_431[58];
char L_432[6];
INT64 L_433[1];
ADDRESS L_434[1];
INT64 L_435[1];
UINT8 L_436[58];
char L_437[6];
INT64 L_438[1];
ADDRESS L_439[1];
INT64 L_440[1];
UINT8 L_441[63];
char L_442[1];
INT64 L_443[1];
ADDRESS L_444[1];
INT64 L_445[1];
UINT8 L_446[58];
char L_447[6];
INT64 L_448[1];
ADDRESS L_449[1];
INT64 L_450[1];
UINT8 L_451[67];
char L_452[5];
INT64 L_453[1];
ADDRESS L_454[1];
INT64 L_455[1];
UINT8 L_456[62];
char L_457[2];
INT64 L_458[1];
ADDRESS L_459[1];
INT64 L_460[1];
UINT8 L_461[28];
char L_462[4];
INT64 L_463[1];
ADDRESS L_464[1];
INT64 L_465[1];
UINT8 L_466[23];
char L_467[1];
INT64 L_468[1];
ADDRESS L_469[1];
INT64 L_470[1];
UINT8 L_471[28];
char L_472[4];
INT64 L_473[1];
ADDRESS L_474[1];
INT64 L_475[1];
UINT8 L_476[23];
char L_477[1];
INT64 L_478[1];
ADDRESS L_479[1];
INT64 L_480[1];
UINT8 L_481[23];
char L_482[1];
INT64 L_483[1];
ADDRESS L_484[1];
INT64 L_485[1];
UINT8 L_486[28];
char L_487[4];
INT64 L_488[1];
ADDRESS L_489[1];
INT64 L_490[1];
UINT8 L_491[47];
char L_492[1];
INT64 L_493[1];
ADDRESS L_494[1];
INT64 L_495[1];
UINT8 L_496[47];
char L_497[1];
INT64 L_498[1];
ADDRESS L_499[1];
INT64 L_500[1];
UINT8 L_501[42];
char L_502[6];
INT64 L_503[1];
ADDRESS L_504[1];
INT64 L_505[1];
UINT8 L_506[42];
char L_507[6];
INT64 L_508[1];
ADDRESS L_509[1];
INT64 L_510[1];
UINT8 L_511[47];
char L_512[1];
INT64 L_513[1];
ADDRESS L_514[1];
INT64 L_515[1];
UINT8 L_516[42];
char L_517[6];
INT64 L_518[1];
ADDRESS L_519[1];
INT64 L_520[1];
UINT8 L_521[32];
char L_522[8];
INT64 L_523[1];
ADDRESS L_524[1];
INT64 L_525[1];
UINT8 L_526[32];
char L_527[8];
INT64 L_528[1];
ADDRESS L_529[1];
INT64 L_530[1];
UINT8 L_531[27];
char L_532[5];
INT64 L_533[1];
ADDRESS L_534[1];
INT64 L_535[1];
UINT8 L_536[27];
char L_537[5];
INT64 L_538[1];
ADDRESS L_539[1];
INT64 L_540[1];
UINT8 L_541[32];
char L_542[8];
INT64 L_543[1];
ADDRESS L_544[1];
INT64 L_545[1];
UINT8 L_546[27];
char L_547[5];
INT64 L_548[1];
ADDRESS L_549[1];
INT64 L_550[1];
UINT8 L_551[28];
char L_552[4];
INT64 L_553[1];
ADDRESS L_554[1];
INT64 L_555[1];
UINT8 L_556[23];
char L_557[1];
INT64 L_558[1];
ADDRESS L_559[1];
INT64 L_560[1];
UINT8 L_561[27];
char L_562[5];
INT64 L_563[1];
ADDRESS L_564[1];
INT64 L_565[1];
UINT8 L_566[32];
char L_567[8];
INT64 L_568[1];
ADDRESS L_569[1];
INT64 L_570[1];
UINT8 L_571[26];
char L_572[6];
INT64 L_573[1];
ADDRESS L_574[1];
INT64 L_575[1];
UINT8 L_576[26];
char L_577[6];
INT64 L_578[1];
ADDRESS L_579[1];
INT64 L_580[1];
UINT8 L_581[31];
char L_582[1];
INT64 L_583[1];
ADDRESS L_584[1];
INT64 L_585[1];
UINT8 L_586[31];
char L_587[1];
INT64 L_588[1];
ADDRESS L_589[1];
INT64 L_590[1];
UINT8 L_591[31];
char L_592[1];
INT64 L_593[1];
ADDRESS L_594[1];
INT64 L_595[1];
UINT8 L_596[26];
char L_597[6];
UINT8 L_598[7];
char L_599[1];
UINT8 L_600[22];
char L_601[1];
UINT8 L_602[23];
char L_603[1];
UINT8 L_604[23];
char L_605[1];
UINT8 L_606[23];
char L_607[1];
UINT8 L_608[22];
char L_609[1];
UINT8 L_610[22];
char L_611[1];
UINT8 L_612[24];
char L_613[1];
UINT8 L_614[23];
char L_615[1];
UINT8 L_616[19];
char L_617[1];
UINT8 L_618[20];
char L_619[1];
UINT8 L_620[23];
char L_621[1];
UINT8 L_622[24];
char L_623[1];
UINT8 L_624[23];
char L_625[1];
UINT8 L_626[23];
char L_627[1];
UINT8 L_628[24];
char L_629[1];
UINT8 L_630[24];
char L_631[1];
UINT8 L_632[28];
char L_633[1];
UINT8 L_634[29];
char L_635[1];
UINT8 L_636[28];
char L_637[1];
UINT8 L_638[28];
char L_639[1];
UINT8 L_640[29];
char L_641[1];
UINT8 L_642[29];
char L_643[1];
UINT8 L_644[18];
char L_645[1];
UINT8 L_646[17];
char L_647[1];
UINT8 L_648[17];
char L_649[1];
UINT8 L_650[18];
char L_651[1];
UINT8 L_652[17];
char L_653[1];
UINT8 L_654[18];
char L_655[1];
UINT8 L_656[36];
char L_657[1];
UINT8 L_658[37];
char L_659[1];
UINT8 L_660[31];
char L_661[1];
UINT8 L_662[32];
char L_663[1];
UINT8 L_664[31];
char L_665[1];
UINT8 L_666[31];
char L_667[1];
UINT8 L_668[32];
char L_669[1];
UINT8 L_670[32];
char L_671[1];
UINT8 L_672[34];
char L_673[1];
UINT8 L_674[33];
char L_675[1];
UINT8 L_676[28];
char L_677[1];
UINT8 L_678[29];
char L_679[1];
UINT8 L_680[29];
char L_681[1];
UINT8 L_682[29];
char L_683[1];
UINT8 L_684[28];
char L_685[1];
UINT8 L_686[28];
char L_687[1];
UINT8 L_688[31];
char L_689[1];
UINT8 L_690[32];
char L_691[1];
UINT8 L_692[26];
char L_693[1];
UINT8 L_694[27];
char L_695[1];
UINT8 L_696[26];
char L_697[1];
UINT8 L_698[26];
char L_699[1];
UINT8 L_700[27];
char L_701[1];
UINT8 L_702[27];
char L_703[1];
UINT8 L_704[29];
char L_705[1];
UINT8 L_706[28];
char L_707[1];
UINT8 L_708[23];
char L_709[1];
UINT8 L_710[24];
char L_711[1];
UINT8 L_712[24];
char L_713[1];
UINT8 L_714[24];
char L_715[1];
UINT8 L_716[23];
char L_717[1];
UINT8 L_718[23];
char L_719[1];
UINT8 L_720[16];
char L_721[1];
UINT8 L_722[17];
char L_723[1];
UINT8 L_724[20];
char L_725[1];
UINT8 L_726[19];
char L_727[1];
UINT8 L_728[19];
char L_729[1];
UINT8 L_730[19];
char L_731[1];
UINT8 L_732[20];
char L_733[1];
UINT8 L_734[20];
char L_735[1];
UINT8 L_736[20];
char L_737[1];
UINT8 L_738[19];
char L_739[1];
UINT8 L_740[19];
char L_741[1];
UINT8 L_742[19];
char L_743[1];
UINT8 L_744[20];
char L_745[1];
UINT8 L_746[20];
char L_747[1];
UINT8 L_748[16];
char L_749[1];
UINT8 L_750[17];
char L_751[3];
ADDRESS L_752[154];
char L_753[8];
UINT8 L_754[10];
char L_755[6];
};
static  const Main_m_20_L_21_t Main_m_20_L_21={{(ADDRESS)&RTHooks__TextLitInfo,(ADDRESS)&RTHooks__TextLitGetChar,(ADDRESS)&RTHooks__TextLitGetWideChar,(ADDRESS)&RTHooks__TextLitGetChars,(ADDRESS)&RTHooks__TextLitGetWideChars},{INT64_(2)},{(char*)&Main_m_20_L_21},{INT64_(25)},{'N','O','T',' ','C','a','r','d','i','n','a','l','L','T','0','_','f','a','l','s','e','(','0',')',' '},{0 /* 1 */ ,0 /* 2 */ ,0 /* 3 */ ,0 /* 4 */ ,0 /* 5 */ ,0 /* 6 */ ,0 /* 7 */ ,},{INT64_(2)},{(char*)&Main_m_20_L_21},{INT64_(20)},{'C','a','r','d','i','n','a','l','G','E','0','_','t','r','u','e','(','0',')',' '},{0 /* 1 */ ,0 /* 2 */ ,0 /* 3 */ ,0 /* 4 */ ,},{INT64_(2)},{(char*)&Main_m_20_L_21},{INT64_(28)},{'N','O','T',' ','C','a','r','d','i','n','a','l','L','T','N','e','g','1','_','f','a','l','s','e','(','0',')',' '},{0 /* 1 */ ,0 /* 2 */ ,0 /* 3 */ ,0 /* 4 */ ,},{INT64_(2)},{(char*)&Main_m_20_L_21},{INT64_(28)},{'N','O','T',' ','C','a','r','d','i','n','a','l','L','E','N','e','g','1','_','f','a','l','s','e','(','0'
,')',' '},{0 /* 1 */ ,0 /* 2 */ ,0 /* 3 */ ,0 /* 4 */ ,},{INT64_(2)},{(char*)&Main_m_20_L_21},{INT64_(23)},{'C','a','r','d','i','n','a','l','G','T','N','e','g','1','_','t','r','u','e','(','0',')',' '},{0 /* 1 */ ,},{INT64_(2)},{(char*)&Main_m_20_L_21},{INT64_(23)},{'C','a','r','d','i','n','a','l','G','E','N','e','g','1','_','t','r','u','e','(','0',')',' '},{0 /* 1 */ ,},{INT64_(2)},{(char*)&Main_m_20_L_21},{INT64_(23)},{'C','a','r','d','i','n','a','l','N','E','N','e','g','1','_','t','r','u','e','(','0',')',' '},{0 /* 1 */ ,},{INT64_(2)},{(char*)&Main_m_20_L_21},{INT64_(28)},{'N','O','T',' ','C','a','r','d','i','n','a','l','E','Q','N','e','g','1','_','f','a','l','s','e','(','0',')',' '},{0 /* 1 */ ,0 /* 2 */ ,0 /* 3 */ ,0 /* 4 */ ,},{INT64_(2)},{(char*)&Main_m_20_L_21},{INT64_(28)},{'N','O','T',' ','C','a','r','d','i','n','a','l','L','T','N','e','g','2','_','f','a','l','s','e','(','0',')',' '},{0 /* 1 */ ,0 /* 2 */ ,0 /* 3 */ ,0 /* 4 */ ,},{INT64_(2)},{(char*)&Main_m_20_L_21},{INT64_(28)
},{'N','O','T',' ','C','a','r','d','i','n','a','l','L','E','N','e','g','2','_','f','a','l','s','e','(','0',')',' '},{0 /* 1 */ ,0 /* 2 */ ,0 /* 3 */ ,0 /* 4 */ ,},{INT64_(2)},{(char*)&Main_m_20_L_21},{INT64_(23)},{'C','a','r','d','i','n','a','l','G','T','N','e','g','2','_','t','r','u','e','(','0',')',' '},{0 /* 1 */ ,},{INT64_(2)},{(char*)&Main_m_20_L_21},{INT64_(23)},{'C','a','r','d','i','n','a','l','G','E','N','e','g','2','_','t','r','u','e','(','0',')',' '},{0 /* 1 */ ,},{INT64_(2)},{(char*)&Main_m_20_L_21},{INT64_(23)},{'C','a','r','d','i','n','a','l','N','E','N','e','g','2','_','t','r','u','e','(','0',')',' '},{0 /* 1 */ ,},{INT64_(2)},{(char*)&Main_m_20_L_21},{INT64_(28)},{'N','O','T',' ','C','a','r','d','i','n','a','l','E','Q','N','e','g','2','_','f','a','l','s','e','(','0',')',' '},{0 /* 1 */ ,0 /* 2 */ ,0 /* 3 */ ,0 /* 4 */ ,},{INT64_(2)},{(char*)&Main_m_20_L_21},{INT64_(26)},{'N','O','T',' ','L','o','n','g','c','a','r','d','L','T','0','_','f','a','l','s','e','(','0','L',')',' '
},{0 /* 1 */ ,0 /* 2 */ ,0 /* 3 */ ,0 /* 4 */ ,0 /* 5 */ ,0 /* 6 */ ,},{INT64_(2)},{(char*)&Main_m_20_L_21},{INT64_(21)},{'L','o','n','g','c','a','r','d','G','E','0','_','t','r','u','e','(','0','L',')',' '},{0 /* 1 */ ,0 /* 2 */ ,0 /* 3 */ ,},{INT64_(2)},{(char*)&Main_m_20_L_21},{INT64_(29)},{'n','o','_','o','v','e','r','l','a','p','_','l','e','s','s','_','L','T','_','t','r','u','e','(','0',',','2',')',' '},{0 /* 1 */ ,0 /* 2 */ ,0 /* 3 */ ,},{INT64_(2)},{(char*)&Main_m_20_L_21},{INT64_(29)},{'n','o','_','o','v','e','r','l','a','p','_','l','e','s','s','_','L','E','_','t','r','u','e','(','0',',','2',')',' '},{0 /* 1 */ ,0 /* 2 */ ,0 /* 3 */ ,},{INT64_(2)},{(char*)&Main_m_20_L_21},{INT64_(34)},{'N','O','T',' ','n','o','_','o','v','e','r','l','a','p','_','l','e','s','s','_','G','T','_','f','a','l','s','e','(','0',',','2',')',' '},{0 /* 1 */ ,0 /* 2 */ ,0 /* 3 */ ,0 /* 4 */ ,0 /* 5 */ ,0 /* 6 */ ,},{INT64_(2)},{(char*)&Main_m_20_L_21},{INT64_(34)},{'N','O','T',' ','n','o','_','o','v','e','r'
,'l','a','p','_','l','e','s','s','_','G','E','_','f','a','l','s','e','(','0',',','2',')',' '},{0 /* 1 */ ,0 /* 2 */ ,0 /* 3 */ ,0 /* 4 */ ,0 /* 5 */ ,0 /* 6 */ ,},{INT64_(2)},{(char*)&Main_m_20_L_21},{INT64_(34)},{'N','O','T',' ','n','o','_','o','v','e','r','l','a','p','_','l','e','s','s','_','E','Q','_','f','a','l','s','e','(','0',',','2',')',' '},{0 /* 1 */ ,0 /* 2 */ ,0 /* 3 */ ,0 /* 4 */ ,0 /* 5 */ ,0 /* 6 */ ,},{INT64_(2)},{(char*)&Main_m_20_L_21},{INT64_(29)},{'n','o','_','o','v','e','r','l','a','p','_','l','e','s','s','_','N','E','_','t','r','u','e','(','0',',','2',')',' '},{0 /* 1 */ ,0 /* 2 */ ,0 /* 3 */ ,},{INT64_(2)},{(char*)&Main_m_20_L_21},{INT64_(34)},{'m','i','n','i','m','u','m','_','o','v','e','r','l','a','p','_','l','e','s','s','_','L','E','_','t','r','u','e','(','0',',','1',')',' '},{0 /* 1 */ ,0 /* 2 */ ,0 /* 3 */ ,0 /* 4 */ ,0 /* 5 */ ,0 /* 6 */ ,},{INT64_(2)},{(char*)&Main_m_20_L_21},{INT64_(39)},{'N','O','T',' ','m','i','n','i','m','u','m','_','o','v','e','r','l',
'a','p','_','l','e','s','s','_','G','T','_','f','a','l','s','e','(','0',',','1',')',' '},{0 /* 1 */ ,},{INT64_(2)},{(char*)&Main_m_20_L_21},{INT64_(37)},{'N','O','T',' ','n','o','_','o','v','e','r','l','a','p','_','g','r','e','a','t','e','r','_','L','T','_','f','a','l','s','e','(','2',',','0',')',' '},{0 /* 1 */ ,0 /* 2 */ ,0 /* 3 */ ,},{INT64_(2)},{(char*)&Main_m_20_L_21},{INT64_(37)},{'N','O','T',' ','n','o','_','o','v','e','r','l','a','p','_','g','r','e','a','t','e','r','_','L','E','_','f','a','l','s','e','(','2',',','0',')',' '},{0 /* 1 */ ,0 /* 2 */ ,0 /* 3 */ ,},{INT64_(2)},{(char*)&Main_m_20_L_21},{INT64_(32)},{'n','o','_','o','v','e','r','l','a','p','_','g','r','e','a','t','e','r','_','G','T','_','t','r','u','e','(','2',',','0',')',' '},{0 /* 1 */ ,0 /* 2 */ ,0 /* 3 */ ,0 /* 4 */ ,0 /* 5 */ ,0 /* 6 */ ,0 /* 7 */ ,0 /* 8 */ ,},{INT64_(2)},{(char*)&Main_m_20_L_21},{INT64_(32)},{'n','o','_','o','v','e','r','l','a','p','_','g','r','e','a','t','e','r','_','G','E','_','t','r','u','e'
,'(','2',',','0',')',' '},{0 /* 1 */ ,0 /* 2 */ ,0 /* 3 */ ,0 /* 4 */ ,0 /* 5 */ ,0 /* 6 */ ,0 /* 7 */ ,0 /* 8 */ ,},{INT64_(2)},{(char*)&Main_m_20_L_21},{INT64_(37)},{'N','O','T',' ','n','o','_','o','v','e','r','l','a','p','_','g','r','e','a','t','e','r','_','E','Q','_','f','a','l','s','e','(','2',',','0',')',' '},{0 /* 1 */ ,0 /* 2 */ ,0 /* 3 */ ,},{INT64_(2)},{(char*)&Main_m_20_L_21},{INT64_(32)},{'n','o','_','o','v','e','r','l','a','p','_','g','r','e','a','t','e','r','_','N','E','_','t','r','u','e','(','2',',','0',')',' '},{0 /* 1 */ ,0 /* 2 */ ,0 /* 3 */ ,0 /* 4 */ ,0 /* 5 */ ,0 /* 6 */ ,0 /* 7 */ ,0 /* 8 */ ,},{INT64_(2)},{(char*)&Main_m_20_L_21},{INT64_(42)},{'N','O','T',' ','m','i','n','i','m','u','m','_','o','v','e','r','l','a','p','_','g','r','e','a','t','e','r','_','L','T','_','f','a','l','s','e','(','1',',','0',')',' '},{0 /* 1 */ ,0 /* 2 */ ,0 /* 3 */ ,0 /* 4 */ ,0 /* 5 */ ,0 /* 6 */ ,},{INT64_(2)},{(char*)&Main_m_20_L_21},{INT64_(37)},{'m','i','n','i','m','u','m','_','o',
'v','e','r','l','a','p','_','g','r','e','a','t','e','r','_','G','E','_','t','r','u','e','(','1',',','0',')',' '},{0 /* 1 */ ,0 /* 2 */ ,0 /* 3 */ ,},{INT64_(2)},{(char*)&Main_m_20_L_21},{INT64_(55)},{'n','o','_','o','v','e','r','l','a','p','_','l','e','s','s','_','e','n','u','m','_','L','T','_','t','r','u','e','(','N','u','m','b','e','r','.','Z','e','r','o',',',' ','N','u','m','b','e','r','.','F','o','u','r',')',' '},{0 /* 1 */ ,},{INT64_(2)},{(char*)&Main_m_20_L_21},{INT64_(55)},{'n','o','_','o','v','e','r','l','a','p','_','l','e','s','s','_','e','n','u','m','_','L','E','_','t','r','u','e','(','N','u','m','b','e','r','.','Z','e','r','o',',',' ','N','u','m','b','e','r','.','F','o','u','r',')',' '},{0 /* 1 */ ,},{INT64_(2)},{(char*)&Main_m_20_L_21},{INT64_(60)},{'N','O','T',' ','n','o','_','o','v','e','r','l','a','p','_','l','e','s','s','_','e','n','u','m','_','G','T','_','f','a','l','s','e','(','N','u','m','b','e','r','.','Z','e','r','o',',',' ','N','u','m','b','e','r','.','F','o','u',
'r',')',' '},{0 /* 1 */ ,0 /* 2 */ ,0 /* 3 */ ,0 /* 4 */ ,},{INT64_(2)},{(char*)&Main_m_20_L_21},{INT64_(60)},{'N','O','T',' ','n','o','_','o','v','e','r','l','a','p','_','l','e','s','s','_','e','n','u','m','_','G','E','_','f','a','l','s','e','(','N','u','m','b','e','r','.','Z','e','r','o',',',' ','N','u','m','b','e','r','.','F','o','u','r',')',' '},{0 /* 1 */ ,0 /* 2 */ ,0 /* 3 */ ,0 /* 4 */ ,},{INT64_(2)},{(char*)&Main_m_20_L_21},{INT64_(60)},{'N','O','T',' ','n','o','_','o','v','e','r','l','a','p','_','l','e','s','s','_','e','n','u','m','_','E','Q','_','f','a','l','s','e','(','N','u','m','b','e','r','.','Z','e','r','o',',',' ','N','u','m','b','e','r','.','F','o','u','r',')',' '},{0 /* 1 */ ,0 /* 2 */ ,0 /* 3 */ ,0 /* 4 */ ,},{INT64_(2)},{(char*)&Main_m_20_L_21},{INT64_(55)},{'n','o','_','o','v','e','r','l','a','p','_','l','e','s','s','_','e','n','u','m','_','N','E','_','t','r','u','e','(','N','u','m','b','e','r','.','Z','e','r','o',',',' ','N','u','m','b','e','r','.','F','o','u','r'
,')',' '},{0 /* 1 */ ,},{INT64_(2)},{(char*)&Main_m_20_L_21},{INT64_(59)},{'m','i','n','i','m','u','m','_','o','v','e','r','l','a','p','_','l','e','s','s','_','e','n','u','m','_','L','E','_','t','r','u','e','(','N','u','m','b','e','r','.','Z','e','r','o',',',' ','N','u','m','b','e','r','.','O','n','e',')',' '},{0 /* 1 */ ,0 /* 2 */ ,0 /* 3 */ ,0 /* 4 */ ,0 /* 5 */ ,},{INT64_(2)},{(char*)&Main_m_20_L_21},{INT64_(64)},{'N','O','T',' ','m','i','n','i','m','u','m','_','o','v','e','r','l','a','p','_','l','e','s','s','_','e','n','u','m','_','G','T','_','f','a','l','s','e','(','N','u','m','b','e','r','.','Z','e','r','o',',',' ','N','u','m','b','e','r','.','O','n','e',')',' '},{0 /* 1 */ ,0 /* 2 */ ,0 /* 3 */ ,0 /* 4 */ ,0 /* 5 */ ,0 /* 6 */ ,0 /* 7 */ ,0 /* 8 */ ,},{INT64_(2)},{(char*)&Main_m_20_L_21},{INT64_(63)},{'N','O','T',' ','n','o','_','o','v','e','r','l','a','p','_','g','r','e','a','t','e','r','_','e','n','u','m','_','L','T','_','f','a','l','s','e','(','N','u','m','b','e','r','.','F',
'o','u','r',',',' ','N','u','m','b','e','r','.','Z','e','r','o',')',' '},{0 /* 1 */ ,},{INT64_(2)},{(char*)&Main_m_20_L_21},{INT64_(63)},{'N','O','T',' ','n','o','_','o','v','e','r','l','a','p','_','g','r','e','a','t','e','r','_','e','n','u','m','_','L','E','_','f','a','l','s','e','(','N','u','m','b','e','r','.','F','o','u','r',',',' ','N','u','m','b','e','r','.','Z','e','r','o',')',' '},{0 /* 1 */ ,},{INT64_(2)},{(char*)&Main_m_20_L_21},{INT64_(58)},{'n','o','_','o','v','e','r','l','a','p','_','g','r','e','a','t','e','r','_','e','n','u','m','_','G','T','_','t','r','u','e','(','N','u','m','b','e','r','.','F','o','u','r',',',' ','N','u','m','b','e','r','.','Z','e','r','o',')',' '},{0 /* 1 */ ,0 /* 2 */ ,0 /* 3 */ ,0 /* 4 */ ,0 /* 5 */ ,0 /* 6 */ ,},{INT64_(2)},{(char*)&Main_m_20_L_21},{INT64_(58)},{'n','o','_','o','v','e','r','l','a','p','_','g','r','e','a','t','e','r','_','e','n','u','m','_','G','E','_','t','r','u','e','(','N','u','m','b','e','r','.','F','o','u','r',',',' ','N','u','m'
,'b','e','r','.','Z','e','r','o',')',' '},{0 /* 1 */ ,0 /* 2 */ ,0 /* 3 */ ,0 /* 4 */ ,0 /* 5 */ ,0 /* 6 */ ,},{INT64_(2)},{(char*)&Main_m_20_L_21},{INT64_(63)},{'N','O','T',' ','n','o','_','o','v','e','r','l','a','p','_','g','r','e','a','t','e','r','_','e','n','u','m','_','E','Q','_','f','a','l','s','e','(','N','u','m','b','e','r','.','F','o','u','r',',',' ','N','u','m','b','e','r','.','Z','e','r','o',')',' '},{0 /* 1 */ ,},{INT64_(2)},{(char*)&Main_m_20_L_21},{INT64_(58)},{'n','o','_','o','v','e','r','l','a','p','_','g','r','e','a','t','e','r','_','e','n','u','m','_','N','E','_','t','r','u','e','(','N','u','m','b','e','r','.','F','o','u','r',',',' ','N','u','m','b','e','r','.','Z','e','r','o',')',' '},{0 /* 1 */ ,0 /* 2 */ ,0 /* 3 */ ,0 /* 4 */ ,0 /* 5 */ ,0 /* 6 */ ,},{INT64_(2)},{(char*)&Main_m_20_L_21},{INT64_(67)},{'N','O','T',' ','m','i','n','i','m','u','m','_','o','v','e','r','l','a','p','_','g','r','e','a','t','e','r','_','e','n','u','m','_','L','T','_','f','a','l','s','e','('
,'N','u','m','b','e','r','.','O','n','e',',',' ','N','u','m','b','e','r','.','Z','e','r','o',')',' '},{0 /* 1 */ ,0 /* 2 */ ,0 /* 3 */ ,0 /* 4 */ ,0 /* 5 */ ,},{INT64_(2)},{(char*)&Main_m_20_L_21},{INT64_(62)},{'m','i','n','i','m','u','m','_','o','v','e','r','l','a','p','_','g','r','e','a','t','e','r','_','e','n','u','m','_','G','E','_','t','r','u','e','(','N','u','m','b','e','r','.','O','n','e',',',' ','N','u','m','b','e','r','.','Z','e','r','o',')',' '},{0 /* 1 */ ,0 /* 2 */ ,},{INT64_(2)},{(char*)&Main_m_20_L_21},{INT64_(28)},{'N','O','T',' ','o','v','e','r','l','a','p','_','1','_','L','T','_','f','a','l','s','e','(','0',',','0',')',' '},{0 /* 1 */ ,0 /* 2 */ ,0 /* 3 */ ,0 /* 4 */ ,},{INT64_(2)},{(char*)&Main_m_20_L_21},{INT64_(23)},{'o','v','e','r','l','a','p','_','1','_','L','E','_','t','r','u','e','(','0',',','0',')',' '},{0 /* 1 */ ,},{INT64_(2)},{(char*)&Main_m_20_L_21},{INT64_(28)},{'N','O','T',' ','o','v','e','r','l','a','p','_','1','_','G','T','_','f','a','l','s','e','(','0'
,',','0',')',' '},{0 /* 1 */ ,0 /* 2 */ ,0 /* 3 */ ,0 /* 4 */ ,},{INT64_(2)},{(char*)&Main_m_20_L_21},{INT64_(23)},{'o','v','e','r','l','a','p','_','1','_','G','E','_','t','r','u','e','(','0',',','0',')',' '},{0 /* 1 */ ,},{INT64_(2)},{(char*)&Main_m_20_L_21},{INT64_(23)},{'o','v','e','r','l','a','p','_','1','_','E','Q','_','t','r','u','e','(','0',',','0',')',' '},{0 /* 1 */ ,},{INT64_(2)},{(char*)&Main_m_20_L_21},{INT64_(28)},{'N','O','T',' ','o','v','e','r','l','a','p','_','1','_','N','E','_','f','a','l','s','e','(','0',',','0',')',' '},{0 /* 1 */ ,0 /* 2 */ ,0 /* 3 */ ,0 /* 4 */ ,},{INT64_(2)},{(char*)&Main_m_20_L_21},{INT64_(47)},{'N','O','T',' ','o','r','d','_','e','n','u','m','_','v','s','_','n','e','g','a','t','i','v','e','_','L','T','_','f','a','l','s','e','(','N','u','m','b','e','r','.','Z','e','r','o',')',' '},{0 /* 1 */ ,},{INT64_(2)},{(char*)&Main_m_20_L_21},{INT64_(47)},{'N','O','T',' ','o','r','d','_','e','n','u','m','_','v','s','_','n','e','g','a','t','i','v','e','_','L'
,'E','_','f','a','l','s','e','(','N','u','m','b','e','r','.','Z','e','r','o',')',' '},{0 /* 1 */ ,},{INT64_(2)},{(char*)&Main_m_20_L_21},{INT64_(42)},{'o','r','d','_','e','n','u','m','_','v','s','_','n','e','g','a','t','i','v','e','_','G','T','_','t','r','u','e','(','N','u','m','b','e','r','.','Z','e','r','o',')',' '},{0 /* 1 */ ,0 /* 2 */ ,0 /* 3 */ ,0 /* 4 */ ,0 /* 5 */ ,0 /* 6 */ ,},{INT64_(2)},{(char*)&Main_m_20_L_21},{INT64_(42)},{'o','r','d','_','e','n','u','m','_','v','s','_','n','e','g','a','t','i','v','e','_','G','E','_','t','r','u','e','(','N','u','m','b','e','r','.','Z','e','r','o',')',' '},{0 /* 1 */ ,0 /* 2 */ ,0 /* 3 */ ,0 /* 4 */ ,0 /* 5 */ ,0 /* 6 */ ,},{INT64_(2)},{(char*)&Main_m_20_L_21},{INT64_(47)},{'N','O','T',' ','o','r','d','_','e','n','u','m','_','v','s','_','n','e','g','a','t','i','v','e','_','E','Q','_','f','a','l','s','e','(','N','u','m','b','e','r','.','Z','e','r','o',')',' '},{0 /* 1 */ ,},{INT64_(2)},{(char*)&Main_m_20_L_21},{INT64_(42)},{'o','r','d','_','e'
,'n','u','m','_','v','s','_','n','e','g','a','t','i','v','e','_','N','E','_','t','r','u','e','(','N','u','m','b','e','r','.','Z','e','r','o',')',' '},{0 /* 1 */ ,0 /* 2 */ ,0 /* 3 */ ,0 /* 4 */ ,0 /* 5 */ ,0 /* 6 */ ,},{INT64_(2)},{(char*)&Main_m_20_L_21},{INT64_(32)},{'N','O','T',' ','a','b','s','_','v','s','_','n','e','g','a','t','i','v','e','_','L','T','_','f','a','l','s','e','(','0',')',' '},{0 /* 1 */ ,0 /* 2 */ ,0 /* 3 */ ,0 /* 4 */ ,0 /* 5 */ ,0 /* 6 */ ,0 /* 7 */ ,0 /* 8 */ ,},{INT64_(2)},{(char*)&Main_m_20_L_21},{INT64_(32)},{'N','O','T',' ','a','b','s','_','v','s','_','n','e','g','a','t','i','v','e','_','L','E','_','f','a','l','s','e','(','0',')',' '},{0 /* 1 */ ,0 /* 2 */ ,0 /* 3 */ ,0 /* 4 */ ,0 /* 5 */ ,0 /* 6 */ ,0 /* 7 */ ,0 /* 8 */ ,},{INT64_(2)},{(char*)&Main_m_20_L_21},{INT64_(27)},{'a','b','s','_','v','s','_','n','e','g','a','t','i','v','e','_','G','T','_','t','r','u','e','(','0',')',' '},{0 /* 1 */ ,0 /* 2 */ ,0 /* 3 */ ,0 /* 4 */ ,0 /* 5 */ ,},{INT64_(2)},{(char*)&Main_m_20_L_21
},{INT64_(27)},{'a','b','s','_','v','s','_','n','e','g','a','t','i','v','e','_','G','E','_','t','r','u','e','(','0',')',' '},{0 /* 1 */ ,0 /* 2 */ ,0 /* 3 */ ,0 /* 4 */ ,0 /* 5 */ ,},{INT64_(2)},{(char*)&Main_m_20_L_21},{INT64_(32)},{'N','O','T',' ','a','b','s','_','v','s','_','n','e','g','a','t','i','v','e','_','E','Q','_','f','a','l','s','e','(','0',')',' '},{0 /* 1 */ ,0 /* 2 */ ,0 /* 3 */ ,0 /* 4 */ ,0 /* 5 */ ,0 /* 6 */ ,0 /* 7 */ ,0 /* 8 */ ,},{INT64_(2)},{(char*)&Main_m_20_L_21},{INT64_(27)},{'a','b','s','_','v','s','_','n','e','g','a','t','i','v','e','_','N','E','_','t','r','u','e','(','0',')',' '},{0 /* 1 */ ,0 /* 2 */ ,0 /* 3 */ ,0 /* 4 */ ,0 /* 5 */ ,},{INT64_(2)},{(char*)&Main_m_20_L_21},{INT64_(28)},{'N','O','T',' ','a','b','s','_','v','s','_','z','e','r','o','_','L','T','_','f','a','l','s','e','(','0',')',' '},{0 /* 1 */ ,0 /* 2 */ ,0 /* 3 */ ,0 /* 4 */ ,},{INT64_(2)},{(char*)&Main_m_20_L_21},{INT64_(23)},{'a','b','s','_','v','s','_','z','e','r','o','_','G','E','_','t','r'
,'u','e','(','0',')',' '},{0 /* 1 */ ,},{INT64_(2)},{(char*)&Main_m_20_L_21},{INT64_(27)},{'n','e','g','_','a','b','s','_','v','s','_','z','e','r','o','_','L','E','_','t','r','u','e','(','0',')',' '},{0 /* 1 */ ,0 /* 2 */ ,0 /* 3 */ ,0 /* 4 */ ,0 /* 5 */ ,},{INT64_(2)},{(char*)&Main_m_20_L_21},{INT64_(32)},{'N','O','T',' ','n','e','g','_','a','b','s','_','v','s','_','z','e','r','o','_','G','T','_','f','a','l','s','e','(','0',')',' '},{0 /* 1 */ ,0 /* 2 */ ,0 /* 3 */ ,0 /* 4 */ ,0 /* 5 */ ,0 /* 6 */ ,0 /* 7 */ ,0 /* 8 */ ,},{INT64_(2)},{(char*)&Main_m_20_L_21},{INT64_(26)},{'n','e','g','_','a','b','s','_','v','s','_','o','n','e','_','L','T','_','t','r','u','e','(','0',')',' '},{0 /* 1 */ ,0 /* 2 */ ,0 /* 3 */ ,0 /* 4 */ ,0 /* 5 */ ,0 /* 6 */ ,},{INT64_(2)},{(char*)&Main_m_20_L_21},{INT64_(26)},{'n','e','g','_','a','b','s','_','v','s','_','o','n','e','_','L','E','_','t','r','u','e','(','0',')',' '},{0 /* 1 */ ,0 /* 2 */ ,0 /* 3 */ ,0 /* 4 */ ,0 /* 5 */ ,0 /* 6 */ ,},{INT64_(2)},{(char*)&Main_m_20_L_21
},{INT64_(31)},{'N','O','T',' ','n','e','g','_','a','b','s','_','v','s','_','o','n','e','_','G','T','_','f','a','l','s','e','(','0',')',' '},{0 /* 1 */ ,},{INT64_(2)},{(char*)&Main_m_20_L_21},{INT64_(31)},{'N','O','T',' ','n','e','g','_','a','b','s','_','v','s','_','o','n','e','_','G','E','_','f','a','l','s','e','(','0',')',' '},{0 /* 1 */ ,},{INT64_(2)},{(char*)&Main_m_20_L_21},{INT64_(31)},{'N','O','T',' ','n','e','g','_','a','b','s','_','v','s','_','o','n','e','_','E','Q','_','f','a','l','s','e','(','0',')',' '},{0 /* 1 */ ,},{INT64_(2)},{(char*)&Main_m_20_L_21},{INT64_(26)},{'n','e','g','_','a','b','s','_','v','s','_','o','n','e','_','N','E','_','t','r','u','e','(','0',')',' '},{0 /* 1 */ ,0 /* 2 */ ,0 /* 3 */ ,0 /* 4 */ ,0 /* 5 */ ,0 /* 6 */ ,},{'M','a','i','n','_','M','3'},{0 /* 1 */ ,},{'n','e','g','_','a','b','s','_','v','s','_','o','n','e','_','N','E','_','t','r','u','e'},{0 /* 1 */ ,},{'n','e','g','_','a','b','s','_','v','s','_','o','n','e','_','E','Q','_','f','a','l','s','e'
},{0 /* 1 */ ,},{'n','e','g','_','a','b','s','_','v','s','_','o','n','e','_','G','E','_','f','a','l','s','e'},{0 /* 1 */ ,},{'n','e','g','_','a','b','s','_','v','s','_','o','n','e','_','G','T','_','f','a','l','s','e'},{0 /* 1 */ ,},{'n','e','g','_','a','b','s','_','v','s','_','o','n','e','_','L','E','_','t','r','u','e'},{0 /* 1 */ ,},{'n','e','g','_','a','b','s','_','v','s','_','o','n','e','_','L','T','_','t','r','u','e'},{0 /* 1 */ ,},{'n','e','g','_','a','b','s','_','v','s','_','z','e','r','o','_','G','T','_','f','a','l','s','e'},{0 /* 1 */ ,},{'n','e','g','_','a','b','s','_','v','s','_','z','e','r','o','_','L','E','_','t','r','u','e'},{0 /* 1 */ ,},{'a','b','s','_','v','s','_','z','e','r','o','_','G','E','_','t','r','u','e'},{0 /* 1 */ ,},{'a','b','s','_','v','s','_','z','e','r','o','_','L','T','_','f','a','l','s','e'},{0 /* 1 */ ,},{'a','b','s','_','v','s','_','n','e','g','a','t','i','v','e','_','N','E','_','t','r','u','e'},{0 /* 1 */ ,},{'a','b','s','_','v','s','_','n','e','g','a'
,'t','i','v','e','_','E','Q','_','f','a','l','s','e'},{0 /* 1 */ ,},{'a','b','s','_','v','s','_','n','e','g','a','t','i','v','e','_','G','E','_','t','r','u','e'},{0 /* 1 */ ,},{'a','b','s','_','v','s','_','n','e','g','a','t','i','v','e','_','G','T','_','t','r','u','e'},{0 /* 1 */ ,},{'a','b','s','_','v','s','_','n','e','g','a','t','i','v','e','_','L','E','_','f','a','l','s','e'},{0 /* 1 */ ,},{'a','b','s','_','v','s','_','n','e','g','a','t','i','v','e','_','L','T','_','f','a','l','s','e'},{0 /* 1 */ ,},{'o','r','d','_','e','n','u','m','_','v','s','_','n','e','g','a','t','i','v','e','_','N','E','_','t','r','u','e'},{0 /* 1 */ ,},{'o','r','d','_','e','n','u','m','_','v','s','_','n','e','g','a','t','i','v','e','_','E','Q','_','f','a','l','s','e'},{0 /* 1 */ ,},{'o','r','d','_','e','n','u','m','_','v','s','_','n','e','g','a','t','i','v','e','_','G','E','_','t','r','u','e'},{0 /* 1 */ ,},{'o','r','d','_','e','n','u','m','_','v','s','_','n','e','g','a','t','i','v','e','_','G','T','_','t','r'
,'u','e'},{0 /* 1 */ ,},{'o','r','d','_','e','n','u','m','_','v','s','_','n','e','g','a','t','i','v','e','_','L','E','_','f','a','l','s','e'},{0 /* 1 */ ,},{'o','r','d','_','e','n','u','m','_','v','s','_','n','e','g','a','t','i','v','e','_','L','T','_','f','a','l','s','e'},{0 /* 1 */ ,},{'o','v','e','r','l','a','p','_','1','_','N','E','_','f','a','l','s','e'},{0 /* 1 */ ,},{'o','v','e','r','l','a','p','_','1','_','E','Q','_','t','r','u','e'},{0 /* 1 */ ,},{'o','v','e','r','l','a','p','_','1','_','G','E','_','t','r','u','e'},{0 /* 1 */ ,},{'o','v','e','r','l','a','p','_','1','_','G','T','_','f','a','l','s','e'},{0 /* 1 */ ,},{'o','v','e','r','l','a','p','_','1','_','L','E','_','t','r','u','e'},{0 /* 1 */ ,},{'o','v','e','r','l','a','p','_','1','_','L','T','_','f','a','l','s','e'},{0 /* 1 */ ,},{'m','i','n','i','m','u','m','_','o','v','e','r','l','a','p','_','g','r','e','a','t','e','r','_','e','n','u','m','_','G','E','_','t','r','u','e'},{0 /* 1 */ ,},{'m','i','n','i','m','u','m','_','o'
,'v','e','r','l','a','p','_','g','r','e','a','t','e','r','_','e','n','u','m','_','L','T','_','f','a','l','s','e'},{0 /* 1 */ ,},{'n','o','_','o','v','e','r','l','a','p','_','g','r','e','a','t','e','r','_','e','n','u','m','_','N','E','_','t','r','u','e'},{0 /* 1 */ ,},{'n','o','_','o','v','e','r','l','a','p','_','g','r','e','a','t','e','r','_','e','n','u','m','_','E','Q','_','f','a','l','s','e'},{0 /* 1 */ ,},{'n','o','_','o','v','e','r','l','a','p','_','g','r','e','a','t','e','r','_','e','n','u','m','_','G','E','_','t','r','u','e'},{0 /* 1 */ ,},{'n','o','_','o','v','e','r','l','a','p','_','g','r','e','a','t','e','r','_','e','n','u','m','_','G','T','_','t','r','u','e'},{0 /* 1 */ ,},{'n','o','_','o','v','e','r','l','a','p','_','g','r','e','a','t','e','r','_','e','n','u','m','_','L','E','_','f','a','l','s','e'},{0 /* 1 */ ,},{'n','o','_','o','v','e','r','l','a','p','_','g','r','e','a','t','e','r','_','e','n','u','m','_','L','T','_','f','a','l','s','e'},{0 /* 1 */ ,},{'m','i','n','i','m'
,'u','m','_','o','v','e','r','l','a','p','_','l','e','s','s','_','e','n','u','m','_','G','T','_','f','a','l','s','e'},{0 /* 1 */ ,},{'m','i','n','i','m','u','m','_','o','v','e','r','l','a','p','_','l','e','s','s','_','e','n','u','m','_','L','E','_','t','r','u','e'},{0 /* 1 */ ,},{'n','o','_','o','v','e','r','l','a','p','_','l','e','s','s','_','e','n','u','m','_','N','E','_','t','r','u','e'},{0 /* 1 */ ,},{'n','o','_','o','v','e','r','l','a','p','_','l','e','s','s','_','e','n','u','m','_','E','Q','_','f','a','l','s','e'},{0 /* 1 */ ,},{'n','o','_','o','v','e','r','l','a','p','_','l','e','s','s','_','e','n','u','m','_','G','E','_','f','a','l','s','e'},{0 /* 1 */ ,},{'n','o','_','o','v','e','r','l','a','p','_','l','e','s','s','_','e','n','u','m','_','G','T','_','f','a','l','s','e'},{0 /* 1 */ ,},{'n','o','_','o','v','e','r','l','a','p','_','l','e','s','s','_','e','n','u','m','_','L','E','_','t','r','u','e'},{0 /* 1 */ ,},{'n','o','_','o','v','e','r','l','a','p','_','l','e','s','s','_','e'
,'n','u','m','_','L','T','_','t','r','u','e'},{0 /* 1 */ ,},{'m','i','n','i','m','u','m','_','o','v','e','r','l','a','p','_','g','r','e','a','t','e','r','_','G','E','_','t','r','u','e'},{0 /* 1 */ ,},{'m','i','n','i','m','u','m','_','o','v','e','r','l','a','p','_','g','r','e','a','t','e','r','_','L','T','_','f','a','l','s','e'},{0 /* 1 */ ,},{'n','o','_','o','v','e','r','l','a','p','_','g','r','e','a','t','e','r','_','N','E','_','t','r','u','e'},{0 /* 1 */ ,},{'n','o','_','o','v','e','r','l','a','p','_','g','r','e','a','t','e','r','_','E','Q','_','f','a','l','s','e'},{0 /* 1 */ ,},{'n','o','_','o','v','e','r','l','a','p','_','g','r','e','a','t','e','r','_','G','E','_','t','r','u','e'},{0 /* 1 */ ,},{'n','o','_','o','v','e','r','l','a','p','_','g','r','e','a','t','e','r','_','G','T','_','t','r','u','e'},{0 /* 1 */ ,},{'n','o','_','o','v','e','r','l','a','p','_','g','r','e','a','t','e','r','_','L','E','_','f','a','l','s','e'},{0 /* 1 */ ,},{'n','o','_','o','v','e','r','l','a','p','_','g'
,'r','e','a','t','e','r','_','L','T','_','f','a','l','s','e'},{0 /* 1 */ ,},{'m','i','n','i','m','u','m','_','o','v','e','r','l','a','p','_','l','e','s','s','_','G','T','_','f','a','l','s','e'},{0 /* 1 */ ,},{'m','i','n','i','m','u','m','_','o','v','e','r','l','a','p','_','l','e','s','s','_','L','E','_','t','r','u','e'},{0 /* 1 */ ,},{'n','o','_','o','v','e','r','l','a','p','_','l','e','s','s','_','N','E','_','t','r','u','e'},{0 /* 1 */ ,},{'n','o','_','o','v','e','r','l','a','p','_','l','e','s','s','_','E','Q','_','f','a','l','s','e'},{0 /* 1 */ ,},{'n','o','_','o','v','e','r','l','a','p','_','l','e','s','s','_','G','E','_','f','a','l','s','e'},{0 /* 1 */ ,},{'n','o','_','o','v','e','r','l','a','p','_','l','e','s','s','_','G','T','_','f','a','l','s','e'},{0 /* 1 */ ,},{'n','o','_','o','v','e','r','l','a','p','_','l','e','s','s','_','L','E','_','t','r','u','e'},{0 /* 1 */ ,},{'n','o','_','o','v','e','r','l','a','p','_','l','e','s','s','_','L','T','_','t','r','u','e'},{0 /* 1 */ ,},{'L'
,'o','n','g','c','a','r','d','G','E','0','_','t','r','u','e'},{0 /* 1 */ ,},{'L','o','n','g','c','a','r','d','L','T','0','_','f','a','l','s','e'},{0 /* 1 */ ,},{'C','a','r','d','i','n','a','l','E','Q','N','e','g','2','_','f','a','l','s','e'},{0 /* 1 */ ,},{'C','a','r','d','i','n','a','l','N','E','N','e','g','2','_','t','r','u','e'},{0 /* 1 */ ,},{'C','a','r','d','i','n','a','l','G','E','N','e','g','2','_','t','r','u','e'},{0 /* 1 */ ,},{'C','a','r','d','i','n','a','l','G','T','N','e','g','2','_','t','r','u','e'},{0 /* 1 */ ,},{'C','a','r','d','i','n','a','l','L','E','N','e','g','2','_','f','a','l','s','e'},{0 /* 1 */ ,},{'C','a','r','d','i','n','a','l','L','T','N','e','g','2','_','f','a','l','s','e'},{0 /* 1 */ ,},{'C','a','r','d','i','n','a','l','E','Q','N','e','g','1','_','f','a','l','s','e'},{0 /* 1 */ ,},{'C','a','r','d','i','n','a','l','N','E','N','e','g','1','_','t','r','u','e'},{0 /* 1 */ ,},{'C','a','r','d','i','n','a','l','G','E','N','e','g','1','_','t','r','u','e'},{0 /* 1 */ 
,},{'C','a','r','d','i','n','a','l','G','T','N','e','g','1','_','t','r','u','e'},{0 /* 1 */ ,},{'C','a','r','d','i','n','a','l','L','E','N','e','g','1','_','f','a','l','s','e'},{0 /* 1 */ ,},{'C','a','r','d','i','n','a','l','L','T','N','e','g','1','_','f','a','l','s','e'},{0 /* 1 */ ,},{'C','a','r','d','i','n','a','l','G','E','0','_','t','r','u','e'},{0 /* 1 */ ,},{'C','a','r','d','i','n','a','l','L','T','0','_','f','a','l','s','e'},{0 /* 1 */ ,0 /* 2 */ ,0 /* 3 */ ,},{(ADDRESS)&Main_M3,4944+(char*)&Main_m_20_L_21,(ADDRESS)&Main__neg_abs_vs_one_NE_true,4952+(char*)&Main_m_20_L_21,(ADDRESS)&Main__neg_abs_vs_one_EQ_false,4975+(char*)&Main_m_20_L_21,(ADDRESS)&Main__neg_abs_vs_one_GE_false,4999+(char*)&Main_m_20_L_21,(ADDRESS)&Main__neg_abs_vs_one_GT_false,5023+(char*)&Main_m_20_L_21,(ADDRESS)&Main__neg_abs_vs_one_LE_true,5047+(char*)&Main_m_20_L_21,(ADDRESS)&Main__neg_abs_vs_one_LT_true,5070+(char*)&Main_m_20_L_21,(ADDRESS)&Main__neg_abs_vs_zero_GT_false,5093+(char*)&Main_m_20_L_21,(ADDRESS)&Main__neg_abs_vs_zero_LE_true
,5118+(char*)&Main_m_20_L_21,(ADDRESS)&Main__abs_vs_zero_GE_true,5142+(char*)&Main_m_20_L_21,(ADDRESS)&Main__abs_vs_zero_LT_false,5162+(char*)&Main_m_20_L_21,(ADDRESS)&Main__abs_vs_negative_NE_true,5183+(char*)&Main_m_20_L_21,(ADDRESS)&Main__abs_vs_negative_EQ_false,5207+(char*)&Main_m_20_L_21,(ADDRESS)&Main__abs_vs_negative_GE_true,5232+(char*)&Main_m_20_L_21,(ADDRESS)&Main__abs_vs_negative_GT_true,5256+(char*)&Main_m_20_L_21,(ADDRESS)&Main__abs_vs_negative_LE_false,5280+(char*)&Main_m_20_L_21,(ADDRESS)&Main__abs_vs_negative_LT_false,5305+(char*)&Main_m_20_L_21,(ADDRESS)&Main__ord_enum_vs_negative_NE_true,5330+(char*)&Main_m_20_L_21,(ADDRESS)&Main__ord_enum_vs_negative_EQ_false,5359+(char*)&Main_m_20_L_21,(ADDRESS)&Main__ord_enum_vs_negative_GE_true,5389+(char*)&Main_m_20_L_21,(ADDRESS)&Main__ord_enum_vs_negative_GT_true,5418+(char*)&Main_m_20_L_21,(ADDRESS)&Main__ord_enum_vs_negative_LE_false,5447+(char*)&Main_m_20_L_21,(ADDRESS)&Main__ord_enum_vs_negative_LT_false,5477+(char*)&Main_m_20_L_21
,(ADDRESS)&Main__overlap_1_NE_false,5507+(char*)&Main_m_20_L_21,(ADDRESS)&Main__overlap_1_EQ_true,5526+(char*)&Main_m_20_L_21,(ADDRESS)&Main__overlap_1_GE_true,5544+(char*)&Main_m_20_L_21,(ADDRESS)&Main__overlap_1_GT_false,5562+(char*)&Main_m_20_L_21,(ADDRESS)&Main__overlap_1_LE_true,5581+(char*)&Main_m_20_L_21,(ADDRESS)&Main__overlap_1_LT_false,5599+(char*)&Main_m_20_L_21,(ADDRESS)&Main__minimum_overlap_greater_enum_GE_true,5618+(char*)&Main_m_20_L_21,(ADDRESS)&Main__minimum_overlap_greater_enum_LT_false,5655+(char*)&Main_m_20_L_21,(ADDRESS)&Main__no_overlap_greater_enum_NE_true,5693+(char*)&Main_m_20_L_21,(ADDRESS)&Main__no_overlap_greater_enum_EQ_false,5725+(char*)&Main_m_20_L_21,(ADDRESS)&Main__no_overlap_greater_enum_GE_true,5758+(char*)&Main_m_20_L_21,(ADDRESS)&Main__no_overlap_greater_enum_GT_true,5790+(char*)&Main_m_20_L_21,(ADDRESS)&Main__no_overlap_greater_enum_LE_false,5822+(char*)&Main_m_20_L_21,(ADDRESS)&Main__no_overlap_greater_enum_LT_false,5855+(char*)&Main_m_20_L_21,(ADDRESS)&Main__minimum_overlap_less_enum_GT_false
,5888+(char*)&Main_m_20_L_21,(ADDRESS)&Main__minimum_overlap_less_enum_LE_true,5923+(char*)&Main_m_20_L_21,(ADDRESS)&Main__no_overlap_less_enum_NE_true,5957+(char*)&Main_m_20_L_21,(ADDRESS)&Main__no_overlap_less_enum_EQ_false,5986+(char*)&Main_m_20_L_21,(ADDRESS)&Main__no_overlap_less_enum_GE_false,6016+(char*)&Main_m_20_L_21,(ADDRESS)&Main__no_overlap_less_enum_GT_false,6046+(char*)&Main_m_20_L_21,(ADDRESS)&Main__no_overlap_less_enum_LE_true,6076+(char*)&Main_m_20_L_21,(ADDRESS)&Main__no_overlap_less_enum_LT_true,6105+(char*)&Main_m_20_L_21,(ADDRESS)&Main__minimum_overlap_greater_GE_true,6134+(char*)&Main_m_20_L_21,(ADDRESS)&Main__minimum_overlap_greater_LT_false,6166+(char*)&Main_m_20_L_21,(ADDRESS)&Main__no_overlap_greater_NE_true,6199+(char*)&Main_m_20_L_21,(ADDRESS)&Main__no_overlap_greater_EQ_false,6226+(char*)&Main_m_20_L_21,(ADDRESS)&Main__no_overlap_greater_GE_true,6254+(char*)&Main_m_20_L_21,(ADDRESS)&Main__no_overlap_greater_GT_true,6281+(char*)&Main_m_20_L_21,(ADDRESS)&Main__no_overlap_greater_LE_false
,6308+(char*)&Main_m_20_L_21,(ADDRESS)&Main__no_overlap_greater_LT_false,6336+(char*)&Main_m_20_L_21,(ADDRESS)&Main__minimum_overlap_less_GT_false,6364+(char*)&Main_m_20_L_21,(ADDRESS)&Main__minimum_overlap_less_LE_true,6394+(char*)&Main_m_20_L_21,(ADDRESS)&Main__no_overlap_less_NE_true,6423+(char*)&Main_m_20_L_21,(ADDRESS)&Main__no_overlap_less_EQ_false,6447+(char*)&Main_m_20_L_21,(ADDRESS)&Main__no_overlap_less_GE_false,6472+(char*)&Main_m_20_L_21,(ADDRESS)&Main__no_overlap_less_GT_false,6497+(char*)&Main_m_20_L_21,(ADDRESS)&Main__no_overlap_less_LE_true,6522+(char*)&Main_m_20_L_21,(ADDRESS)&Main__no_overlap_less_LT_true,6546+(char*)&Main_m_20_L_21,(ADDRESS)&Main__LongcardGE0_true,6570+(char*)&Main_m_20_L_21,(ADDRESS)&Main__LongcardLT0_false,6587+(char*)&Main_m_20_L_21,(ADDRESS)&Main__CardinalEQNeg2_false,6605+(char*)&Main_m_20_L_21,(ADDRESS)&Main__CardinalNENeg2_true,6626+(char*)&Main_m_20_L_21,(ADDRESS)&Main__CardinalGENeg2_true,6646+(char*)&Main_m_20_L_21,(ADDRESS)&Main__CardinalGTNeg2_true
,6666+(char*)&Main_m_20_L_21,(ADDRESS)&Main__CardinalLENeg2_false,6686+(char*)&Main_m_20_L_21,(ADDRESS)&Main__CardinalLTNeg2_false,6707+(char*)&Main_m_20_L_21,(ADDRESS)&Main__CardinalEQNeg1_false,6728+(char*)&Main_m_20_L_21,(ADDRESS)&Main__CardinalNENeg1_true,6749+(char*)&Main_m_20_L_21,(ADDRESS)&Main__CardinalGENeg1_true,6769+(char*)&Main_m_20_L_21,(ADDRESS)&Main__CardinalGTNeg1_true,6789+(char*)&Main_m_20_L_21,(ADDRESS)&Main__CardinalLENeg1_false,6809+(char*)&Main_m_20_L_21,(ADDRESS)&Main__CardinalLTNeg1_false,6830+(char*)&Main_m_20_L_21,(ADDRESS)&Main__CardinalGE0_true,6851+(char*)&Main_m_20_L_21,(ADDRESS)&Main__CardinalLT0_false,6868+(char*)&Main_m_20_L_21},{0 /* 1 */ ,0 /* 2 */ ,0 /* 3 */ ,0 /* 4 */ ,0 /* 5 */ ,0 /* 6 */ ,0 /* 7 */ ,0 /* 8 */ ,},{'.','.','/','M','a','i','n','.','m','3'},{0 /* 1 */ ,0 /* 2 */ ,0 /* 3 */ ,0 /* 4 */ ,0 /* 5 */ ,0 /* 6 */ ,}};
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
 /* end_init */
struct Main_m_M_Main_L_22_t{ADDRESS L_756[1];
char L_757[32];
ADDRESS L_758[1];
char L_759[24];
ADDRESS L_760[1];
char L_761[8];
ADDRESS L_762[1];
INT64 L_763[1];
char L_764[8];
ADDRESS L_765[2];
char L_766[8];
ADDRESS L_767[1];
char L_768[16];
};
static Main_m_M_Main_L_22_t Main_m_M_Main_L_22={{8128+(char*)&Main_m_20_L_21},{0 /* 1 */ ,0 /* 2 */ ,0 /* 3 */ ,0 /* 4 */ ,0 /* 5 */ ,0 /* 6 */ ,0 /* 7 */ ,0 /* 8 */ ,0 /* 9 */ ,0 /* 10 */ ,0 /* 11 */ ,0 /* 12 */ ,0 /* 13 */ ,0 /* 14 */ ,0 /* 15 */ ,0 /* 16 */ ,0 /* 17 */ ,0 /* 18 */ ,0 /* 19 */ ,0 /* 20 */ ,0 /* 21 */ ,0 /* 22 */ ,0 /* 23 */ ,0 /* 24 */ ,0 /* 25 */ ,0 /* 26 */ ,0 /* 27 */ ,0 /* 28 */ ,0 /* 29 */ ,0 /* 30 */ ,0 /* 31 */ ,0 /* 32 */ ,},{6888+(char*)&Main_m_20_L_21},{0 /* 1 */ ,0 /* 2 */ ,0 /* 3 */ ,0 /* 4 */ ,0 /* 5 */ ,0 /* 6 */ ,0 /* 7 */ ,0 /* 8 */ ,0 /* 9 */ ,0 /* 10 */ ,0 /* 11 */ ,0 /* 12 */ ,0 /* 13 */ ,0 /* 14 */ ,0 /* 15 */ ,0 /* 16 */ ,0 /* 17 */ ,0 /* 18 */ ,0 /* 19 */ ,0 /* 20 */ ,0 /* 21 */ ,0 /* 22 */ ,0 /* 23 */ ,0 /* 24 */ ,},{104+(char*)&Main_m_M_Main_L_22},{0 /* 1 */ ,0 /* 2 */ ,0 /* 3 */ ,0 /* 4 */ ,0 /* 5 */ ,0 /* 6 */ ,0 /* 7 */ ,0 /* 8 */ ,},{(ADDRESS)&Main_M3},{INT64_(3)},{0 /* 1 */ ,0 /* 2 */ ,0 /* 3 */ ,0 /* 4 */ ,0 /* 5 */ ,0 /* 6 */ ,0 /* 7 */ 
,0 /* 8 */ ,},{(ADDRESS)&Main_I3,128+(char*)&Main_m_M_Main_L_22},{0 /* 1 */ ,0 /* 2 */ ,0 /* 3 */ ,0 /* 4 */ ,0 /* 5 */ ,0 /* 6 */ ,0 /* 7 */ ,0 /* 8 */ ,},{(ADDRESS)&RTHooks_I3},{0 /* 1 */ ,0 /* 2 */ ,0 /* 3 */ ,0 /* 4 */ ,0 /* 5 */ ,0 /* 6 */ ,0 /* 7 */ ,0 /* 8 */ ,0 /* 9 */ ,0 /* 10 */ ,0 /* 11 */ ,0 /* 12 */ ,0 /* 13 */ ,0 /* 14 */ ,0 /* 15 */ ,0 /* 16 */ ,}};
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
#line 167 "../Main.m3"
 /* CardinalLT0_false */
#line 167 "../Main.m3"
 /* set_source_line */
#line 167 "../Main.m3"
#line 13 "../Main.m3"
 /* begin_procedure */
#line 13 "../Main.m3"
struct Main__CardinalLT0_false_Frame_t {
#line 13 "../Main.m3"
ADDRESS _unused;
#line 13 "../Main.m3"
};
#line 13 "../Main.m3"
BOOLEAN
__cdecl
Main__CardinalLT0_false(
   /* Param_Type1 */ CARDINAL a_L_25)
{
#line 13 "../Main.m3"
Main__CardinalLT0_false_Frame_t _frame;
#line 13 "../Main.m3"
_frame._unused=(ADDRESS)&_frame;
#line 13 "../Main.m3"
 /* load_integer */
#line 13 "../Main.m3"
 /* load */
#line 13 "../Main.m3"
 /* compare */
#line 13 "../Main.m3"
 /* exit_proc */
#line 13 "../Main.m3"
return ((INT64)(m3_gt(INT64,
  INT64_(0),
 ((INT64)(a_L_25)))));
#line 13 "../Main.m3"
 /* end_procedure */
#line 13 "../Main.m3"
} /* CardinalGE0_true */
#line 13 "../Main.m3"
 /* set_source_line */
#line 13 "../Main.m3"
#line 14 "../Main.m3"
 /* begin_procedure */
#line 14 "../Main.m3"
struct Main__CardinalGE0_true_Frame_t {
#line 14 "../Main.m3"
ADDRESS _unused;
#line 14 "../Main.m3"
};
#line 14 "../Main.m3"
BOOLEAN
__cdecl
Main__CardinalGE0_true(
   /* Param_Type1 */ CARDINAL a_L_27)
{
#line 14 "../Main.m3"
Main__CardinalGE0_true_Frame_t _frame;
#line 14 "../Main.m3"
_frame._unused=(ADDRESS)&_frame;
#line 14 "../Main.m3"
 /* load_integer */
#line 14 "../Main.m3"
 /* load */
#line 14 "../Main.m3"
 /* compare */
#line 14 "../Main.m3"
 /* exit_proc */
#line 14 "../Main.m3"
return ((INT64)(m3_le(INT64,
  INT64_(0),
 ((INT64)(a_L_27)))));
#line 14 "../Main.m3"
 /* end_procedure */
#line 14 "../Main.m3"
} /* CardinalLTNeg1_false */
#line 14 "../Main.m3"
 /* set_source_line */
#line 14 "../Main.m3"
#line 19 "../Main.m3"
 /* begin_procedure */
#line 19 "../Main.m3"
struct Main__CardinalLTNeg1_false_Frame_t {
#line 19 "../Main.m3"
ADDRESS _unused;
#line 19 "../Main.m3"
};
#line 19 "../Main.m3"
BOOLEAN
__cdecl
Main__CardinalLTNeg1_false(
   /* Param_Type1 */ CARDINAL a_L_29)
{
#line 19 "../Main.m3"
Main__CardinalLTNeg1_false_Frame_t _frame;
#line 19 "../Main.m3"
_frame._unused=(ADDRESS)&_frame;
#line 19 "../Main.m3"
 /* load_integer */
#line 19 "../Main.m3"
 /* load */
#line 19 "../Main.m3"
 /* compare */
#line 19 "../Main.m3"
 /* exit_proc */
#line 19 "../Main.m3"
return ((INT64)(m3_gt(INT64,
  INT64_(-1),
 ((INT64)(a_L_29)))));
#line 19 "../Main.m3"
 /* end_procedure */
#line 19 "../Main.m3"
} /* CardinalLENeg1_false */
#line 19 "../Main.m3"
 /* set_source_line */
#line 19 "../Main.m3"
#line 20 "../Main.m3"
 /* begin_procedure */
#line 20 "../Main.m3"
struct Main__CardinalLENeg1_false_Frame_t {
#line 20 "../Main.m3"
ADDRESS _unused;
#line 20 "../Main.m3"
};
#line 20 "../Main.m3"
BOOLEAN
__cdecl
Main__CardinalLENeg1_false(
   /* Param_Type1 */ CARDINAL a_L_31)
{
#line 20 "../Main.m3"
Main__CardinalLENeg1_false_Frame_t _frame;
#line 20 "../Main.m3"
_frame._unused=(ADDRESS)&_frame;
#line 20 "../Main.m3"
 /* load_integer */
#line 20 "../Main.m3"
 /* load */
#line 20 "../Main.m3"
 /* compare */
#line 20 "../Main.m3"
 /* exit_proc */
#line 20 "../Main.m3"
return ((INT64)(m3_ge(INT64,
  INT64_(-1),
 ((INT64)(a_L_31)))));
#line 20 "../Main.m3"
 /* end_procedure */
#line 20 "../Main.m3"
} /* CardinalGTNeg1_true */
#line 20 "../Main.m3"
 /* set_source_line */
#line 20 "../Main.m3"
#line 21 "../Main.m3"
 /* begin_procedure */
#line 21 "../Main.m3"
struct Main__CardinalGTNeg1_true_Frame_t {
#line 21 "../Main.m3"
ADDRESS _unused;
#line 21 "../Main.m3"
};
#line 21 "../Main.m3"
BOOLEAN
__cdecl
Main__CardinalGTNeg1_true(
   /* Param_Type1 */ CARDINAL a_L_33)
{
#line 21 "../Main.m3"
Main__CardinalGTNeg1_true_Frame_t _frame;
#line 21 "../Main.m3"
_frame._unused=(ADDRESS)&_frame;
#line 21 "../Main.m3"
 /* load_integer */
#line 21 "../Main.m3"
 /* load */
#line 21 "../Main.m3"
 /* compare */
#line 21 "../Main.m3"
 /* exit_proc */
#line 21 "../Main.m3"
return ((INT64)(m3_lt(INT64,
  INT64_(-1),
 ((INT64)(a_L_33)))));
#line 21 "../Main.m3"
 /* end_procedure */
#line 21 "../Main.m3"
} /* CardinalGENeg1_true */
#line 21 "../Main.m3"
 /* set_source_line */
#line 21 "../Main.m3"
#line 22 "../Main.m3"
 /* begin_procedure */
#line 22 "../Main.m3"
struct Main__CardinalGENeg1_true_Frame_t {
#line 22 "../Main.m3"
ADDRESS _unused;
#line 22 "../Main.m3"
};
#line 22 "../Main.m3"
BOOLEAN
__cdecl
Main__CardinalGENeg1_true(
   /* Param_Type1 */ CARDINAL a_L_35)
{
#line 22 "../Main.m3"
Main__CardinalGENeg1_true_Frame_t _frame;
#line 22 "../Main.m3"
_frame._unused=(ADDRESS)&_frame;
#line 22 "../Main.m3"
 /* load_integer */
#line 22 "../Main.m3"
 /* load */
#line 22 "../Main.m3"
 /* compare */
#line 22 "../Main.m3"
 /* exit_proc */
#line 22 "../Main.m3"
return ((INT64)(m3_le(INT64,
  INT64_(-1),
 ((INT64)(a_L_35)))));
#line 22 "../Main.m3"
 /* end_procedure */
#line 22 "../Main.m3"
} /* CardinalNENeg1_true */
#line 22 "../Main.m3"
 /* set_source_line */
#line 22 "../Main.m3"
#line 23 "../Main.m3"
 /* begin_procedure */
#line 23 "../Main.m3"
struct Main__CardinalNENeg1_true_Frame_t {
#line 23 "../Main.m3"
ADDRESS _unused;
#line 23 "../Main.m3"
};
#line 23 "../Main.m3"
BOOLEAN
__cdecl
Main__CardinalNENeg1_true(
   /* Param_Type1 */ CARDINAL a_L_37)
{
#line 23 "../Main.m3"
Main__CardinalNENeg1_true_Frame_t _frame;
#line 23 "../Main.m3"
_frame._unused=(ADDRESS)&_frame;
#line 23 "../Main.m3"
 /* load_integer */
#line 23 "../Main.m3"
 /* load */
#line 23 "../Main.m3"
 /* compare */
#line 23 "../Main.m3"
 /* exit_proc */
#line 23 "../Main.m3"
return ((INT64)(m3_ne(UINT64,
  INT64_(-1),
 ((INT64)(a_L_37)))));
#line 23 "../Main.m3"
 /* end_procedure */
#line 23 "../Main.m3"
} /* CardinalEQNeg1_false */
#line 23 "../Main.m3"
 /* set_source_line */
#line 23 "../Main.m3"
#line 24 "../Main.m3"
 /* begin_procedure */
#line 24 "../Main.m3"
struct Main__CardinalEQNeg1_false_Frame_t {
#line 24 "../Main.m3"
ADDRESS _unused;
#line 24 "../Main.m3"
};
#line 24 "../Main.m3"
BOOLEAN
__cdecl
Main__CardinalEQNeg1_false(
   /* Param_Type1 */ CARDINAL a_L_39)
{
#line 24 "../Main.m3"
Main__CardinalEQNeg1_false_Frame_t _frame;
#line 24 "../Main.m3"
_frame._unused=(ADDRESS)&_frame;
#line 24 "../Main.m3"
 /* load_integer */
#line 24 "../Main.m3"
 /* load */
#line 24 "../Main.m3"
 /* compare */
#line 24 "../Main.m3"
 /* exit_proc */
#line 24 "../Main.m3"
return ((INT64)(m3_eq(UINT64,
  INT64_(-1),
 ((INT64)(a_L_39)))));
#line 24 "../Main.m3"
 /* end_procedure */
#line 24 "../Main.m3"
} /* CardinalLTNeg2_false */
#line 24 "../Main.m3"
 /* set_source_line */
#line 24 "../Main.m3"
#line 29 "../Main.m3"
 /* begin_procedure */
#line 29 "../Main.m3"
struct Main__CardinalLTNeg2_false_Frame_t {
#line 29 "../Main.m3"
ADDRESS _unused;
#line 29 "../Main.m3"
};
#line 29 "../Main.m3"
BOOLEAN
__cdecl
Main__CardinalLTNeg2_false(
   /* Param_Type1 */ CARDINAL a_L_41)
{
#line 29 "../Main.m3"
Main__CardinalLTNeg2_false_Frame_t _frame;
#line 29 "../Main.m3"
_frame._unused=(ADDRESS)&_frame;
#line 29 "../Main.m3"
 /* load_integer */
#line 29 "../Main.m3"
 /* load */
#line 29 "../Main.m3"
 /* compare */
#line 29 "../Main.m3"
 /* exit_proc */
#line 29 "../Main.m3"
return ((INT64)(m3_gt(INT64,
  INT64_(-2),
 ((INT64)(a_L_41)))));
#line 29 "../Main.m3"
 /* end_procedure */
#line 29 "../Main.m3"
} /* CardinalLENeg2_false */
#line 29 "../Main.m3"
 /* set_source_line */
#line 29 "../Main.m3"
#line 30 "../Main.m3"
 /* begin_procedure */
#line 30 "../Main.m3"
struct Main__CardinalLENeg2_false_Frame_t {
#line 30 "../Main.m3"
ADDRESS _unused;
#line 30 "../Main.m3"
};
#line 30 "../Main.m3"
BOOLEAN
__cdecl
Main__CardinalLENeg2_false(
   /* Param_Type1 */ CARDINAL a_L_43)
{
#line 30 "../Main.m3"
Main__CardinalLENeg2_false_Frame_t _frame;
#line 30 "../Main.m3"
_frame._unused=(ADDRESS)&_frame;
#line 30 "../Main.m3"
 /* load_integer */
#line 30 "../Main.m3"
 /* load */
#line 30 "../Main.m3"
 /* compare */
#line 30 "../Main.m3"
 /* exit_proc */
#line 30 "../Main.m3"
return ((INT64)(m3_ge(INT64,
  INT64_(-2),
 ((INT64)(a_L_43)))));
#line 30 "../Main.m3"
 /* end_procedure */
#line 30 "../Main.m3"
} /* CardinalGTNeg2_true */
#line 30 "../Main.m3"
 /* set_source_line */
#line 30 "../Main.m3"
#line 31 "../Main.m3"
 /* begin_procedure */
#line 31 "../Main.m3"
struct Main__CardinalGTNeg2_true_Frame_t {
#line 31 "../Main.m3"
ADDRESS _unused;
#line 31 "../Main.m3"
};
#line 31 "../Main.m3"
BOOLEAN
__cdecl
Main__CardinalGTNeg2_true(
   /* Param_Type1 */ CARDINAL a_L_45)
{
#line 31 "../Main.m3"
Main__CardinalGTNeg2_true_Frame_t _frame;
#line 31 "../Main.m3"
_frame._unused=(ADDRESS)&_frame;
#line 31 "../Main.m3"
 /* load_integer */
#line 31 "../Main.m3"
 /* load */
#line 31 "../Main.m3"
 /* compare */
#line 31 "../Main.m3"
 /* exit_proc */
#line 31 "../Main.m3"
return ((INT64)(m3_lt(INT64,
  INT64_(-2),
 ((INT64)(a_L_45)))));
#line 31 "../Main.m3"
 /* end_procedure */
#line 31 "../Main.m3"
} /* CardinalGENeg2_true */
#line 31 "../Main.m3"
 /* set_source_line */
#line 31 "../Main.m3"
#line 32 "../Main.m3"
 /* begin_procedure */
#line 32 "../Main.m3"
struct Main__CardinalGENeg2_true_Frame_t {
#line 32 "../Main.m3"
ADDRESS _unused;
#line 32 "../Main.m3"
};
#line 32 "../Main.m3"
BOOLEAN
__cdecl
Main__CardinalGENeg2_true(
   /* Param_Type1 */ CARDINAL a_L_47)
{
#line 32 "../Main.m3"
Main__CardinalGENeg2_true_Frame_t _frame;
#line 32 "../Main.m3"
_frame._unused=(ADDRESS)&_frame;
#line 32 "../Main.m3"
 /* load_integer */
#line 32 "../Main.m3"
 /* load */
#line 32 "../Main.m3"
 /* compare */
#line 32 "../Main.m3"
 /* exit_proc */
#line 32 "../Main.m3"
return ((INT64)(m3_le(INT64,
  INT64_(-2),
 ((INT64)(a_L_47)))));
#line 32 "../Main.m3"
 /* end_procedure */
#line 32 "../Main.m3"
} /* CardinalNENeg2_true */
#line 32 "../Main.m3"
 /* set_source_line */
#line 32 "../Main.m3"
#line 33 "../Main.m3"
 /* begin_procedure */
#line 33 "../Main.m3"
struct Main__CardinalNENeg2_true_Frame_t {
#line 33 "../Main.m3"
ADDRESS _unused;
#line 33 "../Main.m3"
};
#line 33 "../Main.m3"
BOOLEAN
__cdecl
Main__CardinalNENeg2_true(
   /* Param_Type1 */ CARDINAL a_L_49)
{
#line 33 "../Main.m3"
Main__CardinalNENeg2_true_Frame_t _frame;
#line 33 "../Main.m3"
_frame._unused=(ADDRESS)&_frame;
#line 33 "../Main.m3"
 /* load_integer */
#line 33 "../Main.m3"
 /* load */
#line 33 "../Main.m3"
 /* compare */
#line 33 "../Main.m3"
 /* exit_proc */
#line 33 "../Main.m3"
return ((INT64)(m3_ne(UINT64,
  INT64_(-2),
 ((INT64)(a_L_49)))));
#line 33 "../Main.m3"
 /* end_procedure */
#line 33 "../Main.m3"
} /* CardinalEQNeg2_false */
#line 33 "../Main.m3"
 /* set_source_line */
#line 33 "../Main.m3"
#line 34 "../Main.m3"
 /* begin_procedure */
#line 34 "../Main.m3"
struct Main__CardinalEQNeg2_false_Frame_t {
#line 34 "../Main.m3"
ADDRESS _unused;
#line 34 "../Main.m3"
};
#line 34 "../Main.m3"
BOOLEAN
__cdecl
Main__CardinalEQNeg2_false(
   /* Param_Type1 */ CARDINAL a_L_51)
{
#line 34 "../Main.m3"
Main__CardinalEQNeg2_false_Frame_t _frame;
#line 34 "../Main.m3"
_frame._unused=(ADDRESS)&_frame;
#line 34 "../Main.m3"
 /* load_integer */
#line 34 "../Main.m3"
 /* load */
#line 34 "../Main.m3"
 /* compare */
#line 34 "../Main.m3"
 /* exit_proc */
#line 34 "../Main.m3"
return ((INT64)(m3_eq(UINT64,
  INT64_(-2),
 ((INT64)(a_L_51)))));
#line 34 "../Main.m3"
 /* end_procedure */
#line 34 "../Main.m3"
} /* LongcardLT0_false */
#line 34 "../Main.m3"
 /* set_source_line */
#line 34 "../Main.m3"
#line 39 "../Main.m3"
 /* begin_procedure */
#line 39 "../Main.m3"
struct Main__LongcardLT0_false_Frame_t {
#line 39 "../Main.m3"
ADDRESS _unused;
#line 39 "../Main.m3"
};
#line 39 "../Main.m3"
BOOLEAN
__cdecl
Main__LongcardLT0_false(
   /* Param_Type1 */ LONGCARD a_L_53)
{
#line 39 "../Main.m3"
Main__LongcardLT0_false_Frame_t _frame;
#line 39 "../Main.m3"
_frame._unused=(ADDRESS)&_frame;
#line 39 "../Main.m3"
 /* load_integer */
#line 39 "../Main.m3"
 /* load */
#line 39 "../Main.m3"
 /* compare */
#line 39 "../Main.m3"
 /* exit_proc */
#line 39 "../Main.m3"
return ((INT64)(m3_gt(INT64,
  INT64_(0),
 ((INT64)(a_L_53)))));
#line 39 "../Main.m3"
 /* end_procedure */
#line 39 "../Main.m3"
} /* LongcardGE0_true */
#line 39 "../Main.m3"
 /* set_source_line */
#line 39 "../Main.m3"
#line 40 "../Main.m3"
 /* begin_procedure */
#line 40 "../Main.m3"
struct Main__LongcardGE0_true_Frame_t {
#line 40 "../Main.m3"
ADDRESS _unused;
#line 40 "../Main.m3"
};
#line 40 "../Main.m3"
BOOLEAN
__cdecl
Main__LongcardGE0_true(
   /* Param_Type1 */ LONGCARD a_L_55)
{
#line 40 "../Main.m3"
Main__LongcardGE0_true_Frame_t _frame;
#line 40 "../Main.m3"
_frame._unused=(ADDRESS)&_frame;
#line 40 "../Main.m3"
 /* load_integer */
#line 40 "../Main.m3"
 /* load */
#line 40 "../Main.m3"
 /* compare */
#line 40 "../Main.m3"
 /* exit_proc */
#line 40 "../Main.m3"
return ((INT64)(m3_le(INT64,
  INT64_(0),
 ((INT64)(a_L_55)))));
#line 40 "../Main.m3"
 /* end_procedure */
#line 40 "../Main.m3"
} /* no_overlap_less_LT_true */
#line 40 "../Main.m3"
 /* set_source_line */
#line 40 "../Main.m3"
#line 45 "../Main.m3"
 /* begin_procedure */
#line 45 "../Main.m3"
struct Main__no_overlap_less_LT_true_Frame_t {
#line 45 "../Main.m3"
ADDRESS _unused;
#line 45 "../Main.m3"
};
#line 45 "../Main.m3"
BOOLEAN
__cdecl
Main__no_overlap_less_LT_true(
   /* Param_Type1 */ T8B2831D7_8 /*TypeText1*/  a_L_57,
   /* Param_Type1 */ T892833D7_8 /*TypeText1*/  b_L_58)
{
#line 45 "../Main.m3"
Main__no_overlap_less_LT_true_Frame_t _frame;
#line 45 "../Main.m3"
_frame._unused=(ADDRESS)&_frame;
#line 45 "../Main.m3"
 /* load */
#line 45 "../Main.m3"
 /* load */
#line 45 "../Main.m3"
 /* compare */
#line 45 "../Main.m3"
 /* exit_proc */
#line 45 "../Main.m3"
return ((INT64)(m3_gt(INT64,
 ((INT64)(b_L_58)),
 ((INT64)(a_L_57)))));
#line 45 "../Main.m3"
 /* end_procedure */
#line 45 "../Main.m3"
} /* no_overlap_less_LE_true */
#line 45 "../Main.m3"
 /* set_source_line */
#line 45 "../Main.m3"
#line 46 "../Main.m3"
 /* begin_procedure */
#line 46 "../Main.m3"
struct Main__no_overlap_less_LE_true_Frame_t {
#line 46 "../Main.m3"
ADDRESS _unused;
#line 46 "../Main.m3"
};
#line 46 "../Main.m3"
BOOLEAN
__cdecl
Main__no_overlap_less_LE_true(
   /* Param_Type1 */ T8B2831D7_8 /*TypeText1*/  a_L_60,
   /* Param_Type1 */ T892833D7_8 /*TypeText1*/  b_L_61)
{
#line 46 "../Main.m3"
Main__no_overlap_less_LE_true_Frame_t _frame;
#line 46 "../Main.m3"
_frame._unused=(ADDRESS)&_frame;
#line 46 "../Main.m3"
 /* load */
#line 46 "../Main.m3"
 /* load */
#line 46 "../Main.m3"
 /* compare */
#line 46 "../Main.m3"
 /* exit_proc */
#line 46 "../Main.m3"
return ((INT64)(m3_ge(INT64,
 ((INT64)(b_L_61)),
 ((INT64)(a_L_60)))));
#line 46 "../Main.m3"
 /* end_procedure */
#line 46 "../Main.m3"
} /* no_overlap_less_GT_false */
#line 46 "../Main.m3"
 /* set_source_line */
#line 46 "../Main.m3"
#line 47 "../Main.m3"
 /* begin_procedure */
#line 47 "../Main.m3"
struct Main__no_overlap_less_GT_false_Frame_t {
#line 47 "../Main.m3"
ADDRESS _unused;
#line 47 "../Main.m3"
};
#line 47 "../Main.m3"
BOOLEAN
__cdecl
Main__no_overlap_less_GT_false(
   /* Param_Type1 */ T8B2831D7_8 /*TypeText1*/  a_L_63,
   /* Param_Type1 */ T892833D7_8 /*TypeText1*/  b_L_64)
{
#line 47 "../Main.m3"
Main__no_overlap_less_GT_false_Frame_t _frame;
#line 47 "../Main.m3"
_frame._unused=(ADDRESS)&_frame;
#line 47 "../Main.m3"
 /* load */
#line 47 "../Main.m3"
 /* load */
#line 47 "../Main.m3"
 /* compare */
#line 47 "../Main.m3"
 /* exit_proc */
#line 47 "../Main.m3"
return ((INT64)(m3_lt(INT64,
 ((INT64)(b_L_64)),
 ((INT64)(a_L_63)))));
#line 47 "../Main.m3"
 /* end_procedure */
#line 47 "../Main.m3"
} /* no_overlap_less_GE_false */
#line 47 "../Main.m3"
 /* set_source_line */
#line 47 "../Main.m3"
#line 48 "../Main.m3"
 /* begin_procedure */
#line 48 "../Main.m3"
struct Main__no_overlap_less_GE_false_Frame_t {
#line 48 "../Main.m3"
ADDRESS _unused;
#line 48 "../Main.m3"
};
#line 48 "../Main.m3"
BOOLEAN
__cdecl
Main__no_overlap_less_GE_false(
   /* Param_Type1 */ T8B2831D7_8 /*TypeText1*/  a_L_66,
   /* Param_Type1 */ T892833D7_8 /*TypeText1*/  b_L_67)
{
#line 48 "../Main.m3"
Main__no_overlap_less_GE_false_Frame_t _frame;
#line 48 "../Main.m3"
_frame._unused=(ADDRESS)&_frame;
#line 48 "../Main.m3"
 /* load */
#line 48 "../Main.m3"
 /* load */
#line 48 "../Main.m3"
 /* compare */
#line 48 "../Main.m3"
 /* exit_proc */
#line 48 "../Main.m3"
return ((INT64)(m3_le(INT64,
 ((INT64)(b_L_67)),
 ((INT64)(a_L_66)))));
#line 48 "../Main.m3"
 /* end_procedure */
#line 48 "../Main.m3"
} /* no_overlap_less_EQ_false */
#line 48 "../Main.m3"
 /* set_source_line */
#line 48 "../Main.m3"
#line 49 "../Main.m3"
 /* begin_procedure */
#line 49 "../Main.m3"
struct Main__no_overlap_less_EQ_false_Frame_t {
#line 49 "../Main.m3"
ADDRESS _unused;
#line 49 "../Main.m3"
};
#line 49 "../Main.m3"
BOOLEAN
__cdecl
Main__no_overlap_less_EQ_false(
   /* Param_Type1 */ T8B2831D7_8 /*TypeText1*/  a_L_69,
   /* Param_Type1 */ T892833D7_8 /*TypeText1*/  b_L_70)
{
#line 49 "../Main.m3"
Main__no_overlap_less_EQ_false_Frame_t _frame;
#line 49 "../Main.m3"
_frame._unused=(ADDRESS)&_frame;
#line 49 "../Main.m3"
 /* load */
#line 49 "../Main.m3"
 /* load */
#line 49 "../Main.m3"
 /* compare */
#line 49 "../Main.m3"
 /* exit_proc */
#line 49 "../Main.m3"
return ((INT64)(m3_eq(UINT64,
 ((INT64)(b_L_70)),
 ((INT64)(a_L_69)))));
#line 49 "../Main.m3"
 /* end_procedure */
#line 49 "../Main.m3"
} /* no_overlap_less_NE_true */
#line 49 "../Main.m3"
 /* set_source_line */
#line 49 "../Main.m3"
#line 50 "../Main.m3"
 /* begin_procedure */
#line 50 "../Main.m3"
struct Main__no_overlap_less_NE_true_Frame_t {
#line 50 "../Main.m3"
ADDRESS _unused;
#line 50 "../Main.m3"
};
#line 50 "../Main.m3"
BOOLEAN
__cdecl
Main__no_overlap_less_NE_true(
   /* Param_Type1 */ T8B2831D7_8 /*TypeText1*/  a_L_72,
   /* Param_Type1 */ T892833D7_8 /*TypeText1*/  b_L_73)
{
#line 50 "../Main.m3"
Main__no_overlap_less_NE_true_Frame_t _frame;
#line 50 "../Main.m3"
_frame._unused=(ADDRESS)&_frame;
#line 50 "../Main.m3"
 /* load */
#line 50 "../Main.m3"
 /* load */
#line 50 "../Main.m3"
 /* compare */
#line 50 "../Main.m3"
 /* exit_proc */
#line 50 "../Main.m3"
return ((INT64)(m3_ne(UINT64,
 ((INT64)(b_L_73)),
 ((INT64)(a_L_72)))));
#line 50 "../Main.m3"
 /* end_procedure */
#line 50 "../Main.m3"
} /* minimum_overlap_less_LE_true */
#line 50 "../Main.m3"
 /* set_source_line */
#line 50 "../Main.m3"
#line 55 "../Main.m3"
 /* begin_procedure */
#line 55 "../Main.m3"
struct Main__minimum_overlap_less_LE_true_Frame_t {
#line 55 "../Main.m3"
ADDRESS _unused;
#line 55 "../Main.m3"
};
#line 55 "../Main.m3"
BOOLEAN
__cdecl
Main__minimum_overlap_less_LE_true(
   /* Param_Type1 */ T8B2831D7_8 /*TypeText1*/  a_L_75,
   /* Param_Type1 */ T882830D7_8 /*TypeText1*/  b_L_76)
{
#line 55 "../Main.m3"
Main__minimum_overlap_less_LE_true_Frame_t _frame;
#line 55 "../Main.m3"
_frame._unused=(ADDRESS)&_frame;
#line 55 "../Main.m3"
 /* load */
#line 55 "../Main.m3"
 /* load */
#line 55 "../Main.m3"
 /* compare */
#line 55 "../Main.m3"
 /* exit_proc */
#line 55 "../Main.m3"
return ((INT64)(m3_ge(INT64,
 ((INT64)(b_L_76)),
 ((INT64)(a_L_75)))));
#line 55 "../Main.m3"
 /* end_procedure */
#line 55 "../Main.m3"
} /* minimum_overlap_less_GT_false */
#line 55 "../Main.m3"
 /* set_source_line */
#line 55 "../Main.m3"
#line 56 "../Main.m3"
 /* begin_procedure */
#line 56 "../Main.m3"
struct Main__minimum_overlap_less_GT_false_Frame_t {
#line 56 "../Main.m3"
ADDRESS _unused;
#line 56 "../Main.m3"
};
#line 56 "../Main.m3"
BOOLEAN
__cdecl
Main__minimum_overlap_less_GT_false(
   /* Param_Type1 */ T8B2831D7_8 /*TypeText1*/  a_L_78,
   /* Param_Type1 */ T882830D7_8 /*TypeText1*/  b_L_79)
{
#line 56 "../Main.m3"
Main__minimum_overlap_less_GT_false_Frame_t _frame;
#line 56 "../Main.m3"
_frame._unused=(ADDRESS)&_frame;
#line 56 "../Main.m3"
 /* load */
#line 56 "../Main.m3"
 /* load */
#line 56 "../Main.m3"
 /* compare */
#line 56 "../Main.m3"
 /* exit_proc */
#line 56 "../Main.m3"
return ((INT64)(m3_lt(INT64,
 ((INT64)(b_L_79)),
 ((INT64)(a_L_78)))));
#line 56 "../Main.m3"
 /* end_procedure */
#line 56 "../Main.m3"
} /* no_overlap_greater_LT_false */
#line 56 "../Main.m3"
 /* set_source_line */
#line 56 "../Main.m3"
#line 61 "../Main.m3"
 /* begin_procedure */
#line 61 "../Main.m3"
struct Main__no_overlap_greater_LT_false_Frame_t {
#line 61 "../Main.m3"
ADDRESS _unused;
#line 61 "../Main.m3"
};
#line 61 "../Main.m3"
BOOLEAN
__cdecl
Main__no_overlap_greater_LT_false(
   /* Param_Type1 */ T892833D7_8 /*TypeText1*/  a_L_81,
   /* Param_Type1 */ T8B2831D7_8 /*TypeText1*/  b_L_82)
{
#line 61 "../Main.m3"
Main__no_overlap_greater_LT_false_Frame_t _frame;
#line 61 "../Main.m3"
_frame._unused=(ADDRESS)&_frame;
#line 61 "../Main.m3"
 /* load */
#line 61 "../Main.m3"
 /* load */
#line 61 "../Main.m3"
 /* compare */
#line 61 "../Main.m3"
 /* exit_proc */
#line 61 "../Main.m3"
return ((INT64)(m3_gt(INT64,
 ((INT64)(b_L_82)),
 ((INT64)(a_L_81)))));
#line 61 "../Main.m3"
 /* end_procedure */
#line 61 "../Main.m3"
} /* no_overlap_greater_LE_false */
#line 61 "../Main.m3"
 /* set_source_line */
#line 61 "../Main.m3"
#line 62 "../Main.m3"
 /* begin_procedure */
#line 62 "../Main.m3"
struct Main__no_overlap_greater_LE_false_Frame_t {
#line 62 "../Main.m3"
ADDRESS _unused;
#line 62 "../Main.m3"
};
#line 62 "../Main.m3"
BOOLEAN
__cdecl
Main__no_overlap_greater_LE_false(
   /* Param_Type1 */ T892833D7_8 /*TypeText1*/  a_L_84,
   /* Param_Type1 */ T8B2831D7_8 /*TypeText1*/  b_L_85)
{
#line 62 "../Main.m3"
Main__no_overlap_greater_LE_false_Frame_t _frame;
#line 62 "../Main.m3"
_frame._unused=(ADDRESS)&_frame;
#line 62 "../Main.m3"
 /* load */
#line 62 "../Main.m3"
 /* load */
#line 62 "../Main.m3"
 /* compare */
#line 62 "../Main.m3"
 /* exit_proc */
#line 62 "../Main.m3"
return ((INT64)(m3_ge(INT64,
 ((INT64)(b_L_85)),
 ((INT64)(a_L_84)))));
#line 62 "../Main.m3"
 /* end_procedure */
#line 62 "../Main.m3"
} /* no_overlap_greater_GT_true */
#line 62 "../Main.m3"
 /* set_source_line */
#line 62 "../Main.m3"
#line 63 "../Main.m3"
 /* begin_procedure */
#line 63 "../Main.m3"
struct Main__no_overlap_greater_GT_true_Frame_t {
#line 63 "../Main.m3"
ADDRESS _unused;
#line 63 "../Main.m3"
};
#line 63 "../Main.m3"
BOOLEAN
__cdecl
Main__no_overlap_greater_GT_true(
   /* Param_Type1 */ T892833D7_8 /*TypeText1*/  a_L_87,
   /* Param_Type1 */ T8B2831D7_8 /*TypeText1*/  b_L_88)
{
#line 63 "../Main.m3"
Main__no_overlap_greater_GT_true_Frame_t _frame;
#line 63 "../Main.m3"
_frame._unused=(ADDRESS)&_frame;
#line 63 "../Main.m3"
 /* load */
#line 63 "../Main.m3"
 /* load */
#line 63 "../Main.m3"
 /* compare */
#line 63 "../Main.m3"
 /* exit_proc */
#line 63 "../Main.m3"
return ((INT64)(m3_lt(INT64,
 ((INT64)(b_L_88)),
 ((INT64)(a_L_87)))));
#line 63 "../Main.m3"
 /* end_procedure */
#line 63 "../Main.m3"
} /* no_overlap_greater_GE_true */
#line 63 "../Main.m3"
 /* set_source_line */
#line 63 "../Main.m3"
#line 64 "../Main.m3"
 /* begin_procedure */
#line 64 "../Main.m3"
struct Main__no_overlap_greater_GE_true_Frame_t {
#line 64 "../Main.m3"
ADDRESS _unused;
#line 64 "../Main.m3"
};
#line 64 "../Main.m3"
BOOLEAN
__cdecl
Main__no_overlap_greater_GE_true(
   /* Param_Type1 */ T892833D7_8 /*TypeText1*/  a_L_90,
   /* Param_Type1 */ T8B2831D7_8 /*TypeText1*/  b_L_91)
{
#line 64 "../Main.m3"
Main__no_overlap_greater_GE_true_Frame_t _frame;
#line 64 "../Main.m3"
_frame._unused=(ADDRESS)&_frame;
#line 64 "../Main.m3"
 /* load */
#line 64 "../Main.m3"
 /* load */
#line 64 "../Main.m3"
 /* compare */
#line 64 "../Main.m3"
 /* exit_proc */
#line 64 "../Main.m3"
return ((INT64)(m3_le(INT64,
 ((INT64)(b_L_91)),
 ((INT64)(a_L_90)))));
#line 64 "../Main.m3"
 /* end_procedure */
#line 64 "../Main.m3"
} /* no_overlap_greater_EQ_false */
#line 64 "../Main.m3"
 /* set_source_line */
#line 64 "../Main.m3"
#line 65 "../Main.m3"
 /* begin_procedure */
#line 65 "../Main.m3"
struct Main__no_overlap_greater_EQ_false_Frame_t {
#line 65 "../Main.m3"
ADDRESS _unused;
#line 65 "../Main.m3"
};
#line 65 "../Main.m3"
BOOLEAN
__cdecl
Main__no_overlap_greater_EQ_false(
   /* Param_Type1 */ T892833D7_8 /*TypeText1*/  a_L_93,
   /* Param_Type1 */ T8B2831D7_8 /*TypeText1*/  b_L_94)
{
#line 65 "../Main.m3"
Main__no_overlap_greater_EQ_false_Frame_t _frame;
#line 65 "../Main.m3"
_frame._unused=(ADDRESS)&_frame;
#line 65 "../Main.m3"
 /* load */
#line 65 "../Main.m3"
 /* load */
#line 65 "../Main.m3"
 /* compare */
#line 65 "../Main.m3"
 /* exit_proc */
#line 65 "../Main.m3"
return ((INT64)(m3_eq(UINT64,
 ((INT64)(b_L_94)),
 ((INT64)(a_L_93)))));
#line 65 "../Main.m3"
 /* end_procedure */
#line 65 "../Main.m3"
} /* no_overlap_greater_NE_true */
#line 65 "../Main.m3"
 /* set_source_line */
#line 65 "../Main.m3"
#line 66 "../Main.m3"
 /* begin_procedure */
#line 66 "../Main.m3"
struct Main__no_overlap_greater_NE_true_Frame_t {
#line 66 "../Main.m3"
ADDRESS _unused;
#line 66 "../Main.m3"
};
#line 66 "../Main.m3"
BOOLEAN
__cdecl
Main__no_overlap_greater_NE_true(
   /* Param_Type1 */ T892833D7_8 /*TypeText1*/  a_L_96,
   /* Param_Type1 */ T8B2831D7_8 /*TypeText1*/  b_L_97)
{
#line 66 "../Main.m3"
Main__no_overlap_greater_NE_true_Frame_t _frame;
#line 66 "../Main.m3"
_frame._unused=(ADDRESS)&_frame;
#line 66 "../Main.m3"
 /* load */
#line 66 "../Main.m3"
 /* load */
#line 66 "../Main.m3"
 /* compare */
#line 66 "../Main.m3"
 /* exit_proc */
#line 66 "../Main.m3"
return ((INT64)(m3_ne(UINT64,
 ((INT64)(b_L_97)),
 ((INT64)(a_L_96)))));
#line 66 "../Main.m3"
 /* end_procedure */
#line 66 "../Main.m3"
} /* minimum_overlap_greater_LT_false */
#line 66 "../Main.m3"
 /* set_source_line */
#line 66 "../Main.m3"
#line 71 "../Main.m3"
 /* begin_procedure */
#line 71 "../Main.m3"
struct Main__minimum_overlap_greater_LT_false_Frame_t {
#line 71 "../Main.m3"
ADDRESS _unused;
#line 71 "../Main.m3"
};
#line 71 "../Main.m3"
BOOLEAN
__cdecl
Main__minimum_overlap_greater_LT_false(
   /* Param_Type1 */ T882830D7_8 /*TypeText1*/  a_L_99,
   /* Param_Type1 */ T8B2831D7_8 /*TypeText1*/  b_L_100)
{
#line 71 "../Main.m3"
Main__minimum_overlap_greater_LT_false_Frame_t _frame;
#line 71 "../Main.m3"
_frame._unused=(ADDRESS)&_frame;
#line 71 "../Main.m3"
 /* load */
#line 71 "../Main.m3"
 /* load */
#line 71 "../Main.m3"
 /* compare */
#line 71 "../Main.m3"
 /* exit_proc */
#line 71 "../Main.m3"
return ((INT64)(m3_gt(INT64,
 ((INT64)(b_L_100)),
 ((INT64)(a_L_99)))));
#line 71 "../Main.m3"
 /* end_procedure */
#line 71 "../Main.m3"
} /* minimum_overlap_greater_GE_true */
#line 71 "../Main.m3"
 /* set_source_line */
#line 71 "../Main.m3"
#line 72 "../Main.m3"
 /* begin_procedure */
#line 72 "../Main.m3"
struct Main__minimum_overlap_greater_GE_true_Frame_t {
#line 72 "../Main.m3"
ADDRESS _unused;
#line 72 "../Main.m3"
};
#line 72 "../Main.m3"
BOOLEAN
__cdecl
Main__minimum_overlap_greater_GE_true(
   /* Param_Type1 */ T882830D7_8 /*TypeText1*/  a_L_102,
   /* Param_Type1 */ T8B2831D7_8 /*TypeText1*/  b_L_103)
{
#line 72 "../Main.m3"
Main__minimum_overlap_greater_GE_true_Frame_t _frame;
#line 72 "../Main.m3"
_frame._unused=(ADDRESS)&_frame;
#line 72 "../Main.m3"
 /* load */
#line 72 "../Main.m3"
 /* load */
#line 72 "../Main.m3"
 /* compare */
#line 72 "../Main.m3"
 /* exit_proc */
#line 72 "../Main.m3"
return ((INT64)(m3_le(INT64,
 ((INT64)(b_L_103)),
 ((INT64)(a_L_102)))));
#line 72 "../Main.m3"
 /* end_procedure */
#line 72 "../Main.m3"
} /* no_overlap_less_enum_LT_true */
#line 72 "../Main.m3"
 /* set_source_line */
#line 72 "../Main.m3"
#line 86 "../Main.m3"
 /* begin_procedure */
#line 86 "../Main.m3"
struct Main__no_overlap_less_enum_LT_true_Frame_t {
#line 86 "../Main.m3"
ADDRESS _unused;
#line 86 "../Main.m3"
};
#line 86 "../Main.m3"
BOOLEAN
__cdecl
Main__no_overlap_less_enum_LT_true(
   /* Param_Type1 */ Main__LowNumber a_L_105,
   /* Param_Type1 */ Main__HighNumber b_L_106)
{
#line 86 "../Main.m3"
Main__no_overlap_less_enum_LT_true_Frame_t _frame;
#line 86 "../Main.m3"
_frame._unused=(ADDRESS)&_frame;
#line 86 "../Main.m3"
 /* load */
#line 86 "../Main.m3"
 /* load */
#line 86 "../Main.m3"
 /* compare */
#line 86 "../Main.m3"
 /* exit_proc */
#line 86 "../Main.m3"
return ((INT64)(m3_gt(INT64,
 ((INT64)(b_L_106)),
 ((INT64)(a_L_105)))));
#line 86 "../Main.m3"
 /* end_procedure */
#line 86 "../Main.m3"
} /* no_overlap_less_enum_LE_true */
#line 86 "../Main.m3"
 /* set_source_line */
#line 86 "../Main.m3"
#line 87 "../Main.m3"
 /* begin_procedure */
#line 87 "../Main.m3"
struct Main__no_overlap_less_enum_LE_true_Frame_t {
#line 87 "../Main.m3"
ADDRESS _unused;
#line 87 "../Main.m3"
};
#line 87 "../Main.m3"
BOOLEAN
__cdecl
Main__no_overlap_less_enum_LE_true(
   /* Param_Type1 */ Main__LowNumber a_L_108,
   /* Param_Type1 */ Main__HighNumber b_L_109)
{
#line 87 "../Main.m3"
Main__no_overlap_less_enum_LE_true_Frame_t _frame;
#line 87 "../Main.m3"
_frame._unused=(ADDRESS)&_frame;
#line 87 "../Main.m3"
 /* load */
#line 87 "../Main.m3"
 /* load */
#line 87 "../Main.m3"
 /* compare */
#line 87 "../Main.m3"
 /* exit_proc */
#line 87 "../Main.m3"
return ((INT64)(m3_ge(INT64,
 ((INT64)(b_L_109)),
 ((INT64)(a_L_108)))));
#line 87 "../Main.m3"
 /* end_procedure */
#line 87 "../Main.m3"
} /* no_overlap_less_enum_GT_false */
#line 87 "../Main.m3"
 /* set_source_line */
#line 87 "../Main.m3"
#line 88 "../Main.m3"
 /* begin_procedure */
#line 88 "../Main.m3"
struct Main__no_overlap_less_enum_GT_false_Frame_t {
#line 88 "../Main.m3"
ADDRESS _unused;
#line 88 "../Main.m3"
};
#line 88 "../Main.m3"
BOOLEAN
__cdecl
Main__no_overlap_less_enum_GT_false(
   /* Param_Type1 */ Main__LowNumber a_L_111,
   /* Param_Type1 */ Main__HighNumber b_L_112)
{
#line 88 "../Main.m3"
Main__no_overlap_less_enum_GT_false_Frame_t _frame;
#line 88 "../Main.m3"
_frame._unused=(ADDRESS)&_frame;
#line 88 "../Main.m3"
 /* load */
#line 88 "../Main.m3"
 /* load */
#line 88 "../Main.m3"
 /* compare */
#line 88 "../Main.m3"
 /* exit_proc */
#line 88 "../Main.m3"
return ((INT64)(m3_lt(INT64,
 ((INT64)(b_L_112)),
 ((INT64)(a_L_111)))));
#line 88 "../Main.m3"
 /* end_procedure */
#line 88 "../Main.m3"
} /* no_overlap_less_enum_GE_false */
#line 88 "../Main.m3"
 /* set_source_line */
#line 88 "../Main.m3"
#line 89 "../Main.m3"
 /* begin_procedure */
#line 89 "../Main.m3"
struct Main__no_overlap_less_enum_GE_false_Frame_t {
#line 89 "../Main.m3"
ADDRESS _unused;
#line 89 "../Main.m3"
};
#line 89 "../Main.m3"
BOOLEAN
__cdecl
Main__no_overlap_less_enum_GE_false(
   /* Param_Type1 */ Main__LowNumber a_L_114,
   /* Param_Type1 */ Main__HighNumber b_L_115)
{
#line 89 "../Main.m3"
Main__no_overlap_less_enum_GE_false_Frame_t _frame;
#line 89 "../Main.m3"
_frame._unused=(ADDRESS)&_frame;
#line 89 "../Main.m3"
 /* load */
#line 89 "../Main.m3"
 /* load */
#line 89 "../Main.m3"
 /* compare */
#line 89 "../Main.m3"
 /* exit_proc */
#line 89 "../Main.m3"
return ((INT64)(m3_le(INT64,
 ((INT64)(b_L_115)),
 ((INT64)(a_L_114)))));
#line 89 "../Main.m3"
 /* end_procedure */
#line 89 "../Main.m3"
} /* no_overlap_less_enum_EQ_false */
#line 89 "../Main.m3"
 /* set_source_line */
#line 89 "../Main.m3"
#line 90 "../Main.m3"
 /* begin_procedure */
#line 90 "../Main.m3"
struct Main__no_overlap_less_enum_EQ_false_Frame_t {
#line 90 "../Main.m3"
ADDRESS _unused;
#line 90 "../Main.m3"
};
#line 90 "../Main.m3"
BOOLEAN
__cdecl
Main__no_overlap_less_enum_EQ_false(
   /* Param_Type1 */ Main__LowNumber a_L_117,
   /* Param_Type1 */ Main__HighNumber b_L_118)
{
#line 90 "../Main.m3"
Main__no_overlap_less_enum_EQ_false_Frame_t _frame;
#line 90 "../Main.m3"
_frame._unused=(ADDRESS)&_frame;
#line 90 "../Main.m3"
 /* load */
#line 90 "../Main.m3"
 /* load */
#line 90 "../Main.m3"
 /* compare */
#line 90 "../Main.m3"
 /* exit_proc */
#line 90 "../Main.m3"
return ((INT64)(m3_eq(UINT64,
 ((INT64)(b_L_118)),
 ((INT64)(a_L_117)))));
#line 90 "../Main.m3"
 /* end_procedure */
#line 90 "../Main.m3"
} /* no_overlap_less_enum_NE_true */
#line 90 "../Main.m3"
 /* set_source_line */
#line 90 "../Main.m3"
#line 91 "../Main.m3"
 /* begin_procedure */
#line 91 "../Main.m3"
struct Main__no_overlap_less_enum_NE_true_Frame_t {
#line 91 "../Main.m3"
ADDRESS _unused;
#line 91 "../Main.m3"
};
#line 91 "../Main.m3"
BOOLEAN
__cdecl
Main__no_overlap_less_enum_NE_true(
   /* Param_Type1 */ Main__LowNumber a_L_120,
   /* Param_Type1 */ Main__HighNumber b_L_121)
{
#line 91 "../Main.m3"
Main__no_overlap_less_enum_NE_true_Frame_t _frame;
#line 91 "../Main.m3"
_frame._unused=(ADDRESS)&_frame;
#line 91 "../Main.m3"
 /* load */
#line 91 "../Main.m3"
 /* load */
#line 91 "../Main.m3"
 /* compare */
#line 91 "../Main.m3"
 /* exit_proc */
#line 91 "../Main.m3"
return ((INT64)(m3_ne(UINT64,
 ((INT64)(b_L_121)),
 ((INT64)(a_L_120)))));
#line 91 "../Main.m3"
 /* end_procedure */
#line 91 "../Main.m3"
} /* minimum_overlap_less_enum_LE_true */
#line 91 "../Main.m3"
 /* set_source_line */
#line 91 "../Main.m3"
#line 96 "../Main.m3"
 /* begin_procedure */
#line 96 "../Main.m3"
struct Main__minimum_overlap_less_enum_LE_true_Frame_t {
#line 96 "../Main.m3"
ADDRESS _unused;
#line 96 "../Main.m3"
};
#line 96 "../Main.m3"
BOOLEAN
__cdecl
Main__minimum_overlap_less_enum_LE_true(
   /* Param_Type1 */ Main__LowNumber a_L_123,
   /* Param_Type1 */ Main__MiddleNumber b_L_124)
{
#line 96 "../Main.m3"
Main__minimum_overlap_less_enum_LE_true_Frame_t _frame;
#line 96 "../Main.m3"
_frame._unused=(ADDRESS)&_frame;
#line 96 "../Main.m3"
 /* load */
#line 96 "../Main.m3"
 /* load */
#line 96 "../Main.m3"
 /* compare */
#line 96 "../Main.m3"
 /* exit_proc */
#line 96 "../Main.m3"
return ((INT64)(m3_ge(INT64,
 ((INT64)(b_L_124)),
 ((INT64)(a_L_123)))));
#line 96 "../Main.m3"
 /* end_procedure */
#line 96 "../Main.m3"
} /* minimum_overlap_less_enum_GT_false */
#line 96 "../Main.m3"
 /* set_source_line */
#line 96 "../Main.m3"
#line 97 "../Main.m3"
 /* begin_procedure */
#line 97 "../Main.m3"
struct Main__minimum_overlap_less_enum_GT_false_Frame_t {
#line 97 "../Main.m3"
ADDRESS _unused;
#line 97 "../Main.m3"
};
#line 97 "../Main.m3"
BOOLEAN
__cdecl
Main__minimum_overlap_less_enum_GT_false(
   /* Param_Type1 */ Main__LowNumber a_L_126,
   /* Param_Type1 */ Main__MiddleNumber b_L_127)
{
#line 97 "../Main.m3"
Main__minimum_overlap_less_enum_GT_false_Frame_t _frame;
#line 97 "../Main.m3"
_frame._unused=(ADDRESS)&_frame;
#line 97 "../Main.m3"
 /* load */
#line 97 "../Main.m3"
 /* load */
#line 97 "../Main.m3"
 /* compare */
#line 97 "../Main.m3"
 /* exit_proc */
#line 97 "../Main.m3"
return ((INT64)(m3_lt(INT64,
 ((INT64)(b_L_127)),
 ((INT64)(a_L_126)))));
#line 97 "../Main.m3"
 /* end_procedure */
#line 97 "../Main.m3"
} /* no_overlap_greater_enum_LT_false */
#line 97 "../Main.m3"
 /* set_source_line */
#line 97 "../Main.m3"
#line 102 "../Main.m3"
 /* begin_procedure */
#line 102 "../Main.m3"
struct Main__no_overlap_greater_enum_LT_false_Frame_t {
#line 102 "../Main.m3"
ADDRESS _unused;
#line 102 "../Main.m3"
};
#line 102 "../Main.m3"
BOOLEAN
__cdecl
Main__no_overlap_greater_enum_LT_false(
   /* Param_Type1 */ Main__HighNumber a_L_129,
   /* Param_Type1 */ Main__LowNumber b_L_130)
{
#line 102 "../Main.m3"
Main__no_overlap_greater_enum_LT_false_Frame_t _frame;
#line 102 "../Main.m3"
_frame._unused=(ADDRESS)&_frame;
#line 102 "../Main.m3"
 /* load */
#line 102 "../Main.m3"
 /* load */
#line 102 "../Main.m3"
 /* compare */
#line 102 "../Main.m3"
 /* exit_proc */
#line 102 "../Main.m3"
return ((INT64)(m3_gt(INT64,
 ((INT64)(b_L_130)),
 ((INT64)(a_L_129)))));
#line 102 "../Main.m3"
 /* end_procedure */
#line 102 "../Main.m3"
} /* no_overlap_greater_enum_LE_false */
#line 102 "../Main.m3"
 /* set_source_line */
#line 102 "../Main.m3"
#line 103 "../Main.m3"
 /* begin_procedure */
#line 103 "../Main.m3"
struct Main__no_overlap_greater_enum_LE_false_Frame_t {
#line 103 "../Main.m3"
ADDRESS _unused;
#line 103 "../Main.m3"
};
#line 103 "../Main.m3"
BOOLEAN
__cdecl
Main__no_overlap_greater_enum_LE_false(
   /* Param_Type1 */ Main__HighNumber a_L_132,
   /* Param_Type1 */ Main__LowNumber b_L_133)
{
#line 103 "../Main.m3"
Main__no_overlap_greater_enum_LE_false_Frame_t _frame;
#line 103 "../Main.m3"
_frame._unused=(ADDRESS)&_frame;
#line 103 "../Main.m3"
 /* load */
#line 103 "../Main.m3"
 /* load */
#line 103 "../Main.m3"
 /* compare */
#line 103 "../Main.m3"
 /* exit_proc */
#line 103 "../Main.m3"
return ((INT64)(m3_ge(INT64,
 ((INT64)(b_L_133)),
 ((INT64)(a_L_132)))));
#line 103 "../Main.m3"
 /* end_procedure */
#line 103 "../Main.m3"
} /* no_overlap_greater_enum_GT_true */
#line 103 "../Main.m3"
 /* set_source_line */
#line 103 "../Main.m3"
#line 104 "../Main.m3"
 /* begin_procedure */
#line 104 "../Main.m3"
struct Main__no_overlap_greater_enum_GT_true_Frame_t {
#line 104 "../Main.m3"
ADDRESS _unused;
#line 104 "../Main.m3"
};
#line 104 "../Main.m3"
BOOLEAN
__cdecl
Main__no_overlap_greater_enum_GT_true(
   /* Param_Type1 */ Main__HighNumber a_L_135,
   /* Param_Type1 */ Main__LowNumber b_L_136)
{
#line 104 "../Main.m3"
Main__no_overlap_greater_enum_GT_true_Frame_t _frame;
#line 104 "../Main.m3"
_frame._unused=(ADDRESS)&_frame;
#line 104 "../Main.m3"
 /* load */
#line 104 "../Main.m3"
 /* load */
#line 104 "../Main.m3"
 /* compare */
#line 104 "../Main.m3"
 /* exit_proc */
#line 104 "../Main.m3"
return ((INT64)(m3_lt(INT64,
 ((INT64)(b_L_136)),
 ((INT64)(a_L_135)))));
#line 104 "../Main.m3"
 /* end_procedure */
#line 104 "../Main.m3"
} /* no_overlap_greater_enum_GE_true */
#line 104 "../Main.m3"
 /* set_source_line */
#line 104 "../Main.m3"
#line 105 "../Main.m3"
 /* begin_procedure */
#line 105 "../Main.m3"
struct Main__no_overlap_greater_enum_GE_true_Frame_t {
#line 105 "../Main.m3"
ADDRESS _unused;
#line 105 "../Main.m3"
};
#line 105 "../Main.m3"
BOOLEAN
__cdecl
Main__no_overlap_greater_enum_GE_true(
   /* Param_Type1 */ Main__HighNumber a_L_138,
   /* Param_Type1 */ Main__LowNumber b_L_139)
{
#line 105 "../Main.m3"
Main__no_overlap_greater_enum_GE_true_Frame_t _frame;
#line 105 "../Main.m3"
_frame._unused=(ADDRESS)&_frame;
#line 105 "../Main.m3"
 /* load */
#line 105 "../Main.m3"
 /* load */
#line 105 "../Main.m3"
 /* compare */
#line 105 "../Main.m3"
 /* exit_proc */
#line 105 "../Main.m3"
return ((INT64)(m3_le(INT64,
 ((INT64)(b_L_139)),
 ((INT64)(a_L_138)))));
#line 105 "../Main.m3"
 /* end_procedure */
#line 105 "../Main.m3"
} /* no_overlap_greater_enum_EQ_false */
#line 105 "../Main.m3"
 /* set_source_line */
#line 105 "../Main.m3"
#line 106 "../Main.m3"
 /* begin_procedure */
#line 106 "../Main.m3"
struct Main__no_overlap_greater_enum_EQ_false_Frame_t {
#line 106 "../Main.m3"
ADDRESS _unused;
#line 106 "../Main.m3"
};
#line 106 "../Main.m3"
BOOLEAN
__cdecl
Main__no_overlap_greater_enum_EQ_false(
   /* Param_Type1 */ Main__HighNumber a_L_141,
   /* Param_Type1 */ Main__LowNumber b_L_142)
{
#line 106 "../Main.m3"
Main__no_overlap_greater_enum_EQ_false_Frame_t _frame;
#line 106 "../Main.m3"
_frame._unused=(ADDRESS)&_frame;
#line 106 "../Main.m3"
 /* load */
#line 106 "../Main.m3"
 /* load */
#line 106 "../Main.m3"
 /* compare */
#line 106 "../Main.m3"
 /* exit_proc */
#line 106 "../Main.m3"
return ((INT64)(m3_eq(UINT64,
 ((INT64)(b_L_142)),
 ((INT64)(a_L_141)))));
#line 106 "../Main.m3"
 /* end_procedure */
#line 106 "../Main.m3"
} /* no_overlap_greater_enum_NE_true */
#line 106 "../Main.m3"
 /* set_source_line */
#line 106 "../Main.m3"
#line 107 "../Main.m3"
 /* begin_procedure */
#line 107 "../Main.m3"
struct Main__no_overlap_greater_enum_NE_true_Frame_t {
#line 107 "../Main.m3"
ADDRESS _unused;
#line 107 "../Main.m3"
};
#line 107 "../Main.m3"
BOOLEAN
__cdecl
Main__no_overlap_greater_enum_NE_true(
   /* Param_Type1 */ Main__HighNumber a_L_144,
   /* Param_Type1 */ Main__LowNumber b_L_145)
{
#line 107 "../Main.m3"
Main__no_overlap_greater_enum_NE_true_Frame_t _frame;
#line 107 "../Main.m3"
_frame._unused=(ADDRESS)&_frame;
#line 107 "../Main.m3"
 /* load */
#line 107 "../Main.m3"
 /* load */
#line 107 "../Main.m3"
 /* compare */
#line 107 "../Main.m3"
 /* exit_proc */
#line 107 "../Main.m3"
return ((INT64)(m3_ne(UINT64,
 ((INT64)(b_L_145)),
 ((INT64)(a_L_144)))));
#line 107 "../Main.m3"
 /* end_procedure */
#line 107 "../Main.m3"
} /* minimum_overlap_greater_enum_LT_false */
#line 107 "../Main.m3"
 /* set_source_line */
#line 107 "../Main.m3"
#line 112 "../Main.m3"
 /* begin_procedure */
#line 112 "../Main.m3"
struct Main__minimum_overlap_greater_enum_LT_false_Frame_t {
#line 112 "../Main.m3"
ADDRESS _unused;
#line 112 "../Main.m3"
};
#line 112 "../Main.m3"
BOOLEAN
__cdecl
Main__minimum_overlap_greater_enum_LT_false(
   /* Param_Type1 */ Main__MiddleNumber a_L_147,
   /* Param_Type1 */ Main__LowNumber b_L_148)
{
#line 112 "../Main.m3"
Main__minimum_overlap_greater_enum_LT_false_Frame_t _frame;
#line 112 "../Main.m3"
_frame._unused=(ADDRESS)&_frame;
#line 112 "../Main.m3"
 /* load */
#line 112 "../Main.m3"
 /* load */
#line 112 "../Main.m3"
 /* compare */
#line 112 "../Main.m3"
 /* exit_proc */
#line 112 "../Main.m3"
return ((INT64)(m3_gt(INT64,
 ((INT64)(b_L_148)),
 ((INT64)(a_L_147)))));
#line 112 "../Main.m3"
 /* end_procedure */
#line 112 "../Main.m3"
} /* minimum_overlap_greater_enum_GE_true */
#line 112 "../Main.m3"
 /* set_source_line */
#line 112 "../Main.m3"
#line 113 "../Main.m3"
 /* begin_procedure */
#line 113 "../Main.m3"
struct Main__minimum_overlap_greater_enum_GE_true_Frame_t {
#line 113 "../Main.m3"
ADDRESS _unused;
#line 113 "../Main.m3"
};
#line 113 "../Main.m3"
BOOLEAN
__cdecl
Main__minimum_overlap_greater_enum_GE_true(
   /* Param_Type1 */ Main__MiddleNumber a_L_150,
   /* Param_Type1 */ Main__LowNumber b_L_151)
{
#line 113 "../Main.m3"
Main__minimum_overlap_greater_enum_GE_true_Frame_t _frame;
#line 113 "../Main.m3"
_frame._unused=(ADDRESS)&_frame;
#line 113 "../Main.m3"
 /* load */
#line 113 "../Main.m3"
 /* load */
#line 113 "../Main.m3"
 /* compare */
#line 113 "../Main.m3"
 /* exit_proc */
#line 113 "../Main.m3"
return ((INT64)(m3_le(INT64,
 ((INT64)(b_L_151)),
 ((INT64)(a_L_150)))));
#line 113 "../Main.m3"
 /* end_procedure */
#line 113 "../Main.m3"
} /* overlap_1_LT_false */
#line 113 "../Main.m3"
 /* set_source_line */
#line 113 "../Main.m3"
#line 118 "../Main.m3"
 /* begin_procedure */
#line 118 "../Main.m3"
struct Main__overlap_1_LT_false_Frame_t {
#line 118 "../Main.m3"
ADDRESS _unused;
#line 118 "../Main.m3"
};
#line 118 "../Main.m3"
BOOLEAN
__cdecl
Main__overlap_1_LT_false(
   /* Param_Type1 */ T8A2831D7_8 /*TypeText1*/  a_L_153,
   /* Param_Type1 */ T8A2831D7_8 /*TypeText1*/  b_L_154)
{
#line 118 "../Main.m3"
Main__overlap_1_LT_false_Frame_t _frame;
#line 118 "../Main.m3"
_frame._unused=(ADDRESS)&_frame;
#line 118 "../Main.m3"
 /* load */
#line 118 "../Main.m3"
 /* load */
#line 118 "../Main.m3"
 /* compare */
#line 118 "../Main.m3"
 /* exit_proc */
#line 118 "../Main.m3"
return ((INT64)(m3_gt(INT64,
 ((INT64)(b_L_154)),
 ((INT64)(a_L_153)))));
#line 118 "../Main.m3"
 /* end_procedure */
#line 118 "../Main.m3"
} /* overlap_1_LE_true */
#line 118 "../Main.m3"
 /* set_source_line */
#line 118 "../Main.m3"
#line 119 "../Main.m3"
 /* begin_procedure */
#line 119 "../Main.m3"
struct Main__overlap_1_LE_true_Frame_t {
#line 119 "../Main.m3"
ADDRESS _unused;
#line 119 "../Main.m3"
};
#line 119 "../Main.m3"
BOOLEAN
__cdecl
Main__overlap_1_LE_true(
   /* Param_Type1 */ T8A2831D7_8 /*TypeText1*/  a_L_156,
   /* Param_Type1 */ T8A2831D7_8 /*TypeText1*/  b_L_157)
{
#line 119 "../Main.m3"
Main__overlap_1_LE_true_Frame_t _frame;
#line 119 "../Main.m3"
_frame._unused=(ADDRESS)&_frame;
#line 119 "../Main.m3"
 /* load */
#line 119 "../Main.m3"
 /* load */
#line 119 "../Main.m3"
 /* compare */
#line 119 "../Main.m3"
 /* exit_proc */
#line 119 "../Main.m3"
return ((INT64)(m3_ge(INT64,
 ((INT64)(b_L_157)),
 ((INT64)(a_L_156)))));
#line 119 "../Main.m3"
 /* end_procedure */
#line 119 "../Main.m3"
} /* overlap_1_GT_false */
#line 119 "../Main.m3"
 /* set_source_line */
#line 119 "../Main.m3"
#line 120 "../Main.m3"
 /* begin_procedure */
#line 120 "../Main.m3"
struct Main__overlap_1_GT_false_Frame_t {
#line 120 "../Main.m3"
ADDRESS _unused;
#line 120 "../Main.m3"
};
#line 120 "../Main.m3"
BOOLEAN
__cdecl
Main__overlap_1_GT_false(
   /* Param_Type1 */ T8A2831D7_8 /*TypeText1*/  a_L_159,
   /* Param_Type1 */ T8A2831D7_8 /*TypeText1*/  b_L_160)
{
#line 120 "../Main.m3"
Main__overlap_1_GT_false_Frame_t _frame;
#line 120 "../Main.m3"
_frame._unused=(ADDRESS)&_frame;
#line 120 "../Main.m3"
 /* load */
#line 120 "../Main.m3"
 /* load */
#line 120 "../Main.m3"
 /* compare */
#line 120 "../Main.m3"
 /* exit_proc */
#line 120 "../Main.m3"
return ((INT64)(m3_lt(INT64,
 ((INT64)(b_L_160)),
 ((INT64)(a_L_159)))));
#line 120 "../Main.m3"
 /* end_procedure */
#line 120 "../Main.m3"
} /* overlap_1_GE_true */
#line 120 "../Main.m3"
 /* set_source_line */
#line 120 "../Main.m3"
#line 121 "../Main.m3"
 /* begin_procedure */
#line 121 "../Main.m3"
struct Main__overlap_1_GE_true_Frame_t {
#line 121 "../Main.m3"
ADDRESS _unused;
#line 121 "../Main.m3"
};
#line 121 "../Main.m3"
BOOLEAN
__cdecl
Main__overlap_1_GE_true(
   /* Param_Type1 */ T8A2831D7_8 /*TypeText1*/  a_L_162,
   /* Param_Type1 */ T8A2831D7_8 /*TypeText1*/  b_L_163)
{
#line 121 "../Main.m3"
Main__overlap_1_GE_true_Frame_t _frame;
#line 121 "../Main.m3"
_frame._unused=(ADDRESS)&_frame;
#line 121 "../Main.m3"
 /* load */
#line 121 "../Main.m3"
 /* load */
#line 121 "../Main.m3"
 /* compare */
#line 121 "../Main.m3"
 /* exit_proc */
#line 121 "../Main.m3"
return ((INT64)(m3_le(INT64,
 ((INT64)(b_L_163)),
 ((INT64)(a_L_162)))));
#line 121 "../Main.m3"
 /* end_procedure */
#line 121 "../Main.m3"
} /* overlap_1_EQ_true */
#line 121 "../Main.m3"
 /* set_source_line */
#line 121 "../Main.m3"
#line 122 "../Main.m3"
 /* begin_procedure */
#line 122 "../Main.m3"
struct Main__overlap_1_EQ_true_Frame_t {
#line 122 "../Main.m3"
ADDRESS _unused;
#line 122 "../Main.m3"
};
#line 122 "../Main.m3"
BOOLEAN
__cdecl
Main__overlap_1_EQ_true(
   /* Param_Type1 */ T8A2831D7_8 /*TypeText1*/  a_L_165,
   /* Param_Type1 */ T8A2831D7_8 /*TypeText1*/  b_L_166)
{
#line 122 "../Main.m3"
Main__overlap_1_EQ_true_Frame_t _frame;
#line 122 "../Main.m3"
_frame._unused=(ADDRESS)&_frame;
#line 122 "../Main.m3"
 /* load */
#line 122 "../Main.m3"
 /* load */
#line 122 "../Main.m3"
 /* compare */
#line 122 "../Main.m3"
 /* exit_proc */
#line 122 "../Main.m3"
return ((INT64)(m3_eq(UINT64,
 ((INT64)(b_L_166)),
 ((INT64)(a_L_165)))));
#line 122 "../Main.m3"
 /* end_procedure */
#line 122 "../Main.m3"
} /* overlap_1_NE_false */
#line 122 "../Main.m3"
 /* set_source_line */
#line 122 "../Main.m3"
#line 123 "../Main.m3"
 /* begin_procedure */
#line 123 "../Main.m3"
struct Main__overlap_1_NE_false_Frame_t {
#line 123 "../Main.m3"
ADDRESS _unused;
#line 123 "../Main.m3"
};
#line 123 "../Main.m3"
BOOLEAN
__cdecl
Main__overlap_1_NE_false(
   /* Param_Type1 */ T8A2831D7_8 /*TypeText1*/  a_L_168,
   /* Param_Type1 */ T8A2831D7_8 /*TypeText1*/  b_L_169)
{
#line 123 "../Main.m3"
Main__overlap_1_NE_false_Frame_t _frame;
#line 123 "../Main.m3"
_frame._unused=(ADDRESS)&_frame;
#line 123 "../Main.m3"
 /* load */
#line 123 "../Main.m3"
 /* load */
#line 123 "../Main.m3"
 /* compare */
#line 123 "../Main.m3"
 /* exit_proc */
#line 123 "../Main.m3"
return ((INT64)(m3_ne(UINT64,
 ((INT64)(b_L_169)),
 ((INT64)(a_L_168)))));
#line 123 "../Main.m3"
 /* end_procedure */
#line 123 "../Main.m3"
} /* ord_enum_vs_negative_LT_false */
#line 123 "../Main.m3"
 /* set_source_line */
#line 123 "../Main.m3"
#line 128 "../Main.m3"
 /* begin_procedure */
#line 128 "../Main.m3"
struct Main__ord_enum_vs_negative_LT_false_Frame_t {
#line 128 "../Main.m3"
ADDRESS _unused;
#line 128 "../Main.m3"
};
#line 128 "../Main.m3"
BOOLEAN
__cdecl
Main__ord_enum_vs_negative_LT_false(
   /* Param_Type1 */ Main__Number a_L_171)
{
#line 128 "../Main.m3"
Main__ord_enum_vs_negative_LT_false_Frame_t _frame;
#line 128 "../Main.m3"
_frame._unused=(ADDRESS)&_frame;
#line 128 "../Main.m3"
 /* load_integer */
#line 128 "../Main.m3"
 /* load */
#line 128 "../Main.m3"
 /* compare */
#line 128 "../Main.m3"
 /* exit_proc */
#line 128 "../Main.m3"
return ((INT64)(m3_gt(INT64,
  INT64_(-1),
 ((INT64)(a_L_171)))));
#line 128 "../Main.m3"
 /* end_procedure */
#line 128 "../Main.m3"
} /* ord_enum_vs_negative_LE_false */
#line 128 "../Main.m3"
 /* set_source_line */
#line 128 "../Main.m3"
#line 129 "../Main.m3"
 /* begin_procedure */
#line 129 "../Main.m3"
struct Main__ord_enum_vs_negative_LE_false_Frame_t {
#line 129 "../Main.m3"
ADDRESS _unused;
#line 129 "../Main.m3"
};
#line 129 "../Main.m3"
BOOLEAN
__cdecl
Main__ord_enum_vs_negative_LE_false(
   /* Param_Type1 */ Main__Number a_L_173)
{
#line 129 "../Main.m3"
Main__ord_enum_vs_negative_LE_false_Frame_t _frame;
#line 129 "../Main.m3"
_frame._unused=(ADDRESS)&_frame;
#line 129 "../Main.m3"
 /* load_integer */
#line 129 "../Main.m3"
 /* load */
#line 129 "../Main.m3"
 /* compare */
#line 129 "../Main.m3"
 /* exit_proc */
#line 129 "../Main.m3"
return ((INT64)(m3_ge(INT64,
  INT64_(-1),
 ((INT64)(a_L_173)))));
#line 129 "../Main.m3"
 /* end_procedure */
#line 129 "../Main.m3"
} /* ord_enum_vs_negative_GT_true */
#line 129 "../Main.m3"
 /* set_source_line */
#line 129 "../Main.m3"
#line 130 "../Main.m3"
 /* begin_procedure */
#line 130 "../Main.m3"
struct Main__ord_enum_vs_negative_GT_true_Frame_t {
#line 130 "../Main.m3"
ADDRESS _unused;
#line 130 "../Main.m3"
};
#line 130 "../Main.m3"
BOOLEAN
__cdecl
Main__ord_enum_vs_negative_GT_true(
   /* Param_Type1 */ Main__Number a_L_175)
{
#line 130 "../Main.m3"
Main__ord_enum_vs_negative_GT_true_Frame_t _frame;
#line 130 "../Main.m3"
_frame._unused=(ADDRESS)&_frame;
#line 130 "../Main.m3"
 /* load_integer */
#line 130 "../Main.m3"
 /* load */
#line 130 "../Main.m3"
 /* compare */
#line 130 "../Main.m3"
 /* exit_proc */
#line 130 "../Main.m3"
return ((INT64)(m3_lt(INT64,
  INT64_(-1),
 ((INT64)(a_L_175)))));
#line 130 "../Main.m3"
 /* end_procedure */
#line 130 "../Main.m3"
} /* ord_enum_vs_negative_GE_true */
#line 130 "../Main.m3"
 /* set_source_line */
#line 130 "../Main.m3"
#line 131 "../Main.m3"
 /* begin_procedure */
#line 131 "../Main.m3"
struct Main__ord_enum_vs_negative_GE_true_Frame_t {
#line 131 "../Main.m3"
ADDRESS _unused;
#line 131 "../Main.m3"
};
#line 131 "../Main.m3"
BOOLEAN
__cdecl
Main__ord_enum_vs_negative_GE_true(
   /* Param_Type1 */ Main__Number a_L_177)
{
#line 131 "../Main.m3"
Main__ord_enum_vs_negative_GE_true_Frame_t _frame;
#line 131 "../Main.m3"
_frame._unused=(ADDRESS)&_frame;
#line 131 "../Main.m3"
 /* load_integer */
#line 131 "../Main.m3"
 /* load */
#line 131 "../Main.m3"
 /* compare */
#line 131 "../Main.m3"
 /* exit_proc */
#line 131 "../Main.m3"
return ((INT64)(m3_le(INT64,
  INT64_(-1),
 ((INT64)(a_L_177)))));
#line 131 "../Main.m3"
 /* end_procedure */
#line 131 "../Main.m3"
} /* ord_enum_vs_negative_EQ_false */
#line 131 "../Main.m3"
 /* set_source_line */
#line 131 "../Main.m3"
#line 132 "../Main.m3"
 /* begin_procedure */
#line 132 "../Main.m3"
struct Main__ord_enum_vs_negative_EQ_false_Frame_t {
#line 132 "../Main.m3"
ADDRESS _unused;
#line 132 "../Main.m3"
};
#line 132 "../Main.m3"
BOOLEAN
__cdecl
Main__ord_enum_vs_negative_EQ_false(
   /* Param_Type1 */ Main__Number a_L_179)
{
#line 132 "../Main.m3"
Main__ord_enum_vs_negative_EQ_false_Frame_t _frame;
#line 132 "../Main.m3"
_frame._unused=(ADDRESS)&_frame;
#line 132 "../Main.m3"
 /* load_integer */
#line 132 "../Main.m3"
 /* load */
#line 132 "../Main.m3"
 /* compare */
#line 132 "../Main.m3"
 /* exit_proc */
#line 132 "../Main.m3"
return ((INT64)(m3_eq(INT64,
  INT64_(-1),
 ((INT64)(a_L_179)))));
#line 132 "../Main.m3"
 /* end_procedure */
#line 132 "../Main.m3"
} /* ord_enum_vs_negative_NE_true */
#line 132 "../Main.m3"
 /* set_source_line */
#line 132 "../Main.m3"
#line 133 "../Main.m3"
 /* begin_procedure */
#line 133 "../Main.m3"
struct Main__ord_enum_vs_negative_NE_true_Frame_t {
#line 133 "../Main.m3"
ADDRESS _unused;
#line 133 "../Main.m3"
};
#line 133 "../Main.m3"
BOOLEAN
__cdecl
Main__ord_enum_vs_negative_NE_true(
   /* Param_Type1 */ Main__Number a_L_181)
{
#line 133 "../Main.m3"
Main__ord_enum_vs_negative_NE_true_Frame_t _frame;
#line 133 "../Main.m3"
_frame._unused=(ADDRESS)&_frame;
#line 133 "../Main.m3"
 /* load_integer */
#line 133 "../Main.m3"
 /* load */
#line 133 "../Main.m3"
 /* compare */
#line 133 "../Main.m3"
 /* exit_proc */
#line 133 "../Main.m3"
return ((INT64)(m3_ne(INT64,
  INT64_(-1),
 ((INT64)(a_L_181)))));
#line 133 "../Main.m3"
 /* end_procedure */
#line 133 "../Main.m3"
} /* abs_vs_negative_LT_false */
#line 133 "../Main.m3"
 /* set_source_line */
#line 133 "../Main.m3"
#line 138 "../Main.m3"
 /* begin_procedure */
#line 138 "../Main.m3"
struct Main__abs_vs_negative_LT_false_Frame_t {
#line 138 "../Main.m3"
ADDRESS _unused;
#line 138 "../Main.m3"
};
#line 138 "../Main.m3"
BOOLEAN
__cdecl
Main__abs_vs_negative_LT_false(
   /* Param_Type1 */ INTEGER a_L_183)
{
#line 138 "../Main.m3"
Main__abs_vs_negative_LT_false_Frame_t _frame;
#line 138 "../Main.m3"
_frame._unused=(ADDRESS)&_frame;
#line 138 "../Main.m3"
 /* load */
#line 138 "../Main.m3"
 /* abs */
#line 138 "../Main.m3"
 /* load_integer */
#line 138 "../Main.m3"
 /* compare */
#line 138 "../Main.m3"
 /* exit_proc */
#line 138 "../Main.m3"
return ((INT64)(m3_lt(INT64,
 m3_abs_INT64(
  a_L_183),
  INT64_(-1))));
#line 138 "../Main.m3"
 /* end_procedure */
#line 138 "../Main.m3"
} /* abs_vs_negative_LE_false */
#line 138 "../Main.m3"
 /* set_source_line */
#line 138 "../Main.m3"
#line 139 "../Main.m3"
 /* begin_procedure */
#line 139 "../Main.m3"
struct Main__abs_vs_negative_LE_false_Frame_t {
#line 139 "../Main.m3"
ADDRESS _unused;
#line 139 "../Main.m3"
};
#line 139 "../Main.m3"
BOOLEAN
__cdecl
Main__abs_vs_negative_LE_false(
   /* Param_Type1 */ INTEGER a_L_185)
{
#line 139 "../Main.m3"
Main__abs_vs_negative_LE_false_Frame_t _frame;
#line 139 "../Main.m3"
_frame._unused=(ADDRESS)&_frame;
#line 139 "../Main.m3"
 /* load */
#line 139 "../Main.m3"
 /* abs */
#line 139 "../Main.m3"
 /* load_integer */
#line 139 "../Main.m3"
 /* compare */
#line 139 "../Main.m3"
 /* exit_proc */
#line 139 "../Main.m3"
return ((INT64)(m3_le(INT64,
 m3_abs_INT64(
  a_L_185),
  INT64_(-1))));
#line 139 "../Main.m3"
 /* end_procedure */
#line 139 "../Main.m3"
} /* abs_vs_negative_GT_true */
#line 139 "../Main.m3"
 /* set_source_line */
#line 139 "../Main.m3"
#line 140 "../Main.m3"
 /* begin_procedure */
#line 140 "../Main.m3"
struct Main__abs_vs_negative_GT_true_Frame_t {
#line 140 "../Main.m3"
ADDRESS _unused;
#line 140 "../Main.m3"
};
#line 140 "../Main.m3"
BOOLEAN
__cdecl
Main__abs_vs_negative_GT_true(
   /* Param_Type1 */ INTEGER a_L_187)
{
#line 140 "../Main.m3"
Main__abs_vs_negative_GT_true_Frame_t _frame;
#line 140 "../Main.m3"
_frame._unused=(ADDRESS)&_frame;
#line 140 "../Main.m3"
 /* load */
#line 140 "../Main.m3"
 /* abs */
#line 140 "../Main.m3"
 /* load_integer */
#line 140 "../Main.m3"
 /* compare */
#line 140 "../Main.m3"
 /* exit_proc */
#line 140 "../Main.m3"
return ((INT64)(m3_gt(INT64,
 m3_abs_INT64(
  a_L_187),
  INT64_(-1))));
#line 140 "../Main.m3"
 /* end_procedure */
#line 140 "../Main.m3"
} /* abs_vs_negative_GE_true */
#line 140 "../Main.m3"
 /* set_source_line */
#line 140 "../Main.m3"
#line 141 "../Main.m3"
 /* begin_procedure */
#line 141 "../Main.m3"
struct Main__abs_vs_negative_GE_true_Frame_t {
#line 141 "../Main.m3"
ADDRESS _unused;
#line 141 "../Main.m3"
};
#line 141 "../Main.m3"
BOOLEAN
__cdecl
Main__abs_vs_negative_GE_true(
   /* Param_Type1 */ INTEGER a_L_189)
{
#line 141 "../Main.m3"
Main__abs_vs_negative_GE_true_Frame_t _frame;
#line 141 "../Main.m3"
_frame._unused=(ADDRESS)&_frame;
#line 141 "../Main.m3"
 /* load */
#line 141 "../Main.m3"
 /* abs */
#line 141 "../Main.m3"
 /* load_integer */
#line 141 "../Main.m3"
 /* compare */
#line 141 "../Main.m3"
 /* exit_proc */
#line 141 "../Main.m3"
return ((INT64)(m3_ge(INT64,
 m3_abs_INT64(
  a_L_189),
  INT64_(-1))));
#line 141 "../Main.m3"
 /* end_procedure */
#line 141 "../Main.m3"
} /* abs_vs_negative_EQ_false */
#line 141 "../Main.m3"
 /* set_source_line */
#line 141 "../Main.m3"
#line 142 "../Main.m3"
 /* begin_procedure */
#line 142 "../Main.m3"
struct Main__abs_vs_negative_EQ_false_Frame_t {
#line 142 "../Main.m3"
ADDRESS _unused;
#line 142 "../Main.m3"
};
#line 142 "../Main.m3"
BOOLEAN
__cdecl
Main__abs_vs_negative_EQ_false(
   /* Param_Type1 */ INTEGER a_L_191)
{
#line 142 "../Main.m3"
Main__abs_vs_negative_EQ_false_Frame_t _frame;
#line 142 "../Main.m3"
_frame._unused=(ADDRESS)&_frame;
#line 142 "../Main.m3"
 /* load */
#line 142 "../Main.m3"
 /* abs */
#line 142 "../Main.m3"
 /* load_integer */
#line 142 "../Main.m3"
 /* compare */
#line 142 "../Main.m3"
 /* exit_proc */
#line 142 "../Main.m3"
return ((INT64)(m3_eq(INT64,
 m3_abs_INT64(
  a_L_191),
  INT64_(-1))));
#line 142 "../Main.m3"
 /* end_procedure */
#line 142 "../Main.m3"
} /* abs_vs_negative_NE_true */
#line 142 "../Main.m3"
 /* set_source_line */
#line 142 "../Main.m3"
#line 143 "../Main.m3"
 /* begin_procedure */
#line 143 "../Main.m3"
struct Main__abs_vs_negative_NE_true_Frame_t {
#line 143 "../Main.m3"
ADDRESS _unused;
#line 143 "../Main.m3"
};
#line 143 "../Main.m3"
BOOLEAN
__cdecl
Main__abs_vs_negative_NE_true(
   /* Param_Type1 */ INTEGER a_L_193)
{
#line 143 "../Main.m3"
Main__abs_vs_negative_NE_true_Frame_t _frame;
#line 143 "../Main.m3"
_frame._unused=(ADDRESS)&_frame;
#line 143 "../Main.m3"
 /* load */
#line 143 "../Main.m3"
 /* abs */
#line 143 "../Main.m3"
 /* load_integer */
#line 143 "../Main.m3"
 /* compare */
#line 143 "../Main.m3"
 /* exit_proc */
#line 143 "../Main.m3"
return ((INT64)(m3_ne(INT64,
 m3_abs_INT64(
  a_L_193),
  INT64_(-1))));
#line 143 "../Main.m3"
 /* end_procedure */
#line 143 "../Main.m3"
} /* abs_vs_zero_LT_false */
#line 143 "../Main.m3"
 /* set_source_line */
#line 143 "../Main.m3"
#line 148 "../Main.m3"
 /* begin_procedure */
#line 148 "../Main.m3"
struct Main__abs_vs_zero_LT_false_Frame_t {
#line 148 "../Main.m3"
ADDRESS _unused;
#line 148 "../Main.m3"
};
#line 148 "../Main.m3"
BOOLEAN
__cdecl
Main__abs_vs_zero_LT_false(
   /* Param_Type1 */ INTEGER a_L_195)
{
#line 148 "../Main.m3"
Main__abs_vs_zero_LT_false_Frame_t _frame;
#line 148 "../Main.m3"
_frame._unused=(ADDRESS)&_frame;
#line 148 "../Main.m3"
 /* load */
#line 148 "../Main.m3"
 /* abs */
#line 148 "../Main.m3"
 /* load_integer */
#line 148 "../Main.m3"
 /* compare */
#line 148 "../Main.m3"
 /* exit_proc */
#line 148 "../Main.m3"
return ((INT64)(m3_lt(INT64,
 m3_abs_INT64(
  a_L_195),
  INT64_(0))));
#line 148 "../Main.m3"
 /* end_procedure */
#line 148 "../Main.m3"
} /* abs_vs_zero_GE_true */
#line 148 "../Main.m3"
 /* set_source_line */
#line 148 "../Main.m3"
#line 149 "../Main.m3"
 /* begin_procedure */
#line 149 "../Main.m3"
struct Main__abs_vs_zero_GE_true_Frame_t {
#line 149 "../Main.m3"
ADDRESS _unused;
#line 149 "../Main.m3"
};
#line 149 "../Main.m3"
BOOLEAN
__cdecl
Main__abs_vs_zero_GE_true(
   /* Param_Type1 */ INTEGER a_L_197)
{
#line 149 "../Main.m3"
Main__abs_vs_zero_GE_true_Frame_t _frame;
#line 149 "../Main.m3"
_frame._unused=(ADDRESS)&_frame;
#line 149 "../Main.m3"
 /* load */
#line 149 "../Main.m3"
 /* abs */
#line 149 "../Main.m3"
 /* load_integer */
#line 149 "../Main.m3"
 /* compare */
#line 149 "../Main.m3"
 /* exit_proc */
#line 149 "../Main.m3"
return ((INT64)(m3_ge(INT64,
 m3_abs_INT64(
  a_L_197),
  INT64_(0))));
#line 149 "../Main.m3"
 /* end_procedure */
#line 149 "../Main.m3"
} /* neg_abs_vs_zero_LE_true */
#line 149 "../Main.m3"
 /* set_source_line */
#line 149 "../Main.m3"
#line 154 "../Main.m3"
 /* begin_procedure */
#line 154 "../Main.m3"
struct Main__neg_abs_vs_zero_LE_true_Frame_t {
#line 154 "../Main.m3"
ADDRESS _unused;
#line 154 "../Main.m3"
};
#line 154 "../Main.m3"
BOOLEAN
__cdecl
Main__neg_abs_vs_zero_LE_true(
   /* Param_Type1 */ INTEGER a_L_199)
{
#line 154 "../Main.m3"
Main__neg_abs_vs_zero_LE_true_Frame_t _frame;
#line 154 "../Main.m3"
_frame._unused=(ADDRESS)&_frame;
#line 154 "../Main.m3"
 /* load */
#line 154 "../Main.m3"
 /* abs */
#line 154 "../Main.m3"
 /* negate */
#line 154 "../Main.m3"
 /* load_integer */
#line 154 "../Main.m3"
 /* compare */
#line 154 "../Main.m3"
 /* exit_proc */
#line 154 "../Main.m3"
return ((INT64)(m3_le(INT64,
 ((INT64)(-((INT64)(m3_abs_INT64(
  a_L_199))))),
  INT64_(0))));
#line 154 "../Main.m3"
 /* end_procedure */
#line 154 "../Main.m3"
} /* neg_abs_vs_zero_GT_false */
#line 154 "../Main.m3"
 /* set_source_line */
#line 154 "../Main.m3"
#line 155 "../Main.m3"
 /* begin_procedure */
#line 155 "../Main.m3"
struct Main__neg_abs_vs_zero_GT_false_Frame_t {
#line 155 "../Main.m3"
ADDRESS _unused;
#line 155 "../Main.m3"
};
#line 155 "../Main.m3"
BOOLEAN
__cdecl
Main__neg_abs_vs_zero_GT_false(
   /* Param_Type1 */ INTEGER a_L_201)
{
#line 155 "../Main.m3"
Main__neg_abs_vs_zero_GT_false_Frame_t _frame;
#line 155 "../Main.m3"
_frame._unused=(ADDRESS)&_frame;
#line 155 "../Main.m3"
 /* load */
#line 155 "../Main.m3"
 /* abs */
#line 155 "../Main.m3"
 /* negate */
#line 155 "../Main.m3"
 /* load_integer */
#line 155 "../Main.m3"
 /* compare */
#line 155 "../Main.m3"
 /* exit_proc */
#line 155 "../Main.m3"
return ((INT64)(m3_gt(INT64,
 ((INT64)(-((INT64)(m3_abs_INT64(
  a_L_201))))),
  INT64_(0))));
#line 155 "../Main.m3"
 /* end_procedure */
#line 155 "../Main.m3"
} /* neg_abs_vs_one_LT_true */
#line 155 "../Main.m3"
 /* set_source_line */
#line 155 "../Main.m3"
#line 160 "../Main.m3"
 /* begin_procedure */
#line 160 "../Main.m3"
struct Main__neg_abs_vs_one_LT_true_Frame_t {
#line 160 "../Main.m3"
ADDRESS _unused;
#line 160 "../Main.m3"
};
#line 160 "../Main.m3"
BOOLEAN
__cdecl
Main__neg_abs_vs_one_LT_true(
   /* Param_Type1 */ INTEGER a_L_203)
{
#line 160 "../Main.m3"
Main__neg_abs_vs_one_LT_true_Frame_t _frame;
#line 160 "../Main.m3"
_frame._unused=(ADDRESS)&_frame;
#line 160 "../Main.m3"
 /* load */
#line 160 "../Main.m3"
 /* abs */
#line 160 "../Main.m3"
 /* negate */
#line 160 "../Main.m3"
 /* load_integer */
#line 160 "../Main.m3"
 /* compare */
#line 160 "../Main.m3"
 /* exit_proc */
#line 160 "../Main.m3"
return ((INT64)(m3_lt(INT64,
 ((INT64)(-((INT64)(m3_abs_INT64(
  a_L_203))))),
  INT64_(1))));
#line 160 "../Main.m3"
 /* end_procedure */
#line 160 "../Main.m3"
} /* neg_abs_vs_one_LE_true */
#line 160 "../Main.m3"
 /* set_source_line */
#line 160 "../Main.m3"
#line 161 "../Main.m3"
 /* begin_procedure */
#line 161 "../Main.m3"
struct Main__neg_abs_vs_one_LE_true_Frame_t {
#line 161 "../Main.m3"
ADDRESS _unused;
#line 161 "../Main.m3"
};
#line 161 "../Main.m3"
BOOLEAN
__cdecl
Main__neg_abs_vs_one_LE_true(
   /* Param_Type1 */ INTEGER a_L_205)
{
#line 161 "../Main.m3"
Main__neg_abs_vs_one_LE_true_Frame_t _frame;
#line 161 "../Main.m3"
_frame._unused=(ADDRESS)&_frame;
#line 161 "../Main.m3"
 /* load */
#line 161 "../Main.m3"
 /* abs */
#line 161 "../Main.m3"
 /* negate */
#line 161 "../Main.m3"
 /* load_integer */
#line 161 "../Main.m3"
 /* compare */
#line 161 "../Main.m3"
 /* exit_proc */
#line 161 "../Main.m3"
return ((INT64)(m3_le(INT64,
 ((INT64)(-((INT64)(m3_abs_INT64(
  a_L_205))))),
  INT64_(1))));
#line 161 "../Main.m3"
 /* end_procedure */
#line 161 "../Main.m3"
} /* neg_abs_vs_one_GT_false */
#line 161 "../Main.m3"
 /* set_source_line */
#line 161 "../Main.m3"
#line 162 "../Main.m3"
 /* begin_procedure */
#line 162 "../Main.m3"
struct Main__neg_abs_vs_one_GT_false_Frame_t {
#line 162 "../Main.m3"
ADDRESS _unused;
#line 162 "../Main.m3"
};
#line 162 "../Main.m3"
BOOLEAN
__cdecl
Main__neg_abs_vs_one_GT_false(
   /* Param_Type1 */ INTEGER a_L_207)
{
#line 162 "../Main.m3"
Main__neg_abs_vs_one_GT_false_Frame_t _frame;
#line 162 "../Main.m3"
_frame._unused=(ADDRESS)&_frame;
#line 162 "../Main.m3"
 /* load */
#line 162 "../Main.m3"
 /* abs */
#line 162 "../Main.m3"
 /* negate */
#line 162 "../Main.m3"
 /* load_integer */
#line 162 "../Main.m3"
 /* compare */
#line 162 "../Main.m3"
 /* exit_proc */
#line 162 "../Main.m3"
return ((INT64)(m3_gt(INT64,
 ((INT64)(-((INT64)(m3_abs_INT64(
  a_L_207))))),
  INT64_(1))));
#line 162 "../Main.m3"
 /* end_procedure */
#line 162 "../Main.m3"
} /* neg_abs_vs_one_GE_false */
#line 162 "../Main.m3"
 /* set_source_line */
#line 162 "../Main.m3"
#line 163 "../Main.m3"
 /* begin_procedure */
#line 163 "../Main.m3"
struct Main__neg_abs_vs_one_GE_false_Frame_t {
#line 163 "../Main.m3"
ADDRESS _unused;
#line 163 "../Main.m3"
};
#line 163 "../Main.m3"
BOOLEAN
__cdecl
Main__neg_abs_vs_one_GE_false(
   /* Param_Type1 */ INTEGER a_L_209)
{
#line 163 "../Main.m3"
Main__neg_abs_vs_one_GE_false_Frame_t _frame;
#line 163 "../Main.m3"
_frame._unused=(ADDRESS)&_frame;
#line 163 "../Main.m3"
 /* load */
#line 163 "../Main.m3"
 /* abs */
#line 163 "../Main.m3"
 /* negate */
#line 163 "../Main.m3"
 /* load_integer */
#line 163 "../Main.m3"
 /* compare */
#line 163 "../Main.m3"
 /* exit_proc */
#line 163 "../Main.m3"
return ((INT64)(m3_ge(INT64,
 ((INT64)(-((INT64)(m3_abs_INT64(
  a_L_209))))),
  INT64_(1))));
#line 163 "../Main.m3"
 /* end_procedure */
#line 163 "../Main.m3"
} /* neg_abs_vs_one_EQ_false */
#line 163 "../Main.m3"
 /* set_source_line */
#line 163 "../Main.m3"
#line 164 "../Main.m3"
 /* begin_procedure */
#line 164 "../Main.m3"
struct Main__neg_abs_vs_one_EQ_false_Frame_t {
#line 164 "../Main.m3"
ADDRESS _unused;
#line 164 "../Main.m3"
};
#line 164 "../Main.m3"
BOOLEAN
__cdecl
Main__neg_abs_vs_one_EQ_false(
   /* Param_Type1 */ INTEGER a_L_211)
{
#line 164 "../Main.m3"
Main__neg_abs_vs_one_EQ_false_Frame_t _frame;
#line 164 "../Main.m3"
_frame._unused=(ADDRESS)&_frame;
#line 164 "../Main.m3"
 /* load */
#line 164 "../Main.m3"
 /* abs */
#line 164 "../Main.m3"
 /* negate */
#line 164 "../Main.m3"
 /* load_integer */
#line 164 "../Main.m3"
 /* compare */
#line 164 "../Main.m3"
 /* exit_proc */
#line 164 "../Main.m3"
return ((INT64)(m3_eq(INT64,
 ((INT64)(-((INT64)(m3_abs_INT64(
  a_L_211))))),
  INT64_(1))));
#line 164 "../Main.m3"
 /* end_procedure */
#line 164 "../Main.m3"
} /* neg_abs_vs_one_NE_true */
#line 164 "../Main.m3"
 /* set_source_line */
#line 164 "../Main.m3"
#line 165 "../Main.m3"
 /* begin_procedure */
#line 165 "../Main.m3"
struct Main__neg_abs_vs_one_NE_true_Frame_t {
#line 165 "../Main.m3"
ADDRESS _unused;
#line 165 "../Main.m3"
};
#line 165 "../Main.m3"
BOOLEAN
__cdecl
Main__neg_abs_vs_one_NE_true(
   /* Param_Type1 */ INTEGER a_L_213)
{
#line 165 "../Main.m3"
Main__neg_abs_vs_one_NE_true_Frame_t _frame;
#line 165 "../Main.m3"
_frame._unused=(ADDRESS)&_frame;
#line 165 "../Main.m3"
 /* load */
#line 165 "../Main.m3"
 /* abs */
#line 165 "../Main.m3"
 /* negate */
#line 165 "../Main.m3"
 /* load_integer */
#line 165 "../Main.m3"
 /* compare */
#line 165 "../Main.m3"
 /* exit_proc */
#line 165 "../Main.m3"
return ((INT64)(m3_ne(INT64,
 ((INT64)(-((INT64)(m3_abs_INT64(
  a_L_213))))),
  INT64_(1))));
#line 165 "../Main.m3"
 /* end_procedure */
#line 165 "../Main.m3"
} /* Main_M3 */
#line 165 "../Main.m3"
 /* module main body Main_M3 */
#line 165 "../Main.m3"
 /* set_source_line */
#line 165 "../Main.m3"
#line 167 "../Main.m3"
 /* begin_procedure */
#line 167 "../Main.m3"
struct Main_M3_Frame_t {
#line 167 "../Main.m3"
ADDRESS _unused;
#line 167 "../Main.m3"
};
#line 167 "../Main.m3"
RT0__ModulePtr
__cdecl
Main_M3(
   /* Param_Type1 */ INTEGER /*TypeText1*/  mode_L_23)
{
#line 167 "../Main.m3"
 /* Var_Type2 */ INT64 Main_m_214_L_215={0};//always-init
#line 167 "../Main.m3"
Main_M3_Frame_t _frame;
#line 167 "../Main.m3"
_frame._unused=(ADDRESS)&_frame;
#line 167 "../Main.m3"
 /* load */
#line 167 "../Main.m3"
 /* if_true_or_false */
#line 167 "../Main.m3"
 /* load_host_integer */
#line 167 "../Main.m3"
 /* load_integer */
#line 167 "../Main.m3"
 /* if_compare */
#line 167 "../Main.m3"
if(m3_eq(INT64,
  mode_L_23,
   INT64_(0)))goto L1;
#line 167 "../Main.m3"
 /* set_source_line */
#line 167 "../Main.m3"
#line 168 "../Main.m3"
 /* start_call_direct */
#line 168 "../Main.m3"
 /* load_integer */
#line 168 "../Main.m3"
 /* pop_param */
#line 168 "../Main.m3"
 /* call_direct */
#line 168 "../Main.m3"
 /* store */
#line 168 "../Main.m3"
(*(INT64*)(&Main_m_214_L_215))=(INT64)(((INT64)(Main__CardinalLT0_false(
  ( CARDINAL )(((UINT64)( INT64_(0))) )))));
#line 168 "../Main.m3"
 /* load */
#line 168 "../Main.m3"
 /* if_true_or_false */
#line 168 "../Main.m3"
 /* load_host_integer */
#line 168 "../Main.m3"
 /* load_integer */
#line 168 "../Main.m3"
 /* if_compare */
#line 168 "../Main.m3"
if(m3_eq(INT64,
  Main_m_214_L_215,
   INT64_(0)))goto L2;
#line 168 "../Main.m3"
 /* start_call_direct */
#line 168 "../Main.m3"
 /* load_address */
#line 168 "../Main.m3"
 /* pop_param */
#line 168 "../Main.m3"
 /* load_integer */
#line 168 "../Main.m3"
 /* pop_param */
#line 168 "../Main.m3"
 /* load_address */
#line 168 "../Main.m3"
 /* pop_param */
#line 168 "../Main.m3"
 /* call_direct */
#line 168 "../Main.m3"
RTHooks__AssertFailed(
  ( ADDRESS )(((ADDRESS)(&Main_m_M_Main_L_22)) ),
  ( INTEGER )(  INT64_(168) ),
  ( TEXT )(((ADDRESS)(INT64_(48)+((ADDRESS)(&Main_m_20_L_21)))) ));
#line 168 "../Main.m3"
 /* set_label */
#line 168 "../Main.m3"
L2:;
#line 168 "../Main.m3"
 /* set_source_line */
#line 168 "../Main.m3"
#line 169 "../Main.m3"
 /* start_call_direct */
#line 169 "../Main.m3"
 /* load_integer */
#line 169 "../Main.m3"
 /* pop_param */
#line 169 "../Main.m3"
 /* call_direct */
#line 169 "../Main.m3"
 /* store */
#line 169 "../Main.m3"
(*(INT64*)(&Main_m_214_L_215))=(INT64)(((INT64)(Main__CardinalGE0_true(
  ( CARDINAL )(((UINT64)( INT64_(0))) )))));
#line 169 "../Main.m3"
 /* load */
#line 169 "../Main.m3"
 /* if_true_or_false */
#line 169 "../Main.m3"
 /* load_host_integer */
#line 169 "../Main.m3"
 /* load_integer */
#line 169 "../Main.m3"
 /* if_compare */
#line 169 "../Main.m3"
if(m3_ne(INT64,
  Main_m_214_L_215,
   INT64_(0)))goto L3;
#line 169 "../Main.m3"
 /* start_call_direct */
#line 169 "../Main.m3"
 /* load_address */
#line 169 "../Main.m3"
 /* pop_param */
#line 169 "../Main.m3"
 /* load_integer */
#line 169 "../Main.m3"
 /* pop_param */
#line 169 "../Main.m3"
 /* load_address */
#line 169 "../Main.m3"
 /* pop_param */
#line 169 "../Main.m3"
 /* call_direct */
#line 169 "../Main.m3"
RTHooks__AssertFailed(
  ( ADDRESS )(((ADDRESS)(&Main_m_M_Main_L_22)) ),
  ( INTEGER )(  INT64_(169) ),
  ( TEXT )(((ADDRESS)(INT64_(104)+((ADDRESS)(&Main_m_20_L_21)))) ));
#line 169 "../Main.m3"
 /* set_label */
#line 169 "../Main.m3"
L3:;
#line 169 "../Main.m3"
 /* set_source_line */
#line 169 "../Main.m3"
#line 170 "../Main.m3"
 /* start_call_direct */
#line 170 "../Main.m3"
 /* load_integer */
#line 170 "../Main.m3"
 /* pop_param */
#line 170 "../Main.m3"
 /* call_direct */
#line 170 "../Main.m3"
 /* store */
#line 170 "../Main.m3"
(*(INT64*)(&Main_m_214_L_215))=(INT64)(((INT64)(Main__CardinalLTNeg1_false(
  ( CARDINAL )(((UINT64)( INT64_(0))) )))));
#line 170 "../Main.m3"
 /* load */
#line 170 "../Main.m3"
 /* if_true_or_false */
#line 170 "../Main.m3"
 /* load_host_integer */
#line 170 "../Main.m3"
 /* load_integer */
#line 170 "../Main.m3"
 /* if_compare */
#line 170 "../Main.m3"
if(m3_eq(INT64,
  Main_m_214_L_215,
   INT64_(0)))goto L4;
#line 170 "../Main.m3"
 /* start_call_direct */
#line 170 "../Main.m3"
 /* load_address */
#line 170 "../Main.m3"
 /* pop_param */
#line 170 "../Main.m3"
 /* load_integer */
#line 170 "../Main.m3"
 /* pop_param */
#line 170 "../Main.m3"
 /* load_address */
#line 170 "../Main.m3"
 /* pop_param */
#line 170 "../Main.m3"
 /* call_direct */
#line 170 "../Main.m3"
RTHooks__AssertFailed(
  ( ADDRESS )(((ADDRESS)(&Main_m_M_Main_L_22)) ),
  ( INTEGER )(  INT64_(170) ),
  ( TEXT )(((ADDRESS)(INT64_(152)+((ADDRESS)(&Main_m_20_L_21)))) ));
#line 170 "../Main.m3"
 /* set_label */
#line 170 "../Main.m3"
L4:;
#line 170 "../Main.m3"
 /* set_source_line */
#line 170 "../Main.m3"
#line 171 "../Main.m3"
 /* start_call_direct */
#line 171 "../Main.m3"
 /* load_integer */
#line 171 "../Main.m3"
 /* pop_param */
#line 171 "../Main.m3"
 /* call_direct */
#line 171 "../Main.m3"
 /* store */
#line 171 "../Main.m3"
(*(INT64*)(&Main_m_214_L_215))=(INT64)(((INT64)(Main__CardinalLENeg1_false(
  ( CARDINAL )(((UINT64)( INT64_(0))) )))));
#line 171 "../Main.m3"
 /* load */
#line 171 "../Main.m3"
 /* if_true_or_false */
#line 171 "../Main.m3"
 /* load_host_integer */
#line 171 "../Main.m3"
 /* load_integer */
#line 171 "../Main.m3"
 /* if_compare */
#line 171 "../Main.m3"
if(m3_eq(INT64,
  Main_m_214_L_215,
   INT64_(0)))goto L5;
#line 171 "../Main.m3"
 /* start_call_direct */
#line 171 "../Main.m3"
 /* load_address */
#line 171 "../Main.m3"
 /* pop_param */
#line 171 "../Main.m3"
 /* load_integer */
#line 171 "../Main.m3"
 /* pop_param */
#line 171 "../Main.m3"
 /* load_address */
#line 171 "../Main.m3"
 /* pop_param */
#line 171 "../Main.m3"
 /* call_direct */
#line 171 "../Main.m3"
RTHooks__AssertFailed(
  ( ADDRESS )(((ADDRESS)(&Main_m_M_Main_L_22)) ),
  ( INTEGER )(  INT64_(171) ),
  ( TEXT )(((ADDRESS)(INT64_(208)+((ADDRESS)(&Main_m_20_L_21)))) ));
#line 171 "../Main.m3"
 /* set_label */
#line 171 "../Main.m3"
L5:;
#line 171 "../Main.m3"
 /* set_source_line */
#line 171 "../Main.m3"
#line 172 "../Main.m3"
 /* start_call_direct */
#line 172 "../Main.m3"
 /* load_integer */
#line 172 "../Main.m3"
 /* pop_param */
#line 172 "../Main.m3"
 /* call_direct */
#line 172 "../Main.m3"
 /* store */
#line 172 "../Main.m3"
(*(INT64*)(&Main_m_214_L_215))=(INT64)(((INT64)(Main__CardinalGTNeg1_true(
  ( CARDINAL )(((UINT64)( INT64_(0))) )))));
#line 172 "../Main.m3"
 /* load */
#line 172 "../Main.m3"
 /* if_true_or_false */
#line 172 "../Main.m3"
 /* load_host_integer */
#line 172 "../Main.m3"
 /* load_integer */
#line 172 "../Main.m3"
 /* if_compare */
#line 172 "../Main.m3"
if(m3_ne(INT64,
  Main_m_214_L_215,
   INT64_(0)))goto L6;
#line 172 "../Main.m3"
 /* start_call_direct */
#line 172 "../Main.m3"
 /* load_address */
#line 172 "../Main.m3"
 /* pop_param */
#line 172 "../Main.m3"
 /* load_integer */
#line 172 "../Main.m3"
 /* pop_param */
#line 172 "../Main.m3"
 /* load_address */
#line 172 "../Main.m3"
 /* pop_param */
#line 172 "../Main.m3"
 /* call_direct */
#line 172 "../Main.m3"
RTHooks__AssertFailed(
  ( ADDRESS )(((ADDRESS)(&Main_m_M_Main_L_22)) ),
  ( INTEGER )(  INT64_(172) ),
  ( TEXT )(((ADDRESS)(INT64_(264)+((ADDRESS)(&Main_m_20_L_21)))) ));
#line 172 "../Main.m3"
 /* set_label */
#line 172 "../Main.m3"
L6:;
#line 172 "../Main.m3"
 /* set_source_line */
#line 172 "../Main.m3"
#line 173 "../Main.m3"
 /* start_call_direct */
#line 173 "../Main.m3"
 /* load_integer */
#line 173 "../Main.m3"
 /* pop_param */
#line 173 "../Main.m3"
 /* call_direct */
#line 173 "../Main.m3"
 /* store */
#line 173 "../Main.m3"
(*(INT64*)(&Main_m_214_L_215))=(INT64)(((INT64)(Main__CardinalGENeg1_true(
  ( CARDINAL )(((UINT64)( INT64_(0))) )))));
#line 173 "../Main.m3"
 /* load */
#line 173 "../Main.m3"
 /* if_true_or_false */
#line 173 "../Main.m3"
 /* load_host_integer */
#line 173 "../Main.m3"
 /* load_integer */
#line 173 "../Main.m3"
 /* if_compare */
#line 173 "../Main.m3"
if(m3_ne(INT64,
  Main_m_214_L_215,
   INT64_(0)))goto L7;
#line 173 "../Main.m3"
 /* start_call_direct */
#line 173 "../Main.m3"
 /* load_address */
#line 173 "../Main.m3"
 /* pop_param */
#line 173 "../Main.m3"
 /* load_integer */
#line 173 "../Main.m3"
 /* pop_param */
#line 173 "../Main.m3"
 /* load_address */
#line 173 "../Main.m3"
 /* pop_param */
#line 173 "../Main.m3"
 /* call_direct */
#line 173 "../Main.m3"
RTHooks__AssertFailed(
  ( ADDRESS )(((ADDRESS)(&Main_m_M_Main_L_22)) ),
  ( INTEGER )(  INT64_(173) ),
  ( TEXT )(((ADDRESS)(INT64_(312)+((ADDRESS)(&Main_m_20_L_21)))) ));
#line 173 "../Main.m3"
 /* set_label */
#line 173 "../Main.m3"
L7:;
#line 173 "../Main.m3"
 /* set_source_line */
#line 173 "../Main.m3"
#line 174 "../Main.m3"
 /* start_call_direct */
#line 174 "../Main.m3"
 /* load_integer */
#line 174 "../Main.m3"
 /* pop_param */
#line 174 "../Main.m3"
 /* call_direct */
#line 174 "../Main.m3"
 /* store */
#line 174 "../Main.m3"
(*(INT64*)(&Main_m_214_L_215))=(INT64)(((INT64)(Main__CardinalNENeg1_true(
  ( CARDINAL )(((UINT64)( INT64_(0))) )))));
#line 174 "../Main.m3"
 /* load */
#line 174 "../Main.m3"
 /* if_true_or_false */
#line 174 "../Main.m3"
 /* load_host_integer */
#line 174 "../Main.m3"
 /* load_integer */
#line 174 "../Main.m3"
 /* if_compare */
#line 174 "../Main.m3"
if(m3_ne(INT64,
  Main_m_214_L_215,
   INT64_(0)))goto L8;
#line 174 "../Main.m3"
 /* start_call_direct */
#line 174 "../Main.m3"
 /* load_address */
#line 174 "../Main.m3"
 /* pop_param */
#line 174 "../Main.m3"
 /* load_integer */
#line 174 "../Main.m3"
 /* pop_param */
#line 174 "../Main.m3"
 /* load_address */
#line 174 "../Main.m3"
 /* pop_param */
#line 174 "../Main.m3"
 /* call_direct */
#line 174 "../Main.m3"
RTHooks__AssertFailed(
  ( ADDRESS )(((ADDRESS)(&Main_m_M_Main_L_22)) ),
  ( INTEGER )(  INT64_(174) ),
  ( TEXT )(((ADDRESS)(INT64_(360)+((ADDRESS)(&Main_m_20_L_21)))) ));
#line 174 "../Main.m3"
 /* set_label */
#line 174 "../Main.m3"
L8:;
#line 174 "../Main.m3"
 /* set_source_line */
#line 174 "../Main.m3"
#line 175 "../Main.m3"
 /* start_call_direct */
#line 175 "../Main.m3"
 /* load_integer */
#line 175 "../Main.m3"
 /* pop_param */
#line 175 "../Main.m3"
 /* call_direct */
#line 175 "../Main.m3"
 /* store */
#line 175 "../Main.m3"
(*(INT64*)(&Main_m_214_L_215))=(INT64)(((INT64)(Main__CardinalEQNeg1_false(
  ( CARDINAL )(((UINT64)( INT64_(0))) )))));
#line 175 "../Main.m3"
 /* load */
#line 175 "../Main.m3"
 /* if_true_or_false */
#line 175 "../Main.m3"
 /* load_host_integer */
#line 175 "../Main.m3"
 /* load_integer */
#line 175 "../Main.m3"
 /* if_compare */
#line 175 "../Main.m3"
if(m3_eq(INT64,
  Main_m_214_L_215,
   INT64_(0)))goto L9;
#line 175 "../Main.m3"
 /* start_call_direct */
#line 175 "../Main.m3"
 /* load_address */
#line 175 "../Main.m3"
 /* pop_param */
#line 175 "../Main.m3"
 /* load_integer */
#line 175 "../Main.m3"
 /* pop_param */
#line 175 "../Main.m3"
 /* load_address */
#line 175 "../Main.m3"
 /* pop_param */
#line 175 "../Main.m3"
 /* call_direct */
#line 175 "../Main.m3"
RTHooks__AssertFailed(
  ( ADDRESS )(((ADDRESS)(&Main_m_M_Main_L_22)) ),
  ( INTEGER )(  INT64_(175) ),
  ( TEXT )(((ADDRESS)(INT64_(408)+((ADDRESS)(&Main_m_20_L_21)))) ));
#line 175 "../Main.m3"
 /* set_label */
#line 175 "../Main.m3"
L9:;
#line 175 "../Main.m3"
 /* set_source_line */
#line 175 "../Main.m3"
#line 176 "../Main.m3"
 /* start_call_direct */
#line 176 "../Main.m3"
 /* load_integer */
#line 176 "../Main.m3"
 /* pop_param */
#line 176 "../Main.m3"
 /* call_direct */
#line 176 "../Main.m3"
 /* store */
#line 176 "../Main.m3"
(*(INT64*)(&Main_m_214_L_215))=(INT64)(((INT64)(Main__CardinalLTNeg2_false(
  ( CARDINAL )(((UINT64)( INT64_(0))) )))));
#line 176 "../Main.m3"
 /* load */
#line 176 "../Main.m3"
 /* if_true_or_false */
#line 176 "../Main.m3"
 /* load_host_integer */
#line 176 "../Main.m3"
 /* load_integer */
#line 176 "../Main.m3"
 /* if_compare */
#line 176 "../Main.m3"
if(m3_eq(INT64,
  Main_m_214_L_215,
   INT64_(0)))goto LA;
#line 176 "../Main.m3"
 /* start_call_direct */
#line 176 "../Main.m3"
 /* load_address */
#line 176 "../Main.m3"
 /* pop_param */
#line 176 "../Main.m3"
 /* load_integer */
#line 176 "../Main.m3"
 /* pop_param */
#line 176 "../Main.m3"
 /* load_address */
#line 176 "../Main.m3"
 /* pop_param */
#line 176 "../Main.m3"
 /* call_direct */
#line 176 "../Main.m3"
RTHooks__AssertFailed(
  ( ADDRESS )(((ADDRESS)(&Main_m_M_Main_L_22)) ),
  ( INTEGER )(  INT64_(176) ),
  ( TEXT )(((ADDRESS)(INT64_(464)+((ADDRESS)(&Main_m_20_L_21)))) ));
#line 176 "../Main.m3"
 /* set_label */
#line 176 "../Main.m3"
LA:;
#line 176 "../Main.m3"
 /* set_source_line */
#line 176 "../Main.m3"
#line 177 "../Main.m3"
 /* start_call_direct */
#line 177 "../Main.m3"
 /* load_integer */
#line 177 "../Main.m3"
 /* pop_param */
#line 177 "../Main.m3"
 /* call_direct */
#line 177 "../Main.m3"
 /* store */
#line 177 "../Main.m3"
(*(INT64*)(&Main_m_214_L_215))=(INT64)(((INT64)(Main__CardinalLENeg2_false(
  ( CARDINAL )(((UINT64)( INT64_(0))) )))));
#line 177 "../Main.m3"
 /* load */
#line 177 "../Main.m3"
 /* if_true_or_false */
#line 177 "../Main.m3"
 /* load_host_integer */
#line 177 "../Main.m3"
 /* load_integer */
#line 177 "../Main.m3"
 /* if_compare */
#line 177 "../Main.m3"
if(m3_eq(INT64,
  Main_m_214_L_215,
   INT64_(0)))goto LB;
#line 177 "../Main.m3"
 /* start_call_direct */
#line 177 "../Main.m3"
 /* load_address */
#line 177 "../Main.m3"
 /* pop_param */
#line 177 "../Main.m3"
 /* load_integer */
#line 177 "../Main.m3"
 /* pop_param */
#line 177 "../Main.m3"
 /* load_address */
#line 177 "../Main.m3"
 /* pop_param */
#line 177 "../Main.m3"
 /* call_direct */
#line 177 "../Main.m3"
RTHooks__AssertFailed(
  ( ADDRESS )(((ADDRESS)(&Main_m_M_Main_L_22)) ),
  ( INTEGER )(  INT64_(177) ),
  ( TEXT )(((ADDRESS)(INT64_(520)+((ADDRESS)(&Main_m_20_L_21)))) ));
#line 177 "../Main.m3"
 /* set_label */
#line 177 "../Main.m3"
LB:;
#line 177 "../Main.m3"
 /* set_source_line */
#line 177 "../Main.m3"
#line 178 "../Main.m3"
 /* start_call_direct */
#line 178 "../Main.m3"
 /* load_integer */
#line 178 "../Main.m3"
 /* pop_param */
#line 178 "../Main.m3"
 /* call_direct */
#line 178 "../Main.m3"
 /* store */
#line 178 "../Main.m3"
(*(INT64*)(&Main_m_214_L_215))=(INT64)(((INT64)(Main__CardinalGTNeg2_true(
  ( CARDINAL )(((UINT64)( INT64_(0))) )))));
#line 178 "../Main.m3"
 /* load */
#line 178 "../Main.m3"
 /* if_true_or_false */
#line 178 "../Main.m3"
 /* load_host_integer */
#line 178 "../Main.m3"
 /* load_integer */
#line 178 "../Main.m3"
 /* if_compare */
#line 178 "../Main.m3"
if(m3_ne(INT64,
  Main_m_214_L_215,
   INT64_(0)))goto LC;
#line 178 "../Main.m3"
 /* start_call_direct */
#line 178 "../Main.m3"
 /* load_address */
#line 178 "../Main.m3"
 /* pop_param */
#line 178 "../Main.m3"
 /* load_integer */
#line 178 "../Main.m3"
 /* pop_param */
#line 178 "../Main.m3"
 /* load_address */
#line 178 "../Main.m3"
 /* pop_param */
#line 178 "../Main.m3"
 /* call_direct */
#line 178 "../Main.m3"
RTHooks__AssertFailed(
  ( ADDRESS )(((ADDRESS)(&Main_m_M_Main_L_22)) ),
  ( INTEGER )(  INT64_(178) ),
  ( TEXT )(((ADDRESS)(INT64_(576)+((ADDRESS)(&Main_m_20_L_21)))) ));
#line 178 "../Main.m3"
 /* set_label */
#line 178 "../Main.m3"
LC:;
#line 178 "../Main.m3"
 /* set_source_line */
#line 178 "../Main.m3"
#line 179 "../Main.m3"
 /* start_call_direct */
#line 179 "../Main.m3"
 /* load_integer */
#line 179 "../Main.m3"
 /* pop_param */
#line 179 "../Main.m3"
 /* call_direct */
#line 179 "../Main.m3"
 /* store */
#line 179 "../Main.m3"
(*(INT64*)(&Main_m_214_L_215))=(INT64)(((INT64)(Main__CardinalGENeg2_true(
  ( CARDINAL )(((UINT64)( INT64_(0))) )))));
#line 179 "../Main.m3"
 /* load */
#line 179 "../Main.m3"
 /* if_true_or_false */
#line 179 "../Main.m3"
 /* load_host_integer */
#line 179 "../Main.m3"
 /* load_integer */
#line 179 "../Main.m3"
 /* if_compare */
#line 179 "../Main.m3"
if(m3_ne(INT64,
  Main_m_214_L_215,
   INT64_(0)))goto LD;
#line 179 "../Main.m3"
 /* start_call_direct */
#line 179 "../Main.m3"
 /* load_address */
#line 179 "../Main.m3"
 /* pop_param */
#line 179 "../Main.m3"
 /* load_integer */
#line 179 "../Main.m3"
 /* pop_param */
#line 179 "../Main.m3"
 /* load_address */
#line 179 "../Main.m3"
 /* pop_param */
#line 179 "../Main.m3"
 /* call_direct */
#line 179 "../Main.m3"
RTHooks__AssertFailed(
  ( ADDRESS )(((ADDRESS)(&Main_m_M_Main_L_22)) ),
  ( INTEGER )(  INT64_(179) ),
  ( TEXT )(((ADDRESS)(INT64_(624)+((ADDRESS)(&Main_m_20_L_21)))) ));
#line 179 "../Main.m3"
 /* set_label */
#line 179 "../Main.m3"
LD:;
#line 179 "../Main.m3"
 /* set_source_line */
#line 179 "../Main.m3"
#line 180 "../Main.m3"
 /* start_call_direct */
#line 180 "../Main.m3"
 /* load_integer */
#line 180 "../Main.m3"
 /* pop_param */
#line 180 "../Main.m3"
 /* call_direct */
#line 180 "../Main.m3"
 /* store */
#line 180 "../Main.m3"
(*(INT64*)(&Main_m_214_L_215))=(INT64)(((INT64)(Main__CardinalNENeg2_true(
  ( CARDINAL )(((UINT64)( INT64_(0))) )))));
#line 180 "../Main.m3"
 /* load */
#line 180 "../Main.m3"
 /* if_true_or_false */
#line 180 "../Main.m3"
 /* load_host_integer */
#line 180 "../Main.m3"
 /* load_integer */
#line 180 "../Main.m3"
 /* if_compare */
#line 180 "../Main.m3"
if(m3_ne(INT64,
  Main_m_214_L_215,
   INT64_(0)))goto LE;
#line 180 "../Main.m3"
 /* start_call_direct */
#line 180 "../Main.m3"
 /* load_address */
#line 180 "../Main.m3"
 /* pop_param */
#line 180 "../Main.m3"
 /* load_integer */
#line 180 "../Main.m3"
 /* pop_param */
#line 180 "../Main.m3"
 /* load_address */
#line 180 "../Main.m3"
 /* pop_param */
#line 180 "../Main.m3"
 /* call_direct */
#line 180 "../Main.m3"
RTHooks__AssertFailed(
  ( ADDRESS )(((ADDRESS)(&Main_m_M_Main_L_22)) ),
  ( INTEGER )(  INT64_(180) ),
  ( TEXT )(((ADDRESS)(INT64_(672)+((ADDRESS)(&Main_m_20_L_21)))) ));
#line 180 "../Main.m3"
 /* set_label */
#line 180 "../Main.m3"
LE:;
#line 180 "../Main.m3"
 /* set_source_line */
#line 180 "../Main.m3"
#line 181 "../Main.m3"
 /* start_call_direct */
#line 181 "../Main.m3"
 /* load_integer */
#line 181 "../Main.m3"
 /* pop_param */
#line 181 "../Main.m3"
 /* call_direct */
#line 181 "../Main.m3"
 /* store */
#line 181 "../Main.m3"
(*(INT64*)(&Main_m_214_L_215))=(INT64)(((INT64)(Main__CardinalEQNeg2_false(
  ( CARDINAL )(((UINT64)( INT64_(0))) )))));
#line 181 "../Main.m3"
 /* load */
#line 181 "../Main.m3"
 /* if_true_or_false */
#line 181 "../Main.m3"
 /* load_host_integer */
#line 181 "../Main.m3"
 /* load_integer */
#line 181 "../Main.m3"
 /* if_compare */
#line 181 "../Main.m3"
if(m3_eq(INT64,
  Main_m_214_L_215,
   INT64_(0)))goto LF;
#line 181 "../Main.m3"
 /* start_call_direct */
#line 181 "../Main.m3"
 /* load_address */
#line 181 "../Main.m3"
 /* pop_param */
#line 181 "../Main.m3"
 /* load_integer */
#line 181 "../Main.m3"
 /* pop_param */
#line 181 "../Main.m3"
 /* load_address */
#line 181 "../Main.m3"
 /* pop_param */
#line 181 "../Main.m3"
 /* call_direct */
#line 181 "../Main.m3"
RTHooks__AssertFailed(
  ( ADDRESS )(((ADDRESS)(&Main_m_M_Main_L_22)) ),
  ( INTEGER )(  INT64_(181) ),
  ( TEXT )(((ADDRESS)(INT64_(720)+((ADDRESS)(&Main_m_20_L_21)))) ));
#line 181 "../Main.m3"
 /* set_label */
#line 181 "../Main.m3"
LF:;
#line 181 "../Main.m3"
 /* set_source_line */
#line 181 "../Main.m3"
#line 182 "../Main.m3"
 /* start_call_direct */
#line 182 "../Main.m3"
 /* load_integer */
#line 182 "../Main.m3"
 /* pop_param */
#line 182 "../Main.m3"
 /* call_direct */
#line 182 "../Main.m3"
 /* store */
#line 182 "../Main.m3"
(*(INT64*)(&Main_m_214_L_215))=(INT64)(((INT64)(Main__LongcardLT0_false(
  ( LONGCARD )(((UINT64)( INT64_(0))) )))));
#line 182 "../Main.m3"
 /* load */
#line 182 "../Main.m3"
 /* if_true_or_false */
#line 182 "../Main.m3"
 /* load_host_integer */
#line 182 "../Main.m3"
 /* load_integer */
#line 182 "../Main.m3"
 /* if_compare */
#line 182 "../Main.m3"
if(m3_eq(INT64,
  Main_m_214_L_215,
   INT64_(0)))goto L10;
#line 182 "../Main.m3"
 /* start_call_direct */
#line 182 "../Main.m3"
 /* load_address */
#line 182 "../Main.m3"
 /* pop_param */
#line 182 "../Main.m3"
 /* load_integer */
#line 182 "../Main.m3"
 /* pop_param */
#line 182 "../Main.m3"
 /* load_address */
#line 182 "../Main.m3"
 /* pop_param */
#line 182 "../Main.m3"
 /* call_direct */
#line 182 "../Main.m3"
RTHooks__AssertFailed(
  ( ADDRESS )(((ADDRESS)(&Main_m_M_Main_L_22)) ),
  ( INTEGER )(  INT64_(182) ),
  ( TEXT )(((ADDRESS)(INT64_(776)+((ADDRESS)(&Main_m_20_L_21)))) ));
#line 182 "../Main.m3"
 /* set_label */
#line 182 "../Main.m3"
L10:;
#line 182 "../Main.m3"
 /* set_source_line */
#line 182 "../Main.m3"
#line 183 "../Main.m3"
 /* start_call_direct */
#line 183 "../Main.m3"
 /* load_integer */
#line 183 "../Main.m3"
 /* pop_param */
#line 183 "../Main.m3"
 /* call_direct */
#line 183 "../Main.m3"
 /* store */
#line 183 "../Main.m3"
(*(INT64*)(&Main_m_214_L_215))=(INT64)(((INT64)(Main__LongcardGE0_true(
  ( LONGCARD )(((UINT64)( INT64_(0))) )))));
#line 183 "../Main.m3"
 /* load */
#line 183 "../Main.m3"
 /* if_true_or_false */
#line 183 "../Main.m3"
 /* load_host_integer */
#line 183 "../Main.m3"
 /* load_integer */
#line 183 "../Main.m3"
 /* if_compare */
#line 183 "../Main.m3"
if(m3_ne(INT64,
  Main_m_214_L_215,
   INT64_(0)))goto L11;
#line 183 "../Main.m3"
 /* start_call_direct */
#line 183 "../Main.m3"
 /* load_address */
#line 183 "../Main.m3"
 /* pop_param */
#line 183 "../Main.m3"
 /* load_integer */
#line 183 "../Main.m3"
 /* pop_param */
#line 183 "../Main.m3"
 /* load_address */
#line 183 "../Main.m3"
 /* pop_param */
#line 183 "../Main.m3"
 /* call_direct */
#line 183 "../Main.m3"
RTHooks__AssertFailed(
  ( ADDRESS )(((ADDRESS)(&Main_m_M_Main_L_22)) ),
  ( INTEGER )(  INT64_(183) ),
  ( TEXT )(((ADDRESS)(INT64_(832)+((ADDRESS)(&Main_m_20_L_21)))) ));
#line 183 "../Main.m3"
 /* set_label */
#line 183 "../Main.m3"
L11:;
#line 183 "../Main.m3"
 /* set_source_line */
#line 183 "../Main.m3"
#line 184 "../Main.m3"
 /* start_call_direct */
#line 184 "../Main.m3"
 /* load_integer */
#line 184 "../Main.m3"
 /* pop_param */
#line 184 "../Main.m3"
 /* load_integer */
#line 184 "../Main.m3"
 /* pop_param */
#line 184 "../Main.m3"
 /* call_direct */
#line 184 "../Main.m3"
 /* store */
#line 184 "../Main.m3"
(*(INT64*)(&Main_m_214_L_215))=(INT64)(((INT64)(Main__no_overlap_less_LT_true(
  ( T8B2831D7_8 /*TypeText1*/  )(((UINT8)( INT64_(0))) ),
  ( T892833D7_8 /*TypeText1*/  )(((UINT8)( INT64_(2))) )))));
#line 184 "../Main.m3"
 /* load */
#line 184 "../Main.m3"
 /* if_true_or_false */
#line 184 "../Main.m3"
 /* load_host_integer */
#line 184 "../Main.m3"
 /* load_integer */
#line 184 "../Main.m3"
 /* if_compare */
#line 184 "../Main.m3"
if(m3_ne(INT64,
  Main_m_214_L_215,
   INT64_(0)))goto L12;
#line 184 "../Main.m3"
 /* start_call_direct */
#line 184 "../Main.m3"
 /* load_address */
#line 184 "../Main.m3"
 /* pop_param */
#line 184 "../Main.m3"
 /* load_integer */
#line 184 "../Main.m3"
 /* pop_param */
#line 184 "../Main.m3"
 /* load_address */
#line 184 "../Main.m3"
 /* pop_param */
#line 184 "../Main.m3"
 /* call_direct */
#line 184 "../Main.m3"
RTHooks__AssertFailed(
  ( ADDRESS )(((ADDRESS)(&Main_m_M_Main_L_22)) ),
  ( INTEGER )(  INT64_(184) ),
  ( TEXT )(((ADDRESS)(INT64_(880)+((ADDRESS)(&Main_m_20_L_21)))) ));
#line 184 "../Main.m3"
 /* set_label */
#line 184 "../Main.m3"
L12:;
#line 184 "../Main.m3"
 /* set_source_line */
#line 184 "../Main.m3"
#line 185 "../Main.m3"
 /* start_call_direct */
#line 185 "../Main.m3"
 /* load_integer */
#line 185 "../Main.m3"
 /* pop_param */
#line 185 "../Main.m3"
 /* load_integer */
#line 185 "../Main.m3"
 /* pop_param */
#line 185 "../Main.m3"
 /* call_direct */
#line 185 "../Main.m3"
 /* store */
#line 185 "../Main.m3"
(*(INT64*)(&Main_m_214_L_215))=(INT64)(((INT64)(Main__no_overlap_less_LE_true(
  ( T8B2831D7_8 /*TypeText1*/  )(((UINT8)( INT64_(0))) ),
  ( T892833D7_8 /*TypeText1*/  )(((UINT8)( INT64_(2))) )))));
#line 185 "../Main.m3"
 /* load */
#line 185 "../Main.m3"
 /* if_true_or_false */
#line 185 "../Main.m3"
 /* load_host_integer */
#line 185 "../Main.m3"
 /* load_integer */
#line 185 "../Main.m3"
 /* if_compare */
#line 185 "../Main.m3"
if(m3_ne(INT64,
  Main_m_214_L_215,
   INT64_(0)))goto L13;
#line 185 "../Main.m3"
 /* start_call_direct */
#line 185 "../Main.m3"
 /* load_address */
#line 185 "../Main.m3"
 /* pop_param */
#line 185 "../Main.m3"
 /* load_integer */
#line 185 "../Main.m3"
 /* pop_param */
#line 185 "../Main.m3"
 /* load_address */
#line 185 "../Main.m3"
 /* pop_param */
#line 185 "../Main.m3"
 /* call_direct */
#line 185 "../Main.m3"
RTHooks__AssertFailed(
  ( ADDRESS )(((ADDRESS)(&Main_m_M_Main_L_22)) ),
  ( INTEGER )(  INT64_(185) ),
  ( TEXT )(((ADDRESS)(INT64_(936)+((ADDRESS)(&Main_m_20_L_21)))) ));
#line 185 "../Main.m3"
 /* set_label */
#line 185 "../Main.m3"
L13:;
#line 185 "../Main.m3"
 /* set_source_line */
#line 185 "../Main.m3"
#line 186 "../Main.m3"
 /* start_call_direct */
#line 186 "../Main.m3"
 /* load_integer */
#line 186 "../Main.m3"
 /* pop_param */
#line 186 "../Main.m3"
 /* load_integer */
#line 186 "../Main.m3"
 /* pop_param */
#line 186 "../Main.m3"
 /* call_direct */
#line 186 "../Main.m3"
 /* store */
#line 186 "../Main.m3"
(*(INT64*)(&Main_m_214_L_215))=(INT64)(((INT64)(Main__no_overlap_less_GT_false(
  ( T8B2831D7_8 /*TypeText1*/  )(((UINT8)( INT64_(0))) ),
  ( T892833D7_8 /*TypeText1*/  )(((UINT8)( INT64_(2))) )))));
#line 186 "../Main.m3"
 /* load */
#line 186 "../Main.m3"
 /* if_true_or_false */
#line 186 "../Main.m3"
 /* load_host_integer */
#line 186 "../Main.m3"
 /* load_integer */
#line 186 "../Main.m3"
 /* if_compare */
#line 186 "../Main.m3"
if(m3_eq(INT64,
  Main_m_214_L_215,
   INT64_(0)))goto L14;
#line 186 "../Main.m3"
 /* start_call_direct */
#line 186 "../Main.m3"
 /* load_address */
#line 186 "../Main.m3"
 /* pop_param */
#line 186 "../Main.m3"
 /* load_integer */
#line 186 "../Main.m3"
 /* pop_param */
#line 186 "../Main.m3"
 /* load_address */
#line 186 "../Main.m3"
 /* pop_param */
#line 186 "../Main.m3"
 /* call_direct */
#line 186 "../Main.m3"
RTHooks__AssertFailed(
  ( ADDRESS )(((ADDRESS)(&Main_m_M_Main_L_22)) ),
  ( INTEGER )(  INT64_(186) ),
  ( TEXT )(((ADDRESS)(INT64_(992)+((ADDRESS)(&Main_m_20_L_21)))) ));
#line 186 "../Main.m3"
 /* set_label */
#line 186 "../Main.m3"
L14:;
#line 186 "../Main.m3"
 /* set_source_line */
#line 186 "../Main.m3"
#line 187 "../Main.m3"
 /* start_call_direct */
#line 187 "../Main.m3"
 /* load_integer */
#line 187 "../Main.m3"
 /* pop_param */
#line 187 "../Main.m3"
 /* load_integer */
#line 187 "../Main.m3"
 /* pop_param */
#line 187 "../Main.m3"
 /* call_direct */
#line 187 "../Main.m3"
 /* store */
#line 187 "../Main.m3"
(*(INT64*)(&Main_m_214_L_215))=(INT64)(((INT64)(Main__no_overlap_less_GE_false(
  ( T8B2831D7_8 /*TypeText1*/  )(((UINT8)( INT64_(0))) ),
  ( T892833D7_8 /*TypeText1*/  )(((UINT8)( INT64_(2))) )))));
#line 187 "../Main.m3"
 /* load */
#line 187 "../Main.m3"
 /* if_true_or_false */
#line 187 "../Main.m3"
 /* load_host_integer */
#line 187 "../Main.m3"
 /* load_integer */
#line 187 "../Main.m3"
 /* if_compare */
#line 187 "../Main.m3"
if(m3_eq(INT64,
  Main_m_214_L_215,
   INT64_(0)))goto L15;
#line 187 "../Main.m3"
 /* start_call_direct */
#line 187 "../Main.m3"
 /* load_address */
#line 187 "../Main.m3"
 /* pop_param */
#line 187 "../Main.m3"
 /* load_integer */
#line 187 "../Main.m3"
 /* pop_param */
#line 187 "../Main.m3"
 /* load_address */
#line 187 "../Main.m3"
 /* pop_param */
#line 187 "../Main.m3"
 /* call_direct */
#line 187 "../Main.m3"
RTHooks__AssertFailed(
  ( ADDRESS )(((ADDRESS)(&Main_m_M_Main_L_22)) ),
  ( INTEGER )(  INT64_(187) ),
  ( TEXT )(((ADDRESS)(INT64_(1056)+((ADDRESS)(&Main_m_20_L_21)))) ));
#line 187 "../Main.m3"
 /* set_label */
#line 187 "../Main.m3"
L15:;
#line 187 "../Main.m3"
 /* set_source_line */
#line 187 "../Main.m3"
#line 188 "../Main.m3"
 /* start_call_direct */
#line 188 "../Main.m3"
 /* load_integer */
#line 188 "../Main.m3"
 /* pop_param */
#line 188 "../Main.m3"
 /* load_integer */
#line 188 "../Main.m3"
 /* pop_param */
#line 188 "../Main.m3"
 /* call_direct */
#line 188 "../Main.m3"
 /* store */
#line 188 "../Main.m3"
(*(INT64*)(&Main_m_214_L_215))=(INT64)(((INT64)(Main__no_overlap_less_EQ_false(
  ( T8B2831D7_8 /*TypeText1*/  )(((UINT8)( INT64_(0))) ),
  ( T892833D7_8 /*TypeText1*/  )(((UINT8)( INT64_(2))) )))));
#line 188 "../Main.m3"
 /* load */
#line 188 "../Main.m3"
 /* if_true_or_false */
#line 188 "../Main.m3"
 /* load_host_integer */
#line 188 "../Main.m3"
 /* load_integer */
#line 188 "../Main.m3"
 /* if_compare */
#line 188 "../Main.m3"
if(m3_eq(INT64,
  Main_m_214_L_215,
   INT64_(0)))goto L16;
#line 188 "../Main.m3"
 /* start_call_direct */
#line 188 "../Main.m3"
 /* load_address */
#line 188 "../Main.m3"
 /* pop_param */
#line 188 "../Main.m3"
 /* load_integer */
#line 188 "../Main.m3"
 /* pop_param */
#line 188 "../Main.m3"
 /* load_address */
#line 188 "../Main.m3"
 /* pop_param */
#line 188 "../Main.m3"
 /* call_direct */
#line 188 "../Main.m3"
RTHooks__AssertFailed(
  ( ADDRESS )(((ADDRESS)(&Main_m_M_Main_L_22)) ),
  ( INTEGER )(  INT64_(188) ),
  ( TEXT )(((ADDRESS)(INT64_(1120)+((ADDRESS)(&Main_m_20_L_21)))) ));
#line 188 "../Main.m3"
 /* set_label */
#line 188 "../Main.m3"
L16:;
#line 188 "../Main.m3"
 /* set_source_line */
#line 188 "../Main.m3"
#line 189 "../Main.m3"
 /* start_call_direct */
#line 189 "../Main.m3"
 /* load_integer */
#line 189 "../Main.m3"
 /* pop_param */
#line 189 "../Main.m3"
 /* load_integer */
#line 189 "../Main.m3"
 /* pop_param */
#line 189 "../Main.m3"
 /* call_direct */
#line 189 "../Main.m3"
 /* store */
#line 189 "../Main.m3"
(*(INT64*)(&Main_m_214_L_215))=(INT64)(((INT64)(Main__no_overlap_less_NE_true(
  ( T8B2831D7_8 /*TypeText1*/  )(((UINT8)( INT64_(0))) ),
  ( T892833D7_8 /*TypeText1*/  )(((UINT8)( INT64_(2))) )))));
#line 189 "../Main.m3"
 /* load */
#line 189 "../Main.m3"
 /* if_true_or_false */
#line 189 "../Main.m3"
 /* load_host_integer */
#line 189 "../Main.m3"
 /* load_integer */
#line 189 "../Main.m3"
 /* if_compare */
#line 189 "../Main.m3"
if(m3_ne(INT64,
  Main_m_214_L_215,
   INT64_(0)))goto L17;
#line 189 "../Main.m3"
 /* start_call_direct */
#line 189 "../Main.m3"
 /* load_address */
#line 189 "../Main.m3"
 /* pop_param */
#line 189 "../Main.m3"
 /* load_integer */
#line 189 "../Main.m3"
 /* pop_param */
#line 189 "../Main.m3"
 /* load_address */
#line 189 "../Main.m3"
 /* pop_param */
#line 189 "../Main.m3"
 /* call_direct */
#line 189 "../Main.m3"
RTHooks__AssertFailed(
  ( ADDRESS )(((ADDRESS)(&Main_m_M_Main_L_22)) ),
  ( INTEGER )(  INT64_(189) ),
  ( TEXT )(((ADDRESS)(INT64_(1184)+((ADDRESS)(&Main_m_20_L_21)))) ));
#line 189 "../Main.m3"
 /* set_label */
#line 189 "../Main.m3"
L17:;
#line 189 "../Main.m3"
 /* set_source_line */
#line 189 "../Main.m3"
#line 190 "../Main.m3"
 /* start_call_direct */
#line 190 "../Main.m3"
 /* load_integer */
#line 190 "../Main.m3"
 /* pop_param */
#line 190 "../Main.m3"
 /* load_integer */
#line 190 "../Main.m3"
 /* pop_param */
#line 190 "../Main.m3"
 /* call_direct */
#line 190 "../Main.m3"
 /* store */
#line 190 "../Main.m3"
(*(INT64*)(&Main_m_214_L_215))=(INT64)(((INT64)(Main__minimum_overlap_less_LE_true(
  ( T8B2831D7_8 /*TypeText1*/  )(((UINT8)( INT64_(0))) ),
  ( T882830D7_8 /*TypeText1*/  )(((UINT8)( INT64_(1))) )))));
#line 190 "../Main.m3"
 /* load */
#line 190 "../Main.m3"
 /* if_true_or_false */
#line 190 "../Main.m3"
 /* load_host_integer */
#line 190 "../Main.m3"
 /* load_integer */
#line 190 "../Main.m3"
 /* if_compare */
#line 190 "../Main.m3"
if(m3_ne(INT64,
  Main_m_214_L_215,
   INT64_(0)))goto L18;
#line 190 "../Main.m3"
 /* start_call_direct */
#line 190 "../Main.m3"
 /* load_address */
#line 190 "../Main.m3"
 /* pop_param */
#line 190 "../Main.m3"
 /* load_integer */
#line 190 "../Main.m3"
 /* pop_param */
#line 190 "../Main.m3"
 /* load_address */
#line 190 "../Main.m3"
 /* pop_param */
#line 190 "../Main.m3"
 /* call_direct */
#line 190 "../Main.m3"
RTHooks__AssertFailed(
  ( ADDRESS )(((ADDRESS)(&Main_m_M_Main_L_22)) ),
  ( INTEGER )(  INT64_(190) ),
  ( TEXT )(((ADDRESS)(INT64_(1240)+((ADDRESS)(&Main_m_20_L_21)))) ));
#line 190 "../Main.m3"
 /* set_label */
#line 190 "../Main.m3"
L18:;
#line 190 "../Main.m3"
 /* set_source_line */
#line 190 "../Main.m3"
#line 191 "../Main.m3"
 /* start_call_direct */
#line 191 "../Main.m3"
 /* load_integer */
#line 191 "../Main.m3"
 /* pop_param */
#line 191 "../Main.m3"
 /* load_integer */
#line 191 "../Main.m3"
 /* pop_param */
#line 191 "../Main.m3"
 /* call_direct */
#line 191 "../Main.m3"
 /* store */
#line 191 "../Main.m3"
(*(INT64*)(&Main_m_214_L_215))=(INT64)(((INT64)(Main__minimum_overlap_less_GT_false(
  ( T8B2831D7_8 /*TypeText1*/  )(((UINT8)( INT64_(0))) ),
  ( T882830D7_8 /*TypeText1*/  )(((UINT8)( INT64_(1))) )))));
#line 191 "../Main.m3"
 /* load */
#line 191 "../Main.m3"
 /* if_true_or_false */
#line 191 "../Main.m3"
 /* load_host_integer */
#line 191 "../Main.m3"
 /* load_integer */
#line 191 "../Main.m3"
 /* if_compare */
#line 191 "../Main.m3"
if(m3_eq(INT64,
  Main_m_214_L_215,
   INT64_(0)))goto L19;
#line 191 "../Main.m3"
 /* start_call_direct */
#line 191 "../Main.m3"
 /* load_address */
#line 191 "../Main.m3"
 /* pop_param */
#line 191 "../Main.m3"
 /* load_integer */
#line 191 "../Main.m3"
 /* pop_param */
#line 191 "../Main.m3"
 /* load_address */
#line 191 "../Main.m3"
 /* pop_param */
#line 191 "../Main.m3"
 /* call_direct */
#line 191 "../Main.m3"
RTHooks__AssertFailed(
  ( ADDRESS )(((ADDRESS)(&Main_m_M_Main_L_22)) ),
  ( INTEGER )(  INT64_(191) ),
  ( TEXT )(((ADDRESS)(INT64_(1304)+((ADDRESS)(&Main_m_20_L_21)))) ));
#line 191 "../Main.m3"
 /* set_label */
#line 191 "../Main.m3"
L19:;
#line 191 "../Main.m3"
 /* set_source_line */
#line 191 "../Main.m3"
#line 192 "../Main.m3"
 /* start_call_direct */
#line 192 "../Main.m3"
 /* load_integer */
#line 192 "../Main.m3"
 /* pop_param */
#line 192 "../Main.m3"
 /* load_integer */
#line 192 "../Main.m3"
 /* pop_param */
#line 192 "../Main.m3"
 /* call_direct */
#line 192 "../Main.m3"
 /* store */
#line 192 "../Main.m3"
(*(INT64*)(&Main_m_214_L_215))=(INT64)(((INT64)(Main__no_overlap_greater_LT_false(
  ( T892833D7_8 /*TypeText1*/  )(((UINT8)( INT64_(2))) ),
  ( T8B2831D7_8 /*TypeText1*/  )(((UINT8)( INT64_(0))) )))));
#line 192 "../Main.m3"
 /* load */
#line 192 "../Main.m3"
 /* if_true_or_false */
#line 192 "../Main.m3"
 /* load_host_integer */
#line 192 "../Main.m3"
 /* load_integer */
#line 192 "../Main.m3"
 /* if_compare */
#line 192 "../Main.m3"
if(m3_eq(INT64,
  Main_m_214_L_215,
   INT64_(0)))goto L1A;
#line 192 "../Main.m3"
 /* start_call_direct */
#line 192 "../Main.m3"
 /* load_address */
#line 192 "../Main.m3"
 /* pop_param */
#line 192 "../Main.m3"
 /* load_integer */
#line 192 "../Main.m3"
 /* pop_param */
#line 192 "../Main.m3"
 /* load_address */
#line 192 "../Main.m3"
 /* pop_param */
#line 192 "../Main.m3"
 /* call_direct */
#line 192 "../Main.m3"
RTHooks__AssertFailed(
  ( ADDRESS )(((ADDRESS)(&Main_m_M_Main_L_22)) ),
  ( INTEGER )(  INT64_(192) ),
  ( TEXT )(((ADDRESS)(INT64_(1368)+((ADDRESS)(&Main_m_20_L_21)))) ));
#line 192 "../Main.m3"
 /* set_label */
#line 192 "../Main.m3"
L1A:;
#line 192 "../Main.m3"
 /* set_source_line */
#line 192 "../Main.m3"
#line 193 "../Main.m3"
 /* start_call_direct */
#line 193 "../Main.m3"
 /* load_integer */
#line 193 "../Main.m3"
 /* pop_param */
#line 193 "../Main.m3"
 /* load_integer */
#line 193 "../Main.m3"
 /* pop_param */
#line 193 "../Main.m3"
 /* call_direct */
#line 193 "../Main.m3"
 /* store */
#line 193 "../Main.m3"
(*(INT64*)(&Main_m_214_L_215))=(INT64)(((INT64)(Main__no_overlap_greater_LE_false(
  ( T892833D7_8 /*TypeText1*/  )(((UINT8)( INT64_(2))) ),
  ( T8B2831D7_8 /*TypeText1*/  )(((UINT8)( INT64_(0))) )))));
#line 193 "../Main.m3"
 /* load */
#line 193 "../Main.m3"
 /* if_true_or_false */
#line 193 "../Main.m3"
 /* load_host_integer */
#line 193 "../Main.m3"
 /* load_integer */
#line 193 "../Main.m3"
 /* if_compare */
#line 193 "../Main.m3"
if(m3_eq(INT64,
  Main_m_214_L_215,
   INT64_(0)))goto L1B;
#line 193 "../Main.m3"
 /* start_call_direct */
#line 193 "../Main.m3"
 /* load_address */
#line 193 "../Main.m3"
 /* pop_param */
#line 193 "../Main.m3"
 /* load_integer */
#line 193 "../Main.m3"
 /* pop_param */
#line 193 "../Main.m3"
 /* load_address */
#line 193 "../Main.m3"
 /* pop_param */
#line 193 "../Main.m3"
 /* call_direct */
#line 193 "../Main.m3"
RTHooks__AssertFailed(
  ( ADDRESS )(((ADDRESS)(&Main_m_M_Main_L_22)) ),
  ( INTEGER )(  INT64_(193) ),
  ( TEXT )(((ADDRESS)(INT64_(1432)+((ADDRESS)(&Main_m_20_L_21)))) ));
#line 193 "../Main.m3"
 /* set_label */
#line 193 "../Main.m3"
L1B:;
#line 193 "../Main.m3"
 /* set_source_line */
#line 193 "../Main.m3"
#line 194 "../Main.m3"
 /* start_call_direct */
#line 194 "../Main.m3"
 /* load_integer */
#line 194 "../Main.m3"
 /* pop_param */
#line 194 "../Main.m3"
 /* load_integer */
#line 194 "../Main.m3"
 /* pop_param */
#line 194 "../Main.m3"
 /* call_direct */
#line 194 "../Main.m3"
 /* store */
#line 194 "../Main.m3"
(*(INT64*)(&Main_m_214_L_215))=(INT64)(((INT64)(Main__no_overlap_greater_GT_true(
  ( T892833D7_8 /*TypeText1*/  )(((UINT8)( INT64_(2))) ),
  ( T8B2831D7_8 /*TypeText1*/  )(((UINT8)( INT64_(0))) )))));
#line 194 "../Main.m3"
 /* load */
#line 194 "../Main.m3"
 /* if_true_or_false */
#line 194 "../Main.m3"
 /* load_host_integer */
#line 194 "../Main.m3"
 /* load_integer */
#line 194 "../Main.m3"
 /* if_compare */
#line 194 "../Main.m3"
if(m3_ne(INT64,
  Main_m_214_L_215,
   INT64_(0)))goto L1C;
#line 194 "../Main.m3"
 /* start_call_direct */
#line 194 "../Main.m3"
 /* load_address */
#line 194 "../Main.m3"
 /* pop_param */
#line 194 "../Main.m3"
 /* load_integer */
#line 194 "../Main.m3"
 /* pop_param */
#line 194 "../Main.m3"
 /* load_address */
#line 194 "../Main.m3"
 /* pop_param */
#line 194 "../Main.m3"
 /* call_direct */
#line 194 "../Main.m3"
RTHooks__AssertFailed(
  ( ADDRESS )(((ADDRESS)(&Main_m_M_Main_L_22)) ),
  ( INTEGER )(  INT64_(194) ),
  ( TEXT )(((ADDRESS)(INT64_(1496)+((ADDRESS)(&Main_m_20_L_21)))) ));
#line 194 "../Main.m3"
 /* set_label */
#line 194 "../Main.m3"
L1C:;
#line 194 "../Main.m3"
 /* set_source_line */
#line 194 "../Main.m3"
#line 195 "../Main.m3"
 /* start_call_direct */
#line 195 "../Main.m3"
 /* load_integer */
#line 195 "../Main.m3"
 /* pop_param */
#line 195 "../Main.m3"
 /* load_integer */
#line 195 "../Main.m3"
 /* pop_param */
#line 195 "../Main.m3"
 /* call_direct */
#line 195 "../Main.m3"
 /* store */
#line 195 "../Main.m3"
(*(INT64*)(&Main_m_214_L_215))=(INT64)(((INT64)(Main__no_overlap_greater_GE_true(
  ( T892833D7_8 /*TypeText1*/  )(((UINT8)( INT64_(2))) ),
  ( T8B2831D7_8 /*TypeText1*/  )(((UINT8)( INT64_(0))) )))));
#line 195 "../Main.m3"
 /* load */
#line 195 "../Main.m3"
 /* if_true_or_false */
#line 195 "../Main.m3"
 /* load_host_integer */
#line 195 "../Main.m3"
 /* load_integer */
#line 195 "../Main.m3"
 /* if_compare */
#line 195 "../Main.m3"
if(m3_ne(INT64,
  Main_m_214_L_215,
   INT64_(0)))goto L1D;
#line 195 "../Main.m3"
 /* start_call_direct */
#line 195 "../Main.m3"
 /* load_address */
#line 195 "../Main.m3"
 /* pop_param */
#line 195 "../Main.m3"
 /* load_integer */
#line 195 "../Main.m3"
 /* pop_param */
#line 195 "../Main.m3"
 /* load_address */
#line 195 "../Main.m3"
 /* pop_param */
#line 195 "../Main.m3"
 /* call_direct */
#line 195 "../Main.m3"
RTHooks__AssertFailed(
  ( ADDRESS )(((ADDRESS)(&Main_m_M_Main_L_22)) ),
  ( INTEGER )(  INT64_(195) ),
  ( TEXT )(((ADDRESS)(INT64_(1560)+((ADDRESS)(&Main_m_20_L_21)))) ));
#line 195 "../Main.m3"
 /* set_label */
#line 195 "../Main.m3"
L1D:;
#line 195 "../Main.m3"
 /* set_source_line */
#line 195 "../Main.m3"
#line 196 "../Main.m3"
 /* start_call_direct */
#line 196 "../Main.m3"
 /* load_integer */
#line 196 "../Main.m3"
 /* pop_param */
#line 196 "../Main.m3"
 /* load_integer */
#line 196 "../Main.m3"
 /* pop_param */
#line 196 "../Main.m3"
 /* call_direct */
#line 196 "../Main.m3"
 /* store */
#line 196 "../Main.m3"
(*(INT64*)(&Main_m_214_L_215))=(INT64)(((INT64)(Main__no_overlap_greater_EQ_false(
  ( T892833D7_8 /*TypeText1*/  )(((UINT8)( INT64_(2))) ),
  ( T8B2831D7_8 /*TypeText1*/  )(((UINT8)( INT64_(0))) )))));
#line 196 "../Main.m3"
 /* load */
#line 196 "../Main.m3"
 /* if_true_or_false */
#line 196 "../Main.m3"
 /* load_host_integer */
#line 196 "../Main.m3"
 /* load_integer */
#line 196 "../Main.m3"
 /* if_compare */
#line 196 "../Main.m3"
if(m3_eq(INT64,
  Main_m_214_L_215,
   INT64_(0)))goto L1E;
#line 196 "../Main.m3"
 /* start_call_direct */
#line 196 "../Main.m3"
 /* load_address */
#line 196 "../Main.m3"
 /* pop_param */
#line 196 "../Main.m3"
 /* load_integer */
#line 196 "../Main.m3"
 /* pop_param */
#line 196 "../Main.m3"
 /* load_address */
#line 196 "../Main.m3"
 /* pop_param */
#line 196 "../Main.m3"
 /* call_direct */
#line 196 "../Main.m3"
RTHooks__AssertFailed(
  ( ADDRESS )(((ADDRESS)(&Main_m_M_Main_L_22)) ),
  ( INTEGER )(  INT64_(196) ),
  ( TEXT )(((ADDRESS)(INT64_(1624)+((ADDRESS)(&Main_m_20_L_21)))) ));
#line 196 "../Main.m3"
 /* set_label */
#line 196 "../Main.m3"
L1E:;
#line 196 "../Main.m3"
 /* set_source_line */
#line 196 "../Main.m3"
#line 197 "../Main.m3"
 /* start_call_direct */
#line 197 "../Main.m3"
 /* load_integer */
#line 197 "../Main.m3"
 /* pop_param */
#line 197 "../Main.m3"
 /* load_integer */
#line 197 "../Main.m3"
 /* pop_param */
#line 197 "../Main.m3"
 /* call_direct */
#line 197 "../Main.m3"
 /* store */
#line 197 "../Main.m3"
(*(INT64*)(&Main_m_214_L_215))=(INT64)(((INT64)(Main__no_overlap_greater_NE_true(
  ( T892833D7_8 /*TypeText1*/  )(((UINT8)( INT64_(2))) ),
  ( T8B2831D7_8 /*TypeText1*/  )(((UINT8)( INT64_(0))) )))));
#line 197 "../Main.m3"
 /* load */
#line 197 "../Main.m3"
 /* if_true_or_false */
#line 197 "../Main.m3"
 /* load_host_integer */
#line 197 "../Main.m3"
 /* load_integer */
#line 197 "../Main.m3"
 /* if_compare */
#line 197 "../Main.m3"
if(m3_ne(INT64,
  Main_m_214_L_215,
   INT64_(0)))goto L1F;
#line 197 "../Main.m3"
 /* start_call_direct */
#line 197 "../Main.m3"
 /* load_address */
#line 197 "../Main.m3"
 /* pop_param */
#line 197 "../Main.m3"
 /* load_integer */
#line 197 "../Main.m3"
 /* pop_param */
#line 197 "../Main.m3"
 /* load_address */
#line 197 "../Main.m3"
 /* pop_param */
#line 197 "../Main.m3"
 /* call_direct */
#line 197 "../Main.m3"
RTHooks__AssertFailed(
  ( ADDRESS )(((ADDRESS)(&Main_m_M_Main_L_22)) ),
  ( INTEGER )(  INT64_(197) ),
  ( TEXT )(((ADDRESS)(INT64_(1688)+((ADDRESS)(&Main_m_20_L_21)))) ));
#line 197 "../Main.m3"
 /* set_label */
#line 197 "../Main.m3"
L1F:;
#line 197 "../Main.m3"
 /* set_source_line */
#line 197 "../Main.m3"
#line 198 "../Main.m3"
 /* start_call_direct */
#line 198 "../Main.m3"
 /* load_integer */
#line 198 "../Main.m3"
 /* pop_param */
#line 198 "../Main.m3"
 /* load_integer */
#line 198 "../Main.m3"
 /* pop_param */
#line 198 "../Main.m3"
 /* call_direct */
#line 198 "../Main.m3"
 /* store */
#line 198 "../Main.m3"
(*(INT64*)(&Main_m_214_L_215))=(INT64)(((INT64)(Main__minimum_overlap_greater_LT_false(
  ( T882830D7_8 /*TypeText1*/  )(((UINT8)( INT64_(1))) ),
  ( T8B2831D7_8 /*TypeText1*/  )(((UINT8)( INT64_(0))) )))));
#line 198 "../Main.m3"
 /* load */
#line 198 "../Main.m3"
 /* if_true_or_false */
#line 198 "../Main.m3"
 /* load_host_integer */
#line 198 "../Main.m3"
 /* load_integer */
#line 198 "../Main.m3"
 /* if_compare */
#line 198 "../Main.m3"
if(m3_eq(INT64,
  Main_m_214_L_215,
   INT64_(0)))goto L20;
#line 198 "../Main.m3"
 /* start_call_direct */
#line 198 "../Main.m3"
 /* load_address */
#line 198 "../Main.m3"
 /* pop_param */
#line 198 "../Main.m3"
 /* load_integer */
#line 198 "../Main.m3"
 /* pop_param */
#line 198 "../Main.m3"
 /* load_address */
#line 198 "../Main.m3"
 /* pop_param */
#line 198 "../Main.m3"
 /* call_direct */
#line 198 "../Main.m3"
RTHooks__AssertFailed(
  ( ADDRESS )(((ADDRESS)(&Main_m_M_Main_L_22)) ),
  ( INTEGER )(  INT64_(198) ),
  ( TEXT )(((ADDRESS)(INT64_(1752)+((ADDRESS)(&Main_m_20_L_21)))) ));
#line 198 "../Main.m3"
 /* set_label */
#line 198 "../Main.m3"
L20:;
#line 198 "../Main.m3"
 /* set_source_line */
#line 198 "../Main.m3"
#line 199 "../Main.m3"
 /* start_call_direct */
#line 199 "../Main.m3"
 /* load_integer */
#line 199 "../Main.m3"
 /* pop_param */
#line 199 "../Main.m3"
 /* load_integer */
#line 199 "../Main.m3"
 /* pop_param */
#line 199 "../Main.m3"
 /* call_direct */
#line 199 "../Main.m3"
 /* store */
#line 199 "../Main.m3"
(*(INT64*)(&Main_m_214_L_215))=(INT64)(((INT64)(Main__minimum_overlap_greater_GE_true(
  ( T882830D7_8 /*TypeText1*/  )(((UINT8)( INT64_(1))) ),
  ( T8B2831D7_8 /*TypeText1*/  )(((UINT8)( INT64_(0))) )))));
#line 199 "../Main.m3"
 /* load */
#line 199 "../Main.m3"
 /* if_true_or_false */
#line 199 "../Main.m3"
 /* load_host_integer */
#line 199 "../Main.m3"
 /* load_integer */
#line 199 "../Main.m3"
 /* if_compare */
#line 199 "../Main.m3"
if(m3_ne(INT64,
  Main_m_214_L_215,
   INT64_(0)))goto L21;
#line 199 "../Main.m3"
 /* start_call_direct */
#line 199 "../Main.m3"
 /* load_address */
#line 199 "../Main.m3"
 /* pop_param */
#line 199 "../Main.m3"
 /* load_integer */
#line 199 "../Main.m3"
 /* pop_param */
#line 199 "../Main.m3"
 /* load_address */
#line 199 "../Main.m3"
 /* pop_param */
#line 199 "../Main.m3"
 /* call_direct */
#line 199 "../Main.m3"
RTHooks__AssertFailed(
  ( ADDRESS )(((ADDRESS)(&Main_m_M_Main_L_22)) ),
  ( INTEGER )(  INT64_(199) ),
  ( TEXT )(((ADDRESS)(INT64_(1824)+((ADDRESS)(&Main_m_20_L_21)))) ));
#line 199 "../Main.m3"
 /* set_label */
#line 199 "../Main.m3"
L21:;
#line 199 "../Main.m3"
 /* set_source_line */
#line 199 "../Main.m3"
#line 200 "../Main.m3"
 /* start_call_direct */
#line 200 "../Main.m3"
 /* load_integer */
#line 200 "../Main.m3"
 /* pop_param */
#line 200 "../Main.m3"
 /* load_integer */
#line 200 "../Main.m3"
 /* pop_param */
#line 200 "../Main.m3"
 /* call_direct */
#line 200 "../Main.m3"
 /* store */
#line 200 "../Main.m3"
(*(INT64*)(&Main_m_214_L_215))=(INT64)(((INT64)(Main__no_overlap_less_enum_LT_true(
  ( Main__LowNumber )(((UINT8)( INT64_(0))) ),
  ( Main__HighNumber )(((UINT8)( INT64_(4))) )))));
#line 200 "../Main.m3"
 /* load */
#line 200 "../Main.m3"
 /* if_true_or_false */
#line 200 "../Main.m3"
 /* load_host_integer */
#line 200 "../Main.m3"
 /* load_integer */
#line 200 "../Main.m3"
 /* if_compare */
#line 200 "../Main.m3"
if(m3_ne(INT64,
  Main_m_214_L_215,
   INT64_(0)))goto L22;
#line 200 "../Main.m3"
 /* start_call_direct */
#line 200 "../Main.m3"
 /* load_address */
#line 200 "../Main.m3"
 /* pop_param */
#line 200 "../Main.m3"
 /* load_integer */
#line 200 "../Main.m3"
 /* pop_param */
#line 200 "../Main.m3"
 /* load_address */
#line 200 "../Main.m3"
 /* pop_param */
#line 200 "../Main.m3"
 /* call_direct */
#line 200 "../Main.m3"
RTHooks__AssertFailed(
  ( ADDRESS )(((ADDRESS)(&Main_m_M_Main_L_22)) ),
  ( INTEGER )(  INT64_(200) ),
  ( TEXT )(((ADDRESS)(INT64_(1888)+((ADDRESS)(&Main_m_20_L_21)))) ));
#line 200 "../Main.m3"
 /* set_label */
#line 200 "../Main.m3"
L22:;
#line 200 "../Main.m3"
 /* set_source_line */
#line 200 "../Main.m3"
#line 201 "../Main.m3"
 /* start_call_direct */
#line 201 "../Main.m3"
 /* load_integer */
#line 201 "../Main.m3"
 /* pop_param */
#line 201 "../Main.m3"
 /* load_integer */
#line 201 "../Main.m3"
 /* pop_param */
#line 201 "../Main.m3"
 /* call_direct */
#line 201 "../Main.m3"
 /* store */
#line 201 "../Main.m3"
(*(INT64*)(&Main_m_214_L_215))=(INT64)(((INT64)(Main__no_overlap_less_enum_LE_true(
  ( Main__LowNumber )(((UINT8)( INT64_(0))) ),
  ( Main__HighNumber )(((UINT8)( INT64_(4))) )))));
#line 201 "../Main.m3"
 /* load */
#line 201 "../Main.m3"
 /* if_true_or_false */
#line 201 "../Main.m3"
 /* load_host_integer */
#line 201 "../Main.m3"
 /* load_integer */
#line 201 "../Main.m3"
 /* if_compare */
#line 201 "../Main.m3"
if(m3_ne(INT64,
  Main_m_214_L_215,
   INT64_(0)))goto L23;
#line 201 "../Main.m3"
 /* start_call_direct */
#line 201 "../Main.m3"
 /* load_address */
#line 201 "../Main.m3"
 /* pop_param */
#line 201 "../Main.m3"
 /* load_integer */
#line 201 "../Main.m3"
 /* pop_param */
#line 201 "../Main.m3"
 /* load_address */
#line 201 "../Main.m3"
 /* pop_param */
#line 201 "../Main.m3"
 /* call_direct */
#line 201 "../Main.m3"
RTHooks__AssertFailed(
  ( ADDRESS )(((ADDRESS)(&Main_m_M_Main_L_22)) ),
  ( INTEGER )(  INT64_(201) ),
  ( TEXT )(((ADDRESS)(INT64_(1968)+((ADDRESS)(&Main_m_20_L_21)))) ));
#line 201 "../Main.m3"
 /* set_label */
#line 201 "../Main.m3"
L23:;
#line 201 "../Main.m3"
 /* set_source_line */
#line 201 "../Main.m3"
#line 202 "../Main.m3"
 /* start_call_direct */
#line 202 "../Main.m3"
 /* load_integer */
#line 202 "../Main.m3"
 /* pop_param */
#line 202 "../Main.m3"
 /* load_integer */
#line 202 "../Main.m3"
 /* pop_param */
#line 202 "../Main.m3"
 /* call_direct */
#line 202 "../Main.m3"
 /* store */
#line 202 "../Main.m3"
(*(INT64*)(&Main_m_214_L_215))=(INT64)(((INT64)(Main__no_overlap_less_enum_GT_false(
  ( Main__LowNumber )(((UINT8)( INT64_(0))) ),
  ( Main__HighNumber )(((UINT8)( INT64_(4))) )))));
#line 202 "../Main.m3"
 /* load */
#line 202 "../Main.m3"
 /* if_true_or_false */
#line 202 "../Main.m3"
 /* load_host_integer */
#line 202 "../Main.m3"
 /* load_integer */
#line 202 "../Main.m3"
 /* if_compare */
#line 202 "../Main.m3"
if(m3_eq(INT64,
  Main_m_214_L_215,
   INT64_(0)))goto L24;
#line 202 "../Main.m3"
 /* start_call_direct */
#line 202 "../Main.m3"
 /* load_address */
#line 202 "../Main.m3"
 /* pop_param */
#line 202 "../Main.m3"
 /* load_integer */
#line 202 "../Main.m3"
 /* pop_param */
#line 202 "../Main.m3"
 /* load_address */
#line 202 "../Main.m3"
 /* pop_param */
#line 202 "../Main.m3"
 /* call_direct */
#line 202 "../Main.m3"
RTHooks__AssertFailed(
  ( ADDRESS )(((ADDRESS)(&Main_m_M_Main_L_22)) ),
  ( INTEGER )(  INT64_(202) ),
  ( TEXT )(((ADDRESS)(INT64_(2048)+((ADDRESS)(&Main_m_20_L_21)))) ));
#line 202 "../Main.m3"
 /* set_label */
#line 202 "../Main.m3"
L24:;
#line 202 "../Main.m3"
 /* set_source_line */
#line 202 "../Main.m3"
#line 203 "../Main.m3"
 /* start_call_direct */
#line 203 "../Main.m3"
 /* load_integer */
#line 203 "../Main.m3"
 /* pop_param */
#line 203 "../Main.m3"
 /* load_integer */
#line 203 "../Main.m3"
 /* pop_param */
#line 203 "../Main.m3"
 /* call_direct */
#line 203 "../Main.m3"
 /* store */
#line 203 "../Main.m3"
(*(INT64*)(&Main_m_214_L_215))=(INT64)(((INT64)(Main__no_overlap_less_enum_GE_false(
  ( Main__LowNumber )(((UINT8)( INT64_(0))) ),
  ( Main__HighNumber )(((UINT8)( INT64_(4))) )))));
#line 203 "../Main.m3"
 /* load */
#line 203 "../Main.m3"
 /* if_true_or_false */
#line 203 "../Main.m3"
 /* load_host_integer */
#line 203 "../Main.m3"
 /* load_integer */
#line 203 "../Main.m3"
 /* if_compare */
#line 203 "../Main.m3"
if(m3_eq(INT64,
  Main_m_214_L_215,
   INT64_(0)))goto L25;
#line 203 "../Main.m3"
 /* start_call_direct */
#line 203 "../Main.m3"
 /* load_address */
#line 203 "../Main.m3"
 /* pop_param */
#line 203 "../Main.m3"
 /* load_integer */
#line 203 "../Main.m3"
 /* pop_param */
#line 203 "../Main.m3"
 /* load_address */
#line 203 "../Main.m3"
 /* pop_param */
#line 203 "../Main.m3"
 /* call_direct */
#line 203 "../Main.m3"
RTHooks__AssertFailed(
  ( ADDRESS )(((ADDRESS)(&Main_m_M_Main_L_22)) ),
  ( INTEGER )(  INT64_(203) ),
  ( TEXT )(((ADDRESS)(INT64_(2136)+((ADDRESS)(&Main_m_20_L_21)))) ));
#line 203 "../Main.m3"
 /* set_label */
#line 203 "../Main.m3"
L25:;
#line 203 "../Main.m3"
 /* set_source_line */
#line 203 "../Main.m3"
#line 204 "../Main.m3"
 /* start_call_direct */
#line 204 "../Main.m3"
 /* load_integer */
#line 204 "../Main.m3"
 /* pop_param */
#line 204 "../Main.m3"
 /* load_integer */
#line 204 "../Main.m3"
 /* pop_param */
#line 204 "../Main.m3"
 /* call_direct */
#line 204 "../Main.m3"
 /* store */
#line 204 "../Main.m3"
(*(INT64*)(&Main_m_214_L_215))=(INT64)(((INT64)(Main__no_overlap_less_enum_EQ_false(
  ( Main__LowNumber )(((UINT8)( INT64_(0))) ),
  ( Main__HighNumber )(((UINT8)( INT64_(4))) )))));
#line 204 "../Main.m3"
 /* load */
#line 204 "../Main.m3"
 /* if_true_or_false */
#line 204 "../Main.m3"
 /* load_host_integer */
#line 204 "../Main.m3"
 /* load_integer */
#line 204 "../Main.m3"
 /* if_compare */
#line 204 "../Main.m3"
if(m3_eq(INT64,
  Main_m_214_L_215,
   INT64_(0)))goto L26;
#line 204 "../Main.m3"
 /* start_call_direct */
#line 204 "../Main.m3"
 /* load_address */
#line 204 "../Main.m3"
 /* pop_param */
#line 204 "../Main.m3"
 /* load_integer */
#line 204 "../Main.m3"
 /* pop_param */
#line 204 "../Main.m3"
 /* load_address */
#line 204 "../Main.m3"
 /* pop_param */
#line 204 "../Main.m3"
 /* call_direct */
#line 204 "../Main.m3"
RTHooks__AssertFailed(
  ( ADDRESS )(((ADDRESS)(&Main_m_M_Main_L_22)) ),
  ( INTEGER )(  INT64_(204) ),
  ( TEXT )(((ADDRESS)(INT64_(2224)+((ADDRESS)(&Main_m_20_L_21)))) ));
#line 204 "../Main.m3"
 /* set_label */
#line 204 "../Main.m3"
L26:;
#line 204 "../Main.m3"
 /* set_source_line */
#line 204 "../Main.m3"
#line 205 "../Main.m3"
 /* start_call_direct */
#line 205 "../Main.m3"
 /* load_integer */
#line 205 "../Main.m3"
 /* pop_param */
#line 205 "../Main.m3"
 /* load_integer */
#line 205 "../Main.m3"
 /* pop_param */
#line 205 "../Main.m3"
 /* call_direct */
#line 205 "../Main.m3"
 /* store */
#line 205 "../Main.m3"
(*(INT64*)(&Main_m_214_L_215))=(INT64)(((INT64)(Main__no_overlap_less_enum_NE_true(
  ( Main__LowNumber )(((UINT8)( INT64_(0))) ),
  ( Main__HighNumber )(((UINT8)( INT64_(4))) )))));
#line 205 "../Main.m3"
 /* load */
#line 205 "../Main.m3"
 /* if_true_or_false */
#line 205 "../Main.m3"
 /* load_host_integer */
#line 205 "../Main.m3"
 /* load_integer */
#line 205 "../Main.m3"
 /* if_compare */
#line 205 "../Main.m3"
if(m3_ne(INT64,
  Main_m_214_L_215,
   INT64_(0)))goto L27;
#line 205 "../Main.m3"
 /* start_call_direct */
#line 205 "../Main.m3"
 /* load_address */
#line 205 "../Main.m3"
 /* pop_param */
#line 205 "../Main.m3"
 /* load_integer */
#line 205 "../Main.m3"
 /* pop_param */
#line 205 "../Main.m3"
 /* load_address */
#line 205 "../Main.m3"
 /* pop_param */
#line 205 "../Main.m3"
 /* call_direct */
#line 205 "../Main.m3"
RTHooks__AssertFailed(
  ( ADDRESS )(((ADDRESS)(&Main_m_M_Main_L_22)) ),
  ( INTEGER )(  INT64_(205) ),
  ( TEXT )(((ADDRESS)(INT64_(2312)+((ADDRESS)(&Main_m_20_L_21)))) ));
#line 205 "../Main.m3"
 /* set_label */
#line 205 "../Main.m3"
L27:;
#line 205 "../Main.m3"
 /* set_source_line */
#line 205 "../Main.m3"
#line 206 "../Main.m3"
 /* start_call_direct */
#line 206 "../Main.m3"
 /* load_integer */
#line 206 "../Main.m3"
 /* pop_param */
#line 206 "../Main.m3"
 /* load_integer */
#line 206 "../Main.m3"
 /* pop_param */
#line 206 "../Main.m3"
 /* call_direct */
#line 206 "../Main.m3"
 /* store */
#line 206 "../Main.m3"
(*(INT64*)(&Main_m_214_L_215))=(INT64)(((INT64)(Main__minimum_overlap_less_enum_LE_true(
  ( Main__LowNumber )(((UINT8)( INT64_(0))) ),
  ( Main__MiddleNumber )(((UINT8)( INT64_(1))) )))));
#line 206 "../Main.m3"
 /* load */
#line 206 "../Main.m3"
 /* if_true_or_false */
#line 206 "../Main.m3"
 /* load_host_integer */
#line 206 "../Main.m3"
 /* load_integer */
#line 206 "../Main.m3"
 /* if_compare */
#line 206 "../Main.m3"
if(m3_ne(INT64,
  Main_m_214_L_215,
   INT64_(0)))goto L28;
#line 206 "../Main.m3"
 /* start_call_direct */
#line 206 "../Main.m3"
 /* load_address */
#line 206 "../Main.m3"
 /* pop_param */
#line 206 "../Main.m3"
 /* load_integer */
#line 206 "../Main.m3"
 /* pop_param */
#line 206 "../Main.m3"
 /* load_address */
#line 206 "../Main.m3"
 /* pop_param */
#line 206 "../Main.m3"
 /* call_direct */
#line 206 "../Main.m3"
RTHooks__AssertFailed(
  ( ADDRESS )(((ADDRESS)(&Main_m_M_Main_L_22)) ),
  ( INTEGER )(  INT64_(206) ),
  ( TEXT )(((ADDRESS)(INT64_(2392)+((ADDRESS)(&Main_m_20_L_21)))) ));
#line 206 "../Main.m3"
 /* set_label */
#line 206 "../Main.m3"
L28:;
#line 206 "../Main.m3"
 /* set_source_line */
#line 206 "../Main.m3"
#line 207 "../Main.m3"
 /* start_call_direct */
#line 207 "../Main.m3"
 /* load_integer */
#line 207 "../Main.m3"
 /* pop_param */
#line 207 "../Main.m3"
 /* load_integer */
#line 207 "../Main.m3"
 /* pop_param */
#line 207 "../Main.m3"
 /* call_direct */
#line 207 "../Main.m3"
 /* store */
#line 207 "../Main.m3"
(*(INT64*)(&Main_m_214_L_215))=(INT64)(((INT64)(Main__minimum_overlap_less_enum_GT_false(
  ( Main__LowNumber )(((UINT8)( INT64_(0))) ),
  ( Main__MiddleNumber )(((UINT8)( INT64_(1))) )))));
#line 207 "../Main.m3"
 /* load */
#line 207 "../Main.m3"
 /* if_true_or_false */
#line 207 "../Main.m3"
 /* load_host_integer */
#line 207 "../Main.m3"
 /* load_integer */
#line 207 "../Main.m3"
 /* if_compare */
#line 207 "../Main.m3"
if(m3_eq(INT64,
  Main_m_214_L_215,
   INT64_(0)))goto L29;
#line 207 "../Main.m3"
 /* start_call_direct */
#line 207 "../Main.m3"
 /* load_address */
#line 207 "../Main.m3"
 /* pop_param */
#line 207 "../Main.m3"
 /* load_integer */
#line 207 "../Main.m3"
 /* pop_param */
#line 207 "../Main.m3"
 /* load_address */
#line 207 "../Main.m3"
 /* pop_param */
#line 207 "../Main.m3"
 /* call_direct */
#line 207 "../Main.m3"
RTHooks__AssertFailed(
  ( ADDRESS )(((ADDRESS)(&Main_m_M_Main_L_22)) ),
  ( INTEGER )(  INT64_(207) ),
  ( TEXT )(((ADDRESS)(INT64_(2480)+((ADDRESS)(&Main_m_20_L_21)))) ));
#line 207 "../Main.m3"
 /* set_label */
#line 207 "../Main.m3"
L29:;
#line 207 "../Main.m3"
 /* set_source_line */
#line 207 "../Main.m3"
#line 208 "../Main.m3"
 /* start_call_direct */
#line 208 "../Main.m3"
 /* load_integer */
#line 208 "../Main.m3"
 /* pop_param */
#line 208 "../Main.m3"
 /* load_integer */
#line 208 "../Main.m3"
 /* pop_param */
#line 208 "../Main.m3"
 /* call_direct */
#line 208 "../Main.m3"
 /* store */
#line 208 "../Main.m3"
(*(INT64*)(&Main_m_214_L_215))=(INT64)(((INT64)(Main__no_overlap_greater_enum_LT_false(
  ( Main__HighNumber )(((UINT8)( INT64_(4))) ),
  ( Main__LowNumber )(((UINT8)( INT64_(0))) )))));
#line 208 "../Main.m3"
 /* load */
#line 208 "../Main.m3"
 /* if_true_or_false */
#line 208 "../Main.m3"
 /* load_host_integer */
#line 208 "../Main.m3"
 /* load_integer */
#line 208 "../Main.m3"
 /* if_compare */
#line 208 "../Main.m3"
if(m3_eq(INT64,
  Main_m_214_L_215,
   INT64_(0)))goto L2A;
#line 208 "../Main.m3"
 /* start_call_direct */
#line 208 "../Main.m3"
 /* load_address */
#line 208 "../Main.m3"
 /* pop_param */
#line 208 "../Main.m3"
 /* load_integer */
#line 208 "../Main.m3"
 /* pop_param */
#line 208 "../Main.m3"
 /* load_address */
#line 208 "../Main.m3"
 /* pop_param */
#line 208 "../Main.m3"
 /* call_direct */
#line 208 "../Main.m3"
RTHooks__AssertFailed(
  ( ADDRESS )(((ADDRESS)(&Main_m_M_Main_L_22)) ),
  ( INTEGER )(  INT64_(208) ),
  ( TEXT )(((ADDRESS)(INT64_(2576)+((ADDRESS)(&Main_m_20_L_21)))) ));
#line 208 "../Main.m3"
 /* set_label */
#line 208 "../Main.m3"
L2A:;
#line 208 "../Main.m3"
 /* set_source_line */
#line 208 "../Main.m3"
#line 209 "../Main.m3"
 /* start_call_direct */
#line 209 "../Main.m3"
 /* load_integer */
#line 209 "../Main.m3"
 /* pop_param */
#line 209 "../Main.m3"
 /* load_integer */
#line 209 "../Main.m3"
 /* pop_param */
#line 209 "../Main.m3"
 /* call_direct */
#line 209 "../Main.m3"
 /* store */
#line 209 "../Main.m3"
(*(INT64*)(&Main_m_214_L_215))=(INT64)(((INT64)(Main__no_overlap_greater_enum_LE_false(
  ( Main__HighNumber )(((UINT8)( INT64_(4))) ),
  ( Main__LowNumber )(((UINT8)( INT64_(0))) )))));
#line 209 "../Main.m3"
 /* load */
#line 209 "../Main.m3"
 /* if_true_or_false */
#line 209 "../Main.m3"
 /* load_host_integer */
#line 209 "../Main.m3"
 /* load_integer */
#line 209 "../Main.m3"
 /* if_compare */
#line 209 "../Main.m3"
if(m3_eq(INT64,
  Main_m_214_L_215,
   INT64_(0)))goto L2B;
#line 209 "../Main.m3"
 /* start_call_direct */
#line 209 "../Main.m3"
 /* load_address */
#line 209 "../Main.m3"
 /* pop_param */
#line 209 "../Main.m3"
 /* load_integer */
#line 209 "../Main.m3"
 /* pop_param */
#line 209 "../Main.m3"
 /* load_address */
#line 209 "../Main.m3"
 /* pop_param */
#line 209 "../Main.m3"
 /* call_direct */
#line 209 "../Main.m3"
RTHooks__AssertFailed(
  ( ADDRESS )(((ADDRESS)(&Main_m_M_Main_L_22)) ),
  ( INTEGER )(  INT64_(209) ),
  ( TEXT )(((ADDRESS)(INT64_(2664)+((ADDRESS)(&Main_m_20_L_21)))) ));
#line 209 "../Main.m3"
 /* set_label */
#line 209 "../Main.m3"
L2B:;
#line 209 "../Main.m3"
 /* set_source_line */
#line 209 "../Main.m3"
#line 210 "../Main.m3"
 /* start_call_direct */
#line 210 "../Main.m3"
 /* load_integer */
#line 210 "../Main.m3"
 /* pop_param */
#line 210 "../Main.m3"
 /* load_integer */
#line 210 "../Main.m3"
 /* pop_param */
#line 210 "../Main.m3"
 /* call_direct */
#line 210 "../Main.m3"
 /* store */
#line 210 "../Main.m3"
(*(INT64*)(&Main_m_214_L_215))=(INT64)(((INT64)(Main__no_overlap_greater_enum_GT_true(
  ( Main__HighNumber )(((UINT8)( INT64_(4))) ),
  ( Main__LowNumber )(((UINT8)( INT64_(0))) )))));
#line 210 "../Main.m3"
 /* load */
#line 210 "../Main.m3"
 /* if_true_or_false */
#line 210 "../Main.m3"
 /* load_host_integer */
#line 210 "../Main.m3"
 /* load_integer */
#line 210 "../Main.m3"
 /* if_compare */
#line 210 "../Main.m3"
if(m3_ne(INT64,
  Main_m_214_L_215,
   INT64_(0)))goto L2C;
#line 210 "../Main.m3"
 /* start_call_direct */
#line 210 "../Main.m3"
 /* load_address */
#line 210 "../Main.m3"
 /* pop_param */
#line 210 "../Main.m3"
 /* load_integer */
#line 210 "../Main.m3"
 /* pop_param */
#line 210 "../Main.m3"
 /* load_address */
#line 210 "../Main.m3"
 /* pop_param */
#line 210 "../Main.m3"
 /* call_direct */
#line 210 "../Main.m3"
RTHooks__AssertFailed(
  ( ADDRESS )(((ADDRESS)(&Main_m_M_Main_L_22)) ),
  ( INTEGER )(  INT64_(210) ),
  ( TEXT )(((ADDRESS)(INT64_(2752)+((ADDRESS)(&Main_m_20_L_21)))) ));
#line 210 "../Main.m3"
 /* set_label */
#line 210 "../Main.m3"
L2C:;
#line 210 "../Main.m3"
 /* set_source_line */
#line 210 "../Main.m3"
#line 211 "../Main.m3"
 /* start_call_direct */
#line 211 "../Main.m3"
 /* load_integer */
#line 211 "../Main.m3"
 /* pop_param */
#line 211 "../Main.m3"
 /* load_integer */
#line 211 "../Main.m3"
 /* pop_param */
#line 211 "../Main.m3"
 /* call_direct */
#line 211 "../Main.m3"
 /* store */
#line 211 "../Main.m3"
(*(INT64*)(&Main_m_214_L_215))=(INT64)(((INT64)(Main__no_overlap_greater_enum_GE_true(
  ( Main__HighNumber )(((UINT8)( INT64_(4))) ),
  ( Main__LowNumber )(((UINT8)( INT64_(0))) )))));
#line 211 "../Main.m3"
 /* load */
#line 211 "../Main.m3"
 /* if_true_or_false */
#line 211 "../Main.m3"
 /* load_host_integer */
#line 211 "../Main.m3"
 /* load_integer */
#line 211 "../Main.m3"
 /* if_compare */
#line 211 "../Main.m3"
if(m3_ne(INT64,
  Main_m_214_L_215,
   INT64_(0)))goto L2D;
#line 211 "../Main.m3"
 /* start_call_direct */
#line 211 "../Main.m3"
 /* load_address */
#line 211 "../Main.m3"
 /* pop_param */
#line 211 "../Main.m3"
 /* load_integer */
#line 211 "../Main.m3"
 /* pop_param */
#line 211 "../Main.m3"
 /* load_address */
#line 211 "../Main.m3"
 /* pop_param */
#line 211 "../Main.m3"
 /* call_direct */
#line 211 "../Main.m3"
RTHooks__AssertFailed(
  ( ADDRESS )(((ADDRESS)(&Main_m_M_Main_L_22)) ),
  ( INTEGER )(  INT64_(211) ),
  ( TEXT )(((ADDRESS)(INT64_(2840)+((ADDRESS)(&Main_m_20_L_21)))) ));
#line 211 "../Main.m3"
 /* set_label */
#line 211 "../Main.m3"
L2D:;
#line 211 "../Main.m3"
 /* set_source_line */
#line 211 "../Main.m3"
#line 212 "../Main.m3"
 /* start_call_direct */
#line 212 "../Main.m3"
 /* load_integer */
#line 212 "../Main.m3"
 /* pop_param */
#line 212 "../Main.m3"
 /* load_integer */
#line 212 "../Main.m3"
 /* pop_param */
#line 212 "../Main.m3"
 /* call_direct */
#line 212 "../Main.m3"
 /* store */
#line 212 "../Main.m3"
(*(INT64*)(&Main_m_214_L_215))=(INT64)(((INT64)(Main__no_overlap_greater_enum_EQ_false(
  ( Main__HighNumber )(((UINT8)( INT64_(4))) ),
  ( Main__LowNumber )(((UINT8)( INT64_(0))) )))));
#line 212 "../Main.m3"
 /* load */
#line 212 "../Main.m3"
 /* if_true_or_false */
#line 212 "../Main.m3"
 /* load_host_integer */
#line 212 "../Main.m3"
 /* load_integer */
#line 212 "../Main.m3"
 /* if_compare */
#line 212 "../Main.m3"
if(m3_eq(INT64,
  Main_m_214_L_215,
   INT64_(0)))goto L2E;
#line 212 "../Main.m3"
 /* start_call_direct */
#line 212 "../Main.m3"
 /* load_address */
#line 212 "../Main.m3"
 /* pop_param */
#line 212 "../Main.m3"
 /* load_integer */
#line 212 "../Main.m3"
 /* pop_param */
#line 212 "../Main.m3"
 /* load_address */
#line 212 "../Main.m3"
 /* pop_param */
#line 212 "../Main.m3"
 /* call_direct */
#line 212 "../Main.m3"
RTHooks__AssertFailed(
  ( ADDRESS )(((ADDRESS)(&Main_m_M_Main_L_22)) ),
  ( INTEGER )(  INT64_(212) ),
  ( TEXT )(((ADDRESS)(INT64_(2928)+((ADDRESS)(&Main_m_20_L_21)))) ));
#line 212 "../Main.m3"
 /* set_label */
#line 212 "../Main.m3"
L2E:;
#line 212 "../Main.m3"
 /* set_source_line */
#line 212 "../Main.m3"
#line 213 "../Main.m3"
 /* start_call_direct */
#line 213 "../Main.m3"
 /* load_integer */
#line 213 "../Main.m3"
 /* pop_param */
#line 213 "../Main.m3"
 /* load_integer */
#line 213 "../Main.m3"
 /* pop_param */
#line 213 "../Main.m3"
 /* call_direct */
#line 213 "../Main.m3"
 /* store */
#line 213 "../Main.m3"
(*(INT64*)(&Main_m_214_L_215))=(INT64)(((INT64)(Main__no_overlap_greater_enum_NE_true(
  ( Main__HighNumber )(((UINT8)( INT64_(4))) ),
  ( Main__LowNumber )(((UINT8)( INT64_(0))) )))));
#line 213 "../Main.m3"
 /* load */
#line 213 "../Main.m3"
 /* if_true_or_false */
#line 213 "../Main.m3"
 /* load_host_integer */
#line 213 "../Main.m3"
 /* load_integer */
#line 213 "../Main.m3"
 /* if_compare */
#line 213 "../Main.m3"
if(m3_ne(INT64,
  Main_m_214_L_215,
   INT64_(0)))goto L2F;
#line 213 "../Main.m3"
 /* start_call_direct */
#line 213 "../Main.m3"
 /* load_address */
#line 213 "../Main.m3"
 /* pop_param */
#line 213 "../Main.m3"
 /* load_integer */
#line 213 "../Main.m3"
 /* pop_param */
#line 213 "../Main.m3"
 /* load_address */
#line 213 "../Main.m3"
 /* pop_param */
#line 213 "../Main.m3"
 /* call_direct */
#line 213 "../Main.m3"
RTHooks__AssertFailed(
  ( ADDRESS )(((ADDRESS)(&Main_m_M_Main_L_22)) ),
  ( INTEGER )(  INT64_(213) ),
  ( TEXT )(((ADDRESS)(INT64_(3016)+((ADDRESS)(&Main_m_20_L_21)))) ));
#line 213 "../Main.m3"
 /* set_label */
#line 213 "../Main.m3"
L2F:;
#line 213 "../Main.m3"
 /* set_source_line */
#line 213 "../Main.m3"
#line 214 "../Main.m3"
 /* start_call_direct */
#line 214 "../Main.m3"
 /* load_integer */
#line 214 "../Main.m3"
 /* pop_param */
#line 214 "../Main.m3"
 /* load_integer */
#line 214 "../Main.m3"
 /* pop_param */
#line 214 "../Main.m3"
 /* call_direct */
#line 214 "../Main.m3"
 /* store */
#line 214 "../Main.m3"
(*(INT64*)(&Main_m_214_L_215))=(INT64)(((INT64)(Main__minimum_overlap_greater_enum_LT_false(
  ( Main__MiddleNumber )(((UINT8)( INT64_(1))) ),
  ( Main__LowNumber )(((UINT8)( INT64_(0))) )))));
#line 214 "../Main.m3"
 /* load */
#line 214 "../Main.m3"
 /* if_true_or_false */
#line 214 "../Main.m3"
 /* load_host_integer */
#line 214 "../Main.m3"
 /* load_integer */
#line 214 "../Main.m3"
 /* if_compare */
#line 214 "../Main.m3"
if(m3_eq(INT64,
  Main_m_214_L_215,
   INT64_(0)))goto L30;
#line 214 "../Main.m3"
 /* start_call_direct */
#line 214 "../Main.m3"
 /* load_address */
#line 214 "../Main.m3"
 /* pop_param */
#line 214 "../Main.m3"
 /* load_integer */
#line 214 "../Main.m3"
 /* pop_param */
#line 214 "../Main.m3"
 /* load_address */
#line 214 "../Main.m3"
 /* pop_param */
#line 214 "../Main.m3"
 /* call_direct */
#line 214 "../Main.m3"
RTHooks__AssertFailed(
  ( ADDRESS )(((ADDRESS)(&Main_m_M_Main_L_22)) ),
  ( INTEGER )(  INT64_(214) ),
  ( TEXT )(((ADDRESS)(INT64_(3104)+((ADDRESS)(&Main_m_20_L_21)))) ));
#line 214 "../Main.m3"
 /* set_label */
#line 214 "../Main.m3"
L30:;
#line 214 "../Main.m3"
 /* set_source_line */
#line 214 "../Main.m3"
#line 215 "../Main.m3"
 /* start_call_direct */
#line 215 "../Main.m3"
 /* load_integer */
#line 215 "../Main.m3"
 /* pop_param */
#line 215 "../Main.m3"
 /* load_integer */
#line 215 "../Main.m3"
 /* pop_param */
#line 215 "../Main.m3"
 /* call_direct */
#line 215 "../Main.m3"
 /* store */
#line 215 "../Main.m3"
(*(INT64*)(&Main_m_214_L_215))=(INT64)(((INT64)(Main__minimum_overlap_greater_enum_GE_true(
  ( Main__MiddleNumber )(((UINT8)( INT64_(1))) ),
  ( Main__LowNumber )(((UINT8)( INT64_(0))) )))));
#line 215 "../Main.m3"
 /* load */
#line 215 "../Main.m3"
 /* if_true_or_false */
#line 215 "../Main.m3"
 /* load_host_integer */
#line 215 "../Main.m3"
 /* load_integer */
#line 215 "../Main.m3"
 /* if_compare */
#line 215 "../Main.m3"
if(m3_ne(INT64,
  Main_m_214_L_215,
   INT64_(0)))goto L31;
#line 215 "../Main.m3"
 /* start_call_direct */
#line 215 "../Main.m3"
 /* load_address */
#line 215 "../Main.m3"
 /* pop_param */
#line 215 "../Main.m3"
 /* load_integer */
#line 215 "../Main.m3"
 /* pop_param */
#line 215 "../Main.m3"
 /* load_address */
#line 215 "../Main.m3"
 /* pop_param */
#line 215 "../Main.m3"
 /* call_direct */
#line 215 "../Main.m3"
RTHooks__AssertFailed(
  ( ADDRESS )(((ADDRESS)(&Main_m_M_Main_L_22)) ),
  ( INTEGER )(  INT64_(215) ),
  ( TEXT )(((ADDRESS)(INT64_(3200)+((ADDRESS)(&Main_m_20_L_21)))) ));
#line 215 "../Main.m3"
 /* set_label */
#line 215 "../Main.m3"
L31:;
#line 215 "../Main.m3"
 /* set_source_line */
#line 215 "../Main.m3"
#line 216 "../Main.m3"
 /* start_call_direct */
#line 216 "../Main.m3"
 /* load_integer */
#line 216 "../Main.m3"
 /* pop_param */
#line 216 "../Main.m3"
 /* load_integer */
#line 216 "../Main.m3"
 /* pop_param */
#line 216 "../Main.m3"
 /* call_direct */
#line 216 "../Main.m3"
 /* store */
#line 216 "../Main.m3"
(*(INT64*)(&Main_m_214_L_215))=(INT64)(((INT64)(Main__overlap_1_LT_false(
  ( T8A2831D7_8 /*TypeText1*/  )(((UINT8)( INT64_(0))) ),
  ( T8A2831D7_8 /*TypeText1*/  )(((UINT8)( INT64_(0))) )))));
#line 216 "../Main.m3"
 /* load */
#line 216 "../Main.m3"
 /* if_true_or_false */
#line 216 "../Main.m3"
 /* load_host_integer */
#line 216 "../Main.m3"
 /* load_integer */
#line 216 "../Main.m3"
 /* if_compare */
#line 216 "../Main.m3"
if(m3_eq(INT64,
  Main_m_214_L_215,
   INT64_(0)))goto L32;
#line 216 "../Main.m3"
 /* start_call_direct */
#line 216 "../Main.m3"
 /* load_address */
#line 216 "../Main.m3"
 /* pop_param */
#line 216 "../Main.m3"
 /* load_integer */
#line 216 "../Main.m3"
 /* pop_param */
#line 216 "../Main.m3"
 /* load_address */
#line 216 "../Main.m3"
 /* pop_param */
#line 216 "../Main.m3"
 /* call_direct */
#line 216 "../Main.m3"
RTHooks__AssertFailed(
  ( ADDRESS )(((ADDRESS)(&Main_m_M_Main_L_22)) ),
  ( INTEGER )(  INT64_(216) ),
  ( TEXT )(((ADDRESS)(INT64_(3288)+((ADDRESS)(&Main_m_20_L_21)))) ));
#line 216 "../Main.m3"
 /* set_label */
#line 216 "../Main.m3"
L32:;
#line 216 "../Main.m3"
 /* set_source_line */
#line 216 "../Main.m3"
#line 217 "../Main.m3"
 /* start_call_direct */
#line 217 "../Main.m3"
 /* load_integer */
#line 217 "../Main.m3"
 /* pop_param */
#line 217 "../Main.m3"
 /* load_integer */
#line 217 "../Main.m3"
 /* pop_param */
#line 217 "../Main.m3"
 /* call_direct */
#line 217 "../Main.m3"
 /* store */
#line 217 "../Main.m3"
(*(INT64*)(&Main_m_214_L_215))=(INT64)(((INT64)(Main__overlap_1_LE_true(
  ( T8A2831D7_8 /*TypeText1*/  )(((UINT8)( INT64_(0))) ),
  ( T8A2831D7_8 /*TypeText1*/  )(((UINT8)( INT64_(0))) )))));
#line 217 "../Main.m3"
 /* load */
#line 217 "../Main.m3"
 /* if_true_or_false */
#line 217 "../Main.m3"
 /* load_host_integer */
#line 217 "../Main.m3"
 /* load_integer */
#line 217 "../Main.m3"
 /* if_compare */
#line 217 "../Main.m3"
if(m3_ne(INT64,
  Main_m_214_L_215,
   INT64_(0)))goto L33;
#line 217 "../Main.m3"
 /* start_call_direct */
#line 217 "../Main.m3"
 /* load_address */
#line 217 "../Main.m3"
 /* pop_param */
#line 217 "../Main.m3"
 /* load_integer */
#line 217 "../Main.m3"
 /* pop_param */
#line 217 "../Main.m3"
 /* load_address */
#line 217 "../Main.m3"
 /* pop_param */
#line 217 "../Main.m3"
 /* call_direct */
#line 217 "../Main.m3"
RTHooks__AssertFailed(
  ( ADDRESS )(((ADDRESS)(&Main_m_M_Main_L_22)) ),
  ( INTEGER )(  INT64_(217) ),
  ( TEXT )(((ADDRESS)(INT64_(3344)+((ADDRESS)(&Main_m_20_L_21)))) ));
#line 217 "../Main.m3"
 /* set_label */
#line 217 "../Main.m3"
L33:;
#line 217 "../Main.m3"
 /* set_source_line */
#line 217 "../Main.m3"
#line 218 "../Main.m3"
 /* start_call_direct */
#line 218 "../Main.m3"
 /* load_integer */
#line 218 "../Main.m3"
 /* pop_param */
#line 218 "../Main.m3"
 /* load_integer */
#line 218 "../Main.m3"
 /* pop_param */
#line 218 "../Main.m3"
 /* call_direct */
#line 218 "../Main.m3"
 /* store */
#line 218 "../Main.m3"
(*(INT64*)(&Main_m_214_L_215))=(INT64)(((INT64)(Main__overlap_1_GT_false(
  ( T8A2831D7_8 /*TypeText1*/  )(((UINT8)( INT64_(0))) ),
  ( T8A2831D7_8 /*TypeText1*/  )(((UINT8)( INT64_(0))) )))));
#line 218 "../Main.m3"
 /* load */
#line 218 "../Main.m3"
 /* if_true_or_false */
#line 218 "../Main.m3"
 /* load_host_integer */
#line 218 "../Main.m3"
 /* load_integer */
#line 218 "../Main.m3"
 /* if_compare */
#line 218 "../Main.m3"
if(m3_eq(INT64,
  Main_m_214_L_215,
   INT64_(0)))goto L34;
#line 218 "../Main.m3"
 /* start_call_direct */
#line 218 "../Main.m3"
 /* load_address */
#line 218 "../Main.m3"
 /* pop_param */
#line 218 "../Main.m3"
 /* load_integer */
#line 218 "../Main.m3"
 /* pop_param */
#line 218 "../Main.m3"
 /* load_address */
#line 218 "../Main.m3"
 /* pop_param */
#line 218 "../Main.m3"
 /* call_direct */
#line 218 "../Main.m3"
RTHooks__AssertFailed(
  ( ADDRESS )(((ADDRESS)(&Main_m_M_Main_L_22)) ),
  ( INTEGER )(  INT64_(218) ),
  ( TEXT )(((ADDRESS)(INT64_(3392)+((ADDRESS)(&Main_m_20_L_21)))) ));
#line 218 "../Main.m3"
 /* set_label */
#line 218 "../Main.m3"
L34:;
#line 218 "../Main.m3"
 /* set_source_line */
#line 218 "../Main.m3"
#line 219 "../Main.m3"
 /* start_call_direct */
#line 219 "../Main.m3"
 /* load_integer */
#line 219 "../Main.m3"
 /* pop_param */
#line 219 "../Main.m3"
 /* load_integer */
#line 219 "../Main.m3"
 /* pop_param */
#line 219 "../Main.m3"
 /* call_direct */
#line 219 "../Main.m3"
 /* store */
#line 219 "../Main.m3"
(*(INT64*)(&Main_m_214_L_215))=(INT64)(((INT64)(Main__overlap_1_GE_true(
  ( T8A2831D7_8 /*TypeText1*/  )(((UINT8)( INT64_(0))) ),
  ( T8A2831D7_8 /*TypeText1*/  )(((UINT8)( INT64_(0))) )))));
#line 219 "../Main.m3"
 /* load */
#line 219 "../Main.m3"
 /* if_true_or_false */
#line 219 "../Main.m3"
 /* load_host_integer */
#line 219 "../Main.m3"
 /* load_integer */
#line 219 "../Main.m3"
 /* if_compare */
#line 219 "../Main.m3"
if(m3_ne(INT64,
  Main_m_214_L_215,
   INT64_(0)))goto L35;
#line 219 "../Main.m3"
 /* start_call_direct */
#line 219 "../Main.m3"
 /* load_address */
#line 219 "../Main.m3"
 /* pop_param */
#line 219 "../Main.m3"
 /* load_integer */
#line 219 "../Main.m3"
 /* pop_param */
#line 219 "../Main.m3"
 /* load_address */
#line 219 "../Main.m3"
 /* pop_param */
#line 219 "../Main.m3"
 /* call_direct */
#line 219 "../Main.m3"
RTHooks__AssertFailed(
  ( ADDRESS )(((ADDRESS)(&Main_m_M_Main_L_22)) ),
  ( INTEGER )(  INT64_(219) ),
  ( TEXT )(((ADDRESS)(INT64_(3448)+((ADDRESS)(&Main_m_20_L_21)))) ));
#line 219 "../Main.m3"
 /* set_label */
#line 219 "../Main.m3"
L35:;
#line 219 "../Main.m3"
 /* set_source_line */
#line 219 "../Main.m3"
#line 220 "../Main.m3"
 /* start_call_direct */
#line 220 "../Main.m3"
 /* load_integer */
#line 220 "../Main.m3"
 /* pop_param */
#line 220 "../Main.m3"
 /* load_integer */
#line 220 "../Main.m3"
 /* pop_param */
#line 220 "../Main.m3"
 /* call_direct */
#line 220 "../Main.m3"
 /* store */
#line 220 "../Main.m3"
(*(INT64*)(&Main_m_214_L_215))=(INT64)(((INT64)(Main__overlap_1_EQ_true(
  ( T8A2831D7_8 /*TypeText1*/  )(((UINT8)( INT64_(0))) ),
  ( T8A2831D7_8 /*TypeText1*/  )(((UINT8)( INT64_(0))) )))));
#line 220 "../Main.m3"
 /* load */
#line 220 "../Main.m3"
 /* if_true_or_false */
#line 220 "../Main.m3"
 /* load_host_integer */
#line 220 "../Main.m3"
 /* load_integer */
#line 220 "../Main.m3"
 /* if_compare */
#line 220 "../Main.m3"
if(m3_ne(INT64,
  Main_m_214_L_215,
   INT64_(0)))goto L36;
#line 220 "../Main.m3"
 /* start_call_direct */
#line 220 "../Main.m3"
 /* load_address */
#line 220 "../Main.m3"
 /* pop_param */
#line 220 "../Main.m3"
 /* load_integer */
#line 220 "../Main.m3"
 /* pop_param */
#line 220 "../Main.m3"
 /* load_address */
#line 220 "../Main.m3"
 /* pop_param */
#line 220 "../Main.m3"
 /* call_direct */
#line 220 "../Main.m3"
RTHooks__AssertFailed(
  ( ADDRESS )(((ADDRESS)(&Main_m_M_Main_L_22)) ),
  ( INTEGER )(  INT64_(220) ),
  ( TEXT )(((ADDRESS)(INT64_(3496)+((ADDRESS)(&Main_m_20_L_21)))) ));
#line 220 "../Main.m3"
 /* set_label */
#line 220 "../Main.m3"
L36:;
#line 220 "../Main.m3"
 /* set_source_line */
#line 220 "../Main.m3"
#line 221 "../Main.m3"
 /* start_call_direct */
#line 221 "../Main.m3"
 /* load_integer */
#line 221 "../Main.m3"
 /* pop_param */
#line 221 "../Main.m3"
 /* load_integer */
#line 221 "../Main.m3"
 /* pop_param */
#line 221 "../Main.m3"
 /* call_direct */
#line 221 "../Main.m3"
 /* store */
#line 221 "../Main.m3"
(*(INT64*)(&Main_m_214_L_215))=(INT64)(((INT64)(Main__overlap_1_NE_false(
  ( T8A2831D7_8 /*TypeText1*/  )(((UINT8)( INT64_(0))) ),
  ( T8A2831D7_8 /*TypeText1*/  )(((UINT8)( INT64_(0))) )))));
#line 221 "../Main.m3"
 /* load */
#line 221 "../Main.m3"
 /* if_true_or_false */
#line 221 "../Main.m3"
 /* load_host_integer */
#line 221 "../Main.m3"
 /* load_integer */
#line 221 "../Main.m3"
 /* if_compare */
#line 221 "../Main.m3"
if(m3_eq(INT64,
  Main_m_214_L_215,
   INT64_(0)))goto L37;
#line 221 "../Main.m3"
 /* start_call_direct */
#line 221 "../Main.m3"
 /* load_address */
#line 221 "../Main.m3"
 /* pop_param */
#line 221 "../Main.m3"
 /* load_integer */
#line 221 "../Main.m3"
 /* pop_param */
#line 221 "../Main.m3"
 /* load_address */
#line 221 "../Main.m3"
 /* pop_param */
#line 221 "../Main.m3"
 /* call_direct */
#line 221 "../Main.m3"
RTHooks__AssertFailed(
  ( ADDRESS )(((ADDRESS)(&Main_m_M_Main_L_22)) ),
  ( INTEGER )(  INT64_(221) ),
  ( TEXT )(((ADDRESS)(INT64_(3544)+((ADDRESS)(&Main_m_20_L_21)))) ));
#line 221 "../Main.m3"
 /* set_label */
#line 221 "../Main.m3"
L37:;
#line 221 "../Main.m3"
 /* set_source_line */
#line 221 "../Main.m3"
#line 222 "../Main.m3"
 /* start_call_direct */
#line 222 "../Main.m3"
 /* load_integer */
#line 222 "../Main.m3"
 /* pop_param */
#line 222 "../Main.m3"
 /* call_direct */
#line 222 "../Main.m3"
 /* store */
#line 222 "../Main.m3"
(*(INT64*)(&Main_m_214_L_215))=(INT64)(((INT64)(Main__ord_enum_vs_negative_LT_false(
  ( Main__Number )(((UINT8)( INT64_(0))) )))));
#line 222 "../Main.m3"
 /* load */
#line 222 "../Main.m3"
 /* if_true_or_false */
#line 222 "../Main.m3"
 /* load_host_integer */
#line 222 "../Main.m3"
 /* load_integer */
#line 222 "../Main.m3"
 /* if_compare */
#line 222 "../Main.m3"
if(m3_eq(INT64,
  Main_m_214_L_215,
   INT64_(0)))goto L38;
#line 222 "../Main.m3"
 /* start_call_direct */
#line 222 "../Main.m3"
 /* load_address */
#line 222 "../Main.m3"
 /* pop_param */
#line 222 "../Main.m3"
 /* load_integer */
#line 222 "../Main.m3"
 /* pop_param */
#line 222 "../Main.m3"
 /* load_address */
#line 222 "../Main.m3"
 /* pop_param */
#line 222 "../Main.m3"
 /* call_direct */
#line 222 "../Main.m3"
RTHooks__AssertFailed(
  ( ADDRESS )(((ADDRESS)(&Main_m_M_Main_L_22)) ),
  ( INTEGER )(  INT64_(222) ),
  ( TEXT )(((ADDRESS)(INT64_(3600)+((ADDRESS)(&Main_m_20_L_21)))) ));
#line 222 "../Main.m3"
 /* set_label */
#line 222 "../Main.m3"
L38:;
#line 222 "../Main.m3"
 /* set_source_line */
#line 222 "../Main.m3"
#line 223 "../Main.m3"
 /* start_call_direct */
#line 223 "../Main.m3"
 /* load_integer */
#line 223 "../Main.m3"
 /* pop_param */
#line 223 "../Main.m3"
 /* call_direct */
#line 223 "../Main.m3"
 /* store */
#line 223 "../Main.m3"
(*(INT64*)(&Main_m_214_L_215))=(INT64)(((INT64)(Main__ord_enum_vs_negative_LE_false(
  ( Main__Number )(((UINT8)( INT64_(0))) )))));
#line 223 "../Main.m3"
 /* load */
#line 223 "../Main.m3"
 /* if_true_or_false */
#line 223 "../Main.m3"
 /* load_host_integer */
#line 223 "../Main.m3"
 /* load_integer */
#line 223 "../Main.m3"
 /* if_compare */
#line 223 "../Main.m3"
if(m3_eq(INT64,
  Main_m_214_L_215,
   INT64_(0)))goto L39;
#line 223 "../Main.m3"
 /* start_call_direct */
#line 223 "../Main.m3"
 /* load_address */
#line 223 "../Main.m3"
 /* pop_param */
#line 223 "../Main.m3"
 /* load_integer */
#line 223 "../Main.m3"
 /* pop_param */
#line 223 "../Main.m3"
 /* load_address */
#line 223 "../Main.m3"
 /* pop_param */
#line 223 "../Main.m3"
 /* call_direct */
#line 223 "../Main.m3"
RTHooks__AssertFailed(
  ( ADDRESS )(((ADDRESS)(&Main_m_M_Main_L_22)) ),
  ( INTEGER )(  INT64_(223) ),
  ( TEXT )(((ADDRESS)(INT64_(3672)+((ADDRESS)(&Main_m_20_L_21)))) ));
#line 223 "../Main.m3"
 /* set_label */
#line 223 "../Main.m3"
L39:;
#line 223 "../Main.m3"
 /* set_source_line */
#line 223 "../Main.m3"
#line 224 "../Main.m3"
 /* start_call_direct */
#line 224 "../Main.m3"
 /* load_integer */
#line 224 "../Main.m3"
 /* pop_param */
#line 224 "../Main.m3"
 /* call_direct */
#line 224 "../Main.m3"
 /* store */
#line 224 "../Main.m3"
(*(INT64*)(&Main_m_214_L_215))=(INT64)(((INT64)(Main__ord_enum_vs_negative_GT_true(
  ( Main__Number )(((UINT8)( INT64_(0))) )))));
#line 224 "../Main.m3"
 /* load */
#line 224 "../Main.m3"
 /* if_true_or_false */
#line 224 "../Main.m3"
 /* load_host_integer */
#line 224 "../Main.m3"
 /* load_integer */
#line 224 "../Main.m3"
 /* if_compare */
#line 224 "../Main.m3"
if(m3_ne(INT64,
  Main_m_214_L_215,
   INT64_(0)))goto L3A;
#line 224 "../Main.m3"
 /* start_call_direct */
#line 224 "../Main.m3"
 /* load_address */
#line 224 "../Main.m3"
 /* pop_param */
#line 224 "../Main.m3"
 /* load_integer */
#line 224 "../Main.m3"
 /* pop_param */
#line 224 "../Main.m3"
 /* load_address */
#line 224 "../Main.m3"
 /* pop_param */
#line 224 "../Main.m3"
 /* call_direct */
#line 224 "../Main.m3"
RTHooks__AssertFailed(
  ( ADDRESS )(((ADDRESS)(&Main_m_M_Main_L_22)) ),
  ( INTEGER )(  INT64_(224) ),
  ( TEXT )(((ADDRESS)(INT64_(3744)+((ADDRESS)(&Main_m_20_L_21)))) ));
#line 224 "../Main.m3"
 /* set_label */
#line 224 "../Main.m3"
L3A:;
#line 224 "../Main.m3"
 /* set_source_line */
#line 224 "../Main.m3"
#line 225 "../Main.m3"
 /* start_call_direct */
#line 225 "../Main.m3"
 /* load_integer */
#line 225 "../Main.m3"
 /* pop_param */
#line 225 "../Main.m3"
 /* call_direct */
#line 225 "../Main.m3"
 /* store */
#line 225 "../Main.m3"
(*(INT64*)(&Main_m_214_L_215))=(INT64)(((INT64)(Main__ord_enum_vs_negative_GE_true(
  ( Main__Number )(((UINT8)( INT64_(0))) )))));
#line 225 "../Main.m3"
 /* load */
#line 225 "../Main.m3"
 /* if_true_or_false */
#line 225 "../Main.m3"
 /* load_host_integer */
#line 225 "../Main.m3"
 /* load_integer */
#line 225 "../Main.m3"
 /* if_compare */
#line 225 "../Main.m3"
if(m3_ne(INT64,
  Main_m_214_L_215,
   INT64_(0)))goto L3B;
#line 225 "../Main.m3"
 /* start_call_direct */
#line 225 "../Main.m3"
 /* load_address */
#line 225 "../Main.m3"
 /* pop_param */
#line 225 "../Main.m3"
 /* load_integer */
#line 225 "../Main.m3"
 /* pop_param */
#line 225 "../Main.m3"
 /* load_address */
#line 225 "../Main.m3"
 /* pop_param */
#line 225 "../Main.m3"
 /* call_direct */
#line 225 "../Main.m3"
RTHooks__AssertFailed(
  ( ADDRESS )(((ADDRESS)(&Main_m_M_Main_L_22)) ),
  ( INTEGER )(  INT64_(225) ),
  ( TEXT )(((ADDRESS)(INT64_(3816)+((ADDRESS)(&Main_m_20_L_21)))) ));
#line 225 "../Main.m3"
 /* set_label */
#line 225 "../Main.m3"
L3B:;
#line 225 "../Main.m3"
 /* set_source_line */
#line 225 "../Main.m3"
#line 226 "../Main.m3"
 /* start_call_direct */
#line 226 "../Main.m3"
 /* load_integer */
#line 226 "../Main.m3"
 /* pop_param */
#line 226 "../Main.m3"
 /* call_direct */
#line 226 "../Main.m3"
 /* store */
#line 226 "../Main.m3"
(*(INT64*)(&Main_m_214_L_215))=(INT64)(((INT64)(Main__ord_enum_vs_negative_EQ_false(
  ( Main__Number )(((UINT8)( INT64_(0))) )))));
#line 226 "../Main.m3"
 /* load */
#line 226 "../Main.m3"
 /* if_true_or_false */
#line 226 "../Main.m3"
 /* load_host_integer */
#line 226 "../Main.m3"
 /* load_integer */
#line 226 "../Main.m3"
 /* if_compare */
#line 226 "../Main.m3"
if(m3_eq(INT64,
  Main_m_214_L_215,
   INT64_(0)))goto L3C;
#line 226 "../Main.m3"
 /* start_call_direct */
#line 226 "../Main.m3"
 /* load_address */
#line 226 "../Main.m3"
 /* pop_param */
#line 226 "../Main.m3"
 /* load_integer */
#line 226 "../Main.m3"
 /* pop_param */
#line 226 "../Main.m3"
 /* load_address */
#line 226 "../Main.m3"
 /* pop_param */
#line 226 "../Main.m3"
 /* call_direct */
#line 226 "../Main.m3"
RTHooks__AssertFailed(
  ( ADDRESS )(((ADDRESS)(&Main_m_M_Main_L_22)) ),
  ( INTEGER )(  INT64_(226) ),
  ( TEXT )(((ADDRESS)(INT64_(3888)+((ADDRESS)(&Main_m_20_L_21)))) ));
#line 226 "../Main.m3"
 /* set_label */
#line 226 "../Main.m3"
L3C:;
#line 226 "../Main.m3"
 /* set_source_line */
#line 226 "../Main.m3"
#line 227 "../Main.m3"
 /* start_call_direct */
#line 227 "../Main.m3"
 /* load_integer */
#line 227 "../Main.m3"
 /* pop_param */
#line 227 "../Main.m3"
 /* call_direct */
#line 227 "../Main.m3"
 /* store */
#line 227 "../Main.m3"
(*(INT64*)(&Main_m_214_L_215))=(INT64)(((INT64)(Main__ord_enum_vs_negative_NE_true(
  ( Main__Number )(((UINT8)( INT64_(0))) )))));
#line 227 "../Main.m3"
 /* load */
#line 227 "../Main.m3"
 /* if_true_or_false */
#line 227 "../Main.m3"
 /* load_host_integer */
#line 227 "../Main.m3"
 /* load_integer */
#line 227 "../Main.m3"
 /* if_compare */
#line 227 "../Main.m3"
if(m3_ne(INT64,
  Main_m_214_L_215,
   INT64_(0)))goto L3D;
#line 227 "../Main.m3"
 /* start_call_direct */
#line 227 "../Main.m3"
 /* load_address */
#line 227 "../Main.m3"
 /* pop_param */
#line 227 "../Main.m3"
 /* load_integer */
#line 227 "../Main.m3"
 /* pop_param */
#line 227 "../Main.m3"
 /* load_address */
#line 227 "../Main.m3"
 /* pop_param */
#line 227 "../Main.m3"
 /* call_direct */
#line 227 "../Main.m3"
RTHooks__AssertFailed(
  ( ADDRESS )(((ADDRESS)(&Main_m_M_Main_L_22)) ),
  ( INTEGER )(  INT64_(227) ),
  ( TEXT )(((ADDRESS)(INT64_(3960)+((ADDRESS)(&Main_m_20_L_21)))) ));
#line 227 "../Main.m3"
 /* set_label */
#line 227 "../Main.m3"
L3D:;
#line 227 "../Main.m3"
 /* set_source_line */
#line 227 "../Main.m3"
#line 228 "../Main.m3"
 /* start_call_direct */
#line 228 "../Main.m3"
 /* load_integer */
#line 228 "../Main.m3"
 /* pop_param */
#line 228 "../Main.m3"
 /* call_direct */
#line 228 "../Main.m3"
 /* store */
#line 228 "../Main.m3"
(*(INT64*)(&Main_m_214_L_215))=(INT64)(((INT64)(Main__abs_vs_negative_LT_false(
  ( INTEGER )(  INT64_(0) )))));
#line 228 "../Main.m3"
 /* load */
#line 228 "../Main.m3"
 /* if_true_or_false */
#line 228 "../Main.m3"
 /* load_host_integer */
#line 228 "../Main.m3"
 /* load_integer */
#line 228 "../Main.m3"
 /* if_compare */
#line 228 "../Main.m3"
if(m3_eq(INT64,
  Main_m_214_L_215,
   INT64_(0)))goto L3E;
#line 228 "../Main.m3"
 /* start_call_direct */
#line 228 "../Main.m3"
 /* load_address */
#line 228 "../Main.m3"
 /* pop_param */
#line 228 "../Main.m3"
 /* load_integer */
#line 228 "../Main.m3"
 /* pop_param */
#line 228 "../Main.m3"
 /* load_address */
#line 228 "../Main.m3"
 /* pop_param */
#line 228 "../Main.m3"
 /* call_direct */
#line 228 "../Main.m3"
RTHooks__AssertFailed(
  ( ADDRESS )(((ADDRESS)(&Main_m_M_Main_L_22)) ),
  ( INTEGER )(  INT64_(228) ),
  ( TEXT )(((ADDRESS)(INT64_(4032)+((ADDRESS)(&Main_m_20_L_21)))) ));
#line 228 "../Main.m3"
 /* set_label */
#line 228 "../Main.m3"
L3E:;
#line 228 "../Main.m3"
 /* set_source_line */
#line 228 "../Main.m3"
#line 229 "../Main.m3"
 /* start_call_direct */
#line 229 "../Main.m3"
 /* load_integer */
#line 229 "../Main.m3"
 /* pop_param */
#line 229 "../Main.m3"
 /* call_direct */
#line 229 "../Main.m3"
 /* store */
#line 229 "../Main.m3"
(*(INT64*)(&Main_m_214_L_215))=(INT64)(((INT64)(Main__abs_vs_negative_LE_false(
  ( INTEGER )(  INT64_(0) )))));
#line 229 "../Main.m3"
 /* load */
#line 229 "../Main.m3"
 /* if_true_or_false */
#line 229 "../Main.m3"
 /* load_host_integer */
#line 229 "../Main.m3"
 /* load_integer */
#line 229 "../Main.m3"
 /* if_compare */
#line 229 "../Main.m3"
if(m3_eq(INT64,
  Main_m_214_L_215,
   INT64_(0)))goto L3F;
#line 229 "../Main.m3"
 /* start_call_direct */
#line 229 "../Main.m3"
 /* load_address */
#line 229 "../Main.m3"
 /* pop_param */
#line 229 "../Main.m3"
 /* load_integer */
#line 229 "../Main.m3"
 /* pop_param */
#line 229 "../Main.m3"
 /* load_address */
#line 229 "../Main.m3"
 /* pop_param */
#line 229 "../Main.m3"
 /* call_direct */
#line 229 "../Main.m3"
RTHooks__AssertFailed(
  ( ADDRESS )(((ADDRESS)(&Main_m_M_Main_L_22)) ),
  ( INTEGER )(  INT64_(229) ),
  ( TEXT )(((ADDRESS)(INT64_(4096)+((ADDRESS)(&Main_m_20_L_21)))) ));
#line 229 "../Main.m3"
 /* set_label */
#line 229 "../Main.m3"
L3F:;
#line 229 "../Main.m3"
 /* set_source_line */
#line 229 "../Main.m3"
#line 230 "../Main.m3"
 /* start_call_direct */
#line 230 "../Main.m3"
 /* load_integer */
#line 230 "../Main.m3"
 /* pop_param */
#line 230 "../Main.m3"
 /* call_direct */
#line 230 "../Main.m3"
 /* store */
#line 230 "../Main.m3"
(*(INT64*)(&Main_m_214_L_215))=(INT64)(((INT64)(Main__abs_vs_negative_GT_true(
  ( INTEGER )(  INT64_(0) )))));
#line 230 "../Main.m3"
 /* load */
#line 230 "../Main.m3"
 /* if_true_or_false */
#line 230 "../Main.m3"
 /* load_host_integer */
#line 230 "../Main.m3"
 /* load_integer */
#line 230 "../Main.m3"
 /* if_compare */
#line 230 "../Main.m3"
if(m3_ne(INT64,
  Main_m_214_L_215,
   INT64_(0)))goto L40;
#line 230 "../Main.m3"
 /* start_call_direct */
#line 230 "../Main.m3"
 /* load_address */
#line 230 "../Main.m3"
 /* pop_param */
#line 230 "../Main.m3"
 /* load_integer */
#line 230 "../Main.m3"
 /* pop_param */
#line 230 "../Main.m3"
 /* load_address */
#line 230 "../Main.m3"
 /* pop_param */
#line 230 "../Main.m3"
 /* call_direct */
#line 230 "../Main.m3"
RTHooks__AssertFailed(
  ( ADDRESS )(((ADDRESS)(&Main_m_M_Main_L_22)) ),
  ( INTEGER )(  INT64_(230) ),
  ( TEXT )(((ADDRESS)(INT64_(4160)+((ADDRESS)(&Main_m_20_L_21)))) ));
#line 230 "../Main.m3"
 /* set_label */
#line 230 "../Main.m3"
L40:;
#line 230 "../Main.m3"
 /* set_source_line */
#line 230 "../Main.m3"
#line 231 "../Main.m3"
 /* start_call_direct */
#line 231 "../Main.m3"
 /* load_integer */
#line 231 "../Main.m3"
 /* pop_param */
#line 231 "../Main.m3"
 /* call_direct */
#line 231 "../Main.m3"
 /* store */
#line 231 "../Main.m3"
(*(INT64*)(&Main_m_214_L_215))=(INT64)(((INT64)(Main__abs_vs_negative_GE_true(
  ( INTEGER )(  INT64_(0) )))));
#line 231 "../Main.m3"
 /* load */
#line 231 "../Main.m3"
 /* if_true_or_false */
#line 231 "../Main.m3"
 /* load_host_integer */
#line 231 "../Main.m3"
 /* load_integer */
#line 231 "../Main.m3"
 /* if_compare */
#line 231 "../Main.m3"
if(m3_ne(INT64,
  Main_m_214_L_215,
   INT64_(0)))goto L41;
#line 231 "../Main.m3"
 /* start_call_direct */
#line 231 "../Main.m3"
 /* load_address */
#line 231 "../Main.m3"
 /* pop_param */
#line 231 "../Main.m3"
 /* load_integer */
#line 231 "../Main.m3"
 /* pop_param */
#line 231 "../Main.m3"
 /* load_address */
#line 231 "../Main.m3"
 /* pop_param */
#line 231 "../Main.m3"
 /* call_direct */
#line 231 "../Main.m3"
RTHooks__AssertFailed(
  ( ADDRESS )(((ADDRESS)(&Main_m_M_Main_L_22)) ),
  ( INTEGER )(  INT64_(231) ),
  ( TEXT )(((ADDRESS)(INT64_(4216)+((ADDRESS)(&Main_m_20_L_21)))) ));
#line 231 "../Main.m3"
 /* set_label */
#line 231 "../Main.m3"
L41:;
#line 231 "../Main.m3"
 /* set_source_line */
#line 231 "../Main.m3"
#line 232 "../Main.m3"
 /* start_call_direct */
#line 232 "../Main.m3"
 /* load_integer */
#line 232 "../Main.m3"
 /* pop_param */
#line 232 "../Main.m3"
 /* call_direct */
#line 232 "../Main.m3"
 /* store */
#line 232 "../Main.m3"
(*(INT64*)(&Main_m_214_L_215))=(INT64)(((INT64)(Main__abs_vs_negative_EQ_false(
  ( INTEGER )(  INT64_(0) )))));
#line 232 "../Main.m3"
 /* load */
#line 232 "../Main.m3"
 /* if_true_or_false */
#line 232 "../Main.m3"
 /* load_host_integer */
#line 232 "../Main.m3"
 /* load_integer */
#line 232 "../Main.m3"
 /* if_compare */
#line 232 "../Main.m3"
if(m3_eq(INT64,
  Main_m_214_L_215,
   INT64_(0)))goto L42;
#line 232 "../Main.m3"
 /* start_call_direct */
#line 232 "../Main.m3"
 /* load_address */
#line 232 "../Main.m3"
 /* pop_param */
#line 232 "../Main.m3"
 /* load_integer */
#line 232 "../Main.m3"
 /* pop_param */
#line 232 "../Main.m3"
 /* load_address */
#line 232 "../Main.m3"
 /* pop_param */
#line 232 "../Main.m3"
 /* call_direct */
#line 232 "../Main.m3"
RTHooks__AssertFailed(
  ( ADDRESS )(((ADDRESS)(&Main_m_M_Main_L_22)) ),
  ( INTEGER )(  INT64_(232) ),
  ( TEXT )(((ADDRESS)(INT64_(4272)+((ADDRESS)(&Main_m_20_L_21)))) ));
#line 232 "../Main.m3"
 /* set_label */
#line 232 "../Main.m3"
L42:;
#line 232 "../Main.m3"
 /* set_source_line */
#line 232 "../Main.m3"
#line 233 "../Main.m3"
 /* start_call_direct */
#line 233 "../Main.m3"
 /* load_integer */
#line 233 "../Main.m3"
 /* pop_param */
#line 233 "../Main.m3"
 /* call_direct */
#line 233 "../Main.m3"
 /* store */
#line 233 "../Main.m3"
(*(INT64*)(&Main_m_214_L_215))=(INT64)(((INT64)(Main__abs_vs_negative_NE_true(
  ( INTEGER )(  INT64_(0) )))));
#line 233 "../Main.m3"
 /* load */
#line 233 "../Main.m3"
 /* if_true_or_false */
#line 233 "../Main.m3"
 /* load_host_integer */
#line 233 "../Main.m3"
 /* load_integer */
#line 233 "../Main.m3"
 /* if_compare */
#line 233 "../Main.m3"
if(m3_ne(INT64,
  Main_m_214_L_215,
   INT64_(0)))goto L43;
#line 233 "../Main.m3"
 /* start_call_direct */
#line 233 "../Main.m3"
 /* load_address */
#line 233 "../Main.m3"
 /* pop_param */
#line 233 "../Main.m3"
 /* load_integer */
#line 233 "../Main.m3"
 /* pop_param */
#line 233 "../Main.m3"
 /* load_address */
#line 233 "../Main.m3"
 /* pop_param */
#line 233 "../Main.m3"
 /* call_direct */
#line 233 "../Main.m3"
RTHooks__AssertFailed(
  ( ADDRESS )(((ADDRESS)(&Main_m_M_Main_L_22)) ),
  ( INTEGER )(  INT64_(233) ),
  ( TEXT )(((ADDRESS)(INT64_(4336)+((ADDRESS)(&Main_m_20_L_21)))) ));
#line 233 "../Main.m3"
 /* set_label */
#line 233 "../Main.m3"
L43:;
#line 233 "../Main.m3"
 /* set_source_line */
#line 233 "../Main.m3"
#line 234 "../Main.m3"
 /* start_call_direct */
#line 234 "../Main.m3"
 /* load_integer */
#line 234 "../Main.m3"
 /* pop_param */
#line 234 "../Main.m3"
 /* call_direct */
#line 234 "../Main.m3"
 /* store */
#line 234 "../Main.m3"
(*(INT64*)(&Main_m_214_L_215))=(INT64)(((INT64)(Main__abs_vs_zero_LT_false(
  ( INTEGER )(  INT64_(0) )))));
#line 234 "../Main.m3"
 /* load */
#line 234 "../Main.m3"
 /* if_true_or_false */
#line 234 "../Main.m3"
 /* load_host_integer */
#line 234 "../Main.m3"
 /* load_integer */
#line 234 "../Main.m3"
 /* if_compare */
#line 234 "../Main.m3"
if(m3_eq(INT64,
  Main_m_214_L_215,
   INT64_(0)))goto L44;
#line 234 "../Main.m3"
 /* start_call_direct */
#line 234 "../Main.m3"
 /* load_address */
#line 234 "../Main.m3"
 /* pop_param */
#line 234 "../Main.m3"
 /* load_integer */
#line 234 "../Main.m3"
 /* pop_param */
#line 234 "../Main.m3"
 /* load_address */
#line 234 "../Main.m3"
 /* pop_param */
#line 234 "../Main.m3"
 /* call_direct */
#line 234 "../Main.m3"
RTHooks__AssertFailed(
  ( ADDRESS )(((ADDRESS)(&Main_m_M_Main_L_22)) ),
  ( INTEGER )(  INT64_(234) ),
  ( TEXT )(((ADDRESS)(INT64_(4392)+((ADDRESS)(&Main_m_20_L_21)))) ));
#line 234 "../Main.m3"
 /* set_label */
#line 234 "../Main.m3"
L44:;
#line 234 "../Main.m3"
 /* set_source_line */
#line 234 "../Main.m3"
#line 235 "../Main.m3"
 /* start_call_direct */
#line 235 "../Main.m3"
 /* load_integer */
#line 235 "../Main.m3"
 /* pop_param */
#line 235 "../Main.m3"
 /* call_direct */
#line 235 "../Main.m3"
 /* store */
#line 235 "../Main.m3"
(*(INT64*)(&Main_m_214_L_215))=(INT64)(((INT64)(Main__abs_vs_zero_GE_true(
  ( INTEGER )(  INT64_(0) )))));
#line 235 "../Main.m3"
 /* load */
#line 235 "../Main.m3"
 /* if_true_or_false */
#line 235 "../Main.m3"
 /* load_host_integer */
#line 235 "../Main.m3"
 /* load_integer */
#line 235 "../Main.m3"
 /* if_compare */
#line 235 "../Main.m3"
if(m3_ne(INT64,
  Main_m_214_L_215,
   INT64_(0)))goto L45;
#line 235 "../Main.m3"
 /* start_call_direct */
#line 235 "../Main.m3"
 /* load_address */
#line 235 "../Main.m3"
 /* pop_param */
#line 235 "../Main.m3"
 /* load_integer */
#line 235 "../Main.m3"
 /* pop_param */
#line 235 "../Main.m3"
 /* load_address */
#line 235 "../Main.m3"
 /* pop_param */
#line 235 "../Main.m3"
 /* call_direct */
#line 235 "../Main.m3"
RTHooks__AssertFailed(
  ( ADDRESS )(((ADDRESS)(&Main_m_M_Main_L_22)) ),
  ( INTEGER )(  INT64_(235) ),
  ( TEXT )(((ADDRESS)(INT64_(4448)+((ADDRESS)(&Main_m_20_L_21)))) ));
#line 235 "../Main.m3"
 /* set_label */
#line 235 "../Main.m3"
L45:;
#line 235 "../Main.m3"
 /* set_source_line */
#line 235 "../Main.m3"
#line 236 "../Main.m3"
 /* start_call_direct */
#line 236 "../Main.m3"
 /* load_integer */
#line 236 "../Main.m3"
 /* pop_param */
#line 236 "../Main.m3"
 /* call_direct */
#line 236 "../Main.m3"
 /* store */
#line 236 "../Main.m3"
(*(INT64*)(&Main_m_214_L_215))=(INT64)(((INT64)(Main__neg_abs_vs_zero_LE_true(
  ( INTEGER )(  INT64_(0) )))));
#line 236 "../Main.m3"
 /* load */
#line 236 "../Main.m3"
 /* if_true_or_false */
#line 236 "../Main.m3"
 /* load_host_integer */
#line 236 "../Main.m3"
 /* load_integer */
#line 236 "../Main.m3"
 /* if_compare */
#line 236 "../Main.m3"
if(m3_ne(INT64,
  Main_m_214_L_215,
   INT64_(0)))goto L46;
#line 236 "../Main.m3"
 /* start_call_direct */
#line 236 "../Main.m3"
 /* load_address */
#line 236 "../Main.m3"
 /* pop_param */
#line 236 "../Main.m3"
 /* load_integer */
#line 236 "../Main.m3"
 /* pop_param */
#line 236 "../Main.m3"
 /* load_address */
#line 236 "../Main.m3"
 /* pop_param */
#line 236 "../Main.m3"
 /* call_direct */
#line 236 "../Main.m3"
RTHooks__AssertFailed(
  ( ADDRESS )(((ADDRESS)(&Main_m_M_Main_L_22)) ),
  ( INTEGER )(  INT64_(236) ),
  ( TEXT )(((ADDRESS)(INT64_(4496)+((ADDRESS)(&Main_m_20_L_21)))) ));
#line 236 "../Main.m3"
 /* set_label */
#line 236 "../Main.m3"
L46:;
#line 236 "../Main.m3"
 /* set_source_line */
#line 236 "../Main.m3"
#line 237 "../Main.m3"
 /* start_call_direct */
#line 237 "../Main.m3"
 /* load_integer */
#line 237 "../Main.m3"
 /* pop_param */
#line 237 "../Main.m3"
 /* call_direct */
#line 237 "../Main.m3"
 /* store */
#line 237 "../Main.m3"
(*(INT64*)(&Main_m_214_L_215))=(INT64)(((INT64)(Main__neg_abs_vs_zero_GT_false(
  ( INTEGER )(  INT64_(0) )))));
#line 237 "../Main.m3"
 /* load */
#line 237 "../Main.m3"
 /* if_true_or_false */
#line 237 "../Main.m3"
 /* load_host_integer */
#line 237 "../Main.m3"
 /* load_integer */
#line 237 "../Main.m3"
 /* if_compare */
#line 237 "../Main.m3"
if(m3_eq(INT64,
  Main_m_214_L_215,
   INT64_(0)))goto L47;
#line 237 "../Main.m3"
 /* start_call_direct */
#line 237 "../Main.m3"
 /* load_address */
#line 237 "../Main.m3"
 /* pop_param */
#line 237 "../Main.m3"
 /* load_integer */
#line 237 "../Main.m3"
 /* pop_param */
#line 237 "../Main.m3"
 /* load_address */
#line 237 "../Main.m3"
 /* pop_param */
#line 237 "../Main.m3"
 /* call_direct */
#line 237 "../Main.m3"
RTHooks__AssertFailed(
  ( ADDRESS )(((ADDRESS)(&Main_m_M_Main_L_22)) ),
  ( INTEGER )(  INT64_(237) ),
  ( TEXT )(((ADDRESS)(INT64_(4552)+((ADDRESS)(&Main_m_20_L_21)))) ));
#line 237 "../Main.m3"
 /* set_label */
#line 237 "../Main.m3"
L47:;
#line 237 "../Main.m3"
 /* set_source_line */
#line 237 "../Main.m3"
#line 238 "../Main.m3"
 /* start_call_direct */
#line 238 "../Main.m3"
 /* load_integer */
#line 238 "../Main.m3"
 /* pop_param */
#line 238 "../Main.m3"
 /* call_direct */
#line 238 "../Main.m3"
 /* store */
#line 238 "../Main.m3"
(*(INT64*)(&Main_m_214_L_215))=(INT64)(((INT64)(Main__neg_abs_vs_one_LT_true(
  ( INTEGER )(  INT64_(0) )))));
#line 238 "../Main.m3"
 /* load */
#line 238 "../Main.m3"
 /* if_true_or_false */
#line 238 "../Main.m3"
 /* load_host_integer */
#line 238 "../Main.m3"
 /* load_integer */
#line 238 "../Main.m3"
 /* if_compare */
#line 238 "../Main.m3"
if(m3_ne(INT64,
  Main_m_214_L_215,
   INT64_(0)))goto L48;
#line 238 "../Main.m3"
 /* start_call_direct */
#line 238 "../Main.m3"
 /* load_address */
#line 238 "../Main.m3"
 /* pop_param */
#line 238 "../Main.m3"
 /* load_integer */
#line 238 "../Main.m3"
 /* pop_param */
#line 238 "../Main.m3"
 /* load_address */
#line 238 "../Main.m3"
 /* pop_param */
#line 238 "../Main.m3"
 /* call_direct */
#line 238 "../Main.m3"
RTHooks__AssertFailed(
  ( ADDRESS )(((ADDRESS)(&Main_m_M_Main_L_22)) ),
  ( INTEGER )(  INT64_(238) ),
  ( TEXT )(((ADDRESS)(INT64_(4616)+((ADDRESS)(&Main_m_20_L_21)))) ));
#line 238 "../Main.m3"
 /* set_label */
#line 238 "../Main.m3"
L48:;
#line 238 "../Main.m3"
 /* set_source_line */
#line 238 "../Main.m3"
#line 239 "../Main.m3"
 /* start_call_direct */
#line 239 "../Main.m3"
 /* load_integer */
#line 239 "../Main.m3"
 /* pop_param */
#line 239 "../Main.m3"
 /* call_direct */
#line 239 "../Main.m3"
 /* store */
#line 239 "../Main.m3"
(*(INT64*)(&Main_m_214_L_215))=(INT64)(((INT64)(Main__neg_abs_vs_one_LE_true(
  ( INTEGER )(  INT64_(0) )))));
#line 239 "../Main.m3"
 /* load */
#line 239 "../Main.m3"
 /* if_true_or_false */
#line 239 "../Main.m3"
 /* load_host_integer */
#line 239 "../Main.m3"
 /* load_integer */
#line 239 "../Main.m3"
 /* if_compare */
#line 239 "../Main.m3"
if(m3_ne(INT64,
  Main_m_214_L_215,
   INT64_(0)))goto L49;
#line 239 "../Main.m3"
 /* start_call_direct */
#line 239 "../Main.m3"
 /* load_address */
#line 239 "../Main.m3"
 /* pop_param */
#line 239 "../Main.m3"
 /* load_integer */
#line 239 "../Main.m3"
 /* pop_param */
#line 239 "../Main.m3"
 /* load_address */
#line 239 "../Main.m3"
 /* pop_param */
#line 239 "../Main.m3"
 /* call_direct */
#line 239 "../Main.m3"
RTHooks__AssertFailed(
  ( ADDRESS )(((ADDRESS)(&Main_m_M_Main_L_22)) ),
  ( INTEGER )(  INT64_(239) ),
  ( TEXT )(((ADDRESS)(INT64_(4672)+((ADDRESS)(&Main_m_20_L_21)))) ));
#line 239 "../Main.m3"
 /* set_label */
#line 239 "../Main.m3"
L49:;
#line 239 "../Main.m3"
 /* set_source_line */
#line 239 "../Main.m3"
#line 240 "../Main.m3"
 /* start_call_direct */
#line 240 "../Main.m3"
 /* load_integer */
#line 240 "../Main.m3"
 /* pop_param */
#line 240 "../Main.m3"
 /* call_direct */
#line 240 "../Main.m3"
 /* store */
#line 240 "../Main.m3"
(*(INT64*)(&Main_m_214_L_215))=(INT64)(((INT64)(Main__neg_abs_vs_one_GT_false(
  ( INTEGER )(  INT64_(0) )))));
#line 240 "../Main.m3"
 /* load */
#line 240 "../Main.m3"
 /* if_true_or_false */
#line 240 "../Main.m3"
 /* load_host_integer */
#line 240 "../Main.m3"
 /* load_integer */
#line 240 "../Main.m3"
 /* if_compare */
#line 240 "../Main.m3"
if(m3_eq(INT64,
  Main_m_214_L_215,
   INT64_(0)))goto L4A;
#line 240 "../Main.m3"
 /* start_call_direct */
#line 240 "../Main.m3"
 /* load_address */
#line 240 "../Main.m3"
 /* pop_param */
#line 240 "../Main.m3"
 /* load_integer */
#line 240 "../Main.m3"
 /* pop_param */
#line 240 "../Main.m3"
 /* load_address */
#line 240 "../Main.m3"
 /* pop_param */
#line 240 "../Main.m3"
 /* call_direct */
#line 240 "../Main.m3"
RTHooks__AssertFailed(
  ( ADDRESS )(((ADDRESS)(&Main_m_M_Main_L_22)) ),
  ( INTEGER )(  INT64_(240) ),
  ( TEXT )(((ADDRESS)(INT64_(4728)+((ADDRESS)(&Main_m_20_L_21)))) ));
#line 240 "../Main.m3"
 /* set_label */
#line 240 "../Main.m3"
L4A:;
#line 240 "../Main.m3"
 /* set_source_line */
#line 240 "../Main.m3"
#line 241 "../Main.m3"
 /* start_call_direct */
#line 241 "../Main.m3"
 /* load_integer */
#line 241 "../Main.m3"
 /* pop_param */
#line 241 "../Main.m3"
 /* call_direct */
#line 241 "../Main.m3"
 /* store */
#line 241 "../Main.m3"
(*(INT64*)(&Main_m_214_L_215))=(INT64)(((INT64)(Main__neg_abs_vs_one_GE_false(
  ( INTEGER )(  INT64_(0) )))));
#line 241 "../Main.m3"
 /* load */
#line 241 "../Main.m3"
 /* if_true_or_false */
#line 241 "../Main.m3"
 /* load_host_integer */
#line 241 "../Main.m3"
 /* load_integer */
#line 241 "../Main.m3"
 /* if_compare */
#line 241 "../Main.m3"
if(m3_eq(INT64,
  Main_m_214_L_215,
   INT64_(0)))goto L4B;
#line 241 "../Main.m3"
 /* start_call_direct */
#line 241 "../Main.m3"
 /* load_address */
#line 241 "../Main.m3"
 /* pop_param */
#line 241 "../Main.m3"
 /* load_integer */
#line 241 "../Main.m3"
 /* pop_param */
#line 241 "../Main.m3"
 /* load_address */
#line 241 "../Main.m3"
 /* pop_param */
#line 241 "../Main.m3"
 /* call_direct */
#line 241 "../Main.m3"
RTHooks__AssertFailed(
  ( ADDRESS )(((ADDRESS)(&Main_m_M_Main_L_22)) ),
  ( INTEGER )(  INT64_(241) ),
  ( TEXT )(((ADDRESS)(INT64_(4784)+((ADDRESS)(&Main_m_20_L_21)))) ));
#line 241 "../Main.m3"
 /* set_label */
#line 241 "../Main.m3"
L4B:;
#line 241 "../Main.m3"
 /* set_source_line */
#line 241 "../Main.m3"
#line 242 "../Main.m3"
 /* start_call_direct */
#line 242 "../Main.m3"
 /* load_integer */
#line 242 "../Main.m3"
 /* pop_param */
#line 242 "../Main.m3"
 /* call_direct */
#line 242 "../Main.m3"
 /* store */
#line 242 "../Main.m3"
(*(INT64*)(&Main_m_214_L_215))=(INT64)(((INT64)(Main__neg_abs_vs_one_EQ_false(
  ( INTEGER )(  INT64_(0) )))));
#line 242 "../Main.m3"
 /* load */
#line 242 "../Main.m3"
 /* if_true_or_false */
#line 242 "../Main.m3"
 /* load_host_integer */
#line 242 "../Main.m3"
 /* load_integer */
#line 242 "../Main.m3"
 /* if_compare */
#line 242 "../Main.m3"
if(m3_eq(INT64,
  Main_m_214_L_215,
   INT64_(0)))goto L4C;
#line 242 "../Main.m3"
 /* start_call_direct */
#line 242 "../Main.m3"
 /* load_address */
#line 242 "../Main.m3"
 /* pop_param */
#line 242 "../Main.m3"
 /* load_integer */
#line 242 "../Main.m3"
 /* pop_param */
#line 242 "../Main.m3"
 /* load_address */
#line 242 "../Main.m3"
 /* pop_param */
#line 242 "../Main.m3"
 /* call_direct */
#line 242 "../Main.m3"
RTHooks__AssertFailed(
  ( ADDRESS )(((ADDRESS)(&Main_m_M_Main_L_22)) ),
  ( INTEGER )(  INT64_(242) ),
  ( TEXT )(((ADDRESS)(INT64_(4840)+((ADDRESS)(&Main_m_20_L_21)))) ));
#line 242 "../Main.m3"
 /* set_label */
#line 242 "../Main.m3"
L4C:;
#line 242 "../Main.m3"
 /* set_source_line */
#line 242 "../Main.m3"
#line 243 "../Main.m3"
 /* start_call_direct */
#line 243 "../Main.m3"
 /* load_integer */
#line 243 "../Main.m3"
 /* pop_param */
#line 243 "../Main.m3"
 /* call_direct */
#line 243 "../Main.m3"
 /* store */
#line 243 "../Main.m3"
(*(INT64*)(&Main_m_214_L_215))=(INT64)(((INT64)(Main__neg_abs_vs_one_NE_true(
  ( INTEGER )(  INT64_(0) )))));
#line 243 "../Main.m3"
 /* load */
#line 243 "../Main.m3"
 /* if_true_or_false */
#line 243 "../Main.m3"
 /* load_host_integer */
#line 243 "../Main.m3"
 /* load_integer */
#line 243 "../Main.m3"
 /* if_compare */
#line 243 "../Main.m3"
if(m3_ne(INT64,
  Main_m_214_L_215,
   INT64_(0)))goto L4D;
#line 243 "../Main.m3"
 /* start_call_direct */
#line 243 "../Main.m3"
 /* load_address */
#line 243 "../Main.m3"
 /* pop_param */
#line 243 "../Main.m3"
 /* load_integer */
#line 243 "../Main.m3"
 /* pop_param */
#line 243 "../Main.m3"
 /* load_address */
#line 243 "../Main.m3"
 /* pop_param */
#line 243 "../Main.m3"
 /* call_direct */
#line 243 "../Main.m3"
RTHooks__AssertFailed(
  ( ADDRESS )(((ADDRESS)(&Main_m_M_Main_L_22)) ),
  ( INTEGER )(  INT64_(243) ),
  ( TEXT )(((ADDRESS)(INT64_(4896)+((ADDRESS)(&Main_m_20_L_21)))) ));
#line 243 "../Main.m3"
 /* set_label */
#line 243 "../Main.m3"
L4D:;
#line 243 "../Main.m3"
 /* set_label */
#line 243 "../Main.m3"
L1:;
#line 243 "../Main.m3"
 /* load_address */
#line 243 "../Main.m3"
 /* exit_proc */
#line 243 "../Main.m3"
return (RT0__ModulePtr)(&Main_m_M_Main_L_22);
#line 243 "../Main.m3"
 /* end_procedure */
#line 243 "../Main.m3"
} /* global constant type descriptor */
#line 243 "../Main.m3"
 /* global data type descriptor */
#line 243 "../Main.m3"
 /* module global constants */
#line 243 "../Main.m3"
 /* procedure names */
#line 243 "../Main.m3"
 /* procedure table */
#line 243 "../Main.m3"
 /* file name */
#line 243 "../Main.m3"
 /* module global data */
#line 243 "../Main.m3"
 /* load map


 global data allocation for M_Main
     0   104  8  *module info*
   104    24  8  import Main
   128    24  8  import RTHooks
   152     0  8  *TOTAL*


 global constants for M_Main
     0    40  8  TEXT literal methods
    40    50  8  *TEXT literal*
    96    45  8  *TEXT literal*
   144    53  8  *TEXT literal*
   200    53  8  *TEXT literal*
   256    48  8  *TEXT literal*
   304    48  8  *TEXT literal*
   352    48  8  *TEXT literal*
   400    53  8  *TEXT literal*
   456    53  8  *TEXT literal*
   512    53  8  *TEXT literal*
   568    48  8  *TEXT literal*
   616    48  8  *TEXT literal*
   664    48  8  *TEXT literal*
   712    53  8  *TEXT literal*
   768    51  8  *TEXT literal*
   824    46  8  *TEXT literal*
   872    54  8  *TEXT literal*
   928    54  8  *TEXT literal*
   984    59  8  *TEXT literal*
  1048    59  8  *TEXT literal*
  1112    59  8  *TEXT literal*
  1176    54  8  *TEXT literal*
  1232    59  8  *TEXT literal*
  1296    64  8  *TEXT literal*
  1360    62  8  *TEXT literal*
  1424    62  8  *TEXT literal*
  1488    57  8  *TEXT literal*
  1552    57  8  *TEXT literal*
  1616    62  8  *TEXT literal*
  1680    57  8  *TEXT literal*
  1744    67  8  *TEXT literal*
  1816    62  8  *TEXT literal*
  1880    80  8  *TEXT literal*
  1960    80  8  *TEXT literal*
  2040    85  8  *TEXT literal*
  2128    85  8  *TEXT literal*
  2216    85  8  *TEXT literal*
  2304    80  8  *TEXT literal*
  2384    84  8  *TEXT literal*
  2472    89  8  *TEXT literal*
  2568    88  8  *TEXT literal*
  2656    88  8  *TEXT literal*
  2744    83  8  *TEXT literal*
  2832    83  8  *TEXT literal*
  2920    88  8  *TEXT literal*
  3008    83  8  *TEXT literal*
  3096    92  8  *TEXT literal*
  3192    87  8  *TEXT literal*
  3280    53  8  *TEXT literal*
  3336    48  8  *TEXT literal*
  3384    53  8  *TEXT literal*
  3440    48  8  *TEXT literal*
  3488    48  8  *TEXT literal*
  3536    53  8  *TEXT literal*
  3592    72  8  *TEXT literal*
  3664    72  8  *TEXT literal*
  3736    67  8  *TEXT literal*
  3808    67  8  *TEXT literal*
  3880    72  8  *TEXT literal*
  3952    67  8  *TEXT literal*
  4024    57  8  *TEXT literal*
  4088    57  8  *TEXT literal*
  4152    52  8  *TEXT literal*
  4208    52  8  *TEXT literal*
  4264    57  8  *TEXT literal*
  4328    52  8  *TEXT literal*
  4384    53  8  *TEXT literal*
  4440    48  8  *TEXT literal*
  4488    52  8  *TEXT literal*
  4544    57  8  *TEXT literal*
  4608    51  8  *TEXT literal*
  4664    51  8  *TEXT literal*
  4720    56  8  *TEXT literal*
  4776    56  8  *TEXT literal*
  4832    56  8  *TEXT literal*
  4888    51  8  *TEXT literal*
  4944  1942  8  *proc names*
  6888  1240  8  *proc info*
  8128    11  1  *string*
  8144     0  8  *TOTAL*
 */
#line 243 "../Main.m3"
 /* end unit */
#line 243 "../Main.m3"

#ifdef __cplusplus

} /* extern "C" */
#endif
 /* set_runtime_proc */
 /* set_runtime_proc */
 /* set_runtime_proc */
