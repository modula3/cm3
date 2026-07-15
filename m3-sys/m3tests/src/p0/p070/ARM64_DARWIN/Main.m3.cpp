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
 /* declare_subrange */
/*subrange_define*/typedef INT32 TADC6066D_32;
 /* declare_packed */
typedef TADC6066D_32 TE61D104F;
 /* declare_subrange */
/*subrange_define*/typedef UINT8 T8B2831D7_8;
 /* declare_array */
/*array_forwardDeclare*/struct T8C665621;typedef struct T8C665621 T8C665621;

#ifndef T8C665621
#define T8C665621 T8C665621
/*fixedArray_define*/struct T8C665621{TE61D104F _elts[2];};
#endif
 /* declare_proctype */

#if 0 /* avoid type hash collions */
typedef 
void(__cdecl*T7B78C34F)(void);
#else
typedef void (__cdecl*T7B78C34F)(void);
#endif
 /* declare_subrange */
/*subrange_define*/typedef UINT8 T8D2831D7_8;
 /* declare_subrange */
/*subrange_define*/typedef UINT8 TB5B30AA_8;
 /* declare_packed */
typedef TB5B30AA_8 TE3FC5CB;
 /* declare_array */
/*array_forwardDeclare*/struct T386903F9;typedef struct T386903F9 T386903F9;

#ifndef T386903F9
#define T386903F9 T386903F9
/*fixedArray_define*/struct T386903F9{TE3FC5CB _elts[8];};
#endif
 /* declare_record */
 /* declare_field */
 /* record_forwardDeclare Record_t{ typeid:T26466202 text:NIL hash_text:T26466202 base_text:NIL state:0} */
/*record_forwardDeclare*/struct T26466202;typedef struct T26466202 T26466202;
 /* record_canBeDefined Record_t{ typeid:T26466202 text:NIL hash_text:T26466202 base_text:NIL state:0} */
 /* record_define Record_t{ typeid:T26466202 text:NIL hash_text:T26466202 base_text:NIL state:0} */

#ifndef T26466202
#define T26466202 T26466202
/*record_define*/struct T26466202{
T386903F9 byte;
};
#endif
 /* declare_proctype */

#if 0 /* avoid type hash collions */
typedef 
WORD_T(__cdecl*T5C4C299E)(void);
#else
typedef void (__cdecl*T5C4C299E)(void);
#endif
 /* declare_opaque */

#ifndef T62761487
#define T62761487 T62761487
/*1addressType_define*/typedef ADDRESS T62761487;

#endif
 /* declare_proctype */
 /* declare_formal */
 /* declare_subrange */
/*subrange_define*/typedef UINT8 T2AA4581F_8;
 /* declare_proctype */
 /* declare_formal */
 /* declare_formal */

#ifndef Fmt__Base
#define Fmt__Base Fmt__Base
typedef T2AA4581F_8 Fmt__Base;
#endif
 /* declare_proctype */
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

#ifndef Wr__T
#define Wr__T Wr__T
typedef T62761487 Wr__T;
#endif
 /* declare_formal */
 /* declare_raises */
 /* declare_raises */
 /* declare_proctype */
 /* declare_formal */

#ifndef RTProcedure__Proc
#define RTProcedure__Proc RTProcedure__Proc
typedef ADDRESS RTProcedure__Proc;
#endif
 /* declare_indirect */
typedef T26466202*TD9B99DFD;
 /* declare_pointer */
typedef T26466202*TCFB2A08F;
 /* declare_proctype */
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
 /* DeclareTypes_FlushOnce size:12 */

#if 0 /* avoid type hash collions */
typedef 
void(__cdecl*T37E50548)(REFANY);
#else
typedef void (__cdecl*T37E50548)(void);
#endif

#if 0 /* avoid type hash collions */
typedef 
TEXT(__cdecl*TF2A35A9D)(INTEGER,Fmt__Base);
#else
typedef void (__cdecl*TF2A35A9D)(void);
#endif

#if 0 /* avoid type hash collions */
typedef 
TEXT(__cdecl*T97F166D3)(TEXT,TEXT);
#else
typedef void (__cdecl*T97F166D3)(void);
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
void(__cdecl*T1BEE6E85)(Wr__T,TEXT);
#else
typedef void (__cdecl*T1BEE6E85)(void);
#endif

#if 0 /* avoid type hash collions */
typedef 
void(__cdecl*TBF9A4697)(RTProcedure__Proc);
#else
typedef void (__cdecl*TBF9A4697)(void);
#endif

#if 0 /* avoid type hash collions */
typedef 
ADDRESS(__cdecl*TC82056D3)(T26466202*);
#else
typedef void (__cdecl*TC82056D3)(void);
#endif

#if 0 /* avoid type hash collions */
typedef 
void(__cdecl*T8826887A)(BOOLEAN,BOOLEAN);
#else
typedef void (__cdecl*T8826887A)(void);
#endif
 /* DeclareTypes_FlushOnce size:0 */
 /* end: DeclareTypes */
 /* begin: helper functions */
#define m3_extract(T, value, offset, count) ((((T)(value))>>((WORD_T)(offset)))&~(((~(T)0))<<((WORD_T)(count))))
 /* end: helper functions */

#ifndef struct_8_t
#define struct_8_t struct_8_t
STRUCT8(8)
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
/*Proc_ForwardDeclareFrameType*/struct Test_I3_Frame_t;typedef struct Test_I3_Frame_t Test_I3_Frame_t;
 /* internal_declare_param */
RT0__ModulePtr
__cdecl
Test_I3(
   /* Param_Type1 */ INTEGER /*TypeText1*/  mode_L_2);
 /* import_procedure */
/*Proc_ForwardDeclareFrameType*/struct Fmt_I3_Frame_t;typedef struct Fmt_I3_Frame_t Fmt_I3_Frame_t;
 /* internal_declare_param */
RT0__ModulePtr
__cdecl
Fmt_I3(
   /* Param_Type1 */ INTEGER /*TypeText1*/  mode_L_3);
 /* import_procedure */
/*Proc_ForwardDeclareFrameType*/struct Stdio_I3_Frame_t;typedef struct Stdio_I3_Frame_t Stdio_I3_Frame_t;
 /* internal_declare_param */
RT0__ModulePtr
__cdecl
Stdio_I3(
   /* Param_Type1 */ INTEGER /*TypeText1*/  mode_L_4);
 /* import_procedure */
/*Proc_ForwardDeclareFrameType*/struct Wr_I3_Frame_t;typedef struct Wr_I3_Frame_t Wr_I3_Frame_t;
 /* internal_declare_param */
RT0__ModulePtr
__cdecl
Wr_I3(
   /* Param_Type1 */ INTEGER /*TypeText1*/  mode_L_5);
 /* import_procedure */
/*Proc_ForwardDeclareFrameType*/struct RTProcedureSRC_I3_Frame_t;typedef struct RTProcedureSRC_I3_Frame_t RTProcedureSRC_I3_Frame_t;
 /* internal_declare_param */
RT0__ModulePtr
__cdecl
RTProcedureSRC_I3(
   /* Param_Type1 */ INTEGER /*TypeText1*/  mode_L_6);
 /* import_procedure */
/*Proc_ForwardDeclareFrameType*/struct RTProcedure_I3_Frame_t;typedef struct RTProcedure_I3_Frame_t RTProcedure_I3_Frame_t;
 /* internal_declare_param */
RT0__ModulePtr
__cdecl
RTProcedure_I3(
   /* Param_Type1 */ INTEGER /*TypeText1*/  mode_L_7);
 /* import_procedure */
/*Proc_ForwardDeclareFrameType*/struct Fingerprint_I3_Frame_t;typedef struct Fingerprint_I3_Frame_t Fingerprint_I3_Frame_t;
 /* internal_declare_param */
RT0__ModulePtr
__cdecl
Fingerprint_I3(
   /* Param_Type1 */ INTEGER /*TypeText1*/  mode_L_8);
 /* import_procedure */
/*Proc_ForwardDeclareFrameType*/struct RTHooks_I3_Frame_t;typedef struct RTHooks_I3_Frame_t RTHooks_I3_Frame_t;
 /* internal_declare_param */
RT0__ModulePtr
__cdecl
RTHooks_I3(
   /* Param_Type1 */ INTEGER /*TypeText1*/  mode_L_9);
 /* import_procedure */
/*Proc_ForwardDeclareFrameType*/struct RTProcedureSRC__NumProcedures_Frame_t;typedef struct RTProcedureSRC__NumProcedures_Frame_t RTProcedureSRC__NumProcedures_Frame_t;
CARDINAL
__cdecl
RTProcedureSRC__NumProcedures(void);
 /* import_procedure */
/*Proc_ForwardDeclareFrameType*/struct RTHooks__CheckLoadTracedRef_Frame_t;typedef struct RTHooks__CheckLoadTracedRef_Frame_t RTHooks__CheckLoadTracedRef_Frame_t;
 /* internal_declare_param */
void /*TypeText3*/ 
__cdecl
RTHooks__CheckLoadTracedRef(
   /* Param_Type1 */ REFANY ref_L_10);
 /* import_procedure */
/*Proc_ForwardDeclareFrameType*/struct Fmt__Int_Frame_t;typedef struct Fmt__Int_Frame_t Fmt__Int_Frame_t;
 /* internal_declare_param */
 /* internal_declare_param */
TEXT
__cdecl
Fmt__Int(
   /* Param_Type1 */ INTEGER n_L_11,
   /* Param_Type1 */ Fmt__Base base_L_12);
 /* import_procedure */
/*Proc_ForwardDeclareFrameType*/struct RTHooks__Concat_Frame_t;typedef struct RTHooks__Concat_Frame_t RTHooks__Concat_Frame_t;
 /* internal_declare_param */
 /* internal_declare_param */
TEXT
__cdecl
RTHooks__Concat(
   /* Param_Type1 */ TEXT a_L_13,
   /* Param_Type1 */ TEXT b_L_14);
 /* import_procedure */
/*Proc_ForwardDeclareFrameType*/struct RTHooks__TextLitInfo_Frame_t;typedef struct RTHooks__TextLitInfo_Frame_t RTHooks__TextLitInfo_Frame_t;
 /* internal_declare_param */
 /* internal_declare_param */
void /*TypeText3*/ 
__cdecl
RTHooks__TextLitInfo(
   /* Param_Type1 */ RTHooks__TextLiteral t_L_15,
   /* Param_Type1 */ RTHooks__TextInfo* /*TypeText1*/  i_L_16);
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
   /* Param_Type1 */ RTHooks__TextLiteral t_L_17,
   /* Param_Type1 */ CARDINAL i_L_18);
 /* import_procedure */
/*Proc_ForwardDeclareFrameType*/struct RTHooks__TextLitGetWideChar_Frame_t;typedef struct RTHooks__TextLitGetWideChar_Frame_t RTHooks__TextLitGetWideChar_Frame_t;
 /* internal_declare_param */
 /* internal_declare_param */
WIDECHAR
__cdecl
RTHooks__TextLitGetWideChar(
   /* Param_Type1 */ RTHooks__TextLiteral t_L_19,
   /* Param_Type1 */ CARDINAL i_L_20);
 /* import_procedure */
/*Proc_ForwardDeclareFrameType*/struct RTHooks__TextLitGetChars_Frame_t;typedef struct RTHooks__TextLitGetChars_Frame_t RTHooks__TextLitGetChars_Frame_t;
 /* internal_declare_param */
 /* internal_declare_param */
 /* internal_declare_param */
void /*TypeText3*/ 
__cdecl
RTHooks__TextLitGetChars(
   /* Param_Type1 */ RTHooks__TextLiteral t_L_21,
   /* Param_Type1 */ T89CD34BD* /*TypeText1*/  a_L_22,
   /* Param_Type1 */ CARDINAL start_L_23);
 /* import_procedure */
/*Proc_ForwardDeclareFrameType*/struct RTHooks__TextLitGetWideChars_Frame_t;typedef struct RTHooks__TextLitGetWideChars_Frame_t RTHooks__TextLitGetWideChars_Frame_t;
 /* internal_declare_param */
 /* internal_declare_param */
 /* internal_declare_param */
void /*TypeText3*/ 
__cdecl
RTHooks__TextLitGetWideChars(
   /* Param_Type1 */ RTHooks__TextLiteral t_L_24,
   /* Param_Type1 */ TA19BDC21* /*TypeText1*/  a_L_25,
   /* Param_Type1 */ CARDINAL start_L_26);
 /* import_procedure */
/*Proc_ForwardDeclareFrameType*/struct Wr__PutText_Frame_t;typedef struct Wr__PutText_Frame_t Wr__PutText_Frame_t;
 /* internal_declare_param */
 /* internal_declare_param */
void /*TypeText3*/ 
__cdecl
Wr__PutText(
   /* Param_Type1 */ Wr__T wr_L_27,
   /* Param_Type1 */ TEXT t_L_28);
 /* import_procedure */
/*Proc_ForwardDeclareFrameType*/struct RTProcedure__ToFingerprint_Frame_t;typedef struct RTProcedure__ToFingerprint_Frame_t RTProcedure__ToFingerprint_Frame_t;
 /* internal_declare_param */
 /* internal_declare_param */
void /*TypeText3*/ 
__cdecl
RTProcedure__ToFingerprint(
   /* Param_Type1 */ T26466202* /*TypeText1*/  _return_L_29,
   /* Param_Type1 */ RTProcedure__Proc p_L_30);
 /* import_procedure */
/*Proc_ForwardDeclareFrameType*/struct RTProcedure__FromFingerprint_Frame_t;typedef struct RTProcedure__FromFingerprint_Frame_t RTProcedure__FromFingerprint_Frame_t;
 /* internal_declare_param */
RTProcedure__Proc
__cdecl
RTProcedure__FromFingerprint(
   /* Param_Type1 */ T26466202* /*TypeText1*/  fp_L_31);
 /* import_procedure */
/*Proc_ForwardDeclareFrameType*/struct Test__checkB_Frame_t;typedef struct Test__checkB_Frame_t Test__checkB_Frame_t;
 /* internal_declare_param */
 /* internal_declare_param */
void /*TypeText3*/ 
__cdecl
Test__checkB(
   /* Param_Type1 */ BOOLEAN b_L_32,
   /* Param_Type1 */ BOOLEAN shouldBe_L_33);
 /* import_procedure */
/*Proc_ForwardDeclareFrameType*/struct Test__done_Frame_t;typedef struct Test__done_Frame_t Test__done_Frame_t;
void /*TypeText3*/ 
__cdecl
Test__done(void);
 /* end: imports */
 /* begin: locals */
 /* declare_segment name:<NIL> typeid:TFFFFFFFF const:TRUE */
/*declare_segment*/struct Main_m_34_L_35_t;
/*declare_segment*/typedef struct Main_m_34_L_35_t Main_m_34_L_35_t;
 /* declare_segment name:M_Main typeid:TFFFFFFFF const:FALSE */
 /* handler_name_prefixes:Main_M3_LINE_ */
 /* handler_name_prefixes:Main_I3_LINE_ */
/*declare_segment*/struct Main_m_M_Main_L_36_t;
/*declare_segment*/typedef struct Main_m_M_Main_L_36_t Main_m_M_Main_L_36_t;
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct Main_M3_Frame_t;typedef struct Main_M3_Frame_t Main_M3_Frame_t;
 /* internal_declare_param */
RT0__ModulePtr
__cdecl
Main_M3(
   /* Param_Type1 */ INTEGER /*TypeText1*/  mode_L_37);
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct Main__Toto_Frame_t;typedef struct Main__Toto_Frame_t Main__Toto_Frame_t;
void /*TypeText3*/ 
__cdecl
Main__Toto(void);
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
 /* init_chars */
 /* init_chars */
 /* init_proc */
 /* init_var */
 /* init_proc */
 /* init_var */
 /* init_chars */
 /* end_init */
struct Main_m_34_L_35_t{ADDRESS L_56[5];
INT64 L_57[1];
ADDRESS L_58[1];
INT64 L_59[1];
UINT8 L_60[12];
char L_61[4];
INT64 L_62[1];
ADDRESS L_63[1];
INT64 L_64[1];
UINT8 L_65[20];
char L_66[4];
INT64 L_67[1];
ADDRESS L_68[1];
INT64 L_69[1];
UINT8 L_70[2];
char L_71[6];
INT64 L_72[1];
ADDRESS L_73[1];
INT64 L_74[1];
UINT8 L_75[2];
char L_76[6];
UINT8 L_77[7];
char L_78[1];
UINT8 L_79[4];
char L_80[4];
ADDRESS L_81[4];
char L_82[8];
UINT8 L_83[10];
char L_84[14];
};
static  const Main_m_34_L_35_t Main_m_34_L_35={{(ADDRESS)&RTHooks__TextLitInfo,(ADDRESS)&RTHooks__TextLitGetChar,(ADDRESS)&RTHooks__TextLitGetWideChar,(ADDRESS)&RTHooks__TextLitGetChars,(ADDRESS)&RTHooks__TextLitGetWideChars},{INT64_(2)},{(char*)&Main_m_34_L_35},{INT64_(12)},{' ','p','r','o','c','e','d','u','r','e','s',10},{0 /* 1 */ ,0 /* 2 */ ,0 /* 3 */ ,0 /* 4 */ ,},{INT64_(2)},{(char*)&Main_m_34_L_35},{INT64_(20)},{'T','o','t','o',' ','f','i','n','g','e','r','p','r','i','n','t',' ','=',' ','{'},{0 /* 1 */ ,0 /* 2 */ ,0 /* 3 */ ,0 /* 4 */ ,},{INT64_(2)},{(char*)&Main_m_34_L_35},{INT64_(2)},{',',' '},{0 /* 1 */ ,0 /* 2 */ ,0 /* 3 */ ,0 /* 4 */ ,0 /* 5 */ ,0 /* 6 */ ,},{INT64_(2)},{(char*)&Main_m_34_L_35},{INT64_(2)},{'}',10},{0 /* 1 */ ,0 /* 2 */ ,0 /* 3 */ ,0 /* 4 */ ,0 /* 5 */ ,0 /* 6 */ ,},{'M','a','i','n','_','M','3'},{0 /* 1 */ ,},{'T','o','t','o'},{0 /* 1 */ ,0 /* 2 */ ,0 /* 3 */ ,0 /* 4 */ ,},{(ADDRESS)&Main_M3,192+(char*)&Main_m_34_L_35,(ADDRESS)&Main__Toto,200+(char*)&Main_m_34_L_35
},{0 /* 1 */ ,0 /* 2 */ ,0 /* 3 */ ,0 /* 4 */ ,0 /* 5 */ ,0 /* 6 */ ,0 /* 7 */ ,0 /* 8 */ ,},{'.','.','/','M','a','i','n','.','m','3'},{0 /* 1 */ ,0 /* 2 */ ,0 /* 3 */ ,0 /* 4 */ ,0 /* 5 */ ,0 /* 6 */ ,0 /* 7 */ ,0 /* 8 */ ,0 /* 9 */ ,0 /* 10 */ ,0 /* 11 */ ,0 /* 12 */ ,0 /* 13 */ ,0 /* 14 */ ,}};
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
 /* end_init */
struct Main_m_M_Main_L_36_t{ADDRESS L_85[1];
char L_86[32];
ADDRESS L_87[1];
char L_88[24];
ADDRESS L_89[1];
char L_90[8];
ADDRESS L_91[1];
INT64 L_92[1];
char L_93[40];
ADDRESS L_94[2];
char L_95[8];
ADDRESS L_96[2];
char L_97[8];
ADDRESS L_98[2];
char L_99[8];
ADDRESS L_100[2];
char L_101[8];
ADDRESS L_102[2];
char L_103[8];
ADDRESS L_104[2];
char L_105[8];
ADDRESS L_106[2];
char L_107[8];
ADDRESS L_108[2];
char L_109[8];
ADDRESS L_110[1];
char L_111[8];
};
static Main_m_M_Main_L_36_t Main_m_M_Main_L_36={{248+(char*)&Main_m_34_L_35},{0 /* 1 */ ,0 /* 2 */ ,0 /* 3 */ ,0 /* 4 */ ,0 /* 5 */ ,0 /* 6 */ ,0 /* 7 */ ,0 /* 8 */ ,0 /* 9 */ ,0 /* 10 */ ,0 /* 11 */ ,0 /* 12 */ ,0 /* 13 */ ,0 /* 14 */ ,0 /* 15 */ ,0 /* 16 */ ,0 /* 17 */ ,0 /* 18 */ ,0 /* 19 */ ,0 /* 20 */ ,0 /* 21 */ ,0 /* 22 */ ,0 /* 23 */ ,0 /* 24 */ ,0 /* 25 */ ,0 /* 26 */ ,0 /* 27 */ ,0 /* 28 */ ,0 /* 29 */ ,0 /* 30 */ ,0 /* 31 */ ,0 /* 32 */ ,},{208+(char*)&Main_m_34_L_35},{0 /* 1 */ ,0 /* 2 */ ,0 /* 3 */ ,0 /* 4 */ ,0 /* 5 */ ,0 /* 6 */ ,0 /* 7 */ ,0 /* 8 */ ,0 /* 9 */ ,0 /* 10 */ ,0 /* 11 */ ,0 /* 12 */ ,0 /* 13 */ ,0 /* 14 */ ,0 /* 15 */ ,0 /* 16 */ ,0 /* 17 */ ,0 /* 18 */ ,0 /* 19 */ ,0 /* 20 */ ,0 /* 21 */ ,0 /* 22 */ ,0 /* 23 */ ,0 /* 24 */ ,},{136+(char*)&Main_m_M_Main_L_36},{0 /* 1 */ ,0 /* 2 */ ,0 /* 3 */ ,0 /* 4 */ ,0 /* 5 */ ,0 /* 6 */ ,0 /* 7 */ ,0 /* 8 */ ,},{(ADDRESS)&Main_M3},{INT64_(3)},{0 /* 1 */ ,0 /* 2 */ ,0 /* 3 */ ,0 /* 4 */ ,0 /* 5 */ ,0 /* 6 */ ,0 /* 7 */ ,
0 /* 8 */ ,0 /* 9 */ ,0 /* 10 */ ,0 /* 11 */ ,0 /* 12 */ ,0 /* 13 */ ,0 /* 14 */ ,0 /* 15 */ ,0 /* 16 */ ,0 /* 17 */ ,0 /* 18 */ ,0 /* 19 */ ,0 /* 20 */ ,0 /* 21 */ ,0 /* 22 */ ,0 /* 23 */ ,0 /* 24 */ ,0 /* 25 */ ,0 /* 26 */ ,0 /* 27 */ ,0 /* 28 */ ,0 /* 29 */ ,0 /* 30 */ ,0 /* 31 */ ,0 /* 32 */ ,0 /* 33 */ ,0 /* 34 */ ,0 /* 35 */ ,0 /* 36 */ ,0 /* 37 */ ,0 /* 38 */ ,0 /* 39 */ ,0 /* 40 */ ,},{(ADDRESS)&Main_I3,160+(char*)&Main_m_M_Main_L_36},{0 /* 1 */ ,0 /* 2 */ ,0 /* 3 */ ,0 /* 4 */ ,0 /* 5 */ ,0 /* 6 */ ,0 /* 7 */ ,0 /* 8 */ ,},{(ADDRESS)&Test_I3,184+(char*)&Main_m_M_Main_L_36},{0 /* 1 */ ,0 /* 2 */ ,0 /* 3 */ ,0 /* 4 */ ,0 /* 5 */ ,0 /* 6 */ ,0 /* 7 */ ,0 /* 8 */ ,},{(ADDRESS)&Fmt_I3,208+(char*)&Main_m_M_Main_L_36},{0 /* 1 */ ,0 /* 2 */ ,0 /* 3 */ ,0 /* 4 */ ,0 /* 5 */ ,0 /* 6 */ ,0 /* 7 */ ,0 /* 8 */ ,},{(ADDRESS)&Stdio_I3,232+(char*)&Main_m_M_Main_L_36},{0 /* 1 */ ,0 /* 2 */ ,0 /* 3 */ ,0 /* 4 */ ,0 /* 5 */ ,0 /* 6 */ ,0 /* 7 */ ,0 /* 8 */ ,},{(ADDRESS)&Wr_I3,256+(char*)&Main_m_M_Main_L_36
},{0 /* 1 */ ,0 /* 2 */ ,0 /* 3 */ ,0 /* 4 */ ,0 /* 5 */ ,0 /* 6 */ ,0 /* 7 */ ,0 /* 8 */ ,},{(ADDRESS)&RTProcedureSRC_I3,280+(char*)&Main_m_M_Main_L_36},{0 /* 1 */ ,0 /* 2 */ ,0 /* 3 */ ,0 /* 4 */ ,0 /* 5 */ ,0 /* 6 */ ,0 /* 7 */ ,0 /* 8 */ ,},{(ADDRESS)&RTProcedure_I3,304+(char*)&Main_m_M_Main_L_36},{0 /* 1 */ ,0 /* 2 */ ,0 /* 3 */ ,0 /* 4 */ ,0 /* 5 */ ,0 /* 6 */ ,0 /* 7 */ ,0 /* 8 */ ,},{(ADDRESS)&Fingerprint_I3,328+(char*)&Main_m_M_Main_L_36},{0 /* 1 */ ,0 /* 2 */ ,0 /* 3 */ ,0 /* 4 */ ,0 /* 5 */ ,0 /* 6 */ ,0 /* 7 */ ,0 /* 8 */ ,},{(ADDRESS)&RTHooks_I3},{0 /* 1 */ ,0 /* 2 */ ,0 /* 3 */ ,0 /* 4 */ ,0 /* 5 */ ,0 /* 6 */ ,0 /* 7 */ ,0 /* 8 */ ,}};
 /* end: segments/globals */
 /* begin: mark used */
 /* end: mark used */
 /* set_source_file */
 /* set_source_line */
#line 10 "../Main.m3"
 /* module global constants */
#line 10 "../Main.m3"
 /* module global data */
#line 10 "../Main.m3"
 /* set_source_line */
#line 10 "../Main.m3"
#line 26 "../Main.m3"
 /* Toto */
#line 26 "../Main.m3"
 /* set_source_line */
#line 26 "../Main.m3"
#line 20 "../Main.m3"
 /* begin_procedure */
#line 20 "../Main.m3"
struct Main__Toto_Frame_t {
#line 20 "../Main.m3"
ADDRESS _unused;
#line 20 "../Main.m3"
};
#line 20 "../Main.m3"
void /*TypeText3*/ 
__cdecl
Main__Toto(void)
{
#line 20 "../Main.m3"
Main__Toto_Frame_t _frame;
#line 20 "../Main.m3"
_frame._unused=(ADDRESS)&_frame;
#line 20 "../Main.m3"
 /* exit_proc */
#line 20 "../Main.m3"
return;
#line 20 "../Main.m3"
 /* end_procedure */
#line 20 "../Main.m3"
} /* Main_M3 */
#line 20 "../Main.m3"
 /* module main body Main_M3 */
#line 20 "../Main.m3"
 /* set_source_line */
#line 20 "../Main.m3"
#line 26 "../Main.m3"
 /* begin_procedure */
#line 26 "../Main.m3"
struct Main_M3_Frame_t {
#line 26 "../Main.m3"
ADDRESS _unused;
#line 26 "../Main.m3"
};
#line 26 "../Main.m3"
RT0__ModulePtr
__cdecl
Main_M3(
   /* Param_Type1 */ INTEGER /*TypeText1*/  mode_L_37)
{
#line 26 "../Main.m3"
 /* Var_Type2 */ INT64 Main_m_38_L_39={0};//always-init
#line 26 "../Main.m3"
 /* Var_Type2 */ ADDRESS Main_m_40_L_41={0};//always-init
#line 26 "../Main.m3"
 /* Var_Type2 */ ADDRESS Main_m_42_L_43={0};//always-init
#line 26 "../Main.m3"
 /* Var_Type2 */ ADDRESS Main_m_44_L_45={0};//always-init
#line 26 "../Main.m3"
 /* Var_Type3 */ STRUCT(8) Main_m_46_L_47={0};//always-init
#line 26 "../Main.m3"
 /* Var_Type2 */ ADDRESS Main_m_48_L_49={0};//always-init
#line 26 "../Main.m3"
 /* Var_Type2 */ ADDRESS Main_m_50_L_51={0};//always-init
#line 26 "../Main.m3"
 /* Var_Type2 */ ADDRESS Main_m_52_L_53={0};//always-init
#line 26 "../Main.m3"
 /* Var_Type2 */ ADDRESS Main_m_54_L_55={0};//always-init
#line 26 "../Main.m3"
Main_M3_Frame_t _frame;
#line 26 "../Main.m3"
_frame._unused=(ADDRESS)&_frame;
#line 26 "../Main.m3"
 /* load */
#line 26 "../Main.m3"
 /* if_true_or_false */
#line 26 "../Main.m3"
 /* load_host_integer */
#line 26 "../Main.m3"
 /* load_integer */
#line 26 "../Main.m3"
 /* if_compare */
#line 26 "../Main.m3"
if(m3_eq(INT64,
  mode_L_37,
   INT64_(0)))goto L1;
#line 26 "../Main.m3"
 /* set_source_line */
#line 26 "../Main.m3"
#line 18 "../Main.m3"
 /* start_call_direct */
#line 18 "../Main.m3"
 /* call_direct */
#line 18 "../Main.m3"
 /* store */
#line 18 "../Main.m3"
(*(INT64*)(&Main_m_38_L_39))=(INT64)(((INT64)(RTProcedureSRC__NumProcedures(
 ))));
#line 18 "../Main.m3"
 /* load */
#line 18 "../Main.m3"
 /* store */
#line 18 "../Main.m3"
(*(UINT64*)((104)+(char*)(&Main_m_M_Main_L_36)))=(INT64)( Main_m_38_L_39);
#line 18 "../Main.m3"
 /* set_source_line */
#line 18 "../Main.m3"
#line 27 "../Main.m3"
 /* load */
#line 27 "../Main.m3"
 /* load_indirect */
#line 27 "../Main.m3"
 /* store */
#line 27 "../Main.m3"
(*(ADDRESS*)(&Main_m_40_L_41))=(ADDRESS)(((ADDRESS)(*((ADDRESS*)(INT64_(120)+((ADDRESS)(*((ADDRESS*)(INT64_(208)+((ADDRESS)(&Main_m_M_Main_L_36)))))))))));
#line 27 "../Main.m3"
 /* load_nil */
#line 27 "../Main.m3"
 /* load */
#line 27 "../Main.m3"
 /* if_compare */
#line 27 "../Main.m3"
if(m3_eq(ADDRESS,
 ((ADDRESS)(0)),
 ((ADDRESS)(Main_m_40_L_41))))goto L2;
#line 27 "../Main.m3"
 /* load */
#line 27 "../Main.m3"
 /* loophole */
#line 27 "../Main.m3"
 /* load_integer */
#line 27 "../Main.m3"
 /* and */
#line 27 "../Main.m3"
 /* if_true_or_false */
#line 27 "../Main.m3"
 /* load_host_integer */
#line 27 "../Main.m3"
 /* load_integer */
#line 27 "../Main.m3"
 /* if_compare */
#line 27 "../Main.m3"
if(m3_ne(INT64,
  ((INT64)(((INT64)((INT64)Main_m_40_L_41))&  INT64_(1))),
   INT64_(0)))goto L2;
#line 27 "../Main.m3"
 /* load */
#line 27 "../Main.m3"
 /* load_indirect */
#line 27 "../Main.m3"
 /* extract_mn */
#line 27 "../Main.m3"
 /* load_host_integer */
#line 27 "../Main.m3"
 /* load_integer */
#line 27 "../Main.m3"
 /* load_host_integer */
#line 27 "../Main.m3"
 /* load_integer */
#line 27 "../Main.m3"
 /* extract */
#line 27 "../Main.m3"
 /* if_true_or_false */
#line 27 "../Main.m3"
 /* load_host_integer */
#line 27 "../Main.m3"
 /* load_integer */
#line 27 "../Main.m3"
 /* if_compare */
#line 27 "../Main.m3"
if(m3_eq(INT64,
 ((INT64)(m3_extract(
 UINT64,
  *((INT64*)(INT64_(-8)+((ADDRESS)(Main_m_40_L_41)))),
   UINT64_(22),
   UINT64_(1)))),
   INT64_(0)))goto L2;
#line 27 "../Main.m3"
 /* start_call_direct */
#line 27 "../Main.m3"
 /* load */
#line 27 "../Main.m3"
 /* pop_param */
#line 27 "../Main.m3"
 /* call_direct */
#line 27 "../Main.m3"
RTHooks__CheckLoadTracedRef(
  ( REFANY )(((ADDRESS)(Main_m_40_L_41)) ));
#line 27 "../Main.m3"
 /* set_label */
#line 27 "../Main.m3"
L2:;
#line 27 "../Main.m3"
 /* start_call_direct */
#line 27 "../Main.m3"
 /* load */
#line 27 "../Main.m3"
 /* pop_param */
#line 27 "../Main.m3"
 /* load_integer */
#line 27 "../Main.m3"
 /* pop_param */
#line 27 "../Main.m3"
 /* call_direct */
#line 27 "../Main.m3"
 /* store */
#line 27 "../Main.m3"
(*(ADDRESS*)(&Main_m_42_L_43))=(ADDRESS)(((ADDRESS)(Fmt__Int(
  ( INTEGER )( ((INT64)(*((UINT64*)(INT64_(104)+((ADDRESS)(&Main_m_M_Main_L_36)))))) ),
  ( Fmt__Base )(((UINT8)( INT64_(10))) )))));
#line 27 "../Main.m3"
 /* start_call_direct */
#line 27 "../Main.m3"
 /* load */
#line 27 "../Main.m3"
 /* pop_param */
#line 27 "../Main.m3"
 /* load_address */
#line 27 "../Main.m3"
 /* pop_param */
#line 27 "../Main.m3"
 /* call_direct */
#line 27 "../Main.m3"
 /* store */
#line 27 "../Main.m3"
(*(ADDRESS*)(&Main_m_44_L_45))=(ADDRESS)(((ADDRESS)(RTHooks__Concat(
  ( TEXT )(((ADDRESS)(Main_m_42_L_43)) ),
  ( TEXT )(((ADDRESS)(INT64_(48)+((ADDRESS)(&Main_m_34_L_35)))) )))));
#line 27 "../Main.m3"
 /* start_call_direct */
#line 27 "../Main.m3"
 /* load */
#line 27 "../Main.m3"
 /* pop_param */
#line 27 "../Main.m3"
 /* load */
#line 27 "../Main.m3"
 /* pop_param */
#line 27 "../Main.m3"
 /* call_direct */
#line 27 "../Main.m3"
Wr__PutText(
  ( Wr__T )(((ADDRESS)(Main_m_40_L_41)) ),
  ( TEXT )(((ADDRESS)(Main_m_44_L_45)) ));
#line 27 "../Main.m3"
 /* set_source_line */
#line 27 "../Main.m3"
#line 29 "../Main.m3"
 /* load_procedure */
#line 29 "../Main.m3"
 /* store */
#line 29 "../Main.m3"
(*(ADDRESS*)((120)+(char*)(&Main_m_M_Main_L_36)))=(ADDRESS)(((ADDRESS)(Main__Toto)));
#line 29 "../Main.m3"
 /* set_source_line */
#line 29 "../Main.m3"
#line 30 "../Main.m3"
 /* start_call_direct */
#line 30 "../Main.m3"
 /* load_address */
#line 30 "../Main.m3"
 /* pop_param */
#line 30 "../Main.m3"
 /* load */
#line 30 "../Main.m3"
 /* pop_param */
#line 30 "../Main.m3"
 /* call_direct */
#line 30 "../Main.m3"
RTProcedure__ToFingerprint(
  ( T26466202* /*TypeText1*/  )(((ADDRESS)(&Main_m_46_L_47)) ),
  ( RTProcedure__Proc )(((ADDRESS)(*((ADDRESS*)(INT64_(120)+((ADDRESS)(&Main_m_M_Main_L_36)))))) ));
#line 30 "../Main.m3"
 /* load_address */
#line 30 "../Main.m3"
 /* load_address */
#line 30 "../Main.m3"
 /* copy */
#line 30 "../Main.m3"
m3_memmove(
 INT64_(112)+((ADDRESS)(&Main_m_M_Main_L_36)),
 &Main_m_46_L_47,
 8);
#line 30 "../Main.m3"
 /* set_source_line */
#line 30 "../Main.m3"
#line 31 "../Main.m3"
 /* load_address */
#line 31 "../Main.m3"
 /* load_address */
#line 31 "../Main.m3"
 /* copy */
#line 31 "../Main.m3"
m3_memmove(
 INT64_(128)+((ADDRESS)(&Main_m_M_Main_L_36)),
 INT64_(112)+((ADDRESS)(&Main_m_M_Main_L_36)),
 8);
#line 31 "../Main.m3"
 /* set_source_line */
#line 31 "../Main.m3"
#line 32 "../Main.m3"
 /* load */
#line 32 "../Main.m3"
 /* load_indirect */
#line 32 "../Main.m3"
 /* store */
#line 32 "../Main.m3"
(*(ADDRESS*)(&Main_m_44_L_45))=(ADDRESS)(((ADDRESS)(*((ADDRESS*)(INT64_(120)+((ADDRESS)(*((ADDRESS*)(INT64_(208)+((ADDRESS)(&Main_m_M_Main_L_36)))))))))));
#line 32 "../Main.m3"
 /* load_nil */
#line 32 "../Main.m3"
 /* load */
#line 32 "../Main.m3"
 /* if_compare */
#line 32 "../Main.m3"
if(m3_eq(ADDRESS,
 ((ADDRESS)(0)),
 ((ADDRESS)(Main_m_44_L_45))))goto L3;
#line 32 "../Main.m3"
 /* load */
#line 32 "../Main.m3"
 /* loophole */
#line 32 "../Main.m3"
 /* load_integer */
#line 32 "../Main.m3"
 /* and */
#line 32 "../Main.m3"
 /* if_true_or_false */
#line 32 "../Main.m3"
 /* load_host_integer */
#line 32 "../Main.m3"
 /* load_integer */
#line 32 "../Main.m3"
 /* if_compare */
#line 32 "../Main.m3"
if(m3_ne(INT64,
  ((INT64)(((INT64)((INT64)Main_m_44_L_45))&  INT64_(1))),
   INT64_(0)))goto L3;
#line 32 "../Main.m3"
 /* load */
#line 32 "../Main.m3"
 /* load_indirect */
#line 32 "../Main.m3"
 /* extract_mn */
#line 32 "../Main.m3"
 /* load_host_integer */
#line 32 "../Main.m3"
 /* load_integer */
#line 32 "../Main.m3"
 /* load_host_integer */
#line 32 "../Main.m3"
 /* load_integer */
#line 32 "../Main.m3"
 /* extract */
#line 32 "../Main.m3"
 /* if_true_or_false */
#line 32 "../Main.m3"
 /* load_host_integer */
#line 32 "../Main.m3"
 /* load_integer */
#line 32 "../Main.m3"
 /* if_compare */
#line 32 "../Main.m3"
if(m3_eq(INT64,
 ((INT64)(m3_extract(
 UINT64,
  *((INT64*)(INT64_(-8)+((ADDRESS)(Main_m_44_L_45)))),
   UINT64_(22),
   UINT64_(1)))),
   INT64_(0)))goto L3;
#line 32 "../Main.m3"
 /* start_call_direct */
#line 32 "../Main.m3"
 /* load */
#line 32 "../Main.m3"
 /* pop_param */
#line 32 "../Main.m3"
 /* call_direct */
#line 32 "../Main.m3"
RTHooks__CheckLoadTracedRef(
  ( REFANY )(((ADDRESS)(Main_m_44_L_45)) ));
#line 32 "../Main.m3"
 /* set_label */
#line 32 "../Main.m3"
L3:;
#line 32 "../Main.m3"
 /* start_call_direct */
#line 32 "../Main.m3"
 /* load */
#line 32 "../Main.m3"
 /* pop_param */
#line 32 "../Main.m3"
 /* load_integer */
#line 32 "../Main.m3"
 /* pop_param */
#line 32 "../Main.m3"
 /* call_direct */
#line 32 "../Main.m3"
 /* store */
#line 32 "../Main.m3"
(*(ADDRESS*)(&Main_m_42_L_43))=(ADDRESS)(((ADDRESS)(Fmt__Int(
  ( INTEGER )( ((INT64)(*((INT32*)(INT64_(128)+((ADDRESS)(&Main_m_M_Main_L_36)))))) ),
  ( Fmt__Base )(((UINT8)( INT64_(10))) )))));
#line 32 "../Main.m3"
 /* start_call_direct */
#line 32 "../Main.m3"
 /* load_address */
#line 32 "../Main.m3"
 /* pop_param */
#line 32 "../Main.m3"
 /* load */
#line 32 "../Main.m3"
 /* pop_param */
#line 32 "../Main.m3"
 /* call_direct */
#line 32 "../Main.m3"
 /* store */
#line 32 "../Main.m3"
(*(ADDRESS*)(&Main_m_40_L_41))=(ADDRESS)(((ADDRESS)(RTHooks__Concat(
  ( TEXT )(((ADDRESS)(INT64_(88)+((ADDRESS)(&Main_m_34_L_35)))) ),
  ( TEXT )(((ADDRESS)(Main_m_42_L_43)) )))));
#line 32 "../Main.m3"
 /* start_call_direct */
#line 32 "../Main.m3"
 /* load */
#line 32 "../Main.m3"
 /* pop_param */
#line 32 "../Main.m3"
 /* load_address */
#line 32 "../Main.m3"
 /* pop_param */
#line 32 "../Main.m3"
 /* call_direct */
#line 32 "../Main.m3"
 /* store */
#line 32 "../Main.m3"
(*(ADDRESS*)(&Main_m_48_L_49))=(ADDRESS)(((ADDRESS)(RTHooks__Concat(
  ( TEXT )(((ADDRESS)(Main_m_40_L_41)) ),
  ( TEXT )(((ADDRESS)(INT64_(136)+((ADDRESS)(&Main_m_34_L_35)))) )))));
#line 32 "../Main.m3"
 /* start_call_direct */
#line 32 "../Main.m3"
 /* load */
#line 32 "../Main.m3"
 /* pop_param */
#line 32 "../Main.m3"
 /* load_integer */
#line 32 "../Main.m3"
 /* pop_param */
#line 32 "../Main.m3"
 /* call_direct */
#line 32 "../Main.m3"
 /* store */
#line 32 "../Main.m3"
(*(ADDRESS*)(&Main_m_50_L_51))=(ADDRESS)(((ADDRESS)(Fmt__Int(
  ( INTEGER )( ((INT64)(*((INT32*)(INT64_(132)+((ADDRESS)(&Main_m_M_Main_L_36)))))) ),
  ( Fmt__Base )(((UINT8)( INT64_(10))) )))));
#line 32 "../Main.m3"
 /* start_call_direct */
#line 32 "../Main.m3"
 /* load */
#line 32 "../Main.m3"
 /* pop_param */
#line 32 "../Main.m3"
 /* load */
#line 32 "../Main.m3"
 /* pop_param */
#line 32 "../Main.m3"
 /* call_direct */
#line 32 "../Main.m3"
 /* store */
#line 32 "../Main.m3"
(*(ADDRESS*)(&Main_m_52_L_53))=(ADDRESS)(((ADDRESS)(RTHooks__Concat(
  ( TEXT )(((ADDRESS)(Main_m_48_L_49)) ),
  ( TEXT )(((ADDRESS)(Main_m_50_L_51)) )))));
#line 32 "../Main.m3"
 /* start_call_direct */
#line 32 "../Main.m3"
 /* load */
#line 32 "../Main.m3"
 /* pop_param */
#line 32 "../Main.m3"
 /* load_address */
#line 32 "../Main.m3"
 /* pop_param */
#line 32 "../Main.m3"
 /* call_direct */
#line 32 "../Main.m3"
 /* store */
#line 32 "../Main.m3"
(*(ADDRESS*)(&Main_m_54_L_55))=(ADDRESS)(((ADDRESS)(RTHooks__Concat(
  ( TEXT )(((ADDRESS)(Main_m_52_L_53)) ),
  ( TEXT )(((ADDRESS)(INT64_(168)+((ADDRESS)(&Main_m_34_L_35)))) )))));
#line 32 "../Main.m3"
 /* start_call_direct */
#line 32 "../Main.m3"
 /* load */
#line 32 "../Main.m3"
 /* pop_param */
#line 32 "../Main.m3"
 /* load */
#line 32 "../Main.m3"
 /* pop_param */
#line 32 "../Main.m3"
 /* call_direct */
#line 32 "../Main.m3"
Wr__PutText(
  ( Wr__T )(((ADDRESS)(Main_m_44_L_45)) ),
  ( TEXT )(((ADDRESS)(Main_m_54_L_55)) ));
#line 32 "../Main.m3"
 /* set_source_line */
#line 32 "../Main.m3"
#line 36 "../Main.m3"
 /* start_call_direct */
#line 36 "../Main.m3"
 /* load_address */
#line 36 "../Main.m3"
 /* pop_param */
#line 36 "../Main.m3"
 /* call_direct */
#line 36 "../Main.m3"
 /* store */
#line 36 "../Main.m3"
(*(ADDRESS*)(&Main_m_54_L_55))=(ADDRESS)(((ADDRESS)(RTProcedure__FromFingerprint(
  ( T26466202* /*TypeText1*/  )(((ADDRESS)(INT64_(112)+((ADDRESS)(&Main_m_M_Main_L_36)))) )))));
#line 36 "../Main.m3"
 /* load */
#line 36 "../Main.m3"
 /* store */
#line 36 "../Main.m3"
(*(ADDRESS*)((120)+(char*)(&Main_m_M_Main_L_36)))=(ADDRESS)(((ADDRESS)(Main_m_54_L_55)));
#line 36 "../Main.m3"
 /* set_source_line */
#line 36 "../Main.m3"
#line 37 "../Main.m3"
 /* start_call_direct */
#line 37 "../Main.m3"
 /* load_procedure */
#line 37 "../Main.m3"
 /* load */
#line 37 "../Main.m3"
 /* compare */
#line 37 "../Main.m3"
 /* pop_param */
#line 37 "../Main.m3"
 /* load_integer */
#line 37 "../Main.m3"
 /* pop_param */
#line 37 "../Main.m3"
 /* call_direct */
#line 37 "../Main.m3"
Test__checkB(
  ( BOOLEAN )(((UINT8)(((INT64)(m3_eq(ADDRESS,
 ((ADDRESS)Main__Toto),
 ((ADDRESS)*((ADDRESS*)(INT64_(120)+((ADDRESS)(&Main_m_M_Main_L_36)))))))))) ),
  ( BOOLEAN )(((UINT8)( INT64_(1))) ));
#line 37 "../Main.m3"
 /* set_source_line */
#line 37 "../Main.m3"
#line 39 "../Main.m3"
 /* start_call_direct */
#line 39 "../Main.m3"
 /* call_direct */
#line 39 "../Main.m3"
Test__done(
 );
#line 39 "../Main.m3"
 /* set_label */
#line 39 "../Main.m3"
L1:;
#line 39 "../Main.m3"
 /* load_address */
#line 39 "../Main.m3"
 /* exit_proc */
#line 39 "../Main.m3"
return (RT0__ModulePtr)(&Main_m_M_Main_L_36);
#line 39 "../Main.m3"
 /* end_procedure */
#line 39 "../Main.m3"
} /* global constant type descriptor */
#line 39 "../Main.m3"
 /* global data type descriptor */
#line 39 "../Main.m3"
 /* module global constants */
#line 39 "../Main.m3"
 /* procedure names */
#line 39 "../Main.m3"
 /* procedure table */
#line 39 "../Main.m3"
 /* file name */
#line 39 "../Main.m3"
 /* module global data */
#line 39 "../Main.m3"
 /* load map


 global data allocation for M_Main
     0   104  8  *module info*
   104     8  8  Main.n
   112     8  1  Main.fp
   120     8  8  Main.proc
   128     8  4  Main.xfp
   136    24  8  import Main
   160    24  8  import Test
   184    24  8  import Fmt
   208    24  8  import Stdio
   232    24  8  import Wr
   256    24  8  import RTProcedureSRC
   280    24  8  import RTProcedure
   304    24  8  import Fingerprint
   328    24  8  import RTHooks
   352     0  8  *TOTAL*


 global constants for M_Main
     0    40  8  TEXT literal methods
    40    37  8  *TEXT literal*
    80    45  8  *TEXT literal*
   128    27  8  *TEXT literal*
   160    27  8  *TEXT literal*
   192    13  8  *proc names*
   208    40  8  *proc info*
   248    11  1  *string*
   264     0  8  *TOTAL*
 */
#line 39 "../Main.m3"
 /* end unit */
#line 39 "../Main.m3"

#ifdef __cplusplus

} /* extern "C" */
#endif
 /* set_runtime_proc */
 /* set_runtime_proc */
