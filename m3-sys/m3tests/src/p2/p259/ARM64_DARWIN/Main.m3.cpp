// library:pgm
// source_base_name:Main
// target_name:Main.m3.cpp
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
 /* declare_proctype */

#if 0 /* avoid type hash collions */
typedef 
ADDRESS(__cdecl*TADC6B75A)(void);
#else
typedef void (__cdecl*TADC6B75A)(void);
#endif
 /* declare_proctype */

#if 0 /* avoid type hash collions */
typedef 
INTEGER(__cdecl*TEE9B4E5D)(void);
#else
typedef void (__cdecl*TEE9B4E5D)(void);
#endif
 /* declare_opaque */

#ifndef T62761487
#define T62761487 T62761487
/*1addressType_define*/typedef ADDRESS T62761487;

#endif
 /* declare_proctype */
 /* declare_formal */
 /* declare_formal */

#ifndef Wr__T
#define Wr__T Wr__T
typedef T62761487 Wr__T;
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
 /* declare_record */
 /* declare_record */
 /* declare_field */
 /* DeclareTypes_FlushOnce size:7 */

#if 0 /* avoid type hash collions */
typedef 
void(__cdecl*T43604B73)(TEXT,Wr__T);
#else
typedef void (__cdecl*T43604B73)(void);
#endif

#if 0 /* avoid type hash collions */
typedef 
void(__cdecl*TB6592544)(INTEGER,Wr__T);
#else
typedef void (__cdecl*TB6592544)(void);
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
/*Proc_ForwardDeclareFrameType*/struct Main_I3_Frame_t;typedef struct Main_I3_Frame_t Main_I3_Frame_t;
 /* internal_declare_param */
RT0__ModulePtr
__cdecl
Main_I3(
   /* Param_Type1 */ INTEGER /*TypeText1*/  mode_L_1);
 /* import_procedure */
/*Proc_ForwardDeclareFrameType*/struct IO_I3_Frame_t;typedef struct IO_I3_Frame_t IO_I3_Frame_t;
 /* internal_declare_param */
RT0__ModulePtr
__cdecl
IO_I3(
   /* Param_Type1 */ INTEGER /*TypeText1*/  mode_L_2);
 /* import_procedure */
/*Proc_ForwardDeclareFrameType*/struct RTHooks_I3_Frame_t;typedef struct RTHooks_I3_Frame_t RTHooks_I3_Frame_t;
 /* internal_declare_param */
RT0__ModulePtr
__cdecl
RTHooks_I3(
   /* Param_Type1 */ INTEGER /*TypeText1*/  mode_L_3);
 /* import_procedure */
/*Proc_ForwardDeclareFrameType*/struct IO__Put_Frame_t;typedef struct IO__Put_Frame_t IO__Put_Frame_t;
 /* internal_declare_param */
 /* internal_declare_param */
void /*TypeText3*/ 
__cdecl
IO__Put(
   /* Param_Type1 */ TEXT txt_L_4,
   /* Param_Type1 */ Wr__T wr_L_5);
 /* import_procedure */
/*Proc_ForwardDeclareFrameType*/struct IO__PutInt_Frame_t;typedef struct IO__PutInt_Frame_t IO__PutInt_Frame_t;
 /* internal_declare_param */
 /* internal_declare_param */
void /*TypeText3*/ 
__cdecl
IO__PutInt(
   /* Param_Type1 */ INTEGER n_L_6,
   /* Param_Type1 */ Wr__T wr_L_7);
 /* import_procedure */
/*Proc_ForwardDeclareFrameType*/struct RTHooks__TextLitInfo_Frame_t;typedef struct RTHooks__TextLitInfo_Frame_t RTHooks__TextLitInfo_Frame_t;
 /* internal_declare_param */
 /* internal_declare_param */
void /*TypeText3*/ 
__cdecl
RTHooks__TextLitInfo(
   /* Param_Type1 */ RTHooks__TextLiteral t_L_8,
   /* Param_Type1 */ RTHooks__TextInfo* /*TypeText1*/  i_L_9);
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
   /* Param_Type1 */ RTHooks__TextLiteral t_L_10,
   /* Param_Type1 */ CARDINAL i_L_11);
 /* import_procedure */
/*Proc_ForwardDeclareFrameType*/struct RTHooks__TextLitGetWideChar_Frame_t;typedef struct RTHooks__TextLitGetWideChar_Frame_t RTHooks__TextLitGetWideChar_Frame_t;
 /* internal_declare_param */
 /* internal_declare_param */
WIDECHAR
__cdecl
RTHooks__TextLitGetWideChar(
   /* Param_Type1 */ RTHooks__TextLiteral t_L_12,
   /* Param_Type1 */ CARDINAL i_L_13);
 /* import_procedure */
/*Proc_ForwardDeclareFrameType*/struct RTHooks__TextLitGetChars_Frame_t;typedef struct RTHooks__TextLitGetChars_Frame_t RTHooks__TextLitGetChars_Frame_t;
 /* internal_declare_param */
 /* internal_declare_param */
 /* internal_declare_param */
void /*TypeText3*/ 
__cdecl
RTHooks__TextLitGetChars(
   /* Param_Type1 */ RTHooks__TextLiteral t_L_14,
   /* Param_Type1 */ T89CD34BD* /*TypeText1*/  a_L_15,
   /* Param_Type1 */ CARDINAL start_L_16);
 /* import_procedure */
/*Proc_ForwardDeclareFrameType*/struct RTHooks__TextLitGetWideChars_Frame_t;typedef struct RTHooks__TextLitGetWideChars_Frame_t RTHooks__TextLitGetWideChars_Frame_t;
 /* internal_declare_param */
 /* internal_declare_param */
 /* internal_declare_param */
void /*TypeText3*/ 
__cdecl
RTHooks__TextLitGetWideChars(
   /* Param_Type1 */ RTHooks__TextLiteral t_L_17,
   /* Param_Type1 */ TA19BDC21* /*TypeText1*/  a_L_18,
   /* Param_Type1 */ CARDINAL start_L_19);
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
/*Proc_ForwardDeclareFrameType*/struct Main__NL_Frame_t;typedef struct Main__NL_Frame_t Main__NL_Frame_t;
void /*TypeText3*/ 
__cdecl
Main__NL(void);
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct Main__GetStack_Frame_t;typedef struct Main__GetStack_Frame_t Main__GetStack_Frame_t;
ADDRESS
__cdecl
Main__GetStack(void);
 /* declare_local */
 /* declare_local */
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct Main__GetStackHeight_Frame_t;typedef struct Main__GetStackHeight_Frame_t Main__GetStackHeight_Frame_t;
INTEGER
__cdecl
Main__GetStackHeight(void);
 /* declare_local */
 /* declare_local */
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct Main__PrintStackHeight_Frame_t;typedef struct Main__PrintStackHeight_Frame_t Main__PrintStackHeight_Frame_t;
void /*TypeText3*/ 
__cdecl
Main__PrintStackHeight(void);
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct Main__Try1_Frame_t;typedef struct Main__Try1_Frame_t Main__Try1_Frame_t;
void /*TypeText3*/ 
__cdecl
Main__Try1(void);
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct Main__Try1__Try1_Try2_Frame_t;typedef struct Main__Try1__Try1_Try2_Frame_t Main__Try1__Try1_Try2_Frame_t;
 /* internal_declare_param */
void /*TypeText3*/ 
__cdecl
Main__Try1__Try1_Try2(
   /* Param_Type1 */ Main__Try1_Frame_t* _static_link);
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct Main__Try2_Frame_t;typedef struct Main__Try2_Frame_t Main__Try2_Frame_t;
void /*TypeText3*/ 
__cdecl
Main__Try2(void);
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct Main__Try2__Try2_Try2_Frame_t;typedef struct Main__Try2__Try2_Try2_Frame_t Main__Try2__Try2_Try2_Frame_t;
 /* internal_declare_param */
void /*TypeText3*/ 
__cdecl
Main__Try2__Try2_Try2(
   /* Param_Type1 */ Main__Try2_Frame_t* _static_link);
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct Main__Try3_Frame_t;typedef struct Main__Try3_Frame_t Main__Try3_Frame_t;
void /*TypeText3*/ 
__cdecl
Main__Try3(void);
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct Main__Try3__Try3_Try1_Frame_t;typedef struct Main__Try3__Try3_Try1_Frame_t Main__Try3__Try3_Try1_Frame_t;
 /* internal_declare_param */
void /*TypeText3*/ 
__cdecl
Main__Try3__Try3_Try1(
   /* Param_Type1 */ Main__Try3_Frame_t* _static_link);
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct Main__Try3__Try3_Try2_Frame_t;typedef struct Main__Try3__Try3_Try2_Frame_t Main__Try3__Try3_Try2_Frame_t;
 /* internal_declare_param */
void /*TypeText3*/ 
__cdecl
Main__Try3__Try3_Try2(
   /* Param_Type1 */ Main__Try3_Frame_t* _static_link);
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct Main__Try3__Try3_Try3_Frame_t;typedef struct Main__Try3__Try3_Try3_Frame_t Main__Try3__Try3_Try3_Frame_t;
 /* internal_declare_param */
void /*TypeText3*/ 
__cdecl
Main__Try3__Try3_Try3(
   /* Param_Type1 */ Main__Try3_Frame_t* _static_link);
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
 /* Locals_begin_procedure */
 /* begin_block */
 /* declare_temp */
 /* declare_local */
 /* Locals_end_procedure */
 /* end_block */
 /* Locals_begin_procedure */
 /* begin_block */
 /* declare_local */
 /* Locals_end_procedure */
 /* end_block */
 /* Locals_begin_procedure */
 /* begin_block */
 /* declare_local */
 /* declare_local */
 /* Locals_end_procedure */
 /* end_block */
 /* Locals_begin_procedure */
 /* begin_block */
 /* declare_local */
 /* declare_local */
 /* Locals_end_procedure */
 /* end_block */
 /* Locals_begin_procedure */
 /* begin_block */
 /* declare_local */
 /* declare_local */
 /* Locals_end_procedure */
 /* end_block */
 /* Locals_begin_procedure */
 /* begin_block */
 /* declare_local */
 /* declare_local */
 /* declare_local */
 /* Locals_end_procedure */
 /* end_block */
 /* Locals_begin_procedure */
 /* begin_block */
 /* declare_local */
 /* Locals_end_procedure */
 /* end_block */
 /* Locals_begin_procedure */
 /* begin_block */
 /* declare_local */
 /* declare_local */
 /* Locals_end_procedure */
 /* end_block */
 /* Locals_begin_procedure */
 /* begin_block */
 /* declare_local */
 /* declare_local */
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
 /* end_init */
struct Main_m_20_L_21_t{ADDRESS L_66[5];
INT64 L_67[1];
ADDRESS L_68[1];
INT64 L_69[1];
UINT8 L_70[1];
char L_71[7];
INT64 L_72[1];
ADDRESS L_73[1];
INT64 L_74[1];
UINT8 L_75[13];
char L_76[3];
INT64 L_77[1];
ADDRESS L_78[1];
INT64 L_79[1];
UINT8 L_80[1];
char L_81[7];
INT64 L_82[1];
ADDRESS L_83[1];
INT64 L_84[1];
UINT8 L_85[10];
char L_86[6];
INT64 L_87[1];
ADDRESS L_88[1];
INT64 L_89[1];
UINT8 L_90[10];
char L_91[6];
INT64 L_92[1];
ADDRESS L_93[1];
INT64 L_94[1];
UINT8 L_95[5];
char L_96[3];
INT64 L_97[1];
ADDRESS L_98[1];
INT64 L_99[1];
UINT8 L_100[5];
char L_101[3];
INT64 L_102[1];
ADDRESS L_103[1];
INT64 L_104[1];
UINT8 L_105[10];
char L_106[6];
INT64 L_107[1];
ADDRESS L_108[1];
INT64 L_109[1];
UINT8 L_110[10];
char L_111[6];
INT64 L_112[1];
ADDRESS L_113[1];
INT64 L_114[1];
UINT8 L_115[5];
char L_116[3];
INT64 L_117[1];
ADDRESS L_118[1];
INT64 L_119[1];
UINT8 L_120[5];
char L_121[3];
INT64 L_122[1];
ADDRESS L_123[1];
INT64 L_124[1];
UINT8 L_125[10];
char L_126[6];
INT64 L_127[1];
ADDRESS L_128[1];
INT64 L_129[1];
UINT8 L_130[10];
char L_131[6];
INT64 L_132[1];
ADDRESS L_133[1];
INT64 L_134[1];
UINT8 L_135[10];
char L_136[6];
INT64 L_137[1];
ADDRESS L_138[1];
INT64 L_139[1];
UINT8 L_140[10];
char L_141[6];
INT64 L_142[1];
ADDRESS L_143[1];
INT64 L_144[1];
UINT8 L_145[10];
char L_146[6];
INT64 L_147[1];
ADDRESS L_148[1];
INT64 L_149[1];
UINT8 L_150[10];
char L_151[6];
INT64 L_152[1];
ADDRESS L_153[1];
INT64 L_154[1];
UINT8 L_155[5];
char L_156[3];
INT64 L_157[1];
ADDRESS L_158[1];
INT64 L_159[1];
UINT8 L_160[5];
char L_161[3];
UINT8 L_162[7];
char L_163[1];
UINT8 L_164[4];
char L_165[1];
UINT8 L_166[14];
char L_167[1];
UINT8 L_168[14];
char L_169[1];
UINT8 L_170[14];
char L_171[1];
UINT8 L_172[4];
char L_173[1];
UINT8 L_174[14];
char L_175[1];
UINT8 L_176[4];
char L_177[1];
UINT8 L_178[14];
char L_179[1];
UINT8 L_180[16];
char L_181[1];
UINT8 L_182[14];
char L_183[1];
UINT8 L_184[8];
char L_185[1];
UINT8 L_186[2];
char L_187[3];
ADDRESS L_188[26];
char L_189[8];
UINT8 L_190[10];
char L_191[14];
};
static  const Main_m_20_L_21_t Main_m_20_L_21={{(ADDRESS)&RTHooks__TextLitInfo,(ADDRESS)&RTHooks__TextLitGetChar,(ADDRESS)&RTHooks__TextLitGetWideChar,(ADDRESS)&RTHooks__TextLitGetChars,(ADDRESS)&RTHooks__TextLitGetWideChars},{INT64_(2)},{(char*)&Main_m_20_L_21},{INT64_(1)},{10},{0 /* 1 */ ,0 /* 2 */ ,0 /* 3 */ ,0 /* 4 */ ,0 /* 5 */ ,0 /* 6 */ ,0 /* 7 */ ,},{INT64_(2)},{(char*)&Main_m_20_L_21},{INT64_(13)},{'s','t','a','c','k','_','h','e','i','g','h','t',':'},{0 /* 1 */ ,0 /* 2 */ ,0 /* 3 */ ,},{INT64_(2)},{(char*)&Main_m_20_L_21},{INT64_(1)},{' '},{0 /* 1 */ ,0 /* 2 */ ,0 /* 3 */ ,0 /* 4 */ ,0 /* 5 */ ,0 /* 6 */ ,0 /* 7 */ ,},{INT64_(2)},{(char*)&Main_m_20_L_21},{INT64_(10)},{'>','T','r','y','1','_','T','r','y','2'},{0 /* 1 */ ,0 /* 2 */ ,0 /* 3 */ ,0 /* 4 */ ,0 /* 5 */ ,0 /* 6 */ ,},{INT64_(2)},{(char*)&Main_m_20_L_21},{INT64_(10)},{'<','T','r','y','1','_','T','r','y','2'},{0 /* 1 */ ,0 /* 2 */ ,0 /* 3 */ ,0 /* 4 */ ,0 /* 5 */ ,0 /* 6 */ ,},{INT64_(2)},{(char*)&Main_m_20_L_21},{INT64_(5)
},{'>','T','r','y','1'},{0 /* 1 */ ,0 /* 2 */ ,0 /* 3 */ ,},{INT64_(2)},{(char*)&Main_m_20_L_21},{INT64_(5)},{'<','T','r','y','1'},{0 /* 1 */ ,0 /* 2 */ ,0 /* 3 */ ,},{INT64_(2)},{(char*)&Main_m_20_L_21},{INT64_(10)},{'>','T','r','y','2','_','T','r','y','2'},{0 /* 1 */ ,0 /* 2 */ ,0 /* 3 */ ,0 /* 4 */ ,0 /* 5 */ ,0 /* 6 */ ,},{INT64_(2)},{(char*)&Main_m_20_L_21},{INT64_(10)},{'<','T','r','y','2','_','T','r','y','2'},{0 /* 1 */ ,0 /* 2 */ ,0 /* 3 */ ,0 /* 4 */ ,0 /* 5 */ ,0 /* 6 */ ,},{INT64_(2)},{(char*)&Main_m_20_L_21},{INT64_(5)},{'>','T','r','y','2'},{0 /* 1 */ ,0 /* 2 */ ,0 /* 3 */ ,},{INT64_(2)},{(char*)&Main_m_20_L_21},{INT64_(5)},{'<','T','r','y','2'},{0 /* 1 */ ,0 /* 2 */ ,0 /* 3 */ ,},{INT64_(2)},{(char*)&Main_m_20_L_21},{INT64_(10)},{'>','T','r','y','3','_','T','r','y','1'},{0 /* 1 */ ,0 /* 2 */ ,0 /* 3 */ ,0 /* 4 */ ,0 /* 5 */ ,0 /* 6 */ ,},{INT64_(2)},{(char*)&Main_m_20_L_21},{INT64_(10)},{'<','T','r','y','3','_','T','r','y','1'},{0 /* 1 */ ,0 /* 2 */ ,0 /* 3 */ ,0 /* 4 */ 
,0 /* 5 */ ,0 /* 6 */ ,},{INT64_(2)},{(char*)&Main_m_20_L_21},{INT64_(10)},{'>','T','r','y','3','_','T','r','y','2'},{0 /* 1 */ ,0 /* 2 */ ,0 /* 3 */ ,0 /* 4 */ ,0 /* 5 */ ,0 /* 6 */ ,},{INT64_(2)},{(char*)&Main_m_20_L_21},{INT64_(10)},{'<','T','r','y','3','_','T','r','y','2'},{0 /* 1 */ ,0 /* 2 */ ,0 /* 3 */ ,0 /* 4 */ ,0 /* 5 */ ,0 /* 6 */ ,},{INT64_(2)},{(char*)&Main_m_20_L_21},{INT64_(10)},{'>','T','r','y','3','_','T','r','y','3'},{0 /* 1 */ ,0 /* 2 */ ,0 /* 3 */ ,0 /* 4 */ ,0 /* 5 */ ,0 /* 6 */ ,},{INT64_(2)},{(char*)&Main_m_20_L_21},{INT64_(10)},{'<','T','r','y','3','_','T','r','y','3'},{0 /* 1 */ ,0 /* 2 */ ,0 /* 3 */ ,0 /* 4 */ ,0 /* 5 */ ,0 /* 6 */ ,},{INT64_(2)},{(char*)&Main_m_20_L_21},{INT64_(5)},{'>','T','r','y','3'},{0 /* 1 */ ,0 /* 2 */ ,0 /* 3 */ ,},{INT64_(2)},{(char*)&Main_m_20_L_21},{INT64_(5)},{'<','T','r','y','3'},{0 /* 1 */ ,0 /* 2 */ ,0 /* 3 */ ,},{'M','a','i','n','_','M','3'},{0 /* 1 */ ,},{'T','r','y','3'},{0 /* 1 */ ,},{'T','r','y','3','.','T','r','y','3','_',
'T','r','y','3'},{0 /* 1 */ ,},{'T','r','y','3','.','T','r','y','3','_','T','r','y','2'},{0 /* 1 */ ,},{'T','r','y','3','.','T','r','y','3','_','T','r','y','1'},{0 /* 1 */ ,},{'T','r','y','2'},{0 /* 1 */ ,},{'T','r','y','2','.','T','r','y','2','_','T','r','y','2'},{0 /* 1 */ ,},{'T','r','y','1'},{0 /* 1 */ ,},{'T','r','y','1','.','T','r','y','1','_','T','r','y','2'},{0 /* 1 */ ,},{'P','r','i','n','t','S','t','a','c','k','H','e','i','g','h','t'},{0 /* 1 */ ,},{'G','e','t','S','t','a','c','k','H','e','i','g','h','t'},{0 /* 1 */ ,},{'G','e','t','S','t','a','c','k'},{0 /* 1 */ ,},{'N','L'},{0 /* 1 */ ,0 /* 2 */ ,0 /* 3 */ ,},{(ADDRESS)&Main_M3,736+(char*)&Main_m_20_L_21,(ADDRESS)&Main__Try3,744+(char*)&Main_m_20_L_21,(ADDRESS)&Main__Try3__Try3_Try3,749+(char*)&Main_m_20_L_21,(ADDRESS)&Main__Try3__Try3_Try2,764+(char*)&Main_m_20_L_21,(ADDRESS)&Main__Try3__Try3_Try1,779+(char*)&Main_m_20_L_21,(ADDRESS)&Main__Try2,794+(char*)&Main_m_20_L_21,(ADDRESS)&Main__Try2__Try2_Try2,799+(char*)&Main_m_20_L_21
,(ADDRESS)&Main__Try1,814+(char*)&Main_m_20_L_21,(ADDRESS)&Main__Try1__Try1_Try2,819+(char*)&Main_m_20_L_21,(ADDRESS)&Main__PrintStackHeight,834+(char*)&Main_m_20_L_21,(ADDRESS)&Main__GetStackHeight,851+(char*)&Main_m_20_L_21,(ADDRESS)&Main__GetStack,866+(char*)&Main_m_20_L_21,(ADDRESS)&Main__NL,875+(char*)&Main_m_20_L_21},{0 /* 1 */ ,0 /* 2 */ ,0 /* 3 */ ,0 /* 4 */ ,0 /* 5 */ ,0 /* 6 */ ,0 /* 7 */ ,0 /* 8 */ ,},{'.','.','/','M','a','i','n','.','m','3'},{0 /* 1 */ ,0 /* 2 */ ,0 /* 3 */ ,0 /* 4 */ ,0 /* 5 */ ,0 /* 6 */ ,0 /* 7 */ ,0 /* 8 */ ,0 /* 9 */ ,0 /* 10 */ ,0 /* 11 */ ,0 /* 12 */ ,0 /* 13 */ ,0 /* 14 */ ,}};
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
 /* end_init */
struct Main_m_M_Main_L_22_t{ADDRESS L_192[1];
char L_193[32];
ADDRESS L_194[1];
char L_195[24];
ADDRESS L_196[1];
char L_197[8];
ADDRESS L_198[1];
INT64 L_199[1];
char L_200[16];
ADDRESS L_201[2];
char L_202[8];
ADDRESS L_203[2];
char L_204[8];
ADDRESS L_205[1];
char L_206[16];
};
static Main_m_M_Main_L_22_t Main_m_M_Main_L_22={{1096+(char*)&Main_m_20_L_21},{0 /* 1 */ ,0 /* 2 */ ,0 /* 3 */ ,0 /* 4 */ ,0 /* 5 */ ,0 /* 6 */ ,0 /* 7 */ ,0 /* 8 */ ,0 /* 9 */ ,0 /* 10 */ ,0 /* 11 */ ,0 /* 12 */ ,0 /* 13 */ ,0 /* 14 */ ,0 /* 15 */ ,0 /* 16 */ ,0 /* 17 */ ,0 /* 18 */ ,0 /* 19 */ ,0 /* 20 */ ,0 /* 21 */ ,0 /* 22 */ ,0 /* 23 */ ,0 /* 24 */ ,0 /* 25 */ ,0 /* 26 */ ,0 /* 27 */ ,0 /* 28 */ ,0 /* 29 */ ,0 /* 30 */ ,0 /* 31 */ ,0 /* 32 */ ,},{880+(char*)&Main_m_20_L_21},{0 /* 1 */ ,0 /* 2 */ ,0 /* 3 */ ,0 /* 4 */ ,0 /* 5 */ ,0 /* 6 */ ,0 /* 7 */ ,0 /* 8 */ ,0 /* 9 */ ,0 /* 10 */ ,0 /* 11 */ ,0 /* 12 */ ,0 /* 13 */ ,0 /* 14 */ ,0 /* 15 */ ,0 /* 16 */ ,0 /* 17 */ ,0 /* 18 */ ,0 /* 19 */ ,0 /* 20 */ ,0 /* 21 */ ,0 /* 22 */ ,0 /* 23 */ ,0 /* 24 */ ,},{112+(char*)&Main_m_M_Main_L_22},{0 /* 1 */ ,0 /* 2 */ ,0 /* 3 */ ,0 /* 4 */ ,0 /* 5 */ ,0 /* 6 */ ,0 /* 7 */ ,0 /* 8 */ ,},{(ADDRESS)&Main_M3},{INT64_(3)},{0 /* 1 */ ,0 /* 2 */ ,0 /* 3 */ ,0 /* 4 */ ,0 /* 5 */ ,0 /* 6 */ ,0 /* 7 */ 
,0 /* 8 */ ,0 /* 9 */ ,0 /* 10 */ ,0 /* 11 */ ,0 /* 12 */ ,0 /* 13 */ ,0 /* 14 */ ,0 /* 15 */ ,0 /* 16 */ ,},{(ADDRESS)&Main_I3,136+(char*)&Main_m_M_Main_L_22},{0 /* 1 */ ,0 /* 2 */ ,0 /* 3 */ ,0 /* 4 */ ,0 /* 5 */ ,0 /* 6 */ ,0 /* 7 */ ,0 /* 8 */ ,},{(ADDRESS)&IO_I3,160+(char*)&Main_m_M_Main_L_22},{0 /* 1 */ ,0 /* 2 */ ,0 /* 3 */ ,0 /* 4 */ ,0 /* 5 */ ,0 /* 6 */ ,0 /* 7 */ ,0 /* 8 */ ,},{(ADDRESS)&RTHooks_I3},{0 /* 1 */ ,0 /* 2 */ ,0 /* 3 */ ,0 /* 4 */ ,0 /* 5 */ ,0 /* 6 */ ,0 /* 7 */ ,0 /* 8 */ ,0 /* 9 */ ,0 /* 10 */ ,0 /* 11 */ ,0 /* 12 */ ,0 /* 13 */ ,0 /* 14 */ ,0 /* 15 */ ,0 /* 16 */ ,}};
 /* end: segments/globals */
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
#line 88 "../Main.m3"
 /* NL */
#line 88 "../Main.m3"
 /* set_source_line */
#line 88 "../Main.m3"
#line 7 "../Main.m3"
 /* begin_procedure */
#line 7 "../Main.m3"
struct Main__NL_Frame_t {
#line 7 "../Main.m3"
ADDRESS _unused;
#line 7 "../Main.m3"
};
#line 7 "../Main.m3"
void /*TypeText3*/ 
__cdecl
Main__NL(void)
{
#line 7 "../Main.m3"
Main__NL_Frame_t _frame;
#line 7 "../Main.m3"
_frame._unused=(ADDRESS)&_frame;
#line 7 "../Main.m3"
 /* start_call_direct */
#line 7 "../Main.m3"
 /* load_address */
#line 7 "../Main.m3"
 /* pop_param */
#line 7 "../Main.m3"
 /* load_nil */
#line 7 "../Main.m3"
 /* pop_param */
#line 7 "../Main.m3"
 /* call_direct */
#line 7 "../Main.m3"
IO__Put(
  ( TEXT )(((ADDRESS)(INT64_(48)+((ADDRESS)(&Main_m_20_L_21)))) ),
  ( Wr__T )(((ADDRESS)(0)) ));
#line 7 "../Main.m3"
 /* exit_proc */
#line 7 "../Main.m3"
return;
#line 7 "../Main.m3"
 /* end_procedure */
#line 7 "../Main.m3"
} /* GetStack */
#line 7 "../Main.m3"
 /* set_source_line */
#line 7 "../Main.m3"
#line 11 "../Main.m3"
 /* begin_procedure */
#line 11 "../Main.m3"
struct Main__GetStack_Frame_t {
#line 11 "../Main.m3"
ADDRESS _unused;
#line 11 "../Main.m3"
};
#line 11 "../Main.m3"
ADDRESS
__cdecl
Main__GetStack(void)
{
#line 11 "../Main.m3"
 /* Var_Type1 */ ADDRESS b_L_24={0};//always-init
#line 11 "../Main.m3"
Main__GetStack_Frame_t _frame;
#line 11 "../Main.m3"
_frame._unused=(ADDRESS)&_frame;
#line 11 "../Main.m3"
 /* set_source_line */
#line 11 "../Main.m3"
#line 12 "../Main.m3"
 /* load_address */
#line 12 "../Main.m3"
 /* store */
#line 12 "../Main.m3"
(*(ADDRESS*)(&b_L_24))=(ADDRESS)(((ADDRESS)(&b_L_24)));
#line 12 "../Main.m3"
 /* set_source_line */
#line 12 "../Main.m3"
#line 15 "../Main.m3"
 /* load */
#line 15 "../Main.m3"
 /* exit_proc */
#line 15 "../Main.m3"
return (ADDRESS)(b_L_24);
#line 15 "../Main.m3"
 /* end_procedure */
#line 15 "../Main.m3"
} /* GetStackHeight */
#line 15 "../Main.m3"
 /* set_source_line */
#line 15 "../Main.m3"
#line 18 "../Main.m3"
 /* begin_procedure */
#line 18 "../Main.m3"
struct Main__GetStackHeight_Frame_t {
#line 18 "../Main.m3"
ADDRESS _unused;
#line 18 "../Main.m3"
};
#line 18 "../Main.m3"
INTEGER
__cdecl
Main__GetStackHeight(void)
{
#line 18 "../Main.m3"
 /* Var_Type1 */ ADDRESS b_L_26={0};//always-init
#line 18 "../Main.m3"
 /* Var_Type2 */ ADDRESS Main_m_28_L_29={0};//always-init
#line 18 "../Main.m3"
Main__GetStackHeight_Frame_t _frame;
#line 18 "../Main.m3"
_frame._unused=(ADDRESS)&_frame;
#line 18 "../Main.m3"
 /* set_source_line */
#line 18 "../Main.m3"
#line 19 "../Main.m3"
 /* start_call_direct */
#line 19 "../Main.m3"
 /* call_direct */
#line 19 "../Main.m3"
 /* store */
#line 19 "../Main.m3"
(*(ADDRESS*)(&Main_m_28_L_29))=(ADDRESS)(((ADDRESS)(Main__GetStack(
 ))));
#line 19 "../Main.m3"
 /* load */
#line 19 "../Main.m3"
 /* store */
#line 19 "../Main.m3"
(*(ADDRESS*)(&b_L_26))=(ADDRESS)(((ADDRESS)(Main_m_28_L_29)));
#line 19 "../Main.m3"
 /* set_source_line */
#line 19 "../Main.m3"
#line 21 "../Main.m3"
 /* load */
#line 21 "../Main.m3"
 /* load */
#line 21 "../Main.m3"
 /* if_compare */
#line 21 "../Main.m3"
if(m3_ge(ADDRESS,
 ((ADDRESS)(*((ADDRESS*)(INT64_(104)+((ADDRESS)(&Main_m_M_Main_L_22)))))),
 ((ADDRESS)(b_L_26))))goto L2;
#line 21 "../Main.m3"
 /* set_source_line */
#line 21 "../Main.m3"
#line 22 "../Main.m3"
 /* load */
#line 22 "../Main.m3"
 /* loophole */
#line 22 "../Main.m3"
 /* load */
#line 22 "../Main.m3"
 /* loophole */
#line 22 "../Main.m3"
 /* subtract */
#line 22 "../Main.m3"
 /* exit_proc */
#line 22 "../Main.m3"
return ((UINT64)(((UINT64)((UINT64)b_L_26))-((UINT64)((UINT64)*((ADDRESS*)(INT64_(104)+((ADDRESS)(&Main_m_M_Main_L_22))))))));
#line 22 "../Main.m3"
 /* set_label */
#line 22 "../Main.m3"
L2:;
#line 22 "../Main.m3"
 /* set_source_line */
#line 22 "../Main.m3"
#line 24 "../Main.m3"
 /* load */
#line 24 "../Main.m3"
 /* loophole */
#line 24 "../Main.m3"
 /* load */
#line 24 "../Main.m3"
 /* loophole */
#line 24 "../Main.m3"
 /* subtract */
#line 24 "../Main.m3"
 /* exit_proc */
#line 24 "../Main.m3"
return ((UINT64)(((UINT64)((UINT64)*((ADDRESS*)(INT64_(104)+((ADDRESS)(&Main_m_M_Main_L_22))))))-((UINT64)((UINT64)b_L_26))));
#line 24 "../Main.m3"
 /* end_procedure */
#line 24 "../Main.m3"
} /* PrintStackHeight */
#line 24 "../Main.m3"
 /* set_source_line */
#line 24 "../Main.m3"
#line 27 "../Main.m3"
 /* begin_procedure */
#line 27 "../Main.m3"
struct Main__PrintStackHeight_Frame_t {
#line 27 "../Main.m3"
ADDRESS _unused;
#line 27 "../Main.m3"
};
#line 27 "../Main.m3"
void /*TypeText3*/ 
__cdecl
Main__PrintStackHeight(void)
{
#line 27 "../Main.m3"
 /* Var_Type2 */ INT64 Main_m_30_L_31={0};//always-init
#line 27 "../Main.m3"
Main__PrintStackHeight_Frame_t _frame;
#line 27 "../Main.m3"
_frame._unused=(ADDRESS)&_frame;
#line 27 "../Main.m3"
 /* set_source_line */
#line 27 "../Main.m3"
#line 28 "../Main.m3"
 /* set_source_line */
#line 28 "../Main.m3"
#line 29 "../Main.m3"
 /* start_call_direct */
#line 29 "../Main.m3"
 /* load_address */
#line 29 "../Main.m3"
 /* pop_param */
#line 29 "../Main.m3"
 /* load_nil */
#line 29 "../Main.m3"
 /* pop_param */
#line 29 "../Main.m3"
 /* call_direct */
#line 29 "../Main.m3"
IO__Put(
  ( TEXT )(((ADDRESS)(INT64_(80)+((ADDRESS)(&Main_m_20_L_21)))) ),
  ( Wr__T )(((ADDRESS)(0)) ));
#line 29 "../Main.m3"
 /* set_source_line */
#line 29 "../Main.m3"
#line 30 "../Main.m3"
 /* start_call_direct */
#line 30 "../Main.m3"
 /* call_direct */
#line 30 "../Main.m3"
 /* store */
#line 30 "../Main.m3"
(*(INT64*)(&Main_m_30_L_31))=(INT64)(((INT64)(Main__GetStackHeight(
 ))));
#line 30 "../Main.m3"
 /* start_call_direct */
#line 30 "../Main.m3"
 /* load */
#line 30 "../Main.m3"
 /* pop_param */
#line 30 "../Main.m3"
 /* load_nil */
#line 30 "../Main.m3"
 /* pop_param */
#line 30 "../Main.m3"
 /* call_direct */
#line 30 "../Main.m3"
IO__PutInt(
  ( INTEGER )( Main_m_30_L_31 ),
  ( Wr__T )(((ADDRESS)(0)) ));
#line 30 "../Main.m3"
 /* set_source_line */
#line 30 "../Main.m3"
#line 31 "../Main.m3"
 /* start_call_direct */
#line 31 "../Main.m3"
 /* load_address */
#line 31 "../Main.m3"
 /* pop_param */
#line 31 "../Main.m3"
 /* load_nil */
#line 31 "../Main.m3"
 /* pop_param */
#line 31 "../Main.m3"
 /* call_direct */
#line 31 "../Main.m3"
IO__Put(
  ( TEXT )(((ADDRESS)(INT64_(120)+((ADDRESS)(&Main_m_20_L_21)))) ),
  ( Wr__T )(((ADDRESS)(0)) ));
#line 31 "../Main.m3"
 /* set_source_line */
#line 31 "../Main.m3"
#line 32 "../Main.m3"
 /* start_call_direct */
#line 32 "../Main.m3"
 /* call_direct */
#line 32 "../Main.m3"
Main__NL(
 );
#line 32 "../Main.m3"
 /* set_source_line */
#line 32 "../Main.m3"
#line 33 "../Main.m3"
 /* exit_proc */
#line 33 "../Main.m3"
return;
#line 33 "../Main.m3"
 /* end_procedure */
#line 33 "../Main.m3"
} /* Try1 */
#line 33 "../Main.m3"
 /* set_source_line */
#line 33 "../Main.m3"
#line 35 "../Main.m3"
 /* begin_procedure */
#line 35 "../Main.m3"
struct Main__Try1_Frame_t {
#line 35 "../Main.m3"
ADDRESS _unused;
#line 35 "../Main.m3"
};
#line 35 "../Main.m3"
void /*TypeText3*/ 
__cdecl
Main__Try1(void)
{
#line 35 "../Main.m3"
 /* Var_Type2 */ ADDRESS Main_m_32_L_33={0};//always-init
#line 35 "../Main.m3"
Main__Try1_Frame_t _frame;
#line 35 "../Main.m3"
_frame._unused=(ADDRESS)&_frame;
#line 35 "../Main.m3"
 /* note_procedure_origin */
#line 35 "../Main.m3"
 /* set_source_line */
#line 35 "../Main.m3"
#line 36 "../Main.m3"
 /* set_source_line */
#line 36 "../Main.m3"
#line 42 "../Main.m3"
 /* start_call_direct */
#line 42 "../Main.m3"
 /* load_address */
#line 42 "../Main.m3"
 /* pop_param */
#line 42 "../Main.m3"
 /* load_nil */
#line 42 "../Main.m3"
 /* pop_param */
#line 42 "../Main.m3"
 /* call_direct */
#line 42 "../Main.m3"
IO__Put(
  ( TEXT )(((ADDRESS)(INT64_(232)+((ADDRESS)(&Main_m_20_L_21)))) ),
  ( Wr__T )(((ADDRESS)(0)) ));
#line 42 "../Main.m3"
 /* start_call_direct */
#line 42 "../Main.m3"
 /* call_direct */
#line 42 "../Main.m3"
Main__NL(
 );
#line 42 "../Main.m3"
 /* set_source_line */
#line 42 "../Main.m3"
#line 43 "../Main.m3"
 /* start_call_direct */
#line 43 "../Main.m3"
 /* call_direct */
#line 43 "../Main.m3"
 /* get_static_link */
#line 43 "../Main.m3"
Main__Try1__Try1_Try2(
  ( Main__Try1_Frame_t* )(&_frame ));
#line 43 "../Main.m3"
 /* set_source_line */
#line 43 "../Main.m3"
#line 44 "../Main.m3"
 /* load_nil */
#line 44 "../Main.m3"
 /* store */
#line 44 "../Main.m3"
(*(ADDRESS*)(&Main_m_32_L_33))=(ADDRESS)(((ADDRESS)(0)));
#line 44 "../Main.m3"
 /* set_label */
#line 44 "../Main.m3"
 /* start_try */
#line 44 "../Main.m3"
try {
#line 44 "../Main.m3"
 /* start_call_direct */
#line 44 "../Main.m3"
 /* invoke_direct */
#line 44 "../Main.m3"
 /* call_direct */
#line 44 "../Main.m3"
Main__PrintStackHeight(
 );
#line 44 "../Main.m3"
 /* set_label */
#line 44 "../Main.m3"
 /* jump */
#line 44 "../Main.m3"
goto L6;
#line 44 "../Main.m3"
 /* end_try */
#line 44 "../Main.m3"
} catch (_M3Exc& _m3exc) { _m3_caught = _m3exc.act; goto L4; }
#line 44 "../Main.m3"
 /* set_label */
#line 44 "../Main.m3"
L4:;
#line 44 "../Main.m3"
 /* landing_pad */
#line 44 "../Main.m3"
 /* store */
#line 44 "../Main.m3"
(*(ADDRESS*)(&Main_m_32_L_33))=(ADDRESS)(((ADDRESS)(_m3_caught)));
#line 44 "../Main.m3"
 /* set_label */
#line 44 "../Main.m3"
 /* set_label */
#line 44 "../Main.m3"
L6:;
#line 44 "../Main.m3"
 /* set_source_line */
#line 44 "../Main.m3"
#line 45 "../Main.m3"
 /* start_call_direct */
#line 45 "../Main.m3"
 /* load_address */
#line 45 "../Main.m3"
 /* pop_param */
#line 45 "../Main.m3"
 /* load_nil */
#line 45 "../Main.m3"
 /* pop_param */
#line 45 "../Main.m3"
 /* call_direct */
#line 45 "../Main.m3"
IO__Put(
  ( TEXT )(((ADDRESS)(INT64_(264)+((ADDRESS)(&Main_m_20_L_21)))) ),
  ( Wr__T )(((ADDRESS)(0)) ));
#line 45 "../Main.m3"
 /* start_call_direct */
#line 45 "../Main.m3"
 /* call_direct */
#line 45 "../Main.m3"
Main__NL(
 );
#line 45 "../Main.m3"
 /* set_source_line */
#line 45 "../Main.m3"
#line 46 "../Main.m3"
 /* exit_proc */
#line 46 "../Main.m3"
return;
#line 46 "../Main.m3"
 /* end_procedure */
#line 46 "../Main.m3"
} /* Try1.Try1_Try2 */
#line 46 "../Main.m3"
 /* set_source_line */
#line 46 "../Main.m3"
#line 36 "../Main.m3"
 /* begin_procedure */
#line 36 "../Main.m3"
struct Main__Try1__Try1_Try2_Frame_t {
#line 36 "../Main.m3"
ADDRESS _unused;
#line 36 "../Main.m3"
 /* Var_Type1 */ Main__Try1_Frame_t* _static_link;
#line 36 "../Main.m3"
};
#line 36 "../Main.m3"
void /*TypeText3*/ 
__cdecl
Main__Try1__Try1_Try2(
   /* Param_Type1 */ Main__Try1_Frame_t* _static_link)
{
#line 36 "../Main.m3"
 /* Var_Type2 */ ADDRESS Main_m_34_L_35={0};//always-init
#line 36 "../Main.m3"
 /* Var_Type2 */ ADDRESS Main_m_36_L_37={0};//always-init
#line 36 "../Main.m3"
Main__Try1__Try1_Try2_Frame_t _frame;
#line 36 "../Main.m3"
_frame._static_link=_static_link;
#line 36 "../Main.m3"
_frame._unused=(ADDRESS)&_frame;
#line 36 "../Main.m3"
 /* set_source_line */
#line 36 "../Main.m3"
#line 37 "../Main.m3"
 /* start_call_direct */
#line 37 "../Main.m3"
 /* load_address */
#line 37 "../Main.m3"
 /* pop_param */
#line 37 "../Main.m3"
 /* load_nil */
#line 37 "../Main.m3"
 /* pop_param */
#line 37 "../Main.m3"
 /* call_direct */
#line 37 "../Main.m3"
IO__Put(
  ( TEXT )(((ADDRESS)(INT64_(152)+((ADDRESS)(&Main_m_20_L_21)))) ),
  ( Wr__T )(((ADDRESS)(0)) ));
#line 37 "../Main.m3"
 /* start_call_direct */
#line 37 "../Main.m3"
 /* call_direct */
#line 37 "../Main.m3"
Main__NL(
 );
#line 37 "../Main.m3"
 /* set_source_line */
#line 37 "../Main.m3"
#line 38 "../Main.m3"
 /* load_nil */
#line 38 "../Main.m3"
 /* store */
#line 38 "../Main.m3"
(*(ADDRESS*)(&Main_m_34_L_35))=(ADDRESS)(((ADDRESS)(0)));
#line 38 "../Main.m3"
 /* set_label */
#line 38 "../Main.m3"
 /* start_try */
#line 38 "../Main.m3"
try {
#line 38 "../Main.m3"
 /* load_nil */
#line 38 "../Main.m3"
 /* store */
#line 38 "../Main.m3"
(*(ADDRESS*)(&Main_m_36_L_37))=(ADDRESS)(((ADDRESS)(0)));
#line 38 "../Main.m3"
 /* set_label */
#line 38 "../Main.m3"
 /* start_try */
#line 38 "../Main.m3"
try {
#line 38 "../Main.m3"
 /* start_call_direct */
#line 38 "../Main.m3"
 /* invoke_direct */
#line 38 "../Main.m3"
 /* call_direct */
#line 38 "../Main.m3"
Main__PrintStackHeight(
 );
#line 38 "../Main.m3"
 /* set_label */
#line 38 "../Main.m3"
 /* jump */
#line 38 "../Main.m3"
goto LF;
#line 38 "../Main.m3"
 /* end_try */
#line 38 "../Main.m3"
} catch (_M3Exc& _m3exc) { _m3_caught = _m3exc.act; goto LD; }
#line 38 "../Main.m3"
 /* set_label */
#line 38 "../Main.m3"
LD:;
#line 38 "../Main.m3"
 /* landing_pad */
#line 38 "../Main.m3"
 /* store */
#line 38 "../Main.m3"
(*(ADDRESS*)(&Main_m_36_L_37))=(ADDRESS)(((ADDRESS)(_m3_caught)));
#line 38 "../Main.m3"
 /* set_label */
#line 38 "../Main.m3"
 /* set_label */
#line 38 "../Main.m3"
LF:;
#line 38 "../Main.m3"
 /* jump */
#line 38 "../Main.m3"
goto LB;
#line 38 "../Main.m3"
 /* end_try */
#line 38 "../Main.m3"
} catch (...) { throw; }
#line 38 "../Main.m3"
 /* set_label */
#line 38 "../Main.m3"
 /* landing_pad */
#line 38 "../Main.m3"
 /* store */
#line 38 "../Main.m3"
(*(ADDRESS*)(&Main_m_34_L_35))=(ADDRESS)(((ADDRESS)(_m3_caught)));
#line 38 "../Main.m3"
 /* set_label */
#line 38 "../Main.m3"
 /* set_label */
#line 38 "../Main.m3"
LB:;
#line 38 "../Main.m3"
 /* set_source_line */
#line 38 "../Main.m3"
#line 39 "../Main.m3"
 /* start_call_direct */
#line 39 "../Main.m3"
 /* load_address */
#line 39 "../Main.m3"
 /* pop_param */
#line 39 "../Main.m3"
 /* load_nil */
#line 39 "../Main.m3"
 /* pop_param */
#line 39 "../Main.m3"
 /* call_direct */
#line 39 "../Main.m3"
IO__Put(
  ( TEXT )(((ADDRESS)(INT64_(192)+((ADDRESS)(&Main_m_20_L_21)))) ),
  ( Wr__T )(((ADDRESS)(0)) ));
#line 39 "../Main.m3"
 /* start_call_direct */
#line 39 "../Main.m3"
 /* call_direct */
#line 39 "../Main.m3"
Main__NL(
 );
#line 39 "../Main.m3"
 /* set_source_line */
#line 39 "../Main.m3"
#line 40 "../Main.m3"
 /* exit_proc */
#line 40 "../Main.m3"
return;
#line 40 "../Main.m3"
 /* end_procedure */
#line 40 "../Main.m3"
} /* Try2 */
#line 40 "../Main.m3"
 /* set_source_line */
#line 40 "../Main.m3"
#line 48 "../Main.m3"
 /* begin_procedure */
#line 48 "../Main.m3"
struct Main__Try2_Frame_t {
#line 48 "../Main.m3"
ADDRESS _unused;
#line 48 "../Main.m3"
};
#line 48 "../Main.m3"
void /*TypeText3*/ 
__cdecl
Main__Try2(void)
{
#line 48 "../Main.m3"
 /* Var_Type2 */ ADDRESS Main_m_38_L_39={0};//always-init
#line 48 "../Main.m3"
 /* Var_Type2 */ ADDRESS Main_m_40_L_41={0};//always-init
#line 48 "../Main.m3"
Main__Try2_Frame_t _frame;
#line 48 "../Main.m3"
_frame._unused=(ADDRESS)&_frame;
#line 48 "../Main.m3"
 /* note_procedure_origin */
#line 48 "../Main.m3"
 /* set_source_line */
#line 48 "../Main.m3"
#line 49 "../Main.m3"
 /* set_source_line */
#line 49 "../Main.m3"
#line 55 "../Main.m3"
 /* start_call_direct */
#line 55 "../Main.m3"
 /* load_address */
#line 55 "../Main.m3"
 /* pop_param */
#line 55 "../Main.m3"
 /* load_nil */
#line 55 "../Main.m3"
 /* pop_param */
#line 55 "../Main.m3"
 /* call_direct */
#line 55 "../Main.m3"
IO__Put(
  ( TEXT )(((ADDRESS)(INT64_(376)+((ADDRESS)(&Main_m_20_L_21)))) ),
  ( Wr__T )(((ADDRESS)(0)) ));
#line 55 "../Main.m3"
 /* start_call_direct */
#line 55 "../Main.m3"
 /* call_direct */
#line 55 "../Main.m3"
Main__NL(
 );
#line 55 "../Main.m3"
 /* set_source_line */
#line 55 "../Main.m3"
#line 56 "../Main.m3"
 /* start_call_direct */
#line 56 "../Main.m3"
 /* call_direct */
#line 56 "../Main.m3"
 /* get_static_link */
#line 56 "../Main.m3"
Main__Try2__Try2_Try2(
  ( Main__Try2_Frame_t* )(&_frame ));
#line 56 "../Main.m3"
 /* set_source_line */
#line 56 "../Main.m3"
#line 57 "../Main.m3"
 /* load_nil */
#line 57 "../Main.m3"
 /* store */
#line 57 "../Main.m3"
(*(ADDRESS*)(&Main_m_38_L_39))=(ADDRESS)(((ADDRESS)(0)));
#line 57 "../Main.m3"
 /* set_label */
#line 57 "../Main.m3"
 /* start_try */
#line 57 "../Main.m3"
try {
#line 57 "../Main.m3"
 /* load_nil */
#line 57 "../Main.m3"
 /* store */
#line 57 "../Main.m3"
(*(ADDRESS*)(&Main_m_40_L_41))=(ADDRESS)(((ADDRESS)(0)));
#line 57 "../Main.m3"
 /* set_label */
#line 57 "../Main.m3"
 /* start_try */
#line 57 "../Main.m3"
try {
#line 57 "../Main.m3"
 /* start_call_direct */
#line 57 "../Main.m3"
 /* invoke_direct */
#line 57 "../Main.m3"
 /* call_direct */
#line 57 "../Main.m3"
Main__PrintStackHeight(
 );
#line 57 "../Main.m3"
 /* set_label */
#line 57 "../Main.m3"
 /* jump */
#line 57 "../Main.m3"
goto L18;
#line 57 "../Main.m3"
 /* end_try */
#line 57 "../Main.m3"
} catch (_M3Exc& _m3exc) { _m3_caught = _m3exc.act; goto L16; }
#line 57 "../Main.m3"
 /* set_label */
#line 57 "../Main.m3"
L16:;
#line 57 "../Main.m3"
 /* landing_pad */
#line 57 "../Main.m3"
 /* store */
#line 57 "../Main.m3"
(*(ADDRESS*)(&Main_m_40_L_41))=(ADDRESS)(((ADDRESS)(_m3_caught)));
#line 57 "../Main.m3"
 /* set_label */
#line 57 "../Main.m3"
 /* set_label */
#line 57 "../Main.m3"
L18:;
#line 57 "../Main.m3"
 /* jump */
#line 57 "../Main.m3"
goto L14;
#line 57 "../Main.m3"
 /* end_try */
#line 57 "../Main.m3"
} catch (...) { throw; }
#line 57 "../Main.m3"
 /* set_label */
#line 57 "../Main.m3"
 /* landing_pad */
#line 57 "../Main.m3"
 /* store */
#line 57 "../Main.m3"
(*(ADDRESS*)(&Main_m_38_L_39))=(ADDRESS)(((ADDRESS)(_m3_caught)));
#line 57 "../Main.m3"
 /* set_label */
#line 57 "../Main.m3"
 /* set_label */
#line 57 "../Main.m3"
L14:;
#line 57 "../Main.m3"
 /* set_source_line */
#line 57 "../Main.m3"
#line 58 "../Main.m3"
 /* start_call_direct */
#line 58 "../Main.m3"
 /* load_address */
#line 58 "../Main.m3"
 /* pop_param */
#line 58 "../Main.m3"
 /* load_nil */
#line 58 "../Main.m3"
 /* pop_param */
#line 58 "../Main.m3"
 /* call_direct */
#line 58 "../Main.m3"
IO__Put(
  ( TEXT )(((ADDRESS)(INT64_(408)+((ADDRESS)(&Main_m_20_L_21)))) ),
  ( Wr__T )(((ADDRESS)(0)) ));
#line 58 "../Main.m3"
 /* start_call_direct */
#line 58 "../Main.m3"
 /* call_direct */
#line 58 "../Main.m3"
Main__NL(
 );
#line 58 "../Main.m3"
 /* set_source_line */
#line 58 "../Main.m3"
#line 59 "../Main.m3"
 /* exit_proc */
#line 59 "../Main.m3"
return;
#line 59 "../Main.m3"
 /* end_procedure */
#line 59 "../Main.m3"
} /* Try2.Try2_Try2 */
#line 59 "../Main.m3"
 /* set_source_line */
#line 59 "../Main.m3"
#line 49 "../Main.m3"
 /* begin_procedure */
#line 49 "../Main.m3"
struct Main__Try2__Try2_Try2_Frame_t {
#line 49 "../Main.m3"
ADDRESS _unused;
#line 49 "../Main.m3"
 /* Var_Type1 */ Main__Try2_Frame_t* _static_link;
#line 49 "../Main.m3"
};
#line 49 "../Main.m3"
void /*TypeText3*/ 
__cdecl
Main__Try2__Try2_Try2(
   /* Param_Type1 */ Main__Try2_Frame_t* _static_link)
{
#line 49 "../Main.m3"
 /* Var_Type2 */ ADDRESS Main_m_42_L_43={0};//always-init
#line 49 "../Main.m3"
 /* Var_Type2 */ ADDRESS Main_m_44_L_45={0};//always-init
#line 49 "../Main.m3"
Main__Try2__Try2_Try2_Frame_t _frame;
#line 49 "../Main.m3"
_frame._static_link=_static_link;
#line 49 "../Main.m3"
_frame._unused=(ADDRESS)&_frame;
#line 49 "../Main.m3"
 /* set_source_line */
#line 49 "../Main.m3"
#line 50 "../Main.m3"
 /* start_call_direct */
#line 50 "../Main.m3"
 /* load_address */
#line 50 "../Main.m3"
 /* pop_param */
#line 50 "../Main.m3"
 /* load_nil */
#line 50 "../Main.m3"
 /* pop_param */
#line 50 "../Main.m3"
 /* call_direct */
#line 50 "../Main.m3"
IO__Put(
  ( TEXT )(((ADDRESS)(INT64_(296)+((ADDRESS)(&Main_m_20_L_21)))) ),
  ( Wr__T )(((ADDRESS)(0)) ));
#line 50 "../Main.m3"
 /* start_call_direct */
#line 50 "../Main.m3"
 /* call_direct */
#line 50 "../Main.m3"
Main__NL(
 );
#line 50 "../Main.m3"
 /* set_source_line */
#line 50 "../Main.m3"
#line 51 "../Main.m3"
 /* load_nil */
#line 51 "../Main.m3"
 /* store */
#line 51 "../Main.m3"
(*(ADDRESS*)(&Main_m_42_L_43))=(ADDRESS)(((ADDRESS)(0)));
#line 51 "../Main.m3"
 /* set_label */
#line 51 "../Main.m3"
 /* start_try */
#line 51 "../Main.m3"
try {
#line 51 "../Main.m3"
 /* load_nil */
#line 51 "../Main.m3"
 /* store */
#line 51 "../Main.m3"
(*(ADDRESS*)(&Main_m_44_L_45))=(ADDRESS)(((ADDRESS)(0)));
#line 51 "../Main.m3"
 /* set_label */
#line 51 "../Main.m3"
 /* start_try */
#line 51 "../Main.m3"
try {
#line 51 "../Main.m3"
 /* start_call_direct */
#line 51 "../Main.m3"
 /* invoke_direct */
#line 51 "../Main.m3"
 /* call_direct */
#line 51 "../Main.m3"
Main__PrintStackHeight(
 );
#line 51 "../Main.m3"
 /* set_label */
#line 51 "../Main.m3"
 /* jump */
#line 51 "../Main.m3"
goto L21;
#line 51 "../Main.m3"
 /* end_try */
#line 51 "../Main.m3"
} catch (_M3Exc& _m3exc) { _m3_caught = _m3exc.act; goto L1F; }
#line 51 "../Main.m3"
 /* set_label */
#line 51 "../Main.m3"
L1F:;
#line 51 "../Main.m3"
 /* landing_pad */
#line 51 "../Main.m3"
 /* store */
#line 51 "../Main.m3"
(*(ADDRESS*)(&Main_m_44_L_45))=(ADDRESS)(((ADDRESS)(_m3_caught)));
#line 51 "../Main.m3"
 /* set_label */
#line 51 "../Main.m3"
 /* set_label */
#line 51 "../Main.m3"
L21:;
#line 51 "../Main.m3"
 /* jump */
#line 51 "../Main.m3"
goto L1D;
#line 51 "../Main.m3"
 /* end_try */
#line 51 "../Main.m3"
} catch (...) { throw; }
#line 51 "../Main.m3"
 /* set_label */
#line 51 "../Main.m3"
 /* landing_pad */
#line 51 "../Main.m3"
 /* store */
#line 51 "../Main.m3"
(*(ADDRESS*)(&Main_m_42_L_43))=(ADDRESS)(((ADDRESS)(_m3_caught)));
#line 51 "../Main.m3"
 /* set_label */
#line 51 "../Main.m3"
 /* set_label */
#line 51 "../Main.m3"
L1D:;
#line 51 "../Main.m3"
 /* set_source_line */
#line 51 "../Main.m3"
#line 52 "../Main.m3"
 /* start_call_direct */
#line 52 "../Main.m3"
 /* load_address */
#line 52 "../Main.m3"
 /* pop_param */
#line 52 "../Main.m3"
 /* load_nil */
#line 52 "../Main.m3"
 /* pop_param */
#line 52 "../Main.m3"
 /* call_direct */
#line 52 "../Main.m3"
IO__Put(
  ( TEXT )(((ADDRESS)(INT64_(336)+((ADDRESS)(&Main_m_20_L_21)))) ),
  ( Wr__T )(((ADDRESS)(0)) ));
#line 52 "../Main.m3"
 /* start_call_direct */
#line 52 "../Main.m3"
 /* call_direct */
#line 52 "../Main.m3"
Main__NL(
 );
#line 52 "../Main.m3"
 /* set_source_line */
#line 52 "../Main.m3"
#line 53 "../Main.m3"
 /* exit_proc */
#line 53 "../Main.m3"
return;
#line 53 "../Main.m3"
 /* end_procedure */
#line 53 "../Main.m3"
} /* Try3 */
#line 53 "../Main.m3"
 /* set_source_line */
#line 53 "../Main.m3"
#line 61 "../Main.m3"
 /* begin_procedure */
#line 61 "../Main.m3"
struct Main__Try3_Frame_t {
#line 61 "../Main.m3"
ADDRESS _unused;
#line 61 "../Main.m3"
};
#line 61 "../Main.m3"
void /*TypeText3*/ 
__cdecl
Main__Try3(void)
{
#line 61 "../Main.m3"
 /* Var_Type2 */ ADDRESS Main_m_46_L_47={0};//always-init
#line 61 "../Main.m3"
 /* Var_Type2 */ ADDRESS Main_m_48_L_49={0};//always-init
#line 61 "../Main.m3"
 /* Var_Type2 */ ADDRESS Main_m_50_L_51={0};//always-init
#line 61 "../Main.m3"
Main__Try3_Frame_t _frame;
#line 61 "../Main.m3"
_frame._unused=(ADDRESS)&_frame;
#line 61 "../Main.m3"
 /* note_procedure_origin */
#line 61 "../Main.m3"
 /* note_procedure_origin */
#line 61 "../Main.m3"
 /* note_procedure_origin */
#line 61 "../Main.m3"
 /* set_source_line */
#line 61 "../Main.m3"
#line 62 "../Main.m3"
 /* set_source_line */
#line 62 "../Main.m3"
#line 80 "../Main.m3"
 /* start_call_direct */
#line 80 "../Main.m3"
 /* load_address */
#line 80 "../Main.m3"
 /* pop_param */
#line 80 "../Main.m3"
 /* load_nil */
#line 80 "../Main.m3"
 /* pop_param */
#line 80 "../Main.m3"
 /* call_direct */
#line 80 "../Main.m3"
IO__Put(
  ( TEXT )(((ADDRESS)(INT64_(680)+((ADDRESS)(&Main_m_20_L_21)))) ),
  ( Wr__T )(((ADDRESS)(0)) ));
#line 80 "../Main.m3"
 /* start_call_direct */
#line 80 "../Main.m3"
 /* call_direct */
#line 80 "../Main.m3"
Main__NL(
 );
#line 80 "../Main.m3"
 /* set_source_line */
#line 80 "../Main.m3"
#line 81 "../Main.m3"
 /* start_call_direct */
#line 81 "../Main.m3"
 /* call_direct */
#line 81 "../Main.m3"
 /* get_static_link */
#line 81 "../Main.m3"
Main__Try3__Try3_Try1(
  ( Main__Try3_Frame_t* )(&_frame ));
#line 81 "../Main.m3"
 /* set_source_line */
#line 81 "../Main.m3"
#line 82 "../Main.m3"
 /* start_call_direct */
#line 82 "../Main.m3"
 /* call_direct */
#line 82 "../Main.m3"
 /* get_static_link */
#line 82 "../Main.m3"
Main__Try3__Try3_Try2(
  ( Main__Try3_Frame_t* )(&_frame ));
#line 82 "../Main.m3"
 /* set_source_line */
#line 82 "../Main.m3"
#line 83 "../Main.m3"
 /* start_call_direct */
#line 83 "../Main.m3"
 /* call_direct */
#line 83 "../Main.m3"
 /* get_static_link */
#line 83 "../Main.m3"
Main__Try3__Try3_Try3(
  ( Main__Try3_Frame_t* )(&_frame ));
#line 83 "../Main.m3"
 /* set_source_line */
#line 83 "../Main.m3"
#line 84 "../Main.m3"
 /* load_nil */
#line 84 "../Main.m3"
 /* store */
#line 84 "../Main.m3"
(*(ADDRESS*)(&Main_m_46_L_47))=(ADDRESS)(((ADDRESS)(0)));
#line 84 "../Main.m3"
 /* set_label */
#line 84 "../Main.m3"
 /* start_try */
#line 84 "../Main.m3"
try {
#line 84 "../Main.m3"
 /* load_nil */
#line 84 "../Main.m3"
 /* store */
#line 84 "../Main.m3"
(*(ADDRESS*)(&Main_m_48_L_49))=(ADDRESS)(((ADDRESS)(0)));
#line 84 "../Main.m3"
 /* set_label */
#line 84 "../Main.m3"
 /* start_try */
#line 84 "../Main.m3"
try {
#line 84 "../Main.m3"
 /* load_nil */
#line 84 "../Main.m3"
 /* store */
#line 84 "../Main.m3"
(*(ADDRESS*)(&Main_m_50_L_51))=(ADDRESS)(((ADDRESS)(0)));
#line 84 "../Main.m3"
 /* set_label */
#line 84 "../Main.m3"
 /* start_try */
#line 84 "../Main.m3"
try {
#line 84 "../Main.m3"
 /* start_call_direct */
#line 84 "../Main.m3"
 /* invoke_direct */
#line 84 "../Main.m3"
 /* call_direct */
#line 84 "../Main.m3"
Main__PrintStackHeight(
 );
#line 84 "../Main.m3"
 /* set_label */
#line 84 "../Main.m3"
 /* jump */
#line 84 "../Main.m3"
goto L2E;
#line 84 "../Main.m3"
 /* end_try */
#line 84 "../Main.m3"
} catch (_M3Exc& _m3exc) { _m3_caught = _m3exc.act; goto L2C; }
#line 84 "../Main.m3"
 /* set_label */
#line 84 "../Main.m3"
L2C:;
#line 84 "../Main.m3"
 /* landing_pad */
#line 84 "../Main.m3"
 /* store */
#line 84 "../Main.m3"
(*(ADDRESS*)(&Main_m_50_L_51))=(ADDRESS)(((ADDRESS)(_m3_caught)));
#line 84 "../Main.m3"
 /* set_label */
#line 84 "../Main.m3"
 /* set_label */
#line 84 "../Main.m3"
L2E:;
#line 84 "../Main.m3"
 /* jump */
#line 84 "../Main.m3"
goto L2A;
#line 84 "../Main.m3"
 /* end_try */
#line 84 "../Main.m3"
} catch (...) { throw; }
#line 84 "../Main.m3"
 /* set_label */
#line 84 "../Main.m3"
 /* landing_pad */
#line 84 "../Main.m3"
 /* store */
#line 84 "../Main.m3"
(*(ADDRESS*)(&Main_m_48_L_49))=(ADDRESS)(((ADDRESS)(_m3_caught)));
#line 84 "../Main.m3"
 /* set_label */
#line 84 "../Main.m3"
 /* set_label */
#line 84 "../Main.m3"
L2A:;
#line 84 "../Main.m3"
 /* jump */
#line 84 "../Main.m3"
goto L26;
#line 84 "../Main.m3"
 /* end_try */
#line 84 "../Main.m3"
} catch (...) { throw; }
#line 84 "../Main.m3"
 /* set_label */
#line 84 "../Main.m3"
 /* landing_pad */
#line 84 "../Main.m3"
 /* store */
#line 84 "../Main.m3"
(*(ADDRESS*)(&Main_m_46_L_47))=(ADDRESS)(((ADDRESS)(_m3_caught)));
#line 84 "../Main.m3"
 /* set_label */
#line 84 "../Main.m3"
 /* set_label */
#line 84 "../Main.m3"
L26:;
#line 84 "../Main.m3"
 /* set_source_line */
#line 84 "../Main.m3"
#line 85 "../Main.m3"
 /* start_call_direct */
#line 85 "../Main.m3"
 /* load_address */
#line 85 "../Main.m3"
 /* pop_param */
#line 85 "../Main.m3"
 /* load_nil */
#line 85 "../Main.m3"
 /* pop_param */
#line 85 "../Main.m3"
 /* call_direct */
#line 85 "../Main.m3"
IO__Put(
  ( TEXT )(((ADDRESS)(INT64_(712)+((ADDRESS)(&Main_m_20_L_21)))) ),
  ( Wr__T )(((ADDRESS)(0)) ));
#line 85 "../Main.m3"
 /* start_call_direct */
#line 85 "../Main.m3"
 /* call_direct */
#line 85 "../Main.m3"
Main__NL(
 );
#line 85 "../Main.m3"
 /* set_source_line */
#line 85 "../Main.m3"
#line 86 "../Main.m3"
 /* exit_proc */
#line 86 "../Main.m3"
return;
#line 86 "../Main.m3"
 /* end_procedure */
#line 86 "../Main.m3"
} /* Try3.Try3_Try1 */
#line 86 "../Main.m3"
 /* set_source_line */
#line 86 "../Main.m3"
#line 62 "../Main.m3"
 /* begin_procedure */
#line 62 "../Main.m3"
struct Main__Try3__Try3_Try1_Frame_t {
#line 62 "../Main.m3"
ADDRESS _unused;
#line 62 "../Main.m3"
 /* Var_Type1 */ Main__Try3_Frame_t* _static_link;
#line 62 "../Main.m3"
};
#line 62 "../Main.m3"
void /*TypeText3*/ 
__cdecl
Main__Try3__Try3_Try1(
   /* Param_Type1 */ Main__Try3_Frame_t* _static_link)
{
#line 62 "../Main.m3"
 /* Var_Type2 */ ADDRESS Main_m_52_L_53={0};//always-init
#line 62 "../Main.m3"
Main__Try3__Try3_Try1_Frame_t _frame;
#line 62 "../Main.m3"
_frame._static_link=_static_link;
#line 62 "../Main.m3"
_frame._unused=(ADDRESS)&_frame;
#line 62 "../Main.m3"
 /* set_source_line */
#line 62 "../Main.m3"
#line 63 "../Main.m3"
 /* start_call_direct */
#line 63 "../Main.m3"
 /* load_address */
#line 63 "../Main.m3"
 /* pop_param */
#line 63 "../Main.m3"
 /* load_nil */
#line 63 "../Main.m3"
 /* pop_param */
#line 63 "../Main.m3"
 /* call_direct */
#line 63 "../Main.m3"
IO__Put(
  ( TEXT )(((ADDRESS)(INT64_(440)+((ADDRESS)(&Main_m_20_L_21)))) ),
  ( Wr__T )(((ADDRESS)(0)) ));
#line 63 "../Main.m3"
 /* start_call_direct */
#line 63 "../Main.m3"
 /* call_direct */
#line 63 "../Main.m3"
Main__NL(
 );
#line 63 "../Main.m3"
 /* set_source_line */
#line 63 "../Main.m3"
#line 64 "../Main.m3"
 /* load_nil */
#line 64 "../Main.m3"
 /* store */
#line 64 "../Main.m3"
(*(ADDRESS*)(&Main_m_52_L_53))=(ADDRESS)(((ADDRESS)(0)));
#line 64 "../Main.m3"
 /* set_label */
#line 64 "../Main.m3"
 /* start_try */
#line 64 "../Main.m3"
try {
#line 64 "../Main.m3"
 /* start_call_direct */
#line 64 "../Main.m3"
 /* invoke_direct */
#line 64 "../Main.m3"
 /* call_direct */
#line 64 "../Main.m3"
Main__PrintStackHeight(
 );
#line 64 "../Main.m3"
 /* set_label */
#line 64 "../Main.m3"
 /* jump */
#line 64 "../Main.m3"
goto L33;
#line 64 "../Main.m3"
 /* end_try */
#line 64 "../Main.m3"
} catch (_M3Exc& _m3exc) { _m3_caught = _m3exc.act; goto L31; }
#line 64 "../Main.m3"
 /* set_label */
#line 64 "../Main.m3"
L31:;
#line 64 "../Main.m3"
 /* landing_pad */
#line 64 "../Main.m3"
 /* store */
#line 64 "../Main.m3"
(*(ADDRESS*)(&Main_m_52_L_53))=(ADDRESS)(((ADDRESS)(_m3_caught)));
#line 64 "../Main.m3"
 /* set_label */
#line 64 "../Main.m3"
 /* set_label */
#line 64 "../Main.m3"
L33:;
#line 64 "../Main.m3"
 /* set_source_line */
#line 64 "../Main.m3"
#line 65 "../Main.m3"
 /* start_call_direct */
#line 65 "../Main.m3"
 /* load_address */
#line 65 "../Main.m3"
 /* pop_param */
#line 65 "../Main.m3"
 /* load_nil */
#line 65 "../Main.m3"
 /* pop_param */
#line 65 "../Main.m3"
 /* call_direct */
#line 65 "../Main.m3"
IO__Put(
  ( TEXT )(((ADDRESS)(INT64_(480)+((ADDRESS)(&Main_m_20_L_21)))) ),
  ( Wr__T )(((ADDRESS)(0)) ));
#line 65 "../Main.m3"
 /* start_call_direct */
#line 65 "../Main.m3"
 /* call_direct */
#line 65 "../Main.m3"
Main__NL(
 );
#line 65 "../Main.m3"
 /* set_source_line */
#line 65 "../Main.m3"
#line 66 "../Main.m3"
 /* exit_proc */
#line 66 "../Main.m3"
return;
#line 66 "../Main.m3"
 /* end_procedure */
#line 66 "../Main.m3"
} /* Try3.Try3_Try2 */
#line 66 "../Main.m3"
 /* set_source_line */
#line 66 "../Main.m3"
#line 68 "../Main.m3"
 /* begin_procedure */
#line 68 "../Main.m3"
struct Main__Try3__Try3_Try2_Frame_t {
#line 68 "../Main.m3"
ADDRESS _unused;
#line 68 "../Main.m3"
 /* Var_Type1 */ Main__Try3_Frame_t* _static_link;
#line 68 "../Main.m3"
};
#line 68 "../Main.m3"
void /*TypeText3*/ 
__cdecl
Main__Try3__Try3_Try2(
   /* Param_Type1 */ Main__Try3_Frame_t* _static_link)
{
#line 68 "../Main.m3"
 /* Var_Type2 */ ADDRESS Main_m_54_L_55={0};//always-init
#line 68 "../Main.m3"
 /* Var_Type2 */ ADDRESS Main_m_56_L_57={0};//always-init
#line 68 "../Main.m3"
Main__Try3__Try3_Try2_Frame_t _frame;
#line 68 "../Main.m3"
_frame._static_link=_static_link;
#line 68 "../Main.m3"
_frame._unused=(ADDRESS)&_frame;
#line 68 "../Main.m3"
 /* set_source_line */
#line 68 "../Main.m3"
#line 69 "../Main.m3"
 /* start_call_direct */
#line 69 "../Main.m3"
 /* load_address */
#line 69 "../Main.m3"
 /* pop_param */
#line 69 "../Main.m3"
 /* load_nil */
#line 69 "../Main.m3"
 /* pop_param */
#line 69 "../Main.m3"
 /* call_direct */
#line 69 "../Main.m3"
IO__Put(
  ( TEXT )(((ADDRESS)(INT64_(520)+((ADDRESS)(&Main_m_20_L_21)))) ),
  ( Wr__T )(((ADDRESS)(0)) ));
#line 69 "../Main.m3"
 /* start_call_direct */
#line 69 "../Main.m3"
 /* call_direct */
#line 69 "../Main.m3"
Main__NL(
 );
#line 69 "../Main.m3"
 /* set_source_line */
#line 69 "../Main.m3"
#line 70 "../Main.m3"
 /* load_nil */
#line 70 "../Main.m3"
 /* store */
#line 70 "../Main.m3"
(*(ADDRESS*)(&Main_m_54_L_55))=(ADDRESS)(((ADDRESS)(0)));
#line 70 "../Main.m3"
 /* set_label */
#line 70 "../Main.m3"
 /* start_try */
#line 70 "../Main.m3"
try {
#line 70 "../Main.m3"
 /* load_nil */
#line 70 "../Main.m3"
 /* store */
#line 70 "../Main.m3"
(*(ADDRESS*)(&Main_m_56_L_57))=(ADDRESS)(((ADDRESS)(0)));
#line 70 "../Main.m3"
 /* set_label */
#line 70 "../Main.m3"
 /* start_try */
#line 70 "../Main.m3"
try {
#line 70 "../Main.m3"
 /* start_call_direct */
#line 70 "../Main.m3"
 /* invoke_direct */
#line 70 "../Main.m3"
 /* call_direct */
#line 70 "../Main.m3"
Main__PrintStackHeight(
 );
#line 70 "../Main.m3"
 /* set_label */
#line 70 "../Main.m3"
 /* jump */
#line 70 "../Main.m3"
goto L3C;
#line 70 "../Main.m3"
 /* end_try */
#line 70 "../Main.m3"
} catch (_M3Exc& _m3exc) { _m3_caught = _m3exc.act; goto L3A; }
#line 70 "../Main.m3"
 /* set_label */
#line 70 "../Main.m3"
L3A:;
#line 70 "../Main.m3"
 /* landing_pad */
#line 70 "../Main.m3"
 /* store */
#line 70 "../Main.m3"
(*(ADDRESS*)(&Main_m_56_L_57))=(ADDRESS)(((ADDRESS)(_m3_caught)));
#line 70 "../Main.m3"
 /* set_label */
#line 70 "../Main.m3"
 /* set_label */
#line 70 "../Main.m3"
L3C:;
#line 70 "../Main.m3"
 /* jump */
#line 70 "../Main.m3"
goto L38;
#line 70 "../Main.m3"
 /* end_try */
#line 70 "../Main.m3"
} catch (...) { throw; }
#line 70 "../Main.m3"
 /* set_label */
#line 70 "../Main.m3"
 /* landing_pad */
#line 70 "../Main.m3"
 /* store */
#line 70 "../Main.m3"
(*(ADDRESS*)(&Main_m_54_L_55))=(ADDRESS)(((ADDRESS)(_m3_caught)));
#line 70 "../Main.m3"
 /* set_label */
#line 70 "../Main.m3"
 /* set_label */
#line 70 "../Main.m3"
L38:;
#line 70 "../Main.m3"
 /* set_source_line */
#line 70 "../Main.m3"
#line 71 "../Main.m3"
 /* start_call_direct */
#line 71 "../Main.m3"
 /* load_address */
#line 71 "../Main.m3"
 /* pop_param */
#line 71 "../Main.m3"
 /* load_nil */
#line 71 "../Main.m3"
 /* pop_param */
#line 71 "../Main.m3"
 /* call_direct */
#line 71 "../Main.m3"
IO__Put(
  ( TEXT )(((ADDRESS)(INT64_(560)+((ADDRESS)(&Main_m_20_L_21)))) ),
  ( Wr__T )(((ADDRESS)(0)) ));
#line 71 "../Main.m3"
 /* start_call_direct */
#line 71 "../Main.m3"
 /* call_direct */
#line 71 "../Main.m3"
Main__NL(
 );
#line 71 "../Main.m3"
 /* set_source_line */
#line 71 "../Main.m3"
#line 72 "../Main.m3"
 /* exit_proc */
#line 72 "../Main.m3"
return;
#line 72 "../Main.m3"
 /* end_procedure */
#line 72 "../Main.m3"
} /* Try3.Try3_Try3 */
#line 72 "../Main.m3"
 /* set_source_line */
#line 72 "../Main.m3"
#line 74 "../Main.m3"
 /* begin_procedure */
#line 74 "../Main.m3"
struct Main__Try3__Try3_Try3_Frame_t {
#line 74 "../Main.m3"
ADDRESS _unused;
#line 74 "../Main.m3"
 /* Var_Type1 */ Main__Try3_Frame_t* _static_link;
#line 74 "../Main.m3"
};
#line 74 "../Main.m3"
void /*TypeText3*/ 
__cdecl
Main__Try3__Try3_Try3(
   /* Param_Type1 */ Main__Try3_Frame_t* _static_link)
{
#line 74 "../Main.m3"
 /* Var_Type2 */ ADDRESS Main_m_58_L_59={0};//always-init
#line 74 "../Main.m3"
 /* Var_Type2 */ ADDRESS Main_m_60_L_61={0};//always-init
#line 74 "../Main.m3"
 /* Var_Type2 */ ADDRESS Main_m_62_L_63={0};//always-init
#line 74 "../Main.m3"
Main__Try3__Try3_Try3_Frame_t _frame;
#line 74 "../Main.m3"
_frame._static_link=_static_link;
#line 74 "../Main.m3"
_frame._unused=(ADDRESS)&_frame;
#line 74 "../Main.m3"
 /* set_source_line */
#line 74 "../Main.m3"
#line 75 "../Main.m3"
 /* start_call_direct */
#line 75 "../Main.m3"
 /* load_address */
#line 75 "../Main.m3"
 /* pop_param */
#line 75 "../Main.m3"
 /* load_nil */
#line 75 "../Main.m3"
 /* pop_param */
#line 75 "../Main.m3"
 /* call_direct */
#line 75 "../Main.m3"
IO__Put(
  ( TEXT )(((ADDRESS)(INT64_(600)+((ADDRESS)(&Main_m_20_L_21)))) ),
  ( Wr__T )(((ADDRESS)(0)) ));
#line 75 "../Main.m3"
 /* start_call_direct */
#line 75 "../Main.m3"
 /* call_direct */
#line 75 "../Main.m3"
Main__NL(
 );
#line 75 "../Main.m3"
 /* set_source_line */
#line 75 "../Main.m3"
#line 76 "../Main.m3"
 /* load_nil */
#line 76 "../Main.m3"
 /* store */
#line 76 "../Main.m3"
(*(ADDRESS*)(&Main_m_58_L_59))=(ADDRESS)(((ADDRESS)(0)));
#line 76 "../Main.m3"
 /* set_label */
#line 76 "../Main.m3"
 /* start_try */
#line 76 "../Main.m3"
try {
#line 76 "../Main.m3"
 /* load_nil */
#line 76 "../Main.m3"
 /* store */
#line 76 "../Main.m3"
(*(ADDRESS*)(&Main_m_60_L_61))=(ADDRESS)(((ADDRESS)(0)));
#line 76 "../Main.m3"
 /* set_label */
#line 76 "../Main.m3"
 /* start_try */
#line 76 "../Main.m3"
try {
#line 76 "../Main.m3"
 /* load_nil */
#line 76 "../Main.m3"
 /* store */
#line 76 "../Main.m3"
(*(ADDRESS*)(&Main_m_62_L_63))=(ADDRESS)(((ADDRESS)(0)));
#line 76 "../Main.m3"
 /* set_label */
#line 76 "../Main.m3"
 /* start_try */
#line 76 "../Main.m3"
try {
#line 76 "../Main.m3"
 /* start_call_direct */
#line 76 "../Main.m3"
 /* invoke_direct */
#line 76 "../Main.m3"
 /* call_direct */
#line 76 "../Main.m3"
Main__PrintStackHeight(
 );
#line 76 "../Main.m3"
 /* set_label */
#line 76 "../Main.m3"
 /* jump */
#line 76 "../Main.m3"
goto L49;
#line 76 "../Main.m3"
 /* end_try */
#line 76 "../Main.m3"
} catch (_M3Exc& _m3exc) { _m3_caught = _m3exc.act; goto L47; }
#line 76 "../Main.m3"
 /* set_label */
#line 76 "../Main.m3"
L47:;
#line 76 "../Main.m3"
 /* landing_pad */
#line 76 "../Main.m3"
 /* store */
#line 76 "../Main.m3"
(*(ADDRESS*)(&Main_m_62_L_63))=(ADDRESS)(((ADDRESS)(_m3_caught)));
#line 76 "../Main.m3"
 /* set_label */
#line 76 "../Main.m3"
 /* set_label */
#line 76 "../Main.m3"
L49:;
#line 76 "../Main.m3"
 /* jump */
#line 76 "../Main.m3"
goto L45;
#line 76 "../Main.m3"
 /* end_try */
#line 76 "../Main.m3"
} catch (...) { throw; }
#line 76 "../Main.m3"
 /* set_label */
#line 76 "../Main.m3"
 /* landing_pad */
#line 76 "../Main.m3"
 /* store */
#line 76 "../Main.m3"
(*(ADDRESS*)(&Main_m_60_L_61))=(ADDRESS)(((ADDRESS)(_m3_caught)));
#line 76 "../Main.m3"
 /* set_label */
#line 76 "../Main.m3"
 /* set_label */
#line 76 "../Main.m3"
L45:;
#line 76 "../Main.m3"
 /* jump */
#line 76 "../Main.m3"
goto L41;
#line 76 "../Main.m3"
 /* end_try */
#line 76 "../Main.m3"
} catch (...) { throw; }
#line 76 "../Main.m3"
 /* set_label */
#line 76 "../Main.m3"
 /* landing_pad */
#line 76 "../Main.m3"
 /* store */
#line 76 "../Main.m3"
(*(ADDRESS*)(&Main_m_58_L_59))=(ADDRESS)(((ADDRESS)(_m3_caught)));
#line 76 "../Main.m3"
 /* set_label */
#line 76 "../Main.m3"
 /* set_label */
#line 76 "../Main.m3"
L41:;
#line 76 "../Main.m3"
 /* set_source_line */
#line 76 "../Main.m3"
#line 77 "../Main.m3"
 /* start_call_direct */
#line 77 "../Main.m3"
 /* load_address */
#line 77 "../Main.m3"
 /* pop_param */
#line 77 "../Main.m3"
 /* load_nil */
#line 77 "../Main.m3"
 /* pop_param */
#line 77 "../Main.m3"
 /* call_direct */
#line 77 "../Main.m3"
IO__Put(
  ( TEXT )(((ADDRESS)(INT64_(640)+((ADDRESS)(&Main_m_20_L_21)))) ),
  ( Wr__T )(((ADDRESS)(0)) ));
#line 77 "../Main.m3"
 /* start_call_direct */
#line 77 "../Main.m3"
 /* call_direct */
#line 77 "../Main.m3"
Main__NL(
 );
#line 77 "../Main.m3"
 /* set_source_line */
#line 77 "../Main.m3"
#line 78 "../Main.m3"
 /* exit_proc */
#line 78 "../Main.m3"
return;
#line 78 "../Main.m3"
 /* end_procedure */
#line 78 "../Main.m3"
} /* Main_M3 */
#line 78 "../Main.m3"
 /* module main body Main_M3 */
#line 78 "../Main.m3"
 /* set_source_line */
#line 78 "../Main.m3"
#line 88 "../Main.m3"
 /* begin_procedure */
#line 88 "../Main.m3"
struct Main_M3_Frame_t {
#line 88 "../Main.m3"
ADDRESS _unused;
#line 88 "../Main.m3"
};
#line 88 "../Main.m3"
RT0__ModulePtr
__cdecl
Main_M3(
   /* Param_Type1 */ INTEGER /*TypeText1*/  mode_L_23)
{
#line 88 "../Main.m3"
 /* Var_Type2 */ ADDRESS Main_m_64_L_65={0};//always-init
#line 88 "../Main.m3"
Main_M3_Frame_t _frame;
#line 88 "../Main.m3"
_frame._unused=(ADDRESS)&_frame;
#line 88 "../Main.m3"
 /* load */
#line 88 "../Main.m3"
 /* if_true_or_false */
#line 88 "../Main.m3"
 /* load_host_integer */
#line 88 "../Main.m3"
 /* load_integer */
#line 88 "../Main.m3"
 /* if_compare */
#line 88 "../Main.m3"
if(m3_eq(INT64,
  mode_L_23,
   INT64_(0)))goto L4B;
#line 88 "../Main.m3"
 /* set_source_line */
#line 88 "../Main.m3"
#line 89 "../Main.m3"
 /* start_call_direct */
#line 89 "../Main.m3"
 /* call_direct */
#line 89 "../Main.m3"
 /* store */
#line 89 "../Main.m3"
(*(ADDRESS*)(&Main_m_64_L_65))=(ADDRESS)(((ADDRESS)(Main__GetStack(
 ))));
#line 89 "../Main.m3"
 /* load */
#line 89 "../Main.m3"
 /* store */
#line 89 "../Main.m3"
(*(ADDRESS*)((104)+(char*)(&Main_m_M_Main_L_22)))=(ADDRESS)(((ADDRESS)(Main_m_64_L_65)));
#line 89 "../Main.m3"
 /* set_source_line */
#line 89 "../Main.m3"
#line 90 "../Main.m3"
 /* start_call_direct */
#line 90 "../Main.m3"
 /* call_direct */
#line 90 "../Main.m3"
Main__Try1(
 );
#line 90 "../Main.m3"
 /* start_call_direct */
#line 90 "../Main.m3"
 /* call_direct */
#line 90 "../Main.m3"
Main__NL(
 );
#line 90 "../Main.m3"
 /* set_source_line */
#line 90 "../Main.m3"
#line 91 "../Main.m3"
 /* start_call_direct */
#line 91 "../Main.m3"
 /* call_direct */
#line 91 "../Main.m3"
Main__Try2(
 );
#line 91 "../Main.m3"
 /* start_call_direct */
#line 91 "../Main.m3"
 /* call_direct */
#line 91 "../Main.m3"
Main__NL(
 );
#line 91 "../Main.m3"
 /* set_source_line */
#line 91 "../Main.m3"
#line 92 "../Main.m3"
 /* start_call_direct */
#line 92 "../Main.m3"
 /* call_direct */
#line 92 "../Main.m3"
Main__Try3(
 );
#line 92 "../Main.m3"
 /* start_call_direct */
#line 92 "../Main.m3"
 /* call_direct */
#line 92 "../Main.m3"
Main__NL(
 );
#line 92 "../Main.m3"
 /* set_source_line */
#line 92 "../Main.m3"
#line 93 "../Main.m3"
 /* start_call_direct */
#line 93 "../Main.m3"
 /* call_direct */
#line 93 "../Main.m3"
Main__Try1(
 );
#line 93 "../Main.m3"
 /* start_call_direct */
#line 93 "../Main.m3"
 /* call_direct */
#line 93 "../Main.m3"
Main__NL(
 );
#line 93 "../Main.m3"
 /* set_source_line */
#line 93 "../Main.m3"
#line 94 "../Main.m3"
 /* start_call_direct */
#line 94 "../Main.m3"
 /* call_direct */
#line 94 "../Main.m3"
Main__Try2(
 );
#line 94 "../Main.m3"
 /* start_call_direct */
#line 94 "../Main.m3"
 /* call_direct */
#line 94 "../Main.m3"
Main__NL(
 );
#line 94 "../Main.m3"
 /* set_source_line */
#line 94 "../Main.m3"
#line 95 "../Main.m3"
 /* start_call_direct */
#line 95 "../Main.m3"
 /* call_direct */
#line 95 "../Main.m3"
Main__Try3(
 );
#line 95 "../Main.m3"
 /* start_call_direct */
#line 95 "../Main.m3"
 /* call_direct */
#line 95 "../Main.m3"
Main__NL(
 );
#line 95 "../Main.m3"
 /* set_label */
#line 95 "../Main.m3"
L4B:;
#line 95 "../Main.m3"
 /* load_address */
#line 95 "../Main.m3"
 /* exit_proc */
#line 95 "../Main.m3"
return (RT0__ModulePtr)(&Main_m_M_Main_L_22);
#line 95 "../Main.m3"
 /* end_procedure */
#line 95 "../Main.m3"
} /* global constant type descriptor */
#line 95 "../Main.m3"
 /* global data type descriptor */
#line 95 "../Main.m3"
 /* module global constants */
#line 95 "../Main.m3"
 /* procedure names */
#line 95 "../Main.m3"
 /* procedure table */
#line 95 "../Main.m3"
 /* file name */
#line 95 "../Main.m3"
 /* module global data */
#line 95 "../Main.m3"
 /* load map


 global data allocation for M_Main
     0   104  8  *module info*
   104     8  8  Main.top_of_stack
   112    24  8  import Main
   136    24  8  import IO
   160    24  8  import RTHooks
   184     0  8  *TOTAL*


 global constants for M_Main
     0    40  8  TEXT literal methods
    40    26  8  *TEXT literal*
    72    38  8  *TEXT literal*
   112    26  8  *TEXT literal*
   144    35  8  *TEXT literal*
   184    35  8  *TEXT literal*
   224    30  8  *TEXT literal*
   256    30  8  *TEXT literal*
   288    35  8  *TEXT literal*
   328    35  8  *TEXT literal*
   368    30  8  *TEXT literal*
   400    30  8  *TEXT literal*
   432    35  8  *TEXT literal*
   472    35  8  *TEXT literal*
   512    35  8  *TEXT literal*
   552    35  8  *TEXT literal*
   592    35  8  *TEXT literal*
   632    35  8  *TEXT literal*
   672    30  8  *TEXT literal*
   704    30  8  *TEXT literal*
   736   142  8  *proc names*
   880   216  8  *proc info*
  1096    11  1  *string*
  1112     0  8  *TOTAL*
 */
#line 95 "../Main.m3"
 /* end unit */
#line 95 "../Main.m3"

#ifdef __cplusplus

} /* extern "C" */
#endif
 /* set_runtime_proc */
 /* set_runtime_proc */
 /* set_runtime_proc */
