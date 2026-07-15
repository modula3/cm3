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
 /* declare_formal */
 /* declare_object */
 /* record_forwardDeclare Record_t{ typeid:TFFFFFFFF text:TA563D950_fields hash_text:NIL base_text:NIL state:0} */
/*record_forwardDeclare*/struct TA563D950_fields;typedef struct TA563D950_fields TA563D950_fields;
 /* record_canBeDefined Record_t{ typeid:TFFFFFFFF text:TA563D950_fields hash_text:NIL base_text:NIL state:0} */
 /* record_define Record_t{ typeid:TFFFFFFFF text:TA563D950_fields hash_text:NIL base_text:NIL state:0} */

#ifndef TA563D950_fields
#define TA563D950_fields TA563D950_fields
/*record_define*/struct TA563D950_fields{
UINT8 L_0[8];
};
#endif
typedef TA563D950_fields*TA563D950;
 /* declare_proctype */

#if 0 /* avoid type hash collions */
typedef 
void(__cdecl*T7B78C34F)(void);
#else
typedef void (__cdecl*T7B78C34F)(void);
#endif
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
UINT8 L_1[7];
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
 /* declare_proctype */
 /* declare_formal */
 /* declare_proctype */
 /* declare_formal */
 /* declare_formal */
 /* declare_record */
 /* declare_record */
 /* DeclareTypes_FlushOnce size:10 */

#if 0 /* avoid type hash collions */
typedef 
void(__cdecl*TCA710884)(REFANY);
#else
typedef void (__cdecl*TCA710884)(void);
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
void(__cdecl*TA61F1411)(ADDRESS,INTEGER);
#else
typedef void (__cdecl*TA61F1411)(void);
#endif

#if 0 /* avoid type hash collions */
typedef 
ROOT(__cdecl*T58C1D1D1)(ADDRESS);
#else
typedef void (__cdecl*T58C1D1D1)(void);
#endif

#if 0 /* avoid type hash collions */
typedef 
void(__cdecl*TF14AA6EF)(REFANY,TCA710884);
#else
typedef void (__cdecl*TF14AA6EF)(void);
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
   /* Param_Type1 */ INTEGER /*TypeText1*/  mode_L_2);
 /* import_procedure */
/*Proc_ForwardDeclareFrameType*/struct RTCollector_I3_Frame_t;typedef struct RTCollector_I3_Frame_t RTCollector_I3_Frame_t;
 /* internal_declare_param */
RT0__ModulePtr
__cdecl
RTCollector_I3(
   /* Param_Type1 */ INTEGER /*TypeText1*/  mode_L_3);
 /* import_procedure */
/*Proc_ForwardDeclareFrameType*/struct RTIO_I3_Frame_t;typedef struct RTIO_I3_Frame_t RTIO_I3_Frame_t;
 /* internal_declare_param */
RT0__ModulePtr
__cdecl
RTIO_I3(
   /* Param_Type1 */ INTEGER /*TypeText1*/  mode_L_4);
 /* import_procedure */
/*Proc_ForwardDeclareFrameType*/struct RTHeapRep_I3_Frame_t;typedef struct RTHeapRep_I3_Frame_t RTHeapRep_I3_Frame_t;
 /* internal_declare_param */
RT0__ModulePtr
__cdecl
RTHeapRep_I3(
   /* Param_Type1 */ INTEGER /*TypeText1*/  mode_L_5);
 /* import_procedure */
/*Proc_ForwardDeclareFrameType*/struct RTHooks_I3_Frame_t;typedef struct RTHooks_I3_Frame_t RTHooks_I3_Frame_t;
 /* internal_declare_param */
RT0__ModulePtr
__cdecl
RTHooks_I3(
   /* Param_Type1 */ INTEGER /*TypeText1*/  mode_L_6);
 /* import_procedure */
/*Proc_ForwardDeclareFrameType*/struct RTIO__PutText_Frame_t;typedef struct RTIO__PutText_Frame_t RTIO__PutText_Frame_t;
 /* internal_declare_param */
void /*TypeText3*/ 
__cdecl
RTIO__PutText(
   /* Param_Type1 */ TEXT t_L_7);
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
 /* import_procedure */
/*Proc_ForwardDeclareFrameType*/struct RTIO__PutAddr_Frame_t;typedef struct RTIO__PutAddr_Frame_t RTIO__PutAddr_Frame_t;
 /* internal_declare_param */
 /* internal_declare_param */
void /*TypeText3*/ 
__cdecl
RTIO__PutAddr(
   /* Param_Type1 */ ADDRESS a_L_20,
   /* Param_Type1 */ INTEGER width_L_21);
 /* import_procedure */
/*Proc_ForwardDeclareFrameType*/struct RTIO__Flush_Frame_t;typedef struct RTIO__Flush_Frame_t RTIO__Flush_Frame_t;
void /*TypeText3*/ 
__cdecl
RTIO__Flush(void);
 /* import_procedure */
/*Proc_ForwardDeclareFrameType*/struct RTHooks__AllocateTracedObj_Frame_t;typedef struct RTHooks__AllocateTracedObj_Frame_t RTHooks__AllocateTracedObj_Frame_t;
 /* internal_declare_param */
ROOT
__cdecl
RTHooks__AllocateTracedObj(
   /* Param_Type1 */ ADDRESS t_L_22);
 /* import_procedure */
/*Proc_ForwardDeclareFrameType*/struct RTHeapRep__RegisterFinalCleanup_Frame_t;typedef struct RTHeapRep__RegisterFinalCleanup_Frame_t RTHeapRep__RegisterFinalCleanup_Frame_t;
 /* internal_declare_param */
 /* internal_declare_param */
void /*TypeText3*/ 
__cdecl
RTHeapRep__RegisterFinalCleanup(
   /* Param_Type1 */ REFANY r_L_23,
   /* Param_Type1 */ TCA710884 /*TypeText1*/  p_L_24);
 /* import_procedure */
/*Proc_ForwardDeclareFrameType*/struct RTCollector__Collect_Frame_t;typedef struct RTCollector__Collect_Frame_t RTCollector__Collect_Frame_t;
void /*TypeText3*/ 
__cdecl
RTCollector__Collect(void);
 /* end: imports */
 /* begin: locals */
 /* declare_segment name:<NIL> typeid:TFFFFFFFF const:TRUE */
/*declare_segment*/struct Main_m_25_L_26_t;
/*declare_segment*/typedef struct Main_m_25_L_26_t Main_m_25_L_26_t;
 /* declare_segment name:M_Main typeid:TFFFFFFFF const:FALSE */
 /* handler_name_prefixes:Main_M3_LINE_ */
 /* handler_name_prefixes:Main_I3_LINE_ */
/*declare_segment*/struct Main_m_M_Main_L_27_t;
/*declare_segment*/typedef struct Main_m_M_Main_L_27_t Main_m_M_Main_L_27_t;
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct Main_M3_Frame_t;typedef struct Main_M3_Frame_t Main_M3_Frame_t;
 /* internal_declare_param */
RT0__ModulePtr
__cdecl
Main_M3(
   /* Param_Type1 */ INTEGER /*TypeText1*/  mode_L_28);
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct Main__Clean1_Frame_t;typedef struct Main__Clean1_Frame_t Main__Clean1_Frame_t;
 /* internal_declare_param */
void /*TypeText3*/ 
__cdecl
Main__Clean1(
   /* Param_Type1 */ REFANY r_L_29);
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct Main__Clean2_Frame_t;typedef struct Main__Clean2_Frame_t Main__Clean2_Frame_t;
 /* internal_declare_param */
void /*TypeText3*/ 
__cdecl
Main__Clean2(
   /* Param_Type1 */ REFANY r_L_30);
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct Main__Test_Frame_t;typedef struct Main__Test_Frame_t Main__Test_Frame_t;
void /*TypeText3*/ 
__cdecl
Main__Test(void);
 /* declare_local */
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
 /* init_chars */
 /* init_int */
 /* init_int */
 /* init_int */
 /* init_chars */
 /* end_init */
struct Main_m_25_L_26_t{ADDRESS L_34[5];
INT64 L_35[1];
ADDRESS L_36[1];
INT64 L_37[1];
UINT8 L_38[7];
char L_39[1];
INT64 L_40[1];
ADDRESS L_41[1];
INT64 L_42[1];
UINT8 L_43[1];
char L_44[7];
INT64 L_45[1];
ADDRESS L_46[1];
INT64 L_47[1];
UINT8 L_48[7];
char L_49[1];
INT64 L_50[1];
ADDRESS L_51[1];
INT64 L_52[1];
UINT8 L_53[5];
char L_54[3];
UINT8 L_55[7];
char L_56[1];
UINT8 L_57[4];
char L_58[1];
UINT8 L_59[6];
char L_60[1];
UINT8 L_61[6];
char L_62[6];
ADDRESS L_63[8];
char L_64[8];
UINT8 L_65[10];
char L_66[1];
INT8 L_67[3];
UINT8 L_68[6];
char L_69[12];
};
static  const Main_m_25_L_26_t Main_m_25_L_26={{(ADDRESS)&RTHooks__TextLitInfo,(ADDRESS)&RTHooks__TextLitGetChar,(ADDRESS)&RTHooks__TextLitGetWideChar,(ADDRESS)&RTHooks__TextLitGetChars,(ADDRESS)&RTHooks__TextLitGetWideChars},{INT64_(2)},{(char*)&Main_m_25_L_26},{INT64_(7)},{'C','l','e','a','n','1',':'},{0 /* 1 */ ,},{INT64_(2)},{(char*)&Main_m_25_L_26},{INT64_(1)},{10},{0 /* 1 */ ,0 /* 2 */ ,0 /* 3 */ ,0 /* 4 */ ,0 /* 5 */ ,0 /* 6 */ ,0 /* 7 */ ,},{INT64_(2)},{(char*)&Main_m_25_L_26},{INT64_(7)},{'C','l','e','a','n','2',':'},{0 /* 1 */ ,},{INT64_(2)},{(char*)&Main_m_25_L_26},{INT64_(5)},{'T','e','s','t',':'},{0 /* 1 */ ,0 /* 2 */ ,0 /* 3 */ ,},{'M','a','i','n','_','M','3'},{0 /* 1 */ ,},{'T','e','s','t'},{0 /* 1 */ ,},{'C','l','e','a','n','2'},{0 /* 1 */ ,},{'C','l','e','a','n','1'},{0 /* 1 */ ,0 /* 2 */ ,0 /* 3 */ ,0 /* 4 */ ,0 /* 5 */ ,0 /* 6 */ ,},{(ADDRESS)&Main_M3,168+(char*)&Main_m_25_L_26,(ADDRESS)&Main__Test,176+(char*)&Main_m_25_L_26,(ADDRESS)&Main__Clean2,181+(char*)&Main_m_25_L_26
,(ADDRESS)&Main__Clean1,188+(char*)&Main_m_25_L_26},{0 /* 1 */ ,0 /* 2 */ ,0 /* 3 */ ,0 /* 4 */ ,0 /* 5 */ ,0 /* 6 */ ,0 /* 7 */ ,0 /* 8 */ ,},{'.','.','/','M','a','i','n','.','m','3'},{0 /* 1 */ ,},{((INT8)1),((INT8)12),((INT8)0)},{'M','a','i','n','.','A'},{0 /* 1 */ ,0 /* 2 */ ,0 /* 3 */ ,0 /* 4 */ ,0 /* 5 */ ,0 /* 6 */ ,0 /* 7 */ ,0 /* 8 */ ,0 /* 9 */ ,0 /* 10 */ ,0 /* 11 */ ,0 /* 12 */ ,}};
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
 /* init_int */
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
 /* init_int */
 /* end_init */
struct Main_m_M_Main_L_27_t{ADDRESS L_70[3];
char L_71[16];
ADDRESS L_72[1];
char L_73[24];
ADDRESS L_74[1];
char L_75[8];
ADDRESS L_76[1];
INT64 L_77[1];
char L_78[8];
INT64 L_79[1];
UINT8 L_80[1];
INT8 L_81[2];
UINT8 L_82[3];
INT8 L_83[4];
char L_84[1];
INT8 L_85[1];
char L_86[4];
INT64 L_87[1];
char L_88[16];
ADDRESS L_89[1];
char L_90[16];
ADDRESS L_91[1];
char L_92[8];
INT64 L_93[1];
char L_94[24];
INT64 L_95[1];
char L_96[24];
ADDRESS L_97[2];
char L_98[8];
ADDRESS L_99[2];
char L_100[8];
ADDRESS L_101[2];
char L_102[8];
ADDRESS L_103[2];
char L_104[8];
ADDRESS L_105[1];
char L_106[16];
INT64 L_107[1];
char L_108[8];
};
static Main_m_M_Main_L_27_t Main_m_M_Main_L_27={{272+(char*)&Main_m_25_L_26,104+(char*)&Main_m_M_Main_L_27,376+(char*)&Main_m_M_Main_L_27},{0 /* 1 */ ,0 /* 2 */ ,0 /* 3 */ ,0 /* 4 */ ,0 /* 5 */ ,0 /* 6 */ ,0 /* 7 */ ,0 /* 8 */ ,0 /* 9 */ ,0 /* 10 */ ,0 /* 11 */ ,0 /* 12 */ ,0 /* 13 */ ,0 /* 14 */ ,0 /* 15 */ ,0 /* 16 */ ,},{200+(char*)&Main_m_25_L_26},{0 /* 1 */ ,0 /* 2 */ ,0 /* 3 */ ,0 /* 4 */ ,0 /* 5 */ ,0 /* 6 */ ,0 /* 7 */ ,0 /* 8 */ ,0 /* 9 */ ,0 /* 10 */ ,0 /* 11 */ ,0 /* 12 */ ,0 /* 13 */ ,0 /* 14 */ ,0 /* 15 */ ,0 /* 16 */ ,0 /* 17 */ ,0 /* 18 */ ,0 /* 19 */ ,0 /* 20 */ ,0 /* 21 */ ,0 /* 22 */ ,0 /* 23 */ ,0 /* 24 */ ,},{256+(char*)&Main_m_M_Main_L_27},{0 /* 1 */ ,0 /* 2 */ ,0 /* 3 */ ,0 /* 4 */ ,0 /* 5 */ ,0 /* 6 */ ,0 /* 7 */ ,0 /* 8 */ ,},{(ADDRESS)&Main_M3},{INT64_(3)},{0 /* 1 */ ,0 /* 2 */ ,0 /* 3 */ ,0 /* 4 */ ,0 /* 5 */ ,0 /* 6 */ ,0 /* 7 */ ,0 /* 8 */ ,},{INT64_(-1520182960)},{239U},{((INT8)98),((INT8)7)},{177U,191U,187U},{((INT8)100),((INT8)20),((INT8)1),((INT8)2)},{0 /* 1 */ 
,},{((INT8)8)},{0 /* 1 */ ,0 /* 2 */ ,0 /* 3 */ ,0 /* 4 */ ,},{INT64_(0)},{0 /* 1 */ ,0 /* 2 */ ,0 /* 3 */ ,0 /* 4 */ ,0 /* 5 */ ,0 /* 6 */ ,0 /* 7 */ ,0 /* 8 */ ,0 /* 9 */ ,0 /* 10 */ ,0 /* 11 */ ,0 /* 12 */ ,0 /* 13 */ ,0 /* 14 */ ,0 /* 15 */ ,0 /* 16 */ ,},{283+(char*)&Main_m_25_L_26},{0 /* 1 */ ,0 /* 2 */ ,0 /* 3 */ ,0 /* 4 */ ,0 /* 5 */ ,0 /* 6 */ ,0 /* 7 */ ,0 /* 8 */ ,0 /* 9 */ ,0 /* 10 */ ,0 /* 11 */ ,0 /* 12 */ ,0 /* 13 */ ,0 /* 14 */ ,0 /* 15 */ ,0 /* 16 */ ,},{286+(char*)&Main_m_25_L_26},{0 /* 1 */ ,0 /* 2 */ ,0 /* 3 */ ,0 /* 4 */ ,0 /* 5 */ ,0 /* 6 */ ,0 /* 7 */ ,0 /* 8 */ ,},{INT64_(-1651526519)},{0 /* 1 */ ,0 /* 2 */ ,0 /* 3 */ ,0 /* 4 */ ,0 /* 5 */ ,0 /* 6 */ ,0 /* 7 */ ,0 /* 8 */ ,0 /* 9 */ ,0 /* 10 */ ,0 /* 11 */ ,0 /* 12 */ ,0 /* 13 */ ,0 /* 14 */ ,0 /* 15 */ ,0 /* 16 */ ,0 /* 17 */ ,0 /* 18 */ ,0 /* 19 */ ,0 /* 20 */ ,0 /* 21 */ ,0 /* 22 */ ,0 /* 23 */ ,0 /* 24 */ ,},{INT64_(0)},{0 /* 1 */ ,0 /* 2 */ ,0 /* 3 */ ,0 /* 4 */ ,0 /* 5 */ ,0 /* 6 */ ,0 /* 7 */ ,0 /* 8 */ ,
0 /* 9 */ ,0 /* 10 */ ,0 /* 11 */ ,0 /* 12 */ ,0 /* 13 */ ,0 /* 14 */ ,0 /* 15 */ ,0 /* 16 */ ,0 /* 17 */ ,0 /* 18 */ ,0 /* 19 */ ,0 /* 20 */ ,0 /* 21 */ ,0 /* 22 */ ,0 /* 23 */ ,0 /* 24 */ ,},{(ADDRESS)&Main_I3,280+(char*)&Main_m_M_Main_L_27},{0 /* 1 */ ,0 /* 2 */ ,0 /* 3 */ ,0 /* 4 */ ,0 /* 5 */ ,0 /* 6 */ ,0 /* 7 */ ,0 /* 8 */ ,},{(ADDRESS)&RTCollector_I3,304+(char*)&Main_m_M_Main_L_27},{0 /* 1 */ ,0 /* 2 */ ,0 /* 3 */ ,0 /* 4 */ ,0 /* 5 */ ,0 /* 6 */ ,0 /* 7 */ ,0 /* 8 */ ,},{(ADDRESS)&RTIO_I3,328+(char*)&Main_m_M_Main_L_27},{0 /* 1 */ ,0 /* 2 */ ,0 /* 3 */ ,0 /* 4 */ ,0 /* 5 */ ,0 /* 6 */ ,0 /* 7 */ ,0 /* 8 */ ,},{(ADDRESS)&RTHeapRep_I3,352+(char*)&Main_m_M_Main_L_27},{0 /* 1 */ ,0 /* 2 */ ,0 /* 3 */ ,0 /* 4 */ ,0 /* 5 */ ,0 /* 6 */ ,0 /* 7 */ ,0 /* 8 */ ,},{(ADDRESS)&RTHooks_I3},{0 /* 1 */ ,0 /* 2 */ ,0 /* 3 */ ,0 /* 4 */ ,0 /* 5 */ ,0 /* 6 */ ,0 /* 7 */ ,0 /* 8 */ ,0 /* 9 */ ,0 /* 10 */ ,0 /* 11 */ ,0 /* 12 */ ,0 /* 13 */ ,0 /* 14 */ ,0 /* 15 */ ,0 /* 16 */ ,},{INT64_(-1520182960)
},{0 /* 1 */ ,0 /* 2 */ ,0 /* 3 */ ,0 /* 4 */ ,0 /* 5 */ ,0 /* 6 */ ,0 /* 7 */ ,0 /* 8 */ ,}};
 /* end: segments/globals */
 /* begin: mark used */
 /* end: mark used */
 /* set_source_file */
 /* set_source_line */
#line 4 "../Main.m3"
 /* module global constants */
#line 4 "../Main.m3"
 /* module global data */
#line 4 "../Main.m3"
 /* set_source_line */
#line 4 "../Main.m3"
#line 36 "../Main.m3"
 /* Clean1 */
#line 36 "../Main.m3"
 /* set_source_line */
#line 36 "../Main.m3"
#line 7 "../Main.m3"
 /* begin_procedure */
#line 7 "../Main.m3"
struct Main__Clean1_Frame_t {
#line 7 "../Main.m3"
ADDRESS _unused;
#line 7 "../Main.m3"
};
#line 7 "../Main.m3"
void /*TypeText3*/ 
__cdecl
Main__Clean1(
   /* Param_Type1 */ REFANY r_L_29)
{
#line 7 "../Main.m3"
Main__Clean1_Frame_t _frame;
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
 /* load_address */
#line 9 "../Main.m3"
 /* pop_param */
#line 9 "../Main.m3"
 /* call_direct */
#line 9 "../Main.m3"
RTIO__PutText(
  ( TEXT )(((ADDRESS)(INT64_(48)+((ADDRESS)(&Main_m_25_L_26)))) ));
#line 9 "../Main.m3"
 /* set_source_line */
#line 9 "../Main.m3"
#line 10 "../Main.m3"
 /* start_call_direct */
#line 10 "../Main.m3"
 /* load */
#line 10 "../Main.m3"
 /* pop_param */
#line 10 "../Main.m3"
 /* load_integer */
#line 10 "../Main.m3"
 /* pop_param */
#line 10 "../Main.m3"
 /* call_direct */
#line 10 "../Main.m3"
RTIO__PutAddr(
  ( ADDRESS )(((ADDRESS)(r_L_29)) ),
  ( INTEGER )(  INT64_(0) ));
#line 10 "../Main.m3"
 /* set_source_line */
#line 10 "../Main.m3"
#line 11 "../Main.m3"
 /* start_call_direct */
#line 11 "../Main.m3"
 /* load_address */
#line 11 "../Main.m3"
 /* pop_param */
#line 11 "../Main.m3"
 /* call_direct */
#line 11 "../Main.m3"
RTIO__PutText(
  ( TEXT )(((ADDRESS)(INT64_(80)+((ADDRESS)(&Main_m_25_L_26)))) ));
#line 11 "../Main.m3"
 /* set_source_line */
#line 11 "../Main.m3"
#line 12 "../Main.m3"
 /* start_call_direct */
#line 12 "../Main.m3"
 /* call_direct */
#line 12 "../Main.m3"
RTIO__Flush(
 );
#line 12 "../Main.m3"
 /* set_source_line */
#line 12 "../Main.m3"
#line 13 "../Main.m3"
 /* exit_proc */
#line 13 "../Main.m3"
return;
#line 13 "../Main.m3"
 /* end_procedure */
#line 13 "../Main.m3"
} /* Clean2 */
#line 13 "../Main.m3"
 /* set_source_line */
#line 13 "../Main.m3"
#line 15 "../Main.m3"
 /* begin_procedure */
#line 15 "../Main.m3"
struct Main__Clean2_Frame_t {
#line 15 "../Main.m3"
ADDRESS _unused;
#line 15 "../Main.m3"
};
#line 15 "../Main.m3"
void /*TypeText3*/ 
__cdecl
Main__Clean2(
   /* Param_Type1 */ REFANY r_L_30)
{
#line 15 "../Main.m3"
Main__Clean2_Frame_t _frame;
#line 15 "../Main.m3"
_frame._unused=(ADDRESS)&_frame;
#line 15 "../Main.m3"
 /* set_source_line */
#line 15 "../Main.m3"
#line 16 "../Main.m3"
 /* set_source_line */
#line 16 "../Main.m3"
#line 17 "../Main.m3"
 /* start_call_direct */
#line 17 "../Main.m3"
 /* load_address */
#line 17 "../Main.m3"
 /* pop_param */
#line 17 "../Main.m3"
 /* call_direct */
#line 17 "../Main.m3"
RTIO__PutText(
  ( TEXT )(((ADDRESS)(INT64_(112)+((ADDRESS)(&Main_m_25_L_26)))) ));
#line 17 "../Main.m3"
 /* set_source_line */
#line 17 "../Main.m3"
#line 18 "../Main.m3"
 /* start_call_direct */
#line 18 "../Main.m3"
 /* load */
#line 18 "../Main.m3"
 /* pop_param */
#line 18 "../Main.m3"
 /* load_integer */
#line 18 "../Main.m3"
 /* pop_param */
#line 18 "../Main.m3"
 /* call_direct */
#line 18 "../Main.m3"
RTIO__PutAddr(
  ( ADDRESS )(((ADDRESS)(r_L_30)) ),
  ( INTEGER )(  INT64_(0) ));
#line 18 "../Main.m3"
 /* set_source_line */
#line 18 "../Main.m3"
#line 19 "../Main.m3"
 /* start_call_direct */
#line 19 "../Main.m3"
 /* load_address */
#line 19 "../Main.m3"
 /* pop_param */
#line 19 "../Main.m3"
 /* call_direct */
#line 19 "../Main.m3"
RTIO__PutText(
  ( TEXT )(((ADDRESS)(INT64_(80)+((ADDRESS)(&Main_m_25_L_26)))) ));
#line 19 "../Main.m3"
 /* set_source_line */
#line 19 "../Main.m3"
#line 20 "../Main.m3"
 /* start_call_direct */
#line 20 "../Main.m3"
 /* call_direct */
#line 20 "../Main.m3"
RTIO__Flush(
 );
#line 20 "../Main.m3"
 /* set_source_line */
#line 20 "../Main.m3"
#line 21 "../Main.m3"
 /* exit_proc */
#line 21 "../Main.m3"
return;
#line 21 "../Main.m3"
 /* end_procedure */
#line 21 "../Main.m3"
} /* Test */
#line 21 "../Main.m3"
 /* set_source_line */
#line 21 "../Main.m3"
#line 25 "../Main.m3"
 /* begin_procedure */
#line 25 "../Main.m3"
struct Main__Test_Frame_t {
#line 25 "../Main.m3"
ADDRESS _unused;
#line 25 "../Main.m3"
};
#line 25 "../Main.m3"
void /*TypeText3*/ 
__cdecl
Main__Test(void)
{
#line 25 "../Main.m3"
 /* Var_Type1 */ TA563D950_fields* a_L_31={0};//always-init
#line 25 "../Main.m3"
 /* Var_Type2 */ ADDRESS Main_m_32_L_33={0};//always-init
#line 25 "../Main.m3"
Main__Test_Frame_t _frame;
#line 25 "../Main.m3"
_frame._unused=(ADDRESS)&_frame;
#line 25 "../Main.m3"
 /* set_source_line */
#line 25 "../Main.m3"
#line 26 "../Main.m3"
 /* start_call_direct */
#line 26 "../Main.m3"
 /* load */
#line 26 "../Main.m3"
 /* pop_param */
#line 26 "../Main.m3"
 /* call_direct */
#line 26 "../Main.m3"
 /* store */
#line 26 "../Main.m3"
(*(ADDRESS*)(&Main_m_32_L_33))=(ADDRESS)(((ADDRESS)(RTHooks__AllocateTracedObj(
  ( ADDRESS )(((ADDRESS)(*((ADDRESS*)(INT64_(376)+((ADDRESS)(&Main_m_M_Main_L_27)))))) )))));
#line 26 "../Main.m3"
 /* load */
#line 26 "../Main.m3"
 /* store */
#line 26 "../Main.m3"
(*(ADDRESS*)(&a_L_31))=(ADDRESS)(((ADDRESS)(Main_m_32_L_33)));
#line 26 "../Main.m3"
 /* set_source_line */
#line 26 "../Main.m3"
#line 28 "../Main.m3"
 /* start_call_direct */
#line 28 "../Main.m3"
 /* load_address */
#line 28 "../Main.m3"
 /* pop_param */
#line 28 "../Main.m3"
 /* call_direct */
#line 28 "../Main.m3"
RTIO__PutText(
  ( TEXT )(((ADDRESS)(INT64_(144)+((ADDRESS)(&Main_m_25_L_26)))) ));
#line 28 "../Main.m3"
 /* set_source_line */
#line 28 "../Main.m3"
#line 29 "../Main.m3"
 /* start_call_direct */
#line 29 "../Main.m3"
 /* load */
#line 29 "../Main.m3"
 /* pop_param */
#line 29 "../Main.m3"
 /* load_integer */
#line 29 "../Main.m3"
 /* pop_param */
#line 29 "../Main.m3"
 /* call_direct */
#line 29 "../Main.m3"
RTIO__PutAddr(
  ( ADDRESS )(((ADDRESS)(a_L_31)) ),
  ( INTEGER )(  INT64_(0) ));
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
 /* call_direct */
#line 30 "../Main.m3"
RTIO__PutText(
  ( TEXT )(((ADDRESS)(INT64_(80)+((ADDRESS)(&Main_m_25_L_26)))) ));
#line 30 "../Main.m3"
 /* set_source_line */
#line 30 "../Main.m3"
#line 31 "../Main.m3"
 /* start_call_direct */
#line 31 "../Main.m3"
 /* call_direct */
#line 31 "../Main.m3"
RTIO__Flush(
 );
#line 31 "../Main.m3"
 /* set_source_line */
#line 31 "../Main.m3"
#line 32 "../Main.m3"
 /* start_call_direct */
#line 32 "../Main.m3"
 /* load */
#line 32 "../Main.m3"
 /* pop_param */
#line 32 "../Main.m3"
 /* load_procedure */
#line 32 "../Main.m3"
 /* pop_param */
#line 32 "../Main.m3"
 /* call_direct */
#line 32 "../Main.m3"
RTHeapRep__RegisterFinalCleanup(
  ( REFANY )(((ADDRESS)(a_L_31)) ),
  ( TCA710884 /*TypeText1*/  )(((ADDRESS)(Main__Clean1)) ));
#line 32 "../Main.m3"
 /* set_source_line */
#line 32 "../Main.m3"
#line 33 "../Main.m3"
 /* start_call_direct */
#line 33 "../Main.m3"
 /* load */
#line 33 "../Main.m3"
 /* pop_param */
#line 33 "../Main.m3"
 /* load_procedure */
#line 33 "../Main.m3"
 /* pop_param */
#line 33 "../Main.m3"
 /* call_direct */
#line 33 "../Main.m3"
RTHeapRep__RegisterFinalCleanup(
  ( REFANY )(((ADDRESS)(a_L_31)) ),
  ( TCA710884 /*TypeText1*/  )(((ADDRESS)(Main__Clean2)) ));
#line 33 "../Main.m3"
 /* set_source_line */
#line 33 "../Main.m3"
#line 34 "../Main.m3"
 /* exit_proc */
#line 34 "../Main.m3"
return;
#line 34 "../Main.m3"
 /* end_procedure */
#line 34 "../Main.m3"
} /* Main_M3 */
#line 34 "../Main.m3"
 /* module main body Main_M3 */
#line 34 "../Main.m3"
 /* set_source_line */
#line 34 "../Main.m3"
#line 36 "../Main.m3"
 /* begin_procedure */
#line 36 "../Main.m3"
struct Main_M3_Frame_t {
#line 36 "../Main.m3"
ADDRESS _unused;
#line 36 "../Main.m3"
};
#line 36 "../Main.m3"
RT0__ModulePtr
__cdecl
Main_M3(
   /* Param_Type1 */ INTEGER /*TypeText1*/  mode_L_28)
{
#line 36 "../Main.m3"
Main_M3_Frame_t _frame;
#line 36 "../Main.m3"
_frame._unused=(ADDRESS)&_frame;
#line 36 "../Main.m3"
 /* load */
#line 36 "../Main.m3"
 /* if_true_or_false */
#line 36 "../Main.m3"
 /* load_host_integer */
#line 36 "../Main.m3"
 /* load_integer */
#line 36 "../Main.m3"
 /* if_compare */
#line 36 "../Main.m3"
if(m3_eq(INT64,
  mode_L_28,
   INT64_(0)))goto L1;
#line 36 "../Main.m3"
 /* set_source_line */
#line 36 "../Main.m3"
#line 37 "../Main.m3"
 /* jump */
#line 37 "../Main.m3"
goto L3;
#line 37 "../Main.m3"
 /* set_label */
#line 37 "../Main.m3"
L2:;
#line 37 "../Main.m3"
 /* set_source_line */
#line 37 "../Main.m3"
#line 38 "../Main.m3"
 /* start_call_direct */
#line 38 "../Main.m3"
 /* call_direct */
#line 38 "../Main.m3"
Main__Test(
 );
#line 38 "../Main.m3"
 /* set_source_line */
#line 38 "../Main.m3"
#line 39 "../Main.m3"
 /* start_call_direct */
#line 39 "../Main.m3"
 /* call_direct */
#line 39 "../Main.m3"
RTCollector__Collect(
 );
#line 39 "../Main.m3"
 /* set_source_line */
#line 39 "../Main.m3"
#line 37 "../Main.m3"
 /* set_label */
#line 37 "../Main.m3"
L3:;
#line 37 "../Main.m3"
 /* load_integer */
#line 37 "../Main.m3"
 /* if_true_or_false */
#line 37 "../Main.m3"
 /* load_host_integer */
#line 37 "../Main.m3"
 /* load_integer */
#line 37 "../Main.m3"
 /* if_compare */
#line 37 "../Main.m3"
if(m3_ne(INT64,
   INT64_(1),
   INT64_(0)))goto L2;
#line 37 "../Main.m3"
 /* set_label */
#line 37 "../Main.m3"
 /* set_label */
#line 37 "../Main.m3"
L1:;
#line 37 "../Main.m3"
 /* load_address */
#line 37 "../Main.m3"
 /* exit_proc */
#line 37 "../Main.m3"
return (RT0__ModulePtr)(&Main_m_M_Main_L_27);
#line 37 "../Main.m3"
 /* end_procedure */
#line 37 "../Main.m3"
} /* global constant type descriptor */
#line 37 "../Main.m3"
 /* global data type descriptor */
#line 37 "../Main.m3"
 /* module global constants */
#line 37 "../Main.m3"
 /* procedure names */
#line 37 "../Main.m3"
 /* procedure table */
#line 37 "../Main.m3"
 /* file name */
#line 37 "../Main.m3"
 /* type description for _ta563d950 */
#line 37 "../Main.m3"
 /* module global data */
#line 37 "../Main.m3"
 /* typecell for _ta563d950 */
#line 37 "../Main.m3"
 /* load map


 global data allocation for M_Main
     0   104  8  *module info*
   104   152  8  typecell
   256    24  8  import Main
   280    24  8  import RTCollector
   304    24  8  import RTIO
   328    24  8  import RTHeapRep
   352    24  8  import RTHooks
   376    16  8  typecell ptr
   392     0  8  *TOTAL*


 global constants for M_Main
     0    40  8  TEXT literal methods
    40    32  8  *TEXT literal*
    72    26  8  *TEXT literal*
   104    32  8  *TEXT literal*
   136    30  8  *TEXT literal*
   168    27  8  *proc names*
   200    72  8  *proc info*
   272    11  1  *string*
   283     3  1  type_desc
   286     7  1  *string*
   296     0  8  *TOTAL*
 */
#line 37 "../Main.m3"
 /* end unit */
#line 37 "../Main.m3"

#ifdef __cplusplus

} /* extern "C" */
#endif
 /* set_runtime_proc */
 /* set_runtime_proc */
 /* set_runtime_proc */
