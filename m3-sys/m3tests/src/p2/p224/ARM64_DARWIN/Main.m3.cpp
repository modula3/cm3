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
REFANY(__cdecl*TE04DE02E)(void);
#else
typedef void (__cdecl*TE04DE02E)(void);
#endif
 /* declare_object */
 /* record_forwardDeclare Record_t{ typeid:TFFFFFFFF text:TE545939D_fields hash_text:NIL base_text:NIL state:0} */
/*record_forwardDeclare*/struct TE545939D_fields;typedef struct TE545939D_fields TE545939D_fields;
 /* record_canBeDefined Record_t{ typeid:TFFFFFFFF text:TE545939D_fields hash_text:NIL base_text:NIL state:0} */
 /* record_define Record_t{ typeid:TFFFFFFFF text:TE545939D_fields hash_text:NIL base_text:NIL state:0} */

#ifndef TE545939D_fields
#define TE545939D_fields TE545939D_fields
/*record_define*/struct TE545939D_fields{
UINT8 L_0[8];
};
#endif
typedef TE545939D_fields*TE545939D;
 /* declare_method */
 /* declare_object */
 /* declare_field */
 /* record_forwardDeclare Record_t{ typeid:TFFFFFFFF text:T967E734F_fields hash_text:NIL base_text:NIL state:0} */
/*record_forwardDeclare*/struct T967E734F_fields;typedef struct T967E734F_fields T967E734F_fields;
 /* record_canBeDefined Record_t{ typeid:TFFFFFFFF text:T967E734F_fields hash_text:NIL base_text:NIL state:0} */
 /* record_define Record_t{ typeid:TFFFFFFFF text:T967E734F_fields hash_text:NIL base_text:NIL state:0} */

#ifndef T967E734F_fields
#define T967E734F_fields T967E734F_fields
/*record_define*/struct T967E734F_fields{
WORD_T id;
};
#endif
 /* declare_open_array */
/*array_forwardDeclare*/struct T46D79418;typedef struct T46D79418 T46D79418;

#ifndef T46D79418
#define T46D79418 T46D79418
/*openArray_define*/struct T46D79418{
WORD_T*_elts;
CARDINAL _size;
};

#endif
 /* declare_pointer */
typedef T46D79418*TE397683D;
 /* declare_proctype */
 /* declare_formal */
 /* declare_proctype */
 /* declare_formal */
 /* declare_proctype */

#if 0 /* avoid type hash collions */
typedef 
void(__cdecl*T7B78C34F)(void);
#else
typedef void (__cdecl*T7B78C34F)(void);
#endif
 /* declare_opaque */

#ifndef T4B16B0ED
#define T4B16B0ED T4B16B0ED
/*1addressType_define*/typedef ADDRESS T4B16B0ED;

#endif
 /* declare_open_array */
/*array_forwardDeclare*/struct T16894539;typedef struct T16894539 T16894539;

#ifndef T16894539
#define T16894539 T16894539
/*openArray_define*/struct T16894539{
T4B16B0ED*_elts;
CARDINAL _size;
};

#endif
 /* declare_pointer */
typedef T16894539*TD13DAD59;
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
 /* declare_proctype */
 /* declare_formal */
 /* declare_formal */
 /* declare_proctype */
 /* declare_formal */

#ifndef Thread__Mutex
#define Thread__Mutex Thread__Mutex
typedef MUTEX Thread__Mutex;
#endif
 /* declare_proctype */
 /* declare_formal */
 /* declare_proctype */
 /* declare_formal */
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

#ifndef RTHooks__ArrayShape
#define RTHooks__ArrayShape RTHooks__ArrayShape
typedef TF400F3DB RTHooks__ArrayShape;
#endif
typedef RTHooks__ArrayShape*TBFF0C24;
 /* declare_proctype */
 /* declare_formal */
 /* declare_formal */
 /* declare_indirect */
typedef CARDINAL*T681DC81D;
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
 /* declare_proctype */
 /* declare_formal */

#ifndef Thread__Closure
#define Thread__Closure Thread__Closure
typedef TE545939D Thread__Closure;
#endif
 /* declare_proctype */
 /* declare_formal */

#ifndef Thread__T
#define Thread__T Thread__T
typedef T4B16B0ED Thread__T;
#endif
 /* declare_subrange */
/*subrange_define*/typedef UINT32 T51DDFE9B_32;
 /* declare_proctype */
 /* declare_formal */

#ifndef Process__ExitCode
#define Process__ExitCode Process__ExitCode
typedef T51DDFE9B_32 Process__ExitCode;
#endif
 /* declare_record */
 /* declare_record */
 /* declare_field */
 /* declare_field */
 /* declare_field */
 /* declare_field */
 /* declare_field */
 /* declare_field */
 /* DeclareTypes_FlushOnce size:22 */
typedef T967E734F_fields*T967E734F;

#if 0 /* avoid type hash collions */
typedef 
void(__cdecl*TC5673195)(TEXT);
#else
typedef void (__cdecl*TC5673195)(void);
#endif

#ifndef Main__ChildClosure
#define Main__ChildClosure Main__ChildClosure
typedef T967E734F Main__ChildClosure;
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
void(__cdecl*T43604B73)(TEXT,Wr__T);
#else
typedef void (__cdecl*T43604B73)(void);
#endif

#if 0 /* avoid type hash collions */
typedef 
void(__cdecl*T37E50548)(REFANY);
#else
typedef void (__cdecl*T37E50548)(void);
#endif

#if 0 /* avoid type hash collions */
typedef 
void(__cdecl*TA4BB9882)(ADDRESS,INTEGER);
#else
typedef void (__cdecl*TA4BB9882)(void);
#endif

#if 0 /* avoid type hash collions */
typedef 
void(__cdecl*T16DE9EFF)(Thread__Mutex);
#else
typedef void (__cdecl*T16DE9EFF)(void);
#endif

#if 0 /* avoid type hash collions */
typedef 
void(__cdecl*TD2DE6CCC)(LONGREAL);
#else
typedef void (__cdecl*TD2DE6CCC)(void);
#endif

#if 0 /* avoid type hash collions */
typedef 
void(__cdecl*T477C5063)(ADDRESS);
#else
typedef void (__cdecl*T477C5063)(void);
#endif

#if 0 /* avoid type hash collions */
typedef 
REFANY(__cdecl*T983B02E7)(ADDRESS,RTHooks__ArrayShape*);
#else
typedef void (__cdecl*T983B02E7)(void);
#endif

#if 0 /* avoid type hash collions */
typedef 
TEXT(__cdecl*TF2A35A9D)(INTEGER,Fmt__Base);
#else
typedef void (__cdecl*TF2A35A9D)(void);
#endif

#if 0 /* avoid type hash collions */
typedef 
ROOT(__cdecl*T58C1D1D1)(ADDRESS);
#else
typedef void (__cdecl*T58C1D1D1)(void);
#endif

#if 0 /* avoid type hash collions */
typedef 
T4B16B0ED(__cdecl*T6A72B7EF)(Thread__Closure);
#else
typedef void (__cdecl*T6A72B7EF)(void);
#endif

#if 0 /* avoid type hash collions */
typedef 
REFANY(__cdecl*T459E73F7)(Thread__T);
#else
typedef void (__cdecl*T459E73F7)(void);
#endif

#if 0 /* avoid type hash collions */
typedef 
void(__cdecl*T69C9F5BB)(Process__ExitCode);
#else
typedef void (__cdecl*T69C9F5BB)(void);
#endif
 /* DeclareTypes_FlushOnce size:1 */

#if 0 /* avoid type hash collions */
typedef 
REFANY(__cdecl*T9857102C)(Main__ChildClosure);
#else
typedef void (__cdecl*T9857102C)(void);
#endif
 /* DeclareTypes_FlushOnce size:0 */
 /* end: DeclareTypes */
 /* begin: helper functions */

#if __GNUC__ > 2 || __GNUC__ == 2 && __GNUC_MINOR__ >= 5
#define M3_ATTRIBUTE_NO_RETURN __attribute__((__noreturn__))
#else
#define M3_ATTRIBUTE_NO_RETURN
#endif
#ifndef m3_round
#define m3_round m3_round
double __cdecl round(double);
static INT64 __stdcall m3_round(EXTENDED f) {
 return (INT64)round(f); }
#endif

#define m3_extract(T, value, offset, count) ((((T)(value))>>((WORD_T)(offset)))&~(((~(T)0))<<((WORD_T)(count))))
 /* end: helper functions */

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
   /* Param_Type1 */ INTEGER /*TypeText1*/  mode_L_2);
 /* import_procedure */
/*Proc_ForwardDeclareFrameType*/struct Thread_I3_Frame_t;typedef struct Thread_I3_Frame_t Thread_I3_Frame_t;
 /* internal_declare_param */
RT0__ModulePtr
__cdecl
Thread_I3(
   /* Param_Type1 */ INTEGER /*TypeText1*/  mode_L_3);
 /* import_procedure */
/*Proc_ForwardDeclareFrameType*/struct Process_I3_Frame_t;typedef struct Process_I3_Frame_t Process_I3_Frame_t;
 /* internal_declare_param */
RT0__ModulePtr
__cdecl
Process_I3(
   /* Param_Type1 */ INTEGER /*TypeText1*/  mode_L_4);
 /* import_procedure */
/*Proc_ForwardDeclareFrameType*/struct IO_I3_Frame_t;typedef struct IO_I3_Frame_t IO_I3_Frame_t;
 /* internal_declare_param */
RT0__ModulePtr
__cdecl
IO_I3(
   /* Param_Type1 */ INTEGER /*TypeText1*/  mode_L_5);
 /* import_procedure */
/*Proc_ForwardDeclareFrameType*/struct Fmt_I3_Frame_t;typedef struct Fmt_I3_Frame_t Fmt_I3_Frame_t;
 /* internal_declare_param */
RT0__ModulePtr
__cdecl
Fmt_I3(
   /* Param_Type1 */ INTEGER /*TypeText1*/  mode_L_6);
 /* import_procedure */
/*Proc_ForwardDeclareFrameType*/struct RTHooks_I3_Frame_t;typedef struct RTHooks_I3_Frame_t RTHooks_I3_Frame_t;
 /* internal_declare_param */
RT0__ModulePtr
__cdecl
RTHooks_I3(
   /* Param_Type1 */ INTEGER /*TypeText1*/  mode_L_7);
 /* import_procedure */
/*Proc_ForwardDeclareFrameType*/struct RTHooks__Concat_Frame_t;typedef struct RTHooks__Concat_Frame_t RTHooks__Concat_Frame_t;
 /* internal_declare_param */
 /* internal_declare_param */
TEXT
__cdecl
RTHooks__Concat(
   /* Param_Type1 */ TEXT a_L_8,
   /* Param_Type1 */ TEXT b_L_9);
 /* import_procedure */
/*Proc_ForwardDeclareFrameType*/struct RTHooks__TextLitInfo_Frame_t;typedef struct RTHooks__TextLitInfo_Frame_t RTHooks__TextLitInfo_Frame_t;
 /* internal_declare_param */
 /* internal_declare_param */
void /*TypeText3*/ 
__cdecl
RTHooks__TextLitInfo(
   /* Param_Type1 */ RTHooks__TextLiteral t_L_10,
   /* Param_Type1 */ RTHooks__TextInfo* /*TypeText1*/  i_L_11);
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
   /* Param_Type1 */ RTHooks__TextLiteral t_L_12,
   /* Param_Type1 */ CARDINAL i_L_13);
 /* import_procedure */
/*Proc_ForwardDeclareFrameType*/struct RTHooks__TextLitGetWideChar_Frame_t;typedef struct RTHooks__TextLitGetWideChar_Frame_t RTHooks__TextLitGetWideChar_Frame_t;
 /* internal_declare_param */
 /* internal_declare_param */
WIDECHAR
__cdecl
RTHooks__TextLitGetWideChar(
   /* Param_Type1 */ RTHooks__TextLiteral t_L_14,
   /* Param_Type1 */ CARDINAL i_L_15);
 /* import_procedure */
/*Proc_ForwardDeclareFrameType*/struct RTHooks__TextLitGetChars_Frame_t;typedef struct RTHooks__TextLitGetChars_Frame_t RTHooks__TextLitGetChars_Frame_t;
 /* internal_declare_param */
 /* internal_declare_param */
 /* internal_declare_param */
void /*TypeText3*/ 
__cdecl
RTHooks__TextLitGetChars(
   /* Param_Type1 */ RTHooks__TextLiteral t_L_16,
   /* Param_Type1 */ T89CD34BD* /*TypeText1*/  a_L_17,
   /* Param_Type1 */ CARDINAL start_L_18);
 /* import_procedure */
/*Proc_ForwardDeclareFrameType*/struct RTHooks__TextLitGetWideChars_Frame_t;typedef struct RTHooks__TextLitGetWideChars_Frame_t RTHooks__TextLitGetWideChars_Frame_t;
 /* internal_declare_param */
 /* internal_declare_param */
 /* internal_declare_param */
void /*TypeText3*/ 
__cdecl
RTHooks__TextLitGetWideChars(
   /* Param_Type1 */ RTHooks__TextLiteral t_L_19,
   /* Param_Type1 */ TA19BDC21* /*TypeText1*/  a_L_20,
   /* Param_Type1 */ CARDINAL start_L_21);
 /* import_procedure */
/*Proc_ForwardDeclareFrameType*/struct IO__Put_Frame_t;typedef struct IO__Put_Frame_t IO__Put_Frame_t;
 /* internal_declare_param */
 /* internal_declare_param */
void /*TypeText3*/ 
__cdecl
IO__Put(
   /* Param_Type1 */ TEXT txt_L_22,
   /* Param_Type1 */ Wr__T wr_L_23);
 /* import_procedure */
/*Proc_ForwardDeclareFrameType*/struct RTHooks__CheckLoadTracedRef_Frame_t;typedef struct RTHooks__CheckLoadTracedRef_Frame_t RTHooks__CheckLoadTracedRef_Frame_t;
 /* internal_declare_param */
void /*TypeText3*/ 
__cdecl
RTHooks__CheckLoadTracedRef(
   /* Param_Type1 */ REFANY ref_L_24);
 /* import_procedure */
/*Proc_ForwardDeclareFrameType*/struct RTHooks__ReportFault_Frame_t;typedef struct RTHooks__ReportFault_Frame_t RTHooks__ReportFault_Frame_t;
 /* internal_declare_param */
 /* internal_declare_param */
void /*TypeText3*/ 
__cdecl
RTHooks__ReportFault(
   /* Param_Type1 */ ADDRESS module_L_25,
   /* Param_Type1 */ INTEGER info_L_26) M3_ATTRIBUTE_NO_RETURN;
 /* import_procedure */
/*Proc_ForwardDeclareFrameType*/struct Thread__Release_Frame_t;typedef struct Thread__Release_Frame_t Thread__Release_Frame_t;
 /* internal_declare_param */
void /*TypeText3*/ 
__cdecl
Thread__Release(
   /* Param_Type1 */ Thread__Mutex m_L_27);
 /* import_procedure */
/*Proc_ForwardDeclareFrameType*/struct Thread__Pause_Frame_t;typedef struct Thread__Pause_Frame_t Thread__Pause_Frame_t;
 /* internal_declare_param */
void /*TypeText3*/ 
__cdecl
Thread__Pause(
   /* Param_Type1 */ LONGREAL n_L_28);
 /* import_procedure */
/*Proc_ForwardDeclareFrameType*/struct Thread__Acquire_Frame_t;typedef struct Thread__Acquire_Frame_t Thread__Acquire_Frame_t;
 /* internal_declare_param */
void /*TypeText3*/ 
__cdecl
Thread__Acquire(
   /* Param_Type1 */ Thread__Mutex m_L_29);
 /* import_procedure */
/*Proc_ForwardDeclareFrameType*/struct RTHooks__ResumeRaise_Frame_t;typedef struct RTHooks__ResumeRaise_Frame_t RTHooks__ResumeRaise_Frame_t;
 /* internal_declare_param */
void /*TypeText3*/ 
__cdecl
RTHooks__ResumeRaise(
   /* Param_Type1 */ ADDRESS a_L_30);
 /* import_procedure */
/*Proc_ForwardDeclareFrameType*/struct RTHooks__AllocateOpenArray_Frame_t;typedef struct RTHooks__AllocateOpenArray_Frame_t RTHooks__AllocateOpenArray_Frame_t;
 /* internal_declare_param */
 /* internal_declare_param */
REFANY
__cdecl
RTHooks__AllocateOpenArray(
   /* Param_Type1 */ ADDRESS t_L_31,
   /* Param_Type1 */ RTHooks__ArrayShape* /*TypeText1*/  sizes_L_32);
 /* import_procedure */
/*Proc_ForwardDeclareFrameType*/struct Fmt__Int_Frame_t;typedef struct Fmt__Int_Frame_t Fmt__Int_Frame_t;
 /* internal_declare_param */
 /* internal_declare_param */
TEXT
__cdecl
Fmt__Int(
   /* Param_Type1 */ INTEGER n_L_33,
   /* Param_Type1 */ Fmt__Base base_L_34);
 /* import_procedure */
/*Proc_ForwardDeclareFrameType*/struct RTHooks__AllocateTracedObj_Frame_t;typedef struct RTHooks__AllocateTracedObj_Frame_t RTHooks__AllocateTracedObj_Frame_t;
 /* internal_declare_param */
ROOT
__cdecl
RTHooks__AllocateTracedObj(
   /* Param_Type1 */ ADDRESS t_L_35);
 /* import_procedure */
/*Proc_ForwardDeclareFrameType*/struct RTHooks__CheckStoreTraced_Frame_t;typedef struct RTHooks__CheckStoreTraced_Frame_t RTHooks__CheckStoreTraced_Frame_t;
 /* internal_declare_param */
void /*TypeText3*/ 
__cdecl
RTHooks__CheckStoreTraced(
   /* Param_Type1 */ REFANY ref_L_36);
 /* import_procedure */
/*Proc_ForwardDeclareFrameType*/struct Thread__Fork_Frame_t;typedef struct Thread__Fork_Frame_t Thread__Fork_Frame_t;
 /* internal_declare_param */
Thread__T
__cdecl
Thread__Fork(
   /* Param_Type1 */ Thread__Closure cl_L_37);
 /* import_procedure */
/*Proc_ForwardDeclareFrameType*/struct Thread__Join_Frame_t;typedef struct Thread__Join_Frame_t Thread__Join_Frame_t;
 /* internal_declare_param */
REFANY
__cdecl
Thread__Join(
   /* Param_Type1 */ Thread__T t_L_38);
 /* import_procedure */
/*Proc_ForwardDeclareFrameType*/struct Process__Exit_Frame_t;typedef struct Process__Exit_Frame_t Process__Exit_Frame_t;
 /* internal_declare_param */
void /*TypeText3*/ 
__cdecl
Process__Exit(
   /* Param_Type1 */ Process__ExitCode n_L_39);
 /* end: imports */
 /* begin: locals */
 /* declare_segment name:<NIL> typeid:TFFFFFFFF const:TRUE */
/*declare_segment*/struct Main_m_40_L_41_t;
/*declare_segment*/typedef struct Main_m_40_L_41_t Main_m_40_L_41_t;
 /* declare_segment name:M_Main typeid:TFFFFFFFF const:FALSE */
 /* handler_name_prefixes:Main_M3_LINE_ */
 /* handler_name_prefixes:Main_I3_LINE_ */
/*declare_segment*/struct Main_m_M_Main_L_42_t;
/*declare_segment*/typedef struct Main_m_M_Main_L_42_t Main_m_M_Main_L_42_t;
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct Main_M3_Frame_t;typedef struct Main_M3_Frame_t Main_M3_Frame_t;
 /* internal_declare_param */
RT0__ModulePtr
__cdecl
Main_M3(
   /* Param_Type1 */ INTEGER /*TypeText1*/  mode_L_43);
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct Main__Print_Frame_t;typedef struct Main__Print_Frame_t Main__Print_Frame_t;
 /* internal_declare_param */
void /*TypeText3*/ 
__cdecl
Main__Print(
   /* Param_Type1 */ TEXT msg_L_44);
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct Main__ChildApply_Frame_t;typedef struct Main__ChildApply_Frame_t Main__ChildApply_Frame_t;
 /* declare_local */
 /* declare_local */
 /* internal_declare_param */
REFANY
__cdecl
Main__ChildApply(
   /* Param_Type1 */ Main__ChildClosure self_L_47);
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct Main__CheckResults_Frame_t;typedef struct Main__CheckResults_Frame_t Main__CheckResults_Frame_t;
void /*TypeText3*/ 
__cdecl
Main__CheckResults(void);
 /* declare_local */
 /* declare_local */
 /* Locals_begin_procedure */
 /* begin_block */
 /* declare_temp */
 /* declare_local */
 /* Locals_end_procedure */
 /* end_block */
 /* Locals_begin_procedure */
 /* begin_block */
 /* declare_local */
 /* declare_temp */
 /* declare_local */
 /* declare_local */
 /* AllocateTemps_check_nil */
 /* AllocateTemps_common */
 /* declare_temp */
 /* declare_local */
 /* declare_temp */
 /* declare_local */
 /* AllocateTemps_check_index */
 /* AllocateTemps_common */
 /* declare_temp */
 /* declare_local */
 /* declare_temp */
 /* declare_local */
 /* AllocateTemps_check_lo */
 /* AllocateTemps_common */
 /* declare_temp */
 /* declare_local */
 /* AllocateTemps_check_lo */
 /* AllocateTemps_common */
 /* declare_temp */
 /* declare_local */
 /* declare_local */
 /* AllocateTemps_check_lo */
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
 /* declare_temp */
 /* declare_local */
 /* declare_temp */
 /* declare_local */
 /* declare_temp */
 /* declare_local */
 /* declare_local */
 /* declare_local */
 /* AllocateTemps_check_index */
 /* AllocateTemps_common */
 /* declare_temp */
 /* declare_local */
 /* declare_local */
 /* declare_local */
 /* declare_temp */
 /* declare_local */
 /* AllocateTemps_check_index */
 /* AllocateTemps_common */
 /* declare_temp */
 /* declare_local */
 /* declare_temp */
 /* declare_local */
 /* declare_local */
 /* declare_temp */
 /* declare_local */
 /* declare_temp */
 /* declare_local */
 /* declare_temp */
 /* declare_local */
 /* AllocateTemps_check_index */
 /* AllocateTemps_common */
 /* declare_temp */
 /* declare_local */
 /* AllocateTemps_check_lo */
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
 /* AllocateTemps_check_index */
 /* AllocateTemps_common */
 /* declare_temp */
 /* declare_local */
 /* declare_local */
 /* declare_temp */
 /* declare_local */
 /* declare_temp */
 /* declare_local */
 /* declare_temp */
 /* declare_local */
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
 /* declare_local */
 /* declare_local */
 /* declare_temp */
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
 /* declare_temp */
 /* declare_local */
 /* declare_local */
 /* declare_temp */
 /* declare_local */
 /* declare_local */
 /* declare_local */
 /* declare_local */
 /* AllocateTemps_check_index */
 /* AllocateTemps_common */
 /* declare_temp */
 /* declare_local */
 /* declare_local */
 /* declare_local */
 /* AllocateTemps_check_index */
 /* AllocateTemps_common */
 /* declare_temp */
 /* declare_local */
 /* AllocateTemps_check_hi */
 /* AllocateTemps_common */
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
 /* init_int */
 /* init_int */
 /* init_int */
 /* init_int */
 /* init_int */
 /* init_int */
 /* init_int */
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
struct Main_m_40_L_41_t{ADDRESS L_162[5];
INT64 L_163[1];
ADDRESS L_164[1];
INT64 L_165[1];
UINT8 L_166[1];
char L_167[7];
INT64 L_168[1];
ADDRESS L_169[1];
INT64 L_170[1];
UINT8 L_171[55];
char L_172[1];
INT64 L_173[1];
ADDRESS L_174[1];
INT64 L_175[1];
UINT8 L_176[4];
char L_177[4];
INT64 L_178[1];
ADDRESS L_179[1];
INT64 L_180[1];
UINT8 L_181[35];
char L_182[5];
INT64 L_183[1];
ADDRESS L_184[1];
INT64 L_185[1];
UINT8 L_186[35];
char L_187[5];
INT64 L_188[1];
ADDRESS L_189[1];
INT64 L_190[1];
UINT8 L_191[34];
char L_192[6];
INT64 L_193[1];
ADDRESS L_194[1];
INT64 L_195[1];
UINT8 L_196[1];
char L_197[7];
INT64 L_198[1];
ADDRESS L_199[1];
INT64 L_200[1];
UINT8 L_201[2];
char L_202[6];
INT64 L_203[1];
ADDRESS L_204[1];
INT64 L_205[1];
UINT8 L_206[1];
char L_207[7];
INT64 L_208[1];
ADDRESS L_209[1];
INT64 L_210[1];
UINT8 L_211[19];
char L_212[5];
INT64 L_213[1];
ADDRESS L_214[1];
INT64 L_215[1];
UINT8 L_216[68];
char L_217[4];
INT64 L_218[1];
ADDRESS L_219[1];
INT64 L_220[1];
UINT8 L_221[2];
char L_222[6];
INT64 L_223[1];
ADDRESS L_224[1];
INT64 L_225[1];
UINT8 L_226[79];
char L_227[1];
INT64 L_228[1];
ADDRESS L_229[1];
INT64 L_230[1];
UINT8 L_231[79];
char L_232[1];
INT64 L_233[1];
ADDRESS L_234[1];
INT64 L_235[1];
UINT8 L_236[23];
char L_237[1];
INT64 L_238[1];
ADDRESS L_239[1];
INT64 L_240[1];
UINT8 L_241[62];
char L_242[2];
INT64 L_243[1];
ADDRESS L_244[1];
INT64 L_245[1];
UINT8 L_246[49];
char L_247[7];
INT64 L_248[1];
ADDRESS L_249[1];
INT64 L_250[1];
UINT8 L_251[54];
char L_252[2];
INT64 L_253[1];
ADDRESS L_254[1];
INT64 L_255[1];
UINT8 L_256[6];
char L_257[2];
INT64 L_258[1];
ADDRESS L_259[1];
INT64 L_260[1];
UINT8 L_261[25];
char L_262[7];
INT64 L_263[1];
ADDRESS L_264[1];
INT64 L_265[1];
UINT8 L_266[28];
char L_267[4];
INT64 L_268[1];
ADDRESS L_269[1];
INT64 L_270[1];
UINT8 L_271[4];
char L_272[4];
INT64 L_273[1];
ADDRESS L_274[1];
INT64 L_275[1];
UINT8 L_276[9];
char L_277[7];
UINT8 L_278[7];
char L_279[1];
UINT8 L_280[12];
char L_281[1];
UINT8 L_282[10];
char L_283[1];
UINT8 L_284[5];
char L_285[3];
ADDRESS L_286[8];
char L_287[8];
INT8 L_288[1];
UINT8 L_289[1];
INT8 L_290[5];
UINT8 L_291[10];
char L_292[1];
INT8 L_293[14];
UINT8 L_294[2];
INT8 L_295[11];
UINT8 L_296[17];
char L_297[1];
INT8 L_298[6];
char L_299[3];
ADDRESS L_300[1];
UINT8 L_301[17];
char L_302[7];
};
static  const Main_m_40_L_41_t Main_m_40_L_41={{(ADDRESS)&RTHooks__TextLitInfo,(ADDRESS)&RTHooks__TextLitGetChar,(ADDRESS)&RTHooks__TextLitGetWideChar,(ADDRESS)&RTHooks__TextLitGetChars,(ADDRESS)&RTHooks__TextLitGetWideChars},{INT64_(2)},{(char*)&Main_m_40_L_41},{INT64_(1)},{10},{0 /* 1 */ ,0 /* 2 */ ,0 /* 3 */ ,0 /* 4 */ ,0 /* 5 */ ,0 /* 6 */ ,0 /* 7 */ ,},{INT64_(2)},{(char*)&Main_m_40_L_41},{INT64_(55)},{'!','!','!',' ','S','o','m','e','t','h','i','n','g',' ','r','e','a','l','l','y',' ','b','r','o','k','e','n',' ','i','n',' ','C','M','3',' ','b','e','c','a','u','s','e',' ','s','h','a','r','e','d','A','r','r','a','y','['},{0 /* 1 */ ,},{INT64_(2)},{(char*)&Main_m_40_L_41},{INT64_(4)},{']',' ','=',' '},{0 /* 1 */ ,0 /* 2 */ ,0 /* 3 */ ,0 /* 4 */ ,},{INT64_(2)},{(char*)&Main_m_40_L_41},{INT64_(35)},{' ','w','h','i','c','h',' ','i','s',' ','g','r','e','a','t','e','r',' ','t','h','a','n',' ','m','a','x','C','o','u','n','t',' ','!','!','!'},{0 /* 1 */ ,0 /* 2 */ ,0 /* 3 */ ,0 /* 4 */ ,0 /* 5 */ 
,},{INT64_(2)},{(char*)&Main_m_40_L_41},{INT64_(35)},{10,'H','I','S','T','O','G','R','A','M',':',' ',' ','(','r','e','s','u','l','t',' ','s','h','o','u','l','d',' ','b','e',' ','[','1',':',' '},{0 /* 1 */ ,0 /* 2 */ ,0 /* 3 */ ,0 /* 4 */ ,0 /* 5 */ ,},{INT64_(2)},{(char*)&Main_m_40_L_41},{INT64_(34)},{']',' ','w','i','t','h',' ','n','o',' ','o','t','h','e','r',' ','e','n','t','r','i','e','s',')',10,'-','-','-','-','-','-','-','-','-'},{0 /* 1 */ ,0 /* 2 */ ,0 /* 3 */ ,0 /* 4 */ ,0 /* 5 */ ,0 /* 6 */ ,},{INT64_(2)},{(char*)&Main_m_40_L_41},{INT64_(1)},{'['},{0 /* 1 */ ,0 /* 2 */ ,0 /* 3 */ ,0 /* 4 */ ,0 /* 5 */ ,0 /* 6 */ ,0 /* 7 */ ,},{INT64_(2)},{(char*)&Main_m_40_L_41},{INT64_(2)},{':',' '},{0 /* 1 */ ,0 /* 2 */ ,0 /* 3 */ ,0 /* 4 */ ,0 /* 5 */ ,0 /* 6 */ ,},{INT64_(2)},{(char*)&Main_m_40_L_41},{INT64_(1)},{']'},{0 /* 1 */ ,0 /* 2 */ ,0 /* 3 */ ,0 /* 4 */ ,0 /* 5 */ ,0 /* 6 */ ,0 /* 7 */ ,},{INT64_(2)},{(char*)&Main_m_40_L_41},{INT64_(19)},{10,'!',' ','E','R','R','O','R',' ','D','E',
'T','E','C','T','E','D',' ','!'},{0 /* 1 */ ,0 /* 2 */ ,0 /* 3 */ ,0 /* 4 */ ,0 /* 5 */ ,},{INT64_(2)},{(char*)&Main_m_40_L_41},{INT64_(68)},{10,'!','!','!',' ','S','o','m','e','t','h','i','n','g',' ','i','s',' ','b','r','o','k','e','n',' ','i','n',' ','t','h','e',' ','C','M','3',' ','s','y','s','t','e','m',' ','a','n','d',' ','n','e','e','d','s',' ','t','o',' ','b','e',' ','f','i','x','e','d',' ','!','!','!'},{0 /* 1 */ ,0 /* 2 */ ,0 /* 3 */ ,0 /* 4 */ ,},{INT64_(2)},{(char*)&Main_m_40_L_41},{INT64_(2)},{'O','K'},{0 /* 1 */ ,0 /* 2 */ ,0 /* 3 */ ,0 /* 4 */ ,0 /* 5 */ ,0 /* 6 */ ,},{INT64_(2)},{(char*)&Main_m_40_L_41},{INT64_(79)},{'-','-','-','-','-','-','-','-','-','-','-','-','-','-','-','-','-','-','-','-','-','-','-','-','-','-','-','-','-','-','-','-','-','-','-','-','-','-','-','-','-','-','-','-','-','-','-','-','-','-','-','-','-','-','-','-','-','-','-','-','-','-','-','-','-','-','-','-','-','-','-','-','-','-','-','-','-','-','-'},{0 /* 1 */ ,},{INT64_(2)},{(char*)&Main_m_40_L_41
},{INT64_(79)},{'T','h','i','s',' ','p','r','o','g','r','a','m',' ','d','e','s','i','g','n','e','d',' ','t','o',' ','t','e','s','t',' ','i','f',' ','M','U','T','E','X',' ','w','o','r','k','i','n','g',' ','p','r','o','p','e','r','l','y',' ','u','s','i','n','g',' ','m','u','l','t','i','p','l','e',' ','t','h','r','e','a','d','s','.'},{0 /* 1 */ ,},{INT64_(2)},{(char*)&Main_m_40_L_41},{INT64_(23)},{'A','u','t','h','o','r',':',' ',' ','R','a','n','d','y',' ','C','o','l','e','b','u','r','n'},{0 /* 1 */ ,},{INT64_(2)},{(char*)&Main_m_40_L_41},{INT64_(62)},{'I','n','s','p','i','r','a','t','i','o','n',':',' ',' ','"','T','h','e',' ','L','i','t','t','l','e',' ','B','o','o','k',' ','o','f',' ','S','e','m','a','p','h','o','r','e','s','"',',',' ','b','y',' ','A','l','l','e','n',' ','D','o','w','n','e','y'},{0 /* 1 */ ,0 /* 2 */ ,},{INT64_(2)},{(char*)&Main_m_40_L_41},{INT64_(49)},{' ',' ',' ',' ',' ',' ',' ',' ',' ',' ',' ',' ',' ',' ','S','e','c','t','i','o','n',' ','8','.','1',':',' ','M','u','t'
,'e','x',' ','c','h','e','c','k','e','r',' ','p','r','o','b','l','e','m','.'},{0 /* 1 */ ,0 /* 2 */ ,0 /* 3 */ ,0 /* 4 */ ,0 /* 5 */ ,0 /* 6 */ ,0 /* 7 */ ,},{INT64_(2)},{(char*)&Main_m_40_L_41},{INT64_(54)},{' ',' ',' ',' ',' ',' ',' ',' ',' ',' ',' ',' ',' ',' ','h','t','t','p',':','/','/','w','w','w','.','g','r','e','e','n','t','e','a','p','r','e','s','s','.','c','o','m','/','s','e','m','a','p','h','o','r','e','s','/'},{0 /* 1 */ ,0 /* 2 */ ,},{INT64_(2)},{(char*)&Main_m_40_L_41},{INT64_(6)},{'U','s','i','n','g',' '},{0 /* 1 */ ,0 /* 2 */ ,},{INT64_(2)},{(char*)&Main_m_40_L_41},{INT64_(25)},{' ','t','h','r','e','a','d','s',' ','w','i','t','h',' ','m','a','x','C','o','u','n','t',' ','=',' '},{0 /* 1 */ ,0 /* 2 */ ,0 /* 3 */ ,0 /* 4 */ ,0 /* 5 */ ,0 /* 6 */ ,0 /* 7 */ ,},{INT64_(2)},{(char*)&Main_m_40_L_41},{INT64_(28)},{'E','x','p','e','c','t','e','d',' ','r','u','n','t','i','m','e',' ','i','s',' ','a','p','p','r','o','x','.',' '},{0 /* 1 */ ,0 /* 2 */ ,0 /* 3 */ ,0 /* 4 */ ,},{INT64_(2)
},{(char*)&Main_m_40_L_41},{INT64_(4)},{' ','t','o',' '},{0 /* 1 */ ,0 /* 2 */ ,0 /* 3 */ ,0 /* 4 */ ,},{INT64_(2)},{(char*)&Main_m_40_L_41},{INT64_(9)},{' ','m','i','n','u','t','e','s','.'},{0 /* 1 */ ,0 /* 2 */ ,0 /* 3 */ ,0 /* 4 */ ,0 /* 5 */ ,0 /* 6 */ ,0 /* 7 */ ,},{'M','a','i','n','_','M','3'},{0 /* 1 */ ,},{'C','h','e','c','k','R','e','s','u','l','t','s'},{0 /* 1 */ ,},{'C','h','i','l','d','A','p','p','l','y'},{0 /* 1 */ ,},{'P','r','i','n','t'},{0 /* 1 */ ,0 /* 2 */ ,0 /* 3 */ ,},{(ADDRESS)&Main_M3,1368+(char*)&Main_m_40_L_41,(ADDRESS)&Main__CheckResults,1376+(char*)&Main_m_40_L_41,(ADDRESS)&Main__ChildApply,1389+(char*)&Main_m_40_L_41,(ADDRESS)&Main__Print,1400+(char*)&Main_m_40_L_41},{0 /* 1 */ ,0 /* 2 */ ,0 /* 3 */ ,0 /* 4 */ ,0 /* 5 */ ,0 /* 6 */ ,0 /* 7 */ ,0 /* 8 */ ,},{((INT8)43)},{128U},{((INT8)1),((INT8)4),((INT8)41),((INT8)4),((INT8)0)},{'.','.','/','M','a','i','n','.','m','3'},{0 /* 1 */ ,},{((INT8)24),((INT8)1),((INT8)4),((INT8)0),((INT8)0),((INT8)24),((INT8)1),((INT8)4)
,((INT8)0),((INT8)0),((INT8)2),((INT8)13),((INT8)1),((INT8)18)},{237U,176U},{((INT8)22),((INT8)75),((INT8)24),((INT8)1),((INT8)19),((INT8)0),((INT8)0),((INT8)2),((INT8)13),((INT8)1),((INT8)3)},{'M','a','i','n','.','C','o','u','n','t','e','r','A','r','r','a','y'},{0 /* 1 */ ,},{((INT8)19),((INT8)0),((INT8)2),((INT8)12),((INT8)1),((INT8)3)},{0 /* 1 */ ,0 /* 2 */ ,0 /* 3 */ ,},{(ADDRESS)&Main__ChildApply},{'M','a','i','n','.','C','h','i','l','d','C','l','o','s','u','r','e'},{0 /* 1 */ ,0 /* 2 */ ,0 /* 3 */ ,0 /* 4 */ ,0 /* 5 */ ,0 /* 6 */ ,0 /* 7 */ ,}};
 /* bind_segment */
 /* begin_init */
 /* init_var */
 /* init_var */
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
 /* init_var */
 /* init_var */
 /* init_int */
 /* init_int */
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
 /* init_int */
 /* init_int */
 /* init_int */
 /* init_var */
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
 /* init_var */
 /* init_proc */
 /* init_var */
 /* init_int */
 /* init_var */
 /* init_int */
 /* init_var */
 /* init_int */
 /* init_int */
 /* end_init */
struct Main_m_M_Main_L_42_t{ADDRESS L_303[3];
char L_304[16];
ADDRESS L_305[1];
char L_306[8];
ADDRESS L_307[3];
char L_308[8];
ADDRESS L_309[1];
INT64 L_310[1];
char L_311[8];
INT64 L_312[1];
UINT8 L_313[1];
INT8 L_314[1];
UINT8 L_315[3];
INT8 L_316[1];
UINT8 L_317[1];
INT8 L_318[3];
char L_319[1];
INT8 L_320[1];
char L_321[4];
INT64 L_322[1];
ADDRESS L_323[1];
char L_324[8];
ADDRESS L_325[1];
char L_326[16];
ADDRESS L_327[2];
INT64 L_328[1];
char L_329[24];
INT64 L_330[1];
ADDRESS L_331[1];
char L_332[16];
INT64 L_333[1];
INT8 L_334[3];
UINT8 L_335[1];
INT8 L_336[2];
UINT8 L_337[1];
INT8 L_338[3];
char L_339[1];
INT8 L_340[1];
char L_341[4];
INT64 L_342[1];
ADDRESS L_343[1];
char L_344[8];
ADDRESS L_345[1];
char L_346[16];
ADDRESS L_347[2];
INT64 L_348[2];
char L_349[24];
INT64 L_350[1];
char L_351[24];
INT64 L_352[1];
UINT8 L_353[1];
INT8 L_354[1];
UINT8 L_355[5];
INT8 L_356[3];
char L_357[1];
INT8 L_358[1];
char L_359[4];
INT64 L_360[1];
ADDRESS L_361[3];
char L_362[32];
INT64 L_363[2];
char L_364[8];
ADDRESS L_365[2];
char L_366[8];
ADDRESS L_367[2];
char L_368[8];
ADDRESS L_369[2];
char L_370[8];
ADDRESS L_371[2];
char L_372[8];
ADDRESS L_373[2];
char L_374[8];
ADDRESS L_375[1];
char L_376[8];
ADDRESS L_377[1];
INT64 L_378[1];
ADDRESS L_379[1];
INT64 L_380[1];
ADDRESS L_381[1];
INT64 L_382[1];
char L_383[8];
INT64 L_384[1];
};
static Main_m_M_Main_L_42_t Main_m_M_Main_L_42={{1487+(char*)&Main_m_40_L_41,104+(char*)&Main_m_M_Main_L_42,672+(char*)&Main_m_M_Main_L_42},{0 /* 1 */ ,0 /* 2 */ ,0 /* 3 */ ,0 /* 4 */ ,0 /* 5 */ ,0 /* 6 */ ,0 /* 7 */ ,0 /* 8 */ ,0 /* 9 */ ,0 /* 10 */ ,0 /* 11 */ ,0 /* 12 */ ,0 /* 13 */ ,0 /* 14 */ ,0 /* 15 */ ,0 /* 16 */ ,},{1408+(char*)&Main_m_40_L_41},{0 /* 1 */ ,0 /* 2 */ ,0 /* 3 */ ,0 /* 4 */ ,0 /* 5 */ ,0 /* 6 */ ,0 /* 7 */ ,0 /* 8 */ ,},{1480+(char*)&Main_m_40_L_41,1480+(char*)&Main_m_40_L_41,528+(char*)&Main_m_M_Main_L_42},{0 /* 1 */ ,0 /* 2 */ ,0 /* 3 */ ,0 /* 4 */ ,0 /* 5 */ ,0 /* 6 */ ,0 /* 7 */ ,0 /* 8 */ ,},{(ADDRESS)&Main_M3},{INT64_(3)},{0 /* 1 */ ,0 /* 2 */ ,0 /* 3 */ ,0 /* 4 */ ,0 /* 5 */ ,0 /* 6 */ ,0 /* 7 */ ,0 /* 8 */ ,},{INT64_(-1770097841)},{152U},{((INT8)40)},{218U,149U,215U},{((INT8)91)},{164U},{((INT8)3),((INT8)1),((INT8)2)},{0 /* 1 */ ,},{((INT8)8)},{0 /* 1 */ ,0 /* 2 */ ,0 /* 3 */ ,0 /* 4 */ ,},{INT64_(8)},{1543+(char*)&Main_m_40_L_41},{0 /* 1 */ ,0 /* 2 */ ,0 /* 3 */ 
,0 /* 4 */ ,0 /* 5 */ ,0 /* 6 */ ,0 /* 7 */ ,0 /* 8 */ ,},{1545+(char*)&Main_m_40_L_41},{0 /* 1 */ ,0 /* 2 */ ,0 /* 3 */ ,0 /* 4 */ ,0 /* 5 */ ,0 /* 6 */ ,0 /* 7 */ ,0 /* 8 */ ,0 /* 9 */ ,0 /* 10 */ ,0 /* 11 */ ,0 /* 12 */ ,0 /* 13 */ ,0 /* 14 */ ,0 /* 15 */ ,0 /* 16 */ ,},{1560+(char*)&Main_m_40_L_41,256+(char*)&Main_m_M_Main_L_42},{INT64_(-448425059)},{0 /* 1 */ ,0 /* 2 */ ,0 /* 3 */ ,0 /* 4 */ ,0 /* 5 */ ,0 /* 6 */ ,0 /* 7 */ ,0 /* 8 */ ,0 /* 9 */ ,0 /* 10 */ ,0 /* 11 */ ,0 /* 12 */ ,0 /* 13 */ ,0 /* 14 */ ,0 /* 15 */ ,0 /* 16 */ ,0 /* 17 */ ,0 /* 18 */ ,0 /* 19 */ ,0 /* 20 */ ,0 /* 21 */ ,0 /* 22 */ ,0 /* 23 */ ,0 /* 24 */ ,},{INT64_(0)},{1552+(char*)&Main_m_40_L_41},{0 /* 1 */ ,0 /* 2 */ ,0 /* 3 */ ,0 /* 4 */ ,0 /* 5 */ ,0 /* 6 */ ,0 /* 7 */ ,0 /* 8 */ ,0 /* 9 */ ,0 /* 10 */ ,0 /* 11 */ ,0 /* 12 */ ,0 /* 13 */ ,0 /* 14 */ ,0 /* 15 */ ,0 /* 16 */ ,},{INT64_(-476616643)},{((INT8)12),((INT8)75),((INT8)74)},{239U},{((INT8)49),((INT8)35)},{221U},{((INT8)12),((INT8)1),((INT8)3)},{0 /* 1 */ 
,},{((INT8)8)},{0 /* 1 */ ,0 /* 2 */ ,0 /* 3 */ ,0 /* 4 */ ,},{INT64_(16)},{1516+(char*)&Main_m_40_L_41},{0 /* 1 */ ,0 /* 2 */ ,0 /* 3 */ ,0 /* 4 */ ,0 /* 5 */ ,0 /* 6 */ ,0 /* 7 */ ,0 /* 8 */ ,},{1521+(char*)&Main_m_40_L_41},{0 /* 1 */ ,0 /* 2 */ ,0 /* 3 */ ,0 /* 4 */ ,0 /* 5 */ ,0 /* 6 */ ,0 /* 7 */ ,0 /* 8 */ ,0 /* 9 */ ,0 /* 10 */ ,0 /* 11 */ ,0 /* 12 */ ,0 /* 13 */ ,0 /* 14 */ ,0 /* 15 */ ,0 /* 16 */ ,},{1525+(char*)&Main_m_40_L_41,416+(char*)&Main_m_M_Main_L_42},{INT64_(1),INT64_(8)},{0 /* 1 */ ,0 /* 2 */ ,0 /* 3 */ ,0 /* 4 */ ,0 /* 5 */ ,0 /* 6 */ ,0 /* 7 */ ,0 /* 8 */ ,0 /* 9 */ ,0 /* 10 */ ,0 /* 11 */ ,0 /* 12 */ ,0 /* 13 */ ,0 /* 14 */ ,0 /* 15 */ ,0 /* 16 */ ,0 /* 17 */ ,0 /* 18 */ ,0 /* 19 */ ,0 /* 20 */ ,0 /* 21 */ ,0 /* 22 */ ,0 /* 23 */ ,0 /* 24 */ ,},{INT64_(10)},{0 /* 1 */ ,0 /* 2 */ ,0 /* 3 */ ,0 /* 4 */ ,0 /* 5 */ ,0 /* 6 */ ,0 /* 7 */ ,0 /* 8 */ ,0 /* 9 */ ,0 /* 10 */ ,0 /* 11 */ ,0 /* 12 */ ,0 /* 13 */ ,0 /* 14 */ ,0 /* 15 */ ,0 /* 16 */ ,0 /* 17 */ ,0 /* 18 */ ,0 /* 19 */ 
,0 /* 20 */ ,0 /* 21 */ ,0 /* 22 */ ,0 /* 23 */ ,0 /* 24 */ ,},{INT64_(-784487079)},{178U},{((INT8)16)},{248U,195U,235U,189U,197U},{((INT8)18),((INT8)1),((INT8)3)},{0 /* 1 */ ,},{((INT8)8)},{0 /* 1 */ ,0 /* 2 */ ,0 /* 3 */ ,0 /* 4 */ ,},{INT64_(16)},{1498+(char*)&Main_m_40_L_41,1503+(char*)&Main_m_40_L_41,1508+(char*)&Main_m_40_L_41},{0 /* 1 */ ,0 /* 2 */ ,0 /* 3 */ ,0 /* 4 */ ,0 /* 5 */ ,0 /* 6 */ ,0 /* 7 */ ,0 /* 8 */ ,0 /* 9 */ ,0 /* 10 */ ,0 /* 11 */ ,0 /* 12 */ ,0 /* 13 */ ,0 /* 14 */ ,0 /* 15 */ ,0 /* 16 */ ,0 /* 17 */ ,0 /* 18 */ ,0 /* 19 */ ,0 /* 20 */ ,0 /* 21 */ ,0 /* 22 */ ,0 /* 23 */ ,0 /* 24 */ ,0 /* 25 */ ,0 /* 26 */ ,0 /* 27 */ ,0 /* 28 */ ,0 /* 29 */ ,0 /* 30 */ ,0 /* 31 */ ,0 /* 32 */ ,},{INT64_(1),INT64_(8)},{0 /* 1 */ ,0 /* 2 */ ,0 /* 3 */ ,0 /* 4 */ ,0 /* 5 */ ,0 /* 6 */ ,0 /* 7 */ ,0 /* 8 */ ,},{(ADDRESS)&Main_I3,552+(char*)&Main_m_M_Main_L_42},{0 /* 1 */ ,0 /* 2 */ ,0 /* 3 */ ,0 /* 4 */ ,0 /* 5 */ ,0 /* 6 */ ,0 /* 7 */ ,0 /* 8 */ ,},{(ADDRESS)&Thread_I3,576+(char*)&Main_m_M_Main_L_42
},{0 /* 1 */ ,0 /* 2 */ ,0 /* 3 */ ,0 /* 4 */ ,0 /* 5 */ ,0 /* 6 */ ,0 /* 7 */ ,0 /* 8 */ ,},{(ADDRESS)&Process_I3,600+(char*)&Main_m_M_Main_L_42},{0 /* 1 */ ,0 /* 2 */ ,0 /* 3 */ ,0 /* 4 */ ,0 /* 5 */ ,0 /* 6 */ ,0 /* 7 */ ,0 /* 8 */ ,},{(ADDRESS)&IO_I3,624+(char*)&Main_m_M_Main_L_42},{0 /* 1 */ ,0 /* 2 */ ,0 /* 3 */ ,0 /* 4 */ ,0 /* 5 */ ,0 /* 6 */ ,0 /* 7 */ ,0 /* 8 */ ,},{(ADDRESS)&Fmt_I3,648+(char*)&Main_m_M_Main_L_42},{0 /* 1 */ ,0 /* 2 */ ,0 /* 3 */ ,0 /* 4 */ ,0 /* 5 */ ,0 /* 6 */ ,0 /* 7 */ ,0 /* 8 */ ,},{(ADDRESS)&RTHooks_I3},{0 /* 1 */ ,0 /* 2 */ ,0 /* 3 */ ,0 /* 4 */ ,0 /* 5 */ ,0 /* 6 */ ,0 /* 7 */ ,0 /* 8 */ ,},{688+(char*)&Main_m_M_Main_L_42},{INT64_(-476616643)},{704+(char*)&Main_m_M_Main_L_42},{INT64_(356643957)},{720+(char*)&Main_m_M_Main_L_42},{INT64_(-784487079)},{0 /* 1 */ ,0 /* 2 */ ,0 /* 3 */ ,0 /* 4 */ ,0 /* 5 */ ,0 /* 6 */ ,0 /* 7 */ ,0 /* 8 */ ,},{INT64_(-1770097841)}};
static void __cdecl Main_m_M_Main_L_42_CRASH(WORD_T code) M3_ATTRIBUTE_NO_RETURN;
static void __cdecl Main_m_M_Main_L_42_CRASH(WORD_T code){RTHooks__ReportFault((ADDRESS)&Main_m_M_Main_L_42,code);} /* end: segments/globals */
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
#line 124 "../Main.m3"
 /* Print */
#line 124 "../Main.m3"
 /* set_source_line */
#line 124 "../Main.m3"
#line 33 "../Main.m3"
 /* begin_procedure */
#line 33 "../Main.m3"
struct Main__Print_Frame_t {
#line 33 "../Main.m3"
ADDRESS _unused;
#line 33 "../Main.m3"
};
#line 33 "../Main.m3"
void /*TypeText3*/ 
__cdecl
Main__Print(
   /* Param_Type1 */ TEXT msg_L_44)
{
#line 33 "../Main.m3"
 /* Var_Type2 */ ADDRESS Main_m_50_L_51={0};//always-init
#line 33 "../Main.m3"
Main__Print_Frame_t _frame;
#line 33 "../Main.m3"
_frame._unused=(ADDRESS)&_frame;
#line 33 "../Main.m3"
 /* set_source_line */
#line 33 "../Main.m3"
#line 37 "../Main.m3"
 /* set_source_line */
#line 37 "../Main.m3"
#line 38 "../Main.m3"
 /* start_call_direct */
#line 38 "../Main.m3"
 /* load */
#line 38 "../Main.m3"
 /* pop_param */
#line 38 "../Main.m3"
 /* load_address */
#line 38 "../Main.m3"
 /* pop_param */
#line 38 "../Main.m3"
 /* call_direct */
#line 38 "../Main.m3"
 /* store */
#line 38 "../Main.m3"
(*(ADDRESS*)(&Main_m_50_L_51))=(ADDRESS)(((ADDRESS)(RTHooks__Concat(
  ( TEXT )(((ADDRESS)(msg_L_44)) ),
  ( TEXT )(((ADDRESS)(INT64_(48)+((ADDRESS)(&Main_m_40_L_41)))) )))));
#line 38 "../Main.m3"
 /* start_call_direct */
#line 38 "../Main.m3"
 /* load */
#line 38 "../Main.m3"
 /* pop_param */
#line 38 "../Main.m3"
 /* load_nil */
#line 38 "../Main.m3"
 /* pop_param */
#line 38 "../Main.m3"
 /* call_direct */
#line 38 "../Main.m3"
IO__Put(
  ( TEXT )(((ADDRESS)(Main_m_50_L_51)) ),
  ( Wr__T )(((ADDRESS)(0)) ));
#line 38 "../Main.m3"
 /* set_source_line */
#line 38 "../Main.m3"
#line 39 "../Main.m3"
 /* exit_proc */
#line 39 "../Main.m3"
return;
#line 39 "../Main.m3"
 /* end_procedure */
#line 39 "../Main.m3"
} /* ChildApply */
#line 39 "../Main.m3"
 /* set_source_line */
#line 39 "../Main.m3"
#line 43 "../Main.m3"
 /* begin_procedure */
#line 43 "../Main.m3"
struct Main__ChildApply_Frame_t {
#line 43 "../Main.m3"
ADDRESS _unused;
#line 43 "../Main.m3"
};
#line 43 "../Main.m3"
REFANY
__cdecl
Main__ChildApply(
   /* Param_Type1 */ Main__ChildClosure self_L_47)
{
#line 43 "../Main.m3"
 /* Var_Type1 */ WORD_T numLoops_L_45={0};//always-init
#line 43 "../Main.m3"
 /* Var_Type2 */ ADDRESS Main_m_52_L_53={0};//always-init
#line 43 "../Main.m3"
 /* Var_Type2 */ ADDRESS Main_m_54_L_55={0};//always-init
#line 43 "../Main.m3"
 /* Var_Type1 */ MUTEX Main_m_56_L_57={0};//always-init
#line 43 "../Main.m3"
 /* Var_Type2 */ ADDRESS Main_m_58_L_59={0};//always-init
#line 43 "../Main.m3"
 /* Var_Type2 */ ADDRESS Main_m_60_L_61={0};//always-init
#line 43 "../Main.m3"
 /* Var_Type2 */ INT64 Main_m_62_L_63={0};//always-init
#line 43 "../Main.m3"
 /* Var_Type2 */ ADDRESS Main_m_64_L_65={0};//always-init
#line 43 "../Main.m3"
 /* Var_Type2 */ INT64 Main_m_66_L_67={0};//always-init
#line 43 "../Main.m3"
 /* Var_Type2 */ INT64 Main_m_68_L_69={0};//always-init
#line 43 "../Main.m3"
 /* Var_Type2 */ ADDRESS Main_m_70_L_71={0};//always-init
#line 43 "../Main.m3"
 /* Var_Type2 */ INT64 Main_m_72_L_73={0};//always-init
#line 43 "../Main.m3"
 /* Var_Type2 */ ADDRESS Main_m_74_L_75={0};//always-init
#line 43 "../Main.m3"
Main__ChildApply_Frame_t _frame;
#line 43 "../Main.m3"
_frame._unused=(ADDRESS)&_frame;
#line 43 "../Main.m3"
 /* set_source_line */
#line 43 "../Main.m3"
#line 48 "../Main.m3"
 /* load_integer */
#line 48 "../Main.m3"
 /* store */
#line 48 "../Main.m3"
(*(UINT64*)(&numLoops_L_45))=(INT64)(  INT64_(0));
#line 48 "../Main.m3"
 /* set_source_line */
#line 48 "../Main.m3"
#line 47 "../Main.m3"
 /* set_source_line */
#line 47 "../Main.m3"
#line 50 "../Main.m3"
 /* load_nil */
#line 50 "../Main.m3"
 /* store */
#line 50 "../Main.m3"
(*(ADDRESS*)(&Main_m_52_L_53))=(ADDRESS)(((ADDRESS)(0)));
#line 50 "../Main.m3"
 /* load */
#line 50 "../Main.m3"
 /* store */
#line 50 "../Main.m3"
(*(ADDRESS*)(&Main_m_54_L_55))=(ADDRESS)(((ADDRESS)(*((ADDRESS*)(INT64_(384)+((ADDRESS)(&Main_m_M_Main_L_42)))))));
#line 50 "../Main.m3"
 /* load_nil */
#line 50 "../Main.m3"
 /* load */
#line 50 "../Main.m3"
 /* if_compare */
#line 50 "../Main.m3"
if(m3_eq(ADDRESS,
 ((ADDRESS)(0)),
 ((ADDRESS)(Main_m_54_L_55))))goto L1;
#line 50 "../Main.m3"
 /* load */
#line 50 "../Main.m3"
 /* loophole */
#line 50 "../Main.m3"
 /* load_integer */
#line 50 "../Main.m3"
 /* and */
#line 50 "../Main.m3"
 /* if_true_or_false */
#line 50 "../Main.m3"
 /* load_host_integer */
#line 50 "../Main.m3"
 /* load_integer */
#line 50 "../Main.m3"
 /* if_compare */
#line 50 "../Main.m3"
if(m3_ne(INT64,
  ((INT64)(((INT64)((INT64)Main_m_54_L_55))&  INT64_(1))),
   INT64_(0)))goto L1;
#line 50 "../Main.m3"
 /* load */
#line 50 "../Main.m3"
 /* load_indirect */
#line 50 "../Main.m3"
 /* extract_mn */
#line 50 "../Main.m3"
 /* load_host_integer */
#line 50 "../Main.m3"
 /* load_integer */
#line 50 "../Main.m3"
 /* load_host_integer */
#line 50 "../Main.m3"
 /* load_integer */
#line 50 "../Main.m3"
 /* extract */
#line 50 "../Main.m3"
 /* if_true_or_false */
#line 50 "../Main.m3"
 /* load_host_integer */
#line 50 "../Main.m3"
 /* load_integer */
#line 50 "../Main.m3"
 /* if_compare */
#line 50 "../Main.m3"
if(m3_eq(INT64,
 ((INT64)(m3_extract(
 UINT64,
  *((INT64*)(INT64_(-8)+((ADDRESS)(Main_m_54_L_55)))),
   UINT64_(22),
   UINT64_(1)))),
   INT64_(0)))goto L1;
#line 50 "../Main.m3"
 /* start_call_direct */
#line 50 "../Main.m3"
 /* load */
#line 50 "../Main.m3"
 /* pop_param */
#line 50 "../Main.m3"
 /* call_direct */
#line 50 "../Main.m3"
RTHooks__CheckLoadTracedRef(
  ( REFANY )(((ADDRESS)(Main_m_54_L_55)) ));
#line 50 "../Main.m3"
 /* set_label */
#line 50 "../Main.m3"
L1:;
#line 50 "../Main.m3"
 /* load */
#line 50 "../Main.m3"
 /* store */
#line 50 "../Main.m3"
(*(ADDRESS*)(&Main_m_56_L_57))=(ADDRESS)(((ADDRESS)(Main_m_54_L_55)));
#line 50 "../Main.m3"
 /* start_call_indirect */
#line 50 "../Main.m3"
 /* load */
#line 50 "../Main.m3"
 /* pop_param */
#line 50 "../Main.m3"
 /* load */
#line 50 "../Main.m3"
 /* load_indirect */
#line 50 "../Main.m3"
 /* load_indirect */
#line 50 "../Main.m3"
 /* check_nil */
#line 50 "../Main.m3"
 /* store */
#line 50 "../Main.m3"
(*(ADDRESS*)(&Main_m_58_L_59))=(ADDRESS)(((ADDRESS)(*((ADDRESS*)(*((ADDRESS*)(Main_m_56_L_57)))))));
#line 50 "../Main.m3"
 /* load */
#line 50 "../Main.m3"
/*check_nil*/if(!Main_m_58_L_59)Main_m_M_Main_L_42_CRASH(1604);
#line 50 "../Main.m3"
 /* call_indirect */
#line 50 "../Main.m3"
((void (__cdecl*)(void*))Main_m_58_L_59)(
 ((ADDRESS)(Main_m_56_L_57)));
#line 50 "../Main.m3"
 /* set_label */
#line 50 "../Main.m3"
 /* start_try */
#line 50 "../Main.m3"
try {
#line 50 "../Main.m3"
 /* set_source_line */
#line 50 "../Main.m3"
#line 51 "../Main.m3"
 /* jump */
#line 51 "../Main.m3"
goto L7;
#line 51 "../Main.m3"
 /* set_label */
#line 51 "../Main.m3"
L6:;
#line 51 "../Main.m3"
 /* set_source_line */
#line 51 "../Main.m3"
#line 53 "../Main.m3"
 /* load */
#line 53 "../Main.m3"
 /* store */
#line 53 "../Main.m3"
(*(ADDRESS*)(&Main_m_54_L_55))=(ADDRESS)(((ADDRESS)(*((ADDRESS*)(INT64_(400)+((ADDRESS)(&Main_m_M_Main_L_42)))))));
#line 53 "../Main.m3"
 /* load_nil */
#line 53 "../Main.m3"
 /* load */
#line 53 "../Main.m3"
 /* if_compare */
#line 53 "../Main.m3"
if(m3_eq(ADDRESS,
 ((ADDRESS)(0)),
 ((ADDRESS)(Main_m_54_L_55))))goto L9;
#line 53 "../Main.m3"
 /* load */
#line 53 "../Main.m3"
 /* loophole */
#line 53 "../Main.m3"
 /* load_integer */
#line 53 "../Main.m3"
 /* and */
#line 53 "../Main.m3"
 /* if_true_or_false */
#line 53 "../Main.m3"
 /* load_host_integer */
#line 53 "../Main.m3"
 /* load_integer */
#line 53 "../Main.m3"
 /* if_compare */
#line 53 "../Main.m3"
if(m3_ne(INT64,
  ((INT64)(((INT64)((INT64)Main_m_54_L_55))&  INT64_(1))),
   INT64_(0)))goto L9;
#line 53 "../Main.m3"
 /* load */
#line 53 "../Main.m3"
 /* load_indirect */
#line 53 "../Main.m3"
 /* extract_mn */
#line 53 "../Main.m3"
 /* load_host_integer */
#line 53 "../Main.m3"
 /* load_integer */
#line 53 "../Main.m3"
 /* load_host_integer */
#line 53 "../Main.m3"
 /* load_integer */
#line 53 "../Main.m3"
 /* extract */
#line 53 "../Main.m3"
 /* if_true_or_false */
#line 53 "../Main.m3"
 /* load_host_integer */
#line 53 "../Main.m3"
 /* load_integer */
#line 53 "../Main.m3"
 /* if_compare */
#line 53 "../Main.m3"
if(m3_eq(INT64,
 ((INT64)(m3_extract(
 UINT64,
  *((INT64*)(INT64_(-8)+((ADDRESS)(Main_m_54_L_55)))),
   UINT64_(22),
   UINT64_(1)))),
   INT64_(0)))goto L9;
#line 53 "../Main.m3"
 /* start_call_direct */
#line 53 "../Main.m3"
 /* load */
#line 53 "../Main.m3"
 /* pop_param */
#line 53 "../Main.m3"
 /* invoke_direct */
#line 53 "../Main.m3"
 /* call_direct */
#line 53 "../Main.m3"
RTHooks__CheckLoadTracedRef(
  ( REFANY )(((ADDRESS)(Main_m_54_L_55)) ));
#line 53 "../Main.m3"
 /* set_label */
#line 53 "../Main.m3"
 /* set_label */
#line 53 "../Main.m3"
L9:;
#line 53 "../Main.m3"
 /* load */
#line 53 "../Main.m3"
 /* store */
#line 53 "../Main.m3"
(*(ADDRESS*)(&Main_m_60_L_61))=(ADDRESS)(((ADDRESS)(Main_m_54_L_55)));
#line 53 "../Main.m3"
 /* load */
#line 53 "../Main.m3"
 /* load_indirect */
#line 53 "../Main.m3"
 /* load */
#line 53 "../Main.m3"
 /* load_indirect */
#line 53 "../Main.m3"
 /* load */
#line 53 "../Main.m3"
 /* swap */
#line 53 "../Main.m3"
 /* check_index */
#line 53 "../Main.m3"
 /* swap */
#line 53 "../Main.m3"
 /* store */
#line 53 "../Main.m3"
(*(INT64*)(&Main_m_62_L_63))=(INT64)( ((INT64)(*((UINT64*)(INT64_(408)+((ADDRESS)(&Main_m_M_Main_L_42)))))));
#line 53 "../Main.m3"
 /* load */
#line 53 "../Main.m3"
 /* swap */
#line 53 "../Main.m3"
/*check_index*/if(((UINT64)(*((INT64*)(INT64_(8)+((ADDRESS)(Main_m_60_L_61))))))<=((UINT64)(Main_m_62_L_63)))Main_m_M_Main_L_42_CRASH(1698);
#line 53 "../Main.m3"
 /* index_address */
#line 53 "../Main.m3"
 /* store */
#line 53 "../Main.m3"
(*(ADDRESS*)(&Main_m_64_L_65))=(ADDRESS)(((ADDRESS)((((ADDRESS)(*((ADDRESS*)(Main_m_60_L_61))))+(8*( Main_m_62_L_63))))));
#line 53 "../Main.m3"
 /* load */
#line 53 "../Main.m3"
 /* load_indirect */
#line 53 "../Main.m3"
 /* load_integer */
#line 53 "../Main.m3"
 /* add */
#line 53 "../Main.m3"
 /* check_lo */
#line 53 "../Main.m3"
 /* store */
#line 53 "../Main.m3"
(*(INT64*)(&Main_m_66_L_67))=(INT64)( ((INT64)( ((INT64)(*((UINT64*)(Main_m_64_L_65))))+  INT64_(1))));
#line 53 "../Main.m3"
 /* load */
#line 53 "../Main.m3"
/*check_lo*/if(Main_m_66_L_67<INT64_(0))Main_m_M_Main_L_42_CRASH(1697);
#line 53 "../Main.m3"
 /* load */
#line 53 "../Main.m3"
 /* swap */
#line 53 "../Main.m3"
 /* store_indirect */
#line 53 "../Main.m3"
(*(UINT64*)(Main_m_64_L_65))=(INT64)( Main_m_66_L_67);
#line 53 "../Main.m3"
 /* set_source_line */
#line 53 "../Main.m3"
#line 54 "../Main.m3"
 /* load_integer */
#line 54 "../Main.m3"
 /* load */
#line 54 "../Main.m3"
 /* add */
#line 54 "../Main.m3"
 /* check_lo */
#line 54 "../Main.m3"
 /* store */
#line 54 "../Main.m3"
(*(INT64*)(&Main_m_68_L_69))=(INT64)( ((INT64)(  INT64_(1)+ ((INT64)(*((UINT64*)(INT64_(408)+((ADDRESS)(&Main_m_M_Main_L_42)))))))));
#line 54 "../Main.m3"
 /* load */
#line 54 "../Main.m3"
/*check_lo*/if(Main_m_68_L_69<INT64_(0))Main_m_M_Main_L_42_CRASH(1729);
#line 54 "../Main.m3"
 /* store */
#line 54 "../Main.m3"
(*(UINT64*)((408)+(char*)(&Main_m_M_Main_L_42)))=(INT64)( Main_m_68_L_69);
#line 54 "../Main.m3"
 /* set_source_line */
#line 54 "../Main.m3"
#line 55 "../Main.m3"
 /* load */
#line 55 "../Main.m3"
 /* store */
#line 55 "../Main.m3"
(*(ADDRESS*)(&Main_m_64_L_65))=(ADDRESS)(((ADDRESS)(*((ADDRESS*)(INT64_(384)+((ADDRESS)(&Main_m_M_Main_L_42)))))));
#line 55 "../Main.m3"
 /* load_nil */
#line 55 "../Main.m3"
 /* load */
#line 55 "../Main.m3"
 /* if_compare */
#line 55 "../Main.m3"
if(m3_eq(ADDRESS,
 ((ADDRESS)(0)),
 ((ADDRESS)(Main_m_64_L_65))))goto LB;
#line 55 "../Main.m3"
 /* load */
#line 55 "../Main.m3"
 /* loophole */
#line 55 "../Main.m3"
 /* load_integer */
#line 55 "../Main.m3"
 /* and */
#line 55 "../Main.m3"
 /* if_true_or_false */
#line 55 "../Main.m3"
 /* load_host_integer */
#line 55 "../Main.m3"
 /* load_integer */
#line 55 "../Main.m3"
 /* if_compare */
#line 55 "../Main.m3"
if(m3_ne(INT64,
  ((INT64)(((INT64)((INT64)Main_m_64_L_65))&  INT64_(1))),
   INT64_(0)))goto LB;
#line 55 "../Main.m3"
 /* load */
#line 55 "../Main.m3"
 /* load_indirect */
#line 55 "../Main.m3"
 /* extract_mn */
#line 55 "../Main.m3"
 /* load_host_integer */
#line 55 "../Main.m3"
 /* load_integer */
#line 55 "../Main.m3"
 /* load_host_integer */
#line 55 "../Main.m3"
 /* load_integer */
#line 55 "../Main.m3"
 /* extract */
#line 55 "../Main.m3"
 /* if_true_or_false */
#line 55 "../Main.m3"
 /* load_host_integer */
#line 55 "../Main.m3"
 /* load_integer */
#line 55 "../Main.m3"
 /* if_compare */
#line 55 "../Main.m3"
if(m3_eq(INT64,
 ((INT64)(m3_extract(
 UINT64,
  *((INT64*)(INT64_(-8)+((ADDRESS)(Main_m_64_L_65)))),
   UINT64_(22),
   UINT64_(1)))),
   INT64_(0)))goto LB;
#line 55 "../Main.m3"
 /* start_call_direct */
#line 55 "../Main.m3"
 /* load */
#line 55 "../Main.m3"
 /* pop_param */
#line 55 "../Main.m3"
 /* invoke_direct */
#line 55 "../Main.m3"
 /* call_direct */
#line 55 "../Main.m3"
RTHooks__CheckLoadTracedRef(
  ( REFANY )(((ADDRESS)(Main_m_64_L_65)) ));
#line 55 "../Main.m3"
 /* set_label */
#line 55 "../Main.m3"
 /* set_label */
#line 55 "../Main.m3"
LB:;
#line 55 "../Main.m3"
 /* start_call_direct */
#line 55 "../Main.m3"
 /* load */
#line 55 "../Main.m3"
 /* pop_param */
#line 55 "../Main.m3"
 /* invoke_direct */
#line 55 "../Main.m3"
 /* call_direct */
#line 55 "../Main.m3"
Thread__Release(
  ( Thread__Mutex )(((ADDRESS)(Main_m_64_L_65)) ));
#line 55 "../Main.m3"
 /* set_label */
#line 55 "../Main.m3"
 /* set_source_line */
#line 55 "../Main.m3"
#line 56 "../Main.m3"
 /* load_nil */
#line 56 "../Main.m3"
 /* store */
#line 56 "../Main.m3"
(*(ADDRESS*)(&Main_m_70_L_71))=(ADDRESS)(((ADDRESS)(0)));
#line 56 "../Main.m3"
 /* set_label */
#line 56 "../Main.m3"
 /* start_try */
#line 56 "../Main.m3"
try {
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
 /* check_lo */
#line 57 "../Main.m3"
 /* store */
#line 57 "../Main.m3"
(*(INT64*)(&Main_m_72_L_73))=(INT64)( ((INT64)(  INT64_(1)+ ((INT64)(numLoops_L_45)))));
#line 57 "../Main.m3"
 /* load */
#line 57 "../Main.m3"
/*check_lo*/if(Main_m_72_L_73<INT64_(0))Main_m_M_Main_L_42_CRASH(1825);
#line 57 "../Main.m3"
 /* store */
#line 57 "../Main.m3"
(*(UINT64*)(&numLoops_L_45))=(INT64)( Main_m_72_L_73);
#line 57 "../Main.m3"
 /* set_source_line */
#line 57 "../Main.m3"
#line 58 "../Main.m3"
 /* start_call_direct */
#line 58 "../Main.m3"
 /* load_float */
#line 58 "../Main.m3"
 /* pop_param */
#line 58 "../Main.m3"
 /* invoke_direct */
#line 58 "../Main.m3"
 /* call_direct */
#line 58 "../Main.m3"
Thread__Pause(
  ( LONGREAL )( ((double)(1.10000000000000001e-1)) ));
#line 58 "../Main.m3"
 /* set_label */
#line 58 "../Main.m3"
 /* jump */
#line 58 "../Main.m3"
goto L10;
#line 58 "../Main.m3"
 /* end_try */
#line 58 "../Main.m3"
} catch (_M3Exc& _m3exc) { _m3_caught = _m3exc.act; goto LF; }
#line 58 "../Main.m3"
 /* set_label */
#line 58 "../Main.m3"
LF:;
#line 58 "../Main.m3"
 /* landing_pad */
#line 58 "../Main.m3"
 /* store */
#line 58 "../Main.m3"
(*(ADDRESS*)(&Main_m_70_L_71))=(ADDRESS)(((ADDRESS)(_m3_caught)));
#line 58 "../Main.m3"
 /* set_label */
#line 58 "../Main.m3"
L10:;
#line 58 "../Main.m3"
 /* set_source_line */
#line 58 "../Main.m3"
#line 60 "../Main.m3"
 /* load */
#line 60 "../Main.m3"
 /* store */
#line 60 "../Main.m3"
(*(ADDRESS*)(&Main_m_64_L_65))=(ADDRESS)(((ADDRESS)(*((ADDRESS*)(INT64_(384)+((ADDRESS)(&Main_m_M_Main_L_42)))))));
#line 60 "../Main.m3"
 /* load_nil */
#line 60 "../Main.m3"
 /* load */
#line 60 "../Main.m3"
 /* if_compare */
#line 60 "../Main.m3"
if(m3_eq(ADDRESS,
 ((ADDRESS)(0)),
 ((ADDRESS)(Main_m_64_L_65))))goto L13;
#line 60 "../Main.m3"
 /* load */
#line 60 "../Main.m3"
 /* loophole */
#line 60 "../Main.m3"
 /* load_integer */
#line 60 "../Main.m3"
 /* and */
#line 60 "../Main.m3"
 /* if_true_or_false */
#line 60 "../Main.m3"
 /* load_host_integer */
#line 60 "../Main.m3"
 /* load_integer */
#line 60 "../Main.m3"
 /* if_compare */
#line 60 "../Main.m3"
if(m3_ne(INT64,
  ((INT64)(((INT64)((INT64)Main_m_64_L_65))&  INT64_(1))),
   INT64_(0)))goto L13;
#line 60 "../Main.m3"
 /* load */
#line 60 "../Main.m3"
 /* load_indirect */
#line 60 "../Main.m3"
 /* extract_mn */
#line 60 "../Main.m3"
 /* load_host_integer */
#line 60 "../Main.m3"
 /* load_integer */
#line 60 "../Main.m3"
 /* load_host_integer */
#line 60 "../Main.m3"
 /* load_integer */
#line 60 "../Main.m3"
 /* extract */
#line 60 "../Main.m3"
 /* if_true_or_false */
#line 60 "../Main.m3"
 /* load_host_integer */
#line 60 "../Main.m3"
 /* load_integer */
#line 60 "../Main.m3"
 /* if_compare */
#line 60 "../Main.m3"
if(m3_eq(INT64,
 ((INT64)(m3_extract(
 UINT64,
  *((INT64*)(INT64_(-8)+((ADDRESS)(Main_m_64_L_65)))),
   UINT64_(22),
   UINT64_(1)))),
   INT64_(0)))goto L13;
#line 60 "../Main.m3"
 /* start_call_direct */
#line 60 "../Main.m3"
 /* load */
#line 60 "../Main.m3"
 /* pop_param */
#line 60 "../Main.m3"
 /* invoke_direct */
#line 60 "../Main.m3"
 /* call_direct */
#line 60 "../Main.m3"
RTHooks__CheckLoadTracedRef(
  ( REFANY )(((ADDRESS)(Main_m_64_L_65)) ));
#line 60 "../Main.m3"
 /* set_label */
#line 60 "../Main.m3"
 /* set_label */
#line 60 "../Main.m3"
L13:;
#line 60 "../Main.m3"
 /* start_call_direct */
#line 60 "../Main.m3"
 /* load */
#line 60 "../Main.m3"
 /* pop_param */
#line 60 "../Main.m3"
 /* invoke_direct */
#line 60 "../Main.m3"
 /* call_direct */
#line 60 "../Main.m3"
Thread__Acquire(
  ( Thread__Mutex )(((ADDRESS)(Main_m_64_L_65)) ));
#line 60 "../Main.m3"
 /* set_label */
#line 60 "../Main.m3"
 /* load_nil */
#line 60 "../Main.m3"
 /* load */
#line 60 "../Main.m3"
 /* if_compare */
#line 60 "../Main.m3"
if(m3_eq(ADDRESS,
 ((ADDRESS)(0)),
 ((ADDRESS)(Main_m_70_L_71))))goto L11;
#line 60 "../Main.m3"
 /* start_call_direct */
#line 60 "../Main.m3"
 /* load */
#line 60 "../Main.m3"
 /* pop_param */
#line 60 "../Main.m3"
 /* invoke_direct */
#line 60 "../Main.m3"
 /* call_direct */
#line 60 "../Main.m3"
RTHooks__ResumeRaise(
  ( ADDRESS )(((ADDRESS)(Main_m_70_L_71)) ));
#line 60 "../Main.m3"
 /* set_label */
#line 60 "../Main.m3"
 /* set_label */
#line 60 "../Main.m3"
L11:;
#line 60 "../Main.m3"
 /* set_source_line */
#line 60 "../Main.m3"
#line 51 "../Main.m3"
 /* set_label */
#line 51 "../Main.m3"
L7:;
#line 51 "../Main.m3"
 /* load */
#line 51 "../Main.m3"
 /* load */
#line 51 "../Main.m3"
 /* if_compare */
#line 51 "../Main.m3"
if(m3_gt(INT64,
  ((INT64)(*((UINT64*)(INT64_(376)+((ADDRESS)(&Main_m_M_Main_L_42)))))),
  ((INT64)(*((UINT64*)(INT64_(408)+((ADDRESS)(&Main_m_M_Main_L_42))))))))goto L6;
#line 51 "../Main.m3"
 /* set_label */
#line 51 "../Main.m3"
 /* jump */
#line 51 "../Main.m3"
goto L4;
#line 51 "../Main.m3"
 /* end_try */
#line 51 "../Main.m3"
} catch (_M3Exc& _m3exc) { _m3_caught = _m3exc.act; goto L3; }
#line 51 "../Main.m3"
 /* set_label */
#line 51 "../Main.m3"
L3:;
#line 51 "../Main.m3"
 /* landing_pad */
#line 51 "../Main.m3"
 /* store */
#line 51 "../Main.m3"
(*(ADDRESS*)(&Main_m_52_L_53))=(ADDRESS)(((ADDRESS)(_m3_caught)));
#line 51 "../Main.m3"
 /* set_label */
#line 51 "../Main.m3"
L4:;
#line 51 "../Main.m3"
 /* start_call_indirect */
#line 51 "../Main.m3"
 /* load */
#line 51 "../Main.m3"
 /* pop_param */
#line 51 "../Main.m3"
 /* load */
#line 51 "../Main.m3"
 /* load_indirect */
#line 51 "../Main.m3"
 /* load_indirect */
#line 51 "../Main.m3"
 /* check_nil */
#line 51 "../Main.m3"
 /* store */
#line 51 "../Main.m3"
(*(ADDRESS*)(&Main_m_74_L_75))=(ADDRESS)(((ADDRESS)(*((ADDRESS*)(INT64_(8)+((ADDRESS)(*((ADDRESS*)(Main_m_56_L_57)))))))));
#line 51 "../Main.m3"
 /* load */
#line 51 "../Main.m3"
/*check_nil*/if(!Main_m_74_L_75)Main_m_M_Main_L_42_CRASH(1636);
#line 51 "../Main.m3"
 /* call_indirect */
#line 51 "../Main.m3"
((void (__cdecl*)(void*))Main_m_74_L_75)(
 ((ADDRESS)(Main_m_56_L_57)));
#line 51 "../Main.m3"
 /* load_nil */
#line 51 "../Main.m3"
 /* load */
#line 51 "../Main.m3"
 /* if_compare */
#line 51 "../Main.m3"
if(m3_eq(ADDRESS,
 ((ADDRESS)(0)),
 ((ADDRESS)(Main_m_52_L_53))))goto L5;
#line 51 "../Main.m3"
 /* start_call_direct */
#line 51 "../Main.m3"
 /* load */
#line 51 "../Main.m3"
 /* pop_param */
#line 51 "../Main.m3"
 /* call_direct */
#line 51 "../Main.m3"
RTHooks__ResumeRaise(
  ( ADDRESS )(((ADDRESS)(Main_m_52_L_53)) ));
#line 51 "../Main.m3"
 /* set_source_line */
#line 51 "../Main.m3"
#line 63 "../Main.m3"
 /* set_label */
#line 63 "../Main.m3"
L5:;
#line 63 "../Main.m3"
 /* set_source_line */
#line 63 "../Main.m3"
#line 64 "../Main.m3"
 /* load_nil */
#line 64 "../Main.m3"
 /* exit_proc */
#line 64 "../Main.m3"
return (REFANY)(0);
#line 64 "../Main.m3"
 /* end_procedure */
#line 64 "../Main.m3"
} /* CheckResults */
#line 64 "../Main.m3"
 /* set_source_line */
#line 64 "../Main.m3"
#line 69 "../Main.m3"
 /* begin_procedure */
#line 69 "../Main.m3"
struct Main__CheckResults_Frame_t {
#line 69 "../Main.m3"
ADDRESS _unused;
#line 69 "../Main.m3"
};
#line 69 "../Main.m3"
void /*TypeText3*/ 
__cdecl
Main__CheckResults(void)
{
#line 69 "../Main.m3"
 /* Var_Type1 */ T46D79418* count_L_48={0};//always-init
#line 69 "../Main.m3"
 /* Var_Type1 */ BOOLEAN error_L_49={0};//always-init
#line 69 "../Main.m3"
 /* Var_Type3 */ STRUCT(24) Main_m_76_L_77={0};//always-init
#line 69 "../Main.m3"
 /* Var_Type2 */ ADDRESS Main_m_78_L_79={0};//always-init
#line 69 "../Main.m3"
 /* Var_Type2 */ INT64 Main_m_80_L_81={0};//always-init
#line 69 "../Main.m3"
 /* Var_Type1 */ INTEGER i_L_82={0};//always-init
#line 69 "../Main.m3"
 /* Var_Type1 */ INTEGER Main_m_83_L_84={0};//always-init
#line 69 "../Main.m3"
 /* Var_Type2 */ INT64 Main_m_85_L_86={0};//always-init
#line 69 "../Main.m3"
 /* Var_Type1 */ INTEGER i_L_87={0};//always-init
#line 69 "../Main.m3"
 /* Var_Type1 */ INTEGER Main_m_88_L_89={0};//always-init
#line 69 "../Main.m3"
 /* Var_Type2 */ ADDRESS Main_m_90_L_91={0};//always-init
#line 69 "../Main.m3"
 /* Var_Type2 */ INT64 Main_m_92_L_93={0};//always-init
#line 69 "../Main.m3"
 /* Var_Type2 */ ADDRESS Main_m_94_L_95={0};//always-init
#line 69 "../Main.m3"
 /* Var_Type1 */ CARDINAL* c_L_96={0};//always-init
#line 69 "../Main.m3"
 /* Var_Type2 */ ADDRESS Main_m_97_L_98={0};//always-init
#line 69 "../Main.m3"
 /* Var_Type2 */ ADDRESS Main_m_99_L_100={0};//always-init
#line 69 "../Main.m3"
 /* Var_Type2 */ ADDRESS Main_m_101_L_102={0};//always-init
#line 69 "../Main.m3"
 /* Var_Type2 */ INT64 Main_m_103_L_104={0};//always-init
#line 69 "../Main.m3"
 /* Var_Type2 */ INT64 Main_m_105_L_106={0};//always-init
#line 69 "../Main.m3"
 /* Var_Type2 */ ADDRESS Main_m_107_L_108={0};//always-init
#line 69 "../Main.m3"
 /* Var_Type2 */ ADDRESS Main_m_109_L_110={0};//always-init
#line 69 "../Main.m3"
 /* Var_Type2 */ ADDRESS Main_m_111_L_112={0};//always-init
#line 69 "../Main.m3"
 /* Var_Type1 */ INTEGER n_L_113={0};//always-init
#line 69 "../Main.m3"
 /* Var_Type1 */ INTEGER Main_m_114_L_115={0};//always-init
#line 69 "../Main.m3"
 /* Var_Type2 */ INT64 Main_m_116_L_117={0};//always-init
#line 69 "../Main.m3"
 /* Var_Type1 */ CARDINAL* total_L_118={0};//always-init
#line 69 "../Main.m3"
 /* Var_Type2 */ ADDRESS Main_m_119_L_120={0};//always-init
#line 69 "../Main.m3"
 /* Var_Type2 */ ADDRESS Main_m_121_L_122={0};//always-init
#line 69 "../Main.m3"
 /* Var_Type2 */ ADDRESS Main_m_123_L_124={0};//always-init
#line 69 "../Main.m3"
Main__CheckResults_Frame_t _frame;
#line 69 "../Main.m3"
_frame._unused=(ADDRESS)&_frame;
#line 69 "../Main.m3"
 /* set_source_line */
#line 69 "../Main.m3"
#line 73 "../Main.m3"
 /* load_nil */
#line 73 "../Main.m3"
 /* store */
#line 73 "../Main.m3"
(*(ADDRESS*)(&count_L_48))=(ADDRESS)(((ADDRESS)(0)));
#line 73 "../Main.m3"
 /* set_source_line */
#line 73 "../Main.m3"
#line 74 "../Main.m3"
 /* load_integer */
#line 74 "../Main.m3"
 /* store */
#line 74 "../Main.m3"
(*(UINT8*)(&error_L_49))=(INT64)(  INT64_(0));
#line 74 "../Main.m3"
 /* set_source_line */
#line 74 "../Main.m3"
#line 72 "../Main.m3"
 /* set_source_line */
#line 72 "../Main.m3"
#line 76 "../Main.m3"
 /* load_address */
#line 76 "../Main.m3"
 /* store */
#line 76 "../Main.m3"
(*(ADDRESS*)(&Main_m_76_L_77))=(ADDRESS)(((ADDRESS)(INT64_(16)+((ADDRESS)(&Main_m_76_L_77)))));
#line 76 "../Main.m3"
 /* load_integer */
#line 76 "../Main.m3"
 /* store */
#line 76 "../Main.m3"
(*(INT64*)((8)+(char*)(&Main_m_76_L_77)))=(INT64)(  INT64_(1));
#line 76 "../Main.m3"
 /* load_integer */
#line 76 "../Main.m3"
 /* load */
#line 76 "../Main.m3"
 /* add */
#line 76 "../Main.m3"
 /* store */
#line 76 "../Main.m3"
(*(INT64*)((16)+(char*)(&Main_m_76_L_77)))=(INT64)( ((INT64)(  INT64_(1)+ ((INT64)(*((UINT64*)(INT64_(376)+((ADDRESS)(&Main_m_M_Main_L_42)))))))));
#line 76 "../Main.m3"
 /* start_call_direct */
#line 76 "../Main.m3"
 /* load */
#line 76 "../Main.m3"
 /* pop_param */
#line 76 "../Main.m3"
 /* load_address */
#line 76 "../Main.m3"
 /* pop_param */
#line 76 "../Main.m3"
 /* call_direct */
#line 76 "../Main.m3"
 /* store */
#line 76 "../Main.m3"
(*(ADDRESS*)(&Main_m_78_L_79))=(ADDRESS)(((ADDRESS)(RTHooks__AllocateOpenArray(
  ( ADDRESS )(((ADDRESS)(*((ADDRESS*)(INT64_(672)+((ADDRESS)(&Main_m_M_Main_L_42)))))) ),
  ( RTHooks__ArrayShape* /*TypeText1*/  )(((ADDRESS)(&Main_m_76_L_77)) )))));
#line 76 "../Main.m3"
 /* load */
#line 76 "../Main.m3"
 /* store */
#line 76 "../Main.m3"
(*(ADDRESS*)(&count_L_48))=(ADDRESS)(((ADDRESS)(Main_m_78_L_79)));
#line 76 "../Main.m3"
 /* set_source_line */
#line 76 "../Main.m3"
#line 77 "../Main.m3"
 /* load */
#line 77 "../Main.m3"
 /* store */
#line 77 "../Main.m3"
(*(INT64*)(&Main_m_80_L_81))=(INT64)( ((INT64)(*((UINT64*)(INT64_(376)+((ADDRESS)(&Main_m_M_Main_L_42)))))));
#line 77 "../Main.m3"
 /* begin_block */
#line 77 "../Main.m3"
 /* load_integer */
#line 77 "../Main.m3"
 /* store */
#line 77 "../Main.m3"
(*(INT64*)(&i_L_82))=(INT64)(  INT64_(0));
#line 77 "../Main.m3"
 /* load */
#line 77 "../Main.m3"
 /* store */
#line 77 "../Main.m3"
(*(INT64*)(&Main_m_83_L_84))=(INT64)( Main_m_80_L_81);
#line 77 "../Main.m3"
 /* jump */
#line 77 "../Main.m3"
goto L18;
#line 77 "../Main.m3"
 /* set_label */
#line 77 "../Main.m3"
L17:;
#line 77 "../Main.m3"
 /* set_source_line */
#line 77 "../Main.m3"
#line 79 "../Main.m3"
 /* load */
#line 79 "../Main.m3"
 /* store */
#line 79 "../Main.m3"
(*(ADDRESS*)(&Main_m_78_L_79))=(ADDRESS)(((ADDRESS)(count_L_48)));
#line 79 "../Main.m3"
 /* load */
#line 79 "../Main.m3"
 /* load_indirect */
#line 79 "../Main.m3"
 /* load */
#line 79 "../Main.m3"
 /* load_indirect */
#line 79 "../Main.m3"
 /* load */
#line 79 "../Main.m3"
 /* swap */
#line 79 "../Main.m3"
 /* check_index */
#line 79 "../Main.m3"
 /* swap */
#line 79 "../Main.m3"
 /* store */
#line 79 "../Main.m3"
(*(INT64*)(&Main_m_85_L_86))=(INT64)( i_L_82);
#line 79 "../Main.m3"
 /* load */
#line 79 "../Main.m3"
 /* swap */
#line 79 "../Main.m3"
/*check_index*/if(((UINT64)(*((INT64*)(INT64_(8)+((ADDRESS)(Main_m_78_L_79))))))<=((UINT64)(Main_m_85_L_86)))Main_m_M_Main_L_42_CRASH(2530);
#line 79 "../Main.m3"
 /* index_address */
#line 79 "../Main.m3"
 /* load_integer */
#line 79 "../Main.m3"
 /* store_indirect */
#line 79 "../Main.m3"
(*(UINT64*)((((ADDRESS)(*((ADDRESS*)(Main_m_78_L_79))))+(8*( Main_m_85_L_86)))))=(INT64)(  INT64_(0));
#line 79 "../Main.m3"
 /* set_source_line */
#line 79 "../Main.m3"
#line 77 "../Main.m3"
 /* load_integer */
#line 77 "../Main.m3"
 /* load */
#line 77 "../Main.m3"
 /* add */
#line 77 "../Main.m3"
 /* store */
#line 77 "../Main.m3"
(*(INT64*)(&i_L_82))=(INT64)( ((INT64)(  INT64_(1)+ i_L_82)));
#line 77 "../Main.m3"
 /* set_label */
#line 77 "../Main.m3"
L18:;
#line 77 "../Main.m3"
 /* load */
#line 77 "../Main.m3"
 /* load */
#line 77 "../Main.m3"
 /* if_compare */
#line 77 "../Main.m3"
if(m3_ge(INT64,
  Main_m_83_L_84,
  i_L_82))goto L17;
#line 77 "../Main.m3"
 /* set_label */
#line 77 "../Main.m3"
 /* end_block */
#line 77 "../Main.m3"
 /* set_source_line */
#line 77 "../Main.m3"
#line 81 "../Main.m3"
 /* load */
#line 81 "../Main.m3"
 /* load_integer */
#line 81 "../Main.m3"
 /* subtract */
#line 81 "../Main.m3"
 /* store */
#line 81 "../Main.m3"
(*(INT64*)(&Main_m_80_L_81))=(INT64)( ((INT64)( ((INT64)(*((UINT64*)(INT64_(376)+((ADDRESS)(&Main_m_M_Main_L_42))))))-  INT64_(1))));
#line 81 "../Main.m3"
 /* begin_block */
#line 81 "../Main.m3"
 /* load_integer */
#line 81 "../Main.m3"
 /* store */
#line 81 "../Main.m3"
(*(INT64*)(&i_L_87))=(INT64)(  INT64_(0));
#line 81 "../Main.m3"
 /* load */
#line 81 "../Main.m3"
 /* store */
#line 81 "../Main.m3"
(*(INT64*)(&Main_m_88_L_89))=(INT64)( Main_m_80_L_81);
#line 81 "../Main.m3"
 /* jump */
#line 81 "../Main.m3"
goto L1B;
#line 81 "../Main.m3"
 /* set_label */
#line 81 "../Main.m3"
L1A:;
#line 81 "../Main.m3"
 /* set_source_line */
#line 81 "../Main.m3"
#line 83 "../Main.m3"
 /* load */
#line 83 "../Main.m3"
 /* store */
#line 83 "../Main.m3"
(*(ADDRESS*)(&Main_m_78_L_79))=(ADDRESS)(((ADDRESS)(*((ADDRESS*)(INT64_(400)+((ADDRESS)(&Main_m_M_Main_L_42)))))));
#line 83 "../Main.m3"
 /* load_nil */
#line 83 "../Main.m3"
 /* load */
#line 83 "../Main.m3"
 /* if_compare */
#line 83 "../Main.m3"
if(m3_eq(ADDRESS,
 ((ADDRESS)(0)),
 ((ADDRESS)(Main_m_78_L_79))))goto L1D;
#line 83 "../Main.m3"
 /* load */
#line 83 "../Main.m3"
 /* loophole */
#line 83 "../Main.m3"
 /* load_integer */
#line 83 "../Main.m3"
 /* and */
#line 83 "../Main.m3"
 /* if_true_or_false */
#line 83 "../Main.m3"
 /* load_host_integer */
#line 83 "../Main.m3"
 /* load_integer */
#line 83 "../Main.m3"
 /* if_compare */
#line 83 "../Main.m3"
if(m3_ne(INT64,
  ((INT64)(((INT64)((INT64)Main_m_78_L_79))&  INT64_(1))),
   INT64_(0)))goto L1D;
#line 83 "../Main.m3"
 /* load */
#line 83 "../Main.m3"
 /* load_indirect */
#line 83 "../Main.m3"
 /* extract_mn */
#line 83 "../Main.m3"
 /* load_host_integer */
#line 83 "../Main.m3"
 /* load_integer */
#line 83 "../Main.m3"
 /* load_host_integer */
#line 83 "../Main.m3"
 /* load_integer */
#line 83 "../Main.m3"
 /* extract */
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
 ((INT64)(m3_extract(
 UINT64,
  *((INT64*)(INT64_(-8)+((ADDRESS)(Main_m_78_L_79)))),
   UINT64_(22),
   UINT64_(1)))),
   INT64_(0)))goto L1D;
#line 83 "../Main.m3"
 /* start_call_direct */
#line 83 "../Main.m3"
 /* load */
#line 83 "../Main.m3"
 /* pop_param */
#line 83 "../Main.m3"
 /* call_direct */
#line 83 "../Main.m3"
RTHooks__CheckLoadTracedRef(
  ( REFANY )(((ADDRESS)(Main_m_78_L_79)) ));
#line 83 "../Main.m3"
 /* set_label */
#line 83 "../Main.m3"
L1D:;
#line 83 "../Main.m3"
 /* load */
#line 83 "../Main.m3"
 /* store */
#line 83 "../Main.m3"
(*(ADDRESS*)(&Main_m_90_L_91))=(ADDRESS)(((ADDRESS)(Main_m_78_L_79)));
#line 83 "../Main.m3"
 /* load */
#line 83 "../Main.m3"
 /* load_indirect */
#line 83 "../Main.m3"
 /* load */
#line 83 "../Main.m3"
 /* load_indirect */
#line 83 "../Main.m3"
 /* load */
#line 83 "../Main.m3"
 /* swap */
#line 83 "../Main.m3"
 /* check_index */
#line 83 "../Main.m3"
 /* swap */
#line 83 "../Main.m3"
 /* store */
#line 83 "../Main.m3"
(*(INT64*)(&Main_m_92_L_93))=(INT64)( i_L_87);
#line 83 "../Main.m3"
 /* load */
#line 83 "../Main.m3"
 /* swap */
#line 83 "../Main.m3"
/*check_index*/if(((UINT64)(*((INT64*)(INT64_(8)+((ADDRESS)(Main_m_90_L_91))))))<=((UINT64)(Main_m_92_L_93)))Main_m_M_Main_L_42_CRASH(2658);
#line 83 "../Main.m3"
 /* index_address */
#line 83 "../Main.m3"
 /* store */
#line 83 "../Main.m3"
(*(ADDRESS*)(&Main_m_94_L_95))=(ADDRESS)(((ADDRESS)((((ADDRESS)(*((ADDRESS*)(Main_m_90_L_91))))+(8*( Main_m_92_L_93))))));
#line 83 "../Main.m3"
 /* begin_block */
#line 83 "../Main.m3"
 /* load */
#line 83 "../Main.m3"
 /* store */
#line 83 "../Main.m3"
(*(ADDRESS*)(&c_L_96))=(ADDRESS)(((ADDRESS)(Main_m_94_L_95)));
#line 83 "../Main.m3"
 /* set_source_line */
#line 83 "../Main.m3"
#line 85 "../Main.m3"
 /* load */
#line 85 "../Main.m3"
 /* load_indirect */
#line 85 "../Main.m3"
 /* load */
#line 85 "../Main.m3"
 /* if_compare */
#line 85 "../Main.m3"
if(m3_le(INT64,
  ((INT64)(*((UINT64*)(c_L_96)))),
  ((INT64)(*((UINT64*)(INT64_(376)+((ADDRESS)(&Main_m_M_Main_L_42))))))))goto L1F;
#line 85 "../Main.m3"
 /* set_source_line */
#line 85 "../Main.m3"
#line 87 "../Main.m3"
 /* load_integer */
#line 87 "../Main.m3"
 /* store */
#line 87 "../Main.m3"
(*(UINT8*)(&error_L_49))=(INT64)(  INT64_(1));
#line 87 "../Main.m3"
 /* set_source_line */
#line 87 "../Main.m3"
#line 88 "../Main.m3"
 /* start_call_direct */
#line 88 "../Main.m3"
 /* load */
#line 88 "../Main.m3"
 /* pop_param */
#line 88 "../Main.m3"
 /* load_integer */
#line 88 "../Main.m3"
 /* pop_param */
#line 88 "../Main.m3"
 /* call_direct */
#line 88 "../Main.m3"
 /* store */
#line 88 "../Main.m3"
(*(ADDRESS*)(&Main_m_94_L_95))=(ADDRESS)(((ADDRESS)(Fmt__Int(
  ( INTEGER )( i_L_87 ),
  ( Fmt__Base )(((UINT8)( INT64_(10))) )))));
#line 88 "../Main.m3"
 /* start_call_direct */
#line 88 "../Main.m3"
 /* load_address */
#line 88 "../Main.m3"
 /* pop_param */
#line 88 "../Main.m3"
 /* load */
#line 88 "../Main.m3"
 /* pop_param */
#line 88 "../Main.m3"
 /* call_direct */
#line 88 "../Main.m3"
 /* store */
#line 88 "../Main.m3"
(*(ADDRESS*)(&Main_m_90_L_91))=(ADDRESS)(((ADDRESS)(RTHooks__Concat(
  ( TEXT )(((ADDRESS)(INT64_(80)+((ADDRESS)(&Main_m_40_L_41)))) ),
  ( TEXT )(((ADDRESS)(Main_m_94_L_95)) )))));
#line 88 "../Main.m3"
 /* start_call_direct */
#line 88 "../Main.m3"
 /* load */
#line 88 "../Main.m3"
 /* pop_param */
#line 88 "../Main.m3"
 /* load_address */
#line 88 "../Main.m3"
 /* pop_param */
#line 88 "../Main.m3"
 /* call_direct */
#line 88 "../Main.m3"
 /* store */
#line 88 "../Main.m3"
(*(ADDRESS*)(&Main_m_78_L_79))=(ADDRESS)(((ADDRESS)(RTHooks__Concat(
  ( TEXT )(((ADDRESS)(Main_m_90_L_91)) ),
  ( TEXT )(((ADDRESS)(INT64_(160)+((ADDRESS)(&Main_m_40_L_41)))) )))));
#line 88 "../Main.m3"
 /* start_call_direct */
#line 88 "../Main.m3"
 /* load */
#line 88 "../Main.m3"
 /* load_indirect */
#line 88 "../Main.m3"
 /* pop_param */
#line 88 "../Main.m3"
 /* load_integer */
#line 88 "../Main.m3"
 /* pop_param */
#line 88 "../Main.m3"
 /* call_direct */
#line 88 "../Main.m3"
 /* store */
#line 88 "../Main.m3"
(*(ADDRESS*)(&Main_m_97_L_98))=(ADDRESS)(((ADDRESS)(Fmt__Int(
  ( INTEGER )( ((INT64)(*((UINT64*)(c_L_96)))) ),
  ( Fmt__Base )(((UINT8)( INT64_(10))) )))));
#line 88 "../Main.m3"
 /* start_call_direct */
#line 88 "../Main.m3"
 /* load */
#line 88 "../Main.m3"
 /* pop_param */
#line 88 "../Main.m3"
 /* load */
#line 88 "../Main.m3"
 /* pop_param */
#line 88 "../Main.m3"
 /* call_direct */
#line 88 "../Main.m3"
 /* store */
#line 88 "../Main.m3"
(*(ADDRESS*)(&Main_m_99_L_100))=(ADDRESS)(((ADDRESS)(RTHooks__Concat(
  ( TEXT )(((ADDRESS)(Main_m_78_L_79)) ),
  ( TEXT )(((ADDRESS)(Main_m_97_L_98)) )))));
#line 88 "../Main.m3"
 /* start_call_direct */
#line 88 "../Main.m3"
 /* load */
#line 88 "../Main.m3"
 /* pop_param */
#line 88 "../Main.m3"
 /* load_address */
#line 88 "../Main.m3"
 /* pop_param */
#line 88 "../Main.m3"
 /* call_direct */
#line 88 "../Main.m3"
 /* store */
#line 88 "../Main.m3"
(*(ADDRESS*)(&Main_m_101_L_102))=(ADDRESS)(((ADDRESS)(RTHooks__Concat(
  ( TEXT )(((ADDRESS)(Main_m_99_L_100)) ),
  ( TEXT )(((ADDRESS)(INT64_(192)+((ADDRESS)(&Main_m_40_L_41)))) )))));
#line 88 "../Main.m3"
 /* start_call_direct */
#line 88 "../Main.m3"
 /* load */
#line 88 "../Main.m3"
 /* pop_param */
#line 88 "../Main.m3"
 /* call_direct */
#line 88 "../Main.m3"
Main__Print(
  ( TEXT )(((ADDRESS)(Main_m_101_L_102)) ));
#line 88 "../Main.m3"
 /* jump */
#line 88 "../Main.m3"
goto L1E;
#line 88 "../Main.m3"
 /* set_label */
#line 88 "../Main.m3"
L1F:;
#line 88 "../Main.m3"
 /* set_source_line */
#line 88 "../Main.m3"
#line 90 "../Main.m3"
 /* load */
#line 90 "../Main.m3"
 /* store */
#line 90 "../Main.m3"
(*(ADDRESS*)(&Main_m_101_L_102))=(ADDRESS)(((ADDRESS)(count_L_48)));
#line 90 "../Main.m3"
 /* load */
#line 90 "../Main.m3"
 /* load_indirect */
#line 90 "../Main.m3"
 /* load */
#line 90 "../Main.m3"
 /* load_indirect */
#line 90 "../Main.m3"
 /* load */
#line 90 "../Main.m3"
 /* load_indirect */
#line 90 "../Main.m3"
 /* check_index */
#line 90 "../Main.m3"
 /* swap */
#line 90 "../Main.m3"
 /* store */
#line 90 "../Main.m3"
(*(INT64*)(&Main_m_103_L_104))=(INT64)( ((INT64)(*((UINT64*)(c_L_96)))));
#line 90 "../Main.m3"
 /* load */
#line 90 "../Main.m3"
 /* swap */
#line 90 "../Main.m3"
/*check_index*/if(((UINT64)(*((INT64*)(INT64_(8)+((ADDRESS)(Main_m_101_L_102))))))<=((UINT64)(Main_m_103_L_104)))Main_m_M_Main_L_42_CRASH(2882);
#line 90 "../Main.m3"
 /* index_address */
#line 90 "../Main.m3"
 /* store */
#line 90 "../Main.m3"
(*(ADDRESS*)(&Main_m_99_L_100))=(ADDRESS)(((ADDRESS)((((ADDRESS)(*((ADDRESS*)(Main_m_101_L_102))))+(8*( Main_m_103_L_104))))));
#line 90 "../Main.m3"
 /* load */
#line 90 "../Main.m3"
 /* load_indirect */
#line 90 "../Main.m3"
 /* load_integer */
#line 90 "../Main.m3"
 /* add */
#line 90 "../Main.m3"
 /* check_lo */
#line 90 "../Main.m3"
 /* store */
#line 90 "../Main.m3"
(*(INT64*)(&Main_m_105_L_106))=(INT64)( ((INT64)( ((INT64)(*((UINT64*)(Main_m_99_L_100))))+  INT64_(1))));
#line 90 "../Main.m3"
 /* load */
#line 90 "../Main.m3"
/*check_lo*/if(Main_m_105_L_106<INT64_(0))Main_m_M_Main_L_42_CRASH(2881);
#line 90 "../Main.m3"
 /* load */
#line 90 "../Main.m3"
 /* swap */
#line 90 "../Main.m3"
 /* store_indirect */
#line 90 "../Main.m3"
(*(UINT64*)(Main_m_99_L_100))=(INT64)( Main_m_105_L_106);
#line 90 "../Main.m3"
 /* set_label */
#line 90 "../Main.m3"
L1E:;
#line 90 "../Main.m3"
 /* end_block */
#line 90 "../Main.m3"
 /* set_source_line */
#line 90 "../Main.m3"
#line 81 "../Main.m3"
 /* load_integer */
#line 81 "../Main.m3"
 /* load */
#line 81 "../Main.m3"
 /* add */
#line 81 "../Main.m3"
 /* store */
#line 81 "../Main.m3"
(*(INT64*)(&i_L_87))=(INT64)( ((INT64)(  INT64_(1)+ i_L_87)));
#line 81 "../Main.m3"
 /* set_label */
#line 81 "../Main.m3"
L1B:;
#line 81 "../Main.m3"
 /* load */
#line 81 "../Main.m3"
 /* load */
#line 81 "../Main.m3"
 /* if_compare */
#line 81 "../Main.m3"
if(m3_ge(INT64,
  Main_m_88_L_89,
  i_L_87))goto L1A;
#line 81 "../Main.m3"
 /* set_label */
#line 81 "../Main.m3"
 /* end_block */
#line 81 "../Main.m3"
 /* set_source_line */
#line 81 "../Main.m3"
#line 94 "../Main.m3"
 /* start_call_direct */
#line 94 "../Main.m3"
 /* load */
#line 94 "../Main.m3"
 /* pop_param */
#line 94 "../Main.m3"
 /* load_integer */
#line 94 "../Main.m3"
 /* pop_param */
#line 94 "../Main.m3"
 /* call_direct */
#line 94 "../Main.m3"
 /* store */
#line 94 "../Main.m3"
(*(ADDRESS*)(&Main_m_107_L_108))=(ADDRESS)(((ADDRESS)(Fmt__Int(
  ( INTEGER )( ((INT64)(*((UINT64*)(INT64_(376)+((ADDRESS)(&Main_m_M_Main_L_42)))))) ),
  ( Fmt__Base )(((UINT8)( INT64_(10))) )))));
#line 94 "../Main.m3"
 /* start_call_direct */
#line 94 "../Main.m3"
 /* load_address */
#line 94 "../Main.m3"
 /* pop_param */
#line 94 "../Main.m3"
 /* load */
#line 94 "../Main.m3"
 /* pop_param */
#line 94 "../Main.m3"
 /* call_direct */
#line 94 "../Main.m3"
 /* store */
#line 94 "../Main.m3"
(*(ADDRESS*)(&Main_m_109_L_110))=(ADDRESS)(((ADDRESS)(RTHooks__Concat(
  ( TEXT )(((ADDRESS)(INT64_(256)+((ADDRESS)(&Main_m_40_L_41)))) ),
  ( TEXT )(((ADDRESS)(Main_m_107_L_108)) )))));
#line 94 "../Main.m3"
 /* start_call_direct */
#line 94 "../Main.m3"
 /* load */
#line 94 "../Main.m3"
 /* pop_param */
#line 94 "../Main.m3"
 /* load_address */
#line 94 "../Main.m3"
 /* pop_param */
#line 94 "../Main.m3"
 /* call_direct */
#line 94 "../Main.m3"
 /* store */
#line 94 "../Main.m3"
(*(ADDRESS*)(&Main_m_111_L_112))=(ADDRESS)(((ADDRESS)(RTHooks__Concat(
  ( TEXT )(((ADDRESS)(Main_m_109_L_110)) ),
  ( TEXT )(((ADDRESS)(INT64_(320)+((ADDRESS)(&Main_m_40_L_41)))) )))));
#line 94 "../Main.m3"
 /* start_call_direct */
#line 94 "../Main.m3"
 /* load */
#line 94 "../Main.m3"
 /* pop_param */
#line 94 "../Main.m3"
 /* call_direct */
#line 94 "../Main.m3"
Main__Print(
  ( TEXT )(((ADDRESS)(Main_m_111_L_112)) ));
#line 94 "../Main.m3"
 /* set_source_line */
#line 94 "../Main.m3"
#line 95 "../Main.m3"
 /* load */
#line 95 "../Main.m3"
 /* store */
#line 95 "../Main.m3"
(*(INT64*)(&Main_m_80_L_81))=(INT64)( ((INT64)(*((UINT64*)(INT64_(376)+((ADDRESS)(&Main_m_M_Main_L_42)))))));
#line 95 "../Main.m3"
 /* begin_block */
#line 95 "../Main.m3"
 /* load_integer */
#line 95 "../Main.m3"
 /* store */
#line 95 "../Main.m3"
(*(INT64*)(&n_L_113))=(INT64)(  INT64_(0));
#line 95 "../Main.m3"
 /* load */
#line 95 "../Main.m3"
 /* store */
#line 95 "../Main.m3"
(*(INT64*)(&Main_m_114_L_115))=(INT64)( Main_m_80_L_81);
#line 95 "../Main.m3"
 /* jump */
#line 95 "../Main.m3"
goto L21;
#line 95 "../Main.m3"
 /* set_label */
#line 95 "../Main.m3"
L20:;
#line 95 "../Main.m3"
 /* set_source_line */
#line 95 "../Main.m3"
#line 97 "../Main.m3"
 /* load */
#line 97 "../Main.m3"
 /* store */
#line 97 "../Main.m3"
(*(ADDRESS*)(&Main_m_111_L_112))=(ADDRESS)(((ADDRESS)(count_L_48)));
#line 97 "../Main.m3"
 /* load */
#line 97 "../Main.m3"
 /* load_indirect */
#line 97 "../Main.m3"
 /* load */
#line 97 "../Main.m3"
 /* load_indirect */
#line 97 "../Main.m3"
 /* load */
#line 97 "../Main.m3"
 /* swap */
#line 97 "../Main.m3"
 /* check_index */
#line 97 "../Main.m3"
 /* swap */
#line 97 "../Main.m3"
 /* store */
#line 97 "../Main.m3"
(*(INT64*)(&Main_m_116_L_117))=(INT64)( n_L_113);
#line 97 "../Main.m3"
 /* load */
#line 97 "../Main.m3"
 /* swap */
#line 97 "../Main.m3"
/*check_index*/if(((UINT64)(*((INT64*)(INT64_(8)+((ADDRESS)(Main_m_111_L_112))))))<=((UINT64)(Main_m_116_L_117)))Main_m_M_Main_L_42_CRASH(3106);
#line 97 "../Main.m3"
 /* index_address */
#line 97 "../Main.m3"
 /* store */
#line 97 "../Main.m3"
(*(ADDRESS*)(&Main_m_109_L_110))=(ADDRESS)(((ADDRESS)((((ADDRESS)(*((ADDRESS*)(Main_m_111_L_112))))+(8*( Main_m_116_L_117))))));
#line 97 "../Main.m3"
 /* begin_block */
#line 97 "../Main.m3"
 /* load */
#line 97 "../Main.m3"
 /* store */
#line 97 "../Main.m3"
(*(ADDRESS*)(&total_L_118))=(ADDRESS)(((ADDRESS)(Main_m_109_L_110)));
#line 97 "../Main.m3"
 /* set_source_line */
#line 97 "../Main.m3"
#line 99 "../Main.m3"
 /* load */
#line 99 "../Main.m3"
 /* load_indirect */
#line 99 "../Main.m3"
 /* load_integer */
#line 99 "../Main.m3"
 /* if_compare */
#line 99 "../Main.m3"
if(m3_le(INT64,
  ((INT64)(*((UINT64*)(total_L_118)))),
   INT64_(0)))goto L24;
#line 99 "../Main.m3"
 /* set_source_line */
#line 99 "../Main.m3"
#line 101 "../Main.m3"
 /* start_call_direct */
#line 101 "../Main.m3"
 /* load */
#line 101 "../Main.m3"
 /* pop_param */
#line 101 "../Main.m3"
 /* load_integer */
#line 101 "../Main.m3"
 /* pop_param */
#line 101 "../Main.m3"
 /* call_direct */
#line 101 "../Main.m3"
 /* store */
#line 101 "../Main.m3"
(*(ADDRESS*)(&Main_m_109_L_110))=(ADDRESS)(((ADDRESS)(Fmt__Int(
  ( INTEGER )( n_L_113 ),
  ( Fmt__Base )(((UINT8)( INT64_(10))) )))));
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
 /* call_direct */
#line 101 "../Main.m3"
 /* store */
#line 101 "../Main.m3"
(*(ADDRESS*)(&Main_m_111_L_112))=(ADDRESS)(((ADDRESS)(RTHooks__Concat(
  ( TEXT )(((ADDRESS)(INT64_(384)+((ADDRESS)(&Main_m_40_L_41)))) ),
  ( TEXT )(((ADDRESS)(Main_m_109_L_110)) )))));
#line 101 "../Main.m3"
 /* start_call_direct */
#line 101 "../Main.m3"
 /* load */
#line 101 "../Main.m3"
 /* pop_param */
#line 101 "../Main.m3"
 /* load_address */
#line 101 "../Main.m3"
 /* pop_param */
#line 101 "../Main.m3"
 /* call_direct */
#line 101 "../Main.m3"
 /* store */
#line 101 "../Main.m3"
(*(ADDRESS*)(&Main_m_107_L_108))=(ADDRESS)(((ADDRESS)(RTHooks__Concat(
  ( TEXT )(((ADDRESS)(Main_m_111_L_112)) ),
  ( TEXT )(((ADDRESS)(INT64_(416)+((ADDRESS)(&Main_m_40_L_41)))) )))));
#line 101 "../Main.m3"
 /* start_call_direct */
#line 101 "../Main.m3"
 /* load */
#line 101 "../Main.m3"
 /* load_indirect */
#line 101 "../Main.m3"
 /* pop_param */
#line 101 "../Main.m3"
 /* load_integer */
#line 101 "../Main.m3"
 /* pop_param */
#line 101 "../Main.m3"
 /* call_direct */
#line 101 "../Main.m3"
 /* store */
#line 101 "../Main.m3"
(*(ADDRESS*)(&Main_m_119_L_120))=(ADDRESS)(((ADDRESS)(Fmt__Int(
  ( INTEGER )( ((INT64)(*((UINT64*)(total_L_118)))) ),
  ( Fmt__Base )(((UINT8)( INT64_(10))) )))));
#line 101 "../Main.m3"
 /* start_call_direct */
#line 101 "../Main.m3"
 /* load */
#line 101 "../Main.m3"
 /* pop_param */
#line 101 "../Main.m3"
 /* load */
#line 101 "../Main.m3"
 /* pop_param */
#line 101 "../Main.m3"
 /* call_direct */
#line 101 "../Main.m3"
 /* store */
#line 101 "../Main.m3"
(*(ADDRESS*)(&Main_m_121_L_122))=(ADDRESS)(((ADDRESS)(RTHooks__Concat(
  ( TEXT )(((ADDRESS)(Main_m_107_L_108)) ),
  ( TEXT )(((ADDRESS)(Main_m_119_L_120)) )))));
#line 101 "../Main.m3"
 /* start_call_direct */
#line 101 "../Main.m3"
 /* load */
#line 101 "../Main.m3"
 /* pop_param */
#line 101 "../Main.m3"
 /* load_address */
#line 101 "../Main.m3"
 /* pop_param */
#line 101 "../Main.m3"
 /* call_direct */
#line 101 "../Main.m3"
 /* store */
#line 101 "../Main.m3"
(*(ADDRESS*)(&Main_m_123_L_124))=(ADDRESS)(((ADDRESS)(RTHooks__Concat(
  ( TEXT )(((ADDRESS)(Main_m_121_L_122)) ),
  ( TEXT )(((ADDRESS)(INT64_(448)+((ADDRESS)(&Main_m_40_L_41)))) )))));
#line 101 "../Main.m3"
 /* start_call_direct */
#line 101 "../Main.m3"
 /* load */
#line 101 "../Main.m3"
 /* pop_param */
#line 101 "../Main.m3"
 /* call_direct */
#line 101 "../Main.m3"
Main__Print(
  ( TEXT )(((ADDRESS)(Main_m_123_L_124)) ));
#line 101 "../Main.m3"
 /* set_label */
#line 101 "../Main.m3"
L24:;
#line 101 "../Main.m3"
 /* set_source_line */
#line 101 "../Main.m3"
#line 103 "../Main.m3"
 /* load_integer */
#line 103 "../Main.m3"
 /* load */
#line 103 "../Main.m3"
 /* if_compare */
#line 103 "../Main.m3"
if(m3_eq(INT64,
   INT64_(1),
  n_L_113))goto L26;
#line 103 "../Main.m3"
 /* load */
#line 103 "../Main.m3"
 /* load_indirect */
#line 103 "../Main.m3"
 /* load_integer */
#line 103 "../Main.m3"
 /* if_compare */
#line 103 "../Main.m3"
if(m3_eq(INT64,
  ((INT64)(*((UINT64*)(total_L_118)))),
   INT64_(0)))goto L26;
#line 103 "../Main.m3"
 /* set_source_line */
#line 103 "../Main.m3"
#line 105 "../Main.m3"
 /* load_integer */
#line 105 "../Main.m3"
 /* store */
#line 105 "../Main.m3"
(*(UINT8*)(&error_L_49))=(INT64)(  INT64_(1));
#line 105 "../Main.m3"
 /* jump */
#line 105 "../Main.m3"
goto L25;
#line 105 "../Main.m3"
 /* set_label */
#line 105 "../Main.m3"
L26:;
#line 105 "../Main.m3"
 /* set_source_line */
#line 105 "../Main.m3"
#line 106 "../Main.m3"
 /* load_integer */
#line 106 "../Main.m3"
 /* load */
#line 106 "../Main.m3"
 /* if_compare */
#line 106 "../Main.m3"
if(m3_ne(INT64,
   INT64_(1),
  n_L_113))goto L27;
#line 106 "../Main.m3"
 /* load */
#line 106 "../Main.m3"
 /* load_indirect */
#line 106 "../Main.m3"
 /* load */
#line 106 "../Main.m3"
 /* if_compare */
#line 106 "../Main.m3"
if(m3_eq(INT64,
  ((INT64)(*((UINT64*)(total_L_118)))),
  ((INT64)(*((UINT64*)(INT64_(376)+((ADDRESS)(&Main_m_M_Main_L_42))))))))goto L27;
#line 106 "../Main.m3"
 /* set_source_line */
#line 106 "../Main.m3"
#line 108 "../Main.m3"
 /* load_integer */
#line 108 "../Main.m3"
 /* store */
#line 108 "../Main.m3"
(*(UINT8*)(&error_L_49))=(INT64)(  INT64_(1));
#line 108 "../Main.m3"
 /* set_label */
#line 108 "../Main.m3"
L27:;
#line 108 "../Main.m3"
 /* set_label */
#line 108 "../Main.m3"
L25:;
#line 108 "../Main.m3"
 /* end_block */
#line 108 "../Main.m3"
 /* set_source_line */
#line 108 "../Main.m3"
#line 95 "../Main.m3"
 /* load_integer */
#line 95 "../Main.m3"
 /* load */
#line 95 "../Main.m3"
 /* add */
#line 95 "../Main.m3"
 /* store */
#line 95 "../Main.m3"
(*(INT64*)(&n_L_113))=(INT64)( ((INT64)(  INT64_(1)+ n_L_113)));
#line 95 "../Main.m3"
 /* set_label */
#line 95 "../Main.m3"
L21:;
#line 95 "../Main.m3"
 /* load */
#line 95 "../Main.m3"
 /* load */
#line 95 "../Main.m3"
 /* if_compare */
#line 95 "../Main.m3"
if(m3_ge(INT64,
  Main_m_114_L_115,
  n_L_113))goto L20;
#line 95 "../Main.m3"
 /* set_label */
#line 95 "../Main.m3"
 /* end_block */
#line 95 "../Main.m3"
 /* set_source_line */
#line 95 "../Main.m3"
#line 112 "../Main.m3"
 /* load */
#line 112 "../Main.m3"
 /* if_true_or_false */
#line 112 "../Main.m3"
 /* load_host_integer */
#line 112 "../Main.m3"
 /* load_integer */
#line 112 "../Main.m3"
 /* if_compare */
#line 112 "../Main.m3"
if(m3_eq(INT64,
  ((INT64)(error_L_49)),
   INT64_(0)))goto L29;
#line 112 "../Main.m3"
 /* set_source_line */
#line 112 "../Main.m3"
#line 114 "../Main.m3"
 /* start_call_direct */
#line 114 "../Main.m3"
 /* load_address */
#line 114 "../Main.m3"
 /* pop_param */
#line 114 "../Main.m3"
 /* call_direct */
#line 114 "../Main.m3"
Main__Print(
  ( TEXT )(((ADDRESS)(INT64_(480)+((ADDRESS)(&Main_m_40_L_41)))) ));
#line 114 "../Main.m3"
 /* set_source_line */
#line 114 "../Main.m3"
#line 115 "../Main.m3"
 /* load_integer */
#line 115 "../Main.m3"
 /* store */
#line 115 "../Main.m3"
(*(UINT64*)((368)+(char*)(&Main_m_M_Main_L_42)))=(INT64)(  INT64_(1));
#line 115 "../Main.m3"
 /* set_source_line */
#line 115 "../Main.m3"
#line 116 "../Main.m3"
 /* start_call_direct */
#line 116 "../Main.m3"
 /* load_address */
#line 116 "../Main.m3"
 /* pop_param */
#line 116 "../Main.m3"
 /* call_direct */
#line 116 "../Main.m3"
Main__Print(
  ( TEXT )(((ADDRESS)(INT64_(528)+((ADDRESS)(&Main_m_40_L_41)))) ));
#line 116 "../Main.m3"
 /* jump */
#line 116 "../Main.m3"
goto L28;
#line 116 "../Main.m3"
 /* set_label */
#line 116 "../Main.m3"
L29:;
#line 116 "../Main.m3"
 /* set_source_line */
#line 116 "../Main.m3"
#line 118 "../Main.m3"
 /* start_call_direct */
#line 118 "../Main.m3"
 /* load_address */
#line 118 "../Main.m3"
 /* pop_param */
#line 118 "../Main.m3"
 /* call_direct */
#line 118 "../Main.m3"
Main__Print(
  ( TEXT )(((ADDRESS)(INT64_(624)+((ADDRESS)(&Main_m_40_L_41)))) ));
#line 118 "../Main.m3"
 /* set_label */
#line 118 "../Main.m3"
L28:;
#line 118 "../Main.m3"
 /* set_source_line */
#line 118 "../Main.m3"
#line 120 "../Main.m3"
 /* exit_proc */
#line 120 "../Main.m3"
return;
#line 120 "../Main.m3"
 /* end_procedure */
#line 120 "../Main.m3"
} /* Main_M3 */
#line 120 "../Main.m3"
 /* module main body Main_M3 */
#line 120 "../Main.m3"
 /* set_source_line */
#line 120 "../Main.m3"
#line 124 "../Main.m3"
 /* begin_procedure */
#line 124 "../Main.m3"
struct Main_M3_Frame_t {
#line 124 "../Main.m3"
ADDRESS _unused;
#line 124 "../Main.m3"
};
#line 124 "../Main.m3"
RT0__ModulePtr
__cdecl
Main_M3(
   /* Param_Type1 */ INTEGER /*TypeText1*/  mode_L_43)
{
#line 124 "../Main.m3"
 /* Var_Type2 */ ADDRESS Main_m_125_L_126={0};//always-init
#line 124 "../Main.m3"
 /* Var_Type3 */ STRUCT(24) Main_m_127_L_128={0};//always-init
#line 124 "../Main.m3"
 /* Var_Type2 */ INT64 Main_m_129_L_130={0};//always-init
#line 124 "../Main.m3"
 /* Var_Type1 */ INTEGER i_L_131={0};//always-init
#line 124 "../Main.m3"
 /* Var_Type1 */ INTEGER Main_m_132_L_133={0};//always-init
#line 124 "../Main.m3"
 /* Var_Type2 */ ADDRESS Main_m_134_L_135={0};//always-init
#line 124 "../Main.m3"
 /* Var_Type2 */ INT64 Main_m_136_L_137={0};//always-init
#line 124 "../Main.m3"
 /* Var_Type2 */ ADDRESS Main_m_138_L_139={0};//always-init
#line 124 "../Main.m3"
 /* Var_Type2 */ ADDRESS Main_m_140_L_141={0};//always-init
#line 124 "../Main.m3"
 /* Var_Type2 */ ADDRESS Main_m_142_L_143={0};//always-init
#line 124 "../Main.m3"
 /* Var_Type2 */ ADDRESS Main_m_144_L_145={0};//always-init
#line 124 "../Main.m3"
 /* Var_Type1 */ INTEGER minutes_L_146={0};//always-init
#line 124 "../Main.m3"
 /* Var_Type2 */ ADDRESS Main_m_147_L_148={0};//always-init
#line 124 "../Main.m3"
 /* Var_Type1 */ T16894539* child_L_149={0};//always-init
#line 124 "../Main.m3"
 /* Var_Type1 */ INTEGER i_L_150={0};//always-init
#line 124 "../Main.m3"
 /* Var_Type1 */ INTEGER Main_m_151_L_152={0};//always-init
#line 124 "../Main.m3"
 /* Var_Type2 */ INT64 Main_m_153_L_154={0};//always-init
#line 124 "../Main.m3"
 /* Var_Type1 */ INTEGER i_L_155={0};//always-init
#line 124 "../Main.m3"
 /* Var_Type1 */ INTEGER Main_m_156_L_157={0};//always-init
#line 124 "../Main.m3"
 /* Var_Type2 */ INT64 Main_m_158_L_159={0};//always-init
#line 124 "../Main.m3"
 /* Var_Type2 */ INT64 Main_m_160_L_161={0};//always-init
#line 124 "../Main.m3"
Main_M3_Frame_t _frame;
#line 124 "../Main.m3"
_frame._unused=(ADDRESS)&_frame;
#line 124 "../Main.m3"
 /* load */
#line 124 "../Main.m3"
 /* if_true_or_false */
#line 124 "../Main.m3"
 /* load_host_integer */
#line 124 "../Main.m3"
 /* load_integer */
#line 124 "../Main.m3"
 /* if_compare */
#line 124 "../Main.m3"
if(m3_eq(INT64,
  mode_L_43,
   INT64_(0)))goto L2A;
#line 124 "../Main.m3"
 /* set_source_line */
#line 124 "../Main.m3"
#line 26 "../Main.m3"
 /* load_nil */
#line 26 "../Main.m3"
 /* store */
#line 26 "../Main.m3"
(*(ADDRESS*)((384)+(char*)(&Main_m_M_Main_L_42)))=(ADDRESS)(((ADDRESS)(0)));
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
(*(ADDRESS*)(&Main_m_125_L_126))=(ADDRESS)(((ADDRESS)(RTHooks__AllocateTracedObj(
  ( ADDRESS )(((ADDRESS)(*((ADDRESS*)(INT64_(688)+((ADDRESS)(&Main_m_M_Main_L_42)))))) )))));
#line 26 "../Main.m3"
 /* load */
#line 26 "../Main.m3"
 /* store */
#line 26 "../Main.m3"
(*(ADDRESS*)((384)+(char*)(&Main_m_M_Main_L_42)))=(ADDRESS)(((ADDRESS)(Main_m_125_L_126)));
#line 26 "../Main.m3"
 /* set_source_line */
#line 26 "../Main.m3"
#line 125 "../Main.m3"
 /* start_call_direct */
#line 125 "../Main.m3"
 /* load_address */
#line 125 "../Main.m3"
 /* pop_param */
#line 125 "../Main.m3"
 /* call_direct */
#line 125 "../Main.m3"
Main__Print(
  ( TEXT )(((ADDRESS)(INT64_(656)+((ADDRESS)(&Main_m_40_L_41)))) ));
#line 125 "../Main.m3"
 /* set_source_line */
#line 125 "../Main.m3"
#line 126 "../Main.m3"
 /* start_call_direct */
#line 126 "../Main.m3"
 /* load_address */
#line 126 "../Main.m3"
 /* pop_param */
#line 126 "../Main.m3"
 /* call_direct */
#line 126 "../Main.m3"
Main__Print(
  ( TEXT )(((ADDRESS)(INT64_(760)+((ADDRESS)(&Main_m_40_L_41)))) ));
#line 126 "../Main.m3"
 /* set_source_line */
#line 126 "../Main.m3"
#line 127 "../Main.m3"
 /* start_call_direct */
#line 127 "../Main.m3"
 /* load_address */
#line 127 "../Main.m3"
 /* pop_param */
#line 127 "../Main.m3"
 /* call_direct */
#line 127 "../Main.m3"
Main__Print(
  ( TEXT )(((ADDRESS)(INT64_(864)+((ADDRESS)(&Main_m_40_L_41)))) ));
#line 127 "../Main.m3"
 /* set_source_line */
#line 127 "../Main.m3"
#line 128 "../Main.m3"
 /* start_call_direct */
#line 128 "../Main.m3"
 /* load_address */
#line 128 "../Main.m3"
 /* pop_param */
#line 128 "../Main.m3"
 /* call_direct */
#line 128 "../Main.m3"
Main__Print(
  ( TEXT )(((ADDRESS)(INT64_(912)+((ADDRESS)(&Main_m_40_L_41)))) ));
#line 128 "../Main.m3"
 /* set_source_line */
#line 128 "../Main.m3"
#line 129 "../Main.m3"
 /* start_call_direct */
#line 129 "../Main.m3"
 /* load_address */
#line 129 "../Main.m3"
 /* pop_param */
#line 129 "../Main.m3"
 /* call_direct */
#line 129 "../Main.m3"
Main__Print(
  ( TEXT )(((ADDRESS)(INT64_(1000)+((ADDRESS)(&Main_m_40_L_41)))) ));
#line 129 "../Main.m3"
 /* set_source_line */
#line 129 "../Main.m3"
#line 130 "../Main.m3"
 /* start_call_direct */
#line 130 "../Main.m3"
 /* load_address */
#line 130 "../Main.m3"
 /* pop_param */
#line 130 "../Main.m3"
 /* call_direct */
#line 130 "../Main.m3"
Main__Print(
  ( TEXT )(((ADDRESS)(INT64_(1080)+((ADDRESS)(&Main_m_40_L_41)))) ));
#line 130 "../Main.m3"
 /* set_source_line */
#line 130 "../Main.m3"
#line 131 "../Main.m3"
 /* start_call_direct */
#line 131 "../Main.m3"
 /* load_address */
#line 131 "../Main.m3"
 /* pop_param */
#line 131 "../Main.m3"
 /* call_direct */
#line 131 "../Main.m3"
Main__Print(
  ( TEXT )(((ADDRESS)(INT64_(656)+((ADDRESS)(&Main_m_40_L_41)))) ));
#line 131 "../Main.m3"
 /* set_source_line */
#line 131 "../Main.m3"
#line 133 "../Main.m3"
 /* load_integer */
#line 133 "../Main.m3"
 /* store */
#line 133 "../Main.m3"
(*(UINT64*)((392)+(char*)(&Main_m_M_Main_L_42)))=(INT64)(  INT64_(303));
#line 133 "../Main.m3"
 /* set_source_line */
#line 133 "../Main.m3"
#line 135 "../Main.m3"
 /* load_integer */
#line 135 "../Main.m3"
 /* store */
#line 135 "../Main.m3"
(*(UINT64*)((376)+(char*)(&Main_m_M_Main_L_42)))=(INT64)(  INT64_(573741));
#line 135 "../Main.m3"
 /* set_source_line */
#line 135 "../Main.m3"
#line 137 "../Main.m3"
 /* load_address */
#line 137 "../Main.m3"
 /* store */
#line 137 "../Main.m3"
(*(ADDRESS*)(&Main_m_127_L_128))=(ADDRESS)(((ADDRESS)(INT64_(16)+((ADDRESS)(&Main_m_127_L_128)))));
#line 137 "../Main.m3"
 /* load_integer */
#line 137 "../Main.m3"
 /* store */
#line 137 "../Main.m3"
(*(INT64*)((8)+(char*)(&Main_m_127_L_128)))=(INT64)(  INT64_(1));
#line 137 "../Main.m3"
 /* load */
#line 137 "../Main.m3"
 /* store */
#line 137 "../Main.m3"
(*(INT64*)((16)+(char*)(&Main_m_127_L_128)))=(INT64)( ((INT64)(*((UINT64*)(INT64_(376)+((ADDRESS)(&Main_m_M_Main_L_42)))))));
#line 137 "../Main.m3"
 /* start_call_direct */
#line 137 "../Main.m3"
 /* load */
#line 137 "../Main.m3"
 /* pop_param */
#line 137 "../Main.m3"
 /* load_address */
#line 137 "../Main.m3"
 /* pop_param */
#line 137 "../Main.m3"
 /* call_direct */
#line 137 "../Main.m3"
 /* store */
#line 137 "../Main.m3"
(*(ADDRESS*)(&Main_m_125_L_126))=(ADDRESS)(((ADDRESS)(RTHooks__AllocateOpenArray(
  ( ADDRESS )(((ADDRESS)(*((ADDRESS*)(INT64_(672)+((ADDRESS)(&Main_m_M_Main_L_42)))))) ),
  ( RTHooks__ArrayShape* /*TypeText1*/  )(((ADDRESS)(&Main_m_127_L_128)) )))));
#line 137 "../Main.m3"
 /* load */
#line 137 "../Main.m3"
 /* store */
#line 137 "../Main.m3"
(*(ADDRESS*)((400)+(char*)(&Main_m_M_Main_L_42)))=(ADDRESS)(((ADDRESS)(Main_m_125_L_126)));
#line 137 "../Main.m3"
 /* set_source_line */
#line 137 "../Main.m3"
#line 138 "../Main.m3"
 /* load */
#line 138 "../Main.m3"
 /* load_integer */
#line 138 "../Main.m3"
 /* subtract */
#line 138 "../Main.m3"
 /* store */
#line 138 "../Main.m3"
(*(INT64*)(&Main_m_129_L_130))=(INT64)( ((INT64)( ((INT64)(*((UINT64*)(INT64_(376)+((ADDRESS)(&Main_m_M_Main_L_42))))))-  INT64_(1))));
#line 138 "../Main.m3"
 /* begin_block */
#line 138 "../Main.m3"
 /* load_integer */
#line 138 "../Main.m3"
 /* store */
#line 138 "../Main.m3"
(*(INT64*)(&i_L_131))=(INT64)(  INT64_(0));
#line 138 "../Main.m3"
 /* load */
#line 138 "../Main.m3"
 /* store */
#line 138 "../Main.m3"
(*(INT64*)(&Main_m_132_L_133))=(INT64)( Main_m_129_L_130);
#line 138 "../Main.m3"
 /* jump */
#line 138 "../Main.m3"
goto L2C;
#line 138 "../Main.m3"
 /* set_label */
#line 138 "../Main.m3"
L2B:;
#line 138 "../Main.m3"
 /* set_source_line */
#line 138 "../Main.m3"
#line 140 "../Main.m3"
 /* load */
#line 140 "../Main.m3"
 /* store */
#line 140 "../Main.m3"
(*(ADDRESS*)(&Main_m_125_L_126))=(ADDRESS)(((ADDRESS)(*((ADDRESS*)(INT64_(400)+((ADDRESS)(&Main_m_M_Main_L_42)))))));
#line 140 "../Main.m3"
 /* load_nil */
#line 140 "../Main.m3"
 /* load */
#line 140 "../Main.m3"
 /* if_compare */
#line 140 "../Main.m3"
if(m3_eq(ADDRESS,
 ((ADDRESS)(0)),
 ((ADDRESS)(Main_m_125_L_126))))goto L2E;
#line 140 "../Main.m3"
 /* load */
#line 140 "../Main.m3"
 /* loophole */
#line 140 "../Main.m3"
 /* load_integer */
#line 140 "../Main.m3"
 /* and */
#line 140 "../Main.m3"
 /* if_true_or_false */
#line 140 "../Main.m3"
 /* load_host_integer */
#line 140 "../Main.m3"
 /* load_integer */
#line 140 "../Main.m3"
 /* if_compare */
#line 140 "../Main.m3"
if(m3_ne(INT64,
  ((INT64)(((INT64)((INT64)Main_m_125_L_126))&  INT64_(1))),
   INT64_(0)))goto L2E;
#line 140 "../Main.m3"
 /* load */
#line 140 "../Main.m3"
 /* load_indirect */
#line 140 "../Main.m3"
 /* extract_mn */
#line 140 "../Main.m3"
 /* load_host_integer */
#line 140 "../Main.m3"
 /* load_integer */
#line 140 "../Main.m3"
 /* load_host_integer */
#line 140 "../Main.m3"
 /* load_integer */
#line 140 "../Main.m3"
 /* extract */
#line 140 "../Main.m3"
 /* if_true_or_false */
#line 140 "../Main.m3"
 /* load_host_integer */
#line 140 "../Main.m3"
 /* load_integer */
#line 140 "../Main.m3"
 /* if_compare */
#line 140 "../Main.m3"
if(m3_eq(INT64,
 ((INT64)(m3_extract(
 UINT64,
  *((INT64*)(INT64_(-8)+((ADDRESS)(Main_m_125_L_126)))),
   UINT64_(22),
   UINT64_(1)))),
   INT64_(0)))goto L2E;
#line 140 "../Main.m3"
 /* start_call_direct */
#line 140 "../Main.m3"
 /* load */
#line 140 "../Main.m3"
 /* pop_param */
#line 140 "../Main.m3"
 /* call_direct */
#line 140 "../Main.m3"
RTHooks__CheckLoadTracedRef(
  ( REFANY )(((ADDRESS)(Main_m_125_L_126)) ));
#line 140 "../Main.m3"
 /* set_label */
#line 140 "../Main.m3"
L2E:;
#line 140 "../Main.m3"
 /* load */
#line 140 "../Main.m3"
 /* store */
#line 140 "../Main.m3"
(*(ADDRESS*)(&Main_m_134_L_135))=(ADDRESS)(((ADDRESS)(Main_m_125_L_126)));
#line 140 "../Main.m3"
 /* load */
#line 140 "../Main.m3"
 /* load_indirect */
#line 140 "../Main.m3"
 /* load */
#line 140 "../Main.m3"
 /* load_indirect */
#line 140 "../Main.m3"
 /* load */
#line 140 "../Main.m3"
 /* swap */
#line 140 "../Main.m3"
 /* check_index */
#line 140 "../Main.m3"
 /* swap */
#line 140 "../Main.m3"
 /* store */
#line 140 "../Main.m3"
(*(INT64*)(&Main_m_136_L_137))=(INT64)( i_L_131);
#line 140 "../Main.m3"
 /* load */
#line 140 "../Main.m3"
 /* swap */
#line 140 "../Main.m3"
/*check_index*/if(((UINT64)(*((INT64*)(INT64_(8)+((ADDRESS)(Main_m_134_L_135))))))<=((UINT64)(Main_m_136_L_137)))Main_m_M_Main_L_42_CRASH(4482);
#line 140 "../Main.m3"
 /* index_address */
#line 140 "../Main.m3"
 /* load_integer */
#line 140 "../Main.m3"
 /* store_indirect */
#line 140 "../Main.m3"
(*(UINT64*)((((ADDRESS)(*((ADDRESS*)(Main_m_134_L_135))))+(8*( Main_m_136_L_137)))))=(INT64)(  INT64_(0));
#line 140 "../Main.m3"
 /* set_source_line */
#line 140 "../Main.m3"
#line 138 "../Main.m3"
 /* load_integer */
#line 138 "../Main.m3"
 /* load */
#line 138 "../Main.m3"
 /* add */
#line 138 "../Main.m3"
 /* store */
#line 138 "../Main.m3"
(*(INT64*)(&i_L_131))=(INT64)( ((INT64)(  INT64_(1)+ i_L_131)));
#line 138 "../Main.m3"
 /* set_label */
#line 138 "../Main.m3"
L2C:;
#line 138 "../Main.m3"
 /* load */
#line 138 "../Main.m3"
 /* load */
#line 138 "../Main.m3"
 /* if_compare */
#line 138 "../Main.m3"
if(m3_ge(INT64,
  Main_m_132_L_133,
  i_L_131))goto L2B;
#line 138 "../Main.m3"
 /* set_label */
#line 138 "../Main.m3"
 /* end_block */
#line 138 "../Main.m3"
 /* set_source_line */
#line 138 "../Main.m3"
#line 143 "../Main.m3"
 /* start_call_direct */
#line 143 "../Main.m3"
 /* load */
#line 143 "../Main.m3"
 /* pop_param */
#line 143 "../Main.m3"
 /* load_integer */
#line 143 "../Main.m3"
 /* pop_param */
#line 143 "../Main.m3"
 /* call_direct */
#line 143 "../Main.m3"
 /* store */
#line 143 "../Main.m3"
(*(ADDRESS*)(&Main_m_125_L_126))=(ADDRESS)(((ADDRESS)(Fmt__Int(
  ( INTEGER )( ((INT64)(*((UINT64*)(INT64_(392)+((ADDRESS)(&Main_m_M_Main_L_42)))))) ),
  ( Fmt__Base )(((UINT8)( INT64_(10))) )))));
#line 143 "../Main.m3"
 /* start_call_direct */
#line 143 "../Main.m3"
 /* load_address */
#line 143 "../Main.m3"
 /* pop_param */
#line 143 "../Main.m3"
 /* load */
#line 143 "../Main.m3"
 /* pop_param */
#line 143 "../Main.m3"
 /* call_direct */
#line 143 "../Main.m3"
 /* store */
#line 143 "../Main.m3"
(*(ADDRESS*)(&Main_m_138_L_139))=(ADDRESS)(((ADDRESS)(RTHooks__Concat(
  ( TEXT )(((ADDRESS)(INT64_(1160)+((ADDRESS)(&Main_m_40_L_41)))) ),
  ( TEXT )(((ADDRESS)(Main_m_125_L_126)) )))));
#line 143 "../Main.m3"
 /* start_call_direct */
#line 143 "../Main.m3"
 /* load */
#line 143 "../Main.m3"
 /* pop_param */
#line 143 "../Main.m3"
 /* load_address */
#line 143 "../Main.m3"
 /* pop_param */
#line 143 "../Main.m3"
 /* call_direct */
#line 143 "../Main.m3"
 /* store */
#line 143 "../Main.m3"
(*(ADDRESS*)(&Main_m_140_L_141))=(ADDRESS)(((ADDRESS)(RTHooks__Concat(
  ( TEXT )(((ADDRESS)(Main_m_138_L_139)) ),
  ( TEXT )(((ADDRESS)(INT64_(1192)+((ADDRESS)(&Main_m_40_L_41)))) )))));
#line 143 "../Main.m3"
 /* start_call_direct */
#line 143 "../Main.m3"
 /* load */
#line 143 "../Main.m3"
 /* pop_param */
#line 143 "../Main.m3"
 /* load_integer */
#line 143 "../Main.m3"
 /* pop_param */
#line 143 "../Main.m3"
 /* call_direct */
#line 143 "../Main.m3"
 /* store */
#line 143 "../Main.m3"
(*(ADDRESS*)(&Main_m_142_L_143))=(ADDRESS)(((ADDRESS)(Fmt__Int(
  ( INTEGER )( ((INT64)(*((UINT64*)(INT64_(376)+((ADDRESS)(&Main_m_M_Main_L_42)))))) ),
  ( Fmt__Base )(((UINT8)( INT64_(10))) )))));
#line 143 "../Main.m3"
 /* start_call_direct */
#line 143 "../Main.m3"
 /* load */
#line 143 "../Main.m3"
 /* pop_param */
#line 143 "../Main.m3"
 /* load */
#line 143 "../Main.m3"
 /* pop_param */
#line 143 "../Main.m3"
 /* call_direct */
#line 143 "../Main.m3"
 /* store */
#line 143 "../Main.m3"
(*(ADDRESS*)(&Main_m_144_L_145))=(ADDRESS)(((ADDRESS)(RTHooks__Concat(
  ( TEXT )(((ADDRESS)(Main_m_140_L_141)) ),
  ( TEXT )(((ADDRESS)(Main_m_142_L_143)) )))));
#line 143 "../Main.m3"
 /* start_call_direct */
#line 143 "../Main.m3"
 /* load */
#line 143 "../Main.m3"
 /* pop_param */
#line 143 "../Main.m3"
 /* call_direct */
#line 143 "../Main.m3"
Main__Print(
  ( TEXT )(((ADDRESS)(Main_m_144_L_145)) ));
#line 143 "../Main.m3"
 /* set_source_line */
#line 143 "../Main.m3"
#line 144 "../Main.m3"
 /* load */
#line 144 "../Main.m3"
 /* cvt_float */
#line 144 "../Main.m3"
 /* load_float */
#line 144 "../Main.m3"
 /* multiply */
#line 144 "../Main.m3"
 /* load */
#line 144 "../Main.m3"
 /* cvt_float */
#line 144 "../Main.m3"
 /* divide */
#line 144 "../Main.m3"
 /* load_float */
#line 144 "../Main.m3"
 /* divide */
#line 144 "../Main.m3"
 /* cvt_int */
#line 144 "../Main.m3"
 /* load_integer */
#line 144 "../Main.m3"
 /* add */
#line 144 "../Main.m3"
 /* store */
#line 144 "../Main.m3"
(*(INT64*)(&Main_m_129_L_130))=(INT64)( ((INT64)( ((INT64)(m3_round(
  ((double)( ((double)( ((double)( ((double)(((UINT64)(((INT64)(*((UINT64*)(INT64_(376)+((ADDRESS)(&Main_m_M_Main_L_42))))))))))* ((double)(1.10000000000000001e-1))))/ ((double)(((UINT64)(((INT64)(*((UINT64*)(INT64_(392)+((ADDRESS)(&Main_m_M_Main_L_42))))))))))))/ ((double)(6.00000000000000000e1)))))))+  INT64_(1))));
#line 144 "../Main.m3"
 /* begin_block */
#line 144 "../Main.m3"
 /* load */
#line 144 "../Main.m3"
 /* store */
#line 144 "../Main.m3"
(*(INT64*)(&minutes_L_146))=(INT64)( Main_m_129_L_130);
#line 144 "../Main.m3"
 /* set_source_line */
#line 144 "../Main.m3"
#line 146 "../Main.m3"
 /* start_call_direct */
#line 146 "../Main.m3"
 /* load */
#line 146 "../Main.m3"
 /* pop_param */
#line 146 "../Main.m3"
 /* load_integer */
#line 146 "../Main.m3"
 /* pop_param */
#line 146 "../Main.m3"
 /* call_direct */
#line 146 "../Main.m3"
 /* store */
#line 146 "../Main.m3"
(*(ADDRESS*)(&Main_m_144_L_145))=(ADDRESS)(((ADDRESS)(Fmt__Int(
  ( INTEGER )( minutes_L_146 ),
  ( Fmt__Base )(((UINT8)( INT64_(10))) )))));
#line 146 "../Main.m3"
 /* start_call_direct */
#line 146 "../Main.m3"
 /* load_address */
#line 146 "../Main.m3"
 /* pop_param */
#line 146 "../Main.m3"
 /* load */
#line 146 "../Main.m3"
 /* pop_param */
#line 146 "../Main.m3"
 /* call_direct */
#line 146 "../Main.m3"
 /* store */
#line 146 "../Main.m3"
(*(ADDRESS*)(&Main_m_142_L_143))=(ADDRESS)(((ADDRESS)(RTHooks__Concat(
  ( TEXT )(((ADDRESS)(INT64_(1248)+((ADDRESS)(&Main_m_40_L_41)))) ),
  ( TEXT )(((ADDRESS)(Main_m_144_L_145)) )))));
#line 146 "../Main.m3"
 /* start_call_direct */
#line 146 "../Main.m3"
 /* load */
#line 146 "../Main.m3"
 /* pop_param */
#line 146 "../Main.m3"
 /* load_address */
#line 146 "../Main.m3"
 /* pop_param */
#line 146 "../Main.m3"
 /* call_direct */
#line 146 "../Main.m3"
 /* store */
#line 146 "../Main.m3"
(*(ADDRESS*)(&Main_m_140_L_141))=(ADDRESS)(((ADDRESS)(RTHooks__Concat(
  ( TEXT )(((ADDRESS)(Main_m_142_L_143)) ),
  ( TEXT )(((ADDRESS)(INT64_(1304)+((ADDRESS)(&Main_m_40_L_41)))) )))));
#line 146 "../Main.m3"
 /* start_call_direct */
#line 146 "../Main.m3"
 /* load_integer */
#line 146 "../Main.m3"
 /* load */
#line 146 "../Main.m3"
 /* add */
#line 146 "../Main.m3"
 /* pop_param */
#line 146 "../Main.m3"
 /* load_integer */
#line 146 "../Main.m3"
 /* pop_param */
#line 146 "../Main.m3"
 /* call_direct */
#line 146 "../Main.m3"
 /* store */
#line 146 "../Main.m3"
(*(ADDRESS*)(&Main_m_138_L_139))=(ADDRESS)(((ADDRESS)(Fmt__Int(
  ( INTEGER )( ((INT64)(  INT64_(1)+ minutes_L_146)) ),
  ( Fmt__Base )(((UINT8)( INT64_(10))) )))));
#line 146 "../Main.m3"
 /* start_call_direct */
#line 146 "../Main.m3"
 /* load */
#line 146 "../Main.m3"
 /* pop_param */
#line 146 "../Main.m3"
 /* load */
#line 146 "../Main.m3"
 /* pop_param */
#line 146 "../Main.m3"
 /* call_direct */
#line 146 "../Main.m3"
 /* store */
#line 146 "../Main.m3"
(*(ADDRESS*)(&Main_m_125_L_126))=(ADDRESS)(((ADDRESS)(RTHooks__Concat(
  ( TEXT )(((ADDRESS)(Main_m_140_L_141)) ),
  ( TEXT )(((ADDRESS)(Main_m_138_L_139)) )))));
#line 146 "../Main.m3"
 /* start_call_direct */
#line 146 "../Main.m3"
 /* load */
#line 146 "../Main.m3"
 /* pop_param */
#line 146 "../Main.m3"
 /* load_address */
#line 146 "../Main.m3"
 /* pop_param */
#line 146 "../Main.m3"
 /* call_direct */
#line 146 "../Main.m3"
 /* store */
#line 146 "../Main.m3"
(*(ADDRESS*)(&Main_m_147_L_148))=(ADDRESS)(((ADDRESS)(RTHooks__Concat(
  ( TEXT )(((ADDRESS)(Main_m_125_L_126)) ),
  ( TEXT )(((ADDRESS)(INT64_(1336)+((ADDRESS)(&Main_m_40_L_41)))) )))));
#line 146 "../Main.m3"
 /* start_call_direct */
#line 146 "../Main.m3"
 /* load */
#line 146 "../Main.m3"
 /* pop_param */
#line 146 "../Main.m3"
 /* call_direct */
#line 146 "../Main.m3"
Main__Print(
  ( TEXT )(((ADDRESS)(Main_m_147_L_148)) ));
#line 146 "../Main.m3"
 /* end_block */
#line 146 "../Main.m3"
 /* set_source_line */
#line 146 "../Main.m3"
#line 148 "../Main.m3"
 /* start_call_direct */
#line 148 "../Main.m3"
 /* load_address */
#line 148 "../Main.m3"
 /* pop_param */
#line 148 "../Main.m3"
 /* call_direct */
#line 148 "../Main.m3"
Main__Print(
  ( TEXT )(((ADDRESS)(INT64_(656)+((ADDRESS)(&Main_m_40_L_41)))) ));
#line 148 "../Main.m3"
 /* set_source_line */
#line 148 "../Main.m3"
#line 150 "../Main.m3"
 /* begin_block */
#line 150 "../Main.m3"
 /* load_address */
#line 150 "../Main.m3"
 /* store */
#line 150 "../Main.m3"
(*(ADDRESS*)(&Main_m_127_L_128))=(ADDRESS)(((ADDRESS)(INT64_(16)+((ADDRESS)(&Main_m_127_L_128)))));
#line 150 "../Main.m3"
 /* load_integer */
#line 150 "../Main.m3"
 /* store */
#line 150 "../Main.m3"
(*(INT64*)((8)+(char*)(&Main_m_127_L_128)))=(INT64)(  INT64_(1));
#line 150 "../Main.m3"
 /* load */
#line 150 "../Main.m3"
 /* store */
#line 150 "../Main.m3"
(*(INT64*)((16)+(char*)(&Main_m_127_L_128)))=(INT64)( ((INT64)(*((UINT64*)(INT64_(392)+((ADDRESS)(&Main_m_M_Main_L_42)))))));
#line 150 "../Main.m3"
 /* start_call_direct */
#line 150 "../Main.m3"
 /* load */
#line 150 "../Main.m3"
 /* pop_param */
#line 150 "../Main.m3"
 /* load_address */
#line 150 "../Main.m3"
 /* pop_param */
#line 150 "../Main.m3"
 /* call_direct */
#line 150 "../Main.m3"
 /* store */
#line 150 "../Main.m3"
(*(ADDRESS*)(&Main_m_125_L_126))=(ADDRESS)(((ADDRESS)(RTHooks__AllocateOpenArray(
  ( ADDRESS )(((ADDRESS)(*((ADDRESS*)(INT64_(704)+((ADDRESS)(&Main_m_M_Main_L_42)))))) ),
  ( RTHooks__ArrayShape* /*TypeText1*/  )(((ADDRESS)(&Main_m_127_L_128)) )))));
#line 150 "../Main.m3"
 /* load */
#line 150 "../Main.m3"
 /* store */
#line 150 "../Main.m3"
(*(ADDRESS*)(&child_L_149))=(ADDRESS)(((ADDRESS)(Main_m_125_L_126)));
#line 150 "../Main.m3"
 /* set_source_line */
#line 150 "../Main.m3"
#line 152 "../Main.m3"
 /* load */
#line 152 "../Main.m3"
 /* store */
#line 152 "../Main.m3"
(*(INT64*)(&Main_m_129_L_130))=(INT64)( ((INT64)(*((UINT64*)(INT64_(392)+((ADDRESS)(&Main_m_M_Main_L_42)))))));
#line 152 "../Main.m3"
 /* begin_block */
#line 152 "../Main.m3"
 /* load_integer */
#line 152 "../Main.m3"
 /* store */
#line 152 "../Main.m3"
(*(INT64*)(&i_L_150))=(INT64)(  INT64_(1));
#line 152 "../Main.m3"
 /* load */
#line 152 "../Main.m3"
 /* store */
#line 152 "../Main.m3"
(*(INT64*)(&Main_m_151_L_152))=(INT64)( Main_m_129_L_130);
#line 152 "../Main.m3"
 /* jump */
#line 152 "../Main.m3"
goto L30;
#line 152 "../Main.m3"
 /* set_label */
#line 152 "../Main.m3"
L2F:;
#line 152 "../Main.m3"
 /* set_source_line */
#line 152 "../Main.m3"
#line 154 "../Main.m3"
 /* load */
#line 154 "../Main.m3"
 /* store */
#line 154 "../Main.m3"
(*(ADDRESS*)(&Main_m_125_L_126))=(ADDRESS)(((ADDRESS)(child_L_149)));
#line 154 "../Main.m3"
 /* load */
#line 154 "../Main.m3"
 /* load_indirect */
#line 154 "../Main.m3"
 /* extract_mn */
#line 154 "../Main.m3"
 /* load_host_integer */
#line 154 "../Main.m3"
 /* load_integer */
#line 154 "../Main.m3"
 /* load_host_integer */
#line 154 "../Main.m3"
 /* load_integer */
#line 154 "../Main.m3"
 /* extract */
#line 154 "../Main.m3"
 /* if_true_or_false */
#line 154 "../Main.m3"
 /* load_host_integer */
#line 154 "../Main.m3"
 /* load_integer */
#line 154 "../Main.m3"
 /* if_compare */
#line 154 "../Main.m3"
if(m3_ne(INT64,
 ((INT64)(m3_extract(
 UINT64,
  *((INT64*)(INT64_(-8)+((ADDRESS)(Main_m_125_L_126)))),
   UINT64_(21),
   UINT64_(1)))),
   INT64_(0)))goto L32;
#line 154 "../Main.m3"
 /* start_call_direct */
#line 154 "../Main.m3"
 /* load */
#line 154 "../Main.m3"
 /* pop_param */
#line 154 "../Main.m3"
 /* call_direct */
#line 154 "../Main.m3"
RTHooks__CheckStoreTraced(
  ( REFANY )(((ADDRESS)(Main_m_125_L_126)) ));
#line 154 "../Main.m3"
 /* set_label */
#line 154 "../Main.m3"
L32:;
#line 154 "../Main.m3"
 /* start_call_direct */
#line 154 "../Main.m3"
 /* load */
#line 154 "../Main.m3"
 /* pop_param */
#line 154 "../Main.m3"
 /* call_direct */
#line 154 "../Main.m3"
 /* store */
#line 154 "../Main.m3"
(*(ADDRESS*)(&Main_m_138_L_139))=(ADDRESS)(((ADDRESS)(RTHooks__AllocateTracedObj(
  ( ADDRESS )(((ADDRESS)(*((ADDRESS*)(INT64_(720)+((ADDRESS)(&Main_m_M_Main_L_42)))))) )))));
#line 154 "../Main.m3"
 /* load */
#line 154 "../Main.m3"
 /* load */
#line 154 "../Main.m3"
 /* store_indirect */
#line 154 "../Main.m3"
(*(UINT64*)((8)+(char*)(Main_m_138_L_139)))=(INT64)( i_L_150);
#line 154 "../Main.m3"
 /* start_call_direct */
#line 154 "../Main.m3"
 /* load */
#line 154 "../Main.m3"
 /* pop_param */
#line 154 "../Main.m3"
 /* call_direct */
#line 154 "../Main.m3"
 /* store */
#line 154 "../Main.m3"
(*(ADDRESS*)(&Main_m_140_L_141))=(ADDRESS)(((ADDRESS)(Thread__Fork(
  ( Thread__Closure )(((ADDRESS)(Main_m_138_L_139)) )))));
#line 154 "../Main.m3"
 /* load */
#line 154 "../Main.m3"
 /* store */
#line 154 "../Main.m3"
(*(ADDRESS*)(&Main_m_142_L_143))=(ADDRESS)(((ADDRESS)(Main_m_125_L_126)));
#line 154 "../Main.m3"
 /* load */
#line 154 "../Main.m3"
 /* load_indirect */
#line 154 "../Main.m3"
 /* load */
#line 154 "../Main.m3"
 /* load_integer */
#line 154 "../Main.m3"
 /* subtract */
#line 154 "../Main.m3"
 /* load */
#line 154 "../Main.m3"
 /* load_indirect */
#line 154 "../Main.m3"
 /* check_index */
#line 154 "../Main.m3"
 /* swap */
#line 154 "../Main.m3"
 /* store */
#line 154 "../Main.m3"
(*(INT64*)(&Main_m_153_L_154))=(INT64)( ((INT64)( i_L_150-  INT64_(1))));
#line 154 "../Main.m3"
 /* load */
#line 154 "../Main.m3"
 /* swap */
#line 154 "../Main.m3"
/*check_index*/if(((UINT64)(*((INT64*)(INT64_(8)+((ADDRESS)(Main_m_142_L_143))))))<=((UINT64)(Main_m_153_L_154)))Main_m_M_Main_L_42_CRASH(4930);
#line 154 "../Main.m3"
 /* index_address */
#line 154 "../Main.m3"
 /* store */
#line 154 "../Main.m3"
(*(ADDRESS*)(&Main_m_144_L_145))=(ADDRESS)(((ADDRESS)((((ADDRESS)(*((ADDRESS*)(Main_m_142_L_143))))+(8*( Main_m_153_L_154))))));
#line 154 "../Main.m3"
 /* load */
#line 154 "../Main.m3"
 /* load */
#line 154 "../Main.m3"
 /* store_indirect */
#line 154 "../Main.m3"
(*(ADDRESS*)(Main_m_144_L_145))=(ADDRESS)(((ADDRESS)(Main_m_140_L_141)));
#line 154 "../Main.m3"
 /* set_source_line */
#line 154 "../Main.m3"
#line 152 "../Main.m3"
 /* load_integer */
#line 152 "../Main.m3"
 /* load */
#line 152 "../Main.m3"
 /* add */
#line 152 "../Main.m3"
 /* store */
#line 152 "../Main.m3"
(*(INT64*)(&i_L_150))=(INT64)( ((INT64)(  INT64_(1)+ i_L_150)));
#line 152 "../Main.m3"
 /* set_label */
#line 152 "../Main.m3"
L30:;
#line 152 "../Main.m3"
 /* load */
#line 152 "../Main.m3"
 /* load */
#line 152 "../Main.m3"
 /* if_compare */
#line 152 "../Main.m3"
if(m3_ge(INT64,
  Main_m_151_L_152,
  i_L_150))goto L2F;
#line 152 "../Main.m3"
 /* set_label */
#line 152 "../Main.m3"
 /* end_block */
#line 152 "../Main.m3"
 /* set_source_line */
#line 152 "../Main.m3"
#line 156 "../Main.m3"
 /* load */
#line 156 "../Main.m3"
 /* store */
#line 156 "../Main.m3"
(*(INT64*)(&Main_m_129_L_130))=(INT64)( ((INT64)(*((UINT64*)(INT64_(392)+((ADDRESS)(&Main_m_M_Main_L_42)))))));
#line 156 "../Main.m3"
 /* begin_block */
#line 156 "../Main.m3"
 /* load_integer */
#line 156 "../Main.m3"
 /* store */
#line 156 "../Main.m3"
(*(INT64*)(&i_L_155))=(INT64)(  INT64_(1));
#line 156 "../Main.m3"
 /* load */
#line 156 "../Main.m3"
 /* store */
#line 156 "../Main.m3"
(*(INT64*)(&Main_m_156_L_157))=(INT64)( Main_m_129_L_130);
#line 156 "../Main.m3"
 /* jump */
#line 156 "../Main.m3"
goto L34;
#line 156 "../Main.m3"
 /* set_label */
#line 156 "../Main.m3"
L33:;
#line 156 "../Main.m3"
 /* set_source_line */
#line 156 "../Main.m3"
#line 158 "../Main.m3"
 /* load */
#line 158 "../Main.m3"
 /* store */
#line 158 "../Main.m3"
(*(ADDRESS*)(&Main_m_144_L_145))=(ADDRESS)(((ADDRESS)(child_L_149)));
#line 158 "../Main.m3"
 /* load */
#line 158 "../Main.m3"
 /* load_indirect */
#line 158 "../Main.m3"
 /* load */
#line 158 "../Main.m3"
 /* load_integer */
#line 158 "../Main.m3"
 /* subtract */
#line 158 "../Main.m3"
 /* load */
#line 158 "../Main.m3"
 /* load_indirect */
#line 158 "../Main.m3"
 /* check_index */
#line 158 "../Main.m3"
 /* swap */
#line 158 "../Main.m3"
 /* store */
#line 158 "../Main.m3"
(*(INT64*)(&Main_m_158_L_159))=(INT64)( ((INT64)( i_L_155-  INT64_(1))));
#line 158 "../Main.m3"
 /* load */
#line 158 "../Main.m3"
 /* swap */
#line 158 "../Main.m3"
/*check_index*/if(((UINT64)(*((INT64*)(INT64_(8)+((ADDRESS)(Main_m_144_L_145))))))<=((UINT64)(Main_m_158_L_159)))Main_m_M_Main_L_42_CRASH(5058);
#line 158 "../Main.m3"
 /* index_address */
#line 158 "../Main.m3"
 /* load_indirect */
#line 158 "../Main.m3"
 /* store */
#line 158 "../Main.m3"
(*(ADDRESS*)(&Main_m_142_L_143))=(ADDRESS)(((ADDRESS)(*((ADDRESS*)((((ADDRESS)(*((ADDRESS*)(Main_m_144_L_145))))+(8*( Main_m_158_L_159))))))));
#line 158 "../Main.m3"
 /* load_nil */
#line 158 "../Main.m3"
 /* load */
#line 158 "../Main.m3"
 /* if_compare */
#line 158 "../Main.m3"
if(m3_eq(ADDRESS,
 ((ADDRESS)(0)),
 ((ADDRESS)(Main_m_142_L_143))))goto L36;
#line 158 "../Main.m3"
 /* load */
#line 158 "../Main.m3"
 /* loophole */
#line 158 "../Main.m3"
 /* load_integer */
#line 158 "../Main.m3"
 /* and */
#line 158 "../Main.m3"
 /* if_true_or_false */
#line 158 "../Main.m3"
 /* load_host_integer */
#line 158 "../Main.m3"
 /* load_integer */
#line 158 "../Main.m3"
 /* if_compare */
#line 158 "../Main.m3"
if(m3_ne(INT64,
  ((INT64)(((INT64)((INT64)Main_m_142_L_143))&  INT64_(1))),
   INT64_(0)))goto L36;
#line 158 "../Main.m3"
 /* load */
#line 158 "../Main.m3"
 /* load_indirect */
#line 158 "../Main.m3"
 /* extract_mn */
#line 158 "../Main.m3"
 /* load_host_integer */
#line 158 "../Main.m3"
 /* load_integer */
#line 158 "../Main.m3"
 /* load_host_integer */
#line 158 "../Main.m3"
 /* load_integer */
#line 158 "../Main.m3"
 /* extract */
#line 158 "../Main.m3"
 /* if_true_or_false */
#line 158 "../Main.m3"
 /* load_host_integer */
#line 158 "../Main.m3"
 /* load_integer */
#line 158 "../Main.m3"
 /* if_compare */
#line 158 "../Main.m3"
if(m3_eq(INT64,
 ((INT64)(m3_extract(
 UINT64,
  *((INT64*)(INT64_(-8)+((ADDRESS)(Main_m_142_L_143)))),
   UINT64_(22),
   UINT64_(1)))),
   INT64_(0)))goto L36;
#line 158 "../Main.m3"
 /* start_call_direct */
#line 158 "../Main.m3"
 /* load */
#line 158 "../Main.m3"
 /* pop_param */
#line 158 "../Main.m3"
 /* call_direct */
#line 158 "../Main.m3"
RTHooks__CheckLoadTracedRef(
  ( REFANY )(((ADDRESS)(Main_m_142_L_143)) ));
#line 158 "../Main.m3"
 /* set_label */
#line 158 "../Main.m3"
L36:;
#line 158 "../Main.m3"
 /* start_call_direct */
#line 158 "../Main.m3"
 /* load */
#line 158 "../Main.m3"
 /* pop_param */
#line 158 "../Main.m3"
 /* call_direct */
#line 158 "../Main.m3"
 /* store */
#line 158 "../Main.m3"
(*(ADDRESS*)(&Main_m_140_L_141))=(ADDRESS)(((ADDRESS)(Thread__Join(
  ( Thread__T )(((ADDRESS)(Main_m_142_L_143)) )))));
#line 158 "../Main.m3"
 /* set_source_line */
#line 158 "../Main.m3"
#line 156 "../Main.m3"
 /* load_integer */
#line 156 "../Main.m3"
 /* load */
#line 156 "../Main.m3"
 /* add */
#line 156 "../Main.m3"
 /* store */
#line 156 "../Main.m3"
(*(INT64*)(&i_L_155))=(INT64)( ((INT64)(  INT64_(1)+ i_L_155)));
#line 156 "../Main.m3"
 /* set_label */
#line 156 "../Main.m3"
L34:;
#line 156 "../Main.m3"
 /* load */
#line 156 "../Main.m3"
 /* load */
#line 156 "../Main.m3"
 /* if_compare */
#line 156 "../Main.m3"
if(m3_ge(INT64,
  Main_m_156_L_157,
  i_L_155))goto L33;
#line 156 "../Main.m3"
 /* set_label */
#line 156 "../Main.m3"
 /* end_block */
#line 156 "../Main.m3"
 /* end_block */
#line 156 "../Main.m3"
 /* set_source_line */
#line 156 "../Main.m3"
#line 162 "../Main.m3"
 /* start_call_direct */
#line 162 "../Main.m3"
 /* call_direct */
#line 162 "../Main.m3"
Main__CheckResults(
 );
#line 162 "../Main.m3"
 /* set_source_line */
#line 162 "../Main.m3"
#line 163 "../Main.m3"
 /* start_call_direct */
#line 163 "../Main.m3"
 /* load */
#line 163 "../Main.m3"
 /* check_hi */
#line 163 "../Main.m3"
 /* store */
#line 163 "../Main.m3"
(*(INT64*)(&Main_m_160_L_161))=(INT64)( ((INT64)(*((UINT64*)(INT64_(368)+((ADDRESS)(&Main_m_M_Main_L_42)))))));
#line 163 "../Main.m3"
 /* load */
#line 163 "../Main.m3"
/*check_hi*/if(INT64_(2147483647)<Main_m_160_L_161)Main_m_M_Main_L_42_CRASH(5217);
#line 163 "../Main.m3"
 /* pop_param */
#line 163 "../Main.m3"
 /* call_direct */
#line 163 "../Main.m3"
Process__Exit(
  ( Process__ExitCode )(((UINT32)(Main_m_160_L_161)) ));
#line 163 "../Main.m3"
 /* set_label */
#line 163 "../Main.m3"
L2A:;
#line 163 "../Main.m3"
 /* load_address */
#line 163 "../Main.m3"
 /* exit_proc */
#line 163 "../Main.m3"
return (RT0__ModulePtr)(&Main_m_M_Main_L_42);
#line 163 "../Main.m3"
 /* end_procedure */
#line 163 "../Main.m3"
} /* global constant type descriptor */
#line 163 "../Main.m3"
 /* global data type descriptor */
#line 163 "../Main.m3"
 /* module global constants */
#line 163 "../Main.m3"
 /* procedure names */
#line 163 "../Main.m3"
 /* procedure table */
#line 163 "../Main.m3"
 /* global type map */
#line 163 "../Main.m3"
 /* file name */
#line 163 "../Main.m3"
 /* type map for _td13dad59 */
#line 163 "../Main.m3"
 /* type map for _td13dad59 */
#line 163 "../Main.m3"
 /* type description for _td13dad59 */
#line 163 "../Main.m3"
 /* type map for _te397683d */
#line 163 "../Main.m3"
 /* type description for _te397683d */
#line 163 "../Main.m3"
 /* type map for _t967e734f */
#line 163 "../Main.m3"
 /* type description for _t967e734f */
#line 163 "../Main.m3"
 /* module global data */
#line 163 "../Main.m3"
 /* typecell for _t967e734f */
#line 163 "../Main.m3"
 /* typecell for _te397683d */
#line 163 "../Main.m3"
 /* typecell for _td13dad59 */
#line 163 "../Main.m3"
 /* load map


 global data allocation for M_Main
     0   104  8  *module info*
   104   152  8  typecell
   256   112  8  typecell
   368     8  8  Main.exitCode
   376     8  8  Main.maxCount
   384     8  8  Main.mutex
   392     8  8  Main.numThreads
   400     8  8  Main.sharedArray
   408     8  8  Main.sharedCounter
   416   112  8  typecell
   528    24  8  import Main
   552    24  8  import Thread
   576    24  8  import Process
   600    24  8  import IO
   624    24  8  import Fmt
   648    24  8  import RTHooks
   672    16  8  typecell ptr
   688    16  8  typecell ptr
   704    16  8  typecell ptr
   720    16  8  typecell ptr
   736     0  8  *TOTAL*


 global constants for M_Main
     0    40  8  TEXT literal methods
    40    26  8  *TEXT literal*
    72    80  8  *TEXT literal*
   152    29  8  *TEXT literal*
   184    60  8  *TEXT literal*
   248    60  8  *TEXT literal*
   312    59  8  *TEXT literal*
   376    26  8  *TEXT literal*
   408    27  8  *TEXT literal*
   440    26  8  *TEXT literal*
   472    44  8  *TEXT literal*
   520    93  8  *TEXT literal*
   616    27  8  *TEXT literal*
   648   104  8  *TEXT literal*
   752   104  8  *TEXT literal*
   856    48  8  *TEXT literal*
   904    87  8  *TEXT literal*
   992    74  8  *TEXT literal*
  1072    79  8  *TEXT literal*
  1152    31  8  *TEXT literal*
  1184    50  8  *TEXT literal*
  1240    53  8  *TEXT literal*
  1296    29  8  *TEXT literal*
  1328    34  8  *TEXT literal*
  1368    38  8  *proc names*
  1408    72  8  *proc info*
  1480     7  1  type_map
  1487    11  1  *string*
  1498     5  1  type_map
  1503     5  1  type_map
  1508     8  1  type_desc
  1516     5  1  type_map
  1521     4  1  type_desc
  1525    18  1  *string*
  1543     2  1  type_map
  1545     4  1  type_desc
  1552     8  8  method list
  1560    18  1  *string*
  1584     0  8  *TOTAL*
 */
#line 163 "../Main.m3"
 /* end unit */
#line 163 "../Main.m3"

#ifdef __cplusplus

} /* extern "C" */
#endif
 /* set_runtime_proc */
