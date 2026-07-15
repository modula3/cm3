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
 /* declare_formal */
 /* declare_subrange */
/*subrange_define*/typedef UINT8 T8E2831D7_8;
 /* declare_proctype */
 /* declare_formal */

#ifndef Main__ThreadNo
#define Main__ThreadNo Main__ThreadNo
typedef T8E2831D7_8 Main__ThreadNo;
#endif
 /* declare_enum */
 /* declare_enum_elt */
 /* declare_enum_elt: NUMBER(self.enum.names^):7 */
 /* declare_enum_elt: enum_element_count:7 */
 /* declare_enum_elt */
 /* declare_enum_elt: NUMBER(self.enum.names^):7 */
 /* declare_enum_elt: enum_element_count:7 */
 /* declare_enum_elt */
 /* declare_enum_elt: NUMBER(self.enum.names^):7 */
 /* declare_enum_elt: enum_element_count:7 */
 /* declare_enum_elt */
 /* declare_enum_elt: NUMBER(self.enum.names^):7 */
 /* declare_enum_elt: enum_element_count:7 */
 /* declare_enum_elt */
 /* declare_enum_elt: NUMBER(self.enum.names^):7 */
 /* declare_enum_elt: enum_element_count:7 */
 /* declare_enum_elt */
 /* declare_enum_elt: NUMBER(self.enum.names^):7 */
 /* declare_enum_elt: enum_element_count:7 */
 /* declare_enum_elt */
 /* declare_enum_elt: NUMBER(self.enum.names^):7 */
 /* declare_enum_elt: enum_element_count:7 */
/*enum_define*/typedef UINT8 T7609BE10; /*declare_enum*/
#define T7609BE10_Null ((UINT8)0) /*declare_enum_elt*/
#define T7609BE10_Idle ((UINT8)1) /*declare_enum_elt*/
#define T7609BE10_Acq ((UINT8)2) /*declare_enum_elt*/
#define T7609BE10_Rel ((UINT8)3) /*declare_enum_elt*/
#define T7609BE10_Wait ((UINT8)4) /*declare_enum_elt*/
#define T7609BE10_Wait2 ((UINT8)5) /*declare_enum_elt*/
#define T7609BE10_Sig ((UINT8)6) /*declare_enum_elt*/
 /* declare_set */

#ifndef T5FD62D41
#define T5FD62D41 T5FD62D41
/*type_typedef*/typedef UINT8 T5FD62D41;

#endif
 /* declare_array */
/*array_forwardDeclare*/struct T6AEC7467;typedef struct T6AEC7467 T6AEC7467;

#ifndef T6AEC7467
#define T6AEC7467 T6AEC7467
/*fixedArray_define*/struct T6AEC7467{T7609BE10 _elts[5];};
#endif
 /* declare_proctype */
 /* declare_formal */

#ifndef Main__State
#define Main__State Main__State
typedef T7609BE10 Main__State;
#endif
 /* declare_proctype */
 /* declare_formal */
 /* declare_array */
/*array_forwardDeclare*/struct T3E0B34E6;typedef struct T3E0B34E6 T3E0B34E6;
 /* declare_proctype */
 /* declare_formal */
 /* declare_formal */
 /* declare_exception */
 /* declare_proctype */
 /* declare_formal */
 /* declare_formal */
 /* declare_raises */
 /* declare_proctype */

#if 0 /* avoid type hash collions */
typedef 
T8E2831D7_8(__cdecl*TDAF42ACB)(void);
#else
typedef void (__cdecl*TDAF42ACB)(void);
#endif
 /* declare_proctype */
 /* declare_formal */
 /* declare_formal */

#ifndef Main__StateSet
#define Main__StateSet Main__StateSet
typedef T5FD62D41 Main__StateSet;
#endif
 /* declare_proctype */
 /* declare_formal */
 /* declare_formal */
 /* declare_proctype */
 /* declare_formal */
 /* declare_formal */
 /* declare_formal */
 /* declare_formal */
 /* declare_proctype */
 /* declare_formal */
 /* declare_formal */
 /* declare_formal */
 /* declare_formal */
 /* declare_opaque */

#ifndef T57F0A1B6
#define T57F0A1B6 T57F0A1B6
/*1addressType_define*/typedef ADDRESS T57F0A1B6;

#endif
 /* declare_opaque */

#ifndef T4B16B0ED
#define T4B16B0ED T4B16B0ED
/*1addressType_define*/typedef ADDRESS T4B16B0ED;

#endif
 /* declare_array */
/*array_forwardDeclare*/struct T99709475;typedef struct T99709475 T99709475;

#ifndef T99709475
#define T99709475 T99709475
/*fixedArray_define*/struct T99709475{T4B16B0ED _elts[5];};
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
 /* record_forwardDeclare Record_t{ typeid:TFFFFFFFF text:T73AE990F_fields hash_text:NIL base_text:NIL state:0} */
/*record_forwardDeclare*/struct T73AE990F_fields;typedef struct T73AE990F_fields T73AE990F_fields;
 /* record_canBeDefined Record_t{ typeid:TFFFFFFFF text:T73AE990F_fields hash_text:NIL base_text:NIL state:0} */
 /* record_define Record_t{ typeid:TFFFFFFFF text:T73AE990F_fields hash_text:NIL base_text:NIL state:0} */

#ifndef T73AE990F_fields
#define T73AE990F_fields T73AE990F_fields
/*record_define*/struct T73AE990F_fields{
T8E2831D7_8 ClThN;
UINT8 L_1[7];
};
#endif
 /* declare_array */
/*array_forwardDeclare*/struct TB3152C55;typedef struct TB3152C55 TB3152C55;
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

#ifndef T62761487
#define T62761487 T62761487
/*1addressType_define*/typedef ADDRESS T62761487;

#endif
 /* declare_proctype */
 /* declare_formal */
 /* declare_opaque */

#ifndef TD1D9DD0E
#define TD1D9DD0E TD1D9DD0E
/*1addressType_define*/typedef ADDRESS TD1D9DD0E;

#endif
 /* declare_object */
 /* declare_field */
 /* declare_field */
 /* record_forwardDeclare Record_t{ typeid:TFFFFFFFF text:TAD398276_fields hash_text:NIL base_text:NIL state:0} */
/*record_forwardDeclare*/struct TAD398276_fields;typedef struct TAD398276_fields TAD398276_fields;
 /* record_canBeDefined Record_t{ typeid:TFFFFFFFF text:TAD398276_fields hash_text:NIL base_text:NIL state:0} */
 /* declare_proctype */
 /* declare_formal */

#ifndef Wr__T
#define Wr__T Wr__T
typedef T62761487 Wr__T;
#endif
 /* declare_formal */
 /* declare_raises */
 /* declare_raises */
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
UINT8 L_2[7];
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
 /* declare_raises */
 /* declare_raises */
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
 /* declare_proctype */
 /* declare_formal */
 /* declare_formal */
 /* declare_indirect */
 /* declare_proctype */
 /* declare_formal */
 /* declare_formal */
 /* declare_formal */
 /* declare_proctype */
 /* declare_formal */
 /* declare_proctype */
 /* declare_formal */
 /* declare_indirect */
typedef T7609BE10*T89F641EF;
 /* declare_proctype */
 /* declare_formal */
 /* declare_formal */
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
 /* declare_formal */

#ifndef Thread__Condition
#define Thread__Condition Thread__Condition
typedef T57F0A1B6 Thread__Condition;
#endif
 /* declare_proctype */
 /* declare_formal */
 /* declare_proctype */
 /* declare_formal */
 /* declare_proctype */
 /* declare_formal */

#ifndef Thread__Closure
#define Thread__Closure Thread__Closure
typedef TE545939D Thread__Closure;
#endif
 /* declare_record */
 /* declare_field */
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
 /* DeclareTypes_FlushOnce size:39 */

#if 0 /* avoid type hash collions */
typedef 
void(__cdecl*TC56731B5)(TEXT);
#else
typedef void (__cdecl*TC56731B5)(void);
#endif

#if 0 /* avoid type hash collions */
typedef 
TEXT(__cdecl*T5AEB1177)(Main__ThreadNo);
#else
typedef void (__cdecl*T5AEB1177)(void);
#endif

#if 0 /* avoid type hash collions */
typedef 
TEXT(__cdecl*T74D2ABF4)(Main__State);
#else
typedef void (__cdecl*T74D2ABF4)(void);
#endif

#if 0 /* avoid type hash collions */
typedef 
void(__cdecl*T1B64A79D)(Main__ThreadNo);
#else
typedef void (__cdecl*T1B64A79D)(void);
#endif

#ifndef T3E0B34E6
#define T3E0B34E6 T3E0B34E6
/*fixedArray_define*/struct T3E0B34E6{T1B64A79D _elts[5];};
#endif

#ifndef Main__ActionProc
#define Main__ActionProc Main__ActionProc
typedef T1B64A79D Main__ActionProc;
#endif

#if 0 /* avoid type hash collions */
typedef 
void(__cdecl*T17EBA478)(Main__ThreadNo,Main__ActionProc);
#else
typedef void (__cdecl*T17EBA478)(void);
#endif

#if 0 /* avoid type hash collions */
typedef 
void(__cdecl*T20114EE8)(Main__ThreadNo,Main__StateSet);
#else
typedef void (__cdecl*T20114EE8)(void);
#endif

#if 0 /* avoid type hash collions */
typedef 
void(__cdecl*TB7B87B2A)(Main__ThreadNo,Main__State);
#else
typedef void (__cdecl*TB7B87B2A)(void);
#endif

#if 0 /* avoid type hash collions */
typedef 
BOOLEAN(__cdecl*T22CF331F)(Main__ThreadNo,Main__StateSet,TEXT,TEXT);
#else
typedef void (__cdecl*T22CF331F)(void);
#endif

#if 0 /* avoid type hash collions */
typedef 
BOOLEAN(__cdecl*T1B36D2D0)(Main__ThreadNo,Main__State,TEXT,TEXT);
#else
typedef void (__cdecl*T1B36D2D0)(void);
#endif
typedef T73AE990F_fields*T73AE990F;

#ifndef TB3152C55
#define TB3152C55 TB3152C55
/*fixedArray_define*/struct TB3152C55{T73AE990F_fields* _elts[5];};
#endif

#ifndef Main__Cl
#define Main__Cl Main__Cl
typedef T73AE990F Main__Cl;
#endif

#if 0 /* avoid type hash collions */
typedef 
void(__cdecl*T37E50548)(REFANY);
#else
typedef void (__cdecl*T37E50548)(void);
#endif
typedef TAD398276_fields*TAD398276;
 /* record_canBeDefined Record_t{ typeid:TFFFFFFFF text:TAD398276_fields hash_text:NIL base_text:NIL state:0} */
 /* record_define Record_t{ typeid:TFFFFFFFF text:TAD398276_fields hash_text:NIL base_text:NIL state:0} */

#ifndef TAD398276_fields
#define TAD398276_fields TAD398276_fields
/*record_define*/struct TAD398276_fields{
TD1D9DD0E head;
TAD398276_fields* tail;
};
#endif

#if 0 /* avoid type hash collions */
typedef 
void(__cdecl*T1BEE6E85)(Wr__T,TEXT);
#else
typedef void (__cdecl*T1BEE6E85)(void);
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
void(__cdecl*TC5F201D2)(Wr__T);
#else
typedef void (__cdecl*TC5F201D2)(void);
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
void(__cdecl*TA4BB9882)(ADDRESS,INTEGER);
#else
typedef void (__cdecl*TA4BB9882)(void);
#endif
typedef T1B64A79D*TE49B5862;

#if 0 /* avoid type hash collions */
typedef 
void(__cdecl*TFCB2B387)(ADDRESS,INTEGER,TEXT);
#else
typedef void (__cdecl*TFCB2B387)(void);
#endif

#if 0 /* avoid type hash collions */
typedef 
void(__cdecl*T477C5063)(ADDRESS);
#else
typedef void (__cdecl*T477C5063)(void);
#endif

#if 0 /* avoid type hash collions */
typedef 
void(__cdecl*TD2DE6CCC)(LONGREAL);
#else
typedef void (__cdecl*TD2DE6CCC)(void);
#endif

#if 0 /* avoid type hash collions */
typedef 
void(__cdecl*T656BDC20)(ADDRESS,ADDRESS,ADDRESS,INTEGER);
#else
typedef void (__cdecl*T656BDC20)(void);
#endif

#if 0 /* avoid type hash collions */
typedef 
void(__cdecl*T16DE9EFF)(Thread__Mutex);
#else
typedef void (__cdecl*T16DE9EFF)(void);
#endif

#if 0 /* avoid type hash collions */
typedef 
void(__cdecl*T7F103F6E)(Thread__Mutex,Thread__Condition);
#else
typedef void (__cdecl*T7F103F6E)(void);
#endif

#if 0 /* avoid type hash collions */
typedef 
void(__cdecl*T4F2B3AE5)(Thread__Condition);
#else
typedef void (__cdecl*T4F2B3AE5)(void);
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
 /* DeclareTypes_FlushOnce size:2 */

#if 0 /* avoid type hash collions */
typedef 
void(__cdecl*T308224E3)(Main__ThreadNo,Main__ActionProc);
#else
typedef void (__cdecl*T308224E3)(void);
#endif

#if 0 /* avoid type hash collions */
typedef 
REFANY(__cdecl*T8F25BD55)(Main__Cl);
#else
typedef void (__cdecl*T8F25BD55)(void);
#endif
 /* DeclareTypes_FlushOnce size:0 */
 /* end: DeclareTypes */
 /* begin: helper functions */

#if __GNUC__ > 2 || __GNUC__ == 2 && __GNUC_MINOR__ >= 5
#define M3_ATTRIBUTE_NO_RETURN __attribute__((__noreturn__))
#else
#define M3_ATTRIBUTE_NO_RETURN
#endif
#define m3_extract(T, value, offset, count) ((((T)(value))>>((WORD_T)(offset)))&~(((~(T)0))<<((WORD_T)(count))))
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
   /* Param_Type1 */ INTEGER /*TypeText1*/  mode_L_3);
 /* import_procedure */
/*Proc_ForwardDeclareFrameType*/struct Wr_I3_Frame_t;typedef struct Wr_I3_Frame_t Wr_I3_Frame_t;
 /* internal_declare_param */
RT0__ModulePtr
__cdecl
Wr_I3(
   /* Param_Type1 */ INTEGER /*TypeText1*/  mode_L_4);
 /* import_procedure */
/*Proc_ForwardDeclareFrameType*/struct Thread_I3_Frame_t;typedef struct Thread_I3_Frame_t Thread_I3_Frame_t;
 /* internal_declare_param */
RT0__ModulePtr
__cdecl
Thread_I3(
   /* Param_Type1 */ INTEGER /*TypeText1*/  mode_L_5);
 /* import_procedure */
/*Proc_ForwardDeclareFrameType*/struct Stdio_I3_Frame_t;typedef struct Stdio_I3_Frame_t Stdio_I3_Frame_t;
 /* internal_declare_param */
RT0__ModulePtr
__cdecl
Stdio_I3(
   /* Param_Type1 */ INTEGER /*TypeText1*/  mode_L_6);
 /* import_procedure */
/*Proc_ForwardDeclareFrameType*/struct Fmt_I3_Frame_t;typedef struct Fmt_I3_Frame_t Fmt_I3_Frame_t;
 /* internal_declare_param */
RT0__ModulePtr
__cdecl
Fmt_I3(
   /* Param_Type1 */ INTEGER /*TypeText1*/  mode_L_7);
 /* import_procedure */
/*Proc_ForwardDeclareFrameType*/struct RTHooks_I3_Frame_t;typedef struct RTHooks_I3_Frame_t RTHooks_I3_Frame_t;
 /* internal_declare_param */
RT0__ModulePtr
__cdecl
RTHooks_I3(
   /* Param_Type1 */ INTEGER /*TypeText1*/  mode_L_8);
 /* import_procedure */
/*Proc_ForwardDeclareFrameType*/struct RTHooks__CheckLoadTracedRef_Frame_t;typedef struct RTHooks__CheckLoadTracedRef_Frame_t RTHooks__CheckLoadTracedRef_Frame_t;
 /* internal_declare_param */
void /*TypeText3*/ 
__cdecl
RTHooks__CheckLoadTracedRef(
   /* Param_Type1 */ REFANY ref_L_9);
 /* import_procedure */
/*Proc_ForwardDeclareFrameType*/struct Wr__PutText_Frame_t;typedef struct Wr__PutText_Frame_t Wr__PutText_Frame_t;
 /* internal_declare_param */
 /* internal_declare_param */
void /*TypeText3*/ 
__cdecl
Wr__PutText(
   /* Param_Type1 */ Wr__T wr_L_10,
   /* Param_Type1 */ TEXT t_L_11);
 /* import_procedure */
/*Proc_ForwardDeclareFrameType*/struct RTHooks__TextLitInfo_Frame_t;typedef struct RTHooks__TextLitInfo_Frame_t RTHooks__TextLitInfo_Frame_t;
 /* internal_declare_param */
 /* internal_declare_param */
void /*TypeText3*/ 
__cdecl
RTHooks__TextLitInfo(
   /* Param_Type1 */ RTHooks__TextLiteral t_L_12,
   /* Param_Type1 */ RTHooks__TextInfo* /*TypeText1*/  i_L_13);
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
   /* Param_Type1 */ RTHooks__TextLiteral t_L_14,
   /* Param_Type1 */ CARDINAL i_L_15);
 /* import_procedure */
/*Proc_ForwardDeclareFrameType*/struct RTHooks__TextLitGetWideChar_Frame_t;typedef struct RTHooks__TextLitGetWideChar_Frame_t RTHooks__TextLitGetWideChar_Frame_t;
 /* internal_declare_param */
 /* internal_declare_param */
WIDECHAR
__cdecl
RTHooks__TextLitGetWideChar(
   /* Param_Type1 */ RTHooks__TextLiteral t_L_16,
   /* Param_Type1 */ CARDINAL i_L_17);
 /* import_procedure */
/*Proc_ForwardDeclareFrameType*/struct RTHooks__TextLitGetChars_Frame_t;typedef struct RTHooks__TextLitGetChars_Frame_t RTHooks__TextLitGetChars_Frame_t;
 /* internal_declare_param */
 /* internal_declare_param */
 /* internal_declare_param */
void /*TypeText3*/ 
__cdecl
RTHooks__TextLitGetChars(
   /* Param_Type1 */ RTHooks__TextLiteral t_L_18,
   /* Param_Type1 */ T89CD34BD* /*TypeText1*/  a_L_19,
   /* Param_Type1 */ CARDINAL start_L_20);
 /* import_procedure */
/*Proc_ForwardDeclareFrameType*/struct RTHooks__TextLitGetWideChars_Frame_t;typedef struct RTHooks__TextLitGetWideChars_Frame_t RTHooks__TextLitGetWideChars_Frame_t;
 /* internal_declare_param */
 /* internal_declare_param */
 /* internal_declare_param */
void /*TypeText3*/ 
__cdecl
RTHooks__TextLitGetWideChars(
   /* Param_Type1 */ RTHooks__TextLiteral t_L_21,
   /* Param_Type1 */ TA19BDC21* /*TypeText1*/  a_L_22,
   /* Param_Type1 */ CARDINAL start_L_23);
 /* import_procedure */
/*Proc_ForwardDeclareFrameType*/struct Wr__Flush_Frame_t;typedef struct Wr__Flush_Frame_t Wr__Flush_Frame_t;
 /* internal_declare_param */
void /*TypeText3*/ 
__cdecl
Wr__Flush(
   /* Param_Type1 */ Wr__T wr_L_24);
 /* import_procedure */
/*Proc_ForwardDeclareFrameType*/struct Fmt__Int_Frame_t;typedef struct Fmt__Int_Frame_t Fmt__Int_Frame_t;
 /* internal_declare_param */
 /* internal_declare_param */
TEXT
__cdecl
Fmt__Int(
   /* Param_Type1 */ INTEGER n_L_25,
   /* Param_Type1 */ Fmt__Base base_L_26);
 /* import_procedure */
/*Proc_ForwardDeclareFrameType*/struct RTHooks__Concat_Frame_t;typedef struct RTHooks__Concat_Frame_t RTHooks__Concat_Frame_t;
 /* internal_declare_param */
 /* internal_declare_param */
TEXT
__cdecl
RTHooks__Concat(
   /* Param_Type1 */ TEXT a_L_27,
   /* Param_Type1 */ TEXT b_L_28);
 /* import_procedure */
/*Proc_ForwardDeclareFrameType*/struct RTHooks__ReportFault_Frame_t;typedef struct RTHooks__ReportFault_Frame_t RTHooks__ReportFault_Frame_t;
 /* internal_declare_param */
 /* internal_declare_param */
void /*TypeText3*/ 
__cdecl
RTHooks__ReportFault(
   /* Param_Type1 */ ADDRESS module_L_29,
   /* Param_Type1 */ INTEGER info_L_30) M3_ATTRIBUTE_NO_RETURN;
 /* import_procedure */
/*Proc_ForwardDeclareFrameType*/struct RTHooks__AssertFailed_Frame_t;typedef struct RTHooks__AssertFailed_Frame_t RTHooks__AssertFailed_Frame_t;
 /* internal_declare_param */
 /* internal_declare_param */
 /* internal_declare_param */
void /*TypeText3*/ 
__cdecl
RTHooks__AssertFailed(
   /* Param_Type1 */ ADDRESS module_L_31,
   /* Param_Type1 */ INTEGER line_L_32,
   /* Param_Type1 */ TEXT msg_L_33) M3_ATTRIBUTE_NO_RETURN;
 /* import_procedure */
/*Proc_ForwardDeclareFrameType*/struct RTHooks__ResumeRaise_Frame_t;typedef struct RTHooks__ResumeRaise_Frame_t RTHooks__ResumeRaise_Frame_t;
 /* internal_declare_param */
void /*TypeText3*/ 
__cdecl
RTHooks__ResumeRaise(
   /* Param_Type1 */ ADDRESS a_L_34);
 /* import_procedure */
/*Proc_ForwardDeclareFrameType*/struct Thread__Pause_Frame_t;typedef struct Thread__Pause_Frame_t Thread__Pause_Frame_t;
 /* internal_declare_param */
void /*TypeText3*/ 
__cdecl
Thread__Pause(
   /* Param_Type1 */ LONGREAL n_L_35);
 /* import_procedure */
/*Proc_ForwardDeclareFrameType*/struct RTHooks__Raise_Frame_t;typedef struct RTHooks__Raise_Frame_t RTHooks__Raise_Frame_t;
 /* internal_declare_param */
 /* internal_declare_param */
 /* internal_declare_param */
 /* internal_declare_param */
void /*TypeText3*/ 
__cdecl
RTHooks__Raise(
   /* Param_Type1 */ ADDRESS ex_L_36,
   /* Param_Type1 */ ADDRESS arg_L_37,
   /* Param_Type1 */ ADDRESS module_L_38,
   /* Param_Type1 */ INTEGER line_L_39) M3_ATTRIBUTE_NO_RETURN;
 /* import_procedure */
/*Proc_ForwardDeclareFrameType*/struct Thread__Acquire_Frame_t;typedef struct Thread__Acquire_Frame_t Thread__Acquire_Frame_t;
 /* internal_declare_param */
void /*TypeText3*/ 
__cdecl
Thread__Acquire(
   /* Param_Type1 */ Thread__Mutex m_L_40);
 /* import_procedure */
/*Proc_ForwardDeclareFrameType*/struct Thread__Release_Frame_t;typedef struct Thread__Release_Frame_t Thread__Release_Frame_t;
 /* internal_declare_param */
void /*TypeText3*/ 
__cdecl
Thread__Release(
   /* Param_Type1 */ Thread__Mutex m_L_41);
 /* import_procedure */
/*Proc_ForwardDeclareFrameType*/struct Thread__Wait_Frame_t;typedef struct Thread__Wait_Frame_t Thread__Wait_Frame_t;
 /* internal_declare_param */
 /* internal_declare_param */
void /*TypeText3*/ 
__cdecl
Thread__Wait(
   /* Param_Type1 */ Thread__Mutex m_L_42,
   /* Param_Type1 */ Thread__Condition c_L_43);
 /* import_procedure */
/*Proc_ForwardDeclareFrameType*/struct Thread__Signal_Frame_t;typedef struct Thread__Signal_Frame_t Thread__Signal_Frame_t;
 /* internal_declare_param */
void /*TypeText3*/ 
__cdecl
Thread__Signal(
   /* Param_Type1 */ Thread__Condition c_L_44);
 /* import_procedure */
/*Proc_ForwardDeclareFrameType*/struct RTHooks__AllocateTracedObj_Frame_t;typedef struct RTHooks__AllocateTracedObj_Frame_t RTHooks__AllocateTracedObj_Frame_t;
 /* internal_declare_param */
ROOT
__cdecl
RTHooks__AllocateTracedObj(
   /* Param_Type1 */ ADDRESS t_L_45);
 /* import_procedure */

#ifndef Thread__T
#define Thread__T Thread__T
typedef T4B16B0ED /*TypeText1*/  Thread__T;
#endif
/*Proc_ForwardDeclareFrameType*/struct Thread__Fork_Frame_t;typedef struct Thread__Fork_Frame_t Thread__Fork_Frame_t;
 /* internal_declare_param */
Thread__T
__cdecl
Thread__Fork(
   /* Param_Type1 */ Thread__Closure cl_L_46);
 /* end: imports */
 /* begin: locals */
 /* declare_segment name:<NIL> typeid:TFFFFFFFF const:TRUE */
/*declare_segment*/struct Main_m_47_L_48_t;
/*declare_segment*/typedef struct Main_m_47_L_48_t Main_m_47_L_48_t;
 /* declare_segment name:M_Main typeid:TFFFFFFFF const:FALSE */
 /* handler_name_prefixes:Main_M3_LINE_ */
 /* handler_name_prefixes:Main_I3_LINE_ */
/*declare_segment*/struct Main_m_M_Main_L_49_t;
/*declare_segment*/typedef struct Main_m_M_Main_L_49_t Main_m_M_Main_L_49_t;
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct Main_M3_Frame_t;typedef struct Main_M3_Frame_t Main_M3_Frame_t;
 /* internal_declare_param */
RT0__ModulePtr
__cdecl
Main_M3(
   /* Param_Type1 */ INTEGER /*TypeText1*/  mode_L_50);
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct Main__W_Frame_t;typedef struct Main__W_Frame_t Main__W_Frame_t;
 /* internal_declare_param */
void /*TypeText3*/ 
__cdecl
Main__W(
   /* Param_Type1 */ TEXT Msg_L_51);
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct Main__ThImage_Frame_t;typedef struct Main__ThImage_Frame_t Main__ThImage_Frame_t;
 /* declare_local */
 /* internal_declare_param */
TEXT
__cdecl
Main__ThImage(
   /* Param_Type1 */ Main__ThreadNo ThN_L_53);
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct Main__StateImage_Frame_t;typedef struct Main__StateImage_Frame_t Main__StateImage_Frame_t;
 /* declare_local */
 /* declare_local */
 /* internal_declare_param */
TEXT
__cdecl
Main__StateImage(
   /* Param_Type1 */ Main__State St_L_56);
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct Main__Action_Frame_t;typedef struct Main__Action_Frame_t Main__Action_Frame_t;
 /* internal_declare_param */
 /* internal_declare_param */
void /*TypeText3*/ 
__cdecl
Main__Action(
   /* Param_Type1 */ Main__ThreadNo ThN_L_57,
   /* Param_Type1 */ Main__ActionProc Apply_L_58);
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct Main__ActionWait_Frame_t;typedef struct Main__ActionWait_Frame_t Main__ActionWait_Frame_t;
 /* internal_declare_param */
 /* internal_declare_param */
void /*TypeText3*/ 
__cdecl
Main__ActionWait(
   /* Param_Type1 */ Main__ThreadNo ThN_L_59,
   /* Param_Type1 */ Main__ActionProc Apply_L_60);
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct Main__WaitForHeld_Frame_t;typedef struct Main__WaitForHeld_Frame_t Main__WaitForHeld_Frame_t;
Main__ThreadNo
__cdecl
Main__WaitForHeld(void);
 /* declare_local */
 /* declare_local */
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct Main__WaitForStateSet_Frame_t;typedef struct Main__WaitForStateSet_Frame_t Main__WaitForStateSet_Frame_t;
 /* internal_declare_param */
 /* internal_declare_param */
void /*TypeText3*/ 
__cdecl
Main__WaitForStateSet(
   /* Param_Type1 */ Main__ThreadNo ThN_L_63,
   /* Param_Type1 */ Main__StateSet Sts_L_64);
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct Main__WaitForState_Frame_t;typedef struct Main__WaitForState_Frame_t Main__WaitForState_Frame_t;
 /* internal_declare_param */
 /* internal_declare_param */
void /*TypeText3*/ 
__cdecl
Main__WaitForState(
   /* Param_Type1 */ Main__ThreadNo ThN_L_65,
   /* Param_Type1 */ Main__State St_L_66);
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct Main__NoteWhetherStateSet_Frame_t;typedef struct Main__NoteWhetherStateSet_Frame_t Main__NoteWhetherStateSet_Frame_t;
 /* declare_local */
 /* declare_local */
 /* declare_local */
 /* declare_local */
 /* internal_declare_param */
 /* internal_declare_param */
 /* internal_declare_param */
 /* internal_declare_param */
BOOLEAN
__cdecl
Main__NoteWhetherStateSet(
   /* Param_Type1 */ Main__ThreadNo ThN_L_71,
   /* Param_Type1 */ Main__StateSet Sts_L_72,
   /* Param_Type1 */ TEXT YesMsg_L_73,
   /* Param_Type1 */ TEXT NoMsg_L_74);
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct Main__NoteWhetherState_Frame_t;typedef struct Main__NoteWhetherState_Frame_t Main__NoteWhetherState_Frame_t;
 /* declare_local */
 /* internal_declare_param */
 /* internal_declare_param */
 /* internal_declare_param */
 /* internal_declare_param */
BOOLEAN
__cdecl
Main__NoteWhetherState(
   /* Param_Type1 */ Main__ThreadNo ThN_L_76,
   /* Param_Type1 */ Main__State St_L_77,
   /* Param_Type1 */ TEXT YesMsg_L_78,
   /* Param_Type1 */ TEXT NoMsg_L_79);
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct Main__TestApply_Frame_t;typedef struct Main__TestApply_Frame_t Main__TestApply_Frame_t;
 /* declare_local */
 /* declare_local */
 /* internal_declare_param */
REFANY
__cdecl
Main__TestApply(
   /* Param_Type1 */ Main__Cl Self_L_82);
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct Main__DoAcq_Frame_t;typedef struct Main__DoAcq_Frame_t Main__DoAcq_Frame_t;
 /* internal_declare_param */
void /*TypeText3*/ 
__cdecl
Main__DoAcq(
   /* Param_Type1 */ Main__ThreadNo ThN_L_83);
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct Main__DoRel_Frame_t;typedef struct Main__DoRel_Frame_t Main__DoRel_Frame_t;
 /* internal_declare_param */
void /*TypeText3*/ 
__cdecl
Main__DoRel(
   /* Param_Type1 */ Main__ThreadNo ThN_L_84);
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct Main__DoWait_Frame_t;typedef struct Main__DoWait_Frame_t Main__DoWait_Frame_t;
 /* internal_declare_param */
void /*TypeText3*/ 
__cdecl
Main__DoWait(
   /* Param_Type1 */ Main__ThreadNo ThN_L_85);
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct Main__DoSignal_Frame_t;typedef struct Main__DoSignal_Frame_t Main__DoSignal_Frame_t;
 /* internal_declare_param */
void /*TypeText3*/ 
__cdecl
Main__DoSignal(
   /* Param_Type1 */ Main__ThreadNo ThN_L_86);
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct Main__ForceSignalled_Frame_t;typedef struct Main__ForceSignalled_Frame_t Main__ForceSignalled_Frame_t;
 /* declare_local */
 /* internal_declare_param */
void /*TypeText3*/ 
__cdecl
Main__ForceSignalled(
   /* Param_Type1 */ Main__ThreadNo ThN_L_88);
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct Main__TestSeq_Frame_t;typedef struct Main__TestSeq_Frame_t Main__TestSeq_Frame_t;
void /*TypeText3*/ 
__cdecl
Main__TestSeq(void);
 /* declare_local */
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct Main__Init_Frame_t;typedef struct Main__Init_Frame_t Main__Init_Frame_t;
void /*TypeText3*/ 
__cdecl
Main__Init(void);
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
 /* declare_local */
 /* AllocateTemps_check_nil */
 /* AllocateTemps_common */
 /* declare_temp */
 /* declare_local */
 /* declare_local */
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
 /* declare_local */
 /* declare_temp */
 /* declare_local */
 /* declare_local */
 /* AllocateTemps_check_nil */
 /* AllocateTemps_common */
 /* declare_temp */
 /* declare_local */
 /* declare_local */
 /* declare_local */
 /* AllocateTemps_check_nil */
 /* AllocateTemps_common */
 /* declare_temp */
 /* declare_local */
 /* declare_temp */
 /* declare_local */
 /* declare_local */
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
 /* declare_local */
 /* declare_temp */
 /* declare_local */
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
 /* declare_temp */
 /* declare_local */
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
 /* AllocateTemps_check_nil */
 /* AllocateTemps_common */
 /* declare_temp */
 /* declare_local */
 /* declare_temp */
 /* declare_local */
 /* declare_local */
 /* declare_local */
 /* AllocateTemps_check_nil */
 /* AllocateTemps_common */
 /* declare_temp */
 /* declare_local */
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
 /* declare_local */
 /* AllocateTemps_check_nil */
 /* AllocateTemps_common */
 /* declare_temp */
 /* declare_local */
 /* declare_temp */
 /* declare_local */
 /* AllocateTemps_check_nil */
 /* AllocateTemps_common */
 /* declare_temp */
 /* declare_local */
 /* declare_temp */
 /* declare_local */
 /* declare_local */
 /* declare_local */
 /* AllocateTemps_check_nil */
 /* AllocateTemps_common */
 /* declare_temp */
 /* declare_local */
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
 /* declare_local */
 /* AllocateTemps_check_nil */
 /* AllocateTemps_common */
 /* declare_temp */
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
 /* declare_local */
 /* declare_local */
 /* AllocateTemps_check_nil */
 /* AllocateTemps_common */
 /* declare_temp */
 /* declare_local */
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
 /* declare_local */
 /* AllocateTemps_check_nil */
 /* AllocateTemps_common */
 /* declare_temp */
 /* declare_local */
 /* declare_temp */
 /* declare_local */
 /* AllocateTemps_check_nil */
 /* AllocateTemps_common */
 /* declare_temp */
 /* declare_local */
 /* declare_temp */
 /* declare_local */
 /* declare_local */
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
 /* declare_temp */
 /* declare_local */
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
 /* Locals_begin_procedure */
 /* begin_block */
 /* Locals_end_procedure */
 /* end_block */
 /* end: locals */
 /* begin: segments/globals */
 /* bind_segment */
 /* begin_init */
 /* init_int */
 /* init_var */
 /* init_int */
 /* init_chars */
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
 /* init_int */
 /* init_int */
 /* init_proc */
 /* init_chars */
 /* end_init */
struct Main_m_47_L_48_t{INT64 L_309[1];
ADDRESS L_310[1];
INT64 L_311[1];
UINT8 L_312[12];
char L_313[4];
ADDRESS L_314[5];
INT64 L_315[1];
ADDRESS L_316[1];
INT64 L_317[1];
UINT8 L_318[1];
char L_319[7];
INT64 L_320[1];
ADDRESS L_321[1];
INT64 L_322[1];
UINT8 L_323[7];
char L_324[1];
INT64 L_325[1];
ADDRESS L_326[1];
INT64 L_327[1];
UINT8 L_328[4];
char L_329[4];
INT64 L_330[1];
ADDRESS L_331[1];
INT64 L_332[1];
UINT8 L_333[4];
char L_334[4];
INT64 L_335[1];
ADDRESS L_336[1];
INT64 L_337[1];
UINT8 L_338[16];
char L_339[8];
INT64 L_340[1];
ADDRESS L_341[1];
INT64 L_342[1];
UINT8 L_343[16];
char L_344[8];
INT64 L_345[1];
ADDRESS L_346[1];
INT64 L_347[1];
UINT8 L_348[13];
char L_349[3];
INT64 L_350[1];
ADDRESS L_351[1];
INT64 L_352[1];
UINT8 L_353[14];
char L_354[2];
INT64 L_355[1];
ADDRESS L_356[1];
INT64 L_357[1];
UINT8 L_358[15];
char L_359[1];
INT64 L_360[1];
ADDRESS L_361[1];
INT64 L_362[1];
UINT8 L_363[11];
char L_364[5];
INT64 L_365[1];
ADDRESS L_366[1];
INT64 L_367[1];
UINT8 L_368[24];
char L_369[8];
INT64 L_370[1];
ADDRESS L_371[1];
INT64 L_372[1];
UINT8 L_373[13];
char L_374[3];
INT64 L_375[1];
ADDRESS L_376[1];
INT64 L_377[1];
UINT8 L_378[9];
char L_379[7];
INT64 L_380[1];
ADDRESS L_381[1];
INT64 L_382[1];
UINT8 L_383[4];
char L_384[4];
INT64 L_385[1];
ADDRESS L_387[1];
INT64 L_388[1];
UINT8 L_389[31];
char L_390[1];
INT64 L_391[1];
ADDRESS L_392[1];
INT64 L_393[1];
UINT8 L_394[20];
char L_395[4];
INT64 L_396[1];
ADDRESS L_397[1];
INT64 L_398[1];
UINT8 L_399[19];
char L_400[5];
INT64 L_401[1];
ADDRESS L_402[1];
INT64 L_403[1];
UINT8 L_404[22];
char L_405[2];
INT64 L_406[1];
ADDRESS L_407[1];
INT64 L_408[1];
UINT8 L_409[31];
char L_410[1];
INT64 L_411[1];
ADDRESS L_412[1];
INT64 L_413[1];
UINT8 L_414[20];
char L_415[4];
INT64 L_416[1];
ADDRESS L_417[1];
INT64 L_418[1];
UINT8 L_419[19];
char L_420[5];
INT64 L_421[1];
ADDRESS L_422[1];
INT64 L_423[1];
UINT8 L_424[20];
char L_425[4];
INT64 L_426[1];
ADDRESS L_427[1];
INT64 L_428[1];
UINT8 L_429[27];
char L_430[5];
INT64 L_431[1];
ADDRESS L_432[1];
INT64 L_433[1];
UINT8 L_434[52];
char L_435[4];
INT64 L_436[1];
ADDRESS L_437[1];
INT64 L_438[1];
UINT8 L_439[21];
char L_440[3];
INT64 L_441[1];
ADDRESS L_442[1];
INT64 L_443[1];
UINT8 L_444[28];
char L_445[4];
INT64 L_446[1];
ADDRESS L_447[1];
INT64 L_448[1];
UINT8 L_449[20];
char L_450[4];
INT64 L_451[1];
ADDRESS L_452[1];
INT64 L_453[1];
char L_454[8];
INT64 L_455[1];
ADDRESS L_456[1];
INT64 L_457[1];
UINT8 L_458[11];
char L_459[5];
INT64 L_460[1];
ADDRESS L_461[1];
INT64 L_462[1];
UINT8 L_463[10];
char L_464[6];
INT64 L_465[1];
ADDRESS L_466[1];
INT64 L_467[1];
UINT8 L_468[25];
char L_469[7];
INT64 L_470[1];
ADDRESS L_471[1];
INT64 L_472[1];
UINT8 L_473[64];
char L_474[8];
UINT8 L_475[7];
char L_476[1];
UINT8 L_477[4];
char L_478[1];
UINT8 L_479[7];
char L_480[1];
UINT8 L_481[14];
char L_482[1];
UINT8 L_483[8];
char L_484[1];
UINT8 L_485[6];
char L_486[1];
UINT8 L_487[5];
char L_488[1];
UINT8 L_489[5];
char L_490[1];
UINT8 L_491[9];
char L_492[1];
UINT8 L_493[16];
char L_494[1];
UINT8 L_495[19];
char L_496[1];
UINT8 L_497[12];
char L_498[1];
UINT8 L_499[15];
char L_500[1];
UINT8 L_501[11];
char L_502[1];
UINT8 L_503[10];
char L_504[1];
UINT8 L_505[6];
char L_506[1];
UINT8 L_507[10];
char L_508[1];
UINT8 L_509[7];
char L_510[1];
UINT8 L_511[1];
char L_512[2];
ADDRESS L_513[38];
char L_514[8];
INT8 L_515[15];
UINT8 L_516[1];
INT8 L_517[5];
UINT8 L_518[10];
char L_519[1];
INT8 L_520[8];
ADDRESS L_521[1];
UINT8 L_522[7];
char L_523[1];
};
static  const Main_m_47_L_48_t Main_m_47_L_48={{INT64_(-1505293580)},{24+(char*)&Main_m_47_L_48},{INT64_(0)},{'M','a','i','n','.','F','a','i','l','u','r','e'},{0 /* 1 */ ,0 /* 2 */ ,0 /* 3 */ ,0 /* 4 */ ,},{(ADDRESS)&RTHooks__TextLitInfo,(ADDRESS)&RTHooks__TextLitGetChar,(ADDRESS)&RTHooks__TextLitGetWideChar,(ADDRESS)&RTHooks__TextLitGetChars,(ADDRESS)&RTHooks__TextLitGetWideChars},{INT64_(2)},{40+(char*)&Main_m_47_L_48},{INT64_(1)},{10},{0 /* 1 */ ,0 /* 2 */ ,0 /* 3 */ ,0 /* 4 */ ,0 /* 5 */ ,0 /* 6 */ ,0 /* 7 */ ,},{INT64_(2)},{40+(char*)&Main_m_47_L_48},{INT64_(7)},{'T','h','r','e','a','d',' '},{0 /* 1 */ ,},{INT64_(2)},{40+(char*)&Main_m_47_L_48},{INT64_(4)},{'n','u','l','l'},{0 /* 1 */ ,0 /* 2 */ ,0 /* 3 */ ,0 /* 4 */ ,},{INT64_(2)},{40+(char*)&Main_m_47_L_48},{INT64_(4)},{'i','d','l','e'},{0 /* 1 */ ,0 /* 2 */ ,0 /* 3 */ ,0 /* 4 */ ,},{INT64_(2)},{40+(char*)&Main_m_47_L_48},{INT64_(16)},{'e','n','t','e','r','i','n','g',' ','A','c','q','u','i','r','e'},{0 /* 1 */ ,0 /* 2 */ ,0 /* 3 */ 
,0 /* 4 */ ,0 /* 5 */ ,0 /* 6 */ ,0 /* 7 */ ,0 /* 8 */ ,},{INT64_(2)},{40+(char*)&Main_m_47_L_48},{INT64_(16)},{'e','n','t','e','r','i','n','g',' ','R','e','l','e','a','s','e'},{0 /* 1 */ ,0 /* 2 */ ,0 /* 3 */ ,0 /* 4 */ ,0 /* 5 */ ,0 /* 6 */ ,0 /* 7 */ ,0 /* 8 */ ,},{INT64_(2)},{40+(char*)&Main_m_47_L_48},{INT64_(13)},{'e','n','t','e','r','i','n','g',' ','W','a','i','t'},{0 /* 1 */ ,0 /* 2 */ ,0 /* 3 */ ,},{INT64_(2)},{40+(char*)&Main_m_47_L_48},{INT64_(14)},{'a','s','l','e','e','p',' ','i','n',' ','W','a','i','t'},{0 /* 1 */ ,0 /* 2 */ ,},{INT64_(2)},{40+(char*)&Main_m_47_L_48},{INT64_(15)},{'e','n','t','e','r','i','n','g',' ','S','i','g','n','a','l'},{0 /* 1 */ ,},{INT64_(2)},{40+(char*)&Main_m_47_L_48},{INT64_(11)},{'W','A','c','t',' ','=',' ','N','I','L',' '},{0 /* 1 */ ,0 /* 2 */ ,0 /* 3 */ ,0 /* 4 */ ,0 /* 5 */ ,},{INT64_(2)},{40+(char*)&Main_m_47_L_48},{INT64_(24)},{' ','F','a','i','l','e','d',' ','t','o',' ','w','a','i','t',' ','i','n',' ','W','a','i','t','.'},{0 /* 1 */ ,0 /* 2 */ 
,0 /* 3 */ ,0 /* 4 */ ,0 /* 5 */ ,0 /* 6 */ ,0 /* 7 */ ,0 /* 8 */ ,},{INT64_(2)},{40+(char*)&Main_m_47_L_48},{INT64_(13)},{'H','o','l','d','e','r',' ','=',' ','T','h','N',' '},{0 /* 1 */ ,0 /* 2 */ ,0 /* 3 */ ,},{INT64_(2)},{40+(char*)&Main_m_47_L_48},{INT64_(9)},{' ','W','a','i','t','i','n','g','.'},{0 /* 1 */ ,0 /* 2 */ ,0 /* 3 */ ,0 /* 4 */ ,0 /* 5 */ ,0 /* 6 */ ,0 /* 7 */ ,},{INT64_(2)},{40+(char*)&Main_m_47_L_48},{INT64_(4)},{' ','i','s',' '},{0 /* 1 */ ,0 /* 2 */ ,0 /* 3 */ ,0 /* 4 */ ,},{INT64_(2)},{40+(char*)&Main_m_47_L_48},{INT64_(31)},{' ','E','n','t','e','r','i','n','g',' ','A','c','q','u','i','r','e',' ','o','f',' ','T','e','s','t','M','u','t','e','x','.'},{0 /* 1 */ ,},{INT64_(2)},{40+(char*)&Main_m_47_L_48},{INT64_(20)},{' ','A','c','q','u','i','r','e','d',' ','T','e','s','t','M','u','t','e','x','.'},{0 /* 1 */ ,0 /* 2 */ ,0 /* 3 */ ,0 /* 4 */ ,},{INT64_(2)},{40+(char*)&Main_m_47_L_48},{INT64_(19)},{'W','T','h','N',' ','=',' ','S','t','a','t','e',' ','.',' ','A','c','q',
' '},{0 /* 1 */ ,0 /* 2 */ ,0 /* 3 */ ,0 /* 4 */ ,0 /* 5 */ ,},{INT64_(2)},{40+(char*)&Main_m_47_L_48},{INT64_(22)},{'H','o','l','d','e','r',' ','=',' ','T','h','r','e','a','d','N','o','N','u','l','l',' '},{0 /* 1 */ ,0 /* 2 */ ,},{INT64_(2)},{40+(char*)&Main_m_47_L_48},{INT64_(31)},{' ','E','n','t','e','r','i','n','g',' ','r','e','l','e','a','s','e',' ','o','f',' ','T','e','s','t','M','u','t','e','x','.'},{0 /* 1 */ ,},{INT64_(2)},{40+(char*)&Main_m_47_L_48},{INT64_(20)},{' ','R','e','l','e','a','s','e','d',' ','T','e','s','t','M','u','t','e','x','.'},{0 /* 1 */ ,0 /* 2 */ ,0 /* 3 */ ,0 /* 4 */ ,},{INT64_(2)},{40+(char*)&Main_m_47_L_48},{INT64_(19)},{'W','T','h','N',' ','=',' ','S','t','a','t','e',' ','.',' ','R','e','l',' '},{0 /* 1 */ ,0 /* 2 */ ,0 /* 3 */ ,0 /* 4 */ ,0 /* 5 */ ,},{INT64_(2)},{40+(char*)&Main_m_47_L_48},{INT64_(20)},{'W','T','h','N',' ','=',' ','S','t','a','t','e',' ','.',' ','I','d','l','e',' '},{0 /* 1 */ ,0 /* 2 */ ,0 /* 3 */ ,0 /* 4 */ ,},{INT64_(2)},{40+(char*)&Main_m_47_L_48
},{INT64_(27)},{' ','E','n','t','e','r','i','n','g',' ','W','a','i','t',' ','o','n',' ','T','e','s','t','C','o','n','d','.'},{0 /* 1 */ ,0 /* 2 */ ,0 /* 3 */ ,0 /* 4 */ ,0 /* 5 */ ,},{INT64_(2)},{40+(char*)&Main_m_47_L_48},{INT64_(52)},{' ','W','a','s',' ','S','i','g','n','a','l','l','e','d',' ','i','n',' ','T','e','s','t','C','o','n','d',' ','a','n','d',' ','r','e','a','c','q','u','i','r','e','d',' ','T','e','s','t','M','u','t','e','x','.'},{0 /* 1 */ ,0 /* 2 */ ,0 /* 3 */ ,0 /* 4 */ ,},{INT64_(2)},{40+(char*)&Main_m_47_L_48},{INT64_(21)},{'W','T','h','N',' ','=',' ','S','t','a','t','e',' ','.',' ','W','a','i','t','2',' '},{0 /* 1 */ ,0 /* 2 */ ,0 /* 3 */ ,},{INT64_(2)},{40+(char*)&Main_m_47_L_48},{INT64_(28)},{' ','E','n','t','e','r','i','n','g',' ','S','i','g','n','a','l',' ','o','n',' ','T','e','s','t','C','o','n','d'},{0 /* 1 */ ,0 /* 2 */ ,0 /* 3 */ ,0 /* 4 */ ,},{INT64_(2)},{40+(char*)&Main_m_47_L_48},{INT64_(20)},{' ','T','e','s','t','C','o','n','d',' ','S','i','g','n','a','l',
'l','e','d',' '},{0 /* 1 */ ,0 /* 2 */ ,0 /* 3 */ ,0 /* 4 */ ,},{INT64_(2)},{40+(char*)&Main_m_47_L_48},{INT64_(0)},{0 /* 1 */ ,0 /* 2 */ ,0 /* 3 */ ,0 /* 4 */ ,0 /* 5 */ ,0 /* 6 */ ,0 /* 7 */ ,0 /* 8 */ ,},{INT64_(2)},{40+(char*)&Main_m_47_L_48},{INT64_(11)},{'L','T','h','N',' ','=',' ','T','h','N',' '},{0 /* 1 */ ,0 /* 2 */ ,0 /* 3 */ ,0 /* 4 */ ,0 /* 5 */ ,},{INT64_(2)},{40+(char*)&Main_m_47_L_48},{INT64_(10)},{'L','T','h','N','o',' ','=',' ','2',' '},{0 /* 1 */ ,0 /* 2 */ ,0 /* 3 */ ,0 /* 4 */ ,0 /* 5 */ ,0 /* 6 */ ,},{INT64_(2)},{40+(char*)&Main_m_47_L_48},{INT64_(25)},{'S','U','C','C','E','S','S',':',' ','a','l','l',' ','a','s',' ','e','x','p','e','c','t','e','d','.'},{0 /* 1 */ ,0 /* 2 */ ,0 /* 3 */ ,0 /* 4 */ ,0 /* 5 */ ,0 /* 6 */ ,0 /* 7 */ ,},{INT64_(2)},{40+(char*)&Main_m_47_L_48},{INT64_(64)},{'F','A','I','L','U','R','E',':',' ','T','h','i','s',' ','i','s',' ','t','h','e',' ','"','t','w','i','c','e',' ','u','s','e','d',' ','t','i','c','k','e','t','"',' ','b','u','g',' ','w'
,'e',' ','a','r','e',' ','t','e','s','t','i','n','g',' ','f','o','r','.'},{0 /* 1 */ ,0 /* 2 */ ,0 /* 3 */ ,0 /* 4 */ ,0 /* 5 */ ,0 /* 6 */ ,0 /* 7 */ ,0 /* 8 */ ,},{'M','a','i','n','_','M','3'},{0 /* 1 */ ,},{'I','n','i','t'},{0 /* 1 */ ,},{'T','e','s','t','S','e','q'},{0 /* 1 */ ,},{'F','o','r','c','e','S','i','g','n','a','l','l','e','d'},{0 /* 1 */ ,},{'D','o','S','i','g','n','a','l'},{0 /* 1 */ ,},{'D','o','W','a','i','t'},{0 /* 1 */ ,},{'D','o','R','e','l'},{0 /* 1 */ ,},{'D','o','A','c','q'},{0 /* 1 */ ,},{'T','e','s','t','A','p','p','l','y'},{0 /* 1 */ ,},{'N','o','t','e','W','h','e','t','h','e','r','S','t','a','t','e'},{0 /* 1 */ ,},{'N','o','t','e','W','h','e','t','h','e','r','S','t','a','t','e','S','e','t'},{0 /* 1 */ ,},{'W','a','i','t','F','o','r','S','t','a','t','e'},{0 /* 1 */ ,},{'W','a','i','t','F','o','r','S','t','a','t','e','S','e','t'},{0 /* 1 */ ,},{'W','a','i','t','F','o','r','H','e','l','d'},{0 /* 1 */ ,},{'A','c','t','i','o','n','W','a','i','t'},{0 /* 1 */ ,},{'A'
,'c','t','i','o','n'},{0 /* 1 */ ,},{'S','t','a','t','e','I','m','a','g','e'},{0 /* 1 */ ,},{'T','h','I','m','a','g','e'},{0 /* 1 */ ,},{'W'},{0 /* 1 */ ,0 /* 2 */ ,},{(ADDRESS)&Main_M3,1584+(char*)&Main_m_47_L_48,(ADDRESS)&Main__Init,1592+(char*)&Main_m_47_L_48,(ADDRESS)&Main__TestSeq,1597+(char*)&Main_m_47_L_48,(ADDRESS)&Main__ForceSignalled,1605+(char*)&Main_m_47_L_48,(ADDRESS)&Main__DoSignal,1620+(char*)&Main_m_47_L_48,(ADDRESS)&Main__DoWait,1629+(char*)&Main_m_47_L_48,(ADDRESS)&Main__DoRel,1636+(char*)&Main_m_47_L_48,(ADDRESS)&Main__DoAcq,1642+(char*)&Main_m_47_L_48,(ADDRESS)&Main__TestApply,1648+(char*)&Main_m_47_L_48,(ADDRESS)&Main__NoteWhetherState,1658+(char*)&Main_m_47_L_48,(ADDRESS)&Main__NoteWhetherStateSet,1675+(char*)&Main_m_47_L_48,(ADDRESS)&Main__WaitForState,1695+(char*)&Main_m_47_L_48,(ADDRESS)&Main__WaitForStateSet,1708+(char*)&Main_m_47_L_48,(ADDRESS)&Main__WaitForHeld,1724+(char*)&Main_m_47_L_48,(ADDRESS)&Main__ActionWait,1736+(char*)&Main_m_47_L_48,(ADDRESS)&Main__Action
,1747+(char*)&Main_m_47_L_48,(ADDRESS)&Main__StateImage,1754+(char*)&Main_m_47_L_48,(ADDRESS)&Main__ThImage,1765+(char*)&Main_m_47_L_48,(ADDRESS)&Main__W,1773+(char*)&Main_m_47_L_48},{0 /* 1 */ ,0 /* 2 */ ,0 /* 3 */ ,0 /* 4 */ ,0 /* 5 */ ,0 /* 6 */ ,0 /* 7 */ ,0 /* 8 */ ,},{((INT8)42),((INT8)104),((INT8)4),((INT8)42),((INT8)16),((INT8)4),((INT8)42),((INT8)40),((INT8)4),((INT8)4),((INT8)1),((INT8)4),((INT8)26),((INT8)5),((INT8)42)},{152U},{((INT8)1),((INT8)4),((INT8)26),((INT8)5),((INT8)0)},{'.','.','/','M','a','i','n','.','m','3'},{0 /* 1 */ ,},{((INT8)16),((INT8)0),((INT8)2),((INT8)12),((INT8)1),((INT8)21),((INT8)0),((INT8)4)},{(ADDRESS)&Main__TestApply},{'M','a','i','n','.','C','l'},{0 /* 1 */ ,}};
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
 /* init_int */
 /* init_var */
 /* init_int */
 /* init_int */
 /* end_init */
struct Main_m_M_Main_L_49_t{ADDRESS L_524[3];
char L_525[16];
ADDRESS L_526[1];
char L_527[8];
ADDRESS L_528[3];
char L_529[8];
ADDRESS L_530[1];
INT64 L_531[1];
char L_532[16];
ADDRESS L_533[1];
char L_534[112];
INT64 L_535[1];
UINT8 L_536[2];
INT8 L_537[2];
UINT8 L_538[1];
INT8 L_539[1];
UINT8 L_540[1];
INT8 L_541[3];
char L_542[1];
INT8 L_543[1];
char L_544[4];
INT64 L_545[1];
ADDRESS L_546[1];
char L_547[8];
ADDRESS L_548[1];
char L_549[16];
ADDRESS L_550[1];
char L_551[8];
INT64 L_552[1];
char L_553[24];
INT64 L_554[1];
ADDRESS L_555[1];
char L_556[56];
ADDRESS L_557[2];
char L_558[8];
ADDRESS L_559[2];
char L_560[8];
ADDRESS L_561[2];
char L_562[8];
ADDRESS L_563[2];
char L_564[8];
ADDRESS L_565[2];
char L_566[8];
ADDRESS L_567[1];
char L_568[8];
ADDRESS L_569[1];
INT64 L_570[1];
ADDRESS L_571[1];
INT64 L_572[1];
char L_573[8];
INT64 L_574[1];
char L_575[8];
};
static Main_m_M_Main_L_49_t Main_m_M_Main_L_49={{2109+(char*)&Main_m_47_L_48,232+(char*)&Main_m_M_Main_L_49,568+(char*)&Main_m_M_Main_L_49},{0 /* 1 */ ,0 /* 2 */ ,0 /* 3 */ ,0 /* 4 */ ,0 /* 5 */ ,0 /* 6 */ ,0 /* 7 */ ,0 /* 8 */ ,0 /* 9 */ ,0 /* 10 */ ,0 /* 11 */ ,0 /* 12 */ ,0 /* 13 */ ,0 /* 14 */ ,0 /* 15 */ ,0 /* 16 */ ,},{1776+(char*)&Main_m_47_L_48},{0 /* 1 */ ,0 /* 2 */ ,0 /* 3 */ ,0 /* 4 */ ,0 /* 5 */ ,0 /* 6 */ ,0 /* 7 */ ,0 /* 8 */ ,},{2088+(char*)&Main_m_47_L_48,2088+(char*)&Main_m_47_L_48,424+(char*)&Main_m_M_Main_L_49},{0 /* 1 */ ,0 /* 2 */ ,0 /* 3 */ ,0 /* 4 */ ,0 /* 5 */ ,0 /* 6 */ ,0 /* 7 */ ,0 /* 8 */ ,},{(ADDRESS)&Main_M3},{INT64_(3)},{0 /* 1 */ ,0 /* 2 */ ,0 /* 3 */ ,0 /* 4 */ ,0 /* 5 */ ,0 /* 6 */ ,0 /* 7 */ ,0 /* 8 */ ,0 /* 9 */ ,0 /* 10 */ ,0 /* 11 */ ,0 /* 12 */ ,0 /* 13 */ ,0 /* 14 */ ,0 /* 15 */ ,0 /* 16 */ ,},{(char*)&Main_m_47_L_48},{0 /* 1 */ ,0 /* 2 */ ,0 /* 3 */ ,0 /* 4 */ ,0 /* 5 */ ,0 /* 6 */ ,0 /* 7 */ ,0 /* 8 */ ,0 /* 9 */ ,0 /* 10 */ ,0 /* 11 */ ,0 /* 12 */ 
,0 /* 13 */ ,0 /* 14 */ ,0 /* 15 */ ,0 /* 16 */ ,0 /* 17 */ ,0 /* 18 */ ,0 /* 19 */ ,0 /* 20 */ ,0 /* 21 */ ,0 /* 22 */ ,0 /* 23 */ ,0 /* 24 */ ,0 /* 25 */ ,0 /* 26 */ ,0 /* 27 */ ,0 /* 28 */ ,0 /* 29 */ ,0 /* 30 */ ,0 /* 31 */ ,0 /* 32 */ ,0 /* 33 */ ,0 /* 34 */ ,0 /* 35 */ ,0 /* 36 */ ,0 /* 37 */ ,0 /* 38 */ ,0 /* 39 */ ,0 /* 40 */ ,0 /* 41 */ ,0 /* 42 */ ,0 /* 43 */ ,0 /* 44 */ ,0 /* 45 */ ,0 /* 46 */ ,0 /* 47 */ ,0 /* 48 */ ,0 /* 49 */ ,0 /* 50 */ ,0 /* 51 */ ,0 /* 52 */ ,0 /* 53 */ ,0 /* 54 */ ,0 /* 55 */ ,0 /* 56 */ ,0 /* 57 */ ,0 /* 58 */ ,0 /* 59 */ ,0 /* 60 */ ,0 /* 61 */ ,0 /* 62 */ ,0 /* 63 */ ,0 /* 64 */ ,0 /* 65 */ ,0 /* 66 */ ,0 /* 67 */ ,0 /* 68 */ ,0 /* 69 */ ,0 /* 70 */ ,0 /* 71 */ ,0 /* 72 */ ,0 /* 73 */ ,0 /* 74 */ ,0 /* 75 */ ,0 /* 76 */ ,0 /* 77 */ ,0 /* 78 */ ,0 /* 79 */ ,0 /* 80 */ ,0 /* 81 */ ,0 /* 82 */ ,0 /* 83 */ ,0 /* 84 */ ,0 /* 85 */ ,0 /* 86 */ ,0 /* 87 */ ,0 /* 88 */ ,0 /* 89 */ ,0 /* 90 */ ,0 /* 91 */ ,0 /* 92 */ ,0 /* 93 */ ,0 /* 94 */ ,0 /* 95 */ ,0 /* 96 */ 
,0 /* 97 */ ,0 /* 98 */ ,0 /* 99 */ ,0 /* 100 */ ,0 /* 101 */ ,0 /* 102 */ ,0 /* 103 */ ,0 /* 104 */ ,0 /* 105 */ ,0 /* 106 */ ,0 /* 107 */ ,0 /* 108 */ ,0 /* 109 */ ,0 /* 110 */ ,0 /* 111 */ ,0 /* 112 */ ,},{INT64_(1940822287)},{149U,140U},{((INT8)81),((INT8)127)},{154U},{((INT8)21)},{255U},{((INT8)12),((INT8)1),((INT8)2)},{0 /* 1 */ ,},{((INT8)8)},{0 /* 1 */ ,0 /* 2 */ ,0 /* 3 */ ,0 /* 4 */ ,},{INT64_(8)},{2120+(char*)&Main_m_47_L_48},{0 /* 1 */ ,0 /* 2 */ ,0 /* 3 */ ,0 /* 4 */ ,0 /* 5 */ ,0 /* 6 */ ,0 /* 7 */ ,0 /* 8 */ ,},{2122+(char*)&Main_m_47_L_48},{0 /* 1 */ ,0 /* 2 */ ,0 /* 3 */ ,0 /* 4 */ ,0 /* 5 */ ,0 /* 6 */ ,0 /* 7 */ ,0 /* 8 */ ,0 /* 9 */ ,0 /* 10 */ ,0 /* 11 */ ,0 /* 12 */ ,0 /* 13 */ ,0 /* 14 */ ,0 /* 15 */ ,0 /* 16 */ ,},{2136+(char*)&Main_m_47_L_48},{0 /* 1 */ ,0 /* 2 */ ,0 /* 3 */ ,0 /* 4 */ ,0 /* 5 */ ,0 /* 6 */ ,0 /* 7 */ ,0 /* 8 */ ,},{INT64_(-448425059)},{0 /* 1 */ ,0 /* 2 */ ,0 /* 3 */ ,0 /* 4 */ ,0 /* 5 */ ,0 /* 6 */ ,0 /* 7 */ ,0 /* 8 */ ,0 /* 9 */ ,0 /* 10 */ 
,0 /* 11 */ ,0 /* 12 */ ,0 /* 13 */ ,0 /* 14 */ ,0 /* 15 */ ,0 /* 16 */ ,0 /* 17 */ ,0 /* 18 */ ,0 /* 19 */ ,0 /* 20 */ ,0 /* 21 */ ,0 /* 22 */ ,0 /* 23 */ ,0 /* 24 */ ,},{INT64_(0)},{2128+(char*)&Main_m_47_L_48},{0 /* 1 */ ,0 /* 2 */ ,0 /* 3 */ ,0 /* 4 */ ,0 /* 5 */ ,0 /* 6 */ ,0 /* 7 */ ,0 /* 8 */ ,0 /* 9 */ ,0 /* 10 */ ,0 /* 11 */ ,0 /* 12 */ ,0 /* 13 */ ,0 /* 14 */ ,0 /* 15 */ ,0 /* 16 */ ,0 /* 17 */ ,0 /* 18 */ ,0 /* 19 */ ,0 /* 20 */ ,0 /* 21 */ ,0 /* 22 */ ,0 /* 23 */ ,0 /* 24 */ ,0 /* 25 */ ,0 /* 26 */ ,0 /* 27 */ ,0 /* 28 */ ,0 /* 29 */ ,0 /* 30 */ ,0 /* 31 */ ,0 /* 32 */ ,0 /* 33 */ ,0 /* 34 */ ,0 /* 35 */ ,0 /* 36 */ ,0 /* 37 */ ,0 /* 38 */ ,0 /* 39 */ ,0 /* 40 */ ,0 /* 41 */ ,0 /* 42 */ ,0 /* 43 */ ,0 /* 44 */ ,0 /* 45 */ ,0 /* 46 */ ,0 /* 47 */ ,0 /* 48 */ ,0 /* 49 */ ,0 /* 50 */ ,0 /* 51 */ ,0 /* 52 */ ,0 /* 53 */ ,0 /* 54 */ ,0 /* 55 */ ,0 /* 56 */ ,},{(ADDRESS)&Main_I3,448+(char*)&Main_m_M_Main_L_49},{0 /* 1 */ ,0 /* 2 */ ,0 /* 3 */ ,0 /* 4 */ ,0 /* 5 */ ,0 /* 6 */ ,0 /* 7 */ 
,0 /* 8 */ ,},{(ADDRESS)&Wr_I3,472+(char*)&Main_m_M_Main_L_49},{0 /* 1 */ ,0 /* 2 */ ,0 /* 3 */ ,0 /* 4 */ ,0 /* 5 */ ,0 /* 6 */ ,0 /* 7 */ ,0 /* 8 */ ,},{(ADDRESS)&Thread_I3,496+(char*)&Main_m_M_Main_L_49},{0 /* 1 */ ,0 /* 2 */ ,0 /* 3 */ ,0 /* 4 */ ,0 /* 5 */ ,0 /* 6 */ ,0 /* 7 */ ,0 /* 8 */ ,},{(ADDRESS)&Stdio_I3,520+(char*)&Main_m_M_Main_L_49},{0 /* 1 */ ,0 /* 2 */ ,0 /* 3 */ ,0 /* 4 */ ,0 /* 5 */ ,0 /* 6 */ ,0 /* 7 */ ,0 /* 8 */ ,},{(ADDRESS)&Fmt_I3,544+(char*)&Main_m_M_Main_L_49},{0 /* 1 */ ,0 /* 2 */ ,0 /* 3 */ ,0 /* 4 */ ,0 /* 5 */ ,0 /* 6 */ ,0 /* 7 */ ,0 /* 8 */ ,},{(ADDRESS)&RTHooks_I3},{0 /* 1 */ ,0 /* 2 */ ,0 /* 3 */ ,0 /* 4 */ ,0 /* 5 */ ,0 /* 6 */ ,0 /* 7 */ ,0 /* 8 */ ,},{584+(char*)&Main_m_M_Main_L_49},{INT64_(356643957)},{600+(char*)&Main_m_M_Main_L_49},{INT64_(1475387830)},{0 /* 1 */ ,0 /* 2 */ ,0 /* 3 */ ,0 /* 4 */ ,0 /* 5 */ ,0 /* 6 */ ,0 /* 7 */ ,0 /* 8 */ ,},{INT64_(1940822287)},{0 /* 1 */ ,0 /* 2 */ ,0 /* 3 */ ,0 /* 4 */ ,0 /* 5 */ ,0 /* 6 */ ,0 /* 7 */ ,0 /* 8 */ 
,}};
static void __cdecl Main_m_M_Main_L_49_CRASH(WORD_T code) M3_ATTRIBUTE_NO_RETURN;
static void __cdecl Main_m_M_Main_L_49_CRASH(WORD_T code){RTHooks__ReportFault((ADDRESS)&Main_m_M_Main_L_49,code);} /* end: segments/globals */
 /* begin: mark used */
 /* end: mark used */
 /* set_source_file */
 /* set_source_line */
#line 9 "../Main.m3"
 /* module global constants */
#line 9 "../Main.m3"
 /* module global data */
#line 9 "../Main.m3"
 /* set_source_line */
#line 9 "../Main.m3"
#line 426 "../Main.m3"
 /* W */
#line 426 "../Main.m3"
 /* set_source_line */
#line 426 "../Main.m3"
#line 16 "../Main.m3"
 /* begin_procedure */
#line 16 "../Main.m3"
struct Main__W_Frame_t {
#line 16 "../Main.m3"
ADDRESS _unused;
#line 16 "../Main.m3"
};
#line 16 "../Main.m3"
void /*TypeText3*/ 
__cdecl
Main__W(
   /* Param_Type1 */ TEXT Msg_L_51)
{
#line 16 "../Main.m3"
 /* Var_Type2 */ ADDRESS Main_m_90_L_91={0};//always-init
#line 16 "../Main.m3"
Main__W_Frame_t _frame;
#line 16 "../Main.m3"
_frame._unused=(ADDRESS)&_frame;
#line 16 "../Main.m3"
 /* set_label */
#line 16 "../Main.m3"
 /* set_source_line */
#line 16 "../Main.m3"
#line 18 "../Main.m3"
 /* set_source_line */
#line 18 "../Main.m3"
#line 21 "../Main.m3"
 /* load */
#line 21 "../Main.m3"
 /* load_indirect */
#line 21 "../Main.m3"
 /* store */
#line 21 "../Main.m3"
(*(ADDRESS*)(&Main_m_90_L_91))=(ADDRESS)(((ADDRESS)(*((ADDRESS*)(INT64_(112)+((ADDRESS)(*((ADDRESS*)(INT64_(496)+((ADDRESS)(&Main_m_M_Main_L_49)))))))))));
#line 21 "../Main.m3"
 /* load_nil */
#line 21 "../Main.m3"
 /* load */
#line 21 "../Main.m3"
 /* if_compare */
#line 21 "../Main.m3"
if(m3_eq(ADDRESS,
 ((ADDRESS)(0)),
 ((ADDRESS)(Main_m_90_L_91))))goto L3;
#line 21 "../Main.m3"
 /* load */
#line 21 "../Main.m3"
 /* loophole */
#line 21 "../Main.m3"
 /* load_integer */
#line 21 "../Main.m3"
 /* and */
#line 21 "../Main.m3"
 /* if_true_or_false */
#line 21 "../Main.m3"
 /* load_host_integer */
#line 21 "../Main.m3"
 /* load_integer */
#line 21 "../Main.m3"
 /* if_compare */
#line 21 "../Main.m3"
if(m3_ne(INT64,
  ((INT64)(((INT64)((INT64)Main_m_90_L_91))&  INT64_(1))),
   INT64_(0)))goto L3;
#line 21 "../Main.m3"
 /* load */
#line 21 "../Main.m3"
 /* load_indirect */
#line 21 "../Main.m3"
 /* extract_mn */
#line 21 "../Main.m3"
 /* load_host_integer */
#line 21 "../Main.m3"
 /* load_integer */
#line 21 "../Main.m3"
 /* load_host_integer */
#line 21 "../Main.m3"
 /* load_integer */
#line 21 "../Main.m3"
 /* extract */
#line 21 "../Main.m3"
 /* if_true_or_false */
#line 21 "../Main.m3"
 /* load_host_integer */
#line 21 "../Main.m3"
 /* load_integer */
#line 21 "../Main.m3"
 /* if_compare */
#line 21 "../Main.m3"
if(m3_eq(INT64,
 ((INT64)(m3_extract(
 UINT64,
  *((INT64*)(INT64_(-8)+((ADDRESS)(Main_m_90_L_91)))),
   UINT64_(22),
   UINT64_(1)))),
   INT64_(0)))goto L3;
#line 21 "../Main.m3"
 /* start_call_direct */
#line 21 "../Main.m3"
 /* load */
#line 21 "../Main.m3"
 /* pop_param */
#line 21 "../Main.m3"
 /* call_direct */
#line 21 "../Main.m3"
RTHooks__CheckLoadTracedRef(
  ( REFANY )(((ADDRESS)(Main_m_90_L_91)) ));
#line 21 "../Main.m3"
 /* set_label */
#line 21 "../Main.m3"
L3:;
#line 21 "../Main.m3"
 /* start_call_direct */
#line 21 "../Main.m3"
 /* load */
#line 21 "../Main.m3"
 /* pop_param */
#line 21 "../Main.m3"
 /* load */
#line 21 "../Main.m3"
 /* pop_param */
#line 21 "../Main.m3"
 /* call_direct */
#line 21 "../Main.m3"
Wr__PutText(
  ( Wr__T )(((ADDRESS)(Main_m_90_L_91)) ),
  ( TEXT )(((ADDRESS)(Msg_L_51)) ));
#line 21 "../Main.m3"
 /* set_source_line */
#line 21 "../Main.m3"
#line 22 "../Main.m3"
 /* load */
#line 22 "../Main.m3"
 /* load_indirect */
#line 22 "../Main.m3"
 /* store */
#line 22 "../Main.m3"
(*(ADDRESS*)(&Main_m_90_L_91))=(ADDRESS)(((ADDRESS)(*((ADDRESS*)(INT64_(112)+((ADDRESS)(*((ADDRESS*)(INT64_(496)+((ADDRESS)(&Main_m_M_Main_L_49)))))))))));
#line 22 "../Main.m3"
 /* load_nil */
#line 22 "../Main.m3"
 /* load */
#line 22 "../Main.m3"
 /* if_compare */
#line 22 "../Main.m3"
if(m3_eq(ADDRESS,
 ((ADDRESS)(0)),
 ((ADDRESS)(Main_m_90_L_91))))goto L4;
#line 22 "../Main.m3"
 /* load */
#line 22 "../Main.m3"
 /* loophole */
#line 22 "../Main.m3"
 /* load_integer */
#line 22 "../Main.m3"
 /* and */
#line 22 "../Main.m3"
 /* if_true_or_false */
#line 22 "../Main.m3"
 /* load_host_integer */
#line 22 "../Main.m3"
 /* load_integer */
#line 22 "../Main.m3"
 /* if_compare */
#line 22 "../Main.m3"
if(m3_ne(INT64,
  ((INT64)(((INT64)((INT64)Main_m_90_L_91))&  INT64_(1))),
   INT64_(0)))goto L4;
#line 22 "../Main.m3"
 /* load */
#line 22 "../Main.m3"
 /* load_indirect */
#line 22 "../Main.m3"
 /* extract_mn */
#line 22 "../Main.m3"
 /* load_host_integer */
#line 22 "../Main.m3"
 /* load_integer */
#line 22 "../Main.m3"
 /* load_host_integer */
#line 22 "../Main.m3"
 /* load_integer */
#line 22 "../Main.m3"
 /* extract */
#line 22 "../Main.m3"
 /* if_true_or_false */
#line 22 "../Main.m3"
 /* load_host_integer */
#line 22 "../Main.m3"
 /* load_integer */
#line 22 "../Main.m3"
 /* if_compare */
#line 22 "../Main.m3"
if(m3_eq(INT64,
 ((INT64)(m3_extract(
 UINT64,
  *((INT64*)(INT64_(-8)+((ADDRESS)(Main_m_90_L_91)))),
   UINT64_(22),
   UINT64_(1)))),
   INT64_(0)))goto L4;
#line 22 "../Main.m3"
 /* start_call_direct */
#line 22 "../Main.m3"
 /* load */
#line 22 "../Main.m3"
 /* pop_param */
#line 22 "../Main.m3"
 /* call_direct */
#line 22 "../Main.m3"
RTHooks__CheckLoadTracedRef(
  ( REFANY )(((ADDRESS)(Main_m_90_L_91)) ));
#line 22 "../Main.m3"
 /* set_label */
#line 22 "../Main.m3"
L4:;
#line 22 "../Main.m3"
 /* start_call_direct */
#line 22 "../Main.m3"
 /* load */
#line 22 "../Main.m3"
 /* pop_param */
#line 22 "../Main.m3"
 /* load_address */
#line 22 "../Main.m3"
 /* pop_param */
#line 22 "../Main.m3"
 /* call_direct */
#line 22 "../Main.m3"
Wr__PutText(
  ( Wr__T )(((ADDRESS)(Main_m_90_L_91)) ),
  ( TEXT )(((ADDRESS)(INT64_(88)+((ADDRESS)(&Main_m_47_L_48)))) ));
#line 22 "../Main.m3"
 /* set_source_line */
#line 22 "../Main.m3"
#line 23 "../Main.m3"
 /* load */
#line 23 "../Main.m3"
 /* load_indirect */
#line 23 "../Main.m3"
 /* store */
#line 23 "../Main.m3"
(*(ADDRESS*)(&Main_m_90_L_91))=(ADDRESS)(((ADDRESS)(*((ADDRESS*)(INT64_(112)+((ADDRESS)(*((ADDRESS*)(INT64_(496)+((ADDRESS)(&Main_m_M_Main_L_49)))))))))));
#line 23 "../Main.m3"
 /* load_nil */
#line 23 "../Main.m3"
 /* load */
#line 23 "../Main.m3"
 /* if_compare */
#line 23 "../Main.m3"
if(m3_eq(ADDRESS,
 ((ADDRESS)(0)),
 ((ADDRESS)(Main_m_90_L_91))))goto L5;
#line 23 "../Main.m3"
 /* load */
#line 23 "../Main.m3"
 /* loophole */
#line 23 "../Main.m3"
 /* load_integer */
#line 23 "../Main.m3"
 /* and */
#line 23 "../Main.m3"
 /* if_true_or_false */
#line 23 "../Main.m3"
 /* load_host_integer */
#line 23 "../Main.m3"
 /* load_integer */
#line 23 "../Main.m3"
 /* if_compare */
#line 23 "../Main.m3"
if(m3_ne(INT64,
  ((INT64)(((INT64)((INT64)Main_m_90_L_91))&  INT64_(1))),
   INT64_(0)))goto L5;
#line 23 "../Main.m3"
 /* load */
#line 23 "../Main.m3"
 /* load_indirect */
#line 23 "../Main.m3"
 /* extract_mn */
#line 23 "../Main.m3"
 /* load_host_integer */
#line 23 "../Main.m3"
 /* load_integer */
#line 23 "../Main.m3"
 /* load_host_integer */
#line 23 "../Main.m3"
 /* load_integer */
#line 23 "../Main.m3"
 /* extract */
#line 23 "../Main.m3"
 /* if_true_or_false */
#line 23 "../Main.m3"
 /* load_host_integer */
#line 23 "../Main.m3"
 /* load_integer */
#line 23 "../Main.m3"
 /* if_compare */
#line 23 "../Main.m3"
if(m3_eq(INT64,
 ((INT64)(m3_extract(
 UINT64,
  *((INT64*)(INT64_(-8)+((ADDRESS)(Main_m_90_L_91)))),
   UINT64_(22),
   UINT64_(1)))),
   INT64_(0)))goto L5;
#line 23 "../Main.m3"
 /* start_call_direct */
#line 23 "../Main.m3"
 /* load */
#line 23 "../Main.m3"
 /* pop_param */
#line 23 "../Main.m3"
 /* call_direct */
#line 23 "../Main.m3"
RTHooks__CheckLoadTracedRef(
  ( REFANY )(((ADDRESS)(Main_m_90_L_91)) ));
#line 23 "../Main.m3"
 /* set_label */
#line 23 "../Main.m3"
L5:;
#line 23 "../Main.m3"
 /* start_call_direct */
#line 23 "../Main.m3"
 /* load */
#line 23 "../Main.m3"
 /* pop_param */
#line 23 "../Main.m3"
 /* call_direct */
#line 23 "../Main.m3"
Wr__Flush(
  ( Wr__T )(((ADDRESS)(Main_m_90_L_91)) ));
#line 23 "../Main.m3"
 /* set_label */
#line 23 "../Main.m3"
 /* set_source_line */
#line 23 "../Main.m3"
#line 24 "../Main.m3"
 /* exit_proc */
#line 24 "../Main.m3"
return;
#line 24 "../Main.m3"
 /* end_procedure */
#line 24 "../Main.m3"
} /* ThImage */
#line 24 "../Main.m3"
 /* set_source_line */
#line 24 "../Main.m3"
#line 29 "../Main.m3"
 /* begin_procedure */
#line 29 "../Main.m3"
struct Main__ThImage_Frame_t {
#line 29 "../Main.m3"
ADDRESS _unused;
#line 29 "../Main.m3"
};
#line 29 "../Main.m3"
TEXT
__cdecl
Main__ThImage(
   /* Param_Type1 */ Main__ThreadNo ThN_L_53)
{
#line 29 "../Main.m3"
 /* Var_Type2 */ ADDRESS Main_m_92_L_93={0};//always-init
#line 29 "../Main.m3"
 /* Var_Type2 */ ADDRESS Main_m_94_L_95={0};//always-init
#line 29 "../Main.m3"
Main__ThImage_Frame_t _frame;
#line 29 "../Main.m3"
_frame._unused=(ADDRESS)&_frame;
#line 29 "../Main.m3"
 /* set_source_line */
#line 29 "../Main.m3"
#line 30 "../Main.m3"
 /* set_source_line */
#line 30 "../Main.m3"
#line 31 "../Main.m3"
 /* start_call_direct */
#line 31 "../Main.m3"
 /* load */
#line 31 "../Main.m3"
 /* pop_param */
#line 31 "../Main.m3"
 /* load_integer */
#line 31 "../Main.m3"
 /* pop_param */
#line 31 "../Main.m3"
 /* call_direct */
#line 31 "../Main.m3"
 /* store */
#line 31 "../Main.m3"
(*(ADDRESS*)(&Main_m_92_L_93))=(ADDRESS)(((ADDRESS)(Fmt__Int(
  ( INTEGER )( ((INT64)(ThN_L_53)) ),
  ( Fmt__Base )(((UINT8)( INT64_(10))) )))));
#line 31 "../Main.m3"
 /* start_call_direct */
#line 31 "../Main.m3"
 /* load_address */
#line 31 "../Main.m3"
 /* pop_param */
#line 31 "../Main.m3"
 /* load */
#line 31 "../Main.m3"
 /* pop_param */
#line 31 "../Main.m3"
 /* call_direct */
#line 31 "../Main.m3"
 /* store */
#line 31 "../Main.m3"
(*(ADDRESS*)(&Main_m_94_L_95))=(ADDRESS)(((ADDRESS)(RTHooks__Concat(
  ( TEXT )(((ADDRESS)(INT64_(120)+((ADDRESS)(&Main_m_47_L_48)))) ),
  ( TEXT )(((ADDRESS)(Main_m_92_L_93)) )))));
#line 31 "../Main.m3"
 /* load */
#line 31 "../Main.m3"
 /* exit_proc */
#line 31 "../Main.m3"
return (TEXT)(Main_m_94_L_95);
#line 31 "../Main.m3"
 /* end_procedure */
#line 31 "../Main.m3"
} /* StateImage */
#line 31 "../Main.m3"
 /* set_source_line */
#line 31 "../Main.m3"
#line 52 "../Main.m3"
 /* begin_procedure */
#line 52 "../Main.m3"
struct Main__StateImage_Frame_t {
#line 52 "../Main.m3"
ADDRESS _unused;
#line 52 "../Main.m3"
};
#line 52 "../Main.m3"
TEXT
__cdecl
Main__StateImage(
   /* Param_Type1 */ Main__State St_L_56)
{
#line 52 "../Main.m3"
 /* Var_Type1 */ TEXT LResult_L_54={0};//always-init
#line 52 "../Main.m3"
Main__StateImage_Frame_t _frame;
#line 52 "../Main.m3"
_frame._unused=(ADDRESS)&_frame;
#line 52 "../Main.m3"
 /* set_source_line */
#line 52 "../Main.m3"
#line 54 "../Main.m3"
 /* load_nil */
#line 54 "../Main.m3"
 /* store */
#line 54 "../Main.m3"
(*(ADDRESS*)(&LResult_L_54))=(ADDRESS)(((ADDRESS)(0)));
#line 54 "../Main.m3"
 /* set_source_line */
#line 54 "../Main.m3"
#line 57 "../Main.m3"
 /* load */
#line 57 "../Main.m3"
 /* case_jump */
#line 57 "../Main.m3"
switch( ((INT64)(St_L_56))){
#line 57 "../Main.m3"
case 0:goto L6;
#line 57 "../Main.m3"
case 1:goto L7;
#line 57 "../Main.m3"
case 2:goto L8;
#line 57 "../Main.m3"
case 3:goto L9;
#line 57 "../Main.m3"
case 4:goto LA;
#line 57 "../Main.m3"
case 5:goto LB;
#line 57 "../Main.m3"
case 6:goto LC;
#line 57 "../Main.m3"
} /* set_label */
#line 57 "../Main.m3"
L6:;
#line 57 "../Main.m3"
 /* set_source_line */
#line 57 "../Main.m3"
#line 58 "../Main.m3"
 /* load_address */
#line 58 "../Main.m3"
 /* store */
#line 58 "../Main.m3"
(*(ADDRESS*)(&LResult_L_54))=(ADDRESS)(((ADDRESS)(INT64_(152)+((ADDRESS)(&Main_m_47_L_48)))));
#line 58 "../Main.m3"
 /* jump */
#line 58 "../Main.m3"
goto LE;
#line 58 "../Main.m3"
 /* set_label */
#line 58 "../Main.m3"
L7:;
#line 58 "../Main.m3"
 /* set_source_line */
#line 58 "../Main.m3"
#line 59 "../Main.m3"
 /* load_address */
#line 59 "../Main.m3"
 /* store */
#line 59 "../Main.m3"
(*(ADDRESS*)(&LResult_L_54))=(ADDRESS)(((ADDRESS)(INT64_(184)+((ADDRESS)(&Main_m_47_L_48)))));
#line 59 "../Main.m3"
 /* jump */
#line 59 "../Main.m3"
goto LE;
#line 59 "../Main.m3"
 /* set_label */
#line 59 "../Main.m3"
L8:;
#line 59 "../Main.m3"
 /* set_source_line */
#line 59 "../Main.m3"
#line 60 "../Main.m3"
 /* load_address */
#line 60 "../Main.m3"
 /* store */
#line 60 "../Main.m3"
(*(ADDRESS*)(&LResult_L_54))=(ADDRESS)(((ADDRESS)(INT64_(216)+((ADDRESS)(&Main_m_47_L_48)))));
#line 60 "../Main.m3"
 /* jump */
#line 60 "../Main.m3"
goto LE;
#line 60 "../Main.m3"
 /* set_label */
#line 60 "../Main.m3"
L9:;
#line 60 "../Main.m3"
 /* set_source_line */
#line 60 "../Main.m3"
#line 61 "../Main.m3"
 /* load_address */
#line 61 "../Main.m3"
 /* store */
#line 61 "../Main.m3"
(*(ADDRESS*)(&LResult_L_54))=(ADDRESS)(((ADDRESS)(INT64_(264)+((ADDRESS)(&Main_m_47_L_48)))));
#line 61 "../Main.m3"
 /* jump */
#line 61 "../Main.m3"
goto LE;
#line 61 "../Main.m3"
 /* set_label */
#line 61 "../Main.m3"
LA:;
#line 61 "../Main.m3"
 /* set_source_line */
#line 61 "../Main.m3"
#line 62 "../Main.m3"
 /* load_address */
#line 62 "../Main.m3"
 /* store */
#line 62 "../Main.m3"
(*(ADDRESS*)(&LResult_L_54))=(ADDRESS)(((ADDRESS)(INT64_(312)+((ADDRESS)(&Main_m_47_L_48)))));
#line 62 "../Main.m3"
 /* jump */
#line 62 "../Main.m3"
goto LE;
#line 62 "../Main.m3"
 /* set_label */
#line 62 "../Main.m3"
LB:;
#line 62 "../Main.m3"
 /* set_source_line */
#line 62 "../Main.m3"
#line 63 "../Main.m3"
 /* load_address */
#line 63 "../Main.m3"
 /* store */
#line 63 "../Main.m3"
(*(ADDRESS*)(&LResult_L_54))=(ADDRESS)(((ADDRESS)(INT64_(352)+((ADDRESS)(&Main_m_47_L_48)))));
#line 63 "../Main.m3"
 /* jump */
#line 63 "../Main.m3"
goto LE;
#line 63 "../Main.m3"
 /* set_label */
#line 63 "../Main.m3"
LC:;
#line 63 "../Main.m3"
 /* set_source_line */
#line 63 "../Main.m3"
#line 64 "../Main.m3"
 /* load_address */
#line 64 "../Main.m3"
 /* store */
#line 64 "../Main.m3"
(*(ADDRESS*)(&LResult_L_54))=(ADDRESS)(((ADDRESS)(INT64_(392)+((ADDRESS)(&Main_m_47_L_48)))));
#line 64 "../Main.m3"
 /* jump */
#line 64 "../Main.m3"
goto LE;
#line 64 "../Main.m3"
 /* set_label */
#line 64 "../Main.m3"
 /* set_label */
#line 64 "../Main.m3"
LE:;
#line 64 "../Main.m3"
 /* set_source_line */
#line 64 "../Main.m3"
#line 66 "../Main.m3"
 /* load */
#line 66 "../Main.m3"
 /* exit_proc */
#line 66 "../Main.m3"
return (TEXT)(LResult_L_54);
#line 66 "../Main.m3"
 /* end_procedure */
#line 66 "../Main.m3"
} /* Action */
#line 66 "../Main.m3"
 /* set_source_line */
#line 66 "../Main.m3"
#line 76 "../Main.m3"
 /* begin_procedure */
#line 76 "../Main.m3"
struct Main__Action_Frame_t {
#line 76 "../Main.m3"
ADDRESS _unused;
#line 76 "../Main.m3"
};
#line 76 "../Main.m3"
void /*TypeText3*/ 
__cdecl
Main__Action(
   /* Param_Type1 */ Main__ThreadNo ThN_L_57,
   /* Param_Type1 */ Main__ActionProc Apply_L_58)
{
#line 76 "../Main.m3"
 /* Var_Type2 */ ADDRESS Main_m_96_L_97={0};//always-init
#line 76 "../Main.m3"
 /* Var_Type2 */ ADDRESS Main_m_98_L_99={0};//always-init
#line 76 "../Main.m3"
 /* Var_Type1 */ MUTEX Main_m_100_L_101={0};//always-init
#line 76 "../Main.m3"
 /* Var_Type2 */ ADDRESS Main_m_102_L_103={0};//always-init
#line 76 "../Main.m3"
 /* Var_Type2 */ INT64 Main_m_104_L_105={0};//always-init
#line 76 "../Main.m3"
 /* Var_Type1 */ T1B64A79D* WAct_L_106={0};//always-init
#line 76 "../Main.m3"
 /* Var_Type2 */ ADDRESS Main_m_107_L_108={0};//always-init
#line 76 "../Main.m3"
 /* Var_Type2 */ ADDRESS Main_m_109_L_110={0};//always-init
#line 76 "../Main.m3"
 /* Var_Type1 */ MUTEX Main_m_111_L_112={0};//always-init
#line 76 "../Main.m3"
 /* Var_Type2 */ ADDRESS Main_m_113_L_114={0};//always-init
#line 76 "../Main.m3"
 /* Var_Type2 */ ADDRESS Main_m_115_L_116={0};//always-init
#line 76 "../Main.m3"
Main__Action_Frame_t _frame;
#line 76 "../Main.m3"
_frame._unused=(ADDRESS)&_frame;
#line 76 "../Main.m3"
 /* set_source_line */
#line 76 "../Main.m3"
#line 79 "../Main.m3"
 /* set_source_line */
#line 79 "../Main.m3"
#line 80 "../Main.m3"
 /* load_nil */
#line 80 "../Main.m3"
 /* store */
#line 80 "../Main.m3"
(*(ADDRESS*)(&Main_m_96_L_97))=(ADDRESS)(((ADDRESS)(0)));
#line 80 "../Main.m3"
 /* load */
#line 80 "../Main.m3"
 /* store */
#line 80 "../Main.m3"
(*(ADDRESS*)(&Main_m_98_L_99))=(ADDRESS)(((ADDRESS)(*((ADDRESS*)(INT64_(128)+((ADDRESS)(&Main_m_M_Main_L_49)))))));
#line 80 "../Main.m3"
 /* load_nil */
#line 80 "../Main.m3"
 /* load */
#line 80 "../Main.m3"
 /* if_compare */
#line 80 "../Main.m3"
if(m3_eq(ADDRESS,
 ((ADDRESS)(0)),
 ((ADDRESS)(Main_m_98_L_99))))goto LF;
#line 80 "../Main.m3"
 /* load */
#line 80 "../Main.m3"
 /* loophole */
#line 80 "../Main.m3"
 /* load_integer */
#line 80 "../Main.m3"
 /* and */
#line 80 "../Main.m3"
 /* if_true_or_false */
#line 80 "../Main.m3"
 /* load_host_integer */
#line 80 "../Main.m3"
 /* load_integer */
#line 80 "../Main.m3"
 /* if_compare */
#line 80 "../Main.m3"
if(m3_ne(INT64,
  ((INT64)(((INT64)((INT64)Main_m_98_L_99))&  INT64_(1))),
   INT64_(0)))goto LF;
#line 80 "../Main.m3"
 /* load */
#line 80 "../Main.m3"
 /* load_indirect */
#line 80 "../Main.m3"
 /* extract_mn */
#line 80 "../Main.m3"
 /* load_host_integer */
#line 80 "../Main.m3"
 /* load_integer */
#line 80 "../Main.m3"
 /* load_host_integer */
#line 80 "../Main.m3"
 /* load_integer */
#line 80 "../Main.m3"
 /* extract */
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
 ((INT64)(m3_extract(
 UINT64,
  *((INT64*)(INT64_(-8)+((ADDRESS)(Main_m_98_L_99)))),
   UINT64_(22),
   UINT64_(1)))),
   INT64_(0)))goto LF;
#line 80 "../Main.m3"
 /* start_call_direct */
#line 80 "../Main.m3"
 /* load */
#line 80 "../Main.m3"
 /* pop_param */
#line 80 "../Main.m3"
 /* call_direct */
#line 80 "../Main.m3"
RTHooks__CheckLoadTracedRef(
  ( REFANY )(((ADDRESS)(Main_m_98_L_99)) ));
#line 80 "../Main.m3"
 /* set_label */
#line 80 "../Main.m3"
LF:;
#line 80 "../Main.m3"
 /* load */
#line 80 "../Main.m3"
 /* store */
#line 80 "../Main.m3"
(*(ADDRESS*)(&Main_m_100_L_101))=(ADDRESS)(((ADDRESS)(Main_m_98_L_99)));
#line 80 "../Main.m3"
 /* start_call_indirect */
#line 80 "../Main.m3"
 /* load */
#line 80 "../Main.m3"
 /* pop_param */
#line 80 "../Main.m3"
 /* load */
#line 80 "../Main.m3"
 /* load_indirect */
#line 80 "../Main.m3"
 /* load_indirect */
#line 80 "../Main.m3"
 /* check_nil */
#line 80 "../Main.m3"
 /* store */
#line 80 "../Main.m3"
(*(ADDRESS*)(&Main_m_102_L_103))=(ADDRESS)(((ADDRESS)(*((ADDRESS*)(*((ADDRESS*)(Main_m_100_L_101)))))));
#line 80 "../Main.m3"
 /* load */
#line 80 "../Main.m3"
/*check_nil*/if(!Main_m_102_L_103)Main_m_M_Main_L_49_CRASH(2564);
#line 80 "../Main.m3"
 /* call_indirect */
#line 80 "../Main.m3"
((void (__cdecl*)(void*))Main_m_102_L_103)(
 ((ADDRESS)(Main_m_100_L_101)));
#line 80 "../Main.m3"
 /* set_label */
#line 80 "../Main.m3"
 /* start_try */
#line 80 "../Main.m3"
try {
#line 80 "../Main.m3"
 /* set_source_line */
#line 80 "../Main.m3"
#line 81 "../Main.m3"
 /* load */
#line 81 "../Main.m3"
 /* store */
#line 81 "../Main.m3"
(*(INT64*)(&Main_m_104_L_105))=(INT64)( ((INT64)(ThN_L_57)));
#line 81 "../Main.m3"
 /* load_address */
#line 81 "../Main.m3"
 /* load */
#line 81 "../Main.m3"
 /* index_address */
#line 81 "../Main.m3"
 /* store */
#line 81 "../Main.m3"
(*(ADDRESS*)(&Main_m_98_L_99))=(ADDRESS)(((ADDRESS)((((ADDRESS)(INT64_(136)+((ADDRESS)(&Main_m_M_Main_L_49))))+(8*( Main_m_104_L_105))))));
#line 81 "../Main.m3"
 /* begin_block */
#line 81 "../Main.m3"
 /* load */
#line 81 "../Main.m3"
 /* store */
#line 81 "../Main.m3"
(*(ADDRESS*)(&WAct_L_106))=(ADDRESS)(((ADDRESS)(Main_m_98_L_99)));
#line 81 "../Main.m3"
 /* set_source_line */
#line 81 "../Main.m3"
#line 83 "../Main.m3"
 /* load */
#line 83 "../Main.m3"
 /* load_indirect */
#line 83 "../Main.m3"
 /* load_nil */
#line 83 "../Main.m3"
 /* if_compare */
#line 83 "../Main.m3"
if(m3_eq(ADDRESS,
 ((ADDRESS)(*((ADDRESS*)(WAct_L_106)))),
 ((ADDRESS)(0))))goto L14;
#line 83 "../Main.m3"
 /* start_call_direct */
#line 83 "../Main.m3"
 /* load_address */
#line 83 "../Main.m3"
 /* pop_param */
#line 83 "../Main.m3"
 /* load_integer */
#line 83 "../Main.m3"
 /* pop_param */
#line 83 "../Main.m3"
 /* load_address */
#line 83 "../Main.m3"
 /* pop_param */
#line 83 "../Main.m3"
 /* invoke_direct */
#line 83 "../Main.m3"
 /* call_direct */
#line 83 "../Main.m3"
RTHooks__AssertFailed(
  ( ADDRESS )(((ADDRESS)(&Main_m_M_Main_L_49)) ),
  ( INTEGER )(  INT64_(83) ),
  ( TEXT )(((ADDRESS)(INT64_(432)+((ADDRESS)(&Main_m_47_L_48)))) ));
#line 83 "../Main.m3"
 /* set_label */
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
 /* loophole */
#line 84 "../Main.m3"
 /* load_integer */
#line 84 "../Main.m3"
 /* and */
#line 84 "../Main.m3"
 /* load_integer */
#line 84 "../Main.m3"
 /* if_compare */
#line 84 "../Main.m3"
if(m3_ne(INT64,
  ((INT64)(((INT64)((INT64)Apply_L_58))&  INT64_(7))),
   INT64_(0)))goto L16;
#line 84 "../Main.m3"
 /* load */
#line 84 "../Main.m3"
 /* load_nil */
#line 84 "../Main.m3"
 /* if_compare */
#line 84 "../Main.m3"
if(m3_eq(ADDRESS,
 ((ADDRESS)(Apply_L_58)),
 ((ADDRESS)(0))))goto L16;
#line 84 "../Main.m3"
 /* load */
#line 84 "../Main.m3"
 /* load_indirect */
#line 84 "../Main.m3"
 /* load_integer */
#line 84 "../Main.m3"
 /* if_compare */
#line 84 "../Main.m3"
if(m3_ne(INT64,
  *((INT64*)(Apply_L_58)),
   INT64_(-1)))goto L16;
#line 84 "../Main.m3"
 /* set_label */
#line 84 "../Main.m3"
 /* abort */
#line 84 "../Main.m3"
Main_m_M_Main_L_49_CRASH(2693);
#line 84 "../Main.m3"
 /* set_label */
#line 84 "../Main.m3"
L16:;
#line 84 "../Main.m3"
 /* load */
#line 84 "../Main.m3"
 /* load */
#line 84 "../Main.m3"
 /* store_indirect */
#line 84 "../Main.m3"
(*(ADDRESS*)(WAct_L_106))=(ADDRESS)(((ADDRESS)(Apply_L_58)));
#line 84 "../Main.m3"
 /* end_block */
#line 84 "../Main.m3"
 /* jump */
#line 84 "../Main.m3"
goto L12;
#line 84 "../Main.m3"
 /* end_try */
#line 84 "../Main.m3"
} catch (_M3Exc& _m3exc) { _m3_caught = _m3exc.act; goto L11; }
#line 84 "../Main.m3"
 /* set_label */
#line 84 "../Main.m3"
L11:;
#line 84 "../Main.m3"
 /* landing_pad */
#line 84 "../Main.m3"
 /* store */
#line 84 "../Main.m3"
(*(ADDRESS*)(&Main_m_96_L_97))=(ADDRESS)(((ADDRESS)(_m3_caught)));
#line 84 "../Main.m3"
 /* set_label */
#line 84 "../Main.m3"
L12:;
#line 84 "../Main.m3"
 /* start_call_indirect */
#line 84 "../Main.m3"
 /* load */
#line 84 "../Main.m3"
 /* pop_param */
#line 84 "../Main.m3"
 /* load */
#line 84 "../Main.m3"
 /* load_indirect */
#line 84 "../Main.m3"
 /* load_indirect */
#line 84 "../Main.m3"
 /* check_nil */
#line 84 "../Main.m3"
 /* store */
#line 84 "../Main.m3"
(*(ADDRESS*)(&Main_m_107_L_108))=(ADDRESS)(((ADDRESS)(*((ADDRESS*)(INT64_(8)+((ADDRESS)(*((ADDRESS*)(Main_m_100_L_101)))))))));
#line 84 "../Main.m3"
 /* load */
#line 84 "../Main.m3"
/*check_nil*/if(!Main_m_107_L_108)Main_m_M_Main_L_49_CRASH(2692);
#line 84 "../Main.m3"
 /* call_indirect */
#line 84 "../Main.m3"
((void (__cdecl*)(void*))Main_m_107_L_108)(
 ((ADDRESS)(Main_m_100_L_101)));
#line 84 "../Main.m3"
 /* load_nil */
#line 84 "../Main.m3"
 /* load */
#line 84 "../Main.m3"
 /* if_compare */
#line 84 "../Main.m3"
if(m3_eq(ADDRESS,
 ((ADDRESS)(0)),
 ((ADDRESS)(Main_m_96_L_97))))goto L13;
#line 84 "../Main.m3"
 /* start_call_direct */
#line 84 "../Main.m3"
 /* load */
#line 84 "../Main.m3"
 /* pop_param */
#line 84 "../Main.m3"
 /* call_direct */
#line 84 "../Main.m3"
RTHooks__ResumeRaise(
  ( ADDRESS )(((ADDRESS)(Main_m_96_L_97)) ));
#line 84 "../Main.m3"
 /* set_source_line */
#line 84 "../Main.m3"
#line 86 "../Main.m3"
 /* set_label */
#line 86 "../Main.m3"
L13:;
#line 86 "../Main.m3"
 /* set_source_line */
#line 86 "../Main.m3"
#line 87 "../Main.m3"
 /* set_label */
#line 87 "../Main.m3"
L18:;
#line 87 "../Main.m3"
 /* set_source_line */
#line 87 "../Main.m3"
#line 88 "../Main.m3"
 /* start_call_direct */
#line 88 "../Main.m3"
 /* load_float */
#line 88 "../Main.m3"
 /* pop_param */
#line 88 "../Main.m3"
 /* call_direct */
#line 88 "../Main.m3"
Thread__Pause(
  ( LONGREAL )( ((double)(1.00000000000000006e-1)) ));
#line 88 "../Main.m3"
 /* set_source_line */
#line 88 "../Main.m3"
#line 89 "../Main.m3"
 /* load_nil */
#line 89 "../Main.m3"
 /* store */
#line 89 "../Main.m3"
(*(ADDRESS*)(&Main_m_109_L_110))=(ADDRESS)(((ADDRESS)(0)));
#line 89 "../Main.m3"
 /* load */
#line 89 "../Main.m3"
 /* store */
#line 89 "../Main.m3"
(*(ADDRESS*)(&Main_m_98_L_99))=(ADDRESS)(((ADDRESS)(*((ADDRESS*)(INT64_(128)+((ADDRESS)(&Main_m_M_Main_L_49)))))));
#line 89 "../Main.m3"
 /* load_nil */
#line 89 "../Main.m3"
 /* load */
#line 89 "../Main.m3"
 /* if_compare */
#line 89 "../Main.m3"
if(m3_eq(ADDRESS,
 ((ADDRESS)(0)),
 ((ADDRESS)(Main_m_98_L_99))))goto L1A;
#line 89 "../Main.m3"
 /* load */
#line 89 "../Main.m3"
 /* loophole */
#line 89 "../Main.m3"
 /* load_integer */
#line 89 "../Main.m3"
 /* and */
#line 89 "../Main.m3"
 /* if_true_or_false */
#line 89 "../Main.m3"
 /* load_host_integer */
#line 89 "../Main.m3"
 /* load_integer */
#line 89 "../Main.m3"
 /* if_compare */
#line 89 "../Main.m3"
if(m3_ne(INT64,
  ((INT64)(((INT64)((INT64)Main_m_98_L_99))&  INT64_(1))),
   INT64_(0)))goto L1A;
#line 89 "../Main.m3"
 /* load */
#line 89 "../Main.m3"
 /* load_indirect */
#line 89 "../Main.m3"
 /* extract_mn */
#line 89 "../Main.m3"
 /* load_host_integer */
#line 89 "../Main.m3"
 /* load_integer */
#line 89 "../Main.m3"
 /* load_host_integer */
#line 89 "../Main.m3"
 /* load_integer */
#line 89 "../Main.m3"
 /* extract */
#line 89 "../Main.m3"
 /* if_true_or_false */
#line 89 "../Main.m3"
 /* load_host_integer */
#line 89 "../Main.m3"
 /* load_integer */
#line 89 "../Main.m3"
 /* if_compare */
#line 89 "../Main.m3"
if(m3_eq(INT64,
 ((INT64)(m3_extract(
 UINT64,
  *((INT64*)(INT64_(-8)+((ADDRESS)(Main_m_98_L_99)))),
   UINT64_(22),
   UINT64_(1)))),
   INT64_(0)))goto L1A;
#line 89 "../Main.m3"
 /* start_call_direct */
#line 89 "../Main.m3"
 /* load */
#line 89 "../Main.m3"
 /* pop_param */
#line 89 "../Main.m3"
 /* call_direct */
#line 89 "../Main.m3"
RTHooks__CheckLoadTracedRef(
  ( REFANY )(((ADDRESS)(Main_m_98_L_99)) ));
#line 89 "../Main.m3"
 /* set_label */
#line 89 "../Main.m3"
L1A:;
#line 89 "../Main.m3"
 /* load */
#line 89 "../Main.m3"
 /* store */
#line 89 "../Main.m3"
(*(ADDRESS*)(&Main_m_111_L_112))=(ADDRESS)(((ADDRESS)(Main_m_98_L_99)));
#line 89 "../Main.m3"
 /* start_call_indirect */
#line 89 "../Main.m3"
 /* load */
#line 89 "../Main.m3"
 /* pop_param */
#line 89 "../Main.m3"
 /* load */
#line 89 "../Main.m3"
 /* load_indirect */
#line 89 "../Main.m3"
 /* load_indirect */
#line 89 "../Main.m3"
 /* check_nil */
#line 89 "../Main.m3"
 /* store */
#line 89 "../Main.m3"
(*(ADDRESS*)(&Main_m_113_L_114))=(ADDRESS)(((ADDRESS)(*((ADDRESS*)(*((ADDRESS*)(Main_m_111_L_112)))))));
#line 89 "../Main.m3"
 /* load */
#line 89 "../Main.m3"
/*check_nil*/if(!Main_m_113_L_114)Main_m_M_Main_L_49_CRASH(2852);
#line 89 "../Main.m3"
 /* call_indirect */
#line 89 "../Main.m3"
((void (__cdecl*)(void*))Main_m_113_L_114)(
 ((ADDRESS)(Main_m_111_L_112)));
#line 89 "../Main.m3"
 /* set_label */
#line 89 "../Main.m3"
 /* start_try */
#line 89 "../Main.m3"
try {
#line 89 "../Main.m3"
 /* set_source_line */
#line 89 "../Main.m3"
#line 90 "../Main.m3"
 /* load */
#line 90 "../Main.m3"
 /* store */
#line 90 "../Main.m3"
(*(INT64*)(&Main_m_104_L_105))=(INT64)( ((INT64)(ThN_L_57)));
#line 90 "../Main.m3"
 /* load_address */
#line 90 "../Main.m3"
 /* load */
#line 90 "../Main.m3"
 /* index_address */
#line 90 "../Main.m3"
 /* store */
#line 90 "../Main.m3"
(*(ADDRESS*)(&Main_m_98_L_99))=(ADDRESS)(((ADDRESS)((((ADDRESS)(INT64_(136)+((ADDRESS)(&Main_m_M_Main_L_49))))+(8*( Main_m_104_L_105))))));
#line 90 "../Main.m3"
 /* load */
#line 90 "../Main.m3"
 /* load_indirect */
#line 90 "../Main.m3"
 /* load_nil */
#line 90 "../Main.m3"
 /* if_compare */
#line 90 "../Main.m3"
if(m3_ne(ADDRESS,
 ((ADDRESS)(*((ADDRESS*)(Main_m_98_L_99)))),
 ((ADDRESS)(0))))goto L20;
#line 90 "../Main.m3"
 /* load_integer */
#line 90 "../Main.m3"
 /* store */
#line 90 "../Main.m3"
(*(INT64*)(&Main_m_109_L_110))=(INT64)(  INT64_(-2));
#line 90 "../Main.m3"
 /* jump */
#line 90 "../Main.m3"
goto L1D;
#line 90 "../Main.m3"
 /* set_label */
#line 90 "../Main.m3"
L20:;
#line 90 "../Main.m3"
 /* jump */
#line 90 "../Main.m3"
goto L1D;
#line 90 "../Main.m3"
 /* end_try */
#line 90 "../Main.m3"
} catch (...) { throw; }
#line 90 "../Main.m3"
 /* set_label */
#line 90 "../Main.m3"
 /* landing_pad */
#line 90 "../Main.m3"
 /* store */
#line 90 "../Main.m3"
(*(ADDRESS*)(&Main_m_109_L_110))=(ADDRESS)(((ADDRESS)(_m3_caught)));
#line 90 "../Main.m3"
 /* set_label */
#line 90 "../Main.m3"
L1D:;
#line 90 "../Main.m3"
 /* start_call_indirect */
#line 90 "../Main.m3"
 /* load */
#line 90 "../Main.m3"
 /* pop_param */
#line 90 "../Main.m3"
 /* load */
#line 90 "../Main.m3"
 /* load_indirect */
#line 90 "../Main.m3"
 /* load_indirect */
#line 90 "../Main.m3"
 /* check_nil */
#line 90 "../Main.m3"
 /* store */
#line 90 "../Main.m3"
(*(ADDRESS*)(&Main_m_115_L_116))=(ADDRESS)(((ADDRESS)(*((ADDRESS*)(INT64_(8)+((ADDRESS)(*((ADDRESS*)(Main_m_111_L_112)))))))));
#line 90 "../Main.m3"
 /* load */
#line 90 "../Main.m3"
/*check_nil*/if(!Main_m_115_L_116)Main_m_M_Main_L_49_CRASH(2884);
#line 90 "../Main.m3"
 /* call_indirect */
#line 90 "../Main.m3"
((void (__cdecl*)(void*))Main_m_115_L_116)(
 ((ADDRESS)(Main_m_111_L_112)));
#line 90 "../Main.m3"
 /* load */
#line 90 "../Main.m3"
 /* loophole */
#line 90 "../Main.m3"
 /* load_integer */
#line 90 "../Main.m3"
 /* if_compare */
#line 90 "../Main.m3"
if(m3_ne(INT64,
 ((INT64)((INT64)Main_m_109_L_110)),
   INT64_(-2)))goto L21;
#line 90 "../Main.m3"
 /* jump */
#line 90 "../Main.m3"
goto L19;
#line 90 "../Main.m3"
 /* set_label */
#line 90 "../Main.m3"
L21:;
#line 90 "../Main.m3"
 /* load_nil */
#line 90 "../Main.m3"
 /* load */
#line 90 "../Main.m3"
 /* if_compare */
#line 90 "../Main.m3"
if(m3_eq(ADDRESS,
 ((ADDRESS)(0)),
 ((ADDRESS)(Main_m_109_L_110))))goto L1E;
#line 90 "../Main.m3"
 /* start_call_direct */
#line 90 "../Main.m3"
 /* load */
#line 90 "../Main.m3"
 /* pop_param */
#line 90 "../Main.m3"
 /* call_direct */
#line 90 "../Main.m3"
RTHooks__ResumeRaise(
  ( ADDRESS )(((ADDRESS)(Main_m_109_L_110)) ));
#line 90 "../Main.m3"
 /* set_source_line */
#line 90 "../Main.m3"
#line 91 "../Main.m3"
 /* set_label */
#line 91 "../Main.m3"
L1E:;
#line 91 "../Main.m3"
 /* jump */
#line 91 "../Main.m3"
goto L18;
#line 91 "../Main.m3"
 /* set_label */
#line 91 "../Main.m3"
L19:;
#line 91 "../Main.m3"
 /* set_source_line */
#line 91 "../Main.m3"
#line 93 "../Main.m3"
 /* exit_proc */
#line 93 "../Main.m3"
return;
#line 93 "../Main.m3"
 /* end_procedure */
#line 93 "../Main.m3"
} /* ActionWait */
#line 93 "../Main.m3"
 /* set_source_line */
#line 93 "../Main.m3"
#line 95 "../Main.m3"
 /* begin_procedure */
#line 95 "../Main.m3"
struct Main__ActionWait_Frame_t {
#line 95 "../Main.m3"
ADDRESS _unused;
#line 95 "../Main.m3"
};
#line 95 "../Main.m3"
void /*TypeText3*/ 
__cdecl
Main__ActionWait(
   /* Param_Type1 */ Main__ThreadNo ThN_L_59,
   /* Param_Type1 */ Main__ActionProc Apply_L_60)
{
#line 95 "../Main.m3"
 /* Var_Type2 */ ADDRESS Main_m_117_L_118={0};//always-init
#line 95 "../Main.m3"
 /* Var_Type2 */ ADDRESS Main_m_119_L_120={0};//always-init
#line 95 "../Main.m3"
 /* Var_Type1 */ MUTEX Main_m_121_L_122={0};//always-init
#line 95 "../Main.m3"
 /* Var_Type2 */ ADDRESS Main_m_123_L_124={0};//always-init
#line 95 "../Main.m3"
 /* Var_Type2 */ ADDRESS Main_m_125_L_126={0};//always-init
#line 95 "../Main.m3"
 /* Var_Type1 */ MUTEX Main_m_127_L_128={0};//always-init
#line 95 "../Main.m3"
 /* Var_Type2 */ ADDRESS Main_m_129_L_130={0};//always-init
#line 95 "../Main.m3"
 /* Var_Type2 */ INT64 Main_m_131_L_132={0};//always-init
#line 95 "../Main.m3"
 /* Var_Type1 */ T7609BE10* WSt_L_133={0};//always-init
#line 95 "../Main.m3"
 /* Var_Type2 */ ADDRESS Main_m_134_L_135={0};//always-init
#line 95 "../Main.m3"
 /* Var_Type2 */ ADDRESS Main_m_136_L_137={0};//always-init
#line 95 "../Main.m3"
 /* Var_Type2 */ ADDRESS Main_m_138_L_139={0};//always-init
#line 95 "../Main.m3"
Main__ActionWait_Frame_t _frame;
#line 95 "../Main.m3"
_frame._unused=(ADDRESS)&_frame;
#line 95 "../Main.m3"
 /* set_source_line */
#line 95 "../Main.m3"
#line 102 "../Main.m3"
 /* set_source_line */
#line 102 "../Main.m3"
#line 103 "../Main.m3"
 /* start_call_direct */
#line 103 "../Main.m3"
 /* load */
#line 103 "../Main.m3"
 /* pop_param */
#line 103 "../Main.m3"
 /* load */
#line 103 "../Main.m3"
 /* pop_param */
#line 103 "../Main.m3"
 /* call_direct */
#line 103 "../Main.m3"
Main__Action(
  ( Main__ThreadNo )(((UINT8)(((INT64)(ThN_L_59)))) ),
  ( Main__ActionProc )(((ADDRESS)(Apply_L_60)) ));
#line 103 "../Main.m3"
 /* set_source_line */
#line 103 "../Main.m3"
#line 104 "../Main.m3"
 /* set_label */
#line 104 "../Main.m3"
 /* set_source_line */
#line 104 "../Main.m3"
#line 107 "../Main.m3"
 /* load_nil */
#line 107 "../Main.m3"
 /* store */
#line 107 "../Main.m3"
(*(ADDRESS*)(&Main_m_117_L_118))=(ADDRESS)(((ADDRESS)(0)));
#line 107 "../Main.m3"
 /* load */
#line 107 "../Main.m3"
 /* store */
#line 107 "../Main.m3"
(*(ADDRESS*)(&Main_m_119_L_120))=(ADDRESS)(((ADDRESS)(*((ADDRESS*)(INT64_(176)+((ADDRESS)(&Main_m_M_Main_L_49)))))));
#line 107 "../Main.m3"
 /* load_nil */
#line 107 "../Main.m3"
 /* load */
#line 107 "../Main.m3"
 /* if_compare */
#line 107 "../Main.m3"
if(m3_eq(ADDRESS,
 ((ADDRESS)(0)),
 ((ADDRESS)(Main_m_119_L_120))))goto L24;
#line 107 "../Main.m3"
 /* load */
#line 107 "../Main.m3"
 /* loophole */
#line 107 "../Main.m3"
 /* load_integer */
#line 107 "../Main.m3"
 /* and */
#line 107 "../Main.m3"
 /* if_true_or_false */
#line 107 "../Main.m3"
 /* load_host_integer */
#line 107 "../Main.m3"
 /* load_integer */
#line 107 "../Main.m3"
 /* if_compare */
#line 107 "../Main.m3"
if(m3_ne(INT64,
  ((INT64)(((INT64)((INT64)Main_m_119_L_120))&  INT64_(1))),
   INT64_(0)))goto L24;
#line 107 "../Main.m3"
 /* load */
#line 107 "../Main.m3"
 /* load_indirect */
#line 107 "../Main.m3"
 /* extract_mn */
#line 107 "../Main.m3"
 /* load_host_integer */
#line 107 "../Main.m3"
 /* load_integer */
#line 107 "../Main.m3"
 /* load_host_integer */
#line 107 "../Main.m3"
 /* load_integer */
#line 107 "../Main.m3"
 /* extract */
#line 107 "../Main.m3"
 /* if_true_or_false */
#line 107 "../Main.m3"
 /* load_host_integer */
#line 107 "../Main.m3"
 /* load_integer */
#line 107 "../Main.m3"
 /* if_compare */
#line 107 "../Main.m3"
if(m3_eq(INT64,
 ((INT64)(m3_extract(
 UINT64,
  *((INT64*)(INT64_(-8)+((ADDRESS)(Main_m_119_L_120)))),
   UINT64_(22),
   UINT64_(1)))),
   INT64_(0)))goto L24;
#line 107 "../Main.m3"
 /* start_call_direct */
#line 107 "../Main.m3"
 /* load */
#line 107 "../Main.m3"
 /* pop_param */
#line 107 "../Main.m3"
 /* call_direct */
#line 107 "../Main.m3"
RTHooks__CheckLoadTracedRef(
  ( REFANY )(((ADDRESS)(Main_m_119_L_120)) ));
#line 107 "../Main.m3"
 /* set_label */
#line 107 "../Main.m3"
L24:;
#line 107 "../Main.m3"
 /* load */
#line 107 "../Main.m3"
 /* store */
#line 107 "../Main.m3"
(*(ADDRESS*)(&Main_m_121_L_122))=(ADDRESS)(((ADDRESS)(Main_m_119_L_120)));
#line 107 "../Main.m3"
 /* start_call_indirect */
#line 107 "../Main.m3"
 /* load */
#line 107 "../Main.m3"
 /* pop_param */
#line 107 "../Main.m3"
 /* load */
#line 107 "../Main.m3"
 /* load_indirect */
#line 107 "../Main.m3"
 /* load_indirect */
#line 107 "../Main.m3"
 /* check_nil */
#line 107 "../Main.m3"
 /* store */
#line 107 "../Main.m3"
(*(ADDRESS*)(&Main_m_123_L_124))=(ADDRESS)(((ADDRESS)(*((ADDRESS*)(*((ADDRESS*)(Main_m_121_L_122)))))));
#line 107 "../Main.m3"
 /* load */
#line 107 "../Main.m3"
/*check_nil*/if(!Main_m_123_L_124)Main_m_M_Main_L_49_CRASH(3428);
#line 107 "../Main.m3"
 /* call_indirect */
#line 107 "../Main.m3"
((void (__cdecl*)(void*))Main_m_123_L_124)(
 ((ADDRESS)(Main_m_121_L_122)));
#line 107 "../Main.m3"
 /* set_label */
#line 107 "../Main.m3"
 /* start_try */
#line 107 "../Main.m3"
try {
#line 107 "../Main.m3"
 /* set_source_line */
#line 107 "../Main.m3"
#line 108 "../Main.m3"
 /* load_nil */
#line 108 "../Main.m3"
 /* store */
#line 108 "../Main.m3"
(*(ADDRESS*)(&Main_m_125_L_126))=(ADDRESS)(((ADDRESS)(0)));
#line 108 "../Main.m3"
 /* load */
#line 108 "../Main.m3"
 /* store */
#line 108 "../Main.m3"
(*(ADDRESS*)(&Main_m_119_L_120))=(ADDRESS)(((ADDRESS)(*((ADDRESS*)(INT64_(104)+((ADDRESS)(&Main_m_M_Main_L_49)))))));
#line 108 "../Main.m3"
 /* load_nil */
#line 108 "../Main.m3"
 /* load */
#line 108 "../Main.m3"
 /* if_compare */
#line 108 "../Main.m3"
if(m3_eq(ADDRESS,
 ((ADDRESS)(0)),
 ((ADDRESS)(Main_m_119_L_120))))goto L29;
#line 108 "../Main.m3"
 /* load */
#line 108 "../Main.m3"
 /* loophole */
#line 108 "../Main.m3"
 /* load_integer */
#line 108 "../Main.m3"
 /* and */
#line 108 "../Main.m3"
 /* if_true_or_false */
#line 108 "../Main.m3"
 /* load_host_integer */
#line 108 "../Main.m3"
 /* load_integer */
#line 108 "../Main.m3"
 /* if_compare */
#line 108 "../Main.m3"
if(m3_ne(INT64,
  ((INT64)(((INT64)((INT64)Main_m_119_L_120))&  INT64_(1))),
   INT64_(0)))goto L29;
#line 108 "../Main.m3"
 /* load */
#line 108 "../Main.m3"
 /* load_indirect */
#line 108 "../Main.m3"
 /* extract_mn */
#line 108 "../Main.m3"
 /* load_host_integer */
#line 108 "../Main.m3"
 /* load_integer */
#line 108 "../Main.m3"
 /* load_host_integer */
#line 108 "../Main.m3"
 /* load_integer */
#line 108 "../Main.m3"
 /* extract */
#line 108 "../Main.m3"
 /* if_true_or_false */
#line 108 "../Main.m3"
 /* load_host_integer */
#line 108 "../Main.m3"
 /* load_integer */
#line 108 "../Main.m3"
 /* if_compare */
#line 108 "../Main.m3"
if(m3_eq(INT64,
 ((INT64)(m3_extract(
 UINT64,
  *((INT64*)(INT64_(-8)+((ADDRESS)(Main_m_119_L_120)))),
   UINT64_(22),
   UINT64_(1)))),
   INT64_(0)))goto L29;
#line 108 "../Main.m3"
 /* start_call_direct */
#line 108 "../Main.m3"
 /* load */
#line 108 "../Main.m3"
 /* pop_param */
#line 108 "../Main.m3"
 /* invoke_direct */
#line 108 "../Main.m3"
 /* call_direct */
#line 108 "../Main.m3"
RTHooks__CheckLoadTracedRef(
  ( REFANY )(((ADDRESS)(Main_m_119_L_120)) ));
#line 108 "../Main.m3"
 /* set_label */
#line 108 "../Main.m3"
 /* set_label */
#line 108 "../Main.m3"
L29:;
#line 108 "../Main.m3"
 /* load */
#line 108 "../Main.m3"
 /* store */
#line 108 "../Main.m3"
(*(ADDRESS*)(&Main_m_127_L_128))=(ADDRESS)(((ADDRESS)(Main_m_119_L_120)));
#line 108 "../Main.m3"
 /* start_call_indirect */
#line 108 "../Main.m3"
 /* load */
#line 108 "../Main.m3"
 /* pop_param */
#line 108 "../Main.m3"
 /* load */
#line 108 "../Main.m3"
 /* load_indirect */
#line 108 "../Main.m3"
 /* load_indirect */
#line 108 "../Main.m3"
 /* check_nil */
#line 108 "../Main.m3"
 /* store */
#line 108 "../Main.m3"
(*(ADDRESS*)(&Main_m_129_L_130))=(ADDRESS)(((ADDRESS)(*((ADDRESS*)(*((ADDRESS*)(Main_m_127_L_128)))))));
#line 108 "../Main.m3"
 /* load */
#line 108 "../Main.m3"
/*check_nil*/if(!Main_m_129_L_130)Main_m_M_Main_L_49_CRASH(3460);
#line 108 "../Main.m3"
 /* call_indirect */
#line 108 "../Main.m3"
((void (__cdecl*)(void*))Main_m_129_L_130)(
 ((ADDRESS)(Main_m_127_L_128)));
#line 108 "../Main.m3"
 /* set_label */
#line 108 "../Main.m3"
 /* start_try */
#line 108 "../Main.m3"
try {
#line 108 "../Main.m3"
 /* set_source_line */
#line 108 "../Main.m3"
#line 109 "../Main.m3"
 /* load */
#line 109 "../Main.m3"
 /* store */
#line 109 "../Main.m3"
(*(INT64*)(&Main_m_131_L_132))=(INT64)( ((INT64)(ThN_L_59)));
#line 109 "../Main.m3"
 /* load_address */
#line 109 "../Main.m3"
 /* load */
#line 109 "../Main.m3"
 /* index_address */
#line 109 "../Main.m3"
 /* store */
#line 109 "../Main.m3"
(*(ADDRESS*)(&Main_m_119_L_120))=(ADDRESS)(((ADDRESS)((((ADDRESS)(INT64_(112)+((ADDRESS)(&Main_m_M_Main_L_49))))+( Main_m_131_L_132)))));
#line 109 "../Main.m3"
 /* begin_block */
#line 109 "../Main.m3"
 /* load */
#line 109 "../Main.m3"
 /* store */
#line 109 "../Main.m3"
(*(ADDRESS*)(&WSt_L_133))=(ADDRESS)(((ADDRESS)(Main_m_119_L_120)));
#line 109 "../Main.m3"
 /* set_source_line */
#line 109 "../Main.m3"
#line 110 "../Main.m3"
 /* load */
#line 110 "../Main.m3"
 /* load_indirect */
#line 110 "../Main.m3"
 /* load_integer */
#line 110 "../Main.m3"
 /* if_compare */
#line 110 "../Main.m3"
if(m3_eq(UINT64,
 ((UINT64)(((INT64)(*((UINT8*)(WSt_L_133)))))),
 ((UINT64)( INT64_(4)))))goto L30;
#line 110 "../Main.m3"
 /* set_source_line */
#line 110 "../Main.m3"
#line 112 "../Main.m3"
 /* start_call_direct */
#line 112 "../Main.m3"
 /* load */
#line 112 "../Main.m3"
 /* pop_param */
#line 112 "../Main.m3"
 /* invoke_direct */
#line 112 "../Main.m3"
 /* call_direct */
#line 112 "../Main.m3"
 /* set_label */
#line 112 "../Main.m3"
 /* store */
#line 112 "../Main.m3"
(*(ADDRESS*)(&Main_m_119_L_120))=(ADDRESS)(((ADDRESS)(Main__ThImage(
  ( Main__ThreadNo )(((UINT8)(((INT64)(ThN_L_59)))) )))));
#line 112 "../Main.m3"
 /* start_call_direct */
#line 112 "../Main.m3"
 /* load */
#line 112 "../Main.m3"
 /* pop_param */
#line 112 "../Main.m3"
 /* load_address */
#line 112 "../Main.m3"
 /* pop_param */
#line 112 "../Main.m3"
 /* invoke_direct */
#line 112 "../Main.m3"
 /* call_direct */
#line 112 "../Main.m3"
 /* set_label */
#line 112 "../Main.m3"
 /* store */
#line 112 "../Main.m3"
(*(ADDRESS*)(&Main_m_134_L_135))=(ADDRESS)(((ADDRESS)(RTHooks__Concat(
  ( TEXT )(((ADDRESS)(Main_m_119_L_120)) ),
  ( TEXT )(((ADDRESS)(INT64_(472)+((ADDRESS)(&Main_m_47_L_48)))) )))));
#line 112 "../Main.m3"
 /* start_call_direct */
#line 112 "../Main.m3"
 /* load */
#line 112 "../Main.m3"
 /* pop_param */
#line 112 "../Main.m3"
 /* invoke_direct */
#line 112 "../Main.m3"
 /* call_direct */
#line 112 "../Main.m3"
Main__W(
  ( TEXT )(((ADDRESS)(Main_m_134_L_135)) ));
#line 112 "../Main.m3"
 /* set_label */
#line 112 "../Main.m3"
 /* set_source_line */
#line 112 "../Main.m3"
#line 113 "../Main.m3"
 /* start_call_direct */
#line 113 "../Main.m3"
 /* load_address */
#line 113 "../Main.m3"
 /* pop_param */
#line 113 "../Main.m3"
 /* load_nil */
#line 113 "../Main.m3"
 /* pop_param */
#line 113 "../Main.m3"
 /* load_address */
#line 113 "../Main.m3"
 /* pop_param */
#line 113 "../Main.m3"
 /* load_integer */
#line 113 "../Main.m3"
 /* pop_param */
#line 113 "../Main.m3"
 /* invoke_direct */
#line 113 "../Main.m3"
 /* call_direct */
#line 113 "../Main.m3"
RTHooks__Raise(
  ( ADDRESS )(((ADDRESS)(&Main_m_47_L_48)) ),
  ( ADDRESS )(((ADDRESS)(0)) ),
  ( ADDRESS )(((ADDRESS)(&Main_m_M_Main_L_49)) ),
  ( INTEGER )(  INT64_(113) ));
#line 113 "../Main.m3"
 /* set_label */
#line 113 "../Main.m3"
 /* set_label */
#line 113 "../Main.m3"
L30:;
#line 113 "../Main.m3"
 /* set_source_line */
#line 113 "../Main.m3"
#line 115 "../Main.m3"
 /* load */
#line 115 "../Main.m3"
 /* load */
#line 115 "../Main.m3"
 /* if_compare */
#line 115 "../Main.m3"
if(m3_eq(INT64,
  ((INT64)(ThN_L_59)),
  ((INT64)(*((UINT8*)(INT64_(117)+((ADDRESS)(&Main_m_M_Main_L_49))))))))goto L35;
#line 115 "../Main.m3"
 /* start_call_direct */
#line 115 "../Main.m3"
 /* load_address */
#line 115 "../Main.m3"
 /* pop_param */
#line 115 "../Main.m3"
 /* load_integer */
#line 115 "../Main.m3"
 /* pop_param */
#line 115 "../Main.m3"
 /* load_address */
#line 115 "../Main.m3"
 /* pop_param */
#line 115 "../Main.m3"
 /* invoke_direct */
#line 115 "../Main.m3"
 /* call_direct */
#line 115 "../Main.m3"
RTHooks__AssertFailed(
  ( ADDRESS )(((ADDRESS)(&Main_m_M_Main_L_49)) ),
  ( INTEGER )(  INT64_(115) ),
  ( TEXT )(((ADDRESS)(INT64_(528)+((ADDRESS)(&Main_m_47_L_48)))) ));
#line 115 "../Main.m3"
 /* set_label */
#line 115 "../Main.m3"
 /* set_label */
#line 115 "../Main.m3"
L35:;
#line 115 "../Main.m3"
 /* set_source_line */
#line 115 "../Main.m3"
#line 116 "../Main.m3"
 /* load_integer */
#line 116 "../Main.m3"
 /* store */
#line 116 "../Main.m3"
(*(UINT8*)((117)+(char*)(&Main_m_M_Main_L_49)))=(INT64)(  INT64_(0));
#line 116 "../Main.m3"
 /* set_source_line */
#line 116 "../Main.m3"
#line 117 "../Main.m3"
 /* load */
#line 117 "../Main.m3"
 /* load_integer */
#line 117 "../Main.m3"
 /* store_indirect */
#line 117 "../Main.m3"
(*(UINT8*)(WSt_L_133))=(INT64)(  INT64_(5));
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
 /* invoke_direct */
#line 118 "../Main.m3"
 /* call_direct */
#line 118 "../Main.m3"
 /* set_label */
#line 118 "../Main.m3"
 /* store */
#line 118 "../Main.m3"
(*(ADDRESS*)(&Main_m_134_L_135))=(ADDRESS)(((ADDRESS)(Main__ThImage(
  ( Main__ThreadNo )(((UINT8)(((INT64)(ThN_L_59)))) )))));
#line 118 "../Main.m3"
 /* start_call_direct */
#line 118 "../Main.m3"
 /* load */
#line 118 "../Main.m3"
 /* pop_param */
#line 118 "../Main.m3"
 /* load_address */
#line 118 "../Main.m3"
 /* pop_param */
#line 118 "../Main.m3"
 /* invoke_direct */
#line 118 "../Main.m3"
 /* call_direct */
#line 118 "../Main.m3"
 /* set_label */
#line 118 "../Main.m3"
 /* store */
#line 118 "../Main.m3"
(*(ADDRESS*)(&Main_m_119_L_120))=(ADDRESS)(((ADDRESS)(RTHooks__Concat(
  ( TEXT )(((ADDRESS)(Main_m_134_L_135)) ),
  ( TEXT )(((ADDRESS)(INT64_(568)+((ADDRESS)(&Main_m_47_L_48)))) )))));
#line 118 "../Main.m3"
 /* start_call_direct */
#line 118 "../Main.m3"
 /* load */
#line 118 "../Main.m3"
 /* pop_param */
#line 118 "../Main.m3"
 /* invoke_direct */
#line 118 "../Main.m3"
 /* call_direct */
#line 118 "../Main.m3"
Main__W(
  ( TEXT )(((ADDRESS)(Main_m_119_L_120)) ));
#line 118 "../Main.m3"
 /* set_label */
#line 118 "../Main.m3"
 /* set_source_line */
#line 118 "../Main.m3"
#line 119 "../Main.m3"
 /* load_integer */
#line 119 "../Main.m3"
 /* store */
#line 119 "../Main.m3"
(*(INT64*)(&Main_m_125_L_126))=(INT64)(  INT64_(-2));
#line 119 "../Main.m3"
 /* jump */
#line 119 "../Main.m3"
goto L2D;
#line 119 "../Main.m3"
 /* end_block */
#line 119 "../Main.m3"
 /* jump */
#line 119 "../Main.m3"
goto L2D;
#line 119 "../Main.m3"
 /* end_try */
#line 119 "../Main.m3"
} catch (_M3Exc& _m3exc) { _m3_caught = _m3exc.act; goto L2C; }
#line 119 "../Main.m3"
 /* set_label */
#line 119 "../Main.m3"
L2C:;
#line 119 "../Main.m3"
 /* landing_pad */
#line 119 "../Main.m3"
 /* store */
#line 119 "../Main.m3"
(*(ADDRESS*)(&Main_m_125_L_126))=(ADDRESS)(((ADDRESS)(_m3_caught)));
#line 119 "../Main.m3"
 /* set_label */
#line 119 "../Main.m3"
L2D:;
#line 119 "../Main.m3"
 /* start_call_indirect */
#line 119 "../Main.m3"
 /* load */
#line 119 "../Main.m3"
 /* pop_param */
#line 119 "../Main.m3"
 /* load */
#line 119 "../Main.m3"
 /* load_indirect */
#line 119 "../Main.m3"
 /* load_indirect */
#line 119 "../Main.m3"
 /* check_nil */
#line 119 "../Main.m3"
 /* store */
#line 119 "../Main.m3"
(*(ADDRESS*)(&Main_m_136_L_137))=(ADDRESS)(((ADDRESS)(*((ADDRESS*)(INT64_(8)+((ADDRESS)(*((ADDRESS*)(Main_m_127_L_128)))))))));
#line 119 "../Main.m3"
 /* load */
#line 119 "../Main.m3"
/*check_nil*/if(!Main_m_136_L_137)Main_m_M_Main_L_49_CRASH(3812);
#line 119 "../Main.m3"
 /* call_indirect */
#line 119 "../Main.m3"
((void (__cdecl*)(void*))Main_m_136_L_137)(
 ((ADDRESS)(Main_m_127_L_128)));
#line 119 "../Main.m3"
 /* load */
#line 119 "../Main.m3"
 /* loophole */
#line 119 "../Main.m3"
 /* load_integer */
#line 119 "../Main.m3"
 /* if_compare */
#line 119 "../Main.m3"
if(m3_ne(INT64,
 ((INT64)((INT64)Main_m_125_L_126)),
   INT64_(-2)))goto L3A;
#line 119 "../Main.m3"
 /* load_integer */
#line 119 "../Main.m3"
 /* store */
#line 119 "../Main.m3"
(*(INT64*)(&Main_m_117_L_118))=(INT64)(  INT64_(-2));
#line 119 "../Main.m3"
 /* jump */
#line 119 "../Main.m3"
goto L27;
#line 119 "../Main.m3"
 /* set_label */
#line 119 "../Main.m3"
L3A:;
#line 119 "../Main.m3"
 /* load_nil */
#line 119 "../Main.m3"
 /* load */
#line 119 "../Main.m3"
 /* if_compare */
#line 119 "../Main.m3"
if(m3_eq(ADDRESS,
 ((ADDRESS)(0)),
 ((ADDRESS)(Main_m_125_L_126))))goto L2E;
#line 119 "../Main.m3"
 /* start_call_direct */
#line 119 "../Main.m3"
 /* load */
#line 119 "../Main.m3"
 /* pop_param */
#line 119 "../Main.m3"
 /* invoke_direct */
#line 119 "../Main.m3"
 /* call_direct */
#line 119 "../Main.m3"
RTHooks__ResumeRaise(
  ( ADDRESS )(((ADDRESS)(Main_m_125_L_126)) ));
#line 119 "../Main.m3"
 /* set_label */
#line 119 "../Main.m3"
 /* set_source_line */
#line 119 "../Main.m3"
#line 122 "../Main.m3"
 /* set_label */
#line 122 "../Main.m3"
L2E:;
#line 122 "../Main.m3"
 /* jump */
#line 122 "../Main.m3"
goto L27;
#line 122 "../Main.m3"
 /* end_try */
#line 122 "../Main.m3"
} catch (_M3Exc& _m3exc) { _m3_caught = _m3exc.act; goto L26; }
#line 122 "../Main.m3"
 /* set_label */
#line 122 "../Main.m3"
L26:;
#line 122 "../Main.m3"
 /* landing_pad */
#line 122 "../Main.m3"
 /* store */
#line 122 "../Main.m3"
(*(ADDRESS*)(&Main_m_117_L_118))=(ADDRESS)(((ADDRESS)(_m3_caught)));
#line 122 "../Main.m3"
 /* set_label */
#line 122 "../Main.m3"
L27:;
#line 122 "../Main.m3"
 /* start_call_indirect */
#line 122 "../Main.m3"
 /* load */
#line 122 "../Main.m3"
 /* pop_param */
#line 122 "../Main.m3"
 /* load */
#line 122 "../Main.m3"
 /* load_indirect */
#line 122 "../Main.m3"
 /* load_indirect */
#line 122 "../Main.m3"
 /* check_nil */
#line 122 "../Main.m3"
 /* store */
#line 122 "../Main.m3"
(*(ADDRESS*)(&Main_m_138_L_139))=(ADDRESS)(((ADDRESS)(*((ADDRESS*)(INT64_(8)+((ADDRESS)(*((ADDRESS*)(Main_m_121_L_122)))))))));
#line 122 "../Main.m3"
 /* load */
#line 122 "../Main.m3"
/*check_nil*/if(!Main_m_138_L_139)Main_m_M_Main_L_49_CRASH(3908);
#line 122 "../Main.m3"
 /* call_indirect */
#line 122 "../Main.m3"
((void (__cdecl*)(void*))Main_m_138_L_139)(
 ((ADDRESS)(Main_m_121_L_122)));
#line 122 "../Main.m3"
 /* load */
#line 122 "../Main.m3"
 /* loophole */
#line 122 "../Main.m3"
 /* load_integer */
#line 122 "../Main.m3"
 /* if_compare */
#line 122 "../Main.m3"
if(m3_ne(INT64,
 ((INT64)((INT64)Main_m_117_L_118)),
   INT64_(-2)))goto L3C;
#line 122 "../Main.m3"
 /* jump */
#line 122 "../Main.m3"
goto L23;
#line 122 "../Main.m3"
 /* set_label */
#line 122 "../Main.m3"
L3C:;
#line 122 "../Main.m3"
 /* load_nil */
#line 122 "../Main.m3"
 /* load */
#line 122 "../Main.m3"
 /* if_compare */
#line 122 "../Main.m3"
if(m3_eq(ADDRESS,
 ((ADDRESS)(0)),
 ((ADDRESS)(Main_m_117_L_118))))goto L28;
#line 122 "../Main.m3"
 /* start_call_direct */
#line 122 "../Main.m3"
 /* load */
#line 122 "../Main.m3"
 /* pop_param */
#line 122 "../Main.m3"
 /* call_direct */
#line 122 "../Main.m3"
RTHooks__ResumeRaise(
  ( ADDRESS )(((ADDRESS)(Main_m_117_L_118)) ));
#line 122 "../Main.m3"
 /* set_source_line */
#line 122 "../Main.m3"
#line 123 "../Main.m3"
 /* set_label */
#line 123 "../Main.m3"
L28:;
#line 123 "../Main.m3"
 /* set_label */
#line 123 "../Main.m3"
L23:;
#line 123 "../Main.m3"
 /* set_source_line */
#line 123 "../Main.m3"
#line 125 "../Main.m3"
 /* exit_proc */
#line 125 "../Main.m3"
return;
#line 125 "../Main.m3"
 /* end_procedure */
#line 125 "../Main.m3"
} /* WaitForHeld */
#line 125 "../Main.m3"
 /* set_source_line */
#line 125 "../Main.m3"
#line 127 "../Main.m3"
 /* begin_procedure */
#line 127 "../Main.m3"
struct Main__WaitForHeld_Frame_t {
#line 127 "../Main.m3"
ADDRESS _unused;
#line 127 "../Main.m3"
};
#line 127 "../Main.m3"
Main__ThreadNo
__cdecl
Main__WaitForHeld(void)
{
#line 127 "../Main.m3"
 /* Var_Type1 */ T8E2831D7_8 LHolder_L_61={0};//always-init
#line 127 "../Main.m3"
 /* Var_Type2 */ ADDRESS Main_m_140_L_141={0};//always-init
#line 127 "../Main.m3"
 /* Var_Type2 */ ADDRESS Main_m_142_L_143={0};//always-init
#line 127 "../Main.m3"
 /* Var_Type1 */ MUTEX Main_m_144_L_145={0};//always-init
#line 127 "../Main.m3"
 /* Var_Type2 */ ADDRESS Main_m_146_L_147={0};//always-init
#line 127 "../Main.m3"
 /* Var_Type2 */ ADDRESS Main_m_148_L_149={0};//always-init
#line 127 "../Main.m3"
Main__WaitForHeld_Frame_t _frame;
#line 127 "../Main.m3"
_frame._unused=(ADDRESS)&_frame;
#line 127 "../Main.m3"
 /* set_source_line */
#line 127 "../Main.m3"
#line 130 "../Main.m3"
 /* load_integer */
#line 130 "../Main.m3"
 /* store */
#line 130 "../Main.m3"
(*(UINT8*)(&LHolder_L_61))=(INT64)(  INT64_(0));
#line 130 "../Main.m3"
 /* set_source_line */
#line 130 "../Main.m3"
#line 133 "../Main.m3"
 /* set_label */
#line 133 "../Main.m3"
L3D:;
#line 133 "../Main.m3"
 /* set_source_line */
#line 133 "../Main.m3"
#line 134 "../Main.m3"
 /* start_call_direct */
#line 134 "../Main.m3"
 /* load_float */
#line 134 "../Main.m3"
 /* pop_param */
#line 134 "../Main.m3"
 /* call_direct */
#line 134 "../Main.m3"
Thread__Pause(
  ( LONGREAL )( ((double)(1.00000000000000006e-1)) ));
#line 134 "../Main.m3"
 /* set_source_line */
#line 134 "../Main.m3"
#line 135 "../Main.m3"
 /* load_nil */
#line 135 "../Main.m3"
 /* store */
#line 135 "../Main.m3"
(*(ADDRESS*)(&Main_m_140_L_141))=(ADDRESS)(((ADDRESS)(0)));
#line 135 "../Main.m3"
 /* load */
#line 135 "../Main.m3"
 /* store */
#line 135 "../Main.m3"
(*(ADDRESS*)(&Main_m_142_L_143))=(ADDRESS)(((ADDRESS)(*((ADDRESS*)(INT64_(104)+((ADDRESS)(&Main_m_M_Main_L_49)))))));
#line 135 "../Main.m3"
 /* load_nil */
#line 135 "../Main.m3"
 /* load */
#line 135 "../Main.m3"
 /* if_compare */
#line 135 "../Main.m3"
if(m3_eq(ADDRESS,
 ((ADDRESS)(0)),
 ((ADDRESS)(Main_m_142_L_143))))goto L3F;
#line 135 "../Main.m3"
 /* load */
#line 135 "../Main.m3"
 /* loophole */
#line 135 "../Main.m3"
 /* load_integer */
#line 135 "../Main.m3"
 /* and */
#line 135 "../Main.m3"
 /* if_true_or_false */
#line 135 "../Main.m3"
 /* load_host_integer */
#line 135 "../Main.m3"
 /* load_integer */
#line 135 "../Main.m3"
 /* if_compare */
#line 135 "../Main.m3"
if(m3_ne(INT64,
  ((INT64)(((INT64)((INT64)Main_m_142_L_143))&  INT64_(1))),
   INT64_(0)))goto L3F;
#line 135 "../Main.m3"
 /* load */
#line 135 "../Main.m3"
 /* load_indirect */
#line 135 "../Main.m3"
 /* extract_mn */
#line 135 "../Main.m3"
 /* load_host_integer */
#line 135 "../Main.m3"
 /* load_integer */
#line 135 "../Main.m3"
 /* load_host_integer */
#line 135 "../Main.m3"
 /* load_integer */
#line 135 "../Main.m3"
 /* extract */
#line 135 "../Main.m3"
 /* if_true_or_false */
#line 135 "../Main.m3"
 /* load_host_integer */
#line 135 "../Main.m3"
 /* load_integer */
#line 135 "../Main.m3"
 /* if_compare */
#line 135 "../Main.m3"
if(m3_eq(INT64,
 ((INT64)(m3_extract(
 UINT64,
  *((INT64*)(INT64_(-8)+((ADDRESS)(Main_m_142_L_143)))),
   UINT64_(22),
   UINT64_(1)))),
   INT64_(0)))goto L3F;
#line 135 "../Main.m3"
 /* start_call_direct */
#line 135 "../Main.m3"
 /* load */
#line 135 "../Main.m3"
 /* pop_param */
#line 135 "../Main.m3"
 /* call_direct */
#line 135 "../Main.m3"
RTHooks__CheckLoadTracedRef(
  ( REFANY )(((ADDRESS)(Main_m_142_L_143)) ));
#line 135 "../Main.m3"
 /* set_label */
#line 135 "../Main.m3"
L3F:;
#line 135 "../Main.m3"
 /* load */
#line 135 "../Main.m3"
 /* store */
#line 135 "../Main.m3"
(*(ADDRESS*)(&Main_m_144_L_145))=(ADDRESS)(((ADDRESS)(Main_m_142_L_143)));
#line 135 "../Main.m3"
 /* start_call_indirect */
#line 135 "../Main.m3"
 /* load */
#line 135 "../Main.m3"
 /* pop_param */
#line 135 "../Main.m3"
 /* load */
#line 135 "../Main.m3"
 /* load_indirect */
#line 135 "../Main.m3"
 /* load_indirect */
#line 135 "../Main.m3"
 /* check_nil */
#line 135 "../Main.m3"
 /* store */
#line 135 "../Main.m3"
(*(ADDRESS*)(&Main_m_146_L_147))=(ADDRESS)(((ADDRESS)(*((ADDRESS*)(*((ADDRESS*)(Main_m_144_L_145)))))));
#line 135 "../Main.m3"
 /* load */
#line 135 "../Main.m3"
/*check_nil*/if(!Main_m_146_L_147)Main_m_M_Main_L_49_CRASH(4324);
#line 135 "../Main.m3"
 /* call_indirect */
#line 135 "../Main.m3"
((void (__cdecl*)(void*))Main_m_146_L_147)(
 ((ADDRESS)(Main_m_144_L_145)));
#line 135 "../Main.m3"
 /* set_label */
#line 135 "../Main.m3"
 /* start_try */
#line 135 "../Main.m3"
try {
#line 135 "../Main.m3"
 /* load */
#line 135 "../Main.m3"
 /* store */
#line 135 "../Main.m3"
(*(UINT8*)(&LHolder_L_61))=(INT64)( ((INT64)(*((UINT8*)(INT64_(117)+((ADDRESS)(&Main_m_M_Main_L_49)))))));
#line 135 "../Main.m3"
 /* jump */
#line 135 "../Main.m3"
goto L42;
#line 135 "../Main.m3"
 /* end_try */
#line 135 "../Main.m3"
} catch (...) { throw; }
#line 135 "../Main.m3"
 /* set_label */
#line 135 "../Main.m3"
 /* landing_pad */
#line 135 "../Main.m3"
 /* store */
#line 135 "../Main.m3"
(*(ADDRESS*)(&Main_m_140_L_141))=(ADDRESS)(((ADDRESS)(_m3_caught)));
#line 135 "../Main.m3"
 /* set_label */
#line 135 "../Main.m3"
L42:;
#line 135 "../Main.m3"
 /* start_call_indirect */
#line 135 "../Main.m3"
 /* load */
#line 135 "../Main.m3"
 /* pop_param */
#line 135 "../Main.m3"
 /* load */
#line 135 "../Main.m3"
 /* load_indirect */
#line 135 "../Main.m3"
 /* load_indirect */
#line 135 "../Main.m3"
 /* check_nil */
#line 135 "../Main.m3"
 /* store */
#line 135 "../Main.m3"
(*(ADDRESS*)(&Main_m_148_L_149))=(ADDRESS)(((ADDRESS)(*((ADDRESS*)(INT64_(8)+((ADDRESS)(*((ADDRESS*)(Main_m_144_L_145)))))))));
#line 135 "../Main.m3"
 /* load */
#line 135 "../Main.m3"
/*check_nil*/if(!Main_m_148_L_149)Main_m_M_Main_L_49_CRASH(4324);
#line 135 "../Main.m3"
 /* call_indirect */
#line 135 "../Main.m3"
((void (__cdecl*)(void*))Main_m_148_L_149)(
 ((ADDRESS)(Main_m_144_L_145)));
#line 135 "../Main.m3"
 /* load_nil */
#line 135 "../Main.m3"
 /* load */
#line 135 "../Main.m3"
 /* if_compare */
#line 135 "../Main.m3"
if(m3_eq(ADDRESS,
 ((ADDRESS)(0)),
 ((ADDRESS)(Main_m_140_L_141))))goto L43;
#line 135 "../Main.m3"
 /* start_call_direct */
#line 135 "../Main.m3"
 /* load */
#line 135 "../Main.m3"
 /* pop_param */
#line 135 "../Main.m3"
 /* call_direct */
#line 135 "../Main.m3"
RTHooks__ResumeRaise(
  ( ADDRESS )(((ADDRESS)(Main_m_140_L_141)) ));
#line 135 "../Main.m3"
 /* set_label */
#line 135 "../Main.m3"
L43:;
#line 135 "../Main.m3"
 /* set_source_line */
#line 135 "../Main.m3"
#line 136 "../Main.m3"
 /* load_integer */
#line 136 "../Main.m3"
 /* load */
#line 136 "../Main.m3"
 /* if_compare */
#line 136 "../Main.m3"
if(m3_eq(INT64,
   INT64_(0),
  ((INT64)(LHolder_L_61))))goto L45;
#line 136 "../Main.m3"
 /* jump */
#line 136 "../Main.m3"
goto L3E;
#line 136 "../Main.m3"
 /* set_label */
#line 136 "../Main.m3"
L45:;
#line 136 "../Main.m3"
 /* jump */
#line 136 "../Main.m3"
goto L3D;
#line 136 "../Main.m3"
 /* set_label */
#line 136 "../Main.m3"
L3E:;
#line 136 "../Main.m3"
 /* set_source_line */
#line 136 "../Main.m3"
#line 138 "../Main.m3"
 /* load */
#line 138 "../Main.m3"
 /* exit_proc */
#line 138 "../Main.m3"
return ((INT64)(LHolder_L_61));
#line 138 "../Main.m3"
 /* end_procedure */
#line 138 "../Main.m3"
} /* WaitForStateSet */
#line 138 "../Main.m3"
 /* set_source_line */
#line 138 "../Main.m3"
#line 141 "../Main.m3"
 /* begin_procedure */
#line 141 "../Main.m3"
struct Main__WaitForStateSet_Frame_t {
#line 141 "../Main.m3"
ADDRESS _unused;
#line 141 "../Main.m3"
};
#line 141 "../Main.m3"
void /*TypeText3*/ 
__cdecl
Main__WaitForStateSet(
   /* Param_Type1 */ Main__ThreadNo ThN_L_63,
   /* Param_Type1 */ Main__StateSet Sts_L_64)
{
#line 141 "../Main.m3"
 /* Var_Type2 */ ADDRESS Main_m_150_L_151={0};//always-init
#line 141 "../Main.m3"
 /* Var_Type2 */ ADDRESS Main_m_152_L_153={0};//always-init
#line 141 "../Main.m3"
 /* Var_Type1 */ MUTEX Main_m_154_L_155={0};//always-init
#line 141 "../Main.m3"
 /* Var_Type2 */ ADDRESS Main_m_156_L_157={0};//always-init
#line 141 "../Main.m3"
 /* Var_Type2 */ INT64 Main_m_158_L_159={0};//always-init
#line 141 "../Main.m3"
 /* Var_Type1 */ T7609BE10* WSt_L_160={0};//always-init
#line 141 "../Main.m3"
 /* Var_Type2 */ ADDRESS Main_m_161_L_162={0};//always-init
#line 141 "../Main.m3"
Main__WaitForStateSet_Frame_t _frame;
#line 141 "../Main.m3"
_frame._unused=(ADDRESS)&_frame;
#line 141 "../Main.m3"
 /* set_source_line */
#line 141 "../Main.m3"
#line 143 "../Main.m3"
 /* set_source_line */
#line 143 "../Main.m3"
#line 144 "../Main.m3"
 /* set_label */
#line 144 "../Main.m3"
L46:;
#line 144 "../Main.m3"
 /* set_source_line */
#line 144 "../Main.m3"
#line 145 "../Main.m3"
 /* load_nil */
#line 145 "../Main.m3"
 /* store */
#line 145 "../Main.m3"
(*(ADDRESS*)(&Main_m_150_L_151))=(ADDRESS)(((ADDRESS)(0)));
#line 145 "../Main.m3"
 /* load */
#line 145 "../Main.m3"
 /* store */
#line 145 "../Main.m3"
(*(ADDRESS*)(&Main_m_152_L_153))=(ADDRESS)(((ADDRESS)(*((ADDRESS*)(INT64_(104)+((ADDRESS)(&Main_m_M_Main_L_49)))))));
#line 145 "../Main.m3"
 /* load_nil */
#line 145 "../Main.m3"
 /* load */
#line 145 "../Main.m3"
 /* if_compare */
#line 145 "../Main.m3"
if(m3_eq(ADDRESS,
 ((ADDRESS)(0)),
 ((ADDRESS)(Main_m_152_L_153))))goto L48;
#line 145 "../Main.m3"
 /* load */
#line 145 "../Main.m3"
 /* loophole */
#line 145 "../Main.m3"
 /* load_integer */
#line 145 "../Main.m3"
 /* and */
#line 145 "../Main.m3"
 /* if_true_or_false */
#line 145 "../Main.m3"
 /* load_host_integer */
#line 145 "../Main.m3"
 /* load_integer */
#line 145 "../Main.m3"
 /* if_compare */
#line 145 "../Main.m3"
if(m3_ne(INT64,
  ((INT64)(((INT64)((INT64)Main_m_152_L_153))&  INT64_(1))),
   INT64_(0)))goto L48;
#line 145 "../Main.m3"
 /* load */
#line 145 "../Main.m3"
 /* load_indirect */
#line 145 "../Main.m3"
 /* extract_mn */
#line 145 "../Main.m3"
 /* load_host_integer */
#line 145 "../Main.m3"
 /* load_integer */
#line 145 "../Main.m3"
 /* load_host_integer */
#line 145 "../Main.m3"
 /* load_integer */
#line 145 "../Main.m3"
 /* extract */
#line 145 "../Main.m3"
 /* if_true_or_false */
#line 145 "../Main.m3"
 /* load_host_integer */
#line 145 "../Main.m3"
 /* load_integer */
#line 145 "../Main.m3"
 /* if_compare */
#line 145 "../Main.m3"
if(m3_eq(INT64,
 ((INT64)(m3_extract(
 UINT64,
  *((INT64*)(INT64_(-8)+((ADDRESS)(Main_m_152_L_153)))),
   UINT64_(22),
   UINT64_(1)))),
   INT64_(0)))goto L48;
#line 145 "../Main.m3"
 /* start_call_direct */
#line 145 "../Main.m3"
 /* load */
#line 145 "../Main.m3"
 /* pop_param */
#line 145 "../Main.m3"
 /* call_direct */
#line 145 "../Main.m3"
RTHooks__CheckLoadTracedRef(
  ( REFANY )(((ADDRESS)(Main_m_152_L_153)) ));
#line 145 "../Main.m3"
 /* set_label */
#line 145 "../Main.m3"
L48:;
#line 145 "../Main.m3"
 /* load */
#line 145 "../Main.m3"
 /* store */
#line 145 "../Main.m3"
(*(ADDRESS*)(&Main_m_154_L_155))=(ADDRESS)(((ADDRESS)(Main_m_152_L_153)));
#line 145 "../Main.m3"
 /* start_call_indirect */
#line 145 "../Main.m3"
 /* load */
#line 145 "../Main.m3"
 /* pop_param */
#line 145 "../Main.m3"
 /* load */
#line 145 "../Main.m3"
 /* load_indirect */
#line 145 "../Main.m3"
 /* load_indirect */
#line 145 "../Main.m3"
 /* check_nil */
#line 145 "../Main.m3"
 /* store */
#line 145 "../Main.m3"
(*(ADDRESS*)(&Main_m_156_L_157))=(ADDRESS)(((ADDRESS)(*((ADDRESS*)(*((ADDRESS*)(Main_m_154_L_155)))))));
#line 145 "../Main.m3"
 /* load */
#line 145 "../Main.m3"
/*check_nil*/if(!Main_m_156_L_157)Main_m_M_Main_L_49_CRASH(4644);
#line 145 "../Main.m3"
 /* call_indirect */
#line 145 "../Main.m3"
((void (__cdecl*)(void*))Main_m_156_L_157)(
 ((ADDRESS)(Main_m_154_L_155)));
#line 145 "../Main.m3"
 /* set_label */
#line 145 "../Main.m3"
 /* start_try */
#line 145 "../Main.m3"
try {
#line 145 "../Main.m3"
 /* set_source_line */
#line 145 "../Main.m3"
#line 146 "../Main.m3"
 /* load */
#line 146 "../Main.m3"
 /* store */
#line 146 "../Main.m3"
(*(INT64*)(&Main_m_158_L_159))=(INT64)( ((INT64)(ThN_L_63)));
#line 146 "../Main.m3"
 /* load_address */
#line 146 "../Main.m3"
 /* load */
#line 146 "../Main.m3"
 /* index_address */
#line 146 "../Main.m3"
 /* store */
#line 146 "../Main.m3"
(*(ADDRESS*)(&Main_m_152_L_153))=(ADDRESS)(((ADDRESS)((((ADDRESS)(INT64_(112)+((ADDRESS)(&Main_m_M_Main_L_49))))+( Main_m_158_L_159)))));
#line 146 "../Main.m3"
 /* begin_block */
#line 146 "../Main.m3"
 /* load */
#line 146 "../Main.m3"
 /* store */
#line 146 "../Main.m3"
(*(ADDRESS*)(&WSt_L_160))=(ADDRESS)(((ADDRESS)(Main_m_152_L_153)));
#line 146 "../Main.m3"
 /* set_source_line */
#line 146 "../Main.m3"
#line 147 "../Main.m3"
 /* load */
#line 147 "../Main.m3"
 /* load_indirect */
#line 147 "../Main.m3"
 /* loophole */
#line 147 "../Main.m3"
 /* load */
#line 147 "../Main.m3"
 /* swap */
#line 147 "../Main.m3"
 /* load_integer */
#line 147 "../Main.m3"
 /* swap */
#line 147 "../Main.m3"
 /* shift_left */
#line 147 "../Main.m3"
 /* and */
#line 147 "../Main.m3"
 /* load_integer */
#line 147 "../Main.m3"
 /* compare */
#line 147 "../Main.m3"
 /* if_true_or_false */
#line 147 "../Main.m3"
 /* load_host_integer */
#line 147 "../Main.m3"
 /* load_integer */
#line 147 "../Main.m3"
 /* if_compare */
#line 147 "../Main.m3"
if(m3_eq(INT64,
  ((INT64)(m3_ne(UINT64,
 ((INT64)( ((INT64)(Sts_L_64))& ((INT64)(((UINT64)(  INT64_(1)))<<((UINT64)((INT64)((INT64)(*((UINT8*)(WSt_L_160)))))))))),
  INT64_(0)))),
   INT64_(0)))goto L4E;
#line 147 "../Main.m3"
 /* set_source_line */
#line 147 "../Main.m3"
#line 149 "../Main.m3"
 /* load_integer */
#line 149 "../Main.m3"
 /* store */
#line 149 "../Main.m3"
(*(INT64*)(&Main_m_150_L_151))=(INT64)(  INT64_(-2));
#line 149 "../Main.m3"
 /* jump */
#line 149 "../Main.m3"
goto L4B;
#line 149 "../Main.m3"
 /* set_label */
#line 149 "../Main.m3"
L4E:;
#line 149 "../Main.m3"
 /* end_block */
#line 149 "../Main.m3"
 /* jump */
#line 149 "../Main.m3"
goto L4B;
#line 149 "../Main.m3"
 /* end_try */
#line 149 "../Main.m3"
} catch (...) { throw; }
#line 149 "../Main.m3"
 /* set_label */
#line 149 "../Main.m3"
 /* landing_pad */
#line 149 "../Main.m3"
 /* store */
#line 149 "../Main.m3"
(*(ADDRESS*)(&Main_m_150_L_151))=(ADDRESS)(((ADDRESS)(_m3_caught)));
#line 149 "../Main.m3"
 /* set_label */
#line 149 "../Main.m3"
L4B:;
#line 149 "../Main.m3"
 /* start_call_indirect */
#line 149 "../Main.m3"
 /* load */
#line 149 "../Main.m3"
 /* pop_param */
#line 149 "../Main.m3"
 /* load */
#line 149 "../Main.m3"
 /* load_indirect */
#line 149 "../Main.m3"
 /* load_indirect */
#line 149 "../Main.m3"
 /* check_nil */
#line 149 "../Main.m3"
 /* store */
#line 149 "../Main.m3"
(*(ADDRESS*)(&Main_m_161_L_162))=(ADDRESS)(((ADDRESS)(*((ADDRESS*)(INT64_(8)+((ADDRESS)(*((ADDRESS*)(Main_m_154_L_155)))))))));
#line 149 "../Main.m3"
 /* load */
#line 149 "../Main.m3"
/*check_nil*/if(!Main_m_161_L_162)Main_m_M_Main_L_49_CRASH(4772);
#line 149 "../Main.m3"
 /* call_indirect */
#line 149 "../Main.m3"
((void (__cdecl*)(void*))Main_m_161_L_162)(
 ((ADDRESS)(Main_m_154_L_155)));
#line 149 "../Main.m3"
 /* load */
#line 149 "../Main.m3"
 /* loophole */
#line 149 "../Main.m3"
 /* load_integer */
#line 149 "../Main.m3"
 /* if_compare */
#line 149 "../Main.m3"
if(m3_ne(INT64,
 ((INT64)((INT64)Main_m_150_L_151)),
   INT64_(-2)))goto L4F;
#line 149 "../Main.m3"
 /* jump */
#line 149 "../Main.m3"
goto L47;
#line 149 "../Main.m3"
 /* set_label */
#line 149 "../Main.m3"
L4F:;
#line 149 "../Main.m3"
 /* load_nil */
#line 149 "../Main.m3"
 /* load */
#line 149 "../Main.m3"
 /* if_compare */
#line 149 "../Main.m3"
if(m3_eq(ADDRESS,
 ((ADDRESS)(0)),
 ((ADDRESS)(Main_m_150_L_151))))goto L4C;
#line 149 "../Main.m3"
 /* start_call_direct */
#line 149 "../Main.m3"
 /* load */
#line 149 "../Main.m3"
 /* pop_param */
#line 149 "../Main.m3"
 /* call_direct */
#line 149 "../Main.m3"
RTHooks__ResumeRaise(
  ( ADDRESS )(((ADDRESS)(Main_m_150_L_151)) ));
#line 149 "../Main.m3"
 /* set_source_line */
#line 149 "../Main.m3"
#line 152 "../Main.m3"
 /* set_label */
#line 152 "../Main.m3"
L4C:;
#line 152 "../Main.m3"
 /* set_source_line */
#line 152 "../Main.m3"
#line 153 "../Main.m3"
 /* start_call_direct */
#line 153 "../Main.m3"
 /* load_float */
#line 153 "../Main.m3"
 /* pop_param */
#line 153 "../Main.m3"
 /* call_direct */
#line 153 "../Main.m3"
Thread__Pause(
  ( LONGREAL )( ((double)(1.00000000000000006e-1)) ));
#line 153 "../Main.m3"
 /* jump */
#line 153 "../Main.m3"
goto L46;
#line 153 "../Main.m3"
 /* set_label */
#line 153 "../Main.m3"
L47:;
#line 153 "../Main.m3"
 /* set_source_line */
#line 153 "../Main.m3"
#line 155 "../Main.m3"
 /* exit_proc */
#line 155 "../Main.m3"
return;
#line 155 "../Main.m3"
 /* end_procedure */
#line 155 "../Main.m3"
} /* WaitForState */
#line 155 "../Main.m3"
 /* set_source_line */
#line 155 "../Main.m3"
#line 157 "../Main.m3"
 /* begin_procedure */
#line 157 "../Main.m3"
struct Main__WaitForState_Frame_t {
#line 157 "../Main.m3"
ADDRESS _unused;
#line 157 "../Main.m3"
};
#line 157 "../Main.m3"
void /*TypeText3*/ 
__cdecl
Main__WaitForState(
   /* Param_Type1 */ Main__ThreadNo ThN_L_65,
   /* Param_Type1 */ Main__State St_L_66)
{
#line 157 "../Main.m3"
Main__WaitForState_Frame_t _frame;
#line 157 "../Main.m3"
_frame._unused=(ADDRESS)&_frame;
#line 157 "../Main.m3"
 /* set_source_line */
#line 157 "../Main.m3"
#line 159 "../Main.m3"
 /* set_source_line */
#line 159 "../Main.m3"
#line 160 "../Main.m3"
 /* start_call_direct */
#line 160 "../Main.m3"
 /* load */
#line 160 "../Main.m3"
 /* pop_param */
#line 160 "../Main.m3"
 /* load_integer */
#line 160 "../Main.m3"
 /* load */
#line 160 "../Main.m3"
 /* loophole */
#line 160 "../Main.m3"
 /* load_integer */
#line 160 "../Main.m3"
 /* swap */
#line 160 "../Main.m3"
 /* shift_left */
#line 160 "../Main.m3"
 /* or */
#line 160 "../Main.m3"
 /* pop_param */
#line 160 "../Main.m3"
 /* call_direct */
#line 160 "../Main.m3"
Main__WaitForStateSet(
  ( Main__ThreadNo )(((UINT8)(((INT64)(ThN_L_65)))) ),
  ( Main__StateSet )(((UINT8)(((INT64)(  INT64_(0)| ((INT64)(((UINT64)(  INT64_(1)))<<((UINT64)((INT64)((INT64)(St_L_66)))))))))) ));
#line 160 "../Main.m3"
 /* set_source_line */
#line 160 "../Main.m3"
#line 161 "../Main.m3"
 /* exit_proc */
#line 161 "../Main.m3"
return;
#line 161 "../Main.m3"
 /* end_procedure */
#line 161 "../Main.m3"
} /* NoteWhetherStateSet */
#line 161 "../Main.m3"
 /* set_source_line */
#line 161 "../Main.m3"
#line 164 "../Main.m3"
 /* begin_procedure */
#line 164 "../Main.m3"
struct Main__NoteWhetherStateSet_Frame_t {
#line 164 "../Main.m3"
ADDRESS _unused;
#line 164 "../Main.m3"
};
#line 164 "../Main.m3"
BOOLEAN
__cdecl
Main__NoteWhetherStateSet(
   /* Param_Type1 */ Main__ThreadNo ThN_L_71,
   /* Param_Type1 */ Main__StateSet Sts_L_72,
   /* Param_Type1 */ TEXT YesMsg_L_73,
   /* Param_Type1 */ TEXT NoMsg_L_74)
{
#line 164 "../Main.m3"
 /* Var_Type1 */ BOOLEAN LResult_L_67={0};//always-init
#line 164 "../Main.m3"
 /* Var_Type1 */ T7609BE10 LState_L_68={0};//always-init
#line 164 "../Main.m3"
 /* Var_Type1 */ TEXT LMsg_L_69={0};//always-init
#line 164 "../Main.m3"
 /* Var_Type2 */ ADDRESS Main_m_163_L_164={0};//always-init
#line 164 "../Main.m3"
 /* Var_Type2 */ ADDRESS Main_m_165_L_166={0};//always-init
#line 164 "../Main.m3"
 /* Var_Type1 */ MUTEX Main_m_167_L_168={0};//always-init
#line 164 "../Main.m3"
 /* Var_Type2 */ ADDRESS Main_m_169_L_170={0};//always-init
#line 164 "../Main.m3"
 /* Var_Type2 */ INT64 Main_m_171_L_172={0};//always-init
#line 164 "../Main.m3"
 /* Var_Type2 */ ADDRESS Main_m_173_L_174={0};//always-init
#line 164 "../Main.m3"
 /* Var_Type2 */ ADDRESS Main_m_175_L_176={0};//always-init
#line 164 "../Main.m3"
 /* Var_Type2 */ ADDRESS Main_m_177_L_178={0};//always-init
#line 164 "../Main.m3"
 /* Var_Type2 */ ADDRESS Main_m_179_L_180={0};//always-init
#line 164 "../Main.m3"
Main__NoteWhetherStateSet_Frame_t _frame;
#line 164 "../Main.m3"
_frame._unused=(ADDRESS)&_frame;
#line 164 "../Main.m3"
 /* set_source_line */
#line 164 "../Main.m3"
#line 167 "../Main.m3"
 /* load_integer */
#line 167 "../Main.m3"
 /* store */
#line 167 "../Main.m3"
(*(UINT8*)(&LResult_L_67))=(INT64)(  INT64_(0));
#line 167 "../Main.m3"
 /* set_source_line */
#line 167 "../Main.m3"
#line 168 "../Main.m3"
 /* load_integer */
#line 168 "../Main.m3"
 /* store */
#line 168 "../Main.m3"
(*(UINT8*)(&LState_L_68))=(INT64)(  INT64_(0));
#line 168 "../Main.m3"
 /* set_source_line */
#line 168 "../Main.m3"
#line 169 "../Main.m3"
 /* load_nil */
#line 169 "../Main.m3"
 /* store */
#line 169 "../Main.m3"
(*(ADDRESS*)(&LMsg_L_69))=(ADDRESS)(((ADDRESS)(0)));
#line 169 "../Main.m3"
 /* set_source_line */
#line 169 "../Main.m3"
#line 167 "../Main.m3"
 /* set_source_line */
#line 167 "../Main.m3"
#line 172 "../Main.m3"
 /* load_nil */
#line 172 "../Main.m3"
 /* store */
#line 172 "../Main.m3"
(*(ADDRESS*)(&Main_m_163_L_164))=(ADDRESS)(((ADDRESS)(0)));
#line 172 "../Main.m3"
 /* load */
#line 172 "../Main.m3"
 /* store */
#line 172 "../Main.m3"
(*(ADDRESS*)(&Main_m_165_L_166))=(ADDRESS)(((ADDRESS)(*((ADDRESS*)(INT64_(104)+((ADDRESS)(&Main_m_M_Main_L_49)))))));
#line 172 "../Main.m3"
 /* load_nil */
#line 172 "../Main.m3"
 /* load */
#line 172 "../Main.m3"
 /* if_compare */
#line 172 "../Main.m3"
if(m3_eq(ADDRESS,
 ((ADDRESS)(0)),
 ((ADDRESS)(Main_m_165_L_166))))goto L50;
#line 172 "../Main.m3"
 /* load */
#line 172 "../Main.m3"
 /* loophole */
#line 172 "../Main.m3"
 /* load_integer */
#line 172 "../Main.m3"
 /* and */
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
  ((INT64)(((INT64)((INT64)Main_m_165_L_166))&  INT64_(1))),
   INT64_(0)))goto L50;
#line 172 "../Main.m3"
 /* load */
#line 172 "../Main.m3"
 /* load_indirect */
#line 172 "../Main.m3"
 /* extract_mn */
#line 172 "../Main.m3"
 /* load_host_integer */
#line 172 "../Main.m3"
 /* load_integer */
#line 172 "../Main.m3"
 /* load_host_integer */
#line 172 "../Main.m3"
 /* load_integer */
#line 172 "../Main.m3"
 /* extract */
#line 172 "../Main.m3"
 /* if_true_or_false */
#line 172 "../Main.m3"
 /* load_host_integer */
#line 172 "../Main.m3"
 /* load_integer */
#line 172 "../Main.m3"
 /* if_compare */
#line 172 "../Main.m3"
if(m3_eq(INT64,
 ((INT64)(m3_extract(
 UINT64,
  *((INT64*)(INT64_(-8)+((ADDRESS)(Main_m_165_L_166)))),
   UINT64_(22),
   UINT64_(1)))),
   INT64_(0)))goto L50;
#line 172 "../Main.m3"
 /* start_call_direct */
#line 172 "../Main.m3"
 /* load */
#line 172 "../Main.m3"
 /* pop_param */
#line 172 "../Main.m3"
 /* call_direct */
#line 172 "../Main.m3"
RTHooks__CheckLoadTracedRef(
  ( REFANY )(((ADDRESS)(Main_m_165_L_166)) ));
#line 172 "../Main.m3"
 /* set_label */
#line 172 "../Main.m3"
L50:;
#line 172 "../Main.m3"
 /* load */
#line 172 "../Main.m3"
 /* store */
#line 172 "../Main.m3"
(*(ADDRESS*)(&Main_m_167_L_168))=(ADDRESS)(((ADDRESS)(Main_m_165_L_166)));
#line 172 "../Main.m3"
 /* start_call_indirect */
#line 172 "../Main.m3"
 /* load */
#line 172 "../Main.m3"
 /* pop_param */
#line 172 "../Main.m3"
 /* load */
#line 172 "../Main.m3"
 /* load_indirect */
#line 172 "../Main.m3"
 /* load_indirect */
#line 172 "../Main.m3"
 /* check_nil */
#line 172 "../Main.m3"
 /* store */
#line 172 "../Main.m3"
(*(ADDRESS*)(&Main_m_169_L_170))=(ADDRESS)(((ADDRESS)(*((ADDRESS*)(*((ADDRESS*)(Main_m_167_L_168)))))));
#line 172 "../Main.m3"
 /* load */
#line 172 "../Main.m3"
/*check_nil*/if(!Main_m_169_L_170)Main_m_M_Main_L_49_CRASH(5508);
#line 172 "../Main.m3"
 /* call_indirect */
#line 172 "../Main.m3"
((void (__cdecl*)(void*))Main_m_169_L_170)(
 ((ADDRESS)(Main_m_167_L_168)));
#line 172 "../Main.m3"
 /* set_label */
#line 172 "../Main.m3"
 /* start_try */
#line 172 "../Main.m3"
try {
#line 172 "../Main.m3"
 /* set_source_line */
#line 172 "../Main.m3"
#line 174 "../Main.m3"
 /* load */
#line 174 "../Main.m3"
 /* store */
#line 174 "../Main.m3"
(*(INT64*)(&Main_m_171_L_172))=(INT64)( ((INT64)(ThN_L_71)));
#line 174 "../Main.m3"
 /* load_address */
#line 174 "../Main.m3"
 /* load */
#line 174 "../Main.m3"
 /* index_address */
#line 174 "../Main.m3"
 /* store */
#line 174 "../Main.m3"
(*(ADDRESS*)(&Main_m_165_L_166))=(ADDRESS)(((ADDRESS)((((ADDRESS)(INT64_(112)+((ADDRESS)(&Main_m_M_Main_L_49))))+( Main_m_171_L_172)))));
#line 174 "../Main.m3"
 /* load */
#line 174 "../Main.m3"
 /* load_indirect */
#line 174 "../Main.m3"
 /* store */
#line 174 "../Main.m3"
(*(UINT8*)(&LState_L_68))=(INT64)( ((INT64)(*((UINT8*)(Main_m_165_L_166)))));
#line 174 "../Main.m3"
 /* set_source_line */
#line 174 "../Main.m3"
#line 175 "../Main.m3"
 /* load */
#line 175 "../Main.m3"
 /* loophole */
#line 175 "../Main.m3"
 /* load */
#line 175 "../Main.m3"
 /* swap */
#line 175 "../Main.m3"
 /* load_integer */
#line 175 "../Main.m3"
 /* swap */
#line 175 "../Main.m3"
 /* shift_left */
#line 175 "../Main.m3"
 /* and */
#line 175 "../Main.m3"
 /* load_integer */
#line 175 "../Main.m3"
 /* compare */
#line 175 "../Main.m3"
 /* store */
#line 175 "../Main.m3"
(*(UINT8*)(&LResult_L_67))=(INT64)( ((INT64)(m3_ne(UINT64,
 ((INT64)( ((INT64)(Sts_L_72))& ((INT64)(((UINT64)(  INT64_(1)))<<((UINT64)((INT64)((INT64)(LState_L_68)))))))),
  INT64_(0)))));
#line 175 "../Main.m3"
 /* set_source_line */
#line 175 "../Main.m3"
#line 176 "../Main.m3"
 /* start_call_direct */
#line 176 "../Main.m3"
 /* load */
#line 176 "../Main.m3"
 /* pop_param */
#line 176 "../Main.m3"
 /* invoke_direct */
#line 176 "../Main.m3"
 /* call_direct */
#line 176 "../Main.m3"
 /* set_label */
#line 176 "../Main.m3"
 /* store */
#line 176 "../Main.m3"
(*(ADDRESS*)(&Main_m_165_L_166))=(ADDRESS)(((ADDRESS)(Main__ThImage(
  ( Main__ThreadNo )(((UINT8)(((INT64)(ThN_L_71)))) )))));
#line 176 "../Main.m3"
 /* start_call_direct */
#line 176 "../Main.m3"
 /* load */
#line 176 "../Main.m3"
 /* pop_param */
#line 176 "../Main.m3"
 /* load_address */
#line 176 "../Main.m3"
 /* pop_param */
#line 176 "../Main.m3"
 /* invoke_direct */
#line 176 "../Main.m3"
 /* call_direct */
#line 176 "../Main.m3"
 /* set_label */
#line 176 "../Main.m3"
 /* store */
#line 176 "../Main.m3"
(*(ADDRESS*)(&Main_m_173_L_174))=(ADDRESS)(((ADDRESS)(RTHooks__Concat(
  ( TEXT )(((ADDRESS)(Main_m_165_L_166)) ),
  ( TEXT )(((ADDRESS)(INT64_(608)+((ADDRESS)(&Main_m_47_L_48)))) )))));
#line 176 "../Main.m3"
 /* start_call_direct */
#line 176 "../Main.m3"
 /* load */
#line 176 "../Main.m3"
 /* pop_param */
#line 176 "../Main.m3"
 /* invoke_direct */
#line 176 "../Main.m3"
 /* call_direct */
#line 176 "../Main.m3"
 /* set_label */
#line 176 "../Main.m3"
 /* store */
#line 176 "../Main.m3"
(*(ADDRESS*)(&Main_m_175_L_176))=(ADDRESS)(((ADDRESS)(Main__StateImage(
  ( Main__State )(((UINT8)(((INT64)(LState_L_68)))) )))));
#line 176 "../Main.m3"
 /* start_call_direct */
#line 176 "../Main.m3"
 /* load */
#line 176 "../Main.m3"
 /* pop_param */
#line 176 "../Main.m3"
 /* load */
#line 176 "../Main.m3"
 /* pop_param */
#line 176 "../Main.m3"
 /* invoke_direct */
#line 176 "../Main.m3"
 /* call_direct */
#line 176 "../Main.m3"
 /* set_label */
#line 176 "../Main.m3"
 /* store */
#line 176 "../Main.m3"
(*(ADDRESS*)(&Main_m_177_L_178))=(ADDRESS)(((ADDRESS)(RTHooks__Concat(
  ( TEXT )(((ADDRESS)(Main_m_173_L_174)) ),
  ( TEXT )(((ADDRESS)(Main_m_175_L_176)) )))));
#line 176 "../Main.m3"
 /* load */
#line 176 "../Main.m3"
 /* store */
#line 176 "../Main.m3"
(*(ADDRESS*)(&LMsg_L_69))=(ADDRESS)(((ADDRESS)(Main_m_177_L_178)));
#line 176 "../Main.m3"
 /* set_source_line */
#line 176 "../Main.m3"
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
  ((INT64)(LResult_L_67)),
   INT64_(0)))goto L5A;
#line 177 "../Main.m3"
 /* set_source_line */
#line 177 "../Main.m3"
#line 178 "../Main.m3"
 /* load_nil */
#line 178 "../Main.m3"
 /* load */
#line 178 "../Main.m3"
 /* if_compare */
#line 178 "../Main.m3"
if(m3_eq(ADDRESS,
 ((ADDRESS)(0)),
 ((ADDRESS)(YesMsg_L_73))))goto L5C;
#line 178 "../Main.m3"
 /* start_call_direct */
#line 178 "../Main.m3"
 /* load */
#line 178 "../Main.m3"
 /* pop_param */
#line 178 "../Main.m3"
 /* load */
#line 178 "../Main.m3"
 /* pop_param */
#line 178 "../Main.m3"
 /* invoke_direct */
#line 178 "../Main.m3"
 /* call_direct */
#line 178 "../Main.m3"
 /* set_label */
#line 178 "../Main.m3"
 /* store */
#line 178 "../Main.m3"
(*(ADDRESS*)(&Main_m_177_L_178))=(ADDRESS)(((ADDRESS)(RTHooks__Concat(
  ( TEXT )(((ADDRESS)(LMsg_L_69)) ),
  ( TEXT )(((ADDRESS)(YesMsg_L_73)) )))));
#line 178 "../Main.m3"
 /* load */
#line 178 "../Main.m3"
 /* store */
#line 178 "../Main.m3"
(*(ADDRESS*)(&LMsg_L_69))=(ADDRESS)(((ADDRESS)(Main_m_177_L_178)));
#line 178 "../Main.m3"
 /* set_label */
#line 178 "../Main.m3"
L5C:;
#line 178 "../Main.m3"
 /* jump */
#line 178 "../Main.m3"
goto L59;
#line 178 "../Main.m3"
 /* set_label */
#line 178 "../Main.m3"
L5A:;
#line 178 "../Main.m3"
 /* set_source_line */
#line 178 "../Main.m3"
#line 179 "../Main.m3"
 /* load_nil */
#line 179 "../Main.m3"
 /* load */
#line 179 "../Main.m3"
 /* if_compare */
#line 179 "../Main.m3"
if(m3_eq(ADDRESS,
 ((ADDRESS)(0)),
 ((ADDRESS)(NoMsg_L_74))))goto L5F;
#line 179 "../Main.m3"
 /* start_call_direct */
#line 179 "../Main.m3"
 /* load */
#line 179 "../Main.m3"
 /* pop_param */
#line 179 "../Main.m3"
 /* load */
#line 179 "../Main.m3"
 /* pop_param */
#line 179 "../Main.m3"
 /* invoke_direct */
#line 179 "../Main.m3"
 /* call_direct */
#line 179 "../Main.m3"
 /* set_label */
#line 179 "../Main.m3"
 /* store */
#line 179 "../Main.m3"
(*(ADDRESS*)(&Main_m_177_L_178))=(ADDRESS)(((ADDRESS)(RTHooks__Concat(
  ( TEXT )(((ADDRESS)(LMsg_L_69)) ),
  ( TEXT )(((ADDRESS)(NoMsg_L_74)) )))));
#line 179 "../Main.m3"
 /* load */
#line 179 "../Main.m3"
 /* store */
#line 179 "../Main.m3"
(*(ADDRESS*)(&LMsg_L_69))=(ADDRESS)(((ADDRESS)(Main_m_177_L_178)));
#line 179 "../Main.m3"
 /* set_label */
#line 179 "../Main.m3"
L5F:;
#line 179 "../Main.m3"
 /* set_label */
#line 179 "../Main.m3"
L59:;
#line 179 "../Main.m3"
 /* set_source_line */
#line 179 "../Main.m3"
#line 181 "../Main.m3"
 /* start_call_direct */
#line 181 "../Main.m3"
 /* load */
#line 181 "../Main.m3"
 /* pop_param */
#line 181 "../Main.m3"
 /* invoke_direct */
#line 181 "../Main.m3"
 /* call_direct */
#line 181 "../Main.m3"
Main__W(
  ( TEXT )(((ADDRESS)(LMsg_L_69)) ));
#line 181 "../Main.m3"
 /* set_label */
#line 181 "../Main.m3"
 /* jump */
#line 181 "../Main.m3"
goto L53;
#line 181 "../Main.m3"
 /* end_try */
#line 181 "../Main.m3"
} catch (_M3Exc& _m3exc) { _m3_caught = _m3exc.act; goto L52; }
#line 181 "../Main.m3"
 /* set_label */
#line 181 "../Main.m3"
L52:;
#line 181 "../Main.m3"
 /* landing_pad */
#line 181 "../Main.m3"
 /* store */
#line 181 "../Main.m3"
(*(ADDRESS*)(&Main_m_163_L_164))=(ADDRESS)(((ADDRESS)(_m3_caught)));
#line 181 "../Main.m3"
 /* set_label */
#line 181 "../Main.m3"
L53:;
#line 181 "../Main.m3"
 /* start_call_indirect */
#line 181 "../Main.m3"
 /* load */
#line 181 "../Main.m3"
 /* pop_param */
#line 181 "../Main.m3"
 /* load */
#line 181 "../Main.m3"
 /* load_indirect */
#line 181 "../Main.m3"
 /* load_indirect */
#line 181 "../Main.m3"
 /* check_nil */
#line 181 "../Main.m3"
 /* store */
#line 181 "../Main.m3"
(*(ADDRESS*)(&Main_m_179_L_180))=(ADDRESS)(((ADDRESS)(*((ADDRESS*)(INT64_(8)+((ADDRESS)(*((ADDRESS*)(Main_m_167_L_168)))))))));
#line 181 "../Main.m3"
 /* load */
#line 181 "../Main.m3"
/*check_nil*/if(!Main_m_179_L_180)Main_m_M_Main_L_49_CRASH(5796);
#line 181 "../Main.m3"
 /* call_indirect */
#line 181 "../Main.m3"
((void (__cdecl*)(void*))Main_m_179_L_180)(
 ((ADDRESS)(Main_m_167_L_168)));
#line 181 "../Main.m3"
 /* load_nil */
#line 181 "../Main.m3"
 /* load */
#line 181 "../Main.m3"
 /* if_compare */
#line 181 "../Main.m3"
if(m3_eq(ADDRESS,
 ((ADDRESS)(0)),
 ((ADDRESS)(Main_m_163_L_164))))goto L54;
#line 181 "../Main.m3"
 /* start_call_direct */
#line 181 "../Main.m3"
 /* load */
#line 181 "../Main.m3"
 /* pop_param */
#line 181 "../Main.m3"
 /* call_direct */
#line 181 "../Main.m3"
RTHooks__ResumeRaise(
  ( ADDRESS )(((ADDRESS)(Main_m_163_L_164)) ));
#line 181 "../Main.m3"
 /* set_source_line */
#line 181 "../Main.m3"
#line 182 "../Main.m3"
 /* set_label */
#line 182 "../Main.m3"
L54:;
#line 182 "../Main.m3"
 /* set_source_line */
#line 182 "../Main.m3"
#line 183 "../Main.m3"
 /* load */
#line 183 "../Main.m3"
 /* exit_proc */
#line 183 "../Main.m3"
return ((INT64)(LResult_L_67));
#line 183 "../Main.m3"
 /* end_procedure */
#line 183 "../Main.m3"
} /* NoteWhetherState */
#line 183 "../Main.m3"
 /* set_source_line */
#line 183 "../Main.m3"
#line 187 "../Main.m3"
 /* begin_procedure */
#line 187 "../Main.m3"
struct Main__NoteWhetherState_Frame_t {
#line 187 "../Main.m3"
ADDRESS _unused;
#line 187 "../Main.m3"
};
#line 187 "../Main.m3"
BOOLEAN
__cdecl
Main__NoteWhetherState(
   /* Param_Type1 */ Main__ThreadNo ThN_L_76,
   /* Param_Type1 */ Main__State St_L_77,
   /* Param_Type1 */ TEXT YesMsg_L_78,
   /* Param_Type1 */ TEXT NoMsg_L_79)
{
#line 187 "../Main.m3"
 /* Var_Type2 */ INT64 Main_m_181_L_182={0};//always-init
#line 187 "../Main.m3"
Main__NoteWhetherState_Frame_t _frame;
#line 187 "../Main.m3"
_frame._unused=(ADDRESS)&_frame;
#line 187 "../Main.m3"
 /* set_source_line */
#line 187 "../Main.m3"
#line 189 "../Main.m3"
 /* set_source_line */
#line 189 "../Main.m3"
#line 190 "../Main.m3"
 /* start_call_direct */
#line 190 "../Main.m3"
 /* load */
#line 190 "../Main.m3"
 /* pop_param */
#line 190 "../Main.m3"
 /* load_integer */
#line 190 "../Main.m3"
 /* load */
#line 190 "../Main.m3"
 /* loophole */
#line 190 "../Main.m3"
 /* load_integer */
#line 190 "../Main.m3"
 /* swap */
#line 190 "../Main.m3"
 /* shift_left */
#line 190 "../Main.m3"
 /* or */
#line 190 "../Main.m3"
 /* pop_param */
#line 190 "../Main.m3"
 /* load */
#line 190 "../Main.m3"
 /* pop_param */
#line 190 "../Main.m3"
 /* load */
#line 190 "../Main.m3"
 /* pop_param */
#line 190 "../Main.m3"
 /* call_direct */
#line 190 "../Main.m3"
 /* store */
#line 190 "../Main.m3"
(*(INT64*)(&Main_m_181_L_182))=(INT64)(((INT64)(Main__NoteWhetherStateSet(
  ( Main__ThreadNo )(((UINT8)(((INT64)(ThN_L_76)))) ),
  ( Main__StateSet )(((UINT8)(((INT64)(  INT64_(0)| ((INT64)(((UINT64)(  INT64_(1)))<<((UINT64)((INT64)((INT64)(St_L_77)))))))))) ),
  ( TEXT )(((ADDRESS)(YesMsg_L_78)) ),
  ( TEXT )(((ADDRESS)(NoMsg_L_79)) )))));
#line 190 "../Main.m3"
 /* load */
#line 190 "../Main.m3"
 /* exit_proc */
#line 190 "../Main.m3"
return Main_m_181_L_182;
#line 190 "../Main.m3"
 /* end_procedure */
#line 190 "../Main.m3"
} /* TestApply */
#line 190 "../Main.m3"
 /* set_source_line */
#line 190 "../Main.m3"
#line 205 "../Main.m3"
 /* begin_procedure */
#line 205 "../Main.m3"
struct Main__TestApply_Frame_t {
#line 205 "../Main.m3"
ADDRESS _unused;
#line 205 "../Main.m3"
};
#line 205 "../Main.m3"
REFANY
__cdecl
Main__TestApply(
   /* Param_Type1 */ Main__Cl Self_L_82)
{
#line 205 "../Main.m3"
 /* Var_Type1 */ T1B64A79D LProc_L_80={0};//always-init
#line 205 "../Main.m3"
 /* Var_Type2 */ ADDRESS Main_m_183_L_184={0};//always-init
#line 205 "../Main.m3"
 /* Var_Type2 */ ADDRESS Main_m_185_L_186={0};//always-init
#line 205 "../Main.m3"
 /* Var_Type1 */ MUTEX Main_m_187_L_188={0};//always-init
#line 205 "../Main.m3"
 /* Var_Type2 */ ADDRESS Main_m_189_L_190={0};//always-init
#line 205 "../Main.m3"
 /* Var_Type2 */ INT64 Main_m_191_L_192={0};//always-init
#line 205 "../Main.m3"
 /* Var_Type1 */ T1B64A79D* WProc_L_193={0};//always-init
#line 205 "../Main.m3"
 /* Var_Type2 */ ADDRESS Main_m_194_L_195={0};//always-init
#line 205 "../Main.m3"
 /* Var_Type2 */ ADDRESS Main_m_196_L_197={0};//always-init
#line 205 "../Main.m3"
Main__TestApply_Frame_t _frame;
#line 205 "../Main.m3"
_frame._unused=(ADDRESS)&_frame;
#line 205 "../Main.m3"
 /* set_source_line */
#line 205 "../Main.m3"
#line 207 "../Main.m3"
 /* load_nil */
#line 207 "../Main.m3"
 /* store */
#line 207 "../Main.m3"
(*(ADDRESS*)(&LProc_L_80))=(ADDRESS)(((ADDRESS)(0)));
#line 207 "../Main.m3"
 /* set_source_line */
#line 207 "../Main.m3"
#line 209 "../Main.m3"
 /* set_label */
#line 209 "../Main.m3"
L62:;
#line 209 "../Main.m3"
 /* set_source_line */
#line 209 "../Main.m3"
#line 210 "../Main.m3"
 /* start_call_direct */
#line 210 "../Main.m3"
 /* load */
#line 210 "../Main.m3"
 /* load_indirect */
#line 210 "../Main.m3"
 /* cvt_float */
#line 210 "../Main.m3"
 /* load_float */
#line 210 "../Main.m3"
 /* multiply */
#line 210 "../Main.m3"
 /* load_float */
#line 210 "../Main.m3"
 /* add */
#line 210 "../Main.m3"
 /* pop_param */
#line 210 "../Main.m3"
 /* call_direct */
#line 210 "../Main.m3"
Thread__Pause(
  ( LONGREAL )( ((double)( ((double)( ((double)(((UINT64)(((INT64)(*((UINT8*)(INT64_(8)+((ADDRESS)(Self_L_82))))))))))* ((double)(1.00000000000000002e-2))))+ ((double)(2.00000000000000011e-1)))) ));
#line 210 "../Main.m3"
 /* set_source_line */
#line 210 "../Main.m3"
#line 215 "../Main.m3"
 /* load_nil */
#line 215 "../Main.m3"
 /* store */
#line 215 "../Main.m3"
(*(ADDRESS*)(&Main_m_183_L_184))=(ADDRESS)(((ADDRESS)(0)));
#line 215 "../Main.m3"
 /* load */
#line 215 "../Main.m3"
 /* store */
#line 215 "../Main.m3"
(*(ADDRESS*)(&Main_m_185_L_186))=(ADDRESS)(((ADDRESS)(*((ADDRESS*)(INT64_(128)+((ADDRESS)(&Main_m_M_Main_L_49)))))));
#line 215 "../Main.m3"
 /* load_nil */
#line 215 "../Main.m3"
 /* load */
#line 215 "../Main.m3"
 /* if_compare */
#line 215 "../Main.m3"
if(m3_eq(ADDRESS,
 ((ADDRESS)(0)),
 ((ADDRESS)(Main_m_185_L_186))))goto L64;
#line 215 "../Main.m3"
 /* load */
#line 215 "../Main.m3"
 /* loophole */
#line 215 "../Main.m3"
 /* load_integer */
#line 215 "../Main.m3"
 /* and */
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
  ((INT64)(((INT64)((INT64)Main_m_185_L_186))&  INT64_(1))),
   INT64_(0)))goto L64;
#line 215 "../Main.m3"
 /* load */
#line 215 "../Main.m3"
 /* load_indirect */
#line 215 "../Main.m3"
 /* extract_mn */
#line 215 "../Main.m3"
 /* load_host_integer */
#line 215 "../Main.m3"
 /* load_integer */
#line 215 "../Main.m3"
 /* load_host_integer */
#line 215 "../Main.m3"
 /* load_integer */
#line 215 "../Main.m3"
 /* extract */
#line 215 "../Main.m3"
 /* if_true_or_false */
#line 215 "../Main.m3"
 /* load_host_integer */
#line 215 "../Main.m3"
 /* load_integer */
#line 215 "../Main.m3"
 /* if_compare */
#line 215 "../Main.m3"
if(m3_eq(INT64,
 ((INT64)(m3_extract(
 UINT64,
  *((INT64*)(INT64_(-8)+((ADDRESS)(Main_m_185_L_186)))),
   UINT64_(22),
   UINT64_(1)))),
   INT64_(0)))goto L64;
#line 215 "../Main.m3"
 /* start_call_direct */
#line 215 "../Main.m3"
 /* load */
#line 215 "../Main.m3"
 /* pop_param */
#line 215 "../Main.m3"
 /* call_direct */
#line 215 "../Main.m3"
RTHooks__CheckLoadTracedRef(
  ( REFANY )(((ADDRESS)(Main_m_185_L_186)) ));
#line 215 "../Main.m3"
 /* set_label */
#line 215 "../Main.m3"
L64:;
#line 215 "../Main.m3"
 /* load */
#line 215 "../Main.m3"
 /* store */
#line 215 "../Main.m3"
(*(ADDRESS*)(&Main_m_187_L_188))=(ADDRESS)(((ADDRESS)(Main_m_185_L_186)));
#line 215 "../Main.m3"
 /* start_call_indirect */
#line 215 "../Main.m3"
 /* load */
#line 215 "../Main.m3"
 /* pop_param */
#line 215 "../Main.m3"
 /* load */
#line 215 "../Main.m3"
 /* load_indirect */
#line 215 "../Main.m3"
 /* load_indirect */
#line 215 "../Main.m3"
 /* check_nil */
#line 215 "../Main.m3"
 /* store */
#line 215 "../Main.m3"
(*(ADDRESS*)(&Main_m_189_L_190))=(ADDRESS)(((ADDRESS)(*((ADDRESS*)(*((ADDRESS*)(Main_m_187_L_188)))))));
#line 215 "../Main.m3"
 /* load */
#line 215 "../Main.m3"
/*check_nil*/if(!Main_m_189_L_190)Main_m_M_Main_L_49_CRASH(6884);
#line 215 "../Main.m3"
 /* call_indirect */
#line 215 "../Main.m3"
((void (__cdecl*)(void*))Main_m_189_L_190)(
 ((ADDRESS)(Main_m_187_L_188)));
#line 215 "../Main.m3"
 /* set_label */
#line 215 "../Main.m3"
 /* start_try */
#line 215 "../Main.m3"
try {
#line 215 "../Main.m3"
 /* set_source_line */
#line 215 "../Main.m3"
#line 216 "../Main.m3"
 /* load */
#line 216 "../Main.m3"
 /* load_indirect */
#line 216 "../Main.m3"
 /* store */
#line 216 "../Main.m3"
(*(INT64*)(&Main_m_191_L_192))=(INT64)( ((INT64)(*((UINT8*)(INT64_(8)+((ADDRESS)(Self_L_82)))))));
#line 216 "../Main.m3"
 /* load_address */
#line 216 "../Main.m3"
 /* load */
#line 216 "../Main.m3"
 /* index_address */
#line 216 "../Main.m3"
 /* store */
#line 216 "../Main.m3"
(*(ADDRESS*)(&Main_m_185_L_186))=(ADDRESS)(((ADDRESS)((((ADDRESS)(INT64_(136)+((ADDRESS)(&Main_m_M_Main_L_49))))+(8*( Main_m_191_L_192))))));
#line 216 "../Main.m3"
 /* begin_block */
#line 216 "../Main.m3"
 /* load */
#line 216 "../Main.m3"
 /* store */
#line 216 "../Main.m3"
(*(ADDRESS*)(&WProc_L_193))=(ADDRESS)(((ADDRESS)(Main_m_185_L_186)));
#line 216 "../Main.m3"
 /* set_source_line */
#line 216 "../Main.m3"
#line 217 "../Main.m3"
 /* load */
#line 217 "../Main.m3"
 /* load_indirect */
#line 217 "../Main.m3"
 /* load_nil */
#line 217 "../Main.m3"
 /* if_compare */
#line 217 "../Main.m3"
if(m3_eq(ADDRESS,
 ((ADDRESS)(*((ADDRESS*)(WProc_L_193)))),
 ((ADDRESS)(0))))goto L6A;
#line 217 "../Main.m3"
 /* set_source_line */
#line 217 "../Main.m3"
#line 219 "../Main.m3"
 /* load */
#line 219 "../Main.m3"
 /* load_indirect */
#line 219 "../Main.m3"
 /* store */
#line 219 "../Main.m3"
(*(ADDRESS*)(&LProc_L_80))=(ADDRESS)(((ADDRESS)(*((ADDRESS*)(WProc_L_193)))));
#line 219 "../Main.m3"
 /* set_source_line */
#line 219 "../Main.m3"
#line 220 "../Main.m3"
 /* load_nil */
#line 220 "../Main.m3"
 /* load */
#line 220 "../Main.m3"
 /* swap */
#line 220 "../Main.m3"
 /* store_indirect */
#line 220 "../Main.m3"
(*(ADDRESS*)(WProc_L_193))=(ADDRESS)(((ADDRESS)(0)));
#line 220 "../Main.m3"
 /* set_label */
#line 220 "../Main.m3"
L6A:;
#line 220 "../Main.m3"
 /* end_block */
#line 220 "../Main.m3"
 /* jump */
#line 220 "../Main.m3"
goto L67;
#line 220 "../Main.m3"
 /* end_try */
#line 220 "../Main.m3"
} catch (...) { throw; }
#line 220 "../Main.m3"
 /* set_label */
#line 220 "../Main.m3"
 /* landing_pad */
#line 220 "../Main.m3"
 /* store */
#line 220 "../Main.m3"
(*(ADDRESS*)(&Main_m_183_L_184))=(ADDRESS)(((ADDRESS)(_m3_caught)));
#line 220 "../Main.m3"
 /* set_label */
#line 220 "../Main.m3"
L67:;
#line 220 "../Main.m3"
 /* start_call_indirect */
#line 220 "../Main.m3"
 /* load */
#line 220 "../Main.m3"
 /* pop_param */
#line 220 "../Main.m3"
 /* load */
#line 220 "../Main.m3"
 /* load_indirect */
#line 220 "../Main.m3"
 /* load_indirect */
#line 220 "../Main.m3"
 /* check_nil */
#line 220 "../Main.m3"
 /* store */
#line 220 "../Main.m3"
(*(ADDRESS*)(&Main_m_194_L_195))=(ADDRESS)(((ADDRESS)(*((ADDRESS*)(INT64_(8)+((ADDRESS)(*((ADDRESS*)(Main_m_187_L_188)))))))));
#line 220 "../Main.m3"
 /* load */
#line 220 "../Main.m3"
/*check_nil*/if(!Main_m_194_L_195)Main_m_M_Main_L_49_CRASH(7044);
#line 220 "../Main.m3"
 /* call_indirect */
#line 220 "../Main.m3"
((void (__cdecl*)(void*))Main_m_194_L_195)(
 ((ADDRESS)(Main_m_187_L_188)));
#line 220 "../Main.m3"
 /* load_nil */
#line 220 "../Main.m3"
 /* load */
#line 220 "../Main.m3"
 /* if_compare */
#line 220 "../Main.m3"
if(m3_eq(ADDRESS,
 ((ADDRESS)(0)),
 ((ADDRESS)(Main_m_183_L_184))))goto L68;
#line 220 "../Main.m3"
 /* start_call_direct */
#line 220 "../Main.m3"
 /* load */
#line 220 "../Main.m3"
 /* pop_param */
#line 220 "../Main.m3"
 /* call_direct */
#line 220 "../Main.m3"
RTHooks__ResumeRaise(
  ( ADDRESS )(((ADDRESS)(Main_m_183_L_184)) ));
#line 220 "../Main.m3"
 /* set_source_line */
#line 220 "../Main.m3"
#line 223 "../Main.m3"
 /* set_label */
#line 223 "../Main.m3"
L68:;
#line 223 "../Main.m3"
 /* set_source_line */
#line 223 "../Main.m3"
#line 224 "../Main.m3"
 /* load_nil */
#line 224 "../Main.m3"
 /* load */
#line 224 "../Main.m3"
 /* if_compare */
#line 224 "../Main.m3"
if(m3_eq(ADDRESS,
 ((ADDRESS)(0)),
 ((ADDRESS)(LProc_L_80))))goto L6C;
#line 224 "../Main.m3"
 /* set_source_line */
#line 224 "../Main.m3"
#line 225 "../Main.m3"
 /* start_call_indirect */
#line 225 "../Main.m3"
 /* load */
#line 225 "../Main.m3"
 /* load_indirect */
#line 225 "../Main.m3"
 /* pop_param */
#line 225 "../Main.m3"
 /* load */
#line 225 "../Main.m3"
 /* check_nil */
#line 225 "../Main.m3"
 /* store */
#line 225 "../Main.m3"
(*(ADDRESS*)(&Main_m_196_L_197))=(ADDRESS)(((ADDRESS)(LProc_L_80)));
#line 225 "../Main.m3"
 /* load */
#line 225 "../Main.m3"
/*check_nil*/if(!Main_m_196_L_197)Main_m_M_Main_L_49_CRASH(7204);
#line 225 "../Main.m3"
 /* call_indirect */
#line 225 "../Main.m3"
((void (__cdecl*)(unsigned char))Main_m_196_L_197)(
 ((UINT8)(((INT64)(*((UINT8*)(INT64_(8)+((ADDRESS)(Self_L_82)))))))));
#line 225 "../Main.m3"
 /* set_source_line */
#line 225 "../Main.m3"
#line 227 "../Main.m3"
 /* load_nil */
#line 227 "../Main.m3"
 /* store */
#line 227 "../Main.m3"
(*(ADDRESS*)(&LProc_L_80))=(ADDRESS)(((ADDRESS)(0)));
#line 227 "../Main.m3"
 /* set_label */
#line 227 "../Main.m3"
L6C:;
#line 227 "../Main.m3"
 /* jump */
#line 227 "../Main.m3"
goto L62;
#line 227 "../Main.m3"
 /* set_label */
#line 227 "../Main.m3"
 /* end_procedure */
#line 227 "../Main.m3"
return 0;
#line 227 "../Main.m3"
} /* DoAcq */
#line 227 "../Main.m3"
 /* set_source_line */
#line 227 "../Main.m3"
#line 233 "../Main.m3"
 /* begin_procedure */
#line 233 "../Main.m3"
struct Main__DoAcq_Frame_t {
#line 233 "../Main.m3"
ADDRESS _unused;
#line 233 "../Main.m3"
};
#line 233 "../Main.m3"
void /*TypeText3*/ 
__cdecl
Main__DoAcq(
   /* Param_Type1 */ Main__ThreadNo ThN_L_83)
{
#line 233 "../Main.m3"
 /* Var_Type2 */ ADDRESS Main_m_198_L_199={0};//always-init
#line 233 "../Main.m3"
 /* Var_Type2 */ ADDRESS Main_m_200_L_201={0};//always-init
#line 233 "../Main.m3"
 /* Var_Type1 */ MUTEX Main_m_202_L_203={0};//always-init
#line 233 "../Main.m3"
 /* Var_Type2 */ ADDRESS Main_m_204_L_205={0};//always-init
#line 233 "../Main.m3"
 /* Var_Type2 */ INT64 Main_m_206_L_207={0};//always-init
#line 233 "../Main.m3"
 /* Var_Type2 */ ADDRESS Main_m_208_L_209={0};//always-init
#line 233 "../Main.m3"
 /* Var_Type2 */ ADDRESS Main_m_210_L_211={0};//always-init
#line 233 "../Main.m3"
 /* Var_Type2 */ ADDRESS Main_m_212_L_213={0};//always-init
#line 233 "../Main.m3"
 /* Var_Type1 */ MUTEX Main_m_214_L_215={0};//always-init
#line 233 "../Main.m3"
 /* Var_Type2 */ ADDRESS Main_m_216_L_217={0};//always-init
#line 233 "../Main.m3"
 /* Var_Type1 */ T7609BE10* WThN_L_218={0};//always-init
#line 233 "../Main.m3"
 /* Var_Type2 */ ADDRESS Main_m_219_L_220={0};//always-init
#line 233 "../Main.m3"
Main__DoAcq_Frame_t _frame;
#line 233 "../Main.m3"
_frame._unused=(ADDRESS)&_frame;
#line 233 "../Main.m3"
 /* set_source_line */
#line 233 "../Main.m3"
#line 234 "../Main.m3"
 /* set_source_line */
#line 234 "../Main.m3"
#line 235 "../Main.m3"
 /* load_nil */
#line 235 "../Main.m3"
 /* store */
#line 235 "../Main.m3"
(*(ADDRESS*)(&Main_m_198_L_199))=(ADDRESS)(((ADDRESS)(0)));
#line 235 "../Main.m3"
 /* load */
#line 235 "../Main.m3"
 /* store */
#line 235 "../Main.m3"
(*(ADDRESS*)(&Main_m_200_L_201))=(ADDRESS)(((ADDRESS)(*((ADDRESS*)(INT64_(104)+((ADDRESS)(&Main_m_M_Main_L_49)))))));
#line 235 "../Main.m3"
 /* load_nil */
#line 235 "../Main.m3"
 /* load */
#line 235 "../Main.m3"
 /* if_compare */
#line 235 "../Main.m3"
if(m3_eq(ADDRESS,
 ((ADDRESS)(0)),
 ((ADDRESS)(Main_m_200_L_201))))goto L6D;
#line 235 "../Main.m3"
 /* load */
#line 235 "../Main.m3"
 /* loophole */
#line 235 "../Main.m3"
 /* load_integer */
#line 235 "../Main.m3"
 /* and */
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
  ((INT64)(((INT64)((INT64)Main_m_200_L_201))&  INT64_(1))),
   INT64_(0)))goto L6D;
#line 235 "../Main.m3"
 /* load */
#line 235 "../Main.m3"
 /* load_indirect */
#line 235 "../Main.m3"
 /* extract_mn */
#line 235 "../Main.m3"
 /* load_host_integer */
#line 235 "../Main.m3"
 /* load_integer */
#line 235 "../Main.m3"
 /* load_host_integer */
#line 235 "../Main.m3"
 /* load_integer */
#line 235 "../Main.m3"
 /* extract */
#line 235 "../Main.m3"
 /* if_true_or_false */
#line 235 "../Main.m3"
 /* load_host_integer */
#line 235 "../Main.m3"
 /* load_integer */
#line 235 "../Main.m3"
 /* if_compare */
#line 235 "../Main.m3"
if(m3_eq(INT64,
 ((INT64)(m3_extract(
 UINT64,
  *((INT64*)(INT64_(-8)+((ADDRESS)(Main_m_200_L_201)))),
   UINT64_(22),
   UINT64_(1)))),
   INT64_(0)))goto L6D;
#line 235 "../Main.m3"
 /* start_call_direct */
#line 235 "../Main.m3"
 /* load */
#line 235 "../Main.m3"
 /* pop_param */
#line 235 "../Main.m3"
 /* call_direct */
#line 235 "../Main.m3"
RTHooks__CheckLoadTracedRef(
  ( REFANY )(((ADDRESS)(Main_m_200_L_201)) ));
#line 235 "../Main.m3"
 /* set_label */
#line 235 "../Main.m3"
L6D:;
#line 235 "../Main.m3"
 /* load */
#line 235 "../Main.m3"
 /* store */
#line 235 "../Main.m3"
(*(ADDRESS*)(&Main_m_202_L_203))=(ADDRESS)(((ADDRESS)(Main_m_200_L_201)));
#line 235 "../Main.m3"
 /* start_call_indirect */
#line 235 "../Main.m3"
 /* load */
#line 235 "../Main.m3"
 /* pop_param */
#line 235 "../Main.m3"
 /* load */
#line 235 "../Main.m3"
 /* load_indirect */
#line 235 "../Main.m3"
 /* load_indirect */
#line 235 "../Main.m3"
 /* check_nil */
#line 235 "../Main.m3"
 /* store */
#line 235 "../Main.m3"
(*(ADDRESS*)(&Main_m_204_L_205))=(ADDRESS)(((ADDRESS)(*((ADDRESS*)(*((ADDRESS*)(Main_m_202_L_203)))))));
#line 235 "../Main.m3"
 /* load */
#line 235 "../Main.m3"
/*check_nil*/if(!Main_m_204_L_205)Main_m_M_Main_L_49_CRASH(7524);
#line 235 "../Main.m3"
 /* call_indirect */
#line 235 "../Main.m3"
((void (__cdecl*)(void*))Main_m_204_L_205)(
 ((ADDRESS)(Main_m_202_L_203)));
#line 235 "../Main.m3"
 /* set_label */
#line 235 "../Main.m3"
 /* start_try */
#line 235 "../Main.m3"
try {
#line 235 "../Main.m3"
 /* load */
#line 235 "../Main.m3"
 /* store */
#line 235 "../Main.m3"
(*(INT64*)(&Main_m_206_L_207))=(INT64)( ((INT64)(ThN_L_83)));
#line 235 "../Main.m3"
 /* load_address */
#line 235 "../Main.m3"
 /* load */
#line 235 "../Main.m3"
 /* index_address */
#line 235 "../Main.m3"
 /* store */
#line 235 "../Main.m3"
(*(ADDRESS*)(&Main_m_200_L_201))=(ADDRESS)(((ADDRESS)((((ADDRESS)(INT64_(112)+((ADDRESS)(&Main_m_M_Main_L_49))))+( Main_m_206_L_207)))));
#line 235 "../Main.m3"
 /* load */
#line 235 "../Main.m3"
 /* load_integer */
#line 235 "../Main.m3"
 /* store_indirect */
#line 235 "../Main.m3"
(*(UINT8*)(Main_m_200_L_201))=(INT64)(  INT64_(2));
#line 235 "../Main.m3"
 /* jump */
#line 235 "../Main.m3"
goto L70;
#line 235 "../Main.m3"
 /* end_try */
#line 235 "../Main.m3"
} catch (...) { throw; }
#line 235 "../Main.m3"
 /* set_label */
#line 235 "../Main.m3"
 /* landing_pad */
#line 235 "../Main.m3"
 /* store */
#line 235 "../Main.m3"
(*(ADDRESS*)(&Main_m_198_L_199))=(ADDRESS)(((ADDRESS)(_m3_caught)));
#line 235 "../Main.m3"
 /* set_label */
#line 235 "../Main.m3"
L70:;
#line 235 "../Main.m3"
 /* start_call_indirect */
#line 235 "../Main.m3"
 /* load */
#line 235 "../Main.m3"
 /* pop_param */
#line 235 "../Main.m3"
 /* load */
#line 235 "../Main.m3"
 /* load_indirect */
#line 235 "../Main.m3"
 /* load_indirect */
#line 235 "../Main.m3"
 /* check_nil */
#line 235 "../Main.m3"
 /* store */
#line 235 "../Main.m3"
(*(ADDRESS*)(&Main_m_208_L_209))=(ADDRESS)(((ADDRESS)(*((ADDRESS*)(INT64_(8)+((ADDRESS)(*((ADDRESS*)(Main_m_202_L_203)))))))));
#line 235 "../Main.m3"
 /* load */
#line 235 "../Main.m3"
/*check_nil*/if(!Main_m_208_L_209)Main_m_M_Main_L_49_CRASH(7524);
#line 235 "../Main.m3"
 /* call_indirect */
#line 235 "../Main.m3"
((void (__cdecl*)(void*))Main_m_208_L_209)(
 ((ADDRESS)(Main_m_202_L_203)));
#line 235 "../Main.m3"
 /* load_nil */
#line 235 "../Main.m3"
 /* load */
#line 235 "../Main.m3"
 /* if_compare */
#line 235 "../Main.m3"
if(m3_eq(ADDRESS,
 ((ADDRESS)(0)),
 ((ADDRESS)(Main_m_198_L_199))))goto L71;
#line 235 "../Main.m3"
 /* start_call_direct */
#line 235 "../Main.m3"
 /* load */
#line 235 "../Main.m3"
 /* pop_param */
#line 235 "../Main.m3"
 /* call_direct */
#line 235 "../Main.m3"
RTHooks__ResumeRaise(
  ( ADDRESS )(((ADDRESS)(Main_m_198_L_199)) ));
#line 235 "../Main.m3"
 /* set_label */
#line 235 "../Main.m3"
L71:;
#line 235 "../Main.m3"
 /* set_source_line */
#line 235 "../Main.m3"
#line 236 "../Main.m3"
 /* start_call_direct */
#line 236 "../Main.m3"
 /* load */
#line 236 "../Main.m3"
 /* pop_param */
#line 236 "../Main.m3"
 /* call_direct */
#line 236 "../Main.m3"
 /* store */
#line 236 "../Main.m3"
(*(ADDRESS*)(&Main_m_200_L_201))=(ADDRESS)(((ADDRESS)(Main__ThImage(
  ( Main__ThreadNo )(((UINT8)(((INT64)(ThN_L_83)))) )))));
#line 236 "../Main.m3"
 /* start_call_direct */
#line 236 "../Main.m3"
 /* load */
#line 236 "../Main.m3"
 /* pop_param */
#line 236 "../Main.m3"
 /* load_address */
#line 236 "../Main.m3"
 /* pop_param */
#line 236 "../Main.m3"
 /* call_direct */
#line 236 "../Main.m3"
 /* store */
#line 236 "../Main.m3"
(*(ADDRESS*)(&Main_m_210_L_211))=(ADDRESS)(((ADDRESS)(RTHooks__Concat(
  ( TEXT )(((ADDRESS)(Main_m_200_L_201)) ),
  ( TEXT )(((ADDRESS)(INT64_(640)+((ADDRESS)(&Main_m_47_L_48)))) )))));
#line 236 "../Main.m3"
 /* start_call_direct */
#line 236 "../Main.m3"
 /* load */
#line 236 "../Main.m3"
 /* pop_param */
#line 236 "../Main.m3"
 /* call_direct */
#line 236 "../Main.m3"
Main__W(
  ( TEXT )(((ADDRESS)(Main_m_210_L_211)) ));
#line 236 "../Main.m3"
 /* set_source_line */
#line 236 "../Main.m3"
#line 237 "../Main.m3"
 /* load */
#line 237 "../Main.m3"
 /* store */
#line 237 "../Main.m3"
(*(ADDRESS*)(&Main_m_210_L_211))=(ADDRESS)(((ADDRESS)(*((ADDRESS*)(INT64_(176)+((ADDRESS)(&Main_m_M_Main_L_49)))))));
#line 237 "../Main.m3"
 /* load_nil */
#line 237 "../Main.m3"
 /* load */
#line 237 "../Main.m3"
 /* if_compare */
#line 237 "../Main.m3"
if(m3_eq(ADDRESS,
 ((ADDRESS)(0)),
 ((ADDRESS)(Main_m_210_L_211))))goto L72;
#line 237 "../Main.m3"
 /* load */
#line 237 "../Main.m3"
 /* loophole */
#line 237 "../Main.m3"
 /* load_integer */
#line 237 "../Main.m3"
 /* and */
#line 237 "../Main.m3"
 /* if_true_or_false */
#line 237 "../Main.m3"
 /* load_host_integer */
#line 237 "../Main.m3"
 /* load_integer */
#line 237 "../Main.m3"
 /* if_compare */
#line 237 "../Main.m3"
if(m3_ne(INT64,
  ((INT64)(((INT64)((INT64)Main_m_210_L_211))&  INT64_(1))),
   INT64_(0)))goto L72;
#line 237 "../Main.m3"
 /* load */
#line 237 "../Main.m3"
 /* load_indirect */
#line 237 "../Main.m3"
 /* extract_mn */
#line 237 "../Main.m3"
 /* load_host_integer */
#line 237 "../Main.m3"
 /* load_integer */
#line 237 "../Main.m3"
 /* load_host_integer */
#line 237 "../Main.m3"
 /* load_integer */
#line 237 "../Main.m3"
 /* extract */
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
 ((INT64)(m3_extract(
 UINT64,
  *((INT64*)(INT64_(-8)+((ADDRESS)(Main_m_210_L_211)))),
   UINT64_(22),
   UINT64_(1)))),
   INT64_(0)))goto L72;
#line 237 "../Main.m3"
 /* start_call_direct */
#line 237 "../Main.m3"
 /* load */
#line 237 "../Main.m3"
 /* pop_param */
#line 237 "../Main.m3"
 /* call_direct */
#line 237 "../Main.m3"
RTHooks__CheckLoadTracedRef(
  ( REFANY )(((ADDRESS)(Main_m_210_L_211)) ));
#line 237 "../Main.m3"
 /* set_label */
#line 237 "../Main.m3"
L72:;
#line 237 "../Main.m3"
 /* start_call_direct */
#line 237 "../Main.m3"
 /* load */
#line 237 "../Main.m3"
 /* pop_param */
#line 237 "../Main.m3"
 /* call_direct */
#line 237 "../Main.m3"
Thread__Acquire(
  ( Thread__Mutex )(((ADDRESS)(Main_m_210_L_211)) ));
#line 237 "../Main.m3"
 /* set_source_line */
#line 237 "../Main.m3"
#line 238 "../Main.m3"
 /* start_call_direct */
#line 238 "../Main.m3"
 /* load */
#line 238 "../Main.m3"
 /* pop_param */
#line 238 "../Main.m3"
 /* call_direct */
#line 238 "../Main.m3"
 /* store */
#line 238 "../Main.m3"
(*(ADDRESS*)(&Main_m_210_L_211))=(ADDRESS)(((ADDRESS)(Main__ThImage(
  ( Main__ThreadNo )(((UINT8)(((INT64)(ThN_L_83)))) )))));
#line 238 "../Main.m3"
 /* start_call_direct */
#line 238 "../Main.m3"
 /* load */
#line 238 "../Main.m3"
 /* pop_param */
#line 238 "../Main.m3"
 /* load_address */
#line 238 "../Main.m3"
 /* pop_param */
#line 238 "../Main.m3"
 /* call_direct */
#line 238 "../Main.m3"
 /* store */
#line 238 "../Main.m3"
(*(ADDRESS*)(&Main_m_200_L_201))=(ADDRESS)(((ADDRESS)(RTHooks__Concat(
  ( TEXT )(((ADDRESS)(Main_m_210_L_211)) ),
  ( TEXT )(((ADDRESS)(INT64_(696)+((ADDRESS)(&Main_m_47_L_48)))) )))));
#line 238 "../Main.m3"
 /* start_call_direct */
#line 238 "../Main.m3"
 /* load */
#line 238 "../Main.m3"
 /* pop_param */
#line 238 "../Main.m3"
 /* call_direct */
#line 238 "../Main.m3"
Main__W(
  ( TEXT )(((ADDRESS)(Main_m_200_L_201)) ));
#line 238 "../Main.m3"
 /* set_source_line */
#line 238 "../Main.m3"
#line 239 "../Main.m3"
 /* load_nil */
#line 239 "../Main.m3"
 /* store */
#line 239 "../Main.m3"
(*(ADDRESS*)(&Main_m_212_L_213))=(ADDRESS)(((ADDRESS)(0)));
#line 239 "../Main.m3"
 /* load */
#line 239 "../Main.m3"
 /* store */
#line 239 "../Main.m3"
(*(ADDRESS*)(&Main_m_200_L_201))=(ADDRESS)(((ADDRESS)(*((ADDRESS*)(INT64_(104)+((ADDRESS)(&Main_m_M_Main_L_49)))))));
#line 239 "../Main.m3"
 /* load_nil */
#line 239 "../Main.m3"
 /* load */
#line 239 "../Main.m3"
 /* if_compare */
#line 239 "../Main.m3"
if(m3_eq(ADDRESS,
 ((ADDRESS)(0)),
 ((ADDRESS)(Main_m_200_L_201))))goto L73;
#line 239 "../Main.m3"
 /* load */
#line 239 "../Main.m3"
 /* loophole */
#line 239 "../Main.m3"
 /* load_integer */
#line 239 "../Main.m3"
 /* and */
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
  ((INT64)(((INT64)((INT64)Main_m_200_L_201))&  INT64_(1))),
   INT64_(0)))goto L73;
#line 239 "../Main.m3"
 /* load */
#line 239 "../Main.m3"
 /* load_indirect */
#line 239 "../Main.m3"
 /* extract_mn */
#line 239 "../Main.m3"
 /* load_host_integer */
#line 239 "../Main.m3"
 /* load_integer */
#line 239 "../Main.m3"
 /* load_host_integer */
#line 239 "../Main.m3"
 /* load_integer */
#line 239 "../Main.m3"
 /* extract */
#line 239 "../Main.m3"
 /* if_true_or_false */
#line 239 "../Main.m3"
 /* load_host_integer */
#line 239 "../Main.m3"
 /* load_integer */
#line 239 "../Main.m3"
 /* if_compare */
#line 239 "../Main.m3"
if(m3_eq(INT64,
 ((INT64)(m3_extract(
 UINT64,
  *((INT64*)(INT64_(-8)+((ADDRESS)(Main_m_200_L_201)))),
   UINT64_(22),
   UINT64_(1)))),
   INT64_(0)))goto L73;
#line 239 "../Main.m3"
 /* start_call_direct */
#line 239 "../Main.m3"
 /* load */
#line 239 "../Main.m3"
 /* pop_param */
#line 239 "../Main.m3"
 /* call_direct */
#line 239 "../Main.m3"
RTHooks__CheckLoadTracedRef(
  ( REFANY )(((ADDRESS)(Main_m_200_L_201)) ));
#line 239 "../Main.m3"
 /* set_label */
#line 239 "../Main.m3"
L73:;
#line 239 "../Main.m3"
 /* load */
#line 239 "../Main.m3"
 /* store */
#line 239 "../Main.m3"
(*(ADDRESS*)(&Main_m_214_L_215))=(ADDRESS)(((ADDRESS)(Main_m_200_L_201)));
#line 239 "../Main.m3"
 /* start_call_indirect */
#line 239 "../Main.m3"
 /* load */
#line 239 "../Main.m3"
 /* pop_param */
#line 239 "../Main.m3"
 /* load */
#line 239 "../Main.m3"
 /* load_indirect */
#line 239 "../Main.m3"
 /* load_indirect */
#line 239 "../Main.m3"
 /* check_nil */
#line 239 "../Main.m3"
 /* store */
#line 239 "../Main.m3"
(*(ADDRESS*)(&Main_m_216_L_217))=(ADDRESS)(((ADDRESS)(*((ADDRESS*)(*((ADDRESS*)(Main_m_214_L_215)))))));
#line 239 "../Main.m3"
 /* load */
#line 239 "../Main.m3"
/*check_nil*/if(!Main_m_216_L_217)Main_m_M_Main_L_49_CRASH(7652);
#line 239 "../Main.m3"
 /* call_indirect */
#line 239 "../Main.m3"
((void (__cdecl*)(void*))Main_m_216_L_217)(
 ((ADDRESS)(Main_m_214_L_215)));
#line 239 "../Main.m3"
 /* set_label */
#line 239 "../Main.m3"
 /* start_try */
#line 239 "../Main.m3"
try {
#line 239 "../Main.m3"
 /* set_source_line */
#line 239 "../Main.m3"
#line 240 "../Main.m3"
 /* load */
#line 240 "../Main.m3"
 /* store */
#line 240 "../Main.m3"
(*(INT64*)(&Main_m_206_L_207))=(INT64)( ((INT64)(ThN_L_83)));
#line 240 "../Main.m3"
 /* load_address */
#line 240 "../Main.m3"
 /* load */
#line 240 "../Main.m3"
 /* index_address */
#line 240 "../Main.m3"
 /* store */
#line 240 "../Main.m3"
(*(ADDRESS*)(&Main_m_200_L_201))=(ADDRESS)(((ADDRESS)((((ADDRESS)(INT64_(112)+((ADDRESS)(&Main_m_M_Main_L_49))))+( Main_m_206_L_207)))));
#line 240 "../Main.m3"
 /* begin_block */
#line 240 "../Main.m3"
 /* load */
#line 240 "../Main.m3"
 /* store */
#line 240 "../Main.m3"
(*(ADDRESS*)(&WThN_L_218))=(ADDRESS)(((ADDRESS)(Main_m_200_L_201)));
#line 240 "../Main.m3"
 /* set_source_line */
#line 240 "../Main.m3"
#line 242 "../Main.m3"
 /* load */
#line 242 "../Main.m3"
 /* load_indirect */
#line 242 "../Main.m3"
 /* load_integer */
#line 242 "../Main.m3"
 /* if_compare */
#line 242 "../Main.m3"
if(m3_eq(UINT64,
 ((UINT64)(((INT64)(*((UINT8*)(WThN_L_218)))))),
 ((UINT64)( INT64_(2)))))goto L78;
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
 /* invoke_direct */
#line 242 "../Main.m3"
 /* call_direct */
#line 242 "../Main.m3"
RTHooks__AssertFailed(
  ( ADDRESS )(((ADDRESS)(&Main_m_M_Main_L_49)) ),
  ( INTEGER )(  INT64_(242) ),
  ( TEXT )(((ADDRESS)(INT64_(744)+((ADDRESS)(&Main_m_47_L_48)))) ));
#line 242 "../Main.m3"
 /* set_label */
#line 242 "../Main.m3"
 /* set_label */
#line 242 "../Main.m3"
L78:;
#line 242 "../Main.m3"
 /* set_source_line */
#line 242 "../Main.m3"
#line 243 "../Main.m3"
 /* load */
#line 243 "../Main.m3"
 /* load_integer */
#line 243 "../Main.m3"
 /* store_indirect */
#line 243 "../Main.m3"
(*(UINT8*)(WThN_L_218))=(INT64)(  INT64_(1));
#line 243 "../Main.m3"
 /* set_source_line */
#line 243 "../Main.m3"
#line 244 "../Main.m3"
 /* load_integer */
#line 244 "../Main.m3"
 /* load */
#line 244 "../Main.m3"
 /* if_compare */
#line 244 "../Main.m3"
if(m3_eq(INT64,
   INT64_(0),
  ((INT64)(*((UINT8*)(INT64_(117)+((ADDRESS)(&Main_m_M_Main_L_49))))))))goto L7A;
#line 244 "../Main.m3"
 /* start_call_direct */
#line 244 "../Main.m3"
 /* load_address */
#line 244 "../Main.m3"
 /* pop_param */
#line 244 "../Main.m3"
 /* load_integer */
#line 244 "../Main.m3"
 /* pop_param */
#line 244 "../Main.m3"
 /* load_address */
#line 244 "../Main.m3"
 /* pop_param */
#line 244 "../Main.m3"
 /* invoke_direct */
#line 244 "../Main.m3"
 /* call_direct */
#line 244 "../Main.m3"
RTHooks__AssertFailed(
  ( ADDRESS )(((ADDRESS)(&Main_m_M_Main_L_49)) ),
  ( INTEGER )(  INT64_(244) ),
  ( TEXT )(((ADDRESS)(INT64_(792)+((ADDRESS)(&Main_m_47_L_48)))) ));
#line 244 "../Main.m3"
 /* set_label */
#line 244 "../Main.m3"
 /* set_label */
#line 244 "../Main.m3"
L7A:;
#line 244 "../Main.m3"
 /* set_source_line */
#line 244 "../Main.m3"
#line 245 "../Main.m3"
 /* load */
#line 245 "../Main.m3"
 /* store */
#line 245 "../Main.m3"
(*(UINT8*)((117)+(char*)(&Main_m_M_Main_L_49)))=(INT64)( ((INT64)(ThN_L_83)));
#line 245 "../Main.m3"
 /* end_block */
#line 245 "../Main.m3"
 /* jump */
#line 245 "../Main.m3"
goto L76;
#line 245 "../Main.m3"
 /* end_try */
#line 245 "../Main.m3"
} catch (_M3Exc& _m3exc) { _m3_caught = _m3exc.act; goto L75; }
#line 245 "../Main.m3"
 /* set_label */
#line 245 "../Main.m3"
L75:;
#line 245 "../Main.m3"
 /* landing_pad */
#line 245 "../Main.m3"
 /* store */
#line 245 "../Main.m3"
(*(ADDRESS*)(&Main_m_212_L_213))=(ADDRESS)(((ADDRESS)(_m3_caught)));
#line 245 "../Main.m3"
 /* set_label */
#line 245 "../Main.m3"
L76:;
#line 245 "../Main.m3"
 /* start_call_indirect */
#line 245 "../Main.m3"
 /* load */
#line 245 "../Main.m3"
 /* pop_param */
#line 245 "../Main.m3"
 /* load */
#line 245 "../Main.m3"
 /* load_indirect */
#line 245 "../Main.m3"
 /* load_indirect */
#line 245 "../Main.m3"
 /* check_nil */
#line 245 "../Main.m3"
 /* store */
#line 245 "../Main.m3"
(*(ADDRESS*)(&Main_m_219_L_220))=(ADDRESS)(((ADDRESS)(*((ADDRESS*)(INT64_(8)+((ADDRESS)(*((ADDRESS*)(Main_m_214_L_215)))))))));
#line 245 "../Main.m3"
 /* load */
#line 245 "../Main.m3"
/*check_nil*/if(!Main_m_219_L_220)Main_m_M_Main_L_49_CRASH(7844);
#line 245 "../Main.m3"
 /* call_indirect */
#line 245 "../Main.m3"
((void (__cdecl*)(void*))Main_m_219_L_220)(
 ((ADDRESS)(Main_m_214_L_215)));
#line 245 "../Main.m3"
 /* load_nil */
#line 245 "../Main.m3"
 /* load */
#line 245 "../Main.m3"
 /* if_compare */
#line 245 "../Main.m3"
if(m3_eq(ADDRESS,
 ((ADDRESS)(0)),
 ((ADDRESS)(Main_m_212_L_213))))goto L77;
#line 245 "../Main.m3"
 /* start_call_direct */
#line 245 "../Main.m3"
 /* load */
#line 245 "../Main.m3"
 /* pop_param */
#line 245 "../Main.m3"
 /* call_direct */
#line 245 "../Main.m3"
RTHooks__ResumeRaise(
  ( ADDRESS )(((ADDRESS)(Main_m_212_L_213)) ));
#line 245 "../Main.m3"
 /* set_source_line */
#line 245 "../Main.m3"
#line 247 "../Main.m3"
 /* set_label */
#line 247 "../Main.m3"
L77:;
#line 247 "../Main.m3"
 /* set_source_line */
#line 247 "../Main.m3"
#line 248 "../Main.m3"
 /* exit_proc */
#line 248 "../Main.m3"
return;
#line 248 "../Main.m3"
 /* end_procedure */
#line 248 "../Main.m3"
} /* DoRel */
#line 248 "../Main.m3"
 /* set_source_line */
#line 248 "../Main.m3"
#line 250 "../Main.m3"
 /* begin_procedure */
#line 250 "../Main.m3"
struct Main__DoRel_Frame_t {
#line 250 "../Main.m3"
ADDRESS _unused;
#line 250 "../Main.m3"
};
#line 250 "../Main.m3"
void /*TypeText3*/ 
__cdecl
Main__DoRel(
   /* Param_Type1 */ Main__ThreadNo ThN_L_84)
{
#line 250 "../Main.m3"
 /* Var_Type2 */ ADDRESS Main_m_221_L_222={0};//always-init
#line 250 "../Main.m3"
 /* Var_Type2 */ ADDRESS Main_m_223_L_224={0};//always-init
#line 250 "../Main.m3"
 /* Var_Type1 */ MUTEX Main_m_225_L_226={0};//always-init
#line 250 "../Main.m3"
 /* Var_Type2 */ ADDRESS Main_m_227_L_228={0};//always-init
#line 250 "../Main.m3"
 /* Var_Type2 */ INT64 Main_m_229_L_230={0};//always-init
#line 250 "../Main.m3"
 /* Var_Type2 */ ADDRESS Main_m_231_L_232={0};//always-init
#line 250 "../Main.m3"
 /* Var_Type2 */ ADDRESS Main_m_233_L_234={0};//always-init
#line 250 "../Main.m3"
 /* Var_Type2 */ ADDRESS Main_m_235_L_236={0};//always-init
#line 250 "../Main.m3"
 /* Var_Type1 */ MUTEX Main_m_237_L_238={0};//always-init
#line 250 "../Main.m3"
 /* Var_Type2 */ ADDRESS Main_m_239_L_240={0};//always-init
#line 250 "../Main.m3"
 /* Var_Type1 */ T7609BE10* WThN_L_241={0};//always-init
#line 250 "../Main.m3"
 /* Var_Type2 */ ADDRESS Main_m_242_L_243={0};//always-init
#line 250 "../Main.m3"
Main__DoRel_Frame_t _frame;
#line 250 "../Main.m3"
_frame._unused=(ADDRESS)&_frame;
#line 250 "../Main.m3"
 /* set_source_line */
#line 250 "../Main.m3"
#line 251 "../Main.m3"
 /* set_source_line */
#line 251 "../Main.m3"
#line 252 "../Main.m3"
 /* load_nil */
#line 252 "../Main.m3"
 /* store */
#line 252 "../Main.m3"
(*(ADDRESS*)(&Main_m_221_L_222))=(ADDRESS)(((ADDRESS)(0)));
#line 252 "../Main.m3"
 /* load */
#line 252 "../Main.m3"
 /* store */
#line 252 "../Main.m3"
(*(ADDRESS*)(&Main_m_223_L_224))=(ADDRESS)(((ADDRESS)(*((ADDRESS*)(INT64_(104)+((ADDRESS)(&Main_m_M_Main_L_49)))))));
#line 252 "../Main.m3"
 /* load_nil */
#line 252 "../Main.m3"
 /* load */
#line 252 "../Main.m3"
 /* if_compare */
#line 252 "../Main.m3"
if(m3_eq(ADDRESS,
 ((ADDRESS)(0)),
 ((ADDRESS)(Main_m_223_L_224))))goto L7C;
#line 252 "../Main.m3"
 /* load */
#line 252 "../Main.m3"
 /* loophole */
#line 252 "../Main.m3"
 /* load_integer */
#line 252 "../Main.m3"
 /* and */
#line 252 "../Main.m3"
 /* if_true_or_false */
#line 252 "../Main.m3"
 /* load_host_integer */
#line 252 "../Main.m3"
 /* load_integer */
#line 252 "../Main.m3"
 /* if_compare */
#line 252 "../Main.m3"
if(m3_ne(INT64,
  ((INT64)(((INT64)((INT64)Main_m_223_L_224))&  INT64_(1))),
   INT64_(0)))goto L7C;
#line 252 "../Main.m3"
 /* load */
#line 252 "../Main.m3"
 /* load_indirect */
#line 252 "../Main.m3"
 /* extract_mn */
#line 252 "../Main.m3"
 /* load_host_integer */
#line 252 "../Main.m3"
 /* load_integer */
#line 252 "../Main.m3"
 /* load_host_integer */
#line 252 "../Main.m3"
 /* load_integer */
#line 252 "../Main.m3"
 /* extract */
#line 252 "../Main.m3"
 /* if_true_or_false */
#line 252 "../Main.m3"
 /* load_host_integer */
#line 252 "../Main.m3"
 /* load_integer */
#line 252 "../Main.m3"
 /* if_compare */
#line 252 "../Main.m3"
if(m3_eq(INT64,
 ((INT64)(m3_extract(
 UINT64,
  *((INT64*)(INT64_(-8)+((ADDRESS)(Main_m_223_L_224)))),
   UINT64_(22),
   UINT64_(1)))),
   INT64_(0)))goto L7C;
#line 252 "../Main.m3"
 /* start_call_direct */
#line 252 "../Main.m3"
 /* load */
#line 252 "../Main.m3"
 /* pop_param */
#line 252 "../Main.m3"
 /* call_direct */
#line 252 "../Main.m3"
RTHooks__CheckLoadTracedRef(
  ( REFANY )(((ADDRESS)(Main_m_223_L_224)) ));
#line 252 "../Main.m3"
 /* set_label */
#line 252 "../Main.m3"
L7C:;
#line 252 "../Main.m3"
 /* load */
#line 252 "../Main.m3"
 /* store */
#line 252 "../Main.m3"
(*(ADDRESS*)(&Main_m_225_L_226))=(ADDRESS)(((ADDRESS)(Main_m_223_L_224)));
#line 252 "../Main.m3"
 /* start_call_indirect */
#line 252 "../Main.m3"
 /* load */
#line 252 "../Main.m3"
 /* pop_param */
#line 252 "../Main.m3"
 /* load */
#line 252 "../Main.m3"
 /* load_indirect */
#line 252 "../Main.m3"
 /* load_indirect */
#line 252 "../Main.m3"
 /* check_nil */
#line 252 "../Main.m3"
 /* store */
#line 252 "../Main.m3"
(*(ADDRESS*)(&Main_m_227_L_228))=(ADDRESS)(((ADDRESS)(*((ADDRESS*)(*((ADDRESS*)(Main_m_225_L_226)))))));
#line 252 "../Main.m3"
 /* load */
#line 252 "../Main.m3"
/*check_nil*/if(!Main_m_227_L_228)Main_m_M_Main_L_49_CRASH(8068);
#line 252 "../Main.m3"
 /* call_indirect */
#line 252 "../Main.m3"
((void (__cdecl*)(void*))Main_m_227_L_228)(
 ((ADDRESS)(Main_m_225_L_226)));
#line 252 "../Main.m3"
 /* set_label */
#line 252 "../Main.m3"
 /* start_try */
#line 252 "../Main.m3"
try {
#line 252 "../Main.m3"
 /* load */
#line 252 "../Main.m3"
 /* store */
#line 252 "../Main.m3"
(*(INT64*)(&Main_m_229_L_230))=(INT64)( ((INT64)(ThN_L_84)));
#line 252 "../Main.m3"
 /* load_address */
#line 252 "../Main.m3"
 /* load */
#line 252 "../Main.m3"
 /* index_address */
#line 252 "../Main.m3"
 /* store */
#line 252 "../Main.m3"
(*(ADDRESS*)(&Main_m_223_L_224))=(ADDRESS)(((ADDRESS)((((ADDRESS)(INT64_(112)+((ADDRESS)(&Main_m_M_Main_L_49))))+( Main_m_229_L_230)))));
#line 252 "../Main.m3"
 /* load */
#line 252 "../Main.m3"
 /* load_integer */
#line 252 "../Main.m3"
 /* store_indirect */
#line 252 "../Main.m3"
(*(UINT8*)(Main_m_223_L_224))=(INT64)(  INT64_(3));
#line 252 "../Main.m3"
 /* jump */
#line 252 "../Main.m3"
goto L7F;
#line 252 "../Main.m3"
 /* end_try */
#line 252 "../Main.m3"
} catch (...) { throw; }
#line 252 "../Main.m3"
 /* set_label */
#line 252 "../Main.m3"
 /* landing_pad */
#line 252 "../Main.m3"
 /* store */
#line 252 "../Main.m3"
(*(ADDRESS*)(&Main_m_221_L_222))=(ADDRESS)(((ADDRESS)(_m3_caught)));
#line 252 "../Main.m3"
 /* set_label */
#line 252 "../Main.m3"
L7F:;
#line 252 "../Main.m3"
 /* start_call_indirect */
#line 252 "../Main.m3"
 /* load */
#line 252 "../Main.m3"
 /* pop_param */
#line 252 "../Main.m3"
 /* load */
#line 252 "../Main.m3"
 /* load_indirect */
#line 252 "../Main.m3"
 /* load_indirect */
#line 252 "../Main.m3"
 /* check_nil */
#line 252 "../Main.m3"
 /* store */
#line 252 "../Main.m3"
(*(ADDRESS*)(&Main_m_231_L_232))=(ADDRESS)(((ADDRESS)(*((ADDRESS*)(INT64_(8)+((ADDRESS)(*((ADDRESS*)(Main_m_225_L_226)))))))));
#line 252 "../Main.m3"
 /* load */
#line 252 "../Main.m3"
/*check_nil*/if(!Main_m_231_L_232)Main_m_M_Main_L_49_CRASH(8068);
#line 252 "../Main.m3"
 /* call_indirect */
#line 252 "../Main.m3"
((void (__cdecl*)(void*))Main_m_231_L_232)(
 ((ADDRESS)(Main_m_225_L_226)));
#line 252 "../Main.m3"
 /* load_nil */
#line 252 "../Main.m3"
 /* load */
#line 252 "../Main.m3"
 /* if_compare */
#line 252 "../Main.m3"
if(m3_eq(ADDRESS,
 ((ADDRESS)(0)),
 ((ADDRESS)(Main_m_221_L_222))))goto L80;
#line 252 "../Main.m3"
 /* start_call_direct */
#line 252 "../Main.m3"
 /* load */
#line 252 "../Main.m3"
 /* pop_param */
#line 252 "../Main.m3"
 /* call_direct */
#line 252 "../Main.m3"
RTHooks__ResumeRaise(
  ( ADDRESS )(((ADDRESS)(Main_m_221_L_222)) ));
#line 252 "../Main.m3"
 /* set_label */
#line 252 "../Main.m3"
L80:;
#line 252 "../Main.m3"
 /* set_source_line */
#line 252 "../Main.m3"
#line 253 "../Main.m3"
 /* start_call_direct */
#line 253 "../Main.m3"
 /* load */
#line 253 "../Main.m3"
 /* pop_param */
#line 253 "../Main.m3"
 /* call_direct */
#line 253 "../Main.m3"
 /* store */
#line 253 "../Main.m3"
(*(ADDRESS*)(&Main_m_223_L_224))=(ADDRESS)(((ADDRESS)(Main__ThImage(
  ( Main__ThreadNo )(((UINT8)(((INT64)(ThN_L_84)))) )))));
#line 253 "../Main.m3"
 /* start_call_direct */
#line 253 "../Main.m3"
 /* load */
#line 253 "../Main.m3"
 /* pop_param */
#line 253 "../Main.m3"
 /* load_address */
#line 253 "../Main.m3"
 /* pop_param */
#line 253 "../Main.m3"
 /* call_direct */
#line 253 "../Main.m3"
 /* store */
#line 253 "../Main.m3"
(*(ADDRESS*)(&Main_m_233_L_234))=(ADDRESS)(((ADDRESS)(RTHooks__Concat(
  ( TEXT )(((ADDRESS)(Main_m_223_L_224)) ),
  ( TEXT )(((ADDRESS)(INT64_(840)+((ADDRESS)(&Main_m_47_L_48)))) )))));
#line 253 "../Main.m3"
 /* start_call_direct */
#line 253 "../Main.m3"
 /* load */
#line 253 "../Main.m3"
 /* pop_param */
#line 253 "../Main.m3"
 /* call_direct */
#line 253 "../Main.m3"
Main__W(
  ( TEXT )(((ADDRESS)(Main_m_233_L_234)) ));
#line 253 "../Main.m3"
 /* set_source_line */
#line 253 "../Main.m3"
#line 254 "../Main.m3"
 /* load */
#line 254 "../Main.m3"
 /* store */
#line 254 "../Main.m3"
(*(ADDRESS*)(&Main_m_233_L_234))=(ADDRESS)(((ADDRESS)(*((ADDRESS*)(INT64_(176)+((ADDRESS)(&Main_m_M_Main_L_49)))))));
#line 254 "../Main.m3"
 /* load_nil */
#line 254 "../Main.m3"
 /* load */
#line 254 "../Main.m3"
 /* if_compare */
#line 254 "../Main.m3"
if(m3_eq(ADDRESS,
 ((ADDRESS)(0)),
 ((ADDRESS)(Main_m_233_L_234))))goto L81;
#line 254 "../Main.m3"
 /* load */
#line 254 "../Main.m3"
 /* loophole */
#line 254 "../Main.m3"
 /* load_integer */
#line 254 "../Main.m3"
 /* and */
#line 254 "../Main.m3"
 /* if_true_or_false */
#line 254 "../Main.m3"
 /* load_host_integer */
#line 254 "../Main.m3"
 /* load_integer */
#line 254 "../Main.m3"
 /* if_compare */
#line 254 "../Main.m3"
if(m3_ne(INT64,
  ((INT64)(((INT64)((INT64)Main_m_233_L_234))&  INT64_(1))),
   INT64_(0)))goto L81;
#line 254 "../Main.m3"
 /* load */
#line 254 "../Main.m3"
 /* load_indirect */
#line 254 "../Main.m3"
 /* extract_mn */
#line 254 "../Main.m3"
 /* load_host_integer */
#line 254 "../Main.m3"
 /* load_integer */
#line 254 "../Main.m3"
 /* load_host_integer */
#line 254 "../Main.m3"
 /* load_integer */
#line 254 "../Main.m3"
 /* extract */
#line 254 "../Main.m3"
 /* if_true_or_false */
#line 254 "../Main.m3"
 /* load_host_integer */
#line 254 "../Main.m3"
 /* load_integer */
#line 254 "../Main.m3"
 /* if_compare */
#line 254 "../Main.m3"
if(m3_eq(INT64,
 ((INT64)(m3_extract(
 UINT64,
  *((INT64*)(INT64_(-8)+((ADDRESS)(Main_m_233_L_234)))),
   UINT64_(22),
   UINT64_(1)))),
   INT64_(0)))goto L81;
#line 254 "../Main.m3"
 /* start_call_direct */
#line 254 "../Main.m3"
 /* load */
#line 254 "../Main.m3"
 /* pop_param */
#line 254 "../Main.m3"
 /* call_direct */
#line 254 "../Main.m3"
RTHooks__CheckLoadTracedRef(
  ( REFANY )(((ADDRESS)(Main_m_233_L_234)) ));
#line 254 "../Main.m3"
 /* set_label */
#line 254 "../Main.m3"
L81:;
#line 254 "../Main.m3"
 /* start_call_direct */
#line 254 "../Main.m3"
 /* load */
#line 254 "../Main.m3"
 /* pop_param */
#line 254 "../Main.m3"
 /* call_direct */
#line 254 "../Main.m3"
Thread__Release(
  ( Thread__Mutex )(((ADDRESS)(Main_m_233_L_234)) ));
#line 254 "../Main.m3"
 /* set_source_line */
#line 254 "../Main.m3"
#line 255 "../Main.m3"
 /* start_call_direct */
#line 255 "../Main.m3"
 /* load */
#line 255 "../Main.m3"
 /* pop_param */
#line 255 "../Main.m3"
 /* call_direct */
#line 255 "../Main.m3"
 /* store */
#line 255 "../Main.m3"
(*(ADDRESS*)(&Main_m_233_L_234))=(ADDRESS)(((ADDRESS)(Main__ThImage(
  ( Main__ThreadNo )(((UINT8)(((INT64)(ThN_L_84)))) )))));
#line 255 "../Main.m3"
 /* start_call_direct */
#line 255 "../Main.m3"
 /* load */
#line 255 "../Main.m3"
 /* pop_param */
#line 255 "../Main.m3"
 /* load_address */
#line 255 "../Main.m3"
 /* pop_param */
#line 255 "../Main.m3"
 /* call_direct */
#line 255 "../Main.m3"
 /* store */
#line 255 "../Main.m3"
(*(ADDRESS*)(&Main_m_223_L_224))=(ADDRESS)(((ADDRESS)(RTHooks__Concat(
  ( TEXT )(((ADDRESS)(Main_m_233_L_234)) ),
  ( TEXT )(((ADDRESS)(INT64_(896)+((ADDRESS)(&Main_m_47_L_48)))) )))));
#line 255 "../Main.m3"
 /* start_call_direct */
#line 255 "../Main.m3"
 /* load */
#line 255 "../Main.m3"
 /* pop_param */
#line 255 "../Main.m3"
 /* call_direct */
#line 255 "../Main.m3"
Main__W(
  ( TEXT )(((ADDRESS)(Main_m_223_L_224)) ));
#line 255 "../Main.m3"
 /* set_source_line */
#line 255 "../Main.m3"
#line 256 "../Main.m3"
 /* load_nil */
#line 256 "../Main.m3"
 /* store */
#line 256 "../Main.m3"
(*(ADDRESS*)(&Main_m_235_L_236))=(ADDRESS)(((ADDRESS)(0)));
#line 256 "../Main.m3"
 /* load */
#line 256 "../Main.m3"
 /* store */
#line 256 "../Main.m3"
(*(ADDRESS*)(&Main_m_223_L_224))=(ADDRESS)(((ADDRESS)(*((ADDRESS*)(INT64_(104)+((ADDRESS)(&Main_m_M_Main_L_49)))))));
#line 256 "../Main.m3"
 /* load_nil */
#line 256 "../Main.m3"
 /* load */
#line 256 "../Main.m3"
 /* if_compare */
#line 256 "../Main.m3"
if(m3_eq(ADDRESS,
 ((ADDRESS)(0)),
 ((ADDRESS)(Main_m_223_L_224))))goto L82;
#line 256 "../Main.m3"
 /* load */
#line 256 "../Main.m3"
 /* loophole */
#line 256 "../Main.m3"
 /* load_integer */
#line 256 "../Main.m3"
 /* and */
#line 256 "../Main.m3"
 /* if_true_or_false */
#line 256 "../Main.m3"
 /* load_host_integer */
#line 256 "../Main.m3"
 /* load_integer */
#line 256 "../Main.m3"
 /* if_compare */
#line 256 "../Main.m3"
if(m3_ne(INT64,
  ((INT64)(((INT64)((INT64)Main_m_223_L_224))&  INT64_(1))),
   INT64_(0)))goto L82;
#line 256 "../Main.m3"
 /* load */
#line 256 "../Main.m3"
 /* load_indirect */
#line 256 "../Main.m3"
 /* extract_mn */
#line 256 "../Main.m3"
 /* load_host_integer */
#line 256 "../Main.m3"
 /* load_integer */
#line 256 "../Main.m3"
 /* load_host_integer */
#line 256 "../Main.m3"
 /* load_integer */
#line 256 "../Main.m3"
 /* extract */
#line 256 "../Main.m3"
 /* if_true_or_false */
#line 256 "../Main.m3"
 /* load_host_integer */
#line 256 "../Main.m3"
 /* load_integer */
#line 256 "../Main.m3"
 /* if_compare */
#line 256 "../Main.m3"
if(m3_eq(INT64,
 ((INT64)(m3_extract(
 UINT64,
  *((INT64*)(INT64_(-8)+((ADDRESS)(Main_m_223_L_224)))),
   UINT64_(22),
   UINT64_(1)))),
   INT64_(0)))goto L82;
#line 256 "../Main.m3"
 /* start_call_direct */
#line 256 "../Main.m3"
 /* load */
#line 256 "../Main.m3"
 /* pop_param */
#line 256 "../Main.m3"
 /* call_direct */
#line 256 "../Main.m3"
RTHooks__CheckLoadTracedRef(
  ( REFANY )(((ADDRESS)(Main_m_223_L_224)) ));
#line 256 "../Main.m3"
 /* set_label */
#line 256 "../Main.m3"
L82:;
#line 256 "../Main.m3"
 /* load */
#line 256 "../Main.m3"
 /* store */
#line 256 "../Main.m3"
(*(ADDRESS*)(&Main_m_237_L_238))=(ADDRESS)(((ADDRESS)(Main_m_223_L_224)));
#line 256 "../Main.m3"
 /* start_call_indirect */
#line 256 "../Main.m3"
 /* load */
#line 256 "../Main.m3"
 /* pop_param */
#line 256 "../Main.m3"
 /* load */
#line 256 "../Main.m3"
 /* load_indirect */
#line 256 "../Main.m3"
 /* load_indirect */
#line 256 "../Main.m3"
 /* check_nil */
#line 256 "../Main.m3"
 /* store */
#line 256 "../Main.m3"
(*(ADDRESS*)(&Main_m_239_L_240))=(ADDRESS)(((ADDRESS)(*((ADDRESS*)(*((ADDRESS*)(Main_m_237_L_238)))))));
#line 256 "../Main.m3"
 /* load */
#line 256 "../Main.m3"
/*check_nil*/if(!Main_m_239_L_240)Main_m_M_Main_L_49_CRASH(8196);
#line 256 "../Main.m3"
 /* call_indirect */
#line 256 "../Main.m3"
((void (__cdecl*)(void*))Main_m_239_L_240)(
 ((ADDRESS)(Main_m_237_L_238)));
#line 256 "../Main.m3"
 /* set_label */
#line 256 "../Main.m3"
 /* start_try */
#line 256 "../Main.m3"
try {
#line 256 "../Main.m3"
 /* set_source_line */
#line 256 "../Main.m3"
#line 257 "../Main.m3"
 /* load */
#line 257 "../Main.m3"
 /* store */
#line 257 "../Main.m3"
(*(INT64*)(&Main_m_229_L_230))=(INT64)( ((INT64)(ThN_L_84)));
#line 257 "../Main.m3"
 /* load_address */
#line 257 "../Main.m3"
 /* load */
#line 257 "../Main.m3"
 /* index_address */
#line 257 "../Main.m3"
 /* store */
#line 257 "../Main.m3"
(*(ADDRESS*)(&Main_m_223_L_224))=(ADDRESS)(((ADDRESS)((((ADDRESS)(INT64_(112)+((ADDRESS)(&Main_m_M_Main_L_49))))+( Main_m_229_L_230)))));
#line 257 "../Main.m3"
 /* begin_block */
#line 257 "../Main.m3"
 /* load */
#line 257 "../Main.m3"
 /* store */
#line 257 "../Main.m3"
(*(ADDRESS*)(&WThN_L_241))=(ADDRESS)(((ADDRESS)(Main_m_223_L_224)));
#line 257 "../Main.m3"
 /* set_source_line */
#line 257 "../Main.m3"
#line 259 "../Main.m3"
 /* load */
#line 259 "../Main.m3"
 /* load_indirect */
#line 259 "../Main.m3"
 /* load_integer */
#line 259 "../Main.m3"
 /* if_compare */
#line 259 "../Main.m3"
if(m3_eq(UINT64,
 ((UINT64)(((INT64)(*((UINT8*)(WThN_L_241)))))),
 ((UINT64)( INT64_(3)))))goto L87;
#line 259 "../Main.m3"
 /* start_call_direct */
#line 259 "../Main.m3"
 /* load_address */
#line 259 "../Main.m3"
 /* pop_param */
#line 259 "../Main.m3"
 /* load_integer */
#line 259 "../Main.m3"
 /* pop_param */
#line 259 "../Main.m3"
 /* load_address */
#line 259 "../Main.m3"
 /* pop_param */
#line 259 "../Main.m3"
 /* invoke_direct */
#line 259 "../Main.m3"
 /* call_direct */
#line 259 "../Main.m3"
RTHooks__AssertFailed(
  ( ADDRESS )(((ADDRESS)(&Main_m_M_Main_L_49)) ),
  ( INTEGER )(  INT64_(259) ),
  ( TEXT )(((ADDRESS)(INT64_(944)+((ADDRESS)(&Main_m_47_L_48)))) ));
#line 259 "../Main.m3"
 /* set_label */
#line 259 "../Main.m3"
 /* set_label */
#line 259 "../Main.m3"
L87:;
#line 259 "../Main.m3"
 /* set_source_line */
#line 259 "../Main.m3"
#line 260 "../Main.m3"
 /* load */
#line 260 "../Main.m3"
 /* load_integer */
#line 260 "../Main.m3"
 /* store_indirect */
#line 260 "../Main.m3"
(*(UINT8*)(WThN_L_241))=(INT64)(  INT64_(1));
#line 260 "../Main.m3"
 /* set_source_line */
#line 260 "../Main.m3"
#line 261 "../Main.m3"
 /* load */
#line 261 "../Main.m3"
 /* load */
#line 261 "../Main.m3"
 /* if_compare */
#line 261 "../Main.m3"
if(m3_eq(INT64,
  ((INT64)(ThN_L_84)),
  ((INT64)(*((UINT8*)(INT64_(117)+((ADDRESS)(&Main_m_M_Main_L_49))))))))goto L89;
#line 261 "../Main.m3"
 /* start_call_direct */
#line 261 "../Main.m3"
 /* load_address */
#line 261 "../Main.m3"
 /* pop_param */
#line 261 "../Main.m3"
 /* load_integer */
#line 261 "../Main.m3"
 /* pop_param */
#line 261 "../Main.m3"
 /* load_address */
#line 261 "../Main.m3"
 /* pop_param */
#line 261 "../Main.m3"
 /* invoke_direct */
#line 261 "../Main.m3"
 /* call_direct */
#line 261 "../Main.m3"
RTHooks__AssertFailed(
  ( ADDRESS )(((ADDRESS)(&Main_m_M_Main_L_49)) ),
  ( INTEGER )(  INT64_(261) ),
  ( TEXT )(((ADDRESS)(INT64_(528)+((ADDRESS)(&Main_m_47_L_48)))) ));
#line 261 "../Main.m3"
 /* set_label */
#line 261 "../Main.m3"
 /* set_label */
#line 261 "../Main.m3"
L89:;
#line 261 "../Main.m3"
 /* set_source_line */
#line 261 "../Main.m3"
#line 262 "../Main.m3"
 /* load_integer */
#line 262 "../Main.m3"
 /* store */
#line 262 "../Main.m3"
(*(UINT8*)((117)+(char*)(&Main_m_M_Main_L_49)))=(INT64)(  INT64_(0));
#line 262 "../Main.m3"
 /* end_block */
#line 262 "../Main.m3"
 /* jump */
#line 262 "../Main.m3"
goto L85;
#line 262 "../Main.m3"
 /* end_try */
#line 262 "../Main.m3"
} catch (_M3Exc& _m3exc) { _m3_caught = _m3exc.act; goto L84; }
#line 262 "../Main.m3"
 /* set_label */
#line 262 "../Main.m3"
L84:;
#line 262 "../Main.m3"
 /* landing_pad */
#line 262 "../Main.m3"
 /* store */
#line 262 "../Main.m3"
(*(ADDRESS*)(&Main_m_235_L_236))=(ADDRESS)(((ADDRESS)(_m3_caught)));
#line 262 "../Main.m3"
 /* set_label */
#line 262 "../Main.m3"
L85:;
#line 262 "../Main.m3"
 /* start_call_indirect */
#line 262 "../Main.m3"
 /* load */
#line 262 "../Main.m3"
 /* pop_param */
#line 262 "../Main.m3"
 /* load */
#line 262 "../Main.m3"
 /* load_indirect */
#line 262 "../Main.m3"
 /* load_indirect */
#line 262 "../Main.m3"
 /* check_nil */
#line 262 "../Main.m3"
 /* store */
#line 262 "../Main.m3"
(*(ADDRESS*)(&Main_m_242_L_243))=(ADDRESS)(((ADDRESS)(*((ADDRESS*)(INT64_(8)+((ADDRESS)(*((ADDRESS*)(Main_m_237_L_238)))))))));
#line 262 "../Main.m3"
 /* load */
#line 262 "../Main.m3"
/*check_nil*/if(!Main_m_242_L_243)Main_m_M_Main_L_49_CRASH(8388);
#line 262 "../Main.m3"
 /* call_indirect */
#line 262 "../Main.m3"
((void (__cdecl*)(void*))Main_m_242_L_243)(
 ((ADDRESS)(Main_m_237_L_238)));
#line 262 "../Main.m3"
 /* load_nil */
#line 262 "../Main.m3"
 /* load */
#line 262 "../Main.m3"
 /* if_compare */
#line 262 "../Main.m3"
if(m3_eq(ADDRESS,
 ((ADDRESS)(0)),
 ((ADDRESS)(Main_m_235_L_236))))goto L86;
#line 262 "../Main.m3"
 /* start_call_direct */
#line 262 "../Main.m3"
 /* load */
#line 262 "../Main.m3"
 /* pop_param */
#line 262 "../Main.m3"
 /* call_direct */
#line 262 "../Main.m3"
RTHooks__ResumeRaise(
  ( ADDRESS )(((ADDRESS)(Main_m_235_L_236)) ));
#line 262 "../Main.m3"
 /* set_source_line */
#line 262 "../Main.m3"
#line 264 "../Main.m3"
 /* set_label */
#line 264 "../Main.m3"
L86:;
#line 264 "../Main.m3"
 /* set_source_line */
#line 264 "../Main.m3"
#line 265 "../Main.m3"
 /* exit_proc */
#line 265 "../Main.m3"
return;
#line 265 "../Main.m3"
 /* end_procedure */
#line 265 "../Main.m3"
} /* DoWait */
#line 265 "../Main.m3"
 /* set_source_line */
#line 265 "../Main.m3"
#line 267 "../Main.m3"
 /* begin_procedure */
#line 267 "../Main.m3"
struct Main__DoWait_Frame_t {
#line 267 "../Main.m3"
ADDRESS _unused;
#line 267 "../Main.m3"
};
#line 267 "../Main.m3"
void /*TypeText3*/ 
__cdecl
Main__DoWait(
   /* Param_Type1 */ Main__ThreadNo ThN_L_85)
{
#line 267 "../Main.m3"
 /* Var_Type2 */ ADDRESS Main_m_244_L_245={0};//always-init
#line 267 "../Main.m3"
 /* Var_Type2 */ ADDRESS Main_m_246_L_247={0};//always-init
#line 267 "../Main.m3"
 /* Var_Type1 */ MUTEX Main_m_248_L_249={0};//always-init
#line 267 "../Main.m3"
 /* Var_Type2 */ ADDRESS Main_m_250_L_251={0};//always-init
#line 267 "../Main.m3"
 /* Var_Type2 */ INT64 Main_m_252_L_253={0};//always-init
#line 267 "../Main.m3"
 /* Var_Type1 */ T7609BE10* WThN_L_254={0};//always-init
#line 267 "../Main.m3"
 /* Var_Type2 */ ADDRESS Main_m_255_L_256={0};//always-init
#line 267 "../Main.m3"
 /* Var_Type2 */ ADDRESS Main_m_257_L_258={0};//always-init
#line 267 "../Main.m3"
 /* Var_Type2 */ ADDRESS Main_m_259_L_260={0};//always-init
#line 267 "../Main.m3"
 /* Var_Type1 */ MUTEX Main_m_261_L_262={0};//always-init
#line 267 "../Main.m3"
 /* Var_Type2 */ ADDRESS Main_m_263_L_264={0};//always-init
#line 267 "../Main.m3"
 /* Var_Type1 */ T7609BE10* WThN_L_265={0};//always-init
#line 267 "../Main.m3"
 /* Var_Type2 */ ADDRESS Main_m_266_L_267={0};//always-init
#line 267 "../Main.m3"
Main__DoWait_Frame_t _frame;
#line 267 "../Main.m3"
_frame._unused=(ADDRESS)&_frame;
#line 267 "../Main.m3"
 /* set_source_line */
#line 267 "../Main.m3"
#line 268 "../Main.m3"
 /* set_source_line */
#line 268 "../Main.m3"
#line 269 "../Main.m3"
 /* load_nil */
#line 269 "../Main.m3"
 /* store */
#line 269 "../Main.m3"
(*(ADDRESS*)(&Main_m_244_L_245))=(ADDRESS)(((ADDRESS)(0)));
#line 269 "../Main.m3"
 /* load */
#line 269 "../Main.m3"
 /* store */
#line 269 "../Main.m3"
(*(ADDRESS*)(&Main_m_246_L_247))=(ADDRESS)(((ADDRESS)(*((ADDRESS*)(INT64_(104)+((ADDRESS)(&Main_m_M_Main_L_49)))))));
#line 269 "../Main.m3"
 /* load_nil */
#line 269 "../Main.m3"
 /* load */
#line 269 "../Main.m3"
 /* if_compare */
#line 269 "../Main.m3"
if(m3_eq(ADDRESS,
 ((ADDRESS)(0)),
 ((ADDRESS)(Main_m_246_L_247))))goto L8B;
#line 269 "../Main.m3"
 /* load */
#line 269 "../Main.m3"
 /* loophole */
#line 269 "../Main.m3"
 /* load_integer */
#line 269 "../Main.m3"
 /* and */
#line 269 "../Main.m3"
 /* if_true_or_false */
#line 269 "../Main.m3"
 /* load_host_integer */
#line 269 "../Main.m3"
 /* load_integer */
#line 269 "../Main.m3"
 /* if_compare */
#line 269 "../Main.m3"
if(m3_ne(INT64,
  ((INT64)(((INT64)((INT64)Main_m_246_L_247))&  INT64_(1))),
   INT64_(0)))goto L8B;
#line 269 "../Main.m3"
 /* load */
#line 269 "../Main.m3"
 /* load_indirect */
#line 269 "../Main.m3"
 /* extract_mn */
#line 269 "../Main.m3"
 /* load_host_integer */
#line 269 "../Main.m3"
 /* load_integer */
#line 269 "../Main.m3"
 /* load_host_integer */
#line 269 "../Main.m3"
 /* load_integer */
#line 269 "../Main.m3"
 /* extract */
#line 269 "../Main.m3"
 /* if_true_or_false */
#line 269 "../Main.m3"
 /* load_host_integer */
#line 269 "../Main.m3"
 /* load_integer */
#line 269 "../Main.m3"
 /* if_compare */
#line 269 "../Main.m3"
if(m3_eq(INT64,
 ((INT64)(m3_extract(
 UINT64,
  *((INT64*)(INT64_(-8)+((ADDRESS)(Main_m_246_L_247)))),
   UINT64_(22),
   UINT64_(1)))),
   INT64_(0)))goto L8B;
#line 269 "../Main.m3"
 /* start_call_direct */
#line 269 "../Main.m3"
 /* load */
#line 269 "../Main.m3"
 /* pop_param */
#line 269 "../Main.m3"
 /* call_direct */
#line 269 "../Main.m3"
RTHooks__CheckLoadTracedRef(
  ( REFANY )(((ADDRESS)(Main_m_246_L_247)) ));
#line 269 "../Main.m3"
 /* set_label */
#line 269 "../Main.m3"
L8B:;
#line 269 "../Main.m3"
 /* load */
#line 269 "../Main.m3"
 /* store */
#line 269 "../Main.m3"
(*(ADDRESS*)(&Main_m_248_L_249))=(ADDRESS)(((ADDRESS)(Main_m_246_L_247)));
#line 269 "../Main.m3"
 /* start_call_indirect */
#line 269 "../Main.m3"
 /* load */
#line 269 "../Main.m3"
 /* pop_param */
#line 269 "../Main.m3"
 /* load */
#line 269 "../Main.m3"
 /* load_indirect */
#line 269 "../Main.m3"
 /* load_indirect */
#line 269 "../Main.m3"
 /* check_nil */
#line 269 "../Main.m3"
 /* store */
#line 269 "../Main.m3"
(*(ADDRESS*)(&Main_m_250_L_251))=(ADDRESS)(((ADDRESS)(*((ADDRESS*)(*((ADDRESS*)(Main_m_248_L_249)))))));
#line 269 "../Main.m3"
 /* load */
#line 269 "../Main.m3"
/*check_nil*/if(!Main_m_250_L_251)Main_m_M_Main_L_49_CRASH(8612);
#line 269 "../Main.m3"
 /* call_indirect */
#line 269 "../Main.m3"
((void (__cdecl*)(void*))Main_m_250_L_251)(
 ((ADDRESS)(Main_m_248_L_249)));
#line 269 "../Main.m3"
 /* set_label */
#line 269 "../Main.m3"
 /* start_try */
#line 269 "../Main.m3"
try {
#line 269 "../Main.m3"
 /* set_source_line */
#line 269 "../Main.m3"
#line 270 "../Main.m3"
 /* load */
#line 270 "../Main.m3"
 /* store */
#line 270 "../Main.m3"
(*(INT64*)(&Main_m_252_L_253))=(INT64)( ((INT64)(ThN_L_85)));
#line 270 "../Main.m3"
 /* load_address */
#line 270 "../Main.m3"
 /* load */
#line 270 "../Main.m3"
 /* index_address */
#line 270 "../Main.m3"
 /* store */
#line 270 "../Main.m3"
(*(ADDRESS*)(&Main_m_246_L_247))=(ADDRESS)(((ADDRESS)((((ADDRESS)(INT64_(112)+((ADDRESS)(&Main_m_M_Main_L_49))))+( Main_m_252_L_253)))));
#line 270 "../Main.m3"
 /* begin_block */
#line 270 "../Main.m3"
 /* load */
#line 270 "../Main.m3"
 /* store */
#line 270 "../Main.m3"
(*(ADDRESS*)(&WThN_L_254))=(ADDRESS)(((ADDRESS)(Main_m_246_L_247)));
#line 270 "../Main.m3"
 /* set_source_line */
#line 270 "../Main.m3"
#line 272 "../Main.m3"
 /* load */
#line 272 "../Main.m3"
 /* load */
#line 272 "../Main.m3"
 /* if_compare */
#line 272 "../Main.m3"
if(m3_eq(INT64,
  ((INT64)(ThN_L_85)),
  ((INT64)(*((UINT8*)(INT64_(117)+((ADDRESS)(&Main_m_M_Main_L_49))))))))goto L90;
#line 272 "../Main.m3"
 /* start_call_direct */
#line 272 "../Main.m3"
 /* load_address */
#line 272 "../Main.m3"
 /* pop_param */
#line 272 "../Main.m3"
 /* load_integer */
#line 272 "../Main.m3"
 /* pop_param */
#line 272 "../Main.m3"
 /* load_address */
#line 272 "../Main.m3"
 /* pop_param */
#line 272 "../Main.m3"
 /* invoke_direct */
#line 272 "../Main.m3"
 /* call_direct */
#line 272 "../Main.m3"
RTHooks__AssertFailed(
  ( ADDRESS )(((ADDRESS)(&Main_m_M_Main_L_49)) ),
  ( INTEGER )(  INT64_(272) ),
  ( TEXT )(((ADDRESS)(INT64_(528)+((ADDRESS)(&Main_m_47_L_48)))) ));
#line 272 "../Main.m3"
 /* set_label */
#line 272 "../Main.m3"
 /* set_label */
#line 272 "../Main.m3"
L90:;
#line 272 "../Main.m3"
 /* set_source_line */
#line 272 "../Main.m3"
#line 273 "../Main.m3"
 /* load */
#line 273 "../Main.m3"
 /* load_indirect */
#line 273 "../Main.m3"
 /* load_integer */
#line 273 "../Main.m3"
 /* if_compare */
#line 273 "../Main.m3"
if(m3_eq(UINT64,
 ((UINT64)(((INT64)(*((UINT8*)(WThN_L_254)))))),
 ((UINT64)( INT64_(1)))))goto L92;
#line 273 "../Main.m3"
 /* start_call_direct */
#line 273 "../Main.m3"
 /* load_address */
#line 273 "../Main.m3"
 /* pop_param */
#line 273 "../Main.m3"
 /* load_integer */
#line 273 "../Main.m3"
 /* pop_param */
#line 273 "../Main.m3"
 /* load_address */
#line 273 "../Main.m3"
 /* pop_param */
#line 273 "../Main.m3"
 /* invoke_direct */
#line 273 "../Main.m3"
 /* call_direct */
#line 273 "../Main.m3"
RTHooks__AssertFailed(
  ( ADDRESS )(((ADDRESS)(&Main_m_M_Main_L_49)) ),
  ( INTEGER )(  INT64_(273) ),
  ( TEXT )(((ADDRESS)(INT64_(992)+((ADDRESS)(&Main_m_47_L_48)))) ));
#line 273 "../Main.m3"
 /* set_label */
#line 273 "../Main.m3"
 /* set_label */
#line 273 "../Main.m3"
L92:;
#line 273 "../Main.m3"
 /* set_source_line */
#line 273 "../Main.m3"
#line 274 "../Main.m3"
 /* load */
#line 274 "../Main.m3"
 /* load_integer */
#line 274 "../Main.m3"
 /* store_indirect */
#line 274 "../Main.m3"
(*(UINT8*)(WThN_L_254))=(INT64)(  INT64_(4));
#line 274 "../Main.m3"
 /* end_block */
#line 274 "../Main.m3"
 /* jump */
#line 274 "../Main.m3"
goto L8E;
#line 274 "../Main.m3"
 /* end_try */
#line 274 "../Main.m3"
} catch (_M3Exc& _m3exc) { _m3_caught = _m3exc.act; goto L8D; }
#line 274 "../Main.m3"
 /* set_label */
#line 274 "../Main.m3"
L8D:;
#line 274 "../Main.m3"
 /* landing_pad */
#line 274 "../Main.m3"
 /* store */
#line 274 "../Main.m3"
(*(ADDRESS*)(&Main_m_244_L_245))=(ADDRESS)(((ADDRESS)(_m3_caught)));
#line 274 "../Main.m3"
 /* set_label */
#line 274 "../Main.m3"
L8E:;
#line 274 "../Main.m3"
 /* start_call_indirect */
#line 274 "../Main.m3"
 /* load */
#line 274 "../Main.m3"
 /* pop_param */
#line 274 "../Main.m3"
 /* load */
#line 274 "../Main.m3"
 /* load_indirect */
#line 274 "../Main.m3"
 /* load_indirect */
#line 274 "../Main.m3"
 /* check_nil */
#line 274 "../Main.m3"
 /* store */
#line 274 "../Main.m3"
(*(ADDRESS*)(&Main_m_255_L_256))=(ADDRESS)(((ADDRESS)(*((ADDRESS*)(INT64_(8)+((ADDRESS)(*((ADDRESS*)(Main_m_248_L_249)))))))));
#line 274 "../Main.m3"
 /* load */
#line 274 "../Main.m3"
/*check_nil*/if(!Main_m_255_L_256)Main_m_M_Main_L_49_CRASH(8772);
#line 274 "../Main.m3"
 /* call_indirect */
#line 274 "../Main.m3"
((void (__cdecl*)(void*))Main_m_255_L_256)(
 ((ADDRESS)(Main_m_248_L_249)));
#line 274 "../Main.m3"
 /* load_nil */
#line 274 "../Main.m3"
 /* load */
#line 274 "../Main.m3"
 /* if_compare */
#line 274 "../Main.m3"
if(m3_eq(ADDRESS,
 ((ADDRESS)(0)),
 ((ADDRESS)(Main_m_244_L_245))))goto L8F;
#line 274 "../Main.m3"
 /* start_call_direct */
#line 274 "../Main.m3"
 /* load */
#line 274 "../Main.m3"
 /* pop_param */
#line 274 "../Main.m3"
 /* call_direct */
#line 274 "../Main.m3"
RTHooks__ResumeRaise(
  ( ADDRESS )(((ADDRESS)(Main_m_244_L_245)) ));
#line 274 "../Main.m3"
 /* set_source_line */
#line 274 "../Main.m3"
#line 276 "../Main.m3"
 /* set_label */
#line 276 "../Main.m3"
L8F:;
#line 276 "../Main.m3"
 /* set_source_line */
#line 276 "../Main.m3"
#line 277 "../Main.m3"
 /* start_call_direct */
#line 277 "../Main.m3"
 /* load */
#line 277 "../Main.m3"
 /* pop_param */
#line 277 "../Main.m3"
 /* call_direct */
#line 277 "../Main.m3"
 /* store */
#line 277 "../Main.m3"
(*(ADDRESS*)(&Main_m_246_L_247))=(ADDRESS)(((ADDRESS)(Main__ThImage(
  ( Main__ThreadNo )(((UINT8)(((INT64)(ThN_L_85)))) )))));
#line 277 "../Main.m3"
 /* start_call_direct */
#line 277 "../Main.m3"
 /* load */
#line 277 "../Main.m3"
 /* pop_param */
#line 277 "../Main.m3"
 /* load_address */
#line 277 "../Main.m3"
 /* pop_param */
#line 277 "../Main.m3"
 /* call_direct */
#line 277 "../Main.m3"
 /* store */
#line 277 "../Main.m3"
(*(ADDRESS*)(&Main_m_257_L_258))=(ADDRESS)(((ADDRESS)(RTHooks__Concat(
  ( TEXT )(((ADDRESS)(Main_m_246_L_247)) ),
  ( TEXT )(((ADDRESS)(INT64_(1040)+((ADDRESS)(&Main_m_47_L_48)))) )))));
#line 277 "../Main.m3"
 /* start_call_direct */
#line 277 "../Main.m3"
 /* load */
#line 277 "../Main.m3"
 /* pop_param */
#line 277 "../Main.m3"
 /* call_direct */
#line 277 "../Main.m3"
Main__W(
  ( TEXT )(((ADDRESS)(Main_m_257_L_258)) ));
#line 277 "../Main.m3"
 /* set_source_line */
#line 277 "../Main.m3"
#line 278 "../Main.m3"
 /* load */
#line 278 "../Main.m3"
 /* store */
#line 278 "../Main.m3"
(*(ADDRESS*)(&Main_m_257_L_258))=(ADDRESS)(((ADDRESS)(*((ADDRESS*)(INT64_(176)+((ADDRESS)(&Main_m_M_Main_L_49)))))));
#line 278 "../Main.m3"
 /* load_nil */
#line 278 "../Main.m3"
 /* load */
#line 278 "../Main.m3"
 /* if_compare */
#line 278 "../Main.m3"
if(m3_eq(ADDRESS,
 ((ADDRESS)(0)),
 ((ADDRESS)(Main_m_257_L_258))))goto L94;
#line 278 "../Main.m3"
 /* load */
#line 278 "../Main.m3"
 /* loophole */
#line 278 "../Main.m3"
 /* load_integer */
#line 278 "../Main.m3"
 /* and */
#line 278 "../Main.m3"
 /* if_true_or_false */
#line 278 "../Main.m3"
 /* load_host_integer */
#line 278 "../Main.m3"
 /* load_integer */
#line 278 "../Main.m3"
 /* if_compare */
#line 278 "../Main.m3"
if(m3_ne(INT64,
  ((INT64)(((INT64)((INT64)Main_m_257_L_258))&  INT64_(1))),
   INT64_(0)))goto L94;
#line 278 "../Main.m3"
 /* load */
#line 278 "../Main.m3"
 /* load_indirect */
#line 278 "../Main.m3"
 /* extract_mn */
#line 278 "../Main.m3"
 /* load_host_integer */
#line 278 "../Main.m3"
 /* load_integer */
#line 278 "../Main.m3"
 /* load_host_integer */
#line 278 "../Main.m3"
 /* load_integer */
#line 278 "../Main.m3"
 /* extract */
#line 278 "../Main.m3"
 /* if_true_or_false */
#line 278 "../Main.m3"
 /* load_host_integer */
#line 278 "../Main.m3"
 /* load_integer */
#line 278 "../Main.m3"
 /* if_compare */
#line 278 "../Main.m3"
if(m3_eq(INT64,
 ((INT64)(m3_extract(
 UINT64,
  *((INT64*)(INT64_(-8)+((ADDRESS)(Main_m_257_L_258)))),
   UINT64_(22),
   UINT64_(1)))),
   INT64_(0)))goto L94;
#line 278 "../Main.m3"
 /* start_call_direct */
#line 278 "../Main.m3"
 /* load */
#line 278 "../Main.m3"
 /* pop_param */
#line 278 "../Main.m3"
 /* call_direct */
#line 278 "../Main.m3"
RTHooks__CheckLoadTracedRef(
  ( REFANY )(((ADDRESS)(Main_m_257_L_258)) ));
#line 278 "../Main.m3"
 /* set_label */
#line 278 "../Main.m3"
L94:;
#line 278 "../Main.m3"
 /* load */
#line 278 "../Main.m3"
 /* store */
#line 278 "../Main.m3"
(*(ADDRESS*)(&Main_m_246_L_247))=(ADDRESS)(((ADDRESS)(*((ADDRESS*)(INT64_(184)+((ADDRESS)(&Main_m_M_Main_L_49)))))));
#line 278 "../Main.m3"
 /* load_nil */
#line 278 "../Main.m3"
 /* load */
#line 278 "../Main.m3"
 /* if_compare */
#line 278 "../Main.m3"
if(m3_eq(ADDRESS,
 ((ADDRESS)(0)),
 ((ADDRESS)(Main_m_246_L_247))))goto L95;
#line 278 "../Main.m3"
 /* load */
#line 278 "../Main.m3"
 /* loophole */
#line 278 "../Main.m3"
 /* load_integer */
#line 278 "../Main.m3"
 /* and */
#line 278 "../Main.m3"
 /* if_true_or_false */
#line 278 "../Main.m3"
 /* load_host_integer */
#line 278 "../Main.m3"
 /* load_integer */
#line 278 "../Main.m3"
 /* if_compare */
#line 278 "../Main.m3"
if(m3_ne(INT64,
  ((INT64)(((INT64)((INT64)Main_m_246_L_247))&  INT64_(1))),
   INT64_(0)))goto L95;
#line 278 "../Main.m3"
 /* load */
#line 278 "../Main.m3"
 /* load_indirect */
#line 278 "../Main.m3"
 /* extract_mn */
#line 278 "../Main.m3"
 /* load_host_integer */
#line 278 "../Main.m3"
 /* load_integer */
#line 278 "../Main.m3"
 /* load_host_integer */
#line 278 "../Main.m3"
 /* load_integer */
#line 278 "../Main.m3"
 /* extract */
#line 278 "../Main.m3"
 /* if_true_or_false */
#line 278 "../Main.m3"
 /* load_host_integer */
#line 278 "../Main.m3"
 /* load_integer */
#line 278 "../Main.m3"
 /* if_compare */
#line 278 "../Main.m3"
if(m3_eq(INT64,
 ((INT64)(m3_extract(
 UINT64,
  *((INT64*)(INT64_(-8)+((ADDRESS)(Main_m_246_L_247)))),
   UINT64_(22),
   UINT64_(1)))),
   INT64_(0)))goto L95;
#line 278 "../Main.m3"
 /* start_call_direct */
#line 278 "../Main.m3"
 /* load */
#line 278 "../Main.m3"
 /* pop_param */
#line 278 "../Main.m3"
 /* call_direct */
#line 278 "../Main.m3"
RTHooks__CheckLoadTracedRef(
  ( REFANY )(((ADDRESS)(Main_m_246_L_247)) ));
#line 278 "../Main.m3"
 /* set_label */
#line 278 "../Main.m3"
L95:;
#line 278 "../Main.m3"
 /* start_call_direct */
#line 278 "../Main.m3"
 /* load */
#line 278 "../Main.m3"
 /* pop_param */
#line 278 "../Main.m3"
 /* load */
#line 278 "../Main.m3"
 /* pop_param */
#line 278 "../Main.m3"
 /* call_direct */
#line 278 "../Main.m3"
Thread__Wait(
  ( Thread__Mutex )(((ADDRESS)(Main_m_257_L_258)) ),
  ( Thread__Condition )(((ADDRESS)(Main_m_246_L_247)) ));
#line 278 "../Main.m3"
 /* set_source_line */
#line 278 "../Main.m3"
#line 279 "../Main.m3"
 /* start_call_direct */
#line 279 "../Main.m3"
 /* load */
#line 279 "../Main.m3"
 /* pop_param */
#line 279 "../Main.m3"
 /* call_direct */
#line 279 "../Main.m3"
 /* store */
#line 279 "../Main.m3"
(*(ADDRESS*)(&Main_m_246_L_247))=(ADDRESS)(((ADDRESS)(Main__ThImage(
  ( Main__ThreadNo )(((UINT8)(((INT64)(ThN_L_85)))) )))));
#line 279 "../Main.m3"
 /* start_call_direct */
#line 279 "../Main.m3"
 /* load */
#line 279 "../Main.m3"
 /* pop_param */
#line 279 "../Main.m3"
 /* load_address */
#line 279 "../Main.m3"
 /* pop_param */
#line 279 "../Main.m3"
 /* call_direct */
#line 279 "../Main.m3"
 /* store */
#line 279 "../Main.m3"
(*(ADDRESS*)(&Main_m_257_L_258))=(ADDRESS)(((ADDRESS)(RTHooks__Concat(
  ( TEXT )(((ADDRESS)(Main_m_246_L_247)) ),
  ( TEXT )(((ADDRESS)(INT64_(1096)+((ADDRESS)(&Main_m_47_L_48)))) )))));
#line 279 "../Main.m3"
 /* start_call_direct */
#line 279 "../Main.m3"
 /* load */
#line 279 "../Main.m3"
 /* pop_param */
#line 279 "../Main.m3"
 /* call_direct */
#line 279 "../Main.m3"
Main__W(
  ( TEXT )(((ADDRESS)(Main_m_257_L_258)) ));
#line 279 "../Main.m3"
 /* set_source_line */
#line 279 "../Main.m3"
#line 282 "../Main.m3"
 /* load_nil */
#line 282 "../Main.m3"
 /* store */
#line 282 "../Main.m3"
(*(ADDRESS*)(&Main_m_259_L_260))=(ADDRESS)(((ADDRESS)(0)));
#line 282 "../Main.m3"
 /* load */
#line 282 "../Main.m3"
 /* store */
#line 282 "../Main.m3"
(*(ADDRESS*)(&Main_m_257_L_258))=(ADDRESS)(((ADDRESS)(*((ADDRESS*)(INT64_(104)+((ADDRESS)(&Main_m_M_Main_L_49)))))));
#line 282 "../Main.m3"
 /* load_nil */
#line 282 "../Main.m3"
 /* load */
#line 282 "../Main.m3"
 /* if_compare */
#line 282 "../Main.m3"
if(m3_eq(ADDRESS,
 ((ADDRESS)(0)),
 ((ADDRESS)(Main_m_257_L_258))))goto L96;
#line 282 "../Main.m3"
 /* load */
#line 282 "../Main.m3"
 /* loophole */
#line 282 "../Main.m3"
 /* load_integer */
#line 282 "../Main.m3"
 /* and */
#line 282 "../Main.m3"
 /* if_true_or_false */
#line 282 "../Main.m3"
 /* load_host_integer */
#line 282 "../Main.m3"
 /* load_integer */
#line 282 "../Main.m3"
 /* if_compare */
#line 282 "../Main.m3"
if(m3_ne(INT64,
  ((INT64)(((INT64)((INT64)Main_m_257_L_258))&  INT64_(1))),
   INT64_(0)))goto L96;
#line 282 "../Main.m3"
 /* load */
#line 282 "../Main.m3"
 /* load_indirect */
#line 282 "../Main.m3"
 /* extract_mn */
#line 282 "../Main.m3"
 /* load_host_integer */
#line 282 "../Main.m3"
 /* load_integer */
#line 282 "../Main.m3"
 /* load_host_integer */
#line 282 "../Main.m3"
 /* load_integer */
#line 282 "../Main.m3"
 /* extract */
#line 282 "../Main.m3"
 /* if_true_or_false */
#line 282 "../Main.m3"
 /* load_host_integer */
#line 282 "../Main.m3"
 /* load_integer */
#line 282 "../Main.m3"
 /* if_compare */
#line 282 "../Main.m3"
if(m3_eq(INT64,
 ((INT64)(m3_extract(
 UINT64,
  *((INT64*)(INT64_(-8)+((ADDRESS)(Main_m_257_L_258)))),
   UINT64_(22),
   UINT64_(1)))),
   INT64_(0)))goto L96;
#line 282 "../Main.m3"
 /* start_call_direct */
#line 282 "../Main.m3"
 /* load */
#line 282 "../Main.m3"
 /* pop_param */
#line 282 "../Main.m3"
 /* call_direct */
#line 282 "../Main.m3"
RTHooks__CheckLoadTracedRef(
  ( REFANY )(((ADDRESS)(Main_m_257_L_258)) ));
#line 282 "../Main.m3"
 /* set_label */
#line 282 "../Main.m3"
L96:;
#line 282 "../Main.m3"
 /* load */
#line 282 "../Main.m3"
 /* store */
#line 282 "../Main.m3"
(*(ADDRESS*)(&Main_m_261_L_262))=(ADDRESS)(((ADDRESS)(Main_m_257_L_258)));
#line 282 "../Main.m3"
 /* start_call_indirect */
#line 282 "../Main.m3"
 /* load */
#line 282 "../Main.m3"
 /* pop_param */
#line 282 "../Main.m3"
 /* load */
#line 282 "../Main.m3"
 /* load_indirect */
#line 282 "../Main.m3"
 /* load_indirect */
#line 282 "../Main.m3"
 /* check_nil */
#line 282 "../Main.m3"
 /* store */
#line 282 "../Main.m3"
(*(ADDRESS*)(&Main_m_263_L_264))=(ADDRESS)(((ADDRESS)(*((ADDRESS*)(*((ADDRESS*)(Main_m_261_L_262)))))));
#line 282 "../Main.m3"
 /* load */
#line 282 "../Main.m3"
/*check_nil*/if(!Main_m_263_L_264)Main_m_M_Main_L_49_CRASH(9028);
#line 282 "../Main.m3"
 /* call_indirect */
#line 282 "../Main.m3"
((void (__cdecl*)(void*))Main_m_263_L_264)(
 ((ADDRESS)(Main_m_261_L_262)));
#line 282 "../Main.m3"
 /* set_label */
#line 282 "../Main.m3"
 /* start_try */
#line 282 "../Main.m3"
try {
#line 282 "../Main.m3"
 /* set_source_line */
#line 282 "../Main.m3"
#line 283 "../Main.m3"
 /* load */
#line 283 "../Main.m3"
 /* store */
#line 283 "../Main.m3"
(*(INT64*)(&Main_m_252_L_253))=(INT64)( ((INT64)(ThN_L_85)));
#line 283 "../Main.m3"
 /* load_address */
#line 283 "../Main.m3"
 /* load */
#line 283 "../Main.m3"
 /* index_address */
#line 283 "../Main.m3"
 /* store */
#line 283 "../Main.m3"
(*(ADDRESS*)(&Main_m_257_L_258))=(ADDRESS)(((ADDRESS)((((ADDRESS)(INT64_(112)+((ADDRESS)(&Main_m_M_Main_L_49))))+( Main_m_252_L_253)))));
#line 283 "../Main.m3"
 /* begin_block */
#line 283 "../Main.m3"
 /* load */
#line 283 "../Main.m3"
 /* store */
#line 283 "../Main.m3"
(*(ADDRESS*)(&WThN_L_265))=(ADDRESS)(((ADDRESS)(Main_m_257_L_258)));
#line 283 "../Main.m3"
 /* set_source_line */
#line 283 "../Main.m3"
#line 285 "../Main.m3"
 /* load */
#line 285 "../Main.m3"
 /* load_indirect */
#line 285 "../Main.m3"
 /* load_integer */
#line 285 "../Main.m3"
 /* if_compare */
#line 285 "../Main.m3"
if(m3_eq(UINT64,
 ((UINT64)(((INT64)(*((UINT8*)(WThN_L_265)))))),
 ((UINT64)( INT64_(5)))))goto L9B;
#line 285 "../Main.m3"
 /* start_call_direct */
#line 285 "../Main.m3"
 /* load_address */
#line 285 "../Main.m3"
 /* pop_param */
#line 285 "../Main.m3"
 /* load_integer */
#line 285 "../Main.m3"
 /* pop_param */
#line 285 "../Main.m3"
 /* load_address */
#line 285 "../Main.m3"
 /* pop_param */
#line 285 "../Main.m3"
 /* invoke_direct */
#line 285 "../Main.m3"
 /* call_direct */
#line 285 "../Main.m3"
RTHooks__AssertFailed(
  ( ADDRESS )(((ADDRESS)(&Main_m_M_Main_L_49)) ),
  ( INTEGER )(  INT64_(285) ),
  ( TEXT )(((ADDRESS)(INT64_(1176)+((ADDRESS)(&Main_m_47_L_48)))) ));
#line 285 "../Main.m3"
 /* set_label */
#line 285 "../Main.m3"
 /* set_label */
#line 285 "../Main.m3"
L9B:;
#line 285 "../Main.m3"
 /* set_source_line */
#line 285 "../Main.m3"
#line 286 "../Main.m3"
 /* load */
#line 286 "../Main.m3"
 /* store */
#line 286 "../Main.m3"
(*(INT64*)(&Main_m_252_L_253))=(INT64)( ((INT64)(ThN_L_85)));
#line 286 "../Main.m3"
 /* load_address */
#line 286 "../Main.m3"
 /* load */
#line 286 "../Main.m3"
 /* index_address */
#line 286 "../Main.m3"
 /* store */
#line 286 "../Main.m3"
(*(ADDRESS*)(&Main_m_257_L_258))=(ADDRESS)(((ADDRESS)((((ADDRESS)(INT64_(112)+((ADDRESS)(&Main_m_M_Main_L_49))))+( Main_m_252_L_253)))));
#line 286 "../Main.m3"
 /* load */
#line 286 "../Main.m3"
 /* load_integer */
#line 286 "../Main.m3"
 /* store_indirect */
#line 286 "../Main.m3"
(*(UINT8*)(Main_m_257_L_258))=(INT64)(  INT64_(1));
#line 286 "../Main.m3"
 /* set_source_line */
#line 286 "../Main.m3"
#line 287 "../Main.m3"
 /* load */
#line 287 "../Main.m3"
 /* store */
#line 287 "../Main.m3"
(*(UINT8*)((117)+(char*)(&Main_m_M_Main_L_49)))=(INT64)( ((INT64)(ThN_L_85)));
#line 287 "../Main.m3"
 /* end_block */
#line 287 "../Main.m3"
 /* jump */
#line 287 "../Main.m3"
goto L99;
#line 287 "../Main.m3"
 /* end_try */
#line 287 "../Main.m3"
} catch (_M3Exc& _m3exc) { _m3_caught = _m3exc.act; goto L98; }
#line 287 "../Main.m3"
 /* set_label */
#line 287 "../Main.m3"
L98:;
#line 287 "../Main.m3"
 /* landing_pad */
#line 287 "../Main.m3"
 /* store */
#line 287 "../Main.m3"
(*(ADDRESS*)(&Main_m_259_L_260))=(ADDRESS)(((ADDRESS)(_m3_caught)));
#line 287 "../Main.m3"
 /* set_label */
#line 287 "../Main.m3"
L99:;
#line 287 "../Main.m3"
 /* start_call_indirect */
#line 287 "../Main.m3"
 /* load */
#line 287 "../Main.m3"
 /* pop_param */
#line 287 "../Main.m3"
 /* load */
#line 287 "../Main.m3"
 /* load_indirect */
#line 287 "../Main.m3"
 /* load_indirect */
#line 287 "../Main.m3"
 /* check_nil */
#line 287 "../Main.m3"
 /* store */
#line 287 "../Main.m3"
(*(ADDRESS*)(&Main_m_266_L_267))=(ADDRESS)(((ADDRESS)(*((ADDRESS*)(INT64_(8)+((ADDRESS)(*((ADDRESS*)(Main_m_261_L_262)))))))));
#line 287 "../Main.m3"
 /* load */
#line 287 "../Main.m3"
/*check_nil*/if(!Main_m_266_L_267)Main_m_M_Main_L_49_CRASH(9188);
#line 287 "../Main.m3"
 /* call_indirect */
#line 287 "../Main.m3"
((void (__cdecl*)(void*))Main_m_266_L_267)(
 ((ADDRESS)(Main_m_261_L_262)));
#line 287 "../Main.m3"
 /* load_nil */
#line 287 "../Main.m3"
 /* load */
#line 287 "../Main.m3"
 /* if_compare */
#line 287 "../Main.m3"
if(m3_eq(ADDRESS,
 ((ADDRESS)(0)),
 ((ADDRESS)(Main_m_259_L_260))))goto L9A;
#line 287 "../Main.m3"
 /* start_call_direct */
#line 287 "../Main.m3"
 /* load */
#line 287 "../Main.m3"
 /* pop_param */
#line 287 "../Main.m3"
 /* call_direct */
#line 287 "../Main.m3"
RTHooks__ResumeRaise(
  ( ADDRESS )(((ADDRESS)(Main_m_259_L_260)) ));
#line 287 "../Main.m3"
 /* set_source_line */
#line 287 "../Main.m3"
#line 289 "../Main.m3"
 /* set_label */
#line 289 "../Main.m3"
L9A:;
#line 289 "../Main.m3"
 /* set_source_line */
#line 289 "../Main.m3"
#line 290 "../Main.m3"
 /* exit_proc */
#line 290 "../Main.m3"
return;
#line 290 "../Main.m3"
 /* end_procedure */
#line 290 "../Main.m3"
} /* DoSignal */
#line 290 "../Main.m3"
 /* set_source_line */
#line 290 "../Main.m3"
#line 292 "../Main.m3"
 /* begin_procedure */
#line 292 "../Main.m3"
struct Main__DoSignal_Frame_t {
#line 292 "../Main.m3"
ADDRESS _unused;
#line 292 "../Main.m3"
};
#line 292 "../Main.m3"
void /*TypeText3*/ 
__cdecl
Main__DoSignal(
   /* Param_Type1 */ Main__ThreadNo ThN_L_86)
{
#line 292 "../Main.m3"
 /* Var_Type2 */ ADDRESS Main_m_268_L_269={0};//always-init
#line 292 "../Main.m3"
 /* Var_Type2 */ ADDRESS Main_m_270_L_271={0};//always-init
#line 292 "../Main.m3"
 /* Var_Type1 */ MUTEX Main_m_272_L_273={0};//always-init
#line 292 "../Main.m3"
 /* Var_Type2 */ ADDRESS Main_m_274_L_275={0};//always-init
#line 292 "../Main.m3"
 /* Var_Type2 */ INT64 Main_m_276_L_277={0};//always-init
#line 292 "../Main.m3"
 /* Var_Type2 */ ADDRESS Main_m_278_L_279={0};//always-init
#line 292 "../Main.m3"
 /* Var_Type2 */ ADDRESS Main_m_280_L_281={0};//always-init
#line 292 "../Main.m3"
 /* Var_Type2 */ ADDRESS Main_m_282_L_283={0};//always-init
#line 292 "../Main.m3"
 /* Var_Type1 */ MUTEX Main_m_284_L_285={0};//always-init
#line 292 "../Main.m3"
 /* Var_Type2 */ ADDRESS Main_m_286_L_287={0};//always-init
#line 292 "../Main.m3"
 /* Var_Type2 */ ADDRESS Main_m_288_L_289={0};//always-init
#line 292 "../Main.m3"
Main__DoSignal_Frame_t _frame;
#line 292 "../Main.m3"
_frame._unused=(ADDRESS)&_frame;
#line 292 "../Main.m3"
 /* set_source_line */
#line 292 "../Main.m3"
#line 293 "../Main.m3"
 /* set_source_line */
#line 293 "../Main.m3"
#line 294 "../Main.m3"
 /* load_nil */
#line 294 "../Main.m3"
 /* store */
#line 294 "../Main.m3"
(*(ADDRESS*)(&Main_m_268_L_269))=(ADDRESS)(((ADDRESS)(0)));
#line 294 "../Main.m3"
 /* load */
#line 294 "../Main.m3"
 /* store */
#line 294 "../Main.m3"
(*(ADDRESS*)(&Main_m_270_L_271))=(ADDRESS)(((ADDRESS)(*((ADDRESS*)(INT64_(104)+((ADDRESS)(&Main_m_M_Main_L_49)))))));
#line 294 "../Main.m3"
 /* load_nil */
#line 294 "../Main.m3"
 /* load */
#line 294 "../Main.m3"
 /* if_compare */
#line 294 "../Main.m3"
if(m3_eq(ADDRESS,
 ((ADDRESS)(0)),
 ((ADDRESS)(Main_m_270_L_271))))goto L9D;
#line 294 "../Main.m3"
 /* load */
#line 294 "../Main.m3"
 /* loophole */
#line 294 "../Main.m3"
 /* load_integer */
#line 294 "../Main.m3"
 /* and */
#line 294 "../Main.m3"
 /* if_true_or_false */
#line 294 "../Main.m3"
 /* load_host_integer */
#line 294 "../Main.m3"
 /* load_integer */
#line 294 "../Main.m3"
 /* if_compare */
#line 294 "../Main.m3"
if(m3_ne(INT64,
  ((INT64)(((INT64)((INT64)Main_m_270_L_271))&  INT64_(1))),
   INT64_(0)))goto L9D;
#line 294 "../Main.m3"
 /* load */
#line 294 "../Main.m3"
 /* load_indirect */
#line 294 "../Main.m3"
 /* extract_mn */
#line 294 "../Main.m3"
 /* load_host_integer */
#line 294 "../Main.m3"
 /* load_integer */
#line 294 "../Main.m3"
 /* load_host_integer */
#line 294 "../Main.m3"
 /* load_integer */
#line 294 "../Main.m3"
 /* extract */
#line 294 "../Main.m3"
 /* if_true_or_false */
#line 294 "../Main.m3"
 /* load_host_integer */
#line 294 "../Main.m3"
 /* load_integer */
#line 294 "../Main.m3"
 /* if_compare */
#line 294 "../Main.m3"
if(m3_eq(INT64,
 ((INT64)(m3_extract(
 UINT64,
  *((INT64*)(INT64_(-8)+((ADDRESS)(Main_m_270_L_271)))),
   UINT64_(22),
   UINT64_(1)))),
   INT64_(0)))goto L9D;
#line 294 "../Main.m3"
 /* start_call_direct */
#line 294 "../Main.m3"
 /* load */
#line 294 "../Main.m3"
 /* pop_param */
#line 294 "../Main.m3"
 /* call_direct */
#line 294 "../Main.m3"
RTHooks__CheckLoadTracedRef(
  ( REFANY )(((ADDRESS)(Main_m_270_L_271)) ));
#line 294 "../Main.m3"
 /* set_label */
#line 294 "../Main.m3"
L9D:;
#line 294 "../Main.m3"
 /* load */
#line 294 "../Main.m3"
 /* store */
#line 294 "../Main.m3"
(*(ADDRESS*)(&Main_m_272_L_273))=(ADDRESS)(((ADDRESS)(Main_m_270_L_271)));
#line 294 "../Main.m3"
 /* start_call_indirect */
#line 294 "../Main.m3"
 /* load */
#line 294 "../Main.m3"
 /* pop_param */
#line 294 "../Main.m3"
 /* load */
#line 294 "../Main.m3"
 /* load_indirect */
#line 294 "../Main.m3"
 /* load_indirect */
#line 294 "../Main.m3"
 /* check_nil */
#line 294 "../Main.m3"
 /* store */
#line 294 "../Main.m3"
(*(ADDRESS*)(&Main_m_274_L_275))=(ADDRESS)(((ADDRESS)(*((ADDRESS*)(*((ADDRESS*)(Main_m_272_L_273)))))));
#line 294 "../Main.m3"
 /* load */
#line 294 "../Main.m3"
/*check_nil*/if(!Main_m_274_L_275)Main_m_M_Main_L_49_CRASH(9412);
#line 294 "../Main.m3"
 /* call_indirect */
#line 294 "../Main.m3"
((void (__cdecl*)(void*))Main_m_274_L_275)(
 ((ADDRESS)(Main_m_272_L_273)));
#line 294 "../Main.m3"
 /* set_label */
#line 294 "../Main.m3"
 /* start_try */
#line 294 "../Main.m3"
try {
#line 294 "../Main.m3"
 /* load */
#line 294 "../Main.m3"
 /* store */
#line 294 "../Main.m3"
(*(INT64*)(&Main_m_276_L_277))=(INT64)( ((INT64)(ThN_L_86)));
#line 294 "../Main.m3"
 /* load_address */
#line 294 "../Main.m3"
 /* load */
#line 294 "../Main.m3"
 /* index_address */
#line 294 "../Main.m3"
 /* store */
#line 294 "../Main.m3"
(*(ADDRESS*)(&Main_m_270_L_271))=(ADDRESS)(((ADDRESS)((((ADDRESS)(INT64_(112)+((ADDRESS)(&Main_m_M_Main_L_49))))+( Main_m_276_L_277)))));
#line 294 "../Main.m3"
 /* load */
#line 294 "../Main.m3"
 /* load_integer */
#line 294 "../Main.m3"
 /* store_indirect */
#line 294 "../Main.m3"
(*(UINT8*)(Main_m_270_L_271))=(INT64)(  INT64_(6));
#line 294 "../Main.m3"
 /* jump */
#line 294 "../Main.m3"
goto LA0;
#line 294 "../Main.m3"
 /* end_try */
#line 294 "../Main.m3"
} catch (...) { throw; }
#line 294 "../Main.m3"
 /* set_label */
#line 294 "../Main.m3"
 /* landing_pad */
#line 294 "../Main.m3"
 /* store */
#line 294 "../Main.m3"
(*(ADDRESS*)(&Main_m_268_L_269))=(ADDRESS)(((ADDRESS)(_m3_caught)));
#line 294 "../Main.m3"
 /* set_label */
#line 294 "../Main.m3"
LA0:;
#line 294 "../Main.m3"
 /* start_call_indirect */
#line 294 "../Main.m3"
 /* load */
#line 294 "../Main.m3"
 /* pop_param */
#line 294 "../Main.m3"
 /* load */
#line 294 "../Main.m3"
 /* load_indirect */
#line 294 "../Main.m3"
 /* load_indirect */
#line 294 "../Main.m3"
 /* check_nil */
#line 294 "../Main.m3"
 /* store */
#line 294 "../Main.m3"
(*(ADDRESS*)(&Main_m_278_L_279))=(ADDRESS)(((ADDRESS)(*((ADDRESS*)(INT64_(8)+((ADDRESS)(*((ADDRESS*)(Main_m_272_L_273)))))))));
#line 294 "../Main.m3"
 /* load */
#line 294 "../Main.m3"
/*check_nil*/if(!Main_m_278_L_279)Main_m_M_Main_L_49_CRASH(9412);
#line 294 "../Main.m3"
 /* call_indirect */
#line 294 "../Main.m3"
((void (__cdecl*)(void*))Main_m_278_L_279)(
 ((ADDRESS)(Main_m_272_L_273)));
#line 294 "../Main.m3"
 /* load_nil */
#line 294 "../Main.m3"
 /* load */
#line 294 "../Main.m3"
 /* if_compare */
#line 294 "../Main.m3"
if(m3_eq(ADDRESS,
 ((ADDRESS)(0)),
 ((ADDRESS)(Main_m_268_L_269))))goto LA1;
#line 294 "../Main.m3"
 /* start_call_direct */
#line 294 "../Main.m3"
 /* load */
#line 294 "../Main.m3"
 /* pop_param */
#line 294 "../Main.m3"
 /* call_direct */
#line 294 "../Main.m3"
RTHooks__ResumeRaise(
  ( ADDRESS )(((ADDRESS)(Main_m_268_L_269)) ));
#line 294 "../Main.m3"
 /* set_label */
#line 294 "../Main.m3"
LA1:;
#line 294 "../Main.m3"
 /* set_source_line */
#line 294 "../Main.m3"
#line 295 "../Main.m3"
 /* start_call_direct */
#line 295 "../Main.m3"
 /* load */
#line 295 "../Main.m3"
 /* pop_param */
#line 295 "../Main.m3"
 /* call_direct */
#line 295 "../Main.m3"
 /* store */
#line 295 "../Main.m3"
(*(ADDRESS*)(&Main_m_270_L_271))=(ADDRESS)(((ADDRESS)(Main__ThImage(
  ( Main__ThreadNo )(((UINT8)(((INT64)(ThN_L_86)))) )))));
#line 295 "../Main.m3"
 /* start_call_direct */
#line 295 "../Main.m3"
 /* load */
#line 295 "../Main.m3"
 /* pop_param */
#line 295 "../Main.m3"
 /* load_address */
#line 295 "../Main.m3"
 /* pop_param */
#line 295 "../Main.m3"
 /* call_direct */
#line 295 "../Main.m3"
 /* store */
#line 295 "../Main.m3"
(*(ADDRESS*)(&Main_m_280_L_281))=(ADDRESS)(((ADDRESS)(RTHooks__Concat(
  ( TEXT )(((ADDRESS)(Main_m_270_L_271)) ),
  ( TEXT )(((ADDRESS)(INT64_(1224)+((ADDRESS)(&Main_m_47_L_48)))) )))));
#line 295 "../Main.m3"
 /* start_call_direct */
#line 295 "../Main.m3"
 /* load */
#line 295 "../Main.m3"
 /* pop_param */
#line 295 "../Main.m3"
 /* call_direct */
#line 295 "../Main.m3"
Main__W(
  ( TEXT )(((ADDRESS)(Main_m_280_L_281)) ));
#line 295 "../Main.m3"
 /* set_source_line */
#line 295 "../Main.m3"
#line 296 "../Main.m3"
 /* load */
#line 296 "../Main.m3"
 /* store */
#line 296 "../Main.m3"
(*(ADDRESS*)(&Main_m_280_L_281))=(ADDRESS)(((ADDRESS)(*((ADDRESS*)(INT64_(184)+((ADDRESS)(&Main_m_M_Main_L_49)))))));
#line 296 "../Main.m3"
 /* load_nil */
#line 296 "../Main.m3"
 /* load */
#line 296 "../Main.m3"
 /* if_compare */
#line 296 "../Main.m3"
if(m3_eq(ADDRESS,
 ((ADDRESS)(0)),
 ((ADDRESS)(Main_m_280_L_281))))goto LA2;
#line 296 "../Main.m3"
 /* load */
#line 296 "../Main.m3"
 /* loophole */
#line 296 "../Main.m3"
 /* load_integer */
#line 296 "../Main.m3"
 /* and */
#line 296 "../Main.m3"
 /* if_true_or_false */
#line 296 "../Main.m3"
 /* load_host_integer */
#line 296 "../Main.m3"
 /* load_integer */
#line 296 "../Main.m3"
 /* if_compare */
#line 296 "../Main.m3"
if(m3_ne(INT64,
  ((INT64)(((INT64)((INT64)Main_m_280_L_281))&  INT64_(1))),
   INT64_(0)))goto LA2;
#line 296 "../Main.m3"
 /* load */
#line 296 "../Main.m3"
 /* load_indirect */
#line 296 "../Main.m3"
 /* extract_mn */
#line 296 "../Main.m3"
 /* load_host_integer */
#line 296 "../Main.m3"
 /* load_integer */
#line 296 "../Main.m3"
 /* load_host_integer */
#line 296 "../Main.m3"
 /* load_integer */
#line 296 "../Main.m3"
 /* extract */
#line 296 "../Main.m3"
 /* if_true_or_false */
#line 296 "../Main.m3"
 /* load_host_integer */
#line 296 "../Main.m3"
 /* load_integer */
#line 296 "../Main.m3"
 /* if_compare */
#line 296 "../Main.m3"
if(m3_eq(INT64,
 ((INT64)(m3_extract(
 UINT64,
  *((INT64*)(INT64_(-8)+((ADDRESS)(Main_m_280_L_281)))),
   UINT64_(22),
   UINT64_(1)))),
   INT64_(0)))goto LA2;
#line 296 "../Main.m3"
 /* start_call_direct */
#line 296 "../Main.m3"
 /* load */
#line 296 "../Main.m3"
 /* pop_param */
#line 296 "../Main.m3"
 /* call_direct */
#line 296 "../Main.m3"
RTHooks__CheckLoadTracedRef(
  ( REFANY )(((ADDRESS)(Main_m_280_L_281)) ));
#line 296 "../Main.m3"
 /* set_label */
#line 296 "../Main.m3"
LA2:;
#line 296 "../Main.m3"
 /* start_call_direct */
#line 296 "../Main.m3"
 /* load */
#line 296 "../Main.m3"
 /* pop_param */
#line 296 "../Main.m3"
 /* call_direct */
#line 296 "../Main.m3"
Thread__Signal(
  ( Thread__Condition )(((ADDRESS)(Main_m_280_L_281)) ));
#line 296 "../Main.m3"
 /* set_source_line */
#line 296 "../Main.m3"
#line 297 "../Main.m3"
 /* start_call_direct */
#line 297 "../Main.m3"
 /* load */
#line 297 "../Main.m3"
 /* pop_param */
#line 297 "../Main.m3"
 /* call_direct */
#line 297 "../Main.m3"
 /* store */
#line 297 "../Main.m3"
(*(ADDRESS*)(&Main_m_280_L_281))=(ADDRESS)(((ADDRESS)(Main__ThImage(
  ( Main__ThreadNo )(((UINT8)(((INT64)(ThN_L_86)))) )))));
#line 297 "../Main.m3"
 /* start_call_direct */
#line 297 "../Main.m3"
 /* load */
#line 297 "../Main.m3"
 /* pop_param */
#line 297 "../Main.m3"
 /* load_address */
#line 297 "../Main.m3"
 /* pop_param */
#line 297 "../Main.m3"
 /* call_direct */
#line 297 "../Main.m3"
 /* store */
#line 297 "../Main.m3"
(*(ADDRESS*)(&Main_m_270_L_271))=(ADDRESS)(((ADDRESS)(RTHooks__Concat(
  ( TEXT )(((ADDRESS)(Main_m_280_L_281)) ),
  ( TEXT )(((ADDRESS)(INT64_(1280)+((ADDRESS)(&Main_m_47_L_48)))) )))));
#line 297 "../Main.m3"
 /* start_call_direct */
#line 297 "../Main.m3"
 /* load */
#line 297 "../Main.m3"
 /* pop_param */
#line 297 "../Main.m3"
 /* call_direct */
#line 297 "../Main.m3"
Main__W(
  ( TEXT )(((ADDRESS)(Main_m_270_L_271)) ));
#line 297 "../Main.m3"
 /* set_source_line */
#line 297 "../Main.m3"
#line 298 "../Main.m3"
 /* load_nil */
#line 298 "../Main.m3"
 /* store */
#line 298 "../Main.m3"
(*(ADDRESS*)(&Main_m_282_L_283))=(ADDRESS)(((ADDRESS)(0)));
#line 298 "../Main.m3"
 /* load */
#line 298 "../Main.m3"
 /* store */
#line 298 "../Main.m3"
(*(ADDRESS*)(&Main_m_270_L_271))=(ADDRESS)(((ADDRESS)(*((ADDRESS*)(INT64_(104)+((ADDRESS)(&Main_m_M_Main_L_49)))))));
#line 298 "../Main.m3"
 /* load_nil */
#line 298 "../Main.m3"
 /* load */
#line 298 "../Main.m3"
 /* if_compare */
#line 298 "../Main.m3"
if(m3_eq(ADDRESS,
 ((ADDRESS)(0)),
 ((ADDRESS)(Main_m_270_L_271))))goto LA3;
#line 298 "../Main.m3"
 /* load */
#line 298 "../Main.m3"
 /* loophole */
#line 298 "../Main.m3"
 /* load_integer */
#line 298 "../Main.m3"
 /* and */
#line 298 "../Main.m3"
 /* if_true_or_false */
#line 298 "../Main.m3"
 /* load_host_integer */
#line 298 "../Main.m3"
 /* load_integer */
#line 298 "../Main.m3"
 /* if_compare */
#line 298 "../Main.m3"
if(m3_ne(INT64,
  ((INT64)(((INT64)((INT64)Main_m_270_L_271))&  INT64_(1))),
   INT64_(0)))goto LA3;
#line 298 "../Main.m3"
 /* load */
#line 298 "../Main.m3"
 /* load_indirect */
#line 298 "../Main.m3"
 /* extract_mn */
#line 298 "../Main.m3"
 /* load_host_integer */
#line 298 "../Main.m3"
 /* load_integer */
#line 298 "../Main.m3"
 /* load_host_integer */
#line 298 "../Main.m3"
 /* load_integer */
#line 298 "../Main.m3"
 /* extract */
#line 298 "../Main.m3"
 /* if_true_or_false */
#line 298 "../Main.m3"
 /* load_host_integer */
#line 298 "../Main.m3"
 /* load_integer */
#line 298 "../Main.m3"
 /* if_compare */
#line 298 "../Main.m3"
if(m3_eq(INT64,
 ((INT64)(m3_extract(
 UINT64,
  *((INT64*)(INT64_(-8)+((ADDRESS)(Main_m_270_L_271)))),
   UINT64_(22),
   UINT64_(1)))),
   INT64_(0)))goto LA3;
#line 298 "../Main.m3"
 /* start_call_direct */
#line 298 "../Main.m3"
 /* load */
#line 298 "../Main.m3"
 /* pop_param */
#line 298 "../Main.m3"
 /* call_direct */
#line 298 "../Main.m3"
RTHooks__CheckLoadTracedRef(
  ( REFANY )(((ADDRESS)(Main_m_270_L_271)) ));
#line 298 "../Main.m3"
 /* set_label */
#line 298 "../Main.m3"
LA3:;
#line 298 "../Main.m3"
 /* load */
#line 298 "../Main.m3"
 /* store */
#line 298 "../Main.m3"
(*(ADDRESS*)(&Main_m_284_L_285))=(ADDRESS)(((ADDRESS)(Main_m_270_L_271)));
#line 298 "../Main.m3"
 /* start_call_indirect */
#line 298 "../Main.m3"
 /* load */
#line 298 "../Main.m3"
 /* pop_param */
#line 298 "../Main.m3"
 /* load */
#line 298 "../Main.m3"
 /* load_indirect */
#line 298 "../Main.m3"
 /* load_indirect */
#line 298 "../Main.m3"
 /* check_nil */
#line 298 "../Main.m3"
 /* store */
#line 298 "../Main.m3"
(*(ADDRESS*)(&Main_m_286_L_287))=(ADDRESS)(((ADDRESS)(*((ADDRESS*)(*((ADDRESS*)(Main_m_284_L_285)))))));
#line 298 "../Main.m3"
 /* load */
#line 298 "../Main.m3"
/*check_nil*/if(!Main_m_286_L_287)Main_m_M_Main_L_49_CRASH(9540);
#line 298 "../Main.m3"
 /* call_indirect */
#line 298 "../Main.m3"
((void (__cdecl*)(void*))Main_m_286_L_287)(
 ((ADDRESS)(Main_m_284_L_285)));
#line 298 "../Main.m3"
 /* set_label */
#line 298 "../Main.m3"
 /* start_try */
#line 298 "../Main.m3"
try {
#line 298 "../Main.m3"
 /* load */
#line 298 "../Main.m3"
 /* store */
#line 298 "../Main.m3"
(*(INT64*)(&Main_m_276_L_277))=(INT64)( ((INT64)(ThN_L_86)));
#line 298 "../Main.m3"
 /* load_address */
#line 298 "../Main.m3"
 /* load */
#line 298 "../Main.m3"
 /* index_address */
#line 298 "../Main.m3"
 /* store */
#line 298 "../Main.m3"
(*(ADDRESS*)(&Main_m_270_L_271))=(ADDRESS)(((ADDRESS)((((ADDRESS)(INT64_(112)+((ADDRESS)(&Main_m_M_Main_L_49))))+( Main_m_276_L_277)))));
#line 298 "../Main.m3"
 /* load */
#line 298 "../Main.m3"
 /* load_integer */
#line 298 "../Main.m3"
 /* store_indirect */
#line 298 "../Main.m3"
(*(UINT8*)(Main_m_270_L_271))=(INT64)(  INT64_(1));
#line 298 "../Main.m3"
 /* jump */
#line 298 "../Main.m3"
goto LA6;
#line 298 "../Main.m3"
 /* end_try */
#line 298 "../Main.m3"
} catch (...) { throw; }
#line 298 "../Main.m3"
 /* set_label */
#line 298 "../Main.m3"
 /* landing_pad */
#line 298 "../Main.m3"
 /* store */
#line 298 "../Main.m3"
(*(ADDRESS*)(&Main_m_282_L_283))=(ADDRESS)(((ADDRESS)(_m3_caught)));
#line 298 "../Main.m3"
 /* set_label */
#line 298 "../Main.m3"
LA6:;
#line 298 "../Main.m3"
 /* start_call_indirect */
#line 298 "../Main.m3"
 /* load */
#line 298 "../Main.m3"
 /* pop_param */
#line 298 "../Main.m3"
 /* load */
#line 298 "../Main.m3"
 /* load_indirect */
#line 298 "../Main.m3"
 /* load_indirect */
#line 298 "../Main.m3"
 /* check_nil */
#line 298 "../Main.m3"
 /* store */
#line 298 "../Main.m3"
(*(ADDRESS*)(&Main_m_288_L_289))=(ADDRESS)(((ADDRESS)(*((ADDRESS*)(INT64_(8)+((ADDRESS)(*((ADDRESS*)(Main_m_284_L_285)))))))));
#line 298 "../Main.m3"
 /* load */
#line 298 "../Main.m3"
/*check_nil*/if(!Main_m_288_L_289)Main_m_M_Main_L_49_CRASH(9540);
#line 298 "../Main.m3"
 /* call_indirect */
#line 298 "../Main.m3"
((void (__cdecl*)(void*))Main_m_288_L_289)(
 ((ADDRESS)(Main_m_284_L_285)));
#line 298 "../Main.m3"
 /* load_nil */
#line 298 "../Main.m3"
 /* load */
#line 298 "../Main.m3"
 /* if_compare */
#line 298 "../Main.m3"
if(m3_eq(ADDRESS,
 ((ADDRESS)(0)),
 ((ADDRESS)(Main_m_282_L_283))))goto LA7;
#line 298 "../Main.m3"
 /* start_call_direct */
#line 298 "../Main.m3"
 /* load */
#line 298 "../Main.m3"
 /* pop_param */
#line 298 "../Main.m3"
 /* call_direct */
#line 298 "../Main.m3"
RTHooks__ResumeRaise(
  ( ADDRESS )(((ADDRESS)(Main_m_282_L_283)) ));
#line 298 "../Main.m3"
 /* set_label */
#line 298 "../Main.m3"
LA7:;
#line 298 "../Main.m3"
 /* set_source_line */
#line 298 "../Main.m3"
#line 299 "../Main.m3"
 /* exit_proc */
#line 299 "../Main.m3"
return;
#line 299 "../Main.m3"
 /* end_procedure */
#line 299 "../Main.m3"
} /* ForceSignalled */
#line 299 "../Main.m3"
 /* set_source_line */
#line 299 "../Main.m3"
#line 301 "../Main.m3"
 /* begin_procedure */
#line 301 "../Main.m3"
struct Main__ForceSignalled_Frame_t {
#line 301 "../Main.m3"
ADDRESS _unused;
#line 301 "../Main.m3"
};
#line 301 "../Main.m3"
void /*TypeText3*/ 
__cdecl
Main__ForceSignalled(
   /* Param_Type1 */ Main__ThreadNo ThN_L_88)
{
#line 301 "../Main.m3"
 /* Var_Type1 */ T8E2831D7_8 LThN_L_87={0};//always-init
#line 301 "../Main.m3"
 /* Var_Type2 */ INT64 Main_m_290_L_291={0};//always-init
#line 301 "../Main.m3"
Main__ForceSignalled_Frame_t _frame;
#line 301 "../Main.m3"
_frame._unused=(ADDRESS)&_frame;
#line 301 "../Main.m3"
 /* set_source_line */
#line 301 "../Main.m3"
#line 307 "../Main.m3"
 /* load_integer */
#line 307 "../Main.m3"
 /* store */
#line 307 "../Main.m3"
(*(UINT8*)(&LThN_L_87))=(INT64)(  INT64_(0));
#line 307 "../Main.m3"
 /* set_source_line */
#line 307 "../Main.m3"
#line 310 "../Main.m3"
 /* start_call_direct */
#line 310 "../Main.m3"
 /* load_float */
#line 310 "../Main.m3"
 /* pop_param */
#line 310 "../Main.m3"
 /* call_direct */
#line 310 "../Main.m3"
Thread__Pause(
  ( LONGREAL )( ((double)(3.00000000000000000e0)) ));
#line 310 "../Main.m3"
 /* set_source_line */
#line 310 "../Main.m3"
#line 311 "../Main.m3"
 /* start_call_direct */
#line 311 "../Main.m3"
 /* load */
#line 311 "../Main.m3"
 /* pop_param */
#line 311 "../Main.m3"
 /* load_integer */
#line 311 "../Main.m3"
 /* pop_param */
#line 311 "../Main.m3"
 /* load_address */
#line 311 "../Main.m3"
 /* pop_param */
#line 311 "../Main.m3"
 /* load_address */
#line 311 "../Main.m3"
 /* pop_param */
#line 311 "../Main.m3"
 /* call_direct */
#line 311 "../Main.m3"
 /* store */
#line 311 "../Main.m3"
(*(INT64*)(&Main_m_290_L_291))=(INT64)(((INT64)(Main__NoteWhetherState(
  ( Main__ThreadNo )(((UINT8)(((INT64)(ThN_L_88)))) ),
  ( Main__State )(((UINT8)( INT64_(5))) ),
  ( TEXT )(((ADDRESS)(INT64_(1328)+((ADDRESS)(&Main_m_47_L_48)))) ),
  ( TEXT )(((ADDRESS)(INT64_(1328)+((ADDRESS)(&Main_m_47_L_48)))) )))));
#line 311 "../Main.m3"
 /* load */
#line 311 "../Main.m3"
 /* if_true_or_false */
#line 311 "../Main.m3"
 /* load_host_integer */
#line 311 "../Main.m3"
 /* load_integer */
#line 311 "../Main.m3"
 /* if_compare */
#line 311 "../Main.m3"
if(m3_eq(INT64,
  Main_m_290_L_291,
   INT64_(0)))goto LA9;
#line 311 "../Main.m3"
 /* set_source_line */
#line 311 "../Main.m3"
#line 313 "../Main.m3"
 /* start_call_direct */
#line 313 "../Main.m3"
 /* load_integer */
#line 313 "../Main.m3"
 /* pop_param */
#line 313 "../Main.m3"
 /* load_procedure */
#line 313 "../Main.m3"
 /* pop_param */
#line 313 "../Main.m3"
 /* call_direct */
#line 313 "../Main.m3"
Main__Action(
  ( Main__ThreadNo )(((UINT8)( INT64_(3))) ),
  ( Main__ActionProc )(((ADDRESS)(Main__DoSignal)) ));
#line 313 "../Main.m3"
 /* set_source_line */
#line 313 "../Main.m3"
#line 314 "../Main.m3"
 /* start_call_direct */
#line 314 "../Main.m3"
 /* load_integer */
#line 314 "../Main.m3"
 /* pop_param */
#line 314 "../Main.m3"
 /* load_integer */
#line 314 "../Main.m3"
 /* pop_param */
#line 314 "../Main.m3"
 /* call_direct */
#line 314 "../Main.m3"
Main__WaitForState(
  ( Main__ThreadNo )(((UINT8)( INT64_(3))) ),
  ( Main__State )(((UINT8)( INT64_(1))) ));
#line 314 "../Main.m3"
 /* set_source_line */
#line 314 "../Main.m3"
#line 315 "../Main.m3"
 /* start_call_direct */
#line 315 "../Main.m3"
 /* call_direct */
#line 315 "../Main.m3"
 /* store */
#line 315 "../Main.m3"
(*(INT64*)(&Main_m_290_L_291))=(INT64)(((INT64)(Main__WaitForHeld(
 ))));
#line 315 "../Main.m3"
 /* load */
#line 315 "../Main.m3"
 /* store */
#line 315 "../Main.m3"
(*(UINT8*)(&LThN_L_87))=(INT64)( Main_m_290_L_291);
#line 315 "../Main.m3"
 /* set_source_line */
#line 315 "../Main.m3"
#line 316 "../Main.m3"
 /* load */
#line 316 "../Main.m3"
 /* load */
#line 316 "../Main.m3"
 /* if_compare */
#line 316 "../Main.m3"
if(m3_eq(INT64,
  ((INT64)(ThN_L_88)),
  ((INT64)(LThN_L_87))))goto LAA;
#line 316 "../Main.m3"
 /* start_call_direct */
#line 316 "../Main.m3"
 /* load_address */
#line 316 "../Main.m3"
 /* pop_param */
#line 316 "../Main.m3"
 /* load_integer */
#line 316 "../Main.m3"
 /* pop_param */
#line 316 "../Main.m3"
 /* load_address */
#line 316 "../Main.m3"
 /* pop_param */
#line 316 "../Main.m3"
 /* call_direct */
#line 316 "../Main.m3"
RTHooks__AssertFailed(
  ( ADDRESS )(((ADDRESS)(&Main_m_M_Main_L_49)) ),
  ( INTEGER )(  INT64_(316) ),
  ( TEXT )(((ADDRESS)(INT64_(1360)+((ADDRESS)(&Main_m_47_L_48)))) ));
#line 316 "../Main.m3"
 /* set_label */
#line 316 "../Main.m3"
LAA:;
#line 316 "../Main.m3"
 /* set_label */
#line 316 "../Main.m3"
LA9:;
#line 316 "../Main.m3"
 /* set_source_line */
#line 316 "../Main.m3"
#line 318 "../Main.m3"
 /* start_call_direct */
#line 318 "../Main.m3"
 /* load */
#line 318 "../Main.m3"
 /* pop_param */
#line 318 "../Main.m3"
 /* load_procedure */
#line 318 "../Main.m3"
 /* pop_param */
#line 318 "../Main.m3"
 /* call_direct */
#line 318 "../Main.m3"
Main__Action(
  ( Main__ThreadNo )(((UINT8)(((INT64)(ThN_L_88)))) ),
  ( Main__ActionProc )(((ADDRESS)(Main__DoRel)) ));
#line 318 "../Main.m3"
 /* set_source_line */
#line 318 "../Main.m3"
#line 319 "../Main.m3"
 /* start_call_direct */
#line 319 "../Main.m3"
 /* load */
#line 319 "../Main.m3"
 /* pop_param */
#line 319 "../Main.m3"
 /* load_integer */
#line 319 "../Main.m3"
 /* pop_param */
#line 319 "../Main.m3"
 /* call_direct */
#line 319 "../Main.m3"
Main__WaitForState(
  ( Main__ThreadNo )(((UINT8)(((INT64)(ThN_L_88)))) ),
  ( Main__State )(((UINT8)( INT64_(1))) ));
#line 319 "../Main.m3"
 /* set_source_line */
#line 319 "../Main.m3"
#line 320 "../Main.m3"
 /* exit_proc */
#line 320 "../Main.m3"
return;
#line 320 "../Main.m3"
 /* end_procedure */
#line 320 "../Main.m3"
} /* TestSeq */
#line 320 "../Main.m3"
 /* set_source_line */
#line 320 "../Main.m3"
#line 322 "../Main.m3"
 /* begin_procedure */
#line 322 "../Main.m3"
struct Main__TestSeq_Frame_t {
#line 322 "../Main.m3"
ADDRESS _unused;
#line 322 "../Main.m3"
};
#line 322 "../Main.m3"
void /*TypeText3*/ 
__cdecl
Main__TestSeq(void)
{
#line 322 "../Main.m3"
 /* Var_Type1 */ T8E2831D7_8 LThNo_L_89={0};//always-init
#line 322 "../Main.m3"
 /* Var_Type2 */ ADDRESS Main_m_292_L_293={0};//always-init
#line 322 "../Main.m3"
 /* Var_Type2 */ INT64 Main_m_294_L_295={0};//always-init
#line 322 "../Main.m3"
Main__TestSeq_Frame_t _frame;
#line 322 "../Main.m3"
_frame._unused=(ADDRESS)&_frame;
#line 322 "../Main.m3"
 /* set_source_line */
#line 322 "../Main.m3"
#line 324 "../Main.m3"
 /* load_integer */
#line 324 "../Main.m3"
 /* store */
#line 324 "../Main.m3"
(*(UINT8*)(&LThNo_L_89))=(INT64)(  INT64_(0));
#line 324 "../Main.m3"
 /* set_source_line */
#line 324 "../Main.m3"
#line 326 "../Main.m3"
 /* load_nil */
#line 326 "../Main.m3"
 /* store */
#line 326 "../Main.m3"
(*(ADDRESS*)(&Main_m_292_L_293))=(ADDRESS)(((ADDRESS)(0)));
#line 326 "../Main.m3"
 /* set_label */
#line 326 "../Main.m3"
 /* start_try */
#line 326 "../Main.m3"
try {
#line 326 "../Main.m3"
 /* set_source_line */
#line 326 "../Main.m3"
#line 329 "../Main.m3"
 /* start_call_direct */
#line 329 "../Main.m3"
 /* load_integer */
#line 329 "../Main.m3"
 /* pop_param */
#line 329 "../Main.m3"
 /* load_procedure */
#line 329 "../Main.m3"
 /* pop_param */
#line 329 "../Main.m3"
 /* invoke_direct */
#line 329 "../Main.m3"
 /* call_direct */
#line 329 "../Main.m3"
Main__Action(
  ( Main__ThreadNo )(((UINT8)( INT64_(1))) ),
  ( Main__ActionProc )(((ADDRESS)(Main__DoAcq)) ));
#line 329 "../Main.m3"
 /* set_label */
#line 329 "../Main.m3"
 /* set_source_line */
#line 329 "../Main.m3"
#line 330 "../Main.m3"
 /* start_call_direct */
#line 330 "../Main.m3"
 /* load_integer */
#line 330 "../Main.m3"
 /* pop_param */
#line 330 "../Main.m3"
 /* load_integer */
#line 330 "../Main.m3"
 /* pop_param */
#line 330 "../Main.m3"
 /* invoke_direct */
#line 330 "../Main.m3"
 /* call_direct */
#line 330 "../Main.m3"
Main__WaitForState(
  ( Main__ThreadNo )(((UINT8)( INT64_(1))) ),
  ( Main__State )(((UINT8)( INT64_(1))) ));
#line 330 "../Main.m3"
 /* set_label */
#line 330 "../Main.m3"
 /* set_source_line */
#line 330 "../Main.m3"
#line 331 "../Main.m3"
 /* start_call_direct */
#line 331 "../Main.m3"
 /* load_integer */
#line 331 "../Main.m3"
 /* pop_param */
#line 331 "../Main.m3"
 /* load_procedure */
#line 331 "../Main.m3"
 /* pop_param */
#line 331 "../Main.m3"
 /* invoke_direct */
#line 331 "../Main.m3"
 /* call_direct */
#line 331 "../Main.m3"
Main__ActionWait(
  ( Main__ThreadNo )(((UINT8)( INT64_(1))) ),
  ( Main__ActionProc )(((ADDRESS)(Main__DoWait)) ));
#line 331 "../Main.m3"
 /* set_label */
#line 331 "../Main.m3"
 /* set_source_line */
#line 331 "../Main.m3"
#line 332 "../Main.m3"
 /* start_call_direct */
#line 332 "../Main.m3"
 /* load_integer */
#line 332 "../Main.m3"
 /* pop_param */
#line 332 "../Main.m3"
 /* load_integer */
#line 332 "../Main.m3"
 /* pop_param */
#line 332 "../Main.m3"
 /* invoke_direct */
#line 332 "../Main.m3"
 /* call_direct */
#line 332 "../Main.m3"
Main__WaitForState(
  ( Main__ThreadNo )(((UINT8)( INT64_(1))) ),
  ( Main__State )(((UINT8)( INT64_(5))) ));
#line 332 "../Main.m3"
 /* set_label */
#line 332 "../Main.m3"
 /* set_source_line */
#line 332 "../Main.m3"
#line 335 "../Main.m3"
 /* start_call_direct */
#line 335 "../Main.m3"
 /* load_integer */
#line 335 "../Main.m3"
 /* pop_param */
#line 335 "../Main.m3"
 /* load_procedure */
#line 335 "../Main.m3"
 /* pop_param */
#line 335 "../Main.m3"
 /* invoke_direct */
#line 335 "../Main.m3"
 /* call_direct */
#line 335 "../Main.m3"
Main__Action(
  ( Main__ThreadNo )(((UINT8)( INT64_(2))) ),
  ( Main__ActionProc )(((ADDRESS)(Main__DoAcq)) ));
#line 335 "../Main.m3"
 /* set_label */
#line 335 "../Main.m3"
 /* set_source_line */
#line 335 "../Main.m3"
#line 336 "../Main.m3"
 /* start_call_direct */
#line 336 "../Main.m3"
 /* load_integer */
#line 336 "../Main.m3"
 /* pop_param */
#line 336 "../Main.m3"
 /* load_integer */
#line 336 "../Main.m3"
 /* pop_param */
#line 336 "../Main.m3"
 /* invoke_direct */
#line 336 "../Main.m3"
 /* call_direct */
#line 336 "../Main.m3"
Main__WaitForState(
  ( Main__ThreadNo )(((UINT8)( INT64_(2))) ),
  ( Main__State )(((UINT8)( INT64_(1))) ));
#line 336 "../Main.m3"
 /* set_label */
#line 336 "../Main.m3"
 /* set_source_line */
#line 336 "../Main.m3"
#line 337 "../Main.m3"
 /* start_call_direct */
#line 337 "../Main.m3"
 /* load_integer */
#line 337 "../Main.m3"
 /* pop_param */
#line 337 "../Main.m3"
 /* load_procedure */
#line 337 "../Main.m3"
 /* pop_param */
#line 337 "../Main.m3"
 /* invoke_direct */
#line 337 "../Main.m3"
 /* call_direct */
#line 337 "../Main.m3"
Main__ActionWait(
  ( Main__ThreadNo )(((UINT8)( INT64_(2))) ),
  ( Main__ActionProc )(((ADDRESS)(Main__DoWait)) ));
#line 337 "../Main.m3"
 /* set_label */
#line 337 "../Main.m3"
 /* set_source_line */
#line 337 "../Main.m3"
#line 338 "../Main.m3"
 /* start_call_direct */
#line 338 "../Main.m3"
 /* load_integer */
#line 338 "../Main.m3"
 /* pop_param */
#line 338 "../Main.m3"
 /* load_integer */
#line 338 "../Main.m3"
 /* pop_param */
#line 338 "../Main.m3"
 /* invoke_direct */
#line 338 "../Main.m3"
 /* call_direct */
#line 338 "../Main.m3"
Main__WaitForState(
  ( Main__ThreadNo )(((UINT8)( INT64_(2))) ),
  ( Main__State )(((UINT8)( INT64_(5))) ));
#line 338 "../Main.m3"
 /* set_label */
#line 338 "../Main.m3"
 /* set_source_line */
#line 338 "../Main.m3"
#line 351 "../Main.m3"
 /* start_call_direct */
#line 351 "../Main.m3"
 /* load_integer */
#line 351 "../Main.m3"
 /* pop_param */
#line 351 "../Main.m3"
 /* load_procedure */
#line 351 "../Main.m3"
 /* pop_param */
#line 351 "../Main.m3"
 /* invoke_direct */
#line 351 "../Main.m3"
 /* call_direct */
#line 351 "../Main.m3"
Main__Action(
  ( Main__ThreadNo )(((UINT8)( INT64_(3))) ),
  ( Main__ActionProc )(((ADDRESS)(Main__DoAcq)) ));
#line 351 "../Main.m3"
 /* set_label */
#line 351 "../Main.m3"
 /* set_source_line */
#line 351 "../Main.m3"
#line 352 "../Main.m3"
 /* start_call_direct */
#line 352 "../Main.m3"
 /* load_integer */
#line 352 "../Main.m3"
 /* pop_param */
#line 352 "../Main.m3"
 /* load_integer */
#line 352 "../Main.m3"
 /* pop_param */
#line 352 "../Main.m3"
 /* invoke_direct */
#line 352 "../Main.m3"
 /* call_direct */
#line 352 "../Main.m3"
Main__WaitForState(
  ( Main__ThreadNo )(((UINT8)( INT64_(3))) ),
  ( Main__State )(((UINT8)( INT64_(1))) ));
#line 352 "../Main.m3"
 /* set_label */
#line 352 "../Main.m3"
 /* set_source_line */
#line 352 "../Main.m3"
#line 356 "../Main.m3"
 /* start_call_direct */
#line 356 "../Main.m3"
 /* load_integer */
#line 356 "../Main.m3"
 /* pop_param */
#line 356 "../Main.m3"
 /* load_procedure */
#line 356 "../Main.m3"
 /* pop_param */
#line 356 "../Main.m3"
 /* invoke_direct */
#line 356 "../Main.m3"
 /* call_direct */
#line 356 "../Main.m3"
Main__Action(
  ( Main__ThreadNo )(((UINT8)( INT64_(3))) ),
  ( Main__ActionProc )(((ADDRESS)(Main__DoSignal)) ));
#line 356 "../Main.m3"
 /* set_label */
#line 356 "../Main.m3"
 /* set_source_line */
#line 356 "../Main.m3"
#line 357 "../Main.m3"
 /* start_call_direct */
#line 357 "../Main.m3"
 /* load_integer */
#line 357 "../Main.m3"
 /* pop_param */
#line 357 "../Main.m3"
 /* load_integer */
#line 357 "../Main.m3"
 /* pop_param */
#line 357 "../Main.m3"
 /* invoke_direct */
#line 357 "../Main.m3"
 /* call_direct */
#line 357 "../Main.m3"
Main__WaitForState(
  ( Main__ThreadNo )(((UINT8)( INT64_(3))) ),
  ( Main__State )(((UINT8)( INT64_(1))) ));
#line 357 "../Main.m3"
 /* set_label */
#line 357 "../Main.m3"
 /* set_source_line */
#line 357 "../Main.m3"
#line 361 "../Main.m3"
 /* start_call_direct */
#line 361 "../Main.m3"
 /* load_integer */
#line 361 "../Main.m3"
 /* pop_param */
#line 361 "../Main.m3"
 /* load_procedure */
#line 361 "../Main.m3"
 /* pop_param */
#line 361 "../Main.m3"
 /* invoke_direct */
#line 361 "../Main.m3"
 /* call_direct */
#line 361 "../Main.m3"
Main__Action(
  ( Main__ThreadNo )(((UINT8)( INT64_(3))) ),
  ( Main__ActionProc )(((ADDRESS)(Main__DoRel)) ));
#line 361 "../Main.m3"
 /* set_label */
#line 361 "../Main.m3"
 /* set_source_line */
#line 361 "../Main.m3"
#line 362 "../Main.m3"
 /* start_call_direct */
#line 362 "../Main.m3"
 /* load_integer */
#line 362 "../Main.m3"
 /* pop_param */
#line 362 "../Main.m3"
 /* load_integer */
#line 362 "../Main.m3"
 /* pop_param */
#line 362 "../Main.m3"
 /* invoke_direct */
#line 362 "../Main.m3"
 /* call_direct */
#line 362 "../Main.m3"
Main__WaitForState(
  ( Main__ThreadNo )(((UINT8)( INT64_(3))) ),
  ( Main__State )(((UINT8)( INT64_(1))) ));
#line 362 "../Main.m3"
 /* set_label */
#line 362 "../Main.m3"
 /* set_source_line */
#line 362 "../Main.m3"
#line 365 "../Main.m3"
 /* start_call_direct */
#line 365 "../Main.m3"
 /* invoke_direct */
#line 365 "../Main.m3"
 /* call_direct */
#line 365 "../Main.m3"
 /* set_label */
#line 365 "../Main.m3"
 /* store */
#line 365 "../Main.m3"
(*(INT64*)(&Main_m_294_L_295))=(INT64)(((INT64)(Main__WaitForHeld(
 ))));
#line 365 "../Main.m3"
 /* load */
#line 365 "../Main.m3"
 /* store */
#line 365 "../Main.m3"
(*(UINT8*)(&LThNo_L_89))=(INT64)( Main_m_294_L_295);
#line 365 "../Main.m3"
 /* set_source_line */
#line 365 "../Main.m3"
#line 366 "../Main.m3"
 /* load_integer */
#line 366 "../Main.m3"
 /* load */
#line 366 "../Main.m3"
 /* if_compare */
#line 366 "../Main.m3"
if(m3_ne(INT64,
   INT64_(1),
  ((INT64)(LThNo_L_89))))goto LBF;
#line 366 "../Main.m3"
 /* set_source_line */
#line 366 "../Main.m3"
#line 369 "../Main.m3"
 /* start_call_direct */
#line 369 "../Main.m3"
 /* load_integer */
#line 369 "../Main.m3"
 /* pop_param */
#line 369 "../Main.m3"
 /* load_procedure */
#line 369 "../Main.m3"
 /* pop_param */
#line 369 "../Main.m3"
 /* invoke_direct */
#line 369 "../Main.m3"
 /* call_direct */
#line 369 "../Main.m3"
Main__Action(
  ( Main__ThreadNo )(((UINT8)( INT64_(1))) ),
  ( Main__ActionProc )(((ADDRESS)(Main__DoRel)) ));
#line 369 "../Main.m3"
 /* set_label */
#line 369 "../Main.m3"
 /* set_source_line */
#line 369 "../Main.m3"
#line 370 "../Main.m3"
 /* start_call_direct */
#line 370 "../Main.m3"
 /* load_integer */
#line 370 "../Main.m3"
 /* pop_param */
#line 370 "../Main.m3"
 /* load_integer */
#line 370 "../Main.m3"
 /* pop_param */
#line 370 "../Main.m3"
 /* invoke_direct */
#line 370 "../Main.m3"
 /* call_direct */
#line 370 "../Main.m3"
Main__WaitForState(
  ( Main__ThreadNo )(((UINT8)( INT64_(1))) ),
  ( Main__State )(((UINT8)( INT64_(1))) ));
#line 370 "../Main.m3"
 /* set_label */
#line 370 "../Main.m3"
 /* set_source_line */
#line 370 "../Main.m3"
#line 371 "../Main.m3"
 /* start_call_direct */
#line 371 "../Main.m3"
 /* load_integer */
#line 371 "../Main.m3"
 /* pop_param */
#line 371 "../Main.m3"
 /* invoke_direct */
#line 371 "../Main.m3"
 /* call_direct */
#line 371 "../Main.m3"
Main__ForceSignalled(
  ( Main__ThreadNo )(((UINT8)( INT64_(2))) ));
#line 371 "../Main.m3"
 /* set_label */
#line 371 "../Main.m3"
 /* jump */
#line 371 "../Main.m3"
goto LBE;
#line 371 "../Main.m3"
 /* set_label */
#line 371 "../Main.m3"
LBF:;
#line 371 "../Main.m3"
 /* set_source_line */
#line 371 "../Main.m3"
#line 373 "../Main.m3"
 /* load_integer */
#line 373 "../Main.m3"
 /* load */
#line 373 "../Main.m3"
 /* if_compare */
#line 373 "../Main.m3"
if(m3_eq(INT64,
   INT64_(2),
  ((INT64)(LThNo_L_89))))goto LC3;
#line 373 "../Main.m3"
 /* start_call_direct */
#line 373 "../Main.m3"
 /* load_address */
#line 373 "../Main.m3"
 /* pop_param */
#line 373 "../Main.m3"
 /* load_integer */
#line 373 "../Main.m3"
 /* pop_param */
#line 373 "../Main.m3"
 /* load_address */
#line 373 "../Main.m3"
 /* pop_param */
#line 373 "../Main.m3"
 /* invoke_direct */
#line 373 "../Main.m3"
 /* call_direct */
#line 373 "../Main.m3"
RTHooks__AssertFailed(
  ( ADDRESS )(((ADDRESS)(&Main_m_M_Main_L_49)) ),
  ( INTEGER )(  INT64_(373) ),
  ( TEXT )(((ADDRESS)(INT64_(1400)+((ADDRESS)(&Main_m_47_L_48)))) ));
#line 373 "../Main.m3"
 /* set_label */
#line 373 "../Main.m3"
 /* set_label */
#line 373 "../Main.m3"
LC3:;
#line 373 "../Main.m3"
 /* set_source_line */
#line 373 "../Main.m3"
#line 374 "../Main.m3"
 /* start_call_direct */
#line 374 "../Main.m3"
 /* load_integer */
#line 374 "../Main.m3"
 /* pop_param */
#line 374 "../Main.m3"
 /* load_procedure */
#line 374 "../Main.m3"
 /* pop_param */
#line 374 "../Main.m3"
 /* invoke_direct */
#line 374 "../Main.m3"
 /* call_direct */
#line 374 "../Main.m3"
Main__Action(
  ( Main__ThreadNo )(((UINT8)( INT64_(2))) ),
  ( Main__ActionProc )(((ADDRESS)(Main__DoRel)) ));
#line 374 "../Main.m3"
 /* set_label */
#line 374 "../Main.m3"
 /* set_source_line */
#line 374 "../Main.m3"
#line 375 "../Main.m3"
 /* start_call_direct */
#line 375 "../Main.m3"
 /* load_integer */
#line 375 "../Main.m3"
 /* pop_param */
#line 375 "../Main.m3"
 /* load_integer */
#line 375 "../Main.m3"
 /* pop_param */
#line 375 "../Main.m3"
 /* invoke_direct */
#line 375 "../Main.m3"
 /* call_direct */
#line 375 "../Main.m3"
Main__WaitForState(
  ( Main__ThreadNo )(((UINT8)( INT64_(2))) ),
  ( Main__State )(((UINT8)( INT64_(1))) ));
#line 375 "../Main.m3"
 /* set_label */
#line 375 "../Main.m3"
 /* set_source_line */
#line 375 "../Main.m3"
#line 376 "../Main.m3"
 /* start_call_direct */
#line 376 "../Main.m3"
 /* load_integer */
#line 376 "../Main.m3"
 /* pop_param */
#line 376 "../Main.m3"
 /* invoke_direct */
#line 376 "../Main.m3"
 /* call_direct */
#line 376 "../Main.m3"
Main__ForceSignalled(
  ( Main__ThreadNo )(((UINT8)( INT64_(1))) ));
#line 376 "../Main.m3"
 /* set_label */
#line 376 "../Main.m3"
 /* set_label */
#line 376 "../Main.m3"
LBE:;
#line 376 "../Main.m3"
 /* set_source_line */
#line 376 "../Main.m3"
#line 380 "../Main.m3"
 /* start_call_direct */
#line 380 "../Main.m3"
 /* load_integer */
#line 380 "../Main.m3"
 /* pop_param */
#line 380 "../Main.m3"
 /* load_procedure */
#line 380 "../Main.m3"
 /* pop_param */
#line 380 "../Main.m3"
 /* invoke_direct */
#line 380 "../Main.m3"
 /* call_direct */
#line 380 "../Main.m3"
Main__Action(
  ( Main__ThreadNo )(((UINT8)( INT64_(4))) ),
  ( Main__ActionProc )(((ADDRESS)(Main__DoAcq)) ));
#line 380 "../Main.m3"
 /* set_label */
#line 380 "../Main.m3"
 /* set_source_line */
#line 380 "../Main.m3"
#line 381 "../Main.m3"
 /* start_call_direct */
#line 381 "../Main.m3"
 /* load_integer */
#line 381 "../Main.m3"
 /* pop_param */
#line 381 "../Main.m3"
 /* load_integer */
#line 381 "../Main.m3"
 /* pop_param */
#line 381 "../Main.m3"
 /* invoke_direct */
#line 381 "../Main.m3"
 /* call_direct */
#line 381 "../Main.m3"
Main__WaitForState(
  ( Main__ThreadNo )(((UINT8)( INT64_(4))) ),
  ( Main__State )(((UINT8)( INT64_(1))) ));
#line 381 "../Main.m3"
 /* set_label */
#line 381 "../Main.m3"
 /* set_source_line */
#line 381 "../Main.m3"
#line 382 "../Main.m3"
 /* start_call_direct */
#line 382 "../Main.m3"
 /* load_integer */
#line 382 "../Main.m3"
 /* pop_param */
#line 382 "../Main.m3"
 /* load_procedure */
#line 382 "../Main.m3"
 /* pop_param */
#line 382 "../Main.m3"
 /* invoke_direct */
#line 382 "../Main.m3"
 /* call_direct */
#line 382 "../Main.m3"
Main__ActionWait(
  ( Main__ThreadNo )(((UINT8)( INT64_(4))) ),
  ( Main__ActionProc )(((ADDRESS)(Main__DoWait)) ));
#line 382 "../Main.m3"
 /* set_label */
#line 382 "../Main.m3"
 /* set_source_line */
#line 382 "../Main.m3"
#line 383 "../Main.m3"
 /* start_call_direct */
#line 383 "../Main.m3"
 /* load_integer */
#line 383 "../Main.m3"
 /* pop_param */
#line 383 "../Main.m3"
 /* load_integer */
#line 383 "../Main.m3"
 /* pop_param */
#line 383 "../Main.m3"
 /* invoke_direct */
#line 383 "../Main.m3"
 /* call_direct */
#line 383 "../Main.m3"
Main__WaitForState(
  ( Main__ThreadNo )(((UINT8)( INT64_(4))) ),
  ( Main__State )(((UINT8)( INT64_(5))) ));
#line 383 "../Main.m3"
 /* set_label */
#line 383 "../Main.m3"
 /* set_source_line */
#line 383 "../Main.m3"
#line 386 "../Main.m3"
 /* start_call_direct */
#line 386 "../Main.m3"
 /* load_integer */
#line 386 "../Main.m3"
 /* pop_param */
#line 386 "../Main.m3"
 /* load_procedure */
#line 386 "../Main.m3"
 /* pop_param */
#line 386 "../Main.m3"
 /* invoke_direct */
#line 386 "../Main.m3"
 /* call_direct */
#line 386 "../Main.m3"
Main__Action(
  ( Main__ThreadNo )(((UINT8)( INT64_(3))) ),
  ( Main__ActionProc )(((ADDRESS)(Main__DoAcq)) ));
#line 386 "../Main.m3"
 /* set_label */
#line 386 "../Main.m3"
 /* set_source_line */
#line 386 "../Main.m3"
#line 387 "../Main.m3"
 /* start_call_direct */
#line 387 "../Main.m3"
 /* load_integer */
#line 387 "../Main.m3"
 /* pop_param */
#line 387 "../Main.m3"
 /* load_integer */
#line 387 "../Main.m3"
 /* pop_param */
#line 387 "../Main.m3"
 /* invoke_direct */
#line 387 "../Main.m3"
 /* call_direct */
#line 387 "../Main.m3"
Main__WaitForState(
  ( Main__ThreadNo )(((UINT8)( INT64_(3))) ),
  ( Main__State )(((UINT8)( INT64_(1))) ));
#line 387 "../Main.m3"
 /* set_label */
#line 387 "../Main.m3"
 /* set_source_line */
#line 387 "../Main.m3"
#line 388 "../Main.m3"
 /* start_call_direct */
#line 388 "../Main.m3"
 /* load_integer */
#line 388 "../Main.m3"
 /* pop_param */
#line 388 "../Main.m3"
 /* load_procedure */
#line 388 "../Main.m3"
 /* pop_param */
#line 388 "../Main.m3"
 /* invoke_direct */
#line 388 "../Main.m3"
 /* call_direct */
#line 388 "../Main.m3"
Main__Action(
  ( Main__ThreadNo )(((UINT8)( INT64_(3))) ),
  ( Main__ActionProc )(((ADDRESS)(Main__DoSignal)) ));
#line 388 "../Main.m3"
 /* set_label */
#line 388 "../Main.m3"
 /* set_source_line */
#line 388 "../Main.m3"
#line 389 "../Main.m3"
 /* start_call_direct */
#line 389 "../Main.m3"
 /* load_integer */
#line 389 "../Main.m3"
 /* pop_param */
#line 389 "../Main.m3"
 /* load_integer */
#line 389 "../Main.m3"
 /* pop_param */
#line 389 "../Main.m3"
 /* invoke_direct */
#line 389 "../Main.m3"
 /* call_direct */
#line 389 "../Main.m3"
Main__WaitForState(
  ( Main__ThreadNo )(((UINT8)( INT64_(3))) ),
  ( Main__State )(((UINT8)( INT64_(1))) ));
#line 389 "../Main.m3"
 /* set_label */
#line 389 "../Main.m3"
 /* set_source_line */
#line 389 "../Main.m3"
#line 390 "../Main.m3"
 /* start_call_direct */
#line 390 "../Main.m3"
 /* load_integer */
#line 390 "../Main.m3"
 /* pop_param */
#line 390 "../Main.m3"
 /* load_procedure */
#line 390 "../Main.m3"
 /* pop_param */
#line 390 "../Main.m3"
 /* invoke_direct */
#line 390 "../Main.m3"
 /* call_direct */
#line 390 "../Main.m3"
Main__Action(
  ( Main__ThreadNo )(((UINT8)( INT64_(3))) ),
  ( Main__ActionProc )(((ADDRESS)(Main__DoRel)) ));
#line 390 "../Main.m3"
 /* set_label */
#line 390 "../Main.m3"
 /* set_source_line */
#line 390 "../Main.m3"
#line 391 "../Main.m3"
 /* start_call_direct */
#line 391 "../Main.m3"
 /* load_integer */
#line 391 "../Main.m3"
 /* pop_param */
#line 391 "../Main.m3"
 /* load_integer */
#line 391 "../Main.m3"
 /* pop_param */
#line 391 "../Main.m3"
 /* invoke_direct */
#line 391 "../Main.m3"
 /* call_direct */
#line 391 "../Main.m3"
Main__WaitForState(
  ( Main__ThreadNo )(((UINT8)( INT64_(3))) ),
  ( Main__State )(((UINT8)( INT64_(1))) ));
#line 391 "../Main.m3"
 /* set_label */
#line 391 "../Main.m3"
 /* set_source_line */
#line 391 "../Main.m3"
#line 394 "../Main.m3"
 /* start_call_direct */
#line 394 "../Main.m3"
 /* load_float */
#line 394 "../Main.m3"
 /* pop_param */
#line 394 "../Main.m3"
 /* invoke_direct */
#line 394 "../Main.m3"
 /* call_direct */
#line 394 "../Main.m3"
Thread__Pause(
  ( LONGREAL )( ((double)(3.00000000000000000e0)) ));
#line 394 "../Main.m3"
 /* set_label */
#line 394 "../Main.m3"
 /* set_source_line */
#line 394 "../Main.m3"
#line 397 "../Main.m3"
 /* start_call_direct */
#line 397 "../Main.m3"
 /* load_integer */
#line 397 "../Main.m3"
 /* pop_param */
#line 397 "../Main.m3"
 /* load_integer */
#line 397 "../Main.m3"
 /* pop_param */
#line 397 "../Main.m3"
 /* invoke_direct */
#line 397 "../Main.m3"
 /* call_direct */
#line 397 "../Main.m3"
Main__WaitForStateSet(
  ( Main__ThreadNo )(((UINT8)( INT64_(4))) ),
  ( Main__StateSet )(((UINT8)( INT64_(34))) ));
#line 397 "../Main.m3"
 /* set_label */
#line 397 "../Main.m3"
 /* set_source_line */
#line 397 "../Main.m3"
#line 398 "../Main.m3"
 /* start_call_direct */
#line 398 "../Main.m3"
 /* load_integer */
#line 398 "../Main.m3"
 /* pop_param */
#line 398 "../Main.m3"
 /* load_integer */
#line 398 "../Main.m3"
 /* pop_param */
#line 398 "../Main.m3"
 /* load_address */
#line 398 "../Main.m3"
 /* pop_param */
#line 398 "../Main.m3"
 /* load_address */
#line 398 "../Main.m3"
 /* pop_param */
#line 398 "../Main.m3"
 /* invoke_direct */
#line 398 "../Main.m3"
 /* call_direct */
#line 398 "../Main.m3"
 /* set_label */
#line 398 "../Main.m3"
 /* store */
#line 398 "../Main.m3"
(*(INT64*)(&Main_m_294_L_295))=(INT64)(((INT64)(Main__NoteWhetherState(
  ( Main__ThreadNo )(((UINT8)( INT64_(4))) ),
  ( Main__State )(((UINT8)( INT64_(1))) ),
  ( TEXT )(((ADDRESS)(INT64_(1328)+((ADDRESS)(&Main_m_47_L_48)))) ),
  ( TEXT )(((ADDRESS)(INT64_(1328)+((ADDRESS)(&Main_m_47_L_48)))) )))));
#line 398 "../Main.m3"
 /* load */
#line 398 "../Main.m3"
 /* if_true_or_false */
#line 398 "../Main.m3"
 /* load_host_integer */
#line 398 "../Main.m3"
 /* load_integer */
#line 398 "../Main.m3"
 /* if_compare */
#line 398 "../Main.m3"
if(m3_eq(INT64,
  Main_m_294_L_295,
   INT64_(0)))goto LD5;
#line 398 "../Main.m3"
 /* set_source_line */
#line 398 "../Main.m3"
#line 400 "../Main.m3"
 /* start_call_direct */
#line 400 "../Main.m3"
 /* load_integer */
#line 400 "../Main.m3"
 /* pop_param */
#line 400 "../Main.m3"
 /* load_procedure */
#line 400 "../Main.m3"
 /* pop_param */
#line 400 "../Main.m3"
 /* invoke_direct */
#line 400 "../Main.m3"
 /* call_direct */
#line 400 "../Main.m3"
Main__Action(
  ( Main__ThreadNo )(((UINT8)( INT64_(4))) ),
  ( Main__ActionProc )(((ADDRESS)(Main__DoRel)) ));
#line 400 "../Main.m3"
 /* set_label */
#line 400 "../Main.m3"
 /* set_source_line */
#line 400 "../Main.m3"
#line 401 "../Main.m3"
 /* start_call_direct */
#line 401 "../Main.m3"
 /* load_integer */
#line 401 "../Main.m3"
 /* pop_param */
#line 401 "../Main.m3"
 /* load_integer */
#line 401 "../Main.m3"
 /* pop_param */
#line 401 "../Main.m3"
 /* invoke_direct */
#line 401 "../Main.m3"
 /* call_direct */
#line 401 "../Main.m3"
Main__WaitForState(
  ( Main__ThreadNo )(((UINT8)( INT64_(4))) ),
  ( Main__State )(((UINT8)( INT64_(1))) ));
#line 401 "../Main.m3"
 /* set_label */
#line 401 "../Main.m3"
 /* set_source_line */
#line 401 "../Main.m3"
#line 402 "../Main.m3"
 /* start_call_direct */
#line 402 "../Main.m3"
 /* load_address */
#line 402 "../Main.m3"
 /* pop_param */
#line 402 "../Main.m3"
 /* invoke_direct */
#line 402 "../Main.m3"
 /* call_direct */
#line 402 "../Main.m3"
Main__W(
  ( TEXT )(((ADDRESS)(INT64_(1440)+((ADDRESS)(&Main_m_47_L_48)))) ));
#line 402 "../Main.m3"
 /* set_label */
#line 402 "../Main.m3"
 /* jump */
#line 402 "../Main.m3"
goto LD4;
#line 402 "../Main.m3"
 /* set_label */
#line 402 "../Main.m3"
LD5:;
#line 402 "../Main.m3"
 /* set_source_line */
#line 402 "../Main.m3"
#line 404 "../Main.m3"
 /* start_call_direct */
#line 404 "../Main.m3"
 /* load_address */
#line 404 "../Main.m3"
 /* pop_param */
#line 404 "../Main.m3"
 /* invoke_direct */
#line 404 "../Main.m3"
 /* call_direct */
#line 404 "../Main.m3"
Main__W(
  ( TEXT )(((ADDRESS)(INT64_(1496)+((ADDRESS)(&Main_m_47_L_48)))) ));
#line 404 "../Main.m3"
 /* set_label */
#line 404 "../Main.m3"
 /* set_source_line */
#line 404 "../Main.m3"
#line 405 "../Main.m3"
 /* start_call_direct */
#line 405 "../Main.m3"
 /* load_address */
#line 405 "../Main.m3"
 /* pop_param */
#line 405 "../Main.m3"
 /* load_nil */
#line 405 "../Main.m3"
 /* pop_param */
#line 405 "../Main.m3"
 /* load_address */
#line 405 "../Main.m3"
 /* pop_param */
#line 405 "../Main.m3"
 /* load_integer */
#line 405 "../Main.m3"
 /* pop_param */
#line 405 "../Main.m3"
 /* invoke_direct */
#line 405 "../Main.m3"
 /* call_direct */
#line 405 "../Main.m3"
RTHooks__Raise(
  ( ADDRESS )(((ADDRESS)(&Main_m_47_L_48)) ),
  ( ADDRESS )(((ADDRESS)(0)) ),
  ( ADDRESS )(((ADDRESS)(&Main_m_M_Main_L_49)) ),
  ( INTEGER )(  INT64_(405) ));
#line 405 "../Main.m3"
 /* set_label */
#line 405 "../Main.m3"
 /* set_label */
#line 405 "../Main.m3"
LD4:;
#line 405 "../Main.m3"
 /* jump */
#line 405 "../Main.m3"
goto LAE;
#line 405 "../Main.m3"
 /* end_try */
#line 405 "../Main.m3"
} catch (_M3Exc& _m3exc) { _m3_caught = _m3exc.act; goto LAC; }
#line 405 "../Main.m3"
 /* set_label */
#line 405 "../Main.m3"
LAC:;
#line 405 "../Main.m3"
 /* set_source_line */
#line 405 "../Main.m3"
#line 408 "../Main.m3"
 /* landing_pad */
#line 408 "../Main.m3"
 /* store */
#line 408 "../Main.m3"
(*(ADDRESS*)(&Main_m_292_L_293))=(ADDRESS)(((ADDRESS)(_m3_caught)));
#line 408 "../Main.m3"
 /* set_label */
#line 408 "../Main.m3"
 /* load */
#line 408 "../Main.m3"
 /* load_indirect */
#line 408 "../Main.m3"
 /* load_indirect */
#line 408 "../Main.m3"
 /* load_integer */
#line 408 "../Main.m3"
 /* if_compare */
#line 408 "../Main.m3"
if(m3_ne(INT64,
  *((INT64*)(*((ADDRESS*)(Main_m_292_L_293)))),
   INT64_(-1505293580)))goto LDD;
#line 408 "../Main.m3"
 /* jump */
#line 408 "../Main.m3"
goto LAE;
#line 408 "../Main.m3"
 /* set_label */
#line 408 "../Main.m3"
LDD:;
#line 408 "../Main.m3"
 /* start_call_direct */
#line 408 "../Main.m3"
 /* load */
#line 408 "../Main.m3"
 /* pop_param */
#line 408 "../Main.m3"
 /* call_direct */
#line 408 "../Main.m3"
RTHooks__ResumeRaise(
  ( ADDRESS )(((ADDRESS)(Main_m_292_L_293)) ));
#line 408 "../Main.m3"
 /* set_label */
#line 408 "../Main.m3"
LAE:;
#line 408 "../Main.m3"
 /* set_source_line */
#line 408 "../Main.m3"
#line 410 "../Main.m3"
 /* exit_proc */
#line 410 "../Main.m3"
return;
#line 410 "../Main.m3"
 /* end_procedure */
#line 410 "../Main.m3"
} /* Init */
#line 410 "../Main.m3"
 /* set_source_line */
#line 410 "../Main.m3"
#line 412 "../Main.m3"
 /* begin_procedure */
#line 412 "../Main.m3"
struct Main__Init_Frame_t {
#line 412 "../Main.m3"
ADDRESS _unused;
#line 412 "../Main.m3"
};
#line 412 "../Main.m3"
void /*TypeText3*/ 
__cdecl
Main__Init(void)
{
#line 412 "../Main.m3"
 /* Var_Type2 */ ADDRESS Main_m_296_L_297={0};//always-init
#line 412 "../Main.m3"
 /* Var_Type1 */ INTEGER LThNo_L_298={0};//always-init
#line 412 "../Main.m3"
 /* Var_Type2 */ INT64 Main_m_299_L_300={0};//always-init
#line 412 "../Main.m3"
 /* Var_Type2 */ ADDRESS Main_m_301_L_302={0};//always-init
#line 412 "../Main.m3"
 /* Var_Type2 */ ADDRESS Main_m_303_L_304={0};//always-init
#line 412 "../Main.m3"
 /* Var_Type2 */ INT64 Main_m_305_L_306={0};//always-init
#line 412 "../Main.m3"
 /* Var_Type2 */ ADDRESS Main_m_307_L_308={0};//always-init
#line 412 "../Main.m3"
Main__Init_Frame_t _frame;
#line 412 "../Main.m3"
_frame._unused=(ADDRESS)&_frame;
#line 412 "../Main.m3"
 /* set_source_line */
#line 412 "../Main.m3"
#line 413 "../Main.m3"
 /* set_source_line */
#line 413 "../Main.m3"
#line 414 "../Main.m3"
 /* start_call_direct */
#line 414 "../Main.m3"
 /* load */
#line 414 "../Main.m3"
 /* pop_param */
#line 414 "../Main.m3"
 /* call_direct */
#line 414 "../Main.m3"
 /* store */
#line 414 "../Main.m3"
(*(ADDRESS*)(&Main_m_296_L_297))=(ADDRESS)(((ADDRESS)(RTHooks__AllocateTracedObj(
  ( ADDRESS )(((ADDRESS)(*((ADDRESS*)(INT64_(568)+((ADDRESS)(&Main_m_M_Main_L_49)))))) )))));
#line 414 "../Main.m3"
 /* load */
#line 414 "../Main.m3"
 /* store */
#line 414 "../Main.m3"
(*(ADDRESS*)((128)+(char*)(&Main_m_M_Main_L_49)))=(ADDRESS)(((ADDRESS)(Main_m_296_L_297)));
#line 414 "../Main.m3"
 /* set_source_line */
#line 414 "../Main.m3"
#line 415 "../Main.m3"
 /* start_call_direct */
#line 415 "../Main.m3"
 /* load */
#line 415 "../Main.m3"
 /* pop_param */
#line 415 "../Main.m3"
 /* call_direct */
#line 415 "../Main.m3"
 /* store */
#line 415 "../Main.m3"
(*(ADDRESS*)(&Main_m_296_L_297))=(ADDRESS)(((ADDRESS)(RTHooks__AllocateTracedObj(
  ( ADDRESS )(((ADDRESS)(*((ADDRESS*)(INT64_(568)+((ADDRESS)(&Main_m_M_Main_L_49)))))) )))));
#line 415 "../Main.m3"
 /* load */
#line 415 "../Main.m3"
 /* store */
#line 415 "../Main.m3"
(*(ADDRESS*)((104)+(char*)(&Main_m_M_Main_L_49)))=(ADDRESS)(((ADDRESS)(Main_m_296_L_297)));
#line 415 "../Main.m3"
 /* set_source_line */
#line 415 "../Main.m3"
#line 416 "../Main.m3"
 /* start_call_direct */
#line 416 "../Main.m3"
 /* load */
#line 416 "../Main.m3"
 /* pop_param */
#line 416 "../Main.m3"
 /* call_direct */
#line 416 "../Main.m3"
 /* store */
#line 416 "../Main.m3"
(*(ADDRESS*)(&Main_m_296_L_297))=(ADDRESS)(((ADDRESS)(RTHooks__AllocateTracedObj(
  ( ADDRESS )(((ADDRESS)(*((ADDRESS*)(INT64_(568)+((ADDRESS)(&Main_m_M_Main_L_49)))))) )))));
#line 416 "../Main.m3"
 /* load */
#line 416 "../Main.m3"
 /* store */
#line 416 "../Main.m3"
(*(ADDRESS*)((176)+(char*)(&Main_m_M_Main_L_49)))=(ADDRESS)(((ADDRESS)(Main_m_296_L_297)));
#line 416 "../Main.m3"
 /* set_source_line */
#line 416 "../Main.m3"
#line 417 "../Main.m3"
 /* start_call_direct */
#line 417 "../Main.m3"
 /* load */
#line 417 "../Main.m3"
 /* pop_param */
#line 417 "../Main.m3"
 /* call_direct */
#line 417 "../Main.m3"
 /* store */
#line 417 "../Main.m3"
(*(ADDRESS*)(&Main_m_296_L_297))=(ADDRESS)(((ADDRESS)(RTHooks__AllocateTracedObj(
  ( ADDRESS )(((ADDRESS)(*((ADDRESS*)(INT64_(584)+((ADDRESS)(&Main_m_M_Main_L_49)))))) )))));
#line 417 "../Main.m3"
 /* load */
#line 417 "../Main.m3"
 /* store */
#line 417 "../Main.m3"
(*(ADDRESS*)((184)+(char*)(&Main_m_M_Main_L_49)))=(ADDRESS)(((ADDRESS)(Main_m_296_L_297)));
#line 417 "../Main.m3"
 /* set_source_line */
#line 417 "../Main.m3"
#line 418 "../Main.m3"
 /* begin_block */
#line 418 "../Main.m3"
 /* load_integer */
#line 418 "../Main.m3"
 /* store */
#line 418 "../Main.m3"
(*(INT64*)(&LThNo_L_298))=(INT64)(  INT64_(0));
#line 418 "../Main.m3"
 /* set_label */
#line 418 "../Main.m3"
LDE:;
#line 418 "../Main.m3"
 /* set_source_line */
#line 418 "../Main.m3"
#line 420 "../Main.m3"
 /* start_call_direct */
#line 420 "../Main.m3"
 /* load */
#line 420 "../Main.m3"
 /* pop_param */
#line 420 "../Main.m3"
 /* call_direct */
#line 420 "../Main.m3"
 /* store */
#line 420 "../Main.m3"
(*(ADDRESS*)(&Main_m_296_L_297))=(ADDRESS)(((ADDRESS)(RTHooks__AllocateTracedObj(
  ( ADDRESS )(((ADDRESS)(*((ADDRESS*)(INT64_(600)+((ADDRESS)(&Main_m_M_Main_L_49)))))) )))));
#line 420 "../Main.m3"
 /* load */
#line 420 "../Main.m3"
 /* load */
#line 420 "../Main.m3"
 /* store_indirect */
#line 420 "../Main.m3"
(*(UINT8*)((8)+(char*)(Main_m_296_L_297)))=(INT64)( LThNo_L_298);
#line 420 "../Main.m3"
 /* load */
#line 420 "../Main.m3"
 /* store */
#line 420 "../Main.m3"
(*(INT64*)(&Main_m_299_L_300))=(INT64)( LThNo_L_298);
#line 420 "../Main.m3"
 /* load_address */
#line 420 "../Main.m3"
 /* load */
#line 420 "../Main.m3"
 /* index_address */
#line 420 "../Main.m3"
 /* store */
#line 420 "../Main.m3"
(*(ADDRESS*)(&Main_m_301_L_302))=(ADDRESS)(((ADDRESS)((((ADDRESS)(INT64_(384)+((ADDRESS)(&Main_m_M_Main_L_49))))+(8*( Main_m_299_L_300))))));
#line 420 "../Main.m3"
 /* load */
#line 420 "../Main.m3"
 /* load */
#line 420 "../Main.m3"
 /* store_indirect */
#line 420 "../Main.m3"
(*(ADDRESS*)(Main_m_301_L_302))=(ADDRESS)(((ADDRESS)(Main_m_296_L_297)));
#line 420 "../Main.m3"
 /* set_source_line */
#line 420 "../Main.m3"
#line 421 "../Main.m3"
 /* load */
#line 421 "../Main.m3"
 /* store */
#line 421 "../Main.m3"
(*(INT64*)(&Main_m_299_L_300))=(INT64)( LThNo_L_298);
#line 421 "../Main.m3"
 /* load_address */
#line 421 "../Main.m3"
 /* load */
#line 421 "../Main.m3"
 /* index_address */
#line 421 "../Main.m3"
 /* store */
#line 421 "../Main.m3"
(*(ADDRESS*)(&Main_m_301_L_302))=(ADDRESS)(((ADDRESS)((((ADDRESS)(INT64_(112)+((ADDRESS)(&Main_m_M_Main_L_49))))+( Main_m_299_L_300)))));
#line 421 "../Main.m3"
 /* load */
#line 421 "../Main.m3"
 /* load_integer */
#line 421 "../Main.m3"
 /* store_indirect */
#line 421 "../Main.m3"
(*(UINT8*)(Main_m_301_L_302))=(INT64)(  INT64_(1));
#line 421 "../Main.m3"
 /* set_source_line */
#line 421 "../Main.m3"
#line 422 "../Main.m3"
 /* load */
#line 422 "../Main.m3"
 /* store */
#line 422 "../Main.m3"
(*(INT64*)(&Main_m_299_L_300))=(INT64)( LThNo_L_298);
#line 422 "../Main.m3"
 /* load_address */
#line 422 "../Main.m3"
 /* load */
#line 422 "../Main.m3"
 /* index_address */
#line 422 "../Main.m3"
 /* store */
#line 422 "../Main.m3"
(*(ADDRESS*)(&Main_m_301_L_302))=(ADDRESS)(((ADDRESS)((((ADDRESS)(INT64_(384)+((ADDRESS)(&Main_m_M_Main_L_49))))+(8*( Main_m_299_L_300))))));
#line 422 "../Main.m3"
 /* load */
#line 422 "../Main.m3"
 /* load_indirect */
#line 422 "../Main.m3"
 /* store */
#line 422 "../Main.m3"
(*(ADDRESS*)(&Main_m_296_L_297))=(ADDRESS)(((ADDRESS)(*((ADDRESS*)(Main_m_301_L_302)))));
#line 422 "../Main.m3"
 /* load_nil */
#line 422 "../Main.m3"
 /* load */
#line 422 "../Main.m3"
 /* if_compare */
#line 422 "../Main.m3"
if(m3_eq(ADDRESS,
 ((ADDRESS)(0)),
 ((ADDRESS)(Main_m_296_L_297))))goto LE1;
#line 422 "../Main.m3"
 /* load */
#line 422 "../Main.m3"
 /* loophole */
#line 422 "../Main.m3"
 /* load_integer */
#line 422 "../Main.m3"
 /* and */
#line 422 "../Main.m3"
 /* if_true_or_false */
#line 422 "../Main.m3"
 /* load_host_integer */
#line 422 "../Main.m3"
 /* load_integer */
#line 422 "../Main.m3"
 /* if_compare */
#line 422 "../Main.m3"
if(m3_ne(INT64,
  ((INT64)(((INT64)((INT64)Main_m_296_L_297))&  INT64_(1))),
   INT64_(0)))goto LE1;
#line 422 "../Main.m3"
 /* load */
#line 422 "../Main.m3"
 /* load_indirect */
#line 422 "../Main.m3"
 /* extract_mn */
#line 422 "../Main.m3"
 /* load_host_integer */
#line 422 "../Main.m3"
 /* load_integer */
#line 422 "../Main.m3"
 /* load_host_integer */
#line 422 "../Main.m3"
 /* load_integer */
#line 422 "../Main.m3"
 /* extract */
#line 422 "../Main.m3"
 /* if_true_or_false */
#line 422 "../Main.m3"
 /* load_host_integer */
#line 422 "../Main.m3"
 /* load_integer */
#line 422 "../Main.m3"
 /* if_compare */
#line 422 "../Main.m3"
if(m3_eq(INT64,
 ((INT64)(m3_extract(
 UINT64,
  *((INT64*)(INT64_(-8)+((ADDRESS)(Main_m_296_L_297)))),
   UINT64_(22),
   UINT64_(1)))),
   INT64_(0)))goto LE1;
#line 422 "../Main.m3"
 /* start_call_direct */
#line 422 "../Main.m3"
 /* load */
#line 422 "../Main.m3"
 /* pop_param */
#line 422 "../Main.m3"
 /* call_direct */
#line 422 "../Main.m3"
RTHooks__CheckLoadTracedRef(
  ( REFANY )(((ADDRESS)(Main_m_296_L_297)) ));
#line 422 "../Main.m3"
 /* set_label */
#line 422 "../Main.m3"
LE1:;
#line 422 "../Main.m3"
 /* start_call_direct */
#line 422 "../Main.m3"
 /* load */
#line 422 "../Main.m3"
 /* pop_param */
#line 422 "../Main.m3"
 /* call_direct */
#line 422 "../Main.m3"
 /* store */
#line 422 "../Main.m3"
(*(ADDRESS*)(&Main_m_303_L_304))=(ADDRESS)(((ADDRESS)(Thread__Fork(
  ( Thread__Closure )(((ADDRESS)(Main_m_296_L_297)) )))));
#line 422 "../Main.m3"
 /* load */
#line 422 "../Main.m3"
 /* store */
#line 422 "../Main.m3"
(*(INT64*)(&Main_m_305_L_306))=(INT64)( LThNo_L_298);
#line 422 "../Main.m3"
 /* load_address */
#line 422 "../Main.m3"
 /* load */
#line 422 "../Main.m3"
 /* index_address */
#line 422 "../Main.m3"
 /* store */
#line 422 "../Main.m3"
(*(ADDRESS*)(&Main_m_307_L_308))=(ADDRESS)(((ADDRESS)((((ADDRESS)(INT64_(192)+((ADDRESS)(&Main_m_M_Main_L_49))))+(8*( Main_m_305_L_306))))));
#line 422 "../Main.m3"
 /* load */
#line 422 "../Main.m3"
 /* load */
#line 422 "../Main.m3"
 /* store_indirect */
#line 422 "../Main.m3"
(*(ADDRESS*)(Main_m_307_L_308))=(ADDRESS)(((ADDRESS)(Main_m_303_L_304)));
#line 422 "../Main.m3"
 /* set_source_line */
#line 422 "../Main.m3"
#line 418 "../Main.m3"
 /* load_integer */
#line 418 "../Main.m3"
 /* load */
#line 418 "../Main.m3"
 /* add */
#line 418 "../Main.m3"
 /* store */
#line 418 "../Main.m3"
(*(INT64*)(&LThNo_L_298))=(INT64)( ((INT64)(  INT64_(1)+ LThNo_L_298)));
#line 418 "../Main.m3"
 /* set_label */
#line 418 "../Main.m3"
 /* load_integer */
#line 418 "../Main.m3"
 /* load */
#line 418 "../Main.m3"
 /* if_compare */
#line 418 "../Main.m3"
if(m3_ge(INT64,
   INT64_(4),
  LThNo_L_298))goto LDE;
#line 418 "../Main.m3"
 /* set_label */
#line 418 "../Main.m3"
 /* end_block */
#line 418 "../Main.m3"
 /* set_source_line */
#line 418 "../Main.m3"
#line 424 "../Main.m3"
 /* exit_proc */
#line 424 "../Main.m3"
return;
#line 424 "../Main.m3"
 /* end_procedure */
#line 424 "../Main.m3"
} /* Main_M3 */
#line 424 "../Main.m3"
 /* module main body Main_M3 */
#line 424 "../Main.m3"
 /* set_source_line */
#line 424 "../Main.m3"
#line 426 "../Main.m3"
 /* begin_procedure */
#line 426 "../Main.m3"
struct Main_M3_Frame_t {
#line 426 "../Main.m3"
ADDRESS _unused;
#line 426 "../Main.m3"
};
#line 426 "../Main.m3"
RT0__ModulePtr
__cdecl
Main_M3(
   /* Param_Type1 */ INTEGER /*TypeText1*/  mode_L_50)
{
#line 426 "../Main.m3"
Main_M3_Frame_t _frame;
#line 426 "../Main.m3"
_frame._unused=(ADDRESS)&_frame;
#line 426 "../Main.m3"
 /* load */
#line 426 "../Main.m3"
 /* if_true_or_false */
#line 426 "../Main.m3"
 /* load_host_integer */
#line 426 "../Main.m3"
 /* load_integer */
#line 426 "../Main.m3"
 /* if_compare */
#line 426 "../Main.m3"
if(m3_eq(INT64,
  mode_L_50,
   INT64_(0)))goto LE2;
#line 426 "../Main.m3"
 /* set_source_line */
#line 426 "../Main.m3"
#line 427 "../Main.m3"
 /* start_call_direct */
#line 427 "../Main.m3"
 /* call_direct */
#line 427 "../Main.m3"
Main__Init(
 );
#line 427 "../Main.m3"
 /* set_source_line */
#line 427 "../Main.m3"
#line 428 "../Main.m3"
 /* start_call_direct */
#line 428 "../Main.m3"
 /* call_direct */
#line 428 "../Main.m3"
Main__TestSeq(
 );
#line 428 "../Main.m3"
 /* set_label */
#line 428 "../Main.m3"
LE2:;
#line 428 "../Main.m3"
 /* load_address */
#line 428 "../Main.m3"
 /* exit_proc */
#line 428 "../Main.m3"
return (RT0__ModulePtr)(&Main_m_M_Main_L_49);
#line 428 "../Main.m3"
 /* end_procedure */
#line 428 "../Main.m3"
} /* global constant type descriptor */
#line 428 "../Main.m3"
 /* global data type descriptor */
#line 428 "../Main.m3"
 /* module global constants */
#line 428 "../Main.m3"
 /* procedure names */
#line 428 "../Main.m3"
 /* procedure table */
#line 428 "../Main.m3"
 /* global type map */
#line 428 "../Main.m3"
 /* file name */
#line 428 "../Main.m3"
 /* type map for _t73ae990f */
#line 428 "../Main.m3"
 /* type description for _t73ae990f */
#line 428 "../Main.m3"
 /* module global data */
#line 428 "../Main.m3"
 /* typecell for _t73ae990f */
#line 428 "../Main.m3"
 /* load map


 global data allocation for M_Main
     0   104  8  *module info*
   104     8  8  Main.StateMutex
   112     5  1  Main.States
   117     1  1  Main.Holder
   120     8  8  Main.Failure
   128     8  8  Main.ActionMutex
   136    40  8  Main.ActionProcs
   176     8  8  Main.TestMutex
   184     8  8  Main.TestCond
   192    40  8  Main.Threads
   232   152  8  typecell
   384    40  8  Main.Closures
   424    24  8  import Main
   448    24  8  import Wr
   472    24  8  import Thread
   496    24  8  import Stdio
   520    24  8  import Fmt
   544    24  8  import RTHooks
   568    16  8  typecell ptr
   584    16  8  typecell ptr
   600    16  8  typecell ptr
   616     0  8  *TOTAL*


 global constants for M_Main
     0    37  8  Main.Failure
    40    40  8  TEXT literal methods
    80    26  8  *TEXT literal*
   112    32  8  *TEXT literal*
   144    29  8  *TEXT literal*
   176    29  8  *TEXT literal*
   208    41  8  *TEXT literal*
   256    41  8  *TEXT literal*
   304    38  8  *TEXT literal*
   344    39  8  *TEXT literal*
   384    40  8  *TEXT literal*
   424    36  8  *TEXT literal*
   464    49  8  *TEXT literal*
   520    38  8  *TEXT literal*
   560    34  8  *TEXT literal*
   600    29  8  *TEXT literal*
   632    56  8  *TEXT literal*
   688    45  8  *TEXT literal*
   736    44  8  *TEXT literal*
   784    47  8  *TEXT literal*
   832    56  8  *TEXT literal*
   888    45  8  *TEXT literal*
   936    44  8  *TEXT literal*
   984    45  8  *TEXT literal*
  1032    52  8  *TEXT literal*
  1088    77  8  *TEXT literal*
  1168    46  8  *TEXT literal*
  1216    53  8  *TEXT literal*
  1272    45  8  *TEXT literal*
  1320    25  8  *TEXT literal*
  1352    36  8  *TEXT literal*
  1392    35  8  *TEXT literal*
  1432    50  8  *TEXT literal*
  1488    89  8  *TEXT literal*
  1584   191  8  *proc names*
  1776   312  8  *proc info*
  2088    21  1  type_map
  2109    11  1  *string*
  2120     2  1  type_map
  2122     6  1  type_desc
  2128     8  8  method list
  2136     8  1  *string*
  2144     0  8  *TOTAL*
 */
#line 428 "../Main.m3"
 /* end unit */
#line 428 "../Main.m3"

#ifdef __cplusplus

} /* extern "C" */
#endif
 /* set_runtime_proc */
 /* set_runtime_proc */
