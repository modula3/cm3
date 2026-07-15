// library:pgm
// source_base_name:HighlightVBT
// target_name:HighlightVBT.m3.cpp
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
 /* declare_record */
 /* declare_field */
 /* record_forwardDeclare Record_t{ typeid:TFCD63CAE text:NIL hash_text:TFCD63CAE base_text:NIL state:0} */
/*record_forwardDeclare*/struct TFCD63CAE;typedef struct TFCD63CAE TFCD63CAE;
 /* record_canBeDefined Record_t{ typeid:TFCD63CAE text:NIL hash_text:TFCD63CAE base_text:NIL state:0} */
 /* record_define Record_t{ typeid:TFCD63CAE text:NIL hash_text:TFCD63CAE base_text:NIL state:0} */

#ifndef TFCD63CAE
#define TFCD63CAE TFCD63CAE
/*record_define*/struct TFCD63CAE{
INTEGER PaintOp_T_field;
};
#endif
 /* declare_pointer */
typedef TFCD63CAE*TC87182A6;
 /* declare_indirect */
typedef TFCD63CAE*T329C351;
 /* declare_record */
 /* declare_field */
 /* record_forwardDeclare Record_t{ typeid:T5CCFFB05 text:NIL hash_text:T5CCFFB05 base_text:NIL state:0} */
/*record_forwardDeclare*/struct T5CCFFB05;typedef struct T5CCFFB05 T5CCFFB05;
 /* record_canBeDefined Record_t{ typeid:T5CCFFB05 text:NIL hash_text:T5CCFFB05 base_text:NIL state:0} */
 /* record_define Record_t{ typeid:T5CCFFB05 text:NIL hash_text:T5CCFFB05 base_text:NIL state:0} */

#ifndef T5CCFFB05
#define T5CCFFB05 T5CCFFB05
/*record_define*/struct T5CCFFB05{
INTEGER Pixmap_T_field;
};
#endif
 /* declare_pointer */
typedef T5CCFFB05*T7F433FF9;
 /* declare_indirect */
typedef T5CCFFB05*TA33004FA;
 /* declare_record */
 /* declare_field */
 /* record_forwardDeclare Record_t{ typeid:TE99B66B4 text:NIL hash_text:TE99B66B4 base_text:NIL state:0} */
/*record_forwardDeclare*/struct TE99B66B4;typedef struct TE99B66B4 TE99B66B4;
 /* record_canBeDefined Record_t{ typeid:TE99B66B4 text:NIL hash_text:TE99B66B4 base_text:NIL state:0} */
 /* record_define Record_t{ typeid:TE99B66B4 text:NIL hash_text:TE99B66B4 base_text:NIL state:0} */

#ifndef TE99B66B4
#define TE99B66B4 TE99B66B4
/*record_define*/struct TE99B66B4{
INTEGER Point_T_field;
};
#endif
 /* declare_pointer */
typedef TE99B66B4*TB0595BE;
 /* declare_indirect */
typedef TE99B66B4*T1664994B;
 /* declare_subrange */
/*subrange_define*/typedef UINT8 T8A2831D7_8;
 /* declare_array */
/*array_forwardDeclare*/struct T4F238AAE;typedef struct T4F238AAE T4F238AAE;

#ifndef T4F238AAE
#define T4F238AAE T4F238AAE
/*fixedArray_define*/struct T4F238AAE{INTEGER _elts[1];};
#endif
 /* declare_pointer */
typedef T4F238AAE*TB628F43C;
 /* declare_indirect */
typedef T4F238AAE*TB0DC7551;
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
typedef TF400F3DB*TBFF0C24;
 /* declare_subrange */
/*subrange_define*/typedef UINT16 T4A3373B8_16;
 /* declare_set */
 /* declare_array */
/*array_forwardDeclare*/struct T67A7B112;typedef struct T67A7B112 T67A7B112;

#ifndef T67A7B112
#define T67A7B112 T67A7B112
/*fixedArray_define*/struct T67A7B112{WORD_T _elts[17];};
#endif
 /* declare_pointer */
typedef T67A7B112*T4797DFDB;
 /* declare_indirect */
typedef T67A7B112*T98584EED;
 /* declare_subrange */
/*subrange_define*/typedef UINT8 T2CA4581D_8;
 /* declare_set */

#ifndef TDDB62BB7
#define TDDB62BB7 TDDB62BB7
/*type_typedef*/typedef UINT16 TDDB62BB7;

#endif
 /* declare_pointer */
typedef TDDB62BB7*TED1CF615;
 /* declare_indirect */
typedef TDDB62BB7*T2249D448;
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
 /* declare_formal */
 /* declare_formal */
 /* declare_object */
 /* record_forwardDeclare Record_t{ typeid:TFFFFFFFF text:T341F663F_fields hash_text:NIL base_text:NIL state:0} */
/*record_forwardDeclare*/struct T341F663F_fields;typedef struct T341F663F_fields T341F663F_fields;
 /* record_canBeDefined Record_t{ typeid:TFFFFFFFF text:T341F663F_fields hash_text:NIL base_text:NIL state:0} */
 /* record_define Record_t{ typeid:TFFFFFFFF text:T341F663F_fields hash_text:NIL base_text:NIL state:0} */

#ifndef T341F663F_fields
#define T341F663F_fields T341F663F_fields
/*record_define*/struct T341F663F_fields{
UINT8 L_0[8];
};
#endif
typedef T341F663F_fields*T341F663F;
 /* declare_method */
 /* declare_object */
 /* record_forwardDeclare Record_t{ typeid:TFFFFFFFF text:T2A7F2412_fields hash_text:NIL base_text:NIL state:0} */
/*record_forwardDeclare*/struct T2A7F2412_fields;typedef struct T2A7F2412_fields T2A7F2412_fields;
 /* record_canBeDefined Record_t{ typeid:TFFFFFFFF text:T2A7F2412_fields hash_text:NIL base_text:NIL state:0} */
 /* record_define Record_t{ typeid:TFFFFFFFF text:T2A7F2412_fields hash_text:NIL base_text:NIL state:0} */

#ifndef T2A7F2412_fields
#define T2A7F2412_fields T2A7F2412_fields
/*record_define*/struct T2A7F2412_fields{
UINT8 L_1[8];
};
#endif
typedef T2A7F2412_fields*T2A7F2412;
 /* declare_opaque */

#ifndef TF81917DF
#define TF81917DF TF81917DF
/*1addressType_define*/typedef ADDRESS TF81917DF;

#endif
 /* declare_proctype */
 /* declare_formal */

#ifndef HighlightVBT__HighlightVBT_T
#define HighlightVBT__HighlightVBT_T HighlightVBT__HighlightVBT_T
typedef TF81917DF HighlightVBT__HighlightVBT_T;
#endif
 /* declare_formal */

#ifndef VBT__PaintOp_T
#define VBT__PaintOp_T VBT__PaintOp_T
typedef TFCD63CAE VBT__PaintOp_T;
#endif
 /* declare_formal */
 /* declare_formal */

#ifndef VBT__Pixmap_T
#define VBT__Pixmap_T VBT__Pixmap_T
typedef T5CCFFB05 VBT__Pixmap_T;
#endif
 /* declare_formal */
 /* declare_formal */

#ifndef VBT__Point_T
#define VBT__Point_T VBT__Point_T
typedef TE99B66B4 VBT__Point_T;
#endif
 /* declare_formal */
 /* declare_formal */

#ifndef VBT__FixedArray
#define VBT__FixedArray VBT__FixedArray
typedef T4F238AAE VBT__FixedArray;
#endif
 /* declare_formal */
 /* declare_formal */
 /* declare_formal */
 /* declare_formal */

#ifndef VBT__BigSet
#define VBT__BigSet VBT__BigSet
typedef T67A7B112 VBT__BigSet;
#endif
 /* declare_formal */
 /* declare_formal */

#ifndef VBT__SmallSet
#define VBT__SmallSet VBT__SmallSet
typedef TDDB62BB7 VBT__SmallSet;
#endif
 /* declare_formal */
 /* declare_proctype */
 /* declare_formal */
 /* declare_formal */
 /* declare_record */
 /* declare_record */
 /* DeclareTypes_FlushOnce size:3 */

#if 0 /* avoid type hash collions */
typedef 
void(__cdecl*T87ECE3F8)(TFCD63CAE,TFCD63CAE*,T5CCFFB05,T5CCFFB05*,TE99B66B4,TE99B66B4*,T4F238AAE,T4F238AAE*,TF400F3DB*,TF400F3DB*,T67A7B112,T67A7B112*,TDDB62BB7,TDDB62BB7*);
#else
typedef void (__cdecl*T87ECE3F8)(void);
#endif

#if 0 /* avoid type hash collions */
typedef 
void(__cdecl*T4C1CAFCA)(HighlightVBT__HighlightVBT_T,VBT__PaintOp_T,TFCD63CAE*,VBT__Pixmap_T,T5CCFFB05*,VBT__Point_T,TE99B66B4*,VBT__FixedArray,T4F238AAE*,TF400F3DB*,TF400F3DB*,VBT__BigSet,T67A7B112*,VBT__SmallSet,TDDB62BB7*);
#else
typedef void (__cdecl*T4C1CAFCA)(void);
#endif

#if 0 /* avoid type hash collions */
typedef 
REFANY(__cdecl*T983B02E7)(ADDRESS,TF400F3DB*);
#else
typedef void (__cdecl*T983B02E7)(void);
#endif
 /* DeclareTypes_FlushOnce size:0 */
 /* end: DeclareTypes */
 /* begin: helper functions */
 /* end: helper functions */

#ifndef struct_8_t
#define struct_8_t struct_8_t
STRUCT8(8)
#endif

#ifndef struct_16_t
#define struct_16_t struct_16_t
STRUCT8(16)
#endif

#ifndef struct_136_t
#define struct_136_t struct_136_t
STRUCT8(136)
#endif
 /* begin: imports */
 /* import_procedure */

#ifndef RT0__ModulePtr
#define RT0__ModulePtr RT0__ModulePtr
typedef ADDRESS /*TypeText3*/  RT0__ModulePtr;
#endif
/*Proc_ForwardDeclareFrameType*/struct HighlightVBT_I3_Frame_t;typedef struct HighlightVBT_I3_Frame_t HighlightVBT_I3_Frame_t;
 /* internal_declare_param */
RT0__ModulePtr
__cdecl
HighlightVBT_I3(
   /* Param_Type1 */ INTEGER /*TypeText1*/  mode_L_2);
 /* import_procedure */
/*Proc_ForwardDeclareFrameType*/struct VBT_I3_Frame_t;typedef struct VBT_I3_Frame_t VBT_I3_Frame_t;
 /* internal_declare_param */
RT0__ModulePtr
__cdecl
VBT_I3(
   /* Param_Type1 */ INTEGER /*TypeText1*/  mode_L_3);
 /* import_procedure */
/*Proc_ForwardDeclareFrameType*/struct RTHooks_I3_Frame_t;typedef struct RTHooks_I3_Frame_t RTHooks_I3_Frame_t;
 /* internal_declare_param */
RT0__ModulePtr
__cdecl
RTHooks_I3(
   /* Param_Type1 */ INTEGER /*TypeText1*/  mode_L_4);
 /* import_procedure */
/*Proc_ForwardDeclareFrameType*/struct RTHooks__AllocateOpenArray_Frame_t;typedef struct RTHooks__AllocateOpenArray_Frame_t RTHooks__AllocateOpenArray_Frame_t;
 /* internal_declare_param */
 /* internal_declare_param */
REFANY
__cdecl
RTHooks__AllocateOpenArray(
   /* Param_Type1 */ ADDRESS t_L_5,
   /* Param_Type1 */ TF400F3DB* /*TypeText1*/  sizes_L_6);
 /* end: imports */
 /* begin: locals */
 /* declare_segment name:<NIL> typeid:TFFFFFFFF const:TRUE */
/*declare_segment*/struct HighlightVBT_m_7_L_8_t;
/*declare_segment*/typedef struct HighlightVBT_m_7_L_8_t HighlightVBT_m_7_L_8_t;
 /* declare_segment name:M_HighlightVBT typeid:TFFFFFFFF const:FALSE */
 /* handler_name_prefixes:HighlightVBT_M3_LINE_ */
 /* handler_name_prefixes:HighlightVBT_I3_LINE_ */
/*declare_segment*/struct HighlightVBT_m_M_HighlightVBT_L_9_t;
/*declare_segment*/typedef struct HighlightVBT_m_M_HighlightVBT_L_9_t HighlightVBT_m_M_HighlightVBT_L_9_t;
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct HighlightVBT_M3_Frame_t;typedef struct HighlightVBT_M3_Frame_t HighlightVBT_M3_Frame_t;
 /* internal_declare_param */
RT0__ModulePtr
__cdecl
HighlightVBT_M3(
   /* Param_Type1 */ INTEGER /*TypeText1*/  mode_L_10);
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct HighlightVBT__Be_Frame_t;typedef struct HighlightVBT__Be_Frame_t HighlightVBT__Be_Frame_t;
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
 /* internal_declare_param */
 /* internal_declare_param */
void /*TypeText3*/ 
__cdecl
HighlightVBT__Be(
   /* Param_Type1 */ HighlightVBT__HighlightVBT_T v_L_11,
   /* Param_Type1 */ VBT__PaintOp_T*_param_struct_pointer_paintOpValue_L_12,
   /* Param_Type1 */ TFCD63CAE* /*TypeText1*/  paintOpReadOnly_L_13,
   /* Param_Type1 */ VBT__Pixmap_T*_param_struct_pointer_pixmapValue_L_14,
   /* Param_Type1 */ T5CCFFB05* /*TypeText1*/  pixmapReadOnly_L_15,
   /* Param_Type1 */ VBT__Point_T*_param_struct_pointer_recordValue_L_16,
   /* Param_Type1 */ TE99B66B4* /*TypeText1*/  recordReadOnly_L_17,
   /* Param_Type1 */ VBT__FixedArray*_param_struct_pointer_fixedArrayValue_L_18,
   /* Param_Type1 */ T4F238AAE* /*TypeText1*/  fixedArrayReadOnly_L_19,
   /* Param_Type1 */ TF400F3DB* /*TypeText1*/  openArrayValue_L_20,
   /* Param_Type1 */ TF400F3DB* /*TypeText1*/  openArrayReadOnly_L_21,
   /* Param_Type1 */ VBT__BigSet*_param_struct_pointer_bigSetValue_L_22,
   /* Param_Type1 */ T67A7B112* /*TypeText1*/  bigSetReadOnly_L_23,
   /* Param_Type1 */ VBT__SmallSet smallSetValue_L_24,
   /* Param_Type1 */ TDDB62BB7* /*TypeText1*/  smallSetReadOnly_L_25);
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
 /* end: locals */
 /* begin: segments/globals */
 /* bind_segment */
 /* begin_init */
 /* init_int */
 /* init_int */
 /* init_chars */
 /* init_chars */
 /* init_proc */
 /* init_var */
 /* init_proc */
 /* init_var */
 /* init_chars */
 /* init_int */
 /* init_chars */
 /* init_int */
 /* init_int */
 /* init_int */
 /* init_proc */
 /* init_chars */
 /* end_init */
struct HighlightVBT_m_7_L_8_t{INT64 L_30[2];
char L_31[8];
UINT8 L_32[15];
char L_33[1];
UINT8 L_34[2];
char L_35[6];
ADDRESS L_36[4];
char L_37[8];
UINT8 L_38[18];
char L_39[6];
INT64 L_40[1];
UINT8 L_41[34];
char L_42[1];
INT8 L_43[3];
char L_44[2];
ADDRESS L_45[1];
UINT8 L_46[27];
char L_47[13];
};
static  const HighlightVBT_m_7_L_8_t HighlightVBT_m_7_L_8={{INT64_(-132573217),INT64_(712975378)},{0 /* 1 */ ,0 /* 2 */ ,0 /* 3 */ ,0 /* 4 */ ,0 /* 5 */ ,0 /* 6 */ ,0 /* 7 */ ,0 /* 8 */ ,},{'H','i','g','h','l','i','g','h','t','V','B','T','_','M','3'},{0 /* 1 */ ,},{'B','e'},{0 /* 1 */ ,0 /* 2 */ ,0 /* 3 */ ,0 /* 4 */ ,0 /* 5 */ ,0 /* 6 */ ,},{(ADDRESS)&HighlightVBT_M3,24+(char*)&HighlightVBT_m_7_L_8,(ADDRESS)&HighlightVBT__Be,40+(char*)&HighlightVBT_m_7_L_8},{0 /* 1 */ ,0 /* 2 */ ,0 /* 3 */ ,0 /* 4 */ ,0 /* 5 */ ,0 /* 6 */ ,0 /* 7 */ ,0 /* 8 */ ,},{'.','.','/','H','i','g','h','l','i','g','h','t','V','B','T','.','m','3'},{0 /* 1 */ ,0 /* 2 */ ,0 /* 3 */ ,0 /* 4 */ ,0 /* 5 */ ,0 /* 6 */ ,},{INT64_(34)},{'H','i','g','h','l','i','g','h','t','V','B','T',' ','#',' ','A','u','T','o','-','B','r','A','n','D',' ','#',' ','_','0','0','0','0','M'},{0 /* 1 */ ,},{((INT8)1),((INT8)12),((INT8)0)},{0 /* 1 */ ,0 /* 2 */ ,},{(ADDRESS)&HighlightVBT__Be},{'H','i','g','h','l','i','g','h','t','V','B','T','.'
,'H','i','g','h','l','i','g','h','t','V','B','T','_','T'},{0 /* 1 */ ,0 /* 2 */ ,0 /* 3 */ ,0 /* 4 */ ,0 /* 5 */ ,0 /* 6 */ ,0 /* 7 */ ,0 /* 8 */ ,0 /* 9 */ ,0 /* 10 */ ,0 /* 11 */ ,0 /* 12 */ ,0 /* 13 */ ,}};
 /* bind_segment */
 /* begin_init */
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
 /* init_int */
 /* init_int */
 /* init_var */
 /* init_proc */
 /* init_var */
 /* init_proc */
 /* init_var */
 /* init_proc */
 /* init_int */
 /* end_init */
struct HighlightVBT_m_M_HighlightVBT_L_9_t{ADDRESS L_48[4];
char L_49[8];
ADDRESS L_50[1];
char L_51[24];
ADDRESS L_52[1];
char L_53[8];
ADDRESS L_54[1];
INT64 L_55[1];
char L_56[8];
INT64 L_57[1];
UINT8 L_58[1];
INT8 L_59[3];
UINT8 L_60[1];
INT8 L_61[5];
char L_62[1];
INT8 L_63[1];
char L_64[4];
INT64 L_65[1];
char L_66[16];
ADDRESS L_67[1];
char L_68[8];
ADDRESS L_69[2];
char L_70[8];
INT64 L_71[1];
char L_72[24];
INT64 L_73[1];
ADDRESS L_74[1];
char L_75[16];
ADDRESS L_76[2];
char L_77[8];
ADDRESS L_78[2];
char L_79[8];
ADDRESS L_80[1];
char L_81[16];
INT64 L_82[1];
char L_83[8];
};
static HighlightVBT_m_M_HighlightVBT_L_9_t HighlightVBT_m_M_HighlightVBT_L_9={{88+(char*)&HighlightVBT_m_7_L_8,104+(char*)&HighlightVBT_m_M_HighlightVBT_L_9,328+(char*)&HighlightVBT_m_M_HighlightVBT_L_9,(char*)&HighlightVBT_m_7_L_8},{0 /* 1 */ ,0 /* 2 */ ,0 /* 3 */ ,0 /* 4 */ ,0 /* 5 */ ,0 /* 6 */ ,0 /* 7 */ ,0 /* 8 */ ,},{48+(char*)&HighlightVBT_m_7_L_8},{0 /* 1 */ ,0 /* 2 */ ,0 /* 3 */ ,0 /* 4 */ ,0 /* 5 */ ,0 /* 6 */ ,0 /* 7 */ ,0 /* 8 */ ,0 /* 9 */ ,0 /* 10 */ ,0 /* 11 */ ,0 /* 12 */ ,0 /* 13 */ ,0 /* 14 */ ,0 /* 15 */ ,0 /* 16 */ ,0 /* 17 */ ,0 /* 18 */ ,0 /* 19 */ ,0 /* 20 */ ,0 /* 21 */ ,0 /* 22 */ ,0 /* 23 */ ,0 /* 24 */ ,},{256+(char*)&HighlightVBT_m_M_HighlightVBT_L_9},{0 /* 1 */ ,0 /* 2 */ ,0 /* 3 */ ,0 /* 4 */ ,0 /* 5 */ ,0 /* 6 */ ,0 /* 7 */ ,0 /* 8 */ ,},{(ADDRESS)&HighlightVBT_M3},{INT64_(3)},{0 /* 1 */ ,0 /* 2 */ ,0 /* 3 */ ,0 /* 4 */ ,0 /* 5 */ ,0 /* 6 */ ,0 /* 7 */ ,0 /* 8 */ ,},{INT64_(712975378)},{190U},{((INT8)69),((INT8)79),((INT8)63)},{172U},{((INT8)97),((INT8)48)
,((INT8)21),((INT8)1),((INT8)2)},{0 /* 1 */ ,},{((INT8)8)},{0 /* 1 */ ,0 /* 2 */ ,0 /* 3 */ ,0 /* 4 */ ,},{INT64_(0)},{0 /* 1 */ ,0 /* 2 */ ,0 /* 3 */ ,0 /* 4 */ ,0 /* 5 */ ,0 /* 6 */ ,0 /* 7 */ ,0 /* 8 */ ,0 /* 9 */ ,0 /* 10 */ ,0 /* 11 */ ,0 /* 12 */ ,0 /* 13 */ ,0 /* 14 */ ,0 /* 15 */ ,0 /* 16 */ ,},{155+(char*)&HighlightVBT_m_7_L_8},{0 /* 1 */ ,0 /* 2 */ ,0 /* 3 */ ,0 /* 4 */ ,0 /* 5 */ ,0 /* 6 */ ,0 /* 7 */ ,0 /* 8 */ ,},{112+(char*)&HighlightVBT_m_7_L_8,168+(char*)&HighlightVBT_m_7_L_8},{0 /* 1 */ ,0 /* 2 */ ,0 /* 3 */ ,0 /* 4 */ ,0 /* 5 */ ,0 /* 6 */ ,0 /* 7 */ ,0 /* 8 */ ,},{INT64_(874473023)},{0 /* 1 */ ,0 /* 2 */ ,0 /* 3 */ ,0 /* 4 */ ,0 /* 5 */ ,0 /* 6 */ ,0 /* 7 */ ,0 /* 8 */ ,0 /* 9 */ ,0 /* 10 */ ,0 /* 11 */ ,0 /* 12 */ ,0 /* 13 */ ,0 /* 14 */ ,0 /* 15 */ ,0 /* 16 */ ,0 /* 17 */ ,0 /* 18 */ ,0 /* 19 */ ,0 /* 20 */ ,0 /* 21 */ ,0 /* 22 */ ,0 /* 23 */ ,0 /* 24 */ ,},{INT64_(0)},{160+(char*)&HighlightVBT_m_7_L_8},{0 /* 1 */ ,0 /* 2 */ ,0 /* 3 */ ,0 /* 4 */ ,0 /* 5 */ ,0 /* 6 */ 
,0 /* 7 */ ,0 /* 8 */ ,0 /* 9 */ ,0 /* 10 */ ,0 /* 11 */ ,0 /* 12 */ ,0 /* 13 */ ,0 /* 14 */ ,0 /* 15 */ ,0 /* 16 */ ,},{(ADDRESS)&HighlightVBT_I3,280+(char*)&HighlightVBT_m_M_HighlightVBT_L_9},{0 /* 1 */ ,0 /* 2 */ ,0 /* 3 */ ,0 /* 4 */ ,0 /* 5 */ ,0 /* 6 */ ,0 /* 7 */ ,0 /* 8 */ ,},{(ADDRESS)&VBT_I3,304+(char*)&HighlightVBT_m_M_HighlightVBT_L_9},{0 /* 1 */ ,0 /* 2 */ ,0 /* 3 */ ,0 /* 4 */ ,0 /* 5 */ ,0 /* 6 */ ,0 /* 7 */ ,0 /* 8 */ ,},{(ADDRESS)&RTHooks_I3},{0 /* 1 */ ,0 /* 2 */ ,0 /* 3 */ ,0 /* 4 */ ,0 /* 5 */ ,0 /* 6 */ ,0 /* 7 */ ,0 /* 8 */ ,0 /* 9 */ ,0 /* 10 */ ,0 /* 11 */ ,0 /* 12 */ ,0 /* 13 */ ,0 /* 14 */ ,0 /* 15 */ ,0 /* 16 */ ,},{INT64_(562018850)},{0 /* 1 */ ,0 /* 2 */ ,0 /* 3 */ ,0 /* 4 */ ,0 /* 5 */ ,0 /* 6 */ ,0 /* 7 */ ,0 /* 8 */ ,}};
 /* end: segments/globals */
 /* begin: mark used */
 /* end: mark used */
 /* set_source_file */
 /* set_source_line */
#line 7 "../HighlightVBT.m3"
 /* module global constants */
#line 7 "../HighlightVBT.m3"
 /* module global data */
#line 7 "../HighlightVBT.m3"
 /* set_source_line */
#line 7 "../HighlightVBT.m3"
#line 33 "../HighlightVBT.m3"
 /* Be */
#line 33 "../HighlightVBT.m3"
 /* set_source_line */
#line 33 "../HighlightVBT.m3"
#line 13 "../HighlightVBT.m3"
 /* begin_procedure */
#line 13 "../HighlightVBT.m3"
struct HighlightVBT__Be_Frame_t {
#line 13 "../HighlightVBT.m3"
ADDRESS _unused;
#line 13 "../HighlightVBT.m3"
};
#line 13 "../HighlightVBT.m3"
void /*TypeText3*/ 
__cdecl
HighlightVBT__Be(
   /* Param_Type1 */ HighlightVBT__HighlightVBT_T v_L_11,
   /* Param_Type1 */ VBT__PaintOp_T*_param_struct_pointer_paintOpValue_L_12,
   /* Param_Type1 */ TFCD63CAE* /*TypeText1*/  paintOpReadOnly_L_13,
   /* Param_Type1 */ VBT__Pixmap_T*_param_struct_pointer_pixmapValue_L_14,
   /* Param_Type1 */ T5CCFFB05* /*TypeText1*/  pixmapReadOnly_L_15,
   /* Param_Type1 */ VBT__Point_T*_param_struct_pointer_recordValue_L_16,
   /* Param_Type1 */ TE99B66B4* /*TypeText1*/  recordReadOnly_L_17,
   /* Param_Type1 */ VBT__FixedArray*_param_struct_pointer_fixedArrayValue_L_18,
   /* Param_Type1 */ T4F238AAE* /*TypeText1*/  fixedArrayReadOnly_L_19,
   /* Param_Type1 */ TF400F3DB* /*TypeText1*/  openArrayValue_L_20,
   /* Param_Type1 */ TF400F3DB* /*TypeText1*/  openArrayReadOnly_L_21,
   /* Param_Type1 */ VBT__BigSet*_param_struct_pointer_bigSetValue_L_22,
   /* Param_Type1 */ T67A7B112* /*TypeText1*/  bigSetReadOnly_L_23,
   /* Param_Type1 */ VBT__SmallSet smallSetValue_L_24,
   /* Param_Type1 */ TDDB62BB7* /*TypeText1*/  smallSetReadOnly_L_25)
{
#line 13 "../HighlightVBT.m3"
 /* Var_Type3 */ STRUCT(16) HighlightVBT_m_26_L_27={0};//always-init
#line 13 "../HighlightVBT.m3"
 /* Var_Type2 */ ADDRESS HighlightVBT_m_28_L_29={0};//always-init
#line 13 "../HighlightVBT.m3"
HighlightVBT__Be_Frame_t _frame;
#line 13 "../HighlightVBT.m3"
_frame._unused=(ADDRESS)&_frame;
#line 13 "../HighlightVBT.m3"
 /* set_source_line */
#line 13 "../HighlightVBT.m3"
#line 23 "../HighlightVBT.m3"
 /* load */
#line 23 "../HighlightVBT.m3"
 /* add_offset */
#line 23 "../HighlightVBT.m3"
 /* store */
#line 23 "../HighlightVBT.m3"
(*(ADDRESS*)(&HighlightVBT_m_26_L_27))=(ADDRESS)(((ADDRESS)(((8)+(char*)(((ADDRESS)(openArrayValue_L_20)))))));
#line 23 "../HighlightVBT.m3"
 /* load_integer */
#line 23 "../HighlightVBT.m3"
 /* store */
#line 23 "../HighlightVBT.m3"
(*(INT64*)((8)+(char*)(&HighlightVBT_m_26_L_27)))=(INT64)(  INT64_(1));
#line 23 "../HighlightVBT.m3"
 /* start_call_direct */
#line 23 "../HighlightVBT.m3"
 /* load */
#line 23 "../HighlightVBT.m3"
 /* pop_param */
#line 23 "../HighlightVBT.m3"
 /* load_address */
#line 23 "../HighlightVBT.m3"
 /* pop_param */
#line 23 "../HighlightVBT.m3"
 /* call_direct */
#line 23 "../HighlightVBT.m3"
 /* store */
#line 23 "../HighlightVBT.m3"
(*(ADDRESS*)(&HighlightVBT_m_28_L_29))=(ADDRESS)(((ADDRESS)(RTHooks__AllocateOpenArray(
  ( ADDRESS )(((ADDRESS)(*((ADDRESS*)(INT64_(328)+((ADDRESS)(&HighlightVBT_m_M_HighlightVBT_L_9)))))) ),
  ( TF400F3DB* /*TypeText1*/  )(((ADDRESS)(&HighlightVBT_m_26_L_27)) )))));
#line 23 "../HighlightVBT.m3"
 /* load */
#line 23 "../HighlightVBT.m3"
 /* load_indirect */
#line 23 "../HighlightVBT.m3"
 /* load */
#line 23 "../HighlightVBT.m3"
 /* load_indirect */
#line 23 "../HighlightVBT.m3"
 /* load */
#line 23 "../HighlightVBT.m3"
 /* load_indirect */
#line 23 "../HighlightVBT.m3"
 /* copy_n */
#line 23 "../HighlightVBT.m3"
m3_memcpy(
 *((ADDRESS*)(HighlightVBT_m_28_L_29)),
 *((ADDRESS*)(openArrayValue_L_20)),
 8*(size_t) *((INT64*)(INT64_(8)+((ADDRESS)(openArrayValue_L_20)))));
#line 23 "../HighlightVBT.m3"
 /* load */
#line 23 "../HighlightVBT.m3"
 /* store */
#line 23 "../HighlightVBT.m3"
(*(ADDRESS*)(&openArrayValue_L_20))=(ADDRESS)(((ADDRESS)(HighlightVBT_m_28_L_29)));
#line 23 "../HighlightVBT.m3"
 /* set_source_line */
#line 23 "../HighlightVBT.m3"
#line 30 "../HighlightVBT.m3"
 /* set_source_line */
#line 30 "../HighlightVBT.m3"
#line 31 "../HighlightVBT.m3"
 /* exit_proc */
#line 31 "../HighlightVBT.m3"
return;
#line 31 "../HighlightVBT.m3"
 /* end_procedure */
#line 31 "../HighlightVBT.m3"
} /* HighlightVBT_M3 */
#line 31 "../HighlightVBT.m3"
 /* module main body HighlightVBT_M3 */
#line 31 "../HighlightVBT.m3"
 /* set_source_line */
#line 31 "../HighlightVBT.m3"
#line 33 "../HighlightVBT.m3"
 /* begin_procedure */
#line 33 "../HighlightVBT.m3"
struct HighlightVBT_M3_Frame_t {
#line 33 "../HighlightVBT.m3"
ADDRESS _unused;
#line 33 "../HighlightVBT.m3"
};
#line 33 "../HighlightVBT.m3"
RT0__ModulePtr
__cdecl
HighlightVBT_M3(
   /* Param_Type1 */ INTEGER /*TypeText1*/  mode_L_10)
{
#line 33 "../HighlightVBT.m3"
HighlightVBT_M3_Frame_t _frame;
#line 33 "../HighlightVBT.m3"
_frame._unused=(ADDRESS)&_frame;
#line 33 "../HighlightVBT.m3"
 /* load */
#line 33 "../HighlightVBT.m3"
 /* if_true_or_false */
#line 33 "../HighlightVBT.m3"
 /* load_host_integer */
#line 33 "../HighlightVBT.m3"
 /* load_integer */
#line 33 "../HighlightVBT.m3"
 /* if_compare */
#line 33 "../HighlightVBT.m3"
if(m3_eq(INT64,
  mode_L_10,
   INT64_(0)))goto L1;
#line 33 "../HighlightVBT.m3"
 /* set_label */
#line 33 "../HighlightVBT.m3"
L1:;
#line 33 "../HighlightVBT.m3"
 /* load_address */
#line 33 "../HighlightVBT.m3"
 /* exit_proc */
#line 33 "../HighlightVBT.m3"
return (RT0__ModulePtr)(&HighlightVBT_m_M_HighlightVBT_L_9);
#line 33 "../HighlightVBT.m3"
 /* end_procedure */
#line 33 "../HighlightVBT.m3"
} /* global constant type descriptor */
#line 33 "../HighlightVBT.m3"
 /* global data type descriptor */
#line 33 "../HighlightVBT.m3"
 /* module global constants */
#line 33 "../HighlightVBT.m3"
 /* procedure names */
#line 33 "../HighlightVBT.m3"
 /* procedure table */
#line 33 "../HighlightVBT.m3"
 /* file name */
#line 33 "../HighlightVBT.m3"
 /* type description for _t2a7f2412 */
#line 33 "../HighlightVBT.m3"
 /* module global data */
#line 33 "../HighlightVBT.m3"
 /* typecell for _t2a7f2412 */
#line 33 "../HighlightVBT.m3"
 /* load map


 global data allocation for M_HighlightVBT
     0   104  8  *module info*
   104   152  8  typecell
   256    24  8  import HighlightVBT
   280    24  8  import VBT
   304    24  8  import RTHooks
   328    16  8  typecell ptr
   344     0  8  *TOTAL*


 global constants for M_HighlightVBT
     0    24  8  revelations
    24    19  8  *proc names*
    48    40  8  *proc info*
    88    19  1  *string*
   112    43  8  brand
   155     3  1  type_desc
   160     8  8  method list
   168    28  1  *string*
   200     0  8  *TOTAL*
 */
#line 33 "../HighlightVBT.m3"
 /* end unit */
#line 33 "../HighlightVBT.m3"

#ifdef __cplusplus

} /* extern "C" */
#endif
 /* set_runtime_proc */
 /* set_runtime_proc */
 /* set_runtime_proc */
