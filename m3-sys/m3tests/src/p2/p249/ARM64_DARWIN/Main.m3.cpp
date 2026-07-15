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
 /* declare_record */
 /* declare_field */
 /* declare_field */
 /* record_forwardDeclare Record_t{ typeid:TA84B4053 text:NIL hash_text:TA84B4053 base_text:NIL state:0} */
/*record_forwardDeclare*/struct TA84B4053;typedef struct TA84B4053 TA84B4053;
 /* record_canBeDefined Record_t{ typeid:TA84B4053 text:NIL hash_text:TA84B4053 base_text:NIL state:0} */
 /* record_define Record_t{ typeid:TA84B4053 text:NIL hash_text:TA84B4053 base_text:NIL state:0} */

#ifndef TA84B4053
#define TA84B4053 TA84B4053
/*record_define*/struct TA84B4053{
double price;
WORD_T volume;
};
#endif
 /* declare_record */
 /* declare_field */
 /* declare_field */
 /* record_forwardDeclare Record_t{ typeid:T367D1BE0 text:NIL hash_text:T367D1BE0 base_text:NIL state:0} */
/*record_forwardDeclare*/struct T367D1BE0;typedef struct T367D1BE0 T367D1BE0;
 /* record_canBeDefined Record_t{ typeid:T367D1BE0 text:NIL hash_text:T367D1BE0 base_text:NIL state:0} */
 /* record_define Record_t{ typeid:T367D1BE0 text:NIL hash_text:T367D1BE0 base_text:NIL state:0} */

#ifndef T367D1BE0
#define T367D1BE0 T367D1BE0
/*record_define*/struct T367D1BE0{
double time;
TA84B4053 pv;
};
#endif
 /* declare_proctype */

#if 0 /* avoid type hash collions */
typedef 
void(__cdecl*T7B78C34F)(void);
#else
typedef void (__cdecl*T7B78C34F)(void);
#endif
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

#ifndef Main__HFData_S
#define Main__HFData_S Main__HFData_S
typedef T367D1BE0 Main__HFData_S;
#endif
 /* declare_raises */
 /* declare_opaque */

#ifndef T9A0E5DBC
#define T9A0E5DBC T9A0E5DBC
/*1addressType_define*/typedef ADDRESS T9A0E5DBC;

#endif
 /* declare_proctype */
 /* declare_formal */

#ifndef Pathname__T
#define Pathname__T Pathname__T
typedef TEXT Pathname__T;
#endif
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
 /* declare_enum */
 /* declare_enum_elt */
 /* declare_enum_elt: NUMBER(self.enum.names^):3 */
 /* declare_enum_elt: enum_element_count:3 */
 /* declare_enum_elt */
 /* declare_enum_elt: NUMBER(self.enum.names^):3 */
 /* declare_enum_elt: enum_element_count:3 */
 /* declare_enum_elt */
 /* declare_enum_elt: NUMBER(self.enum.names^):3 */
 /* declare_enum_elt: enum_element_count:3 */
/*enum_define*/typedef UINT8 T2E1FCF67; /*declare_enum*/
#define T2E1FCF67_Sci ((UINT8)0) /*declare_enum_elt*/
#define T2E1FCF67_Fix ((UINT8)1) /*declare_enum_elt*/
#define T2E1FCF67_Auto ((UINT8)2) /*declare_enum_elt*/
 /* declare_proctype */
 /* declare_formal */
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

#ifndef Wr__T
#define Wr__T Wr__T
typedef T62761487 Wr__T;
#endif
 /* declare_formal */
 /* declare_raises */
 /* declare_raises */
 /* declare_record */
 /* declare_record */
 /* DeclareTypes_FlushOnce size:11 */
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
void(__cdecl*TFCF1BAB9)(Main__HFData_S);
#else
typedef void (__cdecl*TFCF1BAB9)(void);
#endif

#if 0 /* avoid type hash collions */
typedef 
T9A0E5DBC(__cdecl*T49473232)(Pathname__T);
#else
typedef void (__cdecl*T49473232)(void);
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
TEXT(__cdecl*T95AA481C)(LONGREAL,T2E1FCF67,CARDINAL,BOOLEAN);
#else
typedef void (__cdecl*T95AA481C)(void);
#endif

#if 0 /* avoid type hash collions */
typedef 
void(__cdecl*T1BEE6E85)(Wr__T,TEXT);
#else
typedef void (__cdecl*T1BEE6E85)(void);
#endif
 /* DeclareTypes_FlushOnce size:0 */
 /* end: DeclareTypes */
 /* begin: helper functions */
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
   /* Param_Type1 */ INTEGER /*TypeText1*/  mode_L_1);
 /* import_procedure */
/*Proc_ForwardDeclareFrameType*/struct Fmt_I3_Frame_t;typedef struct Fmt_I3_Frame_t Fmt_I3_Frame_t;
 /* internal_declare_param */
RT0__ModulePtr
__cdecl
Fmt_I3(
   /* Param_Type1 */ INTEGER /*TypeText1*/  mode_L_2);
 /* import_procedure */
/*Proc_ForwardDeclareFrameType*/struct Wr_I3_Frame_t;typedef struct Wr_I3_Frame_t Wr_I3_Frame_t;
 /* internal_declare_param */
RT0__ModulePtr
__cdecl
Wr_I3(
   /* Param_Type1 */ INTEGER /*TypeText1*/  mode_L_3);
 /* import_procedure */
/*Proc_ForwardDeclareFrameType*/struct FileWr_I3_Frame_t;typedef struct FileWr_I3_Frame_t FileWr_I3_Frame_t;
 /* internal_declare_param */
RT0__ModulePtr
__cdecl
FileWr_I3(
   /* Param_Type1 */ INTEGER /*TypeText1*/  mode_L_4);
 /* import_procedure */
/*Proc_ForwardDeclareFrameType*/struct RTHooks_I3_Frame_t;typedef struct RTHooks_I3_Frame_t RTHooks_I3_Frame_t;
 /* internal_declare_param */
RT0__ModulePtr
__cdecl
RTHooks_I3(
   /* Param_Type1 */ INTEGER /*TypeText1*/  mode_L_5);
 /* import_procedure */

#ifndef FileWr__T
#define FileWr__T FileWr__T
typedef T9A0E5DBC /*TypeText1*/  FileWr__T;
#endif
/*Proc_ForwardDeclareFrameType*/struct FileWr__Open_Frame_t;typedef struct FileWr__Open_Frame_t FileWr__Open_Frame_t;
 /* internal_declare_param */
FileWr__T
__cdecl
FileWr__Open(
   /* Param_Type1 */ Pathname__T p_L_6);
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
/*Proc_ForwardDeclareFrameType*/struct Fmt__LongReal_Frame_t;typedef struct Fmt__LongReal_Frame_t Fmt__LongReal_Frame_t;
 /* internal_declare_param */
 /* internal_declare_param */
 /* internal_declare_param */
 /* internal_declare_param */
TEXT
__cdecl
Fmt__LongReal(
   /* Param_Type1 */ LONGREAL x_L_19,
   /* Param_Type1 */ T2E1FCF67 /*TypeText1*/  style_L_20,
   /* Param_Type1 */ CARDINAL prec_L_21,
   /* Param_Type1 */ BOOLEAN literal_L_22);
 /* import_procedure */
/*Proc_ForwardDeclareFrameType*/struct Wr__PutText_Frame_t;typedef struct Wr__PutText_Frame_t Wr__PutText_Frame_t;
 /* internal_declare_param */
 /* internal_declare_param */
void /*TypeText3*/ 
__cdecl
Wr__PutText(
   /* Param_Type1 */ Wr__T wr_L_23,
   /* Param_Type1 */ TEXT t_L_24);
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
/*Proc_ForwardDeclareFrameType*/struct Main__DumpMatching_Frame_t;typedef struct Main__DumpMatching_Frame_t Main__DumpMatching_Frame_t;
void /*TypeText3*/ 
__cdecl
Main__DumpMatching(void);
 /* declare_local */
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct Main__DumpMatching__DumpOne_Frame_t;typedef struct Main__DumpMatching__DumpOne_Frame_t Main__DumpMatching__DumpOne_Frame_t;
 /* internal_declare_param */
 /* internal_declare_param */
void /*TypeText3*/ 
__cdecl
Main__DumpMatching__DumpOne(
   /* Param_Type1 */ Main__HFData_S*_param_struct_pointer_trade_L_30,
   /* Param_Type1 */ Main__DumpMatching_Frame_t* _static_link);
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
 /* init_chars */
 /* init_chars */
 /* init_chars */
 /* init_proc */
 /* init_var */
 /* init_proc */
 /* init_var */
 /* init_proc */
 /* init_var */
 /* init_chars */
 /* end_init */
struct Main_m_25_L_26_t{ADDRESS L_35[5];
INT64 L_36[1];
ADDRESS L_37[1];
INT64 L_38[1];
UINT8 L_39[3];
char L_40[5];
UINT8 L_41[7];
char L_42[1];
UINT8 L_43[12];
char L_44[1];
UINT8 L_45[20];
char L_46[7];
ADDRESS L_47[6];
char L_48[8];
UINT8 L_49[10];
char L_50[6];
};
static  const Main_m_25_L_26_t Main_m_25_L_26={{(ADDRESS)&RTHooks__TextLitInfo,(ADDRESS)&RTHooks__TextLitGetChar,(ADDRESS)&RTHooks__TextLitGetWideChar,(ADDRESS)&RTHooks__TextLitGetChars,(ADDRESS)&RTHooks__TextLitGetWideChars},{INT64_(2)},{(char*)&Main_m_25_L_26},{INT64_(3)},{'o','u','t'},{0 /* 1 */ ,0 /* 2 */ ,0 /* 3 */ ,0 /* 4 */ ,0 /* 5 */ ,},{'M','a','i','n','_','M','3'},{0 /* 1 */ ,},{'D','u','m','p','M','a','t','c','h','i','n','g'},{0 /* 1 */ ,},{'D','u','m','p','M','a','t','c','h','i','n','g','.','D','u','m','p','O','n','e'},{0 /* 1 */ ,0 /* 2 */ ,0 /* 3 */ ,0 /* 4 */ ,0 /* 5 */ ,0 /* 6 */ ,0 /* 7 */ ,},{(ADDRESS)&Main_M3,72+(char*)&Main_m_25_L_26,(ADDRESS)&Main__DumpMatching,80+(char*)&Main_m_25_L_26,(ADDRESS)&Main__DumpMatching__DumpOne,93+(char*)&Main_m_25_L_26},{0 /* 1 */ ,0 /* 2 */ ,0 /* 3 */ ,0 /* 4 */ ,0 /* 5 */ ,0 /* 6 */ ,0 /* 7 */ ,0 /* 8 */ ,},{'.','.','/','M','a','i','n','.','m','3'},{0 /* 1 */ ,0 /* 2 */ ,0 /* 3 */ ,0 /* 4 */ ,0 /* 5 */ ,0 /* 6 */ ,}};
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
 /* end_init */
struct Main_m_M_Main_L_27_t{ADDRESS L_51[1];
char L_52[32];
ADDRESS L_53[1];
char L_54[24];
ADDRESS L_55[1];
char L_56[8];
ADDRESS L_57[1];
INT64 L_58[1];
char L_59[8];
ADDRESS L_60[2];
char L_61[8];
ADDRESS L_62[2];
char L_63[8];
ADDRESS L_64[2];
char L_65[8];
ADDRESS L_66[2];
char L_67[8];
ADDRESS L_68[1];
char L_69[8];
};
static Main_m_M_Main_L_27_t Main_m_M_Main_L_27={{176+(char*)&Main_m_25_L_26},{0 /* 1 */ ,0 /* 2 */ ,0 /* 3 */ ,0 /* 4 */ ,0 /* 5 */ ,0 /* 6 */ ,0 /* 7 */ ,0 /* 8 */ ,0 /* 9 */ ,0 /* 10 */ ,0 /* 11 */ ,0 /* 12 */ ,0 /* 13 */ ,0 /* 14 */ ,0 /* 15 */ ,0 /* 16 */ ,0 /* 17 */ ,0 /* 18 */ ,0 /* 19 */ ,0 /* 20 */ ,0 /* 21 */ ,0 /* 22 */ ,0 /* 23 */ ,0 /* 24 */ ,0 /* 25 */ ,0 /* 26 */ ,0 /* 27 */ ,0 /* 28 */ ,0 /* 29 */ ,0 /* 30 */ ,0 /* 31 */ ,0 /* 32 */ ,},{120+(char*)&Main_m_25_L_26},{0 /* 1 */ ,0 /* 2 */ ,0 /* 3 */ ,0 /* 4 */ ,0 /* 5 */ ,0 /* 6 */ ,0 /* 7 */ ,0 /* 8 */ ,0 /* 9 */ ,0 /* 10 */ ,0 /* 11 */ ,0 /* 12 */ ,0 /* 13 */ ,0 /* 14 */ ,0 /* 15 */ ,0 /* 16 */ ,0 /* 17 */ ,0 /* 18 */ ,0 /* 19 */ ,0 /* 20 */ ,0 /* 21 */ ,0 /* 22 */ ,0 /* 23 */ ,0 /* 24 */ ,},{104+(char*)&Main_m_M_Main_L_27},{0 /* 1 */ ,0 /* 2 */ ,0 /* 3 */ ,0 /* 4 */ ,0 /* 5 */ ,0 /* 6 */ ,0 /* 7 */ ,0 /* 8 */ ,},{(ADDRESS)&Main_M3},{INT64_(3)},{0 /* 1 */ ,0 /* 2 */ ,0 /* 3 */ ,0 /* 4 */ ,0 /* 5 */ ,0 /* 6 */ ,0 /* 7 */ ,
0 /* 8 */ ,},{(ADDRESS)&Main_I3,128+(char*)&Main_m_M_Main_L_27},{0 /* 1 */ ,0 /* 2 */ ,0 /* 3 */ ,0 /* 4 */ ,0 /* 5 */ ,0 /* 6 */ ,0 /* 7 */ ,0 /* 8 */ ,},{(ADDRESS)&Fmt_I3,152+(char*)&Main_m_M_Main_L_27},{0 /* 1 */ ,0 /* 2 */ ,0 /* 3 */ ,0 /* 4 */ ,0 /* 5 */ ,0 /* 6 */ ,0 /* 7 */ ,0 /* 8 */ ,},{(ADDRESS)&Wr_I3,176+(char*)&Main_m_M_Main_L_27},{0 /* 1 */ ,0 /* 2 */ ,0 /* 3 */ ,0 /* 4 */ ,0 /* 5 */ ,0 /* 6 */ ,0 /* 7 */ ,0 /* 8 */ ,},{(ADDRESS)&FileWr_I3,200+(char*)&Main_m_M_Main_L_27},{0 /* 1 */ ,0 /* 2 */ ,0 /* 3 */ ,0 /* 4 */ ,0 /* 5 */ ,0 /* 6 */ ,0 /* 7 */ ,0 /* 8 */ ,},{(ADDRESS)&RTHooks_I3},{0 /* 1 */ ,0 /* 2 */ ,0 /* 3 */ ,0 /* 4 */ ,0 /* 5 */ ,0 /* 6 */ ,0 /* 7 */ ,0 /* 8 */ ,}};
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
#line 25 "../Main.m3"
 /* DumpMatching */
#line 25 "../Main.m3"
 /* set_source_line */
#line 25 "../Main.m3"
#line 15 "../Main.m3"
 /* begin_procedure */
#line 15 "../Main.m3"
struct Main__DumpMatching_Frame_t {
#line 15 "../Main.m3"
ADDRESS _unused;
#line 15 "../Main.m3"
 /* Var_Type1 */ T9A0E5DBC wr_L_29;
#line 15 "../Main.m3"
};
#line 15 "../Main.m3"
void /*TypeText3*/ 
__cdecl
Main__DumpMatching(void)
{
#line 15 "../Main.m3"
 /* Var_Type2 */ ADDRESS Main_m_31_L_32={0};//always-init
#line 15 "../Main.m3"
Main__DumpMatching_Frame_t _frame;
#line 15 "../Main.m3"
_frame._unused=(ADDRESS)&_frame;
#line 15 "../Main.m3"
 /* set_label */
#line 15 "../Main.m3"
 /* note_procedure_origin */
#line 15 "../Main.m3"
 /* set_source_line */
#line 15 "../Main.m3"
#line 21 "../Main.m3"
 /* load_nil */
#line 21 "../Main.m3"
 /* store */
#line 21 "../Main.m3"
(*(ADDRESS*)(&_frame.wr_L_29))=(ADDRESS)(((ADDRESS)(0)));
#line 21 "../Main.m3"
 /* start_call_direct */
#line 21 "../Main.m3"
 /* load_address */
#line 21 "../Main.m3"
 /* pop_param */
#line 21 "../Main.m3"
 /* call_direct */
#line 21 "../Main.m3"
 /* store */
#line 21 "../Main.m3"
(*(ADDRESS*)(&Main_m_31_L_32))=(ADDRESS)(((ADDRESS)(FileWr__Open(
  ( Pathname__T )(((ADDRESS)(INT64_(48)+((ADDRESS)(&Main_m_25_L_26)))) )))));
#line 21 "../Main.m3"
 /* load */
#line 21 "../Main.m3"
 /* store */
#line 21 "../Main.m3"
(*(ADDRESS*)(&_frame.wr_L_29))=(ADDRESS)(((ADDRESS)(Main_m_31_L_32)));
#line 21 "../Main.m3"
 /* set_source_line */
#line 21 "../Main.m3"
#line 16 "../Main.m3"
 /* set_label */
#line 16 "../Main.m3"
 /* set_source_line */
#line 16 "../Main.m3"
#line 23 "../Main.m3"
 /* exit_proc */
#line 23 "../Main.m3"
return;
#line 23 "../Main.m3"
 /* end_procedure */
#line 23 "../Main.m3"
} /* DumpMatching.DumpOne */
#line 23 "../Main.m3"
 /* set_source_line */
#line 23 "../Main.m3"
#line 16 "../Main.m3"
 /* begin_procedure */
#line 16 "../Main.m3"
struct Main__DumpMatching__DumpOne_Frame_t {
#line 16 "../Main.m3"
ADDRESS _unused;
#line 16 "../Main.m3"
 /* Var_Type1 */ Main__DumpMatching_Frame_t* _static_link;
#line 16 "../Main.m3"
};
#line 16 "../Main.m3"
void /*TypeText3*/ 
__cdecl
Main__DumpMatching__DumpOne(
   /* Param_Type1 */ Main__HFData_S*_param_struct_pointer_trade_L_30,
   /* Param_Type1 */ Main__DumpMatching_Frame_t* _static_link)
{
#line 16 "../Main.m3"
 /* Var_Type2 */ ADDRESS Main_m_33_L_34={0};//always-init
#line 16 "../Main.m3"
 /* Var_Type1 */ Main__HFData_S trade_L_30;
#line 16 "../Main.m3"
Main__DumpMatching__DumpOne_Frame_t _frame;
#line 16 "../Main.m3"
_frame._static_link=_static_link;
#line 16 "../Main.m3"
_frame._unused=(ADDRESS)&_frame;
#line 16 "../Main.m3"
trade_L_30=*_param_struct_pointer_trade_L_30;
#line 16 "../Main.m3"
 /* set_label */
#line 16 "../Main.m3"
 /* set_source_line */
#line 16 "../Main.m3"
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
 /* load_integer */
#line 18 "../Main.m3"
 /* pop_param */
#line 18 "../Main.m3"
 /* load_integer */
#line 18 "../Main.m3"
 /* pop_param */
#line 18 "../Main.m3"
 /* call_direct */
#line 18 "../Main.m3"
 /* store */
#line 18 "../Main.m3"
(*(ADDRESS*)(&Main_m_33_L_34))=(ADDRESS)(((ADDRESS)(Fmt__LongReal(
  ( LONGREAL )(((double)(*((double*)(&trade_L_30)))) ),
  ( T2E1FCF67 /*TypeText1*/  )(((UINT8)( INT64_(2))) ),
  ( CARDINAL )(((UINT64)( INT64_(16))) ),
  ( BOOLEAN )(((UINT8)( INT64_(0))) )))));
#line 18 "../Main.m3"
 /* start_call_direct */
#line 18 "../Main.m3"
 /* load */
#line 18 "../Main.m3"
 /* pop_param */
#line 18 "../Main.m3"
 /* load */
#line 18 "../Main.m3"
 /* pop_param */
#line 18 "../Main.m3"
 /* call_direct */
#line 18 "../Main.m3"
Wr__PutText(
  ( Wr__T )(((ADDRESS)(_static_link->wr_L_29)) ),
  ( TEXT )(((ADDRESS)(Main_m_33_L_34)) ));
#line 18 "../Main.m3"
 /* set_label */
#line 18 "../Main.m3"
 /* set_source_line */
#line 18 "../Main.m3"
#line 19 "../Main.m3"
 /* exit_proc */
#line 19 "../Main.m3"
return;
#line 19 "../Main.m3"
 /* end_procedure */
#line 19 "../Main.m3"
} /* Main_M3 */
#line 19 "../Main.m3"
 /* module main body Main_M3 */
#line 19 "../Main.m3"
 /* set_source_line */
#line 19 "../Main.m3"
#line 25 "../Main.m3"
 /* begin_procedure */
#line 25 "../Main.m3"
struct Main_M3_Frame_t {
#line 25 "../Main.m3"
ADDRESS _unused;
#line 25 "../Main.m3"
};
#line 25 "../Main.m3"
RT0__ModulePtr
__cdecl
Main_M3(
   /* Param_Type1 */ INTEGER /*TypeText1*/  mode_L_28)
{
#line 25 "../Main.m3"
Main_M3_Frame_t _frame;
#line 25 "../Main.m3"
_frame._unused=(ADDRESS)&_frame;
#line 25 "../Main.m3"
 /* load */
#line 25 "../Main.m3"
 /* if_true_or_false */
#line 25 "../Main.m3"
 /* load_host_integer */
#line 25 "../Main.m3"
 /* load_integer */
#line 25 "../Main.m3"
 /* if_compare */
#line 25 "../Main.m3"
if(m3_eq(INT64,
  mode_L_28,
   INT64_(0)))goto L5;
#line 25 "../Main.m3"
 /* set_label */
#line 25 "../Main.m3"
L5:;
#line 25 "../Main.m3"
 /* load_address */
#line 25 "../Main.m3"
 /* exit_proc */
#line 25 "../Main.m3"
return (RT0__ModulePtr)(&Main_m_M_Main_L_27);
#line 25 "../Main.m3"
 /* end_procedure */
#line 25 "../Main.m3"
} /* global constant type descriptor */
#line 25 "../Main.m3"
 /* global data type descriptor */
#line 25 "../Main.m3"
 /* module global constants */
#line 25 "../Main.m3"
 /* procedure names */
#line 25 "../Main.m3"
 /* procedure table */
#line 25 "../Main.m3"
 /* file name */
#line 25 "../Main.m3"
 /* module global data */
#line 25 "../Main.m3"
 /* load map


 global data allocation for M_Main
     0   104  8  *module info*
   104    24  8  import Main
   128    24  8  import Fmt
   152    24  8  import Wr
   176    24  8  import FileWr
   200    24  8  import RTHooks
   224     0  8  *TOTAL*


 global constants for M_Main
     0    40  8  TEXT literal methods
    40    28  8  *TEXT literal*
    72    42  8  *proc names*
   120    56  8  *proc info*
   176    11  1  *string*
   192     0  8  *TOTAL*
 */
#line 25 "../Main.m3"
 /* end unit */
#line 25 "../Main.m3"

#ifdef __cplusplus

} /* extern "C" */
#endif
 /* set_runtime_proc */
 /* set_runtime_proc */
 /* set_runtime_proc */
