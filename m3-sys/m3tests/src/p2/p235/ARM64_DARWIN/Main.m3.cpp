// library:a
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
 /* declare_enum */
 /* declare_enum_elt */
 /* declare_enum_elt: NUMBER(self.enum.names^):12 */
 /* declare_enum_elt: enum_element_count:12 */
 /* declare_enum_elt */
 /* declare_enum_elt: NUMBER(self.enum.names^):12 */
 /* declare_enum_elt: enum_element_count:12 */
 /* declare_enum_elt */
 /* declare_enum_elt: NUMBER(self.enum.names^):12 */
 /* declare_enum_elt: enum_element_count:12 */
 /* declare_enum_elt */
 /* declare_enum_elt: NUMBER(self.enum.names^):12 */
 /* declare_enum_elt: enum_element_count:12 */
 /* declare_enum_elt */
 /* declare_enum_elt: NUMBER(self.enum.names^):12 */
 /* declare_enum_elt: enum_element_count:12 */
 /* declare_enum_elt */
 /* declare_enum_elt: NUMBER(self.enum.names^):12 */
 /* declare_enum_elt: enum_element_count:12 */
 /* declare_enum_elt */
 /* declare_enum_elt: NUMBER(self.enum.names^):12 */
 /* declare_enum_elt: enum_element_count:12 */
 /* declare_enum_elt */
 /* declare_enum_elt: NUMBER(self.enum.names^):12 */
 /* declare_enum_elt: enum_element_count:12 */
 /* declare_enum_elt */
 /* declare_enum_elt: NUMBER(self.enum.names^):12 */
 /* declare_enum_elt: enum_element_count:12 */
 /* declare_enum_elt */
 /* declare_enum_elt: NUMBER(self.enum.names^):12 */
 /* declare_enum_elt: enum_element_count:12 */
 /* declare_enum_elt */
 /* declare_enum_elt: NUMBER(self.enum.names^):12 */
 /* declare_enum_elt: enum_element_count:12 */
 /* declare_enum_elt */
 /* declare_enum_elt: NUMBER(self.enum.names^):12 */
 /* declare_enum_elt: enum_element_count:12 */
/*enum_define*/typedef UINT8 TA33463A; /*declare_enum*/
#define TA33463A_Jan ((UINT8)0) /*declare_enum_elt*/
#define TA33463A_Feb ((UINT8)1) /*declare_enum_elt*/
#define TA33463A_Mar ((UINT8)2) /*declare_enum_elt*/
#define TA33463A_Apr ((UINT8)3) /*declare_enum_elt*/
#define TA33463A_May ((UINT8)4) /*declare_enum_elt*/
#define TA33463A_Jun ((UINT8)5) /*declare_enum_elt*/
#define TA33463A_Jul ((UINT8)6) /*declare_enum_elt*/
#define TA33463A_Aug ((UINT8)7) /*declare_enum_elt*/
#define TA33463A_Sep ((UINT8)8) /*declare_enum_elt*/
#define TA33463A_Oct ((UINT8)9) /*declare_enum_elt*/
#define TA33463A_Nov ((UINT8)10) /*declare_enum_elt*/
#define TA33463A_Dec ((UINT8)11) /*declare_enum_elt*/
 /* declare_subrange */
/*subrange_define*/typedef UINT8 T2DA6581C_8;
 /* declare_subrange */
/*subrange_define*/typedef UINT8 T2FA7581D_8;
 /* declare_subrange */
/*subrange_define*/typedef UINT8 T25A0581D_8;
 /* declare_subrange */
/*subrange_define*/typedef UINT8 T2CA3581D_8;
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
/*enum_define*/typedef UINT8 T2A65EC03; /*declare_enum*/
#define T2A65EC03_Sun ((UINT8)0) /*declare_enum_elt*/
#define T2A65EC03_Mon ((UINT8)1) /*declare_enum_elt*/
#define T2A65EC03_Tue ((UINT8)2) /*declare_enum_elt*/
#define T2A65EC03_Wed ((UINT8)3) /*declare_enum_elt*/
#define T2A65EC03_Thu ((UINT8)4) /*declare_enum_elt*/
#define T2A65EC03_Fri ((UINT8)5) /*declare_enum_elt*/
#define T2A65EC03_Sat ((UINT8)6) /*declare_enum_elt*/
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
 /* record_forwardDeclare Record_t{ typeid:T32721A2D text:NIL hash_text:T32721A2D base_text:NIL state:0} */
/*record_forwardDeclare*/struct T32721A2D;typedef struct T32721A2D T32721A2D;
 /* record_canBeDefined Record_t{ typeid:T32721A2D text:NIL hash_text:T32721A2D base_text:NIL state:0} */
 /* record_define Record_t{ typeid:T32721A2D text:NIL hash_text:T32721A2D base_text:NIL state:0} */

#ifndef T32721A2D
#define T32721A2D T32721A2D
/*record_define*/struct T32721A2D{
WORD_T year;
TA33463A month;
T2DA6581C_8 day;
T2FA7581D_8 hour;
T25A0581D_8 minute;
T2CA3581D_8 second;
UINT8 L_0[3];
INTEGER offset;
TEXT zone;
T2A65EC03 weekDay;
UINT8 L_1[7];
};
#endif
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
 /* declare_formal */
 /* declare_proctype */
 /* declare_formal */
 /* declare_formal */
 /* declare_proctype */

#if 0 /* avoid type hash collions */
typedef 
double(__cdecl*TE4E28466)(void);
#else
typedef void (__cdecl*TE4E28466)(void);
#endif
 /* declare_proctype */
 /* declare_formal */
 /* declare_opaque */

#ifndef T188952BF
#define T188952BF T188952BF
/*1addressType_define*/typedef ADDRESS T188952BF;

#endif
 /* declare_proctype */
 /* declare_formal */
 /* declare_proctype */
 /* declare_formal */

#ifndef Time__T
#define Time__T Time__T
typedef double Time__T;
#endif
 /* declare_formal */

#ifndef Date__TimeZone
#define Date__TimeZone Date__TimeZone
typedef T188952BF Date__TimeZone;
#endif
 /* declare_indirect */
typedef T32721A2D*TCD8DE5D2;
 /* declare_record */
 /* declare_record */
 /* declare_field */
 /* DeclareTypes_FlushOnce size:11 */

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
void(__cdecl*T27B0F303)(INTEGER,INTEGER);
#else
typedef void (__cdecl*T27B0F303)(void);
#endif

#if 0 /* avoid type hash collions */
typedef 
void(__cdecl*TDDDE6CCC)(LONGREAL);
#else
typedef void (__cdecl*TDDDE6CCC)(void);
#endif

#if 0 /* avoid type hash collions */
typedef 
void(__cdecl*T37E50548)(REFANY);
#else
typedef void (__cdecl*T37E50548)(void);
#endif

#if 0 /* avoid type hash collions */
typedef 
void(__cdecl*TD83AAF93)(Time__T,Date__TimeZone);
#else
typedef void (__cdecl*TD83AAF93)(void);
#endif
 /* DeclareTypes_FlushOnce size:0 */
 /* end: DeclareTypes */
 /* begin: helper functions */
#define m3_extract(T, value, offset, count) ((((T)(value))>>((WORD_T)(offset)))&~(((~(T)0))<<((WORD_T)(count))))
 /* end: helper functions */

#ifndef struct_40_t
#define struct_40_t struct_40_t
STRUCT8(40)
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
   /* Param_Type1 */ INTEGER /*TypeText1*/  mode_L_3);
 /* import_procedure */
/*Proc_ForwardDeclareFrameType*/struct Time_I3_Frame_t;typedef struct Time_I3_Frame_t Time_I3_Frame_t;
 /* internal_declare_param */
RT0__ModulePtr
__cdecl
Time_I3(
   /* Param_Type1 */ INTEGER /*TypeText1*/  mode_L_4);
 /* import_procedure */
/*Proc_ForwardDeclareFrameType*/struct RTIO_I3_Frame_t;typedef struct RTIO_I3_Frame_t RTIO_I3_Frame_t;
 /* internal_declare_param */
RT0__ModulePtr
__cdecl
RTIO_I3(
   /* Param_Type1 */ INTEGER /*TypeText1*/  mode_L_5);
 /* import_procedure */
/*Proc_ForwardDeclareFrameType*/struct Date_I3_Frame_t;typedef struct Date_I3_Frame_t Date_I3_Frame_t;
 /* internal_declare_param */
RT0__ModulePtr
__cdecl
Date_I3(
   /* Param_Type1 */ INTEGER /*TypeText1*/  mode_L_6);
 /* import_procedure */
/*Proc_ForwardDeclareFrameType*/struct RTHooks_I3_Frame_t;typedef struct RTHooks_I3_Frame_t RTHooks_I3_Frame_t;
 /* internal_declare_param */
RT0__ModulePtr
__cdecl
RTHooks_I3(
   /* Param_Type1 */ INTEGER /*TypeText1*/  mode_L_7);
 /* import_procedure */
/*Proc_ForwardDeclareFrameType*/struct RTIO__PutText_Frame_t;typedef struct RTIO__PutText_Frame_t RTIO__PutText_Frame_t;
 /* internal_declare_param */
void /*TypeText3*/ 
__cdecl
RTIO__PutText(
   /* Param_Type1 */ TEXT t_L_8);
 /* import_procedure */
/*Proc_ForwardDeclareFrameType*/struct RTHooks__TextLitInfo_Frame_t;typedef struct RTHooks__TextLitInfo_Frame_t RTHooks__TextLitInfo_Frame_t;
 /* internal_declare_param */
 /* internal_declare_param */
void /*TypeText3*/ 
__cdecl
RTHooks__TextLitInfo(
   /* Param_Type1 */ RTHooks__TextLiteral t_L_9,
   /* Param_Type1 */ RTHooks__TextInfo* /*TypeText1*/  i_L_10);
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
   /* Param_Type1 */ RTHooks__TextLiteral t_L_11,
   /* Param_Type1 */ CARDINAL i_L_12);
 /* import_procedure */
/*Proc_ForwardDeclareFrameType*/struct RTHooks__TextLitGetWideChar_Frame_t;typedef struct RTHooks__TextLitGetWideChar_Frame_t RTHooks__TextLitGetWideChar_Frame_t;
 /* internal_declare_param */
 /* internal_declare_param */
WIDECHAR
__cdecl
RTHooks__TextLitGetWideChar(
   /* Param_Type1 */ RTHooks__TextLiteral t_L_13,
   /* Param_Type1 */ CARDINAL i_L_14);
 /* import_procedure */
/*Proc_ForwardDeclareFrameType*/struct RTHooks__TextLitGetChars_Frame_t;typedef struct RTHooks__TextLitGetChars_Frame_t RTHooks__TextLitGetChars_Frame_t;
 /* internal_declare_param */
 /* internal_declare_param */
 /* internal_declare_param */
void /*TypeText3*/ 
__cdecl
RTHooks__TextLitGetChars(
   /* Param_Type1 */ RTHooks__TextLiteral t_L_15,
   /* Param_Type1 */ T89CD34BD* /*TypeText1*/  a_L_16,
   /* Param_Type1 */ CARDINAL start_L_17);
 /* import_procedure */
/*Proc_ForwardDeclareFrameType*/struct RTHooks__TextLitGetWideChars_Frame_t;typedef struct RTHooks__TextLitGetWideChars_Frame_t RTHooks__TextLitGetWideChars_Frame_t;
 /* internal_declare_param */
 /* internal_declare_param */
 /* internal_declare_param */
void /*TypeText3*/ 
__cdecl
RTHooks__TextLitGetWideChars(
   /* Param_Type1 */ RTHooks__TextLiteral t_L_18,
   /* Param_Type1 */ TA19BDC21* /*TypeText1*/  a_L_19,
   /* Param_Type1 */ CARDINAL start_L_20);
 /* import_procedure */
/*Proc_ForwardDeclareFrameType*/struct RTIO__Flush_Frame_t;typedef struct RTIO__Flush_Frame_t RTIO__Flush_Frame_t;
void /*TypeText3*/ 
__cdecl
RTIO__Flush(void);
 /* import_procedure */
/*Proc_ForwardDeclareFrameType*/struct RTIO__PutAddr_Frame_t;typedef struct RTIO__PutAddr_Frame_t RTIO__PutAddr_Frame_t;
 /* internal_declare_param */
 /* internal_declare_param */
void /*TypeText3*/ 
__cdecl
RTIO__PutAddr(
   /* Param_Type1 */ ADDRESS a_L_21,
   /* Param_Type1 */ INTEGER width_L_22);
 /* import_procedure */
/*Proc_ForwardDeclareFrameType*/struct RTIO__PutInt_Frame_t;typedef struct RTIO__PutInt_Frame_t RTIO__PutInt_Frame_t;
 /* internal_declare_param */
 /* internal_declare_param */
void /*TypeText3*/ 
__cdecl
RTIO__PutInt(
   /* Param_Type1 */ INTEGER i_L_23,
   /* Param_Type1 */ INTEGER width_L_24);
 /* import_procedure */
/*Proc_ForwardDeclareFrameType*/struct Time__Now_Frame_t;typedef struct Time__Now_Frame_t Time__Now_Frame_t;
Time__T
__cdecl
Time__Now(void);
 /* import_procedure */
/*Proc_ForwardDeclareFrameType*/struct RTIO__PutF_Frame_t;typedef struct RTIO__PutF_Frame_t RTIO__PutF_Frame_t;
 /* internal_declare_param */
void /*TypeText3*/ 
__cdecl
RTIO__PutF(
   /* Param_Type1 */ LONGREAL a_L_25);
 /* import_procedure */
/*Proc_ForwardDeclareFrameType*/struct RTHooks__CheckLoadTracedRef_Frame_t;typedef struct RTHooks__CheckLoadTracedRef_Frame_t RTHooks__CheckLoadTracedRef_Frame_t;
 /* internal_declare_param */
void /*TypeText3*/ 
__cdecl
RTHooks__CheckLoadTracedRef(
   /* Param_Type1 */ REFANY ref_L_26);
 /* import_procedure */
/*Proc_ForwardDeclareFrameType*/struct Date__FromTime_Frame_t;typedef struct Date__FromTime_Frame_t Date__FromTime_Frame_t;
 /* internal_declare_param */
 /* internal_declare_param */
 /* internal_declare_param */
void /*TypeText3*/ 
__cdecl
Date__FromTime(
   /* Param_Type1 */ T32721A2D* /*TypeText1*/  _return_L_27,
   /* Param_Type1 */ Time__T t_L_28,
   /* Param_Type1 */ Date__TimeZone z_L_29);
 /* end: imports */
 /* begin: locals */
 /* declare_segment name:<NIL> typeid:TFFFFFFFF const:TRUE */
/*declare_segment*/struct Main_m_30_L_31_t;
/*declare_segment*/typedef struct Main_m_30_L_31_t Main_m_30_L_31_t;
 /* declare_segment name:M_Main typeid:TFFFFFFFF const:FALSE */
 /* handler_name_prefixes:Main_M3_LINE_ */
 /* handler_name_prefixes:Main_I3_LINE_ */
/*declare_segment*/struct Main_m_M_Main_L_32_t;
/*declare_segment*/typedef struct Main_m_M_Main_L_32_t Main_m_M_Main_L_32_t;
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct Main_M3_Frame_t;typedef struct Main_M3_Frame_t Main_M3_Frame_t;
 /* internal_declare_param */
RT0__ModulePtr
__cdecl
Main_M3(
   /* Param_Type1 */ INTEGER /*TypeText1*/  mode_L_33);
 /* declare_procedure */
/*Proc_ForwardDeclareFrameType*/struct Main__NL_Frame_t;typedef struct Main__NL_Frame_t Main__NL_Frame_t;
void /*TypeText3*/ 
__cdecl
Main__NL(void);
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
 /* init_chars */
 /* init_chars */
 /* init_proc */
 /* init_var */
 /* init_proc */
 /* init_var */
 /* init_int */
 /* init_int */
 /* init_int */
 /* init_int */
 /* init_chars */
 /* end_init */
struct Main_m_30_L_31_t{ADDRESS L_40[5];
INT64 L_41[1];
ADDRESS L_42[1];
INT64 L_43[1];
UINT8 L_44[1];
char L_45[7];
INT64 L_46[1];
ADDRESS L_47[1];
INT64 L_48[1];
UINT8 L_49[3];
char L_50[5];
INT64 L_51[1];
ADDRESS L_52[1];
INT64 L_53[1];
UINT8 L_54[6];
char L_55[2];
INT64 L_56[1];
ADDRESS L_57[1];
INT64 L_58[1];
UINT8 L_59[7];
char L_60[1];
INT64 L_61[1];
ADDRESS L_62[1];
INT64 L_63[1];
UINT8 L_64[5];
char L_65[3];
INT64 L_66[1];
ADDRESS L_67[1];
INT64 L_68[1];
UINT8 L_69[6];
char L_70[2];
INT64 L_71[1];
ADDRESS L_72[1];
INT64 L_73[1];
UINT8 L_74[8];
char L_75[8];
INT64 L_76[1];
ADDRESS L_77[1];
INT64 L_78[1];
UINT8 L_79[8];
char L_80[8];
INT64 L_81[1];
ADDRESS L_82[1];
INT64 L_83[1];
UINT8 L_84[8];
char L_85[8];
INT64 L_86[1];
ADDRESS L_87[1];
INT64 L_88[1];
UINT8 L_89[6];
char L_90[2];
INT64 L_91[1];
ADDRESS L_92[1];
INT64 L_93[1];
UINT8 L_94[9];
char L_95[7];
INT64 L_96[1];
ADDRESS L_97[1];
INT64 L_98[1];
UINT8 L_99[7];
char L_100[1];
INT64 L_101[1];
ADDRESS L_102[1];
INT64 L_103[1];
UINT8 L_104[10];
char L_105[6];
INT64 L_106[1];
ADDRESS L_107[1];
INT64 L_108[1];
UINT8 L_109[11];
char L_110[5];
INT64 L_111[1];
ADDRESS L_112[1];
INT64 L_113[1];
UINT8 L_114[9];
char L_115[7];
INT64 L_116[1];
ADDRESS L_117[1];
INT64 L_118[1];
UINT8 L_119[10];
char L_120[6];
INT64 L_121[1];
ADDRESS L_122[1];
INT64 L_123[1];
UINT8 L_124[12];
char L_125[4];
INT64 L_126[1];
ADDRESS L_127[1];
INT64 L_128[1];
UINT8 L_129[12];
char L_130[4];
INT64 L_131[1];
ADDRESS L_132[1];
INT64 L_133[1];
UINT8 L_134[12];
char L_135[4];
INT64 L_136[1];
ADDRESS L_137[1];
INT64 L_138[1];
UINT8 L_139[10];
char L_140[6];
INT64 L_141[1];
ADDRESS L_142[1];
INT64 L_143[1];
UINT8 L_144[13];
char L_145[3];
INT64 L_146[1];
ADDRESS L_147[1];
INT64 L_148[1];
UINT8 L_149[4];
char L_150[4];
INT64 L_151[1];
ADDRESS L_152[1];
INT64 L_153[1];
UINT8 L_154[47];
char L_155[1];
INT64 L_156[1];
ADDRESS L_157[1];
INT64 L_158[1];
UINT8 L_159[11];
char L_160[5];
INT64 L_161[1];
ADDRESS L_162[1];
INT64 L_163[1];
UINT8 L_164[12];
char L_165[4];
INT64 L_166[1];
ADDRESS L_167[1];
INT64 L_168[1];
UINT8 L_169[10];
char L_170[6];
INT64 L_171[1];
ADDRESS L_172[1];
INT64 L_173[1];
UINT8 L_174[11];
char L_175[5];
INT64 L_176[1];
ADDRESS L_177[1];
INT64 L_178[1];
UINT8 L_179[13];
char L_180[3];
INT64 L_181[1];
ADDRESS L_182[1];
INT64 L_183[1];
UINT8 L_184[13];
char L_185[3];
INT64 L_186[1];
ADDRESS L_187[1];
INT64 L_188[1];
UINT8 L_189[13];
char L_190[3];
INT64 L_191[1];
ADDRESS L_192[1];
INT64 L_193[1];
UINT8 L_194[11];
char L_195[5];
INT64 L_196[1];
ADDRESS L_197[1];
INT64 L_198[1];
UINT8 L_199[14];
char L_200[2];
INT64 L_201[1];
ADDRESS L_202[1];
INT64 L_203[1];
UINT8 L_204[9];
char L_205[7];
INT64 L_206[1];
ADDRESS L_207[1];
INT64 L_208[1];
UINT8 L_209[10];
char L_210[6];
INT64 L_211[1];
ADDRESS L_212[1];
INT64 L_213[1];
UINT8 L_214[8];
char L_215[8];
INT64 L_216[1];
ADDRESS L_217[1];
INT64 L_218[1];
UINT8 L_219[9];
char L_220[7];
INT64 L_221[1];
ADDRESS L_222[1];
INT64 L_223[1];
UINT8 L_224[11];
char L_225[5];
INT64 L_226[1];
ADDRESS L_227[1];
INT64 L_228[1];
UINT8 L_229[11];
char L_230[5];
INT64 L_231[1];
ADDRESS L_232[1];
INT64 L_233[1];
UINT8 L_234[11];
char L_235[5];
INT64 L_236[1];
ADDRESS L_237[1];
INT64 L_238[1];
UINT8 L_239[9];
char L_240[7];
INT64 L_241[1];
ADDRESS L_242[1];
INT64 L_243[1];
UINT8 L_244[12];
char L_245[4];
UINT8 L_246[7];
char L_247[1];
UINT8 L_248[2];
char L_249[6];
ADDRESS L_250[4];
char L_251[8];
INT8 L_252[1];
UINT8 L_253[1];
INT8 L_254[2];
UINT8 L_255[10];
char L_256[2];
};
static  const Main_m_30_L_31_t Main_m_30_L_31={{(ADDRESS)&RTHooks__TextLitInfo,(ADDRESS)&RTHooks__TextLitGetChar,(ADDRESS)&RTHooks__TextLitGetWideChar,(ADDRESS)&RTHooks__TextLitGetChars,(ADDRESS)&RTHooks__TextLitGetWideChars},{INT64_(2)},{(char*)&Main_m_30_L_31},{INT64_(1)},{10},{0 /* 1 */ ,0 /* 2 */ ,0 /* 3 */ ,0 /* 4 */ ,0 /* 5 */ ,0 /* 6 */ ,0 /* 7 */ ,},{INT64_(2)},{(char*)&Main_m_30_L_31},{INT64_(3)},{'&','d',' '},{0 /* 1 */ ,0 /* 2 */ ,0 /* 3 */ ,0 /* 4 */ ,0 /* 5 */ ,},{INT64_(2)},{(char*)&Main_m_30_L_31},{INT64_(6)},{'&','y','e','a','r',' '},{0 /* 1 */ ,0 /* 2 */ ,},{INT64_(2)},{(char*)&Main_m_30_L_31},{INT64_(7)},{'&','m','o','n','t','h',' '},{0 /* 1 */ ,},{INT64_(2)},{(char*)&Main_m_30_L_31},{INT64_(5)},{'&','d','a','y',' '},{0 /* 1 */ ,0 /* 2 */ ,0 /* 3 */ ,},{INT64_(2)},{(char*)&Main_m_30_L_31},{INT64_(6)},{'&','h','o','u','r',' '},{0 /* 1 */ ,0 /* 2 */ ,},{INT64_(2)},{(char*)&Main_m_30_L_31},{INT64_(8)},{'&','m','i','n','u','t','e',' '},{0 /* 1 */ ,0 /* 2 */ ,0 /* 3 */ ,0 /* 4 */ 
,0 /* 5 */ ,0 /* 6 */ ,0 /* 7 */ ,0 /* 8 */ ,},{INT64_(2)},{(char*)&Main_m_30_L_31},{INT64_(8)},{'&','s','e','c','o','n','d',' '},{0 /* 1 */ ,0 /* 2 */ ,0 /* 3 */ ,0 /* 4 */ ,0 /* 5 */ ,0 /* 6 */ ,0 /* 7 */ ,0 /* 8 */ ,},{INT64_(2)},{(char*)&Main_m_30_L_31},{INT64_(8)},{'&','o','f','f','s','e','t',' '},{0 /* 1 */ ,0 /* 2 */ ,0 /* 3 */ ,0 /* 4 */ ,0 /* 5 */ ,0 /* 6 */ ,0 /* 7 */ ,0 /* 8 */ ,},{INT64_(2)},{(char*)&Main_m_30_L_31},{INT64_(6)},{'&','z','o','n','e',' '},{0 /* 1 */ ,0 /* 2 */ ,},{INT64_(2)},{(char*)&Main_m_30_L_31},{INT64_(9)},{'&','w','e','e','k','d','a','y',' '},{0 /* 1 */ ,0 /* 2 */ ,0 /* 3 */ ,0 /* 4 */ ,0 /* 5 */ ,0 /* 6 */ ,0 /* 7 */ ,},{INT64_(2)},{(char*)&Main_m_30_L_31},{INT64_(7)},{'s','i','z','e',' ','T',' '},{0 /* 1 */ ,},{INT64_(2)},{(char*)&Main_m_30_L_31},{INT64_(10)},{'s','i','z','e',' ','y','e','a','r',' '},{0 /* 1 */ ,0 /* 2 */ ,0 /* 3 */ ,0 /* 4 */ ,0 /* 5 */ ,0 /* 6 */ ,},{INT64_(2)},{(char*)&Main_m_30_L_31},{INT64_(11)},{'s','i','z','e',' ','m','o','n','t'
,'h',' '},{0 /* 1 */ ,0 /* 2 */ ,0 /* 3 */ ,0 /* 4 */ ,0 /* 5 */ ,},{INT64_(2)},{(char*)&Main_m_30_L_31},{INT64_(9)},{'s','i','z','e',' ','d','a','y',' '},{0 /* 1 */ ,0 /* 2 */ ,0 /* 3 */ ,0 /* 4 */ ,0 /* 5 */ ,0 /* 6 */ ,0 /* 7 */ ,},{INT64_(2)},{(char*)&Main_m_30_L_31},{INT64_(10)},{'s','i','z','e',' ','h','o','u','r',' '},{0 /* 1 */ ,0 /* 2 */ ,0 /* 3 */ ,0 /* 4 */ ,0 /* 5 */ ,0 /* 6 */ ,},{INT64_(2)},{(char*)&Main_m_30_L_31},{INT64_(12)},{'s','i','z','e',' ','m','i','n','u','t','e',' '},{0 /* 1 */ ,0 /* 2 */ ,0 /* 3 */ ,0 /* 4 */ ,},{INT64_(2)},{(char*)&Main_m_30_L_31},{INT64_(12)},{'s','i','z','e',' ','s','e','c','o','n','d',' '},{0 /* 1 */ ,0 /* 2 */ ,0 /* 3 */ ,0 /* 4 */ ,},{INT64_(2)},{(char*)&Main_m_30_L_31},{INT64_(12)},{'s','i','z','e',' ','o','f','f','s','e','t',' '},{0 /* 1 */ ,0 /* 2 */ ,0 /* 3 */ ,0 /* 4 */ ,},{INT64_(2)},{(char*)&Main_m_30_L_31},{INT64_(10)},{'s','i','z','e',' ','z','o','n','e',' '},{0 /* 1 */ ,0 /* 2 */ ,0 /* 3 */ ,0 /* 4 */ ,0 /* 5 */ ,0 /* 6 */ ,},{INT64_(2)
},{(char*)&Main_m_30_L_31},{INT64_(13)},{'s','i','z','e',' ','w','e','e','k','d','a','y',' '},{0 /* 1 */ ,0 /* 2 */ ,0 /* 3 */ ,},{INT64_(2)},{(char*)&Main_m_30_L_31},{INT64_(4)},{'n','o','w',' '},{0 /* 1 */ ,0 /* 2 */ ,0 /* 3 */ ,0 /* 4 */ ,},{INT64_(2)},{(char*)&Main_m_30_L_31},{INT64_(47)},{'o','r',' ','p','o','s','s','i','b','l','y',' ','t','h','e',' ','p','o','s','i','x',' ','v','a','l','u','e',' ','p','r','i','n','t','e','d',' ','f','r','o','m',' ','W','i','n','3','2',' '},{0 /* 1 */ ,},{INT64_(2)},{(char*)&Main_m_30_L_31},{INT64_(11)},{'l','o','c','a','l',' ','y','e','a','r',' '},{0 /* 1 */ ,0 /* 2 */ ,0 /* 3 */ ,0 /* 4 */ ,0 /* 5 */ ,},{INT64_(2)},{(char*)&Main_m_30_L_31},{INT64_(12)},{'l','o','c','a','l',' ','m','o','n','t','h',' '},{0 /* 1 */ ,0 /* 2 */ ,0 /* 3 */ ,0 /* 4 */ ,},{INT64_(2)},{(char*)&Main_m_30_L_31},{INT64_(10)},{'l','o','c','a','l',' ','d','a','y',' '},{0 /* 1 */ ,0 /* 2 */ ,0 /* 3 */ ,0 /* 4 */ ,0 /* 5 */ ,0 /* 6 */ ,},{INT64_(2)},{(char*)&Main_m_30_L_31},{INT64_(11)
},{'l','o','c','a','l',' ','h','o','u','r',' '},{0 /* 1 */ ,0 /* 2 */ ,0 /* 3 */ ,0 /* 4 */ ,0 /* 5 */ ,},{INT64_(2)},{(char*)&Main_m_30_L_31},{INT64_(13)},{'l','o','c','a','l',' ','m','i','n','u','t','e',' '},{0 /* 1 */ ,0 /* 2 */ ,0 /* 3 */ ,},{INT64_(2)},{(char*)&Main_m_30_L_31},{INT64_(13)},{'l','o','c','a','l',' ','s','e','c','o','n','d',' '},{0 /* 1 */ ,0 /* 2 */ ,0 /* 3 */ ,},{INT64_(2)},{(char*)&Main_m_30_L_31},{INT64_(13)},{'l','o','c','a','l',' ','o','f','f','s','e','t',' '},{0 /* 1 */ ,0 /* 2 */ ,0 /* 3 */ ,},{INT64_(2)},{(char*)&Main_m_30_L_31},{INT64_(11)},{'l','o','c','a','l',' ','z','o','n','e',' '},{0 /* 1 */ ,0 /* 2 */ ,0 /* 3 */ ,0 /* 4 */ ,0 /* 5 */ ,},{INT64_(2)},{(char*)&Main_m_30_L_31},{INT64_(14)},{'l','o','c','a','l',' ','w','e','e','k','d','a','y',' '},{0 /* 1 */ ,0 /* 2 */ ,},{INT64_(2)},{(char*)&Main_m_30_L_31},{INT64_(9)},{'u','t','c',' ','y','e','a','r',' '},{0 /* 1 */ ,0 /* 2 */ ,0 /* 3 */ ,0 /* 4 */ ,0 /* 5 */ ,0 /* 6 */ ,0 /* 7 */ ,},{INT64_(2)},{(char*)&Main_m_30_L_31
},{INT64_(10)},{'u','t','c',' ','m','o','n','t','h',' '},{0 /* 1 */ ,0 /* 2 */ ,0 /* 3 */ ,0 /* 4 */ ,0 /* 5 */ ,0 /* 6 */ ,},{INT64_(2)},{(char*)&Main_m_30_L_31},{INT64_(8)},{'u','t','c',' ','d','a','y',' '},{0 /* 1 */ ,0 /* 2 */ ,0 /* 3 */ ,0 /* 4 */ ,0 /* 5 */ ,0 /* 6 */ ,0 /* 7 */ ,0 /* 8 */ ,},{INT64_(2)},{(char*)&Main_m_30_L_31},{INT64_(9)},{'u','t','c',' ','h','o','u','r',' '},{0 /* 1 */ ,0 /* 2 */ ,0 /* 3 */ ,0 /* 4 */ ,0 /* 5 */ ,0 /* 6 */ ,0 /* 7 */ ,},{INT64_(2)},{(char*)&Main_m_30_L_31},{INT64_(11)},{'u','t','c',' ','m','i','n','u','t','e',' '},{0 /* 1 */ ,0 /* 2 */ ,0 /* 3 */ ,0 /* 4 */ ,0 /* 5 */ ,},{INT64_(2)},{(char*)&Main_m_30_L_31},{INT64_(11)},{'u','t','c',' ','s','e','c','o','n','d',' '},{0 /* 1 */ ,0 /* 2 */ ,0 /* 3 */ ,0 /* 4 */ ,0 /* 5 */ ,},{INT64_(2)},{(char*)&Main_m_30_L_31},{INT64_(11)},{'u','t','c',' ','o','f','f','s','e','t',' '},{0 /* 1 */ ,0 /* 2 */ ,0 /* 3 */ ,0 /* 4 */ ,0 /* 5 */ ,},{INT64_(2)},{(char*)&Main_m_30_L_31},{INT64_(9)},{'u','t','c',' ','z','o'
,'n','e',' '},{0 /* 1 */ ,0 /* 2 */ ,0 /* 3 */ ,0 /* 4 */ ,0 /* 5 */ ,0 /* 6 */ ,0 /* 7 */ ,},{INT64_(2)},{(char*)&Main_m_30_L_31},{INT64_(12)},{'u','t','c',' ','w','e','e','k','d','a','y',' '},{0 /* 1 */ ,0 /* 2 */ ,0 /* 3 */ ,0 /* 4 */ ,},{'M','a','i','n','_','M','3'},{0 /* 1 */ ,},{'N','L'},{0 /* 1 */ ,0 /* 2 */ ,0 /* 3 */ ,0 /* 4 */ ,0 /* 5 */ ,0 /* 6 */ ,},{(ADDRESS)&Main_M3,1640+(char*)&Main_m_30_L_31,(ADDRESS)&Main__NL,1648+(char*)&Main_m_30_L_31},{0 /* 1 */ ,0 /* 2 */ ,0 /* 3 */ ,0 /* 4 */ ,0 /* 5 */ ,0 /* 6 */ ,0 /* 7 */ ,0 /* 8 */ ,},{((INT8)42)},{128U},{((INT8)4),((INT8)0)},{'.','.','/','M','a','i','n','.','m','3'},{0 /* 1 */ ,0 /* 2 */ ,}};
 /* bind_segment */
 /* begin_init */
 /* init_var */
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
 /* init_var */
 /* init_proc */
 /* init_var */
 /* init_proc */
 /* end_init */
struct Main_m_M_Main_L_32_t{ADDRESS L_257[1];
char L_258[32];
ADDRESS L_259[1];
char L_260[8];
ADDRESS L_261[3];
char L_262[8];
ADDRESS L_263[1];
INT64 L_264[1];
char L_265[48];
ADDRESS L_266[2];
char L_267[8];
ADDRESS L_268[2];
char L_269[8];
ADDRESS L_270[2];
char L_271[8];
ADDRESS L_272[2];
char L_273[8];
ADDRESS L_274[1];
char L_275[16];
};
static Main_m_M_Main_L_32_t Main_m_M_Main_L_32={{1700+(char*)&Main_m_30_L_31},{0 /* 1 */ ,0 /* 2 */ ,0 /* 3 */ ,0 /* 4 */ ,0 /* 5 */ ,0 /* 6 */ ,0 /* 7 */ ,0 /* 8 */ ,0 /* 9 */ ,0 /* 10 */ ,0 /* 11 */ ,0 /* 12 */ ,0 /* 13 */ ,0 /* 14 */ ,0 /* 15 */ ,0 /* 16 */ ,0 /* 17 */ ,0 /* 18 */ ,0 /* 19 */ ,0 /* 20 */ ,0 /* 21 */ ,0 /* 22 */ ,0 /* 23 */ ,0 /* 24 */ ,0 /* 25 */ ,0 /* 26 */ ,0 /* 27 */ ,0 /* 28 */ ,0 /* 29 */ ,0 /* 30 */ ,0 /* 31 */ ,0 /* 32 */ ,},{1656+(char*)&Main_m_30_L_31},{0 /* 1 */ ,0 /* 2 */ ,0 /* 3 */ ,0 /* 4 */ ,0 /* 5 */ ,0 /* 6 */ ,0 /* 7 */ ,0 /* 8 */ ,},{1696+(char*)&Main_m_30_L_31,1696+(char*)&Main_m_30_L_31,144+(char*)&Main_m_M_Main_L_32},{0 /* 1 */ ,0 /* 2 */ ,0 /* 3 */ ,0 /* 4 */ ,0 /* 5 */ ,0 /* 6 */ ,0 /* 7 */ ,0 /* 8 */ ,},{(ADDRESS)&Main_M3},{INT64_(3)},{0 /* 1 */ ,0 /* 2 */ ,0 /* 3 */ ,0 /* 4 */ ,0 /* 5 */ ,0 /* 6 */ ,0 /* 7 */ ,0 /* 8 */ ,0 /* 9 */ ,0 /* 10 */ ,0 /* 11 */ ,0 /* 12 */ ,0 /* 13 */ ,0 /* 14 */ ,0 /* 15 */ ,0 /* 16 */ ,0 /* 17 */ ,0 /* 18 */ ,0 /* 19 */ 
,0 /* 20 */ ,0 /* 21 */ ,0 /* 22 */ ,0 /* 23 */ ,0 /* 24 */ ,0 /* 25 */ ,0 /* 26 */ ,0 /* 27 */ ,0 /* 28 */ ,0 /* 29 */ ,0 /* 30 */ ,0 /* 31 */ ,0 /* 32 */ ,0 /* 33 */ ,0 /* 34 */ ,0 /* 35 */ ,0 /* 36 */ ,0 /* 37 */ ,0 /* 38 */ ,0 /* 39 */ ,0 /* 40 */ ,0 /* 41 */ ,0 /* 42 */ ,0 /* 43 */ ,0 /* 44 */ ,0 /* 45 */ ,0 /* 46 */ ,0 /* 47 */ ,0 /* 48 */ ,},{(ADDRESS)&Main_I3,168+(char*)&Main_m_M_Main_L_32},{0 /* 1 */ ,0 /* 2 */ ,0 /* 3 */ ,0 /* 4 */ ,0 /* 5 */ ,0 /* 6 */ ,0 /* 7 */ ,0 /* 8 */ ,},{(ADDRESS)&Time_I3,192+(char*)&Main_m_M_Main_L_32},{0 /* 1 */ ,0 /* 2 */ ,0 /* 3 */ ,0 /* 4 */ ,0 /* 5 */ ,0 /* 6 */ ,0 /* 7 */ ,0 /* 8 */ ,},{(ADDRESS)&RTIO_I3,216+(char*)&Main_m_M_Main_L_32},{0 /* 1 */ ,0 /* 2 */ ,0 /* 3 */ ,0 /* 4 */ ,0 /* 5 */ ,0 /* 6 */ ,0 /* 7 */ ,0 /* 8 */ ,},{(ADDRESS)&Date_I3,240+(char*)&Main_m_M_Main_L_32},{0 /* 1 */ ,0 /* 2 */ ,0 /* 3 */ ,0 /* 4 */ ,0 /* 5 */ ,0 /* 6 */ ,0 /* 7 */ ,0 /* 8 */ ,},{(ADDRESS)&RTHooks_I3},{0 /* 1 */ ,0 /* 2 */ ,0 /* 3 */ ,0 /* 4 */ ,0 /* 5 */ ,0 /* 6 */ 
,0 /* 7 */ ,0 /* 8 */ ,0 /* 9 */ ,0 /* 10 */ ,0 /* 11 */ ,0 /* 12 */ ,0 /* 13 */ ,0 /* 14 */ ,0 /* 15 */ ,0 /* 16 */ ,}};
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
#line 18 "../Main.m3"
 /* NL */
#line 18 "../Main.m3"
 /* set_source_line */
#line 18 "../Main.m3"
#line 12 "../Main.m3"
 /* begin_procedure */
#line 12 "../Main.m3"
struct Main__NL_Frame_t {
#line 12 "../Main.m3"
ADDRESS _unused;
#line 12 "../Main.m3"
};
#line 12 "../Main.m3"
void /*TypeText3*/ 
__cdecl
Main__NL(void)
{
#line 12 "../Main.m3"
Main__NL_Frame_t _frame;
#line 12 "../Main.m3"
_frame._unused=(ADDRESS)&_frame;
#line 12 "../Main.m3"
 /* set_source_line */
#line 12 "../Main.m3"
#line 13 "../Main.m3"
 /* set_source_line */
#line 13 "../Main.m3"
#line 14 "../Main.m3"
 /* start_call_direct */
#line 14 "../Main.m3"
 /* load_address */
#line 14 "../Main.m3"
 /* pop_param */
#line 14 "../Main.m3"
 /* call_direct */
#line 14 "../Main.m3"
RTIO__PutText(
  ( TEXT )(((ADDRESS)(INT64_(48)+((ADDRESS)(&Main_m_30_L_31)))) ));
#line 14 "../Main.m3"
 /* set_source_line */
#line 14 "../Main.m3"
#line 15 "../Main.m3"
 /* start_call_direct */
#line 15 "../Main.m3"
 /* call_direct */
#line 15 "../Main.m3"
RTIO__Flush(
 );
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
} /* Main_M3 */
#line 16 "../Main.m3"
 /* module main body Main_M3 */
#line 16 "../Main.m3"
 /* set_source_line */
#line 16 "../Main.m3"
#line 18 "../Main.m3"
 /* begin_procedure */
#line 18 "../Main.m3"
struct Main_M3_Frame_t {
#line 18 "../Main.m3"
ADDRESS _unused;
#line 18 "../Main.m3"
};
#line 18 "../Main.m3"
RT0__ModulePtr
__cdecl
Main_M3(
   /* Param_Type1 */ INTEGER /*TypeText1*/  mode_L_33)
{
#line 18 "../Main.m3"
 /* Var_Type2 */ double Main_m_34_L_35={0};//always-init
#line 18 "../Main.m3"
 /* Var_Type2 */ ADDRESS Main_m_36_L_37={0};//always-init
#line 18 "../Main.m3"
 /* Var_Type3 */ STRUCT(40) Main_m_38_L_39={0};//always-init
#line 18 "../Main.m3"
Main_M3_Frame_t _frame;
#line 18 "../Main.m3"
_frame._unused=(ADDRESS)&_frame;
#line 18 "../Main.m3"
 /* load */
#line 18 "../Main.m3"
 /* if_true_or_false */
#line 18 "../Main.m3"
 /* load_host_integer */
#line 18 "../Main.m3"
 /* load_integer */
#line 18 "../Main.m3"
 /* if_compare */
#line 18 "../Main.m3"
if(m3_eq(INT64,
  mode_L_33,
   INT64_(0)))goto L1;
#line 18 "../Main.m3"
 /* set_source_line */
#line 18 "../Main.m3"
#line 4 "../Main.m3"
 /* load_integer */
#line 4 "../Main.m3"
 /* store */
#line 4 "../Main.m3"
(*(INT64*)((104)+(char*)(&Main_m_M_Main_L_32)))=(INT64)(  INT64_(0));
#line 4 "../Main.m3"
 /* load_integer */
#line 4 "../Main.m3"
 /* store */
#line 4 "../Main.m3"
(*(INT64*)((112)+(char*)(&Main_m_M_Main_L_32)))=(INT64)(  INT64_(0));
#line 4 "../Main.m3"
 /* load_integer */
#line 4 "../Main.m3"
 /* store */
#line 4 "../Main.m3"
(*(INT64*)((120)+(char*)(&Main_m_M_Main_L_32)))=(INT64)(  INT64_(0));
#line 4 "../Main.m3"
 /* load_integer */
#line 4 "../Main.m3"
 /* store */
#line 4 "../Main.m3"
(*(INT64*)((128)+(char*)(&Main_m_M_Main_L_32)))=(INT64)(  INT64_(0));
#line 4 "../Main.m3"
 /* load_integer */
#line 4 "../Main.m3"
 /* store */
#line 4 "../Main.m3"
(*(INT64*)((136)+(char*)(&Main_m_M_Main_L_32)))=(INT64)(  INT64_(0));
#line 4 "../Main.m3"
 /* load_integer */
#line 4 "../Main.m3"
 /* store */
#line 4 "../Main.m3"
(*(UINT8*)((113)+(char*)(&Main_m_M_Main_L_32)))=(INT64)(  INT64_(1));
#line 4 "../Main.m3"
 /* set_source_line */
#line 4 "../Main.m3"
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
  ( TEXT )(((ADDRESS)(INT64_(80)+((ADDRESS)(&Main_m_30_L_31)))) ));
#line 19 "../Main.m3"
 /* start_call_direct */
#line 19 "../Main.m3"
 /* load_address */
#line 19 "../Main.m3"
 /* pop_param */
#line 19 "../Main.m3"
 /* load_integer */
#line 19 "../Main.m3"
 /* pop_param */
#line 19 "../Main.m3"
 /* call_direct */
#line 19 "../Main.m3"
RTIO__PutAddr(
  ( ADDRESS )(((ADDRESS)(INT64_(104)+((ADDRESS)(&Main_m_M_Main_L_32)))) ),
  ( INTEGER )(  INT64_(0) ));
#line 19 "../Main.m3"
 /* start_call_direct */
#line 19 "../Main.m3"
 /* call_direct */
#line 19 "../Main.m3"
Main__NL(
 );
#line 19 "../Main.m3"
 /* set_source_line */
#line 19 "../Main.m3"
#line 20 "../Main.m3"
 /* start_call_direct */
#line 20 "../Main.m3"
 /* load_address */
#line 20 "../Main.m3"
 /* pop_param */
#line 20 "../Main.m3"
 /* call_direct */
#line 20 "../Main.m3"
RTIO__PutText(
  ( TEXT )(((ADDRESS)(INT64_(112)+((ADDRESS)(&Main_m_30_L_31)))) ));
#line 20 "../Main.m3"
 /* start_call_direct */
#line 20 "../Main.m3"
 /* load_address */
#line 20 "../Main.m3"
 /* pop_param */
#line 20 "../Main.m3"
 /* load_integer */
#line 20 "../Main.m3"
 /* pop_param */
#line 20 "../Main.m3"
 /* call_direct */
#line 20 "../Main.m3"
RTIO__PutAddr(
  ( ADDRESS )(((ADDRESS)(INT64_(104)+((ADDRESS)(&Main_m_M_Main_L_32)))) ),
  ( INTEGER )(  INT64_(0) ));
#line 20 "../Main.m3"
 /* start_call_direct */
#line 20 "../Main.m3"
 /* call_direct */
#line 20 "../Main.m3"
Main__NL(
 );
#line 20 "../Main.m3"
 /* set_source_line */
#line 20 "../Main.m3"
#line 21 "../Main.m3"
 /* start_call_direct */
#line 21 "../Main.m3"
 /* load_address */
#line 21 "../Main.m3"
 /* pop_param */
#line 21 "../Main.m3"
 /* call_direct */
#line 21 "../Main.m3"
RTIO__PutText(
  ( TEXT )(((ADDRESS)(INT64_(144)+((ADDRESS)(&Main_m_30_L_31)))) ));
#line 21 "../Main.m3"
 /* start_call_direct */
#line 21 "../Main.m3"
 /* load_address */
#line 21 "../Main.m3"
 /* pop_param */
#line 21 "../Main.m3"
 /* load_integer */
#line 21 "../Main.m3"
 /* pop_param */
#line 21 "../Main.m3"
 /* call_direct */
#line 21 "../Main.m3"
RTIO__PutAddr(
  ( ADDRESS )(((ADDRESS)(INT64_(112)+((ADDRESS)(&Main_m_M_Main_L_32)))) ),
  ( INTEGER )(  INT64_(0) ));
#line 21 "../Main.m3"
 /* start_call_direct */
#line 21 "../Main.m3"
 /* call_direct */
#line 21 "../Main.m3"
Main__NL(
 );
#line 21 "../Main.m3"
 /* set_source_line */
#line 21 "../Main.m3"
#line 22 "../Main.m3"
 /* start_call_direct */
#line 22 "../Main.m3"
 /* load_address */
#line 22 "../Main.m3"
 /* pop_param */
#line 22 "../Main.m3"
 /* call_direct */
#line 22 "../Main.m3"
RTIO__PutText(
  ( TEXT )(((ADDRESS)(INT64_(176)+((ADDRESS)(&Main_m_30_L_31)))) ));
#line 22 "../Main.m3"
 /* start_call_direct */
#line 22 "../Main.m3"
 /* load_address */
#line 22 "../Main.m3"
 /* pop_param */
#line 22 "../Main.m3"
 /* load_integer */
#line 22 "../Main.m3"
 /* pop_param */
#line 22 "../Main.m3"
 /* call_direct */
#line 22 "../Main.m3"
RTIO__PutAddr(
  ( ADDRESS )(((ADDRESS)(INT64_(113)+((ADDRESS)(&Main_m_M_Main_L_32)))) ),
  ( INTEGER )(  INT64_(0) ));
#line 22 "../Main.m3"
 /* start_call_direct */
#line 22 "../Main.m3"
 /* call_direct */
#line 22 "../Main.m3"
Main__NL(
 );
#line 22 "../Main.m3"
 /* set_source_line */
#line 22 "../Main.m3"
#line 23 "../Main.m3"
 /* start_call_direct */
#line 23 "../Main.m3"
 /* load_address */
#line 23 "../Main.m3"
 /* pop_param */
#line 23 "../Main.m3"
 /* call_direct */
#line 23 "../Main.m3"
RTIO__PutText(
  ( TEXT )(((ADDRESS)(INT64_(208)+((ADDRESS)(&Main_m_30_L_31)))) ));
#line 23 "../Main.m3"
 /* start_call_direct */
#line 23 "../Main.m3"
 /* load_address */
#line 23 "../Main.m3"
 /* pop_param */
#line 23 "../Main.m3"
 /* load_integer */
#line 23 "../Main.m3"
 /* pop_param */
#line 23 "../Main.m3"
 /* call_direct */
#line 23 "../Main.m3"
RTIO__PutAddr(
  ( ADDRESS )(((ADDRESS)(INT64_(114)+((ADDRESS)(&Main_m_M_Main_L_32)))) ),
  ( INTEGER )(  INT64_(0) ));
#line 23 "../Main.m3"
 /* start_call_direct */
#line 23 "../Main.m3"
 /* call_direct */
#line 23 "../Main.m3"
Main__NL(
 );
#line 23 "../Main.m3"
 /* set_source_line */
#line 23 "../Main.m3"
#line 24 "../Main.m3"
 /* start_call_direct */
#line 24 "../Main.m3"
 /* load_address */
#line 24 "../Main.m3"
 /* pop_param */
#line 24 "../Main.m3"
 /* call_direct */
#line 24 "../Main.m3"
RTIO__PutText(
  ( TEXT )(((ADDRESS)(INT64_(240)+((ADDRESS)(&Main_m_30_L_31)))) ));
#line 24 "../Main.m3"
 /* start_call_direct */
#line 24 "../Main.m3"
 /* load_address */
#line 24 "../Main.m3"
 /* pop_param */
#line 24 "../Main.m3"
 /* load_integer */
#line 24 "../Main.m3"
 /* pop_param */
#line 24 "../Main.m3"
 /* call_direct */
#line 24 "../Main.m3"
RTIO__PutAddr(
  ( ADDRESS )(((ADDRESS)(INT64_(115)+((ADDRESS)(&Main_m_M_Main_L_32)))) ),
  ( INTEGER )(  INT64_(0) ));
#line 24 "../Main.m3"
 /* start_call_direct */
#line 24 "../Main.m3"
 /* call_direct */
#line 24 "../Main.m3"
Main__NL(
 );
#line 24 "../Main.m3"
 /* set_source_line */
#line 24 "../Main.m3"
#line 25 "../Main.m3"
 /* start_call_direct */
#line 25 "../Main.m3"
 /* load_address */
#line 25 "../Main.m3"
 /* pop_param */
#line 25 "../Main.m3"
 /* call_direct */
#line 25 "../Main.m3"
RTIO__PutText(
  ( TEXT )(((ADDRESS)(INT64_(280)+((ADDRESS)(&Main_m_30_L_31)))) ));
#line 25 "../Main.m3"
 /* start_call_direct */
#line 25 "../Main.m3"
 /* load_address */
#line 25 "../Main.m3"
 /* pop_param */
#line 25 "../Main.m3"
 /* load_integer */
#line 25 "../Main.m3"
 /* pop_param */
#line 25 "../Main.m3"
 /* call_direct */
#line 25 "../Main.m3"
RTIO__PutAddr(
  ( ADDRESS )(((ADDRESS)(INT64_(116)+((ADDRESS)(&Main_m_M_Main_L_32)))) ),
  ( INTEGER )(  INT64_(0) ));
#line 25 "../Main.m3"
 /* start_call_direct */
#line 25 "../Main.m3"
 /* call_direct */
#line 25 "../Main.m3"
Main__NL(
 );
#line 25 "../Main.m3"
 /* set_source_line */
#line 25 "../Main.m3"
#line 26 "../Main.m3"
 /* start_call_direct */
#line 26 "../Main.m3"
 /* load_address */
#line 26 "../Main.m3"
 /* pop_param */
#line 26 "../Main.m3"
 /* call_direct */
#line 26 "../Main.m3"
RTIO__PutText(
  ( TEXT )(((ADDRESS)(INT64_(320)+((ADDRESS)(&Main_m_30_L_31)))) ));
#line 26 "../Main.m3"
 /* start_call_direct */
#line 26 "../Main.m3"
 /* load_address */
#line 26 "../Main.m3"
 /* pop_param */
#line 26 "../Main.m3"
 /* load_integer */
#line 26 "../Main.m3"
 /* pop_param */
#line 26 "../Main.m3"
 /* call_direct */
#line 26 "../Main.m3"
RTIO__PutAddr(
  ( ADDRESS )(((ADDRESS)(INT64_(120)+((ADDRESS)(&Main_m_M_Main_L_32)))) ),
  ( INTEGER )(  INT64_(0) ));
#line 26 "../Main.m3"
 /* start_call_direct */
#line 26 "../Main.m3"
 /* call_direct */
#line 26 "../Main.m3"
Main__NL(
 );
#line 26 "../Main.m3"
 /* set_source_line */
#line 26 "../Main.m3"
#line 27 "../Main.m3"
 /* start_call_direct */
#line 27 "../Main.m3"
 /* load_address */
#line 27 "../Main.m3"
 /* pop_param */
#line 27 "../Main.m3"
 /* call_direct */
#line 27 "../Main.m3"
RTIO__PutText(
  ( TEXT )(((ADDRESS)(INT64_(360)+((ADDRESS)(&Main_m_30_L_31)))) ));
#line 27 "../Main.m3"
 /* start_call_direct */
#line 27 "../Main.m3"
 /* load_address */
#line 27 "../Main.m3"
 /* pop_param */
#line 27 "../Main.m3"
 /* load_integer */
#line 27 "../Main.m3"
 /* pop_param */
#line 27 "../Main.m3"
 /* call_direct */
#line 27 "../Main.m3"
RTIO__PutAddr(
  ( ADDRESS )(((ADDRESS)(INT64_(128)+((ADDRESS)(&Main_m_M_Main_L_32)))) ),
  ( INTEGER )(  INT64_(0) ));
#line 27 "../Main.m3"
 /* start_call_direct */
#line 27 "../Main.m3"
 /* call_direct */
#line 27 "../Main.m3"
Main__NL(
 );
#line 27 "../Main.m3"
 /* set_source_line */
#line 27 "../Main.m3"
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
  ( TEXT )(((ADDRESS)(INT64_(392)+((ADDRESS)(&Main_m_30_L_31)))) ));
#line 28 "../Main.m3"
 /* start_call_direct */
#line 28 "../Main.m3"
 /* load_address */
#line 28 "../Main.m3"
 /* pop_param */
#line 28 "../Main.m3"
 /* load_integer */
#line 28 "../Main.m3"
 /* pop_param */
#line 28 "../Main.m3"
 /* call_direct */
#line 28 "../Main.m3"
RTIO__PutAddr(
  ( ADDRESS )(((ADDRESS)(INT64_(136)+((ADDRESS)(&Main_m_M_Main_L_32)))) ),
  ( INTEGER )(  INT64_(0) ));
#line 28 "../Main.m3"
 /* start_call_direct */
#line 28 "../Main.m3"
 /* call_direct */
#line 28 "../Main.m3"
Main__NL(
 );
#line 28 "../Main.m3"
 /* set_source_line */
#line 28 "../Main.m3"
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
  ( TEXT )(((ADDRESS)(INT64_(432)+((ADDRESS)(&Main_m_30_L_31)))) ));
#line 30 "../Main.m3"
 /* start_call_direct */
#line 30 "../Main.m3"
 /* load_integer */
#line 30 "../Main.m3"
 /* pop_param */
#line 30 "../Main.m3"
 /* load_integer */
#line 30 "../Main.m3"
 /* pop_param */
#line 30 "../Main.m3"
 /* call_direct */
#line 30 "../Main.m3"
RTIO__PutInt(
  ( INTEGER )(  INT64_(40) ),
  ( INTEGER )(  INT64_(0) ));
#line 30 "../Main.m3"
 /* start_call_direct */
#line 30 "../Main.m3"
 /* call_direct */
#line 30 "../Main.m3"
Main__NL(
 );
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
 /* call_direct */
#line 31 "../Main.m3"
RTIO__PutText(
  ( TEXT )(((ADDRESS)(INT64_(464)+((ADDRESS)(&Main_m_30_L_31)))) ));
#line 31 "../Main.m3"
 /* start_call_direct */
#line 31 "../Main.m3"
 /* load_integer */
#line 31 "../Main.m3"
 /* pop_param */
#line 31 "../Main.m3"
 /* load_integer */
#line 31 "../Main.m3"
 /* pop_param */
#line 31 "../Main.m3"
 /* call_direct */
#line 31 "../Main.m3"
RTIO__PutInt(
  ( INTEGER )(  INT64_(8) ),
  ( INTEGER )(  INT64_(0) ));
#line 31 "../Main.m3"
 /* start_call_direct */
#line 31 "../Main.m3"
 /* call_direct */
#line 31 "../Main.m3"
Main__NL(
 );
#line 31 "../Main.m3"
 /* set_source_line */
#line 31 "../Main.m3"
#line 32 "../Main.m3"
 /* start_call_direct */
#line 32 "../Main.m3"
 /* load_address */
#line 32 "../Main.m3"
 /* pop_param */
#line 32 "../Main.m3"
 /* call_direct */
#line 32 "../Main.m3"
RTIO__PutText(
  ( TEXT )(((ADDRESS)(INT64_(504)+((ADDRESS)(&Main_m_30_L_31)))) ));
#line 32 "../Main.m3"
 /* start_call_direct */
#line 32 "../Main.m3"
 /* load_integer */
#line 32 "../Main.m3"
 /* pop_param */
#line 32 "../Main.m3"
 /* load_integer */
#line 32 "../Main.m3"
 /* pop_param */
#line 32 "../Main.m3"
 /* call_direct */
#line 32 "../Main.m3"
RTIO__PutInt(
  ( INTEGER )(  INT64_(1) ),
  ( INTEGER )(  INT64_(0) ));
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
 /* start_call_direct */
#line 33 "../Main.m3"
 /* load_address */
#line 33 "../Main.m3"
 /* pop_param */
#line 33 "../Main.m3"
 /* call_direct */
#line 33 "../Main.m3"
RTIO__PutText(
  ( TEXT )(((ADDRESS)(INT64_(544)+((ADDRESS)(&Main_m_30_L_31)))) ));
#line 33 "../Main.m3"
 /* start_call_direct */
#line 33 "../Main.m3"
 /* load_integer */
#line 33 "../Main.m3"
 /* pop_param */
#line 33 "../Main.m3"
 /* load_integer */
#line 33 "../Main.m3"
 /* pop_param */
#line 33 "../Main.m3"
 /* call_direct */
#line 33 "../Main.m3"
RTIO__PutInt(
  ( INTEGER )(  INT64_(1) ),
  ( INTEGER )(  INT64_(0) ));
#line 33 "../Main.m3"
 /* start_call_direct */
#line 33 "../Main.m3"
 /* call_direct */
#line 33 "../Main.m3"
Main__NL(
 );
#line 33 "../Main.m3"
 /* set_source_line */
#line 33 "../Main.m3"
#line 34 "../Main.m3"
 /* start_call_direct */
#line 34 "../Main.m3"
 /* load_address */
#line 34 "../Main.m3"
 /* pop_param */
#line 34 "../Main.m3"
 /* call_direct */
#line 34 "../Main.m3"
RTIO__PutText(
  ( TEXT )(((ADDRESS)(INT64_(584)+((ADDRESS)(&Main_m_30_L_31)))) ));
#line 34 "../Main.m3"
 /* start_call_direct */
#line 34 "../Main.m3"
 /* load_integer */
#line 34 "../Main.m3"
 /* pop_param */
#line 34 "../Main.m3"
 /* load_integer */
#line 34 "../Main.m3"
 /* pop_param */
#line 34 "../Main.m3"
 /* call_direct */
#line 34 "../Main.m3"
RTIO__PutInt(
  ( INTEGER )(  INT64_(1) ),
  ( INTEGER )(  INT64_(0) ));
#line 34 "../Main.m3"
 /* start_call_direct */
#line 34 "../Main.m3"
 /* call_direct */
#line 34 "../Main.m3"
Main__NL(
 );
#line 34 "../Main.m3"
 /* set_source_line */
#line 34 "../Main.m3"
#line 35 "../Main.m3"
 /* start_call_direct */
#line 35 "../Main.m3"
 /* load_address */
#line 35 "../Main.m3"
 /* pop_param */
#line 35 "../Main.m3"
 /* call_direct */
#line 35 "../Main.m3"
RTIO__PutText(
  ( TEXT )(((ADDRESS)(INT64_(624)+((ADDRESS)(&Main_m_30_L_31)))) ));
#line 35 "../Main.m3"
 /* start_call_direct */
#line 35 "../Main.m3"
 /* load_integer */
#line 35 "../Main.m3"
 /* pop_param */
#line 35 "../Main.m3"
 /* load_integer */
#line 35 "../Main.m3"
 /* pop_param */
#line 35 "../Main.m3"
 /* call_direct */
#line 35 "../Main.m3"
RTIO__PutInt(
  ( INTEGER )(  INT64_(1) ),
  ( INTEGER )(  INT64_(0) ));
#line 35 "../Main.m3"
 /* start_call_direct */
#line 35 "../Main.m3"
 /* call_direct */
#line 35 "../Main.m3"
Main__NL(
 );
#line 35 "../Main.m3"
 /* set_source_line */
#line 35 "../Main.m3"
#line 36 "../Main.m3"
 /* start_call_direct */
#line 36 "../Main.m3"
 /* load_address */
#line 36 "../Main.m3"
 /* pop_param */
#line 36 "../Main.m3"
 /* call_direct */
#line 36 "../Main.m3"
RTIO__PutText(
  ( TEXT )(((ADDRESS)(INT64_(664)+((ADDRESS)(&Main_m_30_L_31)))) ));
#line 36 "../Main.m3"
 /* start_call_direct */
#line 36 "../Main.m3"
 /* load_integer */
#line 36 "../Main.m3"
 /* pop_param */
#line 36 "../Main.m3"
 /* load_integer */
#line 36 "../Main.m3"
 /* pop_param */
#line 36 "../Main.m3"
 /* call_direct */
#line 36 "../Main.m3"
RTIO__PutInt(
  ( INTEGER )(  INT64_(1) ),
  ( INTEGER )(  INT64_(0) ));
#line 36 "../Main.m3"
 /* start_call_direct */
#line 36 "../Main.m3"
 /* call_direct */
#line 36 "../Main.m3"
Main__NL(
 );
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
 /* call_direct */
#line 37 "../Main.m3"
RTIO__PutText(
  ( TEXT )(((ADDRESS)(INT64_(704)+((ADDRESS)(&Main_m_30_L_31)))) ));
#line 37 "../Main.m3"
 /* start_call_direct */
#line 37 "../Main.m3"
 /* load_integer */
#line 37 "../Main.m3"
 /* pop_param */
#line 37 "../Main.m3"
 /* load_integer */
#line 37 "../Main.m3"
 /* pop_param */
#line 37 "../Main.m3"
 /* call_direct */
#line 37 "../Main.m3"
RTIO__PutInt(
  ( INTEGER )(  INT64_(8) ),
  ( INTEGER )(  INT64_(0) ));
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
 /* start_call_direct */
#line 38 "../Main.m3"
 /* load_address */
#line 38 "../Main.m3"
 /* pop_param */
#line 38 "../Main.m3"
 /* call_direct */
#line 38 "../Main.m3"
RTIO__PutText(
  ( TEXT )(((ADDRESS)(INT64_(744)+((ADDRESS)(&Main_m_30_L_31)))) ));
#line 38 "../Main.m3"
 /* start_call_direct */
#line 38 "../Main.m3"
 /* load_integer */
#line 38 "../Main.m3"
 /* pop_param */
#line 38 "../Main.m3"
 /* load_integer */
#line 38 "../Main.m3"
 /* pop_param */
#line 38 "../Main.m3"
 /* call_direct */
#line 38 "../Main.m3"
RTIO__PutInt(
  ( INTEGER )(  INT64_(8) ),
  ( INTEGER )(  INT64_(0) ));
#line 38 "../Main.m3"
 /* start_call_direct */
#line 38 "../Main.m3"
 /* call_direct */
#line 38 "../Main.m3"
Main__NL(
 );
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
 /* call_direct */
#line 39 "../Main.m3"
RTIO__PutText(
  ( TEXT )(((ADDRESS)(INT64_(784)+((ADDRESS)(&Main_m_30_L_31)))) ));
#line 39 "../Main.m3"
 /* start_call_direct */
#line 39 "../Main.m3"
 /* load_integer */
#line 39 "../Main.m3"
 /* pop_param */
#line 39 "../Main.m3"
 /* load_integer */
#line 39 "../Main.m3"
 /* pop_param */
#line 39 "../Main.m3"
 /* call_direct */
#line 39 "../Main.m3"
RTIO__PutInt(
  ( INTEGER )(  INT64_(1) ),
  ( INTEGER )(  INT64_(0) ));
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
#line 41 "../Main.m3"
 /* start_call_direct */
#line 41 "../Main.m3"
 /* load_address */
#line 41 "../Main.m3"
 /* pop_param */
#line 41 "../Main.m3"
 /* call_direct */
#line 41 "../Main.m3"
RTIO__PutText(
  ( TEXT )(((ADDRESS)(INT64_(824)+((ADDRESS)(&Main_m_30_L_31)))) ));
#line 41 "../Main.m3"
 /* start_call_direct */
#line 41 "../Main.m3"
 /* call_direct */
#line 41 "../Main.m3"
 /* store */
#line 41 "../Main.m3"
(*(double*)(&Main_m_34_L_35))=(double)(((double)(Time__Now(
 ))));
#line 41 "../Main.m3"
 /* start_call_direct */
#line 41 "../Main.m3"
 /* load */
#line 41 "../Main.m3"
 /* pop_param */
#line 41 "../Main.m3"
 /* call_direct */
#line 41 "../Main.m3"
RTIO__PutF(
  ( LONGREAL )( Main_m_34_L_35 ));
#line 41 "../Main.m3"
 /* start_call_direct */
#line 41 "../Main.m3"
 /* call_direct */
#line 41 "../Main.m3"
Main__NL(
 );
#line 41 "../Main.m3"
 /* set_source_line */
#line 41 "../Main.m3"
#line 43 "../Main.m3"
 /* start_call_direct */
#line 43 "../Main.m3"
 /* load_address */
#line 43 "../Main.m3"
 /* pop_param */
#line 43 "../Main.m3"
 /* call_direct */
#line 43 "../Main.m3"
RTIO__PutText(
  ( TEXT )(((ADDRESS)(INT64_(856)+((ADDRESS)(&Main_m_30_L_31)))) ));
#line 43 "../Main.m3"
 /* set_source_line */
#line 43 "../Main.m3"
#line 44 "../Main.m3"
 /* start_call_direct */
#line 44 "../Main.m3"
 /* call_direct */
#line 44 "../Main.m3"
 /* store */
#line 44 "../Main.m3"
(*(double*)(&Main_m_34_L_35))=(double)(((double)(Time__Now(
 ))));
#line 44 "../Main.m3"
 /* start_call_direct */
#line 44 "../Main.m3"
 /* load */
#line 44 "../Main.m3"
 /* load_float */
#line 44 "../Main.m3"
 /* subtract */
#line 44 "../Main.m3"
 /* pop_param */
#line 44 "../Main.m3"
 /* call_direct */
#line 44 "../Main.m3"
RTIO__PutF(
  ( LONGREAL )( ((double)( Main_m_34_L_35- ((double)(1.16444736000000000e10)))) ));
#line 44 "../Main.m3"
 /* set_source_line */
#line 44 "../Main.m3"
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
#line 47 "../Main.m3"
 /* start_call_direct */
#line 47 "../Main.m3"
 /* call_direct */
#line 47 "../Main.m3"
 /* store */
#line 47 "../Main.m3"
(*(double*)(&Main_m_34_L_35))=(double)(((double)(Time__Now(
 ))));
#line 47 "../Main.m3"
 /* load */
#line 47 "../Main.m3"
 /* load_indirect */
#line 47 "../Main.m3"
 /* store */
#line 47 "../Main.m3"
(*(ADDRESS*)(&Main_m_36_L_37))=(ADDRESS)(((ADDRESS)(*((ADDRESS*)(INT64_(104)+((ADDRESS)(*((ADDRESS*)(INT64_(216)+((ADDRESS)(&Main_m_M_Main_L_32)))))))))));
#line 47 "../Main.m3"
 /* load_nil */
#line 47 "../Main.m3"
 /* load */
#line 47 "../Main.m3"
 /* if_compare */
#line 47 "../Main.m3"
if(m3_eq(ADDRESS,
 ((ADDRESS)(0)),
 ((ADDRESS)(Main_m_36_L_37))))goto L2;
#line 47 "../Main.m3"
 /* load */
#line 47 "../Main.m3"
 /* loophole */
#line 47 "../Main.m3"
 /* load_integer */
#line 47 "../Main.m3"
 /* and */
#line 47 "../Main.m3"
 /* if_true_or_false */
#line 47 "../Main.m3"
 /* load_host_integer */
#line 47 "../Main.m3"
 /* load_integer */
#line 47 "../Main.m3"
 /* if_compare */
#line 47 "../Main.m3"
if(m3_ne(INT64,
  ((INT64)(((INT64)((INT64)Main_m_36_L_37))&  INT64_(1))),
   INT64_(0)))goto L2;
#line 47 "../Main.m3"
 /* load */
#line 47 "../Main.m3"
 /* load_indirect */
#line 47 "../Main.m3"
 /* extract_mn */
#line 47 "../Main.m3"
 /* load_host_integer */
#line 47 "../Main.m3"
 /* load_integer */
#line 47 "../Main.m3"
 /* load_host_integer */
#line 47 "../Main.m3"
 /* load_integer */
#line 47 "../Main.m3"
 /* extract */
#line 47 "../Main.m3"
 /* if_true_or_false */
#line 47 "../Main.m3"
 /* load_host_integer */
#line 47 "../Main.m3"
 /* load_integer */
#line 47 "../Main.m3"
 /* if_compare */
#line 47 "../Main.m3"
if(m3_eq(INT64,
 ((INT64)(m3_extract(
 UINT64,
  *((INT64*)(INT64_(-8)+((ADDRESS)(Main_m_36_L_37)))),
   UINT64_(22),
   UINT64_(1)))),
   INT64_(0)))goto L2;
#line 47 "../Main.m3"
 /* start_call_direct */
#line 47 "../Main.m3"
 /* load */
#line 47 "../Main.m3"
 /* pop_param */
#line 47 "../Main.m3"
 /* call_direct */
#line 47 "../Main.m3"
RTHooks__CheckLoadTracedRef(
  ( REFANY )(((ADDRESS)(Main_m_36_L_37)) ));
#line 47 "../Main.m3"
 /* set_label */
#line 47 "../Main.m3"
L2:;
#line 47 "../Main.m3"
 /* start_call_direct */
#line 47 "../Main.m3"
 /* load_address */
#line 47 "../Main.m3"
 /* pop_param */
#line 47 "../Main.m3"
 /* load */
#line 47 "../Main.m3"
 /* pop_param */
#line 47 "../Main.m3"
 /* load */
#line 47 "../Main.m3"
 /* pop_param */
#line 47 "../Main.m3"
 /* call_direct */
#line 47 "../Main.m3"
Date__FromTime(
  ( T32721A2D* /*TypeText1*/  )(((ADDRESS)(&Main_m_38_L_39)) ),
  ( Time__T )( Main_m_34_L_35 ),
  ( Date__TimeZone )(((ADDRESS)(Main_m_36_L_37)) ));
#line 47 "../Main.m3"
 /* load_address */
#line 47 "../Main.m3"
 /* load_address */
#line 47 "../Main.m3"
 /* copy */
#line 47 "../Main.m3"
m3_memmove(
 INT64_(104)+((ADDRESS)(&Main_m_M_Main_L_32)),
 &Main_m_38_L_39,
 40);
#line 47 "../Main.m3"
 /* set_source_line */
#line 47 "../Main.m3"
#line 48 "../Main.m3"
 /* start_call_direct */
#line 48 "../Main.m3"
 /* load_address */
#line 48 "../Main.m3"
 /* pop_param */
#line 48 "../Main.m3"
 /* call_direct */
#line 48 "../Main.m3"
RTIO__PutText(
  ( TEXT )(((ADDRESS)(INT64_(928)+((ADDRESS)(&Main_m_30_L_31)))) ));
#line 48 "../Main.m3"
 /* start_call_direct */
#line 48 "../Main.m3"
 /* load */
#line 48 "../Main.m3"
 /* pop_param */
#line 48 "../Main.m3"
 /* load_integer */
#line 48 "../Main.m3"
 /* pop_param */
#line 48 "../Main.m3"
 /* call_direct */
#line 48 "../Main.m3"
RTIO__PutInt(
  ( INTEGER )( ((INT64)(*((UINT64*)(INT64_(104)+((ADDRESS)(&Main_m_M_Main_L_32)))))) ),
  ( INTEGER )(  INT64_(0) ));
#line 48 "../Main.m3"
 /* start_call_direct */
#line 48 "../Main.m3"
 /* call_direct */
#line 48 "../Main.m3"
Main__NL(
 );
#line 48 "../Main.m3"
 /* set_source_line */
#line 48 "../Main.m3"
#line 49 "../Main.m3"
 /* start_call_direct */
#line 49 "../Main.m3"
 /* load_address */
#line 49 "../Main.m3"
 /* pop_param */
#line 49 "../Main.m3"
 /* call_direct */
#line 49 "../Main.m3"
RTIO__PutText(
  ( TEXT )(((ADDRESS)(INT64_(968)+((ADDRESS)(&Main_m_30_L_31)))) ));
#line 49 "../Main.m3"
 /* start_call_direct */
#line 49 "../Main.m3"
 /* load */
#line 49 "../Main.m3"
 /* pop_param */
#line 49 "../Main.m3"
 /* load_integer */
#line 49 "../Main.m3"
 /* pop_param */
#line 49 "../Main.m3"
 /* call_direct */
#line 49 "../Main.m3"
RTIO__PutInt(
  ( INTEGER )( ((INT64)(*((UINT8*)(INT64_(112)+((ADDRESS)(&Main_m_M_Main_L_32)))))) ),
  ( INTEGER )(  INT64_(0) ));
#line 49 "../Main.m3"
 /* start_call_direct */
#line 49 "../Main.m3"
 /* call_direct */
#line 49 "../Main.m3"
Main__NL(
 );
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
 /* call_direct */
#line 50 "../Main.m3"
RTIO__PutText(
  ( TEXT )(((ADDRESS)(INT64_(1008)+((ADDRESS)(&Main_m_30_L_31)))) ));
#line 50 "../Main.m3"
 /* start_call_direct */
#line 50 "../Main.m3"
 /* load */
#line 50 "../Main.m3"
 /* pop_param */
#line 50 "../Main.m3"
 /* load_integer */
#line 50 "../Main.m3"
 /* pop_param */
#line 50 "../Main.m3"
 /* call_direct */
#line 50 "../Main.m3"
RTIO__PutInt(
  ( INTEGER )( ((INT64)(*((UINT8*)(INT64_(113)+((ADDRESS)(&Main_m_M_Main_L_32)))))) ),
  ( INTEGER )(  INT64_(0) ));
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
 /* start_call_direct */
#line 51 "../Main.m3"
 /* load_address */
#line 51 "../Main.m3"
 /* pop_param */
#line 51 "../Main.m3"
 /* call_direct */
#line 51 "../Main.m3"
RTIO__PutText(
  ( TEXT )(((ADDRESS)(INT64_(1048)+((ADDRESS)(&Main_m_30_L_31)))) ));
#line 51 "../Main.m3"
 /* start_call_direct */
#line 51 "../Main.m3"
 /* load */
#line 51 "../Main.m3"
 /* pop_param */
#line 51 "../Main.m3"
 /* load_integer */
#line 51 "../Main.m3"
 /* pop_param */
#line 51 "../Main.m3"
 /* call_direct */
#line 51 "../Main.m3"
RTIO__PutInt(
  ( INTEGER )( ((INT64)(*((UINT8*)(INT64_(114)+((ADDRESS)(&Main_m_M_Main_L_32)))))) ),
  ( INTEGER )(  INT64_(0) ));
#line 51 "../Main.m3"
 /* start_call_direct */
#line 51 "../Main.m3"
 /* call_direct */
#line 51 "../Main.m3"
Main__NL(
 );
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
 /* call_direct */
#line 52 "../Main.m3"
RTIO__PutText(
  ( TEXT )(((ADDRESS)(INT64_(1088)+((ADDRESS)(&Main_m_30_L_31)))) ));
#line 52 "../Main.m3"
 /* start_call_direct */
#line 52 "../Main.m3"
 /* load */
#line 52 "../Main.m3"
 /* pop_param */
#line 52 "../Main.m3"
 /* load_integer */
#line 52 "../Main.m3"
 /* pop_param */
#line 52 "../Main.m3"
 /* call_direct */
#line 52 "../Main.m3"
RTIO__PutInt(
  ( INTEGER )( ((INT64)(*((UINT8*)(INT64_(115)+((ADDRESS)(&Main_m_M_Main_L_32)))))) ),
  ( INTEGER )(  INT64_(0) ));
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
 /* start_call_direct */
#line 53 "../Main.m3"
 /* load_address */
#line 53 "../Main.m3"
 /* pop_param */
#line 53 "../Main.m3"
 /* call_direct */
#line 53 "../Main.m3"
RTIO__PutText(
  ( TEXT )(((ADDRESS)(INT64_(1128)+((ADDRESS)(&Main_m_30_L_31)))) ));
#line 53 "../Main.m3"
 /* start_call_direct */
#line 53 "../Main.m3"
 /* load */
#line 53 "../Main.m3"
 /* pop_param */
#line 53 "../Main.m3"
 /* load_integer */
#line 53 "../Main.m3"
 /* pop_param */
#line 53 "../Main.m3"
 /* call_direct */
#line 53 "../Main.m3"
RTIO__PutInt(
  ( INTEGER )( ((INT64)(*((UINT8*)(INT64_(116)+((ADDRESS)(&Main_m_M_Main_L_32)))))) ),
  ( INTEGER )(  INT64_(0) ));
#line 53 "../Main.m3"
 /* start_call_direct */
#line 53 "../Main.m3"
 /* call_direct */
#line 53 "../Main.m3"
Main__NL(
 );
#line 53 "../Main.m3"
 /* set_source_line */
#line 53 "../Main.m3"
#line 54 "../Main.m3"
 /* start_call_direct */
#line 54 "../Main.m3"
 /* load_address */
#line 54 "../Main.m3"
 /* pop_param */
#line 54 "../Main.m3"
 /* call_direct */
#line 54 "../Main.m3"
RTIO__PutText(
  ( TEXT )(((ADDRESS)(INT64_(1168)+((ADDRESS)(&Main_m_30_L_31)))) ));
#line 54 "../Main.m3"
 /* start_call_direct */
#line 54 "../Main.m3"
 /* load */
#line 54 "../Main.m3"
 /* pop_param */
#line 54 "../Main.m3"
 /* load_integer */
#line 54 "../Main.m3"
 /* pop_param */
#line 54 "../Main.m3"
 /* call_direct */
#line 54 "../Main.m3"
RTIO__PutInt(
  ( INTEGER )(((INT64)(*((INT64*)(INT64_(120)+((ADDRESS)(&Main_m_M_Main_L_32)))))) ),
  ( INTEGER )(  INT64_(0) ));
#line 54 "../Main.m3"
 /* start_call_direct */
#line 54 "../Main.m3"
 /* call_direct */
#line 54 "../Main.m3"
Main__NL(
 );
#line 54 "../Main.m3"
 /* set_source_line */
#line 54 "../Main.m3"
#line 55 "../Main.m3"
 /* start_call_direct */
#line 55 "../Main.m3"
 /* load_address */
#line 55 "../Main.m3"
 /* pop_param */
#line 55 "../Main.m3"
 /* call_direct */
#line 55 "../Main.m3"
RTIO__PutText(
  ( TEXT )(((ADDRESS)(INT64_(1208)+((ADDRESS)(&Main_m_30_L_31)))) ));
#line 55 "../Main.m3"
 /* load */
#line 55 "../Main.m3"
 /* store */
#line 55 "../Main.m3"
(*(ADDRESS*)(&Main_m_36_L_37))=(ADDRESS)(((ADDRESS)(*((ADDRESS*)(INT64_(128)+((ADDRESS)(&Main_m_M_Main_L_32)))))));
#line 55 "../Main.m3"
 /* load_nil */
#line 55 "../Main.m3"
 /* load */
#line 55 "../Main.m3"
 /* if_compare */
#line 55 "../Main.m3"
if(m3_eq(ADDRESS,
 ((ADDRESS)(0)),
 ((ADDRESS)(Main_m_36_L_37))))goto L3;
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
  ((INT64)(((INT64)((INT64)Main_m_36_L_37))&  INT64_(1))),
   INT64_(0)))goto L3;
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
  *((INT64*)(INT64_(-8)+((ADDRESS)(Main_m_36_L_37)))),
   UINT64_(22),
   UINT64_(1)))),
   INT64_(0)))goto L3;
#line 55 "../Main.m3"
 /* start_call_direct */
#line 55 "../Main.m3"
 /* load */
#line 55 "../Main.m3"
 /* pop_param */
#line 55 "../Main.m3"
 /* call_direct */
#line 55 "../Main.m3"
RTHooks__CheckLoadTracedRef(
  ( REFANY )(((ADDRESS)(Main_m_36_L_37)) ));
#line 55 "../Main.m3"
 /* set_label */
#line 55 "../Main.m3"
L3:;
#line 55 "../Main.m3"
 /* start_call_direct */
#line 55 "../Main.m3"
 /* load */
#line 55 "../Main.m3"
 /* pop_param */
#line 55 "../Main.m3"
 /* call_direct */
#line 55 "../Main.m3"
RTIO__PutText(
  ( TEXT )(((ADDRESS)(Main_m_36_L_37)) ));
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
 /* load_address */
#line 56 "../Main.m3"
 /* pop_param */
#line 56 "../Main.m3"
 /* call_direct */
#line 56 "../Main.m3"
RTIO__PutText(
  ( TEXT )(((ADDRESS)(INT64_(1248)+((ADDRESS)(&Main_m_30_L_31)))) ));
#line 56 "../Main.m3"
 /* start_call_direct */
#line 56 "../Main.m3"
 /* load */
#line 56 "../Main.m3"
 /* pop_param */
#line 56 "../Main.m3"
 /* load_integer */
#line 56 "../Main.m3"
 /* pop_param */
#line 56 "../Main.m3"
 /* call_direct */
#line 56 "../Main.m3"
RTIO__PutInt(
  ( INTEGER )( ((INT64)(*((UINT8*)(INT64_(136)+((ADDRESS)(&Main_m_M_Main_L_32)))))) ),
  ( INTEGER )(  INT64_(0) ));
#line 56 "../Main.m3"
 /* start_call_direct */
#line 56 "../Main.m3"
 /* call_direct */
#line 56 "../Main.m3"
Main__NL(
 );
#line 56 "../Main.m3"
 /* set_source_line */
#line 56 "../Main.m3"
#line 58 "../Main.m3"
 /* start_call_direct */
#line 58 "../Main.m3"
 /* call_direct */
#line 58 "../Main.m3"
 /* store */
#line 58 "../Main.m3"
(*(double*)(&Main_m_34_L_35))=(double)(((double)(Time__Now(
 ))));
#line 58 "../Main.m3"
 /* load */
#line 58 "../Main.m3"
 /* load_indirect */
#line 58 "../Main.m3"
 /* store */
#line 58 "../Main.m3"
(*(ADDRESS*)(&Main_m_36_L_37))=(ADDRESS)(((ADDRESS)(*((ADDRESS*)(INT64_(112)+((ADDRESS)(*((ADDRESS*)(INT64_(216)+((ADDRESS)(&Main_m_M_Main_L_32)))))))))));
#line 58 "../Main.m3"
 /* load_nil */
#line 58 "../Main.m3"
 /* load */
#line 58 "../Main.m3"
 /* if_compare */
#line 58 "../Main.m3"
if(m3_eq(ADDRESS,
 ((ADDRESS)(0)),
 ((ADDRESS)(Main_m_36_L_37))))goto L4;
#line 58 "../Main.m3"
 /* load */
#line 58 "../Main.m3"
 /* loophole */
#line 58 "../Main.m3"
 /* load_integer */
#line 58 "../Main.m3"
 /* and */
#line 58 "../Main.m3"
 /* if_true_or_false */
#line 58 "../Main.m3"
 /* load_host_integer */
#line 58 "../Main.m3"
 /* load_integer */
#line 58 "../Main.m3"
 /* if_compare */
#line 58 "../Main.m3"
if(m3_ne(INT64,
  ((INT64)(((INT64)((INT64)Main_m_36_L_37))&  INT64_(1))),
   INT64_(0)))goto L4;
#line 58 "../Main.m3"
 /* load */
#line 58 "../Main.m3"
 /* load_indirect */
#line 58 "../Main.m3"
 /* extract_mn */
#line 58 "../Main.m3"
 /* load_host_integer */
#line 58 "../Main.m3"
 /* load_integer */
#line 58 "../Main.m3"
 /* load_host_integer */
#line 58 "../Main.m3"
 /* load_integer */
#line 58 "../Main.m3"
 /* extract */
#line 58 "../Main.m3"
 /* if_true_or_false */
#line 58 "../Main.m3"
 /* load_host_integer */
#line 58 "../Main.m3"
 /* load_integer */
#line 58 "../Main.m3"
 /* if_compare */
#line 58 "../Main.m3"
if(m3_eq(INT64,
 ((INT64)(m3_extract(
 UINT64,
  *((INT64*)(INT64_(-8)+((ADDRESS)(Main_m_36_L_37)))),
   UINT64_(22),
   UINT64_(1)))),
   INT64_(0)))goto L4;
#line 58 "../Main.m3"
 /* start_call_direct */
#line 58 "../Main.m3"
 /* load */
#line 58 "../Main.m3"
 /* pop_param */
#line 58 "../Main.m3"
 /* call_direct */
#line 58 "../Main.m3"
RTHooks__CheckLoadTracedRef(
  ( REFANY )(((ADDRESS)(Main_m_36_L_37)) ));
#line 58 "../Main.m3"
 /* set_label */
#line 58 "../Main.m3"
L4:;
#line 58 "../Main.m3"
 /* start_call_direct */
#line 58 "../Main.m3"
 /* load_address */
#line 58 "../Main.m3"
 /* pop_param */
#line 58 "../Main.m3"
 /* load */
#line 58 "../Main.m3"
 /* pop_param */
#line 58 "../Main.m3"
 /* load */
#line 58 "../Main.m3"
 /* pop_param */
#line 58 "../Main.m3"
 /* call_direct */
#line 58 "../Main.m3"
Date__FromTime(
  ( T32721A2D* /*TypeText1*/  )(((ADDRESS)(&Main_m_38_L_39)) ),
  ( Time__T )( Main_m_34_L_35 ),
  ( Date__TimeZone )(((ADDRESS)(Main_m_36_L_37)) ));
#line 58 "../Main.m3"
 /* load_address */
#line 58 "../Main.m3"
 /* load_address */
#line 58 "../Main.m3"
 /* copy */
#line 58 "../Main.m3"
m3_memmove(
 INT64_(104)+((ADDRESS)(&Main_m_M_Main_L_32)),
 &Main_m_38_L_39,
 40);
#line 58 "../Main.m3"
 /* set_source_line */
#line 58 "../Main.m3"
#line 59 "../Main.m3"
 /* start_call_direct */
#line 59 "../Main.m3"
 /* load_address */
#line 59 "../Main.m3"
 /* pop_param */
#line 59 "../Main.m3"
 /* call_direct */
#line 59 "../Main.m3"
RTIO__PutText(
  ( TEXT )(((ADDRESS)(INT64_(1288)+((ADDRESS)(&Main_m_30_L_31)))) ));
#line 59 "../Main.m3"
 /* start_call_direct */
#line 59 "../Main.m3"
 /* load */
#line 59 "../Main.m3"
 /* pop_param */
#line 59 "../Main.m3"
 /* load_integer */
#line 59 "../Main.m3"
 /* pop_param */
#line 59 "../Main.m3"
 /* call_direct */
#line 59 "../Main.m3"
RTIO__PutInt(
  ( INTEGER )( ((INT64)(*((UINT64*)(INT64_(104)+((ADDRESS)(&Main_m_M_Main_L_32)))))) ),
  ( INTEGER )(  INT64_(0) ));
#line 59 "../Main.m3"
 /* start_call_direct */
#line 59 "../Main.m3"
 /* call_direct */
#line 59 "../Main.m3"
Main__NL(
 );
#line 59 "../Main.m3"
 /* set_source_line */
#line 59 "../Main.m3"
#line 60 "../Main.m3"
 /* start_call_direct */
#line 60 "../Main.m3"
 /* load_address */
#line 60 "../Main.m3"
 /* pop_param */
#line 60 "../Main.m3"
 /* call_direct */
#line 60 "../Main.m3"
RTIO__PutText(
  ( TEXT )(((ADDRESS)(INT64_(1328)+((ADDRESS)(&Main_m_30_L_31)))) ));
#line 60 "../Main.m3"
 /* start_call_direct */
#line 60 "../Main.m3"
 /* load */
#line 60 "../Main.m3"
 /* pop_param */
#line 60 "../Main.m3"
 /* load_integer */
#line 60 "../Main.m3"
 /* pop_param */
#line 60 "../Main.m3"
 /* call_direct */
#line 60 "../Main.m3"
RTIO__PutInt(
  ( INTEGER )( ((INT64)(*((UINT8*)(INT64_(112)+((ADDRESS)(&Main_m_M_Main_L_32)))))) ),
  ( INTEGER )(  INT64_(0) ));
#line 60 "../Main.m3"
 /* start_call_direct */
#line 60 "../Main.m3"
 /* call_direct */
#line 60 "../Main.m3"
Main__NL(
 );
#line 60 "../Main.m3"
 /* set_source_line */
#line 60 "../Main.m3"
#line 61 "../Main.m3"
 /* start_call_direct */
#line 61 "../Main.m3"
 /* load_address */
#line 61 "../Main.m3"
 /* pop_param */
#line 61 "../Main.m3"
 /* call_direct */
#line 61 "../Main.m3"
RTIO__PutText(
  ( TEXT )(((ADDRESS)(INT64_(1368)+((ADDRESS)(&Main_m_30_L_31)))) ));
#line 61 "../Main.m3"
 /* start_call_direct */
#line 61 "../Main.m3"
 /* load */
#line 61 "../Main.m3"
 /* pop_param */
#line 61 "../Main.m3"
 /* load_integer */
#line 61 "../Main.m3"
 /* pop_param */
#line 61 "../Main.m3"
 /* call_direct */
#line 61 "../Main.m3"
RTIO__PutInt(
  ( INTEGER )( ((INT64)(*((UINT8*)(INT64_(113)+((ADDRESS)(&Main_m_M_Main_L_32)))))) ),
  ( INTEGER )(  INT64_(0) ));
#line 61 "../Main.m3"
 /* start_call_direct */
#line 61 "../Main.m3"
 /* call_direct */
#line 61 "../Main.m3"
Main__NL(
 );
#line 61 "../Main.m3"
 /* set_source_line */
#line 61 "../Main.m3"
#line 62 "../Main.m3"
 /* start_call_direct */
#line 62 "../Main.m3"
 /* load_address */
#line 62 "../Main.m3"
 /* pop_param */
#line 62 "../Main.m3"
 /* call_direct */
#line 62 "../Main.m3"
RTIO__PutText(
  ( TEXT )(((ADDRESS)(INT64_(1408)+((ADDRESS)(&Main_m_30_L_31)))) ));
#line 62 "../Main.m3"
 /* start_call_direct */
#line 62 "../Main.m3"
 /* load */
#line 62 "../Main.m3"
 /* pop_param */
#line 62 "../Main.m3"
 /* load_integer */
#line 62 "../Main.m3"
 /* pop_param */
#line 62 "../Main.m3"
 /* call_direct */
#line 62 "../Main.m3"
RTIO__PutInt(
  ( INTEGER )( ((INT64)(*((UINT8*)(INT64_(114)+((ADDRESS)(&Main_m_M_Main_L_32)))))) ),
  ( INTEGER )(  INT64_(0) ));
#line 62 "../Main.m3"
 /* start_call_direct */
#line 62 "../Main.m3"
 /* call_direct */
#line 62 "../Main.m3"
Main__NL(
 );
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
 /* call_direct */
#line 63 "../Main.m3"
RTIO__PutText(
  ( TEXT )(((ADDRESS)(INT64_(1448)+((ADDRESS)(&Main_m_30_L_31)))) ));
#line 63 "../Main.m3"
 /* start_call_direct */
#line 63 "../Main.m3"
 /* load */
#line 63 "../Main.m3"
 /* pop_param */
#line 63 "../Main.m3"
 /* load_integer */
#line 63 "../Main.m3"
 /* pop_param */
#line 63 "../Main.m3"
 /* call_direct */
#line 63 "../Main.m3"
RTIO__PutInt(
  ( INTEGER )( ((INT64)(*((UINT8*)(INT64_(115)+((ADDRESS)(&Main_m_M_Main_L_32)))))) ),
  ( INTEGER )(  INT64_(0) ));
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
 /* start_call_direct */
#line 64 "../Main.m3"
 /* load_address */
#line 64 "../Main.m3"
 /* pop_param */
#line 64 "../Main.m3"
 /* call_direct */
#line 64 "../Main.m3"
RTIO__PutText(
  ( TEXT )(((ADDRESS)(INT64_(1488)+((ADDRESS)(&Main_m_30_L_31)))) ));
#line 64 "../Main.m3"
 /* start_call_direct */
#line 64 "../Main.m3"
 /* load */
#line 64 "../Main.m3"
 /* pop_param */
#line 64 "../Main.m3"
 /* load_integer */
#line 64 "../Main.m3"
 /* pop_param */
#line 64 "../Main.m3"
 /* call_direct */
#line 64 "../Main.m3"
RTIO__PutInt(
  ( INTEGER )( ((INT64)(*((UINT8*)(INT64_(116)+((ADDRESS)(&Main_m_M_Main_L_32)))))) ),
  ( INTEGER )(  INT64_(0) ));
#line 64 "../Main.m3"
 /* start_call_direct */
#line 64 "../Main.m3"
 /* call_direct */
#line 64 "../Main.m3"
Main__NL(
 );
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
 /* call_direct */
#line 65 "../Main.m3"
RTIO__PutText(
  ( TEXT )(((ADDRESS)(INT64_(1528)+((ADDRESS)(&Main_m_30_L_31)))) ));
#line 65 "../Main.m3"
 /* start_call_direct */
#line 65 "../Main.m3"
 /* load */
#line 65 "../Main.m3"
 /* pop_param */
#line 65 "../Main.m3"
 /* load_integer */
#line 65 "../Main.m3"
 /* pop_param */
#line 65 "../Main.m3"
 /* call_direct */
#line 65 "../Main.m3"
RTIO__PutInt(
  ( INTEGER )(((INT64)(*((INT64*)(INT64_(120)+((ADDRESS)(&Main_m_M_Main_L_32)))))) ),
  ( INTEGER )(  INT64_(0) ));
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
 /* start_call_direct */
#line 66 "../Main.m3"
 /* load_address */
#line 66 "../Main.m3"
 /* pop_param */
#line 66 "../Main.m3"
 /* call_direct */
#line 66 "../Main.m3"
RTIO__PutText(
  ( TEXT )(((ADDRESS)(INT64_(1568)+((ADDRESS)(&Main_m_30_L_31)))) ));
#line 66 "../Main.m3"
 /* load */
#line 66 "../Main.m3"
 /* store */
#line 66 "../Main.m3"
(*(ADDRESS*)(&Main_m_36_L_37))=(ADDRESS)(((ADDRESS)(*((ADDRESS*)(INT64_(128)+((ADDRESS)(&Main_m_M_Main_L_32)))))));
#line 66 "../Main.m3"
 /* load_nil */
#line 66 "../Main.m3"
 /* load */
#line 66 "../Main.m3"
 /* if_compare */
#line 66 "../Main.m3"
if(m3_eq(ADDRESS,
 ((ADDRESS)(0)),
 ((ADDRESS)(Main_m_36_L_37))))goto L5;
#line 66 "../Main.m3"
 /* load */
#line 66 "../Main.m3"
 /* loophole */
#line 66 "../Main.m3"
 /* load_integer */
#line 66 "../Main.m3"
 /* and */
#line 66 "../Main.m3"
 /* if_true_or_false */
#line 66 "../Main.m3"
 /* load_host_integer */
#line 66 "../Main.m3"
 /* load_integer */
#line 66 "../Main.m3"
 /* if_compare */
#line 66 "../Main.m3"
if(m3_ne(INT64,
  ((INT64)(((INT64)((INT64)Main_m_36_L_37))&  INT64_(1))),
   INT64_(0)))goto L5;
#line 66 "../Main.m3"
 /* load */
#line 66 "../Main.m3"
 /* load_indirect */
#line 66 "../Main.m3"
 /* extract_mn */
#line 66 "../Main.m3"
 /* load_host_integer */
#line 66 "../Main.m3"
 /* load_integer */
#line 66 "../Main.m3"
 /* load_host_integer */
#line 66 "../Main.m3"
 /* load_integer */
#line 66 "../Main.m3"
 /* extract */
#line 66 "../Main.m3"
 /* if_true_or_false */
#line 66 "../Main.m3"
 /* load_host_integer */
#line 66 "../Main.m3"
 /* load_integer */
#line 66 "../Main.m3"
 /* if_compare */
#line 66 "../Main.m3"
if(m3_eq(INT64,
 ((INT64)(m3_extract(
 UINT64,
  *((INT64*)(INT64_(-8)+((ADDRESS)(Main_m_36_L_37)))),
   UINT64_(22),
   UINT64_(1)))),
   INT64_(0)))goto L5;
#line 66 "../Main.m3"
 /* start_call_direct */
#line 66 "../Main.m3"
 /* load */
#line 66 "../Main.m3"
 /* pop_param */
#line 66 "../Main.m3"
 /* call_direct */
#line 66 "../Main.m3"
RTHooks__CheckLoadTracedRef(
  ( REFANY )(((ADDRESS)(Main_m_36_L_37)) ));
#line 66 "../Main.m3"
 /* set_label */
#line 66 "../Main.m3"
L5:;
#line 66 "../Main.m3"
 /* start_call_direct */
#line 66 "../Main.m3"
 /* load */
#line 66 "../Main.m3"
 /* pop_param */
#line 66 "../Main.m3"
 /* call_direct */
#line 66 "../Main.m3"
RTIO__PutText(
  ( TEXT )(((ADDRESS)(Main_m_36_L_37)) ));
#line 66 "../Main.m3"
 /* start_call_direct */
#line 66 "../Main.m3"
 /* call_direct */
#line 66 "../Main.m3"
Main__NL(
 );
#line 66 "../Main.m3"
 /* set_source_line */
#line 66 "../Main.m3"
#line 67 "../Main.m3"
 /* start_call_direct */
#line 67 "../Main.m3"
 /* load_address */
#line 67 "../Main.m3"
 /* pop_param */
#line 67 "../Main.m3"
 /* call_direct */
#line 67 "../Main.m3"
RTIO__PutText(
  ( TEXT )(((ADDRESS)(INT64_(1608)+((ADDRESS)(&Main_m_30_L_31)))) ));
#line 67 "../Main.m3"
 /* start_call_direct */
#line 67 "../Main.m3"
 /* load */
#line 67 "../Main.m3"
 /* pop_param */
#line 67 "../Main.m3"
 /* load_integer */
#line 67 "../Main.m3"
 /* pop_param */
#line 67 "../Main.m3"
 /* call_direct */
#line 67 "../Main.m3"
RTIO__PutInt(
  ( INTEGER )( ((INT64)(*((UINT8*)(INT64_(136)+((ADDRESS)(&Main_m_M_Main_L_32)))))) ),
  ( INTEGER )(  INT64_(0) ));
#line 67 "../Main.m3"
 /* start_call_direct */
#line 67 "../Main.m3"
 /* call_direct */
#line 67 "../Main.m3"
Main__NL(
 );
#line 67 "../Main.m3"
 /* set_source_line */
#line 67 "../Main.m3"
#line 69 "../Main.m3"
 /* start_call_direct */
#line 69 "../Main.m3"
 /* call_direct */
#line 69 "../Main.m3"
RTIO__Flush(
 );
#line 69 "../Main.m3"
 /* set_label */
#line 69 "../Main.m3"
L1:;
#line 69 "../Main.m3"
 /* load_address */
#line 69 "../Main.m3"
 /* exit_proc */
#line 69 "../Main.m3"
return (RT0__ModulePtr)(&Main_m_M_Main_L_32);
#line 69 "../Main.m3"
 /* end_procedure */
#line 69 "../Main.m3"
} /* global constant type descriptor */
#line 69 "../Main.m3"
 /* global data type descriptor */
#line 69 "../Main.m3"
 /* module global constants */
#line 69 "../Main.m3"
 /* procedure names */
#line 69 "../Main.m3"
 /* procedure table */
#line 69 "../Main.m3"
 /* global type map */
#line 69 "../Main.m3"
 /* file name */
#line 69 "../Main.m3"
 /* module global data */
#line 69 "../Main.m3"
 /* load map


 global data allocation for M_Main
     0   104  8  *module info*
   104    40  8  Main.d
   144    24  8  import Main
   168    24  8  import Time
   192    24  8  import RTIO
   216    24  8  import Date
   240    24  8  import RTHooks
   264     0  8  *TOTAL*


 global constants for M_Main
     0    40  8  TEXT literal methods
    40    26  8  *TEXT literal*
    72    28  8  *TEXT literal*
   104    31  8  *TEXT literal*
   136    32  8  *TEXT literal*
   168    30  8  *TEXT literal*
   200    31  8  *TEXT literal*
   232    33  8  *TEXT literal*
   272    33  8  *TEXT literal*
   312    33  8  *TEXT literal*
   352    31  8  *TEXT literal*
   384    34  8  *TEXT literal*
   424    32  8  *TEXT literal*
   456    35  8  *TEXT literal*
   496    36  8  *TEXT literal*
   536    34  8  *TEXT literal*
   576    35  8  *TEXT literal*
   616    37  8  *TEXT literal*
   656    37  8  *TEXT literal*
   696    37  8  *TEXT literal*
   736    35  8  *TEXT literal*
   776    38  8  *TEXT literal*
   816    29  8  *TEXT literal*
   848    72  8  *TEXT literal*
   920    36  8  *TEXT literal*
   960    37  8  *TEXT literal*
  1000    35  8  *TEXT literal*
  1040    36  8  *TEXT literal*
  1080    38  8  *TEXT literal*
  1120    38  8  *TEXT literal*
  1160    38  8  *TEXT literal*
  1200    36  8  *TEXT literal*
  1240    39  8  *TEXT literal*
  1280    34  8  *TEXT literal*
  1320    35  8  *TEXT literal*
  1360    33  8  *TEXT literal*
  1400    34  8  *TEXT literal*
  1440    36  8  *TEXT literal*
  1480    36  8  *TEXT literal*
  1520    36  8  *TEXT literal*
  1560    34  8  *TEXT literal*
  1600    37  8  *TEXT literal*
  1640    11  8  *proc names*
  1656    40  8  *proc info*
  1696     4  1  type_map
  1700    11  1  *string*
  1712     0  8  *TOTAL*
 */
#line 69 "../Main.m3"
 /* end unit */
#line 69 "../Main.m3"

#ifdef __cplusplus

} /* extern "C" */
#endif
 /* set_runtime_proc */
 /* set_runtime_proc */
