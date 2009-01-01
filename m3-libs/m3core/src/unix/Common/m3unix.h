#define _FILE_OFFSET_BITS 64

#ifdef _MSC_VER
typedef   signed __int8   INT8;
typedef unsigned __int8  UINT8;
typedef   signed __int16  INT16;
typedef unsigned __int16 UINT16;
typedef   signed __int32  INT32;
typedef unsigned __int32 UINT32;
typedef   signed __int64  INT64;
typedef unsigned __int64 UINT64;
#else
typedef   signed char       INT8;
typedef unsigned char      UINT8;
typedef   signed short      INT16;
typedef unsigned short     UINT16;
typedef   signed int        INT32;
typedef unsigned int       UINT32;
typedef   signed long long  INT64;
typedef unsigned long long UINT64;
#endif

#include <stddef.h>
#include <sys/types.h>

/* INTEGER is always signed and exactly the same size as a pointer */
typedef ptrdiff_t INTEGER;

/* LONGINT is always signed and exactly 64 bits. */
typedef INT64 LONGINT;
