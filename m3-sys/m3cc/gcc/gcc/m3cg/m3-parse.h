#include <limits.h>

#if UCHAR_MAX == 0x0FFUL
typedef   signed char        INT8;
typedef unsigned char       UINT8;
#else
#error unable to find 8bit integer
#endif
#if USHRT_MAX == 0x0FFFFUL
typedef   signed short      INT16;
typedef unsigned short     UINT16;
#else
#error unable to find 16bit integer
#endif
#if UINT_MAX == 0x0FFFFFFFFUL
typedef   signed int        INT32;
typedef unsigned int       UINT32;
#elif ULONG_MAX == 0x0FFFFFFFFUL
typedef   signed long       INT32;
typedef unsigned long      UINT32;
#else
#error unable to find 32bit integer
#endif
#if defined(_MSC_VER) || defined(__DECC) || defined(__DECCXX)
typedef   signed __int64    INT64;
typedef unsigned __int64   UINT64;
#else
typedef   signed long long  INT64;
typedef unsigned long long UINT64;
#endif
