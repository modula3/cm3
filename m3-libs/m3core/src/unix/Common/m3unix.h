/* TBD: what is the meaning of each of these? */
#define __USE_LARGEFILE64
#define _FILE_OFFSET_BITS 64
#define _LARGEFILE_SOURCE
#define _LARGEFILE64_SOURCE
#define __USE_FILE_OFFSET64

#ifndef _MSC_VER
#define __int64 long long
#endif

#include <stddef.h>
#include <sys/types.h>

typedef ptrdiff_t INTEGER;
typedef __int64 LONGINT;
typedef __int64 INT64;
typedef unsigned __int64 UINT64;
