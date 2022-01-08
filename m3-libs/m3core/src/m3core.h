#if _MSC_VER > 1000
#pragma once
#endif

// Before including m3core.h, check if INCLUDED_M3CORE_H is defined.
// This is not so much as to optimize compilation, but to enable
// concatenating all the files.
#ifndef INCLUDED_M3CORE_H
#define INCLUDED_M3CORE_H

#ifndef WIN32_LEAN_AND_MEAN
#define WIN32_LEAN_AND_MEAN 1
#endif
#define _NO_CRT_STDIO_INLINE 1 /* Do not accidentally export printf. */
#define _CRT_SECURE_NO_DEPRECATE 1
#define _CRT_NONSTDC_NO_DEPRECATE 1
#define _WINSOCK_DEPRECATED_NO_WARNINGS 1
#define _KERNEL32_ 1 /* inhibit declspec(dllimport) for consistency; it is not needed on functions */
#ifdef _MSC_VER
// These two must come first.
#pragma warning(disable:4616) /* there is no warning x (unavoidable if targeting multiple compiler versions) */
#pragma warning(disable:4619) /* there is no warning x (unavoidable if targeting multiple compiler versions) */
// The rest are sorted.
#pragma warning(disable:4115) /* named type definition in parentheses */
#pragma warning(disable:4100) /* unused parameter */
#pragma warning(disable:4127) /* conditional expression is constant */
#pragma warning(disable:4201) /* nonstandard extension: nameless struct/union */
#pragma warning(disable:4209) /* nonstandard extension: benign re-typedef */
#pragma warning(disable:4214) /* nonstandard extension: bitfield other than int */
#pragma warning(disable:4226) /* nonstandard extension: __export */
#pragma warning(disable:4242) /* 'return': conversion from '' to '', possible loss of data */
#pragma warning(disable:4244) /* integer conversion */
#pragma warning(disable:4255) /* () change to (void) */
#pragma warning(disable:4514) /* unused inline function removed */
#pragma warning(disable:4705) /* statement has no effect for merely using assert() at -W4 */
#pragma warning(disable:4710) /* function not inlined */
#pragma warning(disable:4668) /* #if of undefined symbol */
#pragma warning(disable:4820) /* padding inserted */
#pragma warning(disable:5045) /* Compiler will insert Spectre mitigation for memory load if /Qspectre switch specified */
#endif

#ifdef _WIN32
#include "winsock2.h"
#include "ws2tcpip.h"
#endif
typedef int BOOL;
#define TRUE 1
#define FALSE 0

/* __DARWIN_UNIX03 defaults to 1 on older and newer headers,
 * but older headers still have context "ss" instead of "__ss"
 * and such, so we have to force 0.
 * That is -- the defaults vary, the behavior of the newer
 * default is not available in older headers, so we must
 * force the older behavior, so that we can write one compatible source.
 */
#if defined(__APPLE__) && !defined(__DARWIN_UNIX03)
#define __DARWIN_UNIX03 0
#endif

#if __GNUC__ || __SUNPRO_C >= 0x590
#define M3_NO_INLINE __attribute__((noinline))
#elif _MSC_VER >= 1300
#define M3_NO_INLINE __declspec(noinline)
#else
#define M3_NO_INLINE /* nothing */
#endif

#ifdef __osf__
/* To get struct tm.tm_gmtoff, tm_zone. Would be good to autoconf this? */
#ifndef _OSF_SOURCE
#define _OSF_SOURCE
#endif
/* For socklen_t. Would be good to autoconf this.
 * This also gives us "uin-len".
 */
#ifndef _POSIX_PII_SOCKET
#define _POSIX_PII_SOCKET
#endif
/* More clearly get "uin-len". */
#ifndef _SOCKADDR_LEN
#define _SOCKADDR_LEN
#endif
/* Request 64bit time_t. Not available on v4. Would be good to autoconf this.
 * We later check for TIMEVAL64TO32/TIMEVAL32TO64 to see if this works.
 */
#ifndef _TIME64_T
#define _TIME64_T
#endif
#endif /* osf */

/* Autoconf: AC_SYS_LARGEFILE
 */
#define _FILE_OFFSET_BITS 64

/* const is extern const in C, but static const in C++,
 * but gcc gives a warning for the correct portable form "extern const",
 * unless you also forward declare:
 *  extern const int foo;
 *  extern const int foo = 123;
 */
#if defined(__cplusplus) || !defined(__GNUC__)
#define EXTERN_CONST extern const
#else
#define EXTERN_CONST const
#endif

#if defined(__sun) && !defined(__MAKECONTEXT_V2_SOURCE)
/* Support for userthreads on Solaris 9 4/03 and later.
 * Support for older is easy but absent.
 */
#define __MAKECONTEXT_V2_SOURCE
#endif

/* Autoconf: AC_USE_SYSTEM_EXTENSIONS */
/* Enable extensions on AIX 3, Interix.  */
#ifndef _ALL_SOURCE
#define _ALL_SOURCE 1
#endif
/* Enable GNU extensions on systems that have them.  */
#ifndef _GNU_SOURCE
#define _GNU_SOURCE 1
#endif
/* Enable threading extensions on Solaris.  */
#ifndef _POSIX_PTHREAD_SEMANTICS
#define _POSIX_PTHREAD_SEMANTICS 1
#endif
/* Enable extensions on HP NonStop.  */
#ifndef _TANDEM_SOURCE
#define _TANDEM_SOURCE 1
#endif
/* Enable general extensions on Solaris.  */
#ifndef __EXTENSIONS__
#define __EXTENSIONS__ 1
#endif

#ifndef _REENTRANT
#define _REENTRANT
#endif

/* AC_SYS_LARGEFILE */
#ifndef _LARGE_FILES
#define _LARGE_FILES 1
#endif

#ifndef _FILE_OFFSET_BITS
#define _FILE_OFFSET_BITS 64
#endif

/*#ifdef __vms*/
/* Enable support for files larger than 2GB.
 * Autoconf: AC_SYS_LARGEFILE?
 */
#ifndef _LARGEFILE
#define _LARGEFILE
#endif
/* Enable 32bit gids and reveal setreuids. */
#ifndef __USE_LONG_GID_T
#define __USE_LONG_GID_T 1
#endif
/* st_ino has three forms that all fit in the
 * same space; pick the one we want.
 */
#ifndef __USE_INO64
#define __USE_INO64 1
#endif
/*#endif*/

#if defined(__arm__) && defined(__APPLE__)
/* Reveal the correct struct stat? */
#ifndef _DARWIN_FEATURE_64_ONLY_BIT_INODE
#define _DARWIN_FEATURE_64_ONLY_BIT_INODE
#endif
#endif

// Explicit __cdecl supports building Windows/x86 code
// with non-default calling convention (cl /Gz /Gr /Gv),
// while retaining constant meaning for Modula-3 m3core and
// m3c output. This is messy and causes many warnings on Haiku/amd64.
//
// Explicit __stdcall is for a few helper functions on Windows/x86.
// This also produces warnings on Haiku/amd64 but not as many as __cdecl.
//
// cdecl/stdcall have essentially no meaning outside
// of Windows/x86. They might have meaning on Windows/amd64
// compiled with /Gv for default vector call. (Or only for certain parameter lists?)
// Do not do that?
// They might have meaning on Haiku/x86?
// They are correctly placed for Haiku/gcc, for functions
// but maybe not function pointers. The warnings are
// that Haiku/amd64 ignores them and/or they are misplaced for function pointers.
//
// We should probably change the helpers to cdecl.
// Explicit stdcall would remain in use then only for
// imported Windows functions (unless we wrap them all as we do for Posix).
//
#if defined(_WIN32) && !defined(_WIN64)
#undef __cdecl
#undef __stdcall
#define __cdecl /* nothing */
#define __stdcall /* nothing */
#endif
// Prior definition that is good but produces many warnings on Haiku.
//#if !defined(_MSC_VER) && !defined(__cdecl)
//#define __cdecl /* nothing */
//#endif
//#if !defined(_MSC_VER) && !defined(__stdcall)
//#define __stdcall /* nothing */
//#endif

#ifndef _MSC_VER
#define __try /* nothing */
#define __finally /* nothing */
#endif

#ifdef __cplusplus
#define M3_EXTERN_C         extern "C"
#define M3_EXTERN_C_BEGIN   extern "C" {
#define M3_EXTERN_C_END     }
#define M3EXTERNC_BEGIN     extern "C" {
#define M3_EXTERNC_BEGIN    extern "C" {
#define M3EXTERNC_END       }
#define M3_EXTERNC_END      }
#else
#define M3_EXTERN_C         /* nothing */
#define M3EXTERNC_BEGIN     /* nothing */
#define M3_EXTERN_C_BEGIN   /* nothing */
#define M3_EXTERN_C_END     /* nothing */
#define M3_EXTERNC_BEGIN    /* nothing */
#define M3EXTERNC_END       /* nothing */
#define M3_EXTERNC_END      /* nothing */
#endif

#define AGGREGATE_TYPEDEF(aggr, x) aggr x; typedef aggr x x;
#define STRUCT_TYPEDEF(x) AGGREGATE_TYPEDEF (struct, x)
#define UNION_TYPEDEF(x)  AGGREGATE_TYPEDEF (union, x)

STRUCT_TYPEDEF(linger)
STRUCT_TYPEDEF(sockaddr)
STRUCT_TYPEDEF(sockaddr_storage)
STRUCT_TYPEDEF(sockaddr_un)  // unix fifo socket address
STRUCT_TYPEDEF(sockaddr_in)  // internet ipv4 socket address
STRUCT_TYPEDEF(sockaddr_in6) // internet ipv6 socket address
STRUCT_TYPEDEF(in_addr)      // 4 bytes within sockaddr_in
STRUCT_TYPEDEF(in6_addr)     // 16 bytes within sockaddr_in

// Idealized m3 forms.
STRUCT_TYPEDEF(m3_sockaddr_un)  // idealized m3_sockaddr_un, does not necessarily match native
STRUCT_TYPEDEF(m3_sockaddr_in)  // idealized m3_sockaddr_in, does not necessarily match native
STRUCT_TYPEDEF(m3_sockaddr_in6) // idealized m3_sockaddr_in6, does not necessarily match native
STRUCT_TYPEDEF(m3_in_addr)      // idealized m3_in_addr, does not necessarily match native
//STRUCT_TYPEDEF(m3_in6_addr);  // use in6_addr instead

UNION_TYPEDEF(M3SockAddrUnionAll)
UNION_TYPEDEF(NativeSockAddrUnionAll)

/* m3name vs. cname is structured carefully to deal with identifiers
   being #defined in headers, such as on NetBSD. For example, given:
  #define foo bar
  int foo(int);
  M3WRAP1(int, foo, int)

we want:
int Cstdlib__foo (int i) { return bar (i); }
             ^^^                  ^^^
We want the define to affect the body of the wrapper, but not its name.
We want m3name to not undergo further evaluation, but we do want cname to.
This should be achieved by immediate prepending with "__", however even this
can likely fail if __foo is #defined. We take our chances.

__malloc is #defined on OpenBSD, so we can't even do __#name.
It is possible we could win by using just _malloc as the intermediate.
However we selectively don't use token pasting, e.g. in the NO_SWITCHING macros.
Also #undef __foo as needed.
*/

M3EXTERNC_BEGIN
void __cdecl Scheduler__DisableSwitching (void);
void __cdecl Scheduler__EnableSwitching (void);
M3EXTERNC_END

#define M3PASTE_(a, b) a##b
#define M3PASTE(a, b) M3PASTE_(a, b)

#define M3PASTE3_(a, b, c) a##b##c
#define M3PASTE3(a, b, c) M3PASTE3_(a, b, c)

#define M3WRAP(ret, m3name, cname, in, out)                                 \
    M3EXTERNC_BEGIN ret __cdecl M3PASTE(M3MODULE, m3name) in                \
    {                                                                       \
        ret return_value;                                                   \
        return_value = cname out;                                           \
        return return_value;                                                \
    } M3EXTERNC_END

#define M3WRAP_NO_SWITCHING(ret, m3name, cname, in, out) \
    M3EXTERNC_BEGIN ret __cdecl m3name in                \
    {                                                    \
        ret return_value;                                \
        Scheduler__DisableSwitching ();                  \
        return_value = cname out;                        \
        Scheduler__EnableSwitching ();                   \
        return return_value;                             \
    } M3EXTERNC_END

#define M3WRAP_RETURN_VOID(m3name, cname, in, out)       \
    M3EXTERNC_BEGIN void __cdecl m3name in               \
    {                                                    \
        cname out;                                       \
    } M3EXTERNC_END

#define M3WRAP_RETURN_VOID_NO_SWITCHING(m3name, cname, in, out) \
    M3EXTERNC_BEGIN void __cdecl m3name in                      \
    {                                                           \
        Scheduler__DisableSwitching ();                         \
        cname out;                                              \
        Scheduler__EnableSwitching ();                          \
    } M3EXTERNC_END

#ifdef _WIN32
#define M3WRAP_(ret, m3name, cname, in, out)    M3WRAP(ret, m3name, _##cname, in, out)
#else
#define M3WRAP_(ret, m3name, cname, in, out)    M3WRAP(ret, m3name, cname, in, out)
#endif

// Parameters a, b, c, etc. are the types of the first, second, third, etc. parameters.
// i, j, k etc. are the first, second, third, etc. parameter values.
#define M3WRAP1_RETURN_VOID(name, a)    M3WRAP_RETURN_VOID(M3PASTE3(M3MODULE, __, name), name, (a i), (i))
#define M3WRAP2_RETURN_VOID(name, a, b) M3WRAP_RETURN_VOID(M3PASTE3(M3MODULE, __, name), name, (a i, b j), (i, j))

#define M3WRAP0(ret, name)             M3WRAP(ret, __##name, name, (void),               ())
#define M3WRAP1(ret, name, a)          M3WRAP(ret, __##name, name, (a i),                (i))
#define M3WRAP2(ret, name, a, b)       M3WRAP(ret, __##name, name, (a i, b j),           (i, j))
#define M3WRAP3(ret, name, a, b, c)    M3WRAP(ret, __##name, name, (a i, b j, c k),      (i, j, k))
#define M3WRAP4(ret, name, a, b, c, d) M3WRAP(ret, __##name, name, (a i, b j, c k, d m), (i, j, k, m))
#define M3WRAP5(ret, name, a, b, c, d, e) M3WRAP(ret, __##name, name, (a i, b j, c k, d m, e n), (i, j, k, m, n))
#define M3WRAP6(ret, name, a, b, c, d, e, f) M3WRAP(ret, __##name, name, (a i, b j, c k, d m, e n, f o), (i, j, k, m, n, o))

// Leading underscore is placed on C name, for Windows, like open vs. _open.
#define M3WRAP0_(ret, name)           M3WRAP_(ret, __##name, name, (void),               ())
#define M3WRAP1_(ret, name, a)        M3WRAP_(ret, __##name, name, (a i),                (i))
#define M3WRAP2_(ret, name, a, b)     M3WRAP_(ret, __##name, name, (a i, b j),           (i, j))
#define M3WRAP3_(ret, name, a, b, c)  M3WRAP_(ret, __##name, name, (a i, b j, c k),      (i, j, k))

#include <sys/types.h>
#include <sys/stat.h>
#include <assert.h>
#include <stddef.h>
#include <stdio.h>
#include <stdlib.h>
#include <time.h>
#include <errno.h>
#include <fcntl.h>
#include <string.h>
#include <signal.h>
#include <math.h>
#include <limits.h>
#include <setjmp.h>

// TODO Remove this. It supports very old toolsets.
#ifdef _WIN32
#ifndef WIN32
#define WIN32
#endif

#include <direct.h>
#include <io.h>
#include <process.h>
typedef ptrdiff_t ssize_t;
#else
#include <sys/ioctl.h>
#include <sys/mman.h>
#include <sys/socket.h>
#include <sys/time.h>
/* Check if this system really supports _TIME64_T, i.e. Tru64 v5.1 or later. */
#if defined(_TIME64_T) && !defined(TIMEVAL64TO32) && !defined(TIMEVAL32TO64)
#undef _TIME64_T
#endif
#include <sys/wait.h>
#include <dirent.h>
#include <grp.h>
#include <netdb.h>
#include <pthread.h>
#include <unistd.h>
#include <pwd.h>
#include <semaphore.h>
#if !(defined(__OpenBSD__) || defined(__CYGWIN__) || defined(__vms) || defined(__HAIKU__))
#include <sys/ucontext.h>
#ifndef __APPLE__
/* OpenBSD 4.3, 4.7: ucontext.h doesn't exist, ucontext_t is in signal.h
   Cygwin: no state provided to signal handler?
   Apple: http://tinderbox.elegosoft.com/tinderbox/cgi-bin/gunzip.cgi\
          ?tree=cm3&brief-log=1258879870.10595#err9
          /usr/include/ucontext.h:42:2: error: #error ucontext routines are
          deprecated, and require _XOPEN_SOURCE to be defined
          http://duriansoftware.com/joe/PSA:-avoiding-the-%22ucontext-\
          routines-are-deprecated%22-error-on-Mac-OS-X-Snow-Leopard.html */
#include <ucontext.h>
#endif /* Apple */
#endif /* OpenBSD, Cygwin, VMS */
#define ZeroMemory(a, b) (memset((a), 0, (b)))
// Posix says include <arpa/inet.h>, but FreeBSD 4 inet.h
// requires netinet/in.h
#include <netinet/in.h>
#include <arpa/inet.h>
#include <netinet/tcp.h>
#include <sys/un.h>
#include <sys/select.h>
#endif /* Win32 vs. Posix */

#define ZERO_MEMORY(a) (ZeroMemory(&(a), sizeof(a)))

#ifdef __INTERIX
#include <utime.h>
#endif

#if UCHAR_MAX == 0x0FFUL
typedef   signed char        INT8; // TODO: C99
typedef unsigned char       UINT8;
#else
#error unable to find 8bit integer
#endif
#if USHRT_MAX == 0x0FFFFUL
typedef          short      INT16;
typedef unsigned short     UINT16;
#else
#error unable to find 16bit integer
#endif
#if UINT_MAX == 0x0FFFFFFFFUL
typedef          int        INT32;
typedef unsigned int       UINT32;
#elif ULONG_MAX == 0x0FFFFFFFFUL
typedef          long       INT32;
typedef unsigned long      UINT32;
#else
#error unable to find 32bit integer
#endif

// Support pre-C99 for Windows and VMS and any system with an __int64 macro.
#if !defined(_LONGLONG) && (defined(_MSC_VER) || defined(__DECC) || defined(__DECCXX) || defined(__int64))
typedef          __int64    INT64;
typedef unsigned __int64   UINT64;
#else
typedef          long long  INT64;
typedef unsigned long long UINT64;
#endif

typedef unsigned char BOOLEAN;
typedef float REAL;
typedef double LONGREAL;
typedef double EXTENDED;
#if defined(__cplusplus) || __STDC__
//Match m3c, so all the files can be concatenated.
// TODO change m3c to cast to char* and not depend on ADDRESS being either.
// Maybe caddr_t.
//typedef void* ADDRESS;
typedef char* ADDRESS;
#else
typedef char* ADDRESS;
#endif
typedef ADDRESS TEXT;
#define MUTEX MUTEX
typedef ADDRESS MUTEX;

#ifdef __cplusplus
extern "C" {
#endif

// On all known systems, sizeof(size_t) == sizeof(intptr_t) == sizeof(void*).
// Except VMS.
// On VMS/Alpha confirmed April 2021:
//   size_t is always 32bits, allocations are limited to 4GB.
//   void* is 32bits or 64bits, depending on command line and pragmas
//    e.g. cc /pointer_size=64
//   intptr_t is always 64bits, can always store a pointer.
// It is quite possible that VMS/IA64 and VMS/AMD64 are the same.
// These environments provide maximum compatibility with older 32bit code,
//  while still changing instruction set.
/* commented out is correct, but so is the #else */
/*#if defined(_WIN64) || __INITIAL_POINTER_SIZE == 64 || defined(__LP64__) || defined(_LP64) || __WORDSIZE == 64*/
#if __INITIAL_POINTER_SIZE == 64
typedef INT64 INTEGER;
typedef UINT64 WORD_T;
#else
typedef ptrdiff_t INTEGER;
typedef size_t WORD_T;
#endif

#define const_INTEGER const_INTEGER /* inhibit m3c type that lacks const */
typedef const INTEGER const_INTEGER;

// Something (m3front?) is indecisive as to if Word__T is INTEGER or INT64.
// Or rather, if functions like Word__Divide, traffic in INTEGER or
// Word__T. This could be m3front vs. Word.ig. This combines poorly
// with Word__T sometimes being INT64. They do not match. As well,
// between INT64 and INTEGER, INTEGER is preferred for portability.
// But having actual unsigned types might be good too.
//typedef INTEGER WORD_T;
typedef INTEGER Word__T;
// TODO replace WORD_T with Word__T

#define INTEGER INTEGER /* Support concatenation with m3c output. */
#define WORD_T  WORD_T  /* Support concatenation with m3c output. */
#define Word__T Word__T /* Support concatenation with m3c output. */

/* LONGINT is always signed and exactly 64 bits. */
typedef INT64 LONGINT;

/* see Utypes.i3; we assert that these are large enough, they don't have
be exactly correctly sizes, and often are not */
typedef LONGINT m3_dev_t;
typedef INTEGER m3_gid_t;
typedef LONGINT m3_ino_t;
typedef INTEGER m3_mode_t;
typedef LONGINT m3_nlink_t;
typedef INTEGER m3_pid_t;
typedef ADDRESS m3_pthread_t;
typedef LONGINT m3_off_t;
typedef INTEGER m3_uid_t;

// These typenames allow for concatenating the hand written
// .h/.c files with the m3c output so it can all be compiled at once.
//
// The m3c output uses ifndef to guard its own definitions, which
// can vary subtly. For example "int" is a 32bit signed integer type, possibly long.
// "long" is signed and the same size as long, but can on some platforms be int instead,
// if int and long are the same size.
// Presently const is meaningless in Modula-3 and m3c, so const is ommited there,
// and could be ommited here as well.
//
// The names should match the .i3 files, assuming full qualification and replacing "." with "__".
#define Cstddef__ptrdiff_t          Cstddef__ptrdiff_t          /* inhibit m3c type */
#define Cstddef__size_t             Cstddef__size_t             /* inhibit m3c type */
#define Cstddef__ssize_t            Cstddef__ssize_t            /* inhibit m3c type */
#define Cstdio__FILE_star           Cstdio__FILE_star           /* inhibit m3c type */
#define Ctypes__char                Ctypes__char                /* inhibit m3c type */
#define Ctypes__char_star           Ctypes__char_star           /* inhibit m3c type */
#define Ctypes__char_star_star      Ctypes__char_star_star      /* inhibit m3c type */
#define Ctypes__const_char_star     Ctypes__const_char_star     /* inhibit m3c type */
#define Ctypes__const_int           Ctypes__const_int           /* inhibit m3c type */
#define Ctypes__const_void_star     Ctypes__const_void_star     /* inhibit m3c type */
#define Ctypes__int                 Ctypes__int                 /* inhibit m3c type */
#define Ctypes__int_star            Ctypes__int_star            /* inhibit m3c type */
#define Ctypes__long                Ctypes__long                /* inhibit m3c type */
#define Ctypes__size_t              Ctypes__size_t              /* inhibit m3c type */
#define Ctypes__unsigned_char_star  Ctypes__unsigned_char_star  /* inhibit m3c type */
#define Ctypes__unsigned            Ctypes__unsigned            /* inhibit m3c type */
#define Ctypes__unsigned_int        Ctypes__unsigned_int        /* inhibit m3c type */
#define Ctypes__unsigned_long       Ctypes__unsigned_long       /* inhibit m3c type */
#define Ctypes__unsigned_long_int   Ctypes__unsigned_long_int   /* inhibit m3c type */
#define Ctypes__void_star           Ctypes__void_star           /* inhibit m3c type */
#define OSConfigPosix__const_char_star OSConfigPosix__const_char_star /* inhibit m3c type */
#define ThreadPThread__pthread_t    ThreadPThread__pthread_t    /* inhibit m3c type */
#define Utypes__dev_t               Utypes__dev_t               /* inhibit m3c type */
#define Utypes__gid_t               Utypes__gid_t               /* inhibit m3c type */
#define Utypes__ino_t               Utypes__ino_t               /* inhibit m3c type */
#define Utypes__mode_t              Utypes__mode_t              /* inhibit m3c type */
#define Utypes__nlink_t             Utypes__nlink_t             /* inhibit m3c type */
#define Utypes__pid_t               Utypes__pid_t               /* inhibit m3c type */
#define Utypes__off_t               Utypes__off_t               /* inhibit m3c type */
#define Utypes__size_t              Utypes__size_t              /* inhibit m3c type */
#define Utypes__uid_t               Utypes__uid_t               /* inhibit m3c type */
#define WinBaseTypes__const_UINT32  WinBaseTypes__const_UINT32  /* inhibit m3c type */

typedef size_t        Cstddef__size_t;
typedef ssize_t       Cstddef__ssize_t; /* provided above for msc */
typedef ptrdiff_t     Cstddef__ptrdiff_t;
typedef FILE*         Cstdio__FILE_star;
typedef char          Ctypes__char;
typedef char*         Ctypes__char_star;
typedef char**        Ctypes__char_star_star;
typedef const char*   Ctypes__const_char_star;
typedef const char*   OSConfigPosix__const_char_star; // TODO remove redundancy
typedef const int     Ctypes__const_int;
typedef const void*   Ctypes__const_void_star;
typedef int           Ctypes__int;
typedef int*          Ctypes__int_star;
typedef long          Ctypes__long;
typedef unsigned char* Ctypes__unsigned_char_star;
typedef unsigned      Ctypes__unsigned;
typedef unsigned      Ctypes__unsigned_int;
typedef unsigned long Ctypes__unsigned_long;
typedef unsigned long Ctypes__unsigned_long_int;
typedef void*         Ctypes__void_star;
typedef m3_dev_t      Utypes__dev_t;
typedef m3_gid_t      Utypes__gid_t;
typedef m3_ino_t      Utypes__ino_t;
typedef m3_mode_t     Utypes__mode_t;
typedef m3_nlink_t    Utypes__nlink_t;
typedef m3_pid_t      Utypes__pid_t;
typedef m3_pthread_t  ThreadPThread__pthread_t;
typedef m3_off_t      Utypes__off_t;
typedef size_t        Utypes__size_t;             // redundant
typedef m3_uid_t      Utypes__uid_t;
typedef UINT32        WinBaseTypes__const_UINT32;

#define Csignal__Handler Csignal__Handler            /* inhibit m3c type */
typedef void (__cdecl*   Csignal__Handler)(int s);

#define Csetjmp__jmp_buf            Csetjmp__jmp_buf            /* inhibit m3c type */
#ifdef __sun /* Messy. See Csetjmp.c. */
typedef sigjmp_buf*   Csetjmp__jmp_buf;
#else
typedef jmp_buf*      Csetjmp__jmp_buf;
#endif

#define ContextC__T                    ContextC__T                    /* inhibit m3c type */
#define CoroutineUcontext__Arg         CoroutineUcontext__Arg         /* inhibit m3c type */
#define CoroutineUcontext__Entry       CoroutineUcontext__Entry       /* inhibit m3c type */
#define ContextC__untraced_ref_integer ContextC__untraced_ref_integer /* inhibit m3c type */

// CoroutineUcontext__Arg is a pointer to a struct (CoroutineUcontext.Arg) however,
// the C code does not derefernce it and the Modula-3 code defines a different name
// for it (hashed), then casts its address to this. Therefore, the struct
// does not need to be defined.
typedef void* CoroutineUcontext__Arg;
typedef void (__cdecl*CoroutineUcontext__Entry)(CoroutineUcontext__Arg);
struct ContextC__TValue;
typedef struct ContextC__TValue ContextC__TValue;
typedef        ContextC__TValue* ContextC__T;
typedef INTEGER* ContextC__untraced_ref_integer;

/*
 m3_pthread_t is void*.
 pthread_t might be any of: size_t, ptrdiff_t, int, void*, another pointer.
 pthread_t will not be larger than a pointer/size_t. (see Unix__Assertions)
 Only convert integers to/from integers, and pointer-sized integers to/from pointers.
 That is, for example, do NOT convert int <=> pointer.
 */
#define PTHREAD_TO_M3(x)   ((m3_pthread_t)(WORD_T)(x))
#if defined(__APPLE__)
#define PTHREAD_FROM_M3(x) ((mach_port_t)(WORD_T)(x))
#else
#define PTHREAD_FROM_M3(x) ((pthread_t)(WORD_T)(x))
#endif

// TODO: Dragonfly? Autoconf?
#if defined(__APPLE__) || defined(__FreeBSD__) || defined(__NetBSD__) || defined(__OpenBSD__)
#define HAS_STAT_FLAGS
#endif

struct m3_stat_t;
typedef struct m3_stat_t m3_stat_t;

#define Ustat__struct_stat_star Ustat__struct_stat_star /* inhibit m3c type */
typedef m3_stat_t* Ustat__struct_stat_star;

int __cdecl Ustat__fstat(int fd, m3_stat_t* m3st);
int __cdecl Ustat__lstat(const char* path, m3_stat_t* m3st);
int __cdecl Ustat__stat(const char* path, m3_stat_t* m3st);
#ifdef HAS_STAT_FLAGS
int __cdecl Ustat__fchflags(int fd, unsigned long flags);
int __cdecl Ustat__chflags(const char* path, unsigned long flags);
#endif

/*
socklen_t
win32: signed 32 bit
cygwin: signed 32 bit
hpux: size_t (unsigned 32bit or 64bit) subject to ifdef, big mess
everyone else: unsigned 32 bit

The values involved are all small and positive, and we convert carefully.
Note that socklen_t should be declared by system headers, but isn't always.

TODO: Consider changing m3_socklen_t to int.

HP-UX socklen_t is always size_t, but depending on defines, socklen_t or int are used.
*/
typedef WORD_T m3_socklen_t;
#ifndef __hpux
#if defined(__INTERIX) || (defined(__vms) && defined(_DECC_V4_SOURCE)) || defined(_WIN32)
typedef int socklen_t;
#elif defined(__vms)
typedef size_t socklen_t;
#endif
#define m3c_socklen_t socklen_t

#else /* hpux */
#ifdef _XOPEN_SOURCE_EXTENDED
typedef size_t/*socklen_t*/ m3c_socklen_t;
#else
typedef int/*not socklen_t*/ m3c_socklen_t;
#endif
#endif

#define Usocket__socklen_t Usocket__socklen_t /* inhibit m3c type */
typedef m3_socklen_t Usocket__socklen_t;

typedef struct {
/* verified to exactly match struct linger in UnixC.c, except for Cygwin */
    int onoff;
    int linger;
} m3_linger_t;

// This is very unusual. Maybe it should be changed.
// The C functions take a union. The .i3 files claim
// a pointer to one specific variant.
//
// Alternatively the C functions could also take the member
// and cast it back to the union.
//
// More alternatively, there should be separate C functions for each union
// variant and Modula-3 should chose.
#define       Usocket__struct_sockaddr_in_star Usocket__struct_sockaddr_in_star       /* inhibit m3c type */
#define Usocket__const_struct_sockaddr_in_star Usocket__const_struct_sockaddr_in_star /* inhibit m3c type */
#define Usocket__socklen_t_star                Usocket__socklen_t_star                /* inhibit m3c type */
#define Usocket__socklen_t                     Usocket__socklen_t                     /* inhibit m3c type */
typedef       M3SockAddrUnionAll*              Usocket__struct_sockaddr_in_star;
typedef const M3SockAddrUnionAll*              Usocket__const_struct_sockaddr_in_star;
typedef m3_socklen_t                           Usocket__socklen_t;
typedef m3_socklen_t*                          Usocket__socklen_t_star;

int __cdecl Usocket__listen(int s, int backlog);
int __cdecl Usocket__shutdown(int s, int how);
int __cdecl Usocket__socket(int af, int type, int protocol);
int __cdecl Usocket__bind(int s, const M3SockAddrUnionAll*, m3_socklen_t);
int __cdecl Usocket__connect(int s, const M3SockAddrUnionAll*, m3_socklen_t);
ssize_t __cdecl Usocket__sendto(int s, const void* msg, size_t length, int flags, const M3SockAddrUnionAll*, m3_socklen_t);
int __cdecl Usocket__setsockopt(int s, int level, int optname, const void* optval, m3_socklen_t len);
int __cdecl Usocket__getpeername(int s, M3SockAddrUnionAll*, m3_socklen_t*);
int __cdecl Usocket__getsockname(int s, M3SockAddrUnionAll*, m3_socklen_t*);
int __cdecl Usocket__accept(int s, M3SockAddrUnionAll*, m3_socklen_t*);
int __cdecl Usocket__getsockopt(int s, int level, int optname, void* optval, m3_socklen_t*);
ssize_t __cdecl Usocket__recvfrom(int s, void* buf, size_t len, int flags, M3SockAddrUnionAll*, m3_socklen_t*);

#ifndef _WIN32
DIR* __cdecl Udir__opendir(const char* a);
#define Udir__DIR_star Udir__DIR_star /* inhibit m3c type */
typedef DIR*           Udir__DIR_star;
#define Udir__struct_dirent_star Udir__struct_dirent_star /* inhibit m3c type */
STRUCT_TYPEDEF(dirent)
typedef dirent* Udir__struct_dirent_star;
#endif

// char* (caddr_t) instead of void* for default Solaris
typedef char* caddr_t;
typedef caddr_t Umman__caddr_t;
#define Umman__caddr_t Umman__caddr_t /* inhibit m3c type */
int __cdecl Umman__mprotect(caddr_t addr, size_t len, int prot);
void* __cdecl Umman__mmap(caddr_t addr, size_t len, int prot, int flags, int fd, m3_off_t off);
int __cdecl Umman__munmap(caddr_t addr, size_t len);

ssize_t __cdecl Uuio__read(int, void*, size_t);
ssize_t __cdecl Uuio__write(int, const void*, size_t);

typedef INT64 m3_time64_t;

#define Utime__time_t Utime__time_t /* inhibit m3c type */
typedef /*const*/ m3_time64_t Utime__time_t; /* TODO READONLY should imply const */

#ifndef _WIN32
m3_time64_t __cdecl Utime__time(m3_time64_t* tloc);
char* __cdecl Utime__ctime(/*TODO const*/ m3_time64_t* m);
#endif
void __cdecl Utime__tzset(void);

#if 1 /* Some compilers don't like this, will adjust as needed. e.g. old SGI */
#if __GNUC__ /* gcc 4.8 warns about unused local typedefs. */
#define GCC_ATTRIBUTE_UNUSED __attribute__ ((unused))
#else
#define GCC_ATTRIBUTE_UNUSED /* nothing */
#endif
#define M3_STATIC_ASSERT(expr) GCC_ATTRIBUTE_UNUSED typedef char M3PASTE(m3_static_assert, __LINE__)[(expr)?1:-1]
#else
#define M3_STATIC_ASSERT(expr) assert(expr)
#endif

#define M3_FIELD_SIZE(type, field) (sizeof((type*)0)->field)
#define M3_SIZE_THROUGH_FIELD(type, field) (offsetof(type, field) + M3_FIELD_SIZE(type, field))

void __cdecl Unix__Assertions(void);
void __cdecl Usocket__Assertions(void);

int __cdecl Unix__open(const char* path, int flags, m3_mode_t mode);
int __cdecl Unix__mkdir(const char* path, m3_mode_t mode);
int __cdecl Unix__ftruncate(int fd, m3_off_t length);
m3_off_t __cdecl Unix__lseek(int fd, m3_off_t offset, int whence);
m3_off_t __cdecl Unix__tell(int fd);
int __cdecl Unix__fcntl(int fd, INTEGER request, INTEGER arg);
int __cdecl Unix__ioctl(int fd, INTEGER request, ADDRESS argp);
int __cdecl Unix__mknod(const char* path, m3_mode_t mode, m3_dev_t dev);
m3_mode_t __cdecl Unix__umask(m3_mode_t newmask);

STRUCT_TYPEDEF(m3_hostent_t)

#define Unetdb__struct_hostent_star Unetdb__struct_hostent_star /* inhibit m3c type */
typedef m3_hostent_t* Unetdb__struct_hostent_star;

m3_hostent_t* __cdecl Unetdb__gethostbyname(const char* name, m3_hostent_t* m3);
m3_hostent_t* __cdecl Unetdb__gethostbyaddr(const char* addr, int len, int type, m3_hostent_t* m3);

STRUCT_TYPEDEF(m3_group_t)

#define Ugrp__struct_group_star Ugrp__struct_group_star /* inhibit m3c type */
typedef m3_group_t* Ugrp__struct_group_star;

m3_group_t* __cdecl Ugrp__getgrent(m3_group_t* m3group);
m3_group_t* __cdecl Ugrp__getgrgid(m3_group_t* m3group, m3_gid_t gid);
m3_group_t* __cdecl Ugrp__getgrnam(m3_group_t* m3group, const char* name);
void __cdecl Ugrp__setgrent(void);
void __cdecl Ugrp__endgrent(void);

int __cdecl Unix__link(const char* name1, const char* name2);
int __cdecl Unix__chmod(const char* path, m3_mode_t mode);
int __cdecl Unix__fchmod(int fd, m3_mode_t mode);
int __cdecl Unix__chown(const char* path, m3_uid_t owner, m3_gid_t group);
int __cdecl Unix__fchown(int fd, m3_uid_t owner, m3_gid_t group);
int __cdecl Unix__creat(const char* path, m3_mode_t mode);
int __cdecl Unix__dup(int oldd);

// TODO widen to INTEGER
UINT32 __cdecl Uin__ntohl(UINT32 x);
UINT16 __cdecl Uin__ntohs(UINT16 x);
UINT32 __cdecl Uin__htonl(UINT32 x);
UINT16 __cdecl Uin__htons(UINT16 x);

// TODO C should not use TEXT, i.e. for precise compacting GC.
const char*
__cdecl
M3toC__SharedTtoS(TEXT);

void
__cdecl
M3toC__FreeSharedS(TEXT, const char*);

TEXT
__cdecl
M3toC__CopyStoT(const char*);

TEXT
__cdecl
M3toC__StoT(const char*);

/* This MUST match DatePosix.i3.T.
 * The fields are ordered by size and alphabetically.
 */
#define DatePosix__T DatePosix__T /* inhibit m3c type */

typedef struct {
    const char* zone;
    int day;
    int gmt;     // boolean
    int hour;
    int minute;
    int month;
    int offset;
    int second;
    int unknown; // boolean
    int weekDay;
    int year;
} DatePosix__T;

void
__cdecl
DatePosix__FromTime(double t, INTEGER zone, DatePosix__T* date);

double
__cdecl
DatePosix__ToTime(/*const*/ DatePosix__T* date);

void
__cdecl
DatePosix__TypeCheck(/*const*/ DatePosix__T* d, INTEGER sizeof_DateT);

void
__cdecl
Scheduler__DisableScheduling(void);

void
__cdecl
Scheduler__EnableScheduling(void);

void
__cdecl
Process__RegisterExitor(void (__cdecl*)(void));

// GET_PC returns approximately size_t.
// Broadly speaking, try to keep most platform specific code here.

#if defined(__APPLE__)

#if defined(__i386__)
#define M3_HOST "I386_DARWIN"
#if __DARWIN_UNIX03
#define GET_PC(context) ((context)->uc_mcontext->__ss.__eip)
#else
#define GET_PC(context) ((context)->uc_mcontext->ss.eip)
#endif

#elif defined(__x86_64__)

#define M3_HOST "AMD64_DARWIN"

#if __DARWIN_UNIX03
#define GET_PC(context) ((context)->uc_mcontext->__ss.__rip)
#else
#define GET_PC(context) ((context)->uc_mcontext->ss.rip)
#endif

#elif defined(__ppc__) || defined(__ppc64__)

#if defined(__ppc64__)
#define M3_HOST "PPC64_DARWIN"
#else
#define M3_HOST "PPC_DARWIN"
#endif

#if __DARWIN_UNIX03
#define GET_PC(context) ((context)->uc_mcontext->__ss.__srr0)
#else
#define GET_PC(context) ((context)->uc_mcontext->ss.srr0)
#endif

#elif defined(__arm__) || defined(__arm64__)

#if defined(__arm64__)
#define M3_HOST "ARM64_DARWIN"
#else
#define M3_HOST "ARM32_DARWIN"
#endif

#if __DARWIN_UNIX03
#define GET_PC(context) (__darwin_arm_thread_state64_get_pc(context->uc_mcontext->__ss))
#else
#define GET_PC(context) (__darwin_arm_thread_state64_get_pc(context->uc_mcontext->ss))
#endif
#else
#error Unknown __APPLE__ target
#endif

#elif defined(__HAIKU__)
#ifdef __amd64
#define GET_PC(context) ((context)->uc_mcontext.rip)
#define M3_HOST "AMD64_HAIKU"
#else
#error Unsupported Haiku
#endif

#elif defined(__osf__)

#define GET_PC(context) ((context)->uc_mcontext.sc_pc)

#if defined(__alpha__)
#define M3_HOST "ALPHA_OSF"
#endif

#elif defined(__OpenBSD__)

#if defined(__amd64)

#define M3_HOST "AMD64_OPENBSD"
#define GET_PC(context) ((context)->sc_rip)

#elif defined(__powerpc)
// unknown wordsize and endian; use uname
#define GET_PC(context) ((context)->sc_frame.srr0)
#else
#define GET_PC(context) ((context)->sc_pc)
#endif

#elif defined(__linux) && defined(__sparc) && __WORDSIZE == 64
#define M3_HOST "SPARC64_LINUX"
#define GET_PC(context) ((context)->uc_mcontext.mc_gregs[REG_PC])

#elif defined(__sun) || defined(__sparc)

#if defined(REG_PC)
#define GET_PC(context) ((context)->uc_mcontext.gregs[REG_PC])
#elif defined(__sun) && defined(__i386) && (PC == 14)
#define M3_HOST "I386_SOLARIS"
#define GET_PC(context) ((context)->uc_mcontext.gregs[PC])
#elif defined(__sun) && defined(__sparc) && (PC == 1)
#define GET_PC(context) ((context)->uc_mcontext.gregs[PC])
#else
#error unknown __sun/__sparc target
#endif

#elif defined(__linux)

/* see /src/glibc-2.14/sysdeps/unix/sysv/linux/x/sigcontextinfo.h */
#if defined(__i386)
#define M3_HOST "I386_LINUX"
#define GET_PC(context) ((context)->uc_mcontext.gregs[REG_EIP])
#elif defined(__amd64)
#define M3_HOST "AMD64_LINUX"
#define GET_PC(context) ((context)->uc_mcontext.gregs[REG_RIP])
#elif defined(__powerpc)
// ambiguous endian and wordsize; use uname
#define GET_PC(context) ((context)->uc_mcontext.uc_regs->gregs[PT_NIP])
#elif defined(__aarch64__)
#define M3_HOST "ARM64_LINUX"
#define GET_PC(context) ((context)->uc_mcontext.pc)
#elif defined(__arm__)
#define M3_HOST "ARM32_LINUX"
#define GET_PC(context) ((context)->uc_mcontext.arm_pc)
#elif defined(__alpha__)
#define M3_HOST "ALPHA_LINUX"
#define GET_PC(context) ((context)->uc_mcontext.sc_pc)
#elif defined(__ia64__)
#define M3_HOST "IA64_LINUX"
#define GET_PC(context) ((context)->uc_mcontext.sc_ip)
#elif defined(__sh__)
// unknown endian and wordsize; use uname
#error untested __linux target
#define GET_PC(context) ((context)->uc_mcontext.sc_pc)
#elif defined(__s390__)
// unknown wordsize; use uname
#error untested __linux target
#define GET_PC(context) ((context)->uc_mcontext.sregs.regs.psw.addr
#elif defined(__riscv) || defined(__riscv64)
#if defined(__riscv64)
#define M3_HOST "RISCV64_LINUX"
#else
#define M3_HOST "RISCV32_LINUX"
#endif
#define GET_PC(context) ((context)->uc_mcontext.__gregs[REG_PC])
#else
#error unknown __linux target
#endif

#elif defined(__NetBSD__)
#define GET_PC(context) (_UC_MACHINE_PC(context))

#elif defined(__FreeBSD__)
#if defined(__amd64)
#define GET_PC(context) ((context)->uc_mcontext.mc_rip)
#elif defined(__i386)
#define GET_PC(context) ((context)->uc_mcontext.mc_eip)
#else
#define GET_PC(context) ((context)->uc_mcontext.mc_pc)
#endif

#elif defined(__hpux) && defined(__ia64) && (defined(_LP64) || defined(__LP64__))
#define M3_HOST "IA64_HPUX64"
// GET_PC does not fit, see RTSignalC.c

#elif defined(__hpux) && defined(__ia64) && defined(_ILP32)
#define M3_HOST "IA64_HPUX32"
// GET_PC does not fit, see RTSignalC.c

#elif defined(__mips)
#define GET_PC(context) ((context)->uc_mcontext.scp_pc.lo)
#elif defined(__hppa)
#define GET_PC(context) ((context)->uc_mcontext.scp_pc)
#elif defined(__i386)
#define GET_PC(context) ((context)->uc_mcontext.ss.eip)
#elif defined(__amd64)
#define GET_PC(context) ((context)->uc_mcontext.ss.rip)
#elif defined(__powerpc)
#define GET_PC(context) ((context)->uc_mcontext.ss.srr0)
#endif

// m3_time_t is the time_t-ish of the underlying system.
// If there is an explicitly 64bit type, use that. e.g. Tru64.
#ifdef _TIME64_T
typedef time64_t m3_time_t;
#else
typedef time_t m3_time_t;
#endif

// Is this portable?
STRUCT_TYPEDEF(passwd)

#define Upwd__struct_passwd_star Upwd__struct_passwd_star /* inhibit m3c type */
typedef passwd* Upwd__struct_passwd_star;

passwd* __cdecl Upwd__getpwuid(m3_uid_t);
passwd* __cdecl Upwd__getpwnam(char*);

m3_pid_t __cdecl Uprocess__getpid (void);

#if 0 // This would be nice, but structural type equivalence gets
      // in the way. There are other equivalent types
      // and the names clash.
#define Unix__PipeArray Unix__PipeArray /* inhibit m3c type */
STRUCT_TYPEDEF(Unix__PipeArray)
struct Unix__PipeArray
{
    int files [2];
};

int __cdecl Unix__pipe (Unix__PipeArray* files);

#else

int __cdecl UnixC__pipe (int* files);

#endif

// See RTMachine.i3, RTStack.i3, RTStackC.c
// TYPE Frame = RECORD pc, sp: ADDRESS END;
#define RTStack__Frame RTStack__Frame /* inhibit m3c type */
STRUCT_TYPEDEF(RTStack__Frame)
struct RTStack__Frame
{
  ADDRESS pc; // program counter, instruction pointer, etc.
  ADDRESS sp; // stack pointer
};

#if 0

#define RT0__Binder       RT0__Binder       /* inhibit m3c type */
#define RT0__Fingerprint  RT0__Fingerprint  /* inhibit m3c type */
#define RT0__TypeInitProc RT0__TypeInitProc /* inhibit m3c type */
#define RT0__Revelation   RT0__Revelation   /* inhibit m3c type */
#define RT0__Typecell     RT0__Typecell     /* inhibit m3c type */
#define RT0__TypeInitProc RT0__TypeInitProc /* inhibit m3c type */
#define RT0__TypeLink     RT0__TypeLink     /* inhibit m3c type */
#define RT0__ModulePtr    RT0__ModulePtr    /* inhibit m3c type */
STRUCT_TYPEDEF(RT0__Module)
STRUCT_TYPEDEF(RT0__Import)
STRUCT_TYPEDEF(RT0__Revelation)
STRUCT_TYPEDEF(RT0__Typecell)
STRUCT_TYPEDEF(RT0__Brand)
STRUCT_TYPEDEF(RT0__TypeLink)
STRUCT_TYPEDEF(RT0__Proc)

typedef unsigned char    RT0__Fingerprint[8]; // 64 bits, no alignment (but actually 2pointer aligned)
typedef RT0__Module* (__cdecl*RT0__Binder)(INTEGER mode);
typedef         void (__cdecl*RT0__TypeInitProc)(ADDRESS);
typedef RT0__Module* RT0__ModulePtr;

void __cdecl RTLinker__PrintModule(RT0__Module* module);

struct RT0__Proc // one of these is generated for each top-level procedure
{
    ADDRESS     proc;
    const char* name;
};

struct RT0__TypeLink
{
    RT0__Typecell* defn;     // initially a pointer to the next TypeLink
    INTEGER        typecode; // initially the compile-time UID of the type
};

struct RT0__Brand
{
    INTEGER length;
    char chars [1 /* length */];
};

struct RT0__Revelation
{
    INTEGER lhs_id;
    INTEGER rhs_id;
};

struct RT0__Import // Keep this in sync with RT0.i3
{                  // one of these is generated for each imported interface reference
    RT0__Module* import;
    RT0__Binder  binder; // returns "import" pointer
    RT0__Import* next;
};

struct RT0__Typecell // Keep this in sync with RT0.i3
{
    INTEGER           typecode;       // Typecode
    INTEGER           selfID;
    RT0__Fingerprint  fp;
    BOOLEAN           traced;
    UINT8             kind;           // == ORD (TypeKind = { Unknown, Ref, Obj, Array })
    UINT8             link_state;
    UINT8             dataAlignment;
    INTEGER           dataSize;
    ADDRESS           type_map;       // RTTypeMap.T
    ADDRESS           gc_map;         // reduced RTTypeMap.T for collector
    ADDRESS           type_desc;      // enhanced RTTipe map for new pickles
    RT0__TypeInitProc initProc;       // called by NEW
    RT0__Brand*       brand_ptr;
    const char*       name;
    RT0__Typecell*    next;
};

struct RT0__Module
// Keep this in sync with RT0.i3
// allocated at offset 0 of each compilation unit's global data
{
    const char*      file;           //  0
    RT0__Typecell*   type_cells;     //  4  8
    RT0__TypeLink*   type_cell_ptrs; //  8 16
    RT0__Revelation* full_rev;       // 12 24
    RT0__Revelation* partial_rev;    // 16 32
    RT0__Proc*       proc_info;      // 20 40
    ADDRESS          try_scopes;     // 24 48
    ADDRESS          var_map;        // 28 56
    ADDRESS          gc_map;         // 32 64
    RT0__Import*     imports;        // 36 72
    INTEGER          link_state;     // 40 80
    RT0__Binder      binder;         // 44 88
    INTEGER          gc_flags;       // 48 96
};

#endif

#ifndef _WIN32
// TODO Do we need single and double underscores?
#define ThreadPThread_pthread_mutex_t  ThreadPThread_pthread_mutex_t  /* inhibit m3c type */
#define ThreadPThread_pthread_cond_t   ThreadPThread_pthread_cond_t   /* inhibit m3c type */
#define ThreadPThread__pthread_mutex_t ThreadPThread__pthread_mutex_t /* inhibit m3c type */
#define ThreadPThread__pthread_cond_t  ThreadPThread__pthread_cond_t  /* inhibit m3c type */
#define ThreadPThread__const_pthread_mutex_t ThreadPThread__const_pthread_mutex_t /* inhibit m3c type */
#define ThreadPThread__const_pthread_cond_t  ThreadPThread__const_pthread_cond_t  /* inhibit m3c type */
typedef               pthread_mutex_t* ThreadPThread_pthread_mutex_t;
typedef               pthread_cond_t*  ThreadPThread_pthread_cond_t;
typedef               pthread_mutex_t* ThreadPThread__pthread_mutex_t;
typedef               pthread_cond_t*  ThreadPThread__pthread_cond_t;
typedef               pthread_mutex_t* const ThreadPThread__const_pthread_mutex_t;
typedef               pthread_cond_t*  const ThreadPThread__const_pthread_cond_t;

#define ThreadInternal__FDSet ThreadInternal__FDSet /* inhibit m3c type */
typedef                fd_set ThreadInternal__FDSet;

#define ThreadPThread__siginfo_t ThreadPThread__siginfo_t /* inhibit m3c type */
typedef siginfo_t ThreadPThread__siginfo_t;
#endif

#define ThreadPThread__Activation ThreadPThread__Activation /* inhibit m3c type */
typedef void* ThreadPThread__Activation;

#define ThreadPThread__start_routine_t ThreadPThread__start_routine_t /* inhibit m3c type */
typedef void* (__cdecl*ThreadPThread__start_routine_t)(void*);

#define ThreadPThread__ProcessThreadStack ThreadPThread__ProcessThreadStack
typedef void (__cdecl*ThreadPThread__ProcessThreadStack)(void* start, void* limit);

#define RT0__Binder RT0__Binder /* inhibit m3c type */
// The correct type for RT0__Binder is:
// typedef RT0__Module* (__cdecl*RT0__Binder)(INTEGER mode);
// but we cannot use that due to type collision hashes,
// until/unless significant m3c changes.
// This works:
typedef void (__cdecl*M3PROC)(void); // from MxGen.m3
typedef M3PROC RT0__Binder;
// The actual use and pass casts and so getting the type correct here does
// not really matter. What matters is that it is the same in all function
// declarations/definitions.

struct _BY_HANDLE_FILE_INFORMATION;
struct _FILETIME;
struct _OVERLAPPED;
struct _SYSTEMTIME;
struct _TIME_ZONE_INFORMATION;

#define WinBase__const_FILETIME_star                WinBase__const_FILETIME_star                /* inhibit m3c type */
#define WinBase__const_SYSTEMTIME_star              WinBase__const_SYSTEMTIME_star              /* inhibit m3c type */
#define WinBase__const_TIME_ZONE_INFORMATION_star   WinBase__const_TIME_ZONE_INFORMATION_star   /* inhibit m3c type */
#define WinBase__LPSYSTEMTIME                       WinBase__LPSYSTEMTIME                       /* inhibit m3c type */
#define WinBase__PBY_HANDLE_FILE_INFORMATION        WinBase__PBY_HANDLE_FILE_INFORMATION        /* inhibit m3c type */
#define WinBase__PFILETIME                          WinBase__PFILETIME                          /* inhibit m3c type */
#define WinBase__POVERLAPPED                        WinBase__POVERLAPPED                        /* inhibit m3c type */
#define WinBase__PSYSTEMTIME                        WinBase__PSYSTEMTIME                        /* inhibit m3c type */
#define WinBase__PTIME_ZONE_INFORMATION             WinBase__PTIME_ZONE_INFORMATION             /* inhibit m3c type */
#define WinBaseTypes__BOOL                          WinBaseTypes__BOOL                          /* inhibit m3c type */
#define WinBaseTypes__PCSTR                         WinBaseTypes__PCSTR                         /* inhibit m3c type */
#define WinBaseTypes__PSTR                          WinBaseTypes__PSTR                          /* inhibit m3c type */
#define WinBaseTypes__PUINT32                       WinBaseTypes__PUINT32                       /* inhibit m3c type */
#define WinBaseTypes__UINT32                        WinBaseTypes__UINT32                        /* inhibit m3c type */
#define WinNT__SECURITY_INFORMATION                 WinNT__SECURITY_INFORMATION                 /* inhibit m3c type */
#define WinNT__PSECURITY_DESCRIPTOR                 WinNT__PSECURITY_DESCRIPTOR                 /* inhibit m3c type */

typedef const struct _FILETIME*                 WinBase__const_FILETIME_star;
typedef const struct _SYSTEMTIME*               WinBase__const_SYSTEMTIME_star;
typedef const struct _TIME_ZONE_INFORMATION*    WinBase__const_TIME_ZONE_INFORMATION_star;
typedef struct _SYSTEMTIME*                     WinBase__LPSYSTEMTIME;
typedef struct _FILETIME*                       WinBase__PFILETIME;
typedef struct _SYSTEMTIME*                     WinBase__PSYSTEMTIME;
typedef struct _TIME_ZONE_INFORMATION*          WinBase__PTIME_ZONE_INFORMATION;
typedef struct _BY_HANDLE_FILE_INFORMATION*     WinBase__PBY_HANDLE_FILE_INFORMATION;
typedef struct _OVERLAPPED*                     WinBase__POVERLAPPED;
typedef int                                     WinBaseTypes__BOOL;
typedef char*                                   WinBaseTypes__PSTR;
typedef const char*                             WinBaseTypes__PCSTR;
typedef unsigned long*                          WinBaseTypes__PUINT32; // even on 64bit Windows
typedef unsigned long                           WinBaseTypes__UINT32;  // even on 64bit Windows
typedef void*                                   WinNT__PSECURITY_DESCRIPTOR;
typedef unsigned long                           WinNT__SECURITY_INFORMATION;

#ifdef __cplusplus
} /* extern "C" */
#endif

// from m3c
// http://c.knowcoding.com/view/23699-portable-alloca.html
// Find a good version of alloca.
#ifndef alloca
# ifdef __GNUC__
#  define alloca __builtin_alloca
//This was thought to handle VMS but has never been tested.
//# elif !defined(__osf__) && (defined(__DECC) || defined(__DECCXX))
//#  define alloca(x) __ALLOCA(x)
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
/* This is verified correct for Solaris, Tru64/OSF1, HP-UX, documented correct for AIX. TODO: Irix, etc. */
#  include <alloca.h>
# endif
#endif

#endif
