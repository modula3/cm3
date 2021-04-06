#if _MSC_VER > 1000
#pragma once
#endif

#ifndef INCLUDED_M3CORE_H
#define INCLUDED_M3CORE_H

#define _NO_CRT_STDIO_INLINE /* Do not accidentally export printf. */
#ifdef _MSC_VER
#define _CRT_SECURE_NO_DEPRECATE
#define _CRT_NONSTDC_NO_DEPRECATE
#pragma warning(disable:4616) /* there is no warning x (unavoidable if targeting multiple compiler versions) */
#pragma warning(disable:4619) /* there is no warning x (unavoidable if targeting multiple compiler versions) */
#pragma warning(disable:4115) /* named type definition in parentheses */
#pragma warning(disable:4100) /* unused parameter */
#pragma warning(disable:4201) /* nonstandard extension: nameless struct/union */
#pragma warning(disable:4214) /* nonstandard extension: bitfield other than int */
#pragma warning(disable:4514) /* unused inline function removed */
#pragma warning(disable:4705) /* statement has no effect for merely using assert() at -W4 */
#pragma warning(disable:4209) /* nonstandard extension: benign re-typedef */
#pragma warning(disable:4226) /* nonstandard extension: __export */
#pragma warning(disable:4820) /* padding inserted */
#pragma warning(disable:4255) /* () change to (void) */
#pragma warning(disable:4668) /* #if of undefined symbol */
#endif

#ifdef _WIN32
#include "ws2tcpip.h"
#include "wspiapi.h"
#else
typedef int BOOL;
#define TRUE 1
#define FALSE 0
#endif

#if __GNUC__ >= 4 && !defined(__osf__) && !defined(__CYGWIN__)
#define M3_HAS_VISIBILITY 1
#else
#define M3_HAS_VISIBILITY 0
#endif

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

/* http://gcc.gnu.org/wiki/Visibility */
/* Helpers for shared library support */
#if M3_HAS_VISIBILITY
#ifdef __APPLE__
#define M3_DLL_EXPORT __attribute__ ((visibility("default")))
#else
#define M3_DLL_EXPORT __attribute__ ((visibility("protected")))
#endif
#define M3_DLL_LOCAL  __attribute__ ((visibility("hidden")))
#else
#define M3_DLL_EXPORT /* nothing */
#define M3_DLL_LOCAL  /* nothing */
#endif

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

#if defined(__sun) && defined(__sparc) && !defined(__MAKECONTEXT_V2_SOURCE)
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

#if !defined(_MSC_VER) && !defined(__cdecl)
#define __cdecl /* nothing */
#endif

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

#define STRUCT_AND_TYPEDEF(x) struct x; typedef struct x x;

STRUCT_AND_TYPEDEF(linger)
STRUCT_AND_TYPEDEF(sockaddr)
STRUCT_AND_TYPEDEF(sockaddr_storage)
STRUCT_AND_TYPEDEF(sockaddr_un)
STRUCT_AND_TYPEDEF(sockaddr_in)
STRUCT_AND_TYPEDEF(sockaddr_in6)
STRUCT_AND_TYPEDEF(in6_addr)
STRUCT_AND_TYPEDEF(in_addr)

STRUCT_AND_TYPEDEF(m3_sockaddr_un)
STRUCT_AND_TYPEDEF(m3_sockaddr_in)
STRUCT_AND_TYPEDEF(m3_sockaddr_in6)
STRUCT_AND_TYPEDEF(m3_in_addr)
//STRUCT_AND_TYPEDEF(m3_in6_addr); // use in6_addr instead

union M3SockAddrUnionAll;
union NativeSockAddrUnionAll;
typedef union M3SockAddrUnionAll M3SockAddrUnionAll;
typedef union NativeSockAddrUnionAll NativeSockAddrUnionAll;

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
#define M3WRAP(ret, m3name, cname, in, out)                                 \
    M3EXTERNC_BEGIN M3_DLL_EXPORT ret __cdecl M3PASTE(M3MODULE, m3name) in  \
    {                                                                       \
        ret return_value;                                                   \
        return_value = cname out;                                           \
        return return_value;                                                \
    } M3EXTERNC_END

#define M3WRAP_NO_SWITCHING(ret, m3name, cname, in, out) \
    M3EXTERNC_BEGIN M3_DLL_EXPORT ret __cdecl m3name in  \
    {                                                    \
        ret return_value;                                \
        Scheduler__DisableSwitching ();                  \
        return_value = cname out;                        \
        Scheduler__EnableSwitching ();                   \
        return return_value;                             \
    } M3EXTERNC_END

#define M3WRAP_RETURN_VOID(m3name, cname, in, out)       \
    M3EXTERNC_BEGIN M3_DLL_EXPORT void __cdecl m3name in \
    {                                                    \
        cname out;                                       \
    } M3EXTERNC_END

#define M3WRAP_RETURN_VOID_NO_SWITCHING(m3name, cname, in, out) \
    M3EXTERNC_BEGIN M3_DLL_EXPORT void __cdecl m3name in        \
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

#define M3WRAP0(ret, name)             M3WRAP(ret, __##name, name, (void),               ())
#define M3WRAP1(ret, name, a)          M3WRAP(ret, __##name, name, (a i),                (i))
#define M3WRAP2(ret, name, a, b)       M3WRAP(ret, __##name, name, (a i, b j),           (i, j))
#define M3WRAP3(ret, name, a, b, c)    M3WRAP(ret, __##name, name, (a i, b j, c k),      (i, j, k))
#define M3WRAP4(ret, name, a, b, c, d) M3WRAP(ret, __##name, name, (a i, b j, c k, d m), (i, j, k, m))
#define M3WRAP5(ret, name, a, b, c, d, e) M3WRAP(ret, __##name, name, (a i, b j, c k, d m, e n), (i, j, k, m, n))
#define M3WRAP6(ret, name, a, b, c, d, e, f) M3WRAP(ret, __##name, name, (a i, b j, c k, d m, e n, f o), (i, j, k, m, n, o))

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

#ifdef _WIN32
#ifndef WIN32
#define WIN32
#endif
#include <direct.h>
#include <io.h>
#include <winsock.h>
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
#if !(defined(__OpenBSD__) || defined(__CYGWIN__) || defined(__vms))
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
#endif /* Win32 vs. Posix */

#define ZERO_MEMORY(a) (ZeroMemory(&(a), sizeof(a)))

#ifdef __INTERIX
#include <utime.h>
#endif

#if UCHAR_MAX == 0x0FFUL
typedef   signed char        INT8;
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
#if defined(_MSC_VER) || defined(__DECC) || defined(__DECCXX) || defined(__int64)
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
typedef void* ADDRESS;
#else
typedef char* ADDRESS;
#endif
typedef ADDRESS TEXT;
typedef ADDRESS MUTEX;

#ifdef __cplusplus
extern "C" {
#endif

/* WORD_T/INTEGER are always exactly the same size as a pointer.
 * VMS sometimes has 32bit size_t/ptrdiff_t but 64bit pointers.
 */
/* commented out is correct, but so is the #else */
/*#if defined(_WIN64) || __INITIAL_POINTER_SIZE == 64 || defined(__LP64__) || defined(_LP64) || __WORDSIZE == 64*/
#if __INITIAL_POINTER_SIZE == 64
typedef INT64 INTEGER;
typedef UINT64 WORD_T;
#else
typedef ptrdiff_t INTEGER;
typedef size_t WORD_T;
#endif

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

#if defined(__APPLE__) || defined(__FreeBSD__) || defined(__NetBSD__) || defined(__OpenBSD__)
#define HAS_STAT_FLAGS
#endif

struct _m3_stat_t;
typedef struct _m3_stat_t m3_stat_t;

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
*/
#if defined(__INTERIX) || (defined(__vms) && defined(_DECC_V4_SOURCE)) || defined(_WIN32)
typedef int socklen_t;
#elif defined(__vms)
typedef size_t socklen_t;
#endif
typedef WORD_T m3_socklen_t;

typedef struct {
/* verified to exactly match struct linger in UnixC.c, except for Cygwin */
    int onoff;
    int linger;
} m3_linger_t;

int __cdecl Usocket__listen(int s, int backlog);
int __cdecl Usocket__shutdown(int s, int how);
int __cdecl Usocket__socket(int af, int type, int protocol);
int __cdecl Usocket__bind(int s, const M3SockAddrUnionAll*, m3_socklen_t);
int __cdecl Usocket__connect(int s, const M3SockAddrUnionAll*, m3_socklen_t);
INTEGER __cdecl Usocket__sendto(int s, void* msg, WORD_T length, int flags, const M3SockAddrUnionAll*, m3_socklen_t);
int __cdecl Usocket__setsockopt(int s, int level, int optname, void* optval, m3_socklen_t len);
int __cdecl Usocket__getpeername(int s, M3SockAddrUnionAll*, m3_socklen_t*);
int __cdecl Usocket__getsockname(int s, M3SockAddrUnionAll*, m3_socklen_t*);
int __cdecl Usocket__accept(int s, M3SockAddrUnionAll*, m3_socklen_t*);
int __cdecl Usocket__getsockopt(int s, int level, int optname, void* optval, m3_socklen_t*);
INTEGER __cdecl Usocket__recvfrom(int s, void* buf, WORD_T len, int flags, M3SockAddrUnionAll*, m3_socklen_t*);

#ifndef _WIN32
DIR* __cdecl Udir__opendir(const char* a);
#endif

// char* instead of ADDRESS for default Solaris
typedef char* caddr_t;
int __cdecl Umman__mprotect(caddr_t addr, WORD_T len, int prot);
ADDRESS __cdecl Umman__mmap(caddr_t addr, WORD_T len, int prot, int flags, int fd, m3_off_t off);
int __cdecl Umman__munmap(caddr_t addr, WORD_T len);

typedef INT64 m3_time_t;

#ifndef _WIN32
m3_time_t __cdecl Utime__time(m3_time_t* tloc);
char* __cdecl Utime__ctime(const m3_time_t* m);
#endif
void __cdecl Utime__tzset(void);

#if 1 /* Some compilers don't like this, will adjust as needed. */
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
int __cdecl Unix__fcntl(int fd, INTEGER request, void* arg);
int __cdecl Unix__ioctl(int fd, INTEGER request, void* argp);
int __cdecl Unix__mknod(const char* path, m3_mode_t mode, m3_dev_t dev);
m3_mode_t __cdecl Unix__umask(m3_mode_t newmask);

struct _m3_hostent_t;
typedef struct _m3_hostent_t m3_hostent_t;

m3_hostent_t* __cdecl Unetdb__gethostbyname(const char* name, m3_hostent_t* m3);
m3_hostent_t* __cdecl Unetdb__gethostbyaddr(const char* addr, int len, int type, m3_hostent_t* m3);


struct _m3_group_t;
typedef struct _m3_group_t m3_group_t;

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

UINT32 __cdecl Uin__ntohl(UINT32 x);
UINT16 __cdecl Uin__ntohs(UINT16 x);
UINT32 __cdecl Uin__htonl(UINT32 x);
UINT16 __cdecl Uin__htons(UINT16 x);

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
 * (They are all the same size.)
 */
typedef struct {
    INTEGER day;
    INTEGER hour;
    INTEGER minute;
    INTEGER month;
    INTEGER offset;
    INTEGER second;
    INTEGER weekDay;
    INTEGER year;
    TEXT    zone;
    INTEGER zzalign;
} Date_t;

void
__cdecl
DatePosix__FromTime(double t, const INTEGER* zone, Date_t* date, TEXT unknown, TEXT gmt);

double
__cdecl
DatePosix__ToTime(const Date_t* date);

void
__cdecl
DatePosix__TypeCheck(const Date_t* d, WORD_T sizeof_DateT);

void
__cdecl
Scheduler__DisableScheduling(void);

void
__cdecl
Scheduler__EnableScheduling(void);

M3_DLL_LOCAL
int
__cdecl
ThreadInternal__StackGrowsDown (void);

void
__cdecl
Process__RegisterExitor(void (__cdecl*)(void));

// GET_PC returns approximately size_t.

#if defined(__APPLE__)

#if defined(__i386__)

#if __DARWIN_UNIX03

#define GET_PC(context) ((context)->uc_mcontext->__ss.__eip)

#else

#define GET_PC(context) ((context)->uc_mcontext->ss.eip)

#endif

#elif defined(__x86_64__)
#if __DARWIN_UNIX03
#define GET_PC(context) ((context)->uc_mcontext->__ss.__rip)
#else
#define GET_PC(context) ((context)->uc_mcontext->ss.rip)
#endif

#elif defined(__ppc__) || defined(__ppc64__)
#if __DARWIN_UNIX03
#define GET_PC(context) ((context)->uc_mcontext->__ss.__srr0)
#else
#define GET_PC(context) ((context)->uc_mcontext->ss.srr0)
#endif
#elif defined(__arm__) || defined(__arm64__)
#if __DARWIN_UNIX03
#define GET_PC(context) (__darwin_arm_thread_state64_get_pc(context->uc_mcontext->__ss))
#else
#define GET_PC(context) (__darwin_arm_thread_state64_get_pc(context->uc_mcontext->ss))
#endif
#else
#error Unknown __APPLE__ target
#endif

#elif defined(__osf__)

#define GET_PC(context) ((context)->uc_mcontext.sc_pc)

#elif defined(__OpenBSD__)

#if defined(__amd64)

#define GET_PC(context) ((context)->sc_rip)

#elif defined(__powerpc)
#define GET_PC(context) ((context)->sc_frame.srr0)
#else
#define GET_PC(context) ((context)->sc_pc)
#endif

#elif defined(__linux) && defined(__sparc) && __WORDSIZE == 64
#define GET_PC(context) ((context)->uc_mcontext.mc_gregs[REG_PC])

#elif defined(__sun) || defined(__sparc)

#if defined(REG_PC)
#define GET_PC(context) ((context)->uc_mcontext.gregs[REG_PC])
#elif defined(__sun) && defined(__i386) && (PC == 14)
#define GET_PC(context) ((context)->uc_mcontext.gregs[PC])
#elif defined(__sun) && defined(__sparc) && (PC == 1)
#define GET_PC(context) ((context)->uc_mcontext.gregs[PC])
#else
#error unknown __sun/__sparc target
#endif

#elif defined(__linux)

/* see /src/glibc-2.14/sysdeps/unix/sysv/linux/x/sigcontextinfo.h */
#if defined(__i386)
#define GET_PC(context) ((context)->uc_mcontext.gregs[REG_EIP])
#elif defined(__amd64)
#define GET_PC(context) ((context)->uc_mcontext.gregs[REG_RIP])
#elif defined(__powerpc)
#define GET_PC(context) ((context)->uc_mcontext.uc_regs->gregs[PT_NIP])
#elif defined(__arm__)
#define GET_PC(context) ((context)->uc_mcontext.arm_pc)
#elif defined(__alpha__)
#define GET_PC(context) ((context)->uc_mcontext.sc_pc)
#elif defined(__ia64__)
#define GET_PC(context) ((context)->uc_mcontext.sc_ip)
#elif defined(__sh__)
#error untested __linux target
#define GET_PC(context) ((context)->uc_mcontext.sc_pc)
#elif defined(__s390__)
#error untested __linux target
#define GET_PC(context) ((context)->uc_mcontext.sregs.regs.psw.addr
#elif defined(__riscv) || defined(__riscv64)
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

#ifdef __cplusplus
} /* extern "C" */
#endif

#endif
