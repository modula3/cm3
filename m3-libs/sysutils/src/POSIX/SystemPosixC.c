/* Copyright (C) 1990, Digital Equipment Corporation           */
/* All rights reserved.                                        */
/* See the file COPYRIGHT for a full description.              */

#define _FILE_OFFSET_BITS 64

#ifdef __INTERIX
#ifndef _ALL_SOURCE
#define _ALL_SOURCE
#endif
#endif
#ifndef _REENTRANT
#define _REENTRANT
#endif

#include <sys/wait.h>
#include <stddef.h>
#include <unistd.h>
#include <errno.h>

/* copied from m3core/src/unix/Common to fix bootstrapping error:
/cm3/pkg/sysutils/LINUXLIBC6/libsysutils.so: undefined reference to `Uerror__EINVAL'
/cm3/pkg/sysutils/LINUXLIBC6/libsysutils.so: undefined reference to `Unix__X_OK'
/cm3/pkg/sysutils/LINUXLIBC6/libsysutils.so: undefined reference to `Unix__R_OK'
/cm3/pkg/sysutils/LINUXLIBC6/libsysutils.so: undefined reference to `Unix__W_OK'
/cm3/pkg/sysutils/LINUXLIBC6/libsysutils.so: undefined reference to `Uerror__ECHILD'
/cm3/pkg/sysutils/LINUXLIBC6/libsysutils.so: undefined reference to `Uerror__EINTR'
/cm3/pkg/sysutils/LINUXLIBC6/libsysutils.so: undefined reference to `Unix__access'
/cm3/pkg/sysutils/LINUXLIBC6/libsysutils.so: undefined reference to `Unix__gethostname'
*/

/* http://gcc.gnu.org/wiki/Visibility */
/* Generic helper definitions for shared library support */
#if __GNUC__ >= 4
#ifdef __APPLE__
#pragma GCC visibility push(default)
#else
#pragma GCC visibility push(protected)
#endif
#endif

/* const is extern const in C, but static const in C++,
 * but gcc gives a warning for the correct portable form "extern const"
 */
#if defined(__cplusplus) || !defined(__GNUC__)
#define EXTERN_CONST extern const
#else
#define EXTERN_CONST const
#endif

#ifdef __cplusplus
extern "C"
{
#endif

#define M3WRAPNAMEx(a, b)           a##__##b
#define M3WRAPNAME(a, b)            M3WRAPNAMEx(a, b)
#define M3WRAP(ret, name, in, out)  ret M3WRAPNAME(M3MODULE, name) in { return name out; }
#define M3WRAP2(ret, name, a, b)    M3WRAP(ret, name, (a i, b j), (i, j))

#define M3MODULE System

M3WRAP2(int, gethostname, char*, size_t)

typedef ptrdiff_t m3_pid_t;

m3_pid_t System__waitpid(m3_pid_t pid, int* status, int options)
{
    return waitpid(pid, status, options);
}

#define X(x) EXTERN_CONST int System__##x = x;

X(EINVAL)
X(ECHILD)
X(EINTR)

#ifdef __cplusplus
}
#endif
