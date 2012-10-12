/* Copyright (C) 1993, Digital Equipment Corporation                  */
/* All rights reserved.                                               */
/* See the file COPYRIGHT for a full description.                     */

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

#define _FILE_OFFSET_BITS 64

#ifdef __INTERIX
#ifndef _ALL_SOURCE
#define _ALL_SOURCE
#endif
#endif
#ifndef _REENTRANT
#define _REENTRANT
#endif

#ifdef _WIN32
#include <io.h>
#else
#include <unistd.h>
#endif

/* const is extern const in C, but static const in C++,
 * but gcc gives a warning for the correct portable form "extern const"
 */
#if defined(__cplusplus) || !defined(__GNUC__)
#define EXTERN_CONST extern const
#else
#define EXTERN_CONST const
#endif

#if __GNUC__ >= 4
#ifdef __APPLE__
#pragma GCC visibility push(default)
#else
#pragma GCC visibility push(protected)
#endif
#endif

#ifdef __cplusplus
extern "C"
{
#endif

/* There are no #defines in the headers but the documention gives the values. */
#ifdef _WIN32
#define X_OK 0
#define W_OK 2
#define R_OK 4
#undef access
#endif 


#define M3WRAPNAMEx(a, b)           a##__##b
#define M3WRAPNAME(a, b)            M3WRAPNAMEx(a, b)
#ifdef _WIN32
#define M3WRAP_(ret, name, in, out)  ret M3WRAPNAME(M3MODULE, name) in { return _##name out; }
#else
#define M3WRAP_(ret, name, in, out)  ret M3WRAPNAME(M3MODULE, name) in { return name out; }
#endif
#define M3WRAP2_(ret, name, a, b)    M3WRAP_(ret, name, (a i, b j), (i, j))

#define M3MODULE FSUtils

M3WRAP2_(int, access, const char*, int)

#define X(x) EXTERN_CONST int FSUtils__##x = x;

X(X_OK)
X(R_OK)
X(W_OK)

#ifdef __cplusplus
} /* extern C */
#endif
