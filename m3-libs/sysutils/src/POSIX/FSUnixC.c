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
#ifndef _REENTRANT
#define _REENTRANT
#endif
#endif

#include <unistd.h>

#ifdef __cplusplus
extern "C"
{
#endif

#define M3WRAPNAMEx(a, b)           a##__##b
#define M3WRAPNAME(a, b)            M3WRAPNAMEx(a, b)
#define M3WRAP(ret, name, in, out)  ret M3WRAPNAME(M3MODULE, name) in { return name out; }
#define M3WRAP2(ret, name, a, b)    M3WRAP(ret, name, (a i, b j), (i, j))

#define M3MODULE FSUtils

M3WRAP2(int, access, const char*, int)

#define X(x) const int FSUtils__##x = x;

X(X_OK)
X(R_OK)
X(W_OK)

#ifdef __cplusplus
} /* extern C */
#endif
