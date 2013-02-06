/* copied from m3core so can build from older releases,
 * in which there is no m3core/m3core.h
 */

#if _MSC_VER > 1000
#pragma once
#endif

#ifndef INCLUDED_M3CORE_H
#define INCLUDED_M3CORE_H

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

#include <assert.h>
#include <stddef.h>
#include <limits.h>

#if !defined(_MSC_VER) && !defined(__cdecl)
#define __cdecl /* nothing */
#endif

#ifdef _WIN32
#else
#define ZeroMemory(a, b) (memset((a), 0, (b)))
#endif /* Win32 vs. Posix */

#if UINT_MAX == 0x0FFFFFFFFUL
typedef   signed int        INT32;
typedef unsigned int       UINT32;
#elif ULONG_MAX == 0x0FFFFFFFFUL
typedef   signed long       INT32;
typedef unsigned long      UINT32;
#else
#error unable to find 32bit integer
#endif
#if defined(_MSC_VER) || defined(__DECC)
typedef   signed __int64    INT64;
typedef unsigned __int64   UINT64;
#else
typedef   signed long long  INT64;
typedef unsigned long long UINT64;
#endif

/* WORD_T/INTEGER are always exactly the same size as a pointer.
 * VMS sometimes has 32bit size_t/ptrdiff_t but 64bit pointers.
 */
#if __INITIAL_POINTER_SIZE == 64
typedef __int64 INTEGER;
typedef unsigned __int64 WORD_T;
#else
typedef ptrdiff_t INTEGER;
typedef size_t WORD_T;
#endif

typedef void* TEXT;

#ifdef __cplusplus
extern "C" {
#endif

TEXT
__cdecl
M3toC__CopyStoT(const char*);

TEXT
__cdecl
M3toC__StoT(const char*);

#ifdef __cplusplus
} /* extern "C" */
#endif

#endif
