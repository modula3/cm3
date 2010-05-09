/* copied from m3core so can build from older releases,
 * in which there is no m3core/m3core.h
 */

#if _MSC_VER > 1000
#pragma once
#endif

#define _FILE_OFFSET_BITS 64

#ifdef _WIN32
#ifndef WIN32
#define WIN32
#endif
#endif

#ifndef _REENTRANT
#define _REENTRANT
#endif

/* const is extern const in C, but static const in C++,
 * but gcc gives a warning for the correct portable form "extern const" */
#if defined(__cplusplus) || !defined(__GNUC__)
#define EXTERN_CONST extern const
#else
#define EXTERN_CONST const
#endif

#ifdef __arm__
/* Reveal the correct struct stat? */
#ifndef _DARWIN_FEATURE_64_ONLY_BIT_INODE
#define _DARWIN_FEATURE_64_ONLY_BIT_INODE
#endif
#endif

#if !defined(_MSC_VER) && !defined(__cdecl)
#define __cdecl /* nothing */
#endif

#ifndef _WIN32
#include <sys/stat.h>
#include <sys/time.h>
#endif
#include <stddef.h>

#ifdef __cplusplus
extern "C" {
#endif

/* INTEGER is always signed and exactly the same size as a pointer */
#if __INITIAL_POINTER_SIZE == 64
/* VMS with 64 bit pointers but 32bit size_t/ptrdiff_t. */
typedef __int64 INTEGER;
#else
typedef ptrdiff_t INTEGER;
#endif

typedef void* TEXT;

const char*
__cdecl
M3toC__SharedTtoS(TEXT);

void
__cdecl
M3toC__FreeSharedS(TEXT, const char*);

#ifdef __cplusplus
} /* extern "C" */
#endif
