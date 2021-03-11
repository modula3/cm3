/* copied from m3core so can build from older releases,
 * in which there is no m3core/m3core.h
 */

#if _MSC_VER > 1000
#pragma once
#endif

#define _FILE_OFFSET_BITS 64

#ifndef _REENTRANT
#define _REENTRANT
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

#include <sys/stat.h>
#include <sys/time.h>
#include <string.h>
#include <stddef.h>

#define ZeroMemory(a, b) (memset((a), 0, (b)))
#define ZERO_MEMORY(a) (ZeroMemory(&(a), sizeof(a)))

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

#ifdef __cplusplus
} /* extern "C" */
#endif
