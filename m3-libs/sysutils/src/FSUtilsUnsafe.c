#ifdef _MSC_VER
#pragma optimize("gty", on)
#undef _DLL
#ifndef _MT
#define _MT
#endif
#endif

#if !defined(_MSC_VER) && !defined(__cdecl)
#define __cdecl /* nothing */
#endif

#define _FILE_OFFSET_BITS 64

#ifdef _MSC_VER
typedef __int64 INT64;
#pragma warning(disable:4616) /* there is no warning x (unavoidable if targeting multiple compiler versions) */
#pragma warning(disable:4619) /* there is no warning x (unavoidable if targeting multiple compiler versions) */
#pragma warning(disable:4115) /* named type definition in parentheses */
#pragma warning(disable:4226) /* nonstandard extension: __export */
#pragma warning(disable:4514) /* unused inline function removed */
#pragma warning(disable:4201) /* nonstandard extension: nameless struct/union */
#pragma warning(disable:4214) /* nonstandard extension: bitfield other than int */
#pragma warning(disable:4209) /* nonstandard extension: benign re-typedef */
#pragma warning(disable:4820) /* padding inserted */
#pragma warning(disable:4668) /* #if of undefined symbol */
#pragma warning(disable:4255) /* () change to (void) */
#else
typedef long long INT64;
#endif

#include <stddef.h>
#include <limits.h>

typedef ptrdiff_t INTEGER;

#ifdef _WIN64
#define INTEGER_MAX INT64_MAX
#else
#define INTEGER_MAX LONG_MAX
#endif

#ifdef _WIN32
#include <windows.h>
#else
#include <sys/stat.h>
#endif

#ifdef __cplusplus
extern "C" {
#endif

INT64 __cdecl FSUtilsUnsafe__GetFileSize64(const char* path)
{
#ifdef _WIN32
    WIN32_FIND_DATAA data;
    HANDLE h = FindFirstFileA(path, &data);
    if (h == INVALID_HANDLE_VALUE)
        return 0;
    FindClose(h);
    return ((((INT64)data.nFileSizeHigh) << 32) | data.nFileSizeLow);
#else
    struct stat st;
    if (stat(path, &st))
        return 0;
    return st.st_size;
#endif
}

INTEGER __cdecl FSUtilsUnsafe__GetFileSize(const char* path)
{
    INT64 size = FSUtilsUnsafe__GetFileSize64(path);
    return ((size >= 0 && size <= INTEGER_MAX) ? (INTEGER)size : 0);
}

int __cdecl FSUtilsUnsafe__GetFileSize32(const char* path)
{
    INT64 size = FSUtilsUnsafe__GetFileSize64(path);
    return ((size >= 0 && size <= INT_MAX) ? (int)size : 0);
}

#ifdef __cplusplus
} /* extern "C" */
#endif
