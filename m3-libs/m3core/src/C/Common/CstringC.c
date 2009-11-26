/* Copyright (C) 1989, Digital Equipment Corporation           */
/* All rights reserved.                                        */
/* See the file COPYRIGHT for a full description.              */

#define _CRT_SECURE_NO_DEPRECATE

#ifdef __cplusplus
extern "C" {
#endif

#include <string.h>

#if !defined(_MSC_VER) && !defined(__cdecl)
#define __cdecl /* nothing */
#endif

#ifdef _MSC_VER
#pragma optimize("gty", on)
#endif

#define X(ret, name, in, out) ret __cdecl Cstring__##name in { return name out; }

X(void*, memchr, (const void* s, int c, size_t n), (s, c, n))
X(void*, memcpy, (void* s1, const void* s2, size_t n), (s1, s2, n))
X(void*, memset, (void* s, int c, size_t n), (s, c, n))
X(int, memcmp, (const void* s1, const void* s2, size_t n), (s1, s2, n))
X(char*, strcpy, (char* s1, const char* s2), (s1, s2))
X(char*, strncpy, (char* s1, const char* s2, size_t n), (s1, s2, n))
X(char*, strcat, (char* s1, const char* s2), (s1, s2))
X(char*, strncat, (char* s1, const char* s2, size_t n), (s1, s2, n))
X(char*, strchr, (const char* s, int c), (s, c))
X(char*, strrchr, (const char* s, int c), (s, c))
X(char*, strpbrk, (const char* s1, const char* s2), (s1, s2))
X(char*, strtok, (char* s1, const char* s2), (s1, s2))
X(int, strcmp, (const char* s1, const char* s2), (s1, s2))
X(int, strncmp, (const char* s1, const char* s2, size_t n), (s1, s2, n))
X(size_t, strlen, (const char* s), (s))
X(size_t, strspn, (const char* s1, const char* s2), (s1, s2))
X(size_t, strcspn, (const char* s1, const char* s2), (s1, s2))
X(void*, memmove, (void* s1, const void* s2, size_t n), (s1, s2, n))
X(int, strcoll, (const char* s1, const char* s2), (s1, s2))
X(size_t, strxfrm, (char* s1, const char* s2, size_t n), (s1, s2, n))
X(char*, strstr, (const char* s1, const char* s2), (s1, s2))
X(char*, strerror, (int errnum), (errnum))

#ifdef __cplusplus
} /* extern "C" */
#endif
