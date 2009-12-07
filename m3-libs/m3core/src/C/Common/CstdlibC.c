/* Copyright (C) 1989, Digital Equipment Corporation           */
/* All rights reserved.                                        */
/* See the file COPYRIGHT for a full description.              */

#ifdef _MSC_VER
#pragma optimize("gty", on)
#endif
#define _CRT_SECURE_NO_DEPRECATE

#ifdef __cplusplus
extern "C" {
#endif

#include <stdlib.h>

#if !defined(_MSC_VER) && !defined(__cdecl)
#define __cdecl /* nothing */
#endif

#define X(ret, name, in, out) ret __cdecl Cstdlib__##name in { return name out; }
#define V(name, in, out) void __cdecl Cstdlib__##name in { name out; }

V(abort, (void), ())
X(int, atexit, (void (__cdecl*func)(void)), (func))
V(exit, (int status), (status))
X(char*, getenv, (const char* name), (name))
X(int, system, (const char* s), (s))
X(void*, malloc, (size_t n), (n))
X(void*, calloc, (size_t a, size_t b), (a, b))
V(free, (void* a), (a))
X(double, strtod, (const char* a, char** b), (a, b))
X(double, atof, (const char* a), (a))

#ifdef __cplusplus
}
#endif
