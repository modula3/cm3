/* Copyright (C) 1989, Digital Equipment Corporation           */
/* All rights reserved.                                        */
/* See the file COPYRIGHT for a full description.              */

#ifndef INCLUDED_M3CORE_H
#include "m3core.h"
#endif

#ifdef __cplusplus
extern "C" {
#endif

#define M3MODULE Cstdlib

typedef void (__cdecl*AtExitFunction)(void);

M3WRAP1(int, atexit, AtExitFunction)
M3WRAP1(char*, getenv, const char*)
M3WRAP1(int, system, const char*)
M3WRAP2(double, strtod, const char*, char**)
M3WRAP1(double, atof, const char*)

M3WRAP_RETURN_VOID(Cstdlib__abort, abort, (void), ())
M3WRAP_RETURN_VOID(Cstdlib__exit, exit, (int status), (status))

M3WRAP_NO_SWITCHING(void*, Cstdlib__malloc, malloc, (WORD_T a), (a))
M3WRAP_NO_SWITCHING(void*, Cstdlib__calloc, calloc, (WORD_T a, WORD_T b), (a, b))
M3WRAP_RETURN_VOID_NO_SWITCHING(Cstdlib__free, free, (void* a), (a))

#ifdef __cplusplus
}
#endif
