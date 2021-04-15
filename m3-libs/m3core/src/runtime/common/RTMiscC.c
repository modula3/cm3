/* Copyright (C) 1990, Digital Equipment Corporation           */
/* All rights reserved.                                        */
/* See the file COPYRIGHT for a full description.              */

#ifndef INCLUDED_M3CORE_H
#include "m3core.h"
#endif

#ifdef __cplusplus
extern "C" {
#endif

#if !defined(_MSC_VER) && !defined(__cdecl)
#define __cdecl /* nothing */
#endif

/*------------------------------- byte copying ------------------------------*/

void __cdecl RTMisc__Copy(const void* src, void* dest, WORD_T len)
{
    memmove(dest, src, len);
}

void __cdecl RTMisc__Zero(void* dest, WORD_T len)
{
    memset(dest, 0, len);
}

/*------------------------------- rounded arithmetic ------------------------*/

WORD_T __cdecl RTMisc__Upper(WORD_T a, WORD_T y);

void* __cdecl RTMisc__Align(void* a, WORD_T y)
{
    return (void*)RTMisc__Upper((WORD_T)a, y);
}

#ifdef __cplusplus
} /* extern "C" */
#endif
