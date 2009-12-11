/* Copyright (C) 1990, Digital Equipment Corporation           */
/* All rights reserved.                                        */
/* See the file COPYRIGHT for a full description.              */

#include <string.h>

#ifdef __cplusplus
extern "C" {
#endif

#if !defined(_MSC_VER) && !defined(__cdecl)
#define __cdecl /* nothing */
#endif

#ifdef _MSC_VER
#pragma optimize("gty", on)
#endif

/*------------------------------- byte copying ------------------------------*/

void __cdecl RTMisc__Copy(const void* src, void* dest, size_t len)
{
    memmove(dest, src, len);
}

void __cdecl RTMisc__Zero(void* dest, size_t len)
{
    memset(dest, 0, len);
}

/*------------------------------- rounded arithmetic ------------------------*/

/* Align is equated to Upper via <*EXTERNAL*>; this is here for compatiblity. */

size_t RTMisc__Upper(size_t a, size_t y);

void* __cdecl RTMisc__Align(void* a, size_t y)
{
    return (void*)RTMisc__Upper((size_t)a, y);
}

#ifdef __cplusplus
} /* extern "C" */
#endif
