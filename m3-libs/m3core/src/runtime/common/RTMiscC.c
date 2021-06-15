/* Copyright (C) 1990, Digital Equipment Corporation           */
/* All rights reserved.                                        */
/* See the file COPYRIGHT for a full description.              */

#ifndef INCLUDED_M3CORE_H
#include "m3core.h"
#endif

#ifdef __cplusplus
extern "C" {
#endif

/*------------------------------- byte copying ------------------------------*/

void __cdecl RTMisc__Copy(ADDRESS src, ADDRESS dest, INTEGER ilen)
{
    size_t const len = (size_t)ilen; // Modula-3 lacks unsigned types, pass as signed and cast.
    memmove(dest, src, len);
}

void __cdecl RTMisc__Zero(ADDRESS dest, INTEGER ilen)
{
    size_t const len = (size_t)ilen; // Modula-3 lacks unsigned types, pass as signed and cast.
    memset(dest, 0, len);
}

/*------------------------------- rounded arithmetic ------------------------*/

INTEGER __cdecl RTMisc__Upper(INTEGER a, INTEGER y);

ADDRESS __cdecl RTMisc__Align(ADDRESS a, INTEGER y)
{
    return (ADDRESS)RTMisc__Upper((INTEGER)a, y);
}

#ifdef __cplusplus
} /* extern "C" */
#endif
