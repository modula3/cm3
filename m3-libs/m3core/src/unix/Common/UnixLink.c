/* Copyright (C) 1993, Digital Equipment Corporation                  */
/* All rights reserved.                                               */
/* See the file COPYRIGHT for a full description.                     */

#ifndef INCLUDED_M3CORE_H
#include "m3core.h"
#endif
#ifdef _WIN32
#include <windows.h>
#endif

#if M3_HAS_VISIBILITY
#ifdef __APPLE__
#pragma GCC visibility push(default)
#else
#pragma GCC visibility push(protected)
#endif
#endif

#ifdef __cplusplus
extern "C"
{
#endif

M3_DLL_EXPORT int __cdecl
Unix__link(const char* ExistingFile, const char* NewLink)
{
#ifdef _WIN32
    if (CreateHardLinkA(NewLink, ExistingFile, NULL) == FALSE)
        goto Error;
    return 0;
Error:
    errno = (int)GetLastError();
    return -1;
#else
    return link(ExistingFile, NewLink);
#endif
}

#ifdef __cplusplus
} /* extern C */
#endif

#if M3_HAS_VISIBILITY
#pragma GCC visibility pop
#endif
