/* Copyright (C) 1993, Digital Equipment Corporation                  */
/* All rights reserved.                                               */
/* See the file COPYRIGHT for a full description.                     */

#ifndef INCLUDED_M3CORE_H
#include "m3core.h"
#endif
#ifdef _WIN32
#include <windows.h>
#endif

#ifdef __cplusplus
extern "C"
{
#endif

int __cdecl
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
