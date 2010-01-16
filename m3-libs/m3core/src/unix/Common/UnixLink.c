/* Copyright (C) 1993, Digital Equipment Corporation                  */
/* All rights reserved.                                               */
/* See the file COPYRIGHT for a full description.                     */

#ifdef _MSC_VER
#pragma optimize("gty", on)
#undef _DLL
#ifndef _MT
#define _MT
#endif
#endif

#include "m3core.h"
#ifdef _WIN32
#include <windows.h>
#endif

#ifdef __cplusplus
extern "C"
{
#endif

/* This file is #included by m3-sys/cm3 for bootstrapping on systems
whose older m3core does not have Unix.link. m3-sys/cm3 #defines
Unix__link to be Utils__link. */

int Unix__link(const char* ExistingFile, const char* NewLink)
{
#ifdef _WIN32
#ifdef _WIN64
    if (CreateHardLinkA(NewLink, ExistingFile, NULL) == FALSE)
        goto Error;
#else
    typedef BOOL (__stdcall * PFNCreateHardLinkA)(PCSTR NewLink, PCSTR ExistingFile, void* reserved);
    static PFNCreateHardLinkA pfnCreateHardLinkA;
    
    if (pfnCreateHardLinkA == NULL)
    {
        const static WCHAR Kernel32Name[] = L"Kernel32.dll";
        HMODULE Kernel32Handle = LoadLibraryW(Kernel32Name);
        if (Kernel32Handle == NULL)
            goto Error;
        pfnCreateHardLinkA = (PFNCreateHardLinkA)GetProcAddress(Kernel32Handle, "CreateHardLinkA");
        if (pfnCreateHardLinkA == NULL)
            goto Error;
    }
    if (pfnCreateHardLinkA(NewLink, ExistingFile, NULL) == FALSE)
        goto Error;
#endif
    return 0;
Error:
    errno = GetLastError();
    return -1;
#else
    return link(ExistingFile, NewLink);
#endif
}

#ifdef __cplusplus
} /* extern C */
#endif
