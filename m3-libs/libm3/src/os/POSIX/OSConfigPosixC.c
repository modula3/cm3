/* Copyright 1996-2000, Critical Mass, Inc.  All rights reserved. */
/* See file COPYRIGHT-CMASS for details. */

#ifndef INCLUDED_M3CORE_H
#include "m3core.h"
#endif
#include <sys/utsname.h>

#ifdef __cplusplus
extern "C"
{           
#endif

// Callback from C to Modula3 so that C does not traffic in traced references.
void
__cdecl
OSConfigPosixC__InitFromC(
    const char* host_name,
    const char* host_arch,
    const char* os_name,
    const char* os_version);

void
__cdecl
OSConfigPosixC__InitC(void)
{
    struct utsname uts;

    // Workaround: Some builds of WSL1 do not nul terminate.
    ZeroMemory(&uts, sizeof(uts));

    int result = uname(&uts);

    assert(result == 0);

    OSConfigPosixC__InitFromC(uts.nodename, uts.machine, uts.sysname, uts.release);
}

#ifdef __cplusplus
} /* extern "C" */
#endif
