/* Copyright (C) 1990, Digital Equipment Corporation           */
/* All rights reserved.                                        */
/* See the file COPYRIGHT for a full description.              */

#include "m3core.h"

#ifdef __cplusplus
extern "C" {
#endif

INTEGER __cdecl RTStack_Has_walker (void)
{
    /* Systems with tested libunwind. TODO: Test and expand broadly.
     * And wrap Windows RtlVirtualUnwind.
     */
#if M3_HOST_AMD64_DARWIN || M3_HOST_ARM64_DARWIN || M3_HOST_AMD64_LINUX
    return TRUE;
#else
    return FALSE;
#endif
}

#ifdef __cplusplus
} /* extern "C" */
#endif
