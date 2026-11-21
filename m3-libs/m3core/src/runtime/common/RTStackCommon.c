/* Copyright (C) 1990, Digital Equipment Corporation           */
/* All rights reserved.                                        */
/* See the file COPYRIGHT for a full description.              */

#include "m3core.h"

#ifdef __cplusplus
extern "C" {
#endif

INTEGER __cdecl RTStack_Has_walker (void)
{
#if AMD64_DARWIN || ARM64_DARWIN || AMD64_LINUX
    return TRUE;
#else
    return FALSE;
#endif
}

#ifdef __cplusplus
} /* extern "C" */
#endif
