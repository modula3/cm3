/* Copyright (C) 1992, Digital Equipment Corporation          */
/* All rights reserved.                                       */
/* See the file COPYRIGHT for a full description.             */

/* derived from LINUXLIBC6 */

#if _MSC_VER > 1000
#pragma once
#endif

#ifndef RTSIGNALC_INCLUDED
#define RTSIGNALC_INCLUDED

#include "m3text.h"

#ifdef __cplusplus
extern "C" {
#endif

typedef struct _RTSignalC_Texts_t
{
    TEXT aborted;
    TEXT segv;
} RTSignalC_Texts_t;

void RTSignalC_InstallHandlers(RTSignalC_Texts_t* Texts);
void RTSignalC_RestoreHandlers(void);

#ifdef __cplusplus
} /* extern "C" */
#endif

#endif
