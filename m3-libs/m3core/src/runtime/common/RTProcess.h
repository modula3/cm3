/* Copyright (C) 1992, Digital Equipment Corporation          */
/* All rights reserved.                                       */
/* See the file COPYRIGHT for a full description.             */

#if _MSC_VER > 1000
#pragma once
#endif

#ifndef RTPROCESS_INCLUDED
#define RTPROCESS_INCLUDED

#ifdef __cplusplus
extern "C" {
#endif

typedef void (*RTProcess__InterruptHandler)(void);
void RTProcess__InvokeExitors(void);
RTProcess__InterruptHandler RTProcess__OnInterrupt(RTProcess__InterruptHandler);

#ifdef __cplusplus
} /* extern "C" */
#endif

#endif
