/* Copyright (C) 1989, Digital Equipment Corporation           */
/* All rights reserved.                                        */
/* See the file COPYRIGHT for a full description.              */

#if _MSC_VER > 1000
#pragma once
#endif

#ifndef THREADPOSIXC_INCLUDED
#define THREADPOSIXC_INCLUDED

#ifdef __cplusplus
extern "C" {
#endif

typedef void (*ThreadPosixC__SignalHandler1)(int signo);
void ThreadPosixC__setup_sigvtalrm(ThreadPosixC__SignalHandler1 handler);
void ThreadPosixC__allow_sigvtalrm(void);
void ThreadPosixC__disallow_sigvtalrm(void);
void ThreadPosixC__Init(void);

#ifdef __cplusplus
} /* extern "C" */
#endif

#endif
