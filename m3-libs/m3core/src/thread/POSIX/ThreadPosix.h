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

typedef void (*ThreadPosix__SignalHandler1)(int signo);
void ThreadPosix__setup_sigvtalrm(ThreadPosix__SignalHandler1 handler);
void ThreadPosix__allow_sigvtalrm(void);
void ThreadPosix__disallow_sigvtalrm(void);

#ifdef __cplusplus
} /* extern "C" */
#endif

#endif
