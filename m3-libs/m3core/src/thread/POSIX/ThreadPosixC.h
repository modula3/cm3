/* Copyright (C) 1989, Digital Equipment Corporation           */
/* All rights reserved.                                        */
/* See the file COPYRIGHT for a full description.              */

#if _MSC_VER > 1000
#pragma once
#endif

#ifndef THREADPOSIXC_INCLUDED
#define THREADPOSIXC_INCLUDED

#include <signal.h>

#ifdef __cplusplus
extern "C" {
#endif

typedef void /*ThreadPosixC_SignalHandler1)(int signo);
typedef void /*ThreadPosixC_SignalHandler3)(int signo, siginfo_t*, void* /* ucontext_t */);

void ThreadPosixC_setup_sigvtalrm(ThreadPosixC_SignalHandler1 handler);

void ThreadPosixC_allow_sigvtalrm(void);

void ThreadPosixC_disallow_sigvtalrm(void);

void ThreadPosixC_Init(void);

#ifdef __cplusplus
} /* extern "C" */
#endif

#endif
