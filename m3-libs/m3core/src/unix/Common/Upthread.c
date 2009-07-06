/* Copyright (C) 1990, Digital Equipment Corporation           */
/* All rights reserved.                                        */
/* See the file COPYRIGHT for a full description.              */

#include "m3unix.h"

#ifdef __cplusplus
extern "C" {
#endif

#ifndef _WIN32

int Upthread__detach(pthread_t thread)
{
    return pthread_detach(thread);
}

pthread_t Upthread__self(void)
{
    return pthread_self();
}

int Upthread__equal(pthread_t t1, pthread_t t2)
{
    return pthread_equal(t1, t2);
}

int Upthread__kill(pthread_t thread, int sig)
{
    return pthread_kill(thread, sig);
}

#endif

#ifdef __cplusplus
}
#endif
