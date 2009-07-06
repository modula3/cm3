/* Copyright (C) 1990, Digital Equipment Corporation           */
/* All rights reserved.                                        */
/* See the file COPYRIGHT for a full description.              */

#include "m3unix.h"

#ifdef __cplusplus
extern "C" {
#endif

#ifndef _WIN32

/*
m3_pthread_t is void*.
pthread_t might be any of: size_t, ptrdiff_t, int, void*, another pointer.
pthread_t will not be larger than a pointer/size_t. (see Unix__Assertions)
Only convert integers to/from integers, and pointer-sized integers to/from pointers.
*/
#define PTHREAD_TO_M3(x)   ((m3_pthread_t)(size_t)(x))
#define PTHREAD_FROM_M3(x) ((pthread_t)(size_t)(x))
    
int Upthread__detach(m3_pthread_t thread)
{
    pthread_t a = PTHREAD_FROM_M3(thread);
    return pthread_detach(a);
}

m3_pthread_t Upthread__self(void)
{
    return PTHREAD_TO_M3(pthread_self());
}

int Upthread__equal(m3_pthread_t t1, m3_pthread_t t2)
{
    return pthread_equal(PTHREAD_FROM_M3(t1), PTHREAD_FROM_M3(t2));
}

int Upthread__kill(m3_pthread_t thread, int sig)
{
    return pthread_kill(PTHREAD_FROM_M3(thread), sig);
}

#endif

#ifdef __cplusplus
}
#endif
