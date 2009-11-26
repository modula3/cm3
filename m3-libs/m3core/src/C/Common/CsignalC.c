/* Copyright (C) 1992, Digital Equipment Corporation                         */
/* All rights reserved.                                                      */
/* See the file COPYRIGHT for a full description.                            */

#include <signal.h>

#ifdef _MSC_VER
#pragma optimize("gty", on)
#endif

#ifdef __cplusplus
extern "C" {
#endif

#if !defined(_MSC_VER) && !defined(__cdecl)
#define __cdecl /* nothing */
#endif

typedef void (__cdecl*Handler)(int s);

#define X(ret, name, in, out) ret __cdecl Csignal__##name in { return name out; }

X(Handler, signal, (int sig, Handler func), (sig, func))

#ifdef __cplusplus
}
#endif
