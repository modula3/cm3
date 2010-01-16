/* Copyright (C) 1992, Digital Equipment Corporation                         */
/* All rights reserved.                                                      */
/* See the file COPYRIGHT for a full description.                            */

#ifdef _MSC_VER
#undef _DLL
#endif

#include "m3core.h"

#define M3MODULE Csignal

typedef void (__cdecl*SignalHandler)(int s);

M3WRAP2(SignalHandler, signal, int, SignalHandler)
