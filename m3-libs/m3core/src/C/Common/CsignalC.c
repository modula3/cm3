/* Copyright (C) 1992, Digital Equipment Corporation                         */
/* All rights reserved.                                                      */
/* See the file COPYRIGHT for a full description.                            */

#ifndef INCLUDED_M3CORE_H
#include "m3core.h"
#endif

#undef M3MODULE /* Support concatenating multiple .c files. */
#define M3MODULE Csignal

typedef void (__cdecl*SignalHandler)(int s);

M3WRAP2(SignalHandler, signal, int, SignalHandler)
