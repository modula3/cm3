/* Copyright (C) 1990, Digital Equipment Corporation.         */
/* All rights reserved.                                       */
/* See the file COPYRIGHT for a full description.             */

#include "m3unix.h"

#ifndef _WIN32

#define M3MODULE Uutmp
M3WRAP0(char*, getlogin)

#endif
