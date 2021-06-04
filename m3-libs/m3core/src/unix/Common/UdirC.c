/* Copyright (C) 1990, Digital Equipment Corporation.         */
/* All rights reserved.                                       */
/* See the file COPYRIGHT for a full description.             */

#ifndef INCLUDED_M3CORE_H
#include "m3core.h"
#endif

#ifndef _WIN32

#undef M3MODULE /* Support concatenating multiple .c files. */
#define M3MODULE Udir
M3WRAP1(DIR*, opendir, const char*)
M3WRAP1(dirent*, readdir, DIR*)
M3WRAP1(int, closedir, DIR*)

#endif
