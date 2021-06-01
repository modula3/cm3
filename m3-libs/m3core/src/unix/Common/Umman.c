/* Copyright (C) 1992, Digital Equipment Corporation                         */
/* All rights reserved.                                                      */
/* See the file COPYRIGHT for a full description.                            */

#ifndef INCLUDED_M3CORE_H
#include "m3core.h"
#endif
#undef M3MODULE /* Support concatenating multiple .c files. */
#define M3MODULE Umman

#ifndef _WIN32

// char* (caddr_t) instead of void* for default Solaris
M3WRAP3(int, mprotect, caddr_t, size_t, int)
M3WRAP6(void*, mmap, caddr_t, size_t, int, int, int, m3_off_t)
M3WRAP2(int, munmap, caddr_t, size_t)

#endif
