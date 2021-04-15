/* Copyright (C) 1992, Digital Equipment Corporation                         */
/* All rights reserved.                                                      */
/* See the file COPYRIGHT for a full description.                            */

#ifndef INCLUDED_M3CORE_H
#include "m3core.h"
#endif
#define M3MODULE Umman

#ifndef _WIN32

// char* instead of ADDRESS for default Solaris
M3WRAP3(int, mprotect, caddr_t, WORD_T, int)
M3WRAP6(ADDRESS, mmap, caddr_t, WORD_T, int, int, int, m3_off_t)
M3WRAP2(int, munmap, caddr_t, WORD_T)

#endif
