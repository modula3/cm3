/* Copyright (C) 1992, Digital Equipment Corporation                         */
/* All rights reserved.                                                      */
/* See the file COPYRIGHT for a full description.                            */

#include "m3unix.h"

#ifdef __cplusplus
extern "C" {
#endif

int Umman__mprotect(ADDRESS addr, size_t length, int prot)
{
    return mprotect(addr, length, prot);
}

ADDRESS Umman__mmap(ADDRESS addr, size_t length, int prot, int flags, int fd, m3_off_t off)
{
    return mmap(addr, length, prot, flags, fd, off);
}

int Umman__munmap(ADDRESS addr, size_t length)
{
    return munmap(addr, length);
}

#ifdef __cplusplus
} /* extern "C" */
#endif
