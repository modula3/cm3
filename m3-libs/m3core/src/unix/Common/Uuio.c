/* Copyright (C) 1990, Digital Equipment Corporation.         */
/* All rights reserved.                                       */
/* See the file COPYRIGHT for a full description.             */

#ifndef INCLUDED_M3CORE_H
#include "m3core.h"
#endif

#undef M3MODULE /* Support concatenating multiple .c files. */
#define M3MODULE Uuio

M3_NO_INLINE // because alloca
ssize_t
__cdecl
Uuio__write (int fd, const void* buf, size_t n)
{
    ssize_t result;

    Scheduler__DisableSwitching ();
#ifdef _WIN32
    result = _write (fd, buf, n);
#else
    result = write (fd, buf, n);
    if (m3core_trace.s.write)
    {
        char* buf = (char*)alloca (256);
	int maxlen = sizeof buf;
        int len = snprintf (buf, maxlen, "write (fd:%d, buf:%p, n:%ld):%ld\n", fd, buf, (long)n, (long)result);
        write (1, buf, len);
    }
#endif
    Scheduler__EnableSwitching ();
    return result;
}

M3WRAP3_(ssize_t, read, int, void*, size_t)
