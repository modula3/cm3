/* Copyright (C) 1990, Digital Equipment Corporation.         */
/* All rights reserved.                                       */
/* See the file COPYRIGHT for a full description.             */

#include "m3unix.h"

#ifdef __cplusplus
extern "C" {
#endif

#ifndef _WIN32

ssize_t Uuio__read (int d, void* buf, size_t nbytes)
{
#ifdef _WIN32
    return _read(d, buf, nbytes);
#else
    return read(d, buf, nbytes);
#endif
}

ssize_t Uuio__write (int d, const void* buf, size_t nbytes)
{
#ifdef _WIN32
    return _write(d, buf, nbytes);
#else
    return write(d, buf, nbytes);
#endif
}

#endif

#ifdef __cplusplus
}
#endif
