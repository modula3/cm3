/* Copyright (C) 1989, Digital Equipment Corporation           */
/* All rights reserved.                                        */
/* See the file COPYRIGHT for a full description.              */

#include "m3core.h"
#include <unistd.h>

#ifdef __cplusplus
extern "C" {
#endif

int
__cdecl
ThreadF__Select(int nfds,
                ADDRESS read,
                ADDRESS write,
                ADDRESS except,
                LONGREAL timeout)
{
    MicrosecondsStruct_t utime;
    ZeroMemory(&utime, sizeof(utime));
    return select(nfds, read, write, except,
                  (timeout >= 0)
                  ? TimePosix__FloatSecondsToMicrosecondsStruct(timeout, &utime)
                  : NULL);
}

#ifdef __cplusplus
} /* extern "C" */
#endif
