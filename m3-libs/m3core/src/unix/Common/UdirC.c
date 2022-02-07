/* Copyright (C) 1990, Digital Equipment Corporation.         */
/* All rights reserved.                                       */
/* See the file COPYRIGHT for a full description.             */

#ifndef INCLUDED_M3CORE_H
#include "m3core.h"
#endif

#ifndef _WIN32

#undef M3MODULE /* Support concatenating multiple .c files. */
#define M3MODULE Udir

M3_NO_INLINE // because alloca
dirent* Udir__readdir (DIR* d)
{
    dirent* result;

    Scheduler__DisableSwitching ();
    result = readdir (d);
#ifndef _WIN32
    if (result && m3core_trace.readdir)
    {
        char* buf = (char*)alloca (256 + strlen (result->d_name));
        int len = sprintf (buf, "readdir:%s\n", result->d_name);
        write (1, buf, len);
    }
#endif
    Scheduler__EnableSwitching ();
    return result;
}

M3WRAP1(DIR*, opendir, const char*)
M3WRAP1(int, closedir, DIR*)

#endif
