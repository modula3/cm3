/* Copyright (C) 1994, Digital Equipment Corporation         */
/* All rights reserved.                                      */
/* See the file COPYRIGHT for a full description.            */

#include "m3middle.h"

#ifdef __cplusplus
extern "C" {
#endif

#ifndef _WIN32
EXTERN_CONST double CoffTime__EpochAdjust = 0.0e0; /* to 1/1/1970 */
#else
EXTERN_CONST double CoffTime__EpochAdjust = 11644473600.0e0; /* seconds from 1/1/1600 -> 1/1/1970 */
#define TIME_UNIT ((UINT64)10000000)
#define COFF_EPOCH_ADJUST (((UINT64)116444736) * ((UINT64)100) * TIME_UNIT)

static
INTEGER /* should be LONGINT */
CoffTime__FromFileTime(FILETIME* ft)
{
    UINT64 a = ((((UINT64)ft->dwHighDateTime) << 32) | ((UINT64)ft->dwLowDateTime));
    return (INTEGER)((a - COFF_EPOCH_ADJUST) / TIME_UNIT);
}

#endif

INTEGER /* should be LONGINT */
__cdecl
CoffTime__Now(void)
{
#ifdef _WIN32
    FILETIME ft = { 0 };
    GetSystemTimeAsFileTime(&ft);
    return CoffTime__FromFileTime(&ft);
#else
    struct timeval tv;
    tv.tv_sec = 0;
    gettimeofday(&tv, NULL);
    return tv.tv_sec;
#endif
}

INTEGER /* should be LONGINT */
__cdecl
CoffTime__OfFile(TEXT tpath)
{
    const char* path = (tpath ? M3toC__SharedTtoS(tpath) : NULL);
    INTEGER t = 0; /* ignore error */
    if (path)
    {
#ifdef _WIN32
        WIN32_FILE_ATTRIBUTE_DATA st;
        st.ftLastWriteTime.dwLowDateTime = 0;
        st.ftLastWriteTime.dwHighDateTime = 0;
        if (GetFileAttributesExA(path, GetFileExInfoStandard, &st))
            t = CoffTime__FromFileTime(&st.ftLastWriteTime);
#else
        struct stat st;
        st.st_mtime = 0;
        if (stat(path, &st) == 0)
            t = st.st_mtime;
#endif
        M3toC__FreeSharedS(tpath, path);
    }
    return t;
}

#ifdef __cplusplus
} /* extern "C" */
#endif
