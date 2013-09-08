/* Copyright (C) 1993, Digital Equipment Corporation           */
/* All rights reserved.                                        */
/* See the file COPYRIGHT for a full description.              */
/*                                                             */
/* Last modified on Wed Aug 31 09:42:51 PDT 1994 by kalsow     */
/*      modified on Wed Sep 22 14:53:33 PDT 1993 by steveg     */
/*      modified on Thu Mar 11 13:01:04 PST 1993 by mjordan    */

#include <windows.h>

/* Modula-3 Time.T is seconds since a platform specific epoch in floating point.
 */

#ifdef __cplusplus
extern "C" {
#endif

void
__cdecl
TimeWin32__ToFileTime(double t, FILETIME * ft)
{
    LARGE_INTEGER li;

    li.QuadPart = (__int64)(t * 1.0e7);
    ft->dwLowDateTime = li.LowPart;
    ft->dwHighDateTime = li.HighPart;
}

double
__cdecl
TimeWin32__FromFileTime(const FILETIME * ft)
{
    LARGE_INTEGER li;

    li.LowPart = ft->dwLowDateTime;
    li.HighPart = ft->dwHighDateTime;
    return ((double)li.QuadPart) / 1.0e7;
}

#ifdef __cplusplus
} /* extern C */
#endif
