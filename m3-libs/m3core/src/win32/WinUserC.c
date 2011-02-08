/* Copyright (C) 1994, Digital Equipment Corporation         */
/* All rights reserved.                                      */
/* See the file COPYRIGHT for a full description.            */

#include <windows.h>

#ifdef __cplusplus
extern "C" {
#endif

HWND __stdcall WinUser__WindowFromPointWorkaround (POINT* Point)
/* The Modula-3 gcc backend is not naming WindowFromPoint correctly.
   It is calling it WindowFromPoint@4 instead of WindowFromPoint@8.
   We workaround that by passing by VAR instead of by value.
*/
{
	return WindowFromPoint(*Point);
}

#ifdef __cplusplus
} /* extern C */
#endif
