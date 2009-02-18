/* Copyright (C) 1994, Digital Equipment Corporation         */
/* All rights reserved.                                      */
/* See the file COPYRIGHT for a full description.            */

#include <windows.h>

#ifdef __cplusplus
extern "C" {
#endif


/* The Modula-3 gcc backend is not naming WindowFromPoint correctly.
   It is calling it WindowFromPoint@4 instead of WindowFromPoint@8
*/
HWND __stdcall WinUser__WindowFromPointWorkaround (POINT* Point)
{
	return WindowFromPoint(*Point);
}


#ifdef __cplusplus
} /* extern C */
#endif
