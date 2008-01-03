(* Copyright (C) 1994, Digital Equipment Corporation         *)
(* All rights reserved.                                      *)
(* See the file COPYRIGHT for a full description.            *)
(*                                                           *)
(* by Stephen Harrison                                       *)
(*                                                           *)
(* Last modified on Mon Jun 12 08:02:25 PDT 1995 by kalsow   *)
(*      modified on Tue Mar 23 17:31:53 PST 1993 by harrison *)

UNSAFE MODULE WinGDI;

IMPORT WinBase;
FROM Word IMPORT Shift, Or, Extract;
FROM WinDef IMPORT BYTE, COLORREF, WORD, HRGN, BOOL;
FROM Ctypes IMPORT int;

PROCEDURE RGB (r, g, b: BYTE): COLORREF =
  BEGIN
    RETURN Or(r, Or(Shift(g, 8), Shift(b, 16)));
  END RGB;

PROCEDURE PALETTERGB (r, g, b: BYTE): COLORREF =
  BEGIN
    RETURN Or(16_02000000, RGB(r, g, b));
  END PALETTERGB;

PROCEDURE PALETTEINDEX (i: WORD): COLORREF =
  BEGIN
    RETURN Or(16_01000000, i);
  END PALETTEINDEX;

PROCEDURE GetRValue (rgb: COLORREF): BYTE =
  BEGIN
    RETURN Extract(rgb, 0, 8);
  END GetRValue;

PROCEDURE GetGValue (rgb: COLORREF): BYTE =
  BEGIN
    RETURN Extract(rgb, 8, 8);
  END GetGValue;

PROCEDURE GetBValue (rgb: COLORREF): BYTE =
  BEGIN
    RETURN Extract(rgb, 16, 8);
  END GetBValue;

(* hack to patch the buggy return values on Chicago *)

PROCEDURE SetRectRgn (a1: HRGN; a2: int; a3: int; a4: int; a5: int): BOOL =
  VAR b := raw_SetRectRgn (a1, a2, a3, a4, a5);
  BEGIN
    IF is_chicago THEN b := ORD (b # 0); END;
    RETURN b;
  END SetRectRgn;

VAR
  os_version : WinBase.OSVERSIONINFO;
  is_chicago : BOOLEAN;
BEGIN
  os_version.dwOSVersionInfoSize := BYTESIZE (os_version);
  VAR b := WinBase.GetVersionEx (ADR (os_version)); BEGIN <*ASSERT b # 0*> END;
  is_chicago := (os_version.dwPlatformId = WinBase.VER_PLATFORM_WIN32_WINDOWS);
END WinGDI.

