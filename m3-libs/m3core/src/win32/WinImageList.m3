MODULE WinImageList;

(* 
	Based on commctrl.h version 1.2
	Copyright 1991-1998, Microsoft Corp. All rights reserved.
	Copyright Darko Volaric 2002 darko@peter.com.au
*)


FROM WinDef IMPORT HICON;
FROM Ctypes IMPORT int;
FROM WinDef IMPORT BOOL;
FROM WinDef IMPORT HINSTANCE;
FROM WinNT IMPORT LPCSTR;
FROM WinDef IMPORT COLORREF;

PROCEDURE AddIcon(himl: HIMAGELIST; hicon: HICON): int =
BEGIN
	RETURN ReplaceIcon(himl, -1, hicon);
END AddIcon;

PROCEDURE RemoveAll(himl: HIMAGELIST): BOOL =
BEGIN
	RETURN Remove(himl, -1);
END RemoveAll;

PROCEDURE ExtractIcon(<*UNUSED*>hi: HINSTANCE; himl: HIMAGELIST; i: int): HICON =
BEGIN
	RETURN GetIcon(himl, i, 0);
END ExtractIcon;

PROCEDURE LoadBitmap(hi: HINSTANCE; lpbmp: LPCSTR; cx: int; cGrow: int; crMask: COLORREF): HIMAGELIST =
BEGIN
	RETURN LoadImage(hi, lpbmp, cx, cGrow, crMask, 0 (* IMAGE_BITMAP *), 0);
END LoadBitmap;

BEGIN
END WinImageList.
