UNSAFE MODULE WinTabCon;


(* 
	Based on commctrl.h version 1.2
	Copyright 1991-1998, Microsoft Corp. All rights reserved.
	Copyright Darko Volaric 2002 darko@peter.com.au
*)

IMPORT
	WinDef, WinUser, WinImageList;

PROCEDURE InsertItem(hwnd: WinDef.HWND; iItem: INTEGER; READONLY pitem: LPTCITEM): INTEGER =
BEGIN
	RETURN WinUser.SendMessage(hwnd, TCM_INSERTITEM, iItem, LOOPHOLE(pitem, WinDef.LPARAM));
END InsertItem;

PROCEDURE GetCurSel(hwnd: WinDef.HWND): INTEGER =
BEGIN
	RETURN WinUser.SendMessage(hwnd, TCM_GETCURSEL, 0, 0);
END GetCurSel;

PROCEDURE SetCurSel(hwnd: WinDef.HWND; i: INTEGER): INTEGER =
BEGIN
	RETURN WinUser.SendMessage(hwnd, TCM_SETCURSEL, i, 0);
END SetCurSel;

PROCEDURE GetImageList(hwnd: WinDef.HWND): WinImageList.HIMAGELIST =
BEGIN
	RETURN LOOPHOLE(WinUser.SendMessage(hwnd, TCM_GETIMAGELIST, 0, 0), WinImageList.HIMAGELIST);
END GetImageList;

PROCEDURE SetImageList(hwnd: WinDef.HWND; himl: WinImageList.HIMAGELIST): WinImageList.HIMAGELIST =
BEGIN
	RETURN LOOPHOLE(WinUser.SendMessage(hwnd, TCM_SETIMAGELIST, 0, LOOPHOLE(himl, WinDef.LPARAM)), WinImageList.HIMAGELIST);
END SetImageList;

PROCEDURE GetItemCount(hwnd: WinDef.HWND): INTEGER =
BEGIN
	RETURN WinUser.SendMessage(hwnd, TCM_GETITEMCOUNT, 0, 0);
END GetItemCount;

PROCEDURE GetItem(hwnd: WinDef.HWND; iItem: INTEGER; pitem: LPTCITEM): BOOLEAN =
BEGIN
	RETURN VAL(WinUser.SendMessage(hwnd, TCM_GETITEM, iItem, LOOPHOLE(pitem, WinDef.LPARAM)), BOOLEAN);
END GetItem;

PROCEDURE SetItem(hwnd: WinDef.HWND; iItem: INTEGER; pitem: LPTCITEM): BOOLEAN =
BEGIN
	RETURN VAL(WinUser.SendMessage(hwnd, TCM_SETITEM, iItem, LOOPHOLE(pitem, WinDef.LPARAM)), BOOLEAN);
END SetItem;

PROCEDURE DeleteItem(hwnd: WinDef.HWND; iItem: INTEGER): BOOLEAN =
BEGIN
	RETURN VAL(WinUser.SendMessage(hwnd, TCM_DELETEITEM, iItem, 0), BOOLEAN);
END DeleteItem;

PROCEDURE DeleteAllItems(hwnd: WinDef.HWND): BOOLEAN =
BEGIN
	RETURN VAL(WinUser.SendMessage(hwnd, TCM_DELETEALLITEMS, 0, 0), BOOLEAN);
END DeleteAllItems;

PROCEDURE GetItemRect(hwnd: WinDef.HWND; i: INTEGER; prc: WinDef.LPRECT): BOOLEAN =
BEGIN
	RETURN VAL(WinUser.SendMessage(hwnd, TCM_GETITEMRECT, i, LOOPHOLE(prc, WinDef.LPARAM)), BOOLEAN);
END GetItemRect;

PROCEDURE HitTest(hwnd: WinDef.HWND; pinfo: LPTCHITTESTINFO): INTEGER =
BEGIN
	RETURN WinUser.SendMessage(hwnd, TCM_HITTEST, 0, LOOPHOLE(pinfo, WinDef.LPARAM));
END HitTest;

PROCEDURE SetItemExtra(hwnd: WinDef.HWND; cb: INTEGER): BOOLEAN =  
BEGIN
	RETURN VAL(WinUser.SendMessage(hwnd, TCM_SETITEMEXTRA, cb, 0), BOOLEAN);
END SetItemExtra;

PROCEDURE AdjustRect(hwnd: WinDef.HWND; bLarger: BOOLEAN; prc: WinDef.LPRECT): INTEGER =
BEGIN
	RETURN WinUser.SendMessage(hwnd, TCM_ADJUSTRECT, ORD(bLarger), LOOPHOLE(prc, WinDef.LPARAM));
END AdjustRect;

PROCEDURE SetItemSize(hwnd: WinDef.HWND; x, y: WinDef.WORD): WinDef.DWORD =
BEGIN
	RETURN LOOPHOLE(WinUser.SendMessage(hwnd, TCM_SETITEMSIZE, 0, WinDef.MAKELONG(x, y)), WinDef.DWORD);
END SetItemSize;

PROCEDURE RemoveImage(hwnd: WinDef.HWND; i: INTEGER) =
BEGIN
	EVAL WinUser.SendMessage(hwnd, TCM_REMOVEIMAGE, i, 0);
END RemoveImage;

PROCEDURE SetPadding(hwnd: WinDef.HWND; x, y: WinDef.WORD) =
BEGIN
	EVAL WinUser.SendMessage(hwnd, TCM_SETPADDING, 0, WinDef.MAKELONG(x, y));
END SetPadding;

PROCEDURE GetRowCount(hwnd: WinDef.HWND): INTEGER =
BEGIN
	RETURN WinUser.SendMessage(hwnd, TCM_GETROWCOUNT, 0, 0);
END GetRowCount;

PROCEDURE GetToolTips(hwnd: WinDef.HWND): WinDef.HWND =
BEGIN
	RETURN LOOPHOLE(WinUser.SendMessage(hwnd, TCM_GETTOOLTIPS, 0, 0), WinDef.HWND);
END GetToolTips;

PROCEDURE SetToolTips(hwnd: WinDef.HWND; hwndTT: WinDef.HWND) =
BEGIN
	EVAL WinUser.SendMessage(hwnd, TCM_SETTOOLTIPS, LOOPHOLE(hwndTT, WinDef.WPARAM), 0);
END SetToolTips;

PROCEDURE GetCurFocus(hwnd: WinDef.HWND): INTEGER =
BEGIN
	RETURN WinUser.SendMessage(hwnd, TCM_GETCURFOCUS, 0, 0);
END GetCurFocus;

PROCEDURE SetCurFocus(hwnd: WinDef.HWND; i: INTEGER) =
BEGIN
	EVAL WinUser.SendMessage(hwnd, TCM_SETCURFOCUS, i, 0);
END SetCurFocus;

PROCEDURE SetMinTabWidth(hwnd: WinDef.HWND; x: INTEGER): INTEGER =
BEGIN
	RETURN WinUser.SendMessage(hwnd, TCM_SETMINTABWIDTH, 0, x);
END SetMinTabWidth;

PROCEDURE DeselectAll(hwnd: WinDef.HWND; fExcludeFocus: BOOLEAN) =
BEGIN
	EVAL WinUser.SendMessage(hwnd, TCM_DESELECTALL, ORD(fExcludeFocus), 0);
END DeselectAll;

PROCEDURE HighlightItem(hwnd: WinDef.HWND; i: INTEGER; fHighlight: BOOLEAN): BOOLEAN =  
BEGIN
	RETURN VAL(WinUser.SendMessage(hwnd, TCM_HIGHLIGHTITEM, i, WinDef.MAKELONG(ORD(fHighlight), 0)), BOOLEAN);
END HighlightItem;

PROCEDURE SetUnicodeFormat(hwnd: WinDef.HWND; fUnicode: BOOLEAN): BOOLEAN =  
BEGIN
	RETURN VAL(WinUser.SendMessage(hwnd, TCM_SETUNICODEFORMAT, ORD(fUnicode), 0), BOOLEAN);
END SetUnicodeFormat;

PROCEDURE GetUnicodeFormat(hwnd: WinDef.HWND): BOOLEAN =  
BEGIN
	RETURN VAL(WinUser.SendMessage(hwnd, TCM_GETUNICODEFORMAT, 0, 0), BOOLEAN);
END GetUnicodeFormat;

PROCEDURE SetExtendedStyle(hwnd: WinDef.HWND; dw: WinDef.DWORD): WinDef.DWORD =  
BEGIN
	RETURN LOOPHOLE(WinUser.SendMessage(hwnd, TCM_SETEXTENDEDSTYLE, 0, LOOPHOLE(dw, WinDef.LPARAM)), WinDef.DWORD);
END SetExtendedStyle;

PROCEDURE GetExtendedStyle(hwnd: WinDef.HWND): WinDef.DWORD =  
BEGIN
	RETURN LOOPHOLE(WinUser.SendMessage(hwnd, TCM_GETEXTENDEDSTYLE, 0, 0), WinDef.DWORD);
END GetExtendedStyle;

BEGIN
END WinTabCon.