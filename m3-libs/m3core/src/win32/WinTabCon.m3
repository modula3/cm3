UNSAFE MODULE WinTabCon;

(* 
    Based on commctrl.h version 1.2
    Copyright 1991-1998, Microsoft Corp. All rights reserved.
    Copyright Darko Volaric 2002 darko@peter.com.au
*)

FROM WinDef IMPORT UINT32, UINT16, LPARAM, HWND, PRECT, MAKELONG, WPARAM;
FROM WinUser IMPORT SendMessage;
FROM WinImageList IMPORT HIMAGELIST;

PROCEDURE InsertItem(hwnd: HWND; iItem: INTEGER; READONLY pitem: PTCITEM): INTEGER =
BEGIN
    RETURN SendMessage(hwnd, TCM_INSERTITEM, iItem, LOOPHOLE(pitem, LPARAM));
END InsertItem;

PROCEDURE GetCurSel(hwnd: HWND): INTEGER =
BEGIN
    RETURN SendMessage(hwnd, TCM_GETCURSEL, 0, 0);
END GetCurSel;

PROCEDURE SetCurSel(hwnd: HWND; i: INTEGER): INTEGER =
BEGIN
    RETURN SendMessage(hwnd, TCM_SETCURSEL, i, 0);
END SetCurSel;

PROCEDURE GetImageList(hwnd: HWND): HIMAGELIST =
BEGIN
    RETURN LOOPHOLE(SendMessage(hwnd, TCM_GETIMAGELIST, 0, 0), HIMAGELIST);
END GetImageList;

PROCEDURE SetImageList(hwnd: HWND; himl: HIMAGELIST): HIMAGELIST =
BEGIN
    RETURN LOOPHOLE(SendMessage(hwnd, TCM_SETIMAGELIST, 0, LOOPHOLE(himl, LPARAM)), HIMAGELIST);
END SetImageList;

PROCEDURE GetItemCount(hwnd: HWND): INTEGER =
BEGIN
    RETURN SendMessage(hwnd, TCM_GETITEMCOUNT, 0, 0);
END GetItemCount;

PROCEDURE GetItem(hwnd: HWND; iItem: INTEGER; pitem: PTCITEM): BOOLEAN =
BEGIN
    RETURN VAL(SendMessage(hwnd, TCM_GETITEM, iItem, LOOPHOLE(pitem, LPARAM)), BOOLEAN);
END GetItem;

PROCEDURE SetItem(hwnd: HWND; iItem: INTEGER; pitem: PTCITEM): BOOLEAN =
BEGIN
    RETURN VAL(SendMessage(hwnd, TCM_SETITEM, iItem, LOOPHOLE(pitem, LPARAM)), BOOLEAN);
END SetItem;

PROCEDURE DeleteItem(hwnd: HWND; iItem: INTEGER): BOOLEAN =
BEGIN
    RETURN VAL(SendMessage(hwnd, TCM_DELETEITEM, iItem, 0), BOOLEAN);
END DeleteItem;

PROCEDURE DeleteAllItems(hwnd: HWND): BOOLEAN =
BEGIN
    RETURN VAL(SendMessage(hwnd, TCM_DELETEALLITEMS, 0, 0), BOOLEAN);
END DeleteAllItems;

PROCEDURE GetItemRect(hwnd: HWND; i: INTEGER; prc: PRECT): BOOLEAN =
BEGIN
    RETURN VAL(SendMessage(hwnd, TCM_GETITEMRECT, i, LOOPHOLE(prc, LPARAM)), BOOLEAN);
END GetItemRect;

PROCEDURE HitTest(hwnd: HWND; pinfo: PTCHITTESTINFO): INTEGER =
BEGIN
    RETURN SendMessage(hwnd, TCM_HITTEST, 0, LOOPHOLE(pinfo, LPARAM));
END HitTest;

PROCEDURE SetItemExtra(hwnd: HWND; cb: INTEGER): BOOLEAN =  
BEGIN
    RETURN VAL(SendMessage(hwnd, TCM_SETITEMEXTRA, cb, 0), BOOLEAN);
END SetItemExtra;

PROCEDURE AdjustRect(hwnd: HWND; bLarger: BOOLEAN; prc: PRECT): INTEGER =
BEGIN
    RETURN SendMessage(hwnd, TCM_ADJUSTRECT, ORD(bLarger), LOOPHOLE(prc, LPARAM));
END AdjustRect;

PROCEDURE SetItemSize(hwnd: HWND; x, y: UINT16): UINT32 =
BEGIN
    RETURN LOOPHOLE(SendMessage(hwnd, TCM_SETITEMSIZE, 0, MAKELONG(x, y)), UINT32);
END SetItemSize;

PROCEDURE RemoveImage(hwnd: HWND; i: INTEGER) =
BEGIN
    EVAL SendMessage(hwnd, TCM_REMOVEIMAGE, i, 0);
END RemoveImage;

PROCEDURE SetPadding(hwnd: HWND; x, y: UINT16) =
BEGIN
    EVAL SendMessage(hwnd, TCM_SETPADDING, 0, MAKELONG(x, y));
END SetPadding;

PROCEDURE GetRowCount(hwnd: HWND): INTEGER =
BEGIN
    RETURN SendMessage(hwnd, TCM_GETROWCOUNT, 0, 0);
END GetRowCount;

PROCEDURE GetToolTips(hwnd: HWND): HWND =
BEGIN
    RETURN LOOPHOLE(SendMessage(hwnd, TCM_GETTOOLTIPS, 0, 0), HWND);
END GetToolTips;

PROCEDURE SetToolTips(hwnd: HWND; hwndTT: HWND) =
BEGIN
    EVAL SendMessage(hwnd, TCM_SETTOOLTIPS, LOOPHOLE(hwndTT, WPARAM), 0);
END SetToolTips;

PROCEDURE GetCurFocus(hwnd: HWND): INTEGER =
BEGIN
    RETURN SendMessage(hwnd, TCM_GETCURFOCUS, 0, 0);
END GetCurFocus;

PROCEDURE SetCurFocus(hwnd: HWND; i: INTEGER) =
BEGIN
    EVAL SendMessage(hwnd, TCM_SETCURFOCUS, i, 0);
END SetCurFocus;

PROCEDURE SetMinTabWidth(hwnd: HWND; x: INTEGER): INTEGER =
BEGIN
    RETURN SendMessage(hwnd, TCM_SETMINTABWIDTH, 0, x);
END SetMinTabWidth;

PROCEDURE DeselectAll(hwnd: HWND; fExcludeFocus: BOOLEAN) =
BEGIN
    EVAL SendMessage(hwnd, TCM_DESELECTALL, ORD(fExcludeFocus), 0);
END DeselectAll;

PROCEDURE HighlightItem(hwnd: HWND; i: INTEGER; fHighlight: BOOLEAN): BOOLEAN =  
BEGIN
    RETURN VAL(SendMessage(hwnd, TCM_HIGHLIGHTITEM, i, MAKELONG(ORD(fHighlight), 0)), BOOLEAN);
END HighlightItem;

PROCEDURE SetUnicodeFormat(hwnd: HWND; fUnicode: BOOLEAN): BOOLEAN =  
BEGIN
    RETURN VAL(SendMessage(hwnd, TCM_SETUNICODEFORMAT, ORD(fUnicode), 0), BOOLEAN);
END SetUnicodeFormat;

PROCEDURE GetUnicodeFormat(hwnd: HWND): BOOLEAN =  
BEGIN
    RETURN VAL(SendMessage(hwnd, TCM_GETUNICODEFORMAT, 0, 0), BOOLEAN);
END GetUnicodeFormat;

PROCEDURE SetExtendedStyle(hwnd: HWND; dw: UINT32): UINT32 =  
BEGIN
    RETURN LOOPHOLE(SendMessage(hwnd, TCM_SETEXTENDEDSTYLE, 0, LOOPHOLE(dw, LPARAM)), UINT32);
END SetExtendedStyle;

PROCEDURE GetExtendedStyle(hwnd: HWND): UINT32 =  
BEGIN
    RETURN LOOPHOLE(SendMessage(hwnd, TCM_GETEXTENDEDSTYLE, 0, 0), UINT32);
END GetExtendedStyle;

BEGIN
END WinTabCon.
