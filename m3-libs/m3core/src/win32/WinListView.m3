UNSAFE MODULE WinListView;


(* 
	Based on commctrl.h version 1.2
	Copyright 1991-1998, Microsoft Corp. All rights reserved.
	Copyright Darko Volaric 2002 darko@peter.com.au
*)

IMPORT
	WinUser, WinDef, WinNT, Text8, M3toC, WinImageList, Word;

PROCEDURE GetItem(hwnd: WinDef.HWND; pitem: LPLVITEM): BOOLEAN =
BEGIN
	RETURN VAL(WinUser.SendMessage(hwnd, LVM_GETITEM, 0, LOOPHOLE(pitem, WinDef.LPARAM)), BOOLEAN);
END GetItem;

PROCEDURE SetItem(hwnd: WinDef.HWND; pitem: LPLVITEM): BOOLEAN =
BEGIN
	RETURN VAL(WinUser.SendMessage(hwnd, LVM_SETITEM, 0, LOOPHOLE(pitem, WinDef.LPARAM)), BOOLEAN);
END SetItem;

PROCEDURE InsertItem(hwnd: WinDef.HWND; pitem: LPLVITEM): INTEGER =
BEGIN
	RETURN WinUser.SendMessage(hwnd, LVM_INSERTITEM, 0, LOOPHOLE(pitem, WinDef.LPARAM));
END InsertItem;

PROCEDURE DeleteItem(hwnd: WinDef.HWND; i: INTEGER): BOOLEAN =
BEGIN
	RETURN VAL(WinUser.SendMessage(hwnd, LVM_DELETEITEM, i, 0), BOOLEAN);
END DeleteItem;

PROCEDURE DeleteAllItems(hwnd: WinDef.HWND): BOOLEAN =
BEGIN
	RETURN VAL(WinUser.SendMessage(hwnd, LVM_DELETEALLITEMS, 0, 0), BOOLEAN);
END DeleteAllItems;

PROCEDURE GetItemCount(hwnd: WinDef.HWND): INTEGER =
BEGIN
	RETURN WinUser.SendMessage(hwnd, LVM_GETITEMCOUNT, 0, 0);
END GetItemCount;

PROCEDURE EditLabel(hwnd: WinDef.HWND; iItem: INTEGER): WinDef.HWND =
BEGIN
	RETURN LOOPHOLE(WinUser.SendMessage(hwnd, LVM_EDITLABEL, iItem, 0), WinDef.HWND);
END EditLabel;

PROCEDURE GetEditControl(hwnd: WinDef.HWND): WinDef.HWND =
BEGIN
	RETURN LOOPHOLE(WinUser.SendMessage(hwnd, LVM_GETEDITCONTROL, 0, 0), WinDef.HWND);
END GetEditControl;

PROCEDURE GetColumn(hwnd: WinDef.HWND; iCol: INTEGER; pcol: LPLVCOLUMN): BOOLEAN =
BEGIN
	RETURN VAL(WinUser.SendMessage(hwnd, LVM_GETCOLUMN, iCol, LOOPHOLE(pcol, WinDef.LPARAM)), BOOLEAN);
END GetColumn;

PROCEDURE SetColumn(hwnd: WinDef.HWND; iCol: INTEGER; pcol: LPLVCOLUMN): BOOLEAN =
BEGIN
	RETURN VAL(WinUser.SendMessage(hwnd, LVM_SETCOLUMN, iCol, LOOPHOLE(pcol, WinDef.LPARAM)), BOOLEAN);
END SetColumn;

PROCEDURE InsertColumn(hwnd: WinDef.HWND; iCol: INTEGER; pcol: LPLVCOLUMN): INTEGER =
BEGIN
	RETURN WinUser.SendMessage(hwnd, LVM_INSERTCOLUMN, iCol, LOOPHOLE(pcol, WinDef.LPARAM));
END InsertColumn;

PROCEDURE DeleteColumn(hwnd: WinDef.HWND; iCol: INTEGER): BOOLEAN =
BEGIN
	RETURN VAL(WinUser.SendMessage(hwnd, LVM_DELETECOLUMN, iCol, 0), BOOLEAN);
END DeleteColumn;

PROCEDURE SetItemText(hwnd: WinDef.HWND; i, iSubItem: INTEGER; text:  TEXT) =
VAR
	item: LVITEM;
BEGIN
	item.iSubItem := iSubItem;
	item.pszText := M3toC.CopyTtoS(text);
	EVAL WinUser.SendMessage(hwnd, LVM_SETITEMTEXT, i, LOOPHOLE(ADR(item),  WinDef.LPARAM));
	M3toC.FreeCopiedS(item.pszText);
END SetItemText;

PROCEDURE SetExtendedListViewStyle(hwnd: WinDef.HWND; dw: WinDef.DWORD) =
BEGIN
	EVAL WinUser.SendMessage(hwnd, LVM_SETEXTENDEDLISTVIEWSTYLE, 0, dw);
END SetExtendedListViewStyle;

PROCEDURE GetNextItem(hwnd: WinDef.HWND; iStart: INTEGER; flags: WinDef.UINT): INTEGER =
BEGIN
	RETURN WinUser.SendMessage(hwnd, LVM_GETNEXTITEM, iStart, flags);
END GetNextItem;

PROCEDURE SetItemState(hwnd: WinDef.HWND; i: INTEGER; state, mask: WinDef.UINT): BOOLEAN =
VAR
	item: LVITEM;
BEGIN
	item.stateMask := mask;
	item.state := state;
	RETURN VAL(WinUser.SendMessage(hwnd, LVM_SETITEMSTATE, i, LOOPHOLE(ADR(item), WinDef.LPARAM)), BOOLEAN);
END SetItemState;

PROCEDURE GetItemText(hwnd: WinDef.HWND; i, iSubItem: INTEGER): TEXT =
CONST
	maxchars = 256;
VAR
	item: LVITEM;
	buf := ARRAY[0..maxchars-1] OF CHAR{VAL(0, CHAR),..};
BEGIN
	item.iSubItem := iSubItem;
	item.cchTextMax := maxchars;
	item.pszText := ADR(buf);
	EVAL WinUser.SendMessage(hwnd, LVM_GETITEMTEXT, i, LOOPHOLE(ADR(item), WinDef.LPARAM));
	RETURN Text8.New(buf);
END GetItemText;

PROCEDURE CreateDragImage(hwnd: WinDef.HWND; i: INTEGER; lpptUpLeft: WinDef.LPPOINT): WinImageList.HIMAGELIST =
BEGIN
	RETURN LOOPHOLE(WinUser.SendMessage(hwnd, LVM_CREATEDRAGIMAGE, i, 
		LOOPHOLE(lpptUpLeft, WinDef.LPARAM)), WinImageList.HIMAGELIST);
END CreateDragImage;

PROCEDURE HitTest(hwnd: WinDef.HWND; pinfo: LPLVHITTESTINFO): INTEGER =
BEGIN
	RETURN WinUser.SendMessage(hwnd, LVM_HITTEST, 0, LOOPHOLE(pinfo, WinDef.LPARAM));
END HitTest;

PROCEDURE Scroll(hwnd: WinDef.HWND; dx, dy: INTEGER): BOOLEAN =
BEGIN
	RETURN VAL(WinUser.SendMessage(hwnd, LVM_SCROLL, dx, dy), BOOLEAN);
END Scroll;

PROCEDURE GetSelectedCount(hwnd: WinDef.HWND): INTEGER =
BEGIN
	RETURN WinUser.SendMessage(hwnd, LVM_GETSELECTEDCOUNT, 0, 0);
END GetSelectedCount;

PROCEDURE SortItems(
	hwnd: WinDef.HWND; 
	callback: <*CALLBACK*> PROCEDURE(p1, p2: ADDRESS; param: ADDRESS): INTEGER; 
	param: ADDRESS
): BOOLEAN =
BEGIN
	RETURN VAL(WinUser.SendMessage(hwnd, LVM_SORTITEMS, LOOPHOLE(param, WinDef.WPARAM), LOOPHOLE(callback, WinDef.LPARAM)), BOOLEAN);
END SortItems;

PROCEDURE SortItemsEx(
	hwnd: WinDef.HWND; 
	callback: <*CALLBACK*> PROCEDURE(p1, p2: INTEGER; param: ADDRESS): INTEGER; 
	param: ADDRESS
): BOOLEAN =
BEGIN
	RETURN VAL(WinUser.SendMessage(hwnd, LVM_SORTITEMSEX, LOOPHOLE(param, WinDef.WPARAM), LOOPHOLE(callback, WinDef.LPARAM)), BOOLEAN);
END SortItemsEx;

PROCEDURE GetColumnWidth(hwnd: WinDef.HWND; iCol: INTEGER): INTEGER =
BEGIN
	RETURN WinUser.SendMessage(hwnd, LVM_GETCOLUMNWIDTH, iCol, 0);
END GetColumnWidth;

PROCEDURE SetColumnWidth(hwnd: WinDef.HWND; iCol, cx: INTEGER): BOOLEAN =
BEGIN
	RETURN VAL(WinUser.SendMessage(hwnd, LVM_SETCOLUMNWIDTH, iCol, WinDef.MAKELONG(cx, 0)), BOOLEAN);
END SetColumnWidth;

PROCEDURE GetHeader(hwnd: WinDef.HWND): WinDef.HWND =
BEGIN
	RETURN LOOPHOLE(WinUser.SendMessage(hwnd, LVM_GETHEADER, 0, 0), WinDef.HWND);
END GetHeader;

PROCEDURE GetBkColor(hwnd: WinDef.HWND): WinDef.COLORREF =
BEGIN
	RETURN LOOPHOLE(WinUser.SendMessage(hwnd, LVM_GETBKCOLOR, 0, 0), WinDef.COLORREF);
END GetBkColor;

PROCEDURE SetBkColor(hwnd: WinDef.HWND; clrBk: WinDef.COLORREF): BOOLEAN =
BEGIN
	RETURN VAL(WinUser.SendMessage(hwnd, LVM_SETBKCOLOR, 0, clrBk), BOOLEAN);
END SetBkColor;

PROCEDURE GetImageList(hwnd: WinDef.HWND; iImageList: INTEGER): WinImageList.HIMAGELIST =
BEGIN
	RETURN LOOPHOLE(WinUser.SendMessage(hwnd, LVM_GETIMAGELIST, iImageList, 0), WinImageList.HIMAGELIST);
END GetImageList;

PROCEDURE SetImageList(hwnd: WinDef.HWND; himl: WinImageList.HIMAGELIST; iImageList: INTEGER): WinImageList.HIMAGELIST =
BEGIN
	RETURN LOOPHOLE(WinUser.SendMessage(hwnd, LVM_SETIMAGELIST, iImageList, LOOPHOLE(himl, WinDef.LPARAM)), WinImageList.HIMAGELIST);
END SetImageList;

PROCEDURE GetCallbackMask(hwnd: WinDef.HWND): WinDef.UINT =
BEGIN
	RETURN LOOPHOLE(WinUser.SendMessage(hwnd, LVM_GETCALLBACKMASK, 0, 0), WinDef.UINT);
END GetCallbackMask;

PROCEDURE SetCallbackMask(hwnd: WinDef.HWND; mask: WinDef.UINT): BOOLEAN =
BEGIN
	RETURN VAL(WinUser.SendMessage(hwnd, LVM_SETCALLBACKMASK, mask, 0), BOOLEAN);
END SetCallbackMask;

PROCEDURE FindItem(hwnd: WinDef.HWND; iStart: INTEGER; plvfi: LPFINDINFO): INTEGER =
BEGIN
	RETURN WinUser.SendMessage(hwnd, LVM_FINDITEM, iStart, LOOPHOLE(plvfi, WinDef.LPARAM));
END FindItem;

PROCEDURE GetItemRect(hwnd: WinDef.HWND; i: INTEGER; prc: WinDef.LPRECT; code: INTEGER): BOOLEAN =
BEGIN
	IF prc # NIL THEN
		prc.left := code;
	END;
	(* for connoisseurs of the grotesque, the above statement was derived from the following C expression:
	
		 ((prc) ? (((RECT FAR * )(prc))->left = (code),(LPARAM)(RECT FAR* )(prc)) : (LPARAM)(RECT FAR* )NULL))
	
		 Please correct me if my translation is wrong - Darko.
	*)
	
	RETURN VAL(WinUser.SendMessage(hwnd, LVM_GETITEMRECT, i, LOOPHOLE(prc, WinDef.LPARAM)), BOOLEAN);
END GetItemRect;

PROCEDURE SetItemPosition(hwnd: WinDef.HWND; i: INTEGER; x, y: WinDef.WORD): BOOLEAN =
BEGIN
	RETURN VAL(WinUser.SendMessage(hwnd, LVM_SETITEMPOSITION, i, WinDef.MAKELONG(x, y)), BOOLEAN);
END SetItemPosition;

PROCEDURE GetItemPosition(hwnd: WinDef.HWND; i: INTEGER; ppt: WinDef.LPPOINT): BOOLEAN =
BEGIN
	RETURN VAL(WinUser.SendMessage(hwnd, LVM_GETITEMPOSITION, i, LOOPHOLE(ppt, WinDef.LPARAM)), BOOLEAN);
END GetItemPosition;

PROCEDURE GetStringWidth(hwnd: WinDef.HWND; psz: WinNT.LPCTSTR): INTEGER =
BEGIN
	RETURN WinUser.SendMessage(hwnd, LVM_GETSTRINGWIDTH, 0, LOOPHOLE(psz, WinDef.LPARAM));
END GetStringWidth;

PROCEDURE EnsureVisible(hwnd: WinDef.HWND; i: INTEGER; fPartialOK: BOOLEAN): BOOLEAN =
BEGIN
	RETURN VAL(WinUser.SendMessage(hwnd, LVM_ENSUREVISIBLE, i, WinDef.MAKELONG(ORD(fPartialOK), 0)), BOOLEAN);
END EnsureVisible;

PROCEDURE RedrawItems(hwnd: WinDef.HWND; iFirst, iLast: INTEGER): BOOLEAN =
BEGIN
	RETURN VAL(WinUser.SendMessage(hwnd, LVM_REDRAWITEMS, iFirst, iLast), BOOLEAN);
END RedrawItems;

PROCEDURE Arrange(hwnd: WinDef.HWND; code: WinDef.UINT): BOOLEAN =
BEGIN
	RETURN VAL(WinUser.SendMessage(hwnd, LVM_ARRANGE, LOOPHOLE(code, WinDef.WPARAM), 0), BOOLEAN);
END Arrange;

PROCEDURE GetViewRect(hwnd: WinDef.HWND; prc: WinDef.LPRECT): BOOLEAN =
BEGIN
	RETURN VAL(WinUser.SendMessage(hwnd, LVM_GETVIEWRECT, 0, LOOPHOLE(prc, WinDef.LPARAM)), BOOLEAN);
END GetViewRect;

PROCEDURE GetTextColor(hwnd: WinDef.HWND): WinDef.COLORREF =
BEGIN
	RETURN LOOPHOLE(WinUser.SendMessage(hwnd, LVM_GETTEXTCOLOR, 0, 0), WinDef.COLORREF);
END GetTextColor;

PROCEDURE SetTextColor(hwnd: WinDef.HWND; clrText: WinDef.COLORREF): BOOLEAN =
BEGIN
	RETURN VAL(WinUser.SendMessage(hwnd, LVM_SETTEXTCOLOR, 0, LOOPHOLE(clrText, WinDef.LPARAM)), BOOLEAN);
END SetTextColor;

PROCEDURE GetTextBkColor(hwnd: WinDef.HWND): WinDef.COLORREF =
BEGIN
	RETURN LOOPHOLE(WinUser.SendMessage(hwnd, LVM_GETTEXTBKCOLOR, 0, 0), WinDef.COLORREF);
END GetTextBkColor;

PROCEDURE SetTextBkColor(hwnd: WinDef.HWND; clrTextBk: WinDef.COLORREF): BOOLEAN =
BEGIN
	RETURN VAL(WinUser.SendMessage(hwnd, LVM_SETTEXTBKCOLOR, 0, LOOPHOLE(clrTextBk, WinDef.LPARAM)), BOOLEAN);
END SetTextBkColor;

PROCEDURE GetTopIndex(hwnd: WinDef.HWND): INTEGER =
BEGIN
	RETURN WinUser.SendMessage(hwnd, LVM_GETTOPINDEX, 0, 0);
END GetTopIndex;

PROCEDURE GetCountPerPage(hwnd: WinDef.HWND): INTEGER =
BEGIN
	RETURN WinUser.SendMessage(hwnd, LVM_GETCOUNTPERPAGE, 0, 0);
END GetCountPerPage;

PROCEDURE GetOrigin(hwnd: WinDef.HWND; ppt: WinDef.LPPOINT): BOOLEAN =
BEGIN
	RETURN VAL(WinUser.SendMessage(hwnd, LVM_GETORIGIN, 0, LOOPHOLE(ppt, WinDef.LPARAM)), BOOLEAN);
END GetOrigin;

PROCEDURE Update(hwnd: WinDef.HWND; i: INTEGER): BOOLEAN =
BEGIN
	RETURN VAL(WinUser.SendMessage(hwnd, LVM_UPDATE, i, 0), BOOLEAN);
END Update;

PROCEDURE SetUnicodeFormat(hwnd: WinDef.HWND; fUnicode: BOOLEAN): BOOLEAN =
BEGIN
	RETURN VAL(WinUser.SendMessage(hwnd, LVM_SETUNICODEFORMAT, ORD(fUnicode), 0), BOOLEAN);
END SetUnicodeFormat;

PROCEDURE GetUnicodeFormat(hwnd: WinDef.HWND): BOOLEAN =
BEGIN
	RETURN VAL(WinUser.SendMessage(hwnd, LVM_SETUNICODEFORMAT, 0, 0), BOOLEAN);
END GetUnicodeFormat;

PROCEDURE GetItemState(hwnd: WinDef.HWND; i: INTEGER; mask: WinDef.UINT): WinDef.UINT =
BEGIN
	RETURN LOOPHOLE(WinUser.SendMessage(hwnd, LVM_GETITEMSTATE, i, LOOPHOLE(mask, WinDef.LPARAM)), WinDef.UINT);
END GetItemState;

PROCEDURE SetItemCount(hwnd: WinDef.HWND; cItems: INTEGER) =
BEGIN
	EVAL WinUser.SendMessage(hwnd, LVM_SETITEMCOUNT, cItems, 0);
END SetItemCount;

PROCEDURE SetItemCountEx(hwnd: WinDef.HWND; cItems: INTEGER; dwFlags: WinDef.DWORD) =
BEGIN
	EVAL WinUser.SendMessage(hwnd, LVM_SETITEMCOUNT, cItems, dwFlags);
END SetItemCountEx;

PROCEDURE GetItemSpacing(hwnd: WinDef.HWND; fSmall: BOOLEAN): WinDef.DWORD =
BEGIN
	RETURN LOOPHOLE(WinUser.SendMessage(hwnd, LVM_GETITEMSPACING, ORD(fSmall), 0), WinDef.DWORD);
END GetItemSpacing;

PROCEDURE GetISearchString(hwnd: WinDef.HWND; lpsz: WinNT.LPSTR): BOOLEAN =
BEGIN
	RETURN VAL(WinUser.SendMessage(hwnd, LVM_GETISEARCHSTRING, 0, LOOPHOLE(lpsz, WinDef.LPARAM)), BOOLEAN);
END GetISearchString;

PROCEDURE SetIconSpacing(hwnd: WinDef.HWND; cx, cy: WinDef.WORD): WinDef.DWORD =
BEGIN
	RETURN LOOPHOLE(WinUser.SendMessage(hwnd, LVM_SETICONSPACING, 0, WinDef.MAKELONG(cx,cy)), WinDef.DWORD);
END SetIconSpacing;

PROCEDURE SetExtendedListViewStyleEx(hwnd: WinDef.HWND; dwMask, dw: WinDef.DWORD): WinDef.DWORD =
BEGIN
	RETURN LOOPHOLE(WinUser.SendMessage(hwnd, LVM_SETEXTENDEDLISTVIEWSTYLE, dwMask, dw), WinDef.DWORD);
END SetExtendedListViewStyleEx;

PROCEDURE GetExtendedListViewStyle(hwnd: WinDef.HWND): WinDef.DWORD =
BEGIN
	RETURN LOOPHOLE(WinUser.SendMessage(hwnd, LVM_GETEXTENDEDLISTVIEWSTYLE, 0, 0), WinDef.DWORD);
END GetExtendedListViewStyle;

PROCEDURE SubItemHitTest(hwnd: WinDef.HWND; plvhti: LPLVHITTESTINFO): INTEGER =
BEGIN
	RETURN LOOPHOLE(WinUser.SendMessage(hwnd, LVM_SUBITEMHITTEST, 0, LOOPHOLE(plvhti, WinDef.LPARAM)), INTEGER);
END SubItemHitTest;

PROCEDURE SetColumnOrderArray(hwnd: WinDef.HWND; iCount: INTEGER; pi: RefIntArray): BOOLEAN =
BEGIN
	RETURN VAL(WinUser.SendMessage(hwnd, LVM_SETCOLUMNORDERARRAY, iCount, LOOPHOLE(pi, WinDef.LPARAM)), BOOLEAN);
END SetColumnOrderArray;

PROCEDURE GetColumnOrderArray(hwnd: WinDef.HWND; iCount: INTEGER; pi: RefIntArray): BOOLEAN =
BEGIN
	RETURN VAL(WinUser.SendMessage(hwnd, LVM_GETCOLUMNORDERARRAY, iCount, LOOPHOLE(pi, WinDef.LPARAM)), BOOLEAN);
END GetColumnOrderArray;

PROCEDURE SetHotItem(hwnd: WinDef.HWND; i: INTEGER): INTEGER =
BEGIN
	RETURN LOOPHOLE(WinUser.SendMessage(hwnd, LVM_SETHOTITEM, i, 0), INTEGER);
END SetHotItem;

PROCEDURE GetHotItem(hwnd: WinDef.HWND): INTEGER =
BEGIN
	RETURN LOOPHOLE(WinUser.SendMessage(hwnd, LVM_GETHOTITEM, 0, 0), INTEGER);
END GetHotItem;

PROCEDURE SetHotCursor(hwnd: WinDef.HWND; hcur: WinDef.HCURSOR): WinDef.HCURSOR =
BEGIN
	RETURN LOOPHOLE(WinUser.SendMessage(hwnd, LVM_SETHOTCURSOR, 0, LOOPHOLE(hcur, WinDef.LPARAM)), WinDef.HCURSOR);
END SetHotCursor;

PROCEDURE GetHotCursor(hwnd: WinDef.HWND): WinDef.HCURSOR =
BEGIN
	RETURN LOOPHOLE(WinUser.SendMessage(hwnd, LVM_GETHOTCURSOR, 0, 0), WinDef.HCURSOR);
END GetHotCursor;

PROCEDURE ApproximateViewRect(hwnd: WinDef.HWND; iWidth, iHeight: WinDef.WORD; iCount: INTEGER): WinDef.DWORD =
BEGIN
	RETURN LOOPHOLE(WinUser.SendMessage(hwnd, LVM_APPROXIMATEVIEWRECT, iCount, WinDef.MAKELONG(iWidth, iHeight)), WinDef.DWORD);
END ApproximateViewRect;

PROCEDURE SetWorkAreas(hwnd: WinDef.HWND; nWorkAreas: INTEGER; prc: WinDef.LPRECT): BOOLEAN =
BEGIN
	RETURN VAL(WinUser.SendMessage(hwnd, LVM_SETWORKAREAS, nWorkAreas, LOOPHOLE(prc, WinDef.LPARAM)), BOOLEAN);
END SetWorkAreas;

PROCEDURE GetWorkAreas(hwnd: WinDef.HWND; nWorkAreas: INTEGER; prc: WinDef.LPRECT): BOOLEAN =
BEGIN
	RETURN VAL(WinUser.SendMessage(hwnd, LVM_GETWORKAREAS, nWorkAreas, LOOPHOLE(prc, WinDef.LPARAM)), BOOLEAN);
END GetWorkAreas;

PROCEDURE GetNumberOfWorkAreas(hwnd: WinDef.HWND; pnWorkAreas: UNTRACED REF WinDef.UINT): BOOLEAN =
BEGIN
	RETURN VAL(WinUser.SendMessage(hwnd, LVM_GETNUMBEROFWORKAREAS, 0, LOOPHOLE(pnWorkAreas, WinDef.LPARAM)), BOOLEAN);
END GetNumberOfWorkAreas;

PROCEDURE GetSelectionMark(hwnd: WinDef.HWND): INTEGER =
BEGIN
	RETURN LOOPHOLE(WinUser.SendMessage(hwnd, LVM_GETSELECTIONMARK, 0, 0), INTEGER);
END GetSelectionMark;

PROCEDURE SetSelectionMark(hwnd: WinDef.HWND; i: INTEGER): INTEGER =
BEGIN
	RETURN LOOPHOLE(WinUser.SendMessage(hwnd, LVM_SETSELECTIONMARK, i, 0), INTEGER);
END SetSelectionMark;

PROCEDURE SetHoverTime(hwnd: WinDef.HWND; dwHoverTimeMs: WinDef.DWORD): WinDef.DWORD =
BEGIN
	RETURN LOOPHOLE(WinUser.SendMessage(hwnd, LVM_SETHOVERTIME, 0, dwHoverTimeMs), WinDef.DWORD);
END SetHoverTime;

PROCEDURE GetHoverTime(hwnd: WinDef.HWND): WinDef.DWORD =
BEGIN
	RETURN LOOPHOLE(WinUser.SendMessage(hwnd, LVM_GETHOVERTIME, 0, 0), WinDef.DWORD);
END GetHoverTime;

PROCEDURE SetToolTips(hwnd: WinDef.HWND; hwndNewHwnd: WinDef.HWND): WinDef.HWND =
BEGIN
	RETURN LOOPHOLE(WinUser.SendMessage(hwnd, LVM_SETTOOLTIPS, LOOPHOLE(hwndNewHwnd, WinDef.WPARAM), 0), WinDef.HWND);
END SetToolTips;

PROCEDURE GetToolTips(hwnd: WinDef.HWND): WinDef.HWND =
BEGIN
	RETURN LOOPHOLE(WinUser.SendMessage(hwnd, LVM_GETTOOLTIPS, 0, 0), WinDef.HWND);
END GetToolTips;

PROCEDURE SetBkImage(hwnd: WinDef.HWND; plvbki: LPLVBKIMAGE): BOOLEAN =
BEGIN
	RETURN VAL(WinUser.SendMessage(hwnd, LVM_SETBKIMAGEA, 0, LOOPHOLE(plvbki, WinDef.LPARAM)), BOOLEAN);
END SetBkImage;

PROCEDURE GetBkImage(hwnd: WinDef.HWND; plvbki: LPLVBKIMAGE): BOOLEAN =
BEGIN
	RETURN VAL(WinUser.SendMessage(hwnd, LVM_GETBKIMAGEA, 0, LOOPHOLE(plvbki, WinDef.LPARAM)), BOOLEAN);
END GetBkImage;

PROCEDURE GetCheckState(hwnd: WinDef.HWND; i: WinDef.UINT): WinDef.UINT =
BEGIN
	WITH r = WinUser.SendMessage(hwnd, LVM_GETITEMSTATE, LOOPHOLE(i, WinDef.UINT), LVIS_STATEIMAGEMASK) DO
		RETURN Word.RightShift(r, 12) - 1;
	END;
END GetCheckState;

PROCEDURE SetItemPosition32(hwnd: WinDef.HWND; i, x, y: INTEGER) =
VAR
	ptNewPos := WinDef.POINT{x, y};
BEGIN
	EVAL WinUser.SendMessage(hwnd, LVM_SETITEMPOSITION32, i, LOOPHOLE(ADR(ptNewPos), WinDef.LPARAM));
END SetItemPosition32;

PROCEDURE GetSubItemRect(hwnd: WinDef.HWND; iItem, iSubItem, code: INTEGER; prc: WinDef.LPRECT): BOOLEAN =
BEGIN
	IF prc # NIL THEN
		prc.top := iSubItem;
		prc.left := code;
	END;
	(* the above statement was derived from the following C expression:
	
		 ((prc) ? ((((LPRECT)(prc))->top = iSubItem), (((LPRECT)(prc))->left = code), (LPARAM)(prc)) : (LPARAM)(LPRECT)NULL)
	
	*)
	
	RETURN VAL(WinUser.SendMessage(hwnd, LVM_GETSUBITEMRECT, iItem, LOOPHOLE(prc, WinDef.LPARAM)), BOOLEAN);
END GetSubItemRect;

BEGIN
END WinListView.
