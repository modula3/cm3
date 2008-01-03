INTERFACE WinTabCon;


(* 
	Based on commctrl.h version 1.2
	Copyright 1991-1998, Microsoft Corp. All rights reserved.
	Copyright Darko Volaric 2002 darko@peter.com.au
*)

IMPORT
	WinDef, WinImageList;

FROM Word IMPORT Or;


FROM WinNT IMPORT LPSTR;

FROM WinDef IMPORT UINT;
FROM Ctypes IMPORT int;
FROM WinDef IMPORT DWORD;
FROM WinDef IMPORT WORD;
FROM WinDef IMPORT LPARAM;
FROM WinDef IMPORT HWND;
FROM WinDef IMPORT POINT;
FROM WinUser IMPORT NMHDR;
FROM WinCommCtrl IMPORT TCM_FIRST, CCM_SETUNICODEFORMAT, CCM_GETUNICODEFORMAT;

(* constants *)

CONST
	
	WC_TABCONTROL            = "SysTabControl";
	WC_TABCONTROL32          = "SysTabControl32";

	TCS_SCROLLOPPOSITE       = 16_0001;
	TCS_BOTTOM               = 16_0002;
	TCS_RIGHT                = 16_0002;
	TCS_MULTISELECT          = 16_0004;

	TCS_FLATBUTTONS          = 16_0008;

	TCS_FORCEICONLEFT        = 16_0010;
	TCS_FORCELABELLEFT       = 16_0020;

	TCS_HOTTRACK             = 16_0040;
	TCS_VERTICAL             = 16_0080;

	TCS_TABS                 = 16_0000;
	TCS_BUTTONS              = 16_0100;
	TCS_SINGLELINE           = 16_0000;
	TCS_MULTILINE            = 16_0200;
	TCS_RIGHTJUSTIFY         = 16_0000;
	TCS_FIXEDWIDTH           = 16_0400;
	TCS_RAGGEDRIGHT          = 16_0800;
	TCS_FOCUSONBUTTONDOWN    = 16_1000;
	TCS_OWNERDRAWFIXED       = 16_2000;
	TCS_TOOLTIPS             = 16_4000;
	TCS_FOCUSNEVER          =  16_8000;


	TCS_EX_FLATSEPARATORS   =  16_00000001;
	TCS_EX_REGISTERDROP      = 16_00000002;


	TCM_GETIMAGELIST         = (TCM_FIRST + 2);
	TCM_SETIMAGELIST         = (TCM_FIRST + 3);
	TCM_GETITEMCOUNT         = (TCM_FIRST + 4);

	TCIF_TEXT                = 16_0001;
	TCIF_IMAGE               = 16_0002;
	TCIF_RTLREADING          = 16_0004;
	TCIF_PARAM               = 16_0008;

	TCIF_STATE              =  16_0010;


	TCIS_BUTTONPRESSED       = 16_0001;


	TCIS_HIGHLIGHTED        =  16_0002;


	TCM_GETITEMA             = (TCM_FIRST + 5);
	TCM_GETITEMW             = (TCM_FIRST + 60);


	TCM_GETITEM              = TCM_GETITEMA;


	TCM_SETITEMA             = (TCM_FIRST + 6);
	TCM_SETITEMW             = (TCM_FIRST + 61);


	TCM_SETITEM              = TCM_SETITEMA;


	TCM_INSERTITEMA          = (TCM_FIRST + 7);
	TCM_INSERTITEMW          = (TCM_FIRST + 62);


	TCM_INSERTITEM           = TCM_INSERTITEMA;



	TCM_DELETEITEM           = (TCM_FIRST + 8);
	TCM_DELETEALLITEMS       = (TCM_FIRST + 9);
	TCM_GETITEMRECT          = (TCM_FIRST + 10);
	TCM_GETCURSEL            = (TCM_FIRST + 11);
	TCM_SETCURSEL            = (TCM_FIRST + 12);

	TCHT_NOWHERE             = 16_0001;
	TCHT_ONITEMICON          = 16_0002;
	TCHT_ONITEMLABEL         = 16_0004;
	TCHT_ONITEM              = Or(TCHT_ONITEMICON, TCHT_ONITEMLABEL);


	TCM_HITTEST              = (TCM_FIRST + 13);
	TCM_SETITEMEXTRA         = (TCM_FIRST + 14);
	TCM_ADJUSTRECT           = (TCM_FIRST + 40);
	TCM_SETITEMSIZE          = (TCM_FIRST + 41);
	TCM_REMOVEIMAGE          = (TCM_FIRST + 42);
	TCM_SETPADDING           = (TCM_FIRST + 43);
	TCM_GETROWCOUNT          = (TCM_FIRST + 44);
	TCM_GETTOOLTIPS          = (TCM_FIRST + 45);
	TCM_SETTOOLTIPS          = (TCM_FIRST + 46);
	TCM_GETCURFOCUS          = (TCM_FIRST + 47);
	TCM_SETCURFOCUS          = (TCM_FIRST + 48);
	TCM_SETMINTABWIDTH       = (TCM_FIRST + 49);
	TCM_DESELECTALL          = (TCM_FIRST + 50);
	TCM_HIGHLIGHTITEM        = (TCM_FIRST + 51);
	TCM_SETEXTENDEDSTYLE     = (TCM_FIRST + 52);
	TCM_GETEXTENDEDSTYLE     = (TCM_FIRST + 53);
	TCM_SETUNICODEFORMAT      = CCM_SETUNICODEFORMAT;
	TCM_GETUNICODEFORMAT     =  CCM_GETUNICODEFORMAT;

	TCN_FIRST								= -550;

	TCN_KEYDOWN             = (TCN_FIRST - 0);
	TCN_SELCHANGE           = (TCN_FIRST - 1);
	TCN_SELCHANGING         = (TCN_FIRST - 2);
	TCN_GETOBJECT           = (TCN_FIRST - 3);


(* structures *)

TYPE

	LPTCITEM = UNTRACED REF TCITEM;
	TCITEM = RECORD
		mask: 				UINT;
		dwState: 			DWORD;
		dwStateMask:	DWORD;
		pszText: 			LPSTR; (* posible Unicode *)
		cchTextMax: 	int;
		iImage: 			int;
		lParam: 			LPARAM;
	END;

	LPTCITEMHEADER = UNTRACED REF TCITEMHEADER;
	TCITEMHEADER = RECORD
		mask:					UINT;
		lpReserved1:	UINT ;
		lpReserved2:	UINT ;
		pszText:			LPSTR ; (* posible Unicode *)
		cchTextMax:		int ;
		iImage:				int ;
	END;

	LPTCHITTESTINFO =  UNTRACED REF TCHITTESTINFO;
	TCHITTESTINFO = RECORD
		pt:			POINT ;
		flags:	UINT ;
	END;

	NMTCKEYDOWN = RECORD
		hdr:		NMHDR ;
		wVKey:	WORD ;
		flags:	UINT ;
	END;



(* functions and macros *)


PROCEDURE InsertItem(hwnd: HWND; iItem: INTEGER; READONLY pitem: LPTCITEM): INTEGER;
PROCEDURE GetCurSel(hwnd: HWND): INTEGER;
PROCEDURE SetCurSel(hwnd: HWND; i: INTEGER): INTEGER;
PROCEDURE GetImageList(hwnd: WinDef.HWND): WinImageList.HIMAGELIST;
PROCEDURE SetImageList(hwnd: WinDef.HWND; himl: WinImageList.HIMAGELIST): WinImageList.HIMAGELIST;
PROCEDURE GetItemCount(hwnd: WinDef.HWND): INTEGER;
PROCEDURE GetItem(hwnd: WinDef.HWND; iItem: INTEGER; pitem: LPTCITEM): BOOLEAN;
PROCEDURE SetItem(hwnd: WinDef.HWND; iItem: INTEGER; pitem: LPTCITEM): BOOLEAN;
PROCEDURE DeleteItem(hwnd: WinDef.HWND; iItem: INTEGER): BOOLEAN;
PROCEDURE DeleteAllItems(hwnd: WinDef.HWND): BOOLEAN;
PROCEDURE GetItemRect(hwnd: WinDef.HWND; i: INTEGER; prc: WinDef.LPRECT): BOOLEAN;
PROCEDURE HitTest(hwnd: WinDef.HWND; pinfo: LPTCHITTESTINFO): INTEGER;
PROCEDURE SetItemExtra(hwnd: WinDef.HWND; cb: INTEGER): BOOLEAN;  
PROCEDURE AdjustRect(hwnd: WinDef.HWND; bLarger: BOOLEAN; prc: WinDef.LPRECT): INTEGER;
PROCEDURE SetItemSize(hwnd: WinDef.HWND; x, y: WinDef.WORD): WinDef.DWORD;
PROCEDURE RemoveImage(hwnd: WinDef.HWND; i: INTEGER);
PROCEDURE SetPadding(hwnd: WinDef.HWND; x, y: WinDef.WORD);
PROCEDURE GetRowCount(hwnd: WinDef.HWND): INTEGER;
PROCEDURE GetToolTips(hwnd: WinDef.HWND): WinDef.HWND;
PROCEDURE SetToolTips(hwnd: WinDef.HWND; hwndTT: WinDef.HWND);
PROCEDURE GetCurFocus(hwnd: WinDef.HWND): INTEGER;
PROCEDURE SetCurFocus(hwnd: WinDef.HWND; i: INTEGER);
PROCEDURE SetMinTabWidth(hwnd: WinDef.HWND; x: INTEGER): INTEGER;
PROCEDURE DeselectAll(hwnd: WinDef.HWND; fExcludeFocus: BOOLEAN);
PROCEDURE HighlightItem(hwnd: WinDef.HWND; i: INTEGER; fHighlight: BOOLEAN): BOOLEAN;  
PROCEDURE SetUnicodeFormat(hwnd: WinDef.HWND; fUnicode: BOOLEAN): BOOLEAN;  
PROCEDURE GetUnicodeFormat(hwnd: WinDef.HWND): BOOLEAN;  
PROCEDURE SetExtendedStyle(hwnd: WinDef.HWND; dw: WinDef.DWORD): WinDef.DWORD;  
PROCEDURE GetExtendedStyle(hwnd: WinDef.HWND): WinDef.DWORD;  

END WinTabCon.
