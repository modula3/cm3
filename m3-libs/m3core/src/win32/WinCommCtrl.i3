INTERFACE WinCommCtrl;

(* 
	Based on commctrl.h version 1.2
	Copyright 1991-1998, Microsoft Corp. All rights reserved.
	Copyright Darko Volaric 2002 darko@peter.com.au
*)


FROM WinUser IMPORT NMHDR;
FROM WinBaseTypes IMPORT DWORD;
FROM WinDef IMPORT HDC;
FROM WinDef IMPORT RECT;
FROM WinBaseTypes IMPORT UINT;
FROM WinDef IMPORT LPARAM;
FROM Word IMPORT Or;
FROM WinDef IMPORT LONG, POINT, COLORREF;



(* constants *)

CONST
	NM_OUTOFMEMORY          = (NM_FIRST-1);
	NM_CLICK                = (NM_FIRST-2);
	NM_DBLCLK               = (NM_FIRST-3);
	NM_RETURN               = (NM_FIRST-4);
	NM_RCLICK               = (NM_FIRST-5);
	NM_RDBLCLK              = (NM_FIRST-6);
	NM_SETFOCUS             = (NM_FIRST-7);
	NM_KILLFOCUS            = (NM_FIRST-8);
	NM_CUSTOMDRAW           = (NM_FIRST-12);
	NM_HOVER                = (NM_FIRST-13);
	NM_NCHITTEST            = (NM_FIRST-14);
	NM_KEYDOWN              = (NM_FIRST-15);
	NM_RELEASEDCAPTURE      = (NM_FIRST-16);
	NM_SETCURSOR            = (NM_FIRST-17);
	NM_CHAR                 = (NM_FIRST-18);


	ICC_LISTVIEW_CLASSES = 16_00000001;
	ICC_TREEVIEW_CLASSES = 16_00000002;
	ICC_BAR_CLASSES      = 16_00000004;
	ICC_TAB_CLASSES      = 16_00000008;
	ICC_UPDOWN_CLASS     = 16_00000010;
	ICC_PROGRESS_CLASS   = 16_00000020;
	ICC_HOTKEY_CLASS     = 16_00000040;
	ICC_ANIMATE_CLASS    = 16_00000080;
	ICC_WIN95_CLASSES    = 16_000000FF;
	ICC_DATE_CLASSES     = 16_00000100;
	ICC_USEREX_CLASSES   = 16_00000200;
	ICC_COOL_CLASSES     = 16_00000400;
	ICC_INTERNET_CLASSES = 16_00000800;
	ICC_PAGESCROLLER_CLASS = 16_00001000;
	ICC_NATIVEFNTCTL_CLASS = 16_00002000;

	ODT_HEADER              = 100;
	ODT_TAB                 = 101;
	ODT_LISTVIEW            = 102;


	LVM_FIRST               = 16_1000 ;
	TV_FIRST                = 16_1100   ;
	HDM_FIRST               = 16_1200 ;
	TCM_FIRST               = 16_1300;

	PGM_FIRST               = 16_1400;
	CCM_FIRST               = 16_2000;


	CCM_SETBKCOLOR          = (CCM_FIRST + 1);

	CCM_SETCOLORSCHEME      = (CCM_FIRST + 2);
	CCM_GETCOLORSCHEME      = (CCM_FIRST + 3);
	CCM_GETDROPTARGET       = (CCM_FIRST + 4);
	CCM_SETUNICODEFORMAT    = (CCM_FIRST + 5);
	CCM_GETUNICODEFORMAT    = (CCM_FIRST + 6);



	INFOTIPSIZE = 1024;



	NM_FIRST                =  0;
	NM_LAST                 = -99;

	LVN_FIRST               = -100;
	LVN_LAST                = -199;

	HDN_FIRST               = -300;
	HDN_LAST                = -399;

	TVN_FIRST               = -400;
	TVN_LAST                = -499;

	TTN_FIRST               = -520;
	TTN_LAST                = -549;

	TCN_FIRST               = -550;
	TCN_LAST                = -580;

	CDN_FIRST               = -601;
	CDN_LAST                = -699;

	TBN_FIRST               = -700;
	TBN_LAST                = -720;

	UDN_FIRST               = -721;
	UDN_LAST                = -740;
	MCN_FIRST               = -750;
	MCN_LAST                = -759;

	DTN_FIRST               = -760;
	DTN_LAST                = -799;

	CBEN_FIRST              = -800;
	CBEN_LAST               = -830;

	RBN_FIRST               = -831;
	RBN_LAST                = -859;

	IPN_FIRST               = -860;
	IPN_LAST                = -879;

	SBN_FIRST               = -880;
	SBN_LAST                = -899;

	PGN_FIRST               = -900;
	PGN_LAST                = -950;


	MSGF_COMMCTRL_BEGINDRAG     = 16_4200;
	MSGF_COMMCTRL_SIZEHEADER    = 16_4201;
	MSGF_COMMCTRL_DRAGSELECT    = 16_4202;
	MSGF_COMMCTRL_TOOLBARCUST   = 16_4203;

	CDRF_DODEFAULT          = 16_00000000;
	CDRF_NEWFONT            = 16_00000002;
	CDRF_SKIPDEFAULT        = 16_00000004;


	CDRF_NOTIFYPOSTPAINT    = 16_00000010;
	CDRF_NOTIFYITEMDRAW     = 16_00000020;
	CDRF_NOTIFYSUBITEMDRAW  = 16_00000020 ;
	CDRF_NOTIFYPOSTERASE    = 16_00000040;

	CDDS_PREPAINT           = 16_00000001;
	CDDS_POSTPAINT          = 16_00000002;
	CDDS_PREERASE           = 16_00000003;
	CDDS_POSTERASE          = 16_00000004;
	CDDS_ITEM               = 16_00010000;
	CDDS_ITEMPREPAINT       = Or(CDDS_ITEM, CDDS_PREPAINT);
	CDDS_ITEMPOSTPAINT      = Or(CDDS_ITEM, CDDS_POSTPAINT);
	CDDS_ITEMPREERASE       = Or(CDDS_ITEM, CDDS_PREERASE);
	CDDS_ITEMPOSTERASE      = Or(CDDS_ITEM, CDDS_POSTERASE);
	CDDS_SUBITEM            = 16_00020000;


	CDIS_SELECTED       = 16_0001;
	CDIS_GRAYED         = 16_0002;
	CDIS_DISABLED       = 16_0004;
	CDIS_CHECKED        = 16_0008;
	CDIS_FOCUS          = 16_0010;
	CDIS_DEFAULT        = 16_0020;
	CDIS_HOT            = 16_0040;
	CDIS_MARKED         = 16_0080;
	CDIS_INDETERMINATE  = 16_0100;



(* structures *)

TYPE
	HRESULT = LONG ;

	LPNMCUSTOMDRAW = UNTRACED REF NMCUSTOMDRAW;
	NMCUSTOMDRAW = RECORD
		hdr: NMHDR;
		dwDrawStage: DWORD;
		hdc: HDC;
		rc: RECT;
		dwItemSpec: DWORD;
		uItemState: UINT;
		lItemlParam: LPARAM;
	END;

	LPNMMOUSE = UNTRACED REF NMMOUSE;
	NMMOUSE = RECORD
		hdr: NMHDR;
		dwItemSpec: DWORD;
		dwItemData: DWORD;
		pt: POINT;
		dwHitInfo: DWORD;
	END;

	LPNMOBJECTNOTIFY = UNTRACED REF NMOBJECTNOTIFY;
	NMOBJECTNOTIFY = RECORD
		hdr: NMHDR;
		iItem: INTEGER;
		piid: ADDRESS;
		pObject: ADDRESS;
		hResult: HRESULT;
		dwFlags: DWORD;
	END;

	LPNMKEY = UNTRACED REF NMKEY;
	NMKEY = RECORD
		hdr: NMHDR;
		nVKey: UINT;
		uFlags: UINT;
	END;

	LPNMCHAR = UNTRACED REF NMCHAR;
	NMCHAR = RECORD
		hdr: NMHDR;
		ch: UINT;
		dwItemPrev: DWORD;
		dwItemNext: DWORD;
	END;

	LPINITCOMMONCONTROLSEX = UNTRACED REF INITCOMMONCONTROLSEX;
	INITCOMMONCONTROLSEX = RECORD
		dwSize: DWORD;
		dwICC: DWORD;
	END;

	LPCOLORSCHEME = UNTRACED REF COLORSCHEME;
	COLORSCHEME = RECORD
		dwSize: DWORD;
		clrBtnHighlight: COLORREF;
		clrBtnShadow: COLORREF;
	END;

	LPNMTTCUSTOMDRAW = UNTRACED REF NMTTCUSTOMDRAW;
	NMTTCUSTOMDRAW = RECORD
		nmcd: NMCUSTOMDRAW;
		uDrawFlags: UINT;
	END;




(* functions *)


<*EXTERNAL InitCommonControls:WINAPI*>
PROCEDURE InitCommonControls();

<*EXTERNAL InitCommonControlsEx:WINAPI*>
PROCEDURE InitCommonControlsEx(p: LPINITCOMMONCONTROLSEX);


END WinCommCtrl.
