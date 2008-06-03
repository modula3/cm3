INTERFACE WinTabCon;

(* 
    Based on commctrl.h version 1.2
    Copyright 1991-1998, Microsoft Corp. All rights reserved.
    Copyright Darko Volaric 2002 darko@peter.com.au
*)

FROM Word IMPORT Or;
FROM WinNT IMPORT PSTR;
FROM WinDef IMPORT UINT32, INT32, UINT16, LPARAM, HWND, POINT, PRECT;
FROM WinUser IMPORT NMHDR;
FROM WinCommCtrl IMPORT TCM_FIRST, CCM_SETUNICODEFORMAT, CCM_GETUNICODEFORMAT;
FROM WinImageList IMPORT HIMAGELIST;

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

    TCN_FIRST                                = -550;

    TCN_KEYDOWN             = (TCN_FIRST - 0);
    TCN_SELCHANGE           = (TCN_FIRST - 1);
    TCN_SELCHANGING         = (TCN_FIRST - 2);
    TCN_GETOBJECT           = (TCN_FIRST - 3);


(* structures *)

TYPE

    PTCITEM = UNTRACED REF TCITEM;
    LPTCITEM = PTCITEM; (* compat *)
    TCITEM = RECORD
        mask: UINT32;
        dwState: UINT32;
        dwStateMask: UINT32;
        pszText: PSTR; (* posible Unicode *)
        cchTextMax: INT32;
        iImage: INT32;
        lParam: LPARAM;
    END;

    PTCITEMHEADER = UNTRACED REF TCITEMHEADER;
    LPTCITEMHEADER = PTCITEMHEADER; (* compat *)
    TCITEMHEADER = RECORD
        mask: UINT32;
        lpReserved1: UINT32;
        lpReserved2: UINT32;
        pszText: PSTR; (* posible Unicode *)
        cchTextMax: INT32;
        iImage: INT32;
    END;

    PTCHITTESTINFO = UNTRACED REF TCHITTESTINFO;
    LPTCHITTESTINFO = PTCHITTESTINFO; (* compat *)
    TCHITTESTINFO = RECORD
        pt: POINT;
        flags: UINT32;
    END;

    NMTCKEYDOWN = RECORD
        hdr: NMHDR;
        wVKey: UINT16;
        padding:UINT16;
        flags: UINT32;
    END;


(* functions and macros *)


PROCEDURE InsertItem(hwnd: HWND; iItem: INTEGER; READONLY pitem: PTCITEM): INTEGER;
PROCEDURE GetCurSel(hwnd: HWND): INTEGER;
PROCEDURE SetCurSel(hwnd: HWND; i: INTEGER): INTEGER;
PROCEDURE GetImageList(hwnd: HWND): HIMAGELIST;
PROCEDURE SetImageList(hwnd: HWND; himl: HIMAGELIST): HIMAGELIST;
PROCEDURE GetItemCount(hwnd: HWND): INTEGER;
PROCEDURE GetItem(hwnd: HWND; iItem: INTEGER; pitem: PTCITEM): BOOLEAN;
PROCEDURE SetItem(hwnd: HWND; iItem: INTEGER; pitem: PTCITEM): BOOLEAN;
PROCEDURE DeleteItem(hwnd: HWND; iItem: INTEGER): BOOLEAN;
PROCEDURE DeleteAllItems(hwnd: HWND): BOOLEAN;
PROCEDURE GetItemRect(hwnd: HWND; i: INTEGER; prc: PRECT): BOOLEAN;
PROCEDURE HitTest(hwnd: HWND; pinfo: PTCHITTESTINFO): INTEGER;
PROCEDURE SetItemExtra(hwnd: HWND; cb: INTEGER): BOOLEAN;  
PROCEDURE AdjustRect(hwnd: HWND; bLarger: BOOLEAN; prc: PRECT): INTEGER;
PROCEDURE SetItemSize(hwnd: HWND; x, y: UINT16): UINT32;
PROCEDURE RemoveImage(hwnd: HWND; i: INTEGER);
PROCEDURE SetPadding(hwnd: HWND; x, y: UINT16);
PROCEDURE GetRowCount(hwnd: HWND): INTEGER;
PROCEDURE GetToolTips(hwnd: HWND): HWND;
PROCEDURE SetToolTips(hwnd: HWND; hwndTT: HWND);
PROCEDURE GetCurFocus(hwnd: HWND): INTEGER;
PROCEDURE SetCurFocus(hwnd: HWND; i: INTEGER);
PROCEDURE SetMinTabWidth(hwnd: HWND; x: INTEGER): INTEGER;
PROCEDURE DeselectAll(hwnd: HWND; fExcludeFocus: BOOLEAN);
PROCEDURE HighlightItem(hwnd: HWND; i: INTEGER; fHighlight: BOOLEAN): BOOLEAN;  
PROCEDURE SetUnicodeFormat(hwnd: HWND; fUnicode: BOOLEAN): BOOLEAN;  
PROCEDURE GetUnicodeFormat(hwnd: HWND): BOOLEAN;  
PROCEDURE SetExtendedStyle(hwnd: HWND; dw: UINT32): UINT32;  
PROCEDURE GetExtendedStyle(hwnd: HWND): UINT32;  

END WinTabCon.
