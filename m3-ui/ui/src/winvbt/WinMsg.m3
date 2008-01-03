(* Copyright (C) 1996-2000, Critical Mass, Inc.   All rights reserved. *)
(* See file COPYRIGHT-CMASS for details. *)

MODULE WinMsg;

IMPORT WinUser AS WU;

PROCEDURE ToText (msg: INTEGER): TEXT =
  VAR
    lo   : CARDINAL := 0;
    hi   : CARDINAL := NUMBER (Desc);
    mid  : CARDINAL; 
  BEGIN
    WHILE (lo < hi) DO
      mid := (lo + hi) DIV 2;
      IF (msg < Desc[mid].msg)
        THEN hi := mid;
        ELSE lo := mid + 1;
      END;
    END;
    IF (lo > 0) THEN DEC (lo) END;
    IF (Desc[lo].msg = msg) THEN RETURN Desc[lo].txt; END;
    RETURN NIL;
  END ToText;

TYPE
  X = RECORD msg: INTEGER;  txt: TEXT; END;

CONST
  Desc = ARRAY OF X {
    X{ WU.WM_NULL,                        "WM_NULL" },
    X{ WU.WM_CREATE,                      "WM_CREATE" },
    X{ WU.WM_DESTROY,                     "WM_DESTROY" },
    X{ WU.WM_MOVE,                        "WM_MOVE" },
    X{ WU.WM_SIZE,                        "WM_SIZE" },
    X{ WU.WM_ACTIVATE,                    "WM_ACTIVATE" },
    X{ WU.WM_SETFOCUS,                    "WM_SETFOCUS" },
    X{ WU.WM_KILLFOCUS,                   "WM_KILLFOCUS" },
    X{ WU.WM_ENABLE,                      "WM_ENABLE" },
    X{ WU.WM_SETREDRAW,                   "WM_SETREDRAW" },
    X{ WU.WM_SETTEXT,                     "WM_SETTEXT" },
    X{ WU.WM_GETTEXT,                     "WM_GETTEXT" },
    X{ WU.WM_GETTEXTLENGTH,               "WM_GETTEXTLENGTH" },
    X{ WU.WM_PAINT,                       "WM_PAINT" },
    X{ WU.WM_CLOSE,                       "WM_CLOSE" },
    X{ WU.WM_QUERYENDSESSION,             "WM_QUERYENDSESSION" },
    X{ WU.WM_QUIT,                        "WM_QUIT" },
    X{ WU.WM_QUERYOPEN,                   "WM_QUERYOPEN" },
    X{ WU.WM_ERASEBKGND,                  "WM_ERASEBKGND" },
    X{ WU.WM_SYSCOLORCHANGE,              "WM_SYSCOLORCHANGE" },
    X{ WU.WM_ENDSESSION,                  "WM_ENDSESSION" },
    X{ WU.WM_SHOWWINDOW,                  "WM_SHOWWINDOW" },
    X{ WU.WM_WININICHANGE,                "WM_WININICHANGE" },
    X{ WU.WM_DEVMODECHANGE,               "WM_DEVMODECHANGE" },
    X{ WU.WM_ACTIVATEAPP,                 "WM_ACTIVATEAPP" },
    X{ WU.WM_FONTCHANGE,                  "WM_FONTCHANGE" },
    X{ WU.WM_TIMECHANGE,                  "WM_TIMECHANGE" },
    X{ WU.WM_CANCELMODE,                  "WM_CANCELMODE" },
    X{ WU.WM_SETCURSOR,                   "WM_SETCURSOR" },
    X{ WU.WM_MOUSEACTIVATE,               "WM_MOUSEACTIVATE" },
    X{ WU.WM_CHILDACTIVATE,               "WM_CHILDACTIVATE" },
    X{ WU.WM_QUEUESYNC,                   "WM_QUEUESYNC" },
    X{ WU.WM_GETMINMAXINFO,               "WM_GETMINMAXINFO" },
    X{ WU.WM_PAINTICON,                   "WM_PAINTICON" },
    X{ WU.WM_ICONERASEBKGND,              "WM_ICONERASEBKGND" },
    X{ WU.WM_NEXTDLGCTL,                  "WM_NEXTDLGCTL" },
    X{ WU.WM_SPOOLERSTATUS,               "WM_SPOOLERSTATUS" },
    X{ WU.WM_DRAWITEM,                    "WM_DRAWITEM" },
    X{ WU.WM_MEASUREITEM,                 "WM_MEASUREITEM" },
    X{ WU.WM_DELETEITEM,                  "WM_DELETEITEM" },
    X{ WU.WM_VKEYTOITEM,                  "WM_VKEYTOITEM" },
    X{ WU.WM_CHARTOITEM,                  "WM_CHARTOITEM" },
    X{ WU.WM_SETFONT,                     "WM_SETFONT" },
    X{ WU.WM_GETFONT,                     "WM_GETFONT" },
    X{ WU.WM_SETHOTKEY,                   "WM_SETHOTKEY" },
    X{ WU.WM_GETHOTKEY,                   "WM_GETHOTKEY" },
    X{ WU.WM_QUERYDRAGICON,               "WM_QUERYDRAGICON" },
    X{ WU.WM_COMPAREITEM,                 "WM_COMPAREITEM" },
    X{ WU.WM_FULLSCREEN,                  "WM_FULLSCREEN" },
    X{ WU.WM_COMPACTING,                  "WM_COMPACTING" },
    X{ WU.WM_OTHERWINDOWCREATED,          "WM_OTHERWINDOWCREATED" },
    X{ WU.WM_OTHERWINDOWDESTROYED,        "WM_OTHERWINDOWDESTROYED" },
    X{ WU.WM_COMMNOTIFY,                  "WM_COMMNOTIFY" },
    X{ WU.WM_HOTKEYEVENT,                 "WM_HOTKEYEVENT" },
    X{ WU.WM_WINDOWPOSCHANGING,           "WM_WINDOWPOSCHANGING" },
    X{ WU.WM_WINDOWPOSCHANGED,            "WM_WINDOWPOSCHANGED" },
    X{ WU.WM_POWER,                       "WM_POWER" },
    X{ WU.WM_COPYDATA,                    "WM_COPYDATA" },
    X{ WU.WM_NCCREATE,                    "WM_NCCREATE" },
    X{ WU.WM_NCDESTROY,                   "WM_NCDESTROY" },
    X{ WU.WM_NCCALCSIZE,                  "WM_NCCALCSIZE" },
    X{ WU.WM_NCHITTEST,                   "WM_NCHITTEST" },
    X{ WU.WM_NCPAINT,                     "WM_NCPAINT" },
    X{ WU.WM_NCACTIVATE,                  "WM_NCACTIVATE" },
    X{ WU.WM_GETDLGCODE,                  "WM_GETDLGCODE" },
    X{ WU.WM_NCMOUSEMOVE,                 "WM_NCMOUSEMOVE" },
    X{ WU.WM_NCLBUTTONDOWN,               "WM_NCLBUTTONDOWN" },
    X{ WU.WM_NCLBUTTONUP,                 "WM_NCLBUTTONUP" },
    X{ WU.WM_NCLBUTTONDBLCLK,             "WM_NCLBUTTONDBLCLK" },
    X{ WU.WM_NCRBUTTONDOWN,               "WM_NCRBUTTONDOWN" },
    X{ WU.WM_NCRBUTTONUP,                 "WM_NCRBUTTONUP" },
    X{ WU.WM_NCRBUTTONDBLCLK,             "WM_NCRBUTTONDBLCLK" },
    X{ WU.WM_NCMBUTTONDOWN,               "WM_NCMBUTTONDOWN" },
    X{ WU.WM_NCMBUTTONUP,                 "WM_NCMBUTTONUP" },
    X{ WU.WM_NCMBUTTONDBLCLK,             "WM_NCMBUTTONDBLCLK" },
    X{ WU.WM_KEYDOWN,                     "WM_KEYDOWN (aka WM_KEYFIRST)" },
    X{ WU.WM_KEYUP,                       "WM_KEYUP" },
    X{ WU.WM_CHAR,                        "WM_CHAR" },
    X{ WU.WM_DEADCHAR,                    "WM_DEADCHAR" },
    X{ WU.WM_SYSKEYDOWN,                  "WM_SYSKEYDOWN" },
    X{ WU.WM_SYSKEYUP,                    "WM_SYSKEYUP" },
    X{ WU.WM_SYSCHAR,                     "WM_SYSCHAR" },
    X{ WU.WM_SYSDEADCHAR,                 "WM_SYSDEADCHAR" },
    X{ WU.WM_KEYLAST,                     "WM_KEYLAST" },
    X{ WU.WM_INITDIALOG,                  "WM_INITDIALOG" },
    X{ WU.WM_COMMAND,                     "WM_COMMAND" },
    X{ WU.WM_SYSCOMMAND,                  "WM_SYSCOMMAND" },
    X{ WU.WM_TIMER,                       "WM_TIMER" },
    X{ WU.WM_HSCROLL,                     "WM_HSCROLL" },
    X{ WU.WM_VSCROLL,                     "WM_VSCROLL" },
    X{ WU.WM_INITMENU,                    "WM_INITMENU" },
    X{ WU.WM_INITMENUPOPUP,               "WM_INITMENUPOPUP" },
    X{ WU.WM_MENUSELECT,                  "WM_MENUSELECT" },
    X{ WU.WM_MENUCHAR,                    "WM_MENUCHAR" },
    X{ WU.WM_ENTERIDLE,                   "WM_ENTERIDLE" },
    X{ WU.WM_CTLCOLORMSGBOX,              "WM_CTLCOLORMSGBOX" },
    X{ WU.WM_CTLCOLOREDIT,                "WM_CTLCOLOREDIT" },
    X{ WU.WM_CTLCOLORLISTBOX,             "WM_CTLCOLORLISTBOX" },
    X{ WU.WM_CTLCOLORBTN,                 "WM_CTLCOLORBTN" },
    X{ WU.WM_CTLCOLORDLG,                 "WM_CTLCOLORDLG" },
    X{ WU.WM_CTLCOLORSCROLLBAR,           "WM_CTLCOLORSCROLLBAR" },
    X{ WU.WM_CTLCOLORSTATIC,              "WM_CTLCOLORSTATIC" },
    X{ WU.WM_MOUSEMOVE,                   "WM_MOUSEMOVE (aka WM_MOUSEFIRST)" },
    X{ WU.WM_LBUTTONDOWN,                 "WM_LBUTTONDOWN" },
    X{ WU.WM_LBUTTONUP,                   "WM_LBUTTONUP" },
    X{ WU.WM_LBUTTONDBLCLK,               "WM_LBUTTONDBLCLK" },
    X{ WU.WM_RBUTTONDOWN,                 "WM_RBUTTONDOWN" },
    X{ WU.WM_RBUTTONUP,                   "WM_RBUTTONUP" },
    X{ WU.WM_RBUTTONDBLCLK,               "WM_RBUTTONDBLCLK" },
    X{ WU.WM_MBUTTONDOWN,                 "WM_MBUTTONDOWN" },
    X{ WU.WM_MBUTTONUP,                   "WM_MBUTTONUP" },
    X{ WU.WM_MBUTTONDBLCLK,               "WM_MBUTTONDBLCLK (aka MOUSELAST)" },
    X{ WU.WM_PARENTNOTIFY,                "WM_PARENTNOTIFY" },
    X{ WU.WM_ENTERMENULOOP,               "WM_ENTERMENULOOP" },
    X{ WU.WM_EXITMENULOOP,                "WM_EXITMENULOOP" },
(*** Win95 only...
    X{ WU.WM_NEXTMENU,                    "WM_NEXTMENU" },
    X{ WU.WM_SIZING,                      "WM_SIZING" },
    X{ WU.WM_CAPTURECHANGED,              "WM_CAPTURECHANGED" },
    X{ WU.WM_MOVING,                      "WM_MOVING" },
    X{ WU.WM_POWERBROADCAST,              "WM_POWERBROADCAST" },
    X{ WU.WM_DEVICECHANGE,                "WM_DEVICECHANGE" },
***)
    X{ WU.WM_MDICREATE,                   "WM_MDICREATE" },
    X{ WU.WM_MDIDESTROY,                  "WM_MDIDESTROY" },
    X{ WU.WM_MDIACTIVATE,                 "WM_MDIACTIVATE" },
    X{ WU.WM_MDIRESTORE,                  "WM_MDIRESTORE" },
    X{ WU.WM_MDINEXT,                     "WM_MDINEXT" },
    X{ WU.WM_MDIMAXIMIZE,                 "WM_MDIMAXIMIZE" },
    X{ WU.WM_MDITILE,                     "WM_MDITILE" },
    X{ WU.WM_MDICASCADE,                  "WM_MDICASCADE" },
    X{ WU.WM_MDIICONARRANGE,              "WM_MDIICONARRANGE" },
    X{ WU.WM_MDIGETACTIVE,                "WM_MDIGETACTIVE" },
    X{ WU.WM_MDISETMENU,                  "WM_MDISETMENU" },
    X{ WU.WM_ENTERSIZEMOVE_UNDOCUMENTED,  "WM_ENTERSIZEMOVE_UNDOCUMENTED" },
    X{ WU.WM_EXITSIZEMOVE_UNDOCUMENTED,   "WM_EXITSIZEMOVE_UNDOCUMENTED" },
    X{ WU.WM_DROPFILES,                   "WM_DROPFILES" },
    X{ WU.WM_MDIREFRESHMENU,              "WM_MDIREFRESHMENU" },
    X{ WU.WM_CUT,                         "WM_CUT" },
    X{ WU.WM_COPY,                        "WM_COPY" },
    X{ WU.WM_PASTE,                       "WM_PASTE" },
    X{ WU.WM_CLEAR,                       "WM_CLEAR" },
    X{ WU.WM_UNDO,                        "WM_UNDO" },
    X{ WU.WM_RENDERFORMAT,                "WM_RENDERFORMAT" },
    X{ WU.WM_RENDERALLFORMATS,            "WM_RENDERALLFORMATS" },
    X{ WU.WM_DESTROYCLIPBOARD,            "WM_DESTROYCLIPBOARD" },
    X{ WU.WM_DRAWCLIPBOARD,               "WM_DRAWCLIPBOARD" },
    X{ WU.WM_PAINTCLIPBOARD,              "WM_PAINTCLIPBOARD" },
    X{ WU.WM_VSCROLLCLIPBOARD,            "WM_VSCROLLCLIPBOARD" },
    X{ WU.WM_SIZECLIPBOARD,               "WM_SIZECLIPBOARD" },
    X{ WU.WM_ASKCBFORMATNAME,             "WM_ASKCBFORMATNAME" },
    X{ WU.WM_CHANGECBCHAIN,               "WM_CHANGECBCHAIN" },
    X{ WU.WM_HSCROLLCLIPBOARD,            "WM_HSCROLLCLIPBOARD" },
    X{ WU.WM_QUERYNEWPALETTE,             "WM_QUERYNEWPALETTE" },
    X{ WU.WM_PALETTEISCHANGING,           "WM_PALETTEISCHANGING" },
    X{ WU.WM_PALETTECHANGED,              "WM_PALETTECHANGED" },
    X{ WU.WM_HOTKEY,                      "WM_HOTKEY" },
    X{ WU.WM_PENWINFIRST,                 "WM_PENWINFIRST" },
    X{ WU.WM_PENWINLAST,                  "WM_PENWINLAST" },
    X{ WU.WM_MM_RESERVED_FIRST,           "WM_MM_RESERVED_FIRST" },
    X{ WU.WM_MM_RESERVED_LAST,            "WM_MM_RESERVED_LAST" },
    X{ WU.WM_USER,                        "WM_USER" },
    X{ CREATE_OFFSCREEN_VBT,              "CREATE_OFFSCREEN_VBT" },
    X{ RESHAPE_VBT,                       "RESHAPE_VBT" },
    X{ DELETE_VBT,                        "DELETE_VBT" },
    X{ SYNC_VBT,                          "SYNC_VBT" },
    X{ FORGE_VBT,                         "FORGE_VBT" },
    X{ ICONIZE_VBT,                       "ICONIZE_VBT" },
    X{ OVERLAP_VBT,                       "OVERLAP_VBT" },
    X{ RETITLE_VBT,                       "RETITLE_VBT" },
    X{ PAINTBATCH_VBT,                    "PAINTBATCH_VBT" }
  };

BEGIN
  (* verify that we have a sorted list of tags. *)
  FOR i := 1 TO LAST (Desc) DO
    <*ASSERT Desc[i-1].msg < Desc[i].msg*>
  END;
END WinMsg.
