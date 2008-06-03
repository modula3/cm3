(* Copyright (C) 1994, Digital Equipment Corporation         *)
(* All rights reserved.                                      *)
(* See the file COPYRIGHT for a full description.            *)
(*                                                           *)
(* by Stephen Harrison                                       *)
(*                                                           *)
(* Last modified on Fri Jun 16 08:03:58 PDT 1995 by kalsow   *)
(*      modified on Thu May 13 21:16:12 PDT 1993 by mjordan  *)
(*      modified on Wed Apr 14 12:43:59 PDT 1993 by steveg   *)
(*      modified on Fri Mar 19 11:03:05 PST 1993 by harrison *)

INTERFACE WinUser;

(* This file corresponds to build version 0003 of "winuser.h"
   See that file for details.

   Procedure declarations, constant definitions and macros for the User
   component. *)

FROM Word IMPORT Or;
FROM WinBaseTypes IMPORT INT16, BOOL, UINT16, UINT32, PUINT32, PUINT16,
  PVOID, UINT8, PUINT8, PINT32, INT32, WCHAR, HANDLE, PSTR, PWSTR, PTSTR,
  PCSTR, PCTSTR, PCWSTR, SIZE_T;
FROM WinDef IMPORT HWND, HDC, WPARAM, LPARAM, LRESULT, HINSTANCE, HMENU,
  POINT, HKL, HDESK, HWINSTA, HICON, HCURSOR, HBRUSH, RECT, ATOM, PHANDLE,
  HACCEL, HBITMAP, PRECT, HRGN, PPOINT, COLORREF, HHOOK;
FROM WinNT IMPORT PSECURITY_INFORMATION, PSECURITY_DESCRIPTOR;
FROM Ctypes IMPORT char;

CONST WINVER = 16_0314;         (* version 3.2 *)

TYPE
  HDWP = HANDLE;

(* MENUTEMPLATEA = VOID; (* No M3 equivalent *)
  MENUTEMPLATEW = VOID; *)

  LPMENUTEMPLATE = PVOID;

  WNDPROC = <*CALLBACK*> PROCEDURE (a1: HWND; a2: UINT32;
                                    a3: WPARAM; a4: LPARAM): LRESULT;

  DLGPROC = <*CALLBACK*> PROCEDURE (a1: HWND; a2: UINT32;
                                    a3: WPARAM; a4: LPARAM): BOOL;

  TIMERPROC = <*CALLBACK*> PROCEDURE (a1: HWND; a2: SIZE_T; a3: UINT32; a4: UINT32);

  GRAYSTRINGPROC = <*CALLBACK*> PROCEDURE (a1: HDC; a2: LPARAM; a3: INT32): BOOL;

  PROPENUMPROC = <*CALLBACK*> PROCEDURE (a1: HWND; a2: PCSTR;
                                         a3: HANDLE): BOOL;

  PROPENUMPROCEX = <*CALLBACK*> PROCEDURE (a1: HWND; a2: PTSTR;
                                           a3: HANDLE; a4: UINT32): BOOL;

  WNDENUMPROC = <*CALLBACK*> PROCEDURE (a1: HWND; a2: LPARAM): BOOL;

  HOOKPROC = <*CALLBACK*> PROCEDURE (code: INT32; wParam: WPARAM;
                                     lParam: LPARAM): LRESULT;

  EDITWORDBREAKPROC = <*CALLBACK*> PROCEDURE (lpch: PSTR; ichCurrent: INT32;
                                              cch: INT32; code: INT32): INT32;

  SENDASYNCPROC = <*CALLBACK*> PROCEDURE (a1: HWND; a2: UINT32;
                                          a3: UINT32; a4: LRESULT);

(*!!!  #define MAKEINTRESOURCE(i) (PTSTR)((UINT32)((UINT16)(i))) *)

(* Predefined Resource Types *)
VAR                             (* CONST *)
  RT_CURSOR      : PTSTR;
  RT_BITMAP      : PTSTR;
  RT_ICON        : PTSTR;
  RT_MENU        : PTSTR;
  RT_DIALOG      : PTSTR;
  RT_STRING      : PTSTR;
  RT_FONTDIR     : PTSTR;
  RT_FONT        : PTSTR;
  RT_ACCELERATOR : PTSTR;
  RT_RCDATA      : PTSTR;
  RT_MESSAGETABLE: PTSTR;

CONST DIFFERENCE = 11;

VAR                             (* CONST *)
  (* NOTE: if any new resource types are introduced above this point, then
     the ** value of DIFFERENCE must be changed.  ** (RT_GROUP_CURSOR -
     RT_CURSOR) must always be equal to DIFFERENCE ** (RT_GROUP_ICON -
     RT_ICON) must always be equal to DIFFERENCE *)
  RT_GROUP_CURSOR: PTSTR;
  (* The value RT_BITMAP+DIFFERENCE (13) is intentionally unused *)
  RT_GROUP_ICON: PTSTR;
  (* The value 15 is unused/obsolete *)
  RT_VERSION   : PTSTR;
  RT_DLGINCLUDE: PTSTR;

TYPE
  wvsprintfA = <*WINAPI*> PROCEDURE (a1: PSTR; a2: PCSTR;
                                     arglist: PVOID): INT32;
  wvsprintfW = <*WINAPI*> PROCEDURE (a1: PWSTR; a2: PCWSTR;
                                     arglist: PVOID): INT32;
  wvsprintf = wvsprintfA;

(* Steve G.  How do you want to handle varargs??? *)
(*
INT32 WINAPI wsprintfA(PSTR, PCSTR, ...);
INT32 WINAPI wsprintfW(PWSTR, PCWSTR, ...);
#ifdef UNICODE
#define wsprintf wsprintfW
#else
#define wsprintf wsprintfA
#endif // !UNICODE
*)

(* Scroll Bar Constants *)
CONST
  SB_HORZ = 0;
  SB_VERT = 1;
  SB_CTL  = 2;
  SB_BOTH = 3;
  SB_MAX  = 3;

(* Scroll Bar Commands *)
  SB_LINEUP        = 0;
  SB_LINELEFT      = 0;
  SB_LINEDOWN      = 1;
  SB_LINERIGHT     = 1;
  SB_PAGEUP        = 2;
  SB_PAGELEFT      = 2;
  SB_PAGEDOWN      = 3;
  SB_PAGERIGHT     = 3;
  SB_THUMBPOSITION = 4;
  SB_THUMBTRACK    = 5;
  SB_TOP           = 6;
  SB_LEFT          = 6;
  SB_BOTTOM        = 7;
  SB_RIGHT         = 7;
  SB_ENDSCROLL     = 8;
  SB_CMD_MAX       = 8;

(* ShowWindow() Commands *)
  SW_HIDE            = 0;
  SW_SHOWNORMAL      = 1;
  SW_NORMAL          = 1;
  SW_SHOWMINIMIZED   = 2;
  SW_SHOWMAXIMIZED   = 3;
  SW_MAXIMIZE        = 3;
  SW_SHOWNOACTIVATE  = 4;
  SW_SHOW            = 5;
  SW_MINIMIZE        = 6;
  SW_SHOWMINNOACTIVE = 7;
  SW_SHOWNA          = 8;
  SW_RESTORE         = 9;
  SW_SHOWDEFAULT     = 10;
  SW_MAX             = 10;

(* Old ShowWindow() Commands *)
  HIDE_WINDOW         = 0;
  SHOW_OPENWINDOW     = 1;
  SHOW_ICONWINDOW     = 2;
  SHOW_FULLSCREEN     = 3;
  SHOW_OPENNOACTIVATE = 4;

(* Identifiers for the WM_SHOWWINDOW message *)
  SW_PARENTCLOSING = 1;
  SW_OTHERZOOM     = 2;
  SW_PARENTOPENING = 3;
  SW_OTHERUNZOOM   = 4;

(* WM_KEYUP/DOWN/CHAR HIWORD(lParam) flags *)
  KF_EXTENDED = 16_0100;
  KF_DLGMODE  = 16_0800;
  KF_MENUMODE = 16_1000;
  KF_ALTDOWN  = 16_2000;
  KF_REPEAT   = 16_4000;
  KF_UP       = 16_8000;

(* Virtual Keys, Standard Set *)
  VK_LBUTTON = 16_01;
  VK_RBUTTON = 16_02;
  VK_CANCEL  = 16_03;
  VK_MBUTTON = 16_04;           (* NOT contiguous with L & RBUTTON *)

  VK_BACK = 16_08;
  VK_TAB  = 16_09;

  VK_CLEAR  = 16_0C;
  VK_RETURN = 16_0D;

  VK_SHIFT   = 16_10;
  VK_CONTROL = 16_11;
  VK_MENU    = 16_12;
  VK_PAUSE   = 16_13;
  VK_CAPITAL = 16_14;

  VK_ESCAPE = 16_1B;

  VK_SPACE    = 16_20;
  VK_PRIOR    = 16_21;
  VK_NEXT     = 16_22;
  VK_END      = 16_23;
  VK_HOME     = 16_24;
  VK_LEFT     = 16_25;
  VK_UP       = 16_26;
  VK_RIGHT    = 16_27;
  VK_DOWN     = 16_28;
  VK_SELECT   = 16_29;
  VK_PRINT    = 16_2A;
  VK_EXECUTE  = 16_2B;
  VK_SNAPSHOT = 16_2C;
  (* #define VK_COPY 16_2C not used by keyboards. *)
  VK_INSERT = 16_2D;
  VK_DELETE = 16_2E;
  VK_HELP   = 16_2F;

  (* VK_0 thru VK_9 are the same as ASCII '0' thru '9' (16_30 - 16_39) *)
  (* VK_A thru VK_Z are the same as ASCII 'A' thru 'Z' (16_41 - 16_5A) *)

  VK_NUMPAD0   = 16_60;
  VK_NUMPAD1   = 16_61;
  VK_NUMPAD2   = 16_62;
  VK_NUMPAD3   = 16_63;
  VK_NUMPAD4   = 16_64;
  VK_NUMPAD5   = 16_65;
  VK_NUMPAD6   = 16_66;
  VK_NUMPAD7   = 16_67;
  VK_NUMPAD8   = 16_68;
  VK_NUMPAD9   = 16_69;
  VK_MULTIPLY  = 16_6A;
  VK_ADD       = 16_6B;
  VK_SEPARATOR = 16_6C;
  VK_SUBTRACT  = 16_6D;
  VK_DECIMAL   = 16_6E;
  VK_DIVIDE    = 16_6F;
  VK_F1        = 16_70;
  VK_F2        = 16_71;
  VK_F3        = 16_72;
  VK_F4        = 16_73;
  VK_F5        = 16_74;
  VK_F6        = 16_75;
  VK_F7        = 16_76;
  VK_F8        = 16_77;
  VK_F9        = 16_78;
  VK_F10       = 16_79;
  VK_F11       = 16_7A;
  VK_F12       = 16_7B;
  VK_F13       = 16_7C;
  VK_F14       = 16_7D;
  VK_F15       = 16_7E;
  VK_F16       = 16_7F;
  VK_F17       = 16_80;
  VK_F18       = 16_81;
  VK_F19       = 16_82;
  VK_F20       = 16_83;
  VK_F21       = 16_84;
  VK_F22       = 16_85;
  VK_F23       = 16_86;
  VK_F24       = 16_87;

  VK_NUMLOCK = 16_90;
  VK_SCROLL  = 16_91;

  (*
   * VK_L* & VK_R* - left and right Alt, Ctrl and Shift virtual keys.
   * Used only as parameters to GetAsyncKeyState() and GetKeyState().
   * No other API or message will distinguish left and right keys in this way.
   *)
  VK_LSHIFT   = 16_A0;
  VK_RSHIFT   = 16_A1;
  VK_LCONTROL = 16_A2;
  VK_RCONTROL = 16_A3;
  VK_LMENU    = 16_A4;
  VK_RMENU    = 16_A5;

  VK_ATTN      = 16_F6;
  VK_CRSEL     = 16_F7;
  VK_EXSEL     = 16_F8;
  VK_EREOF     = 16_F9;
  VK_PLAY      = 16_FA;
  VK_ZOOM      = 16_FB;
  VK_NONAME    = 16_FC;
  VK_PA1       = 16_FD;
  VK_OEM_CLEAR = 16_FE;

(* SetWindowsHook() codes *)
  WH_MIN             = (-1);
  WH_MSGFILTER       = (-1);
  WH_JOURNALRECORD   = 0;
  WH_JOURNALPLAYBACK = 1;
  WH_KEYBOARD        = 2;
  WH_GETMESSAGE      = 3;
  WH_CALLWNDPROC     = 4;
  WH_CBT             = 5;
  WH_SYSMSGFILTER    = 6;
  WH_MOUSE           = 7;
  WH_HARDWARE        = 8;
  WH_DEBUG           = 9;
  WH_SHELL           = 10;
  WH_MAX             = 10;

(* Obsolete hook codes (NO LONGER SUPPORTED) *)
  HC_GETLPLPFN  = (-3);
  HC_LPLPFNNEXT = (-2);
  HC_LPFNNEXT   = (-1);

(* Hook Codes *)
  HC_ACTION      = 0;
  HC_GETNEXT     = 1;
  HC_SKIP        = 2;
  HC_NOREMOVE    = 3;
  HC_NOREM       = HC_NOREMOVE;
  HC_SYSMODALON  = 4;
  HC_SYSMODALOFF = 5;

(* CBT Hook Codes *)
  HCBT_MOVESIZE     = 0;
  HCBT_MINMAX       = 1;
  HCBT_QS           = 2;
  HCBT_CREATEWND    = 3;
  HCBT_DESTROYWND   = 4;
  HCBT_ACTIVATE     = 5;
  HCBT_CLICKSKIPPED = 6;
  HCBT_KEYSKIPPED   = 7;
  HCBT_SYSCOMMAND   = 8;
  HCBT_SETFOCUS     = 9;

(*
 * HCBT_CREATEWND parameters pointed to by lParam
 *)
TYPE
  LPCBT_CREATEWNDA = UNTRACED REF CBT_CREATEWNDA;
  CBT_CREATEWNDA = RECORD
    lpcs           : UNTRACED REF CREATESTRUCTA;
    hwndInsertAfter: HWND;
  END;

  (*
   * HCBT_CREATEWND parameters pointed to by lParam
   *)
  LPCBT_CREATEWNDW = UNTRACED REF CBT_CREATEWNDW;
  CBT_CREATEWNDW = RECORD
                     lpcs           : UNTRACED REF CREATESTRUCTW;
                     hwndInsertAfter: HWND;
  END;

  CBT_CREATEWND = CBT_CREATEWNDA;
  LPCBT_CREATEWND = LPCBT_CREATEWNDA;

  (*
   * HCBT_ACTIVATE structure pointed to by lParam
   *)
  LPCBTACTIVATESTRUCT = UNTRACED REF CBTACTIVATESTRUCT;
  CBTACTIVATESTRUCT = RECORD
                        fMouse    : BOOL;
                        hWndActive: HWND;
  END;

(* WH_MSGFILTER Filter Proc Codes *)
CONST
  MSGF_DIALOGBOX  = 0;
  MSGF_MESSAGEBOX = 1;
  MSGF_MENU       = 2;
  MSGF_MOVE       = 3;
  MSGF_SIZE       = 4;
  MSGF_SCROLLBAR  = 5;
  MSGF_NEXTWINDOW = 6;
  MSGF_MAINLOOP   = 8;
  MSGF_MAX        = 8;
  MSGF_USER       = 4096;

(* Shell support *)
  HSHELL_WINDOWCREATED       = 1;
  HSHELL_WINDOWDESTROYED     = 2;
  HSHELL_ACTIVATESHELLWINDOW = 3;

(* Window Manager Hook Codes *)
  WC_INIT          = 1;
  WC_SWP           = 2;
  WC_DEFWINDOWPROC = 3;
  WC_MINMAX        = 4;
  WC_MOVE          = 5;
  WC_SIZE          = 6;
  WC_DRAWCAPTION   = 7;

(* Message Structure used in Journaling *)
TYPE
  PEVENTMSGMSG = UNTRACED REF EVENTMSG;
  PEVENTMSG = UNTRACED REF EVENTMSG;
  EVENTMSG = RECORD
    message: UINT32;
    paramL : UINT32;
    paramH : UINT32;
    time   : UINT32;
    hwnd   : HWND;
  END;


  (* Message structure used by WH_CALLWNDPROC *)
  PCWPSTRUCT = UNTRACED REF CWPSTRUCT;
  CWPSTRUCT = RECORD
    lParam : LPARAM;
    wParam : WPARAM;
    message: UINT32;
    hwnd   : HWND;
  END;

  (* Structure used by WH_DEBUG *)
  PDEBUGHOOKINFO = UNTRACED REF DEBUGHOOKINFO;
  DEBUGHOOKINFO = RECORD
    idThread: UINT32;
    reserved: LPARAM;
    lParam  : LPARAM;
    wParam  : WPARAM;
    code    : INT32;
  END;

  MOUSEHOOKSTRUCT = RECORD
    pt          : POINT;
    hwnd        : HWND;
    wHitTestCode: UINT32;
    dwExtraInfo : UINT32;
  END;

(*
 * Keyboard Layout API
 *)
CONST
  HKL_PREV = 0;
  HKL_NEXT = 1;

  KLF_ACTIVATE       = 16_1;
  KLF_SUBSTITUTE_OK  = 16_2;
  KLF_UNLOADPREVIOUS = 16_4;
  KLF_REORDER        = 16_8;

(*
 * Size of KeyboardLayoutName (number of characters), including nul terminator
 *)
CONST KL_NAMELENGTH = 9;

TYPE
  LoadKeyboardLayoutA = <*WINAPI*> PROCEDURE (pwszKLID: PCSTR; Flags: UINT32): HKL;
  LoadKeyboardLayoutW = <*WINAPI*> PROCEDURE (pwszKLID: PCWSTR; Flags: UINT32): HKL;
  LoadKeyboardLayout = LoadKeyboardLayoutA;

  ActivateKeyboardLayout = <*WINAPI*> PROCEDURE (hkl: HKL; Flags: UINT32): BOOL;
  UnloadKeyboardLayout   = <*WINAPI*> PROCEDURE (hkl: HKL): BOOL;
  GetKeyboardLayoutNameA = <*WINAPI*> PROCEDURE (pwszKLID: PSTR): BOOL;
  GetKeyboardLayoutNameW = <*WINAPI*> PROCEDURE (pwszKLID: PWSTR): BOOL;

  GetKeyboardLayoutName = GetKeyboardLayoutNameA;

(*
 * Desktop-specific access flags
 *)
CONST
  DESKTOP_ENUMWINDOWS    : INT32 = 16_0001;
  DESKTOP_CREATEWINDOW   : INT32 = 16_0002;
  DESKTOP_CREATEMENU     : INT32 = 16_0004;
  DESKTOP_HOOKCONTROL    : INT32 = 16_0008;
  DESKTOP_JOURNALRECORD  : INT32 = 16_0010;
  DESKTOP_JOURNALPLAYBACK: INT32 = 16_0020;
  DESKTOP_ENUMERATE      : INT32 = 16_0040;

TYPE GetThreadDesktop = <*WINAPI*> PROCEDURE (arg1: UINT32): HDESK;

(*
 * Windowstation-specific access flags
 *)
CONST
  WINSTA_ENUMDESKTOPS     : INT32 = 16_0001;
  WINSTA_READATTRIBUTES   : INT32 = 16_0002;
  WINSTA_ACCESSCLIPBOARD  : INT32 = 16_0004;
  WINSTA_CREATEDESKTOP    : INT32 = 16_0008;
  WINSTA_WRITEATTRIBUTES  : INT32 = 16_0010;
  WINSTA_ACCESSGLOBALATOMS: INT32 = 16_0020;
  WINSTA_EXITWINDOWS      : INT32 = 16_0040;
  WINSTA_ENUMERATE        : INT32 = 16_0100;
  WINSTA_READSCREEN       : INT32 = 16_0200;


<*EXTERNAL GetProcessWindowStation:WINAPI*>
PROCEDURE GetProcessWindowStation (): HWINSTA;

(*
 * window-specific access flags
 *)
CONST WIN_ACCESSWINDOW: INT32 = 16_0001;

(*
 * menu-specific access flags
 *)
CONST MENU_ACCESSMENU: INT32 = 16_0001;

<*EXTERNAL SetUserObjectSecurity:WINAPI*>
PROCEDURE SetUserObjectSecurity (arg1: HANDLE;
                                 arg2: PSECURITY_INFORMATION;
                                 arg3: PSECURITY_DESCRIPTOR   ): BOOL;

<*EXTERNAL GetUserObjectSecurity:WINAPI*>
PROCEDURE GetUserObjectSecurity (arg1: HANDLE;
                                 arg2: PSECURITY_INFORMATION;
                                 arg3: PSECURITY_DESCRIPTOR;
                                 arg4: UINT32;
                                 arg5: PUINT32                ): BOOL;

<*EXTERNAL ImpersonateDDEClientWindow:WINAPI*>
PROCEDURE ImpersonateDDEClientWindow (hwndClient: HWND; hwndServer: HWND):BOOL;

TYPE
  PWNDCLASSA = UNTRACED REF WNDCLASSA;
  NPWNDCLASSA = PWNDCLASSA;
  LPWNDCLASSA = PWNDCLASSA;
  WNDCLASSA = RECORD
    style        : UINT32;
    lpfnWndProc  : WNDPROC;
    cbClsExtra   : INT32;
    cbWndExtra   : INT32;
    hInstance    : HINSTANCE;
    hIcon        : HICON;
    hCursor      : HCURSOR;
    hbrBackground: HBRUSH;
    lpszMenuName : PCSTR;
    lpszClassName: PCSTR;
  END;

  PWNDCLASSW = UNTRACED REF WNDCLASSW;
  LPWNDCLASSW = PWNDCLASSW;
  WNDCLASSW = RECORD
    style        : UINT32;
    lpfnWndProc  : WNDPROC;
    cbClsExtra   : INT32;
    cbWndExtra   : INT32;
    hInstance    : HINSTANCE;
    hIcon        : HICON;
    hCursor      : HCURSOR;
    hbrBackground: HBRUSH;
    lpszMenuName : PCWSTR;
    lpszClassName: PCWSTR;
  END;

  WNDCLASS = WNDCLASSA;
  PWNDCLASS = PWNDCLASSA;
  NPWNDCLASS = NPWNDCLASSA;
  LPWNDCLASS = LPWNDCLASSA;

  (* Message structure *)
  PMSG = UNTRACED REF MSG;
  LPMSG = PMSG;
  MSG = RECORD
    hwnd   : HWND;
    message: UINT32;
    wParam : WPARAM;
    lParam : LPARAM;
    time   : UINT32;
    pt     : POINT;
  END;

(*!!!  SteveG.  Macros will cost you extra.  #define POINTSTOPOINT(pt,pts)
   {(pt).x = (INT16)LOWORD(pts); \ (pt).y = (INT16)HIWORD(pts);} #define
   POINTTOPOINTS(pt) (MAKELONG((INT16)((pt).x), (INT16)((pt).y))) #define
   MAKEWPARAM(l, h) (WPARAM)MAKELONG(l, h) #define MAKELPARAM(l, h)
   (LPARAM)MAKELONG(l, h) #define MAKELRESULT(l, h) (LRESULT)MAKELONG(l,
   h) *)

(* Window field offsets for GetWindowLong() *)
CONST
  GWL_WNDPROC    = (-4);
  GWL_HINSTANCE  = (-6);
  GWL_HWNDPARENT = (-8);
  GWL_STYLE      = (-16);
  GWL_EXSTYLE    = (-20);
  GWL_USERDATA   = (-21);
  GWL_ID         = (-12);

(* Class field offsets for GetClassLong() *)
  GCL_MENUNAME      = (-8);
  GCL_HBRBACKGROUND = (-10);
  GCL_HCURSOR       = (-12);
  GCL_HICON         = (-14);
  GCL_HMODULE       = (-16);
  GCL_CBWNDEXTRA    = (-18);
  GCL_CBCLSEXTRA    = (-20);
  GCL_WNDPROC       = (-24);
  GCL_STYLE         = (-26);

(*!!!define GCW_ATOM (-32)*)

(* Window Messages *)
  WM_NULL     = 16_0000;
  WM_CREATE   = 16_0001;
  WM_DESTROY  = 16_0002;
  WM_MOVE     = 16_0003;
  WM_SIZE     = 16_0005;
  WM_ACTIVATE = 16_0006;
(*
 * WM_ACTIVATE state values
 *)
  WA_INACTIVE    = 0;
  WA_ACTIVE      = 1;
  WA_CLICKACTIVE = 2;

  WM_SETFOCUS        = 16_0007;
  WM_KILLFOCUS       = 16_0008;
  WM_ENABLE          = 16_000A;
  WM_SETREDRAW       = 16_000B;
  WM_SETTEXT         = 16_000C;
  WM_GETTEXT         = 16_000D;
  WM_GETTEXTLENGTH   = 16_000E;
  WM_PAINT           = 16_000F;
  WM_CLOSE           = 16_0010;
  WM_QUERYENDSESSION = 16_0011;
  WM_QUIT            = 16_0012;
  WM_QUERYOPEN       = 16_0013;
  WM_ERASEBKGND      = 16_0014;
  WM_SYSCOLORCHANGE  = 16_0015;
  WM_ENDSESSION      = 16_0016;
  WM_SHOWWINDOW      = 16_0018;
  WM_WININICHANGE    = 16_001A;
  WM_DEVMODECHANGE   = 16_001B;
  WM_ACTIVATEAPP     = 16_001C;
  WM_FONTCHANGE      = 16_001D;
  WM_TIMECHANGE      = 16_001E;
  WM_CANCELMODE      = 16_001F;
  WM_SETCURSOR       = 16_0020;
  WM_MOUSEACTIVATE   = 16_0021;
  WM_CHILDACTIVATE   = 16_0022;
  WM_QUEUESYNC       = 16_0023;

(*
 * Struct pointed to by WM_GETMINMAXINFO lParam
 *)
TYPE
  MINMAXINFO = RECORD
    ptReserved    : POINT;
    ptMaxSize     : POINT;
    ptMaxPosition : POINT;
    ptMinTrackSize: POINT;
    ptMaxTrackSize: POINT;
  END;
  PMINMAXINFO = UNTRACED REF MINMAXINFO;
  LPMINMAXINFO = PMINMAXINFO;
CONST WM_GETMINMAXINFO = 16_0024;

CONST
  WM_PAINTICON      = 16_0026;
  WM_ICONERASEBKGND = 16_0027;
  WM_NEXTDLGCTL     = 16_0028;
  WM_SPOOLERSTATUS  = 16_002A;
  WM_DRAWITEM       = 16_002B;
  WM_MEASUREITEM    = 16_002C;
  WM_DELETEITEM     = 16_002D;
  WM_VKEYTOITEM     = 16_002E;
  WM_CHARTOITEM     = 16_002F;
  WM_SETFONT        = 16_0030;
  WM_GETFONT        = 16_0031;
  WM_SETHOTKEY      = 16_0032;
  WM_GETHOTKEY      = 16_0033;
  WM_QUERYDRAGICON  = 16_0037;
  WM_COMPAREITEM    = 16_0039;
  WM_FULLSCREEN     = 16_003A;

  WM_COMPACTING           = 16_0041;
  WM_OTHERWINDOWCREATED   = 16_0042;
  WM_OTHERWINDOWDESTROYED = 16_0043;
  WM_COMMNOTIFY           = 16_0044;
  WM_HOTKEYEVENT          = 16_0045;
  WM_WINDOWPOSCHANGING    = 16_0046;
  WM_WINDOWPOSCHANGED     = 16_0047;
  WM_POWER                = 16_0048;
  WM_COPYDATA             = 16_004A;
TYPE
  COPYDATASTRUCT = RECORD
    dwData: SIZE_T;
    cbData: UINT32;
    lpData: PVOID;
  END;
  PCOPYDATASTRUCT = UNTRACED REF COPYDATASTRUCT;

CONST
  WM_NCCREATE        = 16_0081;
  WM_NCDESTROY       = 16_0082;
  WM_NCCALCSIZE      = 16_0083;
  WM_NCHITTEST       = 16_0084;
  WM_NCPAINT         = 16_0085;
  WM_NCACTIVATE      = 16_0086;
  WM_GETDLGCODE      = 16_0087;
  WM_NCMOUSEMOVE     = 16_00A0;
  WM_NCLBUTTONDOWN   = 16_00A1;
  WM_NCLBUTTONUP     = 16_00A2;
  WM_NCLBUTTONDBLCLK = 16_00A3;
  WM_NCRBUTTONDOWN   = 16_00A4;
  WM_NCRBUTTONUP     = 16_00A5;
  WM_NCRBUTTONDBLCLK = 16_00A6;
  WM_NCMBUTTONDOWN   = 16_00A7;
  WM_NCMBUTTONUP     = 16_00A8;
  WM_NCMBUTTONDBLCLK = 16_00A9;

  WM_KEYFIRST    = 16_0100;
  WM_KEYDOWN     = 16_0100;
  WM_KEYUP       = 16_0101;
  WM_CHAR        = 16_0102;
  WM_DEADCHAR    = 16_0103;
  WM_SYSKEYDOWN  = 16_0104;
  WM_SYSKEYUP    = 16_0105;
  WM_SYSCHAR     = 16_0106;
  WM_SYSDEADCHAR = 16_0107;
  WM_KEYLAST     = 16_0108;

  WM_INITDIALOG    = 16_0110;
  WM_COMMAND       = 16_0111;
  WM_SYSCOMMAND    = 16_0112;
  WM_TIMER         = 16_0113;
  WM_HSCROLL       = 16_0114;
  WM_VSCROLL       = 16_0115;
  WM_INITMENU      = 16_0116;
  WM_INITMENUPOPUP = 16_0117;
  WM_MENUSELECT    = 16_011F;
  WM_MENUCHAR      = 16_0120;
  WM_ENTERIDLE     = 16_0121;

  WM_CTLCOLORMSGBOX    = 16_0132;
  WM_CTLCOLOREDIT      = 16_0133;
  WM_CTLCOLORLISTBOX   = 16_0134;
  WM_CTLCOLORBTN       = 16_0135;
  WM_CTLCOLORDLG       = 16_0136;
  WM_CTLCOLORSCROLLBAR = 16_0137;
  WM_CTLCOLORSTATIC    = 16_0138;

  WM_MOUSEFIRST    = 16_0200;
  WM_MOUSEMOVE     = 16_0200;
  WM_LBUTTONDOWN   = 16_0201;
  WM_LBUTTONUP     = 16_0202;
  WM_LBUTTONDBLCLK = 16_0203;
  WM_RBUTTONDOWN   = 16_0204;
  WM_RBUTTONUP     = 16_0205;
  WM_RBUTTONDBLCLK = 16_0206;
  WM_MBUTTONDOWN   = 16_0207;
  WM_MBUTTONUP     = 16_0208;
  WM_MBUTTONDBLCLK = 16_0209;
  WM_MOUSELAST     = 16_0209;

  WM_PARENTNOTIFY   = 16_0210;
  WM_ENTERMENULOOP  = 16_0211;
  WM_EXITMENULOOP   = 16_0212;

  WM_NEXTMENU       = 16_0213;
  WM_SIZING         = 16_0214;
  WM_CAPTURECHANGED = 16_0215;
  WM_MOVING         = 16_0216;
  WM_POWERBROADCAST = 16_0218;
  WM_DEVICECHANGE   = 16_0219;

  WM_MDICREATE      = 16_0220;
  WM_MDIDESTROY     = 16_0221;
  WM_MDIACTIVATE    = 16_0222;
  WM_MDIRESTORE     = 16_0223;
  WM_MDINEXT        = 16_0224;
  WM_MDIMAXIMIZE    = 16_0225;
  WM_MDITILE        = 16_0226;
  WM_MDICASCADE     = 16_0227;
  WM_MDIICONARRANGE = 16_0228;
  WM_MDIGETACTIVE   = 16_0229;
  WM_MDISETMENU     = 16_0230;
  WM_ENTERSIZEMOVE_UNDOCUMENTED = 16_0231;
  WM_EXITSIZEMOVE_UNDOCUMENTED = 16_0232;
  WM_DROPFILES      = 16_0233;
  WM_MDIREFRESHMENU = 16_0234;

  WM_CUT               = 16_0300;
  WM_COPY              = 16_0301;
  WM_PASTE             = 16_0302;
  WM_CLEAR             = 16_0303;
  WM_UNDO              = 16_0304;
  WM_RENDERFORMAT      = 16_0305;
  WM_RENDERALLFORMATS  = 16_0306;
  WM_DESTROYCLIPBOARD  = 16_0307;
  WM_DRAWCLIPBOARD     = 16_0308;
  WM_PAINTCLIPBOARD    = 16_0309;
  WM_VSCROLLCLIPBOARD  = 16_030A;
  WM_SIZECLIPBOARD     = 16_030B;
  WM_ASKCBFORMATNAME   = 16_030C;
  WM_CHANGECBCHAIN     = 16_030D;
  WM_HSCROLLCLIPBOARD  = 16_030E;
  WM_QUERYNEWPALETTE   = 16_030F;
  WM_PALETTEISCHANGING = 16_0310;
  WM_PALETTECHANGED    = 16_0311;
  WM_HOTKEY            = 16_0312;

(* PenWindows specific messages *)
  WM_PENWINFIRST = 16_0380;
  WM_PENWINLAST  = 16_038F;

  WM_MM_RESERVED_FIRST = 16_03A0;
  WM_MM_RESERVED_LAST  = 16_03DF;

(* NOTE: All Message Numbers below 16_0400 are RESERVED. *)

(* Private Window Messages Start Here: *)
CONST WM_USER = 16_0400;

(* WM_SYNCTASK Commands *)
  ST_BEGINSWP = 0;
  ST_ENDSWP   = 1;

(* WinWhere() Area Codes *)
  HTERROR       = (-2);
  HTTRANSPARENT = (-1);
  HTNOWHERE     = 0;
  HTCLIENT      = 1;
  HTCAPTION     = 2;
  HTSYSMENU     = 3;
  HTGROWBOX     = 4;
  HTSIZE        = HTGROWBOX;
  HTMENU        = 5;
  HTHSCROLL     = 6;
  HTVSCROLL     = 7;
  HTMINBUTTON   = 8;
  HTMAXBUTTON   = 9;
  HTLEFT        = 10;
  HTRIGHT       = 11;
  HTTOP         = 12;
  HTTOPLEFT     = 13;
  HTTOPRIGHT    = 14;
  HTBOTTOM      = 15;
  HTBOTTOMLEFT  = 16;
  HTBOTTOMRIGHT = 17;
  HTBORDER      = 18;
  HTREDUCE      = HTMINBUTTON;
  HTZOOM        = HTMAXBUTTON;
  HTSIZEFIRST   = HTLEFT;
  HTSIZELAST    = HTBOTTOMRIGHT;

(* SendMessageTimeout values *)
  SMTO_NORMAL      = 16_0000;
  SMTO_BLOCK       = 16_0001;
  SMTO_ABORTIFHUNG = 16_0002;

(* WM_MOUSEACTIVATE Return Codes *)
  MA_ACTIVATE         = 1;
  MA_ACTIVATEANDEAT   = 2;
  MA_NOACTIVATE       = 3;
  MA_NOACTIVATEANDEAT = 4;

<*EXTERNAL RegisterWindowMessageA:WINAPI*>
PROCEDURE RegisterWindowMessageA (lpString: PCSTR): UINT32;

<*EXTERNAL RegisterWindowMessageW:WINAPI*>
PROCEDURE RegisterWindowMessageW (lpString: PCWSTR): UINT32;
CONST RegisterWindowMessage = RegisterWindowMessageA;

(* WM_SIZE message wParam values *)
CONST
  SIZE_RESTORED  = 0;
  SIZE_MINIMIZED = 1;
  SIZE_MAXIMIZED = 2;
  SIZE_MAXSHOW   = 3;
  SIZE_MAXHIDE   = 4;

(* Obsolete constant names *)
  SIZENORMAL     = SIZE_RESTORED;
  SIZEICONIC     = SIZE_MINIMIZED;
  SIZEFULLSCREEN = SIZE_MAXIMIZED;
  SIZEZOOMSHOW   = SIZE_MAXSHOW;
  SIZEZOOMHIDE   = SIZE_MAXHIDE;

(* WM_WINDOWPOSCHANGING/CHANGED struct pointed to by lParam *)
TYPE
  WINDOWPOS = RECORD
    hwnd           : HWND;
    hwndInsertAfter: HWND;
    x              : INT32;
    y              : INT32;
    cx             : INT32;
    cy             : INT32;
    flags          : UINT32;
  END;
  PWINDOWPOS = UNTRACED REF WINDOWPOS;
  LPWINDOWPOS = PWINDOWPOS;

  (* WM_NCCALCSIZE parameter structure *)
  NCCALCSIZE_PARAMS = RECORD
    rgrc : ARRAY [0 .. 3 - 1] OF RECT;
    lppos: PWINDOWPOS;
  END;
  PNCCALCSIZE_PARAMS = UNTRACED REF NCCALCSIZE_PARAMS;
  LPNCCALCSIZE_PARAMS = PNCCALCSIZE_PARAMS;

(* WM_NCCALCSIZE "window valid rect" return values *)
CONST
  WVR_ALIGNTOP    = 16_0010;
  WVR_ALIGNLEFT   = 16_0020;
  WVR_ALIGNBOTTOM = 16_0040;
  WVR_ALIGNRIGHT  = 16_0080;
  WVR_HREDRAW     = 16_0100;
  WVR_VREDRAW     = 16_0200;
  WVR_REDRAW      = Or(WVR_HREDRAW, WVR_VREDRAW);
  WVR_VALIDRECTS  = 16_0400;

(* Key State Masks for Mouse Messages *)
  MK_LBUTTON = 16_0001;
  MK_RBUTTON = 16_0002;
  MK_SHIFT   = 16_0004;
  MK_CONTROL = 16_0008;
  MK_MBUTTON = 16_0010;

(* Window Styles *)
  WS_OVERLAPPED  : INT32 = 16_00000000;
  WS_POPUP       : INT32 = 16_80000000;
  WS_CHILD       : INT32 = 16_40000000;
  WS_MINIMIZE    : INT32 = 16_20000000;
  WS_VISIBLE     : INT32 = 16_10000000;
  WS_DISABLED    : INT32 = 16_08000000;
  WS_CLIPSIBLINGS: INT32 = 16_04000000;
  WS_CLIPCHILDREN: INT32 = 16_02000000;
  WS_MAXIMIZE    : INT32 = 16_01000000;
  WS_CAPTION     : INT32 = 16_00C00000; (* WS_BORDER | WS_DLGFRAME *)
  WS_BORDER      : INT32 = 16_00800000;
  WS_DLGFRAME    : INT32 = 16_00400000;
  WS_VSCROLL     : INT32 = 16_00200000;
  WS_HSCROLL     : INT32 = 16_00100000;
  WS_SYSMENU     : INT32 = 16_00080000;
  WS_THICKFRAME  : INT32 = 16_00040000;
  WS_GROUP       : INT32 = 16_00020000;
  WS_TABSTOP     : INT32 = 16_00010000;

  WS_MINIMIZEBOX: INT32 = 16_00020000;
  WS_MAXIMIZEBOX: INT32 = 16_00010000;

  WS_TILED       = WS_OVERLAPPED;
  WS_ICONIC      = WS_MINIMIZE;
  WS_SIZEBOX     = WS_THICKFRAME;
  WS_TILEDWINDOW = WS_OVERLAPPEDWINDOW;

(* Common Window Styles *)
CONST
  WS_OVERLAPPEDWINDOW = Or(WS_OVERLAPPED,
                           Or(WS_CAPTION,
                              Or(WS_SYSMENU,
                                 Or(WS_THICKFRAME,
                                    Or(WS_MINIMIZEBOX, WS_MAXIMIZEBOX)))));
  WS_POPUPWINDOW = Or(WS_POPUP, Or(WS_BORDER, WS_SYSMENU));
  WS_CHILDWINDOW = WS_CHILD;

(* Extended Window Styles *)
  WS_EX_DLGMODALFRAME : INT32 = 16_00000001;
  WS_EX_NOPARENTNOTIFY: INT32 = 16_00000004;
  WS_EX_TOPMOST       : INT32 = 16_00000008;
  WS_EX_ACCEPTFILES   : INT32 = 16_00000010;
  WS_EX_TRANSPARENT   : INT32 = 16_00000020;

(* Class styles *)
CONST
  CS_VREDRAW      = 16_0001;
  CS_HREDRAW      = 16_0002;
  CS_KEYCVTWINDOW = 16_0004;
  CS_DBLCLKS      = 16_0008;
  (* 16_0010 - reserved (see user\server\usersrv.h) *)
  CS_OWNDC           = 16_0020;
  CS_CLASSDC         = 16_0040;
  CS_PARENTDC        = 16_0080;
  CS_NOKEYCVT        = 16_0100;
  CS_NOCLOSE         = 16_0200;
  CS_SAVEBITS        = 16_0800;
  CS_BYTEALIGNCLIENT = 16_1000;
  CS_BYTEALIGNWINDOW = 16_2000;
  CS_GLOBALCLASS     = 16_4000; (* Global window class *)

(* Predefined Clipboard Formats *)
  CF_TEXT         = 1;
  CF_BITMAP       = 2;
  CF_METAFILEPICT = 3;
  CF_SYLK         = 4;
  CF_DIF          = 5;
  CF_TIFF         = 6;
  CF_OEMTEXT      = 7;
  CF_DIB          = 8;
  CF_PALETTE      = 9;
  CF_PENDATA      = 10;
  CF_RIFF         = 11;
  CF_WAVE         = 12;
  CF_UNICODETEXT  = 13;
  CF_ENHMETAFILE  = 14;

  CF_OWNERDISPLAY    = 16_0080;
  CF_DSPTEXT         = 16_0081;
  CF_DSPBITMAP       = 16_0082;
  CF_DSPMETAFILEPICT = 16_0083;
  CF_DSPENHMETAFILE  = 16_008E;

(* "Private" formats don't get GlobalFree()'d *)
  CF_PRIVATEFIRST = 16_0200;
  CF_PRIVATELAST  = 16_02FF;

(* "GDIOBJ" formats do get DeleteObject()'d *)
  CF_GDIOBJFIRST = 16_0300;
  CF_GDIOBJLAST  = 16_03FF;

(*
 * Defines for the fVirt field of the Accelerator table structure.
 *)
  FNOINVERT = 16_02;
  FSHIFT    = 16_04;
  FCONTROL  = 16_08;
  FALT      = 16_10;

TYPE
  ACCEL = RECORD
    fVirt: UINT8;        (* Also called the flags field *)
    padding: UINT8;
    key  : UINT16;
    cmd  : UINT16;
  END;
  PACCEL = UNTRACED REF ACCEL;
  LPACCEL = PACCEL;

  PAINTSTRUCT = RECORD
    hdc        : HDC;
    fErase     : BOOL;
    rcPaint    : RECT;
    fRestore   : BOOL;
    fIncUpdate : BOOL;
    rgbReserved: ARRAY [0 .. 32 - 1] OF UINT8;
  END;
  PPAINTSTRUCT = UNTRACED REF PAINTSTRUCT;
  LPPAINTSTRUCT = PPAINTSTRUCT;

  PCREATESTRUCTA = UNTRACED REF CREATESTRUCTA;
  LPCREATESTRUCTA = PCREATESTRUCTA;
  CREATESTRUCTA = RECORD
    lpCreateParams: PVOID;
    hInstance     : HINSTANCE;
    hMenu         : HMENU;
    hwndParent    : HWND;
    cy            : INT32;
    cx            : INT32;
    y             : INT32;
    x             : INT32;
    style         : INT32;
    lpszName      : PCSTR;
    lpszClass     : PCSTR;
    dwExStyle     : UINT32;
  END;

  LPCREATESTRUCTW = CREATESTRUCTW;
  CREATESTRUCTW = RECORD
    hInstance     : HINSTANCE;
    lpCreateParams: PVOID;
    hMenu         : HMENU;
    hwndParent    : HWND;
    cy            : INT32;
    cx            : INT32;
    y             : INT32;
    x             : INT32;
    style         : INT32;
    lpszName      : PCWSTR;
    lpszClass     : PCWSTR;
    dwExStyle     : UINT32;
  END;

TYPE
  CREATESTRUCT = CREATESTRUCTA;
  LPCREATESTRUCT = LPCREATESTRUCTA;

  PWINDOWPLACEMENT = UNTRACED REF WINDOWPLACEMENT;
  LPWINDOWPLACEMENT = UNTRACED REF WINDOWPLACEMENT;
  WINDOWPLACEMENT = RECORD
    length          : UINT32;
    flags           : UINT32;
    showCmd         : UINT32;
    ptMinPosition   : POINT;
    ptMaxPosition   : POINT;
    rcNormalPosition: RECT;
  END;

CONST
  WPF_SETMINPOSITION     = 16_0001;
  WPF_RESTORETOMAXIMIZED = 16_0002;

(* Owner draw control types *)
  ODT_MENU     = 1;
  ODT_LISTBOX  = 2;
  ODT_COMBOBOX = 3;
  ODT_BUTTON   = 4;

(* Owner draw actions *)
  ODA_DRAWENTIRE = 16_0001;
  ODA_SELECT     = 16_0002;
  ODA_FOCUS      = 16_0004;

(* Owner draw state *)
  ODS_SELECTED = 16_0001;
  ODS_GRAYED   = 16_0002;
  ODS_DISABLED = 16_0004;
  ODS_CHECKED  = 16_0008;
  ODS_FOCUS    = 16_0010;

(* MEASUREITEMSTRUCT for ownerdraw *)
TYPE
  MEASUREITEMSTRUCT = RECORD
    CtlType   : UINT32;
    CtlID     : UINT32;
    itemID    : UINT32;
    itemWidth : UINT32;
    itemHeight: UINT32;
    itemData  : SIZE_T;
  END;

  (* DRAWITEMSTRUCT for ownerdraw *)
  DRAWITEMSTRUCT = RECORD
    CtlType   : UINT32;
    CtlID     : UINT32;
    itemID    : UINT32;
    itemAction: UINT32;
    itemState : UINT32;
    hwndItem  : HWND;
    hDC       : HDC;
    rcItem    : RECT;
    itemData  : SIZE_T;
  END;

  (* DELETEITEMSTRUCT for ownerdraw *)
  DELETEITEMSTRUCT = RECORD
    CtlType : UINT32;
    CtlID   : UINT32;
    itemID  : UINT32;
    hwndItem: HWND;
    itemData: SIZE_T;
  END;

  (* COMPAREITEMSTUCT for ownerdraw sorting *)
  COMPAREITEMSTRUCT = RECORD
    CtlType   : UINT32;
    CtlID     : UINT32;
    hwndItem  : HWND;
    itemID1   : UINT32;
    itemData1 : SIZE_T;
    itemID2   : UINT32;
    itemData2 : SIZE_T;
    dwLocaleId: UINT32;
  END;

(* Message Function Templates *)

<*EXTERNAL GetMessageA:WINAPI*>
PROCEDURE GetMessageA (lpMsg        : LPMSG;
                       hWnd         : HWND;
                       wMsgFilterMin: UINT32;
                      wMsgFilterMax: UINT32   ): BOOL;

(* Message Function Templates *)

<*EXTERNAL GetMessageW:WINAPI*>
PROCEDURE GetMessageW (lpMsg        : LPMSG;
                       hWnd         : HWND;
                       wMsgFilterMin: UINT32;
                       wMsgFilterMax: UINT32   ): BOOL;
CONST GetMessage = GetMessageA;

<*EXTERNAL TranslateMessage:WINAPI*>
PROCEDURE TranslateMessage (lpMsg: UNTRACED REF MSG): BOOL;

<*EXTERNAL DispatchMessageA:WINAPI*>
PROCEDURE DispatchMessageA (lpMsg: UNTRACED REF MSG): INT32;

<*EXTERNAL DispatchMessageW:WINAPI*>
PROCEDURE DispatchMessageW (lpMsg: UNTRACED REF MSG): INT32;
CONST DispatchMessage = DispatchMessageA;

<*EXTERNAL PeekMessageA:WINAPI*>
PROCEDURE PeekMessageA (lpMsg        : LPMSG;
                        hWnd         : HWND;
                        wMsgFilterMin: UINT32;
                        wMsgFilterMax: UINT32;
                        wRemoveMsg   : UINT32   ): BOOL;

<*EXTERNAL PeekMessageW:WINAPI*>
PROCEDURE PeekMessageW (lpMsg        : LPMSG;
                        hWnd         : HWND;
                        wMsgFilterMin: UINT32;
                        wMsgFilterMax: UINT32;
                        wRemoveMsg   : UINT32   ): BOOL;
CONST PeekMessage = PeekMessageA;

(* PeekMessage() Options *)
CONST
  PM_NOREMOVE = 16_0000;
  PM_REMOVE   = 16_0001;
  PM_NOYIELD  = 16_0002;

<*EXTERNAL RegisterHotKey:WINAPI*>
PROCEDURE RegisterHotKey (hwnd       : HWND;
                          id         : INT32;
                          fsModifiers: UINT32;
                          vk         : UINT32  ): BOOL;

<*EXTERNAL UnregisterHotKey:WINAPI*>
PROCEDURE UnregisterHotKey (hwnd: HWND; id: INT32): BOOL;

CONST
  MOD_ALT     = 16_0001;
  MOD_CONTROL = 16_0002;
  MOD_SHIFT   = 16_0004;

  IDHOT_SNAPWINDOW  = (-1);     (* SHIFT-PRINTSCRN *)
  IDHOT_SNAPDESKTOP = (-2);     (* PRINTSCRN *)

  EWX_LOGOFF   = 0;
  EWX_SHUTDOWN = 1;
  EWX_REBOOT   = 2;
  EWX_FORCE    = 4;

PROCEDURE ExitWindows(dwReserved: UINT32; Code: UINT32): BOOL;

<*EXTERNAL ExitWindowsEx:WINAPI*>
PROCEDURE ExitWindowsEx (uFlags: UINT32; ForceTimeout: UINT32): BOOL;

<*EXTERNAL SwapMouseButton:WINAPI*>
PROCEDURE SwapMouseButton (arg1: BOOL): BOOL;

<*EXTERNAL GetMessagePos:WINAPI*>
PROCEDURE GetMessagePos (): UINT32;

<*EXTERNAL GetMessageTime:WINAPI*>
PROCEDURE GetMessageTime (): INT32;

<*EXTERNAL GetMessageExtraInfo:WINAPI*>
PROCEDURE GetMessageExtraInfo (): INT32;

<*EXTERNAL SendMessageA:WINAPI*>
PROCEDURE SendMessageA (hWnd  : HWND;
                        Msg   : UINT32;
                        wParam: WPARAM;
                        lParam: LPARAM  ): LRESULT;

<*EXTERNAL SendMessageW:WINAPI*>
PROCEDURE SendMessageW (hWnd  : HWND;
                        Msg   : UINT32;
                        wParam: WPARAM;
                        lParam: LPARAM  ): LRESULT;
CONST SendMessage = SendMessageA;

<*EXTERNAL SendMessageTimeoutA:WINAPI*>
PROCEDURE SendMessageTimeoutA (hWnd      : HWND;
                               Msg       : UINT32;
                               wParam    : WPARAM;
                               lParam    : LPARAM;
                               fuFlags   : UINT32;
                               uTimeout  : UINT32;
                               lpdwResult: PUINT32 ): LRESULT;

<*EXTERNAL SendMessageTimeoutW:WINAPI*>
PROCEDURE SendMessageTimeoutW (hWnd      : HWND;
                               Msg       : UINT32;
                               wParam    : WPARAM;
                               lParam    : LPARAM;
                               fuFlags   : UINT32;
                               uTimeout  : UINT32;
                               lpdwResult: PUINT32 ): LRESULT;
CONST SendMessageTimeout = SendMessageTimeoutA;

<*EXTERNAL SendNotifyMessageA:WINAPI*>
PROCEDURE SendNotifyMessageA (hwnd  : HWND;
                                Msg   : UINT32;
                                wParam: WPARAM;
                                lParam: LPARAM  ): BOOL;

<*EXTERNAL SendNotifyMessageW:WINAPI*>
PROCEDURE SendNotifyMessageW (hwnd  : HWND;
                                Msg   : UINT32;
                                wParam: WPARAM;
                                lParam: LPARAM  ): BOOL;
CONST SendNotifyMessage = SendNotifyMessageA;

<*EXTERNAL SendMessageCallbackA:WINAPI*>
PROCEDURE SendMessageCallbackA (hwnd            : HWND;
                                  Msg             : UINT32;
                                  wParam          : WPARAM;
                                  lParam          : LPARAM;
                                  lpResultCallBack: SENDASYNCPROC;
                                  dwData          : UINT32          ): BOOL;

<*EXTERNAL SendMessageCallbackW:WINAPI*>
PROCEDURE SendMessageCallbackW (hwnd            : HWND;
                                  Msg             : UINT32;
                                  wParam          : WPARAM;
                                  lParam          : LPARAM;
                                  lpResultCallBack: SENDASYNCPROC;
                                  dwData          : UINT32          ): BOOL;
CONST SendMessageCallback = SendMessageCallbackA;

<*EXTERNAL PostMessageA:WINAPI*>
PROCEDURE PostMessageA (hWnd  : HWND;
                        Msg   : UINT32;
                        wParam: WPARAM;
                        lParam: LPARAM  ): BOOL;

<*EXTERNAL PostMessageW:WINAPI*>
PROCEDURE PostMessageW (hWnd  : HWND;
                        Msg   : UINT32;
                        wParam: WPARAM;
                        lParam: LPARAM  ): BOOL;
CONST PostMessage = PostMessageA;

<*EXTERNAL PostThreadMessageA:WINAPI*>
PROCEDURE PostThreadMessageA (idThread: UINT32;
                              Msg     : UINT32;
                              wParam  : WPARAM;
                              lParam  : LPARAM  ): BOOL;

<*EXTERNAL PostThreadMessageW:WINAPI*>
PROCEDURE PostThreadMessageW (idThread: UINT32;
                              Msg     : UINT32;
                              wParam  : WPARAM;
                              lParam  : LPARAM  ): BOOL;
CONST PostThreadMessage = PostThreadMessageA;

PROCEDURE PostAppMessageA (idThread: UINT32;
                           wMsg    : UINT32;
                           wParam  : WPARAM;
                           lParam  : LPARAM  ): BOOL;

PROCEDURE PostAppMessageW (idThread: UINT32;
                           wMsg    : UINT32;
                           wParam  : WPARAM;
                           lParam  : LPARAM  ): BOOL;

CONST PostAppMessage = PostAppMessageA;

(* Special HWND value for use with PostMessage() and SendMessage() *)
VAR (* CONST *)
  HWND_BROADCAST: HWND;

<*EXTERNAL AttachThreadInput:WINAPI*>
PROCEDURE AttachThreadInput (idAttach  : UINT32;
                             idAttachTo: UINT32;
                             fAttach   : BOOL   ): BOOL;

<*EXTERNAL ReplyMessage:WINAPI*>
PROCEDURE ReplyMessage (arg1: LRESULT): BOOL;

<*EXTERNAL WaitMessage:WINAPI*>
PROCEDURE WaitMessage (): BOOL;

<*EXTERNAL WaitForInputIdle:WINAPI*>
PROCEDURE WaitForInputIdle (hProcess: HANDLE; dwMilliseconds: UINT32): UINT32;

<*EXTERNAL DefWindowProcA:WINAPI*>
PROCEDURE DefWindowProcA (hWnd  : HWND;
                            Msg   : UINT32;
                            wParam: WPARAM;
                            lParam: LPARAM  ): LRESULT;

<*EXTERNAL DefWindowProcW:WINAPI*>
PROCEDURE DefWindowProcW (hWnd  : HWND;
                            Msg   : UINT32;
                            wParam: WPARAM;
                            lParam: LPARAM  ): LRESULT;
CONST DefWindowProc = DefWindowProcA;

<*EXTERNAL PostQuitMessage:WINAPI*>
PROCEDURE PostQuitMessage (nExitCode: INT32);

<*EXTERNAL CallWindowProcA:WINAPI*>
PROCEDURE CallWindowProcA (lpPrevWndFunc: WNDPROC;
                             hWnd         : HWND;
                             Msg          : UINT32;
                             wParam       : WPARAM;
                             lParam       : LPARAM   ): LRESULT;

<*EXTERNAL CallWindowProcW:WINAPI*>
PROCEDURE CallWindowProcW (lpPrevWndFunc: WNDPROC;
                             hWnd         : HWND;
                             Msg          : UINT32;
                             wParam       : WPARAM;
                             lParam       : LPARAM   ): LRESULT;
CONST CallWindowProc = CallWindowProcA;

<*EXTERNAL InSendMessage:WINAPI*>
PROCEDURE InSendMessage (): BOOL;

<*EXTERNAL GetDoubleClickTime:WINAPI*>
PROCEDURE GetDoubleClickTime (): UINT32;

<*EXTERNAL SetDoubleClickTime:WINAPI*>
PROCEDURE SetDoubleClickTime (arg1: UINT32): BOOL;

<*EXTERNAL RegisterClassA:WINAPI*>
PROCEDURE RegisterClassA (lpWndClass: UNTRACED REF WNDCLASSA): ATOM;

<*EXTERNAL RegisterClassW:WINAPI*>
PROCEDURE RegisterClassW (lpWndClass: UNTRACED REF WNDCLASSW): ATOM;
CONST RegisterClass = RegisterClassA;

<*EXTERNAL UnregisterClassA:WINAPI*>
PROCEDURE UnregisterClassA (lpClassName: PCSTR; hInstance: HINSTANCE): BOOL;

<*EXTERNAL UnregisterClassW:WINAPI*>
PROCEDURE UnregisterClassW (lpClassName: PCWSTR; hInstance: HINSTANCE): BOOL;
CONST UnregisterClass = UnregisterClassA;

<*EXTERNAL GetClassInfoA:WINAPI*>
PROCEDURE GetClassInfoA (hInstance  : HINSTANCE;
                           lpClassName: PCSTR;
                           lpWndClass : LPWNDCLASSA): BOOL;

<*EXTERNAL GetClassInfoW:WINAPI*>
PROCEDURE GetClassInfoW (hInstance  : HINSTANCE;
                           lpClassName: PCWSTR;
                           lpWndClass : LPWNDCLASSW): BOOL;
CONST GetClassInfo = GetClassInfoA;

CONST CW_USEDEFAULT = 16_80000000;

(* Special value for CreateWindow, et al. *)
VAR                             (* CONST *)
  HWND_DESKTOP: HWND;

<*EXTERNAL CreateWindowExA:WINAPI*>
PROCEDURE CreateWindowExA (dwExStyle   : UINT32;
                             lpClassName : PCSTR;
                             lpWindowName: PCSTR;
                             dwStyle     : UINT32;
                             X           : INT32;
                             Y           : INT32;
                             nWidth      : INT32;
                             nHeight     : INT32;
                             hWndParent  : HWND;
                             hMenu       : HMENU;
                             hInstance   : HINSTANCE;
                             lpParam     : PVOID     ): HWND;

<*EXTERNAL CreateWindowExW:WINAPI*>
PROCEDURE CreateWindowExW (dwExStyle   : UINT32;
                             lpClassName : PCWSTR;
                             lpWindowName: PCWSTR;
                             dwStyle     : UINT32;
                             X           : INT32;
                             Y           : INT32;
                             nWidth      : INT32;
                             nHeight     : INT32;
                             hWndParent  : HWND;
                             hMenu       : HMENU;
                             hInstance   : HINSTANCE;
                             lpParam     : PVOID     ): HWND;
CONST CreateWindowEx = CreateWindowExA;

PROCEDURE CreateWindowA (lpClassName : PCSTR;
                         lpWindowName: PCSTR;
                         dwStyle     : UINT32;
                         x           : INT32;
                         y           : INT32;
                         nWidth      : INT32;
                         nHeight     : INT32;
                         hwndParent  : HWND;
                         hMenu       : HMENU;
                         hInstance   : HINSTANCE;
                         lpParam     : PVOID     ): HWND;

PROCEDURE CreateWindowW (lpClassName : PCWSTR;
                         lpWindowName: PCWSTR;
                         dwStyle     : UINT32;
                         x           : INT32;
                         y           : INT32;
                         nWidth      : INT32;
                         nHeight     : INT32;
                         hwndParent  : HWND;
                         hMenu       : HMENU;
                         hInstance   : HINSTANCE;
                         lpParam     : PVOID     ): HWND;

CONST CreateWindow = CreateWindowA;

<*EXTERNAL IsWindow:WINAPI*>
PROCEDURE IsWindow (hWnd: HWND): BOOL;

<*EXTERNAL IsMenu:WINAPI*>
PROCEDURE IsMenu (hMenu: HMENU): BOOL;

<*EXTERNAL IsChild:WINAPI*>
PROCEDURE IsChild (hWndParent: HWND; hWnd: HWND): BOOL;

<*EXTERNAL DestroyWindow:WINAPI*>
PROCEDURE DestroyWindow (hWnd: HWND): BOOL;

<*EXTERNAL ShowWindow:WINAPI*>
PROCEDURE ShowWindow (hWnd: HWND; nCmdShow: INT32): BOOL;

<*EXTERNAL FlashWindow:WINAPI*>
PROCEDURE FlashWindow (hWnd: HWND; bInvert: BOOL): BOOL;

<*EXTERNAL ShowOwnedPopups:WINAPI*>
PROCEDURE ShowOwnedPopups (hWnd: HWND; fShow: BOOL): BOOL;

<*EXTERNAL OpenIcon:WINAPI*>
PROCEDURE OpenIcon (hWnd: HWND): BOOL;

<*EXTERNAL CloseWindow:WINAPI*>
PROCEDURE CloseWindow (hWnd: HWND): BOOL;

<*EXTERNAL MoveWindow:WINAPI*>
PROCEDURE MoveWindow (hWnd    : HWND;
                      X       : INT32;
                      Y       : INT32;
                      nWidth  : INT32;
                      nHeight : INT32;
                      bRepaint: BOOL  ): BOOL;

<*EXTERNAL SetWindowPos:WINAPI*>
PROCEDURE SetWindowPos (hWnd           : HWND;
                        hWndInsertAfter: HWND;
                        X              : INT32;
                        Y              : INT32;
                        cx             : INT32;
                        cy             : INT32;
                        uFlags         : UINT32  ): BOOL;

<*EXTERNAL GetWindowPlacement:WINAPI*>
PROCEDURE GetWindowPlacement (hwnd   : HWND;
                              lpwndpl: UNTRACED REF WINDOWPLACEMENT): BOOL;

<*EXTERNAL SetWindowPlacement:WINAPI*>
PROCEDURE SetWindowPlacement (hwnd   : HWND;
                              lpwndpl: UNTRACED REF WINDOWPLACEMENT): BOOL;

<*EXTERNAL BeginDeferWindowPos:WINAPI*>
PROCEDURE BeginDeferWindowPos (nNumWindows: INT32): HDWP;

<*EXTERNAL DeferWindowPos:WINAPI*>
PROCEDURE DeferWindowPos (hWinPosInfo    : HDWP;
                          hWnd           : HWND;
                          hWndInsertAfter: HWND;
                          x              : INT32;
                          y              : INT32;
                          cx             : INT32;
                          cy             : INT32;
                          uFlags         : UINT32  ): HDWP;

<*EXTERNAL EndDeferWindowPos:WINAPI*>
PROCEDURE EndDeferWindowPos (hWinPosInfo: HDWP): BOOL;

<*EXTERNAL IsWindowVisible:WINAPI*>
PROCEDURE IsWindowVisible (hWnd: HWND): BOOL;

<*EXTERNAL IsIconic:WINAPI*>
PROCEDURE IsIconic (hWnd: HWND): BOOL;

<*EXTERNAL AnyPopup:WINAPI*>
PROCEDURE AnyPopup (): BOOL;

<*EXTERNAL BringWindowToTop:WINAPI*>
PROCEDURE BringWindowToTop (hWnd: HWND): BOOL;

<*EXTERNAL IsZoomed:WINAPI*>
PROCEDURE IsZoomed (hWnd: HWND): BOOL;

(* SetWindowPos Flags *)
CONST
  SWP_NOSIZE       = 16_0001;
  SWP_NOMOVE       = 16_0002;
  SWP_NOZORDER     = 16_0004;
  SWP_NOREDRAW     = 16_0008;
  SWP_NOACTIVATE   = 16_0010;
  SWP_FRAMECHANGED = 16_0020;   (* The frame changed: send WM_NCCALCSIZE *)
  SWP_SHOWWINDOW   = 16_0040;
  SWP_HIDEWINDOW   = 16_0080;
  SWP_NOCOPYBITS   = 16_0100;
  SWP_NOOWNERZORDER = 16_0200;  (* Don't do owner Z ordering *)

  SWP_DRAWFRAME    = SWP_FRAMECHANGED;
  SWP_NOREPOSITION = SWP_NOOWNERZORDER;

VAR                             (* CONST *)
  HWND_TOP      : HWND;
  HWND_BOTTOM   : HWND;
  HWND_TOPMOST  : HWND;
  HWND_NOTOPMOST: HWND;

(*
 * WARNING: * The following structures must NOT be UINT32 padded because they are
 * followed by strings, etc that do not have to be UINT32 aligned.
 *)
(*???  #pragma pack(2)

   typedef struct { UINT32 style; UINT32 dwExtendedStyle; UINT16 cdit; UINT16 x;
   UINT16 y; UINT16 cx; UINT16 cy; END; typedef DLGTEMPLATE *LPDLGTEMPLATEA;
   typedef DLGTEMPLATE *LPDLGTEMPLATEW; LPDLGTEMPLATE = LPDLGTEMPLATEA;
   typedef CONST DLGTEMPLATE *LPCDLGTEMPLATEA; typedef CONST DLGTEMPLATE
   *LPCDLGTEMPLATEW; LPCDLGTEMPLATE = LPCDLGTEMPLATEA;

   (* * Dialog item template (dit) *) typedef struct { UINT32 style; UINT32
   dwExtendedStyle; UINT16 x; UINT16 y; UINT16 cx; UINT16 cy; UINT16 id; END; typedef
   DLGITEMTEMPLATE *PDLGITEMTEMPLATEA; typedef DLGITEMTEMPLATE
   *PDLGITEMTEMPLATEW; PDLGITEMTEMPLATE = PDLGITEMTEMPLATEA; typedef
   DLGITEMTEMPLATE *LPDLGITEMTEMPLATEA; typedef DLGITEMTEMPLATE
   *LPDLGITEMTEMPLATEW; LPDLGITEMTEMPLATE = LPDLGITEMTEMPLATEA;

   #pragma pack() // Resume normal packing #endif // !RC_INVOKED *)

(*
 *??? Some surrogate types while we figure out how to do the above
 *)
TYPE
  LPDLGTEMPLATEA = ADDRESS;
  LPDLGTEMPLATEW = ADDRESS;
  LPDLGTEMPLATE = LPDLGTEMPLATEA;
  LPCDLGTEMPLATEA = ADDRESS;
  LPCDLGTEMPLATEW = ADDRESS;
  LPCDLGTEMPLATE = LPCDLGTEMPLATEA;

<*EXTERNAL CreateDialogParamA:WINAPI*>
PROCEDURE CreateDialogParamA (hInstance     : HINSTANCE;
                                lpTemplateName: PCSTR;
                                hWndParent    : HWND;
                                lpDialogFunc  : DLGPROC;
                                dwInitParam   : LPARAM     ): HWND;

<*EXTERNAL CreateDialogParamW:WINAPI*>
PROCEDURE CreateDialogParamW (hInstance     : HINSTANCE;
                                lpTemplateName: PCWSTR;
                                hWndParent    : HWND;
                                lpDialogFunc  : DLGPROC;
                                dwInitParam   : LPARAM     ): HWND;
CONST CreateDialogParam = CreateDialogParamA;

<*EXTERNAL CreateDialogIndirectParamA:WINAPI*>
PROCEDURE CreateDialogIndirectParamA (hInstance   : HINSTANCE;
                                        lpTemplate  : LPCDLGTEMPLATEA;
                                        hwndParent  : HWND;
                                        lpDialogFunc: DLGPROC;
                                        dwInitParam : LPARAM           ): HWND;

<*EXTERNAL CreateDialogIndirectParamW:WINAPI*>
PROCEDURE CreateDialogIndirectParamW (hInstance   : HINSTANCE;
                                        lpTemplate  : LPCDLGTEMPLATEW;
                                        hwndParent  : HWND;
                                        lpDialogFunc: DLGPROC;
                                        dwInitParam : LPARAM           ): HWND;
CONST CreateDialogIndirectParam = CreateDialogIndirectParamA;

PROCEDURE CreateDialogA (hInstance   : HINSTANCE;
                         lpName      : PCSTR;
                         hwndParent  : HWND;
                         lpDialogFunc: DLGPROC    ): HWND;

PROCEDURE CreateDialogW (hInstance   : HINSTANCE;
                         lpName      : PCWSTR;
                         hwndParent  : HWND;
                         lpDialogFunc: DLGPROC    ): HWND;

CONST CreateDialog = CreateDialogA;

PROCEDURE CreateDialogIndirectA (hInstance   : HINSTANCE;
                                 lpTemplate  : LPCDLGTEMPLATEA;
                                 hwndParent  : HWND;
                                 lpDialogFunc: DLGPROC          ): HWND;

PROCEDURE CreateDialogIndirectW (hInstance   : HINSTANCE;
                                 lpTemplate  : LPCDLGTEMPLATEW;
                                 hwndParent  : HWND;
                                 lpDialogFunc: DLGPROC          ): HWND;

CONST CreateDialogIndirect = CreateDialogIndirectA;

<*EXTERNAL DialogBoxParamA:WINAPI*>
PROCEDURE DialogBoxParamA (hInstance     : HINSTANCE;
                             lpTemplateName: PCSTR;
                             hWndParent    : HWND;
                             lpDialogFunc  : DLGPROC;
                             dwInitParam   : LPARAM     ): INT32;

<*EXTERNAL DialogBoxParamW:WINAPI*>
PROCEDURE DialogBoxParamW (hInstance     : HINSTANCE;
                             lpTemplateName: PCWSTR;
                             hWndParent    : HWND;
                             lpDialogFunc  : DLGPROC;
                             dwInitParam   : LPARAM     ): INT32;
CONST DialogBoxParam = DialogBoxParamA;

<*EXTERNAL DialogBoxIndirectParamA:WINAPI*>
PROCEDURE DialogBoxIndirectParamA (hInstance      : HINSTANCE;
                                     hDialogTemplate: LPDLGTEMPLATEA;
                                     hWndParent     : HWND;
                                     lpDialogFunc   : DLGPROC;
                                     dwInitParam    : LPARAM          ): INT32;

<*EXTERNAL DialogBoxIndirectParamW:WINAPI*>
PROCEDURE DialogBoxIndirectParamW (hInstance      : HINSTANCE;
                                     hDialogTemplate: LPDLGTEMPLATEW;
                                     hWndParent     : HWND;
                                     lpDialogFunc   : DLGPROC;
                                     dwInitParam    : LPARAM          ): INT32;
CONST DialogBoxIndirectParam = DialogBoxIndirectParamA;

PROCEDURE DialogBoxA(hInstance     : HINSTANCE;
                             lpTemplateName: PCSTR;
                             hWndParent    : HWND;
                             lpDialogFunc  : DLGPROC): INT32;

PROCEDURE DialogBoxW(hInstance     : HINSTANCE;
                             lpTemplateName: PCWSTR;
                             hWndParent    : HWND;
                             lpDialogFunc  : DLGPROC): INT32;

CONST DialogBox = DialogBoxA;

PROCEDURE DialogBoxIndirectA (hInstance      : HINSTANCE;
                              hDialogTemplate: LPDLGTEMPLATEA;
                              hWndParent     : HWND;
                              lpDialogFunc   : DLGPROC         ): INT32;

PROCEDURE DialogBoxIndirectW (hInstance      : HINSTANCE;
                              hDialogTemplate: LPDLGTEMPLATEW;
                              hWndParent     : HWND;
                              lpDialogFunc   : DLGPROC         ): INT32;

CONST DialogBoxIndirect = DialogBoxIndirectA;

<*EXTERNAL EndDialog:WINAPI*>
PROCEDURE EndDialog (hDlg: HWND; nResult: INT32): BOOL;

<*EXTERNAL GetDlgItem:WINAPI*>
PROCEDURE GetDlgItem (hDlg: HWND; nIDDlgItem: INT32): HWND;

<*EXTERNAL SetDlgItemInt:WINAPI*>
PROCEDURE SetDlgItemInt (hDlg      : HWND;
                           nIDDlgItem: INT32;
                           uValue    : UINT32;
                           bSigned   : BOOL  ): BOOL;

<*EXTERNAL GetDlgItemInt:WINAPI*>
PROCEDURE GetDlgItemInt (hDlg        : HWND;
                           nIDDlgItem  : INT32;
                           lpTranslated: UNTRACED REF BOOL;
                           bSigned     : BOOL               ): UINT32;

<*EXTERNAL SetDlgItemTextA:WINAPI*>
PROCEDURE SetDlgItemTextA (hDlg: HWND; nIDDlgItem: INT32; lpString: PCSTR): BOOL;

<*EXTERNAL SetDlgItemTextW:WINAPI*>
PROCEDURE SetDlgItemTextW (hDlg: HWND; nIDDlgItem: INT32; lpString: PCWSTR): BOOL;
CONST SetDlgItemText = SetDlgItemTextA;

<*EXTERNAL GetDlgItemTextA:WINAPI*>
PROCEDURE GetDlgItemTextA (hDlg      : HWND;
                             nIDDlgItem: INT32;
                             lpString  : PSTR;
                             nMaxCount : INT32    ): UINT32;

<*EXTERNAL GetDlgItemTextW:WINAPI*>
PROCEDURE GetDlgItemTextW (hDlg      : HWND;
                             nIDDlgItem: INT32;
                             lpString  : PWSTR;
                             nMaxCount : INT32     ): UINT32;
CONST GetDlgItemText = GetDlgItemTextA;

<*EXTERNAL CheckDlgButton:WINAPI*>
PROCEDURE CheckDlgButton (hDlg: HWND; nIDButton: INT32; uCheck: UINT32): BOOL;

<*EXTERNAL CheckRadioButton:WINAPI*>
PROCEDURE CheckRadioButton (hDlg          : HWND;
                              nIDFirstButton: INT32;
                              nIDLastButton : INT32;
                              nIDCheckButton: INT32   ): BOOL;

<*EXTERNAL IsDlgButtonChecked:WINAPI*>
PROCEDURE IsDlgButtonChecked (hDlg: HWND; nIDButton: INT32): UINT32;

<*EXTERNAL SendDlgItemMessageA:WINAPI*>
PROCEDURE SendDlgItemMessageA (hDlg      : HWND;
                                 nIDDlgItem: INT32;
                                 Msg       : UINT32;
                                 wParam    : WPARAM;
                                 lParam    : LPARAM  ): INT32;

<*EXTERNAL SendDlgItemMessageW:WINAPI*>
PROCEDURE SendDlgItemMessageW (hDlg      : HWND;
                                 nIDDlgItem: INT32;
                                 Msg       : UINT32;
                                 wParam    : WPARAM;
                                 lParam    : LPARAM  ): INT32;
CONST SendDlgItemMessage = SendDlgItemMessageA;

<*EXTERNAL GetNextDlgGroupItem:WINAPI*>
PROCEDURE GetNextDlgGroupItem (hDlg: HWND; hCtl: HWND; bPrevious: BOOL): HWND;

<*EXTERNAL GetNextDlgTabItem:WINAPI*>
PROCEDURE GetNextDlgTabItem (hDlg: HWND; hCtl: HWND; bPrevious: BOOL): HWND;

<*EXTERNAL GetDlgCtrlID:WINAPI*>
PROCEDURE GetDlgCtrlID (hWnd: HWND): INT32;

<*EXTERNAL GetDialogBaseUnits:WINAPI*>
PROCEDURE GetDialogBaseUnits (): INT32;

<*EXTERNAL DefDlgProcA:WINAPI*>
PROCEDURE DefDlgProcA (hDlg  : HWND;
                       Msg   : UINT32;
                       wParam: WPARAM;
                       lParam: LPARAM  ): LRESULT;

<*EXTERNAL DefDlgProcW:WINAPI*>
PROCEDURE DefDlgProcW (hDlg  : HWND;
                       Msg   : UINT32;
                       wParam: WPARAM;
                       lParam: LPARAM  ): LRESULT;
CONST DefDlgProc = DefDlgProcA;

(*
 * Window extra byted needed for private dialog classes.
 *)
CONST DLGWINDOWEXTRA = 30;

<*EXTERNAL CallMsgFilter:WINAPI*>
PROCEDURE CallMsgFilter (lpMsg: LPMSG; nCode: INT32): BOOL;

(* Clipboard Manager Functions *)

<*EXTERNAL OpenClipboard:WINAPI*>
PROCEDURE OpenClipboard (hWnd: HWND): BOOL;

<*EXTERNAL CloseClipboard:WINAPI*>
PROCEDURE CloseClipboard (): BOOL;

<*EXTERNAL GetClipboardOwner:WINAPI*>
PROCEDURE GetClipboardOwner (): HWND;

<*EXTERNAL SetClipboardViewer:WINAPI*>
PROCEDURE SetClipboardViewer (arg1: HWND): HWND;

<*EXTERNAL GetClipboardViewer:WINAPI*>
PROCEDURE GetClipboardViewer (): HWND;

<*EXTERNAL ChangeClipboardChain:WINAPI*>
PROCEDURE ChangeClipboardChain (arg1: HWND; arg2: HWND): BOOL;

<*EXTERNAL SetClipboardData:WINAPI*>
PROCEDURE SetClipboardData (uFormat: UINT32; hMem: HANDLE): HANDLE;

<*EXTERNAL GetClipboardData:WINAPI*>
PROCEDURE GetClipboardData (uFormat: UINT32): HANDLE;

<*EXTERNAL RegisterClipboardFormatA:WINAPI*>
PROCEDURE RegisterClipboardFormatA (arg1: PCSTR): UINT32;

<*EXTERNAL RegisterClipboardFormatW:WINAPI*>
PROCEDURE RegisterClipboardFormatW (arg1: PCWSTR): UINT32;
CONST RegisterClipboardFormat = RegisterClipboardFormatA;

<*EXTERNAL CountClipboardFormats:WINAPI*>
PROCEDURE CountClipboardFormats (): INT32;

<*EXTERNAL EnumClipboardFormats:WINAPI*>
PROCEDURE EnumClipboardFormats (arg1: UINT32): UINT32;

<*EXTERNAL GetClipboardFormatNameA:WINAPI*>
PROCEDURE GetClipboardFormatNameA (arg1: UINT32; arg2: PSTR; arg3: INT32): INT32;

<*EXTERNAL GetClipboardFormatNameW:WINAPI*>
PROCEDURE GetClipboardFormatNameW (arg1: UINT32; arg2: PWSTR; arg3: INT32): INT32;
CONST GetClipboardFormatName = GetClipboardFormatNameA;

<*EXTERNAL EmptyClipboard:WINAPI*>
PROCEDURE EmptyClipboard (): BOOL;

<*EXTERNAL IsClipboardFormatAvailable:WINAPI*>
PROCEDURE IsClipboardFormatAvailable (arg1: UINT32): BOOL;

<*EXTERNAL GetPriorityClipboardFormat:WINAPI*>
PROCEDURE GetPriorityClipboardFormat (arg1: UNTRACED REF UINT32; arg2: INT32): INT32;

<*EXTERNAL GetOpenClipboardWindow:WINAPI*>
PROCEDURE GetOpenClipboardWindow (): HWND;

(* Character Translation Routines *)

<*EXTERNAL CharToOemA:WINAPI*>
PROCEDURE CharToOemA (arg1: PCSTR; arg2: PSTR): BOOL;

<*EXTERNAL CharToOemW:WINAPI*>
PROCEDURE CharToOemW (arg1: PCWSTR; arg2: PSTR): BOOL;
CONST CharToOem = CharToOemA;

<*EXTERNAL OemToCharA:WINAPI*>
PROCEDURE OemToCharA (arg1: PCSTR; arg2: PSTR): BOOL;

<*EXTERNAL OemToCharW:WINAPI*>
PROCEDURE OemToCharW (arg1: PCSTR; arg2: PWSTR): BOOL;
CONST OemToChar = OemToCharA;

<*EXTERNAL CharToOemBuffA:WINAPI*>
PROCEDURE CharToOemBuffA (arg1: PCSTR; arg2: PSTR; arg3: UINT32): BOOL;

<*EXTERNAL CharToOemBuffW:WINAPI*>
PROCEDURE CharToOemBuffW (arg1: PCWSTR; arg2: PSTR; arg3: UINT32): BOOL;
CONST CharToOemBuff = CharToOemBuffA;

<*EXTERNAL OemToCharBuffA:WINAPI*>
PROCEDURE OemToCharBuffA (arg1: PCSTR; arg2: PSTR; arg3: UINT32): BOOL;

<*EXTERNAL OemToCharBuffW:WINAPI*>
PROCEDURE OemToCharBuffW (arg1: PCSTR; arg2: PWSTR; arg3: UINT32): BOOL;
CONST OemToCharBuff = OemToCharBuffA;

<*EXTERNAL CharUpperA:WINAPI*>
PROCEDURE CharUpperA (arg1: PSTR): PSTR;

<*EXTERNAL CharUpperW:WINAPI*>
PROCEDURE CharUpperW (arg1: PWSTR): PWSTR;
CONST CharUpper = CharUpperA;

<*EXTERNAL CharUpperBuffA:WINAPI*>
PROCEDURE CharUpperBuffA (arg1: PSTR; arg2: UINT32): UINT32;

<*EXTERNAL CharUpperBuffW:WINAPI*>
PROCEDURE CharUpperBuffW (arg1: PWSTR; arg2: UINT32): UINT32;
CONST CharUpperBuff = CharUpperBuffA;

<*EXTERNAL CharLowerA:WINAPI*>
PROCEDURE CharLowerA (arg1: PSTR): PSTR;

<*EXTERNAL CharLowerW:WINAPI*>
PROCEDURE CharLowerW (arg1: PWSTR): PWSTR;
CONST CharLower = CharLowerA;

<*EXTERNAL CharLowerBuffA:WINAPI*>
PROCEDURE CharLowerBuffA (a1: PSTR; a2: UINT32): UINT32;

<*EXTERNAL CharLowerBuffW:WINAPI*>
PROCEDURE CharLowerBuffW (a1: PWSTR; a2: UINT32): UINT32;
CONST CharLowerBuff = CharLowerBuffA;

<*EXTERNAL CharNextA:WINAPI*>
PROCEDURE CharNextA (a1: PCSTR): PSTR;

<*EXTERNAL CharNextW:WINAPI*>
PROCEDURE CharNextW (a1: PCWSTR): PWSTR;
CONST CharNext = CharNextA;

<*EXTERNAL CharPrevA:WINAPI*>
PROCEDURE CharPrevA (a1: PCSTR; a2: PCSTR): PSTR;

<*EXTERNAL CharPrevW:WINAPI*>
PROCEDURE CharPrevW (a1: PCWSTR; a2: PCWSTR): PWSTR;
CONST CharPrev = CharPrevA;

(*| ???
  (* Compatibility defines for character translation routines *)
   #define AnsiToOem CharToOemA
   #define OemToAnsi OemToCharA
   #define AnsiToOemBuff CharToOemBuffA
   #define OemToAnsiBuff OemToCharBuffA
   #define AnsiUpper CharUpperA
   #define AnsiUpperBuff CharUpperBuffA
   #define AnsiLower CharLowerA
   #define AnsiLowerBuff CharLowerBuffA
   #define AnsiNext CharNextA
   #define AnsiPrev CharPrevA
*)

(* Language dependent Routines *)

<*EXTERNAL IsCharAlphaA:WINAPI*>
PROCEDURE IsCharAlphaA (a1: CHAR): BOOL;

<*EXTERNAL IsCharAlphaW:WINAPI*>
PROCEDURE IsCharAlphaW (a1: WCHAR): BOOL;
CONST IsCharAlpha = IsCharAlphaA;

<*EXTERNAL IsCharAlphaNumericA:WINAPI*>
PROCEDURE IsCharAlphaNumericA (a1: CHAR): BOOL;

<*EXTERNAL IsCharAlphaNumericW:WINAPI*>
PROCEDURE IsCharAlphaNumericW (a1: WCHAR): BOOL;
CONST IsCharAlphaNumeric = IsCharAlphaNumericA;

<*EXTERNAL IsCharUpperA:WINAPI*>
PROCEDURE IsCharUpperA (a1: CHAR): BOOL;

<*EXTERNAL IsCharUpperW:WINAPI*>
PROCEDURE IsCharUpperW (a1: WCHAR): BOOL;
CONST IsCharUpper = IsCharUpperA;

<*EXTERNAL IsCharLowerA:WINAPI*>
PROCEDURE IsCharLowerA (a1: CHAR): BOOL;

<*EXTERNAL IsCharLowerW:WINAPI*>
PROCEDURE IsCharLowerW (a1: WCHAR): BOOL;
CONST IsCharLower = IsCharLowerA;

<*EXTERNAL SetFocus:WINAPI*>
PROCEDURE SetFocus (hWnd: HWND): HWND;

<*EXTERNAL GetActiveWindow:WINAPI*>
PROCEDURE GetActiveWindow (): HWND;

<*EXTERNAL GetFocus:WINAPI*>
PROCEDURE GetFocus (): HWND;

<*EXTERNAL GetKBCodePage:WINAPI*>
PROCEDURE GetKBCodePage (): UINT32;

<*EXTERNAL GetKeyState:WINAPI*>
PROCEDURE GetKeyState (nVirtKey: INT32): INT16;

<*EXTERNAL GetAsyncKeyState:WINAPI*>
PROCEDURE GetAsyncKeyState (vKey: INT32): INT16;

<*EXTERNAL GetKeyboardState:WINAPI*>
PROCEDURE GetKeyboardState (lpKeyState: PUINT8): BOOL;

<*EXTERNAL SetKeyboardState:WINAPI*>
PROCEDURE SetKeyboardState (lpKeyState: PUINT8): BOOL;

<*EXTERNAL GetKeyNameTextA:WINAPI*>
PROCEDURE GetKeyNameTextA (lParam: INT32; lpString: PSTR; nSize: INT32): INT32;

<*EXTERNAL GetKeyNameTextW:WINAPI*>
PROCEDURE GetKeyNameTextW (lParam: INT32; lpString: PWSTR; nSize: INT32): INT32;
CONST GetKeyNameText = GetKeyNameTextA;

<*EXTERNAL GetKeyboardType:WINAPI*>
PROCEDURE GetKeyboardType (nTypeFlag: INT32): INT32;

<*EXTERNAL ToAscii:WINAPI*>
PROCEDURE ToAscii (uVirtKey  : UINT32;
                   uScanCode : UINT32;
                   lpKeyState: PUINT8;
                   lpChar    : PUINT16;
                   uFlags    : UINT32    ): INT32;

<*EXTERNAL ToUnicode:WINAPI*>
PROCEDURE ToUnicode (wVirtKey  : UINT32;
                     wScanCode : UINT32;
                     lpKeyState: PUINT8;
                     lpChar    : PUINT32;
                     wFlags    : UINT32     ): INT32;

<*EXTERNAL VkKeyScanA:WINAPI*>
PROCEDURE VkKeyScanA (cChar: CHAR): INT16;

<*EXTERNAL VkKeyScanW:WINAPI*>
PROCEDURE VkKeyScanW (cChar: WCHAR): INT16;

CONST VkKeyScan = VkKeyScanA;

<*EXTERNAL MapVirtualKey:WINAPI*>
PROCEDURE MapVirtualKey (uCode: UINT32; uMapType: UINT32): UINT32;

<*EXTERNAL GetInputState:WINAPI*>
PROCEDURE GetInputState (): BOOL;

<*EXTERNAL GetQueueStatus:WINAPI*>
PROCEDURE GetQueueStatus (flags: UINT32): UINT32;

<*EXTERNAL GetCapture:WINAPI*>
PROCEDURE GetCapture (): HWND;

<*EXTERNAL SetCapture:WINAPI*>
PROCEDURE SetCapture (hWnd: HWND): HWND;

<*EXTERNAL ReleaseCapture:WINAPI*>
PROCEDURE ReleaseCapture (): BOOL;

<*EXTERNAL MsgWaitForMultipleObjects:WINAPI*>
PROCEDURE MsgWaitForMultipleObjects (nCount        : UINT32;
                                     pHandles      : PHANDLE;
                                     fWaitAll      : BOOL;
                                     dwMilliseconds: UINT32;
                                     dwWakeMask    : UINT32     ): UINT32;

(* Queue status flags for GetQueueStatus() and
   MsgWaitForMultipleObjects() *)
CONST
  QS_KEY         = 16_01;
  QS_MOUSEMOVE   = 16_02;
  QS_MOUSEBUTTON = 16_04;
  QS_MOUSE       = Or(QS_MOUSEMOVE, QS_MOUSEBUTTON);
  QS_POSTMESSAGE = 16_08;
  QS_TIMER       = 16_10;
  QS_PAINT       = 16_20;
  QS_SENDMESSAGE = 16_40;
  QS_HOTKEY      = 16_80;
  QS_INPUT       = Or(QS_MOUSE, QS_KEY);

  QS_ALLEVENTS   = Or(QS_INPUT,
                   Or(QS_POSTMESSAGE,
                   Or(QS_TIMER,
                   Or(QS_PAINT,
                      QS_HOTKEY))));

  QS_ALLINPUT    = Or(QS_SENDMESSAGE, 
                   Or(QS_PAINT,
                   Or(QS_TIMER,
                   Or(QS_POSTMESSAGE,
                   Or(QS_MOUSEBUTTON,
                   Or(QS_MOUSEMOVE,
                   Or(QS_HOTKEY,
                      QS_KEY)))))));

<*EXTERNAL GetSysInputMode:WINAPI*>
PROCEDURE GetSysInputMode (): UINT32;

(* GetSysInputMode return values *)
CONST
  IMD_NONE             = 0;
  IMD_MENU             = 1;
  IMD_DIALOGBOX        = 2;
  IMD_NEXTWINDOW       = 3;
  IMD_SCROLLBAR        = 4;
  IMD_TITLEBUTTONTRACK = 5;
  IMD_MOVESIZETRACK    = 6;
  IMD_SYSERRDLG        = 7;
  IMD_DRAGOBJECT       = 8;
  IMD_DRAGDETECT       = 9;

(* Windows Functions *)
<*EXTERNAL SetTimer:WINAPI*>
PROCEDURE SetTimer (hwnd       : HWND;
                    nIDEvent   : SIZE_T;
                    uElapse    : UINT32;
                    lpTimerFunc: TIMERPROC): SIZE_T;

<*EXTERNAL KillTimer:WINAPI*>
PROCEDURE KillTimer (hWnd: HWND; uIDEvent: SIZE_T): BOOL;

<*EXTERNAL IsWindowUnicode:WINAPI*>
PROCEDURE IsWindowUnicode (hWnd: HWND): BOOL;

<*EXTERNAL EnableWindow:WINAPI*>
PROCEDURE EnableWindow (hWnd: HWND; bEnable: BOOL): BOOL;

<*EXTERNAL IsWindowEnabled:WINAPI*>
PROCEDURE IsWindowEnabled (hWnd: HWND): BOOL;

<*EXTERNAL LoadAcceleratorsA:WINAPI*>
PROCEDURE LoadAcceleratorsA (hInstance: HINSTANCE; lpTableName: PCSTR): HACCEL;

<*EXTERNAL LoadAcceleratorsW:WINAPI*>
PROCEDURE LoadAcceleratorsW (hInstance: HINSTANCE; lpTableName: PCWSTR): HACCEL;

CONST LoadAccelerators = LoadAcceleratorsA;

<*EXTERNAL CreateAcceleratorTableA:WINAPI*>
PROCEDURE CreateAcceleratorTable (lpaccl: PACCEL; count: INT32): HACCEL;

<*EXTERNAL DestroyAcceleratorTable:WINAPI*>
PROCEDURE DestroyAcceleratorTable (a1: HACCEL): BOOL;

<*EXTERNAL CopyAcceleratorTable:WINAPI*>
PROCEDURE CopyAcceleratorTable (a1: HACCEL; a2: PACCEL; a3: INT32): INT32;

<*EXTERNAL TranslateAccelerator:WINAPI*>
PROCEDURE TranslateAccelerator (hWnd     : HWND;
                                hAccTable: HACCEL;
                                lpMsg    : LPMSG   ): INT32;

(* GetSystemMetrics() codes *)
CONST
  SM_CXSCREEN          = 0;
  SM_CYSCREEN          = 1;
  SM_CXVSCROLL         = 2;
  SM_CYHSCROLL         = 3;
  SM_CYCAPTION         = 4;
  SM_CXBORDER          = 5;
  SM_CYBORDER          = 6;
  SM_CXDLGFRAME        = 7;
  SM_CYDLGFRAME        = 8;
  SM_CYVTHUMB          = 9;
  SM_CXHTHUMB          = 10;
  SM_CXICON            = 11;
  SM_CYICON            = 12;
  SM_CXCURSOR          = 13;
  SM_CYCURSOR          = 14;
  SM_CYMENU            = 15;
  SM_CXFULLSCREEN      = 16;
  SM_CYFULLSCREEN      = 17;
  SM_CYKANJIWINDOW     = 18;
  SM_MOUSEPRESENT      = 19;
  SM_CYVSCROLL         = 20;
  SM_CXHSCROLL         = 21;
  SM_DEBUG             = 22;
  SM_SWAPBUTTON        = 23;
  SM_RESERVED1         = 24;
  SM_RESERVED2         = 25;
  SM_RESERVED3         = 26;
  SM_RESERVED4         = 27;
  SM_CXMIN             = 28;
  SM_CYMIN             = 29;
  SM_CXSIZE            = 30;
  SM_CYSIZE            = 31;
  SM_CXFRAME           = 32;
  SM_CYFRAME           = 33;
  SM_CXMINTRACK        = 34;
  SM_CYMINTRACK        = 35;
  SM_CXDOUBLECLK       = 36;
  SM_CYDOUBLECLK       = 37;
  SM_CXICONSPACING     = 38;
  SM_CYICONSPACING     = 39;
  SM_MENUDROPALIGNMENT = 40;
  SM_PENWINDOWS        = 41;
  SM_DBCSENABLED       = 42;
  SM_CMOUSEBUTTONS     = 43;
  SM_MAX               = 43;
  SM_CMETRICS          = 44;

<*EXTERNAL GetSystemMetrics:WINAPI*>
PROCEDURE GetSystemMetrics (nIndex: INT32): INT32;

<*EXTERNAL LoadMenuA:WINAPI*>
PROCEDURE LoadMenuA (hInstance: HINSTANCE; lpMenuName: PCSTR): HMENU;

<*EXTERNAL LoadMenuW:WINAPI*>
PROCEDURE LoadMenuW (hInstance: HINSTANCE; lpMenuName: PCWSTR): HMENU;
CONST LoadMenu = LoadMenuA;

<*EXTERNAL LoadMenuIndirectA:WINAPI*>
PROCEDURE LoadMenuIndirectA (lpMenuTemplate: LPMENUTEMPLATE): HMENU;

<*EXTERNAL LoadMenuIndirectW:WINAPI*>
PROCEDURE LoadMenuIndirectW (lpMenuTemplate: LPMENUTEMPLATE): HMENU;
CONST LoadMenuIndirect = LoadMenuIndirectA;

<*EXTERNAL GetMenu:WINAPI*>
PROCEDURE GetMenu (hWnd: HWND): HMENU;

<*EXTERNAL SetMenu:WINAPI*>
PROCEDURE SetMenu (hWnd: HWND; hMenu: HMENU): BOOL;

<*EXTERNAL ChangeMenuA:WINAPI*>
PROCEDURE ChangeMenuA (a1: HMENU;
                       a2: UINT32;
                       a3: PCTSTR;
                       a4: UINT32;
                       a5: UINT32     ): BOOL;

<*EXTERNAL ChangeMenuW:WINAPI*>
PROCEDURE ChangeMenuW (a1: HMENU;
                       a2: UINT32;
                       a3: PCTSTR;
                       a4: UINT32;
                       a5: UINT32     ): BOOL;
CONST ChangeMenu = ChangeMenuA;

<*EXTERNAL HiliteMenuItem:WINAPI*>
PROCEDURE HiliteMenuItem (hWnd         : HWND;
                          hMenu        : HMENU;
                          uIDHiliteItem: UINT32;
                          uHilite      : UINT32   ): BOOL;

<*EXTERNAL GetMenuStringA:WINAPI*>
PROCEDURE GetMenuStringA (hMenu    : HMENU;
                          uIDItem  : UINT32;
                          lpString : PSTR;
                          nMaxCount: INT32;
                          uFlag    : UINT32   ): INT32;

<*EXTERNAL GetMenuStringW:WINAPI*>
PROCEDURE GetMenuStringW (hMenu    : HMENU;
                            uIDItem  : UINT32;
                            lpString : PWSTR;
                            nMaxCount: INT32;
                            uFlag    : UINT32    ): INT32;
CONST GetMenuString = GetMenuStringA;

<*EXTERNAL GetMenuState:WINAPI*>
PROCEDURE GetMenuState (hMenu: HMENU; uId: UINT32; uFlags: UINT32): UINT32;

<*EXTERNAL DrawMenuBar:WINAPI*>
PROCEDURE DrawMenuBar (hWnd: HWND): BOOL;

<*EXTERNAL GetSystemMenu:WINAPI*>
PROCEDURE GetSystemMenu (hWnd: HWND; bRevert: BOOL): HMENU;

<*EXTERNAL CreateMenu:WINAPI*>
PROCEDURE CreateMenu (): HMENU;

<*EXTERNAL CreatePopupMenu:WINAPI*>
PROCEDURE CreatePopupMenu (): HMENU;

<*EXTERNAL DestroyMenu:WINAPI*>
PROCEDURE DestroyMenu (hMenu: HMENU): BOOL;

<*EXTERNAL CheckMenuItem:WINAPI*>
PROCEDURE CheckMenuItem (hMenu: HMENU; uIDCheckItem: UINT32; uCheck: UINT32): BOOL;

<*EXTERNAL EnableMenuItem:WINAPI*>
PROCEDURE EnableMenuItem (hMenu        : HMENU;
                            uIDEnableItem: UINT32;
                            uEnable      : UINT32   ): BOOL;

<*EXTERNAL GetSubMenu:WINAPI*>
PROCEDURE GetSubMenu (hMenu: HMENU; nPos: INT32): HMENU;

<*EXTERNAL GetMenuItemID:WINAPI*>
PROCEDURE GetMenuItemID (hMenu: HMENU; nPos: INT32): UINT32;

<*EXTERNAL GetMenuItemCount:WINAPI*>
PROCEDURE GetMenuItemCount (hMenu: HMENU): INT32;

<*EXTERNAL InsertMenuA:WINAPI*>
PROCEDURE InsertMenuA (hMenu     : HMENU;
                         uPosition : UINT32;
                         uFlags    : UINT32;
                         uIDNewItem: SIZE_T;
                         lpNewItem : PCSTR ): BOOL;

<*EXTERNAL InsertMenuW:WINAPI*>
PROCEDURE InsertMenuW (hMenu     : HMENU;
                         uPosition : UINT32;
                         uFlags    : UINT32;
                         uIDNewItem: SIZE_T;
                         lpNewItem : PCWSTR): BOOL;
CONST InsertMenu = InsertMenuA;

<*EXTERNAL AppendMenuA:WINAPI*>
PROCEDURE AppendMenuA (hMenu     : HMENU;
                         uFlags    : UINT32;
                         uIDNewItem: SIZE_T;
                         lpNewItem : PCSTR ): BOOL;

<*EXTERNAL AppendMenuW:WINAPI*>
PROCEDURE AppendMenuW (hMenu     : HMENU;
                         uFlags    : UINT32;
                         uIDNewItem: SIZE_T;
                         lpNewItem : PCWSTR): BOOL;
CONST AppendMenu = AppendMenuA;

<*EXTERNAL ModifyMenuA:WINAPI*>
PROCEDURE ModifyMenuA (hMnu      : HMENU;
                         uPosition : UINT32;
                         uFlags    : UINT32;
                         uIDNewItem: SIZE_T;
                         lpNewItem : PCSTR ): BOOL;

<*EXTERNAL ModifyMenuW:WINAPI*>
PROCEDURE ModifyMenuW (hMnu      : HMENU;
                         uPosition : UINT32;
                         uFlags    : UINT32;
                         uIDNewItem: SIZE_T;
                         lpNewItem : PCWSTR): BOOL;
CONST ModifyMenu = ModifyMenuA;

<*EXTERNAL RemoveMenu:WINAPI*>
PROCEDURE RemoveMenu (hMenu: HMENU; uPosition: UINT32; uFlags: UINT32): BOOL;

<*EXTERNAL DeleteMenu:WINAPI*>
PROCEDURE DeleteMenu (hMenu: HMENU; uPosition: UINT32; uFlags: UINT32): BOOL;

<*EXTERNAL SetMenuItemBitmaps:WINAPI*>
PROCEDURE SetMenuItemBitmaps (hMenu           : HMENU;
                                uPosition       : UINT32;
                                uFlags          : UINT32;
                                hBitmapUnchecked: HBITMAP;
                                hBitmapChecked  : HBITMAP  ): BOOL;

<*EXTERNAL GetMenuCheckMarkDimensions:WINAPI*>
PROCEDURE GetMenuCheckMarkDimensions (): INT32;

<*EXTERNAL TrackPopupMenu:WINAPI*>
PROCEDURE TrackPopupMenu (hMenu    : HMENU;
                            uFlags   : UINT32;
                            x        : INT32;
                            y        : INT32;
                            nReserved: INT32;
                            hWnd     : HWND;
                            prcRect  : PRECT): BOOL;

(* Flags for TrackPopupMenu *)
CONST
  TPM_LEFTBUTTON : INT32 = 16_0000;
  TPM_RIGHTBUTTON: INT32 = 16_0002;
  TPM_LEFTALIGN  : INT32 = 16_0000;
  TPM_CENTERALIGN: INT32 = 16_0004;
  TPM_RIGHTALIGN : INT32 = 16_0008;

<*EXTERNAL DrawIcon:WINAPI*>
PROCEDURE DrawIcon (a1: HDC; a2: INT32; a3: INT32; a4: HICON): BOOL;

(* DrawText() Format Flags *)
CONST
  DT_TOP             = 16_0000;
  DT_LEFT            = 16_0000;
  DT_CENTER          = 16_0001;
  DT_RIGHT           = 16_0002;
  DT_VCENTER         = 16_0004;
  DT_BOTTOM          = 16_0008;
  DT_WORDBREAK       = 16_0010;
  DT_SINGLELINE      = 16_0020;
  DT_EXPANDTABS      = 16_0040;
  DT_TABSTOP         = 16_0080;
  DT_NOCLIP          = 16_0100;
  DT_EXTERNALLEADING = 16_0200;
  DT_CALCRECT        = 16_0400;
  DT_NOPREFIX        = 16_0800;
  DT_INTERNAL        = 16_1000;

<*EXTERNAL DrawTextA:WINAPI*>
PROCEDURE DrawTextA (hDC     : HDC;
                     lpString: PCSTR;
                     nCount  : INT32;
                     lpRect  : PRECT;
                     uFormat : UINT32    ): INT32;

<*EXTERNAL DrawTextW:WINAPI*>
PROCEDURE DrawTextW (hDC     : HDC;
                     lpString: PCWSTR;
                     nCount  : INT32;
                     lpRect  : PRECT;
                     uFormat : UINT32     ): INT32;
CONST DrawText = DrawTextA;

<*EXTERNAL GrayStringA:WINAPI*>
PROCEDURE GrayStringA (hDC         : HDC;
                         hBrush      : HBRUSH;
                         lpOutputFunc: GRAYSTRINGPROC;
                         lpData      : LPARAM;
                         nCount      : INT32;
                         X           : INT32;
                         Y           : INT32;
                         nWidth      : INT32;
                         nHeight     : INT32             ): BOOL;

<*EXTERNAL GrayStringW:WINAPI*>
PROCEDURE GrayStringW (hDC         : HDC;
                         hBrush      : HBRUSH;
                         lpOutputFunc: GRAYSTRINGPROC;
                         lpData      : LPARAM;
                         nCount      : INT32;
                         X           : INT32;
                         Y           : INT32;
                         nWidth      : INT32;
                         nHeight     : INT32             ): BOOL;
CONST GrayString = GrayStringA;

<*EXTERNAL TabbedTextOutA:WINAPI*>
PROCEDURE TabbedTextOutA (hDC                : HDC;
                            X                  : INT32;
                            Y                  : INT32;
                            lpString           : PCSTR;
                            nCount             : INT32;
                            nTabPositions      : INT32;
                            lpnTabStopPositions: PINT32;
                            nTabOrigin         : INT32     ): INT32;

<*EXTERNAL TabbedTextOutW:WINAPI*>
PROCEDURE TabbedTextOutW (hDC                : HDC;
                            X                  : INT32;
                            Y                  : INT32;
                            lpString           : PCWSTR;
                            nCount             : INT32;
                            nTabPositions      : INT32;
                            lpnTabStopPositions: PINT32;
                            nTabOrigin         : INT32      ): INT32;
CONST TabbedTextOut = TabbedTextOutA;

<*EXTERNAL GetTabbedTextExtentA:WINAPI*>
PROCEDURE GetTabbedTextExtentA (hDC                : HDC;
                                  lpString           : PCSTR;
                                  nCount             : INT32;
                                  nTabPositions      : INT32;
                                  lpnTabStopPositions: PINT32   ): UINT32;

<*EXTERNAL GetTabbedTextExtentW:WINAPI*>
PROCEDURE GetTabbedTextExtentW (hDC                : HDC;
                                  lpString           : PCWSTR;
                                  nCount             : INT32;
                                  nTabPositions      : INT32;
                                  lpnTabStopPositions: PINT32    ): UINT32;
CONST GetTabbedTextExtent = GetTabbedTextExtentA;

<*EXTERNAL UpdateWindow:WINAPI*>
PROCEDURE UpdateWindow (hWnd: HWND): BOOL;

<*EXTERNAL SetActiveWindow:WINAPI*>
PROCEDURE SetActiveWindow (hWnd: HWND): HWND;

<*EXTERNAL GetForegroundWindow:WINAPI*>
PROCEDURE GetForegroundWindow (): HWND;

<*EXTERNAL SetForegroundWindow:WINAPI*>
PROCEDURE SetForegroundWindow (hWnd: HWND): BOOL;

<*EXTERNAL WindowFromDC:WINAPI*>
PROCEDURE WindowFromDC (hdc: HDC): HWND;

<*EXTERNAL GetDC:WINAPI*>
PROCEDURE GetDC (hWnd: HWND): HDC;

<*EXTERNAL GetDCEx:WINAPI*>
PROCEDURE GetDCEx (hwnd: HWND; hrgnClip: HRGN; flags: UINT32): HDC;

(* GetDCEx() flags *)
CONST
  DCX_WINDOW      : INT32 = 16_00000001;
  DCX_CACHE       : INT32 = 16_00000002;
  DCX_NORESETATTRS: INT32 = 16_00000004;
  DCX_CLIPCHILDREN: INT32 = 16_00000008;
  DCX_CLIPSIBLINGS: INT32 = 16_00000010;
  DCX_PARENTCLIP  : INT32 = 16_00000020;

  DCX_EXCLUDERGN  : INT32 = 16_00000040;
  DCX_INTERSECTRGN: INT32 = 16_00000080;

  DCX_EXCLUDEUPDATE  : INT32 = 16_00000100;
  DCX_INTERSECTUPDATE: INT32 = 16_00000200;

  DCX_LOCKWINDOWUPDATE: INT32 = 16_00000400;

  DCX_USESTYLE   : INT32 = 16_00010000;
  DCX_NORECOMPUTE: INT32 = 16_00100000;
  DCX_VALIDATE   : INT32 = 16_00200000;

<*EXTERNAL GetWindowDC:WINAPI*>
PROCEDURE GetWindowDC (hWnd: HWND): HDC;

<*EXTERNAL ReleaseDC:WINAPI*>
PROCEDURE ReleaseDC (hWnd: HWND; hDC: HDC): INT32;

<*EXTERNAL BeginPaint:WINAPI*>
PROCEDURE BeginPaint (hWnd: HWND; lpPaint: LPPAINTSTRUCT): HDC;

<*EXTERNAL EndPaint:WINAPI*>
PROCEDURE EndPaint (hWnd: HWND; lpPaint: UNTRACED REF PAINTSTRUCT): BOOL;

<*EXTERNAL GetUpdateRect:WINAPI*>
PROCEDURE GetUpdateRect (hWnd: HWND; lpRect: PRECT; bErase: BOOL): BOOL;

<*EXTERNAL GetUpdateRgn:WINAPI*>
PROCEDURE GetUpdateRgn (hWnd: HWND; hRgn: HRGN; bErase: BOOL): INT32;

<*EXTERNAL ExcludeUpdateRgn:WINAPI*>
PROCEDURE ExcludeUpdateRgn (hDC: HDC; hWnd: HWND): INT32;

<*EXTERNAL InvalidateRect:WINAPI*>
PROCEDURE InvalidateRect (hWnd  : HWND;
                            lpRect: PRECT;
                            bErase: BOOL               ): BOOL;

<*EXTERNAL ValidateRect:WINAPI*>
PROCEDURE ValidateRect (hWnd: HWND; lpRect: PRECT): BOOL;

<*EXTERNAL InvalidateRgn:WINAPI*>
PROCEDURE InvalidateRgn (hWnd: HWND; hRgn: HRGN; bErase: BOOL): BOOL;

<*EXTERNAL ValidateRgn:WINAPI*>
PROCEDURE ValidateRgn (hWnd: HWND; hRgn: HRGN): BOOL;

<*EXTERNAL RedrawWindow:WINAPI*>
PROCEDURE RedrawWindow (hwnd      : HWND;
                          lprcUpdate: PRECT;
                          hrgnUpdate: HRGN;
                          flags     : UINT32               ): BOOL;

(* RedrawWindow() flags *)
CONST
  RDW_INVALIDATE    = 16_0001;
  RDW_INTERNALPAINT = 16_0002;
  RDW_ERASE         = 16_0004;

  RDW_VALIDATE        = 16_0008;
  RDW_NOINTERNALPAINT = 16_0010;
  RDW_NOERASE         = 16_0020;

  RDW_NOCHILDREN  = 16_0040;
  RDW_ALLCHILDREN = 16_0080;

  RDW_UPDATENOW = 16_0100;
  RDW_ERASENOW  = 16_0200;

(* LockWindowUpdate API *)
<*EXTERNAL LockWindowUpdate:WINAPI*>
PROCEDURE LockWindowUpdate (hwndLock: HWND): BOOL;

<*EXTERNAL ScrollWindow:WINAPI*>
PROCEDURE ScrollWindow (hWnd      : HWND;
                          XAmount   : INT32;
                          YAmount   : INT32;
                          lpRect    : PRECT;
                          lpClipRect: PRECT  ): BOOL;

<*EXTERNAL ScrollDC:WINAPI*>
PROCEDURE ScrollDC (hDC       : HDC;
                      dx        : INT32;
                      dy        : INT32;
                      lprcScroll: PRECT;
                      lprcClip  : PRECT;
                      hrgnUpdate: HRGN;
                      lprcUpdate: PRECT             ): BOOL;

<*EXTERNAL ScrollWindowEx:WINAPI*>
PROCEDURE ScrollWindowEx (hwnd      : HWND;
                            dx        : INT32;
                            dy        : INT32;
                            prcScroll : PRECT;
                            prcClip   : PRECT;
                            hrgnUpdate: HRGN;
                            prcUpdate : PRECT;
                            flags     : UINT32               ): INT32;

CONST
  SW_SCROLLCHILDREN = 16_0001;  (* Scroll children within *lprcScroll. *)
  SW_INVALIDATE     = 16_0002;  (* Invalidate after scrolling *)
  SW_ERASE = 16_0004;           (* If SW_INVALIDATE, don't send
                                   WM_ERASEBACKGROUND *)

<*EXTERNAL SetScrollPos:WINAPI*>
PROCEDURE SetScrollPos (hWnd: HWND; nBar: INT32; nPos: INT32; bRedraw: BOOL): INT32;

<*EXTERNAL GetScrollPos:WINAPI*>
PROCEDURE GetScrollPos (hWnd: HWND; nBar: INT32): INT32;

<*EXTERNAL SetScrollRange:WINAPI*>
PROCEDURE SetScrollRange (hWnd   : HWND;
                            nBar   : INT32;
                            nMinPos: INT32;
                            nMaxPos: INT32;
                            bRedraw: BOOL  ): BOOL;

<*EXTERNAL GetScrollRange:WINAPI*>
PROCEDURE GetScrollRange (hWnd    : HWND;
                            nBar    : INT32;
                            lpMinPos: PINT32;
                            lpMaxPos: PINT32  ): BOOL;

<*EXTERNAL ShowScrollBar:WINAPI*>
PROCEDURE ShowScrollBar (hWnd: HWND; wBar: INT32; bShow: BOOL): BOOL;

<*EXTERNAL EnableScrollBar:WINAPI*>
PROCEDURE EnableScrollBar (hwnd: HWND; wSBflags: UINT32; wArrows: UINT32): BOOL;

(* EnableScrollBar() flags *)
CONST
  ESB_ENABLE_BOTH  = 16_0000;
  ESB_DISABLE_BOTH = 16_0003;

  ESB_DISABLE_LEFT  = 16_0001;
  ESB_DISABLE_RIGHT = 16_0002;

  ESB_DISABLE_UP   = 16_0001;
  ESB_DISABLE_DOWN = 16_0002;

  ESB_DISABLE_LTUP = ESB_DISABLE_LEFT;
  ESB_DISABLE_RTDN = ESB_DISABLE_RIGHT;

<*EXTERNAL SetPropA:WINAPI*>
PROCEDURE SetPropA (hWnd: HWND; lpString: PCSTR; hData: HANDLE): BOOL;

<*EXTERNAL SetPropW:WINAPI*>
PROCEDURE SetPropW (hWnd: HWND; lpString: PCWSTR; hData: HANDLE): BOOL;
CONST SetProp = SetPropA;

<*EXTERNAL GetPropA:WINAPI*>
PROCEDURE GetPropA (hWnd: HWND; lpString: PCSTR): HANDLE;

<*EXTERNAL GetPropW:WINAPI*>
PROCEDURE GetPropW (hWnd: HWND; lpString: PCWSTR): HANDLE;
CONST GetProp = GetPropA;

<*EXTERNAL RemovePropA:WINAPI*>
PROCEDURE RemovePropA (hWnd: HWND; lpString: PCSTR): HANDLE;

<*EXTERNAL RemovePropW:WINAPI*>
PROCEDURE RemovePropW (hWnd: HWND; lpString: PCWSTR): HANDLE;
CONST RemoveProp = RemovePropA;

<*EXTERNAL EnumPropsExA:WINAPI*>
PROCEDURE EnumPropsExA (hWnd      : HWND;
                          lpEnumFunc: PROPENUMPROC;
                          lParam    : LPARAM        ): INT32;

<*EXTERNAL EnumPropsExW:WINAPI*>
PROCEDURE EnumPropsExW (hWnd      : HWND;
                          lpEnumFunc: PROPENUMPROC;
                          lParam    : LPARAM        ): INT32;
CONST EnumPropsEx = EnumPropsExA;

<*EXTERNAL EnumPropsA:WINAPI*>
PROCEDURE EnumPropsA (hWnd: HWND; lpEnumFunc: PROPENUMPROC): INT32;

<*EXTERNAL EnumPropsW:WINAPI*>
PROCEDURE EnumPropsW (hWnd: HWND; lpEnumFunc: PROPENUMPROC): INT32;
CONST EnumProps = EnumPropsA;

<*EXTERNAL SetWindowTextA:WINAPI*>
PROCEDURE SetWindowTextA (hWnd: HWND; lpString: PCSTR): BOOL;

<*EXTERNAL SetWindowTextW:WINAPI*>
PROCEDURE SetWindowTextW (hWnd: HWND; lpString: PCWSTR): BOOL;
CONST SetWindowText = SetWindowTextA;

<*EXTERNAL GetWindowTextA:WINAPI*>
PROCEDURE GetWindowTextA (hWnd: HWND; lpString: PSTR; nMaxCount: INT32): INT32;

<*EXTERNAL GetWindowTextW:WINAPI*>
PROCEDURE GetWindowTextW (hWnd: HWND; lpString: PWSTR; nMaxCount: INT32): INT32;
CONST GetWindowText = GetWindowTextA;

<*EXTERNAL GetWindowTextLengthA:WINAPI*>
PROCEDURE GetWindowTextLengthA (hWnd: HWND): INT32;

<*EXTERNAL GetWindowTextLengthW:WINAPI*>
PROCEDURE GetWindowTextLengthW (hWnd: HWND): INT32;
CONST GetWindowTextLength = GetWindowTextLengthA;

<*EXTERNAL GetClientRect:WINAPI*>
PROCEDURE raw_GetClientRect (hWnd: HWND; lpRect: PRECT): BOOL;

PROCEDURE GetClientRect (hWnd: HWND; lpRect: PRECT): BOOL;

<*EXTERNAL GetWindowRect:WINAPI*>
PROCEDURE GetWindowRect (hWnd: HWND; lpRect: PRECT): BOOL;

<*EXTERNAL AdjustWindowRect:WINAPI*>
PROCEDURE AdjustWindowRect (lpRect: PRECT; dwStyle: UINT32; bMenu: BOOL): BOOL;

<*EXTERNAL AdjustWindowRectEx:WINAPI*>
PROCEDURE AdjustWindowRectEx (lpRect   : PRECT;
                                dwStyle  : UINT32;
                                bMenu    : BOOL;
                                dwExStyle: UINT32   ): BOOL;

(* MessageBox() Flags *)
CONST
  MB_OK              : INT32 = 16_0000;
  MB_OKCANCEL        : INT32 = 16_0001;
  MB_ABORTRETRYIGNORE: INT32 = 16_0002;
  MB_YESNOCANCEL     : INT32 = 16_0003;
  MB_YESNO           : INT32 = 16_0004;
  MB_RETRYCANCEL     : INT32 = 16_0005;

  MB_ICONHAND       : INT32 = 16_0010;
  MB_ICONQUESTION   : INT32 = 16_0020;
  MB_ICONEXCLAMATION: INT32 = 16_0030;
  MB_ICONASTERISK   : INT32 = 16_0040;

  MB_ICONINFORMATION = MB_ICONASTERISK;
  MB_ICONSTOP        = MB_ICONHAND;

  MB_DEFBUTTON1: INT32 = 16_0000;
  MB_DEFBUTTON2: INT32 = 16_0100;
  MB_DEFBUTTON3: INT32 = 16_0200;

  MB_APPLMODAL  : INT32 = 16_0000;
  MB_SYSTEMMODAL: INT32 = 16_1000;
  MB_TASKMODAL  : INT32 = 16_2000;

  MB_NOFOCUS             : INT32 = 16_8000;
  MB_SETFOREGROUND       : INT32 = 16_10000;
  MB_DEFAULT_DESKTOP_ONLY: INT32 = 16_20000;

  MB_TYPEMASK: INT32 = 16_000F;
  MB_ICONMASK: INT32 = 16_00F0;
  MB_DEFMASK : INT32 = 16_0F00;
  MB_MODEMASK: INT32 = 16_3000;
  MB_MISCMASK: INT32 = 16_C000;

<*EXTERNAL MessageBoxExA:WINAPI*>
PROCEDURE MessageBoxExA (hWnd       : HWND;
                           lpText     : PCSTR;
                           lpCaption  : PCSTR;
                           uType      : UINT32;
                           wLanguageId: UINT16    ): INT32;

<*EXTERNAL MessageBoxExW:WINAPI*>
PROCEDURE MessageBoxExW (hWnd       : HWND;
                           lpText     : PCWSTR;
                           lpCaption  : PCWSTR;
                           uType      : UINT32;
                           wLanguageId: UINT16     ): INT32;
CONST MessageBoxEx = MessageBoxExA;

PROCEDURE MessageBoxA (hWnd     : HWND;
                       lpText   : PCSTR;
                       lpCaption: PCSTR;
                       uType    : UINT32    ): INT32;

PROCEDURE MessageBoxW (hWnd     : HWND;
                       lpText   : PCWSTR;
                       lpCaption: PCWSTR;
                       uType    : UINT32     ): INT32;

CONST MessageBox = MessageBoxA;

<*EXTERNAL MessageBeep:WINAPI*>
PROCEDURE MessageBeep (uType: UINT32): BOOL;

<*EXTERNAL ShowCursor:WINAPI*>
PROCEDURE ShowCursor (bShow: BOOL): INT32;

<*EXTERNAL SetCursorPos:WINAPI*>
PROCEDURE SetCursorPos (X: INT32; Y: INT32): BOOL;

<*EXTERNAL SetCursor:WINAPI*>
PROCEDURE SetCursor (hCursor: HCURSOR): HCURSOR;

<*EXTERNAL GetCursorPos:WINAPI*>
PROCEDURE raw_GetCursorPos (lpPoint: PPOINT): BOOL;

PROCEDURE GetCursorPos (lpPoint: PPOINT): BOOL;

<*EXTERNAL ClipCursor:WINAPI*>
PROCEDURE ClipCursor (lpRect: PRECT): BOOL;

<*EXTERNAL GetClipCursor:WINAPI*>
PROCEDURE GetClipCursor (lpRect: PRECT): BOOL;

<*EXTERNAL GetCursor:WINAPI*>
PROCEDURE GetCursor (): HCURSOR;

<*EXTERNAL CreateCaret:WINAPI*>
PROCEDURE CreateCaret (hWnd   : HWND;
                       hBitmap: HBITMAP;
                       nWidth : INT32;
                       nHeight: INT32      ): BOOL;

<*EXTERNAL GetCaretBlinkTime:WINAPI*>
PROCEDURE GetCaretBlinkTime (): UINT32;

<*EXTERNAL SetCaretBlinkTime:WINAPI*>
PROCEDURE SetCaretBlinkTime (uMSeconds: UINT32): BOOL;

<*EXTERNAL DestroyCaret:WINAPI*>
PROCEDURE DestroyCaret (): BOOL;

<*EXTERNAL HideCaret:WINAPI*>
PROCEDURE HideCaret (hWnd: HWND): BOOL;

<*EXTERNAL ShowCaret:WINAPI*>
PROCEDURE ShowCaret (hWnd: HWND): BOOL;

<*EXTERNAL SetCaretPos:WINAPI*>
PROCEDURE SetCaretPos (X: INT32; Y: INT32): BOOL;

<*EXTERNAL GetCaretPos:WINAPI*>
PROCEDURE GetCaretPos (lpPoint: PPOINT): BOOL;

<*EXTERNAL ClientToScreen:WINAPI*>
PROCEDURE raw_ClientToScreen (hWnd: HWND; lpPoint: PPOINT): BOOL;

PROCEDURE ClientToScreen (hWnd: HWND; lpPoint: PPOINT): BOOL;

<*EXTERNAL ScreenToClient:WINAPI*>
PROCEDURE raw_ScreenToClient (hWnd: HWND; lpPoint: PPOINT): BOOL;

PROCEDURE ScreenToClient (hWnd: HWND; lpPoint: PPOINT): BOOL;

<*EXTERNAL MapWindowPoints:WINAPI*>
PROCEDURE MapWindowPoints (hWndFrom: HWND;
                             hWndTo  : HWND;
                             lpPoints: PPOINT;
                             cPoints : UINT32     ): INT32;

<*EXTERNAL WindowFromPoint:WINAPI*>
PROCEDURE WindowFromPoint (Point: POINT): HWND;

<*EXTERNAL ChildWindowFromPoint:WINAPI*>
PROCEDURE ChildWindowFromPoint (hWndParent: HWND; Point: POINT): HWND;

(* Color Types *)
CONST
  CTLCOLOR_MSGBOX    = 0;
  CTLCOLOR_EDIT      = 1;
  CTLCOLOR_LISTBOX   = 2;
  CTLCOLOR_BTN       = 3;
  CTLCOLOR_DLG       = 4;
  CTLCOLOR_SCROLLBAR = 5;
  CTLCOLOR_STATIC    = 6;
  CTLCOLOR_MAX       = 8;       (* three bits max *)

  COLOR_SCROLLBAR           = 0;
  COLOR_BACKGROUND          = 1;
  COLOR_ACTIVECAPTION       = 2;
  COLOR_INACTIVECAPTION     = 3;
  COLOR_MENU                = 4;
  COLOR_WINDOW              = 5;
  COLOR_WINDOWFRAME         = 6;
  COLOR_MENUTEXT            = 7;
  COLOR_WINDOWTEXT          = 8;
  COLOR_CAPTIONTEXT         = 9;
  COLOR_ACTIVEBORDER        = 10;
  COLOR_INACTIVEBORDER      = 11;
  COLOR_APPWORKSPACE        = 12;
  COLOR_HIGHLIGHT           = 13;
  COLOR_HIGHLIGHTTEXT       = 14;
  COLOR_BTNFACE             = 15;
  COLOR_BTNSHADOW           = 16;
  COLOR_GRAYTEXT            = 17;
  COLOR_BTNTEXT             = 18;
  COLOR_INACTIVECAPTIONTEXT = 19;
  COLOR_BTNHIGHLIGHT        = 20;
  COLOR_ENDCOLORS           = COLOR_BTNHIGHLIGHT;
  COLOR_MAX                 = 20;

<*EXTERNAL GetSysColor:WINAPI*>
PROCEDURE GetSysColor (nIndex: INT32): UINT32;

<*EXTERNAL SetSysColors:WINAPI*>
PROCEDURE SetSysColors (a1: INT32;
                          a2: UNTRACED REF INT32;
                          a3: UNTRACED REF COLORREF): BOOL;

<*EXTERNAL DrawFocusRect:WINAPI*>
PROCEDURE DrawFocusRect (a1: HDC; a2: PRECT): BOOL;

<*EXTERNAL FillRect:WINAPI*>
PROCEDURE FillRect (hdc: HDC; lprc: PRECT; hbr: HBRUSH): INT32;

<*EXTERNAL FrameRect:WINAPI*>
PROCEDURE FrameRect (hdc: HDC; lprc: PRECT; hbr: HBRUSH): INT32;

<*EXTERNAL InvertRect:WINAPI*>
PROCEDURE InvertRect (hdc: HDC; lprc: PRECT): BOOL;

<*EXTERNAL SetRect:WINAPI*>
PROCEDURE SetRect (a1: PRECT; a2: INT32; a3: INT32; a4: INT32; a5: INT32): BOOL;

<*EXTERNAL SetRectEmpty:WINAPI*>
PROCEDURE SetRectEmpty (a1: PRECT): BOOL;

<*EXTERNAL CopyRect:WINAPI*>
PROCEDURE CopyRect (a1: PRECT; a2: PRECT): INT32;

<*EXTERNAL InflateRect:WINAPI*>
PROCEDURE InflateRect (a1: PRECT; a2: INT32; a3: INT32): BOOL;

<*EXTERNAL IntersectRect:WINAPI*>
PROCEDURE IntersectRect (a1: PRECT;
                           a2: PRECT;
                           a3: PRECT  ): INT32;

<*EXTERNAL UnionRect:WINAPI*>
PROCEDURE UnionRect (a1: PRECT;
                       a2: PRECT;
                       a3: PRECT  ): BOOL;

<*EXTERNAL SubtractRect:WINAPI*>
PROCEDURE SubtractRect (a1: PRECT;
                          a2: PRECT;
                          a3: PRECT  ): BOOL;

<*EXTERNAL OffsetRect:WINAPI*>
PROCEDURE OffsetRect (a1: PRECT; a2: INT32; a3: INT32): BOOL;

<*EXTERNAL IsRectEmpty:WINAPI*>
PROCEDURE IsRectEmpty (lprc: PRECT): BOOL;

<*EXTERNAL EqualRect:WINAPI*>
PROCEDURE EqualRect (a1: PRECT; a2: PRECT): BOOL;

<*EXTERNAL PtInRect:WINAPI*>
PROCEDURE PtInRect (a1: PRECT; a2: POINT): BOOL;

<*EXTERNAL GetWindowWord:WINAPI*>
PROCEDURE GetWindowWord (hWnd: HWND; nIndex: INT32): UINT16;

<*EXTERNAL SetWindowWord:WINAPI*>
PROCEDURE SetWindowWord (hWnd: HWND; nIndex: INT32; wNewWord: UINT16): UINT16;

<*EXTERNAL GetWindowLongA:WINAPI*>
PROCEDURE GetWindowLongA (hWnd: HWND; nIndex: INT32): INT32;

<*EXTERNAL GetWindowLongW:WINAPI*>
PROCEDURE GetWindowLongW (hWnd: HWND; nIndex: INT32): INT32;
CONST GetWindowLong = GetWindowLongA;

<*EXTERNAL SetWindowLongA:WINAPI*>
PROCEDURE SetWindowLongA (hWnd: HWND; nIndex: INT32; dwNewLong: INT32): INT32;

<*EXTERNAL SetWindowLongW:WINAPI*>
PROCEDURE SetWindowLongW (hWnd: HWND; nIndex: INT32; dwNewLong: INT32): INT32;
CONST SetWindowLong = SetWindowLongA;

(*

<*EXTERNAL "m3_GetWindowLongPtrA"*>
PROCEDURE GetWindowLongPtrA (hWnd: HWND; nIndex: INT32): SSIZE_T;

<*EXTERNAL "m3_GetWindowLongPtrW"*>
PROCEDURE GetWindowLongPtrW (hWnd: HWND; nIndex: INT32): SSIZE_T;

CONST GetWindowLongPtr = GetWindowLongPtrA;

<*EXTERNAL "m3_SetWindowLongPtrA"*>
PROCEDURE SetWindowLongPtrA (hWnd: HWND; nIndex: INT32; dwNewLong: SSIZE_T): SSIZE_T;

<*EXTERNAL "m3_SetWindowLongPtrW"*>
PROCEDURE SetWindowLongPtrW (hWnd: HWND; nIndex: INT32; dwNewLong: SSIZE_T): SSIZE_T;

CONST SetWindowLongPtr = SetWindowLongPtrA;

*)

<*EXTERNAL GetClassWord:WINAPI*>
PROCEDURE GetClassWord (hWnd: HWND; nIndex: INT32): UINT16;

<*EXTERNAL SetClassWord:WINAPI*>
PROCEDURE SetClassWord (hWnd: HWND; nIndex: INT32; wNewWord: UINT16): UINT16;

<*EXTERNAL GetClassLongA:WINAPI*>
PROCEDURE GetClassLongA (hWnd: HWND; nIndex: INT32): UINT32;

<*EXTERNAL GetClassLongW:WINAPI*>
PROCEDURE GetClassLongW (hWnd: HWND; nIndex: INT32): UINT32;
CONST GetClassLong = GetClassLongA;

<*EXTERNAL SetClassLongA:WINAPI*>
PROCEDURE SetClassLongA (hWnd: HWND; nIndex: INT32; dwNewLong: INT32): UINT32;

<*EXTERNAL SetClassLongW:WINAPI*>
PROCEDURE SetClassLongW (hWnd: HWND; nIndex: INT32; dwNewLong: INT32): UINT32;
CONST SetClassLong = SetClassLongA;


(*

<*EXTERNAL "m3_GetClassLongPtrA"*>
PROCEDURE GetClassLongPtrA (hWnd: HWND; nIndex: INT32): SIZE_T;

<*EXTERNAL "m3_GetClassLongPtrW"*>
PROCEDURE GetClassLongPtrW (hWnd: HWND; nIndex: INT32): SIZE_T;

CONST GetClassLongPtr = GetClassLongPtrA;

<*EXTERNAL "m3_SetClassLongPtrA"*>
PROCEDURE SetClassLongPtrA (hWnd: HWND; nIndex: INT32; dwNewLong: SSIZE_T): SIZE_T;

<*EXTERNAL "m3_SetClassLongPtrW"*>
PROCEDURE SetClassLongW (hWnd: HWND; nIndex: INT32; dwNewLong: SSIZE_T): SIZE_T;

CONST SetClassLongPtr = SetClassLongPtrA;

*)


<*EXTERNAL GetDesktopWindow:WINAPI*>
PROCEDURE GetDesktopWindow (): HWND;

<*EXTERNAL SetDeskWallpaper:WINAPI*>
PROCEDURE SetDeskWallpaper (lpString: PCSTR): BOOL;

<*EXTERNAL GetParent:WINAPI*>
PROCEDURE GetParent (hWnd: HWND): HWND;

<*EXTERNAL SetParent:WINAPI*>
PROCEDURE SetParent (hWndChild: HWND; hWndNewParent: HWND): HWND;

<*EXTERNAL EnumChildWindows:WINAPI*>
PROCEDURE EnumChildWindows (hWndParent: HWND;
                              lpEnumFunc: WNDENUMPROC;
                              lParam    : LPARAM       ): BOOL;

<*EXTERNAL FindWindowA:WINAPI*>
PROCEDURE FindWindowA (lpClassName: PCSTR; lpWindowName: PCSTR): HWND;

<*EXTERNAL FindWindowW:WINAPI*>
PROCEDURE FindWindowW (lpClassName: PCWSTR; lpWindowName: PCWSTR): HWND;
CONST FindWindow = FindWindowA;

<*EXTERNAL EnumWindows:WINAPI*>
PROCEDURE EnumWindows (lpEnumFunc: WNDENUMPROC; lParam: LPARAM): BOOL;

<*EXTERNAL EnumThreadWindows:WINAPI*>
PROCEDURE EnumThreadWindows (dwThreadId: UINT32;
                               lpfn      : WNDENUMPROC;
                               lParam    : LPARAM       ): BOOL;

PROCEDURE EnumTaskWindows (dwThreadId: UINT32;
                           lpfn      : WNDENUMPROC;
                           lParam    : LPARAM       ): BOOL;

<*EXTERNAL GetClassNameA:WINAPI*>
PROCEDURE GetClassNameA (hWnd: HWND; lpClassName: PSTR; nMaxCount: INT32): INT32;

<*EXTERNAL GetClassNameW:WINAPI*>
PROCEDURE GetClassNameW (hWnd: HWND; lpClassName: PWSTR; nMaxCount: INT32): INT32;
CONST GetClassName = GetClassNameA;

<*EXTERNAL GetTopWindow:WINAPI*>
PROCEDURE GetTopWindow (hWnd: HWND): HWND;

PROCEDURE GetNextWindow(hWnd: HWND; uCmd: UINT32):HWND;

<*EXTERNAL GetWindowThreadProcessId:WINAPI*>
PROCEDURE GetWindowThreadProcessId (hWnd: HWND; lpdwProcessId: PUINT32): UINT32;

<*EXTERNAL GetLastActivePopup:WINAPI*>
PROCEDURE GetLastActivePopup (hWnd: HWND): HWND;

(* GetWindow() Constants *)
CONST
  GW_HWNDFIRST = 0;
  GW_HWNDLAST  = 1;
  GW_HWNDNEXT  = 2;
  GW_HWNDPREV  = 3;
  GW_OWNER     = 4;
  GW_CHILD     = 5;
  GW_MAX       = 5;

<*EXTERNAL GetWindow:WINAPI*>
PROCEDURE GetWindow (hWnd: HWND; uCmd: UINT32): HWND;

<*EXTERNAL SetWindowsHookA:WINAPI*>
PROCEDURE SetWindowsHookA (nFilterType: INT32; pfnFilterProc: HOOKPROC): HHOOK;

<*EXTERNAL SetWindowsHookW:WINAPI*>
PROCEDURE SetWindowsHookW (nFilterType: INT32; fnFilterProc: HOOKPROC): HHOOK;
CONST SetWindowsHook = SetWindowsHookA;

<*EXTERNAL UnhookWindowsHook:WINAPI*>
PROCEDURE UnhookWindowsHook (nCode: INT32; pfnFilterProc: HOOKPROC): BOOL;

<*EXTERNAL SetWindowsHookExA:WINAPI*>
PROCEDURE SetWindowsHookExA (idHook    : INT32;
                               lpfn      : HOOKPROC;
                               hmod      : HINSTANCE;
                               dwThreadId: UINT32      ): HHOOK;

<*EXTERNAL SetWindowsHookExW:WINAPI*>
PROCEDURE SetWindowsHookExW (idHook    : INT32;
                               lpfn      : HOOKPROC;
                               hmod      : HINSTANCE;
                               dwThreadId: UINT32      ): HHOOK;
CONST SetWindowsHookEx = SetWindowsHookExA;

<*EXTERNAL UnhookWindowsHookEx:WINAPI*>
PROCEDURE UnhookWindowsHookEx (hhk: HHOOK): BOOL;

<*EXTERNAL CallNextHookEx:WINAPI*>
PROCEDURE CallNextHookEx (hhk   : HHOOK;
                            nCode : INT32;
                            wParam: WPARAM;
                            lParam: LPARAM  ): LRESULT;

(*
 * Macros for source-level compatibility with old functions.
 *)

PROCEDURE DefHookProc (nCode : INT32;
                       wParam: WPARAM;
                       lParam: LPARAM;
                       phhk  : UNTRACED REF HHOOK): LRESULT;

(* Menu flags for Add/Check/EnableMenuItem() *)
CONST
  MF_INSERT: INT32 = 16_00000000;
  MF_CHANGE: INT32 = 16_00000080;
  MF_APPEND: INT32 = 16_00000100;
  MF_DELETE: INT32 = 16_00000200;
  MF_REMOVE: INT32 = 16_00001000;

  MF_BYCOMMAND : INT32 = 16_00000000;
  MF_BYPOSITION: INT32 = 16_00000400;

  MF_SEPARATOR: INT32 = 16_00000800;

  MF_ENABLED : INT32 = 16_00000000;
  MF_GRAYED  : INT32 = 16_00000001;
  MF_DISABLED: INT32 = 16_00000002;

  MF_UNCHECKED      : INT32 = 16_00000000;
  MF_CHECKED        : INT32 = 16_00000008;
  MF_USECHECKBITMAPS: INT32 = 16_00000200;

  MF_STRING   : INT32 = 16_00000000;
  MF_BITMAP   : INT32 = 16_00000004;
  MF_OWNERDRAW: INT32 = 16_00000100;

  MF_POPUP       : INT32 = 16_00000010;
  MF_MENUBARBREAK: INT32 = 16_00000020;
  MF_MENUBREAK   : INT32 = 16_00000040;

  MF_UNHILITE: INT32 = 16_00000000;
  MF_HILITE  : INT32 = 16_00000080;

  MF_SYSMENU    : INT32 = 16_00002000;
  MF_HELP       : INT32 = 16_00004000;
  MF_MOUSESELECT: INT32 = 16_00008000;

(* Menu item resource format *)
TYPE
  MENUITEMTEMPLATEHEADER = RECORD
                             versionNumber: UINT16;
                             offset       : UINT16;
  END;

  MENUITEMTEMPLATE = RECORD
                       mtOption: UINT16;
                       mtID    : UINT16;
                       mtString: ARRAY [0 .. 1 - 1] OF char;
  END;

CONST MF_END: INT32 = 16_00000080;

(* System Menu Command Values *)
CONST
  SC_SIZE       = 16_F000;
  SC_MOVE       = 16_F010;
  SC_MINIMIZE   = 16_F020;
  SC_MAXIMIZE   = 16_F030;
  SC_NEXTWINDOW = 16_F040;
  SC_PREVWINDOW = 16_F050;
  SC_CLOSE      = 16_F060;
  SC_VSCROLL    = 16_F070;
  SC_HSCROLL    = 16_F080;
  SC_MOUSEMENU  = 16_F090;
  SC_KEYMENU    = 16_F100;
  SC_ARRANGE    = 16_F110;
  SC_RESTORE    = 16_F120;
  SC_TASKLIST   = 16_F130;
  SC_SCREENSAVE = 16_F140;
  SC_HOTKEY     = 16_F150;

(* Obsolete names *)
CONST
  SC_ICON = SC_MINIMIZE;
  SC_ZOOM = SC_MAXIMIZE;

(* Resource Loading Routines *)

<*EXTERNAL LoadBitmapA:WINAPI*>
PROCEDURE LoadBitmapA (hInstance: HINSTANCE; lpBitmapName: PCSTR): HBITMAP;

<*EXTERNAL LoadBitmapW:WINAPI*>
PROCEDURE LoadBitmapW (hInstance: HINSTANCE; lpBitmapName: PCWSTR): HBITMAP;
CONST LoadBitmap = LoadBitmapA;

<*EXTERNAL LoadCursorA:WINAPI*>
PROCEDURE LoadCursorA (hInstance: HINSTANCE; lpCursorName: PCSTR): HCURSOR;

<*EXTERNAL LoadCursorW:WINAPI*>
PROCEDURE LoadCursorW (hInstance: HINSTANCE; lpCursorName: PCWSTR): HCURSOR;
CONST LoadCursor = LoadCursorA;

<*EXTERNAL CreateCursor:WINAPI*>
PROCEDURE CreateCursor (a1: HINSTANCE;
                        a2: INT32;
                        a3: INT32;
                        a4: INT32;
                        a5: INT32;
                        a6: PVOID;
                        a7: PVOID  ): HCURSOR;

<*EXTERNAL DestroyCursor:WINAPI*>
PROCEDURE DestroyCursor (a1: HCURSOR): BOOL;

(* Standard Cursor IDs *)
VAR                             (* CONST *)
  IDC_ARROW      : PTSTR;
  IDC_IBEAM      : PTSTR;
  IDC_WAIT       : PTSTR;
  IDC_CROSS      : PTSTR;
  IDC_UPARROW    : PTSTR;
  IDC_SIZE       : PTSTR;
  IDC_ICON       : PTSTR;
  IDC_SIZENWSE   : PTSTR;
  IDC_SIZENESW   : PTSTR;
  IDC_SIZEWE     : PTSTR;
  IDC_SIZENS     : PTSTR;
  IDC_SIZEALL    : PTSTR;      (* not in win3.1 *)
  IDC_NO         : PTSTR;      (* not in win3.1 *)
  IDC_APPSTARTING: PTSTR;      (* not in win3.1 *)

TYPE
  ICONINFO = RECORD
    fIcon   : BOOL;
    xHotspot: UINT32;
    yHotspot: UINT32;
    hbmMask : HBITMAP;
    hbmColor: HBITMAP;
  END;
  PICONINFO = UNTRACED REF ICONINFO;

<*EXTERNAL LoadIconA:WINAPI*>
PROCEDURE LoadIconA (hInstance: HINSTANCE; lpIconName: PCSTR): HICON;

<*EXTERNAL LoadIconW:WINAPI*>
PROCEDURE LoadIconW (hInstance: HINSTANCE; lpIconName: PCWSTR): HICON;
CONST LoadIcon = LoadIconA;

<*EXTERNAL CreateIcon:WINAPI*>
PROCEDURE CreateIcon (a1: HINSTANCE;
                      a2: INT32;
                      a3: INT32;
                      a4: UINT8;
                      a5: UINT8;
                      a6: UNTRACED REF UINT8;
                      a7: UNTRACED REF UINT8  ): HICON;

<*EXTERNAL DestroyIcon:WINAPI*>
PROCEDURE DestroyIcon (a1: HICON): BOOL;

<*EXTERNAL LookupIconIdFromDirectory:WINAPI*>
PROCEDURE LookupIconIdFromDirectory (presbits: PUINT8; fIcon: BOOL): INT32;

<*EXTERNAL CreateIconFromResource:WINAPI*>
PROCEDURE CreateIconFromResource (presbits : PUINT8;
                                  dwResSize: UINT32;
                                  fIcon    : BOOL;
                                  dwVer    : UINT32  ): HICON;

<*EXTERNAL CreateIconIndirect:WINAPI*>
PROCEDURE CreateIconIndirect (piconinfo: PICONINFO): HICON;

<*EXTERNAL CopyIcon:WINAPI*>
PROCEDURE CopyIcon (a1: HICON): HICON;

<*EXTERNAL GetIconInfo:WINAPI*>
PROCEDURE GetIconInfo (hIcon: HICON; piconinfo: PICONINFO): BOOL;

(* OEM Resource Ordinal Numbers *)
CONST
  OBM_CLOSE    = 32754;
  OBM_UPARROW  = 32753;
  OBM_DNARROW  = 32752;
  OBM_RGARROW  = 32751;
  OBM_LFARROW  = 32750;
  OBM_REDUCE   = 32749;
  OBM_ZOOM     = 32748;
  OBM_RESTORE  = 32747;
  OBM_REDUCED  = 32746;
  OBM_ZOOMD    = 32745;
  OBM_RESTORED = 32744;
  OBM_UPARROWD = 32743;
  OBM_DNARROWD = 32742;
  OBM_RGARROWD = 32741;
  OBM_LFARROWD = 32740;
  OBM_MNARROW  = 32739;
  OBM_COMBO    = 32738;
  OBM_UPARROWI = 32737;
  OBM_DNARROWI = 32736;
  OBM_RGARROWI = 32735;
  OBM_LFARROWI = 32734;

  OBM_OLD_CLOSE   = 32767;
  OBM_SIZE        = 32766;
  OBM_OLD_UPARROW = 32765;
  OBM_OLD_DNARROW = 32764;
  OBM_OLD_RGARROW = 32763;
  OBM_OLD_LFARROW = 32762;
  OBM_BTSIZE      = 32761;
  OBM_CHECK       = 32760;
  OBM_CHECKBOXES  = 32759;
  OBM_BTNCORNERS  = 32758;
  OBM_OLD_REDUCE  = 32757;
  OBM_OLD_ZOOM    = 32756;
  OBM_OLD_RESTORE = 32755;

  OCR_NORMAL   = 32512;
  OCR_IBEAM    = 32513;
  OCR_WAIT     = 32514;
  OCR_CROSS    = 32515;
  OCR_UP       = 32516;
  OCR_SIZE     = 32640;
  OCR_ICON     = 32641;
  OCR_SIZENWSE = 32642;
  OCR_SIZENESW = 32643;
  OCR_SIZEWE   = 32644;
  OCR_SIZENS   = 32645;
  OCR_SIZEALL  = 32646;
  OCR_ICOCUR   = 32647;
  OCR_NO       = 32648;         (* not in win3.1 *)

  OIC_SAMPLE = 32512;
  OIC_HAND   = 32513;
  OIC_QUES   = 32514;
  OIC_BANG   = 32515;
  OIC_NOTE   = 32516;

CONST
  ORD_LANGDRIVER = 1;           (* The ordinal number for the entry point
                                   of ** language drivers. *)

(* Standard Icon IDs *)
VAR                             (* CONST *)
  IDI_APPLICATION: PTSTR;
  IDI_HAND       : PTSTR;
  IDI_QUESTION   : PTSTR;
  IDI_EXCLAMATION: PTSTR;
  IDI_ASTERISK   : PTSTR;

<*EXTERNAL LoadStringA:WINAPI*>
PROCEDURE LoadStringA (hInstance : HINSTANCE;
                         uID       : UINT32;
                         lpBuffer  : PSTR;
                         nBufferMax: INT32        ): INT32;

<*EXTERNAL LoadStringW:WINAPI*>
PROCEDURE LoadStringW (hInstance : HINSTANCE;
                         uID       : UINT32;
                         lpBuffer  : PWSTR;
                         nBufferMax: INT32        ): INT32;
CONST LoadString = LoadStringA;

(* Dialog Box Command IDs *)
CONST
  IDOK     = 1;
  IDCANCEL = 2;
  IDABORT  = 3;
  IDRETRY  = 4;
  IDIGNORE = 5;
  IDYES    = 6;
  IDNO     = 7;

(* Control Manager Structures and Definitions *)

(* Edit Control Styles *)
CONST
  ES_LEFT       : INT32 = 16_0000;
  ES_CENTER     : INT32 = 16_0001;
  ES_RIGHT      : INT32 = 16_0002;
  ES_MULTILINE  : INT32 = 16_0004;
  ES_UPPERCASE  : INT32 = 16_0008;
  ES_LOWERCASE  : INT32 = 16_0010;
  ES_PASSWORD   : INT32 = 16_0020;
  ES_AUTOVSCROLL: INT32 = 16_0040;
  ES_AUTOHSCROLL: INT32 = 16_0080;
  ES_NOHIDESEL  : INT32 = 16_0100;
  ES_OEMCONVERT : INT32 = 16_0400;
  ES_READONLY   : INT32 = 16_0800;
  ES_WANTRETURN : INT32 = 16_1000;

(* Edit Control Notification Codes *)
CONST
  EN_SETFOCUS  = 16_0100;
  EN_KILLFOCUS = 16_0200;
  EN_CHANGE    = 16_0300;
  EN_UPDATE    = 16_0400;
  EN_ERRSPACE  = 16_0500;
  EN_MAXTEXT   = 16_0501;
  EN_HSCROLL   = 16_0601;
  EN_VSCROLL   = 16_0602;

(* Edit Control Messages *)
CONST
  EM_GETSEL              = 16_00B0;
  EM_SETSEL              = 16_00B1;
  EM_GETRECT             = 16_00B2;
  EM_SETRECT             = 16_00B3;
  EM_SETRECTNP           = 16_00B4;
  EM_SCROLL              = 16_00B5;
  EM_LINESCROLL          = 16_00B6;
  EM_SCROLLCARET         = 16_00B7;
  EM_GETMODIFY           = 16_00B8;
  EM_SETMODIFY           = 16_00B9;
  EM_GETLINECOUNT        = 16_00BA;
  EM_LINEINDEX           = 16_00BB;
  EM_SETHANDLE           = 16_00BC;
  EM_GETHANDLE           = 16_00BD;
  EM_GETTHUMB            = 16_00BE;
  EM_LINELENGTH          = 16_00C1;
  EM_REPLACESEL          = 16_00C2;
  EM_SETFONT             = 16_00C3;
  EM_GETLINE             = 16_00C4;
  EM_LIMITTEXT           = 16_00C5;
  EM_CANUNDO             = 16_00C6;
  EM_UNDO                = 16_00C7;
  EM_FMTLINES            = 16_00C8;
  EM_LINEFROMCHAR        = 16_00C9;
  EM_SETWORDBREAK        = 16_00CA;
  EM_SETTABSTOPS         = 16_00CB;
  EM_SETPASSWORDCHAR     = 16_00CC;
  EM_EMPTYUNDOBUFFER     = 16_00CD;
  EM_GETFIRSTVISIBLELINE = 16_00CE;
  EM_SETREADONLY         = 16_00CF;
  EM_SETWORDBREAKPROC    = 16_00D0;
  EM_GETWORDBREAKPROC    = 16_00D1;
  EM_GETPASSWORDCHAR     = 16_00D2;
  EM_MSGMAX              = 16_00D3;

(* EDITWORDBREAKPROC code values *)
CONST
  WB_LEFT        = 0;
  WB_RIGHT       = 1;
  WB_ISDELIMITER = 2;

(* Button Control Styles *)
CONST
  BS_PUSHBUTTON     : INT32 = 16_00;
  BS_DEFPUSHBUTTON  : INT32 = 16_01;
  BS_CHECKBOX       : INT32 = 16_02;
  BS_AUTOCHECKBOX   : INT32 = 16_03;
  BS_RADIOBUTTON    : INT32 = 16_04;
  BS_3STATE         : INT32 = 16_05;
  BS_AUTO3STATE     : INT32 = 16_06;
  BS_GROUPBOX       : INT32 = 16_07;
  BS_USERBUTTON     : INT32 = 16_08;
  BS_AUTORADIOBUTTON: INT32 = 16_09;
  BS_PUSHBOX        : INT32 = 16_0A;
  BS_OWNERDRAW      : INT32 = 16_0B;
  BS_LEFTTEXT       : INT32 = 16_20;

(* User Button Notification Codes *)
CONST
  BN_CLICKED       = 0;
  BN_PAINT         = 1;
  BN_HILITE        = 2;
  BN_UNHILITE      = 3;
  BN_DISABLE       = 4;
  BN_DOUBLECLICKED = 5;

(* Button Control Messages *)
CONST
  BM_GETCHECK = 16_00F0;
  BM_SETCHECK = 16_00F1;
  BM_GETSTATE = 16_00F2;
  BM_SETSTATE = 16_00F3;
  BM_SETSTYLE = 16_00F4;

(* Static Control Constants *)
CONST
  SS_LEFT          : INT32 = 16_00;
  SS_CENTER        : INT32 = 16_01;
  SS_RIGHT         : INT32 = 16_02;
  SS_ICON          : INT32 = 16_03;
  SS_BLACKRECT     : INT32 = 16_04;
  SS_GRAYRECT      : INT32 = 16_05;
  SS_WHITERECT     : INT32 = 16_06;
  SS_BLACKFRAME    : INT32 = 16_07;
  SS_GRAYFRAME     : INT32 = 16_08;
  SS_WHITEFRAME    : INT32 = 16_09;
  SS_USERITEM      : INT32 = 16_0A;
  SS_SIMPLE        : INT32 = 16_0B;
  SS_LEFTNOWORDWRAP: INT32 = 16_0C;
  SS_NOPREFIX      : INT32 = 16_80; (* Don't do "&" character translation *)

(* Static Control Mesages *)
CONST
  STM_SETICON = 16_170;
  STM_GETICON = 16_171;
  STM_MSGMAX  = 16_172;

(*
 * Dialog window class
 *)
VAR                             (* CONST *)
  WC_DIALOG: PTSTR;

(*
 * Get/SetWindowWord/Long offsets for use with WC_DIALOG windows
 *)
CONST
  DWL_MSGRESULT = 0;
  DWL_DLGPROC   = 4;
  DWL_USER      = 8;

(* Dialog Manager Routines *)

<*EXTERNAL IsDialogMessage:WINAPI*>
PROCEDURE IsDialogMessage (hDlg: HWND; lpMsg: LPMSG): BOOL;

<*EXTERNAL MapDialogRect:WINAPI*>
PROCEDURE MapDialogRect (hDlg: HWND; lpRect: PRECT): BOOL;

<*EXTERNAL DlgDirListA:WINAPI*>
PROCEDURE DlgDirListA (hDlg         : HWND;
                         lpPathSpec   : PSTR;
                         nIDListBox   : INT32;
                         nIDStaticPath: INT32;
                         uFileType    : UINT32   ): INT32;

<*EXTERNAL DlgDirListW:WINAPI*>
PROCEDURE DlgDirListW (hDlg         : HWND;
                         lpPathSpec   : PWSTR;
                         nIDListBox   : INT32;
                         nIDStaticPath: INT32;
                         uFileType    : UINT32    ): INT32;
CONST DlgDirList = DlgDirListA;

(*
 * DlgDirList, DlgDirListComboBox flags values
 *)
CONST
  DDL_READWRITE = 16_0000;
  DDL_READONLY  = 16_0001;
  DDL_HIDDEN    = 16_0002;
  DDL_SYSTEM    = 16_0004;
  DDL_DIRECTORY = 16_0010;
  DDL_ARCHIVE   = 16_0020;

  DDL_POSTMSGS  = 16_2000;
  DDL_DRIVES    = 16_4000;
  DDL_EXCLUSIVE = 16_8000;

<*EXTERNAL DlgDirSelectExA:WINAPI*>
PROCEDURE DlgDirSelectExA (hDlg      : HWND;
                             lpString  : PSTR;
                             nCount    : INT32;
                             nIDListBox: INT32    ): BOOL;

<*EXTERNAL DlgDirSelectExW:WINAPI*>
PROCEDURE DlgDirSelectExW (hDlg      : HWND;
                             lpString  : PWSTR;
                             nCount    : INT32;
                             nIDListBox: INT32     ): BOOL;
CONST DlgDirSelectEx = DlgDirSelectExA;

<*EXTERNAL DlgDirListComboBoxA:WINAPI*>
PROCEDURE DlgDirListComboBoxA (hDlg         : HWND;
                                 lpPathSpec   : PSTR;
                                 nIDComboBox  : INT32;
                                 nIDStaticPath: INT32;
                                 uFiletype    : UINT32   ): INT32;

<*EXTERNAL DlgDirListComboBoxW:WINAPI*>
PROCEDURE DlgDirListComboBoxW (hDlg         : HWND;
                                 lpPathSpec   : PWSTR;
                                 nIDComboBox  : INT32;
                                 nIDStaticPath: INT32;
                                 uFiletype    : UINT32    ): INT32;
CONST DlgDirListComboBox = DlgDirListComboBoxA;

<*EXTERNAL DlgDirSelectComboBoxExA:WINAPI*>
PROCEDURE DlgDirSelectComboBoxExA (hDlg       : HWND;
                                     lpString   : PSTR;
                                     nCount     : INT32;
                                     nIDComboBox: INT32    ): BOOL;

<*EXTERNAL DlgDirSelectComboBoxExW:WINAPI*>
PROCEDURE DlgDirSelectComboBoxExW (hDlg       : HWND;
                                     lpString   : PWSTR;
                                     nCount     : INT32;
                                     nIDComboBox: INT32     ): BOOL;
CONST DlgDirSelectComboBoxEx = DlgDirSelectComboBoxExA;

(* Dialog Styles *)
CONST
  DS_ABSALIGN  : INT32 = 16_01;
  DS_SYSMODAL  : INT32 = 16_02;
  DS_LOCALEDIT : INT32 = 16_20;  (* Edit items get Local storage. *)
  DS_SETFONT   : INT32 = 16_40;  (* User specified font for Dlg controls *)
  DS_MODALFRAME: INT32 = 16_80;  (* Can be combined with WS_CAPTION *)
  DS_NOIDLEMSG : INT32 = 16_100; (* WM_ENTERIDLE message will not be sent *)
  DS_SETFOREGROUND: INT32 = 16_200; (* not in win3.1 *)

  DM_GETDEFID = (WM_USER + 0);
  DM_SETDEFID = (WM_USER + 1);
  DC_HASDEFID = 16_534B;        (* not in win3.1 *)

(* Dialog Codes *)
CONST
  DLGC_WANTARROWS      = 16_0001; (* Control wants arrow keys *)
  DLGC_WANTTAB         = 16_0002; (* Control wants tab keys *)
  DLGC_WANTALLKEYS     = 16_0004; (* Control wants all keys *)
  DLGC_WANTMESSAGE     = 16_0004; (* Pass message to control *)
  DLGC_HASSETSEL       = 16_0008; (* Understands EM_SETSEL message *)
  DLGC_DEFPUSHBUTTON   = 16_0010; (* Default pushbutton *)
  DLGC_UNDEFPUSHBUTTON = 16_0020; (* Non-default pushbutton *)
  DLGC_RADIOBUTTON     = 16_0040; (* Radio button *)
  DLGC_WANTCHARS       = 16_0080; (* Want WM_CHAR messages *)
  DLGC_STATIC          = 16_0100; (* Static item: don't include *)
  DLGC_BUTTON          = 16_2000; (* Button item: can be checked *)

  LB_CTLCODE: INT32 = 0;

(* Listbox Return Values *)
CONST
  LB_OKAY     = 0;
  LB_ERR      = (-1);
  LB_ERRSPACE = (-2);

(*
**  The idStaticPath parameter to DlgDirList can have the following values
**  ORed if the list box should show other details of the files along with
**  the name of the files;
*)
(* all other details also will be returned *)

(* Listbox Notification Codes *)
CONST
  LBN_ERRSPACE  = (-2);
  LBN_SELCHANGE = 1;
  LBN_DBLCLK    = 2;
  LBN_SELCANCEL = 3;
  LBN_SETFOCUS  = 4;
  LBN_KILLFOCUS = 5;

(* Listbox messages *)
CONST
  LB_ADDSTRING           = 16_0180;
  LB_INSERTSTRING        = 16_0181;
  LB_DELETESTRING        = 16_0182;
  LB_SELITEMRANGEEX      = 16_0183;
  LB_RESETCONTENT        = 16_0184;
  LB_SETSEL              = 16_0185;
  LB_SETCURSEL           = 16_0186;
  LB_GETSEL              = 16_0187;
  LB_GETCURSEL           = 16_0188;
  LB_GETTEXT             = 16_0189;
  LB_GETTEXTLEN          = 16_018A;
  LB_GETCOUNT            = 16_018B;
  LB_SELECTSTRING        = 16_018C;
  LB_DIR                 = 16_018D;
  LB_GETTOPINDEX         = 16_018E;
  LB_FINDSTRING          = 16_018F;
  LB_GETSELCOUNT         = 16_0190;
  LB_GETSELITEMS         = 16_0191;
  LB_SETTABSTOPS         = 16_0192;
  LB_GETHORIZONTALEXTENT = 16_0193;
  LB_SETHORIZONTALEXTENT = 16_0194;
  LB_SETCOLUMNWIDTH      = 16_0195;
  LB_ADDFILE             = 16_0196;
  LB_SETTOPINDEX         = 16_0197;
  LB_GETITEMRECT         = 16_0198;
  LB_GETITEMDATA         = 16_0199;
  LB_SETITEMDATA         = 16_019A;
  LB_SELITEMRANGE        = 16_019B;
  LB_SETANCHORINDEX      = 16_019C;
  LB_GETANCHORINDEX      = 16_019D;
  LB_SETCARETINDEX       = 16_019E;
  LB_GETCARETINDEX       = 16_019F;
  LB_SETITEMHEIGHT       = 16_01A0;
  LB_GETITEMHEIGHT       = 16_01A1;
  LB_FINDSTRINGEXACT     = 16_01A2;
  LBCB_CARETON           = 16_01A3;
  LBCB_CARETOFF          = 16_01A4;
  LB_SETLOCALE           = 16_01A5;
  LB_GETLOCALE           = 16_01A6;
  LB_SETCOUNT            = 16_01A7;
  LB_MSGMAX              = 16_01A8;

(* Listbox Styles *)
CONST
  LBS_NOTIFY           : INT32 = 16_0001;
  LBS_SORT             : INT32 = 16_0002;
  LBS_NOREDRAW         : INT32 = 16_0004;
  LBS_MULTIPLESEL      : INT32 = 16_0008;
  LBS_OWNERDRAWFIXED   : INT32 = 16_0010;
  LBS_OWNERDRAWVARIABLE: INT32 = 16_0020;
  LBS_HASSTRINGS       : INT32 = 16_0040;
  LBS_USETABSTOPS      : INT32 = 16_0080;
  LBS_NOINTEGRALHEIGHT : INT32 = 16_0100;
  LBS_MULTICOLUMN      : INT32 = 16_0200;
  LBS_WANTKEYBOARDINPUT: INT32 = 16_0400;
  LBS_EXTENDEDSEL      : INT32 = 16_0800;
  LBS_DISABLENOSCROLL  : INT32 = 16_1000;
  LBS_NODATA           : INT32 = 16_2000;
  LBS_STANDARD = Or(LBS_NOTIFY, Or(LBS_SORT, Or(WS_VSCROLL, WS_BORDER)));

(* Combo Box return Values *)
CONST
  CB_OKAY     = 0;
  CB_ERR      = (-1);
  CB_ERRSPACE = (-2);

(* Combo Box Notification Codes *)
CONST
  CBN_ERRSPACE     = (-1);
  CBN_SELCHANGE    = 1;
  CBN_DBLCLK       = 2;
  CBN_SETFOCUS     = 3;
  CBN_KILLFOCUS    = 4;
  CBN_EDITCHANGE   = 5;
  CBN_EDITUPDATE   = 6;
  CBN_DROPDOWN     = 7;
  CBN_CLOSEUP      = 8;
  CBN_SELENDOK     = 9;
  CBN_SELENDCANCEL = 10;

(* Combo Box styles *)
CONST
  CBS_SIMPLE           : INT32 = 16_0001;
  CBS_DROPDOWN         : INT32 = 16_0002;
  CBS_DROPDOWNLIST     : INT32 = 16_0003;
  CBS_OWNERDRAWFIXED   : INT32 = 16_0010;
  CBS_OWNERDRAWVARIABLE: INT32 = 16_0020;
  CBS_AUTOHSCROLL      : INT32 = 16_0040;
  CBS_OEMCONVERT       : INT32 = 16_0080;
  CBS_SORT             : INT32 = 16_0100;
  CBS_HASSTRINGS       : INT32 = 16_0200;
  CBS_NOINTEGRALHEIGHT : INT32 = 16_0400;
  CBS_DISABLENOSCROLL  : INT32 = 16_0800;

(* Combo Box messages *)
CONST
  CB_GETEDITSEL            = 16_0140;
  CB_LIMITTEXT             = 16_0141;
  CB_SETEDITSEL            = 16_0142;
  CB_ADDSTRING             = 16_0143;
  CB_DELETESTRING          = 16_0144;
  CB_DIR                   = 16_0145;
  CB_GETCOUNT              = 16_0146;
  CB_GETCURSEL             = 16_0147;
  CB_GETLBTEXT             = 16_0148;
  CB_GETLBTEXTLEN          = 16_0149;
  CB_INSERTSTRING          = 16_014A;
  CB_RESETCONTENT          = 16_014B;
  CB_FINDSTRING            = 16_014C;
  CB_SELECTSTRING          = 16_014D;
  CB_SETCURSEL             = 16_014E;
  CB_SHOWDROPDOWN          = 16_014F;
  CB_GETITEMDATA           = 16_0150;
  CB_SETITEMDATA           = 16_0151;
  CB_GETDROPPEDCONTROLRECT = 16_0152;
  CB_SETITEMHEIGHT         = 16_0153;
  CB_GETITEMHEIGHT         = 16_0154;
  CB_SETEXTENDEDUI         = 16_0155;
  CB_GETEXTENDEDUI         = 16_0156;
  CB_GETDROPPEDSTATE       = 16_0157;
  CB_FINDSTRINGEXACT       = 16_0158;
  CB_SETLOCALE             = 16_0159;
  CB_GETLOCALE             = 16_015a;
  CB_MSGMAX                = 16_015b;

(* Scroll Bar Styles *)
CONST
  SBS_HORZ                   : INT32 = 16_0000;
  SBS_VERT                   : INT32 = 16_0001;
  SBS_TOPALIGN               : INT32 = 16_0002;
  SBS_LEFTALIGN              : INT32 = 16_0002;
  SBS_BOTTOMALIGN            : INT32 = 16_0004;
  SBS_RIGHTALIGN             : INT32 = 16_0004;
  SBS_SIZEBOXTOPLEFTALIGN    : INT32 = 16_0002;
  SBS_SIZEBOXBOTTOMRIGHTALIGN: INT32 = 16_0004;
  SBS_SIZEBOX                : INT32 = 16_0008;

(* Scroll bar messages *)
CONST
  SBM_SETPOS         = 16_00E0; (* not in win3.1 *)
  SBM_GETPOS         = 16_00E1; (* not in win3.1 *)
  SBM_SETRANGE       = 16_00E2; (* not in win3.1 *)
  SBM_SETRANGEREDRAW = 16_00E6; (* not in win3.1 *)
  SBM_GETRANGE       = 16_00E3; (* not in win3.1 *)
  SBM_ENABLE_ARROWS  = 16_00E4; (* not in win3.1 *)

(* MDI client style bits *)
CONST MDIS_ALLCHILDSTYLES = 16_0001;

(* wParam Flags for WM_MDITILE and WM_MDICASCADE messages. *)
CONST
  MDITILE_VERTICAL     = 16_0000; (* not in win3.1 *)
  MDITILE_HORIZONTAL   = 16_0001; (* not in win3.1 *)
  MDITILE_SKIPDISABLED = 16_0002; (* not in win3.1 *)

TYPE
  MDICREATESTRUCTA = RECORD
                       szClass: PCSTR;
                       szTitle: PCSTR;
                       hOwner : HANDLE;
                       x      : INT32;
                       y      : INT32;
                       cx     : INT32;
                       cy     : INT32;
    style  : UINT32;
    lParam : LPARAM;  (* app-defined stuff *)
  END;
  LPMDICREATESTRUCTA = UNTRACED REF MDICREATESTRUCTA;
  MDICREATESTRUCTW = RECORD
    szClass: PCWSTR;
    szTitle: PCWSTR;
    hOwner : HANDLE;
    x      : INT32;
    y      : INT32;
    cx     : INT32;
    cy     : INT32;
    style  : UINT32;
    lParam : LPARAM;   (* app-defined stuff *)
  END;
  LPMDICREATESTRUCTW = UNTRACED REF MDICREATESTRUCTW;
  MDICREATESTRUCT = MDICREATESTRUCTA;
  LPMDICREATESTRUCT = LPMDICREATESTRUCTA;

  CLIENTCREATESTRUCT = RECORD
    hWindowMenu : HANDLE;
    idFirstChild: UINT32;
  END;
  LPCLIENTCREATESTRUCT = UNTRACED REF CLIENTCREATESTRUCT;

<*EXTERNAL DefFrameProcA:WINAPI*>
PROCEDURE DefFrameProcA (hWnd         : HWND;
    hWndMDIClient: HWND;
    uMsg         : UINT32;
    wParam       : WPARAM;
                           lParam       : LPARAM  ): LRESULT;

<*EXTERNAL DefFrameProcW:WINAPI*>
PROCEDURE DefFrameProcW (hWnd         : HWND;
                           hWndMDIClient: HWND;
                           uMsg         : UINT32;
                           wParam       : WPARAM;
                           lParam       : LPARAM  ): LRESULT;
CONST DefFrameProc = DefFrameProcA;

<*EXTERNAL DefMDIChildProcA:WINAPI*>
PROCEDURE DefMDIChildProcA (hWnd  : HWND;
                              uMsg  : UINT32;
                              wParam: WPARAM;
                              lParam: LPARAM  ): LRESULT;

<*EXTERNAL DefMDIChildProcW:WINAPI*>
PROCEDURE DefMDIChildProcW (hWnd  : HWND;
                              uMsg  : UINT32;
                              wParam: WPARAM;
                              lParam: LPARAM  ): LRESULT;
CONST DefMDIChildProc = DefMDIChildProcA;

<*EXTERNAL TranslateMDISysAccel:WINAPI*>
PROCEDURE TranslateMDISysAccel (hWndClient: HWND; lpMsg: LPMSG): BOOL;

<*EXTERNAL ArrangeIconicWindows:WINAPI*>
PROCEDURE ArrangeIconicWindows (hWnd: HWND): UINT32;

<*EXTERNAL CreateMDIWindowA:WINAPI*>
PROCEDURE CreateMDIWindowA (lpClassName : PSTR;
                              lpWindowName: PSTR;
                              dwStyle     : UINT32;
                              X           : INT32;
                              Y           : INT32;
                              nWidth      : INT32;
                              nHeight     : INT32;
                              hWndParent  : HWND;
                              hInstance   : HINSTANCE;
                              lParam      : INT32       ): HWND;

<*EXTERNAL CreateMDIWindowW:WINAPI*>
PROCEDURE CreateMDIWindowW (lpClassName : PWSTR;
                              lpWindowName: PWSTR;
                              dwStyle     : UINT32;
                              X           : INT32;
                              Y           : INT32;
                              nWidth      : INT32;
                              nHeight     : INT32;
                              hWndParent  : HWND;
                              hInstance   : HINSTANCE;
                              lParam      : INT32       ): HWND;
CONST CreateMDIWindow = CreateMDIWindowA;

(****** Help support ********************************************************)

TYPE
  HELPPOLY = UINT32;
  MULTIKEYHELPA = RECORD
                    mkSize     : UINT32;
                    mkKeylist  : CHAR;
    szKeyphrase: ARRAY [0 .. 1 - 1] OF CHAR;
  END;
  PMULTIKEYHELPA = UNTRACED REF MULTIKEYHELPA;
  LPMULTIKEYHELPA = UNTRACED REF MULTIKEYHELPA;
  MULTIKEYHELPW = RECORD
    mkSize     : UINT32;
    mkKeylist  : WCHAR;
    szKeyphrase: ARRAY [0 .. 1 - 1] OF WCHAR;
  END;
  PMULTIKEYHELPW = UNTRACED REF MULTIKEYHELPW;
  LPMULTIKEYHELPW = UNTRACED REF MULTIKEYHELPW;
  MULTIKEYHELP = MULTIKEYHELPA;
  PMULTIKEYHELP = PMULTIKEYHELPA;
  LPMULTIKEYHELP = LPMULTIKEYHELPA;

  HELPWININFOA = RECORD
    wStructSize: INT32;
    x          : INT32;
    y          : INT32;
    dx         : INT32;
    dy         : INT32;
    wMax       : INT32;
    rgchMember : ARRAY [0 .. 2 - 1] OF CHAR;
  END;
  PHELPWININFOA = UNTRACED REF HELPWININFOA;
  LPHELPWININFOA = UNTRACED REF HELPWININFOA;
  HELPWININFOW = RECORD
    wStructSize: INT32;
    x          : INT32;
    y          : INT32;
    dx         : INT32;
    dy         : INT32;
    wMax       : INT32;
    rgchMember : ARRAY [0 .. 2 - 1] OF WCHAR;
  END;
  PHELPWININFOW = UNTRACED REF HELPWININFOW;
  LPHELPWININFOW = UNTRACED REF HELPWININFOW;
  HELPWININFO = HELPWININFOA;
  PHELPWININFO = PHELPWININFOA;
  LPHELPWININFO = LPHELPWININFOA;

(*
 * Commands to pass WinHelp()
 *)
CONST
  HELP_CONTEXT   : INT32 = 16_0001; (* Display topic in ulTopic *)
  HELP_QUIT      : INT32 = 16_0002; (* Terminate help *)
  HELP_INDEX     : INT32 = 16_0003; (* Display index *)
  HELP_CONTENTS  : INT32 = 16_0003;
  HELP_HELPONHELP: INT32 = 16_0004; (* Display help on using help *)
  HELP_SETINDEX: INT32 = 16_0005; (* Set current Index for multi index
                                    help *)
  HELP_SETCONTENTS : INT32 = 16_0005;
  HELP_CONTEXTPOPUP: INT32 = 16_0008;
  HELP_FORCEFILE   : INT32 = 16_0009;
  HELP_KEY: INT32 = 16_0101;     (* Display topic for keyword in
                                   offabData *)
  HELP_COMMAND   : INT32 = 16_0102;
  HELP_PARTIALKEY: INT32 = 16_0105;
  HELP_MULTIKEY  : INT32 = 16_0201;
  HELP_SETWINPOS : INT32 = 16_0203;

<*EXTERNAL WinHelpA:WINAPI*>
PROCEDURE WinHelpA (hwndMain: HWND;
                    lpszHelp: PCSTR;
                    uCommand: UINT32;
                    dwData  : SIZE_T   ): BOOL;

<*EXTERNAL WinHelpW:WINAPI*>
PROCEDURE WinHelpW (hwndMain: HWND;
                    lpszHelp: PCWSTR;
                    uCommand: UINT32;
                    dwData  : SIZE_T    ): BOOL;
CONST WinHelp = WinHelpA;

(* function declarations for profiler routines contained in Windows
   libraries *)

<*EXTERNAL ProfInsChk:WINAPI*>
PROCEDURE ProfInsChk (): INT32;

<*EXTERNAL ProfSetup:WINAPI*>
PROCEDURE ProfSetup (a1: INT32; a2: INT32);

<*EXTERNAL ProfSampRate:WINAPI*>
PROCEDURE ProfSampRate (a1: INT32; a2: INT32);

<*EXTERNAL ProfStart:WINAPI*>
PROCEDURE ProfStart ();

<*EXTERNAL ProfStop:WINAPI*>
PROCEDURE ProfStop ();

<*EXTERNAL ProfClear:WINAPI*>
PROCEDURE ProfClear ();

<*EXTERNAL ProfFlush:WINAPI*>
PROCEDURE ProfFlush ();

<*EXTERNAL ProfFinish:WINAPI*>
PROCEDURE ProfFinish ();

(* Parameter for SystemParametersInfo() *)
CONST
  SPI_GETBEEP               = 1;
  SPI_SETBEEP               = 2;
  SPI_GETMOUSE              = 3;
  SPI_SETMOUSE              = 4;
  SPI_GETBORDER             = 5;
  SPI_SETBORDER             = 6;
  SPI_TIMEOUTS              = 7;
  SPI_GETKEYBOARDSPEED      = 10;
  SPI_SETKEYBOARDSPEED      = 11;
  SPI_LANGDRIVER            = 12;
  SPI_ICONHORIZONTALSPACING = 13;
  SPI_GETSCREENSAVETIMEOUT  = 14;
  SPI_SETSCREENSAVETIMEOUT  = 15;
  SPI_GETSCREENSAVEACTIVE   = 16;
  SPI_SETSCREENSAVEACTIVE   = 17;
  SPI_GETGRIDGRANULARITY    = 18;
  SPI_SETGRIDGRANULARITY    = 19;
  SPI_SETDESKWALLPAPER      = 20;
  SPI_SETDESKPATTERN        = 21;
  SPI_GETKEYBOARDDELAY      = 22;
  SPI_SETKEYBOARDDELAY      = 23;
  SPI_ICONVERTICALSPACING   = 24;
  SPI_GETICONTITLEWRAP      = 25;
  SPI_SETICONTITLEWRAP      = 26;
  SPI_GETMENUDROPALIGNMENT  = 27;
  SPI_SETMENUDROPALIGNMENT  = 28;
  SPI_SETDOUBLECLKWIDTH     = 29;
  SPI_SETDOUBLECLKHEIGHT    = 30;
  SPI_GETICONTITLELOGFONT   = 31;
  SPI_SETDOUBLECLICKTIME    = 32;
  SPI_SETMOUSEBUTTONSWAP    = 33;
  SPI_SETICONTITLELOGFONT   = 34;
  SPI_GETFASTTASKSWITCH     = 35;
  SPI_SETFASTTASKSWITCH     = 36;
  SPI_MAX                   = 36;

<*EXTERNAL SystemParametersInfoA:WINAPI*>
PROCEDURE SystemParametersInfoA (a1, a2: UINT32; a3: PVOID; a4: UINT32): BOOL;

<*EXTERNAL SystemParametersInfoW:WINAPI*>
PROCEDURE SystemParametersInfoW (a1, a2: UINT32; a3: PVOID; a4: UINT32): BOOL;

CONST SystemParametersInfo = SystemParametersInfoA;

(* Flags *)
CONST
  SPIF_UPDATEINIFILE    = 16_0001;
  SPIF_SENDWININICHANGE = 16_0002;

(* misc stuff added by darkov March 2003 *)

(* constants *)

	SS_OWNERDRAW       = 16_0000000D;
	SS_BITMAP          = 16_0000000E;
	SS_ENHMETAFILE     = 16_0000000F;
	SS_ETCHEDHORZ      = 16_00000010;
	SS_ETCHEDVERT      = 16_00000011;
	SS_ETCHEDFRAME     = 16_00000012;
	SS_TYPEMASK        = 16_0000001F;

	WS_EX_CLIENTEDGE   = 16_00000200;
	
	WM_NOTIFY          = 16_004E;

	DEFAULT_GUI_FONT   = 17;


	STM_SETIMAGE       = 16_0172;
	STM_GETIMAGE       = 16_0173;

	IMAGE_ENHMETAFILE  = 3;

(* structures *)
TYPE

	LPNMHDR = UNTRACED REF NMHDR;
	NMHDR = BITS (BITSIZE(ADDRESS) * 3 * 8) FOR RECORD
	    hwndFrom: HWND;
	    idFrom: SIZE_T;
	    code: UINT32; (* NM_ code *)
        (* padding: UINT32; Win64 only *)
	END;
(*

<*EXTERNAL GetSysColorBrush:WINAPI*>
PROCEDURE GetSysColorBrush(nIndex: INTEGER): HBRUSH;
*)

	
CONST	
	MFS_GRAYED          = 16_00000003;
	MFS_DISABLED        = MFS_GRAYED;
	MFS_CHECKED         = MF_CHECKED;
	MFS_HILITE          = MF_HILITE;
	MFS_ENABLED         = MF_ENABLED;
	MFS_UNCHECKED       = MF_UNCHECKED;
	MFS_UNHILITE        = MF_UNHILITE;
	MFS_DEFAULT         = 16_00001000;
	MFS_MASK            = 16_0000108B;
	MFS_HOTTRACKDRAWN   = 16_10000000;
	MFS_CACHEDBMP       = 16_20000000;
	MFS_BOTTOMGAPDROP   = 16_40000000;
	MFS_TOPGAPDROP      = 16_80000000;
	MFS_GAPDROP         = 16_C0000000;



(* menu stuff *)

	MIIM_STATE       = 16_00000001;
	MIIM_ID          = 16_00000002;
	MIIM_SUBMENU     = 16_00000004;
	MIIM_CHECKMARKS  = 16_00000008;
	MIIM_TYPE        = 16_00000010;
	MIIM_DATA        = 16_00000020;
	MIIM_STRING      = 16_00000040;
	MIIM_BITMAP      = 16_00000080;
	MIIM_FTYPE       = 16_00000100;

TYPE
	LPMENUITEMINFO = UNTRACED REF MENUITEMINFO;
	MENUITEMINFO = RECORD
		cbSize: UINT32;
		fMask: UINT32;
		fType: UINT32;
		fState: UINT32;       
		wID: UINT32;          
		hSubMenu: HMENU;      
		hbmpChecked: HBITMAP;  
		hbmpUnchecked: HBITMAP; 
		dwItemData: SIZE_T;
		dwTypeData: PSTR;    
		cch: UINT32;         
		hbmpItem: HBITMAP;     
	END;

<*EXTERNAL InsertMenuItemA:WINAPI*>
PROCEDURE InsertMenuItem( hMenu: HMENU; uItem: UINT32; fByposition: BOOL; lpmii: LPMENUITEMINFO): BOOL;

<*EXTERNAL GetMenuItemInfoA:WINAPI*>
PROCEDURE GetMenuItemInfo(hMenu: HMENU; uItem: UINT32; fByposition: BOOL; lpmii: LPMENUITEMINFO): BOOL;

<*EXTERNAL SetMenuItemInfoA:WINAPI*>
PROCEDURE SetMenuItemInfo(hMenu: HMENU; uItem: UINT32; fByposition: BOOL; lpmii: LPMENUITEMINFO): BOOL;

<*EXTERNAL GetMenuDefaultItem:WINAPI*>
PROCEDURE GetMenuDefaultItem( hMenu: HMENU;  fByPos: UINT32;  gmdiFlags: UINT32): UINT32;

<*EXTERNAL SetMenuDefaultItem:WINAPI*>
PROCEDURE SetMenuDefaultItem( hMenu: HMENU;  uItem: UINT32;  fByPos: UINT32): BOOL;

<*EXTERNAL GetMenuItemRect:WINAPI*>
PROCEDURE GetMenuItemRect( hWnd: HWND;  hMenu: HMENU;  uItem: UINT32;  lprcItem: PRECT): BOOL;

<*EXTERNAL MenuItemFromPoint:WINAPI*>
PROCEDURE MenuItemFromPoint( hWnd: HWND;  hMenu: HMENU;  ptScreen: POINT): INT32;


<*EXTERNAL SetDCBrushColor:WINAPI*>
PROCEDURE SetDCBrushColor(hdc: HDC; cr: COLORREF): COLORREF;

<*EXTERNAL SetDCPenColor:WINAPI*>
PROCEDURE SetDCPenColor(hdc: HDC; cr: COLORREF): COLORREF;




END WinUser.
