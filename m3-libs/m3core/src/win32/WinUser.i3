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

IMPORT Ctypes;

FROM Word IMPORT Or;
FROM WinDef IMPORT SHORT, BOOL, WORD, DWORD, UINT, LPDWORD, LPWORD, LPVOID,
                   HWND, HDC, WPARAM, LPARAM, LRESULT, HINSTANCE, HMENU,
                   POINT, HKL, HDESK, HWINSTA, HICON, HCURSOR, HBRUSH,
                   RECT, BYTE, ATOM, PBYTE, LPBYTE, LPHANDLE, HACCEL,
                   HBITMAP, LPRECT, LPINT, HRGN, LPPOINT, INT, COLORREF,
                   HHOOK;
FROM WinNT IMPORT WCHAR, HANDLE, LPSTR, LPWSTR, PVOID, LPTSTR, LPCSTR,
                  LPCWSTR, LONG, PSECURITY_INFORMATION,
                  PSECURITY_DESCRIPTOR, LPCTSTR;
FROM Ctypes IMPORT int, char, long, void_star;

CONST WINVER = 16_0314;         (* version 3.2 *)

TYPE
  HDWP = HANDLE;

(* MENUTEMPLATEA = VOID; (* No M3 equivalent *)
  MENUTEMPLATEW = VOID; *)

  LPMENUTEMPLATE = void_star;

  WNDPROC = <*CALLBACK*> PROCEDURE (a1: HWND; a2: UINT;
                                    a3: WPARAM; a4: LPARAM): LRESULT;

  DLGPROC = <*CALLBACK*> PROCEDURE (a1: HWND; a2: UINT;
                                    a3: WPARAM; a4: LPARAM): BOOL;

  TIMERPROC = <*CALLBACK*> PROCEDURE (a1: HWND; a2: UINT; a3: UINT; a4: DWORD);

  GRAYSTRINGPROC = <*CALLBACK*> PROCEDURE (a1: HDC; a2: LPARAM; a3: int): BOOL;

  PROPENUMPROC = <*CALLBACK*> PROCEDURE (a1: HWND; a2: LPCSTR;
                                         a3: HANDLE): BOOL;

  PROPENUMPROCEX = <*CALLBACK*> PROCEDURE (a1: HWND; a2: LPTSTR;
                                           a3: HANDLE; a4: DWORD): BOOL;

  WNDENUMPROC = <*CALLBACK*> PROCEDURE (a1: HWND; a2: LPARAM): BOOL;

  HOOKPROC = <*CALLBACK*> PROCEDURE (code: int; wParam: WPARAM;
                                     lParam: LPARAM): LRESULT;

  EDITWORDBREAKPROC = <*CALLBACK*> PROCEDURE (lpch: LPSTR; ichCurrent: int;
                                              cch: int; code: int): int;

  SENDASYNCPROC = <*CALLBACK*> PROCEDURE (a1: HWND; a2: UINT;
                                          a3: DWORD; a4: LRESULT);

(*!!!  #define MAKEINTRESOURCE(i) (LPTSTR)((DWORD)((WORD)(i))) *)

(* Predefined Resource Types *)
VAR                             (* CONST *)
  RT_CURSOR      : LPTSTR;
  RT_BITMAP      : LPTSTR;
  RT_ICON        : LPTSTR;
  RT_MENU        : LPTSTR;
  RT_DIALOG      : LPTSTR;
  RT_STRING      : LPTSTR;
  RT_FONTDIR     : LPTSTR;
  RT_FONT        : LPTSTR;
  RT_ACCELERATOR : LPTSTR;
  RT_RCDATA      : LPTSTR;
  RT_MESSAGETABLE: LPTSTR;

CONST DIFFERENCE = 11;

VAR                             (* CONST *)
  (* NOTE: if any new resource types are introduced above this point, then
     the ** value of DIFFERENCE must be changed.  ** (RT_GROUP_CURSOR -
     RT_CURSOR) must always be equal to DIFFERENCE ** (RT_GROUP_ICON -
     RT_ICON) must always be equal to DIFFERENCE *)
  RT_GROUP_CURSOR: LPTSTR;
  (* The value RT_BITMAP+DIFFERENCE (13) is intentionally unused *)
  RT_GROUP_ICON: LPTSTR;
  (* The value 15 is unused/obsolete *)
  RT_VERSION   : LPTSTR;
  RT_DLGINCLUDE: LPTSTR;

TYPE
  wvsprintfA = <*WINAPI*> PROCEDURE (a1: LPSTR; a2: LPCSTR;
                                     arglist: Ctypes.void_star): Ctypes.int;
  wvsprintfW = <*WINAPI*> PROCEDURE (a1: LPWSTR; a2: LPCWSTR;
                                     arglist: Ctypes.void_star): Ctypes.int;
  wvsprintf = wvsprintfA;

(* Steve G.  How do you want to handle varargs??? *)
(*
int WINAPI wsprintfA(LPSTR, LPCSTR, ...);
int WINAPI wsprintfW(LPWSTR, LPCWSTR, ...);
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
CONST
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
CONST
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
CONST
  HIDE_WINDOW         = 0;
  SHOW_OPENWINDOW     = 1;
  SHOW_ICONWINDOW     = 2;
  SHOW_FULLSCREEN     = 3;
  SHOW_OPENNOACTIVATE = 4;

(* Identifiers for the WM_SHOWWINDOW message *)
CONST
  SW_PARENTCLOSING = 1;
  SW_OTHERZOOM     = 2;
  SW_PARENTOPENING = 3;
  SW_OTHERUNZOOM   = 4;

(* WM_KEYUP/DOWN/CHAR HIWORD(lParam) flags *)
CONST
  KF_EXTENDED = 16_0100;
  KF_DLGMODE  = 16_0800;
  KF_MENUMODE = 16_1000;
  KF_ALTDOWN  = 16_2000;
  KF_REPEAT   = 16_4000;
  KF_UP       = 16_8000;

(* Virtual Keys, Standard Set *)
CONST
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
CONST
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
CONST
  HC_GETLPLPFN  = (-3);
  HC_LPLPFNNEXT = (-2);
  HC_LPFNNEXT   = (-1);

(* Hook Codes *)
CONST
  HC_ACTION      = 0;
  HC_GETNEXT     = 1;
  HC_SKIP        = 2;
  HC_NOREMOVE    = 3;
  HC_NOREM       = HC_NOREMOVE;
  HC_SYSMODALON  = 4;
  HC_SYSMODALOFF = 5;

(* CBT Hook Codes *)
CONST
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
CONST
  HSHELL_WINDOWCREATED       = 1;
  HSHELL_WINDOWDESTROYED     = 2;
  HSHELL_ACTIVATESHELLWINDOW = 3;

(* Window Manager Hook Codes *)
CONST
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
    message: UINT;
    paramL : UINT;
    paramH : UINT;
    time   : DWORD;
    hwnd   : HWND;
  END;


  (* Message structure used by WH_CALLWNDPROC *)
  PCWPSTRUCT = UNTRACED REF CWPSTRUCT;
  CWPSTRUCT = RECORD
    lParam : LPARAM;
    wParam : WPARAM;
    message: DWORD;
    hwnd   : HWND;
  END;

  (* Structure used by WH_DEBUG *)
  PDEBUGHOOKINFO = UNTRACED REF DEBUGHOOKINFO;
  DEBUGHOOKINFO = RECORD
    idThread: DWORD;
    reserved: LPARAM;
    lParam  : LPARAM;
    wParam  : WPARAM;
    code    : int;
  END;

  MOUSEHOOKSTRUCT = RECORD
    pt          : POINT;
    hwnd        : HWND;
    wHitTestCode: UINT;
    dwExtraInfo : DWORD;
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
  LoadKeyboardLayoutA = <*WINAPI*> PROCEDURE (pwszKLID: LPCSTR; Flags: UINT): HKL;
  LoadKeyboardLayoutW = <*WINAPI*> PROCEDURE (pwszKLID: LPCWSTR; Flags: UINT): HKL;
  LoadKeyboardLayout = LoadKeyboardLayoutA;

TYPE
  ActivateKeyboardLayout = <*WINAPI*> PROCEDURE (hkl: HKL; Flags: UINT): BOOL;
  UnloadKeyboardLayout   = <*WINAPI*> PROCEDURE (hkl: HKL): BOOL;
  GetKeyboardLayoutNameA = <*WINAPI*> PROCEDURE (pwszKLID: LPSTR): BOOL;
  GetKeyboardLayoutNameW = <*WINAPI*> PROCEDURE (pwszKLID: LPWSTR): BOOL;

  GetKeyboardLayoutName = GetKeyboardLayoutNameA;

(*
 * Desktop-specific access flags
 *)
CONST
  DESKTOP_ENUMWINDOWS    : LONG = 16_0001;
  DESKTOP_CREATEWINDOW   : LONG = 16_0002;
  DESKTOP_CREATEMENU     : LONG = 16_0004;
  DESKTOP_HOOKCONTROL    : LONG = 16_0008;
  DESKTOP_JOURNALRECORD  : LONG = 16_0010;
  DESKTOP_JOURNALPLAYBACK: LONG = 16_0020;
  DESKTOP_ENUMERATE      : LONG = 16_0040;

TYPE GetThreadDesktop = <*WINAPI*> PROCEDURE (arg1: DWORD): HDESK;

(*
 * Windowstation-specific access flags
 *)
CONST
  WINSTA_ENUMDESKTOPS     : LONG = 16_0001;
  WINSTA_READATTRIBUTES   : LONG = 16_0002;
  WINSTA_ACCESSCLIPBOARD  : LONG = 16_0004;
  WINSTA_CREATEDESKTOP    : LONG = 16_0008;
  WINSTA_WRITEATTRIBUTES  : LONG = 16_0010;
  WINSTA_ACCESSGLOBALATOMS: LONG = 16_0020;
  WINSTA_EXITWINDOWS      : LONG = 16_0040;
  WINSTA_ENUMERATE        : LONG = 16_0100;
  WINSTA_READSCREEN       : LONG = 16_0200;


<*EXTERNAL GetProcessWindowStation:WINAPI*>
PROCEDURE GetProcessWindowStation (): HWINSTA;

(*
 * window-specific access flags
 *)
CONST WIN_ACCESSWINDOW: LONG = 16_0001;

(*
 * menu-specific access flags
 *)
CONST MENU_ACCESSMENU: LONG = 16_0001;

<*EXTERNAL SetUserObjectSecurity:WINAPI*>
PROCEDURE SetUserObjectSecurity (arg1: HANDLE;
                                 arg2: PSECURITY_INFORMATION;
                                 arg3: PSECURITY_DESCRIPTOR   ): BOOL;

<*EXTERNAL GetUserObjectSecurity:WINAPI*>
PROCEDURE GetUserObjectSecurity (arg1: HANDLE;
                                 arg2: PSECURITY_INFORMATION;
                                 arg3: PSECURITY_DESCRIPTOR;
                                 arg4: DWORD;
                                 arg5: LPDWORD                ): BOOL;

<*EXTERNAL ImpersonateDDEClientWindow:WINAPI*>
PROCEDURE ImpersonateDDEClientWindow (hwndClient: HWND; hwndServer: HWND):BOOL;

TYPE
  PWNDCLASSA = UNTRACED REF WNDCLASSA;
  NPWNDCLASSA = UNTRACED REF WNDCLASSA;
  LPWNDCLASSA = UNTRACED REF WNDCLASSA;
  WNDCLASSA = RECORD
    style        : UINT;
    lpfnWndProc  : WNDPROC;
    cbClsExtra   : int;
    cbWndExtra   : int;
    hInstance    : HINSTANCE;
    hIcon        : HICON;
    hCursor      : HCURSOR;
    hbrBackground: HBRUSH;
    lpszMenuName : LPCSTR;
    lpszClassName: LPCSTR;
  END;

  PWNDCLASSW = UNTRACED REF WNDCLASSW;
  LPWNDCLASSW = UNTRACED REF WNDCLASSW;
  WNDCLASSW = RECORD
    style        : UINT;
    lpfnWndProc  : WNDPROC;
    cbClsExtra   : int;
    cbWndExtra   : int;
    hInstance    : HINSTANCE;
    hIcon        : HICON;
    hCursor      : HCURSOR;
    hbrBackground: HBRUSH;
    lpszMenuName : LPCWSTR;
    lpszClassName: LPCWSTR;
  END;

  WNDCLASS = WNDCLASSA;
  PWNDCLASS = PWNDCLASSA;
  NPWNDCLASS = NPWNDCLASSA;
  LPWNDCLASS = LPWNDCLASSA;

  (* Message structure *)
  PMSG = UNTRACED REF MSG;
  LPMSG = UNTRACED REF MSG;
  MSG = RECORD
    hwnd   : HWND;
    message: UINT;
    wParam : WPARAM;
    lParam : LPARAM;
    time   : DWORD;
    pt     : POINT;
  END;

(*!!!  SteveG.  Macros will cost you extra.  #define POINTSTOPOINT(pt,pts)
   {(pt).x = (SHORT)LOWORD(pts); \ (pt).y = (SHORT)HIWORD(pts);} #define
   POINTTOPOINTS(pt) (MAKELONG((short)((pt).x), (short)((pt).y))) #define
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
CONST
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
CONST
  WM_NULL     = 16_0000;
  WM_CREATE   = 16_0001;
  WM_DESTROY  = 16_0002;
  WM_MOVE     = 16_0003;
  WM_SIZE     = 16_0005;
  WM_ACTIVATE = 16_0006;
(*
 * WM_ACTIVATE state values
 *)
CONST
  WA_INACTIVE    = 0;
  WA_ACTIVE      = 1;
  WA_CLICKACTIVE = 2;

CONST
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
  LPMINMAXINFO = UNTRACED REF MINMAXINFO;
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

CONST
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
    dwData: DWORD;
    cbData: DWORD;
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
CONST
  WM_PENWINFIRST = 16_0380;
  WM_PENWINLAST  = 16_038F;

  WM_MM_RESERVED_FIRST = 16_03A0;
  WM_MM_RESERVED_LAST  = 16_03DF;

(* NOTE: All Message Numbers below 16_0400 are RESERVED. *)

(* Private Window Messages Start Here: *)
CONST WM_USER = 16_0400;

(* WM_SYNCTASK Commands *)
CONST
  ST_BEGINSWP = 0;
  ST_ENDSWP   = 1;

(* WinWhere() Area Codes *)
CONST
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
CONST
  SMTO_NORMAL      = 16_0000;
  SMTO_BLOCK       = 16_0001;
  SMTO_ABORTIFHUNG = 16_0002;

(* WM_MOUSEACTIVATE Return Codes *)
CONST
  MA_ACTIVATE         = 1;
  MA_ACTIVATEANDEAT   = 2;
  MA_NOACTIVATE       = 3;
  MA_NOACTIVATEANDEAT = 4;

<*EXTERNAL RegisterWindowMessageA:WINAPI*>
PROCEDURE RegisterWindowMessageA (lpString: LPCSTR): UINT;

<*EXTERNAL RegisterWindowMessageW:WINAPI*>
PROCEDURE RegisterWindowMessageW (lpString: LPCWSTR): UINT;
CONST RegisterWindowMessage = RegisterWindowMessageA;

(* WM_SIZE message wParam values *)
CONST
  SIZE_RESTORED  = 0;
  SIZE_MINIMIZED = 1;
  SIZE_MAXIMIZED = 2;
  SIZE_MAXSHOW   = 3;
  SIZE_MAXHIDE   = 4;

(* Obsolete constant names *)
CONST
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
    x              : int;
    y              : int;
    cx             : int;
    cy             : int;
    flags          : UINT;
  END;
  PWINDOWPOS = UNTRACED REF WINDOWPOS;
  LPWINDOWPOS = UNTRACED REF WINDOWPOS;

  (* WM_NCCALCSIZE parameter structure *)
  NCCALCSIZE_PARAMS = RECORD
    rgrc : ARRAY [0 .. 3 - 1] OF RECT;
    lppos: PWINDOWPOS;
  END;
  LPNCCALCSIZE_PARAMS = UNTRACED REF NCCALCSIZE_PARAMS;

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
CONST
  MK_LBUTTON = 16_0001;
  MK_RBUTTON = 16_0002;
  MK_SHIFT   = 16_0004;
  MK_CONTROL = 16_0008;
  MK_MBUTTON = 16_0010;

(* Window Styles *)
CONST
  WS_OVERLAPPED  : LONG = 16_00000000;
  WS_POPUP       : LONG = 16_80000000;
  WS_CHILD       : LONG = 16_40000000;
  WS_MINIMIZE    : LONG = 16_20000000;
  WS_VISIBLE     : LONG = 16_10000000;
  WS_DISABLED    : LONG = 16_08000000;
  WS_CLIPSIBLINGS: LONG = 16_04000000;
  WS_CLIPCHILDREN: LONG = 16_02000000;
  WS_MAXIMIZE    : LONG = 16_01000000;
  WS_CAPTION     : LONG = 16_00C00000; (* WS_BORDER | WS_DLGFRAME *)
  WS_BORDER      : LONG = 16_00800000;
  WS_DLGFRAME    : LONG = 16_00400000;
  WS_VSCROLL     : LONG = 16_00200000;
  WS_HSCROLL     : LONG = 16_00100000;
  WS_SYSMENU     : LONG = 16_00080000;
  WS_THICKFRAME  : LONG = 16_00040000;
  WS_GROUP       : LONG = 16_00020000;
  WS_TABSTOP     : LONG = 16_00010000;

  WS_MINIMIZEBOX: LONG = 16_00020000;
  WS_MAXIMIZEBOX: LONG = 16_00010000;

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
CONST
  WS_EX_DLGMODALFRAME : LONG = 16_00000001;
  WS_EX_NOPARENTNOTIFY: LONG = 16_00000004;
  WS_EX_TOPMOST       : LONG = 16_00000008;
  WS_EX_ACCEPTFILES   : LONG = 16_00000010;
  WS_EX_TRANSPARENT   : LONG = 16_00000020;

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
CONST
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
CONST
  CF_PRIVATEFIRST = 16_0200;
  CF_PRIVATELAST  = 16_02FF;

(* "GDIOBJ" formats do get DeleteObject()'d *)
CONST
  CF_GDIOBJFIRST = 16_0300;
  CF_GDIOBJLAST  = 16_03FF;

(*
 * Defines for the fVirt field of the Accelerator table structure.
 *)
CONST
  FNOINVERT = 16_02;
  FSHIFT    = 16_04;
  FCONTROL  = 16_08;
  FALT      = 16_10;

TYPE
  ACCEL = RECORD
    fVirt: BYTE;        (* Also called the flags field *)
    key  : WORD;
    cmd  : WORD;
  END;
  LPACCEL = UNTRACED REF ACCEL;

  PAINTSTRUCT = RECORD
    hdc        : HDC;
    fErase     : BOOL;
    rcPaint    : RECT;
    fRestore   : BOOL;
    fIncUpdate : BOOL;
    rgbReserved: ARRAY [0 .. 32 - 1] OF BYTE;
  END;
  PPAINTSTRUCT = UNTRACED REF PAINTSTRUCT;
  LPPAINTSTRUCT = UNTRACED REF PAINTSTRUCT;

  LPCREATESTRUCTA = UNTRACED REF CREATESTRUCTA;
  CREATESTRUCTA = RECORD
    lpCreateParams: LPVOID;
    hInstance     : HINSTANCE;
    hMenu         : HMENU;
    hwndParent    : HWND;
    cy            : int;
    cx            : int;
    y             : int;
    x             : int;
    style         : LONG;
    lpszName      : LPCSTR;
    lpszClass     : LPCSTR;
    dwExStyle     : DWORD;
  END;

  LPCREATESTRUCTW = CREATESTRUCTW;
  CREATESTRUCTW = RECORD
    hInstance     : HINSTANCE;
    lpCreateParams: LPVOID;
    hMenu         : HMENU;
    hwndParent    : HWND;
    cy            : int;
    cx            : int;
    y             : int;
    x             : int;
    style         : LONG;
    lpszName      : LPCWSTR;
    lpszClass     : LPCWSTR;
    dwExStyle     : DWORD;
  END;

TYPE
  CREATESTRUCT = CREATESTRUCTA;
  LPCREATESTRUCT = LPCREATESTRUCTA;

  PWINDOWPLACEMENT = UNTRACED REF WINDOWPLACEMENT;
  LPWINDOWPLACEMENT = UNTRACED REF WINDOWPLACEMENT;
  WINDOWPLACEMENT = RECORD
    length          : UINT;
    flags           : UINT;
    showCmd         : UINT;
    ptMinPosition   : POINT;
    ptMaxPosition   : POINT;
    rcNormalPosition: RECT;
  END;

CONST
  WPF_SETMINPOSITION     = 16_0001;
  WPF_RESTORETOMAXIMIZED = 16_0002;

(* Owner draw control types *)
CONST
  ODT_MENU     = 1;
  ODT_LISTBOX  = 2;
  ODT_COMBOBOX = 3;
  ODT_BUTTON   = 4;

(* Owner draw actions *)
CONST
  ODA_DRAWENTIRE = 16_0001;
  ODA_SELECT     = 16_0002;
  ODA_FOCUS      = 16_0004;

(* Owner draw state *)
CONST
  ODS_SELECTED = 16_0001;
  ODS_GRAYED   = 16_0002;
  ODS_DISABLED = 16_0004;
  ODS_CHECKED  = 16_0008;
  ODS_FOCUS    = 16_0010;

(* MEASUREITEMSTRUCT for ownerdraw *)
TYPE
  MEASUREITEMSTRUCT = RECORD
    CtlType   : UINT;
    CtlID     : UINT;
    itemID    : UINT;
    itemWidth : UINT;
    itemHeight: UINT;
    itemData  : DWORD;
  END;

  (* DRAWITEMSTRUCT for ownerdraw *)
  DRAWITEMSTRUCT = RECORD
    CtlType   : UINT;
    CtlID     : UINT;
    itemID    : UINT;
    itemAction: UINT;
    itemState : UINT;
    hwndItem  : HWND;
    hDC       : HDC;
    rcItem    : RECT;
    itemData  : DWORD;
  END;

  (* DELETEITEMSTRUCT for ownerdraw *)
  DELETEITEMSTRUCT = RECORD
    CtlType : UINT;
    CtlID   : UINT;
    itemID  : UINT;
    hwndItem: HWND;
    itemData: UINT;
  END;

  (* COMPAREITEMSTUCT for ownerdraw sorting *)
  COMPAREITEMSTRUCT = RECORD
    CtlType   : UINT;
    CtlID     : UINT;
    hwndItem  : HWND;
    itemID1   : UINT;
    itemData1 : DWORD;
    itemID2   : UINT;
    itemData2 : DWORD;
    dwLocaleId: DWORD;
  END;

(* Message Function Templates *)

<*EXTERNAL GetMessageA:WINAPI*>
PROCEDURE GetMessageA (lpMsg        : LPMSG;
                       hWnd         : HWND;
                       wMsgFilterMin: UINT;
                      wMsgFilterMax: UINT   ): BOOL;

(* Message Function Templates *)

<*EXTERNAL GetMessageW:WINAPI*>
PROCEDURE GetMessageW (lpMsg        : LPMSG;
                       hWnd         : HWND;
                       wMsgFilterMin: UINT;
                       wMsgFilterMax: UINT   ): BOOL;
CONST GetMessage = GetMessageA;

<*EXTERNAL TranslateMessage:WINAPI*>
PROCEDURE TranslateMessage (lpMsg: UNTRACED REF MSG): BOOL;

<*EXTERNAL DispatchMessageA:WINAPI*>
PROCEDURE DispatchMessageA (lpMsg: UNTRACED REF MSG): LONG;

<*EXTERNAL DispatchMessageW:WINAPI*>
PROCEDURE DispatchMessageW (lpMsg: UNTRACED REF MSG): LONG;
CONST DispatchMessage = DispatchMessageA;

<*EXTERNAL PeekMessageA:WINAPI*>
PROCEDURE PeekMessageA (lpMsg        : LPMSG;
                        hWnd         : HWND;
                        wMsgFilterMin: UINT;
                        wMsgFilterMax: UINT;
                        wRemoveMsg   : UINT   ): BOOL;

<*EXTERNAL PeekMessageW:WINAPI*>
PROCEDURE PeekMessageW (lpMsg        : LPMSG;
                        hWnd         : HWND;
                        wMsgFilterMin: UINT;
                        wMsgFilterMax: UINT;
                        wRemoveMsg   : UINT   ): BOOL;
CONST PeekMessage = PeekMessageA;

(* PeekMessage() Options *)
CONST
  PM_NOREMOVE = 16_0000;
  PM_REMOVE   = 16_0001;
  PM_NOYIELD  = 16_0002;

<*EXTERNAL RegisterHotKey:WINAPI*>
PROCEDURE RegisterHotKey (hwnd       : HWND;
                          id         : int;
                          fsModifiers: UINT;
                          vk         : UINT  ): BOOL;

<*EXTERNAL UnregisterHotKey:WINAPI*>
PROCEDURE UnregisterHotKey (hwnd: HWND; id: int): BOOL;

CONST
  MOD_ALT     = 16_0001;
  MOD_CONTROL = 16_0002;
  MOD_SHIFT   = 16_0004;

  IDHOT_SNAPWINDOW  = (-1);     (* SHIFT-PRINTSCRN *)
  IDHOT_SNAPDESKTOP = (-2);     (* PRINTSCRN *)

CONST
  EWX_LOGOFF   = 0;
  EWX_SHUTDOWN = 1;
  EWX_REBOOT   = 2;
  EWX_FORCE    = 4;

PROCEDURE ExitWindows(dwReserved: UINT; Code: DWORD): BOOL;

<*EXTERNAL ExitWindowsEx:WINAPI*>
PROCEDURE ExitWindowsEx (uFlags: UINT; ForceTimeout: DWORD): BOOL;

<*EXTERNAL SwapMouseButton:WINAPI*>
PROCEDURE SwapMouseButton (arg1: BOOL): BOOL;

<*EXTERNAL GetMessagePos:WINAPI*>
PROCEDURE GetMessagePos (): DWORD;

<*EXTERNAL GetMessageTime:WINAPI*>
PROCEDURE GetMessageTime (): LONG;

<*EXTERNAL GetMessageExtraInfo:WINAPI*>
PROCEDURE GetMessageExtraInfo (): LONG;

<*EXTERNAL SendMessageA:WINAPI*>
PROCEDURE SendMessageA (hWnd  : HWND;
                        Msg   : UINT;
                        wParam: WPARAM;
                        lParam: LPARAM  ): LRESULT;

<*EXTERNAL SendMessageW:WINAPI*>
PROCEDURE SendMessageW (hWnd  : HWND;
                        Msg   : UINT;
                        wParam: WPARAM;
                        lParam: LPARAM  ): LRESULT;
CONST SendMessage = SendMessageA;

<*EXTERNAL SendMessageTimeoutA:WINAPI*>
PROCEDURE SendMessageTimeoutA (hWnd      : HWND;
                               Msg       : UINT;
                               wParam    : WPARAM;
                               lParam    : LPARAM;
                               fuFlags   : UINT;
                               uTimeout  : UINT;
                               lpdwResult: LPDWORD ): LRESULT;

<*EXTERNAL SendMessageTimeoutW:WINAPI*>
PROCEDURE SendMessageTimeoutW (hWnd      : HWND;
                               Msg       : UINT;
                               wParam    : WPARAM;
                               lParam    : LPARAM;
                               fuFlags   : UINT;
                               uTimeout  : UINT;
                               lpdwResult: LPDWORD ): LRESULT;
CONST SendMessageTimeout = SendMessageTimeoutA;

<*EXTERNAL SendNotifyMessageA:WINAPI*>
PROCEDURE SendNotifyMessageA (hwnd  : HWND;
                                Msg   : UINT;
                                wParam: WPARAM;
                                lParam: LPARAM  ): BOOL;

<*EXTERNAL SendNotifyMessageW:WINAPI*>
PROCEDURE SendNotifyMessageW (hwnd  : HWND;
                                Msg   : UINT;
                                wParam: WPARAM;
                                lParam: LPARAM  ): BOOL;
CONST SendNotifyMessage = SendNotifyMessageA;

<*EXTERNAL SendMessageCallbackA:WINAPI*>
PROCEDURE SendMessageCallbackA (hwnd            : HWND;
                                  Msg             : UINT;
                                  wParam          : WPARAM;
                                  lParam          : LPARAM;
                                  lpResultCallBack: SENDASYNCPROC;
                                  dwData          : DWORD          ): BOOL;

<*EXTERNAL SendMessageCallbackW:WINAPI*>
PROCEDURE SendMessageCallbackW (hwnd            : HWND;
                                  Msg             : UINT;
                                  wParam          : WPARAM;
                                  lParam          : LPARAM;
                                  lpResultCallBack: SENDASYNCPROC;
                                  dwData          : DWORD          ): BOOL;
CONST SendMessageCallback = SendMessageCallbackA;

<*EXTERNAL PostMessageA:WINAPI*>
PROCEDURE PostMessageA (hWnd  : HWND;
                        Msg   : UINT;
                        wParam: WPARAM;
                        lParam: LPARAM  ): BOOL;

<*EXTERNAL PostMessageW:WINAPI*>
PROCEDURE PostMessageW (hWnd  : HWND;
                        Msg   : UINT;
                        wParam: WPARAM;
                        lParam: LPARAM  ): BOOL;
CONST PostMessage = PostMessageA;

<*EXTERNAL PostThreadMessageA:WINAPI*>
PROCEDURE PostThreadMessageA (idThread: DWORD;
                              Msg     : UINT;
                              wParam  : WPARAM;
                              lParam  : LPARAM  ): BOOL;

<*EXTERNAL PostThreadMessageW:WINAPI*>
PROCEDURE PostThreadMessageW (idThread: DWORD;
                              Msg     : UINT;
                              wParam  : WPARAM;
                              lParam  : LPARAM  ): BOOL;
CONST PostThreadMessage = PostThreadMessageA;

PROCEDURE PostAppMessageA (idThread: DWORD;
                           wMsg    : UINT;
                           wParam  : WPARAM;
                           lParam  : LPARAM  ): BOOL;

PROCEDURE PostAppMessageW (idThread: DWORD;
                           wMsg    : UINT;
                           wParam  : WPARAM;
                           lParam  : LPARAM  ): BOOL;

CONST PostAppMessage = PostAppMessageA;

(* Special HWND value for use with PostMessage() and SendMessage() *)
VAR (* CONST *)
  HWND_BROADCAST: HWND;

<*EXTERNAL AttachThreadInput:WINAPI*>
PROCEDURE AttachThreadInput (idAttach  : DWORD;
                             idAttachTo: DWORD;
                             fAttach   : BOOL   ): BOOL;

<*EXTERNAL ReplyMessage:WINAPI*>
PROCEDURE ReplyMessage (arg1: LRESULT): BOOL;

<*EXTERNAL WaitMessage:WINAPI*>
PROCEDURE WaitMessage (): BOOL;

<*EXTERNAL WaitForInputIdle:WINAPI*>
PROCEDURE WaitForInputIdle (hProcess: HANDLE; dwMilliseconds: DWORD): DWORD;

<*EXTERNAL DefWindowProcA:WINAPI*>
PROCEDURE DefWindowProcA (hWnd  : HWND;
                            Msg   : UINT;
                            wParam: WPARAM;
                            lParam: LPARAM  ): LRESULT;

<*EXTERNAL DefWindowProcW:WINAPI*>
PROCEDURE DefWindowProcW (hWnd  : HWND;
                            Msg   : UINT;
                            wParam: WPARAM;
                            lParam: LPARAM  ): LRESULT;
CONST DefWindowProc = DefWindowProcA;

<*EXTERNAL PostQuitMessage:WINAPI*>
PROCEDURE PostQuitMessage (nExitCode: int);

<*EXTERNAL CallWindowProcA:WINAPI*>
PROCEDURE CallWindowProcA (lpPrevWndFunc: WNDPROC;
                             hWnd         : HWND;
                             Msg          : UINT;
                             wParam       : WPARAM;
                             lParam       : LPARAM   ): LRESULT;

<*EXTERNAL CallWindowProcW:WINAPI*>
PROCEDURE CallWindowProcW (lpPrevWndFunc: WNDPROC;
                             hWnd         : HWND;
                             Msg          : UINT;
                             wParam       : WPARAM;
                             lParam       : LPARAM   ): LRESULT;
CONST CallWindowProc = CallWindowProcA;

<*EXTERNAL InSendMessage:WINAPI*>
PROCEDURE InSendMessage (): BOOL;

<*EXTERNAL GetDoubleClickTime:WINAPI*>
PROCEDURE GetDoubleClickTime (): UINT;

<*EXTERNAL SetDoubleClickTime:WINAPI*>
PROCEDURE SetDoubleClickTime (arg1: UINT): BOOL;

<*EXTERNAL RegisterClassA:WINAPI*>
PROCEDURE RegisterClassA (lpWndClass: UNTRACED REF WNDCLASSA): ATOM;

<*EXTERNAL RegisterClassW:WINAPI*>
PROCEDURE RegisterClassW (lpWndClass: UNTRACED REF WNDCLASSW): ATOM;
CONST RegisterClass = RegisterClassA;

<*EXTERNAL UnregisterClassA:WINAPI*>
PROCEDURE UnregisterClassA (lpClassName: LPCSTR; hInstance: HINSTANCE): BOOL;

<*EXTERNAL UnregisterClassW:WINAPI*>
PROCEDURE UnregisterClassW (lpClassName: LPCWSTR; hInstance: HINSTANCE): BOOL;
CONST UnregisterClass = UnregisterClassA;

<*EXTERNAL GetClassInfoA:WINAPI*>
PROCEDURE GetClassInfoA (hInstance  : HINSTANCE;
                           lpClassName: LPCSTR;
                           lpWndClass : LPWNDCLASSA): BOOL;

<*EXTERNAL GetClassInfoW:WINAPI*>
PROCEDURE GetClassInfoW (hInstance  : HINSTANCE;
                           lpClassName: LPCWSTR;
                           lpWndClass : LPWNDCLASSW): BOOL;
CONST GetClassInfo = GetClassInfoA;

CONST CW_USEDEFAULT = 16_80000000;

(* Special value for CreateWindow, et al. *)
VAR                             (* CONST *)
  HWND_DESKTOP: HWND;

<*EXTERNAL CreateWindowExA:WINAPI*>
PROCEDURE CreateWindowExA (dwExStyle   : DWORD;
                             lpClassName : LPCSTR;
                             lpWindowName: LPCSTR;
                             dwStyle     : DWORD;
                             X           : int;
                             Y           : int;
                             nWidth      : int;
                             nHeight     : int;
                             hWndParent  : HWND;
                             hMenu       : HMENU;
                             hInstance   : HINSTANCE;
                             lpParam     : LPVOID     ): HWND;

<*EXTERNAL CreateWindowExW:WINAPI*>
PROCEDURE CreateWindowExW (dwExStyle   : DWORD;
                             lpClassName : LPCWSTR;
                             lpWindowName: LPCWSTR;
                             dwStyle     : DWORD;
                             X           : int;
                             Y           : int;
                             nWidth      : int;
                             nHeight     : int;
                             hWndParent  : HWND;
                             hMenu       : HMENU;
                             hInstance   : HINSTANCE;
                             lpParam     : LPVOID     ): HWND;
CONST CreateWindowEx = CreateWindowExA;

PROCEDURE CreateWindowA (lpClassName : LPCSTR;
                         lpWindowName: LPCSTR;
                         dwStyle     : DWORD;
                         x           : int;
                         y           : int;
                         nWidth      : int;
                         nHeight     : int;
                         hwndParent  : HWND;
                         hMenu       : HMENU;
                         hInstance   : HINSTANCE;
                         lpParam     : LPVOID     ): HWND;

PROCEDURE CreateWindowW (lpClassName : LPCWSTR;
                         lpWindowName: LPCWSTR;
                         dwStyle     : DWORD;
                         x           : int;
                         y           : int;
                         nWidth      : int;
                         nHeight     : int;
                         hwndParent  : HWND;
                         hMenu       : HMENU;
                         hInstance   : HINSTANCE;
                         lpParam     : LPVOID     ): HWND;

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
PROCEDURE ShowWindow (hWnd: HWND; nCmdShow: int): BOOL;

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
                      X       : int;
                      Y       : int;
                      nWidth  : int;
                      nHeight : int;
                      bRepaint: BOOL  ): BOOL;

<*EXTERNAL SetWindowPos:WINAPI*>
PROCEDURE SetWindowPos (hWnd           : HWND;
                        hWndInsertAfter: HWND;
                        X              : int;
                        Y              : int;
                        cx             : int;
                        cy             : int;
                        uFlags         : UINT  ): BOOL;

<*EXTERNAL GetWindowPlacement:WINAPI*>
PROCEDURE GetWindowPlacement (hwnd   : HWND;
                              lpwndpl: UNTRACED REF WINDOWPLACEMENT): BOOL;

<*EXTERNAL SetWindowPlacement:WINAPI*>
PROCEDURE SetWindowPlacement (hwnd   : HWND;
                              lpwndpl: UNTRACED REF WINDOWPLACEMENT): BOOL;

<*EXTERNAL BeginDeferWindowPos:WINAPI*>
PROCEDURE BeginDeferWindowPos (nNumWindows: int): HDWP;

<*EXTERNAL DeferWindowPos:WINAPI*>
PROCEDURE DeferWindowPos (hWinPosInfo    : HDWP;
                          hWnd           : HWND;
                          hWndInsertAfter: HWND;
                          x              : int;
                          y              : int;
                          cx             : int;
                          cy             : int;
                          uFlags         : UINT  ): HDWP;

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
 * WARNING: * The following structures must NOT be DWORD padded because they are
 * followed by strings, etc that do not have to be DWORD aligned.
 *)
(*???  #pragma pack(2)

   typedef struct { DWORD style; DWORD dwExtendedStyle; WORD cdit; WORD x;
   WORD y; WORD cx; WORD cy; END; typedef DLGTEMPLATE *LPDLGTEMPLATEA;
   typedef DLGTEMPLATE *LPDLGTEMPLATEW; LPDLGTEMPLATE = LPDLGTEMPLATEA;
   typedef CONST DLGTEMPLATE *LPCDLGTEMPLATEA; typedef CONST DLGTEMPLATE
   *LPCDLGTEMPLATEW; LPCDLGTEMPLATE = LPCDLGTEMPLATEA;

   (* * Dialog item template (dit) *) typedef struct { DWORD style; DWORD
   dwExtendedStyle; WORD x; WORD y; WORD cx; WORD cy; WORD id; END; typedef
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
                                lpTemplateName: LPCSTR;
                                hWndParent    : HWND;
                                lpDialogFunc  : DLGPROC;
                                dwInitParam   : LPARAM     ): HWND;

<*EXTERNAL CreateDialogParamW:WINAPI*>
PROCEDURE CreateDialogParamW (hInstance     : HINSTANCE;
                                lpTemplateName: LPCWSTR;
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
                         lpName      : LPCSTR;
                         hwndParent  : HWND;
                         lpDialogFunc: DLGPROC    ): HWND;

PROCEDURE CreateDialogW (hInstance   : HINSTANCE;
                         lpName      : LPCWSTR;
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
                             lpTemplateName: LPCSTR;
                             hWndParent    : HWND;
                             lpDialogFunc  : DLGPROC;
                             dwInitParam   : LPARAM     ): int;

<*EXTERNAL DialogBoxParamW:WINAPI*>
PROCEDURE DialogBoxParamW (hInstance     : HINSTANCE;
                             lpTemplateName: LPCWSTR;
                             hWndParent    : HWND;
                             lpDialogFunc  : DLGPROC;
                             dwInitParam   : LPARAM     ): int;
CONST DialogBoxParam = DialogBoxParamA;

<*EXTERNAL DialogBoxIndirectParamA:WINAPI*>
PROCEDURE DialogBoxIndirectParamA (hInstance      : HINSTANCE;
                                     hDialogTemplate: LPDLGTEMPLATEA;
                                     hWndParent     : HWND;
                                     lpDialogFunc   : DLGPROC;
                                     dwInitParam    : LPARAM          ): int;

<*EXTERNAL DialogBoxIndirectParamW:WINAPI*>
PROCEDURE DialogBoxIndirectParamW (hInstance      : HINSTANCE;
                                     hDialogTemplate: LPDLGTEMPLATEW;
                                     hWndParent     : HWND;
                                     lpDialogFunc   : DLGPROC;
                                     dwInitParam    : LPARAM          ): int;
CONST DialogBoxIndirectParam = DialogBoxIndirectParamA;

PROCEDURE DialogBoxA(hInstance     : HINSTANCE;
                             lpTemplateName: LPCSTR;
                             hWndParent    : HWND;
                             lpDialogFunc  : DLGPROC): int;

PROCEDURE DialogBoxW(hInstance     : HINSTANCE;
                             lpTemplateName: LPCWSTR;
                             hWndParent    : HWND;
                             lpDialogFunc  : DLGPROC): int;

CONST DialogBox = DialogBoxA;

PROCEDURE DialogBoxIndirectA (hInstance      : HINSTANCE;
                              hDialogTemplate: LPDLGTEMPLATEA;
                              hWndParent     : HWND;
                              lpDialogFunc   : DLGPROC         ): int;

PROCEDURE DialogBoxIndirectW (hInstance      : HINSTANCE;
                              hDialogTemplate: LPDLGTEMPLATEW;
                              hWndParent     : HWND;
                              lpDialogFunc   : DLGPROC         ): int;

CONST DialogBoxIndirect = DialogBoxIndirectA;

<*EXTERNAL EndDialog:WINAPI*>
PROCEDURE EndDialog (hDlg: HWND; nResult: int): BOOL;

<*EXTERNAL GetDlgItem:WINAPI*>
PROCEDURE GetDlgItem (hDlg: HWND; nIDDlgItem: int): HWND;

<*EXTERNAL SetDlgItemInt:WINAPI*>
PROCEDURE SetDlgItemInt (hDlg      : HWND;
                           nIDDlgItem: int;
                           uValue    : UINT;
                           bSigned   : BOOL  ): BOOL;

<*EXTERNAL GetDlgItemInt:WINAPI*>
PROCEDURE GetDlgItemInt (hDlg        : HWND;
                           nIDDlgItem  : int;
                           lpTranslated: UNTRACED REF BOOL;
                           bSigned     : BOOL               ): UINT;

<*EXTERNAL SetDlgItemTextA:WINAPI*>
PROCEDURE SetDlgItemTextA (hDlg: HWND; nIDDlgItem: int; lpString: LPCSTR): BOOL;

<*EXTERNAL SetDlgItemTextW:WINAPI*>
PROCEDURE SetDlgItemTextW (hDlg: HWND; nIDDlgItem: int; lpString: LPCWSTR): BOOL;
CONST SetDlgItemText = SetDlgItemTextA;

<*EXTERNAL GetDlgItemTextA:WINAPI*>
PROCEDURE GetDlgItemTextA (hDlg      : HWND;
                             nIDDlgItem: int;
                             lpString  : LPSTR;
                             nMaxCount : int    ): UINT;

<*EXTERNAL GetDlgItemTextW:WINAPI*>
PROCEDURE GetDlgItemTextW (hDlg      : HWND;
                             nIDDlgItem: int;
                             lpString  : LPWSTR;
                             nMaxCount : int     ): UINT;
CONST GetDlgItemText = GetDlgItemTextA;

<*EXTERNAL CheckDlgButton:WINAPI*>
PROCEDURE CheckDlgButton (hDlg: HWND; nIDButton: int; uCheck: UINT): BOOL;

<*EXTERNAL CheckRadioButton:WINAPI*>
PROCEDURE CheckRadioButton (hDlg          : HWND;
                              nIDFirstButton: int;
                              nIDLastButton : int;
                              nIDCheckButton: int   ): BOOL;

<*EXTERNAL IsDlgButtonChecked:WINAPI*>
PROCEDURE IsDlgButtonChecked (hDlg: HWND; nIDButton: int): UINT;

<*EXTERNAL SendDlgItemMessageA:WINAPI*>
PROCEDURE SendDlgItemMessageA (hDlg      : HWND;
                                 nIDDlgItem: int;
                                 Msg       : UINT;
                                 wParam    : WPARAM;
                                 lParam    : LPARAM  ): LONG;

<*EXTERNAL SendDlgItemMessageW:WINAPI*>
PROCEDURE SendDlgItemMessageW (hDlg      : HWND;
                                 nIDDlgItem: int;
                                 Msg       : UINT;
                                 wParam    : WPARAM;
                                 lParam    : LPARAM  ): LONG;
CONST SendDlgItemMessage = SendDlgItemMessageA;

<*EXTERNAL GetNextDlgGroupItem:WINAPI*>
PROCEDURE GetNextDlgGroupItem (hDlg: HWND; hCtl: HWND; bPrevious: BOOL): HWND;

<*EXTERNAL GetNextDlgTabItem:WINAPI*>
PROCEDURE GetNextDlgTabItem (hDlg: HWND; hCtl: HWND; bPrevious: BOOL): HWND;

<*EXTERNAL GetDlgCtrlID:WINAPI*>
PROCEDURE GetDlgCtrlID (hWnd: HWND): int;

<*EXTERNAL GetDialogBaseUnits:WINAPI*>
PROCEDURE GetDialogBaseUnits (): long;

<*EXTERNAL DefDlgProcA:WINAPI*>
PROCEDURE DefDlgProcA (hDlg  : HWND;
                       Msg   : UINT;
                       wParam: WPARAM;
                       lParam: LPARAM  ): LRESULT;

<*EXTERNAL DefDlgProcW:WINAPI*>
PROCEDURE DefDlgProcW (hDlg  : HWND;
                       Msg   : UINT;
                       wParam: WPARAM;
                       lParam: LPARAM  ): LRESULT;
CONST DefDlgProc = DefDlgProcA;

(*
 * Window extra byted needed for private dialog classes.
 *)
CONST DLGWINDOWEXTRA = 30;

<*EXTERNAL CallMsgFilter:WINAPI*>
PROCEDURE CallMsgFilter (lpMsg: LPMSG; nCode: int): BOOL;

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
PROCEDURE SetClipboardData (uFormat: UINT; hMem: HANDLE): HANDLE;

<*EXTERNAL GetClipboardData:WINAPI*>
PROCEDURE GetClipboardData (uFormat: UINT): HANDLE;

<*EXTERNAL RegisterClipboardFormatA:WINAPI*>
PROCEDURE RegisterClipboardFormatA (arg1: LPCSTR): UINT;

<*EXTERNAL RegisterClipboardFormatW:WINAPI*>
PROCEDURE RegisterClipboardFormatW (arg1: LPCWSTR): UINT;
CONST RegisterClipboardFormat = RegisterClipboardFormatA;

<*EXTERNAL CountClipboardFormats:WINAPI*>
PROCEDURE CountClipboardFormats (): int;

<*EXTERNAL EnumClipboardFormats:WINAPI*>
PROCEDURE EnumClipboardFormats (arg1: UINT): UINT;

<*EXTERNAL GetClipboardFormatNameA:WINAPI*>
PROCEDURE GetClipboardFormatNameA (arg1: UINT; arg2: LPSTR; arg3: int): int;

<*EXTERNAL GetClipboardFormatNameW:WINAPI*>
PROCEDURE GetClipboardFormatNameW (arg1: UINT; arg2: LPWSTR; arg3: int): int;
CONST GetClipboardFormatName = GetClipboardFormatNameA;

<*EXTERNAL EmptyClipboard:WINAPI*>
PROCEDURE EmptyClipboard (): BOOL;

<*EXTERNAL IsClipboardFormatAvailable:WINAPI*>
PROCEDURE IsClipboardFormatAvailable (arg1: UINT): BOOL;

<*EXTERNAL GetPriorityClipboardFormat:WINAPI*>
PROCEDURE GetPriorityClipboardFormat (arg1: UNTRACED REF UINT; arg2: int): int;

<*EXTERNAL GetOpenClipboardWindow:WINAPI*>
PROCEDURE GetOpenClipboardWindow (): HWND;

(* Character Translation Routines *)

<*EXTERNAL CharToOemA:WINAPI*>
PROCEDURE CharToOemA (arg1: LPCSTR; arg2: LPSTR): BOOL;

<*EXTERNAL CharToOemW:WINAPI*>
PROCEDURE CharToOemW (arg1: LPCWSTR; arg2: LPSTR): BOOL;
CONST CharToOem = CharToOemA;

<*EXTERNAL OemToCharA:WINAPI*>
PROCEDURE OemToCharA (arg1: LPCSTR; arg2: LPSTR): BOOL;

<*EXTERNAL OemToCharW:WINAPI*>
PROCEDURE OemToCharW (arg1: LPCSTR; arg2: LPWSTR): BOOL;
CONST OemToChar = OemToCharA;

<*EXTERNAL CharToOemBuffA:WINAPI*>
PROCEDURE CharToOemBuffA (arg1: LPCSTR; arg2: LPSTR; arg3: DWORD): BOOL;

<*EXTERNAL CharToOemBuffW:WINAPI*>
PROCEDURE CharToOemBuffW (arg1: LPCWSTR; arg2: LPSTR; arg3: DWORD): BOOL;
CONST CharToOemBuff = CharToOemBuffA;

<*EXTERNAL OemToCharBuffA:WINAPI*>
PROCEDURE OemToCharBuffA (arg1: LPCSTR; arg2: LPSTR; arg3: DWORD): BOOL;

<*EXTERNAL OemToCharBuffW:WINAPI*>
PROCEDURE OemToCharBuffW (arg1: LPCSTR; arg2: LPWSTR; arg3: DWORD): BOOL;
CONST OemToCharBuff = OemToCharBuffA;

<*EXTERNAL CharUpperA:WINAPI*>
PROCEDURE CharUpperA (arg1: LPSTR): LPSTR;

<*EXTERNAL CharUpperW:WINAPI*>
PROCEDURE CharUpperW (arg1: LPWSTR): LPWSTR;
CONST CharUpper = CharUpperA;

<*EXTERNAL CharUpperBuffA:WINAPI*>
PROCEDURE CharUpperBuffA (arg1: LPSTR; arg2: DWORD): DWORD;

<*EXTERNAL CharUpperBuffW:WINAPI*>
PROCEDURE CharUpperBuffW (arg1: LPWSTR; arg2: DWORD): DWORD;
CONST CharUpperBuff = CharUpperBuffA;

<*EXTERNAL CharLowerA:WINAPI*>
PROCEDURE CharLowerA (arg1: LPSTR): LPSTR;

<*EXTERNAL CharLowerW:WINAPI*>
PROCEDURE CharLowerW (arg1: LPWSTR): LPWSTR;
CONST CharLower = CharLowerA;

<*EXTERNAL CharLowerBuffA:WINAPI*>
PROCEDURE CharLowerBuffA (a1: LPSTR; a2: DWORD): DWORD;

<*EXTERNAL CharLowerBuffW:WINAPI*>
PROCEDURE CharLowerBuffW (a1: LPWSTR; a2: DWORD): DWORD;
CONST CharLowerBuff = CharLowerBuffA;

<*EXTERNAL CharNextA:WINAPI*>
PROCEDURE CharNextA (a1: LPCSTR): LPSTR;

<*EXTERNAL CharNextW:WINAPI*>
PROCEDURE CharNextW (a1: LPCWSTR): LPWSTR;
CONST CharNext = CharNextA;

<*EXTERNAL CharPrevA:WINAPI*>
PROCEDURE CharPrevA (a1: LPCSTR; a2: LPCSTR): LPSTR;

<*EXTERNAL CharPrevW:WINAPI*>
PROCEDURE CharPrevW (a1: LPCWSTR; a2: LPCWSTR): LPWSTR;
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
PROCEDURE GetKBCodePage (): UINT;

<*EXTERNAL GetKeyState:WINAPI*>
PROCEDURE GetKeyState (nVirtKey: int): SHORT;

<*EXTERNAL GetAsyncKeyState:WINAPI*>
PROCEDURE GetAsyncKeyState (vKey: int): SHORT;

<*EXTERNAL GetKeyboardState:WINAPI*>
PROCEDURE GetKeyboardState (lpKeyState: PBYTE): BOOL;

<*EXTERNAL SetKeyboardState:WINAPI*>
PROCEDURE SetKeyboardState (lpKeyState: LPBYTE): BOOL;

<*EXTERNAL GetKeyNameTextA:WINAPI*>
PROCEDURE GetKeyNameTextA (lParam: LONG; lpString: LPSTR; nSize: int): int;

<*EXTERNAL GetKeyNameTextW:WINAPI*>
PROCEDURE GetKeyNameTextW (lParam: LONG; lpString: LPWSTR; nSize: int): int;
CONST GetKeyNameText = GetKeyNameTextA;

<*EXTERNAL GetKeyboardType:WINAPI*>
PROCEDURE GetKeyboardType (nTypeFlag: int): int;

<*EXTERNAL ToAscii:WINAPI*>
PROCEDURE ToAscii (uVirtKey  : UINT;
                   uScanCode : UINT;
                   lpKeyState: PBYTE;
                   lpChar    : LPWORD;
                   uFlags    : UINT    ): int;

<*EXTERNAL ToUnicode:WINAPI*>
PROCEDURE ToUnicode (wVirtKey  : UINT;
                     wScanCode : UINT;
                     lpKeyState: PBYTE;
                     lpChar    : LPDWORD;
                     wFlags    : UINT     ): int;

<*EXTERNAL VkKeyScanA:WINAPI*>
PROCEDURE VkKeyScanA (cChar: CHAR): SHORT;

<*EXTERNAL VkKeyScanW:WINAPI*>
PROCEDURE VkKeyScanW (cChar: WCHAR): SHORT;

CONST VkKeyScan = VkKeyScanA;

<*EXTERNAL MapVirtualKey:WINAPI*>
PROCEDURE MapVirtualKey (uCode: UINT; uMapType: UINT): UINT;

<*EXTERNAL GetInputState:WINAPI*>
PROCEDURE GetInputState (): BOOL;

<*EXTERNAL GetQueueStatus:WINAPI*>
PROCEDURE GetQueueStatus (flags: UINT): DWORD;

<*EXTERNAL GetCapture:WINAPI*>
PROCEDURE GetCapture (): HWND;

<*EXTERNAL SetCapture:WINAPI*>
PROCEDURE SetCapture (hWnd: HWND): HWND;

<*EXTERNAL ReleaseCapture:WINAPI*>
PROCEDURE ReleaseCapture (): BOOL;

<*EXTERNAL MsgWaitForMultipleObjects:WINAPI*>
PROCEDURE MsgWaitForMultipleObjects (nCount        : DWORD;
                                     pHandles      : LPHANDLE;
                                     fWaitAll      : BOOL;
                                     dwMilliseconds: DWORD;
                                     dwWakeMask    : DWORD     ): DWORD;

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
PROCEDURE GetSysInputMode (): UINT;

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
                    nIDEvent   : UINT;
                    uElapse    : UINT;
                    lpTimerFunc: TIMERPROC): UINT;

<*EXTERNAL KillTimer:WINAPI*>
PROCEDURE KillTimer (hWnd: HWND; uIDEvent: UINT): BOOL;

<*EXTERNAL IsWindowUnicode:WINAPI*>
PROCEDURE IsWindowUnicode (hWnd: HWND): BOOL;

<*EXTERNAL EnableWindow:WINAPI*>
PROCEDURE EnableWindow (hWnd: HWND; bEnable: BOOL): BOOL;

<*EXTERNAL IsWindowEnabled:WINAPI*>
PROCEDURE IsWindowEnabled (hWnd: HWND): BOOL;

<*EXTERNAL LoadAcceleratorsA:WINAPI*>
PROCEDURE LoadAcceleratorsA (hInstance: HINSTANCE; lpTableName: LPCSTR): HACCEL;

<*EXTERNAL LoadAcceleratorsW:WINAPI*>
PROCEDURE LoadAcceleratorsW (hInstance: HINSTANCE; lpTableName: LPCWSTR): HACCEL;

CONST LoadAccelerators = LoadAcceleratorsA;

<*EXTERNAL CreateAcceleratorTable:WINAPI*>
PROCEDURE CreateAcceleratorTable (int: LPACCEL): HACCEL;

<*EXTERNAL DestroyAcceleratorTable:WINAPI*>
PROCEDURE DestroyAcceleratorTable (a1: HACCEL): BOOL;

<*EXTERNAL CopyAcceleratorTable:WINAPI*>
PROCEDURE CopyAcceleratorTable (a1: HACCEL; a2: LPACCEL; a3: int): int;

<*EXTERNAL TranslateAccelerator:WINAPI*>
PROCEDURE TranslateAccelerator (hWnd     : HWND;
                                hAccTable: HACCEL;
                                lpMsg    : LPMSG   ): int;

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
PROCEDURE GetSystemMetrics (nIndex: int): int;

<*EXTERNAL LoadMenuA:WINAPI*>
PROCEDURE LoadMenuA (hInstance: HINSTANCE; lpMenuName: LPCSTR): HMENU;

<*EXTERNAL LoadMenuW:WINAPI*>
PROCEDURE LoadMenuW (hInstance: HINSTANCE; lpMenuName: LPCWSTR): HMENU;
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
                       a2: UINT;
                       a3: LPCTSTR;
                       a4: UINT;
                       a5: UINT     ): BOOL;

<*EXTERNAL ChangeMenuW:WINAPI*>
PROCEDURE ChangeMenuW (a1: HMENU;
                       a2: UINT;
                       a3: LPCTSTR;
                       a4: UINT;
                       a5: UINT     ): BOOL;
CONST ChangeMenu = ChangeMenuA;

<*EXTERNAL HiliteMenuItem:WINAPI*>
PROCEDURE HiliteMenuItem (hWnd         : HWND;
                          hMenu        : HMENU;
                          uIDHiliteItem: UINT;
                          uHilite      : UINT   ): BOOL;

<*EXTERNAL GetMenuStringA:WINAPI*>
PROCEDURE GetMenuStringA (hMenu    : HMENU;
                          uIDItem  : UINT;
                          lpString : LPSTR;
                          nMaxCount: int;
                          uFlag    : UINT   ): int;

<*EXTERNAL GetMenuStringW:WINAPI*>
PROCEDURE GetMenuStringW (hMenu    : HMENU;
                            uIDItem  : UINT;
                            lpString : LPWSTR;
                            nMaxCount: int;
                            uFlag    : UINT    ): int;
CONST GetMenuString = GetMenuStringA;

<*EXTERNAL GetMenuState:WINAPI*>
PROCEDURE GetMenuState (hMenu: HMENU; uId: UINT; uFlags: UINT): UINT;

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
PROCEDURE CheckMenuItem (hMenu: HMENU; uIDCheckItem: UINT; uCheck: UINT): BOOL;

<*EXTERNAL EnableMenuItem:WINAPI*>
PROCEDURE EnableMenuItem (hMenu        : HMENU;
                            uIDEnableItem: UINT;
                            uEnable      : UINT   ): BOOL;

<*EXTERNAL GetSubMenu:WINAPI*>
PROCEDURE GetSubMenu (hMenu: HMENU; nPos: int): HMENU;

<*EXTERNAL GetMenuItemID:WINAPI*>
PROCEDURE GetMenuItemID (hMenu: HMENU; nPos: int): UINT;

<*EXTERNAL GetMenuItemCount:WINAPI*>
PROCEDURE GetMenuItemCount (hMenu: HMENU): int;

<*EXTERNAL InsertMenuA:WINAPI*>
PROCEDURE InsertMenuA (hMenu     : HMENU;
                         uPosition : UINT;
                         uFlags    : UINT;
                         uIDNewItem: UINT;
                         lpNewItem : LPCSTR ): BOOL;

<*EXTERNAL InsertMenuW:WINAPI*>
PROCEDURE InsertMenuW (hMenu     : HMENU;
                         uPosition : UINT;
                         uFlags    : UINT;
                         uIDNewItem: UINT;
                         lpNewItem : LPCWSTR): BOOL;
CONST InsertMenu = InsertMenuA;

<*EXTERNAL AppendMenuA:WINAPI*>
PROCEDURE AppendMenuA (hMenu     : HMENU;
                         uFlags    : UINT;
                         uIDNewItem: UINT;
                         lpNewItem : LPCSTR ): BOOL;

<*EXTERNAL AppendMenuW:WINAPI*>
PROCEDURE AppendMenuW (hMenu     : HMENU;
                         uFlags    : UINT;
                         uIDNewItem: UINT;
                         lpNewItem : LPCWSTR): BOOL;
CONST AppendMenu = AppendMenuA;

<*EXTERNAL ModifyMenuA:WINAPI*>
PROCEDURE ModifyMenuA (hMnu      : HMENU;
                         uPosition : UINT;
                         uFlags    : UINT;
                         uIDNewItem: UINT;
                         lpNewItem : LPCSTR ): BOOL;

<*EXTERNAL ModifyMenuW:WINAPI*>
PROCEDURE ModifyMenuW (hMnu      : HMENU;
                         uPosition : UINT;
                         uFlags    : UINT;
                         uIDNewItem: UINT;
                         lpNewItem : LPCWSTR): BOOL;
CONST ModifyMenu = ModifyMenuA;

<*EXTERNAL RemoveMenu:WINAPI*>
PROCEDURE RemoveMenu (hMenu: HMENU; uPosition: UINT; uFlags: UINT): BOOL;

<*EXTERNAL DeleteMenu:WINAPI*>
PROCEDURE DeleteMenu (hMenu: HMENU; uPosition: UINT; uFlags: UINT): BOOL;

<*EXTERNAL SetMenuItemBitmaps:WINAPI*>
PROCEDURE SetMenuItemBitmaps (hMenu           : HMENU;
                                uPosition       : UINT;
                                uFlags          : UINT;
                                hBitmapUnchecked: HBITMAP;
                                hBitmapChecked  : HBITMAP  ): BOOL;

<*EXTERNAL GetMenuCheckMarkDimensions:WINAPI*>
PROCEDURE GetMenuCheckMarkDimensions (): LONG;

<*EXTERNAL TrackPopupMenu:WINAPI*>
PROCEDURE TrackPopupMenu (hMenu    : HMENU;
                            uFlags   : UINT;
                            x        : int;
                            y        : int;
                            nReserved: int;
                            hWnd     : HWND;
                            prcRect  : LPRECT): BOOL;

(* Flags for TrackPopupMenu *)
CONST
  TPM_LEFTBUTTON : LONG = 16_0000;
  TPM_RIGHTBUTTON: LONG = 16_0002;
  TPM_LEFTALIGN  : LONG = 16_0000;
  TPM_CENTERALIGN: LONG = 16_0004;
  TPM_RIGHTALIGN : LONG = 16_0008;

<*EXTERNAL DrawIcon:WINAPI*>
PROCEDURE DrawIcon (a1: HDC; a2: int; a3: int; a4: HICON): BOOL;

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
                     lpString: LPCSTR;
                     nCount  : int;
                     lpRect  : LPRECT;
                     uFormat : UINT    ): int;

<*EXTERNAL DrawTextW:WINAPI*>
PROCEDURE DrawTextW (hDC     : HDC;
                     lpString: LPCWSTR;
                     nCount  : int;
                     lpRect  : LPRECT;
                     uFormat : UINT     ): int;
CONST DrawText = DrawTextA;

<*EXTERNAL GrayStringA:WINAPI*>
PROCEDURE GrayStringA (hDC         : HDC;
                         hBrush      : HBRUSH;
                         lpOutputFunc: GRAYSTRINGPROC;
                         lpData      : LPARAM;
                         nCount      : int;
                         X           : int;
                         Y           : int;
                         nWidth      : int;
                         nHeight     : int             ): BOOL;

<*EXTERNAL GrayStringW:WINAPI*>
PROCEDURE GrayStringW (hDC         : HDC;
                         hBrush      : HBRUSH;
                         lpOutputFunc: GRAYSTRINGPROC;
                         lpData      : LPARAM;
                         nCount      : int;
                         X           : int;
                         Y           : int;
                         nWidth      : int;
                         nHeight     : int             ): BOOL;
CONST GrayString = GrayStringA;

<*EXTERNAL TabbedTextOutA:WINAPI*>
PROCEDURE TabbedTextOutA (hDC                : HDC;
                            X                  : int;
                            Y                  : int;
                            lpString           : LPCSTR;
                            nCount             : int;
                            nTabPositions      : int;
                            lpnTabStopPositions: LPINT;
                            nTabOrigin         : int     ): LONG;

<*EXTERNAL TabbedTextOutW:WINAPI*>
PROCEDURE TabbedTextOutW (hDC                : HDC;
                            X                  : int;
                            Y                  : int;
                            lpString           : LPCWSTR;
                            nCount             : int;
                            nTabPositions      : int;
                            lpnTabStopPositions: LPINT;
                            nTabOrigin         : int      ): LONG;
CONST TabbedTextOut = TabbedTextOutA;

<*EXTERNAL GetTabbedTextExtentA:WINAPI*>
PROCEDURE GetTabbedTextExtentA (hDC                : HDC;
                                  lpString           : LPCSTR;
                                  nCount             : int;
                                  nTabPositions      : int;
                                  lpnTabStopPositions: LPINT   ): DWORD;

<*EXTERNAL GetTabbedTextExtentW:WINAPI*>
PROCEDURE GetTabbedTextExtentW (hDC                : HDC;
                                  lpString           : LPCWSTR;
                                  nCount             : int;
                                  nTabPositions      : int;
                                  lpnTabStopPositions: LPINT    ): DWORD;
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
PROCEDURE GetDCEx (hwnd: HWND; hrgnClip: HRGN; flags: DWORD): HDC;

(* GetDCEx() flags *)
CONST
  DCX_WINDOW      : LONG = 16_00000001;
  DCX_CACHE       : LONG = 16_00000002;
  DCX_NORESETATTRS: LONG = 16_00000004;
  DCX_CLIPCHILDREN: LONG = 16_00000008;
  DCX_CLIPSIBLINGS: LONG = 16_00000010;
  DCX_PARENTCLIP  : LONG = 16_00000020;

  DCX_EXCLUDERGN  : LONG = 16_00000040;
  DCX_INTERSECTRGN: LONG = 16_00000080;

  DCX_EXCLUDEUPDATE  : LONG = 16_00000100;
  DCX_INTERSECTUPDATE: LONG = 16_00000200;

  DCX_LOCKWINDOWUPDATE: LONG = 16_00000400;

  DCX_USESTYLE   : LONG = 16_00010000;
  DCX_NORECOMPUTE: LONG = 16_00100000;
  DCX_VALIDATE   : LONG = 16_00200000;

<*EXTERNAL GetWindowDC:WINAPI*>
PROCEDURE GetWindowDC (hWnd: HWND): HDC;

<*EXTERNAL ReleaseDC:WINAPI*>
PROCEDURE ReleaseDC (hWnd: HWND; hDC: HDC): int;

<*EXTERNAL BeginPaint:WINAPI*>
PROCEDURE BeginPaint (hWnd: HWND; lpPaint: LPPAINTSTRUCT): HDC;

<*EXTERNAL EndPaint:WINAPI*>
PROCEDURE EndPaint (hWnd: HWND; lpPaint: UNTRACED REF PAINTSTRUCT): BOOL;

<*EXTERNAL GetUpdateRect:WINAPI*>
PROCEDURE GetUpdateRect (hWnd: HWND; lpRect: LPRECT; bErase: BOOL): BOOL;

<*EXTERNAL GetUpdateRgn:WINAPI*>
PROCEDURE GetUpdateRgn (hWnd: HWND; hRgn: HRGN; bErase: BOOL): int;

<*EXTERNAL ExcludeUpdateRgn:WINAPI*>
PROCEDURE ExcludeUpdateRgn (hDC: HDC; hWnd: HWND): int;

<*EXTERNAL InvalidateRect:WINAPI*>
PROCEDURE InvalidateRect (hWnd  : HWND;
                            lpRect: LPRECT;
                            bErase: BOOL               ): BOOL;

<*EXTERNAL ValidateRect:WINAPI*>
PROCEDURE ValidateRect (hWnd: HWND; lpRect: LPRECT): BOOL;

<*EXTERNAL InvalidateRgn:WINAPI*>
PROCEDURE InvalidateRgn (hWnd: HWND; hRgn: HRGN; bErase: BOOL): BOOL;

<*EXTERNAL ValidateRgn:WINAPI*>
PROCEDURE ValidateRgn (hWnd: HWND; hRgn: HRGN): BOOL;

<*EXTERNAL RedrawWindow:WINAPI*>
PROCEDURE RedrawWindow (hwnd      : HWND;
                          lprcUpdate: LPRECT;
                          hrgnUpdate: HRGN;
                          flags     : UINT               ): BOOL;

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
                          XAmount   : int;
                          YAmount   : int;
                          lpRect    : LPRECT;
                          lpClipRect: LPRECT  ): BOOL;

<*EXTERNAL ScrollDC:WINAPI*>
PROCEDURE ScrollDC (hDC       : HDC;
                      dx        : int;
                      dy        : int;
                      lprcScroll: LPRECT;
                      lprcClip  : LPRECT;
                      hrgnUpdate: HRGN;
                      lprcUpdate: LPRECT             ): BOOL;

<*EXTERNAL ScrollWindowEx:WINAPI*>
PROCEDURE ScrollWindowEx (hwnd      : HWND;
                            dx        : int;
                            dy        : int;
                            prcScroll : LPRECT;
                            prcClip   : LPRECT;
                            hrgnUpdate: HRGN;
                            prcUpdate : LPRECT;
                            flags     : UINT               ): int;

CONST
  SW_SCROLLCHILDREN = 16_0001;  (* Scroll children within *lprcScroll. *)
  SW_INVALIDATE     = 16_0002;  (* Invalidate after scrolling *)
  SW_ERASE = 16_0004;           (* If SW_INVALIDATE, don't send
                                   WM_ERASEBACKGROUND *)

<*EXTERNAL SetScrollPos:WINAPI*>
PROCEDURE SetScrollPos (hWnd: HWND; nBar: int; nPos: int; bRedraw: BOOL): int;

<*EXTERNAL GetScrollPos:WINAPI*>
PROCEDURE GetScrollPos (hWnd: HWND; nBar: int): int;

<*EXTERNAL SetScrollRange:WINAPI*>
PROCEDURE SetScrollRange (hWnd   : HWND;
                            nBar   : int;
                            nMinPos: int;
                            nMaxPos: int;
                            bRedraw: BOOL  ): BOOL;

<*EXTERNAL GetScrollRange:WINAPI*>
PROCEDURE GetScrollRange (hWnd    : HWND;
                            nBar    : int;
                            lpMinPos: LPINT;
                            lpMaxPos: LPINT  ): BOOL;

<*EXTERNAL ShowScrollBar:WINAPI*>
PROCEDURE ShowScrollBar (hWnd: HWND; wBar: int; bShow: BOOL): BOOL;

<*EXTERNAL EnableScrollBar:WINAPI*>
PROCEDURE EnableScrollBar (hwnd: HWND; wSBflags: UINT; wArrows: UINT): BOOL;

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
PROCEDURE SetPropA (hWnd: HWND; lpString: LPCSTR; hData: HANDLE): BOOL;

<*EXTERNAL SetPropW:WINAPI*>
PROCEDURE SetPropW (hWnd: HWND; lpString: LPCWSTR; hData: HANDLE): BOOL;
CONST SetProp = SetPropA;

<*EXTERNAL GetPropA:WINAPI*>
PROCEDURE GetPropA (hWnd: HWND; lpString: LPCSTR): HANDLE;

<*EXTERNAL GetPropW:WINAPI*>
PROCEDURE GetPropW (hWnd: HWND; lpString: LPCWSTR): HANDLE;
CONST GetProp = GetPropA;

<*EXTERNAL RemovePropA:WINAPI*>
PROCEDURE RemovePropA (hWnd: HWND; lpString: LPCSTR): HANDLE;

<*EXTERNAL RemovePropW:WINAPI*>
PROCEDURE RemovePropW (hWnd: HWND; lpString: LPCWSTR): HANDLE;
CONST RemoveProp = RemovePropA;

<*EXTERNAL EnumPropsExA:WINAPI*>
PROCEDURE EnumPropsExA (hWnd      : HWND;
                          lpEnumFunc: PROPENUMPROC;
                          lParam    : LPARAM        ): int;

<*EXTERNAL EnumPropsExW:WINAPI*>
PROCEDURE EnumPropsExW (hWnd      : HWND;
                          lpEnumFunc: PROPENUMPROC;
                          lParam    : LPARAM        ): int;
CONST EnumPropsEx = EnumPropsExA;

<*EXTERNAL EnumPropsA:WINAPI*>
PROCEDURE EnumPropsA (hWnd: HWND; lpEnumFunc: PROPENUMPROC): int;

<*EXTERNAL EnumPropsW:WINAPI*>
PROCEDURE EnumPropsW (hWnd: HWND; lpEnumFunc: PROPENUMPROC): int;
CONST EnumProps = EnumPropsA;

<*EXTERNAL SetWindowTextA:WINAPI*>
PROCEDURE SetWindowTextA (hWnd: HWND; lpString: LPCSTR): BOOL;

<*EXTERNAL SetWindowTextW:WINAPI*>
PROCEDURE SetWindowTextW (hWnd: HWND; lpString: LPCWSTR): BOOL;
CONST SetWindowText = SetWindowTextA;

<*EXTERNAL GetWindowTextA:WINAPI*>
PROCEDURE GetWindowTextA (hWnd: HWND; lpString: LPSTR; nMaxCount: int): int;

<*EXTERNAL GetWindowTextW:WINAPI*>
PROCEDURE GetWindowTextW (hWnd: HWND; lpString: LPWSTR; nMaxCount: int): int;
CONST GetWindowText = GetWindowTextA;

<*EXTERNAL GetWindowTextLengthA:WINAPI*>
PROCEDURE GetWindowTextLengthA (hWnd: HWND): int;

<*EXTERNAL GetWindowTextLengthW:WINAPI*>
PROCEDURE GetWindowTextLengthW (hWnd: HWND): int;
CONST GetWindowTextLength = GetWindowTextLengthA;

<*EXTERNAL GetClientRect:WINAPI*>
PROCEDURE raw_GetClientRect (hWnd: HWND; lpRect: LPRECT): BOOL;

PROCEDURE GetClientRect (hWnd: HWND; lpRect: LPRECT): BOOL;

<*EXTERNAL GetWindowRect:WINAPI*>
PROCEDURE GetWindowRect (hWnd: HWND; lpRect: LPRECT): BOOL;

<*EXTERNAL AdjustWindowRect:WINAPI*>
PROCEDURE AdjustWindowRect (lpRect: LPRECT; dwStyle: DWORD; bMenu: BOOL): BOOL;

<*EXTERNAL AdjustWindowRectEx:WINAPI*>
PROCEDURE AdjustWindowRectEx (lpRect   : LPRECT;
                                dwStyle  : DWORD;
                                bMenu    : BOOL;
                                dwExStyle: DWORD   ): BOOL;

(* MessageBox() Flags *)
CONST
  MB_OK              : LONG = 16_0000;
  MB_OKCANCEL        : LONG = 16_0001;
  MB_ABORTRETRYIGNORE: LONG = 16_0002;
  MB_YESNOCANCEL     : LONG = 16_0003;
  MB_YESNO           : LONG = 16_0004;
  MB_RETRYCANCEL     : LONG = 16_0005;

  MB_ICONHAND       : LONG = 16_0010;
  MB_ICONQUESTION   : LONG = 16_0020;
  MB_ICONEXCLAMATION: LONG = 16_0030;
  MB_ICONASTERISK   : LONG = 16_0040;

  MB_ICONINFORMATION = MB_ICONASTERISK;
  MB_ICONSTOP        = MB_ICONHAND;

  MB_DEFBUTTON1: LONG = 16_0000;
  MB_DEFBUTTON2: LONG = 16_0100;
  MB_DEFBUTTON3: LONG = 16_0200;

  MB_APPLMODAL  : LONG = 16_0000;
  MB_SYSTEMMODAL: LONG = 16_1000;
  MB_TASKMODAL  : LONG = 16_2000;

  MB_NOFOCUS             : LONG = 16_8000;
  MB_SETFOREGROUND       : LONG = 16_10000;
  MB_DEFAULT_DESKTOP_ONLY: LONG = 16_20000;

  MB_TYPEMASK: LONG = 16_000F;
  MB_ICONMASK: LONG = 16_00F0;
  MB_DEFMASK : LONG = 16_0F00;
  MB_MODEMASK: LONG = 16_3000;
  MB_MISCMASK: LONG = 16_C000;

<*EXTERNAL MessageBoxExA:WINAPI*>
PROCEDURE MessageBoxExA (hWnd       : HWND;
                           lpText     : LPCSTR;
                           lpCaption  : LPCSTR;
                           uType      : UINT;
                           wLanguageId: WORD    ): int;

<*EXTERNAL MessageBoxExW:WINAPI*>
PROCEDURE MessageBoxExW (hWnd       : HWND;
                           lpText     : LPCWSTR;
                           lpCaption  : LPCWSTR;
                           uType      : UINT;
                           wLanguageId: WORD     ): int;
CONST MessageBoxEx = MessageBoxExA;

PROCEDURE MessageBoxA (hWnd     : HWND;
                       lpText   : LPCSTR;
                       lpCaption: LPCSTR;
                       uType    : UINT    ): int;

PROCEDURE MessageBoxW (hWnd     : HWND;
                       lpText   : LPCWSTR;
                       lpCaption: LPCWSTR;
                       uType    : UINT     ): int;

CONST MessageBox = MessageBoxA;

<*EXTERNAL MessageBeep:WINAPI*>
PROCEDURE MessageBeep (uType: UINT): BOOL;

<*EXTERNAL ShowCursor:WINAPI*>
PROCEDURE ShowCursor (bShow: BOOL): int;

<*EXTERNAL SetCursorPos:WINAPI*>
PROCEDURE SetCursorPos (X: int; Y: int): BOOL;

<*EXTERNAL SetCursor:WINAPI*>
PROCEDURE SetCursor (hCursor: HCURSOR): HCURSOR;

<*EXTERNAL GetCursorPos:WINAPI*>
PROCEDURE raw_GetCursorPos (lpPoint: LPPOINT): BOOL;

PROCEDURE GetCursorPos (lpPoint: LPPOINT): BOOL;

<*EXTERNAL ClipCursor:WINAPI*>
PROCEDURE ClipCursor (lpRect: LPRECT): BOOL;

<*EXTERNAL GetClipCursor:WINAPI*>
PROCEDURE GetClipCursor (lpRect: LPRECT): BOOL;

<*EXTERNAL GetCursor:WINAPI*>
PROCEDURE GetCursor (): HCURSOR;

<*EXTERNAL CreateCaret:WINAPI*>
PROCEDURE CreateCaret (hWnd   : HWND;
                       hBitmap: HBITMAP;
                       nWidth : int;
                       nHeight: int      ): BOOL;

<*EXTERNAL GetCaretBlinkTime:WINAPI*>
PROCEDURE GetCaretBlinkTime (): UINT;

<*EXTERNAL SetCaretBlinkTime:WINAPI*>
PROCEDURE SetCaretBlinkTime (uMSeconds: UINT): BOOL;

<*EXTERNAL DestroyCaret:WINAPI*>
PROCEDURE DestroyCaret (): BOOL;

<*EXTERNAL HideCaret:WINAPI*>
PROCEDURE HideCaret (hWnd: HWND): BOOL;

<*EXTERNAL ShowCaret:WINAPI*>
PROCEDURE ShowCaret (hWnd: HWND): BOOL;

<*EXTERNAL SetCaretPos:WINAPI*>
PROCEDURE SetCaretPos (X: int; Y: int): BOOL;

<*EXTERNAL GetCaretPos:WINAPI*>
PROCEDURE GetCaretPos (lpPoint: LPPOINT): BOOL;

<*EXTERNAL ClientToScreen:WINAPI*>
PROCEDURE raw_ClientToScreen (hWnd: HWND; lpPoint: LPPOINT): BOOL;

PROCEDURE ClientToScreen (hWnd: HWND; lpPoint: LPPOINT): BOOL;

<*EXTERNAL ScreenToClient:WINAPI*>
PROCEDURE raw_ScreenToClient (hWnd: HWND; lpPoint: LPPOINT): BOOL;

PROCEDURE ScreenToClient (hWnd: HWND; lpPoint: LPPOINT): BOOL;

<*EXTERNAL MapWindowPoints:WINAPI*>
PROCEDURE MapWindowPoints (hWndFrom: HWND;
                             hWndTo  : HWND;
                             lpPoints: LPPOINT;
                             cPoints : UINT     ): int;

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
PROCEDURE GetSysColor (nIndex: int): DWORD;

<*EXTERNAL SetSysColors:WINAPI*>
PROCEDURE SetSysColors (a1: int;
                          a2: UNTRACED REF INT;
                          a3: UNTRACED REF COLORREF): BOOL;

<*EXTERNAL DrawFocusRect:WINAPI*>
PROCEDURE DrawFocusRect (a1: HDC; a2: LPRECT): BOOL;

<*EXTERNAL FillRect:WINAPI*>
PROCEDURE FillRect (hdc: HDC; lprc: LPRECT; hbr: HBRUSH): int;

<*EXTERNAL FrameRect:WINAPI*>
PROCEDURE FrameRect (hdc: HDC; lprc: LPRECT; hbr: HBRUSH): int;

<*EXTERNAL InvertRect:WINAPI*>
PROCEDURE InvertRect (hdc: HDC; lprc: LPRECT): BOOL;

<*EXTERNAL SetRect:WINAPI*>
PROCEDURE SetRect (a1: LPRECT; a2: int; a3: int; a4: int; a5: int): BOOL;

<*EXTERNAL SetRectEmpty:WINAPI*>
PROCEDURE SetRectEmpty (a1: LPRECT): BOOL;

<*EXTERNAL CopyRect:WINAPI*>
PROCEDURE CopyRect (a1: LPRECT; a2: LPRECT): int;

<*EXTERNAL InflateRect:WINAPI*>
PROCEDURE InflateRect (a1: LPRECT; a2: int; a3: int): BOOL;

<*EXTERNAL IntersectRect:WINAPI*>
PROCEDURE IntersectRect (a1: LPRECT;
                           a2: LPRECT;
                           a3: LPRECT  ): int;

<*EXTERNAL UnionRect:WINAPI*>
PROCEDURE UnionRect (a1: LPRECT;
                       a2: LPRECT;
                       a3: LPRECT  ): BOOL;

<*EXTERNAL SubtractRect:WINAPI*>
PROCEDURE SubtractRect (a1: LPRECT;
                          a2: LPRECT;
                          a3: LPRECT  ): BOOL;

<*EXTERNAL OffsetRect:WINAPI*>
PROCEDURE OffsetRect (a1: LPRECT; a2: int; a3: int): BOOL;

<*EXTERNAL IsRectEmpty:WINAPI*>
PROCEDURE IsRectEmpty (lprc: LPRECT): BOOL;

<*EXTERNAL EqualRect:WINAPI*>
PROCEDURE EqualRect (a1: LPRECT; a2: LPRECT): BOOL;

<*EXTERNAL PtInRect:WINAPI*>
PROCEDURE PtInRect (a1: LPRECT; a2: POINT): BOOL;

<*EXTERNAL GetWindowWord:WINAPI*>
PROCEDURE GetWindowWord (hWnd: HWND; nIndex: int): WORD;

<*EXTERNAL SetWindowWord:WINAPI*>
PROCEDURE SetWindowWord (hWnd: HWND; nIndex: int; wNewWord: WORD): WORD;

<*EXTERNAL GetWindowLongA:WINAPI*>
PROCEDURE GetWindowLongA (hWnd: HWND; nIndex: int): LONG;

<*EXTERNAL GetWindowLongW:WINAPI*>
PROCEDURE GetWindowLongW (hWnd: HWND; nIndex: int): LONG;
CONST GetWindowLong = GetWindowLongA;

<*EXTERNAL SetWindowLongA:WINAPI*>
PROCEDURE SetWindowLongA (hWnd: HWND; nIndex: int; dwNewLong: LONG): LONG;

<*EXTERNAL SetWindowLongW:WINAPI*>
PROCEDURE SetWindowLongW (hWnd: HWND; nIndex: int; dwNewLong: LONG): LONG;
CONST SetWindowLong = SetWindowLongA;

<*EXTERNAL GetClassWord:WINAPI*>
PROCEDURE GetClassWord (hWnd: HWND; nIndex: int): WORD;

<*EXTERNAL SetClassWord:WINAPI*>
PROCEDURE SetClassWord (hWnd: HWND; nIndex: int; wNewWord: WORD): WORD;

<*EXTERNAL GetClassLongA:WINAPI*>
PROCEDURE GetClassLongA (hWnd: HWND; nIndex: int): DWORD;

<*EXTERNAL GetClassLongW:WINAPI*>
PROCEDURE GetClassLongW (hWnd: HWND; nIndex: int): DWORD;
CONST GetClassLong = GetClassLongA;

<*EXTERNAL SetClassLongA:WINAPI*>
PROCEDURE SetClassLongA (hWnd: HWND; nIndex: int; dwNewLong: LONG): DWORD;

<*EXTERNAL SetClassLongW:WINAPI*>
PROCEDURE SetClassLongW (hWnd: HWND; nIndex: int; dwNewLong: LONG): DWORD;
CONST SetClassLong = SetClassLongA;

<*EXTERNAL GetDesktopWindow:WINAPI*>
PROCEDURE GetDesktopWindow (): HWND;

<*EXTERNAL SetDeskWallpaper:WINAPI*>
PROCEDURE SetDeskWallpaper (lpString: LPCSTR): BOOL;

<*EXTERNAL GetParent:WINAPI*>
PROCEDURE GetParent (hWnd: HWND): HWND;

<*EXTERNAL SetParent:WINAPI*>
PROCEDURE SetParent (hWndChild: HWND; hWndNewParent: HWND): HWND;

<*EXTERNAL EnumChildWindows:WINAPI*>
PROCEDURE EnumChildWindows (hWndParent: HWND;
                              lpEnumFunc: WNDENUMPROC;
                              lParam    : LPARAM       ): BOOL;

<*EXTERNAL FindWindowA:WINAPI*>
PROCEDURE FindWindowA (lpClassName: LPCSTR; lpWindowName: LPCSTR): HWND;

<*EXTERNAL FindWindowW:WINAPI*>
PROCEDURE FindWindowW (lpClassName: LPCWSTR; lpWindowName: LPCWSTR): HWND;
CONST FindWindow = FindWindowA;

<*EXTERNAL EnumWindows:WINAPI*>
PROCEDURE EnumWindows (lpEnumFunc: WNDENUMPROC; lParam: LPARAM): BOOL;

<*EXTERNAL EnumThreadWindows:WINAPI*>
PROCEDURE EnumThreadWindows (dwThreadId: DWORD;
                               lpfn      : WNDENUMPROC;
                               lParam    : LPARAM       ): BOOL;

PROCEDURE EnumTaskWindows (dwThreadId: DWORD;
                           lpfn      : WNDENUMPROC;
                           lParam    : LPARAM       ): BOOL;

<*EXTERNAL GetClassNameA:WINAPI*>
PROCEDURE GetClassNameA (hWnd: HWND; lpClassName: LPSTR; nMaxCount: int): int;

<*EXTERNAL GetClassNameW:WINAPI*>
PROCEDURE GetClassNameW (hWnd: HWND; lpClassName: LPWSTR; nMaxCount: int): int;
CONST GetClassName = GetClassNameA;

<*EXTERNAL GetTopWindow:WINAPI*>
PROCEDURE GetTopWindow (hWnd: HWND): HWND;

PROCEDURE GetNextWindow(hWnd: HWND; uCmd: UINT):HWND;

<*EXTERNAL GetWindowThreadProcessId:WINAPI*>
PROCEDURE GetWindowThreadProcessId (hWnd: HWND; lpdwProcessId: LPDWORD): DWORD;

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
PROCEDURE GetWindow (hWnd: HWND; uCmd: UINT): HWND;

<*EXTERNAL SetWindowsHookA:WINAPI*>
PROCEDURE SetWindowsHookA (nFilterType: int; pfnFilterProc: HOOKPROC): HHOOK;

<*EXTERNAL SetWindowsHookW:WINAPI*>
PROCEDURE SetWindowsHookW (nFilterType: int; fnFilterProc: HOOKPROC): HHOOK;
CONST SetWindowsHook = SetWindowsHookA;

<*EXTERNAL UnhookWindowsHook:WINAPI*>
PROCEDURE UnhookWindowsHook (nCode: int; pfnFilterProc: HOOKPROC): BOOL;

<*EXTERNAL SetWindowsHookExA:WINAPI*>
PROCEDURE SetWindowsHookExA (idHook    : int;
                               lpfn      : HOOKPROC;
                               hmod      : HINSTANCE;
                               dwThreadId: DWORD      ): HHOOK;

<*EXTERNAL SetWindowsHookExW:WINAPI*>
PROCEDURE SetWindowsHookExW (idHook    : int;
                               lpfn      : HOOKPROC;
                               hmod      : HINSTANCE;
                               dwThreadId: DWORD      ): HHOOK;
CONST SetWindowsHookEx = SetWindowsHookExA;

<*EXTERNAL UnhookWindowsHookEx:WINAPI*>
PROCEDURE UnhookWindowsHookEx (hhk: HHOOK): BOOL;

<*EXTERNAL CallNextHookEx:WINAPI*>
PROCEDURE CallNextHookEx (hhk   : HHOOK;
                            nCode : int;
                            wParam: WPARAM;
                            lParam: LPARAM  ): LRESULT;

(*
 * Macros for source-level compatibility with old functions.
 *)

PROCEDURE DefHookProc (nCode : int;
                       wParam: WPARAM;
                       lParam: LPARAM;
                       phhk  : UNTRACED REF HHOOK): LRESULT;

(* Menu flags for Add/Check/EnableMenuItem() *)
CONST
  MF_INSERT: LONG = 16_00000000;
  MF_CHANGE: LONG = 16_00000080;
  MF_APPEND: LONG = 16_00000100;
  MF_DELETE: LONG = 16_00000200;
  MF_REMOVE: LONG = 16_00001000;

  MF_BYCOMMAND : LONG = 16_00000000;
  MF_BYPOSITION: LONG = 16_00000400;

  MF_SEPARATOR: LONG = 16_00000800;

  MF_ENABLED : LONG = 16_00000000;
  MF_GRAYED  : LONG = 16_00000001;
  MF_DISABLED: LONG = 16_00000002;

  MF_UNCHECKED      : LONG = 16_00000000;
  MF_CHECKED        : LONG = 16_00000008;
  MF_USECHECKBITMAPS: LONG = 16_00000200;

  MF_STRING   : LONG = 16_00000000;
  MF_BITMAP   : LONG = 16_00000004;
  MF_OWNERDRAW: LONG = 16_00000100;

  MF_POPUP       : LONG = 16_00000010;
  MF_MENUBARBREAK: LONG = 16_00000020;
  MF_MENUBREAK   : LONG = 16_00000040;

  MF_UNHILITE: LONG = 16_00000000;
  MF_HILITE  : LONG = 16_00000080;

  MF_SYSMENU    : LONG = 16_00002000;
  MF_HELP       : LONG = 16_00004000;
  MF_MOUSESELECT: LONG = 16_00008000;

(* Menu item resource format *)
TYPE
  MENUITEMTEMPLATEHEADER = RECORD
                             versionNumber: WORD;
                             offset       : WORD;
  END;

  MENUITEMTEMPLATE = RECORD
                       mtOption: WORD;
                       mtID    : WORD;
                       mtString: ARRAY [0 .. 1 - 1] OF char;
  END;

CONST MF_END: LONG = 16_00000080;

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
PROCEDURE LoadBitmapA (hInstance: HINSTANCE; lpBitmapName: LPCSTR): HBITMAP;

<*EXTERNAL LoadBitmapW:WINAPI*>
PROCEDURE LoadBitmapW (hInstance: HINSTANCE; lpBitmapName: LPCWSTR): HBITMAP;
CONST LoadBitmap = LoadBitmapA;

<*EXTERNAL LoadCursorA:WINAPI*>
PROCEDURE LoadCursorA (hInstance: HINSTANCE; lpCursorName: LPCSTR): HCURSOR;

<*EXTERNAL LoadCursorW:WINAPI*>
PROCEDURE LoadCursorW (hInstance: HINSTANCE; lpCursorName: LPCWSTR): HCURSOR;
CONST LoadCursor = LoadCursorA;

<*EXTERNAL CreateCursor:WINAPI*>
PROCEDURE CreateCursor (a1: HINSTANCE;
                        a2: int;
                        a3: int;
                        a4: int;
                        a5: int;
                        a6: void_star;
                        a7: void_star  ): HCURSOR;

<*EXTERNAL DestroyCursor:WINAPI*>
PROCEDURE DestroyCursor (a1: HCURSOR): BOOL;

(* Standard Cursor IDs *)
VAR                             (* CONST *)
  IDC_ARROW      : LPTSTR;
  IDC_IBEAM      : LPTSTR;
  IDC_WAIT       : LPTSTR;
  IDC_CROSS      : LPTSTR;
  IDC_UPARROW    : LPTSTR;
  IDC_SIZE       : LPTSTR;
  IDC_ICON       : LPTSTR;
  IDC_SIZENWSE   : LPTSTR;
  IDC_SIZENESW   : LPTSTR;
  IDC_SIZEWE     : LPTSTR;
  IDC_SIZENS     : LPTSTR;
  IDC_SIZEALL    : LPTSTR;      (* not in win3.1 *)
  IDC_NO         : LPTSTR;      (* not in win3.1 *)
  IDC_APPSTARTING: LPTSTR;      (* not in win3.1 *)

TYPE
  ICONINFO = RECORD
    fIcon   : BOOL;
    xHotspot: DWORD;
    yHotspot: DWORD;
    hbmMask : HBITMAP;
    hbmColor: HBITMAP;
  END;
  PICONINFO = UNTRACED REF ICONINFO;

<*EXTERNAL LoadIconA:WINAPI*>
PROCEDURE LoadIconA (hInstance: HINSTANCE; lpIconName: LPCSTR): HICON;

<*EXTERNAL LoadIconW:WINAPI*>
PROCEDURE LoadIconW (hInstance: HINSTANCE; lpIconName: LPCWSTR): HICON;
CONST LoadIcon = LoadIconA;

<*EXTERNAL CreateIcon:WINAPI*>
PROCEDURE CreateIcon (a1: HINSTANCE;
                      a2: int;
                      a3: int;
                      a4: BYTE;
                      a5: BYTE;
                      a6: UNTRACED REF BYTE;
                      a7: UNTRACED REF BYTE  ): HICON;

<*EXTERNAL DestroyIcon:WINAPI*>
PROCEDURE DestroyIcon (a1: HICON): BOOL;

<*EXTERNAL LookupIconIdFromDirectory:WINAPI*>
PROCEDURE LookupIconIdFromDirectory (presbits: PBYTE; fIcon: BOOL): int;

<*EXTERNAL CreateIconFromResource:WINAPI*>
PROCEDURE CreateIconFromResource (presbits : PBYTE;
                                  dwResSize: DWORD;
                                  fIcon    : BOOL;
                                  dwVer    : DWORD  ): HICON;

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
  IDI_APPLICATION: LPTSTR;
  IDI_HAND       : LPTSTR;
  IDI_QUESTION   : LPTSTR;
  IDI_EXCLAMATION: LPTSTR;
  IDI_ASTERISK   : LPTSTR;

<*EXTERNAL LoadStringA:WINAPI*>
PROCEDURE LoadStringA (hInstance : HINSTANCE;
                         uID       : UINT;
                         lpBuffer  : LPSTR;
                         nBufferMax: int        ): int;

<*EXTERNAL LoadStringW:WINAPI*>
PROCEDURE LoadStringW (hInstance : HINSTANCE;
                         uID       : UINT;
                         lpBuffer  : LPWSTR;
                         nBufferMax: int        ): int;
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
  ES_LEFT       : LONG = 16_0000;
  ES_CENTER     : LONG = 16_0001;
  ES_RIGHT      : LONG = 16_0002;
  ES_MULTILINE  : LONG = 16_0004;
  ES_UPPERCASE  : LONG = 16_0008;
  ES_LOWERCASE  : LONG = 16_0010;
  ES_PASSWORD   : LONG = 16_0020;
  ES_AUTOVSCROLL: LONG = 16_0040;
  ES_AUTOHSCROLL: LONG = 16_0080;
  ES_NOHIDESEL  : LONG = 16_0100;
  ES_OEMCONVERT : LONG = 16_0400;
  ES_READONLY   : LONG = 16_0800;
  ES_WANTRETURN : LONG = 16_1000;

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
  BS_PUSHBUTTON     : LONG = 16_00;
  BS_DEFPUSHBUTTON  : LONG = 16_01;
  BS_CHECKBOX       : LONG = 16_02;
  BS_AUTOCHECKBOX   : LONG = 16_03;
  BS_RADIOBUTTON    : LONG = 16_04;
  BS_3STATE         : LONG = 16_05;
  BS_AUTO3STATE     : LONG = 16_06;
  BS_GROUPBOX       : LONG = 16_07;
  BS_USERBUTTON     : LONG = 16_08;
  BS_AUTORADIOBUTTON: LONG = 16_09;
  BS_PUSHBOX        : LONG = 16_0A;
  BS_OWNERDRAW      : LONG = 16_0B;
  BS_LEFTTEXT       : LONG = 16_20;

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
  SS_LEFT          : LONG = 16_00;
  SS_CENTER        : LONG = 16_01;
  SS_RIGHT         : LONG = 16_02;
  SS_ICON          : LONG = 16_03;
  SS_BLACKRECT     : LONG = 16_04;
  SS_GRAYRECT      : LONG = 16_05;
  SS_WHITERECT     : LONG = 16_06;
  SS_BLACKFRAME    : LONG = 16_07;
  SS_GRAYFRAME     : LONG = 16_08;
  SS_WHITEFRAME    : LONG = 16_09;
  SS_USERITEM      : LONG = 16_0A;
  SS_SIMPLE        : LONG = 16_0B;
  SS_LEFTNOWORDWRAP: LONG = 16_0C;
  SS_NOPREFIX      : LONG = 16_80; (* Don't do "&" character translation *)

(* Static Control Mesages *)
CONST
  STM_SETICON = 16_170;
  STM_GETICON = 16_171;
  STM_MSGMAX  = 16_172;

(*
 * Dialog window class
 *)
VAR                             (* CONST *)
  WC_DIALOG: LPTSTR;

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
PROCEDURE MapDialogRect (hDlg: HWND; lpRect: LPRECT): BOOL;

<*EXTERNAL DlgDirListA:WINAPI*>
PROCEDURE DlgDirListA (hDlg         : HWND;
                         lpPathSpec   : LPSTR;
                         nIDListBox   : int;
                         nIDStaticPath: int;
                         uFileType    : UINT   ): int;

<*EXTERNAL DlgDirListW:WINAPI*>
PROCEDURE DlgDirListW (hDlg         : HWND;
                         lpPathSpec   : LPWSTR;
                         nIDListBox   : int;
                         nIDStaticPath: int;
                         uFileType    : UINT    ): int;
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
                             lpString  : LPSTR;
                             nCount    : int;
                             nIDListBox: int    ): BOOL;

<*EXTERNAL DlgDirSelectExW:WINAPI*>
PROCEDURE DlgDirSelectExW (hDlg      : HWND;
                             lpString  : LPWSTR;
                             nCount    : int;
                             nIDListBox: int     ): BOOL;
CONST DlgDirSelectEx = DlgDirSelectExA;

<*EXTERNAL DlgDirListComboBoxA:WINAPI*>
PROCEDURE DlgDirListComboBoxA (hDlg         : HWND;
                                 lpPathSpec   : LPSTR;
                                 nIDComboBox  : int;
                                 nIDStaticPath: int;
                                 uFiletype    : UINT   ): int;

<*EXTERNAL DlgDirListComboBoxW:WINAPI*>
PROCEDURE DlgDirListComboBoxW (hDlg         : HWND;
                                 lpPathSpec   : LPWSTR;
                                 nIDComboBox  : int;
                                 nIDStaticPath: int;
                                 uFiletype    : UINT    ): int;
CONST DlgDirListComboBox = DlgDirListComboBoxA;

<*EXTERNAL DlgDirSelectComboBoxExA:WINAPI*>
PROCEDURE DlgDirSelectComboBoxExA (hDlg       : HWND;
                                     lpString   : LPSTR;
                                     nCount     : int;
                                     nIDComboBox: int    ): BOOL;

<*EXTERNAL DlgDirSelectComboBoxExW:WINAPI*>
PROCEDURE DlgDirSelectComboBoxExW (hDlg       : HWND;
                                     lpString   : LPWSTR;
                                     nCount     : int;
                                     nIDComboBox: int     ): BOOL;
CONST DlgDirSelectComboBoxEx = DlgDirSelectComboBoxExA;

(* Dialog Styles *)
CONST
  DS_ABSALIGN  : LONG = 16_01;
  DS_SYSMODAL  : LONG = 16_02;
  DS_LOCALEDIT : LONG = 16_20;  (* Edit items get Local storage. *)
  DS_SETFONT   : LONG = 16_40;  (* User specified font for Dlg controls *)
  DS_MODALFRAME: LONG = 16_80;  (* Can be combined with WS_CAPTION *)
  DS_NOIDLEMSG : LONG = 16_100; (* WM_ENTERIDLE message will not be sent *)
  DS_SETFOREGROUND: LONG = 16_200; (* not in win3.1 *)

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

  LB_CTLCODE: LONG = 0;

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
  LBS_NOTIFY           : LONG = 16_0001;
  LBS_SORT             : LONG = 16_0002;
  LBS_NOREDRAW         : LONG = 16_0004;
  LBS_MULTIPLESEL      : LONG = 16_0008;
  LBS_OWNERDRAWFIXED   : LONG = 16_0010;
  LBS_OWNERDRAWVARIABLE: LONG = 16_0020;
  LBS_HASSTRINGS       : LONG = 16_0040;
  LBS_USETABSTOPS      : LONG = 16_0080;
  LBS_NOINTEGRALHEIGHT : LONG = 16_0100;
  LBS_MULTICOLUMN      : LONG = 16_0200;
  LBS_WANTKEYBOARDINPUT: LONG = 16_0400;
  LBS_EXTENDEDSEL      : LONG = 16_0800;
  LBS_DISABLENOSCROLL  : LONG = 16_1000;
  LBS_NODATA           : LONG = 16_2000;
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
  CBS_SIMPLE           : LONG = 16_0001;
  CBS_DROPDOWN         : LONG = 16_0002;
  CBS_DROPDOWNLIST     : LONG = 16_0003;
  CBS_OWNERDRAWFIXED   : LONG = 16_0010;
  CBS_OWNERDRAWVARIABLE: LONG = 16_0020;
  CBS_AUTOHSCROLL      : LONG = 16_0040;
  CBS_OEMCONVERT       : LONG = 16_0080;
  CBS_SORT             : LONG = 16_0100;
  CBS_HASSTRINGS       : LONG = 16_0200;
  CBS_NOINTEGRALHEIGHT : LONG = 16_0400;
  CBS_DISABLENOSCROLL  : LONG = 16_0800;

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
  SBS_HORZ                   : LONG = 16_0000;
  SBS_VERT                   : LONG = 16_0001;
  SBS_TOPALIGN               : LONG = 16_0002;
  SBS_LEFTALIGN              : LONG = 16_0002;
  SBS_BOTTOMALIGN            : LONG = 16_0004;
  SBS_RIGHTALIGN             : LONG = 16_0004;
  SBS_SIZEBOXTOPLEFTALIGN    : LONG = 16_0002;
  SBS_SIZEBOXBOTTOMRIGHTALIGN: LONG = 16_0004;
  SBS_SIZEBOX                : LONG = 16_0008;

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
                       szClass: LPCSTR;
                       szTitle: LPCSTR;
                       hOwner : HANDLE;
                       x      : int;
                       y      : int;
                       cx     : int;
                       cy     : int;
    style  : DWORD;
    lParam : LPARAM;  (* app-defined stuff *)
  END;
  LPMDICREATESTRUCTA = UNTRACED REF MDICREATESTRUCTA;
  MDICREATESTRUCTW = RECORD
    szClass: LPCWSTR;
    szTitle: LPCWSTR;
    hOwner : HANDLE;
    x      : int;
    y      : int;
    cx     : int;
    cy     : int;
    style  : DWORD;
    lParam : LPARAM;   (* app-defined stuff *)
  END;
  LPMDICREATESTRUCTW = UNTRACED REF MDICREATESTRUCTW;
  MDICREATESTRUCT = MDICREATESTRUCTA;
  LPMDICREATESTRUCT = LPMDICREATESTRUCTA;

  CLIENTCREATESTRUCT = RECORD
    hWindowMenu : HANDLE;
    idFirstChild: UINT;
  END;
  LPCLIENTCREATESTRUCT = UNTRACED REF CLIENTCREATESTRUCT;

<*EXTERNAL DefFrameProcA:WINAPI*>
PROCEDURE DefFrameProcA (hWnd         : HWND;
    hWndMDIClient: HWND;
    uMsg         : UINT;
    wParam       : WPARAM;
                           lParam       : LPARAM  ): LRESULT;

<*EXTERNAL DefFrameProcW:WINAPI*>
PROCEDURE DefFrameProcW (hWnd         : HWND;
                           hWndMDIClient: HWND;
                           uMsg         : UINT;
                           wParam       : WPARAM;
                           lParam       : LPARAM  ): LRESULT;
CONST DefFrameProc = DefFrameProcA;

<*EXTERNAL DefMDIChildProcA:WINAPI*>
PROCEDURE DefMDIChildProcA (hWnd  : HWND;
                              uMsg  : UINT;
                              wParam: WPARAM;
                              lParam: LPARAM  ): LRESULT;

<*EXTERNAL DefMDIChildProcW:WINAPI*>
PROCEDURE DefMDIChildProcW (hWnd  : HWND;
                              uMsg  : UINT;
                              wParam: WPARAM;
                              lParam: LPARAM  ): LRESULT;
CONST DefMDIChildProc = DefMDIChildProcA;

<*EXTERNAL TranslateMDISysAccel:WINAPI*>
PROCEDURE TranslateMDISysAccel (hWndClient: HWND; lpMsg: LPMSG): BOOL;

<*EXTERNAL ArrangeIconicWindows:WINAPI*>
PROCEDURE ArrangeIconicWindows (hWnd: HWND): UINT;

<*EXTERNAL CreateMDIWindowA:WINAPI*>
PROCEDURE CreateMDIWindowA (lpClassName : LPSTR;
                              lpWindowName: LPSTR;
                              dwStyle     : DWORD;
                              X           : int;
                              Y           : int;
                              nWidth      : int;
                              nHeight     : int;
                              hWndParent  : HWND;
                              hInstance   : HINSTANCE;
                              lParam      : LONG       ): HWND;

<*EXTERNAL CreateMDIWindowW:WINAPI*>
PROCEDURE CreateMDIWindowW (lpClassName : LPWSTR;
                              lpWindowName: LPWSTR;
                              dwStyle     : DWORD;
                              X           : int;
                              Y           : int;
                              nWidth      : int;
                              nHeight     : int;
                              hWndParent  : HWND;
                              hInstance   : HINSTANCE;
                              lParam      : LONG       ): HWND;
CONST CreateMDIWindow = CreateMDIWindowA;

(****** Help support ********************************************************)

TYPE
  HELPPOLY = DWORD;
  MULTIKEYHELPA = RECORD
                    mkSize     : DWORD;
                    mkKeylist  : CHAR;
    szKeyphrase: ARRAY [0 .. 1 - 1] OF CHAR;
  END;
  PMULTIKEYHELPA = UNTRACED REF MULTIKEYHELPA;
  LPMULTIKEYHELPA = UNTRACED REF MULTIKEYHELPA;
  MULTIKEYHELPW = RECORD
    mkSize     : DWORD;
    mkKeylist  : WCHAR;
    szKeyphrase: ARRAY [0 .. 1 - 1] OF WCHAR;
  END;
  PMULTIKEYHELPW = UNTRACED REF MULTIKEYHELPW;
  LPMULTIKEYHELPW = UNTRACED REF MULTIKEYHELPW;
  MULTIKEYHELP = MULTIKEYHELPA;
  PMULTIKEYHELP = PMULTIKEYHELPA;
  LPMULTIKEYHELP = LPMULTIKEYHELPA;

  HELPWININFOA = RECORD
    wStructSize: int;
    x          : int;
    y          : int;
    dx         : int;
    dy         : int;
    wMax       : int;
    rgchMember : ARRAY [0 .. 2 - 1] OF CHAR;
  END;
  PHELPWININFOA = UNTRACED REF HELPWININFOA;
  LPHELPWININFOA = UNTRACED REF HELPWININFOA;
  HELPWININFOW = RECORD
    wStructSize: int;
    x          : int;
    y          : int;
    dx         : int;
    dy         : int;
    wMax       : int;
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
  HELP_CONTEXT   : LONG = 16_0001; (* Display topic in ulTopic *)
  HELP_QUIT      : LONG = 16_0002; (* Terminate help *)
  HELP_INDEX     : LONG = 16_0003; (* Display index *)
  HELP_CONTENTS  : LONG = 16_0003;
  HELP_HELPONHELP: LONG = 16_0004; (* Display help on using help *)
  HELP_SETINDEX: LONG = 16_0005; (* Set current Index for multi index
                                    help *)
  HELP_SETCONTENTS : LONG = 16_0005;
  HELP_CONTEXTPOPUP: LONG = 16_0008;
  HELP_FORCEFILE   : LONG = 16_0009;
  HELP_KEY: LONG = 16_0101;     (* Display topic for keyword in
                                   offabData *)
  HELP_COMMAND   : LONG = 16_0102;
  HELP_PARTIALKEY: LONG = 16_0105;
  HELP_MULTIKEY  : LONG = 16_0201;
  HELP_SETWINPOS : LONG = 16_0203;

<*EXTERNAL WinHelpA:WINAPI*>
PROCEDURE WinHelpA (hwndMain: HWND;
                    lpszHelp: LPCSTR;
                    uCommand: UINT;
                    dwData  : DWORD   ): BOOL;

<*EXTERNAL WinHelpW:WINAPI*>
PROCEDURE WinHelpW (hwndMain: HWND;
                    lpszHelp: LPCWSTR;
                    uCommand: UINT;
                    dwData  : DWORD    ): BOOL;
CONST WinHelp = WinHelpA;

(* function declarations for profiler routines contained in Windows
   libraries *)

<*EXTERNAL ProfInsChk:WINAPI*>
PROCEDURE ProfInsChk (): int;

<*EXTERNAL ProfSetup:WINAPI*>
PROCEDURE ProfSetup (a1: int; a2: int);

<*EXTERNAL ProfSampRate:WINAPI*>
PROCEDURE ProfSampRate (a1: int; a2: int);

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
PROCEDURE SystemParametersInfoA (a1, a2: UINT; a3: PVOID; a4: UINT): BOOL;

<*EXTERNAL SystemParametersInfoW:WINAPI*>
PROCEDURE SystemParametersInfoW (a1, a2: UINT; a3: PVOID; a4: UINT): BOOL;

CONST SystemParametersInfo = SystemParametersInfoA;

(* Flags *)
CONST
  SPIF_UPDATEINIFILE    = 16_0001;
  SPIF_SENDWININICHANGE = 16_0002;

END WinUser.
