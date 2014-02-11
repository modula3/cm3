(* Copyright (C) 1994, Digital Equipment Corporation         *)
(* All rights reserved.                                      *)
(* See the file COPYRIGHT for a full description.            *)
(*                                                           *)
(* by Stephen Harrison                                       *)
(*                                                           *)
(* Last modified on Thu Jun  1 10:18:09 PDT 1995 by kalsow   *)
(*      modified on Thu May 13 21:16:10 PDT 1993 by mjordan  *)
(*      modified on Fri Mar 19 11:14:26 PST 1993 by harrison *)

UNSAFE MODULE WinUser;

FROM WinNT IMPORT PCSTR, PCWSTR;
FROM WinDef IMPORT BOOL, UINT32, PVOID, HWND, WPARAM, LPARAM, INT32,
  LRESULT, HINSTANCE, HMENU, HHOOK, PRECT, PPOINT;

PROCEDURE ExitWindows (<*UNUSED*> dwReserved: UINT32;
                       <*UNUSED*> Code: UINT32): BOOL =
  BEGIN
    RETURN ExitWindowsEx(EWX_LOGOFF, 16_FFFFFFFF);
  END ExitWindows;

PROCEDURE PostAppMessageA (idThread: UINT32;
                           wMsg    : UINT32;
                           wParam  : WPARAM;
                           lParam  : LPARAM  ): BOOL =
  BEGIN
    RETURN PostThreadMessageA(idThread, wMsg, wParam, lParam);
  END PostAppMessageA;

PROCEDURE PostAppMessageW (idThread: UINT32;
                           wMsg    : UINT32;
                           wParam  : WPARAM;
                           lParam  : LPARAM  ): BOOL =
  BEGIN
    RETURN PostThreadMessageW(idThread, wMsg, wParam, lParam);
  END PostAppMessageW;

PROCEDURE CreateWindowA (lpClassName : PCSTR;
                         lpWindowName: PCSTR;
                         dwStyle     : INT32;
                         x           : INT32;
                         y           : INT32;
                         nWidth      : INT32;
                         nHeight     : INT32;
                         hwndParent  : HWND;
                         hMenu       : HMENU;
                         hInstance   : HINSTANCE;
                         lpParam     : PVOID     ): HWND =
  BEGIN
    RETURN
      CreateWindowExA(0, lpClassName, lpWindowName, dwStyle, x, y, nWidth,
                      nHeight, hwndParent, hMenu, hInstance, lpParam);
  END CreateWindowA;

PROCEDURE CreateWindowW (lpClassName : PCWSTR;
                         lpWindowName: PCWSTR;
                         dwStyle     : INT32;
                         x           : INT32;
                         y           : INT32;
                         nWidth      : INT32;
                         nHeight     : INT32;
                         hwndParent  : HWND;
                         hMenu       : HMENU;
                         hInstance   : HINSTANCE;
                         lpParam     : PVOID     ): HWND =
  BEGIN
    RETURN
      CreateWindowExW(0, lpClassName, lpWindowName, dwStyle, x, y, nWidth,
                      nHeight, hwndParent, hMenu, hInstance, lpParam);
  END CreateWindowW;

PROCEDURE CreateDialogA (hInstance   : HINSTANCE;
                         lpName      : PCSTR;
                         hwndParent  : HWND;
                         lpDialogFunc: DLGPROC    ): HWND =
  BEGIN
    RETURN
      CreateDialogParamA(hInstance, lpName, hwndParent, lpDialogFunc, 0);
  END CreateDialogA;

PROCEDURE CreateDialogW (hInstance   : HINSTANCE;
                         lpName      : PCWSTR;
                         hwndParent  : HWND;
                         lpDialogFunc: DLGPROC    ): HWND =
  BEGIN
    RETURN
      CreateDialogParamW(hInstance, lpName, hwndParent, lpDialogFunc, 0);
  END CreateDialogW;

PROCEDURE CreateDialogIndirectA (hInstance   : HINSTANCE;
                                 lpTemplate  : LPCDLGTEMPLATEA;
                                 hwndParent  : HWND;
                                 lpDialogFunc: DLGPROC          ): HWND =
  BEGIN
    RETURN CreateDialogIndirectParamA(
             hInstance, lpTemplate, hwndParent, lpDialogFunc, 0);
  END CreateDialogIndirectA;

PROCEDURE CreateDialogIndirectW (hInstance   : HINSTANCE;
                                 lpTemplate  : LPCDLGTEMPLATEW;
                                 hwndParent  : HWND;
                                 lpDialogFunc: DLGPROC          ): HWND =
  BEGIN
    RETURN CreateDialogIndirectParamW(
             hInstance, lpTemplate, hwndParent, lpDialogFunc, 0);
  END CreateDialogIndirectW;

PROCEDURE DialogBoxA (hInstance     : HINSTANCE;
                      lpTemplateName: PCSTR;
                      hWndParent    : HWND;
                      lpDialogFunc  : DLGPROC    ): INT32 =
  BEGIN
    RETURN
      DialogBoxParamA(hInstance, lpTemplateName, hWndParent, lpDialogFunc, 0);
  END DialogBoxA;

PROCEDURE DialogBoxW (hInstance     : HINSTANCE;
                      lpTemplateName: PCWSTR;
                      hWndParent    : HWND;
                      lpDialogFunc  : DLGPROC    ): INT32 =
  BEGIN
    RETURN
      DialogBoxParamW(hInstance, lpTemplateName, hWndParent, lpDialogFunc, 0);
  END DialogBoxW;

PROCEDURE DialogBoxIndirectA (hInstance      : HINSTANCE;
                              hDialogTemplate: LPDLGTEMPLATEA;
                              hWndParent     : HWND;
                              lpDialogFunc   : DLGPROC         ): INT32 =
  BEGIN
    RETURN DialogBoxIndirectParamA(
             hInstance, hDialogTemplate, hWndParent, lpDialogFunc, 0);
  END DialogBoxIndirectA;

PROCEDURE DialogBoxIndirectW (hInstance      : HINSTANCE;
                              hDialogTemplate: LPDLGTEMPLATEW;
                              hWndParent     : HWND;
                              lpDialogFunc   : DLGPROC         ): INT32 =
  BEGIN
    RETURN DialogBoxIndirectParamW(
             hInstance, hDialogTemplate, hWndParent, lpDialogFunc, 0);
  END DialogBoxIndirectW;

PROCEDURE MessageBoxA (hWnd     : HWND;
                       lpText   : PCSTR;
                       lpCaption: PCSTR;
                       uType    : UINT32    ): INT32 =
  BEGIN
    RETURN MessageBoxExA(hWnd, lpText, lpCaption, uType, 0);
  END MessageBoxA;

PROCEDURE MessageBoxW (hWnd     : HWND;
                       lpText   : PCWSTR;
                       lpCaption: PCWSTR;
                       uType    : UINT32    ): INT32 =
  BEGIN
    RETURN MessageBoxExW(hWnd, lpText, lpCaption, uType, 0);
  END MessageBoxW;

PROCEDURE EnumTaskWindows (dwThreadId: UINT32;
                           lpfn      : WNDENUMPROC;
                           lParam    : LPARAM       ): BOOL =
  BEGIN
    RETURN EnumThreadWindows(dwThreadId, lpfn, lParam);
  END EnumTaskWindows;

PROCEDURE GetNextWindow (hWnd: HWND; uCmd: UINT32): HWND =
  BEGIN
    RETURN GetWindow(hWnd, uCmd);
  END GetNextWindow;

PROCEDURE DefHookProc (nCode : INT32;
                       wParam: WPARAM;
                       lParam: LPARAM;
                       phhk  : UNTRACED REF HHOOK): LRESULT =
  BEGIN
    RETURN CallNextHookEx(phhk^, nCode, wParam, lParam);
  END DefHookProc;

(* hack to patch the buggy return values on Chicago *)

PROCEDURE GetClientRect (hWnd: HWND; lpRect: PRECT): BOOL =
  BEGIN
    RETURN ORD ((raw_GetClientRect (hWnd, lpRect)) # 0);
  END GetClientRect;

PROCEDURE GetCursorPos (lpPoint: PPOINT): BOOL =
  BEGIN
    RETURN ORD ((raw_GetCursorPos (lpPoint)) # 0);
  END GetCursorPos;

PROCEDURE ClientToScreen (hWnd: HWND; lpPoint: PPOINT): BOOL =
  BEGIN
    RETURN ORD ((raw_ClientToScreen (hWnd, lpPoint)) # 0);
  END ClientToScreen;

PROCEDURE ScreenToClient (hWnd: HWND; lpPoint: PPOINT): BOOL =
  BEGIN
    RETURN ORD ((raw_ScreenToClient (hWnd, lpPoint)) # 0);
  END ScreenToClient;

BEGIN

  RT_CURSOR := LOOPHOLE(1, ADDRESS);
  RT_BITMAP := LOOPHOLE(2, ADDRESS);
  RT_ICON := LOOPHOLE(3, ADDRESS);
  RT_MENU := LOOPHOLE(4, ADDRESS);
  RT_DIALOG := LOOPHOLE(5, ADDRESS);
  RT_STRING := LOOPHOLE(6, ADDRESS);
  RT_FONTDIR := LOOPHOLE(7, ADDRESS);
  RT_FONT := LOOPHOLE(8, ADDRESS);
  RT_ACCELERATOR := LOOPHOLE(9, ADDRESS);
  RT_RCDATA := LOOPHOLE(10, ADDRESS);
  RT_MESSAGETABLE := LOOPHOLE(11, ADDRESS);

  (* NOTE: if any new resource types are introduced above this point, then
     the ** value of DIFFERENCE must be changed.  ** (RT_GROUP_CURSOR -
     RT_CURSOR) must always be equal to DIFFERENCE ** (RT_GROUP_ICON -
     RT_ICON) must always be equal to DIFFERENCE *)
  RT_GROUP_CURSOR := LOOPHOLE(RT_CURSOR + DIFFERENCE, ADDRESS);
  (* The value RT_BITMAP+DIFFERENCE (13) is intentionally unused *)
  RT_GROUP_ICON := LOOPHOLE(RT_ICON + DIFFERENCE, ADDRESS);
  (* The value 15 is unused/obsolete *)
  RT_VERSION := LOOPHOLE(16, ADDRESS);
  RT_DLGINCLUDE := LOOPHOLE(17, ADDRESS);

  RT_PLUGPLAY := LOOPHOLE(19, ADDRESS);
  RT_VXD := LOOPHOLE(20, ADDRESS);
  RT_ANICURSOR := LOOPHOLE(21, ADDRESS);
  RT_ANIICON := LOOPHOLE(22, ADDRESS);
  RT_HTML := LOOPHOLE(23, ADDRESS);
  RT_MANIFEST := LOOPHOLE(24, ADDRESS);
  CREATEPROCESS_MANIFEST_RESOURCE_ID := LOOPHOLE(1, ADDRESS);
  ISOLATIONAWARE_MANIFEST_RESOURCE_ID := LOOPHOLE(2, ADDRESS);
  ISOLATIONAWARE_NOSTATICIMPORT_MANIFEST_RESOURCE_ID := LOOPHOLE(3, ADDRESS);
  MINIMUM_RESERVED_MANIFEST_RESOURCE_ID := LOOPHOLE(1, ADDRESS);
  MAXIMUM_RESERVED_MANIFEST_RESOURCE_ID := LOOPHOLE(16, ADDRESS);

  HWND_BROADCAST := LOOPHOLE(16_ffff, ADDRESS);

  HWND_DESKTOP := LOOPHOLE(0, ADDRESS);

  HWND_TOP := LOOPHOLE(0, ADDRESS);
  HWND_BOTTOM := LOOPHOLE(1, ADDRESS);
  HWND_TOPMOST := LOOPHOLE(-1, ADDRESS);
  HWND_NOTOPMOST := LOOPHOLE(-2, ADDRESS);

  IDC_ARROW := LOOPHOLE(32512, ADDRESS);
  IDC_IBEAM := LOOPHOLE(32513, ADDRESS);
  IDC_WAIT := LOOPHOLE(32514, ADDRESS);
  IDC_CROSS := LOOPHOLE(32515, ADDRESS);
  IDC_UPARROW := LOOPHOLE(32516, ADDRESS);
  IDC_SIZE := LOOPHOLE(32640, ADDRESS);
  IDC_ICON := LOOPHOLE(32641, ADDRESS);
  IDC_SIZENWSE := LOOPHOLE(32642, ADDRESS);
  IDC_SIZENESW := LOOPHOLE(32643, ADDRESS);
  IDC_SIZEWE := LOOPHOLE(32644, ADDRESS);
  IDC_SIZENS := LOOPHOLE(32645, ADDRESS);
  IDC_SIZEALL := LOOPHOLE(32646, ADDRESS); (* not in win3.1 *)
  IDC_NO := LOOPHOLE(32648, ADDRESS); (* not in win3.1 *)
  IDC_HAND := LOOPHOLE(32649, ADDRESS);
  IDC_APPSTARTING := LOOPHOLE(32650, ADDRESS); (* not in win3.1 *)
  IDC_HELP := LOOPHOLE(32651, ADDRESS);

  IDI_APPLICATION := LOOPHOLE(32512, ADDRESS);
  IDI_HAND := LOOPHOLE(32513, ADDRESS);
  IDI_QUESTION := LOOPHOLE(32514, ADDRESS);
  IDI_EXCLAMATION := LOOPHOLE(32515, ADDRESS);
  IDI_ASTERISK := LOOPHOLE(32516, ADDRESS);
  IDI_WINLOGO := LOOPHOLE(32517, ADDRESS);
  IDI_WARNING := IDI_EXCLAMATION;
  IDI_ERROR := IDI_HAND;
  IDI_INFORMATION := IDI_ASTERISK;

  WC_DIALOG := LOOPHOLE(16_8002, ADDRESS);

  HBMMENU_CALLBACK := LOOPHOLE(-1, ADDRESS);
  HBMMENU_SYSTEM := LOOPHOLE(1, ADDRESS);
  HBMMENU_MBAR_RESTORE := LOOPHOLE(2, ADDRESS);
  HBMMENU_MBAR_MINIMIZE := LOOPHOLE(3, ADDRESS);
  HBMMENU_MBAR_CLOSE := LOOPHOLE(5, ADDRESS);
  HBMMENU_MBAR_CLOSE_D := LOOPHOLE(6, ADDRESS);
  HBMMENU_MBAR_MINIMIZE_D := LOOPHOLE(7, ADDRESS);
  HBMMENU_POPUP_CLOSE := LOOPHOLE(8, ADDRESS);
  HBMMENU_POPUP_RESTORE := LOOPHOLE(9, ADDRESS);
  HBMMENU_POPUP_MAXIMIZE := LOOPHOLE(10, ADDRESS);
  HBMMENU_POPUP_MINIMIZE := LOOPHOLE(11, ADDRESS);

END WinUser.
