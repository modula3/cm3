(* Copyright (C) 1994, Digital Equipment Corporation         *)
(* All rights reserved.                                      *)
(* See the file COPYRIGHT for a full description.            *)
(*                                                           *)
(* by Stephen Harrison                                       *)
(*                                                           *)
(* Last modified on Tue Oct  3 10:34:10 PDT 1995 by najork   *)
(*      modified on Tue Nov  8 16:20:58 PST 1994 by kalsow   *)
(*      modified on Thu Mar 18 17:53:33 PST 1993 by harrison *)

INTERFACE WinDef;

(* Corresponds to build version 0001 of windef.h *)

IMPORT Ctypes, WinBaseTypes;

TYPE
  ULONG = WinBaseTypes.ULONG;
  PULONG = WinBaseTypes.PULONG;
  USHORT = WinBaseTypes.USHORT;
  PUSHORT = WinBaseTypes.PUSHORT;
  UCHAR = WinBaseTypes.UCHAR;
  PUCHAR = WinBaseTypes.PUCHAR;
  PSZ = WinBaseTypes.PSZ;
  DWORD = WinBaseTypes.DWORD;
  BOOL = WinBaseTypes.BOOL;
  BYTE = WinBaseTypes.BYTE;
  WORD = WinBaseTypes.WORD;
  WFLOAT = WinBaseTypes.WFLOAT;
  PFLOAT = WinBaseTypes.PFLOAT;
  PBOOL = WinBaseTypes.PBOOL;
  LPBOOL = WinBaseTypes.LPBOOL;
  PBYTE = WinBaseTypes.PBYTE;
  LPBYTE = WinBaseTypes.LPBYTE;
  PINT = WinBaseTypes.PINT;
  LPINT = WinBaseTypes.LPINT;
  PWORD = WinBaseTypes.PWORD;
  LPWORD = WinBaseTypes.LPWORD;
  LPLONG = WinBaseTypes.LPLONG;
  PDWORD = WinBaseTypes.PDWORD;
  LPDWORD = WinBaseTypes.LPDWORD;
  LPVOID = WinBaseTypes.LPVOID;
  LPCVOID = WinBaseTypes.LPCVOID;
  INT = WinBaseTypes.INT;
  UINT = WinBaseTypes.UINT;
  PUINT = WinBaseTypes.PUINT;

  CCHAR = Ctypes.char;
  LONG = Ctypes.long;
  PLONG = Ctypes.long_star;
  PSHORT = Ctypes.short_star;
  SHORT = Ctypes.short;

CONST
  MAX_PATH = 260;

(* Types use for passing & returning polymorphic values *)
TYPE
  WPARAM = UINT;
  LPARAM = LONG;
  LRESULT = LONG;

<* INLINE *>
PROCEDURE MAKEWORD(a, b: BYTE): WORD;
<* INLINE *>
PROCEDURE MAKELONG(a, b: WORD): LONG;
<* INLINE *>
PROCEDURE LOWORD(l: LONG): WORD;
<* INLINE *>
PROCEDURE HIWORD(l: LONG): WORD;
<* INLINE *>
PROCEDURE LOBYTE(w: WORD): BYTE;
<* INLINE *>
PROCEDURE HIBYTE(w: WORD): BYTE;

TYPE
  ATOM = WORD;

  HANDLE       = WinBaseTypes.HANDLE;
  SPHANDLE     = UNTRACED REF WinBaseTypes.HANDLE;
  LPHANDLE     = UNTRACED REF WinBaseTypes.HANDLE;
  HGLOBAL      = WinBaseTypes.HANDLE;
  HLOCAL       = WinBaseTypes.HANDLE;
  GLOBALHANDLE = WinBaseTypes.HANDLE;
  LOCALHANDLE  = WinBaseTypes.HANDLE;

  PROC     = <*WINAPI*> PROCEDURE ();
  NEARPROC = <*WINAPI*> PROCEDURE ();
  FARPROC  = <*WINAPI*> PROCEDURE ();

TYPE
  HWND         <: HANDLE;
  HHOOK        <: HANDLE;
  HGDIOBJ      =  HANDLE; (*?? <: HANDLE *)
  HACCEL       <: HANDLE;
  HBITMAP      <: HANDLE;
  HBRUSH       <: HANDLE;
  HDC          <: HANDLE;
  HGLRC        <: HANDLE; 
  HDESK        <: HANDLE;
  HENHMETAFILE <: HANDLE;
  HMF          <: HANDLE;
  HEMF         <: HANDLE;
  HFONT        <: HANDLE;
  HICON        <: HANDLE;
  HMENU        <: HANDLE;
  HMETAFILE    <: HANDLE;
  HINSTANCE    <: HANDLE;
  HMODULE      = HINSTANCE;
  HPALETTE     <: HANDLE;
  HPEN         <: HANDLE;
  HRGN         <: HANDLE;
  HRSRC        <: HANDLE;
  HSTR         <: HANDLE;
  HWINSTA      <: HANDLE;
  HKL          <: HANDLE;

TYPE
  HFILE = Ctypes.int;   (* Polymorphic with C runtime file handle type *)
  HCURSOR = HICON;      (* HICONs & HCURSORs are polymorphic *)

  COLORREF = DWORD;
  LPCOLORREF = UNTRACED REF DWORD;

CONST
  HFILE_ERROR: HFILE = -1;

TYPE
  PRECT = UNTRACED REF RECT;
  NPRECT =  UNTRACED REF RECT;
  LPRECT =  UNTRACED REF RECT;
  RECT = RECORD
    left  : LONG;
    top   : LONG;
    right : LONG;
    bottom: LONG;
  END;

  PRECTL = UNTRACED REF RECTL;
  RECTL = RECORD
    left  : LONG;
    top   : LONG;
    right : LONG;
    bottom: LONG;
  END;

  PPOINT = UNTRACED REF POINT;
  NPPOINT =  UNTRACED REF POINT;
  LPPOINT =  UNTRACED REF POINT;
  POINT = RECORD
    x: LONG;
    y: LONG;
  END;

  PPOINTL = UNTRACED REF POINTL;
  POINTL = RECORD
    x: LONG;
    y: LONG;
  END;

  PSIZE = UNTRACED REF SIZE;
  LPSIZE = UNTRACED REF SIZE;
  SIZE = RECORD
    cx: LONG;
    cy: LONG;
  END;

  SIZEL = SIZE;
  PSIZEL = UNTRACED REF SIZE;

  PPOINTS = UNTRACED REF POINTS;
  LPPOINTS = UNTRACED REF POINTS;
  POINTS = RECORD
    x: SHORT;
    y: SHORT;
  END;

CONST
  (* mode selections for the device mode function *)
  DM_UPDATE = 1;
  DM_COPY   = 2;
  DM_PROMPT = 4;
  DM_MODIFY = 8;

  DM_IN_BUFFER   = DM_MODIFY;
  DM_IN_PROMPT   = DM_PROMPT;
  DM_OUT_BUFFER  = DM_COPY;
  DM_OUT_DEFAULT = DM_UPDATE;

  (* device capabilities indices *)
  DC_FIELDS           = 1;
  DC_PAPERS           = 2;
  DC_PAPERSIZE        = 3;
  DC_MINEXTENT        = 4;
  DC_MAXEXTENT        = 5;
  DC_BINS             = 6;
  DC_DUPLEX           = 7;
  DC_SIZE             = 8;
  DC_EXTRA            = 9;
  DC_VERSION          = 10;
  DC_DRIVER           = 11;
  DC_BINNAMES         = 12;
  DC_ENUMRESOLUTIONS  = 13;
  DC_FILEDEPENDENCIES = 14;
  DC_TRUETYPE         = 15;
  DC_PAPERNAMES       = 16;
  DC_ORIENTATION      = 17;
  DC_COPIES           = 18;

END WinDef.
