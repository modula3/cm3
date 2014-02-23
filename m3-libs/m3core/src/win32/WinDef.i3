(* Copyright (C) 1994, Digital Equipment Corporation         *)
(* All rights reserved.                                      *)
(* See the file COPYRIGHT for a full description.            *)
(*                                                           *)
(* by Stephen Harrison                                       *)
(*                                                           *)
(* Last modified on Tue Oct  3 10:34:10 PDT 1995 by najork   *)
(*      modified on Tue Nov  8 16:20:58 PST 1994 by kalsow   *)
(*      modified on Thu Mar 18 17:53:33 PST 1993 by harrison *)

(* $Id: WinDef.i3,v 1.7.2.1 2014-02-23 00:26:53 rodney Exp $ *)

INTERFACE WinDef;

(* Corresponds to build version 0001 of windef.h *)

IMPORT WinBaseTypes;

TYPE

  (* reexport base types *)

  UINT8 = WinBaseTypes.UINT8;
  UINT16 = WinBaseTypes.UINT16;
  UINT32 = WinBaseTypes.UINT32;
  UINT64 = WinBaseTypes.UINT64;
  INT8 = WinBaseTypes.INT8;
  INT16 = WinBaseTypes.INT16;
  INT32 = WinBaseTypes.INT32;
  INT64 = WinBaseTypes.INT64;
  SIZE_T = WinBaseTypes.SIZE_T;  (* same size as a pointer, unsigned *)
  UINT_PTR = WinBaseTypes.SIZE_T;
  ULONG_PTR = WinBaseTypes.SIZE_T;
  SSIZE_T = WinBaseTypes.SSIZE_T;  (* same size as a pointer, signed *)
  INT_PTR = WinBaseTypes.SSIZE_T;
  LONG_PTR = WinBaseTypes.SSIZE_T;
  PSIZE_T = WinBaseTypes.PSIZE_T;
  PUINT8 = WinBaseTypes.PUINT8;
  PUINT16 = WinBaseTypes.PUINT16;
  PUINT32 = WinBaseTypes.PUINT32;
  PUINT64 = WinBaseTypes.PUINT64;
  PINT8 = WinBaseTypes.PINT8;
  PINT16 = WinBaseTypes.PINT16;
  PINT32 = WinBaseTypes.PINT32;
  PINT64 = WinBaseTypes.PINT64;
  PVOID = WinBaseTypes.PVOID;
  PCVOID = WinBaseTypes.PCVOID;
  BOOL = WinBaseTypes.BOOL;
  CCHAR = WinBaseTypes.CCHAR;
  HANDLE = WinBaseTypes.HANDLE;
  PBOOL = WinBaseTypes.PBOOL;
  PFLOAT = WinBaseTypes.PFLOAT;
  PHANDLE = WinBaseTypes.PHANDLE;
  WFLOAT = WinBaseTypes.WFLOAT;
  WCHAR = WinBaseTypes.WCHAR;
  PSTR = WinBaseTypes.PSTR;
  PCSTR = WinBaseTypes.PCSTR;
  PWSTR = WinBaseTypes.PWSTR;
  PCWSTR = WinBaseTypes.PCWSTR;
  TCHAR = WinBaseTypes.TCHAR;
  PTSTR = WinBaseTypes.PTSTR;
  PCTSTR = WinBaseTypes.PCTSTR;
  PSZ = WinBaseTypes.PSZ;

  (* funny names for base types *)

  BYTE = UINT8;
  UCHAR = UINT8;
  PUCHAR = PUINT8;
  PBYTE = PUINT8;

  PSHORT = PINT16;
  SHORT = INT16;
  USHORT = UINT16;
  PUSHORT = PUINT16;
  WORD = UINT16;
  PWORD = PUINT16;

  ULONG = UINT32;
  DWORD = UINT32;
  UINT = UINT32;
  INT = INT32;
  PULONG = PUINT32;
  LONG = INT32;
  PINT = PINT32;
  LPINT = PINT32;
  LPLONG = PINT32;
  PDWORD = PUINT32;
  PUINT = PUINT32;
  PLONG = PINT32;

  LPBOOL = PBOOL;
  LPBYTE = PBYTE;
  LPWORD = PWORD;
  LPDWORD = PDWORD;
  LPVOID = PVOID;
  LPCVOID = PCVOID;

CONST
  MAX_PATH = 260;

(* Types use for passing & returning polymorphic values *)
TYPE
  WPARAM = SIZE_T;
  LPARAM = SSIZE_T;
  LRESULT = SSIZE_T;

<* INLINE *>
PROCEDURE MAKEWORD(a, b: UINT8): UINT16;
<* INLINE *>
PROCEDURE MAKELONG(a, b: UINT16): INT32;
<* INLINE *>
PROCEDURE LOWORD(l: INT32): UINT16;
<* INLINE *>
PROCEDURE HIWORD(l: INT32): UINT16;
<* INLINE *>
PROCEDURE LOBYTE(w: UINT16): UINT8;
<* INLINE *>
PROCEDURE HIBYTE(w: UINT16): UINT8;

TYPE
  ATOM = UINT16;

  SPHANDLE     = PHANDLE; (* compat *)
  LPHANDLE     = PHANDLE; (* compat *)
  HGLOBAL      = HANDLE;
  HLOCAL       = HANDLE;
  GLOBALHANDLE = HANDLE;
  LOCALHANDLE  = HANDLE;

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
  HFILE = SSIZE_T;   (* Polymorphic with C runtime file handle type, and with CreateFile Win32 HANDLE *)
  HCURSOR = HICON;   (* HICONs & HCURSORs are polymorphic *)

  COLORREF = UINT32;
  PCOLORREF = UNTRACED REF UINT32;
  LPCOLORREF = PCOLORREF; (* compat *)

CONST
  HFILE_ERROR: HFILE = -1;

TYPE
  PRECT = UNTRACED REF RECT;
  NPRECT = PRECT; (* compat *)
  LPRECT = PRECT; (* compat *)
  RECT = RECORD
    left  : INT32;
    top   : INT32;
    right : INT32;
    bottom: INT32;
  END;

  PRECTL = UNTRACED REF RECTL;
  RECTL = RECORD
    left  : INT32;
    top   : INT32;
    right : INT32;
    bottom: INT32;
  END;

  PPOINT = UNTRACED REF POINT;
  NPPOINT = PPOINT; (* compat *)
  LPPOINT = PPOINT; (* compat *)
  POINT = RECORD
    x: INT32;
    y: INT32;
  END;

  PPOINTL = UNTRACED REF POINTL;
  POINTL = RECORD
    x: INT32;
    y: INT32;
  END;

  PSIZE = UNTRACED REF SIZE;
  LPSIZE = PSIZE; (* compat *)
  SIZE = RECORD
    cx: INT32;
    cy: INT32;
  END;

  SIZEL = SIZE;
  PSIZEL = UNTRACED REF SIZE;

  PPOINTS = UNTRACED REF POINTS;
  LPPOINTS = PPOINTS; (* compat *)
  POINTS = RECORD
    x: INT16;
    y: INT16;
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
