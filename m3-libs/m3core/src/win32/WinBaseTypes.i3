(* Copyright (C) 1994, Digital Equipment Corporation         *)
(* All rights reserved.                                      *)
(* See the file COPYRIGHT for a full description.            *)
(*                                                           *)
(* by Stephen Harrison                                       *)
(*                                                           *)
(* Last modified on Tue Nov  8 11:55:35 PST 1994 by kalsow   *)
(*      modified on Wed Feb 10 19:47:46 PST 1993 by harrison *)

INTERFACE WinBaseTypes;

(* Some basic type definitions for WinDef.i3 and WinNT.i3.  These types are
   reexported from those modules as appropiate. *)

IMPORT Ctypes;

TYPE
  BOOL = Ctypes.int;
  BYTE = Ctypes.unsigned_char;
  CCHAR = Ctypes.char;
  DWORD = Ctypes.unsigned_long;
  HANDLE = Ctypes.void_star;  (*** should be <: ADDRESS ***)
  INT = Ctypes.int;
  LONG = Ctypes.long;
  LPBOOL = UNTRACED REF BOOL;
  LPBYTE = UNTRACED REF BYTE;
  LPDWORD = UNTRACED REF DWORD;
  LPINT = UNTRACED REF Ctypes.int;
  LPLONG = UNTRACED REF Ctypes.long;
  LPVOID = Ctypes.void_star;
  LPCVOID = Ctypes.void_star;
  LPWORD = UNTRACED REF WORD;
  PBOOL = UNTRACED REF BOOL;
  PBYTE = UNTRACED REF BYTE;
  PDWORD = UNTRACED REF DWORD;
  PFLOAT = UNTRACED REF WFLOAT;
  PHANDLE = UNTRACED REF HANDLE;
  PINT = UNTRACED REF Ctypes.int;
  PLONG = Ctypes.long_star;
  PSHORT = Ctypes.short_star;
  PSZ = Ctypes.char_star;
  PUCHAR = UNTRACED REF UCHAR;
  PUINT = Ctypes.unsigned_int_star;
  PULONG = UNTRACED REF ULONG;
  PUSHORT = UNTRACED REF USHORT;
  PWORD = UNTRACED REF WORD;
  SHORT = Ctypes.short;
  UCHAR = Ctypes.unsigned_char;
  UINT = Ctypes.unsigned_int;
  ULONG = Ctypes.unsigned_long;
  USHORT = Ctypes.unsigned_short;
  (* !!!  Name clash with Modula-3 builtin.  FLOAT -> WFLOAT *)
  WFLOAT = Ctypes.float;
  WORD = Ctypes.unsigned_short;

END WinBaseTypes.
