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
IMPORT Cstddef;

TYPE
  UINT8 = Ctypes.unsigned_char;
  UINT16 = Ctypes.unsigned_short;
  UINT32 = Ctypes.unsigned_int;
  UINT64 = Ctypes.unsigned_long_long;
  INT8 = Ctypes.signed_char;
  INT16 = Ctypes.short;
  INT32 = Ctypes.int;
  INT64 = Ctypes.long_long;
  SIZE_T = Cstddef.size_t; (* same size as a pointer, unsigned *)
  SSIZE_T = Cstddef.ptrdiff_t; (* same size as a pointer, signed *)
(*
  ULONGLONG = UINT64;
  LONGLONG = INT64;
  UINT_PTR = SIZE_T; (* same size as a pointer, unsigned *)
  ULONG_PTR = SIZE_T; (* same size as a pointer, unsigned *)
  DWORD_PTR = SIZE_T; (* same size as a pointer, unsigned *)
  INT_PTR = SSIZE_T; (* same size as a pointer, signed *)
  LONG_PTR = SSIZE_T; (* same size as a pointer, signed *)
  POINTER_64_INT = SIZE_T;
  LONG32 = INT32;
  ULONG32 = UINT32;
  DWORD32 = UINT32;
  ULONG64 = UINT64;
  LONG64 = INT64;
  __int3264 = SSIZE_T; (* same size as a pointer, signed *)
  SHANDLE_PTR = SSIZE_T; (* same size as a pointer, signed *)
  HANDLE_PTR = SIZE_T; (* same size as a pointer, unsigned *)
  KAFFINITY = SIZE_T;
  (* HALF_PTR, UHALF_PTR, ADDRESS_TAG_BIT, MAX* *)
*)
  PUINT8 = UNTRACED REF UINT8;
  PUINT16 = UNTRACED REF UINT16;
  PUINT32 = UNTRACED REF UINT32;
  PUINT64 = UNTRACED REF UINT64;

  PINT8 = UNTRACED REF INT8;
  PINT16 = UNTRACED REF INT16;
  PINT32 = UNTRACED REF INT32;
  PINT64 = UNTRACED REF INT64;

  PSIZE_T = UNTRACED REF SIZE_T;

  PVOID = Ctypes.void_star;
  PCVOID = Ctypes.const_void_star;
  BOOL = INT32;
  CCHAR = Ctypes.char;
  HANDLE = PVOID;  (*** should be <: ADDRESS ***)
  PBOOL = PINT32;
  PFLOAT = UNTRACED REF WFLOAT;
  PHANDLE = UNTRACED REF HANDLE;
  (* !!!  Name clash with Modula-3 builtin.  FLOAT -> WFLOAT *)
  WFLOAT = Ctypes.float;

  WCHAR = UINT16; (* wc, 16-bit UNICODE character *)
  PSTR = Ctypes.char_star;
  PCSTR = Ctypes.const_char_star;
  PWSTR = UNTRACED REF WCHAR;
  PCWSTR = PWSTR;
  TCHAR = Ctypes.char;
  PTSTR = PSTR;
  PCTSTR = PSTR;
  PSZ = PSTR;

END WinBaseTypes.
