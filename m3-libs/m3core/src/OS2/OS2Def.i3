(* Copyright (C) 1995, Klaus Preschern                       *)
(* All rights reserved.                                      *)
(*                                                           *)
INTERFACE OS2Def;

IMPORT Ctypes;

TYPE
  BYTE    = Ctypes.unsigned_char;
  PBYTE   = UNTRACED REF BYTE;
  WORD    = Ctypes.unsigned_short;
  PWORD   = UNTRACED REF WORD;
  DWORD   = Ctypes.unsigned_long;
  PDWORD  = UNTRACED REF DWORD;

  SHORT   = Ctypes.short;
  PSHORT  = Ctypes.short_star;
  USHORT  = Ctypes.unsigned_short;
  PUSHORT = UNTRACED REF USHORT;

  BOOL    = Ctypes.int;
  PBOOL   = UNTRACED REF BOOL;
  BOOL32  = Ctypes.unsigned_long;
  PBOOL32 = UNTRACED REF BOOL32;

  INT     = Ctypes.int;
  PINT    = UNTRACED REF Ctypes.int;
  UINT    = Ctypes.unsigned_int;
  PUINT   = Ctypes.unsigned_int_star;

  LONG    = Ctypes.long;
  PLONG   = UNTRACED REF Ctypes.long;
  ULONG   = Ctypes.unsigned_long;
  PULONG  = UNTRACED REF ULONG;

  PVOID   = Ctypes.void_star;
  CPVOID  = (* const *) PVOID;
  PPVOID  = UNTRACED REF PVOID;

  PCHAR   = UNTRACED REF Ctypes.char;
  UCHAR   = Ctypes.unsigned_char;
  PUCHAR  = UNTRACED REF UCHAR;
  PSZ     = Ctypes.unsigned_char_star;
  PCSZ    = Ctypes.unsigned_char_star;

  PFLOAT  = UNTRACED REF Ctypes.float;

  APIRET  = ULONG;
  PID     = ULONG;
  PPID    = UNTRACED REF PID;

  TID     = ULONG;
  PTID    = UNTRACED REF TID;

  LHANDLE = ULONG;

END OS2Def.
