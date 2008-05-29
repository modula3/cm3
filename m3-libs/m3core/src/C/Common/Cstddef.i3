(* Copyright (C) 1990, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)

(* File: Cstddef.i3                                            *)
(* Last modified on Tue Nov 20 04:02:50 1990 by muller         *)


INTERFACE Cstddef;
IMPORT Ctypes;

TYPE
  size_t = Ctypes.unsigned_long;
  ssize_t = Ctypes.long;
  ptrdiff_t = Ctypes.long;
  wchar_t = Ctypes.unsigned_short;

END Cstddef.
