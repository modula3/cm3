(* Copyright (C) 1990, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)

(* File: Cstddef.i3                                            *)
(* Last modified on Tue Nov 20 04:02:50 1990 by muller         *)


INTERFACE Cstddef;
IMPORT Ctypes;

TYPE
  size_t = INTEGER; (* This is wrong. It should be unsigned, e.g. [0 .. 2 ** BITSIZE(INTEGER) - 1]. *)
  ptrdiff_t = INTEGER;
  wchar_t = Ctypes.unsigned_short;

END Cstddef.
