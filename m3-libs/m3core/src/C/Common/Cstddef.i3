(* Copyright (C) 1990, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)

INTERFACE Cstddef;
IMPORT Ctypes;

TYPE
  size_t = Ctypes.unsigned_long;
  ssize_t = Ctypes.long;
  ptrdiff_t = Ctypes.long;

END Cstddef.
