(* Copyright (C) 1992, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)
(*                                                                           *)
(* Last modified on Wed Sep 28 18:18:32 PDT 1994 by heydon                   *)

INTERFACE CRowOp;

IMPORT Ctypes;

<* EXTERNAL rowop:C *>
PROCEDURE P(
  len: Ctypes.long_int;
  target, src: Ctypes.float_star;
  factor: Ctypes.float)
  : Ctypes.int;
(* Requires that "target" and "src" are arrays of length "len". Adds "factor *
   src" to "target", storing the result in "target", and returns the index of
   the maximum absolute value over the first "len - 1" elements of the result.
   If "len = 1", returns -1. *)

END CRowOp.
