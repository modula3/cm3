(* Copyright (C) 1990, Digital Equipment Corporation.         *)
(* All rights reserved.                                       *)
(* See the file COPYRIGHT for a full description.             *)
(* Last modified on Fri Jan 28 10:55:36 PST 1994 by detlefs   *)

GENERIC INTERFACE Sort(Elem);

TYPE
  CompareProc = PROCEDURE(x,y: Elem.T): INTEGER;
  (* Must return < 0, 0, >0 according as x<y, x=y, x>y *)

PROCEDURE Array(
    VAR (*inout*) a: ARRAY OF Elem.T;
    p: CompareProc)
    RAISES {};

END Sort.
