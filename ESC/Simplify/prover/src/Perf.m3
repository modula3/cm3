(* Copyright (C) 2002 Hewlett-Packard Company *)
(* Copyright (C) 2000, 2002 Compaq Computer Corporation *)
(* Copyright (C) 1992, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)
(*                                                                           *)
(* Last modified on Fri May 17 11:37:29 PDT 1996 by detlefs                  *)

MODULE Perf;

IMPORT RefList;

PROCEDURE RLCells(ra: REFANY): INTEGER =
  BEGIN
    TYPECASE ra OF
    | NULL => RETURN 0
    | RefList.T(rl) => RETURN 1 + RLCells(rl.head) + RLCells(rl.tail)
    ELSE RETURN 0
    END (* TYPECASE *)
  END RLCells;

BEGIN
END Perf.

