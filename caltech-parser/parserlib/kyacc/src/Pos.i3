(* Copyright (c) 2000 California Institute of Technology *)
(* All rights reserved. See the file COPYRIGHT for a full description. *)
(* $Id$ *)

INTERFACE Pos;
IMPORT Rule;
IMPORT SymList;
TYPE
  T = RECORD
    rule: Rule.T; (* **** allow NIL in compare etc. *)
    cell: SymList.T;
    index: INTEGER;
  END;

CONST
  Null = T{NIL, NIL, 0};   (* accepting return position *)
  Error = T{NIL, NIL, -1}; (* double return position, but 1-elem stack*)

PROCEDURE Compare(a, b: T): [-1 .. 1];
PROCEDURE Equal(a, b: T): BOOLEAN;
PROCEDURE Hash(a: T): INTEGER;
PROCEDURE Format(a: T): TEXT;

PROCEDURE Zero(r: Rule.T): T;
PROCEDURE Advance(a: T): T;

END Pos.
