(* Copyright (c) 2000 California Institute of Technology *)
(* All rights reserved. See the file COPYRIGHT for a full description. *)
(* $Id$ *)

INTERFACE Mark;
IMPORT Pos;
IMPORT Rule;
CONST
  Brand = "Mark";
TYPE
  T = RECORD
    current: Pos.T;
    return: Pos.T;  (* final return point, collapsing tail recursion 
                    if return is Pos.Null then accept guaranteed. *)
    first: Rule.T;  (* the first rule to reduce to get to this state *)
    baseRule: Rule.T; (* rule entered immediately after last advance
                          used only for comparing precedence *)
  END;

PROCEDURE Compare(a, b: T): [-1 .. 1];
PROCEDURE Equal(a, b: T): BOOLEAN;
PROCEDURE Hash(a: T): INTEGER;
PROCEDURE Format(a: T): TEXT;

PROCEDURE Advance(a: T): T;

END Mark.
