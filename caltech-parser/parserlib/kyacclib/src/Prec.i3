(* Copyright (c) 2000 California Institute of Technology *)
(* All rights reserved. See the file COPYRIGHT for a full description. *)
(* $Id: Prec.i3,v 1.1.1.1 2001-09-19 11:40:10 wagner Exp $ *)

INTERFACE Prec;
CONST
  Brand = "Prec";
TYPE
  Kind = {Left, Right, None};
  T = REF RECORD
    kind: Kind;
    val: INTEGER := 0;
    used: BOOLEAN := FALSE;
  END;

PROCEDURE Format(a: T): TEXT;
END Prec.
